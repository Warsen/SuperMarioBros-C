#include <cstring>

#include "../Configuration.hpp"

#include "../Emulation/APU.hpp"
#include "../Emulation/Controller.hpp"
#include "../Emulation/PPU.hpp"

#include "SMBEngine.hpp"

#define DATA_STORAGE_OFFSET 0x8000 // Starting address for storing constant data

//---------------------------------------------------------------------
// Public interface
//---------------------------------------------------------------------

SMBEngine::SMBEngine(uint8_t* romImage) :
	c(), z(), n(), registerA(), registerX(), registerY(), registerS(),
	a(*this, &registerA), x(*this, &registerX), y(*this, &registerY), s(*this, &registerS),
	dataStorage(), ram(), returnIndexStack(), returnIndexStackTop()
{
	apu = new APU();
	ppu = new PPU(*this);
	controller1 = new Controller();
	controller2 = new Controller();

	// CHR Location in ROM: Header (16 bytes) + 2 PRG pages (16k each)
	chr = (romImage + 16 + (16384 * 2));
}

SMBEngine::~SMBEngine()
{
	delete apu;
	delete ppu;
	delete controller1;
	delete controller2;
}

void SMBEngine::audioCallback(uint8_t* stream, int length)
{
	apu->output(stream, length);
}

Controller& SMBEngine::getController1()
{
	return *controller1;
}

Controller& SMBEngine::getController2()
{
	return *controller2;
}

void SMBEngine::render(uint32_t* buffer)
{
	ppu->render(buffer);
}

void SMBEngine::reset()
{
	// Run the decompiled code for initialization
	code(0);
}

void SMBEngine::update()
{
	// Run the decompiled code for the NMI handler
	code(1);

	// Update the APU
	if (Configuration::getAudioEnabled())
	{
		apu->stepFrame();
	}
}

//---------------------------------------------------------------------
// Private methods
//---------------------------------------------------------------------

/// <summary>
/// Logic for CMP, CPY, and CPX instructions.
/// Compares two 8-bit values and sets the C, Z, and N flags based on the result.
/// </summary>
/// <param name="value1">The first value to compare.</param>
/// <param name="value2">The second value to compare.</param>
void SMBEngine::compare(uint8_t value1, uint8_t value2)
{
	uint8_t result = value1 - value2;
	c = (value1 >= value2);
	setZN(result);
}

/// <summary>
/// BIT instruction.
/// Sets the N flag based on the leftmost bit of 8-bit value, and Z flag based on comparison with register A.
/// </summary>
/// <param name="value">The value to perform bitwise operations with.</param>
void SMBEngine::bit(uint8_t value)
{
	n = (value & 0b10000000) != 0;
	z = (value & registerA) == 0;
}

/// <summary>
/// Get CHR data from the ROM.
/// </summary>
/// <returns>A pointer to the CHR data from the ROM.</returns>
uint8_t* SMBEngine::getCHR()
{
	return chr;
}

/// <summary>
/// Get a pointer to a byte in the address space.
/// Returns a pointer to the data corresponding to the given address.
/// </summary>
/// <param name="address">The 16-bit address to retrieve data for.</param>
/// <returns>A pointer to the data corresponding to the given address, or nullptr if the address is invalid.</returns>
uint8_t* SMBEngine::getDataPointer(uint16_t address)
{
	if (address >= DATA_STORAGE_OFFSET) // Constant data
	{
		return dataStorage + (address - DATA_STORAGE_OFFSET);
	}
	else if (address < 0x2000) // RAM and Mirrors
	{
		return ram + (address & 0x7ff);
	}

	return nullptr;
}

/// <summary>
/// Get a memory access object for a particular address.
/// Returns a MemoryAccess object for the specified memory address.
/// </summary>
/// <param name="address">The 16-bit memory address.</param>
/// <returns>A MemoryAccess object for the specified memory address.</returns>
MemoryAccess SMBEngine::getMemory(uint16_t address)
{
	uint8_t* dataPointer = getDataPointer(address);
	if (dataPointer != nullptr)
	{
		return MemoryAccess(*this, dataPointer);
	}
	else
	{
		return MemoryAccess(*this, readData(address));
	}
}

/// <summary>
/// Get a word of memory from a zero-page address and the next byte (wrapped around) in little-endian format.
/// Returns a 16-bit value from memory starting at the specified address. Little-endian byte order.
/// </summary>
/// <param name="address">The 8-bit memory address of the least significant byte.</param>
/// <returns>The 16-bit value from memory starting at the specified address.</returns>
uint16_t SMBEngine::getMemoryWord(uint8_t address)
{
	return (uint16_t)readData(address) + ((uint16_t)(readData(address + 1)) << 8);
}

/// <summary>
/// PHA instruction.
/// Pushes the contents of the accumulator onto the stack.
/// </summary>
void SMBEngine::pha()
{
	writeData(0x100 | (uint16_t)registerS, registerA);
	registerS--;
}

/// <summary>
/// PLA instruction.
/// Pulls an 8-bit value from the stack and loads it into the accumulator.
/// </summary>
void SMBEngine::pla()
{
	registerS++;
	a = readData(0x100 | (uint16_t)registerS);
}

/// <summary>
/// Pop an index from the call stack.
/// Removes and returns the top element of the return index stack.
/// </summary>
/// <returns>The top element of the return index stack.</returns>
int SMBEngine::popReturnIndex()
{
	return returnIndexStack[returnIndexStackTop--];
}

/// <summary>
/// Push an index to the call stack.
/// Pushes a return index onto the return index stack.
/// </summary>
/// <param name="index">The return index to push onto the stack.</param>
void SMBEngine::pushReturnIndex(int index)
{
	returnIndexStack[++returnIndexStackTop] = index;
}

/// <summary>
/// Read data from an address in the NES address space.
/// Reads an 8-bit value from the specified 16-bit address.
/// </summary>
/// <param name="address">The 16-bit address to read the value from.</param>
/// <returns>The 8-bit value read from the address.</returns>
uint8_t SMBEngine::readData(uint16_t address)
{
	if (address >= DATA_STORAGE_OFFSET) // Constant data
	{
		return dataStorage[address - DATA_STORAGE_OFFSET];
	}
	else if (address < 0x2000) // RAM and Mirrors
	{
		return ram[address & 0x7ff];
	}
	else if (address < 0x4000) // PPU Registers and Mirrors
	{
		return ppu->readRegister(0x2000 + (address & 0x7));
	}
	else if (address < 0x4020) // IO registers
	{
		switch (address)
		{
		case 0x4016:
			return controller1->readByte();
		case 0x4017:
			return controller2->readByte();
		}
	}

	return 0;
}

/// <summary>
/// Sets the z and n flags based on the specified 8-bit value.
/// </summary>
/// <param name="value">The 8-bit value to use for setting the flags.</param>
void SMBEngine::setZN(uint8_t value)
{
	z = (value == 0);
	n = (value & 0b10000000) != 0;
}

/// <summary>
/// Write data to an address in the NES address space.
/// Writes the specified 8-bit value to the specified 16-bit address.
/// </summary>
/// <param name="address">The 16-bit address to write the value to.</param>
/// <param name="value">The 8-bit value to write to the address.</param>
void SMBEngine::writeData(uint16_t address, uint8_t value)
{
	if (address < 0x2000) // RAM and Mirrors
	{
		ram[address & 0x7ff] = value;
	}
	else if (address < 0x4000) // PPU Registers and Mirrors
	{
		ppu->writeRegister(0x2000 + (address & 0b0111), value);
	}
	else if (address < 0x4020) // IO registers
	{
		switch( address )
		{
		case 0x4014:
			ppu->writeDMA(value);
			break;
		case 0x4016:
			controller1->writeByte(value);
			controller2->writeByte(value);
			break;
		default:
			apu->writeRegister(address, value);
			break;
		}
	}
}

/// <summary>
/// Map constant data to the address space. The address must be at least 0x8000.
/// Writes the specified data to the data storage array starting at the given address.
/// </summary>
/// <param name="address">The 16-bit address to start writing data at.</param>
/// <param name="data">A pointer to the data to write.</param>
/// <param name="length">The length of the data to write.</param>
void SMBEngine::writeData(uint16_t address, const uint8_t* data, size_t length)
{
	address -= DATA_STORAGE_OFFSET;

	memcpy(dataStorage + (std::ptrdiff_t)address, data, length);
}
