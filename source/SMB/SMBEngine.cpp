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

void SMBEngine::compare(uint8_t value1, uint8_t value2)
{
	uint8_t result = value1 - value2;
	c = (value1 >= value2);
	setZN(result);
}

void SMBEngine::bit(uint8_t value)
{
	n = (value & 0b10000000) != 0;
	z = (value & registerA) == 0;
}

uint8_t* SMBEngine::getCHR()
{
	return chr;
}

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

uint16_t SMBEngine::getMemoryWord(uint8_t address)
{
	return (uint16_t)readData(address) + ((uint16_t)(readData(address + 1)) << 8);
}

void SMBEngine::pha()
{
	writeData(0x100 | (uint16_t)registerS, registerA);
	registerS--;
}

void SMBEngine::pla()
{
	registerS++;
	a = readData(0x100 | (uint16_t)registerS);
}

int SMBEngine::popReturnIndex()
{
	return returnIndexStack[returnIndexStackTop--];
}

void SMBEngine::pushReturnIndex(int index)
{
	returnIndexStack[++returnIndexStackTop] = index;
}

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

void SMBEngine::setZN(uint8_t value)
{
	z = (value == 0);
	n = (value & 0b10000000) != 0;
}

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

void SMBEngine::writeData(uint16_t address, const uint8_t* data, size_t length)
{
	address -= DATA_STORAGE_OFFSET;

	memcpy(dataStorage + (std::ptrdiff_t)address, data, length);
}
