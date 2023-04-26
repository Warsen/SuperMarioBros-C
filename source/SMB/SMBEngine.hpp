#ifndef SMBENGINE_HPP
#define SMBENGINE_HPP

#include <cstdint>
#include <cstddef>

#include "../Emulation/MemoryAccess.hpp"

#include "SMBDataPointers.hpp"

class APU;
class Controller;
class PPU;

// A good reference for how instructions are handled can be found here thanks to Steve Johns:
// https://www.middle-engine.com/blog/posts/2020/06/23/programming-the-nes-the-6502-in-detail

/// <summary>
/// Engine that runs Super Mario Bros. Handles emulation of various NES subsystems for compatibility and accuracy.
/// </summary>
class SMBEngine
{
	friend class MemoryAccess;
	friend class PPU;

public:
	/// <summary>
	/// Construct a new SMBEngine instance.
	/// </summary>
	/// <param name="romImage">the data from the Super Mario Bros. ROM image.</param>
	SMBEngine(uint8_t* romImage);

	~SMBEngine();

	/**
	 * Callback for handling audio buffering.
	 */
	void audioCallback(uint8_t* stream, int length);

	/**
	 * Get player 1's controller.
	 */
	Controller& getController1();

	/**
	 * Get player 2's controller.
	 */
	Controller& getController2();

	/**
	 * Render the screen to a buffer.
	 *
	 * @param buffer a 256x240 32-bit color buffer for storing the rendering.
	 */
	void render(uint32_t* buffer);

	/**
	 * Reset the game engine to power-on state.
	 */
	void reset();

	/**
	 * Update the game engine by one frame.
	 */
	void update();

private:
	// NES Emulation subsystems:
	APU* apu;
	PPU* ppu;
	Controller* controller1;
	Controller* controller2;

	// Fields for NES CPU emulation:
	bool c;                      // Carry flag.
	bool z;                      // Zero flag.
	bool n;                      // Negative flag.
	uint8_t registerA;           // Accumulator register.
	uint8_t registerX;           // X index register.
	uint8_t registerY;           // Y index register.
	uint8_t registerS;           // Stack index register.
	MemoryAccess a;              // Wrapper for A register.
	MemoryAccess x;              // Wrapper for X register.
	MemoryAccess y;              // Wrapper for Y register.
	MemoryAccess s;              // Wrapper for S register.
	uint8_t dataStorage[0x8000]; // 32kb of storage for constant data.
	uint8_t ram[0x800];          // 2kb of RAM.
	uint8_t* chr;                // Pointer to CHR data from the ROM.
	int returnIndexStack[100];   // Stack for managing JSR subroutines.
	int returnIndexStackTop;     // Current index of the top of the call stack.

	// Pointers to constant data used in the decompiled code
	SMBDataPointers dataPointers;

	/// <summary>
	/// Run the decompiled code for the game.
	/// See SMB.cpp for implementation.
	/// </summary>
	/// <param name="mode">the mode to run. 0 runs initialization routines, 1 runs the logic for frames.</param>
	void code(int mode);

	/// <summary>
	/// Load all constant data that was present in the SMB ROM.
	/// See SMBData.cpp for implementation.
	/// </summary>
	void loadConstantData();

	/// <summary>
	/// CMP, CPY, and CPX instructions.
	/// Subtracts value2 from value1 and sets the C, Z, and N flags based on the result.
	/// </summary>
	/// <param name="value1">The first value to compare.</param>
	/// <param name="value2">The second value to compare.</param>
	/// <returns>C if value1 &gt; value2<para />Z if value1 == value2<para />N if diff &gt; 127</returns>
	void compare(uint8_t value1, uint8_t value2);

	/// <summary>
	/// BIT instruction.
	/// Performs a bitwise AND (A &amp; value) and sets the Z and N flags based on the result.
	/// </summary>
	/// <param name="value">The value to perform bitwise operations with.</param>
	/// <returns>Z if A &amp; value == 0<para />N if value &gt; 127</returns>
	void bit(uint8_t value);

	/// <summary>
	/// Get CHR data from the ROM.
	/// </summary>
	/// <returns>A pointer to the CHR data from the ROM.</returns>
	uint8_t* getCHR();

	/// <summary>
	/// Returns a pointer to a byte in the address space.
	/// </summary>
	/// <param name="address">The 16-bit address to retrieve data for.</param>
	/// <returns>A pointer to the data corresponding to the given address, or nullptr if the address is
	/// invalid.</returns>
	uint8_t* getDataPointer(uint16_t address);

	/// <summary>
	/// Returns a MemoryAccess object for the specified memory address.
	/// </summary>
	/// <param name="address">The 16-bit memory address.</param>
	/// <returns>A MemoryAccess object for the specified memory address.</returns>
	MemoryAccess getMemory(uint16_t address);

	/// <summary>
	/// Get a word of memory from a zero-page address and the next byte (wrapped around) in little-endian format.
	/// Returns a 16-bit value from memory starting at the specified address. Little-endian byte order.
	/// </summary>
	/// <param name="address">The 8-bit memory address of the least significant byte.</param>
	/// <returns>The 16-bit value from memory starting at the specified address.</returns>
	uint16_t getMemoryWord(uint8_t address);

	/// <summary>
	/// PHA instruction.
	/// Pushes the contents of the accumulator onto the stack.
	/// </summary>
	void pha();

	/// <summary>
	/// PLA instruction.
	/// Pulls an 8-bit value from the stack and loads it into the accumulator.
	/// </summary>
	void pla();

	/// <summary>
	/// Pop an index from the call stack.
	/// Removes and returns the top element of the return index stack.
	/// </summary>
	/// <returns>The top element of the return index stack.</returns>
	int popReturnIndex();

	/// <summary>
	/// Push an index to the call stack.
	/// Pushes a return index onto the return index stack.
	/// </summary>
	/// <param name="index">The return index to push onto the stack.</param>
	void pushReturnIndex(int index);

	/// <summary>
	/// Read data from an address in the NES address space.
	/// Reads an 8-bit value from the specified 16-bit address.
	/// </summary>
	/// <param name="address">The 16-bit address to read the value from.</param>
	/// <returns>The 8-bit value read from the address.</returns>
	uint8_t readData(uint16_t address);

	/// <summary>
	/// Sets the z and n flags based on the specified 8-bit value.
	/// </summary>
	/// <param name="value">The 8-bit value to use for setting the flags.</param>
	void setZN(uint8_t value);

	/// <summary>
	/// Write data to an address in the NES address space.
	/// Writes the specified 8-bit value to the specified 16-bit address.
	/// </summary>
	/// <param name="address">The 16-bit address to write the value to.</param>
	/// <param name="value">The 8-bit value to write to the address.</param>
	void writeData(uint16_t address, uint8_t value);

	/// <summary>
	/// Map constant data to the address space. The address must be at least 0x8000.
	/// Writes the specified data to the data storage array starting at the given address.
	/// </summary>
	/// <param name="address">The 16-bit address to start writing data at.</param>
	/// <param name="data">A pointer to the data to write.</param>
	/// <param name="length">The length of the data to write.</param>
	void writeData(uint16_t address, const uint8_t* data, std::size_t length);
};

#endif // SMBENGINE_HPP
