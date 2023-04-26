#ifndef MEMORYACCESS_HPP
#define MEMORYACCESS_HPP

#include <cstdint>

class SMBEngine;

// A good reference for how instructions are handled can be found here thanks to Steve Johns:
// https://www.middle-engine.com/blog/posts/2020/06/23/programming-the-nes-the-6502-in-detail

/// <summary>
/// Wraps operations to memory values/registers so that status flags can be set for branch operations.
/// </summary>
class MemoryAccess
{
public:
	/// <summary>
	/// Constructs a MemoryAccess that references a location.
	/// </summary>
	/// <param name="engine">The SMBEngine object that provides access to the memory.</param>
	/// <param name="value">A pointer to the location in memory to access.</param>
	MemoryAccess(SMBEngine& engine, uint8_t* value);

	/// <summary>
	/// Constructs a MemoryAccess that references a constant value.
	/// </summary>
	/// <param name="engine">The SMBEngine object that provides access to the memory.</param>
	/// <param name="constant">The constant value to reference.</param>
	MemoryAccess(SMBEngine& engine, uint8_t constant);

	/// <summary>
	/// Assigns a value to this MemoryAccess object and sets the Z and N flags according to the value.
	/// </summary>
	/// <param name="value">The value to assign.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator=(uint8_t value);

	/// <summary>
	/// Assigns the value of another MemoryAccess object to this object and sets the Z and N flags according to the value.
	/// </summary>
	/// <param name="value">The MemoryAccess object to assign to this object.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator=(const MemoryAccess& value);

	/// <summary>
	/// ADC instruction: A + M + C -> A, C.
	/// Adds the specified value to this MemoryAccess object with carry
	/// and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to add to the MemoryAccess object.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />C if sum &gt; 255 (carry)<para />Z if sum == 0<para />N if sum &gt; 127</returns>
	MemoryAccess& operator+=(uint8_t value);

	/// <summary>
	/// SBC instruction: A - (M + C) -> A, C.
	/// Subtracts the specified value from this MemoryAccess object with borrow
	/// and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to subtract from the MemoryAccess object.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />C if diff &lt;= 255 (borrow)<para />Z if diff == 0<para />N if diff &gt; 127</returns>
	MemoryAccess& operator-=(uint8_t value);

	/// <summary>
	/// INC, INX, INY instruction.
	/// Pre-Increments the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator++();

	/// <summary>
	/// DEC, DEX, DEY instruction.
	/// Pre-Decrements the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator--();

	/// <summary>
	/// INC, INX, INY instruction.
	/// Post-Increments the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator++(int);

	/// <summary>
	/// DEC, DEX, DEY instruction.
	/// Post-Decrements the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator--(int);

	/// <summary>
	/// AND instruction.
	/// Performs a bitwise AND assignment with the specified value and sets the Z and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the bitwise AND operation with.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator&=(uint8_t value);

	/// <summary>
	/// ORA instruction.
	/// Performs a bitwise OR assignment with the specified value and sets the Z and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the bitwise OR operation with.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator|=(uint8_t value);

	/// <summary>
	/// EOR instruction.
	/// Performs a bitwise XOR assignment with the specified value and sets the Z and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the bitwise XOR operation with.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator^=(uint8_t value);

	/// <summary>
	/// ASL instruction (shifts in a zero bit on the right).
	/// Performs a left shift assignment with the specified value
	/// and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the left shift operation with.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />C is old value of bit #7<para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator<<=(int shift);

	/// <summary>
	/// LSR instruction (shifts in a zero bit on the left).
	/// Performs a right shift assignment with the specified value and
	/// sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the right shift operation with.</param>
	/// <returns>A reference to this MemoryAccess object.
	/// <para />C is old value of bit #0<para />Z if var == 0<para />N if var &gt; 127</returns>
	MemoryAccess& operator>>=(int shift);

	/// <summary>
	/// Returns the value of this MemoryAccess object as an unsigned 8-bit integer.
	/// </summary>
	/// <returns>The value of this MemoryAccess object as an unsigned 8-bit integer.</returns>
	operator uint8_t();

	/// <summary>
	/// ROL instruction (shifts in carry bit on the right).
	/// Performs a rotate left operation on the MemoryAccess object
	/// and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <returns>C is old value of bit #7<para />Z if var == 0<para />N if var &gt; 127</returns>
	void rol();

	/// <summary>
	/// ROR instruction (shifts in carry bit on the left)
	/// Performs a rotate right operation on the MemoryAccess object
	/// and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <returns>C is old value of bit #0<para />Z if var == 0<para />N if var &gt; 127</returns>
	void ror();

private:
	SMBEngine& engine;
	uint8_t* value;
	uint8_t constant;
};

#endif // MEMORYACCESS_HPP
