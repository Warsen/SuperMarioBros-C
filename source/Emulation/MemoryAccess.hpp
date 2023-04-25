#ifndef MEMORYACCESS_HPP
#define MEMORYACCESS_HPP

#include <cstdint>

class SMBEngine;

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
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator=(uint8_t value);

	/// <summary>
	/// Assigns the value of another MemoryAccess object to this object and sets the Z and N flags according to the value.
	/// </summary>
	/// <param name="rhs">The MemoryAccess object to assign to this object.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator=(const MemoryAccess& rhs);

	/// <summary>
	/// Adds the specified value to this MemoryAccess object and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to add to the MemoryAccess object.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator+=(uint8_t value);

	/// <summary>
	/// Subtracts the specified value from this MemoryAccess object and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to subtract from the MemoryAccess object.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator-=(uint8_t value);

	/// <summary>
	/// Pre-Increments the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator++();

	/// <summary>
	/// Pre-Decrements the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator--();

	/// <summary>
	/// Post-Increments the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator++(int);

	/// <summary>
	/// Post-Decrements the value of this MemoryAccess object by 1 and sets the Z and N flags according to the result.
	/// </summary>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator--(int);

	/// <summary>
	/// Performs a bitwise AND assignment with the specified value and sets the Z and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the bitwise AND operation with.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator&=(uint8_t value);

	/// <summary>
	/// Performs a bitwise OR assignment with the specified value and sets the Z and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the bitwise OR operation with.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator|=(uint8_t value);

	/// <summary>
	/// Performs a bitwise XOR assignment with the specified value and sets the Z and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the bitwise XOR operation with.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator^=(uint8_t value);

	/// <summary>
	/// Performs a left shift assignment with the specified value and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the left shift operation with.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator<<=(int shift);

	/// <summary>
	/// Performs a right shift assignment with the specified value and sets the C, Z, and N flags according to the result.
	/// </summary>
	/// <param name="value">The value to perform the right shift operation with.</param>
	/// <returns>A reference to this MemoryAccess object.</returns>
	MemoryAccess& operator>>=(int shift);

	/// <summary>
	/// Returns the value of this MemoryAccess object as an unsigned 8-bit integer.
	/// </summary>
	/// <returns>The value of this MemoryAccess object as an unsigned 8-bit integer.</returns>
	operator uint8_t();

	/// <summary>
	/// Performs a rotate left operation on the MemoryAccess object and sets the C, Z, and N flags according to the result.
	/// </summary>
	void rol();

	/// <summary>
	/// Performs a rotate right operation on the MemoryAccess object and sets the C, Z, and N flags according to the result.
	/// </summary>
	void ror();

private:
	SMBEngine& engine;
	uint8_t* value;
	uint8_t constant;
};

#endif // MEMORYACCESS_HPP
