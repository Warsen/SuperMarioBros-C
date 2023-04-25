#ifndef SMB_HPP
#define SMB_HPP

#include "SMBConstants.hpp"
#include "SMBEngine.hpp"

//---------------------------------------------------------------------
// Macros:
//---------------------------------------------------------------------

/// <summary>
/// Returns a MemoryAccess object for the specified memory address.
/// </summary>
/// <param name="address">The 16-bit memory address.</param>
/// <returns>A MemoryAccess object for the specified memory address.</returns>
#define M(addr) getMemory(addr)

/// <summary>
/// Get a word of memory from a zero-page address and the next byte (wrapped around) in little-endian format.
/// Returns a 16-bit value from memory starting at the specified address. Little-endian byte order.
/// </summary>
/// <param name="address">The 8-bit memory address of the least significant byte.</param>
/// <returns>The 16-bit value from memory starting at the specified address.</returns>
#define W(addr) getMemoryWord(addr)

/// <summary>
/// Call a subroutine stored in a goto label. Pushes a return index to a generated return label.
/// </summary>
#define JSR(subroutine, index) pushReturnIndex(index); goto subroutine; Return_ ## index:

/// <summary>
/// Gets the high byte of a 16-bit integer.
/// </summary>
#define HIBYTE(v) (static_cast<uint8_t>((v >> 8) & 0xff))

/// <summary>
/// Gets the low byte of a 16-bit integer.
/// </summary>
#define LOBYTE(v) (static_cast<uint8_t>(v & 0xff))

#endif // SMB_HPP
