SuperMarioBros-C
================

An attempt to translate the original Super Mario Bros. for the NES to readable C/C++.

This project is forked from [MitchellSternke/SuperMarioBros-C](https://github.com/MitchellSternke/SuperMarioBros-C). I have modified the source in order to make it readily buildable with Visual Studio 2022, add gamepad support, remove Boost dependency, compile with the latest C++ specification, and most importantly: restructure the game's code into more readable C++.

Restructuring the game's code is a long term project and hopefully it will result in a game that no longer looks like converted low level assembly, but instead looks like high level C++ code. The game's code is converted from smbdis.asm, which was written by doppelganger. The game's code was converted using a code generator written by Mitchell Sternke with Bison and Flex. Heavy inspiration for the restructure is from smbdishl.s written by Movax12. Both smbdis.asm and smbdishl.s can be found in the docs subdirectory for reference.

Thanks to doppelganger, Mitchell Sternke, and Movax12. We stand on the shoulders of giants. Without them, none of this would be possible.

![Demo gif](https://github.com/MitchellSternke/SuperMarioBros-C/raw/master/demo.gif)

*looks and plays just like the original*

Building
--------

**Dependencies**
- Visual Studio 2022
- SDL2 (obtained as NuGet package)

Clone the project outside or inside of Visual Studio 2022. **Place `Super Mario Bros. (JU) (PRG0) [!].nes` inside of the project directory.** Open the solution file. Start debugging with Local Windows Debugger (will default build Debug-x64). Or select Release configuration, build from the build menu, and run it without debugging.

Running
-------

**This requires an *unmodified* copy of the `Super Mario Bros. (JU) (PRG0) [!].nes` ROM to run.** Without this, the game won't have any graphics, since the CHR data is used for rendering. By default, the program will look for this file in the current working directory, but this can also be configured in `smbc.conf`.

Configuration
-------------

Optionally, the program can be configured with a file named `smbc.conf` in the working directory. This file has an INI format. For example, the following would configure the option `audio.frequency` to be `22050`:

```
[audio]
frequency = 22050
```

The following is a list of all configurable options:

### [audio] options

#### enabled

- Allows audio to be enabled (if set to 1) or disabled (if set to 0).
- Default: 1

#### frequency

- Controls the frequency of sampled audio output, in Hz.
- Default: 48000

### [game] options

#### frame_rate

- The desired frame rate, in frames per second. Note that on some systems it may be necessary to disable vsync (see video.vsync) in order to achieve a frame rate higher than 60 fps.
- Default: 60

#### rom_file

- The path to the Super Mario Bros. ROM file. Can be either relative to the working directory or absolute.
- Default: "Super Mario Bros. (JU) (PRG0) [!].nes"

### [video] options

#### palette_file

- Relative or absolute path to a custom palette file to use for rendering. The following formats are supported:
  - 192-byte palette file: contains 64 (R,G,B) triples with one byte per color channel. Byte order must be (R,G,B). This is the standard *.pal file format used by many NES emulators. For more documentation see https://wiki.nesdev.com/w/index.php/.pal
  - 1536-byte palette file: similar to the 192-byte palette file format described above. Only the first 192 bytes are used, as the rest are used by emphasis bits which are not supported by Super Mario Bros.
- Default: ""

#### scale

- Controls the scale factor for rendered video.
- Default: 4

#### scanlines

- Allows rendering scanlines to be enabled (if set to 1) or disabled (if set to 0)
- Default: 0

#### vsync

- Allows vsync to be enabled (if set to 1) or disabled (if set to 0).
- Default: 1

Architecture
------------

The game consists of a few parts:
- The decompiled original Super Mario Bros. source code in C++
- An emulation layer, consisting of
  - Core NES CPU functionality (RAM, CPU registers, call stack, and emulation of unique 6502 instructions that don't have C++ equivalents)
  - Picture Processing Unit (PPU) emulation (for video)
  - Audio Processing Unit (APU) emulation (for sound/music)
  - Controller emulation
- SDL2 library for cross-platform video/audio/input

Essentially, the game is a statically recompiled version of Super Mario Bros. for modern platforms. The only part of the NES that doesn't have to be emulated is the CPU, since most instructions are now native C++ code.

The plan is to eventually ditch all of the emulation layer and convert code that relies upon it. Once that's done, this will be a true cross-platform version of Super Mario Bros. which behaves identically to the original. It could then be easily modified and extended with new features!

License
-------

The author(s) of this project have no liability for what you do with this project. **DO NOT** distribute any rom file with this project or the executable you build with it: that would be unethical and illegal.
