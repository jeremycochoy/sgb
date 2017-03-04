Simple Game Boy emulator
========================

SGB is a game boy (classic) emulator writen in rust.

It has the purpose of demonstrating how to write an emulator,
using both procedural and fonctionnal programming.

Architecture
------------

The emulator is broken in different modules :
* MMU : Handle the memory access
* GPU : Simulate the screen hardware, and render the video output
* CPU : Emulate the instructions of a Z80 like microcontroler
* IO  : Memory mapping of some registers and control (buttons, etc...)
* Tools : Different utilitary functions

TODO
----

At the moment, the consol do not provide any sound output.

Next goals to archieve:
* Emulate 100% of the 0xCB instructions
* Emulate 100% of the main instructions
* Implement joypad
* Implement sprites
* Implement interrupts
* Implement MBC
* Implement timers
