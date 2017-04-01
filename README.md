Simple Game Boy emulator
========================

SGB is a game boy (classic) emulator writen in rust.

It has the purpose of demonstrating how to write an emulator,
using both procedural and fonctionnal programming.

Architecture
------------

The emulator is structured in different modules :
* MMU : Handle the memory access
* GPU : Simulate the screen hardware, and render the video output
* CPU : Emulate the instructions of a Z80 like microcontroler
* IO  : Memory mapping of some registers and control (buttons, etc...)
* VM  : All units glued together. Also give the functions for
*       input / output.
* Tools : Different utilitary functions

What does it do?
----------------

It handle sprites (8x8 and 8x16), background window,
joypad and timer interrupts.

It can run the orinal game boy tetris game.

Keyboard mapping
----------------

Here is the key mapping :

>   A = Start
>   S = Select
>   Z = A
>   X = B

And directional arrows for the cross.

How to use it?
--------------

First, you need rustc (the rust compiler) and
cargo (the package manager for rust).

I recommand installing rustc through the rustc website https://www.rust-lang.org/en-US/install.html with the command
```
curl https://sh.rustup.rs -sSf | sh
```

Then, clone this git repository.
Enter the folder, an launch your favorite rom with
```
cargo run --release ~/your_favorite_game.gb
```

If you want to skip the nintendo bios, you can just run
```
cargo run --release -- --no-boot ~/your_favorite_game.gb
```

Enjoy :)

TODO
----

At the moment, the emulator do not provide any sound output.

Next goals to archieve:
* Implement all interrupts
* Implement missing registers
* Support window
* Support the sprite rendering order
* Implement MBC
* Sound
