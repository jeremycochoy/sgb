#[derive(PartialEq, Eq, Default, Debug)]
struct Registers {
        // Registers :
        a  : u8,
        b  : u8,
        c  : u8,
        d  : u8,
        e  : u8,
        h  : u8,
        l  : u8,
        // Flags :
        f  : u8,
        // Program counter
        pc : u16,
        // Stack pointer
        sp : u16,
}

#[derive(PartialEq, Eq, Default, Debug)]
// Deriving Eq & show?
struct Clock {
        m : u32,
        t : u32,
}

#[derive(PartialEq, Eq, Debug)]
enum InterruptState {
        IEnabled,
        IDisabled,
        INextInstD,
        INextInstE,
}

impl Default for InterruptState {
    fn default() -> InterruptState { InterruptState::IDisabled }
}

#[derive(PartialEq, Eq, Default, Debug)]
struct Cpu {
        registers : Registers,
        clock : Clock,
        interrupt : InterruptState,
}
