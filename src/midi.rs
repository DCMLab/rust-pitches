use super::{Chromatic, Diatonic, Interval, IntervalClass, Pitch};
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Mul, Neg, Sub};
use std::str::FromStr;

mod parsing;

// Midi Interval

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MidiInterval(i32);

pub fn midi(i: i32) -> MidiInterval {
    MidiInterval(i)
}

impl MidiInterval {
    pub fn new(i: i32) -> MidiInterval {
        midi(i)
    }
}

impl Add for MidiInterval {
    type Output = MidiInterval;
    fn add(self, other: MidiInterval) -> MidiInterval {
        midi(self.0 + other.0)
    }
}

impl Sub for MidiInterval {
    type Output = MidiInterval;
    fn sub(self, other: MidiInterval) -> MidiInterval {
        midi(self.0 - other.0)
    }
}

impl Neg for MidiInterval {
    type Output = MidiInterval;
    fn neg(self) -> MidiInterval {
        midi(-self.0)
    }
}

impl Mul<i32> for MidiInterval {
    type Output = MidiInterval;
    fn mul(self, scale: i32) -> MidiInterval {
        midi(self.0 * scale)
    }
}

impl Interval for MidiInterval {
    type IC = MidiIC;
    const OCTAVE: Self = MidiInterval(12);
    const UNISON: Self = MidiInterval(0);
    fn ic(self) -> MidiIC {
        MidiIC::new(self.0)
    }
    fn direction(self) -> Ordering {
        self.0.cmp(&0)
    }
}

impl Diatonic for MidiInterval {
    fn is_step(self) -> bool {
        self.0.abs() <= 2
    }
}

impl Chromatic for MidiInterval {
    const CHROMATIC_SEMITONE: MidiInterval = MidiInterval(1);
}

impl Display for MidiInterval {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "i{}", -self.0)
    }
}

impl FromStr for MidiInterval {
    type Err = String;
    fn from_str(s: &str) -> Result<MidiInterval, Self::Err> {
        parsing::parse_i(s).or_else(|_| Err("Failed to parse midi interval ".to_string() + s))
    }
}

// Midi Interval Class

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MidiIC(i32);

pub fn midic(i: i32) -> MidiIC {
    MidiIC(i.rem_euclid(12))
}

impl MidiIC {
    pub fn new(i: i32) -> MidiIC {
        midic(i)
    }
}

impl Add for MidiIC {
    type Output = MidiIC;
    fn add(self, other: MidiIC) -> MidiIC {
        midic(self.0 + other.0)
    }
}

impl Sub for MidiIC {
    type Output = MidiIC;
    fn sub(self, other: MidiIC) -> MidiIC {
        midic(self.0 - other.0)
    }
}

impl Neg for MidiIC {
    type Output = MidiIC;
    fn neg(self) -> MidiIC {
        midic(-self.0)
    }
}

impl Mul<i32> for MidiIC {
    type Output = MidiIC;
    fn mul(self, scale: i32) -> MidiIC {
        midic(self.0 * scale)
    }
}

impl Interval for MidiIC {
    type IC = MidiIC;
    const OCTAVE: Self = MidiIC(0);
    const UNISON: Self = MidiIC(0);
    fn ic(self) -> MidiIC {
        MidiIC::new(self.0)
    }
    fn direction(self) -> Ordering {
        if self.0 == 0 {
            Ordering::Equal
        } else {
            self.0.cmp(&0)
        }
    }
}

impl IntervalClass for MidiIC {
    type I = MidiInterval;
    fn emb(self) -> MidiInterval {
        midi(self.0)
    }
}

impl Diatonic for MidiIC {
    fn is_step(self) -> bool {
        self.0 <= 2 || (self.0 < 12 && self.0 > 9)
    }
}

impl Chromatic for MidiIC {
    const CHROMATIC_SEMITONE: MidiIC = MidiIC(1);
}

impl Display for MidiIC {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "ic{}", -self.0)
    }
}

impl FromStr for MidiIC {
    type Err = String;
    fn from_str(s: &str) -> Result<MidiIC, Self::Err> {
        parsing::parse_ic(s)
            .or_else(|_| Err("Failed to parse midi interval class ".to_string() + s))
    }
}

// Midi Pitch (Class)
// ------------------

pub type MidiPitch = Pitch<MidiInterval>;
pub type MidiPC = Pitch<MidiIC>;

pub fn midip(i: i32) -> MidiPitch {
    midi(i).to_pitch()
}

pub fn midipc(i: i32) -> MidiPC {
    midic(i).to_pitch()
}

impl Display for MidiPitch {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "p{}", -self.0 .0)
    }
}

impl Display for MidiPC {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "pc{}", -self.0 .0)
    }
}

impl FromStr for MidiPitch {
    type Err = String;
    fn from_str(s: &str) -> Result<MidiPitch, Self::Err> {
        parsing::parse_p(s).or_else(|_| Err("Failed to parse midi pitch ".to_string() + s))
    }
}

impl FromStr for MidiPC {
    type Err = String;
    fn from_str(s: &str) -> Result<MidiPC, Self::Err> {
        parsing::parse_pc(s).or_else(|_| Err("Failed to parse midi pitch class ".to_string() + s))
    }
}
