//! MIDI (chromatic / enharmonic / 12-tone) intervals and pitches.
//!
//! Represents intervals and pitches in the 12TET system,
//! where each octave is split into 12 equal (neutral) semitones.
//! This system is simpler but also less nuanced
//! than e.g. the "spelled" pitches and intervals used in traditional Western notation.
//! It enforces enharmonic equivalence,
//! so that pitches such as C♯ and D♭ are represented by the same object.
//! This representation is appropriate either when the input or output format is MIDI-like,
//! or when enharonic equivalence is assumed.
//!
//! Intervals are represented as integers that count semitones.
//! Pitches are similarly represented as integers with C4 = 60, as in the MIDI standard.
//! Interval and pitch classes are constrained to values between 0 and 11,
//! which is enforced in their constructors and operations using modular arithmetics.
//! Types are distinguished using newtypes, using internal [`i32`]s.
//!
//! # Notation
//!
//! The types in this module use a string notation that is based on integers
//! with a prefix to distinguish different types.
//! The integers must be written as decimal digits (without auxiliary underscores etc.)
//! With an optional sign between the prefix and the number for non-"class" types.
//!
//! ```text
//! Type          Prefix  Examples
//! MidiInterval  i       i3, i14, i-8
//! MidiIC        ic      ic0, ..., ic11
//! MidiPitch     p       p60, p43, p-2
//! MidiPC        pc      pc0, ..., pc11
//! ```
//!
//! If you are using an input or output format that represents MIDI pitches as raw integers,
//! you can achieve this by manually converting them from and to integers using the constructors
//! and the [`From`] and [`Into`] instances between the types of this module and [`i32`].

use super::{Chromatic, Diatonic, Interval, IntervalClass, Pitch};
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Mul, Neg, Sub};
use std::str::FromStr;

mod parsing;

// Midi Interval

/// MIDI intervals.
///
/// A newtype wrapper around [`i32`].
/// All operations behave just like integers.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MidiInterval(i32);

/// Creates a midi interval from an integer.
pub fn midi(i: i32) -> MidiInterval {
    MidiInterval(i)
}

impl MidiInterval {
    /// Creates a midi interval from an integer.
    pub fn new(i: i32) -> MidiInterval {
        midi(i)
    }
}

impl From<i32> for MidiInterval {
    fn from(i: i32) -> MidiInterval {
        midi(i)
    }
}

impl From<MidiInterval> for i32 {
    fn from(i: MidiInterval) -> i32 {
        i.0
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
        write!(fmt, "i{}", self.0)
    }
}

impl FromStr for MidiInterval {
    type Err = String;
    fn from_str(s: &str) -> Result<MidiInterval, Self::Err> {
        parsing::parse_i(s).or_else(|_| Err("Failed to parse midi interval ".to_string() + s))
    }
}

// Midi Interval Class

/// MIDI interval classes.
///
/// A newtype wrapper around [`i32`] that ensures values between `0` and `11`.
/// All operations behave like integeres modulo 12.
///
/// Since interval classes are circular, they have no musically interpretable order.
/// The [`Ord`] instance only provides an ordering
/// by the representants in the first octave for convenience.
///
/// The [`direction`](Interval::direction) of a `MidiIC`
/// is determined by its smallest absolute representant modulo 12
/// (e.g. `-3` rather than `9`),
/// with unisons (`0`) and tritones (`+/-6`) being neutral.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct MidiIC(i32);

/// Creates a midi interval class from an integer, wrapping octaves.
pub fn midic(i: i32) -> MidiIC {
    MidiIC(i.rem_euclid(12))
}

impl MidiIC {
    /// Creates a midi interval class from an integer, wrapping octaves.
    pub fn new(i: i32) -> MidiIC {
        midic(i)
    }
}

impl From<i32> for MidiIC {
    fn from(i: i32) -> MidiIC {
        midic(i)
    }
}

impl From<MidiIC> for i32 {
    fn from(i: MidiIC) -> i32 {
        i.0
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
            6.cmp(&self.0)
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
        write!(fmt, "ic{}", self.0)
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

/// MIDI pitches.
///
/// Represent chromatic pitches as integers, with C4 = `60`.
pub type MidiPitch = Pitch<MidiInterval>;

/// MIDI pitch classes.
///
/// Represents chromatic pitch classes as integers from `0` (C) to `11` (B).
///
/// Since pitch classes are circular, they have no musically interpretable order.
/// The [`Ord`] instance only provides an arbitrary ordering from C to B for convenience.
pub type MidiPC = Pitch<MidiIC>;

/// Creates a MIDI pitch from an integer.
pub fn midip(i: i32) -> MidiPitch {
    midi(i).to_pitch()
}

/// Creates a MIDI pitch class from an integer, wrapping octaves.
pub fn midipc(i: i32) -> MidiPC {
    midic(i).to_pitch()
}

impl From<i32> for MidiPitch {
    fn from(i: i32) -> MidiPitch {
        midip(i)
    }
}

impl From<MidiPitch> for i32 {
    fn from(i: MidiPitch) -> i32 {
        i.0 .0
    }
}

impl From<i32> for MidiPC {
    fn from(i: i32) -> MidiPC {
        midipc(i)
    }
}

impl From<MidiPC> for i32 {
    fn from(i: MidiPC) -> i32 {
        i.0 .0
    }
}

impl Display for MidiPitch {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "p{}", self.0 .0)
    }
}

impl Display for MidiPC {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "pc{}", self.0 .0)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn rmi(s: &str) -> MidiInterval {
        s.parse().unwrap()
    }

    fn rmic(s: &str) -> MidiIC {
        s.parse().unwrap()
    }

    fn rmp(s: &str) -> MidiPitch {
        s.parse().unwrap()
    }

    fn rmpc(s: &str) -> MidiPC {
        s.parse().unwrap()
    }

    #[test]
    fn midi_interval_props() {
        assert_eq!(MidiInterval::UNISON, midi(0));
        //todo: neutral wrt addition
        //todo: direction and negate
        //todo: - = +(-)
        //todo: a-a=0
        //todo: from_str . fmt = id
    }

    #[test]
    fn midi_constructors() {
        assert_eq!(midip(42), midi(42).to_pitch());
        assert_eq!(midipc(21), midic(21).to_pitch());
    }

    #[test]
    fn midi_named() {
        assert_eq!(MidiInterval::UNISON, midi(0));
        assert_eq!(MidiInterval::OCTAVE, midi(12));
        assert_eq!(MidiInterval::CHROMATIC_SEMITONE, midi(1));

        assert_eq!(MidiIC::UNISON, midic(0));
        assert_eq!(MidiIC::OCTAVE, midic(0));
        assert_eq!(MidiIC::CHROMATIC_SEMITONE, midic(1));
    }

    #[test]
    fn midi_parsing() {
        assert_eq!("i16".parse(), Ok(midi(16)));
        assert_eq!("i-4".parse(), Ok(midi(-4)));
        assert_eq!("p60".parse(), Ok(midip(60)));
        assert_eq!("p-1".parse(), Ok(midip(-1)));
        assert_eq!("ic13".parse(), Ok(midic(1)));
        assert_eq!("ic1".parse(), Ok(midic(13)));
        assert_eq!("pc0".parse(), Ok(midipc(0)));
        assert_eq!("pc13".parse(), Ok(midipc(1)));
    }

    #[test]
    fn midi_printing() {
        assert_eq!(rmi("i16").to_string(), "i16");
        assert_eq!(rmp("p63").to_string(), "p63");
        assert_eq!(rmic("ic3").to_string(), "ic3");
        assert_eq!(rmpc("pc5").to_string(), "pc5");
    }

    #[test]
    fn midi_additive() {
        // todo: replace by property tests?
        assert_eq!(midi(3) + midi(4), midi(7));
        assert_eq!(midi(3) + midi(11), midi(14));
        assert_eq!(midi(7) + midi(7), midi(14));
        assert_eq!(midi(-3) + midi(4), midi(1));
        assert_eq!(midi(3) + midi(-4), midi(-1));
        assert_eq!(midi(3) - midi(4), midi(-1));
        assert_eq!(midi(3) - midi(9), midi(-6));
        assert_eq!(-midi(5), midi(-5));
        assert_eq!(-midi(5), midi(7) - midi(12));
        assert_eq!(-midi(7), MidiInterval::UNISON - midi(7));
    }

    #[test]
    fn midi_multiplication() {
        assert_eq!(midi(7) * 2, midi(14));
    }

    #[test]
    fn midi_direction() {
        assert_eq!(midi(1).direction(), Ordering::Greater);
        assert_eq!(midi(0).direction(), Ordering::Equal);
        assert_eq!(midi(-1).direction(), Ordering::Less);
        assert_eq!(midi(-3).abs(), midi(3));
    }

    #[test]
    fn midi_class_conversion() {
        assert_eq!(midi(14).ic(), midic(2));
        assert_eq!(midi(-14).ic(), midic(10));
    }

    #[test]
    fn midi_steps_true() {
        for i in -2..2 {
            assert!(midi(i).is_step());
        }
    }

    #[test]
    fn midi_steps_false() {
        assert!(!midi(3).is_step());
        assert!(!midi(-3).is_step());
        assert!(!midi(11).is_step());
        assert!(!midi(-11).is_step());
        assert!(!midi(12).is_step());
        assert!(!midi(-12).is_step());
        assert!(!midi(13).is_step());
        assert!(!midi(-13).is_step());
    }

    #[test]
    fn midi_class_additive() {
        //todo: do property testing
        assert_eq!(midic(7) + midic(7), midic(2));
        assert_eq!(midic(2) - midic(4), midic(10));
        assert_eq!(-midic(2), midic(10));
    }

    #[test]
    fn midi_class_multiplication() {
        //todo: do property testing
        assert_eq!(midic(7) * 2, midic(2));
        assert_eq!(midic(7) * -1, midic(5));
    }

    #[test]
    fn midi_class_direction() {
        assert_eq!(midic(0).direction(), Ordering::Equal);
        assert_eq!(midic(6).direction(), Ordering::Equal);
        for i in 1..5 {
            assert_eq!(midic(i).direction(), Ordering::Greater);
            assert_eq!(midic(12 - i).direction(), Ordering::Less);
        }
        assert_eq!(midic(7).abs(), midic(5));
    }

    #[test]
    fn midi_class_class_conversion() {
        assert_eq!(midic(4).ic(), midic(4));
        assert_eq!(midic(4).emb(), midi(4));
    }

    #[test]
    fn midi_class_steps_true() {
        for i in -2..2 {
            assert!(midic(i).is_step());
        }
    }

    #[test]
    fn midi_class_steps_false() {
        for i in 3..9 {
            assert!(!midic(i).is_step());
        }
    }

    #[test]
    fn midi_pitch_conversion() {
        assert_eq!(midi(63).to_pitch(), midip(63));
        assert_eq!(midip(42).to_interval(), midi(42));

        assert_eq!(midic(3).to_pitch(), midipc(3));
        assert_eq!(midipc(4).to_interval(), midic(4));

        assert_eq!(midip(63).pc(), midipc(3));
        assert_eq!(midipc(3).pc(), midipc(3));
        assert_eq!(midipc(3).emb(), midip(3));
    }

    #[test]
    fn midi_pitch_arithmetics() {
        assert_eq!(midip(63) + midi(7), midip(70));
        assert_eq!(midip(64) + midi(-4), midip(60));
        assert_eq!(midip(63) - midi(7), midip(56));
        assert_eq!(midip(67).interval_from(midip(61)), midi(6));
        assert_eq!(midip(67).interval_to(midip(61)), midi(-6));
    }

    #[test]
    fn midi_pitch_class_arithmetics() {
        assert_eq!(midipc(63) + midic(7), midipc(70));
        assert_eq!(midipc(64) + midic(-4), midipc(60));
        assert_eq!(midipc(63) - midic(7), midipc(56));
        assert_eq!(midipc(67).interval_from(midipc(61)), midic(6));
        assert_eq!(midipc(67).interval_to(midipc(61)), midic(-6));
    }

    #[test]
    fn midi_integer_conversion() {
        assert_eq!(midi(60), MidiInterval::from(60));
        assert_eq!(i32::from(midi(60)), 60);
        assert_eq!(midic(60), MidiIC::from(60));
        assert_eq!(i32::from(midic(60)), 0);
        assert_eq!(midip(60), MidiPitch::from(60));
        assert_eq!(i32::from(midip(60)), 60);
        assert_eq!(midipc(60), MidiPC::from(60));
        assert_eq!(i32::from(midipc(60)), 0);
    }
}
