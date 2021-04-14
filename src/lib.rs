#![warn(missing_docs)]
//! This crate provides types and traits for dealing with musical pitch.
//!
//! The two main goals are:
//! - providing types and operations (such as arithmetics, printing and parsing) for common types of pitches and intervals
//! - providing a generic interface for writing code that is agnostic to the specific pitch or interval types.
//!
//! It allows you to write generic algorithms that can then be applied
//! to pitches and intervals in different formats:
//! ```rust
//! use pitches::midi::*;
//! use pitches::spelled::*;
//! use pitches::*;
//!
//! // a function that takes a list of pitches, transposes them by some interval,
//! // and then transposes them by another octave.
//! fn transpose_and_add_octave<I>(pitches: &Vec<Pitch<I>>, by: I) -> Vec<Pitch<I>>
//! where
//!     I: Interval,
//! {
//!     // we use the + operator on pitches and intervals
//!     // as well as the OCTAVE associate constant to compute the result
//!     pitches.iter().map(|p| *p + by + I::OCTAVE).collect()
//! }
//!
//! fn main() {
//!     // apply to MIDI pitches/intervals
//!     println!("MIDI:");
//!     // create MIDI pitches from integers using the midip function:
//!     let midi_pitches = vec![midip(60), midip(64), midip(67), midip(72)];
//!     // apply the above function to all pitches and print them.
//!     for p in transpose_and_add_octave(&midi_pitches, midi(3)).iter() {
//!         println!("- {}", p);
//!     }
//!
//!     // apply to spelled pitches/intervals
//!     println!("spelled:");
//!     // create spelled pitches by parsing them from their standard notation.
//!     // we use unwrap() because we know the pitches and know that they are written correctly.
//!     // when parsing pitches from unknown sources, proper error handling should be used.
//!     let spelled_pitches: Vec<SpelledPitch> = vec!["C4", "E♭4", "G♯4", "C5"]
//!         .iter()
//!         .map(|s| s.parse().unwrap())
//!         .collect();
//!     // similarly, we create the transposition interval from a notation string.
//!     // alternatively, we could use one of the SpelledInterval constructors directly.
//!     let spelled_interval = "M3:0".parse().unwrap();
//!     // apply the above function to all pitches and print them
//!     for p in transpose_and_add_octave(&spelled_pitches, spelled_interval).iter() {
//!         println!("- {}", p);
//!     }
//! }
//! ```
//! Output:
//! ```text
//! MIDI:
//! - p75
//! - p79
//! - p82
//! - p87
//! spelled:
//! - E5
//! - G5
//! - B♯5
//! - E6
//! ```
//!
//! The fundamental idea behind this library is that the central object is the *interval*.
//! Pitches are derived from intervals by interpreting them with respect to a reference point.
//! This is much like the relation between vectors (= intervals) and points (= pitches).
//! For example, the pitch `E♭4` can be represented as an interval (e.g. a minor third, `m3:0`)
//! above a reference pitch such as Middle C (`C4`).
//! The concept of an interval is represented by the [`Interval`] trait.
//! Pitches are represented using the generic [`Pitch<I>`] type.
//! Pitch types for concrete interval types, however, may provide specialized functionality
//! (e.g. for reading and printing) as well as type synonyms
//! (e.g. [`SpelledPitch`](spelled::SpelledPitch) for `Pitch<SpelledInterval>`).
//!
//! Similar to vectors and points, intervals and pitches support a number of operations
//! such as addition and scalar multiplication,
//! which are here implemented using the standard operator traits (such as [`Add`] and [`Mul`])
//! or as methods of the [`Interval`] trait.

use core::ops::{Add, Mul, Neg, Sub};
use std::cmp::Ordering;

pub mod midi;
pub mod spelled;
mod util;

// interval traits
// ---------------

/// The trait that all interval types must implement.
///
/// In addition to special functionality associated with an interval type
/// (such as associate constants, types, and methods)
/// this trait requires the implementation of arithmetic operations that work on intervals,
/// namely addition ([`Add`]), negation ([`Neg`]), subtraction ([`Sub`]),
/// and integer multiplication ([`Mul<i32>`]).
/// Notably excluded are [`Ord`] and [`PartialOrd`]
/// since those might not make sense for every interval type.
pub trait Interval:
    Add<Output = Self> + Sub<Output = Self> + Neg<Output = Self> + Mul<i32, Output = Self> + Copy
{
    /// The interval class type associated with this interval type.
    /// Since interval classes also implement the [`Interval`] trait,
    /// they may refer to themselves here.
    type IC: IntervalClass;

    /// The interval that represents the octave (upward).
    const OCTAVE: Self;
    /// The interval that represents the unison.
    const UNISON: Self;

    /// Turns an interval into the corresponding interval class.
    fn ic(self) -> Self::IC;
    /// Returns the direction of the interval (up, down, or neutral)
    /// as an [`Ordering`] (i.e., [`Greater`](Ordering::Greater),
    /// [`Less`](Ordering::Less), or [`Equal`](Ordering::Equal), respectively).
    fn direction(self) -> Ordering;
    /// Returns the absolute interval as an upward interval, inverting downward intervals.
    fn abs(self) -> Self {
        if self.direction() == Ordering::Less {
            -self
        } else {
            self
        }
    }

    /// Turns an interval into the corresponding pitch.
    /// The exact effect of this method depends on the reference pitch
    /// used by the specific interval type to interpret intervals as pitches,
    /// so it should only be used when this reference pitch is known or irrelevant,
    /// or be avoided altogether.
    fn to_pitch(self) -> Pitch<Self> {
        Pitch(self)
    }
}

/// A trait for intervals with octave equivalence (i.e. "interval classes").
///
/// Interval classes must also implement the [`Interval`] trait,
/// using themselves as the associate interval class type.
pub trait IntervalClass: Interval {
    /// The associate "non-class" interval type.
    type I: Interval<IC = Self>;
    /// Returns a "non-class" version of the interval
    /// in the canonical (i.e. first upward) octave.
    fn emb(self) -> Self::I;
}

/// A trait for intervals that can distinguish between steps and non-steps.
pub trait Diatonic: Interval {
    /// Returns `true`, iff the interval is considered to be within step distance
    /// (e.g. a unison or a second) in any direction.
    fn is_step(self) -> bool;
}

/// A trait for intervals that have a chromatic semitone.
pub trait Chromatic: Interval {
    /// The chromatic semitone (i.e. augmented unison) upward.
    const CHROMATIC_SEMITONE: Self;
}

// Pitch
// -----

/// A generic type for pitches (based on a corresponding interval type).
///
/// Wraps the interval type and generically implements arithmetic operations
/// in terms of the interval type's operations.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pitch<T: Interval>(T);

impl<I: Interval> Pitch<I> {
    /// Creates a pitch from an interval.
    /// The interpretation of the resulting value depends on the implied reference pitch
    /// of the respective type.
    /// Thus, this method should only be used when this reference pitch is known or irrelevant.
    pub fn new(i: I) -> Self {
        Pitch(i)
    }

    /// Extracts the wrapped interval.
    /// The relation between pitch and interval depends on the implied reference pitch
    /// of the respective type.
    /// Thus, this method should only be used when this reference pitch is known or irrelevant.
    pub fn to_interval(self) -> I {
        self.0
    }

    /// Turns a pitch into a pitch class.
    pub fn pc(self) -> Pitch<I::IC> {
        Pitch(self.0.ic())
    }

    /// Returns the (directed) interval between `other` and `self`.
    pub fn interval_from(self, other: Self) -> I {
        self - other
    }

    /// Returns the (directed) interval between `self` and `other`.
    pub fn interval_to(self, other: Self) -> I {
        other - self
    }

    /// Maps a function over the wrapped interval.
    /// This can be used to lift functions on intervals to functions on pitches.
    /// the function may change the type of the interval.
    pub fn map<J: Interval, F: FnOnce(I) -> J>(self, f: F) -> Pitch<J> {
        Pitch(f(self.0))
    }
}

impl<I: IntervalClass> Pitch<I> {
    /// embeds a pitch class in the canonical octave above the type's reference pitch.
    pub fn emb(self) -> Pitch<I::I> {
        Pitch(self.0.emb())
    }
}

impl<I: Add<Output = I> + Interval> Add<I> for Pitch<I> {
    type Output = Self;
    fn add(self, i: I) -> Self {
        Self(self.0 + i)
    }
}

impl<I: Sub<Output = I> + Interval> Sub<I> for Pitch<I> {
    type Output = Self;
    fn sub(self, i: I) -> Self {
        Self(self.0 - i)
    }
}

impl<I: Sub<Output = I> + Interval> Sub<Pitch<I>> for Pitch<I> {
    type Output = I;
    fn sub(self, other: Self) -> I {
        self.to_interval() - other.to_interval()
    }
}
