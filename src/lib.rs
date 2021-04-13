//! This crate provides types and traits for dealing with musical pitch.
//!
//! The two main goals are:
//! - providing types and operations (such as arithmetics, printing and parsing) for common types of pitches and intervals
//! - providing a generic interface for writing code that is agnostic to the specific pitch or interval types.
//! It allows you to write generic algorithms that can then be applied
//! to pitches and intervals in different formats:
//! ```rust
//! use pitches::midi::*;
//! use pitches::spelled::*;
//! use pitches::*;
//!
//! fn transpose_and_add_octave<I>(pitches: &Vec<Pitch<I>>, by: I) -> Vec<Pitch<I>>
//! where
//!     I: Interval,
//! {
//!     pitches.iter().map(|p| *p + by + I::OCTAVE).collect()
//! }
//!
//! fn main() {
//!     // apply to MIDI pitches/intervals
//!     println!("MIDI:");
//!     let midi_pitches = vec![midip(60), midip(64), midip(67), midip(72)];
//!     for p in transpose_and_add_octave(&midi_pitches, midi(3)).iter() {
//!         println!("- {}", p);
//!     }
//!
//!     // apply to spelled pitches/intervals
//!     println!("spelled:");
//!     let spelled_pitches: Vec<SpelledPitch> = vec!["C4", "E♭4", "G♯4", "C5"]
//!         .iter()
//!         .map(|s| s.parse().unwrap())
//!         .collect();
//!     let spelled_interval = "M3:0".parse().unwrap();
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
//!
//! Similar to vectors and points, intervals and pitches support a number of operations
//! such as addition and scalar multiplication,
//! which are here implemented using the standard operator traits.
//!
//! For a detailed introduction to the concepts behind this library,
//! have a look at the abstract [interface description](https://hackmd.io/@chfin/ryWI3NJRL),
//! as well as the docs of the [Julia implementation](https://dcmlab.github.io/Pitches.jl/dev/)

use core::ops::{Add, Mul, Neg, Sub};
use std::cmp::Ordering;

pub mod midi;
pub mod spelled;
mod util;

// interval traits
// ---------------

pub trait Interval:
    Add<Output = Self> + Sub<Output = Self> + Neg<Output = Self> + Mul<i32, Output = Self> + Copy
{
    type IC: IntervalClass;

    const OCTAVE: Self;
    const UNISON: Self;

    fn ic(self) -> Self::IC;
    fn direction(self) -> Ordering;
    fn abs(self) -> Self {
        if self.direction() == Ordering::Less {
            -self
        } else {
            self
        }
    }

    fn to_pitch(self) -> Pitch<Self> {
        Pitch(self)
    }
}

pub trait IntervalClass: Interval {
    type I: Interval<IC = Self>;
    fn emb(self) -> Self::I;
}

pub trait Diatonic: Interval {
    fn is_step(self) -> bool;
}

pub trait Chromatic: Interval {
    const CHROMATIC_SEMITONE: Self;
}

// Pitch
// -----

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pitch<T: Interval>(T);

impl<I: Interval> Pitch<I> {
    pub fn new(i: I) -> Self {
        Pitch(i)
    }

    pub fn to_interval(self) -> I {
        self.0
    }

    pub fn pc(self) -> Pitch<I::IC> {
        Pitch(self.0.ic())
    }

    pub fn from(self, other: Self) -> I {
        self - other
    }

    pub fn to(self, other: Self) -> I {
        other - self
    }

    pub fn map<J: Interval, F: FnOnce(I) -> J>(self, f: F) -> Pitch<J> {
        Pitch(f(self.0))
    }
}

impl<I: IntervalClass> Pitch<I> {
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
