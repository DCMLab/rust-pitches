use core::ops::{Add, Mul, Neg, Sub};
use std::cmp::Ordering;

pub mod spelled;

// interval traits
// ---------------

pub trait Interval: Add + Sub + Neg<Output = Self> + Mul<i32> + Copy {
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pitch<T>(T);

impl<T> Copy for Pitch<T> where T: Copy {}

impl<I: Add<Output = I>> Add<I> for Pitch<I> {
    type Output = Self;
    fn add(self, i: I) -> Self {
        Self(self.0 + i)
    }
}

impl<I: Sub<Output = I>> Sub<I> for Pitch<I> {
    type Output = Self;
    fn sub(self, i: I) -> Self {
        Self(self.0 - i)
    }
}

impl<I: Interval> Pitch<I> {
    pub fn new(i: I) -> Self {
        Pitch(i)
    }

    pub fn pc(self) -> Pitch<I::IC> {
        Pitch(self.0.ic())
    }
}
