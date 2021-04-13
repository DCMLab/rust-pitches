use core::ops::{Add, Mul, Neg, Sub};
use std::cmp::Ordering;

pub mod spelled;

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
