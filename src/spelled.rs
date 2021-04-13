use super::{Chromatic, Diatonic, Interval, IntervalClass, Pitch};
use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Mul, Neg, Sub};
use std::str::FromStr;

mod parsing;

// Spelled trait
// -------------

pub trait Spelled: Copy {
    fn fifths(self) -> i32;
    fn octaves(self) -> i32;
    fn internal_octaves(self) -> i32;
    fn degree(self) -> i32;
    fn generic(self) -> i32;
    fn diasteps(self) -> i32;
    fn alteration(self) -> i32;
}

// SpelledInterval
// ---------------

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SpelledInterval {
    fifths: i32,
    octaves: i32,
}

impl SpelledInterval {
    pub fn new(fifths: i32, octaves: i32) -> SpelledInterval {
        SpelledInterval { fifths, octaves }
    }

    pub const WHOLETONE: SpelledInterval = SpelledInterval {
        fifths: 2,
        octaves: -1,
    };

    pub fn dia_chrom(dia: i32, chrom: i32) -> SpelledInterval {
        let dia_part = Self::WHOLETONE * dia;
        let chrom_part = Self::CHROMATIC_SEMITONE * (chrom - 2 * dia);
        dia_part + chrom_part
    }
}

impl PartialOrd for SpelledInterval {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SpelledInterval {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_rep = (self.diasteps(), self.alteration());
        let other_rep = (other.diasteps(), other.alteration());
        self_rep.cmp(&other_rep)
    }
}

impl Add for SpelledInterval {
    type Output = SpelledInterval;
    fn add(self, other: Self) -> Self {
        Self {
            fifths: self.fifths + other.fifths,
            octaves: self.octaves + other.octaves,
        }
    }
}

impl Sub for SpelledInterval {
    type Output = SpelledInterval;
    fn sub(self, other: Self) -> Self {
        Self {
            fifths: self.fifths - other.fifths,
            octaves: self.octaves - other.octaves,
        }
    }
}

impl Neg for SpelledInterval {
    type Output = SpelledInterval;
    fn neg(self) -> Self {
        Self {
            fifths: -self.fifths,
            octaves: -self.octaves,
        }
    }
}

impl Mul<i32> for SpelledInterval {
    type Output = SpelledInterval;
    fn mul(self, scale: i32) -> Self {
        Self {
            fifths: scale * self.fifths,
            octaves: scale * self.octaves,
        }
    }
}

impl Interval for SpelledInterval {
    type IC = SpelledIC;

    const OCTAVE: Self = Self {
        fifths: 0,
        octaves: 1,
    };

    const UNISON: Self = Self {
        fifths: 0,
        octaves: 0,
    };

    fn ic(self) -> SpelledIC {
        SpelledIC::new(self.fifths)
    }

    fn direction(self) -> Ordering {
        self.diasteps().cmp(&0)
    }
}

impl Diatonic for SpelledInterval {
    fn is_step(self) -> bool {
        self.diasteps().abs() < 2
    }
}

impl Chromatic for SpelledInterval {
    const CHROMATIC_SEMITONE: Self = Self {
        fifths: 7,
        octaves: -4,
    };
}

impl Spelled for SpelledInterval {
    fn fifths(self) -> i32 {
        self.fifths
    }

    fn octaves(self) -> i32 {
        self.octaves + ((self.fifths * 4).div_euclid(7))
    }

    fn internal_octaves(self) -> i32 {
        self.octaves
    }

    fn diasteps(self) -> i32 {
        self.fifths * 4 + self.octaves * 7
    }

    fn generic(self) -> i32 {
        if self.direction() == Ordering::Less {
            -(-self).degree()
        } else {
            self.degree()
        }
    }

    fn degree(self) -> i32 {
        fifths2degree(self.fifths)
    }

    fn alteration(self) -> i32 {
        (self.abs().fifths + 1).div_euclid(7)
    }
}

impl Display for SpelledInterval {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.direction() == Ordering::Less {
            return write!(fmt, "-{}", -*self);
        }

        let dia = self.degree();

        let diff = self.alteration();
        let qual = if is_perfect(dia) {
            qualpf(diff, "a", "P", "d")
        } else {
            qualimpf(diff, "a", "M", "m", "d")
        };

        write!(fmt, "{}{}:{}", qual, dia + 1, self.octaves())
    }
}

impl FromStr for SpelledInterval {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parsing::parse_spelled(s).or_else(|_| Err("Failed to parse interval ".to_string() + s))
    }
}

// SpelledIC
// ---------

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpelledIC {
    fifths: i32,
}

impl SpelledIC {
    pub fn new(fifths: i32) -> SpelledIC {
        SpelledIC { fifths }
    }
}

impl Add for SpelledIC {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        SpelledIC::new(self.fifths + other.fifths)
    }
}

impl Sub for SpelledIC {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        SpelledIC::new(self.fifths - other.fifths)
    }
}

impl Neg for SpelledIC {
    type Output = Self;
    fn neg(self) -> Self {
        SpelledIC::new(-self.fifths)
    }
}

impl Mul<i32> for SpelledIC {
    type Output = Self;
    fn mul(self, scale: i32) -> Self {
        SpelledIC::new(self.fifths * scale)
    }
}

impl Interval for SpelledIC {
    type IC = SpelledIC;

    fn ic(self) -> SpelledIC {
        self
    }

    const UNISON: Self = SpelledIC { fifths: 0 };
    const OCTAVE: Self = SpelledIC { fifths: 0 };

    fn direction(self) -> Ordering {
        if self.fifths == 0 {
            return Ordering::Equal;
        }

        let d = self.diasteps();
        if d == 0 {
            Ordering::Equal
        } else if d < 4 {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}

impl IntervalClass for SpelledIC {
    type I = SpelledInterval;
    fn emb(self) -> SpelledInterval {
        SpelledInterval::new(self.fifths, -((self.fifths * 4).div_euclid(7)))
    }
}

impl Diatonic for SpelledIC {
    fn is_step(self) -> bool {
        let d = self.degree();
        d == 0 || d == 1 || d == 6
    }
}

impl Chromatic for SpelledIC {
    const CHROMATIC_SEMITONE: Self = Self { fifths: 7 };
}

impl Spelled for SpelledIC {
    fn fifths(self) -> i32 {
        self.fifths
    }
    fn octaves(self) -> i32 {
        0
    }
    fn internal_octaves(self) -> i32 {
        0
    }
    fn degree(self) -> i32 {
        fifths2degree(self.fifths)
    }
    fn generic(self) -> i32 {
        fifths2degree(self.fifths)
    }
    fn diasteps(self) -> i32 {
        fifths2degree(self.fifths)
    }
    fn alteration(self) -> i32 {
        (self.fifths + 1).div_euclid(7)
    }
}

impl Display for SpelledIC {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let dia = self.degree();

        let diff = self.alteration();
        let qual = if is_perfect(dia) {
            qualpf(diff, "a", "P", "d")
        } else {
            qualimpf(diff, "a", "M", "m", "d")
        };

        write!(fmt, "{}{}", qual, dia + 1)
    }
}

impl FromStr for SpelledIC {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parsing::parse_sic(s)
            .or_else(|_| Err("Failed to parse spelled interval class ".to_string() + s))
    }
}

// Spelled Pitch (Class)
// ---------------------

impl<I: Interval + Spelled> Spelled for Pitch<I> {
    fn fifths(self) -> i32 {
        self.0.fifths()
    }
    fn octaves(self) -> i32 {
        self.0.octaves()
    }
    fn internal_octaves(self) -> i32 {
        self.0.internal_octaves()
    }
    fn degree(self) -> i32 {
        self.0.degree()
    }
    fn generic(self) -> i32 {
        self.0.generic()
    }
    fn diasteps(self) -> i32 {
        self.0.diasteps()
    }
    fn alteration(self) -> i32 {
        self.0.alteration()
    }
}

impl<I: Interval + Spelled> Pitch<I> {
    pub fn letter(self) -> char {
        let a = 'A' as u8;
        let offset = ((self.degree() + 2).rem_euclid(7)) as u8;
        char::from(a + offset)
    }
}

pub type SpelledPitch = Pitch<SpelledInterval>;
pub type SpelledPC = Pitch<SpelledIC>;

pub fn spelledp(f: i32, o: i32) -> SpelledPitch {
    SpelledInterval::new(f, o).to_pitch()
}

pub fn spc(f: i32) -> SpelledPC {
    SpelledIC::new(f).to_pitch()
}

impl Display for SpelledPitch {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let accs = accstr(self.alteration(), "♯", "♭");
        write!(fmt, "{}{}{}", self.letter(), accs, self.octaves())
    }
}

impl Display for SpelledPC {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let accs = accstr(self.alteration(), "♯", "♭");
        write!(fmt, "{}{}", self.letter(), accs)
    }
}

impl FromStr for SpelledPitch {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parsing::parse_spelledp(s)
            .or_else(|_| Err("Failed to parse spelled pitch ".to_string() + s))
    }
}

impl FromStr for SpelledPC {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parsing::parse_spc(s)
            .or_else(|_| Err("Failed to parse spelled pitch class ".to_string() + s))
    }
}

// helpers
// -------

fn is_perfect(steps: i32) -> bool {
    steps == 0 || steps == 3 || steps == 4
}

fn accstr(n: i32, up: &str, down: &str) -> String {
    if n > 0 {
        up.repeat(n as usize)
    } else {
        down.repeat(-n as usize)
    }
}

fn qualpf(n: i32, aug: &str, perf: &str, dim: &str) -> String {
    if n > 0 {
        aug.repeat(n as usize)
    } else if n < 0 {
        dim.repeat(-n as usize)
    } else {
        String::from(perf)
    }
}

fn qualimpf(n: i32, aug: &str, maj: &str, min: &str, dim: &str) -> String {
    if n > 0 {
        aug.repeat(n as usize)
    } else if n < -1 {
        dim.repeat((-n - 1) as usize)
    } else if n == -1 {
        String::from(min)
    } else {
        String::from(maj)
    }
}

fn fifths2degree(fifths: i32) -> i32 {
    (fifths * 4).rem_euclid(7)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rsi(s: &str) -> SpelledInterval {
        s.parse().unwrap()
    }

    fn rsic(s: &str) -> SpelledIC {
        s.parse().unwrap()
    }

    fn rsp(s: &str) -> SpelledPitch {
        s.parse().unwrap()
    }

    fn rspc(s: &str) -> SpelledPC {
        s.parse().unwrap()
    }

    #[test]
    fn spelled_interval_props() {
        assert_eq!(SpelledInterval::UNISON, SpelledInterval::new(0, 0));
        //todo: neutral wrt addition
        //todo: direction and negate
        //todo: - = +(-)
        //todo: a-a=0
        //todo: from_str . fmt = id
    }

    #[test]
    fn spelled_constructors() {
        assert_eq!(
            SpelledInterval::dia_chrom(3, 5),
            SpelledInterval::new(-1, 1)
        );
        assert_eq!(spelledp(0, 4), SpelledInterval::new(0, 4).to_pitch());
        assert_eq!(spc(3), SpelledIC::new(3).to_pitch());
    }

    #[test]
    fn spelled_named() {
        assert_eq!(SpelledInterval::UNISON, SpelledInterval::new(0, 0));
        assert_eq!(SpelledInterval::OCTAVE, SpelledInterval::new(0, 1));
        assert_eq!(
            SpelledInterval::CHROMATIC_SEMITONE,
            SpelledInterval::new(7, -4)
        );
        assert_eq!(SpelledInterval::WHOLETONE, SpelledInterval::new(2, -1));

        assert_eq!(SpelledIC::UNISON, SpelledIC::new(0));
        assert_eq!(SpelledIC::OCTAVE, SpelledIC::new(0));
        assert_eq!(SpelledIC::CHROMATIC_SEMITONE, SpelledIC::new(7));
    }

    #[test]
    fn spelled_parsing() {
        assert_eq!("M3:1".parse(), Ok(SpelledInterval::new(4, -1)));
        assert_eq!("-M3:0".parse(), Ok(SpelledInterval::new(-4, 2)));
        assert_eq!("-M3:1".parse(), Ok(SpelledInterval::new(-4, 1)));
        assert_eq!("C♭4".parse(), Ok(spelledp(-7, 8)));
        assert_eq!("Cb4".parse(), Ok(spelledp(-7, 8)));
        assert_eq!("m3".parse(), Ok(SpelledIC::new(-3)));
        assert_eq!("-m3".parse(), Ok(SpelledIC::new(3)));
        assert_eq!("C♯".parse(), Ok(spc(7)));
        assert_eq!("C#".parse(), Ok(spc(7)));
    }

    #[test]
    fn spelled_printing() {
        assert_eq!(rsi("m3:1").to_string(), "m3:1");
        assert_eq!(rsp("Eb4").to_string(), "E♭4");
        assert_eq!(rsic("m3").to_string(), "m3");
        assert_eq!(rspc("E#").to_string(), "E♯");
    }

    #[test]
    fn spelled_accessors_positive_i() {
        assert_eq!(rsi("M3:1").octaves(), 1);
        assert_eq!(rsi("M3:1").internal_octaves(), -1);
        assert_eq!(rsi("M3:1").fifths(), 4);
        assert_eq!(rsi("M3:1").degree(), 2);
        assert_eq!(rsi("M3:1").generic(), 2);
        assert_eq!(rsi("M3:1").diasteps(), 9);
        assert_eq!(rsi("M3:1").alteration(), 0);
    }

    #[test]
    fn spelled_accessors_negative_i() {
        assert_eq!(rsi("-M3:1").octaves(), -2);
        assert_eq!(rsi("-M3:1").internal_octaves(), 1);
        assert_eq!(rsi("-M3:1").fifths(), -4);
        assert_eq!(rsi("-M3:1").degree(), 5);
        assert_eq!(rsi("-M3:1").generic(), -2);
        assert_eq!(rsi("-M3:1").diasteps(), -9);
        assert_eq!(rsi("-M3:1").alteration(), 0);
    }

    #[test]
    fn spelled_accessors_ic() {
        assert_eq!(rsic("a5").octaves(), 0);
        assert_eq!(rsic("a5").internal_octaves(), 0);
        assert_eq!(rsic("a5").fifths(), 8);
        assert_eq!(rsic("a5").degree(), 4);
        assert_eq!(rsic("a5").generic(), 4);
        assert_eq!(rsic("a5").diasteps(), 4);
        assert_eq!(rsic("a5").alteration(), 1);
    }

    #[test]
    fn spelled_accessors_p() {
        assert_eq!(rsp("Ebb5").octaves(), 5);
        assert_eq!(rsp("Ebb5").fifths(), -10);
        assert_eq!(rsp("Ebb5").degree(), 2);
        assert_eq!(rsp("Ebb5").alteration(), -2);
        assert_eq!(rsp("Ebb5").letter(), 'E');
    }

    #[test]
    fn spelled_accessors_pc() {
        assert_eq!(rspc("F#").octaves(), 0);
        assert_eq!(rspc("F#").fifths(), 6);
        assert_eq!(rspc("F#").degree(), 3);
        assert_eq!(rspc("F#").alteration(), 1);
        assert_eq!(rspc("F#").letter(), 'F');
    }

    #[test]
    fn spelled_accessors_edge_cases() {
        assert_eq!(rsic("P4").alteration(), 0);
        assert_eq!(rsic("M7").alteration(), 0);
        assert_eq!(rsi("-P4:0").alteration(), 0);
        assert_eq!(rsi("-M7:0").alteration(), 0);

        assert_eq!(rsic("a4").alteration(), 1);
        assert_eq!(rsic("m7").alteration(), -1);
        assert_eq!(rsi("-a4:0").alteration(), 1);
        assert_eq!(rsi("-m7:0").alteration(), -1);

        assert_eq!(rspc("F").alteration(), 0);
        assert_eq!(rspc("B").alteration(), 0);
    }

    #[test]
    fn spelled_additive() {
        assert_eq!(rsi("m3:0") + rsi("M3:0"), rsi("P5:0"));
        assert_eq!(rsi("m3:0") + rsi("M7:0"), rsi("M2:1"));
        assert_eq!(rsi("P5:0") + rsi("P5:0"), rsi("M2:1"));
        assert_eq!(rsi("-m3:0") + rsi("M3:0"), rsi("a1:0"));
        assert_eq!(rsi("m3:0") + rsi("-M3:0"), rsi("-a1:0"));
        assert_eq!(rsi("m3:0") - rsi("M3:0"), rsi("-a1:0"));
        assert_eq!(rsi("m3:0") - rsi("M6:0"), rsi("-a4:0"));
        assert_eq!(-rsi("P4:0"), rsi("-P4:0"));
        assert_eq!(-rsi("P4:0"), rsi("P5:0") - rsi("P1:1"));
        assert_eq!(-rsi("P5:0"), SpelledInterval::UNISON - rsi("P5:0"));
    }

    #[test]
    fn spelled_multiplication() {
        assert_eq!(rsi("P5:0") * 2, rsi("M2:1"));
        assert_eq!(rsi("M2:0") * 4, rsi("a5:0"));
        assert_eq!(rsi("-m3:0") * 4, rsi("-d2:1"));
        assert_eq!(rsi("M3:0") * -3, rsi("-a7:0"));
        assert_eq!(rsi("-M3:0") * 4, rsi("-aa2:1"));
        assert_eq!(rsi("M3:0") * 5, rsi("aaa4:1"));
    }

    #[test]
    fn spelled_direction() {
        assert_eq!(rsi("m2:0").direction(), Ordering::Greater);
        assert_eq!(rsi("P1:0").direction(), Ordering::Equal);
        assert_eq!(rsi("a1:0").direction(), Ordering::Equal);
        assert_eq!(rsi("d1:0").direction(), Ordering::Equal);
        assert_eq!(rsi("-m3:0").direction(), Ordering::Less);
        assert_eq!(rsi("-m3:0").abs(), rsi("m3:0"));
    }

    #[test]
    fn spelled_class_conversion() {
        assert_eq!(rsi("M3:3").ic(), rsic("M3"));
        assert_eq!(rsi("-M3:3").ic(), rsic("m6"));
    }

    #[test]
    fn spelled_steps_true() {
        assert!(rsi("d1:0").is_step());
        assert!(rsi("P1:0").is_step());
        assert!(rsi("a1:0").is_step());
        assert!(rsi("d2:0").is_step());
        assert!(rsi("m2:0").is_step());
        assert!(rsi("M2:0").is_step());
        assert!(rsi("a2:0").is_step());
        assert!(rsi("-d2:0").is_step());
        assert!(rsi("-m2:0").is_step());
        assert!(rsi("-M2:0").is_step());
        assert!(rsi("-a2:0").is_step());
    }

    #[test]
    fn spelled_steps_false() {
        assert!(!rsi("d3:0").is_step());
        assert!(!rsi("-d3:0").is_step());
        assert!(!rsi("M7:0").is_step());
        assert!(!rsi("-M7:0").is_step());
        assert!(!rsi("P1:1").is_step());
        assert!(!rsi("-P1:1").is_step());
        assert!(!rsi("m2:1").is_step());
        assert!(!rsi("-m2:1").is_step());
    }

    #[test]
    fn spelled_class_additive() {
        assert_eq!(rsic("m3") + rsic("M3"), rsic("P5"));
        assert_eq!(rsic("m3") + rsic("M7"), rsic("M2"));
        assert_eq!(rsic("P5") + rsic("P5"), rsic("M2"));
        assert_eq!(rsic("-m3") + rsic("M3"), rsic("a1"));
        assert_eq!(rsic("m3") + rsic("-M3"), rsic("-a1"));
        assert_eq!(rsic("m3") - rsic("M3"), rsic("-a1"));
        assert_eq!(rsic("m3") - rsic("M6"), rsic("-a4"));
        assert_eq!(-rsic("P4"), rsic("-P4"));
        assert_eq!(-rsic("P4"), rsic("P5"));
        assert_eq!(-rsic("P5"), SpelledIC::UNISON - rsic("P5"));
    }

    #[test]
    fn spelled_class_multiplication() {
        assert_eq!(rsic("P5") * 2, rsic("M2"));
        assert_eq!(rsic("M2") * 4, rsic("a5"));
        assert_eq!(rsic("-m3") * 4, rsic("-d2"));
        assert_eq!(rsic("M3") * -3, rsic("-a7"));
        assert_eq!(rsic("-M3") * 4, rsic("-aa2"));
        assert_eq!(rsic("M3") * 5, rsic("aaa4"));
    }

    #[test]
    fn spelled_class_direction() {
        assert_eq!(rsic("m2").direction(), Ordering::Greater);
        assert_eq!(rsic("P1").direction(), Ordering::Equal);
        assert_eq!(rsic("a1").direction(), Ordering::Equal);
        assert_eq!(rsic("d1").direction(), Ordering::Equal);
        assert_eq!(rsic("-m2").direction(), Ordering::Less);
        assert_eq!(rsic("-m3").direction(), Ordering::Less);
        assert_eq!(rsic("-m3").abs(), rsic("m3"));
    }

    #[test]
    fn spelled_class_class_conversion() {
        assert_eq!(rsic("M3").ic(), rsic("M3"));
        assert_eq!(rsic("M3").emb(), rsi("M3:0"));
        assert_eq!(rsic("m3").emb(), rsi("m3:0"));
    }

    #[test]
    fn spelled_class_steps_true() {
        assert!(rsic("d1").is_step());
        assert!(rsic("P1").is_step());
        assert!(rsic("a1").is_step());
        assert!(rsic("d2").is_step());
        assert!(rsic("m2").is_step());
        assert!(rsic("M2").is_step());
        assert!(rsic("a2").is_step());
        assert!(rsic("-d2").is_step());
        assert!(rsic("-m2").is_step());
        assert!(rsic("-M2").is_step());
        assert!(rsic("-a2").is_step());
    }

    #[test]
    fn spelled_class_steps_false() {
        assert!(!rsic("d3").is_step());
        assert!(!rsic("-d3").is_step());
    }

    #[test]
    fn spelled_pitch_conversion() {
        assert_eq!(rsi("m3:4").to_pitch(), rsp("Eb4"));
        assert_eq!(rsp("C#3").to_interval(), rsi("a1:3"));

        assert_eq!(rsic("m3").to_pitch(), rspc("Eb"));
        assert_eq!(rspc("E").to_interval(), rsic("M3"));

        assert_eq!(rsp("Eb4").pc(), rspc("Eb"));
        assert_eq!(rspc("Eb").pc(), rspc("Eb"));
        assert_eq!(rspc("Eb").emb(), rsp("Eb0"));
    }

    #[test]
    fn spelled_pitch_arithmetics() {
        assert_eq!(rsp("Eb4") + rsi("P5:0"), rsp("Bb4"));
        assert_eq!(rsp("Eb4") + rsi("-m3:0"), rsp("C4"));
        assert_eq!(rsp("Eb4") - rsi("P5:0"), rsp("Ab3"));
        assert_eq!(rsp("G4").from(rsp("C#4")), rsi("d5:0"));
        assert_eq!(rsp("G4").to(rsp("C#4")), rsi("-d5:0"));
    }

    #[test]
    fn spelled_pitch_class_arithmetics() {
        assert_eq!(rspc("Eb") + rsic("P5"), rspc("Bb"));
        assert_eq!(rspc("Eb") + rsic("-m3"), rspc("C"));
        assert_eq!(rspc("Eb") - rsic("P5"), rspc("Ab"));
        assert_eq!(rspc("G").from(rspc("C#")), rsic("d5"));
        assert_eq!(rspc("G").to(rspc("C#")), rsic("a4"));
    }
}
