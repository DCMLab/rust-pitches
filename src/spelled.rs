use crate::Pitch;
use crate::{Chromatic, Diatonic, Interval, IntervalClass};
use nom::branch::alt;
use nom::character::complete::{char, digit1, one_of};
use nom::combinator::{eof, map, map_res, opt};
use nom::multi::many1_count;
use nom::IResult;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::fmt::Display;
use std::ops::{Add, Mul, Neg, Sub};
use std::str::FromStr;

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
        self.octaves + (self.fifths * 4 / 7)
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
        (self.abs().fifths + 1) / 7
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

        write!(fmt, "{}{}:{:+}", qual, dia + 1, self.octaves())
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
        SpelledInterval::new(self.fifths, -(self.fifths * 4 / 7))
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
        (self.fifths + 1) / 7
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

// Spelled Pitch (Class)
// ---------------------

impl<I> Spelled for Pitch<I>
where
    I: Spelled,
{
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
        let offset = ((self.degree() + 2) % 7) as u8;
        char::from(a + offset)
    }
}

pub fn spelledp(f: i32, o: i32) -> Pitch<SpelledInterval> {
    Pitch(SpelledInterval::new(f, o))
}

pub fn spelledpc(f: i32) -> Pitch<SpelledIC> {
    Pitch(SpelledIC::new(f))
}

impl Display for Pitch<SpelledInterval> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let accs = accstr(self.alteration(), "♯", "♭");
        write!(fmt, "{}{}{}", self.letter(), accs, self.octaves())
    }
}

impl Display for Pitch<SpelledIC> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let accs = accstr(self.alteration(), "♯", "♭");
        write!(fmt, "{}{}", self.letter(), accs)
    }
}

// parsing notation
// ----------------

enum Qual {
    Augmented(i32),
    Major,
    Perfect,
    Minor,
    Diminished(i32),
}

fn nom_fail<'a, R>() -> IResult<&'a str, R, ()> {
    Err(nom::Err::Error(()))
}

fn parse_aug(i: &str) -> IResult<&str, Qual, ()> {
    let (i, len) = map_res(many1_count(char('a')), i32::try_from)(i)?;
    Ok((i, Qual::Augmented(len)))
}

fn parse_dim(i: &str) -> IResult<&str, Qual, ()> {
    let (i, len) = map_res(many1_count(char('d')), i32::try_from)(i)?;
    Ok((i, Qual::Diminished(len)))
}

fn parse_qual(i: &str) -> IResult<&str, Qual, ()> {
    let (i, qual) = one_of("MPm")(i)?;
    match qual {
        'M' => Ok((i, Qual::Major)),
        'm' => Ok((i, Qual::Minor)),
        'P' => Ok((i, Qual::Perfect)),
        _ => nom_fail(),
    }
}

fn parse_size(i: &str) -> IResult<&str, i32, ()> {
    map_res(one_of("1234567"), |n| {
        n.to_digit(10).map(|n| (n as i32) - 1).ok_or("")
    })(i)
}

fn parse_dia(i: &str) -> IResult<&str, i32, ()> {
    let (o_qual, qual) = alt((parse_qual, parse_aug, parse_dim))(i)?;
    let (o_dia, dia) = parse_size(o_qual)?;
    let alteration = if is_perfect(dia) {
        match qual {
            Qual::Augmented(n) => n,
            Qual::Perfect => 0,
            Qual::Diminished(n) => -n,
            _ => return nom_fail(),
        }
    } else {
        match qual {
            Qual::Augmented(n) => n,
            Qual::Major => 0,
            Qual::Minor => -1,
            Qual::Diminished(n) => (-n) - 1,
            _ => return nom_fail(),
        }
    };
    Ok((o_dia, ((dia * 2 + 1) % 7) - 1 + (7 * alteration)))
}

fn parse_negative(i: &str) -> (&str, bool) {
    match char::<&str, ()>('-')(i) {
        Ok((i, _)) => (i, true),
        Err(_) => (i, false),
    }
}

fn parse_nat(i: &str) -> IResult<&str, i32, ()> {
    map_res(digit1, |x| i32::from_str(x))(i)
}

fn parse_int(i: &str) -> IResult<&str, i32, ()> {
    let (i, neg) = parse_negative(i);
    let (i, n) = parse_nat(i)?;
    Ok((i, if neg { -n } else { n }))
}

fn parse_octs(i: &str) -> IResult<&str, i32, ()> {
    let (i, _) = char(':')(i)?;
    parse_nat(i)
}

fn parse_alteration(i: &str) -> IResult<&str, i32, ()> {
    fn count_acc<'a>(c1: char, c2: char) -> impl nom::Parser<&'a str, i32, ()> {
        map_res(
            alt((many1_count(char(c1)), many1_count(char(c2)))),
            i32::try_from,
        )
    }
    let flats = map(count_acc('b', '♭'), |n| -n);
    let sharps = count_acc('#', '♯');
    map(opt(alt((flats, sharps))), |n| n.unwrap_or(0))(i)
}

fn parse_name(i: &str) -> IResult<&str, i32, ()> {
    let (i, letter) = one_of("ABCDEFG")(i)?;
    let (i, alteration) = parse_alteration(i)?;
    let dia = (((letter as u8) - ('A' as u8) - 2) % 7) as i32;
    Ok((i, ((dia * 2 + 1) % 7) - 1 + 7 * alteration))
}

impl FromStr for SpelledInterval {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fail = "Failed to parse spelled interval class ";

        let (s, negate) = parse_negative(s);
        let (s, fifths) = parse_dia(s).or_else(|_| Err(fail.to_string() + s))?;
        let (s, octs) = parse_octs(s).or_else(|_| Err(fail.to_string() + s))?;
        eof::<&str, ()>(s).or_else(|_| Err(fail.to_string() + s))?;

        let interval = SpelledInterval::new(fifths, octs - (fifths * 4) / 7);
        Ok(if negate { -interval } else { interval })
    }
}

impl FromStr for SpelledIC {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fail = "Failed to parse spelled interval class ";

        let (s, negate) = parse_negative(s);
        let (s, fifths) = parse_dia(s).or_else(|_| Err(fail.to_string() + s))?;
        eof::<&str, ()>(s).or_else(|_| Err(fail.to_string() + s))?;

        let interval = SpelledIC::new(fifths);
        Ok(if negate { -interval } else { interval })
    }
}

impl FromStr for Pitch<SpelledInterval> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fail = "Failed to parse spelled pitch ";

        let (s, fifths) = parse_name(s).or_else(|_| Err(fail.to_string() + s))?;
        let (s, oct) = parse_int(s).or_else(|_| Err(fail.to_string() + s))?;
        eof::<&str, ()>(s).or_else(|_| Err(fail.to_string() + s))?;

        Ok(spelledp(fifths, oct - ((fifths * 4) / 7)))
    }
}

impl FromStr for Pitch<SpelledIC> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fail = "Failed to parse spelled pitch class ";

        let (s, fifths) = parse_name(s).or_else(|_| Err(fail.to_string() + s))?;
        eof::<&str, ()>(s).or_else(|_| Err(fail.to_string() + s))?;

        Ok(spelledpc(fifths))
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
    use crate::spelled::*;

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
        assert_eq!(spelledp(0, 4), Pitch(SpelledInterval::new(0, 4)));
        assert_eq!(spelledpc(3), Pitch(SpelledIC::new(3)));
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
    fn spelled_notation() {
        assert_eq!("M3:1".parse(), Ok(SpelledInterval::new(4, -1)));
        assert_eq!("-M3:0".parse(), Ok(SpelledInterval::new(-4, 2)));
        assert_eq!("C♭4".parse(), Ok(spelledp(-7, 8)));
        assert_eq!("Cb4".parse(), Ok(spelledp(-7, 8)));
        assert_eq!("m3".parse(), Ok(SpelledIC::new(-3)));
        assert_eq!("-m3".parse(), Ok(SpelledIC::new(3)));
        assert_eq!("C♯".parse(), Ok(spelledpc(7)));
        assert_eq!("C#".parse(), Ok(spelledpc(7)));
    }
}
