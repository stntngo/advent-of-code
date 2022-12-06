#![allow(dead_code)]
use std::fmt::Debug;
use std::fs::read_to_string;
use std::iter::Iterator;
use std::str::FromStr;
use std::convert::Infallible;

use lazy_static::lazy_static;
use regex::Regex;

fn main() {
    println!("Advent of Code! Day 2!");

    println!(
        "Part One: {:?}",
        parse_input::<Password>("input")
            .iter()
            .filter(|x| x.is_valid_one())
            .count()
    );

    println!(
        "Part Two: {:?}",
        parse_input::<Password>("input")
            .iter()
            .filter(|x| x.is_valid_two())
            .count()
    );
}

trait Between {
    fn is_between(&self, lo: Self, hi: Self) -> bool;
}

impl Between for usize {
    fn is_between(&self, lo: usize, hi: usize) -> bool {
        lo <= *self && *self <= hi
    }
}

#[derive(Debug, PartialEq)]
struct Policy {
    lo: usize,
    hi: usize,
    char: char,
}

impl FromStr for Policy {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref POLICY_REGEX: Regex = Regex::new(r"^(\d+)-(\d+)\s([a-z])$").unwrap();
        }

        let caps = POLICY_REGEX.captures(s).unwrap();

        Ok(Policy {
            lo: caps.get(1).unwrap().as_str().parse().unwrap(),
            hi: caps.get(2).unwrap().as_str().parse().unwrap(),
            char: caps.get(3).unwrap().as_str().chars().next().unwrap(),
        })
    }
}

#[derive(Debug, PartialEq)]
struct Password {
    policy: Policy,
    password: String,
}

impl Password {
    fn is_valid_one(&self) -> bool {
        self.password
            .chars()
            .filter(|&x| x == self.policy.char)
            .count()
            .is_between(self.policy.lo, self.policy.hi)
    }

    fn is_valid_two(&self) -> bool {
        self
            .password
            .chars()
            .enumerate()
            .filter(|(i, x)| {
                (*i == (self.policy.lo -1) || *i == (self.policy.hi - 1)) && *x == self.policy.char
            })
            .count()
            .eq(&1)
    }
}

impl FromStr for Password {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref PASSWORD_REGEX: Regex = Regex::new(r"^(.+):\s([a-z]+)$").unwrap();
        }

        let caps = PASSWORD_REGEX.captures(s).unwrap();

        Ok(Password {
            policy: caps.get(1).unwrap().as_str().parse().unwrap(),
            password: caps.get(2).unwrap().as_str().to_owned(),
        })
    }
}

fn parse_input<T: FromStr>(path: &str) -> Vec<T>
where
    <T as FromStr>::Err: Debug,
{
    read_to_string(path)
        .expect("unable to open file")
        .lines()
        .map(|l| l.parse::<T>().expect("unable to parse line"))
        .collect()
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_part_one() {
        assert_eq!(
            "1-3 a: abcde".parse::<Password>().unwrap().is_valid_one(),
            true
        );
        assert_eq!(
            "1-3 b: cdefg".parse::<Password>().unwrap().is_valid_one(),
            false
        );
        assert_eq!(
            "2-9 c: ccccccccc"
                .parse::<Password>()
                .unwrap()
                .is_valid_one(),
            true
        );
    }

    #[test]
    fn test_part_two() {
        assert_eq!(
            "1-3 a: abcde".parse::<Password>().unwrap().is_valid_two(),
            true
        );
        assert_eq!(
            "1-3 b: cdefg".parse::<Password>().unwrap().is_valid_two(),
            false
        );
        assert_eq!(
            "2-9 c: ccccccccc"
                .parse::<Password>()
                .unwrap()
                .is_valid_two(),
            false
        );
    }
}
