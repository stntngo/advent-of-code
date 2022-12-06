#![feature(test)]
use itertools::*;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;

extern crate test;

const INPUT: &str = include_str!("../input");
const REQUIRED: &[(&str, Test); 7] = &[
    ("byr", valid_byr),
    ("iyr", valid_iyr),
    ("eyr", valid_eyr),
    ("hgt", valid_hgt),
    ("hcl", valid_hcl),
    ("ecl", valid_ecl),
    ("pid", valid_pid),
];

fn main() {
    println!("Advent of Code! Day 4!");
    println!(
        "Part One: {:?}",
        INPUT
            .split("\n\n")
            .map(parse_passport)
            .filter(|map| REQUIRED.iter().all(|(req, _)| map.contains_key(req)))
            .count(),
    );

    println!(
        "Part Two: {:?}",
        INPUT
            .split("\n\n")
            .map(parse_passport)
            .filter(|map| REQUIRED
                .iter()
                .all(|(req, test)| test(map.get(req).unwrap_or(&""))))
            .count(),
    );
}

fn parse_passport(s: &str) -> HashMap<&str, &str> {
    s.split_whitespace()
        .flat_map(|x| x.split(':'))
        .tuples::<(_, _)>()
        .collect::<HashMap<_, _>>()
}

type Test = fn(&str) -> bool;

fn valid_byr(s: &str) -> bool {
    if let Ok(year) = s.parse::<i32>() {
        return 1920 <= year && year <= 2002;
    }

    false
}

fn valid_iyr(s: &str) -> bool {
    if let Ok(year) = s.parse::<i32>() {
        return 2010 <= year && year <= 2020;
    }

    false
}

fn valid_eyr(s: &str) -> bool {
    if let Ok(year) = s.parse::<i32>() {
        return 2020 <= year && year <= 2030;
    }

    false
}

fn valid_hgt(s: &str) -> bool {
    lazy_static! {
        static ref HGT_REGEX: Regex = Regex::new(r"^(\d+)([a-z]{2})$").unwrap();
    }

    // Ugly AF but I didn't bother letting these functions return options or results
    // so here we are
    if !HGT_REGEX.is_match(s) {
        return false;
    }

    let caps = HGT_REGEX.captures(s).unwrap();

    match caps.get(2).unwrap().as_str() {
        "in" => {
            let value = caps.get(1).unwrap().as_str().parse::<i32>().unwrap_or(-1);
            59 <= value && value <= 76
        }
        "cm" => {
            let value = caps.get(1).unwrap().as_str().parse::<i32>().unwrap_or(-1);
            150 <= value && value <= 193
        }
        _ => false,
    }
}

fn valid_hcl(s: &str) -> bool {
    lazy_static! {
        static ref HCL_REGEX: Regex = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
    }

    HCL_REGEX.is_match(s)
}

fn valid_ecl(s: &str) -> bool {
    matches!(s, "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth")
}

fn valid_pid(s: &str) -> bool {
    lazy_static! {
        static ref PID_REGEX: Regex = Regex::new(r"^[0-9]{9}$").unwrap();
    }

    PID_REGEX.is_match(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_part_one(b: &mut Bencher) {
        b.iter(|| {
            INPUT
                .split("\n\n")
                .map(parse_passport)
                .filter(|map| REQUIRED.iter().all(|(req, _)| map.contains_key(req)))
                .count()
        });
    }

    #[bench]
    fn bench_part_two(b: &mut Bencher) {
        b.iter(|| {
            INPUT
                .split("\n\n")
                .map(parse_passport)
                .filter(|map| {
                    REQUIRED
                        .iter()
                        .all(|(req, test)| test(map.get(req).unwrap_or(&"")))
                })
                .count()
        });
    }
}
