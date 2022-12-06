#![allow(dead_code)]
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Debug, Clone)]
enum Inst {
    Nop(i32),
    Acc(i32),
    Jmp(i32),
}

#[derive(Debug, Clone)]
struct Computer {
    acc: i32,
    ptr: usize,
    tape: Vec<Inst>,
}

impl Computer {
    fn new(tape: Vec<Inst>) -> Self {
        Computer {
            acc: 0,
            ptr: 0,
            tape,
        }
    }

    fn run(&mut self, return_loop: bool) -> Option<i32> {
        let mut visited: HashSet<usize> = HashSet::new();

        loop {
            if visited.contains(&self.ptr) {
                if return_loop {
                    return Some(self.acc);
                }

                return None;
            }

            if self.ptr >= self.tape.len() {
                break;
            }

            visited.insert(self.ptr);

            match self.tape[self.ptr] {
                Inst::Acc(x) => {
                    self.acc += x;
                    self.ptr += 1;
                }
                Inst::Jmp(x) => {
                    if x.is_negative() {
                        self.ptr -= x.wrapping_abs() as u32 as usize;
                    } else {
                        self.ptr += x as usize;
                    }
                }
                _ => self.ptr += 1,
            }
        }

        Some(self.acc)
    }

    fn fix_tape(&self) -> Option<i32> {
        for (i, inst) in self.tape.iter().enumerate() {
            if let Some(x) = match inst {
                Inst::Jmp(x) => {
                    let mut tape_copy = self.tape.clone();
                    tape_copy[i] = Inst::Nop(*x);

                    Computer::new(tape_copy).run(false)
                }
                Inst::Nop(x) => {
                    let mut tape_copy = self.tape.clone();
                    tape_copy[i] = Inst::Jmp(*x);

                    Computer::new(tape_copy).run(false)
                }
                _ => None,
            } {
                return Some(x);
            }
        }

        None
    }
}

impl FromIterator<Inst> for Computer {
    fn from_iter<I: IntoIterator<Item = Inst>>(iter: I) -> Self {
        let mut tape = Vec::new();

        for inst in iter {
            tape.push(inst);
        }

        Computer {
            acc: 0,
            ptr: 0,
            tape,
        }
    }
}

fn main() {
    println!("Advent of Code! Day 8!");
    let iter = include_str!("../input")
        .lines()
        .map(|x| {
            lazy_static! {
                static ref INST_REGEX: Regex = Regex::new(r"(\w+)\s([-+])(\d+)").unwrap();
            }

            INST_REGEX.captures(x).unwrap()
        })
        .map(|x| match x.get(1).unwrap().as_str() {
            "nop" => Inst::Nop(match x.get(2).unwrap().as_str() {
                "-" => x
                    .get(3)
                    .unwrap()
                    .as_str()
                    .parse::<i32>()
                    .unwrap()
                    .checked_neg()
                    .unwrap(),
                "+" => x.get(3).unwrap().as_str().parse::<i32>().unwrap(),
                _ => unreachable!(),
            }),
            "acc" => Inst::Acc(match x.get(2).unwrap().as_str() {
                "-" => x
                    .get(3)
                    .unwrap()
                    .as_str()
                    .parse::<i32>()
                    .unwrap()
                    .checked_neg()
                    .unwrap(),
                "+" => x.get(3).unwrap().as_str().parse::<i32>().unwrap(),
                _ => unreachable!(),
            }),
            "jmp" => Inst::Jmp(match x.get(2).unwrap().as_str() {
                "-" => x
                    .get(3)
                    .unwrap()
                    .as_str()
                    .parse::<i32>()
                    .unwrap()
                    .checked_neg()
                    .unwrap(),
                "+" => x.get(3).unwrap().as_str().parse::<i32>().unwrap(),
                _ => unreachable!(),
            }),
            _ => unreachable!(),
        });

    println!(
        "Part One: {:?}",
        iter.clone().collect::<Computer>().run(true).unwrap()
    );
    println!(
        "Part Two: {:?}",
        iter.clone().collect::<Computer>().fix_tape().unwrap()
    );
}
