use std::convert::Infallible;
use std::iter::Iterator;
use std::str::FromStr;

use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

static INPUT: &str = include_str!("../input");

#[derive(Debug, PartialEq)]
struct BagDef {
    color: String,
    valid_children: Vec<(usize, String)>,
}

impl BagDef {
    fn interior_bag_count(&self, bags: &[Self]) -> usize {
        self.valid_children
            .iter()
            .map(|(count, color)| {
                count
                    * (bags
                        .iter()
                        .find(|x| &x.color == color)
                        .unwrap()
                        .interior_bag_count(bags)
                        + 1)
            })
            .sum::<usize>()
    }
}

impl FromStr for BagDef {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (parent, children) = s.split(" contain ").next_tuple().unwrap();

        lazy_static! {
            static ref PARENT_REGEX: Regex = Regex::new(r"^(.+)\sbag").unwrap();
            static ref CHILD_REGEX: Regex = Regex::new(r"(\d+)\s(.+)\sbag").unwrap();
        }

        let color = PARENT_REGEX
            .captures(parent)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .to_owned();

        if children.contains("no other") {
            return Ok(BagDef {
                color,
                valid_children: Vec::new(),
            });
        }

        Ok(BagDef {
            color,
            valid_children: children
                .split(',')
                .map(|x| {
                    let caps = CHILD_REGEX.captures(x).unwrap();

                    (
                        caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
                        caps.get(2).unwrap().as_str().to_owned(),
                    )
                })
                .collect::<Vec<_>>(),
        })
    }
}

fn main() {
    let defs = INPUT
        .lines()
        .map(|x| x.parse::<BagDef>())
        .map(|x| x.unwrap())
        .collect::<Vec<_>>();

    println!("Advent of Code! Day 7!");
    println!(
        "Part One: {:?}",
        defs.iter()
            .filter(|x| can_contain_bag(&defs, &x.color, "shiny gold"))
            .count()
    );
    println!(
        "Part Two: {:?}",
        defs.iter()
            .find(|x| x.color == "shiny gold")
            .unwrap()
            .interior_bag_count(&defs)
    );
}

fn can_contain_bag(bags: &[BagDef], bag: &str, target: &str) -> bool {
    let current = bags.iter().find(|x| x.color == bag).unwrap();
    current
        .valid_children
        .iter()
        .any(|(_, color)| color == target)
        || current
            .valid_children
            .iter()
            .any(|(_, color)| can_contain_bag(bags, color, target))
}
