use std::fmt::Debug;
use std::fs::read_to_string;
use std::str::FromStr;

use itertools::Itertools;

fn main() {
    println!("Advent of Code! Day 1!");

    let values: Vec<i64> = parse_input("input");
    println!("Part One: {:?}", expense_report(values.clone(), 2));
    println!("Part Two: {:?}", expense_report(values, 3));
}

fn expense_report(report: Vec<i64>, size: usize) -> Option<i64> {
    report
        .into_iter()
        .combinations(size)
        .find(|x| x.iter().sum::<i64>() == 2020)
        .map(|x| x.iter().product())
}

fn parse_input<T: FromStr>(path: &str) -> Vec<T>
where
    <T as FromStr>::Err: Debug,
{
    read_to_string(path)
        .expect("unable to open file")
        .lines()
        .map(|l| l.parse().expect("unable to parse line"))
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part_one() {
        let report: Vec<i64> = vec![1721, 979, 366, 299, 675];

        assert_eq!(514579, expense_report(report, 2).unwrap());
    }

    #[test]
    fn test_part_two() {
        let report: Vec<i64> = vec![1721, 979, 366, 299, 675];

        assert_eq!(241861950, expense_report(report, 3).unwrap());
    }
}
