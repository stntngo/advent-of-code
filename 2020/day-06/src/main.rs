use std::collections::HashSet;

static INPUT: &str = include_str!("../input");
static ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
];

fn main() {
    println!("Advent of Code! Day 6!");
    println!(
        "Part One: {:?}",
        INPUT
            .split("\n\n")
            .map(|group| group.lines().fold(
                HashSet::new(),
                |acc, line| {
                    acc.union(&line.chars().collect::<HashSet<_>>())
                        .cloned()
                        .collect::<HashSet<_>>()
                }
            ))
            .map(|group| group.len())
            .sum::<usize>()
    );
    println!(
        "Part Two: {:?}",
        INPUT
            .split("\n\n")
            .map(|group| group.lines().fold(
                ASCII_LOWER.iter().cloned().collect::<HashSet<_>>(),
                |acc, line| {
                    acc.intersection(&line.chars().collect::<HashSet<_>>())
                        .cloned()
                        .collect::<HashSet<_>>()
                }
            ))
            .map(|group| group.len())
            .sum::<usize>()
    );
    todo!("Try out a bit packing approach and compare benchmarks");
}
