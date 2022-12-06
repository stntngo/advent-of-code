fn main() {
    println!("Advent of Code! Day 3!");

    let map = include_str!("../input");

    println!("Part One: {:?}", traverse_slope(map, 3, 1));
    println!(
        "Part Two: {:?}",
        vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
            .iter()
            .map(|&(x, y)| traverse_slope(map, x, y))
            .product::<usize>()
    );
}

fn traverse_slope(map: &str, dx: usize, dy: usize) -> usize {
    map.lines()
        .step_by(dy)
        .enumerate()
        .filter(|(i, row)| row.chars().cycle().nth(i * dx) == Some('#'))
        .count()
}
