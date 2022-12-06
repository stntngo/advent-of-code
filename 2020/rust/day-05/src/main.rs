#![feature(test)]
extern crate test;

trait Partition<T>
where
    T: Partition<T>,
{
    fn hi(&self) -> T;
    fn lo(&self) -> T;
}

trait Seat {
    fn seat_id(&self) -> usize;
}

impl Partition<(usize, usize)> for (usize, usize) {
    fn hi(&self) -> (usize, usize) {
        ((self.1 + self.0) / 2 + 1, self.1)
    }

    fn lo(&self) -> (usize, usize) {
        (self.0, (self.1 + self.0) / 2)
    }
}

impl Seat for (usize, usize) {
    fn seat_id(&self) -> usize {
        self.0 * 8 + self.1
    }
}

const INPUT: &str = include_str!("../input");

fn main() {
    let input = include_str!("../input");

    println!("Advent of Code! Day 5!");
    println!("Part One: {:?}", input.lines().map(seat_id).max().unwrap());
    let mut ids = input.lines().map(|path| seat_id(path)).collect::<Vec<_>>();
    ids.sort_unstable();

    println!(
        "Part Two: {:?}",
        ids.windows(2)
            .filter(|&x| x[1] - x[0] != 1)
            .map(|x| x[0] + 1)
            .next()
    );
}

#[allow(dead_code)]
fn seat(path: &str) -> (usize, usize) {
    let ((row, _), (col, _)) = path
        .chars()
        .fold(((0, 127), (0, 7)), |(row, col), x| match x {
            'F' => (row.lo(), col),
            'B' => (row.hi(), col),
            'L' => (row, col.lo()),
            'R' => (row, col.hi()),
            _ => panic!("unknown ident"),
        });

    (row, col)
}

fn seat_id(path: &str) -> usize {
    let row = &path[..7]
        .chars()
        .rev()
        .enumerate()
        .fold(0b00000000, |acc, (i, x)| match x {
            'F' => acc,
            'B' => acc | (1 << i),
            _ => unimplemented!(),
        });

    let col = &path[7..]
        .chars()
        .rev()
        .enumerate()
        .fold(0b00000000, |acc, (i, x)| match x {
            'L' => acc,
            'R' => acc | (1 << i),
            _ => unimplemented!(),
        });

    8 * row + col
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn test_seat() {
        assert_eq!(567, seat("BFFFBBFRRR").seat_id());
        assert_eq!(119, seat("FFFBBBFRRR").seat_id());
        assert_eq!(820, seat("BBFFBBFRLL").seat_id());
        assert_eq!(38, seat("FFFFBFFRRL").seat_id());
    }

    #[test]
    fn test_seat_id() {
        assert_eq!(567, seat_id("BFFFBBFRRR"));
        assert_eq!(119, seat_id("FFFBBBFRRR"));
        assert_eq!(820, seat_id("BBFFBBFRLL"));
        assert_eq!(38, seat_id("FFFFBFFRRL"));
    }

    #[bench]
    fn bench_naive_bsp(b: &mut Bencher) {
        b.iter(|| {
            INPUT
                .lines()
                .map(|path| seat(path).seat_id())
                .for_each(drop);
        });
    }

    #[bench]
    fn bench_bitshift_bsp(b: &mut Bencher) {
        b.iter(|| {
            INPUT.lines().map(|path| seat_id(path)).for_each(drop);
        });
    }
}
