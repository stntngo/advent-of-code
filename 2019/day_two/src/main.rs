use std::convert::TryFrom;

fn main() {
    let input = vec!(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,13,23,27,1,6,27,31,1,31,10,35,1,35,6,39,1,39,13,43,2,10,43,47,1,47,6,51,2,6,51,55,1,5,55,59,2,13,59,63,2,63,9,67,1,5,67,71,2,13,71,75,1,75,5,79,1,10,79,83,2,6,83,87,2,13,87,91,1,9,91,95,1,9,95,99,2,99,9,103,1,5,103,107,2,9,107,111,1,5,111,115,1,115,2,119,1,9,119,0,99,2,0,14,0);

    let target = 19690720;

    'outer: for noun in 0..100 {
        for verb in 0..100 {
            let copy = input.to_vec();
            if run_computer(copy, noun, verb) == target {
                println!("[found answer] noun: {} verb: {} answer: {}", noun, verb, 100 * noun + verb);
                break 'outer;
            }
        }
    }
}

fn run_computer(mut v: Vec<i64>, noun: i64, verb: i64) -> i64 {
    v[1] = noun;
    v[2] = verb;

    let mut ptr = 0;

    loop {
        let op = v[ptr];

        if op == 99 {
            return v[0];
        }

        let args = vec!(usize::try_from(v[ptr + 1]).unwrap(), usize::try_from(v[ptr + 2]).unwrap());
        let out = usize::try_from(v[ptr + 3]).unwrap();

        let result = match op {
            1 => v[args[0]] + v[args[1]],
            2 => v[args[0]] * v[args[1]],
            _ => panic!("unrecognized op code"),
        };


        v[out] = result;

        ptr += 4;
    }

}
