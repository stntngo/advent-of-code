use permutohedron::heap_recursive;
use std::convert::TryFrom;

trait Digits {
    fn digits(&self) -> Vec<i64>;
}

impl Digits for i64 {
    fn digits(&self) -> Vec<i64> {
        fn x_inner(n: &i64, xs: &mut Vec<i64>) {
            if *n >= 10 {
                x_inner(&(n / 10), xs);
            }

            xs.push(n % 10);
        }

        let mut xs = Vec::new();
        x_inner(self, &mut xs);
        return xs;
    }
}

struct Output(i64, bool);

#[derive(Debug)]
struct IntComputer {
    tape: Vec<i64>,
    args: Vec<i64>,
    ptr: usize,
    out: Vec<i64>,
}

impl IntComputer {
    fn input(&mut self, in_: i64) {
        self.args.push(in_)
    }

    fn run(&mut self, input: Vec<i64>) -> Output {
        for arg in input {
            self.input(arg);
        }

        loop {
            let cmd = self.next();

            if cmd.op.code == 99 {
                return Output(*self.out.to_vec().last().expect("empty out tape"), true);
            }

            let mut operands = Vec::new();

            for i in 0..cmd.params.len() {
                match cmd.op.modes[i] {
                    0 => {
                        let loc = usize::try_from(cmd.params[i]).unwrap();
                        operands.push(self.tape[loc]);
                    }
                    1 => {
                        operands.push(cmd.params[i]);
                    }
                    _ => (),
                };
            }

            let dest = match cmd.op.code {
                5 | 6 => usize::try_from(operands[1]).unwrap(),
                _ => usize::try_from(*cmd.params.last().expect("failed to parse destination"))
                    .unwrap(),
            };

            if cmd.op.code == 4 {
                self.out.push(operands[0]);
                return Output(*self.out.to_vec().last().expect("empty out tape"), false);
            }

            match cmd.op.code {
                // add
                1 => self.tape[dest] = operands[0] + operands[1],
                // multiply
                2 => self.tape[dest] = operands[0] * operands[1],
                // input
                3 => self.tape[dest] = self.args.remove(0),
                // output
                4 => self.out.push(operands[0]),
                // jump-if-true
                5 => {
                    if operands[0] != 0 {
                        self.ptr = dest;
                    }
                }
                // jump-if-false
                6 => {
                    if operands[0] == 0 {
                        self.ptr = dest;
                    }
                }
                // less than
                7 => self.tape[dest] = if operands[0] < operands[1] { 1 } else { 0 },
                // equals
                8 => self.tape[dest] = if operands[0] == operands[1] { 1 } else { 0 },
                x => panic!("unrecognize opcode: {}", x),
            };
        }
    }

    fn next(&mut self) -> Command {
        let op = parse_opcode(self.tape[self.ptr]);
        self.ptr += 1;

        let params = match op.code {
            1 | 2 | 7 | 8 => self.tape[self.ptr..self.ptr + 3].to_vec(),
            3 | 4 => self.tape[self.ptr..self.ptr + 1].to_vec(),
            5 | 6 => self.tape[self.ptr..self.ptr + 2].to_vec(),
            99 => Vec::new(),
            x => panic!("unrecognized opcode {}", x),
        };

        self.ptr += params.len();

        // println!("{:?} {:?} {:?}", op, params, self.out);

        return Command {
            op: op,
            params: params,
        };
    }
}

#[derive(Debug)]
struct Command {
    op: OpCode,
    params: Vec<i64>,
}

#[derive(Debug)]
struct OpCode {
    code: i64,
    modes: Vec<i64>,
}

#[derive(Debug)]
struct Amplifiers {
    computers: Vec<IntComputer>,
}

impl Amplifiers {
    fn run(&mut self, phase: Vec<i64>) -> i64 {
        let mut last = 0;

        for i in 0..self.computers.len() {
            self.computers[i].input(phase[i]);
        }

        let mut i = 0;
        let mut num_done = 0;
        loop {
            let input = vec![last];
            // println!("running computer {} with input {}", i, last);
            let Output(output, done) = self.computers[i].run(input);
            last = output;
            // println!("Run Computer {} until Instruction Pointer {} Returning {}", i, self.computers[i].ptr, last);
            if done {
                num_done += 1;
                if num_done == self.computers.len() {
                    break;
                }
            }

            i = (i + 1) % self.computers.len();
        }

        return last;
    }
}

fn new_amplifier(program: Vec<i64>) -> Amplifiers {
    let mut computers = Vec::new();

    for _ in 0..5 {
        let comp = new_intcomputer(program.to_vec());
        computers.push(comp);
    }

    return Amplifiers {
        computers: computers,
    };
}

fn parse_opcode(raw: i64) -> OpCode {
    let mut digits = raw.digits();
    digits.reverse();

    while digits.len() < 5 {
        digits.push(0);
    }

    let code = digits[0] + (10 * digits[1]);

    if code == 5 || code == 6 {
        digits[4] = 1;
    }

    return OpCode {
        code: code,
        modes: digits[2..digits.len()].to_vec(),
    };
}

fn new_intcomputer(program: Vec<i64>) -> IntComputer {
    return IntComputer {
        tape: program,
        args: Vec::new(),
        ptr: 0,
        out: Vec::new(),
    };
}

// fn part_one() {
//     let input = include_str!("input");

//     let program: Vec<i64> = input
//         .trim()
//         .split(",")
//         .map(|op| op.trim().parse::<i64>().unwrap())
//         .collect();

//     let mut data = [0, 1, 2, 3, 4];
//     let mut best = 0;
//     heap_recursive(&mut data, |perm| {
//         let _copy = program.to_vec();
//         let mut amp = new_amplifier(_copy);
//         let run_score = amp.run(perm.to_vec());

//         if run_score > best {
//             best = run_score;
//         }
//     });

//     println!("Answer One: {:?}", best);
// }

fn part_two() {
    let input = include_str!("input");

    let program: Vec<i64> = input
        .trim()
        .split(",")
        .map(|op| op.trim().parse::<i64>().unwrap())
        .collect();

    let mut data = [9, 8, 7, 6, 5];
    let mut best = 0;
    heap_recursive(&mut data, |perm| {
        let _copy = program.to_vec();
        let mut amp = new_amplifier(_copy);
        let run_score = amp.run(perm.to_vec());

        if run_score > best {
            best = run_score;
        }
    });

    println!("Answer Two: {:?}", best);


}

fn main() {
    // part_one();
    part_two();
}
