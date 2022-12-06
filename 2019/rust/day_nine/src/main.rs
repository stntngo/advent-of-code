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
struct IntCodeComputer {
    tape: Vec<i64>,
    args: Vec<i64>,
    ptr: usize,
    rel_base: i64,
    out: Vec<i64>,
}

impl IntCodeComputer {
    fn new(program: Vec<i64>) -> Self {
        return Self {
            tape: program,
            args: Vec::new(),
            ptr: 0,
            rel_base: 0,
            out: Vec::new(),
        };
    }

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
                    2 => {
                        let raw_loc = self.rel_base as i64 + cmd.params[i];
                        let loc = usize::try_from(raw_loc).unwrap();
                        operands.push(self.tape[loc]);
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
                // adjust relative base
                9 => self.rel_base += operands[0],
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

        return Command { op, params };
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

fn main() {
    let raw_input = include_str!("input.test");
}
