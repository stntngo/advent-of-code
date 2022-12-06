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
        return xs
    }
}

#[derive(Debug)]
struct IntComputer {
    tape: Vec<i64>,
    ptr: usize,
    out: Vec<i64>,
}

impl IntComputer {
    fn run(&mut self, input: i64) -> Vec<i64> {
        loop {
            let cmd = self.next();

            if cmd.op.code == 99 {
                return self.out.to_vec();
            }

            let mut operands = Vec::new();

            for i in 0..cmd.params.len() {
                match cmd.op.modes[i] {
                    0 => {
                        let loc = usize::try_from(cmd.params[i]).unwrap();
                        operands.push(self.tape[loc]);
                    },
                    1 => {
                        operands.push(cmd.params[i]);
                    },
                    _ => (),
                };
            };

            let dest = match cmd.op.code {
                5 | 6 => usize::try_from(operands[1]).unwrap(),
                _ => usize::try_from(*cmd.params.last().expect("failed to parse destination")).unwrap(),
            };


            match cmd.op.code {
                // add
                1 => self.tape[dest] = operands[0] + operands[1],
                // multiply
                2 => self.tape[dest] = operands[0] * operands[1],
                // input
                3 => self.tape[dest] = input,
                // output
                4 => self.out.push(operands[0]),
                // jump-if-true
                5 => {
                    if operands[0] != 0 {
                        self.ptr = dest;
                    }
                },
                // jump-if-false
                6 => {
                    if operands[0] == 0 {
                        self.ptr = dest;
                    }
                },
                // less than
                7 => {
                    self.tape[dest] = if operands[0] < operands[1] {
                        1
                    } else {
                        0
                    }
                },
                // equals
                8 => {
                    self.tape[dest] = if operands[0] == operands[1] {
                        1
                    } else {
                        0
                    }
                },
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

        return Command{
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

fn parse_opcode(raw: i64) -> OpCode {
    let mut digits = raw.digits();
    digits.reverse();

    while digits.len() < 5{
        digits.push(0);
    }

    let code = digits[0] + (10 * digits[1]);

    if code == 5 || code == 6 {
        digits[4] = 1;
    }

    return OpCode{
        code: code,
        modes: digits[2..digits.len()].to_vec(),
    };
}

fn new_intcomputer(program: Vec<i64>) -> IntComputer {
    return IntComputer {
        tape: program,
        ptr: 0,
        out: Vec::new(),
    }
}

fn main() {
    // let input = vec!(1002,4,3,4,33);
    // let input = vec!(3, 0, 4, 0, 99);
    let input = vec!(3,225,1,225,6,6,1100,1,238,225,104,0,101,67,166,224,1001,224,-110,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,2,62,66,224,101,-406,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,76,51,225,1101,51,29,225,1102,57,14,225,1102,64,48,224,1001,224,-3072,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1001,217,90,224,1001,224,-101,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,57,55,224,1001,224,-112,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1102,5,62,225,1102,49,68,225,102,40,140,224,101,-2720,224,224,4,224,1002,223,8,223,1001,224,4,224,1,223,224,223,1101,92,43,225,1101,93,21,225,1002,170,31,224,101,-651,224,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1,136,57,224,1001,224,-138,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,11,85,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,226,224,102,2,223,223,1006,224,329,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,359,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,226,224,102,2,223,223,1006,224,404,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,419,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,434,1001,223,1,223,1008,677,677,224,1002,223,2,223,1005,224,449,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,494,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,509,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,524,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,539,1001,223,1,223,8,677,677,224,102,2,223,223,1005,224,554,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,569,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,584,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1008,677,226,224,102,2,223,223,1006,224,614,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,629,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,644,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,659,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226);
    let copy = input.to_vec();

    let mut computer = new_intcomputer(input);
    let out = computer.run(1);

    println!("{:?}", out);
    println!("{:?}", new_intcomputer(copy).run(5));
}
