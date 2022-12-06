fn main() {
    let mut valid = 0;
    for i in 146810..612565 {
        if valid_password(i) {
            valid += 1;
        }
    }

    println!("{}", valid);
    // println!("{} {}", 111122, valid_password(111122))
}

fn valid_password(x: i32) -> bool {
    let digits = x.digits();

    for i in 1..digits.len() {
        if digits[i - 1] > digits[i] {
            return false;
        }
    }

    let rle_nodes = rle(digits);

    for node in rle_nodes {
        if node.0 == 2{
            return true;
        }
    }

    return false;
}

trait Digits {
    fn digits(&self) -> Vec<i32>;
}

impl Digits for i32 {
    fn digits(&self) -> Vec<i32> {
        fn x_inner(n: &i32, xs: &mut Vec<i32>) {
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
struct RLENode(i32, i32);

fn rle(digits: Vec<i32>) -> Vec<RLENode> {
    let mut out = Vec::new();
    let mut current_digit = digits[0];
    let mut current_run = 0;

    for digit in digits {
        if digit == current_digit {
            current_run += 1;
        } else {
            out.push(RLENode(current_run, current_digit));
            current_digit = digit;
            current_run = 1;
        }
    }

    out.push(RLENode(current_run, current_digit));

    return out;
}
