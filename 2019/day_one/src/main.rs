use std::fs;

const FILENAME: &str = "input";

fn main() {
    let contents = fs::read_to_string(FILENAME).expect("unable to read file");

    let mut valid: Vec<i64> = Vec::new();

    for line in contents.split('\n') {
        match parse(line) {
            Ok(v) => valid.push(v),
            Err(e) => println!("{}", e),
        }
    }

    let mut result: i64 = 0;

    for mass in valid {
        let fuel = calculate_fuel(mass);
        result += fuel;
    }

    println!("required fuel: {}", result);
    println!("required fuel for mass 1969: {}", calculate_fuel(1969));
}

fn parse(line: &str) -> Result<i64, &'static str> {
    match line.parse::<i64>() {
        Ok(v) => Ok(v),
        Err(_) => Err("unable to parse line"),
    }
}

fn calculate_fuel(x: i64) -> i64 {
    let mut fuel = (x / 3) - 2;

    match fuel > 0 {
        true => fuel += calculate_fuel(fuel),
        false => fuel = 0,
    }

    fuel
}
