fn main() {
    let input = include_str!("input");

    let width = 25;
    let height = 6;

    let image_code: Vec<i64> = input
        .trim()
        .chars()
        .map(|c| c.to_string().parse::<i64>().unwrap())
        .collect();

    let layers: Vec<&[i64]> = image_code.chunks(width * height).collect();

    let mut min_zeros = i64::max_value();
    let mut min_layer: usize = 0;

    for i in 0..layers.len() {
        let mut num_zero = 0;

        for pixel in layers[i] {
            if *pixel == 0 {
                num_zero += 1;
            }
        }

        if num_zero < min_zeros {
            min_zeros = num_zero;
            min_layer = i;
        }
    }

    let mut num_one = 0;
    let mut num_two = 0;

    for pixel in layers[min_layer] {
        match *pixel {
            2 => num_two += 1,
            1 => num_one += 1,
            _ => (),
        }
    }

    println!("Answer One: {}", num_one * num_two);

    let mut out = Vec::new();
    for i in 0..(width * height) {
        let non_transparent = layers
            .iter()
            .map(|layer| layer[i])
            .filter(|px| *px != 2)
            .collect::<Vec<i64>>();

        match non_transparent.first() {
            None => out.push(0),
            Some(v) => out.push(*v),
        }
    }

    println!("Answer Two:");

    let lines: Vec<&[i64]> = out.chunks(width).collect();

    for line in lines {
        let mut s = String::from("");
        for pixel in line {
            match pixel {
                0 => s.push('█'),
                1 => s.push(' '),
                _ => (),
            }
        }

        println!("{}", s);
    }
}
