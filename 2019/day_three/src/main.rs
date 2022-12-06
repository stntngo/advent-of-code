use std::fs;

const FILENAME: &str = "input";

fn main() {
    let contents = fs::read_to_string(FILENAME).unwrap();

    let mut paths = Vec::new();

    for line in contents.split("\n") {
        let mut path = Vec::new();
        for entry in line.split(",") {
            path.push(entry);
        }
        paths.push(path);
    }

    // let path_one = to_wire_path(&vec!("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"));
    // let path_two = to_wire_path(&vec!("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83"));
    // let path_one = to_wire_path(&vec!("R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51"));
    // let path_two = to_wire_path(&vec!("U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7"));
    let path_one = to_wire_path(&paths[0]);
    let path_two = to_wire_path(&paths[1]);

    let mut intersections = Vec::new();

    for vertical in &path_one.vertical {
        for horizontal in &path_two.horizontal {
            match intersection_point(&vertical, &horizontal) {
                Ok(Point { x: 0, y: 0 }) => (),
                Ok(point) => intersections.push(point),
                _ => (),
            }
        }
    }

    for vertical in &path_two.vertical {
        for horizontal in &path_one.horizontal {
            match intersection_point(&vertical, &horizontal) {
                Ok(Point { x: 0, y: 0 }) => (),
                Ok(point) => intersections.push(point),
                _ => (),
            }
        }
    }

    println!("{:?}", intersections);

    let mut low: i64 = -1;

    for point in intersections {
        let distance = path_one.walk_distance(&point) + path_two.walk_distance(&point);

        if low < 0 || distance < low {
            low = distance;
            println!(
                "{:?} {:?}",
                point,
                path_one.walk_distance(&point) + path_two.walk_distance(&point)
            );
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn distance(&self, other: &Point) -> i64 {
        return (other.x - self.x).abs() + (other.y - self.y).abs();
    }
}

#[derive(Debug, Clone, Copy)]
struct LineSegment {
    start: Point,
    end: Point,
}

enum Orientation {
    Vertical,
    Horizontal,
}

fn intersection_point(
    vertical: &LineSegment,
    horizontal: &LineSegment,
) -> Result<Point, &'static str> {
    let (v_min, v_max) = if vertical.start.y > vertical.end.y {
        (vertical.end.y, vertical.start.y)
    } else {
        (vertical.start.y, vertical.end.y)
    };

    let (h_min, h_max) = if horizontal.start.x > horizontal.end.x {
        (horizontal.end.x, horizontal.start.x)
    } else {
        (horizontal.start.x, horizontal.end.x)
    };

    if v_min <= horizontal.start.y && horizontal.start.y <= v_max {
        if h_min <= vertical.start.x && vertical.start.x <= h_max {
            return Ok(Point {
                x: vertical.start.x,
                y: horizontal.start.y,
            });
        }
    }

    return Err("lines do not intersect");
}

impl LineSegment {
    fn orientation(&self) -> Result<Orientation, &'static str> {
        if self.start.x == self.end.x {
            return Ok(Orientation::Vertical);
        }

        if self.start.y == self.end.y {
            return Ok(Orientation::Horizontal);
        }

        return Err("line segment does not lie on the cartesian grid");
    }

    fn contains(&self, p: &Point) -> bool {
        match self.orientation().unwrap() {
            Orientation::Horizontal => {
                let (h_min, h_max) = if self.start.x > self.end.x {
                    (self.end.x, self.start.x)
                } else {
                    (self.start.x, self.end.x)
                };

                return self.start.y == p.y && h_min <= p.x && p.x <= h_max;
            }
            Orientation::Vertical => {
                let (v_min, v_max) = if self.start.y > self.end.y {
                    (self.end.y, self.start.y)
                } else {
                    (self.start.y, self.end.y)
                };

                return self.start.x == p.x && v_min <= p.y && p.y <= v_max;
            }
        };
    }
}

#[derive(Debug, Clone)]
struct WirePath {
    vertical: Vec<LineSegment>,
    horizontal: Vec<LineSegment>,
    path: Vec<LineSegment>,
}

impl WirePath {
    fn walk_distance(&self, p: &Point) -> i64 {
        let mut distance = 0;

        for line in &self.path {
            if line.contains(p) {
                distance += line.start.distance(p);
                break;
            }

            distance += line.start.distance(&line.end);
        }

        return distance;
    }
}

fn to_line_segment(start: Point, vector: &str) -> LineSegment {
    let dir = &vector[0..1];
    let dist = vector[1..vector.len()].parse::<i64>().unwrap();

    let end = match dir {
        "L" => Point {
            x: start.x - dist,
            y: start.y,
        },
        "R" => Point {
            x: start.x + dist,
            y: start.y,
        },
        "U" => Point {
            x: start.x,
            y: start.y + dist,
        },
        "D" => Point {
            x: start.x,
            y: start.y - dist,
        },
        &_ => panic!("unrecognizable input {}", dir),
    };

    return LineSegment {
        start: start,
        end: end,
    };
}

fn to_wire_path(vectors: &Vec<&str>) -> WirePath {
    let mut path = WirePath {
        vertical: Vec::new(),
        horizontal: Vec::new(),
        path: Vec::new(),
    };

    let mut start = Point { x: 0, y: 0 };

    for vector in vectors {
        let segment = to_line_segment(start, vector);

        match segment.orientation() {
            Ok(Orientation::Vertical) => path.vertical.push(segment),
            Ok(Orientation::Horizontal) => path.horizontal.push(segment),
            Err(e) => panic!(e),
        }

        path.path.push(segment);

        start = segment.end;
    }

    return path;
}
