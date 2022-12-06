use std::cmp::min;
use std::collections::{HashMap, HashSet};

fn main() {
    let input = include_str!("input");

    let orbits: Vec<Vec<&str>> = input
        .trim()
        .split("\n")
        .map(|orbit| orbit.split(")").collect())
        .collect();

    let mut moons: HashMap<&str, &str> = HashMap::new();
    let mut orbiters: HashMap<&str, Vec<&str>> = HashMap::new();
    let mut bodies: HashSet<&str> = HashSet::new();

    for orbit in orbits {
        let mut existing = match orbiters.get(orbit[0]) {
            None => Vec::new(),
            Some(v) => v.to_vec(),
        };

        existing.push(orbit[1]);

        orbiters.insert(orbit[0], existing);
        moons.insert(orbit[1], orbit[0]);
        bodies.insert(orbit[0]);
        bodies.insert(orbit[1]);
    }

    println!(
        "Answer One: {}",
        bodies.iter().fold(0, |acc, x| acc + lineage_len(x, &moons))
    );

    let successors = |moon: &str| -> Vec<&str> {
        let mut children = match orbiters.get(moon) {
            None => Vec::new(),
            Some(v) => v.to_vec(),
        };

        match moons.get(moon) {
            None => (),
            Some(v) => children.push(v),
        };

        return children;
    };

    static START: &str = "YOU";
    static GOAL: &str = "SAN";
    let mut unvisited = bodies.clone();

    let mut distances: HashMap<&str, i64> = HashMap::new();
    for body in bodies {
        distances.insert(body, i64::max_value());
    }

    distances.insert(START, 0);

    let mut current_node: &str = START;
    loop {
        if current_node == GOAL {
            break;
        }

        let current_distance = distances
            .get(current_node)
            .cloned()
            .expect("unable to find distance value for current node");

        for node in successors(current_node) {
            if !unvisited.contains(node) {
                continue;
            }

            match distances.get(node).cloned() {
                None => distances.insert(node, current_distance + 1),
                Some(dist) => distances.insert(node, min(current_distance + 1, dist)),
            };
        }

        unvisited.remove(current_node);

        let mut tentative_dist = i64::max_value();
        for node in unvisited.iter() {
            match distances.get(node).cloned() {
                None => (),
                Some(dist) => {
                    if dist < tentative_dist {
                        tentative_dist = dist;
                        current_node = node;
                    }
                }
            }
        }
    }

    println!(
        "Answer Two: {}",
        *distances
            .get(current_node)
            .expect("unable to find distance value")
            - 2
    );
}

fn lineage_len(node: &str, adj: &HashMap<&str, &str>) -> i64 {
    return match adj.get(node) {
        None => 0,
        Some(parent) => 1 + lineage_len(parent, adj),
    };
}
