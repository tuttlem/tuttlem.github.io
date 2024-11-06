---
layout: post
title: A Star Pathfinding Algorithm
date: 2024-11-06
comments: false
categories: [ "" ]
---

# Introduction

Pathfinding is essential in many applications, from game development to logistics. A* (A-star) is one of the most 
efficient algorithms for finding the shortest path in grid-based environments, such as navigating a maze or finding the 
quickest route on a map. Combining the strengths of Dijkstra’s algorithm and greedy best-first search, A* is fast yet 
accurate, making it a popular choice for pathfinding tasks.

In this guide, we’ll walk through implementing A* in Rust, using Rust’s unique ownership model to handle node 
exploration and backtracking. We’ll also take advantage of data structures like priority queues and hash maps to keep 
the algorithm efficient and Rust-safe.

# Core Concepts and Data Structures

## Priority Queue

A* relies on a priority queue to process nodes in the order of their path cost. In Rust, the `BinaryHeap` structure 
provides a simple and efficient way to prioritize nodes based on their cost to reach the destination.

## Hash Map

We’ll use a `HashMap` to keep track of the best cost found for each node. This allows for quick updates and retrievals 
as we explore different paths in the grid.

## Grid Representation

For simplicity, we’ll represent our grid as a 2D array, with each element holding a `Node`. A `Node` will store 
information about its coordinates and path costs, including the estimated cost to reach the target.

# Ownership and Memory Management in Rust

Rust’s ownership model helps us manage memory safely while exploring nodes. For example, as we create references to 
neighboring nodes and update paths, Rust’s borrowing rules ensure we don’t accidentally create dangling pointers or 
memory leaks.

For shared ownership, we can use `Rc` (reference counting) and `RefCell` to allow multiple nodes to reference each other 
safely, making Rust both safe and efficient.

# Implementation

## Setup

First, let’s define the main components of our grid and the A* algorithm.

{% highlight rust %}
use std::collections::{BinaryHeap, HashMap};
use std::cmp::Ordering;

// Define a simple Node structure
#[derive(Copy, Clone, Eq, PartialEq)]
struct Node {
    position: (i32, i32),
    g_cost: i32,  // Cost from start to current node
    h_cost: i32,  // Heuristic cost from current to target node
}

impl Node {
    fn f_cost(&self) -> i32 {
        self.g_cost + self.h_cost
    }
}

// Implement Ord and PartialOrd to use Node in a priority queue
impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.f_cost().cmp(&self.f_cost()) // Reverse for min-heap behavior
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
{% endhighlight %}

## Initialize the Grid and Heuristic Function

A simple heuristic we’ll use here is the Manhattan distance, suitable for grids where movement is limited to horizontal 
and vertical steps.

{% highlight rust %}
fn heuristic(start: (i32, i32), target: (i32, i32)) -> i32 {
    (start.0 - target.0).abs() + (start.1 - target.1).abs()
}
{% endhighlight %}

## Pathfinding Function

Here’s the core of the A* algorithm. We’ll initialize the grid, process nodes, and backtrack to find the shortest path.

{% highlight rust %}
fn astar(start: (i32, i32), target: (i32, i32), grid_size: (i32, i32)) -> Option<Vec<(i32, i32)>> {
    let mut open_set = BinaryHeap::new();
    let mut came_from = HashMap::new();
    let mut g_costs = HashMap::new();

    open_set.push(Node { position: start, g_cost: 0, h_cost: heuristic(start, target) });
    g_costs.insert(start, 0);

    while let Some(current) = open_set.pop() {
        if current.position == target {
            return Some(reconstruct_path(came_from, current.position));
        }

        for neighbor in get_neighbors(current.position, grid_size) {
            let tentative_g_cost = g_costs[&current.position] + 1;

            if tentative_g_cost < *g_costs.get(&neighbor).unwrap_or(&i32::MAX) {
                came_from.insert(neighbor, current.position);
                g_costs.insert(neighbor, tentative_g_cost);
                open_set.push(Node {
                    position: neighbor,
                    g_cost: tentative_g_cost,
                    h_cost: heuristic(neighbor, target),
                });
            }
        }
    }
    None
}

fn get_neighbors(position: (i32, i32), grid_size: (i32, i32)) -> Vec<(i32, i32)> {
    let (x, y) = position;
    let mut neighbors = Vec::new();
    for (dx, dy) in &[(0, 1), (1, 0), (0, -1), (-1, 0)] {
        let nx = x + dx;
        let ny = y + dy;
        if nx >= 0 && nx < grid_size.0 && ny >= 0 && ny < grid_size.1 {
            neighbors.push((nx, ny));
        }
    }
    neighbors
}

fn reconstruct_path(came_from: HashMap<(i32, i32), (i32, i32)>, mut current: (i32, i32)) -> Vec<(i32, i32)> {
    let mut path = Vec::new();
    while let Some(&prev) = came_from.get(&current) {
        path.push(current);
        current = prev;
    }
    path.reverse();
    path
}
{% endhighlight %}

### Explanation

1. **Initialization**: We add the start node to the `open_set` with an initial `g_cost` of `0`.
2. **Exploration**: We pop nodes from `open_set`, starting with the lowest `f_cost`. If a neighbor has a lower `g_cost` (cost so far) than previously recorded, we update its cost and re-insert it into `open_set`.
3. **Backtracking**: Once we reach the target, we backtrack through the `came_from` map to reconstruct the path.

## Running the A* Algorithm

Finally, let’s run the algorithm to see the path from a start to target position:

{% highlight rust %}
fn main() {
    let start = (0, 0);
    let target = (4, 5);
    let grid_size = (10, 10);

    match astar(start, target, grid_size) {
        Some(path) => {
            println!("Path found: {:?}", path);
        }
        None => {
            println!("No path found");
        }
    }
}
{% endhighlight %}

# Output

This will display the sequence of nodes from start to target, giving us the shortest path using the A* algorithm.

We can restructure these functions to also add visual representations on screen.

{% highlight rust %}
use std::collections::{BinaryHeap, HashMap};
use std::{cmp::Ordering, thread, time::Duration};

// Define symbols for visualization
const EMPTY: char = '.';
const START: char = 'S';
const TARGET: char = 'T';
const OPEN: char = '+';
const CLOSED: char = '#';
const PATH: char = '*';

#[derive(Copy, Clone, Eq, PartialEq)]
struct Node {
    position: (i32, i32),
    g_cost: i32,
    h_cost: i32,
}

impl Node {
    fn f_cost(&self) -> i32 {
        self.g_cost + self.h_cost
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.f_cost().cmp(&self.f_cost())
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn heuristic(start: (i32, i32), target: (i32, i32)) -> i32 {
    (start.0 - target.0).abs() + (start.1 - target.1).abs()
}

fn astar(start: (i32, i32), target: (i32, i32), grid_size: (i32, i32)) -> Option<Vec<(i32, i32)>> {
    let mut open_set = BinaryHeap::new();
    let mut came_from = HashMap::new();
    let mut g_costs = HashMap::new();
    let mut closed_set = Vec::new();

    open_set.push(Node { position: start, g_cost: 0, h_cost: heuristic(start, target) });
    g_costs.insert(start, 0);

    // Initial grid setup
    let mut grid = vec![vec![EMPTY; grid_size.1 as usize]; grid_size.0 as usize];
    grid[start.0 as usize][start.1 as usize] = START;
    grid[target.0 as usize][target.1 as usize] = TARGET;

    // Display progress with each iteration
    while let Some(current) = open_set.pop() {
        if current.position == target {
            return Some(reconstruct_path(came_from, current.position, &mut grid, start, target));
        }
        
        closed_set.push(current.position);
        grid[current.position.0 as usize][current.position.1 as usize] = CLOSED;
        display_grid(&grid);
        thread::sleep(Duration::from_millis(100)); // Slow down for visualization

        for neighbor in get_neighbors(current.position, grid_size) {
            let tentative_g_cost = g_costs[&current.position] + 1;
            if tentative_g_cost < *g_costs.get(&neighbor).unwrap_or(&i32::MAX) {
                came_from.insert(neighbor, current.position);
                g_costs.insert(neighbor, tentative_g_cost);

                grid[neighbor.0 as usize][neighbor.1 as usize] = OPEN;
                open_set.push(Node {
                    position: neighbor,
                    g_cost: tentative_g_cost,
                    h_cost: heuristic(neighbor, target),
                });
            }
        }
    }
    None
}

fn get_neighbors(position: (i32, i32), grid_size: (i32, i32)) -> Vec<(i32, i32)> {
    let (x, y) = position;
    let mut neighbors = Vec::new();
    for (dx, dy) in &[(0, 1), (1, 0), (0, -1), (-1, 0)] {
        let nx = x + dx;
        let ny = y + dy;
        if nx >= 0 && nx < grid_size.0 && ny >= 0 && ny < grid_size.1 {
            neighbors.push((nx, ny));
        }
    }
    neighbors
}

fn reconstruct_path(
    came_from: HashMap<(i32, i32), (i32, i32)>, 
    mut current: (i32, i32), 
    grid: &mut Vec<Vec<char>>, 
    start: (i32, i32), 
    target: (i32, i32)
) -> Vec<(i32, i32)> {
    let mut path = Vec::new();
    while let Some(&prev) = came_from.get(&current) {
        path.push(current);
        current = prev;
    }
    path.reverse();

    for &(x, y) in &path {
        grid[x as usize][y as usize] = PATH;
    }
    grid[start.0 as usize][start.1 as usize] = START;
    grid[target.0 as usize][target.1 as usize] = TARGET;
    display_grid(&grid);

    path
}

fn display_grid(grid: &Vec<Vec<char>>) {
    println!("\x1B[2J\x1B[1;1H"); // Clear the screen
    for row in grid {
        for cell in row {
            print!("{} ", cell);
        }
        println!();
    }
}

fn main() {
    let start = (0, 0);
    let target = (7, 9);
    let grid_size = (10, 10);

    match astar(start, target, grid_size) {
        Some(path) => {
            println!("Path found: {:?}", path);
        }
        None => {
            println!("No path found");
        }
    }
}
{% endhighlight %}

## Explanation 

* `display_grid`: This function clears the screen and then prints the current state of the grid to visualize the progress.
* **Thread Sleep**: A short delay is added to slow down the iteration for visual effect. You can adjust `Duration::from_millis(100)` to control the speed.
* **Symbols**:
  * `.` (OPEN): Nodes being considered.
  * `#` (CLOSED): Nodes already explored.
  * `*` (PATH): Final path from start to target.

After running this code, you should see the path being solved from one point to another:

{% highlight text %}
S * * * + . . . . . 
# # # * + . . . . . 
# # # * * + . . . . 
+ # # # * + . . . . 
+ # # # * + . . . . 
. + # # * T . . . . 
. . + + + . . . . . 
. . . . . . . . . . 
. . . . . . . . . . 
. . . . . . . . . . 
Path found: [(0, 1), (0, 2), (0, 3), (1, 3), (2, 3), (2, 4), (3, 4), (4, 4), (5, 4), (5, 5)]
{% endhighlight %}

# Conclusion

The A* algorithm can be optimized by tuning the heuristic function. In Rust, using `BinaryHeap` and `HashMap` helps 
manage the exploration process efficiently, and Rust’s ownership model enforces safe memory practices.

A* in Rust is an excellent example of how the language’s unique features can be leveraged to implement classic 
algorithms effectively. Rust’s focus on memory safety and efficient abstractions makes it a strong candidate for 
implementing pathfinding and other performance-sensitive tasks.
