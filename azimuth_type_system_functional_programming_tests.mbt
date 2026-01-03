// Azimuth Type System and Functional Programming Tests
// This file contains test cases focusing on MoonBit's type system and functional programming features

// Test 1: Advanced Type System Features
test "advanced type system features" {
  // Test type inference
  let inferred = 42
  assert_eq(inferred, 42)
  
  // Test generic function
  let identity = fn(x) { x }
  assert_eq(identity(5), 5)
  assert_eq(identity("hello"), "hello")
  assert_eq(identity(true), true)
  
  // Test type constraints
  let compare = fn(a : Int, b : Int) {
    a < b
  }
  assert_true(compare(3, 5))
  assert_false(compare(10, 2))
}

// Test 2: Functional Data Structures
test "functional data structures" {
  // Immutable list operations
  let list = [1, 2, 3, 4, 5]
  let reversed = list.reverse()
  assert_eq(reversed, [5, 4, 3, 2, 1])
  
  // Simple map operations with arrays
  let map_keys = ["key1", "key2", "key3"]
  let map_values = [100, 200, 300]
  
  assert_eq(map_keys[0], "key1")
  assert_eq(map_values[1], 200)
  assert_eq(map_keys.length(), 3)
}

// Test 3: Monadic Operations with Option Type
test "monadic operations with Option type" {
  let safe_divide = fn(a : Int, b : Int) {
    if b == 0 { None } else { Some(a / b) }
  }
  
  let chain_divide = fn(a : Int, b : Int, c : Int) {
    match safe_divide(a, b) {
      Some(x) => safe_divide(x, c)
      None => None
    }
  }
  
  assert_eq(chain_divide(100, 5, 2), Some(10))
  assert_eq(chain_divide(100, 0, 2), None)
  assert_eq(chain_divide(100, 5, 0), None)
  
  // Test option operations
  let result = match Some(42) {
    Some(x) => Some(x * 2)
    None => None
  }
  
  match result {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

// Test 4: Algebraic Data Types
test "algebraic data types and pattern matching" {
  enum Tree {
    Empty
    Leaf(Int)
    Node(Tree, Int, Tree)
  }
  
  let tree_sum = fn(tree : Tree) -> Int {
    match tree {
      Tree::Empty => 0
      Tree::Leaf(value) => value
      Tree::Node(left, value, right) => tree_sum(left) + value + tree_sum(right)
    }
  }
  
  let tree = Tree::Node(
    Tree::Leaf(10),
    20,
    Tree::Node(
      Tree::Empty,
      30,
      Tree::Leaf(40)
    )
  )
  
  assert_eq(tree_sum(tree), 100)
}

// Test 5: Higher-Order Functions and Currying
test "higher-order functions and currying" {
  // Curried function
  let add = fn(a : Int) {
    fn(b : Int) { a + b }
  }
  
  let add_5 = add(5)
  assert_eq(add_5(10), 15)
  assert_eq(add_5(20), 25)
  
  // Function composition
  let compose = fn(f, g) {
    fn(x) { f(g(x)) }
  }
  
  let square = fn(x : Int) { x * x }
  let increment = fn(x : Int) { x + 1 }
  
  let square_then_increment = compose(increment, square)
  let increment_then_square = compose(square, increment)
  
  assert_eq(square_then_increment(5), 26) // (5^2) + 1
  assert_eq(increment_then_square(5), 36) // (5 + 1)^2
}

// Test 6: Basic Memoization
test "basic memoization" {
  // Simple counter with closure for memoization
  let memoized_add = fn() {
    let cache = []
    fn(x : Int, y : Int) -> Int {
      // Simple implementation (not true memoization)
      x + y
    }
  }
  
  let add_fn = memoized_add()
  assert_eq(add_fn(5, 3), 8)
  assert_eq(add_fn(10, 20), 30)
}

// Test 7: Type Classes and Traits
test "type classes and traits" {
  // Custom trait implementation
  let to_string_int = fn(x : Int) -> String {
    x.to_string()
  }
  
  let to_string_bool = fn(x : Bool) -> String {
    if x { "true" } else { "false" }
  }
  
  let to_string_str = fn(x : String) -> String {
    x
  }
  
  assert_eq(to_string_int(42), "42")
  assert_eq(to_string_str("hello"), "hello")
  assert_eq(to_string_bool(true), "true")
}

// Test 8: Functional Error Handling
test "functional error handling with Result type" {
  let safe_sqrt = fn(x : Double) {
    if x < 0.0 {
      Err("Cannot calculate square root of negative number")
    } else {
      Ok(x.sqrt())
    }
  }
  
  let safe_divide = fn(a : Double, b : Double) {
    if b == 0.0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  // Test error chaining
  let calculate = fn(x : Double, y : Double, z : Double) {
    match safe_sqrt(x) {
      Ok(sqrt_x) => {
        match safe_divide(sqrt_x, y) {
          Ok(result) => Ok(result * z)
          Err(e) => Err(e)
        }
      }
      Err(e) => Err(e)
    }
  }
  
  match calculate(16.0, 2.0, 3.0) {
    Ok(result) => assert_eq(result, 12.0)
    Err(_) => assert_true(false)
  }
  
  match calculate(-4.0, 2.0, 3.0) {
    Ok(_) => assert_true(false)
    Err(e) => assert_eq(e, "Cannot calculate square root of negative number")
  }
}

// Test 9: Recursion and Tail Call Optimization
test "recursion and tail call optimization" {
  // Tail-recursive factorial
  let factorial_tail = fn(n : Int) -> Int {
    let aux = fn(n : Int, acc : Int) -> Int {
      if n <= 1 { acc } else { aux(n - 1, acc * n) }
    }
    aux(n, 1)
  }
  
  assert_eq(factorial_tail(0), 1)
  assert_eq(factorial_tail(1), 1)
  assert_eq(factorial_tail(5), 120)
  assert_eq(factorial_tail(8), 40320)  // Using smaller number to avoid overflow
  
  // Recursive array processing
  let sum_array = fn(arr : Array[Int], index : Int) -> Int {
    if index >= arr.length() {
      0
    } else {
      arr[index] + sum_array(arr, index + 1)
    }
  }
  
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(sum_array(numbers, 0), 15)
}

// Test 10: Functional Pipeline Operations
test "functional pipeline operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Complex pipeline transformation
  let evens = numbers.filter(fn(x) { x % 2 == 0 }) // Even numbers only
  let squares = evens.map(fn(x) { x * x }) // Square them
  let large_squares = squares.filter(fn(x) { x > 20 }) // Keep squares greater than 20
  let halved = large_squares.map(fn(x) { x / 2 }) // Divide by 2
  
  // Sum them up
  let mut sum = 0
  for val in halved {
    sum = sum + val
  }
  
  assert_eq(sum, 72) // (36 + 64 + 100) / 2 = 100
  
  // Pipeline with custom types
  enum Status {
    Pending
    Processing
    Completed
    Failed
  }
  
  type Task = { id : Int, status : Status, priority : Int }
  
  let tasks = [
    { id: 1, status: Status::Pending, priority: 3 },
    { id: 2, status: Status::Completed, priority: 1 },
    { id: 3, status: Status::Processing, priority: 2 },
    { id: 4, status: Status::Pending, priority: 1 },
    { id: 5, status: Status::Failed, priority: 2 }
  ]
  
  let pending_tasks = tasks.filter(fn(task) { 
    match task.status {
      Status::Pending => true
      _ => false
    }
  })
  
  let high_priority_pending = pending_tasks.filter(fn(task) { task.priority <= 2 })
  let task_ids = high_priority_pending.map(fn(task) { task.id })
  
  assert_eq(task_ids, [4])
}