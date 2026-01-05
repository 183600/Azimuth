// Azimuth Telemetry System - MoonBit Advanced Language Features Tests
// This file contains comprehensive test cases for advanced MoonBit language features

// Test 1: Advanced Pattern Matching
test "advanced pattern matching with nested structures" {
  // Test nested pattern matching
  let nested_data = Some((Some(42), "test"))
  
  match nested_data {
    Some((Some(num), str)) => {
      assert_eq(num, 42)
      assert_eq(str, "test")
    }
    _ => assert_true(false)
  }
  
  // Test pattern matching with guards
  let value = 15
  let result = match value {
    x if x < 10 => "small"
    x if x < 20 => "medium"
    _ => "large"
  }
  assert_eq(result, "medium")
}

// Test 2: Higher-Order Functions
test "higher-order functions and functional composition" {
  // Test function as parameter
  let apply_twice = fn(f: (Int) -> Int, x: Int) -> Int {
    f(f(x))
  }
  
  let add_one = fn(x: Int) -> Int { x + 1 }
  let result = apply_twice(add_one, 5)
  assert_eq(result, 7)
  
  // Test function composition
  let compose = fn(f: (Int) -> Int, g: (Int) -> Int) -> (Int) -> Int {
    fn(x: Int) -> Int { f(g(x)) }
  }
  
  let multiply_by_two = fn(x: Int) -> Int { x * 2 }
  let add_five = fn(x: Int) -> Int { x + 5 }
  let composed = compose(multiply_by_two, add_five)
  assert_eq(composed(3), 16) // (3 + 5) * 2 = 16
}

// Test 3: Advanced Type System Features
test "advanced type system features" {
  // Test generic types
  let box_int = Box(42)
  let box_string = Box("hello")
  
  match box_int {
    Box(value) => assert_eq(value, 42)
  }
  
  match box_string {
    Box(value) => assert_eq(value, "hello")
  }
  
  // Test enum with associated values
  let result = Success(100)
  match result {
    Success(value) => assert_eq(value, 100)
    Error(message) => assert_true(false)
  }
  
  let error_result = Error("Something went wrong")
  match error_result {
    Success(_) => assert_true(false)
    Error(message) => assert_eq(message, "Something went wrong")
  }
}

// Test 4: Recursive Data Structures
test "recursive data structures and algorithms" {
  // Test linked list operations
  let list = Cons(1, Cons(2, Cons(3, Nil)))
  
  let length = fn(lst: List) -> Int {
    match lst {
      Nil => 0
      Cons(_, tail) => 1 + length(tail)
    }
  }
  
  assert_eq(length(list), 3)
  
  // Test tree operations
  let tree = Node(10, 
    Node(5, Leaf, Leaf), 
    Node(15, Leaf, Leaf)
  )
  
  let sum = fn(t: Tree) -> Int {
    match t {
      Leaf => 0
      Node(value, left, right) => value + sum(left) + sum(right)
    }
  }
  
  assert_eq(sum(tree), 30) // 10 + 5 + 15 = 30
}

// Test 5: Advanced Error Handling
test "advanced error handling with result types" {
  // Test safe division with Result type
  let safe_divide = fn(a: Int, b: Int) -> Result(Int, String) {
    if b == 0 {
      Error("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  let success_result = safe_divide(10, 2)
  match success_result {
    Ok(value) => assert_eq(value, 5)
    Error(_) => assert_true(false)
  }
  
  let error_result = safe_divide(10, 0)
  match error_result {
    Ok(_) => assert_true(false)
    Error(message) => assert_eq(message, "Division by zero")
  }
  
  // Test chaining operations with Result
  let chain_operations = fn(x: Int) -> Result(Int, String) {
    match safe_divide(x, 2) {
      Ok(result) => safe_divide(result, 5)
      Error(e) => Error(e)
    }
  }
  
  let chain_result = chain_operations(20)
  match chain_result {
    Ok(value) => assert_eq(value, 2) // (20 / 2) / 5 = 2
    Error(_) => assert_true(false)
  }
}

// Test 6: Lazy Evaluation
test "lazy evaluation and memoization" {
  // Test lazy computation
  let expensive_computation = fn(x: Int) -> Int {
    // Simulate expensive computation
    x * x * x
  }
  
  let lazy_value = Lazy(fn() { expensive_computation(10) })
  
  // Value not computed yet
  assert_false(lazy_value.is_computed())
  
  // Force evaluation
  let result = lazy_value.get()
  assert_eq(result, 1000)
  assert_true(lazy_value.is_computed())
  
  // Subsequent calls use cached value
  let cached_result = lazy_value.get()
  assert_eq(cached_result, 1000)
}

// Test 7: Advanced Collection Operations
test "advanced collection operations" {
  // Test map and filter
  let numbers = [1, 2, 3, 4, 5]
  
  let doubled = numbers.map(fn(x: Int) -> Int { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(fn(x: Int) -> Bool { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  // Test fold/reduce
  let sum = numbers.fold(0, fn(acc: Int, x: Int) -> Int { acc + x })
  assert_eq(sum, 15)
  
  let product = numbers.fold(1, fn(acc: Int, x: Int) -> Int { acc * x })
  assert_eq(product, 120)
  
  // Test flat_map
  let nested = [[1, 2], [3, 4], [5]]
  let flattened = nested.flat_map(fn(x: Array) -> Array { x })
  assert_eq(flattened, [1, 2, 3, 4, 5])
}

// Test 8: Advanced String Processing
test "advanced string processing" {
  // Test string interpolation
  let name = "Azimuth"
  let version = "1.0.0"
  let interpolated = "Service: \{name}, Version: \{version}"
  assert_eq(interpolated, "Service: Azimuth, Version: 1.0.0")
  
  // Test string manipulation
  let text = "hello world"
  let upper = text.to_uppercase()
  let lower = text.to_lowercase()
  let reversed = text.reverse()
  
  assert_eq(upper, "HELLO WORLD")
  assert_eq(lower, "hello world")
  assert_eq(reversed, "dlrow olleh")
  
  // Test string splitting and joining
  let csv = "a,b,c,d"
  let parts = csv.split(",")
  assert_eq(parts, ["a", "b", "c", "d"])
  
  let joined = parts.join("-")
  assert_eq(joined, "a-b-c-d")
}

// Test 9: Advanced Numerical Operations
test "advanced numerical operations" {
  // Test big integer operations
  let big_int = BigInt::from_string("12345678901234567890")
  let another_big = BigInt::from_string("98765432109876543210")
  
  let sum = big_int + another_big
  assert_eq(sum.to_string(), "111111111011111111100")
  
  // Test floating point precision
  let precise = 3.14159265359
  let rounded = precise.round(4)
  assert_eq(rounded, 3.1416)
  
  // Test complex numbers
  let complex1 = Complex(3.0, 4.0) // 3 + 4i
  let complex2 = Complex(1.0, 2.0) // 1 + 2i
  
  let sum_complex = complex1 + complex2
  assert_eq(sum_complex.real, 4.0)
  assert_eq(sum_complex.imag, 6.0)
  
  let product_complex = complex1 * complex2
  assert_eq(product_complex.real, -5.0) // (3*1 - 4*2) = -5
  assert_eq(product_complex.imag, 10.0) // (3*2 + 4*1) = 10
}

// Test 10: Advanced Concurrency Patterns
test "advanced concurrency patterns" {
  // Test channel operations
  let channel = Channel::new(10)
  
  // Producer
  let producer = fn(ch: Channel(Int)) -> Unit {
    for i in 0..=4 {
      ch.send(i)
    }
    ch.close()
  }
  
  // Consumer
  let consumer = fn(ch: Channel(Int)) -> Array(Int) {
    let mut results = []
    while true {
      match ch.receive() {
        Some(value) => results.push(value)
        None => break
      }
    }
    results
  }
  
  // Run producer and consumer
  producer(channel)
  let results = consumer(channel)
  assert_eq(results, [0, 1, 2, 3, 4])
  
  // Test async operations
  let async_task = fn(x: Int) -> Future(Int) {
    Future::async(fn() {
      // Simulate async work
      x * x
    })
  }
  
  let future1 = async_task(5)
  let future2 = async_task(10)
  
  let result1 = future1.await()
  let result2 = future2.await()
  
  assert_eq(result1, 25)
  assert_eq(result2, 100)
  
  // Test parallel operations
  let parallel_map = fn(arr: Array(Int), f: (Int) -> Int) -> Array(Int) {
    let futures = arr.map(fn(x: Int) -> Future(Int) {
      Future::async(fn() { f(x) })
    })
    
    futures.map(fn(f: Future(Int)) -> Int { f.await() })
  }
  
  let input = [1, 2, 3, 4, 5]
  let parallel_results = parallel_map(input, fn(x: Int) -> Int { x * 2 })
  assert_eq(parallel_results, [2, 4, 6, 8, 10])
}

// Type definitions for the tests
type Box(T) {
  Box(T)
}

type Result(T, E) {
  Ok(T)
  Error(E)
}

type List {
  Nil
  Cons(Int, List)
}

type Tree {
  Leaf
  Node(Int, Tree, Tree)
}

type Lazy(T) {
  Lazy(() -> T, Bool, Option(T))
  
  get(self) -> T {
    match self {
      Lazy(computation, is_computed, cached_value) => {
        if is_computed {
          match cached_value {
            Some(value) => value
            None => panic!("Inconsistent lazy state")
          }
        } else {
          let value = computation()
          // Update the lazy value with computed result
          value
        }
      }
    }
  }
  
  is_computed(self) -> Bool {
    match self {
      Lazy(_, is_computed, _) => is_computed
    }
  }
}

type Complex {
  Complex(Float, Float)
  
  +(self, other: Complex) -> Complex {
    match (self, other) {
      (Complex(r1, i1), Complex(r2, i2)) => Complex(r1 + r2, i1 + i2)
    }
  }
  
  *(self, other: Complex) -> Complex {
    match (self, other) {
      (Complex(r1, i1), Complex(r2, i2)) => 
        Complex(r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)
    }
  }
}

type Channel(T) {
  // Simplified channel implementation
  Channel(Array(T))
  
  send(self, value: T) -> Unit {
    match self {
      Channel(buffer) => buffer.push(value)
    }
  }
  
  receive(self) -> Option(T) {
    match self {
      Channel(buffer) => {
        if buffer.length() > 0 {
          Some(buffer.shift())
        } else {
          None
        }
      }
    }
  }
  
  close(self) -> Unit {
    // Simplified - just mark as closed
  }
}

type Future(T) {
  // Simplified future implementation
  Future(Option(T))
  
  async(computation: () -> T) -> Future(T) {
    Future(Some(computation()))
  }
  
  await(self) -> T {
    match self {
      Future(Some(value)) => value
      Future(None) => panic!("Future not completed")
    }
  }
}

// BigInt type (simplified)
type BigInt {
  BigInt(String)
  
  from_string(s: String) -> BigInt {
    BigInt(s)
  }
  
  to_string(self) -> String {
    match self {
      BigInt(s) => s
    }
  }
  
  +(self, other: BigInt) -> BigInt {
    match (self, other) {
      (BigInt(a), BigInt(b)) => {
        // Simplified - just concatenate for demo
        BigInt(a + b)
      }
    }
  }
}

// Extensions for existing types
extend Array {
  flat_map(self, f: (Array(T)) -> Array(U)) -> Array(U) {
    let mut result = []
    for item in self {
      let mapped = f(item)
      result = result + mapped
    }
    result
  }
  
  reverse(self) -> Array(T) {
    let mut result = []
    for i in self.length() - 1 ..= 0 {
      result.push(self[i])
    }
    result
  }
}

extend String {
  to_uppercase(self) -> String {
    // Simplified implementation
    self
  }
  
  to_lowercase(self) -> String {
    // Simplified implementation
    self
  }
  
  reverse(self) -> String {
    let mut result = ""
    for i in self.length() - 1 ..= 0 {
      result = result + self[i]
    }
    result
  }
  
  split(self, delimiter: String) -> Array(String) {
    // Simplified implementation
    [self]
  }
}

extend Float {
  round(self, precision: Int) -> Float {
    // Simplified implementation
    self
  }
}