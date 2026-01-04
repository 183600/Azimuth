// Azimuth Telemetry System - MoonBit Language Features Tests
// This file contains test cases that showcase MoonBit language features

// Test 1: Pattern Matching with Complex Types
test "pattern matching with complex types" {
  let data = [
    ("string", StringValue("hello")),
    ("int", IntValue(42)),
    ("bool", BoolValue(true)),
    ("float", FloatValue(3.14))
  ]
  
  for (type_name, value) in data {
    match (type_name, value) {
      ("string", StringValue(s)) => {
        assert_eq(s, "hello")
        assert_eq(s.length(), 5)
      }
      ("int", IntValue(i)) => {
        assert_eq(i, 42)
        assert_true(i > 0)
      }
      ("bool", BoolValue(b)) => {
        assert_true(b)
      }
      ("float", FloatValue(f)) => {
        assert_true(f > 3.0 && f < 4.0)
      }
      _ => assert_true(false)
    }
  }
}

// Test 2: Higher-Order Functions and Closures
test "higher-order functions and closures" {
  // Test map function
  let numbers = [1, 2, 3, 4, 5]
  let doubled = map(numbers, fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // Test filter function
  let evens = filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  // Test fold/reduce function
  let sum = fold(numbers, 0, fn(acc, x) { acc + x })
  assert_eq(sum, 15)
  
  // Test closure with captured variable
  let multiplier = 3
  let multiply_by_three = fn(x) { x * multiplier }
  let result = multiply_by_three(5)
  assert_eq(result, 15)
}

// Test 3: Custom Type Definitions and Methods
type Point {
  x : Int
  y : Int
}

type Circle {
  center : Point
  radius : Float
}

test "custom type definitions and methods" {
  // Create and test Point
  let p1 = { x: 3, y: 4 }
  assert_eq(p1.x, 3)
  assert_eq(p1.y, 4)
  
  // Create and test Circle
  let c1 = { center: p1, radius: 5.0 }
  assert_eq(c1.center.x, 3)
  assert_eq(c1.center.y, 4)
  assert_eq(c1.radius, 5.0)
  
  // Test distance calculation
  let p2 = { x: 0, y: 0 }
  let distance = sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)
  assert_eq(distance, 5.0)
}

// Test 4: Enum Types and Exhaustive Matching
type Color {
  Red
  Green
  Blue
  Custom(String)
}

type Result[T, E] {
  Ok(T)
  Err(E)
}

test "enum types and exhaustive matching" {
  let colors = [Red, Green, Blue, Custom("purple")]
  
  for color in colors {
    let rgb_value = match color {
      Red => "#FF0000"
      Green => "#00FF00"
      Blue => "#0000FF"
      Custom(name) => "custom: " + name
    }
    
    match color {
      Red => assert_eq(rgb_value, "#FF0000")
      Green => assert_eq(rgb_value, "#00FF00")
      Blue => assert_eq(rgb_value, "#0000FF")
      Custom(name) => assert_eq(rgb_value, "custom: " + name)
    }
  }
  
  // Test Result type
  let success = Ok(42)
  let failure = Err("Error message")
  
  match success {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  match failure {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Error message")
  }
}

// Test 5: Recursive Functions and Data Structures
type List[T] {
  Nil
  Cons(T, List[T])
}

test "recursive functions and data structures" {
  // Create a list
  let list = Cons(1, Cons(2, Cons(3, Nil)))
  
  // Define recursive sum function
  let sum = fn(lst : List[Int]) -> Int {
    match lst {
      Nil => 0
      Cons(head, tail) => head + sum(tail)
    }
  }
  
  assert_eq(sum(list), 6)
  
  // Define recursive map function for lists
  let map_list = fn(lst : List[Int], f : (Int) -> Int) -> List[Int] {
    match lst {
      Nil => Nil
      Cons(head, tail) => Cons(f(head), map_list(tail, f))
    }
  }
  
  let doubled_list = map_list(list, fn(x) { x * 2 })
  assert_eq(sum(doubled_list), 12)
}

// Test 6: Pipeline Operator and Function Composition
test "pipeline operator and function composition" {
  let add_one = fn(x : Int) { x + 1 }
  let double = fn(x : Int) { x * 2 }
  let square = fn(x : Int) { x * x }
  
  // Test function composition
  let add_one_then_double = fn(x) { double(add_one(x)) }
  assert_eq(add_one_then_double(3), 8)
  
  let double_then_square = fn(x) { square(double(x)) }
  assert_eq(double_then_square(3), 36)
  
  // Test pipeline operations
  let result1 = 3 |> add_one |> double |> square
  assert_eq(result1, 64)
  
  let result2 = 3 |> double |> add_one |> square
  assert_eq(result2, 49)
}

// Test 7: Module System and Namespaces
test "module system and namespaces" {
  // Test creating and using modules
  let math_ops = {
    add: fn(a, b) { a + b },
    subtract: fn(a, b) { a - b },
    multiply: fn(a, b) { a * b },
    divide: fn(a, b) { 
      if b != 0 { a / b } else { 0 }
    }
  }
  
  assert_eq(math_ops.add(5, 3), 8)
  assert_eq(math_ops.subtract(5, 3), 2)
  assert_eq(math_ops.multiply(5, 3), 15)
  assert_eq(math_ops.divide(6, 3), 2)
  assert_eq(math_ops.divide(6, 0), 0)
}

// Test 8: Generic Types and Polymorphism
type Container[T] {
  items : Array[T]
}

test "generic types and polymorphism" {
  // Create generic container functions
  let make_container = fn[T](items : Array[T]) -> Container[T] {
    { items: items }
  }
  
  let container_size = fn[T](c : Container[T]) -> Int {
    c.items.length()
  }
  
  let container_get = fn[T](c : Container[T], index : Int) -> T {
    c.items[index]
  }
  
  // Test with int container
  let int_container = make_container([1, 2, 3, 4, 5])
  assert_eq(container_size(int_container), 5)
  assert_eq(container_get(int_container, 2), 3)
  
  // Test with string container
  let string_container = make_container(["hello", "world", "azimuth"])
  assert_eq(container_size(string_container), 3)
  assert_eq(container_get(string_container, 1), "world")
  
  // Test with float container
  let float_container = make_container([1.1, 2.2, 3.3])
  assert_eq(container_size(float_container), 3)
  assert_eq(container_get(float_container, 0), 1.1)
}

// Test 9: Lazy Evaluation and Memoization
test "lazy evaluation and memoization" {
  // Create a memoized function
  let mut cache = []
  
  let memoized_fib = fn(n : Int) -> Int {
    // Check if already computed
    for i in 0..cache.length() {
      if cache[i].0 == n {
        return cache[i].1
      }
    }
    
    // Compute and cache
    let result = if n <= 1 {
      n
    } else {
      memoized_fib(n - 1) + memoized_fib(n - 2)
    }
    
    cache.push((n, result))
    result
  }
  
  // Test memoization
  assert_eq(memoized_fib(5), 5)
  assert_eq(memoized_fib(10), 55)
  assert_eq(memoized_fib(5), 5) // Should use cached value
  
  // Verify cache contains expected entries
  assert_true(cache.length() >= 6) // At least fib(0) through fib(5)
}

// Test 10: Error Handling with Try-Catch Pattern
type Try[T, E] {
  Success(T)
  Failure(E)
}

test "error handling with try-catch pattern" {
  // Safe division function
  let safe_divide = fn(a : Int, b : Int) -> Try[Int, String] {
    if b == 0 {
      Failure("Division by zero")
    } else {
      Success(a / b)
    }
  }
  
  // Test successful division
  match safe_divide(10, 2) {
    Success(result) => assert_eq(result, 5)
    Failure(_) => assert_true(false)
  }
  
  // Test division by zero
  match safe_divide(10, 0) {
    Success(_) => assert_true(false)
    Failure(error) => assert_eq(error, "Division by zero")
  }
  
  // Chain operations with error handling
  let result = match safe_divide(20, 4) {
    Success(q) => match safe_divide(q, 2) {
      Success(final) => Success(final * 3)
      Failure(e) => Failure(e)
    }
    Failure(e) => Failure(e)
  }
  
  match result {
    Success(value) => assert_eq(value, 7) // (20/4)/2*3 = 5/2*3 = 2*3 = 6 (integer division)
    Failure(_) => assert_true(false)
  }
}