// Azimuth Simple Type System Tests
// This file contains basic test cases focusing on MoonBit's type system

// Test 1: Basic Type Operations
test "basic type operations" {
  // Test type inference
  let inferred_int = 42
  let inferred_str = "hello"
  let inferred_bool = true
  
  assert_eq(inferred_int, 42)
  assert_eq(inferred_str, "hello")
  assert_eq(inferred_bool, true)
  
  // Test explicit type annotations
  let explicit_int : Int = 100
  let explicit_str : String = "world"
  let explicit_bool : Bool = false
  
  assert_eq(explicit_int, 100)
  assert_eq(explicit_str, "world")
  assert_eq(explicit_bool, false)
}

// Test 2: Array Operations
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c"]
  let bools = [true, false, true]
  
  // Test array length
  assert_eq(numbers.length(), 5)
  assert_eq(strings.length(), 3)
  assert_eq(bools.length(), 3)
  
  // Test array access
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  assert_eq(strings[1], "b")
  assert_eq(bools[2], true)
  
  // Test array operations
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
}

// Test 3: Option Type Operations
test "option type operations" {
  let some_value = Some(42)
  let none_value : Option[Int] = None
  
  // Test pattern matching on Option
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test Option operations
  let mapped = match some_value {
    Some(v) => Some(v * 2)
    None => None
  }
  
  match mapped {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

// Test 4: Result Type Operations
test "result type operations" {
  let success = Ok("success")
  let failure : Result[String, String] = Err("error")
  
  // Test pattern matching on Result
  match success {
    Ok(value) => assert_eq(value, "success")
    Err(msg) => assert_true(false)
  }
  
  match failure {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "error")
  }
  
  // Test Result operations
  let mapped = match success {
    Ok(value) => Ok(value.length())
    Err(msg) => Err(msg)
  }
  
  match mapped {
    Ok(len) => assert_eq(len, 7)
    Err(_) => assert_true(false)
  }
}

// Test 5: Enum Types
test "enum types" {
  enum Color {
    Red
    Green
    Blue
  }
  
  enum Status {
    Pending
    Processing
    Completed
    Failed
  }
  
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  
  let pending = Status::Pending
  let completed = Status::Completed
  
  // Test pattern matching on enums
  let color_name = match red {
    Color::Red => "red"
    Color::Green => "green"
    Color::Blue => "blue"
  }
  assert_eq(color_name, "red")
  
  let is_completed = match completed {
    Status::Completed => true
    _ => false
  }
  assert_true(is_completed)
}

// Test 6: Tuple Operations
test "tuple operations" {
  let pair = (42, "answer")
  let triple = (1, 2, 3)
  
  // Test tuple access
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  
  assert_eq(triple.0 + triple.1 + triple.2, 6)
  
  // Test tuple operations
  let swapped = (pair.1, pair.0)
  assert_eq(swapped.0, "answer")
  assert_eq(swapped.1, 42)
}

// Test 7: Function Types
test "function types" {
  // Test function definition and usage
  let add = fn(a : Int, b : Int) -> Int { a + b }
  let multiply = fn(a : Int, b : Int) -> Int { a * b }
  
  assert_eq(add(2, 3), 5)
  assert_eq(multiply(4, 5), 20)
  
  // Test higher-order functions
  let apply_twice = fn(f : Int -> Int, x : Int) -> Int {
    f(f(x))
  }
  
  let increment = fn(x : Int) -> Int { x + 1 }
  let double = fn(x : Int) -> Int { x * 2 }
  
  assert_eq(apply_twice(increment, 5), 7) // 5 + 1 + 1
  assert_eq(apply_twice(double, 3), 12) // 3 * 2 * 2
}

// Test 8: Record Types
test "record types" {
  type Point = { x : Int, y : Int }
  type Person = { name : String, age : Int }
  
  let point = { x: 10, y: 20 }
  let person = { name: "Alice", age: 30 }
  
  // Test record access
  assert_eq(point.x, 10)
  assert_eq(point.y, 20)
  assert_eq(person.name, "Alice")
  assert_eq(person.age, 30)
  
  // Test record operations
  let distance = point.x * point.x + point.y * point.y
  assert_eq(distance, 500) // 10^2 + 20^2
}

// Test 9: Recursive Functions
test "recursive functions" {
  // Test simple recursion
  let factorial = fn(n : Int) -> Int {
    if n <= 1 { 1 } else { n * factorial(n - 1) }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(6), 720)
  
  // Test tail recursion
  let sum_range = fn(start : Int, end : Int) -> Int {
    let aux = fn(current : Int, acc : Int) -> Int {
      if current > end { acc } else { aux(current + 1, acc + current) }
    }
    aux(start, 0)
  }
  
  assert_eq(sum_range(1, 5), 15) // 1+2+3+4+5
  assert_eq(sum_range(3, 7), 25) // 3+4+5+6+7
}

// Test 10: Pattern Matching
test "pattern matching" {
  // Test pattern matching with literals
  let describe_number = fn(n : Int) -> String {
    match n {
      0 => "zero"
      1 => "one"
      2 => "two"
      _ => "many"
    }
  }
  
  assert_eq(describe_number(0), "zero")
  assert_eq(describe_number(1), "one")
  assert_eq(describe_number(2), "two")
  assert_eq(describe_number(5), "many")
  
  // Test pattern matching with guards
  let categorize = fn(x : Int) -> String {
    match x {
      n if n < 0 => "negative"
      n if n == 0 => "zero"
      n if n > 0 => "positive"
    }
  }
  
  assert_eq(categorize(-5), "negative")
  assert_eq(categorize(0), "zero")
  assert_eq(categorize(10), "positive")
}