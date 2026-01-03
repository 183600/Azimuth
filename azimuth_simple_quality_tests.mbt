// Azimuth Simple Quality Test Suite
// High-quality test cases for basic MoonBit functionality

// Test 1: String Operations
test "string operations" {
  let str = "azimuth-telemetry"
  assert_eq(str.length(), 17)
  assert_eq(str.to_uppercase(), "AZIMUTH-TELEMETRY")
  assert_eq(str.to_lowercase(), "azimuth-telemetry")
  assert_true(str.contains("telemetry"))
  assert_false(str.contains("metrics"))
  assert_eq(str.substring(0, 7), "azimuth")
}

// Test 2: Array Operations
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(doubled, [2, 4, 6, 8, 10])
  assert_eq(evens, [2, 4])
  assert_eq(sum, 15)
  assert_eq(numbers.length(), 5)
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
}

// Test 3: Option Type Operations
test "option type operations" {
  let some_value = Some(42)
  let none_value = None
  
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

// Test 4: Numeric Operations
test "numeric operations" {
  let int_val = 42
  let float_val = 3.14
  
  assert_eq(int_val + 8, 50)
  assert_eq(int_val * 2, 84)
  assert_eq(int_val / 2, 21)
  assert_eq(int_val % 5, 2)
  
  assert_true(int_val > 40)
  assert_true(int_val >= 42)
  assert_false(int_val < 40)
  assert_true(int_val <= 42)
  
  assert_eq(float_val + 1.0, 4.14)
  assert_eq(float_val * 2.0, 6.28)
  assert_true(float_val > 3.0)
  assert_true(float_val < 4.0)
}

// Test 5: Boolean Logic
test "boolean logic" {
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_false(!true)
  assert_true(!false)
  
  let x = 10
  let y = 20
  assert_true(x < y && x > 5)
  assert_true(x > 5 || y < 15)
}

// Test 6: Tuple Operations
test "tuple operations" {
  let pair = (42, "answer")
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  
  let triple = (1, 2, 3)
  assert_eq(triple.0 + triple.1 + triple.2, 6)
  
  let swapped = (pair.1, pair.0)
  assert_eq(swapped.0, "answer")
  assert_eq(swapped.1, 42)
}

// Test 7: Pattern Matching with Enums
test "pattern matching with enums" {
  enum Color {
    Red
    Green
    Blue
    RGB(Int, Int, Int)
  }
  
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  let custom = Color::RGB(255, 128, 0)
  
  let red_name = match red {
    Color::Red => "red"
    Color::Green => "green"
    Color::Blue => "blue"
    Color::RGB(r, g, b) => "custom"
  }
  assert_eq(red_name, "red")
  
  let custom_sum = match custom {
    Color::RGB(r, g, b) => r + g + b
    _ => 0
  }
  assert_eq(custom_sum, 383)
}

// Test 8: Loop and Iteration
test "loop and iteration" {
  let mut sum = 0
  for i = 0; i <= 10; i = i + 1 {
    sum = sum + i
  }
  assert_eq(sum, 55)
  
  let mut product = 1
  for i = 1; i <= 5; i = i + 1 {
    product = product * i
  }
  assert_eq(product, 120)
}

// Test 9: Function Composition
test "function composition" {
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  let add_then_double = fn(x) { double(add_one(x)) }
  let double_then_square = fn(x) { square(double(x)) }
  
  assert_eq(add_then_double(5), 12)
  assert_eq(double_then_square(3), 36)
  
  let numbers = [1, 2, 3, 4, 5]
  let transformed = numbers
    .map(add_one)
    .filter(fn(x) { x % 2 == 0 })
    .map(double)
  
  assert_eq(transformed, [4, 8, 12])
}

// Test 10: Result Type Operations
test "result type operations" {
  let success = Ok(42)
  let failure = Err("error")
  
  match success {
    Ok(value) => assert_eq(value, 42)
    Err(msg) => assert_true(false)
  }
  
  match failure {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "error")
  }
  
  let mapped = success.map(fn(s) { s.length() })
  match mapped {
    Ok(len) => assert_eq(len, 2)
    Err(_) => assert_true(false)
  }
}