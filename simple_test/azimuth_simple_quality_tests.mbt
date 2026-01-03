// Azimuth Simple Quality Test Suite
// High-quality test cases for basic MoonBit functionality

// Test 1: String Operations
test "string operations" {
  let str = "azimuth-telemetry"
  assert_eq(str.length(), 17)
  // Note: to_uppercase and to_lowercase are not available in current MoonBit
  assert_true(str.contains("telemetry"))
  assert_false(str.contains("metrics"))
  assert_eq(str[0:7], "azimuth")
}

// Test 2: Array Operations
test "array operations" {
  let numbers = [1, 2, 3, 4, 5]
  // Note: map, filter, and reduce are not available in current MoonBit
  // We'll test basic array operations instead
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  // Test array operations
  let first_element = numbers[0]
  let last_element = numbers[4]
  assert_eq(first_element, 1)
  assert_eq(last_element, 5)
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

// Test 5: Numeric Operations
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
  
  // Use approximate equality for floating point
  let sum_result = float_val + 1.0
  let product_result = float_val * 2.0
  assert_true(sum_result > 4.13 && sum_result < 4.15)
  assert_true(product_result > 6.27 && product_result < 6.29)
  assert_true(float_val > 3.0)
  assert_true(float_val < 4.0)
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

// Test 7: Pattern Matching
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
  
  let squares = [1, 4, 9, 16, 25]
  assert_eq(squares.length(), 5)
  assert_eq(squares[2], 9)
}

// Test 9: Function Composition
test "function composition" {
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  // Manual function composition
  let add_then_double = fn(x) { double(add_one(x)) }
  let double_then_square = fn(x) { square(double(x)) }
  
  assert_eq(add_then_double(5), 12)
  assert_eq(double_then_square(3), 36)
  
  // Test with individual transformations
  let num1 = add_one(1)
  let num2 = add_one(2)
  let num3 = add_one(3)
  let num4 = add_one(4)
  let num5 = add_one(5)
  
  let doubled_num1 = double(num1)
  let doubled_num2 = double(num2)
  
  assert_eq(doubled_num1, 4)
  assert_eq(doubled_num2, 6)
}

// Test 10: Option Type Advanced Operations
test "option type advanced operations" {
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
  
  let doubled = match some_value {
    Some(v) => Some(v * 2)
    None => None
  }
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

// Test 4: Boolean Logic
test "boolean logic operations" {
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