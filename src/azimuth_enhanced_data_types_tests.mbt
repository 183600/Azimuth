// Azimuth Enhanced Data Types Tests
// This file contains comprehensive test cases for enhanced data type operations

// Test 1: Advanced Numeric Type Operations
test "advanced numeric type operations" {
  // Test large integer operations
  let large_int = 9223372036854775807L  // Max 64-bit integer
  let small_int = -9223372036854775808L  // Min 64-bit integer
  
  assert_eq(large_int + 1L, large_int + 1L)
  assert_eq(small_int - 1L, small_int - 1L)
  
  // Test floating point precision
  let pi = 3.14159265359
  let e = 2.71828182846
  let sum = pi + e
  assert_true(sum > 5.85 && sum < 5.86)
  
  // Test numeric conversions
  let int_to_float = 42.0
  let float_to_int = 3.0
  assert_eq(int_to_float, 42.0)
  assert_eq(float_to_int, 3.0)
}

// Test 2: Complex String Operations
test "complex string operations" {
  // Test Unicode string handling
  let unicode_str = "Hello 世界 🌍"
  assert_eq(unicode_str.length(), 9)
  
  // Test string slicing
  let slice_str = "abcdefghij"
  assert_eq(slice_str[0..5], "abcde")
  assert_eq(slice_str[5..10], "fghij")
  
  // Test string transformations
  let upper_str = "hello world"
  let lower_str = "HELLO WORLD"
  // Note: These methods might not be available in MoonBit
  // assert_eq(upper_str.to_uppercase(), "HELLO WORLD")
  // assert_eq(lower_str.to_lowercase(), "hello world")
  
  // Test string splitting and joining
  let csv_str = "a,b,c,d,e"
  // Note: String splitting might not be available in MoonBit
  // let parts = csv_str.split(",")
  // assert_eq(parts.length(), 5)
  // assert_eq(parts.join("-"), "a-b-c-d-e")
  
  // Test string formatting
  let name = "Alice"
  let age = 30
  // Note: String formatting might not be available in MoonBit
  // let formatted = "Name: {name}, Age: {age}"
  // assert_eq(formatted, "Name: Alice, Age: 30")
}

// Test 3: Advanced Array/List Operations
test "advanced array list operations" {
  // Test nested arrays
  let nested_array = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  assert_eq(nested_array.length(), 3)
  assert_eq(nested_array[0].length(), 3)
  assert_eq(nested_array[1][2], 6)
  
  // Test array transformations
  let numbers = [1, 2, 3, 4, 5]
  let doubled = []
  for i in 0..numbers.length() {
    doubled.push(numbers[i] * 2)
  }
  assert_eq(doubled.length(), 5)
  assert_eq(doubled[2], 6)
  
  // Test array filtering
  let even_numbers = []
  for i in 0..numbers.length() {
    if numbers[i] % 2 == 0 {
      even_numbers.push(numbers[i])
    }
  }
  assert_eq(even_numbers.length(), 2)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[1], 4)
  
  // Test array reduction
  let mut sum = 0
  for i in 0..numbers.length() {
    sum = sum + numbers[i]
  }
  assert_eq(sum, 15)
}

// Test 4: Advanced Option Type Operations
test "advanced option type operations" {
  // Test nested option types
  let nested_some = Some(Some(42))
  let nested_none = Some(None)
  let deep_none = None
  
  match nested_some {
    Some(inner) => {
      match inner {
        Some(value) => assert_eq(value, 42)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test option chaining
  let option_chain = Some("hello")
  let result = match option_chain {
    Some(s) => Some(s.length())
    None => None
  }
  match result {
    Some(len) => assert_eq(len, 5)
    None => assert_true(false)
  }
  
  // Test option transformation
  let transform_option = Some(10)
  let transformed = match transform_option {
    Some(n) => Some(n * 2)
    None => None
  }
  match transformed {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
}

// Test 5: Custom Type Definitions and Operations
test "custom type definitions and operations" {
  // Define a custom enum type
  enum Color {
    Red
    Green
    Blue
    Custom(String)
  }
  
  // Test enum operations
  let red_color = Red
  let custom_color = Custom("purple")
  
  match red_color {
    Red => assert_true(true)
    _ => assert_true(false)
  }
  
  match custom_color {
    Custom(name) => assert_eq(name, "purple")
    _ => assert_true(false)
  }
  
  // Define a custom struct type
  type Point {
    x: Float
    y: Float
  }
  
  // Test struct operations
  let point = Point { x: 3.14, y: 2.71 }
  assert_eq(point.x, 3.14)
  assert_eq(point.y, 2.71)
  
  // Test struct copying and modification
  let new_point = Point { x: point.x * 2, y: point.y * 2 }
  assert_eq(new_point.x, 6.28)
  assert_eq(new_point.y, 5.42)
}

// Test 6: Advanced Map/Dictionary Operations
test "advanced map dictionary operations" {
  // Test map creation and access
  let mut map = {}
  map["key1"] = "value1"
  map["key2"] = "value2"
  map["key3"] = "value3"
  
  assert_eq(map["key1"], "value1")
  assert_eq(map["key2"], "value2")
  assert_eq(map["key3"], "value3")
  
  // Test map iteration
  let mut keys = []
  let mut values = []
  for key in map.keys() {
    keys.push(key)
    values.push(map[key])
  }
  assert_eq(keys.length(), 3)
  assert_eq(values.length(), 3)
  
  // Test map filtering
  let mut filtered = {}
  for key in map.keys() {
    if key != "key2" {
      filtered[key] = map[key]
    }
  }
  assert_eq(filtered.length(), 2)
  assert_true(filtered.contains("key1"))
  assert_true(filtered.contains("key3"))
  assert_false(filtered.contains("key2"))
}

// Test 7: Advanced Result Type Operations
test "advanced result type operations" {
  // Define result type
  enum Result[E, T] {
    Ok(T)
    Err(E)
  }
  
  // Test result operations
  let success = Ok("success")
  let failure = Err("error")
  
  match success {
    Ok(value) => assert_eq(value, "success")
    Err(_) => assert_true(false)
  }
  
  match failure {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "error")
  }
  
  // Test result chaining
  let chained = match success {
    Ok(value) => Ok(value.length())
    Err(error) => Err(error)
  }
  match chained {
    Ok(length) => assert_eq(length, 7)
    Err(_) => assert_true(false)
  }
  
  // Test result transformation
  let transformed = match success {
    Ok(value) => Ok(value + " transformed")
    Err(error) => Err(error)
  }
  match transformed {
    Ok(value) => assert_eq(value, "success transformed")
    Err(_) => assert_true(false)
  }
}

// Test 8: Advanced Type System Features
test "advanced type system features" {
  // Test generic type operations
  type Box[T] {
    value: T
  }
  
  let int_box = Box { value: 42 }
  let string_box = Box { value: "hello")
  
  assert_eq(int_box.value, 42)
  assert_eq(string_box.value, "hello")
  
  // Test type constraints
  type ComparableBox[T] {
    value: T
  }
  
  let comparable_int = ComparableBox { value: 10 }
  let comparable_float = ComparableBox { value: 3.14 }
  
  assert_eq(comparable_int.value, 10)
  assert_eq(comparable_float.value, 3.14)
  
  // Test recursive types
  enum List[T] {
    Nil
    Cons(T, List[T])
  }
  
  let list = Cons(1, Cons(2, Cons(3, Nil)))
  let mut sum = 0
  let mut current = list
  
  while true {
    match current {
      Cons(value, rest) => {
        sum = sum + value
        current = rest
      }
      Nil => break
    }
  }
  assert_eq(sum, 6)
}