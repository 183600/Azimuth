// Azimuth Telemetry System - High Quality Fundamental Tests
// This file contains comprehensive test cases for fundamental data types and operations

// Test 1: Advanced Numeric Operations and Precision
test "advanced numeric operations and precision" {
  // Test integer arithmetic with overflow handling
  let max_int = 2147483647
  let min_int = -2147483648
  
  // Test addition overflow boundaries
  assert_eq(max_int + 0, max_int)
  assert_eq(min_int + 0, min_int)
  assert_eq(max_int - 1, 2147483646)
  assert_eq(min_int + 1, -2147483647)
  
  // Test multiplication edge cases
  assert_eq(0 * max_int, 0)
  assert_eq(1 * max_int, max_int)
  assert_eq(-1 * max_int, -2147483647) // Note: Potential overflow
  
  // Test float precision
  let pi = 3.141592653589793
  let e = 2.718281828459045
  let golden_ratio = 1.618033988749895
  
  // Test floating point arithmetic
  let pi_sum = pi + e
  assert_true(pi_sum > 5.85 && pi_sum < 5.86)
  
  let pi_product = pi * e
  assert_true(pi_product > 8.5 && pi_product < 8.6)
  
  // Test division precision
  let one_third = 1.0 / 3.0
  assert_true(one_third > 0.33 && one_third < 0.34)
  
  // Test square root and power operations (if available)
  let sqrt_16 = 16.0.sqrt()
  assert_eq(sqrt_16, 4.0)
  
  let power_2_8 = 2.0.pow(8.0)
  assert_eq(power_2_8, 256.0)
}

// Test 2: Complex String Operations and Unicode Handling
test "complex string operations and unicode handling" {
  // Test basic string operations
  let basic_str = "Hello, Azimuth!"
  assert_eq(basic_str.length(), 15)
  assert_eq(basic_str.contains("Azimuth"), true)
  assert_eq(basic_str.contains("azimuth"), false) // Case sensitive
  
  // Test string concatenation
  let str1 = "Open"
  let str2 = "Telemetry"
  let combined = str1 + str2
  assert_eq(combined, "OpenTelemetry")
  
  // Test substring operations
  let long_str = "This is a very long string for testing substring operations"
  let substring = long_str.substring(5, 10) // "is a ve"
  assert_eq(substring, "is a ve")
  
  // Test string trimming
  let padded_str = "   trimmed string   "
  let trimmed = padded_str.trim()
  assert_eq(trimmed, "trimmed string")
  
  // Test Unicode string handling
  let unicode_str = "遥测系统 🌍 测试"
  assert_eq(unicode_str.length(), 9) // Unicode characters count
  
  // Test emoji handling
  let emoji_str = "🚀 📊 📈 🎯"
  assert_eq(emoji_str.contains("🚀"), true)
  assert_eq(emoji_str.contains("📊"), true)
  
  // Test string case conversion
  let mixed_case = "MixedCaseString"
  let lower_case = mixed_case.to_lowercase()
  let upper_case = mixed_case.to_uppercase()
  assert_eq(lower_case, "mixedcasestring")
  assert_eq(upper_case, "MIXEDCASESTRING")
  
  // Test string splitting and joining
  let csv_str = "value1,value2,value3,value4"
  let parts = csv_str.split(",")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "value1")
  assert_eq(parts[3], "value4")
  
  let rejoined = parts.join("|")
  assert_eq(rejoined, "value1|value2|value3|value4")
}

// Test 3: Advanced Array and Collection Operations
test "advanced array and collection operations" {
  // Test array creation and manipulation
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  assert_eq(numbers.length(), 10)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[9], 10)
  
  // Test array filtering
  let even_numbers = numbers.filter(|x| x % 2 == 0)
  assert_eq(even_numbers.length(), 5)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[4], 10)
  
  // Test array mapping
  let squared = numbers.map(|x| x * x)
  assert_eq(squared.length(), 10)
  assert_eq(squared[0], 1)
  assert_eq(squared[9], 100)
  
  // Test array reduction
  let sum = numbers.reduce(|acc, x| acc + x, 0)
  assert_eq(sum, 55)
  
  let product = numbers.reduce(|acc, x| acc * x, 1)
  assert_eq(product, 3628800) // 10!
  
  // Test array sorting
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6, 0]
  let sorted = unsorted.sort()
  assert_eq(sorted[0], 0)
  assert_eq(sorted[9], 9)
  
  // Test array reversal
  let reversed = numbers.reverse()
  assert_eq(reversed[0], 10)
  assert_eq(reversed[9], 1)
  
  // Test multidimensional arrays
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  assert_eq(matrix.length(), 3)
  assert_eq(matrix[0].length(), 3)
  assert_eq(matrix[1][1], 5) // Center element
  
  // Test array slicing
  let slice = numbers.slice(2, 7) // Elements from index 2 to 6
  assert_eq(slice.length(), 5)
  assert_eq(slice[0], 3)
  assert_eq(slice[4], 7)
  
  // Test array concatenation
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let concatenated = arr1.concat(arr2)
  assert_eq(concatenated.length(), 6)
  assert_eq(concatenated[2], 3)
  assert_eq(concatenated[3], 4)
}

// Test 4: Boolean Logic and Conditional Operations
test "boolean logic and conditional operations" {
  // Test basic boolean operations
  assert_true(true)
  assert_false(false)
  assert_true(!false)
  assert_false(!true)
  
  // Test AND operations
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  // Test OR operations
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  // Test XOR operations (if available)
  assert_true(true ^ false)
  assert_true(false ^ true)
  assert_false(true ^ true)
  assert_false(false ^ false)
  
  // Test complex boolean expressions
  let a = true
  let b = false
  let c = true
  
  assert_true(a && c)
  assert_false(a && b)
  assert_true(a || b)
  assert_true(b || c)
  assert_false(a && b && c)
  assert_true(a || b || c)
  
  // Test conditional expressions
  let x = 10
  let y = 20
  
  let max = if x > y { x } else { y }
  assert_eq(max, 20)
  
  let min = if x < y { x } else { y }
  assert_eq(min, 10)
  
  // Test nested conditionals
  let result = if x > 5 {
    if y > 15 {
      "both high"
    } else {
      "x high, y low"
    }
  } else {
    if y > 15 {
      "x low, y high"
    } else {
      "both low"
    }
  }
  assert_eq(result, "both high")
  
  // Test boolean conversion from other types
  let zero = 0
  let non_zero = 42
  let empty_str = ""
  let non_empty_str = "hello"
  
  // In many languages, 0 and empty string convert to false
  assert_false(zero.to_bool())
  assert_true(non_zero.to_bool())
  assert_false(empty_str.to_bool())
  assert_true(non_empty_str.to_bool())
}

// Test 5: Option Type and Null Safety Operations
test "option type and null safety operations" {
  // Test Some/None operations
  let some_value = Some(42)
  let none_value = None
  
  // Test option matching
  match some_value {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false) // Should not reach here
  }
  
  match none_value {
    Some(value) => assert_true(false) // Should not reach here
    None => assert_true(true) // Expected case
  }
  
  // Test option mapping
  let mapped_some = Option::map(some_value, |x| x * 2)
  match mapped_some {
    Some(value) => assert_eq(value, 84)
    None => assert_true(false) // Should not reach here
  }
  
  let mapped_none = Option::map(none_value, |x| x * 2)
  assert_eq(mapped_none, None)
  
  // Test option filtering
  let filtered_some = Option::filter(some_value, |x| x > 40)
  match filtered_some {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false) // Should not reach here
  }
  
  let filtered_out = Option::filter(some_value, |x| x > 50)
  assert_eq(filtered_out, None)
  
  // Test option default values
  let some_with_default = Option::unwrap_or(some_value, 0)
  assert_eq(some_with_default, 42)
  
  let none_with_default = Option::unwrap_or(none_value, 0)
  assert_eq(none_with_default, 0)
  
  // Test option chaining
  let chained_result = Option::and_then(some_value, |x| Some(x * 10))
  match chained_result {
    Some(value) => assert_eq(value, 420)
    None => assert_true(false) // Should not reach here
  }
  
  let chained_none = Option::and_then(none_value, |x| Some(x * 10))
  assert_eq(chained_none, None)
  
  // Test option flattening
  let nested_some = Some(Some(42))
  let flattened = Option::flatten(nested_some)
  match flattened {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false) // Should not reach here
  }
  
  let nested_none_inner = Some(None)
  let flattened_inner = Option::flatten(nested_none_inner)
  assert_eq(flattened_inner, None)
  
  let nested_none_outer = None
  let flattened_outer = Option::flatten(nested_none_outer)
  assert_eq(flattened_outer, None)
}

// Test 6: Result Type and Error Handling
test "result type and error handling" {
  // Test Ok/Error operations
  let success_result = Ok(42)
  let error_result = Error("Something went wrong")
  
  // Test result matching
  match success_result {
    Ok(value) => assert_eq(value, 42)
    Error(msg) => assert_true(false) // Should not reach here
  }
  
  match error_result {
    Ok(value) => assert_true(false) // Should not reach here
    Error(msg) => assert_eq(msg, "Something went wrong")
  }
  
  // Test result mapping
  let mapped_success = Result::map(success_result, |x| x * 2)
  match mapped_success {
    Ok(value) => assert_eq(value, 84)
    Error(_) => assert_true(false) // Should not reach here
  }
  
  let mapped_error = Result::map(error_result, |x| x * 2)
  match mapped_error {
    Ok(_) => assert_true(false) // Should not reach here
    Error(msg) => assert_eq(msg, "Something went wrong")
  }
  
  // Test result error mapping
  let error_mapped_success = Result::map_err(success_result, |msg| "New error: " + msg)
  match error_mapped_success {
    Ok(value) => assert_eq(value, 42)
    Error(_) => assert_true(false) // Should not reach here
  }
  
  let error_mapped_error = Result::map_err(error_result, |msg| "New error: " + msg)
  match error_mapped_error {
    Ok(_) => assert_true(false) // Should not reach here
    Error(msg) => assert_eq(msg, "New error: Something went wrong")
  }
  
  // Test result chaining
  let chained_success = Result::and_then(success_result, |x| Ok(x * 10))
  match chained_success {
    Ok(value) => assert_eq(value, 420)
    Error(_) => assert_true(false) // Should not reach here
  }
  
  let chained_error = Result::and_then(error_result, |x| Ok(x * 10))
  match chained_error {
    Ok(_) => assert_true(false) // Should not reach here
    Error(msg) => assert_eq(msg, "Something went wrong")
  }
  
  // Test result unwrapping with defaults
  let unwrapped_success = Result::unwrap_or(success_result, 0)
  assert_eq(unwrapped_success, 42)
  
  let unwrapped_error = Result::unwrap_or(error_result, 0)
  assert_eq(unwrapped_error, 0)
}

// Test 7: Hash Map and Dictionary Operations
test "hash map and dictionary operations" {
  // Test map creation and basic operations
  let mut map = HashMap::new()
  
  // Test insertion
  HashMap::insert(map, "key1", "value1")
  HashMap::insert(map, "key2", "value2")
  HashMap::insert(map, "key3", "value3")
  
  // Test retrieval
  let value1 = HashMap::get(map, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false) // Should not reach here
  }
  
  let non_existent = HashMap::get(map, "non_existent")
  assert_eq(non_existent, None)
  
  // Test update
  HashMap::insert(map, "key1", "new_value1")
  let updated_value = HashMap::get(map, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "new_value1")
    None => assert_true(false) // Should not reach here
  }
  
  // Test removal
  let removed_value = HashMap::remove(map, "key2")
  match removed_value {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false) // Should not reach here
  }
  
  let after_removal = HashMap::get(map, "key2")
  assert_eq(after_removal, None)
  
  // Test map size and emptiness
  assert_eq(HashMap::size(map), 2)
  assert_false(HashMap::is_empty(map))
  
  // Test map clearing
  HashMap::clear(map)
  assert_eq(HashMap::size(map), 0)
  assert_true(HashMap::is_empty(map))
  
  // Test map with different value types
  let mut int_map = HashMap::new()
  HashMap::insert(int_map, "one", 1)
  HashMap::insert(int_map, "two", 2)
  HashMap::insert(int_map, "three", 3)
  
  let int_value = HashMap::get(int_map, "two")
  match int_value {
    Some(v) => assert_eq(v, 2)
    None => assert_true(false) // Should not reach here
  }
  
  // Test map iteration
  let mut keys = []
  let mut values = []
  
  for (key, value) in int_map {
    keys.push(key)
    values.push(value)
  }
  
  assert_eq(keys.length(), 3)
  assert_eq(values.length(), 3)
  assert_true(keys.contains("one"))
  assert_true(keys.contains("two"))
  assert_true(keys.contains("three"))
  assert_true(values.contains(1))
  assert_true(values.contains(2))
  assert_true(values.contains(3))
}

// Test 8: Set Operations and Membership Testing
test "set operations and membership testing" {
  // Test set creation and basic operations
  let mut set = HashSet::new()
  
  // Test insertion
  HashSet::insert(set, 1)
  HashSet::insert(set, 2)
  HashSet::insert(set, 3)
  HashSet::insert(set, 2) // Duplicate, should be ignored
  
  // Test membership
  assert_true(HashSet::contains(set, 1))
  assert_true(HashSet::contains(set, 2))
  assert_true(HashSet::contains(set, 3))
  assert_false(HashSet::contains(set, 4))
  
  // Test set size
  assert_eq(HashSet::size(set), 3) // Duplicate should not increase size
  
  // Test removal
  HashSet::remove(set, 2)
  assert_false(HashSet::contains(set, 2))
  assert_eq(HashSet::size(set), 2)
  
  // Test set operations
  let mut set_a = HashSet::new()
  HashSet::insert(set_a, 1)
  HashSet::insert(set_a, 2)
  HashSet::insert(set_a, 3)
  
  let mut set_b = HashSet::new()
  HashSet::insert(set_b, 3)
  HashSet::insert(set_b, 4)
  HashSet::insert(set_b, 5)
  
  // Test union
  let union_set = HashSet::union(set_a, set_b)
  assert_eq(HashSet::size(union_set), 5)
  assert_true(HashSet::contains(union_set, 1))
  assert_true(HashSet::contains(union_set, 2))
  assert_true(HashSet::contains(union_set, 3))
  assert_true(HashSet::contains(union_set, 4))
  assert_true(HashSet::contains(union_set, 5))
  
  // Test intersection
  let intersection_set = HashSet::intersection(set_a, set_b)
  assert_eq(HashSet::size(intersection_set), 1)
  assert_true(HashSet::contains(intersection_set, 3))
  
  // Test difference
  let difference_set = HashSet::difference(set_a, set_b)
  assert_eq(HashSet::size(difference_set), 2)
  assert_true(HashSet::contains(difference_set, 1))
  assert_true(HashSet::contains(difference_set, 2))
  assert_false(HashSet::contains(difference_set, 3))
  
  // Test subset and superset
  let mut subset_set = HashSet::new()
  HashSet::insert(subset_set, 1)
  HashSet::insert(subset_set, 2)
  
  assert_true(HashSet::is_subset(subset_set, set_a))
  assert_false(HashSet::is_subset(set_a, subset_set))
  assert_true(HashSet::is_superset(set_a, subset_set))
  assert_false(HashSet::is_superset(subset_set, set_a))
  
  // Test set equality
  let mut equal_set = HashSet::new()
  HashSet::insert(equal_set, 1)
  HashSet::insert(equal_set, 2)
  HashSet::insert(equal_set, 3)
  
  assert_true(HashSet::equals(set_a, equal_set))
  
  // Test set iteration
  let mut elements = []
  for element in set_a {
    elements.push(element)
  }
  
  assert_eq(elements.length(), 3)
  assert_true(elements.contains(1))
  assert_true(elements.contains(2))
  assert_true(elements.contains(3))
}

// Test 9: Tuple Operations and Pattern Matching
test "tuple operations and pattern matching" {
  // Test tuple creation and access
  let pair = (1, "hello")
  let triple = (1, "hello", true)
  let quadruple = (1, "hello", true, 3.14)
  
  // Test tuple element access
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "hello")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "hello")
  assert_eq(triple.2, true)
  
  assert_eq(quadruple.0, 1)
  assert_eq(quadruple.1, "hello")
  assert_eq(quadruple.2, true)
  assert_eq(quadruple.3, 3.14)
  
  // Test tuple destructuring
  let (a, b) = pair
  assert_eq(a, 1)
  assert_eq(b, "hello")
  
  let (x, y, z) = triple
  assert_eq(x, 1)
  assert_eq(y, "hello")
  assert_eq(z, true)
  
  // Test tuple comparison
  let pair1 = (1, "hello")
  let pair2 = (1, "hello")
  let pair3 = (1, "world")
  let pair4 = (2, "hello")
  
  assert_eq(pair1, pair2)
  assert_not_eq(pair1, pair3)
  assert_not_eq(pair1, pair4)
  
  // Test tuple ordering
  assert_true(pair1 < pair3) // "hello" < "world"
  assert_true(pair1 < pair4) // 1 < 2
  
  // Test tuple functions
  let swapped = Tuple::swap(pair)
  assert_eq(swapped.0, "hello")
  assert_eq(swapped.1, 1)
  
  let first = Tuple::first(pair)
  let second = Tuple::second(pair)
  assert_eq(first, 1)
  assert_eq(second, "hello")
  
  // Test nested tuples
  let nested = ((1, 2), ("hello", "world"))
  assert_eq(nested.0.0, 1)
  assert_eq(nested.0.1, 2)
  assert_eq(nested.1.0, "hello")
  assert_eq(nested.1.1, "world")
  
  // Test tuple pattern matching
  match pair {
    (1, "hello") => assert_true(true) // Expected case
    _ => assert_true(false) // Should not reach here
  }
  
  match triple {
    (1, "hello", true) => assert_true(true) // Expected case
    _ => assert_true(false) // Should not reach here
  }
  
  // Test tuple with wildcard patterns
  match pair {
    (1, _) => assert_true(true) // Wildcard for second element
    _ => assert_true(false) // Should not reach here
  }
  
  match triple {
    (_, "hello", _) => assert_true(true) // Wildcard for first and third
    _ => assert_true(false) // Should not reach here
  }
}

// Test 10: Type Conversion and Coercion
test "type conversion and coercion" {
  // Test numeric type conversions
  let int_val = 42
  let float_val = 3.14159
  
  // Int to float conversion
  let int_to_float = int_val.to_float()
  assert_eq(int_to_float, 42.0)
  
  // Float to int conversion (truncation)
  let float_to_int = float_val.to_int()
  assert_eq(float_to_int, 3)
  
  // String to numeric conversions
  let str_int = "123"
  let str_float = "3.14159"
  let str_bool = "true"
  
  // String to int
  let parsed_int = str_int.parse_int()
  match parsed_int {
    Ok(value) => assert_eq(value, 123)
    Error(_) => assert_true(false) // Should not reach here
  }
  
  // String to float
  let parsed_float = str_float.parse_float()
  match parsed_float {
    Ok(value) => assert_true(value > 3.14 && value < 3.15)
    Error(_) => assert_true(false) // Should not reach here
  }
  
  // String to bool
  let parsed_bool = str_bool.parse_bool()
  match parsed_bool {
    Ok(value) => assert_true(value)
    Error(_) => assert_true(false) // Should not reach here
  }
  
  // Numeric to string conversions
  let int_str = int_val.to_string()
  assert_eq(int_str, "42")
  
  let float_str = float_val.to_string()
  assert_true(float_str.contains("3.14159"))
  
  let bool_true_str = true.to_string()
  let bool_false_str = false.to_string()
  assert_eq(bool_true_str, "true")
  assert_eq(bool_false_str, "false")
  
  // Test invalid conversions
  let invalid_int_str = "not_a_number"
  let invalid_int_parsed = invalid_int_str.parse_int()
  match invalid_int_parsed {
    Ok(_) => assert_true(false) // Should not reach here
    Error(_) => assert_true(true) // Expected error
  }
  
  // Test safe conversion functions
  let safe_int_to_char = int_val.to_char_safe()
  match safe_int_to_char {
    Some(c) => assert_eq(c, '*') // ASCII 42 is '*'
    None => assert_true(false) // Should not reach here
  }
  
  let large_int = 1000
  let invalid_char = large_int.to_char_safe()
  assert_eq(invalid_char, None) // 1000 is not a valid ASCII char
  
  // Test type checking
  assert_true(int_val.is_int())
  assert_true(float_val.is_float())
  assert_true("hello".is_string())
  assert_true(true.is_bool())
  
  assert_false(int_val.is_float())
  assert_false(float_val.is_int())
  assert_false("hello".is_int())
  assert_true(false.is_bool())
}