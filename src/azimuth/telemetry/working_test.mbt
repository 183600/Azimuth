// 简单的MoonBit测试用例，只使用最基本的语法

test "string_operations" {
  let greeting = "Hello"
  let target = "World"
  let combined = greeting + ", " + target + "!"
  
  assert_eq(combined, "Hello, World!")
  assert_eq(greeting.length(), 5)
  assert_eq(target.length(), 5)
  assert_eq(combined.length(), 13)
}

test "numeric_operations" {
  let int_val = 42
  let float_val = 3.14159
  
  assert_eq(int_val, 42)
  assert_eq(float_val, 3.14159)
  
  let sum = int_val + 8
  assert_eq(sum, 50)
  
  let product = float_val * 2.0
  assert_eq(product, 6.28318)
}

test "array_operations" {
  let string_array = ["apple", "banana", "cherry"]
  let int_array = [1, 2, 3, 4, 5]
  
  assert_eq(string_array.length(), 3)
  assert_eq(int_array.length(), 5)
  
  assert_eq(string_array[0], "apple")
  assert_eq(string_array[2], "cherry")
  
  assert_eq(int_array[0], 1)
  assert_eq(int_array[4], 5)
}

test "boolean_operations" {
  let true_val = true
  let false_val = false
  
  assert_eq(true_val, true)
  assert_eq(false_val, false)
  
  assert_eq(true_val && false_val, false)
  assert_eq(true_val || false_val, true)
  assert_eq(!true_val, false)
}

test "option_operations" {
  let some_value = Some(42)
  
  match some_value {
    Some(value) => assert_eq(value, 42)
    None => @test.fail("Expected Some(value)")
  }
}

test "array_creation" {
  let zero_ints = Array::make(8, 0)
  assert_eq(zero_ints.length(), 8)
  assert_eq(zero_ints[0], 0)
  assert_eq(zero_ints[7], 0)
  
  let zero_strings = Array::make(5, "")
  assert_eq(zero_strings.length(), 5)
  assert_eq(zero_strings[0], "")
  assert_eq(zero_strings[4], "")
}