// 简单的新测试用例
// 基本语法测试

test "basic_string_operations" {
  let greeting = "Hello"
  let target = "World"
  let combined = greeting + ", " + target + "!"
  
  assert_eq(combined, "Hello, World!")
  assert_eq(greeting.length(), 5)
  assert_eq(target.length(), 5)
  assert_eq(combined.length(), 13)
}

test "basic_numeric_operations" {
  let int_val = 42
  let float_val = 3.14159
  
  assert_eq(int_val, 42)
  assert_eq(float_val, 3.14159)
  
  let sum = int_val + 8
  assert_eq(sum, 50)
  
  let product = float_val * 2.0
  assert_eq(product, 6.28318)
}

test "basic_array_operations" {
  let string_array = ["apple", "banana", "cherry"]
  let int_array = [1, 2, 3, 4, 5]
  
  assert_eq(string_array.length(), 3)
  assert_eq(int_array.length(), 5)
  
  assert_eq(string_array[0], "apple")
  assert_eq(int_array[4], 5)
  
  let mapped_array = int_array.map(fn(x) { x * 2 })
  assert_eq(mapped_array, [2, 4, 6, 8, 10])
}

test "basic_boolean_operations" {
  let true_val = true
  let false_val = false
  
  assert_eq(true_val, true)
  assert_eq(false_val, false)
  
  assert_eq(true_val && false_val, false)
  assert_eq(true_val || false_val, true)
  assert_eq(!true_val, false)
  assert_eq(!false_val, true)
}