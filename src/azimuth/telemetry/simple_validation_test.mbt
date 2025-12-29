// 简单验证测试用例，不依赖外部类型

test "basic_string_operations" {
  let test_string = "telemetry_test"
  assert_eq(test_string.length(), 15)
  assert_eq(test_string.has_prefix("telemetry"), true)
  assert_eq(test_string.has_suffix("test"), true)
}

test "array_operations" {
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
  
  let sum = 0
  for item in test_array {
    sum = sum + item
  }
  assert_eq(sum, 15)
}

test "boolean_logic" {
  let true_val = true
  let false_val = false
  assert_eq(true_val, true)
  assert_eq(false_val, false)
  assert_eq(true_val and false_val, false)
  assert_eq(true_val or false_val, true)
  assert_eq(not false_val, true)
}

test "numeric_operations" {
  let int_val = 42
  let float_val = 3.14
  
  assert_eq(int_val > 40, true)
  assert_eq(int_val < 50, true)
  assert_eq(float_val > 3.0, true)
  assert_eq(float_val < 4.0, true)
  
  let result = int_val.to_double() + float_val
  assert_eq(result > 45.0, true)
  assert_eq(result < 46.0, true)
}