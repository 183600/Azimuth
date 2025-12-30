// 简单测试验证

test "string_operations" {
  let test_string = "telemetry"
  assert_eq(test_string.length(), 9)
  assert_eq(test_string.contains("tele"), true)
}

test "numeric_operations" {
  let test_value = 42
  assert_eq(test_value > 40, true)
  assert_eq(test_value < 50, true)
}

test "array_operations" {
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
}