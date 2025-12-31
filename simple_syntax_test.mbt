// 简单测试验证新创建的测试用例语法

test "simple_syntax_validation" {
  // 简单语法验证测试
  let test_value = 42
  assert_eq(test_value, 42)
}

test "string_operations_test" {
  // 字符串操作测试
  let test_string = "hello telemetry"
  assert_eq(test_string.length(), 15)
  assert_eq(test_string.has_prefix("hello"), true)
  assert_eq(test_string.has_suffix("telemetry"), true)
}

test "array_operations_test" {
  // 数组操作测试
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
}