// 简单语法验证测试

test "simple_string_operations" {
  let test_string = "hello world"
  assert_eq(test_string.length(), 11)
  assert_eq(test_string.contains("hello"), true)
  assert_eq(test_string.has_prefix("hello"), true)
  assert_eq(test_string.has_suffix("world"), true)
}

test "simple_numeric_operations" {
  let test_number = 42
  assert_eq(test_number, 42)
  assert_eq(test_number > 40, true)
  assert_eq(test_number < 50, true)
  assert_eq(test_number.to_string(), "42")
}

test "simple_boolean_operations" {
  let test_bool = true
  assert_eq(test_bool, true)
  assert_eq(not test_bool, false)
  assert_eq(test_bool and true, true)
  assert_eq(test_bool or false, true)
}