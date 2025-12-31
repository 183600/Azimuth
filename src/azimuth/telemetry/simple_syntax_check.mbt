// 简单语法验证测试

test "simple_syntax_check" {
  let test_string = "hello"
  assert_eq(test_string.length(), 5)
}

test "number_operations" {
  let test_number = 42
  assert_eq(test_number, 42)
}

test "array_operations" {
  let test_array = [1, 2, 3]
  assert_eq(test_array.length(), 3)
}