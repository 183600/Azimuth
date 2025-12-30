// 最简单的测试用例，用于验证语法

test "simple_string_test" {
  let test_string = "hello"
  assert_eq(test_string.length(), 5)
  assert_eq(test_string, "hello")
}

test "simple_number_test" {
  let test_number = 42
  assert_eq(test_number > 40, true)
  assert_eq(test_number < 50, true)
}

test "simple_array_test" {
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
}