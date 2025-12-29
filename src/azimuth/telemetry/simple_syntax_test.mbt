// 简单测试验证MoonBit测试语法

test "simple_string_test" {
  let test_string = "Hello, World!"
  assert_eq(test_string, "Hello, World!")
  assert_eq(test_string.length(), 13)
}

test "simple_number_test" {
  let test_number = 42
  assert_eq(test_number, 42)
  assert_eq(test_number > 40, true)
}

test "simple_array_test" {
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
}