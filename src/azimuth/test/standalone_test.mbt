// 测试函数和断言函数从 azimuth 包导入

// 测试函数和断言函数从 azimuth 包导入

// 独立测试文件，使用 test_helper 中的函数定义
test "add_basic_standalone" {
  assert_eq(5, add(2, 3))
}

test "add_negative_standalone" {
  assert_eq(0, add(5, -5))
}

test "multiply_basic_standalone" {
  assert_eq(6, multiply(2, 3))
}

test "multiply_negative_standalone" {
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic_standalone" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_empty_standalone" {
  assert_eq_string("Hello, !", greet(""))
}