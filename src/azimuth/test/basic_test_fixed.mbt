// 测试函数和断言函数从 azimuth 包导入

// 测试函数和断言函数从 azimuth 包导入

// 修改版测试文件，使用 test_helper 中的函数

test "test_1_basic_add_fixed" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
}

test "test_1_basic_multiply_fixed" {
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(21, multiply(-7, -3))
}

test "test_1_basic_greet_fixed" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
}
