test "add_basic_functionality" {
  // 测试基本加法运算
  let result = azimuth::add(2, 3)
  assert_eq(5, result)
}

test "multiply_basic_functionality" {
  // 测试基本乘法运算
  let result = azimuth::multiply(2, 3)
  assert_eq(6, result)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  let result = azimuth::greet("World")
  assert_eq_string("Hello, World!", result)
}