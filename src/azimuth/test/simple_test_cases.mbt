// 简单的 MoonBit 测试用例
// 导入 azimuth 包以使用其中的函数
test "add_positive_numbers" {
  // 测试两个正数相加
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(100, azimuth::add(50, 50))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_negative_numbers" {
  // 测试两个负数相加
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(-100, azimuth::add(-50, -50))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_mixed_numbers" {
  // 测试正负数相加
  assert_eq(0, azimuth::add(5, -5))
  assert_eq(-2, azimuth::add(3, -5))
  assert_eq(2, azimuth::add(-3, 5))
}

test "multiply_basic" {
  // 测试基本乘法
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(5, azimuth::multiply(5, 1))
  assert_eq(-5, azimuth::multiply(5, -1))
}

test "multiply_negative" {
  // 测试负数乘法
  assert_eq(15, azimuth::multiply(-3, -5))
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(-15, azimuth::multiply(-3, 5))
}

test "greet_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "greet_special_chars" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
}

test "complex_operations" {
  // 测试复合运算
  // (2 + 3) * 4 = 20
  let sum = azimuth::add(2, 3)
  let result = azimuth::multiply(sum, 4)
  assert_eq(20, result)
  
  // 5 + (6 * 7) = 47
  let product = azimuth::multiply(6, 7)
  let result2 = azimuth::add(5, product)
  assert_eq(47, result2)
}