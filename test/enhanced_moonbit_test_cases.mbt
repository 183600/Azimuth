// 新增的简洁 MoonBit 测试用例
// 包含 5 个基本测试场景

test "simple_addition_test" {
  // 测试基本加法功能
  azimuth.assert_eq(5, azimuth.add(2, 3))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-5, azimuth.add(-2, -3))
}

test "simple_multiplication_test" {
  // 测试基本乘法功能
  azimuth.assert_eq(6, azimuth.multiply(2, 3))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(-6, azimuth.multiply(-2, 3))
}

test "simple_greet_test" {
  // 测试基本问候功能
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
}

test "simple_divide_with_ceil_test" {
  // 测试基本向上取整除法
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "simple_combined_operations" {
  // 测试组合运算
  let result = azimuth.add(azimuth.multiply(2, 3), azimuth.multiply(4, 5))
  azimuth.assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}