// MoonBit 测试用例示例
// 使用标准的 MoonBit 测试语法

// 导入主模块
use azimuth

// 测试基本加法运算
test "add_basic_operation" {
  let result = azimuth.add(3, 5)
  @assertion.assert_eq(result, 8)?
}

// 测试加法零值情况
test "add_with_zero" {
  @assertion.assert_eq(azimuth.add(0, 10), 10)?
  @assertion.assert_eq(azimuth.add(10, 0), 10)?
  @assertion.assert_eq(azimuth.add(0, 0), 0)?
}

// 测试加法负数情况
test "add_with_negative_numbers" {
  @assertion.assert_eq(azimuth.add(-3, 5), 2)?
  @assertion.assert_eq(azimuth.add(3, -5), -2)?
  @assertion.assert_eq(azimuth.add(-3, -5), -8)?
}

// 测试基本乘法运算
test "multiply_basic_operation" {
  let result = azimuth.multiply(4, 6)
  @assertion.assert_eq(result, 24)?
}

// 测试乘法零值情况
test "multiply_with_zero" {
  @assertion.assert_eq(azimuth.multiply(0, 10), 0)?
  @assertion.assert_eq(azimuth.multiply(10, 0), 0)?
  @assertion.assert_eq(azimuth.multiply(0, 0), 0)?
}

// 测试乘法单位元情况
test "multiply_with_identity" {
  @assertion.assert_eq(azimuth.multiply(1, 10), 10)?
  @assertion.assert_eq(azimuth.multiply(10, 1), 10)?
  @assertion.assert_eq(azimuth.multiply(-1, 10), -10)?
  @assertion.assert_eq(azimuth.multiply(10, -1), -10)?
}

// 测试字符串拼接函数
test "greet_basic_function" {
  let result = azimuth.greet("MoonBit")
  @assertion.assert_eq(result, "Hello, MoonBit!")?
}

// 测试字符串拼接空值情况
test "greet_with_empty_string" {
  let result = azimuth.greet("")
  @assertion.assert_eq(result, "Hello, !")?
}

// 测试边界值情况
test "boundary_values_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @assertion.assert_eq(azimuth.add(max_val, 1), max_val)?
  
  // 测试最小值加法
  @assertion.assert_eq(azimuth.add(min_val, -1), min_val)?
  
  // 测试最大值乘法
  @assertion.assert_eq(azimuth.multiply(max_val, 2), max_val)?
}