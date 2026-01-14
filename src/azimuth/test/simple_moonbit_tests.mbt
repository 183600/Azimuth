// 简单的 MoonBit 测试用例
// 测试基本功能
test "add_basic" {
  // 测试基本加法
  let result = azimuth::@azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
test "multiply_basic" {
  // 测试基本乘法
  let result = azimuth::@azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}
test "greet_basic" {
  // 测试基本问候功能
  let result = azimuth::@azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}
test "add_negative" {
  // 测试负数加法
  let result = azimuth::@azimuth.add(-2, -3)
  @azimuth.assert_eq(-5, result)
}
test "multiply_negative" {
  // 测试负数乘法
  let result = azimuth::@azimuth.multiply(-2, 3)
  @azimuth.assert_eq(-6, result)
}
test "add_zero" {
  // 测试与0相加
  let result1 = azimuth::@azimuth.add(5, 0)
  let result2 = azimuth::@azimuth.add(0, 5)
  @azimuth.assert_eq(5, result1)
  @azimuth.assert_eq(5, result2)
}
test "multiply_zero" {
  // 测试与0相乘
  let result1 = azimuth::@azimuth.multiply(5, 0)
  let result2 = azimuth::@azimuth.multiply(0, 5)
  @azimuth.assert_eq(0, result1)
  @azimuth.assert_eq(0, result2)
}
test "greet_empty" {
  // 测试空字符串问候
  let result = azimuth::@azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}
test "complex_calculation" {
  // 测试复合计算：(2 + 3) * 4 = 20
  let sum = azimuth::@azimuth.add(2, 3)
  let result = azimuth::@azimuth.multiply(sum, 4)
  @azimuth.assert_eq(20, result)
}
test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  
  // 测试最小值溢出
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}