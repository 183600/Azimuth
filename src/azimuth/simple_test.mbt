// 简单的新测试用例

///|
test "simple_math_properties" {
  // 测试加法交换律
  assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  
  // 测试乘法交换律
  assert_eq(@azimuth.multiply(4, 7), @azimuth.multiply(7, 4))
  
  // 测试实际应用：计算总价
  let price = 199
  let quantity = 3
  let total = @azimuth.multiply(price, quantity)
  assert_eq(597, total)
}