// 简单的 Azimuth 项目测试用例
// 使用标准 MoonBit 测试语法

test "simple_addition_test" {
  // 简单加法测试
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(-1, 1), 0)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "simple_multiplication_test" {
  // 简单乘法测试
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(-1, 1), -1)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
}

test "simple_greet_test" {
  // 简单问候测试
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
}

test "math_properties_test" {
  // 数学性质测试
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(4, 7), @azimuth.multiply(7, 4))
}

test "practical_calculations_test" {
  // 实际计算测试
  // 计算总价: 单价 × 数量
  let unit_price = 10
  let quantity = 5
  let total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(50, total)
  
  // 计算总和: 小计 + 税费
  let subtotal = 100
  let tax = 10
  let final_total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(110, final_total)
}