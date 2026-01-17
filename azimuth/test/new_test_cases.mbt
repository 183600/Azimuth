// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖多种测试场景

test "large_number_addition" {
  // 测试大数加法
  @azimuth.assert_eq(@azimuth.add(1000000, 2000000), 3000000)
  @azimuth.assert_eq(@azimuth.add(999999, 1), 1000000)
  @azimuth.assert_eq(@azimuth.add(500000, 500000), 1000000)
}

test "large_number_multiplication" {
  // 测试大数乘法
  @azimuth.assert_eq(@azimuth.multiply(1000, 1000), 1000000)
  @azimuth.assert_eq(@azimuth.multiply(2500, 400), 1000000)
  @azimuth.assert_eq(@azimuth.multiply(12345, 10), 123450)
}

test "empty_string_greeting" {
  // 测试空字符串问候
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet(" "), "Hello,  !")
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result, 32)
  
  // a * b + c * d
  let d = 2
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(result2, 28)
}

test "boundary_value_testing" {
  // 测试边界值
  @azimuth.assert_eq(@azimuth.add(2147483647, 0), 2147483647)
  @azimuth.assert_eq(@azimuth.add(-2147483648, 0), -2147483648)
  @azimuth.assert_eq(@azimuth.multiply(1, 2147483647), 2147483647)
  @azimuth.assert_eq(@azimuth.multiply(-1, -2147483648), -2147483648)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(@azimuth.add(-100, -200), -300)
  @azimuth.assert_eq(@azimuth.add(-150, 50), -100)
  @azimuth.assert_eq(@azimuth.multiply(-10, 10), -100)
  @azimuth.assert_eq(@azimuth.multiply(-5, -5), 25)
}

test "unicode_string_greeting" {
  // 测试Unicode字符串问候
  @azimuth.assert_eq_string(@azimuth.greet("张三"), "Hello, 张三!")
  @azimuth.assert_eq_string(@azimuth.greet("🌟"), "Hello, 🌟!")
  @azimuth.assert_eq_string(@azimuth.greet("café"), "Hello, café!")
}

test "mathematical_identities_extended" {
  // 测试扩展的数学恒等式
  // 分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 结合律: (a + b) + c = a + (b + c)
  let left2 = @azimuth.add(@azimuth.add(a, b), c)
  let right2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left2, right2)
}

test "real_world_calculations" {
  // 测试实际应用场景计算
  // 购物车计算：商品价格 * 数量 + 税费
  let item_price = 25
  let quantity = 4
  let tax_rate = 8  // 8%
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(total, 108)
  
  // 面积计算：长方形面积 + 正方形面积
  let rect_length = 10
  let rect_width = 6
  let square_side = 4
  let rect_area = @azimuth.multiply(rect_length, rect_width)
  let square_area = @azimuth.multiply(square_side, square_side)
  let total_area = @azimuth.add(rect_area, square_area)
  @azimuth.assert_eq(total_area, 76)
}

test "sequential_operations" {
  // 测试连续运算
  let mut result = 0
  
  // 连续加法
  result = @azimuth.add(result, 10)  // 10
  result = @azimuth.add(result, 20)  // 30
  result = @azimuth.add(result, 30)  // 60
  @azimuth.assert_eq(result, 60)
  
  // 连续乘法
  let mut product = 1
  product = @azimuth.multiply(product, 2)  // 2
  product = @azimuth.multiply(product, 3)  // 6
  product = @azimuth.multiply(product, 4)  // 24
  @azimuth.assert_eq(product, 24)
  
  // 混合运算
  let mixed = @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(2, 4))
  @azimuth.assert_eq(mixed, 23)
}