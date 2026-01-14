test "add_zero_handling" {
  // 测试加法的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-42, @azimuth.add(0, -42))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
}

test "multiply_edge_cases" {
  // 测试乘法的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -9876))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-9876, @azimuth.multiply(1, -9876))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(9876, @azimuth.multiply(-1, -9876))
}

test "add_negative_boundary" {
  // 测试负数加法的边界情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1, min_val))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val + 100, @azimuth.add(min_val, 100))
}

test "multiply_min_value_special_cases" {
  // 测试最小值乘法的特殊情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "greet_unicode_characters" {
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 15
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算订单总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_fee)
  
  // 验证结果：199*3 + 199*3*8/100 + 15 = 597 + 47 + 15 = 659
  @azimuth.assert_eq(659, total)
}

test "array_index_calculation" {
  // 测试数组索引计算：二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 10  // 数组宽度
  
  // 计算一维索引：row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(34, index)  // 3*10 + 4 = 34
}

test "physics_calculation" {
  // 测试物理计算：自由落体距离（简化版）
  // 距离 = 0.5 * g * t^2，简化为整数计算
  let gravity = 10  // 简化的重力加速度
  let time = 5
  
  // 计算距离：0.5 * 10 * 5^2 = 5 * 25 = 125
  let distance = @azimuth.multiply(gravity / 2, @azimuth.multiply(time, time))
  @azimuth.assert_eq(125, distance)
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @azimuth.greet("abcdefghijklmnopqrstuvwxyz"))
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let a = 12
  let b = 8
  let c = 5
  let d = 3
  
  // 计算：(a + b) * (c - d)
  let result1 = @azimuth.multiply(
    @azimuth.add(a, b),
    @azimuth.add(c, @azimuth.multiply(d, -1))
  )
  @azimuth.assert_eq(40, result1)  // (12 + 8) * (5 - 3) = 20 * 2 = 40
  
  // 计算：a * b + c * d
  let result2 = @azimuth.add(
    @azimuth.multiply(a, b),
    @azimuth.multiply(c, d)
  )
  @azimuth.assert_eq(111, result2)  // 12 * 8 + 5 * 3 = 96 + 15 = 111
}