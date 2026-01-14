// MoonBit 测试示例
// 包含多种测试用例，覆盖项目的核心功能

// 基本算术运算测试
test "basic_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
}

test "basic_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

// 字符串处理测试
test "string_greeting" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 边界条件测试
test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 溢出保护测试
test "overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试减法溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 复合运算测试
test "compound_operations" {
  // 测试 (a + b) * c
  let a = 10
  let b = 20
  let c = 3
  
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(90, result)
  
  // 测试 a * b + c * d
  let a2 = 5
  let b2 = 6
  let c2 = 7
  let d2 = 8
  
  let product1 = @azimuth.multiply(a2, b2)
  let product2 = @azimuth.multiply(c2, d2)
  let result2 = @azimuth.add(product1, product2)
  @azimuth.assert_eq(86, result2)
}

// 特殊值测试
test "special_values" {
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  
  // 测试单位值
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(1, -5))
  
  // 测试负单位值
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))
  @azimuth.assert_eq(5, @azimuth.multiply(-1, -5))
}

// 断言函数测试
test "assertion_functions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试相等断言
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

// 实际应用场景测试
test "real_world_scenarios" {
  // 计算购物车总价
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  let quantity1 = 2
  let quantity2 = 1
  let quantity3 = 3
  
  let total1 = @azimuth.multiply(item1_price, quantity1)
  let total2 = @azimuth.multiply(item2_price, quantity2)
  let total3 = @azimuth.multiply(item3_price, quantity3)
  let grand_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  @azimuth.assert_eq(199 * 2 + 299 + 399 * 3, grand_total)
}

// 数学函数测试
test "mathematical_functions" {
  // 测试平方计算
  let num = 13
  let square = @azimuth.multiply(num, num)
  @azimuth.assert_eq(169, square)
  
  // 测试立方计算
  let cube = @azimuth.multiply(square, num)
  @azimuth.assert_eq(2197, cube)
  
  // 测试阶乘简化版本 (5! = 5 * 4 * 3 * 2 * 1)
  let factorial_5 = @azimuth.multiply(
    @azimuth.multiply(5, 4),
    @azimuth.multiply(3, @azimuth.multiply(2, 1))
  )
  @azimuth.assert_eq(120, factorial_5)
}