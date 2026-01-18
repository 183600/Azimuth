// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  let c = -100
  let d = 250
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
}

test "multiply_by_zero" {
  // 测试乘以零的特性
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-9876, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -9876))
}

test "multiply_distributive_law" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "add_identity_element" {
  // 测试加法单位元：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
}

test "multiply_identity_element" {
  // 测试乘法单位元：a × 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
}

test "greet_with_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) × c - d
  let a = 15
  let b = 25
  let c = 4
  let d = 30
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(130, result)  // (15 + 25) × 4 - 30 = 40 × 4 - 30 = 160 - 30 = 130
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试边界值相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "real_world_calculation" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(647, final_total)  // (199×2) + (299×1) - 50 = 398 + 299 - 50 = 647
}