// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "edge_case_zero_operations" {
  // 测试零值运算的边界情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "overflow_protection_mechanism" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数的交换律
  let c = -5
  let d = 7
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算：(a + b) * c - d
  let a = 8
  let b = 7
  let c = 3
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let result = @azimuth.add(product, -d)  // 35
  
  @azimuth.assert_eq(35, result)
}

test "real_world_shopping_scenario" {
  // 测试实际购物场景
  let apple_price = 5
  let apple_quantity = 8
  let banana_price = 3
  let banana_quantity = 12
  let discount = 20
  
  // 计算总价
  let apple_total = @azimuth.multiply(apple_price, apple_quantity)  // 40
  let banana_total = @azimuth.multiply(banana_price, banana_quantity)  // 36
  let subtotal = @azimuth.add(apple_total, banana_total)  // 76
  
  // 应用折扣
  let final_total = @azimuth.add(subtotal, -discount)  // 56
  
  @azimuth.assert_eq(56, final_total)
}

test "special_character_string_handling" {
  // 测试特殊字符字符串处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 7
  let c = 4
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))  // 6 * (7 + 4) = 66
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))  // 6*7 + 6*4 = 42 + 24 = 66
  
  @azimuth.assert_eq(left_side, right_side)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最大值与1相加（应限制在最大值）
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 最小值与-1相加（应限制在最小值）
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 最小值乘以1
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 最大值乘以0
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}