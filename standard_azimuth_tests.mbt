// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能测试

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
}

test "multiply_function_identity" {
  // 测试乘法单位元：a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
}

test "divide_with_ceil_fractional_cases" {
  // 测试向上取整除法的小数情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))    // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))    // 2.666... -> 3
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 4))   // 5.25 -> 5
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(20, 3))   // 6.666... -> 7
}

test "greet_function_whitespace_handling" {
  // 测试问候函数的空白字符处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))      // 只有空格
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))         // 空字符串
  @azimuth.assert_eq_string("Hello, a b!", @azimuth.greet("a b"))   // 包含空格的字符串
}

test "complex_nested_calculations" {
  // 测试复杂的嵌套计算
  let x = 10
  let y = 15
  let z = 3
  
  // 计算：(x + y) * z - x / y
  let step1 = @azimuth.add(x, y)           // 10 + 15 = 25
  let step2 = @azimuth.multiply(step1, z)  // 25 * 3 = 75
  let step3 = @azimuth.divide_with_ceil(x, y)  // ceil(10/15) = 1
  let result = @azimuth.add(step2, @azimuth.multiply(-1, step3))     // 75 - 1 = 74
  
  @azimuth.assert_eq(74, result)
}

test "negative_divide_with_ceil_precision" {
  // 测试负数向上取整除法的精度
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))    // -4/2 = -2 -> -2
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))    // -3/2 = -1.5 -> -1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-7, 2))    // -7/2 = -3.5 -> -3
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, -3))     // 7/-3 = -2.333... -> -2 (根据实现可能不同)
}

test "boundary_value_operations" {
  // 测试边界值操作
  @azimuth.assert_eq(1, @azimuth.add(0, 1))              // 从零开始
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))            // 负数边界
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))         // 最小正整数乘法
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))         // 零乘法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1)) // 最小正整数除法
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数的组合
  let prefix = "Dr. "
  let name = "Smith"
  let suffix = " Jr."
  let full_name = prefix + name + suffix
  let formal_greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, Dr. Smith Jr.!", formal_greeting)
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列前n项和
  let first_term = 3
  let common_diff = 4
  let n_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let double_sum = @azimuth.multiply(n_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(n_terms - 1, common_diff)))
  let sum = @azimuth.divide_with_ceil(double_sum, 2)
  
  // 手动验证：3 + 7 + 11 + 15 + 19 + 23 = 78
  @azimuth.assert_eq(78, sum)
}

test "real_world_business_calculation" {
  // 测试真实世界业务计算：订单总价
  let unit_price = 45
  let quantity = 12
  let tax_rate_percent = 8
  let shipping_cost = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate_percent), 100)
  
  // 计算最终总价
  let total_cost = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  
  @azimuth.assert_eq(540, subtotal)     // 45 * 12 = 540
  @azimuth.assert_eq(44, tax_amount)    // ceil(540 * 8 / 100) = ceil(43.2) = 44
  @azimuth.assert_eq(599, total_cost)   // 540 + 44 + 15 = 599
}