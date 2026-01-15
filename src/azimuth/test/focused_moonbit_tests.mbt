// 专注于核心功能的 MoonBit 测试用例
// 覆盖基本运算、边界条件和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -12
  let d = 15
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算多项式值
  // 计算：3x² + 2x + 1，其中 x = 4
  let x = 4
  
  // 计算 x²
  let x_squared = @azimuth.multiply(x, x)  // 16
  
  // 计算 3x²
  let term1 = @azimuth.multiply(3, x_squared)  // 48
  
  // 计算 2x
  let term2 = @azimuth.multiply(2, x)  // 8
  
  // 计算最终结果
  let result = @azimuth.add(@azimuth.add(term1, term2), 1)  // 48 + 8 + 1 = 57
  
  @azimuth.assert_eq(57, result)
}

test "real_world_application_calculations" {
  // 测试实际应用计算：购物车折扣计算
  let item_price = 199
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10  // 10% 折扣
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  
  // 判断是否满足折扣条件
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100  // 59
  } else {
    0
  }
  
  // 计算最终价格
  let final_price = @azimuth.add(subtotal, -discount_amount)  // 538
  
  @azimuth.assert_eq(538, final_price)
}

test "zero_handling_special_cases" {
  // 测试零处理的特殊情况
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 加法中的零
  @azimuth.assert_eq(positive, @azimuth.add(positive, zero))
  @azimuth.assert_eq(negative, @azimuth.add(negative, zero))
  @azimuth.assert_eq(zero, @azimuth.add(zero, zero))
  
  // 乘法中的零
  @azimuth.assert_eq(zero, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(negative, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, zero))
  
  // 零与边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
}

test "sequential_operations_chain" {
  // 测试连续操作链：计算 ((a + b) * c) - d
  let a = 8
  let b = 7
  let c = 5
  let d = 12
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 75
  let result = @azimuth.add(product, -d)  // 63
  
  @azimuth.assert_eq(63, result)
  
  // 测试不同的操作顺序
  let alt_result = @azimuth.add(@azimuth.multiply(@azimuth.add(a, b), c), -d)
  @azimuth.assert_eq(result, alt_result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试各种特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaces   !", @azimuth.greet("  spaces  "))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 3.14159!", @azimuth.greet("3.14159"))
  
  // 测试混合字符
  @azimuth.assert_eq_string("Hello, a1b2c3!", @azimuth.greet("a1b2c3"))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
}

test "mathematical_identities_validation" {
  // 测试数学恒等式验证
  let x = 13
  let y = 7
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零律
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
  
  // 负数恒等式
  @azimuth.assert_eq(0, @azimuth.add(x, @azimuth.multiply(-1, x)))
  @azimuth.assert_eq(@azimuth.multiply(-1, x), @azimuth.add(0, -x))
}