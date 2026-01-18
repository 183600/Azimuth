// 用户标准 MoonBit 测试用例
// 测试 azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -5
  let d = 8
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculations" {
  // 测试实际应用场景计算
  // 场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 应用折扣和运费
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(582, total)  // 199*3 - 20 + 15 = 597 - 20 + 15 = 592
}

test "complex_expression_evaluation" {
  // 测试复杂表达式计算
  // 计算：((a + b) * c) - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum, c)  // 100
  let result = @azimuth.add(product, -d)  // 80
  
  @azimuth.assert_eq(80, result)
}

test "sequential_operations" {
  // 测试连续操作
  // 连续加法：1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum1)
  
  // 连续乘法：2 * 3 * 4
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, product1)
  
  // 混合操作：(1 + 2) * (3 + 4)
  let mixed = @azimuth.multiply(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(21, mixed)
}

test "error_prevention_mechanisms" {
  // 测试错误预防机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_formulas" {
  // 测试数学公式计算
  // 计算简单利息：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5  // 5%
  let time = 3  // 3年
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time)
  @azimuth.assert_eq(15000, interest)  // 1000 * 5 * 3 = 15000
  
  // 计算总价：本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "special_character_string_handling" {
  // 测试特殊字符字符串处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, emoji🚀!", @azimuth.greet("emoji🚀"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}