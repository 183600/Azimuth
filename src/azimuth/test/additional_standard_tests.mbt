// 额外的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "mathematical_properties" {
  // 测试数学性质：结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  let result3 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result4 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result3, result4)
}

test "distributive_property" {
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "special_case_handling" {
  // 测试特殊情况处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试与-1的运算
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算多项式：3x² + 2x + 1，当x=5时
  let x = 5
  let x_squared = @azimuth.multiply(x, x)
  let term1 = @azimuth.multiply(3, x_squared)
  let term2 = @azimuth.multiply(2, x)
  let result = @azimuth.add(@azimuth.add(term1, term2), 1)
  @azimuth.assert_eq(86, result)  // 3*25 + 2*5 + 1 = 75 + 10 + 1 = 86
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25*2 + 30 = 80
}

test "error_conditions" {
  // 测试错误条件和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试运算结果的正确性
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
  @azimuth.assert_true(@azimuth.multiply(4, 5) == 20)
  @azimuth.assert_false(@azimuth.multiply(4, 5) == 21)
  
  // 测试边界值运算
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

test "performance_calculations" {
  // 测试性能相关的计算
  // 计算等差数列的和：1 + 2 + 3 + ... + 100
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算等比数列的和：2 + 2² + 2³ + 2⁴
  let term1 = 2
  let term2 = @azimuth.multiply(2, 2)
  let term3 = @azimuth.multiply(2, @azimuth.multiply(2, 2))
  let term4 = @azimuth.multiply(2, @azimuth.multiply(2, @azimuth.multiply(2, 2)))
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(30, geometric_sum)  // 2 + 4 + 8 + 16 = 30
}