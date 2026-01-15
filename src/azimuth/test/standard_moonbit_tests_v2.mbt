// 标准 MoonBit 测试用例集 v2
// 测试 azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
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
  let d = 8
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_calculator_scenarios" {
  // 测试实际计算器场景
  // 场景1：计算折扣价格
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, final_price)
  
  // 场景2：计算矩形面积和周长
  let length = 10
  let width = 6
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(60, area)
  @azimuth.assert_eq(32, perimeter)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式评估
  // 计算：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 5
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)  // 20
  let product = @azimuth.multiply(sum_ab, c)  // 100
  let result = @azimuth.add(product, -d)  // 80
  
  @azimuth.assert_eq(80, result)
  
  // 计算：a * (b + c) + d * e
  let e = 3
  let sum_bc = @azimuth.add(b, c)  // 17
  let term1 = @azimuth.multiply(a, sum_bc)  // 136
  let term2 = @azimuth.multiply(d, e)  // 60
  let result2 = @azimuth.add(term1, term2)  // 196
  
  @azimuth.assert_eq(196, result2)
}

test "error_resilience_and_edge_cases" {
  // 测试错误恢复和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值组合
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "sequence_and_pattern_operations" {
  // 测试序列和模式操作
  // 计算前n个自然数的和：n * (n + 1) / 2
  let n = 10
  let sum_n = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum_n)
  
  // 计算等差数列的和：n * (首项 + 末项) / 2
  let first_term = 3
  let last_term = 15
  let terms_count = 7
  let arithmetic_sum = @azimuth.multiply(terms_count, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(63, arithmetic_sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算订单总价
  let item_price = 99
  let quantity = 4
  let shipping = 12
  let tax_rate = 8
  
  let subtotal = @azimuth.multiply(item_price, quantity)  // 396
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100  // 31
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)  // 439
  
  @azimuth.assert_eq(439, total)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 88
  let score5 = 95
  
  let sum_scores = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  let average_score = sum_scores / 5
  
  @azimuth.assert_eq(87, average_score)
}

test "unicode_and_special_character_handling" {
  // 测试Unicode和特殊字符处理
  // 测试各种Unicode字符
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, Ñoël!", @azimuth.greet("Ñoël"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  
  // 测试特殊符号和表情符号
  @azimuth.assert_eq_string("Hello, 🚀🌙!", @azimuth.greet("🚀🌙"))
  @azimuth.assert_eq_string("Hello, @#%&*!", @azimuth.greet("@#%&*"))
  @azimuth.assert_eq_string("Hello, [Test]!", @azimuth.greet("[Test]"))
  
  // 测试混合字符集
  @azimuth.assert_eq_string("Hello, Hello世界123!", @azimuth.greet("Hello世界123"))
}