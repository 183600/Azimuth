// 额外的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "integer_addition_comprehensive" {
  // 测试整数加法的各种情况
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 100))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -100))
}

test "integer_multiplication_comprehensive" {
  // 测试整数乘法的各种情况
  @azimuth.assert_eq(81, @azimuth.multiply(9, 9))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(-144, @azimuth.multiply(-12, 12))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  @azimuth.assert_eq_string("Hello, Space!", @azimuth.greet("Space"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let x = 15
  let y = 25
  let z = 10
  
  // 加法结合律: (x + y) + z = x + (y + z)
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(x, y), z),
    @azimuth.add(x, @azimuth.add(y, z))
  )
  
  // 乘法分配律: x × (y + z) = x × y + x × z
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

test "practical_business_calculations" {
  // 测试实际业务计算
  // 场景：计算订单总价（商品价格 × 数量 + 税费 - 折扣）
  let product_price = 299
  let quantity = 3
  let tax_rate = 8  // 8%
  let discount = 50
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(@azimuth.add(subtotal, tax), -discount)
  
  @azimuth.assert_eq(943, total)
}

test "scientific_calculations" {
  // 测试科学计算
  // 场景：计算简单物理公式
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时（简化为整数计算：2小时30分钟）
  
  let distance_hours = @azimuth.multiply(speed, 2)
  let distance_minutes = @azimuth.multiply(speed, 30) / 60
  let total_distance = @azimuth.add(distance_hours, distance_minutes)
  
  @azimuth.assert_eq(150, total_distance)
}

test "algorithmic_pattern_testing" {
  // 测试算法模式
  // 场景：计算等差数列前n项和
  let first_term = 3
  let common_diff = 5
  let n = 6
  
  // 手动计算：3 + 8 + 13 + 18 + 23 + 28 = 93
  let term1 = first_term
  let term2 = @azimuth.add(first_term, common_diff)
  let term3 = @azimuth.add(term2, common_diff)
  let term4 = @azimuth.add(term3, common_diff)
  let term5 = @azimuth.add(term4, common_diff)
  let term6 = @azimuth.add(term5, common_diff)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5), term6)
  @azimuth.assert_eq(93, sum)
}

test "complex_string_operations" {
  // 测试复杂字符串操作
  // 场景：生成用户欢迎消息
  let username = "Alice"
  
  let welcome_msg = @azimuth.greet(username)
  let full_msg = welcome_msg + " Welcome to the system!"
  
  @azimuth.assert_eq_string("Hello, Alice! Welcome to the system!", full_msg)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试正常运算
  @azimuth.assert_true(@azimuth.add(100, 200) == 300)
  @azimuth.assert_true(@azimuth.multiply(10, 20) == 200)
}

test "performance_optimization_scenarios" {
  // 测试性能优化场景
  // 场景：批量计算优化
  let base_value = 100
  let multiplier = 5
  let iterations = 10
  
  // 使用公式计算：base_value × multiplier × iterations
  let optimized_result = @azimuth.multiply(@azimuth.multiply(base_value, multiplier), iterations)
  @azimuth.assert_eq(5000, optimized_result)
  
  // 验证与逐步计算结果一致
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.multiply(step1, iterations)
  @azimuth.assert_eq(optimized_result, step2)
}