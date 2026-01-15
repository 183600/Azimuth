// 增强的 MoonBit 测试用例
// 测试 azimuth 库的各种功能和边界情况

// 测试1: 大数运算测试
test "large_number_operations" {
  // 测试接近边界值的大数运算
  let large_num1 = 1000000000
  let large_num2 = 1500000000
  
  // 大数相加（应该触发溢出保护）
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(2147483647, sum_result)
  
  // 大数相乘（应该触发溢出保护）
  let product_result = @azimuth.multiply(50000, 50000)
  @azimuth.assert_eq(2147483647, product_result)
}

// 测试2: 特殊值组合测试
test "special_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 零与特殊值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

// 测试3: 字符串边界情况测试
test "string_boundary_cases" {
  // 测试空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello,   World!", @azimuth.greet("  World"))
  @azimuth.assert_eq_string("Hello, World   !", @azimuth.greet("World  "))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

// 测试4: 复合运算场景测试
test "complex_calculation_scenarios" {
  // 场景1: 计算圆柱体表面积（简化）
  // 表面积 = 2 × π × r × (r + h) ≈ 6 × r × (r + h)
  let radius = 5
  let height = 10
  
  let radius_plus_height = @azimuth.add(radius, height)  // 15
  let surface_area = @azimuth.multiply(6, @azimuth.multiply(radius, radius_plus_height))  // 6 × 5 × 15 = 450
  @azimuth.assert_eq(450, surface_area)
  
  // 场景2: 计算投资回报（简化）
  // 回报 = 本金 × (1 + 收益率 × 年数)
  let principal = 10000
  let return_rate = 8  // 8%
  let years = 5
  
  let growth_factor = @azimuth.add(1, @azimuth.multiply(return_rate, years))  // 1 + 8×5 = 41
  let returns = @azimuth.multiply(principal, growth_factor)  // 10000 × 41 = 410000
  @azimuth.assert_eq(410000, returns)
}

// 测试5: 数学恒等式验证测试
test "mathematical_identities" {
  let a = 12
  let b = 7
  let c = 5
  
  // 验证加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)  // (12 + 7) + 5 = 24
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))  // 12 + (7 + 5) = 24
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 验证乘法分配律: a × (b + c) = a × b + a × c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))  // 12 × (7 + 5) = 144
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))  // 12×7 + 12×5 = 144
  @azimuth.assert_eq(distrib_left, distrib_right)
}

// 测试6: 边界值溢出保护测试
test "overflow_protection_boundary" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试各种溢出情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
  
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-100000, 30000))
}

// 测试7: 实际应用场景测试
test "practical_application_scenarios" {
  // 场景1: 计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 20
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)  // 598
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)  // 597
  let subtotal = @azimuth.add(item1_total, item2_total)  // 1195
  
  // 计算税费
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100  // 95
  let total_with_tax = @azimuth.add(subtotal, tax_amount)  // 1290
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(total_with_tax, shipping_fee)  // 1310
  @azimuth.assert_eq(1310, final_total)
  
  // 场景2: 计算平均分
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 89
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)  // 440
  let average_score = total_score / 5  // 88
  @azimuth.assert_eq(88, average_score)
}

// 测试8: 条件运算测试
test "conditional_operations" {
  // 模拟条件运算：如果 a > b，则计算 a + b，否则计算 a × b
  let a = 15
  let b = 20
  
  // 由于 a < b，应该计算 a × b
  let condition = a > b  // false
  let result = if condition { 
    @azimuth.add(a, b) 
  } else { 
    @azimuth.multiply(a, b) 
  }
  @azimuth.assert_eq(300, result)  // 15 × 20 = 300
  
  // 反向测试
  let c = 25
  let d = 10
  
  // 由于 c > d，应该计算 c + d
  let condition2 = c > d  // true
  let result2 = if condition2 { 
    @azimuth.add(c, d) 
  } else { 
    @azimuth.multiply(c, d) 
  }
  @azimuth.assert_eq(35, result2)  // 25 + 10 = 35
}

// 测试9: 数值范围验证测试
test "numeric_range_validation" {
  // 测试正数范围
  @azimuth.assert_true(@azimuth.add(1000000, 2000000) > 0)
  @azimuth.assert_true(@azimuth.multiply(1000, 1000) > 0)
  
  // 测试负数范围
  @azimuth.assert_true(@azimuth.add(-1000000, -2000000) < 0)
  @azimuth.assert_true(@azimuth.multiply(-1000, 1000) < 0)
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456789))
  @azimuth.assert_eq(123456789, @azimuth.add(0, 123456789))
  
  // 测试边界比较
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_true(max_val > 0)
  @azimuth.assert_true(min_val < 0)
  @azimuth.assert_true(max_val > min_val)
}

// 测试10: 字符串格式化测试
test "string_formatting_tests" {
  // 测试各种字符串格式
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, user_2023!", @azimuth.greet("user_2023"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, John@Doe#123!", @azimuth.greet("John@Doe#123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, John张三123!", @azimuth.greet("John张三123"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, Mary Jane!", @azimuth.greet("Mary Jane"))
  @azimuth.assert_eq_string("Hello,  Hello World  !", @azimuth.greet(" Hello World  "))
}