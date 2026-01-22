// 用户聚焦的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 测试1: 基本算术运算验证
test "fundamental_arithmetic_operations" {
  // 测试加法基本功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  // 测试乘法基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
}

// 测试2: 边界值安全处理
test "boundary_value_safety" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试3: 字符串处理多样性
test "string_processing_diversity" {
  // 基本字符串
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 特殊字符字符串
  @azimuth.assert_eq_string("Hello, @#%&!", @azimuth.greet("@#%&"))
}

// 测试4: 数学运算交换律验证
test "mathematical_commutativity" {
  let a = 12
  let b = 17
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

// 测试5: 实际应用场景 - 面积计算
test "practical_area_calculation" {
  // 矩形面积: 长 × 宽
  let length = 15
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, rectangle_area)
  
  // 周长: 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
}

// 测试6: 金融计算场景
test "financial_calculation_scenario" {
  // 本金
  let principal = 5000
  // 利率 (5% 表示为 5)
  let rate = 5
  // 年数
  let years = 3
  
  // 简单利息计算: 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 总金额: 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(80000, total)  // 5000 + 5000*5*3 = 5000 + 75000 = 80000
}

// 测试7: 温度转换应用
test "temperature_conversion_application" {
  // 摄氏度转华氏度 (简化公式: F = C * 2 + 30)
  
  // 测试多个温度点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  @azimuth.assert_eq(50, @azimuth.add(@azimuth.multiply(10, 2), 30))
  @azimuth.assert_eq(70, @azimuth.add(@azimuth.multiply(20, 2), 30))
  @azimuth.assert_eq(90, @azimuth.add(@azimuth.multiply(30, 2), 30))
  @azimuth.assert_eq(110, @azimuth.add(@azimuth.multiply(40, 2), 30))
}

// 测试8: 多步骤复合计算
test "multi_step_compound_calculation" {
  // 计算: ((a + b) * c) - d
  let a = 8
  let b = 7
  let c = 5
  let d = 20
  
  // 步骤1: a + b = 15
  let sum_ab = @azimuth.add(a, b)
  // 步骤2: (a + b) * c = 15 * 5 = 75
  let product = @azimuth.multiply(sum_ab, c)
  // 步骤3: ((a + b) * c) - d = 75 - 20 = 55
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(55, result)
}

// 测试9: 错误处理和边界条件
test "error_handling_and_edge_cases" {
  // 测试断言函数本身
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试特殊值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值运算安全
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
}

// 测试10: 性能相关的计算
test "performance_related_calculations" {
  // 计算前n个自然数的和: 1 + 2 + 3 + ... + n = n * (n + 1) / 2
  let n = 100
  let sum_formula = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum_formula)
  
  // 计算平方和: 1² + 2² + 3² + ... + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.add(@azimuth.multiply(3, 3), @azimuth.multiply(4, 4))), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}