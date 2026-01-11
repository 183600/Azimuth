// 专门的综合测试用例

// 测试用例 1: 复合运算测试 - 多步计算
///|
test "compound_calculation_test" {
  // 复杂的多步计算: (a + b) * (c - d) + e * f
  // 使用现有的函数实现
  
  let a = 10
  let b = 20
  let c = 50
  let d = 30
  let e = 5
  let f = 8
  
  // (a + b)
  let sum_ab = @azimuth.add(a, b)
  
  // (c - d) = c + (-d)
  let diff_cd = @azimuth.add(c, @azimuth.multiply(-1, d))
  
  // e * f
  let product_ef = @azimuth.multiply(e, f)
  
  // (a + b) * (c - d)
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)
  
  // 最终结果: (a + b) * (c - d) + e * f
  let final_result = @azimuth.add(product_ab_cd, product_ef)
  
  // 验证: (10 + 20) * (50 - 30) + 5 * 8 = 30 * 20 + 40 = 600 + 40 = 640
  assert_eq(640, final_result)
}

// 测试用例 2: 极端边界值组合测试
///|
test "extreme_boundary_combination_test" {
  // 测试多个边界值的组合运算
  
  // 最大值与最小值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // (最大值 + 最小值) * 2
  let sum_max_min = @azimuth.add(max_val, min_val)
  let result1 = @azimuth.multiply(sum_max_min, 2)
  assert_eq(-2, result1)  // (-1) * 2 = -2
  
  // (最小值 + 1) * (最大值 - 1)
  let min_plus_1 = @azimuth.add(min_val, 1)
  let max_minus_1 = @azimuth.add(max_val, @azimuth.multiply(-1, 1))
  let result2 = @azimuth.multiply(min_plus_1, max_minus_1)
  assert_eq(4611686018427387904, result2)  // (-2147483647) * (2147483646) 会溢出到最小值
}

// 测试用例 3: 字符串处理的高级测试
///|
test "advanced_string_processing_test" {
  // 测试字符串的复杂处理和边界情况
  
  // 测试非常长的字符串
  let very_long_name = "ThisIsAVeryLongNameThatCouldBeUsedInRealWorldApplications"
  let long_greeting = @azimuth.greet(very_long_name)
  assert_eq(true, long_greeting.contains("ThisIsAVeryLongNameThatCouldBeUsedInRealWorldApplications"))
  
  // 测试包含数字和特殊字符的字符串
  let complex_name = "User123_!@#$%^&*()"
  let complex_greeting = @azimuth.greet(complex_name)
  assert_eq("Hello, User123_!@#$%^&*()!", complex_greeting)
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe Jr."
  let spaced_greeting = @azimuth.greet(spaced_name)
  assert_eq("Hello, John Doe Jr.!", spaced_greeting)
}

// 测试用例 4: 数学序列和模式测试
///|
test "mathematical_sequence_pattern_test" {
  // 测试斐波那契数列的计算
  // F(n) = F(n-1) + F(n-2)
  
  // 计算前几个斐波那契数
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  
  // 验证计算结果
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  
  // 测试斐波那契数的性质: F(n) * F(n+2) - F(n+1)² = (-1)^(n+1)
  // 对于 n=5: F(5) * F(7) - F(6)² = 5 * 13 - 8² = 65 - 64 = 1
  let fib_property = @azimuth.add(
    @azimuth.multiply(f5, f7),
    @azimuth.multiply(-1, @azimuth.multiply(f6, f6))
  )
  assert_eq(1, fib_property)
}

// 测试用例 5: 实际商业计算测试
///|
test "business_calculation_test" {
  // 模拟商业场景：计算折扣和税费
  
  let original_price = 1000  // 原价
  let discount_rate = 20     // 折扣率 20%
  let tax_rate = 8          // 税率 8%
  
  // 计算折扣金额: 原价 * 折扣率 / 100
  // 我们通过乘法来避免除法
  let discount_times_100 = @azimuth.multiply(original_price, discount_rate)
  
  // 计算折后价格: 原价 - 折扣金额
  // 折后价格 * 100 = 原价 * 100 - 折扣金额
  let discounted_price_times_100 = @azimuth.add(
    @azimuth.multiply(original_price, 100),
    @azimuth.multiply(-1, discount_times_100)
  )
  
  // 计算税费: 折后价格 * 税率 / 100
  // 税费 * 10000 = 折后价格 * 100 * 税率
  let tax_times_10000 = @azimuth.multiply(discounted_price_times_100, tax_rate)
  
  // 计算最终价格: 折后价格 + 税费
  // 最终价格 * 10000 = 折后价格 * 10000 + 税费 * 10000
  let final_price_times_10000 = @azimuth.add(
    @azimuth.multiply(discounted_price_times_100, 100),
    tax_times_10000
  )
  
  // 验证计算: (1000 - 200) * 1.08 = 800 * 1.08 = 864
  // 864 * 10000 = 8640000
  assert_eq(8640000, final_price_times_10000)
}

// 测试用例 6: 科学计算测试
///|
test "scientific_calculation_test" {
  // 模拟科学计算：动能公式 E = 1/2 * m * v²
  
  let mass = 10        // 质量 (kg)
  let velocity = 15    // 速度 (m/s)
  
  // v²
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  
  // m * v²
  let mass_times_velocity_squared = @azimuth.multiply(mass, velocity_squared)
  
  // E * 2 = m * v² (避免除法)
  let energy_times_2 = mass_times_velocity_squared
  
  // 验证: E = 1/2 * 10 * 15² = 1/2 * 10 * 225 = 1125
  // E * 2 = 2250
  assert_eq(2250, energy_times_2)
}

// 测试用例 7: 数据结构模拟测试
///|
test "data_structure_simulation_test" {
  // 模拟栈操作：push和pop
  
  // 初始栈为空
  let stack_empty = 0
  
  // push 5: 栈大小 = 0 + 1 = 1
  let stack_size_1 = @azimuth.add(stack_empty, 1)
  
  // push 10: 栈大小 = 1 + 1 = 2
  let stack_size_2 = @azimuth.add(stack_size_1, 1)
  
  // push 15: 栈大小 = 2 + 1 = 3
  let stack_size_3 = @azimuth.add(stack_size_2, 1)
  
  // pop: 栈大小 = 3 - 1 = 2
  let stack_size_after_pop = @azimuth.add(stack_size_3, @azimuth.multiply(-1, 1))
  
  // 验证栈操作
  assert_eq(1, stack_size_1)
  assert_eq(2, stack_size_2)
  assert_eq(3, stack_size_3)
  assert_eq(2, stack_size_after_pop)
  
  // 模拟栈中元素的总和
  // 假设栈中元素为 [5, 10, 15]
  let sum_elements = @azimuth.add(@azimuth.add(5, 10), 15)
  assert_eq(30, sum_elements)
}

// 测试用例 8: 图形算法测试
///|
test "graphics_algorithm_test" {
  // 模拟图形算法：计算两点之间的距离平方
  // 距离² = (x2 - x1)² + (y2 - y1)²
  
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y2 = 1
  
  // dx = x2 - x1 = x2 + (-x1)
  let dx = @azimuth.add(x2, @azimuth.multiply(-1, x1))
  
  // dy = y2 - y1 = y2 + (-y1)
  let dy = @azimuth.add(y2, @azimuth.multiply(-1, y1))
  
  // dx²
  let dx_squared = @azimuth.multiply(dx, dx)
  
  // dy²
  let dy_squared = @azimuth.multiply(dy, dy)
  
  // 距离² = dx² + dy²
  let distance_squared = @azimuth.add(dx_squared, dy_squared)
  
  // 验证: (7-3)² + (1-4)² = 4² + (-3)² = 16 + 9 = 25
  assert_eq(25, distance_squared)
}

// 测试用例 9: 统计分析扩展测试
///|
test "extended_statistics_test" {
  // 计算方差和标准差（简化版本）
  
  // 数据集: [2, 4, 6, 8, 10]
  let count = 5
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8), 10)
  
  // 计算平均值: 总和 / 数量
  // 平均值 * 数量 = 总和
  let average_times_count = sum
  
  // 计算每个数据点与平均值的差的平方和
  // 平均值 = 30 / 5 = 6
  let average = 6
  
  // (2-6)² + (4-6)² + (6-6)² + (8-6)² + (10-6)²
  // = (-4)² + (-2)² + 0² + 2² + 4²
  // = 16 + 4 + 0 + 4 + 16 = 40
  
  let diff1 = @azimuth.add(2, @azimuth.multiply(-1, average))
  let diff2 = @azimuth.add(4, @azimuth.multiply(-1, average))
  let diff3 = @azimuth.add(6, @azimuth.multiply(-1, average))
  let diff4 = @azimuth.add(8, @azimuth.multiply(-1, average))
  let diff5 = @azimuth.add(10, @azimuth.multiply(-1, average))
  
  let squared_diff_sum = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1),
    @azimuth.multiply(diff2, diff2)
  ), @azimuth.add(
    @azimuth.multiply(diff3, diff3),
    @azimuth.multiply(diff4, diff4)
  )), @azimuth.multiply(diff5, diff5))
  
  // 验证方差计算
  assert_eq(40, squared_diff_sum)
  
  // 方差 = 平方差和 / 数量 = 40 / 5 = 8
  // 方差 * 数量 = 平方差和
  assert_eq(40, @azimuth.multiply(8, count))
}

// 测试用例 10: 递归算法模拟测试
///|
test "recursive_algorithm_simulation_test" {
  // 模拟递归算法：阶乘计算
  // n! = n * (n-1) * (n-2) * ... * 1
  
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n = 5
  
  let factorial_1 = 1
  let factorial_2 = @azimuth.multiply(2, factorial_1)  // 2
  let factorial_3 = @azimuth.multiply(3, factorial_2)  // 6
  let factorial_4 = @azimuth.multiply(4, factorial_3)  // 24
  let factorial_5 = @azimuth.multiply(5, factorial_4)  // 120
  
  // 验证阶乘计算
  assert_eq(2, factorial_2)
  assert_eq(6, factorial_3)
  assert_eq(24, factorial_4)
  assert_eq(120, factorial_5)
  
  // 模拟递归性质：n! = n * (n-1)!
  // 验证: 5! = 5 * 4! = 5 * 24 = 120
  let recursive_check = @azimuth.multiply(5, factorial_4)
  assert_eq(120, recursive_check)
}