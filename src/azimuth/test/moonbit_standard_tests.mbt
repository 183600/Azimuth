// Azimuth 项目标准 MoonBit 测试用例
// 包含10个标准测试，覆盖基本功能和边界情况

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -54321
  let d = 9876
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  
  @azimuth.assert_eq(result3, result4)
}

test "multiplication_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 25
  let c = 35
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let x = -10
  let y = 20
  let z = -5
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, $%^&*()!", @azimuth.greet("$%^&*()"))
}

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 100
  
  // 使用公式计算
  let formula_result = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  // 手动计算前10项验证
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  @azimuth.assert_eq(55, manual_sum)
  
  // 验证公式结果
  @azimuth.assert_eq(5050, formula_result)
}

test "geometric_sequence_sum" {
  // 测试等比数列求和：a + ar + ar² + ... + ar^(n-1) = a × (r^n - 1) / (r - 1)
  let a = 2  // 首项
  let r = 3  // 公比
  let n = 4  // 项数
  
  // 手动计算：2 + 6 + 18 + 54 = 80
  let term1 = a
  let term2 = @azimuth.multiply(a, r)
  let term3 = @azimuth.multiply(term2, r)
  let term4 = @azimuth.multiply(term3, r)
  
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(80, manual_sum)
}

test "temperature_conversion_advanced" {
  // 测试高级温度转换：华氏度转摄氏度
  // 简化公式：C = (F - 30) / 2
  
  // 测试冰点温度
  let freezing_fahrenheit = 30
  let freezing_celsius = @azimuth.add(freezing_fahrenheit, -30) / 2
  @azimuth.assert_eq(0, freezing_celsius)
  
  // 测试室温
  let room_fahrenheit = 70
  let room_celsius = @azimuth.add(room_fahrenheit, -30) / 2
  @azimuth.assert_eq(20, room_celsius)
  
  // 测试沸点
  let boiling_fahrenheit = 230
  let boiling_celsius = @azimuth.add(boiling_fahrenheit, -30) / 2
  @azimuth.assert_eq(100, boiling_celsius)
}

test "discount_calculation_business" {
  // 测试商业折扣计算
  let original_price = 1000
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  @azimuth.assert_eq(150, discount_amount)
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(850, discounted_price)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  @azimuth.assert_eq(68, tax_amount)
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  @azimuth.assert_eq(918, final_price)
}

test "velocity_calculation_physics" {
  // �物理学速度计算：v = d / t
  let distance = 300  // 米
  let time = 60       // 秒
  
  // 计算速度
  let velocity = distance / time
  @azimuth.assert_eq(5, velocity)
  
  // 计算加速度：a = (v_final - v_initial) / t
  let initial_velocity = 10
  let final_velocity = 30
  let acceleration_time = 5
  
  let acceleration = @azimuth.add(final_velocity, -initial_velocity) / acceleration_time
  @azimuth.assert_eq(4, acceleration)
  
  // 计算位移：s = v_initial × t + 0.5 × a × t²
  let motion_time = 3
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, motion_time), @azimuth.multiply(acceleration, @azimuth.multiply(motion_time, motion_time)) / 2)
  @azimuth.assert_eq(48, displacement)
}

test "array_index_calculation_2d_to_1d" {
  // 测试二维数组到一维数组的索引转换
  let row = 3
  let col = 4
  let width = 5
  
  // 计算一维索引：index = row × width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(19, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = @azimuth.add(@azimuth.multiply(first_row, width), first_col)
  @azimuth.assert_eq(0, first_index)
  
  let last_row = 9
  let last_col = 4
  let last_index = @azimuth.add(@azimuth.multiply(last_row, width), last_col)
  @azimuth.assert_eq(49, last_index)
}

test "complex_number_simulation" {
  // 测试复数运算模拟（使用实部和虚部分别计算）
  // 复数加法：(a + bi) + (c + di) = (a + c) + (b + d)i
  let real1 = 5
  let imag1 = 3
  let real2 = 2
  let imag2 = 7
  
  let sum_real = @azimuth.add(real1, real2)
  let sum_imag = @azimuth.add(imag1, imag2)
  
  @azimuth.assert_eq(7, sum_real)  // 5 + 2 = 7
  @azimuth.assert_eq(10, sum_imag) // 3 + 7 = 10
  
  // 复数乘法（简化版）：(a + bi) × (c + di) = (ac - bd) + (ad + bc)i
  let product_real = @azimuth.add(@azimuth.multiply(real1, real2), @azimuth.multiply(-imag1, imag2))
  let product_imag = @azimuth.add(@azimuth.multiply(real1, imag2), @azimuth.multiply(imag1, real2))
  
  @azimuth.assert_eq(-11, product_real) // 5×2 - 3×7 = 10 - 21 = -11
  @azimuth.assert_eq(41, product_imag)  // 5×7 + 3×2 = 35 + 6 = 41
}