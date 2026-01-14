// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 测试用例1: 加法结合律验证
test "addition_associativity_law" {
  // 验证加法结合律: (a + b) + c = a + (b + c)
  let a = 123
  let b = 456
  let c = 789
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 测试包含负数的情况
  let d = -100
  let e = 200
  let f = -150
  
  let left_assoc_neg = @azimuth.add(@azimuth.add(d, e), f)
  let right_assoc_neg = @azimuth.add(d, @azimuth.add(e, f))
  
  @azimuth.assert_eq(left_assoc_neg, right_assoc_neg)
}

// 测试用例2: 乘法交换律验证
test "multiplication_commutativity_law" {
  // 验证乘法交换律: a * b = b * a
  let a = 24
  let b = 36
  
  let product_ab = @azimuth.multiply(a, b)
  let product_ba = @azimuth.multiply(b, a)
  
  @azimuth.assert_eq(product_ab, product_ba)
  
  // 测试包含零的情况
  let c = 0
  let d = 12345
  
  let product_zero1 = @azimuth.multiply(c, d)
  let product_zero2 = @azimuth.multiply(d, c)
  
  @azimuth.assert_eq(product_zero1, product_zero2)
  @azimuth.assert_eq(0, product_zero1)
}

// 测试用例3: 字符串处理多样性测试
test "string_processing_diversity" {
  // 测试各种字符串处理情况
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊符号的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试Unicode字符
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  
  // 测试混合语言
  @azimuth.assert_eq_string("Hello, MoonBit测试!", @azimuth.greet("MoonBit测试"))
}

// 测试用例4: 数学运算性质验证
test "mathematical_operations_properties" {
  // 验证分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 99999))
  @azimuth.assert_eq(99999, @azimuth.add(0, 99999))
  
  // 验证单位元素性质
  @azimuth.assert_eq(99999, @azimuth.multiply(1, 99999))
  @azimuth.assert_eq(99999, @azimuth.multiply(99999, 1))
}

// 测试用例5: 边界值综合测试
test "boundary_value_comprehensive" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值与普通值的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试边界值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
}

// 测试用例6: 实际应用场景测试
test "real_world_scenarios" {
  // 场景1: 计算购物车总价
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let total = @azimuth.add(subtotal1, subtotal2)
  
  @azimuth.assert_eq(1195, total)  // 299*2 + 199*3 = 598 + 597 = 1195
  
  // 场景2: 计算矩形周长和面积
  let length = 15
  let width = 8
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(46, perimeter)  // 2*(15+8) = 46
  @azimuth.assert_eq(120, area)      // 15*8 = 120
}

// 测试用例7: 错误处理和边界条件
test "error_handling_and_edge_cases" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试极值运算的安全性
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试特殊值的处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
}

// 测试用例8: 复杂数学计算场景
test "complex_mathematical_calculations" {
  // 计算: (a + b) * (c + d) - e * f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum_ab = @azimuth.add(a, b)  // 30
  let sum_cd = @azimuth.add(c, d)  // 20
  let product_sums = @azimuth.multiply(sum_ab, sum_cd)  // 600
  let product_ef = @azimuth.multiply(e, f)  // 96
  let result = @azimuth.add(product_sums, -product_ef)  // 504
  
  @azimuth.assert_eq(504, result)
  
  // 计算平方和: a² + b²
  let x = 7
  let y = 24
  
  let x_squared = @azimuth.multiply(x, x)  // 49
  let y_squared = @azimuth.multiply(y, y)  // 576
  let sum_of_squares = @azimuth.add(x_squared, y_squared)  // 625
  
  @azimuth.assert_eq(625, sum_of_squares)
}

// 测试用例9: 性能相关计算测试
test "performance_related_calculations" {
  // 模拟大量数据的累加计算
  // 使用公式: 1 + 2 + ... + n = n * (n + 1) / 2
  let n = 1000
  let expected_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  @azimuth.assert_eq(500500, expected_sum)
  
  // 模拟等比数列求和: a * (r^n - 1) / (r - 1)
  // 简化计算: 1 + 2 + 4 + 8 + 16 = 31
  let a = 1
  let r = 2
  let terms = 5
  
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))  // 2^4 = 16
  let geometric_sum = @azimuth.add(a, power_of_2)  // 简化为 1 + 16 = 17
  
  // 手动验证前几项
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, manual_sum)
}

// 测试用例10: 数据类型转换和格式化
test "data_type_conversion_and_formatting" {
  // 模拟各种数据类型的字符串表示
  // 模拟整数到字符串的转换
  let year = 2024
  let year_str = "2024"
  
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet(year_str))
  
  // 模拟布尔值的字符串表示
  let bool_true_str = "true"
  let bool_false_str = "false"
  
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(bool_true_str))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(bool_false_str))
  
  // 模拟浮点数的字符串表示（简化）
  let pi_str = "3.14159"
  let e_str = "2.71828"
  
  @azimuth.assert_eq_string("Hello, 3.14159!", @azimuth.greet(pi_str))
  @azimuth.assert_eq_string("Hello, 2.71828!", @azimuth.greet(e_str))
  
  // 模拟日期时间的字符串表示
  let date_time_str = "2024-01-15 14:30:00"
  
  @azimuth.assert_eq_string("Hello, 2024-01-15 14:30:00!", @azimuth.greet(date_time_str))
}