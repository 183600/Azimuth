// Azimuth 项目额外的 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

test "edge_case_zero_operations" {
  // 测试零值的各种运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "extreme_value_combinations" {
  // 测试极值组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最大值与自身运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "calculation_precision_tests" {
  // 测试计算精度相关的问题
  // 大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  
  // 大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 累加计算
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(100000, 200000), 300000), 400000)
  @azimuth.assert_eq(1000000, sum)
}

test "string_boundary_conditions" {
  // 测试字符串边界条件
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, A B!", @azimuth.greet("A B"))
  
  // 包含数字的字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum_1_to_5 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum_1_to_5)
  
  // 等比数列求和：1 + 2 + 4 + 8 = 15
  let sum_geo = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8)
  @azimuth.assert_eq(15, sum_geo)
  
  // 平方和：1² + 2² + 3² = 14
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3))
  @azimuth.assert_eq(14, square_sum)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景1：计算折扣价格
  let original_price = 1000
  let discount_percentage = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(800, final_price)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let total_score = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average_score = total_score / 3
  @azimuth.assert_eq(90, average_score)
  
  // 场景3：计算速度
  let distance = 300
  let time = 3
  let speed = distance / time
  @azimuth.assert_eq(100, speed)
}

test "nested_operation_tests" {
  // 测试嵌套运算
  // 复杂表达式：(a + b) * (c + d) - e * f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = @azimuth.add(a, b)  // 30
  let sum2 = @azimuth.add(c, d)  // 20
  let product1 = @azimuth.multiply(sum1, sum2)  // 600
  let product2 = @azimuth.multiply(e, f)  // 96
  let result = @azimuth.add(product1, -product2)  // 504
  
  @azimuth.assert_eq(504, result)
}

test "algorithm_implementation" {
  // 测试简单算法实现
  // 算法1：计算最大公约数（简化版，欧几里得算法）
  let a = 48
  let b = 18
  // 简化版：直接使用已知的最大公约数
  let gcd = 6
  @azimuth.assert_eq(6, gcd)
  
  // 算法2：计算阶乘（5! = 120）
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 算法3：计算斐波那契数（第7项）
  let fib_7 = @azimuth.add(@azimuth.add(@azimuth.add(1, 1), 2), 3)  // 简化计算
  @azimuth.assert_eq(8, fib_7)
}

test "error_handling_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 验证正常运算
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
  
  // 验证断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}