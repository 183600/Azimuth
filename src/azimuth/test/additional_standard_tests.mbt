// 新增的标准 MoonBit 测试用例
// 包含10个全面的测试用例，覆盖主要功能和新的测试场景

test "sequential_addition_operations" {
  // 测试连续加法运算
  let result1 = @azimuth.add(@azimuth.add(5, 10), @azimuth.add(15, 20))
  @azimuth.assert_eq(50, result1)
  
  let result2 = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, result2)
  
  // 测试包含负数的连续加法
  let result3 = @azimuth.add(@azimuth.add(100, -50), @azimuth.add(-30, 80))
  @azimuth.assert_eq(100, result3)
}

test "sequential_multiplication_operations" {
  // 测试连续乘法运算
  let result1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4)
  @azimuth.assert_eq(24, result2)
  
  // 测试包含负数的连续乘法
  let result3 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result3)
}

test "mixed_arithmetic_operations" {
  // 测试混合运算（加减乘）
  let a = 10
  let b = 5
  let c = 3
  
  // 计算：(a + b) * c - a
  let result1 = @azimuth.add(@azimuth.multiply(@azimuth.add(a, b), c), -a)
  @azimuth.assert_eq(35, result1)  // (10 + 5) * 3 - 10 = 15 * 3 - 10 = 45 - 10 = 35
  
  // 计算：a * b + a * c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(80, result2)  // 10 * 5 + 10 * 3 = 50 + 30 = 80
  
  // 计算：(a - b) * (a + b)
  let result3 = @azimuth.multiply(@azimuth.add(a, -b), @azimuth.add(a, b))
  @azimuth.assert_eq(75, result3)  // (10 - 5) * (10 + 5) = 5 * 15 = 75
}

test "string_processing_edge_cases" {
  // 测试字符串处理的各种边界情况
  // 测试长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  let result1 = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalExpectations!", result1)
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result2)
  
  // 测试包含数字和字母的混合字符串
  let mixed = "User123ABC456"
  let result3 = @azimuth.greet(mixed)
  @azimuth.assert_eq_string("Hello, User123ABC456!", result3)
}

test "mathematical_function_combinations" {
  // 测试数学函数的组合使用
  // 计算平方和：a² + b²
  let a = 3
  let b = 4
  let sum_of_squares = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(b, b))
  @azimuth.assert_eq(25, sum_of_squares)  // 3² + 4² = 9 + 16 = 25
  
  // 计算立方差：a³ - b³ = (a - b) * (a² + ab + b²)
  let a2 = 5
  let b2 = 2
  let a_squared = @azimuth.multiply(a2, a2)
  let b_squared = @azimuth.multiply(b2, b2)
  let a_cubed = @azimuth.multiply(a_squared, a2)
  let b_cubed = @azimuth.multiply(b_squared, b2)
  let difference_of_cubes = @azimuth.add(a_cubed, -b_cubed)
  @azimuth.assert_eq(117, difference_of_cubes)  // 5³ - 2³ = 125 - 8 = 117
  
  // 计算平均值：(a + b + c) / 3
  let c = 7
  let sum = @azimuth.add(@azimuth.add(a, b), c)
  let average = sum / 3
  @azimuth.assert_eq(4, average)  // (3 + 4 + 7) / 3 = 14 / 3 = 4 (整数除法)
}

test "currency_conversion_scenario" {
  // 测试实际应用场景：汇率转换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 添加手续费 3%
  let fee = @azimuth.multiply(cny_amount, 3) / 100
  let total_with_fee = @azimuth.add(cny_amount, fee)
  @azimuth.assert_eq(721, total_with_fee)  // 700 + 700*3/100 = 700 + 21 = 721
  
  // 反向转换验证
  let reversed_usd = total_with_fee / exchange_rate
  @azimuth.assert_eq(103, reversed_usd)  // 721 / 7 = 103 (整数除法)
}

test "geometric_calculations" {
  // 测试实际应用场景：几何计算
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 10
  let height = 6
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(45, trapezoid_area)  // (5 + 10) * 6 / 2 = 15 * 6 / 2 = 90 / 2 = 45
  
  // 计算立方体体积：边长³
  let edge_length = 4
  let cube_volume = @azimuth.multiply(@azimuth.multiply(edge_length, edge_length), edge_length)
  @azimuth.assert_eq(64, cube_volume)  // 4³ = 64
  
  // 计算圆柱体侧面积：2 × π × r × h (简化 π = 3)
  let radius = 3
  let cylinder_height = 8
  let lateral_surface_area = @azimuth.multiply(2, @azimuth.multiply(3, @azimuth.multiply(radius, cylinder_height)))
  @azimuth.assert_eq(144, lateral_surface_area)  // 2 * 3 * 3 * 8 = 144
}

test "extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  let result1 = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result1)
  
  let result2 = @azimuth.multiply(max_val, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.multiply(min_val, 0)
  @azimuth.assert_eq(0, result3)
  
  // 边界值附近的运算
  let result4 = @azimuth.add(max_val, -max_val)
  @azimuth.assert_eq(0, result4)
  
  let result5 = @azimuth.add(min_val, -min_val)
  @azimuth.assert_eq(0, result5)
  
  // 特殊边界值运算
  let result6 = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, result6)
  
  let result7 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result7)  // 特殊处理：最小值乘以-1仍为最小值
}

test "error_handling_boundaries" {
  // 测试错误处理的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, underflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  let underflow_mul = @azimuth.multiply(min_val, min_val)
  @azimuth.assert_eq(max_val, underflow_mul)  // 负负得正，但会溢出
  
  // 测试断言函数的边界情况
  @azimuth.assert_true(max_val > 0)
  @azimuth.assert_true(min_val < 0)
  @azimuth.assert_false(max_val < 0)
  @azimuth.assert_false(min_val > 0)
}

test "performance_related_calculations" {
  // 测试性能相关的计算
  // 计算斐波那契数列的简化版本（前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  @azimuth.assert_eq(13, fib7)
  @azimuth.assert_eq(21, fib8)
  
  // 计算阶乘的简化版本（5!）
  let n = 5
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial)
  
  // 计算平方和序列：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.multiply(1, 1), @azimuth.add(@azimuth.multiply(2, 2), @azimuth.add(@azimuth.multiply(3, 3), @azimuth.add(@azimuth.multiply(4, 4), @azimuth.multiply(5, 5)))))
  @azimuth.assert_eq(55, square_sum)
}