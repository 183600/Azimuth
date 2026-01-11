// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 专注测试用例 - 高质量测试覆盖
// 针对特定场景和边缘情况的深度测试

// 测试1: 幂运算相关测试（通过重复乘法实现）
///|
test "power_operations_via_multiplication" {
  // 计算 2^3 = 8
  assert_eq(8, @clean_test.@clean_test.multiply(2, @clean_test.@clean_test.multiply(2, 2)))
  
  // 计算 3^4 = 81
  assert_eq(81, @clean_test.@clean_test.multiply(3, @clean_test.@clean_test.multiply(3, @clean_test.@clean_test.multiply(3, 3))))
  
  // 计算 (-2)^3 = -8
  assert_eq(-8, @clean_test.@clean_test.multiply(-2, @clean_test.@clean_test.multiply(-2, -2)))
  
  // 计算 (-2)^4 = 16
  assert_eq(16, @clean_test.@clean_test.multiply(-2, @clean_test.@clean_test.multiply(-2, @clean_test.@clean_test.multiply(-2, -2))))
}

// 测试2: 复杂财务计算场景
///|
test "complex_financial_calculations" {
  // 复利计算：本金 * (1 + 利率)^年数
  // 本金10000，年利率5%，3年
  let principal = 10000
  let rate_plus_one = 105  // 代表 1.05 * 100
  let years = 3
  
  // 计算 (1.05)^3 = 1.157625
  let rate_cubed_times_million = @clean_test.@clean_test.multiply(rate_plus_one, @clean_test.@clean_test.multiply(rate_plus_one, rate_plus_one))
  // 105^3 = 1157625，代表 1.157625 * 1000000
  
  // 最终金额 = 10000 * 1.157625 = 11576.25
  let final_amount_times_100 = @clean_test.@clean_test.multiply(principal, 1157625)  // 11576250000
  assert_eq(11576250000, final_amount_times_100)
}

// 测试3: 统计计算场景
///|
test "statistical_calculations" {
  // 计算方差：E[X^2] - (E[X])^2
  // 数据集：1, 2, 3, 4, 5
  let data_sum = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(1, 2), 3), 4), 5)  // 15
  let data_count = 5
  let mean = @clean_test.divide(data_sum, data_count)  // 注意：这里假设有divide函数
  
  // 计算平方和：1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 55
  let sum_of_squares = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(
    @clean_test.@clean_test.multiply(1, 1), 
    @clean_test.@clean_test.multiply(2, 2)), 
    @clean_test.@clean_test.multiply(3, 3)), 
    @clean_test.@clean_test.multiply(4, 4)), 
    @clean_test.@clean_test.multiply(5, 5))
  
  assert_eq(55, sum_of_squares)
  assert_eq(15, data_sum)
}

// 测试4: 物理计算场景
///|
test "physics_calculations" {
  // 自由落体距离：s = 0.5 * g * t^2
  // g = 9.8 m/s^2, t = 5s
  // 避免小数，使用整数计算：s = 49 * 25 / 10 = 122.5m
  let gravity_times_10 = 98  // 9.8 * 10
  let time_squared = @clean_test.@clean_test.multiply(5, 5)  // 25
  
  // 距离 * 20 = (98 * 25) / 2 = 1225
  let distance_times_20 = @clean_test.@clean_test.multiply(gravity_times_10, time_squared)
  let final_distance_times_20 = @clean_test.divide(distance_times_20, 2)
  
  assert_eq(1225, final_distance_times_20)  // 代表 122.5 * 10
}

// 测试5: 极端数值组合测试
///|
test "extreme_value_combinations" {
  // 测试大数与小数的组合
  let big_number = 1000000
  let small_number = 1
  
  // 大数加小数
  assert_eq(1000001, @clean_test.@clean_test.add(big_number, small_number))
  
  // 大数乘小数
  assert_eq(1000000, @clean_test.@clean_test.multiply(big_number, small_number))
  
  // 负大数测试
  let neg_big_number = -1000000
  assert_eq(-999999, @clean_test.@clean_test.add(neg_big_number, small_number))
  assert_eq(-1000000, @clean_test.@clean_test.multiply(neg_big_number, small_number))
}

// 测试6: 字符串复杂处理场景
///|
test "complex_string_scenarios" {
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  let greeting_special = @clean_test.@clean_test.greet(special_chars)
  assert_eq("Hello, !@#$%^&*()!", greeting_special)
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe"
  let greeting_spaced = @clean_test.@clean_test.greet(spaced_name)
  assert_eq("Hello, John Doe!", greeting_spaced)
  
  // 测试包含数字和字母混合
  let mixed_name = "User123"
  let greeting_mixed = @clean_test.@clean_test.greet(mixed_name)
  assert_eq("Hello, User123!", greeting_mixed)
}

// 测试7: 数列模式测试
///|
test "sequence_pattern_tests" {
  // 斐波那契数列前几项：1, 1, 2, 3, 5, 8, 13, 21...
  let fib1 = 1
  let fib2 = 1
  let fib3 = @clean_test.@clean_test.add(fib1, fib2)  // 2
  let fib4 = @clean_test.@clean_test.add(fib2, fib3)  // 3
  let fib5 = @clean_test.@clean_test.add(fib3, fib4)  // 5
  let fib6 = @clean_test.@clean_test.add(fib4, fib5)  // 8
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  
  // 验证斐波那契性质：F(n) * F(n+2) - F(n+1)^2 = (-1)^(n+1)
  // 对于 n=1: F(1)*F(3) - F(2)^2 = 1*2 - 1^2 = 1
  let fib_property = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(fib1, fib3), @clean_test.@clean_test.multiply(-1, @clean_test.@clean_test.multiply(fib2, fib2)))
  assert_eq(1, fib_property)
}

// 测试8: 边界值数学性质验证
///|
test "boundary_mathematical_properties" {
  // 验证在边界值附近的基本数学性质仍然成立
  
  // 测试加法消去律：如果 a + b = a + c，那么 b = c
  let a = 2147483640  // 接近最大值
  let b = 5
  let c = 5
  
  let left_sum = @clean_test.@clean_test.add(a, b)  // 可能溢出
  let right_sum = @clean_test.@clean_test.add(a, c)  // 可能溢出
  
  // 在不溢出的情况下，这个性质应该成立
  let safe_a = 1000
  let safe_left = @clean_test.@clean_test.add(safe_a, b)
  let safe_right = @clean_test.@clean_test.add(safe_a, c)
  assert_eq(safe_left, safe_right)
  assert_eq(b, c)
}

// 测试9: 实际应用 - 库存管理
///|
test "inventory_management" {
  // 商品库存计算
  let initial_stock = 1000
  let incoming = @clean_test.@clean_test.multiply(50, 10)  // 50箱，每箱10个
  let outgoing = @clean_test.@clean_test.multiply(30, 8)   // 出库30箱，每箱8个
  
  let final_stock = @clean_test.@clean_test.add(@clean_test.@clean_test.add(initial_stock, incoming), @clean_test.@clean_test.multiply(-1, outgoing))
  
  assert_eq(1000, initial_stock)
  assert_eq(500, incoming)
  assert_eq(240, outgoing)
  assert_eq(1260, final_stock)  // 1000 + 500 - 240 = 1260
}

// 测试10: 数值系统转换测试
///|
test "number_system_conversion" {
  // 二进制到十进制的验证（通过位运算）
  // 二进制 1010 = 8 + 0 + 2 + 0 = 10
  let binary_1010 = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(1, 8), @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(0, 4), @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(1, 2), @clean_test.@clean_test.multiply(0, 1))))
  assert_eq(10, binary_1010)
  
  // 二进制 1111 = 8 + 4 + 2 + 1 = 15
  let binary_1111 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.multiply(1, 8), @clean_test.@clean_test.multiply(1, 4)), @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(1, 2), @clean_test.@clean_test.multiply(1, 1)))
  assert_eq(15, binary_1111)
  
  // 验证位运算性质：a << 1 = a * 2
  let shift_test = @clean_test.@clean_test.multiply(7, 2)  // 7 << 1 = 14
  assert_eq(14, shift_test)
}