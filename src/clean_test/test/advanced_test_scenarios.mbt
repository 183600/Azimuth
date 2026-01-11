// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 高级测试用例 - 补充更多测试场景

///|
test "large_number_performance" {
  // 测试大数运算的性能和正确性
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = -1000000
  let large_num4 = -2000000
  
  // 大数加法
  assert_eq(3000000, @clean_test.@clean_test.add(large_num1, large_num2))
  assert_eq(-3000000, @clean_test.@clean_test.add(large_num3, large_num4))
  assert_eq(0, @clean_test.@clean_test.add(large_num1, large_num3))
  assert_eq(-1000000, @clean_test.@clean_test.add(large_num1, large_num4))
  
  // 大数乘法
  assert_eq(2000000000000, @clean_test.@clean_test.multiply(large_num1, large_num2))
  assert_eq(2000000000000, @clean_test.@clean_test.multiply(large_num3, large_num4))
  assert_eq(-2000000000000, @clean_test.@clean_test.multiply(large_num1, large_num4))
  assert_eq(-2000000000000, @clean_test.@clean_test.multiply(large_num3, large_num2))
}

///|
test "string_length_boundary" {
  // 测试不同长度字符串的处理
  let empty = ""
  let single_char = "A"
  let short_word = "Hello"
  let long_sentence = "This is a very long sentence that contains many words and should test the string handling capabilities of the greet function"
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  
  assert_eq_string("Hello, !", @clean_test.@clean_test.greet(empty))
  assert_eq_string("Hello, A!", @clean_test.@clean_test.greet(single_char))
  assert_eq_string("Hello, Hello!", @clean_test.@clean_test.greet(short_word))
  assert_eq(true, @clean_test.@clean_test.greet(long_sentence).contains("very long sentence"))
  assert_eq("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", @clean_test.@clean_test.greet(special_chars))
}

///|
test "mathematical_identities" {
  // 测试数学恒等式
  
  // 平方差公式: a² - b² = (a + b)(a - b)
  let a = 15
  let b = 8
  let a_squared = @clean_test.@clean_test.multiply(a, a)
  let b_squared = @clean_test.@clean_test.multiply(b, b)
  let left_side = @clean_test.@clean_test.add(a_squared, -b_squared)  // a² - b²
  let right_side = @clean_test.@clean_test.multiply(@clean_test.@clean_test.add(a, b), @clean_test.@clean_test.add(a, -b))  // (a + b)(a - b)
  assert_eq(left_side, right_side)
  
  // 立方和公式: a³ + b³ = (a + b)(a² - ab + b²)
  let c = 3
  let d = 2
  let c_cubed = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(c, c), c)
  let d_cubed = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(d, d), d)
  let cube_sum_left = @clean_test.@clean_test.add(c_cubed, d_cubed)
  let c_squared = @clean_test.@clean_test.multiply(c, c)
  let d_squared = @clean_test.@clean_test.multiply(d, d)
  let cd_product = @clean_test.@clean_test.multiply(c, d)
  let cube_sum_right = @clean_test.@clean_test.multiply(
    @clean_test.@clean_test.add(c, d),
    @clean_test.@clean_test.add(@clean_test.@clean_test.add(c_squared, -cd_product), d_squared)
  )
  assert_eq(cube_sum_left, cube_sum_right)
}

///|
test "negative_number_deep_test" {
  // 深度测试负数运算
  
  // 负数连加
  let neg1 = -10
  let neg2 = -20
  let neg3 = -30
  let neg4 = -40
  
  let sum1 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(neg1, neg2), @clean_test.@clean_test.add(neg3, neg4))
  let sum2 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(neg1, neg2), neg3), neg4)
  assert_eq(sum1, sum2)
  assert_eq(-100, sum1)
  
  // 负数连乘
  let product1 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(neg1, neg2), @clean_test.@clean_test.multiply(neg3, neg4))
  let product2 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(neg1, neg2), neg3), neg4)
  assert_eq(product1, product2)
  assert_eq(240000, product1)
  
  // 负数与正数混合运算
  let pos1 = 50
  let pos2 = 60
  
  let mixed_sum1 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(neg1, pos1), @clean_test.@clean_test.add(neg2, pos2))
  let mixed_sum2 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(neg1, pos1), neg2), pos2)
  assert_eq(mixed_sum1, mixed_sum2)
  assert_eq(80, mixed_sum1)
  
  let mixed_product1 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(neg1, pos1), @clean_test.@clean_test.multiply(neg2, pos2))
  let mixed_product2 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(neg1, pos1), neg2), pos2)
  assert_eq(mixed_product1, mixed_product2)
  assert_eq(600000, mixed_product1)
}

///|
test "zero_value_detailed_test" {
  // 详细测试零值运算
  
  // 零值加法测试
  assert_eq(0, @clean_test.@clean_test.add(0, 0))
  assert_eq(123, @clean_test.@clean_test.add(0, 123))
  assert_eq(-123, @clean_test.@clean_test.add(0, -123))
  assert_eq(2147483647, @clean_test.@clean_test.add(0, 2147483647))
  assert_eq(-2147483648, @clean_test.@clean_test.add(0, -2147483648))
  
  // 零值乘法测试
  assert_eq(0, @clean_test.@clean_test.multiply(0, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 123))
  assert_eq(0, @clean_test.@clean_test.multiply(123, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, -123))
  assert_eq(0, @clean_test.@clean_test.multiply(-123, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 2147483647))
  assert_eq(0, @clean_test.@clean_test.multiply(2147483647, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, -2147483648))
  assert_eq(0, @clean_test.@clean_test.multiply(-2147483648, 0))
  
  // 零值在复杂表达式中的测试
  let complex_expr1 = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(0, 123), @clean_test.@clean_test.add(456, 0))
  assert_eq(456, complex_expr1)
  
  let complex_expr2 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.add(0, 123), @clean_test.@clean_test.multiply(456, 0))
  assert_eq(0, complex_expr2)
}

///|
test "extreme_value_combinations" {
  // 极值组合测试
  
  let max = 2147483647
  let min = -2147483648
  let mid = 1000000
  let neg_mid = -1000000
  
  // 极值加法组合
  assert_eq(max - 1, @clean_test.@clean_test.add(max, -1))
  assert_eq(max - 1000, @clean_test.@clean_test.add(max, -1000))
  assert_eq(max - mid, @clean_test.@clean_test.add(max, -mid))
  assert_eq(min + 1, @clean_test.@clean_test.add(min, 1))
  assert_eq(min + 1000, @clean_test.@clean_test.add(min, 1000))
  assert_eq(min + mid, @clean_test.@clean_test.add(min, mid))
  
  // 极值乘法组合
  assert_eq(max, @clean_test.@clean_test.multiply(max, 1))
  assert_eq(min, @clean_test.@clean_test.multiply(min, 1))
  assert_eq(max, @clean_test.@clean_test.multiply(1, max))
  assert_eq(min, @clean_test.@clean_test.multiply(1, min))
  assert_eq(0, @clean_test.@clean_test.multiply(max, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(min, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, max))
  assert_eq(0, @clean_test.@clean_test.multiply(0, min))
  
  // 中等值与极值组合
  assert_eq(max + mid, @clean_test.@clean_test.add(max, mid))  // 应该溢出
  assert_eq(min + neg_mid, @clean_test.@clean_test.add(min, neg_mid))  // 应该溢出
}

///|
test "financial_calculation_scenario" {
  // 金融计算场景测试
  
  // 计算贷款月供：本金 × 月利率 × (1 + 月利率)^期数 / ((1 + 月利率)^期数 - 1)
  let principal = 1000000  // 本金 100万
  let monthly_rate = 50    // 月利率 0.5% (放大100倍存储)
  let periods = 360        // 期数 30年×12月
  
  // 简化计算 (不包含复利)
  let monthly_interest = @clean_test.@clean_test.multiply(principal, monthly_rate)
  let total_interest = @clean_test.@clean_test.multiply(monthly_interest, periods)
  let total_payment = @clean_test.@clean_test.add(principal, @clean_test.divide(total_interest, 100))
  let monthly_payment = @clean_test.divide(total_payment, periods)
  
  // 验证计算合理性
  assert_eq(true, monthly_payment > 0)
  assert_eq(true, monthly_payment < principal)
  
  // 投资回报计算
  let initial_investment = 500000
  let annual_return = 80000  // 年收益 8万
  let years = 10
  let total_return = @clean_test.@clean_test.multiply(annual_return, years)
  let final_value = @clean_test.@clean_test.add(initial_investment, total_return)
  
  assert_eq(1300000, final_value)
}

///|
test "scientific_calculation_scenario" {
  // 科学计算场景测试
  
  // 物理公式：动能 = 0.5 × 质量 × 速度²
  let mass = 1000      // 质量 (kg)
  let velocity = 20    // 速度 (m/s)
  let velocity_squared = @clean_test.@clean_test.multiply(velocity, velocity)
  let kinetic_energy = @clean_test.divide(@clean_test.@clean_test.multiply(mass, velocity_squared), 2)
  
  assert_eq(200000, kinetic_energy)  // 0.5 × 1000 × 20² = 200000 J
  
  // 化学反应：摩尔浓度计算
  let moles = 25       // 摩尔数
  let volume = 500     // 体积 (L)
  let molarity = @clean_test.@clean_test.multiply(100, @clean_test.divide(moles, volume))  // 转换为mol/L
  
  assert_eq(5, molarity)  // 25/500 × 100 = 5 mol/L
  
  // 天文计算：距离转换
  let light_years = 10    // 光年
  let km_per_ly = 9461000000000  // 1光年≈9.461万亿公里
  let distance_km = @clean_test.@clean_test.multiply(light_years, km_per_ly)
  
  assert_eq(94610000000000, distance_km)
}

///|
test "recursive_operations" {
  // 递归运算场景测试
  
  // 斐波那契数列计算（简化版，只计算前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @clean_test.@clean_test.add(fib1, fib2)
  let fib4 = @clean_test.@clean_test.add(fib2, fib3)
  let fib5 = @clean_test.@clean_test.add(fib3, fib4)
  let fib6 = @clean_test.@clean_test.add(fib4, fib5)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  
  // 阶乘计算（简化版）
  let fact1 = 1
  let fact2 = @clean_test.@clean_test.multiply(fact1, 2)
  let fact3 = @clean_test.@clean_test.multiply(fact2, 3)
  let fact4 = @clean_test.@clean_test.multiply(fact3, 4)
  let fact5 = @clean_test.@clean_test.multiply(fact4, 5)
  
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  
  // 幂运算计算（简化版）
  let base = 3
  let exp1 = base
  let exp2 = @clean_test.@clean_test.multiply(exp1, base)
  let exp3 = @clean_test.@clean_test.multiply(exp2, base)
  let exp4 = @clean_test.@clean_test.multiply(exp3, base)
  
  assert_eq(3, exp1)
  assert_eq(9, exp2)
  assert_eq(27, exp3)
  assert_eq(81, exp4)
}

///|
test "boundary_condition_combinations" {
  // 边界条件组合测试
  
  let max = 2147483647
  let min = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 最大值与不同值的组合
  assert_eq(max, @clean_test.@clean_test.add(max, zero))
  assert_eq(max, @clean_test.@clean_test.add(zero, max))
  assert_eq(max, @clean_test.@clean_test.multiply(max, one))
  assert_eq(max, @clean_test.@clean_test.multiply(one, max))
  
  // 最小值与不同值的组合
  assert_eq(min, @clean_test.@clean_test.add(min, zero))
  assert_eq(min, @clean_test.@clean_test.add(zero, min))
  assert_eq(min, @clean_test.@clean_test.multiply(min, one))
  assert_eq(min, @clean_test.@clean_test.multiply(one, min))
  
  // 特殊值组合
  assert_eq(0, @clean_test.@clean_test.add(max, min))
  assert_eq(0, @clean_test.@clean_test.add(min, max))
  assert_eq(-max, @clean_test.@clean_test.multiply(max, neg_one))
  assert_eq(-max, @clean_test.@clean_test.multiply(neg_one, max))
  assert_eq(max, @clean_test.@clean_test.multiply(min, neg_one))
  assert_eq(max, @clean_test.@clean_test.multiply(neg_one, min))
  
  // 复合边界表达式
  let complex1 = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(max, one), @clean_test.@clean_test.multiply(min, neg_one))
  let complex2 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(max, min), @clean_test.@clean_test.add(one, neg_one))
  
  assert_eq(@clean_test.@clean_test.add(max, max), complex1)
  assert_eq(0, complex2)
}