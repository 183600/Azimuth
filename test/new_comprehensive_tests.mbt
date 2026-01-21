// 新增 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "cascading_overflow_protection" {
  // 测试级联溢出保护
  let max_val = 2147483647
  
  // 多次加法不应该导致溢出
  let result1 = add(add(add(max_val, 100), 200), 300)
  assert_eq(max_val, result1)
  
  // 混合运算的溢出保护
  let result2 = add(multiply(1000, 1000), max_val)
  assert_eq(max_val, result2)
}

test "negative_number_edge_cases" {
  // 测试负数的边界情况
  let min_val = -2147483648
  
  // 负数乘法测试
  assert_eq(2147483647, multiply(multiply(-1, -1), multiply(-1, -1)))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
  
  // 负数加法测试
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(min_val, add(min_val, -100))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", greet(special_chars))
  
  // 测试包含空格和制表符的字符串
  let whitespace_chars = "  \t\n\r  "
  assert_eq_string("Hello,  \t\n\r  !", greet(whitespace_chars))
  
  // 测试非常长的单字符重复字符串
  let repeated_char = "AAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  assert_eq_string("Hello, AAAAAAAAAAAAAAAAAAAAAAAAAAAA!", greet(repeated_char))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算斐波那契数列前几项的和：1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  
  let fib_sum = add(add(add(add(add(fib1, fib2), fib3), fib4), fib5), fib6)
  assert_eq(20, fib_sum)
  
  // 计算等比数列：2, 4, 8, 16, 32
  let geo_sum = add(add(add(add(2, 4), 8), 16), 32)
  assert_eq(62, geo_sum)
}

test "financial_calculation_advanced" {
  // 高级金融计算场景
  let principal = 10000
  let rate1 = 5  // 第一年利率
  let rate2 = 6  // 第二年利率
  let rate3 = 7  // 第三年利率
  
  // 计算复利：每年不同利率
  let year1 = multiply(principal, add(100, rate1)) / 100
  let year2 = multiply(year1, add(100, rate2)) / 100
  let year3 = multiply(year2, add(100, rate3)) / 100
  
  assert_eq(11910, year3)  // 10000 * 1.05 * 1.06 * 1.07 ≈ 11910
  
  // 计算简单利息总和作为对比
  let simple_interest = multiply(principal, add(add(rate1, rate2), rate3)) / 100
  let simple_total = add(principal, simple_interest)
  
  assert_eq(11800, simple_total)  // 10000 + 10000 * (5+6+7)/100 = 11800
}

test "physics_simulation_calculations" {
  // 物理模拟计算
  // 自由落体运动：h = v0*t + 0.5*g*t^2
  // 简化为整数计算：h = v0*t + g*t*t/2
  let initial_velocity = 10
  let gravity = 10  // 简化的重力加速度
  let time = 3
  
  let height = add(multiply(initial_velocity, time), multiply(gravity, multiply(time, time)) / 2)
  assert_eq(75, height)  // 10*3 + 10*3*3/2 = 30 + 45 = 75
  
  // 动能计算：E = 0.5 * m * v^2
  let mass = 100
  let velocity = 20
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  assert_eq(20000, kinetic_energy)  // 100 * 20 * 20 / 2 = 20000
}

test "combinatorial_calculations" {
  // 组合数学计算
  // 计算排列数 P(n,k) = n * (n-1) * ... * (n-k+1)
  let n = 6
  let k = 3
  
  // P(6,3) = 6 * 5 * 4 = 120
  let permutation = multiply(multiply(n, n - 1), n - 2)
  assert_eq(120, permutation)
  
  // 计算组合数 C(n,k) = P(n,k) / k!
  // C(6,3) = P(6,3) / 3! = 120 / 6 = 20
  let factorial_k = multiply(multiply(3, 2), 1)
  let combination = permutation / factorial_k
  assert_eq(20, combination)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  // O(n^2) 算法的计算量
  let n = 100
  let quadratic_ops = multiply(n, n)
  assert_eq(10000, quadratic_ops)
  
  // O(n log n) 算法的计算量（简化为 n * log2(n)）
  // log2(100) ≈ 6.64，简化为 7
  let log_approx = 7
  let nlogn_ops = multiply(n, log_approx)
  assert_eq(700, nlogn_ops)
  
  // O(2^n) 算法的计算量（小规模）
  let small_n = 8
  let exponential_ops = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(2, 2))
  assert_eq(256, exponential_ops)
}

test "error_boundary_stress_test" {
  // 错误边界压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续的边界值运算
  let stress_test_1 = add(multiply(max_val, max_val), add(max_val, min_val))
  assert_eq(max_val, stress_test_1)
  
  let stress_test_2 = multiply(add(min_val, max_val), multiply(min_val, min_val))
  assert_eq(0, stress_test_2)
  
  // 嵌套的边界值运算
  let stress_test_3 = add(multiply(add(max_val, -1), add(min_val, 1)), multiply(2, 2))
  assert_eq(6, stress_test_3)
  
  // 极端情况下的断言测试
  assert_true(add(max_val, min_val) == -1)
  assert_false(add(max_val, min_val) == 0)
  assert_eq_string("Hello, Extreme!", greet("Extreme"))
}