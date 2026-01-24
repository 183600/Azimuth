// 新的标准 MoonBit 测试用例 - 针对 azimuth 库
// 包含边界测试、错误处理和实际应用场景

test "extreme_boundary_addition" {
  // 测试加法的极端边界情况
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(-2147483648, 2147483649))
}

test "divide_with_ceil_comprehensive_negative" {
  // 全面测试负数向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(-3, divide_with_ceil(-9, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "mathematical_identities" {
  // 验证数学恒等式
  let x = 42
  let y = 17
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(y, add(y, 0))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(y, 1))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(y, 0))
}

test "complex_financial_calculation" {
  // 复杂财务计算场景
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 计算复利（简化版，使用向上取整）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  assert_eq(10500, year1_amount)
  assert_eq(11025, year2_amount)
  assert_eq(11576, year3_amount)
}

test "string_greeting_edge_cases" {
  // 字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long name that might cause issues!", greet("This is a very long name that might cause issues"))
  assert_eq_string("Hello, 🌟🌙⭐!", greet("🌟🌙⭐"))
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_resources = 100
  let project1_needs = 23
  let project2_needs = 31
  let project3_needs = 19
  
  let allocated1 = project1_needs
  let allocated2 = project2_needs
  let allocated3 = project3_needs
  let total_allocated = add(add(allocated1, allocated2), allocated3)
  let remaining = add(total_resources, -total_allocated)
  
  // 计算可以支持的小项目数量
  let small_projects = divide_with_ceil(remaining, 7)
  
  assert_eq(73, total_allocated)
  assert_eq(27, remaining)
  assert_eq(4, small_projects)
}

test "time_series_calculation" {
  // 时间序列计算
  let base_value = 100
  let growth_rate = 15
  let periods = 5
  
  // 计算增长序列
  let period1 = add(base_value, divide_with_ceil(multiply(base_value, growth_rate), 100))
  let period2 = add(period1, divide_with_ceil(multiply(period1, growth_rate), 100))
  let period3 = add(period2, divide_with_ceil(multiply(period2, growth_rate), 100))
  let period4 = add(period3, divide_with_ceil(multiply(period3, growth_rate), 100))
  let period5 = add(period4, divide_with_ceil(multiply(period4, growth_rate), 100))
  
  assert_eq(115, period1)
  assert_eq(133, period2)
  assert_eq(153, period3)
  assert_eq(176, period4)
  assert_eq(203, period5)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let n = 100
  let constant_factor = 3
  
  // 模拟 O(n) 算法
  let linear_result = multiply(n, constant_factor)
  
  // 模拟 O(n log n) 算法（简化版）
  let log_approx = divide_with_ceil(n, 10)  // 简化的对数近似
  let nlogn_result = multiply(n, log_approx)
  
  // 模拟 O(n²) 算法
  let quadratic_result = multiply(n, n)
  
  assert_eq(300, linear_result)
  assert_eq(1000, nlogn_result)
  assert_eq(10000, quadratic_result)
}

test "statistical_analysis_basic" {
  // 基础统计分析
  let data1 = 25
  let data2 = 30
  let data3 = 45
  let data4 = 40
  let data5 = 35
  
  // 计算总和
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  
  // 计算平均值（向上取整）
  let count = 5
  let average = divide_with_ceil(sum, count)
  
  // 计算方差（简化版）
  let diff1 = data1 - average
  let diff2 = data2 - average
  let diff3 = data3 - average
  let diff4 = data4 - average
  let diff5 = data5 - average
  
  let variance = divide_with_ceil(
    add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), 
    multiply(diff5, diff5)
  , count)
  
  assert_eq(175, sum)
  assert_eq(35, average)
  assert_eq(50, variance)
}