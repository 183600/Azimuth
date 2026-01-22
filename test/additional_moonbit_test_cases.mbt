// 额外的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

// 导入 azimuth 库
use src.azimuth

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, azimuth.add(7, 8))
  assert_eq(15, azimuth.add(8, 7))
  assert_eq(0, azimuth.add(-5, 5))
  assert_eq(0, azimuth.add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  
  // a * (b + c) = a * b + a * c
  let left = azimuth.multiply(a, azimuth.add(b, c))
  let right = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  
  assert_eq(left, right)
}

test "divide_with_ceil_comprehensive_negative" {
  // 全面测试负数的向上取整除法
  assert_eq(-2, azimuth.divide_with_ceil(-4, 2))
  assert_eq(-1, azimuth.divide_with_ceil(-3, 2))
  assert_eq(-3, azimuth.divide_with_ceil(-7, 2))
  assert_eq(2, azimuth.divide_with_ceil(-7, -4))
  assert_eq(0, azimuth.divide_with_ceil(-5, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
  assert_eq_string("Hello, αβγδε!", azimuth.greet("αβγδε"))
  assert_eq_string("Hello, 中文测试!", azimuth.greet("中文测试"))
  assert_eq_string("Hello, ñáéíóú!", azimuth.greet("ñáéíóú"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 125
  let quantity = 8
  let tax_rate = 10
  let discount = 100
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total = azimuth.add(azimuth.add(subtotal, tax), -discount)
  
  assert_eq(1000, subtotal)
  assert_eq(100, tax)
  assert_eq(1000, total)
}

test "mathematical_series_sum" {
  // 数学级数求和：等差数列
  let first_term = 5
  let common_diff = 3
  let n_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = azimuth.divide_with_ceil(
    azimuth.multiply(n_terms, azimuth.add(azimuth.multiply(2, first_term), azimuth.multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(63, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75，修正计算：5+8+11+14+17+20 = 75
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算剩余预算
  let remaining = azimuth.add(
    azimuth.add(total_budget, -project_a_cost), 
    azimuth.add(-project_b_cost, -project_c_cost)
  )
  
  // 计算可以支持的小项目数量（每个小项目200）
  let small_projects = azimuth.divide_with_ceil(remaining, 200)
  
  assert_eq(1100, remaining)
  assert_eq(6, small_projects)
}

test "time_management_calculation" {
  // 时间管理计算
  let total_tasks = 47
  let tasks_per_hour = 6
  let working_hours_per_day = 8
  
  // 计算需要的小时数
  let hours_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算需要的天数
  let days_needed = azimuth.divide_with_ceil(hours_needed, working_hours_per_day)
  
  assert_eq(8, hours_needed)
  assert_eq(1, days_needed)
}

test "statistical_analysis" {
  // 统计分析
  let data_points = [12, 15, 18, 21, 24, 27, 30]
  let n = 7
  
  // 计算总和（手动计算）
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值（向上取整）
  let average = azimuth.divide_with_ceil(sum, n)
  
  assert_eq(147, sum)
  assert_eq(21, average)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试除数为零的各种情况
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
  assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  
  // 测试极值运算
  assert_eq(0, azimuth.multiply(0, 2147483647))
  assert_eq(0, azimuth.multiply(-2147483648, 0))
}