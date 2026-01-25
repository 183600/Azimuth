// 额外的综合测试用例 - 针对 azimuth 库的高级测试
// 包含实际应用场景、边界条件和复合运算测试

test "data_processing_aggregation" {
  // 数据处理聚合测试
  let data_points = [10, 15, 20, 25, 30]
  let sum = add(add(add(add(data_points[0], data_points[1]), data_points[2]), data_points[3]), data_points[4])
  let average = divide_with_ceil(sum, 5)
  let max_value = 30
  
  assert_eq(100, sum)
  assert_eq(20, average)
  assert_eq(30, max_value)
}

test "algorithm_binary_search_simulation" {
  // 模拟二分查找算法的索引计算
  let low = 0
  let high = 15
  let mid = divide_with_ceil(add(low, high), 2)
  
  assert_eq(8, mid)
  
  // 模拟下一步查找范围
  let new_low = add(mid, 1)
  let new_mid = divide_with_ceil(add(new_low, high), 2)
  
  assert_eq(12, new_mid)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  let max_positive = 2147483647
  let min_negative = -2147483648
  
  // 测试大数运算
  assert_eq(1, add(max_positive, -max_positive))
  assert_eq(-1, add(min_negative, max_positive))
  
  // 测试零的边界
  assert_eq(0, multiply(0, max_positive))
  assert_eq(0, multiply(min_negative, 0))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let project_requirements = [15, 25, 30, 20]
  
  // 计算总需求
  let total_required = add(add(add(project_requirements[0], project_requirements[1]), project_requirements[2]), project_requirements[3])
  
  // 计算剩余资源
  let remaining = add(total_resources, -total_required)
  
  // 计算可以支持的额外项目数（每个项目需要10个资源）
  let additional_projects = divide_with_ceil(remaining, 10)
  
  assert_eq(90, total_required)
  assert_eq(10, remaining)
  assert_eq(1, additional_projects)
}

test "error_robustness_validation" {
  // 错误处理健壮性验证
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极大数运算的稳定性
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "string_processing_validation" {
  // 字符串处理验证测试
  let username = "user123"
  let domain = "example.com"
  let email = username + "@" + domain
  let greeting = greet(email)
  
  assert_eq_string("Hello, user123@example.com!", greeting)
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  // 测试Unicode字符
  let unicode_text = "🚀 MoonBit 测试"
  assert_eq_string("Hello, 🚀 MoonBit 测试!", greet(unicode_text))
}

test "complex_mathematical_series" {
  // 复杂数学级数计算
  // 计算等差数列：3, 7, 11, 15, 19 的和
  let a1 = 3
  let d = 4
  let n = 5
  
  // 使用等差数列求和公式：S = n/2 * (2a1 + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(n, add(multiply(2, a1), multiply(n - 1, d))),
    2
  )
  
  // 验证结果
  assert_eq(55, sum_formula)
  
  // 计算等比数列：2, 4, 8, 16 的和
  let g1 = 2
  let r = 2
  let gn = 4
  
  // 等比数列求和：S = g1 * (r^n - 1) / (r - 1)
  let geometric_sum = multiply(g1, divide_with_ceil(multiply(r, gn) - 1, r - 1))
  assert_eq(30, geometric_sum)
}