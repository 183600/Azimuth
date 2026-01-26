// Azimuth 综合最终测试用例
// 高质量 MoonBit 测试语法，覆盖核心功能和边界情况

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let a = 15
  let b = 4
  let c = 7
  
  // 测试复合运算：(a + b) * c - a / b
  let result1 = add(multiply(add(a, b), c), -divide_with_ceil(a, b))
  assert_eq(129, result1)  // (15 + 4) * 7 - ceil(15/4) = 19 * 7 - 4 = 133 - 4 = 129
  
  // 测试嵌套运算：a * (b + c) + b * c
  let result2 = add(multiply(a, add(b, c)), multiply(b, c))
  assert_eq(193, result2)  // 15 * (4 + 7) + 4 * 7 = 15 * 11 + 28 = 165 + 28 = 193
}

test "mathematical_series_summation" {
  // 数学级数求和测试
  // 计算等差数列：3, 7, 11, 15, 19 的和
  let first_term = 3
  let common_diff = 4
  let num_terms = 5
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(55, series_sum)  // 5/2 * (6 + 16) = 5/2 * 22 = 55
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 8
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10800, year1)  // 10000 + 800
  assert_eq(11664, year2)  // 10800 + 864
  assert_eq(12597, year3)  // 11664 + 933
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(3500, allocated)
  assert_eq(1500, remaining)
  assert_eq(5, additional_projects)
}

test "logistics_packaging_efficiency" {
  // 物流包装效率测试
  
  // 计算每类物品需要的箱子数
  let boxes1 = divide_with_ceil(25, 6)  // 25个物品，每箱装6个
  let boxes2 = divide_with_ceil(47, 8)  // 47个物品，每箱装8个
  let boxes3 = divide_with_ceil(13, 3)  // 13个物品，每箱装3个
  let boxes4 = divide_with_ceil(31, 7)  // 31个物品，每箱装7个
  
  let total_boxes = add(add(add(boxes1, boxes2), boxes3), boxes4)
  
  assert_eq(5, boxes1)   // ceil(25/6) = 5
  assert_eq(6, boxes2)   // ceil(47/8) = 6
  assert_eq(5, boxes3)   // ceil(13/3) = 5
  assert_eq(5, boxes4)   // ceil(31/7) = 5
  assert_eq(21, total_boxes)
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  
  // 测试每种语言的问候
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, العالم!", greet("العالم"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let base_operations = 100
  let data_size = 50
  let algorithm_factor = 3
  
  // 模拟 O(n) 算法
  let linear_ops = multiply(base_operations, data_size)
  
  // 模拟 O(n²) 算法（简化版）
  let quadratic_ops = multiply(base_operations, multiply(data_size, divide_with_ceil(data_size, 10)))
  
  // 模拟 O(log n) 算法（简化版）
  let logarithmic_ops = multiply(base_operations, divide_with_ceil(data_size, algorithm_factor))
  
  assert_eq(5000, linear_ops)      // 100 * 50
  assert_eq(25000, quadratic_ops)  // 100 * 50 * 5
  assert_eq(1700, logarithmic_ops) // 100 * ceil(50/3) = 100 * 17
}

test "engineering_structural_calculation" {
  // 工程结构计算测试
  let beam_length = 12
  let material_density = 2400
  let safety_factor = 2
  
  // 计算梁的体积（简化计算）
  let volume = multiply(multiply(beam_length, 5), 8)  // 0.5->5, 0.8->8
  let volume_scaled = divide_with_ceil(volume, 100)
  
  // 计算重量
  let weight = multiply(volume_scaled, material_density)
  let weight_scaled = divide_with_ceil(weight, 1000)
  
  // 应用安全系数
  let safe_load = multiply(weight_scaled, safety_factor)
  
  assert_eq(48, volume_scaled)   // 12 * 5 * 8 / 100
  assert_eq(115, weight_scaled)  // ceil(48 * 2400 / 1000)
  assert_eq(230, safe_load)      // 115 * 2
}

test "data_analysis_statistics_comprehensive" {
  // 数据分析统计综合测试
  
  // 计算总和
  let sum = add(add(add(add(add(add(add(add(add(45, 67), 23), 89), 12), 56), 78), 34), 90), 41)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, 10)
  
  // 计算最大值和最小值的差
  let max_val = 90
  let min_val = 12
  let range = add(max_val, -min_val)
  
  // 计算方差（简化版）
  let variance = divide_with_ceil(multiply(range, range), 10)
  
  assert_eq(535, sum)
  assert_eq(54, average)
  assert_eq(78, range)
  assert_eq(609, variance)
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  let max_positive = 2147483647
  let min_negative = -2147483648
  
  // 测试接近边界的加法
  let near_max_sum = add(max_positive - 1000, 500)
  let near_min_sum = add(min_negative + 1000, -500)
  
  // 测试大数除法
  let large_division = divide_with_ceil(1000000, 3)
  let negative_large_division = divide_with_ceil(-1000000, 3)
  
  // 测试零值附近
  let zero_plus_small = add(0, 1)
  let zero_minus_small = add(0, -1)
  let zero_times_any = multiply(0, 12345)
  let zero_division = divide_with_ceil(0, 100)
  
  assert_eq(2147482647, near_max_sum)
  assert_eq(-2147482648, near_min_sum)
  assert_eq(333334, large_division)
  assert_eq(-333333, negative_large_division)
  assert_eq(1, zero_plus_small)
  assert_eq(-1, zero_minus_small)
  assert_eq(0, zero_times_any)
  assert_eq(0, zero_division)
}