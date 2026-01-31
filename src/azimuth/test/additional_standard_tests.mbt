// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能提供补充测试覆盖

test "subtract_function_comprehensive" {
  // 全面测试 subtract 函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(10, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(15, subtract(-5, -20))
  assert_eq(-25, subtract(-10, 15))
}

test "complex_financial_calculations" {
  // 复杂金融计算场景
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  let years = 2
  
  // 计算复利：第一年
  let interest1 = divide_with_ceil(multiply(principal, rate1), 100)
  let amount1 = add(principal, interest1)
  
  // 计算复利：第二年（利率变化）
  let interest2 = divide_with_ceil(multiply(amount1, rate2), 100)
  let final_amount = add(amount1, interest2)
  
  assert_eq(500, interest1)
  assert_eq(10500, amount1)
  assert_eq(315, interest2)
  assert_eq(10815, final_amount)
}

test "inventory_optimization" {
  // 库存优化计算
  let current_stock = 150
  let daily_sales = 12
  let restock_threshold = 30
  let package_size = 50
  
  // 计算剩余天数
  let days_until_empty = divide_with_ceil(current_stock, daily_sales)
  
  // 计算是否需要补货
  let need_restock = if current_stock <= restock_threshold { 1 } else { 0 }
  
  // 计算建议订货量（按包装单位）
  let suggested_order = multiply(need_restock, package_size)
  
  assert_eq(13, days_until_empty)
  assert_eq(0, need_restock)
  assert_eq(0, suggested_order)
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let common_diff = 4
  let num_terms = 8
  
  let sum_n = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 计算等比数列前n项和（简化版）
  let geo_first = 2
  let ratio = 3
  let geo_terms = 5
  let geo_sum = add(add(add(add(geo_first, multiply(geo_first, ratio)), 
    multiply(geo_first, multiply(ratio, ratio))), 
    multiply(geo_first, multiply(ratio, multiply(ratio, ratio)))), 
    multiply(geo_first, multiply(ratio, multiply(ratio, multiply(ratio, ratio)))))
  
  assert_eq(132, sum_n)
  assert_eq(242, geo_sum)
}

test "error_boundary_and_robustness" {
  // 错误边界和健壮性测试
  // 测试极端值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数减法
  assert_eq(2147483647, subtract(max_val, 0))
  assert_eq(-2147483648, subtract(min_val, 0))
  assert_eq(0, subtract(max_val, max_val))
  
  // 测试除法的各种边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, max_val))
  assert_eq(0, divide_with_ceil(max_val, 0))
  assert_eq(0, divide_with_ceil(min_val, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_processing_internationalization" {
  // 字符串处理和国际化测试
  // 测试各种Unicode字符和特殊格式
  assert_eq_string("Hello, 🌟✨🚀!", greet("🌟✨🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, ירושלים!", greet("ירושלים"))
  assert_eq_string("Hello, हिंदी!", greet("हिंदी"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
}

test "engineering_calculations" {
  // 工程计算测试
  // 计算梁的承载能力（简化公式）
  let beam_length = 10
  let beam_width = 2
  let material_strength = 500
  let safety_factor = 2
  
  // 计算截面模量
  let section_modulus = multiply(beam_width, multiply(beam_length, beam_length))
  
  // 计算允许应力
  let allowable_stress = divide_with_ceil(material_strength, safety_factor)
  
  // 计算承载能力
  let load_capacity = multiply(section_modulus, allowable_stress)
  
  assert_eq(40, section_modulus)
  assert_eq(250, allowable_stress)
  assert_eq(10000, load_capacity)
}

test "data_analysis_statistics" {
  // 数据分析和统计测试
  let data_points = [45, 67, 23, 89, 12, 56, 78, 34, 90, 41]
  let num_points = 10
  
  // 计算总和（手动累加）
  let sum_data = add(add(add(add(add(add(add(add(add(data_points[0], data_points[1]), 
    data_points[2]), data_points[3]), data_points[4]), data_points[5]), 
    data_points[6]), data_points[7]), data_points[8]), data_points[9])
  
  // 计算平均值
  let average = divide_with_ceil(sum_data, num_points)
  
  // 计算最大值和最小值的差
  let max_val = 90
  let min_val = 12
  let range = subtract(max_val, min_val)
  
  assert_eq(535, sum_data)
  assert_eq(54, average)
  assert_eq(78, range)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 100
  let constant_factor = 3
  
  // 模拟O(n)算法
  let linear_complexity = multiply(input_size, constant_factor)
  
  // 模拟O(n^2)算法（简化版）
  let quadratic_complexity = multiply(input_size, input_size)
  
  // 模拟O(log n)算法（简化为除法）
  let logarithmic_complexity = divide_with_ceil(input_size, 10)
  
  // 计算性能比较
  let linear_vs_quadratic = divide_with_ceil(quadratic_complexity, linear_complexity)
  let linear_vs_logarithmic = divide_with_ceil(linear_complexity, logarithmic_complexity)
  
  assert_eq(300, linear_complexity)
  assert_eq(10000, quadratic_complexity)
  assert_eq(10, logarithmic_complexity)
  assert_eq(34, linear_vs_quadratic)
  assert_eq(30, linear_vs_logarithmic)
}

test "real_world_project_management" {
  // 真实世界项目管理测试
  let team_size = 8
  let tasks_per_person = 5
  let days_per_task = 3
  let buffer_percentage = 20
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_person)
  
  // 计算基础工期
  let base_duration = multiply(tasks_per_person, days_per_task)
  
  // 计算缓冲时间
  let buffer_time = divide_with_ceil(multiply(base_duration, buffer_percentage), 100)
  
  // 计算总工期
  let total_duration = add(base_duration, buffer_time)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(total_duration, 8)
  
  assert_eq(40, total_tasks)
  assert_eq(15, base_duration)
  assert_eq(3, buffer_time)
  assert_eq(18, total_duration)
  assert_eq(144, total_hours)
}