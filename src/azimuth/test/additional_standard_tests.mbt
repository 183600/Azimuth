// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能提供补充测试覆盖

test "subtract_function_comprehensive" {
  // 全面测试 subtract 函数
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(10, 0))
  @azimuth.assert_eq(-10, @azimuth.subtract(0, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(-5, -20))
  @azimuth.assert_eq(-25, @azimuth.subtract(-10, 15))
}

test "complex_financial_calculations" {
  // 复杂金融计算场景
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  let years = 2
  
  // 计算复利：第一年
  let interest1 = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate1), 100)
  let amount1 = @azimuth.add(principal, interest1)
  
  // 计算复利：第二年（利率变化）
  let interest2 = @azimuth.divide_with_ceil(@azimuth.multiply(amount1, rate2), 100)
  let final_amount = @azimuth.add(amount1, interest2)
  
  @azimuth.assert_eq(500, interest1)
  @azimuth.assert_eq(10500, amount1)
  @azimuth.assert_eq(315, interest2)
  @azimuth.assert_eq(10815, final_amount)
}

test "inventory_optimization" {
  // 库存优化计算
  let current_stock = 150
  let daily_sales = 12
  let restock_threshold = 30
  let package_size = 50
  
  // 计算剩余天数
  let days_until_empty = @azimuth.divide_with_ceil(current_stock, daily_sales)
  
  // 计算是否需要补货
  let need_restock = if current_stock <= restock_threshold { 1 } else { 0 }
  
  // 计算建议订货量（按包装单位）
  let suggested_order = @azimuth.multiply(need_restock, package_size)
  
  @azimuth.assert_eq(13, days_until_empty)
  @azimuth.assert_eq(0, need_restock)
  @azimuth.assert_eq(0, suggested_order)
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let first_term = 3
  let common_diff = 4
  let num_terms = 8
  
  let sum_n = @azimuth.divide_with_ceil(
    @azimuth.multiply(num_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 计算等比数列前n项和（简化版）
  let geo_first = 2
  let ratio = 3
  let geo_terms = 5
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geo_first, @azimuth.multiply(geo_first, ratio)), 
    @azimuth.multiply(geo_first, @azimuth.multiply(ratio, ratio))), 
    @azimuth.multiply(geo_first, @azimuth.multiply(ratio, @azimuth.multiply(ratio, ratio)))), 
    @azimuth.multiply(geo_first, @azimuth.multiply(ratio, @azimuth.multiply(ratio, @azimuth.multiply(ratio, ratio)))))
  
  @azimuth.assert_eq(132, sum_n)
  @azimuth.assert_eq(242, geo_sum)
}

test "error_boundary_and_robustness" {
  // 错误边界和健壮性测试
  // 测试极端值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数减法
  @azimuth.assert_eq(2147483647, @azimuth.subtract(max_val, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.subtract(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.subtract(max_val, max_val))
  
  // 测试除法的各种边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_val))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "string_processing_internationalization" {
  // 字符串处理和国际化测试
  // 测试各种Unicode字符和特殊格式
  @azimuth.assert_eq_string("Hello, 🌟✨🚀!", @azimuth.greet("🌟✨🚀"))
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 北京!", @azimuth.greet("北京"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, ירושלים!", @azimuth.greet("ירושלים"))
  @azimuth.assert_eq_string("Hello, हिंदी!", @azimuth.greet("हिंदी"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
}

test "engineering_calculations" {
  // 工程计算测试
  // 计算梁的承载能力（简化公式）
  let beam_length = 10
  let beam_width = 2
  let material_strength = 500
  let safety_factor = 2
  
  // 计算截面模量
  let section_modulus = @azimuth.multiply(beam_width, @azimuth.multiply(beam_length, beam_length))
  
  // 计算允许应力
  let allowable_stress = @azimuth.divide_with_ceil(material_strength, safety_factor)
  
  // 计算承载能力
  let load_capacity = @azimuth.multiply(section_modulus, allowable_stress)
  
  @azimuth.assert_eq(40, section_modulus)
  @azimuth.assert_eq(250, allowable_stress)
  @azimuth.assert_eq(10000, load_capacity)
}

test "data_analysis_statistics" {
  // 数据分析和统计测试
  let data_points = [45, 67, 23, 89, 12, 56, 78, 34, 90, 41]
  let num_points = 10
  
  // 计算总和（手动累加）
  let sum_data = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data_points[0], data_points[1]), 
    data_points[2]), data_points[3]), data_points[4]), data_points[5]), 
    data_points[6]), data_points[7]), data_points[8]), data_points[9])
  
  // 计算平均值
  let average = @azimuth.divide_with_ceil(sum_data, num_points)
  
  // 计算最大值和最小值的差
  let max_val = 90
  let min_val = 12
  let range = @azimuth.subtract(max_val, min_val)
  
  @azimuth.assert_eq(535, sum_data)
  @azimuth.assert_eq(54, average)
  @azimuth.assert_eq(78, range)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 100
  let constant_factor = 3
  
  // 模拟O(n)算法
  let linear_complexity = @azimuth.multiply(input_size, constant_factor)
  
  // 模拟O(n^2)算法（简化版）
  let quadratic_complexity = @azimuth.multiply(input_size, input_size)
  
  // 模拟O(log n)算法（简化为除法）
  let logarithmic_complexity = @azimuth.divide_with_ceil(input_size, 10)
  
  // 计算性能比较
  let linear_vs_quadratic = @azimuth.divide_with_ceil(quadratic_complexity, linear_complexity)
  let linear_vs_logarithmic = @azimuth.divide_with_ceil(linear_complexity, logarithmic_complexity)
  
  @azimuth.assert_eq(300, linear_complexity)
  @azimuth.assert_eq(10000, quadratic_complexity)
  @azimuth.assert_eq(10, logarithmic_complexity)
  @azimuth.assert_eq(34, linear_vs_quadratic)
  @azimuth.assert_eq(30, linear_vs_logarithmic)
}

test "real_world_project_management" {
  // 真实世界项目管理测试
  let team_size = 8
  let tasks_per_person = 5
  let days_per_task = 3
  let buffer_percentage = 20
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  
  // 计算基础工期
  let base_duration = @azimuth.multiply(tasks_per_person, days_per_task)
  
  // 计算缓冲时间
  let buffer_time = @azimuth.divide_with_ceil(@azimuth.multiply(base_duration, buffer_percentage), 100)
  
  // 计算总工期
  let total_duration = @azimuth.add(base_duration, buffer_time)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(total_duration, 8)
  
  @azimuth.assert_eq(40, total_tasks)
  @azimuth.assert_eq(15, base_duration)
  @azimuth.assert_eq(3, buffer_time)
  @azimuth.assert_eq(18, total_duration)
  @azimuth.assert_eq(144, total_hours)
}