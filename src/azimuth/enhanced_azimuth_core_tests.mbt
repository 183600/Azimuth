// Enhanced Azimuth Core Tests - 高质量核心功能测试
// 针对 azimuth 库的核心功能进行全面测试

test "subtract_comprehensive_functionality" {
  // 全面测试减法功能
  assert_eq(5, subtract(10, 5))      // 基本减法
  assert_eq(-5, subtract(5, 10))     // 结果为负数
  assert_eq(0, subtract(7, 7))       // 相等数相减
  assert_eq(15, subtract(20, 5))     // 大数减小数
  assert_eq(-15, subtract(5, 20))    // 小数减大数
}

test "simple_financial_calculation" {
  // 简单金融计算：贷款计算
  let principal = 10000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3          // 贷款年限
  
  // 简化计算：总利息 = 本金 × 年利率 × 年限
  let total_interest = multiply(multiply(principal, annual_rate), years) / 100
  let total_payment = add(principal, total_interest)
  
  assert_eq(1500, total_interest)
  assert_eq(11500, total_payment)
}

test "engineering_tolerance_calculation" {
  // 工程容差计算
  let nominal_dimension = 100  // 标称尺寸
  let positive_tolerance = 2   // 正容差
  let negative_tolerance = -1  // 负容差
  
  let max_dimension = add(nominal_dimension, positive_tolerance)
  let min_dimension = add(nominal_dimension, negative_tolerance)
  let tolerance_range = subtract(max_dimension, min_dimension)
  
  assert_eq(102, max_dimension)
  assert_eq(99, min_dimension)
  assert_eq(3, tolerance_range)
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, AI_Model_v2.0!", greet("AI_Model_v2.0"))
}

test "resource_optimization" {
  // 资源优化计算
  let total_resources = 1000
  let project_a_needs = 350
  let project_b_needs = 420
  
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let total_allocated = add(allocated_a, allocated_b)
  let remaining_resources = subtract(total_resources, total_allocated)
  
  assert_eq(770, total_allocated)
  assert_eq(230, remaining_resources)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "mathematical_sequence" {
  // 数学序列计算：等差数列
  let first_term = 3
  let common_difference = 4
  let terms_count = 6
  
  // 计算等差数列第n项：a_n = a_1 + (n-1)d
  let sixth_term = add(first_term, multiply(terms_count - 1, common_difference))
  
  assert_eq(23, sixth_term)  // 3 + 5*4 = 23
}

test "performance_calculation" {
  // 性能计算
  let baseline_operations = 1000
  let optimization_factor = 3
  
  // 计算优化后的操作数
  let optimized_operations = multiply(baseline_operations, optimization_factor)
  
  assert_eq(3000, optimized_operations)
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let final_stock = subtract(initial_stock, total_sold)
  
  assert_eq(84, total_sold)
  assert_eq(66, final_stock)
}

test "time_management" {
  // 时间管理计算
  let total_tasks = 87
  let working_days = 15
  let team_members = 3
  
  // 计算每天需要完成的任务数
  let daily_tasks_per_team = divide_with_ceil(total_tasks, multiply(working_days, team_members))
  
  assert_eq(2, daily_tasks_per_team)    // ceil(87/(15*3)) = ceil(87/45) = 2
}