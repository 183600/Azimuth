// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行高质量测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(8, @azimuth.add(5, 3))        // 正数加法
  @azimuth.assert_eq(0, @azimuth.add(0, 0))        // 零加零
  @azimuth.assert_eq(-7, @azimuth.add(-2, -5))     // 负数加法
  @azimuth.assert_eq(2, @azimuth.add(10, -8))      // 正负数混合
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))  // 大数加法
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))     // 正数乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))    // 零乘法
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))   // 负数乘法
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))   // 负负得正
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))  // 大数乘法
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))   // 正数除法，需要向上取整
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))    // 整除情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 被除数小于除数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // 负数除法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))     // 除数为零的安全处理
}

test "greet_function_international" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))     // 空字符串处理
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))  // 特殊字符
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 12
  let discount = 20
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let discounted_total = @azimuth.add(subtotal, -discount)
  let final_total = @azimuth.add(discounted_total, shipping)
  
  @azimuth.assert_eq(540, subtotal)
  @azimuth.assert_eq(520, discounted_total)
  @azimuth.assert_eq(535, final_total)
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  
  let used_budget = @azimuth.add(project_a_cost, project_b_cost)
  let remaining_budget = @azimuth.add(total_budget, -used_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 60
  let small_projects_count = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(630, used_budget)
  @azimuth.assert_eq(370, remaining_budget)
  @azimuth.assert_eq(7, small_projects_count)
}

test "time_management_calculation" {
  // 时间管理计算测试
  let daily_work_hours = 8
  let total_tasks = 47
  let tasks_per_hour = 3
  
  let total_work_days = @azimuth.divide_with_ceil(total_tasks, @azimuth.multiply(daily_work_hours, tasks_per_hour))
  let total_hours_needed = @azimuth.multiply(total_work_days, daily_work_hours)
  
  @azimuth.assert_eq(2, total_work_days)
  @azimuth.assert_eq(16, total_hours_needed)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  let q1_sales = 15000
  let q2_sales = 18000
  let q3_sales = 22000
  let q4_sales = 25000
  
  let annual_sales = @azimuth.add(@azimuth.add(@azimuth.add(q1_sales, q2_sales), q3_sales), q4_sales)
  let quarterly_average = @azimuth.divide_with_ceil(annual_sales, 4)
  
  // 计算增长率
  let q2_growth = @azimuth.divide_with_ceil(@azimuth.multiply(q2_sales - q1_sales, 100), q1_sales)
  
  @azimuth.assert_eq(80000, annual_sales)
  @azimuth.assert_eq(20000, quarterly_average)
  @azimuth.assert_eq(20, q2_growth)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种边界情况和异常输入
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))      // 零除零
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))    // 正数除零
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))   // 负数除零
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1000))          // 零乘大数
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))    // 空字符串问候
}