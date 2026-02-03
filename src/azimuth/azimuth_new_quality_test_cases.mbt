// Azimuth 项目新增质量测试用例
// 使用标准 MoonBit 测试语法，专注于核心功能和边界情况测试

// 导入必要的函数
test "import_test" {
  // 确保库函数可用
  assert_eq(1, 1)
  assert_eq_string("a", "a")
}

test "arithmetic_commutative_laws" {
  // 测试算术交换律
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 负数交换律测试
  assert_eq(add(-a, b), add(b, -a))
  assert_eq(multiply(-a, b), multiply(b, -a))
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 零除数情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 相等数值
  assert_eq(1, divide_with_ceil(50, 50))
  assert_eq(1, divide_with_ceil(-50, -50))
  assert_eq(-1, divide_with_ceil(50, -50))
  
  // 小数结果向上取整
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3))   // 3.666... -> 4
  assert_eq(7, divide_with_ceil(50, 8))   // 6.25 -> 7
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Монде!", greet("Монде"))
  assert_eq_string("Hello, عالم!", greet("عالم"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}

test "complex_business_calculation_pipeline" {
  // 复杂业务计算流水线测试
  // 场景：电商订单总价计算
  let unit_price = 89
  let quantity = 7
  let tax_rate = 8
  let shipping_per_item = 5
  let discount_threshold = 500
  let discount_amount = 45
  
  // 计算商品小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算运费
  let shipping_cost = multiply(shipping_per_item, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, shipping_cost), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(623, subtotal)
  assert_eq(35, shipping_cost)
  assert_eq(50, tax_amount)
  assert_eq(708, pre_discount_total)
  assert_eq(663, final_total)
}

test "mathematical_series_and_progressions" {
  // 数学级数和数列测试
  // 等差数列求和：1 + 3 + 5 + ... + 19 (10个奇数)
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let arithmetic_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 等比数列：2, 4, 8, 16, 32
  let geo_term1 = 2
  let geo_term2 = multiply(geo_term1, 2)
  let geo_term3 = multiply(geo_term2, 2)
  let geo_term4 = multiply(geo_term3, 2)
  let geo_term5 = multiply(geo_term4, 2)
  
  let geometric_sum = add(add(add(add(geo_term1, geo_term2), geo_term3), geo_term4), geo_term5)
  
  assert_eq(100, arithmetic_sum)  // 1+3+5+...+19 = 100
  assert_eq(62, geometric_sum)    // 2+4+8+16+32 = 62
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：项目资源分配
  let total_budget = 10000
  let team_a_cost = 3200
  let team_b_cost = 2800
  let team_c_cost = 2100
  let reserve_percentage = 15
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算预留资金（向上取整）
  let reserve_funds = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  
  // 计算剩余可支配预算
  let available_budget = add(total_budget, -add(allocated_budget, reserve_funds))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 450
  let small_projects_count = divide_with_ceil(available_budget, small_project_cost)
  
  assert_eq(8100, allocated_budget)
  assert_eq(1500, reserve_funds)
  assert_eq(400, available_budget)
  assert_eq(1, small_projects_count)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算测试
  // 场景：项目时间管理
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  let meeting_hours_per_day = 1
  
  // 计算所需工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时，减去会议时间）
  let daily_work_hours = 8 - meeting_hours_per_day
  let total_work_hours = multiply(work_days_needed, daily_work_hours)
  
  // 计算所需周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算实际可用于工作的时间（小时）
  let actual_work_time = multiply(total_tasks, 1)  // 假设每个任务1小时
  
  assert_eq(11, work_days_needed)
  assert_eq(77, total_work_hours)
  assert_eq(3, weeks_needed)
  assert_eq(87, actual_work_time)
}

test "data_processing_and_analysis" {
  // 数据处理和分析测试
  // 场景：数据分析计算
  let data_points = [150, 200, 175, 180, 160, 190, 165, 185, 170, 195]
  let data_count = 10
  
  // 计算总和（手动计算）
  let data_sum = add(add(add(add(add(add(add(add(add(150, 200), 175), 180), 160), 190), 165), 185), 170), 195)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(data_sum, data_count)
  
  // 计算最大值和最小值的差
  let max_value = 200
  let min_value = 150
  let range = subtract(max_value, min_value)
  
  // 计算方差估计（简化版本）
  let variance_estimate = divide_with_ceil(multiply(range, range), 4)
  
  assert_eq(1770, data_sum)
  assert_eq(177, average)
  assert_eq(50, range)
  assert_eq(625, variance_estimate)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试极端输入值
  
  // 大数运算
  let large_num1 = 100000
  let large_num2 = 99999
  let large_sum = add(large_num1, large_num2)
  let large_product = multiply(1000, 1000)
  
  // 零值运算
  let zero_add = add(0, 12345)
  let zero_multiply = multiply(0, 98765)
  let zero_divide = divide_with_ceil(0, 123)
  
  // 负数运算
  let negative_sum = add(-1000, -2000)
  let negative_multiply = multiply(-100, 20)
  let negative_divide = divide_with_ceil(-1000, 3)
  
  // 验证结果
  assert_eq(199999, large_sum)
  assert_eq(1000000, large_product)
  assert_eq(12345, zero_add)
  assert_eq(0, zero_multiply)
  assert_eq(0, zero_divide)
  assert_eq(-3000, negative_sum)
  assert_eq(-2000, negative_multiply)
  assert_eq(-333, negative_divide)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试各种特殊字符串组合
  
  // 长字符串测试
  let long_name = "This is a very long name that contains multiple words and spaces"
  let long_greeting = greet(long_name)
  
  // 特殊字符组合
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_greeting = greet(special_chars)
  
  // 数字和字母混合
  let alphanumeric = "User123_Admin456"
  let alpha_greeting = greet(alphanumeric)
  
  // Unicode 表情符号
  let emoji_string = "😀🎉🚀🌟💻"
  let emoji_greeting = greet(emoji_string)
  
  // 验证结果
  assert_eq_string("Hello, This is a very long name that contains multiple words and spaces!", long_greeting)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", special_greeting)
  assert_eq_string("Hello, User123_Admin456!", alpha_greeting)
  assert_eq_string("Hello, 😀🎉🚀🌟💻!", emoji_greeting)
}