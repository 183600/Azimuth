// 标准 MoonBit 测试用例 - 为 azimuth 项目添加的10个高质量测试用例
// 覆盖基本功能、边界条件、错误处理和实际应用场景

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(add(15, 25), 40)
  assert_eq(add(-10, 20), 10)
  assert_eq(add(-15, -25), -40)
  
  assert_eq(multiply(6, 7), 42)
  assert_eq(multiply(-6, 7), -42)
  assert_eq(multiply(-6, -7), 42)
  assert_eq(multiply(0, 12345), 0)
  
  assert_eq(subtract(100, 30), 70)
  assert_eq(subtract(30, 100), -70)
  assert_eq(subtract(50, 50), 0)
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数除法
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(12, 3), 4)
  assert_eq(divide_with_ceil(1, 100), 1)
  
  // 负数除法
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), -4)
  assert_eq(divide_with_ceil(-10, -3), 4)
  
  // 零值处理
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
}

test "greet_function_internationalization" {
  // 问候函数的国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 23
  let y = 17
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  // 场景：仓库货物装箱
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(boxes_needed, 7)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(total_capacity, 168)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(remaining_space, 11)
  
  // 验证空间利用率百分比（简化计算）
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(utilization, 94)  // floor(15700/168) = 93%
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  let buffer_percentage = 20
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(total_hours, 960)
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(total_tasks, 1920)
  
  // 计算缓冲任务数
  let buffer_tasks = divide_with_ceil(multiply(total_tasks, buffer_percentage), 100)
  assert_eq(buffer_tasks, 384)
  
  // 计算最终任务数
  let final_tasks = add(total_tasks, buffer_tasks)
  assert_eq(final_tasks, 2304)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极值测试
  assert_eq(add(2147483647, 0), 2147483647)
  assert_eq(add(-2147483648, 0), -2147483648)
  assert_eq(multiply(46340, 46340), 2147395600)  // 接近最大值的安全乘法
  
  // 边界除法测试
  assert_eq(divide_with_ceil(1, 1), 1)
  assert_eq(divide_with_ceil(-1, -1), 1)
  assert_eq(divide_with_ceil(2147483647, 1), 2147483647)
  assert_eq(divide_with_ceil(2147483647, 2147483647), 1)
  
  // 零值边界测试
  assert_eq(multiply(0, 2147483647), 0)
  assert_eq(multiply(0, -2147483648), 0)
  assert_eq(add(0, 2147483647), 2147483647)
  assert_eq(add(0, -2147483648), -2147483648)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(divide_with_ceil(100, 0), 0)
  assert_eq(divide_with_ceil(-100, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(2147483647, 0), 0)
  assert_eq(divide_with_ceil(-2147483648, 0), 0)
  
  // 极小值运算测试
  assert_eq(divide_with_ceil(1, 2147483647), 1)
  assert_eq(divide_with_ceil(-1, 2147483647), -1)
  assert_eq(divide_with_ceil(1, -2147483648), -1)
  assert_eq(divide_with_ceil(-1, -2147483648), 1)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  // 特殊字符和符号
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
  assert_eq_string("Hello, 
	!", greet("
	"))
  
  // 数字和字母混合
  assert_eq_string("Hello, abc123!", greet("abc123"))
  assert_eq_string("Hello, 123abc!", greet("123abc"))
  assert_eq_string("Hello, a1b2c3!", greet("a1b2c3"))
  
  // 长字符串测试
  assert_eq_string("Hello, very_long_string_name_with_multiple_underscores!", greet("very_long_string_name_with_multiple_underscores"))
}

test "real_world_financial_calculation" {
  // 真实世界金融计算测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment_max = 2500
  let loan_term_years = 3
  
  // 计算最小月供（不考虑利息的简化计算）
  let min_monthly_payment = divide_with_ceil(loan_amount, multiply(loan_term_years, 12))
  assert_eq(min_monthly_payment, 1389)  // ceil(50000/36)
  
  // 验证月供在可接受范围内
  assert_true(min_monthly_payment <= monthly_payment_max)
  
  // 计算总还款期数
  let total_months = multiply(loan_term_years, 12)
  assert_eq(total_months, 36)
  
  // 计算实际总还款额（简化计算，不考虑复利）
  let total_repayment = multiply(min_monthly_payment, total_months)
  assert_eq(total_repayment, 50004)  // 1389 * 36
  
  // 计算总利息（简化计算）
  let total_interest = subtract(total_repayment, loan_amount)
  assert_eq(total_interest, 4)
}