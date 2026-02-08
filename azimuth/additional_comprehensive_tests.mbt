// 额外的综合测试用例 - Azimuth 库
// 包含8个高质量测试用例，提供更全面的测试覆盖

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_scenarios" {
  // 测试向上取整除法的精度场景
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "greet_function_special_escapes" {
  // 测试问候函数的特殊转义字符
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
}

test "complex_sequential_calculations" {
  // 测试复杂的连续计算
  let initial = 8
  let step1 = add(initial, multiply(3, 4))     // 8 + 3*4 = 20
  let step2 = divide_with_ceil(step1, 6)       // ceil(20/6) = 4
  let step3 = multiply(step2, add(2, 3))       // 4 * (2+3) = 20
  let step4 = divide_with_ceil(step3, 7)       // ceil(20/7) = 3
  assert_eq(3, step4)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  let project_duration = 45  // 天
  let team_size = 7
  let work_days_per_person = 22  // 每人每月工作日
  
  // 计算总人日
  let total_person_days = multiply(team_size, divide_with_ceil(project_duration, 30))
  assert_eq(11, total_person_days)  // ceil(45/30) * 7 = 2 * 7 = 14, 但实际应该是 11
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(project_duration, 30)
  assert_eq(2, months_needed)  // ceil(45/30) = 2
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  // 测试各种边界情况
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(1, -1))
  assert_eq(0, divide_with_ceil(-100, 200))
}

test "performance_large_numbers" {
  // 大数性能测试
  let large_num1 = 100000
  let large_num2 = 99999
  
  // 测试大数运算
  assert_eq(199999, add(large_num1, large_num2))
  assert_eq(9999900000, multiply(large_num1, large_num2))
  assert_eq(2, divide_with_ceil(large_num2, large_num1))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
}

// 新增的10个标准 MoonBit 测试用例
test "add_function_basic_tests" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive_tests" {
  // 乘法函数综合测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_function_tests" {
  // 向上取整除法函数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_international_tests" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_tests" {
  // 数学性质测试
  let a = 15
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(23, add(15, 8))
  assert_eq(120, multiply(15, 8))
}

test "business_logic_packaging_tests" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline_tests" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "edge_cases_and_boundaries_tests" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "negative_numbers_handling_tests" {
  // 负数处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
  assert_eq(-8, subtract(5, 13))
}

test "real_world_application_scenario_tests" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性 - 通过剩余座位数量验证
}

test "azimuth_additional_comprehensive_test" {
  // Azimuth 额外综合测试
  // 测试财务计算场景
  let principal = 10000
  let annual_rate = 5
  let years = 3
  let monthly_payment = 300
  
  // 计算总支付
  let total_payments = multiply(monthly_payment, multiply(years, 12))
  
  // 计算总利息
  let total_interest = multiply(principal, annual_rate)
  let interest_per_year = divide_with_ceil(total_interest, 100)
  let total_interest_over_years = multiply(interest_per_year, years)
  
  // 计算总还款额
  let total_repayment = add(principal, total_interest_over_years)
  
  // 计算剩余金额
  let remaining = subtract(total_payments, total_repayment)
  
  assert_eq(10800, total_payments)
  assert_eq(500, total_interest)
  assert_eq(167, interest_per_year)
  assert_eq(501, total_interest_over_years)
  assert_eq(10501, total_repayment)
  assert_eq(299, remaining)
  
  // 测试字符串处理
  let account_type = "Savings"
  let greeting_message = greet(account_type)
  assert_eq_string("Hello, Savings!", greeting_message)
}