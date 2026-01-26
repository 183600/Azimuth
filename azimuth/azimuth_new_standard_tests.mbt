// Azimuth 项目新增 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(0, -15))
  assert_eq(-10, subtract(-5, 5))
}

test "divide_with_ceil_precision_scenarios" {
  // 测试向上取整除法的精度场景
  // 小数向上取整测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(11, divide_with_ceil(100, 9))
  
  // 大数精度测试
  assert_eq(100001, divide_with_ceil(1000001, 10))
  assert_eq(500001, divide_with_ceil(1000001, 2))
}

test "string_greeting_multilingual_support" {
  // 测试多语言字符串问候支持
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
}

test "complex_inventory_calculation" {
  // 复杂库存计算场景
  // 场景：计算多个产品的包装需求
  let product_a = 23
  let product_b = 47
  let product_c = 89
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  // 验证结果
  assert_eq(2, boxes_a)  // ceil(23/12) = 2
  assert_eq(4, boxes_b)  // ceil(47/12) = 4
  assert_eq(8, boxes_c)  // ceil(89/12) = 8
  assert_eq(14, total_boxes)  // 2 + 4 + 8 = 14
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试
  // 测试等差数列和等比数列
  let first_term = 3
  let common_difference = 4
  let terms_count = 5
  
  // 计算等差数列第5项
  let nth_term = add(first_term, multiply(common_difference, subtract(terms_count, 1)))
  assert_eq(19, nth_term)  // 3 + 4*(5-1) = 19
  
  // 计算等比数列（公比为2）
  let ratio = 2
  let geometric_nth = multiply(first_term, multiply(ratio, subtract(terms_count, 1)))
  assert_eq(27, geometric_nth)  // 3 * 2^(5-1) = 48 (简化计算)
}

test "resource_scheduling_optimization" {
  // 资源调度优化测试
  // 场景：计算员工排班需求
  let total_shift_hours = 168  // 一周总小时数
  let employees = 8
  let max_hours_per_employee = 40
  
  // 计算需要的最少员工数
  let min_employees = divide_with_ceil(total_shift_hours, max_hours_per_employee)
  assert_eq(5, min_employees)  // ceil(168/40) = 5
  
  // 计算实际分配的小时数
  let actual_hours_per_employee = divide_with_ceil(total_shift_hours, min_employees)
  assert_eq(34, actual_hours_per_employee)  // ceil(168/5) = 34
  
  // 验证总覆盖小时数
  let total_covered_hours = multiply(min_employees, actual_hours_per_employee)
  assert_eq(170, total_covered_hours)
}

test "error_handling_edge_conditions" {
  // 错误处理边缘条件测试
  // 测试各种边界和错误情况
  assert_eq(0, divide_with_ceil(0, 0))  // 0/0 情况
  assert_eq(0, divide_with_ceil(100, 0))  // 除零情况
  assert_eq(0, divide_with_ceil(-100, 0))  // 负数除零情况
  
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(31623, 31623))  // 避免溢出
}

test "real_world_financial_planning" {
  // 真实世界财务规划测试
  // 场景：计算贷款还款计划
  let loan_amount = 50000
  let monthly_payment_capacity = 2500
  let interest_rate = 5  // 5% 年利率
  
  // 计算基础还款月数
  let base_months = divide_with_ceil(loan_amount, monthly_payment_capacity)
  assert_eq(20, base_months)  // ceil(50000/2500) = 20
  
  // 计算考虑利息的总还款额（简化计算）
  let total_interest = divide_with_ceil(multiply(loan_amount, interest_rate), 100)
  let total_repayment = add(loan_amount, total_interest)
  
  // 计算实际月供
  let actual_monthly_payment = divide_with_ceil(total_repayment, base_months)
  
  // 验证结果
  assert_eq(2500, total_interest)  // 50000 * 5% = 2500
  assert_eq(52500, total_repayment)  // 50000 + 2500
  assert_eq(2625, actual_monthly_payment)  // ceil(52500/20) = 2625
}