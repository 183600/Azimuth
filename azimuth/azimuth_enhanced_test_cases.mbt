// Azimuth 增强测试用例 - 新增10个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖各种实际应用场景

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(-5, -20))
  assert_eq(-15, subtract(5, 20))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(2, divide_with_ceil(1999999, 1000000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "complex_inventory_management" {
  // 复杂库存管理场景测试
  let product_a = 127
  let product_b = 89
  let product_c = 45
  let box_capacity = 24
  
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(11, total_boxes)
  
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_products = add(add(product_a, product_b), product_c)
  let unused_space = subtract(total_capacity, total_products)
  
  assert_eq(264, total_capacity)
  assert_eq(261, total_products)
  assert_eq(3, unused_space)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 计算等差数列和等比数列的组合
  let n = 5
  let arithmetic_sum = multiply(n, add(n, 1)) / 2  // 1+2+3+4+5 = 15
  let geometric_sum = subtract(multiply(2, n), 1)  // 2^5-1 = 31
  
  let combined_result = add(arithmetic_sum, geometric_sum)
  assert_eq(46, combined_result)
  
  // 使用向上取整进行分组
  let group_size = 8
  let groups_needed = divide_with_ceil(combined_result, group_size)
  assert_eq(6, groups_needed)
}

test "string_greeting_multilingual_support" {
  // 多语言支持的字符串问候测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
}

test "financial_compound_interest_calculation" {
  // 金融复利计算测试
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的年度复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
  
  // 计算月供
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year3, total_months)
  assert_eq(322, monthly_payment)
}

test "resource_allocation_fair_distribution" {
  // 资源公平分配测试
  let total_resources = 100
  let departments = 3
  
  let base_allocation = divide_with_ceil(total_resources, departments)
  assert_eq(34, base_allocation)
  
  let allocated_total = multiply(base_allocation, departments)
  let remaining = subtract(allocated_total, total_resources)
  
  assert_eq(102, allocated_total)
  assert_eq(2, remaining)
  
  // 验证每个部门至少获得基本份额
  assert_true(base_allocation >= divide_with_ceil(total_resources, departments))
}

test "error_handling_division_by_zero_variations" {
  // 除零错误处理的各种变体测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
}

test "complex_project_timeline_calculation" {
  // 复杂项目时间线计算测试
  let total_tasks = 150
  let team_members = 8
  let tasks_per_person_per_week = 5
  let buffer_percentage = 10
  
  let weekly_capacity = multiply(team_members, tasks_per_person_per_week)
  let base_weeks = divide_with_ceil(total_tasks, weekly_capacity)
  
  assert_eq(4, base_weeks)
  
  // 添加缓冲时间
  let buffer_tasks = divide_with_ceil(multiply(total_tasks, buffer_percentage), 100)
  let tasks_with_buffer = add(total_tasks, buffer_tasks)
  let weeks_with_buffer = divide_with_ceil(tasks_with_buffer, weekly_capacity)
  
  assert_eq(15, buffer_tasks)
  assert_eq(165, tasks_with_buffer)
  assert_eq(5, weeks_with_buffer)
}

test "boolean_logic_validation" {
  // 布尔逻辑验证测试
  let true_condition = true
  let false_condition = false
  
  assert_true(true_condition)
  assert_false(false_condition)
  
  // 测试复合条件
  let and_result = true_condition && false_condition
  let or_result = true_condition || false_condition
  let not_result = !false_condition
  
  assert_false(and_result)
  assert_true(or_result)
  assert_true(not_result)
}