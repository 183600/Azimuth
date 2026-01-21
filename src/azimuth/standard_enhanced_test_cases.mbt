// 新增标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试

test "extreme_value_operations" {
  // 测试极大值和极小值运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(0, add(1000000, -1000000))
  
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(-1000000000000, multiply(-1000000, 1000000))
}

test "precision_division_tests" {
  // 测试除法精度和边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(1, divide_with_ceil(999, 999))
}

test "string_edge_case_processing" {
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, AA!", greet("AA"))
  
  // 测试包含数字的字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, test123!", greet("test123"))
}

test "complex_nested_operations" {
  // 测试复杂嵌套运算
  let step1 = add(5, 3)
  let step2 = multiply(step1, 4)
  let step3 = divide_with_ceil(step2, 3)
  let step4 = add(step3, 10)
  assert_eq(24, step4)
  
  // 更复杂的嵌套
  let complex = add(
    multiply(2, divide_with_ceil(15, 4)),
    multiply(3, divide_with_ceil(20, 6))
  )
  assert_eq(19, complex)
}

test "real_world_payment_calculation" {
  // 实际支付计算场景
  let product_price = 99
  let quantity = 3
  let shipping_fee = 12
  let tax_rate = 8
  let discount_amount = 25
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax), shipping_fee)
  let final_total = add(total_before_discount, -discount_amount)
  
  assert_eq(298, final_total)
}

test "batch_processing_calculation" {
  // 批量处理计算场景
  let batch_size = 50
  let total_items = 375
  let processing_time_per_batch = 8
  let setup_time = 15
  
  let batches_needed = divide_with_ceil(total_items, batch_size)
  let total_processing_time = multiply(batches_needed, processing_time_per_batch)
  let total_time = add(total_processing_time, setup_time)
  
  assert_eq(79, total_time)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算（等比数列）
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 1000
  let project1_cost = 234
  let project2_cost = 189
  let project3_cost = 156
  let reserve_percentage = 10
  
  let project_costs = add(add(project1_cost, project2_cost), project3_cost)
  let reserve = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  let available_for_projects = add(total_budget, -reserve)
  
  assert_eq(900, available_for_projects)
  assert_eq(579, project_costs)
  assert_eq(321, add(available_for_projects, -project_costs))
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, -100))
  
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(0, -100))
  assert_eq(0, multiply(-100, 0))
}

test "comprehensive_function_integration" {
  // 综合函数集成测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let step3 = add(step2, addition)
  
  assert_eq(15, step3)
  
  // 测试不同的计算路径
  let path1 = add(multiply(5, 3), divide_with_ceil(20, 4))
  let path2 = multiply(add(10, 5), divide_with_ceil(12, 3))
  let path3 = divide_with_ceil(add(multiply(4, 6), 8), 5)
  
  assert_eq(23, path1)
  assert_eq(45, path2)
  assert_eq(6, path3)
}