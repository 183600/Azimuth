// 简单的增强测试用例
// 专注于实际应用场景

test "performance_calculation" {
  // 性能计算测试
  let base_operations = 1000
  let multiplier = 3
  let overhead = 50
  
  let total_ops = multiply(base_operations, multiplier)
  let final_result = add(total_ops, overhead)
  
  assert_eq(3050, final_result)
}

test "business_budget_calculation" {
  // 业务预算计算
  let total_budget = 10000
  let project_costs = multiply(1500, 4)
  let remaining = add(total_budget, -project_costs)
  
  assert_eq(4000, remaining)
}

test "packaging_efficiency" {
  // 包装效率计算
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
}

test "string_greeting_test" {
  // 字符串问候测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_calculation" {
  // 复杂计算测试
  let result = add(multiply(10, 5), divide_with_ceil(100, 3))
  assert_eq(83, result)
}