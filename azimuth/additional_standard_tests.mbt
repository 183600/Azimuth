// 额外的标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "advanced_arithmetic_combinations" {
  // 测试高级算术组合
  add(multiply(3, 4), multiply(5, 2)) = 22
  subtract(multiply(10, 5), add(15, 10)) = 25
  divide_with_ceil(multiply(6, 7), subtract(20, 8)) = 4
}

test "negative_number_operations" {
  // 测试负数运算
  add(-8, 3) = -5
  multiply(-4, 6) = -24
  subtract(-10, -5) = -5
  divide_with_ceil(-15, 4) = -3
  divide_with_ceil(15, -4) = -3
}

test "boundary_value_testing" {
  // 测试边界值
  add(2147483647, 0) = 2147483647
  multiply(1, 2147483647) = 2147483647
  divide_with_ceil(1, 2147483647) = 1
  divide_with_ceil(2147483647, 2147483647) = 1
}

test "string_input_validation" {
  // 测试字符串输入验证
  greet("Test123") = "Hello, Test123!"
  greet("   ") = "Hello,    !"
  greet("@Special#") = "Hello, @Special#!"
  greet("MixedCASE") = "Hello, MixedCASE!"
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  multiply(5, add(3, 2)) = add(multiply(5, 3), multiply(5, 2))
  multiply(4, subtract(10, 3)) = subtract(multiply(4, 10), multiply(4, 3))
}

test "real_world_inventory_calculation" {
  // 真实世界库存计算测试
  // 场景：仓库存储优化
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  boxes_needed = 7
  multiply(boxes_needed, box_capacity) = 168
  subtract(168, total_items) = 11
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：项目资源分配
  let team_members = 8
  let hours_per_day = 8
  let project_days = 15
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  
  total_hours = 960
  let daily_cost = 50
  let total_budget = multiply(total_hours, daily_cost)
  total_budget = 48000
  divide_with_ceil(total_budget, 10000) = 5
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  divide_with_ceil(0, 0) = 0
  divide_with_ceil(100, 0) = 0
  divide_with_ceil(-100, 0) = 0
  multiply(0, 2147483647) = 0
  add(0, -2147483648) = -2147483648
}

test "precision_and_rounding" {
  // 精度和舍入测试
  divide_with_ceil(999999, 1000) = 1000
  divide_with_ceil(100001, 100) = 1001
  divide_with_ceil(1999999, 2) = 1000000
  divide_with_ceil(2000000, 3) = 666667
}

test "sequential_calculation_pipeline" {
  // 连续计算流水线测试
  let initial = 12
  let step1 = multiply(initial, 3)
  let step2 = add(step1, 15)
  let step3 = divide_with_ceil(step2, 5)
  let step4 = subtract(step3, 2)
  let final_result = multiply(step4, 4)
  
  step1 = 36
  step2 = 51
  step3 = 11
  step4 = 9
  final_result = 36
}