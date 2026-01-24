// 增强标准测试用例 - 为 azimuth 库添加额外的测试覆盖

test "add_function_large_numbers" {
  // 测试加法函数的大数处理
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(0, add(1000000, -1000000))
}

test "multiply_function_large_numbers" {
  // 测试乘法函数的大数处理
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(666667, divide_with_ceil(2000000, 3))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 场景：项目预算分配
  let total_budget = 50000
  let team_members = 8
  let budget_per_person = divide_with_ceil(total_budget, team_members)
  assert_eq(6250, budget_per_person)
  
  // 验证总分配
  let total_allocated = multiply(budget_per_person, team_members)
  assert_eq(50000, total_allocated)
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_mixed_signs" {
  // 测试向上取整除法的混合符号
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(7, -4))
  assert_eq(1, divide_with_ceil(3, -4))
}

test "sequential_calculation_chain" {
  // 测试连续计算链
  let initial = 10
  let step1 = add(initial, multiply(2, 3))     // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)       // ceil(16/5) = 4
  let step3 = multiply(step2, add(1, 2))       // 4 * (1+2) = 12
  let step4 = divide_with_ceil(step3, 7)       // ceil(12/7) = 2
  assert_eq(2, step4)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 场景：仓库存储优化
  let items = [
    (25, 8),   // (数量, 每箱容量)
    (47, 12),
    (156, 20)
  ]
  
  // 计算每种物品需要的箱子数量
  let boxes_1 = divide_with_ceil(items[0].0, items[0].1)  // ceil(25/8) = 4
  let boxes_2 = divide_with_ceil(items[1].0, items[1].1)  // ceil(47/12) = 4
  let boxes_3 = divide_with_ceil(items[2].0, items[2].1)  // ceil(156/20) = 8
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_1, boxes_2), boxes_3)
  assert_eq(16, total_boxes)
  
  // 计算总存储容量
  let capacity_1 = multiply(boxes_1, items[0].1)  // 4 * 8 = 32
  let capacity_2 = multiply(boxes_2, items[1].1)  // 4 * 12 = 48
  let capacity_3 = multiply(boxes_3, items[2].1)  // 8 * 20 = 160
  
  let total_capacity = add(add(capacity_1, capacity_2), capacity_3)
  assert_eq(240, total_capacity)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}