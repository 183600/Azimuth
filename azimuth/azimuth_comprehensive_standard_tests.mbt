// Azimuth 综合标准测试用例
// 10个高质量的标准 MoonBit 测试用例

test "mathematical_associative_properties" {
  // 测试数学结合律性质
  let a = 5
  let b = 7
  let c = 3
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(
    add(add(a, b), c),
    add(a, add(b, c))
  )
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(
    multiply(multiply(a, b), c),
    multiply(a, multiply(b, c))
  )
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "complex_inventory_management" {
  // 复杂库存管理场景测试
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 67
  let box_capacity = 20
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 验证箱子数量
  assert_eq(3, boxes_a)  // ceil(45/20) = 3
  assert_eq(2, boxes_b)  // ceil(32/20) = 2
  assert_eq(4, boxes_c)  // ceil(67/20) = 4
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(9, total_boxes)
  assert_eq(180, total_capacity)
}

test "string_special_characters_handling" {
  // 字符串特殊字符处理测试
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
  assert_eq_string("Hello, 
	!", greet("
	"))
  assert_eq_string("Hello, 🌟⭐✨!", greet("🌟⭐✨"))
}

test "mathematical_identity_elements" {
  // 数学恒等式元素测试
  let x = 42
  
  // 加法恒等式: x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式: x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元素: x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 15000
  let departments = 4
  let min_budget_per_dept = 3000
  
  // 计算每个部门的最低预算
  let allocated_per_dept = divide_with_ceil(total_budget, departments)
  assert_eq(3750, allocated_per_dept)  // ceil(15000/4) = 3750
  
  // 验证每个部门预算都满足最低要求
  assert_eq(1, if (allocated_per_dept >= min_budget_per_dept) { 1 } else { 0 })
  
  // 计算总分配预算
  let total_allocated = multiply(allocated_per_dept, departments)
  assert_eq(15000, total_allocated)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 15
  
  // 计算流水线: ((x * 2) + 10) / 3 + 5
  let step1 = multiply(initial_value, 2)        // 15 * 2 = 30
  let step2 = add(step1, 10)                    // 30 + 10 = 40
  let step3 = divide_with_ceil(step2, 3)        // ceil(40/3) = 14
  let result = add(step3, 5)                    // 14 + 5 = 19
  
  assert_eq(19, result)
}

test "negative_number_comprehensive" {
  // 负数综合测试
  let a = -12
  let b = -8
  let c = 15
  
  // 负数加法
  assert_eq(-20, add(a, b))
  assert_eq(3, add(a, c))
  
  // 负数乘法
  assert_eq(96, multiply(a, b))    // (-12) * (-8) = 96
  assert_eq(-180, multiply(a, c))  // (-12) * 15 = -180
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(a, b))   // ceil(-12/-8) = 2
  assert_eq(-1, divide_with_ceil(a, c))   // ceil(-12/15) = -1
  assert_eq(1, divide_with_ceil(c, a))    // ceil(15/-12) = -2
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let guests = 127
  let chairs_per_table = 8
  let cost_per_table = 50
  let budget_per_guest = 25
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(16, tables_needed)  // ceil(127/8) = 16
  
  // 计算桌子总成本
  let table_cost = multiply(tables_needed, cost_per_table)
  assert_eq(800, table_cost)
  
  // 计算餐饮总预算
  let catering_budget = multiply(guests, budget_per_guest)
  assert_eq(3175, catering_budget)
  
  // 计算活动总成本
  let total_cost = add(table_cost, catering_budget)
  assert_eq(3975, total_cost)
  
  // 验证桌子数量足够
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(1, if (total_chairs >= guests) { 1 } else { 0 })
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极大值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 连续运算边界测试
  let chain_result = add(
    multiply(2, 3),
    divide_with_ceil(subtract(10, 4), 2)
  )
  assert_eq(9, chain_result)  // 2*3 + ceil((10-4)/2) = 6 + 3 = 9
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, very long string!", greet("very long string"))
}