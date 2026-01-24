// 增强的 MoonBit 测试用例
// 为 azimuth 库添加额外的测试覆盖

// 导入被测试的模块
use azimuth

// 测试复杂数学运算组合
test complex_mathematical_operations {
  // 测试连续运算的数学表达式
  let a = 10
  let b = 3
  let c = 2
  
  // 复杂表达式: (a + b) * c / a 的向上取整
  let step1 = azimuth.add(a, b)  // 13
  let step2 = azimuth.multiply(step1, c)  // 26
  let result = azimuth.divide_with_ceil(step2, a)  // ceil(26/10) = 3
  azimuth.assert_eq(3, result)
  
  // 测试嵌套运算: a * (b + c/a)
  let nested_result = azimuth.multiply(a, azimuth.add(b, azimuth.divide_with_ceil(c, a)))
  azimuth.assert_eq(31, nested_result)  // 10 * (3 + ceil(2/10)) = 10 * 4 = 40
}

// 测试实际业务场景：项目管理计算
test project_management_calculations {
  // 场景：计算项目资源分配
  let team_members = 7
  let tasks_per_member = 5
  let sprint_capacity = 16
  
  // 计算总任务数
  let total_tasks = azimuth.multiply(team_members, tasks_per_member)
  azimuth.assert_eq(35, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  azimuth.assert_eq(3, sprints_needed)  // ceil(35/16) = 3
  
  // 计算实际利用率
  let total_capacity = azimuth.multiply(sprints_needed, sprint_capacity)
  let unused_capacity = azimuth.subtract(total_capacity, total_tasks)
  azimuth.assert_eq(13, unused_capacity)  // 48 - 35 = 13
}

// 测试字符串处理的边缘情况
test string_edge_cases {
  // 测试各种特殊字符串输入
  azimuth.assert_eq_string("Hello, null!", azimuth.greet("null"))
  azimuth.assert_eq_string("Hello, undefined!", azimuth.greet("undefined"))
  azimuth.assert_eq_string("Hello, 123456!", azimuth.greet("123456"))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("   "))
  azimuth.assert_eq_string("Hello, \n\r\t!", azimuth.greet("\n\r\t"))
  azimuth.assert_eq_string("Hello, \"quoted\"!", azimuth.greet("\"quoted\""))
  azimuth.assert_eq_string("Hello, 'apostrophe'!", azimuth.greet("'apostrophe'"))
}

// 测试大数运算和性能
test large_number_operations {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 999999
  
  azimuth.assert_eq(1999999, azimuth.add(large_num1, large_num2))
  azimuth.assert_eq(999999000000, azimuth.multiply(large_num1, large_num2))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(large_num1, large_num2))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(large_num2, large_num1))
  
  // 测试极值情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  azimuth.assert_eq(2147483647, azimuth.add(max_int, 0))
  azimuth.assert_eq(-2147483648, azimuth.add(min_int, 0))
  azimuth.assert_eq(0, azimuth.multiply(max_int, 0))
  azimuth.assert_eq(0, azimuth.multiply(min_int, 0))
}

// 测试除法的各种边界情况
test division_boundary_conditions {
  // 测试除数为零的各种情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(100, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(2147483647, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-2147483648, 0))
  
  // 测试被除数为零的情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, -1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 100))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, -100))
  
  // 测试相同数相除
  azimuth.assert_eq(1, azimuth.divide_with_ceil(5, 5))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-5, -5))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(5, -5))
  azimuth.assert_eq(-1, azimuth.divide_with_ceil(-5, 5))
}

// 测试数学定律和性质
test mathematical_properties {
  // 测试加法结合律
  let a = 5
  let b = 7
  let c = 3
  
  let left_assoc = azimuth.add(azimuth.add(a, b), c)  // (5 + 7) + 3 = 15
  let right_assoc = azimuth.add(a, azimuth.add(b, c))  // 5 + (7 + 3) = 15
  azimuth.assert_eq(left_assoc, right_assoc)
  
  // 测试乘法结合律
  let mult_left = azimuth.multiply(azimuth.multiply(a, b), c)  // (5 * 7) * 3 = 105
  let mult_right = azimuth.multiply(a, azimuth.multiply(b, c))  // 5 * (7 * 3) = 105
  azimuth.assert_eq(mult_left, mult_right)
  
  // 测试分配律: a * (b + c) = a * b + a * c
  let distrib_left = azimuth.multiply(a, azimuth.add(b, c))  // 5 * (7 + 3) = 50
  let distrib_right = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))  // 5 * 7 + 5 * 3 = 50
  azimuth.assert_eq(distrib_left, distrib_right)
}

// 测试实际应用场景：库存管理
test inventory_management_scenario {
  // 场景：仓库库存装箱计算
  let product_a_qty = 47
  let product_b_qty = 83
  let product_c_qty = 29
  let box_capacity = 12
  
  // 计算各产品需要的箱子数
  let boxes_a = azimuth.divide_with_ceil(product_a_qty, box_capacity)  // ceil(47/12) = 4
  let boxes_b = azimuth.divide_with_ceil(product_b_qty, box_capacity)  // ceil(83/12) = 7
  let boxes_c = azimuth.divide_with_ceil(product_c_qty, box_capacity)  // ceil(29/12) = 3
  
  // 计算总箱子数
  let total_boxes = azimuth.add(azimuth.add(boxes_a, boxes_b), boxes_c)
  azimuth.assert_eq(14, total_boxes)
  
  // 计算总容量
  let total_capacity = azimuth.multiply(total_boxes, box_capacity)
  azimuth.assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let total_items = azimuth.add(azimuth.add(product_a_qty, product_b_qty), product_c_qty)
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(9, remaining_space)  // 168 - 159 = 9
}

// 测试连续运算的复杂场景
test sequential_calculation_chain {
  // 测试连续计算链：预算分配场景
  let total_budget = 50000
  let departments = 4
  let base_allocation = azimuth.divide_with_ceil(total_budget, departments)  // ceil(50000/4) = 12500
  
  // 各部门调整
  let dept1 = azimuth.add(base_allocation, 3000)  // 15500
  let dept2 = azimuth.multiply(base_allocation, 2)  // 25000
  let dept3 = azimuth.subtract(base_allocation, 2000)  // 10500
  let dept4 = azimuth.divide_with_ceil(base_allocation, 2)  // ceil(12500/2) = 6250
  
  // 计算实际总分配
  let actual_total = azimuth.add(azimuth.add(dept1, dept2), azimuth.add(dept3, dept4))
  azimuth.assert_eq(57250, actual_total)
  
  // 计算超支金额
  let over_budget = azimuth.subtract(actual_total, total_budget)
  azimuth.assert_eq(7250, over_budget)
}

// 辅助函数：减法（如果原库中没有）
fn subtract(a : Int, b : Int) -> Int {
  a - b
}