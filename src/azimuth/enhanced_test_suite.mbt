// 增强的 MoonBit 测试套件 - 高质量测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "advanced_mathematical_operations" {
  // 高级数学运算测试
  let x = 12
  let y = 8
  let z = 5
  
  // 测试复合运算：(x + y) * z - x / y
  let complex_result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(99, complex_result)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
  
  // 测试嵌套运算：x * (y + z) + y * z
  let nested_result = add(multiply(x, add(y, z)), multiply(y, z))
  assert_eq(196, nested_result)  // 12 * (8 + 5) + 8 * 5 = 12*13 + 40 = 156 + 40 = 196
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 年度复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)  // 1000 + 5% = 1050
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "logistics_packaging_optimization" {
  // 物流包装优化测试
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 67
  let box_capacity = 24
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(2, boxes_a)   // ceil(45/24) = 2
  assert_eq(2, boxes_b)   // ceil(32/24) = 2
  assert_eq(3, boxes_c)   // ceil(67/24) = 3
  assert_eq(7, total_boxes)  // 2 + 2 + 3 = 7
  
  // 计算总容量和利用率
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let utilization = divide_with_ceil(multiply(total_items, 100), total_capacity)
  
  assert_eq(168, total_capacity)  // 7 * 24 = 168
  assert_eq(144, total_items)     // 45 + 32 + 67 = 144
  assert_eq(86, utilization)      // floor(144*100/168) = 85%
}

test "data_structure_simulation" {
  // 数据结构模拟测试 - 栈和队列操作
  // 模拟栈操作
  let stack_size = 0
  let stack_push1 = add(stack_size, 1)
  let stack_push2 = add(stack_push1, 1)
  let stack_push3 = add(stack_push2, 1)
  let stack_pop1 = subtract(stack_push3, 1)
  let stack_pop2 = subtract(stack_pop1, 1)
  
  assert_eq(1, stack_push1)
  assert_eq(2, stack_push2)
  assert_eq(3, stack_push3)
  assert_eq(2, stack_pop1)
  assert_eq(1, stack_pop2)
  
  // 模拟队列操作
  let queue_capacity = 10
  let queue_items = 7
  let enqueue_count = 3
  let dequeue_count = 2
  
  let final_queue_size = add(subtract(queue_items, dequeue_count), enqueue_count)
  let remaining_capacity = subtract(queue_capacity, final_queue_size)
  
  assert_eq(8, final_queue_size)     // (7 - 2) + 3 = 8
  assert_eq(2, remaining_capacity)  // 10 - 8 = 2
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析测试
  let input_size = 1000
  
  // 模拟 O(n) 算法
  let linear_complexity = multiply(input_size, 3)
  
  // 模拟 O(log n) 算法（简化为除法）
  let logarithmic_complexity = divide_with_ceil(input_size, 100)
  
  // 模拟 O(n^2) 算法（简化为乘法）
  let quadratic_complexity = divide_with_ceil(multiply(input_size, input_size), 10000)
  
  assert_eq(3000, linear_complexity)      // 1000 * 3
  assert_eq(10, logarithmic_complexity)   // ceil(1000/100)
  assert_eq(100, quadratic_complexity)    // ceil(1000*1000/10000)
  
  // 计算效率比较
  let linear_vs_log = divide_with_ceil(linear_complexity, logarithmic_complexity)
  let quadratic_vs_linear = divide_with_ceil(quadratic_complexity, linear_complexity)
  
  assert_eq(300, linear_vs_log)      // 3000 / 10
  assert_eq(1, quadratic_vs_linear)  // ceil(100/3000) = 1
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  let first_name = "张"
  let last_name = "三"
  let company = "科技"
  let department = "研发部"
  
  // 构建完整的身份信息
  let full_name = first_name + last_name
  let work_info = company + " " + department
  let identity = full_name + " @ " + work_info
  
  let formal_greeting = greet(identity)
  
  assert_eq_string("Hello, 张三 @ 科技 研发部!", formal_greeting)
  
  // 测试特殊字符组合
  let special_chars = "αβγ123!@#"
  let special_greeting = greet(special_chars)
  assert_eq_string("Hello, αβγ123!@#!", special_greeting)
}

test "engineering_calculation" {
  // 工程计算测试 - 结构分析
  let beam_length = 12  // 米
  let beam_width = 0.5  // 米（简化为整数计算）
  let beam_height = 8   // 厘米（简化为整数计算）
  let material_density = 7850  // kg/m³（简化计算）
  
  // 计算梁的体积（简化计算）
  let volume = multiply(beam_length, multiply(beam_width, divide_with_ceil(beam_height, 100)))
  
  // 计算重量（简化计算）
  let weight = divide_with_ceil(multiply(volume, material_density), 1000)
  
  // 计算支撑点数量（每3米一个支撑点）
  let support_points = divide_with_ceil(beam_length, 3)
  
  assert_eq(0, volume)  // 12 * 0 * 0 = 0（由于整数简化）
  assert_eq(0, weight)  // 基于体积计算
  assert_eq(4, support_points)  // ceil(12/3) = 4
  
  // 修正计算以避免零值
  let corrected_volume = multiply(beam_length, multiply(5, divide_with_ceil(beam_height, 100)))
  let corrected_weight = divide_with_ceil(multiply(corrected_volume, material_density), 10000)
  
  assert_eq(4, corrected_volume)   // 12 * 5 * 0.08 = 4.8 -> 4
  assert_eq(3, corrected_weight)  // ceil(4 * 7850 / 10000) = 3
}

test "game_mechanics_simulation" {
  // 游戏机制模拟测试
  let player_level = 15
  let base_damage = 100
  let damage_per_level = 12
  let critical_chance = 25  // 百分比
  
  // 计算基础伤害
  let total_base_damage = add(base_damage, multiply(player_level, damage_per_level))
  
  // 计算暴击伤害（2倍伤害）
  let critical_damage = multiply(total_base_damage, 2)
  
  // 计算平均伤害（考虑暴击概率）
  let average_damage = add(
    multiply(total_base_damage, subtract(100, critical_chance)),
    multiply(critical_damage, critical_chance)
  ) / 100
  
  // 计算对护甲的伤害（护甲减免30%）
  let damage_after_armor = divide_with_ceil(multiply(average_damage, 70), 100)
  
  assert_eq(280, total_base_damage)   // 100 + 15 * 12 = 280
  assert_eq(560, critical_damage)     // 280 * 2 = 560
  assert_eq(350, average_damage)      // (280 * 75 + 560 * 25) / 100 = 350
  assert_eq(245, damage_after_armor)  // ceil(350 * 70 / 100) = 245
}

test "resource_management_optimization" {
  // 资源管理优化测试
  let total_budget = 50000
  let project_costs = multiply(8000, 4)  // 4个项目，每个8000
  let operational_costs = 12000
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, add(project_costs, operational_costs))
  
  // 计算可以启动的小项目数量（每个3000）
  let small_projects_possible = divide_with_ceil(remaining_budget, 3000)
  
  // 计算资源利用率
  let utilization_rate = divide_with_ceil(
    multiply(add(project_costs, operational_costs), 100),
    total_budget
  )
  
  assert_eq(32000, project_costs)          // 8000 * 4 = 32000
  assert_eq(44000, add(project_costs, operational_costs))  // 32000 + 12000 = 44000
  assert_eq(6000, remaining_budget)        // 50000 - 44000 = 6000
  assert_eq(2, small_projects_possible)    // ceil(6000/3000) = 2
  assert_eq(88, utilization_rate)          // ceil(44000*100/50000) = 88
}