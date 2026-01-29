// 标准 MoonBit 测试用例 - 补充测试覆盖
// 包含10个高质量测试用例，覆盖各种边缘情况和实际应用场景

test "negative_division_comprehensive" {
  // 全面测试负数除法运算
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(0, divide_with_ceil(-1, 10))
  assert_eq(-1, divide_with_ceil(-10, 10))
}

test "string_length_and_unicode" {
  // 测试字符串长度和Unicode字符处理
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, émoji!", greet("émoji"))
  assert_eq_string("Hello, Ñ!", greet("Ñ"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "extreme_values_calculation" {
  // 测试极值计算
  let max_safe_int = 1000000
  let min_safe_int = -1000000
  
  // 极值加法
  assert_eq(2000000, add(max_safe_int, max_safe_int))
  assert_eq(-2000000, add(min_safe_int, min_safe_int))
  assert_eq(0, add(max_safe_int, min_safe_int))
  
  // 极值乘法
  assert_eq(1000000000000, multiply(max_safe_int, max_safe_int))
  assert_eq(1000000000000, multiply(min_safe_int, min_safe_int))
  assert_eq(-1000000000000, multiply(max_safe_int, min_safe_int))
}

test "function_composition_advanced" {
  // 测试高级函数组合
  let base = 15
  
  // 嵌套函数调用
  let result = divide_with_ceil(
    multiply(
      add(base, 5),
      subtract(base, 5)
    ),
    add(base, divide_with_ceil(base, 3))
  )
  
  // 手动计算：(15+5)*(15-5) / (15+ceil(15/3)) = 20*10 / 20 = 1
  assert_eq(1, result)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let product_a = 120
  let product_b = 85
  let product_c = 200
  let box_capacity = 30
  
  // 计算每种产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(4, boxes_a)  // ceil(120/30) = 4
  assert_eq(3, boxes_b)  // ceil(85/30) = 3
  assert_eq(7, boxes_c)  // ceil(200/30) = 7
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(14, total_boxes)
  assert_eq(420, total_capacity)
  
  // 计算剩余容量
  let total_products = add(add(product_a, product_b), product_c)
  let remaining_capacity = subtract(total_capacity, total_products)
  
  assert_eq(15, remaining_capacity)
}

test "error_recovery_scenarios" {
  // 测试错误恢复场景
  // 连续除零操作的恢复
  let step1 = divide_with_ceil(100, 0)  // 返回0
  let step2 = add(step1, 50)  // 0 + 50 = 50
  let step3 = divide_with_ceil(step2, 10)  // ceil(50/10) = 5
  
  assert_eq(0, step1)
  assert_eq(50, step2)
  assert_eq(5, step3)
  
  // 混合错误场景
  let fallback_result = add(
    divide_with_ceil(20, 0),  // 0
    multiply(5, 3)  // 15
  )
  
  assert_eq(15, fallback_result)
}

test "precision_and_rounding_edge_cases" {
  // 测试精度和舍入边缘情况
  // 精确除法
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(50, divide_with_ceil(100, 2))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(25, divide_with_ceil(100, 4))
  assert_eq(20, divide_with_ceil(100, 5))
  
  // 接近整数的除法
  assert_eq(11, divide_with_ceil(99, 9))
  assert_eq(11, divide_with_ceil(100, 9))
  assert_eq(12, divide_with_ceil(101, 9))
  
  // 大数精度
  assert_eq(10001, divide_with_ceil(100001, 10))
  assert_eq(100001, divide_with_ceil(1000010, 10))
}

test "multi_step_business_logic" {
  // 测试多步骤业务逻辑
  // 场景：计算项目成本和资源分配
  let project_duration = 12  // 月
  let team_size = 8
  let monthly_salary = 8000
  let equipment_cost = 50000
  let monthly_overhead = 3000
  
  // 计算人力成本
  let total_salary_cost = multiply(team_size, multiply(monthly_salary, project_duration))
  
  // 计算总运营成本
  let total_overhead = multiply(monthly_overhead, project_duration)
  let total_cost = add(add(total_salary_cost, equipment_cost), total_overhead)
  
  // 计算季度预算需求
  let quarterly_budget = divide_with_ceil(total_cost, 4)
  
  // 验证计算结果
  assert_eq(768000, total_salary_cost)  // 8 * 8000 * 12
  assert_eq(36000, total_overhead)  // 3000 * 12
  assert_eq(854000, total_cost)  // 768000 + 50000 + 36000
  assert_eq(213500, quarterly_budget)  // ceil(854000/4)
}

test "performance_and_scalability" {
  // 测试性能和可扩展性场景
  let base_value = 1000
  
  // 模拟批量处理
  let batch_size = 50
  let total_items = 1234
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  assert_eq(25, batches_needed)  // ceil(1234/50)
  
  // 计算处理时间（假设每批处理需要5个单位时间）
  let time_per_batch = 5
  let total_processing_time = multiply(batches_needed, time_per_batch)
  
  assert_eq(125, total_processing_time)
  
  // 计算实际处理的最后一项数量
  let processed_in_full_batches = multiply(batches_needed - 1, batch_size)
  let final_batch_size = subtract(total_items, processed_in_full_batches)
  
  assert_eq(1200, processed_in_full_batches)
  assert_eq(34, final_batch_size)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试临界值附近的计算
  let divisor = 7
  
  // 正好整除的情况
  assert_eq(1, divide_with_ceil(7, divisor))
  assert_eq(2, divide_with_ceil(14, divisor))
  assert_eq(3, divide_with_ceil(21, divisor))
  
  // 接近整除的情况
  assert_eq(2, divide_with_ceil(8, divisor))  // 8/7 = 1.14 -> 2
  assert_eq(2, divide_with_ceil(13, divisor))  // 13/7 = 1.86 -> 2
  
  // 边界值组合测试
  let boundary_result = add(
    multiply(divide_with_ceil(10, 3), 3),  // ceil(10/3)*3 = 4*3 = 12
    subtract(divide_with_ceil(20, 7), 2)   // ceil(20/7)-2 = 3-2 = 1
  )
  
  assert_eq(13, boundary_result)
}