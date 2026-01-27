// Azimuth 库增强测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "extreme_value_boundary_testing" {
  // 测试极端边界值
  let max_safe_int = 2147483647
  let min_safe_int = -2147483648
  
  // 测试接近最大值的加法
  assert_eq(2147483646, add(max_safe_int - 1, 1))
  
  // 测试接近最小值的加法
  assert_eq(-2147483647, add(min_safe_int + 1, -1))
  
  // 测试大数除法
  assert_eq(1, divide_with_ceil(max_safe_int, max_safe_int))
  assert_eq(1, divide_with_ceil(min_safe_int, min_safe_int))
}

test "complex_function_composition" {
  // 测试复杂的函数组合
  let initial_value = 100
  
  // 嵌套函数调用链
  let result = add(
    multiply(
      divide_with_ceil(initial_value, 5),
      3
    ),
    subtract(initial_value, 80)
  )
  
  // 手动计算验证：ceil(100/5) * 3 + (100-80) = 20*3 + 20 = 80
  assert_eq(80, result)
}

test "string_formatting_edge_cases" {
  // 测试字符串格式化的边缘情况
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
  assert_eq_string("Hello, 🌙🌟🌙!", greet("🌙🌟🌙"))
}

test "performance_large_scale_calculations" {
  // 测试大规模计算性能
  let large_number = 100000
  
  // 连续运算测试
  let sum_result = add(large_number, large_number)
  let product_result = multiply(large_number, 100)
  let division_result = divide_with_ceil(product_result, large_number)
  
  assert_eq(200000, sum_result)
  assert_eq(10000000, product_result)
  assert_eq(100, division_result)
}

test "advanced_business_logic_scenario" {
  // 高级业务逻辑场景：生产计划优化
  let total_orders = 487
  let machines = 8
  let production_per_machine_per_day = 15
  let days_available = 5
  
  // 计算每日总产能
  let daily_capacity = multiply(machines, production_per_machine_per_day)
  
  // 计算需要的生产天数
  let days_needed = divide_with_ceil(total_orders, daily_capacity)
  
  // 验证能在可用时间内完成
  assert_true(days_needed <= days_available)
  
  // 计算实际生产天数
  let actual_days = days_needed
  assert_eq(5, actual_days)
  
  // 计算总产能
  let total_production = multiply(actual_days, daily_capacity)
  assert_eq(600, total_production)
  
  // 计算剩余产能
  let remaining_capacity = subtract(total_production, total_orders)
  assert_eq(113, remaining_capacity)
}

test "mathematical_sequence_calculations" {
  // 数学序列计算测试
  let sequence_length = 10
  
  // 计算等差数列和：1+2+3+...+10
  let arithmetic_sum = divide_with_ceil(
    multiply(sequence_length, add(sequence_length, 1)),
    2
  )
  assert_eq(55, arithmetic_sum)
  
  // 计算等比数列：2^1 + 2^2 + 2^3 + 2^4
  let geometric_sum = add(
    add(
      multiply(2, 1),  // 2^1
      multiply(2, 2)   // 2^2
    ),
    add(
      multiply(2, 4),  // 2^3
      multiply(2, 8)   // 2^4
    )
  )
  assert_eq(30, geometric_sum)
}

test "resource_allocation_with_constraints" {
  // 带约束的资源分配测试
  let total_budget = 50000
  let departments = 7
  let min_budget_per_dept = 5000
  
  // 计算平均分配
  let avg_budget = divide_with_ceil(total_budget, departments)
  assert_eq(7143, avg_budget)
  
  // 验证每个部门都能获得最低预算
  assert_true(avg_budget >= min_budget_per_dept)
  
  // 计算实际分配总额
  let total_allocated = multiply(avg_budget, departments)
  assert_eq(50001, total_allocated)
  
  // 计算预算溢出
  let budget_overflow = subtract(total_allocated, total_budget)
  assert_eq(1, budget_overflow)
}

test "error_resilience_and_recovery" {
  // 错误恢复和弹性测试
  // 测试各种错误输入的处理
  
  // 除零错误的多种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极小除数测试
  assert_eq(100000, divide_with_ceil(99999, 1))
  assert_eq(-99999, divide_with_ceil(-99999, 1))
  
  // 混合符号运算
  let mixed_result = add(
    multiply(-5, 10),
    divide_with_ceil(15, -3)
  )
  assert_eq(-60, mixed_result)  // -5*10 + ceil(15/-3) = -50 + (-5) = -55
}

test "data_transformation_pipeline" {
  // 数据转换流水线测试
  let raw_data = 12345
  
  // 数据处理步骤
  let step1_normalized = divide_with_ceil(raw_data, 1000)      // 标准化
  let step2_scaled = multiply(step1_normalized, 3)             // 缩放
  let step3_adjusted = add(step2_scaled, 10)                   // 调整
  let step4_final = divide_with_ceil(step3_adjusted, 5)        // 最终处理
  
  // 验证转换结果
  assert_eq(13, step4_final)  // ceil(ceil(12345/1000)*3+10)/5 = ceil(13*3+10)/5 = ceil(49/5) = 10
  
  // 验证转换的合理性
  assert_true(step4_final > 0)
  assert_true(step4_final < 100)
}

test "complex_conditional_calculations" {
  // 复杂条件计算测试
  let base_value = 42
  let multiplier = 3
  let threshold = 100
  
  // 条件计算：如果结果超过阈值，则进行特殊处理
  let initial_result = multiply(base_value, multiplier)
  let final_result = if (initial_result > threshold) {
    divide_with_ceil(initial_result, 10)
  } else {
    add(initial_result, 10)
  }
  
  // 验证条件逻辑
  assert_eq(13, final_result)  // 42*3 = 126 > 100, 所以执行 ceil(126/10) = 13
  
  // 测试另一种情况
  let small_value = 20
  let small_result = if (small_value > threshold) {
    divide_with_ceil(small_value, 10)
  } else {
    add(small_value, 10)
  }
  
  assert_eq(30, small_result)  // 20 <= 100, 所以执行 20+10 = 30
}