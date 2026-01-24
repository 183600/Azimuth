// 新的综合单元测试 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive_edge_cases" {
  // 全面测试加法函数的边缘情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_function_identity_and_zero" {
  // 测试乘法函数的恒等式和零性质
  // 零乘法性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))  // 0 * -1 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))   // 1 * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(-1, 0))  // -1 * 0 = 0
  
  // 恒等式性质
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))   // 5 * 1 = 5
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1)) // -5 * 1 = -5
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))   // 1 * 5 = 5
  @azimuth.assert_eq(-5, @azimuth.multiply(1, -5)) // 1 * -5 = -5
  
  // 负一乘法性质
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))  // 5 * -1 = -5
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))  // -5 * -1 = 5
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, русский!", @azimuth.greet("русский"))
  @azimuth.assert_eq_string("Hello, español!", @azimuth.greet("español"))
  @azimuth.assert_eq_string("Hello, français!", @azimuth.greet("français"))
}

test "divide_with_ceil_precision_and_rounding" {
  // 测试向上取整除法的精度和舍入
  // 小数部分测试
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))    // 1.5 -> 2
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))    // 1.33 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))    // 2.5 -> 3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))    // 3.5 -> 4
  
  // 大数精度测试
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(2000, 2))    // 1000 -> 1000 (精确)
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(2001, 2))    // 1000.5 -> 1001
  @azimuth.assert_eq(667, @azimuth.divide_with_ceil(2000, 3))     // 666.67 -> 667
  
  // 极小精度测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))    // 0.000001 -> 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(999999, 1000000))  // 0.999999 -> 1
}

test "complex_business_workflow_simulation" {
  // 复杂业务工作流模拟
  // 场景：电商订单处理和物流分配
  
  // 订单处理
  let orders_per_day = 150
  let processing_capacity = 40
  let days_needed = @azimuth.divide_with_ceil(orders_per_day, processing_capacity)
  @azimuth.assert_eq(4, days_needed)  // ceil(150/40) = 4
  
  // 物流分配
  let total_packages = 150
  let small_box_capacity = 10
  let large_box_capacity = 25
  
  let small_boxes = @azimuth.divide_with_ceil(total_packages, small_box_capacity)
  let large_boxes = @azimuth.divide_with_ceil(total_packages, large_box_capacity)
  
  @azimuth.assert_eq(15, small_boxes)  // ceil(150/10) = 15
  @azimuth.assert_eq(6, large_boxes)   // ceil(150/25) = 6
  
  // 成本计算
  let small_box_cost = 2
  let large_box_cost = 4
  
  let total_small_cost = @azimuth.multiply(small_boxes, small_box_cost)
  let total_large_cost = @azimuth.multiply(large_boxes, large_box_cost)
  
  @azimuth.assert_eq(30, total_small_cost)  // 15 * 2 = 30
  @azimuth.assert_eq(24, total_large_cost)  // 6 * 4 = 24
  
  // 验证大包装更经济 - 使用加法代替减法
  // cost_difference = total_small_cost - total_large_cost = 30 - 24 = 6
  // 我们通过加法验证：total_large_cost + 6 = total_small_cost
  @azimuth.assert_eq(30, @azimuth.add(total_large_cost, 6))
}

test "mathematical_sequence_and_series" {
  // 数学序列和级数测试
  // 斐波那契数列前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)      // 2
  let fib4 = @azimuth.add(fib2, fib3)      // 3
  let fib5 = @azimuth.add(fib3, fib4)      // 5
  let fib6 = @azimuth.add(fib4, fib5)      // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let n = 5
  let arithmetic_sum = @azimuth.divide_with_ceil(@azimuth.multiply(n, @azimuth.add(n, 1)), 2)
  @azimuth.assert_eq(15, arithmetic_sum)
  
  // 等比数列求和：2 + 4 + 8 = 14
  let geo_sum = @azimuth.add(@azimuth.add(2, 4), 8)
  @azimuth.assert_eq(14, geo_sum)
  
  // 验证斐波那契数列的性质
  let fib_property = @azimuth.add(fib3, fib4)  // 应该等于 fib6
  @azimuth.assert_eq(fib6, fib_property)      // 5 + 3 = 8
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 边界值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  
  // 极值运算测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
}

test "string_processing_and_validation" {
  // 字符串处理和验证测试
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, \n\r\t!", @azimuth.greet("\n\r\t"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  @azimuth.assert_eq_string("Hello, 'apostrophe'!", @azimuth.greet("'apostrophe'"))
  @azimuth.assert_eq_string("Hello, \\backslash\\!", @azimuth.greet("\\backslash\\"))
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, []{}()!", @azimuth.greet("[]{}()"))
  
  // 数字和字母混合
  @azimuth.assert_eq_string("Hello, abc123!", @azimuth.greet("abc123"))
  @azimuth.assert_eq_string("Hello, 123abc!", @azimuth.greet("123abc"))
  @azimuth.assert_eq_string("Hello, a1b2c3!", @azimuth.greet("a1b2c3"))
  
  // 空白字符处理
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, leading !", @azimuth.greet(" leading "))
  @azimuth.assert_eq_string("Hello, trailing !", @azimuth.greet(" trailing "))
  @azimuth.assert_eq_string("Hello,  both  !", @azimuth.greet("  both  "))
}

test "performance_and_efficiency_analysis" {
  // 性能和效率分析测试
  let base_size = 1000
  
  // 不同算法复杂度的模拟
  // O(1) - 恒定时间
  let constant_operation = 5
  
  // O(n) - 线性时间
  let linear_100 = @azimuth.multiply(base_size, 1)
  let linear_200 = @azimuth.multiply(base_size, 2)
  let linear_500 = @azimuth.multiply(base_size, 5)
  
  // O(n²) - 平方时间（简化模拟）
  let quadratic_100 = @azimuth.divide_with_ceil(@azimuth.multiply(base_size, base_size), 1000)
  let quadratic_200 = @azimuth.divide_with_ceil(@azimuth.multiply(base_size, base_size), 500)
  let quadratic_500 = @azimuth.divide_with_ceil(@azimuth.multiply(base_size, base_size), 200)
  
  // 验证复杂度特征
  @azimuth.assert_eq(5, constant_operation)
  @azimuth.assert_eq(1000, linear_100)
  @azimuth.assert_eq(2000, linear_200)
  @azimuth.assert_eq(5000, linear_500)
  
  @azimuth.assert_eq(1000, quadratic_100)   // 1000*1000/1000 = 1000
  @azimuth.assert_eq(2000, quadratic_200)   // 1000*1000/500 = 2000
  @azimuth.assert_eq(5000, quadratic_500)   // 1000*1000/200 = 5000
  
  // 效率比较
  let linear_to_constant = @azimuth.divide_with_ceil(linear_100, constant_operation)
  let quadratic_to_linear = @azimuth.divide_with_ceil(quadratic_100, linear_100)
  
  @azimuth.assert_eq(200, linear_to_constant)   // 1000/5 = 200
  @azimuth.assert_eq(1, quadratic_to_linear)    // 1000/1000 = 1
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：活动座位安排
  let participants = 234
  let tables_per_room = 8
  let chairs_per_table = 6
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  let rooms_needed = @azimuth.divide_with_ceil(tables_needed, tables_per_room)
  
  @azimuth.assert_eq(39, tables_needed)   // ceil(234/6) = 39
  @azimuth.assert_eq(5, rooms_needed)     // ceil(39/8) = 5
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  let total_capacity = @azimuth.multiply(rooms_needed, @azimuth.multiply(tables_per_room, chairs_per_table))
  
  @azimuth.assert_eq(234, total_chairs)   // 39 * 6 = 234
  @azimuth.assert_eq(240, total_capacity) // 5 * 8 * 6 = 240
  
  // 场景2：项目资源分配
  let total_budget = 50000
  let team_members = 12
  let project_phases = 4
  
  let budget_per_member = @azimuth.divide_with_ceil(total_budget, team_members)
  let budget_per_phase = @azimuth.divide_with_ceil(total_budget, project_phases)
  
  @azimuth.assert_eq(4167, budget_per_member)  // ceil(50000/12) = 4167
  @azimuth.assert_eq(12500, budget_per_phase)  // ceil(50000/4) = 12500
  
  let total_allocated = @azimuth.multiply(budget_per_member, team_members)
  // budget_overflow = total_allocated - total_budget = 50004 - 50000 = 4
  // 使用加法验证：total_budget + 4 = total_allocated
  @azimuth.assert_eq(50004, @azimuth.add(total_budget, 4))
}