// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 3))
  
  // 边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
  
  // 负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 5))
}

test "circular_reference_calculation" {
  // 测试循环引用计算场景
  let base_value = 100
  let increment = 20
  let cycles = 5
  
  // 计算循环增长：base + increment + increment*2 + ... + increment*cycles
  let inc1 = increment
  let inc2 = @azimuth.multiply(increment, 2)
  let inc3 = @azimuth.multiply(increment, 3)
  let inc4 = @azimuth.multiply(increment, 4)
  let inc5 = @azimuth.multiply(increment, 5)
  
  let total_increment = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(inc1, inc2), inc3), inc4), inc5)
  let final_value = @azimuth.add(base_value, total_increment)
  
  @azimuth.assert_eq(430, final_value)  // 100 + 20+40+60+80+100 = 430
}

test "batch_processing_simulation" {
  // 测试批处理模拟场景
  let batch_size = 50
  let num_batches = 4
  let overhead_per_batch = 5
  
  // 计算总处理量
  let total_items = @azimuth.multiply(batch_size, num_batches)  // 200
  let total_overhead = @azimuth.multiply(overhead_per_batch, num_batches)  // 20
  let effective_items = @azimuth.add(total_items, -total_overhead)  // 180
  
  @azimuth.assert_eq(200, total_items)
  @azimuth.assert_eq(20, total_overhead)
  @azimuth.assert_eq(180, effective_items)
  
  // 计算处理效率
  let efficiency = @azimuth.multiply(effective_items, 100) / total_items
  @azimuth.assert_eq(90, efficiency)  // 90% 效率
}

test "nested_conditional_calculations" {
  // 测试嵌套条件计算
  let base_score = 1000
  let performance_factor = 3
  let difficulty_level = 2
  let bonus_threshold = 2000
  
  // 计算基础得分
  let adjusted_score = @azimuth.multiply(base_score, performance_factor)  // 3000
  
  // 根据难度调整
  let difficulty_adjusted = if (difficulty_level > 1) {
    @azimuth.multiply(adjusted_score, difficulty_level)
  } else {
    adjusted_score
  }
  
  // 应用奖励
  let final_score = if (difficulty_adjusted > bonus_threshold) {
    @azimuth.add(difficulty_adjusted, 500)
  } else {
    difficulty_adjusted
  }
  
  @azimuth.assert_eq(6500, final_score)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 1000
  let priority_projects = 3
  let standard_projects = 5
  let priority_ratio = 60  // 60%
  
  // 计算优先项目分配
  let priority_allocation = @azimuth.multiply(total_resources, priority_ratio) / 100  // 600
  let priority_per_project = priority_allocation / priority_projects  // 200
  
  // 计算标准项目分配
  let remaining_resources = @azimuth.add(total_resources, -priority_allocation)  // 400
  let standard_per_project = remaining_resources / standard_projects  // 80
  
  @azimuth.assert_eq(600, priority_allocation)
  @azimuth.assert_eq(200, priority_per_project)
  @azimuth.assert_eq(400, remaining_resources)
  @azimuth.assert_eq(80, standard_per_project)
}

test "string_pattern_processing" {
  // 测试字符串模式处理
  let prefix = "User"
  let user_id = "12345"
  let suffix = "Active"
  
  // 使用greet函数模拟字符串模式处理
  let full_prefix = @azimuth.greet(prefix)
  let with_id = @azimuth.greet(user_id)
  let with_suffix = @azimuth.greet(suffix)
  
  // 验证各部分
  @azimuth.assert_eq_string("Hello, User!", full_prefix)
  @azimuth.assert_eq_string("Hello, 12345!", with_id)
  @azimuth.assert_eq_string("Hello, Active!", with_suffix)
  
  // 测试特殊模式
  let empty_pattern = @azimuth.greet("")
  let numeric_pattern = @azimuth.greet("2023-12-31")
  let mixed_pattern = @azimuth.greet("ABC_xyz_123")
  
  @azimuth.assert_eq_string("Hello, !", empty_pattern)
  @azimuth.assert_eq_string("Hello, 2023-12-31!", numeric_pattern)
  @azimuth.assert_eq_string("Hello, ABC_xyz_123!", mixed_pattern)
}

test "mathematical_series_evaluation" {
  // 测试数学级数评估
  // 计算斐波那契数列前几项的和：1, 1, 2, 3, 5, 8, 13, 21
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  let fib7 = 13
  let fib8 = 21
  
  let fib_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8)
  @azimuth.assert_eq(54, fib_sum)
  
  // 计算等比数列：2, 4, 8, 16, 32
  let geo1 = 2
  let geo2 = @azimuth.multiply(geo1, 2)  // 4
  let geo3 = @azimuth.multiply(geo2, 2)  // 8
  let geo4 = @azimuth.multiply(geo3, 2)  // 16
  let geo5 = @azimuth.multiply(geo4, 2)  // 32
  
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(geo1, geo2), geo3), geo4), geo5)
  @azimuth.assert_eq(62, geo_sum)
}

test "extreme_boundary_stress_test" {
  // 测试极值边界压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续极值运算
  let step1 = @azimuth.add(max_val, min_val)  // -1
  let step2 = @azimuth.multiply(step1, max_val)  // -2147483647
  let step3 = @azimuth.add(step2, max_val)  // 0
  let step4 = @azimuth.multiply(step3, max_val)  // 0
  
  @azimuth.assert_eq(-1, step1)
  @azimuth.assert_eq(-2147483647, step2)
  @azimuth.assert_eq(0, step3)
  @azimuth.assert_eq(0, step4)
  
  // 极值与普通值的混合运算
  let mixed1 = @azimuth.add(max_val, 1000000)
  let mixed2 = @azimuth.add(min_val, -1000000)
  let mixed3 = @azimuth.multiply(max_val, 100)
  let mixed4 = @azimuth.multiply(min_val, 100)
  
  @azimuth.assert_eq(max_val, mixed1)
  @azimuth.assert_eq(min_val, mixed2)
  @azimuth.assert_eq(max_val, mixed3)
  @azimuth.assert_eq(min_val, mixed4)
}