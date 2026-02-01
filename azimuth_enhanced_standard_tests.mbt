// Azimuth 增强标准测试用例
// 包含10个高质量 MoonBit 测试用例，使用标准测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-3, @azimuth.subtract(5, 8))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(15, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(12, 4))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 5
  
  // 加法交换律和结合律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), 3), @azimuth.add(x, @azimuth.add(y, 3)))
  
  // 乘法交换律和结合律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), 2), @azimuth.multiply(x, @azimuth.multiply(y, 2)))
  
  // 分配律验证
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "boundary_and_extreme_values" {
  // 边界值和极值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 999))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(888, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-2, @azimuth.add(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-7, -4))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库存储优化
  let total_items = 158
  let box_capacity = 25
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总存储容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(17, remaining_space)
  
  // 验证空间利用率
  let utilization_rate = @azimuth.divide_with_ceil(@azimuth.multiply(total_items, 100), total_capacity)
  @azimuth.assert_eq(91, utilization_rate)  // floor(15800/175) = 90%
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let daily_capacity = 15
  let sprint_capacity = @azimuth.multiply(sprint_duration, daily_capacity)
  
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)      // 8 * 6 = 48
  @azimuth.assert_eq(30, sprint_capacity)  // 2 * 15 = 30
  @azimuth.assert_eq(2, sprints_needed)    // ceil(48/30) = 2
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(60, total_capacity)
  
  // 验证额外容量
  let extra_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(12, extra_capacity)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 极值运算测试
  let large_positive = 500000
  let large_negative = -500000
  
  @azimuth.assert_eq(1000000, @azimuth.add(large_positive, large_positive))
  @azimuth.assert_eq(-1000000, @azimuth.add(large_negative, large_negative))
  @azimuth.assert_eq(0, @azimuth.add(large_positive, large_negative))
  
  @azimuth.assert_eq(250000000, @azimuth.multiply(500, 500000))
  @azimuth.assert_eq(250000000, @azimuth.multiply(-500, -500000))
  @azimuth.assert_eq(-250000000, @azimuth.multiply(500, -500000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议座位安排
  let attendees = 234
  let tables_available = 20
  let chairs_per_table = 12
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(20, tables_needed)
  
  // 检查桌子是否足够
  @azimuth.assert_eq(1, if tables_needed <= tables_available { 1 } else { 0 })
  
  // 计算总座位数
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(240, total_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(total_seats, attendees)
  @azimuth.assert_eq(6, remaining_seats)
  
  // 计算座位利用率
  let seat_utilization = @azimuth.divide_with_ceil(@azimuth.multiply(attendees, 100), total_seats)
  @azimuth.assert_eq(98, seat_utilization)  // floor(23400/240) = 97%
}

test "algorithm_performance_validation" {
  // 算法性能验证测试
  // 模拟不同算法复杂度的性能特征
  let data_size = 10000
  
  // O(n) 线性时间算法
  let linear_time_ops = @azimuth.multiply(data_size, 3)
  @azimuth.assert_eq(30000, linear_time_ops)
  
  // O(log n) 对数时间算法（简化模拟）
  let logarithmic_ops = @azimuth.divide_with_ceil(data_size, 1000)
  @azimuth.assert_eq(10, logarithmic_ops)
  
  // O(n²) 平方时间算法（小数据集）
  let small_data_size = 100
  let quadratic_ops = @azimuth.multiply(small_data_size, small_data_size)
  @azimuth.assert_eq(10000, quadratic_ops)
  
  // 性能比较
  let linear_vs_log_ratio = @azimuth.divide_with_ceil(linear_time_ops, logarithmic_ops)
  @azimuth.assert_eq(3000, linear_vs_log_ratio)
  
  let quadratic_vs_linear_ratio = @azimuth.divide_with_ceil(quadratic_ops, @azimuth.multiply(small_data_size, 3))
  @azimuth.assert_eq(34, quadratic_vs_linear_ratio)  // ceil(10000/300) = 34
}