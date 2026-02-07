// 高质量增强测试套件 - 标准 MoonBit 测试用例
// 为 Azimuth 项目提供全面的质量保证测试

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 17
  let b = 29
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体计算结果
  @azimuth.assert_eq(46, @azimuth.add(17, 29))
  @azimuth.assert_eq(493, @azimuth.multiply(17, 29))
}

test "mathematical_associative_laws" {
  // 数学结合律验证测试
  let a = 5
  let b = 8
  let c = 12
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
}

test "mathematical_distributive_law" {
  // 数学分配律验证测试: a * (b + c) = a * b + a * c
  let a = 7
  let b = 9
  let c = 13
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证具体数值: 7 * (9 + 13) = 7 * 9 + 7 * 13 = 154
  @azimuth.assert_eq(154, left_side)
}

test "warehouse_packaging_optimization" {
  // 仓库包装优化业务场景测试
  let total_products = 247
  let box_capacity = 18
  let pallet_capacity = 6  // 每个托盘可放6个箱子
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(14, boxes_needed)  // ceil(247/18) = 14
  
  // 计算需要的托盘数量
  let pallets_needed = @azimuth.divide_with_ceil(boxes_needed, pallet_capacity)
  @azimuth.assert_eq(3, pallets_needed)  // ceil(14/6) = 3
  
  // 验证总容量
  let total_box_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(252, total_box_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_box_capacity, total_products)
  @azimuth.assert_eq(5, remaining_space)
}

test "project_resource_calculation" {
  // 项目资源计算复杂工作流测试
  let team_members = 9
  let work_hours_per_day = 8
  let working_days = 22
  let tasks_per_member_per_day = 3
  let story_points_per_task = 5
  
  // 计算总工作小时数
  let total_work_hours = @azimuth.multiply(
    team_members, 
    @azimuth.multiply(work_hours_per_day, working_days)
  )
  @azimuth.assert_eq(1584, total_work_hours)  // 9 * 8 * 22 = 1584
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(
    team_members, 
    @azimuth.multiply(tasks_per_member_per_day, working_days)
  )
  @azimuth.assert_eq(594, total_tasks)  // 9 * 3 * 22 = 594
  
  // 计算总故事点数
  let total_story_points = @azimuth.multiply(total_tasks, story_points_per_task)
  @azimuth.assert_eq(2970, total_story_points)  // 594 * 5 = 2970
  
  // 计算需要的冲刺数（每个冲刺400故事点）
  let sprints_needed = @azimuth.divide_with_ceil(total_story_points, 400)
  @azimuth.assert_eq(8, sprints_needed)  // ceil(2970/400) = 8
}

test "internationalization_greeting_tests" {
  // 国际化问候函数测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, München!", @azimuth.greet("München"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
}

test "boundary_conditions_and_extreme_values" {
  // 边界条件和极值测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 12345))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(12345, 0))
  
  // 单位值测试
  @azimuth.assert_eq(12345, @azimuth.add(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(12345, 12345))
  
  // 大数运算测试
  let large_num = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(10000000000, @azimuth.multiply(large_num, large_num))
  @azimuth.assert_eq(100001, @azimuth.divide_with_ceil(100000001, large_num))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(999999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-999999, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 负数除法测试
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-25, 5))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(25, -5))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(-25, -5))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-19, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(19, -7))
  
  // 特殊字符串处理测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 模拟一个完整的业务计算流程
  
  // 初始参数
  let initial_inventory = 150
  let daily_production = 25
  let operating_days = 15
  let packaging_capacity = 40
  let shipping_batches = 3
  
  // 计算总产量
  let total_production = @azimuth.multiply(daily_production, operating_days)
  @azimuth.assert_eq(375, total_production)  // 25 * 15 = 375
  
  // 计算总库存
  let total_inventory = @azimuth.add(initial_inventory, total_production)
  @azimuth.assert_eq(525, total_inventory)  // 150 + 375 = 525
  
  // 计算需要的包装批次数
  let packaging_batches = @azimuth.divide_with_ceil(total_inventory, packaging_capacity)
  @azimuth.assert_eq(14, packaging_batches)  // ceil(525/40) = 14
  
  // 计算运输次数
  let shipping_trips = @azimuth.divide_with_ceil(packaging_batches, shipping_batches)
  @azimuth.assert_eq(5, shipping_trips)  // ceil(14/3) = 5
  
  // 验证总运输能力
  let total_shipping_capacity = @azimuth.multiply(shipping_trips, shipping_batches)
  @azimuth.assert_eq(15, total_shipping_capacity)  // 5 * 3 = 15
  
  // 验证剩余运输能力
  let remaining_capacity = @azimuth.subtract(total_shipping_capacity, packaging_batches)
  @azimuth.assert_eq(1, remaining_capacity)  // 15 - 14 = 1
}