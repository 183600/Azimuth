// Azimuth 项目新增的综合测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "mathematical_commutative_and_associative_laws" {
  // 测试数学交换律和结合律
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "distributive_property_validation" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 5 * (8 + 3) = 5 * 8 + 5 * 3 = 55
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景：项目资源规划
  let team_members = 12
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_members, @azimuth.multiply(hours_per_day, working_days))
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(total_hours, tasks_per_hour)
  
  // 计算需要的冲刺数（每个冲刺最多100个任务）
  let tasks_per_sprint = 100
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  
  @azimuth.assert_eq(1440, total_hours)   // 12 * 8 * 15 = 1440
  @azimuth.assert_eq(2880, total_tasks)  // 1440 * 2 = 2880
  @azimuth.assert_eq(29, sprints_needed) // ceil(2880/100) = 29
}

test "warehouse_inventory_management" {
  // 仓库库存管理场景
  let total_items = 487
  let box_capacity = 24
  let pallet_capacity = 12  // 每个托盘放多少箱
  
  // 计算需要的箱子数
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  
  // 计算需要的托盘数
  let pallets_needed = @azimuth.divide_with_ceil(boxes_needed, pallet_capacity)
  
  // 验证计算结果
  @azimuth.assert_eq(21, boxes_needed)   // ceil(487/24) = 21
  @azimuth.assert_eq(2, pallets_needed)  // ceil(21/12) = 2
  
  // 验证总容量
  let total_box_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let total_pallet_capacity = @azimuth.multiply(pallets_needed, @azimuth.multiply(pallet_capacity, box_capacity))
  
  @azimuth.assert_eq(504, total_box_capacity)    // 21 * 24 = 504
  @azimuth.assert_eq(576, total_pallet_capacity) // 2 * 12 * 24 = 576
}

test "mathematical_identity_elements" {
  // 测试数学恒等元素
  let x = 42
  
  // 加法恒等元素：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等元素：x * 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零元素：x * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
  
  // 减法恒等：x - x = 0, x - 0 = x
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(x, @azimuth.subtract(x, 0))
}

test "internationalization_and_unicode_support" {
  // 国际化和Unicode支持测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 极值测试
  let very_large_number = 1000000
  let very_small_number = 1
  
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(very_large_number, very_small_number))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(very_small_number, very_large_number))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(very_large_number, very_large_number))
  
  // 负数边界情况
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 1000000))
}

test "nested_function_composition" {
  // 嵌套函数组合测试
  let base_value = 8
  
  // 复杂的嵌套计算：ceil((base * 3 + base * 2) / (base + ceil(base/3)))
  let nested_result = @azimuth.divide_with_ceil(
    @azimuth.add(@azimuth.multiply(base_value, 3), @azimuth.multiply(base_value, 2)),
    @azimuth.add(base_value, @azimuth.divide_with_ceil(base_value, 3))
  )
  
  // 手动计算验证：(8*3 + 8*2) / (8 + ceil(8/3)) = 40 / 11 = ceil(3.63) = 4
  @azimuth.assert_eq(4, nested_result)
  
  // 更复杂的嵌套计算
  let complex_result = @azimuth.add(
    @azimuth.multiply(
      @azimuth.divide_with_ceil(@azimuth.add(base_value, 5), 3),
      @azimuth.subtract(base_value, 2)
    ),
    @azimuth.divide_with_ceil(@azimuth.multiply(base_value, 2), @azimuth.add(base_value, 1))
  )
  
  // 手动计算：ceil((8+5)/3) * (8-2) + ceil(8*2/(8+1)) = ceil(13/3) * 6 + ceil(16/9) = 5 * 6 + 2 = 32
  @azimuth.assert_eq(32, complex_result)
}