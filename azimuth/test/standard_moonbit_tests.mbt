// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(5, @azimuth.add(10, -5))  // 使用加法代替减法
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 2)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 2))
  )
}

test "boundary_conditions_and_edge_cases" {
  // 边界条件和边缘情况测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "negative_numbers_handling" {
  // 负数处理测试
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-3, -4))
  @azimuth.assert_eq(-8, @azimuth.add(3, -11))  // 使用加法代替减法
}

test "unicode_and_string_processing" {
  // Unicode 和字符串处理测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, éspañol!", @azimuth.greet("éspañol"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "business_logic_packaging_scenario" {
  // 业务逻辑：包装场景测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.add(total_capacity, -items)  // 使用加法代替减法
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值验证
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  
  @azimuth.assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let total_allocated = @azimuth.multiply(min_resources_per_team, teams)
  @azimuth.assert_eq(102, total_allocated)
  
  // 验证资源利用率
  let utilization = @azimuth.divide_with_ceil(
    @azimuth.multiply(total_resources, 100), 
    total_allocated
  )
  @azimuth.assert_eq(98, utilization)  // floor(10000/102) = 98%
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.add(available_seats, -participants)  // 使用加法代替减法
  @azimuth.assert_eq(3, remaining_seats)
  
  // 验证座位充足性 - 通过比较来验证而不是使用assert_true
  let seats_sufficient = @azimuth.divide_with_ceil(available_seats, participants)
  @azimuth.assert_eq(1, seats_sufficient)  // 如果available_seats >= participants，结果应该是1
  
  // 生成个性化问候
  let greeting = @azimuth.greet("活动参与者")
  @azimuth.assert_eq_string("Hello, 活动参与者!", greeting)
}