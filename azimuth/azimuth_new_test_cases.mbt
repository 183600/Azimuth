// Azimuth 项目新增测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(1, divide_with_ceil(3, 4))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 验证实际分配的资源利用率
  let utilization_percent = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(98, utilization_percent)  // floor(10000/102) = 98%
}

test "error_handling_validation" {
  // 错误处理和验证测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算验证
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "real_world_event_planning" {
  // 真实世界应用场景：活动规划测试
  let participants = 87
  let tables_available = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算总座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(available_seats >= participants)
  
  // 验证桌子可用性
  assert_true(tables_needed <= tables_available)
}