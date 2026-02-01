// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

// 导入 azimuth 模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(7, azimuth.add(3, 4))
  assert_eq(12, azimuth.multiply(3, 4))
  assert_eq(2, azimuth.subtract(10, 8))
  assert_eq(4, azimuth.divide_with_ceil(15, 4))
  assert_eq(3, azimuth.divide_with_ceil(12, 4))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(azimuth.add(x, y), azimuth.add(y, x))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(x, y), azimuth.multiply(y, x))
  
  // 加法结合律
  assert_eq(
    azimuth.add(azimuth.add(x, y), 5),
    azimuth.add(x, azimuth.add(y, 5))
  )
  
  // 乘法结合律
  assert_eq(
    azimuth.multiply(azimuth.multiply(x, y), 2),
    azimuth.multiply(x, azimuth.multiply(y, 2))
  )
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(5, azimuth.subtract(5, 0))
  assert_eq(-5, azimuth.subtract(0, 5))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-2, azimuth.add(-5, 3))
  assert_eq(-15, azimuth.multiply(-5, 3))
  assert_eq(15, azimuth.multiply(-5, -3))
  assert_eq(-1, azimuth.divide_with_ceil(-3, 4))
  assert_eq(1, azimuth.divide_with_ceil(-3, -4))
  assert_eq(-8, azimuth.subtract(-5, 3))
  assert_eq(8, azimuth.subtract(3, -5))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, émoji 🚀!", azimuth.greet("émoji 🚀"))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = azimuth.divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = azimuth.multiply(base_value, multiplier)
  let step2 = azimuth.divide_with_ceil(step1, divisor)
  let result = azimuth.add(step2, addition)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = azimuth.divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = azimuth.multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 计算剩余资源
  let remaining_resources = azimuth.subtract(allocated_total, total_resources)
  assert_eq(2, remaining_resources)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 除零错误处理
  assert_eq(0, azimuth.divide_with_ceil(10, 0))
  assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1000000, azimuth.multiply(1000, 1000))
  assert_eq(2000000, azimuth.add(1000000, 1000000))
  assert_eq(0, azimuth.subtract(1000000, 1000000))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = azimuth.divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = azimuth.multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = azimuth.subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(available_seats >= participants)
}