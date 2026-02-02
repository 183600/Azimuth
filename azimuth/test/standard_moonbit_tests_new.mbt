// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(2, @azimuth.subtract(5, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(19, @azimuth.add(12, 7))
  @azimuth.assert_eq(84, @azimuth.multiply(12, 7))
}

test "negative_numbers_handling" {
  // 负数处理测试
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-3, -4))
  @azimuth.assert_eq(-8, @azimuth.subtract(-5, 3))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
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

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 除法边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-5, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  
  // 零元素性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = @azimuth.subtract(available_seats, participants)
  @azimuth.assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  @azimuth.assert_true(available_seats >= participants)
}