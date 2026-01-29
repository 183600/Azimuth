// 新的标准 MoonBit 测试用例 - 10个高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 零元素性质
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  let wasted_space = @azimuth.add(total_capacity, -total_items)
  @azimuth.assert_eq(1, wasted_space)
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-7, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -3))
}

test "boundary_value_conditions" {
  // 边界值条件测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  let multiplier = 3
  let divisor = 8
  let addition = 5
  
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(13, result)  // ceil(20*3/8) + 5 = 8 + 5 = 13
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 1000
  let departments = 3
  
  let budget_per_department = @azimuth.divide_with_ceil(total_budget, departments)
  @azimuth.assert_eq(334, budget_per_department)  // ceil(1000/3) = 334
  
  let total_allocated = @azimuth.multiply(budget_per_department, departments)
  @azimuth.assert_eq(1002, total_allocated)
  
  let overhead = @azimuth.add(total_allocated, -total_budget)
  @azimuth.assert_eq(2, overhead)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(15, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-15, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let tables_available = 10
  let chairs_per_table = 8
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_chairs)
  
  let empty_chairs = @azimuth.add(total_chairs, -participants)
  @azimuth.assert_eq(1, empty_chairs)
  
  // 验证桌子是否足够
  @azimuth.assert_true(tables_needed <= tables_available)
}