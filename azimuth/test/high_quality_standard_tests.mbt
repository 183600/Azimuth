// 高质量标准 MoonBit 测试用例
// 为 Azimuth 项目创建的10个综合测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
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
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, offset)
  
  @azimuth.assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let projects = 3
  let min_resources_per_project = @azimuth.divide_with_ceil(total_resources, projects)
  
  @azimuth.assert_eq(34, min_resources_per_project)
  
  let total_allocated = @azimuth.multiply(min_resources_per_project, projects)
  @azimuth.assert_eq(102, total_allocated)
  
  let resource_overhead = @azimuth.subtract(total_allocated, total_resources)
  @azimuth.assert_eq(2, resource_overhead)
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 极值测试
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  
  // 除法边界
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let chairs_per_table = 10
  let cost_per_chair = 15
  
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(9, tables_needed)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(90, total_chairs)
  
  let empty_chairs = @azimuth.subtract(total_chairs, participants)
  @azimuth.assert_eq(3, empty_chairs)
  
  let total_cost = @azimuth.multiply(total_chairs, cost_per_chair)
  @azimuth.assert_eq(1350, total_cost)
  
  let cost_per_person = @azimuth.divide_with_ceil(total_cost, participants)
  @azimuth.assert_eq(16, cost_per_person)
}