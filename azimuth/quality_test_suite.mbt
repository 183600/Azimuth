// 高质量 MoonBit 测试用例 - Azimuth 项目
// 包含10个标准测试用例，涵盖基本功能、边界情况和实际应用场景

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 7
  
  // 加法交换律和结合律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), 3), @azimuth.add(x, @azimuth.add(y, 3)))
  
  // 乘法交换律和结合律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), 2), @azimuth.multiply(x, @azimuth.multiply(y, 2)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-7, -4))
}

test "boundary_values_extreme" {
  // 边界值和极值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
}

test "string_processing_various" {
  // 字符串处理各种情况测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(11, remaining_space)
}

test "project_resource_calculation" {
  // 项目资源计算测试
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)
  @azimuth.assert_eq(3, sprints_needed)
  
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let extra_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(12, extra_capacity)
}

test "error_handling_edge_cases" {
  // 错误处理和边界情况测试
  // 除零情况的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_num = 100000
  @azimuth.assert_eq(0, @azimuth.multiply(large_num, 0))
  @azimuth.assert_eq(large_num, @azimuth.add(large_num, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num, large_num))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：活动策划
  let attendees = 250
  let tables = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  
  let tables_needed = @azimuth.divide_with_ceil(attendees, chairs_per_table)
  @azimuth.assert_eq(32, tables_needed)
  
  @azimuth.assert_true(tables_needed <= tables)
  
  let total_chairs = @azimuth.multiply(tables_needed, chairs_per_table)
  let total_cost = @azimuth.multiply(total_chairs, cost_per_chair)
  
  @azimuth.assert_eq(256, total_chairs)
  @azimuth.assert_eq(6400, total_cost)
  
  let empty_chairs = @azimuth.subtract(total_chairs, attendees)
  @azimuth.assert_eq(6, empty_chairs)
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试
  let initial = 5
  
  let step1 = @azimuth.add(initial, @azimuth.multiply(2, 3))
  let step2 = @azimuth.divide_with_ceil(step1, 4)
  let step3 = @azimuth.multiply(step2, @azimuth.add(1, 2))
  let step4 = @azimuth.subtract(step3, 1)
  let final_result = @azimuth.divide_with_ceil(step4, 2)
  
  @azimuth.assert_eq(11, step1)  // 5 + 2*3 = 11
  @azimuth.assert_eq(3, step2)   // ceil(11/4) = 3
  @azimuth.assert_eq(9, step3)   // 3 * (1+2) = 9
  @azimuth.assert_eq(8, step4)   // 9 - 1 = 8
  @azimuth.assert_eq(4, final_result)  // ceil(8/2) = 4
}