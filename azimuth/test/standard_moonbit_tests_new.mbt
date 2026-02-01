// 标准 MoonBit 测试用例 - 新增8个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖各种场景

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-4, @azimuth.subtract(3, 7))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(10, 5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律和结合律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), 5), @azimuth.add(a, @azimuth.add(b, 5)))
  
  // 乘法交换律和分配律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "boundary_values_and_limits" {
  // 边界值和限制测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  
  // 单位值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(6, @azimuth.add(5, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-3, 7))
  @azimuth.assert_eq(-4, @azimuth.add(3, -7))
  
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(10, -5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-10, -5))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, éspañol!", @azimuth.greet("éspañol"))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库库存管理
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(10, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(48, total_tasks)  // 8 * 6 = 48
  @azimuth.assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证总处理能力
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  let extra_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(12, extra_capacity)  // 60 - 48 = 12
}

test "error_handling_edge_cases" {
  // 错误处理边缘情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 极值测试
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  
  // 边界除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
}