// Azimuth 标准增强测试用例 - 8个高质量测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能和边界情况

test "add_function_comprehensive_test" {
  // 加法函数综合测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  @azimuth.assert_eq(100000, @azimuth.add(50000, 50000))
}

test "multiply_function_various_scenarios" {
  // 乘法函数各种场景测试
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-24, @azimuth.multiply(-3, 8))
  @azimuth.assert_eq(24, @azimuth.multiply(-3, -8))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let a = 23
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 复杂表达式交换律验证
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3)),
    @azimuth.add(@azimuth.multiply(b, 3), @azimuth.multiply(a, 2))
  )
}

test "subtract_function_edge_cases" {
  // 减法函数边缘情况测试
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(7, 7))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
  @azimuth.assert_eq(-15, @azimuth.subtract(-10, 5))
}

test "business_inventory_management" {
  // 业务库存管理测试
  let total_products = 158
  let box_capacity = 25
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(175, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(17, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 12
  let weeks_per_sprint = 2
  let hours_per_week = 40
  let total_task_hours = 800
  
  // 计算团队每周工时
  let weekly_capacity = @azimuth.multiply(team_members, hours_per_week)
  @azimuth.assert_eq(480, weekly_capacity)
  
  // 计算每个冲刺的工时
  let sprint_capacity = @azimuth.multiply(weekly_capacity, weeks_per_sprint)
  @azimuth.assert_eq(960, sprint_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_task_hours, sprint_capacity)
  @azimuth.assert_eq(1, sprints_needed)
  
  // 验证总可用工时
  let total_available = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(960, total_available)
  
  // 验证剩余工时
  let remaining_hours = @azimuth.subtract(total_available, total_task_hours)
  @azimuth.assert_eq(160, remaining_hours)
}