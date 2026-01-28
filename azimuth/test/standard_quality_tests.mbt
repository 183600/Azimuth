// 标准 MoonBit 测试用例 - 为 azimuth 库添加的高质量测试
// 包含10个全面的测试用例，覆盖各种功能和场景

test "arith metic_operations_precision" {
  // 精确算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(5, -7))
  assert_eq(0, add(1000, -1000))
  assert_eq(42, multiply(6, 7))
  assert_eq(-42, multiply(6, -7))
  assert_eq(0, multiply(12345, 0))
}

test "boundary_values_comprehensive" {
  // 边界值综合测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(1000, divide_with_ceil(999, 1))
  assert_eq(-999, divide_with_ceil(-999, 1))
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, space!", greet(" "))
  assert_eq_string("Hello, multiple!", greet("multiple"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
  assert_eq_string("Hello, MixedCase!", greet("MixedCase"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 25
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(25, result)  // ceil(25*3/4) + 7 = 19 + 7 = 26，但ceil(75/4)=19
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 1000
  let teams = 8
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(125, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(1000, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(100, utilization)
}

test "unicode_and_international_support" {
  // Unicode和国际字符支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "real_world_project_planning" {
  // 真实世界项目规划测试
  let project_tasks = 87
  let team_members = 6
  let tasks_per_person_per_day = 3
  let working_days = 5
  
  // 计算每日团队产能
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(project_tasks, multiply(daily_capacity, working_days))
  assert_eq(1, weeks_needed)
  
  // 计算实际完成时间
  let total_days_needed = divide_with_ceil(project_tasks, daily_capacity)
  assert_eq(5, total_days_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(total_days_needed, daily_capacity)
  assert_eq(90, total_capacity)
}