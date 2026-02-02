// Azimuth 标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(2, subtract(10, 8))
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

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  // 加法负数测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-5, 7))
  assert_eq(-10, add(-5, -5))
  
  // 乘法负数测试
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  
  // 除法负数测试
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "string_processing_and_greeting" {
  // 字符串处理和问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "business_logic_packaging_optimization" {
  // 业务逻辑：包装优化计算测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(
    multiply(total_items, 100), 
    total_capacity
  )
  assert_eq(98, utilization_percent)  // floor(4700/48) = 97%
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let days_per_sprint = 5
  let total_days = multiply(sprints_needed, days_per_sprint)
  
  // 验证计算结果
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  assert_eq(20, total_days)  // 4 * 5 = 20
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 特殊字符串处理
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}