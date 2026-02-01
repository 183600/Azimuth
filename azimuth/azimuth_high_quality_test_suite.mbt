// Azimuth 高质量测试用例集
// 使用标准 MoonBit 测试语法，包含10个精心设计的测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-10, add(-15, 5))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-35, multiply(-5, 7))
  assert_eq(0, multiply(0, 123))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_business_scenarios" {
  // 向上取整除法业务场景测试
  // 场景1：计算需要的运输车辆
  assert_eq(4, divide_with_ceil(35, 10))
  assert_eq(7, divide_with_ceil(61, 9))
  
  // 场景2：计算需要的容器数量
  assert_eq(3, divide_with_ceil(25, 8))
  assert_eq(1, divide_with_ceil(5, 8))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和分配律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_members = 6
  let tasks_per_member = 8
  let days_per_sprint = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let daily_capacity = 10
  let sprint_capacity = multiply(days_per_sprint, daily_capacity)
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)     // 6 * 8 = 48
  assert_eq(50, sprint_capacity) // 5 * 10 = 50
  assert_eq(1, sprints_needed)   // ceil(48/50) = 1
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(100, 0))
  
  // 单位值测试
  assert_eq(100, multiply(100, 1))
  assert_eq(100, multiply(1, 100))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  // 负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  // 负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-3, -5))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-15, 8))
  assert_eq(-1, divide_with_ceil(-8, 10))
  assert_eq(2, divide_with_ceil(-15, -8))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let item_a = 47
  let item_b = 63
  let box_capacity = 15
  
  let boxes_a = divide_with_ceil(item_a, box_capacity)
  let boxes_b = divide_with_ceil(item_b, box_capacity)
  let total_boxes = add(boxes_a, boxes_b)
  
  assert_eq(4, boxes_a)     // ceil(47/15) = 4
  assert_eq(5, boxes_b)     // ceil(63/15) = 5
  assert_eq(9, total_boxes) // 4 + 5 = 9
  
  // 验证总容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(135, total_capacity) // 9 * 15 = 135
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算验证 - 使用较小的数避免溢出
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
}