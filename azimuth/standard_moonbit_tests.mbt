// 标准 MoonBit 测试用例 - 针对核心功能的综合测试
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_core_functions" {
  // 基本算术核心函数测试
  assert_eq(7, add(3, 4))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  
  assert_eq(12, multiply(3, 4))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(5, 0))
  
  assert_eq(2, subtract(8, 6))
  assert_eq(-2, subtract(6, 8))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数情况测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_string_handling" {
  // 问候函数字符串处理测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 25
  let y = 15
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(0, subtract(x, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(45, total_capacity)
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(5, remaining_capacity)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
}

test "error_handling_and_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 特殊字符串输入处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, special chars!", greet("special chars"))
  
  // 边界条件验证
  assert_true(0 <= divide_with_ceil(100, 3))
  assert_true(0 <= divide_with_ceil(-100, 3))
  assert_true(0 <= divide_with_ceil(100, -3))
}