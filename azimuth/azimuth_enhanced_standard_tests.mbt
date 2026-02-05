// Azimuth 增强标准测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(10, add(7, 3))
  assert_eq(21, multiply(7, 3))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(4, subtract(10, 6))
  
  // 零值测试
  assert_eq(7, add(7, 0))
  assert_eq(0, multiply(7, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "mathematical_commutative_and_associative" {
  // 数学交换律和结合律测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "business_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 95
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(8, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let spare_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, spare_capacity)  // 60 - 48 = 12
}

test "negative_numbers_comprehensive" {
  // 负数综合测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, add(5, -7))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(-5, subtract(2, 7))
  assert_eq(12, subtract(5, -7))
  
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  // 大数运算测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  assert_eq(1, divide_with_ceil(10000, 10000))
  
  // 复杂计算链
  let result = add(
    multiply(100, divide_with_ceil(150, 50)),
    subtract(200, multiply(10, 15))
  )
  assert_eq(350, result)  // 100*3 + (200-150) = 300 + 50 = 350
}