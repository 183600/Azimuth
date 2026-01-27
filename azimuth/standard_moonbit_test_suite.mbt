// 标准 MoonBit 测试套件
// 包含10个高质量测试用例，覆盖 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(2, subtract(10, 8))
  assert_eq(-2, subtract(8, 10))
}

test "multiplication_comprehensive" {
  // 乘法综合测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(16, multiply(-4, -4))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景：计算装盒需求
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
}

test "business_logic_application" {
  // 业务逻辑应用测试
  // 场景：项目资源分配
  let total_tasks = 67
  let team_members = 5
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(14, tasks_per_member)
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_member, team_members)
  assert_eq(70, total_capacity)
  
  // 验证任务分配
  assert_true(total_capacity >= total_tasks)
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}