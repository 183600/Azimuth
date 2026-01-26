// 标准 Azimuth 核心测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(6, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-3, 4))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(15, multiply(-3, -5))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "ceil_division_basic_cases" {
  // 向上取整除法基本案例测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "subtraction_functionality" {
  // 减法功能测试
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(10, subtract(-5, -15))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：项目资源分配
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
}

test "edge_case_negative_numbers" {
  // 边界情况：负数测试
  assert_eq(-8, add(-5, -3))
  assert_eq(16, multiply(-4, -4))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-2, divide_with_ceil(7, -3))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "resource_optimization_scenario" {
  // 资源优化场景测试：仓库存储
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}