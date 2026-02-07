// 标准 Azimuth 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_size = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_size, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "packaging_scenario_test" {
  // 包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(0, subtract(1000000, 1000000))
}

test "mixed_sign_operations" {
  // 混合符号运算测试
  assert_eq(5, add(-10, 15))
  assert_eq(-50, multiply(-5, 10))
  assert_eq(20, subtract(10, -10))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "edge_cases_and_special_inputs" {
  // 边界情况和特殊输入测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  
  // 极值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, add(0, 0))
}