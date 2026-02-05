// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法编写

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(8, 6))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_standard_cases" {
  // 问候函数标准测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-4, add(-7, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_commutative_property" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
}

test "boundary_values_test" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, @user!", greet("@user"))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 10000
  let monthly_payment = 750
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(14, months_needed)
  
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(10500, total_payment)
}