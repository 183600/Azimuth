// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-4, add(-2, -2))
  assert_eq(5, add(-3, 8))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-14, multiply(-2, 7))
  assert_eq(14, multiply(-2, -7))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_basic" {
  // 基本减法功能测试
  assert_eq(6, subtract(10, 4))
  assert_eq(-4, subtract(4, 8))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(5, -10))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 28
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(30, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(2, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 5
  let tasks_per_member = 7
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 12
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(35, total_tasks)  // 5 * 7 = 35
  assert_eq(3, sprints_needed)  // ceil(35/12) = 3
}