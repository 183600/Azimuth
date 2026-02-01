// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(100, add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(25, multiply(-5, -5))
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(4, subtract(7, 3))
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数处理测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_standard_cases" {
  // 问候函数标准情况测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_size = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}