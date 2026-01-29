// Azimuth 新增标准测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "mathematical_commutative_property" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果处理
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "complex_nested_calculations" {
  // 测试复杂的嵌套计算
  let base = 10
  let result = divide_with_ceil(
    multiply(add(base, 5), 3),
    subtract(base, 2)
  )
  assert_eq(6, result)  // ceil((10+5)*3/(10-2)) = ceil(45/8) = 6
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Bonjour!", greet("Bonjour"))
  assert_eq_string("Hello, Hola!", greet("Hola"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_items = 127
  let group_size = 15
  let groups_needed = divide_with_ceil(total_items, group_size)
  assert_eq(9, groups_needed)
  
  let total_capacity = multiply(groups_needed, group_size)
  let unused_capacity = subtract(total_capacity, total_items)
  assert_eq(8, unused_capacity)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(-3, 7))
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(3, divide_with_ceil(2, 1))
}

test "string_special_characters" {
  // 测试特殊字符字符串处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
}

test "business_calculation_pipeline" {
  // 测试业务计算流水线
  let employees = 24
  let tasks_per_employee = 8
  let total_tasks = multiply(employees, tasks_per_employee)
  let tasks_per_week = 45
  let weeks_needed = divide_with_ceil(total_tasks, tasks_per_week)
  assert_eq(5, weeks_needed)
  
  let total_capacity = multiply(weeks_needed, tasks_per_week)
  assert_eq(225, total_capacity)
}

test "mathematical_identity_verification" {
  // 测试数学恒等式验证
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(subtract(0, x), multiply(x, -1))
}
