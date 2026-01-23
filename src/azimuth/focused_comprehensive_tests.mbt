// 专注的综合测试用例
// 针对 azimuth 库核心功能的10个标准测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 12
  let y = 8
  let z = 5
  
  // 测试 (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(99, result)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  let unit_price = 45
  let quantity = 6
  let tax_rate = 10
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)  // 45 * 6 = 270
  assert_eq(27, tax)        // ceil(270 * 10 / 100) = ceil(27) = 27
  assert_eq(312, total)     // 270 + 27 + 15 = 312
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_resources = 89
  let team_size = 8
  let project_duration_days = 15
  
  let resources_per_person = divide_with_ceil(total_resources, team_size)
  let daily_resources = divide_with_ceil(total_resources, project_duration_days)
  
  assert_eq(12, resources_per_person)  // ceil(89/8) = 12
  assert_eq(6, daily_resources)        // ceil(89/15) = 6
}

test "negative_number_operations_comprehensive" {
  // 测试负数运算的综合情况
  assert_eq(-15, add(-8, -7))
  assert_eq(-1, add(-8, 7))
  assert_eq(24, multiply(-8, -3))
  assert_eq(-24, multiply(8, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(0, 1000000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, Special@Chars#123!", greet("Special@Chars#123"))
}