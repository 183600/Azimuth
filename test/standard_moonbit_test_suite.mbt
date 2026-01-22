// 标准 MoonBit 测试套件 - 包含 8 个高质量测试用例
// 覆盖 azimuth 库的核心功能和实际应用场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, add(10, -10))
  assert_eq(2147483647, add(2147483646, 1))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(0, 123))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_international" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, !", greet(""))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(35, sum)  // 3 + 5 + 7 + 9 + 11 = 35
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_budget = 1000
  let team_size = 6
  let project_duration_days = 15
  
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  let daily_budget_per_person = divide_with_ceil(budget_per_person, project_duration_days)
  let total_person_days = multiply(team_size, project_duration_days)
  
  assert_eq(167, budget_per_person)
  assert_eq(12, daily_budget_per_person)
  assert_eq(90, total_person_days)
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  let result = add(
    multiply(base_value, multiplier),
    divide_with_ceil(multiply(offset, 2), divisor)
  )
  
  assert_eq(36, result)  // 10*3 + ceil(7*2/4) = 30 + ceil(14/4) = 30 + 4 = 34
}