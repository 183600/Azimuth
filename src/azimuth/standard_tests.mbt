// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(13, 4))  // 13/4 = 3.25 -> 4
  assert_eq(3, divide_with_ceil(12, 4))  // 12/4 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 10))   // 1/10 = 0.1 -> 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为0，返回0
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let product_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(307, total)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_resources = 100
  let team_size = 8
  let projects_per_team = 3
  
  let teams_needed = divide_with_ceil(total_resources, team_size)
  let total_projects = multiply(teams_needed, projects_per_team)
  
  assert_eq(13, teams_needed)  // ceil(100/8) = 13
  assert_eq(39, total_projects)  // 13 * 3 = 39
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 25
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let total_minutes = multiply(hours_needed, 60)
  
  assert_eq(7, hours_needed)  // ceil(25/4) = 7
  assert_eq(420, total_minutes)  // 7 * 60 = 420
}

test "mathematical_series_sum" {
  // 数学级数求和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 6
  
  // 计算等差数列和：n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(78, series_sum)  // 6/2 * (10 + 15) = 3 * 25 = 75
}

test "string_processing_combination" {
  // 字符串处理组合
  let prefix = "Hello"
  let name = "MoonBit"
  let suffix = "!"
  
  let combined_greeting = greet(name)
  let expected = prefix + ", " + name + suffix
  
  assert_eq_string(expected, combined_greeting)
}

test "edge_case_handling" {
  // 边界情况处理
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  
  // 测试零运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
}