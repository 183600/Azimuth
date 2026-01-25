// 标准 MoonBit 质量测试用例
// 针对 azimuth 库核心功能的全面质量测试

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(59, add(42, 17))
  assert_eq(59, add(17, 42))
}

test "multiplication_identity_element" {
  // 测试乘法单位元：a * 1 = a
  let values = [0, 1, -1, 5, 10, -15]
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(10, multiply(10, 1))
  assert_eq(-15, multiply(-15, 1))
}

test "divide_with_ceil_fractional_handling" {
  // 测试向上取整除法的小数处理
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3))   // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 1000)) // 0.001 -> 1
}

test "greet_function_unicode_comprehensive" {
  // 测试问候函数的 Unicode 字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀🌍!", greet("🚀🌍"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8
  let discount = 45
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算最终价格
  let final_price = add(add(subtotal, tax), -discount)
  
  assert_eq(534, subtotal)
  assert_eq(43, tax)
  assert_eq(532, final_price)
}

test "negative_number_division_precision" {
  // 测试负数除法的精度处理
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(-3, divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  assert_eq(3, divide_with_ceil(-7, -2))   // -7/-2 = 3.5 -> 4
  assert_eq(0, divide_with_ceil(-1, 100))  // -1/100 = -0.01 -> 0
}

test "packaging_optimization_problem" {
  // 测试包装优化问题
  let total_items = 127
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let empty_spaces = add(total_capacity, -total_items)
  
  assert_eq(9, boxes_needed)        // ceil(127/15) = 9
  assert_eq(135, total_capacity)    // 9 * 15 = 135
  assert_eq(8, empty_spaces)        // 135 - 127 = 8
}

test "mathematical_series_summation" {
  // 测试数学级数求和：等差数列 1+3+5+...+19
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(100, series_sum)  // 10/2 * (2 + 18) = 5 * 20 = 100
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数的组合使用
  let prefix = "Dr."
  let first_name = "Alice"
  let last_name = "Smith"
  
  // 构建完整姓名
  let full_name = prefix + " " + first_name + " " + last_name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Alice Smith!", formal_greeting)
  
  // 测试特殊字符组合
  let special_chars = "@" + "test" + "." + "com"
  let email_greeting = greet(special_chars)
  assert_eq_string("Hello, @test.com!", email_greeting)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本250）
  let small_projects_count = divide_with_ceil(remaining_budget, 250)
  
  assert_eq(3900, allocated_budget)
  assert_eq(1100, remaining_budget)
  assert_eq(5, small_projects_count)  // ceil(1100/250) = 5
}