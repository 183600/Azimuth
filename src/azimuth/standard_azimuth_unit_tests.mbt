// 标准 Azimuth 单元测试用例
// 针对 azimuth 库核心功能的10个标准测试用例

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 × (8 + 12) = 5 × 20 = 100
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果处理
  assert_eq(4, divide_with_ceil(10, 3))   // 3.33... → 4
  assert_eq(3, divide_with_ceil(9, 3))    // 3.00 → 3
  assert_eq(7, divide_with_ceil(20, 3))   // 6.66... → 7
  assert_eq(2, divide_with_ceil(5, 3))    // 1.66... → 2
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品价格 × 数量 + 税费 - 折扣
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8  // 8%
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)  // ceil(270 × 8 ÷ 100) = ceil(21.6) = 22
  assert_eq(292, total_before_discount)
  assert_eq(267, final_total)
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 计算等差数列 1 + 3 + 5 + 7 + 9 的和
  let term1 = 1
  let term2 = 3
  let term3 = 5
  let term4 = 7
  let term5 = 9
  
  let series_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(25, series_sum)
  
  // 验证等差数列求和公式：n × (首项 + 末项) ÷ 2
  let formula_result = divide_with_ceil(multiply(5, add(term1, term5)), 2)
  assert_eq(series_sum, formula_result)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 1000
  let project_cost1 = 120
  let project_cost2 = 180
  let project_cost3 = 250
  let project_cost4 = 150
  let allocated_costs = add(add(add(project_cost1, project_cost2), project_cost3), project_cost4)
  let remaining_budget = add(total_budget, -allocated_costs)
  
  // 计算剩余预算可以支持的小项目数量
  let small_project_cost = 80
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(700, allocated_costs)
  assert_eq(300, remaining_budget)
  assert_eq(4, additional_projects)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let total_minutes = 475
  let minutes_per_hour = 60
  
  // 计算小时数和剩余分钟数
  let hours = divide_with_ceil(total_minutes, minutes_per_hour)
  let exact_hours = total_minutes / minutes_per_hour
  let remaining_minutes = total_minutes - multiply(exact_hours, minutes_per_hour)
  
  assert_eq(8, hours)           // 向上取整：ceil(475 ÷ 60) = 8
  assert_eq(7, exact_hours)     // 精确值：475 ÷ 60 = 7.91...
  assert_eq(55, remaining_minutes)  // 475 - 7 × 60 = 55
}

test "string_processing_with_special_characters" {
  // 测试特殊字符的字符串处理
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, HTML5!", greet("HTML5"))
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
  assert_eq_string("Hello, 100%!", greet("100%"))
}

test "comprehensive_error_handling" {
  // 测试全面的错误处理
  // 测试除零安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, -1000000))
  assert_eq(0, add(0, 0))
  assert_eq(1000000, add(1000000, 0))
  
  // 测试负数除法的向上取整行为
  assert_eq(-3, divide_with_ceil(-10, 3))   // -3.33... → -3
  assert_eq(-4, divide_with_ceil(-11, 3))   // -3.66... → -4
  assert_eq(3, divide_with_ceil(10, -3))    // -3.33... → -3
  assert_eq(4, divide_with_ceil(-11, -3))   // 3.66... → 4
}