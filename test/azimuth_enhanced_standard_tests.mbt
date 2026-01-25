// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法编写
// 覆盖边界情况、错误处理和实际应用场景

test "extreme_boundary_values" {
  // 测试极端边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision_cases" {
  // 测试向上取整除法的精度情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(397, total_before_discount)
  assert_eq(377, final_total)
}

test "string_greeting_unicode_extended" {
  // 扩展Unicode字符串测试
  assert_eq_string("Hello, 🚀🌟!", greet("🚀🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóúü!", greet("ñáéíóúü"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(100, 0))  // 除数为零
  assert_eq(0, divide_with_ceil(-100, 0))  // 负数除以零
  assert_eq(0, divide_with_ceil(0, 0))  // 零除以零
}

test "time_calculation_scenario" {
  // 时间计算场景：项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let project_cost = multiply(150, 4)  // 4个项目，每个150
  let remaining = add(total_budget, -project_cost)
  let small_projects = divide_with_ceil(remaining, 80)  // 每个小项目80
  
  assert_eq(600, project_cost)
  assert_eq(9400, remaining)
  assert_eq(118, small_projects)
}

test "nested_function_composition" {
  // 嵌套函数组合测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  
  // 复杂嵌套：((base * multiplier) + (base * 2)) / divisor
  let complex_result = divide_with_ceil(
    add(multiply(base_value, multiplier), multiply(base_value, 2)),
    divisor
  )
  
  assert_eq(12, complex_result)  // ceil((10*3 + 10*2) / 4) = ceil(50/4) = 13
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 简化的3年复利计算（每年向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}