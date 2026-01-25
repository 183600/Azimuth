// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法编写
// 包含边缘测试、性能测试和实际应用场景

// 导入必要的包和函数
test "extreme_boundary_values" {
  // 测试极端边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision_test" {
  // 测试除法向上取整的精度
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
}

test "financial_calculation_scenario" {
  // 金融计算场景：计算复利
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, rate_percent), 100))
  
  assert_eq(1158, amount_after_year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "time_calculation_scenario" {
  // 时间计算场景：计算项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000, multiply(1000, 1000))
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello,   !", greet("   "))  // 多空格
}

test "inventory_management_scenario" {
  // 库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let restock_threshold = 70
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let final_stock = add(initial_stock, -total_sold)
  let needs_restock = final_stock < restock_threshold
  
  assert_eq(84, total_sold)
  assert_eq(66, final_stock)
  // 注意：MoonBit 中没有直接的 boolean 断言，这里用数值表示
  assert_eq(1, if needs_restock { 1 } else { 0 })
}

test "algorithm_performance_simulation" {
  // 算法性能模拟
  let base_value = 42
  let iterations = 1000
  let multiplier = 3
  
  let accumulated_value = multiply(base_value, iterations)
  let final_result = add(accumulated_value, multiply(multiplier, divide_with_ceil(iterations, 10)))
  
  assert_eq(42000, accumulated_value)
  assert_eq(42300, final_result)
}

test "complex_business_workflow" {
  // 复杂业务工作流
  let product_cost = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost_per_item = 5
  let discount_threshold = 1000
  let discount_amount = 50
  
  let subtotal = multiply(product_cost, quantity)
  let total_shipping = multiply(shipping_cost_per_item, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(1800, subtotal)
  assert_eq(75, total_shipping)
  assert_eq(144, tax_amount)
  assert_eq(2019, pre_discount_total)
  assert_eq(1969, final_total)
}