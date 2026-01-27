// Azimuth 核心功能测试用例
// 使用标准 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  azimuth::assert_eq(15, azimuth::add(7, 8))
  azimuth::assert_eq(15, azimuth::add(8, 7))
  azimuth::assert_eq(0, azimuth::add(-5, 5))
  azimuth::assert_eq(0, azimuth::add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  
  // a * (b + c) = a * b + a * c
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))    // 除数为0时的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, Café!", azimuth::greet("Café"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 12
  let tax_rate = 8
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let tax = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  azimuth::assert_eq(270, subtotal)
  azimuth::assert_eq(22, tax)
  azimuth::assert_eq(304, total)
}

test "mathematical_series_sum" {
  // 数学级数求和：等差数列 1+2+3+...+10
  let n = 10
  let sum = azimuth::divide_with_ceil(azimuth::multiply(n, azimuth::add(n, 1)), 2)
  azimuth::assert_eq(55, sum)
}

test "inventory_packaging_optimization" {
  // 库存包装优化
  let items_per_box = 12
  let total_items = 85
  
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  let unused_space = azimuth::multiply(boxes_needed, items_per_box) - total_items
  
  azimuth::assert_eq(8, boxes_needed)
  azimuth::assert_eq(11, unused_space)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let amount_year1 = azimuth::add(principal, azimuth::divide_with_ceil(azimuth::multiply(principal, rate_percent), 100))
  let amount_year2 = azimuth::add(amount_year1, azimuth::divide_with_ceil(azimuth::multiply(amount_year1, rate_percent), 100))
  let amount_year3 = azimuth::add(amount_year2, azimuth::divide_with_ceil(azimuth::multiply(amount_year2, rate_percent), 100))
  
  azimuth::assert_eq(1050, amount_year1)
  azimuth::assert_eq(1103, amount_year2)
  azimuth::assert_eq(1158, amount_year3)
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_tasks = 8
  let total_tasks = 35
  let work_hours_per_day = 8
  
  let days_needed = azimuth::divide_with_ceil(total_tasks, daily_tasks)
  let total_work_hours = azimuth::multiply(days_needed, work_hours_per_day)
  
  azimuth::assert_eq(5, days_needed)
  azimuth::assert_eq(40, total_work_hours)
}

test "error_handling_boundary_cases" {
  // 错误处理边界情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))    // 0除以0
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))   // 正数除以0
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-100, 0))  // 负数除以0
  azimuth::assert_eq(0, azimuth::multiply(0, 0))             // 0乘以0
  azimuth::assert_eq(0, azimuth::add(0, 0))                  // 0加0
}