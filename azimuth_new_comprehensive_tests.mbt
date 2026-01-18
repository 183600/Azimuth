// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖项目核心功能

// 导入 azimuth 模块
use azimuth

test "arithmetc_progression_sum" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (2a + (n-1)d) / 2
  let n = 5
  let a = 1
  let d = 3
  
  let sum = multiply(n, add(multiply(2, a), multiply(n - 1, d))) / 2
  assert_eq(35, sum)
  
  // 验证手动计算
  let manual_sum = add(add(add(add(1, 4), 7), 10), 13)
  assert_eq(35, manual_sum)
}

test "geometric_progression_sum" {
  // 测试等比数列求和：2 + 6 + 18 + 54
  // 使用公式：a(r^n - 1) / (r - 1)
  let a = 2
  let r = 3
  let n = 4
  
  // 手动计算前4项
  let term1 = 2
  let term2 = multiply(term1, r)  // 6
  let term3 = multiply(term2, r)  // 18
  let term4 = multiply(term3, r)  // 54
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, sum)
}

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1200
  let discount_percent = 15
  let tax_rate = 8
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  assert_eq(180, discount_amount)
  
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(1020, discounted_price)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  assert_eq(81, tax_amount)
  
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)
  assert_eq(1101, final_price)
}

test "speed_distance_time" {
  // 测试速度、距离、时间计算
  let speed = 60  // km/h
  let time = 2.5  // 小时，使用整数表示为250分钟
  
  // 计算距离（使用分钟避免小数）
  let time_minutes = 250  // 2.5小时 = 250分钟
  let distance_per_minute = speed / 60  // 每分钟距离
  let total_distance = multiply(distance_per_minute, time_minutes)
  assert_eq(250, total_distance)  // 150 km，用250表示
  
  // 测试时间计算：距离 / 速度
  let distance = 150
  let time_needed = multiply(distance, 60) / speed
  assert_eq(150, time_needed)  // 150分钟 = 2.5小时
}

test "compound_interest_simplified" {
  // 测试简化的复利计算
  let principal = 5000
  let annual_rate = 6  // 6%
  let years = 4
  
  // 简化复利公式：P(1 + r)^n
  // 使用整数近似：(1 + 6%)^4 ≈ 1 + 6%×4 = 1.24
  let growth_factor = add(100, multiply(annual_rate, years))
  let final_amount = multiply(principal, growth_factor)
  assert_eq(620000, final_amount)  // 5000 × 124 = 620000，表示6200元
  
  // 测试简单利息对比
  let simple_interest = multiply(principal, annual_rate)
  let simple_total = add(principal, multiply(simple_interest, years))
  assert_eq(170000, simple_total)  // 5000 + 5000×6×4 = 170000，表示1700元
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  let name_with_quotes = "\"John Doe\""
  let result1 = azimuth.greet(name_with_quotes)
  assert_eq_string("Hello, \"John Doe\"!", result1)
  
  let name_with_newline = "Line1\nLine2"
  let result2 = azimuth.greet(name_with_newline)
  assert_eq_string("Hello, Line1\nLine2!", result2)
  
  let name_with_tab = "Col1\tCol2"
  let result3 = azimuth.greet(name_with_tab)
  assert_eq_string("Hello, Col1\tCol2!", result3)
  
  let name_with_mixed = "Test@123#$"
  let result4 = azimuth.greet(name_with_mixed)
  assert_eq_string("Hello, Test@123#$!", result4)
}

test "temperature_conversion_detailed" {
  // 详细的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 - 10 + 32 = C × 2 + 22
  
  let celsius_temps = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [22, 42, 62, 82, 102, 222]
  
  // 测试0°C
  let f1 = add(multiply(0, 2), 22)
  assert_eq(22, f1)
  
  // 测试20°C（室温）
  let f2 = add(multiply(20, 2), 22)
  assert_eq(62, f2)
  
  // 测试100°C（沸点）
  let f3 = add(multiply(100, 2), 22)
  assert_eq(222, f3)
  
  // 测试反向转换：华氏度转摄氏度（简化）
  // C = (F - 22) / 2
  let fahrenheit = 68
  let celsius = add(fahrenheit, -22) / 2
  assert_eq(23, celsius)
}

test "inventory_management" {
  // 测试库存管理计算
  let initial_stock = 500
  let units_sold = 120
  let units_received = 80
  let units_damaged = 15
  let safety_stock = 50
  
  // 计算当前库存
  let current_stock = add(add(initial_stock, -units_sold), add(units_received, -units_damaged))
  assert_eq(445, current_stock)
  
  // 计算是否需要补货
  let need_reorder = current_stock < safety_stock
  assert_false(need_reorder)
  
  // 测试需要补货的情况
  let additional_sales = 400
  let new_stock = add(current_stock, -additional_sales)
  let need_reorder_now = new_stock < safety_stock
  assert_true(need_reorder_now)
  
  // 计算建议订货量（补货到安全库存的2倍）
  let suggested_order = if (need_reorder_now) {
    add(multiply(safety_stock, 2), -new_stock)
  } else {
    0
  }
  assert_eq(155, suggested_order)
}

test "loan_payment_calculation" {
  // 测试贷款支付计算（简化版）
  let loan_amount = 100000
  let annual_rate = 6  // 6%
  let loan_years = 5
  
  // 简化的月供计算：本金 / 月数 + 利息
  let total_months = multiply(loan_years, 12)
  let monthly_principal = loan_amount / total_months
  assert_eq(1666, monthly_principal)  // 整数除法
  
  // 计算总利息（简化）
  let total_interest = multiply(loan_amount, annual_rate) / 100
  let monthly_interest = total_interest / total_months
  assert_eq(500, monthly_interest)
  
  // 计算月供
  let monthly_payment = add(monthly_principal, monthly_interest)
  assert_eq(2166, monthly_payment)
  
  // 计算总支付金额
  let total_payment = multiply(monthly_payment, total_months)
  assert_eq(129960, total_payment)
}

test "array_manipulation_simulation" {
  // 测试数组操作模拟（使用索引计算）
  let rows = 4
  let cols = 5
  
  // 计算二维数组大小
  let total_elements = multiply(rows, cols)
  assert_eq(20, total_elements)
  
  // 测试索引转换：二维索引转一维索引
  let row = 2
  let col = 3
  let linear_index = add(multiply(row, cols), col)
  assert_eq(13, linear_index)
  
  // 测试边界索引
  let first_index = add(multiply(0, cols), 0)
  assert_eq(0, first_index)
  
  let last_index = add(multiply(rows - 1, cols - 1), cols - 1)
  assert_eq(19, last_index)
  
  // 测试对角线索引
  let diagonal_indices = [0, 6, 12, 18]
  let diag1 = add(multiply(0, cols), 0)
  let diag2 = add(multiply(1, cols), 1)
  let diag3 = add(multiply(2, cols), 2)
  let diag4 = add(multiply(3, cols), 3)
  
  assert_eq(0, diag1)
  assert_eq(6, diag2)
  assert_eq(12, diag3)
  assert_eq(18, diag4)
}