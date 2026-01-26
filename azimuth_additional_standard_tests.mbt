// Azimuth 项目的标准 MoonBit 测试用例
// 包含 10 个高质量的测试用例，覆盖核心功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))   // 10/3 = 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))     // 除数为0时返回0
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "mathematical_series_sum" {
  // 数学级数求和：等差数列 1+2+3+...+10
  let n = 10
  let sum = @azimuth.divide_with_ceil(@azimuth.multiply(n, @azimuth.add(n, 1)), 2)
  @azimuth.assert_eq(55, sum)
}

test "inventory_management_calculation" {
  // 库存管理计算
  let initial_stock = 150
  let daily_sales = 12
  let days = 7
  let new_shipment = 50
  
  let total_sold = @azimuth.multiply(daily_sales, days)
  let current_stock = @azimuth.add(@azimuth.add(initial_stock, -total_sold), new_shipment)
  let restock_needed = @azimuth.divide_with_ceil(@azimuth.multiply(100, 2), daily_sales)  // 2天库存
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(116, current_stock)
  @azimuth.assert_eq(17, restock_needed)
}

test "time_conversion_calculation" {
  // 时间转换计算
  let total_minutes = 155
  let hours = @azimuth.divide_with_ceil(total_minutes, 60)
  let remaining_minutes = @azimuth.add(total_minutes, -@azimuth.multiply(hours - 1, 60))
  
  @azimuth.assert_eq(3, hours)
  @azimuth.assert_eq(35, remaining_minutes)
}

test "energy_efficiency_calculation" {
  // 能源效率计算
  let old_power = 100
  let new_power = 75
  let hours_per_day = 8
  let days_per_month = 30
  
  let old_monthly = @azimuth.multiply(old_power, @azimuth.multiply(hours_per_day, days_per_month))
  let new_monthly = @azimuth.multiply(new_power, @azimuth.multiply(hours_per_day, days_per_month))
  let savings = @azimuth.add(old_monthly, -new_monthly)
  let savings_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(savings, 100), old_monthly)
  
  @azimuth.assert_eq(24000, old_monthly)
  @azimuth.assert_eq(18000, new_monthly)
  @azimuth.assert_eq(6000, savings)
  @azimuth.assert_eq(25, savings_percentage)
}

test "geometric_calculation" {
  // 几何计算：矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  let diagonal_approx = @azimuth.divide_with_ceil(@azimuth.multiply(length, 141), 100)  // 简化的对角线计算
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
  @azimuth.assert_eq(17, diagonal_approx)
}