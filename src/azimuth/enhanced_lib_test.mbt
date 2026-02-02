// Azimuth 库的增强测试用例
// 使用标准 MoonBit 测试语法

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  // 计算剩余空间
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, remaining_space)
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100)
  let amount_year1 = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(amount_year1, rate_percent), 100)
  let amount_year2 = @azimuth.add(amount_year1, year2_interest)
  
  @azimuth.assert_eq(1050, amount_year1)
  @azimuth.assert_eq(1103, amount_year2)
}

test "inventory_management" {
  // 库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let new_shipment = 50
  
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let stock_before_shipment = @azimuth.subtract(initial_stock, total_sold)
  let final_stock = @azimuth.add(stock_before_shipment, new_shipment)
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(66, stock_before_shipment)
  @azimuth.assert_eq(116, final_stock)
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "time_and_resource_calculation" {
  // 时间和资源计算
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(6, days_needed)
  @azimuth.assert_eq(48, total_hours)
}

test "unicode_greeting_comprehensive" {
  // 全面的 Unicode 问候测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "extreme_boundary_values" {
  // 极端边界值测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
}