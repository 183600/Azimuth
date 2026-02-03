// 用户标准测试用例 - 针对 azimuth 库的核心功能
// 使用标准的 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-5, @azimuth.subtract(3, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-10, 0))
  @azimuth.assert_eq(20, @azimuth.subtract(-5, -25))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_special_cases" {
  // 测试问候函数的特殊情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total_before_discount)
  @azimuth.assert_eq(195, final_total)
}

test "inventory_management_scenario" {
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

test "packaging_and_shipping_calculation" {
  // 包装和运输计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let remaining_space = @azimuth.subtract(total_capacity, total_items)
  
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景：计算复利
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let amount_after_year2 = @azimuth.add(amount_after_year1, @azimuth.divide_with_ceil(@azimuth.multiply(amount_after_year1, rate_percent), 100))
  
  @azimuth.assert_eq(1050, amount_after_year1)
  @azimuth.assert_eq(1103, amount_after_year2)
}