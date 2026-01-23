// 高质量标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "divide_with_ceil_advanced" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "edge_case_handling" {
  // 边界情况处理
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}

test "real_world_inventory_management" {
  // 实际库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let restock_amount = 50
  let days = 7
  
  let total_sold = @azimuth.multiply(daily_sales, days)
  let stock_before_restock = @azimuth.add(initial_stock, -total_sold)
  let final_stock = @azimuth.add(stock_before_restock, restock_amount)
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(66, stock_before_restock)
  @azimuth.assert_eq(116, final_stock)
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, interest_rate), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, interest_rate), 100))
  
  @azimuth.assert_eq(1050, year1_amount)
  @azimuth.assert_eq(1103, year2_amount)
  @azimuth.assert_eq(1158, year3_amount)
}

test "packaging_optimization" {
  // 包装优化场景
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  let box_capacity = 12
  
  let small_boxes = @azimuth.divide_with_ceil(small_items, box_capacity)
  let medium_boxes = @azimuth.divide_with_ceil(medium_items, box_capacity)
  let large_boxes = @azimuth.divide_with_ceil(large_items, box_capacity)
  let total_boxes = @azimuth.add(@azimuth.add(small_boxes, medium_boxes), large_boxes)
  
  @azimuth.assert_eq(3, small_boxes)
  @azimuth.assert_eq(4, medium_boxes)
  @azimuth.assert_eq(7, large_boxes)
  @azimuth.assert_eq(14, total_boxes)
}