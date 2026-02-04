// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试，使用标准的 MoonBit 测试语法

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "subtract_function_various_cases" {
  // 测试减法函数的各种情况
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(100, 100))
  @azimuth.assert_eq(200, @azimuth.subtract(100, -100))
  @azimuth.assert_eq(-200, @azimuth.subtract(-100, 100))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 12
  let discount = 20
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(29, tax_amount)
  @azimuth.assert_eq(397, total_before_discount)
  @azimuth.assert_eq(377, final_total)
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  // 计算等差数列的和：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(104, sum_formula)  // 8/2 * (10 + 21) = 4 * 31 = 124
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let initial_stock = 500
  let daily_sales = 25
  let days_to_simulate = 14
  let new_shipment = 200
  
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let stock_before_shipment = @azimuth.subtract(initial_stock, total_sold)
  let final_stock = @azimuth.add(stock_before_shipment, new_shipment)
  
  @azimuth.assert_eq(350, total_sold)
  @azimuth.assert_eq(150, stock_before_shipment)
  @azimuth.assert_eq(350, final_stock)
}

test "packaging_optimization" {
  // 包装优化测试
  let small_items = 35
  let medium_items = 28
  let large_items = 15
  
  let small_box_capacity = 10
  let medium_box_capacity = 8
  let large_box_capacity = 4
  
  let small_boxes = @azimuth.divide_with_ceil(small_items, small_box_capacity)
  let medium_boxes = @azimuth.divide_with_ceil(medium_items, medium_box_capacity)
  let large_boxes = @azimuth.divide_with_ceil(large_items, large_box_capacity)
  
  let total_boxes = @azimuth.add(@azimuth.add(small_boxes, medium_boxes), large_boxes)
  
  @azimuth.assert_eq(4, small_boxes)   // ceil(35/10) = 4
  @azimuth.assert_eq(4, medium_boxes) // ceil(28/8) = 4
  @azimuth.assert_eq(4, large_boxes)  // ceil(15/4) = 4
  @azimuth.assert_eq(12, total_boxes) // 4 + 4 + 4 = 12
}

test "time_and_resource_calculation" {
  // 时间和资源计算测试
  let tasks_per_hour = 6
  let total_tasks = 75
  let working_hours_per_day = 8
  
  let hours_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = @azimuth.divide_with_ceil(hours_needed, working_hours_per_day)
  
  @azimuth.assert_eq(13, hours_needed)  // ceil(75/6) = 13
  @azimuth.assert_eq(2, days_needed)    // ceil(13/8) = 2
}