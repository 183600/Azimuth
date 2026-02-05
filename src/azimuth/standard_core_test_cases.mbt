// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含基础功能测试和边界情况测试

test "add_function_basic_functionality" {
  // 测试加法函数的基础功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_optimization_scenario" {
  // 包装优化场景测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(48, total_capacity)
  assert_eq(1, remaining_space)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  
  assert_eq(1050, amount_after_year1)
  assert_eq(1103, amount_after_year2)
}

test "inventory_management_workflow" {
  // 库存管理工作流测试
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let new_shipment = 50
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let stock_before_shipment = subtract(initial_stock, total_sold)
  let final_stock = add(stock_before_shipment, new_shipment)
  
  assert_eq(84, total_sold)
  assert_eq(66, stock_before_shipment)
  assert_eq(116, final_stock)
}