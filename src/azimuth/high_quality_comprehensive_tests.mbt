// 高质量综合测试用例 - 针对 azimuth 库的核心功能
// 使用标准 MoonBit 测试语法编写

test "add_function_comprehensive_test" {
  // 全面测试加法功能，包括正数、负数和零
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 1000000))
}

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(100, subtract(50, -50))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度和边界情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零返回0
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持和特殊字符处理
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, María!", greet("María"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：订单总价计算
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

test "packaging_optimization_calculation" {
  // 包装优化计算：计算需要的包装箱数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, remaining_space)
}

test "mathematical_properties_verification" {
  // 数学性质验证：交换律和结合律
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "financial_compound_interest" {
  // 金融计算：复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
}

test "inventory_management_workflow" {
  // 库存管理工作流
  let initial_stock = 150
  let daily_sales = 12
  let days = 7
  let new_shipment = 50
  
  let total_sold = multiply(daily_sales, days)
  let stock_before_shipment = subtract(initial_stock, total_sold)
  let final_stock = add(stock_before_shipment, new_shipment)
  
  assert_eq(84, total_sold)
  assert_eq(66, stock_before_shipment)
  assert_eq(116, final_stock)
}