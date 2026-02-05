// 标准 MoonBit 测试用例 - 针对 azimuth 库的高质量测试
// 包含核心功能测试、边界情况和实际应用场景

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 验证数学性质
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

test "real_world_shopping_cart" {
  // 实际应用：购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let shipping = 10
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let total = add(subtotal, shipping)
  
  assert_eq(105, subtotal)
  assert_eq(115, total)
}

test "packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证容量计算
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(1, unused_space)
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
}

test "inventory_management" {
  // 库存管理计算
  let initial_stock = 200
  let daily_sales = 15
  let days = 7
  let new_shipment = 50
  
  let sold = multiply(daily_sales, days)
  let remaining = subtract(initial_stock, sold)
  let final_stock = add(remaining, new_shipment)
  
  assert_eq(105, sold)
  assert_eq(95, remaining)
  assert_eq(145, final_stock)
}

test "complex_business_workflow" {
  // 复杂业务工作流
  let base_price = 100
  let quantity = 8
  let tax_rate = 8
  let discount = 50
  
  let subtotal = multiply(base_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let before_discount = add(subtotal, tax)
  let final_total = add(before_discount, -discount)
  
  assert_eq(800, subtotal)
  assert_eq(64, tax)
  assert_eq(864, before_discount)
  assert_eq(814, final_total)
}