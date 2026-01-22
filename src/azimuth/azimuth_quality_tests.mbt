// Azimuth 项目高质量测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "add_function_comprehensive_test" {
  // 全面测试加法功能，包括正数、负数和零
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-2, 7))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(99, 50))
  assert_eq(-2, divide_with_ceil(-99, 50))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_before_discount)
  assert_eq(211, final_total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  let restock_amount = 50
  let items_per_box = 24
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let stock_before_restock = add(initial_stock, -total_sold)
  let final_stock = add(stock_before_restock, restock_amount)
  let boxes_needed = divide_with_ceil(final_stock, items_per_box)
  
  assert_eq(84, total_sold)
  assert_eq(66, stock_before_restock)
  assert_eq(116, final_stock)
  assert_eq(5, boxes_needed)
}

test "engineering_calculation_scenario" {
  // 工程计算场景
  let room_length = 12
  let room_width = 8
  let room_height = 3
  let tile_area = 2
  let paint_coverage = 10
  
  let floor_area = multiply(room_length, room_width)
  let wall_area = multiply(2, multiply(add(room_length, room_width), room_height))
  let total_surface_area = add(floor_area, wall_area)
  let tiles_needed = divide_with_ceil(total_surface_area, tile_area)
  let paint_needed = divide_with_ceil(wall_area, paint_coverage)
  
  assert_eq(96, floor_area)
  assert_eq(120, wall_area)
  assert_eq(216, total_surface_area)
  assert_eq(108, tiles_needed)
  assert_eq(12, paint_needed)
}

test "algorithm_performance_simulation" {
  // 算法性能模拟测试
  let base_value = 42
  let iterations = 1000
  let multiplier = 3
  let overhead_percentage = 15
  
  let accumulated_value = multiply(base_value, iterations)
  let enhanced_result = add(accumulated_value, multiply(multiplier, divide_with_ceil(iterations, 10)))
  let overhead = divide_with_ceil(multiply(enhanced_result, overhead_percentage), 100)
  let final_performance = add(enhanced_result, -overhead)
  
  assert_eq(42000, accumulated_value)
  assert_eq(42300, enhanced_result)
  assert_eq(6345, overhead)
  assert_eq(35955, final_performance)
}

test "comprehensive_error_handling" {
  // 综合错误处理测试
  // 测试除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试极端数值运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000, multiply(1000, 1000))
}