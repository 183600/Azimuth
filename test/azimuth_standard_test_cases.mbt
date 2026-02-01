// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试、边界情况测试和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
  azimuth::assert_eq(5, azimuth::subtract(8, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 零元素性质
  azimuth::assert_eq(0, azimuth::multiply(a, 0))
  azimuth::assert_eq(a, azimuth::add(a, 0))
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
}

test "negative_number_operations" {
  // 测试负数运算
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(-5, azimuth::add(-8, 3))
  azimuth::assert_eq(0, azimuth::add(-5, 5))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
}

test "complex_business_calculations" {
  // 测试复杂业务计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let total_before_discount = azimuth::add(subtotal, shipping)
  let final_total = azimuth::add(total_before_discount, -discount)
  
  azimuth::assert_eq(200, subtotal)
  azimuth::assert_eq(215, total_before_discount)
  azimuth::assert_eq(195, final_total)
}

test "packaging_and_distribution" {
  // 测试包装和分配计算
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  let total_capacity = azimuth::multiply(boxes_needed, items_per_box)
  let remaining_space = azimuth::subtract(total_capacity, total_items)
  
  azimuth::assert_eq(4, boxes_needed)
  azimuth::assert_eq(48, total_capacity)
  azimuth::assert_eq(1, remaining_space)
}

test "large_scale_operations" {
  // 测试大规模运算
  let base_value = 1000
  let multiplier = 100
  let addition = 50000
  
  let large_product = azimuth::multiply(base_value, multiplier)
  let large_sum = azimuth::add(large_product, addition)
  
  azimuth::assert_eq(100000, large_product)
  azimuth::assert_eq(150000, large_sum)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符处理
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, αβγ!", azimuth::greet("αβγ"))
  azimuth::assert_eq_string("Hello, ñáéíóú!", azimuth::greet("ñáéíóú"))
  azimuth::assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 库存管理
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  
  let total_sold = azimuth::multiply(daily_sales, days_to_simulate)
  let final_stock = azimuth::subtract(initial_stock, total_sold)
  
  azimuth::assert_eq(84, total_sold)
  azimuth::assert_eq(66, final_stock)
  
  // 项目时间线计算
  let tasks_per_day = 5
  let total_tasks = 47
  let workdays_needed = azimuth::divide_with_ceil(total_tasks, tasks_per_day)
  
  azimuth::assert_eq(10, workdays_needed)
}