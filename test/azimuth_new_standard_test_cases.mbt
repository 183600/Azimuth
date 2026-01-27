// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  azimuth::assert_eq(15, azimuth::add(7, 8))
  azimuth::assert_eq(20, azimuth::multiply(4, 5))
  azimuth::assert_eq(0, azimuth::multiply(0, 100))
  azimuth::assert_eq(-3, azimuth::add(-10, 7))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法的核心功能
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 测试基本问候功能
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
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

test "complex_business_calculations" {
  // 测试复杂业务计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 15
  let shipping = 12
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let tax = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  azimuth::assert_eq(200, subtotal)
  azimuth::assert_eq(30, tax)
  azimuth::assert_eq(242, total)
}

test "negative_number_operations" {
  // 测试负数运算
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(-5, azimuth::add(-8, 3))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
}

test "packaging_and_shipping_calculations" {
  // 测试包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  
  let weight_per_item = 3
  let total_weight = azimuth::multiply(total_items, weight_per_item)
  
  azimuth::assert_eq(4, boxes_needed)
  azimuth::assert_eq(141, total_weight)
}

test "string_greeting_special_characters" {
  // 测试特殊字符的字符串问候
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
}

test "large_scale_calculations" {
  // 测试大规模计算
  let base_value = 10000
  let multiplier = 150
  let addition = 25000
  
  let large_product = azimuth::multiply(base_value, multiplier)
  let large_sum = azimuth::add(large_product, addition)
  
  azimuth::assert_eq(1500000, large_product)
  azimuth::assert_eq(1525000, large_sum)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  // 测试1的运算
  azimuth::assert_eq(1, azimuth::multiply(1, 1))
  azimuth::assert_eq(5, azimuth::multiply(1, 5))
  azimuth::assert_eq(5, azimuth::multiply(5, 1))
  
  // 测试相等除法
  azimuth::assert_eq(1, azimuth::divide_with_ceil(5, 5))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(15, 5))
  
  // 测试零的加法
  azimuth::assert_eq(5, azimuth::add(5, 0))
  azimuth::assert_eq(-5, azimuth::add(-5, 0))
}