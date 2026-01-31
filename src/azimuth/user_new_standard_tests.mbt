// 新的 MoonBit 测试用例 - 针对 azimuth 库的标准测试
// 包含10个高质量的测试用例，覆盖库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
  azimuth::assert_eq(-2, azimuth::subtract(3, 5))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 零元素性质
  azimuth::assert_eq(0, azimuth::multiply(a, 0))
  azimuth::assert_eq(a, azimuth::add(a, 0))
}

test "greeting_functionality" {
  // 测试问候功能
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, User123!", azimuth::greet("User123"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(3, 2))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
}

test "negative_number_operations" {
  // 测试负数运算
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(-5, azimuth::add(-8, 3))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
}

test "real_world_shopping_cart" {
  // 模拟购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  let subtotal1 = azimuth::multiply(item1_price, item1_qty)
  let subtotal2 = azimuth::multiply(item2_price, item2_qty)
  let total = azimuth::add(subtotal1, subtotal2)
  
  azimuth::assert_eq(75, subtotal1)
  azimuth::assert_eq(30, subtotal2)
  azimuth::assert_eq(105, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  
  azimuth::assert_eq(4, boxes_needed)
}

test "complex_calculations" {
  // 复杂计算测试
  let result = azimuth::add(
    azimuth::multiply(5, azimuth::divide_with_ceil(25, 4)),
    azimuth::multiply(3, azimuth::divide_with_ceil(17, 5))
  )
  azimuth::assert_eq(43, result)  // 5*7 + 3*4 = 35 + 12 = 47
}

test "large_number_operations" {
  // 大数运算测试
  azimuth::assert_eq(2000000, azimuth::add(1000000, 1000000))
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  azimuth::assert_eq(333334, azimuth::divide_with_ceil(1000000, 3))
}

test "business_order_calculation" {
  // 业务订单计算
  let unit_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let total_before_discount = azimuth::add(subtotal, shipping)
  let final_total = azimuth::add(total_before_discount, -discount)
  
  azimuth::assert_eq(356, subtotal)
  azimuth::assert_eq(368, total_before_discount)
  azimuth::assert_eq(348, final_total)
}