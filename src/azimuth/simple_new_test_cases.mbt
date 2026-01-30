// 简单的新测试用例 - 针对 azimuth 库的核心功能
// 使用标准 MoonBit 测试语法

test "simple_add_test" {
  // 测试基本加法功能
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
}

test "simple_multiply_test" {
  // 测试基本乘法功能
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(0, 10))
  azimuth::assert_eq(-6, azimuth::multiply(-2, 3))
}

test "simple_greet_test" {
  // 测试基本问候功能
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "simple_divide_with_ceil_test" {
  // 测试向上取整除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))
}

test "simple_mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "simple_combined_operations" {
  // 测试组合运算
  let result1 = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "simple_practical_scenario" {
  // 实际应用场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = azimuth::multiply(unit_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  
  azimuth::assert_eq(200, subtotal)
  azimuth::assert_eq(215, total)
}

test "simple_packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  
  azimuth::assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_unicode_test" {
  // 测试 Unicode 字符
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
}

test "simple_edge_cases" {
  // 测试边界情况
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
}