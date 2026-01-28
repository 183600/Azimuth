// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的标准测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "division_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greeting_function_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 8
  let b = 12
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "negative_number_calculations" {
  // 测试负数计算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "boundary_value_testing" {
  // 测试边界值
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
}

test "practical_packaging_scenario" {
  // 测试实际包装场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(4, boxes_needed)
}

test "complex_calculation_combination" {
  // 测试复杂计算组合
  let result = @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(7, 2))
  @azimuth.assert_eq(29, result)
}

test "unicode_character_support" {
  // 测试Unicode字符支持
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total)
}