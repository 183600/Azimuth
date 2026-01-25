// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(10, azimuth::add(7, 3))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(-6, azimuth::multiply(-2, 3))
  assert_eq(15, azimuth::multiply(3, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "ceil_division_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, azimuth::divide_with_ceil(10, 3))
  assert_eq(3, azimuth::divide_with_ceil(9, 3))
  assert_eq(1, azimuth::divide_with_ceil(1, 100))
  assert_eq(2, azimuth::divide_with_ceil(3, 2))
}

test "ceil_division_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  assert_eq(0, azimuth::divide_with_ceil(5, 0))
  assert_eq(-1, azimuth::divide_with_ceil(-1, 2))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(azimuth::add(7, 13), azimuth::add(13, 7))
  assert_eq(azimuth::multiply(7, 13), azimuth::multiply(13, 7))
  assert_eq(20, azimuth::add(8, 12))
  assert_eq(96, azimuth::multiply(8, 12))
}

test "zero_element_properties" {
  // 测试零元素性质
  assert_eq(5, azimuth::add(5, 0))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(0, azimuth::divide_with_ceil(0, 5))
  assert_eq(0, azimuth::divide_with_ceil(0, 1))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算组合
  let result = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = azimuth::divide_with_ceil(azimuth::add(10, 5), 4)
  assert_eq(4, result2)  // ceil(15/4) = 4
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = azimuth::divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = azimuth::multiply(boxes_needed, items_per_box)
  assert_eq(30, total_capacity)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符处理
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  assert_eq_string("Hello, café!", azimuth::greet("café"))
  assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
}