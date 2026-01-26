// 标准 Azimuth 基础测试用例
// 覆盖 azimuth 库的核心功能

test "add_function_basic_test" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_test" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-4, @azimuth.multiply(-2, 2))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_basic_test" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic_test" {
  // 测试基本向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "negative_number_operations_test" {
  // 测试负数运算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "zero_operations_test" {
  // 测试零的运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "large_number_operations_test" {
  // 测试大数运算
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
  @azimuth.assert_eq(334, @azimuth.divide_with_ceil(1000, 3))
}

test "combined_operations_test" {
  // 测试组合运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), 2)
  @azimuth.assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "practical_business_calculation_test" {
  // 测试实际业务计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total)
}

test "unicode_string_greeting_test" {
  // 测试 Unicode 字符串问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
}