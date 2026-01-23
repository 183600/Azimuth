// 简化的高质量 MoonBit 测试套件
// 包含9个核心测试用例

test "add_function_commutative" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "multiply_function_commutative" {
  // 测试乘法交换律
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative" {
  // 测试向上取整除法负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_unicode" {
  // 测试问候函数Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(24, result2)  // (5+3) * ceil(10/4) = 8 * 3 = 24
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item2_price = 299
  let item3_price = 99
  let shipping = 15
  let total = @azimuth.add(@azimuth.add(@azimuth.add(item1_price, item2_price), item3_price), shipping)
  @azimuth.assert_eq(612, total)
  
  // 页数计算
  let total_items = 100
  let items_per_page = 30
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(4, pages_needed)
}