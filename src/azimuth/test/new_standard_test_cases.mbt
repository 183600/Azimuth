// 新增 MoonBit 标准测试用例
// 覆盖 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-4, 2))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "greeting_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "ceil_division_positive" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 8))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2))
}

test "ceil_division_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 4), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(23, result1)  // 2*4 + 3*5 = 8 + 15 = 23
  
  let result2 = @azimuth.multiply(@azimuth.add(6, 4), 3)
  @azimuth.assert_eq(30, result2)  // (6 + 4) * 3 = 10 * 3 = 30
}

test "business_calculation" {
  // 测试业务计算场景
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(270, subtotal)
  @azimuth.assert_eq(285, total)
}

test "packaging_scenario" {
  // 测试包装计算场景
  let total_items = 35
  let items_per_box = 8
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(5, boxes_needed)  // ceil(35/8) = 5
}

test "unicode_support" {
  // 测试 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}