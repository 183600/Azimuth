// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含 10 个高质量的测试用例，覆盖主要功能

test "add_function_standard" {
  // 测试加法函数的标准功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_standard" {
  // 测试乘法函数的标准功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_standard" {
  // 测试问候函数的标准功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_standard" {
  // 测试向上取整除法的标准功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.add(@azimuth.divide_with_ceil(10, 3), @azimuth.multiply(2, 2))
  @azimuth.assert_eq(8, result2)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

test "practical_packaging_calculation" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "greeting_special_characters" {
  // 测试特殊字符的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_business_scenario" {
  // 复杂业务场景：计算订单总价
  let product_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping_cost)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(215, final_total)  // 25*8 + 15 - 20 = 200 + 15 - 20 = 195
}