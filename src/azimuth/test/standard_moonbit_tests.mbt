// 标准 MoonBit 测试用例 - 覆盖核心功能

// 测试基本加法功能
test "add_basic_functionality" {
  let result = @azimuth.add(10, 15)
  @azimuth.assert_eq(25, result)
}

// 测试加法边界情况（正数溢出）
test "add_positive_overflow" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result)
}

// 测试加法边界情况（负数溢出）
test "add_negative_overflow" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

// 测试基本乘法功能
test "multiply_basic_functionality" {
  let result = @azimuth.multiply(7, 8)
  @azimuth.assert_eq(56, result)
}

// 测试乘法零值情况
test "multiply_zero_cases" {
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(456, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试乘法边界情况（最小值乘以-1）
test "multiply_min_value_neg_one" {
  let min_val = -2147483648
  let result = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

// 测试基本问候功能
test "greet_basic_functionality" {
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

// 测试问候功能空字符串情况
test "greet_empty_string" {
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

// 测试复合计算场景（矩形周长）
test "complex_calculation_perimeter" {
  let length = 12
  let width = 8
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
}

// 测试实际应用场景（购物车总价）
test "real_world_shopping_cart" {
  let item_price = 99
  let quantity = 3
  let shipping = 15
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(312, total)
}