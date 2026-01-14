// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 包的核心功能
// 包含不超过10个测试用例

test "add_basic" {
  // 基本加法测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_basic" {
  // 基本乘法测试
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(-16, @azimuth.multiply(-4, 4))
}

test "greet_basic" {
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, Moon!", @azimuth.greet("Moon"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  
  // 测试负数的交换律
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
}

test "multiplication_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let left_side = @azimuth.multiply(5, @azimuth.add(3, 7))
  let right_side = @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(5, 7))
  @azimuth.assert_eq(left_side, right_side)
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "zero_operations" {
  // 测试与零相关的运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let another_large = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num, another_large))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num, another_large))
}

test "practical_application_scenario" {
  // 实际应用场景：计算购物车总价
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  
  // 计算商品总价
  let subtotal = @azimuth.add(item1_price, item2_price)
  let total = @azimuth.add(subtotal, item3_price)
  
  @azimuth.assert_eq(897, total)
  
  // 计算折扣价格（简化为减去固定金额）
  let discount = 100
  let final_price = @azimuth.add(total, -discount)
  
  @azimuth.assert_eq(797, final_price)
}