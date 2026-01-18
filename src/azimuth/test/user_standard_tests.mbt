// 用户标准测试用例
// 测试 Azimuth 项目的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_simple_names" {
  // 测试简单名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 最大值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "real_world_calculation" {
  // 测试实际应用场景计算
  // 计算购物车总价：商品价格 × 数量 + 运费
  let item_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(110, total)
}

test "string_processing" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "complex_mathematical_expression" {
  // 测试复杂数学表达式
  // 计算：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}