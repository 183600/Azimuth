// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试项目的核心功能

test "basic_arithmetic_addition" {
  // 测试基本的加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-4, @azimuth.add(-2, -2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_arithmetic_multiplication" {
  // 测试基本的乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "overflow_protection_addition" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "overflow_protection_multiplication" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -12
  let d = 8
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "mathematical_associativity" {
  // 测试数学结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "practical_calculation_scenario" {
  // 测试实际计算场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), shipping_fee)
  
  // 验证总价：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  @azimuth.assert_eq(717, cart_total)
}

test "edge_case_zero_operations" {
  // 测试零的运算特性
  let positive_num = 42
  let negative_num = -17
  
  // 零的加法特性
  @azimuth.assert_eq(positive_num, @azimuth.add(positive_num, 0))
  @azimuth.assert_eq(negative_num, @azimuth.add(0, negative_num))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 零的乘法特性
  @azimuth.assert_eq(0, @azimuth.multiply(positive_num, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, negative_num))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 5
  let d = 30
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 8 + 12 = 20
  let product = @azimuth.multiply(sum_ab, c)  // 20 * 5 = 100
  let result = @azimuth.add(product, -d)  // 100 - 30 = 70
  
  @azimuth.assert_eq(70, result)
}