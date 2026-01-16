// Azimuth 项目额外的 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_associative_laws" {
  // 测试数学结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律
  let left_associative = @azimuth.add(@azimuth.add(a, b), c)
  let right_associative = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_associative, right_associative)
  
  // 乘法结合律
  let left_mult = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult, right_mult)
}

test "distributive_law" {
  // 测试分配律
  let a = 3
  let b = 4
  let c = 5
  
  // a × (b + c) = a × b + a × c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "zero_and_identity_elements" {
  // 测试零元素和单位元
  let a = 42
  
  // 加法单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法单位元
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  
  // 零元素
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
}

test "negative_number_operations" {
  // 测试负数运算
  let pos = 10
  let neg = -5
  
  // 正负数相加
  @azimuth.assert_eq(5, @azimuth.add(pos, neg))
  @azimuth.assert_eq(-5, @azimuth.add(neg, pos))
  
  // 负数相乘
  @azimuth.assert_eq(25, @azimuth.multiply(neg, neg))
  @azimuth.assert_eq(-50, @azimuth.multiply(pos, neg))
  @azimuth.assert_eq(-50, @azimuth.multiply(neg, pos))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a + b) × (c + d) - e
  let a = 10
  let b = 5
  let c = 8
  let d = 2
  let e = 15
  
  let sum1 = @azimuth.add(a, b)  // 15
  let sum2 = @azimuth.add(c, d)  // 10
  let product = @azimuth.multiply(sum1, sum2)  // 150
  let result = @azimuth.add(product, -e)  // 135
  
  @azimuth.assert_eq(135, result)
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  
  // 应用折扣和运费
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)  // 567
  
  @azimuth.assert_eq(567, total)
}