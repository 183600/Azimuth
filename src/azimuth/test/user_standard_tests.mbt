// 用户请求的标准 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "zero_value_operations" {
  // 测试零值运算
  let test_value = 42
  
  // 加法零值
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法零值
  @azimuth.assert_eq(0, @azimuth.multiply(test_value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_value))
}

test "negative_number_calculations" {
  // 测试负数计算
  let neg_a = -10
  let neg_b = -20
  let pos_c = 15
  
  // 负数相加
  @azimuth.assert_eq(-30, @azimuth.add(neg_a, neg_b))
  
  // 正负数相加
  @azimuth.assert_eq(5, @azimuth.add(neg_a, pos_c))
  
  // 负数相乘
  @azimuth.assert_eq(200, @azimuth.multiply(neg_a, neg_b))
  
  // 正负数相乘
  @azimuth.assert_eq(-150, @azimuth.multiply(neg_a, pos_c))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 8
  let b = 12
  let c = 5
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)  // 20
  let product = @azimuth.multiply(sum_ab, c)  // 100
  let result = @azimuth.add(product, -d)  // 85
  
  @azimuth.assert_eq(85, result)
}

test "string_special_characters" {
  // 测试特殊字符字符串
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 5
  let b = 7
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 99
  let quantity = 3
  let shipping = 10
  let discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 应用折扣和运费
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(307, total)  // 99*3 - 20 + 10 = 297 - 20 + 10 = 287
}