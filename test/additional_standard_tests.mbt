// 额外的标准 MoonBit 测试用例
// 测试核心功能：add, multiply, greet 函数

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(0, @azimuth.add(3, -3))
}

test "multiply_commutative_property" {
  // 测试乘法交换律：a × b = b × a
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(4, 3))
  
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(20, @azimuth.multiply(-5, -4))
}

test "distributive_property" {
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @user!", @azimuth.greet("@user"))
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, user_name!", @azimuth.greet("user_name"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与普通值相加
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 边界值与零相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与1相乘
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 边界值与0相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列：(a + b) × (c + d)
  let a = 10
  let b = 5
  let c = 3
  let d = 2
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result = @azimuth.multiply(sum1, sum2)
  
  @azimuth.assert_eq(75, result)  // (10 + 5) × (3 + 2) = 15 × 5 = 75
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法单位元：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法单位元：x × 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零元：x × 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(717, total)  // (199 × 2) + (299 × 1) + 20 = 398 + 299 + 20 = 717
}