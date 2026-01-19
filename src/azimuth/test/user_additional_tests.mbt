// 用户额外测试用例
// 测试 azimuth 模块的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "add_function_mixed_signs" {
  // 测试混合符号的加法
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "multiply_function_negative" {
  // 测试负数乘法
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_chars" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(17, result2)
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 100
  let item1_qty = 2
  let item2_price = 50
  let item2_qty = 3
  let shipping = 20
  
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(370, final_total)
}