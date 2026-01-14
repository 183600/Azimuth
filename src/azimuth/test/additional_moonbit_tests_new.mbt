// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiplication_precision_test" {
  // 测试乘法精度
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999999999))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 123
  let b = 456
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  let c = -789
  let d = 321
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let x = 10
  let y = 20
  let z = 30
  
  // 计算 (x + y) * z - x * y
  let sum_xy = @azimuth.add(x, y)  // 30
  let product_sum_z = @azimuth.multiply(sum_xy, z)  // 900
  let product_xy = @azimuth.multiply(x, y)  // 200
  let result = @azimuth.add(product_sum_z, -product_xy)  // 700
  
  @azimuth.assert_eq(700, result)
}

test "string_processing_unicode" {
  // 测试字符串 Unicode 处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
}

test "arithmetical_associativity" {
  // 测试算术结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let result3 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result4 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result3, result4)
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(717, final_total)  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 验证正常运算
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}