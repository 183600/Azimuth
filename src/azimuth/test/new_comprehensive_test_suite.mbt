// 新的综合测试套件 - 10个MoonBit测试用例
// 测试不同的功能和场景，确保代码的正确性和健壮性

test "mathematical_operations_test" {
  // 测试基本数学运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-24, @azimuth.multiply(-3, 8))
}

test "boundary_value_test" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_test" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculation_test" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 5
  
  // 计算 (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(150, result)
  
  // 计算 a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(205, result2)
}

test "real_world_scenario_test" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)
}

test "assert_function_test" {
  // 测试断言函数
  assert_true(5 > 3)
  assert_false(5 < 3)
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "error_handling_test" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  
  // 确保溢出被正确处理
  assert_true(@azimuth.add(max_val, 100) == max_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试负数运算
  assert_true(@azimuth.add(-10, -20) == -30)
  assert_true(@azimuth.multiply(-5, 5) == -25)
}

test "sequential_operations_test" {
  // 测试连续运算
  let result = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

test "special_characters_test" {
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "mathematical_properties_test" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), 5), @azimuth.add(a, @azimuth.add(b, 5)))
}