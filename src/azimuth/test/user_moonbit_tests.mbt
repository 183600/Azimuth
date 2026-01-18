// 用户自定义 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_functionality" {
  // 测试字符串处理功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 6
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 2)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 2)))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算表达式：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 5 + 3 = 8
  let product = @azimuth.multiply(sum_ab, c)  // 8 * 4 = 32
  let result = @azimuth.add(product, -d)  // 32 - 10 = 22
  
  @azimuth.assert_eq(22, result)
}

test "real_world_application_test" {
  // 测试实际应用场景：计算订单总价
  let unit_price = 50
  let quantity = 4
  let discount = 20
  let shipping = 10
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  // 计算折扣后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  // 计算最终总价
  let total = @azimuth.add(discounted_price, shipping)
  
  // 50*4 - 20 + 10 = 200 - 20 + 10 = 190
  @azimuth.assert_eq(190, total)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}

test "sequential_operations_test" {
  // 测试连续运算
  // 计算：1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let final_sum = @azimuth.add(sum3, 5)
  
  @azimuth.assert_eq(15, final_sum)
  
  // 计算：2 * 3 * 4
  let product1 = @azimuth.multiply(2, 3)
  let final_product = @azimuth.multiply(product1, 4)
  
  @azimuth.assert_eq(24, final_product)
}

test "mixed_operations_combination" {
  // 测试混合运算组合
  // 计算：(a * b) + (c * d) - (e * f)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  let e = 2
  let f = 7
  
  let product1 = @azimuth.multiply(a, b)  // 3 * 4 = 12
  let product2 = @azimuth.multiply(c, d)  // 5 * 6 = 30
  let product3 = @azimuth.multiply(e, f)  // 2 * 7 = 14
  
  let result = @azimuth.add(@azimuth.add(product1, product2), -product3)  // 12 + 30 - 14 = 28
  
  @azimuth.assert_eq(28, result)
}

test "performance_and_stress_test" {
  // 测试性能和压力情况
  let large_number = 1000000
  
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(large_number, large_number))
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_number, large_number))
  
  // 测试连续大数运算
  let result = @azimuth.add(@azimuth.multiply(large_number, 2), @azimuth.multiply(large_number, 3))
  @azimuth.assert_eq(5000000, result)  // 1000000*2 + 1000000*3 = 2000000 + 3000000 = 5000000
}