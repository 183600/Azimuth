// MoonBit 测试示例
// 包含10个测试用例，覆盖 azimuth 模块的核心功能

test "add_function_comprehensive_test" {
  // 测试 add 函数的综合情况
  // 基本加法
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  
  // 零值加法
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  
  // 大数加法
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
}

test "multiply_function_comprehensive_test" {
  // 测试 multiply 函数的综合情况
  // 基本乘法
  @azimuth.assert_eq(56, @azimuth.multiply(7, 8))
  @azimuth.assert_eq(-24, @azimuth.multiply(-6, 4))
  @azimuth.assert_eq(35, @azimuth.multiply(-5, -7))
  
  // 零值和一值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(99, @azimuth.multiply(99, 1))
  @azimuth.assert_eq(-99, @azimuth.multiply(99, -1))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  // 基本字符串
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // Unicode 字符串
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "divide_with_ceil_function_test" {
  // 测试 divide_with_ceil 函数的各种情况
  // 基本整除
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  
  // 需要向上取整的情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  
  // 零值情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 15
  let discount = 50
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)
  
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping_fee)
  
  // 验证结果：299*2 + 199*3 - 50 + 15 = 598 + 597 - 50 + 15 = 1160
  @azimuth.assert_eq(1160, final_total)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 15
  let b = 8
  let c = 7
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
  
  // 结合律：(a + b) + c = a + (b + c)
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
}

test "boundary_values_extreme_test" {
  // 测试边界值的极端情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最大值和最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "real_world_application_test" {
  // 测试实际应用场景：温度转换计算
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 简化为整数计算：F = C * 2 + 30
  
  let celsius_temps = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 测试温度转换
  let f1 = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  let f2 = @azimuth.add(@azimuth.multiply(celsius_temps[1], 2), 30)
  let f3 = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  let f4 = @azimuth.add(@azimuth.multiply(celsius_temps[3], 2), 30)
  let f5 = @azimuth.add(@azimuth.multiply(celsius_temps[4], 2), 30)
  let f6 = @azimuth.add(@azimuth.multiply(celsius_temps[5], 2), 30)
  
  @azimuth.assert_eq(expected_fahrenheit[0], f1)
  @azimuth.assert_eq(expected_fahrenheit[1], f2)
  @azimuth.assert_eq(expected_fahrenheit[2], f3)
  @azimuth.assert_eq(expected_fahrenheit[3], f4)
  @azimuth.assert_eq(expected_fahrenheit[4], f5)
  @azimuth.assert_eq(expected_fahrenheit[5], f6)
}

test "assertion_functions_test" {
  // 测试断言函数的各种情况
  // assert_true 和 assert_false
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  
  // assert_eq 测试
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq(-50, -50)
  @azimuth.assert_eq(0, 0)
  
  // assert_eq_string 测试
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Test123", "Test123")
  
  // 与函数结果结合的断言
  let add_result = @azimuth.add(25, 17)
  @azimuth.assert_eq(42, add_result)
  
  let multiply_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, multiply_result)
  
  let greet_result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", greet_result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边缘情况
  // 溢出保护验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.add(min_val, min_val) == min_val)
  
  // 乘法溢出保护
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)
  
  // 特殊情况处理
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 边界值的正确处理
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}