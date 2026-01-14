// 新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(-50, @azimuth.add(-50, 0))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-9, @azimuth.multiply(9, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数的交换律
  let c = -12
  let d = 8
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1: 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(40, area)
  @azimuth.assert_eq(26, perimeter)
  
  // 场景2: 简单的利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionWithExtendedInput"
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
}

test "assertion_functions_verification" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试断言函数与计算结果结合
  let result1 = @azimuth.add(10, 20)
  @azimuth.assert_true(result1 == 30)
  @azimuth.assert_false(result1 != 30)
  
  let result2 = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result2)
  
  let result3 = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", result3)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1: 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping = 15
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(709, final_total)
  
  // 场景2: 温度转换（简化版）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 场景3: 平均值计算
  let scores_sum = @azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92)
  let average = scores_sum / 4
  @azimuth.assert_eq(86, average)
}

test "mathematical_series_calculations" {
  // 测试数学数列计算
  // 计算等差数列前5项和: 2 + 5 + 8 + 11 + 14 = 40
  let a1 = 2
  let d = 3
  let n = 5
  
  let term1 = a1
  let term2 = @azimuth.add(a1, d)
  let term3 = @azimuth.add(term2, d)
  let term4 = @azimuth.add(term3, d)
  let term5 = @azimuth.add(term4, d)
  
  let series_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(40, series_sum)
  
  // 计算等比数列前4项积: 2 * 4 * 8 * 16 = 1024
  let geo_term1 = 2
  let geo_term2 = @azimuth.multiply(geo_term1, 2)
  let geo_term3 = @azimuth.multiply(geo_term2, 2)
  let geo_term4 = @azimuth.multiply(geo_term3, 2)
  
  let geo_product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(geo_term1, geo_term2), geo_term3), geo_term4)
  @azimuth.assert_eq(1024, geo_product)
}