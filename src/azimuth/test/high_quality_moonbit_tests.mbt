// 高质量 MoonBit 测试用例
// 测试 azimuth 模块的核心功能，覆盖各种场景和边界情况

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 25
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_properties_associativity" {
  // 测试数学结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let left_mul_assoc = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul_assoc = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul_assoc, right_mul_assoc)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 特殊情况：最小值乘以-1
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价（含运费）
  let subtotal = @azimuth.add(item1_total, item2_total)
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(398, item1_total)  // 199 * 2
  @azimuth.assert_eq(299, item2_total)  // 299 * 1
  @azimuth.assert_eq(697, subtotal)     // 398 + 299
  @azimuth.assert_eq(717, total)        // 697 + 20
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：温度转换
  // 摄氏度转华氏度：F = C * 9/5 + 32，简化为 F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "  Hello World  "
  @azimuth.assert_eq_string("Hello,   Hello World  !", @azimuth.greet(spaced_string))
  
  // 测试包含Unicode字符的字符串
  let unicode_string = "🌍🚀"
  @azimuth.assert_eq_string("Hello, 🌍🚀!", @azimuth.greet(unicode_string))
  
  // 测试非常长的字符串
  let long_string = "ThisIsAVeryLongStringThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
}

test "assert_functions_validation" {
  // 测试断言函数本身
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  
  // 测试 assert_eq
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-10, -10)
  @azimuth.assert_eq(0, 0)
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("hello", "hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}