// 新的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 测试 add 函数的基本功能
test "test_add_basic_functionality" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

// 测试 add 函数的边界值处理
test "test_add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试零值处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

// 测试 multiply 函数的基本功能
test "test_multiply_basic_functionality" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

// 测试 multiply 函数的边界值处理
test "test_multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

// 测试 greet 函数的基本功能
test "test_greet_basic_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

// 测试 greet 函数的特殊字符处理
test "test_greet_special_characters" {
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

// 测试复杂数学运算场景
test "test_complex_mathematical_scenarios" {
  // 测试 (a + b) * c 的计算
  let a = 5
  let b = 3
  let c = 4
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(32, result)
  
  // 测试 a * b + c * d 的计算
  let a2 = 10
  let b2 = 20
  let c2 = 30
  let d2 = 40
  let product1 = @azimuth.multiply(a2, b2)
  let product2 = @azimuth.multiply(c2, d2)
  let sum_result = @azimuth.add(product1, product2)
  @azimuth.assert_eq(1400, sum_result)
}

// 测试实际应用场景：购物车计算
test "test_shopping_cart_calculation" {
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 验证总价：299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  @azimuth.assert_eq(1294, cart_total)
}

// 测试数学性质：交换律和结合律
test "test_mathematical_properties" {
  let a = 12
  let b = 15
  let c = 20
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

// 测试实际应用场景：温度转换
test "test_temperature_conversion" {
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}