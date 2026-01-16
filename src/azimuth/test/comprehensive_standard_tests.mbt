// 综合标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "integer_addition_comprehensive" {
  // 测试整数加法的各种情况
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(-20, @azimuth.add(-15, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
}

test "integer_multiplication_comprehensive" {
  // 测试整数乘法的各种情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(64, @azimuth.multiply(8, 8))
  @azimuth.assert_eq(17, @azimuth.multiply(17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(17, -1))
}

test "string_greeting_variations" {
  // 测试字符串问候功能的各种情况
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "boundary_value_operations" {
  // 测试边界值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 18
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆的面积和周长计算（简化版）
  let radius = 5
  let pi = 3
  
  // 周长 = 2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 面积 = π × r²
  let area = @azimuth.multiply(pi, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
}

test "real_world_business_calculations" {
  // 测试实际业务计算
  // 订单总价计算
  let unit_price = 99
  let quantity = 4
  let discount = 20
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算最终总价
  let final_total = @azimuth.add(discounted_price, shipping)
  
  // 验证结果：99 × 4 - 20 + 15 = 396 - 20 + 15 = 391
  @azimuth.assert_eq(391, final_total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello,  Multiple  Spaces !", @azimuth.greet(" Multiple  Spaces "))
  
  // 包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John Doe\"!", @azimuth.greet("\"John Doe\""))
  
  // 包含换行符的字符串
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
}

test "sequential_operations" {
  // 测试连续运算
  let a = 5
  let b = 10
  let c = 15
  
  // 连续加法
  let sum_result = @azimuth.add(@azimuth.add(a, b), c)
  @azimuth.assert_eq(30, sum_result)
  
  // 连续乘法
  let product_result = @azimuth.multiply(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(750, product_result)
  
  // 混合运算：(a + b) × c
  let mixed_result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(225, mixed_result)
}

test "temperature_conversion_simulation" {
  // 测试温度转换模拟（整数版本）
  // 摄氏度转华氏度：F = C × 2 + 30（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化版）
  let fahrenheit_back = 80
  let celsius_back = @azimuth.add(fahrenheit_back, -30) / 2
  @azimuth.assert_eq(25, celsius_back)
  
  // 测试冰点和沸点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))  // 0°C = 30°F
  @azimuth.assert_eq(230, @azimuth.add(@azimuth.multiply(100, 2), 30))  // 100°C = 230°F
}