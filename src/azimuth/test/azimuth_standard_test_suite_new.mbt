// 新增的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "integer_overflow_protection" {
  // 测试整数溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数加法溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 乘法恒等式：x × 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  
  // 乘法零律：x × 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, y))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景：计算矩形周长 = 2 × (长 + 宽)
  let length = 15
  let width = 8
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
  
  // 场景：计算复合利息（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total)
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -15
  let b = -7
  let c = 10
  
  // 负数加法
  @azimuth.assert_eq(-22, @azimuth.add(a, b))
  
  // 正负数加法
  @azimuth.assert_eq(-5, @azimuth.add(a, c))
  
  // 负数乘法
  @azimuth.assert_eq(105, @azimuth.multiply(a, b))
  
  // 正负数乘法
  @azimuth.assert_eq(-150, @azimuth.multiply(a, c))
}

test "special_value_handling" {
  // 测试特殊值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值与-1相乘的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最大值与-1相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 最小值与1相加
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 最大值与-1相加
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
}

test "sequential_operations" {
  // 测试连续运算
  // 连续加法：(a + b) + c
  let a = 10
  let b = 20
  let c = 30
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  @azimuth.assert_eq(60, result1)
  
  // 连续乘法：(a × b) × c
  let x = 2
  let y = 3
  let z = 4
  let result2 = @azimuth.multiply(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(24, result2)
  
  // 混合运算：a × (b + c)
  let mixed = @azimuth.multiply(a, @azimuth.add(b, c))
  @azimuth.assert_eq(500, mixed)
}

test "assert_function_functionality" {
  // 测试断言函数的功能
  // 测试成功的断言
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_eq(10, @azimuth.add(6, 4))
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件断言
  @azimuth.assert_true(@azimuth.add(2147483647, 1) == 2147483647)
  @azimuth.assert_false(@azimuth.add(2147483647, 1) > 2147483647)
}

test "practical_application_tests" {
  // 测试实际应用场景
  // 场景：计算购物车总价
  let item1_price = 99
  let item1_qty = 2
  let item2_price = 149
  let item2_qty = 1
  let shipping = 10
  
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  @azimuth.assert_eq(357, cart_total)
  
  // 场景：温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "unicode_and_internationalization" {
  // 测试Unicode和国际化支持
  // 测试中文字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  
  // 测试Emoji
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  
  // 测试混合语言
  @azimuth.assert_eq_string("Hello, Hello世界!", @azimuth.greet("Hello世界"))
  
  // 测试特殊Unicode字符
  @azimuth.assert_eq_string("Hello, ©®™!", @azimuth.greet("©®™"))
}