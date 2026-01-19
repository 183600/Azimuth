// 用户新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试大数相加
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
}

test "greet_function_special_chars" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, emoji🌍!", @azimuth.greet("emoji🌍"))
}

test "complex_calculations" {
  // 测试复合计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 计算 (a + b) × (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  let result = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(77, result)  // (3+4) × (5+6) = 7 × 11 = 77
}

test "overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "mathematical_formulas" {
  // 测试数学公式
  // 计算平方和：a² + b²
  let a = 3
  let b = 4
  let sum_of_squares = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(b, b))
  @azimuth.assert_eq(25, sum_of_squares)  // 3² + 4² = 9 + 16 = 25
  
  // 计算立方差：a³ - b³
  let c = 5
  let d = 2
  let c_cubed = @azimuth.multiply(@azimuth.multiply(c, c), c)
  let d_cubed = @azimuth.multiply(@azimuth.multiply(d, d), d)
  let difference = @azimuth.add(c_cubed, -d_cubed)
  @azimuth.assert_eq(117, difference)  // 5³ - 2³ = 125 - 8 = 117
}

test "string_concatenation" {
  // 测试字符串拼接
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
  @azimuth.assert_eq_string("Hello, line1\nline2!", @azimuth.greet("line1\nline2"))
}

test "real_world_applications" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let shipping = 10
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  @azimuth.assert_eq(360, cart_total)  // 100×2 + 50×3 + 10 = 200 + 150 + 10 = 360
  
  // 温度转换（更精确的简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160) / 5
  @azimuth.assert_eq(77, fahrenheit)  // (25 × 9 + 160) / 5 = (225 + 160) / 5 = 385 / 5 = 77
}

test "error_conditions" {
  // 测试错误条件处理
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}