// 用户额外添加的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 模块的核心功能

test "add_function_comprehensive" {
  // 全面测试 add 函数
  // 基本加法
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  
  // 与零相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 连续相加
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.add(5, 5), 5))
}

test "multiply_function_comprehensive" {
  // 全面测试 multiply 函数
  // 基本乘法
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  
  // 与零相乘
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 与一相乘
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  @azimuth.assert_eq_string("Hello, This is a very long name!", @azimuth.greet("This is a very long name"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -5
  let d = 7
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "string_processing_various" {
  // 测试各种字符串处理
  // 包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  
  // 包含emoji的字符串
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // 计算矩形面积和周长
  let length = 15
  let width = 10
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(150, area)
  
  // 周长 = 2 × 长 + 2 × 宽
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(50, perimeter)
  
  // 对角线长度的平方（简化为整数计算）
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(325, diagonal_squared)  // 15² + 10² = 225 + 100 = 325
}