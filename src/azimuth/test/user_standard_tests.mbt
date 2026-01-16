// MoonBit 标准测试用例
// 测试 Azimuth 项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(3, -13))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
}

test "greet_function_variations" {
  // 测试问候函数的各种变体
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 王小明!", @azimuth.greet("王小明"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 17
  let b = 29
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -15
  let d = 23
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算 (a + b) * (c - d) + e
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  let e = 15
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 20
  let diff_cd = @azimuth.add(c, -d)  // 15
  let product = @azimuth.multiply(sum_ab, diff_cd)  // 300
  let result = @azimuth.add(product, e)  // 315
  
  @azimuth.assert_eq(315, result)
}

test "real_world_application_calculate_total_price" {
  // 测试实际应用：计算商品总价
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  // 应用折扣
  let discounted_price = @azimuth.add(subtotal, -discount)  // 547
  // 加上运费
  let total_price = @azimuth.add(discounted_price, shipping)  // 562
  
  @azimuth.assert_eq(562, total_price)
}

test "mathematical_series_sum" {
  // 测试数学数列求和：计算 1 + 2 + 3 + ... + 10
  // 使用公式：n * (n + 1) / 2
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  @azimuth.assert_eq(55, sum)
  
  // 验证前几项
  let sum_first_5 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum_first_5)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含特殊符号的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 包含数字的字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // 使用简化公式：F = C * 2 + 30（避免浮点数）
  
  // 测试冰点
  let celsius_0 = 0
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  // 测试室温
  let celsius_20 = 20
  let fahrenheit_20 = @azimuth.add(@azimuth.multiply(celsius_20, 2), 30)
  @azimuth.assert_eq(70, fahrenheit_20)
  
  // 测试沸点
  let celsius_100 = 100
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_100)
}

test "assertion_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-17, -17)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("测试", "测试")
}