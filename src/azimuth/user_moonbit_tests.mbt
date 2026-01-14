// 用户自定义的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能，使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(-2, add(5, -7))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 123))
}

test "string_processing_functions" {
  // 测试字符串处理功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob123!", greet("Bob123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试负数的情况
  let c = -5
  let d = 15
  
  assert_eq(add(c, d), add(d, c))
  assert_eq(multiply(c, d), multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 99
  let item1_quantity = 2
  let item2_price = 149
  let item2_quantity = 1
  let shipping_fee = 12
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(add(total1, total2), shipping_fee)
  
  // 验证总价：99*2 + 149*1 + 12 = 198 + 149 + 12 = 359
  assert_eq(359, cart_total)
}

test "temperature_conversion_scenario" {
  // 测试实际应用场景：温度转换
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  
  let celsius_temp = 20
  let fahrenheit_temp = add(multiply(celsius_temp, 2), 30)
  assert_eq(70, fahrenheit_temp)
  
  // 测试冰点
  let freezing_temp = add(multiply(0, 2), 30)
  assert_eq(30, freezing_temp)
  
  // 测试沸点
  let boiling_temp = add(multiply(100, 2), 30)
  assert_eq(230, boiling_temp)
}

test "compound_calculations" {
  // 测试复合计算场景
  // 计算 (a + b) * (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 15
  
  let sum_ab = add(a, b)  // 15
  let diff_cd = add(c, -d)  // 12
  let product = multiply(sum_ab, diff_cd)  // 180
  let result = add(product, e)  // 195
  
  assert_eq(195, result)
}

test "special_character_strings" {
  // 测试特殊字符字符串处理
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
  assert_eq_string("Hello, John_Doe123!", greet("John_Doe123"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced   "))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "assertion_functions_test" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试计算结果的断言
  let result = add(17, 25)
  assert_true(result > 40)
  assert_false(result < 40)
}

test "mathematical_series_calculation" {
  // 测试数学数列计算
  // 计算等差数列的和：3 + 6 + 9 + ... + 30（前10个3的倍数）
  // 使用公式：n × (首项 + 末项) / 2
  
  let n = 10
  let first = 3
  let last = 30
  
  let series_sum = multiply(n, add(first, last)) / 2
  assert_eq(165, series_sum)  // 10 × (3 + 30) / 2 = 165
}