// 额外的 MoonBit 测试用例
// 基于 azimuth 包中的 add、multiply、greet 等函数

test "calculation_parentheses_priority" {
  // 测试运算优先级和括号的影响
  let a = 5
  let b = 10
  let c = 2
  
  // 测试 (a + b) * c 与 a + b * c 的区别
  let with_parentheses = multiply(add(a, b), c)  // (5 + 10) * 2 = 30
  let without_parentheses = add(a, multiply(b, c))  // 5 + 10 * 2 = 25
  
  assert_eq(30, with_parentheses)
  assert_eq(25, without_parentheses)
}

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percentage) / 100
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  
  assert_eq(200, discount_amount)
  assert_eq(800, discounted_price)
}

test "compound_calculation" {
  // 测试复合计算：(a + b) * (c - d) + e
  let a = 8
  let b = 7
  let c = 15
  let d = 5
  let e = 10
  
  let sum_ab = add(a, b)  // 15
  let diff_cd = add(c, -d)  // 10
  let product = multiply(sum_ab, diff_cd)  // 150
  let result = add(product, e)  // 160
  
  assert_eq(160, result)
}

test "string_with_various_lengths" {
  // 测试不同长度字符串的处理
  let very_short = "A"
  let short = "Hi"
  let medium = "MoonBit"
  let long = "ThisIsALongStringForTesting"
  let very_long = "ThisIsAVeryVeryLongStringThatContainsManyCharactersToTestTheGreetFunctionsWithDifferentInputLengths"
  
  assert_eq_string("Hello, A!", greet(very_short))
  assert_eq_string("Hello, Hi!", greet(short))
  assert_eq_string("Hello, MoonBit!", greet(medium))
  assert_eq_string("Hello, ThisIsALongStringForTesting!", greet(long))
  assert_eq_string("Hello, ThisIsAVeryVeryLongStringThatContainsManyCharactersToTestTheGreetFunctionsWithDifferentInputLengths!", greet(very_long))
}

test "calculation_with_negative_numbers" {
  // 测试负数计算
  let a = -10
  let b = 5
  let c = -3
  
  // 负数加法
  assert_eq(-5, add(a, b))
  assert_eq(-13, add(a, c))
  
  // 负数乘法
  assert_eq(50, multiply(a, c))
  assert_eq(-30, multiply(b, c))
  
  // 混合运算
  let result = add(multiply(a, b), c)  // (-10) * 5 + (-3) = -53
  assert_eq(-53, result)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30 (简化公式)
  
  let freezing_celsius = 0
  let room_celsius = 20
  let boiling_celsius = 100
  
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  
  assert_eq(30, freezing_fahrenheit)  // 0°C ≈ 30°F (简化)
  assert_eq(70, room_fahrenheit)     // 20°C ≈ 70°F (简化)
  assert_eq(230, boiling_fahrenheit) // 100°C ≈ 230°F (简化)
}

test "calculation_chaining" {
  // 测试计算链：((((a + b) + c) + d) + e)
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  let e = 50
  
  let result = add(add(add(add(a, b), c), d), e)
  assert_eq(150, result)
  
  // 测试乘法链：a * b * c * d
  let m1 = 2
  let m2 = 3
  let m3 = 4
  let m4 = 5
  
  let product_result = multiply(multiply(multiply(m1, m2), m3), m4)
  assert_eq(120, product_result)
}

test "mixed_operations_complex" {
  // 测试混合运算的复杂情况
  let base_value = 100
  let multiplier = 3
  let addition = 50
  let subtraction = 25
  
  // 计算：(base_value * multiplier) + addition - subtraction
  let step1 = multiply(base_value, multiplier)  // 300
  let step2 = add(step1, addition)              // 350
  let final_result = add(step2, -subtraction)   // 325
  
  assert_eq(325, final_result)
}

test "string_special_characters_extended" {
  // 测试包含各种特殊字符的字符串
  let with_spaces = "John Doe"
  let with_symbols = "user@domain.com"
  let with_numbers = "Test123"
  let with_mixed = "User_2023@Project#1"
  let with_unicode = "测试中文"
  let with_emoji = "🌍🚀"
  
  assert_eq_string("Hello, John Doe!", greet(with_spaces))
  assert_eq_string("Hello, user@domain.com!", greet(with_symbols))
  assert_eq_string("Hello, Test123!", greet(with_numbers))
  assert_eq_string("Hello, User_2023@Project#1!", greet(with_mixed))
  assert_eq_string("Hello, 测试中文!", greet(with_unicode))
  assert_eq_string("Hello, 🌍🚀!", greet(with_emoji))
}

test "boundary_value_combinations_extended" {
  // 测试边界值的组合情况
  let max_val = 2147483647
  let min_val = -2147483648
  let one = 1
  let neg_one = -1
  
  // 最大值与不同值的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, one))
  assert_eq(max_val, add(max_val, max_val))
  
  // 最小值与不同值的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, neg_one))
  assert_eq(min_val, add(min_val, min_val))
  
  // 乘法边界测试
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, one))
  assert_eq(min_val, multiply(min_val, one))
  assert_eq(min_val, multiply(max_val, neg_one))
  assert_eq(max_val, multiply(min_val, neg_one))
}