// 额外的 MoonBit 测试用例
// 覆盖各种边界情况和实际应用场景

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 25), @azimuth.add(25, 15))
  @azimuth.assert_eq(@azimuth.add(-100, 50), @azimuth.add(50, -100))
  @azimuth.assert_eq(@azimuth.add(0, 12345), @azimuth.add(12345, 0))
}

test "multiply_function_commutativity" {
  // 测试乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(12, 15), @azimuth.multiply(15, 12))
  @azimuth.assert_eq(@azimuth.multiply(-20, 30), @azimuth.multiply(30, -20))
  @azimuth.assert_eq(@azimuth.multiply(0, 100), @azimuth.multiply(100, 0))
}

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(@azimuth.add(1000000, 2000000), 3000000)
  @azimuth.assert_eq(@azimuth.add(-1000000, -2000000), -3000000)
  @azimuth.assert_eq(@azimuth.add(1500000, -500000), 1000000)
}

test "multiplication_with_powers_of_two" {
  // 测试2的幂次方相乘
  @azimuth.assert_eq(@azimuth.multiply(2, 4), 8)
  @azimuth.assert_eq(@azimuth.multiply(8, 16), 128)
  @azimuth.assert_eq(@azimuth.multiply(32, 64), 2048)
  @azimuth.assert_eq(@azimuth.multiply(1024, 1024), 1048576)
}

test "greet_function_with_various_languages" {
  // 测试不同语言的问候语
  @azimuth.assert_eq_string(@azimuth.greet("世界"), "Hello, 世界!")
  @azimuth.assert_eq_string(@azimuth.greet("Mundo"), "Hello, Mundo!")
  @azimuth.assert_eq_string(@azimuth.greet("Monde"), "Hello, Monde!")
  @azimuth.assert_eq_string(@azimuth.greet("🌍"), "Hello, 🌍!")
}

test "complex_calculation_percentage" {
  // 测试百分比计算场景
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(final_price, 170)  // 200 - (200 * 15 / 100) = 200 - 30 = 170
}

test "geometric_calculation_perimeter" {
  // 测试几何计算：矩形周长
  let length = 15
  let width = 8
  
  let double_length = @azimuth.multiply(length, 2)
  let double_width = @azimuth.multiply(width, 2)
  let perimeter = @azimuth.add(double_length, double_width)
  
  @azimuth.assert_eq(perimeter, 46)  // 2*15 + 2*8 = 30 + 16 = 46
}

test "time_calculation_scenario" {
  // 测试时间计算场景
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 将所有时间转换为秒
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), 
                                   @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  
  @azimuth.assert_eq(total_seconds, 9045)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
}

test "string_length_processing" {
  // 测试字符串长度处理（通过greet函数间接测试）
  let short_name = "AI"
  let medium_name = "MoonBit"
  let long_name = "ArtificialIntelligence"
  
  let short_greeting = @azimuth.greet(short_name)
  let medium_greeting = @azimuth.greet(medium_name)
  let long_greeting = @azimuth.greet(long_name)
  
  @azimuth.assert_eq_string(short_greeting, "Hello, AI!")
  @azimuth.assert_eq_string(medium_greeting, "Hello, MoonBit!")
  @azimuth.assert_eq_string(long_greeting, "Hello, ArtificialIntelligence!")
}

test "arithmetic_sequence_calculation" {
  // 测试等差数列求和：1 + 3 + 5 + 7 + 9 = 25
  let term1 = 1
  let term2 = 3
  let term3 = 5
  let term4 = 7
  let term5 = 9
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  @azimuth.assert_eq(sum, 25)
}