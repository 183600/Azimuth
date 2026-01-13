// 额外的 MoonBit 测试用例
// 补充测试一些边界情况和特殊场景

test "addition_with_alternating_signs" {
  // 测试交替符号的加法运算
  @azimuth.assert_eq(@azimuth.add(100, -100), 0)
  @azimuth.assert_eq(@azimuth.add(-50, 50), 0)
  @azimuth.assert_eq(@azimuth.add(1000, -999), 1)
  @azimuth.assert_eq(@azimuth.add(-1000, 999), -1)
}

test "multiplication_by_powers_of_two" {
  // 测试乘以2的幂次方
  @azimuth.assert_eq(@azimuth.multiply(4, 4), 16)
  @azimuth.assert_eq(@azimuth.multiply(8, 4), 32)
  @azimuth.assert_eq(@azimuth.multiply(16, 4), 64)
  @azimuth.assert_eq(@azimuth.multiply(32, 4), 128)
}

test "string_greeting_with_numeric_strings" {
  // 测试数字字符串的问候
  @azimuth.assert_eq_string(@azimuth.greet("2023"), "Hello, 2023!")
  @azimuth.assert_eq_string(@azimuth.greet("007"), "Hello, 007!")
  @azimuth.assert_eq_string(@azimuth.greet("-123"), "Hello, -123!")
  @azimuth.assert_eq_string(@azimuth.greet("3.14"), "Hello, 3.14!")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let x = 5
  let y = 10
  let z = 15
  
  // 计算 (x + y) * (y - x) + z
  let sum_xy = @azimuth.add(x, y)
  let diff_yx = @azimuth.add(y, -x)
  let product = @azimuth.multiply(sum_xy, diff_yx)
  let result = @azimuth.add(product, z)
  
  @azimuth.assert_eq(result, 90)  // (5+10)*(10-5)+15 = 15*5+15 = 75+15 = 90
}

test "edge_case_zero_operations" {
  // 测试零的边界操作
  @azimuth.assert_eq(@azimuth.multiply(0, 2147483647), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, -2147483648), 0)
  @azimuth.assert_eq(@azimuth.add(0, 2147483647), 2147483647)
  @azimuth.assert_eq(@azimuth.add(0, -2147483648), -2147483648)
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string(@azimuth.greet("C++"), "Hello, C++!")
  @azimuth.assert_eq_string(@azimuth.greet("C#"), "Hello, C#!")
  @azimuth.assert_eq_string(@azimuth.greet("JavaScript"), "Hello, JavaScript!")
  @azimuth.assert_eq_string(@azimuth.greet("Python3.9"), "Hello, Python3.9!")
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 12
  let b = 18
  
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

test "calculation_with_large_numbers" {
  // 测试大数计算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  @azimuth.assert_eq(@azimuth.add(large_num1, large_num2), 3000000)
  
  // 大数相乘（使用较小的数避免溢出）
  @azimuth.assert_eq(@azimuth.multiply(1000, 2000), 2000000)
  
  // 大数与负数相加
  @azimuth.assert_eq(@azimuth.add(large_num1, -large_num2), -1000000)
}

test "string_greeting_multilingual" {
  // 测试多语言字符串问候
  @azimuth.assert_eq_string(@azimuth.greet("Español"), "Hello, Español!")
  @azimuth.assert_eq_string(@azimuth.greet("Français"), "Hello, Français!")
  @azimuth.assert_eq_string(@azimuth.greet("Deutsch"), "Hello, Deutsch!")
  @azimuth.assert_eq_string(@azimuth.greet("日本語"), "Hello, 日本語!")
  @azimuth.assert_eq_string(@azimuth.greet("한국어"), "Hello, 한국어!")
}

test "nested_function_calls_stress" {
  // 测试嵌套函数调用的压力测试
  
  // 嵌套加法：((((1+2)+3)+4)+5)
  let result1 = @azimuth.add(
    @azimuth.add(
      @azimuth.add(@azimuth.add(1, 2), 3), 4
    ), 5
  )
  @azimuth.assert_eq(result1, 15)
  
  // 嵌套乘法：(((1*2)*3)*4)
  let result2 = @azimuth.multiply(
    @azimuth.multiply(
      @azimuth.multiply(1, 2), 3
    ), 4
  )
  @azimuth.assert_eq(result2, 24)
}