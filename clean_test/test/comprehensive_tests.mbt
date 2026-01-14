// 综合测试用例 - 测试 clean_test 模块的各种功能

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近极值的加法
  @clean_test.assert_eq(max_int - 1, @clean_test.add(max_int - 2, 1))
  @clean_test.assert_eq(min_int + 1, @clean_test.add(min_int + 2, -1))
  
  // 测试极值乘法
  @clean_test.assert_eq(0, @clean_test.multiply(min_int, 0))
  @clean_test.assert_eq(max_int, @clean_test.multiply(max_int, 1))
}

test "complex_string_operations" {
  // 测试复杂字符串操作
  let empty_string = ""
  let space_string = " "
  let unicode_string = "你好世界"
  
  // 测试空字符串和特殊字符
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(empty_string))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(space_string))
  @clean_test.assert_eq_string("Hello, 你好世界!", @clean_test.greet(unicode_string))
  
  // 测试包含数字和符号的字符串
  let complex_string = "User123@domain.com"
  @clean_test.assert_eq_string("Hello, User123@domain.com!", @clean_test.greet(complex_string))
}

test "mathematical_identities" {
  // 测试数学恒等式
  
  // 加法恒等式：a + 0 = a
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(1, @clean_test.add(1, 0))
  @clean_test.assert_eq(-1, @clean_test.add(-1, 0))
  @clean_test.assert_eq(100, @clean_test.add(100, 0))
  @clean_test.assert_eq(-100, @clean_test.add(-100, 0))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, 0))
  
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(1, @clean_test.add(0, 1))
  @clean_test.assert_eq(-1, @clean_test.add(0, -1))
  @clean_test.assert_eq(100, @clean_test.add(0, 100))
  @clean_test.assert_eq(-100, @clean_test.add(0, -100))
  @clean_test.assert_eq(2147483647, @clean_test.add(0, 2147483647))
  @clean_test.assert_eq(-2147483648, @clean_test.add(0, -2147483648))
  
  // 乘法恒等式：a * 1 = a, a * 0 = 0
  @clean_test.assert_eq(0, @clean_test.multiply(0, 1))
  @clean_test.assert_eq(1, @clean_test.multiply(1, 1))
  @clean_test.assert_eq(-1, @clean_test.multiply(-1, 1))
  @clean_test.assert_eq(100, @clean_test.multiply(100, 1))
  @clean_test.assert_eq(-100, @clean_test.multiply(-100, 1))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(2147483647, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 1))
  
  @clean_test.assert_eq(0, @clean_test.multiply(1, 0))
  @clean_test.assert_eq(1, @clean_test.multiply(1, 1))
  @clean_test.assert_eq(-1, @clean_test.multiply(1, -1))
  @clean_test.assert_eq(100, @clean_test.multiply(1, 100))
  @clean_test.assert_eq(-100, @clean_test.multiply(1, -100))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(1, 2147483647))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(1, -2147483648))
  
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(1, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-1, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(100, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-100, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(2147483647, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(-2147483648, 0))
}

test "practical_calculation_scenarios" {
  // 实际计算场景测试
  
  // 圆的面积计算（简化版，使用整数运算）
  // 面积 = π × r²，使用 π ≈ 3
  let radius = 10
  let area = @clean_test.multiply(3, @clean_test.multiply(radius, radius))
  @clean_test.assert_eq(300, area)
  
  // 简单的利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 2  // 2年
  let simple_interest = @clean_test.multiply(principal, @clean_test.multiply(rate, time)) / 100
  let total_amount = @clean_test.add(principal, simple_interest)
  @clean_test.assert_eq(11000, total_amount)
}

test "boolean_assertions" {
  // 测试布尔断言函数
  @clean_test.assert_true(true)
  @clean_test.assert_false(false)
  
  // 测试数值比较的布尔表达式
  @clean_test.assert_true(@clean_test.add(2, 3) == 5)
  @clean_test.assert_true(@clean_test.multiply(4, 5) == 20)
  @clean_test.assert_false(@clean_test.add(2, 3) == 6)
  @clean_test.assert_false(@clean_test.multiply(4, 5) == 21)
}

test "string_length_and_content" {
  // 测试字符串长度和内容
  let short_name = "A"
  let medium_name = "MoonBit"
  let long_name = "This is a very long string that contains multiple words and spaces"
  
  let short_greeting = @clean_test.greet(short_name)
  let medium_greeting = @clean_test.greet(medium_name)
  let long_greeting = @clean_test.greet(long_name)
  
  @clean_test.assert_eq_string("Hello, A!", short_greeting)
  @clean_test.assert_eq_string("Hello, MoonBit!", medium_greeting)
  @clean_test.assert_eq_string("Hello, " + long_name + "!", long_greeting)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  
  // 测试大数相乘的溢出保护
  let large_num = 1000000
  let result = @clean_test.multiply(large_num, large_num)
  // 结果应该是最大值，而不是溢出
  @clean_test.assert_true(result > 0)
  
  // 测试负数大数相乘
  let neg_large_num = -1000000
  let neg_result = @clean_test.multiply(neg_large_num, large_num)
  // 结果应该是负数
  @clean_test.assert_true(neg_result < 0)
}

test "sequential_operations" {
  // 测试连续运算
  
  // 计算多项式：3x² + 2x + 1，当 x = 5 时
  let x = 5
  let x_squared = @clean_test.multiply(x, x)  // x² = 25
  let term1 = @clean_test.multiply(3, x_squared)  // 3x² = 75
  let term2 = @clean_test.multiply(2, x)  // 2x = 10
  let polynomial_result = @clean_test.add(@clean_test.add(term1, term2), 1)  // 3x² + 2x + 1 = 86
  @clean_test.assert_eq(86, polynomial_result)
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = @clean_test.multiply(5, @clean_test.multiply(4, @clean_test.multiply(3, @clean_test.multiply(2, 1))))
  @clean_test.assert_eq(120, factorial_5)
}