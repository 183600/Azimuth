// Azimuth 增强单元测试用例
// 使用标准 MoonBit 测试语法

test "add_associative_property" {
  // 加法结合律测试
  let a = 12
  let b = 23
  let c = 35
  
  // (a + b) + c = a + (b + c)
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
}

test "multiply_distributive_property" {
  // 乘法分配律测试
  let a = 7
  let b = 5
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let sum = @azimuth.add(b, c)
  let left = @azimuth.multiply(a, sum)
  
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(a, c)
  let right = @azimuth.add(product1, product2)
  
  @azimuth.assert_eq(left, right)
}

test "maximum_integer_boundary" {
  // 整数边界测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(@azimuth.add(max_int, 0), max_int)
  @azimuth.assert_eq(@azimuth.add(min_int, 0), min_int)
  
  // 边界值乘法
  @azimuth.assert_eq(@azimuth.multiply(max_int, 1), max_int)
  @azimuth.assert_eq(@azimuth.multiply(min_int, 1), min_int)
}

test "string_length_variations" {
  // 不同长度字符串测试
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, AB!", @azimuth.greet("AB"))
  @azimuth.assert_eq_string("Hello, ABC!", @azimuth.greet("ABC"))
  @azimuth.assert_eq_string("Hello, ABCD!", @azimuth.greet("ABCD"))
  @azimuth.assert_eq_string("Hello, ABCDE!", @azimuth.greet("ABCDE"))
}

test "arithmetic_sequence_sum" {
  // 等差数列求和测试
  // 1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let sum4 = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, sum4)
  
  // 使用公式 n * (n + 1) / 2 = 5 * 6 / 2 = 15
  let n = 5
  let formula_result = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(15, formula_result)
}

test "factorial_calculation" {
  // 阶乘计算测试
  // 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n = 5
  let step1 = @azimuth.multiply(n, 4)  // 20
  let step2 = @azimuth.multiply(step1, 3)  // 60
  let step3 = @azimuth.multiply(step2, 2)  // 120
  let step4 = @azimuth.multiply(step3, 1)  // 120
  @azimuth.assert_eq(120, step4)
}

test "geometric_progression" {
  // 等比数列测试
  // 2, 4, 8, 16, 32 (公比为2)
  let first = 2
  let ratio = 2
  
  let term2 = @azimuth.multiply(first, ratio)  // 4
  let term3 = @azimuth.multiply(term2, ratio)  // 8
  let term4 = @azimuth.multiply(term3, ratio)  // 16
  let term5 = @azimuth.multiply(term4, ratio)  // 32
  
  @azimuth.assert_eq(4, term2)
  @azimuth.assert_eq(8, term3)
  @azimuth.assert_eq(16, term4)
  @azimuth.assert_eq(32, term5)
}

test "greet_with_unicode_characters" {
  // Unicode字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
}

test "compound_interest_calculation" {
  // 复利计算测试
  // 本金 * (1 + 利率) ^ 年数，简化计算
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 第一年: 1000 * (1 + 0.05) = 1050
  let year1 = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  
  // 第二年: 1050 * (1 + 0.05) = 1102.5，简化为1102
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  
  // 第三年: 1102 * (1 + 0.05) = 1157.1，简化为1157
  let year3 = @azimuth.add(year2, @azimuth.multiply(year2, rate) / 100)
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1102, year2)
  @azimuth.assert_eq(1157, year3)
}

test "binary_conversion_simulation" {
  // 二进制转换模拟测试
  // 将数字13转换为二进制的模拟计算
  // 13 = 8 + 4 + 0 + 1 = 1101
  
  let number = 13
  let bit8 = @azimuth.multiply(number >= 8 ? 1 : 0, 8)
  let remainder1 = number - bit8
  
  let bit4 = @azimuth.multiply(remainder1 >= 4 ? 1 : 0, 4)
  let remainder2 = remainder1 - bit4
  
  let bit2 = @azimuth.multiply(remainder2 >= 2 ? 1 : 0, 2)
  let remainder3 = remainder2 - bit2
  
  let bit1 = @azimuth.multiply(remainder3 >= 1 ? 1 : 0, 1)
  
  @azimuth.assert_eq(8, bit8)
  @azimuth.assert_eq(4, bit4)
  @azimuth.assert_eq(0, bit2)
  @azimuth.assert_eq(1, bit1)
  
  // 验证总和等于原数
  let total = @azimuth.add(@azimuth.add(@azimuth.add(bit8, bit4), bit2), bit1)
  @azimuth.assert_eq(number, total)
}