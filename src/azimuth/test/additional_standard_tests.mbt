// 额外的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  if (result1 != result2) {
    @builtin.abort("Addition commutative property test failed")
  }
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  if (left_side != right_side) {
    @builtin.abort("Multiplication distributive property test failed")
  }
}

test "add_identity_element" {
  // 测试加法单位元：a + 0 = a
  let value1 = 0
  let value2 = 1
  let value3 = -1
  let value4 = 100
  let value5 = -100
  let value6 = 2147483647
  let value7 = -2147483648
  
  if (@azimuth.add(value1, 0) != value1) {
    @builtin.abort("Addition identity element test failed for value: 0")
  }
  if (@azimuth.add(value2, 0) != value2) {
    @builtin.abort("Addition identity element test failed for value: 1")
  }
  if (@azimuth.add(value3, 0) != value3) {
    @builtin.abort("Addition identity element test failed for value: -1")
  }
  if (@azimuth.add(value4, 0) != value4) {
    @builtin.abort("Addition identity element test failed for value: 100")
  }
  if (@azimuth.add(value5, 0) != value5) {
    @builtin.abort("Addition identity element test failed for value: -100")
  }
  if (@azimuth.add(value6, 0) != value6) {
    @builtin.abort("Addition identity element test failed for value: 2147483647")
  }
  if (@azimuth.add(value7, 0) != value7) {
    @builtin.abort("Addition identity element test failed for value: -2147483648")
  }
}

test "multiply_identity_element" {
  // 测试乘法单位元：a × 1 = a
  let value1 = 0
  let value2 = 1
  let value3 = -1
  let value4 = 100
  let value5 = -100
  let value6 = 2147483647
  let value7 = -2147483648
  
  if (@azimuth.multiply(value1, 1) != value1) {
    @builtin.abort("Multiplication identity element test failed for value: 0")
  }
  if (@azimuth.multiply(value2, 1) != value2) {
    @builtin.abort("Multiplication identity element test failed for value: 1")
  }
  if (@azimuth.multiply(value3, 1) != value3) {
    @builtin.abort("Multiplication identity element test failed for value: -1")
  }
  if (@azimuth.multiply(value4, 1) != value4) {
    @builtin.abort("Multiplication identity element test failed for value: 100")
  }
  if (@azimuth.multiply(value5, 1) != value5) {
    @builtin.abort("Multiplication identity element test failed for value: -100")
  }
  if (@azimuth.multiply(value6, 1) != value6) {
    @builtin.abort("Multiplication identity element test failed for value: 2147483647")
  }
  if (@azimuth.multiply(value7, 1) != value7) {
    @builtin.abort("Multiplication identity element test failed for value: -2147483648")
  }
}

test "multiply_zero_property" {
  // 测试乘法零元素：a × 0 = 0
  let value1 = 1
  let value2 = -1
  let value3 = 100
  let value4 = -100
  let value5 = 2147483647
  let value6 = -2147483648
  
  if (@azimuth.multiply(value1, 0) != 0) {
    @builtin.abort("Multiplication zero property test failed for value: 1")
  }
  if (@azimuth.multiply(value2, 0) != 0) {
    @builtin.abort("Multiplication zero property test failed for value: -1")
  }
  if (@azimuth.multiply(value3, 0) != 0) {
    @builtin.abort("Multiplication zero property test failed for value: 100")
  }
  if (@azimuth.multiply(value4, 0) != 0) {
    @builtin.abort("Multiplication zero property test failed for value: -100")
  }
  if (@azimuth.multiply(value5, 0) != 0) {
    @builtin.abort("Multiplication zero property test failed for value: 2147483647")
  }
  if (@azimuth.multiply(value6, 0) != 0) {
    @builtin.abort("Multiplication zero property test failed for value: -2147483648")
  }
}

test "greet_multilingual_support" {
  // 测试多语言支持
  let name1 = "World"
  let name2 = "世界"
  let name3 = "Мир"
  let name4 = "العالم"
  let name5 = "🌍"
  
  let result1 = @azimuth.greet(name1)
  let expected1 = "Hello, " + name1 + "!"
  if (result1 != expected1) {
    @builtin.abort("Multilingual support test failed for name: " + name1)
  }
  
  let result2 = @azimuth.greet(name2)
  let expected2 = "Hello, " + name2 + "!"
  if (result2 != expected2) {
    @builtin.abort("Multilingual support test failed for name: " + name2)
  }
  
  let result3 = @azimuth.greet(name3)
  let expected3 = "Hello, " + name3 + "!"
  if (result3 != expected3) {
    @builtin.abort("Multilingual support test failed for name: " + name3)
  }
  
  let result4 = @azimuth.greet(name4)
  let expected4 = "Hello, " + name4 + "!"
  if (result4 != expected4) {
    @builtin.abort("Multilingual support test failed for name: " + name4)
  }
  
  let result5 = @azimuth.greet(name5)
  let expected5 = "Hello, " + name5 + "!"
  if (result5 != expected5) {
    @builtin.abort("Multilingual support test failed for name: " + name5)
  }
}

test "add_negative_numbers" {
  // 测试负数加法
  let result1 = @azimuth.add(-5, -3)
  if (result1 != -8) {
    @builtin.abort("Negative numbers addition test failed for: -5 + -3")
  }
  
  let result2 = @azimuth.add(-10, 5)
  if (result2 != -5) {
    @builtin.abort("Negative numbers addition test failed for: -10 + 5")
  }
  
  let result3 = @azimuth.add(-100, -200)
  if (result3 != -300) {
    @builtin.abort("Negative numbers addition test failed for: -100 + -200")
  }
  
  let result4 = @azimuth.add(-2147483648, 1)
  if (result4 != -2147483647) {
    @builtin.abort("Negative numbers addition test failed for: -2147483648 + 1")
  }
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  let result1 = @azimuth.multiply(-5, 3)
  if (result1 != -15) {
    @builtin.abort("Negative numbers multiplication test failed for: -5 × 3")
  }
  
  let result2 = @azimuth.multiply(5, -3)
  if (result2 != -15) {
    @builtin.abort("Negative numbers multiplication test failed for: 5 × -3")
  }
  
  let result3 = @azimuth.multiply(-5, -3)
  if (result3 != 15) {
    @builtin.abort("Negative numbers multiplication test failed for: -5 × -3")
  }
  
  let result4 = @azimuth.multiply(-1, 2147483647)
  if (result4 != -2147483647) {
    @builtin.abort("Negative numbers multiplication test failed for: -1 × 2147483647")
  }
}

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：(首项 + 末项) × 项数 / 2
  let first_term = 1
  let last_term = 100
  let number_of_terms = 100
  
  let expected_sum = @azimuth.multiply(@azimuth.add(first_term, last_term), number_of_terms) / 2
  let calculated_sum = 5050  // 1+2+3+...+100 = 5050
  
  if (expected_sum != calculated_sum) {
    @builtin.abort("Arithmetic sequence sum test failed")
  }
}

test "compound_interest_calculation" {
  // 测试复利计算：本金 × (1 + 利率)^年数
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算，避免浮点数
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor)
  let expected = 115000  // 1000 × (100 + 5×3) = 1000 × 115 = 115000
  
  if (total != expected) {
    @builtin.abort("Compound interest calculation test failed")
  }
}