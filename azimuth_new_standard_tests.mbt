// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，覆盖各种边界情况和功能测试

test "add_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(300, @azimuth.add(100, 200))
  @azimuth.assert_eq(1000, @azimuth.add(999, 1))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-2, @azimuth.add(-1, -1))
  @azimuth.assert_eq(-300, @azimuth.add(-100, -200))
  @azimuth.assert_eq(-15, @azimuth.add(-5, -10))
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(50, @azimuth.add(100, -50))
  @azimuth.assert_eq(-50, @azimuth.add(-100, 50))
}

test "multiply_by_one" {
  // 测试乘以1的性质
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-10, @azimuth.multiply(-10, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的性质
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(10, @azimuth.multiply(-10, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(83810205, @azimuth.multiply(12345, 6789))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_with_different_names" {
  // 测试不同名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test_user!", @azimuth.greet("test_user"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的名称
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, test-case!", @azimuth.greet("test-case"))
  @azimuth.assert_eq_string("Hello, user_123!", @azimuth.greet("user_123"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  // 加法恒等式：a + 0 = a
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  
  // 乘法恒等式：a * 1 = a
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(100, @azimuth.multiply(100, 1))
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  
  // 乘法零律：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-1, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // 表达式：(a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let sum = @azimuth.add(a, b)  // 8
  let result = @azimuth.multiply(sum, c)  // 32
  @azimuth.assert_eq(32, result)
  
  // 表达式：a * b + c * d
  let a2 = 10
  let b2 = 2
  let c2 = 3
  let d2 = 4
  let product1 = @azimuth.multiply(a2, b2)  // 20
  let product2 = @azimuth.multiply(c2, d2)  // 12
  let final_result = @azimuth.add(product1, product2)  // 32
  @azimuth.assert_eq(32, final_result)
}