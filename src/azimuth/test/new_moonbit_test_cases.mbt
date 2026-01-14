// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，对 azimuth 模块进行更全面的测试

test "add_negative_numbers" {
  // 测试负数加法的各种情况
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "add_mixed_sign_numbers" {
  // 测试混合符号数的加法
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(99, @azimuth.add(100, -1))
  @azimuth.assert_eq(-99, @azimuth.add(-100, 1))
}

test "multiply_negative_numbers" {
  // 测试负数乘法的各种情况
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "multiply_large_numbers" {
  // 测试大数乘法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(-1, min_val))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, émojis 🚀!", @azimuth.greet("émojis 🚀"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  let a = 12
  let b = -8
  let c = 4
  
  // 测试混合运算
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(-92, result1)
  
  let result2 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result2)
  
  let result3 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(16, result3)
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与边界值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 1!", @azimuth.greet("1"))
  
  // 测试长字符串
  let long_string = "This is a very long string that contains many words and characters"
  @azimuth.assert_eq_string("Hello, This is a very long string that contains many words and characters!", @azimuth.greet(long_string))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = -17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(y, 0))
  @azimuth.assert_eq(0, @azimuth.add(x, -x))
  @azimuth.assert_eq(0, @azimuth.add(y, -y))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(y, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(y, 0))
  @azimuth.assert_eq(-x, @azimuth.multiply(x, -1))
  @azimuth.assert_eq(-y, @azimuth.multiply(y, -1))
}

test "performance_sensitive_operations" {
  // 测试性能敏感的操作
  // 测试重复运算的一致性
  let a = 123
  let b = 456
  
  // 多次执行相同运算，结果应该一致
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(a, b)
  let result3 = @azimuth.add(a, b)
  @azimuth.assert_eq(result1, result2)
  @azimuth.assert_eq(result2, result3)
  
  let mul_result1 = @azimuth.multiply(a, b)
  let mul_result2 = @azimuth.multiply(a, b)
  let mul_result3 = @azimuth.multiply(a, b)
  @azimuth.assert_eq(mul_result1, mul_result2)
  @azimuth.assert_eq(mul_result2, mul_result3)
  
  // 测试链式运算的一致性
  let chain_result1 = @azimuth.add(@azimuth.add(a, b), @azimuth.add(a, b))
  let chain_result2 = @azimuth.multiply(2, @azimuth.add(a, b))
  @azimuth.assert_eq(chain_result1, chain_result2)
}