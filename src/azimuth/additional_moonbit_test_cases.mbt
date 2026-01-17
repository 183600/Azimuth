// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
}

test "multiply_large_boundary" {
  // 测试大数边界值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_multilingual" {
  // 测试多语言字符串
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "add_sequence_pattern" {
  // 测试序列加法模式
  let base = 100
  let increment = 25
  
  let result1 = add(base, increment)
  let result2 = add(result1, increment)
  let result3 = add(result2, increment)
  
  assert_eq(125, result1)
  assert_eq(150, result2)
  assert_eq(175, result3)
}

test "multiply_power_pattern" {
  // 测试乘法幂模式
  let base = 3
  
  let power2 = multiply(base, base)        // 3^2 = 9
  let power3 = multiply(power2, base)      // 3^3 = 27
  let power4 = multiply(power3, base)      // 3^4 = 81
  
  assert_eq(9, power2)
  assert_eq(27, power3)
  assert_eq(81, power4)
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式
  let a = 12
  let b = 8
  let c = 5
  let d = 3
  
  // 计算 (a + b) * (c - d) + a * d
  let sum = add(a, b)                      // 20
  let diff = add(c, -d)                    // 2
  let product1 = multiply(sum, diff)       // 40
  let product2 = multiply(a, d)            // 36
  let result = add(product1, product2)     // 76
  
  assert_eq(76, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a b c!", greet("a b c"))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(46341, 46341))
}

test "mixed_operations_chain" {
  // 测试混合运算链
  let start = 10
  
  // ((start * 2) + 5) * 3 - 10
  let step1 = multiply(start, 2)           // 20
  let step2 = add(step1, 5)                // 25
  let step3 = multiply(step2, 3)           // 75
  let result = add(step3, -10)             // 65
  
  assert_eq(65, result)
}