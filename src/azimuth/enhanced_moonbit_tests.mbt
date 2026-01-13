// 增强的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，提供更多高级功能测试

// 测试用例 1: 负数运算验证
test "negative_arithmetic_operations" {
  // 测试负数加法
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-10, 10))
  assert_eq(5, add(-15, 20))
  
  // 测试负数乘法
  assert_eq(25, multiply(-5, -5))
  assert_eq(-50, multiply(-5, 10))
  assert_eq(50, multiply(-10, -5))
}

// 测试用例 2: 大数运算验证
test "large_number_operations" {
  // 测试接近边界的加法
  let near_max = 2147483640
  let near_min = -2147483640
  
  assert_eq(2147483647, add(near_max, 7))  // 接近最大值
  assert_eq(-2147483648, add(near_min, -8))  // 接近最小值
  
  // 测试大数乘法（不溢出的情况）
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(4000000, multiply(2000, 2000))
}

// 测试用例 3: 字符串边界情况
test "string_boundary_cases" {
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试单个字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \\\"\\'!", greet("\\\"\\'"))
}

// 测试用例 4: 复合数学运算
test "compound_mathematical_operations" {
  // 测试 (a * b) + (c * d) 的复合运算
  let a = 10
  let b = 5
  let c = 3
  let d = 4
  
  let product1 = multiply(a, b)  // 10 * 5 = 50
  let product2 = multiply(c, d)  // 3 * 4 = 12
  let result = add(product1, product2)  // 50 + 12 = 62
  
  assert_eq(62, result)
}

// 测试用例 5: 递归式计算模拟
test "recursive_calculation_simulation" {
  // 模拟计算阶乘（简化版，只计算小数）
  // 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial_5)
  
  // 模拟计算斐波那契数（简化版）
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let fib_4 = add(2, 1)  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let fib_3 = add(1, 1)  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let fib_5 = add(fib_4, fib_3)
  assert_eq(5, fib_5)
}

// 测试用例 6: 条件表达式测试
test "conditional_expression_tests" {
  // 测试基于数学结果的条件分支
  let x = 15
  let y = 3
  
  // 如果 x 能被 y 整除，则计算商，否则计算和
  let is_divisible = (x / y) * y == x
  let result = if is_divisible { x / y } else { add(x, y) }
  
  // 由于 15 能被 3 整除，所以应该是商：15 / 3 = 5
  assert_eq(5, result)
  
  // 测试不能整除的情况
  let a = 16
  let b = 3
  let is_divisible2 = (a / b) * b == a
  let result2 = if is_divisible2 { a / b } else { add(a, b) }
  
  // 由于 16 不能被 3 整除，所以应该是和：16 + 3 = 19
  assert_eq(19, result2)
}

// 测试用例 7: 字符串处理逻辑
test "string_processing_logic" {
  // 测试字符串长度相关的逻辑（通过拼接验证）
  let short = "Hi"
  let medium = "Hello"
  let long = "Good morning"
  
  let short_result = greet(short)
  let medium_result = greet(medium)
  let long_result = greet(long)
  
  // 验证字符串拼接正确性
  assert_true(short_result.length() < medium_result.length())
  assert_true(medium_result.length() < long_result.length())
}

// 测试用例 8: 数学恒等式验证
test "mathematical_identities" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = multiply(a, add(b, c))  // 5 * (3 + 2) = 5 * 5 = 25
  let right_side = add(multiply(a, b), multiply(a, c))  // 5 * 3 + 5 * 2 = 15 + 10 = 25
  
  assert_eq(left_side, right_side)
  
  // 验证结合律：（a + b）+ c = a + (b + c)
  let x = 10
  let y = 20
  let z = 30
  
  let associativity1 = add(add(x, y), z)  // (10 + 20) + 30 = 30 + 30 = 60
  let associativity2 = add(x, add(y, z))  // 10 + (20 + 30) = 10 + 50 = 60
  
  assert_eq(associativity1, associativity2)
}

// 测试用例 9: 边界值组合测试
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试最小值与1的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试最大值与-1的运算
  assert_eq(max_val - 1, add(max_val, -1))
  assert_eq(min_val, multiply(max_val, -1))
  
  // 测试最小值与-1的运算
  assert_eq(min_val, multiply(min_val, -1))
}

// 测试用例 10: 实际应用场景 - 银行利息计算
test "bank_interest_calculation" {
  // 简化的银行利息计算：本金 + 本金 × 利率 × 时间
  let principal = 10000  // 本金10000元
  let rate = 3  // 年利率3%
  let years = 5  // 5年
  
  // 计算利息：10000 × 3 × 5 = 150000（表示1500元）
  let interest = multiply(principal, multiply(rate, years))
  
  // 计算本息和：10000 + 150000 = 160000（表示16000元）
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}