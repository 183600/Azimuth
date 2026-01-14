// 新增的 MoonBit 测试用例
// 这些测试用例涵盖各种场景，包括边界条件、实际应用和算法验证

test "basic_addition_patterns" {
  // 测试基本的加法模式
  let result1 = add(10, 20)
  assert_eq(30, result1)
  
  let result2 = add(-10, 20)
  assert_eq(10, result2)
  
  let result3 = add(0, 100)
  assert_eq(100, result3)
}

test "basic_multiplication_patterns" {
  // 测试基本的乘法模式
  let result1 = multiply(5, 6)
  assert_eq(30, result1)
  
  let result2 = multiply(-5, 6)
  assert_eq(-30, result2)
  
  let result3 = multiply(5, 0)
  assert_eq(0, result3)
}

test "string_greeting_patterns" {
  // 测试字符串问候模式
  let result1 = greet("Alice")
  assert_eq_string("Hello, Alice!", result1)
  
  let result2 = greet("")
  assert_eq_string("Hello, !", result2)
  
  let result3 = greet("123")
  assert_eq_string("Hello, 123!", result3)
}

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let sum = add(add(add(add(add(1, 2), 3), 4), 5), add(add(add(add(add(6, 7), 8), 9), 10))
  assert_eq(55, sum)
}

test "compound_calculation" {
  // 复合计算：(a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(75, result)
}

test "boundary_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "conditional_calculation" {
  // 条件计算测试
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { add(x, y) } else { multiply(x, y) }
  
  assert_eq(35, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { add(x, y) } else { multiply(x, y) }
  
  assert_eq(300, result2)
}

test "sequential_operations" {
  // 连续运算测试
  // 计算：((a + b) * (c + d)) - (e * f)
  let a = 5
  let b = 10
  let c = 3
  let d = 7
  let e = 4
  let f = 6
  
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let product1 = multiply(sum1, sum2)
  let product2 = multiply(e, f)
  let result = add(product1, -product2)
  
  assert_eq(104, result)
}

test "string_processing_with_numbers" {
  // 数字字符串处理测试
  let number_str = "2023"
  let result = greet(number_str)
  assert_eq_string("Hello, 2023!", result)
  
  let mixed_str = "User123"
  let result2 = greet(mixed_str)
  assert_eq_string("Hello, User123!", result2)
}

test "calculation_with_division" {
  // 包含除法的计算测试
  let dividend = 100
  let divisor = 4
  
  // 计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  assert_eq(25, quotient)
  assert_eq(0, remainder)
  
  // 使用向上取整除法
  let ceil_result = divide_with_ceil(dividend, divisor)
  assert_eq(25, ceil_result)
  
  // 测试有余数的情况
  let dividend2 = 101
  let ceil_result2 = divide_with_ceil(dividend2, divisor)
  assert_eq(26, ceil_result2)
}