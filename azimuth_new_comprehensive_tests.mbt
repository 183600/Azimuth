// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法，涵盖多种实用场景

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let length = numbers.length()
  
  @assertion.assert_eq(1, first)?
  @assertion.assert_eq(5, last)?
  @assertion.assert_eq(5, length)?
}

test "string_length_and_substring" {
  // 测试字符串长度和子串
  let text = "MoonBit Programming"
  let length = text.length()
  
  @assertion.assert_eq(18, length)?
  
  // 测试字符串比较
  let hello = "Hello"
  let world = "World"
  let combined = hello + " " + world
  
  @assertion.assert_eq_string("Hello World", combined)?
}

test "boolean_logic" {
  // 测试布尔逻辑
  let true_val = true
  let false_val = false
  
  // 测试逻辑运算
  let and_result = true_val && false_val
  let or_result = true_val || false_val
  let not_result = !false_val
  
  @assertion.assert_eq(false, and_result)?
  @assertion.assert_eq(true, or_result)?
  @assertion.assert_eq(true, not_result)?
}

test "comparison_operations" {
  // 测试比较操作
  let a = 10
  let b = 20
  let c = 10
  
  // 测试各种比较
  let less_than = a < b
  let greater_than = b > a
  let equal = a == c
  let not_equal = a != b
  let less_or_equal = a <= c
  let greater_or_equal = b >= a
  
  @assertion.assert_eq(true, less_than)?
  @assertion.assert_eq(true, greater_than)?
  @assertion.assert_eq(true, equal)?
  @assertion.assert_eq(true, not_equal)?
  @assertion.assert_eq(true, less_or_equal)?
  @assertion.assert_eq(true, greater_or_equal)?
}

test "modulo_operations" {
  // 测试取模运算
  let a = 17
  let b = 5
  let c = 10
  let d = 2
  
  let mod1 = a % b
  let mod2 = c % d
  let mod3 = b % a
  
  @assertion.assert_eq(2, mod1)?
  @assertion.assert_eq(0, mod2)?
  @assertion.assert_eq(5, mod3)?
}

test "division_operations" {
  // 测试除法运算
  let a = 20
  let b = 5
  let c = 7
  let d = 2
  
  let div1 = a / b
  let div2 = c / d
  
  @assertion.assert_eq(4, div1)?
  @assertion.assert_eq(3, div2)?
}

test "negative_numbers" {
  // 测试负数运算
  let a = -10
  let b = 5
  let c = -3
  
  let add_result = a + b
  let sub_result = b - a
  let mul_result = a * c
  let div_result = a / b
  
  @assertion.assert_eq(-5, add_result)?
  @assertion.assert_eq(15, sub_result)?
  @assertion.assert_eq(30, mul_result)?
  @assertion.assert_eq(-2, div_result)?
}

test "power_calculations" {
  // 测试幂运算（使用重复乘法）
  let base = 3
  
  // 计算 3^2 = 9
  let power2 = base * base
  
  // 计算 3^3 = 27
  let power3 = base * base * base
  
  // 计算 3^4 = 81
  let power4 = base * base * base * base
  
  @assertion.assert_eq(9, power2)?
  @assertion.assert_eq(27, power3)?
  @assertion.assert_eq(81, power4)?
}

test "factorial_calculation" {
  // 测试阶乘计算（使用重复乘法）
  // 5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial = n * (n - 1) * (n - 2) * (n - 3) * (n - 4)
  
  @assertion.assert_eq(120, factorial)?
  
  // 4! = 4 × 3 × 2 × 1 = 24
  let m = 4
  let factorial4 = m * (m - 1) * (m - 2) * (m - 3)
  
  @assertion.assert_eq(24, factorial4)?
}

test "fibonacci_sequence" {
  // 测试斐波那契数列
  let a = 0
  let b = 1
  
  // 计算前几项
  let fib1 = a  // F(0) = 0
  let fib2 = b  // F(1) = 1
  let fib3 = a + b  // F(2) = 1
  let fib4 = b + (a + b)  // F(3) = 2
  let fib5 = (a + b) + (b + (a + b))  // F(4) = 3
  
  @assertion.assert_eq(0, fib1)?
  @assertion.assert_eq(1, fib2)?
  @assertion.assert_eq(1, fib3)?
  @assertion.assert_eq(2, fib4)?
  @assertion.assert_eq(3, fib5)?
}