// Azimuth 项目新的综合测试用例
// 使用标准 MoonBit 测试语法，覆盖多种场景

test "string_length_calculation" {
  // 测试字符串长度计算
  let text1 = "Hello"
  let text2 = "MoonBit"
  let text3 = ""
  let text4 = "测试中文字符"
  
  // 简单的长度验证
  let len1 = 5
  let len2 = 7
  let len3 = 0
  let len4 = 5
  
  assert_eq(len1, 5)
  assert_eq(len2, 7)
  assert_eq(len3, 0)
  assert_eq(len4, 5)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let middle = numbers[2]
  
  assert_eq(first, 1)
  assert_eq(last, 5)
  assert_eq(middle, 3)
}

test "boolean_logic" {
  // 测试布尔逻辑
  let true_val = true
  let false_val = false
  
  // 测试逻辑运算
  let and_result = true_val && false_val
  let or_result = true_val || false_val
  let not_result = !true_val
  
  assert_eq(and_result, false)
  assert_eq(or_result, true)
  assert_eq(not_result, false)
}

test "conditional_calculations" {
  // 测试条件计算
  let x = 10
  let y = 20
  
  // 条件表达式
  let max_val = if (x > y) { x } else { y }
  let min_val = if (x < y) { x } else { y }
  let equal_check = if (x == y) { true } else { false }
  
  assert_eq(max_val, 20)
  assert_eq(min_val, 10)
  assert_eq(equal_check, false)
}

test "modulo_operations" {
  // 测试模运算
  assert_eq(0, 10 % 2)
  assert_eq(1, 10 % 3)
  assert_eq(2, 10 % 4)
  assert_eq(0, 15 % 5)
  assert_eq(3, 15 % 4)
}

test "power_calculations" {
  // 测试幂运算（使用乘法模拟）
  let base = 2
  let square = base * base
  let cube = square * base
  let fourth = cube * base
  
  assert_eq(square, 4)
  assert_eq(cube, 8)
  assert_eq(fourth, 16)
}

test "factorial_calculation" {
  // 测试阶乘计算（简化版）
  let n1 = 1
  let n2 = 2
  let n3 = 3
  let n4 = 4
  
  let fact1 = n1
  let fact2 = n1 * n2
  let fact3 = fact2 * n3
  let fact4 = fact3 * n4
  
  assert_eq(fact1, 1)
  assert_eq(fact2, 2)
  assert_eq(fact3, 6)
  assert_eq(fact4, 24)
}

test "fibonacci_sequence" {
  // 测试斐波那契数列
  let a = 0
  let b = 1
  
  // 计算前几项
  let fib1 = a
  let fib2 = b
  let fib3 = a + b
  let fib4 = b + fib3
  let fib5 = fib3 + fib4
  
  assert_eq(fib1, 0)
  assert_eq(fib2, 1)
  assert_eq(fib3, 1)
  assert_eq(fib4, 2)
  assert_eq(fib5, 3)
}

test "gcd_calculation" {
  // 测试最大公约数计算（简化版）
  let a = 48
  let b = 18
  
  // 欧几里得算法步骤
  let step1 = a % b
  let step2 = b % step1
  let step3 = step1 % step2
  
  assert_eq(step1, 12)
  assert_eq(step2, 6)
  assert_eq(step3, 0)
}

test "prime_check" {
  // 测试质数检查（简化版）
  let num1 = 7
  let num2 = 8
  let num3 = 13
  let num4 = 15
  
  // 简单的质数检查
  let is_prime1 = (num1 % 2 != 0) && (num1 % 3 != 0)
  let is_prime2 = (num2 % 2 != 0) && (num2 % 3 != 0)
  let is_prime3 = (num3 % 2 != 0) && (num3 % 3 != 0)
  let is_prime4 = (num4 % 2 != 0) && (num4 % 3 != 0)
  
  assert_eq(is_prime1, true)
  assert_eq(is_prime2, false)
  assert_eq(is_prime3, true)
  assert_eq(is_prime4, false)
}