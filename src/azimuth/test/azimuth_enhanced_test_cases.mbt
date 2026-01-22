// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，覆盖多种场景

// 测试数组操作
test "array_operations" {
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  
  @azimuth.assert_eq(first, 1)
  @azimuth.assert_eq(last, 5)
}

// 测试条件分支
test "conditional_logic" {
  let x = 10
  let y = 20
  
  let max = if (x > y) { x } else { y }
  let min = if (x < y) { x } else { y }
  
  @azimuth.assert_eq(max, 20)
  @azimuth.assert_eq(min, 10)
}

// 测试字符串比较
test "string_comparison" {
  let short = "Hi"
  let medium = "Hello, World!"
  let long = "This is a very long string for testing purposes"
  
  assert_true(short < medium)
  assert_true(medium < long)
}

// 测试数值范围和边界
test "numeric_ranges" {
  let positive = 42
  let negative = -42
  let zero = 0
  
  assert_true(positive > 0)
  assert_true(negative < 0)
  assert_true(zero == 0)
  
  assert_true(positive > negative)
  assert_true(negative < zero)
  assert_true(positive > zero)
}

// 测试布尔逻辑
test "boolean_logic" {
  let true_val = true
  let false_val = false
  
  assert_true(true_val && true_val)
  assert_true(true_val || false_val)
  assert_false(false_val && false_val)
  assert_false(false_val || false_val)
  
  assert_true(true_val != false_val)
  assert_false(true_val == false_val)
}

// 测试函数组合
test "function_composition" {
  // 使用项目中的函数
  let input = 5
  
  // 使用 add 和 multiply 函数组合
  let doubled = @azimuth.multiply(input, 2)
  let squared = @azimuth.multiply(input, input)
  let double_then_add = @azimuth.add(doubled, 10)
  let square_then_add = @azimuth.add(squared, 10)
  
  @azimuth.assert_eq(doubled, 10)
  @azimuth.assert_eq(squared, 25)
  @azimuth.assert_eq(double_then_add, 20)
  @azimuth.assert_eq(square_then_add, 35)
}

// 测试累加计算
test "accumulation_calculations" {
  // 手动实现累加，不使用循环
  let num1 = 1
  let num2 = 2
  let num3 = 3
  let num4 = 4
  let num5 = 5
  let num6 = 6
  let num7 = 7
  let num8 = 8
  let num9 = 9
  let num10 = 10
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5), @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num6, num7), num8), num9), num10))
  
  // 计算偶数和
  let even_sum = @azimuth.add(@azimuth.add(@azimuth.add(num2, num4), num6), @azimuth.add(num8, num10))
  
  // 计算奇数和
  let odd_sum = @azimuth.add(@azimuth.add(@azimuth.add(num1, num3), num5), @azimuth.add(num7, num9))
  
  @azimuth.assert_eq(sum, 55)
  @azimuth.assert_eq(even_sum, 30)
  @azimuth.assert_eq(odd_sum, 25)
}

// 测试错误处理和异常情况
test "error_handling" {
  // 测试除零保护的逻辑
  let safe_divide = fn(a : Int, b : Int) -> Int {
    if (b == 0) { 0 } else { a / b }
  }
  
  let result1 = safe_divide(10, 2)
  let result2 = safe_divide(10, 0)
  let result3 = safe_divide(0, 5)
  
  @azimuth.assert_eq(result1, 5)
  @azimuth.assert_eq(result2, 0)
  @azimuth.assert_eq(result3, 0)
}

// 测试字符串处理
test "string_processing" {
  // 测试整数和字符串之间的处理
  let num = 42
  let str = "42"
  
  // 使用字符串拼接
  let greeting1 = @azimuth.greet("User")
  let greeting2 = @azimuth.greet("Admin")
  
  @azimuth.assert_eq_string("Hello, User!", greeting1)
  @azimuth.assert_eq_string("Hello, Admin!", greeting2)
}

// 测试递归模式
test "recursive_patterns" {
  // 计算阶乘（使用展开方式）
  let factorial_0 = 1
  let factorial_1 = 1
  let factorial_2 = @azimuth.multiply(2, factorial_1)
  let factorial_3 = @azimuth.multiply(3, factorial_2)
  let factorial_4 = @azimuth.multiply(4, factorial_3)
  let factorial_5 = @azimuth.multiply(5, factorial_4)
  
  @azimuth.assert_eq(factorial_0, 1)
  @azimuth.assert_eq(factorial_1, 1)
  @azimuth.assert_eq(factorial_5, 120)
  
  // 计算斐波那契数列（使用展开方式）
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = @azimuth.add(fib_1, fib_0)
  let fib_3 = @azimuth.add(fib_2, fib_1)
  let fib_4 = @azimuth.add(fib_3, fib_2)
  let fib_5 = @azimuth.add(fib_4, fib_3)
  
  @azimuth.assert_eq(fib_0, 0)
  @azimuth.assert_eq(fib_1, 1)
  @azimuth.assert_eq(fib_5, 5)
}