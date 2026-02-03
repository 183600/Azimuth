// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的补充测试

test "subtract_function_boundary_test" {
  // 测试减法函数的边界情况
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(1000, subtract(1000, 0))
}

test "divide_with_ceil_large_numbers" {
  // 测试大数的向上取整除法
  assert_eq(100000, divide_with_ceil(1000000, 10))
  assert_eq(33334, divide_with_ceil(100000, 3))
  assert_eq(1, divide_with_ceil(99999, 100000))
  assert_eq(0, divide_with_ceil(1000000, 0))
}

test "combined_operations_complex" {
  // 测试复杂的组合运算
  let x = 12
  let y = 8
  let z = 5
  
  // 测试 (x - y) * z + x
  let result1 = add(multiply(subtract(x, y), z), x)
  assert_eq(52, result1)  // (12 - 8) * 5 + 12 = 4 * 5 + 12 = 32
  
  // 测试 x * y - z / x (向上取整)
  let result2 = subtract(multiply(x, y), divide_with_ceil(z, x))
  assert_eq(95, result2)  // 12 * 8 - ceil(5/12) = 96 - 1 = 95
}

test "greet_function_unicode_extended" {
  // 测试扩展Unicode字符的问候功能
  assert_eq_string("Hello, 👨‍💻!", greet("👨‍💻"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, ß!", greet("ß"))
  assert_eq_string("Hello, Ω!", greet("Ω"))
}

test "fibonacci_sequence_calculation" {
  // 使用库函数计算斐波那契数列
  let fib0 = 0
  let fib1 = 1
  let fib2 = add(fib0, fib1)
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(1, fib2)
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let apple_price = 5
  let apple_qty = 3
  let banana_price = 3
  let banana_qty = 4
  let orange_price = 7
  let orange_qty = 2
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let orange_total = multiply(orange_price, orange_qty)
  
  let grand_total = add(add(apple_total, banana_total), orange_total)
  
  assert_eq(15, apple_total)
  assert_eq(12, banana_total)
  assert_eq(14, orange_total)
  assert_eq(41, grand_total)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-5, divide_with_ceil(-9, 2))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "string_greeting_concatenation" {
  // 测试字符串连接与问候函数的组合
  let title = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  
  let full_name = title + " " + first_name + " " + last_name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Jane Smith!", formal_greeting)
}

test "arithmetic_progression_sum" {
  // 使用库函数计算等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 手动计算等差数列各项
  let term1 = first_term
  let term2 = add(term1, common_diff)
  let term3 = add(term2, common_diff)
  let term4 = add(term3, common_diff)
  let term5 = add(term4, common_diff)
  let term6 = add(term5, common_diff)
  
  let sum = add(add(add(add(add(term1, term2), term3), term4), term5), term6)
  
  assert_eq(3, term1)
  assert_eq(7, term2)
  assert_eq(11, term3)
  assert_eq(15, term4)
  assert_eq(19, term5)
  assert_eq(23, term6)
  assert_eq(78, sum)
}

test "celsius_to_fahrenheit_conversion" {
  // 温度转换计算（简化版）
  let celsius = 25
  
  // 标准转换公式：F = C * 9/5 + 32
  // 使用向上取整除法：F = C * 9/5 + 32 = ceil(25 * 9 / 5) + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)  // ceil(225/5) + 32 = 45 + 32 = 77
}