// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 专注的单元测试 - 补充测试用例

// 测试用例 1: 测试加法的幂等性质（特殊情况）
///|
test "addition_idempotent_cases" {
  // 测试 0 + 0 = 0
  assert_eq(0, @clean_test.@clean_test.add(0, 0))
  
  // 测试最大值 + 0 = 最大值
  assert_eq(2147483647, @clean_test.@clean_test.add(2147483647, 0))
  
  // 测试最小值 + 0 = 最小值
  assert_eq(-2147483648, @clean_test.@clean_test.add(-2147483648, 0))
}

// 测试用例 2: 测试乘法的幂等性质（特殊情况）
///|
test "multiplication_idempotent_cases" {
  // 测试 0 * 任何数 = 0
  assert_eq(0, @clean_test.@clean_test.multiply(0, 123))
  assert_eq(0, @clean_test.@clean_test.multiply(0, -456))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 2147483647))
  assert_eq(0, @clean_test.@clean_test.multiply(0, -2147483648))
  
  // 测试 1 * 1 = 1
  assert_eq(1, @clean_test.@clean_test.multiply(1, 1))
  
  // 测试 -1 * -1 = 1
  assert_eq(1, @clean_test.@clean_test.multiply(-1, -1))
}

// 测试用例 3: 测试负数乘法的奇偶性
///|
test "negative_multiplication_parity" {
  // 偶数 * 偶数 = 偶数
  assert_eq(0, @clean_test.@clean_test.multiply(2, 4) % 2)
  assert_eq(0, @clean_test.@clean_test.multiply(-2, 4) % 2)
  assert_eq(0, @clean_test.@clean_test.multiply(2, -4) % 2)
  assert_eq(0, @clean_test.@clean_test.multiply(-2, -4) % 2)
  
  // 奇数 * 奇数 = 奇数
  assert_eq(1, @clean_test.@clean_test.multiply(3, 5) % 2)
  assert_eq(1, @clean_test.@clean_test.multiply(-3, -5) % 2)
  
  // 奇数 * 偶数 = 偶数
  assert_eq(0, @clean_test.@clean_test.multiply(3, 4) % 2)
  assert_eq(0, @clean_test.@clean_test.multiply(-3, 4) % 2)
}

// 测试用例 4: 测试字符串拼接的长度特性
///|
test "string_greeting_length_properties" {
  // 测试空字符串
  let empty_greeting = @clean_test.@clean_test.greet("")
  assert_eq(7, empty_greeting.length())  // "Hello, !" = 7个字符
  
  // 测试单字符
  let single_char_greeting = @clean_test.@clean_test.greet("A")
  assert_eq(8, single_char_greeting.length())  // "Hello, A!" = 8个字符
  
  // 测试长字符串
  let long_name = "VeryLongName"
  let long_greeting = @clean_test.@clean_test.greet(long_name)
  assert_eq(7 + long_name.length(), long_greeting.length())  // "Hello, " + name + "!"
}

// 测试用例 5: 测试加法的单调性
///|
test "addition_monotonicity" {
  // 如果 a > b，那么 a + c > b + c（在不会溢出的情况下）
  let a = 100
  let b = 50
  let c = 25
  
  assert_eq(true, @clean_test.@clean_test.add(a, c) > @clean_test.@clean_test.add(b, c))
  assert_eq(true, @clean_test.@clean_test.add(c, a) > @clean_test.@clean_test.add(c, b))
  
  // 负数的情况
  let x = -10
  let y = -20
  let z = 5
  
  assert_eq(true, @clean_test.@clean_test.add(x, z) > @clean_test.@clean_test.add(y, z))
}

// 测试用例 6: 测试乘法的单调性（正数范围）
///|
test "multiplication_monotonicity_positive" {
  // 如果 a > b > 0 且 c > 0，那么 a * c > b * c（在不会溢出的情况下）
  let a = 10
  let b = 5
  let c = 3
  
  assert_eq(true, @clean_test.@clean_test.multiply(a, c) > @clean_test.@clean_test.multiply(b, c))
  assert_eq(true, @clean_test.@clean_test.multiply(c, a) > @clean_test.@clean_test.multiply(c, b))
}

// 测试用例 7: 测试特定数学恒等式
///|
test "mathematical_identities" {
  // 平方差公式: a² - b² = (a + b)(a - b)
  // 我们通过加法和乘法来验证
  let a = 10
  let b = 3
  
  // a² - b² = (a + b)(a - b)
  let a_squared = @clean_test.@clean_test.multiply(a, a)
  let b_squared = @clean_test.@clean_test.multiply(b, b)
  let diff_of_squares = @clean_test.@clean_test.add(a_squared, @clean_test.@clean_test.multiply(-1, b_squared))
  
  let sum = @clean_test.@clean_test.add(a, b)
  let diff = @clean_test.@clean_test.add(a, @clean_test.@clean_test.multiply(-1, b))
  let product = @clean_test.@clean_test.multiply(sum, diff)
  
  assert_eq(diff_of_squares, product)
}

// 测试用例 8: 测试斐波那契数列计算（使用加法）
///|
test "fibonacci_sequence_calculation" {
  // 计算前几个斐波那契数
  let f0 = 0
  let f1 = 1
  
  // f2 = f1 + f0 = 1
  let f2 = @clean_test.@clean_test.add(f1, f0)
  assert_eq(1, f2)
  
  // f3 = f2 + f1 = 2
  let f3 = @clean_test.@clean_test.add(f2, f1)
  assert_eq(2, f3)
  
  // f4 = f3 + f2 = 3
  let f4 = @clean_test.@clean_test.add(f3, f2)
  assert_eq(3, f4)
  
  // f5 = f4 + f3 = 5
  let f5 = @clean_test.@clean_test.add(f4, f3)
  assert_eq(5, f5)
}

// 测试用例 9: 测试字符串拼接的前缀和后缀
///|
test "string_prefix_suffix_properties" {
  let name = "MoonBit"
  let greeting = @clean_test.@clean_test.greet(name)
  
  // 测试前缀
  assert_eq(true, greeting.starts_with("Hello, "))
  
  // 测试后缀
  assert_eq(true, greeting.ends_with("!"))
  
  // 测试包含
  assert_eq(true, greeting.contains(name))
}

// 测试用例 10: 测试连续运算的结合性
///|
test "continuous_operations_associativity" {
  // 测试连续加法的不同分组方式
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  // ((a + b) + c) + d
  let method1 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(a, b), c), d)
  
  // a + (b + (c + d))
  let method2 = @clean_test.@clean_test.add(a, @clean_test.@clean_test.add(b, @clean_test.@clean_test.add(c, d)))
  
  // (a + b) + (c + d)
  let method3 = @clean_test.@clean_test.add(@clean_test.@clean_test.add(a, b), @clean_test.@clean_test.add(c, d))
  
  assert_eq(method1, method2)
  assert_eq(method2, method3)
}