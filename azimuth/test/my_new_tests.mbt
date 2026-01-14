// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的核心功能

// 导入主模块中的函数
use azimuth

test "add_zero_handling" {
  // 测试加法中的零值处理
  assert_eq(10, add(0, 10))
  assert_eq(10, add(10, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(0, -5))
  assert_eq(-5, add(-5, 0))
}

test "multiply_zero_and_one" {
  // 测试乘法中的零值和一值处理
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(-5, multiply(5, -1))
}

test "greet_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, \n!", greet("\n"))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \\!", greet("\\"))
  assert_eq_string("Hello, \"!", greet("\""))
}

test "add_overflow_protection" {
  // 测试加法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(max_val, multiply(max_val, 2))
  
  // 测试负溢出
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, 2))
}

test "min_value_special_cases" {
  // 测试最小值的特殊情况
  let min_val = -2147483648
  
  // 加法测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, add(min_val, -1))
  
  // 乘法测试
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_empty_and_whitespace" {
  // 测试空字符串和空白字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
  assert_eq_string("Hello, \n!", greet("\n"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 100
  let b = 50
  let c = 25
  
  // 测试组合运算：(a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(3750, result1)
  
  // 测试组合运算：a * (b + c)
  let result2 = multiply(a, add(b, c))
  assert_eq(7500, result2)
  
  // 测试组合运算：(a * b) + (a * c)
  let result3 = add(multiply(a, b), multiply(a, c))
  assert_eq(7500, result3)
}