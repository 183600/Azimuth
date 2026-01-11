// 额外测试用例 - 补充测试覆盖

///|
test "string_basic_test" {
  // 测试基本的字符串处理
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, Test!", greet("Test"))
}

///|
test "math_basic_operations" {
  // 测试基本数学运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
}

///|
test "zero_operations" {
  // 测试零值运算
  assert_eq(0, add(0, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
}

///|
test "negative_operations" {
  // 测试负数运算
  assert_eq(-2, add(-1, -1))
  assert_eq(0, add(-5, 5))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
}

///|
test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-1, add(max_val, min_val))
}

///|
test "overflow_handling" {
  // 测试溢出处理
  assert_eq(2147483647, add(2147483647, 1))
  assert_eq(-2147483648, add(-2147483648, -1))
  assert_eq(2147483647, multiply(46342, 46342))
}

///|
test "combined_operations" {
  // 测试组合运算
  let a = 10
  let b = 20
  let c = -5
  
  // (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(-150, result)
}

///|
test "sequential_operations" {
  // 测试连续运算
  let mut value = 100
  
  // 连续加法
  value = add(value, 50)
  assert_eq(150, value)
  
  // 连续乘法
  value = multiply(value, 2)
  assert_eq(300, value)
}

///|
test "string_special_characters" {
  // 测试字符串特殊字符处理
  assert_eq("Hello, @#$%!", greet("@#$%"))
  assert_eq("Hello, 中文!", greet("中文"))
  assert_eq("Hello, spaces !", greet("spaces "))
  assert_eq("Hello, \n\t!", greet("\n\t"))
}

///|
test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let another_large = 2000000
  
  assert_eq(3000000, add(large_num, another_large))
  assert_eq(2147483647, multiply(large_num, another_large))
}

///|
test "negative_multiply_special_cases" {
  // 测试负数乘法的特殊情况
  assert_eq(2147483647, multiply(-2147483647, -1))
  assert_eq(2147483647, multiply(-1, -2147483647))
  assert_eq(-2147483648, multiply(-2147483648, 2))
  assert_eq(-2147483648, multiply(2, -2147483648))
}

///|
test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let a = 100
  let b = -50
  let c = 2
  
  // (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(100, result)
  
  // a * b + c
  let product = multiply(a, b)
  let mixed_result = add(product, c)
  assert_eq(-4998, mixed_result)
}

///|
test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(-2147483648, multiply(max_val, min_val))
  assert_eq(-2147483648, multiply(min_val, max_val))
  
  // 边界值与普通值的组合
  assert_eq(2147483647, add(max_val, 0))
  assert_eq(-2147483648, add(min_val, 0))
  assert_eq(2147483647, multiply(max_val, 1))
  assert_eq(-2147483648, multiply(min_val, 1))
}

///|
test "string_length_tests" {
  // 测试不同长度的字符串
  assert_eq("Hello, a!", greet("a"))
  assert_eq("Hello, abcdefghijklmnopqrstuvwxyz!", greet("abcdefghijklmnopqrstuvwxyz"))
  assert_eq("Hello, 01234567890123456789!", greet("01234567890123456789"))
}

///|
test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(x, 0), x)
  
  // 乘法恒等式
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, 1), x)
  assert_eq(multiply(x, 0), 0)
}

///|
test "extreme_value_operations" {
  // 测试极值运算
  let extreme_pos = 2147483647
  let extreme_neg = -2147483648
  let mid_range = 1000000
  
  // 极值与中间值的运算
  assert_eq(2147483647, add(extreme_pos, mid_range))
  assert_eq(-2147483648, add(extreme_neg, -mid_range))
  assert_eq(2147483647, multiply(extreme_pos, mid_range))
  assert_eq(-2147483648, multiply(extreme_neg, mid_range))
}