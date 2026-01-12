test "string_basic_test" {
  // 测试基本的字符串处理
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
  @clean_test.assert_eq_string("Hello, Test!", @clean_test.greet("Test"))
}

///|
test "math_basic_operations" {
  // 测试基本数学运算
  @clean_test.assert_eq(10, @clean_test.add(3, 7))
  @clean_test.assert_eq(15, @clean_test.multiply(3, 5))
}

///|
test "zero_operations" {
  // 测试零值运算
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(5, @clean_test.add(0, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
}

///|
test "negative_operations" {
  // 测试负数运算
  @clean_test.assert_eq(-2, @clean_test.add(-1, -1))
  @clean_test.assert_eq(0, @clean_test.add(-5, 5))
  @clean_test.assert_eq(6, @clean_test.multiply(-2, -3))
  @clean_test.assert_eq(-6, @clean_test.multiply(2, -3))
}

///|
test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 0))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, 0))
  @clean_test.assert_eq(-1, @clean_test.add(max_val, min_val))
}

///|
test "overflow_handling" {
  // 测试溢出处理
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, -1))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(46342, 46342))
}

///|
test "combined_operations" {
  // 测试组合运算
  let a = 10
  let b = 20
  let c = -5
  
  // (a + b) * c
  let sum = @clean_test.add(a, b)
  let result = @clean_test.multiply(sum, c)
  @clean_test.assert_eq(-150, result)
}

///|
test "sequential_operations" {
  // 测试连续运算
  let mut value = 100
  
  // 连续加法
  value = @clean_test.add(value, 50)
  @clean_test.assert_eq(150, value)
  
  // 连续乘法
  value = @clean_test.multiply(value, 2)
  @clean_test.assert_eq(300, value)
}

///|
test "string_special_characters" {
  // 测试字符串特殊字符处理
  @clean_test.assert_eq_string("Hello, @#$%!", @clean_test.greet("@#$%"))
  @clean_test.assert_eq_string("Hello, 中文!", @clean_test.greet("中文"))
  @clean_test.assert_eq_string("Hello, spaces !", @clean_test.greet("spaces "))
  @clean_test.assert_eq_string("Hello, \n\t!", @clean_test.greet("\n\t"))
}

///|
test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let another_large = 2000000
  
  @clean_test.assert_eq(3000000, @clean_test.add(large_num, another_large))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(large_num, another_large))
}

///|
test "negative_multiply_special_cases" {
  // 测试负数乘法的特殊情况
  @clean_test.assert_eq(2147483647, @clean_test.multiply(-2147483647, -1))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(-1, -2147483647))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 2))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(2, -2147483648))
}

///|
test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let a = 100
  let b = -50
  let c = 2
  
  // (a + b) * c
  let sum = @clean_test.add(a, b)
  let result = @clean_test.multiply(sum, c)
  @clean_test.assert_eq(100, result)
  
  // a * b + c
  let product = @clean_test.multiply(a, b)
  let mixed_result = @clean_test.add(product, c)
  @clean_test.assert_eq(-4998, mixed_result)
}

///|
test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算组合
  @clean_test.assert_eq(-1, @clean_test.add(max_val, min_val))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(max_val, min_val))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(min_val, max_val))
  
  // 边界值与普通值的组合
  @clean_test.assert_eq(2147483647, @clean_test.add(max_val, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(min_val, 0))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(max_val, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(min_val, 1))
}

///|
test "string_length_tests" {
  // 测试不同长度的字符串
  @clean_test.assert_eq_string("Hello, a!", @clean_test.greet("a"))
  @clean_test.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @clean_test.greet("abcdefghijklmnopqrstuvwxyz"))
  @clean_test.assert_eq_string("Hello, 01234567890123456789!", @clean_test.greet("01234567890123456789"))
}

///|
test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  @clean_test.assert_eq(@clean_test.add(x, y), @clean_test.add(y, x))
  @clean_test.assert_eq(@clean_test.add(x, 0), x)
  
  // 乘法恒等式
  @clean_test.assert_eq(@clean_test.multiply(x, y), @clean_test.multiply(y, x))
  @clean_test.assert_eq(@clean_test.multiply(x, 1), x)
  @clean_test.assert_eq(@clean_test.multiply(x, 0), 0)
}

///|
test "extreme_value_operations" {
  // 测试极值运算
  let extreme_pos = 2147483647
  let extreme_neg = -2147483648
  let mid_range = 1000000
  
  // 极值与中间值的运算
  @clean_test.assert_eq(2147483647, @clean_test.add(extreme_pos, mid_range))
  @clean_test.assert_eq(-2147483648, @clean_test.add(extreme_neg, -mid_range))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(extreme_pos, mid_range))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(extreme_neg, mid_range))
}
