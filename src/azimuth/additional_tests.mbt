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