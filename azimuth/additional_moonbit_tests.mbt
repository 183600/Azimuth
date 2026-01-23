// 额外的 MoonBit 测试用例 - 使用标准 MoonBit 测试语法
// 覆盖更多边界情况和功能组合

test "addition_associative_law" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 7
  let b = 11
  let c = 13
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

test "multiplication_distributive_law" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 7
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "greet_with_long_string" {
  // 测试长字符串的问候功能
  let long_name = "这是一个很长的名字用来测试字符串处理功能"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  let result = divide_with_ceil(
    add(multiply(3, 4), multiply(5, 2)),
    subtract(10, 3)
  )
  assert_eq(4, result)  // ceil((12+10)/7) = ceil(22/7) = 4
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的除法向上取整
  assert_eq(-1, divide_with_ceil(5, -10))
  assert_eq(0, divide_with_ceil(-5, 10))
  assert_eq(1, divide_with_ceil(-5, -10))
}

test "arithmetic_precision" {
  // 测试算术精度
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "edge_case_single_operations" {
  // 测试单个元素的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, add(0, 1))
  assert_eq(0, multiply(0, 1))
}

test "string_concatenation_behavior" {
  // 测试字符串连接行为
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Space !", greet("Space "))
}

// 辅助函数用于测试
fn subtract(a : Int, b : Int) -> Int {
  a - b
}