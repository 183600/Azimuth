// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的简洁测试

test "add_identity_elements" {
  // 测试加法单位元
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, add(0, 0))
}

test "multiply_identity_elements" {
  // 测试乘法单位元
  assert_eq(7, multiply(7, 1))
  assert_eq(-10, multiply(-10, 1))
  assert_eq(0, multiply(0, 1))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法精度
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "greet_function_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, Español!", greet("Español"))
}

test "mathematical_commutativity" {
  // 测试交换律
  let a = 6
  let b = 9
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_zero_handling" {
  // 测试负零处理
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "boundary_value_operations" {
  // 测试边界值运算
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  let x = 10
  let y = 15
  let result = add(multiply(x, 2), divide_with_ceil(y, 3))
  assert_eq(35, result)  // 10*2 + ceil(15/3) = 20 + 5 = 25
}

test "practical_tip_calculation" {
  // 实际小费计算
  let bill = 85
  let tip_percent = 15
  let tip = divide_with_ceil(multiply(bill, tip_percent), 100)
  let total = add(bill, tip)
  assert_eq(13, tip)
  assert_eq(98, total)
}

test "string_greeting_special_chars" {
  // 测试特殊字符问候
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("   "))
}