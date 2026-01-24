// 标准 MoonBit 测试用例 - 为 azimuth 库添加测试覆盖

test "add_function_basic" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
}

test "multiply_function_basic" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_function_basic" {
  // 测试问候函数的基本功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_identity_properties" {
  // 测试加法的恒等式性质
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
}

test "multiply_zero_properties" {
  // 测试乘法的零元素性质
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数处理
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation" {
  // 测试复杂计算组合
  let result = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, result)  // 12 + 3 = 15
}

test "greet_unicode_characters" {
  // 测试问候函数的Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(multiply(5, 7), multiply(7, 5))
}