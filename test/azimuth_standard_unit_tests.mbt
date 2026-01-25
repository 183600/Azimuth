// Azimuth 库标准单元测试
// 包含10个核心功能测试用例

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_commutative_property" {
  // 测试加法交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
}

test "multiply_commutative_property" {
  // 测试乘法交换律
  assert_eq(multiply(7, 13), multiply(13, 7))
  assert_eq(multiply(-5, 10), multiply(10, -5))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_unicode_support" {
  // 测试Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_operations" {
  // 测试复杂运算
  assert_eq(26, add(multiply(2, 3), multiply(4, 5)))
  assert_eq(8, divide_with_ceil(add(multiply(3, 4), multiply(2, 5)), 3))
}

test "boundary_values" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}