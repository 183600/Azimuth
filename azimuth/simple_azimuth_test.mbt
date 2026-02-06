// 简单的 Azimuth 测试 - 8个标准测试用例
// 测试 azimuth 库的核心功能

test "add_simple" {
  assert_eq(5, add(2, 3))
}

test "multiply_simple" {
  assert_eq(6, multiply(2, 3))
}

test "greet_simple" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_simple" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "subtract_simple" {
  assert_eq(2, subtract(5, 3))
}

test "add_comprehensive" {
  // 加法函数综合测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
}

test "divide_with_ceil_negative" {
  // 向上取整除法负数处理测试
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}