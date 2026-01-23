// 简单的高质量 MoonBit 测试用例
// 为 azimuth 库添加的测试覆盖

test "simple_add_test" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiply_test" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greet_test" {
  // 测试问候函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "simple_divide_test" {
  // 测试向上取整除法函数的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "simple_math_test" {
  // 测试基本的数学运算
  let a = add(5, 3)        // 8
  let b = multiply(a, 2)   // 16
  let c = divide_with_ceil(b, 5)  // 4
  assert_eq(4, c)
}