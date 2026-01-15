// 简单的新 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "simple_addition" {
  // 测试简单的加法
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
}

test "simple_multiplication" {
  // 测试简单的乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(15, multiply(3, 5))
}

test "simple_greeting" {
  // 测试简单的问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "complex_calculation" {
  // 测试复杂计算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}