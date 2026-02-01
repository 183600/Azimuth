// 简单的标准 MoonBit 测试用例
// 为 Azimuth 项目添加基本测试

test "simple_addition" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "simple_divide_ceil" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "simple_subtract" {
  assert_eq(5, subtract(10, 5))
}