// 新的 MoonBit 测试用例
// 使用 @test 注释格式

@test
fn test_add_basic() {
  assert_eq(5, add(2, 3))
}

@test
fn test_add_zero() {
  assert_eq(0, add(0, 0))
}

@test
fn test_add_negative() {
  assert_eq(-5, add(-2, -3))
}

@test
fn test_multiply_basic() {
  assert_eq(6, multiply(2, 3))
}

@test
fn test_multiply_zero() {
  assert_eq(0, multiply(0, 5))
}

@test
fn test_greet_basic() {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

@test
fn test_divide_with_ceil() {
  assert_eq(4, divide_with_ceil(11, 3))
}

@test
fn test_assert_true() {
  assert_true(true)
}

@test
fn test_assert_false() {
  assert_false(false)
}