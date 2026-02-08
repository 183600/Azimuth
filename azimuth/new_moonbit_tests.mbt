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

@test
fn test_azimuth_moonbit_enhanced() {
  // Azimuth MoonBit 增强测试
  // 测试数学运算的多种组合
  let a = 12
  let b = 8
  let c = 5
  
  // 复杂表达式测试
  let expr1 = add(multiply(a, b), divide_with_ceil(c, 2))  // 12*8 + ceil(5/2) = 96 + 3 = 99
  let expr2 = divide_with_ceil(subtract(multiply(a, c), b), 10)  // ceil((12*5-8)/10) = ceil(52/10) = 6
  let expr3 = multiply(add(a, b), subtract(c, 2))  // (12+8)*(5-2) = 20*3 = 60
  
  assert_eq(99, expr1)
  assert_eq(6, expr2)
  assert_eq(60, expr3)
}