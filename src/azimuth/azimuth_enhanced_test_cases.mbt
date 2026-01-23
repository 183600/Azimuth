// 增强的 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含实际应用场景和边界情况

test "mathematical_properties_verification" {
  // 验证基本的数学性质
  let a = 15
  let b = 25
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "boundary_conditions_test" {
  // 边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(5, 0))
}