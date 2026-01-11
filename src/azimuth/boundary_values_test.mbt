// 边界值和特殊情况测试

///|
test "extreme_positive_values" {
  // 测试极大的正数
  let max_val = 2147483647
  assert_eq(max_val, @azimuth.add(max_val, 0))  // 最大值 + 0
  assert_eq(max_val, @azimuth.add(max_val, 1))  // 最大值 + 1（溢出）
  assert_eq(max_val, @azimuth.multiply(max_val, 1))  // 最大值 × 1
  assert_eq(-max_val, @azimuth.multiply(max_val, -1))  // 最大值 × -1
}

///|
test "extreme_negative_values" {
  // 测试极小的负数
  let min_val = -2147483648
  assert_eq(min_val, @azimuth.add(min_val, 0))  // 最小值 + 0
  assert_eq(min_val, @azimuth.add(min_val, -1))  // 最小值 - 1（溢出）
  assert_eq(min_val, @azimuth.multiply(min_val, 1))  // 最小值 × 1
  assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 最小值 × -1（溢出）
}

///|
test "boundary_combinations" {
  // 测试边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的运算
  assert_eq(-1, @azimuth.add(max_val, min_val))  // 最大值 + 最小值 = -1
  assert_eq(-1, @azimuth.add(min_val, max_val))  // 最小值 + 最大值 = -1
  
  // 接近边界的情况
  assert_eq(2147483646, @azimuth.add(max_val, -1))  // 最大值 - 1
  assert_eq(-2147483647, @azimuth.add(min_val, 1))  // 最小值 + 1
}