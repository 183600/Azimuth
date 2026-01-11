// 性能测试文件

///|
test "performance_test_add" {
  // 测试大量加法操作的性能
  let mut sum = 0
  for i = 0; i < 10000; i = i + 1 {
    sum = @azimuth.add(sum, i)
  }
  // 0+1+2+...+9999 = 49995000，不会溢出
  assert_eq(49995000, sum)
}

///|
test "performance_test_multiply" {
  // 测试乘法操作的性能
  let mut result = 1
  for i = 1; i < 10; i = i + 1 {
    result = @azimuth.multiply(result, i)
  }
  // 1*2*3*4*5*6*7*8*9 = 362880
  assert_eq(362880, result)
}

///|
test "edge_case_performance" {
  // 测试边界情况的性能
  let mut result = 2147483647
  // 只需要循环2次就能达到稳定状态，避免不必要的CPU消耗
  for i = 0; i < 2; i = i + 1 {
    result = @azimuth.add(result, -2147483648)
  }
  // 第一次：2147483647 + -2147483648 = -1
  // 第二次：-1 + -2147483648 = -2147483649，溢出，返回 -2147483648
  assert_eq(-2147483648, result)
}