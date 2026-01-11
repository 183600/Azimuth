// 调试测试用例
///|
test "debug_multiply_min_max" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试 min_val * max_val 的结果
  let result = @azimuth.multiply(min_val, max_val)
  
  // 先检查实际结果
  // assert_eq(max_val, result)  // 实际返回 max_val
  assert_eq(min_val, result)  // 期望返回 min_val
}