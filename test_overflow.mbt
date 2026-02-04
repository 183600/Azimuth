// 简单测试文件
pub fn test_int_overflow() -> Int {
  2147483647  // 使用32位整数的最大值
}

test "overflow_test" {
  assert_eq(2147483647, test_int_overflow())
}