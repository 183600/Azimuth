// 简单的验证测试，确保 comprehensive_focused_tests.mbt 被正确加载

test "simple_verification_test" {
  // 这个测试用于验证测试文件是否被正确加载
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}