// 简单测试验证新添加的测试用例语法是否正确

test "new_test_syntax_validation" {
  // 验证新添加测试的基本语法
  
  let test_string = "syntax_validation"
  assert_eq(test_string.length(), 17)
  assert_eq(test_string.contains("validation"), true)
}