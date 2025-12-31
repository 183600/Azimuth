// 简单语法验证文件

test "syntax_validation" {
  let test_value = 42
  assert_eq(test_value, 42)
  
  let test_string = "hello"
  assert_eq(test_string.length(), 5)
  
  let test_array = [1, 2, 3]
  assert_eq(test_array.length(), 3)
}