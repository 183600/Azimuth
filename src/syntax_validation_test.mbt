// 简单的语法验证测试

test "simple_syntax_check" {
  // 测试基本语法
  let x = 1
  let y = 2
  assert_eq(x + y, 3)
}

test "string_operations_check" {
  // 测试字符串操作
  let str1 = "hello"
  let str2 = "world"
  let combined = str1 + " " + str2
  assert_eq(combined, "hello world")
}

test "array_operations_check" {
  // 测试数组操作
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
}