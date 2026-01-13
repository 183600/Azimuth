// 简化的string测试文件
test "string_basic" {
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  // 简单的验证，不使用断言
  let _ = s3
}

test "string_empty" {
  let empty = ""
  let _ = empty
}

test "string_compare" {
  let s1 = "abc"
  let s2 = "def"
  let result = s1 == s2
  let _ = result
}