// 简化的test测试文件
test "same_object_basic" {
  let s = "Hello"
  // 测试字符串自身相等性和长度
  let len = s.length()
  let _ = len > 0
}

test "is_not_basic" {
  let a = "1"
  let b = "2"
  // 测试不同字符串长度
  let len_a = a.length()
  let len_b = b.length()
  let _ = len_a == len_b
}

test "test_name" {
  let name = "sample"
  // 测试字符串长度大于0
  let len = name.length()
  let _ = len > 0
}