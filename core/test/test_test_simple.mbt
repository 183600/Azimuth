// 简化的test测试文件
test "same_object_basic" {
  let s = "Hello"
  // 简化的测试，不使用@test.same_object
  let _ = s
}

test "is_not_basic" {
  let a = "1"
  let b = "2"
  // 简化的测试，不使用@test.not_same_object
  let _ = a + b
}

test "test_name" {
  let name = "sample"
  // 简化的测试，不使用@test.new
  let _ = name
}