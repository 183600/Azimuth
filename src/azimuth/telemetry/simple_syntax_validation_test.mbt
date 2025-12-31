// 简单测试验证 - 验证新创建的测试文件的基本语法
test "basic_syntax_validation" {
  // 测试基本的语法结构
  
  let service_name = "test-service"
  let service_version = "1.0.0"
  
  // 验证字符串操作
  assert_eq(service_name.length(), 12)
  assert_eq(service_name.has_prefix("test"), true)
  assert_eq(service_name.has_suffix("service"), true)
  
  // 验证版本号
  assert_eq(service_version.length(), 5)
  assert_eq(service_version.has_prefix("1."), true)
  assert_eq(service_version.has_suffix(".0"), true)
  
  // 验证数组操作
  let test_array = [1L, 2L, 3L, 4L, 5L]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1L)
  assert_eq(test_array[4], 5L)
  
  // 验证基本算术
  let sum = 0L
  let mut i = 0
  while i < test_array.length() {
    sum = sum + test_array[i]
    i = i + 1
  }
  assert_eq(sum, 15L)
  
  // 验证布尔操作
  let is_valid = service_name.length() > 0 && service_version.length() > 0
  assert_eq(is_valid, true)
  
  // 验证条件表达式
  let status = if sum > 10L { "good" } else { "bad" }
  assert_eq(status, "good")
}