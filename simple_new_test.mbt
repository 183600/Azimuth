// 简单的新测试验证
// 验证新创建的测试用例能够正常工作

test "basic_functionality_test" {
  let test_value = 42
  assert_eq(test_value, 42)
  assert_eq(test_value > 0, true)
}

test "string_operations_test" {
  let test_string = "azimuth-telemetry"
  assert_eq(test_string.length(), 16)
  assert_eq(test_string.contains("azimuth"), true)
  assert_eq(test_string.contains("telemetry"), true)
}

test "array_operations_test" {
  let test_array = [1, 2, 3, 4, 5]
  assert_eq(test_array.length(), 5)
  assert_eq(test_array[0], 1)
  assert_eq(test_array[4], 5)
  
  let filtered = test_array.filter(fn(x) { x > 2 })
  assert_eq(filtered.length(), 3)
}