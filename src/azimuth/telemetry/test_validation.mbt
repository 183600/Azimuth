// 简单的测试验证文件
// 用于验证测试框架是否正常工作

test "simple_string_operations" {
  // 测试基本字符串操作
  let test_string = "hello telemetry"
  assert_eq(test_string.length(), 15)
  assert_eq(test_string.has_prefix("hello"), true)
  assert_eq(test_string.has_suffix("telemetry"), true)
  assert_eq(test_string.contains("tele"), true)
}

test "basic_math_operations" {
  // 测试基本数学操作
  let a = 10
  let b = 20
  assert_eq(a + b, 30)
  assert_eq(b - a, 10)
  assert_eq(a * b, 200)
  assert_eq(b / a, 2)
  
  let c = 15.5
  let d = 2.5
  assert_eq(c + d, 18.0)
  assert_eq(c - d, 13.0)
  assert_eq(c * d, 38.75)
  assert_eq(c / d, 6.2)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  let mut sum = 0
  let mut i = 0
  while i < numbers.length() {
    sum = sum + numbers[i]
    i = i + 1
  }
  assert_eq(sum, 15)
}

test "boolean_logic" {
  // 测试布尔逻辑
  let result1 = true && true
  assert_eq(result1, true)
  
  let result2 = true && false
  assert_eq(result2, false)
  
  let result3 = false || true
  assert_eq(result3, true)
  
  let result4 = false || false
  assert_eq(result4, false)
  
  let result5 = !true
  assert_eq(result5, false)
  
  let result6 = !false
  assert_eq(result6, true)
}