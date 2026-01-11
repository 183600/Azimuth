// 字符串处理测试

///|
test "greet_different_languages" {
  // 测试不同语言的问候
  assert_eq("Hello, 张三!", @azimuth.greet("张三"))
  assert_eq("Hello, John!", @azimuth.greet("John"))
  assert_eq("Hello, María!", @azimuth.greet("María"))
  assert_eq("Hello, ヨシダ!", @azimuth.greet("ヨシダ"))
}

///|
test "greet_special_characters" {
  // 测试特殊字符
  assert_eq("Hello, !", @azimuth.greet(""))  // 空字符串
  assert_eq("Hello, A!", @azimuth.greet("A"))  // 单字符
  assert_eq("Hello, 123!", @azimuth.greet("123"))  // 数字
  assert_eq("Hello, @#$!", @azimuth.greet("@#$"))  // 特殊符号
}

///|
test "greet_long_names" {
  // 测试长名称
  let long_name = "VeryLongNameThatExceedsNormalLength"
  let result = @azimuth.greet(long_name)
  assert_eq(true, result.contains("VeryLongNameThatExceedsNormalLength"))
  
  // 测试带空格的名称
  let space_name = "John Doe"
  let space_result = @azimuth.greet(space_name)
  assert_eq(true, space_result.contains("John Doe"))
}