// 测试文件模板

// 测试用例 1: 基本功能测试
///|
test "basic_functionality" {
  // 测试加法
  if 5 != @azimuth.add(2, 3) { raise @moonbitlang/core/builtin.Failure("Test failed") }
  
  // 测试乘法
  if 6 != @azimuth.multiply(2, 3) { raise @moonbitlang/core/builtin.Failure("Test failed") }
  
  // 测试字符串拼接
  if "Hello, World!" != @azimuth.greet("World") { raise @moonbitlang/core/builtin.Failure("Test failed") }
}
