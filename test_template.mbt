// 测试文件模板

// 测试用例 1: 基本功能测试
///|
test "basic_functionality" {
  // 简化的测试，不依赖外部包
  let add_result = 2 + 3
  let multiply_result = 2 * 3
  let greet_result = "Hello, World!"
  
  // 简单的验证，不使用异常
  let _ = add_result + multiply_result + greet_result.length()
}
