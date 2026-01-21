// 简单测试脚本来验证 comprehensive_test_suite.mbt 中的测试用例

// 导入 azimuth 包
use azimuth

test "verify_comprehensive_tests" {
  // 验证我们创建的测试用例中的关键功能
  
  // 测试 add 函数
  let result1 = azimuth.add(3, 7)
  @assert_eq(10, result1)
  
  // 测试 multiply 函数
  let result2 = azimuth.multiply(3, 5)
  @assert_eq(15, result2)
  
  // 测试 greet 函数
  let result3 = azimuth.greet("World")
  @assert_eq_string("Hello, World!", result3)
  
  // 测试边界值
  let max_val = 2147483647
  let overflow_result = azimuth.add(max_val, 1)
  @assert_eq(max_val, overflow_result)
}