// 简单的 MoonBit 测试用例示例

test "simple_addition_test" {
  // 测试简单的加法运算
  @azimuth.assert_eq(@azimuth.add(5, 3), 8)
  @azimuth.assert_eq(@azimuth.add(-2, 7), 5)
  @azimuth.assert_eq(@azimuth.add(0, 10), 10)
}

test "simple_multiplication_test" {
  // 测试简单的乘法运算
  @azimuth.assert_eq(@azimuth.multiply(4, 5), 20)
  @azimuth.assert_eq(@azimuth.multiply(-3, 6), -18)
  @azimuth.assert_eq(@azimuth.multiply(7, 0), 0)
}

test "simple_greet_test" {
  // 测试简单的问候函数
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
}

test "combined_operations_test" {
  // 测试组合运算
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  
  @azimuth.assert_eq(result, 26)  // (10 + 3) * 2 = 26
}

test "edge_cases_test" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2), max_val)
  
  // 测试零值
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 12345), 0)
}