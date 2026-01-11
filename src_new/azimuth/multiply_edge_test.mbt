// 测试乘法函数的边界情况

test "multiply_edge_cases" {
  // 测试最小值乘以各种值
  assert_eq(-2147483648, multiply(-2147483648, 1))// 最小值 * 1
  assert_eq(-2147483648, multiply(-2147483648, -1))// 最小值 * -1 (溢出)
  assert_eq(0, multiply(-2147483648, 0))// 最小值 * 0
  assert_eq(-2147483648, multiply(-2147483648, 2))// 最小值 * 2 (溢出)
  assert_eq(-2147483648, multiply(-2147483648, -2))// 最小值 * -2 (溢出)
  
  // 反过来测试
  assert_eq(-2147483648, multiply(1, -2147483648))// 1 * 最小值
  assert_eq(-2147483648, multiply(-1, -2147483648))// -1 * 最小值 (溢出)
  assert_eq(0, multiply(0, -2147483648))// 0 * 最小值
  assert_eq(-2147483648, multiply(2, -2147483648))// 2 * 最小值 (溢出)
  assert_eq(-2147483648, multiply(-2, -2147483648))// -2 * 最小值 (溢出)
  
  // 测试大数相乘的溢出
  assert_eq(2147483647, multiply(46341, 46341))// 接近溢出边界
  assert_eq(2147483647, multiply(46342, 46342))// 溢出
  assert_eq(2147483647, multiply(65536, 65536))// 大数溢出
  
  // 测试负溢出
  assert_eq(-2147483648, multiply(-46341, 46341))// 负溢出
  assert_eq(-2147483648, multiply(46341, -46341))// 负溢出
}