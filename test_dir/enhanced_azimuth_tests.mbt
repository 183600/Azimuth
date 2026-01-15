// 增强的 Azimuth 测试用例
// 使用标准 MoonBit 测试语法，提供更全面的测试覆盖

// 导入主模块中的函数
use azimuth

test "add_zero_handling" {
  // 测试加法中的零值处理
  assert_eq(42, add(0, 42))
  assert_eq(42, add(42, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-17, add(0, -17))
  assert_eq(-17, add(-17, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(5, add(-3, 8))
  assert_eq(-100, add(-50, -50))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(100, multiply(-10, -10))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-42, multiply(-42, 1))
  assert_eq(-42, multiply(1, -42))
  assert_eq(42, multiply(-42, -1))
  assert_eq(-42, multiply(42, -1))
}

test "overflow_protection_add" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负数溢出
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

test "overflow_protection_multiply" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出
  assert_eq(max_val, multiply(50000, 50000))
  assert_eq(min_val, multiply(-50000, 50000))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数边界情况
  // 测试长字符串
  let long_string = "This is a very long string with many words and spaces"
  assert_eq_string("Hello, This is a very long string with many words and spaces!", greet(long_string))
  
  // 测试包含引号的字符串
  let quoted_string = "Hello \"World\""
  assert_eq_string("Hello, Hello \"World\"!", greet(quoted_string))
  
  // 测试单字符
  assert_eq_string("Hello, x!", greet("x"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算三角形面积：底 * 高 / 2
  let base = 10
  let height = 8
  let area = multiply(base, height) / 2
  assert_eq(40, area)
  
  // 计算立方体体积：边长^3
  let side = 5
  let volume = multiply(multiply(side, side), side)
  assert_eq(125, volume)
  
  // 计算平均值：(a + b + c) / 3
  let a = 12
  let b = 18
  let c = 24
  let average = add(add(a, b), c) / 3
  assert_eq(18, average)
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 7
  let y = 13
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
  
  // 减法作为加负数
  assert_eq(add(x, -y), x - y)
}