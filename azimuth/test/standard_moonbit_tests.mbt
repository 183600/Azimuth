test "add function basic operations" {
  // 测试基本的加法运算
  @assertion.assert_eq(5, azimuth.add(2, 3))
  @assertion.assert_eq(0, azimuth.add(0, 0))
  @assertion.assert_eq(-3, azimuth.add(-1, -2))
  @assertion.assert_eq(10, azimuth.add(5, 5))
}

test "add function with zero" {
  // 测试与零相加的情况
  @assertion.assert_eq(7, azimuth.add(0, 7))
  @assertion.assert_eq(7, azimuth.add(7, 0))
  @assertion.assert_eq(-5, azimuth.add(0, -5))
  @assertion.assert_eq(-5, azimuth.add(-5, 0))
}

test "add function overflow handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))
  @assertion.assert_eq(max_val, azimuth.add(max_val, max_val))
  
  // 负数溢出
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))
  @assertion.assert_eq(min_val, azimuth.add(-1000000000, -1500000000))
}

test "multiply function basic operations" {
  // 测试基本的乘法运算
  @assertion.assert_eq(6, azimuth.multiply(2, 3))
  @assertion.assert_eq(0, azimuth.multiply(0, 5))
  @assertion.assert_eq(0, azimuth.multiply(5, 0))
  @assertion.assert_eq(-6, azimuth.multiply(-2, 3))
  @assertion.assert_eq(6, azimuth.multiply(-2, -3))
}

test "multiply function with one and negative one" {
  // 测试与1和-1相乘的情况
  @assertion.assert_eq(7, azimuth.multiply(1, 7))
  @assertion.assert_eq(7, azimuth.multiply(7, 1))
  @assertion.assert_eq(-7, azimuth.multiply(-1, 7))
  @assertion.assert_eq(-7, azimuth.multiply(7, -1))
  @assertion.assert_eq(7, azimuth.multiply(-1, -7))
}

test "multiply function overflow handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))
  
  // 最小值特殊情况
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 2))
}

test "greet function" {
  // 测试字符串拼接
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "assert helper functions" {
  // 测试断言辅助函数
  @assertion.assert_true(true)
  @assertion.assert_false(false)
  
  // 测试字符串相等断言
  @assertion.assert_eq_string("test", "test")
  @assertion.assert_eq_string("中文测试", "中文测试")
  
  // 测试整数相等断言
  @assertion.assert_eq(42, 42)
  @assertion.assert_eq(-100, -100)
}

test "complex arithmetic operations" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = -5
  
  // 先加后乘
  @assertion.assert_eq(300, azimuth.multiply(azimuth.add(a, b), 10))
  
  // 包含负数的运算
  @assertion.assert_eq(150, azimuth.multiply(azimuth.add(a, c), 30))
  
  // 链式运算
  @assertion.assert_eq(45, azimuth.add(azimuth.multiply(a, b), azimuth.add(c, a)))
}