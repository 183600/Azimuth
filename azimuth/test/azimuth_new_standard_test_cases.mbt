// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法
// 测试 lib.mbt 中的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(1, 2), 3)
  @azimuth.assert_eq(@azimuth.add(-1, 1), 0)
  @azimuth.assert_eq(@azimuth.add(0, 5), 5)
  @azimuth.assert_eq(@azimuth.add(5, 0), 5)
  @azimuth.assert_eq(@azimuth.add(-3, -2), -5)
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(max_val/2, max_val/2 + 1), max_val)
  
  // 测试负数溢出
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(min_val/2, min_val/2 - 1), min_val)
  
  // 测试最小值特殊情况
  @azimuth.assert_eq(@azimuth.add(min_val, 0), min_val)
  @azimuth.assert_eq(@azimuth.add(0, min_val), min_val)
  @azimuth.assert_eq(@azimuth.add(min_val, 1), min_val + 1)
  @azimuth.assert_eq(@azimuth.add(1, min_val), min_val + 1)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(-2, 3), -6)
  @azimuth.assert_eq(@azimuth.multiply(2, -3), -6)
  @azimuth.assert_eq(@azimuth.multiply(-2, -3), 6)
  @azimuth.assert_eq(@azimuth.multiply(0, 5), 0)
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
}

test "multiply_edge_cases" {
  // 测试乘法边界情况
  @azimuth.assert_eq(@azimuth.multiply(1, 5), 5)
  @azimuth.assert_eq(@azimuth.multiply(5, 1), 5)
  @azimuth.assert_eq(@azimuth.multiply(-1, 5), -5)
  @azimuth.assert_eq(@azimuth.multiply(5, -1), -5)
  @azimuth.assert_eq(@azimuth.multiply(-1, -5), 5)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2), max_val)
  @azimuth.assert_eq(@azimuth.multiply(46341, 46341), max_val)  // sqrt(max_val) + 1
  
  // 测试负数溢出
  @azimuth.assert_eq(@azimuth.multiply(min_val, 2), min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -2), max_val)
  
  // 测试最小值特殊情况
  @azimuth.assert_eq(@azimuth.multiply(min_val, 1), min_val)
  @azimuth.assert_eq(@azimuth.multiply(1, min_val), min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.multiply(-1, min_val), min_val)
}

test "greet_functionality" {
  // 测试 greet 函数
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
}

test "assert_functions" {
  // 测试断言函数
  let int_val = 42
  let string_val = "test"
  let bool_true = true
  let bool_false = false
  
  // 测试成功断言（不会抛出异常）
  @azimuth.assert_eq(42, int_val)
  @azimuth.assert_eq_string("test", string_val)
  @azimuth.assert_true(bool_true)
  @azimuth.assert_false(bool_false)
}

test "complex_operations" {
  // 测试复合操作
  let a = 10
  let b = 20
  let c = -5
  
  // 复合加法运算
  @azimuth.assert_eq(@azimuth.add(a, b), 30)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), 25)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), @azimuth.add(c, a)), 35)
  
  // 复合乘法运算
  @azimuth.assert_eq(@azimuth.multiply(a, b), 200)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), 2), 400)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3)), 360)
  
  // 混合运算
  @azimuth.assert_eq(@azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3)), 80)
  
  // 测试字符串与数字结合
  let _greeting = @azimuth.greet("Result")
  let _final_result = @azimuth.add(a, @azimuth.multiply(b, c))
}