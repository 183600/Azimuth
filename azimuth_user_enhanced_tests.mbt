// Azimuth 项目用户增强测试用例
// 使用标准 MoonBit 测试语法，全面覆盖项目功能

// 导入必要的模块
use azimuth

test "add_basic_functionality" {
  // 测试 add 函数的基本功能
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(-1, azimuth.add(2, -3))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
}

test "multiply_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(0, 5))?
  @assertion.assert_eq(-6, azimuth.multiply(2, -3))?
  @assertion.assert_eq(25, azimuth.multiply(5, 5))?
}

test "greet_basic_functionality" {
  // 测试 greet 函数的基本功能
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, Bob!", azimuth.greet("Bob"))?
  @assertion.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
}

test "boundary_value_tests" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  @assertion.assert_eq(max_int, azimuth.add(max_int, 0))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, 0))?
  @assertion.assert_eq(max_int, azimuth.multiply(max_int, 1))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, 1))?
}

test "overflow_handling_tests" {
  // 测试溢出处理
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 正数加法溢出
  @assertion.assert_eq(max_int, azimuth.add(max_int, 1))?
  @assertion.assert_eq(max_int, azimuth.add(max_int, max_int))?
  
  // 负数加法溢出
  @assertion.assert_eq(min_int, azimuth.add(min_int, -1))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, min_int))?
  
  // 乘法溢出
  @assertion.assert_eq(max_int, azimuth.multiply(max_int, 2))?
  @assertion.assert_eq(min_int, azimuth.multiply(min_int, 2))?
}

test "negative_number_tests" {
  // 测试负数运算
  @assertion.assert_eq(-10, azimuth.add(-3, -7))?
  @assertion.assert_eq(15, azimuth.multiply(-3, -5))?
  @assertion.assert_eq(-12, azimuth.multiply(-3, 4))?
  @assertion.assert_eq(2, azimuth.add(-5, 7))?
  @assertion.assert_eq(-8, azimuth.multiply(2, -4))?
}

test "zero_value_tests" {
  // 测试零值运算
  @assertion.assert_eq(0, azimuth.multiply(0, 123))?
  @assertion.assert_eq(0, azimuth.multiply(456, 0))?
  @assertion.assert_eq(123, azimuth.add(0, 123))?
  @assertion.assert_eq(-456, azimuth.add(0, -456))?
  @assertion.assert_eq(0, azimuth.multiply(0, 0))?
}

test "string_special_character_tests" {
  // 测试字符串特殊字符
  @assertion.assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))?
  @assertion.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
  @assertion.assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))?
}

test "complex_operation_tests" {
  // 测试复合运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let temp = azimuth.add(a, b)
  let result = azimuth.multiply(temp, c)
  @assertion.assert_eq(45, result)?
  
  // a * b + c
  let temp2 = azimuth.multiply(a, b)
  let result2 = azimuth.add(temp2, c)
  @assertion.assert_eq(53, result2)?
}

test "custom_assertion_functions" {
  // 测试自定义断言函数
  azimuth.assert_eq(10, 10)
  azimuth.assert_eq(0, 0)
  azimuth.assert_eq(-5, -5)
  
  azimuth.assert_eq_string("test", "test")
  azimuth.assert_eq_string("", "")
  azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  azimuth.assert_true(true)
  azimuth.assert_false(false)
}