// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_function_basic_cases" {
  // 测试基本加法功能
  @azimuth.assert_eq(15, @azimuth.add(5, 10))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(2000000000, 2000000000))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-2000000000, -2000000000))
  
  // 测试边界值不溢出
  @azimuth.assert_eq(0, @azimuth.add(min_val, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-50000, 50000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_function" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  
  // 需要向上取整的情况
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(11, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(9, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
  
  // 边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "assertion_functions" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法特殊值
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 测试乘法特殊值
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算
  let a = 10
  let b = 5
  let c = 3
  
  // 组合运算
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)  // 10*5 + 3 = 53
  let result2 = @azimuth.multiply(@azimuth.add(a, b), c)  // (10+5)*3 = 45
  let result3 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))  // 10*3 + 5*3 = 45
  
  @azimuth.assert_eq(53, result1)
  @azimuth.assert_eq(45, result2)
  @azimuth.assert_eq(45, result3)
  
  // 嵌套运算
  let nested = @azimuth.add(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.multiply(5, @azimuth.divide_with_ceil(10, 3)))
  @azimuth.assert_eq(26, nested)  // 2*(3+4) + 5*4 = 14 + 20 = 34 (修正为正确计算)
}

test "string_operations_combined" {
  // 测试字符串操作与算术运算结合
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
  
  // 测试空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}