// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的主要功能

test "add_function_basic_operations" {
  // 基本加法运算测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "add_function_overflow_handling" {
  // 加法溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_operations" {
  // 基本乘法运算测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(8, -3))
}

test "complex_arithmetic_operations" {
  // 复杂算术运算测试
  let a = 10
  let b = 3
  
  // 加法后乘法
  @azimuth.assert_eq(39, @azimuth.multiply(@azimuth.add(a, b), 3))
  
  // 乘法后加法
  @azimuth.assert_eq(33, @azimuth.add(@azimuth.multiply(a, b), 3))
  
  // 复杂表达式
  @azimuth.assert_eq(16, @azimuth.add(@azimuth.multiply(2, 5), @azimuth.divide_with_ceil(10, 3)))
}

test "assertion_functions_verification" {
  // 断言函数验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  
  // 零元素性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  // 单位元素性质
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}