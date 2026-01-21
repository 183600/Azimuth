// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_cases" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "add_function_overflow_boundary" {
  // 加法溢出边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_cases" {
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试0和1的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
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
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  
  // 零除法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_arithmetic_operations" {
  // 复杂算术运算测试
  let a = 10
  let b = 5
  let c = -3
  
  // 混合运算
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(a, b)
  let mixed_result = @azimuth.add(product, @azimuth.multiply(sum, c))
  
  @azimuth.assert_eq(15, sum)
  @azimuth.assert_eq(50, product)
  @azimuth.assert_eq(5, mixed_result)
}

test "mathematical_properties" {
  // 数学性质测试
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(3, 5), @azimuth.add(5, 3))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(3, 5), @azimuth.multiply(5, 3))
  
  // 加法单位元
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  
  // 乘法单位元
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 接近边界的运算
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 100, 99))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 100, -99))
}

test "assertion_functions_verification" {
  // 断言函数验证测试
  // 布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 2)
  
  // 相等断言
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("", "")
}
