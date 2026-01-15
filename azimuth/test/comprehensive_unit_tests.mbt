// 综合单元测试 - 覆盖 azimuth 模块的核心功能
// 使用标准的 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "add_function_zero_handling" {
  // 测试零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
}

test "multiply_function_basic_cases" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_identity_elements" {
  // 测试乘法单位元
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(1, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
}

test "greet_function_various_inputs" {
  // 测试字符串拼接功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试最小值乘以-1的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "assertion_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(10, 10)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_properties" {
  // 测试数学运算性质
  let test_values = [0, 1, -1, 10, -10, 100, -100]
  
  // 测试加法交换律
  for value in test_values {
    @azimuth.assert_eq(@azimuth.add(5, value), @azimuth.add(value, 5))
  }
  
  // 测试乘法交换律
  for value in test_values {
    @azimuth.assert_eq(@azimuth.multiply(5, value), @azimuth.multiply(value, 5))
  }
  
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 3
  let b = 4
  let c = 5
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}