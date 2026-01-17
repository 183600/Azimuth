// 用户标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_associative_law" {
  // 测试数学结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let left_side = @azimuth.add(@azimuth.add(a, b), c)
  let right_side = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "overflow_handling_tests" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @123!", @azimuth.greet("@123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let area = @azimuth.multiply(length, width)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(96, area)
  @azimuth.assert_eq(40, perimeter)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 简单的贷款计算：本金 + 利息
  let principal = 10000
  let rate = 5  // 5%
  let years = 2
  
  // 计算利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算总额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(110000, total)  // 10000 + 10000*5*2 = 110000
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}