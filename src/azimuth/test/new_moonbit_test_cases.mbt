// 新增的 MoonBit 测试用例
// 测试项目的核心功能，使用标准的 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "boundary_value_tests" {
  // 测试边界值情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 5
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算多项式值：ax² + bx + c
  let x = 4
  let a = 2
  let b = 3
  let c = 5
  
  let x_squared = @azimuth.multiply(x, x)  // 16
  let ax_squared = @azimuth.multiply(a, x_squared)  // 32
  let bx = @azimuth.multiply(b, x)  // 12
  
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  @azimuth.assert_eq(49, result)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 简单的贷款计算：本金 + 利息
  let principal = 10000
  let rate = 6  // 6%
  let years = 2
  
  // 利息 = 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 总额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(130000, total)  // 10000 + 10000*6*2 = 130000
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", @azimuth.greet(long_string))
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet(spaced_string))
}

test "assertion_functions_test" {
  // 测试断言函数
  let test_value = 42
  let test_string = "MoonBit"
  
  // 测试各种断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(test_value, test_value)
  @azimuth.assert_eq_string(test_string, test_string)
  
  // 测试计算结果的断言
  let add_result = @azimuth.add(15, 27)
  @azimuth.assert_eq(42, add_result)
  
  let multiply_result = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, multiply_result)
  
  let greet_result = @azimuth.greet("Tester")
  @azimuth.assert_eq_string("Hello, Tester!", greet_result)
}