// 简洁的 MoonBit 测试用例集
// 使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 基础加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
}

test "add_overflow_protection" {
  // 加法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
}

test "multiply_basic_functionality" {
  // 基础乘法功能测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "multiply_overflow_protection" {
  // 乘法溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_function_various_inputs" {
  // greet 函数各种输入测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3   // 3年
  
  // 利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  // 总额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "temperature_conversion" {
  // 温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30 (简化公式)
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "assertion_functions" {
  // 断言函数测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "edge_case_combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 复合运算
  let result = @azimuth.add(@azimuth.multiply(100, 2), @azimuth.multiply(50, 3))
  @azimuth.assert_eq(350, result)  // 200 + 150 = 350
}