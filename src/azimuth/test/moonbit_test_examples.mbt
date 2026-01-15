// MoonBit 测试用例示例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic" {
  // 基本乘法测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_function_standard" {
  // 标准问候测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_function_special_cases" {
  // 特殊情况问候测试
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
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let time = 3           // 时间 3年
  
  // 利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "temperature_conversion" {
  // 温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30 (简化公式，避免小数)
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25×2 + 30 = 80
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "assert_functions_verification" {
  // 断言函数验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  
  // 测试计算结果
  let result = @azimuth.add(10, 20)
  @azimuth.assert_true(result == 30)
  @azimuth.assert_false(result == 25)
}