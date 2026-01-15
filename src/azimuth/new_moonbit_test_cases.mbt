// 新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 基本加法测试
  assert_eq(10, add(6, 4))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-6, -4))
}

test "add_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 基本乘法测试
  assert_eq(12, multiply(3, 4))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(5, 0))
}

test "multiply_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 金融计算场景：简单利息计算
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 利息 = 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 总额 = 本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "temperature_conversion" {
  // 温度转换场景：摄氏度转华氏度（简化版）
  // F = C × 2 + 30 (简化公式，避免小数)
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "mathematical_properties" {
  // 数学性质验证
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}