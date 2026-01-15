// 新增的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-3, 3))
  assert_eq(-10, add(-3, -7))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(3, 0))
  assert_eq(1, multiply(1, 5))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
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
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景：简单利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算利息：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 计算总额：本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "mathematical_properties" {
  // 测试数学性质：交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "assert_functions_verification" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试比较结果
  assert_true(add(5, 3) == 8)
  assert_false(add(5, 3) == 9)
  assert_true(multiply(4, 6) == 24)
  assert_false(multiply(4, 6) == 25)
}