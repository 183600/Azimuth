// Azimuth 项目新的标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-12, multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 4)), add(multiply(a, b), multiply(a, 4)))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形周长和面积
  let length = 8
  let width = 6
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
  
  // 计算面积：长 × 宽
  let area = multiply(length, width)
  assert_eq(48, area)
}

test "financial_calculation" {
  // 测试金融计算：简单利息计算
  let principal = 1000
  let rate = 5
  let time = 3
  
  // 利息 = 本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  // 总金额 = 本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(16000, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（整数近似）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
}

test "logical_operations" {
  // 测试逻辑运算和条件表达式
  let x = 10
  let y = 20
  
  // 测试比较和条件
  assert_true(x < y)
  assert_false(x > y)
  
  // 条件表达式测试
  let max_value = if (x > y) { x } else { y }
  assert_eq(20, max_value)
  
  let min_value = if (x < y) { x } else { y }
  assert_eq(10, min_value)
}

test "sequence_calculations" {
  // 测试数列计算
  // 计算等差数列前5项和：1 + 2 + 3 + 4 + 5 = 15
  let sum_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_5)
  
  // 计算等比数列前4项和：2 + 4 + 8 + 16 = 30
  let sum_geo = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum_geo)
  
  // 计算平方和：1² + 2² + 3² = 1 + 4 + 9 = 14
  let sum_squares = add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3))
  assert_eq(14, sum_squares)
}

test "error_handling_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}