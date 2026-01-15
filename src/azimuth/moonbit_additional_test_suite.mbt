// MoonBit 额外测试套件
// 包含10个标准 MoonBit 测试用例

test "arithmetics_basic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
}

test "arithmetics_negative_numbers" {
  // 负数运算测试
  assert_eq(-2, add(3, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算矩形周长：2 × (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
  
  // 计算三角形面积（简化）：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let area = multiply(base, height) / 2
  assert_eq(30, area)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "real_world_financial_calculation" {
  // 实际金融计算测试
  // 计算贷款总还款额：本金 + 利息
  let principal = 10000
  let annual_rate = 6  // 6%
  let years = 3
  
  // 简化利息计算：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(annual_rate, years))
  let total_payment = add(principal, interest)
  
  assert_eq(280000, total_payment)  // 10000 + 10000 × 6 × 3 = 280000
}

test "temperature_conversion_calculation" {
  // 温度转换计算测试
  // 摄氏度转华氏度（简化公式）：F = C × 2 + 30
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  // 测试室温
  let room_celsius = 20
  let room_fahrenheit = add(multiply(room_celsius, 2), 30)
  assert_eq(70, room_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "assert_functions_functionality" {
  // 断言函数功能测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试表达式结果
  assert_true(add(5, 3) == 8)
  assert_false(add(5, 3) == 9)
  assert_true(multiply(4, 6) == 24)
  assert_false(multiply(4, 6) == 25)
}

test "sequential_operations_evaluation" {
  // 连续操作评估测试
  // 计算 (a + b) × (c + d) - e
  let a = 10
  let b = 15
  let c = 5
  let d = 8
  let e = 20
  
  let sum1 = add(a, b)  // 25
  let sum2 = add(c, d)  // 13
  let product = multiply(sum1, sum2)  // 325
  let result = add(product, -e)  // 305
  
  assert_eq(305, result)
}