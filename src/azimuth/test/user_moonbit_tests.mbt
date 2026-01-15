// 用户自定义的 MoonBit 测试用例
// 为 Azimuth 项目添加的标准测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-20, multiply(-4, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
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
  // 测试复杂计算场景
  let length = 8
  let width = 6
  let height = 4
  
  // 计算长方体表面积：2 × (长×宽 + 长×高 + 宽×高)
  let side1 = multiply(length, width)
  let side2 = multiply(length, height)
  let side3 = multiply(width, height)
  let surface_area = multiply(2, add(add(side1, side2), side3))
  
  assert_eq(208, surface_area)
}

test "assert_function_validation" {
  // 测试断言函数的有效性
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
}

test "financial_calculation" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
}

test "sequential_operations" {
  // 测试连续运算
  let base = 10
  
  // 连续加法：10 + 5 + 3 + 2 = 20
  let result1 = add(add(add(base, 5), 3), 2)
  assert_eq(20, result1)
  
  // 连续乘法：2 × 3 × 4 × 5 = 120
  let result2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, result2)
}

test "mixed_operations" {
  // 测试混合运算
  let a = 8
  let b = 3
  let c = 5
  
  // (a + b) × c = (8 + 3) × 5 = 55
  let result1 = multiply(add(a, b), c)
  assert_eq(55, result1)
  
  // a × b + c = 8 × 3 + 5 = 29
  let result2 = add(multiply(a, b), c)
  assert_eq(29, result2)
}