// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

test "arithmetic_commutativity" {
  // 测试加法和乘法的交换律
  let a = 123
  let b = 456
  
  // 加法交换律: a + b = b + a
  let add_result1 = azimuth::add(a, b)
  let add_result2 = azimuth::add(b, a)
  assert_eq(add_result1, add_result2)
  
  // 乘法交换律: a * b = b * a
  let mul_result1 = azimuth::multiply(a, b)
  let mul_result2 = azimuth::multiply(b, a)
  assert_eq(mul_result1, mul_result2)
}

test "arithmetic_associativity" {
  // 测试加法和乘法的结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let add_result1 = azimuth::add(azimuth::add(a, b), c)
  let add_result2 = azimuth::add(a, azimuth::add(b, c))
  assert_eq(add_result1, add_result2)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let mul_result1 = azimuth::multiply(azimuth::multiply(a, b), c)
  let mul_result2 = azimuth::multiply(a, azimuth::multiply(b, c))
  assert_eq(mul_result1, mul_result2)
}

test "distributive_property" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left, right)
}

test "identity_elements" {
  // 测试单位元
  let x = 42
  
  // 加法单位元: x + 0 = x
  assert_eq(azimuth::add(x, 0), x)
  assert_eq(azimuth::add(0, x), x)
  
  // 乘法单位元: x * 1 = x
  assert_eq(azimuth::multiply(x, 1), x)
  assert_eq(azimuth::multiply(1, x), x)
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq(azimuth::greet("user@domain.com"), "Hello, user@domain.com!")
  assert_eq(azimuth::greet("test-case"), "Hello, test-case!")
  assert_eq(azimuth::greet("number_123"), "Hello, number_123!")
  assert_eq(azimuth::greet("special!@#$%"), "Hello, special!@#$%!")
}

test "greet_function_multilingual" {
  // 测试 greet 函数处理多语言文本
  assert_eq(azimuth::greet("中文"), "Hello, 中文!")
  assert_eq(azimuth::greet("Español"), "Hello, Español!")
  assert_eq(azimuth::greet("Français"), "Hello, Français!")
  assert_eq(azimuth::greet("Русский"), "Hello, Русский!")
  assert_eq(azimuth::greet("العربية"), "Hello, العربية!")
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  
  // 场景1: 计算三角形面积 (底 * 高 / 2)
  let base = 10
  let height = 8
  let area = azimuth::multiply(base, height) / 2
  assert_eq(area, 40)
  
  // 场景2: 计算圆的周长近似值 (2 * π * r，使用 π ≈ 3)
  let radius = 7
  let circumference = azimuth::multiply(2, azimuth::multiply(3, radius))
  assert_eq(circumference, 42)
  
  // 场景3: 计算复利 (本金 * (1 + 利率)^年数，简化为单利)
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years)) / 100
  let total = azimuth::add(principal, interest)
  assert_eq(total, 1150)
}

test "boundary_value_testing" {
  // 测试边界值
  
  // 测试零值
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::multiply(0, 0), 0)
  
  // 测试单位值
  assert_eq(azimuth::add(1, -1), 0)
  assert_eq(azimuth::multiply(1, 1), 1)
  
  // 测试小数值
  assert_eq(azimuth::add(1, 1), 2)
  assert_eq(azimuth::multiply(2, 3), 6)
}