// 标准 MoonBit 测试套件
// 包含 10 个测试用例，覆盖核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(6, 4))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-5, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  
  // 计算总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  // 验证总价：299*2 + 199*1 = 598 + 199 = 797
  assert_eq(797, cart_total)
}

test "assertion_functions_verification" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("test", "test")
  
  // 测试实际值
  assert_true(add(5, 3) == 8)
  assert_false(add(5, 3) == 9)
  assert_eq(8, add(5, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let base = 100
  let multiplier = 3
  let addition = 50
  let subtraction = 20
  
  // 计算：(base * multiplier) + addition - subtraction
  let step1 = multiply(base, multiplier)  // 300
  let step2 = add(step1, addition)        // 350
  let result = add(step2, -subtraction)   // 330
  
  assert_eq(330, result)
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temp = 25
  let fahrenheit_temp = add(multiply(celsius_temp, 2), 30)
  assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}