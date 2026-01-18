// Azimuth 项目标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 100))
  assert_eq(-1, multiply(-1, 100))
}

test "greet_basic_functionality" {
  // 测试基本的问候语功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界值
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试乘法边界值
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let area = multiply(base, height) / 2
  assert_eq(24, area)
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  let tax_rate = 10
  
  // 计算商品总价
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal = add(subtotal1, subtotal2)
  
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(736, total)  // (199×2 + 299×1) + (697×10/100) + 20 = 697 + 69 + 20 = 786
}

test "temperature_conversion_scenario" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化公式）
  let fahrenheit2 = 100
  let celsius2 = add(fahrenheit2, -30) / 2
  assert_eq(35, celsius2)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试正常情况
  assert_true(add(5, 3) == 8)
  assert_true(multiply(4, 6) == 24)
  assert_true(greet("Test") == "Hello, Test!")
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
}