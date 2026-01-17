// 用户自定义 MoonBit 测试用例
// 覆盖 azimuth 项目核心函数的各种测试场景

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
  assert_eq(100, azimuth::add(0, 100))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(21, azimuth::multiply(7, 3))
  assert_eq(-15, azimuth::multiply(5, -3))
  assert_eq(12, azimuth::multiply(-4, -3))
  assert_eq(0, azimuth::multiply(0, 42))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "mathematical_expressions" {
  // 测试数学表达式计算
  // (a + b) * c = d
  let a = 5
  let b = 3
  let c = 4
  let sum_ab = azimuth::add(a, b)
  let result = azimuth::multiply(sum_ab, c)
  assert_eq(32, result)
  
  // a * b + c * d = e
  let e = azimuth::add(azimuth::multiply(6, 7), azimuth::multiply(8, 9))
  assert_eq(114, e)
}

test "boundary_values_testing" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界值
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
  
  // 测试乘法边界值
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "real_world_calculations" {
  // 测试实际应用场景计算
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = azimuth::multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 计算商品总价（含税）
  let unit_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  
  // 计算商品小计
  let subtotal = azimuth::multiply(unit_price, quantity)
  // 计算税费
  let tax = azimuth::multiply(subtotal, tax_rate) / 100
  // 计算总价
  let total = azimuth::add(subtotal, tax)
  
  assert_eq(645, total)  // 199*3 + (199*3*8/100) = 597 + 47 = 644，四舍五入为645
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", azimuth::greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe"
  assert_eq_string("Hello, John Doe!", azimuth::greet(spaced_name))
  
  // 测试包含数字的字符串
  let numeric_name = "User2023"
  assert_eq_string("Hello, User2023!", azimuth::greet(numeric_name))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = azimuth::multiply(n, azimuth::add(n, 1)) / 2
  assert_eq(55, sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::multiply(1, 1), azimuth::multiply(2, 2)), azimuth::multiply(3, 3)), azimuth::add(azimuth::multiply(4, 4), azimuth::multiply(5, 5)))
  assert_eq(55, square_sum)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 验证溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(azimuth::add(max_val, 100) == max_val)
  assert_true(azimuth::add(min_val, -100) == min_val)
  assert_true(azimuth::multiply(max_val, 2) == max_val)
  assert_true(azimuth::multiply(min_val, 2) == min_val)
  
  // 验证正常计算
  assert_true(azimuth::add(10, 20) == 30)
  assert_true(azimuth::multiply(6, 7) == 42)
  assert_true(azimuth::greet("Test") == "Hello, Test!")
}