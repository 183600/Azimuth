// 用户自定义简单 MoonBit 测试用例
// 覆盖 azimuth 项目核心函数的基本测试场景

test "add_basic_test" {
  // 测试 add 函数的基本功能
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
}

test "multiply_basic_test" {
  // 测试 multiply 函数的基本功能
  assert_eq(21, azimuth::multiply(7, 3))
  assert_eq(-15, azimuth::multiply(5, -3))
  assert_eq(12, azimuth::multiply(-4, -3))
}

test "greet_basic_test" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
}

test "math_expressions_test" {
  // 测试数学表达式计算
  let a = 5
  let b = 3
  let c = 4
  let sum_ab = azimuth::add(a, b)
  let result = azimuth::multiply(sum_ab, c)
  assert_eq(32, result)
}

test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "real_world_calculations_test" {
  // 测试实际应用场景计算
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = azimuth::multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculations_test" {
  // 测试金融计算场景
  let unit_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  
  // 计算商品小计
  let subtotal = azimuth::multiply(unit_price, quantity)
  // 计算税费
  let tax = azimuth::multiply(subtotal, tax_rate) / 100
  // 计算总价
  let total = azimuth::add(subtotal, tax)
  
  assert_eq(645, total)
}

test "string_processing_test" {
  // 测试字符串处理
  assert_eq_string("Hello, !@#$!", azimuth::greet("!@#$"))
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  assert_eq_string("Hello, User2023!", azimuth::greet("User2023"))
}

test "complex_calculation_test" {
  // 测试复杂计算场景
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = azimuth::multiply(n, azimuth::add(n, 1)) / 2
  assert_eq(55, sum)
}

test "error_handling_test" {
  // 测试错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(azimuth::add(max_val, 100) == max_val)
  assert_true(azimuth::add(min_val, -100) == min_val)
  assert_true(azimuth::multiply(max_val, 2) == max_val)
  assert_true(azimuth::multiply(min_val, 2) == min_val)
}