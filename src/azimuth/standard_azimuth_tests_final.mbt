// 标准 MoonBit 测试用例 - Azimuth 项目
// 包含核心功能的全面测试

test "add_basic_functionality" {
  // 基础加法功能测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_boundary_values" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 0))
}

test "multiply_basic_functionality" {
  // 基础乘法功能测试
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_boundary_values" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // greet 函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试：计算矩形面积和周长
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
  // 金融计算场景测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  // 总金额 = 本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}

test "assert_functions_validation" {
  // 断言函数验证测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 3
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}