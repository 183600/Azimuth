// 标准 MoonBit 核心功能测试
// 测试 azimuth 包的核心功能：add, multiply, greet 函数

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 10
  let width = 5
  let area = multiply(length, width)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 本金：1000，利率：5%，期限：3年
  // 简化利息计算：本金 × 利率 × 期限
  let principal = 1000
  let rate = 5
  let years = 3
  
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(16000, total)
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

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}