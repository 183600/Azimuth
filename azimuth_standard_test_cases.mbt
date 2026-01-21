// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(100, add(50, 50))
}

test "add_overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-8, multiply(-2, 4))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 3))
}

test "greet_functionality" {
  // 测试 greet 函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(4, divide_with_ceil(11, 3))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形周长
  let length = 8
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(26, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简化版复利
  let principal = 1000
  let rate = 5
  let years = 2
  let total = multiply(principal, add(100, multiply(rate, years))) / 100
  assert_eq(1100, total)
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, add(max_val, 0))
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let result = add(10, 20)
  assert_true(result == 30)
  assert_false(result == 25)
}