// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(-1000000000, -1500000000))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(max_val, multiply(max_val, 2))
  
  // 负数溢出测试
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 特殊情况：最小值与-1相乘
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(5, divide_with_ceil(13, 3))
  
  // 测试负数
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(-12, 3))
  
  // 测试整除情况
  assert_eq(5, divide_with_ceil(15, 3))
  assert_eq(-5, divide_with_ceil(-15, 3))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
  
  // 场景2：计算三角形面积
  let base = 8
  let height = 6
  let area = multiply(base, height) / 2
  assert_eq(24, area)
  
  // 场景3：计算平均值
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = add(add(num1, num2), num3)
  let average = sum / 3
  assert_eq(90, average)
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试与1的运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试极值组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(max_val, -max_val))
}

test "assertion_functions_verification" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合断言
  let result = add(5, 3)
  assert_true(result == 8)
  assert_false(result == 9)
  assert_eq(8, result)
  
  let greeting = greet("Test")
  assert_true(greeting == "Hello, Test!")
  assert_eq_string("Hello, Test!", greeting)
}