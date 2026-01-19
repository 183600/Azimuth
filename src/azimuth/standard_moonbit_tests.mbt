// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入 azimuth 模块
use azimuth

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, azimuth.add(3, 7))
  assert_eq(0, azimuth.add(5, -5))
  assert_eq(-8, azimuth.add(-3, -5))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-12, azimuth.multiply(3, -4))
  assert_eq(0, azimuth.multiply(0, 100))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 用户123!", azimuth.greet("用户123"))
}

test "boundary_values_addition" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(-1, azimuth.add(max_val, min_val))
}

test "boundary_values_multiplication" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  assert_eq(max_val, azimuth.multiply(max_val, 1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律
  assert_eq(azimuth.multiply(a, azimuth.add(b, 3)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 3)))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = azimuth.multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(26, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "@#$%^&*()"
  assert_eq_string("Hello, @#$%^&*()!", azimuth.greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  assert_eq_string("Hello, Hello World!", azimuth.greet(spaced_string))
  
  // 测试数字字符串
  let numeric_string = "2023"
  assert_eq_string("Hello, 2023!", azimuth.greet(numeric_string))
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 简单的贷款计算：本金 + 利息
  let principal = 10000  // 本金
  let rate = 5          // 利率（百分比）
  let years = 3         // 年数
  
  // 利息 = 本金 × 利率 × 年数
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  // 总额 = 本金 + 利息
  let total = azimuth.add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "error_handling_and_assertions" {
  // 测试错误处理和断言
  // 测试断言函数
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(azimuth.add(max_val, 100) == max_val)
  assert_true(azimuth.multiply(max_val, 2) == max_val)
}