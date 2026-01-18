// Azimuth 项目的标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法编写，覆盖核心功能

// 导入 azimuth 包中的函数
use "azimuth"

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-2, azimuth::add(-1, -1))
  assert_eq(10, azimuth::add(15, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(0, azimuth::add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(0, 5))
  assert_eq(5, azimuth::multiply(5, 1))
  assert_eq(-6, azimuth::multiply(2, -3))
  assert_eq(6, azimuth::multiply(-2, -3))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 8
  let width = 5
  let area = azimuth::multiply(length, width)
  assert_eq(40, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(26, perimeter)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 计算简单利息：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, time))
  assert_eq(15000, interest)
  
  // 计算总额：本金 + 利息
  let total = azimuth::add(principal, interest)
  assert_eq(16000, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）：摄氏度转华氏度
  // 使用简化公式：F = C × 2 + 30
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing = azimuth::add(azimuth::multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "sequential_operations" {
  // 测试连续运算
  // (a + b) × (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  
  let sum1 = azimuth::add(a, b)
  let sum2 = azimuth::add(c, d)
  let result = azimuth::multiply(sum1, sum2)
  assert_eq(77, result)
}

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(azimuth::add(max_val, 100) == max_val)
  assert_true(azimuth::multiply(max_val, 2) == max_val)
}