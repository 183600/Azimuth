// 用户标准测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  let result = @azimuth.add(10, 20)
  if result != 30 { @builtin.abort("basic addition failed") }
  
  let result2 = @azimuth.add(-5, 5)
  if result2 != 0 { @builtin.abort("addition with opposite numbers failed") }
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_result = @azimuth.add(max_val, 1)
  if overflow_result != max_val { @builtin.abort("overflow protection failed") }
  
  let underflow_result = @azimuth.add(min_val, -1)
  if underflow_result != min_val { @builtin.abort("underflow protection failed") }
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(6, 7)
  if result != 42 { @builtin.abort("basic multiplication failed") }
  
  let result2 = @azimuth.multiply(-3, 4)
  if result2 != -12 { @builtin.abort("multiplication with negative number failed") }
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let result1 = @azimuth.multiply(0, 12345)
  if result1 != 0 { @builtin.abort("multiplication by zero failed") }
  
  let result2 = @azimuth.multiply(1, 9876)
  if result2 != 9876 { @builtin.abort("multiplication by one failed") }
  
  let result3 = @azimuth.multiply(-1, 555)
  if result3 != -555 { @builtin.abort("multiplication by negative one failed") }
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  let result = @azimuth.greet("World")
  if result != "Hello, World!" { @builtin.abort("basic greet failed") }
}

test "greet_edge_cases" {
  // 测试问候功能的边界情况
  let empty_result = @azimuth.greet("")
  if empty_result != "Hello, !" { @builtin.abort("greet with empty string failed") }
  
  let number_result = @azimuth.greet("123")
  if number_result != "Hello, 123!" { @builtin.abort("greet with numbers failed") }
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算矩形的周长：2 * (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  if perimeter != 30 { @builtin.abort("perimeter calculation failed") }
}

test "financial_calculation" {
  // 测试金融计算场景
  // 简单利息计算：本金 + (本金 * 利率 * 时间)
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  if total != 16000 { @builtin.abort("interest calculation failed") }
}

test "temperature_conversion" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  if fahrenheit != 80 { @builtin.abort("temperature conversion failed") }
}

test "combined_operations" {
  // 测试组合运算
  // 计算：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  if result != 22 { @builtin.abort("combined operations failed") }
}