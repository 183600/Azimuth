// Azimuth 项目新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，包含断言验证

test "add_function_zero_cases" {
  // 测试 add 函数的零值情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(0, -3))
  assert_eq(-7, add(-7, 0))
}

test "add_function_large_numbers" {
  // 测试 add 函数处理大数
  assert_eq(1000000, add(500000, 500000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(1000000, -1000000))
  assert_eq(-500000, add(-200000, -300000))
}

test "multiply_function_zero_cases" {
  // 测试 multiply 函数的零值情况
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-3, 0))
  assert_eq(0, multiply(0, -7))
}

test "multiply_function_power_cases" {
  // 测试 multiply 函数的幂次情况
  assert_eq(8, multiply(2, 4))  // 2^3
  assert_eq(27, multiply(3, 9))  // 3^3
  assert_eq(16, multiply(4, 4))  // 4^2
  assert_eq(125, multiply(5, 25))  // 5^3
}

test "greet_function_whitespace_cases" {
  // 测试 greet 函数的空白字符情况
  assert_eq_string("Hello, World!", greet(" World"))
  assert_eq_string("Hello, World!", greet("World "))
  assert_eq_string("Hello, World!", greet(" World "))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t!", greet("\t"))
}

test "greet_function_unicode_cases" {
  // 测试 greet 函数的 Unicode 字符情况
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, Español!", greet("Español"))
}

test "complex_business_calculation" {
  // 测试复杂商业计算：折扣和税费
  let original_price = 1000
  let discount_rate = 20  // 20% 折扣
  let tax_rate = 8       // 8% 税费
  
  // 计算折扣价
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算最终价格（含税）
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(800, discounted_price)  // 1000 - 200
  assert_eq(864, final_price)       // 800 + 64
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列 1+2+3+...+10 的和
  let n = 10
  let sum_arithmetic = multiply(n, add(1, n)) / 2
  assert_eq(55, sum_arithmetic)
  
  // 计算等比数列 2+4+8+16 的和
  let sum_geometric = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum_geometric)
}

test "temperature_conversion" {
  // 测试温度转换计算
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 100
  let celsius2 = multiply(add(fahrenheit2, -32), 5) / 9
  assert_eq(37, celsius2)  // 四舍五入
}

test "array_index_calculation" {
  // 测试数组索引计算
  let rows = 5
  let cols = 4
  let row = 2
  let col = 3
  
  // 计算二维数组在二维数组中的索引（行优先）
  let index_row_major = add(multiply(row, cols), col)
  assert_eq(11, index_row_major)
  
  // 计算二维数组在二维数组中的索引（列优先）
  let index_col_major = add(multiply(col, rows), row)
  assert_eq(17, index_col_major)
}