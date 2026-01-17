// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，只使用已存在的函数

test "add_large_numbers" {
  // 测试大数相加
  assert_eq(add(1000000, 2000000), 3000000)
  assert_eq(add(-1000000, -2000000), -3000000)
  assert_eq(add(2147483647, 0), 2147483647)
  assert_eq(add(-2147483648, 0), -2147483648)
}

test "add_mixed_signs" {
  // 测试不同符号的数相加
  assert_eq(add(100, -100), 0)
  assert_eq(add(100, -150), -50)
  assert_eq(add(-100, 150), 50)
  assert_eq(add(2147483647, -1), 2147483646)
}

test "multiply_large_numbers" {
  // 测试大数相乘
  assert_eq(multiply(1000, 1000), 1000000)
  assert_eq(multiply(-1000, 1000), -1000000)
  assert_eq(multiply(-1000, -1000), 1000000)
  assert_eq(multiply(46341, 46341), 2147483647)
}

test "multiply_boundary_combinations" {
  // 测试边界值组合的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(multiply(max_val, 0), 0)
  assert_eq(multiply(min_val, 0), 0)
  assert_eq(multiply(max_val, 1), max_val)
  assert_eq(multiply(min_val, 1), min_val)
  assert_eq(multiply(min_val, -1), min_val)
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string(greet("测试@123"), "Hello, 测试@123!")
  assert_eq_string(greet("🌙"), "Hello, 🌙!")
  assert_eq_string(greet(" spaces "), "Hello, spaces !")
  assert_eq_string(greet(""), "Hello, !")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(15, 3), 5)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(9, 2), 5)
  assert_eq(divide_with_ceil(-7, 3), -2)
}

test "mathematical_sequences" {
  // 测试数学序列计算
  
  // 计算前 5 个自然数的和: 1+2+3+4+5 = 15
  let sum_first_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_first_5)
  
  // 计算前 4 个偶数的和: 2+4+6+8 = 20
  let sum_even_4 = add(add(add(2, 4), 6), 8)
  assert_eq(20, sum_even_4)
}

test "temperature_conversion" {
  // 测试温度转换场景 (摄氏度到华氏度)
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  let celsius2 = 0
  let fahrenheit2 = add(multiply(celsius2, 9) / 5, 32)
  assert_eq(32, fahrenheit2)
}

test "assert_functions_test" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}