// 新增的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let c = -100
  let d = 200
  assert_eq(add(c, d), add(d, c))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
}

test "add_with_large_numbers" {
  // 测试大数相加
  let large_num1 = 1000000000
  let large_num2 = 1000000000
  assert_eq(2000000000, add(large_num1, large_num2))
  
  // 测试负大数相加
  let neg_large1 = -1000000000
  let neg_large2 = -1000000000
  assert_eq(-2000000000, add(neg_large1, neg_large2))
}

test "multiply_with_powers_of_two" {
  // 测试2的幂相乘
  assert_eq(8, multiply(2, 4))
  assert_eq(16, multiply(4, 4))
  assert_eq(64, multiply(8, 8))
  assert_eq(1024, multiply(32, 32))
}

test "calculation_scenario_area" {
  // 实际应用场景：计算矩形面积
  let length = 15
  let width = 8
  let area = multiply(length, width)
  assert_eq(120, area)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(46, perimeter)
}

test "calculation_scenario_temperature" {
  // 实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing = 0
  let freezing_f = add(multiply(freezing, 2), 30)
  assert_eq(30, freezing_f)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值与1的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", greet("abcdefghijklmnopqrstuvwxyz"))
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let a = 10
  let b = 5
  let c = 3
  let d = 2
  
  // 计算：(a + b) × (c - d) + a × b
  let result1 = add(multiply(add(a, b), add(c, -d)), multiply(a, b))
  assert_eq(85, result1)
  
  // 计算：a × b + c × d - a × c
  let result2 = add(add(multiply(a, b), multiply(c, d)), multiply(-a, c))
  assert_eq(46, result2)
}