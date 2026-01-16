// 额外的MoonBit测试用例
// 测试一些边界情况和特殊场景

test "add_with_minimum_values" {
  // 测试最小值相加的特殊情况
  let min_val = -2147483648
  let result1 = @azimuth.add(min_val, 0)
  let result2 = @azimuth.add(0, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

test "add_mixed_signs" {
  // 测试正负数相加的各种情况
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-50, @azimuth.add(100, -150))
  @azimuth.assert_eq(50, @azimuth.add(-100, 150))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "multiply_boundary_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
}

test "greet_with_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "assert_functions_with_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq(2147483647, 2147483647)
  
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
}

test "complex_addition_scenarios" {
  // 测试复杂的加法场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 100, -100))
  
  // 零参与的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法的各种情况
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(-10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(-10, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
}

test "add_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的性质
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(17, @azimuth.multiply(-17, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "add_sequence_pattern" {
  // 测试序列模式相加
  // 斐波那契数列前几项: 1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(16, @azimuth.multiply(8, 2))
  @azimuth.assert_eq(32, @azimuth.multiply(16, 2))
  @azimuth.assert_eq(64, @azimuth.multiply(32, 2))
  @azimuth.assert_eq(128, @azimuth.multiply(64, 2))
}

test "greet_with_special_names" {
  // 测试特殊名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet("Charlie"))
}

test "add_temperature_conversion" {
  // 测试温度转换计算
  // 摄氏度转华氏度: F = C * 9/5 + 32，简化为整数运算
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(68, fahrenheit)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9，简化为整数运算
  let fahrenheit2 = 100
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  @azimuth.assert_eq(37, celsius2)
}

test "multiply_area_calculation" {
  // 测试面积计算
  // 正方形面积: 边长 × 边长
  let side = 7
  let square_area = @azimuth.multiply(side, side)
  @azimuth.assert_eq(49, square_area)
  
  // 三角形面积: 底 × 高 ÷ 2，简化为整数运算
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}