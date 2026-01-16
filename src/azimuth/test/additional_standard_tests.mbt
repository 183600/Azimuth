// 额外的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiplication_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(2, @azimuth.multiply(1, 2))
  @azimuth.assert_eq(4, @azimuth.multiply(2, 2))
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(256, @azimuth.multiply(16, 16))
}

test "greet_function_with_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, Single!", @azimuth.greet("Single"))
  @azimuth.assert_eq_string("Hello, Multiple Words!", @azimuth.greet("Multiple Words"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, @#%&!", @azimuth.greet("@#%&"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "mathematical_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "boundary_value_combinations" {
  // 测试边界值的组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 (1 + 2) × (3 + 4) - 5
  let result = @azimuth.add(
    @azimuth.multiply(
      @azimuth.add(1, 2),
      @azimuth.add(3, 4)
    ),
    -5
  )
  @azimuth.assert_eq(16, result)
  
  // 计算 10 × 5 + 20 × 3
  let result2 = @azimuth.add(
    @azimuth.multiply(10, 5),
    @azimuth.multiply(20, 3)
  )
  @azimuth.assert_eq(110, result2)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-7, 11))
  @azimuth.assert_eq(-10, @azimuth.multiply(-2, 5))
  @azimuth.assert_eq(10, @azimuth.multiply(-2, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, newline\n!", @azimuth.greet("newline\n"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算平均值：(10 + 20 + 30) / 3
  let sum = @azimuth.add(@azimuth.add(10, 20), 30)
  let average = sum / 3
  @azimuth.assert_eq(20, average)
  
  // 计算平方和：1² + 2² + 3²
  let square_sum = @azimuth.add(
    @azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)),
    @azimuth.multiply(3, 3)
  )
  @azimuth.assert_eq(14, square_sum)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 圆周长计算（简化）：2 × π × r ≈ 2 × 3 × r
  let radius = 5
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 温度转换：摄氏度转华氏度（简化）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}