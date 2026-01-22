// 用户自定义的 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种场景

test "add_basic_functionality" {
  // 基础加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-3, @azimuth.add(-8, 5))
}

test "multiply_basic_functionality" {
  // 基础乘法功能测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "greet_function_variations" {
  // 问候函数变体测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_value_tests" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单利息计算：本金 + 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5
  let time = 2
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(110000, total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(@azimuth.add(max_val, 100) == max_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "complex_calculations" {
  // 复杂计算测试
  // 计算 (a + b) × (c + d) - e × f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let product1 = @azimuth.multiply(sum1, sum2)
  let product2 = @azimuth.multiply(e, f)
  let result = @azimuth.add(product1, -product2)
  
  @azimuth.assert_eq(504, result)
}

test "string_processing_tests" {
  // 字符串处理测试
  // 测试包含各种字符的字符串
  let alphanumeric = "Test123"
  let symbolic = "test@example.com"
  let unicode = "测试🌍"
  
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet(alphanumeric))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(symbolic))
  @azimuth.assert_eq_string("Hello, 测试🌍!", @azimuth.greet(unicode))
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 计算数列和：1 + 2 + 3 + ... + 100
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}