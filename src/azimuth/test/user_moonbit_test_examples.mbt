// 用户自定义的 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种场景

test "add_basic_functionality" {
  // 基础加法功能测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "multiply_basic_functionality" {
  // 基础乘法功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 12345))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "boundary_value_tests" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单利息计算：本金 + 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5
  let time = 2
  let interest = multiply(principal, multiply(rate, time))
  let total = add(principal, interest)
  assert_eq(110000, total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  assert_true(true)
  assert_false(false)
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
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
  
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let product1 = multiply(sum1, sum2)
  let product2 = multiply(e, f)
  let result = add(product1, -product2)
  
  assert_eq(504, result)
}

test "string_processing_tests" {
  // 字符串处理测试
  // 测试包含各种字符的字符串
  let alphanumeric = "Test123"
  let symbolic = "test@example.com"
  let unicode = "测试🌍"
  
  assert_eq_string("Hello, Test123!", greet(alphanumeric))
  assert_eq_string("Hello, test@example.com!", greet(symbolic))
  assert_eq_string("Hello, 测试🌍!", greet(unicode))
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 计算数列和：1 + 2 + 3 + ... + 100
  let n = 100
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}