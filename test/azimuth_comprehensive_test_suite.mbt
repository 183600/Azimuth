// Azimuth 综合测试套件
// 测试核心功能的各种场景和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(-2, add(5, -7))
  assert_eq(0, add(0, 0))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 123))
}

test "overflow_boundary_conditions" {
  // 溢出边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 乘法溢出测试
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(46341, 46341))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试123!", greet("测试123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let cart_total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, cart_total)
  
  // 温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(16000, total)
}

test "edge_case_combinations" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 特殊值处理
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
}

test "complex_calculation_chains" {
  // 复杂计算链测试
  // ((a + b) × c) - (d × e)
  let a = 10
  let b = 15
  let c = 4
  let d = 8
  let e = 6
  
  let step1 = add(a, b)  // 25
  let step2 = multiply(step1, c)  // 100
  let step3 = multiply(d, e)  // 48
  let result = add(step2, -step3)  // 52
  
  assert_eq(52, result)
  
  // 嵌套计算：a × (b + (c × d))
  let f = 3
  let g = 4
  let h = 5
  let i = 2
  
  let inner = multiply(h, i)  // 10
  let middle = add(g, inner)  // 14
  let nested_result = multiply(f, middle)  // 42
  
  assert_eq(42, nested_result)
}

test "statistical_calculations" {
  // 统计计算测试
  // 计算平均值
  let numbers_sum = add(add(add(add(85, 90), 78), 92), 80)
  let average = numbers_sum / 5
  assert_eq(85, average)
  
  // 计算总和（前n个自然数）
  let n = 10
  let sum_n = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum_n)
  
  // 计算平方和（前5个自然数）
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}

test "error_handling_validation" {
  // 错误处理验证测试
  // 验证断言函数正常工作
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("test", "test")
  
  // 验证溢出保护正确工作
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 验证正常计算
  assert_true(add(10, 20) == 30)
  assert_true(multiply(6, 7) == 42)
  assert_true(greet("Test") == "Hello, Test!")
}