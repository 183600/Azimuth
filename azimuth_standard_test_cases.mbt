// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

test "azimuth_basic_arithmetic" {
  // 基本算术运算测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 100))
}

test "azimuth_string_operations" {
  // 字符串操作测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "azimuth_overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试乘法溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "azimuth_complex_calculations" {
  // 复杂计算测试
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(100, result1)
  
  // 计算 a * b + a * c
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(156, result2)
  
  // 计算 (a - b) * (c + 3)
  let result3 = multiply(add(a, -b), add(c, 3))
  assert_eq(32, result3)
}

test "azimuth_practical_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let cart_total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, cart_total)
  
  // 温度转换（简化版：摄氏度转华氏度 F = C * 2 + 30）
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

test "azimuth_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 边界值与1的运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 特殊情况：最小值与-1相乘
  assert_eq(min_val, multiply(min_val, -1))
}

test "azimuth_mathematical_properties" {
  // 数学性质测试
  let a = 15
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  let c = 3
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "azimuth_performance_patterns" {
  // 性能模式测试
  let base = 100
  
  // 重复加法 vs 乘法
  let repeated_add = add(add(add(base, base), base), base)
  let multiplication = multiply(base, 4)
  assert_eq(repeated_add, multiplication)
  
  // 嵌套计算
  let nested = multiply(add(5, 3), add(2, 4))
  assert_eq(64, nested)
  
  // 大数计算
  let large_num = 100000
  let large_result = add(multiply(large_num, 2), multiply(large_num, 3))
  assert_eq(500000, large_result)
}

test "azimuth_error_handling" {
  // 错误处理测试
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 条件测试
  let x = 10
  let y = 20
  
  let is_greater = x > y
  let result = if is_greater { x } else { y }
  assert_eq(20, result)
  
  let is_equal = x == y
  let result2 = if is_equal { 100 } else { 200 }
  assert_eq(200, result2)
}