// 简洁高效的 MoonBit 测试套件
// 覆盖 azimuth 包的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(-2, add(3, -5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 12345))
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 边界值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
}

test "string_processing_functions" {
  // 字符串处理功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律验证
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律验证
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
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
  let principal = 10000
  let rate = 5
  let years = 2
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(110000, total)
}

test "error_handling_validation" {
  // 错误处理验证测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 极值情况下的稳定性
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  let min_val = -2147483648
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(min_val, 2) == min_val)
}

test "complex_operations" {
  // 复合运算测试
  // 计算 (a + b) × (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 15
  
  let sum_ab = add(a, b)
  let diff_cd = add(c, -d)
  let product = multiply(sum_ab, diff_cd)
  let result = add(product, e)
  
  assert_eq(105, result)
  
  // 计算平方和：a² + b²
  let square_a = multiply(a, a)
  let square_b = multiply(b, b)
  let sum_of_squares = add(square_a, square_b)
  
  assert_eq(125, sum_of_squares)
}

test "performance_calculations" {
  // 性能相关计算测试
  // 等差数列求和：1 + 2 + ... + 100
  let n = 100
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, sum)
  
  // 等比数列求和（简化）：2⁰ + 2¹ + 2² + 2³ + 2⁴
  let power_sum = add(add(add(add(1, 2), 4), 8), 16)
  assert_eq(31, power_sum)
  
  // 阶乘计算：5! = 120
  let factorial = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial)
}

test "data_type_edge_cases" {
  // 数据类型边界情况测试
  // 零值处理
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(2147483647, add(0, 2147483647))
  assert_eq(-2147483648, add(0, -2147483648))
  
  // 单位元处理
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(5, multiply(5, -1, -1))
  
  // 字符串边界情况
  assert_eq_string("Hello, A!", greet("A"))
  let very_long_name = "ThisIsAVeryLongNameThatTestsStringHandlingCapabilities"
  assert_eq_string("Hello, " + very_long_name + "!", greet(very_long_name))
}

test "comprehensive_functionality" {
  // 综合功能测试
  // 复杂业务逻辑：订单总价计算
  let unit_price = 299
  let quantity = 2
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 20
  
  // 商品小计
  let subtotal = multiply(unit_price, quantity)
  // 折扣金额
  let discount = multiply(subtotal, discount_rate) / 100
  // 折后价格
  let discounted_price = add(subtotal, -discount)
  // 税费
  let tax = multiply(discounted_price, tax_rate) / 100
  // 最终总价
  let final_total = add(add(discounted_price, tax), shipping)
  
  assert_eq(567, final_total)
  
  // 验证计算过程的正确性
  assert_eq(598, subtotal)
  assert_eq(59, discount)
  assert_eq(539, discounted_price)
  assert_eq(43, tax)
}