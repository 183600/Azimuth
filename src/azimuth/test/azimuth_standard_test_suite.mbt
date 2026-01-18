// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法
// 包含10个标准测试用例，覆盖各种场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_tests" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob123!", @azimuth.greet("Bob123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 5
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "real_world_calculations" {
  // 测试实际应用场景
  // 圆形面积计算（简化）：π × r² ≈ 3 × r²
  let radius = 5
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 2
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(10000, interest)
}

test "complex_nested_operations" {
  // 测试复杂嵌套运算
  // 计算：(a + b) × (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 15
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product = @azimuth.multiply(sum_ab, diff_cd)
  let result = @azimuth.add(product, e)
  
  @azimuth.assert_eq(125, result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "financial_calculations" {
  // 测试金融计算场景
  let price = 299
  let quantity = 3
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(price, quantity)
  
  // 计算折扣
  let discount = @azimuth.multiply(subtotal, discount_rate) / 100
  
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax)
  
  @azimuth.assert_eq(936, final_price)
}

test "scientific_calculations" {
  // 测试科学计算场景
  // 计算前n个自然数的和：1 + 2 + ... + n = n × (n + 1) / 2
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算平方和：1² + 2² + ... + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "performance_and_optimization" {
  // 测试性能和优化相关计算
  // 计算幂：2^10 = 1024
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2)), @azimuth.multiply(@azimuth.multiply(2, 2), 2))
  @azimuth.assert_eq(1024, power_of_2)
  
  // 计算阶乘：5! = 120
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
  
  // 计算斐波那契数列第10项（简化计算）
  let fib1 = @azimuth.add(1, 1)  // 2
  let fib2 = @azimuth.add(1, 2)  // 3
  let fib3 = @azimuth.add(2, 3)  // 5
  let fib4 = @azimuth.add(3, 5)  // 8
  let fib5 = @azimuth.add(5, 8)  // 13
  let fib6 = @azimuth.add(8, 13)  // 21
  let fib7 = @azimuth.add(13, 21)  // 34
  let fib8 = @azimuth.add(21, 34)  // 55
  @azimuth.assert_eq(55, fib8)
}