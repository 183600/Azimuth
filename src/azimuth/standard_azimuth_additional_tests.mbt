// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含对核心功能的全面测试

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试边界组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试负溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊边界情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的各种情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长 (2 * (length + width))
  let length = 8
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
  
  // 场景2：计算折扣价格 (price * (1 - discount_rate))
  let price = 200
  let discount_rate = 20  // 20%
  let discounted_price = @azimuth.multiply(price, @azimuth.add(100, -discount_rate))
  @azimuth.assert_eq(16000, discounted_price)  // 200 * 80 = 16000
  
  // 场景3：计算复利 (principal * (1 + rate * years))
  let principal = 1000
  let rate = 5
  let years = 3
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(16000, total)  // 1000 * (1 + 5*3) = 16000
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "edge_cases_and_special_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值运算
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试单位值运算
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(1 == 0)
  
  // 测试 assert_eq 和 assert_eq_string
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  @azimuth.assert_true(@azimuth.add(x, y) == 30)
  @azimuth.assert_true(@azimuth.multiply(x, y) == 200)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 15
  
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(712, total)  // (199*2) + (299*1) + 15 = 398 + 299 + 15 = 712
  
  // 场景2：温度转换（摄氏度转华氏度，简化版：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 场景3：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average = sum / 4
  @azimuth.assert_eq(87, average)  // (85+90+95+80)/4 = 350/4 = 87
}