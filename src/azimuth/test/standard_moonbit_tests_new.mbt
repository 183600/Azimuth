// 标准 MoonBit 测试用例
// 包含10个测试用例，覆盖基本数学运算、字符串处理和边界条件

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  let c = 5
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "boundary_value_testing" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))  // 溢出保护
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))  // 溢出保护
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculations" {
  // 测试复杂计算
  // 场景：计算矩形的面积和周长
  let length = 10
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(32, perimeter)
  
  // 计算对角线平方（不使用平方根）
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(136, diagonal_squared)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景：温度转换（摄氏度转华氏度，简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let total = @azimuth.add(@azimuth.multiply(item_price, quantity), shipping)
  @azimuth.assert_eq(612, total)
  
  // 场景：简单利息计算
  let principal = 1000
  let rate = 5
  let years = 2
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(11000, final_amount)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", @azimuth.greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet(spaced_string))
  
  // 测试包含数字和字母的混合字符串
  let mixed_string = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(mixed_string))
}

test "mathematical_formulas" {
  // 测试数学公式
  // 场景：计算等差数列的和（前n个自然数）
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 场景：计算等比数列的和（简化版：2^0 + 2^1 + 2^2 + 2^3）
  let term1 = 1
  let term2 = @azimuth.multiply(2, 1)
  let term3 = @azimuth.multiply(2, term2)
  let term4 = @azimuth.multiply(2, term3)
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(15, geometric_sum)
  
  // 场景：计算平方和（1² + 2² + 3²）
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let sum_of_squares = @azimuth.add(@azimuth.add(square1, square2), square3)
  @azimuth.assert_eq(14, sum_of_squares)
}

test "performance_and_optimization" {
  // 测试性能和优化场景
  // 场景：批量计算
  let base = 100
  let multiplier = 5
  let count = 10
  
  // 使用循环思维计算：base + base*multiplier + ... (简化为前3项)
  let term1 = base
  let term2 = @azimuth.multiply(base, multiplier)
  let term3 = @azimuth.multiply(term2, multiplier)
  let batch_sum = @azimuth.add(@azimuth.add(term1, term2), term3)
  @azimuth.assert_eq(3100, batch_sum)
  
  // 场景：缓存计算结果（模拟）
  let expensive_calc = @azimuth.multiply(12345, 6789)
  let reused_result = @azimuth.add(expensive_calc, 1000)
  @azimuth.assert_eq(@azimuth.add(83810205, 1000), reused_result)
  
  // 场景：条件计算
  let x = 15
  let y = 20
  let condition = x < y
  let result = if condition { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result)
}