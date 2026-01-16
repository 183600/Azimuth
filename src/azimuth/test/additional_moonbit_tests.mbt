// 额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "addition_mathematical_properties" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let c = 54321
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试加法单位元：a + 0 = a
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(b, @azimuth.add(0, b))
}

test "multiplication_mathematical_properties" {
  // 测试乘法交换律：a × b = b × a
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试乘法结合律：(a × b) × c = a × (b × c)
  let c = 789
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试乘法单位元：a × 1 = a
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  
  // 测试乘法零元：a × 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
}

test "string_processing_edge_cases" {
  // 测试空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
}

test "shopping_cart_calculation" {
  // 测试购物车计算场景
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 20
  let discount = 50
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)
  
  // 应用折扣
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 添加运费
  let final_total = @azimuth.add(discounted_total, shipping_fee)
  
  // 验证最终总价：299*2 + 199*3 - 50 + 20 = 598 + 597 - 50 + 20 = 1165
  @azimuth.assert_eq(1165, final_total)
}

test "area_and_perimeter_calculations" {
  // 测试几何计算：面积和周长
  let length = 12
  let width = 8
  
  // 计算矩形面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.divide_with_ceil(@azimuth.multiply(base, height), 2)
  @azimuth.assert_eq(30, triangle_area)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 8
  let trapezoid_area = @azimuth.divide_with_ceil(
    @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height), 
    2
  )
  @azimuth.assert_eq(80, trapezoid_area)
}

test "numeric_range_validation" {
  // 测试正数范围验证
  let positive_sum = @azimuth.add(1000000, 2000000)
  @azimuth.assert_true(positive_sum > 0)
  
  let positive_product = @azimuth.multiply(1000, 1000)
  @azimuth.assert_true(positive_product > 0)
  
  // 测试负数范围验证
  let negative_sum = @azimuth.add(-1000000, -2000000)
  @azimuth.assert_true(negative_sum < 0)
  
  let negative_product = @azimuth.multiply(-1000, 1000)
  @azimuth.assert_true(negative_product < 0)
  
  // 测试零值验证
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456789))
  @azimuth.assert_eq(123456789, @azimuth.add(0, 123456789))
  
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "conditional_logic_scenarios" {
  // 测试基于比较结果的条件运算
  let x = 25
  let y = 30
  let z = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition1 = x < y
  let result1 = if condition1 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(55, result1)  // 25 < 30 为真，所以计算 25 + 30 = 55
  
  // 如果 x > z，则计算 x * z，否则计算 x + z
  let condition2 = x > z
  let result2 = if condition2 { @azimuth.multiply(x, z) } else { @azimuth.add(x, z) }
  @azimuth.assert_eq(500, result2)  // 25 > 20 为真，所以计算 25 * 20 = 500
  
  // 嵌套条件：如果 x < y 且 y > z，则计算 x + y + z
  let condition3 = x < y && y > z
  let result3 = if condition3 { 
    @azimuth.add(@azimuth.add(x, y), z) 
  } else { 
    @azimuth.multiply(x, @azimuth.multiply(y, z)) 
  }
  @azimuth.assert_eq(75, result3)  // 25 < 30 且 30 > 20 为真，所以计算 25 + 30 + 20 = 75
}

test "data_conversion_scenarios" {
  // 测试数值到字符串的间接转换（通过greet函数）
  let number = 42
  let number_str = "42"
  let result1 = @azimuth.greet(number_str)
  @azimuth.assert_eq_string("Hello, 42!", result1)
  
  // 测试布尔值场景（通过字符串表示）
  let true_str = "true"
  let false_str = "false"
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet(true_str))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet(false_str))
  
  // 测试浮点数的简化表示（使用整数表示小数部分）
  let pi_approx_str = "3.14"
  @azimuth.assert_eq_string("Hello, 3.14!", @azimuth.greet(pi_approx_str))
  
  // 测试日期时间字符串
  let date_str = "2023-12-25"
  let time_str = "14:30:00"
  @azimuth.assert_eq_string("Hello, 2023-12-25!", @azimuth.greet(date_str))
  @azimuth.assert_eq_string("Hello, 14:30:00!", @azimuth.greet(time_str))
}

test "error_handling_validation" {
  // 测试断言函数的正常使用
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界情况的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试正常运算结果
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
  
  // 测试特殊值的处理
  @azimuth.assert_true(@azimuth.multiply(0, 0) == 0)
  @azimuth.assert_true(@azimuth.add(0, 0) == 0)
  @azimuth.assert_true(@azimuth.multiply(1, 1) == 1)
  @azimuth.assert_true(@azimuth.add(1, 0) == 1)
}

test "performance_related_calculations" {
  // 测试大量数据的累加计算
  let base_value = 100
  let count = 100
  
  // 使用公式计算：base_value × (1 + 2 + ... + count) = base_value × count × (count + 1) / 2
  let sum_total = @azimuth.multiply(
    base_value, 
    @azimuth.divide_with_ceil(@azimuth.multiply(count, @azimuth.add(count, 1)), 2)
  )
  
  // 验证计算结果：100 × 100 × 101 / 2 = 505000
  @azimuth.assert_eq(505000, sum_total)
  
  // 测试指数增长计算
  let initial_value = 2
  let periods = 8
  
  // 计算：2^8 = 256
  let power_result = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), 
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))
  )
  @azimuth.assert_eq(256, power_result)
  
  // 测试阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial_result = @azimuth.multiply(
    n, 
    @azimuth.multiply(
      n - 1, 
      @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))
    )
  )
  @azimuth.assert_eq(120, factorial_result)
}