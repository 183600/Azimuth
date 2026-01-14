// 新的 MoonBit 测试用例
// 这些测试用例专注于验证 azimuth 库的核心功能和边界情况

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：1 + 4 + 7 + ... + 28（前10项，公差为3）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 28
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(145, sum)  // 10 × (1 + 28) / 2 = 145
}

test "geometric_sequence_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16（前4项，公比为2）
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)
  let term3 = @azimuth.multiply(term2, 2)
  let term4 = @azimuth.multiply(term3, 2)
  
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(term1, term2), term3), term4)
  @azimuth.assert_eq(1024, product)  // 2 × 4 × 8 × 16 = 1024
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30（近似值）
  let celsius_temps = @azimuth.add(0, 0)  // 0°C
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temps, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_temp)  // 0°C ≈ 30°F（简化计算）
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100°C ≈ 230°F（简化计算）
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time)
  @azimuth.assert_eq(150000, interest)  // 10000 × 5 × 3 = 150000
  
  // 计算本息和
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 150000 = 160000
}

test "perimeter_and_area_calculations" {
  // 计算矩形的周长和面积
  let length = 15
  let width = 8
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)  // 2 × (15 + 8) = 46
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)  // 15 × 8 = 120
  
  // 计算正方形的周长和面积
  let side = 10
  let square_perimeter = @azimuth.multiply(4, side)
  let square_area = @azimuth.multiply(side, side)
  
  @azimuth.assert_eq(40, square_perimeter)  // 4 × 10 = 40
  @azimuth.assert_eq(100, square_area)      // 10 × 10 = 100
}

test "string_greeting_variations" {
  // 测试各种问候语变体
  // 测试常见英文名
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  
  // 测试中文名
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 李四!", @azimuth.greet("李四"))
  
  // 测试包含空格的名字
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, Mary Jane!", @azimuth.greet("Mary Jane"))
  
  // 测试数字和字母混合
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, Test2024!", @azimuth.greet("Test2024"))
}

test "boundary_value_extremes" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))  // 溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))  // 溢出保护
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊处理
  
  // 测试极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 7
  let c = 5
  
  // 验证加法交换律：a + b = b + a
  let add_ab = @azimuth.add(a, b)
  let add_ba = @azimuth.add(b, a)
  @azimuth.assert_eq(add_ab, add_ba)
  
  // 验证乘法交换律：a × b = b × a
  let mul_ab = @azimuth.multiply(a, b)
  let mul_ba = @azimuth.multiply(b, a)
  @azimuth.assert_eq(mul_ab, mul_ba)
  
  // 验证加法结合律：(a + b) + c = a + (b + c)
  let add_abc_left = @azimuth.add(@azimuth.add(a, b), c)
  let add_abc_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(add_abc_left, add_abc_right)
  
  // 验证乘法结合律：(a × b) × c = a × (b × c)
  let mul_abc_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mul_abc_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mul_abc_left, mul_abc_right)
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount_threshold = 1000
  let discount_rate = 10
  let shipping_fee = 20
  
  // 计算每种商品的小计
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(discounted_total, shipping_fee)
  
  // 验证计算结果：299×2 + 199×3 = 598 + 597 = 1195
  // 折扣：1195 × 10% = 119.5 ≈ 119（整数除法）
  // 最终：1195 - 119 + 20 = 1096
  @azimuth.assert_eq(1096, final_total)
}

test "number_system_conversions" {
  // 数字系统转换测试（简化版）
  // 二进制转十进制：1010 = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let binary_1010 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(0, 4)), 
                                 @azimuth.add(@azimuth.multiply(1, 2), @azimuth.multiply(0, 1)))
  @azimuth.assert_eq(10, binary_1010)
  
  // 十进制转二进制验证：13 = 1101（二进制）
  // 1101 = 1×2³ + 1×2² + 0×2¹ + 1×2⁰ = 8 + 4 + 0 + 1 = 13
  let decimal_13 = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), 
                                @azimuth.add(@azimuth.multiply(0, 2), @azimuth.multiply(1, 1)))
  @azimuth.assert_eq(13, decimal_13)
  
  // 十六进制转十进制：1A = 1×16 + 10 = 26
  let hex_1A = @azimuth.add(@azimuth.multiply(1, 16), 10)
  @azimuth.assert_eq(26, hex_1A)
}