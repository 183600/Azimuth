// 综合 MoonBit 测试套件
// 测试 azimuth 模块的核心功能，包含多样化的测试场景

test "integer_addition_comprehensive" {
  // 测试整数加法的各种情况
  // 正数相加
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  
  // 负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  // 正负数相加
  @azimuth.assert_eq(2, @azimuth.add(10, -8))
  
  // 与零相加
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "integer_multiplication_comprehensive" {
  // 测试整数乘法的各种情况
  // 正数相乘
  @azimuth.assert_eq(24, @azimuth.multiply(4, 6))
  
  // 负数相乘
  @azimuth.assert_eq(35, @azimuth.multiply(-5, -7))
  
  // 正负数相乘
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
  
  // 与零和一相乘
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_greeting_variations" {
  // 测试字符串问候功能的各种变体
  // 基本问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
  
  // 包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  
  // 包含emoji的字符串
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "mathematical_properties_validation" {
  // 测试数学性质的验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let mul_left_assoc = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mul_right_assoc = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mul_left_assoc, mul_right_assoc)
  
  // 分配律：a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 简单利息计算：本金 + (本金 × 利率 × 时间)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + (10000 * 5 * 3) = 160000
  
  // 折扣计算：原价 - (原价 × 折扣百分比)
  let original_price = 500
  let discount_percent = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(400, discounted_price)  // 500 - (500 * 20 / 100) = 400
  
  // 税费计算：价格 + (价格 × 税率)
  let price = 200
  let tax_rate = 8  // 8%
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  let final_price = @azimuth.add(price, tax_amount)
  @azimuth.assert_eq(216, final_price)  // 200 + (200 * 8 / 100) = 216
}

test "geometry_calculations" {
  // 测试几何计算
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 三角形面积（简化）：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
  
  // 圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let radius = 5
  let circumference = @azimuth.multiply(2, @azimuth.multiply(3, radius))
  @azimuth.assert_eq(30, circumference)
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  
  // 冰点温度
  let celsius_freezing = 0
  let fahrenheit_freezing = @azimuth.add(@azimuth.multiply(celsius_freezing, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_freezing)
  
  // 室温
  let celsius_room = 20
  let fahrenheit_room = @azimuth.add(@azimuth.multiply(celsius_room, 2), 30)
  @azimuth.assert_eq(70, fahrenheit_room)
  
  // 体温
  let celsius_body = 37
  let fahrenheit_body = @azimuth.add(@azimuth.multiply(celsius_body, 2), 30)
  @azimuth.assert_eq(104, fahrenheit_body)
  
  // 沸点温度
  let celsius_boiling = 100
  let fahrenheit_boiling = @azimuth.add(@azimuth.multiply(celsius_boiling, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_boiling)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  // 订单总价计算：商品价格 × 数量 + 运费 - 折扣
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(@azimuth.add(subtotal, shipping), -discount)
  @azimuth.assert_eq(627, total)  // 199*3 + 20 - 50 = 597 + 20 - 50 = 627
  
  // 库存管理：初始库存 + 进货 - 销货 + 退货 - 损耗
  let initial_stock = 100
  let received = 30
  let sold = 45
  let returned = 5
  let damaged = 3
  
  let current_stock = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(initial_stock, received), -sold), returned), -damaged)
  @azimuth.assert_eq(87, current_stock)  // 100 + 30 - 45 + 5 - 3 = 87
  
  // 利润计算：收入 - 成本
  let revenue = 1500
  let cost = 900
  let profit = @azimuth.add(revenue, -cost)
  @azimuth.assert_eq(600, profit)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算总和：1 + 2 + 3 + 4 + 5
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum)
  
  // 计算平均值：总和 / 数量
  let average = sum / 5
  @azimuth.assert_eq(3, average)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
  
  // 计算方差（简化）：平方和 / 数量 - 平均值²
  let variance = square_sum / 5 - @azimuth.multiply(average, average)
  @azimuth.assert_eq(2, variance)  // 55/5 - 3² = 11 - 9 = 2
}

test "assert_functions_validation" {
  // 测试断言函数的有效性
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-10 < 0)
  
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(-10 > 0)
  
  // 测试 assert_eq
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-10, -10)
  @azimuth.assert_eq(0, 0)
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Test123", "Test123")
  
  // 测试复合条件
  let x = 10
  let y = 20
  @azimuth.assert_true(@azimuth.add(x, y) == 30)
  @azimuth.assert_true(@azimuth.multiply(x, y) == 200)
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}