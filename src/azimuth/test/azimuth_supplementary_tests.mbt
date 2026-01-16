// Azimuth 项目补充测试用例
// 使用标准 MoonBit 测试语法

test "add_zero_identity" {
  // 测试加法零元特性
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
}

test "multiply_zero_property" {
  // 测试乘法零元特性
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(15, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 15))
  @azimuth.assert_eq(0, @azimuth.multiply(-23, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -23))
}

test "multiply_identity_property" {
  // 测试乘法单位元特性
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-12, @azimuth.multiply(-12, 1))
  @azimuth.assert_eq(-12, @azimuth.multiply(1, -12))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(12, @azimuth.add(20, -8))
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  @azimuth.assert_eq(-18, @azimuth.multiply(9, -2))
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  @azimuth.assert_eq_string("Hello, @user!", @azimuth.greet("@user"))
  @azimuth.assert_eq_string("Hello, user_name!", @azimuth.greet("user_name"))
  @azimuth.assert_eq_string("Hello, user-123!", @azimuth.greet("user-123"))
  @azimuth.assert_eq_string("Hello, user@email.com!", @azimuth.greet("user@email.com"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  
  // 平方差公式: (a+b)(a-b) = a²-b²
  let a = 10
  let b = 3
  let left_side = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, -b))
  let right_side = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(b, -b))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证具体值: (10+3)(10-3) = 10²-3² = 100-9 = 91
  @azimuth.assert_eq(91, left_side)
}

test "financial_calculations" {
  // 测试金融计算场景
  
  // 计算折扣后价格: 原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20% 折扣
  let discount_amount = @azimuth.multiply(original_price, @azimuth.multiply(discount_rate, -1))
  let final_price = @azimuth.add(original_price, discount_amount)
  @azimuth.assert_eq(160, final_price)  // 200 - 40 = 160
  
  // 计算税收: 价格 × 税率
  let price = 150
  let tax_rate = 8  // 8% 税率
  let tax_amount = @azimuth.multiply(price, tax_rate)
  @azimuth.assert_eq(1200, tax_amount)  // 150 × 8 = 1200 (代表 12.00)
}

test "measurement_conversions" {
  // 测试单位转换计算
  
  // 厘米转毫米: 厘米数 × 10
  let cm = 15
  let mm = @azimuth.multiply(cm, 10)
  @azimuth.assert_eq(150, mm)
  
  // 分钟转秒: 分钟数 × 60
  let minutes = 5
  let seconds = @azimuth.multiply(minutes, 60)
  @azimuth.assert_eq(300, seconds)
  
  // 计算总秒数: 小时×3600 + 分钟×60 + 秒
  let hours = 2
  let mins = 30
  let secs = 45
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.add(@azimuth.multiply(mins, 60), secs))
  @azimuth.assert_eq(9045, total_seconds)  // 2×3600 + 30×60 + 45 = 7200 + 1800 + 45 = 9045
}

test "string_length_operations" {
  // 测试字符串长度相关的问候
  
  // 单字符名称
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长名称
  let long_name = "VeryLongNameWithManyCharacters"
  @azimuth.assert_eq_string("Hello, VeryLongNameWithManyCharacters!", @azimuth.greet(long_name))
  
  // 包含数字的名称
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 包含空格的名称
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑计算
  
  // 计算订单总价: (商品1数量 × 单价1) + (商品2数量 × 单价2) - 折扣
  let item1_qty = 3
  let item1_price = 25
  let item2_qty = 2
  let item2_price = 40
  let discount = 20
  
  let item1_total = @azimuth.multiply(item1_qty, item1_price)
  let item2_total = @azimuth.multiply(item2_qty, item2_price)
  let subtotal = @azimuth.add(item1_total, item2_total)
  let final_total = @azimuth.add(subtotal, -discount)
  
  // 验证: (3×25) + (2×40) - 20 = 75 + 80 - 20 = 135
  @azimuth.assert_eq(135, final_total)
  
  // 计算平均价格: 总价 ÷ 商品总数
  let total_items = @azimuth.add(item1_qty, item2_qty)
  // 注意: 这里我们用乘法模拟除法，实际项目中可能需要真正的除法函数
  // 这里我们只是验证计算逻辑
  @azimuth.assert_eq(5, total_items)  // 3 + 2 = 5 件商品
}