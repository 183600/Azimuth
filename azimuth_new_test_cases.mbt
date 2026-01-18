// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖多种场景

test "arithmeti_operations" {
  // 测试基本算术运算组合
  let a = 15
  let b = 7
  let c = 3
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(66, result)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(108, result2)
}

test "negative_number_operations" {
  // 测试负数运算
  let neg_a = -10
  let neg_b = -5
  let pos_c = 8
  
  // 负数加法
  let neg_sum = @azimuth.add(neg_a, neg_b)
  @azimuth.assert_eq(-15, neg_sum)
  
  // 正负数乘法
  let mixed_product = @azimuth.multiply(neg_a, pos_c)
  @azimuth.assert_eq(-80, mixed_product)
  
  // 负数乘法
  let neg_product = @azimuth.multiply(neg_a, neg_b)
  @azimuth.assert_eq(50, neg_product)
}

test "zero_operations" {
  // 测试零值运算
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 与零的加法
  @azimuth.assert_eq(positive, @azimuth.add(positive, zero))
  @azimuth.assert_eq(negative, @azimuth.add(negative, zero))
  
  // 与零的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(negative, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(zero, zero))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  let medium_num = 50000
  let small_num = 123
  
  // 大数加法
  let large_sum = @azimuth.add(large_num, medium_num)
  @azimuth.assert_eq(1050000, large_sum)
  
  // 大数乘法（注意不要溢出）
  let controlled_product = @azimuth.multiply(1000, 2000)
  @azimuth.assert_eq(2000000, controlled_product)
  
  // 混合运算
  let mixed_result = @azimuth.add(@azimuth.multiply(100, 100), small_num)
  @azimuth.assert_eq(10123, mixed_result)
}

test "string_greeting_variations" {
  // 测试各种字符串问候场景
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test_user@domain.com!", @azimuth.greet("test_user@domain.com"))
}

test "mathematical_formulas" {
  // 测试数学公式计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  let perimeter = @azimuth.add(@azimuth.add(length, width), @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算立方体体积
  let side = 5
  let volume = @azimuth.multiply(@azimuth.multiply(side, side), side)
  @azimuth.assert_eq(125, volume)
}

test "business_calculations" {
  // 测试商业计算场景
  // 计算总价（单价 × 数量）
  let unit_price = 25
  let quantity = 8
  let subtotal = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, subtotal)
  
  // 应用折扣
  let discount_rate = 10  // 10%
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  @azimuth.assert_eq(180, discounted_total)
  
  // 加税
  let tax_rate = 5  // 5%
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  let final_total = @azimuth.add(discounted_total, tax_amount)
  @azimuth.assert_eq(189, final_total)
}

test "sequence_calculations" {
  // 测试序列计算
  // 计算等差数列和: 1 + 2 + 3 + 4 + 5
  let n = 5
  let first = 1
  let last = 5
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(15, arithmetic_sum)
  
  // 计算等比数列和: 2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
  
  // 计算平方和: 1² + 2² + 3² + 4²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 4), 9), 16)
  @azimuth.assert_eq(30, square_sum)
}

test "temperature_conversions" {
  // 测试温度转换计算
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度: C = (F - 32) * 5/9
  let fahrenheit2 = 212  // 沸点
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  @azimuth.assert_eq(100, celsius2)
  
  // 温度差计算
  let temp_diff = @azimuth.add(fahrenheit, -fahrenheit2)
  @azimuth.assert_eq(-135, temp_diff)
}

test "assertion_validations" {
  // 测试各种断言验证
  // 基本相等断言
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-17, -17)
  
  // 字符串相等断言
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  
  // 布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 计算结果断言
  let calc_result = @azimuth.add(@azimuth.multiply(3, 4), 5)
  @azimuth.assert_eq(17, calc_result)
}