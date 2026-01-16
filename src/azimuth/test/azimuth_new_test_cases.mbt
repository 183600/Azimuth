// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

test "add_zero_element" {
  // 测试加法零元素：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_identity_and_zero" {
  // 测试乘法单位元素和零元素
  // 单位元素：a * 1 = a
  @azimuth.assert_eq(25, @azimuth.multiply(25, 1))
  @azimuth.assert_eq(25, @azimuth.multiply(1, 25))
  @azimuth.assert_eq(-13, @azimuth.multiply(-13, 1))
  @azimuth.assert_eq(-13, @azimuth.multiply(1, -13))
  
  // 零元素：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(37, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 37))
  @azimuth.assert_eq(0, @azimuth.multiply(-8, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -8))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-5, -3))
  @azimuth.assert_eq(-7, @azimuth.add(-10, 3))
  @azimuth.assert_eq(-5, @azimuth.add(15, -20))
  @azimuth.assert_eq(-300, @azimuth.add(-100, -200))
  @azimuth.assert_eq(0, @azimuth.add(-50, 50))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  // 负数 × 负数 = 正数
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  
  // 负数 × 正数 = 负数
  @azimuth.assert_eq(-40, @azimuth.multiply(-5, 8))
  @azimuth.assert_eq(-18, @azimuth.multiply(9, -2))
  
  // 零的特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_special_characters" {
  // 测试包含特殊字符的名称
  @azimuth.assert_eq_string("Hello, Alice-Bob!", @azimuth.greet("Alice-Bob"))
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, O'Connor!", @azimuth.greet("O'Connor"))
  @azimuth.assert_eq_string("Hello, Jean-Luc!", @azimuth.greet("Jean-Luc"))
  @azimuth.assert_eq_string("Hello, Mr. Smith!", @azimuth.greet("Mr. Smith"))
}

test "greet_long_names" {
  // 测试长名称
  let long_name = "Alexander Bartholomew Montgomery III"
  @azimuth.assert_eq_string("Hello, Alexander Bartholomew Montgomery III!", @azimuth.greet(long_name))
  
  let very_long_name = "Elizabeth Alexandra Mary Windsor Mountbatten-Windsor"
  @azimuth.assert_eq_string("Hello, Elizabeth Alexandra Mary Windsor Mountbatten-Windsor!", @azimuth.greet(very_long_name))
}

test "mathematical_identities" {
  // 测试数学恒等式
  
  // 平方差公式：(a+b)(a-b) = a²-b²
  let a = 10
  let b = 3
  let left_side = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, -b))
  let right_side = @azimuth.add(@azimuth.multiply(a, a), @azimuth.multiply(-b, b))
  @azimuth.assert_eq(left_side, right_side)
  
  // 分配律：a(b+c) = ab + ac
  @azimuth.assert_eq(@azimuth.multiply(6, @azimuth.add(4, 2)), 
                     @azimuth.add(@azimuth.multiply(6, 4), @azimuth.multiply(6, 2)))
}

test "practical_business_calculations" {
  // 测试实际业务计算场景
  
  // 计算折扣总价：原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20% 折扣
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, final_price)
  
  // 计算总成本：单价 × 数量 + 运费
  let unit_cost = 35
  let quantity = 6
  let shipping = 15
  let subtotal = @azimuth.multiply(unit_cost, quantity)
  let total_cost = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(225, total_cost)
}

test "temperature_conversion" {
  // 测试温度转换计算（使用线性变换）
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  let fahrenheit2 = 68
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  @azimuth.assert_eq(20, celsius2)
}