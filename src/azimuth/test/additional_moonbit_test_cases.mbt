// Azimuth 项目额外的 MoonBit 测试用例
// 包含各种场景的测试，使用标准的 MoonBit 测试语法

test "integer_division_simulation" {
  // 模拟整数除法运算
  let dividend = 17
  let divisor = 5
  
  // 模拟整数除法：17 / 5 = 3 余 2
  let quotient = dividend / divisor
  let remainder = dividend - (divisor * quotient)
  
  @azimuth.assert_eq(3, quotient)
  @azimuth.assert_eq(2, remainder)
  
  // 测试边界情况
  @azimuth.assert_eq(0, 0 / 5)
  @azimuth.assert_eq(5, 25 / 5)
}

test "absolute_value_calculation" {
  // 模拟绝对值计算
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 正数的绝对值是本身
  @azimuth.assert_eq(positive, positive)
  
  // 负数的绝对值是其相反数
  @azimuth.assert_eq(42, -negative)
  
  // 零的绝对值是零
  @azimuth.assert_eq(0, zero)
}

test "string_length_analysis" {
  // 分析字符串长度特性
  let empty = ""
  let single = "A"
  let short = "Hello"
  let long = "This is a longer string for testing purposes"
  
  // 测试不同长度字符串的greet函数处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single))
  @azimuth.assert_eq_string("Hello, Hello!", @azimuth.greet(short))
  @azimuth.assert_eq_string("Hello, This is a longer string for testing purposes!", @azimuth.greet(long))
}

test "range_validation" {
  // 数值范围验证测试
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 测试极值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试中间值
  let mid_val = 0
  @azimuth.assert_eq(mid_val, @azimuth.add(min_val, max_val))
}

test "scientific_notation_simulation" {
  // 模拟科学计数法计算
  // 计算 1.23 × 10^6，使用整数模拟
  let mantissa = 123
  let exponent = 4  // 表示 10^4
  let scale = 10000  // 10^4
  
  let result = @azimuth.multiply(mantissa, scale)
  @azimuth.assert_eq(1230000, result)
  
  // 测试另一个例子：2.5 × 10^3
  let mantissa2 = 25
  let exponent2 = 2
  let scale2 = 100
  
  let result2 = @azimuth.multiply(mantissa2, scale2)
  @azimuth.assert_eq(2500, result2)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage = 25  // 25%
  
  // 计算百分比值：总数 × 百分比 / 100
  let value = @azimuth.multiply(total, percentage) / 100
  @azimuth.assert_eq(50, value)
  
  // 计算增加后的值：总数 + 百分比值
  let increased = @azimuth.add(total, value)
  @azimuth.assert_eq(250, increased)
  
  // 计算减少后的值：总数 - 百分比值
  let decreased = @azimuth.add(total, -value)
  @azimuth.assert_eq(150, decreased)
}

test "temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30
  
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "geometric_calculations" {
  // 几何计算测试
  let radius = 7
  
  // 圆周长（简化）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
  
  // 球体体积（简化）：4/3 × π × r³ ≈ 4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(1372, volume)
}

test "number_system_conversion" {
  // 数制转换测试（简化版）
  // 十进制转二进制（模拟）
  let decimal = 13
  
  // 模拟二进制转换：13 = 1101（二进制）
  // 通过位运算模拟：8 + 4 + 0 + 1 = 13
  let binary_value = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.multiply(1, 1))
  @azimuth.assert_eq(decimal, binary_value)
  
  // 十进制转十六进制（模拟）
  let decimal2 = 255
  
  // 模拟十六进制转换：255 = FF（十六进制）
  // 通过计算：15 × 16 + 15 = 255
  let hex_value = @azimuth.add(@azimuth.multiply(15, 16), 15)
  @azimuth.assert_eq(decimal2, hex_value)
}

test "comprehensive_business_calculation" {
  // 综合业务计算测试
  let unit_price = 99
  let quantity = 5
  let discount_threshold = 400
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping)
  
  @azimuth.assert_eq(530, final_total)
}