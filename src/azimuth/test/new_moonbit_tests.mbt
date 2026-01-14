// 新的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = azimuth::@azimuth.multiply(n, azimuth::@azimuth.add(n, 1)) / 2
  azimuth::@azimuth.assert_eq(55, sum)
}

test "geometric_sequence_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  let product = azimuth::@azimuth.multiply(
    azimuth::@azimuth.multiply(term1, term2),
    azimuth::@azimuth.multiply(term3, term4)
  )
  azimuth::@azimuth.assert_eq(1024, product)
}

test "temperature_conversion" {
  // 温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(celsius, 2),
    30
  )
  azimuth::@azimuth.assert_eq(80, fahrenheit)
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = azimuth::@azimuth.multiply(
    principal,
    azimuth::@azimuth.multiply(rate, time)
  )
  azimuth::@azimuth.assert_eq(15000, interest)
}

test "area_perimeter_calculation" {
  // 计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 面积 = 长 × 宽
  let area = azimuth::@azimuth.multiply(length, width)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth::@azimuth.multiply(
    2,
    azimuth::@azimuth.add(length, width)
  )
  
  azimuth::@azimuth.assert_eq(96, area)
  azimuth::@azimuth.assert_eq(40, perimeter)
}

test "greeting_with_special_characters" {
  // 测试包含特殊字符的问候
  let name_with_symbols = "John@Doe#123"
  let greeting = azimuth::@azimuth.greet(name_with_symbols)
  azimuth::@azimuth.assert_eq_string("Hello, John@Doe#123!", greeting)
  
  let name_with_unicode = "张三"
  let greeting_unicode = azimuth::@azimuth.greet(name_with_unicode)
  azimuth::@azimuth.assert_eq_string("Hello, 张三!", greeting_unicode)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算：(a + b) × (c + d) - e × f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = azimuth::@azimuth.add(a, b)
  let sum2 = azimuth::@azimuth.add(c, d)
  let product1 = azimuth::@azimuth.multiply(sum1, sum2)
  let product2 = azimuth::@azimuth.multiply(e, f)
  let result = azimuth::@azimuth.add(product1, azimuth::@azimuth.multiply(product2, -1))
  
  azimuth::@azimuth.assert_eq(504, result)
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  let sum = azimuth::@azimuth.add(max_val, min_val)
  azimuth::@azimuth.assert_eq(-1, sum)
  
  // 最小值与-1相乘
  let product = azimuth::@azimuth.multiply(min_val, -1)
  azimuth::@azimuth.assert_eq(min_val, product)
}

test "string_concatenation_edge_cases" {
  // 字符串连接边界情况
  let empty_string = ""
  let single_char = "A"
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(empty_string))
  azimuth::@azimuth.assert_eq_string("Hello, A!", azimuth::@azimuth.greet(single_char))
  azimuth::@azimuth.assert_eq_string("Hello, ThisIsAVeryLongStringForTestingPurposes!", azimuth::@azimuth.greet(long_string))
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let x = 15
  let y = 20
  
  // 验证交换律：x + y = y + x
  let sum1 = azimuth::@azimuth.add(x, y)
  let sum2 = azimuth::@azimuth.add(y, x)
  azimuth::@azimuth.assert_eq(sum1, sum2)
  
  // 验证交换律：x × y = y × x
  let product1 = azimuth::@azimuth.multiply(x, y)
  let product2 = azimuth::@azimuth.multiply(y, x)
  azimuth::@azimuth.assert_eq(product1, product2)
  
  // 验证分配律：x × (y + z) = x × y + x × z
  let z = 5
  let left_side = azimuth::@azimuth.multiply(x, azimuth::@azimuth.add(y, z))
  let right_side = azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(x, y),
    azimuth::@azimuth.multiply(x, z)
  )
  azimuth::@azimuth.assert_eq(left_side, right_side)
}