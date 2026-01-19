// 新的标准 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：折扣计算
  let original_price = 1000
  let discount_rate = 20  // 20% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算折后价格
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算含税价格
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(864, final_price)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算矩形面积
  let length = 12
  let width = 8
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "sequential_operations" {
  // 测试连续运算
  // (a + b) × c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "VeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, VeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
}