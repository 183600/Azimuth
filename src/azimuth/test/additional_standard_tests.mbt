// 额外的标准 MoonBit 测试用例

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(@azimuth.add(1000000, 2000000), 3000000)
  @azimuth.assert_eq(@azimuth.add(-1000000, -2000000), -3000000)
  @azimuth.assert_eq(@azimuth.add(1500000000, 1500000000), 2147483647)
}

test "multiplication_precision_test" {
  // 测试乘法精度
  @azimuth.assert_eq(@azimuth.multiply(12345, 6789), 83810205)
  @azimuth.assert_eq(@azimuth.multiply(-12345, 6789), -83810205)
  @azimuth.assert_eq(@azimuth.multiply(12345, -6789), -83810205)
  @azimuth.assert_eq(@azimuth.multiply(-12345, -6789), 83810205)
}

test "greet_function_with_various_name_formats" {
  // 测试 greet 函数处理不同格式的名字
  @azimuth.assert_eq_string(@azimuth.greet("John Doe"), "Hello, John Doe!")
  @azimuth.assert_eq_string(@azimuth.greet("  LeadingSpace"), "Hello,   LeadingSpace!")
  @azimuth.assert_eq_string(@azimuth.greet("TrailingSpace  "), "Hello, TrailingSpace  !")
  @azimuth.assert_eq_string(@azimuth.greet("Mixed CASE"), "Hello, Mixed CASE!")
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等：a + 0 = a
  @azimuth.assert_eq(@azimuth.add(42, 0), 42)
  @azimuth.assert_eq(@azimuth.add(-42, 0), -42)
  
  // 乘法恒等：a * 1 = a
  @azimuth.assert_eq(@azimuth.multiply(42, 1), 42)
  @azimuth.assert_eq(@azimuth.multiply(-42, 1), -42)
  
  // 乘法零性质：a * 0 = 0
  @azimuth.assert_eq(@azimuth.multiply(42, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(-42, 0), 0)
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算：((a + b) * c) + (d * e)
  let a = 10
  let b = 20
  let c = 5
  let d = 3
  let e = 4
  
  let sum_ab = @azimuth.add(a, b)  // 30
  let product1 = @azimuth.multiply(sum_ab, c)  // 150
  let product2 = @azimuth.multiply(d, e)  // 12
  let result = @azimuth.add(product1, product2)  // 162
  
  @azimuth.assert_eq(result, 162)
}

test "string_length_edge_cases" {
  // 测试字符串长度边界情况
  // 单字符
  @azimuth.assert_eq_string(@azimuth.greet("A"), "Hello, A!")
  
  // 空字符串
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  
  // 包含特殊字符
  @azimuth.assert_eq_string(@azimuth.greet("\n\t"), "Hello, \n\t!")
  
  // Unicode字符
  @azimuth.assert_eq_string(@azimuth.greet("🌟"), "Hello, 🌟!")
}

test "number_system_conversions" {
  // 测试数字系统转换（通过字符串表示）
  // 二进制表示
  @azimuth.assert_eq_string(@azimuth.greet("1010"), "Hello, 1010!")
  
  // 十六进制表示
  @azimuth.assert_eq_string(@azimuth.greet("0xFF"), "Hello, 0xFF!")
  
  // 科学计数法表示
  @azimuth.assert_eq_string(@azimuth.greet("1.23e4"), "Hello, 1.23e4!")
}

test "financial_calculations" {
  // 测试金融计算场景
  // 计算折扣价：原价 * (1 - 折扣率)
  let original_price = 1000
  let discount_rate = 20  // 20%
  
  // 简化计算：1000 * (100 - 20) / 100 = 1000 * 80 / 100 = 800
  let discounted_price = @azimuth.multiply(original_price, 80) / 100
  @azimuth.assert_eq(discounted_price, 800)
  
  // 计算税费：价格 * 税率
  let tax_rate = 8  // 8%
  let tax_amount = @azimuth.multiply(original_price, tax_rate) / 100
  @azimuth.assert_eq(tax_amount, 80)
  
  // 计算总价：价格 + 税费
  let total_price = @azimuth.add(original_price, tax_amount)
  @azimuth.assert_eq(total_price, 1080)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算正方形面积：边长²
  let side_length = 7
  let square_area = @azimuth.multiply(side_length, side_length)
  @azimuth.assert_eq(square_area, 49)
  
  // 计算长方体体积：长 × 宽 × 高
  let length = 5
  let width = 3
  let height = 4
  
  let base_area = @azimuth.multiply(length, width)  // 15
  let volume = @azimuth.multiply(base_area, height)  // 60
  @azimuth.assert_eq(volume, 60)
  
  // 计算梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 4
  let bottom_base = 8
  let trapezoid_height = 5
  
  let sum_bases = @azimuth.add(top_base, bottom_base)  // 12
  let trapezoid_area = @azimuth.multiply(sum_bases, trapezoid_height) / 2  // 30
  @azimuth.assert_eq(trapezoid_area, 30)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(@azimuth.add(max_val, max_val), max_val)
  @azimuth.assert_eq(@azimuth.add(max_val, -max_val), 0)
  @azimuth.assert_eq(@azimuth.multiply(max_val, max_val), max_val)
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(@azimuth.add(min_val, min_val), min_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -min_val), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, min_val), max_val)
  
  // 测试最大值和最小值的混合运算
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
  @azimuth.assert_eq(@azimuth.multiply(max_val, min_val), min_val)
}