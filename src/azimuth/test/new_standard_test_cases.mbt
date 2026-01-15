// 新增的标准 MoonBit 测试用例
// 覆盖核心功能和边界情况

test "addition_with_various_ranges" {
  // 测试不同数值范围的加法
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(-50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "multiplication_precision_tests" {
  // 测试乘法精度和边界情况
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "mathematical_associative_laws" {
  // 测试数学结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.multiply(a, b), c),
    @azimuth.multiply(a, @azimuth.multiply(b, c))
  )
}

test "calculation_geometric_shapes" {
  // 测试几何图形计算
  // 正方形面积和周长
  let side = 5
  let square_area = @azimuth.multiply(side, side)
  let square_perimeter = @azimuth.multiply(4, side)
  @azimuth.assert_eq(25, square_area)
  @azimuth.assert_eq(20, square_perimeter)
  
  // 三角形面积（底×高÷2）
  let base = 8
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, triangle_area)
}

test "financial_calculation_discount" {
  // 测试财务计算：折扣场景
  let original_price = 1000
  let discount_percentage = 20
  let expected_final_price = 800
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  @azimuth.assert_eq(200, discount_amount)
  
  // 计算最终价格
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(expected_final_price, final_price)
}

test "time_conversion_scenarios" {
  // 测试时间转换场景
  // 小时转分钟
  let hours = 3
  let minutes = @azimuth.multiply(hours, 60)
  @azimuth.assert_eq(180, minutes)
  
  // 天转小时
  let days = 2
  let hours_in_days = @azimuth.multiply(days, 24)
  @azimuth.assert_eq(48, hours_in_days)
  
  // 计算总分钟数（3小时30分钟）
  let total_minutes = @azimuth.add(180, 30)
  @azimuth.assert_eq(210, total_minutes)
}

test "statistical_calculations_basic" {
  // 测试基础统计计算
  // 数据集: 15, 25, 35, 45, 55
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(35, average)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(data5, -data1)
  @azimuth.assert_eq(40, range)
}

test "number_system_conversions" {
  // 测试数字系统转换（简化版）
  // 二进制转十进制：1010 = 8 + 0 + 2 + 0 = 10
  let binary_1010 = @azimuth.add(@azimuth.add(8, 0), @azimuth.add(2, 0))
  @azimuth.assert_eq(10, binary_1010)
  
  // 十六进制转十进制：FF = 15×16 + 15 = 255
  let hex_ff = @azimuth.add(@azimuth.multiply(15, 16), 15)
  @azimuth.assert_eq(255, hex_ff)
  
  // 罗马数字转十进制：XIV = 10 + (5 - 1) = 14
  let roman_xiv = @azimuth.add(10, @azimuth.add(5, -1))
  @azimuth.assert_eq(14, roman_xiv)
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  // 订单计算：商品价格×数量 + 税费 - 折扣 + 运费
  let unit_price = 299
  let quantity = 3
  let tax_rate = 8  // 8%
  let discount_amount = 100
  let shipping_fee = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(@azimuth.add(subtotal, tax), -discount_amount), shipping_fee)
  
  // 验证结果：(299×3) + (299×3×8/100) - 100 + 20 = 897 + 71 - 100 + 20 = 888
  @azimuth.assert_eq(888, final_total)
}