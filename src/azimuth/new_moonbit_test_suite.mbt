// 新的 MoonBit 测试套件
// 包含 10 个标准 MoonBit 测试用例

test "arithmetic_sequence_calculation" {
  // 测试等差数列计算：1 + 3 + 5 + ... + 19（前10个奇数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 1
  let last = 19
  
  let sum = azimuth::multiply(n, azimuth::add(first, last)) / 2
  assert_eq(100, sum)  // 10 × (1 + 19) / 2 = 100
}

test "string_processing_with_numbers" {
  // 测试包含数字的字符串处理
  let numeric_name = "User2023"
  let result = azimuth::greet(numeric_name)
  assert_eq_string("Hello, User2023!", result)
  
  let phone_number = "123-456-7890"
  let result2 = azimuth::greet(phone_number)
  assert_eq_string("Hello, 123-456-7890!", result2)
}

test "compound_interest_calculation" {
  // 测试复利计算：本金 × (1 + 利率)^年数
  // 简化为整数计算：1000 × (1 + 5%)^3 ≈ 1000 × 116 = 116000
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利公式，避免浮点数
  let growth_factor = azimuth::add(100, azimuth::multiply(rate_percent, years))
  let total = azimuth::multiply(principal, growth_factor)
  assert_eq(115000, total)  // 1000 × (100 + 5×3) = 1000 × 115 = 115000
}

test "matrix_element_calculation" {
  // 测试矩阵元素计算：二维数组转一维数组的索引
  let row = 2
  let col = 3
  let width = 4
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = azimuth::add(azimuth::multiply(row, width), col)
  assert_eq(11, index)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::add(azimuth::multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal1 = azimuth::multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth::multiply(item2_price, item2_quantity)
  let subtotal = azimuth::add(subtotal1, subtotal2)
  
  // 计算税费
  let tax = azimuth::multiply(subtotal, tax_rate) / 100
  let total = azimuth::add(subtotal, tax)
  
  assert_eq(753, total)  // (199×2 + 299×1) + 8% = 697 + 56 = 753
}

test "velocity_distance_calculation" {
  // �物理学计算：距离 = 速度 × 时间
  let speed = 60  // km/h
  let time_hours = 2
  let time_minutes = 30
  
  // 转换为小时
  let total_time_hours = azimuth::add(time_hours, azimuth::divide_with_ceil(time_minutes, 60))
  let distance = azimuth::multiply(speed, total_time_hours)
  
  assert_eq(150, distance)  // 60 × 2.5 = 150 km
}

test "binary_number_simulation" {
  // 测试二进制数模拟：使用十进制数表示二进制运算
  // 模拟二进制 1010 (10) + 0110 (6) = 10000 (16)
  let binary1 = 10  // 1010 in binary
  let binary2 = 6   // 0110 in binary
  
  let sum = azimuth::add(binary1, binary2)
  assert_eq(16, sum)
  
  // 模拟二进制乘法：101 (5) × 110 (6) = 11110 (30)
  let binary3 = 5   // 101 in binary
  let binary4 = 6   // 110 in binary
  
  let product = azimuth::multiply(binary3, binary4)
  assert_eq(30, product)
}

test "area_perimeter_calculation" {
  // 测试几何图形的面积和周长计算
  let length = 8
  let width = 5
  
  // 矩形面积
  let area = azimuth::multiply(length, width)
  assert_eq(40, area)
  
  // 矩形周长
  let perimeter = azimuth::multiply(azimuth::add(length, width), 2)
  assert_eq(26, perimeter)
  
  // 三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = azimuth::divide_with_ceil(azimuth::multiply(base, height), 2)
  assert_eq(30, triangle_area)
}

test "loan_payment_calculation" {
  // 测试贷款计算：月还款额 = (本金 × 月利率) / (1 - (1 + 月利率)^-还款月数)
  // 简化计算：本金 + 本金 × 利率 × 年数
  let principal = 100000
  let annual_rate = 5  // 5%
  let years = 10
  
  // 简化的总还款额计算
  let total_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years))
  let total_payment = azimuth::add(principal, total_interest)
  
  assert_eq(6000000, total_payment)  // 100000 + 100000×5×10 = 100000 + 5000000 = 5100000
}