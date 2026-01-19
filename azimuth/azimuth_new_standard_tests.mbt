// Azimuth 项目新标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 加法测试
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(22, sum)
  
  // 乘法测试
  let product = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, product)
  
  // 复合运算测试
  let complex = @azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3))
  @azimuth.assert_eq(51, complex)
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  // 测试基本问候功能
  let greeting1 = @azimuth.greet(name1)
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  
  let greeting2 = @azimuth.greet(name2)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  
  let greeting3 = @azimuth.greet(name3)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
}

test "mathematical_formulas" {
  // 数学公式测试
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
  
  // 计算立方体体积
  let side = 4
  let volume = @azimuth.multiply(side, @azimuth.multiply(side, side))
  @azimuth.assert_eq(64, volume)
}

test "financial_calculations" {
  // 金融计算测试
  // 计算简单利息
  let principal = 1000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  let total_amount = @azimuth.add(principal, simple_interest)
  
  @azimuth.assert_eq(150, simple_interest)
  @azimuth.assert_eq(1150, total_amount)
  
  // 计算折扣价格
  let original_price = 200
  let discount_percent = 25
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(50, discount_amount)
  @azimuth.assert_eq(150, final_price)
}

test "temperature_conversion" {
  // 温度转换测试
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.multiply(@azimuth.multiply(celsius, 9), 1) / 5 + 32
  @azimuth.assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 86
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  @azimuth.assert_eq(30, celsius2)
}

test "array_index_calculations" {
  // 数组索引计算测试
  let array_size = 10
  
  // 计算中间索引
  let middle_index = array_size / 2
  @azimuth.assert_eq(5, middle_index)
  
  // 计算最后一个索引
  let last_index = @azimuth.add(array_size, -1)
  @azimuth.assert_eq(9, last_index)
  
  // 计算四分之一位置
  let quarter_index = array_size / 4
  @azimuth.assert_eq(2, quarter_index)
}

test "time_calculations" {
  // 时间计算测试
  // 计算总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(
    @azimuth.multiply(hours, 3600),
    @azimuth.add(@azimuth.multiply(minutes, 60), seconds)
  )
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算天数
  let total_hours = 72
  let days = total_hours / 24
  @azimuth.assert_eq(3, days)
}

test "statistical_operations" {
  // 统计运算测试
  // 计算平均值
  let numbers_sum = 150
  let count = 5
  let average = numbers_sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算百分比
  let part = 25
  let whole = 200
  let percentage = @azimuth.multiply(part, 100) / whole
  @azimuth.assert_eq(12, percentage)
  
  // 计算比例
  let ratio_numerator = 3
  let ratio_denominator = 4
  let ratio_decimal = @azimuth.multiply(ratio_numerator, 100) / ratio_denominator
  @azimuth.assert_eq(75, ratio_decimal)
}

test "geometry_calculations" {
  // 几何计算测试
  // 计算圆的周长（简化版，使用π≈22/7）
  let radius = 7
  let pi_approx = 22
  let circumference = @azimuth.multiply(2, @azimuth.multiply(radius, pi_approx)) / 7
  @azimuth.assert_eq(44, circumference)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
  
  // 计算梯形面积（(上底+下底)×高÷2）
  let top_base = 5
  let bottom_base = 9
  let trapezoid_height = 4
  let trapezoid_area = @azimuth.multiply(
    @azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(28, trapezoid_area)
}

test "boundary_value_tests" {
  // 边界值测试
  let zero = 0
  let one = 1
  let negative_one = -1
  
  // 测试与零的运算
  @azimuth.assert_eq(0, @azimuth.multiply(100, zero))
  @azimuth.assert_eq(100, @azimuth.add(100, zero))
  @azimuth.assert_eq(100, @azimuth.multiply(100, one))
  @azimuth.assert_eq(101, @azimuth.add(100, one))
  @azimuth.assert_eq(99, @azimuth.add(100, negative_one))
  
  // 测试负数运算
  let negative_a = -15
  let negative_b = -7
  let negative_sum = @azimuth.add(negative_a, negative_b)
  let negative_product = @azimuth.multiply(negative_a, negative_b)
  
  @azimuth.assert_eq(-22, negative_sum)
  @azimuth.assert_eq(105, negative_product)
}