// 额外的标准 MoonBit 测试用例
// 测试实际应用场景和边缘情况

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
}

test "bank_interest_calculation" {
  // 测试银行利息计算：本金 + 本金 × 利率 × 年数
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 计算利息
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算总额
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + 10000 * 5 * 3 = 160000
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(1195, cart_total)  // 299*2 + 199*3 = 598 + 597 = 1195
}

test "area_perimeter_calculation" {
  // 测试面积和周长计算
  let length = 10
  let width = 5
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)  // 10 * 5 = 50
  
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)  // 2*10 + 2*5 = 20 + 10 = 30
}

test "score_calculation" {
  // 测试得分计算
  let base_score = 1000
  let level_multiplier = 3
  let bonus = 500
  let penalty = 200
  
  // 计算最终得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)
  let final_score = @azimuth.add(@azimuth.add(level_score, bonus), -penalty)
  
  @azimuth.assert_eq(3300, final_score)  // 1000*3 + 500 - 200 = 3000 + 500 - 200 = 3300
}

test "time_conversion" {
  // 测试时间转换
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(150, total_minutes)  // 2*60 + 30 = 120 + 30 = 150
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9000, total_seconds)  // 150 * 60 = 9000
}

test "average_calculation" {
  // 测试平均值计算
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  let score5 = 100
  
  // 计算总分
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  // 计算平均值
  let average = sum / 5
  
  @azimuth.assert_eq(90, average)  // (85+90+95+80+100)/5 = 450/5 = 90
}

test "percentage_calculation" {
  // 测试百分比计算
  let total = 500
  let percentage = 25  // 25%
  
  // 计算百分比值
  let value = @azimuth.multiply(total, percentage) / 100
  @azimuth.assert_eq(125, value)  // 500 * 25 / 100 = 125
  
  // 计算增加后的值
  let increased = @azimuth.add(total, value)
  @azimuth.assert_eq(625, increased)  // 500 + 125 = 625
}

test "fuel_consumption" {
  // 测试燃油消耗计算
  let distance = 300  // 公里
  let fuel_efficiency = 15  // 每升汽油行驶的公里数
  let fuel_price = 8  // 每升汽油价格
  
  // 计算所需燃油量
  let fuel_needed = distance / fuel_efficiency
  // 计算总费用
  let total_cost = @azimuth.multiply(fuel_needed, fuel_price)
  
  @azimuth.assert_eq(160, total_cost)  // (300/15) * 8 = 20 * 8 = 160
}

test "data_storage_calculation" {
  // 测试数据存储计算
  let file_size = 1024  // KB
  let file_count = 100
  let redundancy_factor = 3  // 冗余因子
  
  // 计算原始存储需求
  let raw_storage = @azimuth.multiply(file_size, file_count)
  // 计算包含冗余的总存储需求
  let total_storage = @azimuth.multiply(raw_storage, redundancy_factor)
  
  @azimuth.assert_eq(307200, total_storage)  // 1024 * 100 * 3 = 307200 KB
}