// Azimuth 项目实用测试用例
// 使用标准 MoonBit 测试语法，涵盖多种实际应用场景

test "financial_calculations" {
  // 金融计算测试
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简单利息计算
  let interest = @azimuth.multiply(principal, rate) / 100
  let yearly_interest = @azimuth.multiply(interest, years)
  let total = @azimuth.add(principal, yearly_interest)
  
  @azimuth.assert_eq(50, interest)
  @azimuth.assert_eq(150, yearly_interest)
  @azimuth.assert_eq(1150, total)
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度 (简化公式: F = C * 2 + 30)
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)
}

test "shopping_cart_calculations" {
  // 购物车计算测试
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 10% 折扣
  let discount = subtotal / 10
  // 使用加法代替减法：total = subtotal - discount = subtotal + (-discount)
  let total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(50, item1_total)
  @azimuth.assert_eq(45, item2_total)
  @azimuth.assert_eq(95, subtotal)
  @azimuth.assert_eq(9, discount)
  @azimuth.assert_eq(86, total)
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  let speed = 60  // km/h
  let time = 2    // hours
  
  let distance = @azimuth.multiply(speed, time)
  @azimuth.assert_eq(120, distance)
  
  // 计算平均速度
  let total_distance = 300
  let total_time = 5
  let average_speed = total_distance / total_time
  @azimuth.assert_eq(60, average_speed)
}

test "score_calculations" {
  // 分数计算测试
  let test1 = 85
  let test2 = 90
  let test3 = 78
  let test4 = 92
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(test1, test2), test3), test4)
  let average_score = total_score / 4
  
  @azimuth.assert_eq(345, total_score)
  @azimuth.assert_eq(86, average_score)
}

test "area_perimeter_calculations" {
  // 面积和周长计算测试
  let length = 8
  let width = 6
  
  // 矩形面积
  let area = @azimuth.multiply(length, width)
  // 矩形周长
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
  
  // 三角形面积 (底 × 高 ÷ 2)
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}



test "inventory_management" {
  // 库存管理测试
  let initial_stock = 100
  let sold = 25
  let received = 40
  
  // 使用加法代替减法：current_stock = (initial_stock + received) - sold = (initial_stock + received) + (-sold)
  let current_stock = @azimuth.add(@azimuth.add(initial_stock, received), -sold)
  
  @azimuth.assert_eq(115, current_stock)
  
  // 计算库存周转率
  let cost_per_item = 15
  let total_cost = @azimuth.multiply(current_stock, cost_per_item)
  @azimuth.assert_eq(1725, total_cost)
}

test "data_analysis" {
  // 数据分析测试
  let data1 = 45
  let data2 = 67
  let data3 = 23
  let data4 = 89
  let data5 = 34
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let average = sum / 5
  
  // 找出最大值和最小值的简单方法
  let max = data4  // 89
  let min = data3  // 23
  // 使用加法代替减法：range = max - min = max + (-min)
  let range = @azimuth.add(max, -min)
  
  @azimuth.assert_eq(258, sum)
  @azimuth.assert_eq(51, average)
  @azimuth.assert_eq(66, range)
}

test "coordinate_geometry" {
  // 坐标几何测试
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  // 计算两点之间的距离的平方 (避免平方根)
  // 使用加法代替减法：delta_x = x2 - x1 = x2 + (-x1)
  let delta_x = @azimuth.add(x2, -x1)
  let delta_y = @azimuth.add(y2, -y1)
  let distance_squared = @azimuth.add(@azimuth.multiply(delta_x, delta_x), @azimuth.multiply(delta_y, delta_y))
  
  @azimuth.assert_eq(3, delta_x)
  @azimuth.assert_eq(4, delta_y)
  @azimuth.assert_eq(25, distance_squared)  // 3² + 4² = 9 + 16 = 25
}