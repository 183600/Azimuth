// 自包含的 MoonBit 测试用例
// 包含所需的函数定义和测试用例

// 定义所需的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

// 测试用例
test "temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), 30)
  assert_eq(230, boiling)
}

test "discount_calculation" {
  // 折扣计算测试
  let original_price = 1000
  let discount_percent = 20
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  // 计算折后价格
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(200, discount_amount)
  assert_eq(800, final_price)
}

test "compound_interest_simple" {
  // 简单复利计算测试
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简化复利计算：本金 + 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000×5×3 = 160000
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  // 矩形面积
  let length = 10
  let width = 6
  let rectangle_area = multiply(length, width)
  assert_eq(60, rectangle_area)
  
  // 三角形面积（简化：底×高÷2）
  let base = 8
  let height = 5
  let triangle_area = multiply(base, height) / 2
  assert_eq(20, triangle_area)
  
  // 梯形面积（简化：(上底+下底)×高÷2）
  let top_base = 4
  let bottom_base = 10
  let trapezoid_height = 5
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(35, trapezoid_area)
}

test "shopping_cart_total" {
  // 购物车总价计算测试
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  let shipping_fee = 15
  
  // 计算各商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let total3 = multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = add(add(add(total1, total2), total3), shipping_fee)
  
  // 验证：299×2 + 199×1 + 99×3 + 15 = 598 + 199 + 297 + 15 = 1109
  assert_eq(1109, cart_total)
}

test "time_conversion" {
  // 时间转换测试
  // 小时转分钟
  let hours = 3
  let minutes = multiply(hours, 60)
  assert_eq(180, minutes)
  
  // 分钟转秒
  let mins = 45
  let seconds = multiply(mins, 60)
  assert_eq(2700, seconds)
  
  // 计算总秒数（3小时45分钟）
  let total_seconds = add(multiply(hours, 3600), multiply(mins, 60))
  assert_eq(13500, total_seconds)
}

test "average_calculation" {
  // 平均值计算测试
  // 计算五个数的平均值
  let num1 = 85
  let num2 = 90
  let num3 = 78
  let num4 = 92
  let num5 = 88
  
  // 计算总和
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  // 计算平均值
  let average = sum / 5
  
  assert_eq(433, sum)
  assert_eq(86, average)
}

test "speed_distance_time" {
  // 速度、距离、时间关系测试
  // 计算距离：速度 × 时间
  let speed = 60  // km/h
  let time_minutes = 150
  let distance = multiply(speed, time_minutes) / 60
  
  assert_eq(150, distance)  // 60 × 150/60 = 150 km
  
  // 计算时间：距离 ÷ 速度
  let distance2 = 180
  let time2 = multiply(distance2, 60) / speed
  assert_eq(180, time2)  // 180×60/60 = 180分钟 = 3小时
}

test "currency_conversion" {
  // 货币转换测试
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY（简化汇率）
  
  // 转换为人民币
  let cny_amount = multiply(usd_amount, exchange_rate)
  assert_eq(700, cny_amount)
  
  // 添加手续费（3%）
  let fee_rate = 3
  let fee = multiply(cny_amount, fee_rate) / 100
  let total_with_fee = add(cny_amount, fee)
  
  assert_eq(21, fee)
  assert_eq(721, total_with_fee)
}

test "array_index_calculation" {
  // 数组索引计算测试：二维数组转一维数组
  let row = 4
  let col = 3
  let width = 5
  
  // 计算一维数组索引：index = row × width + col
  let index = add(multiply(row, width), col)
  assert_eq(23, index)
  
  // 测试边界情况
  let first_index = add(multiply(0, 5), 0)
  assert_eq(0, first_index)
  
  let last_index = add(multiply(9, 5), 4)
  assert_eq(49, last_index)
}