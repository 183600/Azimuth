// 标准 MoonBit 测试套件
// 为 azimuth 项目添加的高质量测试用例

test "arithmeti_sequence_sum" {
  // 测试等差数列求和
  let first_term = 3
  let difference = 2
  let terms = 5
  
  // 计算 3 + 5 + 7 + 9 + 11 = 35
  let sum = add(first_term, add(add(first_term + difference, add(first_term + 2*difference, add(first_term + 3*difference, first_term + 4*difference)))))
  assert_eq(35, sum)
}

test "geometric_progression" {
  // 测试等比数列计算
  let first = 2
  let ratio = 3
  let terms = 4
  
  // 计算 2 * 3^3 = 54
  let result = multiply(first, multiply(ratio, multiply(ratio, ratio)))
  assert_eq(54, result)
}

test "factorial_calculation" {
  // 测试阶乘计算 (5! = 120)
  let five = 5
  let factorial = multiply(five, multiply(four, multiply(three, multiply(two, one))))
  assert_eq(120, factorial)
}

test "temperature_conversion" {
  // 测试温度转换场景
  let celsius = 25
  // 转换为华氏度: F = C * 9/5 + 32
  let fahrenheit = add(multiply(celsius, divide_with_ceil(9, 5)), 32)
  assert_eq(77, fahrenheit)
}

test "area_calculation" {
  // 测试面积计算
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算需要多少块 2x2 的地砖
  let tile_size = 4
  let tiles_needed = divide_with_ceil(area, tile_size)
  assert_eq(12, tiles_needed)
}

test "time_calculation" {
  // 测试时间计算
  let hours = 5
  let minutes = 45
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(345, total_minutes)
  
  // 计算需要多少个 30 分钟的时间段
  let session_length = 30
  let sessions = divide_with_ceil(total_minutes, session_length)
  assert_eq(12, sessions)
}

test "percentage_calculation" {
  // 测试百分比计算
  let total = 200
  let percentage = 25
  let result = divide_with_ceil(multiply(total, percentage), 100)
  assert_eq(50, result)
}

test "discount_calculation" {
  // 测试折扣计算
  let original_price = 120
  let discount_percentage = 15
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percentage), 100)
  let final_price = subtract(original_price, discount_amount)
  assert_eq(102, final_price)
}

test "speed_distance_time" {
  // 测试速度、距离、时间关系
  let distance = 150
  let speed = 50
  let time = divide_with_ceil(distance, speed)
  assert_eq(3, time)
  
  // 验证计算
  let calculated_distance = multiply(speed, time)
  assert_eq(150, calculated_distance)
}

test "compound_interest" {
  // 测试复利计算（简化版）
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简化计算：1000 * (1 + 5/100)^3 ≈ 1158
  let growth_factor = add(100, rate)
  let amount = divide_with_ceil(multiply(principal, multiply(growth_factor, multiply(growth_factor, growth_factor))), 10000)
  assert_eq(1158, amount)
}

// 辅助函数定义
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 辅助常量定义
let one = 1
let two = 2
let three = 3
let four = 4
let five = 5