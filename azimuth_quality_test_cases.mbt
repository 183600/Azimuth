// Azimuth 高质量测试用例
// 使用标准 MoonBit 测试语法编写的精选测试用例

test "azimuth_error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "azimuth_string_greeting_with_emoji" {
  // 测试包含表情符号的字符串问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  assert_eq_string("Hello, 🎯!", greet("🎯"))
}

test "azimuth_mathematical_series_sum" {
  // 测试数学级数求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = add(1, add(2, add(3, add(4, 5))))
  assert_eq(15, sum1)
  
  // 测试等差数列：2 + 4 + 6 + 8 = 20
  let sum2 = add(add(2, 4), add(6, 8))
  assert_eq(20, sum2)
}

test "azimuth_business_inventory_calculation" {
  // 测试商业库存计算
  let initial_stock = 500
  let units_sold = 137
  let units_received = 85
  
  let final_stock = add(add(initial_stock, -units_sold), units_received)
  assert_eq(448, final_stock)
}

test "azimuth_time_conversion_hours_to_minutes" {
  // 测试时间转换：小时到分钟
  let hours = 8
  let minutes_per_hour = 60
  let total_minutes = multiply(hours, minutes_per_hour)
  assert_eq(480, total_minutes)
}

test "azimuth_geometric_area_calculation" {
  // 测试几何面积计算：矩形面积
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 测试周长计算
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
}

test "azimuth_financial_simple_interest" {
  // 测试金融简单利息计算
  let principal = 1000
  let rate_percent = 5
  let time_years = 3
  
  let simple_interest = divide_with_ceil(multiply(multiply(principal, rate_percent), time_years), 100)
  let total_amount = add(principal, simple_interest)
  
  assert_eq(150, simple_interest)
  assert_eq(1150, total_amount)
}

test "azimuth_temperature_conversion" {
  // 测试温度转换（简化版）
  let celsius = 25
  let conversion_factor = 9
  let divisor = 5
  let offset = 32
  
  // 简化的摄氏度到华氏度转换：F = (C * 9/5) + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, conversion_factor), divisor), offset)
  assert_eq(77, fahrenheit)
}

test "azimuth_data_transfer_calculation" {
  // 测试数据传输计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  
  // 计算传输时间（秒）
  let transfer_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  assert_eq(17, transfer_time)
  
  // 计算传输到更慢网络的时间
  let slower_speed = 20
  let slower_time = divide_with_ceil(file_size_mb, slower_speed)
  assert_eq(43, slower_time)
}

test "azimuth_combinatorial_analysis" {
  // 测试组合分析：计算排列数
  let n = 5
  let r = 3
  
  // 简化的排列计算 P(5,3) = 5 * 4 * 3 = 60
  let permutation = multiply(multiply(n, n - 1), n - 2)
  assert_eq(60, permutation)
  
  // 简化的组合计算 C(5,3) = 5 * 4 * 3 / (3 * 2 * 1) = 10
  let combination = divide_with_ceil(permutation, multiply(multiply(r, r - 1), 1))
  assert_eq(10, combination)
}