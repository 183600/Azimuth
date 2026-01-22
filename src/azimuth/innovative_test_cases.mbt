// 创新测试用例 - 为 azimuth 库添加的独特测试场景
// 这些测试用例涵盖了新的应用场景和边界情况

test "geometric_calculations" {
  // 几何计算：计算矩形的周长和面积
  let length = 8
  let width = 5
  
  // 计算面积
  let area = multiply(length, width)
  
  // 计算周长
  let perimeter = multiply(2, add(length, width))
  
  // 计算对角线长度的平方（勾股定理）
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
  assert_eq(89, diagonal_squared)
}

test "temperature_conversion" {
  // 温度转换计算
  let celsius = 25
  
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let back_to_celsius = multiply(fahrenheit - 32, 5) / 9
  
  assert_eq(77, fahrenheit)
  assert_eq(25, back_to_celsius)
}

test "digital_clock_arithmetic" {
  // 数字时钟算术：12小时制的时间计算
  let current_hour = 9
  let hours_to_add = 5
  
  // 计算5小时后的时间（12小时制）
  let future_hour = add(current_hour, hours_to_add)
  let adjusted_hour = if future_hour > 12 { 
    add(future_hour, -12) 
  } else { 
    future_hour 
  }
  
  assert_eq(14, future_hour)
  assert_eq(2, adjusted_hour)
}

test "binary_representation_simulation" {
  // 二进制表示模拟：计算二进制数的十进制值
  // 模拟二进制数 1011 (1*2^3 + 0*2^2 + 1*2^1 + 1*2^0)
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1
  
  let decimal_value = add(add(
    multiply(bit3, 8),  // 1 * 2^3
    multiply(bit2, 4)   // 0 * 2^2
  ), add(
    multiply(bit1, 2),  // 1 * 2^1
    multiply(bit0, 1)   // 1 * 2^0
  ))
  
  assert_eq(11, decimal_value)
}

test "loan_interest_calculation" {
  // 贷款利息计算
  let principal = 10000
  let annual_rate_percent = 6
  let years = 3
  
  // 简单利息计算
  let total_interest = multiply(principal, years) * annual_rate_percent / 100
  let total repayment = add(principal, total_interest)
  
  // 使用向上取整计算月供
  let monthly_payment = divide_with_ceil(total_repayment, multiply(years, 12))
  
  assert_eq(1800, total_interest)
  assert_eq(11800, total_repayment)
  assert_eq(328, monthly_payment)
}

test "game_score_system" {
  // 游戏评分系统
  let base_score = 100
  let level_multiplier = 3
  let bonus_points = 50
  let penalty_points = 20
  
  // 计算最终得分
  let level_score = multiply(base_score, level_multiplier)
  let final_score = add(add(level_score, bonus_points), -penalty_points)
  
  // 计算星级评分（每100分1星）
  let star_rating = divide_with_ceil(final_score, 100)
  
  assert_eq(300, level_score)
  assert_eq(330, final_score)
  assert_eq(4, star_rating)
}

test "calendar_date_arithmetic" {
  // 日历日期算术：计算某月的天数
  let is_leap_year = true
  let february_days = if is_leap_year { 29 } else { 28 }
  
  // 计算第一季度的总天数
  let q1_days = add(add(31, february_days), 31)
  
  // 计算上半年天数
  let half_year_days = add(q1_days, add(30, 31))
  
  assert_eq(29, february_days)
  assert_eq(91, q1_days)
  assert_eq(151, half_year_days)
}

test "cpu_scheduling_simulation" {
  // CPU调度模拟：时间片轮转
  let total_processes = 5
  let time_slice = 10
  let total_cycles = 3
  
  // 计算每个进程的总执行时间
  let time_per_process = multiply(time_slice, total_cycles)
  
  // 计算系统总运行时间
  let total_system_time = multiply(total_processes, time_per_process)
  
  // 计算上下文切换次数
  let context_switches = multiply(total_processes, total_cycles)
  
  assert_eq(30, time_per_process)
  assert_eq(150, total_system_time)
  assert_eq(15, context_switches)
}

test "data_structure_array_simulation" {
  // 数据结构数组模拟：计算数组操作
  let array_size = 8
  let element_size = 4
  
  // 计算数组总大小
  let total_array_size = multiply(array_size, element_size)
  
  // 模拟数组索引访问（计算偏移量）
  let index = 5
  let offset = multiply(index, element_size)
  
  // 计算剩余元素数量
  let remaining_elements = add(array_size, -index)
  
  assert_eq(32, total_array_size)
  assert_eq(20, offset)
  assert_eq(3, remaining_elements)
}

test "network_protocol_simulation" {
  // 网络协议模拟：数据包传输
  let header_size = 20
  let data_size = 1024
  let max_packet_size = 1500
  
  // 计算单个数据包大小
  let packet_size = add(header_size, data_size)
  
  // 计算传输文件所需的数据包数量
  let file_size = 5000
  let packets_needed = divide_with_ceil(file_size, data_size)
  
  // 计算总传输大小
  let total_transferred = multiply(packets_needed, packet_size)
  
  assert_eq(1044, packet_size)
  assert_eq(5, packets_needed)
  assert_eq(5220, total_transferred)
}