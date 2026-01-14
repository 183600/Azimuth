// 紧凑型 MoonBit 测试用例
// 包含10个精心设计的测试用例，覆盖各种实际应用场景

test "binary_search_simulation" {
  // 模拟二分查找的索引计算
  let low = 0
  let high = 99
  let mid = add(low, (add(high, -low)) / 2)
  assert_eq(49, mid)
  
  // 测试第二次查找
  let new_low = add(mid, 1)
  let new_mid = add(new_low, (add(high, -new_low)) / 2)
  assert_eq(74, new_mid)
}

test "velocity_calculation" {
  // 物理速度计算：距离 = 速度 × 时间
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 计算最终速度：v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  assert_eq(20, final_velocity)
  
  // 计算距离：s = ut + 0.5at²
  let distance = add(multiply(initial_velocity, time), multiply(multiply(acceleration, time), time) / 2)
  assert_eq(75, distance)
}

test "loan_amortization" {
  // 贷款分期计算（简化版）
  let principal = 120000  // 贷款本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  
  // 计算总利息（简化）：本金 × 年利率 × 年数
  let total_interest = multiply(principal, multiply(annual_rate, years)) / 100
  assert_eq(216000, total_interest)
  
  // 计算总还款额
  let total_payment = add(principal, total_interest)
  assert_eq(336000, total_payment)
  
  // 计算月供（简化）
  let monthly_payment = total_payment / multiply(years, 12)
  assert_eq(933, monthly_payment)  // 整数除法
}

test "data_structure_operations" {
  // 模拟数据结构操作：栈的压入和弹出
  let stack_size = 0
  
  // 压入5个元素
  let after_push = add(stack_size, 5)
  assert_eq(5, after_push)
  
  // 弹出2个元素
  let after_pop = add(after_push, -2)
  assert_eq(3, after_pop)
  
  // 检查是否为空
  let is_empty = after_pop == 0
  assert_false(is_empty)
  
  // 清空栈
  let after_clear = 0
  assert_eq(0, after_clear)
}

test "calendar_calculations" {
  // 日历计算：计算两个日期之间的天数
  let year1 = 2023
  let month1 = 1
  let day1 = 15
  
  let year2 = 2023
  let month2 = 3
  let day2 = 20
  
  // 简化计算：假设每月30天
  let days1 = add(multiply(year1, 360), add(multiply(month1, 30), day1))
  let days2 = add(multiply(year2, 360), add(multiply(month2, 30), day2))
  
  let difference = add(days2, -days1)
  assert_eq(65, difference)
}

test "network_throughput" {
  // 网络吞吐量计算
  let file_size_mb = 100    // 文件大小(MB)
  let network_speed_mbps = 20  // 网络速度(Mbps)
  
  // 计算传输时间（秒）：文件大小 × 8 / 网络速度
  let transfer_time = multiply(file_size, 8) / network_speed
  assert_eq(40, transfer_time)
  
  // 考虑网络开销，增加20%的时间
  let overhead = multiply(transfer_time, 20) / 100
  let total_time = add(transfer_time, overhead)
  assert_eq(48, total_time)
}

test "game_physics_simulation" {
  // 游戏物理模拟：抛物线运动
  let initial_velocity = 20  // 初始速度
  let angle = 45            // 发射角度
  let gravity = 10          // 重力加速度
  
  // 水平速度分量（简化）
  let vx = multiply(initial_velocity, 707) / 1000  // cos(45°) ≈ 0.707
  assert_eq(14, vx)  // 整数除法
  
  // 垂直速度分量（简化）
  let vy = multiply(initial_velocity, 707) / 1000  // sin(45°) ≈ 0.707
  assert_eq(14, vy)
  
  // 飞行时间（简化）：2 * vy / g
  let flight_time = multiply(2, vy) / gravity
  assert_eq(2, flight_time)
  
  // 射程：vx * flight_time
  let range = multiply(vx, flight_time)
  assert_eq(28, range)
}

test "image_processing_filters" {
  // 图像处理滤镜：RGB颜色调整
  let red = 100
  let green = 150
  let blue = 200
  
  // 增加亮度：每个通道增加20
  let brightness = 20
  let new_red = add(red, brightness)
  let new_green = add(green, brightness)
  let new_blue = add(blue, brightness)
  
  assert_eq(120, new_red)
  assert_eq(170, new_green)
  assert_eq(220, new_blue)
  
  // 增加对比度：超出255的设为255
  let contrast_factor = 150  // 150%
  let contrast_red = multiply(new_red, contrast_factor) / 100
  let contrast_green = multiply(new_green, contrast_factor) / 100
  let contrast_blue = multiply(new_blue, contrast_factor) / 100
  
  // 确保不超过255
  let final_red = if contrast_red > 255 { 255 } else { contrast_red }
  let final_green = if contrast_green > 255 { 255 } else { contrast_green }
  let final_blue = if contrast_blue > 255 { 255 } else { contrast_blue }
  
  assert_eq(180, final_red)
  assert_eq(255, final_green)
  assert_eq(255, final_blue)
}

test "cryptography_simulation" {
  // 密码学模拟：简单的凯撒密码
  let plaintext = "HELLO"
  let shift = 3
  
  // 由于我们只有整数运算，我们模拟字母位置的偏移
  let A_position = 1
  let H_position = 8
  let E_position = 5
  let L_position = 12
  let O_position = 15
  
  // 加密：(position + shift) mod 26
  let H_encrypted = add(add(H_position, shift), -26) / 26 * 26 + add(H_position, shift)
  let H_final = if add(H_position, shift) > 26 { add(H_position, shift, -26) } else { add(H_position, shift) }
  assert_eq(11, H_final)  // K
  
  let E_encrypted = add(E_position, shift)
  assert_eq(8, E_encrypted)  // H
  
  let L_encrypted = add(L_position, shift)
  assert_eq(15, L_encrypted)  // O
  
  let O_encrypted = add(O_position, shift)
  let O_final = if add(O_position, shift) > 26 { add(O_position, shift, -26) } else { add(O_position, shift) }
  assert_eq(18, O_final)  // R
}

test "machine_learning_metrics" {
  // 机器学习指标计算
  let total_samples = 1000
  let true_positive = 450
  let false_positive = 50
  let false_negative = 100
  
  // 计算精确率：TP / (TP + FP)
  let precision = multiply(true_positive, 100) / add(true_positive, false_positive)
  assert_eq(90, precision)  // 90%
  
  // 计算召回率：TP / (TP + FN)
  let recall = multiply(true_positive, 100) / add(true_positive, false_negative)
  assert_eq(81, recall)  // 81%（整数除法）
  
  // 计算F1分数：2 * (precision * recall) / (precision + recall)
  let f1_score = multiply(2, multiply(precision, recall)) / add(precision, recall)
  assert_eq(85, f1_score)  // 85%（整数除法）
  
  // 计算准确率：(TP + TN) / Total
  let true_negative = add(total_samples, add(-true_positive, add(-false_positive, -false_negative)))
  let accuracy = multiply(add(true_positive, true_negative), 100) / total_samples
  assert_eq(90, accuracy)  // 90%
}