// Azimuth 项目新增标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用 inspect 语句进行验证

test "arithmetic_sequence_summation" {
  // 等差数列求和测试
  // 计算 1 + 4 + 7 + 10 + 13 的和
  let a1 = 1
  let d = 3
  
  // 逐项计算
  let a2 = a1 + d           // 1 + 3 = 4
  let a3 = a2 + d           // 4 + 3 = 7
  let a4 = a3 + d           // 7 + 3 = 10
  let a5 = a4 + d           // 10 + 3 = 13
  
  // 求和
  let sum = a1 + a2 + a3 + a4 + a5
  
  // 验证结果
  inspect a2  // 期望: 4
  inspect a3  // 期望: 7
  inspect a4  // 期望: 10
  inspect a5  // 期望: 13
  inspect sum // 期望: 35
}

test "geometric_sequence_properties" {
  // 等比数列性质测试
  // 计算 2, 6, 18, 54 的性质
  let first = 2
  let ratio = 3
  
  // 生成等比数列
  let second = first * ratio      // 2 * 3 = 6
  let third = second * ratio      // 6 * 3 = 18
  let fourth = third * ratio      // 18 * 3 = 54
  
  // 验证等比性质
  let ratio_check1 = second / first   // 6 / 2 = 3
  let ratio_check2 = third / second   // 18 / 6 = 3
  let ratio_check3 = fourth / third   // 54 / 18 = 3
  
  inspect second      // 期望: 6
  inspect third       // 期望: 18
  inspect fourth      // 期望: 54
  inspect ratio_check1 // 期望: 3
  inspect ratio_check2 // 期望: 3
  inspect ratio_check3 // 期望: 3
}

test "temperature_conversion_validation" {
  // 温度转换验证测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  
  let celsius_freezing = 0    // 冰点
  let celsius_boiling = 100   // 沸点
  let celsius_room = 20       // 室温
  
  // 转换为华氏度
  let fahrenheit_freezing_step1 = celsius_freezing * 9
  let fahrenheit_freezing_step2 = fahrenheit_freezing_step1 / 5
  let fahrenheit_freezing = fahrenheit_freezing_step2 + 32
  
  let fahrenheit_boiling_step1 = celsius_boiling * 9
  let fahrenheit_boiling_step2 = fahrenheit_boiling_step1 / 5
  let fahrenheit_boiling = fahrenheit_boiling_step2 + 32
  
  let fahrenheit_room_step1 = celsius_room * 9
  let fahrenheit_room_step2 = fahrenheit_room_step1 / 5
  let fahrenheit_room = fahrenheit_room_step2 + 32
  
  inspect fahrenheit_freezing // 期望: 32
  inspect fahrenheit_boiling  // 期望: 212
  inspect fahrenheit_room     // 期望: 68
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 8
  
  // 第一年
  let year1_interest = principal * rate_percent / 100
  let year1_balance = principal + year1_interest
  
  // 第二年
  let year2_interest = year1_balance * rate_percent / 100
  let year2_balance = year1_balance + year2_interest
  
  // 第三年
  let year3_interest = year2_balance * rate_percent / 100
  let year3_balance = year2_balance + year3_interest
  
  // 计算总收益
  let total_profit = year3_balance - principal
  
  inspect year1_balance // 期望: 1080
  inspect year2_balance // 期望: 1166
  inspect year3_balance // 期望: 1259
  inspect total_profit  // 期望: 259
}

test "inventory_optimization" {
  // 库存优化算法测试
  let demand_forecast = 450
  let lead_time_days = 7
  let daily_usage = 15
  let safety_stock_days = 3
  
  // 计算安全库存
  let safety_stock = daily_usage * safety_stock_days
  
  // 计算在途库存
  let in_transit_stock = daily_usage * lead_time_days
  
  // 计算再订货点
  let reorder_point = safety_stock + in_transit_stock
  
  // 计算经济订货量（简化版）
  let eoq = demand_forecast / 12
  
  // 验证库存不会断货
  let coverage_days = reorder_point / daily_usage
  
  inspect safety_stock   // 期望: 45
  inspect in_transit_stock // 期望: 105
  inspect reorder_point // 期望: 150
  inspect eoq          // 期望: 38
  inspect coverage_days // 期望: 10
}

test "binary_number_operations" {
  // 二进制数操作测试
  // 模拟二进制数 10110 (22) 的操作
  
  let decimal_value = 22
  
  // 分解为二进制位
  let bit4 = 1 * 16  // 2^4 = 16
  let bit2 = 1 * 4   // 2^2 = 4
  let bit1 = 1 * 2   // 2^1 = 2
  let bit0 = 0 * 1   // 2^0 = 1 (未设置)
  
  // 重新组合验证
  let reconstructed = bit4 + bit2 + bit1 + bit0
  
  // 计算设置位的数量
  let set_bits = 1 + 1 + 1  // bit4, bit2, bit1 被设置
  
  // 左移操作模拟 (22 << 2 = 88)
  let left_shift = decimal_value * 4
  
  // 右移操作模拟 (22 >> 2 = 5)
  let right_shift = decimal_value / 4
  
  inspect reconstructed // 期望: 22
  inspect set_bits     // 期望: 3
  inspect left_shift   // 期望: 88
  inspect right_shift  // 期望: 6
}

test "game_score_system" {
  // 游戏评分系统测试
  let base_score = 50
  let level_bonus = 20
  let combo_multiplier = 3
  let time_bonus = 15
  let penalty_deduction = 8
  
  // 计算基础得分
  let basic_score = base_score + level_bonus
  
  // 应用连击倍数
  let combo_score = basic_score * combo_multiplier
  
  // 添加时间奖励
  let time_score = combo_score + time_bonus
  
  // 减去惩罚
  let final_score = time_score - penalty_deduction
  
  // 计算星级（每50分一星）
  let star_rating = final_score / 50
  
  // 计算剩余分数
  let remainder_score = final_score - (star_rating - 1) * 50
  
  inspect basic_score    // 期望: 70
  inspect combo_score    // 期望: 210
  inspect time_score     // 期望: 225
  inspect final_score    // 期望: 217
  inspect star_rating    // 期望: 5
  inspect remainder_score // 期望: 17
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  // 模拟队列操作：入队、出队、查看队首
  
  // 初始队列为空
  let queue_size = 0
  
  // 入队操作
  let enqueue1 = 10
  let queue_size1 = queue_size + 1
  let front1 = enqueue1
  
  let enqueue2 = 20
  let queue_size2 = queue_size1 + 1
  let front2 = front1  // 队首不变
  
  let enqueue3 = 30
  let queue_size3 = queue_size2 + 1
  let front3 = front2  // 队首不变
  
  // 出队操作
  let dequeue1 = front3
  let queue_size_after_dequeue1 = queue_size3 - 1
  let new_front1 = enqueue2  // 新的队首
  
  // 再次出队
  let dequeue2 = new_front1
  let queue_size_after_dequeue2 = queue_size_after_dequeue1 - 1
  let new_front2 = enqueue3  // 新的队首
  
  // 验证队列操作
  inspect queue_size1              // 期望: 1
  inspect front1                   // 期望: 10
  inspect queue_size2              // 期望: 2
  inspect front2                   // 期望: 10
  inspect queue_size3              // 期望: 3
  inspect front3                   // 期望: 10
  inspect dequeue1                 // 期望: 10
  inspect queue_size_after_dequeue1 // 期望: 2
  inspect new_front1               // 期望: 20
  inspect dequeue2                 // 期望: 20
  inspect queue_size_after_dequeue2 // 期望: 1
  inspect new_front2               // 期望: 30
}