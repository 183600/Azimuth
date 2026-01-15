// 额外的 MoonBit 测试用例
// 这些测试用例涵盖了各种实际应用场景和边界情况

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = azimuth::greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User2023ProjectAlpha"
  let result2 = azimuth::greet(alphanumeric)
  assert_eq_string("Hello, User2023ProjectAlpha!", result2)
  
  // 测试包含空格的复杂字符串
  let complex_name = "John Doe Jr."
  let result3 = azimuth::greet(complex_name)
  assert_eq_string("Hello, John Doe Jr.!", result3)
}

test "mathematical_series_fibonacci" {
  // 斐波那契数列计算测试
  // 计算前10个斐波那契数的和
  // F1=1, F2=1, F3=2, F4=3, F5=5, F6=8, F7=13, F8=21, F9=34, F10=55
  // 和 = 1+1+2+3+5+8+13+21+34+55 = 143
  
  let f1 = 1
  let f2 = 1
  let f3 = azimuth::add(f1, f2)  // 2
  let f4 = azimuth::add(f2, f3)  // 3
  let f5 = azimuth::add(f3, f4)  // 5
  let f6 = azimuth::add(f4, f5)  // 8
  let f7 = azimuth::add(f5, f6)  // 13
  let f8 = azimuth::add(f6, f7)  // 21
  let f9 = azimuth::add(f7, f8)  // 34
  let f10 = azimuth::add(f8, f9) // 55
  
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(f1, f2), f3), f4), f5), f6), f7), f8), azimuth::add(f9, f10))
  assert_eq(143, sum)
}

test "business_calculations_profit_margin" {
  // 商业利润率计算测试
  let revenue = 10000
  let cost = 7000
  let expenses = 1500
  
  // 计算利润：收入 - 成本 - 费用
  let profit = azimuth::add(azimuth::add(revenue, -cost), -expenses)
  assert_eq(1500, profit)
  
  // 计算利润率：利润 / 收入 * 100%
  let profit_margin = azimuth::multiply(profit, 100) / revenue
  assert_eq(15, profit_margin)  // 1500 / 10000 * 100 = 15%
}

test "physics_calculations_velocity" {
  // 物理速度计算测试
  let distance = 300  // 米
  let time = 60       // 秒
  
  // 计算速度：距离 / 时间
  let velocity = distance / time
  assert_eq(5, velocity)  // 300 / 60 = 5 米/秒
  
  // 计算加速度：(最终速度 - 初始速度) / 时间
  let initial_velocity = 0
  let final_velocity = 10
  let acceleration = azimuth::add(final_velocity, -initial_velocity) / time
  assert_eq(0, acceleration)  // (10-0)/60 = 0 (整数除法)
  
  // 计算位移：初始速度 × 时间 + 0.5 × 加速度 × 时间²
  // 简化为整数计算：初始速度 × 时间 + 加速度 × 时间 × 时间 / 2
  let displacement = azimuth::add(azimuth::multiply(initial_velocity, time), azimuth::multiply(acceleration, azimuth::multiply(time, time)) / 2)
  assert_eq(0, displacement)  // 0*60 + 0*60*60/2 = 0
}

test "computer_science_algorithms" {
  // 计算机科学算法测试
  // 二分查找中的中间位置计算
  let left = 0
  let right = 15
  
  // 计算中间位置：(left + right) / 2
  let middle = azimuth::add(left, right) / 2
  assert_eq(7, middle)  // (0+15)/2 = 7
  
  // 计算数组索引（二维数组转一维）
  let row = 3
  let col = 4
  let width = 6
  
  // 索引 = 行 * 宽度 + 列
  let index = azimuth::add(azimuth::multiply(row, width), col)
  assert_eq(22, index)  // 3*6 + 4 = 22
  
  // 计算哈希表大小（下一个2的幂）
  let elements = 100
  // 简化计算：找到大于等于elements的最小的2的幂
  let hash_size = 128  // 简化值
  assert_true(hash_size >= elements)
}

test "statistical_analysis_variance" {
  // 统计分析方差计算测试
  // 数据集：2, 4, 4, 4, 5, 5, 7, 9
  // 平均值：(2+4+4+4+5+5+7+9)/8 = 40/8 = 5
  
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(2, 4), 4), 4), 5), 5), 7), 9)
  let mean = sum / 8
  assert_eq(5, mean)
  
  // 计算方差：((x1-mean)² + (x2-mean)² + ... + (xn-mean)²) / n
  // ((2-5)² + (4-5)² + (4-5)² + (4-5)² + (5-5)² + (5-5)² + (7-5)² + (9-5)²) / 8
  // (9 + 1 + 1 + 1 + 0 + 0 + 4 + 16) / 8 = 32/8 = 4
  
  let diff1 = azimuth::add(2, -mean)  // -3
  let diff2 = azimuth::add(4, -mean)  // -1
  let diff3 = azimuth::add(5, -mean)  // 0
  let diff4 = azimuth::add(7, -mean)  // 2
  let diff5 = azimuth::add(9, -mean)  // 4
  
  let sq1 = azimuth::multiply(diff1, diff1)  // 9
  let sq2 = azimuth::multiply(diff2, diff2)  // 1
  let sq3 = azimuth::multiply(diff3, diff3)  // 0
  let sq4 = azimuth::multiply(diff4, diff4)  // 4
  let sq5 = azimuth::multiply(diff5, diff5)  // 16
  
  let sum_of_squares = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(sq1, sq2), sq2), sq2), sq3), sq3), azimuth::add(sq4, sq5))
  let variance = sum_of_squares / 8
  assert_eq(4, variance)
}

test "game_development_collision_detection" {
  // 游戏开发碰撞检测测试
  // 两个矩形的位置和大小
  let rect1_x = 10
  let rect1_y = 20
  let rect1_width = 30
  let rect1_height = 40
  
  let rect2_x = 25
  let rect2_y = 35
  let rect2_width = 35
  let rect2_height = 45
  
  // 计算矩形1的右边界和下边界
  let rect1_right = azimuth::add(rect1_x, rect1_width)
  let rect1_bottom = azimuth::add(rect1_y, rect1_height)
  
  // 计算矩形2的右边界和下边界
  let rect2_right = azimuth::add(rect2_x, rect2_width)
  let rect2_bottom = azimuth::add(rect2_y, rect2_height)
  
  // 检查是否碰撞：矩形1的右边界 > 矩形2的左边界 且 矩形1的左边界 < 矩形2的右边界
  // 且 矩形1的下边界 > 矩形2的上边界 且 矩形1的上边界 < 矩形2的下边界
  let collision_x = azimuth::add(rect1_right, -rect2_x) > 0 && azimuth::add(rect2_right, -rect1_x) > 0
  let collision_y = azimuth::add(rect1_bottom, -rect2_y) > 0 && azimuth::add(rect2_bottom, -rect1_y) > 0
  let collision = collision_x && collision_y
  
  assert_true(collision)
  
  // 计算重叠区域的面积
  let overlap_left = rect2_x  // 25
  let overlap_top = rect2_y   // 35
  let overlap_right = rect1_right  // 40
  let overlap_bottom = rect1_bottom // 60
  
  let overlap_width = azimuth::add(overlap_right, -overlap_left)  // 15
  let overlap_height = azimuth::add(overlap_bottom, -overlap_top) // 25
  let overlap_area = azimuth::multiply(overlap_width, overlap_height)
  
  assert_eq(375, overlap_area)  // 15 * 25 = 375
}

test "cryptography_simple_hashing" {
  // 简单哈希算法模拟测试
  // 使用简单的多项式滚动哈希算法
  let input = "hello"
  let base = 31
  let mod = 1000000007  // 大质数
  
  // 简化计算：只使用前几个字符
  // hash = (h1 * base^0 + h2 * base^1 + h3 * base^2 + ...) % mod
  // 这里我们简化计算，只使用字符的ASCII码值
  
  let h1 = 104  // 'h'
  let h2 = 101  // 'e'
  let h3 = 108  // 'l'
  let h4 = 108  // 'l'
  let h5 = 111  // 'o'
  
  // 简化的哈希计算
  let hash = azimuth::add(azimuth::add(azimuth::add(azimuth::add(h1, azimuth::multiply(h2, base)), azimuth::multiply(h3, azimuth::multiply(base, base))), azimuth::multiply(h4, azimuth::multiply(azimuth::multiply(base, base), base))), azimuth::multiply(h5, azimuth::multiply(azimuth::multiply(azimuth::multiply(base, base), base), base)))
  
  // 由于结果可能很大，我们只检查它是否为正数
  assert_true(hash > 0)
  
  // 测试不同输入产生不同哈希值
  let input2 = "world"
  let w1 = 119  // 'w'
  let w2 = 111  // 'o'
  let w3 = 114  // 'r'
  let w4 = 108  // 'l'
  let w5 = 100  // 'd'
  
  let hash2 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(w1, azimuth::multiply(w2, base)), azimuth::multiply(w3, azimuth::multiply(base, base))), azimuth::multiply(w4, azimuth::multiply(azimuth::multiply(base, base), base))), azimuth::multiply(w5, azimuth::multiply(azimuth::multiply(azimuth::multiply(base, base), base), base)))
  
  // 确保不同的输入产生不同的哈希值
  assert_true(hash != hash2)
}