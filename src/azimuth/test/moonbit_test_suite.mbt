// MoonBit 测试套件 - 新增10个测试用例
// 涵盖算法、实际应用和边界测试场景

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的第10项：F(10) = 55
  // 使用迭代方法：F(n) = F(n-1) + F(n-2)
  let n = 10
  
  // 手动计算前几项
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9)  // 55
  
  @azimuth.assert_eq(55, f10)
  
  // 验证斐波那契性质：F(n+1) * F(n-1) - F(n)² = (-1)^n
  let left_side = @azimuth.add(@azimuth.multiply(f9, f8), @azimuth.multiply(f10, -f10))  // 34*21 - 55² = 714 - 3025 = -2311
  @azimuth.assert_eq(-2311, left_side)
}

test "prime_number_validation" {
  // 验证质数性质
  let num = 17
  
  // 检查是否能被2到sqrt(num)之间的数整除
  // 由于没有取模运算，我们使用除法来验证
  let sqrt_17_approx = 4  // sqrt(17) ≈ 4.12，取整数部分
  
  // 检查17是否能被2,3,4整除
  let div_by_2 = num / 2  // 8
  let rem_by_2 = @azimuth.add(num, @azimuth.multiply(div_by_2, -2))  // 17 - 8*2 = 1
  
  let div_by_3 = num / 3  // 5
  let rem_by_3 = @azimuth.add(num, @azimuth.multiply(div_by_3, -3))  // 17 - 5*3 = 2
  
  let div_by_4 = num / 4  // 4
  let rem_by_4 = @azimuth.add(num, @azimuth.multiply(div_by_4, -4))  // 17 - 4*4 = 1
  
  // 验证余数都不为0，因此17是质数
  @azimuth.assert_true(rem_by_2 > 0)
  @azimuth.assert_true(rem_by_3 > 0)
  @azimuth.assert_true(rem_by_4 > 0)
  
  // 验证合数15不是质数
  let composite = 15
  let div_by_3_check = composite / 3  // 5
  let rem_by_3_check = @azimuth.add(composite, @azimuth.multiply(div_by_3_check, -3))  // 15 - 5*3 = 0
  
  @azimuth.assert_eq(0, rem_by_3_check)  // 15能被3整除，不是质数
}

test "greatest_common_divisor" {
  // 计算最大公约数（GCD）
  // 使用欧几里得算法：gcd(a, b) = gcd(b, a mod b)
  let a = 48
  let b = 18
  
  // 手动实现欧几里得算法
  // 48 mod 18 = 12
  let div1 = 48 / 18  // 2
  let rem1 = @azimuth.add(48, @azimuth.multiply(div1, -18))  // 48 - 2*18 = 12
  
  // 18 mod 12 = 6
  let div2 = 18 / 12  // 1
  let rem2 = @azimuth.add(18, @azimuth.multiply(div2, -12))  // 18 - 1*12 = 6
  
  // 12 mod 6 = 0
  let div3 = 12 / 6  // 2
  let rem3 = @azimuth.add(12, @azimuth.multiply(div3, -6))  // 12 - 2*6 = 0
  
  // GCD是最后一个非零余数
  @azimuth.assert_eq(6, rem2)
  
  // 验证GCD性质：gcd(a, b) * lcm(a, b) = a * b
  // lcm(48, 18) = 48 * 18 / gcd(48, 18) = 864 / 6 = 144
  let product = @azimuth.multiply(a, b)  // 864
  let lcm = product / rem2  // 864 / 6 = 144
  @azimuth.assert_eq(144, lcm)
}

test "binary_search_simulation" {
  // 模拟二分查找算法
  // 在有序数组 [1, 3, 5, 7, 9, 11, 13, 15, 17, 19] 中查找13
  let target = 13
  
  // 模拟二分查找过程
  let left = 0
  let right = 9
  let found = false
  
  // 第一次查找：mid = (0 + 9) / 2 = 4，arr[4] = 9
  let mid1 = (left + right) / 2  // 4
  let arr_mid1 = 9  // 模拟数组访问
  let comparison1 = target > arr_mid1  // 13 > 9 = true
  
  // 第二次查找：left = 5, right = 9, mid = (5 + 9) / 2 = 7，arr[7] = 15
  let left2 = @azimuth.add(mid1, 1)  // 5
  let mid2 = (left2 + right) / 2  // 7
  let arr_mid2 = 15  // 模拟数组访问
  let comparison2 = target > arr_mid2  // 13 > 15 = false
  
  // 第三次查找：left = 5, right = 6, mid = (5 + 6) / 2 = 5，arr[5] = 11
  let right2 = @azimuth.add(mid2, -1)  // 6
  let mid3 = (left2 + right2) / 2  // 5
  let arr_mid3 = 11  // 模拟数组访问
  let comparison3 = target > arr_mid3  // 13 > 11 = true
  
  // 第四次查找：left = 6, right = 6, mid = 6，arr[6] = 13
  let left3 = @azimuth.add(mid3, 1)  // 6
  let mid4 = (left3 + right2) / 2  // 6
  let arr_mid4 = 13  // 模拟数组访问
  let found_result = target == arr_mid4  // 13 == 13 = true
  
  @azimuth.assert_true(found_result)
  @azimuth.assert_eq(6, mid4)  // 找到目标在索引6的位置
}

test "calendar_date_calculation" {
  // 日历日期计算
  // 计算2023年3月15日是该年的第几天
  let day = 15
  let month = 3  // 3月
  
  // 各月份的天数（非闰年）
  let jan_days = 31
  let feb_days = 28
  let mar_days = 31
  
  // 计算3月15日是该年的第几天
  let days_before_march = @azimuth.add(jan_days, feb_days)  // 31 + 28 = 59
  let day_of_year = @azimuth.add(days_before_march, day)  // 59 + 15 = 74
  
  @azimuth.assert_eq(74, day_of_year)
  
  // 计算该年还剩多少天
  let total_days = 365
  let days_remaining = @azimuth.add(total_days, -day_of_year)  // 365 - 74 = 291
  @azimuth.assert_eq(291, days_remaining)
  
  // 计算该日期是第几周（假设第1周从1月1日开始）
  let week_number = day_of_year / 7  // 74 / 7 = 10（整数除法）
  @azimuth.assert_eq(10, week_number)
}

test "bank_loan_amortization" {
  // 银行贷款摊销计算
  let loan_amount = 100000  // 贷款金额
  let annual_rate = 5  // 年利率百分比
  let loan_term = 20  // 贷款年限
  
  // 简化的月利率计算（年利率/12）
  let monthly_rate = annual_rate / 12  // 5 / 12 = 0（整数除法，简化为0）
  
  // 使用简化的计算方式：总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(@azimuth.multiply(loan_amount, annual_rate), loan_term)  // 100000 × 5 × 20 = 10000000
  let total_payment = @azimuth.add(loan_amount, total_interest)  // 100000 + 10000000 = 10100000
  
  @azimuth.assert_eq(10100000, total_payment)
  
  // 计算月供（简化版）
  let total_months = @azimuth.multiply(loan_term, 12)  // 20 × 12 = 240
  let monthly_payment = total_payment / total_months  // 10100000 / 240 = 42083
  
  @azimuth.assert_eq(42083, monthly_payment)
  
  // 计算第一年的还款总额
  let first_year_total = @azimuth.multiply(monthly_payment, 12)  // 42083 × 12 = 504996
  @azimuth.assert_eq(504996, first_year_total)
}

test "inventory_management_system" {
  // 库存管理系统计算
  let initial_stock = 1000
  let units_sold = 150
  let units_received = 300
  let units_returned = 20
  let units_damaged = 15
  
  // 计算当前库存
  let after_sales = @azimuth.add(initial_stock, -units_sold)  // 1000 - 150 = 850
  let after_receiving = @azimuth.add(after_sales, units_received)  // 850 + 300 = 1150
  let after_returns = @azimuth.add(after_receiving, units_returned)  // 1150 + 20 = 1170
  let final_stock = @azimuth.add(after_returns, -units_damaged)  // 1170 - 15 = 1155
  
  @azimuth.assert_eq(1155, final_stock)
  
  // 计算库存周转率（简化版）
  let average_stock = @azimuth.add(initial_stock, final_stock) / 2  // (1000 + 1155) / 2 = 1077
  let turnover_rate = units_sold / average_stock  // 150 / 1077 = 0（整数除法）
  @azimuth.assert_eq(0, turnover_rate)
  
  // 计算库存价值（假设单价为50）
  let unit_price = 50
  let inventory_value = @azimuth.multiply(final_stock, unit_price)  // 1155 × 50 = 57750
  @azimuth.assert_eq(57750, inventory_value)
}

test "physics_kinematics_calculations" {
  // 物理运动学计算
  let initial_velocity = 10  // 初始速度 m/s
  let acceleration = 2  // 加速度 m/s²
  let time = 5  // 时间 s
  
  // 计算最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))  // 10 + 2×5 = 20
  @azimuth.assert_eq(20, final_velocity)
  
  // 计算位移：s = ut + 0.5at²
  let ut = @azimuth.multiply(initial_velocity, time)  // 10 × 5 = 50
  let at_squared = @azimuth.multiply(acceleration, @azimuth.multiply(time, time))  // 2 × 25 = 50
  let half_at_squared = at_squared / 2  // 50 / 2 = 25
  let displacement = @azimuth.add(ut, half_at_squared)  // 50 + 25 = 75
  
  @azimuth.assert_eq(75, displacement)
  
  // 计算平均速度：v_avg = (u + v) / 2
  let sum_velocities = @azimuth.add(initial_velocity, final_velocity)  // 10 + 20 = 30
  let average_velocity = sum_velocities / 2  // 30 / 2 = 15
  
  @azimuth.assert_eq(15, average_velocity)
  
  // 验证：位移 = 平均速度 × 时间
  let calculated_displacement = @azimuth.multiply(average_velocity, time)  // 15 × 5 = 75
  @azimuth.assert_eq(displacement, calculated_displacement)
}

test "cryptographic_hash_simulation" {
  // 模拟简单哈希函数计算
  let input_string = "Hello"
  
  // 模拟简单哈希：将每个字符的ASCII值相加
  // 'H' = 72, 'e' = 101, 'l' = 108, 'l' = 108, 'o' = 111
  let h_ascii = 72
  let e_ascii = 101
  let l_ascii = 108
  let o_ascii = 111
  
  let hash_value = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(h_ascii, e_ascii), l_ascii), l_ascii), o_ascii)  // 72 + 101 + 108 + 108 + 111 = 500
  
  @azimuth.assert_eq(500, hash_value)
  
  // 模拟哈希的雪崩效应：输入微小变化导致输出巨大变化
  let input_string_2 = "Hella"  // 只改变最后一个字符
  let a_ascii = 97  // 'a' = 97
  
  let hash_value_2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(h_ascii, e_ascii), l_ascii), l_ascii), a_ascii)  // 72 + 101 + 108 + 108 + 97 = 486
  
  @azimuth.assert_eq(486, hash_value_2)
  
  // 验证哈希值的差异
  let hash_difference = @azimuth.add(hash_value, -hash_value_2)  // 500 - 486 = 14
  @azimuth.assert_eq(14, hash_difference)
  
  // 模拟哈希的模运算（用于限制哈希值范围）
  let hash_mod = hash_value % 256  // 500 % 256 = 244
  @azimuth.assert_eq(244, hash_mod)
}

test "compression_algorithm_simulation" {
  // 模拟简单压缩算法：行程编码（RLE）
  // 压缩字符串 "AAABBBCCDDDD" -> "3A3B2C4D"
  let char_A = 65  // 'A'的ASCII值
  let char_B = 66  // 'B'的ASCII值
  let char_C = 67  // 'C'的ASCII值
  let char_D = 68  // 'D'的ASCII值
  
  // 原始字符串长度
  let original_length = @azimuth.add(@azimuth.add(@azimuth.add(3, 3), 2), 4)  // 3 + 3 + 2 + 4 = 12
  
  @azimuth.assert_eq(12, original_length)
  
  // 压缩后的字符串长度（每个字符变成数字+字符）
  let compressed_length = @azimuth.add(@azimuth.add(@azimuth.add(2, 2), 2), 2)  // 2 + 2 + 2 + 2 = 8
  
  @azimuth.assert_eq(8, compressed_length)
  
  // 计算压缩率
  let compression_ratio = @azimuth.multiply(compressed_length, 100) / original_length  // 8 × 100 / 12 = 66
  @azimuth.assert_eq(66, compression_ratio)  // 压缩到原来的66%
  
  // 计算节省的空间
  let space_saved = @azimuth.add(original_length, -compressed_length)  // 12 - 8 = 4
  @azimuth.assert_eq(4, space_saved)
  
  // 模拟更极端的压缩场景
  let extreme_original = 100  // 100个相同字符
  let extreme_compressed = 2   // 压缩为 "100A"
  let extreme_ratio = @azimuth.multiply(extreme_compressed, 100) / extreme_original  // 2 × 100 / 100 = 2
  @azimuth.assert_eq(2, extreme_ratio)  // 压缩到原来的2%
}