// Azimuth 项目增强测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖核心功能、边界情况和实际应用场景

test "data_validation_comprehensive" {
  // 数据验证综合测试
  // 测试各种输入数据的有效性和边界情况
  
  // 正整数验证
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(0, @azimuth.subtract(100, 100))
  
  // 负数验证
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(0, @azimuth.subtract(-50, -50))
  
  // 混合符号验证
  @azimuth.assert_eq(0, @azimuth.add(50, -50))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(100, @azimuth.subtract(50, -50))
  
  // 零值验证
  @azimuth.assert_eq(50, @azimuth.add(50, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(50, 0))
  @azimuth.assert_eq(50, @azimuth.subtract(50, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 50))
}

test "performance_boundary_testing" {
  // 性能边界测试
  // 测试大数值运算的性能和准确性
  
  // 大数加法
  let large_num1 = 1000000
  let large_num2 = 500000
  @azimuth.assert_eq(1500000, @azimuth.add(large_num1, large_num2))
  
  // 大数乘法
  let large_mult1 = 10000
  let large_mult2 = 10000
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_mult1, large_mult2))
  
  // 大数除法
  let large_div = 1000000
  let divisor = 3
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(large_div, divisor))
  
  // 边界值测试
  let boundary_val = 1000000  // 使用较小的边界值避免溢出
  @azimuth.assert_eq(1000001, @azimuth.add(boundary_val, 1))
  @azimuth.assert_eq(1, @azimuth.subtract(boundary_val, 999999))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种错误情况下的函数行为
  
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值除法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  
  // 负数除法边界
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "algorithm_application_scenarios" {
  // 算法应用场景测试
  // 模拟实际算法应用中的计算场景
  
  // 模拟二分查找过程
  let low = 0
  let high = 99
  
  // 第一次二分
  let mid1 = @azimuth.divide_with_ceil(@azimuth.add(low, high), 2)
  @azimuth.assert_eq(50, mid1)
  
  // 调整范围后的第二次二分
  let new_low = @azimuth.add(mid1, 1)
  let mid2 = @azimuth.divide_with_ceil(@azimuth.add(new_low, high), 2)
  @azimuth.assert_eq(75, mid2)
  
  // 调整范围后的第三次二分
  let new_high = @azimuth.subtract(mid2, 1)
  let mid3 = @azimuth.divide_with_ceil(@azimuth.add(new_low, new_high), 2)
  @azimuth.assert_eq(63, mid3)
  
  // 最终定位
  let final_low = @azimuth.add(mid3, 1)
  let final_mid = @azimuth.divide_with_ceil(@azimuth.add(final_low, new_high), 2)
  @azimuth.assert_eq(69, final_mid)
}

test "string_processing_extended" {
  // 字符串处理扩展测试
  // 测试字符串相关的各种操作和边界情况
  
  // 基本问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 特殊字符输入
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 空格测试 !", @azimuth.greet("空格测试 "))
  
  // 边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  
  // 长字符串测试
  let long_name = "VeryLongName"
  @azimuth.assert_eq_string("Hello, VeryLongName!", @azimuth.greet(long_name))
  
  // Unicode字符测试
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "mathematical_function_combinations" {
  // 数学函数组合测试
  // 测试多个数学函数的复杂组合运算
  
  let base = 10
  let multiplier = 5
  let offset = 20
  let divisor = 3
  
  // 复杂计算链：(base * multiplier + offset) / divisor
  let step1 = @azimuth.multiply(base, multiplier)      // 10 * 5 = 50
  let step2 = @azimuth.add(step1, offset)               // 50 + 20 = 70
  let step3 = @azimuth.divide_with_ceil(step2, divisor) // ceil(70/3) = 24
  @azimuth.assert_eq(50, step1)
  @azimuth.assert_eq(70, step2)
  @azimuth.assert_eq(24, step3)
  
  // 嵌套计算：base + (base * multiplier) / (divisor + offset)
  let nested1 = @azimuth.multiply(base, multiplier)           // 10 * 5 = 50
  let nested2 = @azimuth.add(divisor, offset)                 // 3 + 20 = 23
  let nested3 = @azimuth.divide_with_ceil(nested1, nested2)   // ceil(50/23) = 3
  let nested4 = @azimuth.add(base, nested3)                   // 10 + 3 = 13
  @azimuth.assert_eq(13, nested4)
  
  // 逆向验证
  let reverse_check = @azimuth.multiply(divisor, step3)
  let reverse_remainder = @azimuth.subtract(reverse_check, step2)
  @azimuth.assert_eq(2, reverse_remainder)  // 24 * 3 - 70 = 2
}

test "business_logic_simulation" {
  // 业务逻辑模拟测试
  // 模拟真实业务场景中的计算逻辑
  
  // 订单计算场景
  let unit_price = 25
  let quantity = 8
  let discount_rate = 10
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)  // 25 * 8 = 200
  @azimuth.assert_eq(200, subtotal)
  
  // 计算折扣（超过100元享受10%折扣）
  let discount_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, discount_rate), 100)  // ceil(200*10/100) = 20
  let discounted_total = @azimuth.subtract(subtotal, discount_amount)  // 200 - 20 = 180
  @azimuth.assert_eq(20, discount_amount)
  @azimuth.assert_eq(180, discounted_total)
  
  // 计算税费（5%）
  let tax_rate = 5
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(discounted_total, tax_rate), 100)  // ceil(180*5/100) = 9
  let final_total = @azimuth.add(discounted_total, tax_amount)  // 180 + 9 = 189
  @azimuth.assert_eq(9, tax_amount)
  @azimuth.assert_eq(189, final_total)
  
  // 计算包装箱数（每箱最多装6个）
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(quantity, items_per_box)  // ceil(8/6) = 2
  @azimuth.assert_eq(2, boxes_needed)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟常见数据结构的操作计算
  
  // 模拟数组操作
  let array_size = 10
  let element_size = 4
  
  // 计算数组总大小
  let total_size = @azimuth.multiply(array_size, element_size)  // 10 * 4 = 40
  @azimuth.assert_eq(40, total_size)
  
  // 模拟数组扩容（每次翻倍）
  let new_capacity = @azimuth.multiply(array_size, 2)  // 10 * 2 = 20
  let new_total_size = @azimuth.multiply(new_capacity, element_size)  // 20 * 4 = 80
  @azimuth.assert_eq(20, new_capacity)
  @azimuth.assert_eq(80, new_total_size)
  
  // 模拟栈操作
  let stack_capacity = 16
  let current_size = 12
  let push_count = 5
  
  // 计算push后的栈大小
  let new_stack_size = @azimuth.add(current_size, push_count)  // 12 + 5 = 17
  @azimuth.assert_eq(17, new_stack_size)
  
  // 检查是否需要扩容
  let needs_expansion = new_stack_size > stack_capacity
  @azimuth.assert_eq(true, needs_expansion)
  
  // 计算扩容后的新容量
  let expanded_capacity = @azimuth.multiply(stack_capacity, 2)  // 16 * 2 = 32
  @azimuth.assert_eq(32, expanded_capacity)
}

test "statistical_calculations" {
  // 统计计算测试
  // 模拟统计学中的基本计算
  
  // 数据集：[15, 22, 28, 35, 40, 48, 55]
  let data1 = 15
  let data2 = 22
  let data3 = 28
  let data4 = 35
  let data5 = 40
  let data6 = 48
  let data7 = 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5), data6), data7)
  @azimuth.assert_eq(243, sum)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean = @azimuth.divide_with_ceil(sum, count)  // ceil(243/7) = 35
  @azimuth.assert_eq(35, mean)
  
  // 计算中位数（奇数个数据点）
  let median = data4  // 第4个数据点
  @azimuth.assert_eq(35, median)
  
  // 计算范围
  let min_val = data1
  let max_val = data7
  let range = @azimuth.subtract(max_val, min_val)  // 55 - 15 = 40
  @azimuth.assert_eq(40, range)
  
  // 计算方差近似值
  let variance_step1 = @azimuth.subtract(data1, mean)  // 15 - 35 = -20
  let variance_step2 = @azimuth.multiply(variance_step1, variance_step1)  // (-20)^2 = 400
  let variance_step3 = @azimuth.subtract(data7, mean)  // 55 - 35 = 20
  let variance_step4 = @azimuth.multiply(variance_step3, variance_step3)  // 20^2 = 400
  let variance_approx = @azimuth.divide_with_ceil(@azimuth.add(variance_step2, variance_step4), count)  // ceil(800/7) = 115
  @azimuth.assert_eq(115, variance_approx)
}

test "system_integration_comprehensive" {
  // 系统集成综合测试
  // 测试多个功能的综合应用
  
  // 模拟用户注册和积分系统
  let base_points = 100
  let referral_bonus = 50
  let daily_login_bonus = 5
  let activity_multiplier = 2
  
  // 推荐奖励
  let referral_count = 3
  let total_referral_bonus = @azimuth.multiply(referral_bonus, referral_count)  // 50 * 3 = 150
  
  // 连续登录奖励
  let login_days = 7
  let total_login_bonus = @azimuth.multiply(daily_login_bonus, login_days)  // 5 * 7 = 35
  
  // 活动奖励（双倍积分）
  let activity_points = 80
  let activity_bonus = @azimuth.multiply(activity_points, activity_multiplier)  // 80 * 2 = 160
  
  // 计算总积分
  let user_points = @azimuth.add(@azimuth.add(@azimuth.add(base_points, total_referral_bonus), total_login_bonus), activity_bonus)
  @azimuth.assert_eq(445, user_points)
  
  // 计算用户等级（每100分一级）
  let user_level = @azimuth.divide_with_ceil(user_points, 100)  // ceil(445/100) = 5
  @azimuth.assert_eq(5, user_level)
  
  // 计算下一级所需积分
  let next_level_threshold = @azimuth.multiply(user_level, 100)  // 5 * 100 = 500
  let points_needed = @azimuth.subtract(next_level_threshold, user_points)  // 500 - 445 = 55
  @azimuth.assert_eq(500, next_level_threshold)
  @azimuth.assert_eq(55, points_needed)
  
  // 验证用户等级字符串功能
  @azimuth.assert_eq_string("Hello, Level 5!", @azimuth.greet("Level 5"))
}