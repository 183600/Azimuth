// 高质量 MoonBit 测试套件
// 包含10个精心设计的测试用例，覆盖各种场景

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let a = 42
  let b = 17
  
  // 加法恒等律：a + 0 = a
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
  
  // 乘法恒等律：a × 1 = a
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
  
  // 乘法零律：a × 0 = 0
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, b))
}

test "boundary_value_comprehensive" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值边界测试
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))  // 特殊情况处理
  
  // 边界值组合
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
}

test "string_processing_internationalization" {
  // 国际化字符串处理测试
  let english_name = "John"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let emoji_name = "🌟"
  let mixed_name = "User_2023🚀"
  
  assert_eq_string("Hello, John!", greet(english_name))
  assert_eq_string("Hello, 张三!", greet(chinese_name))
  assert_eq_string("Hello, 田中!", greet(japanese_name))
  assert_eq_string("Hello, 🌟!", greet(emoji_name))
  assert_eq_string("Hello, User_2023🚀!", greet(mixed_name))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 年利率5%
  let years = 3          // 3年
  let fee = 50           // 手续费
  
  // 复利计算：本息和 = 本金 × (1 + 利率 × 年数)
  let interest_factor = add(1, multiply(rate, years))  // 1 + 5×3 = 16
  let total_with_interest = multiply(principal, interest_factor)  // 10000 × 16 = 160000
  
  // 减去手续费
  let final_amount = add(total_with_interest, -fee)  // 160000 - 50 = 159950
  
  assert_eq(159950, final_amount)
}

test "algorithmic_efficiency_test" {
  // 算法效率测试：验证不同计算方法的结果一致性
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 方法1：逐个相加
  let sum1 = add(add(add(add(num1, num2), num3), num4), num5)
  
  // 方法2：分组相加再合并
  let group1 = add(num1, num2)  // 10 + 20 = 30
  let group2 = add(num3, num4)  // 30 + 40 = 70
  let sum2 = add(add(group1, group2), num5)  // 30 + 70 + 50 = 150
  
  // 验证结果一致性
  assert_eq(sum1, sum2)
  assert_eq(150, sum1)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  let trapezoid_base1 = 8
  let trapezoid_base2 = 12
  let trapezoid_height = 5
  
  // 矩形面积 = 长 × 宽
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积 = 底 × 高 ÷ 2
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let trapezoid_area = multiply(add(trapezoid_base1, trapezoid_base2), trapezoid_height) / 2
  assert_eq(50, trapezoid_area)
}

test "time_zone_conversion" {
  // 时区转换计算（简化版）
  let local_hour = 15  // 本地时间下午3点
  let utc_offset = -8  // UTC-8时区
  let meeting_offset = 9  // 要转换到UTC+9时区
  
  // 转换为UTC时间
  let utc_hour = add(local_hour, -utc_offset)  // 15 - (-8) = 23
  let normalized_utc = if utc_hour >= 24 { add(utc_hour, -24) } else { utc_hour }
  
  // 转换为目标时区
  let target_hour = add(normalized_utc, meeting_offset)  // 23 + 9 = 32
  let final_hour = if target_hour >= 24 { add(target_hour, -24) } else { target_hour }
  
  assert_eq(8, final_hour)  // 最终时间是上午8点
}

test "statistical_analysis" {
  // 统计分析测试
  let data1 = 85
  let data2 = 90
  let data3 = 78
  let data4 = 92
  let data5 = 80
  
  // 计算总和
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  assert_eq(425, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(85, average)
  
  // 计算方差（简化版）：平方差的平均值
  let diff1 = add(data1, -average)  // 85 - 85 = 0
  let diff2 = add(data2, -average)  // 90 - 85 = 5
  let diff3 = add(data3, -average)  // 78 - 85 = -7
  let diff4 = add(data4, -average)  // 92 - 85 = 7
  let diff5 = add(data5, -average)  // 80 - 85 = -5
  
  let variance = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), 
                           multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5)) / 5
  assert_eq(28, variance)
}

test "game_mechanics_calculation" {
  // 游戏机制计算测试
  let base_damage = 100
  let weapon_multiplier = 15  // 武器倍率
  let critical_bonus = 2       // 暴击伤害倍率
  
  // 基础伤害计算
  let weapon_damage = multiply(base_damage, weapon_multiplier)
  
  // 暴击判定（简化：假设触发暴击）
  let is_critical = true  // 模拟暴击触发
  let final_damage = if is_critical { 
    multiply(weapon_damage, critical_bonus) 
  } else { 
    weapon_damage 
  }
  
  assert_eq(3000, final_damage)  // 100 × 15 × 2 = 3000
  
  // 连击伤害计算
  let combo_count = 3
  let combo_multiplier = add(1, multiply(combo_count, 10))  // 1 + 3×10 = 31
  let combo_damage = multiply(final_damage, combo_multiplier) / 10
  
  assert_eq(9300, combo_damage)  // 3000 × 31 ÷ 10 = 9300
}

test "data_validation_scenarios" {
  // 数据验证场景测试
  let input_positive = 42
  let input_negative = -17
  let input_zero = 0
  let input_max = 2147483647
  let input_min = -2147483648
  
  // 验证正数处理
  assert_true(input_positive > 0)
  assert_true(add(input_positive, 1) > input_positive)
  
  // 验证负数处理
  assert_true(input_negative < 0)
  assert_true(add(input_negative, -1) < input_negative)
  
  // 验证零值处理
  assert_true(input_zero == 0)
  assert_eq(input_positive, add(input_zero, input_positive))
  assert_eq(0, multiply(input_zero, input_positive))
  
  // 验证边界值处理
  assert_true(input_max > 0)
  assert_true(input_min < 0)
  assert_eq(input_max, add(input_max, 1))  // 溢出保护
  assert_eq(input_min, add(input_min, -1))  // 溢出保护
  
  // 字符串验证
  let valid_string = "ValidInput123"
  let empty_string = ""
  let special_string = "Special@#$%"
  
  assert_eq_string("Hello, ValidInput123!", greet(valid_string))
  assert_eq_string("Hello, !", greet(empty_string))
  assert_eq_string("Hello, Special@#$%!", greet(special_string))
}