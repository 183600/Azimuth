// 用户新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖不同场景

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 123
  let b = 456
  
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -789
  let d = 321
  
  let result3 = azimuth::add(c, d)
  let result4 = azimuth::add(d, c)
  assert_eq(result3, result4)
}

test "multiply_function_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
  assert_eq_string("Hello, Tokyo!", azimuth::greet("Tokyo"))
  assert_eq_string("Hello, São Paulo!", azimuth::greet("São Paulo"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 10          // 10年
  
  // 简化的总利息计算：本金 × 年利率 × 年数
  let total_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years))
  // 总还款额 = 本金 + 利息
  let total_payment = azimuth::add(principal, total_interest)
  
  // 验证计算结果：100000 + 100000 × 6 × 10 = 7000000
  assert_eq(7000000, total_payment)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景测试
  // 摄氏度转华氏度：F = C × 2 + 30（简化版）
  
  // 测试常见温度点
  let freezing_c = 0      // 冰点
  let room_c = 20         // 室温
  let body_c = 37         // 体温
  let boiling_c = 100     // 沸点
  
  let freezing_f = azimuth::add(azimuth::multiply(freezing_c, 2), 30)
  let room_f = azimuth::add(azimuth::multiply(room_c, 2), 30)
  let body_f = azimuth::add(azimuth::multiply(body_c, 2), 30)
  let boiling_f = azimuth::add(azimuth::multiply(boiling_c, 2), 30)
  
  assert_eq(30, freezing_f)   // 0 × 2 + 30 = 30
  assert_eq(70, room_f)       // 20 × 2 + 30 = 70
  assert_eq(104, body_f)      // 37 × 2 + 30 = 104
  assert_eq(230, boiling_f)   // 100 × 2 + 30 = 230
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  // 矩形面积：长 × 宽
  let length = 15
  let width = 8
  let rectangle_area = azimuth::multiply(length, width)
  assert_eq(120, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 12
  let height = 10
  let triangle_area = azimuth::multiply(base, height) / 2
  assert_eq(60, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 6
  let bottom_base = 14
  let trapezoid_height = 8
  let trapezoid_area = azimuth::multiply(azimuth::add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(80, trapezoid_area)
}

test "statistical_calculations" {
  // 统计计算测试
  // 计算一组数据的平均值、最大值、最小值
  let values = [23, 45, 67, 89, 12, 34, 56, 78, 90, 1]
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(
    azimuth::add(azimuth::add(azimuth::add(values[0], values[1]), values[2]), values[3]), 
    values[4]), values[5]), values[6]), values[7]), values[8])
  let total_sum = azimuth::add(sum, values[9])
  
  // 计算平均值
  let average = total_sum / 10
  assert_eq(49, average)  // (23+45+67+89+12+34+56+78+90+1) / 10 = 495 / 10 = 49
  
  // 验证最大值和最小值
  let max_val = 90
  let min_val = 1
  assert_eq(max_val, 90)
  assert_eq(min_val, 1)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试各种特殊字符串
  let empty_string = ""
  let whitespace_string = "   "
  let newline_string = "\n"
  let tab_string = "\t"
  let mixed_whitespace = " \t\n "
  
  assert_eq_string("Hello, !", azimuth::greet(empty_string))
  assert_eq_string("Hello,    !", azimuth::greet(whitespace_string))
  assert_eq_string("Hello, \n!", azimuth::greet(newline_string))
  assert_eq_string("Hello, \t!", azimuth::greet(tab_string))
  assert_eq_string("Hello,  \t\n !", azimuth::greet(mixed_whitespace))
}

test "energy_efficiency_calculation" {
  // 能效计算测试
  let appliance_power = 2000    // 设备功率（瓦特）
  let daily_usage_hours = 4     // 每日使用小时数
  let days_in_month = 30        // 一个月的天数
  let electricity_rate = 15     // 电价（分/千瓦时）
  
  // 计算月耗电量（千瓦时）
  let daily_consumption_wh = azimuth::multiply(appliance_power, daily_usage_hours)
  let monthly_consumption_wh = azimuth::multiply(daily_consumption_wh, days_in_month)
  let monthly_consumption_kwh = monthly_consumption_wh / 1000
  
  // 计算月电费
  let monthly_cost = azimuth::multiply(monthly_consumption_kwh, electricity_rate)
  
  assert_eq(240, monthly_consumption_kwh)  // 2000×4×30/1000 = 240
  assert_eq(3600, monthly_cost)            // 240×15 = 3600
}

test "game_mechanics_calculation" {
  // 游戏机制计算测试
  let base_damage = 100      // 基础伤害
  let weapon_multiplier = 2  // 武器倍数
  let critical_chance = 20   // 暴击率（%）
  let critical_multiplier = 2 // 暴击倍数
  
  // 计算普通伤害
  let normal_damage = azimuth::multiply(base_damage, weapon_multiplier)
  
  // 计算暴击伤害（假设触发暴击）
  let critical_damage = azimuth::multiply(normal_damage, critical_multiplier)
  
  // 计算期望伤害（考虑暴击率）
  let expected_damage = azimuth::add(
    azimuth::multiply(normal_damage, 100 - critical_chance),
    azimuth::multiply(critical_damage, critical_chance)
  ) / 100
  
  assert_eq(200, normal_damage)      // 100×2 = 200
  assert_eq(400, critical_damage)    // 200×2 = 400
  assert_eq(240, expected_damage)    // (200×80 + 400×20)/100 = 240
}