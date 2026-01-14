// MoonBit 综合测试用例
// 覆盖 azimuth 库的核心功能和实际应用场景

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：2 + 4 + 6 + ... + 20（前10个偶数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 2
  let last = 20
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(110, sum)
}

test "string_multilingual_support" {
  // 测试多语言字符串支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, القاهرة!", @azimuth.greet("القاهرة"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期还款计算
  let loan_amount = 100000
  let annual_rate = 6  // 6%
  let loan_years = 10
  
  // 简化的月供计算：本金 / 月数 + 利息
  let total_months = @azimuth.multiply(loan_years, 12)
  let monthly_principal = loan_amount / total_months
  
  // 简化的利息计算
  let total_interest = @azimuth.multiply(loan_amount, annual_rate) / 100
  let monthly_interest = total_interest / total_months
  
  let monthly_payment = @azimuth.add(monthly_principal, monthly_interest)
  @azimuth.assert_eq(1388, monthly_payment)
}

test "temperature_conversion_comprehensive" {
  // 温度转换综合测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30
  
  let freezing_c = 0
  let boiling_c = 100
  let room_c = 22
  
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  
  @azimuth.assert_eq(30, freezing_f)
  @azimuth.assert_eq(230, boiling_f)
  @azimuth.assert_eq(74, room_f)
}

test "geometric_area_calculations" {
  // 几何面积计算
  let rectangle_length = 15
  let rectangle_width = 8
  let triangle_base = 12
  let triangle_height = 6
  let trapezoid_base1 = 10
  let trapezoid_base2 = 6
  let trapezoid_height = 4
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let trapezoid_area = @azimuth.multiply(@azimuth.add(trapezoid_base1, trapezoid_base2), trapezoid_height) / 2
  
  @azimuth.assert_eq(120, rectangle_area)
  @azimuth.assert_eq(36, triangle_area)
  @azimuth.assert_eq(32, trapezoid_area)
}

test "inventory_management_system" {
  // 库存管理系统计算
  let product_cost = 50
  let selling_price = 75
  let initial_stock = 100
  let sold_quantity = 30
  let reorder_threshold = 40
  
  // 计算当前库存
  let current_stock = @azimuth.add(initial_stock, -sold_quantity)
  
  // 计算总销售额
  let total_revenue = @azimuth.multiply(selling_price, sold_quantity)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(product_cost, sold_quantity)
  
  // 计算毛利润
  let gross_profit = @azimuth.add(total_revenue, -total_cost)
  
  // 判断是否需要补货
  let needs_reorder = current_stock < reorder_threshold
  
  @azimuth.assert_eq(70, current_stock)
  @azimuth.assert_eq(2250, total_revenue)
  @azimuth.assert_eq(1500, total_cost)
  @azimuth.assert_eq(750, gross_profit)
  @azimuth.assert_true(needs_reorder)
}

test "statistical_analysis_basic" {
  // 基础统计分析
  let data1 = 85
  let data2 = 92
  let data3 = 78
  let data4 = 96
  let data5 = 89
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  
  // 计算平均值
  let average = sum / 5
  
  // 计算最大值和最小值
  let max_value = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let min_value = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  
  @azimuth.assert_eq(440, sum)
  @azimuth.assert_eq(88, average)
}

test "game_mechanics_calculation" {
  // 游戏机制计算
  let base_damage = 100
  let weapon_multiplier = 2
  let critical_chance = 25
  let enemy_armor = 30
  
  // 计算基础攻击伤害
  let weapon_damage = @azimuth.multiply(base_damage, weapon_multiplier)
  
  // 模拟暴击（25%概率）
  let critical_damage = if (weapon_damage > 150) {
    @azimuth.multiply(weapon_damage, 2)
  } else {
    weapon_damage
  }
  
  // 计算最终伤害（减去护甲）
  let final_damage = @azimuth.add(critical_damage, -enemy_armor)
  
  @azimuth.assert_eq(170, final_damage)
}

test "energy_efficiency_calculation" {
  // 能效计算
  let old_appliance_power = 2000  // 瓦特
  let new_appliance_power = 1500  // 瓦特
  let daily_usage_hours = 8
  let electricity_rate = 15  // 每千瓦时费用
  
  // 计算日耗电量（千瓦时）
  let old_daily_kwh = @azimuth.multiply(old_appliance_power, daily_usage_hours) / 1000
  let new_daily_kwh = @azimuth.multiply(new_appliance_power, daily_usage_hours) / 1000
  
  // 计算日成本
  let old_daily_cost = @azimuth.multiply(old_daily_kwh, electricity_rate)
  let new_daily_cost = @azimuth.multiply(new_daily_kwh, electricity_rate)
  
  // 计算年节省
  let daily_savings = @azimuth.add(old_daily_cost, -new_daily_cost)
  let annual_savings = @azimuth.multiply(daily_savings, 365)
  
  @azimuth.assert_eq(16, old_daily_kwh)
  @azimuth.assert_eq(12, new_daily_kwh)
  @azimuth.assert_eq(240, annual_savings)
}

test "complex_number_pattern" {
  // 复数模式计算（使用整数模拟）
  let real_part = 3
  let imaginary_part = 4
  
  // 计算模的平方：a² + b²
  let magnitude_squared = @azimuth.add(@azimuth.multiply(real_part, real_part), 
                                      @azimuth.multiply(imaginary_part, imaginary_part))
  
  // 计算共轭复数的乘积：(a + bi)(a - bi) = a² + b²
  let conjugate_product = magnitude_squared
  
  // 模拟复数加法：(a + bi) + (c + di) = (a + c) + (b + d)i
  let other_real = 2
  let other_imaginary = 1
  let sum_real = @azimuth.add(real_part, other_real)
  let sum_imaginary = @azimuth.add(imaginary_part, other_imaginary)
  
  @azimuth.assert_eq(25, magnitude_squared)
  @azimuth.assert_eq(25, conjugate_product)
  @azimuth.assert_eq(5, sum_real)
  @azimuth.assert_eq(5, sum_imaginary)
}