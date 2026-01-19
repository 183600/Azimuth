// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(15, @azimuth.add(-5, 20))
  @azimuth.assert_eq(-25, @azimuth.add(-10, -15))
  
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(30, @azimuth.multiply(-6, -5))
}

test "zero_handling" {
  // 测试零值处理
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(-10, @azimuth.add(0, -10))
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  // 修改为较小的乘法结果以避免溢出
  @azimuth.assert_eq(2000000, @azimuth.multiply(large_num1, 2))
  
  let large_num3 = 999999
  let large_num4 = 1
  @azimuth.assert_eq(1000000, @azimuth.add(large_num3, large_num4))
}

test "decimal_simulation" {
  // 模拟小数运算（使用整数表示）
  // 使用放大100倍的方式模拟小数
  let price1 = 1995  // 19.95元
  let price2 = 2999  // 29.99元
  
  let total = @azimuth.add(price1, price2)
  @azimuth.assert_eq(4994, total)  // 49.94元
  
  // 模拟折扣计算
  let discount_rate = 10  // 10%
  let discount_amount = @azimuth.multiply(total, discount_rate) / 100
  let final_price = @azimuth.add(total, -discount_amount)
  @azimuth.assert_eq(4494, final_price)  // 44.94元
}

test "compound_calculations" {
  // 测试复合计算
  let base = 10
  let rate = 5
  
  // 计算复利：principal * (1 + rate)^years
  // 简化计算：逐年计算
  let year1 = @azimuth.add(base, @azimuth.multiply(base, rate) / 100)
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  let year3 = @azimuth.add(year2, @azimuth.multiply(year2, rate) / 100)
  
  @azimuth.assert_eq(1157, year3)  // 近似值
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(10, -average)
  let diff2 = @azimuth.add(20, -average)
  let diff3 = @azimuth.add(30, -average)
  let diff4 = @azimuth.add(40, -average)
  let diff5 = @azimuth.add(50, -average)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), 
    @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), 
    @azimuth.multiply(diff5, diff5)) / count
  
  @azimuth.assert_eq(200, variance)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), 
    @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算时间差
  let start_time = 7200  // 2:00:00
  let end_time = 10800   // 3:00:00
  let duration = @azimuth.add(end_time, -start_time)
  @azimuth.assert_eq(3600, duration)
}

test "string_edge_cases" {
  // 测试字符串边缘情况
  let result1 = @azimuth.greet(" ")
  @azimuth.assert_eq_string("Hello,  !", result1)
  
  let result2 = @azimuth.greet("123")
  @azimuth.assert_eq_string("Hello, 123!", result2)
  
  let result3 = @azimuth.greet("用户测试")
  @azimuth.assert_eq_string("Hello, 用户测试!", result3)
  
  let result4 = @azimuth.greet("!@#$%")
  @azimuth.assert_eq_string("Hello, !@#$%!", result4)
}

test "conversion_calculations" {
  // 测试转换计算
  // 温度转换：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
  
  // 距离转换：公里到英里 1公里 ≈ 0.621371英里
  // 使用整数近似：1公里 = 621/1000英里
  let kilometers = 10
  let miles = @azimuth.multiply(kilometers, 621) / 1000
  @azimuth.assert_eq(6, miles)  // 近似值
  
  // 重量转换：公斤到磅 1公斤 ≈ 2.20462磅
  // 使用整数近似：1公斤 = 2205/1000磅
  let kilograms = 5
  let pounds = @azimuth.multiply(kilograms, 2205) / 1000
  @azimuth.assert_eq(11, pounds)  // 近似值
}

test "game_logic_simulation" {
  // 测试游戏逻辑模拟
  // 模拟简单的RPG战斗计算
  let player_hp = 100
  let player_attack = 20
  let monster_hp = 80
  let monster_defense = 5
  
  // 计算玩家对怪物的伤害
  let damage_to_monster = @azimuth.add(player_attack, -monster_defense)
  let monster_hp_after_hit = @azimuth.add(monster_hp, -damage_to_monster)
  @azimuth.assert_eq(65, monster_hp_after_hit)
  
  // 计算怪物对玩家的伤害
  let monster_attack = 15
  let player_defense = 10
  let damage_to_player = @azimuth.add(monster_attack, -player_defense)
  let player_hp_after_hit = @azimuth.add(player_hp, -damage_to_player)
  @azimuth.assert_eq(95, player_hp_after_hit)
  
  // 计算经验值
  let monster_exp = 30
  let exp_gained = @azimuth.multiply(monster_exp, 2)  // 双倍经验
  @azimuth.assert_eq(60, exp_gained)
}