// 额外的 MoonBit 测试用例
// 这些测试用例补充了现有的测试，覆盖更多场景

test "mathematical_series_summation" {
  // 数学级数求和测试：等差数列和等比数列
  // 等差数列：1, 3, 5, 7, 9 的和
  let arithmetic_sum = @azimuth.add(1, @azimuth.add(3, @azimuth.add(5, @azimuth.add(7, 9))))
  @azimuth.assert_eq(25, arithmetic_sum)
  
  // 等比数列：1, 2, 4, 8, 16 的和（使用乘法和加法）
  let geometric_sum = @azimuth.add(1, @azimuth.add(2, @azimuth.add(4, @azimuth.add(8, 16))))
  @azimuth.assert_eq(31, geometric_sum)
  
  // 平方数列：1², 2², 3², 4², 5² 的和
  let square_sum = @azimuth.add(1, @azimuth.add(4, @azimuth.add(9, @azimuth.add(16, 25))))
  @azimuth.assert_eq(55, square_sum)
}

test "temperature_conversion" {
  // 温度转换测试：摄氏度与华氏度之间的转换
  // 摄氏度到华氏度：F = C × 9/5 + 32
  let celsius = 100
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(212, fahrenheit)
  
  // 华氏度到摄氏度：C = (F - 32) × 5/9
  let fahrenheit2 = 32
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  @azimuth.assert_eq(0, celsius2)
  
  // 冰点测试
  let celsius3 = 0
  let fahrenheit3 = @azimuth.add(@azimuth.multiply(celsius3, 9) / 5, 32)
  @azimuth.assert_eq(32, fahrenheit3)
}

test "time_calculations" {
  // 时间计算测试：小时、分钟、秒之间的转换
  // 2小时30分钟45秒转换为总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  @azimuth.assert_eq(9045, total_seconds)
  
  // 5000秒转换为小时、分钟、秒
  let total_seconds2 = 5000
  let hours2 = total_seconds2 / 3600
  let remaining_seconds = @azimuth.add(total_seconds2, -@azimuth.multiply(hours2, 3600))
  let minutes2 = remaining_seconds / 60
  let seconds2 = @azimuth.add(remaining_seconds, -@azimuth.multiply(minutes2, 60))
  
  @azimuth.assert_eq(1, hours2)      // 1小时
  @azimuth.assert_eq(23, minutes2)   // 23分钟
  @azimuth.assert_eq(20, seconds2)   // 20秒
}

test "geometric_calculations" {
  // 几何计算测试：面积和周长计算
  // 矩形面积和周长
  let length = 8
  let width = 5
  let rectangle_area = @azimuth.multiply(length, width)
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(40, rectangle_area)
  @azimuth.assert_eq(26, rectangle_perimeter)
  
  // 正方形面积和周长
  let side = 6
  let square_area = @azimuth.multiply(side, side)
  let square_perimeter = @azimuth.multiply(4, side)
  
  @azimuth.assert_eq(36, square_area)
  @azimuth.assert_eq(24, square_perimeter)
  
  // 三角形面积（近似计算：底 × 高 ÷ 2）
  let base = 10
  let height = 8
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(40, triangle_area)
}

test "statistical_calculations" {
  // 统计计算测试：平均值、中位数、众数
  // 数据集：5, 8, 12, 15, 20
  let data1 = 5
  let data2 = 8
  let data3 = 12
  let data4 = 15
  let data5 = 20
  
  // 平均值计算
  let sum = @azimuth.add(data1, @azimuth.add(data2, @azimuth.add(data3, @azimuth.add(data4, data5))))
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(12, mean)
  
  // 中位数（已排序数据集的中间值）
  let median = data3  // 第三个值
  @azimuth.assert_eq(12, median)
  
  // 极差计算
  let range = @azimuth.add(data5, -data1)
  @azimuth.assert_eq(15, range)
}

test "business_calculations" {
  // 商业计算测试：利润、折扣、税费计算
  // 成本、售价和利润计算
  let cost_price = 150
  let selling_price = 200
  let profit = @azimuth.add(selling_price, -cost_price)
  let profit_margin = @azimuth.multiply(profit, 100) / selling_price
  
  @azimuth.assert_eq(50, profit)
  @azimuth.assert_eq(25, profit_margin)  // 25% 利润率
  
  // 折扣计算
  let original_price = 300
  let discount_percentage = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(45, discount_amount)
  @azimuth.assert_eq(255, discounted_price)
  
  // 税费计算
  let price_before_tax = 400
  let tax_rate = 8  // 8%
  let tax_amount = @azimuth.multiply(price_before_tax, tax_rate) / 100
  let price_after_tax = @azimuth.add(price_before_tax, tax_amount)
  
  @azimuth.assert_eq(32, tax_amount)
  @azimuth.assert_eq(432, price_after_tax)
}

test "physics_calculations" {
  // 物理计算测试：速度、加速度、距离计算
  // 基本运动学计算：s = ut + (1/2)at²
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 距离 = 初速度 × 时间 + (1/2) × 加速度 × 时间²
  let distance = @azimuth.add(@azimuth.multiply(initial_velocity, time), @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2)
  @azimuth.assert_eq(75, distance)
  
  // 最终速度：v = u + at
  let final_velocity = @azimuth.add(initial_velocity, @azimuth.multiply(acceleration, time))
  @azimuth.assert_eq(20, final_velocity)
  
  // 动能计算：KE = (1/2)mv²
  let mass = 10  // kg
  let velocity = 15  // m/s
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(1125, kinetic_energy)
}

test "game_logic_simulation" {
  // 游戏逻辑模拟测试：简单的回合制游戏
  // 玩家属性
  let player_health = 100
  let player_attack = 20
  let player_defense = 10
  
  // 敌人属性
  let enemy_health = 80
  let enemy_attack = 15
  let enemy_defense = 5
  
  // 第一回合：玩家攻击敌人
  // 伤害 = 攻击力 - 防御力
  let player_damage = @azimuth.add(player_attack, -enemy_defense)
  let enemy_health_after_player_attack = @azimuth.add(enemy_health, -player_damage)
  @azimuth.assert_eq(65, enemy_health_after_player_attack)
  
  // 第二回合：敌人攻击玩家
  let enemy_damage = @azimuth.add(enemy_attack, -player_defense)
  let player_health_after_enemy_attack = @azimuth.add(player_health, -enemy_damage)
  @azimuth.assert_eq(95, player_health_after_enemy_attack)
  
  // 第三回合：玩家使用特殊攻击（伤害 × 2）
  let special_damage = @azimuth.multiply(player_damage, 2)
  let enemy_health_after_special = @azimuth.add(enemy_health_after_player_attack, -special_damage)
  @azimuth.assert_eq(45, enemy_health_after_special)
  
  // 治疗效果：恢复 20 点生命值
  let heal_amount = 20
  let player_health_after_heal = @azimuth.add(player_health_after_enemy_attack, heal_amount)
  @azimuth.assert_eq(115, player_health_after_heal)
}

test "string_concatenation_simulation" {
  // 字符串连接模拟测试（通过 greet 函数）
  // 模拟字符串连接的效果
  let first_name = "John"
  let last_name = "Doe"
  
  // 通过 greet 函数模拟字符串处理
  let first_greeting = @azimuth.greet(first_name)
  let last_greeting = @azimuth.greet(last_name)
  
  @azimuth.assert_eq_string("Hello, John!", first_greeting)
  @azimuth.assert_eq_string("Hello, Doe!", last_greeting)
  
  // 测试空字符串处理
  let empty_string = ""
  let empty_greeting = @azimuth.greet(empty_string)
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
  
  // 测试长字符串处理
  let long_string = "This is a very long string with many words"
  let long_greeting = @azimuth.greet(long_string)
  @azimuth.assert_eq_string("Hello, This is a very long string with many words!", long_greeting)
  
  // 测试包含特殊字符的字符串
  let special_chars = "Hello\nWorld\t!"
  let special_greeting = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, Hello\nWorld\t!!", special_greeting)
}

test "algorithm_optimization_test" {
  // 算法优化测试：比较不同算法的效率
  // 线性搜索 vs 二分搜索（模拟）
  
  // 数据集（已排序）：1, 5, 10, 15, 20, 25, 30, 35, 40, 45
  let target = 25
  
  // 线性搜索（模拟）：最多需要检查所有元素
  let data_size = 10
  let linear_search_steps = data_size  // 最坏情况
  @azimuth.assert_eq(10, linear_search_steps)
  
  // 二分搜索（模拟）：log₂(n) 步骤
  // 第一步：检查中间元素（第5个：20）
  // 第二步：检查右半部分中间（第8个：35）
  // 第三步：检查左半部分中间（第6个：25）- 找到目标
  let binary_search_steps = 3
  @azimuth.assert_eq(3, binary_search_steps)
  
  // 效率比较
  let efficiency_improvement = @azimuth.add(linear_search_steps, -binary_search_steps)
  @azimuth.assert_eq(7, efficiency_improvement)
  
  // 百分比改进
  let percentage_improvement = @azimuth.multiply(efficiency_improvement, 100) / linear_search_steps
  @azimuth.assert_eq(70, percentage_improvement)  // 70% 的改进
}