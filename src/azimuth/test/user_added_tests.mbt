// 用户添加的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "advanced_arithmetic_combinations" {
  // 测试高级算术组合：计算 (a+b)*(c-d) + e*f
  let a = 15
  let b = 25
  let c = 40
  let d = 30
  let e = 8
  let f = 12
  
  let sum_ab = @azimuth.add(a, b)  // 40
  let diff_cd = @azimuth.add(c, -d)  // 10
  let product_ef = @azimuth.multiply(e, f)  // 96
  let product_sum_diff = @azimuth.multiply(sum_ab, diff_cd)  // 400
  let result = @azimuth.add(product_sum_diff, product_ef)  // 496
  
  @azimuth.assert_eq(496, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c !", @azimuth.greet("a b c "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算：计算等比数列前n项和
  // 2 + 4 + 8 + 16 + 32 = 62
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)  // 4
  let term3 = @azimuth.multiply(term2, 2)  // 8
  let term4 = @azimuth.multiply(term3, 2)  // 16
  let term5 = @azimuth.multiply(term4, 2)  // 32
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(62, sum)
}

test "financial_calculation_compound" {
  // 测试复合金融计算：计算贷款月供
  // 简化计算：月供 = (本金 + 总利息) / 月数
  let principal = 120000  // 本金
  let annual_rate = 6  // 年利率6%
  let years = 30  // 30年
  let months = @azimuth.multiply(years, 12)  // 总月数
  
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / months
  
  @azimuth.assert_eq(2500, monthly_payment)
}

test "geometry_area_perimeter" {
  // 测试几何计算：计算梯形面积和周长
  let top_base = 10
  let bottom_base = 20
  let height = 8
  let left_side = 10
  let right_side = 10
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(120, area)
  
  // 梯形周长 = 上底 + 下底 + 左边 + 右边
  let perimeter = @azimuth.add(@azimuth.add(@azimuth.add(top_base, bottom_base), left_side), right_side)
  @azimuth.assert_eq(50, perimeter)
}

test "data_analysis_statistics" {
  // 测试数据分析统计计算
  // 数据集：15, 25, 35, 45, 55
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(35, average)
  
  // 计算方差（简化版）：(x1-avg)² + (x2-avg)² + ... / n
  let diff1 = @azimuth.add(data1, -average)
  let diff2 = @azimuth.add(data2, -average)
  let diff3 = @azimuth.add(data3, -average)
  let diff4 = @azimuth.add(data4, -average)
  let diff5 = @azimuth.add(data5, -average)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1), 
    @azimuth.multiply(diff2, diff2)), 
    @azimuth.multiply(diff3, diff3)), 
    @azimuth.multiply(diff4, diff4)), 
    @azimuth.multiply(diff5, diff5)) / 5
  
  @azimuth.assert_eq(200, variance)
}

test "physics_kinematics" {
  // 测试物理运动学计算
  // 计算自由落体距离：s = v₀t + ½gt²
  // 简化计算：s = v₀t + gt²/2，取g=10m/s²
  let initial_velocity = 5  // 初速度5m/s
  let time = 4  // 时间4秒
  let gravity = 10  // 重力加速度10m/s²
  
  let distance_with_initial = @azimuth.multiply(initial_velocity, time)  // 20
  let distance_from_gravity = @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 2  // 80
  let total_distance = @azimuth.add(distance_with_initial, distance_from_gravity)
  
  @azimuth.assert_eq(100, total_distance)
}

test "game_logic_scoring" {
  // 测试游戏逻辑计分
  let base_score = 1000
  let level = 5
  let combo_multiplier = 3
  let bonus_items = 8
  let penalty = 200
  
  // 基础分数 × 等级系数
  let level_score = @azimuth.multiply(base_score, level)
  // 应用连击倍数
  let combo_score = @azimuth.multiply(level_score, combo_multiplier)
  // 添加奖励物品分数
  let bonus_score = @azimuth.add(combo_score, @azimuth.multiply(bonus_items, 50))
  // 减去惩罚分数
  let final_score = @azimuth.add(bonus_score, -penalty)
  
  @azimuth.assert_eq(15300, final_score)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：模拟O(n²)算法的运算次数
  let n = 10
  
  // 计算 1 + 2 + 3 + ... + n = n(n+1)/2
  let operations = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, operations)
  
  // 模拟嵌套循环的总运算次数：n²
  let nested_operations = @azimuth.multiply(n, n)
  @azimuth.assert_eq(100, nested_operations)
  
  // 模拟三分法算法的运算次数：log₂(n) 简化为 n/2
  let binary_ops = n / 2
  @azimuth.assert_eq(5, binary_ops)
}

test "unit_conversion_systems" {
  // 测试单位转换系统
  // 长度转换：1英寸 = 2.54厘米，简化为1英寸=2.5厘米
  let inches = 12
  let centimeters = @azimuth.multiply(inches, 25) / 10
  @azimuth.assert_eq(30, centimeters)
  
  // 重量转换：1磅 = 0.45千克，简化为1磅=0.5千克
  let pounds = 10
  let kilograms = pounds / 2
  @azimuth.assert_eq(5, kilograms)
  
  // 温度转换：华氏度转摄氏度 C = (F-32)×5/9，简化为 C = (F-30)/2
  let fahrenheit = 86
  let celsius = @azimuth.add(fahrenheit, -30) / 2
  @azimuth.assert_eq(28, celsius)
}