// Azimuth 项目新的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let sum = azimuth.add(a, b)
  @assertion.assert_eq(22, sum)?
  
  // 测试乘法
  let product = azimuth.multiply(a, b)
  @assertion.assert_eq(105, product)?
  
  // 测试复合运算
  let result = azimuth.add(azimuth.multiply(a, 2), azimuth.multiply(b, 3))
  @assertion.assert_eq(51, result)?
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  let names = ["Alice", "Bob", "张三", "李四", "John"]
  let expected = ["Hello, Alice!", "Hello, Bob!", "Hello, 张三!", "Hello, 李四!", "Hello, John!"]
  
  // 测试各种姓名的问候
  let result1 = azimuth.greet(names[0])
  @assertion.assert_eq_string(expected[0], result1)?
  
  let result2 = azimuth.greet(names[1])
  @assertion.assert_eq_string(expected[1], result2)?
  
  let result3 = azimuth.greet(names[2])
  @assertion.assert_eq_string(expected[2], result3)?
  
  let result4 = azimuth.greet(names[3])
  @assertion.assert_eq_string(expected[3], result4)?
  
  let result5 = azimuth.greet(names[4])
  @assertion.assert_eq_string(expected[4], result5)?
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  
  // 等差数列前n项和: n/2 * (2a1 + (n-1)d)
  let n = 5
  let a1 = 3  // 首项
  let d = 2   // 公差
  
  // 计算前5项: 3 + 5 + 7 + 9 + 11 = 35
  let sum = azimuth.multiply(
    azimuth.divide(n, 2),
    azimuth.add(
      azimuth.multiply(2, a1),
      azimuth.multiply(azimuth.add(n, -1), d)
    )
  )
  @assertion.assert_eq(35, sum)?
  
  // 等比数列前n项和: a1 * (1 - r^n) / (1 - r)
  let r = 2  // 公比
  // 计算: 3 * (1 - 2^5) / (1 - 2) = 3 * (-31) / (-1) = 93
  let geo_sum = azimuth.multiply(
    a1,
    azimuth.divide(
      azimuth.add(1, azimuth.multiply(-1, azimuth.multiply(2, 2, 2, 2, 2))),
      azimuth.add(1, -r)
    )
  )
  @assertion.assert_eq(93, geo_sum)?
}

test "financial_calculations" {
  // 金融计算测试
  
  // 复利计算: A = P * (1 + r/n)^(n*t)
  let principal = 1000  // 本金
  let rate = 5           // 年利率（百分比）
  let times = 12         // 每年计息次数
  let years = 2          // 年数
  
  // 简化版复利计算: A = P * (1 + r)^(t)
  let amount = azimuth.multiply(
    principal,
    azimuth.add(1, azimuth.multiply(rate, years) / 100)
  )
  @assertion.assert_eq(1100, amount)?
  
  // 折扣计算
  let original_price = 500
  let discount_percent = 15
  let discount_amount = azimuth.multiply(original_price, discount_percent) / 100
  let final_price = azimuth.add(original_price, -discount_amount)
  @assertion.assert_eq(425, final_price)?
}

test "geometry_calculations" {
  // 几何计算测试
  
  // 计算立方体表面积: 6 * a^2
  let cube_side = 4
  let cube_surface = azimuth.multiply(6, azimuth.multiply(cube_side, cube_side))
  @assertion.assert_eq(96, cube_surface)?
  
  // 计算立方体体积: a^3
  let cube_volume = azimuth.multiply(cube_side, azimuth.multiply(cube_side, cube_side))
  @assertion.assert_eq(64, cube_volume)?
  
  // 计算圆柱体表面积（简化）: 2πr(h+r) ≈ 2 * 3 * r * (h+r)
  let cylinder_radius = 3
  let cylinder_height = 10
  let cylinder_surface = azimuth.multiply(
    azimuth.multiply(2, 3),
    azimuth.multiply(
      cylinder_radius,
      azimuth.add(cylinder_height, cylinder_radius)
    )
  )
  @assertion.assert_eq(234, cylinder_surface)?
}

test "physics_calculations" {
  // 物理计算测试
  
  // 自由落体距离: s = 1/2 * g * t^2
  let gravity = 10  // 重力加速度（简化值）
  let time = 3       // 时间（秒）
  let distance = azimuth.multiply(
    azimuth.multiply(gravity, time, time),
    1
  ) / 2
  @assertion.assert_eq(45, distance)?
  
  // 动能计算: E = 1/2 * m * v^2
  let mass = 10      // 质量
  let velocity = 5   // 速度
  let kinetic_energy = azimuth.multiply(
    azimuth.multiply(mass, azimuth.multiply(velocity, velocity)),
    1
  ) / 2
  @assertion.assert_eq(125, kinetic_energy)?
  
  // 功率计算: P = W / t
  let work = 1000    // 功
  let time2 = 10     // 时间
  let power = azimuth.divide(work, time2)
  @assertion.assert_eq(100, power)?
}

test "data_analysis" {
  // 数据分析测试
  let data = [12, 15, 18, 21, 24, 27, 30]
  
  // 计算平均值
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(data[0], data[1]), data[2]), data[3]), data[4]), data[5]), data[6])
  let mean = azimuth.divide(sum, azimuth.multiply(7, 1))
  @assertion.assert_eq(21, mean)?
  
  // 计算中位数
  let median = data[3]  // 已排序数组的中间值
  @assertion.assert_eq(21, median)?
  
  // 计算范围
  let range = azimuth.add(data[6], -data[0])
  @assertion.assert_eq(18, range)?
}

test "game_mechanics" {
  // 游戏机制测试
  
  // 经验值计算
  let base_exp = 100
  let level = 5
  let difficulty_multiplier = 2
  let bonus = 50
  
  let total_exp = azimuth.add(
    azimuth.multiply(
      azimuth.multiply(base_exp, level),
      difficulty_multiplier
    ),
    bonus
  )
  @assertion.assert_eq(1050, total_exp)?
  
  // 伤害计算
  let base_damage = 50
  let weapon_bonus = 20
  let critical_multiplier = 2
  let defense = 15
  
  let damage = azimuth.multiply(
    azimuth.add(base_damage, weapon_bonus),
    critical_multiplier
  )
  let final_damage = azimuth.add(damage, -defense)
  @assertion.assert_eq(125, final_damage)?
}

test "time_zone_calculations" {
  // 时区计算测试
  
  // 北京时间（UTC+8）转纽约时间（UTC-5）
  let beijing_hour = 20
  let time_difference = 13  // 8 - (-5) = 13
  
  let new_york_hour = azimuth.add(beijing_hour, -time_difference)
  @assertion.assert_eq(7, new_york_hour)?
  
  // 计算跨越国际日期变更线的情况
  let beijing_hour2 = 6
  let new_york_hour2 = azimuth.add(beijing_hour2, -time_difference)
  @assertion.assert_eq(-7, new_york_hour2)?
  
  // 计算24小时制转换
  let adjusted_hour2 = azimuth.add(new_york_hour2, 24)
  @assertion.assert_eq(17, adjusted_hour2)?
}

test "boundary_conditions" {
  // 边界条件测试
  
  // 零值运算
  let zero_add = azimuth.add(0, 42)
  @assertion.assert_eq(42, zero_add)?
  
  let zero_multiply = azimuth.multiply(42, 0)
  @assertion.assert_eq(0, zero_multiply)?
  
  // 负数运算
  let neg1 = -10
  let neg2 = -5
  let neg_add = azimuth.add(neg1, neg2)
  @assertion.assert_eq(-15, neg_add)?
  
  let neg_multiply = azimuth.multiply(neg1, neg2)
  @assertion.assert_eq(50, neg_multiply)?
  
  // 混合符号运算
  let mixed_add = azimuth.add(neg1, 15)
  @assertion.assert_eq(5, mixed_add)?
  
  let mixed_multiply = azimuth.multiply(neg1, 5)
  @assertion.assert_eq(-50, mixed_multiply)?
}