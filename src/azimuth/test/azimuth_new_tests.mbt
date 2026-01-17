// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供更多测试场景

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-15, @azimuth.add(-5, -10))
  @azimuth.assert_eq(-50, @azimuth.add(-100, 50))
  @azimuth.assert_eq(0, @azimuth.add(-25, 25))
  @azimuth.assert_eq(-4, @azimuth.add(-7, 3))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(-14, @azimuth.multiply(7, -2))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "mixed_operations" {
  // 测试混合运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(45, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(53, result2)
  
  // a - b * c (模拟减法: a + (-b*c))
  let result3 = @azimuth.add(a, @azimuth.multiply(-b, c))
  @azimuth.assert_eq(-5, result3)
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  @azimuth.assert_eq_string("Hello, @user!", @azimuth.greet("@user"))
  @azimuth.assert_eq_string("Hello, #hashtag!", @azimuth.greet("#hashtag"))
  @azimuth.assert_eq_string("Hello, $price!", @azimuth.greet("$price"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
}

test "mathematical_constants" {
  // 测试数学常数相关计算
  // 圆周率近似计算 (22/7)
  let pi_approx = 22
  let pi_denominator = 7
  let pi_value = @azimuth.multiply(100, pi_approx) / pi_denominator  // 放大100倍
  @azimuth.assert_eq(314, pi_value)
  
  // 黄金比例近似 (1.618)
  let golden_ratio_num = 1618
  let golden_ratio_den = 1000
  let golden_test = @azimuth.multiply(5, golden_ratio_num) / golden_ratio_den
  @azimuth.assert_eq(8, golden_test)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值: (a + b + c) / 3
  let values_sum = @azimuth.add(@azimuth.add(15, 25), 35)
  let average = values_sum / 3
  @azimuth.assert_eq(25, average)
  
  // 计算加权平均: (a*2 + b*3 + c*5) / (2+3+5)
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(10, 2), @azimuth.multiply(20, 3)), @azimuth.multiply(30, 5))
  let weights_total = @azimuth.add(@azimuth.add(2, 3), 5)
  let weighted_average = weighted_sum / weights_total
  @azimuth.assert_eq(23, weighted_average)
}

test "unit_conversion" {
  // 测试单位转换
  // 公里转米: 公里 * 1000
  let kilometers = 5
  let meters = @azimuth.multiply(kilometers, 1000)
  @azimuth.assert_eq(5000, meters)
  
  // 千克转克: 千克 * 1000
  let kilograms = 3
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(3000, grams)
  
  // 升转毫升: 升 * 1000
  let liters = 2
  let milliliters = @azimuth.multiply(liters, 1000)
  @azimuth.assert_eq(2000, milliliters)
}

test "sequential_calculations" {
  // 测试序列计算
  // 计算阶乘: 5! = 5*4*3*2*1 = 120
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算平方和: 1² + 2² + 3² + 4² = 1 + 4 + 9 + 16 = 30
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4))
  @azimuth.assert_eq(30, square_sum)
  
  // 计算立方和: 1³ + 2³ + 3³ = 1 + 8 + 27 = 36
  let cube_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(@azimuth.multiply(1, 1), 1), @azimuth.multiply(@azimuth.multiply(2, 2), 2)), @azimuth.multiply(@azimuth.multiply(3, 3), 3))
  @azimuth.assert_eq(36, cube_sum)
}

test "business_calculations" {
  // 测试商业计算
  // 计算利润: 收入 - 成本
  let revenue = 10000
  let cost = 7000
  let profit = @azimuth.add(revenue, -cost)
  @azimuth.assert_eq(3000, profit)
  
  // 计算税额: 金额 * 税率
  let amount = 5000
  let tax_rate = 20  // 20%
  let tax = @azimuth.multiply(amount, tax_rate) / 100
  @azimuth.assert_eq(1000, tax)
  
  // 计算折扣后价格: 原价 * (1 - 折扣率)
  let original_price = 800
  let discount_rate = 15  // 15%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(680, final_price)
}

test "scientific_calculations" {
  // 测试科学计算
  // 计算动能: 0.5 * 质量 * 速度²
  let mass = 10
  let velocity = 5
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, velocity), velocity) / 2
  @azimuth.assert_eq(125, kinetic_energy)
  
  // 计算势能: 质量 * 重力加速度 * 高度
  let height = 20
  let gravity = 10  // 简化的重力加速度
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  @azimuth.assert_eq(2000, potential_energy)
  
  // 计算功率: 功 / 时间
  let work = 5000
  let time = 10
  let power = work / time
  @azimuth.assert_eq(500, power)
}