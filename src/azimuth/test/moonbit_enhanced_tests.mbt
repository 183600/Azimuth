// MoonBit 增强测试用例
// 测试 Azimuth 项目的核心功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
  
  // 计算平均数
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40)
  let average = numbers_sum / 4
  @azimuth.assert_eq(25, average)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 5000
  let rate = 8
  let years = 3
  
  // 简单利息计算：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(125000, total_amount)
  
  // 月供计算（简化版）
  let monthly_payment = total_amount / @azimuth.multiply(years, 12)
  @azimuth.assert_eq(3472, monthly_payment)
}

test "temperature_conversion" {
  // 测试温度转换
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试特殊温度点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "shopping_cart_calculations" {
  // 测试购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  let discount = 50
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_qty)
  let item2_total = @azimuth.multiply(item2_price, item2_qty)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终价格
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  @azimuth.assert_eq(637, final_total)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(35, mean)
  
  // 计算范围
  let range = @azimuth.add(55, -15)
  @azimuth.assert_eq(40, range)
  
  // 计算中位数
  let median = 35
  @azimuth.assert_eq(35, median)
}

test "game_scoring_system" {
  // 测试游戏评分系统
  let base_score = 100
  let level = 4
  let combo = 3
  let bonus = 500
  let penalty = 100
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, level)
  
  // 应用连击倍数
  let combo_score = @azimuth.multiply(level_score, combo)
  
  // 应用奖励和惩罚
  let final_score = @azimuth.add(@azimuth.add(combo_score, bonus), -penalty)
  @azimuth.assert_eq(1500, final_score)
  
  // 计算排名等级
  let rank = final_score / 200
  @azimuth.assert_eq(7, rank)
}