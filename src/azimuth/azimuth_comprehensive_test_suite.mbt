// Azimuth 综合测试套件
// 包含 8 个高质量标准 MoonBit 测试用例

test "basic_arithmetic_validation" {
  // 基本算术验证测试
  @azimuth.assert_eq(7, @azimuth.add(3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-1, @azimuth.subtract(3, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 100
  let multiplier = 3
  let offset = 25
  let divisor = 5
  
  let step1 = @azimuth.multiply(base, multiplier)
  let step2 = @azimuth.add(step1, offset)
  let result = @azimuth.divide_with_ceil(step2, divisor)
  
  @azimuth.assert_eq(300, step1)
  @azimuth.assert_eq(325, step2)
  @azimuth.assert_eq(65, result)
}

test "financial_calculation_simulation" {
  // 金融计算模拟测试
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate1), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, rate2), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  @azimuth.assert_eq(10500, year1_total)
  @azimuth.assert_eq(10815, year2_total)
}

test "temperature_conversion_logic" {
  // 温度转换逻辑测试
  let celsius = 20
  
  let fahrenheit_step1 = @azimuth.multiply(celsius, 9)
  let fahrenheit_step2 = @azimuth.divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = @azimuth.add(fahrenheit_step2, 32)
  
  @azimuth.assert_eq(68, fahrenheit)
}

test "game_mechanics_calculation" {
  // 游戏机制计算测试
  let base_score = 200
  let level_bonus = 50
  let combo_multiplier = 2
  let penalty = 30
  
  let level_score = @azimuth.add(base_score, level_bonus)
  let combo_score = @azimuth.multiply(level_score, combo_multiplier)
  let final_score = @azimuth.subtract(combo_score, penalty)
  
  @azimuth.assert_eq(250, level_score)
  @azimuth.assert_eq(500, combo_score)
  @azimuth.assert_eq(470, final_score)
}

test "inventory_management_logic" {
  // 库存管理逻辑测试
  let initial_stock = 1000
  let daily_sales = 45
  let days = 5
  let new_shipment = 300
  
  let weekly_sales = @azimuth.multiply(daily_sales, days)
  let remaining_stock = @azimuth.subtract(initial_stock, weekly_sales)
  let final_stock = @azimuth.add(remaining_stock, new_shipment)
  
  @azimuth.assert_eq(225, weekly_sales)
  @azimuth.assert_eq(775, remaining_stock)
  @azimuth.assert_eq(1075, final_stock)
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "edge_case_comprehensive_testing" {
  // 边界情况综合测试
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  
  // 负数测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 5))
  
  // 大数测试
  @azimuth.assert_eq(200000, @azimuth.add(100000, 100000))
  @azimuth.assert_eq(33334, @azimuth.divide_with_ceil(100000, 3))
}