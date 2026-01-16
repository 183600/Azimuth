// 用户综合测试用例
// 使用标准 MoonBit 测试语法

test "arithmetics_series_sum" {
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
}

test "geometry_calculations" {
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "temperature_conversion" {
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
}

test "financial_calculation" {
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(160000, total)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "complex_formula_evaluation" {
  // 复杂公式计算：(a + b) × (c - d) + e × f
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product_ef = @azimuth.multiply(e, f)
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  @azimuth.assert_eq(271, result)
}

test "statistical_calculations" {
  // 统计计算：平均值和中位数
  let values = [10, 20, 30, 40, 50]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 中位数（已排序数组）
  let median = 30
  @azimuth.assert_eq(30, median)
}

test "game_mechanics" {
  // 游戏机制计算：得分系统
  let base_score = 1000
  let level_bonus = 200
  let combo_multiplier = 3
  let penalty = 150
  
  let final_score = @azimuth.add(
    @azimuth.multiply(@azimuth.add(base_score, level_bonus), combo_multiplier),
    -penalty
  )
  
  @azimuth.assert_eq(3450, final_score)
}

test "energy_consumption" {
  // 能耗计算：电器使用成本
  let power_rating = 100  // 瓦特
  let hours_per_day = 8
  let days_per_month = 30
  let cost_per_kwh = 10  // 每千瓦时费用（简化）
  
  // 计算月度能耗（千瓦时）
  let monthly_kwh = @azimuth.multiply(
    @azimuth.multiply(power_rating, hours_per_day),
    days_per_month
  ) / 1000
  
  @azimuth.assert_eq(24, monthly_kwh)
  
  // 计算月度费用
  let monthly_cost = @azimuth.multiply(monthly_kwh, cost_per_kwh)
  @azimuth.assert_eq(240, monthly_cost)
}