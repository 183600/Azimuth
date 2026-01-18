// 用户添加的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))  // 10/4 = 2.5 -> 3
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))  // 20/4 = 5 -> 5
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))   // 1/2 = 0.5 -> 1
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))   // 0/5 = 0 -> 0
}

test "complex_calculations_with_divide_ceil" {
  // 测试包含向上取整除法的复杂计算
  // 场景：计算需要的包装箱数量（每个箱子最多装10个物品）
  let items1 = 25
  let items2 = 30
  let items3 = 7
  
  let boxes1 = @azimuth.divide_with_ceil(items1, 10)  // 25/10 = 2.5 -> 3
  let boxes2 = @azimuth.divide_with_ceil(items2, 10)  // 30/10 = 3 -> 3
  let boxes3 = @azimuth.divide_with_ceil(items3, 10)  // 7/10 = 0.7 -> 1
  
  let total_boxes = @azimuth.add(@azimuth.add(boxes1, boxes2), boxes3)
  @azimuth.assert_eq(7, total_boxes)
}

test "mathematical_series_summation" {
  // 测试数学数列求和
  // 计算等差数列：2 + 5 + 8 + 11 + 14 + 17 + 20
  let term1 = 2
  let diff = 3
  let terms = 7
  
  // 使用公式：n/2 * (2a + (n-1)d)
  let sum = @azimuth.multiply(terms, @azimuth.add(@azimuth.multiply(2, term1), @azimuth.multiply(terms - 1, diff))) / 2
  @azimuth.assert_eq(77, sum)
}

test "temperature_conversion_formula" {
  // 测试温度转换公式（更精确的版本）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 使用整数近似：F = C * 2 - C/5 + 32
  
  let celsius1 = 0   // 冰点
  let fahrenheit1 = @azimuth.add(@azimuth.add(@azimuth.multiply(celsius1, 2), -celsius1/5), 32)
  @azimuth.assert_eq(32, fahrenheit1)
  
  let celsius2 = 100 // 沸点
  let fahrenheit2 = @azimuth.add(@azimuth.add(@azimuth.multiply(celsius2, 2), -celsius2/5), 32)
  @azimuth.assert_eq(212, fahrenheit2)
  
  let celsius3 = 25  // 室温
  let fahrenheit3 = @azimuth.add(@azimuth.add(@azimuth.multiply(celsius3, 2), -celsius3/5), 32)
  @azimuth.assert_eq(77, fahrenheit3)
}

test "percentage_calculations_advanced" {
  // 测试高级百分比计算
  // 场景：计算折扣后的价格
  let original_price = 1000
  let discount1 = 10  // 10% 折扣
  let discount2 = 20  // 20% 折扣
  let tax = 8         // 8% 税费
  
  // 第一次折扣
  let discounted_price1 = @azimuth.multiply(original_price, 100 - discount1) / 100
  // 第二次折扣
  let discounted_price2 = @azimuth.multiply(discounted_price1, 100 - discount2) / 100
  // 加税
  let final_price = @azimuth.multiply(discounted_price2, 100 + tax) / 100
  
  @azimuth.assert_eq(777, final_price)  // 1000 * 0.9 * 0.8 * 1.08 = 777.6 -> 777
}

test "geometric_progression_sum" {
  // 测试等比数列求和
  // 计算：2 + 4 + 8 + 16 + 32 + 64
  let first_term = 2
  let ratio = 2
  let terms = 6
  
  // 使用公式：a * (r^n - 1) / (r - 1)
  let power = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2))  // 2^6 = 64
  let sum = @azimuth.multiply(first_term, @azimuth.add(power, -1)) / @azimuth.add(ratio, -1)
  @azimuth.assert_eq(126, sum)
}

test "business_profit_calculation" {
  // 测试商业利润计算
  // 场景：计算季度利润
  let revenue_q1 = 50000
  let revenue_q2 = 60000
  let revenue_q3 = 55000
  let revenue_q4 = 70000
  
  let cost_q1 = 30000
  let cost_q2 = 35000
  let cost_q3 = 32000
  let cost_q4 = 40000
  
  // 计算总收入
  let total_revenue = @azimuth.add(@azimuth.add(@azimuth.add(revenue_q1, revenue_q2), revenue_q3), revenue_q4)
  // 计算总成本
  let total_cost = @azimuth.add(@azimuth.add(@azimuth.add(cost_q1, cost_q2), cost_q3), cost_q4)
  // 计算总利润
  let total_profit = @azimuth.add(total_revenue, -total_cost)
  
  @azimuth.assert_eq(98000, total_profit)
}

test "time_zone_conversion" {
  // 测试时区转换
  // 场景：从北京时间转换为纽约时间（假设北京比纽约快12小时）
  let beijing_time = 1400  // 下午2点（24小时制）
  let time_difference = 12
  
  // 转换为纽约时间
  let new_york_time = @azimuth.add(beijing_time, -time_difference)
  
  // 处理跨日情况（如果结果是负数，则加上24小时）
  let adjusted_time = if (new_york_time < 0) {
    @azimuth.add(new_york_time, 24)
  } else {
    new_york_time
  }
  
  @azimuth.assert_eq(200, adjusted_time)  // 1400 - 1200 = 200（凌晨2点）
}

test "loan_repayment_calculation" {
  // 测试贷款还款计算
  // 场景：计算等额本息还款（简化版）
  let loan_amount = 100000  // 贷款金额
  let annual_rate = 6       // 年利率6%
  let years = 10            // 贷款年限
  
  // 简化计算：总还款额 = 本金 + 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(loan_amount, @azimuth.multiply(annual_rate, years)) / 100
  let total_repayment = @azimuth.add(loan_amount, total_interest)
  
  // 月还款额（简化）
  let monthly_payment = total_repayment / @azimuth.multiply(years, 12)
  
  @azimuth.assert_eq(160000, total_repayment)  // 100000 + 100000*6*10/100 = 160000
  @azimuth.assert_eq(1333, monthly_payment)     // 160000 / 120 = 1333.33 -> 1333
}

test "inventory_management" {
  // 测试库存管理计算
  // 场景：计算库存周转率
  let initial_stock = 1000
  let purchases = 500
  let sales = 800
  let final_stock = @azimuth.add(@azimuth.add(initial_stock, purchases), -sales)
  
  // 计算库存周转率（销售量 / 平均库存）
  let average_stock = @azimuth.add(initial_stock, final_stock) / 2
  let turnover_rate = @azimuth.multiply(sales, 100) / average_stock
  
  @azimuth.assert_eq(700, final_stock)  // 1000 + 500 - 800 = 700
  @azimuth.assert_eq(93, turnover_rate) // 800 * 100 / 850 = 94.11 -> 94
}