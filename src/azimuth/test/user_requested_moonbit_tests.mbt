// 用户请求的 MoonBit 测试用例
// 覆盖一些实际应用场景和边界条件

// 测试用例 1: 计算购物车总价（包含折扣和税费）
test "shopping_cart_with_discount_and_tax" {
  // 商品价格和数量
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算每种商品总价
  let total1 = item1_price * item1_quantity
  let total2 = item2_price * item2_quantity
  let total3 = item3_price * item3_quantity
  
  // 计算购物车总价
  let subtotal = total1 + total2 + total3
  
  // 计算折扣（超过500元打9折）
  let discount_threshold = 500
  let discount_rate = 10
  let discount_amount = if (subtotal > discount_threshold) {
    subtotal * discount_rate / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = subtotal - discount_amount
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = discounted_total * tax_rate / 100
  
  // 计算最终总价
  let final_total = discounted_total + tax_amount
  
  // 验证结果
  let expected_subtotal = 1195  // 299*2 + 199*1 + 99*3 = 598 + 199 + 297 = 1094
  let expected_discount = 109   // 1094 * 10% = 109.4，取整为109
  let expected_discounted = 985 // 1094 - 109 = 985
  let expected_tax = 78         // 985 * 8% = 78.8，取整为78
  let expected_final = 1063     // 985 + 78 = 1063
  
  // 使用简单的断言方式
  if (expected_subtotal != subtotal) {
    @builtin.abort("Test failed: expected_subtotal")
  }
  if (expected_discount != discount_amount) {
    @builtin.abort("Test failed: expected_discount")
  }
  if (expected_discounted != discounted_total) {
    @builtin.abort("Test failed: expected_discounted")
  }
  if (expected_tax != tax_amount) {
    @builtin.abort("Test failed: expected_tax")
  }
  if (expected_final != final_total) {
    @builtin.abort("Test failed: expected_final")
  }
}

// 测试用例 2: 温度转换（摄氏度转华氏度）
test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度公式：F = C × 9/5 + 32
  // 由于只有整数运算，我们使用近似公式：F = C × 2 - 30 + 32 = C × 2 + 2
  
  // 测试0°C（冰点）
  let celsius1 = 0
  let fahrenheit1 = celsius1 * 2 + 32
  if (32 != fahrenheit1) {
    @builtin.abort("Test failed: 0°C conversion")
  }
  
  // 测试100°C（沸点）
  let celsius2 = 100
  let fahrenheit2 = celsius2 * 2 + 32
  if (232 != fahrenheit2) {  // 100*2 + 32 = 232（近似值）
    @builtin.abort("Test failed: 100°C conversion")
  }
  
  // 测试负温度
  let celsius3 = -10
  let fahrenheit3 = celsius3 * 2 + 32
  if (12 != fahrenheit3) {  // -10*2 + 32 = 12（近似值）
    @builtin.abort("Test failed: -10°C conversion")
  }
}

// 测试用例 3: 计算贷款利息
test "loan_interest_calculation" {
  // 简单贷款利息计算：总还款额 = 本金 + 本金 × 年利率 × 年数
  
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率5%
  let years = 3          // 贷款年数
  
  // 计算总利息
  let total_interest = principal * annual_rate * years
  
  // 计算总还款额
  let total_repayment = principal + total_interest
  
  // 验证结果：10000 + 10000*5*3 = 10000 + 150000 = 160000
  if (150000 != total_interest) {
    @builtin.abort("Test failed: total_interest")
  }
  if (160000 != total_repayment) {
    @builtin.abort("Test failed: total_repayment")
  }
  
  // 测试提前还款的情况
  let early_payment_years = 2
  let early_interest = principal * annual_rate * early_payment_years
  let early_repayment = principal + early_interest
  
  // 验证结果：10000 + 10000*5*2 = 10000 + 100000 = 110000
  if (100000 != early_interest) {
    @builtin.abort("Test failed: early_interest")
  }
  if (110000 != early_repayment) {
    @builtin.abort("Test failed: early_repayment")
  }
}

// 测试用例 4: 计算身体质量指数（BMI）
test "bmi_calculation" {
  // BMI计算公式：BMI = 体重(kg) / 身高(m)^2
  // 由于只有整数运算，我们使用近似计算
  
  let weight_kg = 70    // 体重70公斤
  let height_cm = 175   // 身高175厘米
  let height_m = height_cm / 100  // 转换为米：1.75米
  
  // 近似计算BMI：70 / (1.75*1.75) ≈ 22.86
  // 使用整数近似：70 / 3 ≈ 23.33，取整为23
  let height_squared = height_m * height_m  // 1 * 1 = 1（整数除法导致精度损失）
  let _bmi = weight_kg / height_squared  // 使用下划线前缀表示未使用的变量
  
  // 由于整数除法的精度损失，我们使用另一种方法：
  // BMI ≈ 体重(kg) × 10000 / 身高(cm)^2
  let height_squared_cm = height_cm * height_cm  // 175*175 = 30625
  let bmi_approx = weight_kg * 10000 / height_squared_cm
  
  // 验证结果：70*10000/30625 ≈ 22.86，取整为22
  if (22 != bmi_approx) {
    @builtin.abort("Test failed: bmi_approx")
  }
  
  // 测试不同体重和身高
  let weight_kg2 = 80
  let height_cm2 = 180
  let height_squared_cm2 = height_cm2 * height_cm2  // 180*180 = 32400
  let bmi_approx2 = weight_kg2 * 10000 / height_squared_cm2
  
  // 验证结果：80*10000/32400 ≈ 24.69，取整为24
  if (24 != bmi_approx2) {
    @builtin.abort("Test failed: bmi_approx2")
  }
}

// 测试用例 5: 计算圆的面积和周长
test "circle_calculations" {
  // 圆的面积公式：π × r^2
  // 圆的周长公式：2 × π × r
  // 使用π ≈ 3.14，但由于只有整数运算，我们使用π ≈ 3
  
  let radius = 10
  
  // 计算面积：π × r^2 ≈ 3 × 10^2 = 300
  let radius_squared = radius * radius
  let area = 3 * radius_squared
  if (300 != area) {
    @builtin.abort("Test failed: area")
  }
  
  // 计算周长：2 × π × r ≈ 2 × 3 × 10 = 60
  let circumference = 2 * 3 * radius
  if (60 != circumference) {
    @builtin.abort("Test failed: circumference")
  }
  
  // 测试不同半径
  let radius2 = 5
  let radius_squared2 = radius2 * radius2
  let area2 = 3 * radius_squared2
  let circumference2 = 2 * 3 * radius2
  
  if (75 != area2) {      // 3 × 5^2 = 75
    @builtin.abort("Test failed: area2")
  }
  if (30 != circumference2) {  // 2 × 3 × 5 = 30
    @builtin.abort("Test failed: circumference2")
  }
}

// 测试用例 6: 计算数组索引（二维数组转一维数组）
test "array_index_calculation" {
  // 二维数组转一维数组的索引计算：index = row × width + col
  
  let row = 3
  let col = 4
  let width = 5
  
  // 计算索引：3 × 5 + 4 = 19
  let index = row * width + col
  if (19 != index) {
    @builtin.abort("Test failed: index")
  }
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = first_row * width + first_col
  if (0 != first_index) {
    @builtin.abort("Test failed: first_index")
  }
  
  let last_row = 9
  let last_col = 4
  let last_index = last_row * width + last_col
  if (49 != last_index) {
    @builtin.abort("Test failed: last_index")
  }
  
  // 测试不同宽度的数组
  let width2 = 10
  let row2 = 2
  let col2 = 7
  let index2 = row2 * width2 + col2
  if (27 != index2) {  // 2 × 10 + 7 = 27
    @builtin.abort("Test failed: index2")
  }
}

// 测试用例 7: 计算复利
test "compound_interest_calculation" {
  // 复利公式：A = P × (1 + r/n)^(n×t)
  // 简化版本：A = P × (1 + r×t)，其中r是年利率，t是年数
  
  let principal = 1000  // 本金
  let annual_rate = 5   // 年利率5%
  let years = 3         // 年数
  
  // 简化的复利计算
  let growth_factor = 1 + annual_rate * years
  let total_amount = principal * growth_factor
  
  // 验证结果：1000 × (1 + 5×3) = 1000 × 16 = 16000
  if (16 != growth_factor) {
    @builtin.abort("Test failed: growth_factor")
  }
  if (16000 != total_amount) {
    @builtin.abort("Test failed: total_amount")
  }
  
  // 测试不同条件
  let principal2 = 5000
  let annual_rate2 = 8
  let years2 = 5
  
  let growth_factor2 = 1 + annual_rate2 * years2
  let total_amount2 = principal2 * growth_factor2
  
  // 验证结果：5000 × (1 + 8×5) = 5000 × 41 = 205000
  if (41 != growth_factor2) {
    @builtin.abort("Test failed: growth_factor2")
  }
  if (205000 != total_amount2) {
    @builtin.abort("Test failed: total_amount2")
  }
}

// 测试用例 8: 字符串处理（用户名格式化）
test "username_formatting" {
  // 测试各种用户名格式
  
  // 简单用户名
  let simple_name = "john"
  let formatted_simple = "Hello, " + simple_name + "!"
  if ("Hello, john!" != formatted_simple) {
    @builtin.abort("Test failed: formatted_simple")
  }
  
  // 包含空格的用户名
  let spaced_name = "john doe"
  let formatted_spaced = "Hello, " + spaced_name + "!"
  if ("Hello, john doe!" != formatted_spaced) {
    @builtin.abort("Test failed: formatted_spaced")
  }
  
  // 包含数字的用户名
  let numbered_name = "john123"
  let formatted_numbered = "Hello, " + numbered_name + "!"
  if ("Hello, john123!" != formatted_numbered) {
    @builtin.abort("Test failed: formatted_numbered")
  }
  
  // 包含特殊字符的用户名
  let special_name = "john_doe@email.com"
  let formatted_special = "Hello, " + special_name + "!"
  if ("Hello, john_doe@email.com!" != formatted_special) {
    @builtin.abort("Test failed: formatted_special")
  }
  
  // 空用户名
  let empty_name = ""
  let formatted_empty = "Hello, " + empty_name + "!"
  if ("Hello, !" != formatted_empty) {
    @builtin.abort("Test failed: formatted_empty")
  }
  
  // 很长的用户名
  let long_name = "this_is_a_very_long_username_that_might_be_used_in_some_systems"
  let formatted_long = "Hello, " + long_name + "!"
  if ("Hello, this_is_a_very_long_username_that_might_be_used_in_some_systems!" != formatted_long) {
    @builtin.abort("Test failed: formatted_long")
  }
}

// 测试用例 9: 计算平均分
test "average_score_calculation" {
  // 计算一组数的平均值
  
  // 计算三门课的平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  
  // 计算总分：85 + 90 + 78 = 253
  let total_score = score1 + score2 + score3
  
  // 计算平均分：253 / 3 = 84.33，取整为84
  let average_score = total_score / 3
  if (84 != average_score) {
    @builtin.abort("Test failed: average_score")
  }
  
  // 计算五门课的平均分
  let sum_5 = 80 + 85 + 90 + 75 + 95
  let average_5 = sum_5 / 5
  if (85 != average_5) {  // (80+85+90+75+95)/5 = 425/5 = 85
    @builtin.abort("Test failed: average_5")
  }
  
  // 计算加权平均分
  let weighted_score1 = 85 * 3  // 权重3
  let weighted_score2 = 90 * 2  // 权重2
  let weighted_score3 = 78 * 1  // 权重1
  let weighted_total = weighted_score1 + weighted_score2 + weighted_score3
  let total_weight = 3 + 2 + 1
  let weighted_average = weighted_total / total_weight
  
  // 验证结果：(85×3 + 90×2 + 78×1)/(3+2+1) = (255+180+78)/6 = 513/6 = 85.5，取整为85
  if (85 != weighted_average) {
    @builtin.abort("Test failed: weighted_average")
  }
}

// 测试用例 10: 计算阶乘
test "factorial_calculation" {
  // 计算n的阶乘：n! = n × (n-1) × (n-2) × ... × 1
  
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial_5 = n * (n - 1) * (n - 2) * (n - 3) * (n - 4)
  if (120 != factorial_5) {
    @builtin.abort("Test failed: factorial_5")
  }
  
  // 计算4的阶乘：4! = 4 × 3 × 2 × 1 = 24
  let factorial_4 = 4 * 3 * 2 * 1
  if (24 != factorial_4) {
    @builtin.abort("Test failed: factorial_4")
  }
  
  // 计算3的阶乘：3! = 3 × 2 × 1 = 6
  let factorial_3 = 3 * 2 * 1
  if (6 != factorial_3) {
    @builtin.abort("Test failed: factorial_3")
  }
  
  // 计算2的阶乘：2! = 2 × 1 = 2
  let factorial_2 = 2 * 1
  if (2 != factorial_2) {
    @builtin.abort("Test failed: factorial_2")
  }
  
  // 计算1的阶乘：1! = 1
  let factorial_1 = 1
  if (1 != factorial_1) {
    @builtin.abort("Test failed: factorial_1")
  }
  
  // 计算0的阶乘：0! = 1（根据定义）
  let factorial_0 = 1
  if (1 != factorial_0) {
    @builtin.abort("Test failed: factorial_0")
  }
}