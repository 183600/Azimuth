// 额外的 MoonBit 测试用例
// 这些测试用例覆盖了 azimuth 库的各种功能和场景

test "absolute_value_simulation" {
  // 模拟绝对值计算
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 正数的绝对值是它本身
  let pos_abs = if positive < 0 { azimuth::multiply(positive, -1) } else { positive }
  assert_eq(42, pos_abs)
  
  // 负数的绝对值是它的相反数
  let neg_abs = if negative < 0 { azimuth::multiply(negative, -1) } else { negative }
  assert_eq(42, neg_abs)
  
  // 零的绝对值是零
  let zero_abs = if zero < 0 { azimuth::multiply(zero, -1) } else { zero }
  assert_eq(0, zero_abs)
}

test "distance_calculation" {
  // 计算两点之间的距离（简化版，只考虑整数）
  let x1 = 3
  let y1 = 4
  let x2 = 9
  let y2 = 12
  
  // 计算 x 坐标差值
  let dx = azimuth::add(x2, -x1)
  // 计算 y 坐标差值
  let dy = azimuth::add(y2, -y1)
  
  // 计算平方和（简化距离计算）
  let dx_squared = azimuth::multiply(dx, dx)
  let dy_squared = azimuth::multiply(dy, dy)
  let distance_squared = azimuth::add(dx_squared, dy_squared)
  
  assert_eq(100, distance_squared)  // 6² + 8² = 36 + 64 = 100
}

test "loan_amortization" {
  // 贷款摊还计算（简化版）
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率（百分比）
  let years = 3          // 贷款年限
  
  // 计算总利息（简化计算：本金 × 年利率 × 年数）
  let total_interest = azimuth::multiply(principal, azimuth::multiply(annual_rate, years)) / 100
  // 计算总还款额
  let total_payment = azimuth::add(principal, total_interest)
  // 计算月还款额（简化：总还款额 / 总月数）
  let monthly_payment = total_payment / (years * 12)
  
  assert_eq(1800, total_interest)    // 10000 × 6 × 3 / 100 = 1800
  assert_eq(11800, total_payment)    // 10000 + 1800 = 11800
  assert_eq(327, monthly_payment)    // 11800 / 36 ≈ 327（整数除法）
}

test "temperature_conversion_extended" {
  // 扩展的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数：F = C × 2 + 30（近似值）
  
  let freezing_c = 0     // 冰点
  let boiling_c = 100    // 沸点
  let room_c = 20        // 室温
  let body_c = 37        // 体温
  
  // 转换为华氏度
  let freezing_f = azimuth::add(azimuth::multiply(freezing_c, 2), 30)
  let boiling_f = azimuth::add(azimuth::multiply(boiling_c, 2), 30)
  let room_f = azimuth::add(azimuth::multiply(room_c, 2), 30)
  let body_f = azimuth::add(azimuth::multiply(body_c, 2), 30)
  
  assert_eq(30, freezing_f)    // 0 × 2 + 30 = 30（近似32°F）
  assert_eq(230, boiling_f)    // 100 × 2 + 30 = 230（近似212°F）
  assert_eq(70, room_f)        // 20 × 2 + 30 = 70（近似68°F）
  assert_eq(104, body_f)       // 37 × 2 + 30 = 104（近似98.6°F）
}

test "inventory_management" {
  // 库存管理计算
  let initial_stock = 500
  let units_sold = 120
  let units_received = 80
  let units_damaged = 15
  let safety_stock = 50
  
  // 计算当前库存
  let current_stock = azimuth::add(azimuth::add(initial_stock, -units_sold), units_received)
  // 减去损坏商品
  let available_stock = azimuth::add(current_stock, -units_damaged)
  // 计算是否需要补货
  let needs_reorder = available_stock < safety_stock
  
  assert_eq(445, available_stock)  // 500 - 120 + 80 - 15 = 445
  assert_false(needs_reorder)      // 445 > 50，不需要补货
}

test "compound_interest_comparison" {
  // 复利与单利比较
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 单利计算：本金 + (本金 × 利率 × 年数)
  let simple_interest = azimuth::multiply(principal, azimuth::multiply(rate, years)) / 100
  let simple_total = azimuth::add(principal, simple_interest)
  
  // 复利计算（简化）：本金 × (1 + 利率 × 年数)
  let compound_factor = azimuth::add(100, azimuth::multiply(rate, years))
  let compound_total = azimuth::multiply(principal, compound_factor)
  
  assert_eq(150, simple_interest)     // 1000 × 5 × 3 / 100 = 150
  assert_eq(1150, simple_total)       // 1000 + 150 = 1150
  assert_eq(115000, compound_total)   // 1000 × 115 = 115000（表示1150）
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种特殊字符串情况
  
  // 测试包含数字的字符串
  let numeric_name = "User2023"
  assert_eq_string("Hello, User2023!", azimuth::greet(numeric_name))
  
  // 测试包含下划线的字符串
  let underscore_name = "first_last"
  assert_eq_string("Hello, first_last!", azimuth::greet(underscore_name))
  
  // 测试包含连字符的字符串
  let hyphen_name = "user-name"
  assert_eq_string("Hello, user-name!", azimuth::greet(hyphen_name))
  
  // 测试混合大小写
  let mixed_case = "TeStInG"
  assert_eq_string("Hello, TeStInG!", azimuth::greet(mixed_case))
}

test "array_sum_simulation" {
  // 模拟数组求和（使用展开的方式）
  // 假设有数组 [10, 20, 30, 40, 50]
  
  let elem1 = 10
  let elem2 = 20
  let elem3 = 30
  let elem4 = 40
  let elem5 = 50
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(elem1, elem2), elem3), elem4), elem5)
  
  // 计算平均值
  let average = sum / 5
  
  // 计算加权平均（权重分别为 1, 2, 3, 4, 5）
  let weighted_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(
    azimuth::multiply(elem1, 1), 
    azimuth::multiply(elem2, 2)), 
    azimuth::multiply(elem3, 3)), 
    azimuth::multiply(elem4, 4)), 
    azimuth::multiply(elem5, 5))
  
  let total_weights = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  let weighted_average = weighted_sum / total_weights
  
  assert_eq(150, sum)
  assert_eq(30, average)
  assert_eq(36, weighted_average)  // (10×1 + 20×2 + 30×3 + 40×4 + 50×5) / 15 = 550 / 15 ≈ 36
}

test "sales_commission" {
  // 销售佣金计算
  let base_salary = 2000
  let sales_amount = 50000
  let commission_rate = 8  // 8%
  let bonus_threshold = 40000
  let bonus_amount = 500
  
  // 计算佣金
  let commission = azimuth::multiply(sales_amount, commission_rate) / 100
  // 计算是否达到奖金门槛
  let has_bonus = sales_amount > bonus_threshold
  let total_bonus = if has_bonus { bonus_amount } else { 0 }
  // 计算总收入
  let total_income = azimuth::add(azimuth::add(base_salary, commission), total_bonus)
  
  assert_eq(4000, commission)      // 50000 × 8 / 100 = 4000
  assert_true(has_bonus)           // 50000 > 40000
  assert_eq(6500, total_income)    // 2000 + 4000 + 500 = 6500
}

test "greeting_with_context" {
  // 带上下文的问候测试
  // 测试不同场景下的问候功能
  
  // 测试正式场合
  let formal_name = "Mr. Smith"
  assert_eq_string("Hello, Mr. Smith!", azimuth::greet(formal_name))
  
  // 测试非正式场合
  let informal_name = "Bob"
  assert_eq_string("Hello, Bob!", azimuth::greet(informal_name))
  
  // 测试带头衔的姓名
  let titled_name = "Dr. Jane Johnson"
  assert_eq_string("Hello, Dr. Jane Johnson!", azimuth::greet(titled_name))
  
  // 测试包含数字的用户ID
  let user_id = "User12345"
  assert_eq_string("Hello, User12345!", azimuth::greet(user_id))
}