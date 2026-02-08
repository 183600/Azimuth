// Azimuth 项目增强单元测试
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "power_calculation_simulation" {
  // 幂运算模拟测试 - 计算 2^3, 3^2, 4^2
  let base1 = 2
  let exponent1 = 3
  let power1 = multiply(multiply(base1, base1), base1)  // 2^3 = 8
  
  let base2 = 3
  let exponent2 = 2
  let power2 = multiply(base2, base2)  // 3^2 = 9
  
  let base3 = 4
  let exponent3 = 2
  let power3 = multiply(base3, base3)  // 4^2 = 16
  
  assert_eq(power1, 8)
  assert_eq(power2, 9)
  assert_eq(power3, 16)
}

test "average_calculation_with_ceil" {
  // 平均值计算测试（使用向上取整）
  let numbers_sum = add(add(add(15, 25), 35), 45)  // 总和 = 120
  let count = 4
  let average = divide_with_ceil(numbers_sum, count)  // 平均值 = 30
  
  // 测试不能整除的情况
  let odd_sum = add(add(add(10, 20), 30), 25)  // 总和 = 85
  let odd_average = divide_with_ceil(odd_sum, count)  // 向上取整平均值 = 22
  
  assert_eq(average, 30)
  assert_eq(odd_average, 22)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let part1 = 50
  let part2 = 75
  let part3 = 30
  
  // 计算百分比（使用整数运算）
  let percent1 = divide_with_ceil(multiply(part1, 100), total)  // 25%
  let percent2 = divide_with_ceil(multiply(part2, 100), total)  // 38%
  let percent3 = divide_with_ceil(multiply(part3, 100), total)  // 15%
  
  // 验证百分比总和不超过100%（由于向上取整可能超过）
  let total_percent = add(add(percent1, percent2), percent3)
  
  assert_eq(percent1, 25)
  assert_eq(percent2, 38)
  assert_eq(percent3, 15)
  assert_eq(total_percent, 78)
}

test "discount_calculation_scenario" {
  // 折扣计算场景测试
  let original_price = 120
  let discount_percent = 15
  let tax_percent = 8
  
  // 计算折扣金额
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percent), 100)
  let discounted_price = subtract(original_price, discount_amount)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_percent), 100)
  let final_price = add(discounted_price, tax_amount)
  
  // 计算节省的金额
  let total_savings = subtract(original_price, final_price)
  
  assert_eq(discount_amount, 18)
  assert_eq(discounted_price, 102)
  assert_eq(tax_amount, 9)
  assert_eq(final_price, 111)
  assert_eq(total_savings, 9)
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算测试
  let distance = 150  // 公里
  let speed1 = 50     // 公里/小时
  let speed2 = 60     // 公里/小时
  
  // 计算时间
  let time1 = divide_with_ceil(distance, speed1)  // 3小时
  let time2 = divide_with_ceil(distance, speed2)  // 3小时（向上取整）
  
  // 计算在不同时间下的距离
  let time3 = 2  // 小时
  let distance3 = multiply(speed1, time3)  // 100公里
  
  // 计算平均速度
  let total_distance = add(distance, distance3)
  let total_time = add(time1, time3)
  let average_speed = divide_with_ceil(total_distance, total_time)
  
  assert_eq(time1, 3)
  assert_eq(time2, 3)
  assert_eq(distance3, 100)
  assert_eq(total_distance, 250)
  assert_eq(total_time, 5)
  assert_eq(average_speed, 50)
}

test "array_index_calculations" {
  // 数组索引计算测试
  let array_size = 10
  let start_index = 0
  
  // 计算中间索引
  let middle_index = divide_with_ceil(array_size, 2)  // 5
  
  // 计算四分位索引
  let first_quarter = divide_with_ceil(array_size, 4)  // 3
  let third_quarter = multiply(middle_index, first_quarter)  // 15
  
  // 计算最后索引
  let last_index = subtract(array_size, 1)  // 9
  
  // 验证索引范围
  let is_valid_middle = middle_index < array_size
  let is_valid_last = last_index < array_size
  
  assert_eq(middle_index, 5)
  assert_eq(first_quarter, 3)
  assert_eq(third_quarter, 15)
  assert_eq(last_index, 9)
  assert_true(is_valid_middle)
  assert_true(is_valid_last)
}

test "currency_conversion_rounding" {
  // 货币转换四舍五入测试
  let amount_usd = 100
  let exchange_rate_eur = 0.85  // 简化为整数运算：85/100
  let exchange_rate_gbp = 0.75  // 简化为整数运算：75/100
  
  // 转换为欧元（使用向上取整模拟银行 rounding）
  let amount_eur_raw = multiply(amount_usd, 85)
  let amount_eur = divide_with_ceil(amount_eur_raw, 100)
  
  // 转换为英镑
  let amount_gbp_raw = multiply(amount_usd, 75)
  let amount_gbp = divide_with_ceil(amount_gbp_raw, 100)
  
  // 计算转换手续费（2%）
  let fee_rate = 2
  let fee_eur = divide_with_ceil(multiply(amount_eur, fee_rate), 100)
  let final_eur = add(amount_eur, fee_eur)
  
  assert_eq(amount_eur, 85)
  assert_eq(amount_gbp, 75)
  assert_eq(fee_eur, 2)
  assert_eq(final_eur, 87)
}

test "boolean_logic_evaluation" {
  // 布尔逻辑评估测试
  // 模拟布尔运算结果（1表示true，0表示false）
  
  // 逻辑与运算模拟
  let true_val = 1
  let false_val = 0
  
  // true AND true = true
  let and_true_true = multiply(true_val, true_val)
  
  // true AND false = false
  let and_true_false = multiply(true_val, false_val)
  
  // false AND false = false
  let and_false_false = multiply(false_val, false_val)
  
  // 逻辑或运算模拟（使用加法，但限制最大值为1）
  let or_true_true = add(true_val, true_val)
  let or_true_false = add(true_val, false_val)
  let or_false_false = add(false_val, false_val)
  
  // 将结果转换为布尔值
  let is_and_true_true = and_true_true > 0
  let is_and_true_false = and_true_false > 0
  let is_and_false_false = and_false_false > 0
  let is_or_true_true = or_true_true > 0
  let is_or_true_false = or_true_false > 0
  let is_or_false_false = or_false_false > 0
  
  assert_true(is_and_true_true)
  assert_false(is_and_true_false)
  assert_false(is_and_false_false)
  assert_true(is_or_true_true)
  assert_true(is_or_true_false)
  assert_false(is_or_false_false)
}