// 额外的单元测试用例 - 针对 azimuth 库的高质量测试
// 包含边界情况、实际应用场景和数学性质验证

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数
}

test "multiply_with_zero_properties" {
  // 测试乘法的零性质
  assert_eq(0, multiply(0, 2147483647))  // 零乘以最大值
  assert_eq(0, multiply(-2147483648, 0))  // 最小值乘以零
  assert_eq(0, multiply(0, -1))  // 零乘以负数
}

test "divide_with_ceil_edge_scenarios" {
  // 测试向上取整除法的边界场景
  assert_eq(1, divide_with_ceil(1, 2147483647))  // 小分子大分母
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))  // 大分子小分母
  assert_eq(-1, divide_with_ceil(-1, 2147483647))  // 负小分子大分母
}

test "greet_function_unicode_comprehensive" {
  // 测试问候函数的全面Unicode支持
  assert_eq_string("Hello, 🌟🌙☀️!", greet("🌟🌙☀️"))  // 表情符号
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))  // 希腊字母
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))  // 带重音的拉丁字母
  assert_eq_string("Hello, 🏠🚗🌳!", greet("🏠🚗🌳"))  // 物体表情符号
}

test "complex_financial_calculation" {
  // 复杂的金融计算场景
  let principal = 10000
  let rate1 = 5  // 第一年利率
  let rate2 = 6  // 第二年利率
  let rate3 = 7  // 第三年利率
  
  // 计算复利（简化版，使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate1), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate2), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate3), 100))
  
  assert_eq(10500, year1)  // 10000 + 500
  assert_eq(11130, year2)  // 10500 + 630
  assert_eq(11909, year3)  // 11130 + 779
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let used_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -used_budget)
  
  // 计算可以支持的小项目数量（每个300元）
  let small_projects = divide_with_ceil(remaining, 300)
  
  assert_eq(3500, used_budget)
  assert_eq(1500, remaining)
  assert_eq(5, small_projects)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等比数列
  let first_term = 2
  let ratio = 3
  let terms = 5
  
  // 计算等比数列的和（简化计算）
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  assert_eq(2, term1)
  assert_eq(6, term2)
  assert_eq(18, term3)
  assert_eq(54, term4)
  assert_eq(162, term5)
  assert_eq(242, sum)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与UTC的时差
  
  // 计算UTC时间（简化处理）
  let utc_time = add(local_time, -time_difference)
  let adjusted_utc = if utc_time < 0 { add(utc_time, 24) } else { utc_time }
  
  assert_eq(6, adjusted_utc)  // 14 - 8 = 6（上午6点）
}

test "inventory_reorder_calculation" {
  // 库存重新订购计算
  let current_stock = 45
  let daily_sales = 7
  let lead_time_days = 5
  let safety_stock = 20
  
  // 计算重新订购点
  let lead_time_demand = multiply(daily_sales, lead_time_days)
  let reorder_point = add(lead_time_demand, safety_stock)
  
  // 计算需要订购的数量（订购到100个）
  let target_stock = 100
  let order_quantity = if current_stock < reorder_point {
    add(target_stock, -current_stock)
  } else {
    0
  }
  
  assert_eq(35, lead_time_demand)
  assert_eq(55, reorder_point)
  assert_eq(55, order_quantity)  // 45 < 55，需要订购
}

test "data_structure_simulation" {
  // 数据结构模拟：计算数组的统计信息
  let values = [10, 15, 20, 25, 30]  // 模拟数组
  let count = 5
  
  // 计算总和（模拟数组遍历）
  let sum = add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4])
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, count)
  
  // 计算中位数（奇数个元素）
  let median = values[2]  // 排序后的中间值
  
  assert_eq(100, sum)
  assert_eq(20, average)
  assert_eq(20, median)
}