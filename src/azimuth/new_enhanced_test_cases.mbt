// 新的增强测试用例 - 针对 azimuth 库的补充测试
// 使用标准的 MoonBit 测试语法

test "mathematical_identity_laws" {
  // 测试数学恒等律
  let x = 42
  assert_eq(x, add(x, 0))  // 加法单位元
  assert_eq(x, multiply(x, 1))  // 乘法单位元
  assert_eq(0, multiply(x, 0))  // 零元素性质
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期计算
  let principal = 10000
  let annual_rate = 6  // 6% 年利率
  let months = 12
  
  // 简化的月利率计算
  let monthly_rate = divide_with_ceil(annual_rate, 12)
  
  // 简化的月供计算（本金 + 利息）
  let monthly_interest = divide_with_ceil(multiply(principal, monthly_rate), 100)
  let total_interest = multiply(monthly_interest, months)
  let total_payment = add(principal, total_interest)
  
  assert_eq(600, total_interest)
  assert_eq(10600, total_payment)
}

test "string_processing_with_special_characters" {
  // 测试特殊字符字符串处理
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Niño!", greet("Niño"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, a&b|c!", greet("a&b|c"))
}

test "advanced_division_scenarios" {
  // 高级除法场景测试
  // 测试连续除法
  let result1 = divide_with_ceil(divide_with_ceil(100, 3), 2)
  assert_eq(17, result1)  // ceil(ceil(100/3)/2) = ceil(34/2) = 17
  
  // 测试嵌套除法
  let result2 = divide_with_ceil(100, divide_with_ceil(3, 2))
  assert_eq(50, result2)  // ceil(100/ceil(3/2)) = ceil(100/2) = 50
  
  // 测试负数连续除法
  let result3 = divide_with_ceil(divide_with_ceil(-100, 3), -2)
  assert_eq(17, result3)  // ceil(ceil(-100/3)/-2) = ceil(-33/-2) = 17
}

test "real_world_inventory_management" {
  // 真实世界库存管理
  let initial_stock = 500
  let daily_sales = 15
  let restock_frequency = 7  // 每7天补货一次
  let restock_amount = 200
  
  // 计算一周后的库存
  let weekly_sales = multiply(daily_sales, restock_frequency)
  let stock_before_restock = add(initial_stock, -weekly_sales)
  let stock_after_restock = add(stock_before_restock, restock_amount)
  
  // 计算需要的货架空间（每箱20件）
  let shelves_needed = divide_with_ceil(stock_after_restock, 20)
  
  assert_eq(105, weekly_sales)
  assert_eq(395, stock_before_restock)
  assert_eq(595, stock_after_restock)
  assert_eq(30, shelves_needed)
}

test "time_zone_calculation" {
  // 时区计算
  let local_time = 14  // 下午2点
  let time_difference = 8  // 时差8小时
  
  // 计算另一个时区的时间
  let remote_time_add = add(local_time, time_difference)
  let remote_time_mod = if remote_time_add > 24 {
    add(remote_time_add, -24)
  } else {
    remote_time_add
  }
  
  let remote_time_sub = add(local_time, -time_difference)
  let remote_time_sub_final = if remote_time_sub < 0 {
    add(remote_time_sub, 24)
  } else {
    remote_time_sub
  }
  
  assert_eq(22, remote_time_mod)  // 14 + 8 = 22
  assert_eq(6, remote_time_sub_final)  // 14 - 8 = 6
}

test "data_structure_simulation" {
  // 模拟数据结构操作
  let array_size = 10
  let page_size = 3
  
  // 计算分页数量
  let total_pages = divide_with_ceil(array_size, page_size)
  
  // 计算最后一页的元素数量
  let last_page_items = if array_size % page_size == 0 {
    page_size
  } else {
    array_size % page_size
  }
  
  // 计算总索引数量（从0开始）
  let total_indices = add(array_size, -1)
  
  assert_eq(4, total_pages)
  assert_eq(1, last_page_items)
  assert_eq(9, total_indices)
}

test "engineering_tolerance_calculation" {
  // 工程容差计算
  let nominal_value = 100
  let tolerance_percentage = 5
  
  // 计算容差范围
  let tolerance_value = divide_with_ceil(multiply(nominal_value, tolerance_percentage), 100)
  let upper_limit = add(nominal_value, tolerance_value)
  let lower_limit = add(nominal_value, -tolerance_value)
  
  // 计算容差带宽
  let tolerance_band = multiply(2, tolerance_value)
  
  assert_eq(5, tolerance_value)
  assert_eq(105, upper_limit)
  assert_eq(95, lower_limit)
  assert_eq(10, tolerance_band)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  let linear_factor = 2
  let logarithmic_factor = 10
  
  // 模拟 O(n + log n) 复杂度
  let linear_component = multiply(input_size, linear_factor)
  let log_component = multiply(divide_with_ceil(input_size, 100), logarithmic_factor)
  let total_complexity = add(linear_component, log_component)
  
  // 模拟 O(n log n) 复杂度
  let n_log_n_component = multiply(input_size, divide_with_ceil(input_size, 100))
  
  assert_eq(2000, linear_component)
  assert_eq(100, log_component)
  assert_eq(2100, total_complexity)
  assert_eq(10000, n_log_n_component)
}

test "comprehensive_error_handling" {
  // 综合错误处理测试
  // 测试除零安全的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}