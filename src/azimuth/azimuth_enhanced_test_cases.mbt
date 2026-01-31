// 高质量 MoonBit 测试用例 - 针对 azimuth 库的增强测试
// 包含边界情况、错误处理、实际应用场景和组合操作

test "advanced_arithmetic_sequences" {
  // 测试等差数列和等比数列的计算
  // 等差数列：3, 7, 11, 15, 19 (公差为4)
  let arithmetic_sum = add(add(add(add(3, 7), 11), 15), 19)
  assert_eq(55, arithmetic_sum)
  
  // 等比数列模拟：2, 4, 8, 16 (通过乘法模拟)
  let geometric_sum = add(add(add(2, multiply(2, 2)), multiply(2, 4)), multiply(2, 8))
  assert_eq(30, geometric_sum)
}

test "financial_compound_interest" {
  // 模拟复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)  // 使用向上取整的复利计算
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  
  // 长字符串和Unicode
  let long_name = "VeryLongNameThatMightCauseIssues"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
  
  // 数字和符号组合
  assert_eq_string("Hello, user123@domain.com!", greet("user123@domain.com"))
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 10000
  let project_costs = multiply(1500, 4)  // 4个项目，每个1500
  let remaining = add(total_budget, -project_costs)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 800
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(6000, project_costs)
  assert_eq(4000, remaining)
  assert_eq(5, additional_projects)
}

test "time_zone_calculation" {
  // 时区转换计算
  let local_time = 14  // 下午2点
  let time_difference = -8  // 与UTC的时差
  
  // 计算UTC时间（处理24小时制）
  let utc_time_raw = add(local_time, time_difference)
  let utc_time = if utc_time_raw < 0 {
    add(utc_time_raw, 24)
  } else if utc_time_raw > 23 {
    add(utc_time_raw, -24)
  } else {
    utc_time_raw
  }
  
  assert_eq(6, utc_time)  // 下午2点 - 8小时 = 早上6点
}

test "inventory_batch_processing" {
  // 库存批量处理
  let batch_size = 25
  let total_items = 187
  let processing_time_per_batch = 2  // 每批2小时
  
  let total_batches = divide_with_ceil(total_items, batch_size)
  let total_processing_time = multiply(total_batches, processing_time_per_batch)
  
  // 计算剩余未满批次的项目数
  let processed_items = multiply(total_batches - 1, batch_size)
  let remaining_items = add(total_items, -processed_items)
  
  assert_eq(8, total_batches)
  assert_eq(16, total_processing_time)
  assert_eq(12, remaining_items)
}

test "temperature_conversion" {
  // 温度转换计算
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  // 反向验证
  let back_to_celsius = divide_with_ceil(multiply(add(fahrenheit, -32), 5), 9)
  
  assert_eq(77, fahrenheit)
  assert_eq(25, back_to_celsius)
}

test "network_data_packet_analysis" {
  // 网络数据包分析
  let packet_header_size = 20
  let max_packet_size = 1500
  let data_payload = 1480
  
  let total_packets = divide_with_ceil(data_payload, subtract(max_packet_size, packet_header_size))
  let last_packet_size = if data_payload % subtract(max_packet_size, packet_header_size) == 0 {
    subtract(max_packet_size, packet_header_size)
  } else {
    data_payload % subtract(max_packet_size, packet_header_size)
  }
  
  assert_eq(2, total_packets)
  assert_eq(1460, last_packet_size)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 极值测试
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(0, add(large_positive, -large_positive))
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  
  // 除法边界情况
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "complex_business_workflow" {
  // 复杂业务工作流
  let base_price = 100
  let quantity = 15
  let tier1_discount = 5    // 5% 折扣
  let tier2_discount = 10   // 10% 折扣
  let tax_rate = 8          // 8% 税率
  let shipping_base = 20
  
  // 计算小计
  let subtotal = multiply(base_price, quantity)
  
  // 根据数量计算折扣
  let discount_rate = if quantity >= 20 {
    tier2_discount
  } else if quantity >= 10 {
    tier1_discount
  } else {
    0
  }
  
  let discount_amount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  let after_discount = add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  
  // 计算运费（每10个物品一个运费单位）
  let shipping_units = divide_with_ceil(quantity, 10)
  let total_shipping = multiply(shipping_base, shipping_units)
  
  // 最终总价
  let final_total = add(add(after_discount, tax_amount), total_shipping)
  
  assert_eq(1500, subtotal)
  assert_eq(75, discount_amount)
  assert_eq(1425, after_discount)
  assert_eq(114, tax_amount)
  assert_eq(40, total_shipping)
  assert_eq(1579, final_total)
}