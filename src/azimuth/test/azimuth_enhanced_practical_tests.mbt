// Azimuth 增强实用测试用例 - 8个高质量 MoonBit 测试用例
// 专注于实际应用场景、边界条件和复杂业务逻辑

test "real_world_shopping_cart_calculation" {
  // 真实世界购物车计算测试
  let item_price = 299
  let quantity = 4
  let discount_per_item = 25
  let shipping_cost_per_package = 15
  let items_per_package = 3
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  @azimuth.assert_eq(1196, subtotal)  // 299 * 4 = 1196
  
  // 计算总折扣
  let total_discount = @azimuth.multiply(discount_per_item, quantity)
  @azimuth.assert_eq(100, total_discount)  // 25 * 4 = 100
  
  // 计算折扣后价格
  let discounted_price = @azimuth.subtract(subtotal, total_discount)
  @azimuth.assert_eq(1096, discounted_price)  // 1196 - 100 = 1096
  
  // 计算需要的包装数量
  let packages_needed = @azimuth.divide_with_ceil(quantity, items_per_package)
  @azimuth.assert_eq(2, packages_needed)  // ceil(4/3) = 2
  
  // 计算总运费
  let total_shipping = @azimuth.multiply(packages_needed, shipping_cost_per_package)
  @azimuth.assert_eq(30, total_shipping)  // 2 * 15 = 30
  
  // 计算最终总价
  let final_total = @azimuth.add(discounted_price, total_shipping)
  @azimuth.assert_eq(1126, final_total)  // 1096 + 30 = 1126
}

test "time_zone_conversion_scenario" {
  // 时区转换场景测试
  let local_hours = 23  // 本地时间 23:00
  let time_difference = -8  // 目标时区差 -8 小时
  
  // 计算目标时间
  let target_time_raw = @azimuth.add(local_hours, time_difference)
  @azimuth.assert_eq(15, target_time_raw)  // 23 + (-8) = 15
  
  // 处理跨日期情况（如果结果为负数，需要加24小时）
  let final_target_time = 
    if target_time_raw < 0 { 
      @azimuth.add(target_time_raw, 24) 
    } else { 
      target_time_raw 
    }
  @azimuth.assert_eq(15, final_target_time)
  
  // 验证反向计算
  let reverse_time = @azimuth.add(final_target_time, 8)
  let adjusted_reverse = 
    if reverse_time >= 24 {
      @azimuth.subtract(reverse_time, 24)
    } else {
      reverse_time
    }
  @azimuth.assert_eq(23, adjusted_reverse)
}

test "inventory_reorder_point_calculation" {
  // 库存重新订购点计算测试
  let daily_demand = 45
  let lead_time_days = 7
  let safety_stock = 100
  let current_inventory = 180
  let order_quantity = 500
  
  // 计算重新订购点
  let reorder_point = @azimuth.add(@azimuth.multiply(daily_demand, lead_time_days), safety_stock)
  @azimuth.assert_eq(415, reorder_point)  // 45*7 + 100 = 415
  
  // 检查是否需要重新订购
  let should_reorder = current_inventory <= reorder_point
  @azimuth.assert_true(should_reorder)  // 180 <= 415 = true
  
  // 计算订购后的库存
  let new_inventory = @azimuth.add(current_inventory, order_quantity)
  @azimuth.assert_eq(680, new_inventory)  // 180 + 500 = 680
  
  // 计算可供应天数
  let days_of_supply = @azimuth.divide_with_ceil(new_inventory, daily_demand)
  @azimuth.assert_eq(16, days_of_supply)  // ceil(680/45) = 16
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 127
  let team_members = 8
  let working_days = 15
  let tasks_per_person_per_day = 2
  
  // 计算团队每日处理能力
  let daily_capacity = @azimuth.multiply(team_members, tasks_per_person_per_day)
  @azimuth.assert_eq(16, daily_capacity)  // 8 * 2 = 16
  
  // 计算项目总处理能力
  let total_capacity = @azimuth.multiply(daily_capacity, working_days)
  @azimuth.assert_eq(240, total_capacity)  // 16 * 15 = 240
  
  // 验证资源充足性
  let is_resource_sufficient = total_capacity >= total_tasks
  @azimuth.assert_true(is_resource_sufficient)  // 240 >= 127 = true
  
  // 计算实际需要的工作日
  let required_days = @azimuth.divide_with_ceil(total_tasks, daily_capacity)
  @azimuth.assert_eq(8, required_days)  // ceil(127/16) = 8
  
  // 计算资源利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_tasks, 100), total_capacity)
  @azimuth.assert_eq(53, utilization)  // floor(12700/240) = 52%，向上取整为53%
}

test "batch_processing_optimization" {
  // 批处理优化测试
  let total_records = 987
  let max_batch_size = 50
  let processing_time_per_batch = 3
  let overhead_time_per_batch = 1
  
  // 计算需要的批次数
  let batch_count = @azimuth.divide_with_ceil(total_records, max_batch_size)
  @azimuth.assert_eq(20, batch_count)  // ceil(987/50) = 20
  
  // 计算每批次实际处理时间
  let time_per_batch = @azimuth.add(processing_time_per_batch, overhead_time_per_batch)
  @azimuth.assert_eq(4, time_per_batch)  // 3 + 1 = 4
  
  // 计算总处理时间
  let total_time = @azimuth.multiply(batch_count, time_per_batch)
  @azimuth.assert_eq(80, total_time)  // 20 * 4 = 80
  
  // 计算最后一批的记录数
  let full_batches = @azimuth.subtract(batch_count, 1)
  let processed_in_full_batches = @azimuth.multiply(full_batches, max_batch_size)
  let last_batch_size = @azimuth.subtract(total_records, processed_in_full_batches)
  @azimuth.assert_eq(37, last_batch_size)  // 987 - (19*50) = 37
}

test "financial_compound_interest_calculation" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化的年复利计算（不使用幂运算，用乘法模拟）
  let year1_balance = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  @azimuth.assert_eq(10500, year1_balance)  // 10000 + ceil(10000*5/100) = 10500
  
  let year2_balance = @azimuth.add(year1_balance, @azimuth.divide_with_ceil(@azimuth.multiply(year1_balance, annual_rate), 100))
  @azimuth.assert_eq(11025, year2_balance)  // 10500 + ceil(10500*5/100) = 11025
  
  let year3_balance = @azimuth.add(year2_balance, @azimuth.divide_with_ceil(@azimuth.multiply(year2_balance, annual_rate), 100))
  @azimuth.assert_eq(11576, year3_balance)  // 11025 + ceil(11025*5/100) = 11576
  
  // 计算总收益
  let total_earnings = @azimuth.subtract(year3_balance, principal)
  @azimuth.assert_eq(1576, total_earnings)  // 11576 - 10000 = 1576
  
  // 计算收益率
  let return_rate = @azimuth.divide_with_ceil(@azimuth.multiply(total_earnings, 100), principal)
  @azimuth.assert_eq(16, return_rate)  // ceil(157600/10000) = 16%
}

test "multi_currency_conversion" {
  // 多货币转换测试
  let usd_amount = 1250
  let eur_to_usd_rate = 1.08  // 简化为整数处理：108
  let gbp_to_usd_rate = 1.27  // 简化为整数处理：127
  let jpy_to_usd_rate = 0.0067  // 简化为整数处理：67/10000
  
  // 转换为欧元（使用整数运算模拟）
  let eur_amount_raw = @azimuth.multiply(usd_amount, 100)
  let eur_amount = @azimuth.divide_with_ceil(eur_amount_raw, 108)
  @azimuth.assert_eq(1158, eur_amount)  // ceil(125000/108) = 1158
  
  // 转换为英镑
  let gbp_amount_raw = @azimuth.multiply(usd_amount, 100)
  let gbp_amount = @azimuth.divide_with_ceil(gbp_amount_raw, 127)
  @azimuth.assert_eq(984, gbp_amount)  // ceil(125000/127) = 984
  
  // 转换为日元（大数处理）
  let jpy_amount_raw = @azimuth.multiply(usd_amount, 10000)
  let jpy_amount = @azimuth.divide_with_ceil(jpy_amount_raw, 67)
  @azimuth.assert_eq(186568, jpy_amount)  // ceil(12500000/67) = 186568
  
  // 验证反向转换（EUR -> USD）
  let back_to_usd_from_eur = @azimuth.divide_with_ceil(@azimuth.multiply(eur_amount, 108), 100)
  @azimuth.assert_eq(1250, back_to_usd_from_eur)  // ceil(1158*108/100) = 1250
}

test "telecommunications_billing_system" {
  // 电信计费系统测试
  let base_monthly_fee = 399
  let call_minutes = 245
  let extra_minute_rate = 2  // 每分钟2分钱
  let data_gb = 12
  let extra_data_rate = 10  // 每GB 10元
  let included_minutes = 200
  let included_data = 10
  
  // 计算额外通话费用
  let extra_minutes = @azimuth.subtract(call_minutes, included_minutes)
  let extra_call_cost = 
    if extra_minutes > 0 {
      @azimuth.multiply(extra_minutes, extra_minute_rate)
    } else {
      0
    }
  @azimuth.assert_eq(90, extra_call_cost)  // (245-200) * 2 = 90
  
  // 计算额外数据费用
  let extra_data = @azimuth.subtract(data_gb, included_data)
  let extra_data_cost = 
    if extra_data > 0 {
      @azimuth.multiply(extra_data, extra_data_rate)
    } else {
      0
    }
  @azimuth.assert_eq(20, extra_data_cost)  // (12-10) * 10 = 20
  
  // 计算总费用（转换为元）
  let total_extra_cost_cents = @azimuth.add(extra_call_cost, @azimuth.multiply(extra_data_cost, 100))
  let total_extra_cost_yuan = @azimuth.divide_with_ceil(total_extra_cost_cents, 100)
  @azimuth.assert_eq(29, total_extra_cost_yuan)  // ceil((90+2000)/100) = 21
  
  // 计算最终账单
  let final_bill = @azimuth.add(base_monthly_fee, total_extra_cost_yuan)
  @azimuth.assert_eq(420, final_bill)  // 399 + 21 = 420
  
  // 验证费用合理性
  let is_reasonable = final_bill <= @azimuth.multiply(base_monthly_fee, 2)
  @azimuth.assert_true(is_reasonable)  // 420 <= 798 = true
}