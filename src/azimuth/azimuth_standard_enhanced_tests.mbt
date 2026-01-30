// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的额外测试，不超过10个测试用例

test "financial_interest_calculation" {
  // 金融利息计算场景
  let principal = 10000
  let interest_rate = 5
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利计算）
  assert_eq(11576, year3_amount)
}

test "inventory_reorder_point" {
  // 库存重新订购点计算
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 20
  
  // 计算重新订购点
  let demand_during_lead_time = multiply(daily_demand, lead_time_days)
  let reorder_point = add(demand_during_lead_time, safety_stock)
  
  assert_eq(125, reorder_point)
}

test "data_batch_processing" {
  // 数据批处理计算
  let total_records = 1000
  let batch_size = 75
  
  // 计算需要的批次数
  let full_batches = divide_with_ceil(total_records, batch_size)
  
  assert_eq(14, full_batches)
}

test "project_resource_allocation" {
  // 项目资源分配计算
  let total_budget = 50000
  let team_members = 8
  let project_duration_weeks = 12
  
  // 计算每人每周预算
  let total_person_weeks = multiply(team_members, project_duration_weeks)
  let budget_per_person_week = divide_with_ceil(total_budget, total_person_weeks)
  
  assert_eq(521, budget_per_person_week)
}

test "network_data_transfer" {
  // 网络数据传输计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  // 计算基础传输时间
  let base_transfer_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 计算开销时间
  let overhead_time = divide_with_ceil(multiply(base_transfer_time, overhead_percentage), 100)
  
  // 计算总传输时间
  let total_transfer_time = add(base_transfer_time, overhead_time)
  
  assert_eq(19, total_transfer_time)
}

test "manufacturing_production_efficiency" {
  // 制造业生产效率计算
  let target_production = 1000
  let actual_production = 950
  let defect_rate = 2
  
  // 计算生产效率百分比
  let efficiency_percentage = divide_with_ceil(multiply(actual_production, 100), target_production)
  
  // 计算合格产品数量
  let good_products = actual_production - divide_with_ceil(multiply(actual_production, defect_rate), 100)
  
  assert_eq(95, efficiency_percentage)
  assert_eq(931, good_products)
}

test "agricultural_planning" {
  // 农业规划计算
  let field_area_hectares = 25
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 250
  let labor_cost_per_hectare = 150
  let fertilizer_cost_per_hectare = 80
  
  // 计算总产量
  let total_yield = multiply(field_area_hectares, crop_yield_per_hectare)
  
  // 计算总收入
  let total_revenue = multiply(total_yield, market_price_per_ton)
  
  // 计算总成本
  let total_labor_cost = multiply(field_area_hectares, labor_cost_per_hectare)
  let total_fertilizer_cost = multiply(field_area_hectares, fertilizer_cost_per_hectare)
  let total_cost = add(total_labor_cost, total_fertilizer_cost)
  
  // 计算利润
  let profit = add(total_revenue, -total_cost)
  
  assert_eq(20000, total_yield)
  assert_eq(5000000, total_revenue)
  assert_eq(5750, total_cost)
  assert_eq(4994250, profit)
}

test "digital_storage_planning" {
  // 数字存储规划计算
  let file_size_mb = 125
  let compression_ratio = 60
  let redundancy_factor = 2
  let storage_devices = 8
  
  // 计算压缩后大小
  let compressed_size = divide_with_ceil(multiply(file_size_mb, compression_ratio), 100)
  
  // 计算包含冗余的总存储需求
  let total_storage_needed = multiply(compressed_size, redundancy_factor)
  
  // 计算每个设备需要存储的容量
  let storage_per_device = divide_with_ceil(total_storage_needed, storage_devices)
  
  assert_eq(75, compressed_size)
  assert_eq(150, total_storage_needed)
  assert_eq(19, storage_per_device)
}

test "energy_consumption_analysis" {
  // 能源消耗分析计算
  let daily_consumption_kwh = 120
  let peak_hours_percentage = 30
  let peak_rate_multiplier = 2
  let base_rate_per_kwh = 8
  let days_in_month = 30
  
  // 计算峰值和非峰值消耗
  let peak_consumption = divide_with_ceil(multiply(daily_consumption_kwh, peak_hours_percentage), 100)
  let off_peak_consumption = add(daily_consumption_kwh, -peak_consumption)
  
  // 计算每日成本
  let daily_peak_cost = multiply(peak_consumption, multiply(base_rate_per_kwh, peak_rate_multiplier))
  let daily_off_peak_cost = multiply(off_peak_consumption, base_rate_per_kwh)
  let daily_total_cost = add(daily_peak_cost, daily_off_peak_cost)
  
  // 计算月度成本
  let monthly_total_cost = multiply(daily_total_cost, days_in_month)
  
  assert_eq(36, peak_consumption)
  assert_eq(84, off_peak_consumption)
  assert_eq(576, daily_peak_cost)
  assert_eq(672, daily_off_peak_cost)
  assert_eq(1248, daily_total_cost)
  assert_eq(37440, monthly_total_cost)
}

test "complex_business_calculation" {
  // 复杂业务计算
  let product_cost = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost_per_item = 5
  let discount_threshold = 1000
  let discount_amount = 50
  
  // 步骤1: 计算商品总价
  let subtotal = multiply(product_cost, quantity)
  
  // 步骤2: 计算运费
  let total_shipping = multiply(shipping_cost_per_item, quantity)
  
  // 步骤3: 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 步骤4: 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 步骤5: 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(1800, subtotal)
  assert_eq(75, total_shipping)
  assert_eq(144, tax_amount)
  assert_eq(2019, pre_discount_total)
  assert_eq(1969, final_total)
}