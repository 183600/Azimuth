// 遥测自定义仪表测试用例

test "custom_counter_instrumentation" {
  // 测试自定义计数器仪表
  
  let business_metrics = [
    ("user_registrations_total", 1250),
    ("orders_completed_total", 890),
    ("payments_processed_total", 875),
    ("items_added_to_cart_total", 2340),
    ("user_login_total", 5670)
  ]
  
  // 验证业务指标数量
  assert_eq(business_metrics.length(), 5)
  
  // 验证用户注册指标
  assert_eq(business_metrics[0].0, "user_registrations_total")
  assert_eq(business_metrics[0].1 > 1000, true)
  assert_eq(business_metrics[0].1 < 1500, true)
  
  // 验证订单完成指标
  assert_eq(business_metrics[1].0, "orders_completed_total")
  assert_eq(business_metrics[1].1 > 800, true)
  assert_eq(business_metrics[1].1 < 1000, true)
  
  // 计算转化率 (订单完成 / 用户注册)
  let conversion_rate = (business_metrics[1].1 * 100) / business_metrics[0].1
  assert_eq(conversion_rate, 71) // 890/1250 * 100 ≈ 71%
  
  // 计算支付成功率 (支付处理 / 订单完成)
  let payment_success_rate = (business_metrics[2].1 * 100) / business_metrics[1].1
  assert_eq(payment_success_rate, 98) // 875/890 * 100 ≈ 98%
  
  // 验证购物车添加指标
  assert_eq(business_metrics[3].0, "items_added_to_cart_total")
  assert_eq(business_metrics[3].1 > 2000, true)
  
  // 计算购物车转化率 (订单完成 / 购物车添加)
  let cart_conversion_rate = (business_metrics[1].1 * 100) / business_metrics[3].1
  assert_eq(cart_conversion_rate, 38) // 890/2340 * 100 ≈ 38%
}

test "custom_gauge_instrumentation" {
  // 测试自定义仪表仪表
  
  let system_gauges = [
    ("active_user_sessions", 342),
    ("pending_orders", 28),
    ("queue_depth", 156),
    ("cache_size_mb", 512),
    ("connection_pool_active", 45)
  ]
  
  // 验证系统仪表数量
  assert_eq(system_gauges.length(), 5)
  
  // 验证活跃用户会话
  assert_eq(system_gauges[0].0, "active_user_sessions")
  assert_eq(system_gauges[0].1 > 300, true)
  assert_eq(system_gauges[0].1 < 400, true)
  
  // 验证待处理订单
  assert_eq(system_gauges[1].0, "pending_orders")
  assert_eq(system_gauges[1].1 < 50, true)
  
  // 验证队列深度
  assert_eq(system_gauges[2].0, "queue_depth")
  assert_eq(system_gauges[2].1 > 100, true)
  
  // 验证缓存大小
  assert_eq(system_gauges[3].0, "cache_size_mb")
  assert_eq(system_gauges[3].1, 512)
  
  // 验证连接池活跃连接
  assert_eq(system_gauges[4].0, "connection_pool_active")
  assert_eq(system_gauges[4].1 < 100, true)
  
  // 计算系统负载指标
  let total_system_load = system_gauges[0].1 + system_gauges[2].1 + system_gauges[4].1
  assert_eq(total_system_load, 543) // 342 + 156 + 45
  
  // 检测潜在问题
  let issues = []
  
  // 检查待处理订单是否过多
  if system_gauges[1].1 > 25 {
    issues.push("high_pending_orders")
  }
  
  // 检查队列深度是否过大
  if system_gauges[2].1 > 150 {
    issues.push("high_queue_depth")
  }
  
  // 验证问题检测
  assert_eq(issues.length(), 2)
  assert_eq(issues[0], "high_pending_orders")
  assert_eq(issues[1], "high_queue_depth")
}

test "custom_histogram_instrumentation" {
  // 测试自定义直方图仪表
  
  let response_time_histogram = [
    ("le_0.1", 120),    // ≤100ms
    ("le_0.25", 280),   // ≤250ms
    ("le_0.5", 450),    // ≤500ms
    ("le_1.0", 680),    // ≤1s
    ("le_2.5", 890),    // ≤2.5s
    ("le_5.0", 980),    // ≤5s
    ("le_inf", 1000)    // ≤∞ (总数)
  ]
  
  // 验证直方图桶数量
  assert_eq(response_time_histogram.length(), 7)
  
  // 验证总请求数
  assert_eq(response_time_histogram[6].1, 1000)
  
  // 计算各范围内的请求数
  let fast_requests = response_time_histogram[1].1          // ≤250ms
  let medium_requests = response_time_histogram[3].1 - response_time_histogram[1].1  // 250ms-1s
  let slow_requests = response_time_histogram[6].1 - response_time_histogram[3].1   // >1s
  
  // 验证请求数分布
  assert_eq(fast_requests, 280)
  assert_eq(medium_requests, 400) // 680 - 280
  assert_eq(slow_requests, 320)  // 1000 - 680
  
  // 计算百分比
  let fast_percentage = (fast_requests * 100) / response_time_histogram[6].1
  let medium_percentage = (medium_requests * 100) / response_time_histogram[6].1
  let slow_percentage = (slow_requests * 100) / response_time_histogram[6].1
  
  // 验证百分比分布
  assert_eq(fast_percentage, 28)
  assert_eq(medium_percentage, 40)
  assert_eq(slow_percentage, 32)
  
  // 验证单调性 (直方图桶值应该递增)
  let mut i = 1
  while i < response_time_histogram.length() {
    assert_eq(response_time_histogram[i].1 >= response_time_histogram[i-1].1, true)
    i = i + 1
  }
}

test "custom_business_metrics" {
  // 测试自定义业务指标
  
  let business_kpis = [
    ("revenue_per_hour", 1250.50),
    ("customer_lifetime_value", 4850.75),
    ("average_order_value", 89.99),
    ("customer_acquisition_cost", 25.50),
    ("monthly_recurring_revenue", 15000.00)
  ]
  
  // 验证业务KPI数量
  assert_eq(business_kpis.length(), 5)
  
  // 验证每小时收入
  assert_eq(business_kpis[0].0, "revenue_per_hour")
  assert_eq(business_kpis[0].1 > 1000.0, true)
  assert_eq(business_kpis[0].1 < 1500.0, true)
  
  // 验证客户终身价值
  assert_eq(business_kpis[1].0, "customer_lifetime_value")
  assert_eq(business_kpis[1].1 > 4000.0, true)
  assert_eq(business_kpis[1].1 < 5000.0, true)
  
  // 验证平均订单价值
  assert_eq(business_kpis[2].0, "average_order_value")
  assert_eq(business_kpis[2].1 > 80.0, true)
  assert_eq(business_kpis[2].1 < 100.0, true)
  
  // 验证客户获取成本
  assert_eq(business_kpis[3].0, "customer_acquisition_cost")
  assert_eq(business_kpis[3].1 < 50.0, true)
  
  // 验证月度经常性收入
  assert_eq(business_kpis[4].0, "monthly_recurring_revenue")
  assert_eq(business_kpis[4].1, 15000.00)
  
  // 计算LTV/CAC比率 (客户终身价值 / 客户获取成本)
  let ltv_cac_ratio = business_kpis[1].1 / business_kpis[3].1
  assert_eq(ltv_cac_ratio > 150.0, true) // 4850.75 / 25.50 ≈ 190
  
  // 计算投资回收期 (CAC / (AOV * 毛利率))，假设毛利率为40%
  let gross_margin = 0.4
  let monthly_profit_per_customer = business_kpis[2].1 * gross_margin
  let payback_period_months = business_kpis[3].1 / monthly_profit_per_customer
  
  assert_eq(payback_period_months > 0.5, true)
  assert_eq(payback_period_months < 1.0, true)
  
  // 计算年度经常性收入
  let annual_recurring_revenue = business_kpis[4].1 * 12.0
  assert_eq(annual_recurring_revenue, 180000.00)
}

test "custom_error_metrics" {
  // 测试自定义错误指标
  
  let error_metrics = [
    ("validation_errors_total", 145),
    ("timeout_errors_total", 23),
    ("authentication_errors_total", 67),
    ("rate_limit_errors_total", 34),
    ("system_errors_total", 12)
  ]
  
  // 验证错误指标数量
  assert_eq(error_metrics.length(), 5)
  
  // 计算总错误数
  let mut total_errors = 0
  for error in error_metrics {
    total_errors = total_errors + error.1
  }
  assert_eq(total_errors, 281)
  
  // 验证验证错误 (最常见)
  assert_eq(error_metrics[0].0, "validation_errors_total")
  assert_eq(error_metrics[0].1 > 100, true)
  
  // 验证系统错误 (最少见)
  assert_eq(error_metrics[4].0, "system_errors_total")
  assert_eq(error_metrics[4].1 < 20, true)
  
  // 计算错误率分布
  let validation_error_rate = (error_metrics[0].1 * 100) / total_errors
  let timeout_error_rate = (error_metrics[1].1 * 100) / total_errors
  let auth_error_rate = (error_metrics[2].1 * 100) / total_errors
  let rate_limit_error_rate = (error_metrics[3].1 * 100) / total_errors
  let system_error_rate = (error_metrics[4].1 * 100) / total_errors
  
  // 验证错误率分布
  assert_eq(validation_error_rate, 51) // 145/281 * 100 ≈ 51%
  assert_eq(timeout_error_rate, 8)    // 23/281 * 100 ≈ 8%
  assert_eq(auth_error_rate, 23)      // 67/281 * 100 ≈ 23%
  assert_eq(rate_limit_error_rate, 12) // 34/281 * 100 ≈ 12%
  assert_eq(system_error_rate, 4)     // 12/281 * 100 ≈ 4%
  
  // 计算严重错误率 (超时 + 系统)
  let severe_error_rate = timeout_error_rate + system_error_rate
  assert_eq(severe_error_rate, 12) // 8% + 4%
  
  // 计算客户端错误率 (验证 + 认证 + 限流)
  let client_error_rate = validation_error_rate + auth_error_rate + rate_limit_error_rate
  assert_eq(client_error_rate, 86) // 51% + 23% + 12%
  
  // 验证错误分类总和
  assert_eq(severe_error_rate + client_error_rate, 98) // 98% (由于四舍五入)
}