// Azimuth 新增测试用例集合
// 涵盖遥测系统的高级功能和边缘情况测试

// 测试1: 遥测数据的异常检测
test "遥测数据异常检测" {
  // 模拟正常和异常的遥测数据
  let telemetry_data = [
    (1640995200, "cpu", 45.0, "normal"),
    (1640995260, "cpu", 48.0, "normal"),
    (1640995320, "cpu", 95.0, "anomaly"),
    (1640995380, "cpu", 42.0, "normal"),
    (1640995440, "memory", 1024.0, "normal"),
    (1640995500, "memory", 4096.0, "anomaly")
  ]
  
  // 异常检测算法：基于阈值
  let cpu_threshold = 80.0
  let memory_threshold = 2048.0
  
  let mut detected_anomalies = []
  
  for data_point in telemetry_data {
    let is_anomaly = match data_point.1 {
      "cpu" => data_point.2 > cpu_threshold
      "memory" => data_point.2 > memory_threshold
      _ => false
    }
    
    if is_anomaly {
      let severity = if data_point.2 > (cpu_threshold * 1.5) || data_point.2 > (memory_threshold * 1.5) {
        "critical"
      } else {
        "warning"
      }
      
      detected_anomalies = detected_anomalies.push((data_point.0, data_point.1, data_point.2, severity))
    }
  }
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 2)
  assert_eq(detected_anomalies[0].1, "cpu")
  assert_eq(detected_anomalies[0].2, 95.0)
  assert_eq(detected_anomalies[0].3, "warning")
  assert_eq(detected_anomalies[1].1, "memory")
  assert_eq(detected_anomalies[1].2, 4096.0)
  assert_eq(detected_anomalies[1].3, "critical")
}

// 测试2: 遥测数据的智能采样策略
test "遥测数据智能采样策略" {
  // 模拟高频遥测数据
  let mut high_frequency_data = []
  let mut i = 0
  while i < 100 {
    let importance = if i % 10 == 0 { "high" } else { "normal" }
    high_frequency_data = high_frequency_data.push((i, 1640995200 + i * 10, "cpu", 45.0 + (i % 20).to_int().to_float() * 0.5, importance))
    i = i + 1
  }
  
  // 智能采样策略：保留重要数据点和代表性数据
  let sampling_rate = 0.3 // 30%采样率
  let target_sample_size = (high_frequency_data.length().to_float() * sampling_rate).to_int()
  
  let mut sampled_data = []
  let mut normal_sample_count = 0
  
  for data_point in high_frequency_data {
    let should_sample = match data_point.4 {
      "high" => true // 高重要性数据始终保留
      "normal" => normal_sample_count < target_sample_size && (data_point.0 % 3 == 0) // 普通数据按间隔采样
      _ => false
    }
    
    if should_sample {
      sampled_data = sampled_data.push(data_point)
      if data_point.4 == "normal" {
        normal_sample_count = normal_sample_count + 1
      }
    }
  }
  
  // 验证采样结果
  assert_true(sampled_data.length() <= target_sample_size + 10) // 允许高重要性数据的额外数量
  
  // 验证所有高重要性数据都被保留
  let mut high_importance_count = 0
  for data in sampled_data {
    if data.4 == "high" {
      high_importance_count = high_importance_count + 1
    }
  }
  assert_eq(high_importance_count, 10) // 原始数据中有10个高重要性数据点
  
  // 验证采样数据的代表性
  let mut sum = 0.0
  for data in sampled_data {
    sum = sum + data.3
  }
  let avg_sampled_value = sum / sampled_data.length().to_float()
  
  let mut original_sum = 0.0
  for data in high_frequency_data {
    original_sum = original_sum + data.3
  }
  let avg_original_value = original_sum / high_frequency_data.length().to_float()
  
  // 采样平均值应该接近原始平均值（允许5%的误差）
  let error_margin = avg_original_value * 0.05
  assert_true(avg_sampled_value >= avg_original_value - error_margin)
  assert_true(avg_sampled_value <= avg_original_value + error_margin)
}

// 测试3: 遥测数据的自适应配置管理
test "遥测数据自适应配置管理" {
  // 模拟不同环境下的遥测配置
  let base_config = (0.5, 100, 60, true) // (sampling_rate, batch_size, transmission_interval, compression_enabled)
  
  // 模拟环境条件
  let network_conditions = [
    ("production", "high", "low", "high"),
    ("staging", "medium", "medium", "medium"),
    ("development", "low", "high", "low")
  ]
  
  // 自适应配置调整
  let mut adaptive_configs = []
  
  for condition in network_conditions {
    let adjusted_config = match condition.0 {
      "production" => {
        // 生产环境：高采样率，大批次，低传输频率
        (0.8, 200, 120, true)
      }
      "staging" => {
        // 测试环境：中等配置
        (0.5, 100, 60, true)
      }
      "development" => {
        // 开发环境：低采样率，小批次，高传输频率，禁用压缩以便调试
        (0.2, 50, 30, false)
      }
      _ => base_config
    }
    adaptive_configs = adaptive_configs.push((condition.0, adjusted_config.0, adjusted_config.1, adjusted_config.2, adjusted_config.3))
  }
  
  // 验证自适应配置
  assert_eq(adaptive_configs.length(), 3)
  
  // 验证生产环境配置
  assert_eq(adaptive_configs[0].0, "production")
  assert_eq(adaptive_configs[0].1, 0.8)
  assert_eq(adaptive_configs[0].2, 200)
  assert_eq(adaptive_configs[0].3, 120)
  assert_eq(adaptive_configs[0].4, true)
  
  // 验证开发环境配置
  assert_eq(adaptive_configs[2].0, "development")
  assert_eq(adaptive_configs[2].1, 0.2)
  assert_eq(adaptive_configs[2].2, 50)
  assert_eq(adaptive_configs[2].3, 30)
  assert_eq(adaptive_configs[2].4, false)
  
  // 验证配置优化：生产环境应该有最高的数据效率
  let production_efficiency = adaptive_configs[0].1 * adaptive_configs[0].2.to_float() / adaptive_configs[0].3.to_float()
  let development_efficiency = adaptive_configs[2].1 * adaptive_configs[2].2.to_float() / adaptive_configs[2].3.to_float()
  
  assert_true(production_efficiency > development_efficiency)
}

// 测试4: 遥测数据的跨服务关联分析
test "遥测数据跨服务关联分析" {
  // 模拟跨服务的遥测数据
  let service_telemetry = [
    ("auth-service", "trace-123", "login", 150, "success", 1640995200),
    ("user-service", "trace-123", "get_profile", 80, "success", 1640995201),
    ("order-service", "trace-124", "create_order", 300, "success", 1640995202),
    ("payment-service", "trace-124", "process_payment", 500, "error", 1640995203),
    ("notification-service", "trace-125", "send_email", 200, "success", 1640995204)
  ]
  
  // 按trace_id分组进行关联分析
  let mut trace_groups = {}
  
  for telemetry in service_telemetry {
    let trace_id = telemetry.1
    if trace_groups.contains(trace_id) {
      let existing_group = trace_groups[trace_id]
      trace_groups = trace_groups.set(trace_id, existing_group.push(telemetry))
    } else {
      trace_groups = trace_groups.set(trace_id, [telemetry])
    }
  }
  
  // 分析每个trace的性能
  let mut trace_analysis = []
  
  for trace_id in trace_groups.keys() {
    let trace_data = trace_groups[trace_id]
    let mut total_duration = 0
    let mut service_count = 0
    let mut has_error = false
    let mut services = []
    
    for telemetry in trace_data {
      total_duration = total_duration + telemetry.3
      service_count = service_count + 1
      if telemetry.4 == "error" {
        has_error = true
      }
      services = services.push(telemetry.0)
    }
    
    trace_analysis = trace_analysis.push((trace_id, total_duration, service_count, has_error, services, total_duration / service_count))
  }
  
  // 验证跨服务关联分析结果
  assert_eq(trace_analysis.length(), 3) // 应该有3个不同的trace
  
  // 验证trace-123的分析结果
  let trace_123 = trace_analysis.filter(fn(t) { t.0 == "trace-123" })[0]
  assert_eq(trace_123.2, 2)
  assert_eq(trace_123.1, 230)
  assert_eq(trace_123.5, 115)
  assert_eq(trace_123.3, false)
  
  // 验证trace-124的分析结果（包含错误）
  let trace_124 = trace_analysis.filter(fn(t) { t.0 == "trace-124" })[0]
  assert_eq(trace_124.2, 2)
  assert_eq(trace_124.1, 800)
  assert_eq(trace_124.5, 400)
  assert_eq(trace_124.3, true)
  
  // 验证trace-125的分析结果（单个服务）
  let trace_125 = trace_analysis.filter(fn(t) { t.0 == "trace-125" })[0]
  assert_eq(trace_125.2, 1)
  assert_eq(trace_125.1, 200)
  assert_eq(trace_125.5, 200)
  assert_eq(trace_125.3, false)
  
  // 验证性能分析：有错误的trace应该有更长的平均持续时间
  let error_traces = trace_analysis.filter(fn(t) { t.3 })
  let success_traces = trace_analysis.filter(fn(t) { not(t.3) })
  
  assert_true(error_traces.length() > 0)
  assert_true(success_traces.length() > 0)
  
  let error_avg_duration = error_traces.reduce(fn(acc, t) { acc + t.5 }, 0) / error_traces.length()
  let success_avg_duration = success_traces.reduce(fn(acc, t) { acc + t.5 }, 0) / success_traces.length()
  
  assert_true(error_avg_duration > success_avg_duration)
}

// 测试5: 遥测数据的实时仪表板聚合
test "遥测数据实时仪表板聚合" {
  // 模拟实时遥测数据流
  let real_time_metrics = [
    ("api-gateway", "request_count", 1000, 1640995200),
    ("api-gateway", "error_rate", 2.5, 1640995200),
    ("user-service", "request_count", 800, 1640995200),
    ("user-service", "response_time", 120, 1640995200),
    ("order-service", "request_count", 300, 1640995200),
    ("order-service", "response_time", 250, 1640995200),
    ("payment-service", "request_count", 150, 1640995200),
    ("payment-service", "error_rate", 1.2, 1640995200)
  ]
  
  // 仪表板聚合：按服务分组
  let mut service_dashboards = {}
  
  for metric in real_time_metrics {
    let service = metric.0
    if service_dashboards.contains(service) {
      let existing_dashboard = service_dashboards[service]
      service_dashboards = service_dashboards.set(service, existing_dashboard.push(metric))
    } else {
      service_dashboards = service_dashboards.set(service, [metric])
    }
  }
  
  // 计算每个服务的关键指标
  let mut dashboard_summary = []
  
  for service in service_dashboards.keys() {
    let metrics = service_dashboards[service]
    let mut request_count = 0
    let mut error_rate = 0.0
    let mut response_time = 0
    let mut error_rate_count = 0
    let mut response_time_count = 0
    
    for metric in metrics {
      match metric.1 {
        "request_count" => request_count = request_count + metric.2.to_int()
        "error_rate" => {
          error_rate = error_rate + metric.2
          error_rate_count = error_rate_count + 1
        }
        "response_time" => {
          response_time = response_time + metric.2.to_int()
          response_time_count = response_time_count + 1
        }
        _ => ()
      }
    }
    
    let avg_error_rate = if error_rate_count > 0 { error_rate / error_rate_count.to_float() } else { 0.0 }
    let avg_response_time = if response_time_count > 0 { response_time / response_time_count } else { 0 }
    
    let health_status = if avg_error_rate > 5.0 || avg_response_time > 200 {
      "critical"
    } else if avg_error_rate > 2.0 || avg_response_time > 100 {
      "warning"
    } else {
      "healthy"
    }
    
    dashboard_summary = dashboard_summary.push((service, request_count, avg_error_rate, avg_response_time, health_status))
  }
  
  // 验证仪表板聚合结果
  assert_eq(dashboard_summary.length(), 4) // 应该有4个服务
  
  // 验证api-gateway的聚合结果
  let api_gateway = dashboard_summary.filter(fn(d) { d.0 == "api-gateway" })[0]
  assert_eq(api_gateway.1, 1000)
  assert_eq(api_gateway.2, 2.5)
  assert_eq(api_gateway.3, 0)
  assert_eq(api_gateway.4, "warning") // 错误率在2-5%之间
  
  // 验证order-service的聚合结果
  let order_service = dashboard_summary.filter(fn(d) { d.0 == "order-service" })[0]
  assert_eq(order_service.1, 300)
  assert_eq(order_service.2, 0.0)
  assert_eq(order_service.3, 250)
  assert_eq(order_service.4, "critical") // 响应时间超过200ms
  
  // 验证payment-service的聚合结果
  let payment_service = dashboard_summary.filter(fn(d) { d.0 == "payment-service" })[0]
  assert_eq(payment_service.1, 150)
  assert_eq(payment_service.2, 1.2)
  assert_eq(payment_service.3, 0)
  assert_eq(payment_service.4, "healthy") // 错误率低于2%
  
  // 验证健康状态统计
  let healthy_services = dashboard_summary.filter(fn(d) { d.4 == "healthy" })
  let warning_services = dashboard_summary.filter(fn(d) { d.4 == "warning" })
  let critical_services = dashboard_summary.filter(fn(d) { d.4 == "critical" })
  
  assert_eq(healthy_services.length(), 1)
  assert_eq(warning_services.length(), 1)
  assert_eq(critical_services.length(), 2)
}

// 测试6: 遥测数据的预测性分析
test "遥测数据预测性分析" {
  // 模拟历史遥测数据
  let historical_data = [
    (1640995200, 45.0, 1024.0, 1000),
    (1640995260, 48.0, 1050.0, 1100),
    (1640995320, 52.0, 1100.0, 1200),
    (1640995380, 55.0, 1150.0, 1300),
    (1640995440, 58.0, 1200.0, 1400),
    (1640995500, 62.0, 1280.0, 1550),
    (1640995560, 65.0, 1350.0, 1700),
    (1640995620, 68.0, 1420.0, 1850)
  ]
  
  // 简单线性回归预测（简化版）
  let calculate_trend = fn(values) {
    let n = values.length()
    if n < 2 { 0.0 } else {
      let first_value = values[0]
      let last_value = values[n-1]
      (last_value - first_value) / (n - 1).to_float()
    }
  }
  
  let cpu_values = historical_data.map(fn(d) { d.1 })
  let memory_values = historical_data.map(fn(d) { d.2 })
  let request_values = historical_data.map(fn(d) { d.3.to_float() })
  
  let cpu_trend = calculate_trend(cpu_values)
  let memory_trend = calculate_trend(memory_values)
  let request_trend = calculate_trend(request_values)
  
  // 预测下一个时间点的值
  let next_cpu = cpu_values[cpu_values.length()-1] + cpu_trend
  let next_memory = memory_values[memory_values.length()-1] + memory_trend
  let next_request_count = request_values[request_values.length()-1] + request_trend
  
  // 预测未来3个时间点的值
  let mut predictions = []
  let mut i = 1
  while i <= 3 {
    let future_cpu = cpu_values[cpu_values.length()-1] + cpu_trend * i.to_float()
    let future_memory = memory_values[memory_values.length()-1] + memory_trend * i.to_float()
    let future_request_count = request_values[request_values.length()-1] + request_trend * i.to_float()
    
    let risk_level = if future_cpu > 80.0 || future_memory > 2000.0 {
      "high"
    } else if future_cpu > 70.0 || future_memory > 1500.0 {
      "medium"
    } else {
      "low"
    }
    
    predictions = predictions.push((i, future_cpu, future_memory, future_request_count, risk_level))
    i = i + 1
  }
  
  // 验证预测结果
  assert_eq(predictions.length(), 3)
  
  // 验证趋势计算
  assert_true(cpu_trend > 0.0) // CPU使用率应该呈上升趋势
  assert_true(memory_trend > 0.0) // 内存使用量应该呈上升趋势
  assert_true(request_trend > 0.0) // 请求数应该呈上升趋势
  
  // 验证下一个时间点的预测值
  assert_true(next_cpu > 68.0) // 下一个CPU值应该大于最后一个历史值
  assert_true(next_memory > 1420.0) // 下一个内存值应该大于最后一个历史值
  assert_true(next_request_count > 1850.0) // 下一个请求数应该大于最后一个历史值
  
  // 验证风险等级评估
  let high_risk_predictions = predictions.filter(fn(p) { p.4 == "high" })
  let medium_risk_predictions = predictions.filter(fn(p) { p.4 == "medium" })
  let low_risk_predictions = predictions.filter(fn(p) { p.4 == "low" })
  
  // 根据趋势，未来预测中应该有中等或高风险的预测
  assert_true(medium_risk_predictions.length() > 0 || high_risk_predictions.length() > 0)
  
  // 验证预测值的合理性
  for prediction in predictions {
    assert_true(prediction.1 > 0.0)
    assert_true(prediction.2 > 0.0)
    assert_true(prediction.3 > 0.0)
    assert_true(prediction.0 >= 1 && prediction.0 <= 3)
  }
}

// 测试7: 遥测数据的智能告警系统
test "遥测数据智能告警系统" {
  // 模拟触发性遥测数据
  let alert_triggers = [
    ("cpu", 85.0, 80.0, "warning", "api-gateway", 1640995200),
    ("memory", 90.0, 85.0, "critical", "user-service", 1640995260),
    ("error_rate", 6.0, 5.0, "critical", "order-service", 1640995320),
    ("response_time", 150.0, 200.0, "warning", "payment-service", 1640995380),
    ("disk_space", 95.0, 90.0, "critical", "database", 1640995440)
  ]
  
  // 智能告警规则引擎
  let mut active_alerts = []
  let mut suppressed_alerts = []
  
  for trigger in alert_triggers {
    let is_triggered = trigger.1 > trigger.2
    
    if is_triggered {
      // 告警抑制逻辑：相同服务的相同严重级别告警在5分钟内只发送一次
      let recent_similar_alert = active_alerts.any(fn(alert) {
        alert.4 == trigger.4 && 
        alert.3 == trigger.3 && 
        (trigger.5 - alert.5) < 300 // 5分钟
      })
      
      if not recent_similar_alert {
        let alert_id = "alert-" + trigger.4 + "-" + trigger.0
        
        // 告警升级规则：如果多个服务同时有critical告警，升级为系统级告警
        let critical_count = active_alerts.filter(fn(a) { a.3 == "critical" }).length()
        let final_severity = if trigger.3 == "critical" && critical_count >= 2 {
          "system_critical"
        } else {
          trigger.3
        }
        
        active_alerts = active_alerts.push((alert_id, trigger.0, trigger.1, trigger.2, final_severity, trigger.4, trigger.5))
      } else {
        suppressed_alerts = suppressed_alerts.push((trigger.4, trigger.0, "Duplicate alert within suppression window"))
      }
    }
  }
  
  // 验证智能告警结果
  assert_eq(active_alerts.length(), 4) // 应该有4个活跃告警（response_time没有超过阈值）
  assert_eq(suppressed_alerts.length(), 0) // 在这个例子中，没有重复告警被抑制
  
  // 验证告警严重性分布
  let warning_alerts = active_alerts.filter(fn(alert) { alert.3 == "warning" })
  let critical_alerts = active_alerts.filter(fn(alert) { alert.3 == "critical" })
  let system_critical_alerts = active_alerts.filter(fn(alert) { alert.3 == "system_critical" })
  
  assert_eq(warning_alerts.length(), 1) // CPU告警
  assert_true(critical_alerts.length() >= 2) // 至少有2个critical告警
  assert_true(system_critical_alerts.length() >= 1) // 由于有多个critical告警，应该有系统级告警
  
  // 验证告警消息生成
  let cpu_alert = active_alerts.filter(fn(alert) { alert.1 == "cpu" })[0]
  assert_eq(cpu_alert.4, "api-gateway")
  assert_eq(cpu_alert.1, "cpu")
  assert_eq(cpu_alert.2, 85.0)
  assert_eq(cpu_alert.3, "warning")
  
  // 验证告警升级逻辑
  if system_critical_alerts.length() > 0 {
    assert_eq(system_critical_alerts[0].3, "system_critical")
  }
}

// 测试8: 遥测数据的多维分析
test "遥测数据多维分析" {
  // 模拟多维遥测数据
  let multi_dimensional_data = [
    ("auth", "us-east", "v1.2", "latency", 120, 1640995200),
    ("auth", "us-east", "v1.2", "throughput", 1000, 1640995200),
    ("auth", "us-west", "v1.1", "latency", 150, 1640995200),
    ("auth", "us-west", "v1.1", "throughput", 800, 1640995200),
    ("payment", "us-east", "v2.0", "latency", 200, 1640995200),
    ("payment", "us-east", "v2.0", "throughput", 500, 1640995200),
    ("payment", "eu-west", "v1.9", "latency", 180, 1640995200),
    ("payment", "eu-west", "v1.9", "throughput", 600, 1640995200)
  ]
  
  // 多维分析：按服务、区域、版本分组
  let mut dimensions = {}
  
  for data_point in multi_dimensional_data {
    let service = data_point.0
    let region = data_point.1
    let version = data_point.2
    let dimension_key = service + ":" + region + ":" + version
    
    if dimensions.contains(dimension_key) {
      let existing_data = dimensions[dimension_key]
      dimensions = dimensions.set(dimension_key, existing_data.push(data_point))
    } else {
      dimensions = dimensions.set(dimension_key, [data_point])
    }
  }
  
  // 计算每个维度的聚合指标
  let mut dimension_analysis = []
  
  for dimension_key in dimensions.keys() {
    let data_points = dimensions[dimension_key]
    let mut latency_sum = 0
    let mut throughput_sum = 0
    let mut latency_count = 0
    let mut throughput_count = 0
    
    for point in data_points {
      match point.3 {
        "latency" => {
          latency_sum = latency_sum + point.4
          latency_count = latency_count + 1
        }
        "throughput" => {
          throughput_sum = throughput_sum + point.4
          throughput_count = throughput_count + 1
        }
        _ => ()
      }
    }
    
    let avg_latency = if latency_count > 0 { latency_sum / latency_count } else { 0 }
    let total_throughput = if throughput_count > 0 { throughput_sum } else { 0 }
    
    let parts = dimension_key.split(":")
    let service = parts[0]
    let region = parts[1]
    let version = parts[2]
    
    let performance_score = if avg_latency < 150 && total_throughput > 800 {
      "excellent"
    } else if avg_latency < 200 && total_throughput > 500 {
      "good"
    } else {
      "needs_improvement"
    }
    
    dimension_analysis = dimension_analysis.push((dimension_key, service, region, version, avg_latency, total_throughput, performance_score))
  }
  
  // 验证多维分析结果
  assert_eq(dimension_analysis.length(), 4) // 应该有4个不同的维度组合
  
  // 验证auth服务的分析结果
  let auth_us_east = dimension_analysis.filter(fn(d) { d.1 == "auth" && d.2 == "us-east" })[0]
  assert_eq(auth_us_east.3, "v1.2")
  assert_eq(auth_us_east.4, 120)
  assert_eq(auth_us_east.5, 1000)
  assert_eq(auth_us_east.6, "excellent")
  
  let auth_us_west = dimension_analysis.filter(fn(d) { d.1 == "auth" && d.2 == "us-west" })[0]
  assert_eq(auth_us_west.3, "v1.1")
  assert_eq(auth_us_west.4, 150)
  assert_eq(auth_us_west.5, 800)
  assert_eq(auth_us_west.6, "excellent")
  
  // 验证payment服务的分析结果
  let payment_us_east = dimension_analysis.filter(fn(d) { d.1 == "payment" && d.2 == "us-east" })[0]
  assert_eq(payment_us_east.3, "v2.0")
  assert_eq(payment_us_east.4, 200)
  assert_eq(payment_us_east.5, 500)
  assert_eq(payment_us_east.6, "good")
  
  let payment_eu_west = dimension_analysis.filter(fn(d) { d.1 == "payment" && d.2 == "eu-west" })[0]
  assert_eq(payment_eu_west.3, "v1.9")
  assert_eq(payment_eu_west.4, 180)
  assert_eq(payment_eu_west.5, 600)
  assert_eq(payment_eu_west.6, "good")
  
  // 验证跨区域性能比较
  let auth_regions = dimension_analysis.filter(fn(d) { d.1 == "auth" })
  let payment_regions = dimension_analysis.filter(fn(d) { d.1 == "payment" })
  
  assert_eq(auth_regions.length(), 2)
  assert_eq(payment_regions.length(), 2)
  
  // 验证版本性能比较
  let v1_auth = dimension_analysis.filter(fn(d) { d.1 == "auth" && d.3.starts_with("v1.") })
  let v2_payment = dimension_analysis.filter(fn(d) { d.1 == "payment" && d.3.starts_with("v2.") })
  
  assert_eq(v1_auth.length(), 2)
  assert_eq(v2_payment.length(), 1)
  
  // 验证性能评分分布
  let excellent_performance = dimension_analysis.filter(fn(d) { d.6 == "excellent" })
  let good_performance = dimension_analysis.filter(fn(d) { d.6 == "good" })
  let needs_improvement = dimension_analysis.filter(fn(d) { d.6 == "needs_improvement" })
  
  assert_eq(excellent_performance.length(), 2)
  assert_eq(good_performance.length(), 2)
  assert_eq(needs_improvement.length(), 0)
}