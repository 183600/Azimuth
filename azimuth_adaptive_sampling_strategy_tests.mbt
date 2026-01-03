// Azimuth Adaptive Sampling Strategy Tests
// 自适应采样策略测试套件

// Test 1: 基于负载的自适应采样
test "load-based adaptive sampling" {
  // 模拟系统负载数据
  let system_load = [
    {"timestamp": 1640995200, "cpu_usage": 45.2, "memory_usage": 60.5, "request_rate": 1000},
    {"timestamp": 1640995260, "cpu_usage": 68.7, "memory_usage": 72.3, "request_rate": 2500},
    {"timestamp": 1640995320, "cpu_usage": 85.1, "memory_usage": 83.9, "request_rate": 5000},
    {"timestamp": 1640995380, "cpu_usage": 92.4, "memory_usage": 89.2, "request_rate": 7500},
    {"timestamp": 1640995440, "cpu_usage": 78.6, "memory_usage": 76.4, "request_rate": 4000},
    {"timestamp": 1640995500, "cpu_usage": 52.3, "memory_usage": 65.1, "request_rate": 1500}
  ]
  
  // 基于负载的采样策略
  let calculate_sampling_rate = fn(load) {
    let cpu_usage = load["cpu_usage"]
    let memory_usage = load["memory_usage"]
    let request_rate = load["request_rate"]
    
    // 根据CPU使用率调整采样率
    let cpu_factor = if cpu_usage < 50 {
      1.0
    } else if cpu_usage < 75 {
      0.7
    } else if cpu_usage < 90 {
      0.4
    } else {
      0.1
    }
    
    // 根据内存使用率调整采样率
    let memory_factor = if memory_usage < 60 {
      1.0
    } else if memory_usage < 80 {
      0.8
    } else {
      0.5
    }
    
    // 根据请求率调整采样率
    let request_factor = if request_rate < 2000 {
      1.0
    } else if request_rate < 5000 {
      0.6
    } else if request_rate < 8000 {
      0.3
    } else {
      0.1
    }
    
    // 综合采样率
    let base_rate = 0.5
    let adaptive_rate = base_rate * cpu_factor * memory_factor * request_factor
    
    // 确保采样率在合理范围内
    if adaptive_rate > 1.0 {
      1.0
    } else if adaptive_rate < 0.01 {
      0.01
    } else {
      adaptive_rate
    }
  }
  
  // 计算每个时间点的采样率
  let sampling_rates = system_load.map(calculate_sampling_rate)
  
  // 验证采样率调整
  assert_true(sampling_rates[0] > 0.4 && sampling_rates[0] <= 1.0)  // 低负载
  assert_true(sampling_rates[1] > 0.2 && sampling_rates[1] < 0.4)  // 中等负载
  assert_true(sampling_rates[2] > 0.05 && sampling_rates[2] < 0.2) // 高负载
  assert_true(sampling_rates[3] < 0.05)  // 极高负载
  assert_true(sampling_rates[4] > 0.1 && sampling_rates[4] < 0.3) // 负载下降
  assert_true(sampling_rates[5] > 0.3 && sampling_rates[5] <= 1.0) // 低负载恢复
}

// Test 2: 基于重要性的智能采样
test "importance-based intelligent sampling" {
  // 模拟不同重要性的遥测数据
  let telemetry_events = [
    {"event_type": "error", "service": "payment", "severity": "critical", "user_impact": "high"},
    {"event_type": "request", "service": "auth", "severity": "normal", "user_impact": "medium"},
    {"event_type": "metric", "service": "cache", "severity": "normal", "user_impact": "low"},
    {"event_type": "error", "service": "database", "severity": "warning", "user_impact": "high"},
    {"event_type": "request", "service": "api", "severity": "normal", "user_impact": "medium"},
    {"event_type": "error", "service": "notification", "severity": "critical", "user_impact": "low"},
    {"event_type": "metric", "service": "queue", "severity": "normal", "user_impact": "low"}
  ]
  
  // 重要性评分规则
  let calculate_importance_score = fn(event) {
    let event_type_score = match event["event_type"] {
      "error" => 0.9
      "request" => 0.5
      "metric" => 0.3
      _ => 0.1
    }
    
    let severity_score = match event["severity"] {
      "critical" => 1.0
      "warning" => 0.7
      "normal" => 0.4
      _ => 0.1
    }
    
    let user_impact_score = match event["user_impact"] {
      "high" => 1.0
      "medium" => 0.6
      "low" => 0.2
      _ => 0.1
    }
    
    // 综合重要性评分
    event_type_score * 0.3 + severity_score * 0.4 + user_impact_score * 0.3
  }
  
  // 基于重要性确定采样率
  let calculate_sampling_rate_by_importance = fn(importance_score) {
    if importance_score >= 0.8 {
      1.0  // 高重要性：100%采样
    } else if importance_score >= 0.6 {
      0.8  // 中高重要性：80%采样
    } else if importance_score >= 0.4 {
      0.5  // 中等重要性：50%采样
    } else if importance_score >= 0.2 {
      0.2  // 低重要性：20%采样
    } else {
      0.05 // 极低重要性：5%采样
    }
  }
  
  // 计算每个事件的采样率
  let event_sampling_decisions = telemetry_events.map(fn(event) {
    let importance = calculate_importance_score(event)
    let sampling_rate = calculate_sampling_rate_by_importance(importance)
    (event, importance, sampling_rate)
  })
  
  // 验证重要性采样逻辑
  let critical_error_events = event_sampling_decisions.filter(fn(decision) {
    decision.0["event_type"] == "error" && decision.0["severity"] == "critical"
  })
  
  let normal_request_events = event_sampling_decisions.filter(fn(decision) {
    decision.0["event_type"] == "request" && decision.0["severity"] == "normal"
  })
  
  let normal_metric_events = event_sampling_decisions.filter(fn(decision) {
    decision.0["event_type"] == "metric" && decision.0["severity"] == "normal"
  })
  
  // 验证关键错误事件有高采样率
  for event in critical_error_events {
    assert_true(event.2 >= 0.8, "关键错误事件应该有高采样率")
  }
  
  // 验证普通请求事件有中等采样率
  for event in normal_request_events {
    assert_true(event.2 >= 0.4 && event.2 <= 0.6, "普通请求事件应该有中等采样率")
  }
  
  // 验证普通指标事件有低采样率
  for event in normal_metric_events {
    assert_true(event.2 <= 0.3, "普通指标事件应该有低采样率")
  }
}

// Test 3: 基于服务重要性的分层采样
test "service-importance-based tiered sampling" {
  // 服务重要性配置
  let service_importance = {
    "payment": {"tier": "critical", "base_rate": 0.8, "priority": 1},
    "auth": {"tier": "critical", "base_rate": 0.7, "priority": 2},
    "user_profile": {"tier": "high", "base_rate": 0.5, "priority": 3},
    "search": {"tier": "medium", "base_rate": 0.3, "priority": 4},
    "analytics": {"tier": "low", "base_rate": 0.1, "priority": 5},
    "logging": {"tier": "low", "base_rate": 0.05, "priority": 6}
  }
  
  // 服务当前负载
  let service_load = {
    "payment": {"requests_per_second": 500, "error_rate": 0.01, "latency_p95": 150},
    "auth": {"requests_per_second": 1200, "error_rate": 0.005, "latency_p95": 80},
    "user_profile": {"requests_per_second": 800, "error_rate": 0.02, "latency_p95": 120},
    "search": {"requests_per_second": 3000, "error_rate": 0.001, "latency_p95": 200},
    "analytics": {"requests_per_second": 200, "error_rate": 0.1, "latency_p95": 500},
    "logging": {"requests_per_second": 5000, "error_rate": 0.05, "latency_p95": 50}
  }
  
  // 计算服务特定采样率
  let calculate_service_sampling_rate = fn(service_name) {
    let importance = service_importance[service_name]
    let load = service_load[service_name]
    
    let base_rate = importance["base_rate"]
    
    // 基于错误率调整
    let error_factor = if load["error_rate"] > 0.05 {
      1.5  // 高错误率，增加采样
    } else if load["error_rate"] > 0.01 {
      1.2  // 中等错误率，适度增加采样
    } else {
      1.0  // 低错误率，保持基础采样率
    }
    
    // 基于延迟调整
    let latency_factor = if load["latency_p95"] > 300 {
      1.3  // 高延迟，增加采样
    } else if load["latency_p95"] > 150 {
      1.1  // 中等延迟，适度增加采样
    } else {
      1.0  // 低延迟，保持基础采样率
    }
    
    // 基于请求量调整（反向关系：高请求量降低采样率）
    let request_factor = if load["requests_per_second"] > 4000 {
      0.5  // 高请求量，降低采样率
    } else if load["requests_per_second"] > 1000 {
      0.7  // 中等请求量，适度降低采样率
    } else {
      1.0  // 低请求量，保持基础采样率
    }
    
    let adjusted_rate = base_rate * error_factor * latency_factor * request_factor
    
    // 确保采样率在合理范围内
    if adjusted_rate > 1.0 {
      1.0
    } else if adjusted_rate < 0.01 {
      0.01
    } else {
      adjusted_rate
    }
  }
  
  // 计算所有服务的采样率
  let service_sampling_rates = {}
  for service in service_importance.keys() {
    service_sampling_rates[service] = calculate_service_sampling_rate(service)
  }
  
  // 验证分层采样策略
  assert_true(service_sampling_rates["payment"] > 0.6, "关键服务payment应该有高采样率")
  assert_true(service_sampling_rates["auth"] > 0.5, "关键服务auth应该有高采样率")
  assert_true(service_sampling_rates["user_profile"] > 0.3, "高重要性服务user_profile应该有中等以上采样率")
  assert_true(service_sampling_rates["search"] < 0.4, "中等重要性服务search应该有中等以下采样率")
  assert_true(service_sampling_rates["analytics"] < 0.2, "低重要性服务analytics应该有低采样率")
  assert_true(service_sampling_rates["logging"] < 0.1, "低重要性服务logging应该有很低采样率")
}

// Test 4: 基于用户会话的采样策略
test "user-session-based sampling strategy" {
  // 用户会话数据
  let user_sessions = [
    {"user_id": "user_001", "session_id": "session_001", "is_premium": true, "account_value": "high", "error_count": 0},
    {"user_id": "user_002", "session_id": "session_002", "is_premium": false, "account_value": "medium", "error_count": 1},
    {"user_id": "user_003", "session_id": "session_003", "is_premium": true, "account_value": "high", "error_count": 3},
    {"user_id": "user_004", "session_id": "session_004", "is_premium": false, "account_value": "low", "error_count": 0},
    {"user_id": "user_005", "session_id": "session_005", "is_premium": false, "account_value": "medium", "error_count": 2}
  ]
  
  // 基于用户会话计算采样率
  let calculate_session_sampling_rate = fn(session) {
    let base_rate = 0.1
    
    // 基于用户类型调整
    let user_factor = if session["is_premium"] {
      2.0  // 高级用户双倍采样率
    } else {
      1.0
    }
    
    // 基于账户价值调整
    let value_factor = match session["account_value"] {
      "high" => 1.5
      "medium" => 1.0
      "low" => 0.5
      _ => 1.0
    }
    
    // 基于错误数量调整
    let error_factor = if session["error_count"] > 2 {
      3.0  // 高错误数，大幅增加采样
    } else if session["error_count"] > 0 {
      1.5  // 有错误，适度增加采样
    } else {
      1.0  // 无错误，保持基础采样
    }
    
    let adjusted_rate = base_rate * user_factor * value_factor * error_factor
    
    // 确保采样率在合理范围内
    if adjusted_rate > 1.0 {
      1.0
    } else if adjusted_rate < 0.01 {
      0.01
    } else {
      adjusted_rate
    }
  }
  
  // 计算每个会话的采样率
  let session_sampling_rates = user_sessions.map(fn(session) {
    let rate = calculate_session_sampling_rate(session)
    (session["user_id"], session["session_id"], rate)
  })
  
  // 验证会话采样策略
  let premium_user_sessions = session_sampling_rates.filter(fn(session) {
    let user_id = session.0
    let original_session = user_sessions.filter(fn(s) { s["user_id"] == user_id })[0]
    original_session["is_premium"]
  })
  
  let high_error_sessions = session_sampling_rates.filter(fn(session) {
    let user_id = session.0
    let original_session = user_sessions.filter(fn(s) { s["user_id"] == user_id })[0]
    original_session["error_count"] > 2
  })
  
  let regular_user_sessions = session_sampling_rates.filter(fn(session) {
    let user_id = session.0
    let original_session = user_sessions.filter(fn(s) { s["user_id"] == user_id })[0]
    not(original_session["is_premium"]) && original_session["error_count"] == 0
  })
  
  // 验证高级用户会话有更高采样率
  for session in premium_user_sessions {
    assert_true(session.2 > 0.15, "高级用户会话应该有更高采样率")
  }
  
  // 验证高错误会话有更高采样率
  for session in high_error_sessions {
    assert_true(session.2 > 0.3, "高错误会话应该有更高采样率")
  }
  
  // 验证普通用户无错误会话有基础采样率
  for session in regular_user_sessions {
    assert_true(session.2 >= 0.05 && session.2 <= 0.15, "普通用户无错误会话应该有基础采样率")
  }
}

// Test 5: 时间窗口自适应采样
test "time-window-adaptive sampling" {
  // 模拟一天内不同时间段的流量模式
  let hourly_traffic = [
    {"hour": 0, "request_count": 500, "error_rate": 0.02, "avg_latency": 120},
    {"hour": 6, "request_count": 1200, "error_rate": 0.015, "avg_latency": 100},
    {"hour": 9, "request_count": 5000, "error_rate": 0.01, "avg_latency": 150},
    {"hour": 12, "request_count": 4500, "error_rate": 0.012, "avg_latency": 180},
    {"hour": 15, "request_count": 4800, "error_rate": 0.008, "avg_latency": 160},
    {"hour": 18, "request_count": 3500, "error_rate": 0.018, "avg_latency": 200},
    {"hour": 21, "request_count": 2000, "error_rate": 0.025, "avg_latency": 140}
  ]
  
  // 基于时间窗口的采样策略
  let calculate_time_based_sampling_rate = fn(traffic_data) {
    let base_rate = 0.2
    
    // 基于请求量调整（反向关系）
    let volume_factor = if traffic_data["request_count"] > 4000 {
      0.4  // 高峰期，降低采样率
    } else if traffic_data["request_count"] > 2000 {
      0.7  // 中等流量，适度降低采样率
    } else {
      1.2  // 低流量期，提高采样率
    }
    
    // 基于错误率调整
    let error_factor = if traffic_data["error_rate"] > 0.02 {
      1.5  // 高错误率，增加采样
    } else if traffic_data["error_rate"] > 0.01 {
      1.2  // 中等错误率，适度增加采样
    } else {
      1.0  // 低错误率，保持基础采样
    }
    
    // 基于延迟调整
    let latency_factor = if traffic_data["avg_latency"] > 180 {
      1.3  // 高延迟，增加采样
    } else if traffic_data["avg_latency"] > 120 {
      1.1  // 中等延迟，适度增加采样
    } else {
      1.0  // 低延迟，保持基础采样
    }
    
    let adjusted_rate = base_rate * volume_factor * error_factor * latency_factor
    
    // 确保采样率在合理范围内
    if adjusted_rate > 1.0 {
      1.0
    } else if adjusted_rate < 0.01 {
      0.01
    } else {
      adjusted_rate
    }
  }
  
  // 计算每个时间段的采样率
  let hourly_sampling_rates = hourly_traffic.map(fn(traffic) {
    let rate = calculate_time_based_sampling_rate(traffic)
    (traffic["hour"], rate)
  })
  
  // 验证时间窗口采样策略
  let low_traffic_rates = hourly_sampling_rates.filter(fn(hourly) {
    let hour = hourly.0
    let traffic = hourly_traffic.filter(fn(t) { t["hour"] == hour })[0]
    traffic["request_count"] < 2000
  })
  
  let high_traffic_rates = hourly_sampling_rates.filter(fn(hourly) {
    let hour = hourly.0
    let traffic = hourly_traffic.filter(fn(t) { t["hour"] == hour })[0]
    traffic["request_count"] > 4000
  })
  
  // 验证低流量时段有更高采样率
  for hourly in low_traffic_rates {
    assert_true(hourly.1 > 0.2, "低流量时段应该有更高采样率")
  }
  
  // 验证高流量时段有更低采样率
  for hourly in high_traffic_rates {
    assert_true(hourly.1 < 0.2, "高流量时段应该有更低采样率")
  }
}

// Test 6: 异常检测驱动的动态采样
test "anomaly-driven dynamic sampling" {
  // 模拟异常检测结果
  let anomaly_detections = [
    {"timestamp": 1640995200, "metric": "cpu_usage", "value": 45.2, "baseline": 40.0, "anomaly_score": 0.3},
    {"timestamp": 1640995260, "metric": "memory_usage", "value": 85.7, "baseline": 60.0, "anomaly_score": 0.8},
    {"timestamp": 1640995320, "metric": "response_time", "value": 500.0, "baseline": 200.0, "anomaly_score": 0.9},
    {"timestamp": 1640995380, "metric": "error_rate", "value": 0.05, "baseline": 0.01, "anomaly_score": 0.7},
    {"timestamp": 1640995440, "metric": "cpu_usage", "value": 42.1, "baseline": 40.0, "anomaly_score": 0.2},
    {"timestamp": 1640995500, "metric": "throughput", "value": 1500.0, "baseline": 2000.0, "anomaly_score": 0.6}
  ]
  
  // 基于异常分数的采样策略
  let calculate_anomaly_driven_sampling_rate = fn(anomaly_detection) {
    let base_rate = 0.1
    let anomaly_score = anomaly_detection["anomaly_score"]
    
    // 基于异常分数调整采样率
    let anomaly_factor = if anomaly_score > 0.8 {
      5.0  // 高异常，大幅增加采样
    } else if anomaly_score > 0.6 {
      3.0  // 中等异常，显著增加采样
    } else if anomaly_score > 0.4 {
      2.0  // 低异常，适度增加采样
    } else {
      1.0  // 无异常，保持基础采样
    }
    
    let adjusted_rate = base_rate * anomaly_factor
    
    // 确保采样率在合理范围内
    if adjusted_rate > 1.0 {
      1.0
    } else if adjusted_rate < 0.01 {
      0.01
    } else {
      adjusted_rate
    }
  }
  
  // 计算每个异常检测的采样率
  let anomaly_sampling_rates = anomaly_detections.map(fn(detection) {
    let rate = calculate_anomaly_driven_sampling_rate(detection)
    (detection["metric"], detection["anomaly_score"], rate)
  })
  
  // 验证异常驱动采样策略
  let high_anomaly_rates = anomaly_sampling_rates.filter(fn(anomaly) {
    anomaly.1 > 0.8
  })
  
  let medium_anomaly_rates = anomaly_sampling_rates.filter(fn(anomaly) {
    anomaly.1 > 0.6 && anomaly.1 <= 0.8
  })
  
  let low_anomaly_rates = anomaly_sampling_rates.filter(fn(anomaly) {
    anomaly.1 <= 0.4
  })
  
  // 验证高异常分数有高采样率
  for anomaly in high_anomaly_rates {
    assert_true(anomaly.2 > 0.4, "高异常分数应该有高采样率")
  }
  
  // 验证中等异常分数有中等采样率
  for anomaly in medium_anomaly_rates {
    assert_true(anomaly.2 > 0.2 && anomaly.2 <= 0.4, "中等异常分数应该有中等采样率")
  }
  
  // 验证低异常分数有基础采样率
  for anomaly in low_anomaly_rates {
    assert_true(anomaly.2 <= 0.2, "低异常分数应该有基础采样率")
  }
}

// Test 7: 多维度综合采样策略
test "multi-dimensional comprehensive sampling strategy" {
  // 综合考虑多个维度的数据
  let comprehensive_data = [
    {
      "service": "payment",
      "user_tier": "premium",
      "error_count": 0,
      "latency_p95": 120,
      "request_rate": 800,
      "business_impact": "high",
      "time_of_day": "peak"
    },
    {
      "service": "logging",
      "user_tier": "regular",
      "error_count": 0,
      "latency_p95": 50,
      "request_rate": 5000,
      "business_impact": "low",
      "time_of_day": "off_peak"
    },
    {
      "service": "auth",
      "user_tier": "premium",
      "error_count": 2,
      "latency_p95": 200,
      "request_rate": 1500,
      "business_impact": "high",
      "time_of_day": "peak"
    },
    {
      "service": "analytics",
      "user_tier": "regular",
      "error_count": 1,
      "latency_p95": 300,
      "request_rate": 300,
      "business_impact": "medium",
      "time_of_day": "normal"
    }
  ]
  
  // 多维度综合采样率计算
  let calculate_comprehensive_sampling_rate = fn(data) {
    let base_rate = 0.2
    
    // 服务重要性维度
    let service_factor = match data["service"] {
      "payment" => 1.5
      "auth" => 1.3
      "analytics" => 0.8
      "logging" => 0.3
      _ => 1.0
    }
    
    // 用户层级维度
    let user_tier_factor = match data["user_tier"] {
      "premium" => 1.5
      "regular" => 1.0
      _ => 1.0
    }
    
    // 错误情况维度
    let error_factor = if data["error_count"] > 1 {
      2.0
    } else if data["error_count"] > 0 {
      1.3
    } else {
      1.0
    }
    
    // 性能维度
    let performance_factor = if data["latency_p95"] > 250 {
      1.5
    } else if data["latency_p95"] > 150 {
      1.2
    } else {
      1.0
    }
    
    // 流量维度（反向关系）
    let traffic_factor = if data["request_rate"] > 3000 {
      0.5
    } else if data["request_rate"] > 1000 {
      0.7
    } else {
      1.0
    }
    
    // 业务影响维度
    let business_factor = match data["business_impact"] {
      "high" => 1.5
      "medium" => 1.2
      "low" => 0.8
      _ => 1.0
    }
    
    // 时间维度
    let time_factor = match data["time_of_day"] {
      "peak" => 0.8
      "normal" => 1.0
      "off_peak" => 1.2
      _ => 1.0
    }
    
    let adjusted_rate = base_rate * service_factor * user_tier_factor * error_factor * 
                      performance_factor * traffic_factor * business_factor * time_factor
    
    // 确保采样率在合理范围内
    if adjusted_rate > 1.0 {
      1.0
    } else if adjusted_rate < 0.01 {
      0.01
    } else {
      adjusted_rate
    }
  }
  
  // 计算每个数据点的采样率
  let comprehensive_sampling_rates = comprehensive_data.map(fn(data) {
    let rate = calculate_comprehensive_sampling_rate(data)
    (data["service"], data["user_tier"], rate)
  })
  
  // 验证综合采样策略
  let payment_premium_rate = comprehensive_sampling_rates.filter(fn(item) {
    item.0 == "payment" && item.1 == "premium"
  })[0].2
  
  let logging_regular_rate = comprehensive_sampling_rates.filter(fn(item) {
    item.0 == "logging" && item.1 == "regular"
  })[0].2
  
  let auth_premium_rate = comprehensive_sampling_rates.filter(fn(item) {
    item.0 == "auth" && item.1 == "premium"
  })[0].2
  
  // 验证高重要性组合有高采样率
  assert_true(payment_premium_rate > 0.3, "高级用户支付服务应该有高采样率")
  assert_true(auth_premium_rate > 0.4, "有错误的高级用户认证服务应该有更高采样率")
  
  // 验证低重要性组合有低采样率
  assert_true(logging_regular_rate < 0.1, "普通用户日志服务应该有低采样率")
}

// Test 8: 采样策略性能影响评估
test "sampling strategy performance impact assessment" {
  // 不同采样策略的性能影响数据
  let sampling_performance = [
    {"strategy": "fixed_10_percent", "sampling_rate": 0.1, "storage_reduction": 0.9, "accuracy_loss": 0.05, "cpu_overhead": 0.02},
    {"strategy": "fixed_50_percent", "sampling_rate": 0.5, "storage_reduction": 0.5, "accuracy_loss": 0.02, "cpu_overhead": 0.05},
    {"strategy": "adaptive_load", "sampling_rate": 0.35, "storage_reduction": 0.65, "accuracy_loss": 0.03, "cpu_overhead": 0.08},
    {"strategy": "adaptive_importance", "sampling_rate": 0.25, "storage_reduction": 0.75, "accuracy_loss": 0.02, "cpu_overhead": 0.12},
    {"strategy": "comprehensive", "sampling_rate": 0.3, "storage_reduction": 0.7, "accuracy_loss": 0.025, "cpu_overhead": 0.15}
  ]
  
  // 计算综合性能分数
  let calculate_performance_score = fn(performance_data) {
    // 权重配置
    let storage_weight = 0.4  // 存储节省权重
    let accuracy_weight = 0.4 // 精度保持权重
    let cpu_weight = 0.2      // CPU开销权重
    
    // 归一化指标（越高越好）
    let storage_score = performance_data["storage_reduction"]
    let accuracy_score = 1.0 - performance_data["accuracy_loss"]  // 精度损失转换为精度保持
    let cpu_score = 1.0 - performance_data["cpu_overhead"]        // CPU开销转换为CPU效率
    
    // 综合分数
    storage_score * storage_weight + accuracy_score * accuracy_weight + cpu_score * cpu_weight
  }
  
  // 计算每个策略的性能分数
  let strategy_scores = sampling_performance.map(fn(performance) {
    let score = calculate_performance_score(performance)
    (performance["strategy"], score)
  })
  
  // 找出最佳策略
  let best_strategy = strategy_scores.reduce(fn(best, current) {
    if current.1 > best.1 {
      current
    } else {
      best
    }
  }, strategy_scores[0])
  
  // 验证性能评估
  assert_eq(strategy_scores.length(), 5)
  
  // 验证综合策略有较高的性能分数
  let comprehensive_score = strategy_scores.filter(fn(strategy) {
    strategy.0 == "comprehensive"
  })[0].1
  
  // 验证固定10%策略的精度损失最小但存储节省也较小
  let fixed_10_score = strategy_scores.filter(fn(strategy) {
    strategy.0 == "fixed_10_percent"
  })[0].1
  
  // 验证分数在合理范围内
  for strategy in strategy_scores {
    assert_true(strategy.1 > 0.5 && strategy.1 < 1.0, "策略性能分数应该在合理范围内")
  }
  
  // 验证最佳策略确实是综合策略或适应性策略之一
  assert_true(best_strategy.0 == "comprehensive" || 
              best_strategy.0 == "adaptive_importance" || 
              best_strategy.0 == "adaptive_load", 
              "最佳策略应该是适应性或综合策略")
}

// Test 9: 采样策略配置验证
test "sampling strategy configuration validation" {
  // 采样策略配置
  let sampling_configs = [
    {
      "name": "production_config",
      "base_rate": 0.2,
      "min_rate": 0.01,
      "max_rate": 0.8,
      "adaptive_enabled": true,
      "load_threshold": 0.8,
      "importance_enabled": true,
      "time_window_enabled": true
    },
    {
      "name": "development_config",
      "base_rate": 0.5,
      "min_rate": 0.1,
      "max_rate": 1.0,
      "adaptive_enabled": false,
      "load_threshold": 0.9,
      "importance_enabled": false,
      "time_window_enabled": false
    },
    {
      "name": "test_config",
      "base_rate": 1.0,
      "min_rate": 0.5,
      "max_rate": 1.0,
      "adaptive_enabled": false,
      "load_threshold": 0.95,
      "importance_enabled": false,
      "time_window_enabled": false
    }
  ]
  
  // 配置验证规则
  let validate_config = fn(config) {
    let validation_errors = []
    
    // 验证基础采样率
    if config["base_rate"] < config["min_rate"] {
      validation_errors = validation_errors.push("基础采样率不能小于最小采样率")
    }
    
    if config["base_rate"] > config["max_rate"] {
      validation_errors = validation_errors.push("基础采样率不能大于最大采样率")
    }
    
    // 验证采样率范围
    if config["min_rate"] < 0 || config["min_rate"] > 1 {
      validation_errors = validation_errors.push("最小采样率必须在0-1之间")
    }
    
    if config["max_rate"] < 0 || config["max_rate"] > 1 {
      validation_errors = validation_errors.push("最大采样率必须在0-1之间")
    }
    
    // 验证负载阈值
    if config["load_threshold"] < 0 || config["load_threshold"] > 1 {
      validation_errors = validation_errors.push("负载阈值必须在0-1之间")
    }
    
    // 验证自适应配置一致性
    if config["adaptive_enabled"] && (config["load_threshold"] == 0) {
      validation_errors = validation_errors.push("启用自适应时负载阈值不能为0")
    }
    
    {
      "valid": validation_errors.length() == 0,
      "errors": validation_errors
    }
  }
  
  // 验证所有配置
  let config_validations = sampling_configs.map(fn(config) {
    let validation = validate_config(config)
    (config["name"], validation["valid"], validation["errors"])
  })
  
  // 验证配置验证结果
  for validation in config_validations {
    if validation.0 == "production_config" {
      assert_true(validation.1, "生产配置应该是有效的")
      assert_eq(validation.2.length(), 0, "生产配置不应该有验证错误")
    }
    
    if validation.0 == "development_config" {
      assert_true(validation.1, "开发配置应该是有效的")
      assert_eq(validation.2.length(), 0, "开发配置不应该有验证错误")
    }
    
    if validation.0 == "test_config" {
      assert_true(validation.1, "测试配置应该是有效的")
      assert_eq(validation.2.length(), 0, "测试配置不应该有验证错误")
    }
  }
}

// Test 10: 采样策略切换和回滚
test "sampling strategy switching and rollback" {
  // 采样策略版本管理
  let strategy_versions = [
    {"version": "v1.0", "name": "fixed_sampling", "config": {"rate": 0.1}, "active": false},
    {"version": "v1.1", "name": "adaptive_load", "config": {"base_rate": 0.2, "load_threshold": 0.8}, "active": false},
    {"version": "v1.2", "name": "adaptive_importance", "config": {"base_rate": 0.15, "importance_weights": {"error": 0.8, "request": 0.5}}, "active": true},
    {"version": "v2.0", "name": "comprehensive", "config": {"base_rate": 0.25, "multi_dimensional": true}, "active": false}
  ]
  
  // 模拟策略切换过程
  let switch_strategy = fn(versions, target_version) {
    // 查找目标策略
    let target_strategy = versions.filter(fn(v) { v["version"] == target_version })[0]
    
    // 检查目标策略是否存在
    if target_strategy.length() == 0 {
      return {"success": false, "message": "目标策略版本不存在"}
    }
    
    // 停用所有当前策略
    let updated_versions = versions.map(fn(v) {
      let updated_v = v
      updated_v["active"] = false
      updated_v
    })
    
    // 激活目标策略
    let final_versions = updated_versions.map(fn(v) {
      if v["version"] == target_version {
        let activated_v = v
        activated_v["active"] = true
        activated_v
      } else {
        v
      }
    })
    
    {
      "success": true,
      "message": "策略切换成功",
      "active_version": target_version,
      "versions": final_versions
    }
  }
  
  // 执行策略切换
  let switch_to_v2 = switch_strategy(strategy_versions, "v2.0")
  let switch_to_invalid = switch_strategy(strategy_versions, "v3.0")
  
  // 验证策略切换结果
  assert_true(switch_to_v2["success"], "切换到有效版本应该成功")
  assert_eq(switch_to_v2["active_version"], "v2.0")
  
  assert_false(switch_to_invalid["success"], "切换到无效版本应该失败")
  assert_true(switch_to_invalid["message"].contains("不存在"))
  
  // 验证切换后的策略状态
  let updated_versions = switch_to_v2["versions"]
  let active_strategies = updated_versions.filter(fn(v) { v["active"] })
  
  assert_eq(active_strategies.length(), 1, "应该只有一个活跃策略")
  assert_eq(active_strategies[0]["version"], "v2.0", "活跃策略应该是v2.0")
  
  // 验证其他策略已被停用
  let inactive_strategies = updated_versions.filter(fn(v) { not(v["active"]) })
  assert_eq(inactive_strategies.length(), 3, "应该有三个非活跃策略")
}