// Azimuth 综合遥测功能测试
// 覆盖遥测系统的核心功能和高级特性

// 测试1: 遥测数据异常检测
test "遥测数据异常检测" {
  // 模拟正常和异常的遥测数据
  let telemetry_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, status: "normal" },
    { timestamp: 1640995260, metric: "cpu", value: 50.0, status: "normal" },
    { timestamp: 1640995320, metric: "cpu", value: 95.0, status: "anomaly" },
    { timestamp: 1640995380, metric: "cpu", value: 48.0, status: "normal" },
    { timestamp: 1640995440, metric: "cpu", value: 98.0, status: "anomaly" }
  ]
  
  // 异常检测算法：基于阈值
  let threshold = 80.0
  let mut anomalies = []
  let mut normal_data = []
  
  for data in telemetry_data {
    if data.value > threshold {
      anomalies = anomalies.push({
        timestamp: data.timestamp,
        metric: data.metric,
        value: data.value,
        anomaly_score: (data.value - threshold) / threshold
      })
    } else {
      normal_data = normal_data.push(data)
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)
  assert_eq(normal_data.length(), 3)
  assert_eq(anomalies[0].value, 95.0)
  assert_eq(anomalies[1].value, 98.0)
  assert_true(anomalies[0].anomaly_score > 0.1)
  assert_true(anomalies[1].anomaly_score > 0.1)
}

// 测试2: 遥测数据聚合和统计
test "遥测数据聚合和统计" {
  // 模拟多指标遥测数据
  let multi_metric_data = [
    { timestamp: 1640995200, service: "auth", cpu: 45.0, memory: 1024.0, network: 100.0 },
    { timestamp: 1640995260, service: "auth", cpu: 50.0, memory: 1030.0, network: 120.0 },
    { timestamp: 1640995320, service: "db", cpu: 30.0, memory: 2048.0, network: 80.0 },
    { timestamp: 1640995380, service: "db", cpu: 35.0, memory: 2060.0, network: 90.0 },
    { timestamp: 1640995440, service: "cache", cpu: 25.0, memory: 512.0, network: 200.0 }
  ]
  
  // 按服务分组聚合
  let mut service_stats = []
  
  // 初始化服务统计
  let services = ["auth", "db", "cache"]
  for service in services {
    service_stats = service_stats.push({
      service: service,
      cpu_total: 0.0,
      memory_total: 0.0,
      network_total: 0.0,
      count: 0
    })
  }
  
  // 聚合数据
  for data in multi_metric_data {
    let mut i = 0
    while i < service_stats.length() {
      if service_stats[i].service == data.service {
        service_stats[i] = {
          service: service_stats[i].service,
          cpu_total: service_stats[i].cpu_total + data.cpu,
          memory_total: service_stats[i].memory_total + data.memory,
          network_total: service_stats[i].network_total + data.network,
          count: service_stats[i].count + 1
        }
        break
      }
      i = i + 1
    }
  }
  
  // 计算平均值
  let mut i = 0
  while i < service_stats.length() {
    if service_stats[i].count > 0 {
      service_stats[i] = {
        service: service_stats[i].service,
        cpu_total: service_stats[i].cpu_total / service_stats[i].count.to_float(),
        memory_total: service_stats[i].memory_total / service_stats[i].count.to_float(),
        network_total: service_stats[i].network_total / service_stats[i].count.to_float(),
        count: service_stats[i].count
      }
    }
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(service_stats.length(), 3)
  
  // 验证auth服务统计
  let auth_stats = service_stats[0]
  assert_eq(auth_stats.service, "auth")
  assert_eq(auth_stats.cpu_total, 47.5) // (45 + 50) / 2
  assert_eq(auth_stats.memory_total, 1027.0) // (1024 + 1030) / 2
  assert_eq(auth_stats.network_total, 110.0) // (100 + 120) / 2
  assert_eq(auth_stats.count, 2)
}

// 测试3: 遥测配置动态管理
test "遥测配置动态管理" {
  // 模拟遥测系统配置
  let base_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    flush_interval: 5000,
    metrics_enabled: ["cpu", "memory", "network"],
    log_level: "INFO"
  }
  
  // 模拟配置更新
  let config_updates = [
    { path: "sampling_rate", value: 0.5 },
    { path: "batch_size", value: 200 },
    { path: "metrics_enabled", value: ["cpu", "memory", "network", "disk"] },
    { path: "log_level", value: "DEBUG" }
  ]
  
  // 应用配置更新
  let mut updated_config = base_config
  
  for update in config_updates {
    match update.path {
      "sampling_rate" => updated_config = { 
        sampling_rate: update.value,
        batch_size: updated_config.batch_size,
        flush_interval: updated_config.flush_interval,
        metrics_enabled: updated_config.metrics_enabled,
        log_level: updated_config.log_level
      }
      "batch_size" => updated_config = { 
        sampling_rate: updated_config.sampling_rate,
        batch_size: update.value,
        flush_interval: updated_config.flush_interval,
        metrics_enabled: updated_config.metrics_enabled,
        log_level: updated_config.log_level
      }
      "metrics_enabled" => updated_config = { 
        sampling_rate: updated_config.sampling_rate,
        batch_size: updated_config.batch_size,
        flush_interval: updated_config.flush_interval,
        metrics_enabled: update.value,
        log_level: updated_config.log_level
      }
      "log_level" => updated_config = { 
        sampling_rate: updated_config.sampling_rate,
        batch_size: updated_config.batch_size,
        flush_interval: updated_config.flush_interval,
        metrics_enabled: updated_config.metrics_enabled,
        log_level: update.value
      }
      _ => ()
    }
  }
  
  // 验证配置更新结果
  assert_eq(updated_config.sampling_rate, 0.5)
  assert_eq(updated_config.batch_size, 200)
  assert_eq(updated_config.flush_interval, 5000)
  assert_eq(updated_config.metrics_enabled.length(), 4)
  assert_eq(updated_config.log_level, "DEBUG")
  assert_true(updated_config.metrics_enabled.contains("disk"))
}

// 测试4: 遥测数据生命周期管理
test "遥测数据生命周期管理" {
  // 模拟遥测数据的生命周期阶段
  let data_lifecycle = [
    { id: 1, stage: "collection", timestamp: 1640995200, retention_days: 7 },
    { id: 2, stage: "processing", timestamp: 1640995260, retention_days: 7 },
    { id: 3, stage: "aggregation", timestamp: 1640995320, retention_days: 30 },
    { id: 4, stage: "archival", timestamp: 1640995380, retention_days: 365 },
    { id: 5, stage: "deletion", timestamp: 1640995440, retention_days: 0 }
  ]
  
  // 数据生命周期管理
  let current_time = 1640995500
  let mut active_data = []
  let mut archival_data = []
  let mut expired_data = []
  
  for data in data_lifecycle {
    let age_days = (current_time - data.timestamp) / 86400 // 简化计算
    
    if data.stage == "deletion" {
      expired_data = expired_data.push(data)
    } else if age_days >= data.retention_days {
      archival_data = archival_data.push(data)
    } else {
      active_data = active_data.push(data)
    }
  }
  
  // 验证生命周期管理结果
  assert_eq(active_data.length(), 2)
  assert_eq(archival_data.length(), 2)
  assert_eq(expired_data.length(), 1)
  
  // 验证活跃数据
  assert_eq(active_data[0].stage, "collection")
  assert_eq(active_data[1].stage, "processing")
  
  // 验证归档数据
  assert_eq(archival_data[0].stage, "aggregation")
  assert_eq(archival_data[1].stage, "archival")
  
  // 验证过期数据
  assert_eq(expired_data[0].stage, "deletion")
}

// 测试5: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 模拟原始遥测数据
  let raw_telemetry = [
    { service: "auth", metric: "cpu", value: 45.0, timestamp: 1640995200, tags: ["region:us-east", "env:prod"] },
    { service: "db", metric: "memory", value: 2048.0, timestamp: 1640995260, tags: ["region:us-west", "env:prod"] }
  ]
  
  // 序列化为紧凑格式
  let mut serialized_data = []
  
  for data in raw_telemetry {
    // 简化的序列化过程：将数据转换为字符串表示
    let serialized_entry = data.service + "|" + data.metric + "|" + 
                          data.value.to_string() + "|" + data.timestamp.to_string() + "|" +
                          data.tags.join(",")
    
    serialized_data = serialized_data.push(serialized_entry)
  }
  
  // 反序列化过程
  let mut deserialized_data = []
  
  for entry in serialized_data {
    let parts = entry.split("|")
    if parts.length() >= 4 {
      let tags_str = if parts.length() > 4 { parts[4] } else { "" }
      let tags = if tags_str != "" { tags_str.split(",") } else { [] }
      
      deserialized_data = deserialized_data.push({
        service: parts[0],
        metric: parts[1],
        value: parts[2].to_float(),
        timestamp: parts[3].to_int(),
        tags: tags
      })
    }
  }
  
  // 验证序列化和反序列化结果
  assert_eq(deserialized_data.length(), 2)
  assert_eq(deserialized_data[0].service, "auth")
  assert_eq(deserialized_data[0].metric, "cpu")
  assert_eq(deserialized_data[0].value, 45.0)
  assert_eq(deserialized_data[0].timestamp, 1640995200)
  assert_eq(deserialized_data[0].tags.length(), 2)
  
  assert_eq(deserialized_data[1].service, "db")
  assert_eq(deserialized_data[1].metric, "memory")
  assert_eq(deserialized_data[1].value, 2048.0)
  assert_eq(deserialized_data[1].timestamp, 1640995260)
  assert_eq(deserialized_data[1].tags.length(), 2)
}

// 测试6: 遥测数据实时告警
test "遥测数据实时告警" {
  // 模拟实时遥测数据和告警规则
  let alert_rules = [
    { name: "high_cpu", metric: "cpu", threshold: 80.0, operator: ">", severity: "critical" },
    { name: "low_memory", metric: "memory", threshold: 500.0, operator: "<", severity: "warning" },
    { name: "high_network", metric: "network", threshold: 150.0, operator: ">", severity: "warning" }
  ]
  
  let live_telemetry = [
    { timestamp: 1640995200, service: "auth", cpu: 85.0, memory: 600.0, network: 120.0 },
    { timestamp: 1640995260, service: "db", cpu: 45.0, memory: 400.0, network: 160.0 },
    { timestamp: 1640995320, service: "cache", cpu: 90.0, memory: 800.0, network: 180.0 }
  ]
  
  // 实时告警检测
  let mut triggered_alerts = []
  
  for data in live_telemetry {
    for rule in alert_rules {
      let metric_value = match rule.metric {
        "cpu" => data.cpu
        "memory" => data.memory
        "network" => data.network
        _ => 0.0
      }
      
      let alert_triggered = match rule.operator {
        ">" => metric_value > rule.threshold
        "<" => metric_value < rule.threshold
        ">=" => metric_value >= rule.threshold
        "<=" => metric_value <= rule.threshold
        "==" => metric_value == rule.threshold
        _ => false
      }
      
      if alert_triggered {
        triggered_alerts = triggered_alerts.push({
          timestamp: data.timestamp,
          service: data.service,
          alert_name: rule.name,
          metric: rule.metric,
          value: metric_value,
          threshold: rule.threshold,
          severity: rule.severity
        })
      }
    }
  }
  
  // 验证告警检测结果
  assert_eq(triggered_alerts.length(), 4)
  
  // 验证第一个数据点的告警
  assert_eq(triggered_alerts[0].service, "auth")
  assert_eq(triggered_alerts[0].alert_name, "high_cpu")
  assert_eq(triggered_alerts[0].value, 85.0)
  assert_eq(triggered_alerts[0].severity, "critical")
  
  // 验证第二个数据点的告警
  assert_eq(triggered_alerts[1].service, "db")
  assert_eq(triggered_alerts[1].alert_name, "low_memory")
  assert_eq(triggered_alerts[2].service, "db")
  assert_eq(triggered_alerts[2].alert_name, "high_network")
  
  // 验证第三个数据点的告警
  assert_eq(triggered_alerts[3].service, "cache")
  assert_eq(triggered_alerts[3].alert_name, "high_cpu")
}

// 测试7: 遥测数据自适应采样
test "遥测数据自适应采样" {
  // 模拟自适应采样策略
  let sampling_strategy = {
    base_rate: 1.0,
    high_volume_threshold: 1000,
    high_volume_rate: 0.1,
    error_threshold: 10,
    error_rate: 1.0
  }
  
  // 模拟不同条件下的遥测数据流
  let data_streams = [
    { name: "normal", volume: 500, error_count: 2 },
    { name: "high_volume", volume: 1500, error_count: 5 },
    { name: "high_error", volume: 800, error_count: 15 },
    { name: "critical", volume: 2000, error_count: 50 }
  ]
  
  // 自适应采样计算
  let mut adjusted_sampling_rates = []
  
  for stream in data_streams {
    let mut sampling_rate = sampling_strategy.base_rate
    
    // 基于数据量调整采样率
    if stream.volume > sampling_strategy.high_volume_threshold {
      sampling_rate = sampling_rate * sampling_strategy.high_volume_rate
    }
    
    // 基于错误率调整采样率
    let error_rate = stream.error_count.to_float() / stream.volume.to_float()
    if error_rate > (sampling_strategy.error_threshold.to_float() / 100.0) {
      sampling_rate = sampling_rate * sampling_strategy.error_rate
    }
    
    // 确保采样率在合理范围内
    if sampling_rate > 1.0 { sampling_rate = 1.0 }
    if sampling_rate < 0.01 { sampling_rate = 0.01 }
    
    adjusted_sampling_rates = adjusted_sampling_rates.push({
      stream_name: stream.name,
      original_rate: sampling_strategy.base_rate,
      adjusted_rate: sampling_rate,
      volume: stream.volume,
      error_count: stream.error_count,
      error_rate: error_rate
    })
  }
  
  // 验证自适应采样结果
  assert_eq(adjusted_sampling_rates.length(), 4)
  
  // 验证正常流量采样率
  let normal_stream = adjusted_sampling_rates[0]
  assert_eq(normal_stream.stream_name, "normal")
  assert_eq(normal_stream.adjusted_rate, 1.0) // 保持原始采样率
  
  // 验证高流量采样率
  let high_volume_stream = adjusted_sampling_rates[1]
  assert_eq(high_volume_stream.stream_name, "high_volume")
  assert_eq(high_volume_stream.adjusted_rate, 0.1) // 降低采样率
  
  // 验证高错误率采样率
  let high_error_stream = adjusted_sampling_rates[2]
  assert_eq(high_error_stream.stream_name, "high_error")
  assert_eq(high_error_stream.adjusted_rate, 1.0) // 提高采样率
  
  // 验证临界状态采样率
  let critical_stream = adjusted_sampling_rates[3]
  assert_eq(critical_stream.stream_name, "critical")
  assert_eq(critical_stream.adjusted_rate, 0.1) // 同时应用高流量和高错误率规则
}

// 测试8: 遥测数据分布式追踪
test "遥测数据分布式追踪" {
  // 模拟分布式追踪数据
  let trace_data = [
    { trace_id: "trace123", span_id: "span1", parent_span: "", service: "gateway", operation: "request", duration: 10 },
    { trace_id: "trace123", span_id: "span2", parent_span: "span1", service: "auth", operation: "authenticate", duration: 50 },
    { trace_id: "trace123", span_id: "span3", parent_span: "span2", service: "db", operation: "query", duration: 30 },
    { trace_id: "trace123", span_id: "span4", parent_span: "span1", service: "cache", operation: "get", duration: 5 },
    { trace_id: "trace456", span_id: "span5", parent_span: "", service: "gateway", operation: "request", duration: 15 }
  ]
  
  // 按追踪ID分组
  let mut trace_groups = []
  let processed_traces = []
  
  for span in trace_data {
    if not processed_traces.contains(span.trace_id) {
      processed_traces = processed_traces.push(span.trace_id)
      
      let mut trace_spans = []
      for s in trace_data {
        if s.trace_id == span.trace_id {
          trace_spans = trace_spans.push(s)
        }
      }
      
      trace_groups = trace_groups.push({
        trace_id: span.trace_id,
        spans: trace_spans
      })
    }
  }
  
  // 分析追踪数据
  let mut trace_analysis = []
  
  for trace in trace_groups {
    let mut total_duration = 0
    let mut service_breakdown = []
    let processed_services = []
    
    // 计算总持续时间和服务分解
    for span in trace.spans {
      total_duration = total_duration + span.duration
      
      if not processed_services.contains(span.service) {
        processed_services = processed_services.push(span.service)
        
        let mut service_duration = 0
        let mut span_count = 0
        
        for s in trace.spans {
          if s.service == span.service {
            service_duration = service_duration + s.duration
            span_count = span_count + 1
          }
        }
        
        service_breakdown = service_breakdown.push({
          service: span.service,
          total_duration: service_duration,
          span_count: span_count,
          avg_duration: service_duration / span_count
        })
      }
    }
    
    trace_analysis = trace_analysis.push({
      trace_id: trace.trace_id,
      total_duration: total_duration,
      span_count: trace.spans.length(),
      service_count: service_breakdown.length(),
      service_breakdown: service_breakdown
    })
  }
  
  // 验证分布式追踪分析结果
  assert_eq(trace_analysis.length(), 2)
  
  // 验证第一个追踪
  let trace123 = trace_analysis[0]
  assert_eq(trace123.trace_id, "trace123")
  assert_eq(trace123.span_count, 4)
  assert_eq(trace123.service_count, 3)
  assert_eq(trace123.total_duration, 95) // 10 + 50 + 30 + 5
  
  // 验证服务分解
  assert_eq(trace123.service_breakdown.length(), 3)
  
  let gateway_service = trace123.service_breakdown[0]
  assert_eq(gateway_service.service, "gateway")
  assert_eq(gateway_service.total_duration, 10)
  assert_eq(gateway_service.span_count, 1)
  
  let auth_service = trace123.service_breakdown[1]
  assert_eq(auth_service.service, "auth")
  assert_eq(auth_service.total_duration, 50)
  assert_eq(auth_service.span_count, 1)
  
  // 验证第二个追踪
  let trace456 = trace_analysis[1]
  assert_eq(trace456.trace_id, "trace456")
  assert_eq(trace456.span_count, 1)
  assert_eq(trace456.service_count, 1)
  assert_eq(trace456.total_duration, 15)
}

// 测试9: 遥测数据多租户隔离
test "遥测数据多租户隔离" {
  // 模拟多租户遥测数据
  let tenant_data = [
    { tenant_id: "tenant1", service: "app1", metric: "cpu", value: 45.0, timestamp: 1640995200 },
    { tenant_id: "tenant1", service: "app2", metric: "memory", value: 1024.0, timestamp: 1640995260 },
    { tenant_id: "tenant2", service: "app1", metric: "cpu", value: 55.0, timestamp: 1640995320 },
    { tenant_id: "tenant2", service: "app3", metric: "network", value: 200.0, timestamp: 1640995380 },
    { tenant_id: "tenant3", service: "app2", metric: "disk", value: 2048.0, timestamp: 1640995440 }
  ]
  
  // 租户隔离策略
  let isolation_policies = [
    { tenant_id: "tenant1", data_retention_days: 30, sampling_rate: 1.0, allowed_metrics: ["cpu", "memory", "network"] },
    { tenant_id: "tenant2", data_retention_days: 60, sampling_rate: 0.5, allowed_metrics: ["cpu", "disk"] },
    { tenant_id: "tenant3", data_retention_days: 90, sampling_rate: 0.1, allowed_metrics: ["memory", "disk"] }
  ]
  
  // 应用租户隔离策略
  let mut isolated_data = []
  
  for data in tenant_data {
    let mut policy_applied = false
    
    for policy in isolation_policies {
      if data.tenant_id == policy.tenant_id {
        // 检查指标是否允许
        let metric_allowed = policy.allowed_metrics.contains(data.metric)
        
        if metric_allowed {
          isolated_data = isolated_data.push({
            tenant_id: data.tenant_id,
            service: data.service,
            metric: data.metric,
            value: data.value,
            timestamp: data.timestamp,
            retention_days: policy.data_retention_days,
            sampling_rate: policy.sampling_rate
          })
        }
        
        policy_applied = true
        break
      }
    }
    
    // 如果没有找到策略，应用默认策略
    if not policy_applied {
      isolated_data = isolated_data.push({
        tenant_id: data.tenant_id,
        service: data.service,
        metric: data.metric,
        value: data.value,
        timestamp: data.timestamp,
        retention_days: 7,
        sampling_rate: 0.1
      })
    }
  }
  
  // 按租户分组分析
  let mut tenant_analysis = []
  let processed_tenants = []
  
  for data in isolated_data {
    if not processed_tenants.contains(data.tenant_id) {
      processed_tenants = processed_tenants.push(data.tenant_id)
      
      let mut tenant_data_points = []
      for d in isolated_data {
        if d.tenant_id == data.tenant_id {
          tenant_data_points = tenant_data_points.push(d)
        }
      }
      
      tenant_analysis = tenant_analysis.push({
        tenant_id: data.tenant_id,
        data_points: tenant_data_points,
        metrics_count: tenant_data_points.length()
      })
    }
  }
  
  // 验证多租户隔离结果
  assert_eq(isolated_data.length(), 4) // tenant2的network指标被过滤掉
  assert_eq(tenant_analysis.length(), 3)
  
  // 验证tenant1数据
  let tenant1_data = tenant_analysis[0]
  assert_eq(tenant1_data.tenant_id, "tenant1")
  assert_eq(tenant1_data.metrics_count, 2)
  assert_eq(tenant1_data.data_points[0].retention_days, 30)
  assert_eq(tenant1_data.data_points[0].sampling_rate, 1.0)
  
  // 验证tenant2数据
  let tenant2_data = tenant_analysis[1]
  assert_eq(tenant2_data.tenant_id, "tenant2")
  assert_eq(tenant2_data.metrics_count, 1) // network指标被过滤
  assert_eq(tenant2_data.data_points[0].metric, "cpu")
  assert_eq(tenant2_data.data_points[0].retention_days, 60)
  assert_eq(tenant2_data.data_points[0].sampling_rate, 0.5)
  
  // 验证tenant3数据
  let tenant3_data = tenant_analysis[2]
  assert_eq(tenant3_data.tenant_id, "tenant3")
  assert_eq(tenant3_data.metrics_count, 1)
  assert_eq(tenant3_data.data_points[0].metric, "disk")
  assert_eq(tenant3_data.data_points[0].retention_days, 90)
  assert_eq(tenant3_data.data_points[0].sampling_rate, 0.1)
}

// 测试10: 遥测数据智能预测
test "遥测数据智能预测" {
  // 模拟历史遥测数据
  let historical_data = [
    { timestamp: 1640995200, metric: "cpu", value: 40.0 },
    { timestamp: 1640995260, metric: "cpu", value: 45.0 },
    { timestamp: 1640995320, metric: "cpu", value: 50.0 },
    { timestamp: 1640995380, metric: "cpu", value: 48.0 },
    { timestamp: 1640995440, metric: "cpu", value: 52.0 },
    { timestamp: 1640995500, metric: "cpu", value: 55.0 },
    { timestamp: 1640995560, metric: "cpu", value: 53.0 },
    { timestamp: 1640995620, metric: "cpu", value: 58.0 }
  ]
  
  // 简单的线性回归预测算法
  let predict_value = fn(data_points) {
    if data_points.length() < 2 {
      return 0.0
    }
    
    // 计算平均值
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let n = data_points.length().to_float()
    
    let mut i = 0
    while i < data_points.length() {
      sum_x = sum_x + i.to_float()
      sum_y = sum_y + data_points[i].value
      i = i + 1
    }
    
    let mean_x = sum_x / n
    let mean_y = sum_y / n
    
    // 计算斜率和截距
    let mut numerator = 0.0
    let mut denominator = 0.0
    
    i = 0
    while i < data_points.length() {
      let x = i.to_float()
      let y = data_points[i].value
      numerator = numerator + (x - mean_x) * (y - mean_y)
      denominator = denominator + (x - mean_x) * (x - mean_x)
      i = i + 1
    }
    
    let slope = if denominator != 0.0 { numerator / denominator } else { 0.0 }
    let intercept = mean_y - slope * mean_x
    
    // 预测下一个值
    let next_x = n
    slope * next_x + intercept
  }
  
  // 使用不同窗口大小进行预测
  let window_sizes = [3, 5, 8]
  let mut predictions = []
  
  for size in window_sizes {
    if historical_data.length() >= size {
      let window_data = historical_data.slice(historical_data.length() - size, historical_data.length())
      let predicted_value = predict_value(window_data)
      
      predictions = predictions.push({
        window_size: size,
        predicted_value: predicted_value,
        last_actual_value: window_data[window_data.length() - 1].value
      })
    }
  }
  
  // 计算预测准确性指标
  let mut accuracy_metrics = []
  
  for prediction in predictions {
    let error_margin = (prediction.predicted_value - prediction.last_actual_value).abs()
    let relative_error = if prediction.last_actual_value != 0.0 {
      error_margin / prediction.last_actual_value
    } else {
      0.0
    }
    
    accuracy_metrics = accuracy_metrics.push({
      window_size: prediction.window_size,
      predicted_value: prediction.predicted_value,
      actual_value: prediction.last_actual_value,
      absolute_error: error_margin,
      relative_error: relative_error
    })
  }
  
  // 验证预测结果
  assert_eq(predictions.length(), 3)
  assert_eq(accuracy_metrics.length(), 3)
  
  // 验证预测值在合理范围内
  for prediction in predictions {
    assert_true(prediction.predicted_value >= 0.0)
    assert_true(prediction.predicted_value <= 100.0)
  }
  
  // 验证准确性指标
  for metric in accuracy_metrics {
    assert_true(metric.absolute_error >= 0.0)
    assert_true(metric.relative_error >= 0.0)
  }
  
  // 找出最佳预测窗口（相对误差最小）
  let mut best_prediction = accuracy_metrics[0]
  let mut i = 1
  while i < accuracy_metrics.length() {
    if accuracy_metrics[i].relative_error < best_prediction.relative_error {
      best_prediction = accuracy_metrics[i]
    }
    i = i + 1
  }
  
  // 验证最佳预测
  assert_true(best_prediction.window_size >= 3)
  assert_true(best_prediction.window_size <= 8)
}