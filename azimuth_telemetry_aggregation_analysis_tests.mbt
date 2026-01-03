// Azimuth 遥测聚合分析测试用例
// 专注于遥测数据的聚合、分析和高级处理功能

// 测试1: 时间序列数据聚合
test "时间序列数据聚合分析" {
  // 创建时间序列数据点
  let data_points = [
    {"timestamp": 1640995200000, "value": 10.5, "metric": "cpu.usage"},
    {"timestamp": 1640995200600, "value": 12.3, "metric": "cpu.usage"},
    {"timestamp": 1640995201200, "value": 15.7, "metric": "cpu.usage"},
    {"timestamp": 1640995201800, "value": 14.2, "metric": "cpu.usage"},
    {"timestamp": 1640995202400, "value": 11.8, "metric": "cpu.usage"}
  ]
  
  // 计算聚合统计
  let sum = data_points.reduce(fn(acc, point) { acc + point["value"] }, 0.0)
  let count = data_points.length().to_float()
  let average = sum / count
  
  // 找到最大值和最小值
  let max_value = data_points.reduce(fn(acc, point) { 
    if point["value"] > acc { point["value"] } else { acc } 
  }, data_points[0]["value"])
  
  let min_value = data_points.reduce(fn(acc, point) { 
    if point["value"] < acc { point["value"] } else { acc } 
  }, data_points[0]["value"])
  
  // 计算方差
  let variance = data_points.reduce(fn(acc, point) { 
    let diff = point["value"] - average
    acc + (diff * diff)
  }, 0.0) / count
  
  // 验证聚合结果
  assert_eq(count, 5.0)
  assert_eq(sum, 64.5)
  assert_eq(average, 12.9)
  assert_eq(max_value, 15.7)
  assert_eq(min_value, 10.5)
  assert_true(variance > 0.0)
  
  // 按时间窗口聚合（1分钟窗口）
  let time_windows = {}
  for point in data_points {
    let window_start = (point["timestamp"] / 60000) * 60000
    let window_key = window_start.to_string()
    
    let current_window = match time_windows.get(window_key) {
      Some(window) => window
      None => {"sum": 0.0, "count": 0}
    }
    
    let updated_window = {
      "sum": current_window["sum"] + point["value"],
      "count": current_window["count"] + 1
    }
    
    time_windows = time_windows.set(window_key, updated_window)
  }
  
  // 验证时间窗口聚合
  assert_eq(time_windows.keys().length(), 1) // 所有数据点在同一分钟窗口
  let window = time_windows.get("1640995200000").unwrap()
  assert_eq(window["sum"], 64.5)
  assert_eq(window["count"], 5)
}

// 测试2: 异常检测和告警机制
test "异常检测和告警机制" {
  // 创建阈值配置
  let alert_thresholds = {
    "cpu.usage": {"warning": 70.0, "critical": 90.0},
    "memory.usage": {"warning": 80.0, "critical": 95.0},
    "response.time": {"warning": 1000.0, "critical": 5000.0},
    "error.rate": {"warning": 5.0, "critical": 10.0}
  }
  
  // 创建监控数据点
  let metrics = [
    {"metric": "cpu.usage", "value": 45.2, "timestamp": 1640995200000},
    {"metric": "memory.usage", "value": 82.5, "timestamp": 1640995200000},
    {"metric": "response.time", "value": 1200.0, "timestamp": 1640995200000},
    {"metric": "error.rate", "value": 12.3, "timestamp": 1640995200000}
  ]
  
  // 检测异常并生成告警
  let alerts = []
  for metric in metrics {
    let metric_name = metric["metric"]
    let value = metric["value"]
    
    match alert_thresholds.get(metric_name) {
      Some(thresholds) => {
        if value >= thresholds["critical"] {
          let alert = {
            "level": "critical",
            "metric": metric_name,
            "value": value,
            "threshold": thresholds["critical"],
            "timestamp": metric["timestamp"]
          }
          alerts = alerts.push(alert)
        } else if value >= thresholds["warning"] {
          let alert = {
            "level": "warning",
            "metric": metric_name,
            "value": value,
            "threshold": thresholds["warning"],
            "timestamp": metric["timestamp"]
          }
          alerts = alerts.push(alert)
        }
      }
      None => {}
    }
  }
  
  // 验证告警生成
  assert_eq(alerts.length(), 3) // memory.usage(warning), response.time(warning), error.rate(critical)
  
  // 验证具体告警
  let critical_alerts = alerts.filter(fn(alert) { alert["level"] == "critical" })
  let warning_alerts = alerts.filter(fn(alert) { alert["level"] == "warning" })
  
  assert_eq(critical_alerts.length(), 1)
  assert_eq(critical_alerts[0]["metric"], "error.rate")
  assert_eq(critical_alerts[0]["value"], 12.3)
  assert_eq(critical_alerts[0]["threshold"], 10.0)
  
  assert_eq(warning_alerts.length(), 2)
  let memory_alert = warning_alerts.filter(fn(alert) { alert["metric"] == "memory.usage" })[0]
  assert_eq(memory_alert["value"], 82.5)
  assert_eq(memory_alert["threshold"], 80.0)
  
  let response_alert = warning_alerts.filter(fn(alert) { alert["metric"] == "response.time" })[0]
  assert_eq(response_alert["value"], 1200.0)
  assert_eq(response_alert["threshold"], 1000.0)
}

// 测试3: 自适应采样策略
test "自适应采样策略" {
  // 定义采样策略配置
  let sampling_strategies = {
    "error_based": {
      "error_threshold": 5.0,
      "sample_rate_multiplier": 2.0
    },
    "latency_based": {
      "latency_threshold": 1000.0,
      "sample_rate_multiplier": 1.5
    },
    "volume_based": {
      "high_volume_threshold": 1000,
      "low_volume_threshold": 100,
      "high_sample_rate": 0.1,
      "low_sample_rate": 0.5
    }
  }
  
  // 创建服务指标数据
  let service_metrics = [
    {
      "service": "auth.service",
      "request_count": 150,
      "error_rate": 2.1,
      "avg_latency": 450.0,
      "base_sample_rate": 0.2
    },
    {
      "service": "payment.service",
      "request_count": 2500,
      "error_rate": 8.7,
      "avg_latency": 2100.0,
      "base_sample_rate": 0.1
    },
    {
      "service": "notification.service",
      "request_count": 50,
      "error_rate": 0.5,
      "avg_latency": 200.0,
      "base_sample_rate": 0.3
    }
  ]
  
  // 计算自适应采样率
  let adaptive_sampling_rates = []
  for service in service_metrics {
    let mut sample_rate = service["base_sample_rate"]
    
    // 基于错误的采样调整
    if service["error_rate"] >= sampling_strategies["error_based"]["error_threshold"] {
      sample_rate = sample_rate * sampling_strategies["error_based"]["sample_rate_multiplier"]
    }
    
    // 基于延迟的采样调整
    if service["avg_latency"] >= sampling_strategies["latency_based"]["latency_threshold"] {
      sample_rate = sample_rate * sampling_strategies["latency_based"]["sample_rate_multiplier"]
    }
    
    // 基于请求量的采样调整
    if service["request_count"] >= sampling_strategies["volume_based"]["high_volume_threshold"] {
      sample_rate = sampling_strategies["volume_based"]["high_sample_rate"]
    } else if service["request_count"] <= sampling_strategies["volume_based"]["low_volume_threshold"] {
      sample_rate = sampling_strategies["volume_based"]["low_sample_rate"]
    }
    
    // 确保采样率在合理范围内
    sample_rate = if sample_rate > 1.0 { 1.0 } else { sample_rate }
    sample_rate = if sample_rate < 0.01 { 0.01 } else { sample_rate }
    
    adaptive_sampling_rates.push({
      "service": service["service"],
      "original_rate": service["base_sample_rate"],
      "adaptive_rate": sample_rate,
      "request_count": service["request_count"],
      "error_rate": service["error_rate"],
      "avg_latency": service["avg_latency"]
    })
  }
  
  // 验证自适应采样率
  assert_eq(adaptive_sampling_rates.length(), 3)
  
  // auth.service: 低错误率、低延迟、中等请求量，采样率应该保持或略微调整
  let auth_sampling = adaptive_sampling_rates.filter(fn(s) { s["service"] == "auth.service" })[0]
  assert_eq(auth_sampling["original_rate"], 0.2)
  assert_eq(auth_sampling["adaptive_rate"], 0.2) // 没有触发调整条件
  
  // payment.service: 高错误率、高延迟、高请求量，采样率应该显著调整
  let payment_sampling = adaptive_sampling_rates.filter(fn(s) { s["service"] == "payment.service" })[0]
  assert_eq(payment_sampling["original_rate"], 0.1)
  assert_eq(payment_sampling["adaptive_rate"], 0.3) // 触发错误和延迟调整，但受高请求量限制
  
  // notification.service: 低错误率、低延迟、低请求量，采样率应该提高
  let notification_sampling = adaptive_sampling_rates.filter(fn(s) { s["service"] == "notification.service" })[0]
  assert_eq(notification_sampling["original_rate"], 0.3)
  assert_eq(notification_sampling["adaptive_rate"], 0.5) // 触发低请求量调整
}

// 测试4: 跨服务上下文传播
test "跨服务上下文传播" {
  // 创建服务调用链
  let service_chain = [
    {
      "service": "api.gateway",
      "operation": "POST /api/orders",
      "trace_id": "trace-12345-67890-abcdef",
      "span_id": "span-gateway-00000-00000",
      "parent_span_id": None,
      "baggage": {
        "user.id": "user-12345",
        "request.id": "req-abcde",
        "auth.token": "token-xyz"
      }
    },
    {
      "service": "order.service",
      "operation": "createOrder",
      "trace_id": "trace-12345-67890-abcdef",
      "span_id": "span-order-11111-11111",
      "parent_span_id": Some("span-gateway-00000-00000"),
      "baggage": {
        "user.id": "user-12345",
        "request.id": "req-abcde",
        "auth.token": "token-xyz",
        "order.source": "web"
      }
    },
    {
      "service": "payment.service",
      "operation": "processPayment",
      "trace_id": "trace-12345-67890-abcdef",
      "span_id": "span-payment-22222-22222",
      "parent_span_id": Some("span-order-11111-11111"),
      "baggage": {
        "user.id": "user-12345",
        "request.id": "req-abcde",
        "auth.token": "token-xyz",
        "order.source": "web",
        "payment.method": "credit_card"
      }
    },
    {
      "service": "notification.service",
      "operation": "sendConfirmation",
      "trace_id": "trace-12345-67890-abcdef",
      "span_id": "span-notification-33333-33333",
      "parent_span_id": Some("span-order-11111-11111"),
      "baggage": {
        "user.id": "user-12345",
        "request.id": "req-abcde",
        "auth.token": "token-xyz",
        "notification.type": "email"
      }
    }
  ]
  
  // 验证追踪上下文一致性
  let trace_ids = service_chain.map(fn(service) { service["trace_id"] })
  for trace_id in trace_ids {
    assert_eq(trace_id, "trace-12345-67890-abcdef")
  }
  
  // 验证父子关系
  assert_eq(service_chain[1]["parent_span_id"].unwrap(), service_chain[0]["span_id"])
  assert_eq(service_chain[2]["parent_span_id"].unwrap(), service_chain[1]["span_id"])
  assert_eq(service_chain[3]["parent_span_id"].unwrap(), service_chain[1]["span_id"])
  
  // 验证baggage传播
  let root_baggage = service_chain[0]["baggage"]
  let order_baggage = service_chain[1]["baggage"]
  let payment_baggage = service_chain[2]["baggage"]
  let notification_baggage = service_chain[3]["baggage"]
  
  // 验证原始baggage项在所有服务中保持不变
  assert_eq(root_baggage["user.id"], "user-12345")
  assert_eq(order_baggage["user.id"], "user-12345")
  assert_eq(payment_baggage["user.id"], "user-12345")
  assert_eq(notification_baggage["user.id"], "user-12345")
  
  // 验证服务特定的baggage项
  assert_eq(order_baggage["order.source"], "web")
  assert_eq(payment_baggage["payment.method"], "credit_card")
  assert_eq(notification_baggage["notification.type"], "email")
  
  // 构建调用链拓扑
  let service_topology = {}
  for service in service_chain {
    let service_name = service["service"]
    let parent_span_id = service["parent_span_id"]
    
    match parent_span_id {
      Some(parent_id) => {
        // 找到父服务
        let parent_service = service_chain.filter(fn(s) { s["span_id"] == parent_id })[0]
        let parent_name = parent_service["service"]
        
        // 更新拓扑
        let current_children = match service_topology.get(parent_name) {
          Some(children) => children
          None => []
        }
        
        service_topology = service_topology.set(parent_name, current_children.push(service_name))
      }
      None => {
        // 根服务
        if service_topology.contains(service_name) == false {
          service_topology = service_topology.set(service_name, [])
        }
      }
    }
  }
  
  // 验证调用链拓扑
  assert_eq(service_topology["api.gateway"], ["order.service"])
  assert_eq(service_topology["order.service"], ["payment.service", "notification.service"])
  assert_eq(service_topology["payment.service"], [])
  assert_eq(service_topology["notification.service"], [])
}

// 测试5: 性能基准和资源监控
test "性能基准和资源监控" {
  // 创建性能基准数据
  let performance_benchmarks = {
    "api.gateway": {
      "max_response_time": 500.0,
      "max_memory_usage": 512.0,
      "max_cpu_usage": 70.0,
      "min_throughput": 1000.0
    },
    "order.service": {
      "max_response_time": 1000.0,
      "max_memory_usage": 1024.0,
      "max_cpu_usage": 80.0,
      "min_throughput": 500.0
    },
    "payment.service": {
      "max_response_time": 2000.0,
      "max_memory_usage": 2048.0,
      "max_cpu_usage": 90.0,
      "min_throughput": 200.0
    }
  }
  
  // 创建实际性能数据
  let actual_performance = [
    {
      "service": "api.gateway",
      "response_time": 420.0,
      "memory_usage": 480.0,
      "cpu_usage": 65.0,
      "throughput": 1200.0,
      "timestamp": 1640995200000
    },
    {
      "service": "order.service",
      "response_time": 1100.0,
      "memory_usage": 980.0,
      "cpu_usage": 75.0,
      "throughput": 450.0,
      "timestamp": 1640995200000
    },
    {
      "service": "payment.service",
      "response_time": 1800.0,
      "memory_usage": 2100.0,
      "cpu_usage": 85.0,
      "throughput": 220.0,
      "timestamp": 1640995200000
    }
  ]
  
  // 评估性能合规性
  let performance_violations = []
  for perf in actual_performance {
    let service_name = perf["service"]
    let benchmark = performance_benchmarks[service_name]
    
    // 检查响应时间
    if perf["response_time"] > benchmark["max_response_time"] {
      performance_violations.push({
        "service": service_name,
        "metric": "response_time",
        "actual": perf["response_time"],
        "threshold": benchmark["max_response_time"],
        "violation": "exceeds_threshold"
      })
    }
    
    // 检查内存使用
    if perf["memory_usage"] > benchmark["max_memory_usage"] {
      performance_violations.push({
        "service": service_name,
        "metric": "memory_usage",
        "actual": perf["memory_usage"],
        "threshold": benchmark["max_memory_usage"],
        "violation": "exceeds_threshold"
      })
    }
    
    // 检查CPU使用
    if perf["cpu_usage"] > benchmark["max_cpu_usage"] {
      performance_violations.push({
        "service": service_name,
        "metric": "cpu_usage",
        "actual": perf["cpu_usage"],
        "threshold": benchmark["max_cpu_usage"],
        "violation": "exceeds_threshold"
      })
    }
    
    // 检查吞吐量
    if perf["throughput"] < benchmark["min_throughput"] {
      performance_violations.push({
        "service": service_name,
        "metric": "throughput",
        "actual": perf["throughput"],
        "threshold": benchmark["min_throughput"],
        "violation": "below_threshold"
      })
    }
  }
  
  // 验证性能违规
  assert_eq(performance_violations.length(), 3)
  
  // order.service: 响应时间超标，吞吐量不足
  let order_violations = performance_violations.filter(fn(v) { v["service"] == "order.service" })
  assert_eq(order_violations.length(), 2)
  
  let response_time_violation = order_violations.filter(fn(v) { v["metric"] == "response_time" })[0]
  assert_eq(response_time_violation["actual"], 1100.0)
  assert_eq(response_time_violation["threshold"], 1000.0)
  assert_eq(response_time_violation["violation"], "exceeds_threshold")
  
  let throughput_violation = order_violations.filter(fn(v) { v["metric"] == "throughput" })[0]
  assert_eq(throughput_violation["actual"], 450.0)
  assert_eq(throughput_violation["threshold"], 500.0)
  assert_eq(throughput_violation["violation"], "below_threshold")
  
  // payment.service: 内存使用超标
  let payment_violations = performance_violations.filter(fn(v) { v["service"] == "payment.service" })
  assert_eq(payment_violations.length(), 1)
  
  let memory_violation = payment_violations[0]
  assert_eq(memory_violation["metric"], "memory_usage")
  assert_eq(memory_violation["actual"], 2100.0)
  assert_eq(memory_violation["threshold"], 2048.0)
  
  // api.gateway: 无违规
  let gateway_violations = performance_violations.filter(fn(v) { v["service"] == "api.gateway" })
  assert_eq(gateway_violations.length(), 0)
}

// 测试6: 数据压缩和传输优化
test "数据压缩和传输优化" {
  // 创建原始遥测数据
  let raw_telemetry_data = [
    {
      "trace_id": "trace-compression-12345",
      "span_id": "span-001",
      "service": "api.gateway",
      "operation": "GET /api/users",
      "start_time": 1640995200000,
      "end_time": 1640995200200,
      "attributes": {
        "http.method": "GET",
        "http.url": "/api/users",
        "http.status_code": 200,
        "user.id": "user-12345",
        "client.ip": "192.168.1.100"
      }
    },
    {
      "trace_id": "trace-compression-12345",
      "span_id": "span-002",
      "service": "user.service",
      "operation": "getUserProfile",
      "start_time": 1640995200100,
      "end_time": 1640995200180,
      "attributes": {
        "db.system": "postgresql",
        "db.statement": "SELECT * FROM users WHERE id = ?",
        "db.operation": "SELECT",
        "user.id": "user-12345"
      }
    }
  ]
  
  // 计算原始数据大小（简化计算）
  let original_size = raw_telemetry_data.to_string().length()
  assert_true(original_size > 0)
  
  // 应用压缩策略
  let compression_strategies = {
    "attribute_deduplication": true,
    "common_string_replacement": true,
    "numeric_compression": true,
    "timestamp_delta_encoding": true
  }
  
  // 实现属性去重
  let common_attributes = {}
  let deduplicated_data = []
  
  for span in raw_telemetry_data {
    let mut new_span = span
    let mut new_attributes = {}
    
    // 识别公共属性
    for (key, value) in span["attributes"] {
      if common_attributes.contains(key) && common_attributes[key] == value {
        // 属性已在公共集合中，跳过
      } else {
        // 新属性或值不同
        if common_attributes.contains(key) == false {
          common_attributes = common_attributes.set(key, value)
        }
        new_attributes = new_attributes.set(key, value)
      }
    }
    
    new_span = new_span.set("attributes", new_attributes)
    deduplicated_data = deduplicated_data.push(new_span)
  }
  
  // 验证属性去重
  assert_eq(deduplicated_data.length(), 2)
  assert_eq(common_attributes.keys().length(), 8) // 所有属性都被识别为公共属性
  
  // 实现时间戳增量编码
  let base_timestamp = raw_telemetry_data[0]["start_time"]
  let delta_encoded_data = []
  
  for span in deduplicated_data {
    let start_time_delta = span["start_time"] - base_timestamp
    let end_time_delta = span["end_time"] - base_timestamp
    
    let encoded_span = {
      "trace_id": span["trace_id"],
      "span_id": span["span_id"],
      "service": span["service"],
      "operation": span["operation"],
      "start_time_delta": start_time_delta,
      "end_time_delta": end_time_delta,
      "attributes": span["attributes"]
    }
    
    delta_encoded_data = delta_encoded_data.push(encoded_span)
  }
  
  // 验证增量编码
  assert_eq(delta_encoded_data.length(), 2)
  assert_eq(delta_encoded_data[0]["start_time_delta"], 0)
  assert_eq(delta_encoded_data[0]["end_time_delta"], 200)
  assert_eq(delta_encoded_data[1]["start_time_delta"], 100)
  assert_eq(delta_encoded_data[1]["end_time_delta"], 180)
  
  // 计算压缩后大小（简化计算）
  let compressed_size = (delta_encoded_data.to_string().length() + common_attributes.to_string().length())
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // 验证压缩效果
  assert_true(compression_ratio < 1.0) // 压缩后应该更小
  assert_true(compression_ratio > 0.1) // 但不应该过度压缩
  
  // 模拟批量传输优化
  let batch_config = {
    "max_batch_size": 100,
    "max_batch_delay_ms": 5000,
    "compression_threshold": 1024
  }
  
  // 创建批处理逻辑
  let create_batch = fn(data_points) {
    if data_points.length() == 0 {
      None
    } else if data_points.length() >= batch_config["max_batch_size"] {
      Some(data_points.take(batch_config["max_batch_size"]))
    } else {
      // 简化批处理逻辑
      Some(data_points)
    }
  }
  
  // 测试批处理
  let batch = create_batch(delta_encoded_data)
  match batch {
    Some(batch_data) => {
      assert_eq(batch_data.length(), 2)
      assert_eq(batch_data[0]["span_id"], "span-001")
      assert_eq(batch_data[1]["span_id"], "span-002")
    }
    None => assert_true(false)
  }
}

// 测试7: 配置动态更新和热重载
test "配置动态更新和热重载" {
  // 创建初始配置
  let initial_config = {
    "sampling": {
      "default_rate": 0.1,
      "max_rate": 1.0,
      "min_rate": 0.01
    },
    "exporter": {
      "endpoint": "https://otel-collector.example.com:4317",
      "timeout_ms": 30000,
      "batch_size": 512,
      "max_export_batch_size": 512
    },
    "processor": {
      "attribute_limits": {
        "attribute_count_limit": 128,
        "attribute_value_length_limit": 1024
      },
      "span_limits": {
        "attribute_count_limit": 128,
        "event_count_limit": 128,
        "link_count_limit": 128
      }
    }
  }
  
  // 创建配置版本管理
  let config_versions = []
  config_versions = config_versions.push({
    "version": "1.0.0",
    "timestamp": 1640995200000,
    "config": initial_config
  })
  
  // 模拟配置更新请求
  let update_requests = [
    {
      "timestamp": 1640995201000,
      "changes": {
        "sampling.default_rate": 0.2,
        "exporter.endpoint": "https://otel-collector-v2.example.com:4317"
      }
    },
    {
      "timestamp": 1640995202000,
      "changes": {
        "processor.attribute_limits.attribute_count_limit": 256,
        "processor.span_limits.event_count_limit": 256
      }
    }
  ]
  
  // 应用配置更新
  let apply_config_update = fn(config, changes) {
    let mut updated_config = config
    
    for (path, new_value) in changes {
      let path_parts = path.split(".")
      let mut current = updated_config
      
      // 简化的路径更新逻辑
      if path_parts[0] == "sampling" && path_parts[1] == "default_rate" {
        let sampling = updated_config["sampling"]
        let updated_sampling = sampling.set("default_rate", new_value)
        updated_config = updated_config.set("sampling", updated_sampling)
      } else if path_parts[0] == "exporter" && path_parts[1] == "endpoint" {
        let exporter = updated_config["exporter"]
        let updated_exporter = exporter.set("endpoint", new_value)
        updated_config = updated_config.set("exporter", updated_exporter)
      } else if path_parts[0] == "processor" && path_parts[1] == "attribute_limits" && path_parts[2] == "attribute_count_limit" {
        let processor = updated_config["processor"]
        let attribute_limits = processor["attribute_limits"]
        let updated_attribute_limits = attribute_limits.set("attribute_count_limit", new_value)
        let updated_processor = processor.set("attribute_limits", updated_attribute_limits)
        updated_config = updated_config.set("processor", updated_processor)
      } else if path_parts[0] == "processor" && path_parts[1] == "span_limits" && path_parts[2] == "event_count_limit" {
        let processor = updated_config["processor"]
        let span_limits = processor["span_limits"]
        let updated_span_limits = span_limits.set("event_count_limit", new_value)
        let updated_processor = processor.set("span_limits", updated_span_limits)
        updated_config = updated_config.set("processor", updated_processor)
      }
    }
    
    updated_config
  }
  
  // 按顺序应用更新
  let mut current_config = initial_config
  for request in update_requests {
    current_config = apply_config_update(current_config, request["changes"])
    
    // 记录新版本
    config_versions = config_versions.push({
      "version": (config_versions.length() + 1).to_string() + ".0.0",
      "timestamp": request["timestamp"],
      "config": current_config
    })
  }
  
  // 验证配置版本
  assert_eq(config_versions.length(), 3)
  assert_eq(config_versions[0]["version"], "1.0.0")
  assert_eq(config_versions[1]["version"], "2.0.0")
  assert_eq(config_versions[2]["version"], "3.0.0")
  
  // 验证最终配置
  let final_config = config_versions[2]["config"]
  assert_eq(final_config["sampling"]["default_rate"], 0.2) // 已更新
  assert_eq(final_config["exporter"]["endpoint"], "https://otel-collector-v2.example.com:4317") // 已更新
  assert_eq(final_config["processor"]["attribute_limits"]["attribute_count_limit"], 256) // 已更新
  assert_eq(final_config["processor"]["span_limits"]["event_count_limit"], 256) // 已更新
  
  // 验证未更改的配置
  assert_eq(final_config["sampling"]["max_rate"], 1.0) // 未更改
  assert_eq(final_config["exporter"]["timeout_ms"], 30000) // 未更改
  assert_eq(final_config["processor"]["attribute_limits"]["attribute_value_length_limit"], 1024) // 未更改
  
  // 模拟配置回滚
  let rollback_to_version = fn(versions, target_version) {
    let target_version_data = versions.filter(fn(v) { v["version"] == target_version })[0]
    target_version_data["config"]
  }
  
  let rollback_config = rollback_to_version(config_versions, "1.0.0")
  
  // 验证回滚配置
  assert_eq(rollback_config["sampling"]["default_rate"], 0.1) // 回滚到原始值
  assert_eq(rollback_config["exporter"]["endpoint"], "https://otel-collector.example.com:4317") // 回滚到原始值
  assert_eq(rollback_config["processor"]["attribute_limits"]["attribute_count_limit"], 128) // 回滚到原始值
}

// 测试8: 多租户隔离和权限控制
test "多租户隔离和权限控制" {
  // 创建租户配置
  let tenant_configs = [
    {
      "tenant_id": "tenant-enterprise",
      "tenant_name": "Enterprise Corp",
      "plan": "enterprise",
      "quota": {
        "max_metrics_per_day": 10000000,
        "max_traces_per_day": 5000000,
        "max_spans_per_trace": 1000,
        "max_attribute_value_length": 4096
      },
      "permissions": {
        "can_export": true,
        "can_access_raw_data": true,
        "can_configure_sampling": true,
        "can_view_all_services": true
      },
      "services": ["payment.service", "order.service", "user.service", "inventory.service"]
    },
    {
      "tenant_id": "tenant-standard",
      "tenant_name": "Standard LLC",
      "plan": "standard",
      "quota": {
        "max_metrics_per_day": 1000000,
        "max_traces_per_day": 500000,
        "max_spans_per_trace": 500,
        "max_attribute_value_length": 1024
      },
      "permissions": {
        "can_export": true,
        "can_access_raw_data": false,
        "can_configure_sampling": false,
        "can_view_all_services": false
      },
      "services": ["web.service", "api.service"]
    },
    {
      "tenant_id": "tenant-basic",
      "tenant_name": "Basic Startup",
      "plan": "basic",
      "quota": {
        "max_metrics_per_day": 100000,
        "max_traces_per_day": 50000,
        "max_spans_per_trace": 100,
        "max_attribute_value_length": 512
      },
      "permissions": {
        "can_export": false,
        "can_access_raw_data": false,
        "can_configure_sampling": false,
        "can_view_all_services": false
      },
      "services": ["web.service"]
    }
  ]
  
  // 创建租户使用数据
  let tenant_usage = [
    {
      "tenant_id": "tenant-enterprise",
      "date": "2023-01-01",
      "metrics_count": 8500000,
      "traces_count": 4200000,
      "avg_spans_per_trace": 750,
      "max_attribute_value_length": 2048
    },
    {
      "tenant_id": "tenant-standard",
      "date": "2023-01-01",
      "metrics_count": 950000,
      "traces_count": 480000,
      "avg_spans_per_trace": 450,
      "max_attribute_value_length": 800
    },
    {
      "tenant_id": "tenant-basic",
      "date": "2023-01-01",
      "metrics_count": 120000,
      "traces_count": 55000,
      "avg_spans_per_trace": 120,
      "max_attribute_value_length": 400
    }
  ]
  
  // 检查配额违规
  let quota_violations = []
  for usage in tenant_usage {
    let tenant_id = usage["tenant_id"]
    let tenant_config = tenant_configs.filter(fn(t) { t["tenant_id"] == tenant_id })[0]
    let quota = tenant_config["quota"]
    
    // 检查指标配额
    if usage["metrics_count"] > quota["max_metrics_per_day"] {
      quota_violations.push({
        "tenant_id": tenant_id,
        "quota_type": "metrics",
        "used": usage["metrics_count"],
        "limit": quota["max_metrics_per_day"],
        "overage": usage["metrics_count"] - quota["max_metrics_per_day"]
      })
    }
    
    // 检查追踪配额
    if usage["traces_count"] > quota["max_traces_per_day"] {
      quota_violations.push({
        "tenant_id": tenant_id,
        "quota_type": "traces",
        "used": usage["traces_count"],
        "limit": quota["max_traces_per_day"],
        "overage": usage["traces_count"] - quota["max_traces_per_day"]
      })
    }
    
    // 检查每个追踪的Span配额
    if usage["avg_spans_per_trace"] > quota["max_spans_per_trace"] {
      quota_violations.push({
        "tenant_id": tenant_id,
        "quota_type": "spans_per_trace",
        "used": usage["avg_spans_per_trace"],
        "limit": quota["max_spans_per_trace"],
        "overage": usage["avg_spans_per_trace"] - quota["max_spans_per_trace"]
      })
    }
    
    // 检查属性值长度配额
    if usage["max_attribute_value_length"] > quota["max_attribute_value_length"] {
      quota_violations.push({
        "tenant_id": tenant_id,
        "quota_type": "attribute_value_length",
        "used": usage["max_attribute_value_length"],
        "limit": quota["max_attribute_value_length"],
        "overage": usage["max_attribute_value_length"] - quota["max_attribute_value_length"]
      })
    }
  }
  
  // 验证配额违规
  assert_eq(quota_violations.length(), 2)
  
  // tenant-basic: 指标和每个追踪的Span数超标
  let basic_violations = quota_violations.filter(fn(v) { v["tenant_id"] == "tenant-basic" })
  assert_eq(basic_violations.length(), 2)
  
  let metrics_violation = basic_violations.filter(fn(v) { v["quota_type"] == "metrics" })[0]
  assert_eq(metrics_violation["used"], 120000)
  assert_eq(metrics_violation["limit"], 100000)
  assert_eq(metrics_violation["overage"], 20000)
  
  let spans_violation = basic_violations.filter(fn(v) { v["quota_type"] == "spans_per_trace" })[0]
  assert_eq(spans_violation["used"], 120)
  assert_eq(spans_violation["limit"], 100)
  assert_eq(spans_violation["overage"], 20)
  
  // tenant-enterprise和tenant-standard无违规
  let enterprise_violations = quota_violations.filter(fn(v) { v["tenant_id"] == "tenant-enterprise" })
  let standard_violations = quota_violations.filter(fn(v) { v["tenant_id"] == "tenant-standard" })
  assert_eq(enterprise_violations.length(), 0)
  assert_eq(standard_violations.length(), 0)
  
  // 验证权限控制
  let check_permission = fn(tenant_id, permission) {
    let tenant_config = tenant_configs.filter(fn(t) { t["tenant_id"] == tenant_id })[0]
    tenant_config["permissions"][permission]
  }
  
  // 验证企业租户权限
  assert_true(check_permission("tenant-enterprise", "can_export"))
  assert_true(check_permission("tenant-enterprise", "can_access_raw_data"))
  assert_true(check_permission("tenant-enterprise", "can_configure_sampling"))
  assert_true(check_permission("tenant-enterprise", "can_view_all_services"))
  
  // 验证标准租户权限
  assert_true(check_permission("tenant-standard", "can_export"))
  assert_false(check_permission("tenant-standard", "can_access_raw_data"))
  assert_false(check_permission("tenant-standard", "can_configure_sampling"))
  assert_false(check_permission("tenant-standard", "can_view_all_services"))
  
  // 验证基础租户权限
  assert_false(check_permission("tenant-basic", "can_export"))
  assert_false(check_permission("tenant-basic", "can_access_raw_data"))
  assert_false(check_permission("tenant-basic", "can_configure_sampling"))
  assert_false(check_permission("tenant-basic", "can_view_all_services"))
  
  // 验证服务隔离
  let get_tenant_services = fn(tenant_id) {
    let tenant_config = tenant_configs.filter(fn(t) { t["tenant_id"] == tenant_id })[0]
    tenant_config["services"]
  }
  
  let enterprise_services = get_tenant_services("tenant-enterprise")
  let standard_services = get_tenant_services("tenant-standard")
  let basic_services = get_tenant_services("tenant-basic")
  
  assert_eq(enterprise_services.length(), 4)
  assert_true(enterprise_services.contains("payment.service"))
  assert_true(enterprise_services.contains("order.service"))
  
  assert_eq(standard_services.length(), 2)
  assert_true(standard_services.contains("web.service"))
  assert_true(standard_services.contains("api.service"))
  assert_false(standard_services.contains("payment.service")) // 企业租户专属服务
  
  assert_eq(basic_services.length(), 1)
  assert_eq(basic_services[0], "web.service")
}