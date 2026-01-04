// Azimuth 增强遥测功能测试用例
// 专注于高级遥测功能和新兴技术场景

// 测试1: 遥测数据的安全性和加密
test "遥测数据的安全性和加密处理" {
  // 模拟敏感遥测数据
  let sensitive_data = [
    { metric: "user.auth", user_id: "user123", token: "secret_token_123", timestamp: 1640995200 },
    { metric: "payment.process", amount: 99.99, card_last4: "4567", timestamp: 1640995201 },
    { metric: "database.query", query: "SELECT * FROM users WHERE id = 123", duration: 45, timestamp: 1640995202 }
  ]
  
  // 定义敏感字段
  let sensitive_fields = ["user_id", "token", "card_last4", "query"]
  
  // 数据脱敏处理
  let sanitize_data = fn(data, fields) {
    let mut sanitized = {}
    for (key, value) in data {
      let is_sensitive = false
      for field in fields {
        if key == field {
          is_sensitive = true
          break
        }
      }
      
      let final_value = if is_sensitive {
        // 简单脱敏：保留首尾字符
        if value.length() <= 2 {
          "*".repeat(value.length())
        } else {
          value[0] + "*".repeat(value.length() - 2) + value[value.length() - 1]
        }
      } else {
        value
      }
      
      sanitized = { sanitized | (key, final_value) }
    }
    sanitized
  }
  
  // 应用脱敏处理
  let mut sanitized_data = []
  for data_point in sensitive_data {
    sanitized_data = sanitized_data.push(sanitize_data(data_point, sensitive_fields))
  }
  
  // 验证脱敏结果
  assert_eq(sanitized_data[0].user_id, "u**3")
  assert_eq(sanitized_data[0].token, "s**3")
  assert_eq(sanitized_data[1].card_last4, "4**7")
  assert_eq(sanitized_data[2].query, "S**y")
  
  // 验证非敏感字段未被修改
  assert_eq(sanitized_data[0].metric, "user.auth")
  assert_eq(sanitized_data[1].amount, 99.99)
  assert_eq(sanitized_data[2].duration, 45)
}

// 测试2: 遥测数据的异常检测
test "遥测数据的异常检测算法" {
  // 模拟正常和异常的遥测数据
  let telemetry_metrics = [
    { name: "cpu_usage", value: 45.0, timestamp: 1640995200 },
    { name: "cpu_usage", value: 48.0, timestamp: 1640995260 },
    { name: "cpu_usage", value: 52.0, timestamp: 1640995320 },
    { name: "cpu_usage", value: 95.0, timestamp: 1640995380 }, // 异常值
    { name: "cpu_usage", value: 47.0, timestamp: 1640995440 },
    { name: "cpu_usage", value: 50.0, timestamp: 1640995500 },
    { name: "cpu_usage", value: 98.0, timestamp: 1640995560 }  // 异常值
  ]
  
  // 计算统计阈值（使用3σ规则）
  let calculate_thresholds = fn(values) {
    // 计算平均值
    let mut sum = 0.0
    for v in values {
      sum = sum + v
    }
    let mean = sum / values.length().to_float()
    
    // 计算标准差
    let mut variance_sum = 0.0
    for v in values {
      let diff = v - mean
      variance_sum = variance_sum + diff * diff
    }
    let variance = variance_sum / values.length().to_float()
    let std_dev = variance.sqrt()
    
    // 返回3σ阈值
    { lower: mean - 3.0 * std_dev, upper: mean + 3.0 * std_dev, mean }
  }
  
  // 提取所有值
  let mut values = []
  for metric in telemetry_metrics {
    values = values.push(metric.value)
  }
  
  // 计算阈值
  let thresholds = calculate_thresholds(values)
  
  // 检测异常值
  let mut anomalies = []
  for metric in telemetry_metrics {
    if metric.value < thresholds.lower or metric.value > thresholds.upper {
      anomalies = anomalies.push({
        metric: metric.name,
        value: metric.value,
        timestamp: metric.timestamp,
        deviation: if metric.value > thresholds.mean {
          metric.value - thresholds.upper
        } else {
          metric.value - thresholds.lower
        }
      })
    }
  }
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 2)
  assert_eq(anomalies[0].value, 95.0)
  assert_eq(anomalies[1].value, 98.0)
  assert_true(anomalies[0].deviation > 0)
  assert_true(anomalies[1].deviation > 0)
}

// 测试3: 遥测数据的实时告警
test "遥测数据的实时告警系统" {
  // 模拟触发告警的遥测数据
  let alert_conditions = [
    { metric: "error_rate", threshold: 5.0, operator: ">", severity: "critical" },
    { metric: "response_time", threshold: 1000.0, operator: ">", severity: "warning" },
    { metric: "cpu_usage", threshold: 90.0, operator: ">", severity: "warning" },
    { metric: "memory_usage", threshold: 95.0, operator: ">", severity: "critical" },
    { metric: "disk_space", threshold: 10.0, operator: "<", severity: "critical" }
  ]
  
  // 模拟实时遥测数据流
  let telemetry_stream = [
    { metric: "error_rate", value: 7.5, timestamp: 1640995200 },
    { metric: "response_time", value: 850.0, timestamp: 1640995201 },
    { metric: "cpu_usage", value: 92.0, timestamp: 1640995202 },
    { metric: "memory_usage", value: 98.0, timestamp: 1640995203 },
    { metric: "disk_space", value: 8.0, timestamp: 1640995204 }
  ]
  
  // 告警评估函数
  let evaluate_alert = fn(condition, data_point) {
    let threshold_met = match condition.operator {
      ">" => data_point.value > condition.threshold
      "<" => data_point.value < condition.threshold
      ">=" => data_point.value >= condition.threshold
      "<=" => data_point.value <= condition.threshold
      "==" => data_point.value == condition.threshold
      _ => false
    }
    
    if threshold_met {
      Some({
        metric: condition.metric,
        current_value: data_point.value,
        threshold: condition.threshold,
        severity: condition.severity,
        timestamp: data_point.timestamp
      })
    } else {
      None
    }
  }
  
  // 处理数据流并生成告警
  let mut alerts = []
  for data_point in telemetry_stream {
    for condition in alert_conditions {
      if data_point.metric == condition.metric {
        let alert = evaluate_alert(condition, data_point)
        match alert {
          Some(a) => alerts = alerts.push(a)
          None => ()
        }
      }
    }
  }
  
  // 验证告警结果
  assert_eq(alerts.length(), 4) // 除了response_time，其他都超过阈值
  
  // 验证关键告警
  let mut critical_alerts = []
  let mut warning_alerts = []
  
  for alert in alerts {
    if alert.severity == "critical" {
      critical_alerts = critical_alerts.push(alert)
    } else if alert.severity == "warning" {
      warning_alerts = warning_alerts.push(alert)
    }
  }
  
  assert_eq(critical_alerts.length(), 3)
  assert_eq(warning_alerts.length(), 1)
  
  // 验证告警内容
  assert_eq(critical_alerts[0].metric, "error_rate")
  assert_eq(critical_alerts[0].current_value, 7.5)
  assert_eq(critical_alerts[1].metric, "memory_usage")
  assert_eq(critical_alerts[2].metric, "disk_space")
  assert_eq(warning_alerts[0].metric, "cpu_usage")
}

// 测试4: 遥测数据的多租户支持
test "遥测数据的多租户支持和隔离" {
  // 模拟多租户环境
  let tenants = [
    { id: "tenant_a", name: "Acme Corp", tier: "enterprise" },
    { id: "tenant_b", name: "Startup Inc", tier: "standard" },
    { id: "tenant_c", name: "Dev Team", tier: "basic" }
  ]
  
  // 模拟来自不同租户的遥测数据
  let multi_tenant_data = [
    { tenant_id: "tenant_a", metric: "api_calls", value: 15000, timestamp: 1640995200 },
    { tenant_id: "tenant_b", metric: "api_calls", value: 5000, timestamp: 1640995201 },
    { tenant_id: "tenant_c", metric: "api_calls", value: 1000, timestamp: 1640995202 },
    { tenant_id: "tenant_a", metric: "storage_gb", value: 500.0, timestamp: 1640995203 },
    { tenant_id: "tenant_b", metric: "storage_gb", value: 100.0, timestamp: 1640995204 },
    { tenant_id: "tenant_c", metric: "storage_gb", value: 20.0, timestamp: 1640995205 }
  ]
  
  // 租户配额限制
  let tenant_quotas = {
    "tenant_a": { api_calls: 20000, storage_gb: 1000.0 },
    "tenant_b": { api_calls: 10000, storage_gb: 200.0 },
    "tenant_c": { api_calls: 5000, storage_gb: 50.0 }
  }
  
  // 租户数据隔离和聚合
  let isolate_tenant_data = fn(tenant_id, data) {
    let mut tenant_data = []
    for data_point in data {
      if data_point.tenant_id == tenant_id {
        tenant_data = tenant_data.push(data_point)
      }
    }
    tenant_data
  }
  
  // 计算租户资源使用情况
  let calculate_usage = fn(tenant_id, data) {
    let tenant_data = isolate_tenant_data(tenant_id, data)
    let mut api_calls = 0
    let mut storage_gb = 0.0
    
    for data_point in tenant_data {
      if data_point.metric == "api_calls" {
        api_calls = api_calls + data_point.value
      } else if data_point.metric == "storage_gb" {
        storage_gb = storage_gb + data_point.value
      }
    }
    
    { api_calls, storage_gb }
  }
  
  // 检查配额使用情况
  let check_quota_usage = fn(tenant_id, usage, quotas) {
    let quota = quotas[tenant_id]
    {
      api_calls_usage_percent: (usage.api_calls.to_float() / quota.api_calls.to_float()) * 100.0,
      storage_usage_percent: (usage.storage_gb / quota.storage_gb) * 100.0,
      api_calls_exceeded: usage.api_calls > quota.api_calls,
      storage_exceeded: usage.storage_gb > quota.storage_gb
    }
  }
  
  // 计算所有租户的使用情况
  let mut tenant_usages = {}
  for tenant in tenants {
    let usage = calculate_usage(tenant.id, multi_tenant_data)
    let quota_usage = check_quota_usage(tenant.id, usage, tenant_quotas)
    tenant_usages = { tenant_usages | (tenant.id, { usage, quota_usage }) }
  }
  
  // 验证多租户隔离和配额检查
  assert_eq(tenant_usages["tenant_a"].usage.api_calls, 15000)
  assert_eq(tenant_usages["tenant_a"].usage.storage_gb, 500.0)
  assert_eq(tenant_usages["tenant_b"].usage.api_calls, 5000)
  assert_eq(tenant_usages["tenant_c"].usage.storage_gb, 20.0)
  
  // 验证配额使用百分比
  assert_eq(tenant_usages["tenant_a"].quota_usage.api_calls_usage_percent, 75.0)
  assert_eq(tenant_usages["tenant_a"].quota_usage.storage_usage_percent, 50.0)
  assert_eq(tenant_usages["tenant_b"].quota_usage.api_calls_usage_percent, 50.0)
  
  // 验证没有超出配额
  assert_false(tenant_usages["tenant_a"].quota_usage.api_calls_exceeded)
  assert_false(tenant_usages["tenant_a"].quota_usage.storage_exceeded)
}

// 测试5: 遥测数据的自动扩缩容
test "遥测数据的自动扩缩容决策" {
  // 模拟负载监控数据
  let load_metrics = [
    { timestamp: 1640995200, cpu: 65.0, memory: 70.0, request_rate: 1000, active_connections: 500 },
    { timestamp: 1640995260, cpu: 75.0, memory: 75.0, request_rate: 1200, active_connections: 600 },
    { timestamp: 1640995320, cpu: 85.0, memory: 80.0, request_rate: 1500, active_connections: 750 },
    { timestamp: 1640995380, cpu: 90.0, memory: 85.0, request_rate: 1800, active_connections: 900 },
    { timestamp: 1640995440, cpu: 70.0, memory: 72.0, request_rate: 1100, active_connections: 550 }
  ]
  
  // 扩缩容策略配置
  let scaling_policy = {
    scale_up_threshold: 80.0,    // CPU使用率超过80%时扩容
    scale_down_threshold: 30.0,  // CPU使用率低于30%时缩容
    min_instances: 2,            // 最小实例数
    max_instances: 10,           // 最大实例数
    scale_up_cooldown: 300,      // 扩容冷却时间（秒）
    scale_down_cooldown: 600     // 缩容冷却时间（秒）
  }
  
  // 当前服务状态
  let current_state = {
    instances: 4,
    last_scale_action: 1640995000,  // 上次扩缩容时间
    last_action_type: "scale_up"
  }
  
  // 扩缩容决策函数
  let make_scaling_decision = fn(metrics, policy, state) {
    // 获取最新的CPU使用率
    let latest_cpu = metrics[metrics.length() - 1].cpu
    
    // 检查是否在冷却期内
    let now = metrics[metrics.length() - 1].timestamp
    let cooldown_passed = match state.last_action_type {
      "scale_up" => (now - state.last_scale_action) >= policy.scale_up_cooldown
      "scale_down" => (now - state.last_scale_action) >= policy.scale_down_cooldown
      _ => true
    }
    
    // 决策逻辑
    if latest_cpu > policy.scale_up_threshold and cooldown_passed and state.instances < policy.max_instances {
      // 扩容决策
      {
        action: "scale_up",
        current_instances: state.instances,
        target_instances: state.instances + 1,
        reason: "CPU usage (" + latest_cpu.to_string() + "%) exceeds threshold (" + policy.scale_up_threshold.to_string() + "%)"
      }
    } else if latest_cpu < policy.scale_down_threshold and cooldown_passed and state.instances > policy.min_instances {
      // 缩容决策
      {
        action: "scale_down",
        current_instances: state.instances,
        target_instances: state.instances - 1,
        reason: "CPU usage (" + latest_cpu.to_string() + "%) below threshold (" + policy.scale_down_threshold.to_string() + "%)"
      }
    } else {
      // 不采取行动
      {
        action: "no_action",
        current_instances: state.instances,
        target_instances: state.instances,
        reason: "CPU usage (" + latest_cpu.to_string() + "%) within thresholds or in cooldown period"
      }
    }
  }
  
  // 执行扩缩容决策
  let scaling_decision = make_scaling_decision(load_metrics, scaling_policy, current_state)
  
  // 验证扩缩容决策
  assert_eq(scaling_decision.action, "scale_up")
  assert_eq(scaling_decision.current_instances, 4)
  assert_eq(scaling_decision.target_instances, 5)
  assert_true(scaling_decision.reason.contains("CPU usage"))
  assert_true(scaling_decision.reason.contains("90.0"))
  
  // 测试缩容场景
  let low_load_metrics = [
    { timestamp: 1640996000, cpu: 25.0, memory: 30.0, request_rate: 200, active_connections: 100 },
    { timestamp: 1640996060, cpu: 28.0, memory: 32.0, request_rate: 250, active_connections: 120 }
  ]
  
  let scale_down_state = {
    instances: 6,
    last_scale_action: 1640995000,
    last_action_type: "scale_up"
  }
  
  let scale_down_decision = make_scaling_decision(low_load_metrics, scaling_policy, scale_down_state)
  
  // 验证缩容决策
  assert_eq(scale_down_decision.action, "scale_down")
  assert_eq(scale_down_decision.current_instances, 6)
  assert_eq(scale_down_decision.target_instances, 5)
  assert_true(scale_down_decision.reason.contains("CPU usage"))
  assert_true(scale_down_decision.reason.contains("below threshold"))
}

// 测试6: 遥测数据的边缘计算处理
test "遥测数据的边缘计算处理" {
  // 模拟边缘设备遥测数据
  let edge_device_data = [
    { device_id: "edge_001", location: "warehouse_a", temperature: 25.5, humidity: 60.0, timestamp: 1640995200 },
    { device_id: "edge_002", location: "warehouse_a", temperature: 24.8, humidity: 62.0, timestamp: 1640995201 },
    { device_id: "edge_003", location: "warehouse_b", temperature: 26.2, humidity: 58.0, timestamp: 1640995202 },
    { device_id: "edge_001", location: "warehouse_a", temperature: 30.0, humidity: 65.0, timestamp: 1640995203 },
    { device_id: "edge_002", location: "warehouse_a", temperature: 31.5, humidity: 68.0, timestamp: 1640995204 }
  ]
  
  // 边缘计算处理规则
  let edge_processing_rules = {
    temperature_threshold: 28.0,  // 温度阈值
    humidity_threshold: 65.0,     // 湿度阈值
    aggregation_window: 5,        // 聚合窗口大小
    compression_ratio: 0.5        // 压缩比率
  }
  
  // 边缘数据聚合函数
  let aggregate_edge_data = fn(data, location, rules) {
    let mut location_data = []
    for data_point in data {
      if data_point.location == location {
        location_data = location_data.push(data_point)
      }
    }
    
    // 计算平均值
    let mut temp_sum = 0.0
    let mut humidity_sum = 0.0
    let count = location_data.length().to_float()
    
    for data_point in location_data {
      temp_sum = temp_sum + data_point.temperature
      humidity_sum = humidity_sum + data_point.humidity
    }
    
    {
      location,
      avg_temperature: temp_sum / count,
      avg_humidity: humidity_sum / count,
      device_count: location_data.length(),
      timestamp: location_data[location_data.length() - 1].timestamp
    }
  }
  
  // 边缘异常检测函数
  let detect_edge_anomalies = fn(data, rules) {
    let mut anomalies = []
    for data_point in data {
      let is_temp_anomaly = data_point.temperature > rules.temperature_threshold
      let is_humidity_anomaly = data_point.humidity > rules.humidity_threshold
      
      if is_temp_anomaly or is_humidity_anomaly {
        anomalies = anomalies.push({
          device_id: data_point.device_id,
          location: data_point.location,
          temperature: data_point.temperature,
          humidity: data_point.humidity,
          timestamp: data_point.timestamp,
          anomaly_type: if is_temp_anomaly and is_humidity_anomaly {
            "both"
          } else if is_temp_anomaly {
            "temperature"
          } else {
            "humidity"
          }
        })
      }
    }
    anomalies
  }
  
  // 边缘数据压缩函数
  let compress_edge_data = fn(data, compression_ratio) {
    // 简单压缩：只保留部分数据点
    let keep_count = (data.length().to_float() * compression_ratio).to_int()
    let step = data.length() / keep_count
    let mut compressed = []
    
    let mut i = 0
    while i < data.length() {
      compressed = compressed.push(data[i])
      i = i + step
    }
    
    compressed
  }
  
  // 执行边缘计算处理
  let warehouse_a_aggregated = aggregate_edge_data(edge_device_data, "warehouse_a", edge_processing_rules)
  let warehouse_b_aggregated = aggregate_edge_data(edge_device_data, "warehouse_b", edge_processing_rules)
  
  // 检测边缘异常
  let edge_anomalies = detect_edge_anomalies(edge_device_data, edge_processing_rules)
  
  // 压缩边缘数据
  let compressed_data = compress_edge_data(edge_device_data, edge_processing_rules.compression_ratio)
  
  // 验证边缘聚合结果
  assert_eq(warehouse_a_aggregated.location, "warehouse_a")
  assert_eq(warehouse_a_aggregated.device_count, 4)
  assert_eq(warehouse_b_aggregated.location, "warehouse_b")
  assert_eq(warehouse_b_aggregated.device_count, 1)
  
  // 验证边缘异常检测结果
  assert_eq(edge_anomalies.length(), 2)
  assert_eq(edge_anomalies[0].device_id, "edge_001")
  assert_eq(edge_anomalies[0].anomaly_type, "temperature")
  assert_eq(edge_anomalies[1].device_id, "edge_002")
  assert_eq(edge_anomalies[1].anomaly_type, "both")
  
  // 验证数据压缩效果
  assert_true(compressed_data.length() < edge_device_data.length())
  assert_eq(compressed_data[0].device_id, "edge_001")
}

// 测试7: 遥测数据的机器学习应用
test "遥测数据的机器学习预测分析" {
  // 模拟历史遥测数据用于训练
  let historical_metrics = [
    { timestamp: 1640990000, cpu: 45.0, memory: 60.0, request_rate: 800, response_time: 120.0 },
    { timestamp: 1640990300, cpu: 50.0, memory: 62.0, request_rate: 850, response_time: 125.0 },
    { timestamp: 1640990600, cpu: 55.0, memory: 65.0, request_rate: 900, response_time: 130.0 },
    { timestamp: 1640990900, cpu: 60.0, memory: 68.0, request_rate: 950, response_time: 135.0 },
    { timestamp: 1640991200, cpu: 65.0, memory: 70.0, request_rate: 1000, response_time: 140.0 },
    { timestamp: 1640991500, cpu: 70.0, memory: 73.0, request_rate: 1050, response_time: 150.0 },
    { timestamp: 1640991800, cpu: 75.0, memory: 75.0, request_rate: 1100, response_time: 160.0 },
    { timestamp: 1640992100, cpu: 80.0, memory: 78.0, request_rate: 1150, response_time: 175.0 }
  ]
  
  // 简单线性回归预测模型
  let train_linear_model = fn(data) {
    // 计算CPU和响应时间的线性关系
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    let n = data.length().to_float()
    
    for point in data {
      sum_x = sum_x + point.cpu
      sum_y = sum_y + point.response_time
      sum_xy = sum_xy + point.cpu * point.response_time
      sum_x2 = sum_x2 + point.cpu * point.cpu
    }
    
    // 计算斜率和截距
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    { slope, intercept }
  }
  
  // 使用模型进行预测
  let predict_response_time = fn(model, cpu_usage) {
    model.slope * cpu_usage + model.intercept
  }
  
  // 训练模型
  let prediction_model = train_linear_model(historical_metrics)
  
  // 测试预测
  let predicted_time_85 = predict_response_time(prediction_model, 85.0)
  let predicted_time_90 = predict_response_time(prediction_model, 90.0)
  
  // 验证预测结果
  assert_true(predicted_time_85 > 175.0)  // 应该比历史最高值高
  assert_true(predicted_time_90 > predicted_time_85)  // CPU越高，响应时间越长
  
  // 异常检测基于预测
  let current_metrics = [
    { timestamp: 1640995200, cpu: 70.0, memory: 72.0, request_rate: 1050, response_time: 200.0 }, // 异常
    { timestamp: 1640995260, cpu: 75.0, memory: 75.0, request_rate: 1100, response_time: 165.0 }  // 正常
  ]
  
  let detect_prediction_anomalies = fn(model, data) {
    let mut anomalies = []
    for point in data {
      let expected = predict_response_time(model, point.cpu)
      let deviation = point.response_time - expected
      let percent_deviation = (deviation.abs() / expected) * 100.0
      
      // 如果偏差超过20%，标记为异常
      if percent_deviation > 20.0 {
        anomalies = anomalies.push({
          timestamp: point.timestamp,
          cpu: point.cpu,
          actual_response_time: point.response_time,
          expected_response_time: expected,
          deviation_percent: percent_deviation
        })
      }
    }
    anomalies
  }
  
  // 检测预测异常
  let prediction_anomalies = detect_prediction_anomalies(prediction_model, current_metrics)
  
  // 验证异常检测结果
  assert_eq(prediction_anomalies.length(), 1)
  assert_eq(prediction_anomalies[0].cpu, 70.0)
  assert_eq(prediction_anomalies[0].actual_response_time, 200.0)
  assert_true(prediction_anomalies[0].deviation_percent > 20.0)
}

// 测试8: 遥测数据的可视化处理
test "遥测数据的可视化处理和格式化" {
  // 模拟需要可视化的遥测数据
  let visualization_data = [
    { metric: "cpu_usage", values: [45.0, 50.0, 55.0, 60.0, 58.0], timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440] },
    { metric: "memory_usage", values: [60.0, 62.0, 65.0, 68.0, 67.0], timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440] },
    { metric: "response_time", values: [120.0, 125.0, 130.0, 140.0, 135.0], timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440] }
  ]
  
  // 时间序列数据格式化为图表数据点
  let format_time_series = fn(metric_data) {
    let mut chart_points = []
    let values = metric_data.values
    let timestamps = metric_data.timestamps
    
    for i in 0..values.length() {
      chart_points = chart_points.push({
        x: timestamps[i],
        y: values[i],
        label: metric_data.metric + ": " + values[i].to_string()
      })
    }
    
    {
      metric: metric_data.metric,
      data_points: chart_points,
      min_value: values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0]),
      max_value: values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0]),
      avg_value: values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
    }
  }
  
  // 生成饼图数据
  let generate_pie_chart_data = fn(metrics) {
    let total = metrics.reduce(fn(acc, m) { acc + m.values[m.values.length() - 1] }, 0.0)
    let mut pie_slices = []
    
    for metric in metrics {
      let current_value = metric.values[metric.values.length() - 1]
      let percentage = (current_value / total) * 100.0
      
      pie_slices = pie_slices.push({
        label: metric.metric,
        value: current_value,
        percentage: percentage,
        color: match metric.metric {
          "cpu_usage" => "#FF6B6B"
          "memory_usage" => "#4ECDC4"
          "response_time" => "#45B7D1"
          _ => "#95E1D3"
        }
      })
    }
    
    { total, slices: pie_slices }
  }
  
  // 生成热力图数据
  let generate_heatmap_data = fn(metrics, time_buckets) {
    let mut heatmap_data = []
    
    for metric in metrics {
      let values = metric.values
      let mut bucket_data = []
      
      for i in 0..time_buckets {
        let start_idx = (values.length() * i) / time_buckets
        let end_idx = (values.length() * (i + 1)) / time_buckets
        let mut bucket_sum = 0.0
        let mut bucket_count = 0
        
        for j in start_idx..end_idx {
          if j < values.length() {
            bucket_sum = bucket_sum + values[j]
            bucket_count = bucket_count + 1
          }
        }
        
        let bucket_avg = if bucket_count > 0 {
          bucket_sum / bucket_count.to_float()
        } else {
          0.0
        }
        
        bucket_data = bucket_data.push(bucket_avg)
      }
      
      heatmap_data = heatmap_data.push({
        metric: metric.metric,
        buckets: bucket_data
      })
    }
    
    heatmap_data
  }
  
  // 执行可视化处理
  let mut chart_data = []
  for metric_data in visualization_data {
    chart_data = chart_data.push(format_time_series(metric_data))
  }
  
  let pie_chart_data = generate_pie_chart_data(visualization_data)
  let heatmap_data = generate_heatmap_data(visualization_data, 3)
  
  // 验证时间序列图表数据
  assert_eq(chart_data.length(), 3)
  assert_eq(chart_data[0].metric, "cpu_usage")
  assert_eq(chart_data[0].data_points.length(), 5)
  assert_eq(chart_data[0].min_value, 45.0)
  assert_eq(chart_data[0].max_value, 60.0)
  
  // 验证饼图数据
  assert_true(pie_chart_data.total > 0.0)
  assert_eq(pie_chart_data.slices.length(), 3)
  assert_eq(pie_chart_data.slices[0].label, "cpu_usage")
  assert_eq(pie_chart_data.slices[0].color, "#FF6B6B")
  assert_true(pie_chart_data.slices[0].percentage > 0.0)
  
  // 验证热力图数据
  assert_eq(heatmap_data.length(), 3)
  assert_eq(heatmap_data[0].metric, "cpu_usage")
  assert_eq(heatmap_data[0].buckets.length(), 3)
  assert_true(heatmap_data[0].buckets[0] >= 0.0)
}