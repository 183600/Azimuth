// Azimuth新功能综合测试用例
// 覆盖遥测系统的多个核心功能和高级特性

// 测试1: 时间序列数据处理
test "time series data processing" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // 创建时间序列数据
  let time_series = [
    { timestamp: 1640995200, value: 25.5, metadata: [("metric", "cpu"), ("unit", "percent")] },
    { timestamp: 1640995260, value: 30.2, metadata: [("metric", "cpu"), ("unit", "percent")] },
    { timestamp: 1640995320, value: 28.7, metadata: [("metric", "cpu"), ("unit", "percent")] },
    { timestamp: 1640995380, value: 35.1, metadata: [("metric", "cpu"), ("unit", "percent")] },
    { timestamp: 1640995440, value: 32.8, metadata: [("metric", "cpu"), ("unit", "percent")] }
  ]
  
  // 计算移动平均值
  let calculate_moving_average = fn(data: Array[TimeSeriesPoint], window_size: Int) {
    let mut result = []
    
    for i in window_size-1..data.length() {
      let mut sum = 0.0
      for j in (i-window_size+1)..=i {
        sum = sum + data[j].value
      }
      let avg = sum / window_size.to_float()
      result = result + [{ timestamp: data[i].timestamp, value: avg, metadata: data[i].metadata }]
    }
    
    result
  }
  
  let moving_avg = calculate_moving_average(time_series, 3)
  assert_eq(moving_avg.length(), 3)
  assert_eq(moving_avg[0].timestamp, 1640995320)
  assert_eq(moving_avg[0].value, 28.133333333333336)  // (25.5 + 30.2 + 28.7) / 3
  
  // 检测异常值
  let detect_anomalies = fn(data: Array[TimeSeriesPoint], threshold: Float) {
    let mean = data.reduce(fn(acc, point) { acc + point.value }, 0.0) / data.length().to_float()
    let variance = data.reduce(fn(acc, point) { 
      let diff = point.value - mean
      acc + diff * diff 
    }, 0.0) / data.length().to_float()
    let std_dev = variance.sqrt()
    
    data.filter(fn(point) {
      (point.value - mean).abs() > threshold * std_dev
    })
  }
  
  let anomalies = detect_anomalies(time_series, 1.5)
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].timestamp, 1640995380)
  assert_eq(anomalies[0].value, 35.1)
  
  // 时间序列下采样
  let downsample = fn(data: Array[TimeSeriesPoint], interval: Int) {
    let mut result = []
    let mut i = 0
    
    while i < data.length() {
      let start_time = data[i].timestamp
      let end_time = start_time + interval
      
      let window = data.filter(fn(point) {
        point.timestamp >= start_time and point.timestamp < end_time
      })
      
      if window.length() > 0 {
        let avg_value = window.reduce(fn(acc, point) { acc + point.value }, 0.0) / window.length().to_float()
        result = result + [{ 
          timestamp: start_time, 
          value: avg_value, 
          metadata: window[0].metadata 
        }]
      }
      
      i = i + 1
      while i < data.length() and data[i].timestamp < end_time {
        i = i + 1
      }
    }
    
    result
  }
  
  let downsampled = downsample(time_series, 600)  // 10分钟间隔
  assert_eq(downsampled.length(), 1)
  assert_eq(downsampled[0].timestamp, 1640995200)
  assert_eq(downsampled[0].value, 30.46)  // 所有值的平均
}

// 测试2: 遥测数据压缩
test "telemetry data compression" {
  // 定义原始遥测数据
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据
  let telemetry_data = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "http_request",
      start_time: 1640995200,
      duration: 100,
      status: "ok",
      attributes: [("http.method", "GET"), ("http.url", "/api/users")]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database_query",
      start_time: 1640995220,
      duration: 50,
      status: "ok",
      attributes: [("db.type", "postgresql"), ("db.table", "users")]
    },
    {
      trace_id: "trace-002",
      span_id: "span-003",
      parent_span_id: None,
      operation_name: "http_request",
      start_time: 1640995300,
      duration: 200,
      status: "error",
      attributes: [("http.method", "POST"), ("http.url", "/api/orders")]
    }
  ]
  
  // 字典压缩
  let create_dictionary = fn(data: Array[TelemetryData]) {
    let mut string_dict = {}
    let mut next_id = 0
    let compressed_data = []
    
    for record in data {
      let compress_string = fn(s: String) {
        match string_dict.get(s) {
          Some(id) => id
          None => {
            let id = next_id
            string_dict = string_dict.set(s, id)
            next_id = next_id + 1
            id
          }
        }
      }
      
      let compressed_attrs = record.attributes.map(fn(attr) {
        match attr {
          (key, value) => (compress_string(key), compress_string(value))
        }
      })
      
      compressed_data = compressed_data + [{
        trace_id: compress_string(record.trace_id),
        span_id: compress_string(record.span_id),
        parent_span_id: match record.parent_span_id {
          Some(id) => Some(compress_string(id))
          None => None
        },
        operation_name: compress_string(record.operation_name),
        start_time: record.start_time,
        duration: record.duration,
        status: compress_string(record.status),
        attributes: compressed_attrs
      }]
    }
    
    (string_dict, compressed_data)
  }
  
  let (dictionary, compressed) = create_dictionary(telemetry_data)
  
  // 验证压缩结果
  assert_eq(compressed.length(), 3)
  assert_eq(compressed[0].duration, 100)
  assert_eq(compressed[1].duration, 50)
  assert_eq(compressed[2].duration, 200)
  
  // 计算压缩率
  let original_size = telemetry_data.reduce(fn(acc, record) {
    acc + record.trace_id.length() + record.span_id.length() + 
    record.operation_name.length() + record.status.length() +
    record.attributes.reduce(fn(attr_acc, attr) {
      match attr {
        (key, value) => attr_acc + key.length() + value.length()
      }
    }, 0)
  }, 0)
  
  let compressed_size = compressed.reduce(fn(acc, record) {
    acc + record.attributes.length() * 2  // 假设每个压缩ID占2字节
  }, 0) + dictionary.size() * 10  // 假设字典条目平均10字节
  
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 1.0)  // 压缩后应该更小
  
  // 增量压缩
  let incremental_compress = fn(existing_dict: Map[String, Int], next_id: Int, new_data: Array[TelemetryData]) {
    let mut string_dict = existing_dict
    let mut current_next_id = next_id
    let compressed_data = []
    
    for record in new_data {
      let compress_string = fn(s: String) {
        match string_dict.get(s) {
          Some(id) => id
          None => {
            let id = current_next_id
            string_dict = string_dict.set(s, id)
            current_next_id = current_next_id + 1
            id
          }
        }
      }
      
      compressed_data = compressed_data + [{
        trace_id: compress_string(record.trace_id),
        span_id: compress_string(record.span_id),
        parent_span_id: match record.parent_span_id {
          Some(id) => Some(compress_string(id))
          None => None
        },
        operation_name: compress_string(record.operation_name),
        start_time: record.start_time,
        duration: record.duration,
        status: compress_string(record.status),
        attributes: []  // 简化
      }]
    }
    
    (string_dict, current_next_id, compressed_data)
  }
  
  let new_telemetry_data = [
    {
      trace_id: "trace-001",  // 已存在的trace_id
      span_id: "span-004",
      parent_span_id: Some("span-001"),  // 已存在的parent_span_id
      operation_name: "cache_lookup",
      start_time: 1640995400,
      duration: 25,
      status: "ok",
      attributes: [("cache.type", "redis")]
    }
  ]
  
  let (updated_dict, new_next_id, new_compressed) = incremental_compress(dictionary, 10, new_telemetry_data)
  
  // 验证增量压缩
  assert_eq(new_compressed.length(), 1)
  assert_eq(new_compressed[0].duration, 25)
  assert_true(new_next_id > 10)  // 应该添加了新的字典条目
}

// 测试3: 分布式追踪一致性
test "distributed tracing consistency" {
  // 定义分布式追踪节点
  type TraceNode = {
    service_name: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation: String,
    start_time: Int,
    end_time: Int,
    tags: Array[(String, String)]
  }
  
  // 创建跨服务的追踪数据
  let distributed_trace = [
    // API Gateway
    {
      service_name: "api-gateway",
      trace_id: "trace-123",
      span_id: "span-001",
      parent_span_id: None,
      operation: "http.request",
      start_time: 1640995200,
      end_time: 1640995250,
      tags: [("http.method", "GET"), ("http.path", "/api/data")]
    },
    // Auth Service
    {
      service_name: "auth-service",
      trace_id: "trace-123",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation: "auth.validate",
      start_time: 1640995210,
      end_time: 1640995230,
      tags: [("user.id", "user-456"), ("auth.result", "success")]
    },
    // Data Service
    {
      service_name: "data-service",
      trace_id: "trace-123",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation: "data.query",
      start_time: 1640995235,
      end_time: 1640995245,
      tags: [("db.query", "SELECT * FROM data"), ("db.rows", "10")]
    }
  ]
  
  // 验证追踪一致性
  let validate_trace_consistency = fn(trace: Array[TraceNode]) {
    let mut issues = []
    let trace_ids = trace.map(fn(node) { node.trace_id }).unique()
    
    // 检查所有节点是否有相同的trace_id
    if trace_ids.length() > 1 {
      issues = issues + "Multiple trace IDs found: " + trace_ids.join(", ")
    }
    
    // 检查父子关系
    for node in trace {
      match node.parent_span_id {
        Some(parent_id) => {
          let parent_exists = trace.any(fn(p) { p.span_id == parent_id })
          if not(parent_exists) {
            issues = issues + "Parent span " + parent_id + " not found for span " + node.span_id
          }
        }
        None => ()  // 根span，没有父级
      }
    }
    
    // 检查时间顺序
    let sorted_by_time = trace.sort(fn(a, b) { a.start_time <= b.start_time })
    for i in 1..sorted_by_time.length() {
      let current = sorted_by_time[i]
      let previous = sorted_by_time[i-1]
      
      if current.start_time < previous.start_time {
        issues = issues + "Time order violation: span " + current.span_id + " starts before " + previous.span_id
      }
    }
    
    issues
  }
  
  let consistency_issues = validate_trace_consistency(distributed_trace)
  assert_eq(consistency_issues.length(), 0)
  
  // 创建不一致的追踪数据进行测试
  let inconsistent_trace = [
    {
      service_name: "service-a",
      trace_id: "trace-456",
      span_id: "span-001",
      parent_span_id: None,
      operation: "operation.a",
      start_time: 1640995200,
      end_time: 1640995250,
      tags: []
    },
    {
      service_name: "service-b",
      trace_id: "trace-789",  // 不同的trace_id
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation: "operation.b",
      start_time: 1640995210,
      end_time: 1640995230,
      tags: []
    },
    {
      service_name: "service-c",
      trace_id: "trace-456",
      span_id: "span-003",
      parent_span_id: Some("span-999"),  // 不存在的父span
      operation: "operation.c",
      start_time: 1640995190,  // 时间顺序错误
      end_time: 1640995240,
      tags: []
    }
  ]
  
  let inconsistent_issues = validate_trace_consistency(inconsistent_trace)
  assert_eq(inconsistent_issues.length(), 3)
  assert_true(inconsistent_issues.any(fn(issue) { issue.contains("Multiple trace IDs") }))
  assert_true(inconsistent_issues.any(fn(issue) { issue.contains("Parent span span-999 not found") }))
  assert_true(inconsistent_issues.any(fn(issue) { issue.contains("Time order violation") }))
  
  // 计算追踪统计信息
  let calculate_trace_stats = fn(trace: Array[TraceNode]) {
    let services = trace.map(fn(node) { node.service_name }).unique()
    let total_duration = trace.reduce(fn(acc, node) { 
      acc + (node.end_time - node.start_time) 
    }, 0)
    let trace_start = trace.reduce(fn(acc, node) { 
      if node.start_time < acc { node.start_time } else { acc } 
    }, trace[0].start_time)
    let trace_end = trace.reduce(fn(acc, node) { 
      if node.end_time > acc { node.end_time } else { acc } 
    }, trace[0].end_time)
    
    {
      trace_id: trace[0].trace_id,
      service_count: services.length(),
      services: services,
      span_count: trace.length(),
      total_duration: total_duration,
      trace_duration: trace_end - trace_start,
      critical_path: trace.sort(fn(a, b) { (b.end_time - b.start_time) >= (a.end_time - a.start_time) })[0].span_id
    }
  }
  
  let trace_stats = calculate_trace_stats(distributed_trace)
  assert_eq(trace_stats.trace_id, "trace-123")
  assert_eq(trace_stats.service_count, 3)
  assert_eq(trace_stats.span_count, 3)
  assert_eq(trace_stats.total_duration, 100)  // 50 + 20 + 30
  assert_eq(trace_stats.trace_duration, 50)   // 1640995250 - 1640995200
}

// 测试4: 动态配置管理
test "dynamic configuration management" {
  // 定义配置项类型
  type ConfigValue = 
    | StringValue(String)
    | IntValue(Int)
    | FloatValue(Float)
    | BoolValue(Bool)
    | ArrayValue(Array[String])
  
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    description: String,
    category: String,
    mutable: Bool
  }
  
  // 创建初始配置
  let initial_config = [
    {
      key: "telemetry.enabled",
      value: BoolValue(true),
      description: "Enable/disable telemetry collection",
      category: "telemetry",
      mutable: true
    },
    {
      key: "telemetry.sampling_rate",
      value: FloatValue(0.1),
      description: "Telemetry sampling rate (0.0-1.0)",
      category: "telemetry",
      mutable: true
    },
    {
      key: "service.name",
      value: StringValue("payment-service"),
      description: "Service name for telemetry",
      category: "service",
      mutable: false
    },
    {
      key: "service.version",
      value: StringValue("1.2.3"),
      description: "Service version",
      category: "service",
      mutable: false
    },
    {
      key: "export.endpoints",
      value: ArrayValue(["http://collector:4317"]),
      description: "Telemetry export endpoints",
      category: "export",
      mutable: true
    }
  ]
  
  // 配置查找函数
  let get_config = fn(config: Array[ConfigItem], key: String) {
    let mut found = None
    for item in config {
      if item.key == key {
        found = Some(item)
        break
      }
    }
    found
  }
  
  // 测试配置查找
  let telemetry_enabled = get_config(initial_config, "telemetry.enabled")
  match telemetry_enabled {
    Some(item) => {
      match item.value {
        BoolValue(value) => assert_true(value)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let sampling_rate = get_config(initial_config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(item) => {
      match item.value {
        FloatValue(value) => assert_eq(value, 0.1)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let missing_config = get_config(initial_config, "nonexistent.key")
  assert_eq(missing_config, None)
  
  // 配置更新函数
  let update_config = fn(config: Array[ConfigItem], key: String, new_value: ConfigValue) {
    let mut updated = []
    let mut found = false
    
    for item in config {
      if item.key == key and item.mutable {
        updated = updated + [{ item | value = new_value }]
        found = true
      } else {
        updated = updated + [item]
      }
    }
    
    if not(found) {
      // 如果配置项不存在，添加新配置项
      updated = updated + [{
        key: key,
        value: new_value,
        description: "Auto-generated config item",
        category: "custom",
        mutable: true
      }]
    }
    
    updated
  }
  
  // 测试可变配置更新
  let updated_config1 = update_config(initial_config, "telemetry.sampling_rate", FloatValue(0.2))
  let updated_sampling_rate = get_config(updated_config1, "telemetry.sampling_rate")
  match updated_sampling_rate {
    Some(item) => {
      match item.value {
        FloatValue(value) => assert_eq(value, 0.2)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试不可变配置更新（应该失败）
  let updated_config2 = update_config(updated_config1, "service.name", StringValue("updated-service"))
  let service_name = get_config(updated_config2, "service.name")
  match service_name {
    Some(item) => {
      match item.value {
        StringValue(value) => assert_eq(value, "payment-service")  // 应该保持原值
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试新配置项添加
  let updated_config3 = update_config(updated_config2, "new.config.key", StringValue("new-value"))
  let new_config = get_config(updated_config3, "new.config.key")
  match new_config {
    Some(item) => {
      match item.value {
        StringValue(value) => assert_eq(value, "new-value")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 配置验证函数
  let validate_config = fn(config: Array[ConfigItem]) {
    let mut issues = []
    
    for item in config {
      match item.value {
        FloatValue(rate) => {
          if item.key == "telemetry.sampling_rate" and (rate < 0.0 or rate > 1.0) {
            issues = issues + "Invalid sampling rate: " + rate.to_string() + " (must be between 0.0 and 1.0)"
          }
        }
        ArrayValue(endpoints) => {
          if item.key == "export.endpoints" and endpoints.length() == 0 {
            issues = issues + "Export endpoints cannot be empty"
          }
        }
        _ => ()
      }
    }
    
    issues
  }
  
  let validation_issues = validate_config(updated_config3)
  assert_eq(validation_issues.length(), 0)
  
  // 测试无效配置
  let invalid_config = update_config(updated_config3, "telemetry.sampling_rate", FloatValue(1.5))
  let invalid_config2 = update_config(invalid_config, "export.endpoints", ArrayValue([]))
  
  let invalid_issues = validate_config(invalid_config2)
  assert_eq(invalid_issues.length(), 2)
  assert_true(invalid_issues.any(fn(issue) { issue.contains("Invalid sampling rate") }))
  assert_true(invalid_issues.any(fn(issue) { issue.contains("Export endpoints cannot be empty") }))
  
  // 配置分类和过滤
  let filter_by_category = fn(config: Array[ConfigItem], category: String) {
    config.filter(fn(item) { item.category == category })
  }
  
  let telemetry_configs = filter_by_category(updated_config3, "telemetry")
  assert_eq(telemetry_configs.length(), 2)
  
  let service_configs = filter_by_category(updated_config3, "service")
  assert_eq(service_configs.length(), 2)
  
  let export_configs = filter_by_category(updated_config3, "export")
  assert_eq(export_configs.length(), 1)
}

// 测试5: 资源限制下的性能测试
test "performance under resource constraints" {
  // 定义资源限制
  type ResourceLimits = {
    max_memory: Int,
    max_cpu_time: Int,
    max_file_descriptors: Int,
    max_network_connections: Int
  }
  
  type ResourceUsage = {
    memory_used: Int,
    cpu_time_used: Int,
    file_descriptors_used: Int,
    network_connections_used: Int
  }
  
  type PerformanceMetrics = {
    operation_count: Int,
    total_time: Int,
    success_count: Int,
    error_count: Int,
    average_response_time: Float
  }
  
  // 设置资源限制
  let limits = {
    max_memory: 1024 * 1024 * 100,  // 100MB
    max_cpu_time: 5000,              // 5秒
    max_file_descriptors: 1000,
    max_network_connections: 100
  }
  
  // 模拟资源使用情况
  let simulate_resource_usage = fn(operations: Int, intensity: Int) {
    let mut usage = {
      memory_used: 0,
      cpu_time_used: 0,
      file_descriptors_used: 0,
      network_connections_used: 0
    }
    
    let mut metrics = {
      operation_count: 0,
      total_time: 0,
      success_count: 0,
      error_count: 0,
      average_response_time: 0.0
    }
    
    for i in 1..=operations {
      let operation_start = 1640995200 + i
      let operation_time = intensity * (10 + (i % 20))  // 10-30ms per operation
      
      // 更新资源使用情况
      usage.memory_used = usage.memory_used + intensity * 1024  // 1KB per intensity unit
      usage.cpu_time_used = usage.cpu_time_used + operation_time
      usage.file_descriptors_used = usage.file_descriptors_used + (i % 10)
      usage.network_connections_used = usage.network_connections_used + (i % 5)
      
      // 更新性能指标
      metrics.operation_count = metrics.operation_count + 1
      metrics.total_time = metrics.total_time + operation_time
      
      // 模拟一些操作失败
      if i % 10 == 0 {
        metrics.error_count = metrics.error_count + 1
      } else {
        metrics.success_count = metrics.success_count + 1
      }
    }
    
    metrics.average_response_time = metrics.total_time.to_float() / metrics.operation_count.to_float()
    
    (usage, metrics)
  }
  
  // 测试不同负载下的性能
  let (light_usage, light_metrics) = simulate_resource_usage(100, 1)
  let (medium_usage, medium_metrics) = simulate_resource_usage(500, 5)
  let (heavy_usage, heavy_metrics) = simulate_resource_usage(1000, 10)
  
  // 验证负载增加时资源使用增加
  assert_true(heavy_usage.memory_used > medium_usage.memory_used)
  assert_true(medium_usage.memory_used > light_usage.memory_used)
  
  assert_true(heavy_usage.cpu_time_used > medium_usage.cpu_time_used)
  assert_true(medium_usage.cpu_time_used > light_usage.cpu_time_used)
  
  // 验证性能指标
  assert_eq(light_metrics.operation_count, 100)
  assert_eq(medium_metrics.operation_count, 500)
  assert_eq(heavy_metrics.operation_count, 1000)
  
  assert_eq(light_metrics.success_count + light_metrics.error_count, 100)
  assert_eq(medium_metrics.success_count + medium_metrics.error_count, 500)
  assert_eq(heavy_metrics.success_count + heavy_metrics.error_count, 1000)
  
  // 资源限制检查函数
  let check_resource_limits = fn(usage: ResourceUsage, limits: ResourceLimits) {
    let mut violations = []
    
    if usage.memory_used > limits.max_memory {
      violations = violations + "Memory limit exceeded: " + 
                   usage.memory_used.to_string() + " > " + 
                   limits.max_memory.to_string()
    }
    
    if usage.cpu_time_used > limits.max_cpu_time {
      violations = violations + "CPU time limit exceeded: " + 
                   usage.cpu_time_used.to_string() + " > " + 
                   limits.max_cpu_time.to_string()
    }
    
    if usage.file_descriptors_used > limits.max_file_descriptors {
      violations = violations + "File descriptor limit exceeded: " + 
                   usage.file_descriptors_used.to_string() + " > " + 
                   limits.max_file_descriptors.to_string()
    }
    
    if usage.network_connections_used > limits.max_network_connections {
      violations = violations + "Network connection limit exceeded: " + 
                   usage.network_connections_used.to_string() + " > " + 
                   limits.max_network_connections.to_string()
    }
    
    violations
  }
  
  // 检查资源限制违规
  let light_violations = check_resource_limits(light_usage, limits)
  let medium_violations = check_resource_limits(medium_usage, limits)
  let heavy_violations = check_resource_limits(heavy_usage, limits)
  
  assert_eq(light_violations.length(), 0)  // 轻负载不应超过限制
  assert_eq(medium_violations.length(), 0)  // 中等负载不应超过限制
  assert_true(heavy_violations.length() > 0)  // 重负载应超过某些限制
  
  // 自适应负载调整
  let adaptive_load_adjustment = fn(current_usage: ResourceUsage, limits: ResourceLimits, current_intensity: Int) {
    let memory_ratio = current_usage.memory_used.to_float() / limits.max_memory.to_float()
    let cpu_ratio = current_usage.cpu_time_used.to_float() / limits.max_cpu_time.to_float()
    
    let max_ratio = [memory_ratio, cpu_ratio].max()
    
    if max_ratio > 0.8 {
      // 资源使用超过80%，降低负载
      (current_intensity * 7) / 10  // 降低30%
    } else if max_ratio < 0.5 {
      // 资源使用低于50%，可以增加负载
      (current_intensity * 12) / 10  // 增加20%
    } else {
      current_intensity  // 保持当前负载
    }
  }
  
  let adjusted_intensity1 = adaptive_load_adjustment(light_usage, limits, 10)
  let adjusted_intensity2 = adaptive_load_adjustment(medium_usage, limits, 10)
  let adjusted_intensity3 = adaptive_load_adjustment(heavy_usage, limits, 10)
  
  assert_true(adjusted_intensity1 > 10)  // 轻负载可以增加
  assert_eq(adjusted_intensity2, 10)     // 中等负载保持
  assert_true(adjusted_intensity3 < 10)  // 重负载需要降低
  
  // 性能基准测试
  let benchmark_performance = fn(iterations: Int) {
    let results = []
    
    for i in 1..=iterations {
      let intensity = i * 2
      let (usage, metrics) = simulate_resource_usage(100, intensity)
      
      results = results + [{
        intensity: intensity,
        throughput: metrics.operation_count.to_float() / (metrics.total_time.to_float() / 1000.0),
        error_rate: metrics.error_count.to_float() / metrics.operation_count.to_float(),
        memory_efficiency: metrics.operation_count.to_float() / (usage.memory_used.to_float() / 1024.0)
      }]
    }
    
    results
  }
  
  let benchmark_results = benchmark_performance(5)
  assert_eq(benchmark_results.length(), 5)
  
  // 验证基准结果趋势
  for i in 1..benchmark_results.length() {
    let current = benchmark_results[i]
    let previous = benchmark_results[i-1]
    
    // 随着强度增加，内存效率应该提高（更多操作每KB内存）
    assert_true(current.memory_efficiency >= previous.memory_efficiency * 0.9)  // 允许一些波动
  }
}

// 测试6: 多维度数据分析
test "multi-dimensional data analysis" {
  // 定义多维度数据点
  type DataPoint = {
    timestamp: Int,
    dimensions: Map[String, String],
    metrics: Map[String, Float]
  }
  
  // 创建测试数据
  let data_points = [
    {
      timestamp: 1640995200,
      dimensions: {
        "service": "payment",
        "region": "us-west-2",
        "version": "1.2.3",
        "environment": "production"
      },
      metrics: {
        "latency": 120.5,
        "error_rate": 0.01,
        "throughput": 1000.0,
        "cpu_usage": 65.2
      }
    },
    {
      timestamp: 1640995260,
      dimensions: {
        "service": "payment",
        "region": "us-west-2",
        "version": "1.2.3",
        "environment": "production"
      },
      metrics: {
        "latency": 115.2,
        "error_rate": 0.02,
        "throughput": 950.0,
        "cpu_usage": 70.1
      }
    },
    {
      timestamp: 1640995320,
      dimensions: {
        "service": "auth",
        "region": "us-east-1",
        "version": "2.1.0",
        "environment": "production"
      },
      metrics: {
        "latency": 85.3,
        "error_rate": 0.005,
        "throughput": 800.0,
        "cpu_usage": 45.8
      }
    },
    {
      timestamp: 1640995380,
      dimensions: {
        "service": "auth",
        "region": "us-east-1",
        "version": "2.1.0",
        "environment": "staging"
      },
      metrics: {
        "latency": 95.7,
        "error_rate": 0.0,
        "throughput": 200.0,
        "cpu_usage": 30.2
      }
    },
    {
      timestamp: 1640995440,
      dimensions: {
        "service": "notification",
        "region": "eu-west-1",
        "version": "1.0.5",
        "environment": "production"
      },
      metrics: {
        "latency": 200.1,
        "error_rate": 0.05,
        "throughput": 500.0,
        "cpu_usage": 80.5
      }
    }
  ]
  
  // 按维度分组
  let group_by_dimensions = fn(data: Array[DataPoint], dimensions: Array[String]) {
    let mut groups = {}
    
    for point in data {
      let mut key_parts = []
      for dim in dimensions {
        match point.dimensions.get(dim) {
          Some(value) => key_parts = key_parts + [dim + ":" + value]
          None => key_parts = key_parts + [dim + ":unknown"]
        }
      }
      let key = key_parts.join(",")
      
      match groups.get(key) {
        Some(group) => {
          groups = groups.set(key, group + [point])
        }
        None => {
          groups = groups.set(key, [point])
        }
      }
    }
    
    groups
  }
  
  // 测试按服务分组
  let grouped_by_service = group_by_dimensions(data_points, ["service"])
  let service_names = grouped_by_service.keys()
  assert_eq(service_names.length(), 3)
  assert_true(service_names.contains("service:payment"))
  assert_true(service_names.contains("service:auth"))
  assert_true(service_names.contains("service:notification"))
  
  // 验证分组数据
  let payment_points = grouped_by_service.get("service:payment")
  match payment_points {
    Some(points) => assert_eq(points.length(), 2)
    None => assert_true(false)
  }
  
  let auth_points = grouped_by_service.get("service:auth")
  match auth_points {
    Some(points) => assert_eq(points.length(), 2)
    None => assert_true(false)
  }
  
  let notification_points = grouped_by_service.get("service:notification")
  match notification_points {
    Some(points) => assert_eq(points.length(), 1)
    None => assert_true(false)
  }
  
  // 测试按多个维度分组
  let grouped_by_service_and_region = group_by_dimensions(data_points, ["service", "region"])
  assert_eq(grouped_by_service_and_region.keys().length(), 4)
  
  // 聚合函数
  let aggregate_metrics = fn(data: Array[DataPoint], aggregation_fn: (Array[Float]) -> Float) {
    let mut metric_names = []
    
    // 收集所有指标名称
    for point in data {
      for name in point.metrics.keys() {
        if not(metric_names.contains(name)) {
          metric_names = metric_names + [name]
        }
      }
    }
    
    // 聚合每个指标
    let mut aggregated_metrics = {}
    for metric_name in metric_names {
      let mut values = []
      for point in data {
        match point.metrics.get(metric_name) {
          Some(value) => values = values + [value]
          None => ()
        }
      }
      if values.length() > 0 {
        let aggregated_value = aggregation_fn(values)
        aggregated_metrics = aggregated_metrics.set(metric_name, aggregated_value)
      }
    }
    
    aggregated_metrics
  }
  
  // 计算平均值
  let average = fn(values: Array[Float]) {
    values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
  }
  
  let payment_metrics = match payment_points {
    Some(points) => aggregate_metrics(points, average)
    None => {}
  }
  
  match payment_metrics.get("latency") {
    Some(value) => assert_eq(value, 117.85)  // (120.5 + 115.2) / 2
    None => assert_true(false)
  }
  
  match payment_metrics.get("error_rate") {
    Some(value) => assert_eq(value, 0.015)  // (0.01 + 0.02) / 2
    None => assert_true(false)
  }
  
  // 计算最大值
  let maximum = fn(values: Array[Float]) {
    values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
  }
  
  let auth_metrics = match auth_points {
    Some(points) => aggregate_metrics(points, maximum)
    None => {}
  }
  
  match auth_metrics.get("latency") {
    Some(value) => assert_eq(value, 95.7)  // max(85.3, 95.7)
    None => assert_true(false)
  }
  
  match auth_metrics.get("throughput") {
    Some(value) => assert_eq(value, 800.0)  // max(800.0, 200.0)
    None => assert_true(false)
  }
  
  // 多维度查询
  let query_data = fn(data: Array[DataPoint], dimension_filters: Map[String, String], metric_range: Option[(String, Float, Float)]) {
    let mut filtered = []
    
    for point in data {
      let mut matches = true
      
      // 检查维度过滤条件
      for (dim, value) in dimension_filters {
        match point.dimensions.get(dim) {
          Some(actual_value) => {
            if actual_value != value {
              matches = false
              break
            }
          }
          None => {
            matches = false
            break
          }
        }
      }
      
      // 检查指标范围
      if matches and metric_range.is_some() {
        match metric_range {
          Some((metric_name, min_val, max_val)) => {
            match point.metrics.get(metric_name) {
              Some(metric_value) => {
                if metric_value < min_val or metric_value > max_val {
                  matches = false
                }
              }
              None => {
                matches = false
              }
            }
          }
          None => ()
        }
      }
      
      if matches {
        filtered = filtered + [point]
      }
    }
    
    filtered
  }
  
  // 查询生产环境的payment服务
  let prod_payment_data = query_data(
    data_points, 
    { "service": "payment", "environment": "production" }, 
    None
  )
  
  assert_eq(prod_payment_data.length(), 2)
  
  // 查询延迟在100-150ms之间的数据点
  let latency_range_data = query_data(
    data_points, 
    {}, 
    Some(("latency", 100.0, 150.0))
  )
  
  assert_eq(latency_range_data.length(), 2)
  
  // 查询生产环境中错误率低于0.02的数据点
  let low_error_data = query_data(
    data_points, 
    { "environment": "production" }, 
    Some(("error_rate", 0.0, 0.02))
  )
  
  assert_eq(low_error_data.length(), 2)
  
  // 时间窗口分析
  let analyze_time_window = fn(data: Array[DataPoint], window_start: Int, window_end: Int) {
    let window_data = data.filter(fn(point) {
      point.timestamp >= window_start and point.timestamp <= window_end
    })
    
    if window_data.length() == 0 {
      None
    } else {
      let metrics = aggregate_metrics(window_data, average)
      let services = window_data.map(fn(point) { 
        match point.dimensions.get("service") {
          Some(service) => service
          None => "unknown"
        }
      }).unique()
      
      Some({
        window_start: window_start,
        window_end: window_end,
        point_count: window_data.length(),
        services: services,
        aggregated_metrics: metrics
      })
    }
  }
  
  // 分析5分钟时间窗口
  let window_analysis = analyze_time_window(data_points, 1640995200, 1640995440)
  match window_analysis {
    Some(analysis) => {
      assert_eq(analysis.point_count, 5)
      assert_eq(analysis.services.length(), 3)
      
      match analysis.aggregated_metrics.get("latency") {
        Some(value) => assert_eq(value, 123.36)  // 所有延迟值的平均
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试7: 实时流处理
test "real-time stream processing" {
  // 定义流事件类型
  type StreamEvent = 
    | MetricEvent(String, Float, Int)  // name, value, timestamp
    | LogEvent(String, String, Int)    // level, message, timestamp
    | TraceEvent(String, String, Int)  // trace_id, span_id, timestamp
    | AlertEvent(String, String, Int)  // severity, message, timestamp
  
  // 定义流处理窗口
  type ProcessingWindow = {
    events: Array[StreamEvent],
    start_time: Int,
    end_time: Int,
    size: Int
  }
  
  // 创建模拟流数据
  let stream_events = [
    MetricEvent("cpu.usage", 65.5, 1640995200),
    LogEvent("info", "Application started", 1640995205),
    TraceEvent("trace-001", "span-001", 1640995210),
    MetricEvent("memory.usage", 75.2, 1640995215),
    LogEvent("warn", "High memory usage detected", 1640995220),
    MetricEvent("cpu.usage", 70.1, 1640995225),
    TraceEvent("trace-001", "span-002", 1640995230),
    MetricEvent("request.count", 1250.0, 1640995235),
    LogEvent("error", "Database connection failed", 1640995240),
    AlertEvent("critical", "System overload detected", 1640995245),
    MetricEvent("cpu.usage", 85.3, 1640995250),
    TraceEvent("trace-002", "span-003", 1640995255),
    MetricEvent("memory.usage", 82.7, 1640995260),
    LogEvent("info", "Database connection restored", 1640995265),
    MetricEvent("cpu.usage", 72.4, 1640995270)
  ]
  
  // 窗口化函数
  let window_events = fn(events: Array[StreamEvent], window_size: Int, slide_size: Int) {
    let mut windows = []
    
    if events.length() == 0 {
      return windows
    }
    
    let first_timestamp = match events[0] {
      MetricEvent(_, _, ts) => ts
      LogEvent(_, _, ts) => ts
      TraceEvent(_, _, ts) => ts
      AlertEvent(_, _, ts) => ts
    }
    
    let mut window_start = first_timestamp
    
    while window_start <= match events[events.length() - 1] {
      MetricEvent(_, _, ts) => ts
      LogEvent(_, _, ts) => ts
      TraceEvent(_, _, ts) => ts
      AlertEvent(_, _, ts) => ts
    } {
      let window_end = window_start + window_size
      let window_events = events.filter(fn(event) {
        let timestamp = match event {
          MetricEvent(_, _, ts) => ts
          LogEvent(_, _, ts) => ts
          TraceEvent(_, _, ts) => ts
          AlertEvent(_, _, ts) => ts
        }
        timestamp >= window_start and timestamp < window_end
      })
      
      windows = windows + [{
        events: window_events,
        start_time: window_start,
        end_time: window_end,
        size: window_events.length()
      }]
      
      window_start = window_start + slide_size
    }
    
    windows
  }
  
  // 创建30秒窗口，15秒滑动
  let windows = window_events(stream_events, 30, 15)
  assert_eq(windows.length(), 3)
  
  // 验证第一个窗口
  let first_window = windows[0]
  assert_eq(first_window.start_time, 1640995200)
  assert_eq(first_window.end_time, 1640995230)
  assert_eq(first_window.size, 5)
  
  // 验证第二个窗口
  let second_window = windows[1]
  assert_eq(second_window.start_time, 1640995215)
  assert_eq(second_window.end_time, 1640995245)
  assert_eq(second_window.size, 7)
  
  // 窗口聚合函数
  let aggregate_window = fn(window: ProcessingWindow) {
    let mut metric_count = 0
    let mut log_count = 0
    let mut trace_count = 0
    let mut alert_count = 0
    
    let mut cpu_values = []
    let mut memory_values = []
    let mut error_logs = []
    let mut critical_alerts = []
    
    for event in window.events {
      match event {
        MetricEvent(name, value, _) => {
          metric_count = metric_count + 1
          if name == "cpu.usage" {
            cpu_values = cpu_values + [value]
          } else if name == "memory.usage" {
            memory_values = memory_values + [value]
          }
        }
        LogEvent(level, message, _) => {
          log_count = log_count + 1
          if level == "error" {
            error_logs = error_logs + [message]
          }
        }
        TraceEvent(_, _, _) => {
          trace_count = trace_count + 1
        }
        AlertEvent(severity, message, _) => {
          alert_count = alert_count + 1
          if severity == "critical" {
            critical_alerts = critical_alerts + [message]
          }
        }
      }
    }
    
    let avg_cpu = if cpu_values.length() > 0 {
      cpu_values.reduce(fn(acc, v) { acc + v }, 0.0) / cpu_values.length().to_float()
    } else {
      0.0
    }
    
    let avg_memory = if memory_values.length() > 0 {
      memory_values.reduce(fn(acc, v) { acc + v }, 0.0) / memory_values.length().to_float()
    } else {
      0.0
    }
    
    {
      window_start: window.start_time,
      window_end: window.end_time,
      event_counts: {
        metrics: metric_count,
        logs: log_count,
        traces: trace_count,
        alerts: alert_count
      },
      aggregations: {
        avg_cpu: avg_cpu,
        avg_memory: avg_memory,
        error_count: error_logs.length(),
        critical_alert_count: critical_alerts.length()
      }
    }
  }
  
  // 聚合窗口数据
  let window_aggregations = windows.map(aggregate_window)
  assert_eq(window_aggregations.length(), 3)
  
  // 验证第一个窗口聚合
  let first_agg = window_aggregations[0]
  assert_eq(first_agg.event_counts.metrics, 3)
  assert_eq(first_agg.event_counts.logs, 2)
  assert_eq(first_agg.event_counts.traces, 0)
  assert_eq(first_agg.event_counts.alerts, 0)
  assert_eq(first_agg.aggregations.avg_cpu, 67.8)  // (65.5 + 70.1) / 2
  
  // 验证第二个窗口聚合
  let second_agg = window_aggregations[1]
  assert_eq(second_agg.event_counts.metrics, 3)
  assert_eq(second_agg.event_counts.logs, 2)
  assert_eq(second_agg.event_counts.traces, 2)
  assert_eq(second_agg.event_counts.alerts, 0)
  assert_eq(second_agg.aggregations.avg_cpu, 85.3)  // 只有一个值
  assert_eq(second_agg.aggregations.avg_memory, 78.95)  // (75.2 + 82.7) / 2
  
  // 验证第三个窗口聚合
  let third_agg = window_aggregations[2]
  assert_eq(third_agg.event_counts.alerts, 1)
  assert_eq(third_agg.aggregations.critical_alert_count, 1)
  
  // 实时模式检测
  let detect_patterns = fn(window: ProcessingWindow) {
    let mut patterns = []
    
    let aggregation = aggregate_window(window)
    
    // 检测CPU使用率过高
    if aggregation.aggregations.avg_cpu > 80.0 {
      patterns = patterns + "High CPU usage detected: " + aggregation.aggregations.avg_cpu.to_string() + "%"
    }
    
    // 检测内存使用率过高
    if aggregation.aggregations.avg_memory > 80.0 {
      patterns = patterns + "High memory usage detected: " + aggregation.aggregations.avg_memory.to_string() + "%"
    }
    
    // 检测错误日志
    if aggregation.aggregations.error_count > 0 {
      patterns = patterns + "Error logs detected: " + aggregation.aggregations.error_count.to_string() + " errors"
    }
    
    // 检测关键告警
    if aggregation.aggregations.critical_alert_count > 0 {
      patterns = patterns + "Critical alerts detected: " + aggregation.aggregations.critical_alert_count.to_string() + " alerts"
    }
    
    // 检测事件激增
    let total_events = aggregation.event_counts.metrics + aggregation.event_counts.logs + 
                     aggregation.event_counts.traces + aggregation.event_counts.alerts
    if total_events > 5 {
      patterns = patterns + "Event surge detected: " + total_events.to_string() + " events in window"
    }
    
    patterns
  }
  
  // 检测各窗口中的模式
  let first_patterns = detect_patterns(first_window)
  let second_patterns = detect_patterns(second_window)
  let third_patterns = detect_patterns(third_window)
  
  assert_eq(first_patterns.length(), 0)  // 第一个窗口无异常模式
  
  assert_eq(second_patterns.length(), 2)  // 第二个窗口有高CPU使用和事件激增
  assert_true(second_patterns.any(fn(p) { p.contains("High CPU usage") }))
  assert_true(second_patterns.any(fn(p) { p.contains("Event surge") }))
  
  assert_eq(third_patterns.length(), 3)  // 第三个窗口有关键告警、错误日志和事件激增
  assert_true(third_patterns.any(fn(p) { p.contains("Critical alerts") }))
  assert_true(third_patterns.any(fn(p) { p.contains("Error logs") }))
  assert_true(third_patterns.any(fn(p) { p.contains("Event surge") }))
  
  // 动态阈值调整
  let adjust_thresholds = fn(historical_aggregations: Array[Map[String, Float]]) {
    if historical_aggregations.length() == 0 {
      return {
        cpu_threshold: 80.0,
        memory_threshold: 80.0,
        event_surge_threshold: 5
      }
    }
    
    let mut cpu_values = []
    let mut memory_values = []
    let mut event_counts = []
    
    for agg in historical_aggregations {
      match agg.get("avg_cpu") {
        Some(value) => cpu_values = cpu_values + [value]
        None => ()
      }
      match agg.get("avg_memory") {
        Some(value) => memory_values = memory_values + [value]
        None => ()
      }
      match agg.get("total_events") {
        Some(value) => event_counts = event_counts + [value]
        None => ()
      }
    }
    
    let cpu_avg = if cpu_values.length() > 0 {
      cpu_values.reduce(fn(acc, v) { acc + v }, 0.0) / cpu_values.length().to_float()
    } else {
      50.0
    }
    
    let memory_avg = if memory_values.length() > 0 {
      memory_values.reduce(fn(acc, v) { acc + v }, 0.0) / memory_values.length().to_float()
    } else {
      50.0
    }
    
    let events_avg = if event_counts.length() > 0 {
      event_counts.reduce(fn(acc, v) { acc + v }, 0.0) / event_counts.length().to_float()
    } else {
      3.0
    }
    
    {
      cpu_threshold: cpu_avg * 1.5,  // 历史平均值的1.5倍
      memory_threshold: memory_avg * 1.5,
      event_surge_threshold: (events_avg * 2.0).to_int()
    }
  }
  
  // 基于历史数据调整阈值
  let historical_data = window_aggregations.map(fn(agg) {
    {
      "avg_cpu": agg.aggregations.avg_cpu,
      "avg_memory": agg.aggregations.avg_memory,
      "total_events": (agg.event_counts.metrics + agg.event_counts.logs + 
                       agg.event_counts.traces + agg.event_counts.alerts).to_float()
    }
  })
  
  let adjusted_thresholds = adjust_thresholds(historical_data)
  assert_true(adjusted_thresholds.cpu_threshold > 0.0)
  assert_true(adjusted_thresholds.memory_threshold > 0.0)
  assert_true(adjusted_thresholds.event_surge_threshold > 0)
}

// 测试8: 错误恢复机制
test "error recovery mechanisms" {
  // 定义错误类型
  type ErrorType = 
    | NetworkError(String)
    | DatabaseError(String)
    | TimeoutError(Int)
    | ResourceExhaustedError(String)
    | ConfigurationError(String)
  
  type RecoveryStrategy = 
    | Retry(Int)                    // 重试次数
    | Fallback(String)              // 回退方案
    | CircuitBreaker(Int, Int)      // 失败阈值, 恢复超时
    | DegradedService(String)       // 降级服务
    | GracefulShutdown              // 优雅关闭
  
  type ErrorContext = {
    operation: String,
    component: String,
    timestamp: Int,
    error: ErrorType,
    metadata: Map[String, String]
  }
  
  type RecoveryResult = {
    success: Bool,
    strategy: RecoveryStrategy,
    attempts: Int,
    duration: Int,
    message: String
  }
  
  // 模拟错误上下文
  let error_contexts = [
    {
      operation: "database_query",
      component: "payment_service",
      timestamp: 1640995200,
      error: DatabaseError("Connection timeout"),
      metadata: { "query": "SELECT * FROM orders", "retry_count": "0" }
    },
    {
      operation: "http_request",
      component: "notification_service",
      timestamp: 1640995210,
      error: NetworkError("Connection refused"),
      metadata: { "url": "http://notification:8080/send", "retry_count": "1" }
    },
    {
      operation: "file_processing",
      component: "data_processor",
      timestamp: 1640995220,
      error: ResourceExhaustedError("Out of memory"),
      metadata: { "file_size": "1GB", "available_memory": "512MB" }
    },
    {
      operation: "config_load",
      component: "auth_service",
      timestamp: 1640995230,
      error: ConfigurationError("Missing required field"),
      metadata: { "config_file": "/etc/auth/config.yaml", "missing_field": "database_url" }
    },
    {
      operation: "api_call",
      component: "external_client",
      timestamp: 1640995240,
      error: TimeoutError(5000),
      metadata: { "endpoint": "/api/v1/data", "timeout_ms": "5000" }
    }
  ]
  
  // 错误分类器
  let classify_error = fn(error: ErrorType) {
    match error {
      NetworkError(_) => "transient"      // 网络错误可能是暂时的
      DatabaseError(msg) => {
        if msg.contains("timeout") or msg.contains("connection") {
          "transient"
        } else {
          "permanent"
        }
      }
      TimeoutError(_) => "transient"      // 超时可能是暂时的
      ResourceExhaustedError(_) => "resource"  // 资源耗尽
      ConfigurationError(_) => "permanent"     // 配置错误通常是永久的
    }
  }
  
  // 测试错误分类
  for context in error_contexts {
    let classification = classify_error(context.error)
    match context.error {
      DatabaseError(msg) => assert_eq(classification, "transient")
      NetworkError(_) => assert_eq(classification, "transient")
      ResourceExhaustedError(_) => assert_eq(classification, "resource")
      ConfigurationError(_) => assert_eq(classification, "permanent")
      TimeoutError(_) => assert_eq(classification, "transient")
    }
  }
  
  // 恢复策略选择器
  let select_recovery_strategy = fn(error_type: String, component: String, retry_count: Int) {
    match error_type {
      "transient" => {
        if retry_count < 3 {
          Retry(3 - retry_count)
        } else {
          Fallback("circuit_breaker")
        }
      }
      "resource" => {
        if retry_count < 2 {
          Retry(2 - retry_count)
        } else {
          DegradedService("limited_functionality")
        }
      }
      "permanent" => {
        Fallback("default_behavior")
      }
      _ => {
        Retry(1)  // 默认重试一次
      }
    }
  }
  
  // 测试恢复策略选择
  let db_strategy = select_recovery_strategy("transient", "payment_service", 0)
  match db_strategy {
    Retry(attempts) => assert_eq(attempts, 3)
    _ => assert_true(false)
  }
  
  let config_strategy = select_recovery_strategy("permanent", "auth_service", 0)
  match config_strategy {
    Fallback(plan) => assert_eq(plan, "default_behavior")
    _ => assert_true(false)
  }
  
  let resource_strategy = select_recovery_strategy("resource", "data_processor", 2)
  match resource_strategy {
    DegradedService(mode) => assert_eq(mode, "limited_functionality")
    _ => assert_true(false)
  }
  
  // 模拟恢复执行
  let execute_recovery = fn(context: ErrorContext, strategy: RecoveryStrategy) {
    let start_time = context.timestamp
    let mut attempts = 0
    let mut success = false
    let mut message = ""
    
    match strategy {
      Retry(max_attempts) => {
        attempts = max_attempts
        // 模拟重试逻辑
        if max_attempts > 0 and context.component != "data_processor" {
          success = true
          message = "Operation succeeded after " + max_attempts.to_string() + " retries"
        } else {
          message = "Operation failed after " + max_attempts.to_string() + " retries"
        }
      }
      Fallback(plan) => {
        attempts = 1
        success = true  // 假设回退总是成功
        message = "Operation succeeded using fallback: " + plan
      }
      CircuitBreaker(threshold, timeout) => {
        attempts = 1
        success = false  // 熔断器打开，操作失败
        message = "Circuit breaker is open, operation blocked"
      }
      DegradedService(mode) => {
        attempts = 1
        success = true  // 降级服务提供有限功能
        message = "Operation succeeded in degraded mode: " + mode
      }
      GracefulShutdown => {
        attempts = 1
        success = true
        message = "System shutting down gracefully"
      }
    }
    
    let duration = 100 + attempts * 50  // 模拟恢复时间
    
    {
      success: success,
      strategy: strategy,
      attempts: attempts,
      duration: duration,
      message: message
    }
  }
  
  // 测试恢复执行
  let db_context = error_contexts[0]
  let db_strategy = select_recovery_strategy(classify_error(db_context.error), db_context.component, 0)
  let db_recovery = execute_recovery(db_context, db_strategy)
  
  assert_true(db_recovery.success)
  assert_eq(db_recovery.attempts, 3)
  assert_eq(db_recovery.duration, 250)  // 100 + 3 * 50
  
  let config_context = error_contexts[3]
  let config_strategy = select_recovery_strategy(classify_error(config_context.error), config_context.component, 0)
  let config_recovery = execute_recovery(config_context, config_strategy)
  
  assert_true(config_recovery.success)
  match config_recovery.strategy {
    Fallback(plan) => assert_eq(plan, "default_behavior")
    _ => assert_true(false)
  }
  
  // 错误恢复统计
  let calculate_recovery_stats = fn(results: Array[RecoveryResult]) {
    let total = results.length()
    let successful = results.filter(fn(r) { r.success }).length()
    let failed = total - successful
    
    let mut strategy_counts = {}
    let mut total_attempts = 0
    let mut total_duration = 0
    
    for result in results {
      total_attempts = total_attempts + result.attempts
      total_duration = total_duration + result.duration
      
      let strategy_name = match result.strategy {
        Retry(_) => "retry"
        Fallback(_) => "fallback"
        CircuitBreaker(_, _) => "circuit_breaker"
        DegradedService(_) => "degraded_service"
        GracefulShutdown => "graceful_shutdown"
      }
      
      match strategy_counts.get(strategy_name) {
        Some(count) => {
          strategy_counts = strategy_counts.set(strategy_name, count + 1)
        }
        None => {
          strategy_counts = strategy_counts.set(strategy_name, 1)
        }
      }
    }
    
    {
      total_operations: total,
      successful_operations: successful,
      failed_operations: failed,
      success_rate: successful.to_float() / total.to_float(),
      average_attempts: total_attempts.to_float() / total.to_float(),
      average_duration: total_duration.to_float() / total.to_float(),
      strategy_distribution: strategy_counts
    }
  }
  
  // 测试所有错误上下文的恢复
  let recovery_results = []
  for context in error_contexts {
    let error_type = classify_error(context.error)
    let retry_count = match context.metadata.get("retry_count") {
      Some(count_str) => count_str.to_int()
      None => 0
    }
    let strategy = select_recovery_strategy(error_type, context.component, retry_count)
    let result = execute_recovery(context, strategy)
    recovery_results = recovery_results + [result]
  }
  
  let recovery_stats = calculate_recovery_stats(recovery_results)
  assert_eq(recovery_stats.total_operations, 5)
  assert_eq(recovery_stats.successful_operations, 4)
  assert_eq(recovery_stats.failed_operations, 1)
  assert_eq(recovery_stats.success_rate, 0.8)
  
  // 验证策略分布
  assert_eq(recovery_stats.strategy_distribution.get("retry"), Some(3))
  assert_eq(recovery_stats.strategy_distribution.get("fallback"), Some(2))
  
  // 自适应恢复策略
  let adaptive_recovery = fn(context: ErrorContext, historical_results: Array[RecoveryResult]) {
    let error_type = classify_error(context.error)
    
    // 分析历史成功率
    let relevant_results = historical_results.filter(fn(result) {
      // 简化：假设所有结果都是相关的
      true
    })
    
    if relevant_results.length() == 0 {
      return select_recovery_strategy(error_type, context.component, 0)
    }
    
    let success_rate = relevant_results.filter(fn(r) { r.success }).length().to_float() / 
                      relevant_results.length().to_float()
    
    // 根据历史成功率调整策略
    if success_rate > 0.8 {
      // 历史成功率高，使用标准策略
      select_recovery_strategy(error_type, context.component, 0)
    } else if success_rate > 0.5 {
      // 中等成功率，更保守的策略
      match error_type {
        "transient" => Fallback("alternative_service")
        "resource" => DegradedService("minimal_functionality")
        _ => Fallback("safe_default")
      }
    } else {
      // 成功率低，直接使用回退
      Fallback("immediate_fallback")
    }
  }
  
  // 测试自适应恢复
  let adaptive_strategy = adaptive_recovery(error_contexts[0], recovery_results)
  match adaptive_strategy {
    Retry(attempts) => assert_eq(attempts, 3)
    _ => assert_true(false)
  }
  
  let adaptive_strategy2 = adaptive_recovery(error_contexts[2], recovery_results)
  match adaptive_strategy2 {
    DegradedService(mode) => assert_eq(mode, "limited_functionality")
    _ => assert_true(false)
  }
}

// 测试9: 数据完整性验证
test "data integrity validation" {
  // 定义数据完整性规则
  type IntegrityRule = {
    name: String,
    description: String,
    validator: (Map[String, String]) -> Bool,
    severity: String  // "error", "warning", "info"
  }
  
  type ValidationResult = {
    valid: Bool,
    violations: Array[String],
    warnings: Array[String],
    info: Array[String]
  }
  
  // 定义测试数据记录
  type DataRecord = {
    id: String,
    timestamp: Int,
    data_type: String,
    fields: Map[String, String],
    checksum: Option[String]
  }
  
  // 创建完整性规则
  let integrity_rules = [
    {
      name: "required_fields",
      description: "All required fields must be present",
      validator: fn(fields: Map[String, String]) {
        let required_fields = ["id", "timestamp", "data_type"]
        for field in required_fields {
          if not(fields.contains(field)) {
            return false
          }
        }
        true
      },
      severity: "error"
    },
    {
      name: "timestamp_range",
      description: "Timestamp must be within reasonable range",
      validator: fn(fields: Map[String, String]) {
        match fields.get("timestamp") {
          Some(ts_str) => {
            let ts = ts_str.to_int()
            ts >= 1600000000 and ts <= 1700000000  // 2020-2023年范围
          }
          None => false
        }
      },
      severity: "error"
    },
    {
      name: "id_format",
      description: "ID must follow expected format",
      validator: fn(fields: Map[String, String]) {
        match fields.get("id") {
          Some(id) => {
            id.length() > 0 and (id.contains("-") or id.contains("_"))
          }
          None => false
        }
      },
      severity: "warning"
    },
    {
      name: "data_type_known",
      description: "Data type should be a known type",
      validator: fn(fields: Map[String, String]) {
        match fields.get("data_type") {
          Some(data_type) => {
            let known_types = ["metric", "log", "trace", "span", "event"]
            known_types.contains(data_type)
          }
          None => false
        }
      },
      severity: "warning"
    },
    {
      name: "field_count_reasonable",
      description: "Number of fields should be reasonable",
      validator: fn(fields: Map[String, String]) {
        fields.size() >= 3 and fields.size() <= 50
      },
      severity: "info"
    }
  ]
  
  // 创建测试数据记录
  let data_records = [
    // 有效记录
    {
      id: "record-001",
      timestamp: 1640995200,
      data_type: "metric",
      fields: {
        "id": "record-001",
        "timestamp": "1640995200",
        "data_type": "metric",
        "name": "cpu.usage",
        "value": "65.5",
        "unit": "percent"
      },
      checksum: Some("abc123")
    },
    // 缺少必需字段
    {
      id: "record-002",
      timestamp: 1640995300,
      data_type: "log",
      fields: {
        "id": "record-002",
        "data_type": "log",
        "level": "error",
        "message": "Database connection failed"
        // 缺少timestamp字段
      },
      checksum: Some("def456")
    },
    // 时间戳超出范围
    {
      id: "record-003",
      timestamp: 1800000000,  // 超出2023年范围
      data_type: "trace",
      fields: {
        "id": "record-003",
        "timestamp": "1800000000",
        "data_type": "trace",
        "trace_id": "trace-123",
        "span_id": "span-456"
      },
      checksum: Some("ghi789")
    },
    // ID格式不正确
    {
      id: "invalidid",  // 不包含-或_
      timestamp: 1640995400,
      data_type: "span",
      fields: {
        "id": "invalidid",
        "timestamp": "1640995400",
        "data_type": "span",
        "parent_id": "span-123"
      },
      checksum: Some("jkl012")
    },
    // 未知数据类型
    {
      id: "record-005",
      timestamp: 1640995500,
      data_type: "unknown_type",
      fields: {
        "id": "record-005",
        "timestamp": "1640995500",
        "data_type": "unknown_type",
        "custom_field": "custom_value"
      },
      checksum: Some("mno345")
    }
  ]
  
  // 数据验证函数
  let validate_data = fn(record: DataRecord, rules: Array[IntegrityRule]) {
    let mut violations = []
    let mut warnings = []
    let mut info = []
    
    for rule in rules {
      let is_valid = rule.validator(record.fields)
      
      if not(is_valid) {
        match rule.severity {
          "error" => violations = violations + rule.name + ": " + rule.description
          "warning" => warnings = warnings + rule.name + ": " + rule.description
          "info" => info = info + rule.name + ": " + rule.description
          _ => ()
        }
      }
    }
    
    {
      valid: violations.length() == 0,
      violations: violations,
      warnings: warnings,
      info: info
    }
  }
  
  // 验证所有记录
  let validation_results = []
  for record in data_records {
    let result = validate_data(record, integrity_rules)
    validation_results = validation_results + [result]
  }
  
  assert_eq(validation_results.length(), 5)
  
  // 验证第一个记录（有效）
  assert_true(validation_results[0].valid)
  assert_eq(validation_results[0].violations.length(), 0)
  assert_eq(validation_results[0].warnings.length(), 0)
  
  // 验证第二个记录（缺少必需字段）
  assert_false(validation_results[1].valid)
  assert_eq(validation_results[1].violations.length(), 2)  // required_fields 和 timestamp_range
  assert_true(validation_results[1].violations.any(fn(v) { v.contains("required_fields") }))
  assert_true(validation_results[1].violations.any(fn(v) { v.contains("timestamp_range") }))
  
  // 验证第三个记录（时间戳超出范围）
  assert_false(validation_results[2].valid)
  assert_eq(validation_results[2].violations.length(), 1)
  assert_true(validation_results[2].violations.any(fn(v) { v.contains("timestamp_range") }))
  
  // 验证第四个记录（ID格式不正确）
  assert_false(validation_results[3].valid)
  assert_eq(validation_results[3].violations.length(), 0)
  assert_eq(validation_results[3].warnings.length(), 1)
  assert_true(validation_results[3].warnings.any(fn(w) { w.contains("id_format") }))
  
  // 验证第五个记录（未知数据类型）
  assert_false(validation_results[4].valid)
  assert_eq(validation_results[4].violations.length(), 0)
  assert_eq(validation_results[4].warnings.length(), 1)
  assert_true(validation_results[4].warnings.any(fn(w) { w.contains("data_type_known") }))
  
  // 校验和验证
  let calculate_checksum = fn(record: DataRecord) {
    let mut combined = ""
    for (key, value) in record.fields {
      combined = combined + key + ":" + value + ","
    }
    // 简化的校验和计算（实际应用中应使用更强的哈希算法）
    let hash = combined.length() % 10000
    hash.to_string()
  }
  
  let verify_checksum = fn(record: DataRecord) {
    match record.checksum {
      Some(stored_checksum) => {
        let calculated_checksum = calculate_checksum(record)
        stored_checksum == calculated_checksum
      }
      None => true  // 没有校验和时跳过验证
    }
  }
  
  // 测试校验和验证
  let first_record_checksum_valid = verify_checksum(data_records[0])
  assert_false(first_record_checksum_valid)  // 预期不匹配，因为我们的校验和是模拟的
  
  // 创建匹配的校验和
  let record_with_valid_checksum = { data_records[0] | 
    checksum = Some(calculate_checksum(data_records[0])) 
  }
  let valid_checksum = verify_checksum(record_with_valid_checksum)
  assert_true(valid_checksum)
  
  // 数据修复函数
  let repair_data = fn(record: DataRecord, validation_result: ValidationResult) {
    let mut repaired_fields = record.fields
    
    // 修复缺少的必需字段
    for violation in validation_result.violations {
      if violation.contains("required_fields") {
        if not(repaired_fields.contains("timestamp")) {
          repaired_fields = repaired_fields.set("timestamp", "1640995600")
        }
        if not(repaired_fields.contains("data_type")) {
          repaired_fields = repaired_fields.set("data_type", "unknown")
        }
      }
      
      if violation.contains("timestamp_range") {
        match repaired_fields.get("timestamp") {
          Some(ts_str) => {
            let ts = ts_str.to_int()
            if ts > 1700000000 {
              repaired_fields = repaired_fields.set("timestamp", "1640995600")
            }
          }
          None => ()
        }
      }
    }
    
    // 修复警告
    for warning in validation_result.warnings {
      if warning.contains("id_format") {
        match repaired_fields.get("id") {
          Some(id) => {
            if not(id.contains("-")) and not(id.contains("_")) {
              repaired_fields = repaired_fields.set("id", id + "-fixed")
            }
          }
          None => ()
        }
      }
      
      if warning.contains("data_type_known") {
        match repaired_fields.get("data_type") {
          Some(data_type) => {
            let known_types = ["metric", "log", "trace", "span", "event"]
            if not(known_types.contains(data_type)) {
              repaired_fields = repaired_fields.set("data_type", "event")
            }
          }
          None => ()
        }
      }
    }
    
    // 重新计算校验和
    let repaired_record = {
      id: match repaired_fields.get("id") {
        Some(id) => id
        None => "unknown"
      },
      timestamp: match repaired_fields.get("timestamp") {
        Some(ts_str) => ts_str.to_int()
        None => 1640995600
      },
      data_type: match repaired_fields.get("data_type") {
        Some(dt) => dt
        None => "unknown"
      },
      fields: repaired_fields,
      checksum: Some(calculate_checksum({ record | fields = repaired_fields }))
    }
    
    repaired_record
  }
  
  // 测试数据修复
  let invalid_record = data_records[1]  // 缺少时间戳
  let invalid_validation = validation_results[1]
  let repaired_record = repair_data(invalid_record, invalid_validation)
  
  // 验证修复后的记录
  let repaired_validation = validate_data(repaired_record, integrity_rules)
  assert_true(repaired_validation.valid)  // 修复后应该有效
  assert_true(repaired_record.fields.contains("timestamp"))
  assert_eq(repaired_record.fields.get("timestamp"), Some("1640995600"))
  
  // 批量数据验证
  let batch_validate = fn(records: Array[DataRecord], rules: Array[IntegrityRule]) {
    let mut valid_count = 0
    let mut invalid_count = 0
    let mut total_violations = 0
    let mut total_warnings = 0
    let mut violation_types = {}
    let mut warning_types = {}
    
    let results = []
    
    for record in records {
      let result = validate_data(record, rules)
      results = results + [result]
      
      if result.valid {
        valid_count = valid_count + 1
      } else {
        invalid_count = invalid_count + 1
      }
      
      total_violations = total_violations + result.violations.length()
      total_warnings = total_warnings + result.warnings.length()
      
      // 统计违规类型
      for violation in result.violations {
        let violation_type = violation.split(":")[0]
        match violation_types.get(violation_type) {
          Some(count) => {
            violation_types = violation_types.set(violation_type, count + 1)
          }
          None => {
            violation_types = violation_types.set(violation_type, 1)
          }
        }
      }
      
      // 统计警告类型
      for warning in result.warnings {
        let warning_type = warning.split(":")[0]
        match warning_types.get(warning_type) {
          Some(count) => {
            warning_types = warning_types.set(warning_type, count + 1)
          }
          None => {
            warning_types = warning_types.set(warning_type, 1)
          }
        }
      }
    }
    
    {
      total_records: records.length(),
      valid_records: valid_count,
      invalid_records: invalid_count,
      validity_rate: valid_count.to_float() / records.length().to_float(),
      total_violations: total_violations,
      total_warnings: total_warnings,
      violation_distribution: violation_types,
      warning_distribution: warning_types,
      detailed_results: results
    }
  }
  
  // 测试批量验证
  let batch_result = batch_validate(data_records, integrity_rules)
  assert_eq(batch_result.total_records, 5)
  assert_eq(batch_result.valid_records, 1)
  assert_eq(batch_result.invalid_records, 4)
  assert_eq(batch_result.validity_rate, 0.2)
  assert_eq(batch_result.total_violations, 3)
  assert_eq(batch_result.total_warnings, 3)
  
  // 验证违规分布
  assert_eq(batch_result.violation_distribution.get("required_fields"), Some(1))
  assert_eq(batch_result.violation_distribution.get("timestamp_range"), Some(2))
  
  // 验证警告分布
  assert_eq(batch_result.warning_distribution.get("id_format"), Some(1))
  assert_eq(batch_result.warning_distribution.get("data_type_known"), Some(1))
}

// 测试10: 跨平台兼容性
test "cross-platform compatibility" {
  // 定义平台类型
  type Platform = 
    | Linux
    | Windows
    | MacOS
    | Docker
    | Kubernetes
  
  type PlatformFeature = {
    name: String,
    supported_platforms: Array[Platform],
    implementation: Map[Platform, String],
    fallback: Option[String]
  }
  
  type CompatibilityTest = {
    feature_name: String,
    platform: Platform,
    test_result: Bool,
    execution_time: Int,
    error_message: Option[String]
  }
  
  // 定义平台特性
  let platform_features = [
    {
      name: "file_system_operations",
      supported_platforms: [Linux, Windows, MacOS, Docker],
      implementation: {
        Linux: "native_linux_fs",
        Windows: "windows_fs_api",
        MacOS: "macos_fs_framework",
        Docker: "container_fs_layer",
        Kubernetes: "persistent_volume"
      },
      fallback: Some("in_memory_storage")
    },
    {
      name: "network_telemetry",
      supported_platforms: [Linux, Windows, MacOS, Docker, Kubernetes],
      implementation: {
        Linux: "linux_netlink",
        Windows: "winsock_api",
        MacOS: "macos_network_framework",
        Docker: "container_network",
        Kubernetes: "cni_plugin"
      },
      fallback: Some("http_telemetry")
    },
    {
      name: "process_monitoring",
      supported_platforms: [Linux, Windows, MacOS],
      implementation: {
        Linux: "proc_filesystem",
        Windows: "wmi_api",
        MacOS: "activity_monitor_api",
        Docker: "container_stats",
        Kubernetes: "cgroup_metrics"
      },
      fallback: Some("self_monitoring")
    },
    {
      name: "container_orchestration",
      supported_platforms: [Docker, Kubernetes],
      implementation: {
        Linux: "docker_api",
        Windows: "windows_containers",
        MacOS: "docker_desktop",
        Docker: "docker_engine_api",
        Kubernetes: "kubernetes_api"
      },
      fallback: None
    }
  ]
  
  // 检查平台特性支持
  let is_platform_supported = fn(feature: PlatformFeature, platform: Platform) {
    feature.supported_platforms.any(fn(p) { 
      match (p, platform) {
        (Linux, Linux) => true
        (Windows, Windows) => true
        (MacOS, MacOS) => true
        (Docker, Docker) => true
        (Kubernetes, Kubernetes) => true
        (_, _) => false
      }
    })
  }
  
  // 获取平台特定实现
  let get_platform_implementation = fn(feature: PlatformFeature, platform: Platform) {
    match feature.implementation.get(platform) {
      Some(impl) => Some(impl)
      None => {
        if is_platform_supported(feature, platform) {
          // 支持但没有特定实现，使用通用实现
          Some("generic_implementation")
        } else {
          feature.fallback
        }
      }
    }
  }
  
  // 测试特性支持检查
  let fs_feature = platform_features[0]
  assert_true(is_platform_supported(fs_feature, Linux))
  assert_true(is_platform_supported(fs_feature, Windows))
  assert_true(is_platform_supported(fs_feature, MacOS))
  assert_false(is_platform_supported(fs_feature, Kubernetes))  // 不在支持列表中
  
  let container_feature = platform_features[3]
  assert_false(is_platform_supported(container_feature, Linux))  // 不在支持列表中
  assert_true(is_platform_supported(container_feature, Docker))
  assert_true(is_platform_supported(container_feature, Kubernetes))
  
  // 测试实现获取
  let linux_fs_impl = get_platform_implementation(fs_feature, Linux)
  assert_eq(linux_fs_impl, Some("native_linux_fs"))
  
  let k8s_fs_impl = get_platform_implementation(fs_feature, Kubernetes)
  assert_eq(k8s_fs_impl, Some("in_memory_storage"))  // 使用回退
  
  let windows_container_impl = get_platform_implementation(container_feature, Windows)
  assert_eq(windows_container_impl, Some("windows_containers"))
  
  let linux_container_impl = get_platform_implementation(container_feature, Linux)
  assert_eq(linux_container_impl, None)  // 不支持且无回退
  
  // 模拟兼容性测试
  let run_compatibility_test = fn(feature: PlatformFeature, platform: Platform) {
    let implementation = get_platform_implementation(feature, platform)
    
    match implementation {
      Some(impl) => {
        // 模拟测试执行
        let test_result = if impl.contains("error") {
          false
        } else if impl.contains("slow") {
          true  // 成功但慢
        } else {
          true  // 正常成功
        }
        
        let execution_time = if impl.contains("slow") {
          5000  // 5秒
        } else {
          100   // 100ms
        }
        
        let error_message = if test_result {
          None
        } else {
          Some("Implementation failed: " + impl)
        }
        
        {
          feature_name: feature.name,
          platform: platform,
          test_result: test_result,
          execution_time: execution_time,
          error_message: error_message
        }
      }
      None => {
        {
          feature_name: feature.name,
          platform: platform,
          test_result: false,
          execution_time: 0,
          error_message: Some("No implementation available for platform")
        }
      }
    }
  }
  
  // 运行跨平台测试矩阵
  let platforms = [Linux, Windows, MacOS, Docker, Kubernetes]
  let test_results = []
  
  for feature in platform_features {
    for platform in platforms {
      let result = run_compatibility_test(feature, platform)
      test_results = test_results + [result]
    }
  }
  
  assert_eq(test_results.length(), 20)  // 4个特性 × 5个平台
  
  // 分析测试结果
  let analyze_compatibility = fn(results: Array[CompatibilityTest]) {
    let mut total_tests = 0
    let mut passed_tests = 0
    let mut failed_tests = 0
    let mut total_execution_time = 0
    
    let mut platform_stats = {}
    let mut feature_stats = {}
    
    for result in results {
      total_tests = total_tests + 1
      total_execution_time = total_execution_time + result.execution_time
      
      if result.test_result {
        passed_tests = passed_tests + 1
      } else {
        failed_tests = failed_tests + 1
      }
      
      // 按平台统计
      let platform_name = match result.platform {
        Linux => "Linux"
        Windows => "Windows"
        MacOS => "MacOS"
        Docker => "Docker"
        Kubernetes => "Kubernetes"
      }
      
      match platform_stats.get(platform_name) {
        Some((passed, failed, time)) => {
          let new_passed = if result.test_result { passed + 1 } else { passed }
          let new_failed = if result.test_result { failed } else { failed + 1 }
          platform_stats = platform_stats.set(platform_name, (new_passed, new_failed, time + result.execution_time))
        }
        None => {
          let passed = if result.test_result { 1 } else { 0 }
          let failed = if result.test_result { 0 } else { 1 }
          platform_stats = platform_stats.set(platform_name, (passed, failed, result.execution_time))
        }
      }
      
      // 按特性统计
      match feature_stats.get(result.feature_name) {
        Some((passed, failed)) => {
          let new_passed = if result.test_result { passed + 1 } else { passed }
          let new_failed = if result.test_result { failed } else { failed + 1 }
          feature_stats = feature_stats.set(result.feature_name, (new_passed, new_failed))
        }
        None => {
          let passed = if result.test_result { 1 } else { 0 }
          let failed = if result.test_result { 0 } else { 1 }
          feature_stats = feature_stats.set(result.feature_name, (passed, failed))
        }
      }
    }
    
    {
      total_tests: total_tests,
      passed_tests: passed_tests,
      failed_tests: failed_tests,
      success_rate: passed_tests.to_float() / total_tests.to_float(),
      average_execution_time: total_execution_time.to_float() / total_tests.to_float(),
      platform_compatibility: platform_stats,
      feature_reliability: feature_stats
    }
  }
  
  // 分析测试结果
  let compatibility_analysis = analyze_compatibility(test_results)
  assert_eq(compatibility_analysis.total_tests, 20)
  assert_eq(compatibility_analysis.passed_tests, 16)  // 预期16个通过
  assert_eq(compatibility_analysis.failed_tests, 4)   // 预期4个失败
  assert_eq(compatibility_analysis.success_rate, 0.8)
  
  // 验证平台兼容性
  let linux_stats = compatibility_analysis.platform_compatibility.get("Linux")
  match linux_stats {
    Some((passed, failed, _)) => {
      assert_eq(passed + failed, 4)  // Linux上测试了4个特性
      assert_eq(passed, 3)           // 3个通过
      assert_eq(failed, 1)           // 1个失败（容器编排）
    }
    None => assert_true(false)
  }
  
  let docker_stats = compatibility_analysis.platform_compatibility.get("Docker")
  match docker_stats {
    Some((passed, failed, _)) => {
      assert_eq(passed + failed, 4)  // Docker上测试了4个特性
      assert_eq(passed, 4)           // 全部通过
      assert_eq(failed, 0)
    }
    None => assert_true(false)
  }
  
  // 验证特性可靠性
  let fs_reliability = compatibility_analysis.feature_reliability.get("file_system_operations")
  match fs_reliability {
    Some((passed, failed)) => {
      assert_eq(passed + failed, 5)  // 在5个平台上测试
      assert_eq(passed, 5)           // 全部通过
      assert_eq(failed, 0)
    }
    None => assert_true(false)
  }
  
  let container_reliability = compatibility_analysis.feature_reliability.get("container_orchestration")
  match container_reliability {
    Some((passed, failed)) => {
      assert_eq(passed + failed, 5)  // 在5个平台上测试
      assert_eq(passed, 2)           // 2个通过（Docker和Kubernetes）
      assert_eq(failed, 3)           // 3个失败
    }
    None => assert_true(false)
  }
  
  // 平台特性映射
  let create_platform_feature_map = fn(features: Array[PlatformFeature], target_platform: Platform) {
    let mut feature_map = {}
    
    for feature in features {
      let implementation = get_platform_implementation(feature, target_platform)
      match implementation {
        Some(impl) => {
          feature_map = feature_map.set(feature.name, impl)
        }
        None => {
          // 特性不支持
          feature_map = feature_map.set(feature.name, "unsupported")
        }
      }
    }
    
    feature_map
  }
  
  // 创建不同平台的特性映射
  let linux_feature_map = create_platform_feature_map(platform_features, Linux)
  let docker_feature_map = create_platform_feature_map(platform_features, Docker)
  let kubernetes_feature_map = create_platform_feature_map(platform_features, Kubernetes)
  
  // 验证Linux特性映射
  assert_eq(linux_feature_map.get("file_system_operations"), Some("native_linux_fs"))
  assert_eq(linux_feature_map.get("network_telemetry"), Some("linux_netlink"))
  assert_eq(linux_feature_map.get("process_monitoring"), Some("proc_filesystem"))
  assert_eq(linux_feature_map.get("container_orchestration"), Some("unsupported"))
  
  // 验证Docker特性映射
  assert_eq(docker_feature_map.get("file_system_operations"), Some("container_fs_layer"))
  assert_eq(docker_feature_map.get("network_telemetry"), Some("container_network"))
  assert_eq(docker_feature_map.get("process_monitoring"), Some("container_stats"))
  assert_eq(docker_feature_map.get("container_orchestration"), Some("docker_engine_api"))
  
  // 验证Kubernetes特性映射
  assert_eq(kubernetes_feature_map.get("file_system_operations"), Some("in_memory_storage"))
  assert_eq(kubernetes_feature_map.get("network_telemetry"), Some("cni_plugin"))
  assert_eq(kubernetes_feature_map.get("process_monitoring"), Some("cgroup_metrics"))
  assert_eq(kubernetes_feature_map.get("container_orchestration"), Some("kubernetes_api"))
  
  // 跨平台兼容性评分
  let calculate_compatibility_score = fn(platform: Platform, features: Array[PlatformFeature]) {
    let mut supported_features = 0
    let mut total_features = features.length()
    let mut total_score = 0.0
    
    for feature in features {
      if is_platform_supported(feature, platform) {
        supported_features = supported_features + 1
        total_score = total_score + 1.0
      } else if feature.fallback.is_some() {
        supported_features = supported_features + 1
        total_score = total_score + 0.5  // 回退实现得分减半
      }
    }
    
    let support_ratio = supported_features.to_float() / total_features.to_float()
    let implementation_score = total_score / total_features.to_float()
    
    {
      platform: match platform {
        Linux => "Linux"
        Windows => "Windows"
        MacOS => "MacOS"
        Docker => "Docker"
        Kubernetes => "Kubernetes"
      },
      support_ratio: support_ratio,
      implementation_score: implementation_score,
      overall_score: (support_ratio + implementation_score) / 2.0
    }
  }
  
  // 计算各平台兼容性评分
  let linux_score = calculate_compatibility_score(Linux, platform_features)
  let docker_score = calculate_compatibility_score(Docker, platform_features)
  let kubernetes_score = calculate_compatibility_score(Kubernetes, platform_features)
  
  // 验证评分结果
  assert_eq(linux_score.platform, "Linux")
  assert_true(linux_score.support_ratio > 0.5)  // 至少支持一半特性
  assert_true(linux_score.overall_score > 0.5)
  
  assert_eq(docker_score.platform, "Docker")
  assert_eq(docker_score.support_ratio, 1.0)     // Docker支持所有特性
  assert_eq(docker_score.implementation_score, 1.0)  // 全部有原生实现
  assert_eq(docker_score.overall_score, 1.0)
  
  assert_eq(kubernetes_score.platform, "Kubernetes")
  assert_true(kubernetes_score.support_ratio > 0.5)  // 支持大部分特性（通过回退）
  assert_true(kubernetes_score.overall_score > 0.5)
  
  // 验证Docker是最兼容的平台
  assert_true(docker_score.overall_score >= linux_score.overall_score)
  assert_true(docker_score.overall_score >= kubernetes_score.overall_score)
}