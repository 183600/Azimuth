// Azimuth 综合新测试用例
// 包含10个覆盖不同遥测系统功能领域的测试用例

// 测试用例1: 遥测数据异常检测
test "遥测数据异常检测算法" {
  // 定义遥测数据点类型
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建包含异常值的测试数据
  let telemetry_data = [
    { timestamp: 1609459200, metric_name: "cpu_usage", value: 45.2, tags: [("host", "server1")] },
    { timestamp: 1609459260, metric_name: "cpu_usage", value: 47.8, tags: [("host", "server1")] },
    { timestamp: 1609459320, metric_name: "cpu_usage", value: 46.5, tags: [("host", "server1")] },
    { timestamp: 1609459380, metric_name: "cpu_usage", value: 95.3, tags: [("host", "server1")] }, // 异常值
    { timestamp: 1609459440, metric_name: "cpu_usage", value: 48.1, tags: [("host", "server1")] },
    { timestamp: 1609459500, metric_name: "cpu_usage", value: 44.9, tags: [("host", "server1")] }
  ]
  
  // 实现简单的Z-score异常检测算法
  let detect_anomalies = fn(data: Array[TelemetryPoint], threshold: Float) {
    // 计算平均值
    let sum = data.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let mean = sum / (data.length() as Float)
    
    // 计算标准差
    let variance = data.reduce(fn(acc, point) { 
      let diff = point.value - mean
      acc + (diff * diff) 
    }, 0.0) / (data.length() as Float)
    let std_dev = @math.sqrt(variance)
    
    // 检测异常值
    let anomalies = []
    for point in data {
      let z_score = @math.abs(point.value - mean) / std_dev
      if z_score > threshold {
        anomalies = anomalies.push(point)
      }
    }
    anomalies
  }
  
  // 执行异常检测
  let anomalies = detect_anomalies(telemetry_data, 2.0)
  
  // 验证检测结果
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].timestamp, 1609459380)
  assert_eq(anomalies[0].value, 95.3)
}

// 测试用例2: 遥测数据压缩算法
test "遥测数据压缩算法" {
  // 定义时间序列数据点
  type DataPoint = {
    timestamp: Int,
    value: Float
  }
  
  // 创建测试数据（包含重复值）
  let raw_data = [
    { timestamp: 1609459200, value: 25.5 },
    { timestamp: 1609459260, value: 25.5 },
    { timestamp: 1609459320, value: 25.5 },
    { timestamp: 1609459380, value: 26.0 },
    { timestamp: 1609459440, value: 26.0 },
    { timestamp: 1609459500, value: 27.5 },
    { timestamp: 1609459560, value: 27.5 },
    { timestamp: 1609459620, value: 27.5 },
    { timestamp: 1609459680, value: 27.5 }
  ]
  
  // 实现简单的游程编码压缩算法
  let run_length_encode = fn(data: Array[DataPoint]) {
    let mut result = []
    let mut i = 0
    
    while i < data.length() {
      let current_value = data[i].value
      let mut count = 1
      
      // 计算连续相同值的数量
      while i + count < data.length() && data[i + count].value == current_value {
        count = count + 1
      }
      
      // 添加压缩后的数据点
      result = result.push({
        timestamp: data[i].timestamp,
        value: current_value,
        count: count
      })
      
      i = i + count
    }
    
    result
  }
  
  // 执行压缩
  let compressed_data = run_length_encode(raw_data)
  
  // 验证压缩结果
  assert_eq(compressed_data.length(), 4) // 原始9个点压缩为4个
  assert_eq(compressed_data[0].count, 3) // 前3个点值相同
  assert_eq(compressed_data[1].count, 2) // 接下来2个点值相同
  assert_eq(compressed_data[2].count, 1) // 单独的点
  assert_eq(compressed_data[3].count, 3) // 最后3个点值相同
}

// 测试用例3: 分布式追踪链路分析
test "分布式追踪链路分析" {
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    service_name: String
  }
  
  // 创建测试追踪数据
  let trace_spans = [
    { trace_id: "trace123", span_id: "span1", parent_span_id: "", operation_name: "HTTP GET /api/data", start_time: 1609459200, end_time: 1609459210, service_name: "api-gateway" },
    { trace_id: "trace123", span_id: "span2", parent_span_id: "span1", operation_name: "Database Query", start_time: 1609459202, end_time: 1609459208, service_name: "user-service" },
    { trace_id: "trace123", span_id: "span3", parent_span_id: "span1", operation_name: "Cache Lookup", start_time: 1609459203, end_time: 1609459205, service_name: "cache-service" },
    { trace_id: "trace123", span_id: "span4", parent_span_id: "span2", operation_name: "Auth Check", start_time: 1609459204, end_time: 1609459206, service_name: "auth-service" }
  ]
  
  // 构建追踪树结构
  let build_trace_tree = fn(spans: Array[Span]) {
    // 按span_id创建索引
    let span_map = Map::empty()
    for span in spans {
      span_map = Map::insert(span_map, span.span_id, span)
    }
    
    // 找到根span
    let root_spans = spans.filter(fn(span) { span.parent_span_id == "" })
    
    // 递归构建子span
    let build_children = fn(parent_span_id: String) {
      spans.filter(fn(span) { span.parent_span_id == parent_span_id })
    }
    
    // 计算每个服务的总耗时
    let service_durations = Map::empty()
    for span in spans {
      let duration = span.end_time - span.start_time
      let current_duration = match Map::get(service_durations, span.service_name) {
        Some(d) => d
        None => 0
      }
      service_durations = Map::insert(service_durations, span.service_name, current_duration + duration)
    }
    
    service_durations
  }
  
  // 分析追踪数据
  let service_durations = build_trace_tree(trace_spans)
  
  // 验证分析结果
  assert_eq(Map::get(service_durations, "api-gateway"), Some(10))
  assert_eq(Map::get(service_durations, "user-service"), Some(6))
  assert_eq(Map::get(service_durations, "cache-service"), Some(2))
  assert_eq(Map::get(service_durations, "auth-service"), Some(2))
}

// 测试用例4: 遥测数据聚合统计
test "遥测数据聚合统计" {
  // 定义指标类型
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // 创建测试指标数据
  let metrics = [
    { name: "response_time", value: 120.5, timestamp: 1609459200, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { name: "response_time", value: 98.3, timestamp: 1609459260, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { name: "response_time", value: 150.2, timestamp: 1609459320, tags: [("endpoint", "/api/products"), ("method", "GET")] },
    { name: "response_time", value: 85.7, timestamp: 1609459380, tags: [("endpoint", "/api/users"), ("method", "POST")] },
    { name: "response_time", value: 110.4, timestamp: 1609459440, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { name: "error_rate", value: 0.02, timestamp: 1609459200, tags: [("service", "user-service")] },
    { name: "error_rate", value: 0.03, timestamp: 1609459260, tags: [("service", "user-service")] },
    { name: "error_rate", value: 0.01, timestamp: 1609459320, tags: [("service", "product-service")] }
  ]
  
  // 按标签分组聚合
  let aggregate_by_tags = fn(data: Array[Metric], tag_keys: Array[String]) {
    let groups = Map::empty()
    
    for metric in data {
      // 构建分组键
      let mut group_key = metric.name
      for tag_key in tag_keys {
        let tag_value = match metric.tags.find(fn(tag) { tag.0 == tag_key }) {
          Some((_, value)) => value
          None => "unknown"
        }
        group_key = group_key + ":" + tag_value
      }
      
      // 获取或创建分组
      let group = match Map::get(groups, group_key) {
        Some(g) => g
        None => []
      }
      
      // 添加指标到分组
      groups = Map::insert(groups, group_key, group.push(metric.value))
    }
    
    // 计算统计信息
    let result = Map::empty()
    let keys = Map::keys(groups)
    for key in keys {
      let values = match Map::get(groups, key) { Some(v) => v | None => [] }
      let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
      let count = values.length() as Float
      let avg = sum / count
      
      // 计算最小值和最大值
      let min_val = values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, values[0])
      let max_val = values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, values[0])
      
      let stats = {
        count: values.length(),
        sum: sum,
        avg: avg,
        min: min_val,
        max: max_val
      }
      
      result = Map::insert(result, key, stats)
    }
    
    result
  }
  
  // 执行聚合统计
  let aggregated_stats = aggregate_by_tags(metrics, ["endpoint", "method"])
  
  // 验证聚合结果
  let get_stats = fn(key: String) {
    match Map::get(aggregated_stats, key) {
      Some(stats) => stats
      None => { count: 0, sum: 0.0, avg: 0.0, min: 0.0, max: 0.0 }
    }
  }
  
  let users_get_stats = get_stats("response_time:/api/users:GET")
  assert_eq(users_get_stats.count, 3)
  assert_eq(@math.round(users_get_stats.avg * 10) / 10.0, 109.7)
  assert_eq(users_get_stats.min, 98.3)
  assert_eq(users_get_stats.max, 120.5)
}

// 测试用例5: 遥测系统性能基准测试
test "遥测系统性能基准测试" {
  // 模拟性能测试数据
  type PerformanceMetric = {
    operation: String,
    data_size: Int,
    processing_time_ms: Int,
    memory_usage_mb: Float
  }
  
  // 创建性能测试数据
  let performance_data = [
    { operation: "data_ingestion", data_size: 1000, processing_time_ms: 50, memory_usage_mb: 12.5 },
    { operation: "data_ingestion", data_size: 2000, processing_time_ms: 95, memory_usage_mb: 23.8 },
    { operation: "data_ingestion", data_size: 5000, processing_time_ms: 220, memory_usage_mb: 58.2 },
    { operation: "data_compression", data_size: 1000, processing_time_ms: 15, memory_usage_mb: 8.3 },
    { operation: "data_compression", data_size: 2000, processing_time_ms: 28, memory_usage_mb: 15.7 },
    { operation: "data_compression", data_size: 5000, processing_time_ms: 65, memory_usage_mb: 38.9 },
    { operation: "data_analysis", data_size: 1000, processing_time_ms: 120, memory_usage_mb: 25.4 },
    { operation: "data_analysis", data_size: 2000, processing_time_ms: 235, memory_usage_mb: 48.7 },
    { operation: "data_analysis", data_size: 5000, processing_time_ms: 580, memory_usage_mb: 118.3 }
  ]
  
  // 计算吞吐量（每秒处理的数据点数）
  let calculate_throughput = fn(data: Array[PerformanceMetric]) {
    let result = []
    
    for metric in data {
      let throughput = (metric.data_size as Float) / ((metric.processing_time_ms as Float) / 1000.0)
      let memory_efficiency = (metric.data_size as Float) / metric.memory_usage_mb
      
      result = result.push({
        operation: metric.operation,
        data_size: metric.data_size,
        throughput: throughput,
        memory_efficiency: memory_efficiency
      })
    }
    
    result
  }
  
  // 执行性能分析
  let performance_analysis = calculate_throughput(performance_data)
  
  // 验证性能分析结果
  let ingestion_5000 = performance_analysis.find(fn(m) { 
    m.operation == "data_ingestion" && m.data_size == 5000 
  })
  
  match ingestion_5000 {
    Some(metric) => {
      assert_eq(@math.round(metric.throughput * 10) / 10.0, 22727.3) // 5000 / 0.220
      assert_eq(@math.round(metric.memory_efficiency * 10) / 10.0, 85.9) // 5000 / 58.2
    }
    None => assert_false(true) // 测试失败
  }
}

// 测试用例6: 遥测数据质量验证
test "遥测数据质量验证" {
  // 定义数据质量问题类型
  type QualityIssue = {
    issue_type: String,
    description: String,
    severity: String
  }
  
  // 定义遥测数据点
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建包含质量问题的测试数据
  let test_data = [
    { timestamp: 1609459200, metric_name: "cpu_usage", value: 45.5, tags: [("host", "server1")] },
    { timestamp: 1609459260, metric_name: "memory_usage", value: -10.2, tags: [("host", "server1")] }, // 负值异常
    { timestamp: 1609459320, metric_name: "", value: 78.3, tags: [("host", "server1")] }, // 空指标名
    { timestamp: 0, metric_name: "disk_usage", value: 65.7, tags: [("host", "server1")] }, // 无效时间戳
    { timestamp: 1609459440, metric_name: "network_usage", value: @math.inf, tags: [] } // 无限值
  ]
  
  // 数据质量验证函数
  let validate_data_quality = fn(data: Array[TelemetryData]) {
    let issues = []
    
    for data_point in data {
      // 检查时间戳有效性
      if data_point.timestamp <= 0 {
        issues = issues.push({
          issue_type: "invalid_timestamp",
          description: "Timestamp must be positive",
          severity: "high"
        })
      }
      
      // 检查指标名称有效性
      if data_point.metric_name == "" {
        issues = issues.push({
          issue_type: "empty_metric_name",
          description: "Metric name cannot be empty",
          severity: "high"
        })
      }
      
      // 检查数值有效性
      if data_point.value < 0.0 {
        issues = issues.push({
          issue_type: "negative_value",
          description: "Value should not be negative for this metric type",
          severity: "medium"
        })
      }
      
      if @math.is_infinite(data_point.value) || @math.is_nan(data_point.value) {
        issues = issues.push({
          issue_type: "invalid_numeric_value",
          description: "Value must be a finite number",
          severity: "high"
        })
      }
      
      // 检查标签有效性
      if data_point.tags.length() == 0 {
        issues = issues.push({
          issue_type: "missing_tags",
          description: "At least one tag should be present",
          severity: "low"
        })
      }
    }
    
    issues
  }
  
  // 执行数据质量验证
  let quality_issues = validate_data_quality(test_data)
  
  // 验证质量问题检测结果
  assert_eq(quality_issues.length(), 5) // 应该检测到5个问题
  
  // 检查特定问题类型
  let has_invalid_timestamp = quality_issues.any(fn(issue) { issue.issue_type == "invalid_timestamp" })
  let has_empty_metric_name = quality_issues.any(fn(issue) { issue.issue_type == "empty_metric_name" })
  let has_negative_value = quality_issues.any(fn(issue) { issue.issue_type == "negative_value" })
  let has_invalid_numeric_value = quality_issues.any(fn(issue) { issue.issue_type == "invalid_numeric_value" })
  let has_missing_tags = quality_issues.any(fn(issue) { issue.issue_type == "missing_tags" })
  
  assert_true(has_invalid_timestamp)
  assert_true(has_empty_metric_name)
  assert_true(has_negative_value)
  assert_true(has_invalid_numeric_value)
  assert_true(has_missing_tags)
}

// 测试用例7: 遥测系统资源管理
test "遥测系统资源管理" {
  // 定义资源使用情况
  type ResourceUsage = {
    timestamp: Int,
    cpu_percent: Float,
    memory_mb: Int,
    disk_io_mb: Int,
    network_io_mb: Int
  }
  
  // 创建资源使用数据
  let resource_data = [
    { timestamp: 1609459200, cpu_percent: 45.2, memory_mb: 1024, disk_io_mb: 50, network_io_mb: 20 },
    { timestamp: 1609459260, cpu_percent: 52.8, memory_mb: 1089, disk_io_mb: 75, network_io_mb: 35 },
    { timestamp: 1609459320, cpu_percent: 78.5, memory_mb: 1256, disk_io_mb: 120, network_io_mb: 80 },
    { timestamp: 1609459380, cpu_percent: 65.3, memory_mb: 1187, disk_io_mb: 90, network_io_mb: 60 },
    { timestamp: 1609459440, cpu_percent: 48.7, memory_mb: 1045, disk_io_mb: 55, network_io_mb: 25 }
  ]
  
  // 资源使用分析和预警
  let analyze_resource_usage = fn(data: Array[ResourceUsage], thresholds: { cpu: Float, memory: Int }) {
    let alerts = []
    let resource_trends = {
      cpu_trend: [],
      memory_trend: []
    }
    
    for i in 0..data.length() {
      let current = data[i]
      
      // 检查资源使用是否超过阈值
      if current.cpu_percent > thresholds.cpu {
        alerts = alerts.push({
          timestamp: current.timestamp,
          alert_type: "high_cpu",
          message: "CPU usage exceeds threshold: " + @math.to_string(current.cpu_percent) + "%",
          severity: if current.cpu_percent > 90.0 { "critical" } else { "warning" }
        })
      }
      
      if current.memory_mb > thresholds.memory {
        alerts = alerts.push({
          timestamp: current.timestamp,
          alert_type: "high_memory",
          message: "Memory usage exceeds threshold: " + @math.to_string(current.memory_mb) + "MB",
          severity: if current.memory_mb > 1500 { "critical" } else { "warning" }
        })
      }
      
      // 计算资源使用趋势
      if i > 0 {
        let previous = data[i-1]
        let cpu_change = current.cpu_percent - previous.cpu_percent
        let memory_change = current.memory_mb - previous.memory_mb
        
        resource_trends.cpu_trend = resource_trends.cpu_trend.push(cpu_change)
        resource_trends.memory_trend = resource_trends.memory_trend.push(memory_change)
      }
    }
    
    // 计算平均趋势
    let avg_cpu_trend = if resource_trends.cpu_trend.length() > 0 {
      let sum = resource_trends.cpu_trend.reduce(fn(acc, val) { acc + val }, 0.0)
      sum / (resource_trends.cpu_trend.length() as Float)
    } else {
      0.0
    }
    
    let avg_memory_trend = if resource_trends.memory_trend.length() > 0 {
      let sum = resource_trends.memory_trend.reduce(fn(acc, val) { acc + val }, 0)
      sum / resource_trends.memory_trend.length()
    } else {
      0
    }
    
    {
      alerts: alerts,
      avg_cpu_trend: avg_cpu_trend,
      avg_memory_trend: avg_memory_trend
    }
  }
  
  // 执行资源分析
  let resource_analysis = analyze_resource_usage(resource_data, { cpu: 70.0, memory: 1200 })
  
  // 验证资源分析结果
  assert_eq(resource_analysis.alerts.length(), 2) // 应该有2个警报
  
  // 检查CPU警报
  let cpu_alerts = resource_analysis.alerts.filter(fn(alert) { alert.alert_type == "high_cpu" })
  assert_eq(cpu_alerts.length(), 1)
  assert_eq(cpu_alerts[0].severity, "warning") // 78.5% > 70% 但 < 90%
  
  // 检查内存警报
  let memory_alerts = resource_analysis.alerts.filter(fn(alert) { alert.alert_type == "high_memory" })
  assert_eq(memory_alerts.length(), 1)
  assert_eq(memory_alerts[0].severity, "warning") // 1256MB > 1200MB 但 < 1500MB
  
  // 验证趋势分析
  assert_eq(@math.round(resource_analysis.avg_cpu_trend * 10) / 10.0, 0.9) // 平均CPU趋势
  assert_eq(resource_analysis.avg_memory_trend, 58) // 平均内存趋势
}

// 测试用例8: 遥测数据实时流处理
test "遥测数据实时流处理" {
  // 定义流事件类型
  type StreamEvent = {
    event_id: String,
    event_type: String,
    timestamp: Int,
    data: Map[String, String]
  }
  
  // 定义处理结果
  type ProcessedResult = {
    event_count: Int,
    error_count: Int,
    metrics: Map[String, Float],
    last_processed_timestamp: Int
  }
  
  // 创建模拟流数据
  let stream_data = [
    { event_id: "evt001", event_type: "metric", timestamp: 1609459200, data: Map::from([("name", "cpu"), ("value", "45.2")]) },
    { event_id: "evt002", event_type: "log", timestamp: 1609459201, data: Map::from([("level", "INFO"), ("message", "Service started")]) },
    { event_id: "evt003", event_type: "metric", timestamp: 1609459202, data: Map::from([("name", "memory"), ("value", "1024")]) },
    { event_id: "evt004", event_type: "trace", timestamp: 1609459203, data: Map::from([("trace_id", "trace123"), ("span", "operation")]) },
    { event_id: "evt005", event_type: "metric", timestamp: 1609459204, data: Map::from([("name", "cpu"), ("value", "48.7")]) },
    { event_id: "evt006", event_type: "error", timestamp: 1609459205, data: Map::from([("error_code", "500"), ("message", "Internal error")]) }
  ]
  
  // 实时流处理函数
  let process_stream = fn(events: Array[StreamEvent]) {
    let mut result = {
      event_count: 0,
      error_count: 0,
      metrics: Map::empty(),
      last_processed_timestamp: 0
    }
    
    for event in events {
      result.event_count = result.event_count + 1
      result.last_processed_timestamp = event.timestamp
      
      // 处理不同类型的事件
      match event.event_type {
        "metric" => {
          let metric_name = match Map::get(event.data, "name") {
            Some(name) => name
            None => "unknown"
          }
          
          let metric_value = match Map::get(event.data, "value") {
            Some(val_str) => match @float.parse(val_str) {
              Ok(val) => val
              Err(_) => 0.0
            }
            None => 0.0
          }
          
          // 聚合指标值
          let current_value = match Map::get(result.metrics, metric_name) {
            Some(val) => val
            None => 0.0
          }
          
          result.metrics = Map::insert(result.metrics, metric_name, current_value + metric_value)
        }
        
        "error" => {
          result.error_count = result.error_count + 1
        }
        
        _ => () // 其他类型事件不做特殊处理
      }
    }
    
    result
  }
  
  // 执行流处理
  let processed_result = process_stream(stream_data)
  
  // 验证处理结果
  assert_eq(processed_result.event_count, 6)
  assert_eq(processed_result.error_count, 1)
  assert_eq(processed_result.last_processed_timestamp, 1609459205)
  
  // 验证指标聚合
  let cpu_sum = match Map::get(processed_result.metrics, "cpu") {
    Some(val) => val
    None => 0.0
  }
  let memory_sum = match Map::get(processed_result.metrics, "memory") {
    Some(val) => val
    None => 0.0
  }
  
  assert_eq(cpu_sum, 93.9) // 45.2 + 48.7
  assert_eq(memory_sum, 1024.0)
}

// 测试用例9: 遥测数据多维分析
test "遥测数据多维分析" {
  // 定义多维数据点
  type MultiDimPoint = {
    timestamp: Int,
    metrics: Map[String, Float],
    dimensions: Map[String, String]
  }
  
  // 创建多维测试数据
  let multi_dim_data = [
    { 
      timestamp: 1609459200, 
      metrics: Map::from([("cpu", 45.2), ("memory", 1024.0), ("disk", 78.5)]), 
      dimensions: Map::from([("region", "us-east"), ("service", "api"), ("version", "v1.2")])
    },
    { 
      timestamp: 1609459260, 
      metrics: Map::from([("cpu", 52.8), ("memory", 1089.0), ("disk", 82.1)]), 
      dimensions: Map::from([("region", "us-east"), ("service", "api"), ("version", "v1.2")])
    },
    { 
      timestamp: 1609459320, 
      metrics: Map::from([("cpu", 38.5), ("memory", 956.0), ("disk", 75.3)]), 
      dimensions: Map::from([("region", "us-west"), ("service", "web"), ("version", "v1.3")])
    },
    { 
      timestamp: 1609459380, 
      metrics: Map::from([("cpu", 65.3), ("memory", 1187.0), ("disk", 88.7)]), 
      dimensions: Map::from([("region", "us-west"), ("service", "web"), ("version", "v1.3")])
    },
    { 
      timestamp: 1609459440, 
      metrics: Map::from([("cpu", 48.7), ("memory", 1045.0), ("disk", 79.9)]), 
      dimensions: Map::from([("region", "us-east"), ("service", "api"), ("version", "v1.3")])
    }
  ]
  
  // 多维分析函数
  let analyze_multi_dimensional = fn(data: Array[MultiDimPoint], dimension_keys: Array[String]) {
    let groups = Map::empty()
    
    // 按维度分组
    for point in data {
      // 构建分组键
      let mut group_key = ""
      for dim_key in dimension_keys {
        let dim_value = match Map::get(point.dimensions, dim_key) {
          Some(value) => value
          None => "unknown"
        }
        if group_key == "" {
          group_key = dim_key + ":" + dim_value
        } else {
          group_key = group_key + "|" + dim_key + ":" + dim_value
        }
      }
      
      // 获取或创建分组
      let group = match Map::get(groups, group_key) {
        Some(g) => g
        None => { count: 0, metric_sums: Map::empty() }
      }
      
      // 更新分组统计
      let updated_metric_sums = Map::fold(point.metrics, group.metric_sums, fn(acc, key, value) {
        let current_sum = match Map::get(acc, key) {
          Some(sum) => sum
          None => 0.0
        }
        Map::insert(acc, key, current_sum + value)
      })
      
      groups = Map::insert(groups, group_key, {
        count: group.count + 1,
        metric_sums: updated_metric_sums
      })
    }
    
    // 计算平均值
    let result = Map::empty()
    let keys = Map::keys(groups)
    for key in keys {
      let group = match Map::get(groups, key) { Some(g) => g | None => { count: 0, metric_sums: Map::empty() } }
      let metric_averages = Map::map(group.metric_sums, fn(_, sum) { sum / (group.count as Float) })
      
      result = Map::insert(result, key, {
        count: group.count,
        metric_averages: metric_averages
      })
    }
    
    result
  }
  
  // 执行多维分析
  let analysis_result = analyze_multi_dimensional(multi_dim_data, ["region", "service"])
  
  // 验证分析结果
  let get_group_stats = fn(key: String) {
    match Map::get(analysis_result, key) {
      Some(stats) => stats
      None => { count: 0, metric_averages: Map::empty() }
    }
  }
  
  // 检查us-east:api分组
  let us_east_api_stats = get_group_stats("region:us-east|service:api")
  assert_eq(us_east_api_stats.count, 3)
  
  let us_east_api_cpu = match Map::get(us_east_api_stats.metric_averages, "cpu") {
    Some(val) => val
    None => 0.0
  }
  assert_eq(@math.round(us_east_api_cpu * 10) / 10.0, 48.9) // (45.2 + 52.8 + 48.7) / 3
  
  // 检查us-west:web分组
  let us_west_web_stats = get_group_stats("region:us-west|service:web")
  assert_eq(us_west_web_stats.count, 2)
  
  let us_west_web_memory = match Map::get(us_west_web_stats.metric_averages, "memory") {
    Some(val) => val
    None => 0.0
  }
  assert_eq(@math.round(us_west_web_memory * 10) / 10.0, 1071.5) // (956.0 + 1187.0) / 2
}

// 测试用例10: 遥测系统容错恢复
test "遥测系统容错恢复" {
  // 定义系统状态
  type SystemState = {
    is_healthy: Bool,
    error_count: Int,
    last_error_time: Int,
    recovery_attempts: Int
  }
  
  // 定义错误事件
  type ErrorEvent = {
    timestamp: Int,
    error_type: String,
    severity: String,
    recoverable: Bool
  }
  
  // 创建错误事件序列
  let error_events = [
    { timestamp: 1609459200, error_type: "network_timeout", severity: "warning", recoverable: true },
    { timestamp: 1609459230, error_type: "database_connection", severity: "error", recoverable: true },
    { timestamp: 1609459260, error_type: "memory_exhaustion", severity: "critical", recoverable: true },
    { timestamp: 1609459290, error_type: "disk_full", severity: "critical", recoverable: false },
    { timestamp: 1609459320, error_type: "service_unavailable", severity: "error", recoverable: true }
  ]
  
  // 容错恢复模拟
  let simulate_fault_tolerance = fn(events: Array[ErrorEvent]) {
    let mut system_state = {
      is_healthy: true,
      error_count: 0,
      last_error_time: 0,
      recovery_attempts: 0
    }
    
    let recovery_log = []
    
    for event in events {
      system_state.error_count = system_state.error_count + 1
      system_state.last_error_time = event.timestamp
      
      // 根据错误严重程度影响系统状态
      match event.severity {
        "warning" => () // 警告不影响系统健康状态
        "error" => {
          system_state.is_healthy = false
        }
        "critical" => {
          system_state.is_healthy = false
          // 严重错误可能导致系统需要更多恢复尝试
          system_state.recovery_attempts = system_state.recovery_attempts + 1
        }
        _ => ()
      }
      
      // 尝试恢复
      if event.recoverable && !system_state.is_healthy {
        system_state.recovery_attempts = system_state.recovery_attempts + 1
        
        // 模拟恢复过程
        let recovery_success = match event.error_type {
          "network_timeout" => true
          "database_connection" => true
          "memory_exhaustion" => system_state.recovery_attempts > 1 // 需要多次尝试
          "service_unavailable" => true
          _ => false
        }
        
        if recovery_success {
          system_state.is_healthy = true
          recovery_log = recovery_log.push({
            timestamp: event.timestamp + 30, // 假设恢复需要30秒
            event_type: "recovery_success",
            error_type: event.error_type,
            attempts: system_state.recovery_attempts
          })
        } else {
          recovery_log = recovery_log.push({
            timestamp: event.timestamp + 30,
            event_type: "recovery_failed",
            error_type: event.error_type,
            attempts: system_state.recovery_attempts
          })
        }
      } else if !event.recoverable {
        // 不可恢复的错误
        recovery_log = recovery_log.push({
          timestamp: event.timestamp,
          event_type: "unrecoverable_error",
          error_type: event.error_type,
          attempts: 0
        })
      }
    }
    
    {
      final_state: system_state,
      recovery_log: recovery_log
    }
  }
  
  // 执行容错恢复模拟
  let fault_tolerance_result = simulate_fault_tolerance(error_events)
  
  // 验证容错恢复结果
  assert_eq(fault_tolerance_result.final_state.error_count, 5)
  assert_false(fault_tolerance_result.final_state.is_healthy) // 最后一个错误后系统不健康
  assert_eq(fault_tolerance_result.final_state.recovery_attempts, 4) // 总共4次恢复尝试
  
  // 验证恢复日志
  assert_eq(fault_tolerance_result.recovery_log.length(), 5)
  
  // 检查特定恢复事件
  let recovery_success_events = fault_tolerance_result.recovery_log.filter(fn(log) { 
    log.event_type == "recovery_success" 
  })
  assert_eq(recovery_success_events.length(), 3) // 3个成功恢复
  
  let unrecoverable_events = fault_tolerance_result.recovery_log.filter(fn(log) { 
    log.event_type == "unrecoverable_error" 
  })
  assert_eq(unrecoverable_events.length(), 1) // 1个不可恢复错误
}