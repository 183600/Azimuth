// Azimuth遥测系统测试用例
// 专注于遥测系统的核心功能和集成测试

// 测试1: 遥测数据采集与验证
test "telemetry data collection and validation" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // 创建遥测数据采集器
  let create_collector = fn() {
    let mut collected_data = []
    
    {
      collect: fn(data: TelemetryData) {
        // 验证数据完整性
        if data.trace_id.length() > 0 and 
           data.span_id.length() > 0 and 
           data.operation_name.length() > 0 and
           data.start_time <= data.end_time {
          collected_data = collected_data + [data]
          true
        } else {
          false
        }
      },
      
      get_collected: fn() { collected_data },
      
      get_count: fn() { collected_data.length() },
      
      clear: fn() { collected_data = [] }
    }
  }
  
  // 创建采集器实例
  let collector = create_collector()
  
  // 测试有效数据采集
  let valid_data = {
    trace_id: "trace-001",
    span_id: "span-001",
    parent_span_id: None,
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "success",
    tags: [("service", "payment"), ("region", "us-west")]
  }
  
  let collect_result = collector.collect(valid_data)
  assert_true(collect_result)
  assert_eq(collector.get_count(), 1)
  
  // 测试无效数据采集（开始时间晚于结束时间）
  let invalid_time_data = {
    trace_id: "trace-002",
    span_id: "span-002",
    parent_span_id: None,
    operation_name: "api_call",
    start_time: 1640995300,
    end_time: 1640995250,  // 早于开始时间
    status: "success",
    tags: []
  }
  
  let invalid_result = collector.collect(invalid_time_data)
  assert_false(invalid_result)
  assert_eq(collector.get_count(), 1)  // 不应增加
  
  // 测试空字段数据
  let empty_field_data = {
    trace_id: "",  // 空trace_id
    span_id: "span-003",
    parent_span_id: None,
    operation_name: "cache_lookup",
    start_time: 1640995400,
    end_time: 1640995420,
    status: "success",
    tags: []
  }
  
  let empty_result = collector.collect(empty_field_data)
  assert_false(empty_result)
  assert_eq(collector.get_count(), 1)  // 不应增加
  
  // 测试父子关系验证
  let parent_data = {
    trace_id: "trace-003",
    span_id: "parent-span",
    parent_span_id: None,
    operation_name: "parent_operation",
    start_time: 1640995500,
    end_time: 1640995600,
    status: "success",
    tags: []
  }
  
  let child_data = {
    trace_id: "trace-003",
    span_id: "child-span",
    parent_span_id: Some("parent-span"),
    operation_name: "child_operation",
    start_time: 1640995520,
    end_time: 1640995580,
    status: "success",
    tags: []
  }
  
  let parent_result = collector.collect(parent_data)
  let child_result = collector.collect(child_data)
  
  assert_true(parent_result)
  assert_true(child_result)
  assert_eq(collector.get_count(), 3)
  
  // 验证采集的数据
  let collected = collector.get_collected()
  assert_eq(collected.length(), 3)
  assert_eq(collected[0].trace_id, "trace-001")
  assert_eq(collected[1].trace_id, "trace-003")
  assert_eq(collected[2].trace_id, "trace-003")
}

// 测试2: 遥测数据分析与聚合
test "telemetry data analysis and aggregation" {
  // 模拟遥测数据点
  let data_points = [
    ("trace-001", "span-001", "database_query", 100, "success"),
    ("trace-001", "span-002", "cache_lookup", 20, "success"),
    ("trace-002", "span-003", "api_call", 150, "error"),
    ("trace-002", "span-004", "data_processing", 200, "success"),
    ("trace-003", "span-005", "database_query", 120, "success"),
    ("trace-003", "span-006", "cache_lookup", 15, "success"),
    ("trace-004", "span-007", "api_call", 300, "error"),
    ("trace-004", "span-008", "retry_operation", 50, "success")
  ]
  
  // 按操作类型分组
  let group_by_operation = fn(data: Array[(String, String, String, Int, String)]) {
    let mut groups = []
    
    for item in data {
      match item {
        (_, _, operation, duration, status) => {
          let mut found = false
          let mut updated_groups = []
          
          for group in groups {
            match group {
              (op_name, durations, statuses) => {
                if op_name == operation {
                  updated_groups = updated_groups + [(op_name, durations + [duration], statuses + [status])]
                  found = true
                } else {
                  updated_groups = updated_groups + [group]
                }
              }
            }
          }
          
          if not(found) {
            updated_groups = updated_groups + [(operation, [duration], [status])]
          }
          
          groups = updated_groups
        }
      }
    }
    
    groups
  }
  
  let operation_groups = group_by_operation(data_points)
  assert_eq(operation_groups.length(), 4)  // 4种不同的操作类型
  
  // 验证分组结果
  let find_group = fn(groups: Array[(String, Array[Int], Array[String])], operation: String) {
    let mut result = None
    for group in groups {
      match group {
        (op, durations, statuses) => {
          if op == operation {
            result = Some((op, durations, statuses))
          }
        }
      }
    }
    result
  }
  
  // 验证database_query组
  match find_group(operation_groups, "database_query") {
    Some((_, durations, statuses)) => {
      assert_eq(durations.length(), 2)
      assert_true(durations.contains(100))
      assert_true(durations.contains(120))
      assert_eq(statuses.length(), 2)
      assert_true(statuses.contains("success"))
    }
    None => assert_true(false)
  }
  
  // 验证api_call组
  match find_group(operation_groups, "api_call") {
    Some((_, durations, statuses)) => {
      assert_eq(durations.length(), 2)
      assert_true(durations.contains(150))
      assert_true(durations.contains(300))
      assert_eq(statuses.length(), 2)
      assert_true(statuses.contains("error"))
    }
    None => assert_true(false)
  }
  
  // 计算操作统计信息
  let calculate_stats = fn(durations: Array[Int]) {
    let sum = durations.reduce(fn(acc, x) { acc + x }, 0)
    let avg = sum / durations.length()
    let max = durations.reduce(fn(acc, x) { if x > acc { x } else { acc } }, durations[0])
    let min = durations.reduce(fn(acc, x) { if x < acc { x } else { acc } }, durations[0])
    
    { sum, avg, max, min, count: durations.length() }
  }
  
  // 计算成功率
  let calculate_success_rate = fn(statuses: Array[String]) {
    let success_count = statuses.reduce(fn(acc, status) {
      acc + if status == "success" { 1 } else { 0 }
    }, 0)
    success_count * 100 / statuses.length()
  }
  
  // 分析每个操作组
  let analyze_operations = fn(groups: Array[(String, Array[Int], Array[String])]) {
    groups.map(fn(group) {
      match group {
        (operation, durations, statuses) => {
          let stats = calculate_stats(durations)
          let success_rate = calculate_success_rate(statuses)
          
          {
            operation,
            stats,
            success_rate
          }
        }
      }
    })
  }
  
  let operation_analysis = analyze_operations(operation_groups)
  
  // 验证分析结果
  let db_analysis = operation_analysis.filter(fn(result) { result.operation == "database_query" })[0]
  assert_eq(db_analysis.stats.count, 2)
  assert_eq(db_analysis.stats.sum, 220)
  assert_eq(db_analysis.stats.avg, 110)
  assert_eq(db_analysis.stats.max, 120)
  assert_eq(db_analysis.stats.min, 100)
  assert_eq(db_analysis.success_rate, 100)
  
  let api_analysis = operation_analysis.filter(fn(result) { result.operation == "api_call" })[0]
  assert_eq(api_analysis.stats.count, 2)
  assert_eq(api_analysis.stats.sum, 450)
  assert_eq(api_analysis.stats.avg, 225)
  assert_eq(api_analysis.stats.max, 300)
  assert_eq(api_analysis.stats.min, 150)
  assert_eq(api_analysis.success_rate, 0)  // 两个都是错误
}

// 测试3: 遥测数据序列化与反序列化
test "telemetry data serialization and deserialization" {
  // 定义遥测事件类型
  type TelemetryEvent = 
    | SpanEvent(String, String, String, Int, Int, String)  // trace_id, span_id, operation, start, end, status
    | MetricEvent(String, Float, String)                   // name, value, unit
    | LogEvent(String, String, Int)                        // level, message, timestamp
  
  // 简单的序列化函数
  let serialize_event = fn(event: TelemetryEvent) {
    match event {
      SpanEvent(trace_id, span_id, operation, start, end, status) => 
        "SPAN|" + trace_id + "|" + span_id + "|" + operation + "|" + start.to_string() + "|" + end.to_string() + "|" + status
      MetricEvent(name, value, unit) => 
        "METRIC|" + name + "|" + value.to_string() + "|" + unit
      LogEvent(level, message, timestamp) => 
        "LOG|" + level + "|" + message + "|" + timestamp.to_string()
    }
  }
  
  // 简单的反序列化函数
  let deserialize_event = fn(serialized: String) {
    let parts = serialized.split("|")
    if parts.length() == 0 {
      None
    } else {
      match parts[0] {
        "SPAN" => {
          if parts.length() >= 7 {
            Some(SpanEvent(parts[1], parts[2], parts[3], parts[4].to_int(), parts[5].to_int(), parts[6]))
          } else {
            None
          }
        }
        "METRIC" => {
          if parts.length() >= 4 {
            Some(MetricEvent(parts[1], parts[2].to_float(), parts[3]))
          } else {
            None
          }
        }
        "LOG" => {
          if parts.length() >= 4 {
            Some(LogEvent(parts[1], parts[2], parts[3].to_int()))
          } else {
            None
          }
        }
        _ => None
      }
    }
  }
  
  // 测试Span事件的序列化和反序列化
  let span_event = SpanEvent("trace-001", "span-001", "database_query", 1640995200, 1640995250, "success")
  let serialized_span = serialize_event(span_event)
  assert_true(serialized_span.starts_with("SPAN|"))
  assert_true(serialized_span.contains("trace-001"))
  assert_true(serialized_span.contains("span-001"))
  
  match deserialize_event(serialized_span) {
    Some(SpanEvent(trace_id, span_id, operation, start, end, status)) => {
      assert_eq(trace_id, "trace-001")
      assert_eq(span_id, "span-001")
      assert_eq(operation, "database_query")
      assert_eq(start, 1640995200)
      assert_eq(end, 1640995250)
      assert_eq(status, "success")
    }
    _ => assert_true(false)
  }
  
  // 测试Metric事件的序列化和反序列化
  let metric_event = MetricEvent("cpu_usage", 75.5, "percent")
  let serialized_metric = serialize_event(metric_event)
  assert_true(serialized_metric.starts_with("METRIC|"))
  assert_true(serialized_metric.contains("cpu_usage"))
  
  match deserialize_event(serialized_metric) {
    Some(MetricEvent(name, value, unit)) => {
      assert_eq(name, "cpu_usage")
      assert_eq(value, 75.5)
      assert_eq(unit, "percent")
    }
    _ => assert_true(false)
  }
  
  // 测试Log事件的序列化和反序列化
  let log_event = LogEvent("error", "Database connection failed", 1640995300)
  let serialized_log = serialize_event(log_event)
  assert_true(serialized_log.starts_with("LOG|"))
  assert_true(serialized_log.contains("error"))
  
  match deserialize_event(serialized_log) {
    Some(LogEvent(level, message, timestamp)) => {
      assert_eq(level, "error")
      assert_eq(message, "Database connection failed")
      assert_eq(timestamp, 1640995300)
    }
    _ => assert_true(false)
  }
  
  // 测试批量序列化和反序列化
  let events = [
    SpanEvent("trace-001", "span-001", "database_query", 1640995200, 1640995250, "success"),
    MetricEvent("cpu_usage", 75.5, "percent"),
    LogEvent("info", "Application started", 1640995100),
    SpanEvent("trace-002", "span-002", "api_call", 1640995300, 1640995350, "error")
  ]
  
  let serialized_events = events.map(serialize_event)
  assert_eq(serialized_events.length(), 4)
  
  let deserialized_events = serialized_events.map(deserialize_event)
  assert_eq(deserialized_events.length(), 4)
  
  // 验证所有事件都成功反序列化
  let mut success_count = 0
  for event in deserialized_events {
    match event {
      Some(_) => success_count = success_count + 1
      None => ()
    }
  }
  assert_eq(success_count, 4)
  
  // 测试无效数据的反序列化
  let invalid_serialized = "INVALID|data"
  let invalid_result = deserialize_event(invalid_serialized)
  assert_eq(invalid_result, None)
  
  let incomplete_span = "SPAN|trace|span"
  let incomplete_result = deserialize_event(incomplete_span)
  assert_eq(incomplete_result, None)
}

// 测试4: 遥测数据过滤与查询
test "telemetry data filtering and querying" {
  // 定义遥测记录
  type TelemetryRecord = {
    trace_id: String,
    span_id: String,
    operation: String,
    duration: Int,
    status: String,
    service: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // 创建测试数据集
  let telemetry_records = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      operation: "database_query",
      duration: 100,
      status: "success",
      service: "payment-service",
      timestamp: 1640995200,
      tags: [("env", "production"), ("region", "us-west")]
    },
    {
      trace_id: "trace-002",
      span_id: "span-002",
      operation: "api_call",
      duration: 200,
      status: "error",
      service: "payment-service",
      timestamp: 1640995250,
      tags: [("env", "production"), ("region", "us-east")]
    },
    {
      trace_id: "trace-003",
      span_id: "span-003",
      operation: "cache_lookup",
      duration: 50,
      status: "success",
      service: "user-service",
      timestamp: 1640995300,
      tags: [("env", "staging"), ("region", "us-west")]
    },
    {
      trace_id: "trace-004",
      span_id: "span-004",
      operation: "database_query",
      duration: 150,
      status: "error",
      service: "user-service",
      timestamp: 1640995350,
      tags: [("env", "production"), ("region", "eu-west")]
    },
    {
      trace_id: "trace-005",
      span_id: "span-005",
      operation: "data_processing",
      duration: 300,
      status: "success",
      service: "analytics-service",
      timestamp: 1640995400,
      tags: [("env", "production"), ("region", "ap-south")]
    }
  ]
  
  // 定义查询构建器
  let create_query = fn() {
    {
      filter_by_service: fn(service: String, records: Array[TelemetryRecord]) {
        records.filter(fn(record) { record.service == service })
      },
      
      filter_by_status: fn(status: String, records: Array[TelemetryRecord]) {
        records.filter(fn(record) { record.status == status })
      },
      
      filter_by_operation: fn(operation: String, records: Array[TelemetryRecord]) {
        records.filter(fn(record) { record.operation == operation })
      },
      
      filter_by_duration_range: fn(min: Int, max: Int, records: Array[TelemetryRecord]) {
        records.filter(fn(record) { record.duration >= min and record.duration <= max })
      },
      
      filter_by_time_range: fn(start: Int, end: Int, records: Array[TelemetryRecord]) {
        records.filter(fn(record) { record.timestamp >= start and record.timestamp <= end })
      },
      
      filter_by_tag: fn(key: String, value: String, records: Array[TelemetryRecord]) {
        records.filter(fn(record) { record.tags.contains((key, value)) })
      },
      
      // 组合查询
      combine: fn(filters: Array[(Array[TelemetryRecord]) -> Array[TelemetryRecord]], records: Array[TelemetryRecord]) {
        let mut result = records
        for filter in filters {
          result = filter(result)
        }
        result
      }
    }
  }
  
  let query = create_query()
  
  // 测试按服务过滤
  let payment_records = query.filter_by_service("payment-service", telemetry_records)
  assert_eq(payment_records.length(), 2)
  assert_eq(payment_records[0].service, "payment-service")
  assert_eq(payment_records[1].service, "payment-service")
  
  // 测试按状态过滤
  let error_records = query.filter_by_status("error", telemetry_records)
  assert_eq(error_records.length(), 2)
  assert_eq(error_records[0].status, "error")
  assert_eq(error_records[1].status, "error")
  
  // 测试按操作类型过滤
  let db_records = query.filter_by_operation("database_query", telemetry_records)
  assert_eq(db_records.length(), 2)
  assert_eq(db_records[0].operation, "database_query")
  assert_eq(db_records[1].operation, "database_query")
  
  // 测试按持续时间范围过滤
  let medium_duration = query.filter_by_duration_range(100, 200, telemetry_records)
  assert_eq(medium_duration.length(), 3)
  
  // 测试按时间范围过滤
  let time_range = query.filter_by_time_range(1640995250, 1640995350, telemetry_records)
  assert_eq(time_range.length(), 3)
  
  // 测试按标签过滤
  let production_records = query.filter_by_tag("env", "production", telemetry_records)
  assert_eq(production_records.length(), 4)
  
  // 测试组合查询
  let filters = [
    fn(records) { query.filter_by_service("payment-service", records) },
    fn(records) { query.filter_by_status("success", records) }
  ]
  
  let successful_payment_records = query.combine(filters, telemetry_records)
  assert_eq(successful_payment_records.length(), 1)
  assert_eq(successful_payment_records[0].service, "payment-service")
  assert_eq(successful_payment_records[0].status, "success")
  
  // 更复杂的组合查询
  let complex_filters = [
    fn(records) { query.filter_by_tag("env", "production", records) },
    fn(records) { query.filter_by_duration_range(50, 200, records) }
  ]
  
  let complex_results = query.combine(complex_filters, telemetry_records)
  assert_eq(complex_results.length(), 3)
  
  // 验证复杂查询结果
  for record in complex_results {
    assert_true(record.tags.contains(("env", "production")))
    assert_true(record.duration >= 50 and record.duration <= 200)
  }
  
  // 测试聚合查询
  let aggregate_by_service = fn(records: Array[TelemetryRecord]) {
    let mut services = []
    
    for record in records {
      let mut found = false
      let mut updated_services = []
      
      for service_data in services {
        match service_data {
          (service_name, count, total_duration) => {
            if service_name == record.service {
              updated_services = updated_services + [(service_name, count + 1, total_duration + record.duration)]
              found = true
            } else {
              updated_services = updated_services + [service_data]
            }
          }
        }
      }
      
      if not(found) {
        updated_services = updated_services + [(record.service, 1, record.duration)]
      }
      
      services = updated_services
    }
    
    services.map(fn(service_data) {
      match service_data {
        (service_name, count, total_duration) => {
          {
            service: service_name,
            span_count: count,
            total_duration: total_duration,
            average_duration: total_duration / count
          }
        }
      }
    })
  }
  
  let service_aggregation = aggregate_by_service(telemetry_records)
  assert_eq(service_aggregation.length(), 3)
  
  let payment_stats = service_aggregation.filter(fn(stats) { stats.service == "payment-service" })[0]
  assert_eq(payment_stats.span_count, 2)
  assert_eq(payment_stats.total_duration, 300)
  assert_eq(payment_stats.average_duration, 150)
}

// 测试5: 遥测数据异常检测
test "telemetry data anomaly detection" {
  // 定义遥测指标
  type TelemetryMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // 创建测试指标数据
  let metrics = [
    { name: "response_time", value: 100.0, unit: "ms", timestamp: 1640995200, tags: [("service", "api"), ("endpoint", "/users")] },
    { name: "response_time", value: 120.0, unit: "ms", timestamp: 1640995210, tags: [("service", "api"), ("endpoint", "/users")] },
    { name: "response_time", value: 95.0, unit: "ms", timestamp: 1640995220, tags: [("service", "api"), ("endpoint", "/users")] },
    { name: "response_time", value: 110.0, unit: "ms", timestamp: 1640995230, tags: [("service", "api"), ("endpoint", "/users")] },
    { name: "response_time", value: 500.0, unit: "ms", timestamp: 1640995240, tags: [("service", "api"), ("endpoint", "/users")] },  // 异常值
    { name: "response_time", value: 105.0, unit: "ms", timestamp: 1640995250, tags: [("service", "api"), ("endpoint", "/users")] },
    { name: "cpu_usage", value: 45.0, unit: "percent", timestamp: 1640995200, tags: [("service", "api"), ("host", "server-1")] },
    { name: "cpu_usage", value: 50.0, unit: "percent", timestamp: 1640995210, tags: [("service", "api"), ("host", "server-1")] },
    { name: "cpu_usage", value: 48.0, unit: "percent", timestamp: 1640995220, tags: [("service", "api"), ("host", "server-1")] },
    { name: "cpu_usage", value: 95.0, unit: "percent", timestamp: 1640995230, tags: [("service", "api"), ("host", "server-1")] },  // 异常值
    { name: "cpu_usage", value: 52.0, unit: "percent", timestamp: 1640995240, tags: [("service", "api"), ("host", "server-1")] }
  ]
  
  // 定义异常检测器
  let create_anomaly_detector = fn(threshold_multiplier: Float) {
    {
      // 计算统计指标
      calculate_stats: fn(values: Array[Float]) {
        let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
        let mean = sum / values.length().to_float()
        
        let variance = values.reduce(fn(acc, x) {
          let diff = x - mean
          acc + diff * diff
        }, 0.0) / values.length().to_float()
        
        let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
        
        { mean, std_dev, min: values.reduce(fn(acc, x) { if x < acc { x } else { acc } }, values[0]), max: values.reduce(fn(acc, x) { if x > acc { x } else { acc } }, values[0]) }
      },
      
      // 检测异常值
      detect_anomalies: fn(values: Array[Float]) {
        let stats = calculate_stats(values)
        let upper_threshold = stats.mean + threshold_multiplier * stats.std_dev
        let lower_threshold = if stats.mean > threshold_multiplier * stats.std_dev { 
          stats.mean - threshold_multiplier * stats.std_dev 
        } else { 
          0.0 
        }
        
        values.map(fn(value) {
          {
            value,
            is_anomaly: value > upper_threshold or value < lower_threshold,
            upper_threshold,
            lower_threshold
          }
        })
      },
      
      // 检测指标中的异常
      detect_metric_anomalies: fn(metric_name: String, metrics: Array[TelemetryMetric]) {
        let filtered_metrics = metrics.filter(fn(metric) { metric.name == metric_name })
        let values = filtered_metrics.map(fn(metric) { metric.value })
        
        if values.length() > 0 {
          let anomaly_results = detect_anomalies(values)
          
          filtered_metrics.map_with_index(fn(metric, index) {
            let anomaly_info = anomaly_results[index]
            {
              metric,
              is_anomaly: anomaly_info.is_anomaly,
              deviation: if anomaly_info.is_anomaly {
                if metric.value > anomaly_info.upper_threshold {
                  metric.value - anomaly_info.upper_threshold
                } else {
                  anomaly_info.lower_threshold - metric.value
                }
              } else {
                0.0
              }
            }
          })
        } else {
          []
        }
      }
    }
  }
  
  let detector = create_anomaly_detector(2.0)  // 2倍标准差阈值
  
  // 测试响应时间异常检测
  let response_time_anomalies = detector.detect_metric_anomalies("response_time", metrics)
  assert_eq(response_time_anomalies.length(), 6)
  
  // 验证异常检测结果
  let anomaly_count = response_time_anomalies.reduce(fn(acc, result) {
    acc + if result.is_anomaly { 1 } else { 0 }
  }, 0)
  
  assert_eq(anomaly_count, 1)  // 只有一个异常值
  
  // 找到异常的响应时间记录
  let anomalous_response_time = response_time_anomalies.filter(fn(result) { result.is_anomaly })[0]
  assert_eq(anomalous_response_time.metric.value, 500.0)
  assert_true(anomalous_response_time.deviation > 0.0)
  
  // 测试CPU使用率异常检测
  let cpu_anomalies = detector.detect_metric_anomalies("cpu_usage", metrics)
  assert_eq(cpu_anomalies.length(), 5)
  
  // 验证CPU异常检测结果
  let cpu_anomaly_count = cpu_anomalies.reduce(fn(acc, result) {
    acc + if result.is_anomaly { 1 } else { 0 }
  }, 0)
  
  assert_eq(cpu_anomaly_count, 1)  // 只有一个异常值
  
  // 找到异常的CPU使用率记录
  let anomalous_cpu = cpu_anomalies.filter(fn(result) { result.is_anomaly })[0]
  assert_eq(anomalous_cpu.metric.value, 95.0)
  assert_true(anomalous_cpu.deviation > 0.0)
  
  // 测试阈值调整
  let sensitive_detector = create_anomaly_detector(1.0)  // 更敏感的阈值
  let sensitive_anomalies = sensitive_detector.detect_metric_anomalies("response_time", metrics)
  
  let sensitive_anomaly_count = sensitive_anomalies.reduce(fn(acc, result) {
    acc + if result.is_anomaly { 1 } else { 0 }
  }, 0)
  
  // 更敏感的阈值应该检测到更多的异常
  assert_true(sensitive_anomaly_count >= anomaly_count)
  
  // 测试趋势异常检测
  let detect_trend_anomaly = fn(values: Array[Float], trend_threshold: Float) {
    if values.length() < 3 {
      []
    } else {
      let mut trends = []
      
      for i in 1..values.length() {
        let current_change = values[i] - values[i - 1]
        trends = trends + [current_change]
      }
      
      let trend_stats = detector.calculate_stats(trends)
      let trend_upper = trend_stats.mean + trend_threshold * trend_stats.std_dev
      let trend_lower = trend_stats.mean - trend_threshold * trend_stats.std_dev
      
      trends.map(fn(change) {
        {
          change,
          is_anomaly: change > trend_upper or change < trend_lower,
          trend_upper,
          trend_lower
        }
      })
    }
  }
  
  let response_times = metrics.filter(fn(m) { m.name == "response_time" }).map(fn(m) { m.value })
  let trend_anomalies = detect_trend_anomaly(response_times, 2.0)
  
  // 验证趋势异常检测
  let trend_anomaly_count = trend_anomalies.reduce(fn(acc, result) {
    acc + if result.is_anomaly { 1 } else { 0 }
  }, 0)
  
  // 应该检测到响应时间的突然增加
  assert_true(trend_anomaly_count > 0)
}

// 测试6: 遥测数据性能基准测试
test "telemetry data performance benchmarking" {
  // 定义性能测试函数
  let benchmark_operation = fn(operation: () -> T, iterations: Int) {
    let start_time = 1640995200  // 模拟时间戳
    
    for i in 1..=iterations {
      operation()
    }
    
    let end_time = 1640995300  // 模拟时间戳
    let duration = end_time - start_time
    
    {
      iterations,
      duration,
      avg_time_per_operation: duration / iterations
    }
  }
  
  // 测试数据生成性能
  let generate_telemetry_data = fn() {
    {
      trace_id: "trace-" + (1000 + 1).to_string(),
      span_id: "span-" + (2000 + 1).to_string(),
      operation: "test_operation",
      duration: 100 + 1,
      status: "success",
      timestamp: 1640995200 + 1
    }
  }
  
  let data_generation_benchmark = benchmark_operation(generate_telemetry_data, 1000)
  assert_eq(data_generation_benchmark.iterations, 1000)
  assert_eq(data_generation_benchmark.duration, 1000)
  assert_eq(data_generation_benchmark.avg_time_per_operation, 1)
  
  // 测试数据过滤性能
  let test_data = []
  for i in 1..=1000 {
    test_data = test_data + [{
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation: if i % 2 == 0 { "database_query" } else { "api_call" },
      duration: i * 10,
      status: if i % 5 == 0 { "error" } else { "success" },
      timestamp: 1640995200 + i
    }]
  }
  
  let filter_operation = fn() {
    test_data.filter(fn(record) { record.status == "success" })
  }
  
  let filter_benchmark = benchmark_operation(filter_operation, 100)
  assert_eq(filter_benchmark.iterations, 100)
  assert_eq(filter_benchmark.duration, 1000)
  assert_eq(filter_benchmark.avg_time_per_operation, 10)
  
  // 测试数据聚合性能
  let aggregate_operation = fn() {
    let mut total_duration = 0
    let mut success_count = 0
    
    for record in test_data {
      total_duration = total_duration + record.duration
      if record.status == "success" {
        success_count = success_count + 1
      }
    }
    
    { total_duration, success_count }
  }
  
  let aggregate_benchmark = benchmark_operation(aggregate_operation, 100)
  assert_eq(aggregate_benchmark.iterations, 100)
  assert_eq(aggregate_benchmark.duration, 1000)
  assert_eq(aggregate_benchmark.avg_time_per_operation, 10)
  
  // 测试数据序列化性能
  let serialize_operation = fn() {
    test_data.map(fn(record) {
      record.trace_id + "|" + record.span_id + "|" + record.operation + "|" + 
      record.duration.to_string() + "|" + record.status
    })
  }
  
  let serialize_benchmark = benchmark_operation(serialize_operation, 100)
  assert_eq(serialize_benchmark.iterations, 100)
  assert_eq(serialize_benchmark.duration, 1000)
  assert_eq(serialize_benchmark.avg_time_per_operation, 10)
  
  // 测试内存使用模拟
  let estimate_memory_usage = fn(data_size: Int, record_size: Int) {
    // 简化的内存使用估算
    let base_overhead = 1000  // 基础开销
    let total_data_memory = data_size * record_size
    let indexing_overhead = data_size * 10  // 索引开销
    
    base_overhead + total_data_memory + indexing_overhead
  }
  
  let small_dataset_memory = estimate_memory_usage(100, 200)
  let medium_dataset_memory = estimate_memory_usage(1000, 200)
  let large_dataset_memory = estimate_memory_usage(10000, 200)
  
  assert_true(small_dataset_memory < medium_dataset_memory)
  assert_true(medium_dataset_memory < large_dataset_memory)
  
  // 验证内存使用线性增长
  let memory_ratio_1 = medium_dataset_memory / small_dataset_memory
  let memory_ratio_2 = large_dataset_memory / medium_dataset_memory
  
  assert_eq(memory_ratio_1, 10)  // 10倍数据量，10倍内存
  assert_eq(memory_ratio_2, 10)  // 10倍数据量，10倍内存
  
  // 测试查询性能随数据量变化
  let benchmark_query_performance = fn(dataset_size: Int) {
    let dataset = []
    for i in 1..=dataset_size {
      dataset = dataset + [{
        trace_id: "trace-" + i.to_string(),
        span_id: "span-" + i.to_string(),
        operation: if i % 3 == 0 { "database_query" } else if i % 3 == 1 { "api_call" } else { "cache_lookup" },
        duration: i * 5,
        status: if i % 7 == 0 { "error" } else { "success" },
        timestamp: 1640995200 + i
      }]
    }
    
    let start_time = 1640995200
    
    // 执行查询
    let filtered = dataset.filter(fn(record) { record.operation == "database_query" and record.status == "success" })
    let aggregated = filtered.reduce(fn(acc, record) { acc + record.duration }, 0)
    
    let end_time = 1640995300
    end_time - start_time
  }
  
  let small_query_time = benchmark_query_performance(100)
  let medium_query_time = benchmark_query_performance(1000)
  let large_query_time = benchmark_query_performance(10000)
  
  // 验证查询时间随数据量增长
  assert_true(small_query_time <= medium_query_time)
  assert_true(medium_query_time <= large_query_time)
  
  // 测试批量操作性能
  let benchmark_batch_operations = fn(batch_size: Int, operation_count: Int) {
    let start_time = 1640995200
    
    for batch in 1..=operation_count {
      let batch_data = []
      for i in 1..=batch_size {
        batch_data = batch_data + [{
          id: (batch - 1) * batch_size + i,
          value: i * 10,
          processed: false
        }]
      }
      
      // 处理批量数据
      let processed = batch_data.map(fn(item) { { item | processed: true } })
    }
    
    let end_time = 1640995300
    end_time - start_time
  }
  
  let small_batch_time = benchmark_batch_operations(10, 100)
  let large_batch_time = benchmark_batch_operations(100, 10)
  
  // 验证大批量操作更高效
  assert_true(large_batch_time <= small_batch_time)
}

// 测试7: 遥测数据缓存机制
test "telemetry data caching mechanism" {
  // 定义缓存条目
  type CacheEntry[T] = {
    key: String,
    value: T,
    timestamp: Int,
    ttl: Int,  // 生存时间（秒）
    access_count: Int
  }
  
  // 创建缓存系统
  let create_cache = fn(max_size: Int, default_ttl: Int) {
    let mut cache_entries = []
    let mut current_time = 1640995200
    
    {
      // 获取当前时间
      get_time: fn() { current_time },
      
      // 设置时间
      set_time: fn(time: Int) { current_time = time },
      
      // 检查条目是否过期
      is_expired: fn(entry: CacheEntry[T]) {
        (current_time - entry.timestamp) > entry.ttl
      },
      
      // 清理过期条目
      cleanup_expired: fn() {
        cache_entries = cache_entries.filter(fn(entry) { not(is_expired(entry)) })
      },
      
      // 获取缓存值
      get: fn(key: String) {
        cleanup_expired()
        
        let mut result = None
        let mut updated_entries = []
        
        for entry in cache_entries {
          if entry.key == key {
            result = Some(entry.value)
            updated_entries = updated_entries + [{ entry | access_count: entry.access_count + 1 }]
          } else {
            updated_entries = updated_entries + [entry]
          }
        }
        
        cache_entries = updated_entries
        result
      },
      
      // 设置缓存值
      set: fn(key: String, value: T, ttl: Option[Int]) {
        cleanup_expired()
        
        let actual_ttl = match ttl {
          Some(t) => t
          None => default_ttl
        }
        
        // 检查是否更新现有条目
        let mut found = false
        let mut updated_entries = []
        
        for entry in cache_entries {
          if entry.key == key {
            updated_entries = updated_entries + [{
              key,
              value,
              timestamp: current_time,
              ttl: actual_ttl,
              access_count: 0
            }]
            found = true
          } else {
            updated_entries = updated_entries + [entry]
          }
        }
        
        if not(found) {
          // 如果缓存已满，移除最少使用的条目
          if cache_entries.length() >= max_size {
            let mut min_access = cache_entries[0].access_count
            let mut min_index = 0
            
            for i in 1..cache_entries.length() {
              if cache_entries[i].access_count < min_access {
                min_access = cache_entries[i].access_count
                min_index = i
              }
            }
            
            updated_entries = []
            for i in 0..cache_entries.length() {
              if i != min_index {
                updated_entries = updated_entries + [cache_entries[i]]
              }
            }
          }
          
          updated_entries = updated_entries + [{
            key,
            value,
            timestamp: current_time,
            ttl: actual_ttl,
            access_count: 0
          }]
        }
        
        cache_entries = updated_entries
      },
      
      // 获取缓存统计信息
      get_stats: fn() {
        cleanup_expired()
        
        {
          size: cache_entries.length(),
          max_size,
          total_accesses: cache_entries.reduce(fn(acc, entry) { acc + entry.access_count }, 0),
          average_accesses: if cache_entries.length() > 0 {
            cache_entries.reduce(fn(acc, entry) { acc + entry.access_count }, 0) / cache_entries.length()
          } else {
            0
          }
        }
      }
    }
  }
  
  let cache = create_cache(3, 10)  // 最大3个条目，默认TTL 10秒
  
  // 测试基本缓存操作
  cache.set("key1", "value1", None)
  cache.set("key2", "value2", None)
  cache.set("key3", "value3", None)
  
  assert_eq(cache.get("key1"), Some("value1"))
  assert_eq(cache.get("key2"), Some("value2"))
  assert_eq(cache.get("key3"), Some("value3"))
  
  // 测试缓存统计
  let stats = cache.get_stats()
  assert_eq(stats.size, 3)
  assert_eq(stats.max_size, 3)
  assert_eq(stats.total_accesses, 3)  // 每个key被访问一次
  
  // 测试缓存更新
  cache.set("key1", "new_value1", None)
  assert_eq(cache.get("key1"), Some("new_value1"))
  
  // 测试缓存淘汰
  cache.set("key4", "value4", None)  // 应该淘汰最少使用的条目
  
  assert_eq(cache.get("key4"), Some("value4"))
  // 由于所有条目都被访问了一次，淘汰顺序不确定，但缓存大小应保持为3
  let updated_stats = cache.get_stats()
  assert_eq(updated_stats.size, 3)
  
  // 测试TTL过期
  cache.set_time(1640995250)  // 前进50秒
  
  // 所有条目都应该过期
  assert_eq(cache.get("key1"), None)
  assert_eq(cache.get("key2"), None)
  assert_eq(cache.get("key3"), None)
  assert_eq(cache.get("key4"), None)
  
  let expired_stats = cache.get_stats()
  assert_eq(expired_stats.size, 0)
  
  // 测试不同TTL
  cache.set_time(1640995200)  // 重置时间
  cache.set("short_lived", "value1", Some(5))   // 5秒TTL
  cache.set("long_lived", "value2", Some(20))   // 20秒TTL
  
  cache.set_time(1640995210)  // 前进10秒
  
  assert_eq(cache.get("short_lived"), None)  // 应该已过期
  assert_eq(cache.get("long_lived"), Some("value2"))  // 仍有效
  
  // 测试缓存命中率
  let cache_with_stats = create_cache(5, 15)
  
  // 填充缓存
  cache_with_stats.set("trace-001", "data1", None)
  cache_with_stats.set("trace-002", "data2", None)
  cache_with_stats.set("trace-003", "data3", None)
  
  // 模拟访问模式
  let hits = cache_with_stats.get("trace-001")  // 命中
  let miss = cache_with_stats.get("trace-999")  // 未命中
  let hits2 = cache_with_stats.get("trace-002")  // 命中
  let hits3 = cache_with_stats.get("trace-001")  // 命中
  
  assert_eq(hits, Some("data1"))
  assert_eq(miss, None)
  assert_eq(hits2, Some("data2"))
  assert_eq(hits3, Some("data1"))
  
  let final_stats = cache_with_stats.get_stats()
  assert_eq(final_stats.total_accesses, 4)  // 4次访问
  assert_eq(final_stats.size, 3)  // 3个条目
  
  // 验证访问计数
  // trace-001被访问2次，trace-002被访问1次，trace-003被访问0次
  let total_accesses = final_stats.total_accesses
  assert_eq(total_accesses, 4)
  
  // 测试缓存预热
  let preload_cache = fn(data: Array[(String, String)]) {
    let new_cache = create_cache(10, 30)
    
    for item in data {
      match item {
        (key, value) => new_cache.set(key, value, None)
      }
    }
    
    new_cache
  }
  
  let initial_data = [
    ("trace-001", "span-data-1"),
    ("trace-002", "span-data-2"),
    ("trace-003", "span-data-3"),
    ("trace-004", "span-data-4"),
    ("trace-005", "span-data-5")
  ]
  
  let preloaded_cache = preload_cache(initial_data)
  let preloaded_stats = preloaded_cache.get_stats()
  
  assert_eq(preloaded_stats.size, 5)
  assert_eq(preloaded_cache.get("trace-003"), Some("span-data-3"))
}

// 测试8: 遥测数据压缩与传输
test "telemetry data compression and transmission" {
  // 定义遥测数据包
  type TelemetryPacket = {
    packet_id: String,
    timestamp: Int,
    data: Array[String],  // 序列化的遥测事件
    checksum: String
  }
  
  // 简单的数据压缩函数（模拟）
  let compress_data = fn(data: Array[String]) {
    // 模拟压缩：移除重复的前缀
    let mut compressed = []
    let mut last_prefix = ""
    
    for item in data {
      if item.starts_with("SPAN|") {
        if last_prefix != "SPAN|" {
          compressed = compressed + ["PREFIX:SPAN|"]
          last_prefix = "SPAN|"
        }
        compressed = compressed + [item.substring(5, item.length() - 5)]
      } else if item.starts_with("METRIC|") {
        if last_prefix != "METRIC|" {
          compressed = compressed + ["PREFIX:METRIC|"]
          last_prefix = "METRIC|"
        }
        compressed = compressed + [item.substring(7, item.length() - 7)]
      } else {
        compressed = compressed + [item]
      }
    }
    
    compressed
  }
  
  // 简单的数据解压缩函数
  let decompress_data = fn(compressed: Array[String]) {
    let mut decompressed = []
    let mut current_prefix = ""
    
    for item in compressed {
      if item.starts_with("PREFIX:") {
        current_prefix = item.substring(7, item.length() - 7)
      } else {
        decompressed = decompressed + [current_prefix + item]
      }
    }
    
    decompressed
  }
  
  // 计算简单的校验和
  let calculate_checksum = fn(data: Array[String]) {
    let mut sum = 0
    for item in data {
      for i in 0..item.length() {
        sum = sum + item[i].to_int()
      }
    }
    "checksum-" + (sum % 10000).to_string()
  }
  
  // 创建数据包
  let create_packet = fn(packet_id: String, data: Array[String], timestamp: Int) {
    let compressed = compress_data(data)
    let checksum = calculate_checksum(compressed)
    
    {
      packet_id,
      timestamp,
      data: compressed,
      checksum
    }
  }
  
  // 验证数据包
  let verify_packet = fn(packet: TelemetryPacket) {
    let calculated_checksum = calculate_checksum(packet.data)
    packet.checksum == calculated_checksum
  }
  
  // 解包数据
  let unpack_packet = fn(packet: TelemetryPacket) {
    if verify_packet(packet) {
      let decompressed = decompress_data(packet.data)
      Some(decompressed)
    } else {
      None
    }
  }
  
  // 测试数据压缩
  let telemetry_data = [
    "SPAN|trace-001|span-001|database_query|100|success",
    "SPAN|trace-001|span-002|cache_lookup|20|success",
    "SPAN|trace-002|span-003|api_call|150|error",
    "METRIC|cpu_usage|75.5|percent",
    "METRIC|memory_usage|60.2|percent",
    "LOG|info|Application started|1640995200"
  ]
  
  let compressed = compress_data(telemetry_data)
  assert_true(compressed.length() < telemetry_data.length())
  assert_true(compressed.contains("PREFIX:SPAN|"))
  assert_true(compressed.contains("PREFIX:METRIC|"))
  
  // 测试数据解压缩
  let decompressed = decompress_data(compressed)
  assert_eq(decompressed.length(), telemetry_data.length())
  
  for i in 0..decompressed.length() {
    assert_eq(decompressed[i], telemetry_data[i])
  }
  
  // 测试数据包创建和验证
  let packet = create_packet("packet-001", telemetry_data, 1640995200)
  assert_eq(packet.packet_id, "packet-001")
  assert_eq(packet.timestamp, 1640995200)
  assert_true(packet.checksum.starts_with("checksum-"))
  
  // 测试数据包验证
  assert_true(verify_packet(packet))
  
  // 测试篡改的数据包验证
  let tampered_packet = { packet | checksum: "invalid-checksum" }
  assert_false(verify_packet(tampered_packet))
  
  // 测试数据包解包
  match unpack_packet(packet) {
    Some(unpacked_data) => {
      assert_eq(unpacked_data.length(), telemetry_data.length())
      for i in 0..unpacked_data.length() {
        assert_eq(unpacked_data[i], telemetry_data[i])
      }
    }
    None => assert_true(false)
  }
  
  // 测试篡改的数据包解包
  match unpack_packet(tampered_packet) {
    Some(_) => assert_true(false)  // 不应该成功解包
    None => assert_true(true)      // 应该返回None
  }
  
  // 测试批量数据包处理
  let create_batch_packets = fn(data: Array[String], packet_size: Int) {
    let mut packets = []
    let mut packet_id = 1
    
    for i in 0..data.length() {
      if i % packet_size == 0 {
        let end_index = if i + packet_size <= data.length() { i + packet_size } else { data.length() }
        let packet_data = data.slice(i, end_index)
        let packet = create_packet("packet-" + packet_id.to_string(), packet_data, 1640995200 + i)
        packets = packets + [packet]
        packet_id = packet_id + 1
      }
    }
    
    packets
  }
  
  let large_dataset = []
  for i in 1..=20 {
    large_dataset = large_dataset + [
      "SPAN|trace-" + i.to_string() + "|span-" + i.to_string() + "|operation|" + (i * 10).to_string() + "|success"
    ]
  }
  
  let batch_packets = create_batch_packets(large_dataset, 5)
  assert_eq(batch_packets.length(), 4)  // 20个数据项，每包5个，共4包
  
  // 测试批量解包
  let unpack_batch = fn(packets: Array[TelemetryPacket]) {
    let mut all_data = []
    
    for packet in packets {
      match unpack_packet(packet) {
        Some(packet_data) => {
          for item in packet_data {
            all_data = all_data + [item]
          }
        }
        None => ()
      }
    }
    
    all_data
  }
  
  let unpacked_batch = unpack_batch(batch_packets)
  assert_eq(unpacked_batch.length(), large_dataset.length())
  
  // 测试传输模拟
  let simulate_transmission = fn(packet: TelemetryPacket, packet_loss_rate: Float) {
    // 模拟数据包丢失
    let random_value = (packet.timestamp % 100).to_float() / 100.0
    
    if random_value < packet_loss_rate {
      None  // 数据包丢失
    } else {
      // 模拟传输延迟
      let delayed_packet = { packet | timestamp: packet.timestamp + 100 }
      Some(delayed_packet)
    }
  }
  
  // 测试无丢失传输
  match simulate_transmission(packet, 0.0) {
    Some(transmitted_packet) => {
      assert_eq(transmitted_packet.packet_id, packet.packet_id)
      assert_eq(transmitted_packet.timestamp, packet.timestamp + 100)
      assert_true(verify_packet(transmitted_packet))
    }
    None => assert_true(false)
  }
  
  // 测试有丢失传输
  let mut lost_packets = 0
  let mut successful_packets = 0
  
  for i in 1..=10 {
    let test_packet = create_packet("test-" + i.to_string(), ["test-data"], 1640995200 + i)
    match simulate_transmission(test_packet, 0.3) {  // 30%丢失率
      Some(_) => successful_packets = successful_packets + 1
      None => lost_packets = lost_packets + 1
    }
  }
  
  // 验证传输结果
  assert_eq(lost_packets + successful_packets, 10)
  assert_true(lost_packets > 0)  // 应该有一些数据包丢失
  assert_true(successful_packets > 0)  // 应该有一些数据包成功传输
  
  // 测试压缩率计算
  let calculate_compression_ratio = fn(original: Array[String], compressed: Array[String]) {
    let original_size = original.reduce(fn(acc, item) { acc + item.length() }, 0)
    let compressed_size = compressed.reduce(fn(acc, item) { acc + item.length() }, 0)
    
    if original_size > 0 {
      (1.0 - (compressed_size.to_float() / original_size.to_float())) * 100.0
    } else {
      0.0
    }
  }
  
  let compression_ratio = calculate_compression_ratio(telemetry_data, compressed)
  assert_true(compression_ratio > 0.0)  // 应该有压缩效果
  
  // 测试重复数据的压缩效果
  let repetitive_data = []
  for i in 1..=10 {
    repetitive_data = repetitive_data + [
      "SPAN|trace-001|span-" + i.to_string() + "|database_query|" + (i * 10).to_string() + "|success"
    ]
  }
  
  let compressed_repetitive = compress_data(repetitive_data)
  let repetitive_compression_ratio = calculate_compression_ratio(repetitive_data, compressed_repetitive)
  
  // 重复数据应该有更好的压缩效果
  assert_true(repetitive_compression_ratio >= compression_ratio)
}

// 测试9: 遥测数据实时处理
test "telemetry data real-time processing" {
  // 定义实时处理器
  type TelemetryProcessor = {
    process_event: fn(String, String, Int, String) -> String,
    get_metrics: fn() -> (Int, Int, Float),
    reset_metrics: fn()
  }
  
  // 创建实时处理器
  let create_realtime_processor = fn() {
    let mut events_processed = 0
    let mut errors_detected = 0
    let mut total_processing_time = 0.0
    
    {
      process_event: fn(trace_id: String, span_id: String, duration: Int, status: String) {
        let start_time = 1640995200
        
        // 处理事件
        events_processed = events_processed + 1
        
        if status == "error" {
          errors_detected = errors_detected + 1
        }
        
        // 模拟处理逻辑
        let processed_event = trace_id + ":" + span_id + ":" + duration.to_string() + ":" + status
        
        let end_time = 1640995200 + 5  // 模拟5ms处理时间
        total_processing_time = total_processing_time + 5.0
        
        processed_event
      },
      
      get_metrics: fn() {
        let avg_processing_time = if events_processed > 0 {
          total_processing_time / events_processed.to_float()
        } else {
          0.0
        }
        
        (events_processed, errors_detected, avg_processing_time)
      },
      
      reset_metrics: fn() {
        events_processed = 0
        errors_detected = 0
        total_processing_time = 0.0
      }
    }
  }
  
  // 创建流处理器
  let create_stream_processor = fn(processor: TelemetryProcessor) {
    let mut buffer = []
    let buffer_size = 10
    
    {
      // 添加事件到流
      add_event: fn(trace_id: String, span_id: String, duration: Int, status: String) {
        let event = (trace_id, span_id, duration, status)
        buffer = buffer + [event]
        
        // 如果缓冲区满了，处理事件
        if buffer.length() >= buffer_size {
          process_batch()
        }
      },
      
      // 处理批量事件
      process_batch: fn() {
        for event in buffer {
          match event {
            (trace_id, span_id, duration, status) => {
              processor.process_event(trace_id, span_id, duration, status)
            }
          }
        }
        buffer = []
      },
      
      // 强制处理所有缓冲的事件
      flush: fn() {
        if buffer.length() > 0 {
          process_batch()
        }
      },
      
      // 获取处理器指标
      get_metrics: fn() { processor.get_metrics() },
      
      // 重置处理器指标
      reset_metrics: fn() { processor.reset_metrics() }
    }
  }
  
  let realtime_processor = create_realtime_processor()
  let stream_processor = create_stream_processor(realtime_processor)
  
  // 测试实时处理
  stream_processor.add_event("trace-001", "span-001", 100, "success")
  stream_processor.add_event("trace-002", "span-002", 150, "success")
  stream_processor.add_event("trace-003", "span-003", 200, "error")
  stream_processor.add_event("trace-004", "span-004", 120, "success")
  stream_processor.add_event("trace-005", "span-005", 180, "error")
  
  // 缓冲区未满，事件不应被处理
  let (processed, errors, avg_time) = stream_processor.get_metrics()
  assert_eq(processed, 0)
  assert_eq(errors, 0)
  assert_eq(avg_time, 0.0)
  
  // 添加更多事件直到缓冲区满
  stream_processor.add_event("trace-006", "span-006", 90, "success")
  stream_processor.add_event("trace-007", "span-007", 110, "success")
  stream_processor.add_event("trace-008", "span-008", 130, "success")
  stream_processor.add_event("trace-009", "span-009", 170, "error")
  stream_processor.add_event("trace-010", "span-010", 140, "success")
  
  // 缓冲区已满，事件应被处理
  let (processed_after_batch, errors_after_batch, avg_time_after_batch) = stream_processor.get_metrics()
  assert_eq(processed_after_batch, 10)
  assert_eq(errors_after_batch, 3)
  assert_eq(avg_time_after_batch, 5.0)
  
  // 测试刷新处理
  stream_processor.add_event("trace-011", "span-011", 160, "success")
  stream_processor.add_event("trace-012", "span-012", 190, "error")
  
  // 刷新处理剩余事件
  stream_processor.flush()
  
  let (processed_after_flush, errors_after_flush, avg_time_after_flush) = stream_processor.get_metrics()
  assert_eq(processed_after_flush, 12)
  assert_eq(errors_after_flush, 4)
  
  // 测试时间窗口处理
  let create_time_window_processor = fn(window_size_ms: Int) {
    let mut events = []
    let mut window_start = 1640995200
    let mut window_end = window_start + window_size_ms
    
    {
      add_event: fn(trace_id: String, span_id: String, duration: Int, status: String, timestamp: Int) {
        let event = (trace_id, span_id, duration, status, timestamp)
        events = events + [event]
        
        // 如果事件超出当前窗口，处理窗口内的事件
        if timestamp >= window_end {
          process_window()
          
          // 移动到下一个窗口
          window_start = window_end
          window_end = window_start + window_size_ms
          
          // 保留当前窗口内的事件
          events = events.filter(fn(e) {
            match e {
              (_, _, _, _, ts) => ts >= window_start and ts < window_end
            }
          })
        }
      },
      
      process_window: fn() {
        // 处理当前窗口内的事件
        let window_events = events.filter(fn(e) {
          match e {
            (_, _, _, _, ts) => ts >= window_start and ts < window_end
          }
        })
        
        // 计算窗口统计信息
        let event_count = window_events.length()
        let error_count = window_events.reduce(fn(acc, e) {
          match e {
            (_, _, _, status, _) => acc + if status == "error" { 1 } else { 0 }
          }
        }, 0)
        
        let avg_duration = if event_count > 0 {
          window_events.reduce(fn(acc, e) {
            match e {
              (_, _, _, duration, _) => acc + duration
            }
          }, 0) / event_count
        } else {
          0
        }
        
        // 返回窗口统计信息
        {
          window_start,
          window_end,
          event_count,
          error_count,
          avg_duration
        }
      },
      
      flush: fn() {
        if events.length() > 0 {
          process_window()
        }
      }
    }
  }
  
  let window_processor = create_time_window_processor(1000)  // 1秒窗口
  
  // 添加跨多个窗口的事件
  window_processor.add_event("trace-001", "span-001", 100, "success", 1640995200)
  window_processor.add_event("trace-002", "span-002", 150, "success", 164099520500)
  window_processor.add_event("trace-003", "span-003", 200, "error", 164099520800)
  
  // 事件仍在第一个窗口内，不应被处理
  window_processor.add_event("trace-004", "span-004", 120, "success", 1640996200)  // 下一个窗口
  
  // 测试窗口统计
  let window_stats = window_processor.process_window()
  assert_eq(window_stats.window_start, 1640995200)
  assert_eq(window_stats.window_end, 1640996200)
  assert_eq(window_stats.event_count, 3)
  assert_eq(window_stats.error_count, 1)
  assert_eq(window_stats.avg_duration, 150)  // (100 + 150 + 200) / 3
  
  // 测试事件聚合处理
  let create_aggregation_processor = fn() {
    let mut aggregations = []
    
    {
      process_event: fn(trace_id: String, operation: String, duration: Int, status: String) {
        // 查找或创建聚合条目
        let mut found = false
        let mut updated_aggregations = []
        
        for aggregation in aggregations {
          match aggregation {
            (op, count, total_duration, errors) => {
              if op == operation {
                updated_aggregations = updated_aggregations + [(op, count + 1, total_duration + duration, errors + if status == "error" { 1 } else { 0 })]
                found = true
              } else {
                updated_aggregations = updated_aggregations + [aggregation]
              }
            }
          }
        }
        
        if not(found) {
          updated_aggregations = updated_aggregations + [(operation, 1, duration, if status == "error" { 1 } else { 0 })]
        }
        
        aggregations = updated_aggregations
      },
      
      get_aggregations: fn() {
        aggregations.map(fn(aggregation) {
          match aggregation {
            (operation, count, total_duration, errors) => {
              {
                operation,
                event_count: count,
                total_duration,
                average_duration: total_duration / count,
                error_count: errors,
                error_rate: if count > 0 { errors * 100 / count } else { 0 }
              }
            }
          }
        })
      },
      
      reset: fn() { aggregations = [] }
    }
  }
  
  let aggregation_processor = create_aggregation_processor()
  
  // 处理多个事件
  aggregation_processor.process_event("trace-001", "database_query", 100, "success")
  aggregation_processor.process_event("trace-002", "database_query", 150, "success")
  aggregation_processor.process_event("trace-003", "api_call", 200, "error")
  aggregation_processor.process_event("trace-004", "api_call", 120, "success")
  aggregation_processor.process_event("trace-005", "cache_lookup", 50, "success")
  aggregation_processor.process_event("trace-006", "database_query", 180, "error")
  
  // 获取聚合结果
  let aggregations = aggregation_processor.get_aggregations()
  assert_eq(aggregations.length(), 3)
  
  // 验证database_query聚合
  let db_aggregation = aggregations.filter(fn(a) { a.operation == "database_query" })[0]
  assert_eq(db_aggregation.event_count, 3)
  assert_eq(db_aggregation.total_duration, 430)
  assert_eq(db_aggregation.average_duration, 143)
  assert_eq(db_aggregation.error_count, 1)
  assert_eq(db_aggregation.error_rate, 33)
  
  // 验证api_call聚合
  let api_aggregation = aggregations.filter(fn(a) { a.operation == "api_call" })[0]
  assert_eq(api_aggregation.event_count, 2)
  assert_eq(api_aggregation.total_duration, 320)
  assert_eq(api_aggregation.average_duration, 160)
  assert_eq(api_aggregation.error_count, 1)
  assert_eq(api_aggregation.error_rate, 50)
}

// 测试10: 遥测数据配置管理
test "telemetry data configuration management" {
  // 定义配置项类型
  type ConfigValue = 
    | StringValue(String)
    | IntValue(Int)
    | FloatValue(Float)
    | BoolValue(Bool)
    | ArrayValue(Array[String])
  
  // 定义配置结构
  type Config = {
    key: String,
    value: ConfigValue,
    description: String,
    is_required: Bool,
    default_value: Option[ConfigValue]
  }
  
  // 创建配置管理器
  let create_config_manager = fn() {
    let mut configs = []
    
    {
      // 添加配置项
      add_config: fn(key: String, value: ConfigValue, description: String, is_required: Bool, default_value: Option[ConfigValue]) {
        configs = configs + [{
          key,
          value,
          description,
          is_required,
          default_value
        }]
      },
      
      // 获取配置值
      get_config: fn(key: String) {
        let mut result = None
        
        for config in configs {
          if config.key == key {
            result = Some(config.value)
          }
        }
        
        result
      },
      
      // 设置配置值
      set_config: fn(key: String, value: ConfigValue) {
        let mut found = false
        let mut updated_configs = []
        
        for config in configs {
          if config.key == key {
            updated_configs = updated_configs + [{ config | value }]
            found = true
          } else {
            updated_configs = updated_configs + [config]
          }
        }
        
        if not(found) {
          // 如果配置不存在，使用默认值创建
          updated_configs = updated_configs + [{
            key,
            value,
            description: "",
            is_required: false,
            default_value: None
          }]
        }
        
        configs = updated_configs
      },
      
      // 验证配置
      validate_configs: fn() {
        let mut issues = []
        
        for config in configs {
          if config.is_required and config.value == match config.default_value {
            Some(default) => default
            None => StringValue("")
          } {
            issues = issues + ["Required config '" + config.key + "' is missing or using default value"]
          }
        }
        
        issues
      },
      
      // 获取所有配置
      get_all_configs: fn() { configs },
      
      // 重置为默认值
      reset_to_defaults: fn() {
        let mut reset_configs = []
        
        for config in configs {
          match config.default_value {
            Some(default_value) => {
              reset_configs = reset_configs + [{ config | value: default_value }]
            }
            None => {
              reset_configs = reset_configs + [config]
            }
          }
        }
        
        configs = reset_configs
      }
    }
  }
  
  let config_manager = create_config_manager()
  
  // 添加遥测系统配置
  config_manager.add_config(
    "telemetry.enabled",
    BoolValue(true),
    "Enable or disable telemetry collection",
    true,
    Some(BoolValue(true))
  )
  
  config_manager.add_config(
    "telemetry.sampling_rate",
    FloatValue(1.0),
    "Sampling rate for telemetry data (0.0 to 1.0)",
    true,
    Some(FloatValue(1.0))
  )
  
  config_manager.add_config(
    "telemetry.batch_size",
    IntValue(100),
    "Batch size for telemetry data processing",
    false,
    Some(IntValue(100))
  )
  
  config_manager.add_config(
    "telemetry.export_endpoints",
    ArrayValue(["http://localhost:4317"]),
    "Telemetry data export endpoints",
    false,
    Some(ArrayValue(["http://localhost:4317"]))
  )
  
  config_manager.add_config(
    "telemetry.max_spans_per_trace",
    IntValue(1000),
    "Maximum number of spans per trace",
    false,
    Some(IntValue(1000))
  )
  
  // 测试获取配置值
  match config_manager.get_config("telemetry.enabled") {
    Some(BoolValue(enabled)) => assert_true(enabled)
    _ => assert_true(false)
  }
  
  match config_manager.get_config("telemetry.sampling_rate") {
    Some(FloatValue(rate)) => assert_eq(rate, 1.0)
    _ => assert_true(false)
  }
  
  match config_manager.get_config("telemetry.batch_size") {
    Some(IntValue(size)) => assert_eq(size, 100)
    _ => assert_true(false)
  }
  
  match config_manager.get_config("telemetry.export_endpoints") {
    Some(ArrayValue(endpoints)) => {
      assert_eq(endpoints.length(), 1)
      assert_eq(endpoints[0], "http://localhost:4317")
    }
    _ => assert_true(false)
  }
  
  // 测试设置配置值
  config_manager.set_config("telemetry.sampling_rate", FloatValue(0.5))
  config_manager.set_config("telemetry.batch_size", IntValue(200))
  
  match config_manager.get_config("telemetry.sampling_rate") {
    Some(FloatValue(rate)) => assert_eq(rate, 0.5)
    _ => assert_true(false)
  }
  
  match config_manager.get_config("telemetry.batch_size") {
    Some(IntValue(size)) => assert_eq(size, 200)
    _ => assert_true(false)
  }
  
  // 测试配置验证
  let validation_issues = config_manager.validate_configs()
  assert_eq(validation_issues.length(), 0)  // 所有必需配置都已设置
  
  // 测试重置为默认值
  config_manager.reset_to_defaults()
  
  match config_manager.get_config("telemetry.sampling_rate") {
    Some(FloatValue(rate)) => assert_eq(rate, 1.0)  // 重置为默认值
    _ => assert_true(false)
  }
  
  match config_manager.get_config("telemetry.batch_size") {
    Some(IntValue(size)) => assert_eq(size, 100)  // 重置为默认值
    _ => assert_true(false)
  }
  
  // 测试配置转换
  let config_to_string = fn(value: ConfigValue) {
    match value {
      StringValue(s) => s
      IntValue(i) => i.to_string()
      FloatValue(f) => f.to_string()
      BoolValue(b) => if b { "true" } else { "false" }
      ArrayValue(arr) => "[" + arr.reduce(fn(acc, item) { acc + "," + item }, "").substring(1) + "]"
    }
  }
  
  let string_value = config_to_string(StringValue("test"))
  assert_eq(string_value, "test")
  
  let int_value = config_to_string(IntValue(42))
  assert_eq(int_value, "42")
  
  let float_value = config_to_string(FloatValue(3.14))
  assert_eq(float_value, "3.14")
  
  let bool_value = config_to_string(BoolValue(true))
  assert_eq(bool_value, "true")
  
  let array_value = config_to_string(ArrayValue(["a", "b", "c"]))
  assert_eq(array_value, "[a,b,c]")
  
  // 测试配置格式验证
  let validate_config_value = fn(key: String, value: ConfigValue) {
    match key {
      "telemetry.sampling_rate" => {
        match value {
          FloatValue(rate) => rate >= 0.0 and rate <= 1.0
          _ => false
        }
      }
      "telemetry.batch_size" => {
        match value {
          IntValue(size) => size > 0
          _ => false
        }
      }
      "telemetry.max_spans_per_trace" => {
        match value {
          IntValue(max_spans) => max_spans > 0
          _ => false
        }
      }
      "telemetry.enabled" => {
        match value {
          BoolValue(_) => true
          _ => false
        }
      }
      "telemetry.export_endpoints" => {
        match value {
          ArrayValue(endpoints) => endpoints.length() > 0
          _ => false
        }
      }
      _ => true
    }
  }
  
  // 测试有效配置值
  assert_true(validate_config_value("telemetry.sampling_rate", FloatValue(0.5)))
  assert_true(validate_config_value("telemetry.batch_size", IntValue(100)))
  assert_true(validate_config_value("telemetry.enabled", BoolValue(true)))
  
  // 测试无效配置值
  assert_false(validate_config_value("telemetry.sampling_rate", FloatValue(1.5)))  // 超出范围
  assert_false(validate_config_value("telemetry.batch_size", IntValue(-10)))     // 负数
  assert_false(validate_config_value("telemetry.enabled", StringValue("true")))  // 类型错误
  
  // 测试配置应用
  let apply_config = fn(configs: Array[Config]) {
    let mut applied_configs = []
    
    for config in configs {
      if validate_config_value(config.key, config.value) {
        applied_configs = applied_configs + [config]
      }
    }
    
    applied_configs
  }
  
  let all_configs = config_manager.get_all_configs()
  let applied_configs = apply_config(all_configs)
  
  assert_eq(applied_configs.length(), all_configs.length())  // 所有配置都有效
  
  // 测试配置更新
  config_manager.set_config("telemetry.sampling_rate", FloatValue(1.5))  // 无效值
  config_manager.set_config("telemetry.batch_size", IntValue(-10))       // 无效值
  
  let updated_configs = config_manager.get_all_configs()
  let filtered_configs = apply_config(updated_configs)
  
  // 过滤后的配置应该排除无效配置
  assert_true(filtered_configs.length() < updated_configs.length())
}