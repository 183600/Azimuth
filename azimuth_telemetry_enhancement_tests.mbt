// Azimuth 遥测增强测试用例
// 专注于遥测系统的高级功能和特性

// 测试1: 智能采样策略
test "智能采样策略测试" {
  // 创建采样配置
  let sampling_config = {
    default_rate: 0.1,
    high_error_rate_threshold: 0.05,
    adaptive_sampling: true
  }
  
  // 高错误率场景模拟
  let error_rate = 0.08
  let adaptive_rate = if error_rate > sampling_config.high_error_rate_threshold {
    sampling_config.default_rate * 2.0
  } else {
    sampling_config.default_rate
  }
  
  // 验证自适应采样
  assert_eq(adaptive_rate, 0.2)
  
  // 低错误率场景
  let low_error_rate = 0.02
  let normal_rate = if low_error_rate > sampling_config.high_error_rate_threshold {
    sampling_config.default_rate * 2.0
  } else {
    sampling_config.default_rate
  }
  
  assert_eq(normal_rate, 0.1)
}

// 测试2: 遥测数据聚合
test "遥测数据聚合测试" {
  // 创建指标数据点
  let metrics_data = [
    { timestamp: 1640995200, value: 100.0, tags: ["endpoint:/api/users", "method:GET"] },
    { timestamp: 1640995260, value: 150.0, tags: ["endpoint:/api/users", "method:GET"] },
    { timestamp: 1640995320, value: 120.0, tags: ["endpoint:/api/orders", "method:POST"] },
    { timestamp: 1640995380, value: 180.0, tags: ["endpoint:/api/users", "method:GET"] },
    { timestamp: 1640995440, value: 200.0, tags: ["endpoint:/api/orders", "method:POST"] }
  ]
  
  // 按端点聚合
  let aggregate_by_endpoint = fn(data: Array[{ timestamp: Int, value: Float, tags: Array[String] }]) {
    let mut result = []
    let mut processed = []
    
    for item in data {
      let endpoint_tag = item.tags.find(fn(tag) { tag.starts_with("endpoint:") })
      match endpoint_tag {
        Some(endpoint) => {
          if not(processed.contains(endpoint)) {
            processed = processed.push(endpoint)
            let endpoint_metrics = data.filter(fn(m) { m.tags.contains(endpoint) })
            let total_value = endpoint_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
            let avg_value = total_value / endpoint_metrics.length().to_float()
            result = result.push({ endpoint, count: endpoint_metrics.length(), avg_value })
          }
        }
        None => {}
      }
    }
    result
  }
  
  let aggregated = aggregate_by_endpoint(metrics_data)
  
  // 验证聚合结果
  assert_eq(aggregated.length(), 2)
  
  let users_endpoint = aggregated.find(fn(item) { item.endpoint == "endpoint:/api/users" })
  match users_endpoint {
    Some(data) => {
      assert_eq(data.count, 3)
      assert_eq(data.avg_value, 143.33333333333334)
    }
    None => assert_true(false)
  }
  
  let orders_endpoint = aggregated.find(fn(item) { item.endpoint == "endpoint:/api/orders" })
  match orders_endpoint {
    Some(data) => {
      assert_eq(data.count, 2)
      assert_eq(data.avg_value, 160.0)
    }
    None => assert_true(false)
  }
}

// 测试3: 异常检测和告警
test "异常检测和告警测试" {
  // 创建异常检测规则
  let anomaly_rules = [
    { name: "高延迟", threshold: 1000.0, comparison: "gt" },
    { name: "错误率", threshold: 0.05, comparison: "gt" },
    { name: "低吞吐量", threshold: 10.0, comparison: "lt" }
  ]
  
  // 模拟监控数据
  let monitoring_data = [
    { metric: "latency", value: 1200.0, timestamp: 1640995200 },
    { metric: "error_rate", value: 0.03, timestamp: 1640995200 },
    { metric: "throughput", value: 8.0, timestamp: 1640995200 }
  ]
  
  // 异常检测函数
  let detect_anomalies = fn(data: Array[{ metric: String, value: Float, timestamp: Int }], rules: Array[{ name: String, threshold: Float, comparison: String }]) {
    let mut anomalies = []
    
    for item in data {
      let rule = rules.find(fn(r) { r.name == item.metric })
      match rule {
        Some(r) => {
          let is_anomaly = match r.comparison {
            "gt" => item.value > r.threshold
            "lt" => item.value < r.threshold
            _ => false
          }
          
          if is_anomaly {
            anomalies = anomalies.push({
              rule: r.name,
              metric: item.metric,
              value: item.value,
              threshold: r.threshold,
              timestamp: item.timestamp
            })
          }
        }
        None => {}
      }
    }
    anomalies
  }
  
  let detected_anomalies = detect_anomalies(monitoring_data, anomaly_rules)
  
  // 验证异常检测结果
  assert_eq(detected_anomalies.length(), 2)
  
  let latency_anomaly = detected_anomalies.find(fn(a) { a.rule == "高延迟" })
  match latency_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.metric, "latency")
      assert_eq(anomaly.value, 1200.0)
      assert_eq(anomaly.threshold, 1000.0)
    }
    None => assert_true(false)
  }
  
  let throughput_anomaly = detected_anomalies.find(fn(a) { a.rule == "低吞吐量" })
  match throughput_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.metric, "throughput")
      assert_eq(anomaly.value, 8.0)
      assert_eq(anomaly.threshold, 10.0)
    }
    None => assert_true(false)
  }
}

// 测试4: 遥测数据压缩
test "遥测数据压缩测试" {
  // 创建原始遥测数据
  let telemetry_data = [
    { trace_id: "trace-001", span_id: "span-001", service: "auth", duration: 120 },
    { trace_id: "trace-001", span_id: "span-002", service: "user", duration: 85 },
    { trace_id: "trace-002", span_id: "span-003", service: "auth", duration: 150 },
    { trace_id: "trace-002", span_id: "span-004", service: "order", duration: 200 },
    { trace_id: "trace-003", span_id: "span-005", service: "payment", duration: 300 }
  ]
  
  // 按服务分组压缩
  let compress_by_service = fn(data: Array[{ trace_id: String, span_id: String, service: String, duration: Int }]) {
    let mut services = []
    let mut processed = []
    
    for item in data {
      if not(processed.contains(item.service)) {
        processed = processed.push(item.service)
        let service_spans = data.filter_fn(span) { span.service == item.service }
        let total_duration = service_spans.reduce(fn(acc, span) { acc + span.duration }, 0)
        let avg_duration = total_duration / service_spans.length()
        
        services = services.push({
          service: item.service,
          span_count: service_spans.length(),
          total_duration,
          avg_duration
        })
      }
    }
    services
  }
  
  let compressed_data = compress_by_service(telemetry_data)
  
  // 验证压缩结果
  assert_eq(compressed_data.length(), 3)
  
  let auth_service = compressed_data.find(fn(s) { s.service == "auth" })
  match auth_service {
    Some(service) => {
      assert_eq(service.span_count, 2)
      assert_eq(service.total_duration, 270)
      assert_eq(service.avg_duration, 135)
    }
    None => assert_true(false)
  }
  
  let user_service = compressed_data.find(fn(s) { s.service == "user" })
  match user_service {
    Some(service) => {
      assert_eq(service.span_count, 1)
      assert_eq(service.total_duration, 85)
      assert_eq(service.avg_duration, 85)
    }
    None => assert_true(false)
  }
}

// 测试5: 遥测数据缓存
test "遥测数据缓存测试" {
  // 模拟缓存存储
  let cache_storage = { mut data: [] }
  
  // 缓存操作函数
  let cache_get = fn(key: String) {
    cache_storage.data.find(fn(item) { item.key == key })
  }
  
  let cache_set = fn(key: String, value: String, ttl: Int) {
    let mut found = false
    let mut updated = []
    
    for item in cache_storage.data {
      if item.key == key {
        updated = updated.push({ key, value, ttl, timestamp: 1640995200 })
        found = true
      } else {
        updated = updated.push(item)
      }
    }
    
    if not(found) {
      updated = updated.push({ key, value, ttl, timestamp: 1640995200 })
    }
    
    cache_storage.data = updated
  }
  
  let cache_delete = fn(key: String) {
    cache_storage.data = cache_storage.data.filter(fn(item) { item.key != key })
  }
  
  // 测试缓存设置和获取
  cache_set("trace:001", "span_data_001", 3600)
  cache_set("trace:002", "span_data_002", 7200)
  cache_set("trace:003", "span_data_003", 1800)
  
  // 验证缓存数据
  let cached_data_001 = cache_get("trace:001")
  match cached_data_001 {
    Some(data) => {
      assert_eq(data.key, "trace:001")
      assert_eq(data.value, "span_data_001")
      assert_eq(data.ttl, 3600)
    }
    None => assert_true(false)
  }
  
  let cached_data_002 = cache_get("trace:002")
  match cached_data_002 {
    Some(data) => {
      assert_eq(data.key, "trace:002")
      assert_eq(data.value, "span_data_002")
      assert_eq(data.ttl, 7200)
    }
    None => assert_true(false)
  }
  
  // 测试缓存删除
  cache_delete("trace:002")
  let deleted_data = cache_get("trace:002")
  assert_eq(deleted_data, None)
  
  // 验证其他缓存数据仍然存在
  let remaining_data_001 = cache_get("trace:001")
  assert_ne(remaining_data_001, None)
  
  let remaining_data_003 = cache_get("trace:003")
  assert_ne(remaining_data_003, None)
}

// 测试6: 遥测数据导出
test "遥测数据导出测试" {
  // 创建导出配置
  let export_config = {
    format: "json",
    compression: true,
    batch_size: 100,
    endpoint: "https://otel-collector.example.com"
  }
  
  // 创建示例遥测数据
  let telemetry_spans = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "GET /api/users",
      start_time: 1640995200000,
      end_time: 1640995200150,
      status: "ok",
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", "200"),
        ("user.id", "user-123")
      ]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "database.query",
      start_time: 1640995200020,
      end_time: 1640995200100,
      status: "ok",
      attributes: [
        ("db.statement", "SELECT * FROM users"),
        ("db.type", "postgresql")
      ]
    }
  ]
  
  // JSON格式化函数
  let format_as_json = fn(spans: Array[SpanData]) {
    let mut json_objects = []
    
    for span in spans {
      let mut attributes_json = ""
      for (key, value) in span.attributes {
        if attributes_json.length() > 0 {
          attributes_json = attributes_json + ","
        }
        attributes_json = attributes_json + "\"" + key + "\":\"" + value + "\""
      }
      
      let span_json = "{" +
        "\"trace_id\":\"" + span.trace_id + "\"," +
        "\"span_id\":\"" + span.span_id + "\"," +
        "\"operation_name\":\"" + span.operation_name + "\"," +
        "\"start_time\":" + span.start_time.to_string() + "," +
        "\"end_time\":" + span.end_time.to_string() + "," +
        "\"status\":\"" + span.status + "\"," +
        "\"attributes\":{" + attributes_json + "}" +
        "}"
      
      json_objects = json_objects.push(span_json)
    }
    
    "[" + json_objects.join(",") + "]"
  }
  
  // 导出数据
  let exported_json = format_as_json(telemetry_spans)
  
  // 验证导出格式
  assert_true(exported_json.starts_with("["))
  assert_true(exported_json.ends_with("]"))
  assert_true(exported_json.contains("trace-001"))
  assert_true(exported_json.contains("span-001"))
  assert_true(exported_json.contains("GET /api/users"))
  assert_true(exported_json.contains("database.query"))
}

// 测试7: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  // 创建生命周期配置
  let lifecycle_config = {
    retention_days: 30,
    cleanup_interval_hours: 24,
    archive_after_days: 7
  }
  
  // 模拟不同时间的数据
  let current_time = 1640995200  // 2022-01-01
  let telemetry_data = [
    { id: "data-001", timestamp: current_time - 86400 * 2, size: 1024 },  // 2天前
    { id: "data-002", timestamp: current_time - 86400 * 5, size: 2048 },  // 5天前
    { id: "data-003", timestamp: current_time - 86400 * 10, size: 1536 }, // 10天前
    { id: "data-004", timestamp: current_time - 86400 * 20, size: 3072 }, // 20天前
    { id: "data-005", timestamp: current_time - 86400 * 35, size: 4096 }  // 35天前
  ]
  
  // 数据分类函数
  let classify_data = fn(data: Array[{ id: String, timestamp: Int, size: Int }]) {
    let mut active = []
    let mut archive = []
    let mut expired = []
    
    for item in data {
      let age_days = (current_time - item.timestamp) / 86400
      
      if age_days > lifecycle_config.retention_days {
        expired = expired.push(item)
      } else if age_days > lifecycle_config.archive_after_days {
        archive = archive.push(item)
      } else {
        active = active.push(item)
      }
    }
    
    { active, archive, expired }
  }
  
  let classified = classify_data(telemetry_data)
  
  // 验证数据分类
  assert_eq(classified.active.length(), 2)
  assert_eq(classified.archive.length(), 2)
  assert_eq(classified.expired.length(), 1)
  
  // 验证活跃数据
  let active_ids = classified.active.map(fn(item) { item.id })
  assert_true(active_ids.contains("data-001"))
  assert_true(active_ids.contains("data-002"))
  
  // 验证归档数据
  let archive_ids = classified.archive.map(fn(item) { item.id })
  assert_true(archive_ids.contains("data-003"))
  assert_true(archive_ids.contains("data-004"))
  
  // 验证过期数据
  assert_eq(classified.expired[0].id, "data-005")
  
  // 计算存储空间节省
  let total_size = telemetry_data.reduce(fn(acc, item) { acc + item.size }, 0)
  let expired_size = classified.expired.reduce(fn(acc, item) { acc + item.size }, 0)
  let savings_percentage = (expired_size.to_float() / total_size.to_float()) * 100.0
  
  assert_eq(savings_percentage, 40.0)  // 4096 / 10240 * 100
}

// 测试8: 遥测数据查询优化
test "遥测数据查询优化测试" {
  // 创建索引结构
  let index_storage = { mut trace_index: [], mut service_index: [], mut time_index: [] }
  
  // 示例数据
  let telemetry_data = [
    { trace_id: "trace-001", service: "auth", timestamp: 1640995200, duration: 120 },
    { trace_id: "trace-002", service: "user", timestamp: 1640995300, duration: 85 },
    { trace_id: "trace-003", service: "auth", timestamp: 1640995400, duration: 150 },
    { trace_id: "trace-004", service: "order", timestamp: 1640995500, duration: 200 },
    { trace_id: "trace-005", service: "payment", timestamp: 1640995600, duration: 300 }
  ]
  
  // 构建索引
  let build_indexes = fn(data: Array[TelemetryData]) {
    let mut trace_index = []
    let mut service_index = []
    let mut time_index = []
    
    for i in 0..data.length() {
      let item = data[i]
      trace_index = trace_index.push((item.trace_id, i))
      service_index = service_index.push((item.service, i))
      time_index = time_index.push((item.timestamp, i))
    }
    
    index_storage.trace_index = trace_index
    index_storage.service_index = service_index
    index_storage.time_index = time_index
  }
  
  // 查询函数
  let query_by_trace = fn(trace_id: String) {
    let matches = index_storage.trace_index.filter_fn(pair) { pair.0 == trace_id }
    matches.map(fn(pair) { telemetry_data[pair.1] })
  }
  
  let query_by_service = fn(service: String) {
    let matches = index_storage.service_index.filter_fn(pair) { pair.0 == service }
    matches.map(fn(pair) { telemetry_data[pair.1] })
  }
  
  let query_by_time_range = fn(start_time: Int, end_time: Int) {
    let matches = index_storage.time_index.filter_fn(pair) { 
      pair.0 >= start_time and pair.0 <= end_time 
    }
    matches.map(fn(pair) { telemetry_data[pair.1] })
  }
  
  // 构建索引
  build_indexes(telemetry_data)
  
  // 测试按trace查询
  let trace_results = query_by_trace("trace-001")
  assert_eq(trace_results.length(), 1)
  assert_eq(trace_results[0].service, "auth")
  assert_eq(trace_results[0].duration, 120)
  
  // 测试按服务查询
  let auth_results = query_by_service("auth")
  assert_eq(auth_results.length(), 2)
  
  let payment_results = query_by_service("payment")
  assert_eq(payment_results.length(), 1)
  assert_eq(payment_results[0].duration, 300)
  
  // 测试按时间范围查询
  let time_results = query_by_time_range(1640995300, 1640995500)
  assert_eq(time_results.length(), 3)
  
  let service_names = time_results.map(fn(item) { item.service })
  assert_true(service_names.contains("user"))
  assert_true(service_names.contains("auth"))
  assert_true(service_names.contains("order"))
}

// 测试9: 遥测数据关联分析
test "遥测数据关联分析测试" {
  // 创建关联分析数据
  let trace_data = [
    {
      trace_id: "trace-001",
      spans: [
        { span_id: "span-001", service: "gateway", operation: "GET /api/orders", duration: 50 },
        { span_id: "span-002", service: "order", operation: "process_order", duration: 120, parent: "span-001" },
        { span_id: "span-003", service: "payment", operation: "process_payment", duration: 200, parent: "span-002" },
        { span_id: "span-004", service: "inventory", operation: "check_stock", duration: 80, parent: "span-002" }
      ]
    },
    {
      trace_id: "trace-002",
      spans: [
        { span_id: "span-005", service: "gateway", operation: "GET /api/users", duration: 30 },
        { span_id: "span-006", service: "user", operation: "get_user", duration: 60, parent: "span-005" }
      ]
    }
  ]
  
  // 服务依赖分析
  let analyze_service_dependencies = fn(traces: Array[TraceData]) {
    let mut dependencies = []
    let mut processed = []
    
    for trace in traces {
      for span in trace.spans {
        if not(processed.contains(span.service)) {
          processed = processed.push(span.service)
          
          let mut child_services = []
          for child in trace.spans {
            match child.parent {
              Some(parent_id) => {
                let parent_span = trace.spans.find(fn(s) { s.span_id == parent_id })
                match parent_span {
                  Some(parent) => {
                    if parent.service == span.service {
                      child_services = child_services.push(child.service)
                    }
                  }
                  None => {}
                }
              }
              None => {}
            }
          }
          
          if child_services.length() > 0 {
            dependencies = dependencies.push({
              service: span.service,
              dependencies: child_services.unique()
            })
          }
        }
      }
    }
    dependencies
  }
  
  let dependencies = analyze_service_dependencies(trace_data)
  
  // 验证依赖关系
  assert_eq(dependencies.length(), 3)
  
  let gateway_deps = dependencies.find(fn(d) { d.service == "gateway" })
  match gateway_deps {
    Some(deps) => {
      assert_eq(deps.dependencies.length(), 2)
      assert_true(deps.dependencies.contains("order"))
      assert_true(deps.dependencies.contains("user"))
    }
    None => assert_true(false)
  }
  
  let order_deps = dependencies.find(fn(d) { d.service == "order" })
  match order_deps {
    Some(deps) => {
      assert_eq(deps.dependencies.length(), 2)
      assert_true(deps.dependencies.contains("payment"))
      assert_true(deps.dependencies.contains("inventory"))
    }
    None => assert_true(false)
  }
  
  // 性能瓶颈分析
  let analyze_performance_bottlenecks = fn(traces: Array[TraceData]) {
    let mut bottlenecks = []
    
    for trace in traces {
      let total_duration = trace.spans.reduce(fn(acc, span) { acc + span.duration }, 0)
      let slowest_span = trace.spans.reduce(fn(acc, span) { 
        if span.duration > acc.duration { span } else { acc } 
      }, trace.spans[0])
      
      let bottleneck_percentage = (slowest_span.duration.to_float() / total_duration.to_float()) * 100.0
      
      if bottleneck_percentage > 40.0 {
        bottlenecks = bottlenecks.push({
          trace_id: trace.trace_id,
          service: slowest_span.service,
          operation: slowest_span.operation,
          duration: slowest_span.duration,
          percentage: bottleneck_percentage
        })
      }
    }
    bottlenecks
  }
  
  let bottlenecks = analyze_performance_bottlenecks(trace_data)
  
  // 验证性能瓶颈
  assert_eq(bottlenecks.length(), 1)
  assert_eq(bottlenecks[0].trace_id, "trace-001")
  assert_eq(bottlenecks[0].service, "payment")
  assert_eq(bottlenecks[0].operation, "process_payment")
  assert_eq(bottlenecks[0].duration, 200)
  assert_eq(bottlenecks[0].percentage, 47.61904761904762)
}