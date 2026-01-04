// Azimuth高级综合测试用例
// 专注于遥测系统的核心功能与高级MoonBit语言特性的结合

// 测试1: 遥测数据序列化与反序列化
test "telemetry data serialization and deserialization" {
  // 定义遥测Span数据结构
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试Span
  let original_span = {
    trace_id: "trace-001",
    span_id: "span-001",
    parent_span_id: None,
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: Some(1640995250),
    status: "completed",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("db.result_count", "42")
    ]
  }
  
  // 序列化函数
  let serialize_span = fn(span: Span) {
    let attributes_str = span.attributes.reduce(fn(acc, attr) {
      match attr {
        (key, value) => acc + key + ":" + value + ";"
      }
    }, "")
    
    let parent_str = match span.parent_span_id {
      Some(parent) => parent
      None => ""
    }
    
    let end_str = match span.end_time {
      Some(end) => end.to_string()
      None => ""
    }
    
    span.trace_id + "|" + span.span_id + "|" + parent_str + "|" + 
    span.operation_name + "|" + span.start_time.to_string() + "|" + 
    end_str + "|" + span.status + "|" + attributes_str
  }
  
  // 反序列化函数
  let deserialize_span = fn(serialized: String) {
    let parts = serialized.split("|")
    if parts.length() >= 7 {
      let trace_id = parts[0]
      let span_id = parts[1]
      let parent_str = parts[2]
      let operation_name = parts[3]
      let start_time = parts[4].to_int()
      let end_str = parts[5]
      let status = parts[6]
      
      let parent_span_id = if parent_str == "" { None } else { Some(parent_str) }
      let end_time = if end_str == "" { None } else { Some(end_str.to_int()) }
      
      let attributes_str = if parts.length() > 7 { parts[7] } else { "" }
      let attr_pairs = attributes_str.split(";")
      let mut attributes = []
      
      for pair in attr_pairs {
        if pair.contains(":") {
          let key_value = pair.split(":")
          if key_value.length() >= 2 {
            attributes = attributes + [(key_value[0], key_value[1])]
          }
        }
      }
      
      {
        trace_id,
        span_id,
        parent_span_id,
        operation_name,
        start_time,
        end_time,
        status,
        attributes
      }
    } else {
      // 返回一个默认的Span结构
      {
        trace_id: "",
        span_id: "",
        parent_span_id: None,
        operation_name: "",
        start_time: 0,
        end_time: None,
        status: "",
        attributes: []
      }
    }
  }
  
  // 测试序列化
  let serialized = serialize_span(original_span)
  assert_true(serialized.contains("trace-001"))
  assert_true(serialized.contains("span-001"))
  assert_true(serialized.contains("database_query"))
  assert_true(serialized.contains("completed"))
  assert_true(serialized.contains("db.type:postgresql"))
  
  // 测试反序列化
  let deserialized = deserialize_span(serialized)
  assert_eq(deserialized.trace_id, original_span.trace_id)
  assert_eq(deserialized.span_id, original_span.span_id)
  assert_eq(deserialized.operation_name, original_span.operation_name)
  assert_eq(deserialized.start_time, original_span.start_time)
  assert_eq(deserialized.status, original_span.status)
  
  match deserialized.parent_span_id {
    Some(parent) => assert_eq(parent, "")
    None => assert_true(true)
  }
  
  match deserialized.end_time {
    Some(end) => assert_eq(end, 1640995250)
    None => assert_true(false)
  }
  
  assert_eq(deserialized.attributes.length(), 3)
  assert_true(deserialized.attributes.contains(("db.type", "postgresql")))
  assert_true(deserialized.attributes.contains(("db.statement", "SELECT * FROM users")))
  assert_true(deserialized.attributes.contains(("db.result_count", "42")))
}

// 测试2: 遥测数据聚合与分析
test "telemetry data aggregation and analysis" {
  // 定义遥测指标类型
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // 创建测试指标数据
  let metrics = [
    { name: "cpu_usage", value: 25.5, unit: "percent", timestamp: 1640995200, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "memory_usage", value: 45.2, unit: "percent", timestamp: 1640995200, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "response_time", value: 120.0, unit: "ms", timestamp: 1640995210, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { name: "cpu_usage", value: 30.1, unit: "percent", timestamp: 1640995220, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "memory_usage", value: 50.8, unit: "percent", timestamp: 1640995220, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "response_time", value: 150.0, unit: "ms", timestamp: 1640995230, tags: [("endpoint", "/api/users"), ("method", "GET")] },
    { name: "cpu_usage", value: 35.7, unit: "percent", timestamp: 1640995240, tags: [("host", "server2"), ("region", "us-east")] },
    { name: "memory_usage", value: 60.3, unit: "percent", timestamp: 1640995240, tags: [("host", "server2"), ("region", "us-east")] },
    { name: "response_time", value: 95.0, unit: "ms", timestamp: 1640995250, tags: [("endpoint", "/api/orders"), ("method", "POST")] }
  ]
  
  // 按指标名称分组
  let group_by_name = fn(metrics: Array[Metric]) {
    let mut groups = []
    let mut processed_names = []
    
    for metric in metrics {
      if not(processed_names.contains(metric.name)) {
        processed_names = processed_names + [metric.name]
        let same_metrics = metrics.filter(fn(m) { m.name == metric.name })
        groups = groups + [(metric.name, same_metrics)]
      }
    }
    
    groups
  }
  
  let grouped_metrics = group_by_name(metrics)
  assert_eq(grouped_metrics.length(), 3)
  
  // 计算每个指标的平均值
  let calculate_average = fn(metrics: Array[Metric]) {
    let sum = metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
    sum / metrics.length().to_float()
  }
  
  let averages = grouped_metrics.map(fn(group) {
    match group {
      (name, metrics) => (name, calculate_average(metrics))
    }
  })
  
  assert_eq(averages.length(), 3)
  
  // 验证CPU使用率平均值
  let cpu_avg = averages.filter(fn(avg) {
    match avg {
      (name, _) => name == "cpu_usage"
    }
  })[0]
  
  match cpu_avg {
    (_, value) => {
      assert_true(value > 30.0 && value < 31.0)  // (25.5 + 30.1 + 35.7) / 3 ≈ 30.43
    }
  }
  
  // 按标签过滤指标
  let filter_by_tag = fn(metrics: Array[Metric], tag_key: String, tag_value: String) {
    metrics.filter(fn(metric) {
      metric.tags.contains((tag_key, tag_value))
    })
  }
  
  let server1_metrics = filter_by_tag(metrics, "host", "server1")
  assert_eq(server1_metrics.length(), 4)
  
  let us_west_metrics = filter_by_tag(metrics, "region", "us-west")
  assert_eq(us_west_metrics.length(), 4)
  
  let get_endpoint_metrics = filter_by_tag(metrics, "endpoint", "/api/users")
  assert_eq(get_endpoint_metrics.length(), 2)
  
  // 计算时间窗口内的指标统计
  let time_window_stats = fn(metrics: Array[Metric], start_time: Int, end_time: Int) {
    let window_metrics = metrics.filter(fn(m) { 
      m.timestamp >= start_time && m.timestamp <= end_time 
    })
    
    if window_metrics.length() > 0 {
      let values = window_metrics.map(fn(m) { m.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / values.length().to_float()
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      
      {
        count: window_metrics.length(),
        average: avg,
        maximum: max,
        minimum: min,
        sum: sum
      }
    } else {
      {
        count: 0,
        average: 0.0,
        maximum: 0.0,
        minimum: 0.0,
        sum: 0.0
      }
    }
  }
  
  let cpu_metrics = metrics.filter(fn(m) { m.name == "cpu_usage" })
  let cpu_stats = time_window_stats(cpu_metrics, 1640995200, 1640995240)
  
  assert_eq(cpu_stats.count, 3)
  assert_true(cpu_stats.average > 30.0 && cpu_stats.average < 31.0)
  assert_eq(cpu_stats.maximum, 35.7)
  assert_eq(cpu_stats.minimum, 25.5)
  assert_true(cpu_stats.sum > 90.0 && cpu_stats.sum < 92.0)
}

// 测试3: 分布式追踪上下文传播
test "distributed tracing context propagation" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // 定义传播格式
  type PropagationFormat = 
    | TextMap(Array[(String, String)])
    | Binary(Array[Byte])
    | HttpHeader(String)
  
  // 创建初始追踪上下文
  let root_context = {
    trace_id: "trace-001",
    span_id: "span-001",
    baggage: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("tenant.id", "tenant-789")
    ],
    flags: 1
  }
  
  // 文本映射传播
  let inject_text_map = fn(context: TraceContext) {
    let mut headers = []
    headers = headers + [("x-trace-id", context.trace_id)]
    headers = headers + [("x-span-id", context.span_id)]
    headers = headers + [("x-trace-flags", context.flags.to_string())]
    
    // 注入baggage项
    for item in context.baggage {
      match item {
        (key, value) => {
          headers = headers + [("x-baggage-" + key, value)]
        }
      }
    }
    
    TextMap(headers)
  }
  
  // 从文本映射提取上下文
  let extract_text_map = fn(format: PropagationFormat) {
    match format {
      TextMap(headers) => {
        let mut trace_id = ""
        let mut span_id = ""
        let mut flags = 0
        let mut baggage = []
        
        for header in headers {
          match header {
            (key, value) => {
              if key == "x-trace-id" {
                trace_id = value
              } else if key == "x-span-id" {
                span_id = value
              } else if key == "x-trace-flags" {
                flags = value.to_int()
              } else if key.starts_with("x-baggage-") {
                let baggage_key = key.substring(11)  // 移除"x-baggage-"前缀
                baggage = baggage + [(baggage_key, value)]
              }
            }
          }
        }
        
        if trace_id != "" && span_id != "" {
          Some({
            trace_id,
            span_id,
            baggage,
            flags
          })
        } else {
          None
        }
      }
      _ => None
    }
  }
  
  // 测试文本映射传播
  let text_map_format = inject_text_map(root_context)
  match text_map_format {
    TextMap(headers) => {
      assert_eq(headers.length(), 6)  // 3个基本头 + 3个baggage头
      assert_true(headers.contains(("x-trace-id", "trace-001")))
      assert_true(headers.contains(("x-span-id", "span-001")))
      assert_true(headers.contains(("x-trace-flags", "1")))
      assert_true(headers.contains(("x-baggage-user.id", "user-123")))
      assert_true(headers.contains(("x-baggage-request.id", "req-456")))
      assert_true(headers.contains(("x-baggage-tenant.id", "tenant-789")))
    }
    _ => assert_true(false)
  }
  
  // 测试文本映射提取
  let extracted_context = extract_text_map(text_map_format)
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, root_context.trace_id)
      assert_eq(context.span_id, root_context.span_id)
      assert_eq(context.flags, root_context.flags)
      assert_eq(context.baggage.length(), 3)
      assert_true(context.baggage.contains(("user.id", "user-123")))
      assert_true(context.baggage.contains(("request.id", "req-456")))
      assert_true(context.baggage.contains(("tenant.id", "tenant-789")))
    }
    None => assert_true(false)
  }
  
  // 创建子Span上下文
  let create_child_context = fn(parent: TraceContext, child_span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id: child_span_id,
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  let child_context = create_child_context(root_context, "span-002")
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_eq(child_context.span_id, "span-002")
  assert_eq(child_context.baggage, root_context.baggage)
  assert_eq(child_context.flags, root_context.flags)
  
  // 添加baggage项
  let add_baggage_item = fn(context: TraceContext, key: String, value: String) {
    let mut new_baggage = context.baggage
    // 检查是否已存在相同key的项
    let mut found = false
    let mut updated_baggage = []
    
    for item in new_baggage {
      match item {
        (k, v) => {
          if k == key {
            updated_baggage = updated_baggage + [(k, value)]
            found = true
          } else {
            updated_baggage = updated_baggage + [(k, v)]
          }
        }
      }
    }
    
    if not(found) {
      updated_baggage = updated_baggage + [(key, value)]
    }
    
    { context | baggage: updated_baggage }
  }
  
  let updated_context = add_baggage_item(child_context, "service.name", "payment-service")
  assert_eq(updated_context.baggage.length(), 4)
  assert_true(updated_context.baggage.contains(("service.name", "payment-service")))
  
  // 验证原始上下文未被修改
  assert_eq(child_context.baggage.length(), 3)
  assert_false(child_context.baggage.contains(("service.name", "payment-service")))
}

// 测试4: 遥测数据采样策略
test "telemetry data sampling strategies" {
  // 定义采样决策
  type SamplingDecision = 
    | Recorded
    | NotRecorded
  
  // 定义采样结果
  type SamplingResult = {
    decision: SamplingDecision,
    attributes: Array[(String, String)]
  }
  
  // 定义采样策略
  type SamplingStrategy = 
    | AlwaysOn
    | AlwaysOff
    | Probabilistic(Float)  // 采样率 (0.0 - 1.0)
    | RateLimiting(Int)     // 每秒最大采样数
    | Adaptive(String)      // 基于属性的自适应采样
  
  // 实现采样决策函数
  let make_sampling_decision = fn(strategy: SamplingStrategy, trace_id: String, attributes: Array[(String, String)]) {
    match strategy {
      AlwaysOn => {
        {
          decision: Recorded,
          attributes: [("sampler.type", "always_on")]
        }
      }
      AlwaysOff => {
        {
          decision: NotRecorded,
          attributes: [("sampler.type", "always_off")]
        }
      }
      Probabilistic(rate) => {
        // 使用trace_id的哈希值进行确定性采样
        let hash = trace_id.length() % 100  // 简化的哈希函数
        let threshold = (rate * 100.0).to_int()
        
        if hash < threshold {
          {
            decision: Recorded,
            attributes: [
              ("sampler.type", "probabilistic"),
              ("sampler.rate", rate.to_string())
            ]
          }
        } else {
          {
            decision: NotRecorded,
            attributes: [
              ("sampler.type", "probabilistic"),
              ("sampler.rate", rate.to_string())
            ]
          }
        }
      }
      RateLimiting(max_per_second) => {
        // 简化的速率限制实现
        let current_second = 1640995200  // 模拟当前时间戳
        let trace_hash = trace_id.length() % 10  // 简化的哈希
        
        if trace_hash < max_per_second % 10 {
          {
            decision: Recorded,
            attributes: [
              ("sampler.type", "rate_limiting"),
              ("sampler.limit", max_per_second.to_string())
            ]
          }
        } else {
          {
            decision: NotRecorded,
            attributes: [
              ("sampler.type", "rate_limiting"),
              ("sampler.limit", max_per_second.to_string())
            ]
          }
        }
      }
      Adaptive(rule) => {
        // 基于属性的自适应采样
        let should_sample = match rule {
          "error_only" => {
            attributes.contains(("error", "true"))
          }
          "high_value_operations" => {
            attributes.contains(("operation.value", "high")) or 
            attributes.contains(("user.type", "premium"))
          }
          "slow_operations" => {
            attributes.contains(("duration.slow", "true"))
          }
          _ => false
        }
        
        if should_sample {
          {
            decision: Recorded,
            attributes: [
              ("sampler.type", "adaptive"),
              ("sampler.rule", rule)
            ]
          }
        } else {
          {
            decision: NotRecorded,
            attributes: [
              ("sampler.type", "adaptive"),
              ("sampler.rule", rule)
            ]
          }
        }
      }
    }
  }
  
  // 测试AlwaysOn策略
  let always_on_result = make_sampling_decision(AlwaysOn, "trace-001", [])
  match always_on_result.decision {
    Recorded => assert_true(true)
    NotRecorded => assert_true(false)
  }
  assert_true(always_on_result.attributes.contains(("sampler.type", "always_on")))
  
  // 测试AlwaysOff策略
  let always_off_result = make_sampling_decision(AlwaysOff, "trace-002", [])
  match always_off_result.decision {
    Recorded => assert_true(false)
    NotRecorded => assert_true(true)
  }
  assert_true(always_off_result.attributes.contains(("sampler.type", "always_off")))
  
  // 测试概率采样策略
  let prob_result1 = make_sampling_decision(Probabilistic(0.5), "trace-003", [])
  let prob_result2 = make_sampling_decision(Probabilistic(0.5), "trace-004", [])
  
  // 由于使用确定性哈希，结果应该是可预测的
  // trace-003长度为9，9 % 100 = 9，阈值是50，应该被采样
  // trace-004长度为9，9 % 100 = 9，阈值是50，应该被采样
  match prob_result1.decision {
    Recorded => assert_true(true)
    NotRecorded => assert_true(false)
  }
  
  match prob_result2.decision {
    Recorded => assert_true(true)
    NotRecorded => assert_true(false)
  }
  
  // 测试速率限制策略
  let rate_limit_result1 = make_sampling_decision(RateLimiting(5), "trace-005", [])
  let rate_limit_result2 = make_sampling_decision(RateLimiting(5), "trace-006", [])
  
  // trace-005长度为9，9 % 10 = 9，5 % 10 = 5，9 >= 5，不应该被采样
  // trace-006长度为9，9 % 10 = 9，5 % 10 = 5，9 >= 5，不应该被采样
  match rate_limit_result1.decision {
    Recorded => assert_true(false)
    NotRecorded => assert_true(true)
  }
  
  match rate_limit_result2.decision {
    Recorded => assert_true(false)
    NotRecorded => assert_true(true)
  }
  
  // 测试自适应采样策略 - 错误优先
  let error_attributes = [("error", "true"), ("error.message", "Connection timeout")]
  let adaptive_error_result = make_sampling_decision(Adaptive("error_only"), "trace-007", error_attributes)
  match adaptive_error_result.decision {
    Recorded => assert_true(true)
    NotRecorded => assert_true(false)
  }
  
  // 测试自适应采样策略 - 高价值操作
  let high_value_attributes = [("operation.value", "high"), ("user.type", "premium")]
  let adaptive_high_value_result = make_sampling_decision(Adaptive("high_value_operations"), "trace-008", high_value_attributes)
  match adaptive_high_value_result.decision {
    Recorded => assert_true(true)
    NotRecorded => assert_true(false)
  }
  
  // 测试自适应采样策略 - 普通操作
  let normal_attributes = [("operation.type", "standard"), ("user.type", "regular")]
  let adaptive_normal_result = make_sampling_decision(Adaptive("high_value_operations"), "trace-009", normal_attributes)
  match adaptive_normal_result.decision {
    Recorded => assert_true(false)
    NotRecorded => assert_true(true)
  }
  
  // 测试采样决策的属性
  match prob_result1.attributes {
    attrs => {
      assert_true(attrs.contains(("sampler.type", "probabilistic")))
      assert_true(attrs.contains(("sampler.rate", "0.5")))
    }
  }
  
  match adaptive_error_result.attributes {
    attrs => {
      assert_true(attrs.contains(("sampler.type", "adaptive")))
      assert_true(attrs.contains(("sampler.rule", "error_only")))
    }
  }
}

// 测试5: 遥测指标计算与转换
test "telemetry metrics calculation and transformation" {
  // 定义原始遥测事件
  type TelemetryEvent = {
    timestamp: Int,
    event_type: String,
    duration: Int,
    status: String,
    service: String,
    attributes: Array[(String, String)]
  }
  
  // 定义计算后的指标
  type CalculatedMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    dimensions: Array[(String, String)]
  }
  
  // 创建测试事件数据
  let events = [
    { timestamp: 1640995200, event_type: "http_request", duration: 120, status: "200", service: "api-service", attributes: [("endpoint", "/api/users"), ("method", "GET")] },
    { timestamp: 1640995210, event_type: "http_request", duration: 150, status: "200", service: "api-service", attributes: [("endpoint", "/api/users"), ("method", "GET")] },
    { timestamp: 1640995220, event_type: "http_request", duration: 300, status: "500", service: "api-service", attributes: [("endpoint", "/api/orders"), ("method", "POST")] },
    { timestamp: 1640995230, event_type: "http_request", duration: 80, status: "200", service: "api-service", attributes: [("endpoint", "/api/products"), ("method", "GET")] },
    { timestamp: 1640995240, event_type: "http_request", duration: 200, status: "404", service: "api-service", attributes: [("endpoint", "/api/invalid"), ("method", "GET")] },
    { timestamp: 1640995250, event_type: "database_query", duration: 50, status: "success", service: "db-service", attributes: [("query_type", "SELECT"), ("table", "users")] },
    { timestamp: 1640995260, event_type: "database_query", duration: 100, status: "success", service: "db-service", attributes: [("query_type", "UPDATE"), ("table", "orders")] },
    { timestamp: 1640995270, event_type: "database_query", duration: 200, status: "error", service: "db-service", attributes: [("query_type", "INSERT"), ("table", "products")] }
  ]
  
  // 计算请求成功率
  let calculate_success_rate = fn(events: Array[TelemetryEvent], service: String) {
    let service_events = events.filter(fn(e) { e.service == service })
    let total_requests = service_events.length()
    
    if total_requests > 0 {
      let successful_requests = service_events.filter(fn(e) { 
        e.status == "200" or e.status == "success" 
      }).length()
      
      (successful_requests * 100) / total_requests
    } else {
      0
    }
  }
  
  let api_success_rate = calculate_success_rate(events, "api-service")
  assert_eq(api_success_rate, 60)  // 3/5 = 60%
  
  let db_success_rate = calculate_success_rate(events, "db-service")
  assert_eq(db_success_rate, 66)  // 2/3 ≈ 66.67%，整数除法截断为66
  
  // 计算平均响应时间
  let calculate_average_duration = fn(events: Array[TelemetryEvent], service: String) {
    let service_events = events.filter(fn(e) { e.service == service })
    
    if service_events.length() > 0 {
      let total_duration = service_events.reduce(fn(acc, e) { acc + e.duration }, 0)
      total_duration / service_events.length()
    } else {
      0
    }
  }
  
  let api_avg_duration = calculate_average_duration(events, "api-service")
  assert_eq(api_avg_duration, 170)  // (120 + 150 + 300 + 80 + 200) / 5 = 170
  
  let db_avg_duration = calculate_average_duration(events, "db-service")
  assert_eq(db_avg_duration, 116)  // (50 + 100 + 200) / 3 ≈ 116.67，整数除法截断为116
  
  // 计算百分位数响应时间
  let calculate_percentile = fn(durations: Array[Int], percentile: Int) {
    if durations.length() > 0 {
      let sorted = durations.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
      let index = (sorted.length() * percentile) / 100
      if index >= sorted.length() {
        sorted[sorted.length() - 1]
      } else {
        sorted[index]
      }
    } else {
      0
    }
  }
  
  let api_durations = events
    .filter(fn(e) { e.service == "api-service" })
    .map(fn(e) { e.duration })
  
  let api_p95 = calculate_percentile(api_durations, 95)
  assert_eq(api_p95, 300)  // 95%的请求响应时间小于等于300ms
  
  let api_p50 = calculate_percentile(api_durations, 50)
  assert_eq(api_p50, 150)  // 中位数响应时间为150ms
  
  // 生成计算指标
  let generate_metrics = fn(events: Array[TelemetryEvent]) {
    let mut metrics = []
    
    // 按服务分组
    let services = ["api-service", "db-service"]
    
    for service in services {
      let service_events = events.filter(fn(e) { e.service == service })
      
      if service_events.length() > 0 {
        // 成功率指标
        let success_rate = calculate_success_rate(events, service)
        metrics = metrics + [{
          name: "success_rate",
          value: success_rate.to_float(),
          unit: "percent",
          timestamp: 1640995300,
          dimensions: [("service", service)]
        }]
        
        // 平均响应时间指标
        let avg_duration = calculate_average_duration(events, service)
        metrics = metrics + [{
          name: "average_duration",
          value: avg_duration.to_float(),
          unit: "ms",
          timestamp: 1640995300,
          dimensions: [("service", service)]
        }]
        
        // 请求计数指标
        metrics = metrics + [{
          name: "request_count",
          value: service_events.length().to_float(),
          unit: "count",
          timestamp: 1640995300,
          dimensions: [("service", service)]
        }]
      }
    }
    
    metrics
  }
  
  let calculated_metrics = generate_metrics(events)
  assert_eq(calculated_metrics.length(), 6)  // 每个服务3个指标
  
  // 验证API服务的指标
  let api_metrics = calculated_metrics.filter(fn(m) { 
    m.dimensions.contains(("service", "api-service")) 
  })
  
  assert_eq(api_metrics.length(), 3)
  
  let api_success_rate_metric = api_metrics.filter(fn(m) { m.name == "success_rate" })[0]
  assert_eq(api_success_rate_metric.value, 60.0)
  assert_eq(api_success_rate_metric.unit, "percent")
  
  let api_avg_duration_metric = api_metrics.filter(fn(m) { m.name == "average_duration" })[0]
  assert_eq(api_avg_duration_metric.value, 170.0)
  assert_eq(api_avg_duration_metric.unit, "ms")
  
  let api_request_count_metric = api_metrics.filter(fn(m) { m.name == "request_count" })[0]
  assert_eq(api_request_count_metric.value, 5.0)
  assert_eq(api_request_count_metric.unit, "count")
  
  // 验证DB服务的指标
  let db_metrics = calculated_metrics.filter(fn(m) { 
    m.dimensions.contains(("service", "db-service")) 
  })
  
  assert_eq(db_metrics.length(), 3)
  
  let db_success_rate_metric = db_metrics.filter(fn(m) { m.name == "success_rate" })[0]
  assert_eq(db_success_rate_metric.value, 66.0)
  
  let db_avg_duration_metric = db_metrics.filter(fn(m) { m.name == "average_duration" })[0]
  assert_eq(db_avg_duration_metric.value, 116.0)
  
  let db_request_count_metric = db_metrics.filter(fn(m) { m.name == "request_count" })[0]
  assert_eq(db_request_count_metric.value, 3.0)
}

// 测试6: 遥测数据的时间序列分析
test "telemetry time series analysis" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    labels: Array[(String, String)]
  }
  
  // 定义时间序列
  type TimeSeries = {
    name: String,
    points: Array[TimeSeriesPoint],
    aggregation: String
  }
  
  // 创建测试时间序列数据
  let cpu_series = {
    name: "cpu_usage",
    points: [
      { timestamp: 1640995200, value: 25.5, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995260, value: 30.1, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995320, value: 35.7, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995380, value: 28.3, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995440, value: 32.9, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995500, value: 40.2, labels: [("host", "server1"), ("region", "us-west")] }
    ],
    aggregation: "avg"
  }
  
  let memory_series = {
    name: "memory_usage",
    points: [
      { timestamp: 1640995200, value: 45.2, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995260, value: 50.8, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995320, value: 55.1, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995380, value: 48.7, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995440, value: 52.3, labels: [("host", "server1"), ("region", "us-west")] },
      { timestamp: 1640995500, value: 60.5, labels: [("host", "server1"), ("region", "us-west")] }
    ],
    aggregation: "avg"
  }
  
  // 计算时间序列的基本统计信息
  let calculate_series_stats = fn(series: TimeSeries) {
    let values = series.points.map(fn(p) { p.value })
    
    if values.length() > 0 {
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / values.length().to_float()
      
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      
      // 计算标准差
      let variance = values.reduce(fn(acc, v) { 
        let diff = v - avg
        acc + (diff * diff)
      }, 0.0) / values.length().to_float()
      
      let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
      
      {
        count: values.length(),
        average: avg,
        maximum: max,
        minimum: min,
        standard_deviation: std_dev,
        sum: sum
      }
    } else {
      {
        count: 0,
        average: 0.0,
        maximum: 0.0,
        minimum: 0.0,
        standard_deviation: 0.0,
        sum: 0.0
      }
    }
  }
  
  // 计算CPU序列统计
  let cpu_stats = calculate_series_stats(cpu_series)
  assert_eq(cpu_stats.count, 6)
  assert_eq(cpu_stats.maximum, 40.2)
  assert_eq(cpu_stats.minimum, 25.5)
  assert_true(cpu_stats.average > 30.0 && cpu_stats.average < 33.0)  // (25.5 + 30.1 + 35.7 + 28.3 + 32.9 + 40.2) / 6 ≈ 32.12
  
  // 计算内存序列统计
  let memory_stats = calculate_series_stats(memory_series)
  assert_eq(memory_stats.count, 6)
  assert_eq(memory_stats.maximum, 60.5)
  assert_eq(memory_stats.minimum, 45.2)
  assert_true(memory_stats.average > 50.0 && memory_stats.average < 53.0)  // (45.2 + 50.8 + 55.1 + 48.7 + 52.3 + 60.5) / 6 ≈ 52.1
  
  // 时间序列重采样 - 将数据聚合到更大的时间窗口
  let resample_series = fn(series: TimeSeries, window_size: Int) {
    let mut resampled_points = []
    
    if series.points.length() > 0 {
      let start_time = series.points[0].timestamp
      let end_time = series.points[series.points.length() - 1].timestamp
      
      let mut current_start = start_time
      
      while current_start < end_time {
        let current_end = current_start + window_size
        
        // 获取当前窗口内的点
        let window_points = series.points.filter(fn(p) { 
          p.timestamp >= current_start && p.timestamp < current_end 
        })
        
        if window_points.length() > 0 {
          let values = window_points.map(fn(p) { p.value })
          let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
          let avg = sum / values.length().to_float()
          
          resampled_points = resampled_points + [{
            timestamp: current_start + (window_size / 2),  // 窗口中点
            value: avg,
            labels: series.points[0].labels  // 使用第一个点的标签
          }]
        }
        
        current_start = current_end
      }
    }
    
    {
      name: series.name,
      points: resampled_points,
      aggregation: series.aggregation + "_resampled"
    }
  }
  
  // 重采样CPU数据到10分钟窗口
  let resampled_cpu = resample_series(cpu_series, 600)  // 10分钟 = 600秒
  assert_eq(resampled_cpu.points.length(), 5)  // 原始6个点，重采样后应该有5个窗口
  
  // 验证第一个重采样点
  let first_resampled = resampled_cpu.points[0]
  assert_eq(first_resampled.timestamp, 1640995500)  // 1640995200 + 300
  assert_eq(first_resampled.labels, [("host", "server1"), ("region", "us-west")])
  
  // 时间序列趋势分析 - 简单线性回归
  let calculate_trend = fn(series: TimeSeries) {
    if series.points.length() >= 2 {
      let n = series.points.length().to_float()
      let first_time = series.points[0].timestamp.to_float()
      
      // 计算斜率和截距
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for point in series.points {
        let x = (point.timestamp - first_time).to_float()  // 相对时间
        let y = point.value
        
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + (x * y)
        sum_x2 = sum_x2 + (x * x)
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // 计算相关系数
      let mean_x = sum_x / n
      let mean_y = sum_y / n
      
      let mut numerator = 0.0
      let mut sum_x_diff_sq = 0.0
      let mut sum_y_diff_sq = 0.0
      
      for point in series.points {
        let x = (point.timestamp - first_time).to_float()
        let y = point.value
        
        numerator = numerator + ((x - mean_x) * (y - mean_y))
        sum_x_diff_sq = sum_x_diff_sq + ((x - mean_x) * (x - mean_x))
        sum_y_diff_sq = sum_y_diff_sq + ((y - mean_y) * (y - mean_y))
      }
      
      let correlation = if sum_x_diff_sq > 0.0 && sum_y_diff_sq > 0.0 {
        numerator / (sum_x_diff_sq * sum_y_diff_sq).sqrt()
      } else {
        0.0
      }
      
      {
        slope,
        intercept,
        correlation,
        trend: if slope > 0.01 { "increasing" } else if slope < -0.01 { "decreasing" } else { "stable" }
      }
    } else {
      {
        slope: 0.0,
        intercept: 0.0,
        correlation: 0.0,
        trend: "insufficient_data"
      }
    }
  }
  
  // 分析CPU趋势
  let cpu_trend = calculate_trend(cpu_series)
  assert_eq(cpu_trend.trend, "increasing")  // CPU使用率整体呈上升趋势
  assert_true(cpu_trend.slope > 0.0)
  assert_true(cpu_trend.correlation > 0.5)  // 应该有一定的正相关性
  
  // 分析内存趋势
  let memory_trend = calculate_trend(memory_series)
  assert_eq(memory_trend.trend, "increasing")  // 内存使用率整体呈上升趋势
  assert_true(memory_trend.slope > 0.0)
  assert_true(memory_trend.correlation > 0.5)  // 应该有一定的正相关性
  
  // 时间序列异常检测 - 基于标准差的简单方法
  let detect_anomalies = fn(series: TimeSeries, threshold: Float) {
    let stats = calculate_series_stats(series)
    let mut anomalies = []
    
    for point in series.points {
      let z_score = if stats.standard_deviation > 0.0 {
        (point.value - stats.average) / stats.standard_deviation
      } else {
        0.0
      }
      
      if z_score.abs() > threshold {
        anomalies = anomalies + [{
          timestamp: point.timestamp,
          value: point.value,
          z_score: z_score,
          labels: point.labels
        }]
      }
    }
    
    anomalies
  }
  
  // 检测CPU异常（使用2个标准差作为阈值）
  let cpu_anomalies = detect_anomalies(cpu_series, 2.0)
  // 由于数据变化范围较大，可能会有一些异常点
  assert_eq(cpu_anomalies.length(), 0)  // 在这个测试数据中，没有超过2个标准差的点
  
  // 检测内存异常（使用1.5个标准差作为阈值）
  let memory_anomalies = detect_anomalies(memory_series, 1.5)
  // 由于数据变化范围较大，可能会有一些异常点
  assert_eq(memory_anomalies.length(), 1)  // 最后一个点60.5可能被视为异常
  
  match memory_anomalies[0] {
    anomaly => {
      assert_eq(anomaly.timestamp, 1640995500)
      assert_eq(anomaly.value, 60.5)
      assert_true(anomaly.z_score > 1.5)
    }
  }
}

// 测试7: 遥测数据的关联分析
test "telemetry data correlation analysis" {
  // 定义关联事件
  type CorrelatedEvent = {
    id: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    event_type: String,
    timestamp: Int,
    duration: Option[Int],
    attributes: Array[(String, String)]
  }
  
  // 定义关联关系
  type Correlation = {
    source_event_id: String,
    target_event_id: String,
    correlation_type: String,
    strength: Float
  }
  
  // 创建测试事件数据
  let events = [
    { id: "event-001", trace_id: "trace-001", span_id: "span-001", parent_span_id: None, event_type: "http_request", timestamp: 1640995200, duration: Some(120), attributes: [("endpoint", "/api/users"), ("method", "GET")] },
    { id: "event-002", trace_id: "trace-001", span_id: "span-002", parent_span_id: Some("span-001"), event_type: "database_query", timestamp: 1640995210, duration: Some(50), attributes: [("query", "SELECT * FROM users")] },
    { id: "event-003", trace_id: "trace-001", span_id: "span-003", parent_span_id: Some("span-001"), event_type: "cache_lookup", timestamp: 1640995220, duration: Some(10), attributes: [("cache_key", "user:123")] },
    { id: "event-004", trace_id: "trace-002", span_id: "span-004", parent_span_id: None, event_type: "http_request", timestamp: 1640995230, duration: Some(300), attributes: [("endpoint", "/api/orders"), ("method", "POST")] },
    { id: "event-005", trace_id: "trace-002", span_id: "span-005", parent_span_id: Some("span-004"), event_type: "database_query", timestamp: 1640995240, duration: Some(200), attributes: [("query", "INSERT INTO orders")] },
    { id: "event-006", trace_id: "trace-002", span_id: "span-006", parent_span_id: Some("span-004"), event_type: "auth_check", timestamp: 1640995250, duration: Some(30), attributes: [("user_id", "user-456")] },
    { id: "event-007", trace_id: "trace-003", span_id: "span-007", parent_span_id: None, event_type: "http_request", timestamp: 1640995260, duration: Some(80), attributes: [("endpoint", "/api/products"), ("method", "GET")] },
    { id: "event-008", trace_id: "trace-003", span_id: "span-008", parent_span_id: Some("span-007"), event_type: "cache_lookup", timestamp: 1640995270, duration: Some(5), attributes: [("cache_key", "product:789")] }
  ]
  
  // 构建事件关联图
  let build_correlation_graph = fn(events: Array[CorrelatedEvent]) {
    let mut correlations = []
    
    // 基于父子关系构建关联
    for event in events {
      match event.parent_span_id {
        Some(parent_span_id) => {
          // 查找父事件
          let parent_events = events.filter(fn(e) { 
            e.span_id == parent_span_id && e.trace_id == event.trace_id 
          })
          
          for parent in parent_events {
            correlations = correlations + [{
              source_event_id: parent.id,
              target_event_id: event.id,
              correlation_type: "parent_child",
              strength: 1.0
            }]
          }
        }
        None => ()
      }
    }
    
    // 基于时间序列构建关联
    for i in 0..events.length() {
      for j in (i + 1)..events.length() {
        let event1 = events[i]
        let event2 = events[j]
        
        // 如果事件在同一跟踪中且时间相近，可能存在关联
        if event1.trace_id == event2.trace_id {
          let time_diff = (event2.timestamp - event1.timestamp).abs()
          
          // 如果时间差小于5秒，可能存在关联
          if time_diff < 5000 {
            let strength = 1.0 - (time_diff.to_float() / 5000.0)  // 时间越近，关联越强
            
            correlations = correlations + [{
              source_event_id: event1.id,
              target_event_id: event2.id,
              correlation_type: "temporal",
              strength: strength
            }]
          }
        }
      }
    }
    
    correlations
  }
  
  let correlations = build_correlation_graph(events)
  
  // 验证父子关联
  let parent_child_correlations = correlations.filter(fn(c) { c.correlation_type == "parent_child" })
  assert_eq(parent_child_correlations.length(), 5)  // event-002->event-001, event-003->event-001, event-005->event-004, event-006->event-004, event-008->event-007
  
  // 验证时间关联
  let temporal_correlations = correlations.filter(fn(c) { c.correlation_type == "temporal" })
  assert_true(temporal_correlations.length() > 0)
  
  // 分析事件链
  let trace_event_chains = fn(events: Array[CorrelatedEvent]) {
    let mut chains = []
    let processed_traces = []
    
    for event in events {
      if not(processed_traces.contains(event.trace_id)) {
        processed_traces = processed_traces + [event.trace_id]
        
        // 获取同一跟踪中的所有事件
        let trace_events = events.filter(fn(e) { e.trace_id == event.trace_id })
        
        // 按时间排序
        let sorted_events = trace_events.sort(fn(a, b) { 
          if a.timestamp < b.timestamp { -1 } 
          else if a.timestamp > b.timestamp { 1 } 
          else { 0 } 
        })
        
        chains = chains + [(event.trace_id, sorted_events)]
      }
    }
    
    chains
  }
  
  let event_chains = trace_event_chains(events)
  assert_eq(event_chains.length(), 3)  // 3个不同的跟踪
  
  // 验证第一个跟踪链
  let trace1_chain = event_chains.filter(fn(chain) {
    match chain {
      (trace_id, _) => trace_id == "trace-001"
    }
  })[0]
  
  match trace1_chain {
    (_, events) => {
      assert_eq(events.length(), 3)
      assert_eq(events[0].event_type, "http_request")  // 第一个事件应该是HTTP请求
      assert_eq(events[0].span_id, "span-001")
      assert_eq(events[1].event_type, "database_query")  // 第二个事件应该是数据库查询
      assert_eq(events[1].span_id, "span-002")
      assert_eq(events[2].event_type, "cache_lookup")  // 第三个事件应该是缓存查找
      assert_eq(events[2].span_id, "span-003")
    }
  }
  
  // 计算跟踪链的总持续时间
  let calculate_trace_duration = fn(events: Array[CorrelatedEvent]) {
    let mut durations = []
    
    for chain in event_chains {
      match chain {
        (trace_id, trace_events) => {
          if trace_events.length() > 0 {
            let start_time = trace_events[0].timestamp
            let end_time = trace_events[trace_events.length() - 1].timestamp
            
            // 计算实际执行时间
            let mut total_duration = 0
            for event in trace_events {
              match event.duration {
                Some(d) => total_duration = total_duration + d
                None => ()
              }
            }
            
            durations = durations + [(trace_id, end_time - start_time, total_duration)]
          }
        }
      }
    }
    
    durations
  }
  
  let trace_durations = calculate_trace_duration(events)
  assert_eq(trace_durations.length(), 3)
  
  // 验证第一个跟踪的持续时间
  let trace1_duration = trace_durations.filter(fn(d) {
    match d {
      (trace_id, _, _) => trace_id == "trace-001"
    }
  })[0]
  
  match trace1_duration {
    (_, wall_time, cpu_time) => {
      assert_eq(wall_time, 70)  // 1640995270 - 1640995200 = 70
      assert_eq(cpu_time, 180)  // 120 + 50 + 10 = 180
    }
  }
  
  // 分析事件模式
  let analyze_event_patterns = fn(events: Array[CorrelatedEvent]) {
    let mut event_types = []
    let mut type_counts = []
    
    // 统计事件类型
    for event in events {
      if not(event_types.contains(event.event_type)) {
        event_types = event_types + [event.event_type]
      }
    }
    
    for event_type in event_types {
      let count = events.filter(fn(e) { e.event_type == event_type }).length()
      type_counts = type_counts + [(event_type, count)]
    }
    
    // 分析事件序列模式
    let mut sequences = []
    
    for chain in event_chains {
      match chain {
        (_, trace_events) => {
          if trace_events.length() >= 2 {
            let mut sequence = ""
            for event in trace_events {
              sequence = sequence + event.event_type + "->"
            }
            // 移除最后的"->"
            sequence = sequence.substring(0, sequence.length() - 2)
            sequences = sequences + [sequence]
          }
        }
      }
    }
    
    {
      type_distribution: type_counts,
      common_sequences: sequences
    }
  }
  
  let pattern_analysis = analyze_event_patterns(events)
  
  // 验证事件类型分布
  assert_eq(pattern_analysis.type_distribution.length(), 3)
  assert_true(pattern_analysis.type_distribution.contains(("http_request", 3)))
  assert_true(pattern_analysis.type_distribution.contains(("database_query", 2)))
  assert_true(pattern_analysis.type_distribution.contains(("cache_lookup", 2)))
  assert_true(pattern_analysis.type_distribution.contains(("auth_check", 1)))
  
  // 验证常见序列
  assert_eq(pattern_analysis.common_sequences.length(), 3)
  assert_true(pattern_analysis.common_sequences.contains("http_request->database_query->cache_lookup"))
  assert_true(pattern_analysis.common_sequences.contains("http_request->database_query->auth_check"))
  assert_true(pattern_analysis.common_sequences.contains("http_request->cache_lookup"))
}

// 测试8: 遥测数据的异常检测与告警
test "telemetry anomaly detection and alerting" {
  // 定义告警级别
  type AlertSeverity = 
    | Info
    | Warning
    | Error
    | Critical
  
  // 定义告警
  type Alert = {
    id: String,
    severity: AlertSeverity,
    title: String,
    description: String,
    timestamp: Int,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // 定义异常检测结果
  type AnomalyDetectionResult = {
    is_anomaly: Bool,
    confidence: Float,
    anomaly_type: String,
    details: Array[(String, String)]
  }
  
  // 定义指标数据点
  type MetricDataPoint = {
    metric_name: String,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 创建测试指标数据
  let metric_data = [
    { metric_name: "cpu_usage", value: 25.5, timestamp: 1640995200, labels: [("host", "server1")] },
    { metric_name: "cpu_usage", value: 28.3, timestamp: 1640995260, labels: [("host", "server1")] },
    { metric_name: "cpu_usage", value: 30.1, timestamp: 1640995320, labels: [("host", "server1")] },
    { metric_name: "cpu_usage", value: 95.2, timestamp: 1640995380, labels: [("host", "server1")] },  // 异常值
    { metric_name: "cpu_usage", value: 32.9, timestamp: 1640995440, labels: [("host", "server1")] },
    { metric_name: "cpu_usage", value: 35.7, timestamp: 1640995500, labels: [("host", "server1")] },
    { metric_name: "memory_usage", value: 45.2, timestamp: 1640995200, labels: [("host", "server1")] },
    { metric_name: "memory_usage", value: 48.7, timestamp: 1640995260, labels: [("host", "server1")] },
    { metric_name: "memory_usage", value: 52.3, timestamp: 1640995320, labels: [("host", "server1")] },
    { metric_name: "memory_usage", value: 98.5, timestamp: 1640995380, labels: [("host", "server1")] },  // 异常值
    { metric_name: "memory_usage", value: 55.1, timestamp: 1640995440, labels: [("host", "server1")] },
    { metric_name: "memory_usage", value: 60.8, timestamp: 1640995500, labels: [("host", "server1")] },
    { metric_name: "error_rate", value: 0.1, timestamp: 1640995200, labels: [("service", "api-service")] },
    { metric_name: "error_rate", value: 0.2, timestamp: 1640995260, labels: [("service", "api-service")] },
    { metric_name: "error_rate", value: 0.15, timestamp: 1640995320, labels: [("service", "api-service")] },
    { metric_name: "error_rate", value: 25.5, timestamp: 1640995380, labels: [("service", "api-service")] },  // 异常值
    { metric_name: "error_rate", value: 0.3, timestamp: 1640995440, labels: [("service", "api-service")] },
    { metric_name: "error_rate", value: 0.25, timestamp: 1640995500, labels: [("service", "api-service")] }
  ]
  
  // 基于统计的异常检测
  let statistical_anomaly_detection = fn(data_points: Array[MetricDataPoint], threshold: Float) {
    if data_points.length() < 3 {
      return {
        is_anomaly: false,
        confidence: 0.0,
        anomaly_type: "insufficient_data",
        details: []
      }
    }
    
    let values = data_points.map(fn(p) { p.value })
    
    // 计算基本统计量
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let mean = sum / values.length().to_float()
    
    let mut variance = 0.0
    for value in values {
      let diff = value - mean
      variance = variance + (diff * diff)
    }
    variance = variance / values.length().to_float()
    
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    // 检查最新数据点是否异常
    let latest_point = data_points[data_points.length() - 1]
    let z_score = if std_dev > 0.0 {
      (latest_point.value - mean) / std_dev
    } else {
      0.0
    }
    
    let is_anomaly = z_score.abs() > threshold
    let confidence = if std_dev > 0.0 { z_score.abs() / threshold } else { 0.0 }
    
    {
      is_anomaly,
      confidence: if confidence > 1.0 { 1.0 } else { confidence },
      anomaly_type: "statistical_outlier",
      details: [
        ("metric_name", latest_point.metric_name),
        ("value", latest_point.value.to_string()),
        ("mean", mean.to_string()),
        ("std_dev", std_dev.to_string()),
        ("z_score", z_score.to_string())
      ]
    }
  }
  
  // 基于阈值的异常检测
  let threshold_anomaly_detection = fn(data_point: MetricDataPoint, thresholds: Array[(String, Float)]) {
    let mut is_anomaly = false
    let mut severity = "info"
    let mut matched_threshold = 0.0
    
    for threshold in thresholds {
      match threshold {
        (metric_name, value) => {
          if data_point.metric_name == metric_name {
            if data_point.value > value {
              is_anomaly = true
              matched_threshold = value
              
              // 确定严重程度
              if metric_name == "cpu_usage" {
                if data_point.value > 90.0 {
                  severity = "critical"
                } else if data_point.value > 80.0 {
                  severity = "error"
                } else if data_point.value > 70.0 {
                  severity = "warning"
                }
              } else if metric_name == "memory_usage" {
                if data_point.value > 95.0 {
                  severity = "critical"
                } else if data_point.value > 85.0 {
                  severity = "error"
                } else if data_point.value > 75.0 {
                  severity = "warning"
                }
              } else if metric_name == "error_rate" {
                if data_point.value > 10.0 {
                  severity = "critical"
                } else if data_point.value > 5.0 {
                  severity = "error"
                } else if data_point.value > 1.0 {
                  severity = "warning"
                }
              }
            }
          }
        }
      }
    }
    
    {
      is_anomaly,
      confidence: if is_anomaly { 1.0 } else { 0.0 },
      anomaly_type: "threshold_breach",
      details: [
        ("metric_name", data_point.metric_name),
        ("value", data_point.value.to_string()),
        ("threshold", matched_threshold.to_string()),
        ("severity", severity)
      ]
    }
  }
  
  // 生成告警
  let generate_alert = fn(detection_result: AnomalyDetectionResult, data_point: MetricDataPoint) {
    if detection_result.is_anomaly {
      let alert_id = "alert-" + data_point.timestamp.to_string()
      
      let severity = match detection_result.anomaly_type {
        "statistical_outlier" => {
          if detection_result.confidence > 0.8 {
            Critical
          } else if detection_result.confidence > 0.5 {
            Error
          } else {
            Warning
          }
        }
        "threshold_breach" => {
          match detection_result.details.filter(fn(d) { match d { (k, _) => k == "severity" } })[0] {
            (_, severity_str) => {
              match severity_str {
                "critical" => Critical,
                "error" => Error,
                "warning" => Warning,
                _ => Info
              }
            }
          }
        }
        _ => Warning
      }
      
      let title = data_point.metric_name + " anomaly detected"
      let description = "Anomaly detected in " + data_point.metric_name + " with value " + data_point.value.to_string()
      
      Some({
        id: alert_id,
        severity,
        title,
        description,
        timestamp: data_point.timestamp,
        source: data_point.metric_name,
        attributes: data_point.labels + detection_result.details
      })
    } else {
      None
    }
  }
  
  // 测试统计异常检测
  let cpu_data = metric_data.filter(fn(p) { p.metric_name == "cpu_usage" })
  let cpu_anomaly_result = statistical_anomaly_detection(cpu_data, 2.0)
  
  assert_true(cpu_anomaly_result.is_anomaly)  // CPU使用率95.2%应该被检测为异常
  assert_true(cpu_anomaly_result.confidence > 0.8)  // 高置信度
  
  // 测试阈值异常检测
  let latest_cpu_point = cpu_data[cpu_data.length() - 1]
  let cpu_thresholds = [("cpu_usage", 80.0), ("memory_usage", 85.0), ("error_rate", 5.0)]
  let cpu_threshold_result = threshold_anomaly_detection(latest_cpu_point, cpu_thresholds)
  
  assert_true(cpu_threshold_result.is_anomaly)  // CPU使用率35.7%应该不会触发阈值告警
  assert_eq(cpu_threshold_result.confidence, 0.0)  // 不应该触发阈值告警
  
  // 测试异常CPU使用率点
  let high_cpu_point = { metric_name: "cpu_usage", value: 95.2, timestamp: 1640995380, labels: [("host", "server1")] }
  let high_cpu_threshold_result = threshold_anomaly_detection(high_cpu_point, cpu_thresholds)
  
  assert_true(high_cpu_threshold_result.is_anomaly)  // CPU使用率95.2%应该触发阈值告警
  assert_eq(high_cpu_threshold_result.confidence, 1.0)  // 高置信度
  
  // 生成告警
  let cpu_alert = generate_alert(cpu_anomaly_result, high_cpu_point)
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, Critical)  // 高CPU使用率应该是严重告警
      assert_eq(alert.title, "cpu_usage anomaly detected")
      assert_true(alert.description.contains("95.2"))
      assert_eq(alert.source, "cpu_usage")
    }
    None => assert_true(false)
  }
  
  // 测试内存异常检测
  let memory_data = metric_data.filter(fn(p) { p.metric_name == "memory_usage" })
  let memory_anomaly_result = statistical_anomaly_detection(memory_data, 2.0)
  
  assert_true(memory_anomaly_result.is_anomaly)  // 内存使用率98.5%应该被检测为异常
  assert_true(memory_anomaly_result.confidence > 0.8)  // 高置信度
  
  // 测试错误率异常检测
  let error_data = metric_data.filter(fn(p) { p.metric_name == "error_rate" })
  let error_anomaly_result = statistical_anomaly_detection(error_data, 2.0)
  
  assert_true(error_anomaly_result.is_anomaly)  // 错误率25.5%应该被检测为异常
  assert_true(error_anomaly_result.confidence > 0.8)  // 高置信度
  
  // 批量异常检测
  let batch_anomaly_detection = fn(all_data: Array[MetricDataPoint]) {
    let mut alerts = []
    let metric_names = ["cpu_usage", "memory_usage", "error_rate"]
    
    for metric_name in metric_names {
      let metric_data = all_data.filter(fn(p) { p.metric_name == metric_name })
      
      if metric_data.length() >= 3 {
        // 统计异常检测
        let stat_result = statistical_anomaly_detection(metric_data, 2.0)
        
        // 阈值异常检测
        let thresholds = [
          ("cpu_usage", 80.0),
          ("memory_usage", 85.0),
          ("error_rate", 5.0)
        ]
        
        let latest_point = metric_data[metric_data.length() - 1]
        let threshold_result = threshold_anomaly_detection(latest_point, thresholds)
        
        // 生成告警
        match generate_alert(stat_result, latest_point) {
          Some(alert) => alerts = alerts + [alert]
          None => ()
        }
        
        match generate_alert(threshold_result, latest_point) {
          Some(alert) => alerts = alerts + [alert]
          None => ()
        }
      }
    }
    
    alerts
  }
  
  let batch_alerts = batch_anomaly_detection(metric_data)
  assert_true(batch_alerts.length() >= 3)  // 至少应该有3个告警（CPU、内存、错误率）
  
  // 验证告警严重程度
  let critical_alerts = batch_alerts.filter(fn(alert) { 
    match alert.severity { Critical => true, _ => false }
  })
  assert_true(critical_alerts.length() >= 2)  // 至少应该有2个严重告警（CPU和内存）
  
  // 验证告警属性
  for alert in batch_alerts {
    assert_true(alert.id.starts_with("alert-"))
    assert_true(alert.title.contains("anomaly detected"))
    assert_true(alert.description.contains("value"))
    assert_true(alert.attributes.length() > 0)
  }
}

// 测试9: 遥测数据的实时流处理
test "telemetry real-time stream processing" {
  // 定义流事件
  type StreamEvent = {
    id: String,
    timestamp: Int,
    event_type: String,
    data: Array[(String, String)]
  }
  
  // 定义流窗口
  type StreamWindow = {
    start_time: Int,
    end_time: Int,
    events: Array[StreamEvent]
  }
  
  // 定义流处理结果
  type StreamResult = {
    window_start: Int,
    window_end: Int,
    metrics: Array[(String, Float)],
    alerts: Array[String]
  }
  
  // 创建测试流事件
  let stream_events = [
    { id: "event-001", timestamp: 1640995200, event_type: "request", data: [("service", "api"), ("status", "200"), ("duration", "120")] },
    { id: "event-002", timestamp: 1640995210, event_type: "request", data: [("service", "api"), ("status", "200"), ("duration", "150")] },
    { id: "event-003", timestamp: 1640995220, event_type: "error", data: [("service", "db"), ("error_type", "timeout")] },
    { id: "event-004", timestamp: 1640995230, event_type: "request", data: [("service", "api"), ("status", "500"), ("duration", "300")] },
    { id: "event-005", timestamp: 1640995240, event_type: "request", data: [("service", "cache"), ("status", "200"), ("duration", "10")] },
    { id: "event-006", timestamp: 1640995250, event_type: "request", data: [("service", "api"), ("status", "200"), ("duration", "80")] },
    { id: "event-007", timestamp: 1640995260, event_type: "error", data: [("service", "db"), ("error_type", "connection")] },
    { id: "event-008", timestamp: 1640995270, event_type: "request", data: [("service", "api"), ("status", "200"), ("duration", "100")] },
    { id: "event-009", timestamp: 1640995280, event_type: "request", data: [("service", "cache"), ("status", "200"), ("duration", "5")] },
    { id: "event-010", timestamp: 1640995290, event_type: "request", data: [("service", "api"), ("status", "200"), ("duration", "90")] }
  ]
  
  // 创建时间窗口
  let create_time_windows = fn(events: Array[StreamEvent], window_size: Int) {
    let mut windows = []
    
    if events.length() > 0 {
      let start_time = events[0].timestamp
      let end_time = events[events.length() - 1].timestamp
      
      let mut window_start = start_time
      
      while window_start <= end_time {
        let window_end = window_start + window_size
        
        // 获取窗口内的事件
        let window_events = events.filter(fn(event) {
          event.timestamp >= window_start && event.timestamp < window_end
        })
        
        if window_events.length() > 0 {
          windows = windows + [{
            start_time: window_start,
            end_time: window_end,
            events: window_events
          }]
        }
        
        window_start = window_end
      }
    }
    
    windows
  }
  
  // 创建60秒的时间窗口
  let time_windows = create_time_windows(stream_events, 60)
  assert_eq(time_windows.length(), 2)  // 10个事件在2分钟内，60秒窗口应该有2个
  
  // 处理时间窗口
  let process_window = fn(window: StreamWindow) {
    let mut metrics = []
    let mut alerts = []
    
    // 统计请求总数
    let request_events = window.events.filter(fn(event) { event.event_type == "request" })
    metrics = metrics + [("request_count", request_events.length().to_float())]
    
    // 统计错误数
    let error_events = window.events.filter(fn(event) { event.event_type == "error" })
    metrics = metrics + [("error_count", error_events.length().to_float())]
    
    // 计算错误率
    let total_events = window.events.length()
    if total_events > 0 {
      let error_rate = (error_events.length() * 100) / total_events
      metrics = metrics + [("error_rate", error_rate.to_float())]
    } else {
      metrics = metrics + [("error_rate", 0.0)]
    }
    
    // 计算平均响应时间
    let durations = request_events.map(fn(event) {
      let duration_item = event.data.filter(fn(item) {
        match item {
          (key, _) => key == "duration"
        }
      })
      
      if duration_item.length() > 0 {
        match duration_item[0] {
          (_, value) => value.to_int()
        }
      } else {
        0
      }
    })
    
    if durations.length() > 0 {
      let total_duration = durations.reduce(fn(acc, duration) { acc + duration }, 0)
      let avg_duration = total_duration / durations.length()
      metrics = metrics + [("avg_response_time", avg_duration.to_float())]
    } else {
      metrics = metrics + [("avg_response_time", 0.0)]
    }
    
    // 检查告警条件
    if error_events.length() > 0 {
      alerts = alerts + ["High error rate detected: " + error_events.length().to_string() + " errors"]
    }
    
    if durations.length() > 0 {
      let total_duration = durations.reduce(fn(acc, duration) { acc + duration }, 0)
      let avg_duration = total_duration / durations.length()
      
      if avg_duration > 200 {
        alerts = alerts + ["High response time detected: " + avg_duration.to_string() + "ms"]
      }
    }
    
    // 统计各服务的请求数
    let services = ["api", "db", "cache"]
    
    for service in services {
      let service_requests = request_events.filter(fn(event) {
        let service_item = event.data.filter(fn(item) {
          match item {
            (key, value) => key == "service" && value == service
          }
        })
        service_item.length() > 0
      })
      
      metrics = metrics + [("requests_" + service, service_requests.length().to_float())]
    }
    
    {
      window_start: window.start_time,
      window_end: window.end_time,
      metrics,
      alerts
    }
  }
  
  // 处理所有时间窗口
  let processed_windows = time_windows.map(process_window)
  assert_eq(processed_windows.length(), 2)
  
  // 验证第一个窗口的处理结果
  let first_window = processed_windows[0]
  assert_eq(first_window.window_start, 1640995200)
  assert_eq(first_window.window_end, 1640995260)
  
  // 第一个窗口应该包含前6个事件
  assert_eq(first_window.metrics.filter(fn(m) { match m { (k, _) => k == "request_count" } })[0].1, 5.0)
  assert_eq(first_window.metrics.filter(fn(m) { match m { (k, _) => k == "error_count" } })[0].1, 1.0)
  
  // 验证第二个窗口的处理结果
  let second_window = processed_windows[1]
  assert_eq(second_window.window_start, 1640995260)
  assert_eq(second_window.window_end, 1640995320)
  
  // 第二个窗口应该包含后4个事件
  assert_eq(second_window.metrics.filter(fn(m) { match m { (k, _) => k == "request_count" } })[0].1, 3.0)
  assert_eq(second_window.metrics.filter(fn(m) { match m { (k, _) => k == "error_count" } })[0].1, 1.0)
  
  // 实时流处理模拟
  let real_time_stream_processing = fn(events: Array[StreamEvent], window_size: Int, slide_interval: Int) {
    let mut results = []
    
    if events.length() > 0 {
      let start_time = events[0].timestamp
      let end_time = events[events.length() - 1].timestamp
      
      let mut window_start = start_time
      
      while window_start + window_size <= end_time {
        let window_end = window_start + window_size
        
        // 获取窗口内的事件
        let window_events = events.filter(fn(event) {
          event.timestamp >= window_start && event.timestamp < window_end
        })
        
        if window_events.length() > 0 {
          let window = {
            start_time: window_start,
            end_time: window_end,
            events: window_events
          }
          
          let result = process_window(window)
          results = results + [result]
        }
        
        window_start = window_start + slide_interval
      }
    }
    
    results
  }
  
  // 使用滑动窗口进行实时处理
  let sliding_window_results = real_time_stream_processing(stream_events, 60, 30)
  assert_true(sliding_window_results.length() > 2)  // 滑动窗口应该产生更多结果
  
  // 流聚合
  let stream_aggregation = fn(results: Array[StreamResult]) {
    if results.length() > 0 {
      let mut total_requests = 0.0
      let mut total_errors = 0.0
      let mut total_response_time = 0.0
      let mut response_time_count = 0
      let mut all_alerts = []
      
      for result in results {
        for metric in result.metrics {
          match metric {
            (name, value) => {
              if name == "request_count" {
                total_requests = total_requests + value
              } else if name == "error_count" {
                total_errors = total_errors + value
              } else if name == "avg_response_time" {
                total_response_time = total_response_time + value
                response_time_count = response_time_count + 1
              }
            }
          }
        }
        
        all_alerts = all_alerts + result.alerts
      }
      
      let overall_error_rate = if total_requests > 0.0 {
        (total_errors * 100.0) / total_requests
      } else {
        0.0
      }
      
      let overall_avg_response_time = if response_time_count > 0 {
        total_response_time / response_time_count.to_float()
      } else {
        0.0
      }
      
      {
        total_requests,
        total_errors,
        overall_error_rate,
        overall_avg_response_time,
        unique_alerts: all_alerts
      }
    } else {
      {
        total_requests: 0.0,
        total_errors: 0.0,
        overall_error_rate: 0.0,
        overall_avg_response_time: 0.0,
        unique_alerts: []
      }
    }
  }
  
  let aggregation_result = stream_aggregation(processed_windows)
  assert_eq(aggregation_result.total_requests, 8.0)  // 总共8个请求
  assert_eq(aggregation_result.total_errors, 2.0)   // 总共2个错误
  assert_eq(aggregation_result.overall_error_rate, 25.0)  // 错误率25%
  
  // 验证平均响应时间
  let expected_avg_response_time = (120.0 + 150.0 + 300.0 + 10.0 + 80.0 + 100.0 + 5.0 + 90.0) / 8.0
  assert_eq(aggregation_result.overall_avg_response_time, expected_avg_response_time)
  
  // 验证告警
  assert_true(aggregation_result.unique_alerts.length() > 0)
  assert_true(aggregation_result.unique_alerts.contains("High error rate detected: 1 errors"))
  assert_true(aggregation_result.unique_alerts.contains("High response time detected: 300ms"))
}

// 测试10: 遥测数据的智能分析与预测
test "telemetry intelligent analysis and prediction" {
  // 定义趋势分析结果
  type TrendAnalysisResult = {
    trend: String,
    slope: Float,
    confidence: Float,
    prediction: Array[(Int, Float)]  // (timestamp, predicted_value)
  }
  
  // 定义异常模式
  type AnomalyPattern = {
    pattern_type: String,
    frequency: Int,
    severity: String,
    description: String
  }
  
  // 定义预测模型
  type PredictionModel = 
    | LinearRegression
    | MovingAverage(Int)  // window size
    | ExponentialSmoothing(Float)  // alpha value
    | SeasonalDecomposition(Int)  // season length
  
  // 定义时间序列数据点
  type TimeSeriesDataPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // 创建测试时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 25.5, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995260, value: 28.3, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995320, value: 30.1, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995380, value: 32.9, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995440, value: 35.7, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995500, value: 38.2, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995560, value: 40.5, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995620, value: 42.8, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995680, value: 45.1, metadata: [("metric", "cpu_usage")] },
    { timestamp: 1640995740, value: 47.6, metadata: [("metric", "cpu_usage")] }
  ]
  
  // 线性回归预测
  let linear_regression_prediction = fn(data: Array[TimeSeriesDataPoint], steps: Int) {
    if data.length() < 2 {
      return {
        trend: "insufficient_data",
        slope: 0.0,
        confidence: 0.0,
        prediction: []
      }
    }
    
    let n = data.length().to_float()
    let first_timestamp = data[0].timestamp.to_float()
    
    // 计算线性回归参数
    let mut sum_x = 0.0
    let mut sum_y = 0.0
    let mut sum_xy = 0.0
    let mut sum_x2 = 0.0
    
    for point in data {
      let x = (point.timestamp - first_timestamp).to_float()
      let y = point.value
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + (x * y)
      sum_x2 = sum_x2 + (x * x)
    }
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let intercept = (sum_y - slope * sum_x) / n
    
    // 计算相关系数作为置信度
    let mean_x = sum_x / n
    let mean_y = sum_y / n
    
    let mut numerator = 0.0
    let mut sum_x_diff_sq = 0.0
    let mut sum_y_diff_sq = 0.0
    
    for point in data {
      let x = (point.timestamp - first_timestamp).to_float()
      let y = point.value
      
      numerator = numerator + ((x - mean_x) * (y - mean_y))
      sum_x_diff_sq = sum_x_diff_sq + ((x - mean_x) * (x - mean_x))
      sum_y_diff_sq = sum_y_diff_sq + ((y - mean_y) * (y - mean_y))
    }
    
    let correlation = if sum_x_diff_sq > 0.0 && sum_y_diff_sq > 0.0 {
      numerator.abs() / (sum_x_diff_sq * sum_y_diff_sq).sqrt()
    } else {
      0.0
    }
    
    // 生成预测
    let mut predictions = []
    let last_timestamp = data[data.length() - 1].timestamp
    let interval = if data.length() > 1 {
      (data[1].timestamp - data[0].timestamp).to_float()
    } else {
      60.0  // 默认60秒间隔
    }
    
    for i in 1..=steps {
      let future_timestamp = last_timestamp + (i * interval).to_int()
      let x = (future_timestamp.to_int() - first_timestamp.to_int()).to_float()
      let predicted_value = slope * x + intercept
      predictions = predictions + [(future_timestamp, predicted_value)]
    }
    
    // 确定趋势
    let trend = if slope > 0.5 {
      "increasing"
    } else if slope < -0.5 {
      "decreasing"
    } else {
      "stable"
    }
    
    {
      trend,
      slope,
      confidence: correlation,
      prediction: predictions
    }
  }
  
  // 移动平均预测
  let moving_average_prediction = fn(data: Array[TimeSeriesDataPoint], window_size: Int, steps: Int) {
    if data.length() < window_size {
      return {
        trend: "insufficient_data",
        slope: 0.0,
        confidence: 0.0,
        prediction: []
      }
    }
    
    // 计算最近的移动平均值
    let recent_data = data.slice(data.length() - window_size, window_size)
    let sum = recent_data.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let avg = sum / window_size.to_float()
    
    // 计算趋势（比较前后两个窗口的平均值）
    let prev_data = data.slice(data.length() - (window_size * 2), window_size)
    let prev_sum = prev_data.reduce(fn(acc, point) { acc + point.value }, 0.0)
    let prev_avg = prev_sum / window_size.to_float()
    
    let slope = avg - prev_avg
    let trend = if slope > 1.0 {
      "increasing"
    } else if slope < -1.0 {
      "decreasing"
    } else {
      "stable"
    }
    
    // 计算置信度（基于数据的稳定性）
    let variance = recent_data.reduce(fn(acc, point) {
      let diff = point.value - avg
      acc + (diff * diff)
    }, 0.0) / window_size.to_float()
    
    let confidence = if variance > 0.0 {
      1.0 / (1.0 + variance)
    } else {
      1.0
    }
    
    // 生成预测（假设趋势持续）
    let mut predictions = []
    let last_timestamp = data[data.length() - 1].timestamp
    let interval = if data.length() > 1 {
      (data[1].timestamp - data[0].timestamp)
    } else {
      60
    }
    
    for i in 1..=steps {
      let future_timestamp = last_timestamp + (i * interval)
      let predicted_value = avg + (slope * i.to_float())
      predictions = predictions + [(future_timestamp, predicted_value)]
    }
    
    {
      trend,
      slope,
      confidence,
      prediction: predictions
    }
  }
  
  // 指数平滑预测
  let exponential_smoothing_prediction = fn(data: Array[TimeSeriesDataPoint], alpha: Float, steps: Int) {
    if data.length() < 1 {
      return {
        trend: "insufficient_data",
        slope: 0.0,
        confidence: 0.0,
        prediction: []
      }
    }
    
    // 计算指数平滑值
    let mut smoothed_value = data[0].value
    
    for i in 1..data.length() {
      smoothed_value = alpha * data[i].value + (1.0 - alpha) * smoothed_value
    }
    
    // 计算趋势（基于最近几个点的变化）
    let recent_points = if data.length() >= 3 {
      data.slice(data.length() - 3, 3)
    } else {
      data
    }
    
    let mut slope = 0.0
    if recent_points.length() >= 2 {
      let first_value = recent_points[0].value
      let last_value = recent_points[recent_points.length() - 1].value
      slope = (last_value - first_value) / recent_points.length().to_float()
    }
    
    let trend = if slope > 0.5 {
      "increasing"
    } else if slope < -0.5 {
      "decreasing"
    } else {
      "stable"
    }
    
    // 计算置信度（基于预测误差）
    let mut error_sum = 0.0
    let mut prev_smoothed = data[0].value
    
    for i in 1..data.length() {
      let current_smoothed = alpha * data[i].value + (1.0 - alpha) * prev_smoothed
      let error = (data[i].value - prev_smoothed).abs()
      error_sum = error_sum + error
      prev_smoothed = current_smoothed
    }
    
    let avg_error = error_sum / data.length().to_float()
    let confidence = if avg_error > 0.0 {
      1.0 / (1.0 + avg_error)
    } else {
      1.0
    }
    
    // 生成预测
    let mut predictions = []
    let last_timestamp = data[data.length() - 1].timestamp
    let interval = if data.length() > 1 {
      (data[1].timestamp - data[0].timestamp)
    } else {
      60
    }
    
    for i in 1..=steps {
      let future_timestamp = last_timestamp + (i * interval)
      let predicted_value = smoothed_value + (slope * i.to_float())
      predictions = predictions + [(future_timestamp, predicted_value)]
    }
    
    {
      trend,
      slope,
      confidence,
      prediction: predictions
    }
  }
  
  // 测试线性回归预测
  let linear_result = linear_regression_prediction(time_series_data, 3)
  assert_eq(linear_result.trend, "increasing")
  assert_true(linear_result.slope > 0.0)
  assert_true(linear_result.confidence > 0.9)  // 数据呈明显线性趋势，相关性应该很高
  assert_eq(linear_result.prediction.length(), 3)  // 预测3个点
  
  // 验证预测值递增
  assert_true(linear_result.prediction[0].1 < linear_result.prediction[1].1)
  assert_true(linear_result.prediction[1].1 < linear_result.prediction[2].1)
  
  // 测试移动平均预测
  let moving_avg_result = moving_average_prediction(time_series_data, 3, 3)
  assert_eq(moving_avg_result.trend, "increasing")
  assert_true(moving_avg_result.slope > 0.0)
  assert_true(moving_avg_result.confidence > 0.0)
  assert_eq(moving_avg_result.prediction.length(), 3)  // 预测3个点
  
  // 测试指数平滑预测
  let exp_smoothing_result = exponential_smoothing_prediction(time_series_data, 0.3, 3)
  assert_eq(exp_smoothing_result.trend, "increasing")
  assert_true(exp_smoothing_result.slope > 0.0)
  assert_true(exp_smoothing_result.confidence > 0.0)
  assert_eq(exp_smoothing_result.prediction.length(), 3)  // 预测3个点
  
  // 异常模式检测
  let detect_anomaly_patterns = fn(data: Array[TimeSeriesDataPoint]) {
    if data.length() < 3 {
      return []
    }
    
    let mut patterns = []
    
    // 计算基本统计量
    let values = data.map(fn(p) { p.value })
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let mean = sum / values.length().to_float()
    
    let mut variance = 0.0
    for value in values {
      let diff = value - mean
      variance = variance + (diff * diff)
    }
    variance = variance / values.length().to_float()
    let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    // 检测异常点
    let mut anomaly_count = 0
    let mut max_anomaly_value = 0.0
    
    for point in data {
      let z_score = if std_dev > 0.0 {
        (point.value - mean) / std_dev
      } else {
        0.0
      }
      
      if z_score.abs() > 2.0 {
        anomaly_count = anomaly_count + 1
        if point.value.abs() > max_anomaly_value {
          max_anomaly_value = point.value.abs()
        }
      }
    }
    
    if anomaly_count > 0 {
      let severity = if anomaly_count > data.length() / 2 {
        "high"
      } else if anomaly_count > data.length() / 4 {
        "medium"
      } else {
        "low"
      }
      
      patterns = patterns + [{
        pattern_type: "statistical_outliers",
        frequency: anomaly_count,
        severity,
        description: "Detected " + anomaly_count.to_string() + " statistical outliers with z-score > 2.0"
      }]
    }
    
    // 检测趋势变化
    if data.length() >= 4 {
      let first_half = data.slice(0, data.length() / 2)
      let second_half = data.slice(data.length() / 2, data.length() - data.length() / 2)
      
      let first_avg = first_half.reduce(fn(acc, p) { acc + p.value }, 0.0) / first_half.length().to_float()
      let second_avg = second_half.reduce(fn(acc, p) { acc + p.value }, 0.0) / second_half.length().to_float()
      
      let change_rate = (second_avg - first_avg) / first_avg
      
      if change_rate.abs() > 0.2 {  // 20%以上的变化
        let severity = if change_rate.abs() > 0.5 {
          "high"
        } else if change_rate.abs() > 0.3 {
          "medium"
        } else {
          "low"
        }
        
        patterns = patterns + [{
          pattern_type: "trend_change",
          frequency: 1,
          severity,
          description: "Detected significant trend change: " + (change_rate * 100.0).to_string() + "%"
        }]
      }
    }
    
    patterns
  }
  
  // 测试异常模式检测
  let anomaly_patterns = detect_anomaly_patterns(time_series_data)
  assert_eq(anomaly_patterns.length(), 1)  // 应该检测到趋势变化模式
  
  match anomaly_patterns[0] {
    pattern => {
      assert_eq(pattern.pattern_type, "trend_change")
      assert_eq(pattern.frequency, 1)
      assert_eq(pattern.severity, "high")  // CPU使用率从25.5%增长到47.6%，变化率超过50%
      assert_true(pattern.description.contains("significant trend change"))
    }
  }
  
  // 综合预测分析
  let comprehensive_prediction_analysis = fn(data: Array[TimeSeriesDataPoint]) {
    let linear_pred = linear_regression_prediction(data, 3)
    let moving_avg_pred = moving_average_prediction(data, 3, 3)
    let exp_smoothing_pred = exponential_smoothing_prediction(data, 0.3, 3)
    
    let anomaly_patterns = detect_anomaly_patterns(data)
    
    // 集成预测（简单平均）
    let mut ensemble_predictions = []
    
    for i in 0..3 {
      if i < linear_pred.prediction.length() && 
         i < moving_avg_pred.prediction.length() && 
         i < exp_smoothing_pred.prediction.length() {
        
        let timestamp = linear_pred.prediction[i].0
        let linear_value = linear_pred.prediction[i].1
        let moving_avg_value = moving_avg_pred.prediction[i].1
        let exp_smoothing_value = exp_smoothing_pred.prediction[i].1
        
        let ensemble_value = (linear_value + moving_avg_value + exp_smoothing_value) / 3.0
        ensemble_predictions = ensemble_predictions + [(timestamp, ensemble_value)]
      }
    }
    
    // 计算集成预测的置信度
    let avg_confidence = (linear_pred.confidence + moving_avg_pred.confidence + exp_smoothing_pred.confidence) / 3.0
    
    // 确定总体趋势
    let trend_votes = [
      linear_pred.trend,
      moving_avg_pred.trend,
      exp_smoothing_pred.trend
    ]
    
    let increasing_votes = trend_votes.filter(fn(t) { t == "increasing" }).length()
    let decreasing_votes = trend_votes.filter(fn(t) { t == "decreasing" }).length()
    let stable_votes = trend_votes.filter(fn(t) { t == "stable" }).length()
    
    let consensus_trend = if increasing_votes > decreasing_votes && increasing_votes > stable_votes {
      "increasing"
    } else if decreasing_votes > increasing_votes && decreasing_votes > stable_votes {
      "decreasing"
    } else {
      "stable"
    }
    
    {
      consensus_trend,
      ensemble_predictions,
      confidence: avg_confidence,
      anomaly_patterns,
      model_agreement: if increasing_votes == 3 || decreasing_votes == 3 || stable_votes == 3 {
        "high"
      } else if (increasing_votes >= 2) || (decreasing_votes >= 2) || (stable_votes >= 2) {
        "medium"
      } else {
        "low"
      }
    }
  }
  
  // 测试综合预测分析
  let comprehensive_result = comprehensive_prediction_analysis(time_series_data)
  assert_eq(comprehensive_result.consensus_trend, "increasing")  // 所有模型都应该预测上升趋势
  assert_eq(comprehensive_result.ensemble_predictions.length(), 3)  // 3个预测点
  assert_true(comprehensive_result.confidence > 0.5)  // 中等以上的置信度
  assert_eq(comprehensive_result.model_agreement, "high")  // 所有模型应该一致
  assert_eq(comprehensive_result.anomaly_patterns.length(), 1)  // 检测到1个异常模式
  
  // 验证集成预测值的合理性
  let last_actual_value = time_series_data[time_series_data.length() - 1].value
  let first_predicted = comprehensive_result.ensemble_predictions[0].1
  
  // 预测值应该大于最后一个实际值（上升趋势）
  assert_true(first_predicted > last_actual_value)
}