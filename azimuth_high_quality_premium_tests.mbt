// Azimuth 高级遥测系统测试套件
// 包含高级遥测功能和系统集成的深度测试

// 测试1: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)>,
    events: Array[(Int, String, Array[(String, String)>)>,
    links: Array[(String, String)>
  }
  
  // 创建示例遥测数据
  let telemetry_data = {
    trace_id: "trace-12345-67890-abcdef",
    span_id: "span-11111-22222-33333",
    parent_span_id: Some("span-00000-11111-22222"),
    operation_name: "database_query",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("service.version", "1.2.3"),
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM payments WHERE id = $1"),
      ("db.instance", "payments-db-primary"),
      ("net.peer.name", "payments-db.internal"),
      ("net.peer.port", "5432")
    ],
    events: [
      (1640995220000, "query.start", []),
      (1640995235000, "cache.hit", [("cache.key", "payment_12345")]),
      (1640995245000, "query.complete", [("rows.affected", "1")])
    ],
    links: [
      ("trace-99999-88888-77777", "span-44444-55555-66666")
    ]
  }
  
  // JSON序列化函数（简化版）
  let serialize_to_json = fn(data: TelemetryData) {
    let attributes_json = data.attributes.map(fn(attr) {
      let (key, value) = attr
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    
    let events_json = data.events.map(fn(event) {
      let (timestamp, name, attrs) = event
      let attrs_json = attrs.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      "{\"timestamp\":" + timestamp.to_string() + ",\"name\":\"" + name + "\",\"attributes\":{" + attrs_json + "}}"
    }).join(",")
    
    let links_json = data.links.map(fn(link) {
      let (trace_id, span_id) = link
      "{\"trace_id\":\"" + trace_id + "\",\"span_id\":\"" + span_id + "\"}"
    }).join(",")
    
    let parent_json = match data.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{" +
    "\"trace_id\":\"" + data.trace_id + "\"," +
    "\"span_id\":\"" + data.span_id + "\"," +
    "\"parent_span_id\":" + parent_json + "," +
    "\"operation_name\":\"" + data.operation_name + "\"," +
    "\"start_time\":" + data.start_time.to_string() + "," +
    "\"end_time\":" + data.end_time.to_string() + "," +
    "\"status\":\"" + data.status + "\"," +
    "\"attributes\":{" + attributes_json + "}," +
    "\"events\":[" + events_json + "]," +
    "\"links\":[" + links_json + "]" +
    "}"
  }
  
  // 测试序列化
  let json_string = serialize_to_json(telemetry_data)
  assert_true(json_string.contains("\"trace_id\":\"trace-12345-67890-abcdef\""))
  assert_true(json_string.contains("\"span_id\":\"span-11111-22222-33333\""))
  assert_true(json_string.contains("\"operation_name\":\"database_query\""))
  assert_true(json_string.contains("\"service.name\":\"payment-service\""))
  assert_true(json_string.contains("\"cache.hit\""))
  
  // 简化的JSON解析函数
  let parse_json_value = fn(json: String, key: String) {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json.index_of(key_pattern)
    if start_index == -1 {
      None
    } else {
      let value_start = start_index + key_pattern.length()
      let value_end = json.index_of("\"", value_start)
      if value_end == -1 {
        None
      } else {
        Some(json.substring(value_start, value_end - value_start))
      }
    }
  }
  
  // 测试反序列化
  let parsed_trace_id = parse_json_value(json_string, "trace_id")
  assert_eq(parsed_trace_id, Some("trace-12345-67890-abcdef"))
  
  let parsed_operation = parse_json_value(json_string, "operation_name")
  assert_eq(parsed_operation, Some("database_query"))
  
  let parsed_status = parse_json_value(json_string, "status")
  assert_eq(parsed_status, Some("ok"))
  
  // 测试嵌套属性解析
  let service_name = parse_json_value(json_string, "service.name")
  assert_eq(service_name, Some("payment-service"))
  
  let db_system = parse_json_value(json_string, "db.system")
  assert_eq(db_system, Some("postgresql"))
  
  // 验证序列化/反序列化的一致性
  let duration = telemetry_data.end_time - telemetry_data.start_time
  assert_eq(duration, 50000)
  
  // 测试复杂数据结构的完整性
  assert_eq(telemetry_data.attributes.length(), 7)
  assert_eq(telemetry_data.events.length(), 3)
  assert_eq(telemetry_data.links.length(), 1)
  
  // 验证事件时间戳的顺序
  assert_true(telemetry_data.events[0].0 < telemetry_data.events[1].0)
  assert_true(telemetry_data.events[1].0 < telemetry_data.events[2].0)
}

// 测试2: 分布式追踪上下文传播
test "分布式追踪上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: String,
    is_remote: Bool
  }
  
  // 定义传播器接口
  type Propagator = {
    inject: (TraceContext, Array[(String, String)>) -> Array[(String, String)>,
    extract: (Array[(String, String)>) -> Option[TraceContext],
    fields: Array[String]
  }
  
  // 创建W3C TraceContext传播器
  let w3c_propagator = {
    // 注入追踪上下文到头部
    inject: fn(context: TraceContext, headers: Array[(String, String)>) {
      let trace_parent = "00-" + context.trace_id + "-" + context.span_id + "-" + 
                        context.trace_flags.to_string(16).to_uppercase()
      let trace_state = context.trace_state
      
      let updated_headers = headers.push(("traceparent", trace_parent))
      if trace_state.length() > 0 {
        updated_headers.push(("tracestate", trace_state))
      } else {
        updated_headers
      }
    },
    
    // 从头部提取追踪上下文
    extract: fn(headers: Array[(String, String)>) {
      let trace_parent = headers.find(fn(header) { header.0 == "traceparent" })
      let trace_state = headers.find(fn(header) { header.0 == "tracestate" })
      
      match trace_parent {
        Some((_, value)) => {
          // 解析traceparent: version-trace_id-span_id-flags
          let parts = value.split("-")
          if parts.length() >= 4 {
            let trace_id = parts[1]
            let span_id = parts[2]
            let trace_flags = parts[3].to_int(16)
            
            let state = match trace_state {
              Some((_, state_value)) => state_value
              None => ""
            }
            
            Some({
              trace_id,
              span_id,
              trace_flags,
              trace_state: state,
              is_remote: true
            })
          } else {
            None
          }
        }
        None => None
      }
    },
    
    fields: ["traceparent", "tracestate"]
  }
  
  // 创建Baggage传播器
  let baggage_propagator = {
    inject: fn(context: TraceContext, headers: Array[(String, String)>) {
      // 简化的baggage注入
      headers.push(("baggage", "user.id=12345,request.source=mobile"))
    },
    
    extract: fn(headers: Array[(String, String)>) {
      match headers.find(fn(header) { header.0 == "baggage" }) {
        Some((_, value)) => {
          // 简化处理，返回默认上下文
          Some({
            trace_id: "extracted-trace-id",
            span_id: "extracted-span-id",
            trace_flags: 1,
            trace_state: value,
            is_remote: true
          })
        }
        None => None
      }
    },
    
    fields: ["baggage"]
  }
  
  // 创建复合传播器
  let composite_propagator = {
    inject: fn(context: TraceContext, headers: Array[(String, String)>) {
      let headers_with_w3c = w3c_propagator.inject(context, headers)
      baggage_propagator.inject(context, headers_with_w3c)
    },
    
    extract: fn(headers: Array[(String, String)>) {
      match w3c_propagator.extract(headers) {
        Some(context) => Some(context)
        None => baggage_propagator.extract(headers)
      }
    },
    
    fields: w3c_propagator.fields + baggage_propagator.fields
  }
  
  // 测试追踪上下文创建
  let original_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    trace_flags: 1,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    is_remote: false
  }
  
  // 测试注入
  let empty_headers = []
  let headers_with_context = composite_propagator.inject(original_context, empty_headers)
  
  assert_eq(headers_with_context.length(), 3)
  assert_true(headers_with_context.contains(("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b9c7c989f97918e1-01")))
  assert_true(headers_with_context.contains(("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")))
  assert_true(headers_with_context.contains(("baggage", "user.id=12345,request.source=mobile")))
  
  // 测试提取
  let extracted_context = composite_propagator.extract(headers_with_context)
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, "0af7651916cd43dd8448eb211c80319c")
      assert_eq(context.span_id, "b9c7c989f97918e1")
      assert_eq(context.trace_flags, 1)
      assert_eq(context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
      assert_true(context.is_remote)
    }
    None => assert_true(false)
  }
  
  // 测试跨服务传播
  let service_a_headers = [("authorization", "Bearer token123")]
  let headers_for_service_b = composite_propagator.inject(original_context, service_a_headers)
  
  // 模拟HTTP请求
  let service_b_received = headers_for_service_b + [("x-forwarded-for", "10.0.0.1")]
  
  // 服务B提取上下文
  let context_at_service_b = composite_propagator.extract(service_b_received)
  
  match context_at_service_b {
    Some(context) => {
      // 创建子span
      let child_context = {
        trace_id: context.trace_id,
        span_id: "new-child-span-id",
        trace_flags: context.trace_flags,
        trace_state: context.trace_state,
        is_remote: false
      }
      
      // 验证父子关系
      assert_eq(child_context.trace_id, original_context.trace_id)
      assert_eq(child_context.trace_flags, original_context.trace_flags)
      assert_eq(child_context.trace_state, original_context.trace_state)
      assert_not_eq(child_context.span_id, original_context.span_id)
    }
    None => assert_true(false)
  }
  
  // 测试无效头部处理
  let invalid_headers = [
    ("traceparent", "invalid-format"),
    ("baggage", "invalid=baggage=value")
  ]
  
  let invalid_context = composite_propagator.extract(invalid_headers)
  assert_eq(invalid_context, None)
  
  // 测试部分缺失头部
  let partial_headers = [("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b9c7c989f97918e1-01")]
  let partial_context = composite_propagator.extract(partial_headers)
  
  match partial_context {
    Some(context) => {
      assert_eq(context.trace_id, "0af7651916cd43dd8448eb211c80319c")
      assert_eq(context.span_id, "b9c7c989f97918e1")
      assert_eq(context.trace_flags, 1)
      assert_eq(context.trace_state, "")
      assert_true(context.is_remote)
    }
    None => assert_true(false)
  }
}

// 测试3: 遥测指标聚合和统计分析
test "遥测指标聚合和统计分析" {
  // 定义指标类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义数据点
  type DataPoint = {
    timestamp: Int,
    value: Float,
    attributes: Array[(String, String)>
  }
  
  // 定义指标
  type Metric = {
    name: String,
    description: String,
    metric_type: MetricType,
    unit: String,
    data_points: Array[DataPoint]
  }
  
  // 定义聚合结果
  type AggregationResult = {
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    mean: Float,
    median: Float,
    variance: Float,
    std_deviation: Float,
    percentiles: Array[(Float, Float)>  // (percentile, value)
  }
  
  // 创建聚合函数
  let aggregate_data_points = fn(points: Array[DataPoint>) {
    if points.length() == 0 {
      return {
        count: 0,
        sum: 0.0,
        min: 0.0,
        max: 0.0,
        mean: 0.0,
        median: 0.0,
        variance: 0.0,
        std_deviation: 0.0,
        percentiles: []
      }
    }
    
    let values = points.map(fn(p) { p.value })
    let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let min = sorted_values[0]
    let max = sorted_values[count - 1]
    let mean = sum / (count as Float)
    
    // 计算中位数
    let median = if count % 2 == 0 {
      let mid1 = sorted_values[count / 2 - 1]
      let mid2 = sorted_values[count / 2]
      (mid1 + mid2) / 2.0
    } else {
      sorted_values[count / 2]
    }
    
    // 计算方差
    let variance = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / (count as Float)
    let std_deviation = variance.sqrt()
    
    // 计算百分位数
    let percentiles = [
      (50.0, median),
      (90.0, sorted_values[((count as Float) * 0.9) as Int]),
      (95.0, sorted_values[((count as Float) * 0.95) as Int]),
      (99.0, sorted_values[((count as Float) * 0.99) as Int])
    ]
    
    {
      count,
      sum,
      min,
      max,
      mean,
      median,
      variance,
      std_deviation,
      percentiles
    }
  }
  
  // 创建测试数据
  let response_time_data = [
    { timestamp: 1000, value: 120.5, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 2000, value: 85.3, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 3000, value: 200.1, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 4000, value: 95.7, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 5000, value: 150.2, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 6000, value: 110.8, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 7000, value: 180.4, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 8000, value: 75.6, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 9000, value: 165.9, attributes: [("service", "api"), ("endpoint", "/users")] },
    { timestamp: 10000, value: 90.3, attributes: [("service", "api"), ("endpoint", "/users")] }
  ]
  
  // 测试基本聚合
  let aggregation = aggregate_data_points(response_time_data)
  assert_eq(aggregation.count, 10)
  assert_eq(aggregation.min, 75.6)
  assert_eq(aggregation.max, 200.1)
  assert_eq(aggregation.mean.round(), 137.48)
  assert_eq(aggregation.median, 125.35)  // (110.8 + 120.5) / 2
  assert_true(aggregation.variance > 0.0)
  assert_true(aggregation.std_deviation > 0.0)
  
  // 测试百分位数
  let p90 = aggregation.percentiles.find(fn(p) { p.0 == 90.0 })
  match p90 {
    Some((_, value)) => assert_eq(value, 180.4)
    None => assert_true(false)
  }
  
  let p95 = aggregation.percentiles.find(fn(p) { p.0 == 95.0 })
  match p95 {
    Some((_, value)) => assert_eq(value, 180.4)
    None => assert_true(false)
  }
  
  let p99 = aggregation.percentiles.find(fn(p) { p.0 == 99.0 })
  match p99 {
    Some((_, value)) => assert_eq(value, 200.1)
    None => assert_true(false)
  }
  
  // 创建时间窗口聚合函数
  let aggregate_by_time_window = fn(points: Array[DataPoint>, window_size_ms: Int) {
    if points.length() == 0 {
      return []
    }
    
    let sorted_points = points.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    let windows = []
    
    let mut current_window_start = sorted_points[0].timestamp
    let mut current_window_points = []
    
    for point in sorted_points {
      if point.timestamp < current_window_start + window_size_ms {
        current_window_points = current_window_points.push(point)
      } else {
        // 处理当前窗口
        if current_window_points.length() > 0 {
          let window_aggregation = aggregate_data_points(current_window_points)
          windows = windows.push({
            window_start: current_window_start,
            window_end: current_window_start + window_size_ms,
            aggregation: window_aggregation
          })
        }
        
        // 开始新窗口
        current_window_start = point.timestamp
        current_window_points = [point]
      }
    }
    
    // 处理最后一个窗口
    if current_window_points.length() > 0 {
      let window_aggregation = aggregate_data_points(current_window_points)
      windows = windows.push({
        window_start: current_window_start,
        window_end: current_window_start + window_size_ms,
        aggregation: window_aggregation
      })
    }
    
    windows
  }
  
  // 测试时间窗口聚合
  let windows = aggregate_by_time_window(response_time_data, 5000)  // 5秒窗口
  assert_eq(windows.length(), 2)
  
  // 第一个窗口应该包含前5个点
  assert_eq(windows[0].aggregation.count, 5)
  assert_eq(windows[0].window_start, 1000)
  assert_eq(windows[0].window_end, 6000)
  
  // 第二个窗口应该包含后5个点
  assert_eq(windows[1].aggregation.count, 5)
  assert_eq(windows[1].window_start, 6000)
  assert_eq(windows[1].window_end, 11000)
  
  // 创建按属性分组的聚合函数
  let aggregate_by_attributes = fn(points: Array[DataPoint>, group_keys: Array[String>) {
    let groups = Map::empty()
    
    for point in points {
      // 构建分组键
      let group_key_parts = group_keys.map(fn(key) {
        match point.attributes.find(fn(attr) { attr.0 == key }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = group_key_parts.join("|")
      
      // 获取或创建分组
      let group_points = match Map::get(groups, group_key) {
        Some(pts) => pts
        None => []
      }
      
      // 添加点到分组
      let updated_group = group_points.push(point)
      let _ = Map::insert(groups, group_key, updated_group)
    }
    
    // 聚合每个分组
    let results = Map::empty()
    for (group_key, group_points) in groups {
      let aggregation = aggregate_data_points(group_points)
      let _ = Map::insert(results, group_key, aggregation)
    }
    
    results
  }
  
  // 测试按属性分组聚合
  let multi_service_data = response_time_data + [
    { timestamp: 11000, value: 45.2, attributes: [("service", "web"), ("endpoint", "/home")] },
    { timestamp: 12000, value: 55.7, attributes: [("service", "web"), ("endpoint", "/home")] },
    { timestamp: 13000, value: 65.3, attributes: [("service", "web"), ("endpoint", "/home")] }
  ]
  
  let service_groups = aggregate_by_attributes(multi_service_data, ["service"])
  
  // API服务聚合
  let api_aggregation = match Map::get(service_groups, "api") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, mean: 0.0, median: 0.0, variance: 0.0, std_deviation: 0.0, percentiles: [] }
  }
  assert_eq(api_aggregation.count, 10)
  
  // Web服务聚合
  let web_aggregation = match Map::get(service_groups, "web") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, mean: 0.0, median: 0.0, variance: 0.0, std_deviation: 0.0, percentiles: [] }
  }
  assert_eq(web_aggregation.count, 3)
  assert_eq(web_aggregation.min, 45.2)
  assert_eq(web_aggregation.max, 65.3)
}

// 测试4: 自适应采样策略
test "自适应采样策略" {
  // 定义采样决策
  type SamplingDecision = {
    sampled: Bool,
    attributes: Array[(String, String)>
  }
  
  // 定义采样器接口
  type Sampler = {
    should_sample: (String, String, Array[(String, String)>) -> SamplingDecision,
    description: String
  }
  
  // 创建始终开启采样器
  let always_on_sampler = {
    should_sample: fn(trace_id: String, span_name: String, attributes: Array[(String, String)>) {
      { sampled: true, attributes: [] }
    },
    description: "AlwaysOnSampler"
  }
  
  // 创建始终关闭采样器
  let always_off_sampler = {
    should_sample: fn(trace_id: String, span_name: String, attributes: Array[(String, String)>) {
      { sampled: false, attributes: [] }
    },
    description: "AlwaysOffSampler"
  }
  
  // 创建基于比例的采样器
  let trace_id_ratio_sampler = fn(ratio: Float) {
    let should_sample = fn(trace_id: String, span_name: String, attributes: Array[(String, String)>) {
      // 简化的采样算法：基于trace_id的哈希值
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      let sampled = normalized <= ratio
      
      let sampling_attributes = if sampled {
        [("sampler.type", "trace_id_ratio"), ("sampler.ratio", ratio.to_string())]
      } else {
        []
      }
      
      { sampled, attributes: sampling_attributes }
    }
    
    { should_sample, description: "TraceIdRatioSampler(" + ratio.to_string() + ")" }
  }
  
  // 创建基于属性的采样器
  let attribute_based_sampler = fn(rules: Array[(String, String)>) {
    let should_sample = fn(trace_id: String, span_name: String, attributes: Array[(String, String)>) {
      let mut sampled = false
      
      for rule in rules {
        let (key, expected_value) = rule
        let match_found = attributes.any(fn(attr) {
          let (attr_key, attr_value) = attr
          attr_key == key && attr_value == expected_value
        })
        
        if match_found {
          sampled = true
        }
      }
      
      let sampling_attributes = if sampled {
        [("sampler.type", "attribute_based"), ("sampler.rules", rules.length().to_string())]
      } else {
        []
      }
      
      { sampled, attributes: sampling_attributes }
    }
    
    { should_sample, description: "AttributeBasedSampler" }
  }
  
  // 创建自适应采样器
  let adaptive_sampler = fn(base_ratio: Float, max_ratio: Float, error_threshold: Float) {
    let mut current_ratio = base_ratio
    let mut error_rate = 0.0
    let mut sample_count = 0
    let mut error_count = 0
    
    let should_sample = fn(trace_id: String, span_name: String, attributes: Array[(String, String)>) {
      // 更新错误率
      let is_error = attributes.any(fn(attr) {
        let (key, value) = attr
        key == "span.status" && value == "error"
      })
      
      sample_count = sample_count + 1
      if is_error {
        error_count = error_count + 1
      }
      
      error_rate = (error_count as Float) / (sample_count as Float)
      
      // 调整采样率
      if error_rate > error_threshold && current_ratio < max_ratio {
        current_ratio = current_ratio + 0.1
        if current_ratio > max_ratio {
          current_ratio = max_ratio
        }
      } else if error_rate < error_threshold / 2.0 && current_ratio > base_ratio {
        current_ratio = current_ratio - 0.05
        if current_ratio < base_ratio {
          current_ratio = base_ratio
        }
      }
      
      // 使用当前比例进行采样
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      let sampled = normalized <= current_ratio
      
      let sampling_attributes = [
        ("sampler.type", "adaptive"),
        ("sampler.ratio", current_ratio.to_string()),
        ("sampler.error_rate", error_rate.to_string()),
        ("sampler.sample_count", sample_count.to_string())
      ]
      
      { sampled, attributes: sampling_attributes }
    }
    
    { should_sample, description: "AdaptiveSampler" }
  }
  
  // 测试始终开启采样器
  let always_on_result = always_on_sampler.should_sample("trace-123", "span-abc", [])
  assert_true(always_on_result.sampled)
  assert_eq(always_on_result.attributes.length(), 0)
  
  // 测试始终关闭采样器
  let always_off_result = always_off_sampler.should_sample("trace-456", "span-def", [])
  assert_false(always_off_result.sampled)
  assert_eq(always_off_result.attributes.length(), 0)
  
  // 测试基于比例的采样器
  let ratio_sampler = trace_id_ratio_sampler(0.5)  // 50%采样率
  let ratio_result1 = ratio_sampler.should_sample("trace-789", "span-ghi", [])
  // 注意：实际结果取决于哈希计算，这里只测试属性
  assert_eq(ratio_result1.attributes, [("sampler.type", "trace_id_ratio"), ("sampler.ratio", "0.5")])
  
  // 测试基于属性的采样器
  let attr_sampler = attribute_based_sampler([
    ("service.name", "critical-service"),
    ("span.kind", "server"),
    ("environment", "production")
  ])
  
  let matching_attrs = [
    ("service.name", "critical-service"),
    ("span.kind", "server"),
    ("environment", "production")
  ]
  let attr_result1 = attr_sampler.should_sample("trace-111", "span-jkl", matching_attrs)
  assert_true(attr_result1.sampled)
  assert_eq(attr_result1.attributes, [("sampler.type", "attribute_based"), ("sampler.rules", "3")])
  
  let non_matching_attrs = [
    ("service.name", "regular-service"),
    ("span.kind", "client"),
    ("environment", "development")
  ]
  let attr_result2 = attr_sampler.should_sample("trace-222", "span-mno", non_matching_attrs)
  assert_false(attr_result2.sampled)
  assert_eq(attr_result2.attributes.length(), 0)
  
  // 测试自适应采样器
  let adaptive = adaptive_sampler(0.1, 0.8, 0.05)  // 基础10%，最大80%，错误阈值5%
  
  // 初始采样
  let adaptive_result1 = adaptive.should_sample("trace-333", "span-pqr", [])
  let initial_ratio = adaptive_result1.attributes.find(fn(attr) { attr.0 == "sampler.ratio" })
  match initial_ratio {
    Some((_, ratio_str)) => assert_eq(ratio_str, "0.1")
    None => assert_true(false)
  }
  
  // 模拟错误span
  let error_attrs = [("span.status", "error")]
  let adaptive_result2 = adaptive.should_sample("trace-444", "span-stu", error_attrs)
  let updated_ratio = adaptive_result2.attributes.find(fn(attr) { attr.0 == "sampler.ratio" })
  match updated_ratio {
    Some((_, ratio_str)) => {
      // 错误率上升，采样率应该增加
      let ratio_value = ratio_str.to_float()
      assert_true(ratio_value >= 0.1)
    }
    None => assert_true(false)
  }
  
  // 测试采样器组合
  let parent_based_sampler = fn(parent_sampler: Sampler, root_sampler: Sampler) {
    let should_sample = fn(trace_id: String, span_name: String, attributes: Array[(String, String)>) {
      let has_parent = attributes.any(fn(attr) {
        let (key, _) = attr
        key == "parent.span"
      })
      
      if has_parent {
        parent_sampler.should_sample(trace_id, span_name, attributes)
      } else {
        root_sampler.should_sample(trace_id, span_name, attributes)
      }
    }
    
    { should_sample, description: "ParentBasedSampler" }
  }
  
  let parent_sampler = parent_based_sampler(always_on_sampler, trace_id_ratio_sampler(0.2))
  
  // 测试有parent的情况
  let with_parent_attrs = [("parent.span", "parent-123")]
  let parent_result1 = parent_sampler.should_sample("trace-555", "span-vwx", with_parent_attrs)
  assert_true(parent_result1.sampled)  // 使用parent sampler (always_on)
  
  // 测试无parent的情况
  let without_parent_attrs = [("service.name", "test-service")]
  let parent_result2 = parent_sampler.should_sample("trace-666", "span-yza", without_parent_attrs)
  // 使用root sampler (20%比例)，实际结果取决于哈希
  assert_eq(parent_result2.attributes, [("sampler.type", "trace_id_ratio"), ("sampler.ratio", "0.2")])
}

// 测试5: 遥测数据压缩和传输优化
test "遥测数据压缩和传输优化" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_delay_ms: Int,
    max_export_batch_size: Int,
    max_export_timeout_ms: Int
  }
  
  // 定义数据批处理
  type Batch = {
    items: Array[String],
    timestamp: Int,
    size: Int,
    compressed: Bool
  }
  
  // 创建简单的字符串压缩函数（模拟）
  let compress_string = fn(input: String) {
    // 简化的压缩：替换重复的模式
    let compressed = input
      .replace("trace_id", "t_id")
      .replace("span_id", "s_id")
      .replace("parent_span_id", "p_id")
      .replace("operation_name", "op_name")
      .replace("start_time", "s_time")
      .replace("end_time", "e_time")
      .replace("attributes", "attrs")
      .replace("service.name", "s.name")
      .replace("service.version", "s.ver")
      .replace("environment", "env")
    
    compressed
  }
  
  // 创建简单的字符串解压缩函数（模拟）
  let decompress_string = fn(input: String) {
    // 简化的解压缩：恢复替换的模式
    let decompressed = input
      .replace("t_id", "trace_id")
      .replace("s_id", "span_id")
      .replace("p_id", "parent_span_id")
      .replace("op_name", "operation_name")
      .replace("s_time", "start_time")
      .replace("e_time", "end_time")
      .replace("attrs", "attributes")
      .replace("s.name", "service.name")
      .replace("s.ver", "service.version")
      .replace("env", "environment")
    
    decompressed
  }
  
  // 创建批处理器
  let batch_processor = fn(config: BatchConfig) {
    let mut current_batch = []
    let mut batch_start_time = 0
    let mut batches = []
    
    let process_item = fn(item: String, current_time: Int) {
      current_batch = current_batch.push(item)
      
      let batch_size = current_batch.length()
      let batch_age = current_time - batch_start_time
      
      if batch_size >= config.max_batch_size || batch_age >= config.max_delay_ms {
        // 创建新批次
        let batch = {
          items: current_batch,
          timestamp: batch_start_time,
          size: batch_size,
          compressed: false
        }
        batches = batches.push(batch)
        
        // 重置批次
        current_batch = []
        batch_start_time = current_time
      }
    }
    
    let flush = fn(current_time: Int) {
      if current_batch.length() > 0 {
        let batch = {
          items: current_batch,
          timestamp: batch_start_time,
          size: current_batch.length(),
          compressed: false
        }
        batches = batches.push(batch)
        current_batch = []
        batch_start_time = current_time
      }
    }
    
    let get_batches = fn() { batches }
    
    { process_item, flush, get_batches }
  }
  
  // 创建压缩处理器
  let compress_batches = fn(batches: Array[Batch>) {
    batches.map(fn(batch) {
      let compressed_items = batch.items.map(compress_string)
      {
        items: compressed_items,
        timestamp: batch.timestamp,
        size: batch.size,
        compressed: true
      }
    })
  }
  
  // 创建传输优化器
  let optimize_for_transmission = fn(batches: Array[Batch>) {
    // 合并小批次
    let mut optimized_batches = []
    let mut current_merged = []
    let mut current_size = 0
    let target_size = 50  // 目标批次大小
    
    for batch in batches {
      if current_size + batch.size <= target_size {
        current_merged = current_merged + batch.items
        current_size = current_size + batch.size
      } else {
        if current_size > 0 {
          optimized_batches = optimized_batches.push({
            items: current_merged,
            timestamp: batches[0].timestamp,  // 使用第一个批次的时间戳
            size: current_size,
            compressed: batches[0].compressed
          })
        }
        current_merged = batch.items
        current_size = batch.size
      }
    }
    
    if current_size > 0 {
      optimized_batches = optimized_batches.push({
        items: current_merged,
        timestamp: batches[0].timestamp,
        size: current_size,
        compressed: batches[0].compressed
      })
    }
    
    optimized_batches
  }
  
  // 测试数据
  let telemetry_data = [
    "{trace_id: \"trace-123\", span_id: \"span-456\", operation_name: \"op1\", start_time: 1000, end_time: 1100, attributes: {service.name: \"api\", environment: \"prod\"}}",
    "{trace_id: \"trace-124\", span_id: \"span-457\", operation_name: \"op2\", start_time: 1100, end_time: 1200, attributes: {service.name: \"api\", environment: \"prod\"}}",
    "{trace_id: \"trace-125\", span_id: \"span-458\", operation_name: \"op3\", start_time: 1200, end_time: 1300, attributes: {service.name: \"web\", environment: \"prod\"}}",
    "{trace_id: \"trace-126\", span_id: \"span-459\", operation_name: \"op4\", start_time: 1300, end_time: 1400, attributes: {service.name: \"web\", environment: \"dev\"}}",
    "{trace_id: \"trace-127\", span_id: \"span-460\", operation_name: \"op5\", start_time: 1400, end_time: 1500, attributes: {service.name: \"db\", environment: \"prod\"}}"
  ]
  
  // 测试压缩
  let original_data = telemetry_data[0]
  let compressed_data = compress_string(original_data)
  let decompressed_data = decompress_string(compressed_data)
  
  assert_eq(original_data, decompressed_data)
  assert_true(compressed_data.length() < original_data.length())
  
  // 测试批处理
  let config = {
    max_batch_size: 3,
    max_delay_ms: 5000,
    max_export_batch_size: 10,
    max_export_timeout_ms: 30000
  }
  
  let processor = batch_processor(config)
  processor.process_item(telemetry_data[0], 1000)
  processor.process_item(telemetry_data[1], 2000)
  processor.process_item(telemetry_data[2], 3000)  // 这应该触发第一批次
  processor.process_item(telemetry_data[3], 4000)
  processor.process_item(telemetry_data[4], 10000)  // 这应该触发第二批次（基于时间）
  processor.flush(10000)
  
  let batches = processor.get_batches()
  assert_eq(batches.length(), 2)
  assert_eq(batches[0].size, 3)
  assert_eq(batches[1].size, 2)
  
  // 测试压缩批次
  let compressed_batches = compress_batches(batches)
  assert_eq(compressed_batches.length(), 2)
  assert_true(compressed_batches[0].compressed)
  assert_true(compressed_batches[1].compressed)
  
  // 验证压缩后的数据
  let compressed_item = compressed_batches[0].items[0]
  assert_true(compressed_item.contains("t_id"))
  assert_true(compressed_item.contains("s_id"))
  assert_false(compressed_item.contains("trace_id"))
  assert_false(compressed_item.contains("span_id"))
  
  // 测试传输优化
  let optimized_batches = optimize_for_transmission(compressed_batches)
  
  // 创建更多小批次进行测试
  let small_batches = [
    { items: ["data1"], timestamp: 1000, size: 1, compressed: true },
    { items: ["data2"], timestamp: 2000, size: 1, compressed: true },
    { items: ["data3"], timestamp: 3000, size: 1, compressed: true }
  ]
  
  let optimized_small = optimize_for_transmission(small_batches)
  assert_eq(optimized_small.length(), 1)
  assert_eq(optimized_small[0].size, 3)
  
  // 测试网络传输模拟
  let simulate_transmission = fn(batches: Array[Batch>, packet_loss_rate: Float, max_retries: Int) {
    let mut successful_batches = []
    let mut failed_batches = []
    
    for batch in batches {
      let mut attempts = 0
      let mut transmitted = false
      
      while attempts < max_retries and not(transmitted) {
        attempts = attempts + 1
        
        // 模拟网络传输（有概率失败）
        let random_value = (Time::now() % 100) as Float / 100.0
        if random_value >= packet_loss_rate {
          successful_batches = successful_batches.push(batch)
          transmitted = true
        }
      }
      
      if not(transmitted) {
        failed_batches = failed_batches.push(batch)
      }
    }
    
    { successful_batches, failed_batches }
  }
  
  // 测试传输
  let transmission_result = simulate_transmission(optimized_batches, 0.1, 3)  // 10%丢包率，最多重试3次
  assert_eq(transmission_result.successful_batches.length(), 2)
  assert_eq(transmission_result.failed_batches.length(), 0)
  
  // 测试高丢包率场景
  let high_loss_result = simulate_transmission(optimized_batches, 0.5, 2)  // 50%丢包率，最多重试2次
  // 由于是模拟，结果可能不同，这里只验证结构
  assert_eq(high_loss_result.successful_batches.length() + high_loss_result.failed_batches.length(), 2)
}

// 测试6: 遥测系统性能基准测试
test "遥测系统性能基准测试" {
  // 定义性能指标
  type PerformanceMetrics = {
    span_creation_time_ns: Int,
    span_completion_time_ns: Int,
    attribute_set_time_ns: Int,
    event_add_time_ns: Int,
    serialization_time_ns: Int,
    memory_usage_bytes: Int,
    throughput_spans_per_second: Int
  }
  
  // 定义基准测试工具
  let benchmark = fn(name: String, iterations: Int, operation: () -> Unit) {
    let start_time = Time::nano_time()
    let start_memory = get_memory_usage()  // 假设的函数
    
    for i in 0..iterations {
      operation()
    }
    
    let end_time = Time::nano_time()
    let end_memory = get_memory_usage()
    
    let total_time_ns = end_time - start_time
    let avg_time_ns = total_time_ns / iterations
    let memory_delta = end_memory - start_memory
    
    {
      operation_name: name,
      iterations,
      total_time_ns,
      avg_time_ns,
      memory_delta,
      throughput_ops_per_second: (iterations as Float) / ((total_time_ns as Float) / 1_000_000_000.0)
    }
  }
  
  // 假设的内存使用函数
  let get_memory_usage = fn() {
    // 模拟内存使用情况
    Time::now() % 10000
  }
  
  // 创建Span结构（简化版）
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    attributes: Array[(String, String)>,
    events: Array[(Int, String, Array[(String, String)>)>
  }
  
  // Span创建函数
  let create_span = fn(trace_id: String, span_id: String, operation_name: String) {
    {
      trace_id,
      span_id,
      parent_span_id: None,
      operation_name,
      start_time: Time::now(),
      end_time: None,
      status: "ok",
      attributes: [],
      events: []
    }
  }
  
  // Span完成函数
  let end_span = fn(span: Span, status: String) {
    { span | end_time: Some(Time::now()), status }
  }
  
  // 设置属性函数
  let set_attribute = fn(span: Span, key: String, value: String) {
    let new_attr = (key, value)
    { span | attributes: span.attributes.push(new_attr) }
  }
  
  // 添加事件函数
  let add_event = fn(span: Span, name: String, attributes: Array[(String, String)>) {
    let event = (Time::now(), name, attributes)
    { span | events: span.events.push(event) }
  }
  
  // 序列化函数
  let serialize_span = fn(span: Span) {
    let attributes_json = span.attributes.map(fn(attr) {
      let (key, value) = attr
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    
    let end_time_json = match span.end_time {
      Some(time) => time.to_string()
      None => "null"
    }
    
    "{" +
    "\"trace_id\":\"" + span.trace_id + "\"," +
    "\"span_id\":\"" + span.span_id + "\"," +
    "\"operation_name\":\"" + span.operation_name + "\"," +
    "\"start_time\":" + span.start_time.to_string() + "," +
    "\"end_time\":" + end_time_json + "," +
    "\"status\":\"" + span.status + "\"," +
    "\"attributes\":{" + attributes_json + "}" +
    "}"
  }
  
  // 基准测试：Span创建
  let span_creation_benchmark = benchmark("Span Creation", 10000, fn() {
    let trace_id = "trace-" + Time::now().to_string()
    let span_id = "span-" + Time::now().to_string()
    create_span(trace_id, span_id, "test_operation")
  })
  
  // 基准测试：Span完成
  let test_span = create_span("test-trace", "test-span", "test_operation")
  let span_completion_benchmark = benchmark("Span Completion", 10000, fn() {
    end_span(test_span, "ok")
  })
  
  // 基准测试：属性设置
  let span_for_attrs = create_span("test-trace", "test-span", "test_operation")
  let attribute_set_benchmark = benchmark("Attribute Setting", 10000, fn() {
    set_attribute(span_for_attrs, "test.key", "test.value")
  })
  
  // 基准测试：事件添加
  let span_for_events = create_span("test-trace", "test-span", "test_operation")
  let event_add_benchmark = benchmark("Event Adding", 10000, fn() {
    add_event(span_for_events, "test.event", [("attr1", "value1")])
  })
  
  // 基准测试：序列化
  let complex_span = create_span("complex-trace", "complex-span", "complex_operation")
  let span_with_attrs = set_attribute(set_attribute(complex_span, "service.name", "test-service"), "environment", "production")
  let span_with_events = add_event(span_with_attrs, "test.event", [("attr1", "value1")])
  
  let serialization_benchmark = benchmark("Span Serialization", 10000, fn() {
    serialize_span(span_with_events)
  })
  
  // 验证基准测试结果
  assert_true(span_creation_benchmark.avg_time_ns > 0)
  assert_true(span_completion_benchmark.avg_time_ns > 0)
  assert_true(attribute_set_benchmark.avg_time_ns > 0)
  assert_true(event_add_benchmark.avg_time_ns > 0)
  assert_true(serialization_benchmark.avg_time_ns > 0)
  
  // 性能指标应该合理（这些值是示例，实际值取决于系统）
  assert_true(span_creation_benchmark.avg_time_ns < 100000)  // 小于100微秒
  assert_true(span_completion_benchmark.avg_time_ns < 50000)  // 小于50微秒
  assert_true(attribute_set_benchmark.avg_time_ns < 20000)   // 小于20微秒
  assert_true(event_add_benchmark.avg_time_ns < 30000)       // 小于30微秒
  assert_true(serialization_benchmark.avg_time_ns < 100000)  // 小于100微秒
  
  // 测试吞吐量
  assert_true(span_creation_benchmark.throughput_ops_per_second > 1000)  // 至少1000 ops/sec
  assert_true(serialization_benchmark.throughput_ops_per_second > 500)   // 至少500 ops/sec
  
  // 综合性能测试
  let comprehensive_benchmark = benchmark("Comprehensive Telemetry Flow", 1000, fn() {
    // 创建完整的遥测流程
    let span = create_span("trace-" + Time::now().to_string(), "span-" + Time::now().to_string(), "db.query")
    let span_with_attrs = set_attribute(set_attribute(span, "db.system", "postgresql"), "db.operation", "SELECT")
    let span_with_events = add_event(span_with_attrs, "query.start", [])
    let completed_span = end_span(span_with_events, "ok")
    let _ = serialize_span(completed_span)
  })
  
  assert_true(comprehensive_benchmark.avg_time_ns > 0)
  assert_true(comprehensive_benchmark.throughput_ops_per_second > 100)  // 至少100完整流程/sec
  
  // 内存使用测试
  let memory_intensive_benchmark = benchmark("Memory Intensive Operations", 1000, fn() {
    let mut span = create_span("trace-" + Time::now().to_string(), "span-" + Time::now().to_string(), "memory.test")
    
    // 添加大量属性
    for i in 0..50 {
      span = set_attribute(span, "attr" + i.to_string(), "value" + i.to_string())
    }
    
    // 添加大量事件
    for i in 0..20 {
      span = add_event(span, "event" + i.to_string(), [("index", i.to_string())])
    }
    
    let _ = serialize_span(span)
  })
  
  assert_true(memory_intensive_benchmark.memory_delta >= 0)
  
  // 并发性能测试（模拟）
  let concurrent_benchmark = benchmark("Concurrent Operations", 1000, fn() {
    // 模拟并发操作
    let spans = []
    for i in 0..10 {
      let span = create_span("concurrent-trace", "span-" + i.to_string(), "concurrent.op")
      let span_with_attrs = set_attribute(span, "worker.id", i.to_string())
      spans = spans.push(span_with_attrs)
    }
    
    // 并发处理spans
    let serialized_spans = spans.map(serialize_span)
    assert_eq(serialized_spans.length(), 10)
  })
  
  assert_true(concurrent_benchmark.avg_time_ns > 0)
  assert_true(concurrent_benchmark.throughput_ops_per_second > 50)
}

// 测试7: 遥测数据质量验证
test "遥测数据质量验证" {
  // 定义数据质量规则
  enum ValidationRule {
    RequiredField(String)
    MinLength(String, Int)
    MaxLength(String, Int)
    Pattern(String, String)  // field_name, regex_pattern
    Range(String, Float, Float)  // field_name, min_value, max_value
    TimestampOrder
    NoNullValues
    UniqueValues(String)  // field_name
  }
  
  // 定义验证结果
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[String],
    warnings: Array[String]
  }
  
  // 定义遥测数据结构（简化版）
  type TelemetryRecord = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)>
  }
  
  // 创建验证器
  let create_validator = fn(rules: Array[ValidationRule>) {
    let validate = fn(record: TelemetryRecord) {
      let mut errors = []
      let mut warnings = []
      
      for rule in rules {
        match rule {
          ValidationRule::RequiredField(field_name) => {
            match field_name {
              "trace_id" => {
                if record.trace_id.length() == 0 {
                  errors = errors.push("trace_id is required")
                }
              }
              "span_id" => {
                if record.span_id.length() == 0 {
                  errors = errors.push("span_id is required")
                }
              }
              "operation_name" => {
                if record.operation_name.length() == 0 {
                  errors = errors.push("operation_name is required")
                }
              }
              _ => warnings = warnings.push("Unknown required field: " + field_name)
            }
          }
          
          ValidationRule::MinLength(field_name, min_length) => {
            match field_name {
              "trace_id" => {
                if record.trace_id.length() < min_length {
                  errors = errors.push("trace_id must be at least " + min_length.to_string() + " characters")
                }
              }
              "span_id" => {
                if record.span_id.length() < min_length {
                  errors = errors.push("span_id must be at least " + min_length.to_string() + " characters")
                }
              }
              "operation_name" => {
                if record.operation_name.length() < min_length {
                  errors = errors.push("operation_name must be at least " + min_length.to_string() + " characters")
                }
              }
              _ => warnings = warnings.push("Unknown field for min length validation: " + field_name)
            }
          }
          
          ValidationRule::MaxLength(field_name, max_length) => {
            match field_name {
              "trace_id" => {
                if record.trace_id.length() > max_length {
                  errors = errors.push("trace_id must be at most " + max_length.to_string() + " characters")
                }
              }
              "span_id" => {
                if record.span_id.length() > max_length {
                  errors = errors.push("span_id must be at most " + max_length.to_string() + " characters")
                }
              }
              "operation_name" => {
                if record.operation_name.length() > max_length {
                  errors = errors.push("operation_name must be at most " + max_length.to_string() + " characters")
                }
              }
              _ => warnings = warnings.push("Unknown field for max length validation: " + field_name)
            }
          }
          
          ValidationRule::Pattern(field_name, pattern) => {
            match field_name {
              "trace_id" => {
                // 简化的模式匹配：检查是否只包含十六进制字符
                if not(record.trace_id.chars().all(fn(c) { 
                  (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or c == '-'
                })) {
                  errors = errors.push("trace_id must contain only hexadecimal characters and hyphens")
                }
              }
              "span_id" => {
                if not(record.span_id.chars().all(fn(c) { 
                  (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or c == '-'
                })) {
                  errors = errors.push("span_id must contain only hexadecimal characters and hyphens")
                }
              }
              _ => warnings = warnings.push("Unknown field for pattern validation: " + field_name)
            }
          }
          
          ValidationRule::Range(field_name, min_value, max_value) => {
            match field_name {
              "duration" => {
                let duration = record.end_time - record.start_time
                let duration_ms = duration as Float
                if duration_ms < min_value || duration_ms > max_value {
                  errors = errors.push("duration must be between " + min_value.to_string() + " and " + max_value.to_string() + " ms")
                }
              }
              _ => warnings = warnings.push("Unknown field for range validation: " + field_name)
            }
          }
          
          ValidationRule::TimestampOrder => {
            if record.start_time > record.end_time {
              errors = errors.push("start_time must be before end_time")
            }
          }
          
          ValidationRule::NoNullValues => {
            // 在这个简化实现中，我们检查某些字段是否为空
            if record.trace_id.length() == 0 || record.span_id.length() == 0 || record.operation_name.length() == 0 {
              errors = errors.push("No null values allowed in required fields")
            }
          }
          
          ValidationRule::UniqueValues(field_name) => {
            // 在单个记录验证中，这个规则不太适用
            // 在实际实现中，这会在批量验证中使用
            warnings = warnings.push("UniqueValues validation should be applied to batches")
          }
        }
      }
      
      {
        is_valid: errors.length() == 0,
        errors,
        warnings
      }
    }
    
    validate
  }
  
  // 创建批量验证器
  let create_batch_validator = fn(rules: Array[ValidationRule>) {
    let single_validator = create_validator(rules)
    
    let validate_batch = fn(records: Array[TelemetryRecord>) {
      let mut batch_errors = []
      let mut batch_warnings = []
      let mut valid_count = 0
      
      // 单个记录验证
      let record_results = records.map(single_validator)
      
      for i in 0..record_results.length() {
        let result = record_results[i]
        if result.is_valid {
          valid_count = valid_count + 1
        }
        
        // 为每个错误添加记录索引
        let indexed_errors = result.errors.map(fn(error) { "Record " + i.to_string() + ": " + error })
        let indexed_warnings = result.warnings.map(fn(warning) { "Record " + i.to_string() + ": " + warning })
        
        batch_errors = batch_errors + indexed_errors
        batch_warnings = batch_warnings + indexed_warnings
      }
      
      // 批量特定验证
      for rule in rules {
        match rule {
          ValidationRule::UniqueValues(field_name) => {
            match field_name {
              "trace_id" => {
                let trace_ids = records.map(fn(r) { r.trace_id })
                let unique_trace_ids = trace_ids.reduce(fn(acc, id) {
                  if acc.contains(id) { acc } else { acc.push(id) }
                }, [])
                
                if unique_trace_ids.length() < trace_ids.length() {
                  batch_errors = batch_errors.push("Duplicate trace_ids found in batch")
                }
              }
              "span_id" => {
                let span_ids = records.map(fn(r) { r.span_id })
                let unique_span_ids = span_ids.reduce(fn(acc, id) {
                  if acc.contains(id) { acc } else { acc.push(id) }
                }, [])
                
                if unique_span_ids.length() < span_ids.length() {
                  batch_errors = batch_errors.push("Duplicate span_ids found in batch")
                }
              }
              _ => batch_warnings = batch_warnings.push("Unknown field for unique validation: " + field_name)
            }
          }
          _ => {}  // 其他规则已在单个记录验证中处理
        }
      }
      
      {
        total_records: records.length(),
        valid_records: valid_count,
        invalid_records: records.length() - valid_count,
        is_valid: batch_errors.length() == 0,
        errors: batch_errors,
        warnings: batch_warnings
      }
    }
    
    validate_batch
  }
  
  // 创建标准验证规则
  let standard_rules = [
    ValidationRule::RequiredField("trace_id"),
    ValidationRule::RequiredField("span_id"),
    ValidationRule::RequiredField("operation_name"),
    ValidationRule::MinLength("trace_id", 10),
    ValidationRule::MaxLength("trace_id", 64),
    ValidationRule::MinLength("span_id", 10),
    ValidationRule::MaxLength("span_id", 64),
    ValidationRule::MinLength("operation_name", 1),
    ValidationRule::MaxLength("operation_name", 128),
    ValidationRule::Pattern("trace_id", "^[a-f0-9-]+$"),
    ValidationRule::Pattern("span_id", "^[a-f0-9-]+$"),
    ValidationRule::Range("duration", 0.0, 3600000.0),  // 0到1小时
    ValidationRule::TimestampOrder,
    ValidationRule::NoNullValues
  ]
  
  // 测试有效记录
  let valid_record = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    parent_span_id: Some("a1b2c3d4e5f67890"),
    operation_name: "database.query",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("db.system", "postgresql")
    ]
  }
  
  let validator = create_validator(standard_rules)
  let valid_result = validator(valid_record)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // 测试无效记录 - 缺少必填字段
  let missing_trace_id = { valid_record | trace_id: "" }
  let missing_field_result = validator(missing_trace_id)
  assert_false(missing_field_result.is_valid)
  assert_true(missing_field_result.errors.any(fn(error) { error.contains("trace_id is required") }))
  
  // 测试无效记录 - 长度不符合要求
  let short_trace_id = { valid_record | trace_id: "abc" }
  let short_result = validator(short_trace_id)
  assert_false(short_result.is_valid)
  assert_true(short_result.errors.any(fn(error) { error.contains("trace_id must be at least") }))
  
  let long_operation_name = { valid_record | operation_name: "a".repeat(200) }
  let long_result = validator(long_operation_name)
  assert_false(long_result.is_valid)
  assert_true(long_result.errors.any(fn(error) { error.contains("operation_name must be at most") }))
  
  // 测试无效记录 - 模式不匹配
  let invalid_trace_id = { valid_record | trace_id: "invalid-trace-id-with-chars!" }
  let pattern_result = validator(invalid_trace_id)
  assert_false(pattern_result.is_valid)
  assert_true(pattern_result.errors.any(fn(error) { error.contains("trace_id must contain only hexadecimal") }))
  
  // 测试无效记录 - 时间戳顺序错误
  let invalid_timestamp = { valid_record | start_time: 1640995300000, end_time: 1640995200000 }
  let timestamp_result = validator(invalid_timestamp)
  assert_false(timestamp_result.is_valid)
  assert_true(timestamp_result.errors.any(fn(error) { error.contains("start_time must be before end_time") }))
  
  // 测试无效记录 - 超出范围的持续时间
  let long_duration = { valid_record | start_time: 1640995200000, end_time: 1640995200000 + 4000000 }  // 4000秒
  let duration_result = validator(long_duration)
  assert_false(duration_result.is_valid)
  assert_true(duration_result.errors.any(fn(error) { error.contains("duration must be between") }))
  
  // 测试批量验证
  let batch_validator = create_batch_validator(standard_rules + [ValidationRule::UniqueValues("trace_id")])
  
  let valid_batch = [
    valid_record,
    { valid_record | trace_id: "11111111111111111111111111111111", span_id: "22222222222222222222222222222222" },
    { valid_record | trace_id: "33333333333333333333333333333333", span_id: "44444444444444444444444444444444" }
  ]
  
  let valid_batch_result = batch_validator.validate_batch(valid_batch)
  assert_true(valid_batch_result.is_valid)
  assert_eq(valid_batch_result.valid_records, 3)
  assert_eq(valid_batch_result.invalid_records, 0)
  
  // 测试包含无效记录的批次
  let mixed_batch = [
    valid_record,
    missing_trace_id,  // 无效
    { valid_record | trace_id: "55555555555555555555555555555555", span_id: "66666666666666666666666666666666" }
  ]
  
  let mixed_batch_result = batch_validator.validate_batch(mixed_batch)
  assert_false(mixed_batch_result.is_valid)
  assert_eq(mixed_batch_result.valid_records, 2)
  assert_eq(mixed_batch_result.invalid_records, 1)
  
  // 测试重复ID的批次
  let duplicate_batch = [
    valid_record,
    { valid_record | trace_id: "0af7651916cd43dd8448eb211c80319c", span_id: "99999999999999999999999999999999" },  // 重复的trace_id
    { valid_record | trace_id: "77777777777777777777777777777777", span_id: "88888888888888888888888888888888" }
  ]
  
  let duplicate_batch_result = batch_validator.validate_batch(duplicate_batch)
  assert_false(duplicate_batch_result.is_valid)
  assert_true(duplicate_batch_result.errors.any(fn(error) { error.contains("Duplicate trace_ids found") }))
  
  // 测试数据质量报告生成
  let generate_quality_report = fn(batch_result: {
    total_records: Int,
    valid_records: Int,
    invalid_records: Int,
    is_valid: Bool,
    errors: Array[String],
    warnings: Array[String]>
  }) {
    let quality_score = (batch_result.valid_records as Float) / (batch_result.total_records as Float) * 100.0
    
    let error_summary = batch_result.errors.reduce(fn(acc, error) {
      let parts = error.split(": ")
      if parts.length() >= 2 {
        let error_type = parts[1]
        let current_count = match Map::get(acc, error_type) {
          Some(count) => count
          None => 0
        }
        Map::insert(acc, error_type, current_count + 1)
      } else {
        acc
      }
    }, Map::empty())
    
    {
      total_records: batch_result.total_records,
      valid_records: batch_result.valid_records,
      invalid_records: batch_result.invalid_records,
      quality_score: quality_score.round(),
      error_count: batch_result.errors.length(),
      warning_count: batch_result.warnings.length(),
      error_summary
    }
  }
  
  let quality_report = generate_quality_report(mixed_batch_result)
  assert_eq(quality_report.total_records, 3)
  assert_eq(quality_report.valid_records, 2)
  assert_eq(quality_report.invalid_records, 1)
  assert_eq(quality_report.quality_score, 67.0)
  assert_eq(quality_report.error_count, 1)
  
  // 测试数据修复建议
  let suggest_fixes = fn(record: TelemetryRecord, validation_result: ValidationResult) {
    let fixes = []
    
    for error in validation_result.errors {
      if error.contains("trace_id is required") {
        fixes = fixes.push("Set trace_id to a valid UUID or hex string")
      } else if error.contains("span_id is required") {
        fixes = fixes.push("Set span_id to a valid UUID or hex string")
      } else if error.contains("operation_name is required") {
        fixes = fixes.push("Set operation_name to describe the operation")
      } else if error.contains("must be at least") {
        fixes = fixes.push("Ensure field meets minimum length requirements")
      } else if error.contains("must be at most") {
        fixes = fixes.push("Truncate field to meet maximum length requirements")
      } else if error.contains("must contain only hexadecimal") {
        fixes = fixes.push("Use only hexadecimal characters and hyphens in ID fields")
      } else if error.contains("duration must be between") {
        fixes = fixes.push("Check timestamps and ensure reasonable duration")
      } else if error.contains("start_time must be before end_time") {
        fixes = fixes.push("Correct timestamp order")
      }
    }
    
    fixes
  }
  
  let fixes = suggest_fixes(missing_trace_id, missing_field_result)
  assert_true(fixes.contains("Set trace_id to a valid UUID or hex string"))
}

// 测试8: 遥测系统配置管理和动态更新
test "遥测系统配置管理和动态更新" {
  // 定义配置类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String>)
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    required: Bool,
    restart_required: Bool
  }
  
  // 定义配置变更
  type ConfigChange = {
    key: String,
    old_value: ConfigValue,
    new_value: ConfigValue,
    timestamp: Int,
    source: String  // 变更来源，如"file", "env", "api"
  }
  
  // 定义配置管理器
  type ConfigManager = {
    get: (String) -> Option[ConfigValue],
    set: (String, ConfigValue, String) -> Result[Unit, String]>,
    get_all: () -> Array[ConfigItem],
    get_changes: () -> Array[ConfigChange],
    reload: () -> Result[Unit, String]>
  }
  
  // 创建默认配置
  let create_default_config = fn() {
    [
      {
        key: "service.name",
        value: ConfigValue::StringValue("azimuth-service"),
        default_value: ConfigValue::StringValue("azimuth-service"),
        description: "Service name for telemetry",
        required: true,
        restart_required: false
      },
      {
        key: "service.version",
        value: ConfigValue::StringValue("1.0.0"),
        default_value: ConfigValue::StringValue("1.0.0"),
        description: "Service version",
        required: true,
        restart_required: false
      },
      {
        key: "telemetry.enabled",
        value: ConfigValue::BoolValue(true),
        default_value: ConfigValue::BoolValue(true),
        description: "Enable/disable telemetry collection",
        required: false,
        restart_required: false
      },
      {
        key: "telemetry.sampling.ratio",
        value: ConfigValue::FloatValue(0.1),
        default_value: ConfigValue::FloatValue(0.1),
        description: "Sampling ratio for traces",
        required: false,
        restart_required: false
      },
      {
        key: "telemetry.batch.size",
        value: ConfigValue::IntValue(100),
        default_value: ConfigValue::IntValue(100),
        description: "Batch size for telemetry data",
        required: false,
        restart_required: false
      },
      {
        key: "telemetry.exporter.endpoint",
        value: ConfigValue::StringValue("http://localhost:4317"),
        default_value: ConfigValue::StringValue("http://localhost:4317"),
        description: "Telemetry exporter endpoint",
        required: false,
        restart_required: true
      },
      {
        key: "telemetry.attributes",
        value: ConfigValue::ArrayValue(["environment=production", "region=us-west-2"]),
        default_value: ConfigValue::ArrayValue(["environment=production"]),
        description: "Global telemetry attributes",
        required: false,
        restart_required: false
      }
    ]
  }
  
  // 创建配置管理器
  let create_config_manager = fn() {
    let mut config_items = create_default_config()
    let mut change_history = []
    
    // 查找配置项
    let find_config_item = fn(key: String) {
      config_items.find(fn(item) { item.key == key })
    }
    
    // 获取配置值
    let get = fn(key: String) {
      match find_config_item(key) {
        Some(item) => Some(item.value)
        None => None
      }
    }
    
    // 设置配置值
    let set = fn(key: String, new_value: ConfigValue, source: String) {
      match find_config_item(key) {
        Some(item) => {
          let old_value = item.value
          
          // 验证配置值
          let validation_result = validate_config_value(key, new_value)
          match validation_result {
            Ok(()) => {
              // 更新配置项
              let updated_item = { item | value: new_value }
              
              // 更新配置列表
              let mut new_config_items = []
              for config_item in config_items {
                if config_item.key == key {
                  new_config_items = new_config_items.push(updated_item)
                } else {
                  new_config_items = new_config_items.push(config_item)
                }
              }
              config_items = new_config_items
              
              // 记录变更历史
              let change = {
                key,
                old_value,
                new_value,
                timestamp: Time::now(),
                source
              }
              change_history = change_history.push(change)
              
              Ok(())
            }
            Err(error) => Err(error)
          }
        }
        None => Err("Configuration key not found: " + key)
      }
    }
    
    // 获取所有配置
    let get_all = fn() { config_items }
    
    // 获取变更历史
    let get_changes = fn() { change_history }
    
    // 重新加载配置
    let reload = fn() {
      // 在实际实现中，这里会从文件或环境变量重新加载配置
      // 这里只是模拟
      Ok(())
    }
    
    // 配置值验证函数
    let validate_config_value = fn(key: String, value: ConfigValue) {
      match key {
        "telemetry.sampling.ratio" => {
          match value {
            ConfigValue::FloatValue(ratio) => {
              if ratio >= 0.0 && ratio <= 1.0 {
                Ok(())
              } else {
                Err("Sampling ratio must be between 0.0 and 1.0")
              }
            }
            _ => Err("Sampling ratio must be a float value")
          }
        }
        "telemetry.batch.size" => {
          match value {
            ConfigValue::IntValue(size) => {
              if size > 0 {
                Ok(())
              } else {
                Err("Batch size must be positive")
              }
            }
            _ => Err("Batch size must be an integer value")
          }
        }
        "service.name" => {
          match value {
            ConfigValue::StringValue(name) => {
              if name.length() > 0 {
                Ok(())
              } else {
                Err("Service name cannot be empty")
              }
            }
            _ => Err("Service name must be a string value")
          }
        }
        "service.version" => {
          match value {
            ConfigValue::StringValue(version) => {
              if version.length() > 0 {
                Ok(())
              } else {
                Err("Service version cannot be empty")
              }
            }
            _ => Err("Service version must be a string value")
          }
        }
        "telemetry.enabled" => {
          match value {
            ConfigValue::BoolValue(_) => Ok(())
            _ => Err("Telemetry enabled must be a boolean value")
          }
        }
        "telemetry.exporter.endpoint" => {
          match value {
            ConfigValue::StringValue(endpoint) => {
              if endpoint.starts_with("http://") || endpoint.starts_with("https://") {
                Ok(())
              } else {
                Err("Exporter endpoint must be a valid URL")
              }
            }
            _ => Err("Exporter endpoint must be a string value")
          }
        }
        _ => Ok(())
      }
    }
    
    { get, set, get_all, get_changes, reload }
  }
  
  // 创建配置管理器实例
  let config_manager = create_config_manager()
  
  // 测试获取配置
  let service_name = config_manager.get("service.name")
  match service_name {
    Some(ConfigValue::StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let sampling_ratio = config_manager.get("telemetry.sampling.ratio")
  match sampling_ratio {
    Some(ConfigValue::FloatValue(ratio)) => assert_eq(ratio, 0.1)
    _ => assert_true(false)
  }
  
  let missing_config = config_manager.get("nonexistent.key")
  assert_eq(missing_config, None)
  
  // 测试设置配置
  let set_result1 = config_manager.set("telemetry.sampling.ratio", ConfigValue::FloatValue(0.5), "test")
  assert_eq(set_result1, Ok(()))
  
  let updated_ratio = config_manager.get("telemetry.sampling.ratio")
  match updated_ratio {
    Some(ConfigValue::FloatValue(ratio)) => assert_eq(ratio, 0.5)
    _ => assert_true(false)
  }
  
  // 测试无效配置值
  let invalid_ratio_result = config_manager.set("telemetry.sampling.ratio", ConfigValue::FloatValue(1.5), "test")
  match invalid_ratio_result {
    Err(error) => assert_eq(error, "Sampling ratio must be between 0.0 and 1.0")
    _ => assert_true(false)
  }
  
  // 测试无效配置类型
  let invalid_type_result = config_manager.set("telemetry.sampling.ratio", ConfigValue::StringValue("invalid"), "test")
  match invalid_type_result {
    Err(error) => assert_eq(error, "Sampling ratio must be a float value")
    _ => assert_true(false)
  }
  
  // 测试不存在的配置键
  let nonexistent_key_result = config_manager.set("nonexistent.key", ConfigValue::StringValue("value"), "test")
  match nonexistent_key_result {
    Err(error) => assert_true(error.contains("Configuration key not found"))
    _ => assert_true(false)
  }
  
  // 测试获取所有配置
  let all_configs = config_manager.get_all()
  assert_eq(all_configs.length(), 7)
  
  let service_config = all_configs.find(fn(item) { item.key == "service.name" })
  match service_config {
    Some(item) => {
      assert_eq(item.description, "Service name for telemetry")
      assert_true(item.required)
      assert_false(item.restart_required)
    }
    None => assert_true(false)
  }
  
  let exporter_config = all_configs.find(fn(item) { item.key == "telemetry.exporter.endpoint" })
  match exporter_config {
    Some(item) => {
      assert_false(item.required)
      assert_true(item.restart_required)
    }
    None => assert_true(false)
  }
  
  // 测试变更历史
  let changes = config_manager.get_changes()
  assert_eq(changes.length(), 1)
  assert_eq(changes[0].key, "telemetry.sampling.ratio")
  
  match changes[0].old_value {
    ConfigValue::FloatValue(old_ratio) => assert_eq(old_ratio, 0.1)
    _ => assert_true(false)
  }
  
  match changes[0].new_value {
    ConfigValue::FloatValue(new_ratio)) => assert_eq(new_ratio, 0.5)
    _ => assert_true(false)
  }
  
  assert_eq(changes[0].source, "test")
  
  // 测试多个变更
  let _ = config_manager.set("service.version", ConfigValue::StringValue("1.1.0"), "api")
  let _ = config_manager.set("telemetry.enabled", ConfigValue::BoolValue(false), "file")
  
  let updated_changes = config_manager.get_changes()
  assert_eq(updated_changes.length(), 3)
  
  // 验证变更顺序
  assert_eq(updated_changes[0].key, "telemetry.sampling.ratio")
  assert_eq(updated_changes[1].key, "service.version")
  assert_eq(updated_changes[2].key, "telemetry.enabled")
  
  // 测试配置重载
  let reload_result = config_manager.reload()
  assert_eq(reload_result, Ok(()))
  
  // 测试配置验证
  let validate_all_configs = fn(configs: Array[ConfigItem>) {
    let mut errors = []
    
    for config in configs {
      if config.required {
        match config.value {
          ConfigValue::StringValue(value) => {
            if value.length() == 0 {
              errors = errors.push(config.key + " is required but empty")
            }
          }
          ConfigValue::ArrayValue(values) => {
            if values.length() == 0 {
              errors = errors.push(config.key + " is required but empty")
            }
          }
          _ => {}  // 其他类型默认非空
        }
      }
    }
    
    errors
  }
  
  let validation_errors = validate_all_configs(config_manager.get_all())
  assert_eq(validation_errors.length(), 0)
  
  // 测试配置导出/导入
  let export_config = fn(configs: Array[ConfigItem>) {
    configs.map(fn(config) {
      let value_str = match config.value {
        ConfigValue::StringValue(v) => "\"" + v + "\""
        ConfigValue::IntValue(v) => v.to_string()
        ConfigValue::FloatValue(v) => v.to_string()
        ConfigValue::BoolValue(v) => if v { "true" } else { "false" }
        ConfigValue::ArrayValue(values) => "[" + values.map(fn(v) { "\"" + v + "\"" }).join(",") + "]"
      }
      config.key + "=" + value_str
    }).join("\n")
  }
  
  let exported_config = export_config(config_manager.get_all())
  assert_true(exported_config.contains("service.name=\"azimuth-service\""))
  assert_true(exported_config.contains("telemetry.sampling.ratio=0.5"))
  assert_true(exported_config.contains("telemetry.enabled=false"))
  
  // 测试配置模板生成
  let generate_config_template = fn(configs: Array[ConfigItem>) {
    configs.map(fn(config) {
      let default_value_str = match config.default_value {
        ConfigValue::StringValue(v) => "\"" + v + "\""
        ConfigValue::IntValue(v) => v.to_string()
        ConfigValue::FloatValue(v) => v.to_string()
        ConfigValue::BoolValue(v) => if v { "true" } else { "false" }
        ConfigValue::ArrayValue(values) => "[" + values.map(fn(v) { "\"" + v + "\"" }).join(",") + "]"
      }
      
      let required_marker = if config.required { " (required)" } else { "" }
      let restart_marker = if config.restart_required { " (restart required)" } else { "" }
      
      "# " + config.description + required_marker + restart_marker + "\n" +
      config.key + "=" + default_value_str + "\n"
    }).join("\n")
  }
  
  let config_template = generate_config_template(create_default_config())
  assert_true(config_template.contains("# Service name for telemetry (required)"))
  assert_true(config_template.contains("service.name=\"azimuth-service\""))
  assert_true(config_template.contains("# Enable/disable telemetry collection"))
  assert_true(config_template.contains("# Telemetry exporter endpoint (restart required)"))
}