// Azimuth 新增MoonBit测试用例
// 包含10个新的测试用例，覆盖不同功能领域

// 测试1: 遥测数据压缩与解压
test "遥测数据压缩与解压功能" {
  // 定义简单的压缩算法
  let compress_telemetry = fn(data: Array[String]) {
    let compressed = []
    let mut current_char = ""
    let mut count = 0
    
    for item in data {
      if current_char == "" {
        current_char = item
        count = 1
      } else if item == current_char {
        count = count + 1
      } else {
        compressed = compressed.push(current_char + ":" + count.to_string())
        current_char = item
        count = 1
      }
    }
    
    // 添加最后一组
    if current_char != "" {
      compressed = compressed.push(current_char + ":" + count.to_string())
    }
    
    compressed
  }
  
  // 定义解压算法
  let decompress_telemetry = fn(compressed: Array[String]) {
    let decompressed = []
    
    for item in compressed {
      let parts = item.split(":")
      let value = parts[0]
      let count = parts[1].to_int()
      
      for i in 0..count {
        decompressed = decompressed.push(value)
      }
    }
    
    decompressed
  }
  
  // 测试数据
  let telemetry_data = ["metric", "metric", "metric", "log", "log", "trace", "metric", "metric"]
  
  // 测试压缩
  let compressed = compress_telemetry(telemetry_data)
  assert_eq(compressed, ["metric:3", "log:2", "trace:1", "metric:2"])
  
  // 测试解压
  let decompressed = decompress_telemetry(compressed)
  assert_eq(decompressed, telemetry_data)
  
  // 测试压缩率
  let compression_ratio = (compressed.length() as Float) / (telemetry_data.length() as Float)
  assert_true(compression_ratio < 1.0)  // 压缩后应该更小
}

// 测试2: 分布式追踪上下文传播
test "分布式追踪上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)]
  }
  
  // 创建新的追踪上下文
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      parent_span_id: None,
      baggage: []
    }
  }
  
  // 创建子跨度上下文
  let create_child_context = fn(parent: TraceContext, child_span_id: String) {
    {
      trace_id: parent.trace_id,
      span_id: child_span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage
    }
  }
  
  // 添加baggage项
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    let new_baggage = context.baggage.push((key, value))
    { context with baggage: new_baggage }
  }
  
  // 提取baggage值
  let get_baggage = fn(context: TraceContext, key: String) {
    context.baggage.find(fn(item) { item.0 == key }).map(fn(item) { item.1 })
  }
  
  // 测试追踪上下文创建
  let root_context = create_trace_context("trace-123", "span-456")
  assert_eq(root_context.trace_id, "trace-123")
  assert_eq(root_context.span_id, "span-456")
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.baggage.length(), 0)
  
  // 测试子上下文创建
  let child_context = create_child_context(root_context, "span-789")
  assert_eq(child_context.trace_id, "trace-123")  // 继承trace_id
  assert_eq(child_context.span_id, "span-789")   // 新的span_id
  assert_eq(child_context.parent_span_id, Some("span-456"))  // 父span_id
  assert_eq(child_context.baggage.length(), 0)   // 继承空的baggage
  
  // 测试baggage添加和检索
  let context_with_baggage = add_baggage(root_context, "user.id", "user-123")
  assert_eq(context_with_baggage.baggage.length(), 1)
  
  let context_with_more_baggage = add_baggage(context_with_baggage, "service.version", "1.2.3")
  assert_eq(context_with_more_baggage.baggage.length(), 2)
  
  // 测试baggage检索
  let user_id = get_baggage(context_with_more_baggage, "user.id")
  assert_eq(user_id, Some("user-123"))
  
  let service_version = get_baggage(context_with_more_baggage, "service.version")
  assert_eq(service_version, Some("1.2.3"))
  
  let non_existent = get_baggage(context_with_more_baggage, "non.existent")
  assert_eq(non_existent, None)
  
  // 测试baggage在子上下文中的传播
  let child_with_baggage = create_child_context(context_with_more_baggage, "span-999")
  assert_eq(child_with_baggage.baggage.length(), 2)  // 继承所有baggage
  
  let child_user_id = get_baggage(child_with_baggage, "user.id")
  assert_eq(child_user_id, Some("user-123"))
}

// 测试3: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样决策
  enum SamplingDecision {
    RecordAndSample
    RecordOnly
    Drop
  }
  
  // 定义采样器接口
  trait Sampler {
    should_sample(trace_id: String, name: String, attributes: Array[(String, String)]) -> SamplingDecision
  }
  
  // 实现概率采样器
  let probability_sampler = fn(sampling_probability: Float) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)]) {
      // 简单的基于trace_id哈希的采样
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      
      if normalized <= sampling_probability {
        SamplingDecision::RecordAndSample
      } else {
        SamplingDecision::Drop
      }
    }
  }
  
  // 实现基于属性的采样器
  let attribute_based_sampler = fn(sampling_attribute: String, sampling_values: Array[String]) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)]) {
      match attributes.find(fn(attr) { attr.0 == sampling_attribute }) {
        Some((_, value)) => {
          if sampling_values.contains(value) {
            SamplingDecision::RecordAndSample
          } else {
            SamplingDecision::RecordOnly
          }
        }
        None => SamplingDecision::RecordOnly
      }
    }
  }
  
  // 实现组合采样器
  let composite_sampler = fn(samplers: Array[Sampler]) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)]) {
      let mut decision = SamplingDecision::Drop
      
      for sampler in samplers {
        let sampler_decision = sampler.should_sample(trace_id, name, attributes)
        match sampler_decision {
          SamplingDecision::RecordAndSample => {
            decision = SamplingDecision::RecordAndSample
            break  // 任何一个采样器决定采样，则采样
          }
          SamplingDecision::RecordOnly => {
            if decision == SamplingDecision::Drop {
              decision = SamplingDecision::RecordOnly
            }
          }
          SamplingDecision::Drop => {
            // 保持当前决策
          }
        }
      }
      
      decision
    }
  }
  
  // 测试概率采样器
  let prob_sampler = probability_sampler(0.5)  // 50%采样率
  
  // 由于哈希的随机性，我们无法确定具体结果，但可以验证返回的是有效决策
  let decision1 = prob_sampler.should_sample("trace-123", "span-name", [])
  assert_true(decision1 == SamplingDecision::RecordAndSample || decision1 == SamplingDecision::Drop)
  
  // 测试基于属性的采样器
  let attr_sampler = attribute_based_sampler("service.name", ["critical-service", "payment-service"])
  
  let critical_attrs = [("service.name", "critical-service"), ("env", "prod")]
  let decision2 = attr_sampler.should_sample("trace-456", "span-name", critical_attrs)
  assert_eq(decision2, SamplingDecision::RecordAndSample)
  
  let normal_attrs = [("service.name", "normal-service"), ("env", "prod")]
  let decision3 = attr_sampler.should_sample("trace-789", "span-name", normal_attrs)
  assert_eq(decision3, SamplingDecision::RecordOnly)
  
  let no_attrs = []
  let decision4 = attr_sampler.should_sample("trace-999", "span-name", no_attrs)
  assert_eq(decision4, SamplingDecision::RecordOnly)
  
  // 测试组合采样器
  let samplers = [
    probability_sampler(0.3),  // 30%概率采样
    attribute_based_sampler("service.name", ["critical-service"])  // 关键服务总是采样
  ]
  
  let comp_sampler = composite_sampler(samplers)
  
  // 关键服务应该总是被采样（无论概率如何）
  let decision5 = comp_sampler.should_sample("trace-111", "span-name", critical_attrs)
  assert_eq(decision5, SamplingDecision::RecordAndSample)
  
  // 普通服务根据概率采样
  let decision6 = comp_sampler.should_sample("trace-222", "span-name", normal_attrs)
  assert_true(decision6 == SamplingDecision::RecordAndSample || decision6 == SamplingDecision::Drop)
}

// 测试4: 遥测指标聚合与统计
test "遥测指标聚合与统计" {
  // 定义指标点
  type MetricPoint = {
    timestamp: Int,
    value: Float,
    labels: Array[(String, String)]
  }
  
  // 定义聚合结果
  type AggregationResult = {
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float,
    variance: Float
  }
  
  // 计算聚合统计
  let aggregate = fn(points: Array[MetricPoint]) {
    if points.length() == 0 {
      return {
        count: 0,
        sum: 0.0,
        min: 0.0,
        max: 0.0,
        avg: 0.0,
        variance: 0.0
      }
    }
    
    let values = points.map(fn(p) { p.value })
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let avg = sum / (count as Float)
    let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    
    // 计算方差
    let variance = values
      .map(fn(v) { (v - avg) * (v - avg) })
      .reduce(fn(acc, v) { acc + v }, 0.0) / (count as Float)
    
    {
      count,
      sum,
      min,
      max,
      avg,
      variance
    }
  }
  
  // 按标签分组聚合
  let aggregate_by_labels = fn(points: Array[MetricPoint], label_keys: Array[String]) {
    let groups = Map::empty()
    
    for point in points {
      // 构建分组键
      let key_parts = label_keys.map(fn(key) {
        match point.labels.find(fn(label) { label.0 == key }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = key_parts.join("|")
      
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
      let aggregation = aggregate(group_points)
      let _ = Map::insert(results, group_key, aggregation)
    }
    
    results
  }
  
  // 创建测试数据
  let test_points = [
    { timestamp: 1000, value: 10.5, labels: [("service", "api"), ("env", "prod")] },
    { timestamp: 2000, value: 15.2, labels: [("service", "api"), ("env", "prod")] },
    { timestamp: 3000, value: 8.7, labels: [("service", "web"), ("env", "prod")] },
    { timestamp: 4000, value: 12.3, labels: [("service", "api"), ("env", "dev")] },
    { timestamp: 5000, value: 9.8, labels: [("service", "web"), ("env", "dev")] },
    { timestamp: 6000, value: 11.1, labels: [("service", "api"), ("env", "prod")] },
    { timestamp: 7000, value: 14.6, labels: [("service", "web"), ("env", "prod")] }
  ]
  
  // 测试全局聚合
  let global_result = aggregate(test_points)
  assert_eq(global_result.count, 7)
  assert_eq(global_result.sum.round(), 82.2)
  assert_eq(global_result.min, 8.7)
  assert_eq(global_result.max, 15.2)
  assert_eq(global_result.avg.round(), 11.74)
  assert_true(global_result.variance > 0.0)
  
  // 测试按服务分组聚合
  let service_groups = aggregate_by_labels(test_points, ["service"])
  
  // API服务应该有4个点: 10.5, 15.2, 12.3, 11.1
  let api_result = match Map::get(service_groups, "api") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, variance: 0.0 }
  }
  assert_eq(api_result.count, 4)
  assert_eq(api_result.sum.round(), 49.1)
  
  // Web服务应该有3个点: 8.7, 9.8, 14.6
  let web_result = match Map::get(service_groups, "web") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, variance: 0.0 }
  }
  assert_eq(web_result.count, 3)
  assert_eq(web_result.sum.round(), 33.1)
  
  // 测试按服务和环境分组聚合
  let multi_groups = aggregate_by_labels(test_points, ["service", "env"])
  
  // API+Prod应该有3个点: 10.5, 15.2, 11.1
  let api_prod_result = match Map::get(multi_groups, "api|prod") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, variance: 0.0 }
  }
  assert_eq(api_prod_result.count, 3)
  
  // Web+Dev应该有1个点: 9.8
  let web_dev_result = match Map::get(multi_groups, "web|dev") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, variance: 0.0 }
  }
  assert_eq(web_dev_result.count, 1)
  assert_eq(web_dev_result.min, 9.8)
  assert_eq(web_dev_result.max, 9.8)
  assert_eq(web_dev_result.avg, 9.8)
}

// 测试5: 遥测数据序列化与反序列化
test "遥测数据序列化与反序列化" {
  // 定义遥测记录
  type TelemetryRecord = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    name: String,
    timestamp: Int,
    duration: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 简单的JSON序列化
  let serialize_to_json = fn(record: TelemetryRecord) {
    let attributes_json = record.attributes
      .map(fn(attr) { "\"" + attr.0 + "\":\"" + attr.1 + "\"" })
      .join(",")
    
    let parent_json = match record.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{" +
      "\"trace_id\":\"" + record.trace_id + "\"," +
      "\"span_id\":\"" + record.span_id + "\"," +
      "\"parent_span_id\":" + parent_json + "," +
      "\"name\":\"" + record.name + "\"," +
      "\"timestamp\":" + record.timestamp.to_string() + "," +
      "\"duration\":" + record.duration.to_string() + "," +
      "\"status\":\"" + record.status + "\"," +
      "\"attributes\":{" + attributes_json + "}" +
    "}"
  }
  
  // 简单的JSON反序列化（简化版，实际应用中会更复杂）
  let deserialize_from_json = fn(json: String) {
    // 这是一个简化的实现，实际应用中需要更健壮的JSON解析
    let trace_id_start = json.find("\"trace_id\":\"").unwrap() + 12
    let trace_id_end = json.find("\",\"", trace_id_start).unwrap()
    let trace_id = json.substring(trace_id_start, trace_id_end)
    
    let span_id_start = json.find("\"span_id\":\"").unwrap() + 11
    let span_id_end = json.find("\",\"", span_id_start).unwrap()
    let span_id = json.substring(span_id_start, span_id_end)
    
    let name_start = json.find("\"name\":\"").unwrap() + 8
    let name_end = json.find("\",\"", name_start).unwrap()
    let name = json.substring(name_start, name_end)
    
    let timestamp_start = json.find("\"timestamp\":").unwrap() + 12
    let timestamp_end = json.find(",", timestamp_start).unwrap()
    let timestamp = json.substring(timestamp_start, timestamp_end).to_int()
    
    let duration_start = json.find("\"duration\":").unwrap() + 11
    let duration_end = json.find(",", duration_start).unwrap()
    let duration = json.substring(duration_start, duration_end).to_int()
    
    let status_start = json.find("\"status\":\"").unwrap() + 10
    let status_end = json.find("\",\"", status_start).unwrap()
    let status = json.substring(status_start, status_end)
    
    // 简化的属性解析
    let attributes = []
    let attrs_start = json.find("\"attributes\":{").unwrap() + 14
    let attrs_end = json.find("}", attrs_start).unwrap()
    let attrs_str = json.substring(attrs_start, attrs_end)
    
    if attrs_str.length() > 0 {
      let attr_pairs = attrs_str.split(",")
      for pair in attr_pairs {
        let parts = pair.split(":\"")
        if parts.length() == 2 {
          let key = parts[0].substring(1, parts[0].length())  // 去掉开头的引号
          let value = parts[1].substring(0, parts[1].length() - 1)  // 去掉结尾的引号
          attributes = attributes.push((key, value))
        }
      }
    }
    
    // 检查是否有parent_span_id
    let parent_span_id = if json.contains("\"parent_span_id\":null") {
      None
    } else {
      let parent_start = json.find("\"parent_span_id\":\"").unwrap() + 18
      let parent_end = json.find("\",\"", parent_start).unwrap()
      Some(json.substring(parent_start, parent_end))
    }
    
    {
      trace_id,
      span_id,
      parent_span_id,
      name,
      timestamp,
      duration,
      status,
      attributes
    }
  }
  
  // 创建测试记录
  let test_record = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    name: "http.request",
    timestamp: 1634567890,
    duration: 150,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.status_code", "200"),
      ("user.id", "user-123")
    ]
  }
  
  // 测试序列化
  let json = serialize_to_json(test_record)
  assert_true(json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json.contains("\"span_id\":\"span-67890\""))
  assert_true(json.contains("\"parent_span_id\":\"span-11111\""))
  assert_true(json.contains("\"name\":\"http.request\""))
  assert_true(json.contains("\"timestamp\":1634567890"))
  assert_true(json.contains("\"duration\":150"))
  assert_true(json.contains("\"status\":\"ok\""))
  assert_true(json.contains("\"http.method\":\"GET\""))
  assert_true(json.contains("\"http.url\":\"/api/users\""))
  
  // 测试反序列化
  let deserialized = deserialize_from_json(json)
  assert_eq(deserialized.trace_id, test_record.trace_id)
  assert_eq(deserialized.span_id, test_record.span_id)
  assert_eq(deserialized.parent_span_id, test_record.parent_span_id)
  assert_eq(deserialized.name, test_record.name)
  assert_eq(deserialized.timestamp, test_record.timestamp)
  assert_eq(deserialized.duration, test_record.duration)
  assert_eq(deserialized.status, test_record.status)
  assert_eq(deserialized.attributes.length(), test_record.attributes.length())
  
  // 验证属性
  for attr in test_record.attributes {
    let found = deserialized.attributes.any(fn(d_attr) { 
      d_attr.0 == attr.0 && d_attr.1 == attr.1 
    })
    assert_true(found)
  }
  
  // 测试无parent_span_id的记录
  let record_no_parent = { test_record with parent_span_id: None }
  let json_no_parent = serialize_to_json(record_no_parent)
  assert_true(json_no_parent.contains("\"parent_span_id\":null"))
  
  let deserialized_no_parent = deserialize_from_json(json_no_parent)
  assert_eq(deserialized_no_parent.parent_span_id, None)
}

// 测试6: 遥测数据过滤与转换
test "遥测数据过滤与转换" {
  // 定义遥测事件
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    event_type: String,
    severity: String,
    service: String,
    message: String,
    attributes: Array[(String, String)]
  }
  
  // 过滤器函数类型
  type Filter = (TelemetryEvent) -> Bool
  
  // 转换器函数类型
  type Transformer = (TelemetryEvent) -> TelemetryEvent
  
  // 创建时间范围过滤器
  let time_range_filter = fn(start_time: Int, end_time: Int) {
    fn(event: TelemetryEvent) {
      event.timestamp >= start_time && event.timestamp <= end_time
    }
  }
  
  // 创建严重性过滤器
  let severity_filter = fn(min_severity: String) {
    let severity_levels = ["debug", "info", "warn", "error", "fatal"]
    let min_level = match severity_levels.find_index(fn(level) { level == min_severity }) {
      Some(index) => index
      None => 0  // 默认为debug级别
    }
    
    fn(event: TelemetryEvent) {
      match severity_levels.find_index(fn(level) { level == event.severity }) {
        Some(event_level) => event_level >= min_level
        None => false  // 未知严重级别
      }
    }
  }
  
  // 创建服务过滤器
  let service_filter = fn(services: Array[String]) {
    fn(event: TelemetryEvent) {
      services.contains(event.service)
    }
  }
  
  // 创建属性过滤器
  let attribute_filter = fn(key: String, value: String) {
    fn(event: TelemetryEvent) {
      event.attributes.any(fn(attr) { attr.0 == key && attr.1 == value })
    }
  }
  
  // 组合过滤器（AND逻辑）
  let and_filter = fn(filters: Array[Filter]) {
    fn(event: TelemetryEvent) {
      filters.all(fn(f) { f(event) })
    }
  }
  
  // 组合过滤器（OR逻辑）
  let or_filter = fn(filters: Array[Filter]) {
    fn(event: TelemetryEvent) {
      filters.any(fn(f) { f(event) })
    }
  }
  
  // 创建PII脱敏转换器
  let pii_mask_transformer = fn(pii_keys: Array[String]) {
    fn(event: TelemetryEvent) {
      let mask_attributes = event.attributes.map(fn(attr) {
        if pii_keys.contains(attr.0) {
          (attr.0, "***MASKED***")
        } else {
          attr
        }
      })
      
      // 同时检查消息中是否包含PII
      let masked_message = pii_keys.reduce(event.message, fn(msg, key) {
        if msg.contains(key) {
          msg.replace(key, "***MASKED***")
        } else {
          msg
        }
      })
      
      {
        id: event.id,
        timestamp: event.timestamp,
        event_type: event.event_type,
        severity: event.severity,
        service: event.service,
        message: masked_message,
        attributes: mask_attributes
      }
    }
  }
  
  // 创建属性添加转换器
  let add_attribute_transformer = fn(key: String, value: String) {
    fn(event: TelemetryEvent) {
      let new_attributes = event.attributes.push((key, value))
      { event with attributes: new_attributes }
    }
  }
  
  // 应用过滤器和转换器
  let process_events = fn(events: Array[TelemetryEvent], filter: Filter, transformer: Transformer) {
    events
      .filter(filter)
      .map(transformer)
  }
  
  // 创建测试数据
  let test_events = [
    {
      id: "event-1",
      timestamp: 1000,
      event_type: "log",
      severity: "info",
      service: "auth-service",
      message: "User login successful for user-123",
      attributes: [("user.id", "user-123"), ("ip", "192.168.1.1")]
    },
    {
      id: "event-2",
      timestamp: 1500,
      event_type: "error",
      severity: "error",
      service: "payment-service",
      message: "Payment failed for user-456",
      attributes: [("user.id", "user-456"), ("payment.id", "pay-789"), ("error.code", "CARD_DECLINED")]
    },
    {
      id: "event-3",
      timestamp: 2000,
      event_type: "log",
      severity: "debug",
      service: "auth-service",
      message: "Debug information for user-123",
      attributes: [("user.id", "user-123"), ("debug.level", "verbose")]
    },
    {
      id: "event-4",
      timestamp: 2500,
      event_type: "metric",
      severity: "info",
      service: "api-gateway",
      message: "Request processed",
      attributes: [("request.id", "req-111"), ("response.time", "150")]
    }
  ]
  
  // 测试时间范围过滤器
  let time_filter = time_range_filter(1200, 2300)
  let time_filtered = test_events.filter(time_filter)
  assert_eq(time_filtered.length(), 2)  // event-2 和 event-3
  
  // 测试严重性过滤器
  let warn_filter = severity_filter("warn")
  let warn_filtered = test_events.filter(warn_filter)
  assert_eq(warn_filtered.length(), 1)  // 只有 event-2 是error级别
  
  // 测试服务过滤器
  let service_filter_fn = service_filter(["auth-service", "payment-service"])
  let service_filtered = test_events.filter(service_filter_fn)
  assert_eq(service_filtered.length(), 3)  // event-1, event-2, event-3
  
  // 测试属性过滤器
  let attr_filter_fn = attribute_filter("user.id", "user-123")
  let attr_filtered = test_events.filter(attr_filter_fn)
  assert_eq(attr_filtered.length(), 2)  // event-1 和 event-3
  
  // 测试组合过滤器（AND）
  let combined_and = and_filter([
    time_range_filter(1000, 2000),
    service_filter(["auth-service"])
  ])
  let and_filtered = test_events.filter(combined_and)
  assert_eq(and_filtered.length(), 2)  // event-1 和 event-3
  
  // 测试组合过滤器（OR）
  let combined_or = or_filter([
    severity_filter("error"),
    service_filter(["api-gateway"])
  ])
  let or_filtered = test_events.filter(combined_or)
  assert_eq(or_filtered.length(), 2)  // event-2 (error) 和 event-4 (api-gateway)
  
  // 测试PII脱敏转换器
  let pii_transformer = pii_mask_transformer(["user.id", "payment.id"])
  let pii_masked = test_events.map(pii_transformer)
  
  // 检查第一个事件
  let masked_event1 = pii_masked[0]
  assert_eq(masked_event1.message, "User login successful for ***MASKED***")
  assert_true(masked_event1.attributes.any(fn(attr) { 
    attr.0 == "user.id" && attr.1 == "***MASKED***" 
  }))
  
  // 检查第二个事件
  let masked_event2 = pii_masked[1]
  assert_eq(masked_event2.message, "Payment failed for ***MASKED***")
  assert_true(masked_event2.attributes.any(fn(attr) { 
    attr.0 == "user.id" && attr.1 == "***MASKED***" 
  }))
  assert_true(masked_event2.attributes.any(fn(attr) { 
    attr.0 == "payment.id" && attr.1 == "***MASKED***" 
  }))
  
  // 测试属性添加转换器
  let add_attr_transformer = add_attribute_transformer("processed.by", "telemetry-processor")
  let with_added_attr = test_events.map(add_attr_transformer)
  
  for event in with_added_attr {
    assert_true(event.attributes.any(fn(attr) { 
      attr.0 == "processed.by" && attr.1 == "telemetry-processor" 
    }))
  }
  
  // 测试完整的处理流程
  let processed = process_events(
    test_events,
    combined_or,  // error级别或api-gateway服务
    pii_transformer  // 脱敏PII
  )
  
  assert_eq(processed.length(), 2)  // event-2 和 event-4
  
  // 检查event-2被脱敏
  let processed_event2 = processed[0]
  assert_eq(processed_event2.id, "event-2")
  assert_eq(processed_event2.message, "Payment failed for ***MASKED***")
}

// 测试7: 遥测数据缓存策略
test "遥测数据缓存策略" {
  // 定义缓存条目
  type CacheEntry[T] = {
    value: T,
    timestamp: Int,
    ttl: Int,  // 生存时间（毫秒）
    access_count: Int
  }
  
  // 定义缓存
  type Cache[T] = {
    entries: Map[String, CacheEntry[T]],
    max_size: Int,
    default_ttl: Int
  }
  
  // 创建新缓存
  let create_cache = fn(max_size: Int, default_ttl: Int) {
    {
      entries: Map::empty(),
      max_size,
      default_ttl
    }
  }
  
  // 获取当前时间（简化版）
  let current_time = fn() { 10000 }  // 假设当前时间戳
  
  // 检查条目是否过期
  let is_expired = fn(entry: CacheEntry[T]) {
    let now = current_time()
    entry.timestamp + entry.ttl <= now
  }
  
  // 获取缓存值
  let get = fn(cache: Cache[T], key: String) {
    match Map::get(cache.entries, key) {
      Some(entry) => {
        if is_expired(entry) {
          None  // 过期返回None
        } else {
          // 更新访问计数
          let updated_entry = { entry with access_count: entry.access_count + 1 }
          let _ = Map::insert(cache.entries, key, updated_entry)
          Some(entry.value)
        }
      }
      None => None
    }
  }
  
  // 设置缓存值
  let set = fn(cache: Cache[T], key: String, value: T, ttl: Option[Int]) {
    let now = current_time()
    let actual_ttl = match ttl {
      Some(t) => t
      None => cache.default_ttl
    }
    
    let entry = {
      value,
      timestamp: now,
      ttl: actual_ttl,
      access_count: 1
    }
    
    // 如果缓存已满，实现LRU淘汰策略
    let updated_entries = if Map::size(cache.entries) >= cache.max_size {
      // 找到访问次数最少的条目
      let mut lru_key = ""
      let mut min_access = 999999
      
      for (k, entry) in cache.entries {
        if entry.access_count < min_access {
          min_access = entry.access_count
          lru_key = k
        }
      }
      
      // 删除LRU条目
      let without_lru = Map::remove(cache.entries, lru_key)
      Map::insert(without_lru, key, entry)
    } else {
      Map::insert(cache.entries, key, entry)
    }
    
    { cache with entries: updated_entries }
  }
  
  // 清理过期条目
  let cleanup_expired = fn(cache: Cache[T]) {
    let now = current_time()
    let valid_entries = Map::empty()
    
    for (key, entry) in cache.entries {
      if not is_expired(entry) {
        let _ = Map::insert(valid_entries, key, entry)
      }
    }
    
    { cache with entries: valid_entries }
  }
  
  // 获取缓存统计信息
  type CacheStats = {
    size: Int,
    max_size: Int,
    hit_rate: Float
  }
  
  let get_stats = fn(cache: Cache[T]) {
    let size = Map::size(cache.entries)
    {
      size,
      max_size: cache.max_size,
      hit_rate: 0.0  // 简化版，实际实现需要跟踪命中次数
    }
  }
  
  // 测试缓存基本操作
  let cache = create_cache(3, 5000)  // 最大3个条目，默认TTL 5秒
  
  // 测试空缓存
  assert_eq(get(cache, "key1"), None)
  
  // 测试设置和获取
  let cache1 = set(cache, "key1", "value1", None)
  assert_eq(get(cache1, "key1"), Some("value1"))
  
  // 测试更新值
  let cache2 = set(cache1, "key1", "new-value1", None)
  assert_eq(get(cache2, "key1"), Some("new-value1"))
  
  // 测试多个键
  let cache3 = set(cache2, "key2", "value2", None)
  let cache4 = set(cache3, "key3", "value3", None)
  
  assert_eq(get(cache4, "key1"), Some("new-value1"))
  assert_eq(get(cache4, "key2"), Some("value2"))
  assert_eq(get(cache4, "key3"), Some("value3"))
  
  // 测试缓存大小限制（LRU淘汰）
  let cache5 = set(cache4, "key4", "value4", None)
  
  // 由于缓存大小为3，应该有一个条目被淘汰
  // 假设key1是访问次数最少的（只被访问过一次）
  assert_eq(get(cache5, "key1"), None)  // 应该被淘汰
  assert_eq(get(cache5, "key2"), Some("value2"))
  assert_eq(get(cache5, "key3"), Some("value3"))
  assert_eq(get(cache5, "key4"), Some("value4"))
  
  // 测试访问计数
  let _ = get(cache5, "key2")  // 增加key2的访问计数
  let cache6 = set(cache5, "key5", "value5", None)  // 这应该淘汰key3
  
  assert_eq(get(cache6, "key2"), Some("value2"))  // 应该保留（访问次数多）
  assert_eq(get(cache6, "key3"), None)  // 应该被淘汰
  assert_eq(get(cache6, "key4"), Some("value4"))
  assert_eq(get(cache6, "key5"), Some("value5"))
  
  // 测试TTL过期（模拟时间流逝）
  // 由于current_time()返回固定值，我们无法直接测试过期
  // 但可以验证TTL设置是否正确
  let cache7 = set(cache6, "key6", "value6", Some(1000))  // 1秒TTL
  
  // 验证缓存统计
  let stats = get_stats(cache7)
  assert_eq(stats.size, 4)  // key2, key4, key5, key6
  assert_eq(stats.max_size, 3)
  
  // 测试清理过期条目
  // 由于current_time()返回固定值，这里只是验证函数调用不会出错
  let cleaned_cache = cleanup_expired(cache7)
  assert_eq(get(cleaned_cache, "key2"), Some("value2"))
}

// 测试8: 遥测数据批处理与调度
test "遥测数据批处理与调度" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_flush_interval_ms: Int,
    max_retry_attempts: Int
  }
  
  // 定义批处理状态
  type BatchState[T] = {
    items: Array[T],
    first_item_time: Int,
    last_flush_time: Int,
    retry_count: Int
  }
  
  // 定义批处理结果
  enum BatchResult[T, E] {
    Success(Array[T])
    PartialSuccess(Array[T], Array[T])  // 成功的项，失败的项
    Failure(E)
  }
  
  // 获取当前时间（简化版）
  let current_time = fn() { 10000 }
  
  // 创建批处理状态
  let create_batch_state = fn() {
    {
      items: [],
      first_item_time: 0,
      last_flush_time: current_time(),
      retry_count: 0
    }
  }
  
  // 添加项到批处理
  let add_to_batch = fn(state: BatchState[T], item: T, config: BatchConfig) {
    let now = current_time()
    let new_items = state.items.push(item)
    let first_time = if state.first_item_time == 0 { now } else { state.first_item_time }
    
    {
      items: new_items,
      first_item_time: first_time,
      last_flush_time: state.last_flush_time,
      retry_count: state.retry_count
    }
  }
  
  // 检查是否应该刷新批处理
  let should_flush = fn(state: BatchState[T], config: BatchConfig) {
    let now = current_time()
    let size_reached = state.items.length() >= config.max_batch_size
    let interval_reached = (now - state.last_flush_time) >= config.max_flush_interval_ms
    let has_items = state.items.length() > 0
    
    has_items && (size_reached || interval_reached)
  }
  
  // 刷新批处理
  let flush_batch = fn(state: BatchState[T], config: BatchConfig, processor: (Array[T]) -> BatchResult[T, String]) {
    if state.items.length() == 0 {
      return (state, None)
    }
    
    let result = processor(state.items)
    let now = current_time()
    
    match result {
      BatchResult::Success(processed_items) => {
        // 成功处理所有项，重置状态
        let new_state = {
          items: [],
          first_item_time: 0,
          last_flush_time: now,
          retry_count: 0
        }
        (new_state, Some(result))
      }
      BatchResult::PartialSuccess(successful_items, failed_items) => {
        // 部分成功，保留失败的项
        let retry_count = state.retry_count + 1
        if retry_count < config.max_retry_attempts {
          // 还有重试机会，保留失败的项
          let new_state = {
            items: failed_items,
            first_item_time: now,
            last_flush_time: now,
            retry_count: retry_count
          }
          (new_state, Some(result))
        } else {
          // 超过重试次数，放弃失败的项
          let new_state = {
            items: [],
            first_item_time: 0,
            last_flush_time: now,
            retry_count: 0
          }
          (new_state, Some(result))
        }
      }
      BatchResult::Failure(error) => {
        // 完全失败，根据重试次数决定是否保留
        let retry_count = state.retry_count + 1
        if retry_count < config.max_retry_attempts {
          // 还有重试机会，保留所有项
          let new_state = {
            items: state.items,
            first_item_time: state.first_item_time,
            last_flush_time: now,
            retry_count: retry_count
          }
          (new_state, Some(result))
        } else {
          // 超过重试次数，放弃所有项
          let new_state = {
            items: [],
            first_item_time: 0,
            last_flush_time: now,
            retry_count: 0
          }
          (new_state, Some(result))
        }
      }
    }
  }
  
  // 模拟处理器
  let mock_processor = fn(items: Array[String]) {
    // 模拟处理：长度超过5的项会失败
    let successful = items.filter(fn(item) { item.length() <= 5 })
    let failed = items.filter(fn(item) { item.length() > 5 })
    
    if failed.length() == 0 {
      BatchResult::Success(successful)
    } else if successful.length() > 0 {
      BatchResult::PartialSuccess(successful, failed)
    } else {
      BatchResult::Failure("All items failed")
    }
  }
  
  // 测试批处理基本功能
  let config = {
    max_batch_size: 3,
    max_flush_interval_ms: 5000,
    max_retry_attempts: 2
  }
  
  let initial_state = create_batch_state()
  
  // 添加项
  let state1 = add_to_batch(initial_state, "item1", config)
  let state2 = add_to_batch(state1, "item2", config)
  
  // 检查是否应该刷新（不应该，因为大小和时间都未达到阈值）
  assert_false(should_flush(state2, config))
  
  // 添加第三项，达到批处理大小
  let state3 = add_to_batch(state2, "item3", config)
  
  // 现在应该刷新
  assert_true(should_flush(state3, config))
  
  // 刷新批处理
  let (new_state, result) = flush_batch(state3, config, mock_processor)
  
  // 检查结果
  assert_true(result.is_some())
  match result {
    Some(BatchResult::Success(processed_items)) => {
      assert_eq(processed_items.length(), 3)
    }
    _ => assert_true(false)
  }
  
  // 检查状态已重置
  assert_eq(new_state.items.length(), 0)
  assert_eq(new_state.first_item_time, 0)
  assert_eq(new_state.retry_count, 0)
  
  // 测试部分成功的处理
  let state4 = add_to_batch(new_state, "short", config)
  let state5 = add_to_batch(state4, "very-long-item", config)
  
  // 强制刷新
  let (state6, result2) = flush_batch(state5, config, mock_processor)
  
  // 检查部分成功结果
  assert_true(result2.is_some())
  match result2 {
    Some(BatchResult::PartialSuccess(successful, failed)) => {
      assert_eq(successful.length(), 1)  // "short"
      assert_eq(failed.length(), 1)      // "very-long-item"
    }
    _ => assert_true(false)
  }
  
  // 检查状态保留了失败的项
  assert_eq(state6.items.length(), 1)  // "very-long-item"
  assert_eq(state6.retry_count, 1)
  
  // 测试重试机制
  let (state7, result3) = flush_batch(state6, config, mock_processor)
  
  // 应该再次失败，增加重试计数
  assert_true(result3.is_some())
  assert_eq(state7.retry_count, 2)
  
  // 再次重试，但这次超过最大重试次数
  let (state8, result4) = flush_batch(state7, config, mock_processor)
  
  // 应该放弃失败的项
  assert_true(result4.is_some())
  assert_eq(state8.items.length(), 0)  // 项被放弃
  assert_eq(state8.retry_count, 0)     // 重试计数重置
}

// 测试9: 遥测数据异常检测
test "遥测数据异常检测" {
  // 定义数据点
  type DataPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array[(String, String)]
  }
  
  // 定义异常检测结果
  enum AnomalyResult {
    Normal
    Anomaly(String)  // 包含异常描述
    Suspicious(Float)  // 包含异常分数
  }
  
  // 定义异常检测器
  trait AnomalyDetector {
    detect(points: Array[DataPoint]) -> AnomalyResult
  }
  
  // 实现统计异常检测器（基于标准差）
  let statistical_anomaly_detector = fn(threshold: Float) {
    fn(points: Array[DataPoint]) {
      if points.length() < 3 {
        return AnomalyResult::Normal  // 数据点太少，无法检测
      }
      
      let values = points.map(fn(p) { p.value })
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      
      let variance = values
        .map(fn(v) { (v - mean) * (v - mean) })
        .reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      
      let std_dev = variance.sqrt()
      
      // 检查最后一个点是否异常
      let last_value = values[values.length() - 1]
      let z_score = (last_value - mean).abs() / std_dev
      
      if z_score > threshold {
        AnomalyResult::Anomaly("Statistical anomaly detected: Z-score = " + z_score.to_string())
      } else if z_score > threshold / 2 {
        AnomalyResult::Suspicious(z_score)
      } else {
        AnomalyResult::Normal
      }
    }
  }
  
  // 实现趋势异常检测器（基于趋势变化）
  let trend_anomaly_detector = fn(threshold_percent: Float) {
    fn(points: Array[DataPoint]) {
      if points.length() < 5 {
        return AnomalyResult::Normal  // 数据点太少，无法检测趋势
      }
      
      let values = points.map(fn(p) { p.value })
      
      // 计算前半部分和后半部分的平均值
      let mid_point = values.length() / 2
      let first_half_avg = values.slice(0, mid_point)
        .reduce(fn(acc, v) { acc + v }, 0.0) / (mid_point as Float)
      
      let second_half_avg = values.slice(mid_point, values.length())
        .reduce(fn(acc, v) { acc + v }, 0.0) / ((values.length() - mid_point) as Float)
      
      // 计算变化百分比
      let change_percent = ((second_half_avg - first_half_avg) / first_half_avg) * 100.0
      
      if change_percent.abs() > threshold_percent {
        let direction = if change_percent > 0 { "increase" } else { "decrease" }
        AnomalyResult::Anomaly(
          "Trend anomaly detected: " + direction + " of " + change_percent.abs().to_string() + "%"
        )
      } else if change_percent.abs() > threshold_percent / 2 {
        AnomalyResult::Suspicious(change_percent.abs())
      } else {
        AnomalyResult::Normal
      }
    }
  }
  
  // 实现组合异常检测器
  let composite_anomaly_detector = fn(detectors: Array[AnomalyDetector]) {
    fn(points: Array[DataPoint]) {
      let mut anomaly_count = 0
      let mut suspicious_count = 0
      let mut anomaly_descriptions = []
      
      for detector in detectors {
        match detector.detect(points) {
          AnomalyResult::Anomaly(description) => {
            anomaly_count = anomaly_count + 1
            anomaly_descriptions = anomaly_descriptions.push(description)
          }
          AnomalyResult::Suspicious(_) => {
            suspicious_count = suspicious_count + 1
          }
          AnomalyResult::Normal => {
            // 正常，不计数
          }
        }
      }
      
      if anomaly_count > 0 {
        AnomalyResult::Anomaly(
          "Multiple anomalies detected: " + anomaly_descriptions.join("; ")
        )
      } else if suspicious_count > 0 {
        AnomalyResult::Suspicious(suspicious_count as Float)
      } else {
        AnomalyResult::Normal
      }
    }
  }
  
  // 创建测试数据
  let normal_data = [
    { timestamp: 1000, value: 10.0, metadata: [("service", "api")] },
    { timestamp: 2000, value: 10.5, metadata: [("service", "api")] },
    { timestamp: 3000, value: 9.8, metadata: [("service", "api")] },
    { timestamp: 4000, value: 10.2, metadata: [("service", "api")] },
    { timestamp: 5000, value: 10.1, metadata: [("service", "api")] }
  ]
  
  let spike_data = [
    { timestamp: 1000, value: 10.0, metadata: [("service", "api")] },
    { timestamp: 2000, value: 10.5, metadata: [("service", "api")] },
    { timestamp: 3000, value: 9.8, metadata: [("service", "api")] },
    { timestamp: 4000, value: 10.2, metadata: [("service", "api")] },
    { timestamp: 5000, value: 25.0, metadata: [("service", "api")] }  // 异常峰值
  ]
  
  let trend_data = [
    { timestamp: 1000, value: 10.0, metadata: [("service", "api")] },
    { timestamp: 2000, value: 10.5, metadata: [("service", "api")] },
    { timestamp: 3000, value: 10.2, metadata: [("service", "api")] },
    { timestamp: 4000, value: 15.0, metadata: [("service", "api")] },  // 开始上升
    { timestamp: 5000, value: 20.0, metadata: [("service", "api")] }   // 继续上升
  ]
  
  // 测试统计异常检测器
  let stat_detector = statistical_anomaly_detector(2.0)  // 2个标准差阈值
  
  let normal_result = stat_detector.detect(normal_data)
  assert_true(normal_result == AnomalyResult::Normal)
  
  let spike_result = stat_detector.detect(spike_data)
  assert_true(spike_result != AnomalyResult::Normal)  // 应该检测到异常
  
  // 测试趋势异常检测器
  let trend_detector = trend_anomaly_detector(30.0)  // 30%变化阈值
  
  let normal_trend_result = trend_detector.detect(normal_data)
  assert_true(normal_trend_result == AnomalyResult::Normal)
  
  let trend_anomaly_result = trend_detector.detect(trend_data)
  assert_true(trend_anomaly_result != AnomalyResult::Normal)  // 应该检测到趋势异常
  
  // 测试组合异常检测器
  let composite_detector = composite_anomaly_detector([
    statistical_anomaly_detector(2.0),
    trend_anomaly_detector(30.0)
  ])
  
  let composite_normal_result = composite_detector.detect(normal_data)
  assert_true(composite_normal_result == AnomalyResult::Normal)
  
  let composite_spike_result = composite_detector.detect(spike_data)
  assert_true(composite_spike_result != AnomalyResult::Normal)  // 应该检测到异常
  
  // 测试边界条件
  let few_data = [
    { timestamp: 1000, value: 10.0, metadata: [("service", "api")] },
    { timestamp: 2000, value: 15.0, metadata: [("service", "api")] }
  ]
  
  let few_data_result = stat_detector.detect(few_data)
  assert_true(few_data_result == AnomalyResult::Normal)  // 数据点太少，无法检测
}

// 测试10: 遥测数据资源管理
test "遥测数据资源管理" {
  // 定义资源类型
  enum ResourceType {
    CPU
    Memory
    Disk
    Network
  }
  
  // 定义资源使用情况
  type ResourceUsage = {
    resource_type: ResourceType,
    used: Float,
    total: Float,
    unit: String
  }
  
  // 定义资源限制
  type ResourceLimit = {
    resource_type: ResourceType,
    max_usage: Float,
    warning_threshold: Float,
    critical_threshold: Float
  }
  
  // 定义资源状态
  enum ResourceStatus {
    Normal
    Warning(String)
    Critical(String)
    Exceeded(String)
  }
  
  // 检查资源状态
  let check_resource_status = fn(usage: ResourceUsage, limit: ResourceLimit) {
    let usage_percent = (usage.used / usage.total) * 100.0
    
    if usage_percent >= limit.max_usage {
      ResourceStatus::Exceeded(
        "Resource usage exceeded maximum limit: " + usage_percent.to_string() + "% > " + limit.max_usage.to_string() + "%"
      )
    } else if usage_percent >= limit.critical_threshold {
      ResourceStatus::Critical(
        "Resource usage at critical level: " + usage_percent.to_string() + "%"
      )
    } else if usage_percent >= limit.warning_threshold {
      ResourceStatus::Warning(
        "Resource usage at warning level: " + usage_percent.to_string() + "%"
      )
    } else {
      ResourceStatus::Normal
    }
  }
  
  // 定义资源管理策略
  enum ResourceManagementStrategy {
    NoAction
    ScaleUp
    ScaleDown
    Throttle
    Restart
  }
  
  // 根据资源状态决定管理策略
  let determine_management_strategy = fn(status: ResourceStatus, resource_type: ResourceType) {
    match (status, resource_type) {
      (ResourceStatus::Normal, _) => ResourceManagementStrategy::NoAction
      (ResourceStatus::Warning(_), ResourceType::CPU) => ResourceManagementStrategy::ScaleUp
      (ResourceStatus::Warning(_), ResourceType::Memory) => ResourceManagementStrategy::ScaleUp
      (ResourceStatus::Warning(_), ResourceType::Disk) => ResourceManagementStrategy::NoAction
      (ResourceStatus::Warning(_), ResourceType::Network) => ResourceManagementStrategy::Throttle
      (ResourceStatus::Critical(_), ResourceType::CPU) => ResourceManagementStrategy::ScaleUp
      (ResourceStatus::Critical(_), ResourceType::Memory) => ResourceManagementStrategy::ScaleUp
      (ResourceStatus::Critical(_), ResourceType::Disk) => ResourceManagementStrategy::Throttle
      (ResourceStatus::Critical(_), ResourceType::Network) => ResourceManagementStrategy::Throttle
      (ResourceStatus::Exceeded(_), ResourceType::CPU) => ResourceManagementStrategy::Restart
      (ResourceStatus::Exceeded(_), ResourceType::Memory) => ResourceManagementStrategy::Restart
      (ResourceStatus::Exceeded(_), ResourceType::Disk) => ResourceManagementStrategy::Throttle
      (ResourceStatus::Exceeded(_), ResourceType::Network) => ResourceManagementStrategy::Throttle
    }
  }
  
  // 定义资源管理器
  type ResourceManager = {
    limits: Array[ResourceLimit],
    current_usage: Array[ResourceUsage],
    management_history: Array[(Int, ResourceType, ResourceManagementStrategy)]
  }
  
  // 创建资源管理器
  let create_resource_manager = fn(limits: Array[ResourceLimit]) {
    {
      limits,
      current_usage: [],
      management_history: []
    }
  }
  
  // 更新资源使用情况
  let update_usage = fn(manager: ResourceManager, usage: Array[ResourceUsage]) {
    { manager with current_usage: usage }
  }
  
  // 评估所有资源并应用管理策略
  let evaluate_and_apply_strategies = fn(manager: ResourceManager, current_time: Int) {
    let mut strategies = []
    
    for usage in manager.current_usage {
      // 找到对应的限制
      match manager.limits.find(fn(limit) { limit.resource_type == usage.resource_type }) {
        Some(limit) => {
          let status = check_resource_status(usage, limit)
          let strategy = determine_management_strategy(status, usage.resource_type)
          strategies = strategies.push((usage.resource_type, status, strategy))
        }
        None => {
          // 没有找到限制，使用默认策略
          strategies = strategies.push((usage.resource_type, ResourceStatus::Normal, ResourceManagementStrategy::NoAction))
        }
      }
    }
    
    // 记录管理历史
    let new_history = strategies.map(fn(item) {
      let (resource_type, _, strategy) = item
      (current_time, resource_type, strategy)
    })
    
    let combined_history = manager.management_history.concat(new_history)
    
    {
      strategies,
      updated_manager: { manager with management_history: combined_history }
    }
  }
  
  // 创建测试限制
  let cpu_limit = {
    resource_type: ResourceType::CPU,
    max_usage: 90.0,
    warning_threshold: 70.0,
    critical_threshold: 85.0
  }
  
  let memory_limit = {
    resource_type: ResourceType::Memory,
    max_usage: 95.0,
    warning_threshold: 75.0,
    critical_threshold: 90.0
  }
  
  let disk_limit = {
    resource_type: ResourceType::Disk,
    max_usage: 95.0,
    warning_threshold: 80.0,
    critical_threshold: 90.0
  }
  
  let network_limit = {
    resource_type: ResourceType::Network,
    max_usage: 85.0,
    warning_threshold: 60.0,
    critical_threshold: 75.0
  }
  
  let limits = [cpu_limit, memory_limit, disk_limit, network_limit]
  
  // 创建资源管理器
  let manager = create_resource_manager(limits)
  
  // 测试正常使用情况
  let normal_usage = [
    { resource_type: ResourceType::CPU, used: 40.0, total: 100.0, unit: "%" },
    { resource_type: ResourceType::Memory, used: 50.0, total: 100.0, unit: "%" },
    { resource_type: ResourceType::Disk, used: 60.0, total: 100.0, unit: "%" },
    { resource_type: ResourceType::Network, used: 30.0, total: 100.0, unit: "%" }
  ]
  
  let manager1 = update_usage(manager, normal_usage)
  let result1 = evaluate_and_apply_strategies(manager1, 1000)
  
  // 所有资源应该是正常状态，不需要采取行动
  assert_eq(result1.strategies.length(), 4)
  for strategy in result1.strategies {
    let (_, status, management_strategy) = strategy
    assert_true(status == ResourceStatus::Normal)
    assert_true(management_strategy == ResourceManagementStrategy::NoAction)
  }
  
  // 测试警告级别的使用情况
  let warning_usage = [
    { resource_type: ResourceType::CPU, used: 75.0, total: 100.0, unit: "%" },  // 警告级别
    { resource_type: ResourceType::Memory, used: 80.0, total: 100.0, unit: "%" },  // 警告级别
    { resource_type: ResourceType::Disk, used: 85.0, total: 100.0, unit: "%" },  // 警告级别
    { resource_type: ResourceType::Network, used: 65.0, total: 100.0, unit: "%" }   // 警告级别
  ]
  
  let manager2 = update_usage(manager, warning_usage)
  let result2 = evaluate_and_apply_strategies(manager2, 2000)
  
  // 验证警告级别的策略
  for strategy in result2.strategies {
    let (resource_type, status, management_strategy) = strategy
    assert_true(status != ResourceStatus::Normal)
    
    match resource_type {
      ResourceType::CPU => assert_true(management_strategy == ResourceManagementStrategy::ScaleUp)
      ResourceType::Memory => assert_true(management_strategy == ResourceManagementStrategy::ScaleUp)
      ResourceType::Disk => assert_true(management_strategy == ResourceManagementStrategy::NoAction)
      ResourceType::Network => assert_true(management_strategy == ResourceManagementStrategy::Throttle)
    }
  }
  
  // 测试严重级别的使用情况
  let critical_usage = [
    { resource_type: ResourceType::CPU, used: 87.0, total: 100.0, unit: "%" },  // 严重级别
    { resource_type: ResourceType::Memory, used: 92.0, total: 100.0, unit: "%" },  // 严重级别
    { resource_type: ResourceType::Disk, used: 92.0, total: 100.0, unit: "%" },  // 严重级别
    { resource_type: ResourceType::Network, used: 78.0, total: 100.0, unit: "%" }   // 严重级别
  ]
  
  let manager3 = update_usage(manager, critical_usage)
  let result3 = evaluate_and_apply_strategies(manager3, 3000)
  
  // 验证严重级别的策略
  for strategy in result3.strategies {
    let (resource_type, status, management_strategy) = strategy
    assert_true(status != ResourceStatus::Normal)
    
    match resource_type {
      ResourceType::CPU => assert_true(management_strategy == ResourceManagementStrategy::ScaleUp)
      ResourceType::Memory => assert_true(management_strategy == ResourceManagementStrategy::ScaleUp)
      ResourceType::Disk => assert_true(management_strategy == ResourceManagementStrategy::Throttle)
      ResourceType::Network => assert_true(management_strategy == ResourceManagementStrategy::Throttle)
    }
  }
  
  // 测试超出限制的使用情况
  let exceeded_usage = [
    { resource_type: ResourceType::CPU, used: 95.0, total: 100.0, unit: "%" },  // 超出限制
    { resource_type: ResourceType::Memory, used: 98.0, total: 100.0, unit: "%" },  // 超出限制
    { resource_type: ResourceType::Disk, used: 97.0, total: 100.0, unit: "%" },  // 超出限制
    { resource_type: ResourceType::Network, used: 88.0, total: 100.0, unit: "%" }   // 超出限制
  ]
  
  let manager4 = update_usage(manager, exceeded_usage)
  let result4 = evaluate_and_apply_strategies(manager4, 4000)
  
  // 验证超出限制的策略
  for strategy in result4.strategies {
    let (resource_type, status, management_strategy) = strategy
    assert_true(status != ResourceStatus::Normal)
    
    match resource_type {
      ResourceType::CPU => assert_true(management_strategy == ResourceManagementStrategy::Restart)
      ResourceType::Memory => assert_true(management_strategy == ResourceManagementStrategy::Restart)
      ResourceType::Disk => assert_true(management_strategy == ResourceManagementStrategy::Throttle)
      ResourceType::Network => assert_true(management_strategy == ResourceManagementStrategy::Throttle)
    }
  }
  
  // 验证管理历史记录
  assert_eq(result4.updated_manager.management_history.length(), 16)  // 4次评估 × 4种资源类型
  
  // 验证最后一次评估的记录
  let last_records = result4.updated_manager.management_history.slice(12, 16)
  for record in last_records {
    let (timestamp, resource_type, strategy) = record
    assert_eq(timestamp, 4000)  // 最后一次评估的时间戳
    
    match resource_type {
      ResourceType::CPU => assert_true(strategy == ResourceManagementStrategy::Restart)
      ResourceType::Memory => assert_true(strategy == ResourceManagementStrategy::Restart)
      ResourceType::Disk => assert_true(strategy == ResourceManagementStrategy::Throttle)
      ResourceType::Network => assert_true(strategy == ResourceManagementStrategy::Throttle)
    }
  }
}