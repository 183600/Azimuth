// Azimuth 遥测系统专项测试用例
// 专注于遥测系统的核心功能和MoonBit高级特性

// 测试1: 遥测数据生成和收集
test "遥测数据生成和收集" {
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建Span生成器
  let create_span = fn(trace_id: String, operation_name: String, parent_id: Option[String]) {
    let timestamp = Time::now()
    {
      trace_id,
      span_id: "span-" + timestamp.to_string(),
      parent_span_id: parent_id,
      operation_name,
      start_time: timestamp,
      end_time: timestamp + 100,  // 假设操作耗时100ms
      status: "ok",
      attributes: []
    }
  }
  
  // 添加属性
  let add_attribute = fn(span: Span, key: String, value: String) {
    { span | attributes: span.attributes.push((key, value)) }
  }
  
  // 测试Span创建
  let root_span = create_span("trace-12345", "http_request", None)
  assert_eq(root_span.trace_id, "trace-12345")
  assert_eq(root_span.operation_name, "http_request")
  assert_eq(root_span.parent_span_id, None)
  assert_eq(root_span.status, "ok")
  
  // 测试添加属性
  let span_with_attrs = add_attribute(root_span, "http.method", "GET")
  let final_span = add_attribute(span_with_attrs, "http.status_code", "200")
  
  assert_eq(final_span.attributes.length(), 2)
  assert_true(final_span.attributes.contains(("http.method", "GET")))
  assert_true(final_span.attributes.contains(("http.status_code", "200")))
  
  // 测试子Span创建
  let child_span = create_span("trace-12345", "database_query", Some(final_span.span_id))
  assert_eq(child_span.parent_span_id, Some(final_span.span_id))
  assert_eq(child_span.trace_id, final_span.trace_id)
}

// 测试2: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 定义度量类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义度量点
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 序列化函数
  let serialize_metric = fn(metric: Metric) {
    let type_str = match metric.metric_type {
      MetricType::Counter => "counter"
      MetricType::Gauge => "gauge"
      MetricType::Histogram => "histogram"
      MetricType::Summary => "summary"
    }
    
    let labels_str = metric.labels.map(fn(pair) {
      let (key, value) = pair
      key + "=" + value
    }).join(",")
    
    metric.name + "|" + type_str + "|" + metric.value.to_string() + "|" + 
    metric.timestamp.to_string() + "|" + labels_str
  }
  
  // 反序列化函数
  let deserialize_metric = fn serialized: String {
    let parts = serialized.split("|")
    if parts.length() != 5 {
      None
    } else {
      let metric_type = match parts[1] {
        "counter" => MetricType::Counter
        "gauge" => MetricType::Gauge
        "histogram" => MetricType::Histogram
        "summary" => MetricType::Summary
        _ => MetricType::Counter  // 默认值
      }
      
      let value = match parts[2].to_float() {
        Some(v) => v
        None => 0.0
      }
      
      let timestamp = match parts[3].to_int() {
        Some(t) => t
        None => 0
      }
      
      let labels_str = parts[4]
      let labels = if labels_str == "" {
        []
      } else {
        labels_str.split(",").map(fn(pair_str) {
          let pair_parts = pair_str.split("=")
          if pair_parts.length() == 2 {
            (pair_parts[0], pair_parts[1])
          } else {
            (pair_str, "")
          }
        })
      }
      
      Some({
        name: parts[0],
        metric_type,
        value,
        timestamp,
        labels
      })
    }
  }
  
  // 测试序列化
  let metric = {
    name: "http_requests_total",
    metric_type: MetricType::Counter,
    value: 1000.0,
    timestamp: 1640995200,
    labels: [("method", "GET"), ("status", "200")]
  }
  
  let serialized = serialize_metric(metric)
  assert_true(serialized.contains("http_requests_total"))
  assert_true(serialized.contains("counter"))
  assert_true(serialized.contains("1000"))
  assert_true(serialized.contains("method=GET"))
  assert_true(serialized.contains("status=200"))
  
  // 测试反序列化
  let deserialized = deserialize_metric(serialized)
  match deserialized {
    Some(m) => {
      assert_eq(m.name, "http_requests_total")
      assert_eq(m.metric_type, MetricType::Counter)
      assert_eq(m.value, 1000.0)
      assert_eq(m.timestamp, 1640995200)
      assert_true(m.labels.contains(("method", "GET")))
      assert_true(m.labels.contains(("status", "200")))
    }
    None => assert_true(false)
  }
}

// 测试3: 遥测数据过滤和转换
test "遥测数据过滤和转换" {
  // 定义事件类型
  enum EventSeverity {
    Debug
    Info
    Warning
    Error
    Critical
  }
  
  // 定义事件
  type Event = {
    id: String,
    timestamp: Int,
    severity: EventSeverity,
    message: String,
    source: String,
    attributes: Array[(String, String)]
  }
  
  // 创建事件
  let create_event = fn(id: String, severity: EventSeverity, message: String, source: String) {
    {
      id,
      timestamp: Time::now(),
      severity,
      message,
      source,
      attributes: []
    }
  }
  
  // 严重程度过滤器
  let filter_by_severity = fn(events: Array[Event], min_severity: EventSeverity) {
    events.filter(fn(event) {
      match (event.severity, min_severity) {
        (EventSeverity::Debug, EventSeverity::Debug) => true
        (EventSeverity::Info, EventSeverity::Debug) => true
        (EventSeverity::Info, EventSeverity::Info) => true
        (EventSeverity::Warning, EventSeverity::Debug) => true
        (EventSeverity::Warning, EventSeverity::Info) => true
        (EventSeverity::Warning, EventSeverity::Warning) => true
        (EventSeverity::Error, EventSeverity::Debug) => true
        (EventSeverity::Error, EventSeverity::Info) => true
        (EventSeverity::Error, EventSeverity::Warning) => true
        (EventSeverity::Error, EventSeverity::Error) => true
        (EventSeverity::Critical, _) => true
        (_, _) => false
      }
    })
  }
  
  // 源过滤器
  let filter_by_source = fn(events: Array[Event], allowed_sources: Array[String]) {
    events.filter(fn(event) { allowed_sources.contains(event.source) })
  }
  
  // 时间范围过滤器
  let filter_by_time_range = fn(events: Array[Event], start_time: Int, end_time: Int) {
    events.filter(fn(event) { event.timestamp >= start_time && event.timestamp <= end_time })
  }
  
  // 事件转换器
  let transform_events = fn(events: Array[Event], transformer: (Event) -> Event) {
    events.map(transformer)
  }
  
  // 创建测试事件
  let events = [
    create_event("event-1", EventSeverity::Debug, "调试信息", "service-a"),
    create_event("event-2", EventSeverity::Info, "正常操作", "service-a"),
    create_event("event-3", EventSeverity::Warning, "警告信息", "service-b"),
    create_event("event-4", EventSeverity::Error, "错误发生", "service-b"),
    create_event("event-5", EventSeverity::Critical, "严重错误", "service-c")
  ]
  
  // 测试严重程度过滤
  let warning_and_above = filter_by_severity(events, EventSeverity::Warning)
  assert_eq(warning_and_above.length(), 3)  // Warning, Error, Critical
  
  let error_and_above = filter_by_severity(events, EventSeverity::Error)
  assert_eq(error_and_above.length(), 2)  // Error, Critical
  
  // 测试源过滤
  let service_a_events = filter_by_source(events, ["service-a"])
  assert_eq(service_a_events.length(), 2)
  assert_true(service_a_events.all(fn(e) { e.source == "service-a" }))
  
  // 测试时间范围过滤
  let now = Time::now()
  let time_filtered = filter_by_time_range(events, now - 1000, now + 1000)
  assert_eq(time_filtered.length(), events.length())  // 所有事件都在当前时间附近
  
  // 测试事件转换
  let add_timestamp_prefix = fn(event: Event) {
    { event | message: "[" + event.timestamp.to_string() + "] " + event.message }
  }
  
  let transformed = transform_events(events, add_timestamp_prefix)
  assert_true(transformed.all(fn(e) { e.message.starts_with("[") }))
  assert_true(transformed.all(fn(e) { e.message.contains("]") }))
  
  // 测试组合过滤
  let service_b_errors = events
    |> filter_by_source(["service-b"])
    |> filter_by_severity(EventSeverity::Error)
  
  assert_eq(service_b_errors.length(), 1)
  assert_eq(service_b_errors[0].severity, EventSeverity::Error)
  assert_eq(service_b_errors[0].source, "service-b")
}

// 测试4: 遥测数据聚合和统计
test "遥测数据聚合和统计" {
  // 定义性能指标
  type PerformanceMetric = {
    operation: String,
    duration_ms: Int,
    success: Bool,
    timestamp: Int,
    service: String
  }
  
  // 计算统计信息
  let calculate_stats = fn(metrics: Array[PerformanceMetric]) {
    if metrics.length() == 0 {
      {
        count: 0,
        avg_duration: 0.0,
        min_duration: 0,
        max_duration: 0,
        success_rate: 0.0,
        operations: Map::empty()
      }
    } else {
      let durations = metrics.map(fn(m) { m.duration_ms })
      let successes = metrics.filter(fn(m) { m.success }).length()
      
      let count = metrics.length()
      let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
      let avg_duration = (total_duration as Float) / (count as Float)
      let min_duration = durations.reduce(fn(acc, d) { if d < acc { d } else { acc } }, durations[0])
      let max_duration = durations.reduce(fn(acc, d) { if d > acc { d } else { acc } }, durations[0])
      let success_rate = (successes as Float) / (count as Float) * 100.0
      
      // 按操作类型分组统计
      let operations = Map::empty()
      for metric in metrics {
        let op_stats = match Map::get(operations, metric.operation) {
          Some(stats) => stats
          None => { count: 0, total_duration: 0, successes: 0 }
        }
        
        let updated_stats = {
          count: op_stats.count + 1,
          total_duration: op_stats.total_duration + metric.duration_ms,
          successes: op_stats.successes + (if metric.success { 1 } else { 0 })
        }
        
        let _ = Map::insert(operations, metric.operation, updated_stats)
      }
      
      {
        count,
        avg_duration,
        min_duration,
        max_duration,
        success_rate,
        operations
      }
    }
  }
  
  // 计算百分位数
  let calculate_percentiles = fn(metrics: Array[PerformanceMetric], percentiles: Array<Float>) {
    let durations = metrics.map(fn(m) { m.duration_ms }).sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let count = durations.length()
    
    percentiles.map(fn(p) {
      let index = ((p / 100.0) * (count as Float)) as Int
      if index >= count {
        durations[count - 1]
      } else {
        durations[index]
      }
    })
  }
  
  // 创建测试数据
  let metrics = [
    { operation: "http_get", duration_ms: 120, success: true, timestamp: 1000, service: "api" },
    { operation: "http_post", duration_ms: 250, success: true, timestamp: 1100, service: "api" },
    { operation: "http_get", duration_ms: 95, success: true, timestamp: 1200, service: "api" },
    { operation: "database_query", duration_ms: 450, success: true, timestamp: 1300, service: "db" },
    { operation: "http_get", duration_ms: 180, success: false, timestamp: 1400, service: "api" },
    { operation: "database_query", duration_ms: 320, success: true, timestamp: 1500, service: "db" },
    { operation: "cache_get", duration_ms: 15, success: true, timestamp: 1600, service: "cache" },
    { operation: "http_post", duration_ms: 310, success: false, timestamp: 1700, service: "api" }
  ]
  
  // 测试统计计算
  let stats = calculate_stats(metrics)
  assert_eq(stats.count, 8)
  assert_eq(stats.success_rate.round(), 75.0)  // 6/8 = 75%
  assert_eq(stats.min_duration, 15)
  assert_eq(stats.max_duration, 450)
  
  // 测试操作分组统计
  let http_get_stats = match Map::get(stats.operations, "http_get") {
    Some(s) => s
    None => { count: 0, total_duration: 0, successes: 0 }
  }
  assert_eq(http_get_stats.count, 3)
  assert_eq(http_get_stats.successes, 2)
  
  let database_query_stats = match Map::get(stats.operations, "database_query") {
    Some(s) => s
    None => { count: 0, total_duration: 0, successes: 0 }
  }
  assert_eq(database_query_stats.count, 2)
  assert_eq(database_query_stats.successes, 2)
  
  // 测试百分位数计算
  let percentiles = calculate_percentiles(metrics, [50.0, 90.0, 95.0, 99.0])
  assert_eq(percentiles.length(), 4)
  assert_true(percentiles[0] <= percentiles[1])  // 50th <= 90th
  assert_true(percentiles[1] <= percentiles[2])  // 90th <= 95th
  assert_true(percentiles[2] <= percentiles[3])  // 95th <= 99th
}

// 测试5: 遥测数据关联和追踪
test "遥测数据关联和追踪" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)]
  }
  
  // 定义分布式追踪信息
  type DistributedTrace = {
    context: TraceContext,
    spans: Array[String],
    services: Array[String],
    start_time: Int,
    end_time: Int
  }
  
  // 创建追踪上下文
  let create_trace_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      baggage: []
    }
  }
  
  // 添加baggage项
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    { context | baggage: context.baggage.push((key, value)) }
  }
  
  // 提取baggage
  let get_baggage = fn(context: TraceContext, key: String) {
    context.baggage.find(fn(item) {
      let (k, _) = item
      k == key
    }).map(fn(item) {
      let (_, v) = item
      v
    })
  }
  
  // 创建分布式追踪
  let create_distributed_trace = fn(trace_id: String, initial_span: String) {
    {
      context: create_trace_context(trace_id, initial_span),
      spans: [initial_span],
      services: [],
      start_time: Time::now(),
      end_time: Time::now()
    }
  }
  
  // 添加span到追踪
  let add_span_to_trace = fn(trace: DistributedTrace, span_id: String, service: String) {
    {
      context: trace.context,
      spans: trace.spans.push(span_id),
      services: if not(trace.services.contains(service)) {
        trace.services.push(service)
      } else {
        trace.services
      },
      start_time: trace.start_time,
      end_time: Time::now()
    }
  }
  
  // 测试追踪上下文
  let context = create_trace_context("trace-123", "span-456")
  assert_eq(context.trace_id, "trace-123")
  assert_eq(context.span_id, "span-456")
  assert_eq(context.baggage.length(), 0)
  
  // 测试baggage操作
  let context_with_baggage = add_baggage(context, "user.id", "user-789")
  let final_context = add_baggage(context_with_baggage, "request.id", "req-101")
  
  assert_eq(final_context.baggage.length(), 2)
  assert_eq(get_baggage(final_context, "user.id"), Some("user-789"))
  assert_eq(get_baggage(final_context, "request.id"), Some("req-101"))
  assert_eq(get_baggage(final_context, "nonexistent"), None)
  
  // 测试分布式追踪
  let trace = create_distributed_trace("trace-123", "root-span")
  assert_eq(trace.context.trace_id, "trace-123")
  assert_eq(trace.spans.length(), 1)
  assert_eq(trace.services.length(), 0)
  
  // 添加更多span
  let trace_with_spans = trace
    |> add_span_to_trace("db-span", "database-service")
    |> add_span_to_trace("cache-span", "cache-service")
    |> add_span_to_trace("api-span", "api-service")
    |> add_span_to_trace("db-span-2", "database-service")  // 重复服务
  
  assert_eq(trace_with_spans.spans.length(), 5)
  assert_eq(trace_with_spans.services.length(), 3)  // database-service, cache-service, api-service
  assert_true(trace_with_spans.services.contains("database-service"))
  assert_true(trace_with_spans.services.contains("cache-service"))
  assert_true(trace_with_spans.services.contains("api-service"))
  
  // 测试追踪持续时间
  let duration = trace_with_spans.end_time - trace_with_spans.start_time
  assert_true(duration >= 0)
}

// 测试6: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样决策
  type SamplingDecision = {
    sampled: Bool,
    reason: String
  }
  
  // 定义采样器接口
  trait Sampler {
    should_sample(trace_id: String, name: String, attributes: Array[(String, String)>) -> SamplingDecision
  }
  
  // 实现基于概率的采样器
  let probability_sampler = fn(sampling_rate: Float) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      // 简化的采样算法：基于trace_id的哈希
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let probability = (hash % 100) as Float / 100.0
      
      if probability <= sampling_rate {
        { sampled: true, reason: "概率采样: " + sampling_rate.to_string() }
      } else {
        { sampled: false, reason: "概率采样: " + sampling_rate.to_string() }
      }
    }
  }
  
  // 实现基于属性的采样器
  let attribute_sampler = fn(required_attributes: Array[(String, String)>) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      let all_required_present = required_attributes.all(fn(required) {
        let (req_key, req_value) = required
        attributes.any(fn(attr) {
          let (key, value) = attr
          key == req_key && value == req_value
        })
      })
      
      if all_required_present {
        { sampled: true, reason: "属性匹配" }
      } else {
        { sampled: false, reason: "属性不匹配" }
      }
    }
  }
  
  // 实现组合采样器
  let composite_sampler = fn(samplers: Array[(String) -> SamplingDecision>) {
    fn(trace_id: String, name: String, attributes: Array[(String, String)>) {
      let decisions = samplers.map(fn(sampler) { sampler(trace_id, name, attributes) })
      let sampled_count = decisions.filter(fn(d) { d.sampled }).length()
      
      // 如果至少有一个采样器决定采样，则采样
      if sampled_count > 0 {
        let reasons = decisions.filter(fn(d) { d.sampled }).map(fn(d) { d.reason }).join(", ")
        { sampled: true, reason: reasons }
      } else {
        { sampled: false, reason: "所有采样器拒绝" }
      }
    }
  }
  
  // 测试概率采样器
  let prob_sampler = probability_sampler(0.5)  // 50%采样率
  let prob_decision1 = prob_sampler("trace-123", "operation", [])
  let prob_decision2 = prob_sampler("trace-456", "operation", [])
  
  // 注意：实际结果取决于哈希计算，这里只是验证结构
  assert_true(prob_decision1.reason.contains("概率采样"))
  assert_true(prob_decision2.reason.contains("概率采样"))
  
  // 测试属性采样器
  let attr_sampler = attribute_sampler([
    ("environment", "production"),
    ("service.name", "api-service")
  ])
  
  let prod_attrs = [
    ("environment", "production"),
    ("service.name", "api-service"),
    ("version", "1.0.0")
  ]
  
  let dev_attrs = [
    ("environment", "development"),
    ("service.name", "api-service"),
    ("version", "1.0.0")
  ]
  
  let prod_decision = attr_sampler("trace-789", "operation", prod_attrs)
  let dev_decision = attr_sampler("trace-101", "operation", dev_attrs)
  
  assert_true(prod_decision.sampled)
  assert_eq(prod_decision.reason, "属性匹配")
  
  assert_false(dev_decision.sampled)
  assert_eq(dev_decision.reason, "属性不匹配")
  
  // 测试组合采样器
  let combined_sampler = composite_sampler([
    prob_sampler,
    attr_sampler
  ])
  
  let combined_decision1 = combined_sampler("trace-111", "operation", prod_attrs)
  let combined_decision2 = combined_sampler("trace-222", "operation", dev_attrs)
  
  // 生产环境应该被采样（属性采样器通过）
  assert_true(combined_decision1.sampled)
  assert_true(combined_decision1.reason.contains("属性匹配"))
  
  // 开发环境可能被采样（取决于概率采样器）
  // assert_true(combined_decision2.reason.contains("概率采样") || combined_decision2.reason.contains("所有采样器拒绝"))
}

// 测试7: 遥测数据导出和传输
test "遥测数据导出和传输" {
  // 定义导出器配置
  type ExporterConfig = {
    endpoint: String,
    batch_size: Int,
    timeout_ms: Int,
    retry_count: Int,
    compression_enabled: Bool
  }
  
  // 定义导出结果
  type ExportResult = {
    success: Bool,
    exported_count: Int,
    error_message: Option[String]
  }
  
  // 定义遥测数据批处理
  type TelemetryBatch = {
    spans: Array[String],
    metrics: Array[String],
    logs: Array[String],
    timestamp: Int
  }
  
  // 创建批处理
  let create_batch = fn() {
    {
      spans: [],
      metrics: [],
      logs: [],
      timestamp: Time::now()
    }
  }
  
  // 添加span到批处理
  let add_span_to_batch = fn(batch: TelemetryBatch, span: String) {
    { batch | spans: batch.spans.push(span) }
  }
  
  // 添加metric到批处理
  let add_metric_to_batch = fn(batch: TelemetryBatch, metric: String) {
    { batch | metrics: batch.metrics.push(metric) }
  }
  
  // 添加log到批处理
  let add_log_to_batch = fn(batch: TelemetryBatch, log: String) {
    { batch | logs: batch.logs.push(log) }
  }
  
  // 模拟导出函数
  let export_batch = fn(batch: TelemetryBatch, config: ExporterConfig) {
    let total_items = batch.spans.length() + batch.metrics.length() + batch.logs.length()
    
    // 模拟导出逻辑
    if total_items == 0 {
      {
        success: true,
        exported_count: 0,
        error_message: None
      }
    } else if total_items > config.batch_size {
      {
        success: false,
        exported_count: 0,
        error_message: Some("批处理大小超过限制")
      }
    } else {
      // 模拟网络延迟
      let _ = Time::sleep(10)  // 10ms延迟
      
      {
        success: true,
        exported_count: total_items,
        error_message: None
      }
    }
  }
  
  // 批量导出函数
  let batch_export = fn(batches: Array[TelemetryBatch], config: ExporterConfig) {
    let results = []
    
    for batch in batches {
      let result = export_batch(batch, config)
      results = results.push(result)
    }
    
    let total_exported = results.filter(fn(r) { r.success }).reduce(fn(acc, r) { acc + r.exported_count }, 0)
    let failed_count = results.filter(fn(r) { not(r.success) }).length()
    
    {
      total_batches: batches.length(),
      successful_batches: results.filter(fn(r) { r.success }).length(),
      failed_batches: failed_count,
      total_exported: total_exported
    }
  }
  
  // 测试批处理创建和添加
  let batch = create_batch()
  assert_eq(batch.spans.length(), 0)
  assert_eq(batch.metrics.length(), 0)
  assert_eq(batch.logs.length(), 0)
  
  let batch_with_data = batch
    |> add_span_to_batch("span-1")
    |> add_span_to_batch("span-2")
    |> add_metric_to_batch("metric-1")
    |> add_log_to_batch("log-1")
  
  assert_eq(batch_with_data.spans.length(), 2)
  assert_eq(batch_with_data.metrics.length(), 1)
  assert_eq(batch_with_data.logs.length(), 1)
  
  // 测试导出
  let config = {
    endpoint: "https://telemetry.example.com/api/v1/traces",
    batch_size: 10,
    timeout_ms: 5000,
    retry_count: 3,
    compression_enabled: true
  }
  
  let export_result = export_batch(batch_with_data, config)
  assert_true(export_result.success)
  assert_eq(export_result.exported_count, 4)
  assert_eq(export_result.error_message, None)
  
  // 测试批处理大小限制
  let oversized_batch = batch_with_data
    |> add_span_to_batch("span-3")
    |> add_span_to_batch("span-4")
    |> add_span_to_batch("span-5")
    |> add_span_to_batch("span-6")
    |> add_span_to_batch("span-7")
    |> add_span_to_batch("span-8")
    |> add_span_to_batch("span-9")
    |> add_span_to_batch("span-10")
  
  let oversized_result = export_batch(oversized_batch, { config | batch_size: 5 })
  assert_false(oversized_result.success)
  assert_eq(oversized_result.exported_count, 0)
  assert_eq(oversized_result.error_message, Some("批处理大小超过限制"))
  
  // 测试批量导出
  let batches = [batch_with_data, oversized_batch, create_batch()]
  let batch_result = batch_export(batches, config)
  
  assert_eq(batch_result.total_batches, 3)
  assert_eq(batch_result.successful_batches, 2)  // batch_with_data 和空批处理成功
  assert_eq(batch_result.failed_batches, 1)     // oversized_batch 失败
  assert_eq(batch_result.total_exported, 4)     // 只有batch_with_data的数据被导出
}

// 测试8: 遥测数据存储和检索
test "遥测数据存储和检索" {
  // 定义存储查询
  type StorageQuery = {
    trace_id: Option[String],
    service_name: Option[String],
    start_time: Option[Int],
    end_time: Option[Int],
    limit: Int
  }
  
  // 定义存储结果
  type StorageResult = {
    data: Array[String],
    total_count: Int,
    has_more: Bool
  }
  
  // 模拟存储
  let storage = {
    data: [],
    
    // 存储数据
    store: fn(item: String) {
      storage.data = storage.data.push(item)
    },
    
    // 查询数据
    query: fn(query: StorageQuery) {
      let filtered = storage.data.filter(fn(item) {
        let matches_trace_id = match query.trace_id {
          Some(trace_id) => item.contains(trace_id)
          None => true
        }
        
        let matches_service = match query.service_name {
          Some(service) => item.contains(service)
          None => true
        }
        
        let matches_time = match (query.start_time, query.end_time) {
          (Some(start), Some(end)) => true  // 简化：假设所有数据都在时间范围内
          (Some(start), None) => true
          (None, Some(end)) => true
          (None, None) => true
        }
        
        matches_trace_id && matches_service && matches_time
      })
      
      let limited = if filtered.length() > query.limit {
        filtered.slice(0, query.limit)
      } else {
        filtered
      }
      
      {
        data: limited,
        total_count: filtered.length(),
        has_more: filtered.length() > query.limit
      }
    },
    
    // 清空存储
    clear: fn() {
      storage.data = []
    }
  }
  
  // 测试数据存储
  storage.clear()
  storage.store("trace-123|span-456|service-a|operation-1|1000")
  storage.store("trace-123|span-789|service-b|operation-2|1100")
  storage.store("trace-456|span-101|service-a|operation-3|1200")
  storage.store("trace-789|span-112|service-c|operation-4|1300")
  storage.store("trace-123|span-131|service-a|operation-5|1400")
  
  assert_eq(storage.data.length(), 5)
  
  // 测试按trace_id查询
  let trace_query = {
    trace_id: Some("trace-123"),
    service_name: None,
    start_time: None,
    end_time: None,
    limit: 10
  }
  
  let trace_result = storage.query(trace_query)
  assert_eq(trace_result.total_count, 3)
  assert_eq(trace_result.data.length(), 3)
  assert_false(trace_result.has_more)
  assert_true(trace_result.data.all(fn(item) { item.contains("trace-123") }))
  
  // 测试按service_name查询
  let service_query = {
    trace_id: None,
    service_name: Some("service-a"),
    start_time: None,
    end_time: None,
    limit: 10
  }
  
  let service_result = storage.query(service_query)
  assert_eq(service_result.total_count, 3)
  assert_eq(service_result.data.length(), 3)
  assert_true(service_result.data.all(fn(item) { item.contains("service-a") }))
  
  // 测试组合查询
  let combined_query = {
    trace_id: Some("trace-123"),
    service_name: Some("service-a"),
    start_time: None,
    end_time: None,
    limit: 10
  }
  
  let combined_result = storage.query(combined_query)
  assert_eq(combined_result.total_count, 2)
  assert_eq(combined_result.data.length(), 2)
  assert_true(combined_result.data.all(fn(item) { 
    item.contains("trace-123") && item.contains("service-a")
  }))
  
  // 测试限制查询
  let limited_query = {
    trace_id: None,
    service_name: None,
    start_time: None,
    end_time: None,
    limit: 3
  }
  
  let limited_result = storage.query(limited_query)
  assert_eq(limited_result.total_count, 5)
  assert_eq(limited_result.data.length(), 3)
  assert_true(limited_result.has_more)
  
  // 测试空结果
  let empty_query = {
    trace_id: Some("nonexistent"),
    service_name: None,
    start_time: None,
    end_time: None,
    limit: 10
  }
  
  let empty_result = storage.query(empty_query)
  assert_eq(empty_result.total_count, 0)
  assert_eq(empty_result.data.length(), 0)
  assert_false(empty_result.has_more)
}

// 测试9: 遥测配置管理
test "遥测配置管理" {
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: String,
    description: String,
    required: Bool,
    default_value: Option[String]
  }
  
  // 定义配置变更监听器
  type ConfigChangeListener = (String, String, String) -> Unit  // (key, old_value, new_value)
  
  // 配置管理器
  let config_manager = {
    configs: Map::empty(),
    listeners: [],
    
    // 注册配置项
    register_config: fn(item: ConfigItem) {
      let current_value = match Map::get(config_manager.configs, item.key) {
        Some((value, _, _)) => value
        None => match item.default_value {
          Some(default) => default
          None => ""
        }
      }
      
      let _ = Map::insert(config_manager.configs, item.key, (current_value, item.description, item.required))
    },
    
    // 设置配置值
    set_config: fn(key: String, value: String) {
      match Map::get(config_manager.configs, key) {
        Some((old_value, description, required)) => {
          let _ = Map::insert(config_manager.configs, key, (value, description, required))
          
          // 通知监听器
          for listener in config_manager.listeners {
            listener(key, old_value, value)
          }
        }
        None => {
          // 配置项不存在，自动注册
          let _ = Map::insert(config_manager.configs, key, (value, "", false))
        }
      }
    },
    
    // 获取配置值
    get_config: fn(key: String) {
      match Map::get(config_manager.configs, key) {
        Some((value, _, _)) => Some(value)
        None => None
      }
    },
    
    // 添加变更监听器
    add_listener: fn(listener: ConfigChangeListener) {
      config_manager.listeners = config_manager.listeners.push(listener)
    },
    
    // 验证配置
    validate_configs: fn() {
      let errors = []
      
      for (key, (value, description, required)) in config_manager.configs {
        if required && (value == "" || value == "null") {
          errors = errors.push("必需配置项 '" + key + "' 未设置")
        }
      }
      
      errors
    }
  }
  
  // 注册遥测配置项
  config_manager.register_config({
    key: "telemetry.enabled",
    value: "",
    description: "是否启用遥测",
    required: false,
    default_value: Some("true")
  })
  
  config_manager.register_config({
    key: "telemetry.sampling_rate",
    value: "",
    description: "遥测采样率",
    required: false,
    default_value: Some("0.1")
  })
  
  config_manager.register_config({
    key: "telemetry.exporter.endpoint",
    value: "",
    description: "遥测导出端点",
    required: true,
    default_value: None
  })
  
  // 测试默认值
  assert_eq(config_manager.get_config("telemetry.enabled"), Some("true"))
  assert_eq(config_manager.get_config("telemetry.sampling_rate"), Some("0.1"))
  assert_eq(config_manager.get_config("telemetry.exporter.endpoint"), None)
  
  // 测试设置配置
  config_manager.set_config("telemetry.exporter.endpoint", "https://telemetry.example.com")
  assert_eq(config_manager.get_config("telemetry.exporter.endpoint"), Some("https://telemetry.example.com"))
  
  // 测试配置验证
  let validation_errors = config_manager.validate_configs()
  assert_eq(validation_errors.length(), 0)  // 所有必需配置都已设置
  
  // 测试缺失必需配置
  config_manager.set_config("telemetry.exporter.endpoint", "")
  let validation_errors2 = config_manager.validate_configs()
  assert_eq(validation_errors2.length(), 1)
  assert_true(validation_errors2[0].contains("telemetry.exporter.endpoint"))
  
  // 测试配置变更监听
  let change_log = { mut changes: [] }
  
  let listener = fn(key: String, old_value: String, new_value: String) {
    change_log.changes = change_log.changes.push(key + ": " + old_value + " -> " + new_value)
  }
  
  config_manager.add_listener(listener)
  config_manager.set_config("telemetry.sampling_rate", "0.5")
  
  assert_eq(change_log.changes.length(), 1)
  assert_eq(change_log.changes[0], "telemetry.sampling_rate: 0.1 -> 0.5")
  assert_eq(config_manager.get_config("telemetry.sampling_rate"), Some("0.5"))
}

// 测试10: 遥测数据生命周期管理
test "遥测数据生命周期管理" {
  // 定义数据状态
  enum DataStatus {
    Active
    Archived
    PendingDeletion
    Deleted
  }
  
  // 定义数据元数据
  type DataMetadata = {
    id: String,
    status: DataStatus,
    created_at: Int,
    last_accessed: Int,
    access_count: Int,
    size_bytes: Int,
    retention_policy: String
  }
  
  // 定义生命周期策略
  type LifecyclePolicy = {
    max_age_days: Int,
    max_access_count: Int,
    inactive_threshold_days: Int,
    archive_threshold_days: Int
  }
  
  // 数据生命周期管理器
  let lifecycle_manager = {
    metadata: Map::empty(),
    
    // 注册数据
    register_data: fn(id: String, size_bytes: Int, retention_policy: String) {
      let now = Time::now()
      let metadata = {
        id,
        status: DataStatus::Active,
        created_at: now,
        last_accessed: now,
        access_count: 0,
        size_bytes,
        retention_policy
      }
      
      let _ = Map::insert(lifecycle_manager.metadata, id, metadata)
    },
    
    // 访问数据
    access_data: fn(id: String) {
      match Map::get(lifecycle_manager.metadata, id) {
        Some(meta) => {
          let updated = {
            meta |
            last_accessed: Time::now(),
            access_count: meta.access_count + 1
          }
          let _ = Map::insert(lifecycle_manager.metadata, id, updated)
        }
        None => {}
      }
    },
    
    // 应用生命周期策略
    apply_policy: fn(policy: LifecyclePolicy) {
      let now = Time::now()
      let day_ms = 24 * 60 * 60 * 1000
      
      for (id, meta) in lifecycle_manager.metadata {
        let age_days = (now - meta.created_at) / day_ms
        let inactive_days = (now - meta.last_accessed) / day_ms
        
        let new_status = match meta.status {
          DataStatus::Active => {
            if age_days > policy.max_age_days {
              DataStatus::PendingDeletion
            } else if inactive_days > policy.inactive_threshold_days {
              DataStatus::Archived
            } else {
              DataStatus::Active
            }
          }
          DataStatus::Archived => {
            if age_days > policy.archive_threshold_days {
              DataStatus::PendingDeletion
            } else {
              DataStatus::Archived
            }
          }
          DataStatus::PendingDeletion => DataStatus::PendingDeletion
          DataStatus::Deleted => DataStatus::Deleted
        }
        
        if new_status != meta.status {
          let updated = { meta | status: new_status }
          let _ = Map::insert(lifecycle_manager.metadata, id, updated)
        }
      }
    },
    
    // 获取数据状态
    get_status: fn(id: String) {
      match Map::get(lifecycle_manager.metadata, id) {
        Some(meta) => Some(meta.status)
        None => None
      }
    },
    
    // 执行清理
    cleanup: fn() {
      let to_delete = []
      
      for (id, meta) in lifecycle_manager.metadata {
        if meta.status == DataStatus::PendingDeletion {
          to_delete = to_delete.push(id)
        }
      }
      
      for id in to_delete {
        let updated = match Map::get(lifecycle_manager.metadata, id) {
          Some(meta) => { meta | status: DataStatus::Deleted }
          None => { id: "", status: DataStatus::Deleted, created_at: 0, last_accessed: 0, access_count: 0, size_bytes: 0, retention_policy: "" }
        }
        let _ = Map::insert(lifecycle_manager.metadata, id, updated)
      }
      
      to_delete.length()
    }
  }
  
  // 注册测试数据
  lifecycle_manager.register_data("data-1", 1024, "standard")
  lifecycle_manager.register_data("data-2", 2048, "extended")
  lifecycle_manager.register_data("data-3", 512, "short-term")
  
  // 测试初始状态
  assert_eq(lifecycle_manager.get_status("data-1"), Some(DataStatus::Active))
  assert_eq(lifecycle_manager.get_status("data-2"), Some(DataStatus::Active))
  assert_eq(lifecycle_manager.get_status("data-3"), Some(DataStatus::Active))
  
  // 测试数据访问
  lifecycle_manager.access_data("data-1")
  lifecycle_manager.access_data("data-1")
  lifecycle_manager.access_data("data-2")
  
  // 获取访问后的元数据
  let data1_meta = match Map::get(lifecycle_manager.metadata, "data-1") {
    Some(meta) => meta
    None => { id: "", status: DataStatus::Active, created_at: 0, last_accessed: 0, access_count: 0, size_bytes: 0, retention_policy: "" }
  }
  
  assert_eq(data1_meta.access_count, 2)
  
  // 测试生命周期策略应用
  let policy = {
    max_age_days: 30,
    max_access_count: 1000,
    inactive_threshold_days: 7,
    archive_threshold_days: 90
  }
  
  // 由于所有数据都是刚创建的，应用策略后应该仍然是Active状态
  lifecycle_manager.apply_policy(policy)
  assert_eq(lifecycle_manager.get_status("data-1"), Some(DataStatus::Active))
  assert_eq(lifecycle_manager.get_status("data-2"), Some(DataStatus::Active))
  assert_eq(lifecycle_manager.get_status("data-3"), Some(DataStatus::Active))
  
  // 模拟时间流逝和状态变更
  let now = Time::now()
  let old_timestamp = now - (10 * 24 * 60 * 60 * 1000)  // 10天前
  
  // 手动更新元数据模拟旧数据
  let old_data1 = match Map::get(lifecycle_manager.metadata, "data-1") {
    Some(meta) => { meta | created_at: old_timestamp, last_accessed: old_timestamp }
    None => { id: "", status: DataStatus::Active, created_at: 0, last_accessed: 0, access_count: 0, size_bytes: 0, retention_policy: "" }
  }
  
  let _ = Map::insert(lifecycle_manager.metadata, "data-1", old_data1)
  
  // 应用更严格的策略
  let strict_policy = {
    max_age_days: 5,
    max_access_count: 1000,
    inactive_threshold_days: 2,
    archive_threshold_days: 10
  }
  
  lifecycle_manager.apply_policy(strict_policy)
  
  // data-1应该被标记为PendingDeletion（因为超过最大年龄）
  assert_eq(lifecycle_manager.get_status("data-1"), Some(DataStatus::PendingDeletion))
  
  // 执行清理
  let deleted_count = lifecycle_manager.cleanup()
  assert_eq(deleted_count, 1)
  assert_eq(lifecycle_manager.get_status("data-1"), Some(DataStatus::Deleted))
  
  // 其他数据应该仍然是Active状态
  assert_eq(lifecycle_manager.get_status("data-2"), Some(DataStatus::Active))
  assert_eq(lifecycle_manager.get_status("data-3"), Some(DataStatus::Active))
}