// Azimuth 遥测系统核心测试用例
// 包含遥测数据采集、处理、分析和传输的关键测试

// 测试1: 遥测数据采集和过滤
test "遥测数据采集和智能过滤" {
  // 定义遥测数据点结构
  type TelemetryPoint = {
    timestamp: Int,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status_code: Int,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据点
  let telemetry_points = [
    { timestamp: 1640995200, service_name: "auth-service", operation_name: "login", duration_ms: 120, status_code: 200, attributes: [("user_id", "12345"), ("region", "us-west")] },
    { timestamp: 1640995260, service_name: "payment-service", operation_name: "process", duration_ms: 350, status_code: 200, attributes: [("amount", "100.00"), ("currency", "USD")] },
    { timestamp: 1640995320, service_name: "auth-service", operation_name: "logout", duration_ms: 80, status_code: 200, attributes: [("user_id", "12345"), ("region", "us-west")] },
    { timestamp: 1640995380, service_name: "inventory-service", operation_name: "check_stock", duration_ms: 2000, status_code: 500, attributes: [("product_id", "prod-789"), ("warehouse", "wh-01")] },
    { timestamp: 1640995440, service_name: "notification-service", operation_name: "send_email", duration_ms: 500, status_code: 200, attributes: [("template", "welcome"), ("recipient", "user@example.com")] }
  ]
  
  // 实现时间窗口过滤器
  let time_window_filter = fn(points: Array[TelemetryPoint], start_time: Int, end_time: Int) {
    points.filter(fn(p) { p.timestamp >= start_time and p.timestamp <= end_time })
  }
  
  // 实现服务过滤器
  let service_filter = fn(points: Array[TelemetryPoint], services: Array[String]) {
    points.filter(fn(p) { services.contains(p.service_name) })
  }
  
  // 实现性能过滤器
  let performance_filter = fn(points: Array[TelemetryPoint], threshold_ms: Int) {
    points.filter(fn(p) { p.duration_ms > threshold_ms })
  }
  
  // 实现错误过滤器
  let error_filter = fn(points: Array[TelemetryPoint]) {
    points.filter(fn(p) { p.status_code >= 400 })
  }
  
  // 测试时间窗口过滤
  let windowed_points = time_window_filter(telemetry_points, 1640995200, 1640995320)
  assert_eq(windowed_points.length(), 3)
  assert_eq(windowed_points[0].service_name, "auth-service")
  assert_eq(windowed_points[1].service_name, "payment-service")
  assert_eq(windowed_points[2].service_name, "auth-service")
  
  // 测试服务过滤
  let auth_points = service_filter(telemetry_points, ["auth-service"])
  assert_eq(auth_points.length(), 2)
  assert_true(auth_points.all(fn(p) { p.service_name == "auth-service" }))
  
  // 测试性能过滤
  let slow_points = performance_filter(telemetry_points, 300)
  assert_eq(slow_points.length(), 2)
  assert_true(slow_points.all(fn(p) { p.duration_ms > 300 }))
  
  // 测试错误过滤
  let error_points = error_filter(telemetry_points)
  assert_eq(error_points.length(), 1)
  assert_eq(error_points[0].service_name, "inventory-service")
  assert_eq(error_points[0].status_code, 500)
  
  // 测试组合过滤器
  let combined_points = telemetry_points
    |> time_window_filter(1640995200, 1640995440)
    |> service_filter(["auth-service", "payment-service"])
    |> performance_filter(100)
  
  assert_eq(combined_points.length(), 2)
  assert_true(combined_points.all(fn(p) { p.duration_ms > 100 }))
  assert_true(combined_points.all(fn(p) { p.service_name == "auth-service" or p.service_name == "payment-service" }))
}

// 测试2: 分布式追踪测试
test "分布式追踪链路传播" {
  // 定义追踪上下文结构
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // 定义跨度结构
  type Span = {
    context: TraceContext,
    operation_name: String,
    start_time: Int,
    end_time: Option[Int],
    status: String,
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // 创建根追踪上下文
  let create_root_context = fn() {
    {
      trace_id: "trace_" + Time::now().to_string(),
      span_id: "span_" + Time::now().to_string(),
      parent_span_id: None,
      baggage: [],
      flags: 1
    }
  }
  
  // 创建子上下文
  let create_child_context = fn(parent: TraceContext, operation_name: String) {
    {
      trace_id: parent.trace_id,
      span_id: "span_" + operation_name + "_" + Time::now().to_string(),
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // 创建跨度
  let create_span = fn(context: TraceContext, operation_name: String, start_time: Int) {
    {
      context,
      operation_name,
      start_time,
      end_time: None,
      status: "running",
      events: []
    }
  }
  
  // 完成跨度
  let finish_span = fn(span: Span, end_time: Int, status: String) {
    { span | end_time: Some(end_time), status }
  }
  
  // 添加事件
  let add_event = fn(span: Span, event_name: String, timestamp: Int, attributes: Array[(String, String)]) {
    let event = { timestamp, name: event_name, attributes }
    { span | events: span.events.push(event) }
  }
  
  // 测试追踪链路创建
  let root_context = create_root_context()
  assert_true(root_context.trace_id.starts_with("trace_"))
  assert_true(root_context.span_id.starts_with("span_"))
  assert_eq(root_context.parent_span_id, None)
  
  // 测试子上下文创建
  let child_context = create_child_context(root_context, "database_query")
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_eq(child_context.parent_span_id, Some(root_context.span_id))
  assert_not_eq(child_context.span_id, root_context.span_id)
  
  // 测试跨度创建和完成
  let start_time = 1640995200
  let root_span = create_span(root_context, "http_request", start_time)
  assert_eq(root_span.operation_name, "http_request")
  assert_eq(root_span.start_time, start_time)
  assert_eq(root_span.status, "running")
  assert_eq(root_span.end_time, None)
  
  let end_time = start_time + 500
  let finished_root_span = finish_span(root_span, end_time, "ok")
  assert_eq(finished_root_span.end_time, Some(end_time))
  assert_eq(finished_root_span.status, "ok")
  
  // 测试事件添加
  let span_with_event = add_event(finished_root_span, "cache_miss", start_time + 100, [("cache_key", "user_123")])
  assert_eq(span_with_event.events.length(), 1)
  assert_eq(span_with_event.events[0].name, "cache_miss")
  assert_eq(span_with_event.events[0].timestamp, start_time + 100)
  
  // 测试多级追踪链路
  let db_context = create_child_context(child_context, "sql_query")
  let db_span = create_span(db_context, "select_users", start_time + 50)
  let finished_db_span = finish_span(db_span, start_time + 200, "ok")
  
  // 验证追踪链路关系
  assert_eq(finished_db_span.context.trace_id, root_context.trace_id)
  assert_eq(finished_db_span.context.parent_span_id, Some(child_context.span_id))
  
  // 计算跨度持续时间
  let calculate_duration = fn(span: Span) {
    match span.end_time {
      Some(end) => Some(end - span.start_time)
      None => None
    }
  }
  
  let root_duration = calculate_duration(finished_root_span)
  let db_duration = calculate_duration(finished_db_span)
  
  assert_eq(root_duration, Some(500))
  assert_eq(db_duration, Some(150))
  
  // 测试行李传播
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    { context | baggage: context.baggage.push((key, value)) }
  }
  
  let context_with_baggage = add_baggage(root_context, "user_id", "12345")
  let child_with_baggage = create_child_context(context_with_baggage, "child_operation")
  
  assert_eq(child_with_baggage.baggage.length(), 1)
  assert_eq(child_with_baggage.baggage[0], ("user_id", "12345"))
}

// 测试3: 性能指标计算
test "性能指标计算和聚合" {
  // 定义指标类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义指标点
  type MetricPoint = {
    name: String,
    metric_type: MetricType,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 定义直方图桶
  type HistogramBucket = {
    upper_bound: Float,
    count: Int
  }
  
  // 定义摘要统计
  type SummaryStats = {
    count: Int,
    sum: Float,
    quantiles: Array[(Float, Float)]
  }
  
  // 创建测试指标数据
  let metric_points = [
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 1640995230, labels: [("method", "GET"), ("status", "200")] },
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 1640995240, labels: [("method", "POST"), ("status", "201")] },
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 1640995250, labels: [("method", "GET"), ("status", "404")] },
    { name: "response_time_ms", metric_type: MetricType::Gauge, value: 120.5, timestamp: 1640995260, labels: [("endpoint", "/api/users")] },
    { name: "response_time_ms", metric_type: MetricType::Gauge, value: 85.2, timestamp: 1640995270, labels: [("endpoint", "/api/users")] },
    { name: "request_duration", metric_type: MetricType::Histogram, value: 150.0, timestamp: 1640995280, labels: [("service", "auth")] },
    { name: "request_duration", metric_type: MetricType::Histogram, value: 75.0, timestamp: 1640995290, labels: [("service", "auth")] },
    { name: "request_duration", metric_type: MetricType::Histogram, value: 200.0, timestamp: 1640995300, labels: [("service", "auth")] },
    { name: "request_duration", metric_type: MetricType::Histogram, value: 50.0, timestamp: 1640995310, labels: [("service", "auth")] },
    { name: "request_duration", metric_type: MetricType::Histogram, value: 300.0, timestamp: 1640995320, labels: [("service", "auth")] }
  ]
  
  // 计数器聚合
  let aggregate_counter = fn(points: Array[MetricPoint], metric_name: String) {
    points
      .filter(fn(p) { p.name == metric_name and p.metric_type == MetricType::Counter })
      .reduce(fn(acc, p) { acc + p.value }, 0.0)
  }
  
  // 仪表盘统计
  let gauge_stats = fn(points: Array[MetricPoint], metric_name: String) {
    let filtered_points = points
      .filter(fn(p) { p.name == metric_name and p.metric_type == MetricType::Gauge })
    
    if filtered_points.length() == 0 {
      { min: 0.0, max: 0.0, avg: 0.0, count: 0 }
    } else {
      let values = filtered_points.map(fn(p) { p.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      
      { min, max, avg: sum / (values.length() as Float), count: values.length() }
    }
  }
  
  // 直方图计算
  let calculate_histogram = fn(points: Array[MetricPoint], metric_name: String, buckets: Array[Float]) {
    let values = points
      .filter(fn(p) { p.name == metric_name and p.metric_type == MetricType::Histogram })
      .map(fn(p) { p.value })
    
    let bucket_counts = buckets.map(fn(upper_bound) {
      let count = values.filter(fn(v) { v <= upper_bound }).length()
      { upper_bound, count }
    })
    
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    
    { buckets: bucket_counts, count, sum }
  }
  
  // 分位数计算
  let calculate_quantiles = fn(values: Array[Float], quantiles: Array[Float]) {
    let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let count = sorted_values.length()
    
    quantiles.map(fn(q) {
      let index = ((q * (count as Float)) as Int)
      let value = if index < count { sorted_values[index] } else { sorted_values[count - 1] }
      (q, value)
    })
  }
  
  // 摘要计算
  let calculate_summary = fn(points: Array[MetricPoint], metric_name: String, quantiles: Array[Float]) {
    let values = points
      .filter(fn(p) { p.name == metric_name and p.metric_type == MetricType::Summary })
      .map(fn(p) { p.value })
    
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let quantile_values = calculate_quantiles(values, quantiles)
    
    { count, sum, quantiles: quantile_values }
  }
  
  // 测试计数器聚合
  let total_requests = aggregate_counter(metric_points, "http_requests_total")
  assert_eq(total_requests, 3.0)
  
  // 测试仪表盘统计
  let response_time_stats = gauge_stats(metric_points, "response_time_ms")
  assert_eq(response_time_stats.count, 2)
  assert_eq(response_time_stats.min, 85.2)
  assert_eq(response_time_stats.max, 120.5)
  assert_eq(response_time_stats.avg.round(), 102.85)
  
  // 测试直方图计算
  let histogram_buckets = [50.0, 100.0, 200.0, 300.0, 500.0]
  let duration_histogram = calculate_histogram(metric_points, "request_duration", histogram_buckets)
  assert_eq(duration_histogram.count, 5)
  assert_eq(duration_histogram.sum, 775.0)
  
  // 验证桶计数
  let bucket_50 = duration_histogram.buckets.filter(fn(b) { b.upper_bound == 50.0 })[0]
  let bucket_100 = duration_histogram.buckets.filter(fn(b) { b.upper_bound == 100.0 })[0]
  let bucket_200 = duration_histogram.buckets.filter(fn(b) { b.upper_bound == 200.0 })[0]
  let bucket_300 = duration_histogram.buckets.filter(fn(b) { b.upper_bound == 300.0 })[0]
  
  assert_eq(bucket_50.count, 1)  // 50ms
  assert_eq(bucket_100.count, 2)  // 50ms, 75ms
  assert_eq(bucket_200.count, 4)  // 50ms, 75ms, 150ms, 200ms
  assert_eq(bucket_300.count, 5)  // 所有值
  
  // 测试分位数计算
  let duration_values = [50.0, 75.0, 150.0, 200.0, 300.0]
  let quantiles = [0.5, 0.9, 0.95, 0.99]
  let quantile_results = calculate_quantiles(duration_values, quantiles)
  
  assert_eq(quantile_results.length(), 4)
  assert_eq(quantile_results[0], (0.5, 150.0))  // 中位数
  assert_eq(quantile_results[1], (0.9, 300.0))  // 90百分位数
  
  // 测试按标签分组
  let group_by_labels = fn(points: Array[MetricPoint], label_key: String) {
    let groups = Map::empty()
    
    for point in points {
      let label_value = match point.labels.find(fn(l) { l.0 == label_key }) {
        Some((_, value)) => value
        None => "unknown"
      }
      
      let group_points = match Map::get(groups, label_value) {
        Some(pts) => pts
        None => []
      }
      
      let _ = Map::insert(groups, label_value, group_points.push(point))
    }
    
    groups
  }
  
  let grouped_by_method = group_by_labels(metric_points, "method")
  assert_true(Map::has_key(grouped_by_method, "GET"))
  assert_true(Map::has_key(grouped_by_method, "POST"))
  
  let get_requests = match Map::get(grouped_by_method, "GET") {
    Some(points) => points
    None => []
  }
  assert_eq(get_requests.length(), 2)
}

// 测试4: 日志聚合和分析
test "日志聚合和分析处理" {
  // 定义日志级别
  enum LogLevel {
    Trace
    Debug
    Info
    Warn
    Error
    Fatal
  }
  
  // 定义日志条目
  type LogEntry = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    service: String,
    trace_id: Option[String],
    span_id: Option[String],
    attributes: Array[(String, String)]
  }
  
  // 定义日志聚合结果
  type LogAggregation = {
    total_count: Int,
    level_counts: Map[String, Int],
    service_counts: Map[String, Int],
    error_patterns: Array[String],
    time_distribution: Map[String, Int]
  }
  
  // 创建测试日志数据
  let log_entries = [
    { timestamp: 1640995200, level: LogLevel::Info, message: "User login successful", service: "auth-service", trace_id: Some("trace-123"), span_id: Some("span-456"), attributes: [("user_id", "12345")] },
    { timestamp: 1640995210, level: LogLevel::Info, message: "Payment processed", service: "payment-service", trace_id: Some("trace-123"), span_id: Some("span-789"), attributes: [("amount", "100.00")] },
    { timestamp: 1640995220, level: LogLevel::Error, message: "Database connection failed", service: "inventory-service", trace_id: Some("trace-456"), span_id: Some("span-123"), attributes: [("error_code", "CONN_TIMEOUT")] },
    { timestamp: 1640995230, level: LogLevel::Warn, message: "High memory usage detected", service: "auth-service", trace_id: Some("trace-789"), span_id: Some("span-234"), attributes: [("memory_usage", "85%")] },
    { timestamp: 1640995240, level: LogLevel::Error, message: "Payment gateway timeout", service: "payment-service", trace_id: Some("trace-123"), span_id: Some("span-789"), attributes: [("timeout_ms", "5000")] },
    { timestamp: 1640995250, level: LogLevel::Info, message: "Email notification sent", service: "notification-service", trace_id: Some("trace-123"), span_id: Some("span-345"), attributes: [("recipient", "user@example.com")] },
    { timestamp: 1640995260, level: LogLevel::Debug, message: "Cache hit for user profile", service: "auth-service", trace_id: Some("trace-123"), span_id: Some("span-456"), attributes: [("cache_key", "user_12345")] },
    { timestamp: 1640995270, level: LogLevel::Error, message: "Database connection failed", service: "inventory-service", trace_id: Some("trace-456"), span_id: Some("span-567"), attributes: [("error_code", "CONN_TIMEOUT")] }
  ]
  
  // 日志级别转换为字符串
  let level_to_string = fn(level: LogLevel) {
    match level {
      LogLevel::Trace => "TRACE"
      LogLevel::Debug => "DEBUG"
      LogLevel::Info => "INFO"
      LogLevel::Warn => "WARN"
      LogLevel::Error => "ERROR"
      LogLevel::Fatal => "FATAL"
    }
  }
  
  // 按时间窗口聚合日志
  let aggregate_by_time_window = fn(entries: Array[LogEntry], window_size_minutes: Int) {
    let windows = Map::empty()
    
    for entry in entries {
      let window_start = (entry.timestamp / (window_size_minutes * 60)) * (window_size_minutes * 60)
      let window_key = window_start.to_string()
      
      let window_entries = match Map::get(windows, window_key) {
        Some(es) => es
        None => []
      }
      
      let _ = Map::insert(windows, window_key, window_entries.push(entry))
    }
    
    windows
  }
  
  // 按日志级别聚合
  let aggregate_by_level = fn(entries: Array[LogEntry]) {
    let level_counts = Map::empty()
    
    for entry in entries {
      let level_str = level_to_string(entry.level)
      let count = match Map::get(level_counts, level_str) {
        Some(c) => c + 1
        None => 1
      }
      let _ = Map::insert(level_counts, level_str, count)
    }
    
    level_counts
  }
  
  // 按服务聚合
  let aggregate_by_service = fn(entries: Array[LogEntry]) {
    let service_counts = Map::empty()
    
    for entry in entries {
      let count = match Map::get(service_counts, entry.service) {
        Some(c) => c + 1
        None => 1
      }
      let _ = Map::insert(service_counts, entry.service, count)
    }
    
    service_counts
  }
  
  // 提取错误模式
  let extract_error_patterns = fn(entries: Array[LogEntry]) {
    let error_entries = entries.filter(fn(e) { e.level == LogLevel::Error or e.level == LogLevel::Fatal })
    let patterns = Map::empty()
    
    for entry in error_entries {
      // 简单的模式提取：提取错误消息的前三个词
      let words = entry.message.split(" ")
      let pattern = if words.length() >= 3 {
        words[0] + " " + words[1] + " " + words[2]
      } else {
        entry.message
      }
      
      let count = match Map::get(patterns, pattern) {
        Some(c) => c + 1
        None => 1
      }
      let _ = Map::insert(patterns, pattern, count)
    }
    
    // 转换为数组并按频率排序
    let pattern_array = []
    for (pattern, count) in patterns {
      pattern_array = pattern_array.push((pattern, count))
    }
    
    pattern_array.sort(fn(a, b) { if a.1 > b.1 { -1 } else if a.1 < b.1 { 1 } else { 0 } })
      .map(fn(p) { p.0 })
  }
  
  // 按追踪ID关联日志
  let correlate_by_trace = fn(entries: Array[LogEntry]) {
    let trace_groups = Map::empty()
    
    for entry in entries {
      match entry.trace_id {
        Some(trace_id) => {
          let trace_entries = match Map::get(trace_groups, trace_id) {
            Some(es) => es
            None => []
          }
          let _ = Map::insert(trace_groups, trace_id, trace_entries.push(entry))
        }
        None => {}
      }
    }
    
    trace_groups
  }
  
  // 测试时间窗口聚合
  let time_windows = aggregate_by_time_window(log_entries, 1)  // 1分钟窗口
  assert_eq(Map::size(time_windows), 1)  // 所有日志都在同一分钟内
  
  let window_entries = match Map::get(time_windows, "1640995200") {
    Some(es) => es
    None => []
  }
  assert_eq(window_entries.length(), 8)
  
  // 测试日志级别聚合
  let level_counts = aggregate_by_level(log_entries)
  assert_eq(match Map::get(level_counts, "INFO") { Some(c) => c | None => 0 }, 3)
  assert_eq(match Map::get(level_counts, "ERROR") { Some(c) => c | None => 0 }, 3)
  assert_eq(match Map::get(level_counts, "WARN") { Some(c) => c | None => 0 }, 1)
  assert_eq(match Map::get(level_counts, "DEBUG") { Some(c) => c | None => 0 }, 1)
  
  // 测试服务聚合
  let service_counts = aggregate_by_service(log_entries)
  assert_eq(match Map::get(service_counts, "auth-service") { Some(c) => c | None => 0 }, 3)
  assert_eq(match Map::get(service_counts, "payment-service") { Some(c) => c | None => 0 }, 2)
  assert_eq(match Map::get(service_counts, "inventory-service") { Some(c) => c | None => 0 }, 2)
  assert_eq(match Map::get(service_counts, "notification-service") { Some(c) => c | None => 0 }, 1)
  
  // 测试错误模式提取
  let error_patterns = extract_error_patterns(log_entries)
  assert_eq(error_patterns.length(), 2)
  assert_eq(error_patterns[0], "Database connection failed")  // 出现2次
  assert_eq(error_patterns[1], "Payment gateway timeout")   // 出现1次
  
  // 测试追踪关联
  let trace_groups = correlate_by_trace(log_entries)
  assert_eq(Map::size(trace_groups), 3)  // 3个不同的追踪ID
  
  let trace_123_entries = match Map::get(trace_groups, "trace-123") {
    Some(es) => es
    None => []
  }
  assert_eq(trace_123_entries.length(), 5)  // 5条日志属于trace-123
  
  let trace_456_entries = match Map::get(trace_groups, "trace-456") {
    Some(es) => es
    None => []
  }
  assert_eq(trace_456_entries.length(), 2)  // 2条日志属于trace-456
  
  // 测试错误率计算
  let calculate_error_rate = fn(entries: Array[LogEntry]) {
    let total = entries.length()
    let errors = entries.filter(fn(e) { e.level == LogLevel::Error or e.level == LogLevel::Fatal }).length()
    if total > 0 {
      (errors as Float) / (total as Float)
    } else {
      0.0
    }
  }
  
  let overall_error_rate = calculate_error_rate(log_entries)
  assert_eq(overall_error_rate, 0.375)  // 3个错误 / 8个总日志
  
  // 按服务计算错误率
  let calculate_service_error_rates = fn(entries: Array[LogEntry]) {
    let service_groups = aggregate_by_service(entries)
    let service_error_rates = Map::empty()
    
    for (service, _) in service_groups {
      let service_entries = entries.filter(fn(e) { e.service == service })
      let error_rate = calculate_error_rate(service_entries)
      let _ = Map::insert(service_error_rates, service, error_rate)
    }
    
    service_error_rates
  }
  
  let service_error_rates = calculate_service_error_rates(log_entries)
  assert_eq(match Map::get(service_error_rates, "inventory-service") { Some(r) => r | None => 0.0 }, 1.0)  // 2个错误 / 2个总日志
  assert_eq(match Map::get(service_error_rates, "payment-service") { Some(r) => r | None => 0.0 }, 0.5)  // 1个错误 / 2个总日志
  assert_eq(match Map::get(service_error_rates, "auth-service") { Some(r) => r | None => 0.0 }, 0.0)    // 0个错误 / 3个总日志
}

// 测试5: 服务网格遥测
test "服务网格遥测数据收集" {
  // 定义服务网格组件
  enum MeshComponent {
    Gateway
    Sidecar
    ControlPlane
    Egress
    Ingress
  }
  
  // 定义网格流量类型
  enum TrafficType {
    Inbound
    Outbound
    Internal
  }
  
  // 定义网格遥测点
  type MeshTelemetryPoint = {
    timestamp: Int,
    source_service: String,
    destination_service: String,
    component: MeshComponent,
    traffic_type: TrafficType,
    request_count: Int,
    response_count: Int,
    error_count: Int,
    latency_ms: Int,
    bytes_sent: Int,
    bytes_received: Int,
    protocol: String
  }
  
  // 定义服务拓扑节点
  type TopologyNode = {
    service: String,
    inbound_connections: Array[String],
    outbound_connections: Array[String],
    request_rate: Float,
    error_rate: Float,
    avg_latency: Float
  }
  
  // 创建测试网格遥测数据
  let mesh_telemetry = [
    { timestamp: 1640995200, source_service: "frontend", destination_service: "auth-service", component: MeshComponent::Sidecar, traffic_type: TrafficType::Outbound, request_count: 100, response_count: 95, error_count: 5, latency_ms: 120, bytes_sent: 10240, bytes_received: 20480, protocol: "HTTP" },
    { timestamp: 1640995210, source_service: "frontend", destination_service: "product-service", component: MeshComponent::Sidecar, traffic_type: TrafficType::Outbound, request_count: 80, response_count: 78, error_count: 2, latency_ms: 200, bytes_sent: 8192, bytes_received: 16384, protocol: "HTTP" },
    { timestamp: 1640995220, source_service: "auth-service", destination_service: "user-db", component: MeshComponent::Sidecar, traffic_type: TrafficType::Outbound, request_count: 50, response_count: 50, error_count: 0, latency_ms: 80, bytes_sent: 5120, bytes_received: 10240, protocol: "TCP" },
    { timestamp: 1640995230, source_service: "product-service", destination_service: "inventory-db", component: MeshComponent::Sidecar, traffic_type: TrafficType::Outbound, request_count: 40, response_count: 38, error_count: 2, latency_ms: 150, bytes_sent: 4096, bytes_received: 8192, protocol: "TCP" },
    { timestamp: 1640995240, source_service: "external-client", destination_service: "frontend", component: MeshComponent::Gateway, traffic_type: TrafficType::Inbound, request_count: 200, response_count: 190, error_count: 10, latency_ms: 250, bytes_sent: 20480, bytes_received: 40960, protocol: "HTTP" },
    { timestamp: 1640995250, source_service: "payment-service", destination_service: "payment-gateway", component: MeshComponent::Egress, traffic_type: TrafficType::Outbound, request_count: 20, response_count: 18, error_count: 2, latency_ms: 500, bytes_sent: 2048, bytes_received: 1024, protocol: "HTTPS" },
    { timestamp: 1640995260, source_service: "notification-service", destination_service: "email-service", component: MeshComponent::Sidecar, traffic_type: TrafficType::Outbound, request_count: 30, response_count: 30, error_count: 0, latency_ms: 100, bytes_sent: 3072, bytes_received: 1536, protocol: "SMTP" }
  ]
  
  // 按服务分组遥测数据
  let group_by_service = fn(points: Array[MeshTelemetryPoint]) {
    let services = Map::empty()
    
    for point in points {
      // 处理源服务
      let source_data = match Map::get(services, point.source_service) {
        Some(data) => data
        None => { requests: 0, responses: 0, errors: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
      }
      
      let updated_source = {
        requests: source_data.requests + point.request_count,
        responses: source_data.responses + point.response_count,
        errors: source_data.errors + point.error_count,
        latency_sum: source_data.latency_sum + point.latency_ms,
        latency_count: source_data.latency_count + 1,
        bytes_sent: source_data.bytes_sent + point.bytes_sent,
        bytes_received: source_data.bytes_received + point.bytes_received
      }
      
      let _ = Map::insert(services, point.source_service, updated_source)
      
      // 处理目标服务
      let dest_data = match Map::get(services, point.destination_service) {
        Some(data) => data
        None => { requests: 0, responses: 0, errors: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
      }
      
      let updated_dest = {
        requests: dest_data.requests + point.request_count,
        responses: dest_data.responses + point.response_count,
        errors: dest_data.errors + point.error_count,
        latency_sum: dest_data.latency_sum + point.latency_ms,
        latency_count: dest_data.latency_count + 1,
        bytes_sent: dest_data.bytes_sent + point.bytes_sent,
        bytes_received: dest_data.bytes_received + point.bytes_received
      }
      
      let _ = Map::insert(services, point.destination_service, updated_dest)
    }
    
    services
  }
  
  // 构建服务拓扑
  let build_topology = fn(points: Array[MeshTelemetryPoint]) {
    let nodes = Map::empty()
    let connections = Map::empty()
    
    for point in points {
      // 添加源服务节点
      let source_node = match Map::get(nodes, point.source_service) {
        Some(node) => node
        None => {
          service: point.source_service,
          inbound_connections: [],
          outbound_connections: [],
          request_rate: 0.0,
          error_rate: 0.0,
          avg_latency: 0.0
        }
      }
      
      // 更新源服务的出站连接
      let updated_source_outbound = if not(source_node.outbound_connections.contains(point.destination_service)) {
        source_node.outbound_connections.push(point.destination_service)
      } else {
        source_node.outbound_connections
      }
      
      let updated_source = { source_node | outbound_connections: updated_source_outbound }
      let _ = Map::insert(nodes, point.source_service, updated_source)
      
      // 添加目标服务节点
      let dest_node = match Map::get(nodes, point.destination_service) {
        Some(node) => node
        None => {
          service: point.destination_service,
          inbound_connections: [],
          outbound_connections: [],
          request_rate: 0.0,
          error_rate: 0.0,
          avg_latency: 0.0
        }
      }
      
      // 更新目标服务的入站连接
      let updated_dest_inbound = if not(dest_node.inbound_connections.contains(point.source_service)) {
        dest_node.inbound_connections.push(point.source_service)
      } else {
        dest_node.inbound_connections
      }
      
      let updated_dest = { dest_node | inbound_connections: updated_dest_inbound }
      let _ = Map::insert(nodes, point.destination_service, updated_dest)
      
      // 记录连接指标
      let connection_key = point.source_service + "->" + point.destination_service
      let connection_data = match Map::get(connections, connection_key) {
        Some(data) => data
        None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0 }
      }
      
      let updated_connection = {
        request_count: connection_data.request_count + point.request_count,
        error_count: connection_data.error_count + point.error_count,
        latency_sum: connection_data.latency_sum + point.latency_ms,
        latency_count: connection_data.latency_count + 1
      }
      
      let _ = Map::insert(connections, connection_key, updated_connection)
    }
    
    // 计算节点指标
    let calculated_nodes = Map::empty()
    for (service, node) in nodes {
      // 计算请求率（简化：使用总请求数）
      let total_requests = match Map::get(connections, service + "->*") {
        Some(data) => data.request_count
        None => {
          // 计算该服务的所有出站请求
          let mut total = 0
          for (key, data) in connections {
            if key.starts_with(service + "->") {
              total = total + data.request_count
            }
          }
          total
        }
      }
      
      // 计算错误率
      let total_errors = match Map::get(connections, service + "->*") {
        Some(data) => data.error_count
        None => {
          // 计算该服务的所有出站错误
          let mut total = 0
          for (key, data) in connections {
            if key.starts_with(service + "->") {
              total = total + data.error_count
            }
          }
          total
        }
      }
      
      let error_rate = if total_requests > 0 {
        (total_errors as Float) / (total_requests as Float)
      } else {
        0.0
      }
      
      // 计算平均延迟
      let total_latency = match Map::get(connections, service + "->*") {
        Some(data) => data.latency_sum
        None => {
          // 计算该服务的所有出站延迟
          let mut total = 0
          let mut count = 0
          for (key, data) in connections {
            if key.starts_with(service + "->") {
              total = total + data.latency_sum
              count = count + data.latency_count
            }
          }
          if count > 0 { total / count } else { 0 }
        }
      }
      
      let calculated_node = {
        service: node.service,
        inbound_connections: node.inbound_connections,
        outbound_connections: node.outbound_connections,
        request_rate: total_requests as Float,
        error_rate,
        avg_latency: total_latency as Float
      }
      
      let _ = Map::insert(calculated_nodes, service, calculated_node)
    }
    
    calculated_nodes
  }
  
  // 按组件分析流量
  let analyze_by_component = fn(points: Array[MeshTelemetryPoint]) {
    let components = Map::empty()
    
    for point in points {
      let component_str = match point.component {
        MeshComponent::Gateway => "Gateway"
        MeshComponent::Sidecar => "Sidecar"
        MeshComponent::ControlPlane => "ControlPlane"
        MeshComponent::Egress => "Egress"
        MeshComponent::Ingress => "Ingress"
      }
      
      let component_data = match Map::get(components, component_str) {
        Some(data) => data
        None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
      }
      
      let updated_data = {
        request_count: component_data.request_count + point.request_count,
        error_count: component_data.error_count + point.error_count,
        latency_sum: component_data.latency_sum + point.latency_ms,
        latency_count: component_data.latency_count + 1,
        bytes_sent: component_data.bytes_sent + point.bytes_sent,
        bytes_received: component_data.bytes_received + point.bytes_received
      }
      
      let _ = Map::insert(components, component_str, updated_data)
    }
    
    components
  }
  
  // 测试服务分组
  let service_groups = group_by_service(mesh_telemetry)
  assert_eq(Map::size(service_groups), 7)  // 7个不同的服务
  
  let frontend_data = match Map::get(service_groups, "frontend") {
    Some(data) => data
    None => { requests: 0, responses: 0, errors: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
  }
  assert_eq(frontend_data.requests, 300)  // 100 + 80 + 200 (作为目标服务)
  assert_eq(frontend_data.responses, 288)
  assert_eq(frontend_data.errors, 12)
  
  // 测试拓扑构建
  let topology = build_topology(mesh_telemetry)
  assert_eq(Map::size(topology), 7)  // 7个节点
  
  let frontend_node = match Map::get(topology, "frontend") {
    Some(node) => node
    None => { service: "", inbound_connections: [], outbound_connections: [], request_rate: 0.0, error_rate: 0.0, avg_latency: 0.0 }
  }
  assert_eq(frontend_node.service, "frontend")
  assert_eq(frontend_node.outbound_connections.length(), 2)  // 到auth-service和product-service
  assert_true(frontend_node.outbound_connections.contains("auth-service"))
  assert_true(frontend_node.outbound_connections.contains("product-service"))
  assert_eq(frontend_node.inbound_connections.length(), 1)  // 从external-client
  assert_true(frontend_node.inbound_connections.contains("external-client"))
  
  // 测试组件分析
  let component_analysis = analyze_by_component(mesh_telemetry)
  assert_eq(Map::size(component_analysis), 3)  // Gateway, Sidecar, Egress
  
  let sidecar_data = match Map::get(component_analysis, "Sidecar") {
    Some(data) => data
    None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
  }
  assert_eq(sidecar_data.request_count, 270)  // 100 + 80 + 50 + 40
  assert_eq(sidecar_data.error_count, 9)      // 5 + 2 + 0 + 2
  
  let gateway_data = match Map::get(component_analysis, "Gateway") {
    Some(data) => data
    None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
  }
  assert_eq(gateway_data.request_count, 200)
  assert_eq(gateway_data.error_count, 10)
  
  let egress_data = match Map::get(component_analysis, "Egress") {
    Some(data) => data
    None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0, bytes_sent: 0, bytes_received: 0 }
  }
  assert_eq(egress_data.request_count, 20)
  assert_eq(egress_data.error_count, 2)
  
  // 测试流量类型分析
  let analyze_by_traffic_type = fn(points: Array[MeshTelemetryPoint]) {
    let traffic_types = Map::empty()
    
    for point in points {
      let type_str = match point.traffic_type {
        TrafficType::Inbound => "Inbound"
        TrafficType::Outbound => "Outbound"
        TrafficType::Internal => "Internal"
      }
      
      let type_data = match Map::get(traffic_types, type_str) {
        Some(data) => data
        None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0 }
      }
      
      let updated_data = {
        request_count: type_data.request_count + point.request_count,
        error_count: type_data.error_count + point.error_count,
        latency_sum: type_data.latency_sum + point.latency_ms,
        latency_count: type_data.latency_count + 1
      }
      
      let _ = Map::insert(traffic_types, type_str, updated_data)
    }
    
    traffic_types
  }
  
  let traffic_analysis = analyze_by_traffic_type(mesh_telemetry)
  assert_eq(Map::size(traffic_analysis), 2)  // Inbound和Outbound
  
  let inbound_data = match Map::get(traffic_analysis, "Inbound") {
    Some(data) => data
    None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0 }
  }
  assert_eq(inbound_data.request_count, 200)
  assert_eq(inbound_data.error_count, 10)
  
  let outbound_data = match Map::get(traffic_analysis, "Outbound") {
    Some(data) => data
    None => { request_count: 0, error_count: 0, latency_sum: 0, latency_count: 0 }
  }
  assert_eq(outbound_data.request_count, 320)  // 100 + 80 + 50 + 40 + 20 + 30
  assert_eq(outbound_data.error_count, 11)     // 5 + 2 + 0 + 2 + 2 + 0
}

// 测试6: 实时监控告警
test "实时监控告警系统" {
  // 定义告警级别
  enum AlertSeverity {
    Info
    Warning
    Critical
    Emergency
  }
  
  // 定义告警状态
  enum AlertStatus {
    Firing
    Resolved
    Suppressed
  }
  
  // 定义告警条件
  type AlertCondition = {
    name: String,
    metric_name: String,
    threshold: Float,
    operator: String,  // ">", "<", ">=", "<=", "=="
    duration_seconds: Int,
    severity: AlertSeverity,
    labels: Array[(String, String)]
  }
  
  // 定义告警实例
  type AlertInstance = {
    id: String,
    condition_name: String,
    severity: AlertSeverity,
    status: AlertStatus,
    start_time: Int,
    end_time: Option[Int],
    message: String,
    labels: Array[(String, String)],
    annotations: Array[(String, String)]
  }
  
  // 定义指标数据点
  type MetricDataPoint = {
    name: String,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 创建测试告警条件
  let alert_conditions = [
    {
      name: "high_error_rate",
      metric_name: "error_rate",
      threshold: 0.1,  // 10%
      operator: ">",
      duration_seconds: 300,  // 5分钟
      severity: AlertSeverity::Critical,
      labels: [("team", "backend"), ("service", "*")]
    },
    {
      name: "high_latency",
      metric_name: "request_latency",
      threshold: 1000.0,  // 1000ms
      operator: ">",
      duration_seconds: 180,  // 3分钟
      severity: AlertSeverity::Warning,
      labels: [("team", "backend"), ("endpoint", "*")]
    },
    {
      name: "low_throughput",
      metric_name: "requests_per_second",
      threshold: 10.0,
      operator: "<",
      duration_seconds: 600,  // 10分钟
      severity: AlertSeverity::Warning,
      labels: [("team", "frontend"), ("service", "*")]
    },
    {
      name: "high_memory_usage",
      metric_name: "memory_usage_percent",
      threshold: 90.0,
      operator: ">",
      duration_seconds: 120,  // 2分钟
      severity: AlertSeverity::Critical,
      labels: [("team", "infra"), ("instance", "*")]
    }
  ]
  
  // 创建测试指标数据
  let metric_data = [
    { name: "error_rate", value: 0.05, timestamp: 1640995200, labels: [("service", "auth"), ("endpoint", "/login")] },
    { name: "error_rate", value: 0.08, timestamp: 1640995260, labels: [("service", "auth"), ("endpoint", "/login")] },
    { name: "error_rate", value: 0.12, timestamp: 1640995320, labels: [("service", "auth"), ("endpoint", "/login")] },
    { name: "error_rate", value: 0.15, timestamp: 1640995380, labels: [("service", "auth"), ("endpoint", "/login")] },
    { name: "error_rate", value: 0.18, timestamp: 1640995440, labels: [("service", "auth"), ("endpoint", "/login")] },
    { name: "request_latency", value: 800.0, timestamp: 1640995200, labels: [("service", "payment"), ("endpoint", "/process")] },
    { name: "request_latency", value: 1100.0, timestamp: 1640995260, labels: [("service", "payment"), ("endpoint", "/process")] },
    { name: "request_latency", value: 1200.0, timestamp: 1640995320, labels: [("service", "payment"), ("endpoint", "/process")] },
    { name: "request_latency", value: 1050.0, timestamp: 1640995380, labels: [("service", "payment"), ("endpoint", "/process")] },
    { name: "requests_per_second", value: 15.0, timestamp: 1640995200, labels: [("service", "frontend"), ("endpoint", "/home")] },
    { name: "requests_per_second", value: 12.0, timestamp: 1640995260, labels: [("service", "frontend"), ("endpoint", "/home")] },
    { name: "requests_per_second", value: 8.0, timestamp: 1640995320, labels: [("service", "frontend"), ("endpoint", "/home")] },
    { name: "requests_per_second", value: 5.0, timestamp: 1640995380, labels: [("service", "frontend"), ("endpoint", "/home")] },
    { name: "requests_per_second", value: 3.0, timestamp: 1640995440, labels: [("service", "frontend"), ("endpoint", "/home")] },
    { name: "memory_usage_percent", value: 85.0, timestamp: 1640995200, labels: [("instance", "server-1"), ("datacenter", "dc1")] },
    { name: "memory_usage_percent", value: 88.0, timestamp: 1640995260, labels: [("instance", "server-1"), ("datacenter", "dc1")] },
    { name: "memory_usage_percent", value: 92.0, timestamp: 1640995320, labels: [("instance", "server-1"), ("datacenter", "dc1")] },
    { name: "memory_usage_percent", value: 95.0, timestamp: 1640995380, labels: [("instance", "server-1"), ("datacenter", "dc1")] }
  ]
  
  // 严重级别转换为字符串
  let severity_to_string = fn(severity: AlertSeverity) {
    match severity {
      AlertSeverity::Info => "Info"
      AlertSeverity::Warning => "Warning"
      AlertSeverity::Critical => "Critical"
      AlertSeverity::Emergency => "Emergency"
    }
  }
  
  // 状态转换为字符串
  let status_to_string = fn(status: AlertStatus) {
    match status {
      AlertStatus::Firing => "Firing"
      AlertStatus::Resolved => "Resolved"
      AlertStatus::Suppressed => "Suppressed"
    }
  }
  
  // 评估告警条件
  let evaluate_condition = fn(condition: AlertCondition, data_points: Array[MetricDataPoint], current_time: Int) {
    // 过滤相关的指标数据
    let relevant_data = data_points.filter(fn(point) {
      point.name == condition.metric_name and
      point.timestamp >= current_time - condition.duration_seconds
    })
    
    if relevant_data.length() == 0 {
      return { should_fire: false, message: "No data available" }
    }
    
    // 检查标签匹配
    let label_matches = fn(data_labels: Array[(String, String)], condition_labels: Array[(String, String)]) {
      for (cond_key, cond_value) in condition_labels {
        match data_labels.find(fn(l) { l.0 == cond_key }) {
          Some((_, data_value)) => {
            if cond_value != "*" and data_value != cond_value {
              return false
            }
          }
          None => return false
        }
      }
      true
    }
    
    let matching_data = relevant_data.filter(fn(point) { label_matches(point.labels, condition.labels) })
    
    if matching_data.length() == 0 {
      return { should_fire: false, message: "No matching labels" }
    }
    
    // 检查阈值条件
    let check_threshold = fn(value: Float, threshold: Float, operator: String) {
      match operator {
        ">" => value > threshold
        "<" => value < threshold
        ">=" => value >= threshold
        "<=" => value <= threshold
        "==" => value == threshold
        _ => false
      }
    }
    
    let violating_points = matching_data.filter(fn(point) {
      check_threshold(point.value, condition.threshold, condition.operator)
    })
    
    // 如果所有数据点都违反条件，则触发告警
    let should_fire = violating_points.length() == matching_data.length()
    
    if should_fire {
      let latest_value = matching_data[matching_data.length() - 1].value
      let message = condition.metric_name + " is " + latest_value.to_string() + " " + condition.operator + " " + condition.threshold.to_string()
      { should_fire: true, message }
    } else {
      { should_fire: false, message: "Threshold not met" }
    }
  }
  
  // 创建告警实例
  let create_alert = fn(condition: AlertCondition, evaluation: { should_fire: Bool, message: String }, current_time: Int) {
    if evaluation.should_fire {
      Some({
        id: condition.name + "_" + current_time.to_string(),
        condition_name: condition.name,
        severity: condition.severity,
        status: AlertStatus::Firing,
        start_time: current_time,
        end_time: None,
        message: evaluation.message,
        labels: condition.labels,
        annotations: [("evaluation_time", current_time.to_string())]
      })
    } else {
      None
    }
  }
  
  // 测试告警条件评估
  let current_time = 1640995500
  
  // 测试高错误率告警
  let error_rate_condition = alert_conditions[0]
  let error_rate_evaluation = evaluate_condition(error_rate_condition, metric_data, current_time)
  assert_true(error_rate_evaluation.should_fire)
  assert_true(error_rate_evaluation.message.contains("error_rate"))
  assert_true(error_rate_evaluation.message.contains("0.18"))  // 最新值
  
  let error_rate_alert = create_alert(error_rate_condition, error_rate_evaluation, current_time)
  assert_true(error_rate_alert.is_some())
  
  match error_rate_alert {
    Some(alert) => {
      assert_eq(alert.condition_name, "high_error_rate")
      assert_eq(alert.severity, AlertSeverity::Critical)
      assert_eq(alert.status, AlertStatus::Firing)
      assert_eq(alert.start_time, current_time)
      assert_eq(alert.end_time, None)
    }
    None => assert_true(false)
  }
  
  // 测试高延迟告警
  let latency_condition = alert_conditions[1]
  let latency_evaluation = evaluate_condition(latency_condition, metric_data, current_time)
  assert_true(latency_evaluation.should_fire)
  assert_true(latency_evaluation.message.contains("request_latency"))
  assert_true(latency_evaluation.message.contains("1050.0"))  // 最新值
  
  let latency_alert = create_alert(latency_condition, latency_evaluation, current_time)
  assert_true(latency_alert.is_some())
  
  match latency_alert {
    Some(alert) => {
      assert_eq(alert.condition_name, "high_latency")
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_eq(alert.status, AlertStatus::Firing)
    }
    None => assert_true(false)
  }
  
  // 测试低吞吐量告警
  let throughput_condition = alert_conditions[2]
  let throughput_evaluation = evaluate_condition(throughput_condition, metric_data, current_time)
  assert_true(throughput_evaluation.should_fire)
  assert_true(throughput_evaluation.message.contains("requests_per_second"))
  assert_true(throughput_evaluation.message.contains("3.0"))  // 最新值
  
  let throughput_alert = create_alert(throughput_condition, throughput_evaluation, current_time)
  assert_true(throughput_alert.is_some())
  
  match throughput_alert {
    Some(alert) => {
      assert_eq(alert.condition_name, "low_throughput")
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_eq(alert.status, AlertStatus::Firing)
    }
    None => assert_true(false)
  }
  
  // 测试高内存使用告警
  let memory_condition = alert_conditions[3]
  let memory_evaluation = evaluate_condition(memory_condition, metric_data, current_time)
  assert_true(memory_evaluation.should_fire)
  assert_true(memory_evaluation.message.contains("memory_usage_percent"))
  assert_true(memory_evaluation.message.contains("95.0"))  // 最新值
  
  let memory_alert = create_alert(memory_condition, memory_evaluation, current_time)
  assert_true(memory_alert.is_some())
  
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.condition_name, "high_memory_usage")
      assert_eq(alert.severity, AlertSeverity::Critical)
      assert_eq(alert.status, AlertStatus::Firing)
    }
    None => assert_true(false)
  }
  
  // 测试不会触发的条件（时间提前，没有足够的数据点）
  let early_time = 1640995250  // 提前的时间点
  let early_evaluation = evaluate_condition(error_rate_condition, metric_data, early_time)
  assert_false(early_evaluation.should_fire)
  assert_eq(early_evaluation.message, "Threshold not met")
  
  // 测试告警解决
  let resolve_alert = fn(alert: AlertInstance, resolve_time: Int) {
    { alert | status: AlertStatus::Resolved, end_time: Some(resolve_time) }
  }
  
  match error_rate_alert {
    Some(alert) => {
      let resolved_alert = resolve_alert(alert, current_time + 300)
      assert_eq(resolved_alert.status, AlertStatus::Resolved)
      assert_eq(resolved_alert.end_time, Some(current_time + 300))
    }
    None => assert_true(false)
  }
  
  // 测试告警分组
  let group_alerts = fn(alerts: Array[AlertInstance]) {
    let groups = Map::empty()
    
    for alert in alerts {
      let group_key = severity_to_string(alert.severity)
      let group_alerts = match Map::get(groups, group_key) {
        Some(a) => a
        None => []
      }
      
      let _ = Map::insert(groups, group_key, group_alerts.push(alert))
    }
    
    groups
  }
  
  let all_alerts = [
    error_rate_alert.unwrap(),
    latency_alert.unwrap(),
    throughput_alert.unwrap(),
    memory_alert.unwrap()
  ]
  
  let alert_groups = group_alerts(all_alerts)
  assert_eq(Map::size(alert_groups), 2)  // Critical和Warning
  
  let critical_alerts = match Map::get(alert_groups, "Critical") {
    Some(alerts) => alerts
    None => []
  }
  assert_eq(critical_alerts.length(), 2)  // error_rate和memory_usage
  
  let warning_alerts = match Map::get(alert_groups, "Warning") {
    Some(alerts) => alerts
    None => []
  }
  assert_eq(warning_alerts.length(), 2)  // latency和throughput
}

// 测试7: 遥测数据压缩和传输
test "遥测数据压缩和传输优化" {
  // 定义压缩算法类型
  enum CompressionType {
    None
    Gzip
    Snappy
    LZ4
    Zstd
  }
  
  // 定义传输协议
  enum TransferProtocol {
    HTTP
    HTTPS
    gRPC
    TCP
    UDP
  }
  
  // 定义数据批次
  type DataBatch = {
    id: String,
    data: Array[String],  // 简化为字符串数组，实际应为二进制数据
    original_size: Int,
    compressed_size: Option[Int],
    compression_type: CompressionType,
    timestamp: Int
  }
  
  // 定义传输结果
  type TransferResult = {
    batch_id: String,
    success: Bool,
    duration_ms: Int,
    bytes_sent: Int,
    error_message: Option[String]
  }
  
  // 定义传输配置
  type TransferConfig = {
    protocol: TransferProtocol,
    compression: CompressionType,
    batch_size: Int,
    max_retries: Int,
    timeout_ms: Int,
    endpoint: String
  }
  
  // 创建测试数据
  let create_test_data = fn(count: Int) {
    let mut data = []
    for i in 1..=count {
      data = data.push("telemetry_data_point_" + i.to_string() + "_with_additional_metadata_and_attributes")
    }
    data
  }
  
  // 模拟压缩函数
  let compress_data = fn(data: Array[String], compression_type: CompressionType) {
    let original_size = data.reduce(fn(acc, s) { acc + s.length() }, 0)
    
    let (compressed_size, compression_ratio) = match compression_type {
      CompressionType::None => (original_size, 1.0)
      CompressionType::Gzip => (original_size / 3, 0.33)  // 假设gzip压缩比为3:1
      CompressionType::Snappy => (original_size / 2, 0.5)  // 假设snappy压缩比为2:1
      CompressionType::LZ4 => (original_size / 2, 0.5)    // 假设LZ4压缩比为2:1
      CompressionType::Zstd => (original_size / 4, 0.25)   // 假设zstd压缩比为4:1
    }
    
    { compressed_data: data, compressed_size, compression_ratio }
  }
  
  // 创建数据批次
  let create_batch = fn(data: Array[String], compression_type: CompressionType) {
    let original_size = data.reduce(fn(acc, s) { acc + s.length() }, 0)
    let compression_result = compress_data(data, compression_type)
    
    {
      id: "batch_" + Time::now().to_string(),
      data: compression_result.compressed_data,
      original_size,
      compressed_size: Some(compression_result.compressed_size),
      compression_type,
      timestamp: Time::now()
    }
  }
  
  // 模拟传输函数
  let transfer_batch = fn(batch: DataBatch, config: TransferConfig) {
    let batch_size = match batch.compressed_size {
      Some(size) => size
      None => batch.original_size
    }
    
    // 模拟不同协议的传输时间
    let base_duration = match config.protocol {
      TransferProtocol::HTTP => 100
      TransferProtocol::HTTPS => 120
      TransferProtocol::gRPC => 80
      TransferProtocol::TCP => 60
      TransferProtocol::UDP => 40
    }
    
    // 根据数据大小调整传输时间
    let size_factor = batch_size / 1000
    let duration = base_duration + size_factor
    
    // 模拟传输成功率（基于协议类型）
    let success_rate = match config.protocol {
      TransferProtocol::HTTP => 0.95
      TransferProtocol::HTTPS => 0.98
      TransferProtocol::gRPC => 0.99
      TransferProtocol::TCP => 0.97
      TransferProtocol::UDP => 0.90
    }
    
    let random_value = (Time::now() % 100) as Float / 100.0
    let success = random_value < success_rate
    
    {
      batch_id: batch.id,
      success,
      duration_ms: duration,
      bytes_sent: batch_size,
      error_message: if success { None } else { Some("Network timeout") }
    }
  }
  
  // 批量传输函数
  let transfer_batches = fn(batches: Array[DataBatch], config: TransferConfig) {
    let mut results = []
    let mut retries = 0
    
    for batch in batches {
      let mut result = transfer_batch(batch, config)
      
      // 重试逻辑
      while not(result.success) and retries < config.max_retries {
        retries = retries + 1
        result = transfer_batch(batch, config)
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // 计算传输统计
  let calculate_transfer_stats = fn(results: Array[TransferResult]) {
    let total_batches = results.length()
    let successful_batches = results.filter(fn(r) { r.success }).length()
    let total_bytes = results.reduce(fn(acc, r) { acc + r.bytes_sent }, 0)
    let total_duration = results.reduce(fn(acc, r) { acc + r.duration_ms }, 0)
    let avg_duration = if total_batches > 0 { total_duration / total_batches } else { 0 }
    
    {
      total_batches,
      successful_batches,
      failed_batches: total_batches - successful_batches,
      success_rate: if total_batches > 0 { (successful_batches as Float) / (total_batches as Float) } else { 0.0 },
      total_bytes,
      avg_duration_ms: avg_duration
    }
  }
  
  // 测试数据压缩
  let test_data = create_test_data(100)
  let uncompressed_batch = create_batch(test_data, CompressionType::None)
  let gzip_batch = create_batch(test_data, CompressionType::Gzip)
  let snappy_batch = create_batch(test_data, CompressionType::Snappy)
  let zstd_batch = create_batch(test_data, CompressionType::Zstd)
  
  assert_eq(uncompressed_batch.original_size, uncompressed_batch.compressed_size.unwrap())
  assert_eq(gzip_batch.original_size, 3 * gzip_batch.compressed_size.unwrap())  // 3:1压缩比
  assert_eq(snappy_batch.original_size, 2 * snappy_batch.compressed_size.unwrap())  // 2:1压缩比
  assert_eq(zstd_batch.original_size, 4 * zstd_batch.compressed_size.unwrap())  // 4:1压缩比
  
  // 测试不同协议的传输
  let http_config = {
    protocol: TransferProtocol::HTTP,
    compression: CompressionType::Gzip,
    batch_size: 100,
    max_retries: 3,
    timeout_ms: 5000,
    endpoint: "http://telemetry-collector:8080/api/v1/telemetry"
  }
  
  let https_config = {
    protocol: TransferProtocol::HTTPS,
    compression: CompressionType::Gzip,
    batch_size: 100,
    max_retries: 3,
    timeout_ms: 5000,
    endpoint: "https://telemetry-collector:8443/api/v1/telemetry"
  }
  
  let grpc_config = {
    protocol: TransferProtocol::gRPC,
    compression: CompressionType::Snappy,
    batch_size: 100,
    max_retries: 3,
    timeout_ms: 5000,
    endpoint: "grpc://telemetry-collector:9090"
  }
  
  let batches = [gzip_batch, snappy_batch, zstd_batch]
  
  let http_results = transfer_batches(batches, http_config)
  let https_results = transfer_batches(batches, https_config)
  let grpc_results = transfer_batches(batches, grpc_config)
  
  // 计算传输统计
  let http_stats = calculate_transfer_stats(http_results)
  let https_stats = calculate_transfer_stats(https_results)
  let grpc_stats = calculate_transfer_stats(grpc_results)
  
  // 验证传输统计
  assert_eq(http_stats.total_batches, 3)
  assert_eq(https_stats.total_batches, 3)
  assert_eq(grpc_stats.total_batches, 3)
  
  // HTTPS应该有更高的成功率
  assert_true(https_stats.success_rate >= http_stats.success_rate)
  assert_true(grpc_stats.success_rate >= http_stats.success_rate)
  
  // gRPC应该有更低的平均延迟
  assert_true(grpc_stats.avg_duration_ms <= http_stats.avg_duration_ms)
  
  // 测试压缩效率比较
  let compare_compression = fn(batches: Array[DataBatch]) {
    let mut comparison = []
    
    for batch in batches {
      let compression_ratio = match batch.compressed_size {
        Some(size) => (batch.original_size as Float) / (size as Float)
        None => 1.0
      }
      
      let compression_type_str = match batch.compression_type {
        CompressionType::None => "None"
        CompressionType::Gzip => "Gzip"
        CompressionType::Snappy => "Snappy"
        CompressionType::LZ4 => "LZ4"
        CompressionType::Zstd => "Zstd"
      }
      
      comparison = comparison.push({
        type: compression_type_str,
        original_size: batch.original_size,
        compressed_size: batch.compressed_size.unwrap(),
        compression_ratio
      })
    }
    
    comparison.sort(fn(a, b) { if a.compression_ratio > b.compression_ratio { -1 } else if a.compression_ratio < b.compression_ratio { 1 } else { 0 } })
  }
  
  let compression_comparison = compare_compression([uncompressed_batch, gzip_batch, snappy_batch, zstd_batch])
  
  // Zstd应该有最高的压缩比
  assert_eq(compression_comparison[0].type, "Zstd")
  assert_true(compression_comparison[0].compression_ratio >= compression_comparison[1].compression_ratio)
  
  // None应该有最低的压缩比（1.0）
  assert_eq(compression_comparison[3].type, "None")
  assert_eq(compression_comparison[3].compression_ratio, 1.0)
  
  // 测试批量大小优化
  let find_optimal_batch_size = fn(base_data: Array[String], config: TransferConfig, test_sizes: Array[Int]) {
    let results = []
    
    for size in test_sizes {
      let batch_data = base_data.slice(0, size)
      let batch = create_batch(batch_data, config.compression)
      let transfer_result = transfer_batch(batch, config)
      
      let throughput = if transfer_result.success {
        (transfer_result.bytes_sent as Float) / ((transfer_result.duration_ms as Float) / 1000.0)
      } else {
        0.0
      }
      
      results = results.push({
        batch_size: size,
        throughput,
        success: transfer_result.success,
        duration_ms: transfer_result.duration_ms
      })
    }
    
    results
  }
  
  let batch_sizes = [50, 100, 200, 500, 1000]
  let large_test_data = create_test_data(1000)
  let batch_size_results = find_optimal_batch_size(large_test_data, grpc_config, batch_sizes)
  
  // 验证结果
  assert_eq(batch_size_results.length(), 5)
  
  // 找到最优批量大小（最高吞吐量）
  let optimal_result = batch_size_results.reduce(fn(best, current) {
    if current.throughput > best.throughput { current } else { best }
  }, batch_size_results[0])
  
  assert_true(optimal_result.throughput > 0.0)
  assert_true(optimal_result.success)
  
  // 测试自适应压缩选择
  let adaptive_compression = fn(data: Array[String], network_conditions: { bandwidth: Int, latency: Int }) {
    let data_size = data.reduce(fn(acc, s) { acc + s.length() }, 0)
    
    // 根据网络条件选择压缩算法
    if network_conditions.bandwidth < 1000 and network_conditions.latency > 100 {
      // 低带宽高延迟：使用最高压缩比
      CompressionType::Zstd
    } else if network_conditions.bandwidth < 5000 {
      // 中等带宽：使用平衡压缩
      CompressionType::Gzip
    } else if network_conditions.latency > 50 {
      // 高延迟：使用快速压缩
      CompressionType::Snappy
    } else {
      // 良好网络：不压缩或轻量压缩
      CompressionType::LZ4
    }
  }
  
  let slow_network = { bandwidth: 500, latency: 200 }
  let fast_network = { bandwidth: 10000, latency: 10 }
  
  let slow_compression = adaptive_compression(test_data, slow_network)
  let fast_compression = adaptive_compression(test_data, fast_network)
  
  assert_eq(slow_compression, CompressionType::Zstd)
  assert_eq(fast_compression, CompressionType::LZ4)
}

// 测试8: 自定义指标和事件
test "自定义指标和事件处理" {
  // 定义指标类型
  enum MetricValueType {
    Int
    Float
    String
    Boolean
  }
  
  // 定义事件类型
  enum EventType {
    UserAction
    SystemEvent
    BusinessEvent
    SecurityEvent
  }
  
  // 定义自定义指标
  type CustomMetric = {
    name: String,
    value_type: MetricValueType,
    value: String,  // 使用字符串存储，根据类型转换
    timestamp: Int,
    tags: Array[(String, String)],
    unit: Option[String]
  }
  
  // 定义自定义事件
  type CustomEvent = {
    name: String,
    event_type: EventType,
    timestamp: Int,
    attributes: Array[(String, String)],
    user_id: Option[String],
    session_id: Option[String]
  }
  
  // 定义指标注册表
  type MetricRegistry = {
    metrics: Map[String, CustomMetric],
    events: Array[CustomEvent],
    aggregations: Map[String, MetricAggregation]
  }
  
  // 定义指标聚合
  type MetricAggregation = {
    metric_name: String,
    aggregation_type: String,  // "sum", "avg", "min", "max", "count"
    value: Float,
    timestamp: Int
  }
  
  // 创建测试自定义指标
  let custom_metrics = [
    {
      name: "user_cart_value",
      value_type: MetricValueType::Float,
      value: "99.99",
      timestamp: 1640995200,
      tags: [("currency", "USD"), ("user_tier", "premium")],
      unit: Some("dollars")
    },
    {
      name: "user_cart_value",
      value_type: MetricValueType::Float,
      value: "49.99",
      timestamp: 1640995260,
      tags: [("currency", "USD"), ("user_tier", "standard")],
      unit: Some("dollars")
    },
    {
      name: "api_calls_count",
      value_type: MetricValueType::Int,
      value: "42",
      timestamp: 1640995320,
      tags: [("endpoint", "/api/users"), ("method", "GET")],
      unit: Some("calls")
    },
    {
      name: "feature_flag_enabled",
      value_type: MetricValueType::Boolean,
      value: "true",
      timestamp: 1640995380,
      tags: [("flag_name", "new_dashboard"), ("environment", "production")],
      unit: None
    },
    {
      name: "last_error_message",
      value_type: MetricValueType::String,
      value: "Database connection timeout",
      timestamp: 1640995440,
      tags: [("service", "payment"), ("error_code", "DB_TIMEOUT")],
      unit: None
    }
  ]
  
  // 创建测试自定义事件
  let custom_events = [
    {
      name: "user_login",
      event_type: EventType::UserAction,
      timestamp: 1640995200,
      attributes: [("method", "email"), ("success", "true")],
      user_id: Some("user_123"),
      session_id: Some("session_456")
    },
    {
      name: "payment_completed",
      event_type: EventType::BusinessEvent,
      timestamp: 1640995260,
      attributes: [("amount", "99.99"), ("payment_method", "credit_card")],
      user_id: Some("user_123"),
      session_id: Some("session_456")
    },
    {
      name: "server_restart",
      event_type: EventType::SystemEvent,
      timestamp: 1640995320,
      attributes: [("reason", "maintenance"), ("duration_seconds", "120")],
      user_id: None,
      session_id: None
    },
    {
      name: "security_alert",
      event_type: EventType::SecurityEvent,
      timestamp: 1640995380,
      attributes: [("alert_type", "brute_force"), ("source_ip", "192.168.1.100")],
      user_id: None,
      session_id: None
    },
    {
      name: "feature_used",
      event_type: EventType::UserAction,
      timestamp: 1640995440,
      attributes: [("feature_name", "advanced_search"), ("usage_time_ms", "500")],
      user_id: Some("user_789"),
      session_id: Some("session_123")
    }
  ]
  
  // 值类型转换为字符串
  let value_to_string = fn(value_type: MetricValueType, value: String) {
    match value_type {
      MetricValueType::Int => value
      MetricValueType::Float => value
      MetricValueType::String => "\"" + value + "\""
      MetricValueType::Boolean => value
    }
  }
  
  // 从字符串解析值
  let parse_value = fn(value_type: MetricValueType, value: String) {
    match value_type {
      MetricValueType::Int => value.to_int()
      MetricValueType::Float => value.to_float()
      MetricValueType::String => value
      MetricValueType::Boolean => value == "true"
    }
  }
  
  // 注册指标
  let register_metric = fn(registry: MetricRegistry, metric: CustomMetric) {
    let updated_metrics = Map::insert(registry.metrics, metric.name, metric)
    { registry | metrics: updated_metrics }
  }
  
  // 记录事件
  let record_event = fn(registry: MetricRegistry, event: CustomEvent) {
    { registry | events: registry.events.push(event) }
  }
  
  // 按标签过滤指标
  let filter_metrics_by_tags = fn(metrics: Array[CustomMetric], tag_filters: Array[(String, String)]) {
    metrics.filter(fn(metric) {
      for (tag_key, tag_value) in tag_filters {
        match metric.tags.find(fn(t) { t.0 == tag_key }) {
          Some((_, value)) => {
            if value != tag_value {
              return false
            }
          }
          None => return false
        }
      }
      true
    })
  }
  
  // 按事件类型过滤事件
  let filter_events_by_type = fn(events: Array[CustomEvent], event_types: Array[EventType]) {
    events.filter(fn(event) { event_types.contains(event.event_type) })
  }
  
  // 按用户ID过滤事件
  let filter_events_by_user = fn(events: Array[CustomEvent], user_id: String) {
    events.filter(fn(event) { 
      match event.user_id {
        Some(id) => id == user_id
        None => false
      }
    })
  }
  
  // 计算指标聚合
  let calculate_metric_aggregation = fn(metrics: Array[CustomMetric], aggregation_type: String) {
    if metrics.length() == 0 {
      return []
    }
    
    let groups = Map::empty()
    
    for metric in metrics {
      let group_metrics = match Map::get(groups, metric.name) {
        Some(m) => m
        None => []
      }
      
      let _ = Map::insert(groups, metric.name, group_metrics.push(metric))
    }
    
    let aggregations = []
    for (metric_name, grouped_metrics) in groups {
      let values = grouped_metrics.map(fn(m) { parse_value(m.value_type, m.value) })
      
      let aggregated_value = match aggregation_type {
        "sum" => {
          let mut sum = 0.0
          for v in values {
            match v {
              Int(i) => sum = sum + (i as Float)
              Float(f) => sum = sum + f
              _ => {}
            }
          }
          sum
        }
        "avg" => {
          let mut sum = 0.0
          let mut count = 0.0
          for v in values {
            match v {
              Int(i) => { sum = sum + (i as Float); count = count + 1.0 }
              Float(f) => { sum = sum + f; count = count + 1.0 }
              _ => {}
            }
          }
          if count > 0.0 { sum / count } else { 0.0 }
        }
        "min" => {
          let mut min = Float::infinity()
          for v in values {
            match v {
              Int(i) => {
                let f = i as Float
                if f < min { min = f }
              }
              Float(f) => {
                if f < min { min = f }
              }
              _ => {}
            }
          }
          if min == Float::infinity() { 0.0 } else { min }
        }
        "max" => {
          let mut max = 0.0
          for v in values {
            match v {
              Int(i) => {
                let f = i as Float
                if f > max { max = f }
              }
              Float(f) => {
                if f > max { max = f }
              }
              _ => {}
            }
          }
          max
        }
        "count" => values.length() as Float
        _ => 0.0
      }
      
      aggregations = aggregations.push({
        metric_name,
        aggregation_type,
        value: aggregated_value,
        timestamp: Time::now()
      })
    }
    
    aggregations
  }
  
  // 创建初始注册表
  let initial_registry = {
    metrics: Map::empty(),
    events: [],
    aggregations: Map::empty()
  }
  
  // 注册所有指标
  let registry_with_metrics = custom_metrics.reduce(fn(registry, metric) {
    register_metric(registry, metric)
  }, initial_registry)
  
  // 记录所有事件
  let full_registry = custom_events.reduce(fn(registry, event) {
    record_event(registry, event)
  }, registry_with_metrics)
  
  // 测试指标注册
  assert_eq(Map::size(full_registry.metrics), 4)  // 4个不同的指标名称
  
  let cart_value_metric = match Map::get(full_registry.metrics, "user_cart_value") {
    Some(metric) => metric
    None => { name: "", value_type: MetricValueType::Float, value: "", timestamp: 0, tags: [], unit: None }
  }
  assert_eq(cart_value_metric.name, "user_cart_value")
  assert_eq(cart_value_metric.value_type, MetricValueType::Float)
  assert_eq(cart_value_metric.unit, Some("dollars"))
  
  // 测试事件记录
  assert_eq(full_registry.events.length(), 5)
  
  let user_login_event = full_registry.events.filter(fn(e) { e.name == "user_login" })[0]
  assert_eq(user_login_event.event_type, EventType::UserAction)
  assert_eq(user_login_event.user_id, Some("user_123"))
  assert_eq(user_login_event.session_id, Some("session_456"))
  
  // 测试按标签过滤指标
  let premium_cart_metrics = filter_metrics_by_tags(custom_metrics, [("user_tier", "premium")])
  assert_eq(premium_cart_metrics.length(), 1)
  assert_eq(premium_cart_metrics[0].value, "99.99")
  
  let get_api_metrics = filter_metrics_by_tags(custom_metrics, [("endpoint", "/api/users"), ("method", "GET")])
  assert_eq(get_api_metrics.length(), 1)
  assert_eq(get_api_metrics[0].name, "api_calls_count")
  
  // 测试按事件类型过滤
  let user_action_events = filter_events_by_type(custom_events, [EventType::UserAction])
  assert_eq(user_action_events.length(), 2)
  assert_true(user_action_events.all(fn(e) { e.event_type == EventType::UserAction }))
  
  let system_events = filter_events_by_type(custom_events, [EventType::SystemEvent])
  assert_eq(system_events.length(), 1)
  assert_eq(system_events[0].name, "server_restart")
  
  // 测试按用户过滤事件
  let user_123_events = filter_events_by_user(custom_events, "user_123")
  assert_eq(user_123_events.length(), 2)
  assert_true(user_123_events.all(fn(e) { e.user_id == Some("user_123") }))
  
  // 测试指标聚合
  let cart_value_aggregations = calculate_metric_aggregation(
    custom_metrics.filter(fn(m) { m.name == "user_cart_value" }),
    "avg"
  )
  assert_eq(cart_value_aggregations.length(), 1)
  assert_eq(cart_value_aggregations[0].metric_name, "user_cart_value")
  assert_eq(cart_value_aggregations[0].aggregation_type, "avg")
  assert_eq(cart_value_aggregations[0].value, 74.99)  // (99.99 + 49.99) / 2
  
  let sum_aggregations = calculate_metric_aggregation(
    custom_metrics.filter(fn(m) { m.name == "api_calls_count" }),
    "sum"
  )
  assert_eq(sum_aggregations.length(), 1)
  assert_eq(sum_aggregations[0].value, 42.0)
  
  // 测试事件序列分析
  let analyze_user_journey = fn(events: Array[CustomEvent], user_id: String) {
    let user_events = filter_events_by_user(events, user_id)
    let sorted_events = user_events.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    
    let journey_steps = sorted_events.map(fn(event) {
      {
        step_name: event.name,
        timestamp: event.timestamp,
        attributes: event.attributes,
        event_type: event.event_type
      }
    })
    
    journey_steps
  }
  
  let user_123_journey = analyze_user_journey(custom_events, "user_123")
  assert_eq(user_123_journey.length(), 2)
  assert_eq(user_123_journey[0].step_name, "user_login")
  assert_eq(user_123_journey[1].step_name, "payment_completed")
  assert_true(user_123_journey[0].timestamp < user_123_journey[1].timestamp)
  
  // 测试事件漏斗分析
  let analyze_conversion_funnel = fn(events: Array[CustomEvent], funnel_steps: Array[String]) {
    let step_counts = Map::empty()
    
    for step in funnel_steps {
      let step_events = events.filter(fn(e) { e.name == step })
      let unique_users = Map::empty()
      
      for event in step_events {
        match event.user_id {
          Some(user_id) => {
            let _ = Map::insert(unique_users, user_id, true)
          }
          None => {}
        }
      }
      
      let _ = Map::insert(step_counts, step, Map::size(unique_users))
    }
    
    step_counts
  }
  
  let funnel_steps = ["user_login", "payment_completed", "feature_used"]
  let funnel_analysis = analyze_conversion_funnel(custom_events, funnel_steps)
  
  assert_eq(match Map::get(funnel_analysis, "user_login") { Some(count) => count | None => 0 }, 1)
  assert_eq(match Map::get(funnel_analysis, "payment_completed") { Some(count) => count | None => 0 }, 1)
  assert_eq(match Map::get(funnel_analysis, "feature_used") { Some(count) => count | None => 0 }, 1)
  
  // 测试指标趋势分析
  let analyze_metric_trend = fn(metrics: Array[CustomMetric], time_window_seconds: Int) {
    let sorted_metrics = metrics.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    let current_time = Time::now()
    
    let recent_metrics = sorted_metrics.filter(fn(m) { 
      current_time - m.timestamp <= time_window_seconds 
    })
    
    if recent_metrics.length() < 2 {
      return { trend: "insufficient_data", change_percent: 0.0 }
    }
    
    let values = recent_metrics.map(fn(m) { parse_value(m.value_type, m.value) })
    let numeric_values = values.filter(fn(v) { 
      match v {
        Int(_) or Float(_) => true
        _ => false
      }
    })
    
    if numeric_values.length() < 2 {
      return { trend: "insufficient_data", change_percent: 0.0 }
    }
    
    let first_value = match numeric_values[0] {
      Int(i) => i as Float
      Float(f) => f
      _ => 0.0
    }
    
    let last_value = match numeric_values[numeric_values.length() - 1] {
      Int(i) => i as Float
      Float(f) => f
      _ => 0.0
    }
    
    let change_percent = if first_value != 0.0 {
      ((last_value - first_value) / first_value) * 100.0
    } else {
      0.0
    }
    
    let trend = if change_percent > 5.0 {
      "increasing"
    } else if change_percent < -5.0 {
      "decreasing"
    } else {
      "stable"
    }
    
    { trend, change_percent }
  }
  
  let cart_value_trend = analyze_metric_trend(
    custom_metrics.filter(fn(m) { m.name == "user_cart_value" }),
    3600  // 1小时窗口
  )
  
  assert_eq(cart_value_trend.trend, "decreasing")
  assert_true(cart_value_trend.change_percent < 0.0)
}

// 测试9: 遥测数据存储和查询
test "遥测数据存储和查询系统" {
  // 定义存储后端类型
  enum StorageBackend {
    MemoryStore
    FileStore
    Database
    TimeSeriesDB
  }
  
  // 定义查询操作符
  enum QueryOperator {
    Equals
    NotEquals
    GreaterThan
    LessThan
    Contains
    In
    Between
  }
  
  // 定义查询条件
  type QueryCondition = {
    field: String,
    operator: QueryOperator,
    value: String
  }
  
  // 定义查询
  type Query = {
    conditions: Array[QueryCondition],
    time_range: Option[(Int, Int)],  // (start_time, end_time)
    limit: Option[Int],
    order_by: Option[String],
    order_direction: Option[String]  // "asc" or "desc"
  }
  
  // 定义遥测记录
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    trace_id: String,
    span_id: String,
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status_code: Int,
    tags: Array[(String, String)],
    metrics: Array[(String, Float)],
    logs: Array[String]
  }
  
  // 定义查询结果
  type QueryResult = {
    records: Array[TelemetryRecord],
    total_count: Int,
    execution_time_ms: Int,
    has_more: Bool
  }
  
  // 创建测试遥测记录
  let telemetry_records = [
    {
      id: "record_1",
      timestamp: 1640995200,
      trace_id: "trace_123",
      span_id: "span_456",
      service_name: "auth-service",
      operation_name: "login",
      duration_ms: 120,
      status_code: 200,
      tags: [("user_id", "12345"), ("region", "us-west")],
      metrics: [("cpu_usage", 25.5), ("memory_usage", 45.2)],
      logs: ["User login successful", "Session created"]
    },
    {
      id: "record_2",
      timestamp: 1640995260,
      trace_id: "trace_123",
      span_id: "span_789",
      service_name: "payment-service",
      operation_name: "process_payment",
      duration_ms: 350,
      status_code: 200,
      tags: [("user_id", "12345"), ("amount", "99.99")],
      metrics: [("cpu_usage", 35.2), ("memory_usage", 55.8)],
      logs: ["Payment processed", "Transaction ID: txn_789"]
    },
    {
      id: "record_3",
      timestamp: 1640995320,
      trace_id: "trace_456",
      span_id: "span_234",
      service_name: "inventory-service",
      operation_name: "check_stock",
      duration_ms: 2000,
      status_code: 500,
      tags: [("product_id", "prod_789"), ("warehouse", "wh_01")],
      metrics: [("cpu_usage", 85.3), ("memory_usage", 75.6)],
      logs: ["Database connection timeout", "Retry attempt failed"]
    },
    {
      id: "record_4",
      timestamp: 1640995380,
      trace_id: "trace_789",
      span_id: "span_345",
      service_name: "notification-service",
      operation_name: "send_email",
      duration_ms: 500,
      status_code: 200,
      tags: [("template", "welcome"), ("recipient", "user@example.com")],
      metrics: [("cpu_usage", 15.8), ("memory_usage", 35.2)],
      logs: ["Email sent successfully"]
    },
    {
      id: "record_5",
      timestamp: 1640995440,
      trace_id: "trace_123",
      span_id: "span_567",
      service_name: "user-service",
      operation_name: "get_profile",
      duration_ms: 80,
      status_code: 200,
      tags: [("user_id", "12345"), ("fields", "name,email")],
      metrics: [("cpu_usage", 12.3), ("memory_usage", 28.9)],
      logs: ["Profile retrieved from cache"]
    }
  ]
  
  // 模拟存储实现
  type TelemetryStore = {
    backend: StorageBackend,
    records: Array[TelemetryRecord]
  }
  
  // 创建内存存储
  let create_memory_store = fn(records: Array[TelemetryRecord]) {
    {
      backend: StorageBackend::MemoryStore,
      records
    }
  }
  
  // 查询条件评估
  let evaluate_condition = fn(record: TelemetryRecord, condition: QueryCondition) {
    let get_field_value = fn(record: TelemetryRecord, field: String) {
      match field {
        "service_name" => record.service_name
        "operation_name" => record.operation_name
        "trace_id" => record.trace_id
        "span_id" => record.span_id
        "status_code" => record.status_code.to_string()
        "duration_ms" => record.duration_ms.to_string()
        _ => {
          // 查找标签
          match record.tags.find(fn(t) { t.0 == field }) {
            Some((_, value)) => value
            None => ""
          }
        }
      }
    }
    
    let field_value = get_field_value(record, condition.field)
    
    match condition.operator {
      QueryOperator::Equals => field_value == condition.value
      QueryOperator::NotEquals => field_value != condition.value
      QueryOperator::GreaterThan => {
        let field_num = field_value.to_int()
        let value_num = condition.value.to_int()
        field_num > value_num
      }
      QueryOperator::LessThan => {
        let field_num = field_value.to_int()
        let value_num = condition.value.to_int()
        field_num < value_num
      }
      QueryOperator::Contains => field_value.contains(condition.value)
      QueryOperator::In => {
        let values = condition.value.split(",")
        values.contains(field_value)
      }
      QueryOperator::Between => {
        let range = condition.value.split(",")
        if range.length() == 2 {
          let field_num = field_value.to_int()
          let min = range[0].to_int()
          let max = range[1].to_int()
          field_num >= min and field_num <= max
        } else {
          false
        }
      }
    }
  }
  
  // 执行查询
  let execute_query = fn(store: TelemetryStore, query: Query) {
    let start_time = Time::now()
    
    // 应用时间范围过滤
    let time_filtered = match query.time_range {
      Some((start, end)) => {
        store.records.filter(fn(r) { r.timestamp >= start and r.timestamp <= end })
      }
      None => store.records
    }
    
    // 应用条件过滤
    let condition_filtered = time_filtered.filter(fn(record) {
      query.conditions.all(fn(condition) { evaluate_condition(record, condition) })
    })
    
    // 排序
    let sorted = match query.order_by {
      Some(order_field) => {
        let get_sort_value = fn(record: TelemetryRecord, field: String) {
          match field {
            "timestamp" => record.timestamp
            "duration_ms" => record.duration_ms
            "status_code" => record.status_code
            "service_name" => record.service_name.length()  // 简化：使用长度作为排序依据
            _ => 0
          }
        }
        
        let direction = match query.order_direction {
          Some("desc") => -1
          _ => 1
        }
        
        condition_filtered.sort(fn(a, b) {
          let value_a = get_sort_value(a, order_field)
          let value_b = get_sort_value(b, order_field)
          
          if value_a < value_b { -1 * direction } 
          else if value_a > value_b { 1 * direction } 
          else { 0 }
        })
      }
      None => condition_filtered
    }
    
    // 应用限制
    let limited = match query.limit {
      Some(limit) => sorted.slice(0, limit)
      None => sorted
    }
    
    let end_time = Time::now()
    let execution_time = end_time - start_time
    
    {
      records: limited,
      total_count: condition_filtered.length(),
      execution_time_ms: execution_time,
      has_more: limited.length() < condition_filtered.length()
    }
  }
  
  // 创建存储实例
  let store = create_memory_store(telemetry_records)
  
  // 测试简单查询
  let service_query = {
    conditions: [{ field: "service_name", operator: QueryOperator::Equals, value: "auth-service" }],
    time_range: None,
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let service_result = execute_query(store, service_query)
  assert_eq(service_result.records.length(), 1)
  assert_eq(service_result.records[0].service_name, "auth-service")
  assert_eq(service_result.total_count, 1)
  
  // 测试多条件查询
  let multi_condition_query = {
    conditions: [
      { field: "trace_id", operator: QueryOperator::Equals, value: "trace_123" },
      { field: "status_code", operator: QueryOperator::Equals, value: "200" }
    ],
    time_range: None,
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let multi_condition_result = execute_query(store, multi_condition_query)
  assert_eq(multi_condition_result.records.length(), 3)
  assert_true(multi_condition_result.records.all(fn(r) { r.trace_id == "trace_123" and r.status_code == 200 }))
  
  // 测试时间范围查询
  let time_range_query = {
    conditions: [],
    time_range: Some((1640995200, 1640995320)),
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let time_range_result = execute_query(store, time_range_query)
  assert_eq(time_range_result.records.length(), 3)
  assert_true(time_range_result.records.all(fn(r) { r.timestamp >= 1640995200 and r.timestamp <= 1640995320 }))
  
  // 测试排序查询
  let order_query = {
    conditions: [],
    time_range: None,
    limit: None,
    order_by: Some("duration_ms"),
    order_direction: Some("desc")
  }
  
  let order_result = execute_query(store, order_query)
  assert_eq(order_result.records.length(), 5)
  assert_eq(order_result.records[0].duration_ms, 2000)  // 最长的持续时间
  assert_eq(order_result.records[4].duration_ms, 80)    // 最短的持续时间
  
  // 测试限制查询
  let limit_query = {
    conditions: [],
    time_range: None,
    limit: Some(3),
    order_by: Some("timestamp"),
    order_direction: Some("asc")
  }
  
  let limit_result = execute_query(store, limit_query)
  assert_eq(limit_result.records.length(), 3)
  assert_eq(limit_result.total_count, 5)
  assert_true(limit_result.has_more)
  
  // 测试包含查询
  let contains_query = {
    conditions: [{ field: "operation_name", operator: QueryOperator::Contains, value: "email" }],
    time_range: None,
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let contains_result = execute_query(store, contains_query)
  assert_eq(contains_result.records.length(), 1)
  assert_eq(contains_result.records[0].operation_name, "send_email")
  
  // 测试IN查询
  let in_query = {
    conditions: [{ field: "service_name", operator: QueryOperator::In, value: "auth-service,user-service" }],
    time_range: None,
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let in_result = execute_query(store, in_query)
  assert_eq(in_result.records.length(), 2)
  assert_true(in_result.records.all(fn(r) { r.service_name == "auth-service" or r.service_name == "user-service" }))
  
  // 测试Between查询
  let between_query = {
    conditions: [{ field: "duration_ms", operator: QueryOperator::Between, value: "100,500" }],
    time_range: None,
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let between_result = execute_query(store, between_query)
  assert_eq(between_result.records.length(), 3)
  assert_true(between_result.records.all(fn(r) { r.duration_ms >= 100 and r.duration_ms <= 500 }))
  
  // 测试标签查询
  let tag_query = {
    conditions: [{ field: "user_id", operator: QueryOperator::Equals, value: "12345" }],
    time_range: None,
    limit: None,
    order_by: None,
    order_direction: None
  }
  
  let tag_result = execute_query(store, tag_query)
  assert_eq(tag_result.records.length(), 3)
  assert_true(tag_result.records.all(fn(r) { r.tags.any(fn(t) { t.0 == "user_id" and t.1 == "12345" }) }))
  
  // 测试复合查询
  let complex_query = {
    conditions: [
      { field: "status_code", operator: QueryOperator::Equals, value: "200" },
      { field: "duration_ms", operator: QueryOperator::LessThan, value: "500" }
    ],
    time_range: Some((1640995200, 1640995440)),
    limit: Some(10),
    order_by: Some("timestamp"),
    order_direction: Some("desc")
  }
  
  let complex_result = execute_query(store, complex_query)
  assert_eq(complex_result.records.length(), 4)
  assert_true(complex_result.records.all(fn(r) { r.status_code == 200 and r.duration_ms < 500 }))
  assert_false(complex_result.has_more)
  
  // 测试聚合查询
  let aggregate_by_field = fn(records: Array[TelemetryRecord], field: String) {
    let groups = Map::empty()
    
    for record in records {
      let field_value = match field {
        "service_name" => record.service_name
        "operation_name" => record.operation_name
        "status_code" => record.status_code.to_string()
        _ => {
          match record.tags.find(fn(t) { t.0 == field }) {
            Some((_, value)) => value
            None => "unknown"
          }
        }
      }
      
      let group_records = match Map::get(groups, field_value) {
        Some(recs) => recs
        None => []
      }
      
      let _ = Map::insert(groups, field_value, group_records.push(record))
    }
    
    groups
  }
  
  let service_groups = aggregate_by_field(telemetry_records, "service_name")
  assert_eq(Map::size(service_groups), 4)  // 4个不同的服务
  
  let auth_group = match Map::get(service_groups, "auth-service") {
    Some(group) => group
    None => []
  }
  assert_eq(auth_group.length(), 1)
  
  let payment_group = match Map::get(service_groups, "payment-service") {
    Some(group) => group
    None => []
  }
  assert_eq(payment_group.length(), 1)
  
  // 计算聚合统计
  let calculate_aggregation_stats = fn(records: Array[TelemetryRecord]) {
    if records.length() == 0 {
      return { count: 0, avg_duration: 0.0, error_rate: 0.0 }
    }
    
    let total_duration = records.reduce(fn(acc, r) { acc + r.duration_ms }, 0)
    let avg_duration = (total_duration as Float) / (records.length() as Float)
    
    let error_count = records.filter(fn(r) { r.status_code >= 400 }).length()
    let error_rate = (error_count as Float) / (records.length() as Float)
    
    {
      count: records.length(),
      avg_duration,
      error_rate
    }
  }
  
  let auth_stats = calculate_aggregation_stats(auth_group)
  assert_eq(auth_stats.count, 1)
  assert_eq(auth_stats.avg_duration, 120.0)
  assert_eq(auth_stats.error_rate, 0.0)
  
  // 测试性能指标查询
  let metric_query = fn(store: TelemetryStore, metric_name: String, aggregation: String) {
    let all_records = store.records
    
    // 提取所有指标值
    let metric_values = []
    for record in all_records {
      for (name, value) in record.metrics {
        if name == metric_name {
          metric_values = metric_values.push(value)
        }
      }
    }
    
    if metric_values.length() == 0 {
      return { value: 0.0, count: 0 }
    }
    
    let result = match aggregation {
      "avg" => {
        let sum = metric_values.reduce(fn(acc, v) { acc + v }, 0.0)
        sum / (metric_values.length() as Float)
      }
      "min" => {
        metric_values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, metric_values[0])
      }
      "max" => {
        metric_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, metric_values[0])
      }
      "sum" => {
        metric_values.reduce(fn(acc, v) { acc + v }, 0.0)
      }
      _ => 0.0
    }
    
    { value: result, count: metric_values.length() }
  }
  
  let cpu_avg = metric_query(store, "cpu_usage", "avg")
  assert_eq(cpu_avg.count, 5)
  assert_true(cpu_avg.value > 0.0)
  
  let memory_max = metric_query(store, "memory_usage", "max")
  assert_eq(memory_max.count, 5)
  assert_eq(memory_max.value, 75.6)  // 来自inventory-service记录
}

// 测试10: 跨服务遥测关联
test "跨服务遥测关联分析" {
  // 定义服务依赖类型
  enum DependencyType {
    Synchronous
    Asynchronous
    SharedResource
    MessageQueue
  }
  
  // 定义服务关系
  type ServiceRelation = {
    source_service: String,
    target_service: String,
    dependency_type: DependencyType,
    call_count: Int,
    error_count: Int,
    avg_latency_ms: Float,
    timestamp: Int
  }
  
  // 定义服务拓扑
  type ServiceTopology = {
    services: Array[String],
    relations: Array[ServiceRelation],
    critical_path: Array[String]
  }
  
  // 定义跨服务追踪
  type CrossServiceTrace = {
    trace_id: String,
    services: Array[String],
    spans: Array[CrossServiceSpan],
    total_duration_ms: Int,
    error_count: Int
  }
  
  // 定义跨服务跨度
  type CrossServiceSpan = {
    span_id: String,
    service_name: String,
    operation_name: String,
    parent_span_id: Option[String],
    start_time: Int,
    end_time: Int,
    status_code: Int,
    tags: Array[(String, String)]
  }
  
  // 定义服务依赖图
  type ServiceDependencyGraph = {
    nodes: Map[String, ServiceNode],
    edges: Map[String, Array[DependencyEdge]]
  }
  
  type ServiceNode = {
    name: String,
    inbound_count: Int,
    outbound_count: Int,
    error_rate: Float,
    avg_latency: Float
  }
  
  type DependencyEdge = {
    target: String,
    dependency_type: DependencyType,
    weight: Float,
    error_rate: Float
  }
  
  // 创建测试跨服务追踪数据
  let cross_service_traces = [
    {
      trace_id: "trace_001",
      services: ["frontend", "auth-service", "user-service", "order-service"],
      spans: [
        { span_id: "span_001", service_name: "frontend", operation_name: "http_request", parent_span_id: None, start_time: 1000, end_time: 1200, status_code: 200, tags: [("endpoint", "/checkout")] },
        { span_id: "span_002", service_name: "auth-service", operation_name: "validate_token", parent_span_id: Some("span_001"), start_time: 1050, end_time: 1100, status_code: 200, tags: [] },
        { span_id: "span_003", service_name: "user-service", operation_name: "get_user", parent_span_id: Some("span_001"), start_time: 1100, end_time: 1150, status_code: 200, tags: [] },
        { span_id: "span_004", service_name: "order-service", operation_name: "create_order", parent_span_id: Some("span_001"), start_time: 1150, end_time: 1200, status_code: 200, tags: [] }
      ],
      total_duration_ms: 200,
      error_count: 0
    },
    {
      trace_id: "trace_002",
      services: ["frontend", "payment-service", "inventory-service", "notification-service"],
      spans: [
        { span_id: "span_005", service_name: "frontend", operation_name: "http_request", parent_span_id: None, start_time: 2000, end_time: 2400, status_code: 200, tags: [("endpoint", "/payment")] },
        { span_id: "span_006", service_name: "payment-service", operation_name: "process_payment", parent_span_id: Some("span_005"), start_time: 2050, end_time: 2200, status_code: 200, tags: [] },
        { span_id: "span_007", service_name: "inventory-service", operation_name: "reserve_items", parent_span_id: Some("span_006"), start_time: 2100, end_time: 2350, status_code: 500, tags: [] },
        { span_id: "span_008", service_name: "notification-service", operation_name: "send_confirmation", parent_span_id: Some("span_005"), start_time: 2200, end_time: 2400, status_code: 200, tags: [] }
      ],
      total_duration_ms: 400,
      error_count: 1
    },
    {
      trace_id: "trace_003",
      services: ["api-gateway", "product-service", "review-service"],
      spans: [
        { span_id: "span_009", service_name: "api-gateway", operation_name: "route_request", parent_span_id: None, start_time: 3000, end_time: 3300, status_code: 200, tags: [("endpoint", "/products")] },
        { span_id: "span_010", service_name: "product-service", operation_name: "get_product_details", parent_span_id: Some("span_009"), start_time: 3050, end_time: 3150, status_code: 200, tags: [] },
        { span_id: "span_011", service_name: "review-service", operation_name: "get_reviews", parent_span_id: Some("span_009"), start_time: 3150, end_time: 3300, status_code: 200, tags: [] }
      ],
      total_duration_ms: 300,
      error_count: 0
    }
  ]
  
  // 提取服务关系
  let extract_service_relations = fn(traces: Array[CrossServiceTrace]) {
    let relations = Map::empty()
    
    for trace in traces {
      for span in trace.spans {
        match span.parent_span_id {
          Some(parent_id) => {
            // 查找父跨度
            let parent_span = trace.spans.find(fn(s) { s.span_id == parent_id })
            
            match parent_span {
              Some(p) => {
                if p.service_name != span.service_name {
                  // 创建服务关系
                  let relation_key = p.service_name + "->" + span.service_name
                  let existing_relation = match Map::get(relations, relation_key) {
                    Some(r) => r
                    None => {
                      {
                        source_service: p.service_name,
                        target_service: span.service_name,
                        dependency_type: DependencyType::Synchronous,  // 默认为同步
                        call_count: 0,
                        error_count: 0,
                        avg_latency_ms: 0.0,
                        timestamp: 0
                      }
                    }
                  }
                  
                  let duration = span.end_time - span.start_time
                  let total_calls = existing_relation.call_count + 1
                  let total_errors = existing_relation.error_count + if span.status_code >= 400 { 1 } else { 0 }
                  let total_latency = existing_relation.avg_latency_ms * (existing_relation.call_count as Float) + (duration as Float)
                  let avg_latency = total_latency / (total_calls as Float)
                  
                  let updated_relation = {
                    source_service: existing_relation.source_service,
                    target_service: existing_relation.target_service,
                    dependency_type: existing_relation.dependency_type,
                    call_count: total_calls,
                    error_count: total_errors,
                    avg_latency_ms: avg_latency,
                    timestamp: span.start_time
                  }
                  
                  let _ = Map::insert(relations, relation_key, updated_relation)
                }
              }
              None => {}
            }
          }
          None => {}  // 根跨度，没有父级
        }
      }
    }
    
    // 转换为数组
    let relation_array = []
    for (_, relation) in relations {
      relation_array = relation_array.push(relation)
    }
    
    relation_array
  }
  
  // 构建服务拓扑
  let build_service_topology = fn(traces: Array[CrossServiceTrace]) {
    let services = []
    let service_set = Set::empty()
    
    // 收集所有服务
    for trace in traces {
      for service in trace.services {
        if not(Set::contains(service_set, service)) {
          services = services.push(service)
          service_set = Set::add(service_set, service)
        }
      }
    }
    
    let relations = extract_service_relations(traces)
    
    // 简单的关键路径分析：基于调用频率
    let critical_path = []
    let call_counts = Map::empty()
    
    for relation in relations {
      let count = match Map::get(call_counts, relation.source_service) {
        Some(c) => c + relation.call_count
        None => relation.call_count
      }
      let _ = Map::insert(call_counts, relation.source_service, count)
    }
    
    // 按调用次数排序服务
    let sorted_services = services.sort(fn(a, b) {
      let count_a = match Map::get(call_counts, a) { Some(c) => c | None => 0 }
      let count_b = match Map::get(call_counts, b) { Some(c) => c | None => 0 }
      if count_a > count_b { -1 } else if count_a < count_b { 1 } else { 0 }
    })
    
    // 选择前3个服务作为关键路径
    for i in 0..(if sorted_services.length() > 3 { 3 } else { sorted_services.length() }) {
      critical_path = critical_path.push(sorted_services[i])
    }
    
    {
      services,
      relations,
      critical_path
    }
  }
  
  // 构建服务依赖图
  let build_dependency_graph = fn(topology: ServiceTopology) {
    let nodes = Map::empty()
    let edges = Map::empty()
    
    // 初始化节点
    for service in topology.services {
      let node = {
        name: service,
        inbound_count: 0,
        outbound_count: 0,
        error_rate: 0.0,
        avg_latency: 0.0
      }
      let _ = Map::insert(nodes, service, node)
    }
    
    // 添加边和更新节点统计
    for relation in topology.relations {
      // 更新源节点的出站计数
      let source_node = match Map::get(nodes, relation.source_service) {
        Some(n) => n
        None => { name: relation.source_service, inbound_count: 0, outbound_count: 0, error_rate: 0.0, avg_latency: 0.0 }
      }
      let updated_source = { source_node | outbound_count: source_node.outbound_count + relation.call_count }
      let _ = Map::insert(nodes, relation.source_service, updated_source)
      
      // 更新目标节点的入站计数
      let target_node = match Map::get(nodes, relation.target_service) {
        Some(n) => n
        None => { name: relation.target_service, inbound_count: 0, outbound_count: 0, error_rate: 0.0, avg_latency: 0.0 }
      }
      let updated_target = { target_node | inbound_count: target_node.inbound_count + relation.call_count }
      let _ = Map::insert(nodes, relation.target_service, updated_target)
      
      // 添加边
      let edge = {
        target: relation.target_service,
        dependency_type: relation.dependency_type,
        weight: relation.call_count as Float,
        error_rate: if relation.call_count > 0 { (relation.error_count as Float) / (relation.call_count as Float) } else { 0.0 }
      }
      
      let existing_edges = match Map::get(edges, relation.source_service) {
        Some(e) => e
        None => []
      }
      
      let _ = Map::insert(edges, relation.source_service, existing_edges.push(edge))
    }
    
    // 计算节点平均延迟和错误率
    let calculated_nodes = Map::empty()
    for (service, node) in nodes {
      let service_relations = topology.relations.filter(fn(r) { r.source_service == service or r.target_service == service })
      
      let total_latency = service_relations.reduce(fn(acc, r) { acc + r.avg_latency_ms }, 0.0)
      let avg_latency = if service_relations.length() > 0 { total_latency / (service_relations.length() as Float) } else { 0.0 }
      
      let total_errors = service_relations.reduce(fn(acc, r) { acc + r.error_count }, 0)
      let total_calls = service_relations.reduce(fn(acc, r) { acc + r.call_count }, 0)
      let error_rate = if total_calls > 0 { (total_errors as Float) / (total_calls as Float) } else { 0.0 }
      
      let calculated_node = {
        name: node.name,
        inbound_count: node.inbound_count,
        outbound_count: node.outbound_count,
        error_rate,
        avg_latency
      }
      
      let _ = Map::insert(calculated_nodes, service, calculated_node)
    }
    
    {
      nodes: calculated_nodes,
      edges
    }
  }
  
  // 分析跨服务性能
  let analyze_cross_service_performance = fn(traces: Array[CrossServiceTrace]) {
    let service_performance = Map::empty()
    
    for trace in traces {
      for span in trace.spans {
        let service_data = match Map::get(service_performance, span.service_name) {
          Some(data) => data
          None => { call_count: 0, total_duration: 0, error_count: 0 }
        }
        
        let duration = span.end_time - span.start_time
        let updated_data = {
          call_count: service_data.call_count + 1,
          total_duration: service_data.total_duration + duration,
          error_count: service_data.error_count + if span.status_code >= 400 { 1 } else { 0 }
        }
        
        let _ = Map::insert(service_performance, span.service_name, updated_data)
      }
    }
    
    // 计算平均值
    let performance_stats = Map::empty()
    for (service, data) in service_performance {
      let avg_duration = if data.call_count > 0 { (data.total_duration as Float) / (data.call_count as Float) } else { 0.0 }
      let error_rate = if data.call_count > 0 { (data.error_count as Float) / (data.call_count as Float) } else { 0.0 }
      
      let stats = {
        call_count: data.call_count,
        avg_duration,
        error_rate
      }
      
      let _ = Map::insert(performance_stats, service, stats)
    }
    
    performance_stats
  }
  
  // 识别服务瓶颈
  let identify_bottlenecks = fn(performance_stats: Map[String, { call_count: Int, avg_duration: Float, error_rate: Float }]) {
    let bottlenecks = []
    
    for (service, stats) in performance_stats {
      // 定义瓶颈阈值
      let is_slow = stats.avg_duration > 200.0  // 平均延迟超过200ms
      let is_error_prone = stats.error_rate > 0.1  // 错误率超过10%
      let is_high_volume = stats.call_count > 10  // 高调用量
      
      if is_slow or is_error_prone or is_high_volume {
        bottlenecks = bottlenecks.push({
          service,
          avg_duration: stats.avg_duration,
          error_rate: stats.error_rate,
          call_count: stats.call_count,
          issues: [
            if is_slow { "high_latency" } else { "" },
            if is_error_prone { "high_error_rate" } else { "" },
            if is_high_volume { "high_volume" } else { "" }
          ].filter(fn(i) { i != "" })
        })
      }
    }
    
    // 按严重程度排序（综合考虑延迟、错误率和调用量）
    bottlenecks.sort(fn(a, b) {
      let score_a = a.avg_duration * 0.4 + a.error_rate * 100.0 * 0.4 + (a.call_count as Float) * 0.2
      let score_b = b.avg_duration * 0.4 + b.error_rate * 100.0 * 0.4 + (b.call_count as Float) * 0.2
      
      if score_a > score_b { -1 } else if score_a < score_b { 1 } else { 0 }
    })
  }
  
  // 测试服务关系提取
  let relations = extract_service_relations(cross_service_traces)
  assert_eq(relations.length(), 6)  // 6个服务关系
  
  let frontend_auth = relations.filter(fn(r) { r.source_service == "frontend" and r.target_service == "auth-service" })[0]
  assert_eq(frontend_auth.call_count, 1)
  assert_eq(frontend_auth.error_count, 0)
  
  let payment_inventory = relations.filter(fn(r) { r.source_service == "payment-service" and r.target_service == "inventory-service" })[0]
  assert_eq(payment_inventory.call_count, 1)
  assert_eq(payment_inventory.error_count, 1)  // inventory-service返回500错误
  
  // 测试服务拓扑构建
  let topology = build_service_topology(cross_service_traces)
  assert_eq(topology.services.length(), 7)  // 7个不同的服务
  assert_eq(topology.relations.length(), 6)
  assert_eq(topology.critical_path.length(), 3)  // 前3个关键服务
  
  // 验证关键路径包含高频服务
  assert_true(topology.critical_path.contains("frontend"))
  assert_true(topology.critical_path.contains("payment-service"))
  
  // 测试依赖图构建
  let dependency_graph = build_dependency_graph(topology)
  assert_eq(Map::size(dependency_graph.nodes), 7)
  
  let frontend_node = match Map::get(dependency_graph.nodes, "frontend") {
    Some(node) => node
    None => { name: "", inbound_count: 0, outbound_count: 0, error_rate: 0.0, avg_latency: 0.0 }
  }
  assert_eq(frontend_node.name, "frontend")
  assert_eq(frontend_node.outbound_count, 4)  // frontend调用4个其他服务
  
  let inventory_node = match Map::get(dependency_graph.nodes, "inventory-service") {
    Some(node) => node
    None => { name: "", inbound_count: 0, outbound_count: 0, error_rate: 0.0, avg_latency: 0.0 }
  }
  assert_eq(inventory_node.name, "inventory-service")
  assert_eq(inventory_node.inbound_count, 1)  // 只有payment-service调用inventory-service
  
  // 测试跨服务性能分析
  let performance_stats = analyze_cross_service_performance(cross_service_traces)
  assert_eq(Map::size(performance_stats), 7)
  
  let inventory_performance = match Map::get(performance_stats, "inventory-service") {
    Some(stats) => stats
    None => { call_count: 0, avg_duration: 0.0, error_rate: 0.0 }
  }
  assert_eq(inventory_performance.call_count, 1)
  assert_eq(inventory_performance.avg_duration, 250.0)  // 2350-2100=250ms
  assert_eq(inventory_performance.error_rate, 1.0)     // 1个错误 / 1个调用
  
  // 测试瓶颈识别
  let bottlenecks = identify_bottlenecks(performance_stats)
  assert_eq(bottlenecks.length(), 2)  // inventory-service和payment-service应该是瓶颈
  
  let top_bottleneck = bottlenecks[0]
  assert_eq(top_bottleneck.service, "inventory-service")
  assert_true(top_bottleneck.issues.contains("high_latency"))
  assert_true(top_bottleneck.issues.contains("high_error_rate"))
  
  // 测试服务影响分析
  let analyze_service_impact = fn(graph: ServiceDependencyGraph, service: String) {
    let impacted_services = []
    let visited = Set::empty()
    
    // DFS查找下游服务
    let find_downstream = fn(current_service: String) {
      if Set::contains(visited, current_service) {
        return
      }
      visited = Set::add(visited, current_service)
      
      match Map::get(graph.edges, current_service) {
        Some(edges) => {
          for edge in edges {
            impacted_services = impacted_services.push(edge.target)
            find_downstream(edge.target)
          }
        }
        None => {}
      }
    }
    
    find_downstream(service)
    impacted_services
  }
  
  let frontend_impact = analyze_service_impact(dependency_graph, "frontend")
  assert_eq(frontend_impact.length(), 4)  // frontend影响4个服务
  assert_true(frontend_impact.contains("auth-service"))
  assert_true(frontend_impact.contains("user-service"))
  assert_true(frontend_impact.contains("order-service"))
  assert_true(frontend_impact.contains("payment-service"))
  
  let payment_impact = analyze_service_impact(dependency_graph, "payment-service")
  assert_eq(payment_impact.length(), 1)  // payment-service只影响inventory-service
  assert_true(payment_impact.contains("inventory-service"))
  
  // 测试服务链路分析
  let analyze_service_chain = fn(trace: CrossServiceTrace) {
    let chain = []
    let mut visited_spans = []
    
    // 找到根跨度
    let root_span = trace.spans.find(fn(s) { s.parent_span_id == None })
    
    match root_span {
      Some(root) => {
        visited_spans = visited_spans.push(root)
        chain = chain.push(root.service_name)
        
        // 构建调用链
        let mut current_level = [root]
        
        while current_level.length() > 0 {
          let mut next_level = []
          
          for span in current_level {
            let child_spans = trace.spans.filter(fn(s) { 
              match s.parent_span_id {
                Some(parent_id) => parent_id == span.span_id
                None => false
              }
            })
            
            for child in child_spans {
              if not(visited_spans.any(fn(s) { s.span_id == child.span_id })) {
                visited_spans = visited_spans.push(child)
                next_level = next_level.push(child)
                chain = chain.push(child.service_name)
              }
            }
          }
          
          current_level = next_level
        }
      }
      None => {}
    }
    
    chain
  }
  
  let trace_001_chain = analyze_service_chain(cross_service_traces[0])
  assert_eq(trace_001_chain.length(), 4)
  assert_eq(trace_001_chain[0], "frontend")
  assert_eq(trace_001_chain[1], "auth-service")
  assert_eq(trace_001_chain[2], "user-service")
  assert_eq(trace_001_chain[3], "order-service")
  
  // 测试服务依赖强度分析
  let analyze_dependency_strength = fn(relations: Array[ServiceRelation]) {
    let total_calls = relations.reduce(fn(acc, r) { acc + r.call_count }, 0)
    
    let strength_analysis = relations.map(fn(relation) {
      let strength = if total_calls > 0 { (relation.call_count as Float) / (total_calls as Float) } else { 0.0 }
      
      {
        source: relation.source_service,
        target: relation.target_service,
        call_count: relation.call_count,
        strength,
        reliability: if relation.call_count > 0 { 1.0 - ((relation.error_count as Float) / (relation.call_count as Float)) } else { 1.0 },
        avg_latency: relation.avg_latency_ms
      }
    })
    
    // 按强度排序
    strength_analysis.sort(fn(a, b) { if a.strength > b.strength { -1 } else if a.strength < b.strength { 1 } else { 0 } })
  }
  
  let dependency_strength = analyze_dependency_strength(relations)
  assert_eq(dependency_strength.length(), 6)
  
  // 最强的依赖关系应该是调用次数最多的
  let strongest_dependency = dependency_strength[0]
  assert_true(strongest_dependency.call_count >= 1)
  
  // 测试服务健康评分
  let calculate_service_health_score = fn(graph: ServiceDependencyGraph, service: String) {
    let service_node = match Map::get(graph.nodes, service) {
      Some(node) => node
      None => { name: service, inbound_count: 0, outbound_count: 0, error_rate: 0.0, avg_latency: 0.0 }
    }
    
    // 基于错误率、延迟和连接数的健康评分
    let error_score = if service_node.error_rate > 0.0 { 1.0 - service_node.error_rate } else { 1.0 }
    let latency_score = if service_node.avg_latency > 0.0 { 1.0 / (1.0 + service_node.avg_latency / 100.0) } else { 1.0 }
    let connection_score = if service_node.inbound_count + service_node.outbound_count > 0 {
      1.0 - (1.0 / ((service_node.inbound_count + service_node.outbound_count) as Float + 1.0))
    } else { 0.5 }
    
    // 加权平均
    let health_score = error_score * 0.5 + latency_score * 0.3 + connection_score * 0.2
    
    {
      service,
      health_score,
      error_score,
      latency_score,
      connection_score
    }
  }
  
  let frontend_health = calculate_service_health_score(dependency_graph, "frontend")
  assert_eq(frontend_health.service, "frontend")
  assert_true(frontend_health.health_score >= 0.0)
  assert_true(frontend_health.health_score <= 1.0)
  
  let inventory_health = calculate_service_health_score(dependency_graph, "inventory-service")
  assert_eq(inventory_health.service, "inventory-service")
  assert_true(inventory_health.error_score < frontend_health.error_score)  // inventory-service有错误
}