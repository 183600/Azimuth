// Azimuth Custom Instrumentation Test Suite
// This file contains test cases for custom instrumentation and telemetry

// Test 1: Custom Span Creation and Management
test "custom span creation and lifecycle management" {
  // Define span kind
  enum SpanKind {
    Internal
    Server
    Client
    Producer
    Consumer
  }
  
  // Define span status
  type SpanStatus = {
    code: Int,
    message: String
  }
  
  // Define custom span
  type CustomSpan = {
    span_id: String,
    trace_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    kind: SpanKind,
    start_time: Int,
    end_time: Option[Int>,
    status: SpanStatus,
    attributes: Array[(String, String)>,
    events: Array<SpanEvent>,
    links: Array<SpanLink>
  }
  
  // Define span event
  type SpanEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)>
  }
  
  // Define span link
  type SpanLink = {
    trace_id: String,
    span_id: String,
    attributes: Array[(String, String)>
  }
  
  // Define span context
  type SpanContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int
  }
  
  // Create span builder
  let span_builder = fn(operation_name: String) {
    {
      span_id: "",
      trace_id: "",
      parent_span_id: None,
      operation_name,
      kind: SpanKind::Internal,
      start_time: 0,
      end_time: None,
      status: { code: 0, message: "" },
      attributes: [],
      events: [],
      links: []
    }
  }
  
  // Set span kind
  let with_kind = fn(builder: CustomSpan, kind: SpanKind) {
    { builder | kind }
  }
  
  // Set parent span
  let with_parent = fn(builder: CustomSpan, parent_context: SpanContext) {
    { builder | parent_span_id: Some(parent_context.span_id), trace_id: parent_context.trace_id }
  }
  
  // Set attributes
  let with_attributes = fn(builder: CustomSpan, attrs: Array[(String, String)>) {
    { builder | attributes: builder.attributes + attrs }
  }
  
  // Start span
  let start_span = fn(builder: CustomSpan) {
    let current_time = 1640995200  // Fixed timestamp for testing
    let trace_id = if builder.trace_id.length() > 0 {
      builder.trace_id
    } else {
      "trace-" + current_time.to_string()
    }
    
    let span_id = "span-" + (current_time + 1000).to_string()
    
    {
      span_id,
      trace_id,
      parent_span_id: builder.parent_span_id,
      operation_name: builder.operation_name,
      kind: builder.kind,
      start_time: current_time,
      end_time: None,
      status: builder.status,
      attributes: builder.attributes,
      events: [],
      links: builder.links
    }
  }
  
  // End span
  let end_span = fn(span: CustomSpan, status: Option<SpanStatus>) {
    let end_time = 1640995300  // 100 seconds after start
    let final_status = match status {
      Some(s) => s
      None => { code: 0, message: "OK" }
    }
    
    {
      span_id: span.span_id,
      trace_id: span.trace_id,
      parent_span_id: span.parent_span_id,
      operation_name: span.operation_name,
      kind: span.kind,
      start_time: span.start_time,
      end_time: Some(end_time),
      status: final_status,
      attributes: span.attributes,
      events: span.events,
      links: span.links
    }
  }
  
  // Add event to span
  let add_event = fn(span: CustomSpan, name: String, attributes: Array<(String, String)>) {
    let current_time = 1640995250  // 50 seconds after start
    let new_event = {
      name,
      timestamp: current_time,
      attributes
    }
    
    { span | events: span.events.push(new_event) }
  }
  
  // Add link to span
  let add_link = fn(span: CustomSpan, link_context: SpanContext, attributes: Array<(String, String)>) {
    let new_link = {
      trace_id: link_context.trace_id,
      span_id: link_context.span_id,
      attributes
    }
    
    { span | links: span.links.push(new_link) }
  }
  
  // Test span builder
  let builder = span_builder("database_query")
  assert_eq(builder.operation_name, "database_query")
  assert_eq(builder.kind, SpanKind::Internal)
  
  // Test span configuration
  let configured_builder = builder
    |> with_kind(SpanKind::Client)
    |> with_attributes([("db.system", "postgresql"), ("db.statement", "SELECT * FROM users")])
  
  assert_eq(configured_builder.kind, SpanKind::Client)
  assert_eq(configured_builder.attributes.length(), 2)
  
  // Test span with parent
  let parent_context = {
    trace_id: "trace-1234567890",
    span_id: "span-0987654321",
    trace_flags: 1
  }
  
  let builder_with_parent = with_parent(configured_builder, parent_context)
  assert_eq(builder_with_parent.trace_id, "trace-1234567890")
  assert_eq(builder_with_parent.parent_span_id, Some("span-0987654321"))
  
  // Test span start
  let started_span = start_span(builder_with_parent)
  assert_eq(started_span.span_id, "span-1640996200")
  assert_eq(started_span.trace_id, "trace-1234567890")
  assert_eq(started_span.operation_name, "database_query")
  assert_eq(started_span.kind, SpanKind::Client)
  assert_eq(started_span.start_time, 1640995200)
  assert_eq(started_span.end_time, None)
  assert_eq(started_span.attributes.length(), 2)
  
  // Test adding events
  let span_with_event = add_event(started_span, "query.executed", [
    ("db.rows_affected", "5"),
    ("db.duration_ms", "150")
  ])
  
  assert_eq(span_with_event.events.length(), 1)
  assert_eq(span_with_event.events[0].name, "query.executed")
  assert_eq(span_with_event.events[0].timestamp, 1640995250)
  assert_eq(span_with_event.events[0].attributes.length(), 2)
  
  // Test adding links
  let linked_context = {
    trace_id: "trace-1111111111",
    span_id: "span-2222222222",
    trace_flags: 1
  }
  
  let span_with_link = add_link(span_with_event, linked_context, [
    ("link.type", "follows_from"),
    ("service", "cache-service")
  ])
  
  assert_eq(span_with_link.links.length(), 1)
  assert_eq(span_with_link.links[0].trace_id, "trace-1111111111")
  assert_eq(span_with_link.links[0].span_id, "span-2222222222")
  assert_eq(span_with_link.links[0].attributes.length(), 2)
  
  // Test span end
  let error_status = { code: 1, message: "Connection timeout" }
  let ended_span = end_span(span_with_link, Some(error_status))
  
  assert_eq(ended_span.end_time, Some(1640995300))
  assert_eq(ended_span.status.code, 1)
  assert_eq(ended_span.status.message, "Connection timeout")
  
  // Test span duration calculation
  let calculate_span_duration = fn(span: CustomSpan) {
    match span.end_time {
      Some(end_time) => end_time - span.start_time
      None => 0  // Not ended yet
    }
  }
  
  let duration = calculate_span_duration(ended_span)
  assert_eq(duration, 100)  // 100 seconds
  
  let active_duration = calculate_span_duration(started_span)
  assert_eq(active_duration, 0)  // Not ended yet
  
  // Test span serialization
  let serialize_span = fn(span: CustomSpan) {
    let attributes_str = span.attributes.map(fn(attr) {
      "\"" + attr.0 + "\": \"" + attr.1 + "\""
    }).join(",")
    
    let events_str = span.events.map(fn(event) {
      "{\"name\":\"" + event.name + "\",\"timestamp\":" + event.timestamp.to_string() + "}"
    }).join(",")
    
    let links_str = span.links.map(fn(link) {
      "{\"trace_id\":\"" + link.trace_id + "\",\"span_id\":\"" + link.span_id + "\"}"
    }).join(",")
    
    "{" +
      "\"span_id\":\"" + span.span_id + "\"," +
      "\"trace_id\":\"" + span.trace_id + "\"," +
      "\"operation_name\":\"" + span.operation_name + "\"," +
      "\"start_time\":" + span.start_time.to_string() + "," +
      "\"end_time\":" + match span.end_time {
        Some(end) => end.to_string()
        None => "null"
      } + "," +
      "\"status\":{\"code\":" + span.status.code.to_string() + ",\"message\":\"" + span.status.message + "\"}," +
      "\"attributes\":{" + attributes_str + "}," +
      "\"events\":[" + events_str + "]," +
      "\"links\":[" + links_str + "]" +
    "}"
  }
  
  let serialized = serialize_span(ended_span)
  assert_true(serialized.contains("\"span_id\":\"span-1640996200\""))
  assert_true(serialized.contains("\"operation_name\":\"database_query\""))
  assert_true(serialized.contains("\"db.system\":\"postgresql\""))
  assert_true(serialized.contains("\"name\":\"query.executed\""))
}

// Test 2: Custom Metrics Creation
test "custom metrics creation and recording" {
  // Define metric type
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // Define metric value
  type MetricValue = {
    value: Float,
    labels: Array[(String, String)>,
    timestamp: Int
  }
  
  // Define metric
  type Metric = {
    name: String,
    description: String,
    unit: String,
    metric_type: MetricType,
    values: Array[MetricValue>
  }
  
  // Define metric registry
  type MetricRegistry = {
    metrics: Array[Metric]
  }
  
  // Create metric registry
  let create_metric_registry = fn() {
    { metrics: [] }
  }
  
  // Create counter metric
  let create_counter = fn(name: String, description: String, unit: String) {
    {
      name,
      description,
      unit,
      metric_type: MetricType::Counter,
      values: []
    }
  }
  
  // Create gauge metric
  let create_gauge = fn(name: String, description: String, unit: String) {
    {
      name,
      description,
      unit,
      metric_type: MetricType::Gauge,
      values: []
    }
  }
  
  // Create histogram metric
  let create_histogram = fn(name: String, description: String, unit: String, buckets: Array<Float>) {
    {
      name,
      description,
      unit,
      metric_type: MetricType::Histogram,
      values: []
    }
  }
  
  // Register metric
  let register_metric = fn(registry: MetricRegistry, metric: Metric) {
    let existing = registry.metrics.find_index(fn(m) { m.name == metric.name })
    
    match existing {
      Some(index) => {
        // Update existing metric
        let updated_metrics = registry.metrics.slice(0, index) + 
                             [metric] + 
                             registry.metrics.slice(index + 1, registry.metrics.length())
        { registry | metrics: updated_metrics }
      }
      None => {
        // Add new metric
        { registry | metrics: registry.metrics.push(metric) }
      }
    }
  }
  
  // Record metric value
  let record_metric = fn(registry: MetricRegistry, metric_name: String, value: Float, 
                         labels: Array<(String, String)>) {
    let current_time = 1640995200
    
    let updated_metrics = registry.metrics.map(fn(metric) {
      if metric.name == metric_name {
        let metric_value = {
          value,
          labels,
          timestamp: current_time
        }
        
        { metric | values: metric.values.push(metric_value) }
      } else {
        metric
      }
    })
    
    { registry | metrics: updated_metrics }
  }
  
  // Increment counter
  let increment_counter = fn(registry: MetricRegistry, counter_name: String, amount: Float, 
                            labels: Array<(String, String)>) {
    let current_time = 1640995200
    
    let updated_metrics = registry.metrics.map(fn(metric) {
      if metric.name == counter_name && metric.metric_type == MetricType::Counter {
        // Find existing value with same labels
        let existing_index = metric.values.find_index(fn(v) { v.labels == labels })
        
        match existing_index {
          Some(index) => {
            let current_value = metric.values[index].value
            let updated_value = metric.values[index] | { value: current_value + amount, timestamp: current_time }
            let updated_values = metric.values.slice(0, index) + 
                                 [updated_value] + 
                                 metric.values.slice(index + 1, metric.values.length())
            { metric | values: updated_values }
          }
          None => {
            let new_value = {
              value: amount,
              labels,
              timestamp: current_time
            }
            { metric | values: metric.values.push(new_value) }
          }
        }
      } else {
        metric
      }
    })
    
    { registry | metrics: updated_metrics }
  }
  
  // Set gauge value
  let set_gauge = fn(registry: MetricRegistry, gauge_name: String, value: Float, 
                     labels: Array<(String, String)>) {
    let current_time = 1640995200
    
    let updated_metrics = registry.metrics.map(fn(metric) {
      if metric.name == gauge_name && metric.metric_type == MetricType::Gauge {
        // Find existing value with same labels
        let existing_index = metric.values.find_index(fn(v) { v.labels == labels })
        
        match existing_index {
          Some(index) => {
            let updated_value = metric.values[index] | { value, timestamp: current_time }
            let updated_values = metric.values.slice(0, index) + 
                                 [updated_value] + 
                                 metric.values.slice(index + 1, metric.values.length())
            { metric | values: updated_values }
          }
          None => {
            let new_value = {
              value,
              labels,
              timestamp: current_time
            }
            { metric | values: metric.values.push(new_value) }
          }
        }
      } else {
        metric
      }
    })
    
    { registry | metrics: updated_metrics }
  }
  
  // Test registry creation
  let registry = create_metric_registry()
  assert_eq(registry.metrics.length(), 0)
  
  // Test metric creation
  let request_counter = create_counter("http_requests_total", "Total HTTP requests", "requests")
  let memory_gauge = create_gauge("memory_usage_bytes", "Memory usage in bytes", "bytes")
  let response_histogram = create_histogram("http_request_duration_seconds", 
                                          "HTTP request duration", "seconds", 
                                          [0.1, 0.5, 1.0, 2.5, 5.0])
  
  // Test metric registration
  let registry1 = register_metric(registry, request_counter)
  let registry2 = register_metric(registry1, memory_gauge)
  let registry3 = register_metric(registry2, response_histogram)
  
  assert_eq(registry3.metrics.length(), 3)
  assert_eq(registry3.metrics[0].name, "http_requests_total")
  assert_eq(registry3.metrics[1].name, "memory_usage_bytes")
  assert_eq(registry3.metrics[2].name, "http_request_duration_seconds")
  
  // Test counter increment
  let registry4 = increment_counter(registry3, "http_requests_total", 1.0, 
                                   [("method", "GET"), ("/api/users", "path")])
  let registry5 = increment_counter(registry4, "http_requests_total", 1.0, 
                                   [("method", "GET"), ("/api/users", "path")])
  let registry6 = increment_counter(registry5, "http_requests_total", 1.0, 
                                   [("method", "POST"), ("/api/orders", "path")])
  
  // Check counter values
  let http_requests_metric = registry6.metrics.find(fn(m) { m.name == "http_requests_total" })
  match http_requests_metric {
    Some(metric) => {
      assert_eq(metric.values.length(), 2)  // Two different label combinations
      
      let get_value = fn(labels: Array<(String, String)>) {
        metric.values.find(fn(v) { v.labels == labels })
      }
      
      let get_requests = get_value([("method", "GET"), ("/api/users", "path")])
      match get_requests {
        Some(value) => assert_eq(value.value, 2.0)  // Incremented twice
        None => assert_true(false)
      }
      
      let post_requests = get_value([("method", "POST"), ("/api/orders", "path")])
      match post_requests {
        Some(value) => assert_eq(value.value, 1.0)  // Incremented once
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test gauge setting
  let registry7 = set_gauge(registry6, "memory_usage_bytes", 1024.0 * 1024.0 * 512.0, 
                            [("instance", "server-1")])
  let registry8 = set_gauge(registry7, "memory_usage_bytes", 1024.0 * 1024.0 * 600.0, 
                            [("instance", "server-1")])
  
  // Check gauge values
  let memory_metric = registry8.metrics.find(fn(m) { m.name == "memory_usage_bytes" })
  match memory_metric {
    Some(metric) => {
      assert_eq(metric.values.length(), 1)
      assert_eq(metric.values[0].value, 1024.0 * 1024.0 * 600.0)  // Updated value
    }
    None => assert_true(false)
  }
  
  // Test metric aggregation
  let aggregate_metric = fn(metric: Metric, aggregation_fn: (Array<Float>) -> Float) {
    let values = metric.values.map(fn(v) { v.value })
    aggregation_fn(values)
  }
  
  let sum_fn = fn(values: Array<Float>) {
    values.reduce(fn(sum, value) { sum + value }, 0.0)
  }
  
  let avg_fn = fn(values: Array<Float>) {
    if values.length() > 0 {
      sum_fn(values) / (values.length() as Float)
    } else {
      0.0
    }
  }
  
  let max_fn = fn(values: Array<Float>) {
    values.reduce(fn(max, value) { if value > max { value } else { max }, 0.0)
  }
  
  // Test aggregation
  let total_requests = aggregate_metric(http_requests_metric.unwrap(), sum_fn)
  assert_eq(total_requests, 3.0)  // 2 GET + 1 POST
  
  let avg_memory = aggregate_metric(memory_metric.unwrap(), avg_fn)
  assert_eq(avg_memory, 1024.0 * 1024.0 * 600.0)  // Only one value
  
  // Test metric filtering
  let filter_metrics = fn(registry: MetricRegistry, filter_fn: (Metric) -> Bool) {
    registry.metrics.filter(filter_fn)
  }
  
  let counter_metrics = filter_metrics(registry8, fn(m) { m.metric_type == MetricType::Counter })
  assert_eq(counter_metrics.length(), 1)
  assert_eq(counter_metrics[0].name, "http_requests_total")
  
  let gauge_metrics = filter_metrics(registry8, fn(m) { m.metric_type == MetricType::Gauge })
  assert_eq(gauge_metrics.length(), 1)
  assert_eq(gauge_metrics[0].name, "memory_usage_bytes")
  
  // Test metric export
  let export_metric = fn(metric: Metric) {
    let values_str = metric.values.map(fn(value) {
      let labels_str = value.labels.map(fn(label) {
        label.0 + "=\"" + label.1 + "\""
      }).join(",")
      
      metric.name + "{" + labels_str + "} " + value.value.to_string()
    }).join("\n")
    
    "# HELP " + metric.name + " " + metric.description + "\n" +
    "# TYPE " + metric.name + " " + match metric.metric_type {
      MetricType::Counter => "counter"
      MetricType::Gauge => "gauge"
      MetricType::Histogram => "histogram"
      MetricType::Summary => "summary"
    } + "\n" +
    values_str
  }
  
  let exported_requests = export_metric(http_requests_metric.unwrap())
  assert_true(exported_requests.contains("# HELP http_requests_total Total HTTP requests"))
  assert_true(exported_requests.contains("# TYPE http_requests_total counter"))
  assert_true(exported_requests.contains("http_requests_total{method=\"GET\",path=\"/api/users\"} 2.0"))
  assert_true(exported_requests.contains("http_requests_total{method=\"POST\",path=\"/api/orders\"} 1.0"))
}

// Test 3: Custom Logging and Structured Events
test "custom logging and structured event creation" {
  // Define log level
  enum LogLevel {
    Trace
    Debug
    Info
    Warn
    Error
    Fatal
  }
  
  // Define log entry
  type LogEntry = {
    timestamp: Int,
    level: LogLevel,
    message: String,
    logger_name: String,
    thread_name: String,
    exception: Option<String>,
    markers: Array<String>,
    mdc: Array[(String, String)>,  // Mapped Diagnostic Context
    context: Array<(String, String)>
  }
  
  // Define logger configuration
  type LoggerConfig = {
    name: String,
    level: LogLevel,
    appenders: Array<String>,
    include_location: Bool,
    include_thread: Bool
  }
  
  // Define logger
  type Logger = {
    config: LoggerConfig,
    entries: Array<LogEntry>
  }
  
  // Create logger
  let create_logger = fn(config: LoggerConfig) {
    {
      config,
      entries: []
    }
  }
  
  // Convert log level to string
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
  
  // Check if log level should be logged
  let should_log = fn(config_level: LogLevel, message_level: LogLevel) {
    let config_order = match config_level {
      LogLevel::Trace => 0
      LogLevel::Debug => 1
      LogLevel::Info => 2
      LogLevel::Warn => 3
      LogLevel::Error => 4
      LogLevel::Fatal => 5
    }
    
    let message_order = match message_level {
      LogLevel::Trace => 0
      LogLevel::Debug => 1
      LogLevel::Info => 2
      LogLevel::Warn => 3
      LogLevel::Error => 4
      LogLevel::Fatal => 5
    }
    
    message_order >= config_order
  }
  
  // Create log entry
  let create_log_entry = fn(logger: Logger, level: LogLevel, message: String, 
                           exception: Option<String>, markers: Array<String>,
                           mdc: Array<(String, String)>, context: Array<(String, String)>) {
    if should_log(logger.config.level, level) {
      let current_time = 1640995200
      let thread_name = if logger.config.include_thread { "main-thread" } else { "" }
      
      Some({
        timestamp: current_time,
        level,
        message,
        logger_name: logger.config.name,
        thread_name,
        exception,
        markers,
        mdc,
        context
      })
    } else {
      None
    }
  }
  
  // Log message
  let log = fn(logger: Logger, level: LogLevel, message: String, exception: Option<String>,
               markers: Array<String>, mdc: Array<(String, String)>, 
               context: Array<(String, String)>) {
    let entry = create_log_entry(logger, level, message, exception, markers, mdc, context)
    
    match entry {
      Some(log_entry) => {
        { logger | entries: logger.entries.push(log_entry) }
      }
      None => logger
    }
  }
  
  // Test logger creation
  let logger_config = {
    name: "azimuth.telemetry",
    level: LogLevel::Info,
    appenders: ["console", "file"],
    include_location: true,
    include_thread: true
  }
  
  let logger = create_logger(logger_config)
  assert_eq(logger.config.name, "azimuth.telemetry")
  assert_eq(logger.config.level, LogLevel::Info)
  assert_eq(logger.entries.length(), 0)
  
  // Test logging at different levels
  let logger1 = log(logger, LogLevel::Debug, "Debug message", None, [], [], [])
  assert_eq(logger1.entries.length(), 0)  // Debug level below Info threshold
  
  let logger2 = log(logger1, LogLevel::Info, "Application started", None, [], [], [])
  assert_eq(logger2.entries.length(), 1)
  assert_eq(logger2.entries[0].level, LogLevel::Info)
  assert_eq(logger2.entries[0].message, "Application started")
  assert_eq(logger2.entries[0].thread_name, "main-thread")
  
  // Test logging with exception
  let logger3 = log(logger2, LogLevel::Error, "Database connection failed", 
                    Some("java.sql.SQLException: Connection timeout"), [], [], [])
  assert_eq(logger3.entries.length(), 2)
  assert_eq(logger3.entries[1].level, LogLevel::Error)
  assert_eq(logger3.entries[1].exception, Some("java.sql.SQLException: Connection timeout"))
  
  // Test logging with markers
  let logger4 = log(logger3, LogLevel::Warn, "Rate limit approaching", 
                    None, ["METRICS", "PERFORMANCE"], [], [])
  assert_eq(logger4.entries.length(), 3)
  assert_eq(logger4.entries[2].markers.length(), 2)
  assert_eq(logger4.entries[2].markers[0], "METRICS")
  assert_eq(logger4.entries[2].markers[1], "PERFORMANCE")
  
  // Test logging with MDC
  let logger5 = log(logger4, LogLevel::Info, "User authenticated", 
                    None, [], [("user_id", "12345"), ("session_id", "abc-def")], [])
  assert_eq(logger5.entries.length(), 4)
  assert_eq(logger5.entries[3].mdc.length(), 2)
  assert_eq(logger5.entries[3].mdc[0], ("user_id", "12345"))
  assert_eq(logger5.entries[3].mdc[1], ("session_id", "abc-def"))
  
  // Test logging with context
  let logger6 = log(logger5, LogLevel::Info, "Processing request", 
                    None, [], [], [("request_id", "req-123"), ("trace_id", "trace-456")])
  assert_eq(logger6.entries.length(), 5)
  assert_eq(logger6.entries[4].context.length(), 2)
  assert_eq(logger6.entries[4].context[0], ("request_id", "req-123"))
  assert_eq(logger6.entries[4].context[1], ("trace_id", "trace-456"))
  
  // Test log filtering
  let filter_logs = fn(logger: Logger, filter_fn: (LogEntry) -> Bool) {
    logger.entries.filter(filter_fn)
  }
  
  let error_logs = filter_logs(logger6, fn(entry) { 
    match entry.level {
      LogLevel::Error => true
      LogLevel::Fatal => true
      _ => false
    }
  })
  assert_eq(error_logs.length(), 1)
  assert_eq(error_logs[0].message, "Database connection failed")
  
  let marker_logs = filter_logs(logger6, fn(entry) { 
    entry.markers.contains("PERFORMANCE") 
  })
  assert_eq(marker_logs.length(), 1)
  assert_eq(marker_logs[0].message, "Rate limit approaching")
  
  // Test structured logging
  type StructuredLog = {
    timestamp: Int,
    level: String,
    logger: String,
    message: String,
    fields: Array<(String, String)>
  }
  
  let to_structured_log = fn(entry: LogEntry) {
    let mut fields = []
    
    // Add MDC fields
    for mdc_entry in entry.mdc {
      fields = fields.push(mdc_entry)
    }
    
    // Add context fields
    for context_entry in entry.context {
      fields = fields.push(context_entry)
    }
    
    // Add markers as field
    if entry.markers.length() > 0 {
      fields = fields.push(("markers", entry.markers.join(",")))
    }
    
    // Add exception as field
    match entry.exception {
      Some(exception) => {
        fields = fields.push(("exception", exception))
      }
      None => {}
    }
    
    {
      timestamp: entry.timestamp,
      level: level_to_string(entry.level),
      logger: entry.logger_name,
      message: entry.message,
      fields
    }
  }
  
  // Test structured log conversion
  let structured_logs = logger6.entries.map(to_structured_log)
  assert_eq(structured_logs.length(), 5)
  
  let auth_structured = structured_logs.find(fn(log) { log.message == "User authenticated" })
  match auth_structured {
    Some(log) => {
      assert_eq(log.level, "INFO")
      assert_eq(log.logger, "azimuth.telemetry")
      assert_eq(log.fields.length(), 2)
      assert_true(log.fields.contains(("user_id", "12345")))
      assert_true(log.fields.contains(("session_id", "abc-def")))
    }
    None => assert_true(false)
  }
  
  // Test JSON serialization
  let serialize_structured_log = fn(log: StructuredLog) {
    let fields_str = log.fields.map(fn(field) {
      "\"" + field.0 + "\":\"" + field.1 + "\""
    }).join(",")
    
    "{" +
      "\"timestamp\":" + log.timestamp.to_string() + "," +
      "\"level\":\"" + log.level + "\"," +
      "\"logger\":\"" + log.logger + "\"," +
      "\"message\":\"" + log.message + "\"," +
      "\"fields\":{" + fields_str + "}" +
    "}"
  }
  
  let json_logs = structured_logs.map(serialize_structured_log)
  assert_eq(json_logs.length(), 5)
  
  let auth_json = json_logs.find(fn(json) { json.contains("User authenticated") })
  match auth_json {
    Some(json) => {
      assert_true(json.contains("\"level\":\"INFO\""))
      assert_true(json.contains("\"user_id\":\"12345\""))
      assert_true(json.contains("\"session_id\":\"abc-def\""))
    }
    None => assert_true(false)
  }
  
  // Test log correlation
  let correlate_logs = fn(logs: Array<LogEntry>, correlation_key: String) {
    let mut correlated = []
    let mut processed_keys = []
    
    for log in logs {
      let correlation_value = log.context.find(fn(c) { c.0 == correlation_key })
      
      match correlation_value {
        Some((_, value)) => {
          if not(processed_keys.contains(value)) {
            processed_keys = processed_keys.push(value)
            let related_logs = logs.filter(fn(l) {
              l.context.any(fn(c) { c.0 == correlation_key && c.1 == value })
            })
            correlated = correlated.push((value, related_logs))
          }
        }
        None => {}
      }
    }
    
    correlated
  }
  
  // Test log correlation
  let correlated_logs = correlate_logs(logger6.entries, "trace_id")
  assert_eq(correlated_logs.length(), 1)
  assert_eq(correlated_logs[0].0, "trace-456")
  assert_eq(correlated_logs[0].1.length(), 1)  // Only one log with this trace_id
}

// Test 4: Custom Baggage and Context Propagation
test "custom baggage and context propagation mechanisms" {
  // Define baggage entry
  type BaggageEntry = {
    key: String,
    value: String,
    metadata: Option<String>
  }
  
  // Define propagation context
  type PropagationContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[BaggageEntry>,
    trace_flags: Int
  }
  
  // Define context injector
  type ContextInjector = {
    name: String,
    inject: (PropagationContext) -> Array[(String, String)]
  }
  
  // Define context extractor
  type ContextExtractor = {
    name: String,
    extract: (Array[(String, String)>) -> Option[PropagationContext>
  }
  
  // Create propagation context
  let create_context = fn(trace_id: String, span_id: String) {
    {
      trace_id,
      span_id,
      baggage: [],
      trace_flags: 1
    }
  }
  
  // Add baggage entry
  let add_baggage = fn(context: PropagationContext, key: String, value: String, metadata: Option<String>) {
    // Remove existing entry with same key
    let filtered_baggage = context.baggage.filter(fn(entry) { entry.key != key })
    
    { context | 
      baggage: filtered_baggage.push({
        key,
        value,
        metadata
      })
    }
  }
  
  // Get baggage value
  let get_baggage = fn(context: PropagationContext, key: String) {
    context.baggage.find(fn(entry) { entry.key == key })
  }
  
  // Test context creation
  let context = create_context("trace-1234567890", "span-0987654321")
  assert_eq(context.trace_id, "trace-1234567890")
  assert_eq(context.span_id, "span-0987654321")
  assert_eq(context.baggage.length(), 0)
  assert_eq(context.trace_flags, 1)
  
  // Test baggage operations
  let context1 = add_baggage(context, "user_id", "12345", None)
  let context2 = add_baggage(context1, "session_id", "abc-def", Some("propagated"))
  let context3 = add_baggage(context2, "request_id", "req-789", None)
  
  assert_eq(context3.baggage.length(), 3)
  
  let user_id = get_baggage(context3, "user_id")
  match user_id {
    Some(entry) => {
      assert_eq(entry.key, "user_id")
      assert_eq(entry.value, "12345")
      assert_eq(entry.metadata, None)
    }
    None => assert_true(false)
  }
  
  let session_id = get_baggage(context3, "session_id")
  match session_id {
    Some(entry) => {
      assert_eq(entry.key, "session_id")
      assert_eq(entry.value, "abc-def")
      assert_eq(entry.metadata, Some("propagated"))
    }
    None => assert_true(false)
  }
  
  let missing_key = get_baggage(context3, "non_existent")
  assert_eq(missing_key, None)
  
  // Test baggage update
  let context4 = add_baggage(context3, "user_id", "54321", Some("updated"))
  assert_eq(context4.baggage.length(), 3)  // Still 3, user_id updated
  
  let updated_user_id = get_baggage(context4, "user_id")
  match updated_user_id {
    Some(entry) => {
      assert_eq(entry.value, "54321")
      assert_eq(entry.metadata, Some("updated"))
    }
    None => assert_true(false)
  }
  
  // Test HTTP header injector
  let http_header_injector = {
    name: "http-header",
    inject: fn(context: PropagationContext) {
      let mut headers = []
      
      // Inject trace context
      headers = headers.push(("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-01"))
      
      // Inject baggage
      if context.baggage.length() > 0 {
        let baggage_str = context.baggage.map(fn(entry) {
          match entry.metadata {
            Some(metadata) => entry.key + "=" + entry.value + ";" + metadata
            None => entry.key + "=" + entry.value
          }
        }).join(",")
        
        headers = headers.push(("baggage", baggage_str))
      }
      
      headers
    }
  }
  
  // Test context injection
  let injected_headers = http_header_injector.inject(context4)
  assert_eq(injected_headers.length(), 2)
  
  let traceparent_header = injected_headers.find(fn(h) { h.0 == "traceparent" })
  match traceparent_header {
    Some(header) => {
      assert_eq(header.1, "00-trace-1234567890-span-0987654321-01")
    }
    None => assert_true(false)
  }
  
  let baggage_header = injected_headers.find(fn(h) { h.0 == "baggage" })
  match baggage_header {
    Some(header) => {
      assert_true(header.1.contains("user_id=54321;updated"))
      assert_true(header.1.contains("session_id=abc-def;propagated"))
      assert_true(header.1.contains("request_id=req-789"))
    }
    None => assert_true(false)
  }
  
  // Test HTTP header extractor
  let http_header_extractor = {
    name: "http-header",
    extract: fn(headers: Array<(String, String)>) {
      let traceparent = headers.find(fn(h) { h.0 == "traceparent" })
      let baggage = headers.find(fn(h) { h.0 == "baggage" })
      
      match traceparent {
        Some((_, traceparent_value)) => {
          // Parse traceparent (simplified)
          let parts = traceparent_value.split("-")
          if parts.length() >= 4 {
            let trace_id = parts[1]
            let span_id = parts[2]
            
            // Parse baggage
            let baggage_entries = match baggage {
              Some((_, baggage_value)) => {
                let entries_str = baggage_value.split(",")
                entries_str.map(fn(entry_str) {
                  let parts = entry_str.split(";")
                  let key_value = parts[0].split("=")
                  if key_value.length() >= 2 {
                    let key = key_value[0]
                    let value = key_value[1]
                    let metadata = if parts.length() > 1 {
                      Some(parts[1])
                    } else {
                      None
                    }
                    
                    {
                      key,
                      value,
                      metadata
                    }
                  } else {
                    { key: "", value: "", metadata: None }
                  }
                }).filter(fn(entry) { entry.key.length() > 0 })
              }
              None => []
            }
            
            Some({
              trace_id,
              span_id,
              baggage: baggage_entries,
              trace_flags: 1
            })
          } else {
            None
          }
        }
        None => None
      }
    }
  }
  
  // Test context extraction
  let extracted_context = http_header_extractor.extract(injected_headers)
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, "trace-1234567890")
      assert_eq(context.span_id, "span-0987654321")
      assert_eq(context.baggage.length(), 3)
      
      let extracted_user_id = get_baggage(context, "user_id")
      match extracted_user_id {
        Some(entry) => {
          assert_eq(entry.value, "54321")
          assert_eq(entry.metadata, Some("updated"))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test context merging
  let merge_contexts = fn(base: PropagationContext, override: PropagationContext) {
    let merged_baggage = base.baggage
    
    // Add or override baggage entries from override context
    let mut final_baggage = merged_baggage
    for entry in override.baggage {
      let existing_index = final_baggage.find_index(fn(e) { e.key == entry.key })
      
      match existing_index {
        Some(index) => {
          final_baggage = final_baggage.slice(0, index) + 
                           [entry] + 
                           final_baggage.slice(index + 1, final_baggage.length())
        }
        None => {
          final_baggage = final_baggage.push(entry)
        }
      }
    }
    
    {
      trace_id: base.trace_id,
      span_id: base.span_id,
      baggage: final_baggage,
      trace_flags: base.trace_flags
    }
  }
  
  // Test context merging
  let base_context = create_context("trace-1111111111", "span-1111111111")
  let base_with_baggage = add_baggage(base_context, "service", "user-service", None)
  
  let override_context = create_context("trace-2222222222", "span-2222222222")
  let override_with_baggage = add_baggage(override_context, "version", "1.2.3", None)
  let override_with_both = add_baggage(override_with_baggage, "service", "auth-service", None)
  
  let merged_context = merge_contexts(base_with_baggage, override_with_both)
  assert_eq(merged_context.trace_id, "trace-1111111111")  // Keep base trace_id
  assert_eq(merged_context.span_id, "span-1111111111")    // Keep base span_id
  assert_eq(merged_context.baggage.length(), 2)           // service + version
  
  let merged_service = get_baggage(merged_context, "service")
  match merged_service {
    Some(entry) => assert_eq(entry.value, "auth-service")  // Override value
    None => assert_true(false)
  }
  
  let merged_version = get_baggage(merged_context, "version")
  match merged_version {
    Some(entry) => assert_eq(entry.value, "1.2.3")
    None => assert_true(false)
  }
  
  // Test baggage filtering
  let filter_baggage = fn(context: PropagationContext, filter_fn: (BaggageEntry) -> Bool) {
    { context | baggage: context.baggage.filter(filter_fn) }
  }
  
  let filtered_context = filter_baggage(context4, fn(entry) {
    not(entry.key.starts_with("request"))
  })
  
  assert_eq(filtered_context.baggage.length(), 2)  // user_id and session_id remain
  assert_true(get_baggage(filtered_context, "user_id").is_some())
  assert_true(get_baggage(filtered_context, "session_id").is_some())
  assert_eq(get_baggage(filtered_context, "request_id"), None)
}

// Test 5: Custom Resource and Attribute Management
test "custom resource and attribute management for telemetry" {
  // Define resource attribute
  type ResourceAttribute = {
    key: String,
    value: String,
    description: Option<String>
  }
  
  // Define telemetry resource
  type TelemetryResource = {
    attributes: Array[ResourceAttribute>,
    schema_url: Option<String>
  }
  
  // Define instrumentation library
  type InstrumentationLibrary = {
    name: String,
    version: String,
    schema_url: Option<String>
  }
  
  // Define instrument configuration
  type InstrumentConfig = {
    name: String,
    type: String,
    description: String,
    unit: String,
    enabled: Bool
  }
  
  // Create telemetry resource
  let create_resource = fn(attributes: Array<(String, String)>) {
    let resource_attributes = attributes.map(fn(attr) {
      {
        key: attr.0,
        value: attr.1,
        description: None
      }
    })
    
    {
      attributes: resource_attributes,
      schema_url: None
    }
  }
  
  // Add resource attribute
  let add_resource_attribute = fn(resource: TelemetryResource, key: String, value: String, 
                                 description: Option<String>) {
    // Remove existing attribute with same key
    let filtered_attributes = resource.attributes.filter(fn(attr) { attr.key != key })
    
    { resource | 
      attributes: filtered_attributes.push({
        key,
        value,
        description
      })
    }
  }
  
  // Get resource attribute
  let get_resource_attribute = fn(resource: TelemetryResource, key: String) {
    resource.attributes.find(fn(attr) { attr.key == key })
  }
  
  // Test resource creation
  let resource = create_resource([
    ("service.name", "user-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production")
  ])
  
  assert_eq(resource.attributes.length(), 3)
  assert_eq(resource.attributes[0].key, "service.name")
  assert_eq(resource.attributes[0].value, "user-service")
  
  // Test attribute operations
  let resource1 = add_resource_attribute(resource, "service.instance.id", "instance-12345", None)
  let resource2 = add_resource_attribute(resource1, "host.name", "server-01", Some("Hostname of the server"))
  
  assert_eq(resource2.attributes.length(), 5)
  
  let host_attr = get_resource_attribute(resource2, "host.name")
  match host_attr {
    Some(attr) => {
      assert_eq(attr.key, "host.name")
      assert_eq(attr.value, "server-01")
      assert_eq(attr.description, Some("Hostname of the server"))
    }
    None => assert_true(false)
  }
  
  // Test attribute update
  let resource3 = add_resource_attribute(resource2, "service.version", "1.3.0", Some("Updated version"))
  assert_eq(resource3.attributes.length(), 5)  // Still 5, version updated
  
  let updated_version = get_resource_attribute(resource3, "service.version")
  match updated_version {
    Some(attr) => {
      assert_eq(attr.value, "1.3.0")
      assert_eq(attr.description, Some("Updated version"))
    }
    None => assert_true(false)
  }
  
  // Test resource filtering
  let filter_resource_attributes = fn(resource: TelemetryResource, filter_fn: (ResourceAttribute) -> Bool) {
    { resource | attributes: resource.attributes.filter(filter_fn) }
  }
  
  let service_attributes = filter_resource_attributes(resource3, fn(attr) {
    attr.key.starts_with("service.")
  })
  
  assert_eq(service_attributes.attributes.length(), 3)
  assert_true(service_attributes.attributes.any(fn(a) { a.key == "service.name" }))
  assert_true(service_attributes.attributes.any(fn(a) { a.key == "service.version" }))
  assert_true(service_attributes.attributes.any(fn(a) { a.key == "service.instance.id" }))
  
  // Test instrumentation library
  let create_instrumentation_library = fn(name: String, version: String) {
    {
      name,
      version,
      schema_url: None
    }
  }
  
  let user_lib = create_instrumentation_library("azimuth.user", "1.0.0")
  assert_eq(user_lib.name, "azimuth.user")
  assert_eq(user_lib.version, "1.0.0")
  
  // Test instrument configuration
  let create_instrument = fn(name: String, type: String, description: String, unit: String) {
    {
      name,
      type,
      description,
      unit,
      enabled: true
    }
  }
  
  let user_counter = create_instrument("user.created", "counter", 
                                       "Number of users created", "users")
  assert_eq(user_counter.name, "user.created")
  assert_eq(user_counter.type, "counter")
  assert_eq(user_counter.unit, "users")
  assert_eq(user_counter.enabled, true)
  
  // Test instrument management
  type InstrumentRegistry = {
    instruments: Array<InstrumentConfig>
  }
  
  let create_instrument_registry = fn() {
    { instruments: [] }
  }
  
  let register_instrument = fn(registry: InstrumentRegistry, instrument: InstrumentConfig) {
    let existing = registry.instruments.find_index(fn(i) { i.name == instrument.name })
    
    match existing {
      Some(index) => {
        let updated_instruments = registry.instruments.slice(0, index) + 
                                 [instrument] + 
                                 registry.instruments.slice(index + 1, registry.instruments.length())
        { registry | instruments: updated_instruments }
      }
      None => {
        { registry | instruments: registry.instruments.push(instrument) }
      }
    }
  }
  
  let instrument_registry = create_instrument_registry()
  let registry1 = register_instrument(instrument_registry, user_counter)
  
  let order_counter = create_instrument("order.placed", "counter", 
                                        "Number of orders placed", "orders")
  let registry2 = register_instrument(registry1, order_counter)
  
  assert_eq(registry2.instruments.length(), 2)
  
  // Test instrument lookup
  let get_instrument = fn(registry: InstrumentRegistry, name: String) {
    registry.instruments.find(fn(i) { i.name == name })
  }
  
  let found_instrument = get_instrument(registry2, "user.created")
  match found_instrument {
    Some(instrument) => {
      assert_eq(instrument.name, "user.created")
      assert_eq(instrument.type, "counter")
    }
    None => assert_true(false)
  }
  
  let not_found = get_instrument(registry2, "non.existent")
  assert_eq(not_found, None)
  
  // Test instrument enable/disable
  let enable_instrument = fn(registry: InstrumentRegistry, name: String, enabled: Bool) {
    let updated_instruments = registry.instruments.map(fn(instrument) {
      if instrument.name == name {
        { instrument | enabled }
      } else {
        instrument
      }
    })
    
    { registry | instruments: updated_instruments }
  }
  
  let registry3 = enable_instrument(registry2, "order.placed", false)
  
  let disabled_instrument = get_instrument(registry3, "order.placed")
  match disabled_instrument {
    Some(instrument) => assert_eq(instrument.enabled, false)
    None => assert_true(false)
  }
  
  // Test resource serialization
  let serialize_resource = fn(resource: TelemetryResource) {
    let attributes_str = resource.attributes.map(fn(attr) {
      let desc_str = match attr.description {
        Some(desc) => ",\"description\":\"" + desc + "\""
        None => ""
      }
      "{\"key\":\"" + attr.key + "\",\"value\":\"" + attr.value + "\"" + desc_str + "}"
    }).join(",")
    
    let schema_str = match resource.schema_url {
      Some(url) => ",\"schema_url\":\"" + url + "\""
      None => ""
    }
    
    "{" +
      "\"attributes\":[" + attributes_str + "]" +
      schema_str +
    "}"
  }
  
  let serialized = serialize_resource(resource3)
  assert_true(serialized.contains("\"key\":\"service.name\",\"value\":\"user-service\""))
  assert_true(serialized.contains("\"key\":\"host.name\",\"value\":\"server-01\",\"description\":\"Hostname of the server\""))
  
  // Test resource merging
  let merge_resources = fn(base: TelemetryResource, override: TelemetryResource) {
    let merged_attributes = base.attributes
    
    // Add or override attributes from override resource
    let mut final_attributes = merged_attributes
    for attr in override.attributes {
      let existing_index = final_attributes.find_index(fn(a) { a.key == attr.key })
      
      match existing_index {
        Some(index) => {
          final_attributes = final_attributes.slice(0, index) + 
                             [attr] + 
                             final_attributes.slice(index + 1, final_attributes.length())
        }
        None => {
          final_attributes = final_attributes.push(attr)
        }
      }
    }
    
    {
      attributes: final_attributes,
      schema_url: override.schema_url.or(base.schema_url)
    }
  }
  
  // Test resource merging
  let base_resource = create_resource([
    ("service.name", "base-service"),
    ("service.version", "1.0.0")
  ])
  
  let override_resource = create_resource([
    ("service.version", "2.0.0"),
    ("deployment.environment", "production")
  ])
  
  let merged = merge_resources(base_resource, override_resource)
  assert_eq(merged.attributes.length(), 3)
  
  let merged_version = get_resource_attribute(merged, "service.version")
  match merged_version {
    Some(attr) => assert_eq(attr.value, "2.0.0")  // Override value
    None => assert_true(false)
  }
  
  let merged_env = get_resource_attribute(merged, "deployment.environment")
  match merged_env {
    Some(attr) => assert_eq(attr.value, "production")
    None => assert_true(false)
  }
}