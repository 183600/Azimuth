// Azimuth高级MoonBit特性测试用例
// 专注于高级MoonBit语言特性与Azimuth遥测系统的集成

// 测试1: 高阶函数与遥测数据处理
test "higher order functions with telemetry data" {
  // 创建模拟的遥测数据
  let telemetry_data = [
    (1, "trace-001", "span-001", 100, "ok"),
    (2, "trace-001", "span-002", 150, "ok"),
    (3, "trace-002", "span-003", 50, "error"),
    (4, "trace-003", "span-004", 200, "ok"),
    (5, "trace-003", "span-005", 300, "ok")
  ]
  
  // 使用高阶函数过滤出状态为"ok"的记录
  let successful_spans = telemetry_data.filter(fn(item) {
    match item {
      (_, _, _, _, status) => status == "ok"
    }
  })
  
  assert_eq(successful_spans.length(), 4)
  
  // 使用高阶函数计算所有成功span的总持续时间
  let total_duration = successful_spans.reduce(fn(acc, item) {
    match item {
      (_, _, _, duration, _) => acc + duration
    }
  }, 0)
  
  assert_eq(total_duration, 750)
  
  // 使用高阶函数转换数据格式
  let span_summaries = successful_spans.map(fn(item) {
    match item {
      (id, trace_id, span_id, duration, _) => 
        trace_id + ":" + span_id + "=" + duration.to_string()
    }
  })
  
  assert_true(span_summaries.contains("trace-001:span-001=100"))
  assert_true(span_summaries.contains("trace-001:span-002=150"))
  assert_true(span_summaries.contains("trace-003:span-004=200"))
  assert_true(span_summaries.contains("trace-003:span-005=300"))
}

// 测试2: 函数组合与遥测管道
test "function composition with telemetry pipeline" {
  // 定义基本的遥测数据处理函数
  let normalize_timestamp = fn(data: (Int, String, String, Int, String)) {
    match data {
      (id, trace_id, span_id, duration, status) => 
        (id, trace_id, span_id, duration, status.to_lowercase())
    }
  }
  
  let calculate_throughput = fn(data: (Int, String, String, Int, String)) {
    match data {
      (id, trace_id, span_id, duration, status) => 
        (id, trace_id, span_id, duration / 10, status)  // 简化的吞吐量计算
    }
  }
  
  let add_metadata = fn(data: (Int, String, String, Int, String)) {
    match data {
      (id, trace_id, span_id, throughput, status) => 
        (id, trace_id, span_id, throughput, status + ":processed")
    }
  }
  
  // 定义函数组合函数
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  
  // 创建处理管道
  let process_pipeline = compose(add_metadata, compose(calculate_throughput, normalize_timestamp))
  
  // 测试管道处理
  let test_data = (1, "trace-001", "span-001", 100, "OK")
  let processed_data = process_pipeline(test_data)
  
  match processed_data {
    (_, _, _, throughput, status) => {
      assert_eq(throughput, 10)
      assert_eq(status, "ok:processed")
    }
  }
  
  // 测试批量处理
  let telemetry_batch = [
    (1, "trace-001", "span-001", 100, "OK"),
    (2, "trace-002", "span-002", 200, "ERROR"),
    (3, "trace-003", "span-003", 150, "OK")
  ]
  
  let processed_batch = telemetry_batch.map(process_pipeline)
  
  assert_eq(processed_batch.length(), 3)
  
  match processed_batch[0] {
    (_, _, _, throughput, status) => {
      assert_eq(throughput, 10)
      assert_eq(status, "ok:processed")
    }
  }
  
  match processed_batch[1] {
    (_, _, _, throughput, status) => {
      assert_eq(throughput, 20)
      assert_eq(status, "error:processed")
    }
  }
}

// 测试3: 递归与遥测数据结构遍历
test "recursive traversal of telemetry data structures" {
  // 模拟嵌套的遥测数据结构
  type TraceNode = {
    trace_id: String,
    span_id: String,
    duration: Int,
    children: Array[TraceNode]
  }
  
  // 创建示例跟踪树结构
  let trace_tree = {
    trace_id: "trace-001",
    span_id: "span-001",
    duration: 200,
    children: [
      {
        trace_id: "trace-001",
        span_id: "span-002",
        duration: 100,
        children: [
          {
            trace_id: "trace-001",
            span_id: "span-003",
            duration: 50,
            children: []
          },
          {
            trace_id: "trace-001",
            span_id: "span-004",
            duration: 30,
            children: []
          }
        ]
      },
      {
        trace_id: "trace-001",
        span_id: "span-005",
        duration: 80,
        children: []
      }
    ]
  }
  
  // 递归计算跟踪树的总持续时间
  let calculate_total_duration = fn(node: TraceNode) {
    let children_duration = node.children.reduce(fn(acc, child) {
      acc + calculate_total_duration(child)
    }, 0)
    node.duration + children_duration
  }
  
  let total_duration = calculate_total_duration(trace_tree)
  assert_eq(total_duration, 460)
  
  // 递归查找特定span
  let find_span = fn(node: TraceNode, target_span_id: String) {
    if node.span_id == target_span_id {
      Some(node)
    } else {
      let mut result = None
      for child in node.children {
        match find_span(child, target_span_id) {
          Some(found) => {
            result = Some(found)
            break
          }
          None => ()
        }
      }
      result
    }
  }
  
  let found_span = find_span(trace_tree, "span-003")
  match found_span {
    Some(node) => {
      assert_eq(node.span_id, "span-003")
      assert_eq(node.duration, 50)
    }
    None => assert_true(false)
  }
  
  let not_found = find_span(trace_tree, "span-999")
  match not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 递归收集所有span ID
  let collect_span_ids = fn(node: TraceNode) {
    let children_ids = node.children.reduce(fn(acc, child) {
      acc + collect_span_ids(child)
    }, [])
    [node.span_id] + children_ids
  }
  
  let all_span_ids = collect_span_ids(trace_tree)
  assert_eq(all_span_ids.length(), 5)
  assert_true(all_span_ids.contains("span-001"))
  assert_true(all_span_ids.contains("span-002"))
  assert_true(all_span_ids.contains("span-003"))
  assert_true(all_span_ids.contains("span-004"))
  assert_true(all_span_ids.contains("span-005"))
}

// 测试4: 模式匹配与遥测数据验证
test "pattern matching with telemetry data validation" {
  // 定义遥测数据类型
  type TelemetryMetric = 
    | Counter(String, Int)
    | Gauge(String, Float)
    | Histogram(String, Array[Int])
    | Summary(String, Float, Int)
  
  type TelemetryStatus = 
    | Healthy
    | Warning(String)
    | Critical(String, Int)
  
  // 创建测试指标
  let metrics = [
    Counter("request_count", 1000),
    Gauge("cpu_usage", 75.5),
    Histogram("response_time", [10, 20, 30, 40, 50]),
    Summary("memory_usage", 85.2, 1024)
  ]
  
  // 使用模式匹配验证指标
  let validate_metrics = fn(metrics: Array[TelemetryMetric]) {
    let mut issues = []
    
    for metric in metrics {
      match metric {
        Counter(name, value) => {
          if value < 0 {
            issues = issues.push("Counter " + name + " has negative value")
          }
        }
        Gauge(name, value) => {
          if value < 0.0 or value > 100.0 {
            issues = issues.push("Gauge " + name + " has invalid percentage value")
          }
        }
        Histogram(name, values) => {
          if values.length() == 0 {
            issues = issues.push("Histogram " + name + " is empty")
          }
        }
        Summary(name, value, count) => {
          if count <= 0 {
            issues = issues.push("Summary " + name + " has non-positive count")
          }
        }
      }
    }
    
    issues
  }
  
  let validation_issues = validate_metrics(metrics)
  assert_eq(validation_issues.length(), 0)
  
  // 测试无效指标
  let invalid_metrics = [
    Counter("invalid_counter", -10),
    Gauge("invalid_gauge", 150.0),
    Histogram("empty_histogram", []),
    Summary("invalid_summary", 50.0, 0)
  ]
  
  let invalid_issues = validate_metrics(invalid_metrics)
  assert_eq(invalid_issues.length(), 4)
  assert_true(invalid_issues.contains("Counter invalid_counter has negative value"))
  assert_true(invalid_issues.contains("Gauge invalid_gauge has invalid percentage value"))
  assert_true(invalid_issues.contains("Histogram empty_histogram is empty"))
  assert_true(invalid_issues.contains("Summary invalid_summary has non-positive count"))
  
  // 使用模式匹配评估系统状态
  let evaluate_system_status = fn(metrics: Array[TelemetryMetric]) {
    let mut cpu_usage = 0.0
    let mut error_count = 0
    let mut response_times = []
    
    for metric in metrics {
      match metric {
        Gauge("cpu_usage", value) => cpu_usage = value
        Counter("error_count", value) => error_count = value
        Histogram("response_time", values) => response_times = values
        _ => ()
      }
    }
    
    if cpu_usage > 90.0 or error_count > 100 {
      Critical("System overloaded", error_count)
    } else if cpu_usage > 70.0 or error_count > 50 {
      Warning("System under stress")
    } else {
      Healthy
    }
  }
  
  let health_metrics = [
    Gauge("cpu_usage", 65.5),
    Counter("error_count", 10),
    Histogram("response_time", [10, 20, 30, 40, 50])
  ]
  
  let health_status = evaluate_system_status(health_metrics)
  match health_status {
    Healthy => assert_true(true)
    _ => assert_true(false)
  }
  
  let warning_metrics = [
    Gauge("cpu_usage", 80.0),
    Counter("error_count", 60),
    Histogram("response_time", [100, 200, 300])
  ]
  
  let warning_status = evaluate_system_status(warning_metrics)
  match warning_status {
    Warning(message) => assert_eq(message, "System under stress")
    _ => assert_true(false)
  }
  
  let critical_metrics = [
    Gauge("cpu_usage", 95.0),
    Counter("error_count", 150),
    Histogram("response_time", [500, 600, 700])
  ]
  
  let critical_status = evaluate_system_status(critical_metrics)
  match critical_status {
    Critical(message, count) => {
      assert_eq(message, "System overloaded")
      assert_eq(count, 150)
    }
    _ => assert_true(false)
  }
}

// 测试5: 闭包与遥测事件处理
test "closures with telemetry event processing" {
  // 模拟事件处理器工厂
  let create_event_processor = fn(initial_count: Int) {
    let mut event_count = initial_count
    
    {
      process_event: fn(event_type: String, payload: String) {
        event_count = event_count + 1
        "Processed " + event_type + " event #" + event_count.to_string() + ": " + payload
      },
      
      get_count: fn() { event_count },
      
      reset: fn() { event_count = 0 }
    }
  }
  
  // 创建事件处理器
  let processor = create_event_processor(0)
  
  // 处理事件
  let result1 = processor.process_event("span_start", "trace-001:span-001")
  let result2 = processor.process_event("span_end", "trace-001:span-001")
  let result3 = processor.process_event("log_record", "Error: Database connection failed")
  
  assert_eq(result1, "Processed span_start event #1: trace-001:span-001")
  assert_eq(result2, "Processed span_end event #2: trace-001:span-001")
  assert_eq(result3, "Processed log_record event #3: Error: Database connection failed")
  assert_eq(processor.get_count(), 3)
  
  // 重置计数器
  processor.reset()
  assert_eq(processor.get_count(), 0)
  
  // 测试多个事件处理器
  let telemetry_processor = create_event_processor(0)
  let log_processor = create_event_processor(0)
  let metric_processor = create_event_processor(0)
  
  let telemetry_result = telemetry_processor.process_event("trace", "New trace started")
  let log_result = log_processor.process_event("log", "Application started")
  let metric_result = metric_processor.process_event("metric", "CPU usage recorded")
  
  assert_eq(telemetry_processor.get_count(), 1)
  assert_eq(log_processor.get_count(), 1)
  assert_eq(metric_processor.get_count(), 1)
  
  // 测试闭包捕获状态
  let create_filter = fn(min_level: String) {
    let levels = ["trace", "debug", "info", "warn", "error"]
    let mut level_index = 0
    
    for i in 0..levels.length() {
      if levels[i] == min_level {
        level_index = i
        break
      }
    }
    
    fn(log_level: String) {
      let mut log_level_index = 0
      for i in 0..levels.length() {
        if levels[i] == log_level {
          log_level_index = i
          break
        }
      }
      log_level_index >= level_index
    }
  }
  
  let warn_filter = create_filter("warn")
  
  assert_false(warn_filter("trace"))
  assert_false(warn_filter("debug"))
  assert_false(warn_filter("info"))
  assert_true(warn_filter("warn"))
  assert_true(warn_filter("error"))
  
  let error_filter = create_filter("error")
  
  assert_false(error_filter("trace"))
  assert_false(error_filter("debug"))
  assert_false(error_filter("info"))
  assert_false(error_filter("warn"))
  assert_true(error_filter("error"))
  
  // 使用过滤器处理日志
  let log_entries = [
    ("trace", "Detailed trace information"),
    ("info", "Application started"),
    ("warn", "Database connection slow"),
    ("error", "Database connection failed"),
    ("error", "Request processing failed")
  ]
  
  let warn_logs = log_entries.filter(fn(entry) {
    match entry {
      (level, _) => warn_filter(level)
    }
  })
  
  assert_eq(warn_logs.length(), 3)
  assert_eq(warn_logs[0], ("warn", "Database connection slow"))
  assert_eq(warn_logs[1], ("error", "Database connection failed"))
  assert_eq(warn_logs[2], ("error", "Request processing failed"))
}

// 测试6: 记录更新与不可变数据结构
test "record updates with immutable telemetry data" {
  // 定义遥测Span记录
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
  
  // 创建初始Span
  let initial_span = {
    trace_id: "trace-001",
    span_id: "span-001",
    parent_span_id: None,
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: None,
    status: "running",
    attributes: [("db.type", "postgresql"), ("db.statement", "SELECT * FROM users")]
  }
  
  // 更新Span结束时间
  let completed_span = { initial_span | 
    end_time: Some(1640995250), 
    status: "completed" 
  }
  
  assert_eq(initial_span.end_time, None)
  assert_eq(initial_span.status, "running")
  
  match completed_span.end_time {
    Some(end_time) => assert_eq(end_time, 1640995250)
    None => assert_true(false)
  }
  assert_eq(completed_span.status, "completed")
  
  // 添加子Span
  let child_span = {
    trace_id: "trace-001",
    span_id: "span-002",
    parent_span_id: Some("span-001"),
    operation_name: "cache_lookup",
    start_time: 1640995225,
    end_time: Some(1640995235),
    status: "completed",
    attributes: [("cache.type", "redis"), ("cache.key", "user:123")]
  }
  
  // 更新父Span以包含子Span引用
  let parent_with_child = { completed_span | 
    attributes: completed_span.attributes + [("child_span", "span-002")] 
  }
  
  assert_eq(parent_with_child.attributes.length(), 3)
  assert_true(parent_with_child.attributes.contains(("db.type", "postgresql")))
  assert_true(parent_with_child.attributes.contains(("db.statement", "SELECT * FROM users")))
  assert_true(parent_with_child.attributes.contains(("child_span", "span-002")))
  
  // 创建Span链
  let root_span = initial_span
  let middle_span = { root_span | 
    span_id: "span-002", 
    parent_span_id: Some("span-001"),
    operation_name: "data_processing",
    start_time: 1640995250,
    end_time: Some(1640995300),
    status: "completed",
    attributes: [("processing.type", "batch"), ("batch.size", "100")]
  }
  
  let leaf_span = { middle_span | 
    span_id: "span-003", 
    parent_span_id: Some("span-002"),
    operation_name: "result_aggregation",
    start_time: 1640995275,
    end_time: Some(1640995290),
    status: "completed",
    attributes: [("aggregation.type", "sum"), ("records.processed", "100")]
  }
  
  // 验证Span链的属性
  assert_eq(root_span.parent_span_id, None)
  assert_eq(root_span.span_id, "span-001")
  
  match middle_span.parent_span_id {
    Some(parent) => assert_eq(parent, "span-001")
    None => assert_true(false)
  }
  assert_eq(middle_span.span_id, "span-002")
  
  match leaf_span.parent_span_id {
    Some(parent) => assert_eq(parent, "span-002")
    None => assert_true(false)
  }
  assert_eq(leaf_span.span_id, "span-003")
  
  // 计算Span持续时间
  let calculate_duration = fn(span: Span) {
    match span.end_time {
      Some(end_time) => end_time - span.start_time
      None => -1  // 表示仍在运行
    }
  }
  
  assert_eq(calculate_duration(root_span), -1)
  assert_eq(calculate_duration(middle_span), 50)
  assert_eq(calculate_duration(leaf_span), 15)
  
  // 验证不可变性
  let updated_root = { root_span | status: "completed" }
  
  assert_eq(updated_root.status, "completed")
  assert_eq(root_span.status, "running")  // 原始记录未被修改
}

// 测试7: 流式处理与遥测数据聚合
test "stream processing with telemetry data aggregation" {
  // 模拟流式遥测数据
  type TelemetryEvent = 
    | SpanEvent(String, String, Int, String)  // trace_id, span_id, duration, status
    | MetricEvent(String, Float)              // metric_name, value
    | LogEvent(String, String, Int)           // log_level, message, timestamp
  
  let telemetry_stream = [
    SpanEvent("trace-001", "span-001", 100, "ok"),
    MetricEvent("cpu.usage", 25.5),
    LogEvent("info", "Application started", 1640995200),
    SpanEvent("trace-001", "span-002", 150, "ok"),
    MetricEvent("memory.usage", 45.2),
    LogEvent("warn", "High memory usage", 1640995250),
    SpanEvent("trace-002", "span-003", 200, "error"),
    MetricEvent("cpu.usage", 75.8),
    LogEvent("error", "Database connection failed", 1640995300),
    SpanEvent("trace-002", "span-004", 50, "ok"),
    MetricEvent("memory.usage", 80.1),
    LogEvent("info", "Database connection restored", 1640995350)
  ]
  
  // 流式处理函数
  let process_stream = fn(events: Array[TelemetryEvent]) {
    let mut span_count = 0
    let mut metric_count = 0
    let mut log_count = 0
    let mut total_span_duration = 0
    let mut error_spans = 0
    let mut cpu_sum = 0.0
    let mut cpu_measurements = 0
    let mut log_levels = []
    
    for event in events {
      match event {
        SpanEvent(_, _, duration, status) => {
          span_count = span_count + 1
          total_span_duration = total_span_duration + duration
          if status == "error" {
            error_spans = error_spans + 1
          }
        }
        MetricEvent(name, value) => {
          metric_count = metric_count + 1
          if name == "cpu.usage" {
            cpu_sum = cpu_sum + value
            cpu_measurements = cpu_measurements + 1
          }
        }
        LogEvent(level, _, _) => {
          log_count = log_count + 1
          if not(log_levels.contains(level)) {
            log_levels = log_levels + [level]
          }
        }
      }
    }
    
    {
      span_count,
      metric_count,
      log_count,
      average_span_duration: if span_count > 0 { total_span_duration / span_count } else { 0 },
      error_rate: if span_count > 0 { error_spans * 100 / span_count } else { 0 },
      average_cpu: if cpu_measurements > 0 { cpu_sum / cpu_measurements.to_float() } else { 0.0 },
      unique_log_levels: log_levels.length()
    }
  }
  
  let aggregation_result = process_stream(telemetry_stream)
  
  assert_eq(aggregation_result.span_count, 4)
  assert_eq(aggregation_result.metric_count, 4)
  assert_eq(aggregation_result.log_count, 4)
  assert_eq(aggregation_result.total_span_duration, 500)
  assert_eq(aggregation_result.average_span_duration, 125)
  assert_eq(aggregation_result.error_spans, 1)
  assert_eq(aggregation_result.error_rate, 25)
  assert_eq(aggregation_result.cpu_measurements, 2)
  assert_eq(aggregation_result.average_cpu, 50.65)
  assert_eq(aggregation_result.unique_log_levels, 3)
  
  // 流式过滤和转换
  let filter_and_transform = fn(events: Array[TelemetryEvent], filter_fn: (TelemetryEvent) -> Bool, transform_fn: (TelemetryEvent) -> String) {
    let mut results = []
    for event in events {
      if filter_fn(event) {
        results = results + [transform_fn(event)]
      }
    }
    results
  }
  
  // 过滤出错误事件并转换为描述字符串
  let error_events = filter_and_transform(
    telemetry_stream,
    fn(event) {
      match event {
        SpanEvent(_, _, _, status) => status == "error"
        LogEvent(level, _, _) => level == "error"
        _ => false
      }
    },
    fn(event) {
      match event {
        SpanEvent(trace_id, span_id, _, _) => 
          "Error in span " + span_id + " of trace " + trace_id
        LogEvent(_, message, _) => 
          "Error log: " + message
        _ => "Unknown error event"
      }
    }
  )
  
  assert_eq(error_events.length(), 2)
  assert_eq(error_events[0], "Error in span span-003 of trace trace-002")
  assert_eq(error_events[1], "Error log: Database connection failed")
  
  // 过滤出CPU指标事件并转换为报告字符串
  let cpu_events = filter_and_transform(
    telemetry_stream,
    fn(event) {
      match event {
        MetricEvent(name, _) => name == "cpu.usage"
        _ => false
      }
    },
    fn(event) {
      match event {
        MetricEvent(_, value) => 
          "CPU usage: " + value.to_string() + "%"
        _ => "Non-CPU metric"
      }
    }
  )
  
  assert_eq(cpu_events.length(), 2)
  assert_eq(cpu_events[0], "CPU usage: 25.5%")
  assert_eq(cpu_events[1], "CPU usage: 75.8%")
  
  // 流式窗口聚合
  let window_aggregate = fn(events: Array[TelemetryEvent], window_size: Int) {
    let mut windows = []
    
    for i in 0..events.length() {
      if i + window_size <= events.length() {
        let window = events.slice(i, i + window_size)
        let window_result = process_stream(window)
        windows = windows + [window_result]
      }
    }
    
    windows
  }
  
  let windowed_results = window_aggregate(telemetry_stream, 5)
  assert_eq(windowed_results.length(), 6)  // 10 events with window size 5 = 6 windows
  
  // 验证第一个窗口
  let first_window = windowed_results[0]
  assert_eq(first_window.span_count, 2)
  assert_eq(first_window.metric_count, 2)
  assert_eq(first_window.log_count, 1)
  
  // 验证最后一个窗口
  let last_window = windowed_results[5]
  assert_eq(last_window.span_count, 1)
  assert_eq(last_window.metric_count, 1)
  assert_eq(last_window.log_count, 2)
}

// 测试8: 泛型与遥测数据处理
test "generics with telemetry data processing" {
  // 定义泛型遥测数据容器
  type TelemetryContainer[T] = {
    data: T,
    timestamp: Int,
    metadata: Array[(String, String)]
  }
  
  // 定义泛型处理函数
  let process_container = fn[T](container: TelemetryContainer[T], processor: (T) -> String) {
    {
      processed_data: processor(container.data),
      processing_time: 1640995300,
      original_timestamp: container.timestamp
    }
  }
  
  // 创建字符串类型的遥测容器
  let string_container = {
    data: "database_query",
    timestamp: 1640995200,
    metadata: [("operation.type", "query"), ("operation.status", "success")]
  }
  
  let string_processor = fn(data: String) {
    "Processed string: " + data.to_uppercase()
  }
  
  let processed_string = process_container(string_container, string_processor)
  assert_eq(processed_string.processed_data, "Processed string: DATABASE_QUERY")
  assert_eq(processed_string.processing_time, 1640995300)
  assert_eq(processed_string.original_timestamp, 1640995200)
  
  // 创建数值类型的遥测容器
  let int_container = {
    data: 250,
    timestamp: 1640995250,
    metadata: [("metric.type", "duration"), ("metric.unit", "ms")]
  }
  
  let int_processor = fn(data: Int) {
    "Duration: " + data.to_string() + "ms (" + (data / 1000).to_string() + "s)"
  }
  
  let processed_int = process_container(int_container, int_processor)
  assert_eq(processed_int.processed_data, "Duration: 250ms (0s)")
  assert_eq(processed_int.processing_time, 1640995300)
  assert_eq(processed_int.original_timestamp, 1640995250)
  
  // 创建数组类型的遥测容器
  let array_container = {
    data: [100, 150, 200, 120, 180],
    timestamp: 1640995275,
    metadata: [("metric.type", "response_times"), ("metric.unit", "ms")]
  }
  
  let array_processor = fn(data: Array[Int]) {
    let sum = data.reduce(fn(acc, x) { acc + x }, 0)
    let avg = sum / data.length()
    let max = data.reduce(fn(acc, x) { if x > acc { x } else { acc } }, data[0])
    let min = data.reduce(fn(acc, x) { if x < acc { x } else { acc } }, data[0])
    
    "Response times - Avg: " + avg.to_string() + "ms, Min: " + min.to_string() + "ms, Max: " + max.to_string() + "ms"
  }
  
  let processed_array = process_container(array_container, array_processor)
  assert_eq(processed_array.processed_data, "Response times - Avg: 150ms, Min: 100ms, Max: 200ms")
  assert_eq(processed_array.processing_time, 1640995300)
  assert_eq(processed_array.original_timestamp, 1640995275)
  
  // 定义泛型比较函数
  let compare_containers = fn[T](a: TelemetryContainer[T], b: TelemetryContainer[T], comparator: (T, T) -> Int) {
    comparator(a.data, b.data)
  }
  
  // 使用比较函数
  let int_comparator = fn(a: Int, b: Int) {
    if a < b { -1 } else if a > b { 1 } else { 0 }
  }
  
  let container1 = { data: 100, timestamp: 1640995200, metadata: [] }
  let container2 = { data: 200, timestamp: 1640995250, metadata: [] }
  let container3 = { data: 150, timestamp: 1640995300, metadata: [] }
  
  assert_eq(compare_containers(container1, container2, int_comparator), -1)
  assert_eq(compare_containers(container2, container1, int_comparator), 1)
  assert_eq(compare_containers(container1, container1, int_comparator), 0)
  assert_eq(compare_containers(container1, container3, int_comparator), -1)
  
  // 定义泛型过滤函数
  let filter_containers = fn[T](containers: Array[TelemetryContainer[T]], predicate: (TelemetryContainer[T]) -> Bool) {
    let mut filtered = []
    for container in containers {
      if predicate(container) {
        filtered = filtered + [container]
      }
    }
    filtered
  }
  
  // 使用过滤函数
  let containers = [
    { data: 100, timestamp: 1640995200, metadata: [("type", "metric")] },
    { data: 200, timestamp: 1640995250, metadata: [("type", "span")] },
    { data: 150, timestamp: 1640995300, metadata: [("type", "metric")] },
    { data: 300, timestamp: 1640995350, metadata: [("type", "log")] },
    { data: 250, timestamp: 1640995400, metadata: [("type", "metric")] }
  ]
  
  let metric_containers = filter_containers(containers, fn(container) {
    container.metadata.contains(("type", "metric"))
  })
  
  assert_eq(metric_containers.length(), 3)
  assert_eq(metric_containers[0].data, 100)
  assert_eq(metric_containers[1].data, 150)
  assert_eq(metric_containers[2].data, 250)
  
  // 定义泛型聚合函数
  let aggregate_containers = fn[T](containers: Array[TelemetryContainer[T]], aggregator: (Array[T]) -> T) {
    let data_values = containers.map(fn(container) { container.data })
    aggregator(data_values)
  }
  
  // 使用聚合函数
  let sum_aggregator = fn(values: Array[Int]) {
    values.reduce(fn(acc, x) { acc + x }, 0)
  }
  
  let total_value = aggregate_containers(metric_containers, sum_aggregator)
  assert_eq(total_value, 500)  // 100 + 150 + 250
  
  let avg_aggregator = fn(values: Array[Int]) {
    let sum = values.reduce(fn(acc, x) { acc + x }, 0)
    sum / values.length()
  }
  
  let average_value = aggregate_containers(metric_containers, avg_aggregator)
  assert_eq(average_value, 166)  // 500 / 3 = 166.67，整数除法截断为166
}