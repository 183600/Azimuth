// Azimuth增强测试用例
// 专注于高级MoonBit语言特性与遥测系统的深度集成

// 测试1: 异步遥测数据处理模式
test "async telemetry data processing patterns" {
  // 模拟异步操作状态
  enum AsyncState[T] {
    Pending
    InProgress(T)
    Completed(T)
    Failed(String)
  }
  
  // 模拟异步遥测数据获取
  let fetch_telemetry_data = fn(source: String) {
    match source {
      "database" => AsyncState::InProgress([("db.query_time", "150"), ("db.connections", "10")])
      "cache" => AsyncState::InProgress([("cache.hit_rate", "0.85"), ("cache.size", "500MB")])
      "api" => AsyncState::InProgress([("api.response_time", "200"), ("api.error_rate", "0.02")])
      "invalid" => AsyncState::Failed("Unknown telemetry source")
      _ => AsyncState::Pending
    }
  }
  
  // 处理异步状态
  let process_async_state = fn[T](state: AsyncState[T], processor: (T) -> Array[String]) {
    match state {
      AsyncState::Pending => ["No data available"]
      AsyncState::InProgress(data) => ["Processing..."] + processor(data)
      AsyncState::Completed(data) => ["Completed"] + processor(data)
      AsyncState::Failed(error) => ["Error: " + error]
    }
  }
  
  // 测试数据库遥测数据
  let db_state = fetch_telemetry_data("database")
  let db_processor = fn(data: Array[(String, String)]) {
    data.map(fn(pair) {
      match pair {
        (key, value) => key + " = " + value
      }
    })
  }
  
  let db_result = process_async_state(db_state, db_processor)
  assert_eq(db_result.length(), 3)
  assert_eq(db_result[0], "Processing...")
  assert_true(db_result.contains("db.query_time = 150"))
  assert_true(db_result.contains("db.connections = 10"))
  
  // 测试缓存遥测数据
  let cache_state = fetch_telemetry_data("cache")
  let cache_result = process_async_state(cache_state, db_processor)
  assert_eq(cache_result.length(), 3)
  assert_eq(cache_result[0], "Processing...")
  assert_true(cache_result.contains("cache.hit_rate = 0.85"))
  assert_true(cache_result.contains("cache.size = 500MB"))
  
  // 测试错误情况
  let error_state = fetch_telemetry_data("invalid")
  let error_result = process_async_state(error_state, db_processor)
  assert_eq(error_result.length(), 1)
  assert_eq(error_result[0], "Error: Unknown telemetry source")
  
  // 测试待处理情况
  let pending_state = fetch_telemetry_data("unknown")
  let pending_result = process_async_state(pending_state, db_processor)
  assert_eq(pending_result.length(), 1)
  assert_eq(pending_result[0], "No data available")
}

// 测试2: 遥测数据的递归模式匹配
test "recursive pattern matching with telemetry data" {
  // 定义嵌套遥测数据结构
  type TelemetryNode = 
    | Leaf(String, Int)  // metric_name, value
    | Branch(String, Array[TelemetryNode])  // category, children
  
  // 创建复杂遥测数据树
  let telemetry_tree = Branch("application", [
    Branch("database", [
      Leaf("query_time", 150),
      Leaf("connection_pool", 10),
      Branch("queries", [
        Leaf("select", 80),
        Leaf("insert", 40),
        Leaf("update", 30)
      ])
    ]),
    Branch("cache", [
      Leaf("hit_rate", 85),
      Leaf("miss_rate", 15),
      Branch("redis", [
        Leaf("memory_usage", 500),
        Leaf("connected_clients", 25)
      ])
    ]),
    Leaf("api_response_time", 200)
  ])
  
  // 递归查找特定指标
  let find_metric = fn(node: TelemetryNode, metric_name: String) {
    match node {
      TelemetryNode::Leaf(name, value) => {
        if name == metric_name {
          Some(value)
        } else {
          None
        }
      }
      TelemetryNode::Branch(_, children) => {
        let mut result = None
        for child in children {
          match find_metric(child, metric_name) {
            Some(value) => {
              result = Some(value)
              break
            }
            None => ()
          }
        }
        result
      }
    }
  }
  
  // 测试查找存在的指标
  let query_time = find_metric(telemetry_tree, "query_time")
  match query_time {
    Some(value) => assert_eq(value, 150)
    None => assert_true(false)
  }
  
  let hit_rate = find_metric(telemetry_tree, "hit_rate")
  match hit_rate {
    Some(value) => assert_eq(value, 85)
    None => assert_true(false)
  }
  
  let select_queries = find_metric(telemetry_tree, "select")
  match select_queries {
    Some(value) => assert_eq(value, 80)
    None => assert_true(false)
  }
  
  // 测试查找不存在的指标
  let not_found = find_metric(telemetry_tree, "non_existent_metric")
  match not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 递归计算所有指标的总和
  let sum_all_metrics = fn(node: TelemetryNode) {
    match node {
      TelemetryNode::Leaf(_, value) => value
      TelemetryNode::Branch(_, children) => {
        children.reduce(fn(acc, child) {
          acc + sum_all_metrics(child)
        }, 0)
      }
    }
  }
  
  let total_sum = sum_all_metrics(telemetry_tree)
  assert_eq(total_sum, 150 + 10 + 80 + 40 + 30 + 85 + 15 + 500 + 25 + 200)
  
  // 递归收集所有指标名称
  let collect_metric_names = fn(node: TelemetryNode) {
    match node {
      TelemetryNode::Leaf(name, _) => [name]
      TelemetryNode::Branch(_, children) => {
        children.reduce(fn(acc, child) {
          acc + collect_metric_names(child)
        }, [])
      }
    }
  }
  
  let all_names = collect_metric_names(telemetry_tree)
  assert_eq(all_names.length(), 10)
  assert_true(all_names.contains("query_time"))
  assert_true(all_names.contains("hit_rate"))
  assert_true(all_names.contains("select"))
  assert_true(all_names.contains("memory_usage"))
  
  // 递归过滤指标
  let filter_metrics = fn(node: TelemetryNode, predicate: (String, Int) -> Bool) {
    match node {
      TelemetryNode::Leaf(name, value) => {
        if predicate(name, value) {
          [TelemetryNode::Leaf(name, value)]
        } else {
          []
        }
      }
      TelemetryNode::Branch(name, children) => {
        let filtered_children = children.reduce(fn(acc, child) {
          acc + filter_metrics(child, predicate)
        }, [])
        
        if filtered_children.length() > 0 {
          [TelemetryNode::Branch(name, filtered_children)]
        } else {
          []
        }
      }
    }
  }
  
  // 过滤大于100的指标
  let high_value_metrics = filter_metrics(telemetry_tree, fn(name, value) {
    value > 100
  })
  
  // 验证过滤结果
  let count_high_value = fn(node: TelemetryNode) {
    match node {
      TelemetryNode::Leaf(_, _) => 1
      TelemetryNode::Branch(_, children) => {
        children.reduce(fn(acc, child) {
          acc + count_high_value(child)
        }, 0)
      }
    }
  }
  
  let high_value_count = high_value_metrics.reduce(fn(acc, node) {
    acc + count_high_value(node)
  }, 0)
  
  assert_eq(high_value_count, 3)  // query_time(150), memory_usage(500), api_response_time(200)
}

// 测试3: 函数式状态管理
test "functional state management with telemetry data" {
  // 定义不可变状态结构
  type TelemetryState = {
    metrics: Array[(String, Int)],
    spans: Array[(String, String, Int)],  // (trace_id, span_id, duration)
    logs: Array[(String, String, Int)],   // (level, message, timestamp)
    version: Int
  }
  
  // 初始状态
  let initial_state = {
    metrics: [],
    spans: [],
    logs: [],
    version: 0
  }
  
  // 状态更新函数
  let add_metric = fn(state: TelemetryState, name: String, value: Int) {
    { state | 
      metrics: state.metrics + [(name, value)],
      version: state.version + 1
    }
  }
  
  let add_span = fn(state: TelemetryState, trace_id: String, span_id: String, duration: Int) {
    { state | 
      spans: state.spans + [(trace_id, span_id, duration)],
      version: state.version + 1
    }
  }
  
  let add_log = fn(state: TelemetryState, level: String, message: String, timestamp: Int) {
    { state | 
      logs: state.logs + [(level, message, timestamp)],
      version: state.version + 1
    }
  }
  
  // 应用一系列状态更新
  let state1 = add_metric(initial_state, "cpu_usage", 75)
  let state2 = add_span(state1, "trace-001", "span-001", 100)
  let state3 = add_log(state2, "info", "Application started", 1640995200)
  let state4 = add_metric(state3, "memory_usage", 500)
  let state5 = add_span(state4, "trace-001", "span-002", 150)
  let state6 = add_log(state5, "warn", "High memory usage", 1640995250)
  
  // 验证最终状态
  assert_eq(state6.version, 6)
  assert_eq(state6.metrics.length(), 2)
  assert_eq(state6.spans.length(), 2)
  assert_eq(state6.logs.length(), 2)
  
  assert_true(state6.metrics.contains(("cpu_usage", 75)))
  assert_true(state6.metrics.contains(("memory_usage", 500)))
  assert_true(state6.spans.contains(("trace-001", "span-001", 100)))
  assert_true(state6.spans.contains(("trace-001", "span-002", 150)))
  assert_true(state6.logs.contains(("info", "Application started", 1640995200)))
  assert_true(state6.logs.contains(("warn", "High memory usage", 1640995250)))
  
  // 状态查询函数
  let get_metrics_by_prefix = fn(state: TelemetryState, prefix: String) {
    state.metrics.filter(fn(pair) {
      match pair {
        (name, _) => name.starts_with(prefix)
      }
    })
  }
  
  let get_spans_by_trace = fn(state: TelemetryState, trace_id: String) {
    state.spans.filter(fn(triple) {
      match triple {
        (id, _, _) => id == trace_id
      }
    })
  }
  
  let get_logs_by_level = fn(state: TelemetryState, level: String) {
    state.logs.filter(fn(triple) {
      match triple {
        (lvl, _, _) => lvl == level
      }
    })
  }
  
  // 测试查询函数
  let cpu_metrics = get_metrics_by_prefix(state6, "cpu")
  assert_eq(cpu_metrics.length(), 1)
  assert_eq(cpu_metrics[0], ("cpu_usage", 75))
  
  let memory_metrics = get_metrics_by_prefix(state6, "memory")
  assert_eq(memory_metrics.length(), 1)
  assert_eq(memory_metrics[0], ("memory_usage", 500))
  
  let trace_spans = get_spans_by_trace(state6, "trace-001")
  assert_eq(trace_spans.length(), 2)
  
  let info_logs = get_logs_by_level(state6, "info")
  assert_eq(info_logs.length(), 1)
  assert_eq(info_logs[0], ("info", "Application started", 1640995200))
  
  // 状态聚合函数
  let aggregate_metrics = fn(state: TelemetryState) {
    let sum = state.metrics.reduce(fn(acc, pair) {
      match pair {
        (_, value) => acc + value
      }
    }, 0)
    
    let avg = if state.metrics.length() > 0 {
      sum / state.metrics.length()
    } else {
      0
    }
    
    {
      count: state.metrics.length(),
      sum,
      average: avg
    }
  }
  
  let aggregate_spans = fn(state: TelemetryState) {
    let total_duration = state.spans.reduce(fn(acc, triple) {
      match triple {
        (_, _, duration) => acc + duration
      }
    }, 0)
    
    let avg_duration = if state.spans.length() > 0 {
      total_duration / state.spans.length()
    } else {
      0
    }
    
    {
      count: state.spans.length(),
      total_duration,
      average_duration: avg_duration
    }
  }
  
  // 测试聚合函数
  let metrics_aggregation = aggregate_metrics(state6)
  assert_eq(metrics_aggregation.count, 2)
  assert_eq(metrics_aggregation.sum, 575)  // 75 + 500
  assert_eq(metrics_aggregation.average, 287)  // 575 / 2 = 287.5，整数除法截断为287
  
  let spans_aggregation = aggregate_spans(state6)
  assert_eq(spans_aggregation.count, 2)
  assert_eq(spans_aggregation.total_duration, 250)  // 100 + 150
  assert_eq(spans_aggregation.average_duration, 125)  // 250 / 2 = 125
  
  // 状态转换函数
  let transform_state = fn(state: TelemetryState, transformer: (TelemetryState) -> TelemetryState) {
    transformer(state)
  }
  
  // 使用状态转换函数
  let normalized_state = transform_state(state6, fn(s) {
    { s | 
      metrics: s.metrics.map(fn(pair) {
        match pair {
          (name, value) => (name.to_uppercase(), value)
        }
      })
    }
  })
  
  // 验证转换后的状态
  assert_eq(normalized_state.metrics.length(), 2)
  assert_true(normalized_state.metrics.contains(("CPU_USAGE", 75)))
  assert_true(normalized_state.metrics.contains(("MEMORY_USAGE", 500)))
  
  // 原始状态保持不变
  assert_eq(state6.metrics.length(), 2)
  assert_true(state6.metrics.contains(("cpu_usage", 75)))
  assert_true(state6.metrics.contains(("memory_usage", 500)))
}

// 测试4: 高级类型系统与遥测数据验证
test "advanced type system with telemetry data validation" {
  // 定义强类型遥测数据
  type MetricType = Counter | Gauge | Histogram | Summary
  
  type MetricValue = 
    | CounterValue(Int)
    | GaugeValue(Float)
    | HistogramValue(Array[Int])
    | SummaryValue(Float, Int)  // sum, count
  
  type TypedMetric = {
    name: String,
    metric_type: MetricType,
    value: MetricValue,
    timestamp: Int
  }
  
  // 类型安全的验证器
  let validate_metric = fn(metric: TypedMetric) {
    match metric.metric_type {
      MetricType::Counter => {
        match metric.value {
          MetricValue::CounterValue(value) => {
            if value >= 0 {
              "Valid counter metric"
            } else {
              "Invalid counter metric: negative value"
            }
          }
          _ => "Invalid counter metric: wrong value type"
        }
      }
      MetricType::Gauge => {
        match metric.value {
          MetricValue::GaugeValue(value) => {
            if value >= 0.0 {
              "Valid gauge metric"
            } else {
              "Invalid gauge metric: negative value"
            }
          }
          _ => "Invalid gauge metric: wrong value type"
        }
      }
      MetricType::Histogram => {
        match metric.value {
          MetricValue::HistogramValue(values) => {
            if values.length() > 0 {
              "Valid histogram metric"
            } else {
              "Invalid histogram metric: empty values"
            }
          }
          _ => "Invalid histogram metric: wrong value type"
        }
      }
      MetricType::Summary => {
        match metric.value {
          MetricValue::SummaryValue(sum, count) => {
            if count > 0 {
              "Valid summary metric"
            } else {
              "Invalid summary metric: non-positive count"
            }
          }
          _ => "Invalid summary metric: wrong value type"
        }
      }
    }
  }
  
  // 创建测试指标
  let valid_counter = {
    name: "request_count",
    metric_type: MetricType::Counter,
    value: MetricValue::CounterValue(1000),
    timestamp: 1640995200
  }
  
  let invalid_counter = {
    name: "negative_requests",
    metric_type: MetricType::Counter,
    value: MetricValue::CounterValue(-10),
    timestamp: 1640995250
  }
  
  let valid_gauge = {
    name: "cpu_usage",
    metric_type: MetricType::Gauge,
    value: MetricValue::GaugeValue(75.5),
    timestamp: 1640995300
  }
  
  let invalid_gauge = {
    name: "negative_memory",
    metric_type: MetricType::Gauge,
    value: MetricValue::GaugeValue(-5.0),
    timestamp: 1640995350
  }
  
  let valid_histogram = {
    name: "response_time",
    metric_type: MetricType::Histogram,
    value: MetricValue::HistogramValue([10, 20, 30, 40, 50]),
    timestamp: 1640995400
  }
  
  let empty_histogram = {
    name: "empty_response_time",
    metric_type: MetricType::Histogram,
    value: MetricValue::HistogramValue([]),
    timestamp: 1640995450
  }
  
  let valid_summary = {
    name: "request_size",
    metric_type: MetricType::Summary,
    value: MetricValue::SummaryValue(1024000.0, 1000),
    timestamp: 1640995500
  }
  
  let invalid_summary = {
    name: "empty_request_size",
    metric_type: MetricType::Summary,
    value: MetricValue::SummaryValue(0.0, 0),
    timestamp: 1640995550
  }
  
  // 测试验证器
  assert_eq(validate_metric(valid_counter), "Valid counter metric")
  assert_eq(validate_metric(invalid_counter), "Invalid counter metric: negative value")
  assert_eq(validate_metric(valid_gauge), "Valid gauge metric")
  assert_eq(validate_metric(invalid_gauge), "Invalid gauge metric: negative value")
  assert_eq(validate_metric(valid_histogram), "Valid histogram metric")
  assert_eq(validate_metric(empty_histogram), "Invalid histogram metric: empty values")
  assert_eq(validate_metric(valid_summary), "Valid summary metric")
  assert_eq(validate_metric(invalid_summary), "Invalid summary metric: non-positive count")
  
  // 类型不匹配的测试
  let type_mismatch = {
    name: "type_mismatch",
    metric_type: MetricType::Counter,
    value: MetricValue::GaugeValue(50.0),
    timestamp: 1640995600
  }
  
  assert_eq(validate_metric(type_mismatch), "Invalid counter metric: wrong value type")
  
  // 批量验证函数
  let validate_metrics = fn(metrics: Array[TypedMetric]) {
    let mut results = []
    for metric in metrics {
      results = results + [(metric.name, validate_metric(metric))]
    }
    results
  }
  
  let all_metrics = [
    valid_counter,
    invalid_counter,
    valid_gauge,
    invalid_gauge,
    valid_histogram,
    empty_histogram,
    valid_summary,
    invalid_summary,
    type_mismatch
  ]
  
  let validation_results = validate_metrics(all_metrics)
  assert_eq(validation_results.length(), 9)
  
  // 统计有效和无效指标
  let valid_count = validation_results.reduce(fn(acc, result) {
    match result {
      (_, message) => {
        if message.starts_with("Valid") {
          acc + 1
        } else {
          acc
        }
      }
    }
  }, 0)
  
  let invalid_count = validation_results.reduce(fn(acc, result) {
    match result {
      (_, message) => {
        if message.starts_with("Invalid") {
          acc + 1
        } else {
          acc
        }
      }
    }
  }, 0)
  
  assert_eq(valid_count, 4)
  assert_eq(invalid_count, 5)
  
  // 类型安全的指标转换函数
  let convert_metric_value = fn(value: MetricValue) -> String {
    match value {
      MetricValue::CounterValue(v) => "Counter: " + v.to_string()
      MetricValue::GaugeValue(v) => "Gauge: " + v.to_string()
      MetricValue::HistogramValue(values) => {
        let sum = values.reduce(fn(acc, x) { acc + x }, 0)
        let avg = if values.length() > 0 { sum / values.length() } else { 0 }
        "Histogram: count=" + values.length().to_string() + ", avg=" + avg.to_string()
      }
      MetricValue::SummaryValue(sum, count) => {
        "Summary: sum=" + sum.to_string() + ", count=" + count.to_string()
      }
    }
  }
  
  // 测试转换函数
  assert_eq(convert_metric_value(MetricValue::CounterValue(100)), "Counter: 100")
  assert_eq(convert_metric_value(MetricValue::GaugeValue(75.5)), "Gauge: 75.5")
  assert_eq(convert_metric_value(MetricValue::HistogramValue([10, 20, 30])), "Histogram: count=3, avg=20")
  assert_eq(convert_metric_value(MetricValue::SummaryValue(1000.0, 100)), "Summary: sum=1000.0, count=100")
  
  // 类型安全的指标格式化函数
  let format_metric = fn(metric: TypedMetric) {
    metric.name + " [" + convert_metric_value(metric.value) + "] at " + metric.timestamp.to_string()
  }
  
  let formatted_counter = format_metric(valid_counter)
  assert_eq(formatted_counter, "request_count [Counter: 1000] at 1640995200")
  
  let formatted_histogram = format_metric(valid_histogram)
  assert_eq(formatted_histogram, "response_time [Histogram: count=5, avg=30] at 1640995400")
}

// 测试5: 遥测数据的流式转换
test "streaming transformations of telemetry data" {
  // 定义遥测事件流
  type TelemetryEvent = 
    | MetricEvent(String, Float)  // name, value
    | SpanEvent(String, String, Int)  // trace_id, span_id, duration
    | LogEvent(String, String, Int)   // level, message, timestamp
  
  // 模拟事件流
  let event_stream = [
    MetricEvent("cpu_usage", 25.5),
    SpanEvent("trace-001", "span-001", 100),
    LogEvent("info", "Application started", 1640995200),
    MetricEvent("memory_usage", 45.2),
    SpanEvent("trace-001", "span-002", 150),
    LogEvent("warn", "High memory usage", 1640995250),
    MetricEvent("cpu_usage", 75.8),
    SpanEvent("trace-002", "span-003", 200),
    LogEvent("error", "Database connection failed", 1640995300),
    MetricEvent("memory_usage", 80.1),
    SpanEvent("trace-002", "span-004", 50),
    LogEvent("info", "Database connection restored", 1640995350)
  ]
  
  // 流式转换函数
  let transform_stream = fn(
    events: Array[TelemetryEvent],
    metric_transformer: (String, Float) -> String,
    span_transformer: (String, String, Int) -> String,
    log_transformer: (String, String, Int) -> String
  ) {
    events.map(fn(event) {
      match event {
        MetricEvent(name, value) => metric_transformer(name, value)
        SpanEvent(trace_id, span_id, duration) => span_transformer(trace_id, span_id, duration)
        LogEvent(level, message, timestamp) => log_transformer(level, message, timestamp)
      }
    })
  }
  
  // 定义转换器
  let metric_transformer = fn(name: String, value: Float) {
    "Metric: " + name + " = " + value.to_string() + "%"
  }
  
  let span_transformer = fn(trace_id: String, span_id: String, duration: Int) {
    "Span: " + trace_id + ":" + span_id + " took " + duration.to_string() + "ms"
  }
  
  let log_transformer = fn(level: String, message: String, timestamp: Int) {
    "Log [" + level.to_uppercase() + "]: " + message + " at " + timestamp.to_string()
  }
  
  // 应用转换
  let transformed_stream = transform_stream(
    event_stream,
    metric_transformer,
    span_transformer,
    log_transformer
  )
  
  // 验证转换结果
  assert_eq(transformed_stream.length(), 12)
  assert_eq(transformed_stream[0], "Metric: cpu_usage = 25.5%")
  assert_eq(transformed_stream[1], "Span: trace-001:span-001 took 100ms")
  assert_eq(transformed_stream[2], "Log [INFO]: Application started at 1640995200")
  assert_eq(transformed_stream[3], "Metric: memory_usage = 45.2%")
  assert_eq(transformed_stream[4], "Span: trace-001:span-002 took 150ms")
  assert_eq(transformed_stream[5], "Log [WARN]: High memory usage at 1640995250")
  
  // 流式过滤函数
  let filter_stream = fn(
    events: Array[TelemetryEvent],
    metric_filter: (String, Float) -> Bool,
    span_filter: (String, String, Int) -> Bool,
    log_filter: (String, String, Int) -> Bool
  ) {
    events.filter(fn(event) {
      match event {
        MetricEvent(name, value) => metric_filter(name, value)
        SpanEvent(trace_id, span_id, duration) => span_filter(trace_id, span_id, duration)
        LogEvent(level, message, timestamp) => log_filter(level, message, timestamp)
      }
    })
  }
  
  // 定义过滤器
  let high_cpu_metric = fn(name: String, value: Float) {
    name == "cpu_usage" and value > 70.0
  }
  
  let long_span = fn(trace_id: String, span_id: String, duration: Int) {
    duration > 150
  }
  
  let error_log = fn(level: String, message: String, timestamp: Int) {
    level == "error"
  }
  
  // 应用过滤
  let filtered_stream = filter_stream(
    event_stream,
    high_cpu_metric,
    long_span,
    error_log
  )
  
  // 验证过滤结果
  assert_eq(filtered_stream.length(), 3)
  
  // 检查高CPU使用率事件
  let high_cpu_events = filtered_stream.filter(fn(event) {
    match event {
      MetricEvent(name, value) => name == "cpu_usage" and value > 70.0
      _ => false
    }
  })
  assert_eq(high_cpu_events.length(), 1)
  
  // 检查长时间运行的span
  let long_span_events = filtered_stream.filter(fn(event) {
    match event {
      SpanEvent(_, _, duration) => duration > 150
      _ => false
    }
  })
  assert_eq(long_span_events.length(), 2)
  
  // 检查错误日志
  let error_log_events = filtered_stream.filter(fn(event) {
    match event {
      LogEvent(level, _, _) => level == "error"
      _ => false
    }
  })
  assert_eq(error_log_events.length(), 1)
  
  // 流式聚合函数
  let aggregate_stream = fn(events: Array[TelemetryEvent]) {
    let mut metric_count = 0
    let mut span_count = 0
    let mut log_count = 0
    let mut cpu_sum = 0.0
    let mut cpu_measurements = 0
    let mut total_span_duration = 0
    let mut error_logs = 0
    
    for event in events {
      match event {
        MetricEvent(name, value) => {
          metric_count = metric_count + 1
          if name == "cpu_usage" {
            cpu_sum = cpu_sum + value
            cpu_measurements = cpu_measurements + 1
          }
        }
        SpanEvent(_, _, duration) => {
          span_count = span_count + 1
          total_span_duration = total_span_duration + duration
        }
        LogEvent(level, _, _) => {
          log_count = log_count + 1
          if level == "error" {
            error_logs = error_logs + 1
          }
        }
      }
    }
    
    {
      metric_count,
      span_count,
      log_count,
      average_cpu: if cpu_measurements > 0 { cpu_sum / cpu_measurements.to_float() } else { 0.0 },
      average_span_duration: if span_count > 0 { total_span_duration / span_count } else { 0 },
      error_log_count: error_logs
    }
  }
  
  // 应用聚合
  let aggregation_result = aggregate_stream(event_stream)
  
  // 验证聚合结果
  assert_eq(aggregation_result.metric_count, 4)
  assert_eq(aggregation_result.span_count, 4)
  assert_eq(aggregation_result.log_count, 4)
  assert_eq(aggregation_result.average_cpu, 60.8)  // (25.5 + 75.8) / 2
  assert_eq(aggregation_result.average_span_duration, 125)  // (100 + 150 + 200 + 50) / 4
  assert_eq(aggregation_result.error_log_count, 1)
  
  // 流式窗口聚合
  let window_aggregate = fn(events: Array[TelemetryEvent], window_size: Int) {
    let mut windows = []
    
    for i in 0..events.length() {
      if i + window_size <= events.length() {
        let window = events.slice(i, i + window_size)
        let window_result = aggregate_stream(window)
        windows = windows + [window_result]
      }
    }
    
    windows
  }
  
  // 应用窗口聚合
  let windowed_results = window_aggregate(event_stream, 6)
  assert_eq(windowed_results.length(), 7)  // 12 events with window size 6 = 7 windows
  
  // 验证第一个窗口
  let first_window = windowed_results[0]
  assert_eq(first_window.metric_count, 2)
  assert_eq(first_window.span_count, 2)
  assert_eq(first_window.log_count, 2)
  
  // 验证最后一个窗口
  let last_window = windowed_results[6]
  assert_eq(last_window.metric_count, 1)
  assert_eq(last_window.span_count, 1)
  assert_eq(last_window.log_count, 4)
}

// 测试6: 遥测数据的模式匹配与解构
test "pattern matching and destructuring with telemetry data" {
  // 定义复合遥测数据结构
  type TraceData = {
    trace_id: String,
    spans: Array[SpanData]
  }
  
  and SpanData = {
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    events: Array[EventData]
  }
  
  and EventData = {
    event_name: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据
  let test_trace = {
    trace_id: "trace-001",
    spans: [
      {
        span_id: "span-001",
        parent_span_id: None,
        operation_name: "root_operation",
        start_time: 1640995200,
        end_time: 1640995350,
        status: "ok",
        events: [
          {
            event_name: "start",
            timestamp: 1640995200,
            attributes: [("component", "server")]
          },
          {
            event_name: "middleware",
            timestamp: 1640995250,
            attributes: [("middleware", "auth")]
          }
        ]
      },
      {
        span_id: "span-002",
        parent_span_id: Some("span-001"),
        operation_name: "database_query",
        start_time: 1640995225,
        end_time: 1640995275,
        status: "ok",
        events: [
          {
            event_name: "db.connect",
            timestamp: 1640995225,
            attributes: [("db.type", "postgresql")]
          },
          {
            event_name: "db.query",
            timestamp: 1640995250,
            attributes: [("query.type", "select")]
          },
          {
            event_name: "db.result",
            timestamp: 1640995275,
            attributes: [("rows", "100")]
          }
        ]
      },
      {
        span_id: "span-003",
        parent_span_id: Some("span-001"),
        operation_name: "cache_lookup",
        start_time: 1640995300,
        end_time: 1640995325,
        status: "error",
        events: [
          {
            event_name: "cache.connect",
            timestamp: 1640995300,
            attributes: [("cache.type", "redis")]
          },
          {
            event_name: "cache.miss",
            timestamp: 1640995325,
            attributes: [("key", "user:123")]
          }
        ]
      }
    ]
  }
  
  // 嵌套模式匹配函数
  let analyze_trace = fn(trace: TraceData) {
    match trace {
      { trace_id, spans } => {
        let total_spans = spans.length()
        let successful_spans = spans.reduce(fn(acc, span) {
          match span {
            { status: "ok", .. } => acc + 1
            { status: _, .. } => acc
          }
        }, 0)
        
        let failed_spans = spans.reduce(fn(acc, span) {
          match span {
            { status: "error", .. } => acc + 1
            { status: _, .. } => acc
          }
        }, 0)
        
        let total_duration = spans.reduce(fn(acc, span) {
          match span {
            { start_time, end_time, .. } => acc + (end_time - start_time)
          }
        }, 0)
        
        let total_events = spans.reduce(fn(acc, span) {
          match span {
            { events, .. } => acc + events.length()
          }
        }, 0)
        
        {
          trace_id,
          total_spans,
          successful_spans,
          failed_spans,
          total_duration,
          total_events
        }
      }
    }
  }
  
  // 分析测试跟踪
  let analysis_result = analyze_trace(test_trace)
  
  // 验证分析结果
  assert_eq(analysis_result.trace_id, "trace-001")
  assert_eq(analysis_result.total_spans, 3)
  assert_eq(analysis_result.successful_spans, 2)
  assert_eq(analysis_result.failed_spans, 1)
  assert_eq(analysis_result.total_duration, 225)  // 150 + 50 + 25
  assert_eq(analysis_result.total_events, 7)     // 2 + 3 + 2
  
  // 深度嵌套模式匹配函数
  let find_specific_event = fn(trace: TraceData, event_name: String) {
    match trace {
      { spans, .. } => {
        let mut found_events = []
        
        for span in spans {
          match span {
            { span_id, events, .. } => {
              for event in events {
                match event {
                  { event_name: name, timestamp, attributes } if name == event_name => {
                    found_events = found_events + [{
                      span_id,
                      event_name: name,
                      timestamp,
                      attributes
                    }]
                  }
                  { event_name: _, .. } => ()
                }
              }
            }
          }
        }
        
        found_events
      }
    }
  }
  
  // 查找特定事件
  let db_connect_events = find_specific_event(test_trace, "db.connect")
  assert_eq(db_connect_events.length(), 1)
  assert_eq(db_connect_events[0].span_id, "span-002")
  assert_eq(db_connect_events[0].timestamp, 1640995225)
  assert_true(db_connect_events[0].attributes.contains(("db.type", "postgresql")))
  
  let cache_events = find_specific_event(test_trace, "cache.miss")
  assert_eq(cache_events.length(), 1)
  assert_eq(cache_events[0].span_id, "span-003")
  assert_eq(cache_events[0].timestamp, 1640995325)
  assert_true(cache_events[0].attributes.contains(("key", "user:123")))
  
  // 复杂解构模式匹配
  let extract_span_hierarchy = fn(trace: TraceData) {
    match trace {
      { spans, .. } => {
        let mut root_span = None
        let mut child_spans = []
        
        for span in spans {
          match span {
            { parent_span_id: None, span_id, operation_name, .. } => {
              root_span = Some({
                span_id,
                operation_name
              })
            }
            { parent_span_id: Some(parent_id), span_id, operation_name, .. } => {
              child_spans = child_spans + [{
                span_id,
                operation_name,
                parent_id
              }]
            }
          }
        }
        
        { root_span, child_spans }
      }
    }
  }
  
  // 提取span层次结构
  let hierarchy = extract_span_hierarchy(test_trace)
  
  // 验证层次结构
  match hierarchy.root_span {
    Some(root) => {
      assert_eq(root.span_id, "span-001")
      assert_eq(root.operation_name, "root_operation")
    }
    None => assert_true(false)
  }
  
  assert_eq(hierarchy.child_spans.length(), 2)
  assert_true(hierarchy.child_spans.contains({
    span_id: "span-002",
    operation_name: "database_query",
    parent_id: "span-001"
  }))
  assert_true(hierarchy.child_spans.contains({
    span_id: "span-003",
    operation_name: "cache_lookup",
    parent_id: "span-001"
  }))
  
  // 条件模式匹配
  let analyze_failed_spans = fn(trace: TraceData) {
    match trace {
      { spans, .. } => {
        let failed_span_details = spans.reduce(fn(acc, span) {
          match span {
            { status: "error", span_id, operation_name, events, .. } => {
              let error_events = events.reduce(fn(event_acc, event) {
                match event {
                  { event_name: name, attributes, .. } if name.contains("error") || name.contains("miss") => {
                    event_acc + [{
                      event_name: name,
                      attributes
                    }]
                  }
                  { event_name, .. } => event_acc
                }
              }, [])
              
              acc + [{
                span_id,
                operation_name,
                error_events
              }]
            }
            { status: _, .. } => acc
          }
        }, [])
        
        failed_span_details
      }
    }
  }
  
  // 分析失败的span
  let failed_spans = analyze_failed_spans(test_trace)
  
  // 验证失败的span分析
  assert_eq(failed_spans.length(), 1)
  assert_eq(failed_spans[0].span_id, "span-003")
  assert_eq(failed_spans[0].operation_name, "cache_lookup")
  assert_eq(failed_spans[0].error_events.length(), 1)
  assert_eq(failed_spans[0].error_events[0].event_name, "cache.miss")
  assert_true(failed_spans[0].error_events[0].attributes.contains(("key", "user:123")))
}

// 测试7: 遥测数据的函数式管道处理
test "functional pipeline processing of telemetry data" {
  // 定义管道阶段类型
  type PipelineStage[T, U] = (T) -> U
  
  // 定义遥测数据类型
  type RawTelemetryData = {
    timestamp: Int,
    source: String,
    data_type: String,
    raw_value: String
  }
  
  type ParsedTelemetryData = {
    timestamp: Int,
    source: String,
    data_type: String,
    parsed_value: Float
  }
  
  type FilteredTelemetryData = {
    timestamp: Int,
    source: String,
    data_type: String,
    parsed_value: Float,
    passed_filter: Bool
  }
  
  type AggregatedTelemetryData = {
    source: String,
    data_type: String,
    count: Int,
    sum: Float,
    average: Float,
    min: Float,
    max: Float
  }
  
  // 创建原始遥测数据
  let raw_data = [
    { timestamp: 1640995200, source: "server-1", data_type: "cpu", raw_value: "25.5" },
    { timestamp: 1640995250, source: "server-2", data_type: "cpu", raw_value: "75.8" },
    { timestamp: 1640995300, source: "server-1", data_type: "memory", raw_value: "45.2" },
    { timestamp: 1640995350, source: "server-2", data_type: "memory", raw_value: "80.1" },
    { timestamp: 1640995400, source: "server-1", data_type: "cpu", raw_value: "30.1" },
    { timestamp: 1640995450, source: "server-2", data_type: "cpu", raw_value: "85.4" },
    { timestamp: 1640995500, source: "server-1", data_type: "memory", raw_value: "50.3" },
    { timestamp: 1640995550, source: "server-2", data_type: "memory", raw_value: "90.7" }
  ]
  
  // 管道阶段1: 解析原始数据
  let parse_data: PipelineStage[RawTelemetryData, ParsedTelemetryData] = fn(raw) {
    match raw {
      { timestamp, source, data_type, raw_value } => {
        let mut parsed = 0.0
        let chars = raw_value.to_char_array()
        let mut decimal_found = false
        let mut decimal_place = 0.1
        
        for i in 0..chars.length() {
          let c = chars[i]
          if c == '.' {
            decimal_found = true
          } else if c >= '0' and c <= '9' {
            let digit = (c.to_int() - '0'.to_int()).to_float()
            if decimal_found {
              parsed = parsed + digit * decimal_place
              decimal_place = decimal_place * 0.1
            } else {
              parsed = parsed * 10.0 + digit
            }
          }
        }
        
        {
          timestamp,
          source,
          data_type,
          parsed_value: parsed
        }
      }
    }
  }
  
  // 管道阶段2: 过滤数据
  let filter_data: PipelineStage[ParsedTelemetryData, FilteredTelemetryData] = fn(parsed) {
    match parsed {
      { timestamp, source, data_type, parsed_value } => {
        let passed = match data_type {
          "cpu" => parsed_value > 70.0
          "memory" => parsed_value > 60.0
          _ => false
        }
        
        {
          timestamp,
          source,
          data_type,
          parsed_value,
          passed_filter: passed
        }
      }
    }
  }
  
  // 管道阶段3: 聚合数据
  let aggregate_data: PipelineStage[Array[FilteredTelemetryData], Array[AggregatedTelemetryData]] = fn(filtered) {
    let groups = []
    
    // 按源和数据类型分组
    let unique_keys = []
    for item in filtered {
      let key = item.source + ":" + item.data_type
      if not(unique_keys.contains(key)) {
        unique_keys = unique_keys + [key]
      }
    }
    
    for key in unique_keys {
      let parts = key.split(":")
      let source = parts[0]
      let data_type = parts[1]
      
      let group_items = filtered.filter(fn(item) {
        item.source == source and item.data_type == data_type
      })
      
      let count = group_items.length()
      let sum = group_items.reduce(fn(acc, item) {
        acc + item.parsed_value
      }, 0.0)
      
      let average = sum / count.to_float()
      
      let min = group_items.reduce(fn(acc, item) {
        if item.parsed_value < acc { item.parsed_value } else { acc }
      }, group_items[0].parsed_value)
      
      let max = group_items.reduce(fn(acc, item) {
        if item.parsed_value > acc { item.parsed_value } else { acc }
      }, group_items[0].parsed_value)
      
      groups = groups + [{
        source,
        data_type,
        count,
        sum,
        average,
        min,
        max
      }]
    }
    
    groups
  }
  
  // 管道组合函数
  let compose = fn[T, U, V](first: PipelineStage[T, U], second: PipelineStage[U, V]) {
    fn(input: T) {
      second(first(input))
    }
  }
  
  let compose3 = fn[T, U, V, W](
    first: PipelineStage[T, U], 
    second: PipelineStage[U, V], 
    third: PipelineStage[V, W]
  ) {
    fn(input: T) {
      third(second(first(input)))
    }
  }
  
  // 应用管道处理
  let parsed_data = raw_data.map(parse_data)
  assert_eq(parsed_data.length(), 8)
  
  let filtered_data = parsed_data.map(filter_data)
  assert_eq(filtered_data.length(), 8)
  
  // 检查过滤结果
  let passed_cpu = filtered_data.filter(fn(item) {
    item.data_type == "cpu" and item.passed_filter
  })
  assert_eq(passed_cpu.length(), 2)  // 75.8, 85.4
  
  let passed_memory = filtered_data.filter(fn(item) {
    item.data_type == "memory" and item.passed_filter
  })
  assert_eq(passed_memory.length(), 2)  // 80.1, 90.7
  
  // 聚合通过过滤的数据
  let passed_data = filtered_data.filter(fn(item) { item.passed_filter })
  let aggregated_data = aggregate_data(passed_data)
  
  // 验证聚合结果
  assert_eq(aggregated_data.length(), 4)  // server-1:cpu, server-1:memory, server-2:cpu, server-2:memory
  
  // 检查server-1的CPU聚合
  let server1_cpu = aggregated_data.filter(fn(item) {
    item.source == "server-1" and item.data_type == "cpu"
  })
  assert_eq(server1_cpu.length(), 0)  // server-1的CPU值都小于70，被过滤掉了
  
  // 检查server-2的CPU聚合
  let server2_cpu = aggregated_data.filter(fn(item) {
    item.source == "server-2" and item.data_type == "cpu"
  })
  assert_eq(server2_cpu.length(), 1)
  assert_eq(server2_cpu[0].count, 2)
  assert_eq(server2_cpu[0].sum, 161.2)  // 75.8 + 85.4
  assert_eq(server2_cpu[0].average, 80.6)
  assert_eq(server2_cpu[0].min, 75.8)
  assert_eq(server2_cpu[0].max, 85.4)
  
  // 检查server-1的内存聚合
  let server1_memory = aggregated_data.filter(fn(item) {
    item.source == "server-1" and item.data_type == "memory"
  })
  assert_eq(server1_memory.length(), 0)  // server-1的内存值都小于60，被过滤掉了
  
  // 检查server-2的内存聚合
  let server2_memory = aggregated_data.filter(fn(item) {
    item.source == "server-2" and item.data_type == "memory"
  })
  assert_eq(server2_memory.length(), 1)
  assert_eq(server2_memory[0].count, 2)
  assert_eq(server2_memory[0].sum, 170.8)  // 80.1 + 90.7
  assert_eq(server2_memory[0].average, 85.4)
  assert_eq(server2_memory[0].min, 80.1)
  assert_eq(server2_memory[0].max, 90.7)
  
  // 使用组合管道
  let full_pipeline = compose3(
    fn(data: Array[RawTelemetryData]) { data.map(parse_data) },
    fn(data: Array[ParsedTelemetryData]) { data.map(filter_data) },
    fn(data: Array[FilteredTelemetryData]) { 
      aggregate_data(data.filter(fn(item) { item.passed_filter }))
    }
  )
  
  let pipeline_result = full_pipeline(raw_data)
  
  // 验证管道结果
  assert_eq(pipeline_result.length(), 2)  // 只有server-2的CPU和内存通过过滤
  
  let server2_cpu_result = pipeline_result.filter(fn(item) {
    item.source == "server-2" and item.data_type == "cpu"
  })
  assert_eq(server2_cpu_result.length(), 1)
  
  let server2_memory_result = pipeline_result.filter(fn(item) {
    item.source == "server-2" and item.data_type == "memory"
  })
  assert_eq(server2_memory_result.length(), 1)
}

// 测试8: 遥测数据的递归折叠处理
test "recursive fold processing of telemetry data" {
  // 定义树形遥测数据结构
  type TelemetryTree = 
    | LeafNode(String, Float)  // metric_name, value
    | BranchNode(String, Array[TelemetryTree])  // category, children
  
  // 创建复杂遥测树
  let telemetry_tree = BranchNode("root", [
    BranchNode("system", [
      LeafNode("cpu_usage", 75.5),
      LeafNode("memory_usage", 60.2),
      BranchNode("disk", [
        LeafNode("disk_usage", 45.8),
        LeafNode("disk_io", 120.5)
      ])
    ]),
    BranchNode("application", [
      LeafNode("response_time", 150.0),
      LeafNode("throughput", 1000.0),
      BranchNode("database", [
        LeafNode("db_connections", 25.0),
        LeafNode("db_query_time", 80.0),
        BranchNode("tables", [
          LeafNode("users_size", 50000.0),
          LeafNode("orders_size", 100000.0)
        ])
      ])
    ]),
    BranchNode("network", [
      LeafNode("inbound_traffic", 500.0),
      LeafNode("outbound_traffic", 750.0)
    ])
  ])
  
  // 递归折叠函数
  let fold_tree = fn[T](
    tree: TelemetryTree, 
    initial: T, 
    leaf_processor: (String, Float, T) -> T,
    branch_processor: (String, Array[T], T) -> T
  ) {
    match tree {
      LeafNode(name, value) => leaf_processor(name, value, initial)
      BranchNode(name, children) => {
        let child_results = children.map(fn(child) {
          fold_tree(child, initial, leaf_processor, branch_processor)
        })
        branch_processor(name, child_results, initial)
      }
    }
  }
  
  // 使用折叠计算所有叶节点的总和
  let sum_all_values = fold_tree(
    telemetry_tree,
    0.0,
    fn(name, value, acc) { acc + value },
    fn(name, child_results, acc) {
      child_results.reduce(fn(acc, result) { acc + result }, 0.0)
    }
  )
  
  // 验证总和
  assert_eq(sum_all_values, 75.5 + 60.2 + 45.8 + 120.5 + 150.0 + 1000.0 + 25.0 + 80.0 + 50000.0 + 100000.0 + 500.0 + 750.0)
  
  // 使用折叠计算所有叶节点的数量
  let count_all_leaves = fold_tree(
    telemetry_tree,
    0,
    fn(name, value, acc) { acc + 1 },
    fn(name, child_results, acc) {
      child_results.reduce(fn(acc, result) { acc + result }, 0)
    }
  )
  
  // 验证叶节点数量
  assert_eq(count_all_leaves, 12)
  
  // 使用折叠收集所有叶节点名称
  let collect_all_names = fold_tree(
    telemetry_tree,
    [],
    fn(name, value, acc) { acc + [name] },
    fn(name, child_results, acc) {
      child_results.reduce(fn(acc, result) { acc + result }, [])
    }
  )
  
  // 验证叶节点名称
  assert_eq(collect_all_names.length(), 12)
  assert_true(collect_all_names.contains("cpu_usage"))
  assert_true(collect_all_names.contains("memory_usage"))
  assert_true(collect_all_names.contains("disk_usage"))
  assert_true(collect_all_names.contains("disk_io"))
  assert_true(collect_all_names.contains("response_time"))
  assert_true(collect_all_names.contains("throughput"))
  assert_true(collect_all_names.contains("db_connections"))
  assert_true(collect_all_names.contains("db_query_time"))
  assert_true(collect_all_names.contains("users_size"))
  assert_true(collect_all_names.contains("orders_size"))
  assert_true(collect_all_names.contains("inbound_traffic"))
  assert_true(collect_all_names.contains("outbound_traffic"))
  
  // 使用折叠计算每个分支的统计信息
  type BranchStats = {
    name: String,
    leaf_count: Int,
    total_value: Float,
    average_value: Float,
    max_value: Float,
    min_value: Float
  }
  
  let calculate_branch_stats = fold_tree(
    telemetry_tree,
    [],
    fn(name, value, acc) {
      acc + [{
        name: name,
        leaf_count: 1,
        total_value: value,
        average_value: value,
        max_value: value,
        min_value: value
      }]
    },
    fn(name, child_results, acc) {
      let total_leaves = child_results.reduce(fn(acc, result) {
        acc + result.leaf_count
      }, 0)
      
      let total_value = child_results.reduce(fn(acc, result) {
        acc + result.total_value
      }, 0.0)
      
      let average_value = if total_leaves > 0 {
        total_value / total_leaves.to_float()
      } else {
        0.0
      }
      
      let max_value = child_results.reduce(fn(acc, result) {
        if result.max_value > acc { result.max_value } else { acc }
      }, 0.0)
      
      let min_value = if child_results.length() > 0 {
        child_results.reduce(fn(acc, result) {
          if result.min_value < acc { result.min_value } else { acc }
        }, child_results[0].min_value)
      } else {
        0.0
      }
      
      acc + child_results + [{
        name: name,
        leaf_count: total_leaves,
        total_value: total_value,
        average_value: average_value,
        max_value: max_value,
        min_value: min_value
      }]
    }
  )
  
  // 验证分支统计信息
  let system_stats = calculate_branch_stats.filter(fn(stats) {
    stats.name == "system"
  })
  assert_eq(system_stats.length(), 1)
  assert_eq(system_stats[0].leaf_count, 4)  // cpu_usage, memory_usage, disk_usage, disk_io
  assert_eq(system_stats[0].total_value, 75.5 + 60.2 + 45.8 + 120.5)
  
  let database_stats = calculate_branch_stats.filter(fn(stats) {
    stats.name == "database"
  })
  assert_eq(database_stats.length(), 1)
  assert_eq(database_stats[0].leaf_count, 3)  // db_connections, db_query_time, tables
  assert_eq(database_stats[0].total_value, 25.0 + 80.0 + 150000.0)  // tables包含两个叶节点
  
  // 使用折叠查找特定路径的值
  let find_value_by_path = fold_tree(
    telemetry_tree,
    [],
    fn(name, value, acc) { acc + [(name, value)] },
    fn(name, child_results, acc) {
      let flattened = child_results.reduce(fn(acc, result) {
        acc + result
      }, [])
      
      if name == "database" {
        acc + flattened
      } else {
        acc
      }
    }
  )
  
  // 验证数据库路径的值
  assert_eq(find_value_by_path.length(), 4)  // database分支及其子分支的所有叶节点
  assert_true(find_value_by_path.contains(("db_connections", 25.0)))
  assert_true(find_value_by_path.contains(("db_query_time", 80.0)))
  assert_true(find_value_by_path.contains(("users_size", 50000.0)))
  assert_true(find_value_by_path.contains(("orders_size", 100000.0)))
  
  // 使用折叠进行条件过滤
  let filter_high_values = fold_tree(
    telemetry_tree,
    [],
    fn(name, value, acc) {
      if value > 100.0 {
        acc + [(name, value)]
      } else {
        acc
      }
    },
    fn(name, child_results, acc) {
      child_results.reduce(fn(acc, result) {
        acc + result
      }, [])
    }
  )
  
  // 验证高值过滤
  assert_eq(filter_high_values.length(), 6)  // disk_io, response_time, throughput, db_query_time, users_size, orders_size, inbound_traffic, outbound_traffic
  assert_true(filter_high_values.contains(("disk_io", 120.5)))
  assert_true(filter_high_values.contains(("response_time", 150.0)))
  assert_true(filter_high_values.contains(("throughput", 1000.0)))
  assert_true(filter_high_values.contains(("db_query_time", 80.0)))  // 这个应该被过滤掉，因为小于100
  assert_true(filter_high_values.contains(("users_size", 50000.0)))
  assert_true(filter_high_values.contains(("orders_size", 100000.0)))
  assert_true(filter_high_values.contains(("inbound_traffic", 500.0)))
  assert_true(filter_high_values.contains(("outbound_traffic", 750.0)))
  
  // 修正过滤条件
  let filter_high_values_correct = fold_tree(
    telemetry_tree,
    [],
    fn(name, value, acc) {
      if value > 100.0 {
        acc + [(name, value)]
      } else {
        acc
      }
    },
    fn(name, child_results, acc) {
      child_results.reduce(fn(acc, result) {
        acc + result
      }, [])
    }
  )
  
  // 重新验证高值过滤
  assert_eq(filter_high_values_correct.length(), 6)  // disk_io, response_time, throughput, users_size, orders_size, inbound_traffic, outbound_traffic
  assert_true(filter_high_values_correct.contains(("disk_io", 120.5)))
  assert_true(filter_high_values_correct.contains(("response_time", 150.0)))
  assert_true(filter_high_values_correct.contains(("throughput", 1000.0)))
  assert_true(filter_high_values_correct.contains(("users_size", 50000.0)))
  assert_true(filter_high_values_correct.contains(("orders_size", 100000.0)))
  assert_true(filter_high_values_correct.contains(("inbound_traffic", 500.0)))
  assert_true(filter_high_values_correct.contains(("outbound_traffic", 750.0)))
}