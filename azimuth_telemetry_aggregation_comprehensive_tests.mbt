// Azimuth 遥测数据聚合综合测试
// 测试遥测系统的数据聚合功能，包括指标聚合、日志聚合和追踪聚合

test "指标数据聚合测试" {
  // 模拟指标数据点
  struct MetricPoint {
    timestamp: Int
    value: Float
    attributes: Array[(String, String)]
  }
  
  // 模拟指标聚合器
  struct MetricAggregator {
    name: String
    unit: String
    data_points: Array[MetricPoint]
    aggregation_type: String // "sum", "avg", "min", "max", "count"
  }
  
  // 创建指标聚合器
  let create_metric_aggregator = fn(name: String, unit: String, aggregation_type: String) -> MetricAggregator {
    MetricAggregator {
      name: name,
      unit: unit,
      data_points: [],
      aggregation_type: aggregation_type
    }
  }
  
  // 添加数据点
  let add_data_point = fn(aggregator: MetricAggregator, timestamp: Int, value: Float, attributes: Array[(String, String)]) -> MetricAggregator {
    let new_point = MetricPoint {
      timestamp: timestamp,
      value: value,
      attributes: attributes
    }
    
    { ...aggregator, data_points: aggregator.data_points.push(new_point) }
  }
  
  // 执行聚合
  let aggregate = fn(aggregator: MetricAggregator) -> Float {
    if aggregator.data_points.length() == 0 {
      return 0.0
    }
    
    match aggregator.aggregation_type {
      "sum" => {
        let mut sum = 0.0
        for point in aggregator.data_points {
          sum = sum + point.value
        }
        sum
      }
      "avg" => {
        let mut sum = 0.0
        for point in aggregator.data_points {
          sum = sum + point.value
        }
        sum / aggregator.data_points.length().to_float()
      }
      "min" => {
        let mut min = aggregator.data_points[0].value
        for point in aggregator.data_points {
          if point.value < min {
            min = point.value
          }
        }
        min
      }
      "max" => {
        let mut max = aggregator.data_points[0].value
        for point in aggregator.data_points {
          if point.value > max {
            max = point.value
          }
        }
        max
      }
      "count" => {
        aggregator.data_points.length().to_float()
      }
      _ => 0.0
    }
  }
  
  // 按属性分组聚合
  let aggregate_by_attributes = fn(aggregator: MetricAggregator, attribute_key: String) -> Array[(String, Float)] {
    let mut groups = []
    
    // 收集所有唯一的属性值
    let unique_values = aggregator.data_points.map(fn(point) {
      let attr_value = point.attributes.find(fn(attr) { attr.0 == attribute_key })
      match attr_value {
        Some((_, value)) => value
        None => "default"
      }
    }).unique()
    
    // 为每个属性值计算聚合
    for value in unique_values {
      let filtered_points = aggregator.data_points.filter(fn(point) {
        let attr_value = point.attributes.find(fn(attr) { attr.0 == attribute_key })
        match attr_value {
          Some((_, v)) => v == value
          None => value == "default"
        }
      })
      
      let filtered_aggregator = { ...aggregator, data_points: filtered_points }
      let aggregated_value = aggregate(filtered_aggregator)
      
      groups = groups.push((value, aggregated_value))
    }
    
    groups
  }
  
  // 创建计数器聚合器
  let counter_aggregator = create_metric_aggregator("request_count", "count", "sum")
  
  // 添加数据点
  let counter1 = add_data_point(counter_aggregator, 1000, 1.0, [("endpoint", "/api/users"), ("method", "GET")])
  let counter2 = add_data_point(counter1, 2000, 1.0, [("endpoint", "/api/users"), ("method", "GET")])
  let counter3 = add_data_point(counter2, 3000, 1.0, [("endpoint", "/api/orders"), ("method", "POST")])
  let counter4 = add_data_point(counter3, 4000, 1.0, [("endpoint", "/api/users"), ("method", "POST")])
  
  // 验证总和聚合
  let total_count = aggregate(counter4)
  assert_eq(total_count, 4.0)
  
  // 按端点分组聚合
  let endpoint_groups = aggregate_by_attributes(counter4, "endpoint")
  assert_eq(endpoint_groups.length(), 2)
  assert_true(endpoint_groups.some(fn(group) { group.0 == "/api/users" && group.1 == 3.0 }))
  assert_true(endpoint_groups.some(fn(group) { group.0 == "/api/orders" && group.1 == 1.0 }))
  
  // 按方法分组聚合
  let method_groups = aggregate_by_attributes(counter4, "method")
  assert_eq(method_groups.length(), 2)
  assert_true(method_groups.some(fn(group) { group.0 == "GET" && group.1 == 2.0 }))
  assert_true(method_groups.some(fn(group) { group.0 == "POST" && group.1 == 2.0 }))
  
  // 创建延迟聚合器
  let latency_aggregator = create_metric_aggregator("request_latency", "ms", "avg")
  
  // 添加延迟数据点
  let latency1 = add_data_point(latency_aggregator, 1000, 100.0, [("endpoint", "/api/users")])
  let latency2 = add_data_point(latency1, 2000, 150.0, [("endpoint", "/api/users")])
  let latency3 = add_data_point(latency2, 3000, 200.0, [("endpoint", "/api/orders")])
  let latency4 = add_data_point(latency3, 4000, 50.0, [("endpoint", "/api/orders")])
  
  // 验证平均延迟
  let avg_latency = aggregate(latency4)
  assert_eq(avg_latency, (100.0 + 150.0 + 200.0 + 50.0) / 4.0)
  
  // 按端点分组的平均延迟
  let latency_by_endpoint = aggregate_by_attributes(latency4, "endpoint")
  assert_eq(latency_by_endpoint.length(), 2)
  assert_true(latency_by_endpoint.some(fn(group) { group.0 == "/api/users" && group.1 == (100.0 + 150.0) / 2.0 }))
  assert_true(latency_by_endpoint.some(fn(group) { group.0 == "/api/orders" && group.1 == (200.0 + 50.0) / 2.0 }))
}

test "时间窗口聚合测试" {
  // 模拟时间窗口
  struct TimeWindow {
    start_time: Int
    end_time: Int
    duration: Int
  }
  
  // 模拟时间序列数据点
  struct TimeSeriesPoint {
    timestamp: Int
    value: Float
  }
  
  // 创建时间窗口
  let create_time_window = fn(start_time: Int, duration: Int) -> TimeWindow {
    TimeWindow {
      start_time: start_time,
      end_time: start_time + duration,
      duration: duration
    }
  }
  
  // 检查点是否在窗口内
  let is_point_in_window = fn(point: TimeSeriesPoint, window: TimeWindow) -> Bool {
    point.timestamp >= window.start_time && point.timestamp < window.end_time
  }
  
  // 按时间窗口聚合
  let aggregate_by_time_window = fn(points: Array[TimeSeriesPoint], windows: Array[TimeWindow], aggregation_type: String) -> Array[(TimeWindow, Float)] {
    let mut results = []
    
    for window in windows {
      let window_points = points.filter(fn(point) { is_point_in_window(point, window) })
      
      let aggregated_value = match aggregation_type {
        "sum" => {
          let mut sum = 0.0
          for point in window_points {
            sum = sum + point.value
          }
          sum
        }
        "avg" => {
          if window_points.length() == 0 {
            0.0
          } else {
            let mut sum = 0.0
            for point in window_points {
              sum = sum + point.value
            }
            sum / window_points.length().to_float()
          }
        }
        "count" => {
          window_points.length().to_float()
        }
        _ => 0.0
      }
      
      results = results.push((window, aggregated_value))
    }
    
    results
  }
  
  // 创建时间序列数据点
  let time_series_points = [
    TimeSeriesPoint { timestamp: 1000, value: 10.0 },
    TimeSeriesPoint { timestamp: 1500, value: 20.0 },
    TimeSeriesPoint { timestamp: 2000, value: 15.0 },
    TimeSeriesPoint { timestamp: 2500, value: 25.0 },
    TimeSeriesPoint { timestamp: 3000, value: 30.0 },
    TimeSeriesPoint { timestamp: 3500, value: 35.0 },
    TimeSeriesPoint { timestamp: 4000, value: 40.0 },
    TimeSeriesPoint { timestamp: 4500, value: 45.0 }
  ]
  
  // 创建时间窗口（每个窗口1秒）
  let time_windows = [
    create_time_window(1000, 1000), // 1000-2000
    create_time_window(2000, 1000), // 2000-3000
    create_time_window(3000, 1000), // 3000-4000
    create_time_window(4000, 1000)  // 4000-5000
  ]
  
  // 按时间窗口求和聚合
  let sum_by_window = aggregate_by_time_window(time_series_points, time_windows, "sum")
  assert_eq(sum_by_window.length(), 4)
  assert_eq(sum_by_window[0].1, 10.0 + 20.0) // 1000-2000窗口
  assert_eq(sum_by_window[1].1, 15.0 + 25.0) // 2000-3000窗口
  assert_eq(sum_by_window[2].1, 30.0 + 35.0) // 3000-4000窗口
  assert_eq(sum_by_window[3].1, 40.0 + 45.0) // 4000-5000窗口
  
  // 按时间窗口平均聚合
  let avg_by_window = aggregate_by_time_window(time_series_points, time_windows, "avg")
  assert_eq(avg_by_window.length(), 4)
  assert_eq(avg_by_window[0].1, (10.0 + 20.0) / 2.0) // 1000-2000窗口
  assert_eq(avg_by_window[1].1, (15.0 + 25.0) / 2.0) // 2000-3000窗口
  assert_eq(avg_by_window[2].1, (30.0 + 35.0) / 2.0) // 3000-4000窗口
  assert_eq(avg_by_window[3].1, (40.0 + 45.0) / 2.0) // 4000-5000窗口
  
  // 按时间窗口计数聚合
  let count_by_window = aggregate_by_time_window(time_series_points, time_windows, "count")
  assert_eq(count_by_window.length(), 4)
  assert_eq(count_by_window[0].1, 2.0) // 1000-2000窗口
  assert_eq(count_by_window[1].1, 2.0) // 2000-3000窗口
  assert_eq(count_by_window[2].1, 2.0) // 3000-4000窗口
  assert_eq(count_by_window[3].1, 2.0) // 4000-5000窗口
  
  // 测试重叠窗口
  let overlapping_windows = [
    create_time_window(1250, 1500), // 1250-2750
    create_time_window(2750, 1500)  // 2750-4250
  ]
  
  let overlapping_sum = aggregate_by_time_window(time_series_points, overlapping_windows, "sum")
  assert_eq(overlapping_sum.length(), 2)
  assert_eq(overlapping_sum[0].1, 20.0 + 15.0 + 25.0) // 1250-2750窗口
  assert_eq(overlapping_sum[1].1, 30.0 + 35.0 + 40.0) // 2750-4250窗口
}

test "日志数据聚合测试" {
  // 模拟日志记录
  struct LogRecord {
    timestamp: Int
    level: String
    message: String
    source: String
    attributes: Array[(String, String)]
  }
  
  // 模拟日志聚合器
  struct LogAggregator {
    logs: Array[LogRecord]
  }
  
  // 创建日志聚合器
  let create_log_aggregator = fn() -> LogAggregator {
    LogAggregator { logs: [] }
  }
  
  // 添加日志记录
  let add_log = fn(aggregator: LogAggregator, timestamp: Int, level: String, message: String, source: String, attributes: Array[(String, String)]) -> LogAggregator {
    let log_record = LogRecord {
      timestamp: timestamp,
      level: level,
      message: message,
      source: source,
      attributes: attributes
    }
    
    { ...aggregator, logs: aggregator.logs.push(log_record) }
  }
  
  // 按日志级别聚合
  let aggregate_by_level = fn(aggregator: LogAggregator) -> Array[(String, Int)] {
    let mut levels = []
    
    // 收集所有日志级别
    let unique_levels = aggregator.logs.map(fn(log) { log.level }).unique()
    
    // 为每个级别计数
    for level in unique_levels {
      let count = aggregator.logs.filter(fn(log) { log.level == level }).length()
      levels = levels.push((level, count))
    }
    
    levels
  }
  
  // 按来源聚合
  let aggregate_by_source = fn(aggregator: LogAggregator) -> Array[(String, Int)] {
    let mut sources = []
    
    // 收集所有来源
    let unique_sources = aggregator.logs.map(fn(log) { log.source }).unique()
    
    // 为每个来源计数
    for source in unique_sources {
      let count = aggregator.logs.filter(fn(log) { log.source == source }).length()
      sources = sources.push((source, count))
    }
    
    sources
  }
  
  // 按时间窗口聚合
  let aggregate_logs_by_time_window = fn(aggregator: LogAggregator, start_time: Int, duration: Int) -> Int {
    let end_time = start_time + duration
    aggregator.logs.filter(fn(log) { 
      log.timestamp >= start_time && log.timestamp < end_time 
    }).length()
  }
  
  // 按错误消息模式聚合
  let aggregate_by_error_pattern = fn(aggregator: LogAggregator, pattern: String) -> Int {
    aggregator.logs.filter(fn(log) { 
      log.level == "ERROR" && log.message.contains(pattern)
    }).length()
  }
  
  // 创建日志聚合器
  let log_aggregator = create_log_aggregator()
  
  // 添加日志记录
  let log1 = add_log(log_aggregator, 1000, "INFO", "Application started", "app-server", [])
  let log2 = add_log(log1, 1500, "INFO", "User logged in", "auth-service", [])
  let log3 = add_log(log2, 2000, "WARN", "High memory usage", "app-server", [])
  let log4 = add_log(log3, 2500, "ERROR", "Database connection failed", "db-service", [])
  let log5 = add_log(log4, 3000, "ERROR", "Database connection failed", "db-service", [])
  let log6 = add_log(log5, 3500, "INFO", "User logged out", "auth-service", [])
  let log7 = add_log(log6, 4000, "ERROR", "Invalid user input", "auth-service", [])
  let log8 = add_log(log7, 4500, "WARN", "Slow query detected", "db-service", [])
  
  // 按日志级别聚合
  let level_counts = aggregate_by_level(log8)
  assert_eq(level_counts.length(), 3)
  assert_true(level_counts.some(fn(count) { count.0 == "INFO" && count.1 == 3 }))
  assert_true(level_counts.some(fn(count) { count.0 == "WARN" && count.1 == 2 }))
  assert_true(level_counts.some(fn(count) { count.0 == "ERROR" && count.1 == 3 }))
  
  // 按来源聚合
  let source_counts = aggregate_by_source(log8)
  assert_eq(source_counts.length(), 3)
  assert_true(source_counts.some(fn(count) { count.0 == "app-server" && count.1 == 2 }))
  assert_true(source_counts.some(fn(count) { count.0 == "auth-service" && count.1 == 3 }))
  assert_true(source_counts.some(fn(count) { count.0 == "db-service" && count.1 == 3 }))
  
  // 按时间窗口聚合
  let window1_count = aggregate_logs_by_time_window(log8, 1000, 2000) // 1000-3000
  let window2_count = aggregate_logs_by_time_window(log8, 3000, 2000) // 3000-5000
  assert_eq(window1_count, 4) // 1000, 1500, 2000, 2500
  assert_eq(window2_count, 4) // 3000, 3500, 4000, 4500
  
  // 按错误消息模式聚合
  let db_error_count = aggregate_by_error_pattern(log8, "Database")
  let input_error_count = aggregate_by_error_pattern(log8, "Invalid")
  assert_eq(db_error_count, 2) // 两个数据库连接错误
  assert_eq(input_error_count, 1) // 一个输入错误
}

test "追踪数据聚合测试" {
  // 模拟跨度事件
  struct SpanEvent {
    timestamp: Int
    name: String
    attributes: Array[(String, String)]
  }
  
  // 模拟跨度
  struct Span {
    trace_id: String
    span_id: String
    parent_span_id: Option[String]
    name: String
    start_time: Int
    end_time: Int
    status: String
    events: Array[SpanEvent]
  }
  
  // 模拟追踪聚合器
  struct TraceAggregator {
    spans: Array[Span]
  }
  
  // 创建追踪聚合器
  let create_trace_aggregator = fn() -> TraceAggregator {
    TraceAggregator { spans: [] }
  }
  
  // 添加跨度
  let add_span = fn(aggregator: TraceAggregator, trace_id: String, span_id: String, parent_span_id: Option[String], name: String, start_time: Int, end_time: Int, status: String) -> TraceAggregator {
    let span = Span {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: parent_span_id,
      name: name,
      start_time: start_time,
      end_time: end_time,
      status: status,
      events: []
    }
    
    { ...aggregator, spans: aggregator.spans.push(span) }
  }
  
  // 计算跨度持续时间
  let span_duration = fn(span: Span) -> Int {
    span.end_time - span.start_time
  }
  
  // 按追踪ID分组跨度
  let group_spans_by_trace = fn(aggregator: TraceAggregator) -> Array[(String, Array[Span])] {
    let mut traces = []
    
    // 收集所有追踪ID
    let unique_trace_ids = aggregator.spans.map(fn(span) { span.trace_id }).unique()
    
    // 为每个追踪ID收集跨度
    for trace_id in unique_trace_ids {
      let trace_spans = aggregator.spans.filter(fn(span) { span.trace_id == trace_id })
      traces = traces.push((trace_id, trace_spans))
    }
    
    traces
  }
  
  // 计算追踪总持续时间
  let trace_duration = fn(trace_spans: Array[Span]) -> Int {
    if trace_spans.length() == 0 {
      return 0
    }
    
    let start_times = trace_spans.map(fn(span) { span.start_time })
    let end_times = trace_spans.map(fn(span) { span.end_time })
    
    let trace_start = start_times.reduce(fn(min, time) { if time < min { time } else { min })
    let trace_end = end_times.reduce(fn(max, time) { if time > max { time } else { max })
    
    trace_end - trace_start
  }
  
  // 按跨度名称聚合
  let aggregate_by_span_name = fn(aggregator: TraceAggregator) -> Array[(String, Int)] {
    let mut span_names = []
    
    // 收集所有跨度名称
    let unique_names = aggregator.spans.map(fn(span) { span.name }).unique()
    
    // 为每个名称计数
    for name in unique_names {
      let count = aggregator.spans.filter(fn(span) { span.name == name }).length()
      span_names = span_names.push((name, count))
    }
    
    span_names
  }
  
  // 按状态聚合
  let aggregate_by_status = fn(aggregator: TraceAggregator) -> Array[(String, Int)] {
    let mut statuses = []
    
    // 收集所有状态
    let unique_statuses = aggregator.spans.map(fn(span) { span.status }).unique()
    
    // 为每个状态计数
    for status in unique_statuses {
      let count = aggregator.spans.filter(fn(span) { span.status == status }).length()
      statuses = statuses.push((status, count))
    }
    
    statuses
  }
  
  // 计算平均跨度持续时间
  let average_span_duration = fn(aggregator: TraceAggregator) -> Float {
    if aggregator.spans.length() == 0 {
      return 0.0
    }
    
    let mut total_duration = 0
    for span in aggregator.spans {
      total_duration = total_duration + span_duration(span)
    }
    
    total_duration.to_float() / aggregator.spans.length().to_float()
  }
  
  // 创建追踪聚合器
  let trace_aggregator = create_trace_aggregator()
  
  // 添加跨度
  let agg1 = add_span(trace_aggregator, "trace1", "span1", None, "HTTP GET /api/users", 1000, 1500, "OK")
  let agg2 = add_span(agg1, "trace1", "span2", Some("span1"), "Database query", 1100, 1400, "OK")
  let agg3 = add_span(agg2, "trace2", "span3", None, "HTTP POST /api/orders", 2000, 2800, "OK")
  let agg4 = add_span(agg3, "trace2", "span4", Some("span3"), "Validate order", 2100, 2300, "OK")
  let agg5 = add_span(agg4, "trace2", "span5", Some("span3"), "Save order", 2400, 2700, "OK")
  let agg6 = add_span(agg5, "trace3", "span6", None, "HTTP GET /api/products", 3000, 3200, "ERROR")
  let agg7 = add_span(agg6, "trace3", "span7", Some("span6"), "Cache lookup", 3050, 3150, "OK")
  
  // 按追踪ID分组
  let traces = group_spans_by_trace(agg7)
  assert_eq(traces.length(), 3)
  
  // 验证每个追踪的跨度
  let trace1_spans = traces.find(fn(t) { t.0 == "trace1" }).unwrap().1
  let trace2_spans = traces.find(fn(t) { t.0 == "trace2" }).unwrap().1
  let trace3_spans = traces.find(fn(t) { t.0 == "trace3" }).unwrap().1
  
  assert_eq(trace1_spans.length(), 2)
  assert_eq(trace2_spans.length(), 3)
  assert_eq(trace3_spans.length(), 2)
  
  // 计算追踪持续时间
  let trace1_duration = trace_duration(trace1_spans)
  let trace2_duration = trace_duration(trace2_spans)
  let trace3_duration = trace_duration(trace3_spans)
  
  assert_eq(trace1_duration, 1500 - 1000) // 500
  assert_eq(trace2_duration, 2800 - 2000) // 800
  assert_eq(trace3_duration, 3200 - 3000) // 200
  
  // 按跨度名称聚合
  let span_names = aggregate_by_span_name(agg7)
  assert_eq(span_names.length(), 6)
  assert_true(span_names.some(fn(name) { name.0 == "HTTP GET /api/users" && name.1 == 1 }))
  assert_true(span_names.some(fn(name) { name.0 == "Database query" && name.1 == 1 }))
  assert_true(span_names.some(fn(name) { name.0 == "HTTP POST /api/orders" && name.1 == 1 }))
  assert_true(span_names.some(fn(name) { name.0 == "Validate order" && name.1 == 1 }))
  assert_true(span_names.some(fn(name) { name.0 == "Save order" && name.1 == 1 }))
  assert_true(span_names.some(fn(name) { name.0 == "HTTP GET /api/products" && name.1 == 1 }))
  
  // 按状态聚合
  let statuses = aggregate_by_status(agg7)
  assert_eq(statuses.length(), 2)
  assert_true(statuses.some(fn(status) { status.0 == "OK" && status.1 == 6 }))
  assert_true(statuses.some(fn(status) { status.0 == "ERROR" && status.1 == 1 }))
  
  // 计算平均跨度持续时间
  let avg_duration = average_span_duration(agg7)
  let expected_avg = (500 + 300 + 800 + 200 + 300 + 100 + 200).to_float() / 7.0
  assert_eq(avg_duration, expected_avg)
}

test "多维数据聚合测试" {
  // 模拟多维数据点
  struct MultiDimensionalPoint {
    timestamp: Int
    value: Float
    dimensions: Array[(String, String)]
  }
  
  // 模拟多维聚合器
  struct MultiDimensionalAggregator {
    points: Array[MultiDimensionalPoint]
  }
  
  // 创建多维聚合器
  let create_multi_dimensional_aggregator = fn() -> MultiDimensionalAggregator {
    MultiDimensionalAggregator { points: [] }
  }
  
  // 添加数据点
  let add_point = fn(aggregator: MultiDimensionalAggregator, timestamp: Int, value: Float, dimensions: Array[(String, String)]) -> MultiDimensionalAggregator {
    let point = MultiDimensionalPoint {
      timestamp: timestamp,
      value: value,
      dimensions: dimensions
    }
    
    { ...aggregator, points: aggregator.points.push(point) }
  }
  
  // 按多个维度聚合
  let aggregate_by_dimensions = fn(aggregator: MultiDimensionalAggregator, dimension_keys: Array[String], aggregation_type: String) -> Array[(Array[(String, String)], Float)] {
    let mut groups = []
    
    // 收集所有唯一的维度组合
    let unique_combinations = aggregator.points.map(fn(point) {
      dimension_keys.map(fn(key) {
        let dim_value = point.dimensions.find(fn(dim) { dim.0 == key })
        match dim_value {
          Some((_, value)) => (key, value)
          None => (key, "default")
        }
      })
    }).unique()
    
    // 为每个维度组合计算聚合
    for combination in unique_combinations {
      let filtered_points = aggregator.points.filter(fn(point) {
        dimension_keys.all(fn(key) {
          let dim_value = point.dimensions.find(fn(dim) { dim.0 == key })
          match dim_value {
            Some((_, value)) => {
              let comb_value = combination.find(fn(comb) { comb.0 == key })
              match comb_value {
                Some((_, comb_val)) => value == comb_val
                None => false
              }
            }
            None => {
              let comb_value = combination.find(fn(comb) { comb.0 == key })
              match comb_value {
                Some((_, comb_val)) => comb_val == "default"
                None => false
              }
            }
          }
        })
      })
      
      let aggregated_value = match aggregation_type {
        "sum" => {
          let mut sum = 0.0
          for point in filtered_points {
            sum = sum + point.value
          }
          sum
        }
        "avg" => {
          if filtered_points.length() == 0 {
            0.0
          } else {
            let mut sum = 0.0
            for point in filtered_points {
              sum = sum + point.value
            }
            sum / filtered_points.length().to_float()
          }
        }
        "count" => {
          filtered_points.length().to_float()
        }
        _ => 0.0
      }
      
      groups = groups.push((combination, aggregated_value))
    }
    
    groups
  }
  
  // 创建多维聚合器
  let aggregator = create_multi_dimensional_aggregator()
  
  // 添加多维数据点
  let agg1 = add_point(aggregator, 1000, 100.0, [("region", "us-east"), ("service", "auth"), ("version", "v1")])
  let agg2 = add_point(agg1, 1500, 150.0, [("region", "us-east"), ("service", "auth"), ("version", "v2")])
  let agg3 = add_point(agg2, 2000, 200.0, [("region", "us-west"), ("service", "auth"), ("version", "v1")])
  let agg4 = add_point(agg3, 2500, 120.0, [("region", "us-east"), ("service", "payment"), ("version", "v1")])
  let agg5 = add_point(agg4, 3000, 180.0, [("region", "us-west"), ("service", "payment"), ("version", "v2")])
  
  // 按单个维度（region）聚合
  let region_groups = aggregate_by_dimensions(agg5, ["region"], "sum")
  assert_eq(region_groups.length(), 2)
  assert_true(region_groups.some(fn(group) { 
    group.0.length() == 1 && 
    group.0[0].0 == "region" && 
    group.0[0].1 == "us-east" && 
    group.1 == 100.0 + 150.0 + 120.0 
  }))
  assert_true(region_groups.some(fn(group) { 
    group.0.length() == 1 && 
    group.0[0].0 == "region" && 
    group.0[0].1 == "us-west" && 
    group.1 == 200.0 + 180.0 
  }))
  
  // 按两个维度（region, service）聚合
  let region_service_groups = aggregate_by_dimensions(agg5, ["region", "service"], "sum")
  assert_eq(region_service_groups.length(), 4)
  assert_true(region_service_groups.some(fn(group) { 
    group.0.length() == 2 && 
    group.0.any(fn(dim) { dim.0 == "region" && dim.1 == "us-east" }) && 
    group.0.any(fn(dim) { dim.0 == "service" && dim.1 == "auth" }) && 
    group.1 == 100.0 + 150.0
  }))
  assert_true(region_service_groups.some(fn(group) { 
    group.0.length() == 2 && 
    group.0.any(fn(dim) { dim.0 == "region" && dim.1 == "us-west" }) && 
    group.0.any(fn(dim) { dim.0 == "service" && dim.1 == "auth" }) && 
    group.1 == 200.0
  }))
  assert_true(region_service_groups.some(fn(group) { 
    group.0.length() == 2 && 
    group.0.any(fn(dim) { dim.0 == "region" && dim.1 == "us-east" }) && 
    group.0.any(fn(dim) { dim.0 == "service" && dim.1 == "payment" }) && 
    group.1 == 120.0
  }))
  assert_true(region_service_groups.some(fn(group) { 
    group.0.length() == 2 && 
    group.0.any(fn(dim) { dim.0 == "region" && dim.1 == "us-west" }) && 
    group.0.any(fn(dim) { dim.0 == "service" && dim.1 == "payment" }) && 
    group.1 == 180.0
  }))
  
  // 按三个维度（region, service, version）聚合
  let full_dimension_groups = aggregate_by_dimensions(agg5, ["region", "service", "version"], "avg")
  assert_eq(full_dimension_groups.length(), 5)
  assert_true(full_dimension_groups.some(fn(group) { 
    group.0.length() == 3 && 
    group.0.any(fn(dim) { dim.0 == "region" && dim.1 == "us-east" }) && 
    group.0.any(fn(dim) { dim.0 == "service" && dim.1 == "auth" }) && 
    group.0.any(fn(dim) { dim.0 == "version" && dim.1 == "v1" }) && 
    group.1 == 100.0
  }))
  assert_true(full_dimension_groups.some(fn(group) { 
    group.0.length() == 3 && 
    group.0.any(fn(dim) { dim.0 == "region" && dim.1 == "us-east" }) && 
    group.0.any(fn(dim) { dim.0 == "service" && dim.1 == "auth" }) && 
    group.0.any(fn(dim) { dim.0 == "version" && dim.1 == "v2" }) && 
    group.1 == 150.0
  }))
}