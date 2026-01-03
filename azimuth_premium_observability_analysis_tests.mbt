// Azimuth 高级可观测性分析测试
// 专注于遥测系统的核心功能和高级分析特性

// 测试1: 度量指标收集与聚合
test "度量指标收集与聚合" {
  // 1. 创建基础度量指标
  let metrics = [
    Counter({ name: "http.requests.total", value: 150, attributes: [("method", "GET"), ("status", "200")] }),
    Counter({ name: "http.requests.total", value: 25, attributes: [("method", "POST"), ("status", "201")] }),
    Histogram({ name: "http.request.duration", value: 120.5, attributes: [("endpoint", "/api/users")] }),
    Histogram({ name: "http.request.duration", value: 85.3, attributes: [("endpoint", "/api/orders")] }),
    Gauge({ name: "system.memory.usage", value: 75.2, attributes: [("host", "server-1")] })
  ]
  
  // 2. 验证度量指标数量
  assert_eq(metrics.length(), 5)
  
  // 3. 验证Counter类型指标
  let counter_metrics = metrics.filter(fn(m) {
    match m {
      Counter(_) => true
      _ => false
    }
  })
  assert_eq(counter_metrics.length(), 2)
  
  // 4. 验证Histogram类型指标
  let histogram_metrics = metrics.filter(fn(m) {
    match m {
      Histogram(_) => true
      _ => false
    }
  })
  assert_eq(histogram_metrics.length(), 2)
  
  // 5. 验证Gauge类型指标
  let gauge_metrics = metrics.filter(fn(m) {
    match m {
      Gauge(_) => true
      _ => false
    }
  })
  assert_eq(gauge_metrics.length(), 1)
  
  // 6. 计算总请求数
  let total_requests = counter_metrics.reduce(fn(acc, metric) {
    match metric {
      Counter({ value, ... }) => acc + value
      _ => acc
    }
  }, 0)
  assert_eq(total_requests, 175)
  
  // 7. 计算平均响应时间
  let total_duration = histogram_metrics.reduce(fn(acc, metric) {
    match metric {
      Histogram({ value, ... }) => acc + value
      _ => acc
    }
  }, 0.0)
  let avg_duration = total_duration / histogram_metrics.length()
  assert_true(avg_duration > 100.0 && avg_duration < 110.0)
}

// 测试2: 分布式追踪链路分析
test "分布式追踪链路分析" {
  // 1. 创建追踪链路
  let trace_spans = [
    Span({
      trace_id: "trace-abc123",
      span_id: "span-def456",
      parent_span_id: None,
      name: "HTTP GET /api/users",
      kind: Server,
      start_time: 1735689600000000000L,
      end_time: 1735689600150000000L,
      status: Ok,
      attributes: [("http.method", "GET"), ("http.status_code", "200"), ("user.id", "12345")]
    }),
    Span({
      trace_id: "trace-abc123",
      span_id: "span-ghi789",
      parent_span_id: Some("span-def456"),
      name: "Database Query",
      kind: Client,
      start_time: 1735689600020000000L,
      end_time: 1735689600080000000L,
      status: Ok,
      attributes: [("db.statement", "SELECT * FROM users"), ("db.type", "postgresql")]
    }),
    Span({
      trace_id: "trace-abc123",
      span_id: "span-jkl012",
      parent_span_id: Some("span-def456"),
      name: "Cache Lookup",
      kind: Internal,
      start_time: 1735689600010000000L,
      end_time: 1735689600015000000L,
      status: Ok,
      attributes: [("cache.key", "user:12345"), ("cache.hit", "false")]
    })
  ]
  
  // 2. 验证追踪链路完整性
  assert_eq(trace_spans.length(), 3)
  
  // 3. 验证所有跨度属于同一追踪
  let trace_ids = trace_spans.map(fn(span) { span.trace_id })
  for trace_id in trace_ids {
    assert_eq(trace_id, "trace-abc123")
  }
  
  // 4. 验证根跨度
  let root_spans = trace_spans.filter(fn(span) { span.parent_span_id == None })
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].span_id, "span-def456")
  
  // 5. 验证子跨度关系
  let child_spans = trace_spans.filter(fn(span) { span.parent_span_id != None })
  assert_eq(child_spans.length(), 2)
  
  for child in child_spans {
    assert_eq(child.parent_span_id!, "span-def456")
  }
  
  // 6. 计算总追踪时间
  let root_span = root_spans[0]
  let trace_duration = root_span.end_time - root_span.start_time
  assert_eq(trace_duration, 15000000L)  // 15ms
  
  // 7. 验证时间顺序
  assert_true(trace_spans[1].start_time >= root_span.start_time)
  assert_true(trace_spans[2].start_time >= root_span.start_time)
  assert_true(trace_spans[1].end_time <= root_span.end_time)
  assert_true(trace_spans[2].end_time <= root_span.end_time)
}

// 测试3: 日志关联性与上下文传播
test "日志关联性与上下文传播" {
  // 1. 创建日志记录集合
  let log_entries = [
    LogEntry({
      timestamp: 1735689600000000000L,
      severity: Info,
      message: "Request received",
      attributes: [
        ("request.id", "req-12345"),
        ("user.id", "user-67890"),
        ("trace.id", "trace-abc123"),
        ("span.id", "span-def456")
      ]
    }),
    LogEntry({
      timestamp: 1735689600050000000L,
      severity: Debug,
      message: "Authentication successful",
      attributes: [
        ("request.id", "req-12345"),
        ("user.id", "user-67890"),
        ("trace.id", "trace-abc123"),
        ("span.id", "span-def456"),
        ("auth.method", "JWT")
      ]
    }),
    LogEntry({
      timestamp: 1735689600100000000L,
      severity: Warning,
      message: "Rate limit approaching",
      attributes: [
        ("request.id", "req-12345"),
        ("user.id", "user-67890"),
        ("trace.id", "trace-abc123"),
        ("span.id", "span-ghi789"),
        ("rate.limit.remaining", "5")
      ]
    }),
    LogEntry({
      timestamp: 1735689600150000000L,
      severity: Info,
      message: "Request completed",
      attributes: [
        ("request.id", "req-12345"),
        ("user.id", "user-67890"),
        ("trace.id", "trace-abc123"),
        ("span.id", "span-def456"),
        ("response.status", "200"),
        ("duration.ms", "15")
      ]
    })
  ]
  
  // 2. 验证日志记录数量
  assert_eq(log_entries.length(), 4)
  
  // 3. 验证请求ID一致性
  let request_ids = log_entries.map(fn(entry) {
    let attrs = entry.attributes
    let mut request_id = ""
    for (key, value) in attrs {
      if key == "request.id" {
        request_id = value
      }
    }
    request_id
  })
  
  for req_id in request_ids {
    assert_eq(req_id, "req-12345")
  }
  
  // 4. 验证追踪ID一致性
  let trace_ids = log_entries.map(fn(entry) {
    let attrs = entry.attributes
    let mut trace_id = ""
    for (key, value) in attrs {
      if key == "trace.id" {
        trace_id = value
      }
    }
    trace_id
  })
  
  for trace_id in trace_ids {
    assert_eq(trace_id, "trace-abc123")
  }
  
  // 5. 验证时间序列
  for i in 1..log_entries.length() {
    assert_true(log_entries[i].timestamp >= log_entries[i-1].timestamp)
  }
  
  // 6. 验证日志严重级别分布
  let info_logs = log_entries.filter(fn(entry) { entry.severity == Info })
  let debug_logs = log_entries.filter(fn(entry) { entry.severity == Debug })
  let warning_logs = log_entries.filter(fn(entry) { entry.severity == Warning })
  
  assert_eq(info_logs.length(), 2)
  assert_eq(debug_logs.length(), 1)
  assert_eq(warning_logs.length(), 1)
  
  // 7. 验证跨度关联性
  let span_def456_logs = log_entries.filter(fn(entry) {
    let attrs = entry.attributes
    let mut span_id = ""
    for (key, value) in attrs {
      if key == "span.id" {
        span_id = value
      }
    }
    span_id == "span-def456"
  })
  
  let span_ghi789_logs = log_entries.filter(fn(entry) {
    let attrs = entry.attributes
    let mut span_id = ""
    for (key, value) in attrs {
      if key == "span.id" {
        span_id = value
      }
    }
    span_id == "span-ghi789"
  })
  
  assert_eq(span_def456_logs.length(), 3)
  assert_eq(span_ghi789_logs.length(), 1)
}

// 测试4: 实时数据流处理
test "实时数据流处理" {
  // 1. 创建数据流事件
  let stream_events = [
    StreamEvent({
      id: "event-001",
      timestamp: 1735689600000000000L,
      type: Metric,
      data: MetricData({
        name: "cpu.usage",
        value: 45.2,
        labels: [("host", "server-1"), ("region", "us-east-1")]
      })
    }),
    StreamEvent({
      id: "event-002",
      timestamp: 1735689600010000000L,
      type: Trace,
      data: TraceData({
        trace_id: "trace-xyz789",
        span_id: "span-abc123",
        operation_name: "database.query",
        duration_ms: 25
      })
    }),
    StreamEvent({
      id: "event-003",
      timestamp: 1735689600020000000L,
      type: Log,
      data: LogData({
        level: Error,
        message: "Database connection failed",
        attributes: [("error.code", "ECONNREFUSED"), ("retry.count", "3")]
      })
    }),
    StreamEvent({
      id: "event-004",
      timestamp: 1735689600030000000L,
      type: Metric,
      data: MetricData({
        name: "memory.usage",
        value: 78.5,
        labels: [("host", "server-1"), ("region", "us-east-1")]
      })
    })
  ]
  
  // 2. 验证事件数量
  assert_eq(stream_events.length(), 4)
  
  // 3. 按事件类型分类
  let metric_events = stream_events.filter(fn(event) { event.type == Metric })
  let trace_events = stream_events.filter(fn(event) { event.type == Trace })
  let log_events = stream_events.filter(fn(event) { event.type == Log })
  
  assert_eq(metric_events.length(), 2)
  assert_eq(trace_events.length(), 1)
  assert_eq(log_events.length(), 1)
  
  // 4. 验证时间序列
  for i in 1..stream_events.length() {
    assert_true(stream_events[i].timestamp >= stream_events[i-1].timestamp)
  }
  
  // 5. 处理度量事件
  let processed_metrics = metric_events.map(fn(event) {
    match event.data {
      MetricData({ name, value, labels }) => {
        let mut region = ""
        for (key, val) in labels {
          if key == "region" {
            region = val
          }
        }
        (name, value, region)
      }
      _ => ("", 0.0, "")
    }
  })
  
  assert_eq(processed_metrics.length(), 2)
  assert_eq(processed_metrics[0], ("cpu.usage", 45.2, "us-east-1"))
  assert_eq(processed_metrics[1], ("memory.usage", 78.5, "us-east-1"))
  
  // 6. 处理追踪事件
  let processed_trace = trace_events.map(fn(event) {
    match event.data {
      TraceData({ trace_id, span_id, operation_name, duration_ms }) => {
        (trace_id, span_id, operation_name, duration_ms)
      }
      _ => ("", "", "", 0)
    }
  })
  
  assert_eq(processed_trace.length(), 1)
  assert_eq(processed_trace[0], ("trace-xyz789", "span-abc123", "database.query", 25))
  
  // 7. 处理日志事件
  let processed_logs = log_events.map(fn(event) {
    match event.data {
      LogData({ level, message, attributes }) => {
        let mut error_code = ""
        for (key, value) in attributes {
          if key == "error.code" {
            error_code = value
          }
        }
        (level, message, error_code)
      }
      _ => ("", "", "")
    }
  })
  
  assert_eq(processed_logs.length(), 1)
  assert_eq(processed_logs[0], (Error, "Database connection failed", "ECONNREFUSED"))
  
  // 8. 计算事件处理延迟
  let first_event_time = stream_events[0].timestamp
  let last_event_time = stream_events[stream_events.length() - 1].timestamp
  let total_duration = last_event_time - first_event_time
  
  assert_eq(total_duration, 3000000L)  // 3ms
}

// 测试5: 异常检测与告警
test "异常检测与告警" {
  // 1. 创建监控指标数据
  let monitoring_data = [
    MetricPoint({
      timestamp: 1735689600000000000L,
      metric_name: "error.rate",
      value: 0.05,
      threshold: 0.1,
      labels: [("service", "api"), ("endpoint", "/users")]
    }),
    MetricPoint({
      timestamp: 1735689600100000000L,
      metric_name: "error.rate",
      value: 0.15,
      threshold: 0.1,
      labels: [("service", "api"), ("endpoint", "/users")]
    }),
    MetricPoint({
      timestamp: 1735689600200000000L,
      metric_name: "response.time",
      value: 850.0,
      threshold: 500.0,
      labels: [("service", "api"), ("endpoint", "/orders")]
    }),
    MetricPoint({
      timestamp: 1735689600300000000L,
      metric_name: "cpu.usage",
      value: 92.5,
      threshold: 80.0,
      labels: [("host", "server-1"), ("region", "us-east-1")]
    }),
    MetricPoint({
      timestamp: 1735689600400000000L,
      metric_name: "memory.usage",
      value: 75.0,
      threshold: 85.0,
      labels: [("host", "server-1"), ("region", "us-east-1")]
    })
  ]
  
  // 2. 验证监控数据
  assert_eq(monitoring_data.length(), 5)
  
  // 3. 检测异常值
  let anomalies = monitoring_data.filter(fn(point) { point.value > point.threshold })
  
  assert_eq(anomalies.length(), 3)  // error.rate(0.15), response.time(850), cpu.usage(92.5)
  
  // 4. 验证异常检测详情
  let error_rate_anomalies = anomalies.filter(fn(point) { point.metric_name == "error.rate" })
  let response_time_anomalies = anomalies.filter(fn(point) { point.metric_name == "response.time" })
  let cpu_anomalies = anomalies.filter(fn(point) { point.metric_name == "cpu.usage" })
  
  assert_eq(error_rate_anomalies.length(), 1)
  assert_eq(response_time_anomalies.length(), 1)
  assert_eq(cpu_anomalies.length(), 1)
  
  // 5. 生成告警
  let alerts = anomalies.map(fn(point) {
    Alert({
      id: "alert-" + point.metric_name + "-" + point.timestamp.to_string(),
      severity: if point.metric_name == "cpu.usage" { Critical } else { Warning },
      title: point.metric_name + " threshold exceeded",
      description: point.metric_name + " value " + point.value.to_string() + " exceeds threshold " + point.threshold.to_string(),
      source_labels: point.labels,
      timestamp: point.timestamp
    })
  })
  
  assert_eq(alerts.length(), 3)
  
  // 6. 验证告警严重级别
  let critical_alerts = alerts.filter(fn(alert) { alert.severity == Critical })
  let warning_alerts = alerts.filter(fn(alert) { alert.severity == Warning })
  
  assert_eq(critical_alerts.length(), 1)
  assert_eq(warning_alerts.length(), 2)
  
  // 7. 验证告警源标签
  for alert in alerts {
    assert_true(alert.source_labels.length() > 0)
    
    if alert.title.contains("error.rate") {
      let service_label = alert.source_labels.filter(fn(label) { label.0 == "service" })
      assert_eq(service_label.length(), 1)
      assert_eq(service_label[0].1, "api")
    }
    
    if alert.title.contains("cpu.usage") {
      let host_label = alert.source_labels.filter(fn(label) { label.0 == "host" })
      assert_eq(host_label.length(), 1)
      assert_eq(host_label[0].1, "server-1")
    }
  }
}

// 测试6: 数据聚合与统计
test "数据聚合与统计" {
  // 1. 创建时间序列数据
  let time_series_data = [
    TimeSeriesPoint({
      timestamp: 1735689600000000000L,
      metric: "request.count",
      value: 100,
      tags: [("service", "auth"), ("method", "POST")]
    }),
    TimeSeriesPoint({
      timestamp: 1735689600600000000L,
      metric: "request.count",
      value: 120,
      tags: [("service", "auth"), ("method", "POST")]
    }),
    TimeSeriesPoint({
      timestamp: 1735689601200000000L,
      metric: "request.count",
      value: 95,
      tags: [("service", "auth"), ("method", "POST")]
    }),
    TimeSeriesPoint({
      timestamp: 1735689600000000000L,
      metric: "request.count",
      value: 200,
      tags: [("service", "users"), ("method", "GET")]
    }),
    TimeSeriesPoint({
      timestamp: 1735689600600000000L,
      metric: "request.count",
      value: 180,
      tags: [("service", "users"), ("method", "GET")]
    })
  ]
  
  // 2. 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  
  // 3. 按服务分组
  let auth_service_data = time_series_data.filter(fn(point) {
    let service_tag = point.tags.filter(fn(tag) { tag.0 == "service" && tag.1 == "auth" })
    service_tag.length() > 0
  })
  
  let users_service_data = time_series_data.filter(fn(point) {
    let service_tag = point.tags.filter(fn(tag) { tag.0 == "service" && tag.1 == "users" })
    service_tag.length() > 0
  })
  
  assert_eq(auth_service_data.length(), 3)
  assert_eq(users_service_data.length(), 2)
  
  // 4. 计算每个服务的总和
  let auth_total = auth_service_data.reduce(fn(acc, point) { acc + point.value }, 0)
  let users_total = users_service_data.reduce(fn(acc, point) { acc + point.value }, 0)
  
  assert_eq(auth_total, 315)  // 100 + 120 + 95
  assert_eq(users_total, 380) // 200 + 180
  
  // 5. 计算每个服务的平均值
  let auth_avg = auth_total / auth_service_data.length()
  let users_avg = users_total / users_service_data.length()
  
  assert_eq(auth_avg, 105)
  assert_eq(users_avg, 190)
  
  // 6. 计算每个服务的最大值和最小值
  let auth_max = auth_service_data.reduce(fn(acc, point) { 
    if point.value > acc { point.value } else { acc } 
  }, 0)
  
  let auth_min = auth_service_data.reduce(fn(acc, point) { 
    if point.value < acc { point.value } else { acc } 
  }, 1000)
  
  assert_eq(auth_max, 120)
  assert_eq(auth_min, 95)
  
  // 7. 计算全局统计
  let global_total = time_series_data.reduce(fn(acc, point) { acc + point.value }, 0)
  let global_avg = global_total / time_series_data.length()
  let global_max = time_series_data.reduce(fn(acc, point) { 
    if point.value > acc { point.value } else { acc } 
  }, 0)
  
  assert_eq(global_total, 695)  // 315 + 380
  assert_eq(global_avg, 139)    // 695 / 5
  assert_eq(global_max, 200)
  
  // 8. 按方法分组统计
  let post_method_data = time_series_data.filter(fn(point) {
    let method_tag = point.tags.filter(fn(tag) { tag.0 == "method" && tag.1 == "POST" })
    method_tag.length() > 0
  })
  
  let get_method_data = time_series_data.filter(fn(point) {
    let method_tag = point.tags.filter(fn(tag) { tag.0 == "method" && tag.1 == "GET" })
    method_tag.length() > 0
  })
  
  assert_eq(post_method_data.length(), 3)
  assert_eq(get_method_data.length(), 2)
  
  let post_total = post_method_data.reduce(fn(acc, point) { acc + point.value }, 0)
  let get_total = get_method_data.reduce(fn(acc, point) { acc + point.value }, 0)
  
  assert_eq(post_total, 315)
  assert_eq(get_total, 380)
}

// 测试7: 性能基准分析
test "性能基准分析" {
  // 1. 创建性能测试数据
  let performance_metrics = [
    PerformanceMetric({
      name: "throughput",
      value: 1500.0,
      unit: "requests/sec",
      test_scenario: "baseline",
      timestamp: 1735689600000000000L
    }),
    PerformanceMetric({
      name: "throughput",
      value: 1650.0,
      unit: "requests/sec",
      test_scenario: "optimized",
      timestamp: 1735689600100000000L
    }),
    PerformanceMetric({
      name: "latency.p99",
      value: 250.0,
      unit: "ms",
      test_scenario: "baseline",
      timestamp: 1735689600000000000L
    }),
    PerformanceMetric({
      name: "latency.p99",
      value: 180.0,
      unit: "ms",
      test_scenario: "optimized",
      timestamp: 1735689600100000000L
    }),
    PerformanceMetric({
      name: "memory.peak",
      value: 512.0,
      unit: "MB",
      test_scenario: "baseline",
      timestamp: 1735689600000000000L
    }),
    PerformanceMetric({
      name: "memory.peak",
      value: 384.0,
      unit: "MB",
      test_scenario: "optimized",
      timestamp: 1735689600100000000L
    })
  ]
  
  // 2. 验证性能指标
  assert_eq(performance_metrics.length(), 6)
  
  // 3. 按测试场景分组
  let baseline_metrics = performance_metrics.filter(fn(metric) { metric.test_scenario == "baseline" })
  let optimized_metrics = performance_metrics.filter(fn(metric) { metric.test_scenario == "optimized" })
  
  assert_eq(baseline_metrics.length(), 3)
  assert_eq(optimized_metrics.length(), 3)
  
  // 4. 按指标名称分组
  let throughput_metrics = performance_metrics.filter(fn(metric) { metric.name == "throughput" })
  let latency_metrics = performance_metrics.filter(fn(metric) { metric.name == "latency.p99" })
  let memory_metrics = performance_metrics.filter(fn(metric) { metric.name == "memory.peak" })
  
  assert_eq(throughput_metrics.length(), 2)
  assert_eq(latency_metrics.length(), 2)
  assert_eq(memory_metrics.length(), 2)
  
  // 5. 计算性能改进
  let baseline_throughput = throughput_metrics.filter(fn(m) { m.test_scenario == "baseline" })[0].value
  let optimized_throughput = throughput_metrics.filter(fn(m) { m.test_scenario == "optimized" })[0].value
  let throughput_improvement = (optimized_throughput - baseline_throughput) / baseline_throughput * 100.0
  
  assert_eq(baseline_throughput, 1500.0)
  assert_eq(optimized_throughput, 1650.0)
  assert_eq(throughput_improvement, 10.0)
  
  // 6. 计算延迟改进
  let baseline_latency = latency_metrics.filter(fn(m) { m.test_scenario == "baseline" })[0].value
  let optimized_latency = latency_metrics.filter(fn(m) { m.test_scenario == "optimized" })[0].value
  let latency_improvement = (baseline_latency - optimized_latency) / baseline_latency * 100.0
  
  assert_eq(baseline_latency, 250.0)
  assert_eq(optimized_latency, 180.0)
  assert_eq(latency_improvement, 28.0)
  
  // 7. 计算内存使用改进
  let baseline_memory = memory_metrics.filter(fn(m) { m.test_scenario == "baseline" })[0].value
  let optimized_memory = memory_metrics.filter(fn(m) { m.test_scenario == "optimized" })[0].value
  let memory_improvement = (baseline_memory - optimized_memory) / baseline_memory * 100.0
  
  assert_eq(baseline_memory, 512.0)
  assert_eq(optimized_memory, 384.0)
  assert_eq(memory_improvement, 25.0)
  
  // 8. 生成性能报告
  let performance_report = PerformanceReport({
    baseline_scenario: "baseline",
    optimized_scenario: "optimized",
    improvements: [
      ("throughput", throughput_improvement, "%"),
      ("latency.p99", latency_improvement, "%"),
      ("memory.peak", memory_improvement, "%")
    ],
    summary: "Overall performance improved by " + ((throughput_improvement + latency_improvement + memory_improvement) / 3.0).to_string() + "%"
  })
  
  assert_eq(performance_report.improvements.length(), 3)
  assert_true(performance_report.summary.contains("21.0"))  // Average improvement
}

// 测试8: 数据质量验证
test "数据质量验证" {
  // 1. 创建测试数据集
  let dataset = [
    DataPoint({
      id: "dp-001",
      timestamp: 1735689600000000000L,
      metric: "cpu.usage",
      value: 45.5,
      quality_score: 0.95,
      labels: [("host", "server-1"), ("region", "us-east-1")]
    }),
    DataPoint({
      id: "dp-002",
      timestamp: 1735689600100000000L,
      metric: "memory.usage",
      value: 78.2,
      quality_score: 0.88,
      labels: [("host", "server-1"), ("region", "us-east-1")]
    }),
    DataPoint({
      id: "dp-003",
      timestamp: 1735689600200000000L,
      metric: "disk.usage",
      value: 65.0,
      quality_score: 0.92,
      labels: [("host", "server-2"), ("region", "us-west-1")]
    }),
    DataPoint({
      id: "dp-004",
      timestamp: 1735689600300000000L,
      metric: "network.throughput",
      value: 1024.5,
      quality_score: 0.75,
      labels: [("host", "server-2"), ("region", "us-west-1")]
    }),
    DataPoint({
      id: "dp-005",
      timestamp: 1735689600400000000L,
      metric: "response.time",
      value: 150.0,
      quality_score: 0.98,
      labels: [("host", "server-1"), ("region", "us-east-1")]
    })
  ]
  
  // 2. 验证数据集
  assert_eq(dataset.length(), 5)
  
  // 3. 计算数据质量统计
  let quality_scores = dataset.map(fn(point) { point.quality_score })
  let avg_quality = quality_scores.reduce(fn(acc, score) { acc + score }, 0.0) / quality_scores.length()
  
  assert_true(avg_quality > 0.85 && avg_quality < 0.95)
  
  // 4. 识别低质量数据点
  let low_quality_threshold = 0.8
  let low_quality_points = dataset.filter(fn(point) { point.quality_score < low_quality_threshold })
  
  assert_eq(low_quality_points.length(), 1)
  assert_eq(low_quality_points[0].id, "dp-004")
  assert_eq(low_quality_points[0].metric, "network.throughput")
  
  // 5. 识别高质量数据点
  let high_quality_threshold = 0.95
  let high_quality_points = dataset.filter(fn(point) { point.quality_score >= high_quality_threshold })
  
  assert_eq(high_quality_points.length(), 2)
  assert_eq(high_quality_points[0].id, "dp-001")
  assert_eq(high_quality_points[1].id, "dp-005")
  
  // 6. 按区域分组质量分析
  let us_east_data = dataset.filter(fn(point) {
    let region_tag = point.labels.filter(fn(label) { label.0 == "region" && label.1 == "us-east-1" })
    region_tag.length() > 0
  })
  
  let us_west_data = dataset.filter(fn(point) {
    let region_tag = point.labels.filter(fn(label) { label.0 == "region" && label.1 == "us-west-1" })
    region_tag.length() > 0
  })
  
  assert_eq(us_east_data.length(), 3)
  assert_eq(us_west_data.length(), 2)
  
  // 7. 计算区域平均质量
  let us_east_quality = us_east_data.reduce(fn(acc, point) { acc + point.quality_score }, 0.0) / us_east_data.length()
  let us_west_quality = us_west_data.reduce(fn(acc, point) { acc + point.quality_score }, 0.0) / us_west_data.length()
  
  assert_true(us_east_quality > 0.9)
  assert_true(us_west_quality > 0.8 && us_west_quality < 0.9)
  
  // 8. 验证数据完整性
  let complete_data_points = dataset.filter(fn(point) {
    point.id != "" && 
    point.timestamp > 0L && 
    point.metric != "" && 
    point.quality_score >= 0.0 && 
    point.quality_score <= 1.0 &&
    point.labels.length() > 0
  })
  
  assert_eq(complete_data_points.length(), dataset.length())
  
  // 9. 生成数据质量报告
  let quality_report = DataQualityReport({
    total_points: dataset.length(),
    avg_quality_score: avg_quality,
    high_quality_count: high_quality_points.length(),
    low_quality_count: low_quality_points.length(),
    quality_by_region: [
      ("us-east-1", us_east_quality),
      ("us-west-1", us_west_quality)
    ],
    recommendations: if low_quality_points.length() > 0 { 
      ["Investigate network.throughput data quality issues"] 
    } else { 
      [] 
    }
  })
  
  assert_eq(quality_report.total_points, 5)
  assert_eq(quality_report.high_quality_count, 2)
  assert_eq(quality_report.low_quality_count, 1)
  assert_eq(quality_report.recommendations.length(), 1)
}