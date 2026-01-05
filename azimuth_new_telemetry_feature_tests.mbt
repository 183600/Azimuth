// Azimuth New Telemetry Feature Tests
// 新增遥测功能测试用例

test "adaptive sampling strategy with dynamic thresholds" {
  // 测试自适应采样策略和动态阈值
  let adaptive_sampler = @azimuth.AdaptiveSampler {
    base_probability : 0.1,
    max_probability : 0.5,
    min_probability : 0.01,
    error_rate_threshold : 0.05,
    latency_threshold_ms : 1000,
    throughput_threshold : 10000,
    current_load : 7500,
    current_error_rate : 0.03,
    adjustment_factor : 0.1
  }
  
  // 计算当前采样概率
  let load_factor = adaptive_sampler.current_load as Float / adaptive_sampler.throughput_threshold as Float
  let error_factor = adaptive_sampler.current_error_rate / adaptive_sampler.error_rate_threshold
  
  let adjusted_probability = if load_factor > 0.8 or error_factor > 1.0 {
    // 高负载或高错误率时增加采样率
    let new_prob = adaptive_sampler.base_probability + adaptive_sampler.adjustment_factor
    if new_prob > adaptive_sampler.max_probability {
      adaptive_sampler.max_probability
    } else {
      new_prob
    }
  } else if load_factor < 0.3 and error_factor < 0.5 {
    // 低负载和低错误率时减少采样率
    let new_prob = adaptive_sampler.base_probability - adaptive_sampler.adjustment_factor
    if new_prob < adaptive_sampler.min_probability {
      adaptive_sampler.min_probability
    } else {
      new_prob
    }
  } else {
    adaptive_sampler.base_probability
  }
  
  // 验证采样概率调整
  assert_true(adjusted_probability >= adaptive_sampler.min_probability)
  assert_true(adjusted_probability <= adaptive_sampler.max_probability)
  assert_true(adjusted_probability > adaptive_sampler.base_probability) // 当前负载较高
  
  // 测试采样决策
  let trace_id = "1234567890abcdef1234567890abcdef"
  let sampling_decision = @azimuth.should_sample(adaptive_sampler, trace_id)
  
  // 基于调整后的概率进行采样决策
  let hash_value = @azimuth.hash_trace_id(trace_id)
  let normalized_hash = (hash_value as Float) / (@azimuth.MAX_HASH_VALUE as Float)
  let should_sample = normalized_hash < adjusted_probability
  
  assert_eq(should_sample, sampling_decision)
}

test "distributed trace parent-child relationships" {
  // 测试分布式追踪的父子关系
  let root_span = @azimuth.Span {
    context : @azimuth.SpanContext {
      trace_id : "abcdef1234567890abcdef1234567890",
      span_id : "root1234567890",
      sampled : true,
      trace_state : "sampling.decision=true"
    },
    parent_span_id : None,
    operation_name : "user.request",
    start_time : 1640995200000L,
    end_time : Some(1640995200500L),
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("user.id", @azimuth.StringValue("user123")),
      ("request.path", @azimuth.StringValue("/api/profile"))
    ],
    events : []
  }
  
  // 创建子span
  let child_span1 = @azimuth.create_child_span(root_span, "database.query")
  let child_span2 = @azimuth.create_child_span(root_span, "cache.lookup")
  
  // 验证父子关系
  assert_eq(child_span1.context.trace_id, root_span.context.trace_id)
  assert_eq(child_span2.context.trace_id, root_span.context.trace_id)
  assert_not_eq(child_span1.context.span_id, root_span.context.span_id)
  assert_not_eq(child_span2.context.span_id, root_span.context.span_id)
  assert_not_eq(child_span1.context.span_id, child_span2.context.span_id)
  
  match child_span1.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_span.context.span_id)
    None => assert_true(false)
  }
  
  match child_span2.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_span.context.span_id)
    None => assert_true(false)
  }
  
  // 创建嵌套子span
  let grandchild_span = @azimuth.create_child_span(child_span1, "sql.execute")
  
  // 验证嵌套关系
  assert_eq(grandchild_span.context.trace_id, root_span.context.trace_id)
  assert_not_eq(grandchild_span.context.span_id, child_span1.context.span_id)
  
  match grandchild_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, child_span1.context.span_id)
    None => assert_true(false)
  }
  
  // 验证span层次结构
  let span_tree = @azimuth.build_span_tree([root_span, child_span1, child_span2, grandchild_span])
  assert_eq(span_tree.root.context.span_id, root_span.context.span_id)
  assert_eq(span_tree.children.length(), 2)
  assert_eq(span_tree.children[0].span.context.span_id, child_span1.context.span_id)
  assert_eq(span_tree.children[1].span.context.span_id, child_span2.context.span_id)
  assert_eq(span_tree.children[0].children.length(), 1)
  assert_eq(span_tree.children[0].children[0].span.context.span_id, grandchild_span.context.span_id)
}

test "metric data type conversion and aggregation" {
  // 测试指标数据类型转换和聚合
  let raw_metrics = [
    ("cpu.usage", @azimuth.FloatValue(0.65)),
    ("memory.available", @azimuth.IntValue(2147483648)),
    ("request.count", @azimuth.IntValue(1250)),
    ("response.time", @azimuth.FloatValue(125.5)),
    ("error.rate", @azimuth.FloatValue(0.02)),
    ("throughput", @azimuth.FloatValue(850.75))
  ]
  
  // 测试类型转换
  let convert_to_double = fn(metric: @azimuth.MetricValue) {
    match metric {
      @azimuth.IntValue(v) => v as Float
      @azimuth.FloatValue(v) => v
      @azimuth.StringValue(_) => 0.0
      @azimuth.BoolValue(v) => if v { 1.0 } else { 0.0 }
    }
  }
  
  let converted_metrics = raw_metrics.map(fn(item) {
    (item.0, convert_to_double(item.1))
  })
  
  // 验证转换结果
  let cpu_usage = converted_metrics.filter(fn(m) { m.0 == "cpu.usage" })[0]
  assert_eq(cpu_usage.1, 0.65)
  
  let memory_available = converted_metrics.filter(fn(m) { m.0 == "memory.available" })[0]
  assert_eq(memory_available.1, 2147483648.0)
  
  // 测试指标聚合
  let numeric_metrics = converted_metrics.filter(fn(m) {
    m.0 != "error.rate" // 排除错误率，它已经是百分比
  })
  
  let sum_values = numeric_metrics.reduce(fn(acc, m) { acc + m.1 }, 0.0)
  let avg_value = sum_values / (numeric_metrics.length() as Float)
  let max_value = numeric_metrics.reduce(fn(acc, m) { if m.1 > acc { m.1 } else { acc } }, 0.0)
  let min_value = numeric_metrics.reduce(fn(acc, m) { if m.1 < acc { m.1 } else { acc } }, 9999999999.0)
  
  // 验证聚合结果
  assert_true(sum_values > 0.0)
  assert_true(avg_value > 0.0)
  assert_true(max_value >= min_value)
  assert_eq(max_value, 2147483648.0) // memory.available应该是最大值
  
  // 测试时间窗口聚合
  let time_series_metrics = [
    (1640995200L, 100.0), // 0s
    (1640995201L, 105.0), // 1s
    (1640995202L, 98.0),  // 2s
    (1640995203L, 110.0), // 3s
    (1640995204L, 102.0)  // 4s
  ]
  
  let windowed_avg = @azimuth.calculate_time_window_average(time_series_metrics, 1640995200L, 1640995204L)
  let expected_avg = (100.0 + 105.0 + 98.0 + 110.0 + 102.0) / 5.0
  assert_eq(windowed_avg, expected_avg)
}

test "log level filtering and structured parsing" {
  // 测试日志级别过滤和结构化解析
  let log_entries = [
    @azimuth.LogEntry {
      timestamp : 1640995200000L,
      level : @azimuth.LogLevel::Debug,
      message : "Initializing connection pool",
      attributes : [
        ("pool.size", @azimuth.IntValue(10)),
        ("timeout", @azimuth.IntValue(5000))
      ]
    },
    @azimuth.LogEntry {
      timestamp : 1640995200100L,
      level : @azimuth.LogLevel::Info,
      message : "User authenticated successfully",
      attributes : [
        ("user.id", @azimuth.StringValue("user123")),
        ("auth.method", @azimuth.StringValue("oauth2"))
      ]
    },
    @azimuth.LogEntry {
      timestamp : 1640995200200L,
      level : @azimuth.LogLevel::Warn,
      message : "High memory usage detected",
      attributes : [
        ("memory.usage", @azimuth.FloatValue(0.85)),
        ("threshold", @azimuth.FloatValue(0.8))
      ]
    },
    @azimuth.LogEntry {
      timestamp : 1640995200300L,
      level : @azimuth.LogLevel::Error,
      message : "Database connection failed",
      attributes : [
        ("error.code", @azimuth.StringValue("CONN_TIMEOUT")),
        ("retry.count", @azimuth.IntValue(3))
      ]
    }
  ]
  
  // 测试日志级别过滤
  let filter_by_level = fn(entries: Array[@azimuth.LogEntry], min_level: @azimuth.LogLevel) {
    entries.filter(fn(entry) {
      @azimuth.compare_log_levels(entry.level, min_level) >= 0
    })
  }
  
  let warn_and_above = filter_by_level(log_entries, @azimuth.LogLevel::Warn)
  assert_eq(warn_and_above.length(), 2)
  assert_eq(warn_and_above[0].level, @azimuth.LogLevel::Warn)
  assert_eq(warn_and_above[1].level, @azimuth.LogLevel::Error)
  
  let info_and_above = filter_by_level(log_entries, @azimuth.LogLevel::Info)
  assert_eq(info_and_above.length(), 3)
  
  // 测试结构化日志解析
  let parse_json_log = fn(json_str: String) {
    // 简化的JSON解析逻辑
    let log_obj = @azimuth.parse_json_to_log(json_str)
    log_obj
  }
  
  let json_log = "{\"timestamp\":1640995200400,\"level\":\"INFO\",\"message\":\"Request processed\",\"attributes\":{\"duration\":150,\"status\":200}}"
  let parsed_log = parse_json_log(json_log)
  
  assert_eq(parsed_log.level, @azimuth.LogLevel::Info)
  assert_eq(parsed_log.message, "Request processed")
  assert_true(parsed_log.attributes.length() > 0)
  
  // 测试日志模式匹配
  let find_error_patterns = fn(entries: Array[@azimuth.LogEntry]) {
    entries.filter(fn(entry) {
      entry.level == @azimuth.LogLevel::Error or
      entry.message.contains("failed") or
      entry.message.contains("error") or
      entry.attributes.any(fn(attr) {
        match attr.0 {
          "error.code" | "error.type" | "error.message" => true
          _ => false
        }
      })
    })
  }
  
  let error_entries = find_error_patterns(log_entries)
  assert_eq(error_entries.length(), 1)
  assert_eq(error_entries[0].level, @azimuth.LogLevel::Error)
  
  // 测试日志聚合统计
  let log_stats = @azimuth.calculate_log_statistics(log_entries)
  assert_eq(log_stats.total_count, 4)
  assert_eq(log_stats.debug_count, 1)
  assert_eq(log_stats.info_count, 1)
  assert_eq(log_stats.warn_count, 1)
  assert_eq(log_stats.error_count, 1)
  assert_true(log_stats.error_rate > 0.0)
}

test "resource monitoring and alerting thresholds" {
  // 测试资源监控和告警阈值
  let resource_monitor = @azimuth.ResourceMonitor {
    metrics : [
      ("cpu.usage", @azimuth.FloatValue(0.75)),
      ("memory.usage", @azimuth.FloatValue(0.82)),
      ("disk.usage", @azimuth.FloatValue(0.45)),
      ("network.in", @azimuth.FloatValue(1024.5)),
      ("network.out", @azimuth.FloatValue(2048.0)),
      ("connection.count", @azimuth.IntValue(850)),
      ("thread.active", @azimuth.IntValue(45))
    ],
    thresholds : [
      ("cpu.usage", @azimuth.FloatValue(0.8)),
      ("memory.usage", @azimuth.FloatValue(0.85)),
      ("disk.usage", @azimuth.FloatValue(0.9)),
      ("connection.count", @azimuth.IntValue(1000))
    ],
    alert_rules : [
      @azimuth.AlertRule {
        name : "high_cpu_usage",
        condition : "cpu.usage > 0.8",
        severity : @azimuth.AlertSeverity::Warning,
        duration : 300000L // 5分钟
      },
      @azimuth.AlertRule {
        name : "high_memory_usage",
        condition : "memory.usage > 0.85",
        severity : @azimuth.AlertSeverity::Critical,
        duration : 120000L // 2分钟
      }
    ]
  }
  
  // 测试阈值检查
  let check_thresholds = fn(monitor: @azimuth.ResourceMonitor) {
    let violations = []
    for metric in monitor.metrics {
      match monitor.thresholds.find(fn(threshold) { threshold.0 == metric.0 }) {
        Some(threshold) => {
          let metric_value = metric.1
          let threshold_value = threshold.1
          
          let is_violation = match (metric_value, threshold_value) {
            (@azimuth.FloatValue(m), @azimuth.FloatValue(t)) => m > t
            (@azimuth.IntValue(m), @azimuth.IntValue(t)) => m > t
            _ => false
          }
          
          if is_violation {
            violations.push((metric.0, metric_value, threshold_value))
          }
        }
        None => {}
      }
    }
    violations
  }
  
  let threshold_violations = check_thresholds(resource_monitor)
  assert_eq(threshold_violations.length(), 0) // 当前没有违反阈值
  
  // 模拟资源使用率增加
  let updated_metrics = resource_monitor.metrics.map(fn(metric) {
    match metric.0 {
      "cpu.usage" => ("cpu.usage", @azimuth.FloatValue(0.85))
      "memory.usage" => ("memory.usage", @azimuth.FloatValue(0.87))
      _ => metric
    }
  })
  
  let updated_monitor = { resource_monitor | metrics: updated_metrics }
  let new_violations = check_thresholds(updated_monitor)
  assert_eq(new_violations.length(), 2) // CPU和内存都超过阈值
  
  // 测试告警规则评估
  let evaluate_alert_rules = fn(monitor: @azimuth.ResourceMonitor) {
    let active_alerts = []
    for rule in monitor.alert_rules {
      let condition_met = @azimuth.evaluate_condition(rule.condition, monitor.metrics)
      if condition_met {
        active_alerts.push(rule)
      }
    }
    active_alerts
  }
  
  let active_alerts = evaluate_alert_rules(updated_monitor)
  assert_eq(active_alerts.length(), 2)
  assert_true(active_alerts.any(fn(alert) { alert.name == "high_cpu_usage" }))
  assert_true(active_alerts.any(fn(alert) { alert.name == "high_memory_usage" }))
  
  // 测试告警去重和聚合
  let alert_history = [
    @azimuth.Alert {
      id : "alert-001",
      rule_name : "high_cpu_usage",
      severity : @azimuth.AlertSeverity::Warning,
      start_time : 1640995200000L,
      end_time : None,
      message : "CPU usage is above 80%"
    },
    @azimuth.Alert {
      id : "alert-002",
      rule_name : "high_memory_usage",
      severity : @azimuth.AlertSeverity::Critical,
      start_time : 1640995200100L,
      end_time : None,
      message : "Memory usage is above 85%"
    }
  ]
  
  let deduplicated_alerts = @azimuth.deduplicate_alerts(alert_history ++ [
    @azimuth.Alert {
      id : "alert-003",
      rule_name : "high_cpu_usage",
      severity : @azimuth.AlertSeverity::Warning,
      start_time : 1640995200200L,
      end_time : None,
      message : "CPU usage is above 80%"
    }
  ])
  
  assert_eq(deduplicated_alerts.length(), 2) // CPU告警应该被去重
}

test "service mesh telemetry collection" {
  // 测试服务网格遥测收集
  let mesh_telemetry = @azimuth.MeshTelemetry {
    service_mesh : "istio",
    namespace : "production",
    services : [
      @azimuth.MeshService {
        name : "frontend",
        version : "v1.2.3",
        pods : 3,
        replicas : 3,
        endpoints : [
          "10.0.1.10:8080",
          "10.0.1.11:8080",
          "10.0.1.12:8080"
        ],
        metrics : [
          ("request.count", @azimuth.IntValue(15000)),
          ("error.rate", @azimuth.FloatValue(0.02)),
          ("latency.p95", @azimuth.FloatValue(150.0))
        ]
      },
      @azimuth.MeshService {
        name : "backend",
        version : "v2.1.0",
        pods : 5,
        replicas : 5,
        endpoints : [
          "10.0.1.20:9090",
          "10.0.1.21:9090",
          "10.0.1.22:9090",
          "10.0.1.23:9090",
          "10.0.1.24:9090"
        ],
        metrics : [
          ("request.count", @azimuth.IntValue(45000)),
          ("error.rate", @azimuth.FloatValue(0.01)),
          ("latency.p95", @azimuth.FloatValue(85.0))
        ]
      }
    ],
    traffic_policies : [
      @azimuth.TrafficPolicy {
        name : "frontend-to-backend",
        source : "frontend",
        destination : "backend",
        protocol : "http",
        port : 8080,
        timeout : 30,
        retries : 3,
        circuit_breaker : @azimuth.CircuitBreakerConfig {
          consecutive_errors : 5,
          interval : 300,
          base_ejection_time : 30
        }
      }
    ]
  }
  
  // 验证服务网格结构
  assert_eq(mesh_telemetry.service_mesh, "istio")
  assert_eq(mesh_telemetry.namespace, "production")
  assert_eq(mesh_telemetry.services.length(), 2)
  
  // 验证服务指标
  let frontend_service = mesh_telemetry.services[0]
  assert_eq(frontend_service.name, "frontend")
  assert_eq(frontend_service.pods, 3)
  assert_eq(frontend_service.replicas, 3)
  assert_eq(frontend_service.endpoints.length(), 3)
  
  let frontend_requests = frontend_service.metrics.filter(fn(m) { m.0 == "request.count" })[0]
  match frontend_requests.1 {
    @azimuth.IntValue(v) => assert_eq(v, 15000)
    _ => assert_true(false)
  }
  
  // 验证流量策略
  assert_eq(mesh_telemetry.traffic_policies.length(), 1)
  let traffic_policy = mesh_telemetry.traffic_policies[0]
  assert_eq(traffic_policy.source, "frontend")
  assert_eq(traffic_policy.destination, "backend")
  assert_eq(traffic_policy.protocol, "http")
  assert_eq(traffic_policy.port, 8080)
  
  // 测试服务依赖关系
  let service_dependencies = @azimuth.extract_service_dependencies(mesh_telemetry)
  assert_eq(service_dependencies.length(), 1)
  assert_eq(service_dependencies[0].from, "frontend")
  assert_eq(service_dependencies[0].to, "backend")
  
  // 测试网格级别的聚合指标
  let total_requests = mesh_telemetry.services.reduce(fn(acc, service) {
    let request_metric = service.metrics.find(fn(m) { m.0 == "request.count" })
    match request_metric {
      Some((_, @azimuth.IntValue(v))) => acc + v
      _ => acc
    }
  }, 0)
  
  assert_eq(total_requests, 60000) // 15000 + 45000
  
  let avg_error_rate = mesh_telemetry.services.reduce(fn(acc, service) {
    let error_metric = service.metrics.find(fn(m) { m.0 == "error.rate" })
    match error_metric {
      Some((_, @azimuth.FloatValue(v))) => acc + v
      _ => acc
    }
  }, 0.0) / (mesh_telemetry.services.length() as Float)
  
  assert_eq(avg_error_rate, 0.015) // (0.02 + 0.01) / 2
}

test "custom event handling and correlation" {
  // 测试自定义事件处理和关联
  let event_processor = @azimuth.EventProcessor {
    correlation_rules : [
      @azimuth.CorrelationRule {
        name : "user_journey",
        condition : "event.type = 'user_action' AND user.id IS NOT NULL",
        correlation_window : 1800000L, // 30分钟
        correlation_attributes : ["user.id", "session.id"]
      },
      @azimuth.CorrelationRule {
        name : "error_chain",
        condition : "event.type = 'error'",
        correlation_window : 600000L, // 10分钟
        correlation_attributes : ["trace.id", "error.code"]
      }
    ],
    event_handlers : [
      @azimuth.EventHandler {
        name : "security_analyzer",
        event_types : ["login", "logout", "permission.denied"],
        action : @azimuth.HandlerAction::Analyze
      },
      @azimuth.EventHandler {
        name : "performance_tracker",
        event_types : ["slow.query", "high.latency"],
        action : @azimuth.HandlerAction::Track
      }
    ]
  }
  
  // 创建测试事件
  let events = [
    @azimuth.CustomEvent {
      id : "event-001",
      timestamp : 1640995200000L,
      event_type : "user_action",
      attributes : [
        ("user.id", @azimuth.StringValue("user123")),
        ("session.id", @azimuth.StringValue("session-456")),
        ("action", @azimuth.StringValue("login")),
        ("ip.address", @azimuth.StringValue("192.168.1.100"))
      ]
    },
    @azimuth.CustomEvent {
      id : "event-002",
      timestamp : 1640995200300L,
      event_type : "page_view",
      attributes : [
        ("user.id", @azimuth.StringValue("user123")),
        ("session.id", @azimuth.StringValue("session-456")),
        ("page", @azimuth.StringValue("/dashboard"))
      ]
    },
    @azimuth.CustomEvent {
      id : "event-003",
      timestamp : 1640995200600L,
      event_type : "error",
      attributes : [
        ("trace.id", @azimuth.StringValue("trace-789")),
        ("error.code", @azimuth.StringValue("DB_TIMEOUT")),
        ("error.message", @azimuth.StringValue("Database connection timeout"))
      ]
    },
    @azimuth.CustomEvent {
      id : "event-004",
      timestamp : 1640995200900L,
      event_type : "error",
      attributes : [
        ("trace.id", @azimuth.StringValue("trace-789")),
        ("error.code", @azimuth.StringValue("DB_TIMEOUT")),
        ("retry.attempt", @azimuth.IntValue(1))
      ]
    }
  ]
  
  // 测试事件关联
  let correlate_events = fn(processor: @azimuth.EventProcessor, events: Array[@azimuth.CustomEvent]) {
    let correlated_groups = []
    
    for rule in processor.correlation_rules {
      let matching_events = events.filter(fn(event) {
        @azimuth.evaluate_event_condition(rule.condition, event)
      })
      
      if matching_events.length() > 1 {
        let groups = @azimuth.group_events_by_attributes(matching_events, rule.correlation_attributes)
        correlated_groups = correlated_groups + groups
      }
    }
    
    correlated_groups
  }
  
  let correlated_groups = correlate_events(event_processor, events)
  assert_eq(correlated_groups.length(), 2) // 用户旅程和错误链各一个组
  
  // 验证用户旅程关联
  let user_journey = correlated_groups.filter(fn(group) { group.correlation_type == "user_journey" })[0]
  assert_eq(user_journey.events.length(), 2) // login和page_view
  assert_true(user_journey.events.any(fn(event) { event.event_type == "user_action" }))
  assert_true(user_journey.events.any(fn(event) { event.event_type == "page_view" }))
  
  // 验证错误链关联
  let error_chain = correlated_groups.filter(fn(group) { group.correlation_type == "error_chain" })[0]
  assert_eq(error_chain.events.length(), 2) // 两个DB_TIMEOUT错误
  assert_true(error_chain.events.all(fn(event) { event.event_type == "error" }))
  
  // 测试事件处理
  let process_events = fn(processor: @azimuth.EventProcessor, events: Array[@azimuth.CustomEvent]) {
    let processing_results = []
    
    for event in events {
      let matching_handlers = processor.event_handlers.filter(fn(handler) {
        handler.event_types.any(fn(event_type) { event_type == event.event_type })
      })
      
      for handler in matching_handlers {
        let result = @azimuth.apply_event_handler(handler, event)
        processing_results.push((event.id, handler.name, result))
      }
    }
    
    processing_results
  }
  
  let processing_results = process_events(event_processor, events)
  assert_true(processing_results.length() > 0)
  
  // 测试事件序列模式检测
  let detect_sequences = fn(events: Array[@azimuth.CustomEvent]) {
    let sequences = []
    
    // 检测登录失败序列
    let login_events = events.filter(fn(event) { event.event_type == "user_action" })
    let error_events = events.filter(fn(event) { event.event_type == "error" })
    
    for login_event in login_events {
      let subsequent_errors = error_events.filter(fn(error_event) {
        error_event.timestamp > login_event.timestamp and
        error_event.timestamp - login_event.timestamp < 60000L // 1分钟内
      })
      
      if subsequent_errors.length() > 0 {
        sequences.push({
          pattern : "login_followed_by_error",
          trigger_event : login_event.id,
          related_events : subsequent_errors.map(fn(e) { e.id })
        })
      }
    }
    
    sequences
  }
  
  let detected_sequences = detect_sequences(events)
  assert_eq(detected_sequences.length(), 1)
  assert_eq(detected_sequences[0].pattern, "login_followed_by_error")
}

test "telemetry data persistence and retention" {
  // 测试遥测数据持久化和保留策略
  let retention_policy = @azimuth.RetentionPolicy {
    trace_retention_days : 30,
    metric_retention_days : 90,
    log_retention_days : 7,
    event_retention_days : 14,
    max_storage_size_gb : 1000,
    compression_enabled : true,
    cold_storage_enabled : true,
    cold_storage_threshold_days : 7
  }
  
  // 创建测试数据
  let telemetry_data = [
    @azimuth.StorableTelemetry {
      id : "data-001",
      type : @azimuth.TelemetryType::Trace,
      timestamp : 1640995200000L, // 2022-01-01
      size_bytes : 1024,
      data : @azimuth.TraceData {
        trace_id : "trace-001",
        spans : [
          @azimuth.SpanSummary {
            span_id : "span-001",
            operation_name : "http.request",
            duration_ms : 150L
          }
        ]
      },
      compressed : false,
      storage_tier : @azimuth.StorageTier::Hot
    },
    @azimuth.StorableTelemetry {
      id : "data-002",
      type : @azimuth.TelemetryType::Metric,
      timestamp : 1640995200000L, // 2022-01-01
      size_bytes : 512,
      data : @azimuth.MetricData {
        name : "cpu.usage",
        value : 0.65,
        unit : "percent"
      },
      compressed : false,
      storage_tier : @azimuth.StorageTier::Hot
    },
    @azimuth.StorableTelemetry {
      id : "data-003",
      type : @azimuth.TelemetryType::Log,
      timestamp : 1640995200000L, // 2022-01-01
      size_bytes : 256,
      data : @azimuth.LogData {
        level : "INFO",
        message : "Application started"
      },
      compressed : false,
      storage_tier : @azimuth.StorageTier::Hot
    }
  ]
  
  // 测试数据压缩
  let compress_data = fn(data: @azimuth.StorableTelemetry) {
    if retention_policy.compression_enabled and not(data.compressed) {
      let compressed_size = (data.size_bytes as Float) * 0.7 // 假设压缩率为30%
      {
        data |
        size_bytes: compressed_size as Int,
        compressed: true
      }
    } else {
      data
    }
  }
  
  let compressed_data = telemetry_data.map(compress_data)
  assert_true(compressed_data.all(fn(data) { data.compressed }))
  
  // 验证压缩后的大小
  let original_total_size = telemetry_data.reduce(fn(acc, data) { acc + data.size_bytes }, 0)
  let compressed_total_size = compressed_data.reduce(fn(acc, data) { acc + data.size_bytes }, 0)
  assert_true(compressed_total_size < original_total_size)
  
  // 测试存储分层
  let current_timestamp = 1643673600L // 2022-02-01 (31天后)
  let days_since_epoch = current_timestamp / 86400000L
  
  let assign_storage_tier = fn(data: @azimuth.StorableTelemetry) {
    let data_age_days = (days_since_epoch - (data.timestamp / 86400000L))
    
    if retention_policy.cold_storage_enabled and data_age_days >= retention_policy.cold_storage_threshold_days {
      { data | storage_tier: @azimuth.StorageTier::Cold }
    } else {
      data
    }
  }
  
  let tiered_data = compressed_data.map(assign_storage_tier)
  
  // 验证冷存储分配
  let cold_storage_data = tiered_data.filter(fn(data) { data.storage_tier == @azimuth.StorageTier::Cold })
  assert_eq(cold_storage_data.length(), 3) // 所有数据都超过7天阈值
  
  // 测试数据保留策略
  let should_retain = fn(data: @azimuth.StorableTelemetry, policy: @azimuth.RetentionPolicy, current_time: Int) {
    let data_age_days = (current_time / 86400000L) - (data.timestamp / 86400000L)
    
    match data.type {
      @azimuth.TelemetryType::Trace => data_age_days <= policy.trace_retention_days
      @azimuth.TelemetryType::Metric => data_age_days <= policy.metric_retention_days
      @azimuth.TelemetryType::Log => data_age_days <= policy.log_retention_days
      @azimuth.TelemetryType::Event => data_age_days <= policy.event_retention_days
    }
  }
  
  // 测试不同时间点的保留状态
  let after_10_days = 1640995200000L + (10 * 86400000L)
  let after_40_days = 1640995200000L + (40 * 86400000L)
  let after_100_days = 1640995200000L + (100 * 86400000L)
  
  // 10天后，所有数据都应该保留
  let retained_after_10 = tiered_data.filter(fn(data) { should_retain(data, retention_policy, after_10_days) })
  assert_eq(retained_after_10.length(), 3)
  
  // 40天后，追踪数据应该被删除，但指标数据应该保留
  let retained_after_40 = tiered_data.filter(fn(data) { should_retain(data, retention_policy, after_40_days) })
  assert_eq(retained_after_40.length(), 1) // 只有指标数据
  assert_eq(retained_after_40[0].type, @azimuth.TelemetryType::Metric)
  
  // 100天后，所有数据都应该被删除
  let retained_after_100 = tiered_data.filter(fn(data) { should_retain(data, retention_policy, after_100_days) })
  assert_eq(retained_after_100.length(), 0)
  
  // 测试存储容量管理
  let total_storage_used = tiered_data.reduce(fn(acc, data) { acc + data.size_bytes }, 0)
  let storage_usage_percentage = (total_storage_used as Float) / (retention_policy.max_storage_size_gb * 1024 * 1024 * 1024 as Float)
  
  assert_true(storage_usage_percentage < 1.0) // 当前使用量应该小于最大容量
  
  // 模拟存储空间不足时的清理策略
  let cleanup_old_data = fn(data: Array[@azimuth.StorableTelemetry], policy: @azimuth.RetentionPolicy, current_time: Int) {
    let sorted_by_age = data.sort(fn(a, b) { a.timestamp < b.timestamp })
    let mut cleaned_data = []
    let mut current_size = 0
    let max_size = policy.max_storage_size_gb * 1024 * 1024 * 1024
    
    for item in sorted_by_age {
      if should_retain(item, policy, current_time) and current_size + item.size_bytes <= max_size {
        cleaned_data = cleaned_data.push(item)
        current_size = current_size + item.size_bytes
      }
    }
    
    cleaned_data
  }
  
  // 模拟大量数据占用存储空间
  let large_dataset = Array.range(0, 1000).map(fn(i) {
    @azimuth.StorableTelemetry {
      id: "data-" + i.to_string(),
      type: @azimuth.TelemetryType::Metric,
      timestamp: 1640995200000L + (i as Int * 86400000L),
      size_bytes: 1024 * 1024, // 1MB per item
      data: @azimuth.MetricData { name: "metric-" + i.to_string(), value: 0.5, unit: "percent" },
      compressed: true,
      storage_tier: @azimuth.StorageTier::Hot
    }
  })
  
  let cleaned_dataset = cleanup_old_data(large_dataset, retention_policy, after_10_days)
  assert_true(cleaned_dataset.length() < large_dataset.length())
  
  let cleaned_total_size = cleaned_dataset.reduce(fn(acc, data) { acc + data.size_bytes }, 0)
  assert_true(cleaned_total_size <= retention_policy.max_storage_size_gb * 1024 * 1024 * 1024)
}