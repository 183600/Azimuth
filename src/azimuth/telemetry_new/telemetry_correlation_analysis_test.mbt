// 遥测数据关联分析测试用例

test "trace_span_correlation_analysis" {
  // 测试追踪和跨度关联分析
  
  let trace_data = [
    {
      "trace_id": "trace_001",
      "spans": [
        {
          "span_id": "span_001",
          "parent_span_id": "",
          "operation": "api.gateway",
          "service": "gateway-service",
          "start_time": 1641018000000L,
          "end_time": 1641018000100L,
          "attributes": {"http.method": "GET", "http.url": "/api/users"}
        },
        {
          "span_id": "span_002", 
          "parent_span_id": "span_001",
          "operation": "user.service",
          "service": "user-service",
          "start_time": 1641018000050L,
          "end_time": 1641018000200L,
          "attributes": {"user.id": "12345", "operation": "get_user"}
        },
        {
          "span_id": "span_003",
          "parent_span_id": "span_002", 
          "operation": "database.query",
          "service": "user-db",
          "start_time": 1641018000100L,
          "end_time": 1641018000180L,
          "attributes": {"db.query": "SELECT * FROM users WHERE id = ?", "db.duration": "80"}
        }
      ]
    }
  ]
  
  // 验证追踪数据
  assert_eq(trace_data.length(), 1)
  assert_eq(trace_data[0]["spans"].length(), 3)
  
  // 分析跨度层次结构
  let mut span_hierarchy = []
  let spans = trace_data[0]["spans"]
  
  for span in spans {
    let span_id = span["span_id"]
    let parent_span_id = span["parent_span_id"]
    let operation = span["operation"]
    let service = span["service"]
    
    // 确定层次深度
    let mut depth = 0
    if parent_span_id == "" {
      depth = 0  // 根跨度
    } else {
      // 查找父跨度深度
      for parent in spans {
        if parent["span_id"] == parent_span_id {
          // 简化计算：这里假设我们已经知道父跨度深度
          if parent["parent_span_id"] == "" {
            depth = 1
          } else {
            depth = 2
          }
          break
        }
      }
    }
    
    span_hierarchy.push((span_id, parent_span_id, operation, service, depth))
  }
  
  // 验证跨度层次结构
  assert_eq(span_hierarchy.length(), 3)
  
  // 验证根跨度
  let root_spans = []
  for span in span_hierarchy {
    if span.1 == "" {
      root_spans.push(span)
    }
  }
  assert_eq(root_spans.length(), 1)
  assert_eq(root_spans[0].0, "span_001")
  assert_eq(root_spans[0].2, "api.gateway")
  assert_eq(root_spans[0].4, 0)  // 深度0
  
  // 验证子跨度关系
  let mut child_spans = []
  for span in span_hierarchy {
    if span.1 == "span_001" {
      child_spans.push(span)
    }
  }
  assert_eq(child_spans.length(), 1)
  assert_eq(child_spans[0].0, "span_002")
  assert_eq(child_spans[0].2, "user.service")
  assert_eq(child_spans[0].4, 1)  // 深度1
  
  // 分析服务调用链
  let mut service_chain = []
  for span in span_hierarchy {
    service_chain.push(span.3)  // service
  }
  
  // 验证服务调用链
  assert_eq(service_chain, ["gateway-service", "user-service", "user-db"])
  
  // 分析执行时间
  let mut execution_times = []
  for span in spans {
    let start_time = span["start_time"]
    let end_time = span["end_time"]
    let duration = end_time - start_time
    execution_times.push((span["span_id"], span["operation"], duration))
  }
  
  // 验证执行时间
  assert_eq(execution_times.length(), 3)
  
  // 计算总执行时间
  let total_trace_time = execution_times[0].2  // 根跨度的持续时间
  assert_eq(total_trace_time, 100L)  // 100ms
  
  // 计算各服务时间占比
  let gateway_time = execution_times[0].2
  let user_service_time = execution_times[1].2  
  let db_time = execution_times[2].2
  
  assert_eq(gateway_time, 100L)
  assert_eq(user_service_time, 150L)
  assert_eq(db_time, 80L)
  
  // 验证并发执行（用户服务和数据库查询有重叠）
  assert_eq(user_service_time > gateway_time, true)  // 用户服务执行时间超过网关总时间，说明有并发
}

test "cross_service_correlation_analysis" {
  // 测试跨服务关联分析
  
  let service_interactions = [
    {
      "timestamp": 1641018000000L,
      "source_service": "api-gateway",
      "target_service": "user-service", 
      "operation": "get_user",
      "trace_id": "trace_001",
      "request_id": "req_001",
      "latency": 50,
      "status": "success"
    },
    {
      "timestamp": 1641018000050L,
      "source_service": "user-service",
      "target_service": "database",
      "operation": "query_user",
      "trace_id": "trace_001", 
      "request_id": "req_001",
      "latency": 30,
      "status": "success"
    },
    {
      "timestamp": 1641018001000L,
      "source_service": "api-gateway",
      "target_service": "order-service",
      "operation": "create_order",
      "trace_id": "trace_002",
      "request_id": "req_002", 
      "latency": 80,
      "status": "success"
    },
    {
      "timestamp": 1641018001080L,
      "source_service": "order-service",
      "target_service": "payment-service",
      "operation": "process_payment",
      "trace_id": "trace_002",
      "request_id": "req_002",
      "latency": 120,
      "status": "success"
    },
    {
      "timestamp": 1641018002000L,
      "source_service": "api-gateway", 
      "target_service": "notification-service",
      "operation": "send_notification",
      "trace_id": "trace_003",
      "request_id": "req_003",
      "latency": 25,
      "status": "failed"
    }
  ]
  
  // 验证服务交互数据
  assert_eq(service_interactions.length(), 5)
  
  // 按追踪ID分组
  let mut trace_groups = []
  for interaction in service_interactions {
    let trace_id = interaction["trace_id"]
    let mut found_group = false
    
    // 查找现有分组
    let mut i = 0
    while i < trace_groups.length() {
      if trace_groups[i].0 == trace_id {
        trace_groups[i] = (trace_groups[i].0, trace_groups[i].1 + 1)
        found_group = true
        break
      }
      i = i + 1
    }
    
    // 创建新分组
    if not found_group {
      trace_groups.push((trace_id, 1))
    }
  }
  
  // 验证追踪分组
  assert_eq(trace_groups.length(), 3)
  assert_eq(trace_groups[0], ("trace_001", 2))
  assert_eq(trace_groups[1], ("trace_002", 2))
  assert_eq(trace_groups[2], ("trace_003", 1))
  
  // 分析服务依赖关系
  let mut service_dependencies = []
  
  for interaction in service_interactions {
    let source = interaction["source_service"]
    let target = interaction["target_service"]
    let operation = interaction["operation"]
    let latency = interaction["latency"]
    
    // 查找现有依赖关系
    let mut found_dependency = false
    let mut i = 0
    while i < service_dependencies.length() {
      if service_dependencies[i].0 == source and service_dependencies[i].1 == target {
        // 更新依赖统计
        service_dependencies[i] = (
          service_dependencies[i].0,
          service_dependencies[i].1, 
          service_dependencies[i].2 + 1,
          service_dependencies[i].3 + latency,
          service_dependencies[i].4 + [operation]
        )
        found_dependency = true
        break
      }
      i = i + 1
    }
    
    // 创建新依赖关系
    if not found_dependency {
      service_dependencies.push((source, target, 1, latency, [operation]))
    }
  }
  
  // 验证服务依赖关系
  assert_eq(service_dependencies.length(), 4)
  
  // 验证api-gateway依赖
  let mut gateway_dependencies = []
  for dep in service_dependencies {
    if dep.0 == "api-gateway" {
      gateway_dependencies.push(dep)
    }
  }
  assert_eq(gateway_dependencies.length(), 3)
  
  // 分析服务性能
  let mut service_performance = []
  
  for interaction in service_interactions {
    let service = interaction["source_service"]
    let latency = interaction["latency"]
    let status = interaction["status"]
    
    // 查找现有服务性能记录
    let mut found_service = false
    let mut i = 0
    while i < service_performance.length() {
      if service_performance[i].0 == service {
        // 更新性能统计
        service_performance[i] = (
          service_performance[i].0,
          service_performance[i].1 + 1,
          service_performance[i].2 + latency,
          service_performance[i].3 + (if status == "success" { 1 } else { 0 })
        )
        found_service = true
        break
      }
      i = i + 1
    }
    
    // 创建新服务性能记录
    if not found_service {
      service_performance.push((service, 1, latency, if status == "success" { 1 } else { 0 }))
    }
  }
  
  // 验证服务性能
  assert_eq(service_performance.length(), 4)
  
  // 计算平均延迟
  let mut performance_summary = []
  for perf in service_performance {
    let avg_latency = perf.2.to_double() / perf.1.to_double()
    let success_rate = perf.3.to_double() / perf.1.to_double()
    
    performance_summary.push((perf.0, avg_latency, success_rate))
  }
  
  // 验证性能摘要
  assert_eq(performance_summary.length(), 4)
  
  // 找出最慢的服务
  let mut slowest_service = performance_summary[0]
  for summary in performance_summary {
    if summary.1 > slowest_service.1 {
      slowest_service = summary
    }
  }
  assert_eq(slowest_service.0, "payment-service")  // 支付服务平均延迟最高
}

test "temporal_correlation_analysis" {
  // 测试时间关联分析
  
  let time_series_events = [
    {
      "timestamp": 1641018000000L,
      "event_type": "user.login",
      "user_id": "user_001",
      "session_id": "session_001",
      "ip_address": "192.168.1.100"
    },
    {
      "timestamp": 1641018000300L,
      "event_type": "page.view",
      "user_id": "user_001", 
      "session_id": "session_001",
      "page": "/dashboard"
    },
    {
      "timestamp": 1641018000600L,
      "event_type": "api.call",
      "user_id": "user_001",
      "session_id": "session_001", 
      "endpoint": "/api/user/profile"
    },
    {
      "timestamp": 1641018000900L,
      "event_type": "user.logout",
      "user_id": "user_001",
      "session_id": "session_001",
      "duration": 900
    },
    {
      "timestamp": 1641018005000L,
      "event_type": "user.login",
      "user_id": "user_002",
      "session_id": "session_002",
      "ip_address": "192.168.1.101"
    },
    {
      "timestamp": 1641018005300L,
      "event_type": "error.occurred",
      "user_id": "user_002",
      "session_id": "session_002",
      "error_code": "E500"
    }
  ]
  
  // 验证时间序列事件
  assert_eq(time_series_events.length(), 6)
  
  // 按会话分组
  let mut session_groups = []
  for event in time_series_events {
    let session_id = event["session_id"]
    let user_id = event["user_id"]
    
    // 查找现有会话组
    let mut found_session = false
    let mut i = 0
    while i < session_groups.length() {
      if session_groups[i].0 == session_id {
        session_groups[i] = (session_groups[i].0, session_groups[i].1, session_groups[i].2 + 1)
        found_session = true
        break
      }
      i = i + 1
    }
    
    // 创建新会话组
    if not found_session {
      session_groups.push((session_id, user_id, 1))
    }
  }
  
  // 验证会话分组
  assert_eq(session_groups.length(), 2)
  assert_eq(session_groups[0], ("session_001", "user_001", 4))
  assert_eq(session_groups[1], ("session_002", "user_002", 2))
  
  // 分析用户行为序列
  let user_001_events = []
  let user_002_events = []
  
  for event in time_series_events {
    if event["user_id"] == "user_001" {
      user_001_events.push(event)
    } else if event["user_id"] == "user_002" {
      user_002_events.push(event)
    }
  }
  
  // 验证用户事件
  assert_eq(user_001_events.length(), 4)
  assert_eq(user_002_events.length(), 2)
  
  // 分析用户001的行为模式
  let mut user_001_sequence = []
  for event in user_001_events {
    user_001_sequence.push(event["event_type"])
  }
  
  // 验证用户001行为序列
  assert_eq(user_001_sequence, ["user.login", "page.view", "api.call", "user.logout"])
  
  // 计算会话持续时间
  let session_001_start = user_001_events[0]["timestamp"]
  let session_001_end = user_001_events[3]["timestamp"]
  let session_001_duration = session_001_end - session_001_start
  
  assert_eq(session_001_duration, 900L)  // 900ms
  
  // 分析事件间隔
  let mut event_intervals = []
  let mut i = 1
  while i < user_001_events.length() {
    let prev_timestamp = user_001_events[i-1]["timestamp"]
    let curr_timestamp = user_001_events[i]["timestamp"]
    let interval = curr_timestamp - prev_timestamp
    
    event_intervals.push((
      user_001_events[i-1]["event_type"],
      user_001_events[i]["event_type"], 
      interval
    ))
    i = i + 1
  }
  
  // 验证事件间隔
  assert_eq(event_intervals.length(), 3)
  assert_eq(event_intervals[0], ("user.login", "page.view", 300L))
  assert_eq(event_intervals[1], ("page.view", "api.call", 300L))
  assert_eq(event_intervals[2], ("api.call", "user.logout", 300L))
  
  // 分析异常模式
  let error_events = []
  for event in time_series_events {
    if event["event_type"].has_prefix("error") {
      error_events.push(event)
    }
  }
  
  // 验证错误事件
  assert_eq(error_events.length(), 1)
  assert_eq(error_events[0]["user_id"], "user_002")
  assert_eq(error_events[0]["error_code"], "E500")
  
  // 分析错误发生前的行为
  let mut pre_error_events = []
  for event in time_series_events {
    if event["timestamp"] < error_events[0]["timestamp"] and event["user_id"] == "user_002" {
      pre_error_events.push(event)
    }
  }
  
  // 验证错误前行为
  assert_eq(pre_error_events.length(), 1)
  assert_eq(pre_error_events[0]["event_type"], "user.login")
  
  // 计算错误发生时间
  let login_time = pre_error_events[0]["timestamp"] 
  let error_time = error_events[0]["timestamp"]
  let time_to_error = error_time - login_time
  
  assert_eq(time_to_error, 300L)  // 登录后300ms发生错误
}

test "metric_log_correlation_analysis" {
  // 测试指标和日志关联分析
  
  let metric_data = [
    {
      "timestamp": 1641018000000L,
      "metric_name": "http.requests.total",
      "metric_value": 100.0,
      "service": "api-gateway",
      "tags": {"method": "GET", "status": "200"}
    },
    {
      "timestamp": 1641018000000L,
      "metric_name": "http.request.duration",
      "metric_value": 150.5,
      "service": "api-gateway", 
      "tags": {"method": "GET", "status": "200"}
    },
    {
      "timestamp": 1641018000300L,
      "metric_name": "http.requests.total",
      "metric_value": 101.0,
      "service": "api-gateway",
      "tags": {"method": "POST", "status": "500"}
    },
    {
      "timestamp": 1641018000300L,
      "metric_name": "http.request.duration", 
      "metric_value": 2500.0,
      "service": "api-gateway",
      "tags": {"method": "POST", "status": "500"}
    },
    {
      "timestamp": 1641018000600L,
      "metric_name": "cpu.usage",
      "metric_value": 85.2,
      "service": "user-service",
      "tags": {}
    }
  ]
  
  let log_data = [
    {
      "timestamp": 1641018000000L,
      "level": "INFO",
      "message": "Request processed successfully",
      "service": "api-gateway",
      "trace_id": "trace_001",
      "context": {"method": "GET", "status": "200", "duration": "150ms"}
    },
    {
      "timestamp": 1641018000300L,
      "level": "ERROR", 
      "message": "Internal server error occurred",
      "service": "api-gateway",
      "trace_id": "trace_002",
      "context": {"method": "POST", "status": "500", "error": "Database connection failed"}
    },
    {
      "timestamp": 1641018000600L,
      "level": "WARN",
      "message": "High CPU usage detected",
      "service": "user-service", 
      "trace_id": "trace_003",
      "context": {"cpu_usage": "85.2%"}
    }
  ]
  
  // 验证指标和日志数据
  assert_eq(metric_data.length(), 5)
  assert_eq(log_data.length(), 3)
  
  // 按时间窗口对齐数据（1秒窗口）
  let time_windows = [
    (1641018000000L, 1641018000999L),  // 窗口1
    (1641018001000L, 1641018001999L),  // 窗口2  
    (1641018002000L, 1641018002999L),  // 窗口3
    (1641018003000L, 1641018003999L),  // 窗口4
    (1641018004000L, 1641018004999L),  // 窗口5
    (1641018005000L, 1641018005999L),  // 窗口6
    (1641018006000L, 1641018006999L)   // 窗口7
  ]
  
  // 分析每个时间窗口的数据
  let mut window_correlations = []
  
  for window in time_windows {
    let window_start = window.0
    let window_end = window.1
    
    // 收集窗口内的指标
    let mut window_metrics = []
    for metric in metric_data {
      if metric["timestamp"] >= window_start and metric["timestamp"] <= window_end {
        window_metrics.push(metric)
      }
    }
    
    // 收集窗口内的日志
    let mut window_logs = []
    for log in log_data {
      if log["timestamp"] >= window_start and log["timestamp"] <= window_end {
        window_logs.push(log)
      }
    }
    
    if window_metrics.length() > 0 or window_logs.length() > 0 {
      window_correlations.push((window_start, window_metrics, window_logs))
    }
  }
  
  // 验证时间窗口关联
  assert_eq(window_correlations.length(), 3)
  
  // 分析第一个窗口（成功请求）
  let window_1 = window_correlations[0]
  assert_eq(window_1.0, 1641018000000L)
  assert_eq(window_1.1.length(), 2)  // 2个指标
  assert_eq(window_1.2.length(), 1)  // 1个日志
  
  // 验证指标和日志的一致性
  let window_1_metrics = window_1.1
  let window_1_logs = window_1.2
  
  // 检查HTTP请求指标和日志的一致性
  let http_request_metrics = []
  for metric in window_1_metrics {
    if metric["metric_name"] == "http.requests.total" {
      http_request_metrics.push(metric)
    }
  }
  
  assert_eq(http_request_metrics.length(), 1)
  assert_eq(http_request_metrics[0]["metric_value"], 100.0)
  assert_eq(http_request_metrics[0]["tags"]["status"], "200")
  
  // 检查对应的日志
  assert_eq(window_1_logs[0]["level"], "INFO")
  assert_eq(window_1_logs[0]["context"]["status"], "200")
  
  // 分析第三个窗口（错误和警告）
  let window_3 = window_correlations[2]
  assert_eq(window_3.0, 1641018006000L)
  assert_eq(window_3.1.length(), 1)  // 1个指标
  assert_eq(window_3.2.length(), 1)  // 1个日志
  
  // 验证CPU使用率指标和警告日志的关联
  let cpu_metric = window_3.1[0]
  let cpu_log = window_3.2[0]
  
  assert_eq(cpu_metric["metric_name"], "cpu.usage")
  assert_eq(cpu_metric["metric_value"], 85.2)
  assert_eq(cpu_log["level"], "WARN")
  assert_eq(cpu_log["message"], "High CPU usage detected")
  
  // 分析异常模式
  let mut anomaly_indicators = []
  
  for correlation in window_correlations {
    let window_start = correlation.0
    let metrics = correlation.1
    let logs = correlation.2
    
    // 检查错误日志
    let mut error_count = 0
    for log in logs {
      if log["level"] == "ERROR" {
        error_count = error_count + 1
      }
    }
    
    // 检查异常指标值
    let mut anomalous_metrics = 0
    for metric in metrics {
      if metric["metric_name"] == "cpu.usage" and metric["metric_value"] > 80.0 {
        anomalous_metrics = anomalous_metrics + 1
      }
      if metric["metric_name"] == "http.request.duration" and metric["metric_value"] > 1000.0 {
        anomalous_metrics = anomalous_metrics + 1
      }
    }
    
    if error_count > 0 or anomalous_metrics > 0 {
      anomaly_indicators.push((window_start, error_count, anomalous_metrics))
    }
  }
  
  // 验证异常指标
  assert_eq(anomaly_indicators.length(), 2)
  
  // 验证错误窗口
  let error_window = anomaly_indicators[0]
  assert_eq(error_window.0, 1641018003000L)  // 错误发生的时间窗口
  assert_eq(error_window.1, 1)  // 1个错误
  assert_eq(error_window.2, 1)  // 1个异常指标（高延迟）
  
  // 验证警告窗口
  let warning_window = anomaly_indicators[1]
  assert_eq(warning_window.0, 1641018006000L)  // 警告发生的时间窗口
  assert_eq(warning_window.1, 0)  // 0个错误
  assert_eq(warning_window.2, 1)  // 1个异常指标（高CPU使用率）
}