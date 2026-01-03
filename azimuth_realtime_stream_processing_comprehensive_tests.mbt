// Azimuth 实时流处理测试用例
// 专注于测试实时遥测数据流的处理、分析和转发能力

// 测试1: 基本流数据处理
test "基本流数据处理" {
  // 模拟实时遥测数据流
  let telemetry_stream = [
    {timestamp: 1640995200000L, event_type: "metric", value: 100.0, tags: [("service", "auth")]},
    {timestamp: 1640995201000L, event_type: "trace", value: 0.0, tags: [("service", "api")]},
    {timestamp: 1640995202000L, event_type: "metric", value: 105.0, tags: [("service", "auth")]},
    {timestamp: 1640995203000L, event_type: "log", value: 0.0, tags: [("level", "info")]},
    {timestamp: 1640995204000L, event_type: "metric", value: 98.0, tags: [("service", "auth")]},
    {timestamp: 1640995205000L, event_type: "trace", value: 0.0, tags: [("service", "db")]}
  ]
  
  // 按事件类型分类
  let metrics_stream = telemetry_stream.filter(fn(event) { event.event_type == "metric" })
  let traces_stream = telemetry_stream.filter(fn(event) { event.event_type == "trace" })
  let logs_stream = telemetry_stream.filter(fn(event) { event.event_type == "log" })
  
  // 验证流分类
  assert_eq(metrics_stream.length(), 3)
  assert_eq(traces_stream.length(), 2)
  assert_eq(logs_stream.length(), 1)
  
  // 计算指标流的平均值
  let metric_values = metrics_stream.map(fn(event) { event.value })
  let metric_avg = metric_values.fold(0.0, fn(acc, value) { acc + value }) / metric_values.length().to_double()
  
  // 验证指标计算
  assert_eq(metric_avg, (100.0 + 105.0 + 98.0) / 3.0)
  assert_eq(metric_avg, 101.0)
  
  // 按服务标签过滤
  let auth_service_events = telemetry_stream.filter(fn(event) {
    event.tags.contains(fn(tag) { tag.0 == "service" && tag.1 == "auth" })
  })
  
  // 验证服务过滤
  assert_eq(auth_service_events.length(), 3)
  assert_true(auth_service_events.all(fn(event) { event.event_type == "metric" }))
}

// 测试2: 流式窗口聚合
test "流式窗口聚合" {
  // 模拟时间序列数据流
  let time_series_stream = [
    {timestamp: 1640995200000L, value: 10.0},
    {timestamp: 1640995201000L, value: 15.0},
    {timestamp: 1640995202000L, value: 12.0},
    {timestamp: 1640995203000L, value: 18.0},
    {timestamp: 1640995204000L, value: 20.0},
    {timestamp: 1640995205000L, value: 14.0},
    {timestamp: 1640995206000L, value: 16.0},
    {timestamp: 1640995207000L, value: 22.0}
  ]
  
  // 定义时间窗口大小（3秒）
  let window_size_ms = 3000L
  
  // 滑动窗口聚合
  let sliding_window_aggregation = fn(stream: Array[{timestamp: Int, value: Double}], window_start: Int) -> {
    window_start: Int,
    window_end: Int,
    count: Int,
    sum: Double,
    avg: Double,
    min: Double,
    max: Double
  } {
    let window_end = window_start + window_size_ms
    let window_data = stream.filter(fn(point) { 
      point.timestamp >= window_start && point.timestamp < window_end 
    })
    
    if window_data.length() > 0 {
      let values = window_data.map(fn(point) { point.value })
      let sum = values.fold(0.0, fn(acc, value) { acc + value })
      let count = values.length()
      
      {
        window_start: window_start,
        window_end: window_end,
        count: count,
        sum: sum,
        avg: sum / count.to_double(),
        min: values.fold(999999.0, fn(acc, value) { if value < acc { value } else { acc } }),
        max: values.fold(0.0, fn(acc, value) { if value > acc { value } else { acc } })
      }
    } else {
      {
        window_start: window_start,
        window_end: window_end,
        count: 0,
        sum: 0.0,
        avg: 0.0,
        min: 0.0,
        max: 0.0
      }
    }
  }
  
  // 计算第一个窗口（0-3秒）
  let window1 = sliding_window_aggregation(time_series_stream, 1640995200000L)
  
  // 验证第一个窗口结果
  assert_eq(window1.window_start, 1640995200000L)
  assert_eq(window1.window_end, 1640995203000L)
  assert_eq(window1.count, 3)  // 前3个数据点
  assert_eq(window1.sum, 37.0)  // 10 + 15 + 12
  assert_eq(window1.avg, 12.333333333333334)
  assert_eq(window1.min, 10.0)
  assert_eq(window1.max, 15.0)
  
  // 计算第二个窗口（1-4秒）
  let window2 = sliding_window_aggregation(time_series_stream, 1640995201000L)
  
  // 验证第二个窗口结果
  assert_eq(window2.count, 3)  // 第2-4个数据点
  assert_eq(window2.sum, 45.0)  // 15 + 12 + 18
  assert_eq(window2.avg, 15.0)
  
  // 计算所有滑动窗口
  let window_starts = [
    1640995200000L,
    1640995201000L,
    1640995202000L,
    1640995203000L,
    1640995204000L,
    1640995205000L
  ]
  
  let all_windows = window_starts.map(sliding_window_aggregation)
  
  // 验证窗口数量
  assert_eq(all_windows.length(), 6)
  
  // 验证每个窗口都有数据
  assert_true(all_windows.all(fn(window) { window.count > 0 }))
}

// 测试3: 流式异常检测
test "流式异常检测" {
  // 模拟包含异常的指标流
  let metrics_with_anomalies = [
    {timestamp: 1640995200000L, metric_name: "cpu_usage", value: 45.0},
    {timestamp: 1640995201000L, metric_name: "cpu_usage", value: 48.0},
    {timestamp: 1640995202000L, metric_name: "cpu_usage", value: 52.0},
    {timestamp: 1640995203000L, metric_name: "cpu_usage", value: 95.0},  // 异常值
    {timestamp: 1640995204000L, metric_name: "cpu_usage", value: 46.0},
    {timestamp: 1640995205000L, metric_name: "cpu_usage", value: 49.0},
    {timestamp: 1640995206000L, metric_name: "cpu_usage", value: 51.0},
    {timestamp: 1640995207000L, metric_name: "cpu_usage", value: 98.0}   // 异常值
  ]
  
  // 基于统计的异常检测
  let statistical_anomaly_detection = fn(stream: Array[{timestamp: Int, metric_name: String, value: Double}], threshold: Double) -> Array[Int] {
    if stream.length() < 3 { return [] }
    
    // 计算移动平均和标准差
    let moving_averages = stream.slice(0, stream.length() - 2).map_with_index(fn(i, point) {
      let window = stream.slice(i, i + 3)
      let values = window.map(fn(p) { p.value })
      let avg = values.fold(0.0, fn(acc, v) { acc + v }) / values.length().to_double()
      
      (i + 1, avg)  // 返回中间点的索引和移动平均值
    })
    
    // 检测异常（值与移动平均的差异超过阈值）
    moving_averages.filter(fn(pair) {
      let index = pair.0
      let avg = pair.1
      let value = stream[index].value
      (value - avg).abs() > threshold
    }).map(fn(pair) { pair.0 })
  }
  
  // 使用统计方法检测异常
  let anomaly_indices = statistical_anomaly_detection(metrics_with_anomalies, 30.0)
  
  // 验证异常检测
  assert_eq(anomaly_indices.length(), 2)
  assert_true(anomaly_indices.contains(3))  // 第4个数据点（95.0）
  assert_true(anomaly_indices.contains(7))  // 第8个数据点（98.0）
  
  // 基于阈值的简单异常检测
  let threshold_anomaly_detection = fn(stream: Array[{timestamp: Int, metric_name: String, value: Double}], min_threshold: Double, max_threshold: Double) -> Array[Int] {
    stream.map_with_index(fn(i, point) {
      if point.value < min_threshold || point.value > max_threshold {
        i
      } else {
        -1
      }
    }).filter(fn(index) { index >= 0 })
  }
  
  // 使用阈值方法检测异常
  let threshold_anomalies = threshold_anomaly_detection(metrics_with_anomalies, 20.0, 80.0)
  
  // 验证阈值异常检测
  assert_eq(threshold_anomalies.length(), 2)
  assert_true(threshold_anomalies.contains(3))  // 95.0 > 80.0
  assert_true(threshold_anomalies.contains(7))  // 98.0 > 80.0
  
  // 生成异常警报
  let anomaly_alerts = anomaly_indices.map(fn(index) {
    let point = metrics_with_anomalies[index]
    {
      timestamp: point.timestamp,
      metric_name: point.metric_name,
      value: point.value,
      alert_type: "statistical_anomaly",
      severity: if point.value > 90.0 { "critical" } else { "warning" }
    }
  })
  
  // 验证警报生成
  assert_eq(anomaly_alerts.length(), 2)
  assert_true(anomaly_alerts.all(fn(alert) { alert.metric_name == "cpu_usage" }))
  assert_true(anomaly_alerts.all(fn(alert) { alert.severity == "critical" }))
}

// 测试4: 流式数据转换和丰富
test "流式数据转换和丰富" {
  // 模拟原始遥测事件流
  let raw_events = [
    {
      timestamp: 1640995200000L,
      event_type: "http_request",
      method: "GET",
      path: "/api/users",
      status_code: 200,
      duration_ms: 150
    },
    {
      timestamp: 1640995201000L,
      event_type: "http_request",
      method: "POST",
      path: "/api/orders",
      status_code: 201,
      duration_ms: 300
    },
    {
      timestamp: 1640995202000L,
      event_type: "http_request",
      method: "GET",
      path: "/api/products",
      status_code: 500,
      duration_ms: 800
    }
  ]
  
  // 数据转换：添加计算字段
  let enrich_events = fn(events: Array[{
    timestamp: Int,
    event_type: String,
    method: String,
    path: String,
    status_code: Int,
    duration_ms: Int
  }]) -> Array[{
    timestamp: Int,
    event_type: String,
    method: String,
    path: String,
    status_code: Int,
    duration_ms: Int,
    status_class: String,
    endpoint_category: String,
    performance_tier: String
  }> {
    events.map(fn(event) {
      // 状态码分类
      let status_class = if event.status_code >= 200 && event.status_code < 300 {
        "success"
      } else if event.status_code >= 400 && event.status_code < 500 {
        "client_error"
      } else if event.status_code >= 500 {
        "server_error"
      } else {
        "unknown"
      }
      
      // 端点分类
      let endpoint_category = if event.path.contains("/users") {
        "user_management"
      } else if event.path.contains("/orders") {
        "order_management"
      } else if event.path.contains("/products") {
        "product_management"
      } else {
        "other"
      }
      
      // 性能分层
      let performance_tier = if event.duration_ms < 200 {
        "fast"
      } else if event.duration_ms < 500 {
        "normal"
      } else {
        "slow"
      }
      
      {
        timestamp: event.timestamp,
        event_type: event.event_type,
        method: event.method,
        path: event.path,
        status_code: event.status_code,
        duration_ms: event.duration_ms,
        status_class: status_class,
        endpoint_category: endpoint_category,
        performance_tier: performance_tier
      }
    })
  }
  
  // 执行数据丰富
  let enriched_events = enrich_events(raw_events)
  
  // 验证数据丰富结果
  assert_eq(enriched_events.length(), 3)
  
  // 验证第一个事件的丰富字段
  let first_event = enriched_events[0]
  assert_eq(first_event.status_class, "success")      // 200 -> success
  assert_eq(first_event.endpoint_category, "user_management")  // /api/users -> user_management
  assert_eq(first_event.performance_tier, "fast")    // 150ms -> fast
  
  // 验证第二个事件的丰富字段
  let second_event = enriched_events[1]
  assert_eq(second_event.status_class, "success")      // 201 -> success
  assert_eq(second_event.endpoint_category, "order_management")  // /api/orders -> order_management
  assert_eq(second_event.performance_tier, "normal")  // 300ms -> normal
  
  // 验证第三个事件的丰富字段
  let third_event = enriched_events[2]
  assert_eq(third_event.status_class, "server_error")  // 500 -> server_error
  assert_eq(third_event.endpoint_category, "product_management")  // /api/products -> product_management
  assert_eq(third_event.performance_tier, "slow")     // 800ms -> slow
  
  // 按性能分层统计
  let performance_stats = enriched_events.fold(
    {fast: 0, normal: 0, slow: 0}, 
    fn(acc, event) {
      match event.performance_tier {
        "fast" => {fast: acc.fast + 1, normal: acc.normal, slow: acc.slow}
        "normal" => {fast: acc.fast, normal: acc.normal + 1, slow: acc.slow}
        "slow" => {fast: acc.fast, normal: acc.normal, slow: acc.slow + 1}
        _ => acc
      }
    }
  )
  
  // 验证性能统计
  assert_eq(performance_stats.fast, 1)
  assert_eq(performance_stats.normal, 1)
  assert_eq(performance_stats.slow, 1)
}

// 测试5: 流式数据路由和分发
test "流式数据路由和分发" {
  // 模拟混合遥测数据流
  let mixed_stream = [
    {timestamp: 1640995200000L, event_type: "metric", source: "service-a", priority: "high"},
    {timestamp: 1640995201000L, event_type: "trace", source: "service-b", priority: "normal"},
    {timestamp: 1640995202000L, event_type: "log", source: "service-a", priority: "low"},
    {timestamp: 1640995203000L, event_type: "metric", source: "service-c", priority: "critical"},
    {timestamp: 1640995204000L, event_type: "trace", source: "service-a", priority: "high"},
    {timestamp: 1640995205000L, event_type: "metric", source: "service-b", priority: "normal"}
  ]
  
  // 定义路由规则
  let routing_rules = [
    {
      name: "metrics_store",
      condition: fn(event: {timestamp: Int, event_type: String, source: String, priority: String}) -> Bool {
        event.event_type == "metric"
      }
    },
    {
      name: "traces_store",
      condition: fn(event: {timestamp: Int, event_type: String, source: String, priority: String}) -> Bool {
        event.event_type == "trace"
      }
    },
    {
      name: "logs_store",
      condition: fn(event: {timestamp: Int, event_type: String, source: String, priority: String}) -> Bool {
        event.event_type == "log"
      }
    },
    {
      name: "high_priority_queue",
      condition: fn(event: {timestamp: Int, event_type: String, source: String, priority: String}) -> Bool {
        event.priority == "high" || event.priority == "critical"
      }
    }
  ]
  
  // 应用路由规则
  let routed_streams = routing_rules.map(fn(rule) {
    let matched_events = mixed_stream.filter(rule.condition)
    
    {
      destination: rule.name,
      events: matched_events,
      count: matched_events.length()
    }
  })
  
  // 验证路由结果
  assert_eq(routed_streams.length(), 4)
  
  // 验证metrics_store路由
  let metrics_route = routed_streams.find(fn(route) { route.destination == "metrics_store" })
  match metrics_route {
    Some(route) => {
      assert_eq(route.count, 3)  // 3个metric事件
      assert_true(route.events.all(fn(event) { event.event_type == "metric" }))
    }
    None => assert_true(false)
  }
  
  // 验证traces_store路由
  let traces_route = routed_streams.find(fn(route) { route.destination == "traces_store" })
  match traces_route {
    Some(route) => {
      assert_eq(route.count, 2)  // 2个trace事件
      assert_true(route.events.all(fn(event) { event.event_type == "trace" }))
    }
    None => assert_true(false)
  }
  
  // 验证high_priority_queue路由
  let high_priority_route = routed_streams.find(fn(route) { route.destination == "high_priority_queue" })
  match high_priority_route {
    Some(route) => {
      assert_eq(route.count, 2)  // 2个高优先级事件
      assert_true(route.events.all(fn(event) { 
        event.priority == "high" || event.priority == "critical" 
      }))
    }
    None => assert_true(false)
  }
  
  // 验证事件可以路由到多个目的地
  let high_priority_metric = mixed_stream.find(fn(event) { 
    event.event_type == "metric" && (event.priority == "high" || event.priority == "critical") 
  })
  
  match high_priority_metric {
    Some(event) => {
      // 这个事件应该同时出现在metrics_store和high_priority_queue中
      let metrics_contains = metrics_route.unwrap().events.contains(event)
      let high_priority_contains = high_priority_route.unwrap().events.contains(event)
      
      assert_true(metrics_contains)
      assert_true(high_priority_contains)
    }
    None => assert_true(false)
  }
}

// 测试6: 流式数据背压处理
test "流式数据背压处理" {
  // 模拟流处理系统状态
  let stream_processor_state = {
    input_queue_size: 1000,
    processing_capacity: 800,
    output_queue_size: 200,
    max_queue_size: 1500,
    backpressure_threshold: 0.8
  }
  
  // 计算系统负载
  let input_load = stream_processor_state.input_queue_size.to_double() / stream_processor_state.max_queue_size.to_double()
  let output_load = stream_processor_state.output_queue_size.to_double() / stream_processor_state.max_queue_size.to_double()
  let total_load = (stream_processor_state.input_queue_size + stream_processor_state.output_queue_size).to_double() / 
                   (stream_processor_state.max_queue_size * 2).to_double()
  
  // 验证负载计算
  assert_eq(input_load, 0.6666666666666666)  // 1000/1500
  assert_eq(output_load, 0.13333333333333333)  // 200/1500
  assert_eq(total_load, 0.4)  // (1000+200)/(1500*2)
  
  // 检测背压条件
  let backpressure_detected = total_load >= stream_processor_state.backpressure_threshold ||
                            input_load >= stream_processor_state.backpressure_threshold
  
  // 验证背压检测
  assert_false(backpressure_detected)  // 0.4 < 0.8，没有背压
  
  // 模拟高负载情况
  let high_load_state = {
    input_queue_size: 1400,
    processing_capacity: 800,
    output_queue_size: 600,
    max_queue_size: 1500,
    backpressure_threshold: 0.8
  }
  
  let high_input_load = high_load_state.input_queue_size.to_double() / high_load_state.max_queue_size.to_double()
  let high_total_load = (high_load_state.input_queue_size + high_load_state.output_queue_size).to_double() / 
                       (high_load_state.max_queue_size * 2).to_double()
  
  let high_load_backpressure = high_input_load >= high_load_state.backpressure_threshold ||
                              high_total_load >= high_load_state.backpressure_threshold
  
  // 验证高负载背压检测
  assert_true(high_load_backpressure)  // 0.933 > 0.8，有背压
  
  // 定义背压处理策略
  let backpressure_strategies = [
    {
      load_level: "moderate",
      actions: ["reduce_batch_size", "increase_processing_threads"]
    },
    {
      load_level: "high",
      actions: ["drop_non_critical_events", "increase_sampling_rate"]
    },
    {
      load_level: "critical",
      actions: ["reject_new_events", "alert_operator", "scale_up_resources"]
    }
  ]
  
  // 根据负载级别选择策略
  let select_backpressure_strategy = fn(load_ratio: Double) -> String {
    if load_ratio >= 0.9 {
      "critical"
    } else if load_ratio >= 0.7 {
      "high"
    } else if load_ratio >= 0.5 {
      "moderate"
    } else {
      "normal"
    }
  }
  
  // 测试策略选择
  assert_eq(select_backpressure_strategy(0.4), "normal")
  assert_eq(select_backpressure_strategy(0.6), "moderate")
  assert_eq(select_backpressure_strategy(0.8), "high")
  assert_eq(select_backpressure_strategy(0.95), "critical")
  
  // 为高负载情况选择策略
  let high_load_strategy = select_backpressure_strategy(high_input_load)
  assert_eq(high_load_strategy, "critical")
  
  // 获取对应的处理动作
  let selected_actions = backpressure_strategies.find_fn(fn(strategy) {
    strategy.load_level == high_load_strategy
  })
  
  match selected_actions {
    Some(strategy) => {
      assert_eq(strategy.actions.length(), 3)
      assert_true(strategy.actions.contains("reject_new_events"))
      assert_true(strategy.actions.contains("alert_operator"))
      assert_true(strategy.actions.contains("scale_up_resources"))
    }
    None => assert_true(false)
  }
  
  // 模拟背压恢复
  let apply_backpressure_recovery = fn(state: {
    input_queue_size: Int,
    processing_capacity: Int,
    output_queue_size: Int,
    max_queue_size: Int,
    backpressure_threshold: Double
  }) -> {
    input_queue_size: Int,
    processing_capacity: Int,
    output_queue_size: Int,
    max_queue_size: Int,
    backpressure_threshold: Double,
    recovery_actions: Array[String]
  } {
    let input_load = state.input_queue_size.to_double() / state.max_queue_size.to_double()
    let total_load = (state.input_queue_size + state.output_queue_size).to_double() / 
                    (state.max_queue_size * 2).to_double()
    
    if total_load >= state.backpressure_threshold {
      // 应用背压恢复措施
      let reduced_input = (state.input_queue_size * 7 / 10)  // 减少30%
      let increased_capacity = state.processing_capacity + 200  // 增加处理能力
      
      {
        input_queue_size: reduced_input,
        processing_capacity: increased_capacity,
        output_queue_size: state.output_queue_size,
        max_queue_size: state.max_queue_size,
        backpressure_threshold: state.backpressure_threshold,
        recovery_actions: ["reduce_input_rate", "increase_processing_capacity"]
      }
    } else {
      {
        input_queue_size: state.input_queue_size,
        processing_capacity: state.processing_capacity,
        output_queue_size: state.output_queue_size,
        max_queue_size: state.max_queue_size,
        backpressure_threshold: state.backpressure_threshold,
        recovery_actions: []
      }
    }
  }
  
  // 应用背压恢复
  let recovered_state = apply_backpressure_recovery(high_load_state)
  
  // 验证恢复结果
  assert_eq(recovered_state.input_queue_size, 980)  // 1400 * 0.7
  assert_eq(recovered_state.processing_capacity, 1000)  // 800 + 200
  assert_eq(recovered_state.recovery_actions.length(), 2)
  assert_true(recovered_state.recovery_actions.contains("reduce_input_rate"))
  assert_true(recovered_state.recovery_actions.contains("increase_processing_capacity"))
  
  // 验证恢复后的负载
  let recovered_load = (recovered_state.input_queue_size + recovered_state.output_queue_size).to_double() / 
                      (recovered_state.max_queue_size * 2).to_double()
  
  assert_true(recovered_load < high_total_load)  // 负载应该降低
}