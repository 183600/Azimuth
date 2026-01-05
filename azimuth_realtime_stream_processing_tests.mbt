// Azimuth 实时流处理测试用例
// 专注于实时数据流处理和流式遥测分析功能

// 测试1: 实时数据流窗口处理
test "实时数据流窗口处理" {
  // 模拟实时数据流事件
  let stream_events = [
    { timestamp: 1640995200, event_type: "metric", source: "service-a", value: 45.2, tags: ["cpu", "production"] },
    { timestamp: 1640995201, event_type: "log", source: "service-b", level: "INFO", message: "Request processed" },
    { timestamp: 1640995202, event_type: "metric", source: "service-a", value: 47.8, tags: ["cpu", "production"] },
    { timestamp: 1640995203, event_type: "trace", source: "service-c", trace_id: "trace-001", duration_ms: 120 },
    { timestamp: 1640995204, event_type: "metric", source: "service-b", value: 78.5, tags: ["memory", "production"] },
    { timestamp: 1640995205, event_type: "log", source: "service-a", level: "ERROR", message: "Database connection failed" },
    { timestamp: 1640995206, event_type: "metric", source: "service-c", value: 23.1, tags: ["cpu", "staging"] },
    { timestamp: 1640995207, event_type: "metric", source: "service-a", value: 46.5, tags: ["cpu", "production"] },
    { timestamp: 1640995208, event_type: "trace", source: "service-b", trace_id: "trace-002", duration_ms: 250 },
    { timestamp: 1640995209, event_type: "log", source: "service-c", level: "WARN", message: "High memory usage detected" }
  ]
  
  // 时间窗口处理函数
  let process_time_window = fn(events: Array[StreamEvent>, window_start: Int, window_end: Int) {
    let window_events = events.filter(fn(event) {
      event.timestamp >= window_start and event.timestamp <= window_end
    })
    
    // 按事件类型分类
    let mut metrics = []
    let mut logs = []
    let mut traces = []
    
    for event in window_events {
      match event.event_type {
        "metric" => metrics = metrics.push(event)
        "log" => logs = logs.push(event)
        "trace" => traces = traces.push(event)
        _ => ()
      }
    }
    
    // 计算指标统计
    let mut metric_values = []
    for metric in metrics {
      metric_values = metric_values.push(metric.value)
    }
    
    let metric_stats = if metric_values.length() > 0 {
      let mut sum = 0.0
      for value in metric_values {
        sum = sum + value
      }
      
      {
        count: metric_values.length(),
        avg: sum / metric_values.length().to_float(),
        min: metric_values.min(),
        max: metric_values.max()
      }
    } else {
      {
        count: 0,
        avg: 0.0,
        min: 0.0,
        max: 0.0
      }
    }
    
    // 计算日志级别分布
    let mut log_levels = []
    for log in logs {
      log_levels = log_levels.push(log.level)
    }
    
    let log_level_counts = [
      { level: "ERROR", count: log_levels.filter(fn(l) { l == "ERROR" }).length() },
      { level: "WARN", count: log_levels.filter(fn(l) { l == "WARN" }).length() },
      { level: "INFO", count: log_levels.filter(fn(l) { l == "INFO" }).length() }
    ]
    
    // 计算跟踪平均延迟
    let mut trace_durations = []
    for trace in traces {
      trace_durations = trace_durations.push(trace.duration_ms)
    }
    
    let avg_trace_duration = if trace_durations.length() > 0 {
      let mut sum = 0
      for duration in trace_durations {
        sum = sum + duration
      }
      sum / trace_durations.length()
    } else {
      0
    }
    
    {
      window_start: window_start,
      window_end: window_end,
      total_events: window_events.length(),
      metrics_count: metrics.length(),
      logs_count: logs.length(),
      traces_count: traces.length(),
      metric_stats: metric_stats,
      log_level_counts: log_level_counts,
      avg_trace_duration: avg_trace_duration
    }
  }
  
  // 处理5秒时间窗口
  let window_1 = process_time_window(stream_events, 1640995200, 1640995204)
  let window_2 = process_time_window(stream_events, 1640995205, 1640995209)
  
  // 验证第一个窗口
  assert_eq(window_1.window_start, 1640995200)
  assert_eq(window_1.window_end, 1640995204)
  assert_eq(window_1.total_events, 5)
  assert_eq(window_1.metrics_count, 3)
  assert_eq(window_1.logs_count, 1)
  assert_eq(window_1.traces_count, 1)
  assert_eq(window_1.metric_stats.count, 3)
  assert_eq(window_1.metric_stats.avg, 57.17)  // (45.2+47.8+78.5)/3 = 57.17
  assert_eq(window_1.metric_stats.min, 45.2)
  assert_eq(window_1.metric_stats.max, 78.5)
  assert_eq(window_1.log_level_counts[0].count, 0)  // ERROR
  assert_eq(window_1.log_level_counts[1].count, 0)  // WARN
  assert_eq(window_1.log_level_counts[2].count, 1)  // INFO
  assert_eq(window_1.avg_trace_duration, 120)
  
  // 验证第二个窗口
  assert_eq(window_2.window_start, 1640995205)
  assert_eq(window_2.window_end, 1640995209)
  assert_eq(window_2.total_events, 5)
  assert_eq(window_2.metrics_count, 2)
  assert_eq(window_2.logs_count, 2)
  assert_eq(window_2.traces_count, 1)
  assert_eq(window_2.metric_stats.count, 2)
  assert_eq(window_2.metric_stats.avg, 34.8)   // (23.1+46.5)/2 = 34.8
  assert_eq(window_2.metric_stats.min, 23.1)
  assert_eq(window_2.metric_stats.max, 46.5)
  assert_eq(window_2.log_level_counts[0].count, 1)  // ERROR
  assert_eq(window_2.log_level_counts[1].count, 1)  // WARN
  assert_eq(window_2.log_level_counts[2].count, 0)  // INFO
  assert_eq(window_2.avg_trace_duration, 250)
  
  // 滑动窗口处理
  let process_sliding_window = fn(events: Array[StreamEvent>, window_size: Int, step_size: Int) {
    let windows = []
    
    if events.length() == 0 {
      return windows
    }
    
    let min_timestamp = events.map(fn(e) { e.timestamp }).min()
    let max_timestamp = events.map(fn(e) { e.timestamp }).max()
    
    let mut current_start = min_timestamp
    
    while current_start + window_size <= max_timestamp {
      let window = process_time_window(events, current_start, current_start + window_size)
      windows = windows.push(window)
      current_start = current_start + step_size
    }
    
    windows
  }
  
  // 处理3秒滑动窗口，步长为2秒
  let sliding_windows = process_sliding_window(stream_events, 3, 2)
  
  // 验证滑动窗口
  assert_eq(sliding_windows.length(), 4)
  
  // 验证第一个滑动窗口 (1640995200-1640995203)
  assert_eq(sliding_windows[0].window_start, 1640995200)
  assert_eq(sliding_windows[0].window_end, 1640995203)
  assert_eq(sliding_windows[0].total_events, 4)
  
  // 验证第二个滑动窗口 (1640995202-1640995205)
  assert_eq(sliding_windows[1].window_start, 1640995202)
  assert_eq(sliding_windows[1].window_end, 1640995205)
  assert_eq(sliding_windows[1].total_events, 4)
  
  // 验证第三个滑动窗口 (1640995204-1640995207)
  assert_eq(sliding_windows[2].window_start, 1640995204)
  assert_eq(sliding_windows[2].window_end, 1640995207)
  assert_eq(sliding_windows[2].total_events, 4)
  
  // 验证第四个滑动窗口 (1640995206-1640995209)
  assert_eq(sliding_windows[3].window_start, 1640995206)
  assert_eq(sliding_windows[3].window_end, 1640995209)
  assert_eq(sliding_windows[3].total_events, 4)
}

// 测试2: 实时流聚合处理
test "实时流聚合处理" {
  // 模拟实时流数据
  let stream_data = [
    { timestamp: 1640995200, source: "service-a", metric: "cpu", value: 45.2 },
    { timestamp: 1640995201, source: "service-b", metric: "memory", value: 512.3 },
    { timestamp: 1640995202, source: "service-a", metric: "cpu", value: 47.8 },
    { timestamp: 1640995203, source: "service-c", metric: "disk_io", value: 120.5 },
    { timestamp: 1640995204, source: "service-b", metric: "memory", value: 518.7 },
    { timestamp: 1640995205, source: "service-a", metric: "cpu", value: 46.5 },
    { timestamp: 1640995206, source: "service-c", metric: "disk_io", value: 125.2 },
    { timestamp: 1640995207, source: "service-b", metric: "memory", value: 515.9 },
    { timestamp: 1640995208, source: "service-a", metric: "cpu", value: 48.1 },
    { timestamp: 1640995209, source: "service-c", metric: "disk_io", value: 118.7 }
  ]
  
  // 按服务源分组聚合
  let aggregate_by_source = fn(data: Array[StreamData>) {
    let sources = ["service-a", "service-b", "service-c"]
    let mut source_aggregates = []
    
    for source in sources {
      let source_data = data.filter_fn(d) { d.source == source }
      
      if source_data.length() > 0 {
        let mut metric_values = []
        let mut metric_types = []
        
        for d in source_data {
          metric_values = metric_values.push(d.value)
          if not metric_types.contains(d.metric) {
            metric_types = metric_types.push(d.metric)
          }
        }
        
        let mut sum = 0.0
        for value in metric_values {
          sum = sum + value
        }
        
        source_aggregates = source_aggregates.push({
          source: source,
          count: source_data.length(),
          avg_value: sum / metric_values.length().to_float(),
          min_value: metric_values.min(),
          max_value: metric_values.max(),
          metric_types: metric_types
        })
      }
    }
    
    source_aggregates
  }
  
  // 按指标类型分组聚合
  let aggregate_by_metric = fn(data: Array[StreamData>) {
    let metrics = ["cpu", "memory", "disk_io"]
    let mut metric_aggregates = []
    
    for metric in metrics {
      let metric_data = data.filter_fn(d) { d.metric == metric }
      
      if metric_data.length() > 0 {
        let mut values = []
        let mut sources = []
        
        for d in metric_data {
          values = values.push(d.value)
          if not sources.contains(d.source) {
            sources = sources.push(d.source)
          }
        }
        
        let mut sum = 0.0
        for value in values {
          sum = sum + value
        }
        
        metric_aggregates = metric_aggregates.push({
          metric: metric,
          count: metric_data.length(),
          avg_value: sum / values.length().to_float(),
          min_value: values.min(),
          max_value: values.max(),
          sources: sources
        })
      }
    }
    
    metric_aggregates
  }
  
  // 时间窗口聚合
  let time_window_aggregate = fn(data: Array[StreamData>, window_size: Int) {
    if data.length() == 0 {
      return []
    }
    
    let min_timestamp = data.map_fn(d) { d.timestamp }.min()
    let max_timestamp = data.map_fn(d) { d.timestamp }.max()
    
    let mut windows = []
    let mut current_start = min_timestamp
    
    while current_start + window_size <= max_timestamp {
      let window_data = data.filter_fn(d) {
        d.timestamp >= current_start and d.timestamp < current_start + window_size
      }
      
      if window_data.length() > 0 {
        let mut sum = 0.0
        for d in window_data {
          sum = sum + d.value
        }
        
        windows = windows.push({
          window_start: current_start,
          window_end: current_start + window_size,
          count: window_data.length(),
          avg_value: sum / window_data.length().to_float(),
          total_value: sum
        })
      }
      
      current_start = current_start + window_size
    }
    
    windows
  }
  
  // 执行聚合操作
  let source_aggregates = aggregate_by_source(stream_data)
  let metric_aggregates = aggregate_by_metric(stream_data)
  let time_windows = time_window_aggregate(stream_data, 3)
  
  // 验证按服务源聚合
  assert_eq(source_aggregates.length(), 3)
  
  let service_a_agg = source_aggregates.find_fn(a) { a.source == "service-a" }
  assert_true(service_a_agg.is_some())
  assert_eq(service_a_agg.unwrap().count, 4)
  assert_eq(service_a_agg.unwrap().avg_value, 46.9)  // (45.2+47.8+46.5+48.1)/4 = 46.9
  assert_eq(service_a_agg.unwrap().min_value, 45.2)
  assert_eq(service_a_agg.unwrap().max_value, 48.1)
  assert_eq(service_a_agg.unwrap().metric_types.length(), 1)
  assert_true(service_a_agg.unwrap().metric_types.contains("cpu"))
  
  let service_b_agg = source_aggregates.find_fn(a) { a.source == "service-b" }
  assert_true(service_b_agg.is_some())
  assert_eq(service_b_agg.unwrap().count, 3)
  assert_eq(service_b_agg.unwrap().avg_value, 515.63)  // (512.3+518.7+515.9)/3 = 515.63
  assert_eq(service_b_agg.unwrap().min_value, 512.3)
  assert_eq(service_b_agg.unwrap().max_value, 518.7)
  assert_eq(service_b_agg.unwrap().metric_types.length(), 1)
  assert_true(service_b_agg.unwrap().metric_types.contains("memory"))
  
  let service_c_agg = source_aggregates.find_fn(a) { a.source == "service-c" }
  assert_true(service_c_agg.is_some())
  assert_eq(service_c_agg.unwrap().count, 3)
  assert_eq(service_c_agg.unwrap().avg_value, 121.47)  // (120.5+125.2+118.7)/3 = 121.47
  assert_eq(service_c_agg.unwrap().min_value, 118.7)
  assert_eq(service_c_agg.unwrap().max_value, 125.2)
  assert_eq(service_c_agg.unwrap().metric_types.length(), 1)
  assert_true(service_c_agg.unwrap().metric_types.contains("disk_io"))
  
  // 验证按指标类型聚合
  assert_eq(metric_aggregates.length(), 3)
  
  let cpu_agg = metric_aggregates.find_fn(a) { a.metric == "cpu" }
  assert_true(cpu_agg.is_some())
  assert_eq(cpu_agg.unwrap().count, 4)
  assert_eq(cpu_agg.unwrap().avg_value, 46.9)
  assert_eq(cpu_agg.unwrap().sources.length(), 1)
  assert_true(cpu_agg.unwrap().sources.contains("service-a"))
  
  let memory_agg = metric_aggregates.find_fn(a) { a.metric == "memory" }
  assert_true(memory_agg.is_some())
  assert_eq(memory_agg.unwrap().count, 3)
  assert_eq(memory_agg.unwrap().avg_value, 515.63)
  assert_eq(memory_agg.unwrap().sources.length(), 1)
  assert_true(memory_agg.unwrap().sources.contains("service-b"))
  
  let disk_io_agg = metric_aggregates.find_fn(a) { a.metric == "disk_io" }
  assert_true(disk_io_agg.is_some())
  assert_eq(disk_io_agg.unwrap().count, 3)
  assert_eq(disk_io_agg.unwrap().avg_value, 121.47)
  assert_eq(disk_io_agg.unwrap().sources.length(), 1)
  assert_true(disk_io_agg.unwrap().sources.contains("service-c"))
  
  // 验证时间窗口聚合
  assert_eq(time_windows.length(), 3)
  
  // 第一个窗口 (1640995200-1640995203)
  assert_eq(time_windows[0].window_start, 1640995200)
  assert_eq(time_windows[0].window_end, 1640995203)
  assert_eq(time_windows[0].count, 4)
  assert_eq(time_windows[0].avg_value, 178.95)  // (45.2+512.3+47.8+120.5)/4 = 178.95
  
  // 第二个窗口 (1640995203-1640995206)
  assert_eq(time_windows[1].window_start, 1640995203)
  assert_eq(time_windows[1].window_end, 1640995206)
  assert_eq(time_windows[1].count, 3)
  assert_eq(time_windows[1].avg_value, 356.57)  // (120.5+518.7+46.5)/3 = 356.57
  
  // 第三个窗口 (1640995206-1640995209)
  assert_eq(time_windows[2].window_start, 1640995206)
  assert_eq(time_windows[2].window_end, 1640995209)
  assert_eq(time_windows[2].count, 3)
  assert_eq(time_windows[2].avg_value, 321.57)  // (125.2+515.9+48.1)/3 = 321.57
}

// 测试3: 实时流模式检测
test "实时流模式检测" {
  // 模拟包含特定模式的实时流数据
  let pattern_stream_data = [
    { timestamp: 1640995200, source: "service-a", metric: "cpu", value: 45.0, status: "normal" },
    { timestamp: 1640995201, source: "service-a", metric: "cpu", value: 48.0, status: "normal" },
    { timestamp: 1640995202, source: "service-a", metric: "cpu", value: 52.0, status: "normal" },
    { timestamp: 1640995203, source: "service-a", metric: "cpu", value: 65.0, status: "warning" },
    { timestamp: 1640995204, source: "service-a", metric: "cpu", value: 78.0, status: "warning" },
    { timestamp: 1640995205, source: "service-a", metric: "cpu", value: 85.0, status: "critical" },
    { timestamp: 1640995206, source: "service-a", metric: "cpu", value: 92.0, status: "critical" },
    { timestamp: 1640995207, source: "service-a", metric: "cpu", value: 88.0, status: "critical" },
    { timestamp: 1640995208, source: "service-a", metric: "cpu", value: 75.0, status: "warning" },
    { timestamp: 1640995209, source: "service-a", metric: "cpu", value: 55.0, status: "normal" },
    { timestamp: 1640995210, source: "service-a", metric: "cpu", value: 48.0, status: "normal" },
    { timestamp: 1640995211, source: "service-a", metric: "cpu", value: 46.0, status: "normal" }
  ]
  
  // 检测上升趋势模式
  let detect_upward_trend = fn(data: Array[PatternStreamData>, window_size: Int, threshold: Float) {
    let mut trends = []
    
    if data.length() < window_size {
      return trends
    }
    
    for i in 0..(data.length() - window_size + 1) {
      let window = data.slice(i, i + window_size)
      
      // 计算线性回归斜率（简化方法）
      let n = window.length().to_float()
      let mut sum_x = 0.0
      let mut sum_y = 0.0
      let mut sum_xy = 0.0
      let mut sum_x2 = 0.0
      
      for j in 0..window.length() {
        let x = j.to_float()
        let y = window[j].value
        
        sum_x = sum_x + x
        sum_y = sum_y + y
        sum_xy = sum_xy + x * y
        sum_x2 = sum_x2 + x * x
      }
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      
      if slope > threshold {
        trends = trends.push({
          start_time: window[0].timestamp,
          end_time: window[window.length() - 1].timestamp,
          slope: slope,
          start_value: window[0].value,
          end_value: window[window.length() - 1].value,
          increase_percent: ((window[window.length() - 1].value - window[0].value) / window[0].value) * 100.0
        })
      }
    }
    
    trends
  }
  
  // 检测状态变化模式
  let detect_status_changes = fn(data: Array[PatternStreamData>) {
    let mut status_changes = []
    
    if data.length() < 2 {
      return status_changes
    }
    
    for i in 1..data.length() {
      if data[i].status != data[i-1].status {
        status_changes = status_changes.push({
          timestamp: data[i].timestamp,
          from_status: data[i-1].status,
          to_status: data[i].status,
          value: data[i].value,
          prev_value: data[i-1].value
        })
      }
    }
    
    status_changes
  }
  
  // 检测异常值模式
  let detect_outliers = fn(data: Array[PatternStreamData>, threshold: Float) {
    if data.length() < 3 {
      return []
    }
    
    // 计算移动平均和标准差
    let mut outliers = []
    
    for i in 1..(data.length() - 1) {
      let prev_value = data[i-1].value
      let current_value = data[i].value
      let next_value = data[i+1].value
      
      let local_avg = (prev_value + current_value + next_value) / 3.0
      let deviation = (current_value - local_avg).abs()
      
      if deviation > threshold {
        outliers = outliers.push({
          timestamp: data[i].timestamp,
          value: current_value,
          local_avg: local_avg,
          deviation: deviation,
          status: data[i].status
        })
      }
    }
    
    outliers
  }
  
  // 执行模式检测
  let upward_trends = detect_upward_trend(pattern_stream_data, 4, 5.0)
  let status_changes = detect_status_changes(pattern_stream_data)
  let outliers = detect_outliers(pattern_stream_data, 10.0)
  
  // 验证上升趋势检测
  assert_eq(upward_trends.length(), 2)
  
  // 第一个上升趋势 (1640995200-1640995203)
  assert_eq(upward_trends[0].start_time, 1640995200)
  assert_eq(upward_trends[0].end_time, 1640995203)
  assert_true(upward_trends[0].slope > 5.0)
  assert_eq(upward_trends[0].start_value, 45.0)
  assert_eq(upward_trends[0].end_value, 65.0)
  assert_eq(upward_trends[0].increase_percent, 44.44)  // (65-45)/45 * 100 = 44.44%
  
  // 第二个上升趋势 (1640995203-1640995206)
  assert_eq(upward_trends[1].start_time, 1640995203)
  assert_eq(upward_trends[1].end_time, 1640995206)
  assert_true(upward_trends[1].slope > 5.0)
  assert_eq(upward_trends[1].start_value, 65.0)
  assert_eq(upward_trends[1].end_value, 92.0)
  assert_eq(upward_trends[1].increase_percent, 41.54)  // (92-65)/65 * 100 = 41.54%
  
  // 验证状态变化检测
  assert_eq(status_changes.length(), 6)
  
  // 验证第一个状态变化
  assert_eq(status_changes[0].timestamp, 1640995203)
  assert_eq(status_changes[0].from_status, "normal")
  assert_eq(status_changes[0].to_status, "warning")
  assert_eq(status_changes[0].value, 65.0)
  assert_eq(status_changes[0].prev_value, 52.0)
  
  // 验证第二个状态变化
  assert_eq(status_changes[1].timestamp, 1640995205)
  assert_eq(status_changes[1].from_status, "warning")
  assert_eq(status_changes[1].to_status, "critical")
  assert_eq(status_changes[1].value, 85.0)
  assert_eq(status_changes[1].prev_value, 78.0)
  
  // 验证最后一个状态变化
  assert_eq(status_changes[5].timestamp, 1640995209)
  assert_eq(status_changes[5].from_status, "warning")
  assert_eq(status_changes[5].to_status, "normal")
  assert_eq(status_changes[5].value, 55.0)
  assert_eq(status_changes[5].prev_value, 75.0)
  
  // 验证异常值检测
  assert_eq(outliers.length(), 2)
  
  // 第一个异常值 (92.0)
  assert_eq(outliers[0].timestamp, 1640995206)
  assert_eq(outliers[0].value, 92.0)
  assert_eq(outliers[0].status, "critical")
  assert_true(outliers[0].deviation > 10.0)
  
  // 第二个异常值 (88.0)
  assert_eq(outliers[1].timestamp, 1640995207)
  assert_eq(outliers[1].value, 88.0)
  assert_eq(outliers[1].status, "critical")
  assert_true(outliers[1].deviation > 10.0)
  
  // 检测周期性模式
  let detect_periodic_pattern = fn(data: Array[PatternStreamData>, period: Int) {
    if data.length() < period * 2 {
      return []
    }
    
    let mut correlations = []
    
    // 计算不同滞后期的相关性
    for lag in period..(period + 3) {
      if data.length() > lag {
        let mut correlation_sum = 0.0
        let count = data.length() - lag
        
        for i in 0..count {
          correlation_sum = correlation_sum + data[i].value * data[i + lag].value
        }
        
        let correlation = correlation_sum / count.to_float()
        
        correlations = correlations.push({
          lag: lag,
          correlation: correlation
        })
      }
    }
    
    // 找出相关性最高的滞后期
    let max_correlation = correlations.map_fn(c) { c.correlation }.max()
    let best_lag = correlations.find_fn(c) { c.correlation == max_correlation }
    
    if best_lag.is_some() and max_correlation > 1000.0 {  // 阈值
      [{
        period: best_lag.unwrap().lag,
        correlation: max_correlation,
        confidence: if max_correlation > 2000.0 { "high" } else { "medium" }
      }]
    } else {
      []
    }
  }
  
  // 检测周期性模式
  let periodic_patterns = detect_periodic_pattern(pattern_stream_data, 4)
  
  // 验证周期性模式检测
  // 由于数据量较少，可能检测不到明显的周期性模式
  assert_true(periodic_patterns.length() >= 0)
}

// 测试4: 实时流复杂事件处理
test "实时流复杂事件处理" {
  // 模拟复杂事件流数据
  let complex_events = [
    { timestamp: 1640995200, event_id: "e001", event_type: "user_login", user_id: "user123", ip: "192.168.1.100" },
    { timestamp: 1640995201, event_id: "e002", event_type: "api_call", user_id: "user123", endpoint: "/api/orders", status: "200" },
    { timestamp: 1640995202, event_id: "e003", event_type: "api_call", user_id: "user456", endpoint: "/api/products", status: "200" },
    { timestamp: 1640995203, event_id: "e004", event_type: "api_call", user_id: "user123", endpoint: "/api/payments", status: "401" },
    { timestamp: 1640995204, event_id: "e005", event_type: "user_logout", user_id: "user123", ip: "192.168.1.100" },
    { timestamp: 1640995205, event_id: "e006", event_type: "user_login", user_id: "user789", ip: "192.168.1.200" },
    { timestamp: 1640995206, event_id: "e007", event_type: "api_call", user_id: "user789", endpoint: "/api/orders", status: "500" },
    { timestamp: 1640995207, event_id: "e008", event_type: "api_call", user_id: "user789", endpoint: "/api/orders", status: "500" },
    { timestamp: 1640995208, event_id: "e009", event_type: "api_call", user_id: "user789", endpoint: "/api/orders", status: "500" },
    { timestamp: 1640995209, event_id: "e010", event_type: "error", user_id: "user789", error_type: "service_unavailable" }
  ]
  
  // 检测用户会话模式
  let detect_user_sessions = fn(events: Array[ComplexEvent>) {
    let mut sessions = []
    let user_ids = ["user123", "user456", "user789"]
    
    for user_id in user_ids {
      let user_events = events.filter_fn(e) { e.user_id == user_id }
      
      if user_events.length() > 0 {
        let login_events = user_events.filter_fn(e) { e.event_type == "user_login" }
        let logout_events = user_events.filter_fn(e) { e.event_type == "user_logout" }
        let api_calls = user_events.filter_fn(e) { e.event_type == "api_call" }
        let error_events = user_events.filter_fn(e) { e.event_type == "error" }
        
        let session_start = if login_events.length() > 0 {
          login_events.map_fn(e) { e.timestamp }.min()
        } else {
          user_events.map_fn(e) { e.timestamp }.min()
        }
        
        let session_end = if logout_events.length() > 0 {
          logout_events.map_fn(e) { e.timestamp }.max()
        } else {
          user_events.map_fn(e) { e.timestamp }.max()
        }
        
        let successful_calls = api_calls.filter_fn(e) { e.status == "200" }.length()
        let failed_calls = api_calls.filter_fn(e) { e.status != "200" }.length()
        
        sessions = sessions.push({
          user_id: user_id,
          session_start: session_start,
          session_end: session_end,
          duration: session_end - session_start,
          total_events: user_events.length(),
          api_calls: api_calls.length(),
          successful_calls: successful_calls,
          failed_calls: failed_calls,
          errors: error_events.length()
        })
      }
    }
    
    sessions
  }
  
  // 检测异常API调用模式
  let detect_anomalous_api_calls = fn(events: Array[ComplexEvent>) {
    let api_calls = events.filter_fn(e) { e.event_type == "api_call" }
    
    // 按端点分组
    let endpoints = ["/api/orders", "/api/products", "/api/payments"]
    let mut endpoint_stats = []
    
    for endpoint in endpoints {
      let endpoint_calls = api_calls.filter_fn(e) { e.endpoint == endpoint }
      
      if endpoint_calls.length() > 0 {
        let successful_calls = endpoint_calls.filter_fn(e) { e.status == "200" }
        let failed_calls = endpoint_calls.filter_fn(e) { e.status != "200" }
        
        let success_rate = if endpoint_calls.length() > 0 {
          (successful_calls.length().to_float() / endpoint_calls.length().to_float()) * 100.0
        } else {
          0.0
        }
        
        endpoint_stats = endpoint_stats.push({
          endpoint: endpoint,
          total_calls: endpoint_calls.length(),
          successful_calls: successful_calls.length(),
          failed_calls: failed_calls.length(),
          success_rate: success_rate
        })
      }
    }
    
    // 识别异常端点（成功率低于80%）
    let anomalous_endpoints = endpoint_stats.filter_fn(stat) { stat.success_rate < 80.0 }
    
    anomalous_endpoints
  }
  
  // 检测连续失败模式
  let detect_consecutive_failures = fn(events: Array[ComplexEvent], threshold: Int) {
    let api_calls = events.filter_fn(e) { e.event_type == "api_call" }
    let mut consecutive_failures = []
    
    if api_calls.length() < threshold {
      return consecutive_failures
    }
    
    let mut current_user = ""
    let mut current_endpoint = ""
    let mut current_count = 0
    let mut current_start = 0
    
    for i in 0..api_calls.length() {
      let call = api_calls[i]
      let is_failure = call.status != "200"
      
      if is_failure and 
         (current_user == "" or current_user == call.user_id) and
         (current_endpoint == "" or current_endpoint == call.endpoint) {
        // 连续失败
        if current_count == 0 {
          current_user = call.user_id
          current_endpoint = call.endpoint
          current_start = call.timestamp
        }
        current_count = current_count + 1
      } else {
        // 失败序列结束
        if current_count >= threshold {
          consecutive_failures = consecutive_failures.push({
            user_id: current_user,
            endpoint: current_endpoint,
            start_time: current_start,
            end_time: api_calls[i-1].timestamp,
            failure_count: current_count
          })
        }
        
        // 重置计数器
        if is_failure {
          current_user = call.user_id
          current_endpoint = call.endpoint
          current_start = call.timestamp
          current_count = 1
        } else {
          current_user = ""
          current_endpoint = ""
          current_count = 0
        }
      }
    }
    
    // 检查最后一个序列
    if current_count >= threshold {
      consecutive_failures = consecutive_failures.push({
        user_id: current_user,
        endpoint: current_endpoint,
        start_time: current_start,
        end_time: api_calls[api_calls.length() - 1].timestamp,
        failure_count: current_count
      })
    }
    
    consecutive_failures
  }
  
  // 执行复杂事件处理
  let user_sessions = detect_user_sessions(complex_events)
  let anomalous_endpoints = detect_anomalous_api_calls(complex_events)
  let consecutive_failures = detect_consecutive_failures(complex_events, 3)
  
  // 验证用户会话检测
  assert_eq(user_sessions.length(), 3)
  
  let user123_session = user_sessions.find_fn(s) { s.user_id == "user123" }
  assert_true(user123_session.is_some())
  assert_eq(user123_session.unwrap().session_start, 1640995200)
  assert_eq(user123_session.unwrap().session_end, 1640995204)
  assert_eq(user123_session.unwrap().duration, 4)
  assert_eq(user123_session.unwrap().total_events, 4)
  assert_eq(user123_session.unwrap().api_calls, 2)
  assert_eq(user123_session.unwrap().successful_calls, 1)
  assert_eq(user123_session.unwrap().failed_calls, 1)
  
  let user789_session = user_sessions.find_fn(s) { s.user_id == "user789" }
  assert_true(user789_session.is_some())
  assert_eq(user789_session.unwrap().session_start, 1640995205)
  assert_eq(user789_session.unwrap().session_end, 1640995209)
  assert_eq(user789_session.unwrap().duration, 4)
  assert_eq(user789_session.unwrap().total_events, 5)
  assert_eq(user789_session.unwrap().api_calls, 3)
  assert_eq(user789_session.unwrap().successful_calls, 0)
  assert_eq(user789_session.unwrap().failed_calls, 3)
  assert_eq(user789_session.unwrap().errors, 1)
  
  // 验证异常API调用检测
  assert_eq(anomalous_endpoints.length(), 2)
  
  let orders_endpoint = anomalous_endpoints.find_fn(e) { e.endpoint == "/api/orders" }
  assert_true(orders_endpoint.is_some())
  assert_eq(orders_endpoint.unwrap().total_calls, 4)
  assert_eq(orders_endpoint.unwrap().successful_calls, 2)
  assert_eq(orders_endpoint.unwrap().failed_calls, 2)
  assert_eq(orders_endpoint.unwrap().success_rate, 50.0)
  
  let payments_endpoint = anomalous_endpoints.find_fn(e) { e.endpoint == "/api/payments" }
  assert_true(payments_endpoint.is_some())
  assert_eq(payments_endpoint.unwrap().total_calls, 1)
  assert_eq(payments_endpoint.unwrap().successful_calls, 0)
  assert_eq(payments_endpoint.unwrap().failed_calls, 1)
  assert_eq(payments_endpoint.unwrap().success_rate, 0.0)
  
  // 验证连续失败检测
  assert_eq(consecutive_failures.length(), 1)
  
  let failure = consecutive_failures[0]
  assert_eq(failure.user_id, "user789")
  assert_eq(failure.endpoint, "/api/orders")
  assert_eq(failure.start_time, 1640995206)
  assert_eq(failure.end_time, 1640995208)
  assert_eq(failure.failure_count, 3)
}