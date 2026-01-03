// Azimuth 额外MoonBit测试用例
// 包含遥测系统的高级功能和MoonBit语言特性测试

// 测试1: 时间序列数据分析
test "时间序列数据分析功能" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建时间序列分析器
  let analyze_time_series = fn(points: Array[TimeSeriesPoint]) {
    if points.length() < 2 {
      return {
        trend: "insufficient_data",
        average_change_rate: 0.0,
        volatility: 0.0,
        outliers: []
      }
    }
    
    // 按时间戳排序
    let sorted_points = points.sort(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    // 计算变化率
    let change_rates = []
    for i in 1..sorted_points.length() {
      let prev = sorted_points[i - 1]
      let curr = sorted_points[i]
      let time_diff = (curr.timestamp - prev.timestamp) as Float
      let value_diff = curr.value - prev.value
      let rate = if time_diff > 0.0 { value_diff / time_diff } else { 0.0 }
      change_rates = change_rates.push(rate)
    }
    
    // 计算平均变化率
    let avg_change_rate = change_rates.reduce(fn(acc, rate) { acc + rate }, 0.0) / (change_rates.length() as Float)
    
    // 计算波动性（标准差）
    let variance = change_rates.reduce(fn(acc, rate) { 
      let diff = rate - avg_change_rate
      acc + diff * diff 
    }, 0.0) / (change_rates.length() as Float)
    let volatility = variance.sqrt()
    
    // 检测异常值（使用1.5*IQR规则）
    let sorted_rates = change_rates.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    let q1_index = (sorted_rates.length() / 4)
    let q3_index = (sorted_rates.length() * 3 / 4)
    let q1 = sorted_rates[q1_index]
    let q3 = sorted_rates[q3_index]
    let iqr = q3 - q1
    let lower_bound = q1 - 1.5 * iqr
    let upper_bound = q3 + 1.5 * iqr
    
    let outliers = []
    for i in 0..change_rates.length() {
      if change_rates[i] < lower_bound || change_rates[i] > upper_bound {
        outliers = outliers.push(sorted_points[i + 1])  // 对应原始数据点
      }
    }
    
    // 确定趋势
    let trend = if avg_change_rate > 0.1 {
      "increasing"
    } else if avg_change_rate < -0.1 {
      "decreasing"
    } else {
      "stable"
    }
    
    {
      trend,
      average_change_rate,
      volatility,
      outliers
    }
  }
  
  // 创建测试数据
  let test_data = [
    { timestamp: 1000, value: 10.0, tags: [("service", "api")] },
    { timestamp: 2000, value: 12.0, tags: [("service", "api")] },
    { timestamp: 3000, value: 11.5, tags: [("service", "api")] },
    { timestamp: 4000, value: 13.0, tags: [("service", "api")] },
    { timestamp: 5000, value: 25.0, tags: [("service", "api")] },  // 异常值
    { timestamp: 6000, value: 13.5, tags: [("service", "api")] },
    { timestamp: 7000, value: 14.0, tags: [("service", "api")] }
  ]
  
  // 分析时间序列
  let analysis = analyze_time_series(test_data)
  
  // 验证结果
  assert_eq(analysis.trend, "increasing")
  assert_true(analysis.average_change_rate > 0.0)
  assert_true(analysis.volatility > 0.0)
  assert_eq(analysis.outliers.length(), 1)  // 应该检测到25.0这个异常值
  assert_eq(analysis.outliers[0].value, 25.0)
}

// 测试2: 分布式追踪上下文传播
test "分布式追踪上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // 创建上下文注入器
  let inject_context = fn(context: TraceContext, headers: Array[(String, String)]) {
    let updated_headers = headers
      .push(("trace-id", context.trace_id))
      .push(("span-id", context.span_id))
    
    // 添加父span ID（如果存在）
    let with_parent = match context.parent_span_id {
      Some(parent_id) => updated_headers.push(("parent-span-id", parent_id))
      None => updated_headers
    }
    
    // 添加baggage项
    let with_baggage = context.baggage.reduce(with_parent, fn(acc, item) {
      let (key, value) = item
      acc.push(("baggage-" + key, value))
    })
    
    // 添加标志
    with_baggage.push(("trace-flags", context.flags.to_string()))
  }
  
  // 创建上下文提取器
  let extract_context = fn(headers: Array[(String, String)]) {
    let find_header = fn(key: String) {
      headers.find(fn(header) { header.0 == key }).map(fn(h) { h.1 })
    }
    
    let trace_id = find_header("trace-id").unwrap_or("unknown")
    let span_id = find_header("span-id").unwrap_or("unknown")
    let parent_span_id = find_header("parent-span-id")
    let flags = find_header("trace-flags").map(fn(s) { s.parse_int() }).unwrap_or(0)
    
    // 提取baggage项
    let baggage_headers = headers.filter(fn(header) { 
      header.0.starts_with("baggage-") 
    })
    let baggage = baggage_headers.map(fn(header) {
      let key = header.0.substring(8)  // 移除"baggage-"前缀
      (key, header.1)
    })
    
    {
      trace_id,
      span_id,
      parent_span_id,
      baggage,
      flags
    }
  }
  
  // 创建子上下文生成器
  let create_child_context = fn(parent: TraceContext, operation_name: String) {
    let new_span_id = "span-" + operation_name + "-" + Time::now().to_string()
    {
      trace_id: parent.trace_id,
      span_id: new_span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // 测试上下文注入和提取
  let original_context = {
    trace_id: "trace-12345",
    span_id: "span-abcde",
    parent_span_id: None,
    baggage: [("user-id", "user-67890"), ("request-id", "req-11111")],
    flags: 1
  }
  
  // 注入上下文到HTTP头
  let headers = [("content-type", "application/json")]
  let headers_with_context = inject_context(original_context, headers)
  
  // 验证注入的头
  assert_true(headers_with_context.any(fn(h) { h.0 == "trace-id" && h.1 == "trace-12345" }))
  assert_true(headers_with_context.any(fn(h) { h.0 == "span-id" && h.1 == "span-abcde" }))
  assert_true(headers_with_context.any(fn(h) { h.0 == "baggage-user-id" && h.1 == "user-67890" }))
  assert_true(headers_with_context.any(fn(h) { h.0 == "baggage-request-id" && h.1 == "req-11111" }))
  
  // 提取上下文
  let extracted_context = extract_context(headers_with_context)
  
  // 验证提取的上下文
  assert_eq(extracted_context.trace_id, original_context.trace_id)
  assert_eq(extracted_context.span_id, original_context.span_id)
  assert_eq(extracted_context.parent_span_id, original_context.parent_span_id)
  assert_eq(extracted_context.flags, original_context.flags)
  assert_eq(extracted_context.baggage.length(), 2)
  
  // 创建子上下文
  let child_context = create_child_context(original_context, "database-query")
  assert_eq(child_context.trace_id, original_context.trace_id)
  assert_eq(child_context.parent_span_id, Some(original_context.span_id))
  assert_eq(child_context.baggage, original_context.baggage)
}

// 测试3: 遥测数据压缩和优化
test "遥测数据压缩和优化" {
  // 定义遥测数据点
  type TelemetryPoint = {
    metric_name: String,
    value: Float,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // 创建数据压缩器（基于时间窗口的聚合）
  let compress_by_time_window = fn(points: Array[TelemetryPoint], window_size_ms: Int) {
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    // 按时间窗口分组
    let windows = []
    let current_window = []
    let mut window_start = sorted_points[0].timestamp
    
    for point in sorted_points {
      if point.timestamp < window_start + window_size_ms {
        current_window = current_window.push(point)
      } else {
        windows = windows.push(current_window)
        current_window = [point]
        window_start = point.timestamp
      }
    }
    windows = windows.push(current_window)  // 添加最后一个窗口
    
    // 聚合每个窗口
    windows.map(fn(window_points) {
      let metric_name = window_points[0].metric_name
      let timestamps = window_points.map(fn(p) { p.timestamp })
      let values = window_points.map(fn(p) { p.value })
      
      let window_start = timestamps.reduce(fn(acc, ts) { if ts < acc { ts } else { acc } }, timestamps[0])
      let window_end = timestamps.reduce(fn(acc, ts) { if ts > acc { ts } else { acc } }, timestamps[0])
      
      let avg_value = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      let min_value = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      let max_value = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      
      // 合并所有属性（去重）
      let all_attributes = []
      for point in window_points {
        for attr in point.attributes {
          if not all_attributes.any(fn(a) { a.0 == attr.0 && a.1 == attr.1 }) {
            all_attributes = all_attributes.push(attr)
          }
        }
      }
      
      {
        metric_name: metric_name + "_compressed",
        value: avg_value,
        timestamp: window_start + (window_end - window_start) / 2,  // 窗口中点
        attributes: all_attributes.push([
          ("compression", "time_window"),
          ("window_size_ms", window_size_ms.to_string()),
          ("point_count", window_points.length().to_string()),
          ("min_value", min_value.to_string()),
          ("max_value", max_value.to_string())
        ])
      }
    })
  }
  
  // 创建数据采样器（基于重要性）
  let importance_sample = fn(points: Array[TelemetryPoint], sample_rate: Float) {
    if points.length() == 0 {
      return []
    }
    
    // 计算每个点的重要性分数
    let scored_points = points.map(fn(point) {
      // 重要性基于：
      // 1. 值的变化程度（与平均值的偏差）
      // 2. 时间新鲜度（越新越重要）
      // 3. 属性的唯一性
      
      let values = points.map(fn(p) { p.value })
      let avg_value = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      let value_deviation = (point.value - avg_value).abs() / avg_value
      
      let timestamps = points.map(fn(p) { p.timestamp })
      let min_timestamp = timestamps.reduce(fn(acc, ts) { if ts < acc { ts } else { acc } }, timestamps[0])
      let max_timestamp = timestamps.reduce(fn(acc, ts) { if ts > acc { ts } else { acc } }, timestamps[0])
      let time_freshness = (point.timestamp - min_timestamp) as Float / (max_timestamp - min_timestamp) as Float
      
      let attribute_uniqueness = point.attributes.length() as Float
      
      let importance_score = value_deviation * 0.5 + time_freshness * 0.3 + attribute_uniqueness * 0.2
      
      (point, importance_score)
    })
    
    // 按重要性排序
    let sorted_by_importance = scored_points.sort(fn(a, b) { 
      if a.1 > b.1 { -1 } 
      else if a.1 < b.1 { 1 } 
      else { 0 } 
    })
    
    // 采样前N%的点
    let sample_count = ((points.length() as Float) * sample_rate) as Int
    let sampled_count = if sample_count < 1 { 1 } else if sample_count > points.length() { points.length() } else { sample_count }
    
    sorted_by_importance.take(sampled_count).map(fn(item) { item.0 })
  }
  
  // 创建测试数据
  let test_points = [
    { metric_name: "response_time", value: 100.0, timestamp: 1000, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 120.0, timestamp: 1500, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 95.0, timestamp: 2000, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 110.0, timestamp: 2500, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 200.0, timestamp: 3000, attributes: [("endpoint", "/api/users")] },  // 异常值
    { metric_name: "response_time", value: 105.0, timestamp: 3500, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 98.0, timestamp: 4000, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 115.0, timestamp: 4500, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 102.0, timestamp: 5000, attributes: [("endpoint", "/api/users")] },
    { metric_name: "response_time", value: 99.0, timestamp: 5500, attributes: [("endpoint", "/api/users")] }
  ]
  
  // 测试时间窗口压缩
  let compressed = compress_by_time_window(test_points, 2000)  // 2秒窗口
  assert_eq(compressed.length(), 3)  // 10个点分成3个窗口
  
  // 验证第一个窗口（1000-3000）
  let first_window = compressed[0]
  assert_eq(first_window.metric_name, "response_time_compressed")
  assert_true(first_window.value >= 95.0 && first_window.value <= 120.0)  // 平均值应该在这个范围内
  assert_eq(first_window.timestamp, 2000)  // 窗口中点
  
  // 验证压缩属性
  let compression_attr = first_window.attributes.find(fn(attr) { attr.0 == "compression" })
  assert_eq(compression_attr.unwrap().1, "time_window")
  
  let point_count_attr = first_window.attributes.find(fn(attr) { attr.0 == "point_count" })
  assert_eq(point_count_attr.unwrap().1, "4")  // 第一个窗口有4个点
  
  // 测试重要性采样
  let sampled = importance_sample(test_points, 0.3)  // 采样30%
  assert_eq(sampled.length(), 3)  // 10个点的30%是3个点
  
  // 验证异常值（200.0）应该被采样，因为它的重要性高
  let has_outlier = sampled.any(fn(p) { p.value == 200.0 })
  assert_true(has_outlier)
}

// 测试4: 自定义度量集合和聚合
test "自定义度量集合和聚合" {
  // 定义度量类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义度量数据
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 定义聚合结果
  type AggregatedMetric = {
    name: String,
    metric_type: MetricType,
    values: Array[Float],
    count: Int,
    sum: Float,
    avg: Float,
    min: Float,
    max: Float,
    labels: Array[(String, String)]
  }
  
  // 创建度量聚合器
  let aggregate_metrics = fn(metrics: Array[Metric], group_by_labels: Array[String]) {
    let groups = Map::empty()
    
    for metric in metrics {
      // 构建分组键
      let group_key_parts = group_by_labels.map(fn(label_key) {
        match metric.labels.find(fn(label) { label.0 == label_key }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = group_key_parts.join("|")
      
      // 获取或创建分组
      let group_metrics = match Map::get(groups, group_key) {
        Some(ms) => ms
        None => []
      }
      
      // 添加度量到分组
      let updated_group = group_metrics.push(metric)
      let _ = Map::insert(groups, group_key, updated_group)
    }
    
    // 聚合每个分组
    let results = []
    for (group_key, group_metrics) in groups {
      let name = group_metrics[0].name
      let metric_type = group_metrics[0].metric_type
      let values = group_metrics.map(fn(m) { m.value })
      let count = values.length()
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (count as Float)
      let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
      let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
      
      // 合并所有标签（去重）
      let all_labels = []
      for metric in group_metrics {
        for label in metric.labels {
          if not all_labels.any(fn(l) { l.0 == label.0 && l.1 == label.1 }) {
            all_labels = all_labels.push(label)
          }
        }
      }
      
      results = results.push({
        name,
        metric_type,
        values,
        count,
        sum,
        avg,
        min,
        max,
        labels: all_labels
      })
    }
    
    results
  }
  
  // 创建直方图计算器
  let calculate_histogram = fn(values: Array[Float], buckets: Array[Float]) {
    let counts = buckets.map(fn(_) { 0 })
    let total_count = values.length()
    
    for value in values {
      let mut bucket_index = 0
      for i in 0..buckets.length() {
        if value <= buckets[i] {
          bucket_index = i
          break
        } else if i == buckets.length() - 1 {
          bucket_index = buckets.length()  // 超出最大桶
          break
        }
      }
      
      // 增加当前桶及所有更小桶的计数
      for i in 0..=bucket_index {
        if i < counts.length() {
          counts[i] = counts[i] + 1
        }
      }
    }
    
    let bucket_counts = []
    for i in 0..buckets.length() {
      bucket_counts = bucket_counts.push((buckets[i], counts[i]))
    }
    
    // 添加总和
    if bucket_index == buckets.length() {
      bucket_counts = bucket_counts.push(("+Inf", counts[buckets.length() - 1]))
    }
    
    {
      bucket_counts,
      total_count,
      sum: values.reduce(fn(acc, v) { acc + v }, 0.0)
    }
  }
  
  // 创建测试数据
  let test_metrics = [
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 1000, labels: [("method", "GET"), ("/status", "200")] },
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 2000, labels: [("method", "GET"), ("/status", "200")] },
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 3000, labels: [("method", "POST"), ("/status", "201")] },
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 4000, labels: [("method", "GET"), ("/status", "404")] },
    { name: "http_requests_total", metric_type: MetricType::Counter, value: 1.0, timestamp: 5000, labels: [("method", "POST"), ("/status", "400")] },
    { name: "response_time_seconds", metric_type: MetricType::Histogram, value: 0.1, timestamp: 1500, labels: [("endpoint", "/api/users")] },
    { name: "response_time_seconds", metric_type: MetricType::Histogram, value: 0.2, timestamp: 2500, labels: [("endpoint", "/api/users")] },
    { name: "response_time_seconds", metric_type: MetricType::Histogram, value: 0.5, timestamp: 3500, labels: [("endpoint", "/api/users")] },
    { name: "response_time_seconds", metric_type: MetricType::Histogram, value: 1.2, timestamp: 4500, labels: [("endpoint", "/api/users")] },
    { name: "response_time_seconds", metric_type: MetricType::Histogram, value: 0.3, timestamp: 5500, labels: [("endpoint", "/api/users")] }
  ]
  
  // 测试按状态码分组的HTTP请求聚合
  let request_aggregations = aggregate_metrics(test_metrics, ["status"])
  
  // 应该有3个分组：200, 201, 404, 400
  assert_eq(request_aggregations.length(), 4)
  
  // 验证200状态码的聚合
  let status_200_agg = request_aggregations.find(fn(agg) { 
    agg.labels.any(fn(l) { l.0 == "status" && l.1 == "200" }) 
  })
  assert_true(status_200_agg.is_some())
  assert_eq(status_200_agg.unwrap().count, 2)
  assert_eq(status_200_agg.unwrap().sum, 2.0)
  
  // 验证404状态码的聚合
  let status_404_agg = request_aggregations.find(fn(agg) { 
    agg.labels.any(fn(l) { l.0 == "status" && l.1 == "404" }) 
  })
  assert_true(status_404_agg.is_some())
  assert_eq(status_404_agg.unwrap().count, 1)
  assert_eq(status_404_agg.unwrap().sum, 1.0)
  
  // 测试响应时间直方图
  let response_times = test_metrics
    .filter(fn(m) { m.name == "response_time_seconds" })
    .map(fn(m) { m.value })
  
  let buckets = [0.1, 0.5, 1.0, 2.0]
  let histogram = calculate_histogram(response_times, buckets)
  
  assert_eq(histogram.total_count, 5)
  assert_eq(histogram.bucket_counts.length(), 4)
  
  // 验证桶计数
  let bucket_0_1 = histogram.bucket_counts.find_fn(bucket) { bucket.0 == 0.1 }
  assert_eq(bucket_0_1.unwrap().1, 1)  // 只有0.1在第一个桶
  
  let bucket_0_5 = histogram.bucket_counts.find_fn(bucket) { bucket.0 == 0.5 }
  assert_eq(bucket_0_5.unwrap().1, 3)  // 0.1, 0.2, 0.3, 0.5在第二个桶
  
  let bucket_1_0 = histogram.bucket_counts.find_fn(bucket) { bucket.0 == 1.0 }
  assert_eq(bucket_1_0.unwrap().1, 4)  // 0.1, 0.2, 0.3, 0.5, 1.2在第三个桶
}

// 测试5: 遥测数据的实时处理
test "遥测数据的实时处理" {
  // 定义事件类型
  enum TelemetryEvent {
    Metric(String, Float, Array[(String, String)])  // name, value, labels
    Trace(String, String, Int)  // trace_id, operation_name, duration_ms
    Log(String, String, String)  // level, message, timestamp
    Alert(String, String, Int)  // alert_name, message, severity
  }
  
  // 定义处理结果
  type ProcessingResult = {
    processed_count: Int,
    alerts_generated: Int,
    metrics_aggregated: Int,
    errors_count: Int
  }
  
  // 创建实时处理器
  let process_events = fn(events: Array[TelemetryEvent], rules: Array[(TelemetryEvent) -> Bool]) {
    let mut processed = 0
    let mut alerts = 0
    let mut metrics = 0
    let mut errors = 0
    
    for event in events {
      processed = processed + 1
      
      // 应用规则
      let rule_matches = rules.map(fn(rule) { rule(event) })
      let any_rule_matched = rule_matches.any(fn(matched) { matched })
      
      match event {
        TelemetryEvent::Metric(name, value, labels) => {
          metrics = metrics + 1
          
          // 检查度量规则
          if any_rule_matched {
            alerts = alerts + 1
          }
        }
        TelemetryEvent::Trace(trace_id, operation, duration) => {
          // 检查追踪规则
          if any_rule_matched {
            alerts = alerts + 1
          }
        }
        TelemetryEvent::Log(level, message, timestamp) => {
          // 检查日志规则
          if any_rule_matched {
            alerts = alerts + 1
          }
          
          // 统计错误日志
          if level == "ERROR" {
            errors = errors + 1
          }
        }
        TelemetryEvent::Alert(name, message, severity) => {
          alerts = alerts + 1
        }
      }
    }
    
    {
      processed_count: processed,
      alerts_generated: alerts,
      metrics_aggregated: metrics,
      errors_count: errors
    }
  }
  
  // 创建滑动窗口处理器
  let sliding_window_process = fn(events: Array[TelemetryEvent], window_size: Int) {
    if events.length() < window_size {
      return process_events(events, [])
    }
    
    let windows = []
    for i in 0..=(events.length() - window_size) {
      let window = events.slice(i, i + window_size)
      windows = windows.push(window)
    }
    
    // 处理每个窗口
    windows.map(fn(window) {
      process_events(window, [])
    })
  }
  
  // 创建测试事件
  let test_events = [
    TelemetryEvent::Metric("cpu_usage", 0.8, [("host", "server-1")]),
    TelemetryEvent::Metric("memory_usage", 0.9, [("host", "server-1")]),
    TelemetryEvent::Trace("trace-123", "database_query", 150),
    TelemetryEvent::Log("INFO", "Request processed", "2023-01-01T12:00:00Z"),
    TelemetryEvent::Log("ERROR", "Database connection failed", "2023-01-01T12:01:00Z"),
    TelemetryEvent::Alert("high_cpu", "CPU usage above 80%", 2),
    TelemetryEvent::Metric("response_time", 2.5, [("endpoint", "/api/users")]),
    TelemetryEvent::Trace("trace-456", "http_request", 500),
    TelemetryEvent::Log("WARN", "Slow query detected", "2023-01-01T12:02:00Z"),
    TelemetryEvent::Metric("disk_usage", 0.95, [("host", "server-1")])
  ]
  
  // 创建处理规则
  let rules = [
    fn(event) {
      match event {
        TelemetryEvent::Metric(name, value, _) => name == "cpu_usage" && value > 0.8
        _ => false
      }
    },
    fn(event) {
      match event {
        TelemetryEvent::Metric(name, value, _) => name == "memory_usage" && value > 0.85
        _ => false
      }
    },
    fn(event) {
      match event {
        TelemetryEvent::Trace(_, operation, duration) => duration > 400
        _ => false
      }
    },
    fn(event) {
      match event {
        TelemetryEvent::Log(level, _, _) => level == "ERROR"
        _ => false
      }
    }
  ]
  
  // 测试实时处理
  let result = process_events(test_events, rules)
  
  assert_eq(result.processed_count, 10)  // 处理了10个事件
  assert_eq(result.metrics_aggregated, 4)  // 4个度量事件
  assert_eq(result.errors_count, 1)  // 1个错误日志
  
  // 验证告警数量
  // CPU使用率>0.8: 1个告警
  // 内存使用率>0.85: 1个告警
  // 追踪延迟>400ms: 1个告警
  // 错误日志: 1个告警
  // 显式告警事件: 1个告警
  // 总计: 5个告警
  assert_eq(result.alerts_generated, 5)
  
  // 测试滑动窗口处理
  let window_results = sliding_window_process(test_events, 5)
  assert_eq(window_results.length(), 6)  // 10个事件，窗口大小5，应该有6个窗口
  
  // 验证第一个窗口
  let first_window = window_results[0]
  assert_eq(first_window.processed_count, 5)
  assert_eq(first_window.metrics_aggregated, 2)
}

// 测试6: 资源使用监控
test "资源使用监控功能" {
  // 定义资源类型
  enum ResourceType {
    CPU
    Memory
    Disk
    Network
  }
  
  // 定义资源度量
  type ResourceMetric = {
    resource_type: ResourceType,
    usage_percent: Float,
    timestamp: Int,
    instance_id: String,
    tags: Array[(String, String)]
  }
  
  // 定义资源警报
  type ResourceAlert = {
    alert_type: String,
    resource_type: ResourceType,
    instance_id: String,
    current_value: Float,
    threshold: Float,
    message: String,
    severity: String
  }
  
  // 创建资源监控器
  let monitor_resources = fn(metrics: Array[ResourceMetric], thresholds: Array[(ResourceType, Float)]) {
    let alerts = []
    
    for metric in metrics {
      // 查找对应的阈值
      let threshold = match thresholds.find(fn(t) { t.0 == metric.resource_type }) {
        Some((_, value)) => value
        None => 0.8  // 默认阈值
      }
      
      // 检查是否超过阈值
      if metric.usage_percent > threshold {
        let alert_type = match metric.resource_type {
          ResourceType::CPU => "high_cpu_usage"
          ResourceType::Memory => "high_memory_usage"
          ResourceType::Disk => "high_disk_usage"
          ResourceType::Network => "high_network_usage"
        }
        
        let severity = if metric.usage_percent > threshold + 0.1 {
          "critical"
        } else if metric.usage_percent > threshold + 0.05 {
          "warning"
        } else {
          "info"
        }
        
        let message = match metric.resource_type {
          ResourceType::CPU => "CPU usage is " + metric.usage_percent.to_string() + "%"
          ResourceType::Memory => "Memory usage is " + metric.usage_percent.to_string() + "%"
          ResourceType::Disk => "Disk usage is " + metric.usage_percent.to_string() + "%"
          ResourceType::Network => "Network usage is " + metric.usage_percent.to_string() + "%"
        }
        
        alerts = alerts.push({
          alert_type,
          resource_type: metric.resource_type,
          instance_id: metric.instance_id,
          current_value: metric.usage_percent,
          threshold,
          message,
          severity
        })
      }
    }
    
    alerts
  }
  
  // 创建资源趋势分析器
  let analyze_resource_trends = fn(metrics: Array[ResourceMetric], time_window_ms: Int) {
    if metrics.length() < 2 {
      return {
        trend: "insufficient_data",
        avg_usage: 0.0,
        peak_usage: 0.0,
        growth_rate: 0.0
      }
    }
    
    // 按时间戳排序
    let sorted_metrics = metrics.sort(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    // 计算时间窗口内的指标
    let window_start = sorted_metrics[0].timestamp
    let window_metrics = sorted_metrics.filter(fn(m) { m.timestamp <= window_start + time_window_ms })
    
    if window_metrics.length() < 2 {
      return {
        trend: "insufficient_data",
        avg_usage: 0.0,
        peak_usage: 0.0,
        growth_rate: 0.0
      }
    }
    
    let usages = window_metrics.map(fn(m) { m.usage_percent })
    let avg_usage = usages.reduce(fn(acc, u) { acc + u }, 0.0) / (usages.length() as Float)
    let peak_usage = usages.reduce(fn(acc, u) { if u > acc { u } else { acc } }, usages[0])
    
    // 计算增长率
    let first_usage = usages[0]
    let last_usage = usages[usages.length() - 1]
    let growth_rate = (last_usage - first_usage) / first_usage
    
    // 确定趋势
    let trend = if growth_rate > 0.1 {
      "increasing"
    } else if growth_rate < -0.1 {
      "decreasing"
    } else {
      "stable"
    }
    
    {
      trend,
      avg_usage,
      peak_usage,
      growth_rate
    }
  }
  
  // 创建测试数据
  let test_metrics = [
    { resource_type: ResourceType::CPU, usage_percent: 0.45, timestamp: 1000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::Memory, usage_percent: 0.67, timestamp: 1000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::Disk, usage_percent: 0.78, timestamp: 1000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::Network, usage_percent: 0.23, timestamp: 1000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::CPU, usage_percent: 0.52, timestamp: 2000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::Memory, usage_percent: 0.71, timestamp: 2000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::Disk, usage_percent: 0.81, timestamp: 2000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::Network, usage_percent: 0.35, timestamp: 2000, instance_id: "server-1", tags: [("region", "us-east-1")] },
    { resource_type: ResourceType::CPU, usage_percent: 0.85, timestamp: 3000, instance_id: "server-1", tags: [("region", "us-east-1")] },  // 超过阈值
    { resource_type: ResourceType::Memory, usage_percent: 0.92, timestamp: 3000, instance_id: "server-1", tags: [("region", "us-east-1")] },  // 超过阈值
    { resource_type: ResourceType::Disk, usage_percent: 0.83, timestamp: 3000, instance_id: "server-1", tags: [("region", "us-east-1")] },  // 超过阈值
    { resource_type: ResourceType::Network, usage_percent: 0.41, timestamp: 3000, instance_id: "server-1", tags: [("region", "us-east-1")] }
  ]
  
  // 定义资源阈值
  let thresholds = [
    (ResourceType::CPU, 0.8),
    (ResourceType::Memory, 0.85),
    (ResourceType::Disk, 0.8),
    (ResourceType::Network, 0.9)
  ]
  
  // 测试资源监控
  let alerts = monitor_resources(test_metrics, thresholds)
  
  // 应该有3个警报：CPU、内存和磁盘使用率超过阈值
  assert_eq(alerts.length(), 3)
  
  // 验证CPU警报
  let cpu_alert = alerts.find(fn(a) { a.resource_type == ResourceType::CPU })
  assert_true(cpu_alert.is_some())
  assert_eq(cpu_alert.unwrap().alert_type, "high_cpu_usage")
  assert_eq(cpu_alert.unwrap().current_value, 0.85)
  assert_eq(cpu_alert.unwrap().threshold, 0.8)
  assert_eq(cpu_alert.unwrap().severity, "info")  // 刚超过阈值，不是严重
  
  // 验证内存警报
  let memory_alert = alerts.find(fn(a) { a.resource_type == ResourceType::Memory })
  assert_true(memory_alert.is_some())
  assert_eq(memory_alert.unwrap().alert_type, "high_memory_usage")
  assert_eq(memory_alert.unwrap().current_value, 0.92)
  assert_eq(memory_alert.unwrap().threshold, 0.85)
  assert_eq(memory_alert.unwrap().severity, "warning")  // 超过阈值+0.05
  
  // 测试资源趋势分析
  let cpu_metrics = test_metrics.filter(fn(m) { m.resource_type == ResourceType::CPU })
  let cpu_trend = analyze_resource_trends(cpu_metrics, 3000)
  
  assert_eq(cpu_trend.trend, "increasing")  // CPU使用率在增加
  assert_true(cpu_trend.avg_usage > 0.45 && cpu_trend.avg_usage < 0.85)  // 平均值在范围内
  assert_eq(cpu_trend.peak_usage, 0.85)  // 峰值是0.85
  assert_true(cpu_trend.growth_rate > 0.0)  // 正增长
}

// 测试7: 遥测数据的安全性和隐私保护
test "遥测数据的安全性和隐私保护" {
  // 定义数据类型
  type TelemetryData = {
    payload: String,
    timestamp: Int,
    source: String,
    sensitivity_level: String  // "public", "internal", "confidential", "restricted"
  }
  
  // 定义加密配置
  type EncryptionConfig = {
    algorithm: String,
    key_rotation_interval_ms: Int,
    min_sensitivity_for_encryption: String
  }
  
  // 创建数据分类器
  let classify_data = fn(data: TelemetryData) {
    // 检查敏感信息模式
    let has_email = data.payload.contains("@")
    let has_phone = data.payload.any(fn(c) { c.is_digit() }) && data.payload.contains("-")
    let has_credit_card = data.payload.length() >= 13 && data.payload.length() <= 19 && data.payload.all(fn(c) { c.is_digit() || c == ' ' || c == '-' })
    let has_ip_address = data.payload.contains(".") && data.payload.split(".").length() == 4
    let has_sensitive_keywords = data.payload.contains("password") || data.payload.contains("token") || data.payload.contains("secret")
    
    let sensitivity = if has_credit_card || has_sensitive_keywords {
      "restricted"
    } else if has_email || has_phone {
      "confidential"
    } else if has_ip_address {
      "internal"
    } else {
      "public"
    }
    
    {
      data,
      sensitivity,
      has_email,
      has_phone,
      has_credit_card,
      has_ip_address,
      has_sensitive_keywords
    }
  }
  
  // 创建数据脱敏器
  let sanitize_data = fn(data: TelemetryData, sensitivity: String) {
    let payload = match sensitivity {
      "restricted" => {
        // 完全移除敏感数据
        "[REDACTED-RESTRICTED]"
      }
      "confidential" => {
        // 部分脱敏
        let sanitized = data.payload
          .replace(fn(s) { 
            if s.contains("@") {
              let parts = s.split("@")
              if parts.length() == 2 {
                let username = parts[0]
                let domain = parts[1]
                let masked_username = if username.length() > 2 {
                  username.substring(0, 2) + "*".repeat(username.length() - 2)
                } else {
                  "*".repeat(username.length())
                }
                masked_username + "@" + domain
              } else {
                s
              }
            } else {
              s
            }
          })
        sanitized
      }
      "internal" => {
        // 保留数据但标记为内部
        "[INTERNAL] " + data.payload
      }
      _ => data.payload  // public数据不需要脱敏
    }
    
    {
      payload,
      timestamp: data.timestamp,
      source: data.source,
      sensitivity_level: sensitivity
    }
  }
  
  // 创建访问控制检查器
  let check_access = fn(data: TelemetryData, user_role: String, user_clearance: String) {
    let sensitivity_levels = ["public", "internal", "confidential", "restricted"]
    let data_level_index = sensitivity_levels.find_index(fn(level) { level == data.sensitivity_level }).unwrap_or(0)
    let clearance_index = sensitivity_levels.find_index(fn(level) { level == user_clearance }).unwrap_or(0)
    
    let has_clearance = clearance_index >= data_level_index
    
    // 特殊角色检查
    let role_has_access = match user_role {
      "admin" => true,
      "security_analyst" => data.sensitivity_level != "restricted",
      "developer" => data.sensitivity_level == "public" || data.sensitivity_level == "internal",
      "externel_contractor" => data.sensitivity_level == "public",
      _ => false
    }
    
    has_clearance && role_has_access
  }
  
  // 创建审计日志记录器
  let log_access = fn(data: TelemetryData, user: String, action: String, granted: Bool) {
    {
      timestamp: Time::now(),
      user,
      action,
      data_source: data.source,
      data_sensitivity: data.sensitivity_level,
      access_granted: granted,
      audit_id: "audit-" + Time::now().to_string()
    }
  }
  
  // 创建测试数据
  let test_data = [
    { payload: "User logged in successfully", timestamp: 1000, source: "auth-service", sensitivity_level: "public" },
    { payload: "Request from 192.168.1.100 processed", timestamp: 2000, source: "api-gateway", sensitivity_level: "public" },
    { payload: "Database connection established to 10.0.0.5", timestamp: 3000, source: "database", sensitivity_level: "internal" },
    { payload: "User email: user@example.com registered", timestamp: 4000, source: "user-service", sensitivity_level: "public" },
    { payload: "Payment processed for card ending in 1234", timestamp: 5000, source: "payment-service", sensitivity_level: "public" },
    { payload: "API token: abc123def456 generated", timestamp: 6000, source: "auth-service", sensitivity_level: "public" },
    { payload: "Credit card number: 4111111111111111 expired", timestamp: 7000, source: "payment-service", sensitivity_level: "public" },
    { payload: "Password reset requested for user", timestamp: 8000, source: "auth-service", sensitivity_level: "public" }
  ]
  
  // 测试数据分类
  let classified_data = test_data.map(classify_data)
  
  // 验证分类结果
  let email_data = classified_data.find_fn(cd) { cd.has_email }
  assert_true(email_data.is_some())
  assert_eq(email_data.unwrap().sensitivity, "confidential")
  
  let ip_data = classified_data.find_fn(cd) { cd.has_ip_address }
  assert_true(ip_data.is_some())
  assert_eq(ip_data.unwrap().sensitivity, "internal")
  
  let credit_card_data = classified_data.find_fn(cd) { cd.has_credit_card }
  assert_true(credit_card_data.is_some())
  assert_eq(credit_card_data.unwrap().sensitivity, "restricted")
  
  let sensitive_keyword_data = classified_data.find_fn(cd) { cd.has_sensitive_keywords }
  assert_true(sensitive_keyword_data.is_some())
  assert_eq(sensitive_keyword_data.unwrap().sensitivity, "restricted")
  
  // 测试数据脱敏
  let confidential_data = classified_data.find_fn(cd) { cd.sensitivity == "confidential" }
  if confidential_data.is_some() {
    let sanitized = sanitize_data(confidential_data.unwrap().data, "confidential")
    assert_true(sanitized.payload.contains("*"))
    assert_false(sanitized.payload.contains("user@example.com"))
  }
  
  let restricted_data = classified_data.find_fn(cd) { cd.sensitivity == "restricted" }
  if restricted_data.is_some() {
    let sanitized = sanitize_data(restricted_data.unwrap().data, "restricted")
    assert_eq(sanitized.payload, "[REDACTED-RESTRICTED]")
  }
  
  // 测试访问控制
  let public_data = test_data[0]
  let restricted_data_item = classified_data.find_fn(cd) { cd.sensitivity == "restricted" }.unwrap().data
  
  // 管理员应该能访问所有数据
  assert_true(check_access(public_data, "admin", "public"))
  assert_true(check_access(restricted_data_item, "admin", "public"))
  
  // 开发者只能访问公开和内部数据
  assert_true(check_access(public_data, "developer", "public"))
  assert_false(check_access(restricted_data_item, "developer", "public"))
  
  // 外部承包商只能访问公开数据
  assert_true(check_access(public_data, "externel_contractor", "public"))
  assert_false(check_access(restricted_data_item, "externel_contractor", "public"))
  
  // 测试审计日志
  let audit_log = log_access(public_data, "developer", "read", true)
  assert_eq(audit_log.user, "developer")
  assert_eq(audit_log.action, "read")
  assert_eq(audit_log.data_source, "auth-service")
  assert_eq(audit_log.data_sensitivity, "public")
  assert_true(audit_log.access_granted)
}

// 测试8: 性能基准测试
test "性能基准测试功能" {
  // 定义基准测试结果
  type BenchmarkResult = {
    test_name: String,
    iterations: Int,
    total_time_ms: Int,
    avg_time_ms: Float,
    min_time_ms: Float,
    max_time_ms: Float,
    throughput_ops_per_sec: Float,
    memory_usage_mb: Float
  }
  
  // 创建基准测试执行器
  let run_benchmark = fn(test_name: String, test_fn: () -> Unit, iterations: Int) {
    let times = []
    let mut total_memory = 0.0
    
    for i in 0..iterations {
      let start_time = Time::now()
      let start_memory = get_memory_usage()  // 假设的函数
      
      test_fn()
      
      let end_time = Time::now()
      let end_memory = get_memory_usage()  // 假设的函数
      
      let duration = end_time - start_time
      times = times.push(duration)
      total_memory = total_memory + (end_memory - start_memory)
    }
    
    let total_time = times.reduce(fn(acc, time) { acc + time }, 0)
    let avg_time = (total_time as Float) / (iterations as Float)
    let min_time = times.reduce(fn(acc, time) { if time < acc { time } else { acc } }, times[0])
    let max_time = times.reduce(fn(acc, time) { if time > acc { time } else { acc } }, times[0])
    let throughput = 1000.0 / avg_time  // 操作每秒
    let avg_memory_usage = total_memory / (iterations as Float)
    
    {
      test_name,
      iterations,
      total_time_ms: total_time,
      avg_time_ms: avg_time,
      min_time_ms: min_time as Float,
      max_time_ms: max_time as Float,
      throughput_ops_per_sec: throughput,
      memory_usage_mb: avg_memory_usage
    }
  }
  
  // 假设的内存使用函数
  let get_memory_usage = fn() {
    50.0  // 假设50MB
  }
  
  // 创建测试函数
  let string_concat_test = fn() {
    let mut result = ""
    for i in 0..100 {
      result = result + "test-string-" + i.to_string()
    }
  }
  
  let array_sort_test = fn() {
    let data = []
    for i in 0..1000 {
      data = data.push(1000 - i)  // 逆序数组
    }
    data.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
  }
  
  let hash_map_test = fn() {
    let map = Map::empty()
    for i in 0..1000 {
      let _ = Map::insert(map, "key-" + i.to_string(), "value-" + i.to_string())
    }
    
    for i in 0..1000 {
      let _ = Map::get(map, "key-" + i.to_string())
    }
  }
  
  let regex_match_test = fn() {
    let patterns = ["test.*", "azimuth.*", "telemetry.*", "\\d+", "[a-z]+"]
    let strings = ["test-string", "azimuth-telemetry", "12345", "lowercase"]
    
    for pattern in patterns {
      for str in strings {
        let _ = str.matches(pattern)  // 假设的匹配函数
      }
    }
  }
  
  // 运行基准测试
  let benchmarks = [
    run_benchmark("string_concat", string_concat_test, 100),
    run_benchmark("array_sort", array_sort_test, 100),
    run_benchmark("hash_map_ops", hash_map_test, 100),
    run_benchmark("regex_match", regex_match_test, 100)
  ]
  
  // 验证基准测试结果
  assert_eq(benchmarks.length(), 4)
  
  // 验证字符串连接测试
  let string_benchmark = benchmarks.find_fn(b) { b.test_name == "string_concat" }
  assert_true(string_benchmark.is_some())
  assert_eq(string_benchmark.unwrap().iterations, 100)
  assert_true(string_benchmark.unwrap().avg_time_ms > 0.0)
  assert_true(string_benchmark.unwrap().throughput_ops_per_sec > 0.0)
  
  // 验证数组排序测试
  let sort_benchmark = benchmarks.find_fn(b) { b.test_name == "array_sort" }
  assert_true(sort_benchmark.is_some())
  assert_true(sort_benchmark.unwrap().max_time_ms >= sort_benchmark.unwrap().min_time_ms)
  
  // 验证哈希表操作测试
  let hashmap_benchmark = benchmarks.find_fn(b) { b.test_name == "hash_map_ops" }
  assert_true(hashmap_benchmark.is_some())
  assert_true(hashmap_benchmark.unwrap().memory_usage_mb >= 0.0)
  
  // 验证正则表达式匹配测试
  let regex_benchmark = benchmarks.find_fn(b) { b.test_name == "regex_match" }
  assert_true(regex_benchmark.is_some())
  assert_true(regex_benchmark.unwrap().total_time_ms > 0)
  
  // 比较性能
  let sorted_by_throughput = benchmarks.sort(fn(a, b) { 
    if a.throughput_ops_per_sec > b.throughput_ops_per_sec { -1 } 
    else if a.throughput_ops_per_sec < b.throughput_ops_per_sec { 1 } 
    else { 0 } 
  })
  
  // 最快的操作应该有最高的吞吐量
  let fastest = sorted_by_throughput[0]
  let slowest = sorted_by_throughput[sorted_by_throughput.length() - 1]
  assert_true(fastest.throughput_ops_per_sec >= slowest.throughput_ops_per_sec)
  
  // 创建性能回归检测器
  let detect_regression = fn(current: BenchmarkResult, baseline: BenchmarkResult, threshold_percent: Float) {
    if current.test_name != baseline.test_name {
      return false
    }
    
    let avg_time_diff = ((current.avg_time_ms - baseline.avg_time_ms) / baseline.avg_time_ms) * 100.0
    let throughput_diff = ((baseline.throughput_ops_per_sec - current.throughput_ops_per_sec) / baseline.throughput_ops_per_sec) * 100.0
    
    avg_time_diff > threshold_percent || throughput_diff > threshold_percent
  }
  
  // 测试性能回归检测
  let baseline = benchmarks[0]
  let current_with_regression = {
    test_name: baseline.test_name,
    iterations: baseline.iterations,
    total_time_ms: baseline.total_time_ms * 2,  // 两倍时间
    avg_time_ms: baseline.avg_time_ms * 2.0,
    min_time_ms: baseline.min_time_ms * 2.0,
    max_time_ms: baseline.max_time_ms * 2.0,
    throughput_ops_per_sec: baseline.throughput_ops_per_sec / 2.0,  // 一半吞吐量
    memory_usage_mb: baseline.memory_usage_mb
  }
  
  let has_regression = detect_regression(current_with_regression, baseline, 10.0)  // 10%阈值
  assert_true(has_regression)
  
  let current_without_regression = baseline
  let no_regression = detect_regression(current_without_regression, baseline, 10.0)
  assert_false(no_regression)
}

// 测试9: 遥测数据的可视化准备
test "遥测数据的可视化准备" {
  // 定义图表类型
  enum ChartType {
    Line
    Bar
    Pie
    Heatmap
    Scatter
  }
  
  // 定义数据点
  type DataPoint = {
    x: Float,
    y: Float,
    label: Option[String],
    color: Option[String],
    size: Option[Float]
  }
  
  // 定义图表数据
  type ChartData = {
    chart_type: ChartType,
    title: String,
    x_axis_label: String,
    y_axis_label: String,
    data_points: Array[DataPoint],
    annotations: Array[(String, Float, Float)]  // text, x, y
  }
  
  // 创建时间序列图表数据准备器
  let prepare_time_series_chart = fn(metrics: Array[(Int, Float)], title: String) {
    let data_points = metrics.map(fn(point) {
      let (timestamp, value) = point
      {
        x: timestamp as Float,
        y: value,
        label: Some(Time::format(timestamp)),
        color: None,
        size: None
      }
    })
    
    {
      chart_type: ChartType::Line,
      title,
      x_axis_label: "Time",
      y_axis_label: "Value",
      data_points,
      annotations: []
    }
  }
  
  // 创建柱状图数据准备器
  let prepare_bar_chart = fn(categories: Array[String], values: Array[Float], title: String) {
    let data_points = []
    for i in 0..categories.length() {
      data_points = data_points.push({
        x: i as Float,
        y: values[i],
        label: Some(categories[i]),
        color: None,
        size: None
      })
    }
    
    {
      chart_type: ChartType::Bar,
      title,
      x_axis_label: "Category",
      y_axis_label: "Value",
      data_points,
      annotations: []
    }
  }
  
  // 创建饼图数据准备器
  let prepare_pie_chart = fn(labels: Array[String], values: Array[Float], title: String) {
    let total = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let colors = ["#FF6384", "#36A2EB", "#FFCE56", "#4BC0C0", "#9966FF", "#FF9F40"]
    
    let data_points = []
    for i in 0..labels.length() {
      let percentage = (values[i] / total) * 100.0
      data_points = data_points.push({
        x: percentage,
        y: percentage,
        label: Some(labels[i] + " (" + percentage.round().to_string() + "%)"),
        color: Some(colors[i % colors.length()]),
        size: Some(values[i])
      })
    }
    
    {
      chart_type: ChartType::Pie,
      title,
      x_axis_label: "",
      y_axis_label: "",
      data_points,
      annotations: []
    }
  }
  
  // 创建热力图数据准备器
  let prepare_heatmap = fn(matrix: Array[Array[Float]], x_labels: Array[String], y_labels: Array[String], title: String) {
    let data_points = []
    
    for y in 0..matrix.length() {
      for x in 0..matrix[y].length() {
        let value = matrix[y][x]
        data_points = data_points.push({
          x: x as Float,
          y: y as Float,
          label: Some(x_labels[x] + " vs " + y_labels[y] + ": " + value.to_string()),
          color: None,  // 将由图表库根据值设置
          size: Some(value)
        })
      }
    }
    
    {
      chart_type: ChartType::Heatmap,
      title,
      x_axis_label: "X Axis",
      y_axis_label: "Y Axis",
      data_points,
      annotations: []
    }
  }
  
  // 创建散点图数据准备器
  let prepare_scatter_chart = fn(points: Array[(Float, Float)], title: String) {
    let data_points = points.map(fn(point) {
      let (x, y) = point
      {
        x,
        y,
        label: None,
        color: None,
        size: None
      }
    })
    
    {
      chart_type: ChartType::Scatter,
      title,
      x_axis_label: "X Value",
      y_axis_label: "Y Value",
      data_points,
      annotations: []
    }
  }
  
  // 创建图表数据序列化器
  let serialize_chart_data = fn(chart: ChartData) {
    let chart_type_str = match chart.chart_type {
      ChartType::Line => "line"
      ChartType::Bar => "bar"
      ChartType::Pie => "pie"
      ChartType::Heatmap => "heatmap"
      ChartType::Scatter => "scatter"
    }
    
    let points_str = chart.data_points.map(fn(point) {
      let x_str = point.x.to_string()
      let y_str = point.y.to_string()
      let label_str = match point.label {
        Some(l) => "\"" + l + "\""
        None => "null"
      }
      let color_str = match point.color {
        Some(c) => "\"" + c + "\""
        None => "null"
      }
      let size_str = match point.size {
        Some(s) => s.to_string()
        None => "null"
      }
      
      "{x:" + x_str + ",y:" + y_str + ",label:" + label_str + ",color:" + color_str + ",size:" + size_str + "}"
    }).join(",")
    
    "{"
    + "type:\"" + chart_type_str + "\"," 
    + "title:\"" + chart.title + "\"," 
    + "xAxisLabel:\"" + chart.x_axis_label + "\"," 
    + "yAxisLabel:\"" + chart.y_axis_label + "\"," 
    + "data:[" + points_str + "]"
    + "}"
  }
  
  // 创建测试数据
  let time_series_data = [
    (1000, 10.5),
    (2000, 12.3),
    (3000, 11.8),
    (4000, 14.2),
    (5000, 13.7)
  ]
  
  let categories = ["CPU", "Memory", "Disk", "Network"]
  let values = [0.75, 0.62, 0.43, 0.28]
  
  let pie_labels = ["Success", "Warning", "Error"]
  let pie_values = [85.0, 12.0, 3.0]
  
  let heatmap_matrix = [
    [0.8, 0.6, 0.4],
    [0.7, 0.5, 0.3],
    [0.9, 0.7, 0.5]
  ]
  let heatmap_x_labels = ["Server1", "Server2", "Server3"]
  let heatmap_y_labels = ["CPU", "Memory", "Disk"]
  
  let scatter_points = [
    (1.2, 3.4),
    (2.3, 5.6),
    (3.4, 2.1),
    (4.5, 6.7),
    (5.6, 4.3)
  ]
  
  // 测试时间序列图表
  let time_series_chart = prepare_time_series_chart(time_series_data, "Response Time Over Time")
  assert_eq(time_series_chart.chart_type, ChartType::Line)
  assert_eq(time_series_chart.title, "Response Time Over Time")
  assert_eq(time_series_chart.data_points.length(), 5)
  assert_eq(time_series_chart.data_points[0].x, 1000.0)
  assert_eq(time_series_chart.data_points[0].y, 10.5)
  
  // 测试柱状图
  let bar_chart = prepare_bar_chart(categories, values, "Resource Usage")
  assert_eq(bar_chart.chart_type, ChartType::Bar)
  assert_eq(bar_chart.title, "Resource Usage")
  assert_eq(bar_chart.data_points.length(), 4)
  assert_eq(bar_chart.data_points[0].x, 0.0)
  assert_eq(bar_chart.data_points[0].y, 0.75)
  assert_eq(bar_chart.data_points[0].label, Some("CPU"))
  
  // 测试饼图
  let pie_chart = prepare_pie_chart(pie_labels, pie_values, "Status Distribution")
  assert_eq(pie_chart.chart_type, ChartType::Pie)
  assert_eq(pie_chart.title, "Status Distribution")
  assert_eq(pie_chart.data_points.length(), 3)
  assert_eq(pie_chart.data_points[0].x, 85.0)  // Success的百分比
  assert_eq(pie_chart.data_points[0].y, 85.0)
  assert_true(pie_chart.data_points[0].label.unwrap().contains("Success"))
  assert_eq(pie_chart.data_points[0].color, Some("#FF6384"))
  
  // 测试热力图
  let heatmap_chart = prepare_heatmap(heatmap_matrix, heatmap_x_labels, heatmap_y_labels, "Server Performance")
  assert_eq(heatmap_chart.chart_type, ChartType::Heatmap)
  assert_eq(heatmap_chart.title, "Server Performance")
  assert_eq(heatmap_chart.data_points.length(), 9)  // 3x3矩阵
  assert_eq(heatmap_chart.data_points[0].x, 0.0)
  assert_eq(heatmap_chart.data_points[0].y, 0.0)
  assert_eq(heatmap_chart.data_points[0].y, 0.8)  // 第一个值
  
  // 测试散点图
  let scatter_chart = prepare_scatter_chart(scatter_points, "Correlation Analysis")
  assert_eq(scatter_chart.chart_type, ChartType::Scatter)
  assert_eq(scatter_chart.title, "Correlation Analysis")
  assert_eq(scatter_chart.data_points.length(), 5)
  assert_eq(scatter_chart.data_points[0].x, 1.2)
  assert_eq(scatter_chart.data_points[0].y, 3.4)
  
  // 测试序列化
  let serialized = serialize_chart_data(bar_chart)
  assert_true(serialized.contains("type:\"bar\""))
  assert_true(serialized.contains("title:\"Resource Usage\""))
  assert_true(serialized.contains("data:["))
}

// 测试10: 错误恢复和容错机制
test "错误恢复和容错机制" {
  // 定义错误类型
  enum SystemError {
    NetworkTimeout(String)
    DatabaseConnection(String)
    ServiceUnavailable(String)
    RateLimitExceeded(Int)  // 重试延迟
    ResourceExhausted(String)
  }
  
  // 定义恢复策略
  enum RecoveryStrategy {
    Retry(Int)  // 最大重试次数
    ExponentialBackoff(Int, Float)  // 初始延迟, 退避因子
    CircuitBreaker(Int, Int)  // 失败阈值, 恢复超时
    Failover(Array[String])  // 备用服务列表
    GracefulDegradation  // 降级功能
  }
  
  // 定义系统状态
  type SystemState = {
    service_name: String,
    is_healthy: Bool,
    consecutive_failures: Int,
    last_error: Option[SystemError],
    last_success_time: Int,
    circuit_breaker_open: Bool,
    circuit_breaker_open_time: Option[Int]
  }
  
  // 创建错误处理器
  let handle_error = fn(error: SystemError, strategy: RecoveryStrategy, state: SystemState) {
    match (error, strategy) {
      (SystemError::NetworkTimeout(service), RecoveryStrategy::Retry(max_attempts)) => {
        if state.consecutive_failures < max_attempts {
          ("retry", {
            service_name: state.service_name,
            is_healthy: false,
            consecutive_failures: state.consecutive_failures + 1,
            last_error: Some(error),
            last_success_time: state.last_success_time,
            circuit_breaker_open: false,
            circuit_breaker_open_time: None
          })
        } else {
          ("fail", {
            service_name: state.service_name,
            is_healthy: false,
            consecutive_failures: state.consecutive_failures + 1,
            last_error: Some(error),
            last_success_time: state.last_success_time,
            circuit_breaker_open: true,
            circuit_breaker_open_time: Some(Time::now())
          })
        }
      }
      (SystemError::ServiceUnavailable(service), RecoveryStrategy::ExponentialBackoff(initial_delay, backoff_factor)) => {
        let delay = initial_delay * (backoff_factor.pow(state.consecutive_failures))
        ("exponential_backoff", {
          service_name: state.service_name,
          is_healthy: false,
          consecutive_failures: state.consecutive_failures + 1,
          last_error: Some(error),
          last_success_time: state.last_success_time,
          circuit_breaker_open: false,
          circuit_breaker_open_time: None
        })
      }
      (SystemError::DatabaseConnection(msg), RecoveryStrategy::CircuitBreaker(threshold, timeout)) => {
        if state.consecutive_failures >= threshold {
          ("circuit_breaker_open", {
            service_name: state.service_name,
            is_healthy: false,
            consecutive_failures: state.consecutive_failures + 1,
            last_error: Some(error),
            last_success_time: state.last_success_time,
            circuit_breaker_open: true,
            circuit_breaker_open_time: Some(Time::now())
          })
        } else {
          ("retry", {
            service_name: state.service_name,
            is_healthy: false,
            consecutive_failures: state.consecutive_failures + 1,
            last_error: Some(error),
            last_success_time: state.last_success_time,
            circuit_breaker_open: false,
            circuit_breaker_open_time: None
          })
        }
      }
      (SystemError::RateLimitExceeded(delay), RecoveryStrategy::Failover(services)) => {
        if services.length() > 0 {
          ("failover", {
            service_name: services[0],
            is_healthy: true,
            consecutive_failures: 0,
            last_error: None,
            last_success_time: Time::now(),
            circuit_breaker_open: false,
            circuit_breaker_open_time: None
          })
        } else {
          ("delay_retry", {
            service_name: state.service_name,
            is_healthy: false,
            consecutive_failures: state.consecutive_failures + 1,
            last_error: Some(error),
            last_success_time: state.last_success_time,
            circuit_breaker_open: false,
            circuit_breaker_open_time: None
          })
        }
      }
      (SystemError::ResourceExhausted(resource), RecoveryStrategy::GracefulDegradation) => {
        ("graceful_degradation", {
          service_name: state.service_name,
          is_healthy: true,  // 服务仍然健康，但功能降级
          consecutive_failures: 0,
          last_error: Some(error),
          last_success_time: state.last_success_time,
          circuit_breaker_open: false,
          circuit_breaker_open_time: None
        })
      }
      (error, strategy) => {
        ("unknown_error", state)
      }
    }
  }
  
  // 创建断路器状态检查器
  let check_circuit_breaker = fn(state: SystemState, timeout_ms: Int) {
    if not state.circuit_breaker_open {
      return false  // 断路器未打开
    }
    
    match state.circuit_breaker_open_time {
      Some(open_time) => {
        let current_time = Time::now()
        current_time - open_time > timeout_ms
      }
      None => false
    }
  }
  
  // 创建健康状态检查器
  let check_health = fn(state: SystemState) {
    let time_since_last_success = Time::now() - state.last_success_time
    let recent_success = time_since_last_success < 60000  // 1分钟内有成功
    
    let low_failure_rate = state.consecutive_failures < 5
    let circuit_closed = not state.circuit_breaker_open
    
    recent_success && low_failure_rate && circuit_closed
  }
  
  // 创建初始状态
  let initial_state = {
    service_name: "primary-service",
    is_healthy: true,
    consecutive_failures: 0,
    last_error: None,
    last_success_time: Time::now(),
    circuit_breaker_open: false,
    circuit_breaker_open_time: None
  }
  
  // 测试网络超时错误和重试策略
  let network_error = SystemError::NetworkTimeout("api-service")
  let retry_strategy = RecoveryStrategy::Retry(3)
  let (action1, state1) = handle_error(network_error, retry_strategy, initial_state)
  assert_eq(action1, "retry")
  assert_eq(state1.consecutive_failures, 1)
  assert_false(state1.is_healthy)
  
  // 继续重试直到达到最大次数
  let (action2, state2) = handle_error(network_error, retry_strategy, state1)
  assert_eq(action2, "retry")
  assert_eq(state2.consecutive_failures, 2)
  
  let (action3, state3) = handle_error(network_error, retry_strategy, state2)
  assert_eq(action3, "retry")
  assert_eq(state3.consecutive_failures, 3)
  
  // 达到最大重试次数，应该触发断路器
  let (action4, state4) = handle_error(network_error, retry_strategy, state3)
  assert_eq(action4, "fail")
  assert_eq(state4.consecutive_failures, 4)
  assert_true(state4.circuit_breaker_open)
  assert_true(state4.circuit_breaker_open_time.is_some())
  
  // 测试服务不可用错误和指数退避策略
  let service_error = SystemError::ServiceUnavailable("payment-service")
  let backoff_strategy = RecoveryStrategy::ExponentialBackoff(1000, 2.0)
  let (action5, state5) = handle_error(service_error, backoff_strategy, initial_state)
  assert_eq(action5, "exponential_backoff")
  assert_eq(state5.consecutive_failures, 1)
  
  // 测试数据库连接错误和断路器策略
  let db_error = SystemError::DatabaseConnection("connection pool exhausted")
  let circuit_strategy = RecoveryStrategy::CircuitBreaker(3, 30000)
  let db_state = {
    service_name: "database-service",
    is_healthy: true,
    consecutive_failures: 2,  // 已经有2次失败
    last_error: None,
    last_success_time: Time::now(),
    circuit_breaker_open: false,
    circuit_breaker_open_time: None
  }
  let (action6, state6) = handle_error(db_error, circuit_strategy, db_state)
  assert_eq(action6, "circuit_breaker_open")
  assert_eq(state6.consecutive_failures, 3)
  assert_true(state6.circuit_breaker_open)
  
  // 测试速率限制错误和故障转移策略
  let rate_limit_error = SystemError::RateLimitExceeded(5000)
  let failover_strategy = RecoveryStrategy::Failover(["backup-service-1", "backup-service-2"])
  let (action7, state7) = handle_error(rate_limit_error, failover_strategy, initial_state)
  assert_eq(action7, "failover")
  assert_eq(state7.service_name, "backup-service-1")
  assert_true(state7.is_healthy)
  assert_eq(state7.consecutive_failures, 0)
  
  // 测试资源耗尽错误和优雅降级策略
  let resource_error = SystemError::ResourceExhausted("memory")
  let degradation_strategy = RecoveryStrategy::GracefulDegradation
  let (action8, state8) = handle_error(resource_error, degradation_strategy, initial_state)
  assert_eq(action8, "graceful_degradation")
  assert_true(state8.is_healthy)  // 服务仍然健康
  assert_true(state8.last_error.is_some())
  
  // 测试断路器状态检查
  let should_attempt_reset = check_circuit_breaker(state6, 100)  // 100ms超时
  assert_false(should_attempt_reset)  // 应该仍然打开，因为时间不够
  
  let should_attempt_reset_later = check_circuit_breaker(state6, 60000)  // 60秒超时
  assert_true(should_attempt_reset_later)  // 应该关闭，因为超时已过
  
  // 测试健康状态检查
  let is_healthy = check_health(initial_state)
  assert_true(is_healthy)
  
  let is_unhealthy = check_health(state4)
  assert_false(is_unhealthy)
}