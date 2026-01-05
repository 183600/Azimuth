// Azimuth 新增遥测测试用例
// 专注于遥测系统的高级功能和特性

// 测试1: 遥测数据的异常检测
test "telemetry anomaly detection" {
  // 模拟正常和异常的遥测数据
  let telemetry_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, status: "normal" },
    { timestamp: 1640995260, metric: "cpu", value: 48.0, status: "normal" },
    { timestamp: 1640995320, metric: "cpu", value: 95.0, status: "anomaly" },
    { timestamp: 1640995380, metric: "cpu", value: 42.0, status: "normal" },
    { timestamp: 1640995440, metric: "memory", value: 1024.0, status: "normal" },
    { timestamp: 1640995500, metric: "memory", value: 2048.0, status: "warning" },
    { timestamp: 1640995560, metric: "memory", value: 4096.0, status: "anomaly" }
  ]
  
  // 定义异常检测函数
  let detect_anomalies = fn(data: Array[T]) {
    let mut anomalies = []
    
    // 计算每个指标的平均值和标准差
    let cpu_values = data.filter(fn(item) { item.metric == "cpu" }).map(fn(item) { item.value })
    let memory_values = data.filter(fn(item) { item.metric == "memory" }).map(fn(item) { item.value })
    
    // CPU异常检测（阈值法）
    let cpu_avg = cpu_values.reduce(fn(acc, x) { acc + x }, 0.0) / cpu_values.length().to_float()
    let cpu_threshold = cpu_avg * 1.5  // 超过平均值1.5倍视为异常
    
    // 内存异常检测（固定阈值）
    let memory_threshold = 3000.0
    
    for item in data {
      let is_anomaly = match item.metric {
        "cpu" => item.value > cpu_threshold
        "memory" => item.value > memory_threshold
        _ => false
      }
      
      if is_anomaly {
        anomalies = anomalies.push({
          timestamp: item.timestamp,
          metric: item.metric,
          value: item.value,
          anomaly_type: "threshold_exceeded"
        })
      }
    }
    
    anomalies
  }
  
  let detected_anomalies = detect_anomalies(telemetry_data)
  
  assert_eq(detected_anomalies.length(), 2)
  assert_eq(detected_anomalies[0].timestamp, 1640995320)
  assert_eq(detected_anomalies[0].metric, "cpu")
  assert_eq(detected_anomalies[0].value, 95.0)
  assert_eq(detected_anomalies[1].timestamp, 1640995560)
  assert_eq(detected_anomalies[1].metric, "memory")
  assert_eq(detected_anomalies[1].value, 4096.0)
  
  // 测试异常评分
  let calculate_anomaly_score = fn(item: T, baseline: Float) {
    if item.value > baseline {
      (item.value - baseline) / baseline
    } else {
      0.0
    }
  }
  
  let cpu_baseline = 45.0
  let anomaly_score = calculate_anomaly_score(
    { timestamp: 1640995320, metric: "cpu", value: 95.0, status: "normal" },
    cpu_baseline
  )
  
  assert_eq(anomaly_score, 1.1111111111111112)  // (95-45)/45
}

// 测试2: 遥测数据的压缩和编码
test "telemetry data compression and encoding" {
  // 模拟原始遥测数据
  let raw_telemetry = [
    { service: "auth-service", version: "1.0.0", metric: "cpu", value: 45.0, timestamp: 1640995200 },
    { service: "auth-service", version: "1.0.0", metric: "memory", value: 1024.0, timestamp: 1640995201 },
    { service: "auth-service", version: "1.0.0", metric: "cpu", value: 50.0, timestamp: 1640995202 },
    { service: "db-service", version: "2.1.0", metric: "cpu", value: 30.0, timestamp: 1640995203 },
    { service: "db-service", version: "2.1.0", metric: "disk", value: 2048.0, timestamp: 1640995204 }
  ]
  
  // 数据字典压缩
  let create_dictionary = fn(data: Array[T]) {
    let mut dict = []
    let mut service_dict = []
    let mut version_dict = []
    let mut metric_dict = []
    
    for item in data {
      if not(service_dict.contains(item.service)) {
        service_dict = service_dict.push(item.service)
      }
      if not(version_dict.contains(item.version)) {
        version_dict = version_dict.push(item.version)
      }
      if not(metric_dict.contains(item.metric)) {
        metric_dict = metric_dict.push(item.metric)
      }
    }
    
    dict = [
      ("service", service_dict),
      ("version", version_dict),
      ("metric", metric_dict)
    ]
    
    dict
  }
  
  let dictionary = create_dictionary(raw_telemetry)
  
  // 编码数据
  let encode_data = fn(data: Array[T], dict: Array[(String, Array[String])]) {
    let mut encoded = []
    
    // 获取字典
    let service_dict = dict.filter(fn(entry) { entry.0 == "service" })[0].1
    let version_dict = dict.filter(fn(entry) { entry.0 == "version" })[0].1
    let metric_dict = dict.filter(fn(entry) { entry.0 == "metric" })[0].1
    
    for item in data {
      let get_index = fn(value: String, dict_array: Array[String]) {
        let mut index = 0
        for i in 0..dict_array.length() {
          if dict_array[i] == value {
            index = i
            break
          }
        }
        index
      }
      
      let encoded_item = {
        service_id: get_index(item.service, service_dict),
        version_id: get_index(item.version, version_dict),
        metric_id: get_index(item.metric, metric_dict),
        value: item.value,
        timestamp: item.timestamp
      }
      
      encoded = encoded.push(encoded_item)
    }
    
    encoded
  }
  
  let encoded_data = encode_data(raw_telemetry, dictionary)
  
  // 验证编码结果
  assert_eq(encoded_data.length(), 5)
  assert_eq(encoded_data[0].service_id, 0)  // auth-service
  assert_eq(encoded_data[0].version_id, 0)   // 1.0.0
  assert_eq(encoded_data[0].metric_id, 0)    // cpu
  assert_eq(encoded_data[3].service_id, 1)  // db-service
  assert_eq(encoded_data[3].version_id, 1)   // 2.1.0
  assert_eq(encoded_data[4].metric_id, 1)    // disk
  
  // 计算压缩率
  let original_size = raw_telemetry.length() * 4  // 4个字段
  let dict_size = dictionary.reduce(fn(acc, entry) { acc + entry.1.length() }, 0)
  let encoded_size = dict_size + encoded_data.length() * 3  // 3个ID字段 + 2个原始字段
  
  let compression_ratio = encoded_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 1.0)  // 应该有压缩效果
  
  // 时间序列压缩（差分编码）
  let compress_time_series = fn(data: Array[T]) {
    let mut compressed = []
    
    if data.length() > 0 {
      // 第一个数据点保持完整
      compressed = compressed.push(data[0])
      
      // 后续数据点只存储差值
      for i in 1..data.length() {
        let diff = {
          service: data[i].service,
          version: data[i].version,
          metric: data[i].metric,
          value: data[i].value,
          timestamp: data[i].timestamp - data[i-1].timestamp
        }
        compressed = compressed.push(diff)
      }
    }
    
    compressed
  }
  
  let compressed_series = compress_time_series(raw_telemetry.filter(fn(item) { item.metric == "cpu" }))
  
  assert_eq(compressed_series.length(), 3)
  assert_eq(compressed_series[0].timestamp, 1640995200)  // 第一个时间戳保持不变
  assert_eq(compressed_series[1].timestamp, 1)          // 差值
  assert_eq(compressed_series[2].timestamp, 1)          // 差值
}

// 测试3: 遥测数据的采样策略
test "telemetry data sampling strategies" {
  // 模拟高频遥测数据
  let high_frequency_data = []
  let mut i = 0
  while i < 100 {
    high_frequency_data = high_frequency_data.push({
      id: i,
      timestamp: 1640995200 + i,
      metric: "cpu",
      value: 45.0 + (i % 20).to_float(),
      priority: if i % 10 == 0 { "high" } else { "normal" }
    })
    i = i + 1
  }
  
  // 均匀采样策略
  let uniform_sampling = fn(data: Array[T], sample_rate: Int) {
    let mut sampled = []
    let step = data.length() / sample_rate
    
    for i in 0..sample_rate {
      let index = i * step
      if index < data.length() {
        sampled = sampled.push(data[index])
      }
    }
    
    sampled
  }
  
  let uniform_sampled = uniform_sampling(high_frequency_data, 10)
  assert_eq(uniform_sampled.length(), 10)
  assert_eq(uniform_sampled[0].id, 0)
  assert_eq(uniform_sampled[9].id, 90)
  
  // 基于优先级的采样策略
  let priority_sampling = fn(data: Array[T], high_priority_ratio: Float) {
    let high_priority_data = data.filter(fn(item) { item.priority == "high" })
    let normal_priority_data = data.filter(fn(item) { item.priority == "normal" })
    
    let high_priority_count = (high_priority_data.length().to_float() * high_priority_ratio).to_int()
    let normal_priority_count = (normal_priority_data.length().to_float() * (1.0 - high_priority_ratio)).to_int()
    
    let mut sampled = []
    
    // 采样高优先级数据
    for i in 0..high_priority_count {
      if i < high_priority_data.length() {
        sampled = sampled.push(high_priority_data[i])
      }
    }
    
    // 采样普通优先级数据
    let normal_step = normal_priority_data.length() / normal_priority_count
    for i in 0..normal_priority_count {
      let index = i * normal_step
      if index < normal_priority_data.length() {
        sampled = sampled.push(normal_priority_data[index])
      }
    }
    
    sampled
  }
  
  let priority_sampled = priority_sampling(high_frequency_data, 0.8)
  
  // 验证优先级采样结果
  let high_priority_in_sample = priority_sampled.filter(fn(item) { item.priority == "high" })
  let normal_priority_in_sample = priority_sampled.filter(fn(item) { item.priority == "normal" })
  
  assert_true(high_priority_in_sample.length() > 0)
  assert_true(normal_priority_in_sample.length() > 0)
  
  // 基于时间窗口的采样策略
  let time_window_sampling = fn(data: Array[T], window_size: Int) {
    let mut sampled = []
    let mut window_start = 0
    
    while window_start < data.length() {
      let window_end = if window_start + window_size < data.length() {
        window_start + window_size
      } else {
        data.length()
      }
      
      // 在每个窗口中选择一个代表性数据点（例如中间值）
      let window_index = (window_start + window_end) / 2
      if window_index < data.length() {
        sampled = sampled.push(data[window_index])
      }
      
      window_start = window_end
    }
    
    sampled
  }
  
  let time_window_sampled = time_window_sampling(high_frequency_data, 20)
  assert_eq(time_window_sampled.length(), 5)  // 100个数据点，窗口大小20，应该有5个采样点
  assert_eq(time_window_sampled[0].id, 10)    // 第一个窗口的中间点
  assert_eq(time_window_sampled[4].id, 90)    // 最后一个窗口的中间点
  
  // 自适应采样策略（基于数据变化率）
  let adaptive_sampling = fn(data: Array[T], threshold: Float) {
    let mut sampled = []
    
    if data.length() > 0 {
      sampled = sampled.push(data[0])  // 总是包含第一个数据点
      
      for i in 1..data.length() {
        let change_rate = (data[i].value - data[i-1].value).abs() / data[i-1].value
        
        if change_rate > threshold {
          sampled = sampled.push(data[i])
        }
      }
    }
    
    sampled
  }
  
  let adaptive_sampled = adaptive_sampling(high_frequency_data, 0.1)
  
  // 验证自适应采样结果
  assert_true(adaptive_sampled.length() > 1)
  assert_eq(adaptive_sampled[0].id, 0)
  
  // 验证采样点的变化率都超过阈值
  for i in 1..adaptive_sampled.length() {
    let current = adaptive_sampled[i]
    let previous = adaptive_sampled[i-1]
    let change_rate = (current.value - previous.value).abs() / previous.value
    assert_true(change_rate >= 0.1)
  }
}

// 测试4: 遥测数据的分布式追踪
test "distributed tracing with telemetry data" {
  // 定义分布式追踪数据结构
  type TraceEvent = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // 模拟分布式追踪数据
  let trace_events = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "handle_request",
      start_time: 1640995200,
      end_time: 1640995250,
      status: "ok",
      tags: [("http.method", "GET"), ("http.path", "/api/users")]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      service_name: "auth-service",
      operation_name: "authenticate_user",
      start_time: 1640995210,
      end_time: 1640995230,
      status: "ok",
      tags: [("user.id", "12345"), ("auth.method", "jwt")]
    },
    {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      service_name: "user-service",
      operation_name: "get_user_profile",
      start_time: 1640995235,
      end_time: 1640995245,
      status: "ok",
      tags: [("user.id", "12345"), ("cache.hit", "false")]
    },
    {
      trace_id: "trace-002",
      span_id: "span-004",
      parent_span_id: None,
      service_name: "api-gateway",
      operation_name: "handle_request",
      start_time: 1640995300,
      end_time: 1640995400,
      status: "error",
      tags: [("http.method", "POST"), ("http.path", "/api/orders")]
    },
    {
      trace_id: "trace-002",
      span_id: "span-005",
      parent_span_id: Some("span-004"),
      service_name: "order-service",
      operation_name: "create_order",
      start_time: 1640995310,
      end_time: 1640995390,
      status: "error",
      tags: [("user.id", "67890"), ("error.code", "500")]
    }
  ]
  
  // 按追踪ID分组
  let group_by_trace = fn(events: Array[TraceEvent]) {
    let mut traces = []
    let mut processed_traces = []
    
    for event in events {
      if not(traces.contains(event.trace_id)) {
        traces = traces.push(event.trace_id)
      }
    }
    
    for trace_id in traces {
      let trace_events = events.filter(fn(event) { event.trace_id == trace_id })
      processed_traces = processed_traces.push({
        trace_id: trace_id,
        events: trace_events
      })
    }
    
    processed_traces
  }
  
  let grouped_traces = group_by_trace(trace_events)
  assert_eq(grouped_traces.length(), 2)
  assert_eq(grouped_traces[0].trace_id, "trace-001")
  assert_eq(grouped_traces[0].events.length(), 3)
  assert_eq(grouped_traces[1].trace_id, "trace-002")
  assert_eq(grouped_traces[1].events.length(), 2)
  
  // 构建追踪树
  let build_trace_tree = fn(events: Array[TraceEvent]) {
    let mut roots = []
    let mut children = []
    
    // 分离根span和子span
    for event in events {
      match event.parent_span_id {
        None => roots = roots.push(event)
        Some(_) => children = children.push(event)
      }
    }
    
    // 为每个根span构建子树
    let build_tree = fn(root: TraceEvent, all_children: Array[TraceEvent]) {
      let direct_children = all_children.filter(fn(child) {
        match child.parent_span_id {
          Some(parent_id) => parent_id == root.span_id
          None => false
        }
      })
      
      let processed_children = []
      for child in direct_children {
        processed_children = processed_children.push(build_tree(child, all_children))
      }
      
      {
        span: root,
        children: processed_children
      }
    }
    
    let trees = []
    for root in roots {
      trees = trees.push(build_tree(root, children))
    }
    
    trees
  }
  
  let trace001_events = grouped_traces.filter(fn(trace) { trace.trace_id == "trace-001" })[0].events
  let trace001_tree = build_trace_tree(trace001_events)
  
  assert_eq(trace001_tree.length(), 1)
  assert_eq(trace001_tree[0].span.span_id, "span-001")
  assert_eq(trace001_tree[0].children.length(), 2)
  assert_eq(trace001_tree[0].children[0].span.span_id, "span-002")
  assert_eq(trace001_tree[0].children[1].span.span_id, "span-003")
  
  // 计算追踪统计信息
  let calculate_trace_stats = fn(events: Array[TraceEvent]) {
    let mut total_duration = 0
    let mut service_counts = []
    let mut status_counts = []
    
    for event in events {
      let duration = event.end_time - event.start_time
      total_duration = total_duration + duration
      
      // 统计服务调用次数
      let service_name = event.service_name
      let mut found = false
      for i in 0..service_counts.length() {
        if service_counts[i].0 == service_name {
          service_counts[i] = (service_name, service_counts[i].1 + 1)
          found = true
          break
        }
      }
      if not(found) {
        service_counts = service_counts.push((service_name, 1))
      }
      
      // 统计状态
      let status = event.status
      let mut status_found = false
      for i in 0..status_counts.length() {
        if status_counts[i].0 == status {
          status_counts[i] = (status, status_counts[i].1 + 1)
          status_found = true
          break
        }
      }
      if not(status_found) {
        status_counts = status_counts.push((status, 1))
      }
    }
    
    {
      total_spans: events.length(),
      total_duration: total_duration,
      average_duration: total_duration / events.length(),
      service_counts: service_counts,
      status_counts: status_counts
    }
  }
  
  let trace001_stats = calculate_trace_stats(trace001_events)
  assert_eq(trace001_stats.total_spans, 3)
  assert_eq(trace001_stats.total_duration, 100)
  assert_eq(trace001_stats.average_duration, 33)
  assert_eq(trace001_stats.service_counts.length(), 3)
  assert_eq(trace001_stats.status_counts[0].0, "ok")
  assert_eq(trace001_stats.status_counts[0].1, 3)
  
  // 错误追踪分析
  let analyze_error_traces = fn(traces: Array[T]) {
    let error_traces = traces.filter(fn(trace) {
      let has_error = trace.events.reduce(fn(acc, event) {
        acc or event.status == "error"
      }, false)
      has_error
    })
    
    let error_analysis = []
    for trace in error_traces {
      let error_spans = trace.events.filter(fn(event) { event.status == "error" })
      let root_span = trace.events.filter(fn(event) { event.parent_span_id == None })[0]
      
      error_analysis = error_analysis.push({
        trace_id: trace.trace_id,
        root_operation: root_span.operation_name,
        error_count: error_spans.length(),
        error_services: error_spans.map(fn(span) { span.service_name }),
        total_duration: root_span.end_time - root_span.start_time
      })
    }
    
    error_analysis
  }
  
  let error_analysis = analyze_error_traces(grouped_traces)
  assert_eq(error_analysis.length(), 1)
  assert_eq(error_analysis[0].trace_id, "trace-002")
  assert_eq(error_analysis[0].root_operation, "handle_request")
  assert_eq(error_analysis[0].error_count, 2)
  assert_true(error_analysis[0].error_services.contains("api-gateway"))
  assert_true(error_analysis[0].error_services.contains("order-service"))
}

// 测试5: 遥测数据的性能优化
test "telemetry data performance optimization" {
  // 模拟大量遥测数据
  let mut large_dataset = []
  let mut i = 0
  while i < 1000 {
    large_dataset = large_dataset.push({
      id: i,
      timestamp: 1640995200 + i,
      service: "service-" + (i % 10).to_string(),
      metric: "metric-" + (i % 5).to_string(),
      value: (i % 100).to_float(),
      tags: [("env", "production"), ("version", "1.0.0")]
    })
    i = i + 1
  }
  
  // 批处理优化
  let batch_process = fn(data: Array[T], batch_size: Int) {
    let mut batches = []
    let mut start = 0
    
    while start < data.length() {
      let end = if start + batch_size < data.length() {
        start + batch_size
      } else {
        data.length()
      }
      
      let batch = data.slice(start, end)
      batches = batches.push(batch)
      start = end
    }
    
    batches
  }
  
  let batches = batch_process(large_dataset, 100)
  assert_eq(batches.length(), 10)
  assert_eq(batches[0].length(), 100)
  assert_eq(batches[9].length(), 100)
  
  // 并行处理模拟（使用批处理）
  let parallel_aggregate = fn(batches: Array[Array[T]]) {
    let process_batch = fn(batch: Array[T]) {
      let mut sum = 0.0
      let mut count = 0
      
      for item in batch {
        sum = sum + item.value
        count = count + 1
      }
      
      {
        batch_sum: sum,
        batch_count: count,
        batch_avg: sum / count.to_float()
      }
    }
    
    let mut results = []
    for batch in batches {
      results = results.push(process_batch(batch))
    }
    
    // 合并结果
    let total_sum = results.reduce(fn(acc, result) { acc + result.batch_sum }, 0.0)
    let total_count = results.reduce(fn(acc, result) { acc + result.batch_count }, 0)
    
    {
      total_sum: total_sum,
      total_count: total_count,
      overall_avg: total_sum / total_count.to_float(),
      batch_count: results.length()
    }
  }
  
  let aggregation_result = parallel_aggregate(batches)
  assert_eq(aggregation_result.total_count, 1000)
  assert_eq(aggregation_result.batch_count, 10)
  assert_eq(aggregation_result.overall_avg, 49.5)  // 0-99的平均值
  
  // 索引优化
  let create_index = fn(data: Array[T], field: String) {
    let mut index = []
    
    for item in data {
      let key = match field {
        "service" => item.service
        "metric" => item.metric
        _ => ""
      }
      
      let mut found = false
      for i in 0..index.length() {
        if index[i].0 == key {
          index[i] = (key, index[i].1 + 1)
          found = true
          break
        }
      }
      
      if not(found) {
        index = index.push((key, 1))
      }
    }
    
    index
  }
  
  let service_index = create_index(large_dataset, "service")
  let metric_index = create_index(large_dataset, "metric")
  
  assert_eq(service_index.length(), 10)
  assert_eq(metric_index.length(), 5)
  
  // 验证索引正确性
  for entry in service_index {
    assert_eq(entry.1, 100)  // 每个服务应该有100条记录
  }
  
  for entry in metric_index {
    assert_eq(entry.1, 200)  // 每个指标应该有200条记录
  }
  
  // 使用索引进行快速查询
  let indexed_query = fn(data: Array[T], index: Array[(String, Int)], field: String, value: String) {
    let count = index.reduce(fn(acc, entry) {
      if entry.0 == value { entry.1 } else { acc }
    }, 0)
    
    count
  }
  
  let service0_count = indexed_query(large_dataset, service_index, "service", "service-0")
  let metric0_count = indexed_query(large_dataset, metric_index, "metric", "metric-0")
  
  assert_eq(service0_count, 100)
  assert_eq(metric0_count, 200)
  
  // 内存优化 - 使用循环缓冲区
  let circular_buffer = fn(capacity: Int) {
    let mut buffer = []
    let mut head = 0
    let mut size = 0
    
    {
      add: fn(item: T) {
        if size < capacity {
          buffer = buffer.push(item)
          size = size + 1
        } else {
          buffer[head] = item
          head = (head + 1) % capacity
        }
      },
      
      get_all: fn() {
        if size < capacity {
          buffer
        } else {
          let mut result = []
          for i in 0..capacity {
            let index = (head + i) % capacity
            result = result.push(buffer[index])
          }
          result
        }
      },
      
      size: fn() { size }
    }
  }
  
  let buffer = circular_buffer(100)
  
  // 添加数据
  for i in 0..150 {
    buffer.add({
      id: i,
      value: i.to_float(),
      timestamp: 1640995200 + i
    })
  }
  
  // 验证缓冲区大小
  assert_eq(buffer.size(), 100)
  
  // 验证数据内容（应该包含最新的100条记录）
  let buffer_data = buffer.get_all()
  assert_eq(buffer_data.length(), 100)
  assert_eq(buffer_data[0].id, 50)   // 最旧的数据点
  assert_eq(buffer_data[99].id, 149) // 最新的数据点
}

// 测试6: 遥测数据的安全性和隐私保护
test "telemetry data security and privacy protection" {
  // 模拟包含敏感信息的遥测数据
  let sensitive_telemetry = [
    {
      user_id: "user-12345",
      email: "user@example.com",
      ip_address: "192.168.1.100",
      session_id: "sess-abcdef123456",
      action: "login",
      timestamp: 1640995200,
      service: "auth-service"
    },
    {
      user_id: "user-67890",
      email: "admin@example.com",
      ip_address: "10.0.0.50",
      session_id: "sess-xyz789012345",
      action: "data_access",
      timestamp: 1640995250,
      service: "user-service"
    },
    {
      user_id: "user-11111",
      email: "test@example.com",
      ip_address: "172.16.0.25",
      session_id: "sess-pqr555666777",
      action: "query",
      timestamp: 1640995300,
      service: "query-service"
    }
  ]
  
  // 数据脱敏函数
  let sanitize_data = fn(data: Array[T]) {
    let sanitize_email = fn(email: String) {
      let parts = email.split("@")
      if parts.length() == 2 {
        let username = parts[0]
        let domain = parts[1]
        let sanitized_username = if username.length() > 2 {
          username.substring(0, 2) + "*".repeat(username.length() - 2)
        } else {
          "*".repeat(username.length())
        }
        sanitized_username + "@" + domain
      } else {
        "***@***.***"
      }
    }
    
    let sanitize_ip = fn(ip: String) {
      let parts = ip.split(".")
      if parts.length() == 4 {
        parts[0] + "." + parts[1] + ".*.*"
      } else {
        "*.*.*.*"
      }
    }
    
    let sanitize_id = fn(id: String) {
      if id.length() > 4 {
        id.substring(0, 4) + "*".repeat(id.length() - 4)
      } else {
        "*".repeat(id.length())
      }
    }
    
    let sanitized = []
    for item in data {
      sanitized = sanitized.push({
        user_id: sanitize_id(item.user_id),
        email: sanitize_email(item.email),
        ip_address: sanitize_ip(item.ip_address),
        session_id: sanitize_id(item.session_id),
        action: item.action,
        timestamp: item.timestamp,
        service: item.service
      })
    }
    
    sanitized
  }
  
  let sanitized_data = sanitize_data(sensitive_telemetry)
  
  // 验证脱敏结果
  assert_eq(sanitized_data.length(), 3)
  assert_eq(sanitized_data[0].user_id, "user-*****")
  assert_eq(sanitized_data[0].email, "us***@example.com")
  assert_eq(sanitized_data[0].ip_address, "192.168.*.*")
  assert_eq(sanitized_data[0].session_id, "sess-************")
  
  assert_eq(sanitized_data[1].user_id, "user-*****")
  assert_eq(sanitized_data[1].email, "ad***@example.com")
  assert_eq(sanitized_data[1].ip_address, "10.0.*.*")
  assert_eq(sanitized_data[1].session_id, "sess-************")
  
  // 数据分类和标记
  let classify_data = fn(data: Array[T]) {
    let classified = []
    
    for item in data {
      let sensitivity_level = match item.action {
        "login" => "high"
        "data_access" => "high"
        "query" => "medium"
        _ => "low"
      }
      
      let pii_fields = []
      if item.user_id != "" { pii_fields = pii_fields.push("user_id") }
      if item.email != "" { pii_fields = pii_fields.push("email") }
      if item.ip_address != "" { pii_fields = pii_fields.push("ip_address") }
      
      classified = classified.push({
        original_data: item,
        sensitivity_level: sensitivity_level,
        pii_fields: pii_fields,
        requires_consent: sensitivity_level == "high"
      })
    }
    
    classified
  }
  
  let classified_data = classify_data(sensitive_telemetry)
  
  // 验证分类结果
  assert_eq(classified_data.length(), 3)
  assert_eq(classified_data[0].sensitivity_level, "high")
  assert_eq(classified_data[0].pii_fields.length(), 4)
  assert_true(classified_data[0].requires_consent)
  
  assert_eq(classified_data[1].sensitivity_level, "high")
  assert_eq(classified_data[1].pii_fields.length(), 4)
  assert_true(classified_data[1].requires_consent)
  
  assert_eq(classified_data[2].sensitivity_level, "medium")
  assert_eq(classified_data[2].pii_fields.length(), 4)
  assert_false(classified_data[2].requires_consent)
  
  // 访问控制模拟
  let check_access = fn(user_role: String, data_sensitivity: String) {
    match user_role {
      "admin" => true
      "analyst" => data_sensitivity != "high"
      "viewer" => data_sensitivity == "low"
      _ => false
    }
  }
  
  // 测试访问控制
  assert_true(check_access("admin", "high"))
  assert_false(check_access("analyst", "high"))
  assert_true(check_access("analyst", "medium"))
  assert_false(check_access("viewer", "medium"))
  assert_true(check_access("viewer", "low"))
  
  // 数据加密模拟
  let encrypt_field = fn(value: String, key: String) {
    // 简单的XOR加密模拟
    let mut encrypted = ""
    for i in 0..value.length() {
      let char_code = value[i].to_int()
      let key_code = key[i % key.length()].to_int()
      let encrypted_code = char_code ^ key_code
      encrypted = encrypted + encrypted_code.to_char()
    }
    encrypted
  }
  
  let decrypt_field = fn(encrypted: String, key: String) {
    // XOR解密（与加密相同）
    encrypt_field(encrypted, key)
  }
  
  let encryption_key = "secret-key-12345"
  let original_email = "user@example.com"
  let encrypted_email = encrypt_field(original_email, encryption_key)
  let decrypted_email = decrypt_field(encrypted_email, encryption_key)
  
  assert_eq(original_email, decrypted_email)
  assert_true(encrypted_email != original_email)
  
  // 数据保留策略
  let apply_retention_policy = fn(data: Array[T], retention_days: Int, current_timestamp: Int) {
    let cutoff_timestamp = current_timestamp - (retention_days * 86400)
    
    let filtered = []
    for item in data {
      if item.timestamp >= cutoff_timestamp {
        filtered = filtered.push(item)
      }
    }
    
    filtered
  }
  
  // 测试数据保留策略
  let current_time = 1640995400  // 2022-01-01 00:03:20 UTC
  let retained_data = apply_retention_policy(sensitive_telemetry, 1, current_time)
  
  // 所有数据都在1天内，所以应该全部保留
  assert_eq(retained_data.length(), 3)
  
  // 测试30天保留策略
  let old_data = [
    {
      user_id: "user-old",
      email: "old@example.com",
      ip_address: "1.2.3.4",
      session_id: "sess-old",
      action: "login",
      timestamp: 1640995200 - (31 * 86400),  // 31天前
      service: "auth-service"
    }
  ]
  
  let filtered_old_data = apply_retention_policy(old_data, 30, current_time)
  assert_eq(filtered_old_data.length(), 0)  // 应该被过滤掉
}

// 测试7: 遥测数据的实时监控
test "real-time monitoring with telemetry data" {
  // 模拟实时遥测数据流
  let telemetry_stream = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "web" },
    { timestamp: 1640995201, metric: "memory", value: 1024.0, service: "web" },
    { timestamp: 1640995202, metric: "cpu", value: 50.0, service: "api" },
    { timestamp: 1640995203, metric: "disk", value: 2048.0, service: "db" },
    { timestamp: 1640995204, metric: "cpu", value: 75.0, service: "web" },
    { timestamp: 1640995205, metric: "memory", value: 1536.0, service: "api" },
    { timestamp: 1640995206, metric: "cpu", value: 90.0, service: "web" },
    { timestamp: 1640995207, metric: "network", value: 100.0, service: "web" },
    { timestamp: 1640995208, metric: "cpu", value: 85.0, service: "api" },
    { timestamp: 1640995209, metric: "memory", value: 2048.0, service: "db" }
  ]
  
  // 实时阈值监控
  let threshold_monitoring = fn(stream: Array[T]) {
    let thresholds = [
      ("cpu", 80.0),
      ("memory", 1800.0),
      ("disk", 2500.0),
      ("network", 150.0)
    ]
    
    let mut alerts = []
    
    for data_point in stream {
      for threshold in thresholds {
        if data_point.metric == threshold.0 and data_point.value > threshold.1 {
          alerts = alerts.push({
            timestamp: data_point.timestamp,
            service: data_point.service,
            metric: data_point.metric,
            value: data_point.value,
            threshold: threshold.1,
            severity: if data_point.value > threshold.1 * 1.2 { "critical" } else { "warning" }
          })
        }
      }
    }
    
    alerts
  }
  
  let alerts = threshold_monitoring(telemetry_stream)
  
  // 验证告警结果
  assert_eq(alerts.length(), 3)
  assert_eq(alerts[0].timestamp, 1640995206)
  assert_eq(alerts[0].service, "web")
  assert_eq(alerts[0].metric, "cpu")
  assert_eq(alerts[0].value, 90.0)
  assert_eq(alerts[0].severity, "warning")
  
  assert_eq(alerts[1].timestamp, 1640995208)
  assert_eq(alerts[1].service, "api")
  assert_eq(alerts[1].metric, "cpu")
  assert_eq(alerts[1].value, 85.0)
  assert_eq(alerts[1].severity, "warning")
  
  assert_eq(alerts[2].timestamp, 1640995209)
  assert_eq(alerts[2].service, "db")
  assert_eq(alerts[2].metric, "memory")
  assert_eq(alerts[2].value, 2048.0)
  assert_eq(alerts[2].severity, "warning")
  
  // 滑动窗口监控
  let sliding_window_monitor = fn(stream: Array[T], window_size: Int) {
    let mut window_stats = []
    
    for i in window_size..stream.length() {
      let window = stream.slice(i - window_size, i)
      
      // 计算窗口统计信息
      let cpu_values = window.filter(fn(dp) { dp.metric == "cpu" }).map(fn(dp) { dp.value })
      let memory_values = window.filter(fn(dp) { dp.metric == "memory" }).map(fn(dp) { dp.value })
      
      let cpu_avg = if cpu_values.length() > 0 {
        cpu_values.reduce(fn(acc, v) { acc + v }, 0.0) / cpu_values.length().to_float()
      } else {
        0.0
      }
      
      let memory_avg = if memory_values.length() > 0 {
        memory_values.reduce(fn(acc, v) { acc + v }, 0.0) / memory_values.length().to_float()
      } else {
        0.0
      }
      
      window_stats = window_stats.push({
        window_end: stream[i-1].timestamp,
        cpu_avg: cpu_avg,
        memory_avg: memory_avg,
        data_points: window.length()
      })
    }
    
    window_stats
  }
  
  let window_stats = sliding_window_monitor(telemetry_stream, 5)
  
  // 验证滑动窗口统计
  assert_eq(window_stats.length(), 5)  // 10个数据点，窗口大小5，应该有5个窗口
  
  // 验证第一个窗口（数据点0-4）
  assert_eq(window_stats[0].window_end, 1640995204)
  assert_eq(window_stats[0].cpu_avg, (45.0 + 50.0 + 75.0) / 3.0)
  assert_eq(window_stats[0].memory_avg, 1024.0)
  
  // 验证最后一个窗口（数据点5-9）
  assert_eq(window_stats[4].window_end, 1640995209)
  assert_eq(window_stats[4].cpu_avg, (90.0 + 85.0) / 2.0)
  assert_eq(window_stats[4].memory_avg, (1536.0 + 2048.0) / 2.0)
  
  // 趋势检测
  let trend_detection = fn(values: Array[Float]) {
    if values.length() < 3 {
      "insufficient_data"
    } else {
      let first_half = values.slice(0, values.length() / 2)
      let second_half = values.slice(values.length() / 2, values.length())
      
      let first_avg = first_half.reduce(fn(acc, v) { acc + v }, 0.0) / first_half.length().to_float()
      let second_avg = second_half.reduce(fn(acc, v) { acc + v }, 0.0) / second_half.length().to_float()
      
      let change_rate = (second_avg - first_avg) / first_avg
      
      if change_rate > 0.1 {
        "increasing"
      } else if change_rate < -0.1 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  let cpu_values = telemetry_stream.filter(fn(dp) { dp.metric == "cpu" }).map(fn(dp) { dp.value })
  let cpu_trend = trend_detection(cpu_values)
  
  assert_eq(cpu_trend, "increasing")  // CPU值从45逐渐增加到85
  
  // 服务健康评分
  let calculate_health_score = fn(service_data: Array[T]) {
    let mut score = 100.0
    
    // CPU评分
    let cpu_values = service_data.filter(fn(dp) { dp.metric == "cpu" }).map(fn(dp) { dp.value })
    if cpu_values.length() > 0 {
      let cpu_avg = cpu_values.reduce(fn(acc, v) { acc + v }, 0.0) / cpu_values.length().to_float()
      if cpu_avg > 80.0 {
        score = score - (cpu_avg - 80.0)
      }
    }
    
    // 内存评分
    let memory_values = service_data.filter(fn(dp) { dp.metric == "memory" }).map(fn(dp) { dp.value })
    if memory_values.length() > 0 {
      let memory_avg = memory_values.reduce(fn(acc, v) { acc + v }, 0.0) / memory_values.length().to_float()
      if memory_avg > 1800.0 {
        score = score - ((memory_avg - 1800.0) / 10.0)
      }
    }
    
    if score < 0.0 { score = 0.0 }
    score
  }
  
  let web_service_data = telemetry_stream.filter(fn(dp) { dp.service == "web" })
  let web_health_score = calculate_health_score(web_service_data)
  
  // Web服务的CPU值：45, 75, 90, 85，平均值约为73.75，低于80，所以不应该扣分
  // 但90超过了80，应该扣10分
  assert_eq(web_health_score, 90.0)
  
  // 实时仪表板数据准备
  let prepare_dashboard_data = fn(stream: Array[T]) {
    let services = []
    let metrics = []
    
    // 获取所有服务
    for dp in stream {
      if not(services.contains(dp.service)) {
        services = services.push(dp.service)
      }
      if not(metrics.contains(dp.metric)) {
        metrics = metrics.push(dp.metric)
      }
    }
    
    let service_metrics = []
    for service in services {
      let service_data = stream.filter(fn(dp) { dp.service == service })
      let health_score = calculate_health_score(service_data)
      
      let metric_values = []
      for metric in metrics {
        let metric_data = service_data.filter(fn(dp) { dp.metric == metric })
        if metric_data.length() > 0 {
          let latest_value = metric_data[metric_data.length() - 1].value
          metric_values = metric_values.push((metric, latest_value))
        }
      }
      
      service_metrics = service_metrics.push({
        service: service,
        health_score: health_score,
        metrics: metric_values
      })
    }
    
    {
      services: services,
      metrics: metrics,
      service_metrics: service_metrics,
      last_update: stream[stream.length() - 1].timestamp
    }
  }
  
  let dashboard_data = prepare_dashboard_data(telemetry_stream)
  
  // 验证仪表板数据
  assert_eq(dashboard_data.services.length(), 3)
  assert_true(dashboard_data.services.contains("web"))
  assert_true(dashboard_data.services.contains("api"))
  assert_true(dashboard_data.services.contains("db"))
  
  assert_eq(dashboard_data.metrics.length(), 4)
  assert_true(dashboard_data.metrics.contains("cpu"))
  assert_true(dashboard_data.metrics.contains("memory"))
  
  assert_eq(dashboard_data.service_metrics.length(), 3)
  assert_eq(dashboard_data.last_update, 1640995209)
}

// 测试8: 遥测数据的自适应配置
test "adaptive configuration with telemetry data" {
  // 模拟配置参数
  let base_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    timeout_ms: 5000,
    retry_count: 3,
    compression_enabled: true,
    alert_thresholds: [
      ("cpu", 80.0),
      ("memory", 1800.0),
      ("disk", 2500.0)
    ]
  }
  
  // 模拟系统性能指标
  let system_metrics = [
    { timestamp: 1640995200, cpu_usage: 45.0, memory_usage: 1024.0, throughput: 1000 },
    { timestamp: 1640995260, cpu_usage: 55.0, memory_usage: 1200.0, throughput: 1200 },
    { timestamp: 1640995320, cpu_usage: 70.0, memory_usage: 1500.0, throughput: 1300 },
    { timestamp: 1640995380, cpu_usage: 85.0, memory_usage: 1800.0, throughput: 1100 },
    { timestamp: 1640995440, cpu_usage: 90.0, memory_usage: 2000.0, throughput: 900 }
  ]
  
  // 自适应采样率调整
  let adaptive_sampling_rate = fn(metrics: Array[T], current_rate: Float) {
    // 计算最近的平均CPU使用率
    let recent_metrics = metrics.slice(metrics.length() - 3, metrics.length())
    let avg_cpu = recent_metrics.reduce(fn(acc, m) { acc + m.cpu_usage }, 0.0) / recent_metrics.length().to_float()
    
    // 根据CPU使用率调整采样率
    let new_rate = if avg_cpu > 80.0 {
      // 高负载时降低采样率
      (current_rate * 0.8).max(0.1)
    } else if avg_cpu < 50.0 {
      // 低负载时提高采样率
      (current_rate * 1.2).min(1.0)
    } else {
      // 中等负载时保持当前采样率
      current_rate
    }
    
    new_rate
  }
  
  let adjusted_sampling_rate = adaptive_sampling_rate(system_metrics, base_config.sampling_rate)
  
  // 最近的平均CPU使用率：(70 + 85 + 90) / 3 = 81.67，高于80，所以应该降低采样率
  assert_eq(adjusted_sampling_rate, 0.8)
  
  // 自适应批处理大小调整
  let adaptive_batch_size = fn(metrics: Array[T], current_batch_size: Int) {
    // 计算最近的平均吞吐量
    let recent_metrics = metrics.slice(metrics.length() - 3, metrics.length())
    let avg_throughput = recent_metrics.reduce(fn(acc, m) { acc + m.throughput }, 0) / recent_metrics.length()
    
    // 根据吞吐量调整批处理大小
    let new_batch_size = if avg_throughput > 1200 {
      // 高吞吐量时增大批处理大小
      current_batch_size + 20
    } else if avg_throughput < 1000 {
      // 低吞吐量时减小批处理大小
      (current_batch_size - 20).max(10)
    } else {
      // 中等吞吐量时保持当前批处理大小
      current_batch_size
    }
    
    new_batch_size
  }
  
  let adjusted_batch_size = adaptive_batch_size(system_metrics, base_config.batch_size)
  
  // 最近的平均吞吐量：(1300 + 1100 + 900) / 3 = 1100，在1000-1200之间，所以保持不变
  assert_eq(adjusted_batch_size, 100)
  
  // 自适应超时调整
  let adaptive_timeout = fn(metrics: Array[T], current_timeout: Int) {
    // 计算最近的平均响应时间（模拟）
    let recent_metrics = metrics.slice(metrics.length() - 3, metrics.length())
    let avg_response_time = recent_metrics.reduce(fn(acc, m) { 
      // 模拟响应时间与CPU使用率成正比
      acc + (1000 + m.cpu_usage * 10).to_int() 
    }, 0) / recent_metrics.length()
    
    // 根据平均响应时间调整超时
    let new_timeout = if avg_response_time > current_timeout * 0.8 {
      // 响应时间接近超时，增加超时时间
      current_timeout + 1000
    } else if avg_response_time < current_timeout * 0.4 {
      // 响应时间远小于超时，减少超时时间
      (current_timeout - 1000).max(1000)
    } else {
      // 响应时间适中，保持当前超时
      current_timeout
    }
    
    new_timeout
  }
  
  let adjusted_timeout = adaptive_timeout(system_metrics, base_config.timeout_ms)
  
  // 计算平均响应时间并调整超时
  assert_true(adjusted_timeout >= 5000)  // 应该大于或等于原始超时
  
  // 自适应告警阈值调整
  let adaptive_alert_thresholds = fn(metrics: Array[T], current_thresholds: Array[(String, Float)]) {
    let adjusted_thresholds = []
    
    for threshold in current_thresholds {
      let metric_name = threshold.0
      let current_threshold = threshold.1
      
      // 获取最近的指标值
      let recent_values = match metric_name {
        "cpu" => metrics.slice(metrics.length() - 3, metrics.length()).map(fn(m) { m.cpu_usage })
        "memory" => metrics.slice(metrics.length() - 3, metrics.length()).map(fn(m) { m.memory_usage })
        _ => []
      }
      
      if recent_values.length() > 0 {
        let avg_value = recent_values.reduce(fn(acc, v) { acc + v }, 0.0) / recent_values.length().to_float()
        let max_value = recent_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, recent_values[0])
        
        // 根据最近值调整阈值
        let new_threshold = if max_value > current_threshold {
          // 如果最近值超过当前阈值，提高阈值
          current_threshold + (max_value - current_threshold) * 0.2
        } else if avg_value < current_threshold * 0.7 {
          // 如果平均值远低于阈值，降低阈值
          current_threshold * 0.9
        } else {
          // 保持当前阈值
          current_threshold
        }
        
        adjusted_thresholds = adjusted_thresholds.push((metric_name, new_threshold))
      } else {
        adjusted_thresholds = adjusted_thresholds.push(threshold)
      }
    }
    
    adjusted_thresholds
  }
  
  let adjusted_thresholds = adaptive_alert_thresholds(system_metrics, base_config.alert_thresholds)
  
  // 验证CPU阈值调整
  let cpu_threshold = adjusted_thresholds.filter(fn(th) { th.0 == "cpu" })[0].1
  let recent_cpu_values = system_metrics.slice(system_metrics.length() - 3, system_metrics.length()).map(fn(m) { m.cpu_usage })
  let max_cpu = recent_cpu_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, recent_cpu_values[0])
  
  // 最大CPU值为90，原始阈值为80，应该提高阈值
  assert_true(cpu_threshold > 80.0)
  
  // 生成自适应配置
  let generate_adaptive_config = fn(base_config: T, metrics: Array[T]) {
    {
      sampling_rate: adaptive_sampling_rate(metrics, base_config.sampling_rate),
      batch_size: adaptive_batch_size(metrics, base_config.batch_size),
      timeout_ms: adaptive_timeout(metrics, base_config.timeout_ms),
      retry_count: base_config.retry_count,  // 保持不变
      compression_enabled: base_config.compression_enabled,  // 保持不变
      alert_thresholds: adaptive_alert_thresholds(metrics, base_config.alert_thresholds),
      last_updated: metrics[metrics.length() - 1].timestamp
    }
  }
  
  let adaptive_config = generate_adaptive_config(base_config, system_metrics)
  
  // 验证自适应配置
  assert_eq(adaptive_config.sampling_rate, 0.8)
  assert_eq(adaptive_config.batch_size, 100)
  assert_true(adaptive_config.timeout_ms >= 5000)
  assert_eq(adaptive_config.retry_count, 3)
  assert_true(adaptive_config.compression_enabled)
  assert_eq(adaptive_config.last_updated, 1640995440)
  
  // 配置效果验证
  let validate_config_effectiveness = fn(old_config: T, new_config: T, metrics: Array[T]) {
    // 模拟使用新配置后的性能
    let old_performance = {
      cpu_overhead: old_config.sampling_rate * 10.0,
      memory_usage: old_config.batch_size * 0.5,
      processing_time: old_config.timeout_ms * 0.8,
      alert_accuracy: 0.85
    }
    
    let new_performance = {
      cpu_overhead: new_config.sampling_rate * 10.0,
      memory_usage: new_config.batch_size * 0.5,
      processing_time: new_config.timeout_ms * 0.8,
      alert_accuracy: 0.92  // 假设调整阈值后准确性提高
    }
    
    // 计算改进
    let cpu_improvement = (old_performance.cpu_overhead - new_performance.cpu_overhead) / old_performance.cpu_overhead
    let memory_change = (new_performance.memory_usage - old_performance.memory_usage) / old_performance.memory_usage
    let processing_change = (new_performance.processing_time - old_performance.processing_time) / old_performance.processing_time
    let accuracy_improvement = new_performance.alert_accuracy - old_performance.alert_accuracy
    
    {
      cpu_improvement: cpu_improvement,
      memory_change: memory_change,
      processing_change: processing_change,
      accuracy_improvement: accuracy_improvement,
      overall_better: cpu_improvement > 0.0 and accuracy_improvement > 0.0
    }
  }
  
  let config_effectiveness = validate_config_effectiveness(base_config, adaptive_config, system_metrics)
  
  // 验证配置效果
  assert_true(config_effectiveness.cpu_improvement > 0.0)  // CPU开销应该降低
  assert_eq(config_effectiveness.accuracy_improvement, 0.07)  // 准确性应该提高
  assert_true(config_effectiveness.overall_better)  // 整体应该更好
}