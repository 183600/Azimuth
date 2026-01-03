// Azimuth 高质量遥测系统测试用例
// 包含遥测系统核心功能的高级测试用例

// 测试1: 时间序列数据处理和分析
test "时间序列数据处理和分析" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array<(String, String)>
  }
  
  // 定义时间窗口
  enum TimeWindow {
    Minute
    Hour
    Day
    Week
    Custom(Int)  // 自定义窗口大小（秒）
  }
  
  // 时间序列聚合器
  let aggregate_time_series = fn(points: Array[TimeSeriesPoint], window: TimeWindow) {
    let window_size_ms = match window {
      TimeWindow::Minute => 60 * 1000
      TimeWindow::Hour => 60 * 60 * 1000
      TimeWindow::Day => 24 * 60 * 60 * 1000
      TimeWindow::Week => 7 * 24 * 60 * 60 * 1000
      TimeWindow::Custom(seconds) => seconds * 1000
    }
    
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    
    let mut result = []
    let mut window_start = sorted_points[0].timestamp
    let mut window_points = []
    
    for point in sorted_points {
      if point.timestamp - window_start < window_size_ms {
        window_points = window_points.push(point)
      } else {
        // 处理当前窗口
        if window_points.length() > 0 {
          let values = window_points.map(fn(p) { p.value })
          let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
          let avg = sum / (values.length() as Float)
          let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
          let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
          
          result = result.push({
            timestamp: window_start,
            value: avg,
            tags: [("aggregated", "true"), ("count", values.length().to_string())]
          })
        }
        
        // 开始新窗口
        window_start = point.timestamp
        window_points = [point]
      }
    }
    
    // 处理最后一个窗口
    if window_points.length() > 0 {
      let values = window_points.map(fn(p) { p.value })
      let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
      let avg = sum / (values.length() as Float)
      
      result = result.push({
        timestamp: window_start,
        value: avg,
        tags: [("aggregated", "true"), ("count", values.length().to_string())]
      })
    }
    
    result
  }
  
  // 创建测试数据
  let test_points = [
    { timestamp: 1000, value: 10.5, tags: [("service", "api")] },
    { timestamp: 30000, value: 12.3, tags: [("service", "api")] },
    { timestamp: 70000, value: 11.8, tags: [("service", "api")] },
    { timestamp: 150000, value: 13.2, tags: [("service", "api")] },
    { timestamp: 180000, value: 14.1, tags: [("service", "api")] }
  ]
  
  // 测试分钟级聚合
  let minute_aggregation = aggregate_time_series(test_points, TimeWindow::Minute)
  assert_eq(minute_aggregation.length(), 3)  // 3个时间窗口
  
  // 第一个窗口: 1000, 30000, 70000 -> 平均值约为11.53
  assert_eq(minute_aggregation[0].timestamp, 1000)
  assert_eq(minute_aggregation[0].tags[1].1, "3")  // 3个数据点
  
  // 第二个窗口: 150000, 180000 -> 平均值约为13.65
  assert_eq(minute_aggregation[1].timestamp, 150000)
  assert_eq(minute_aggregation[1].tags[1].1, "2")  // 2个数据点
  
  // 测试自定义窗口聚合
  let custom_aggregation = aggregate_time_series(test_points, TimeWindow::Custom(120))  // 2分钟窗口
  assert_eq(custom_aggregation.length(), 2)  // 2个时间窗口
}

// 测试2: 分布式追踪链路管理
test "分布式追踪链路管理" {
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)],
    events: Array[(Int, String)]  // (timestamp, event_name)
  }
  
  // 创建新的Span
  let create_span = fn(trace_id: String, operation_name: String, parent_span_id: Option[String]) {
    {
      trace_id,
      span_id: "span-" + Time::now().to_string(),
      parent_span_id,
      operation_name,
      start_time: Time::now(),
      end_time: 0,  // 尚未结束
      status: "RUNNING",
      tags: [],
      events: []
    }
  }
  
  // 结束Span
  let finish_span = fn(span: Span, status: String) {
    { span with 
      end_time: Time::now(),
      status: status
    }
  }
  
  // 添加事件到Span
  let add_event = fn(span: Span, event_name: String) {
    let timestamp = Time::now()
    let new_events = span.events.push((timestamp, event_name))
    { span with events: new_events }
  }
  
  // 添加标签到Span
  let add_tag = fn(span: Span, key: String, value: String) {
    let new_tags = span.tags.push((key, value))
    { span with tags: new_tags }
  }
  
  // 构建追踪树
  let build_trace_tree = fn(spans: Array[Span]) {
    let root_spans = spans.filter(fn(s) { s.parent_span_id.is_none() })
    
    let find_children = fn(parent_id: String) {
      spans.filter(fn(s) { 
        match s.parent_span_id {
          Some(id) => id == parent_id
          None => false
        }
      })
    }
    
    let build_tree_recursive = fn(span: Span) {
      let children = find_children(span.span_id)
      let child_trees = children.map(build_tree_recursive)
      (span, child_trees)
    }
    
    root_spans.map(build_tree_recursive)
  }
  
  // 创建测试追踪链路
  let trace_id = "trace-12345"
  
  // 根Span
  let root_span = create_span(trace_id, "http.request", None)
  let root_with_tag = add_tag(root_span, "http.method", "GET")
  let root_with_event = add_event(root_with_tag, "request.started")
  
  // 子Span
  let child_span1 = create_span(trace_id, "database.query", Some(root_span.span_id))
  let child1_with_tag = add_tag(child_span1, "db.statement", "SELECT * FROM users")
  let child1_finished = finish_span(child1_with_tag, "OK")
  
  let child_span2 = create_span(trace_id, "cache.get", Some(root_span.span_id))
  let child2_with_tag = add_tag(child_span2, "cache.key", "user:123")
  let child2_finished = finish_span(child2_with_tag, "MISS")
  
  // 孙子Span
  let grandchild_span = create_span(trace_id, "cache.set", Some(child2_finished.span_id))
  let grandchild_with_tag = add_tag(grandchild_span, "cache.key", "user:123")
  let grandchild_finished = finish_span(grandchild_with_tag, "OK")
  
  // 结束根Span
  let root_finished = finish_span(root_with_event, "OK")
  
  // 构建Span数组
  let all_spans = [root_finished, child1_finished, child2_finished, grandchild_finished]
  
  // 测试追踪树构建
  let trace_tree = build_trace_tree(all_spans)
  assert_eq(trace_tree.length(), 1)  // 只有一个根Span
  
  let (root_span_in_tree, root_children) = trace_tree[0]
  assert_eq(root_span_in_tree.operation_name, "http.request")
  assert_eq(root_children.length(), 2)  // 两个子Span
  
  let (child1_in_tree, child1_children) = root_children[0]
  assert_eq(child1_in_tree.operation_name, "database.query")
  assert_eq(child1_children.length(), 0)  // 没有子Span
  
  let (child2_in_tree, child2_children) = root_children[1]
  assert_eq(child2_in_tree.operation_name, "cache.get")
  assert_eq(child2_children.length(), 1)  // 一个子Span
  
  let (grandchild_in_tree, grandchild_children) = child2_children[0]
  assert_eq(grandchild_in_tree.operation_name, "cache.set")
  assert_eq(grandchild_children.length(), 0)  // 没有子Span
}

// 测试3: 性能监控和指标收集
test "性能监控和指标收集" {
  // 定义指标类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义指标
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // 定义指标收集器
  type MetricCollector = {
    metrics: Array[Metric],
    start_time: Int
  }
  
  // 创建指标收集器
  let create_collector = fn() {
    {
      metrics: [],
      start_time: Time::now()
    }
  }
  
  // 记录指标
  let record_metric = fn(collector: MetricCollector, name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)]) {
    let metric = {
      name,
      metric_type,
      value,
      labels,
      timestamp: Time::now()
    }
    
    let updated_metrics = collector.metrics.push(metric)
    { collector with metrics: updated_metrics }
  }
  
  // 按名称和标签过滤指标
  let filter_metrics = fn(collector: MetricCollector, name: String, labels: Array[(String, String)]) {
    collector.metrics.filter(fn(m) {
      if m.name != name {
        return false
      }
      
      for label in labels {
        let match_found = m.labels.any(fn(ml) { ml.0 == label.0 && ml.1 == label.1 })
        if not match_found {
          return false
        }
      }
      
      true
    })
  }
  
  // 计算指标统计
  let calculate_metrics_stats = fn(metrics: Array[Metric]) {
    if metrics.length() == 0 {
      return {
        count: 0,
        sum: 0.0,
        avg: 0.0,
        min: 0.0,
        max: 0.0,
        latest: 0.0
      }
    }
    
    let values = metrics.map(fn(m) { m.value })
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let avg = sum / (count as Float)
    let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    
    // 找到最新的值
    let latest_metric = metrics.reduce(fn(acc, m) { 
      if m.timestamp > acc.timestamp { m } else { acc } 
    }, metrics[0])
    let latest = latest_metric.value
    
    {
      count,
      sum,
      avg,
      min,
      max,
      latest
    }
  }
  
  // 创建测试数据
  let collector = create_collector()
  
  // 记录各种指标
  let collector1 = record_metric(collector, "http.requests.total", MetricType::Counter, 1.0, [("method", "GET"), ("status", "200")])
  let collector2 = record_metric(collector1, "http.requests.total", MetricType::Counter, 1.0, [("method", "GET"), ("status", "200")])
  let collector3 = record_metric(collector2, "http.requests.total", MetricType::Counter, 1.0, [("method", "POST"), ("status", "201")])
  let collector4 = record_metric(collector3, "cpu.usage", MetricType::Gauge, 75.5, [("host", "server1")])
  let collector5 = record_metric(collector4, "cpu.usage", MetricType::Gauge, 80.2, [("host", "server1")])
  let collector6 = record_metric(collector5, "cpu.usage", MetricType::Gauge, 45.3, [("host", "server2")])
  let collector7 = record_metric(collector6, "request.duration", MetricType::Histogram, 120.5, [("endpoint", "/api/users")])
  let collector8 = record_metric(collector7, "request.duration", MetricType::Histogram, 85.3, [("endpoint", "/api/users")])
  let collector9 = record_metric(collector8, "request.duration", MetricType::Histogram, 200.1, [("endpoint", "/api/users")])
  
  // 测试指标过滤
  let get_requests = filter_metrics(collector9, "http.requests.total", [("method", "GET")])
  assert_eq(get_requests.length(), 2)  // 两个GET请求
  
  let server1_cpu = filter_metrics(collector9, "cpu.usage", [("host", "server1")])
  assert_eq(server1_cpu.length(), 2)  // 两个server1的CPU指标
  
  // 测试指标统计
  let get_requests_stats = calculate_metrics_stats(get_requests)
  assert_eq(get_requests_stats.count, 2)
  assert_eq(get_requests_stats.sum, 2.0)
  assert_eq(get_requests_stats.avg, 1.0)
  
  let server1_cpu_stats = calculate_metrics_stats(server1_cpu)
  assert_eq(server1_cpu_stats.count, 2)
  assert_eq(server1_cpu_stats.sum.round(), 155.7)
  assert_eq(server1_cpu_stats.avg.round(), 77.85)
  assert_eq(server1_cpu_stats.min, 75.5)
  assert_eq(server1_cpu_stats.max, 80.2)
  
  let request_duration_stats = calculate_metrics_stats(filter_metrics(collector9, "request.duration", []))
  assert_eq(request_duration_stats.count, 3)
  assert_eq(request_duration_stats.sum.round(), 405.9)
  assert_eq(request_duration_stats.avg.round(), 135.3)
  assert_eq(request_duration_stats.min, 85.3)
  assert_eq(request_duration_stats.max, 200.1)
}

// 测试4: 数据序列化和反序列化
test "数据序列化和反序列化" {
  // 定义可序列化类型
  enum SerializationFormat {
    Json
    Protobuf
    Avro
    Custom(String)
  }
  
  // 定义序列化结果
  type SerializationResult = {
    data: Array[Byte],
    format: SerializationFormat,
    size: Int
  }
  
  // 定义遥测数据
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    duration: Int,
    status: String,
    tags: Array[(String, String)],
    logs: Array[(Int, String)]
  }
  
  // JSON序列化
  let serialize_to_json = fn(data: TelemetryData) {
    let json_str = "{" +
      "\"trace_id\":\"" + data.trace_id + "\"," +
      "\"span_id\":\"" + data.span_id + "\"," +
      "\"parent_span_id\":" + match data.parent_span_id {
        Some(id) => "\"" + id + "\""
        None => "null"
      } + "," +
      "\"operation_name\":\"" + data.operation_name + "\"," +
      "\"start_time\":" + data.start_time.to_string() + "," +
      "\"duration\":" + data.duration.to_string() + "," +
      "\"status\":\"" + data.status + "\"," +
      "\"tags\":" + "[" + data.tags.map(fn(t) { "{\"key\":\"" + t.0 + "\",\"value\":\"" + t.1 + "\"}" }).join(",") + "]," +
      "\"logs\":" + "[" + data.logs.map(fn(l) { "{\"timestamp\":" + l.0.to_string() + ",\"message\":\"" + l.1 + "\"}" }).join(",") + "]" +
    "}"
    
    let bytes = json_str.to_bytes()
    {
      data: bytes,
      format: SerializationFormat::Json,
      size: bytes.length()
    }
  }
  
  // JSON反序列化
  let deserialize_from_json = fn(result: SerializationResult) {
    match result.format {
      SerializationFormat::Json => {
        let json_str = result.data.to_string()
        
        // 简化的JSON解析（实际实现会更复杂）
        let trace_id_start = json_str.index_of("\"trace_id\":\"") + 12
        let trace_id_end = json_str.index_of("\"", trace_id_start)
        let trace_id = json_str.substring(trace_id_start, trace_id_end - trace_id_start)
        
        let span_id_start = json_str.index_of("\"span_id\":\"") + 11
        let span_id_end = json_str.index_of("\"", span_id_start)
        let span_id = json_str.substring(span_id_start, span_id_end - span_id_start)
        
        let operation_name_start = json_str.index_of("\"operation_name\":\"") + 18
        let operation_name_end = json_str.index_of("\"", operation_name_start)
        let operation_name = json_str.substring(operation_name_start, operation_name_end - operation_name_start)
        
        let start_time_start = json_str.index_of("\"start_time\":") + 13
        let start_time_end = json_str.index_of(",", start_time_start)
        let start_time = json_str.substring(start_time_start, start_time_end - start_time_start).to_int()
        
        let duration_start = json_str.index_of("\"duration\":") + 11
        let duration_end = json_str.index_of(",", duration_start)
        let duration = json_str.substring(duration_start, duration_end - duration_start).to_int()
        
        let status_start = json_str.index_of("\"status\":\"") + 10
        let status_end = json_str.index_of("\"", status_start)
        let status = json_str.substring(status_start, status_end - status_start)
        
        Some({
          trace_id,
          span_id,
          parent_span_id: None,  // 简化实现
          operation_name,
          start_time,
          duration,
          status,
          tags: [],  // 简化实现
          logs: []   // 简化实现
        })
      }
      _ => None
    }
  }
  
  // 压缩数据
  let compress_data = fn(result: SerializationResult) {
    // 简化的压缩实现（实际实现会更复杂）
    let compressed_size = (result.size as Float * 0.7) as Int  // 假设压缩率为70%
    let compressed_data = Array::create(compressed_size, 0 as Byte)  // 简化实现
    
    {
      data: compressed_data,
      format: result.format,
      size: compressed_size
    }
  }
  
  // 创建测试数据
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-parent"),
    operation_name: "http.request",
    start_time: 1640995200000,  // 2022-01-01 00:00:00 UTC
    duration: 150,  // 150ms
    status: "OK",
    tags: [("http.method", "GET"), ("http.status_code", "200")],
    logs: [(1640995200000, "Request started"), (1640995200150, "Request completed")]
  }
  
  // 测试序列化
  let serialized = serialize_to_json(telemetry_data)
  assert_eq(serialized.format, SerializationFormat::Json)
  assert_true(serialized.size > 0)
  
  // 测试反序列化
  let deserialized = deserialize_from_json(serialized)
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(data) => {
      assert_eq(data.trace_id, telemetry_data.trace_id)
      assert_eq(data.span_id, telemetry_data.span_id)
      assert_eq(data.operation_name, telemetry_data.operation_name)
      assert_eq(data.start_time, telemetry_data.start_time)
      assert_eq(data.duration, telemetry_data.duration)
      assert_eq(data.status, telemetry_data.status)
    }
    None => assert_true(false)
  }
  
  // 测试压缩
  let compressed = compress_data(serialized)
  assert_eq(compressed.format, SerializationFormat::Json)
  assert_true(compressed.size < serialized.size)  // 压缩后应该更小
  
  // 计算压缩率
  let compression_ratio = (compressed.size as Float) / (serialized.size as Float)
  assert_true(compression_ratio < 1.0)  // 压缩率应该小于1
  assert_true(compression_ratio > 0.5)  // 压缩率不应该太低
}

// 测试5: 高效缓存机制
test "高效缓存机制" {
  // 定义缓存项
  type CacheItem[T] = {
    value: T,
    timestamp: Int,
    access_count: Int,
    ttl: Int  // 生存时间（毫秒）
  }
  
  // 定义缓存
  type Cache[T] = {
    items: Map[String, CacheItem[T]],
    max_size: Int,
    eviction_policy: String  // "LRU", "LFU", "TTL"
  }
  
  // 创建缓存
  let create_cache = fn(max_size: Int, eviction_policy: String) {
    {
      items: Map::empty(),
      max_size,
      eviction_policy
    }
  }
  
  // 获取当前时间
  let current_time = fn() { Time::now() }
  
  // 检查项是否过期
  let is_expired = fn(item: CacheItem[T]) {
    let now = current_time()
    now - item.timestamp > item.ttl
  }
  
  // 获取缓存项
  let get = fn(cache: Cache[T], key: String) {
    match Map::get(cache.items, key) {
      Some(item) => {
        if is_expired(item) {
          // 项已过期，移除并返回None
          let _ = Map::remove(cache.items, key)
          None
        } else {
          // 更新访问计数和时间戳
          let updated_item = { item with 
            access_count: item.access_count + 1,
            timestamp: current_time()
          }
          let _ = Map::insert(cache.items, key, updated_item)
          Some(updated_item.value)
        }
      }
      None => None
    }
  }
  
  // 设置缓存项
  let set = fn(cache: Cache[T], key: String, value: T, ttl: Int) {
    let now = current_time()
    let item = {
      value,
      timestamp: now,
      access_count: 1,
      ttl
    }
    
    // 检查是否需要驱逐项
    if Map::size(cache.items) >= cache.max_size {
      let evicted_cache = evict_items(cache)
      let _ = Map::insert(evicted_cache.items, key, item)
      evicted_cache
    } else {
      let _ = Map::insert(cache.items, key, item)
      cache
    }
  }
  
  // 驱逐缓存项
  let evict_items = fn(cache: Cache[T]) {
    match cache.eviction_policy {
      "LRU" => {
        // 最近最少使用
        let oldest_key = Map::keys(cache.items).reduce(fn(oldest, key) {
          match Map::get(cache.items, key) {
            Some(item) => {
              match Map::get(cache.items, oldest) {
                Some(oldest_item) => {
                  if item.timestamp < oldest_item.timestamp { key } else { oldest }
                }
                None => key
              }
            }
            None => oldest
          }
        }, Map::keys(cache.items)[0])
        
        let _ = Map::remove(cache.items, oldest_key)
        cache
      }
      "LFU" => {
        // 最少使用频率
        let least_used_key = Map::keys(cache.items).reduce(fn(least, key) {
          match Map::get(cache.items, key) {
            Some(item) => {
              match Map::get(cache.items, least) {
                Some(least_item) => {
                  if item.access_count < least_item.access_count { key } else { least }
                }
                None => key
              }
            }
            None => least
          }
        }, Map::keys(cache.items)[0])
        
        let _ = Map::remove(cache.items, least_used_key)
        cache
      }
      "TTL" => {
        // 基于TTL，移除所有过期项
        let expired_keys = Map::keys(cache.items).filter(fn(key) {
          match Map::get(cache.items, key) {
            Some(item) => is_expired(item)
            None => false
          }
        })
        
        let mut updated_cache = cache
        for key in expired_keys {
          let _ = Map::remove(updated_cache.items, key)
        }
        updated_cache
      }
      _ => cache
    }
  }
  
  // 清理过期项
  let cleanup_expired = fn(cache: Cache[T]) {
    let expired_keys = Map::keys(cache.items).filter(fn(key) {
      match Map::get(cache.items, key) {
        Some(item) => is_expired(item)
        None => false
      }
    })
    
    let mut updated_cache = cache
    for key in expired_keys {
      let _ = Map::remove(updated_cache.items, key)
    }
    updated_cache
  }
  
  // 测试LRU缓存
  let lru_cache = create_cache(3, "LRU")
  
  // 添加项
  let cache1 = set(lru_cache, "key1", "value1", 5000)  // 5秒TTL
  let cache2 = set(cache1, "key2", "value2", 5000)
  let cache3 = set(cache2, "key3", "value3", 5000)
  
  // 获取项（更新访问时间）
  let _ = get(cache3, "key1")  // 使key1成为最近使用的
  
  // 添加第4项，应该驱逐key2（最少最近使用的）
  let cache4 = set(cache3, "key4", "value4", 5000)
  
  // 验证key2被驱逐
  let value2 = get(cache4, "key2")
  assert_eq(value2, None)
  
  // 验证其他项仍然存在
  let value1 = get(cache4, "key1")
  let value3 = get(cache4, "key3")
  let value4 = get(cache4, "key4")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value3, Some("value3"))
  assert_eq(value4, Some("value4"))
  
  // 测试TTL过期
  let ttl_cache = create_cache(5, "TTL")
  
  // 添加一个短TTL的项
  let cache5 = set(ttl_cache, "short_lived", "will_expire", 100)  // 100ms TTL
  
  // 模拟时间流逝（在实际实现中，这里可能需要使用可控制的时间）
  let _ = Time::sleep(150)  // 等待150ms
  
  // 清理过期项
  let cache6 = cleanup_expired(cache5)
  
  // 验证过期项被移除
  let expired_value = get(cache6, "short_lived")
  assert_eq(expired_value, None)
}

// 测试6: 并发遥测数据处理
test "并发遥测数据处理" {
  // 定义任务状态
  enum TaskStatus {
    Pending
    Running
    Completed
    Failed
  }
  
  // 定义并发任务
  type ConcurrentTask = {
    id: String,
    status: TaskStatus,
    start_time: Int,
    end_time: Option[Int],
    result: Option[String],
    error: Option[String]
  }
  
  // 定义任务队列
  type TaskQueue = {
    tasks: Array[ConcurrentTask],
    max_concurrent: Int,
    running_tasks: Int
  }
  
  // 创建任务队列
  let create_task_queue = fn(max_concurrent: Int) {
    {
      tasks: [],
      max_concurrent,
      running_tasks: 0
    }
  }
  
  // 添加任务
  let add_task = fn(queue: TaskQueue, task_id: String) {
    let task = {
      id: task_id,
      status: TaskStatus::Pending,
      start_time: 0,
      end_time: None,
      result: None,
      error: None
    }
    
    let updated_tasks = queue.tasks.push(task)
    { queue with tasks: updated_tasks }
  }
  
  // 启动任务
  let start_task = fn(queue: TaskQueue, task_id: String) {
    let updated_tasks = queue.tasks.map(fn(task) {
      if task.id == task_id && task.status == TaskStatus::Pending {
        { task with 
          status: TaskStatus::Running,
          start_time: Time::now()
        }
      } else {
        task
      }
    })
    
    { queue with 
      tasks: updated_tasks,
      running_tasks: queue.running_tasks + 1
    }
  }
  
  // 完成任务
  let complete_task = fn(queue: TaskQueue, task_id: String, result: String) {
    let updated_tasks = queue.tasks.map(fn(task) {
      if task.id == task_id && task.status == TaskStatus::Running {
        { task with 
          status: TaskStatus::Completed,
          end_time: Some(Time::now()),
          result: Some(result)
        }
      } else {
        task
      }
    })
    
    { queue with 
      tasks: updated_tasks,
      running_tasks: queue.running_tasks - 1
    }
  }
  
  // 任务失败
  let fail_task = fn(queue: TaskQueue, task_id: String, error: String) {
    let updated_tasks = queue.tasks.map(fn(task) {
      if task.id == task_id && task.status == TaskStatus::Running {
        { task with 
          status: TaskStatus::Failed,
          end_time: Some(Time::now()),
          error: Some(error)
        }
      } else {
        task
      }
    })
    
    { queue with 
      tasks: updated_tasks,
      running_tasks: queue.running_tasks - 1
    }
  }
  
  // 获取可运行的任务
  let get_runnable_tasks = fn(queue: TaskQueue) {
    if queue.running_tasks < queue.max_concurrent {
      queue.tasks.filter(fn(task) { task.status == TaskStatus::Pending })
    } else {
      []
    }
  }
  
  // 获取任务统计
  let get_task_stats = fn(queue: TaskQueue) {
    let pending = queue.tasks.filter(fn(t) { t.status == TaskStatus::Pending }).length()
    let running = queue.tasks.filter(fn(t) { t.status == TaskStatus::Running }).length()
    let completed = queue.tasks.filter(fn(t) { t.status == TaskStatus::Completed }).length()
    let failed = queue.tasks.filter(fn(t) { t.status == TaskStatus::Failed }).length()
    
    {
      total: queue.tasks.length(),
      pending,
      running,
      completed,
      failed,
      running_tasks: queue.running_tasks
    }
  }
  
  // 创建任务队列
  let queue = create_task_queue(2)  // 最大并发2个任务
  
  // 添加任务
  let queue1 = add_task(queue, "task1")
  let queue2 = add_task(queue1, "task2")
  let queue3 = add_task(queue2, "task3")
  let queue4 = add_task(queue3, "task4")
  
  // 检查初始状态
  let initial_stats = get_task_stats(queue4)
  assert_eq(initial_stats.total, 4)
  assert_eq(initial_stats.pending, 4)
  assert_eq(initial_stats.running, 0)
  assert_eq(initial_stats.completed, 0)
  assert_eq(initial_stats.failed, 0)
  
  // 获取可运行的任务
  let runnable_tasks = get_runnable_tasks(queue4)
  assert_eq(runnable_tasks.length(), 2)  // 最多可以运行2个任务
  
  // 启动前两个任务
  let queue5 = start_task(queue4, "task1")
  let queue6 = start_task(queue5, "task2")
  
  // 检查状态
  let running_stats = get_task_stats(queue6)
  assert_eq(running_stats.pending, 2)
  assert_eq(running_stats.running, 2)
  assert_eq(running_stats.running_tasks, 2)
  
  // 尝试启动第三个任务（应该失败，因为已达到最大并发数）
  let runnable_tasks2 = get_runnable_tasks(queue6)
  assert_eq(runnable_tasks2.length(), 0)  // 没有可运行的任务
  
  // 完成第一个任务
  let queue7 = complete_task(queue6, "task1", "task1 result")
  
  // 检查状态
  let completed_stats = get_task_stats(queue7)
  assert_eq(completed_stats.pending, 2)
  assert_eq(completed_stats.running, 1)
  assert_eq(completed_stats.completed, 1)
  assert_eq(completed_stats.running_tasks, 1)
  
  // 现在应该可以运行第三个任务
  let runnable_tasks3 = get_runnable_tasks(queue7)
  assert_eq(runnable_tasks3.length(), 1)  // 有一个可运行的任务
  
  // 启动第三个任务
  let queue8 = start_task(queue7, "task3")
  
  // 第二个任务失败
  let queue9 = fail_task(queue8, "task2", "network error")
  
  // 检查状态
  let failed_stats = get_task_stats(queue9)
  assert_eq(failed_stats.pending, 1)
  assert_eq(failed_stats.running, 1)
  assert_eq(failed_stats.completed, 1)
  assert_eq(failed_stats.failed, 1)
  
  // 完成第三个任务
  let queue10 = complete_task(queue9, "task3", "task3 result")
  
  // 启动并完成第四个任务
  let queue11 = start_task(queue10, "task4")
  let queue12 = complete_task(queue11, "task4", "task4 result")
  
  // 检查最终状态
  let final_stats = get_task_stats(queue12)
  assert_eq(final_stats.total, 4)
  assert_eq(final_stats.pending, 0)
  assert_eq(final_stats.running, 0)
  assert_eq(final_stats.completed, 3)
  assert_eq(final_stats.failed, 1)
  assert_eq(final_stats.running_tasks, 0)
}

// 测试7: 动态配置管理
test "动态配置管理" {
  // 定义配置值类型
  enum ConfigValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[String])
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    is_sensitive: Bool,
    last_modified: Int
  }
  
  // 定义配置变更监听器
  type ConfigChangeListener = (String, ConfigValue, ConfigValue) -> Unit  // (key, old_value, new_value)
  
  // 定义配置管理器
  type ConfigManager = {
    configs: Map[String, ConfigItem],
    listeners: Array[ConfigChangeListener],
    config_file: String
  }
  
  // 创建配置管理器
  let create_config_manager = fn(config_file: String) {
    {
      configs: Map::empty(),
      listeners: [],
      config_file
    }
  }
  
  // 添加配置项
  let add_config = fn(manager: ConfigManager, key: String, value: ConfigValue, default_value: ConfigValue, description: String, is_sensitive: Bool) {
    let item = {
      key,
      value,
      default_value,
      description,
      is_sensitive,
      last_modified: Time::now()
    }
    
    let updated_configs = Map::insert(manager.configs, key, item)
    { manager with configs: updated_configs }
  }
  
  // 获取配置值
  let get_config = fn(manager: ConfigManager, key: String) {
    match Map::get(manager.configs, key) {
      Some(item) => Some(item.value)
      None => None
    }
  }
  
  // 更新配置值
  let update_config = fn(manager: ConfigManager, key: String, new_value: ConfigValue) {
    match Map::get(manager.configs, key) {
      Some(item) => {
        let old_value = item.value
        let updated_item = { item with 
          value: new_value,
          last_modified: Time::now()
        }
        
        let updated_configs = Map::insert(manager.configs, key, updated_item)
        let updated_manager = { manager with configs: updated_configs }
        
        // 通知监听器
        for listener in manager.listeners {
          listener(key, old_value, new_value)
        }
        
        updated_manager
      }
      None => manager
    }
  }
  
  // 重置配置为默认值
  let reset_config = fn(manager: ConfigManager, key: String) {
    match Map::get(manager.configs, key) {
      Some(item) => {
        update_config(manager, key, item.default_value)
      }
      None => manager
    }
  }
  
  // 添加配置变更监听器
  let add_listener = fn(manager: ConfigManager, listener: ConfigChangeListener) {
    let updated_listeners = manager.listeners.push(listener)
    { manager with listeners: updated_listeners }
  }
  
  // 验证配置值
  let validate_config = fn(value: ConfigValue, expected_type: String) {
    match (value, expected_type) {
      (ConfigValue::StringValue(_), "string") => true
      (ConfigValue::IntValue(_), "int") => true
      (ConfigValue::FloatValue(_), "float") => true
      (ConfigValue::BoolValue(_), "bool") => true
      (ConfigValue::ArrayValue(_), "array") => true
      _ => false
    }
  }
  
  // 创建配置管理器
  let manager = create_config_manager("telemetry.conf")
  
  // 添加配置项
  let manager1 = add_config(manager, "service.name", ConfigValue::StringValue("telemetry-service"), ConfigValue::StringValue("telemetry-service"), "服务名称", false)
  let manager2 = add_config(manager1, "service.port", ConfigValue::IntValue(8080), ConfigValue::IntValue(8080), "服务端口", false)
  let manager3 = add_config(manager2, "sampling.rate", ConfigValue::FloatValue(0.1), ConfigValue::FloatValue(0.1), "采样率", false)
  let manager4 = add_config(manager3, "debug.enabled", ConfigValue::BoolValue(false), ConfigValue::BoolValue(false), "调试模式", false)
  let manager5 = add_config(manager4, "export.endpoints", ConfigValue::ArrayValue(["http://localhost:4317"]), ConfigValue::ArrayValue(["http://localhost:4317"]), "导出端点", false)
  let manager6 = add_config(manager5, "database.password", ConfigValue::StringValue("secret"), ConfigValue::StringValue("secret"), "数据库密码", true)
  
  // 测试获取配置
  let service_name = get_config(manager6, "service.name")
  match service_name {
    Some(ConfigValue::StringValue(name)) => assert_eq(name, "telemetry-service")
    _ => assert_true(false)
  }
  
  let service_port = get_config(manager6, "service.port")
  match service_port {
    Some(ConfigValue::IntValue(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  let sampling_rate = get_config(manager6, "sampling.rate")
  match sampling_rate {
    Some(ConfigValue::FloatValue(rate)) => assert_eq(rate, 0.1)
    _ => assert_true(false)
  }
  
  let debug_enabled = get_config(manager6, "debug.enabled")
  match debug_enabled {
    Some(ConfigValue::BoolValue(enabled)) => assert_eq(enabled, false)
    _ => assert_true(false)
  }
  
  let export_endpoints = get_config(manager6, "export.endpoints")
  match export_endpoints {
    Some(ConfigValue::ArrayValue(endpoints)) => assert_eq(endpoints, ["http://localhost:4317"])
    _ => assert_true(false)
  }
  
  // 测试更新配置
  let manager7 = update_config(manager6, "service.port", ConfigValue::IntValue(9090))
  let updated_port = get_config(manager7, "service.port")
  match updated_port {
    Some(ConfigValue::IntValue(port)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  // 测试配置验证
  assert_true(validate_config(ConfigValue::StringValue("test"), "string"))
  assert_true(validate_config(ConfigValue::IntValue(42), "int"))
  assert_true(validate_config(ConfigValue::FloatValue(3.14), "float"))
  assert_true(validate_config(ConfigValue::BoolValue(true), "bool"))
  assert_true(validate_config(ConfigValue::ArrayValue(["a", "b"]), "array"))
  
  assert_false(validate_config(ConfigValue::StringValue("test"), "int"))
  assert_false(validate_config(ConfigValue::IntValue(42), "string"))
  
  // 测试重置配置
  let manager8 = reset_config(manager7, "service.port")
  let reset_port = get_config(manager8, "service.port")
  match reset_port {
    Some(ConfigValue::IntValue(port)) => assert_eq(port, 8080)  // 应该重置为默认值
    _ => assert_true(false)
  }
  
  // 测试配置变更监听器
  let mut notified = false
  let listener = fn(key: String, old_value: ConfigValue, new_value: ConfigValue) {
    if key == "service.name" {
      match (old_value, new_value) {
        (ConfigValue::StringValue(old), ConfigValue::StringValue(new)) => {
          if old == "telemetry-service" && new == "updated-service" {
            notified = true
          }
        }
        _ => {}
      }
    }
  }
  
  let manager9 = add_listener(manager8, listener)
  let manager10 = update_config(manager9, "service.name", ConfigValue::StringValue("updated-service"))
  
  assert_true(notified)  // 监听器应该被调用
}

// 测试8: 高级错误处理和恢复
test "高级错误处理和恢复" {
  // 定义错误类型
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    ValidationError(String)
    ResourceExhaustedError(String)
    TimeoutError(String)
    UnknownError(String)
  }
  
  // 定义错误严重程度
  enum ErrorSeverity {
    Low
    Medium
    High
    Critical
  }
  
  // 定义错误上下文
  type ErrorContext = {
    error: TelemetryError,
    severity: ErrorSeverity,
    timestamp: Int,
    component: String,
    operation: String,
    retry_count: Int,
    metadata: Array[(String, String)]
  }
  
  // 定义恢复策略
  enum RecoveryStrategy {
    NoRetry
    FixedDelay(Int)  // Int表示延迟毫秒数
    ExponentialBackoff(Int, Float)  // Int表示初始延迟，Float表示倍数
    CircuitBreaker(Int, Int)  // Int, Int表示失败阈值和恢复超时
    FailFast
  }
  
  // 定义错误处理器
  type ErrorHandler = {
    errors: Array[ErrorContext],
    strategies: Map[String, RecoveryStrategy],
    circuit_breaker_states: Map[String, (Bool, Int)]  // (is_open, last_failure_time)
  }
  
  // 创建错误处理器
  let create_error_handler = fn() {
    {
      errors: [],
      strategies: Map::empty(),
      circuit_breaker_states: Map::empty()
    }
  }
  
  // 记录错误
  let record_error = fn(handler: ErrorHandler, error: TelemetryError, severity: ErrorSeverity, component: String, operation: String, metadata: Array[(String, String)]) {
    let context = {
      error,
      severity,
      timestamp: Time::now(),
      component,
      operation,
      retry_count: 0,
      metadata
    }
    
    let updated_errors = handler.errors.push(context)
    { handler with errors: updated_errors }
  }
  
  // 设置恢复策略
  let set_recovery_strategy = fn(handler: ErrorHandler, error_type: String, strategy: RecoveryStrategy) {
    let updated_strategies = Map::insert(handler.strategies, error_type, strategy)
    { handler with strategies: updated_strategies }
  }
  
  // 获取错误统计
  let get_error_stats = fn(handler: ErrorHandler, time_window: Int) {
    let now = Time::now()
    let recent_errors = handler.errors.filter(fn(ctx) { now - ctx.timestamp <= time_window })
    
    let by_severity = fn(severity: ErrorSeverity) {
      recent_errors.filter(fn(ctx) { ctx.severity == severity }).length()
    }
    
    let by_component = fn(component: String) {
      recent_errors.filter(fn(ctx) { ctx.component == component }).length()
    }
    
    let by_error_type = fn(error_type: String) {
      recent_errors.filter(fn(ctx) {
        match ctx.error {
          TelemetryError::NetworkError(_) => error_type == "NetworkError"
          TelemetryError::SerializationError(_) => error_type == "SerializationError"
          TelemetryError::ValidationError(_) => error_type == "ValidationError"
          TelemetryError::ResourceExhaustedError(_) => error_type == "ResourceExhaustedError"
          TelemetryError::TimeoutError(_) => error_type == "TimeoutError"
          TelemetryError::UnknownError(_) => error_type == "UnknownError"
        }
      }).length()
    }
    
    {
      total: recent_errors.length(),
      low_severity: by_severity(ErrorSeverity::Low),
      medium_severity: by_severity(ErrorSeverity::Medium),
      high_severity: by_severity(ErrorSeverity::High),
      critical_severity: by_severity(ErrorSeverity::Critical),
      network_errors: by_error_type("NetworkError"),
      serialization_errors: by_error_type("SerializationError"),
      validation_errors: by_error_type("ValidationError"),
      resource_exhausted_errors: by_error_type("ResourceExhaustedError"),
      timeout_errors: by_error_type("TimeoutError"),
      unknown_errors: by_error_type("UnknownError")
    }
  }
  
  // 处理错误并决定恢复策略
  let handle_error = fn(handler: ErrorHandler, error: TelemetryError, component: String, operation: String) {
    let error_type = match error {
      TelemetryError::NetworkError(_) => "NetworkError"
      TelemetryError::SerializationError(_) => "SerializationError"
      TelemetryError::ValidationError(_) => "ValidationError"
      TelemetryError::ResourceExhaustedError(_) => "ResourceExhaustedError"
      TelemetryError::TimeoutError(_) => "TimeoutError"
      TelemetryError::UnknownError(_) => "UnknownError"
    }
    
    let strategy = match Map::get(handler.strategies, error_type) {
      Some(s) => s
      None => RecoveryStrategy::NoRetry
    }
    
    // 检查断路器状态
    let circuit_breaker_state = match Map::get(handler.circuit_breaker_states, component) {
      Some(state) => state
      None => (false, 0)  // (is_open, last_failure_time)
    }
    
    let (is_open, last_failure_time) = circuit_breaker_state
    let now = Time::now()
    
    if is_open {
      // 检查是否可以尝试恢复
      match strategy {
        RecoveryStrategy::CircuitBreaker(threshold, timeout) => {
          if now - last_failure_time > timeout {
            // 断路器半开状态，允许一次尝试
            let updated_states = Map::insert(handler.circuit_breaker_states, component, (false, now))
            let updated_handler = { handler with circuit_breaker_states: updated_states }
            (updated_handler, "half_open_retry")
          } else {
            // 断路器仍然打开
            (handler, "circuit_breaker_open")
          }
        }
        _ => (handler, "no_retry")
      }
    } else {
      // 断路器关闭，正常处理
      match strategy {
        RecoveryStrategy::NoRetry => (handler, "no_retry")
        RecoveryStrategy::FixedDelay(delay) => (handler, "retry_with_delay:" + delay.to_string())
        RecoveryStrategy::ExponentialBackoff(initial_delay, multiplier) => (handler, "retry_with_exponential_backoff:" + initial_delay.to_string() + ":" + multiplier.to_string())
        RecoveryStrategy::CircuitBreaker(threshold, timeout) => {
          // 检查是否需要打开断路器
          let recent_errors = handler.errors.filter(fn(ctx) { 
            ctx.component == component && now - ctx.timestamp <= 60000  // 最近1分钟
          })
          
          if recent_errors.length() >= threshold {
            // 打开断路器
            let updated_states = Map::insert(handler.circuit_breaker_states, component, (true, now))
            let updated_handler = { handler with circuit_breaker_states: updated_states }
            (updated_handler, "circuit_breaker_opened")
          } else {
            (handler, "retry_allowed")
          }
        }
        RecoveryStrategy::FailFast => (handler, "fail_fast")
      }
    }
  }
  
  // 创建错误处理器
  let handler = create_error_handler()
  
  // 设置恢复策略
  let handler1 = set_recovery_strategy(handler, "NetworkError", RecoveryStrategy::ExponentialBackoff(1000, 2.0))
  let handler2 = set_recovery_strategy(handler1, "TimeoutError", RecoveryStrategy::FixedDelay(5000))
  let handler3 = set_recovery_strategy(handler2, "ResourceExhaustedError", RecoveryStrategy::CircuitBreaker(5, 60000))
  let handler4 = set_recovery_strategy(handler3, "ValidationError", RecoveryStrategy::FailFast)
  
  // 记录一些错误
  let handler5 = record_error(handler4, TelemetryError::NetworkError("连接超时"), ErrorSeverity::Medium, "http-client", "send_request", [("url", "http://example.com")])
  let handler6 = record_error(handler5, TelemetryError::TimeoutError("请求超时"), ErrorSeverity::High, "database", "execute_query", [("query", "SELECT * FROM users")])
  let handler7 = record_error(handler6, TelemetryError::ValidationError("无效参数"), ErrorSeverity::Low, "api-server", "validate_request", [("parameter", "user_id")])
  let handler8 = record_error(handler7, TelemetryError::ResourceExhaustedError("内存不足"), ErrorSeverity::Critical, "data-processor", "process_data", [])
  
  // 测试错误统计
  let stats = get_error_stats(handler8, 60000)  // 最近1分钟
  assert_eq(stats.total, 4)
  assert_eq(stats.low_severity, 1)
  assert_eq(stats.medium_severity, 1)
  assert_eq(stats.high_severity, 1)
  assert_eq(stats.critical_severity, 1)
  assert_eq(stats.network_errors, 1)
  assert_eq(stats.timeout_errors, 1)
  assert_eq(stats.validation_errors, 1)
  assert_eq(stats.resource_exhausted_errors, 1)
  
  // 测试错误处理
  let (handler9, result1) = handle_error(handler8, TelemetryError::NetworkError("连接拒绝"), "http-client", "send_request")
  assert_eq(result1, "retry_with_exponential_backoff:1000:2.0")
  
  let (handler10, result2) = handle_error(handler9, TelemetryError::TimeoutError("查询超时"), "database", "execute_query")
  assert_eq(result2, "retry_with_delay:5000")
  
  let (handler11, result3) = handle_error(handler10, TelemetryError::ValidationError("缺少必需参数"), "api-server", "validate_request")
  assert_eq(result3, "fail_fast")
  
  // 测试断路器
  let (handler12, result4) = handle_error(handler11, TelemetryError::ResourceExhaustedError("CPU使用率过高"), "data-processor", "process_data")
  assert_eq(result4, "retry_allowed")  // 还未达到阈值
  
  // 记录更多错误以触发断路器
  let handler13 = record_error(handler12, TelemetryError::ResourceExhaustedError("磁盘空间不足"), ErrorSeverity::High, "data-processor", "process_data", [])
  let handler14 = record_error(handler13, TelemetryError::ResourceExhaustedError("连接池耗尽"), ErrorSeverity::High, "data-processor", "process_data", [])
  let handler15 = record_error(handler14, TelemetryError::ResourceExhaustedError("线程池耗尽"), ErrorSeverity::High, "data-processor", "process_data", [])
  let handler16 = record_error(handler15, TelemetryError::ResourceExhaustedError("文件描述符耗尽"), ErrorSeverity::High, "data-processor", "process_data", [])
  
  // 现在应该触发断路器
  let (handler17, result5) = handle_error(handler16, TelemetryError::ResourceExhaustedError("内存不足"), "data-processor", "process_data")
  assert_eq(result5, "circuit_breaker_opened")
  
  // 再次尝试应该被断路器阻止
  let (handler18, result6) = handle_error(handler17, TelemetryError::ResourceExhaustedError("内存不足"), "data-processor", "process_data")
  assert_eq(result6, "circuit_breaker_open")
}

// 测试9: 资源管理和监控
test "资源管理和监控" {
  // 定义资源类型
  enum ResourceType {
    Memory
    CPU
    Disk
    Network
    FileHandle
    DatabaseConnection
  }
  
  // 定义资源使用情况
  type ResourceUsage = {
    resource_type: ResourceType,
    used: Float,
    total: Float,
    unit: String,
    timestamp: Int
  }
  
  // 定义资源阈值
  type ResourceThreshold = {
    warning_threshold: Float,  // 警告阈值（百分比）
    critical_threshold: Float,  // 严重阈值（百分比）
    check_interval: Int  // 检查间隔（毫秒）
  }
  
  // 定义资源监控器
  type ResourceMonitor = {
    usages: Array[ResourceUsage],
    thresholds: Map[ResourceType, ResourceThreshold],
    alerts: Array[ResourceAlert]
  }
  
  // 定义资源警报
  type ResourceAlert = {
    resource_type: ResourceType,
    level: String,  // "warning" 或 "critical"
    message: String,
    timestamp: Int,
    resolved: Bool
  }
  
  // 创建资源监控器
  let create_resource_monitor = fn() {
    {
      usages: [],
      thresholds: Map::empty(),
      alerts: []
    }
  }
  
  // 设置资源阈值
  let set_threshold = fn(monitor: ResourceMonitor, resource_type: ResourceType, warning_threshold: Float, critical_threshold: Float, check_interval: Int) {
    let threshold = {
      warning_threshold,
      critical_threshold,
      check_interval
    }
    
    let updated_thresholds = Map::insert(monitor.thresholds, resource_type, threshold)
    { monitor with thresholds: updated_thresholds }
  }
  
  // 记录资源使用情况
  let record_usage = fn(monitor: ResourceMonitor, resource_type: ResourceType, used: Float, total: Float, unit: String) {
    let usage = {
      resource_type,
      used,
      total,
      unit,
      timestamp: Time::now()
    }
    
    let updated_usages = monitor.usages.push(usage)
    let updated_monitor = { monitor with usages: updated_usages }
    
    // 检查是否需要生成警报
    check_resource_alerts(updated_monitor, usage)
  }
  
  // 检查资源警报
  let check_resource_alerts = fn(monitor: ResourceMonitor, usage: ResourceUsage) {
    match Map::get(monitor.thresholds, usage.resource_type) {
      Some(threshold) => {
        let usage_percentage = (usage.used / usage.total) * 100.0
        
        if usage_percentage >= threshold.critical_threshold {
          // 生成严重警报
          let alert = {
            resource_type: usage.resource_type,
            level: "critical",
            message: "资源使用率严重过高: " + usage_percentage.to_string() + "%",
            timestamp: Time::now(),
            resolved: false
          }
          
          let updated_alerts = monitor.alerts.push(alert)
          { monitor with alerts: updated_alerts }
        } else if usage_percentage >= threshold.warning_threshold {
          // 生成警告警报
          let alert = {
            resource_type: usage.resource_type,
            level: "warning",
            message: "资源使用率过高: " + usage_percentage.to_string() + "%",
            timestamp: Time::now(),
            resolved: false
          }
          
          let updated_alerts = monitor.alerts.push(alert)
          { monitor with alerts: updated_alerts }
        } else {
          monitor
        }
      }
      None => monitor
    }
  }
  
  // 获取当前资源使用情况
  let get_current_usage = fn(monitor: ResourceMonitor, resource_type: ResourceType) {
    let now = Time::now()
    let recent_usages = monitor.usages.filter(fn(u) { 
      u.resource_type == resource_type && now - u.timestamp <= 60000  // 最近1分钟
    })
    
    if recent_usages.length() > 0 {
      let latest = recent_usages.reduce(fn(latest, current) { 
        if current.timestamp > latest.timestamp { current } else { latest } 
      }, recent_usages[0])
      
      Some(latest)
    } else {
      None
    }
  }
  
  // 获取资源使用趋势
  let get_usage_trend = fn(monitor: ResourceMonitor, resource_type: ResourceType, time_window: Int) {
    let now = Time::now()
    let usages = monitor.usages.filter(fn(u) { 
      u.resource_type == resource_type && now - u.timestamp <= time_window 
    })
    
    if usages.length() < 2 {
      return "stable"  // 数据点不足，无法确定趋势
    }
    
    // 简单的趋势分析：比较最早和最新的使用率
    let sorted_usages = usages.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    let earliest = sorted_usages[0]
    let latest = sorted_usages[sorted_usages.length() - 1]
    
    let earliest_percentage = (earliest.used / earliest.total) * 100.0
    let latest_percentage = (latest.used / latest.total) * 100.0
    let change = latest_percentage - earliest_percentage
    
    if change > 10.0 {
      "increasing"
    } else if change < -10.0 {
      "decreasing"
    } else {
      "stable"
    }
  }
  
  // 解决警报
  let resolve_alerts = fn(monitor: ResourceMonitor, resource_type: ResourceType) {
    let updated_alerts = monitor.alerts.map(fn(alert) {
      if alert.resource_type == resource_type && not alert.resolved {
        { alert with resolved: true }
      } else {
        alert
      }
    })
    
    { monitor with alerts: updated_alerts }
  }
  
  // 创建资源监控器
  let monitor = create_resource_monitor()
  
  // 设置资源阈值
  let monitor1 = set_threshold(monitor, ResourceType::Memory, 80.0, 90.0, 5000)
  let monitor2 = set_threshold(monitor1, ResourceType::CPU, 70.0, 85.0, 3000)
  let monitor3 = set_threshold(monitor2, ResourceType::Disk, 85.0, 95.0, 10000)
  let monitor4 = set_threshold(monitor3, ResourceType::DatabaseConnection, 75.0, 90.0, 2000)
  
  // 记录正常资源使用情况
  let monitor5 = record_usage(monitor4, ResourceType::Memory, 4.0, 8.0, "GB")
  let monitor6 = record_usage(monitor5, ResourceType::CPU, 2.5, 4.0, "cores")
  let monitor7 = record_usage(monitor6, ResourceType::Disk, 250.0, 500.0, "GB")
  let monitor8 = record_usage(monitor7, ResourceType::DatabaseConnection, 15.0, 20.0, "connections")
  
  // 检查当前资源使用情况
  let memory_usage = get_current_usage(monitor8, ResourceType::Memory)
  match memory_usage {
    Some(usage) => {
      assert_eq(usage.used, 4.0)
      assert_eq(usage.total, 8.0)
      assert_eq(usage.unit, "GB")
    }
    None => assert_true(false)
  }
  
  // 记录高资源使用情况，触发警报
  let monitor9 = record_usage(monitor8, ResourceType::Memory, 7.2, 8.0, "GB")  // 90%使用率，应该触发严重警报
  let monitor10 = record_usage(monitor9, ResourceType::CPU, 3.0, 4.0, "cores")  // 75%使用率，应该触发警告警报
  
  // 检查警报
  assert_eq(monitor10.alerts.length(), 2)
  
  let memory_alert = monitor10.alerts.find(fn(alert) { alert.resource_type == ResourceType::Memory })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.level, "critical")
      assert_true(alert.message.contains("90"))
      assert_eq(alert.resolved, false)
    }
    None => assert_true(false)
  }
  
  let cpu_alert = monitor10.alerts.find(fn(alert) { alert.resource_type == ResourceType::CPU })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.level, "warning")
      assert_true(alert.message.contains("75"))
      assert_eq(alert.resolved, false)
    }
    None => assert_true(false)
  }
  
  // 记录更多资源使用情况以分析趋势
  let monitor11 = record_usage(monitor10, ResourceType::Memory, 6.0, 8.0, "GB")
  let monitor12 = record_usage(monitor11, ResourceType::Memory, 6.5, 8.0, "GB")
  let monitor13 = record_usage(monitor12, ResourceType::Memory, 7.0, 8.0, "GB")
  
  // 分析内存使用趋势
  let memory_trend = get_usage_trend(monitor13, ResourceType::Memory, 60000)  // 最近1分钟
  assert_eq(memory_trend, "increasing")
  
  // 解决内存警报
  let monitor14 = resolve_alerts(monitor13, ResourceType::Memory)
  
  // 验证警报已解决
  let resolved_memory_alert = monitor14.alerts.find(fn(alert) { alert.resource_type == ResourceType::Memory })
  match resolved_memory_alert {
    Some(alert) => assert_eq(alert.resolved, true)
    None => assert_true(false)
  }
  
  // CPU警报应该仍然未解决
  let unresolved_cpu_alert = monitor14.alerts.find(fn(alert) { alert.resource_type == ResourceType::CPU })
  match unresolved_cpu_alert {
    Some(alert) => assert_eq(alert.resolved, false)
    None => assert_true(false)
  }
}

// 测试10: 安全性和访问控制
test "安全性和访问控制" {
  // 定义权限类型
  enum Permission {
    Read
    Write
    Delete
    Admin
  }
  
  // 定义用户角色
  enum UserRole {
    Viewer
    Editor
    Operator
    Administrator
  }
  
  // 定义用户
  type User = {
    id: String,
    username: String,
    role: UserRole,
    permissions: Array[Permission],
    active: Bool
  }
  
  // 定义访问控制策略
  type AccessPolicy = {
    resource: String,
    required_permissions: Array[Permission],
    allowed_roles: Array[UserRole]
  }
  
  // 定义访问日志
  type AccessLog = {
    user_id: String,
    resource: String,
    action: String,
    granted: Bool,
    timestamp: Int,
    ip_address: String,
    user_agent: String
  }
  
  // 定义访问控制管理器
  type AccessControlManager = {
    users: Map[String, User],
    policies: Map[String, AccessPolicy],
    access_logs: Array[AccessLog]
  }
  
  // 创建访问控制管理器
  let create_access_control_manager = fn() {
    {
      users: Map::empty(),
      policies: Map::empty(),
      access_logs: []
    }
  }
  
  // 添加用户
  let add_user = fn(manager: AccessControlManager, user: User) {
    let updated_users = Map::insert(manager.users, user.id, user)
    { manager with users: updated_users }
  }
  
  // 添加访问策略
  let add_policy = fn(manager: AccessControlManager, resource: String, policy: AccessPolicy) {
    let updated_policies = Map::insert(manager.policies, resource, policy)
    { manager with policies: updated_policies }
  }
  
  // 检查用户权限
  let has_permission = fn(user: User, permission: Permission) {
    user.permissions.any(fn(p) { p == permission })
  }
  
  // 检查用户角色
  let has_role = fn(user: User, role: UserRole) {
    user.role == role
  }
  
  // 检查访问权限
  let check_access = fn(manager: AccessControlManager, user_id: String, resource: String, action: String, ip_address: String, user_agent: String) {
    match Map::get(manager.users, user_id) {
      Some(user) => {
        if not user.active {
          let log = {
            user_id,
            resource,
            action,
            granted: false,
            timestamp: Time::now(),
            ip_address,
            user_agent
          }
          
          let updated_logs = manager.access_logs.push(log)
          let updated_manager = { manager with access_logs: updated_logs }
          (updated_manager, false, "用户已禁用")
        } else {
          match Map::get(manager.policies, resource) {
            Some(policy) => {
              // 检查角色权限
              let role_allowed = policy.allowed_roles.any(fn(role) { has_role(user, role) })
              
              // 检查操作权限
              let required_permission = match action {
                "read" => Permission::Read
                "write" => Permission::Write
                "delete" => Permission::Delete
                "admin" => Permission::Admin
                _ => Permission::Read  // 默认为读权限
              }
              
              let permission_allowed = has_permission(user, required_permission)
              
              let granted = role_allowed && permission_allowed
              let reason = if not role_allowed {
                "角色不被允许"
              } else if not permission_allowed {
                "缺少必要权限"
              } else {
                "访问允许"
              }
              
              let log = {
                user_id,
                resource,
                action,
                granted,
                timestamp: Time::now(),
                ip_address,
                user_agent
              }
              
              let updated_logs = manager.access_logs.push(log)
              let updated_manager = { manager with access_logs: updated_logs }
              
              (updated_manager, granted, reason)
            }
            None => {
              let log = {
                user_id,
                resource,
                action,
                granted: false,
                timestamp: Time::now(),
                ip_address,
                user_agent
              }
              
              let updated_logs = manager.access_logs.push(log)
              let updated_manager = { manager with access_logs: updated_logs }
              
              (updated_manager, false, "资源不存在或无访问策略")
            }
          }
        }
      }
      None => {
        let log = {
          user_id,
          resource,
          action,
          granted: false,
          timestamp: Time::now(),
          ip_address,
          user_agent
        }
        
        let updated_logs = manager.access_logs.push(log)
        let updated_manager = { manager with access_logs: updated_logs }
        
        (updated_manager, false, "用户不存在")
      }
    }
  }
  
  // 获取用户访问日志
  let get_user_access_logs = fn(manager: AccessControlManager, user_id: String, time_window: Int) {
    let now = Time::now()
    manager.access_logs.filter(fn(log) { 
      log.user_id == user_id && now - log.timestamp <= time_window 
    })
  }
  
  // 获取资源访问日志
  let get_resource_access_logs = fn(manager: AccessControlManager, resource: String, time_window: Int) {
    let now = Time::now()
    manager.access_logs.filter(fn(log) { 
      log.resource == resource && now - log.timestamp <= time_window 
    })
  }
  
  // 创建访问控制管理器
  let manager = create_access_control_manager()
  
  // 创建用户
  let viewer_user = {
    id: "user1",
    username: "viewer",
    role: UserRole::Viewer,
    permissions: [Permission::Read],
    active: true
  }
  
  let editor_user = {
    id: "user2",
    username: "editor",
    role: UserRole::Editor,
    permissions: [Permission::Read, Permission::Write],
    active: true
  }
  
  let operator_user = {
    id: "user3",
    username: "operator",
    role: UserRole::Operator,
    permissions: [Permission::Read, Permission::Write, Permission::Delete],
    active: true
  }
  
  let admin_user = {
    id: "user4",
    username: "admin",
    role: UserRole::Administrator,
    permissions: [Permission::Read, Permission::Write, Permission::Delete, Permission::Admin],
    active: true
  }
  
  let inactive_user = {
    id: "user5",
    username: "inactive",
    role: UserRole::Viewer,
    permissions: [Permission::Read],
    active: false
  }
  
  // 添加用户
  let manager1 = add_user(manager, viewer_user)
  let manager2 = add_user(manager1, editor_user)
  let manager3 = add_user(manager2, operator_user)
  let manager4 = add_user(manager3, admin_user)
  let manager5 = add_user(manager4, inactive_user)
  
  // 创建访问策略
  let telemetry_read_policy = {
    resource: "telemetry.data",
    required_permissions: [Permission::Read],
    allowed_roles: [UserRole::Viewer, UserRole::Editor, UserRole::Operator, UserRole::Administrator]
  }
  
  let telemetry_write_policy = {
    resource: "telemetry.config",
    required_permissions: [Permission::Write],
    allowed_roles: [UserRole::Editor, UserRole::Operator, UserRole::Administrator]
  }
  
  let telemetry_delete_policy = {
    resource: "telemetry.data",
    required_permissions: [Permission::Delete],
    allowed_roles: [UserRole::Operator, UserRole::Administrator]
  }
  
  let system_admin_policy = {
    resource: "system.config",
    required_permissions: [Permission::Admin],
    allowed_roles: [UserRole::Administrator]
  }
  
  // 添加策略
  let manager6 = add_policy(manager5, "telemetry.data", telemetry_read_policy)
  let manager7 = add_policy(manager6, "telemetry.config", telemetry_write_policy)
  let manager8 = add_policy(manager7, "telemetry.data", telemetry_delete_policy)
  let manager9 = add_policy(manager8, "system.config", system_admin_policy)
  
  // 测试访问权限
  let (manager10, viewer_read_result, viewer_read_reason) = check_access(manager9, "user1", "telemetry.data", "read", "192.168.1.100", "Mozilla/5.0")
  assert_eq(viewer_read_result, true)
  assert_eq(viewer_read_reason, "访问允许")
  
  let (manager11, viewer_write_result, viewer_write_reason) = check_access(manager10, "user1", "telemetry.config", "write", "192.168.1.100", "Mozilla/5.0")
  assert_eq(viewer_write_result, false)
  assert_eq(viewer_write_reason, "角色不被允许")
  
  let (manager12, editor_read_result, editor_read_reason) = check_access(manager11, "user2", "telemetry.data", "read", "192.168.1.101", "Mozilla/5.0")
  assert_eq(editor_read_result, true)
  assert_eq(editor_read_reason, "访问允许")
  
  let (manager13, editor_write_result, editor_write_reason) = check_access(manager12, "user2", "telemetry.config", "write", "192.168.1.101", "Mozilla/5.0")
  assert_eq(editor_write_result, true)
  assert_eq(editor_write_reason, "访问允许")
  
  let (manager14, editor_delete_result, editor_delete_reason) = check_access(manager13, "user2", "telemetry.data", "delete", "192.168.1.101", "Mozilla/5.0")
  assert_eq(editor_delete_result, false)
  assert_eq(editor_delete_reason, "缺少必要权限")
  
  let (manager15, operator_delete_result, operator_delete_reason) = check_access(manager14, "user3", "telemetry.data", "delete", "192.168.1.102", "Mozilla/5.0")
  assert_eq(operator_delete_result, true)
  assert_eq(operator_delete_reason, "访问允许")
  
  let (manager16, admin_system_result, admin_system_reason) = check_access(manager15, "user4", "system.config", "admin", "192.168.1.103", "Mozilla/5.0")
  assert_eq(admin_system_result, true)
  assert_eq(admin_system_reason, "访问允许")
  
  let (manager17, inactive_result, inactive_reason) = check_access(manager16, "user5", "telemetry.data", "read", "192.168.1.104", "Mozilla/5.0")
  assert_eq(inactive_result, false)
  assert_eq(inactive_reason, "用户已禁用")
  
  let (manager18, nonexistent_result, nonexistent_reason) = check_access(manager17, "user6", "telemetry.data", "read", "192.168.1.105", "Mozilla/5.0")
  assert_eq(nonexistent_result, false)
  assert_eq(nonexistent_reason, "用户不存在")
  
  // 测试访问日志
  let viewer_logs = get_user_access_logs(manager18, "user1", 60000)  // 最近1分钟
  assert_eq(viewer_logs.length(), 2)  // 两次访问尝试
  
  let telemetry_data_logs = get_resource_access_logs(manager18, "telemetry.data", 60000)  // 最近1分钟
  assert_eq(telemetry_data_logs.length(), 5)  // 5次对telemetry.data的访问尝试
  
  // 验证日志内容
  let viewer_read_log = viewer_logs.find(fn(log) { log.action == "read" })
  match viewer_read_log {
    Some(log) => {
      assert_eq(log.user_id, "user1")
      assert_eq(log.resource, "telemetry.data")
      assert_eq(log.action, "read")
      assert_eq(log.granted, true)
      assert_eq(log.ip_address, "192.168.1.100")
    }
    None => assert_true(false)
  }
  
  let viewer_write_log = viewer_logs.find(fn(log) { log.action == "write" })
  match viewer_write_log {
    Some(log) => {
      assert_eq(log.user_id, "user1")
      assert_eq(log.resource, "telemetry.config")
      assert_eq(log.action, "write")
      assert_eq(log.granted, false)
      assert_eq(log.ip_address, "192.168.1.100")
    }
    None => assert_true(false)
  }
}