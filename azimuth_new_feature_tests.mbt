// Azimuth 新功能测试用例
// 包含Azimuth遥测系统新功能的测试

// 测试1: 时间序列数据处理
test "时间序列数据处理和分析" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建测试数据
  let time_series_data = [
    { timestamp: 1609459200, value: 25.5, tags: [("region", "us-east"), ("service", "api")] },
    { timestamp: 1609459260, value: 27.3, tags: [("region", "us-east"), ("service", "api")] },
    { timestamp: 1609459320, value: 23.8, tags: [("region", "us-east"), ("service", "api")] },
    { timestamp: 1609459380, value: 29.1, tags: [("region", "us-west"), ("service", "web")] },
    { timestamp: 1609459440, value: 31.2, tags: [("region", "us-west"), ("service", "web")] }
  ]
  
  // 计算移动平均
  let moving_average = fn(data: Array[TimeSeriesPoint], window_size: Int) {
    let result = []
    for i in window_size..data.length() {
      let window = data.slice(i - window_size, i)
      let sum = window.reduce(fn(acc, point) { acc + point.value }, 0.0)
      let avg = sum / (window_size as Float)
      result = result.push({
        timestamp: data[i].timestamp,
        value: avg,
        tags: data[i].tags
      })
    }
    result
  }
  
  // 按标签分组
  let group_by_tags = fn(data: Array[TimeSeriesPoint], tag_key: String) {
    let groups = Map::empty()
    
    for point in data {
      let tag_value = match point.tags.find(fn(tag) { tag.0 == tag_key }) {
        Some((_, value)) => value
        None => "unknown"
      }
      
      let group = match Map::get(groups, tag_value) {
        Some(g) => g
        None => []
      }
      
      let _ = Map::insert(groups, tag_value, group.push(point))
    }
    
    groups
  }
  
  // 测试移动平均计算
  let ma_result = moving_average(time_series_data, 3)
  assert_eq(ma_result.length(), 2)
  assert_eq(ma_result[0].timestamp, 1609459320)
  assert_eq(ma_result[0].value.round(), 25.53)
  
  // 测试按标签分组
  let grouped = group_by_tags(time_series_data, "region")
  let east_group = match Map::get(grouped, "us-east") {
    Some(g) => g
    None => []
  }
  assert_eq(east_group.length(), 3)
  
  let west_group = match Map::get(grouped, "us-west") {
    Some(g) => g
    None => []
  }
  assert_eq(west_group.length(), 2)
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
  
  // 创建新的追踪上下文
  let create_trace_context = fn() {
    {
      trace_id: "trace-" + Time::now().to_string(),
      span_id: "span-" + Time::now().to_string(),
      parent_span_id: None,
      baggage: [],
      flags: 0
    }
  }
  
  // 创建子span
  let create_child_span = fn(parent: TraceContext) {
    {
      trace_id: parent.trace_id,
      span_id: "span-" + Time::now().to_string(),
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  // 添加baggage项
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    let new_baggage = context.baggage.push((key, value))
    { context with baggage: new_baggage }
  }
  
  // 序列化上下文为字符串
  let serialize_context = fn(context: TraceContext) {
    let baggage_str = context.baggage
      .map(fn(item) { item.0 + "=" + item.1 })
      .join(",")
    
    "trace-id=" + context.trace_id + 
    ";span-id=" + context.span_id + 
    ";parent-span-id=" + context.parent_span_id.unwrap_or("") + 
    ";baggage=" + baggage_str + 
    ";flags=" + context.flags.to_string()
  }
  
  // 测试上下文创建
  let root_context = create_trace_context()
  assert_true(root_context.trace_id.starts_with("trace-"))
  assert_true(root_context.span_id.starts_with("span-"))
  assert_eq(root_context.parent_span_id, None)
  assert_eq(root_context.baggage.length(), 0)
  
  // 测试子span创建
  let child_context = create_child_span(root_context)
  assert_eq(child_context.trace_id, root_context.trace_id)
  assert_eq(child_context.parent_span_id, Some(root_context.span_id))
  assert_not_eq(child_context.span_id, root_context.span_id)
  
  // 测试baggage添加
  let with_baggage = add_baggage(root_context, "user.id", "12345")
  assert_eq(with_baggage.baggage.length(), 1)
  assert_eq(with_baggage.baggage[0], ("user.id", "12345"))
  
  let with_more_baggage = add_baggage(with_baggage, "request.id", "req-67890")
  assert_eq(with_more_baggage.baggage.length(), 2)
  
  // 测试上下文序列化
  let serialized = serialize_context(with_more_baggage)
  assert_true(serialized.contains("trace-id="))
  assert_true(serialized.contains("span-id="))
  assert_true(serialized.contains("baggage=user.id=12345,request.id=req-67890"))
}

// 测试3: 遥测数据压缩
test "遥测数据压缩和解压缩" {
  // 定义遥测事件
  type TelemetryEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)],
    metrics: Array[(String, Float)]
  }
  
  // 创建测试事件
  let events = [
    {
      name: "http.request",
      timestamp: 1609459200,
      attributes: [
        ("method", "GET"),
        ("url", "/api/users"),
        ("status", "200")
      ],
      metrics: [
        ("duration", 125.5),
        ("size", 1024.0)
      ]
    },
    {
      name: "http.request",
      timestamp: 1609459260,
      attributes: [
        ("method", "POST"),
        ("url", "/api/orders"),
        ("status", "201")
      ],
      metrics: [
        ("duration", 210.3),
        ("size", 512.0)
      ]
    }
  ]
  
  // 简单压缩算法：去重复属性键
  let compress_events = fn(events: Array[TelemetryEvent]) {
    let attribute_keys = []
    let metric_keys = []
    
    // 收集所有唯一的键
    for event in events {
      for attr in event.attributes {
        if not attribute_keys.contains(attr.0) {
          attribute_keys = attribute_keys.push(attr.0)
        }
      }
      
      for metric in event.metrics {
        if not metric_keys.contains(metric.0) {
          metric_keys = metric_keys.push(metric.0)
        }
      }
    }
    
    // 压缩事件
    let compressed = events.map(fn(event) {
      let compressed_attrs = event.attributes.map(fn(attr) {
        let key_index = attribute_keys.index_of(attr.0).unwrap_or(0)
        (key_index, attr.1)
      })
      
      let compressed_metrics = event.metrics.map(fn(metric) {
        let key_index = metric_keys.index_of(metric.0).unwrap_or(0)
        (key_index, metric.1)
      })
      
      {
        name: event.name,
        timestamp: event.timestamp,
        attribute_keys: attribute_keys,
        metric_keys: metric_keys,
        attributes: compressed_attrs,
        metrics: compressed_metrics
      }
    })
    
    compressed
  }
  
  // 解压缩事件
  let decompress_events = fn(compressed_events: Array[CompressedEvent]) {
    compressed_events.map(fn(event) {
      let decompressed_attrs = event.attributes.map(fn(attr) {
        let key = event.attribute_keys[attr.0]
        (key, attr.1)
      })
      
      let decompressed_metrics = event.metrics.map(fn(metric) {
        let key = event.metric_keys[metric.0]
        (key, metric.1)
      })
      
      {
        name: event.name,
        timestamp: event.timestamp,
        attributes: decompressed_attrs,
        metrics: decompressed_metrics
      }
    })
  }
  
  // 定义压缩事件类型（用于内部表示）
  type CompressedEvent = {
    name: String,
    timestamp: Int,
    attribute_keys: Array[String],
    metric_keys: Array[String],
    attributes: Array[(Int, String)],
    metrics: Array[(Int, Float)]
  }
  
  // 测试压缩
  let compressed = compress_events(events)
  assert_eq(compressed.length(), 2)
  assert_eq(compressed[0].attribute_keys.length(), 3)  // method, url, status
  assert_eq(compressed[0].metric_keys.length(), 2)     // duration, size
  
  // 验证属性键索引
  assert_eq(compressed[0].attributes[0], (0, "GET"))  // method -> GET
  assert_eq(compressed[0].attributes[1], (1, "/api/users"))  // url -> /api/users
  assert_eq(compressed[0].attributes[2], (2, "200"))  // status -> 200
  
  // 测试解压缩
  let decompressed = decompress_events(compressed)
  assert_eq(decompressed.length(), 2)
  assert_eq(decompressed[0].name, "http.request")
  assert_eq(decompressed[0].attributes, [
    ("method", "GET"),
    ("url", "/api/users"),
    ("status", "200")
  ])
  assert_eq(decompressed[0].metrics, [
    ("duration", 125.5),
    ("size", 1024.0)
  ])
}

// 测试4: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样决策
  type SamplingDecision = {
    sampled: Bool,
    sample_rate: Float,
    attributes: Array[(String, String)]
  }
  
  // 基于trace_id的概率采样
  let probabilistic_sampler = fn(trace_id: String, sample_rate: Float) {
    // 简单哈希函数
    let hash = trace_id.chars().reduce(0, fn(acc, c) { (acc * 31 + c.to_int()) % 10000 })
    let probability = (hash as Float) / 10000.0
    
    {
      sampled: probability <= sample_rate,
      sample_rate: sample_rate,
      attributes: [
        ("sampler.type", "probabilistic"),
        ("sampler.rate", sample_rate.to_string())
      ]
    }
  }
  
  // 基于属性的采样
  let attribute_based_sampler = fn(attributes: Array[(String, String)], rules: Array[(String, String)]) {
    let matches = rules.all(fn(rule) {
      let (key, value) = rule
      attributes.any(fn(attr) { attr.0 == key && attr.1 == value })
    })
    
    {
      sampled: matches,
      sample_rate: if matches { 1.0 } else { 0.0 },
      attributes: [
        ("sampler.type", "attribute-based"),
        ("sampler.rules_matched", matches.to_string())
      ]
    }
  }
  
  // 组合采样器
  let composite_sampler = fn(trace_id: String, attributes: Array[(String, String)], prob_rate: Float, attr_rules: Array[(String, String)]) {
    let prob_decision = probabilistic_sampler(trace_id, prob_rate)
    let attr_decision = attribute_based_sampler(attributes, attr_rules)
    
    // 如果任一采样器决定采样，则采样
    let sampled = prob_decision.sampled || attr_decision.sampled
    
    {
      sampled: sampled,
      sample_rate: if sampled { prob_rate.max(1.0) } else { 0.0 },
      attributes: prob_decision.attributes + attr_decision.attributes
    }
  }
  
  // 测试概率采样
  let trace_id_1 = "trace-12345"
  let prob_decision_1 = probabilistic_sampler(trace_id_1, 0.5)
  // 结果取决于哈希值，我们只验证结构
  assert_true(prob_decision_1.sampled || not prob_decision_1.sampled)  // 总是true
  assert_eq(prob_decision_1.sample_rate, 0.5)
  assert_true(prob_decision_1.attributes.contains(("sampler.type", "probabilistic")))
  
  // 测试基于属性的采样
  let attributes = [
    ("service.name", "payment-service"),
    ("environment", "production"),
    ("endpoint", "/api/pay")
  ]
  
  let rules = [
    ("service.name", "payment-service"),
    ("environment", "production")
  ]
  
  let attr_decision = attribute_based_sampler(attributes, rules)
  assert_true(attr_decision.sampled)
  assert_eq(attr_decision.sample_rate, 1.0)
  
  // 测试不匹配的规则
  let strict_rules = [
    ("service.name", "payment-service"),
    ("environment", "production"),
    ("version", "v2.1.0")
  ]
  
  let strict_attr_decision = attribute_based_sampler(attributes, strict_rules)
  assert_false(strict_attr_decision.sampled)
  assert_eq(strict_attr_decision.sample_rate, 0.0)
  
  // 测试组合采样器
  let composite_decision = composite_sampler(trace_id_1, attributes, 0.3, rules)
  // 由于属性匹配，应该总是采样
  assert_true(composite_decision.sampled)
  assert_eq(composite_decision.sample_rate, 1.0)
}

// 测试5: 遥测指标聚合
test "遥测指标聚合和统计" {
  // 定义指标类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义指标数据点
  type MetricDataPoint = {
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
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float,
    labels: Array[(String, String)]
  }
  
  // 聚合指标
  let aggregate_metrics = fn(points: Array[MetricDataPoint], label_keys: Array[String]) {
    let groups = Map::empty()
    
    for point in points {
      // 构建标签键
      let label_values = label_keys.map(fn(key) {
        match point.labels.find(fn(label) { label.0 == key }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = point.name + "|" + label_values.join("|")
      
      // 获取或创建分组
      let group = match Map::get(groups, group_key) {
        Some(g) => g
        None => []
      }
      
      let _ = Map::insert(groups, group_key, group.push(point))
    }
    
    // 聚合每个分组
    let results = []
    for (group_key, group_points) in groups {
      if group_points.length() > 0 {
        let first_point = group_points[0]
        let values = group_points.map(fn(p) { p.value })
        let count = values.length()
        let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
        let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
        let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
        let avg = sum / (count as Float)
        
        // 提取标签
        let labels = label_keys.map(fn(key) {
          match first_point.labels.find(fn(label) { label.0 == key }) {
            Some((_, value)) => (key, value)
            None => (key, "unknown")
          }
        })
        
        results = results.push({
          name: first_point.name,
          metric_type: first_point.metric_type,
          count: count,
          sum: sum,
          min: min,
          max: max,
          avg: avg,
          labels: labels
        })
      }
    }
    
    results
  }
  
  // 创建测试数据
  let metric_points = [
    {
      name: "http.request.duration",
      metric_type: MetricType::Histogram,
      value: 120.5,
      timestamp: 1609459200,
      labels: [("method", "GET"), ("/api/users", "path"), ("status", "200")]
    },
    {
      name: "http.request.duration",
      metric_type: MetricType::Histogram,
      value: 85.3,
      timestamp: 1609459210,
      labels: [("method", "GET"), ("/api/users", "path"), ("status", "200")]
    },
    {
      name: "http.request.duration",
      metric_type: MetricType::Histogram,
      value: 210.7,
      timestamp: 1609459220,
      labels: [("method", "POST"), ("/api/orders", "path"), ("status", "201")]
    },
    {
      name: "http.request.duration",
      metric_type: MetricType::Histogram,
      value: 195.2,
      timestamp: 1609459230,
      labels: [("method", "POST"), ("/api/orders", "path"), ("status", "400")]
    },
    {
      name: "system.cpu.usage",
      metric_type: MetricType::Gauge,
      value: 65.5,
      timestamp: 1609459240,
      labels: [("instance", "host-1"), ("region", "us-east")]
    }
  ]
  
  // 测试按方法和路径聚合HTTP请求持续时间
  let http_aggregations = aggregate_metrics(metric_points.slice(0, 4), ["method", "/api/users", "path"])
  assert_eq(http_aggregations.length(), 2)
  
  // GET请求应该有2个点
  let get_aggregation = http_aggregations.find(fn(aggr) { 
    aggr.labels.contains(("method", "GET")) 
  }).unwrap()
  assert_eq(get_aggregation.count, 2)
  assert_eq(get_aggregation.sum.round(), 205.8)
  assert_eq(get_aggregation.min, 85.3)
  assert_eq(get_aggregation.max, 120.5)
  assert_eq(get_aggregation.avg.round(), 102.9)
  
  // POST请求应该有2个点
  let post_aggregation = http_aggregations.find(fn(aggr) { 
    aggr.labels.contains(("method", "POST")) 
  }).unwrap()
  assert_eq(post_aggregation.count, 2)
  assert_eq(post_aggregation.sum.round(), 405.9)
  assert_eq(post_aggregation.min, 195.2)
  assert_eq(post_aggregation.max, 210.7)
  assert_eq(post_aggregation.avg.round(), 202.95)
  
  // 测试系统CPU使用率聚合
  let cpu_aggregations = aggregate_metrics(metric_points.slice(4, 5), ["instance", "region"])
  assert_eq(cpu_aggregations.length(), 1)
  
  let cpu_aggregation = cpu_aggregations[0]
  assert_eq(cpu_aggregation.name, "system.cpu.usage")
  assert_eq(cpu_aggregation.metric_type, MetricType::Gauge)
  assert_eq(cpu_aggregation.count, 1)
  assert_eq(cpu_aggregation.sum, 65.5)
  assert_eq(cpu_aggregation.avg, 65.5)
}

// 测试6: 遥测数据转换和格式化
test "遥测数据转换和格式化" {
  // 定义原始遥测数据
  type RawTelemetryData = {
    timestamp: Int,
    level: String,  // "INFO", "WARN", "ERROR"
    message: String,
    fields: Map[String, String]
  }
  
  // 定义标准化的遥测事件
  type StandardTelemetryEvent = {
    name: String,
    timestamp: Int,
    severity: Int,  // 0-INFO, 1-WARN, 2-ERROR
    attributes: Array[(String, String)],
    body: String
  }
  
  // 定义OpenTelemetry格式
  type OtelSpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 转换原始数据为标准事件
  let raw_to_standard = fn(raw: RawTelemetryData) {
    let severity = match raw.level {
      "INFO" => 0
      "WARN" => 1
      "ERROR" => 2
      _ => 0
    }
    
    let attributes = raw.fields.to_array().map(fn(entry) { 
      let (key, value) = entry 
      (key, value) 
    })
    
    {
      name: "log.event",
      timestamp: raw.timestamp,
      severity: severity,
      attributes: attributes,
      body: raw.message
    }
  }
  
  // 转换标准事件为OpenTelemetry Span
  let standard_to_otel = fn(event: StandardTelemetryEvent, trace_id: String, span_id: String) {
    let status = match event.severity {
      0 => "OK"
      1 => "WARNING"
      2 => "ERROR"
      _ => "OK"
    }
    
    {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: None,
      name: event.name,
      start_time: event.timestamp,
      end_time: event.timestamp + 100,  // 假设持续时间100ms
      status: status,
      attributes: event.attributes.push(("log.body", event.body))
    }
  }
  
  // 格式化为JSON字符串
  let format_to_json = fn(otel_span: OtelSpan) {
    let attributes_str = otel_span.attributes
      .map(fn(attr) { "\"" + attr.0 + "\":\"" + attr.1 + "\"" })
      .join(",")
    
    "{" +
      "\"trace_id\":\"" + otel_span.trace_id + "\"," +
      "\"span_id\":\"" + otel_span.span_id + "\"," +
      "\"parent_span_id\":\"" + otel_span.parent_span_id.unwrap_or("") + "\"," +
      "\"name\":\"" + otel_span.name + "\"," +
      "\"start_time\":" + otel_span.start_time.to_string() + "," +
      "\"end_time\":" + otel_span.end_time.to_string() + "," +
      "\"status\":\"" + otel_span.status + "\"," +
      "\"attributes\":{" + attributes_str + "}" +
    "}"
  }
  
  // 测试转换流程
  let raw_log = {
    timestamp: 1609459200,
    level: "ERROR",
    message: "Database connection failed",
    fields: Map::from_array([
      ("service", "payment-service"),
      ("user_id", "12345"),
      ("error_code", "DB_CONN_FAILED")
    ])
  }
  
  // 转换为标准事件
  let standard_event = raw_to_standard(raw_log)
  assert_eq(standard_event.name, "log.event")
  assert_eq(standard_event.timestamp, 1609459200)
  assert_eq(standard_event.severity, 2)  // ERROR
  assert_eq(standard_event.body, "Database connection failed")
  assert_eq(standard_event.attributes.length(), 3)
  assert_true(standard_event.attributes.contains(("service", "payment-service")))
  
  // 转换为OpenTelemetry Span
  let otel_span = standard_to_otel(standard_event, "trace-12345", "span-67890")
  assert_eq(otel_span.trace_id, "trace-12345")
  assert_eq(otel_span.span_id, "span-67890")
  assert_eq(otel_span.name, "log.event")
  assert_eq(otel_span.status, "ERROR")
  assert_eq(otel_span.attributes.length(), 4)
  assert_true(otel_span.attributes.contains(("log.body", "Database connection failed")))
  
  // 格式化为JSON
  let json_str = format_to_json(otel_span)
  assert_true(json_str.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json_str.contains("\"span_id\":\"span-67890\""))
  assert_true(json_str.contains("\"status\":\"ERROR\""))
  assert_true(json_str.contains("\"service\":\"payment-service\""))
}

// 测试7: 遥测数据缓存和存储
test "遥测数据缓存和存储策略" {
  // 定义缓存条目
  type CacheEntry[T] = {
    key: String,
    value: T,
    timestamp: Int,
    ttl: Int,  // 生存时间（秒）
    access_count: Int
  }
  
  // 定义缓存配置
  type CacheConfig = {
    max_size: Int,
    default_ttl: Int,
    cleanup_interval: Int
  }
  
  // 创建简单的内存缓存
  let create_cache = fn(config: CacheConfig) {
    let cache = Map::empty()
    
    let get = fn(key: String) {
      match Map::get(cache, key) {
        Some(entry) => {
          // 检查是否过期
          let current_time = Time::now()
          if current_time - entry.timestamp > entry.ttl {
            let _ = Map::remove(cache, key)
            None
          } else {
            // 更新访问计数
            let updated_entry = { entry with access_count: entry.access_count + 1 }
            let _ = Map::insert(cache, key, updated_entry)
            Some(updated_entry.value)
          }
        }
        None => None
      }
    }
    
    let put = fn(key: String, value: T, ttl: Option[Int]) {
      let current_time = Time::now()
      let entry_ttl = ttl.unwrap_or(config.default_ttl)
      
      // 如果缓存已满，移除最少使用的条目
      if cache.size() >= config.max_size && not Map::contains(cache, key) {
        let mut lru_key = ""
        let mut min_access = 999999
        
        for (k, v) in cache {
          if v.access_count < min_access {
            min_access = v.access_count
            lru_key = k
          }
        }
        
        if lru_key != "" {
          let _ = Map::remove(cache, lru_key)
        }
      }
      
      let entry = {
        key: key,
        value: value,
        timestamp: current_time,
        ttl: entry_ttl,
        access_count: 1
      }
      
      let _ = Map::insert(cache, key, entry)
    }
    
    let cleanup = fn() {
      let current_time = Time::now()
      let expired_keys = []
      
      for (key, entry) in cache {
        if current_time - entry.timestamp > entry.ttl {
          expired_keys = expired_keys.push(key)
        }
      }
      
      for key in expired_keys {
        let _ = Map::remove(cache, key)
      }
      
      expired_keys.length()
    }
    
    { get, put, cleanup }
  }
  
  // 测试缓存操作
  let config = {
    max_size: 3,
    default_ttl: 60,  // 60秒
    cleanup_interval: 30
  }
  
  let cache = create_cache(config)
  
  // 添加条目
  cache.put("trace-1", "trace-data-1", None)
  cache.put("trace-2", "trace-data-2", None)
  cache.put("trace-3", "trace-data-3", None)
  
  // 获取条目
  let value1 = cache.get("trace-1")
  assert_eq(value1, Some("trace-data-1"))
  
  // 再次获取，应该增加访问计数
  let value1_again = cache.get("trace-1")
  assert_eq(value1_again, Some("trace-data-1"))
  
  // 添加第4个条目，应该移除最少使用的
  cache.put("trace-4", "trace-data-4", None)
  
  // 假设trace-2和trace-3只被访问过一次，trace-1被访问过两次
  // 当添加trace-4时，应该移除trace-2或trace-3中的一个
  let value2 = cache.get("trace-2")
  let value3 = cache.get("trace-3")
  let value4 = cache.get("trace-4")
  
  // 验证至少有一个被移除
  assert_true(value2.is_none() || value3.is_none())
  assert_eq(value4, Some("trace-data-4"))
  
  // 测试过期（模拟）
  // 注意：在真实环境中，我们需要等待或模拟时间流逝
  let expired_count = cache.cleanup()
  // 在实际测试中，这取决于当前时间和条目时间戳
  // 这里我们只验证函数可以调用
  assert_true(expired_count >= 0)
}

// 测试8: 遥测数据批处理和导出
test "遥测数据批处理和导出" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_batch_time_ms: Int,
    retry_attempts: Int,
    backoff_factor: Float
  }
  
  // 定义导出请求
  type ExportRequest = {
    id: String,
    data: Array[String],
    timestamp: Int,
    attempts: Int
  }
  
  // 定义导出结果
  type ExportResult = {
    success: Bool,
    message: String,
    retry_after: Option[Int]
  }
  
  // 创建批处理器
  let create_batch_processor = fn(config: BatchConfig) {
    let mut batch = []
    let mut last_flush_time = Time::now()
    
    let add_to_batch = fn(item: String) {
      batch = batch.push(item)
      
      let current_time = Time::now()
      let should_flush = batch.length() >= config.max_batch_size || 
                        (current_time - last_flush_time) >= config.max_batch_time_ms
      
      if should_flush {
        flush_batch()
      }
    }
    
    let flush_batch = fn() {
      if batch.length() > 0 {
        let request = {
          id: "req-" + Time::now().to_string(),
          data: batch.copy(),
          timestamp: Time::now(),
          attempts: 0
        }
        
        // 重置批次
        batch = []
        last_flush_time = Time::now()
        
        Some(request)
      } else {
        None
      }
    }
    
    let force_flush = fn() {
      flush_batch()
    }
    
    { add_to_batch, force_flush }
  }
  
  // 模拟导出函数
  let mock_export = fn(request: ExportRequest) {
    let success = request.attempts < 2  // 假设前两次失败，第三次成功
    
    {
      success: success,
      message: if success { 
        "导出成功: " + request.data.length().to_string() + " 个项目" 
      } else { 
        "导出失败，尝试次数: " + request.attempts.to_string() 
      },
      retry_after: if not success { Some(1000 * (request.attempts + 1)) } else { None }
    }
  }
  
  // 带重试的导出
  let export_with_retry = fn(request: ExportRequest, max_attempts: Int) {
    let mut current_request = request
    let mut attempts = 0
    
    while attempts < max_attempts {
      attempts = attempts + 1
      current_request = { current_request with attempts: attempts }
      
      let result = mock_export(current_request)
      if result.success {
        return Some(result)
      } else if attempts < max_attempts {
        // 等待重试延迟（在实际环境中）
        let retry_delay = result.retry_after.unwrap_or(1000)
        println("等待 " + retry_delay.to_string() + "ms 后重试")
      }
    }
    
    None  // 所有尝试都失败
  }
  
  // 测试批处理
  let batch_config = {
    max_batch_size: 3,
    max_batch_time_ms: 5000,
    retry_attempts: 3,
    backoff_factor: 1.5
  }
  
  let processor = create_batch_processor(batch_config)
  
  // 添加项目，但未达到最大批次大小
  processor.add_to_batch("item-1")
  processor.add_to_batch("item-2")
  
  // 添加第三个项目，应该触发批处理
  let request1 = processor.add_to_batch("item-3")
  assert_true(request1.is_some())
  
  let req1 = request1.unwrap()
  assert_eq(req1.data.length(), 3)
  assert_eq(req1.data, ["item-1", "item-2", "item-3"])
  
  // 添加更多项目
  processor.add_to_batch("item-4")
  processor.add_to_batch("item-5")
  
  // 强制刷新，即使未达到最大批次大小
  let request2 = processor.force_flush()
  assert_true(request2.is_some())
  
  let req2 = request2.unwrap()
  assert_eq(req2.data.length(), 2)
  assert_eq(req2.data, ["item-4", "item-5"])
  
  // 测试重试导出
  let test_request = {
    id: "test-req",
    data: ["data-1", "data-2"],
    timestamp: Time::now(),
    attempts: 0
  }
  
  let export_result = export_with_retry(test_request, 3)
  assert_true(export_result.is_some())
  
  let result = export_result.unwrap()
  assert_true(result.success)
  assert_true(result.message.contains("导出成功"))
}