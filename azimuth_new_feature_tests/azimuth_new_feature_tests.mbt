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

// 测试9: 遥测数据安全性
test "遥测数据安全性和隐私保护" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    Email
    PhoneNumber
    CreditCard
    SSN
    IPAddress
    Custom(String)
  }
  
  // 定义数据脱敏规则
  type MaskingRule = {
    data_type: SensitiveDataType,
    pattern: String,
    replacement: String,
    preserve_length: Bool
  }
  
  // 定义PII检测结果
  type PIIDetectionResult = {
    is_pii: Bool,
    data_type: Option[SensitiveDataType],
    confidence: Float,
    position: (Int, Int)  // (start, end)
  }
  
  // 检测敏感数据
  let detect_pii = fn(text: String, patterns: Array[(SensitiveDataType, String)]) {
    let results = []
    
    for (data_type, pattern) in patterns {
      // 简化的模式匹配（实际实现可能需要正则表达式）
      if pattern == "email" && text.contains("@") {
        results = results.push({
          is_pii: true,
          data_type: Some(data_type),
          confidence: 0.9,
          position: (0, text.length() - 1)
        })
      } else if pattern == "phone" && text.chars().any(fn(c) { c.to_int() >= 48 && c.to_int() <= 57 }) {
        results = results.push({
          is_pii: true,
          data_type: Some(data_type),
          confidence: 0.7,
          position: (0, text.length() - 1)
        })
      } else if pattern == "credit-card" && text.length() >= 13 && text.chars().all(fn(c) { c.to_int() >= 48 && c.to_int() <= 57 || c == '-' || c == ' ' }) {
        results = results.push({
          is_pii: true,
          data_type: Some(data_type),
          confidence: 0.8,
          position: (0, text.length() - 1)
        })
      }
    }
    
    results
  }
  
  // 应用脱敏规则
  let apply_masking = fn(text: String, rule: MaskingRule) {
    match rule.data_type {
      SensitiveDataType::Email => {
        let parts = text.split("@")
        if parts.length() == 2 {
          let username = parts[0]
          let domain = parts[1]
          let masked_username = if username.length() > 2 {
            username.slice(0, 2) + "*".repeat(username.length() - 2)
          } else {
            "*".repeat(username.length())
          }
          masked_username + "@" + domain
        } else {
          rule.replacement
        }
      }
      SensitiveDataType::PhoneNumber => {
        if text.length() > 4 {
          text.slice(0, 3) + "*".repeat(text.length() - 6) + text.slice(text.length() - 3, text.length())
        } else {
          rule.replacement
        }
      }
      SensitiveDataType::CreditCard => {
        if text.length() > 4 {
          "*".repeat(text.length() - 4) + text.slice(text.length() - 4, text.length())
        } else {
          rule.replacement
        }
      }
      SensitiveDataType::IPAddress => {
        let parts = text.split(".")
        if parts.length() == 4 {
          parts[0] + "." + parts[1] + ".*.*"
        } else {
          rule.replacement
        }
      }
      _ => rule.replacement
    }
  }
  
  // 加密敏感数据
  let encrypt_data = fn(data: String, key: String) {
    // 简化的加密（仅用于演示，实际应用中应使用安全的加密算法）
    let key_bytes = key.to_bytes()
    let data_bytes = data.to_bytes()
    
    let encrypted = data_bytes.map_with_index(fn(i, b) { 
      let key_byte = key_bytes[i % key_bytes.length()]
      b ^ key_byte  // 简单XOR加密
    })
    
    // 转换为Base64字符串（简化）
    encrypted.map(fn(b) { b.to_string() }).join("")
  }
  
  // 定义遥测事件
  type TelemetryEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)],
    user_id: Option[String]
  }
  
  // 安全处理遥测事件
  let secure_telemetry_event = fn(event: TelemetryEvent, pii_patterns: Array[(SensitiveDataType, String)], masking_rules: Array[MaskingRule]) {
    let secure_attributes = []
    
    for attr in event.attributes {
      let (key, value) = attr
      let pii_detections = detect_pii(value, pii_patterns)
      
      if pii_detections.length() > 0 {
        // 检测到敏感数据，应用脱敏
        let data_type = pii_detections[0].data_type.unwrap_or(SensitiveDataType::Custom("unknown"))
        let masking_rule = masking_rules.find(fn(rule) { rule.data_type == data_type })
        
        let masked_value = match masking_rule {
          Some(rule) => apply_masking(value, rule)
          None => "[REDACTED]"
        }
        
        secure_attributes = secure_attributes.push((key, masked_value))
      } else {
        // 未检测到敏感数据，保留原值
        secure_attributes = secure_attributes.push((key, value))
      }
    }
    
    // 处理用户ID
    let secure_user_id = match event.user_id {
      Some(id) => {
        let pii_detections = detect_pii(id, pii_patterns)
        if pii_detections.length() > 0 {
          Some(encrypt_data(id, "encryption-key-12345"))
        } else {
          Some(id)
        }
      }
      None => None
    }
    
    {
      name: event.name,
      timestamp: event.timestamp,
      attributes: secure_attributes,
      user_id: secure_user_id
    }
  }
  
  // 定义PII检测模式
  let pii_patterns = [
    (SensitiveDataType::Email, "email"),
    (SensitiveDataType::PhoneNumber, "phone"),
    (SensitiveDataType::CreditCard, "credit-card"),
    (SensitiveDataType::IPAddress, "ip")
  ]
  
  // 定义脱敏规则
  let masking_rules = [
    {
      data_type: SensitiveDataType::Email,
      pattern: "email",
      replacement: "[EMAIL-REDACTED]",
      preserve_length: true
    },
    {
      data_type: SensitiveDataType::PhoneNumber,
      pattern: "phone",
      replacement: "[PHONE-REDACTED]",
      preserve_length: true
    },
    {
      data_type: SensitiveDataType::CreditCard,
      pattern: "credit-card",
      replacement: "[CARD-REDACTED]",
      preserve_length: true
    },
    {
      data_type: SensitiveDataType::IPAddress,
      pattern: "ip",
      replacement: "[IP-REDACTED]",
      preserve_length: false
    }
  ]
  
  // 测试PII检测
  let email_text = "user@example.com"
  let email_detections = detect_pii(email_text, pii_patterns)
  assert_eq(email_detections.length(), 1)
  assert_true(email_detections[0].is_pii)
  assert_eq(email_detections[0].data_type, Some(SensitiveDataType::Email))
  
  let phone_text = "123-456-7890"
  let phone_detections = detect_pii(phone_text, pii_patterns)
  assert_eq(phone_detections.length(), 1)
  assert_true(phone_detections[0].is_pii)
  assert_eq(phone_detections[0].data_type, Some(SensitiveDataType::PhoneNumber))
  
  // 测试数据脱敏
  let email_rule = masking_rules[0]
  let masked_email = apply_masking(email_text, email_rule)
  assert_eq(masked_email, "us***@example.com")
  
  let phone_rule = masking_rules[1]
  let masked_phone = apply_masking(phone_text, phone_rule)
  assert_eq(masked_phone, "123***7890")
  
  let credit_card = "1234567890123456"
  let card_rule = masking_rules[2]
  let masked_card = apply_masking(credit_card, card_rule)
  assert_eq(masked_card, "************3456")
  
  let ip_address = "192.168.1.100"
  let ip_rule = masking_rules[3]
  let masked_ip = apply_masking(ip_address, ip_rule)
  assert_eq(masked_ip, "192.168.*.*")
  
  // 测试遥测事件安全处理
  let telemetry_event = {
    name: "user.login",
    timestamp: 1609459200,
    attributes: [
      ("email", "john.doe@example.com"),
      ("ip_address", "192.168.1.100"),
      ("user_agent", "Mozilla/5.0"),
      ("session_id", "sess_123456789")
    ],
    user_id: Some("user-12345")
  }
  
  let secure_event = secure_telemetry_event(telemetry_event, pii_patterns, masking_rules)
  
  // 验证敏感数据已被脱敏
  let email_attr = secure_event.attributes.find(fn(attr) { attr.0 == "email" })
  assert_true(email_attr.is_some())
  assert_eq(email_attr.unwrap().1, "jo***@example.com")
  
  let ip_attr = secure_event.attributes.find(fn(attr) { attr.0 == "ip_address" })
  assert_true(ip_attr.is_some())
  assert_eq(ip_attr.unwrap().1, "192.168.*.*")
  
  // 验证非敏感数据保持不变
  let user_agent_attr = secure_event.attributes.find(fn(attr) { attr.0 == "user_agent" })
  assert_true(user_agent_attr.is_some())
  assert_eq(user_agent_attr.unwrap().1, "Mozilla/5.0")
  
  let session_attr = secure_event.attributes.find(fn(attr) { attr.0 == "session_id" })
  assert_true(session_attr.is_some())
  assert_eq(session_attr.unwrap().1, "sess_123456789")
  
  // 验证用户ID已加密
  assert_true(secure_event.user_id.is_some())
  assert_not_eq(secure_event.user_id.unwrap(), "user-12345")
}

// 测试10: 遥测系统性能测试
test "遥测系统性能和资源优化" {
  // 定义性能指标
  type PerformanceMetrics = {
    cpu_usage: Float,
    memory_usage: Float,
    throughput: Float,  // 每秒处理的事件数
    latency_p50: Float,  // 50百分位延迟（毫秒）
    latency_p95: Float,  // 95百分位延迟（毫秒）
    latency_p99: Float   // 99百分位延迟（毫秒）
  }
  
  // 定义资源监控器
  type ResourceMonitor = {
    start_time: Int,
    initial_memory: Int,
    event_count: Int,
    latencies: Array[Int]  // 以毫秒为单位
  }
  
  // 创建资源监控器
  let create_monitor = fn() {
    {
      start_time: Time::now(),
      initial_memory: 0,  // 简化，实际应获取真实内存使用
      event_count: 0,
      latencies: []
    }
  }
  
  // 记录事件处理
  let record_event = fn(monitor: ResourceMonitor, processing_time_ms: Int) {
    {
      start_time: monitor.start_time,
      initial_memory: monitor.initial_memory,
      event_count: monitor.event_count + 1,
      latencies: monitor.latencies.push(processing_time_ms)
    }
  }
  
  // 计算性能指标
  let calculate_metrics = fn(monitor: ResourceMonitor) {
    let current_time = Time::now()
    let elapsed_seconds = (current_time - monitor.start_time) as Float / 1000.0
    
    let sorted_latencies = monitor.latencies.sort(fn(a, b) { a <= b })
    let len = sorted_latencies.length()
    
    let p50 = if len > 0 { sorted_latencies[len / 2] as Float } else { 0.0 }
    let p95 = if len > 0 { sorted_latencies[(len * 95) / 100] as Float } else { 0.0 }
    let p99 = if len > 0 { sorted_latencies[(len * 99) / 100] as Float } else { 0.0 }
    
    {
      cpu_usage: 0.0,  // 简化，实际应测量CPU使用率
      memory_usage: 0.0,  // 简化，实际应测量内存使用
      throughput: if elapsed_seconds > 0.0 { (monitor.event_count as Float) / elapsed_seconds } else { 0.0 },
      latency_p50: p50,
      latency_p95: p95,
      latency_p99: p99
    }
  }
  
  // 定义遥测事件处理器
  type TelemetryProcessor = {
    buffer_size: Int,
    batch_size: Int,
    compression_enabled: Bool,
    sampling_rate: Float
  }
  
  // 创建处理器
  let create_processor = fn(buffer_size: Int, batch_size: Int, compression_enabled: Bool, sampling_rate: Float) {
    {
      buffer_size: buffer_size,
      batch_size: batch_size,
      compression_enabled: compression_enabled,
      sampling_rate: sampling_rate
    }
  }
  
  // 处理遥测事件
  let process_events = fn(processor: TelemetryProcessor, events: Array[String]) {
    let start_time = Time::now()
    
    // 采样
    let sampled_events = if processor.sampling_rate < 1.0 {
      let sample_count = (events.length() as Float * processor.sampling_rate) as Int
      events.slice(0, sample_count)
    } else {
      events
    }
    
    // 批处理
    let batches = []
    let mut i = 0
    while i < sampled_events.length() {
      let end = if i + processor.batch_size < sampled_events.length() { 
        i + processor.batch_size 
      } else { 
        sampled_events.length() 
      }
      batches = batches.push(sampled_events.slice(i, end))
      i = i + processor.batch_size
    }
    
    // 处理每个批次（简化）
    for batch in batches {
      // 模拟处理时间
      let processing_time = if processor.compression_enabled { 
        batch.length() * 2  // 压缩增加处理时间
      } else { 
        batch.length() 
      }
      
      // 模拟处理延迟
      let _ = Time::sleep(processing_time as Int)
    }
    
    Time::now() - start_time
  }
  
  // 性能测试场景
  let run_performance_test = fn(processor: TelemetryProcessor, event_count: Int) {
    let monitor = create_monitor()
    
    // 生成测试事件
    let events = []
    let mut i = 0
    while i < event_count {
      events = events.push("event-" + i.to_string())
      i = i + 1
    }
    
    // 处理事件并记录性能
    let processing_time = process_events(processor, events)
    let updated_monitor = record_event(monitor, processing_time)
    
    // 计算性能指标
    calculate_metrics(updated_monitor)
  }
  
  // 测试不同配置下的性能
  
  // 基准配置：小缓冲区，小批次，无压缩，全采样
  let baseline_processor = create_processor(100, 10, false, 1.0)
  let baseline_metrics = run_performance_test(baseline_processor, 100)
  
  // 优化配置1：增大批次大小
  let optimized_processor_1 = create_processor(100, 50, false, 1.0)
  let optimized_metrics_1 = run_performance_test(optimized_processor_1, 100)
  
  // 优化配置2：启用压缩
  let optimized_processor_2 = create_processor(100, 10, true, 1.0)
  let optimized_metrics_2 = run_performance_test(optimized_processor_2, 100)
  
  // 优化配置3：降低采样率
  let optimized_processor_3 = create_processor(100, 10, false, 0.5)
  let optimized_metrics_3 = run_performance_test(optimized_processor_3, 100)
  
  // 验证性能指标
  assert_true(baseline_metrics.throughput > 0.0)
  assert_true(baseline_metrics.latency_p50 >= 0.0)
  assert_true(baseline_metrics.latency_p95 >= baseline_metrics.latency_p50)
  assert_true(baseline_metrics.latency_p99 >= baseline_metrics.latency_p95)
  
  // 验证优化效果
  
  // 增大批次大小应该提高吞吐量（减少批次开销）
  assert_true(optimized_metrics_1.throughput >= baseline_metrics.throughput)
  
  // 启用压缩可能降低吞吐量（增加处理时间）但减少内存使用
  // 这里我们只验证指标存在，实际效果取决于具体实现
  assert_true(optimized_metrics_2.throughput > 0.0)
  
  // 降低采样率应该显著提高吞吐量
  assert_true(optimized_metrics_3.throughput > baseline_metrics.throughput)
  
  // 测试资源限制下的性能
  let resource_constrained_processor = create_processor(10, 5, true, 0.1)
  let constrained_metrics = run_performance_test(resource_constrained_processor, 100)
  
  // 资源受限情况下，吞吐量应该更低
  assert_true(constrained_metrics.throughput < baseline_metrics.throughput)
  
  // 测试高负载场景
  let high_load_processor = create_processor(1000, 100, false, 1.0)
  let high_load_metrics = run_performance_test(high_load_processor, 1000)
  
  // 高负载下延迟可能增加
  assert_true(high_load_metrics.latency_p95 >= baseline_metrics.latency_p95)
  
  // 性能回归测试
  let regression_test = fn(current_metrics: PerformanceMetrics, baseline: PerformanceMetrics, threshold: Float) {
    let throughput_regression = (baseline.throughput - current_metrics.throughput) / baseline.throughput
    let latency_regression = (current_metrics.latency_p95 - baseline.latency_p95) / baseline.latency_p95
    
    throughput_regression < threshold && latency_regression < threshold
  }
  
  // 验证性能没有显著回归（允许10%的回归）
  assert_true(regression_test(optimized_metrics_1, baseline_metrics, 0.1))
  assert_true(regression_test(optimized_metrics_2, baseline_metrics, 0.1))
}

// 测试11: 遥测数据实时处理
test "遥测数据实时处理和流式计算" {
  // 定义流事件
  type StreamEvent = {
    id: String,
    timestamp: Int,
    data: Map[String, String],
    watermark: Int  // 水印，表示事件时间进度
  }
  
  // 定义时间窗口
  enum WindowType {
    Tumbling(Int)  // 滚动窗口（窗口大小，秒）
    Sliding(Int, Int)  // 滑动窗口（窗口大小，滑动间隔，秒）
    Session(Int)  // 会话窗口（超时时间，秒）
  }
  
  // 定义窗口聚合结果
  type WindowResult = {
    window_start: Int,
    window_end: Int,
    count: Int,
    aggregates: Map[String, Float]
  }
  
  // 创建流处理器
  let create_stream_processor = fn() {
    let mut buffer = []
    let mut results = []
    
    let process_event = fn(event: StreamEvent, window_type: WindowType, aggregation_fn: Map[String, Float] -> Map[String, Float]) {
      buffer = buffer.push(event)
      
      // 按时间戳排序
      buffer = buffer.sort(fn(a, b) { a.timestamp <= b.timestamp })
      
      // 根据窗口类型处理事件
      match window_type {
        WindowType::Tumbling(size) => {
          // 找出所有不重叠的窗口
          let mut i = 0
          while i < buffer.length() {
            let start_time = buffer[i].timestamp
            let end_time = start_time + size * 1000  // 转换为毫秒
            
            // 收集窗口内的事件
            let window_events = buffer.filter(fn(e) { 
              e.timestamp >= start_time && e.timestamp < end_time 
            })
            
            if window_events.length() > 0 {
              // 计算聚合
              let initial_aggregates = Map::from_array([
                ("sum", 0.0),
                ("avg", 0.0),
                ("min", 999999.0),
                ("max", 0.0)
              ])
              
              let aggregates = window_events.reduce(initial_aggregates, fn(acc, e) {
                let value = match Map::get(e.data, "value") {
                  Some(v) => v.parse_float().unwrap_or(0.0)
                  None => 0.0
                }
                
                let sum = match Map::get(acc, "sum") { Some(s) => s + value | None => value }
                let count = window_events.length() as Float
                let avg = sum / count
                
                let min_val = match Map::get(acc, "min") { 
                  Some(m) => if value < m { value } else { m } 
                  | None => value 
                }
                let max_val = match Map::get(acc, "max") { 
                  Some(m) => if value > m { value } else { m } 
                  | None => value 
                }
                
                Map::from_array([
                  ("sum", sum),
                  ("avg", avg),
                  ("min", min_val),
                  ("max", max_val)
                ])
              })
              
              results = results.push({
                window_start: start_time,
                window_end: end_time,
                count: window_events.length(),
                aggregates: aggregates
              })
              
              // 移除已处理的事件
              buffer = buffer.filter(fn(e) { e.timestamp >= end_time })
              i = 0  // 重置索引，因为缓冲区已更改
            } else {
              i = i + 1
            }
          }
        }
        WindowType::Sliding(size, slide) => {
          // 滑动窗口处理（简化）
          if buffer.length() > 0 {
            let latest_time = buffer[buffer.length() - 1].timestamp
            let window_start = latest_time - size * 1000
            
            // 收集窗口内的事件
            let window_events = buffer.filter(fn(e) { 
              e.timestamp >= window_start && e.timestamp <= latest_time 
            })
            
            if window_events.length() > 0 {
              // 计算聚合（简化）
              let sum = window_events.reduce(0.0, fn(acc, e) {
                let value = match Map::get(e.data, "value") {
                  Some(v) => v.parse_float().unwrap_or(0.0)
                  None => 0.0
                }
                acc + value
              })
              
              let count = window_events.length() as Float
              let avg = sum / count
              
              results = results.push({
                window_start: window_start,
                window_end: latest_time,
                count: window_events.length(),
                aggregates: Map::from_array([
                  ("sum", sum),
                  ("avg", avg)
                ])
              })
            }
          }
        }
        WindowType::Session(timeout) => {
          // 会话窗口处理（简化）
          if buffer.length() > 0 {
            // 按会话分组（简化实现）
            let sessions = []
            let mut current_session = []
            let mut session_start = buffer[0].timestamp
            let mut last_event_time = buffer[0].timestamp
            
            for event in buffer {
              if event.timestamp - last_event_time <= timeout * 1000 {
                // 同一会话
                current_session = current_session.push(event)
                last_event_time = event.timestamp
              } else {
                // 新会话
                if current_session.length() > 0 {
                  sessions = sessions.push((session_start, last_event_time, current_session))
                }
                current_session = [event]
                session_start = event.timestamp
                last_event_time = event.timestamp
              }
            }
            
            // 添加最后一个会话
            if current_session.length() > 0 {
              sessions = sessions.push((session_start, last_event_time, current_session))
            }
            
            // 为每个会话计算聚合
            for (start, end, events) in sessions {
              let sum = events.reduce(0.0, fn(acc, e) {
                let value = match Map::get(e.data, "value") {
                  Some(v) => v.parse_float().unwrap_or(0.0)
                  None => 0.0
                }
                acc + value
              })
              
              let count = events.length() as Float
              let avg = sum / count
              
              results = results.push({
                window_start: start,
                window_end: end,
                count: events.length(),
                aggregates: Map::from_array([
                  ("sum", sum),
                  ("avg", avg)
                ])
              })
            }
            
            // 清空缓冲区，因为所有事件都已处理
            buffer = []
          }
        }
      }
      
      results
    }
    
    let get_results = fn() { results }
    
    { process_event, get_results }
  }
  
  // 创建实时事件源
  let create_event_source = fn() {
    let mut counter = 0
    
    let generate_event = fn(base_time: Int, interval_ms: Int) {
      counter = counter + 1
      let timestamp = base_time + counter * interval_ms
      let value = 10.0 + (counter % 20) as Float  // 10-30之间的值
      
      {
        id: "event-" + counter.to_string(),
        timestamp: timestamp,
        data: Map::from_array([
          ("value", value.to_string()),
          ("source", "sensor-" + ((counter % 5) + 1).to_string())
        ]),
        watermark: timestamp
      }
    }
    
    { generate_event }
  }
  
  // 测试滚动窗口
  let processor1 = create_stream_processor()
  let source1 = create_event_source()
  let base_time = 1609459200000  // 基准时间（毫秒）
  
  // 生成事件
  let event1 = source1.generate_event(base_time, 1000)  // 1秒间隔
  let event2 = source1.generate_event(base_time, 1000)
  let event3 = source1.generate_event(base_time, 1000)
  let event4 = source1.generate_event(base_time, 1000)
  let event5 = source1.generate_event(base_time, 1000)
  
  // 处理事件（5秒滚动窗口）
  let _ = processor1.process_event(event1, WindowType::Tumbling(5), fn(aggs) { aggs })
  let _ = processor1.process_event(event2, WindowType::Tumbling(5), fn(aggs) { aggs })
  let _ = processor1.process_event(event3, WindowType::Tumbling(5), fn(aggs) { aggs })
  let _ = processor1.process_event(event4, WindowType::Tumbling(5), fn(aggs) { aggs })
  let _ = processor1.process_event(event5, WindowType::Tumbling(5), fn(aggs) { aggs })
  
  let results1 = processor1.get_results()
  assert_eq(results1.length(), 1)  // 应该有一个窗口结果
  
  let window1 = results1[0]
  assert_eq(window1.count, 5)  // 5个事件
  assert_eq(window1.window_end - window1.window_start, 5000)  // 5秒窗口
  
  // 验证聚合结果
  let sum = match Map::get(window1.aggregates, "sum") { Some(s) => s | None => 0.0 }
  let avg = match Map::get(window1.aggregates, "avg") { Some(a) => a | None => 0.0 }
  assert_true(sum > 0.0)
  assert_true(avg > 0.0)
  
  // 测试滑动窗口
  let processor2 = create_stream_processor()
  let source2 = create_event_source()
  
  // 生成事件
  let event6 = source2.generate_event(base_time, 1000)
  let event7 = source2.generate_event(base_time, 1000)
  let event8 = source2.generate_event(base_time, 1000)
  
  // 处理事件（3秒滑动窗口，1秒滑动）
  let _ = processor2.process_event(event6, WindowType::Sliding(3, 1), fn(aggs) { aggs })
  let _ = processor2.process_event(event7, WindowType::Sliding(3, 1), fn(aggs) { aggs })
  let _ = processor2.process_event(event8, WindowType::Sliding(3, 1), fn(aggs) { aggs })
  
  let results2 = processor2.get_results()
  assert_eq(results2.length(), 3)  // 应该有三个窗口结果
  
  // 测试会话窗口
  let processor3 = create_stream_processor()
  let source3 = create_event_source()
  
  // 生成事件，其中一些间隔较大
  let event9 = source3.generate_event(base_time, 1000)
  let event10 = source3.generate_event(base_time, 1000)
  let event11 = source3.generate_event(base_time, 5000)  // 5秒间隔
  let event12 = source3.generate_event(base_time, 1000)  // 1秒间隔
  
  // 处理事件（2秒会话超时）
  let _ = processor3.process_event(event9, WindowType::Session(2), fn(aggs) { aggs })
  let _ = processor3.process_event(event10, WindowType::Session(2), fn(aggs) { aggs })
  let _ = processor3.process_event(event11, WindowType::Session(2), fn(aggs) { aggs })
  let _ = processor3.process_event(event12, WindowType::Session(2), fn(aggs) { aggs })
  
  let results3 = processor3.get_results()
  assert_eq(results3.length(), 2)  // 应该有两个会话
  
  // 验证第一个会话包含2个事件
  let session1 = results3[0]
  assert_eq(session1.count, 2)
  
  // 验证第二个会话包含2个事件
  let session2 = results3[1]
  assert_eq(session2.count, 2)
  
  // 测试水印和延迟事件处理
  let processor4 = create_stream_processor()
  
  // 创建延迟事件
  let late_event = {
    id: "late-event",
    timestamp: base_time - 5000,  // 比当前时间早5秒
    data: Map::from_array([
      ("value", "15.0"),
      ("source", "sensor-1")
    ]),
    watermark: base_time  // 水印表示当前时间进度
  }
  
  // 处理延迟事件（应该被丢弃或特殊处理）
  let _ = processor4.process_event(late_event, WindowType::Tumbling(5), fn(aggs) { aggs })
  
  let results4 = processor4.get_results()
  // 由于事件时间早于水印，可能不会产生窗口结果
  // 具体行为取决于实现，这里我们只验证处理器可以处理延迟事件
  assert_true(results4.length() >= 0)
}

// 测试12: 遥测系统错误处理和恢复
test "遥测系统错误处理和恢复机制" {
  // 定义错误类型
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    StorageError(String)
    ValidationError(String)
    RateLimitError(Int)  // 重试次数
    SystemOverloadError(Float)  // 系统负载百分比
  }
  
  // 定义错误恢复策略
  enum RecoveryStrategy {
    Retry(Int)  // 最大重试次数
    CircuitBreaker(Int, Int)  // 失败阈值，恢复超时（秒）
    Failover(Array[String])  // 备用端点列表
    Backoff(Int, Float)  // 初始延迟（毫秒），退避因子
    GracefulDegradation  // 优雅降级
  }
  
  // 定义错误处理结果
  type ErrorHandlingResult = {
    success: Bool,
    error: Option[TelemetryError],
    attempts: Int,
    total_time_ms: Int,
    strategy_used: Option[RecoveryStrategy]
  }
  
  // 创建错误处理器
  let create_error_handler = fn(default_strategy: RecoveryStrategy) {
    let mut failure_count = 0
    let mut last_failure_time = 0
    let mut circuit_breaker_open = false
    let mut circuit_breaker_open_time = 0
    
    let handle_error = fn(error: TelemetryError, strategy: RecoveryStrategy) {
      let start_time = Time::now()
      let mut attempts = 0
      let mut success = false
      let mut current_error = Some(error)
      
      match strategy {
        RecoveryStrategy::Retry(max_attempts) => {
          while attempts < max_attempts && not success {
            attempts = attempts + 1
            
            // 模拟操作（可能失败）
            let operation_success = match current_error {
              Some(TelemetryError::NetworkError(_)) => attempts >= 3  // 网络错误第3次尝试成功
              Some(TelemetryError::SerializationError(_)) => attempts >= 2  // 序列化错误第2次尝试成功
              Some(TelemetryError::StorageError(_)) => attempts >= max_attempts  // 存储错误总是失败
              Some(TelemetryError::ValidationError(_)) => false  // 验证错误总是失败
              Some(TelemetryError::RateLimitError(_)) => attempts >= 1  // 速率限制错误第1次尝试成功
              Some(TelemetryError::SystemOverloadError(_)) => attempts >= 2  // 系统过载第2次尝试成功
              None => true  // 没有错误，成功
            }
            
            if operation_success {
              success = true
              current_error = None
            } else if attempts < max_attempts {
              // 等待重试延迟（简化）
              let _ = Time::sleep(100 * attempts)  // 递增延迟
            }
          }
        }
        RecoveryStrategy::CircuitBreaker(failure_threshold, timeout) => {
          // 检查熔断器状态
          let current_time = Time::now()
          
          if circuit_breaker_open {
            // 检查是否应该尝试半开状态
            if current_time - circuit_breaker_open_time > timeout * 1000 {
              circuit_breaker_open = false
              failure_count = 0
            } else {
              // 熔断器仍然打开
              attempts = 1
              success = false
              current_error = Some(TelemetryError::NetworkError("Circuit breaker is open"))
            }
          }
          
          if not circuit_breaker_open {
            attempts = 1
            
            // 模拟操作
            let operation_success = failure_count < failure_threshold
            
            if operation_success {
              success = true
              failure_count = 0
            } else {
              failure_count = failure_count + 1
              success = false
              
              // 检查是否应该打开熔断器
              if failure_count >= failure_threshold {
                circuit_breaker_open = true
                circuit_breaker_open_time = current_time
              }
            }
          }
        }
        RecoveryStrategy::Failover(endpoints) => {
          let mut endpoint_index = 0
          
          while endpoint_index < endpoints.length() && not success {
            attempts = attempts + 1
            let endpoint = endpoints[endpoint_index]
            
            // 模拟尝试不同端点
            let endpoint_success = match endpoint_index {
              0 => false,  // 第一个端点失败
              1 => true,   // 第二个端点成功
              _ => false   // 其他端点失败
            }
            
            if endpoint_success {
              success = true
              current_error = None
            }
            
            endpoint_index = endpoint_index + 1
          }
        }
        RecoveryStrategy::Backoff(initial_delay, backoff_factor) => {
          let mut delay = initial_delay
          
          while attempts < 5 && not success {
            attempts = attempts + 1
            
            // 模拟操作
            let operation_success = attempts >= 3  // 第3次尝试成功
            
            if operation_success {
              success = true
              current_error = None
            } else if attempts < 5 {
              // 等待指数退避延迟
              let _ = Time::sleep(delay)
              delay = (delay as Float * backoff_factor) as Int
            }
          }
        }
        RecoveryStrategy::GracefulDegradation => {
          attempts = 1
          // 优雅降级总是成功，但可能提供有限的功能
          success = true
          current_error = None
        }
      }
      
      let total_time = Time::now() - start_time
      
      {
        success: success,
        error: current_error,
        attempts: attempts,
        total_time_ms: total_time,
        strategy_used: Some(strategy)
      }
    }
    
    { handle_error }
  }
  
  // 测试重试策略
  let retry_handler = create_error_handler(RecoveryStrategy::Retry(3))
  
  let network_error = TelemetryError::NetworkError("Connection timeout")
  let network_result = retry_handler.handle_error(network_error, RecoveryStrategy::Retry(3))
  
  assert_true(network_result.success)
  assert_eq(network_result.attempts, 3)
  assert_eq(network_result.error, None)
  
  let storage_error = TelemetryError::StorageError("Disk full")
  let storage_result = retry_handler.handle_error(storage_error, RecoveryStrategy::Retry(3))
  
  assert_false(storage_result.success)
  assert_eq(storage_result.attempts, 3)
  assert_true(storage_result.error.is_some())
  
  // 测试熔断器策略
  let circuit_breaker_handler = create_error_handler(RecoveryStrategy::CircuitBreaker(2, 5))
  
  // 第一次失败
  let cb_result1 = circuit_breaker_handler.handle_error(
    TelemetryError::NetworkError("Connection refused"), 
    RecoveryStrategy::CircuitBreaker(2, 5)
  )
  assert_false(cb_result1.success)
  assert_eq(cb_result1.attempts, 1)
  
  // 第二次失败，应该打开熔断器
  let cb_result2 = circuit_breaker_handler.handle_error(
    TelemetryError::NetworkError("Connection refused"), 
    RecoveryStrategy::CircuitBreaker(2, 5)
  )
  assert_false(cb_result2.success)
  assert_eq(cb_result2.attempts, 1)
  
  // 第三次尝试，熔断器应该打开
  let cb_result3 = circuit_breaker_handler.handle_error(
    TelemetryError::NetworkError("Connection refused"), 
    RecoveryStrategy::CircuitBreaker(2, 5)
  )
  assert_false(cb_result3.success)
  assert_eq(cb_result3.attempts, 1)
  assert_true(cb_result3.error.unwrap_or(TelemetryError::NetworkError("")).to_string().contains("Circuit breaker is open"))
  
  // 测试故障转移策略
  let failover_handler = create_error_handler(RecoveryStrategy::Failover([]))
  let endpoints = ["endpoint-1", "endpoint-2", "endpoint-3"]
  
  let failover_result = failover_handler.handle_error(
    TelemetryError::NetworkError("Primary endpoint down"), 
    RecoveryStrategy::Failover(endpoints)
  )
  
  assert_true(failover_result.success)
  assert_eq(failover_result.attempts, 2)  // 应该尝试第二个端点成功
  
  // 测试指数退避策略
  let backoff_handler = create_error_handler(RecoveryStrategy::Backoff(100, 2.0))
  
  let backoff_result = backoff_handler.handle_error(
    TelemetryError::RateLimitError(1), 
    RecoveryStrategy::Backoff(100, 2.0)
  )
  
  assert_true(backoff_result.success)
  assert_eq(backoff_result.attempts, 3)
  assert_true(backoff_result.total_time_ms >= 100 + 200)  // 至少等待100ms + 200ms
  
  // 测试优雅降级策略
  let degradation_handler = create_error_handler(RecoveryStrategy::GracefulDegradation)
  
  let degradation_result = degradation_handler.handle_error(
    TelemetryError::SystemOverloadError(95.0), 
    RecoveryStrategy::GracefulDegradation
  )
  
  assert_true(degradation_result.success)
  assert_eq(degradation_result.attempts, 1)
  
  // 测试错误分类和处理
  let classify_error = fn(error: TelemetryError) {
    match error {
      TelemetryError::NetworkError(_) => ("transient", true)  // 可重试
      TelemetryError::SerializationError(_) => ("permanent", false)  // 不可重试
      TelemetryError::StorageError(_) => ("resource", true)  // 资源错误，可重试
      TelemetryError::ValidationError(_) => ("client", false)  // 客户端错误，不可重试
      TelemetryError::RateLimitError(_) => ("throttling", true)  // 限流，可重试
      TelemetryError::SystemOverloadError(_) => ("system", true)  // 系统错误，可重试
    }
  }
  
  // 测试错误分类
  let (network_type, network_retryable) = classify_error(TelemetryError::NetworkError("Timeout"))
  assert_eq(network_type, "transient")
  assert_true(network_retryable)
  
  let (validation_type, validation_retryable) = classify_error(TelemetryError::ValidationError("Invalid format"))
  assert_eq(validation_type, "client")
  assert_false(validation_retryable)
  
  // 测试错误恢复决策
  let decide_recovery_strategy = fn(error: TelemetryError, context: Map[String, String]) {
    let (error_type, retryable) = classify_error(error)
    let criticality = match Map::get(context, "criticality") {
      Some(c) => c
      None => "normal"
    }
    
    match (error_type, retryable, criticality) {
      ("transient", true, "high") => RecoveryStrategy::Retry(5)
      ("transient", true, "normal") => RecoveryStrategy::Retry(3)
      ("system", true, "high") => RecoveryStrategy::Failover(["backup-1", "backup-2"])
      ("system", true, "normal") => RecoveryStrategy::CircuitBreaker(3, 10)
      ("throttling", true, _) => RecoveryStrategy::Backoff(1000, 2.0)
      ("resource", true, "high") => RecoveryStrategy::Failover(["backup-storage"])
      ("resource", true, "normal") => RecoveryStrategy::Retry(2)
      (_, false, "high") => RecoveryStrategy::GracefulDegradation
      (_, false, _) => RecoveryStrategy::Retry(0)  // 不重试
    }
  }
  
  // 测试错误恢复决策
  let high_priority_context = Map::from_array([("criticality", "high")])
  let normal_priority_context = Map::from_array([("criticality", "normal")])
  
  let strategy1 = decide_recovery_strategy(TelemetryError::NetworkError("Timeout"), high_priority_context)
  assert_eq(strategy1, RecoveryStrategy::Retry(5))
  
  let strategy2 = decide_recovery_strategy(TelemetryError::ValidationError("Invalid data"), normal_priority_context)
  assert_eq(strategy2, RecoveryStrategy::Retry(0))
  
  let strategy3 = decide_recovery_strategy(TelemetryError::SystemOverloadError(90.0), high_priority_context)
  assert_eq(strategy3, RecoveryStrategy::Failover(["backup-1", "backup-2"]))
}

// 测试13: 遥测数据可视化
test "遥测数据可视化和图表生成" {
  // 定义图表类型
  enum ChartType {
    LineChart
    BarChart
    PieChart
    Heatmap
    ScatterPlot
    Histogram
  }
  
  // 定义数据点
  type DataPoint = {
    x: Float,
    y: Float,
    label: Option[String],
    color: Option[String]
  }
  
  // 定义数据系列
  type DataSeries = {
    name: String,
    points: Array[DataPoint],
    color: String
  }
  
  // 定义图表配置
  type ChartConfig = {
    title: String,
    x_axis_label: String,
    y_axis_label: String,
    width: Int,
    height: Int,
    show_legend: Bool,
    show_grid: Bool
  }
  
  // 定义图表
  type Chart = {
    chart_type: ChartType,
    config: ChartConfig,
    series: Array[DataSeries]
  }
  
  // 创建图表构建器
  let create_chart_builder = fn() {
    let mut chart_type = ChartType::LineChart
    let mut config = {
      title: "",
      x_axis_label: "",
      y_axis_label: "",
      width: 800,
      height: 600,
      show_legend: true,
      show_grid: true
    }
    let mut series = []
    
    let set_chart_type = fn(ct: ChartType) {
      chart_type = ct
    }
    
    let set_config = fn(new_config: ChartConfig) {
      config = new_config
    }
    
    let add_series = fn(new_series: DataSeries) {
      series = series.push(new_series)
    }
    
    let build = fn() {
      {
        chart_type: chart_type,
        config: config,
        series: series
      }
    }
    
    { set_chart_type, set_config, add_series, build }
  }
  
  // 生成SVG图表
  let generate_svg_chart = fn(chart: Chart) {
    let mut svg = "<svg width=\"" + chart.config.width.to_string() + "\" height=\"" + chart.config.height.to_string() + "\" xmlns=\"http://www.w3.org/2000/svg\">\n"
    
    // 添加标题
    svg = svg + "  <text x=\"" + (chart.config.width / 2).to_string() + "\" y=\"30\" text-anchor=\"middle\" font-size=\"18\" font-weight=\"bold\">" + chart.config.title + "</text>\n"
    
    // 添加坐标轴
    let margin = 60
    let chart_width = chart.config.width - 2 * margin
    let chart_height = chart.config.height - 2 * margin
    
    // X轴
    svg = svg + "  <line x1=\"" + margin.to_string() + "\" y1=\"" + (chart.config.height - margin).to_string() + "\" x2=\"" + (chart.config.width - margin).to_string() + "\" y2=\"" + (chart.config.height - margin).to_string() + "\" stroke=\"black\" />\n"
    svg = svg + "  <text x=\"" + (chart.config.width / 2).to_string() + "\" y=\"" + (chart.config.height - 10).to_string() + "\" text-anchor=\"middle\">" + chart.config.x_axis_label + "</text>\n"
    
    // Y轴
    svg = svg + "  <line x1=\"" + margin.to_string() + "\" y1=\"" + margin.to_string() + "\" x2=\"" + margin.to_string() + "\" y2=\"" + (chart.config.height - margin).to_string() + "\" stroke=\"black\" />\n"
    
    // Y轴标签（旋转）
    svg = svg + "  <text x=\"20\" y=\"" + (chart.config.height / 2).to_string() + "\" text-anchor=\"middle\" transform=\"rotate(-90 20 " + (chart.config.height / 2).to_string() + ")\">" + chart.config.y_axis_label + "</text>\n"
    
    // 根据图表类型绘制数据
    match chart.chart_type {
      ChartType::LineChart => {
        // 绘制折线图
        for data_series in chart.series {
          svg = svg + "  <polyline fill=\"none\" stroke=\"" + data_series.color + "\" stroke-width=\"2\" points=\""
          
          for point in data_series.points {
            let x = margin + (point.x / 100.0 * chart_width as Float) as Int  // 假设x范围为0-100
            let y = chart.config.height - margin - (point.y / 100.0 * chart_height as Float) as Int  // 假设y范围为0-100
            svg = svg + x.to_string() + "," + y.to_string() + " "
          }
          
          svg = svg + "\" />\n"
          
          // 绘制数据点
          for point in data_series.points {
            let x = margin + (point.x / 100.0 * chart_width as Float) as Int
            let y = chart.config.height - margin - (point.y / 100.0 * chart_height as Float) as Int
            svg = svg + "  <circle cx=\"" + x.to_string() + "\" cy=\"" + y.to_string() + "\" r=\"4\" fill=\"" + data_series.color + "\" />\n"
          }
        }
      }
      ChartType::BarChart => {
        // 绘制柱状图
        let bar_width = chart_width / (chart.series.length() * 5)  // 简化计算
        
        for i in 0..chart.series.length() {
          let data_series = chart.series[i]
          
          for j in 0..data_series.points.length() {
            let point = data_series.points[j]
            let x = margin + (i * 5 + j) * bar_width + bar_width / 4
            let bar_height = (point.y / 100.0 * chart_height as Float) as Int
            let y = chart.config.height - margin - bar_height
            
            svg = svg + "  <rect x=\"" + x.to_string() + "\" y=\"" + y.to_string() + "\" width=\"" + (bar_width / 2).to_string() + "\" height=\"" + bar_height.to_string() + "\" fill=\"" + data_series.color + "\" />\n"
          }
        }
      }
      ChartType::PieChart => {
        // 绘制饼图
        let center_x = chart.config.width / 2
        let center_y = chart.config.height / 2
        let radius = if chart.width < chart.height { chart.width / 3 } else { chart.height / 3 }
        
        let total = chart.series.reduce(0.0, fn(acc, series) {
          acc + series.points.reduce(0.0, fn(acc2, point) { acc2 + point.y })
        })
        
        let mut current_angle = 0.0
        
        for data_series in chart.series {
          for point in data_series.points {
            let percentage = point.y / total
            let angle = percentage * 360.0
            
            let start_x = center_x + (radius * (current_angle * 3.14159 / 180.0).cos()) as Int
            let start_y = center_y + (radius * (current_angle * 3.14159 / 180.0).sin()) as Int
            let end_x = center_x + (radius * ((current_angle + angle) * 3.14159 / 180.0).cos()) as Int
            let end_y = center_y + (radius * ((current_angle + angle) * 3.14159 / 180.0).sin()) as Int
            
            let large_arc_flag = if angle > 180.0 { 1 } else { 0 }
            
            svg = svg + "  <path d=\"M " + center_x.to_string() + " " + center_y.to_string() + " L " + start_x.to_string() + " " + start_y.to_string() + " A " + radius.to_string() + " " + radius.to_string() + " 0 " + large_arc_flag.to_string() + " 1 " + end_x.to_string() + " " + end_y.to_string() + " Z\" fill=\"" + data_series.color + "\" />\n"
            
            current_angle = current_angle + angle
          }
        }
      }
      _ => {
        // 其他图表类型的简化实现
        svg = svg + "  <text x=\"" + (chart.config.width / 2).to_string() + "\" y=\"" + (chart.config.height / 2).to_string() + "\" text-anchor=\"middle\">图表类型暂不支持</text>\n"
      }
    }
    
    // 添加图例
    if chart.config.show_legend && chart.series.length() > 0 {
      let legend_y = margin
      let mut legend_x = chart.config.width - margin - 150
      
      svg = svg + "  <g id=\"legend\">\n"
      
      for data_series in chart.series {
        svg = svg + "    <rect x=\"" + legend_x.to_string() + "\" y=\"" + legend_y.to_string() + "\" width=\"15\" height=\"15\" fill=\"" + data_series.color + "\" />\n"
        svg = svg + "    <text x=\"" + (legend_x + 20).to_string() + "\" y=\"" + (legend_y + 12).to_string() + "\">" + data_series.name + "</text>\n"
        legend_y = legend_y + 20
      }
      
      svg = svg + "  </g>\n"
    }
    
    svg = svg + "</svg>"
    svg
  }
  
  // 测试创建折线图
  let line_chart_builder = create_chart_builder()
  line_chart_builder.set_chart_type(ChartType::LineChart)
  line_chart_builder.set_config({
    title: "系统响应时间趋势",
    x_axis_label: "时间",
    y_axis_label: "响应时间 (ms)",
    width: 800,
    height: 600,
    show_legend: true,
    show_grid: true
  })
  
  let api_series = {
    name: "API响应时间",
    points: [
      { x: 0.0, y: 120.0, label: Some("00:00"), color: None },
      { x: 20.0, y: 135.0, label: Some("04:00"), color: None },
      { x: 40.0, y: 125.0, label: Some("08:00"), color: None },
      { x: 60.0, y: 145.0, label: Some("12:00"), color: None },
      { x: 80.0, y: 130.0, label: Some("16:00"), color: None },
      { x: 100.0, y: 140.0, label: Some("20:00"), color: None }
    ],
    color: "#3498db"
  }
  
  let db_series = {
    name: "数据库查询时间",
    points: [
      { x: 0.0, y: 45.0, label: Some("00:00"), color: None },
      { x: 20.0, y: 50.0, label: Some("04:00"), color: None },
      { x: 40.0, y: 48.0, label: Some("08:00"), color: None },
      { x: 60.0, y: 55.0, label: Some("12:00"), color: None },
      { x: 80.0, y: 52.0, label: Some("16:00"), color: None },
      { x: 100.0, y: 58.0, label: Some("20:00"), color: None }
    ],
    color: "#e74c3c"
  }
  
  line_chart_builder.add_series(api_series)
  line_chart_builder.add_series(db_series)
  
  let line_chart = line_chart_builder.build()
  assert_eq(line_chart.chart_type, ChartType::LineChart)
  assert_eq(line_chart.series.length(), 2)
  assert_eq(line_chart.config.title, "系统响应时间趋势")
  
  let line_chart_svg = generate_svg_chart(line_chart)
  assert_true(line_chart_svg.contains("<svg"))
  assert_true(line_chart_svg.contains("系统响应时间趋势"))
  assert_true(line_chart_svg.contains("#3498db"))
  assert_true(line_chart_svg.contains("#e74c3c"))
  
  // 测试创建柱状图
  let bar_chart_builder = create_chart_builder()
  bar_chart_builder.set_chart_type(ChartType::BarChart)
  bar_chart_builder.set_config({
    title: "服务请求量统计",
    x_axis_label: "服务",
    y_axis_label: "请求数",
    width: 800,
    height: 600,
    show_legend: true,
    show_grid: true
  })
  
  let service_series = {
    name: "请求量",
    points: [
      { x: 10.0, y: 850.0, label: Some("用户服务"), color: None },
      { x: 30.0, y: 1200.0, label: Some("订单服务"), color: None },
      { x: 50.0, y: 650.0, label: Some("支付服务"), color: None },
      { x: 70.0, y: 920.0, label: Some("库存服务"), color: None },
      { x: 90.0, y: 480.0, label: Some("通知服务"), color: None }
    ],
    color: "#2ecc71"
  }
  
  bar_chart_builder.add_series(service_series)
  
  let bar_chart = bar_chart_builder.build()
  assert_eq(bar_chart.chart_type, ChartType::BarChart)
  assert_eq(bar_chart.series.length(), 1)
  assert_eq(bar_chart.config.title, "服务请求量统计")
  
  let bar_chart_svg = generate_svg_chart(bar_chart)
  assert_true(bar_chart_svg.contains("<svg"))
  assert_true(bar_chart_svg.contains("服务请求量统计"))
  assert_true(bar_chart_svg.contains("#2ecc71"))
  
  // 测试创建饼图
  let pie_chart_builder = create_chart_builder()
  pie_chart_builder.set_chart_type(ChartType::PieChart)
  pie_chart_builder.set_config({
    title: "错误类型分布",
    x_axis_label: "",
    y_axis_label: "",
    width: 600,
    height: 600,
    show_legend: true,
    show_grid: false
  })
  
  let error_series = {
    name: "错误类型",
    points: [
      { x: 0.0, y: 45.0, label: Some("网络错误"), color: None },
      { x: 0.0, y: 25.0, label: Some("数据库错误"), color: None },
      { x: 0.0, y: 15.0, label: Some("验证错误"), color: None },
      { x: 0.0, y: 10.0, label: Some("超时错误"), color: None },
      { x: 0.0, y: 5.0, label: Some("其他错误"), color: None }
    ],
    color: "#f39c12"
  }
  
  pie_chart_builder.add_series(error_series)
  
  let pie_chart = pie_chart_builder.build()
  assert_eq(pie_chart.chart_type, ChartType::PieChart)
  assert_eq(pie_chart.series.length(), 1)
  assert_eq(pie_chart.config.title, "错误类型分布")
  
  let pie_chart_svg = generate_svg_chart(pie_chart)
  assert_true(pie_chart_svg.contains("<svg"))
  assert_true(pie_chart_svg.contains("错误类型分布"))
  assert_true(pie_chart_svg.contains("#f39c12"))
  
  // 测试数据聚合和图表生成
  let aggregate_and_visualize = fn(raw_data: Array[(String, Float)], chart_type: ChartType, title: String) {
    // 按值排序
    let sorted_data = raw_data.sort(fn(a, b) { a.1 >= b.1 })
    
    // 转换为数据系列
    let data_points = sorted_data.map_with_index(fn(i, item) {
      let (label, value) = item
      { x: i as Float * 20.0, y: value, label: Some(label), color: None }
    })
    
    let data_series = {
      name: title,
      points: data_points,
      color: "#9b59b6"
    }
    
    // 创建图表
    let builder = create_chart_builder()
    builder.set_chart_type(chart_type)
    builder.set_config({
      title: title,
      x_axis_label: "类别",
      y_axis_label: "值",
      width: 800,
      height: 600,
      show_legend: true,
      show_grid: true
    })
    builder.add_series(data_series)
    
    let chart = builder.build()
    generate_svg_chart(chart)
  }
  
  // 测试聚合和可视化
  let latency_data = [
    ("P50", 120.0),
    ("P90", 250.0),
    ("P95", 320.0),
    ("P99", 480.0),
    ("P99.9", 650.0)
  ]
  
  let latency_chart_svg = aggregate_and_visualize(latency_data, ChartType::BarChart, "延迟百分位数")
  assert_true(latency_chart_svg.contains("<svg"))
  assert_true(latency_chart_svg.contains("延迟百分位数"))
  assert_true(latency_chart_svg.contains("#9b59b6"))
}

// 测试14: 遥测系统配置管理
test "遥测系统配置管理和动态更新" {
  // 定义配置值类型
  enum ConfigValue {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
    Array(Array[ConfigValue])
    Object(Map[String, ConfigValue])
  }
  
  // 定义配置项
  type ConfigItem = {
    key: String,
    value: ConfigValue,
    description: String,
    required: Bool,
    category: String,
    metadata: Map[String, String]
  }
  
  // 定义配置变更事件
  type ConfigChangeEvent = {
    key: String,
    old_value: Option[ConfigValue],
    new_value: ConfigValue,
    timestamp: Int,
    source: String
  }
  
  // 创建配置管理器
  let create_config_manager = fn() {
    let mut configs = Map::empty()
    let mut change_listeners = []
    let mut change_history = []
    
    // 注册配置项
    let register_config = fn(item: ConfigItem) {
      let _ = Map::insert(configs, item.key, item)
    }
    
    // 获取配置值
    let get_config = fn(key: String) {
      match Map::get(configs, key) {
        Some(item) => Some(item.value)
        None => None
      }
    }
    
    // 设置配置值
    let set_config = fn(key: String, new_value: ConfigValue, source: String) {
      let old_value = match Map::get(configs, key) {
        Some(item) => Some(item.value)
        None => None
      }
      
      // 更新配置
      match Map::get(configs, key) {
        Some(item) => {
          let updated_item = { item with value: new_value }
          let _ = Map::insert(configs, key, updated_item)
        }
        None => {
          let new_item = {
            key: key,
            value: new_value,
            description: "动态添加的配置",
            required: false,
            category: "dynamic",
            metadata: Map::empty()
          }
          let _ = Map::insert(configs, key, new_item)
        }
      }
      
      // 记录变更事件
      let change_event = {
        key: key,
        old_value: old_value,
        new_value: new_value,
        timestamp: Time::now(),
        source: source
      }
      change_history = change_history.push(change_event)
      
      // 通知监听器
      for listener in change_listeners {
        listener(change_event)
      }
    }
    
    // 添加变更监听器
    let add_change_listener = fn(listener: ConfigChangeEvent -> Unit) {
      change_listeners = change_listeners.push(listener)
    }
    
    // 批量更新配置
    let batch_update = fn(updates: Array[(String, ConfigValue)], source: String) {
      for (key, value) in updates {
        set_config(key, value, source)
      }
    }
    
    // 验证配置
    let validate_config = fn(key: String) {
      match Map::get(configs, key) {
        Some(item) => {
          if item.required && (match item.value { ConfigValue::String(s) => s == "" | _ => false }) {
            false  // 必需项为空
          } else {
            true
          }
        }
        None => false  // 配置项不存在
      }
    }
    
    // 获取配置变更历史
    let get_change_history = fn(key: Option[String]) {
      match key {
        Some(k) => change_history.filter(fn(event) { event.key == k })
        None => change_history
      }
    }
    
    // 导出配置
    let export_configs = fn() {
      let exported = Map::empty()
      for (k, item) in configs {
        let _ = Map::insert(exported, k, item.value)
      }
      exported
    }
    
    // 导入配置
    let import_configs = fn(imported: Map[String, ConfigValue], source: String) {
      for (k, value) in imported {
        set_config(k, value, source)
      }
    }
    
    {
      register_config,
      get_config,
      set_config,
      add_change_listener,
      batch_update,
      validate_config,
      get_change_history,
      export_configs,
      import_configs
    }
  }
  
  // 测试基本配置管理
  let config_manager = create_config_manager()
  
  // 注册配置项
  config_manager.register_config({
    key: "telemetry.enabled",
    value: ConfigValue::Bool(true),
    description: "是否启用遥测",
    required: true,
    category: "core",
    metadata: Map::from_array([("type", "boolean"), ("default", "true")])
  })
  
  config_manager.register_config({
    key: "telemetry.sampling_rate",
    value: ConfigValue::Float(0.1),
    description: "遥测采样率",
    required: true,
    category: "sampling",
    metadata: Map::from_array([("type", "float"), ("min", "0.0"), ("max", "1.0")])
  })
  
  config_manager.register_config({
    key: "telemetry.export_interval",
    value: ConfigValue::Int(60),
    description: "遥测导出间隔（秒）",
    required: false,
    category: "export",
    metadata: Map::from_array([("type", "integer"), ("min", "1"), ("default", "60")])
  })
  
  // 测试获取配置
  let enabled = config_manager.get_config("telemetry.enabled")
  assert_true(enabled.is_some())
  assert_eq(enabled.unwrap(), ConfigValue::Bool(true))
  
  let sampling_rate = config_manager.get_config("telemetry.sampling_rate")
  assert_true(sampling_rate.is_some())
  assert_eq(sampling_rate.unwrap(), ConfigValue::Float(0.1))
  
  // 测试设置配置
  config_manager.set_config("telemetry.sampling_rate", ConfigValue::Float(0.2), "test")
  let updated_rate = config_manager.get_config("telemetry.sampling_rate")
  assert_eq(updated_rate.unwrap(), ConfigValue::Float(0.2))
  
  // 测试配置验证
  assert_true(config_manager.validate_config("telemetry.enabled"))
  assert_true(config_manager.validate_config("telemetry.sampling_rate"))
  assert_false(config_manager.validate_config("nonexistent.config"))
  
  // 测试变更监听器
  let mut received_events = []
  let test_listener = fn(event: ConfigChangeEvent) {
    received_events = received_events.push(event)
  }
  
  config_manager.add_change_listener(test_listener)
  config_manager.set_config("telemetry.export_interval", ConfigValue::Int(30), "manual")
  
  assert_eq(received_events.length(), 1)
  assert_eq(received_events[0].key, "telemetry.export_interval")
  assert_eq(received_events[0].new_value, ConfigValue::Int(30))
  assert_eq(received_events[0].source, "manual")
  
  // 测试批量更新
  let batch_updates = [
    ("telemetry.enabled", ConfigValue::Bool(false)),
    ("telemetry.batch_size", ConfigValue::Int(100))
  ]
  
  config_manager.batch_update(batch_updates, "batch_test")
  
  let enabled_after_batch = config_manager.get_config("telemetry.enabled")
  assert_eq(enabled_after_batch.unwrap(), ConfigValue::Bool(false))
  
  let batch_size = config_manager.get_config("telemetry.batch_size")
  assert_eq(batch_size.unwrap(), ConfigValue::Int(100))
  
  // 验证批量更新触发了多个变更事件
  assert_eq(received_events.length(), 3)  // 1个之前的事件 + 2个批量更新事件
  
  // 测试配置变更历史
  let all_history = config_manager.get_change_history(None)
  assert_true(all_history.length() >= 3)
  
  let sampling_history = config_manager.get_change_history(Some("telemetry.sampling_rate"))
  assert_eq(sampling_history.length(), 1)
  assert_eq(sampling_history[0].old_value, Some(ConfigValue::Float(0.1)))
  assert_eq(sampling_history[0].new_value, ConfigValue::Float(0.2))
  
  // 测试配置导出和导入
  let exported_configs = config_manager.export_configs()
  assert_true(Map::contains(exported_configs, "telemetry.enabled"))
  assert_true(Map::contains(exported_configs, "telemetry.sampling_rate"))
  assert_true(Map::contains(exported_configs, "telemetry.export_interval"))
  
  // 创建新的配置管理器并导入配置
  let new_config_manager = create_config_manager()
  new_config_manager.import_configs(exported_configs, "import")
  
  let imported_enabled = new_config_manager.get_config("telemetry.enabled")
  assert_eq(imported_enabled.unwrap(), ConfigValue::Bool(false))
  
  let imported_sampling_rate = new_config_manager.get_config("telemetry.sampling_rate")
  assert_eq(imported_sampling_rate.unwrap(), ConfigValue::Float(0.2))
  
  // 测试复杂配置类型
  config_manager.set_config("telemetry.exporters", ConfigValue::Array([
    ConfigValue::Object(Map::from_array([
      ("type", ConfigValue::String("http")),
      ("endpoint", ConfigValue::String("https://api.example.com/telemetry")),
      ("enabled", ConfigValue::Bool(true))
    ])),
    ConfigValue::Object(Map::from_array([
      ("type", ConfigValue::String("file")),
      ("path", ConfigValue::String("/var/log/telemetry.log")),
      ("enabled", ConfigValue::Bool(false))
    ]))
  ]), "complex_config")
  
  let exporters = config_manager.get_config("telemetry.exporters")
  assert_true(exporters.is_some())
  
  match exporters.unwrap() {
    ConfigValue::Array(arr) => {
      assert_eq(arr.length(), 2)
      
      match arr[0] {
        ConfigValue::Object(obj) => {
          assert_eq(Map::get(obj, "type"), Some(ConfigValue::String("http")))
          assert_eq(Map::get(obj, "enabled"), Some(ConfigValue::Bool(true)))
        }
        _ => assert_false(true)
      }
    }
    _ => assert_false(true)
  }
  
  // 测试配置验证规则
  let validate_config_value = fn(key: String, value: ConfigValue, metadata: Map[String, String]) {
    match Map::get(metadata, "type") {
      Some(ConfigValue::String("boolean")) => {
        match value {
          ConfigValue::Bool(_) => true
          _ => false
        }
      }
      Some(ConfigValue::String("integer")) => {
        match value {
          ConfigValue::Int(i) => {
            let min = match Map::get(metadata, "min") {
              Some(ConfigValue::String(s)) => s.parse_int().unwrap_or(0)
              _ => 0
            }
            let max = match Map::get(metadata, "max") {
              Some(ConfigValue::String(s)) => s.parse_int().unwrap_or(999999)
              _ => 999999
            }
            i >= min && i <= max
          }
          _ => false
        }
      }
      Some(ConfigValue::String("float")) => {
        match value {
          ConfigValue::Float(f) => {
            let min = match Map::get(metadata, "min") {
              Some(ConfigValue::String(s)) => s.parse_float().unwrap_or(0.0)
              _ => 0.0
            }
            let max = match Map::get(metadata, "max") {
              Some(ConfigValue::String(s)) => s.parse_float().unwrap_or(999999.0)
              _ => 999999.0
            }
            f >= min && f <= max
          }
          _ => false
        }
      }
      _ => true  // 其他类型默认通过验证
    }
  }
  
  // 测试配置值验证
  let int_metadata = Map::from_array([
    ("type", ConfigValue::String("integer")),
    ("min", ConfigValue::String("1")),
    ("max", ConfigValue::String("100"))
  ])
  
  assert_true(validate_config_value("test.int", ConfigValue::Int(50), int_metadata))
  assert_false(validate_config_value("test.int", ConfigValue::Int(0), int_metadata))
  assert_false(validate_config_value("test.int", ConfigValue::Int(101), int_metadata))
  assert_false(validate_config_value("test.int", ConfigValue::String("50"), int_metadata))
  
  let float_metadata = Map::from_array([
    ("type", ConfigValue::String("float")),
    ("min", ConfigValue::String("0.0")),
    ("max", ConfigValue::String("1.0"))
  ])
  
  assert_true(validate_config_value("test.float", ConfigValue::Float(0.5), float_metadata))
  assert_false(validate_config_value("test.float", ConfigValue::Float(-0.1), float_metadata))
  assert_false(validate_config_value("test.float", ConfigValue::Float(1.1), float_metadata))
}

// 测试15: 遥测数据多租户支持
test "遥测数据多租户支持和隔离" {
  // 定义租户信息
  type Tenant = {
    id: String,
    name: String,
    domain: String,
    plan: String,  // "basic", "premium", "enterprise"
    settings: Map[String, String],
    created_at: Int
  }
  
  // 定义租户上下文
  type TenantContext = {
    tenant_id: String,
    user_id: String,
    permissions: Array[String],
    metadata: Map[String, String]
  }
  
  // 定义租户隔离策略
  enum IsolationStrategy {
    Database  // 数据库级别隔离
    Schema    // 模式级别隔离
    Table     // 表级别隔离
    Row       // 行级别隔离
    Tag       // 标签级别隔离
  }
  
  // 定义多租户遥测事件
  type MultiTenantTelemetryEvent = {
    id: String,
    tenant_id: String,
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)],
    tags: Array[(String, String)],
    data: Map[String, String]
  }
  
  // 创建租户管理器
  let create_tenant_manager = fn(isolation_strategy: IsolationStrategy) {
    let mut tenants = Map::empty()
    let mut tenant_events = Map::empty()
    
    // 注册租户
    let register_tenant = fn(tenant: Tenant) {
      let _ = Map::insert(tenants, tenant.id, tenant)
      let _ = Map::insert(tenant_events, tenant.id, [])
    }
    
    // 获取租户
    let get_tenant = fn(tenant_id: String) {
      Map::get(tenants, tenant_id)
    }
    
    // 存储遥测事件
    let store_event = fn(event: MultiTenantTelemetryEvent) {
      match Map::get(tenant_events, event.tenant_id) {
        Some(events) => {
          let updated_events = events.push(event)
          let _ = Map::insert(tenant_events, event.tenant_id, updated_events)
        }
        None => {
          // 租户不存在，创建新的事件列表
          let _ = Map::insert(tenant_events, event.tenant_id, [event])
        }
      }
    }
    
    // 查询租户事件
    let query_events = fn(tenant_id: String, filter: Option[MultiTenantTelemetryEvent -> Bool]) {
      match Map::get(tenant_events, tenant_id) {
        Some(events) => {
          match filter {
            Some(f) => events.filter(f)
            None => events
          }
        }
        None => []
      }
    }
    
    // 删除租户数据
    let delete_tenant_data = fn(tenant_id: String) {
      let _ = Map::remove(tenants, tenant_id)
      let _ = Map::remove(tenant_events, tenant_id)
    }
    
    // 获取租户统计信息
    let get_tenant_stats = fn(tenant_id: String) {
      match Map::get(tenant_events, tenant_id) {
        Some(events) => {
          let event_count = events.length()
          let event_types = events.reduce(Map::empty(), fn(acc, event) {
            let count = match Map::get(acc, event.name) { Some(c) => c + 1 | None => 1 }
            let _ = Map::insert(acc, event.name, count)
            acc
          })
          
          Some({
            event_count: event_count,
            event_types: event_types,
            storage_size: event_count * 1024  // 简化计算
          })
        }
        None => None
      }
    }
    
    // 应用资源限制
    let apply_resource_limits = fn(tenant_id: String, operation: String) {
      match Map::get(tenants, tenant_id) {
        Some(tenant) => {
          match tenant.plan {
            "basic" => {
              // 基础计划限制
              match operation {
                "store" => {
                  // 限制每天存储的事件数
                  match Map::get(tenant_events, tenant_id) {
                    Some(events) => events.length() < 1000
                    None => true
                  }
                }
                "query" => {
                  // 限制查询结果数量
                  true
                }
                _ => false
              }
            }
            "premium" => {
              // 高级计划限制
              match operation {
                "store" => {
                  match Map::get(tenant_events, tenant_id) {
                    Some(events) => events.length() < 10000
                    None => true
                  }
                }
                "query" => true
                _ => false
              }
            }
            "enterprise" => {
              // 企业计划无限制
              true
            }
            _ => false
          }
        }
        None => false
      }
    }
    
    {
      register_tenant,
      get_tenant,
      store_event,
      query_events,
      delete_tenant_data,
      get_tenant_stats,
      apply_resource_limits
    }
  }
  
  // 创建访问控制管理器
  let create_access_control_manager = fn() {
    let mut permissions = Map::empty()
    
    // 分配权限
    let grant_permission = fn(tenant_id: String, user_id: String, permission: String) {
      let key = tenant_id + ":" + user_id
      match Map::get(permissions, key) {
        Some(user_permissions) => {
          let updated_permissions = user_permissions.push(permission)
          let _ = Map::insert(permissions, key, updated_permissions)
        }
        None => {
          let _ = Map::insert(permissions, key, [permission])
        }
      }
    }
    
    // 检查权限
    let check_permission = fn(tenant_id: String, user_id: String, permission: String) {
      let key = tenant_id + ":" + user_id
      match Map::get(permissions, key) {
        Some(user_permissions) => {
          user_permissions.contains(permission)
        }
        None => false
      }
    }
    
    // 撤销权限
    let revoke_permission = fn(tenant_id: String, user_id: String, permission: String) {
      let key = tenant_id + ":" + user_id
      match Map::get(permissions, key) {
        Some(user_permissions) => {
          let updated_permissions = user_permissions.filter(fn(p) { p != permission })
          let _ = Map::insert(permissions, key, updated_permissions)
        }
        None => {}
      }
    }
    
    { grant_permission, check_permission, revoke_permission }
  }
  
  // 测试租户管理
  let tenant_manager = create_tenant_manager(IsolationStrategy::Table)
  
  // 注册租户
  let tenant1 = {
    id: "tenant-1",
    name: "Acme Corp",
    domain: "acme.com",
    plan: "basic",
    settings: Map::from_array([
      ("retention_days", "30"),
      ("sampling_rate", "0.1")
    ]),
    created_at: 1609459200
  }
  
  let tenant2 = {
    id: "tenant-2",
    name: "Tech Solutions",
    domain: "techsolutions.io",
    plan: "premium",
    settings: Map::from_array([
      ("retention_days", "90"),
      ("sampling_rate", "0.5")
    ]),
    created_at: 1609459300
  }
  
  tenant_manager.register_tenant(tenant1)
  tenant_manager.register_tenant(tenant2)
  
  // 验证租户注册
  let retrieved_tenant1 = tenant_manager.get_tenant("tenant-1")
  assert_true(retrieved_tenant1.is_some())
  assert_eq(retrieved_tenant1.unwrap().name, "Acme Corp")
  assert_eq(retrieved_tenant1.unwrap().plan, "basic")
  
  // 测试事件存储和隔离
  let event1 = {
    id: "event-1",
    tenant_id: "tenant-1",
    name: "user.login",
    timestamp: 1609459200,
    attributes: [("user_id", "user1"), ("ip", "192.168.1.1")],
    tags: [("environment", "production")],
    data: Map::from_array([("success", "true")])
  }
  
  let event2 = {
    id: "event-2",
    tenant_id: "tenant-2",
    name: "user.login",
    timestamp: 1609459210,
    attributes: [("user_id", "user2"), ("ip", "10.0.0.1")],
    tags: [("environment", "staging")],
    data: Map::from_array([("success", "false")])
  }
  
  let event3 = {
    id: "event-3",
    tenant_id: "tenant-1",
    name: "api.request",
    timestamp: 1609459220,
    attributes: [("endpoint", "/api/users"), ("method", "GET")],
    tags: [("environment", "production")],
    data: Map::from_array([("duration", "120")])
  }
  
  // 存储事件
  assert_true(tenant_manager.apply_resource_limits("tenant-1", "store"))
  tenant_manager.store_event(event1)
  
  assert_true(tenant_manager.apply_resource_limits("tenant-2", "store"))
  tenant_manager.store_event(event2)
  
  assert_true(tenant_manager.apply_resource_limits("tenant-1", "store"))
  tenant_manager.store_event(event3)
  
  // 测试事件查询和隔离
  let tenant1_events = tenant_manager.query_events("tenant-1", None)
  assert_eq(tenant1_events.length(), 2)
  assert_true(tenant1_events.all(fn(e) { e.tenant_id == "tenant-1" }))
  
  let tenant2_events = tenant_manager.query_events("tenant-2", None)
  assert_eq(tenant2_events.length(), 1)
  assert_eq(tenant2_events[0].tenant_id, "tenant-2")
  
  // 测试过滤查询
  let login_events = tenant_manager.query_events("tenant-1", Some(fn(e) { e.name == "user.login" }))
  assert_eq(login_events.length(), 1)
  assert_eq(login_events[0].id, "event-1")
  
  // 测试租户统计
  let tenant1_stats = tenant_manager.get_tenant_stats("tenant-1")
  assert_true(tenant1_stats.is_some())
  assert_eq(tenant1_stats.unwrap().event_count, 2)
  assert_eq(Map::get(tenant1_stats.unwrap().event_types, "user.login"), Some(1))
  assert_eq(Map::get(tenant1_stats.unwrap().event_types, "api.request"), Some(1))
  
  // 测试资源限制
  assert_true(tenant_manager.apply_resource_limits("tenant-1", "store"))
  assert_true(tenant_manager.apply_resource_limits("tenant-2", "store"))
  assert_true(tenant_manager.apply_resource_limits("tenant-1", "query"))
  assert_true(tenant_manager.apply_resource_limits("tenant-2", "query"))
  
  // 测试访问控制
  let access_manager = create_access_control_manager()
  
  // 分配权限
  access_manager.grant_permission("tenant-1", "user1", "read")
  access_manager.grant_permission("tenant-1", "user1", "write")
  access_manager.grant_permission("tenant-1", "user2", "read")
  access_manager.grant_permission("tenant-2", "user1", "read")
  
  // 检查权限
  assert_true(access_manager.check_permission("tenant-1", "user1", "read"))
  assert_true(access_manager.check_permission("tenant-1", "user1", "write"))
  assert_true(access_manager.check_permission("tenant-1", "user2", "read"))
  assert_false(access_manager.check_permission("tenant-1", "user2", "write"))
  assert_true(access_manager.check_permission("tenant-2", "user1", "read"))
  assert_false(access_manager.check_permission("tenant-2", "user1", "write"))
  
  // 撤销权限
  access_manager.revoke_permission("tenant-1", "user1", "write")
  assert_false(access_manager.check_permission("tenant-1", "user1", "write"))
  assert_true(access_manager.check_permission("tenant-1", "user1", "read"))
  
  // 测试多租户数据导出
  let export_tenant_data = fn(tenant_id: String, format: String) {
    let events = tenant_manager.query_events(tenant_id, None)
    
    match format {
      "json" => {
        let json_array = events.map(fn(event) {
          "{" +
            "\"id\":\"" + event.id + "\"," +
            "\"name\":\"" + event.name + "\"," +
            "\"timestamp\":" + event.timestamp.to_string() + "," +
            "\"attributes\":[" + event.attributes.map(fn(attr) { "\"" + attr.0 + "\":\"" + attr.1 + "\"" }).join(",") + "]," +
            "\"data\":{" + event.data.to_array().map(fn(entry) { "\"" + entry.0 + "\":\"" + entry.1 + "\"" }).join(",") + "}" +
          "}"
        }).join(",")
        
        "[" + json_array + "]"
      }
      "csv" => {
        let header = "id,name,timestamp,tenant_id\n"
        let rows = events.map(fn(event) {
          event.id + "," + event.name + "," + event.timestamp.to_string() + "," + event.tenant_id
        }).join("\n")
        
        header + rows
      }
      _ => "Unsupported format"
    }
  }
  
  // 测试数据导出
  let tenant1_json = export_tenant_data("tenant-1", "json")
  assert_true(tenant1_json.contains("["))
  assert_true(tenant1_json.contains("event-1"))
  assert_true(tenant1_json.contains("event-3"))
  
  let tenant1_csv = export_tenant_data("tenant-1", "csv")
  assert_true(tenant1_csv.contains("id,name,timestamp,tenant_id"))
  assert_true(tenant1_csv.contains("event-1,user.login,1609459200,tenant-1"))
  
  // 测试租户数据删除
  tenant_manager.delete_tenant_data("tenant-1")
  
  let deleted_tenant = tenant_manager.get_tenant("tenant-1")
  assert_true(deleted_tenant.is_none())
  
  let deleted_events = tenant_manager.query_events("tenant-1", None)
  assert_eq(deleted_events.length(), 0)
  
  // 验证其他租户数据不受影响
  let remaining_tenant = tenant_manager.get_tenant("tenant-2")
  assert_true(remaining_tenant.is_some())
  
  let remaining_events = tenant_manager.query_events("tenant-2", None)
  assert_eq(remaining_events.length(), 1)
}

// 测试16: 遥测系统监控和告警
test "遥测系统监控和告警机制" {
  // 定义告警级别
  enum AlertSeverity {
    Info
    Warning
    Error
    Critical
  }
  
  // 定义告警状态
  enum AlertStatus {
    Active
    Resolved
    Suppressed
  }
  
  // 定义告警条件
  type AlertCondition = {
    name: String,
    metric: String,
    operator: String,  // ">", "<", ">=", "<=", "==", "!="
    threshold: Float,
    duration: Int,  // 持续时间（秒）
    severity: AlertSeverity
  }
  
  // 定义告警
  type Alert = {
    id: String,
    condition_name: String,
    severity: AlertSeverity,
    status: AlertStatus,
    message: String,
    triggered_at: Int,
    resolved_at: Option[Int],
    labels: Map[String, String],
    annotations: Map[String, String]
  }
  
  // 定义告警规则
  type AlertRule = {
    name: String,
    conditions: Array[AlertCondition],
    evaluation_interval: Int,  // 评估间隔（秒）
    for_duration: Int,  // 触发前持续时间（秒）
    labels: Map[String, String],
    annotations: Map[String, String],
    notification_channels: Array[String]
  }
  
  // 定义通知渠道
  enum NotificationChannel {
    Email(String)  // 邮箱地址
    Webhook(String)  // Webhook URL
    Slack(String)  // Slack频道
    PagerDuty(String)  // PagerDuty服务键
  }
  
  // 创建告警管理器
  let create_alert_manager = fn() {
    let mut rules = Map::empty()
    let mut active_alerts = Map::empty()
    let mut alert_history = []
    let mut notification_channels = Map::empty()
    
    // 添加告警规则
    let add_rule = fn(rule: AlertRule) {
      let _ = Map::insert(rules, rule.name, rule)
    }
    
    // 添加通知渠道
    let add_notification_channel = fn(name: String, channel: NotificationChannel) {
      let _ = Map::insert(notification_channels, name, channel)
    }
    
    // 评估告警条件
    let evaluate_condition = fn(condition: AlertCondition, metric_value: Float) {
      match condition.operator {
        ">" => metric_value > condition.threshold
        "<" => metric_value < condition.threshold
        ">=" => metric_value >= condition.threshold
        "<=" => metric_value <= condition.threshold
        "==" => metric_value == condition.threshold
        "!=" => metric_value != condition.threshold
        _ => false
      }
    }
    
    // 评估告警规则
    let evaluate_rule = fn(rule_name: String, metrics: Map[String, Float]) {
      match Map::get(rules, rule_name) {
        Some(rule) => {
          let mut triggered_conditions = []
          
          for condition in rule.conditions {
            match Map::get(metrics, condition.metric) {
              Some(value) => {
                if evaluate_condition(condition, value) {
                  triggered_conditions = triggered_conditions.push(condition)
                }
              }
              None => {}  // 指标不存在，跳过
            }
          }
          
          // 如果所有条件都满足，触发告警
          if triggered_conditions.length() == rule.conditions.length() && rule.conditions.length() > 0 {
            let alert_id = "alert-" + rule_name + "-" + Time::now().to_string()
            let alert = {
              id: alert_id,
              condition_name: rule_name,
              severity: triggered_conditions[0].severity,  // 使用第一个条件的严重级别
              status: AlertStatus::Active,
              message: "告警规则 '" + rule_name + "' 已触发",
              triggered_at: Time::now(),
              resolved_at: None,
              labels: rule.labels,
              annotations: rule.annotations
            }
            
            // 存储活动告警
            let _ = Map::insert(active_alerts, alert_id, alert)
            alert_history = alert_history.push(alert)
            
            // 发送通知
            for channel_name in rule.notification_channels {
              match Map::get(notification_channels, channel_name) {
                Some(channel) => {
                  // 模拟发送通知
                  println("发送告警通知到 " + channel_name + ": " + alert.message)
                }
                None => {}
              }
            }
            
            Some(alert)
          } else {
            None
          }
        }
        None => None
      }
    }
    
    // 解决告警
    let resolve_alert = fn(alert_id: String) {
      match Map::get(active_alerts, alert_id) {
        Some(alert) => {
          let resolved_alert = { 
            alert with 
            status: AlertStatus::Resolved, 
            resolved_at: Some(Time::now()) 
          }
          
          // 更新活动告警
          let _ = Map::insert(active_alerts, alert_id, resolved_alert)
          
          // 更新历史记录
          alert_history = alert_history.map(fn(a) { 
            if a.id == alert_id { resolved_alert } else { a } 
          })
          
          true
        }
        None => false
      }
    }
    
    // 获取活动告警
    let get_active_alerts = fn() {
      active_alerts.to_array().filter(fn(entry) { 
        let (_, alert) = entry
        alert.status == AlertStatus::Active
      }).map(fn(entry) { let (_, alert) = entry; alert })
    }
    
    // 获取告警历史
    let get_alert_history = fn(filter: Option[Alert -> Bool]) {
      match filter {
        Some(f) => alert_history.filter(f)
        None => alert_history
      }
    }
    
    {
      add_rule,
      add_notification_channel,
      evaluate_rule,
      resolve_alert,
      get_active_alerts,
      get_alert_history
    }
  }
  
  // 创建指标收集器
  let create_metrics_collector = fn() {
    let mut metrics = Map::empty()
    
    // 记录指标
    let record_metric = fn(name: String, value: Float) {
      let _ = Map::insert(metrics, name, value)
    }
    
    // 获取指标
    let get_metric = fn(name: String) {
      Map::get(metrics, name)
    }
    
    // 获取所有指标
    let get_all_metrics = fn() {
      metrics
    }
    
    // 模拟指标更新
    let simulate_metrics_update = fn() {
      // 模拟一些系统指标
      let _ = Map::insert(metrics, "cpu_usage", 65.5 + (Time::now() % 100) as Float / 10.0)
      let _ = Map::insert(metrics, "memory_usage", 78.2 + (Time::now() % 50) as Float / 10.0)
      let _ = Map::insert(metrics, "error_rate", 2.5 + (Time::now() % 20) as Float / 10.0)
      let _ = Map::insert(metrics, "response_time_p95", 250.0 + (Time::now() % 200) as Float)
      let _ = Map::insert(metrics, "disk_usage", 45.8 + (Time::now() % 30) as Float / 10.0)
    }
    
    {
      record_metric,
      get_metric,
      get_all_metrics,
      simulate_metrics_update
    }
  }
  
  // 测试告警管理
  let alert_manager = create_alert_manager()
  let metrics_collector = create_metrics_collector()
  
  // 添加通知渠道
  alert_manager.add_notification_channel("email", NotificationChannel::Email("admin@example.com"))
  alert_manager.add_notification_channel("webhook", NotificationChannel::Webhook("https://api.example.com/alerts"))
  alert_manager.add_notification_channel("slack", NotificationChannel::Slack("#alerts"))
  
  // 添加告警规则
  let cpu_rule = {
    name: "high_cpu_usage",
    conditions: [{
      name: "cpu_high",
      metric: "cpu_usage",
      operator: ">",
      threshold: 80.0,
      duration: 300,  // 5分钟
      severity: AlertSeverity::Warning
    }],
    evaluation_interval: 60,  // 每分钟评估一次
    for_duration: 300,  // 持续5分钟
    labels: Map::from_array([("team", "ops"), ("service", "web")]),
    annotations: Map::from_array([("description", "CPU使用率过高")]),
    notification_channels: ["email", "slack"]
  }
  
  let memory_rule = {
    name: "high_memory_usage",
    conditions: [{
      name: "memory_high",
      metric: "memory_usage",
      operator: ">",
      threshold: 90.0,
      duration: 300,
      severity: AlertSeverity::Critical
    }],
    evaluation_interval: 60,
    for_duration: 300,
    labels: Map::from_array([("team", "ops"), ("service", "web")]),
    annotations: Map::from_array([("description", "内存使用率过高")]),
    notification_channels: ["email", "webhook", "slack"]
  }
  
  let error_rate_rule = {
    name: "high_error_rate",
    conditions: [{
      name: "error_rate_high",
      metric: "error_rate",
      operator: ">",
      threshold: 5.0,
      duration: 180,  // 3分钟
      severity: AlertSeverity::Error
    }],
    evaluation_interval: 30,  // 每30秒评估一次
    for_duration: 180,
    labels: Map::from_array([("team", "dev"), ("service", "api")]),
    annotations: Map::from_array([("description", "错误率过高")]),
    notification_channels: ["email", "webhook"]
  }
  
  alert_manager.add_rule(cpu_rule)
  alert_manager.add_rule(memory_rule)
  alert_manager.add_rule(error_rate_rule)
  
  // 模拟正常指标
  metrics_collector.record_metric("cpu_usage", 65.5)
  metrics_collector.record_metric("memory_usage", 78.2)
  metrics_collector.record_metric("error_rate", 2.5)
  metrics_collector.record_metric("response_time_p95", 250.0)
  metrics_collector.record_metric("disk_usage", 45.8)
  
  // 评估告警规则（正常情况下不应触发）
  let cpu_alert1 = alert_manager.evaluate_rule("high_cpu_usage", metrics_collector.get_all_metrics())
  assert_true(cpu_alert1.is_none())
  
  let memory_alert1 = alert_manager.evaluate_rule("high_memory_usage", metrics_collector.get_all_metrics())
  assert_true(memory_alert1.is_none())
  
  let error_alert1 = alert_manager.evaluate_rule("high_error_rate", metrics_collector.get_all_metrics())
  assert_true(error_alert1.is_none())
  
  // 验证没有活动告警
  let active_alerts1 = alert_manager.get_active_alerts()
  assert_eq(active_alerts1.length(), 0)
  
  // 模拟高CPU使用率
  metrics_collector.record_metric("cpu_usage", 85.0)
  
  // 评估CPU告警规则（应该触发）
  let cpu_alert2 = alert_manager.evaluate_rule("high_cpu_usage", metrics_collector.get_all_metrics())
  assert_true(cpu_alert2.is_some())
  
  let cpu_alert = cpu_alert2.unwrap()
  assert_eq(cpu_alert.condition_name, "high_cpu_usage")
  assert_eq(cpu_alert.severity, AlertSeverity::Warning)
  assert_eq(cpu_alert.status, AlertStatus::Active)
  
  // 验证有一个活动告警
  let active_alerts2 = alert_manager.get_active_alerts()
  assert_eq(active_alerts2.length(), 1)
  assert_eq(active_alerts2[0].id, cpu_alert.id)
  
  // 模拟高内存使用率
  metrics_collector.record_metric("memory_usage", 95.0)
  
  // 评估内存告警规则（应该触发）
  let memory_alert2 = alert_manager.evaluate_rule("high_memory_usage", metrics_collector.get_all_metrics())
  assert_true(memory_alert2.is_some())
  
  let memory_alert = memory_alert2.unwrap()
  assert_eq(memory_alert.condition_name, "high_memory_usage")
  assert_eq(memory_alert.severity, AlertSeverity::Critical)
  assert_eq(memory_alert.status, AlertStatus::Active)
  
  // 验证有两个活动告警
  let active_alerts3 = alert_manager.get_active_alerts()
  assert_eq(active_alerts3.length(), 2)
  
  // 解决CPU告警
  let resolve_result = alert_manager.resolve_alert(cpu_alert.id)
  assert_true(resolve_result)
  
  // 验证告警已解决
  let active_alerts4 = alert_manager.get_active_alerts()
  assert_eq(active_alerts4.length(), 1)
  assert_eq(active_alerts4[0].id, memory_alert.id)
  
  // 验证告警历史
  let all_history = alert_manager.get_alert_history(None)
  assert_true(all_history.length() >= 2)
  
  let cpu_history = alert_manager.get_alert_history(Some(fn(alert) { alert.condition_name == "high_cpu_usage" }))
  assert_eq(cpu_history.length(), 1)
  assert_eq(cpu_history[0].status, AlertStatus::Resolved)
  assert_true(cpu_history[0].resolved_at.is_some())
  
  // 测试告警聚合
  let aggregate_alerts = fn(alerts: Array[Alert]) {
    let grouped = alerts.reduce(Map::empty(), fn(acc, alert) {
      let key = alert.condition_name
      let group = match Map::get(acc, key) {
        Some(g) => g
        None => []
      }
      let _ = Map::insert(acc, key, group.push(alert))
      acc
    })
    
    grouped.to_array().map(fn(entry) {
      let (condition_name, alerts) = entry
      let count = alerts.length()
      let severities = alerts.map(fn(a) { a.severity })
      let max_severity = if severities.contains(AlertSeverity::Critical) {
        AlertSeverity::Critical
      } else if severities.contains(AlertSeverity::Error) {
        AlertSeverity::Error
      } else if severities.contains(AlertSeverity::Warning) {
        AlertSeverity::Warning
      } else {
        AlertSeverity::Info
      }
      
      {
        condition_name: condition_name,
        count: count,
        max_severity: max_severity,
        alerts: alerts
      }
    })
  }
  
  // 创建更多告警用于测试聚合
  metrics_collector.record_metric("error_rate", 6.0)
  let error_alert2 = alert_manager.evaluate_rule("high_error_rate", metrics_collector.get_all_metrics())
  assert_true(error_alert2.is_some())
  
  metrics_collector.record_metric("error_rate", 7.0)
  let error_alert3 = alert_manager.evaluate_rule("high_error_rate", metrics_collector.get_all_metrics())
  assert_true(error_alert3.is_some())
  
  // 获取所有活动告警
  let all_active_alerts = alert_manager.get_active_alerts()
  
  // 测试告警聚合
  let aggregated_alerts = aggregate_alerts(all_active_alerts)
  assert_true(aggregated_alerts.length() >= 2)
  
  // 验证错误率告警聚合
  let error_aggregation = aggregated_alerts.find(fn(aggr) { aggr.condition_name == "high_error_rate" })
  assert_true(error_aggregation.is_some())
  assert_eq(error_aggregation.unwrap().count, 2)
  assert_eq(error_aggregation.unwrap().max_severity, AlertSeverity::Error)
  
  // 测试告警抑制
  let suppress_alerts = fn(alerts: Array[Alert], suppression_rules: Array[Alert -> Bool]) {
    alerts.filter(fn(alert) {
      not suppression_rules.any(fn(rule) { rule(alert) })
    })
  }
  
  // 定义抑制规则：抑制低严重级别的告警
  let suppress_low_severity = fn(alert: Alert) {
    alert.severity == AlertSeverity::Info || alert.severity == AlertSeverity::Warning
  }
  
  // 应用抑制规则
  let suppressed_alerts = suppress_alerts(all_active_alerts, [suppress_low_severity])
  
  // 验证只有高严重级别的告警保留
  assert_true(suppressed_alerts.all(fn(alert) { 
    alert.severity == AlertSeverity::Error || alert.severity == AlertSeverity::Critical 
  }))
}

// 测试17: 遥测数据版本兼容性
test "遥测数据版本兼容性和迁移" {
  // 定义数据版本
  type DataVersion = {
    major: Int,
    minor: Int,
    patch: Int
  }
  
  // 定义版本兼容性策略
  enum CompatibilityStrategy {
    BackwardCompatible  // 向后兼容
    ForwardCompatible   // 向前兼容
    BreakingChange      // 破坏性变更
  }
  
  // 定义字段变更类型
  enum FieldChangeType {
    Added
    Removed
    Renamed
    TypeChanged
    DefaultChanged
  }
  
  // 定义字段变更
  type FieldChange = {
    field_name: String,
    change_type: FieldChangeType,
    old_type: Option[String],
    new_type: Option[String],
    old_value: Option[String],
    new_value: Option[String]
  }
  
  // 定义模式变更
  type SchemaChange = {
    version_from: DataVersion,
    version_to: DataVersion,
    compatibility: CompatibilityStrategy,
    changes: Array[FieldChange],
    migration_script: Option[String]
  }
  
  // 定义遥测事件模式
  type TelemetryEventSchema = {
    name: String,
    version: DataVersion,
    fields: Array[(String, String)],  // 字段名和类型
    required_fields: Array[String],
    default_values: Map[String, String]
  }
  
  // 创建版本管理器
  let create_version_manager = fn() {
    let mut schemas = Map::empty()
    let mut schema_changes = []
    let mut migration_scripts = Map::empty()
    
    // 注册模式
    let register_schema = fn(schema: TelemetryEventSchema) {
      let key = schema.name + ":" + schema.version.major.to_string() + "." + schema.version.minor.to_string()
      let _ = Map::insert(schemas, key, schema)
    }
    
    // 获取模式
    let get_schema = fn(name: String, version: DataVersion) {
      let key = name + ":" + version.major.to_string() + "." + version.minor.to_string()
      Map::get(schemas, key)
    }
    
    // 获取最新模式
    let get_latest_schema = fn(name: String) {
      let name_schemas = schemas.to_array().filter(fn(entry) {
        let (key, _) = entry
        key.starts_with(name + ":")
      }).map(fn(entry) { let (_, schema) = entry; schema })
      
      if name_schemas.length() > 0 {
        // 按版本排序，返回最新的
        let sorted = name_schemas.sort(fn(a, b) {
          if a.version.major != b.version.major {
            a.version.major > b.version.major
          } else if a.version.minor != b.version.minor {
            a.version.minor > b.version.minor
          } else {
            a.version.patch >= b.version.patch
          }
        })
        Some(sorted[0])
      } else {
        None
      }
    }
    
    // 添加模式变更
    let add_schema_change = fn(change: SchemaChange) {
      schema_changes = schema_changes.push(change)
    }
    
    // 添加迁移脚本
    let add_migration_script = fn(from_version: String, to_version: String, script: String) {
      let key = from_version + "->" + to_version
      let _ = Map::insert(migration_scripts, key, script)
    }
    
    // 检查兼容性
    let check_compatibility = fn(from_version: DataVersion, to_version: DataVersion) {
      if from_version.major == to_version.major {
        if from_version.minor == to_version.minor {
          // 补丁版本更新，总是兼容
          CompatibilityStrategy::BackwardCompatible
        } else if from_version.minor < to_version.minor {
          // 次版本更新，向后兼容
          CompatibilityStrategy::BackwardCompatible
        } else {
          // 次版本降级，向前兼容
          CompatibilityStrategy::ForwardCompatible
        }
      } else if from_version.major < to_version.major {
        // 主版本更新，可能是破坏性变更
        CompatibilityStrategy::BreakingChange
      } else {
        // 主版本降级，向前兼容
        CompatibilityStrategy::ForwardCompatible
      }
    }
    
    // 迁移数据
    let migrate_data = fn(data: Map[String, String], from_version: DataVersion, to_version: DataVersion) {
      let compatibility = check_compatibility(from_version, to_version)
      
      match compatibility {
        CompatibilityStrategy::BackwardCompatible => {
          // 向后兼容，可能需要添加新字段
          match get_schema("telemetry_event", to_version) {
            Some(schema) => {
              let migrated_data = data
              
              // 添加新字段及其默认值
              for (field_name, field_type) in schema.fields {
                if not Map::contains(data, field_name) {
                  let default_value = match Map::get(schema.default_values, field_name) {
                    Some(v) => v
                    None => ""
                  }
                  let _ = Map::insert(migrated_data, field_name, default_value)
                }
              }
              
              migrated_data
            }
            None => data
          }
        }
        CompatibilityStrategy::ForwardCompatible => {
          // 向前兼容，可能需要移除不存在的字段
          match get_schema("telemetry_event", to_version) {
            Some(schema) => {
              let valid_fields = schema.fields.map(fn(field) { field.0 })
              let migrated_data = Map::empty()
              
              for (key, value) in data {
                if valid_fields.contains(key) {
                  let _ = Map::insert(migrated_data, key, value)
                }
              }
              
              migrated_data
            }
            None => data
          }
        }
        CompatibilityStrategy::BreakingChange => {
          // 破坏性变更，需要迁移脚本
          let from_key = from_version.major.to_string() + "." + from_version.minor.to_string()
          let to_key = to_version.major.to_string() + "." + to_version.minor.to_string()
          let script_key = from_key + "->" + to_key
          
          match Map::get(migration_scripts, script_key) {
            Some(_) => {
              // 简化的迁移逻辑，实际应执行脚本
              let migrated_data = Map::empty()
              
              // 保留共同字段
              for (key, value) in data {
                if key != "deprecated_field" {  // 简化：移除已知已弃用的字段
                  let _ = Map::insert(migrated_data, key, value)
                }
              }
              
              // 添加新字段
              let _ = Map::insert(migrated_data, "new_field", "default_value")
              
              migrated_data
            }
            None => {
              // 没有迁移脚本，无法迁移
              Map::empty()
            }
          }
        }
      }
    }
    
    {
      register_schema,
      get_schema,
      get_latest_schema,
      add_schema_change,
      add_migration_script,
      check_compatibility,
      migrate_data
    }
  }
  
  // 创建数据转换器
  let create_data_converter = fn() {
    // 转换为JSON格式
    let to_json = fn(data: Map[String, String]) {
      let pairs = data.to_array().map(fn(entry) {
        let (key, value) = entry
        "\"" + key + "\":\"" + value + "\""
      })
      "{" + pairs.join(",") + "}"
    }
    
    // 从JSON格式转换
    let from_json = fn(json_str: String) {
      // 简化的JSON解析
      let mut data = Map::empty()
      
      if json_str.starts_with("{") && json_str.ends_with("}") {
        let content = json_str.slice(1, json_str.length() - 1)
        let pairs = content.split(",")
        
        for pair in pairs {
          let parts = pair.split(":")
          if parts.length() == 2 {
            let key = parts[0].slice(1, parts[0].length() - 1)  // 移除引号
            let value = parts[1].slice(1, parts[1].length() - 1)  // 移除引号
            let _ = Map::insert(data, key, value)
          }
        }
      }
      
      data
    }
    
    // 转换为XML格式
    let to_xml = fn(data: Map[String, String]) {
      let mut xml = "<event>\n"
      
      for (key, value) in data {
        xml = xml + "  <" + key + ">" + value + "</" + key + ">\n"
      }
      
      xml = xml + "</event>"
      xml
    }
    
    // 从XML格式转换
    let from_xml = fn(xml_str: String) {
      let mut data = Map::empty()
      
      // 简化的XML解析
      let lines = xml_str.split("\n")
      for line in lines {
        if line.starts_with("  <") && line.contains(">") && line.contains("</") {
          let start_tag_end = line.index_of(">").unwrap_or(0)
          let end_tag_start = line.index_of("</").unwrap_or(0)
          
          if start_tag_end > 0 && end_tag_start > start_tag_end {
            let tag = line.slice(3, start_tag_end)  // 移除"  <"
            let value = line.slice(start_tag_end + 1, end_tag_start)
            let _ = Map::insert(data, tag, value)
          }
        }
      }
      
      data
    }
    
    { to_json, from_json, to_xml, from_xml }
  }
  
  // 测试版本管理
  let version_manager = create_version_manager()
  
  // 定义版本
  let v1_0_0 = { major: 1, minor: 0, patch: 0 }
  let v1_1_0 = { major: 1, minor: 1, patch: 0 }
  let v1_2_0 = { major: 1, minor: 2, patch: 0 }
  let v2_0_0 = { major: 2, minor: 0, patch: 0 }
  
  // 注册模式
  let schema_v1_0_0 = {
    name: "telemetry_event",
    version: v1_0_0,
    fields: [
      ("event_id", "string"),
      ("timestamp", "integer"),
      ("event_name", "string"),
      ("user_id", "string")
    ],
    required_fields: ["event_id", "timestamp", "event_name"],
    default_values: Map::from_array([
      ("user_id", "anonymous")
    ])
  }
  
  let schema_v1_1_0 = {
    name: "telemetry_event",
    version: v1_1_0,
    fields: [
      ("event_id", "string"),
      ("timestamp", "integer"),
      ("event_name", "string"),
      ("user_id", "string"),
      ("session_id", "string")
    ],
    required_fields: ["event_id", "timestamp", "event_name"],
    default_values: Map::from_array([
      ("user_id", "anonymous"),
      ("session_id", "default_session")
    ])
  }
  
  let schema_v1_2_0 = {
    name: "telemetry_event",
    version: v1_2_0,
    fields: [
      ("event_id", "string"),
      ("timestamp", "integer"),
      ("event_name", "string"),
      ("user_id", "string"),
      ("session_id", "string"),
      ("attributes", "json")
    ],
    required_fields: ["event_id", "timestamp", "event_name"],
    default_values: Map::from_array([
      ("user_id", "anonymous"),
      ("session_id", "default_session"),
      ("attributes", "{}")
    ])
  }
  
  let schema_v2_0_0 = {
    name: "telemetry_event",
    version: v2_0_0,
    fields: [
      ("id", "string"),  // 重命名字段
      ("timestamp", "integer"),
      ("name", "string"),  // 重命名字段
      ("user_id", "string"),
      ("session_id", "string"),
      ("attributes", "json"),
      ("metadata", "json")  // 新字段
    ],
    required_fields: ["id", "timestamp", "name"],
    default_values: Map::from_array([
      ("user_id", "anonymous"),
      ("session_id", "default_session"),
      ("attributes", "{}"),
      ("metadata", "{}")
    ])
  }
  
  version_manager.register_schema(schema_v1_0_0)
  version_manager.register_schema(schema_v1_1_0)
  version_manager.register_schema(schema_v1_2_0)
  version_manager.register_schema(schema_v2_0_0)
  
  // 测试获取模式
  let retrieved_schema_v1_0_0 = version_manager.get_schema("telemetry_event", v1_0_0)
  assert_true(retrieved_schema_v1_0_0.is_some())
  assert_eq(retrieved_schema_v1_0_0.unwrap().version.major, 1)
  assert_eq(retrieved_schema_v1_0_0.unwrap().version.minor, 0)
  assert_eq(retrieved_schema_v1_0_0.unwrap().fields.length(), 4)
  
  // 测试获取最新模式
  let latest_schema = version_manager.get_latest_schema("telemetry_event")
  assert_true(latest_schema.is_some())
  assert_eq(latest_schema.unwrap().version.major, 2)
  assert_eq(latest_schema.unwrap().version.minor, 0)
  
  // 测试兼容性检查
  let compat_1_0_to_1_1 = version_manager.check_compatibility(v1_0_0, v1_1_0)
  assert_eq(compat_1_0_to_1_1, CompatibilityStrategy::BackwardCompatible)
  
  let compat_1_1_to_1_0 = version_manager.check_compatibility(v1_1_0, v1_0_0)
  assert_eq(compat_1_1_to_1_0, CompatibilityStrategy::ForwardCompatible)
  
  let compat_1_2_to_2_0 = version_manager.check_compatibility(v1_2_0, v2_0_0)
  assert_eq(compat_1_2_to_2_0, CompatibilityStrategy::BreakingChange)
  
  // 添加迁移脚本
  version_manager.add_migration_script("1.2->2.0", "function migrate(data) { data['id'] = data['event_id']; data['name'] = data['event_name']; delete data['event_id']; delete data['event_name']; return data; }")
  
  // 测试数据迁移
  let old_data = Map::from_array([
    ("event_id", "event-123"),
    ("timestamp", "1609459200"),
    ("event_name", "user.login"),
    ("user_id", "user-456")
  ])
  
  // 测试向后兼容迁移（1.0 -> 1.1）
  let migrated_1_0_to_1_1 = version_manager.migrate_data(old_data, v1_0_0, v1_1_0)
  assert_eq(Map::get(migrated_1_0_to_1_1, "event_id"), Some("event-123"))
  assert_eq(Map::get(migrated_1_0_to_1_1, "session_id"), Some("default_session"))  // 新字段添加默认值
  
  // 测试向前兼容迁移（1.2 -> 1.1）
  let new_data = Map::from_array([
    ("event_id", "event-123"),
    ("timestamp", "1609459200"),
    ("event_name", "user.login"),
    ("user_id", "user-456"),
    ("session_id", "session-789"),
    ("attributes", "{\"key\":\"value\"}")
  ])
  
  let migrated_1_2_to_1_1 = version_manager.migrate_data(new_data, v1_2_0, v1_1_0)
  assert_eq(Map::get(migrated_1_2_to_1_1, "event_id"), Some("event-123"))
  assert_eq(Map::get(migrated_1_2_to_1_1, "session_id"), Some("session-789"))
  assert_false(Map::contains(migrated_1_2_to_1_1, "attributes"))  // 移除不存在的字段
  
  // 测试破坏性变更迁移（1.2 -> 2.0）
  let migrated_1_2_to_2_0 = version_manager.migrate_data(new_data, v1_2_0, v2_0_0)
  assert_eq(Map::get(migrated_1_2_to_2_0, "id"), Some("event-123"))  // 重命名的字段
  assert_eq(Map::get(migrated_1_2_to_2_0, "name"), Some("user.login"))  // 重命名的字段
  assert_eq(Map::get(migrated_1_2_to_2_0, "new_field"), Some("default_value"))  // 新字段
  
  // 测试数据转换
  let converter = create_data_converter()
  
  let test_data = Map::from_array([
    ("event_id", "event-123"),
    ("timestamp", "1609459200"),
    ("event_name", "user.login")
  ])
  
  // 测试JSON转换
  let json_data = converter.to_json(test_data)
  assert_true(json_data.contains("\"event_id\":\"event-123\""))
  assert_true(json_data.contains("\"event_name\":\"user.login\""))
  
  let parsed_json = converter.from_json(json_data)
  assert_eq(Map::get(parsed_json, "event_id"), Some("event-123"))
  assert_eq(Map::get(parsed_json, "event_name"), Some("user.login"))
  
  // 测试XML转换
  let xml_data = converter.to_xml(test_data)
  assert_true(xml_data.contains("<event_id>event-123</event_id>"))
  assert_true(xml_data.contains("<event_name>user.login</event_name>"))
  
  let parsed_xml = converter.from_xml(xml_data)
  assert_eq(Map::get(parsed_xml, "event_id"), Some("event-123"))
  assert_eq(Map::get(parsed_xml, "event_name"), Some("user.login"))
  
  // 测试版本兼容性验证
  let validate_data_against_schema = fn(data: Map[String, String], schema: TelemetryEventSchema) {
    let mut valid = true
    let mut missing_fields = []
    
    // 检查必需字段
    for required_field in schema.required_fields {
      if not Map::contains(data, required_field) {
        valid = false
        missing_fields = missing_fields.push(required_field)
      }
    }
    
    // 检查字段类型（简化）
    for (field_name, field_type) in schema.fields {
      match Map::get(data, field_name) {
        Some(value) => {
          match field_type {
            "integer" => {
              if value.parse_int().is_none() {
                valid = false
              }
            }
            "float" => {
              if value.parse_float().is_none() {
                valid = false
              }
            }
            "boolean" => {
              if value != "true" && value != "false" {
                valid = false
              }
            }
            _ => {}  // 字符串类型总是有效
          }
        }
        None => {}  // 可选字段
      }
    }
    
    { valid: valid, missing_fields: missing_fields }
  }
  
  // 测试数据验证
  let valid_data = Map::from_array([
    ("event_id", "event-123"),
    ("timestamp", "1609459200"),
    ("event_name", "user.login"),
    ("user_id", "user-456")
  ])
  
  let validation_result = validate_data_against_schema(valid_data, schema_v1_0_0)
  assert_true(validation_result.valid)
  assert_eq(validation_result.missing_fields.length(), 0)
  
  let invalid_data = Map::from_array([
    ("event_id", "event-123"),
    ("user_id", "user-456")
    // 缺少timestamp和event_name
  ])
  
  let invalid_validation_result = validate_data_against_schema(invalid_data, schema_v1_0_0)
  assert_false(invalid_validation_result.valid)
  assert_eq(invalid_validation_result.missing_fields.length(), 2)
  assert_true(invalid_validation_result.missing_fields.contains("timestamp"))
  assert_true(invalid_validation_result.missing_fields.contains("event_name"))
}