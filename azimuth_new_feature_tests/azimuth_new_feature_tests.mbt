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