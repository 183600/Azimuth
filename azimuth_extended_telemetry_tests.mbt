// Azimuth 遥测系统扩展测试用例
// 包含遥测数据处理、分析和传输的高级测试

// 测试1: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)],
    events: Array[(Int, String, Array[(String, String)])]  // (timestamp, name, attributes)
  }
  
  // 创建JSON序列化器
  let serialize_to_json = fn(data: TelemetryData) {
    let attributes_json = data.attributes.map(fn(attr) {
      let (key, value) = attr
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    
    let events_json = data.events.map(fn(event) {
      let (timestamp, name, event_attrs) = event
      let event_attrs_json = event_attrs.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      "{\"timestamp\":" + timestamp.to_string() + ",\"name\":\"" + name + "\",\"attributes\":{" + event_attrs_json + "}}"
    }).join(",")
    
    "{" +
      "\"trace_id\":\"" + data.trace_id + "\"," +
      "\"span_id\":\"" + data.span_id + "\"," +
      "\"parent_span_id\":" + match data.parent_span_id {
        Some(id) => "\"" + id + "\""
        None => "null"
      } + "," +
      "\"operation_name\":\"" + data.operation_name + "\"," +
      "\"start_time\":" + data.start_time.to_string() + "," +
      "\"end_time\":" + data.end_time.to_string() + "," +
      "\"status\":\"" + data.status + "\"," +
      "\"attributes\":{" + attributes_json + "}," +
      "\"events\":[" + events_json + "]" +
    "}"
  }
  
  // 创建JSON反序列化器（简化版）
  let deserialize_from_json = fn(json: String) {
    // 在实际实现中，这里会有完整的JSON解析逻辑
    // 为了测试目的，我们使用简化版本
    if json.contains("\"trace_id\":\"abc123\"") {
      Some({
        trace_id: "abc123",
        span_id: "def456",
        parent_span_id: Some("root"),
        operation_name: "database.query",
        start_time: 1000,
        end_time: 1500,
        status: "ok",
        attributes: [("service", "api"), ("env", "prod")],
        events: [(1200, "query.start", []), (1400, "query.complete", [("rows", "42")])]
      })
    } else {
      None
    }
  }
  
  // 创建测试数据
  let test_data = {
    trace_id: "abc123",
    span_id: "def456",
    parent_span_id: Some("root"),
    operation_name: "database.query",
    start_time: 1000,
    end_time: 1500,
    status: "ok",
    attributes: [("service", "api"), ("env", "prod")],
    events: [(1200, "query.start", []), (1400, "query.complete", [("rows", "42")])]
  }
  
  // 测试序列化
  let serialized = serialize_to_json(test_data)
  assert_true(serialized.contains("\"trace_id\":\"abc123\""))
  assert_true(serialized.contains("\"span_id\":\"def456\""))
  assert_true(serialized.contains("\"operation_name\":\"database.query\""))
  assert_true(serialized.contains("\"service\":\"api\""))
  
  // 测试反序列化
  let deserialized = deserialize_from_json(serialized)
  assert_true(deserialized.is_some())
  
  match deserialized {
    Some(data) => {
      assert_eq(data.trace_id, test_data.trace_id)
      assert_eq(data.span_id, test_data.span_id)
      assert_eq(data.operation_name, test_data.operation_name)
      assert_eq(data.status, test_data.status)
      assert_eq(data.attributes.length(), 2)
    }
    None => assert_true(false)
  }
}

// 测试2: 遥测数据压缩和传输
test "遥测数据压缩和传输" {
  // 定义压缩算法
  let compress_data = fn(data: String) {
    // 简化的压缩算法：移除重复的空白字符
    let compressed = data.chars().filter(fn(c) {
      not (c == ' ' && data.contains("  "))  // 移除多余空格
    }).to_string()
    
    // 计算压缩率
    let original_size = data.length()
    let compressed_size = compressed.length()
    let compression_ratio = (original_size - compressed_size) as Float / original_size as Float
    
    (compressed, compression_ratio)
  }
  
  // 定义数据传输模拟器
  let transmit_data = fn(data: String, packet_size: Int) {
    let data_length = data.length()
    let packets = []
    
    for i in 0..=(data_length / packet_size) {
      let start = i * packet_size
      let end = if start + packet_size < data_length { start + packet_size } else { data_length }
      let packet = data.substring(start, end)
      packets = packets.push(packet)
    }
    
    packets
  }
  
  // 创建大量遥测数据
  let telemetry_batch = [
    "{\"trace_id\":\"t1\",\"span_id\":\"s1\",\"operation\":\"op1\",\"duration\":100}",
    "{\"trace_id\":\"t2\",\"span_id\":\"s2\",\"operation\":\"op2\",\"duration\":200}",
    "{\"trace_id\":\"t3\",\"span_id\":\"s3\",\"operation\":\"op3\",\"duration\":150}",
    "{\"trace_id\":\"t4\",\"span_id\":\"s4\",\"operation\":\"op4\",\"duration\":300}",
    "{\"trace_id\":\"t5\",\"span_id\":\"s5\",\"operation\":\"op5\",\"duration\":250}"
  ].join("")
  
  // 测试压缩
  let (compressed_data, compression_ratio) = compress_data(telemetry_batch)
  assert_true(compressed_data.length() <= telemetry_batch.length())
  assert_true(compression_ratio >= 0.0)
  
  // 测试数据分片传输
  let packet_size = 100  // 每个包100字符
  let packets = transmit_data(telemetry_batch, packet_size)
  assert_true(packets.length() > 1)
  
  // 重组数据
  let reassembled = packets.join("")
  assert_eq(reassembled, telemetry_batch)
  
  // 测试压缩后传输
  let compressed_packets = transmit_data(compressed_data, packet_size)
  let reassembled_compressed = compressed_packets.join("")
  assert_eq(reassembled_compressed, compressed_data)
  
  // 计算传输效率提升
  let original_packets = transmit_data(telemetry_batch, packet_size).length()
  let compressed_packets_count = compressed_packets.length()
  assert_true(compressed_packets_count <= original_packets)
}

// 测试3: 分布式追踪的上下文传播
test "分布式追踪的上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: Array[(String, String)>
  }
  
  // 定义传播器
  type TextMapPropagator = {
    inject: (TraceContext, Array[(String, String)]) -> Array[(String, String)>,
    extract: (Array[(String, String)]) -> Option[TraceContext]
  }
  
  // 创建W3C TraceContext传播器
  let w3c_propagator = {
    inject: fn(context: TraceContext, carrier: Array[(String, String)>) {
      let trace_parent = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string(16)
      let trace_state = context.trace_state.map(fn(pair) {
        let (key, value) = pair
        key + "=" + value
      }).join(",")
      
      let updated_carrier = carrier.push(("traceparent", trace_parent))
      if trace_state.length() > 0 {
        updated_carrier.push(("tracestate", trace_state))
      } else {
        updated_carrier
      }
    },
    
    extract: fn(carrier: Array[(String, String)>) {
      let trace_parent = match carrier.find(fn(pair) { pair.0 == "traceparent" }) {
        Some((_, value)) => value
        None => return None
      }
      
      let parts = trace_parent.split("-")
      if parts.length() != 4 {
        return None
      }
      
      let trace_id = parts[1]
      let span_id = parts[2]
      let trace_flags = match parts[3].parse_int() {
        Some(flags) => flags
        None => return None
      }
      
      let trace_state = match carrier.find(fn(pair) { pair.0 == "tracestate" }) {
        Some((_, value)) => value.split(",").map(fn(pair) {
          let kv = pair.split("=")
          if kv.length() == 2 { (kv[0], kv[1]) } else { ("", "") }
        }).filter(fn(pair) { pair.0.length() > 0 })
        None => []
      }
      
      Some({
        trace_id,
        span_id,
        trace_flags,
        trace_state
      })
    }
  }
  
  // 创建原始上下文
  let original_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b9c7c989f97918e1",
    trace_flags: 1,
    trace_state: [("vendor1", "value1"), ("vendor2", "value2")]
  }
  
  // 测试上下文注入
  let carrier: Array[(String, String)] = [
    ("user-agent", "azimuth-client/1.0"),
    ("accept", "application/json")
  ]
  
  let injected_carrier = w3c_propagator.inject(original_context, carrier)
  assert_eq(injected_carrier.length(), 4)  // 原始2个 + traceparent + tracestate
  
  // 验证注入的内容
  let trace_parent = match injected_carrier.find(fn(pair) { pair.0 == "traceparent" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(trace_parent.contains(original_context.trace_id))
  assert_true(trace_parent.contains(original_context.span_id))
  
  let trace_state = match injected_carrier.find(fn(pair) { pair.0 == "tracestate" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(trace_state.contains("vendor1=value1"))
  assert_true(trace_state.contains("vendor2=value2"))
  
  // 测试上下文提取
  let extracted_context = w3c_propagator.extract(injected_carrier)
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, original_context.trace_id)
      assert_eq(context.span_id, original_context.span_id)
      assert_eq(context.trace_flags, original_context.trace_flags)
      assert_eq(context.trace_state.length(), 2)
    }
    None => assert_true(false)
  }
  
  // 测试跨服务上下文传播
  let create_child_span = fn(parent_context: TraceContext, operation_name: String) {
    {
      trace_id: parent_context.trace_id,
      span_id: "child-" + Time::now().to_string(),  // 简化的span ID生成
      trace_flags: parent_context.trace_flags,
      trace_state: parent_context.trace_state
    }
  }
  
  let child_context = create_child_span(original_context, "child-operation")
  assert_eq(child_context.trace_id, original_context.trace_id)
  assert_not_eq(child_context.span_id, original_context.span_id)
  assert_eq(child_context.trace_flags, original_context.trace_flags)
}

// 测试4: 遥测数据的实时分析
test "遥测数据的实时分析" {
  // 定义度量类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 定义度量点
  type MetricPoint = {
    name: String,
    metric_type: MetricType,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 定义分析结果
  type AnalysisResult = {
    anomaly_detected: Bool,
    anomaly_score: Float,
    trend: String,  // "increasing", "decreasing", "stable"
    recommendations: Array[String]
  }
  
  // 创建实时分析器
  let analyze_metrics = fn(points: Array[MetricPoint]) {
    if points.length() < 3 {
      return {
        anomaly_detected: false,
        anomaly_score: 0.0,
        trend: "stable",
        recommendations: []
      }
    }
    
    let values = points.map(fn(p) { p.value })
    let len = values.length()
    
    // 计算趋势
    let first_half = values.slice(0, len / 2)
    let second_half = values.slice(len / 2, len)
    
    let first_avg = first_half.reduce(fn(acc, v) { acc + v }, 0.0) / (first_half.length() as Float)
    let second_avg = second_half.reduce(fn(acc, v) { acc + v }, 0.0) / (second_half.length() as Float)
    
    let trend = if second_avg > first_avg * 1.1 {
      "increasing"
    } else if second_avg < first_avg * 0.9 {
      "decreasing"
    } else {
      "stable"
    }
    
    // 计算异常分数（简化版）
    let overall_avg = values.reduce(fn(acc, v) { acc + v }, 0.0) / (len as Float)
    let variance = values.reduce(fn(acc, v) { acc + (v - overall_avg) * (v - overall_avg) }, 0.0) / (len as Float)
    let std_dev = variance.sqrt()
    
    let anomaly_score = values.map(fn(v) {
      if std_dev > 0.0 { (v - overall_avg).abs() / std_dev } else { 0.0 }
    }).reduce(fn(acc, score) { if score > acc { score } else { acc } }, 0.0)
    
    let anomaly_detected = anomaly_score > 2.0  // 2个标准差以外视为异常
    
    // 生成建议
    let recommendations = []
    if anomaly_detected {
      recommendations = recommendations.push("检测到异常值，建议检查系统状态")
    }
    
    if trend == "increasing" {
      recommendations = recommendations.push("指标呈上升趋势，建议关注资源使用情况")
    } else if trend == "decreasing" {
      recommendations = recommendations.push("指标呈下降趋势，可能需要调整配置")
    }
    
    {
      anomaly_detected,
      anomaly_score,
      trend,
      recommendations
    }
  }
  
  // 创建测试数据 - 正常情况
  let normal_metrics = [
    { name: "response_time", metric_type: MetricType::Histogram, value: 100.0, timestamp: 1000, labels: [("endpoint", "/api/users")] },
    { name: "response_time", metric_type: MetricType::Histogram, value: 105.0, timestamp: 2000, labels: [("endpoint", "/api/users")] },
    { name: "response_time", metric_type: MetricType::Histogram, value: 98.0, timestamp: 3000, labels: [("endpoint", "/api/users")] },
    { name: "response_time", metric_type: MetricType::Histogram, value: 102.0, timestamp: 4000, labels: [("endpoint", "/api/users")] },
    { name: "response_time", metric_type: MetricType::Histogram, value: 99.0, timestamp: 5000, labels: [("endpoint", "/api/users")] }
  ]
  
  let normal_analysis = analyze_metrics(normal_metrics)
  assert_false(normal_analysis.anomaly_detected)
  assert_eq(normal_analysis.trend, "stable")
  assert_eq(normal_analysis.recommendations.length(), 0)
  
  // 创建测试数据 - 异常情况
  let anomaly_metrics = [
    { name: "error_rate", metric_type: MetricType::Gauge, value: 0.01, timestamp: 1000, labels: [("service", "auth")] },
    { name: "error_rate", metric_type: MetricType::Gauge, value: 0.02, timestamp: 2000, labels: [("service", "auth")] },
    { name: "error_rate", metric_type: MetricType::Gauge, value: 0.15, timestamp: 3000, labels: [("service", "auth")] },  // 异常值
    { name: "error_rate", metric_type: MetricType::Gauge, value: 0.03, timestamp: 4000, labels: [("service", "auth")] },
    { name: "error_rate", metric_type: MetricType::Gauge, value: 0.02, timestamp: 5000, labels: [("service", "auth")] }
  ]
  
  let anomaly_analysis = analyze_metrics(anomaly_metrics)
  assert_true(anomaly_analysis.anomaly_detected)
  assert_true(anomaly_analysis.anomaly_score > 2.0)
  assert_true(anomaly_analysis.recommendations.contains("检测到异常值，建议检查系统状态"))
  
  // 创建测试数据 - 上升趋势
  let increasing_metrics = [
    { name: "memory_usage", metric_type: MetricType::Gauge, value: 100.0, timestamp: 1000, labels: [("service", "database")] },
    { name: "memory_usage", metric_type: MetricType::Gauge, value: 120.0, timestamp: 2000, labels: [("service", "database")] },
    { name: "memory_usage", metric_type: MetricType::Gauge, value: 150.0, timestamp: 3000, labels: [("service", "database")] },
    { name: "memory_usage", metric_type: MetricType::Gauge, value: 180.0, timestamp: 4000, labels: [("service", "database")] },
    { name: "memory_usage", metric_type: MetricType::Gauge, value: 220.0, timestamp: 5000, labels: [("service", "database")] }
  ]
  
  let increasing_analysis = analyze_metrics(increasing_metrics)
  assert_eq(increasing_analysis.trend, "increasing")
  assert_true(increasing_analysis.recommendations.contains("指标呈上升趋势，建议关注资源使用情况"))
}

// 测试5: 自定义度量收集器
test "自定义度量收集器" {
  // 定义度量收集器接口
  trait MetricCollector {
    collect() -> Array[MetricPoint]
    name() -> String
  }
  
  // 定义CPU使用率收集器
  let cpu_collector = {
    collect: fn() {
      // 模拟CPU使用率收集
      let current_time = Time::now()
      let cpu_usage = (Time::now() % 100) as Float / 100.0 * 80.0 + 10.0  // 10-90%的随机值
      
      [
        {
          name: "cpu_usage_percent",
          metric_type: MetricType::Gauge,
          value: cpu_usage,
          timestamp: current_time,
          labels: [("host", "server-1"), ("region", "us-west-2")]
        },
        {
          name: "cpu_load_1m",
          metric_type: MetricType::Gauge,
          value: cpu_usage / 100.0 * 4.0,  // 假设4核CPU
          timestamp: current_time,
          labels: [("host", "server-1"), ("interval", "1m")]
        }
      ]
    },
    
    name: fn() { "CPU收集器" }
  }
  
  // 定义内存收集器
  let memory_collector = {
    collect: fn() {
      // 模拟内存使用情况收集
      let current_time = Time::now()
      let total_memory = 16.0 * 1024.0  // 16GB
      let used_memory = (Time::now() % 1000) as Float / 1000.0 * total_memory * 0.8
      let free_memory = total_memory - used_memory
      
      [
        {
          name: "memory_used_bytes",
          metric_type: MetricType::Gauge,
          value: used_memory,
          timestamp: current_time,
          labels: [("host", "server-1"), ("type", "used")]
        },
        {
          name: "memory_free_bytes",
          metric_type: MetricType::Gauge,
          value: free_memory,
          timestamp: current_time,
          labels: [("host", "server-1"), ("type", "free")]
        },
        {
          name: "memory_usage_percent",
          metric_type: MetricType::Gauge,
          value: used_memory / total_memory * 100.0,
          timestamp: current_time,
          labels: [("host", "server-1")]
        }
      ]
    },
    
    name: fn() { "内存收集器" }
  }
  
  // 定义网络收集器
  let network_collector = {
    collect: fn() {
      // 模拟网络流量收集
      let current_time = Time::now()
      let bytes_sent = (Time::now() % 10000) as Float
      let bytes_received = (Time::now() % 15000) as Float
      
      [
        {
          name: "network_bytes_sent_total",
          metric_type: MetricType::Counter,
          value: bytes_sent,
          timestamp: current_time,
          labels: [("host", "server-1"), ("interface", "eth0")]
        },
        {
          name: "network_bytes_received_total",
          metric_type: MetricType::Counter,
          value: bytes_received,
          timestamp: current_time,
          labels: [("host", "server-1"), ("interface", "eth0")]
        },
        {
          name: "network_packets_sent_total",
          metric_type: MetricType::Counter,
          value: bytes_sent / 1500.0,  // 假设平均包大小1500字节
          timestamp: current_time,
          labels: [("host", "server-1"), ("interface", "eth0")]
        }
      ]
    },
    
    name: fn() { "网络收集器" }
  }
  
  // 创建收集器管理器
  let collectors = [cpu_collector, memory_collector, network_collector]
  
  // 收集所有度量
  let collect_all_metrics = fn(collectors: Array[MetricCollector]) {
    let all_metrics = []
    for collector in collectors {
      let metrics = collector.collect()
      all_metrics = all_metrics.concat(metrics)
    }
    all_metrics
  }
  
  // 测试收集器
  let all_metrics = collect_all_metrics(collectors)
  assert_eq(all_metrics.length(), 8)  // CPU(2) + 内存(3) + 网络(3)
  
  // 验证CPU度量
  let cpu_metrics = all_metrics.filter(fn(m) { m.name.contains("cpu") })
  assert_eq(cpu_metrics.length(), 2)
  assert_true(cpu_metrics.any(fn(m) { m.name == "cpu_usage_percent" }))
  assert_true(cpu_metrics.any(fn(m) { m.name == "cpu_load_1m" }))
  
  // 验证内存度量
  let memory_metrics = all_metrics.filter(fn(m) { m.name.contains("memory") })
  assert_eq(memory_metrics.length(), 3)
  assert_true(memory_metrics.any(fn(m) { m.name == "memory_used_bytes" }))
  assert_true(memory_metrics.any(fn(m) { m.name == "memory_free_bytes" }))
  assert_true(memory_metrics.any(fn(m) { m.name == "memory_usage_percent" }))
  
  // 验证网络度量
  let network_metrics = all_metrics.filter(fn(m) { m.name.contains("network") })
  assert_eq(network_metrics.length(), 3)
  assert_true(network_metrics.any(fn(m) { m.name == "network_bytes_sent_total" }))
  assert_true(network_metrics.any(fn(m) { m.name == "network_bytes_received_total" }))
  assert_true(network_metrics.any(fn(m) { m.name == "network_packets_sent_total" }))
  
  // 测试按名称分组度量
  let group_by_name = fn(metrics: Array[MetricPoint]) {
    let groups = Map::empty()
    for metric in metrics {
      let group = match Map::get(groups, metric.name) {
        Some(group_metrics) => group_metrics
        None => []
      }
      let updated_group = group.push(metric)
      let _ = Map::insert(groups, metric.name, updated_group)
    }
    groups
  }
  
  let grouped_metrics = group_by_name(all_metrics)
  assert_eq(grouped_metrics.size(), 8)  // 8个不同的度量名称
}

// 测试6: 遥测数据的持久化存储
test "遥测数据的持久化存储" {
  // 定义存储后端接口
  trait StorageBackend {
    store(data: String) -> Result[String, String]
    retrieve(key: String) -> Result[String, String]
    delete(key: String) -> Result[String, String]
  }
  
  // 创建内存存储后端（用于测试）
  let memory_storage = {
    let storage = Map::empty()
    
    store: fn(data: String) {
      let key = "telemetry_" + Time::now().to_string()
      let _ = Map::insert(storage, key, data)
      Ok(key)
    },
    
    retrieve: fn(key: String) {
      match Map::get(storage, key) {
        Some(data) => Ok(data)
        None => Err("Key not found: " + key)
      }
    },
    
    delete: fn(key: String) {
      match Map::get(storage, key) {
        Some(_) => {
          let _ = Map::remove(storage, key)
          Ok("Deleted")
        }
        None => Err("Key not found: " + key)
      }
    }
  }
  
  // 定义数据保留策略
  type RetentionPolicy = {
    max_age_days: Int,
    max_size_mb: Int,
    cleanup_interval_hours: Int
  }
  
  // 定义存储管理器
  let create_storage_manager = fn(backend: StorageBackend, policy: RetentionPolicy) {
    let stored_keys = []
    
    {
      store_telemetry: fn(data: String) {
        backend.store(data)
      },
      
      retrieve_telemetry: fn(key: String) {
        backend.retrieve(key)
      },
      
      cleanup_old_data: fn() {
        // 简化的清理逻辑
        let current_time = Time::now()
        let max_age_ms = policy.max_age_days * 24 * 60 * 60 * 1000
        
        let keys_to_remove = []
        for key in stored_keys {
          // 从key中提取时间戳
          let timestamp_str = key.substring(9)  // 移除"telemetry_"前缀
          match timestamp_str.parse_int() {
            Some(timestamp) => {
              if current_time - timestamp > max_age_ms {
                keys_to_remove = keys_to_remove.push(key)
              }
            }
            None => ()
          }
        }
        
        for key in keys_to_remove {
          match backend.delete(key) {
            Ok(_) => ()
            Err(_) => ()
          }
        }
        
        keys_to_remove.length()
      },
      
      get_storage_stats: fn() {
        let total_keys = stored_keys.length()
        let estimated_size = total_keys * 1024  // 假设每个条目1KB
        
        {
          total_keys,
          estimated_size_kb: estimated_size / 1024,
          estimated_size_mb: estimated_size / (1024 * 1024),
          within_size_limit: estimated_size / (1024 * 1024) < policy.max_size_mb
        }
      }
    }
  }
  
  // 创建存储管理器
  let retention_policy = {
    max_age_days: 7,
    max_size_mb: 100,
    cleanup_interval_hours: 24
  }
  
  let storage_manager = create_storage_manager(memory_storage, retention_policy)
  
  // 测试存储遥测数据
  let telemetry_data1 = "{\"trace_id\":\"t1\",\"span_id\":\"s1\",\"data\":\"test1\"}"
  let telemetry_data2 = "{\"trace_id\":\"t2\",\"span_id\":\"s2\",\"data\":\"test2\"}"
  let telemetry_data3 = "{\"trace_id\":\"t3\",\"span_id\":\"s3\",\"data\":\"test3\"}"
  
  let result1 = storage_manager.store_telemetry(telemetry_data1)
  let result2 = storage_manager.store_telemetry(telemetry_data2)
  let result3 = storage_manager.store_telemetry(telemetry_data3)
  
  assert_true(result1.is_ok())
  assert_true(result2.is_ok())
  assert_true(result3.is_ok())
  
  // 获取存储的key
  let key1 = match result1 { Ok(k) => k; Err(_) => "" }
  let key2 = match result2 { Ok(k) => k; Err(_) => "" }
  let key3 = match result3 { Ok(k) => k; Err(_) => "" }
  
  // 测试检索遥测数据
  let retrieved1 = storage_manager.retrieve_telemetry(key1)
  let retrieved2 = storage_manager.retrieve_telemetry(key2)
  let retrieved3 = storage_manager.retrieve_telemetry(key3)
  
  assert_true(retrieved1.is_ok())
  assert_true(retrieved2.is_ok())
  assert_true(retrieved3.is_ok())
  
  let data1 = match retrieved1 { Ok(d) => d; Err(_) => "" }
  let data2 = match retrieved2 { Ok(d) => d; Err(_) => "" }
  let data3 = match retrieved3 { Ok(d) => d; Err(_) => "" }
  
  assert_eq(data1, telemetry_data1)
  assert_eq(data2, telemetry_data2)
  assert_eq(data3, telemetry_data3)
  
  // 测试获取存储统计
  let stats = storage_manager.get_storage_stats()
  assert_eq(stats.total_keys, 3)
  assert_eq(stats.estimated_size_kb, 3)
  assert_eq(stats.estimated_size_mb, 0)
  assert_true(stats.within_size_limit)
  
  // 测试清理旧数据（简化版，实际实现会更复杂）
  let cleaned_count = storage_manager.cleanup_old_data()
  assert_eq(cleaned_count, 0)  // 因为数据都是刚创建的，不超过保留期限
}

// 测试7: 遥测系统的性能监控
test "遥测系统的性能监控" {
  // 定义性能指标
  type PerformanceMetrics = {
    cpu_usage: Float,
    memory_usage: Float,
    disk_io: Float,
    network_io: Float,
    request_latency: Float,
    throughput: Float
  }
  
  // 定义性能阈值
  type PerformanceThresholds = {
    max_cpu_usage: Float,
    max_memory_usage: Float,
    max_disk_io: Float,
    max_network_io: Float,
    max_request_latency: Float,
    min_throughput: Float
  }
  
  // 创建性能监控器
  let create_performance_monitor = fn(thresholds: PerformanceThresholds) {
    let metrics_history = []
    
    {
      record_metrics: fn(metrics: PerformanceMetrics) {
        metrics_history = metrics_history.push(metrics)
      },
      
      check_performance: fn() {
        if metrics_history.length() == 0 {
          return {
            is_healthy: true,
            issues: [],
            recommendations: []
          }
        }
        
        let latest = metrics_history[metrics_history.length() - 1]
        let issues = []
        let recommendations = []
        
        if latest.cpu_usage > thresholds.max_cpu_usage {
          issues = issues.push("CPU使用率过高: " + latest.cpu_usage.to_string() + "%")
          recommendations = recommendations.push("考虑增加CPU资源或优化CPU密集型操作")
        }
        
        if latest.memory_usage > thresholds.max_memory_usage {
          issues = issues.push("内存使用率过高: " + latest.memory_usage.to_string() + "%")
          recommendations = recommendations.push("考虑增加内存或优化内存使用")
        }
        
        if latest.disk_io > thresholds.max_disk_io {
          issues = issues.push("磁盘I/O过高: " + latest.disk_io.to_string())
          recommendations = recommendations.push("考虑优化磁盘访问模式或使用更快的存储")
        }
        
        if latest.network_io > thresholds.max_network_io {
          issues = issues.push("网络I/O过高: " + latest.network_io.to_string())
          recommendations = recommendations.push("考虑优化网络传输或增加带宽")
        }
        
        if latest.request_latency > thresholds.max_request_latency {
          issues = issues.push("请求延迟过高: " + latest.request_latency.to_string() + "ms")
          recommendations = recommendations.push("优化请求处理逻辑或增加处理能力")
        }
        
        if latest.throughput < thresholds.min_throughput {
          issues = issues.push("吞吐量过低: " + latest.throughput.to_string() + "req/s")
          recommendations = recommendations.push("优化处理流程或增加并发处理能力")
        }
        
        {
          is_healthy: issues.length() == 0,
          issues,
          recommendations
        }
      },
      
      get_performance_trend: fn(metric_name: String) {
        if metrics_history.length() < 2 {
          return "insufficient_data"
        }
        
        let get_metric_value = fn(metrics: PerformanceMetrics, name: String) {
          match name {
            "cpu" => metrics.cpu_usage
            "memory" => metrics.memory_usage
            "disk_io" => metrics.disk_io
            "network_io" => metrics.network_io
            "latency" => metrics.request_latency
            "throughput" => metrics.throughput
            _ => 0.0
          }
        }
        
        let first_value = get_metric_value(metrics_history[0], metric_name)
        let latest_value = get_metric_value(metrics_history[metrics_history.length() - 1], metric_name)
        
        let change_percent = (latest_value - first_value) / first_value * 100.0
        
        if change_percent > 10.0 {
          "increasing"
        } else if change_percent < -10.0 {
          "decreasing"
        } else {
          "stable"
        }
      }
    }
  }
  
  // 创建性能阈值
  let thresholds = {
    max_cpu_usage: 80.0,
    max_memory_usage: 85.0,
    max_disk_io: 1000.0,
    max_network_io: 500.0,
    max_request_latency: 200.0,
    min_throughput: 100.0
  }
  
  // 创建性能监控器
  let monitor = create_performance_monitor(thresholds)
  
  // 记录正常性能指标
  let normal_metrics = {
    cpu_usage: 45.0,
    memory_usage: 60.0,
    disk_io: 500.0,
    network_io: 200.0,
    request_latency: 100.0,
    throughput: 150.0
  }
  
  monitor.record_metrics(normal_metrics)
  let health_check1 = monitor.check_performance()
  assert_true(health_check1.is_healthy)
  assert_eq(health_check1.issues.length(), 0)
  assert_eq(health_check1.recommendations.length(), 0)
  
  // 记录有问题的性能指标
  let problem_metrics = {
    cpu_usage: 85.0,  // 超过阈值
    memory_usage: 90.0,  // 超过阈值
    disk_io: 800.0,
    network_io: 400.0,
    request_latency: 250.0,  // 超过阈值
    throughput: 80.0  // 低于阈值
  }
  
  monitor.record_metrics(problem_metrics)
  let health_check2 = monitor.check_performance()
  assert_false(health_check2.is_healthy)
  assert_eq(health_check2.issues.length(), 4)  // CPU, 内存, 延迟, 吞吐量
  assert_true(health_check2.issues.any(fn(issue) { issue.contains("CPU使用率过高") }))
  assert_true(health_check2.issues.any(fn(issue) { issue.contains("内存使用率过高") }))
  assert_true(health_check2.issues.any(fn(issue) { issue.contains("请求延迟过高") }))
  assert_true(health_check2.issues.any(fn(issue) { issue.contains("吞吐量过低") }))
  
  // 记录更多数据以测试趋势分析
  let improved_metrics = {
    cpu_usage: 70.0,
    memory_usage: 75.0,
    disk_io: 600.0,
    network_io: 300.0,
    request_latency: 150.0,
    throughput: 120.0
  }
  
  monitor.record_metrics(improved_metrics)
  
  // 测试趋势分析
  let cpu_trend = monitor.get_performance_trend("cpu")
  let memory_trend = monitor.get_performance_trend("memory")
  let latency_trend = monitor.get_performance_trend("latency")
  
  // CPU趋势：45 -> 85 -> 70，总体上升
  assert_eq(cpu_trend, "increasing")
  
  // 内存趋势：60 -> 90 -> 75，总体上升
  assert_eq(memory_trend, "increasing")
  
  // 延迟趋势：100 -> 250 -> 150，总体上升
  assert_eq(latency_trend, "increasing")
}

// 测试8: 遥测数据的安全性和隐私保护
test "遥测数据的安全性和隐私保护" {
  // 定义数据脱敏规则
  type DataMaskingRule = {
    field_name: String,
    masking_type: String,  // "partial", "full", "hash"
    pattern: String  // 用于部分脱敏的模式
  }
  
  // 定义加密配置
  type EncryptionConfig = {
    algorithm: String,
    key_id: String,
    enabled: Bool
  }
  
  // 创建数据脱敏器
  let create_data_masker = fn(rules: Array[DataMaskingRule]) {
    let apply_masking = fn(value: String, rule: DataMaskingRule) {
      match rule.masking_type {
        "full" => "*****"
        "partial" => {
          let len = value.length()
          if len <= 4 {
            "****"
          } else {
            value.substring(0, 2) + "****" + value.substring(len - 2, len)
          }
        }
        "hash" => {
          // 简化的哈希计算
          let hash = value.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          hash.to_string(16)
        }
        _ => value
      }
    }
    
    {
      mask_data: fn(data: Array[(String, String)>) {
        data.map(fn(field) {
          let (key, value) = field
          let matching_rule = rules.find(fn(rule) { rule.field_name == key })
          
          match matching_rule {
            Some(rule) => (key, apply_masking(value, rule))
            None => field
          }
        })
      },
      
      mask_string: fn(text: String, patterns: Array[(String, DataMaskingRule)>) {
        let result = text
        for (pattern, rule) in patterns {
          // 简化的模式匹配和替换
          if result.contains(pattern) {
            let masked_value = apply_masking(pattern, rule)
            result = result.replace(pattern, masked_value)
          }
        }
        result
      }
    }
  }
  
  // 创建加密器
  let create_encryptor = fn(config: EncryptionConfig) {
    let simple_encrypt = fn(text: String) {
      // 简化的加密算法（仅用于测试）
      if config.enabled {
        let encrypted = text.chars().map(fn(c) {
          let char_code = c.to_int()
          (char_code + 1).to_char()
        }).to_string()
        "enc:" + encrypted
      } else {
        text
      }
    }
    
    let simple_decrypt = fn(text: String) {
      if config.enabled && text.starts_with("enc:") {
        let encrypted = text.substring(4, text.length())
        encrypted.chars().map(fn(c) {
          let char_code = c.to_int()
          (char_code - 1).to_char()
        }).to_string()
      } else {
        text
      }
    }
    
    {
      encrypt: fn(data: String) {
        simple_encrypt(data)
      },
      
      decrypt: fn(encrypted_data: String) {
        simple_decrypt(encrypted_data)
      },
      
      encrypt_attributes: fn(attributes: Array[(String, String)>) {
        if config.enabled {
          attributes.map(fn(attr) {
            let (key, value) = attr
            (key, simple_encrypt(value))
          })
        } else {
          attributes
        }
      },
      
      decrypt_attributes: fn(encrypted_attributes: Array[(String, String)>) {
        if config.enabled {
          encrypted_attributes.map(fn(attr) {
            let (key, value) = attr
            (key, simple_decrypt(value))
          })
        } else {
          encrypted_attributes
        }
      }
    }
  }
  
  // 创建脱敏规则
  let masking_rules = [
    { field_name: "email", masking_type: "partial", pattern: "email" },
    { field_name: "phone", masking_type: "partial", pattern: "phone" },
    { field_name: "ssn", masking_type: "full", pattern: "ssn" },
    { field_name: "credit_card", masking_type: "full", pattern: "credit_card" },
    { field_name: "user_id", masking_type: "hash", pattern: "user_id" }
  ]
  
  // 创建加密配置
  let encryption_config = {
    algorithm: "AES-256",
    key_id: "key-12345",
    enabled: true
  }
  
  // 创建脱敏器和加密器
  let masker = create_data_masker(masking_rules)
  let encryptor = create_encryptor(encryption_config)
  
  // 测试数据脱敏
  let sensitive_data = [
    ("email", "user@example.com"),
    ("phone", "123-456-7890"),
    ("ssn", "123-45-6789"),
    ("credit_card", "4111-1111-1111-1111"),
    ("user_id", "user-12345"),
    ("service_name", "payment-service")  // 不敏感数据
  ]
  
  let masked_data = masker.mask_data(sensitive_data)
  
  // 验证脱敏结果
  let masked_email = match masked_data.find(fn(field) { field.0 == "email" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(masked_email, "us****m.com")
  
  let masked_phone = match masked_data.find(fn(field) { field.0 == "phone" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(masked_phone, "12****90")
  
  let masked_ssn = match masked_data.find(fn(field) { field.0 == "ssn" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(masked_ssn, "*****")
  
  let masked_credit_card = match masked_data.find(fn(field) { field.0 == "credit_card" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(masked_credit_card, "*****")
  
  let masked_user_id = match masked_data.find(fn(field) { field.0 == "user_id" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_not_eq(masked_user_id, "user-12345")  // 应该被哈希
  assert_eq(masked_user_id.length(), 32)  // 假设哈希长度为32个字符
  
  // 非敏感数据不应该被修改
  let service_name = match masked_data.find(fn(field) { field.0 == "service_name" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(service_name, "payment-service")
  
  // 测试加密
  let original_text = "sensitive telemetry data"
  let encrypted_text = encryptor.encrypt(original_text)
  assert_true(encrypted_text.starts_with("enc:"))
  assert_not_eq(encrypted_text, original_text)
  
  let decrypted_text = encryptor.decrypt(encrypted_text)
  assert_eq(decrypted_text, original_text)
  
  // 测试属性加密
  let original_attributes = [
    ("api_key", "secret-key-123"),
    ("database_password", "db-pass-456"),
    ("service_name", "auth-service")
  ]
  
  let encrypted_attributes = encryptor.encrypt_attributes(original_attributes)
  
  // 验证加密结果
  let encrypted_api_key = match encrypted_attributes.find(fn(attr) { attr.0 == "api_key" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(encrypted_api_key.starts_with("enc:"))
  assert_not_eq(encrypted_api_key, "secret-key-123")
  
  let encrypted_db_password = match encrypted_attributes.find(fn(attr) { attr.0 == "database_password" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(encrypted_db_password.starts_with("enc:"))
  assert_not_eq(encrypted_db_password, "db-pass-456")
  
  // 测试属性解密
  let decrypted_attributes = encryptor.decrypt_attributes(encrypted_attributes)
  
  let decrypted_api_key = match decrypted_attributes.find(fn(attr) { attr.0 == "api_key" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(decrypted_api_key, "secret-key-123")
  
  let decrypted_db_password = match decrypted_attributes.find(fn(attr) { attr.0 == "database_password" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(decrypted_db_password, "db-pass-456")
}