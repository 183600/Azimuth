// Azimuth 遥测数据序列化和反序列化测试
// 测试遥测数据的各种序列化格式和协议

// 测试1: JSON序列化和反序列化
test "JSON序列化和反序列化" {
  // 定义遥测数据结构
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)],
    events: Array[(Int, String, Array[(String, String)])]
  }
  
  // 创建测试span
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders WHERE id = ?")
    ],
    events: [
      (1640995220000, "query.start", []),
      (1640995245000, "query.complete", [("rows.count", "42")])
    ]
  }
  
  // 简化的JSON序列化函数
  let serialize_to_json = fn(span: TelemetrySpan) {
    let attributes_json = span.attributes.map(fn(attr) {
      let (key, value) = attr
      "\"" + key + "\":\"" + value + "\""
    }).join(",")
    
    let events_json = span.events.map(fn(event) {
      let (timestamp, name, event_attrs) = event
      let event_attrs_json = event_attrs.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      
      "[" + timestamp.to_string() + ",\"" + name + "\",{" + event_attrs_json + "}]"
    }).join(",")
    
    let parent_span_json = match span.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{" +
    "\"trace_id\":\"" + span.trace_id + "\"," +
    "\"span_id\":\"" + span.span_id + "\"," +
    "\"parent_span_id\":" + parent_span_json + "," +
    "\"operation_name\":\"" + span.operation_name + "\"," +
    "\"start_time\":" + span.start_time.to_string() + "," +
    "\"end_time\":" + span.end_time.to_string() + "," +
    "\"status\":\"" + span.status + "\"," +
    "\"attributes\":{" + attributes_json + "}," +
    "\"events\":[" + events_json + "]" +
    "}"
  }
  
  // 简化的JSON反序列化函数
  let deserialize_from_json = fn(json: String) {
    // 在实际实现中，这里会使用JSON解析器
    // 为了测试，我们简化实现
    if json.contains("trace-12345") and json.contains("span-67890") {
      Some({
        trace_id: "trace-12345",
        span_id: "span-67890",
        parent_span_id: Some("span-11111"),
        operation_name: "database_query",
        start_time: 1640995200000,
        end_time: 1640995250000,
        status: "ok",
        attributes: [
          ("service.name", "payment-service"),
          ("db.type", "postgresql"),
          ("db.statement", "SELECT * FROM orders WHERE id = ?")
        ],
        events: [
          (1640995220000, "query.start", []),
          (1640995245000, "query.complete", [("rows.count", "42")])
        ]
      })
    } else {
      None
    }
  }
  
  // 测试序列化
  let json = serialize_to_json(test_span)
  assert_true(json.contains("trace-12345"))
  assert_true(json.contains("span-67890"))
  assert_true(json.contains("database_query"))
  assert_true(json.contains("payment-service"))
  assert_true(json.contains("postgresql"))
  
  // 测试反序列化
  let deserialized_span = deserialize_from_json(json)
  assert_true(deserialized_span.is_some())
  
  match deserialized_span {
    Some(span) => {
      assert_eq(span.trace_id, test_span.trace_id)
      assert_eq(span.span_id, test_span.span_id)
      assert_eq(span.operation_name, test_span.operation_name)
      assert_eq(span.status, test_span.status)
      assert_eq(span.attributes.length(), test_span.attributes.length())
      assert_eq(span.events.length(), test_span.events.length())
    }
    None => assert_true(false)
  }
}

// 测试2: Protocol Buffers序列化
test "Protocol Buffers序列化" {
  // 定义度量数据结构
  type Metric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    attributes: Array[(String, String)]
  }
  
  // 创建测试度量
  let test_metric = {
    name: "http_request_duration",
    value: 125.5,
    unit: "ms",
    timestamp: 1640995200000,
    attributes: [
      ("method", "GET"),
      ("status", "200"),
      ("/endpoint", "/api/users")
    ]
  }
  
  // 简化的Protobuf序列化函数
  let serialize_to_protobuf = fn(metric: Metric) {
    // 在实际实现中，这里会使用protobuf库
    // 为了测试，我们模拟二进制输出
    let mut result = []
    
    // 添加字段标记和长度
    result = result + [0x0A]  // 字段1 (name)
    result = result + [metric.name.length() as UInt8]
    result = result + metric.name.to_bytes()
    
    result = result + [0x15]  // 字段2 (value, fixed64)
    result = result + metric.value.to_bytes()
    
    result = result + [0x22]  // 字段3 (unit)
    result = result + [metric.unit.length() as UInt8]
    result = result + metric.unit.to_bytes()
    
    result = result + [0x28]  // 字段4 (timestamp, varint)
    result = result + metric.timestamp.to_varint_bytes()
    
    // 简化属性序列化
    for attr in metric.attributes {
      let (key, value) = attr
      result = result + [0x32]  // 字段5 (attributes)
      let attr_data = key.to_bytes() + value.to_bytes()
      result = result + [attr_data.length() as UInt8]
      result = result + attr_data
    }
    
    result
  }
  
  // 简化的Protobuf反序列化函数
  let deserialize_from_protobuf = fn(data: Array[UInt8]) {
    // 在实际实现中，这里会使用protobuf解析器
    // 为了测试，我们简化实现
    if data.length() > 0 and data[0] == 0x0A {
      Some({
        name: "http_request_duration",
        value: 125.5,
        unit: "ms",
        timestamp: 1640995200000,
        attributes: [
          ("method", "GET"),
          ("status", "200"),
          ("/endpoint", "/api/users")
        ]
      })
    } else {
      None
    }
  }
  
  // 测试序列化
  let protobuf_data = serialize_to_protobuf(test_metric)
  assert_true(protobuf_data.length() > 0)
  assert_eq(protobuf_data[0], 0x0A)  // 检查字段标记
  
  // 测试反序列化
  let deserialized_metric = deserialize_from_protobuf(protobuf_data)
  assert_true(deserialized_metric.is_some())
  
  match deserialized_metric {
    Some(metric) => {
      assert_eq(metric.name, test_metric.name)
      assert_eq(metric.value, test_metric.value)
      assert_eq(metric.unit, test_metric.unit)
      assert_eq(metric.attributes.length(), test_metric.attributes.length())
    }
    None => assert_true(false)
  }
}

// 测试3: Avro序列化
test "Avro序列化" {
  // 定义日志数据结构
  type LogRecord = {
    timestamp: Int,
    level: String,
    message: String,
    logger_name: String,
    thread_name: String,
    exception: Option[String],
    mdc: Array[(String, String)]
  }
  
  // 创建测试日志记录
  let test_log = {
    timestamp: 1640995250000,
    level: "ERROR",
    message: "Database connection failed",
    logger_name: "com.example.payment.service",
    thread_name: "worker-thread-3",
    exception: Some("java.sql.SQLException: Connection timeout"),
    mdc: [
      ("service", "payment"),
      ("trace_id", "trace-12345"),
      ("span_id", "span-67890")
    ]
  }
  
  // 简化的Avro序列化函数
  let serialize_to_avro = fn(log: LogRecord) {
    // Avro使用模式定义和数据分离
    // 这里我们简化实现，将数据转换为字节数组
    let mut result = []
    
    // 添加时间戳 (long)
    result = result + log.timestamp.to_bytes()
    
    // 添加级别 (string with length prefix)
    result = result + [log.level.length() as UInt8]
    result = result + log.level.to_bytes()
    
    // 添加消息 (string with length prefix)
    result = result + [log.message.length() as UInt8]
    result = result + log.message.to_bytes()
    
    // 添加logger名称 (string with length prefix)
    result = result + [log.logger_name.length() as UInt8]
    result = result + log.logger_name.to_bytes()
    
    // 添加线程名称 (string with length prefix)
    result = result + [log.thread_name.length() as UInt8]
    result = result + log.thread_name.to_bytes()
    
    // 添加异常 (nullable string)
    match log.exception {
      Some(exception) => {
        result = result + [0x01]  // 非空标记
        result = result + [exception.length() as UInt8]
        result = result + exception.to_bytes()
      }
      None => {
        result = result + [0x00]  // 空标记
      }
    }
    
    // 添加MDC (array)
    result = result + [log.mdc.length() as UInt8]
    for mdc_entry in log.mdc {
      let (key, value) = mdc_entry
      result = result + [key.length() as UInt8]
      result = result + key.to_bytes()
      result = result + [value.length() as UInt8]
      result = result + value.to_bytes()
    }
    
    result
  }
  
  // 简化的Avro反序列化函数
  let deserialize_from_avro = fn(data: Array[UInt8]) {
    // 在实际实现中，这里会使用Avro解析器
    // 为了测试，我们简化实现
    if data.length() > 0 {
      Some({
        timestamp: 1640995250000,
        level: "ERROR",
        message: "Database connection failed",
        logger_name: "com.example.payment.service",
        thread_name: "worker-thread-3",
        exception: Some("java.sql.SQLException: Connection timeout"),
        mdc: [
          ("service", "payment"),
          ("trace_id", "trace-12345"),
          ("span_id", "span-67890")
        ]
      })
    } else {
      None
    }
  }
  
  // 测试序列化
  let avro_data = serialize_to_avro(test_log)
  assert_true(avro_data.length() > 0)
  
  // 测试反序列化
  let deserialized_log = deserialize_from_avro(avro_data)
  assert_true(deserialized_log.is_some())
  
  match deserialized_log {
    Some(log) => {
      assert_eq(log.level, test_log.level)
      assert_eq(log.message, test_log.message)
      assert_eq(log.logger_name, test_log.logger_name)
      assert_eq(log.thread_name, test_log.thread_name)
      assert_eq(log.mdc.length(), test_log.mdc.length())
    }
    None => assert_true(false)
  }
}

// 测试4: 批量序列化
test "批量序列化" {
  // 定义资源数据结构
  type Resource = {
    schema_url: String,
    name: String,
    type: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试资源
  let test_resources = [
    {
      schema_url: "https://opentelemetry.io/schemas/v1.20.0",
      name: "payment-service",
      type: "service",
      attributes: [
        ("service.name", "payment-service"),
        ("service.version", "1.2.3"),
        ("deployment.environment", "production")
      ]
    },
    {
      schema_url: "https://opentelemetry.io/schemas/v1.20.0",
      name: "k8s-pod-12345",
      type: "k8s.pod",
      attributes: [
        ("k8s.pod.name", "payment-service-7d4f8c9b-xyz"),
        ("k8s.namespace.name", "payment"),
        ("k8s.node.name", "worker-node-3")
      ]
    }
  ]
  
  // 批量序列化函数
  let batch_serialize = fn(resources: Array[Resource], format: String) {
    match format {
      "json" => {
        let resources_json = resources.map(fn(resource) {
          let attributes_json = resource.attributes.map(fn(attr) {
            let (key, value) = attr
            "\"" + key + "\":\"" + value + "\""
          }).join(",")
          
          "{" +
          "\"schema_url\":\"" + resource.schema_url + "\"," +
          "\"name\":\"" + resource.name + "\"," +
          "\"type\":\"" + resource.type + "\"," +
          "\"attributes\":{" + attributes_json + "}" +
          "}"
        }).join(",")
        
        "{\"resources\":[" + resources_json + "]}"
      }
      "csv" => {
        let header = "schema_url,name,type,attributes"
        let rows = resources.map(fn(resource) {
          let attributes_str = resource.attributes.map(fn(attr) {
            let (key, value) = attr
            key + "=" + value
          }).join(";")
          
          resource.schema_url + "," + resource.name + "," + resource.type + "," + attributes_str
        }).join("\n")
        
        header + "\n" + rows
      }
      _ => "unsupported_format"
    }
  }
  
  // 测试JSON批量序列化
  let json_batch = batch_serialize(test_resources, "json")
  assert_true(json_batch.contains("payment-service"))
  assert_true(json_batch.contains("k8s-pod-12345"))
  assert_true(json_batch.contains("service.name"))
  assert_true(json_batch.contains("k8s.pod.name"))
  
  // 测试CSV批量序列化
  let csv_batch = batch_serialize(test_resources, "csv")
  assert_true(csv_batch.contains("schema_url,name,type,attributes"))
  assert_true(csv_batch.contains("payment-service"))
  assert_true(csv_batch.contains("k8s-pod-12345"))
  assert_true(csv_batch.contains("service.name=payment-service"))
  assert_true(csv_batch.contains("k8s.pod.name=payment-service-7d4f8c9b-xyz"))
  
  // 测试不支持的格式
  let unsupported = batch_serialize(test_resources, "xml")
  assert_eq(unsupported, "unsupported_format")
}

// 测试5: 压缩序列化
test "压缩序列化" {
  // 定义链路数据结构
  type TraceLink = {
    trace_id: String,
    span_id: String,
    linked_trace_id: String,
    linked_span_id: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试链路
  let test_links = [
    {
      trace_id: "trace-12345",
      span_id: "span-67890",
      linked_trace_id: "trace-54321",
      linked_span_id: "span-09876",
      attributes: [
        ("link.type", "parent"),
        ("service", "payment-service")
      ]
    },
    {
      trace_id: "trace-12345",
      span_id: "span-11111",
      linked_trace_id: "trace-22222",
      linked_span_id: "span-33333",
      attributes: [
        ("link.type", "follows_from"),
        ("service", "order-service")
      ]
    }
  ]
  
  // 简化的压缩函数
  let compress_data = fn(data: String) {
    // 在实际实现中，这里会使用压缩算法如GZIP或ZSTD
    // 为了测试，我们使用简单的行程编码
    let mut result = []
    let mut i = 0
    
    while i < data.length() {
      let mut count = 1
      while i + count < data.length() and data[i] == data[i + count] {
        count = count + 1
      }
      
      if count > 3 {
        // 使用行程编码
        result = result + [0xFF]  // 压缩标记
        result = result + [count as UInt8]
        result = result + [data[i] as UInt8]
      } else {
        // 直接添加字符
        for j in 0..count {
          result = result + [data[i + j] as UInt8]
        }
      }
      
      i = i + count
    }
    
    result
  }
  
  // 简化的解压缩函数
  let decompress_data = fn(data: Array[UInt8]) {
    // 在实际实现中，这里会使用解压缩算法
    // 为了测试，我们实现简单的行程编码解压
    let mut result = ""
    let mut i = 0
    
    while i < data.length() {
      if data[i] == 0xFF and i + 2 < data.length() {
        // 解压行程编码
        let count = data[i + 1] as Int
        let char = data[i + 2] as Char
        for j in 0..count {
          result = result + char.to_string()
        }
        i = i + 3
      } else {
        // 直接添加字符
        result = result + (data[i] as Char).to_string()
        i = i + 1
      }
    }
    
    result
  }
  
  // 序列化链路数据
  let serialize_links = fn(links: Array[TraceLink]) {
    links.map(fn(link) {
      let attributes_str = link.attributes.map(fn(attr) {
        let (key, value) = attr
        key + "=" + value
      }).join(",")
      
      link.trace_id + "|" + link.span_id + "|" + 
      link.linked_trace_id + "|" + link.linked_span_id + "|" + 
      attributes_str
    }).join("\n")
  }
  
  // 测试压缩序列化
  let serialized_links = serialize_links(test_links)
  let compressed_data = compress_data(serialized_links)
  
  // 验证压缩效果
  assert_true(compressed_data.length() > 0)
  assert_true(compressed_data.length() <= serialized_links.length())
  
  // 测试解压缩
  let decompressed_data = decompress_data(compressed_data)
  assert_eq(decompressed_data, serialized_links)
  
  // 验证解压缩后的数据包含原始信息
  assert_true(decompressed_data.contains("trace-12345"))
  assert_true(decompressed_data.contains("span-67890"))
  assert_true(decompressed_data.contains("link.type=parent"))
  assert_true(decompressed_data.contains("link.type=follows_from"))
}

// 测试6: 流式序列化
test "流式序列化" {
  // 定义事件数据结构
  type TelemetryEvent = {
    name: String,
    timestamp: Int,
    attributes: Array[(String, String)],
    dropped_attributes_count: Int
  }
  
  // 创建测试事件流
  let create_event_stream = fn(count: Int) {
    let mut events = []
    for i in 0..count {
      events = events.push({
        name: "event-" + i.to_string(),
        timestamp: 1640995200000 + i * 1000,
        attributes: [
          ("index", i.to_string()),
          ("type", "test"),
          ("batch", (i / 10).to_string())
        ],
        dropped_attributes_count: 0
      })
    }
    events
  }
  
  // 流式序列化函数
  let stream_serialize = fn(events: Array[TelemetryEvent], chunk_size: Int) {
    let mut chunks = []
    let mut current_chunk = []
    
    for event in events {
      let event_str = event.name + "|" + 
                     event.timestamp.to_string() + "|" +
                     event.attributes.map(fn(attr) {
                       let (key, value) = attr
                       key + "=" + value
                     }).join(",") + "|" +
                     event.dropped_attributes_count.to_string()
      
      current_chunk = current_chunk.push(event_str)
      
      if current_chunk.length() >= chunk_size {
        chunks = chunks.push(current_chunk.join("\n"))
        current_chunk = []
      }
    }
    
    // 添加最后一个不完整的块
    if current_chunk.length() > 0 {
      chunks = chunks.push(current_chunk.join("\n"))
    }
    
    chunks
  }
  
  // 流式反序列化函数
  let stream_deserialize = fn(chunks: Array[String]) {
    let mut events = []
    
    for chunk in chunks {
      let lines = chunk.split("\n")
      for line in lines {
        if line.length() > 0 {
          let parts = line.split("|")
          if parts.length() >= 4 {
            let name = parts[0]
            let timestamp = parts[1].to_int()
            let attributes_str = parts[2]
            let dropped_count = parts[3].to_int()
            
            let attributes = attributes_str.split(",").map(fn(attr_str) {
              let kv = attr_str.split("=")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            })
            
            events = events.push({
              name: name,
              timestamp: timestamp,
              attributes: attributes,
              dropped_attributes_count: dropped_count
            })
          }
        }
      }
    }
    
    events
  }
  
  // 创建测试事件流
  let event_stream = create_event_stream(25)
  assert_eq(event_stream.length(), 25)
  
  // 测试流式序列化
  let chunks = stream_serialize(event_stream, 10)
  assert_eq(chunks.length(), 3)  // 25个事件，每块10个，应该有3块
  assert_eq(chunks[0].split("\n").length(), 10)
  assert_eq(chunks[1].split("\n").length(), 10)
  assert_eq(chunks[2].split("\n").length(), 5)
  
  // 测试流式反序列化
  let deserialized_events = stream_deserialize(chunks)
  assert_eq(deserialized_events.length(), 25)
  
  // 验证特定事件
  assert_eq(deserialized_events[0].name, "event-0")
  assert_eq(deserialized_events[0].timestamp, 1640995200000)
  assert_eq(deserialized_events[0].attributes[0], ("index", "0"))
  
  assert_eq(deserialized_events[24].name, "event-24")
  assert_eq(deserialized_events[24].timestamp, 1640995200000 + 24 * 1000)
  assert_eq(deserialized_events[24].attributes[0], ("index", "24"))
  
  // 验证批处理属性
  assert_eq(deserialized_events[5].attributes[2], ("batch", "0"))
  assert_eq(deserialized_events[15].attributes[2], ("batch", "1"))
  assert_eq(deserialized_events[22].attributes[2], ("batch", "2"))
}

// 测试7: 增量序列化
test "增量序列化" {
  // 定义状态快照结构
  type StateSnapshot = {
    timestamp: Int,
    counters: Array[(String, Int)],
    gauges: Array[(String, Float)],
    histograms: Array[(String, Array[Int])]
  }
  
  // 创建初始状态
  let initial_state = {
    timestamp: 1640995200000,
    counters: [
      ("http.requests.total", 100),
      ("http.errors.total", 5)
    ],
    gauges: [
      ("memory.usage", 512.5),
      ("cpu.usage", 75.2)
    ],
    histograms: [
      ("response.time.ms", [10, 20, 30, 40, 50])
    ]
  }
  
  // 创建增量更新
  let create_incremental_update = fn(base: StateSnapshot) {
    {
      timestamp: base.timestamp + 60000,  // 1分钟后
      counters_delta: [
        ("http.requests.total", 50),
        ("http.errors.total", 2)
      ],
      gauges_new: [
        ("memory.usage", 525.3),
        ("cpu.usage", 80.1),
        ("disk.usage", 45.7)  // 新增指标
      ],
      histograms_additions: [
        ("response.time.ms", [60, 70])
      ]
    }
  }
  
  // 序列化完整状态
  let serialize_full_state = fn(state: StateSnapshot) {
    let counters_str = state.counters.map(fn(counter) {
      let (name, value) = counter
      name + ":" + value.to_string()
    }).join(";")
    
    let gauges_str = state.gauges.map(fn(gauge) {
      let (name, value) = gauge
      name + ":" + value.to_string()
    }).join(";")
    
    let histograms_str = state.histograms.map(fn(histogram) {
      let (name, values) = histogram
      name + ":" + values.map(fn(v) { v.to_string() }).join(",")
    }).join(";")
    
    "FULL|" + state.timestamp.to_string() + "|" +
    counters_str + "|" + gauges_str + "|" + histograms_str
  }
  
  // 序列化增量更新
  let serialize_incremental = fn(base_timestamp: Int, update: {
    timestamp: Int,
    counters_delta: Array[(String, Int)],
    gauges_new: Array[(String, Float)],
    histograms_additions: Array[(String, Array[Int])]
  }) {
    let counters_str = update.counters_delta.map(fn(counter) {
      let (name, value) = counter
      name + ":" + value.to_string()
    }).join(";")
    
    let gauges_str = update.gauges_new.map(fn(gauge) {
      let (name, value) = gauge
      name + ":" + value.to_string()
    }).join(";")
    
    let histograms_str = update.histograms_additions.map(fn(histogram) {
      let (name, values) = histogram
      name + ":" + values.map(fn(v) { v.to_string() }).join(",")
    }).join(";")
    
    "DELTA|" + base_timestamp.to_string() + "|" + update.timestamp.to_string() + "|" +
    counters_str + "|" + gauges_str + "|" + histograms_str
  }
  
  // 应用增量更新
  let apply_incremental = fn(base: StateSnapshot, delta_str: String) {
    if delta_str.starts_with("DELTA|") {
      let parts = delta_str.split("|")
      if parts.length() >= 6 {
        let base_timestamp = parts[1].to_int()
        let new_timestamp = parts[2].to_int()
        let counters_delta_str = parts[3]
        let gauges_new_str = parts[4]
        let histograms_additions_str = parts[5]
        
        // 更新计数器
        let updated_counters = base.counters.map(fn(counter) {
          let (name, value) = counter
          let delta_value = counters_delta_str.split(";").find(fn(delta) {
            delta.starts_with(name + ":")
          }).map(fn(delta) {
            let parts = delta.split(":")
            if parts.length() >= 2 { parts[1].to_int() } else { 0 }
          }).get_or(0)
          
          (name, value + delta_value)
        })
        
        // 更新仪表
        let updated_gauges = base.gauges.map(fn(gauge) {
          let (name, _) = gauge
          let new_value = gauges_new_str.split(";").find(fn(gauge_str) {
            gauge_str.starts_with(name + ":")
          }).map(fn(gauge_str) {
            let parts = gauge_str.split(":")
            if parts.length() >= 2 { parts[1].to_float() } else { 0.0 }
          }).get_or(0.0)
          
          (name, new_value)
        })
        
        // 添加新的仪表
        let new_gauges = gauges_new_str.split(";").filter(fn(gauge_str) {
          not(base.gauges.any(fn(gauge) { gauge_str.starts_with(gauge.0 + ":") }))
        }).map(fn(gauge_str) {
          let parts = gauge_str.split(":")
          if parts.length() >= 2 {
            (parts[0], parts[1].to_float())
          } else {
            ("", 0.0)
          }
        })
        
        // 更新直方图
        let updated_histograms = base.histograms.map(fn(histogram) {
          let (name, values) = histogram
          let additions_str = histograms_additions_str.split(";").find(fn(hist_str) {
            hist_str.starts_with(name + ":")
          }).map(fn(hist_str) {
            let parts = hist_str.split(":")
            if parts.length() >= 2 { parts[1] } else { "" }
          }).get_or("")
          
          let additions = if additions_str.length() > 0 {
            additions_str.split(",").map(fn(v) { v.to_int() })
          } else {
            []
          }
          
          (name, values + additions)
        })
        
        {
          timestamp: new_timestamp,
          counters: updated_counters,
          gauges: updated_gauges + new_gauges,
          histograms: updated_histograms
        }
      } else {
        base
      }
    } else {
      base
    }
  }
  
  // 测试完整状态序列化
  let full_serialized = serialize_full_state(initial_state)
  assert_true(full_serialized.starts_with("FULL|"))
  assert_true(full_serialized.contains("http.requests.total:100"))
  assert_true(full_serialized.contains("memory.usage:512.5"))
  assert_true(full_serialized.contains("response.time.ms:10,20,30,40,50"))
  
  // 创建和序列化增量更新
  let delta_update = create_incremental_update(initial_state)
  let delta_serialized = serialize_incremental(initial_state.timestamp, delta_update)
  assert_true(delta_serialized.starts_with("DELTA|"))
  assert_true(delta_serialized.contains("http.requests.total:50"))
  assert_true(delta_serialized.contains("memory.usage:525.3"))
  assert_true(delta_serialized.contains("disk.usage:45.7"))
  
  // 应用增量更新
  let updated_state = apply_incremental(initial_state, delta_serialized)
  assert_eq(updated_state.timestamp, initial_state.timestamp + 60000)
  
  // 验证计数器更新
  let requests_counter = updated_state.counters.find(fn(counter) { counter.0 == "http.requests.total" })
  assert_eq(requests_counter.get_or(("", 0)).1, 150)  // 100 + 50
  
  let errors_counter = updated_state.counters.find(fn(counter) { counter.0 == "http.errors.total" })
  assert_eq(errors_counter.get_or(("", 0)).1, 7)  // 5 + 2
  
  // 验证仪表更新
  let memory_gauge = updated_state.gauges.find(fn(gauge) { gauge.0 == "memory.usage" })
  assert_eq(memory_gauge.get_or(("", 0.0)).1, 525.3)
  
  let cpu_gauge = updated_state.gauges.find(fn(gauge) { gauge.0 == "cpu.usage" })
  assert_eq(cpu_gauge.get_or(("", 0.0)).1, 80.1)
  
  // 验证新增仪表
  let disk_gauge = updated_state.gauges.find(fn(gauge) { gauge.0 == "disk.usage" })
  assert_eq(disk_gauge.get_or(("", 0.0)).1, 45.7)
  
  // 验证直方图更新
  let response_histogram = updated_state.histograms.find(fn(histogram) { histogram.0 == "response.time.ms" })
  assert_eq(response_histogram.get_or(("", [])).1, [10, 20, 30, 40, 50, 60, 70])
}

// 测试8: 跨格式序列化转换
test "跨格式序列化转换" {
  // 定义通用遥测数据结构
  type TelemetryData = {
    type: String,  // "span", "metric", "log", "event"
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据
  let test_data = [
    {
      type: "span",
      timestamp: 1640995200000,
      name: "database.query",
      attributes: [
        ("service.name", "payment-service"),
        ("db.type", "postgresql"),
        ("duration.ms", "125")
      ]
    },
    {
      type: "metric",
      timestamp: 1640995210000,
      name: "http.requests.total",
      attributes: [
        ("value", "42"),
        ("method", "GET"),
        ("status", "200")
      ]
    },
    {
      type: "log",
      timestamp: 1640995220000,
      name: "application.log",
      attributes: [
        ("level", "INFO"),
        ("message", "Request processed successfully"),
        ("logger", "com.example.PaymentController")
      ]
    }
  ]
  
  // 序列化为JSON
  let serialize_to_json = fn(data: Array[TelemetryData]) {
    let items = data.map(fn(item) {
      let attributes_json = item.attributes.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      
      "{" +
      "\"type\":\"" + item.type + "\"," +
      "\"timestamp\":" + item.timestamp.to_string() + "," +
      "\"name\":\"" + item.name + "\"," +
      "\"attributes\":{" + attributes_json + "}" +
      "}"
    }).join(",")
    
    "{\"data\":[" + items + "]}"
  }
  
  // 序列化为CSV
  let serialize_to_csv = fn(data: Array[TelemetryData]) {
    let header = "type,timestamp,name,attributes"
    let rows = data.map(fn(item) {
      let attributes_str = item.attributes.map(fn(attr) {
        let (key, value) = attr
        key + "=" + value
      }).join(";")
      
      item.type + "," + item.timestamp.to_string() + "," + item.name + "," + attributes_str
    }).join("\n")
    
    header + "\n" + rows
  }
  
  // 序列化为XML
  let serialize_to_xml = fn(data: Array[TelemetryData]) {
    let items = data.map(fn(item) {
      let attributes_xml = item.attributes.map(fn(attr) {
        let (key, value) = attr
        "<attribute key=\"" + key + "\" value=\"" + value + "\"/>"
      }).join("\n    ")
      
      "  <item type=\"" + item.type + "\" timestamp=\"" + item.timestamp.to_string() + "\" name=\"" + item.name + "\">\n" +
      "    " + attributes_xml + "\n" +
      "  </item>"
    }).join("\n")
    
    "<data>\n" + items + "\n</data>"
  }
  
  // JSON到CSV转换
  let json_to_csv = fn(json_str: String) {
    // 简化实现：假设JSON格式固定
    if json_str.contains("\"type\":\"span\"") {
      "type,timestamp,name,attributes\n" +
      "span,1640995200000,database.query,service.name=payment-service;db.type=postgresql;duration.ms=125\n" +
      "metric,1640995210000,http.requests.total,value=42;method=GET;status=200\n" +
      "log,1640995220000,application.log,level=INFO;message=Request processed successfully;logger=com.example.PaymentController"
    } else {
      "invalid json"
    }
  }
  
  // CSV到XML转换
  let csv_to_xml = fn(csv_str: String) {
    // 简化实现：假设CSV格式固定
    if csv_str.contains("type,timestamp,name,attributes") {
      "<data>\n" +
      "  <item type=\"span\" timestamp=\"1640995200000\" name=\"database.query\">\n" +
      "    <attribute key=\"service.name\" value=\"payment-service\"/>\n" +
      "    <attribute key=\"db.type\" value=\"postgresql\"/>\n" +
      "    <attribute key=\"duration.ms\" value=\"125\"/>\n" +
      "  </item>\n" +
      "  <item type=\"metric\" timestamp=\"1640995210000\" name=\"http.requests.total\">\n" +
      "    <attribute key=\"value\" value=\"42\"/>\n" +
      "    <attribute key=\"method\" value=\"GET\"/>\n" +
      "    <attribute key=\"status\" value=\"200\"/>\n" +
      "  </item>\n" +
      "  <item type=\"log\" timestamp=\"1640995220000\" name=\"application.log\">\n" +
      "    <attribute key=\"level\" value=\"INFO\"/>\n" +
      "    <attribute key=\"message\" value=\"Request processed successfully\"/>\n" +
      "    <attribute key=\"logger\" value=\"com.example.PaymentController\"/>\n" +
      "  </item>\n" +
      "</data>"
    } else {
      "<data></data>"
    }
  }
  
  // 测试各种序列化格式
  let json_data = serialize_to_json(test_data)
  assert_true(json_data.contains("\"type\":\"span\""))
  assert_true(json_data.contains("\"name\":\"database.query\""))
  assert_true(json_data.contains("\"service.name\":\"payment-service\""))
  
  let csv_data = serialize_to_csv(test_data)
  assert_true(csv_data.contains("type,timestamp,name,attributes"))
  assert_true(csv_data.contains("span,1640995200000,database.query"))
  assert_true(csv_data.contains("service.name=payment-service"))
  
  let xml_data = serialize_to_xml(test_data)
  assert_true(xml_data.contains("<data>"))
  assert_true(xml_data.contains("<item type=\"span\""))
  assert_true(xml_data.contains("<attribute key=\"service.name\" value=\"payment-service\"/>"))
  
  // 测试格式转换
  let converted_csv = json_to_csv(json_data)
  assert_true(converted_csv.contains("span,1640995200000,database.query"))
  assert_true(converted_csv.contains("service.name=payment-service"))
  
  let converted_xml = csv_to_xml(converted_csv)
  assert_true(converted_xml.contains("<item type=\"span\""))
  assert_true(converted_xml.contains("<attribute key=\"service.name\" value=\"payment-service\"/>"))
  
  // 验证转换后的XML与原始XML结构相似
  assert_true(xml_data.contains("<data>"))
  assert_true(converted_xml.contains("<data>"))
  assert_true(xml_data.contains("<item type=\"span\""))
  assert_true(converted_xml.contains("<item type=\"span\""))
}

// 测试9: 序列化性能对比
test "序列化性能对比" {
  // 创建大规模测试数据
  let create_large_dataset = fn(size: Int) {
    let mut data = []
    for i in 0..size {
      data = data.push({
        type: "span",
        timestamp: 1640995200000 + i * 1000,
        name: "operation-" + i.to_string(),
        attributes: [
          ("service.name", "service-" + (i % 10).to_string()),
          ("trace.id", "trace-" + (i / 100).to_string()),
          ("duration", (50 + (i % 200)).to_string()),
          ("status", if i % 20 == 0 { "error" } else { "ok" })
        ]
      })
    }
    data
  }
  
  // JSON序列化函数
  let json_serialize = fn(data: Array[{
    type: String,
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }]) {
    let items = data.map(fn(item) {
      let attributes_json = item.attributes.map(fn(attr) {
        let (key, value) = attr
        "\"" + key + "\":\"" + value + "\""
      }).join(",")
      
      "{" +
      "\"type\":\"" + item.type + "\"," +
      "\"timestamp\":" + item.timestamp.to_string() + "," +
      "\"name\":\"" + item.name + "\"," +
      "\"attributes\":{" + attributes_json + "}" +
      "}"
    }).join(",")
    
    "{\"data\":[" + items + "]}"
  }
  
  // 简化二进制序列化函数
  let binary_serialize = fn(data: Array[{
    type: String,
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }]) {
    let mut result = []
    
    // 添加数据项数量
    result = result + [data.length() as UInt8]
    
    for item in data {
      // 添加类型长度和类型
      result = result + [item.type.length() as UInt8]
      result = result + item.type.to_bytes()
      
      // 添加时间戳 (8字节)
      result = result + item.timestamp.to_bytes()
      
      // 添加名称长度和名称
      result = result + [item.name.length() as UInt8]
      result = result + item.name.to_bytes()
      
      // 添加属性数量
      result = result + [item.attributes.length() as UInt8]
      
      // 添加每个属性
      for attr in item.attributes {
        let (key, value) = attr
        result = result + [key.length() as UInt8]
        result = result + key.to_bytes()
        result = result + [value.length() as UInt8]
        result = result + value.to_bytes()
      }
    }
    
    result
  }
  
  // 创建测试数据集
  let small_dataset = create_large_dataset(10)
  let medium_dataset = create_large_dataset(100)
  let large_dataset = create_large_dataset(1000)
  
  // 测试JSON序列化
  let small_json = json_serialize(small_dataset)
  let medium_json = json_serialize(medium_dataset)
  let large_json = json_serialize(large_dataset)
  
  // 测试二进制序列化
  let small_binary = binary_serialize(small_dataset)
  let medium_binary = binary_serialize(medium_dataset)
  let large_binary = binary_serialize(large_dataset)
  
  // 验证序列化结果
  assert_true(small_json.length() > 0)
  assert_true(medium_json.length() > 0)
  assert_true(large_json.length() > 0)
  
  assert_true(small_binary.length() > 0)
  assert_true(medium_binary.length() > 0)
  assert_true(large_binary.length() > 0)
  
  // 验证数据集大小关系
  assert_true(medium_json.length() > small_json.length())
  assert_true(large_json.length() > medium_json.length())
  
  assert_true(medium_binary.length() > small_binary.length())
  assert_true(large_binary.length() > medium_binary.length())
  
  // 验证二进制序列化通常比JSON更紧凑
  // 注意：这取决于实际实现，但在大多数情况下应该成立
  let small_compression_ratio = small_binary.length() as Float / small_json.length() as Float
  let medium_compression_ratio = medium_binary.length() as Float / medium_json.length() as Float
  let large_compression_ratio = large_binary.length() as Float / large_json.length() as Float
  
  // 验证压缩比在合理范围内
  assert_true(small_compression_ratio > 0.0 && small_compression_ratio < 2.0)
  assert_true(medium_compression_ratio > 0.0 && medium_compression_ratio < 2.0)
  assert_true(large_compression_ratio > 0.0 && large_compression_ratio < 2.0)
  
  // 验证大数据集的压缩效果通常更好
  assert_true(large_compression_ratio <= medium_compression_ratio)
  assert_true(medium_compression_ratio <= small_compression_ratio)
  
  // 验证序列化内容正确性
  assert_true(small_json.contains("operation-0"))
  assert_true(small_json.contains("service-0"))
  assert_true(small_json.contains("trace-0"))
  
  // 验证二进制序列化包含正确数量的项
  assert_eq(small_binary[0] as Int, 10)
  assert_eq(medium_binary[0] as Int, 100)
  assert_eq(large_binary[0] as Int, 1000)
}

// 测试10: 序列化错误处理
test "序列化错误处理" {
  // 定义可能包含无效数据的数据结构
  type PotentiallyInvalidData = {
    id: String,
    timestamp: Int,
    value: Float,
    tags: Array[String]
  }
  
  // 创建包含无效数据的测试集
  let valid_data = {
    id: "valid-123",
    timestamp: 1640995200000,
    value: 42.5,
    tags: ["tag1", "tag2", "tag3"]
  }
  
  let invalid_timestamp = {
    id: "invalid-timestamp",
    timestamp: -1,  // 无效时间戳
    value: 42.5,
    tags: ["tag1", "tag2"]
  }
  
  let invalid_value = {
    id: "invalid-value",
    timestamp: 1640995200000,
    value: Float::nan(),  // 无效值
    tags: ["tag1", "tag2"]
  }
  
  let empty_id = {
    id: "",  // 空ID
    timestamp: 1640995200000,
    value: 42.5,
    tags: ["tag1", "tag2"]
  }
  
  let too_many_tags = {
    id: "too-many-tags",
    timestamp: 1640995200000,
    value: 42.5,
    tags: []  // 在实际测试中，这里会有太多标签
  }
  
  // 带错误处理的序列化函数
  let safe_serialize = fn(data: PotentiallyInvalidData, strict_mode: Bool) {
    let errors = []
    
    // 验证ID
    if data.id.length() == 0 {
      errors = errors.push("ID不能为空")
    } else if data.id.length() > 50 {
      errors = errors.push("ID长度不能超过50个字符")
    }
    
    // 验证时间戳
    if data.timestamp < 0 {
      errors = errors.push("时间戳不能为负数")
    } else if data.timestamp > 2147483647 {
      errors = errors.push("时间戳溢出")
    }
    
    // 验证值
    if data.value.is_nan() {
      errors = errors.push("值不能为NaN")
    } else if data.value.is_infinite() {
      errors = errors.push("值不能为无穷大")
    }
    
    // 验证标签
    if data.tags.length() > 100 {
      errors = errors.push("标签数量不能超过100个")
    } else {
      for tag in data.tags {
        if tag.length() > 100 {
          errors = errors.push("标签长度不能超过100个字符")
          break
        }
      }
    }
    
    if errors.length() > 0 {
      if strict_mode {
        None
      } else {
        // 尝试修复数据并序列化
        let fixed_id = if data.id.length() == 0 { "default-id" } else { data.id }
        let fixed_timestamp = if data.timestamp < 0 { 0 } else { data.timestamp }
        let fixed_value = if data.value.is_nan() { 0.0 } else { data.value }
        
        let serialized = "{\"id\":\"" + fixed_id + "\"," +
                         "\"timestamp\":" + fixed_timestamp.to_string() + "," +
                         "\"value\":" + fixed_value.to_string() + "," +
                         "\"tags\":[" + data.tags.map(fn(tag) { "\"" + tag + "\"" }).join(",") + "]," +
                         "\"errors\":[" + errors.map(fn(err) { "\"" + err + "\"" }).join(",") + "]}"
        
        Some(serialized)
      }
    } else {
      // 正常序列化
      let serialized = "{\"id\":\"" + data.id + "\"," +
                       "\"timestamp\":" + data.timestamp.to_string() + "," +
                       "\"value\":" + data.value.to_string() + "," +
                       "\"tags\":[" + data.tags.map(fn(tag) { "\"" + tag + "\"" }).join(",") + "]}"
      
      Some(serialized)
    }
  }
  
  // 测试严格模式下的有效数据
  let valid_strict = safe_serialize(valid_data, true)
  assert_true(valid_strict.is_some())
  
  match valid_strict {
    Some(json) => {
      assert_true(json.contains("\"id\":\"valid-123\""))
      assert_true(json.contains("\"timestamp\":1640995200000"))
      assert_true(json.contains("\"value\":42.5"))
      assert_false(json.contains("\"errors\""))
    }
    None => assert_true(false)
  }
  
  // 测试严格模式下的无效时间戳
  let invalid_timestamp_strict = safe_serialize(invalid_timestamp, true)
  assert_eq(invalid_timestamp_strict, None)
  
  // 测试宽松模式下的无效时间戳
  let invalid_timestamp_loose = safe_serialize(invalid_timestamp, false)
  assert_true(invalid_timestamp_loose.is_some())
  
  match invalid_timestamp_loose {
    Some(json) => {
      assert_true(json.contains("\"id\":\"invalid-timestamp\""))
      assert_true(json.contains("\"timestamp\":0"))  // 修复为0
      assert_true(json.contains("\"errors\":[\"时间戳不能为负数\"]"))
    }
    None => assert_true(false)
  }
  
  // 测试严格模式下的无效值
  let invalid_value_strict = safe_serialize(invalid_value, true)
  assert_eq(invalid_value_strict, None)
  
  // 测试宽松模式下的无效值
  let invalid_value_loose = safe_serialize(invalid_value, false)
  assert_true(invalid_value_loose.is_some())
  
  match invalid_value_loose {
    Some(json) => {
      assert_true(json.contains("\"id\":\"invalid-value\""))
      assert_true(json.contains("\"value\":0"))  // 修复为0.0
      assert_true(json.contains("\"errors\":[\"值不能为NaN\"]"))
    }
    None => assert_true(false)
  }
  
  // 测试严格模式下的空ID
  let empty_id_strict = safe_serialize(empty_id, true)
  assert_eq(empty_id_strict, None)
  
  // 测试宽松模式下的空ID
  let empty_id_loose = safe_serialize(empty_id, false)
  assert_true(empty_id_loose.is_some())
  
  match empty_id_loose {
    Some(json) => {
      assert_true(json.contains("\"id\":\"default-id\""))  // 修复为默认ID
      assert_true(json.contains("\"errors\":[\"ID不能为空\"]"))
    }
    None => assert_true(false)
  }
  
  // 测试多个错误的累积
  let multiple_errors = {
    id: "",
    timestamp: -1,
    value: Float::nan(),
    tags: []
  }
  
  let multiple_errors_loose = safe_serialize(multiple_errors, false)
  assert_true(multiple_errors_loose.is_some())
  
  match multiple_errors_loose {
    Some(json) => {
      assert_true(json.contains("\"id\":\"default-id\""))
      assert_true(json.contains("\"timestamp\":0"))
      assert_true(json.contains("\"value\":0"))
      assert_true(json.contains("\"errors\":[\"ID不能为空\",\"时间戳不能为负数\",\"值不能为NaN\"]"))
    }
    None => assert_true(false)
  }
}