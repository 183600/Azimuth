// Azimuth 高质量测试套件
// 专注于遥测系统的核心功能和高级特性的深度测试

// 测试1: 遥测数据生成和验证
test "遥测数据生成和验证" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 生成唯一ID的函数
  let generate_id = fn() {
    let timestamp = Time::now().to_string()
    let random = Random::int(1000, 9999).to_string()
    timestamp + "-" + random
  }
  
  // 创建遥测数据的函数
  let create_telemetry = fn(operation: String, parent_id: Option[String]) {
    let trace_id = generate_id()
    let span_id = generate_id()
    let now = Time::now()
    
    {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: parent_id,
      operation_name: operation,
      start_time: now,
      end_time: now + Random::int(100, 5000),
      status: "ok",
      attributes: [
        ("service.name", "azimuth-service"),
        ("service.version", "1.0.0"),
        ("environment", "production")
      ]
    }
  }
  
  // 验证遥测数据的函数
  let validate_telemetry = fn(data: TelemetryData) {
    let errors = []
    
    if data.trace_id.length() == 0 {
      errors = errors.push("trace_id不能为空")
    }
    
    if data.span_id.length() == 0 {
      errors = errors.push("span_id不能为空")
    }
    
    if data.operation_name.length() == 0 {
      errors = errors.push("operation_name不能为空")
    }
    
    if data.end_time < data.start_time {
      errors = errors.push("end_time不能早于start_time")
    }
    
    if data.status != "ok" && data.status != "error" {
      errors = errors.push("status必须是ok或error")
    }
    
    errors
  }
  
  // 测试遥测数据创建
  let root_telemetry = create_telemetry("root-operation", None)
  let child_telemetry = create_telemetry("child-operation", Some(root_telemetry.span_id))
  
  // 验证根遥测数据
  let root_validation = validate_telemetry(root_telemetry)
  assert_eq(root_validation.length(), 0)
  
  // 验证子遥测数据
  let child_validation = validate_telemetry(child_telemetry)
  assert_eq(child_validation.length(), 0)
  
  // 验证父子关系
  match child_telemetry.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, root_telemetry.span_id)
    None => assert_true(false)
  }
  
  // 测试无效数据
  let invalid_telemetry = {
    trace_id: "",
    span_id: "",
    parent_span_id: None,
    operation_name: "",
    start_time: 1000,
    end_time: 500,
    status: "invalid",
    attributes: []
  }
  
  let invalid_validation = validate_telemetry(invalid_telemetry)
  assert_eq(invalid_validation.length(), 5)
  assert_true(invalid_validation.contains("trace_id不能为空"))
  assert_true(invalid_validation.contains("span_id不能为空"))
  assert_true(invalid_validation.contains("operation_name不能为空"))
  assert_true(invalid_validation.contains("end_time不能早于start_time"))
  assert_true(invalid_validation.contains("status必须是ok或error"))
}

// 测试2: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  // 定义遥测事件类型
  enum TelemetryEvent {
    SpanStart(String, Int, Array[(String, String)])  // span_id, timestamp, attributes
    SpanEnd(String, Int, String)  // span_id, timestamp, status
    SpanAttribute(String, String, String)  // span_id, key, value
    SpanEvent(String, String, Int, Array[(String, String)])  // span_id, event_name, timestamp, attributes
  }
  
  // 序列化函数
  let serialize_event = fn(event: TelemetryEvent) {
    match event {
      TelemetryEvent::SpanStart(span_id, timestamp, attributes) => {
        let attr_str = attributes.map(fn(attr) {
          let (key, value) = attr
          key + ":" + value
        }).join(",")
        "span_start:" + span_id + ":" + timestamp.to_string() + ":" + attr_str
      }
      TelemetryEvent::SpanEnd(span_id, timestamp, status) => {
        "span_end:" + span_id + ":" + timestamp.to_string() + ":" + status
      }
      TelemetryEvent::SpanAttribute(span_id, key, value) => {
        "span_attr:" + span_id + ":" + key + ":" + value
      }
      TelemetryEvent::SpanEvent(span_id, event_name, timestamp, attributes) => {
        let attr_str = attributes.map(fn(attr) {
          let (key, value) = attr
          key + ":" + value
        }).join(",")
        "span_event:" + span_id + ":" + event_name + ":" + timestamp.to_string() + ":" + attr_str
      }
    }
  }
  
  // 反序列化函数
  let deserialize_event = fn(serialized: String) {
    let parts = serialized.split(":")
    if parts.length() == 0 {
      return None
    }
    
    match parts[0] {
      "span_start" => {
        if parts.length() >= 3 {
          let span_id = parts[1]
          let timestamp = parts[2].to_int()
          let attributes = if parts.length() > 3 {
            parts[3].split(",").map(fn(attr) {
              let kv = attr.split(":")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            })
          } else {
            []
          }
          Some(TelemetryEvent::SpanStart(span_id, timestamp, attributes))
        } else {
          None
        }
      }
      "span_end" => {
        if parts.length() >= 4 {
          let span_id = parts[1]
          let timestamp = parts[2].to_int()
          let status = parts[3]
          Some(TelemetryEvent::SpanEnd(span_id, timestamp, status))
        } else {
          None
        }
      }
      "span_attr" => {
        if parts.length() >= 4 {
          let span_id = parts[1]
          let key = parts[2]
          let value = parts[3]
          Some(TelemetryEvent::SpanAttribute(span_id, key, value))
        } else {
          None
        }
      }
      "span_event" => {
        if parts.length() >= 4 {
          let span_id = parts[1]
          let event_name = parts[2]
          let timestamp = parts[3].to_int()
          let attributes = if parts.length() > 4 {
            parts[4].split(",").map(fn(attr) {
              let kv = attr.split(":")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            })
          } else {
            []
          }
          Some(TelemetryEvent::SpanEvent(span_id, event_name, timestamp, attributes))
        } else {
          None
        }
      }
      _ => None
    }
  }
  
  // 测试序列化和反序列化
  let start_event = TelemetryEvent::SpanStart("span-123", 1000, [
    ("service.name", "api-service"),
    ("operation.type", "http")
  ])
  
  let serialized_start = serialize_event(start_event)
  assert_true(serialized_start.starts_with("span_start:span-123:1000:"))
  
  let deserialized_start = deserialize_event(serialized_start)
  match deserialized_start {
    Some(TelemetryEvent::SpanStart(span_id, timestamp, attributes)) => {
      assert_eq(span_id, "span-123")
      assert_eq(timestamp, 1000)
      assert_eq(attributes.length(), 2)
      assert_true(attributes.contains(("service.name", "api-service")))
      assert_true(attributes.contains(("operation.type", "http")))
    }
    _ => assert_true(false)
  }
  
  // 测试span事件
  let event = TelemetryEvent::SpanEvent("span-456", "error_occurred", 1500, [
    ("error.type", "timeout"),
    ("error.message", "operation timed out")
  ])
  
  let serialized_event = serialize_event(event)
  let deserialized_event = deserialize_event(serialized_event)
  
  match deserialized_event {
    Some(TelemetryEvent::SpanEvent(span_id, event_name, timestamp, attributes)) => {
      assert_eq(span_id, "span-456")
      assert_eq(event_name, "error_occurred")
      assert_eq(timestamp, 1500)
      assert_eq(attributes.length(), 2)
    }
    _ => assert_true(false)
  }
  
  // 测试无效数据
  let invalid_serialized = "invalid_event:data"
  let invalid_deserialized = deserialize_event(invalid_serialized)
  assert_eq(invalid_deserialized, None)
  
  let incomplete_serialized = "span_start:span-789"
  let incomplete_deserialized = deserialize_event(incomplete_serialized)
  assert_eq(incomplete_deserialized, None)
}

// 测试3: 遥测数据采样策略
test "遥测数据采样策略" {
  // 定义采样决策
  enum SamplingDecision {
    RecordAndSample
    RecordOnly
    Drop
  }
  
  // 定义采样器类型
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)  // 采样率，0.0到1.0
    ParentBased(Box[SamplerType])  // 基于父span的采样决策
    AttributeBased(Array[(String, String)])  // 基于属性的采样
  }
  
  // 计算trace_id的哈希值
  let trace_id_hash = fn(trace_id: String) {
    let chars = trace_id.to_char_array()
    chars.reduce(0, fn(acc, char) {
      (acc * 31 + char.to_int()) % 1000000
    })
  }
  
  // 采样决策函数
  let should_sample = fn(sampler: SamplerType, trace_id: String, parent_decision: Option[SamplingDecision], attributes: Array[(String, String)]) {
    match sampler {
      SamplerType::AlwaysOn => SamplingDecision::RecordAndSample
      SamplerType::AlwaysOff => SamplingDecision::Drop
      SamplerType::TraceIdRatio(ratio) => {
        let hash = trace_id_hash(trace_id)
        let normalized = (hash as Float) / 1000000.0
        if normalized <= ratio {
          SamplingDecision::RecordAndSample
        } else {
          SamplingDecision::Drop
        }
      }
      SamplerType::ParentBased(inner_sampler) => {
        match parent_decision {
          Some(SamplingDecision::RecordAndSample) => SamplingDecision::RecordAndSample
          Some(SamplingDecision::RecordOnly) => SamplingDecision::RecordOnly
          Some(SamplingDecision::Drop) => SamplingDecision::Drop
          None => should_sample(inner_sampler, trace_id, None, attributes)
        }
      }
      SamplerType::AttributeBased(required_attributes) => {
        let all_match = required_attributes.all(fn(required_attr) {
          let (req_key, req_value) = required_attr
          attributes.any(fn(attr) {
            let (key, value) = attr
            key == req_key && value == req_value
          })
        })
        
        if all_match {
          SamplingDecision::RecordAndSample
        } else {
          SamplingDecision::Drop
        }
      }
    }
  }
  
  // 测试AlwaysOn采样器
  let always_on_sampler = SamplerType::AlwaysOn
  let decision1 = should_sample(always_on_sampler, "trace-123", None, [])
  assert_eq(decision1, SamplingDecision::RecordAndSample)
  
  // 测试AlwaysOff采样器
  let always_off_sampler = SamplerType::AlwaysOff
  let decision2 = should_sample(always_off_sampler, "trace-456", None, [])
  assert_eq(decision2, SamplingDecision::Drop)
  
  // 测试TraceIdRatio采样器
  let ratio_sampler = SamplerType::TraceIdRatio(0.5)  // 50%采样率
  let decision3 = should_sample(ratio_sampler, "trace-789", None, [])
  // 结果取决于哈希值，但我们测试多次应该有采样和不采样的情况
  let mut sampled_count = 0
  for i in 0..100 {
    let trace_id = "trace-" + i.to_string()
    let decision = should_sample(ratio_sampler, trace_id, None, [])
    if decision == SamplingDecision::RecordAndSample {
      sampled_count = sampled_count + 1
    }
  }
  // 采样次数应该在合理范围内（30-70次）
  assert_true(sampled_count >= 30 && sampled_count <= 70)
  
  // 测试ParentBased采样器
  let parent_based_sampler = SamplerType::ParentBased(Box[SamplerType::AlwaysOff])
  let decision4 = should_sample(parent_based_sampler, "trace-abc", Some(SamplingDecision::RecordAndSample), [])
  assert_eq(decision4, SamplingDecision::RecordAndSample)
  
  let decision5 = should_sample(parent_based_sampler, "trace-def", Some(SamplingDecision::Drop), [])
  assert_eq(decision5, SamplingDecision::Drop)
  
  let decision6 = should_sample(parent_based_sampler, "trace-ghi", None, [])
  assert_eq(decision6, SamplingDecision::Drop)  // 使用内部采样器（AlwaysOff）
  
  // 测试AttributeBased采样器
  let attr_sampler = SamplerType::AttributeBased([
    ("service.name", "critical-service"),
    ("environment", "production")
  ])
  
  let matching_attrs = [
    ("service.name", "critical-service"),
    ("environment", "production"),
    ("version", "1.0.0")
  ]
  
  let decision7 = should_sample(attr_sampler, "trace-jkl", None, matching_attrs)
  assert_eq(decision7, SamplingDecision::RecordAndSample)
  
  let non_matching_attrs = [
    ("service.name", "regular-service"),
    ("environment", "production")
  ]
  
  let decision8 = should_sample(attr_sampler, "trace-mno", None, non_matching_attrs)
  assert_eq(decision8, SamplingDecision::Drop)
}

// 测试4: 遥测数据批处理和缓冲
test "遥测数据批处理和缓冲" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    max_buffer_size: Int
  }
  
  // 定义缓冲区状态
  type BufferState = {
    items: Array[String],
    count: Int,
    last_flush_time: Int,
    overflow_count: Int
  }
  
  // 默认配置
  let default_config = {
    max_batch_size: 100,
    max_wait_time_ms: 5000,
    max_buffer_size: 1000
  }
  
  // 创建缓冲区
  let create_buffer = fn() {
    {
      items: [],
      count: 0,
      last_flush_time: Time::now(),
      overflow_count: 0
    }
  }
  
  // 添加项目到缓冲区
  let add_to_buffer = fn(buffer: BufferState, item: String, config: BatchConfig) {
    if buffer.count >= config.max_buffer_size {
      // 缓冲区溢出，丢弃最旧的项目
      let new_items = buffer.items.slice(1, buffer.count)
      let new_buffer = {
        items: new_items.push(item),
        count: new_items.length(),
        last_flush_time: buffer.last_flush_time,
        overflow_count: buffer.overflow_count + 1
      }
      (new_buffer, true)  // 返回缓冲区和溢出标志
    } else {
      let new_buffer = {
        items: buffer.items.push(item),
        count: buffer.count + 1,
        last_flush_time: buffer.last_flush_time,
        overflow_count: buffer.overflow_count
      }
      (new_buffer, false)  // 返回缓冲区和无溢出标志
    }
  }
  
  // 检查是否应该刷新
  let should_flush = fn(buffer: BufferState, config: BatchConfig) {
    let now = Time::now()
    let time_elapsed = now - buffer.last_flush_time
    
    buffer.count >= config.max_batch_size || time_elapsed >= config.max_wait_time_ms
  }
  
  // 刷新缓冲区
  let flush_buffer = fn(buffer: BufferState) {
    let flushed_items = buffer.items.copy()
    let new_buffer = {
      items: [],
      count: 0,
      last_flush_time: Time::now(),
      overflow_count: buffer.overflow_count
    }
    (new_buffer, flushed_items)
  }
  
  // 测试缓冲区操作
  let buffer = create_buffer()
  
  // 添加项目
  let (buffer1, overflow1) = add_to_buffer(buffer, "item1", default_config)
  assert_eq(buffer1.count, 1)
  assert_false(overflow1)
  
  let (buffer2, overflow2) = add_to_buffer(buffer1, "item2", default_config)
  assert_eq(buffer2.count, 2)
  assert_false(overflow2)
  
  // 检查是否应该刷新（不应该，因为项目数量和时间都不足）
  assert_false(should_flush(buffer2, default_config))
  
  // 添加足够多的项目触发刷新
  let mut buffer3 = buffer2
  for i in 3..=default_config.max_batch_size {
    let (new_buffer, overflow) = add_to_buffer(buffer3, "item" + i.to_string(), default_config)
    buffer3 = new_buffer
    assert_false(overflow)
  }
  
  // 现在应该刷新
  assert_true(should_flush(buffer3, default_config))
  
  // 刷新缓冲区
  let (buffer4, flushed_items) = flush_buffer(buffer3)
  assert_eq(buffer4.count, 0)
  assert_eq(flushed_items.length(), default_config.max_batch_size)
  assert_eq(flushed_items[0], "item1")
  assert_eq(flushed_items[default_config.max_batch_size - 1], "item" + default_config.max_batch_size.to_string())
  
  // 测试缓冲区溢出
  let small_config = {
    max_batch_size: 5,
    max_wait_time_ms: 1000,
    max_buffer_size: 3
  }
  
  let small_buffer = create_buffer()
  let (small_buffer1, _) = add_to_buffer(small_buffer, "item1", small_config)
  let (small_buffer2, _) = add_to_buffer(small_buffer1, "item2", small_config)
  let (small_buffer3, _) = add_to_buffer(small_buffer2, "item3", small_config)
  
  // 缓冲区已满，再添加应该溢出
  let (small_buffer4, overflow4) = add_to_buffer(small_buffer3, "item4", small_config)
  assert_eq(small_buffer4.count, 3)  // 大小不变
  assert_true(overflow4)  // 溢出标志为true
  assert_eq(small_buffer4.overflow_count, 1)
  assert_eq(small_buffer4.items[0], "item2")  // item1被丢弃
  assert_eq(small_buffer4.items[2], "item4")
}

// 测试5: 遥测数据压缩和传输优化
test "遥测数据压缩和传输优化" {
  // 定义压缩策略
  enum CompressionStrategy {
    None
    Gzip
    Lz4
    Custom((String) -> String)  // 自定义压缩函数
  }
  
  // 简单的字符串压缩算法（模拟）
  let simple_compress = fn(input: String) {
    // 简单的行程编码压缩
    if input.length() == 0 {
      return ""
    }
    
    let chars = input.to_char_array()
    let mut result = ""
    let mut count = 1
    let mut current = chars[0]
    
    for i in 1..chars.length() {
      if chars[i] == current && count < 9 {
        count = count + 1
      } else {
        result = result + count.to_string() + current.to_string()
        current = chars[i]
        count = 1
      }
    }
    
    result = result + count.to_string() + current.to_string()
    result
  }
  
  // 简单的字符串解压缩算法
  let simple_decompress = fn(compressed: String) {
    if compressed.length() == 0 {
      return ""
    }
    
    let mut result = ""
    let mut i = 0
    
    while i < compressed.length() {
      let count_char = compressed[i]
      let count = count_char.to_int() - 48  // 字符转数字
      if i + 1 < compressed.length() {
        let char_to_repeat = compressed[i + 1]
        for j in 0..count {
          result = result + char_to_repeat.to_string()
        }
      }
      i = i + 2
    }
    
    result
  }
  
  // 压缩函数
  let compress_data = fn(data: String, strategy: CompressionStrategy) {
    match strategy {
      CompressionStrategy::None => data
      CompressionStrategy::Gzip => "gzip:" + data  // 模拟gzip压缩
      CompressionStrategy::Lz4 => "lz4:" + data    // 模拟lz4压缩
      CompressionStrategy::Custom(compress_fn) => compress_fn(data)
    }
  }
  
  // 解压缩函数
  let decompress_data = fn(compressed_data: String, strategy: CompressionStrategy) {
    match strategy {
      CompressionStrategy::None => compressed_data
      CompressionStrategy::Gzip => {
        if compressed_data.starts_with("gzip:") {
          compressed_data.substring(5, compressed_data.length() - 5)
        } else {
          compressed_data
        }
      }
      CompressionStrategy::Lz4 => {
        if compressed_data.starts_with("lz4:") {
          compressed_data.substring(4, compressed_data.length() - 4)
        } else {
          compressed_data
        }
      }
      CompressionStrategy::Custom(_) => {
        // 对于自定义压缩，我们使用相应的解压缩函数
        simple_decompress(compressed_data)
      }
    }
  }
  
  // 计算压缩率
  let compression_ratio = fn(original: String, compressed: String) {
    if original.length() == 0 {
      return 1.0
    }
    (compressed.length() as Float) / (original.length() as Float)
  }
  
  // 测试无压缩
  let original_data = "azimuth telemetry data"
  let uncompressed = compress_data(original_data, CompressionStrategy::None)
  assert_eq(uncompressed, original_data)
  assert_eq(compression_ratio(original_data, uncompressed), 1.0)
  
  // 测试模拟压缩
  let gzipped = compress_data(original_data, CompressionStrategy::Gzip)
  assert_true(gzipped.starts_with("gzip:"))
  
  let gzip_decompressed = decompress_data(gzipped, CompressionStrategy::Gzip)
  assert_eq(gzip_decompressed, original_data)
  
  // 测试自定义压缩
  let repetitive_data = "aaaaabbbcccaaaabbbcccaaaabbbccc"
  let custom_compressed = compress_data(repetitive_data, CompressionStrategy::Custom(simple_compress))
  assert_eq(custom_compressed, "5a3b2c3a3b2c3a3b2c")
  
  let custom_decompressed = decompress_data(custom_compressed, CompressionStrategy::Custom(simple_compress))
  assert_eq(custom_decompressed, repetitive_data)
  
  // 验证压缩率
  let custom_ratio = compression_ratio(repetitive_data, custom_compressed)
  assert_true(custom_ratio < 1.0)  // 压缩后应该更小
  
  // 测试批量压缩
  let data_items = [
    "trace:123:start:1000",
    "trace:123:event:1500:error",
    "trace:123:end:2000:success",
    "trace:456:start:3000",
    "trace:456:end:3500:success"
  ]
  
  let batch_data = data_items.join("\n")
  let batch_compressed = compress_data(batch_data, CompressionStrategy::Custom(simple_compress))
  let batch_decompressed = decompress_data(batch_compressed, CompressionStrategy::Custom(simple_compress))
  
  assert_eq(batch_data, batch_decompressed)
  
  // 验证批量压缩率更好
  let single_ratios = data_items.map(fn(item) {
    let compressed = compress_data(item, CompressionStrategy::Custom(simple_compress))
    compression_ratio(item, compressed)
  })
  
  let avg_single_ratio = single_ratios.reduce(fn(acc, ratio) { acc + ratio }, 0.0) / (single_ratios.length() as Float)
  let batch_ratio = compression_ratio(batch_data, batch_compressed)
  
  // 批量压缩应该比单独压缩每个项目更有效
  assert_true(batch_ratio <= avg_single_ratio)
}

// 测试6: 遥测数据上下文传播
test "遥测数据上下文传播" {
  // 定义上下文键类型
  type ContextKey = String
  
  // 定义上下文值类型
  enum ContextValue {
    StringValue(String)
    IntValue(Int)
    BoolValue(Bool)
    FloatValue(Float)
    ArrayValue(Array[String])
  }
  
  // 定义传播上下文
  type PropagationContext = {
    trace_id: String,
    span_id: String,
    trace_flags: Int,
    trace_state: Array[(String, String)],
    baggage: Array[(String, ContextValue)]
  }
  
  // 创建传播上下文
  let create_context = fn(trace_id: String, span_id: String) {
    {
      trace_id: trace_id,
      span_id: span_id,
      trace_flags: 1,  // 采样标志
      trace_state: [],
      baggage: []
    }
  }
  
  // 添加baggage项
  let add_baggage = fn(context: PropagationContext, key: String, value: ContextValue) {
    let new_baggage = context.baggage.push((key, value))
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      trace_flags: context.trace_flags,
      trace_state: context.trace_state,
      baggage: new_baggage
    }
  }
  
  // 添加trace state
  let add_trace_state = fn(context: PropagationContext, key: String, value: String) {
    let new_trace_state = context.trace_state.push((key, value))
    {
      trace_id: context.trace_id,
      span_id: context.span_id,
      trace_flags: context.trace_flags,
      trace_state: new_trace_state,
      baggage: context.baggage
    }
  }
  
  // 序列化上下文为HTTP头
  let serialize_context_to_headers = fn(context: PropagationContext) {
    let traceparent = "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string()
    
    let tracestate = context.trace_state.map(fn(pair) {
      let (key, value) = pair
      key + "=" + value
    }).join(",")
    
    let baggage = context.baggage.map(fn(pair) {
      let (key, value) = pair
      let value_str = match value {
        ContextValue::StringValue(s) => s
        ContextValue::IntValue(i) => i.to_string()
        ContextValue::BoolValue(b) => b.to_string()
        ContextValue::FloatValue(f) => f.to_string()
        ContextValue::ArrayValue(arr) => arr.join(",")
      }
      key + "=" + value_str
    }).join(",")
    
    [
      ("traceparent", traceparent),
      ("tracestate", tracestate),
      ("baggage", baggage)
    ]
  }
  
  // 从HTTP头反序列化上下文
  let deserialize_context_from_headers = fn(headers: Array[(String, String)]) {
    let traceparent = match headers.find(fn(header) { header.0 == "traceparent" }) {
      Some((_, value)) => value
      None => ""
    }
    
    let tracestate = match headers.find(fn(header) { header.0 == "tracestate" }) {
      Some((_, value)) => value
      None => ""
    }
    
    let baggage = match headers.find(fn(header) { header.0 == "baggage" }) {
      Some((_, value)) => value
      None => ""
    }
    
    // 解析traceparent: 00-trace_id-span_id-trace_flags
    let traceparent_parts = traceparent.split("-")
    let trace_id = if traceparent_parts.length() >= 2 { traceparent_parts[1] } else { "" }
    let span_id = if traceparent_parts.length() >= 3 { traceparent_parts[2] } else { "" }
    let trace_flags = if traceparent_parts.length() >= 4 { 
      traceparent_parts[3].to_int() 
    } else { 
      0 
    }
    
    // 解析tracestate
    let trace_state_pairs = if tracestate.length() > 0 {
      tracestate.split(",").map(fn(pair) {
        let kv = pair.split("=")
        if kv.length() == 2 {
          (kv[0], kv[1])
        } else {
          ("", "")
        }
      })
    } else {
      []
    }
    
    // 解析baggage
    let baggage_pairs = if baggage.length() > 0 {
      baggage.split(",").map(fn(pair) {
        let kv = pair.split("=")
        if kv.length() == 2 {
          (kv[0], ContextValue::StringValue(kv[1]))
        } else {
          ("", ContextValue::StringValue(""))
        }
      })
    } else {
      []
    }
    
    {
      trace_id: trace_id,
      span_id: span_id,
      trace_flags: trace_flags,
      trace_state: trace_state_pairs,
      baggage: baggage_pairs
    }
  }
  
  // 测试上下文创建和操作
  let context = create_context("trace-12345", "span-67890")
  assert_eq(context.trace_id, "trace-12345")
  assert_eq(context.span_id, "span-67890")
  assert_eq(context.trace_flags, 1)
  assert_eq(context.trace_state.length(), 0)
  assert_eq(context.baggage.length(), 0)
  
  // 添加baggage
  let context_with_baggage = add_baggage(context, "user.id", ContextValue::StringValue("user-123"))
  let context_with_more_baggage = add_baggage(context_with_baggage, "request.id", ContextValue::StringValue("req-456"))
  
  assert_eq(context_with_more_baggage.baggage.length(), 2)
  
  // 添加trace state
  let context_with_state = add_trace_state(context_with_more_baggage, "vendor.name", "acme")
  let context_with_more_state = add_trace_state(context_with_state, "service.version", "1.0.0")
  
  assert_eq(context_with_more_state.trace_state.length(), 2)
  
  // 测试序列化
  let headers = serialize_context_to_headers(context_with_more_state)
  
  let traceparent_header = match headers.find(fn(h) { h.0 == "traceparent" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_eq(traceparent_header, "00-trace-12345-span-67890-1")
  
  let tracestate_header = match headers.find(fn(h) { h.0 == "tracestate" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(tracestate_header.contains("vendor.name=acme"))
  assert_true(tracestate_header.contains("service.version=1.0.0"))
  
  let baggage_header = match headers.find(fn(h) { h.0 == "baggage" }) {
    Some((_, value)) => value
    None => ""
  }
  assert_true(baggage_header.contains("user.id=user-123"))
  assert_true(baggage_header.contains("request.id=req-456"))
  
  // 测试反序列化
  let deserialized_context = deserialize_context_from_headers(headers)
  assert_eq(deserialized_context.trace_id, "trace-12345")
  assert_eq(deserialized_context.span_id, "span-67890")
  assert_eq(deserialized_context.trace_flags, 1)
  assert_eq(deserialized_context.trace_state.length(), 2)
  assert_eq(deserialized_context.baggage.length(), 2)
  
  // 验证trace state
  assert_true(deserialized_context.trace_state.contains(("vendor.name", "acme")))
  assert_true(deserialized_context.trace_state.contains(("service.version", "1.0.0")))
  
  // 验证baggage
  assert_true(deserialized_context.baggage.contains(("user.id", ContextValue::StringValue("user-123"))))
  assert_true(deserialized_context.baggage.contains(("request.id", ContextValue::StringValue("req-456"))))
}

// 测试7: 遥测数据质量监控
test "遥测数据质量监控" {
  // 定义质量问题类型
  enum QualityIssue {
    MissingTraceId
    MissingSpanId
    InvalidTimestamp
    IncompleteSpan
    MalformedAttributes
    DuplicateSpanId
    OrphanedSpan  // 没有父span的子span
  }
  
  // 定义质量检查结果
  type QualityCheckResult = {
    total_items: Int,
    issue_count: Int,
    issues: Array[(String, QualityIssue)]  // (item_id, issue_type)
    quality_score: Float  // 0.0到1.0
  }
  
  // 定义遥测项
  type TelemetryItem = {
    id: String,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    timestamp: Int,
    operation_name: String,
    attributes: Array[(String, String)]
  }
  
  // 质量检查函数
  let check_quality = fn(items: Array[TelemetryItem]) {
    let mut issues = []
    let trace_spans = Map::empty()  // trace_id -> span_ids
    let span_parents = Map::empty()  // span_id -> parent_span_id
    
    // 收集span信息
    for item in items {
      let span_ids = match Map::get(trace_spans, item.trace_id) {
        Some(ids) => ids
        None => []
      }
      let _ = Map::insert(trace_spans, item.trace_id, span_ids.push(item.span_id))
      let _ = Map::insert(span_parents, item.span_id, item.parent_span_id)
    }
    
    // 检查每个项目
    for item in items {
      // 检查缺失的trace_id
      if item.trace_id.length() == 0 {
        issues = issues.push((item.id, QualityIssue::MissingTraceId))
      }
      
      // 检查缺失的span_id
      if item.span_id.length() == 0 {
        issues = issues.push((item.id, QualityIssue::MissingSpanId))
      }
      
      // 检查无效的时间戳
      if item.timestamp <= 0 {
        issues = issues.push((item.id, QualityIssue::InvalidTimestamp))
      }
      
      // 检查重复的span_id
      let duplicate_spans = match Map::get(trace_spans, item.trace_id) {
        Some(span_ids) => span_ids.count(item.span_id) > 1
        None => false
      }
      if duplicate_spans {
        issues = issues.push((item.id, QualityIssue::DuplicateSpanId))
      }
      
      // 检查孤立的span
      match item.parent_span_id {
        Some(parent_id) => {
          let parent_exists = match Map::get(span_parents, parent_id) {
            Some(_) => true
            None => false
          }
          if not parent_exists {
            issues = issues.push((item.id, QualityIssue::OrphanedSpan))
          }
        }
        None => {}  // 根span，不需要检查
      }
      
      // 检查格式错误的属性
      for attr in item.attributes {
        let (key, value) = attr
        if key.length() == 0 || value.contains("\n") || value.contains("\r") {
          issues = issues.push((item.id, QualityIssue::MalformedAttributes))
          break  // 每个项目只报告一次属性问题
        }
      }
    }
    
    // 计算质量分数
    let total_issues = issues.length()
    let total_possible_issues = items.length() * 6  // 每个项目最多6种问题
    let quality_score = if total_possible_issues > 0 {
      1.0 - ((total_issues as Float) / (total_possible_issues as Float))
    } else {
      1.0
    }
    
    {
      total_items: items.length(),
      issue_count: total_issues,
      issues: issues,
      quality_score: quality_score
    }
  }
  
  // 创建测试数据
  let valid_item = {
    id: "item-1",
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: None,
    timestamp: 1000,
    operation_name: "root-operation",
    attributes: [("service.name", "api-service")]
  }
  
  let child_item = {
    id: "item-2",
    trace_id: "trace-123",
    span_id: "span-789",
    parent_span_id: Some("span-456"),
    timestamp: 1500,
    operation_name: "child-operation",
    attributes: [("operation.type", "http")]
  }
  
  let invalid_item1 = {
    id: "item-3",
    trace_id: "",  // 缺失trace_id
    span_id: "span-abc",
    parent_span_id: None,
    timestamp: 2000,
    operation_name: "invalid-operation",
    attributes: []
  }
  
  let invalid_item2 = {
    id: "item-4",
    trace_id: "trace-456",
    span_id: "",  // 缺失span_id
    parent_span_id: None,
    timestamp: -100,  // 无效时间戳
    operation_name: "another-invalid-operation",
    attributes: [("", "value")]  // 格式错误的属性
  }
  
  let orphaned_item = {
    id: "item-5",
    trace_id: "trace-789",
    span_id: "span-def",
    parent_span_id: Some("nonexistent-parent"),  // 孤立的span
    timestamp: 3000,
    operation_name: "orphaned-operation",
    attributes: [("service.name", "web-service")]
  }
  
  let duplicate_item = {
    id: "item-6",
    trace_id: "trace-123",  // 与valid_item和child_item相同的trace_id
    span_id: "span-456",   // 与valid_item相同的span_id
    parent_span_id: None,
    timestamp: 4000,
    operation_name: "duplicate-operation",
    attributes: [("service.name", "duplicate-service")]
  }
  
  // 测试质量检查
  let all_items = [valid_item, child_item, invalid_item1, invalid_item2, orphaned_item, duplicate_item]
  let quality_result = check_quality(all_items)
  
  assert_eq(quality_result.total_items, 6)
  assert_eq(quality_result.issue_count, 6)  // 每个无效项目一个
  
  // 验证具体问题
  assert_true(quality_result.issues.contains(("item-3", QualityIssue::MissingTraceId)))
  assert_true(quality_result.issues.contains(("item-4", QualityIssue::MissingSpanId)))
  assert_true(quality_result.issues.contains(("item-4", QualityIssue::InvalidTimestamp)))
  assert_true(quality_result.issues.contains(("item-4", QualityIssue::MalformedAttributes)))
  assert_true(quality_result.issues.contains(("item-5", QualityIssue::OrphanedSpan)))
  assert_true(quality_result.issues.contains(("item-6", QualityIssue::DuplicateSpanId)))
  
  // 验证质量分数
  assert_true(quality_result.quality_score >= 0.0 && quality_result.quality_score <= 1.0)
  assert_true(quality_result.quality_score < 1.0)  // 有问题，所以分数应该小于1
  
  // 测试完全有效的数据
  let valid_items = [valid_item, child_item]
  let valid_quality_result = check_quality(valid_items)
  
  assert_eq(valid_quality_result.total_items, 2)
  assert_eq(valid_quality_result.issue_count, 0)
  assert_eq(valid_quality_result.quality_score, 1.0)
}

// 测试8: 遥测数据性能优化
test "遥测数据性能优化" {
  // 定义性能指标
  type PerformanceMetrics = {
    processing_time_ms: Int,
    memory_usage_mb: Float,
    throughput_items_per_sec: Float,
    cpu_usage_percent: Float
  }
  
  // 定义优化策略
  enum OptimizationStrategy {
    NoOptimization
    Batching(Int)  // 批量大小
    Caching(Int)   // 缓存大小
    Compression
    Sampling(Float)  // 采样率
    Combined(Array[OptimizationStrategy])  // 组合策略
  }
  
  // 模拟遥测数据处理
  let process_telemetry_data = fn(items: Array[String], strategy: OptimizationStrategy) {
    let start_time = Time::now()
    
    // 根据策略处理数据
    let processed_items = match strategy {
      OptimizationStrategy::NoOptimization => {
        // 直接处理每个项目
        items.map(fn(item) {
          // 模拟处理时间
          let _ = Time::sleep(1)  // 1ms
          "processed:" + item
        })
      }
      OptimizationStrategy::Batching(batch_size) => {
        // 批量处理
        let mut result = []
        let mut i = 0
        
        while i < items.length() {
          let batch_end = if i + batch_size < items.length() {
            i + batch_size
          } else {
            items.length()
          }
          
          let batch = items.slice(i, batch_end)
          
          // 模拟批量处理时间（比单独处理更高效）
          let _ = Time::sleep(batch.length() / 2)  // 每个项目0.5ms
          
          let processed_batch = batch.map(fn(item) {
            "batch_processed:" + item
          })
          
          result = result.concat(processed_batch)
          i = i + batch_size
        }
        
        result
      }
      OptimizationStrategy::Caching(cache_size) => {
        // 使用缓存处理重复项目
        let cache = Map::empty()
        items.map(fn(item) {
          match Map::get(cache, item) {
            Some(cached_result) => {
              // 从缓存获取，处理时间更短
              let _ = Time::sleep(0)  // 0ms
              cached_result
            }
            None => {
              // 处理并缓存
              let _ = Time::sleep(1)  // 1ms
              let result = "cached:" + item
              // 简化的缓存管理：如果缓存未满，添加新项
              if cache.size() < cache_size {
                let _ = Map::insert(cache, item, result)
              }
              result
            }
          }
        })
      }
      OptimizationStrategy::Compression => {
        // 压缩处理
        items.map(fn(item) {
          // 模拟压缩处理时间
          let _ = Time::sleep(2)  // 2ms，压缩需要更多时间
          "compressed:" + item
        })
      }
      OptimizationStrategy::Sampling(sampling_rate) => {
        // 采样处理
        items.filter_map(fn(item) {
          let hash = item.to_char_array().reduce(0, fn(acc, c) { acc + c.to_int() })
          let normalized = (hash % 100) as Float / 100.0
          
          if normalized <= sampling_rate {
            // 采样的项目需要处理
            let _ = Time::sleep(1)  // 1ms
            Some("sampled:" + item)
          } else {
            // 未采样的项目被丢弃
            None
          }
        })
      }
      OptimizationStrategy::Combined(strategies) => {
        // 组合策略：应用第一个策略
        if strategies.length() > 0 {
          process_telemetry_data(items, strategies[0])
        } else {
          items.map(fn(item) { "processed:" + item })
        }
      }
    }
    
    let end_time = Time::now()
    let processing_time = end_time - start_time
    
    // 计算性能指标
    let throughput = if processing_time > 0 {
      (items.length() as Float) / ((processing_time as Float) / 1000.0)
    } else {
      0.0
    }
    
    // 模拟内存使用（基于处理的项目数量）
    let memory_usage = (processed_items.length() as Float) * 0.1  // 每个项目0.1MB
    
    // 模拟CPU使用（基于处理策略）
    let cpu_usage = match strategy {
      OptimizationStrategy::NoOptimization => 80.0
      OptimizationStrategy::Batching(_) => 60.0
      OptimizationStrategy::Caching(_) => 50.0
      OptimizationStrategy::Compression => 90.0
      OptimizationStrategy::Sampling(_) => 40.0
      OptimizationStrategy::Combined(_) => 70.0
    }
    
    {
      processed_items: processed_items,
      metrics: {
        processing_time_ms: processing_time,
        memory_usage_mb: memory_usage,
        throughput_items_per_sec: throughput,
        cpu_usage_percent: cpu_usage
      }
    }
  }
  
  // 创建测试数据
  let test_items = []
  for i in 0..50 {
    test_items = test_items.push("telemetry-item-" + i.to_string())
  }
  
  // 测试不同优化策略
  let no_opt_result = process_telemetry_data(test_items, OptimizationStrategy::NoOptimization)
  assert_eq(no_opt_result.processed_items.length(), 50)
  assert_true(no_opt_result.metrics.processing_time_ms > 0)
  
  let batch_result = process_telemetry_data(test_items, OptimizationStrategy::Batching(10))
  assert_eq(batch_result.processed_items.length(), 50)
  // 批量处理应该比无优化更快
  assert_true(batch_result.metrics.processing_time_ms <= no_opt_result.metrics.processing_time_ms)
  // 批量处理的CPU使用率应该更低
  assert_true(batch_result.metrics.cpu_usage_percent < no_opt_result.metrics.cpu_usage_percent)
  
  let cache_result = process_telemetry_data(test_items, OptimizationStrategy::Caching(20))
  assert_eq(cache_result.processed_items.length(), 50)
  // 缓存处理的CPU使用率应该更低
  assert_true(cache_result.metrics.cpu_usage_percent < no_opt_result.metrics.cpu_usage_percent)
  
  let compression_result = process_telemetry_data(test_items, OptimizationStrategy::Compression)
  assert_eq(compression_result.processed_items.length(), 50)
  // 压缩处理应该更慢但内存使用更少
  assert_true(compression_result.metrics.processing_time_ms > no_opt_result.metrics.processing_time_ms)
  
  let sampling_result = process_telemetry_data(test_items, OptimizationStrategy::Sampling(0.5))
  // 采样处理应该处理更少的项目
  assert_true(sampling_result.processed_items.length() < 50)
  assert_true(sampling_result.processed_items.length() > 0)
  // 采样处理的吞吐量应该更高
  assert_true(sampling_result.metrics.throughput_items_per_sec > no_opt_result.metrics.throughput_items_per_sec)
  
  // 测试组合策略
  let combined_result = process_telemetry_data(
    test_items, 
    OptimizationStrategy::Combined([OptimizationStrategy::Batching(10)])
  )
  assert_eq(combined_result.processed_items.length(), 50)
  
  // 验证性能指标合理性
  assert_true(no_opt_result.metrics.throughput_items_per_sec > 0)
  assert_true(no_opt_result.metrics.memory_usage_mb > 0)
  assert_true(no_opt_result.metrics.cpu_usage_percent >= 0 && no_opt_result.metrics.cpu_usage_percent <= 100)
  
  // 比较不同策略的吞吐量
  assert_true(batch_result.metrics.throughput_items_per_sec >= no_opt_result.metrics.throughput_items_per_sec)
  assert_true(sampling_result.metrics.throughput_items_per_sec > no_opt_result.metrics.throughput_items_per_sec)
}