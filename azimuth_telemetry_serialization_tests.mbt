// Azimuth Telemetry System - 序列化和反序列化功能测试
// 测试遥测数据的序列化和反序列化功能，确保数据在不同组件间的正确传递

test "AttributeValue序列化和反序列化测试" {
  // 创建各种类型的AttributeValue
  let string_attr = AttributeValue::StringValue("test.string.value")
  let int_attr = AttributeValue::IntValue(42)
  let float_attr = AttributeValue::FloatValue(3.14159)
  let bool_attr = AttributeValue::BoolValue(true)
  let string_array_attr = AttributeValue::ArrayStringValue(["value1", "value2", "value3"])
  let int_array_attr = AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  
  // 模拟序列化过程 - 将AttributeValue转换为字符串表示
  fn serialize_attribute(attr : AttributeValue) -> String {
    match attr {
      AttributeValue::StringValue(s) => "string:" + s
      AttributeValue::IntValue(i) => "int:" + i.to_string()
      AttributeValue::FloatValue(f) => "float:" + f.to_string()
      AttributeValue::BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
      AttributeValue::ArrayStringValue(arr) => {
        let mut result = "array.string:"
        for i = 0; i < arr.length(); i = i + 1 {
          if i > 0 { result = result + "," }
          result = result + arr[i]
        }
        result
      }
      AttributeValue::ArrayIntValue(arr) => {
        let mut result = "array.int:"
        for i = 0; i < arr.length(); i = i + 1 {
          if i > 0 { result = result + "," }
          result = result + arr[i].to_string()
        }
        result
      }
    }
  }
  
  // 模拟反序列化过程 - 将字符串表示转换回AttributeValue
  fn deserialize_attribute(serialized : String) -> AttributeValue {
    if serialized.starts_with("string:") {
      let value = serialized.substring(7, serialized.length() - 7)
      AttributeValue::StringValue(value)
    } else if serialized.starts_with("int:") {
      let value_str = serialized.substring(4, serialized.length() - 4)
      let value = value_str.to_int()
      AttributeValue::IntValue(value)
    } else if serialized.starts_with("float:") {
      let value_str = serialized.substring(6, serialized.length() - 6)
      let value = value_str.to_double()
      AttributeValue::FloatValue(value)
    } else if serialized.starts_with("bool:") {
      let value_str = serialized.substring(5, serialized.length() - 5)
      let value = value_str == "true"
      AttributeValue::BoolValue(value)
    } else if serialized.starts_with("array.string:") {
      let values_str = serialized.substring(12, serialized.length() - 12)
      let values = values_str.split(",")
      AttributeValue::ArrayStringValue(values)
    } else if serialized.starts_with("array.int:") {
      let values_str = serialized.substring(10, serialized.length() - 10)
      let value_strs = values_str.split(",")
      let mut values = []
      for value_str in value_strs {
        values.push(value_str.to_int())
      }
      AttributeValue::ArrayIntValue(values)
    } else {
      AttributeValue::StringValue("unknown")
    }
  }
  
  // 测试各种类型的序列化和反序列化
  let serialized_string = serialize_attribute(string_attr)
  let deserialized_string = deserialize_attribute(serialized_string)
  match deserialized_string {
    AttributeValue::StringValue(s) => assert_eq(s, "test.string.value")
    _ => assert_true(false)
  }
  
  let serialized_int = serialize_attribute(int_attr)
  let deserialized_int = deserialize_attribute(serialized_int)
  match deserialized_int {
    AttributeValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  let serialized_float = serialize_attribute(float_attr)
  let deserialized_float = deserialize_attribute(serialized_float)
  match deserialized_float {
    AttributeValue::FloatValue(f) => assert_eq(f, 3.14159)
    _ => assert_true(false)
  }
  
  let serialized_bool = serialize_attribute(bool_attr)
  let deserialized_bool = deserialize_attribute(serialized_bool)
  match deserialized_bool {
    AttributeValue::BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  let serialized_string_array = serialize_attribute(string_array_attr)
  let deserialized_string_array = deserialize_attribute(serialized_string_array)
  match deserialized_string_array {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "value1")
      assert_eq(arr[1], "value2")
      assert_eq(arr[2], "value3")
    }
    _ => assert_true(false)
  }
  
  let serialized_int_array = serialize_attribute(int_array_attr)
  let deserialized_int_array = deserialize_attribute(serialized_int_array)
  match deserialized_int_array {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false)
  }
}

test "SpanContext序列化和反序列化测试" {
  // 创建一个SpanContext
  let original_context = SpanContext::new(
    "1234567890abcdef1234567890abcdef",
    "1234567890abcdef",
    true,
    "key1=value1,key2=value2"
  )
  
  // 模拟序列化过程
  fn serialize_span_context(ctx : SpanContext) -> String {
    ctx.trace_id + "|" + ctx.span_id + "|" + 
    (if ctx.sampled { "1" } else { "0" }) + "|" + ctx.trace_state
  }
  
  // 模拟反序列化过程
  fn deserialize_span_context(serialized : String) -> SpanContext {
    let parts = serialized.split("|")
    let trace_id = parts[0]
    let span_id = parts[1]
    let sampled = parts[2] == "1"
    let trace_state = if parts.length() > 3 { parts[3] } else { "" }
    SpanContext::new(trace_id, span_id, sampled, trace_state)
  }
  
  // 测试序列化和反序列化
  let serialized = serialize_span_context(original_context)
  let deserialized = deserialize_span_context(serialized)
  
  assert_eq(deserialized.trace_id, original_context.trace_id)
  assert_eq(deserialized.span_id, original_context.span_id)
  assert_eq(deserialized.sampled, original_context.sampled)
  assert_eq(deserialized.trace_state, original_context.trace_state)
  
  // 测试未采样的SpanContext
  let unsampled_context = SpanContext::new(
    "fedcba0987654321fedcba0987654321",
    "fedcba0987654321",
    false,
    ""
  )
  
  let serialized_unsampled = serialize_span_context(unsampled_context)
  let deserialized_unsampled = deserialize_span_context(serialized_unsampled)
  
  assert_eq(deserialized_unsampled.trace_id, unsampled_context.trace_id)
  assert_eq(deserialized_unsampled.span_id, unsampled_context.span_id)
  assert_eq(deserialized_unsampled.sampled, unsampled_context.sampled)
  assert_eq(deserialized_unsampled.trace_state, unsampled_context.trace_state)
}

test "Resource序列化和反序列化测试" {
  // 创建一个Resource
  let original_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(12345)),
    ("process.cpu.usage", FloatValue(0.75)),
    ("process.healthy", BoolValue(true))
  ])
  
  // 模拟序列化过程
  fn serialize_resource(resource : Resource) -> String {
    let mut result = ""
    for i = 0; i < resource.attributes.length(); i = i + 1 {
      if i > 0 { result = result + ";" }
      let (key, value) = resource.attributes[i]
      result = result + key + "="
      match value {
        AttributeValue::StringValue(s) => result = result + "s:" + s
        AttributeValue::IntValue(i) => result = result + "i:" + i.to_string()
        AttributeValue::FloatValue(f) => result = result + "f:" + f.to_string()
        AttributeValue::BoolValue(b) => result = result + "b:" + (if b { "true" } else { "false" })
        _ => result = result + "unknown"
      }
    }
    result
  }
  
  // 模拟反序列化过程
  fn deserialize_resource(serialized : String) -> Resource {
    if serialized == "" {
      return Resource::new()
    }
    
    let attr_pairs = serialized.split(";")
    let mut attributes = []
    
    for pair in attr_pairs {
      let parts = pair.split("=")
      if parts.length() >= 2 {
        let key = parts[0]
        let value_part = parts[1]
        
        if value_part.starts_with("s:") {
          let value = value_part.substring(2, value_part.length() - 2)
          attributes.push((key, StringValue(value)))
        } else if value_part.starts_with("i:") {
          let value_str = value_part.substring(2, value_part.length() - 2)
          let value = value_str.to_int()
          attributes.push((key, IntValue(value)))
        } else if value_part.starts_with("f:") {
          let value_str = value_part.substring(2, value_part.length() - 2)
          let value = value_str.to_double()
          attributes.push((key, FloatValue(value)))
        } else if value_part.starts_with("b:") {
          let value_str = value_part.substring(2, value_part.length() - 2)
          let value = value_str == "true"
          attributes.push((key, BoolValue(value)))
        }
      }
    }
    
    Resource::with_attributes(Resource::new(), attributes)
  }
  
  // 测试序列化和反序列化
  let serialized = serialize_resource(original_resource)
  let deserialized = deserialize_resource(serialized)
  
  // 验证反序列化的属性
  assert_eq(deserialized.attributes.length(), original_resource.attributes.length())
  
  // 检查特定属性
  let service_name = Resource::get_attribute(deserialized, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth.service")
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(deserialized, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
  
  let process_cpu_usage = Resource::get_attribute(deserialized, "process.cpu.usage")
  match process_cpu_usage {
    Some(FloatValue(usage)) => assert_eq(usage, 0.75)
    _ => assert_true(false)
  }
  
  let process_healthy = Resource::get_attribute(deserialized, "process.healthy")
  match process_healthy {
    Some(BoolValue(healthy)) => assert_true(healthy)
    _ => assert_true(false)
  }
}

test "LogRecord序列化和反序列化测试" {
  // 创建一个LogRecord
  let original_log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::with_attributes(Attributes::new(), [
      ("error.code", StringValue("DB_CONN_ERROR")),
      ("retry.count", IntValue(3)),
      ("db.host", StringValue("primary.db.example.com"))
    ])),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("0af7651916cd43dd8448eb211c80319c"),
    Some("b7ad6b7169203331"),
    Some(Context::with_value(Context::root(), ContextKey::new("request.id"), "req-12345"))
  )
  
  // 模拟序列化过程
  fn serialize_log_record(log : LogRecord) -> String {
    let mut result = ""
    
    // 序列化严重级别
    result = result + "severity=" + match log.severity {
      Trace => "trace"
      Debug => "debug"
      Info => "info"
      Warn => "warn"
      Error => "error"
      Fatal => "fatal"
    }
    
    // 序列化消息体
    match log.body {
      Some(body) => result = result + "|body=" + body
      None => ()
    }
    
    // 序列化时间戳
    match log.timestamp {
      Some(ts) => result = result + "|timestamp=" + ts.to_string()
      None => ()
    }
    
    // 序列化观察时间戳
    match log.observed_timestamp {
      Some(ts) => result = result + "|observed=" + ts.to_string()
      None => ()
    }
    
    // 序列化追踪ID
    match log.trace_id {
      Some(trace_id) => result = result + "|trace_id=" + trace_id
      None => ()
    }
    
    // 序列化Span ID
    match log.span_id {
      Some(span_id) => result = result + "|span_id=" + span_id
      None => ()
    }
    
    result
  }
  
  // 模拟反序列化过程
  fn deserialize_log_record(serialized : String) -> LogRecord {
    let parts = serialized.split("|")
    
    let mut severity = Info
    let mut body = None
    let mut timestamp = None
    let mut observed_timestamp = None
    let mut trace_id = None
    let mut span_id = None
    
    for part in parts {
      if part.starts_with("severity=") {
        let severity_str = part.substring(9, part.length() - 9)
        severity = match severity_str {
          "trace" => Trace
          "debug" => Debug
          "info" => Info
          "warn" => Warn
          "error" => Error
          "fatal" => Fatal
          _ => Info
        }
      } else if part.starts_with("body=") {
        body = Some(part.substring(5, part.length() - 5))
      } else if part.starts_with("timestamp=") {
        let ts_str = part.substring(10, part.length() - 10)
        timestamp = Some(ts_str.to_int64())
      } else if part.starts_with("observed=") {
        let ts_str = part.substring(10, part.length() - 10)
        observed_timestamp = Some(ts_str.to_int64())
      } else if part.starts_with("trace_id=") {
        trace_id = Some(part.substring(9, part.length() - 9))
      } else if part.starts_with("span_id=") {
        span_id = Some(part.substring(8, part.length() - 8))
      }
    }
    
    LogRecord::new_with_context(
      severity,
      body,
      None, // 简化实现中忽略属性
      timestamp,
      observed_timestamp,
      trace_id,
      span_id,
      None // 简化实现中忽略上下文
    )
  }
  
  // 测试序列化和反序列化
  let serialized = serialize_log_record(original_log)
  let deserialized = deserialize_log_record(serialized)
  
  // 验证反序列化的字段
  assert_eq(deserialized.severity, Error)
  match deserialized.body {
    Some(b) => assert_eq(b, "Database connection failed")
    None => assert_true(false)
  }
  assert_eq(deserialized.timestamp, Some(1735689600000000000L))
  assert_eq(deserialized.observed_timestamp, Some(1735689600000000001L))
  assert_eq(deserialized.trace_id, Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(deserialized.span_id, Some("b7ad6b7169203331"))
}

test "复合遥测数据序列化测试" {
  // 创建一个包含多种遥测数据的复合结构
  let span_context = SpanContext::new(
    "1234567890abcdef1234567890abcdef",
    "1234567890abcdef",
    true,
    "key1=value1,key2=value2"
  )
  
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth.service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage = Baggage::set_entry(baggage, "request.id", "req-67890")
  
  // 模拟复合序列化
  fn serialize_composite_telemetry(
    span_ctx : SpanContext,
    res : Resource,
    bg : Baggage
  ) -> String {
    let mut result = ""
    
    // 序列化SpanContext
    result = result + "span_context=" + span_ctx.trace_id + "|" + span_ctx.span_id + "|" + 
    (if span_ctx.sampled { "1" } else { "0" }) + "|" + span_ctx.trace_state
    
    // 序列化Resource
    result = result + "|resource="
    for i = 0; i < res.attributes.length(); i = i + 1 {
      if i > 0 { result = result + "," }
      let (key, value) = res.attributes[i]
      result = result + key + ":"
      match value {
        AttributeValue::StringValue(s) => result = result + s
        _ => result = result + "other"
      }
    }
    
    // 序列化Baggage
    result = result + "|baggage="
    for i = 0; i < bg.entries.length(); i = i + 1 {
      if i > 0 { result = result + "," }
      let (key, value) = bg.entries[i]
      result = result + key + "=" + value
    }
    
    result
  }
  
  // 测试复合序列化
  let serialized = serialize_composite_telemetry(span_context, resource, baggage)
  
  // 验证序列化结果包含所有必要信息
  assert_true(serialized.contains("span_context="))
  assert_true(serialized.contains("1234567890abcdef1234567890abcdef"))
  assert_true(serialized.contains("1234567890abcdef"))
  assert_true(serialized.contains("resource="))
  assert_true(serialized.contains("service.name:azimuth.service"))
  assert_true(serialized.contains("service.version:1.0.0"))
  assert_true(serialized.contains("baggage="))
  assert_true(serialized.contains("user.id=12345"))
  assert_true(serialized.contains("request.id=req-67890"))
}