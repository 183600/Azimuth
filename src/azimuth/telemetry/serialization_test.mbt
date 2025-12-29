// 序列化/反序列化测试 - 测试遥测数据的序列化和反序列化功能
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, SpanLink, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

// 辅助函数：将AttributeValue转换为字符串表示（模拟序列化）
fn attribute_value_to_string(attr : AttributeValue) -> String {
  match attr {
    StringValue(s) => "string:" + s
    IntValue(i) => "int:" + i.to_string()
    FloatValue(f) => "float:" + f.to_string()
    BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
    ArrayStringValue(arr) => {
      let result = "array_string:["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + arr[i]
        i = i + 1
      }
      result + "]"
    }
    ArrayIntValue(arr) => {
      let result = "array_int:["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + arr[i].to_string()
        i = i + 1
      }
      result + "]"
    }
    ArrayFloatValue(arr) => {
      let result = "array_float:["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + arr[i].to_string()
        i = i + 1
      }
      result + "]"
    }
    ArrayBoolValue(arr) => {
      let result = "array_bool:["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { result = result + "," }
        result = result + (if arr[i] { "true" } else { "false" })
        i = i + 1
      }
      result + "]"
    }
  }
}

// 辅助函数：将字节数组转换为十六进制字符串（模拟序列化）
fn bytes_to_hex(bytes : Array[Byte]) -> String {
  let result = ""
  let mut i = 0
  while i < bytes.length() {
    if i > 0 { result = result + " " }
    let byte_value = bytes[i].to_int()
    let hex_char = if byte_value < 16 {
      "0" + byte_value.to_string(16)
    } else {
      byte_value.to_string(16)
    }
    result = result + hex_char
    i = i + 1
  }
  result
}

test "serialization_attribute_values" {
  // 测试AttributeValue的序列化
  
  // 基本类型
  let string_attr = AttributeValue::string("test_value")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  
  let string_serialized = attribute_value_to_string(string_attr)
  let int_serialized = attribute_value_to_string(int_attr)
  let float_serialized = attribute_value_to_string(float_attr)
  let bool_serialized = attribute_value_to_string(bool_attr)
  
  assert_eq(string_serialized, "string:test_value")
  assert_eq(int_serialized, "int:42")
  assert_eq(float_serialized, "float:3.14159")
  assert_eq(bool_serialized, "bool:true")
  
  // 数组类型
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  let string_array_serialized = attribute_value_to_string(string_array)
  let int_array_serialized = attribute_value_to_string(int_array)
  let float_array_serialized = attribute_value_to_string(float_array)
  let bool_array_serialized = attribute_value_to_string(bool_array)
  
  assert_eq(string_array_serialized, "array_string:[a,b,c]")
  assert_eq(int_array_serialized, "array_int:[1,2,3]")
  assert_eq(float_array_serialized, "array_float:[1.1,2.2,3.3]")
  assert_eq(bool_array_serialized, "array_bool:[true,false,true]")
  
  // 边界情况
  let empty_string = AttributeValue::string("")
  let empty_string_array = AttributeValue::array_string([])
  
  let empty_string_serialized = attribute_value_to_string(empty_string)
  let empty_string_array_serialized = attribute_value_to_string(empty_string_array)
  
  assert_eq(empty_string_serialized, "string:")
  assert_eq(empty_string_array_serialized, "array_string:[]")
}

test "serialization_span_context" {
  // 测试SpanContext的序列化
  
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x0a_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x0b_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 序列化trace_id和span_id
  let trace_id_hex = bytes_to_hex(span_context.trace_id)
  let span_id_hex = bytes_to_hex(span_context.span_id)
  
  // 验证序列化结果
  assert_eq(trace_id_hex, "0a 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a 0a")
  assert_eq(span_id_hex, "0b 0b 0b 0b 0b 0b 0b 0b")
  
  // 验证其他字段
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // 测试全零和全1的情况
  let zero_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let zero_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  let zero_context = SpanContext::{
    trace_id: zero_trace_id,
    span_id: zero_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  let zero_trace_hex = bytes_to_hex(zero_context.trace_id)
  let zero_span_hex = bytes_to_hex(zero_context.span_id)
  
  assert_eq(zero_trace_hex, "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00")
  assert_eq(zero_span_hex, "00 00 00 00 00 00 00 00")
  
  let max_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xff_byte })
  let max_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xff_byte })
  let max_context = SpanContext::{
    trace_id: max_trace_id,
    span_id: max_span_id,
    trace_flags: 0xff_byte,
    trace_state: ""
  }
  
  let max_trace_hex = bytes_to_hex(max_context.trace_id)
  let max_span_hex = bytes_to_hex(max_context.span_id)
  
  assert_eq(max_trace_hex, "ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff")
  assert_eq(max_span_hex, "ff ff ff ff ff ff ff ff")
}

test "serialization_span_data" {
  // 测试Span数据的序列化
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("serialization-test", Some("1.0.0"))
  
  // 创建带有各种属性的span
  let complex_attributes = [
    ("string.key", AttributeValue::string("string_value")),
    ("int.key", AttributeValue::int(42L)),
    ("float.key", AttributeValue::float(3.14)),
    ("bool.key", AttributeValue::bool(true)),
    ("array.string", AttributeValue::array_string(["a", "b", "c"])),
    ("array.int", AttributeValue::array_int([1L, 2L, 3L]))
  ]
  
  let (_, span) = tracer.start_span(
    ctx, 
    "serialization_test_span", 
    Server, 
    Some(complex_attributes)
  )
  
  // 序列化span基本信息
  let span_name_serialized = "name:" + span.name
  let span_kind_serialized = "kind:" + match span.kind {
    Internal => "internal"
    Server => "server"
    Client => "client"
    Producer => "producer"
    Consumer => "consumer"
  }
  let span_status_serialized = "status:" + match span.status {
    Unset => "unset"
    Ok => "ok"
    Error => "error"
  }
  
  // 序列化span属性
  let attributes_serialized = "attributes:["
  let mut i = 0
  while i < span.attributes.length() {
    if i > 0 { attributes_serialized = attributes_serialized + "," }
    let (key, value) = span.attributes[i]
    attributes_serialized = attributes_serialized + key + "=" + attribute_value_to_string(value)
    i = i + 1
  }
  attributes_serialized = attributes_serialized + "]"
  
  // 验证序列化结果
  assert_eq(span_name_serialized, "name:serialization_test_span")
  assert_eq(span_kind_serialized, "kind:server")
  assert_eq(span_status_serialized, "status:unset")
  assert_eq(attributes_serialized.contains("string.key=string:string_value"), true)
  assert_eq(attributes_serialized.contains("int.key=int:42"), true)
  assert_eq(attributes_serialized.contains("float.key=float:3.14"), true)
  assert_eq(attributes_serialized.contains("bool.key=bool:true"), true)
  assert_eq(attributes_serialized.contains("array.string=array_string:[a,b,c]"), true)
  assert_eq(attributes_serialized.contains("array.int=array_int:[1,2,3]"), true)
}

test "serialization_log_record" {
  // 测试LogRecord的序列化
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Error)
    .body("Test log message")
    .with_attribute("service.name", AttributeValue::string("test-service"))
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .with_attribute("tags", AttributeValue::array_string(["api", "error"]))
    .build()
  
  // 序列化日志记录
  let timestamp_serialized = "timestamp:" + log_record.timestamp_unix_nanos.to_string()
  let severity_serialized = "severity:" + match log_record.severity_number {
    Trace => "trace"
    Debug => "debug"
    Info => "info"
    Warn => "warn"
    Error => "error"
    Fatal => "fatal"
  }
  let body_serialized = match log_record.body {
    Some(body_text) => "body:" + body_text
    None => "body:"
  }
  
  // 序列化属性
  let attributes_serialized = "attributes:["
  let mut i = 0
  while i < log_record.attributes.length() {
    if i > 0 { attributes_serialized = attributes_serialized + "," }
    let (key, value) = log_record.attributes[i]
    attributes_serialized = attributes_serialized + key + "=" + attribute_value_to_string(value)
    i = i + 1
  }
  attributes_serialized = attributes_serialized + "]"
  
  // 验证序列化结果
  assert_eq(timestamp_serialized, "timestamp:1640995200000000000")
  assert_eq(severity_serialized, "severity:error")
  assert_eq(body_serialized, "body:Test log message")
  assert_eq(attributes_serialized.contains("service.name=string:test-service"), true)
  assert_eq(attributes_serialized.contains("error.code=int:500"), true)
  assert_eq(attributes_serialized.contains("retry.count=int:3"), true)
  assert_eq(attributes_serialized.contains("tags=array_string:[api,error]"), true)
}

test "serialization_context_and_baggage" {
  // 测试Context和Baggage的序列化
  
  // 创建带有多个值的Context
  let ctx = Context::empty()
  let key1 = create_key("user.id")
  let key2 = create_key("request.id")
  let key3 = create_key("session.id")
  
  let ctx_with_values = ctx
    .with_value(key1, "user-12345")
    .with_value(key2, "req-67890")
    .with_value(key3, "sess-abcde")
  
  // 序列化Context
  let context_serialized = "context:["
  let mut i = 0
  while i < ctx_with_values.values.length() {
    if i > 0 { context_serialized = context_serialized + "," }
    let (key, value) = ctx_with_values.values[i]
    context_serialized = context_serialized + key + "=" + value
    i = i + 1
  }
  context_serialized = context_serialized + "]"
  
  // 验证Context序列化
  assert_eq(context_serialized.contains("user.id=user-12345"), true)
  assert_eq(context_serialized.contains("request.id=req-67890"), true)
  assert_eq(context_serialized.contains("session.id=sess-abcde"), true)
  
  // 创建Baggage
  let baggage = Baggage::empty()
    .with_entry("correlation.id", "corr-12345")
    .with_entry("tenant.id", "tenant-67890")
    .with_entry("region", "us-west-2")
  
  // 序列化Baggage
  let baggage_serialized = "baggage:["
  let mut j = 0
  while j < baggage.entries.length() {
    if j > 0 { baggage_serialized = baggage_serialized + "," }
    let (key, value) = baggage.entries[j]
    baggage_serialized = baggage_serialized + key + "=" + value
    j = j + 1
  }
  baggage_serialized = baggage_serialized + "]"
  
  // 验证Baggage序列化
  assert_eq(baggage_serialized.contains("correlation.id=corr-12345"), true)
  assert_eq(baggage_serialized.contains("tenant.id=tenant-67890"), true)
  assert_eq(baggage_serialized.contains("region=us-west-2"), true)
}

test "serialization_resource" {
  // 测试Resource的序列化
  
  let resource = Resource::default("test-service")
  
  // 序列化Resource基本信息
  let service_name_serialized = "service_name:" + resource.service_name
  let sdk_name_serialized = "sdk_name:" + resource.telemetry_sdk_name
  let sdk_version_serialized = "sdk_version:" + resource.telemetry_sdk_version
  let service_version_serialized = match resource.service_version {
    Some(version) => "service_version:" + version
    None => "service_version:"
  }
  
  // 验证Resource序列化
  assert_eq(service_name_serialized, "service_name:test-service")
  assert_eq(sdk_name_serialized, "sdk_name:azimuth")
  assert_eq(sdk_version_serialized, "sdk_version:0.1.0")
  assert_eq(service_version_serialized, "service_version:")
  
  // 测试带有属性的Resource
  let complex_resource = Resource::default("complex-service")
  let resource_attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("cloud.region", AttributeValue::string("us-west-2")),
    ("tags", AttributeValue::array_string(["web", "api", "critical"]))
  ]
  
  // 序列化Resource属性
  let resource_attributes_serialized = "resource_attributes:["
  let mut k = 0
  while k < resource_attributes.length() {
    if k > 0 { resource_attributes_serialized = resource_attributes_serialized + "," }
    let (key, value) = resource_attributes[k]
    resource_attributes_serialized = resource_attributes_serialized + key + "=" + attribute_value_to_string(value)
    k = k + 1
  }
  resource_attributes_serialized = resource_attributes_serialized + "]"
  
  // 验证Resource属性序列化
  assert_eq(resource_attributes_serialized.contains("service.namespace=string:production"), true)
  assert_eq(resource_attributes_serialized.contains("service.version=string:2.1.0"), true)
  assert_eq(resource_attributes_serialized.contains("deployment.environment=string:prod"), true)
  assert_eq(resource_attributes_serialized.contains("host.name=string:web-server-01"), true)
  assert_eq(resource_attributes_serialized.contains("process.pid=int:12345"), true)
  assert_eq(resource_attributes_serialized.contains("cloud.region=string:us-west-2"), true)
  assert_eq(resource_attributes_serialized.contains("tags=array_string:[web,api,critical]"), true)
}

test "serialization_complex_scenarios" {
  // 测试复杂序列化场景
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("complex-test", Some("1.0.0"))
  
  // 创建复杂的span事件
  let event_attributes = [
    ("event.type", AttributeValue::string("error")),
    ("error.code", AttributeValue::int(500)),
    ("error.message", AttributeValue::string("Internal server error")),
    ("stack.trace", AttributeValue::string("at function1() at line 10\nat function2() at line 20")),
    ("context.data", AttributeValue::array_string(["key1=value1", "key2=value2"]))
  ]
  
  let span_event = SpanEvent::{
    name: "error_occurred",
    timestamp_unix_nanos: 1640995200000000000L,
    attributes: event_attributes
  }
  
  // 序列化SpanEvent
  let event_name_serialized = "event_name:" + span_event.name
  let event_timestamp_serialized = "event_timestamp:" + span_event.timestamp_unix_nanos.to_string()
  
  let event_attributes_serialized = "event_attributes:["
  let mut i = 0
  while i < span_event.attributes.length() {
    if i > 0 { event_attributes_serialized = event_attributes_serialized + "," }
    let (key, value) = span_event.attributes[i]
    event_attributes_serialized = event_attributes_serialized + key + "=" + attribute_value_to_string(value)
    i = i + 1
  }
  event_attributes_serialized = event_attributes_serialized + "]"
  
  // 验证SpanEvent序列化
  assert_eq(event_name_serialized, "event_name:error_occurred")
  assert_eq(event_timestamp_serialized, "event_timestamp:1640995200000000000")
  assert_eq(event_attributes_serialized.contains("event.type=string:error"), true)
  assert_eq(event_attributes_serialized.contains("error.code=int:500"), true)
  assert_eq(event_attributes_serialized.contains("error.message=string:Internal server error"), true)
  
  // 创建复杂的SpanLink
  let linked_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte })
  let linked_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte })
  let linked_context = SpanContext::{
    trace_id: linked_trace_id,
    span_id: linked_span_id,
    trace_flags: 1_byte,
    trace_state: "key=value"
  }
  
  let link_attributes = [
    ("link.type", AttributeValue::string("parent")),
    ("parent.service", AttributeValue::string("api-gateway")),
    ("parent.span", AttributeValue::string("request-handler"))
  ]
  
  let span_link = SpanLink::{
    context: linked_context,
    attributes: link_attributes
  }
  
  // 序列化SpanLink
  let link_trace_id_serialized = "link_trace_id:" + bytes_to_hex(span_link.context.trace_id)
  let link_span_id_serialized = "link_span_id:" + bytes_to_hex(span_link.context.span_id)
  let link_trace_flags_serialized = "link_trace_flags:" + span_link.context.trace_flags.to_string()
  let link_trace_state_serialized = "link_trace_state:" + span_link.context.trace_state
  
  let link_attributes_serialized = "link_attributes:["
  let mut j = 0
  while j < span_link.attributes.length() {
    if j > 0 { link_attributes_serialized = link_attributes_serialized + "," }
    let (key, value) = span_link.attributes[j]
    link_attributes_serialized = link_attributes_serialized + key + "=" + attribute_value_to_string(value)
    j = j + 1
  }
  link_attributes_serialized = link_attributes_serialized + "]"
  
  // 验证SpanLink序列化
  assert_eq(link_trace_id_serialized, "link_trace_id:01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01")
  assert_eq(link_span_id_serialized, "link_span_id:02 02 02 02 02 02 02 02")
  assert_eq(link_trace_flags_serialized, "link_trace_flags:1")
  assert_eq(link_trace_state_serialized, "link_trace_state:key=value")
  assert_eq(link_attributes_serialized.contains("link.type=string:parent"), true)
  assert_eq(link_attributes_serialized.contains("parent.service=string:api-gateway"), true)
  assert_eq(link_attributes_serialized.contains("parent.span=string:request-handler"), true)
}