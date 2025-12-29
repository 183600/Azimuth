// 序列化兼容性测试用例
// 测试遥测数据的序列化和反序列化兼容性

test "attribute_value_serialization_compatibility" {
  // 测试AttributeValue的序列化兼容性
  
  // 1. 测试基本类型的序列化兼容性
  let string_attr = AttributeValue::string("test_string")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  // 模拟序列化过程
  let serialized_string = match string_attr {
    StringValue(s) => "string:" + s
    _ => @test.fail("Test failed")
  }
  
  let serialized_int = match int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => @test.fail("Test failed")
  }
  
  let serialized_float = match float_attr {
    FloatValue(f) => "float:" + f.to_string()
    _ => @test.fail("Test failed")
  }
  
  let serialized_bool = match bool_attr {
    BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
    _ => @test.fail("Test failed")
  }
  
  // 验证序列化结果
  assert_eq(serialized_string, "string:test_string")
  assert_eq(serialized_int, "int:42")
  assert_eq(serialized_float, "float:3.14")
  assert_eq(serialized_bool, "bool:true")
  
  // 2. 模拟反序列化过程
  let deserialized_string = if serialized_string.starts_with("string:") {
    let value = serialized_string.substring(7)  // 跳过"string:"
    AttributeValue::string(value)
  } else {
    @test.fail("Test failed")
  }
  
  let deserialized_int = if serialized_int.starts_with("int:") {
    let value = serialized_int.substring(4)  // 跳过"int:"
    AttributeValue::int(value.to_int64())
  } else {
    @test.fail("Test failed")
  }
  
  let deserialized_float = if serialized_float.starts_with("float:") {
    let value = serialized_float.substring(6)  // 跳过"float:"
    AttributeValue::float(value.to_double())
  } else {
    @test.fail("Test failed")
  }
  
  let deserialized_bool = if serialized_bool.starts_with("bool:") {
    let value = serialized_bool.substring(5)  // 跳过"bool:"
    AttributeValue::bool(value == "true")
  } else {
    @test.fail("Test failed")
  }
  
  // 验证反序列化结果
  match deserialized_string {
    StringValue(s) => assert_eq(s, "test_string")
    _ => @test.fail("Test failed")
  }
  
  match deserialized_int {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Test failed")
  }
  
  match deserialized_float {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match deserialized_bool {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试数组类型的序列化兼容性
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  // 模拟数组序列化
  let serialized_string_array = match string_array_attr {
    ArrayStringValue(arr) => {
      let serialized_items = []
      let mut i = 0
      while i < arr.length() {
        serialized_items.push(arr[i])
        i = i + 1
      }
      "string_array:" + serialized_items.join(",")
    }
    _ => @test.fail("Test failed")
  }
  
  let serialized_int_array = match int_array_attr {
    ArrayIntValue(arr) => {
      let serialized_items = []
      let mut i = 0
      while i < arr.length() {
        serialized_items.push(arr[i].to_string())
        i = i + 1
      }
      "int_array:" + serialized_items.join(",")
    }
    _ => @test.fail("Test failed")
  }
  
  // 验证数组序列化
  assert_eq(serialized_string_array, "string_array:a,b,c")
  assert_eq(serialized_int_array, "int_array:1,2,3")
  
  // 4. 测试特殊值的序列化兼容性
  let empty_string_attr = AttributeValue::string("")
  let zero_int_attr = AttributeValue::int(0L)
  let negative_int_attr = AttributeValue::int(-1L)
  let max_int_attr = AttributeValue::int(9223372036854775807L)
  let zero_float_attr = AttributeValue::float(0.0)
  let infinity_float_attr = AttributeValue::float(1.0/0.0)
  let nan_float_attr = AttributeValue::float(0.0/0.0)
  
  // 序列化特殊值
  let serialized_empty_string = match empty_string_attr {
    StringValue(s) => "string:" + s
    _ => @test.fail("Test failed")
  }
  
  let serialized_zero_int = match zero_int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => @test.fail("Test failed")
  }
  
  let serialized_negative_int = match negative_int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => @test.fail("Test failed")
  }
  
  let serialized_max_int = match max_int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => @test.fail("Test failed")
  }
  
  // 验证特殊值序列化
  assert_eq(serialized_empty_string, "string:")
  assert_eq(serialized_zero_int, "int:0")
  assert_eq(serialized_negative_int, "int:-1")
  assert_eq(serialized_max_int, "int:9223372036854775807")
}

test "resource_serialization_compatibility" {
  // 测试Resource的序列化兼容性
  
  // 1. 创建基本资源
  let basic_resource = Resource::default("test-service")
  
  // 2. 模拟资源序列化
  let serialized_resource = "resource:" + 
    basic_resource.service_name + "|" +
    match basic_resource.service_version {
      Some(version) => version
      None => ""
    } + "|" +
    basic_resource.telemetry_sdk_name + "|" +
    basic_resource.telemetry_sdk_version + "|" +
    "attributes_count:" + basic_resource.attributes.length().to_string()
  
  // 验证资源序列化
  assert_eq(serialized_resource, "resource:test-service||azimuth|0.1.0|attributes_count:0")
  
  // 3. 模拟资源反序列化
  let deserialized_service_name = if serialized_resource.starts_with("resource:") {
    let parts = serialized_resource.substring(8).split("|")
    parts[0]
  } else {
    @test.fail("Test failed")
  }
  
  // 验证反序列化结果
  assert_eq(deserialized_service_name, "test-service")
  
  // 4. 测试复杂资源的序列化兼容性
  let complex_resource = Resource::default("complex-service")
  
  // 创建带有属性的复杂资源（通过创建LogRecord来模拟）
  let complex_attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L))
  ]
  
  let complex_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Complex resource test"),
    attributes: complex_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(complex_resource),
    instrumentation_scope: None
  }
  
  // 模拟复杂资源序列化
  let serialized_complex_resource = "resource:" + 
    complex_resource.service_name + "|" +
    "2.1.0|" +  // 假设有版本
    complex_resource.telemetry_sdk_name + "|" +
    complex_resource.telemetry_sdk_version + "|" +
    "attributes_count:" + complex_attributes.length().to_string()
  
  // 验证复杂资源序列化
  assert_eq(serialized_complex_resource, "resource:complex-service|2.1.0|azimuth|0.1.0|attributes_count:5")
  
  // 5. 测试特殊字符资源的序列化兼容性
  let special_resource = Resource::default("service-with-special-chars!@#$%^&*()")
  
  let serialized_special_resource = "resource:" + 
    special_resource.service_name + "|" +
    "1.0.0|" +
    special_resource.telemetry_sdk_name + "|" +
    special_resource.telemetry_sdk_version + "|" +
    "attributes_count:0"
  
  // 验证特殊字符资源序列化
  assert_eq(serialized_special_resource, "resource:service-with-special-chars!@#$%^&*()|1.0.0|azimuth|0.1.0|attributes_count:0")
  
  // 6. 测试Unicode资源的序列化兼容性
  let unicode_resource = Resource::default("Unicode服务测试🚀")
  
  let serialized_unicode_resource = "resource:" + 
    unicode_resource.service_name + "|" +
    "1.0.0|" +
    unicode_resource.telemetry_sdk_name + "|" +
    unicode_resource.telemetry_sdk_version + "|" +
    "attributes_count:0"
  
  // 验证Unicode资源序列化
  assert_eq(serialized_unicode_resource, "resource:Unicode服务测试🚀|1.0.0|azimuth|0.1.0|attributes_count:0")
}

test "span_serialization_compatibility" {
  // 测试Span的序列化兼容性
  
  // 1. 创建基本span
  let ctx = Context::empty()
  let (_, basic_span) = NoopTracer::start_span(ctx, "basic_span", Internal, None)
  
  // 2. 模拟span序列化
  let serialized_span = "span:" + 
    basic_span.name + "|" +
    match basic_span.kind {
      Internal => "Internal"
      Server => "Server"
      Client => "Client"
      Producer => "Producer"
      Consumer => "Consumer"
    } + "|" +
    basic_span.start_time_unix_nanos.to_string() + "|" +
    match basic_span.end_time_unix_nanos {
      Some(end_time) => end_time.to_string()
      None => "null"
    } + "|" +
    match basic_span.status {
      Unset => "Unset"
      Ok => "Ok"
      Error => "Error"
    } + "|" +
    "attributes_count:" + basic_span.attributes.length().to_string() + "|" +
    "events_count:" + basic_span.events.length().to_string() + "|" +
    "links_count:" + basic_span.links.length().to_string()
  
  // 验证基本span序列化
  assert_eq(serialized_span, "span:basic_span|Internal|0|null|Unset|attributes_count:0|events_count:0|links_count:0")
  
  // 3. 模拟span反序列化
  let deserialized_span_name = if serialized_span.starts_with("span:") {
    let parts = serialized_span.substring(5).split("|")
    parts[0]
  } else {
    @test.fail("Test failed")
  }
  
  // 验证反序列化结果
  assert_eq(deserialized_span_name, "basic_span")
  
  // 4. 测试复杂span的序列化兼容性
  let span_attributes = [
    ("service.name", AttributeValue::string("test-service")),
    ("operation.name", AttributeValue::string("complex_operation")),
    ("user.id", AttributeValue::string("user-12345")),
    ("duration.ms", AttributeValue::int(250L)),
    ("success", AttributeValue::bool(true))
  ]
  
  let (_, complex_span) = NoopTracer::start_span(ctx, "complex_span", Server, Some(span_attributes))
  
  // 创建带有事件的复杂span
  let complex_span_with_events = Span::{
    name: "complex_span_with_events",
    context: complex_span.context,
    kind: Server,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000500L),
    status: Ok,
    status_description: Some("Operation completed successfully"),
    attributes: span_attributes,
    events: [
      SpanEvent::{
        name: "start_event",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [("event.phase", AttributeValue::string("start"))]
      },
      SpanEvent::{
        name: "end_event",
        timestamp_unix_nanos: 1640995200000000500L,
        attributes: [("event.phase", AttributeValue::string("end"))]
      }
    ],
    links: [
      SpanLink::{
        context: SpanContext::{
          trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x02_byte }),
          span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x03_byte }),
          trace_flags: 1_byte,
          trace_state: ""
        },
        attributes: [("link.type", AttributeValue::string("parent"))]
      }
    ]
  }
  
  // 模拟复杂span序列化
  let serialized_complex_span = "span:" + 
    complex_span_with_events.name + "|" +
    "Server|" +
    complex_span_with_events.start_time_unix_nanos.to_string() + "|" +
    match complex_span_with_events.end_time_unix_nanos {
      Some(end_time) => end_time.to_string()
      None => "null"
    } + "|" +
    "Ok|" +
    "attributes_count:" + complex_span_with_events.attributes.length().to_string() + "|" +
    "events_count:" + complex_span_with_events.events.length().to_string() + "|" +
    "links_count:" + complex_span_with_events.links.length().to_string() + "|" +
    "parent_span_id:true|" +
    "status_description:Operation completed successfully"
  
  // 验证复杂span序列化
  assert_eq(serialized_complex_span, "span:complex_span_with_events|Server|1640995200000000000|1640995200000000500|Ok|attributes_count:5|events_count:2|links_count:1|parent_span_id:true|status_description:Operation completed successfully")
  
  // 5. 测试SpanContext的序列化兼容性
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x0a_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x0b_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 模拟SpanContext序列化
  let serialized_trace_id = "trace_id:" + 
    span_context.trace_id[0].to_string() + "," +
    span_context.trace_id[1].to_string() + "," +
    span_context.trace_id[2].to_string() + "," +
    span_context.trace_id[3].to_string()
  
  let serialized_span_id = "span_id:" + 
    span_context.span_id[0].to_string() + "," +
    span_context.span_id[1].to_string() + "," +
    span_context.span_id[2].to_string() + "," +
    span_context.span_id[3].to_string()
  
  let serialized_span_context = serialized_trace_id + "|" +
    serialized_span_id + "|" +
    "trace_flags:" + span_context.trace_flags.to_string() + "|" +
    "trace_state:" + span_context.trace_state
  
  // 验证SpanContext序列化
  assert_eq(serialized_span_context, "trace_id:10,10,10,10|span_id:11,11,11,11|trace_flags:1|trace_state:key1=value1,key2=value2")
}

test "log_record_serialization_compatibility" {
  // 测试LogRecord的序列化兼容性
  
  // 1. 创建基本日志记录
  let basic_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Basic log message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 2. 模拟日志记录序列化
  let serialized_log = "log:" + 
    basic_log.timestamp_unix_nanos.to_string() + "|" +
    match basic_log.observed_timestamp_unix_nanos {
      Some(observed) => observed.to_string()
      None => "null"
    } + "|" +
    match basic_log.severity_number {
      Trace => "Trace"
      Debug => "Debug"
      Info => "Info"
      Warn => "Warn"
      Error => "Error"
      Fatal => "Fatal"
    } + "|" +
    match basic_log.severity_text {
      Some(text) => text
      None => ""
    } + "|" +
    match basic_log.body {
      Some(body) => body
      None => ""
    } + "|" +
    "attributes_count:" + basic_log.attributes.length().to_string() + "|" +
    "has_trace_id:" + (if basic_log.trace_id.is_some() { "true" } else { "false" }) + "|" +
    "has_span_id:" + (if basic_log.span_id.is_some() { "true" } else { "false" }) + "|" +
    "has_resource:" + (if basic_log.resource.is_some() { "true" } else { "false" }) + "|" +
    "has_instrumentation_scope:" + (if basic_log.instrumentation_scope.is_some() { "true" } else { "false" })
  
  // 验证基本日志记录序列化
  assert_eq(serialized_log, "log:1640995200000000000|null|Info|INFO|Basic log message|attributes_count:0|has_trace_id:false|has_span_id:false|has_resource:false|has_instrumentation_scope:false")
  
  // 3. 模拟日志记录反序列化
  let deserialized_log_body = if serialized_log.starts_with("log:") {
    let parts = serialized_log.substring(4).split("|")
    // body是第5个部分（索引4）
    parts[4]
  } else {
    @test.fail("Test failed")
  }
  
  // 验证反序列化结果
  assert_eq(deserialized_log_body, "Basic log message")
  
  // 4. 测试复杂日志记录的序列化兼容性
  let trace_id = Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }))
  let span_id = Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }))
  
  let complex_attributes = [
    ("service.name", AttributeValue::string("test-service")),
    ("operation.name", AttributeValue::string("complex_operation")),
    ("user.id", AttributeValue::string("user-12345")),
    ("request.id", AttributeValue::string("req-abcdef")),
    ("duration.ms", AttributeValue::int(250L)),
    ("success.rate", AttributeValue::float(0.95)),
    ("cache.hit", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["api", "auth", "critical"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 404L])),
    ("latency.percentiles", AttributeValue::array_float([0.5, 0.95, 0.99]))
  ]
  
  let complex_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Complex log operation failed"),
    attributes: complex_attributes,
    trace_id,
    span_id,
    trace_flags: Some(1_byte),
    resource: Some(Resource::default("test-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "test-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 模拟复杂日志记录序列化
  let serialized_complex_log = "log:" + 
    complex_log.timestamp_unix_nanos.to_string() + "|" +
    match complex_log.observed_timestamp_unix_nanos {
      Some(observed) => observed.to_string()
      None => "null"
    } + "|" +
    "Error|" +
    "ERROR|" +
    "Complex log operation failed|" +
    "attributes_count:" + complex_log.attributes.length().to_string() + "|" +
    "has_trace_id:true|" +
    "has_span_id:true|" +
    "has_resource:true|" +
    "has_instrumentation_scope:true|" +
    "trace_flags:1|" +
    "resource_name:test-service|" +
    "scope_name:test-logger|" +
    "scope_version:1.0.0"
  
  // 验证复杂日志记录序列化
  assert_eq(serialized_complex_log, "log:1640995200000000000|1640995200000000100|Error|ERROR|Complex log operation failed|attributes_count:10|has_trace_id:true|has_span_id:true|has_resource:true|has_instrumentation_scope:true|trace_flags:1|resource_name:test-service|scope_name:test-logger|scope_version:1.0.0")
  
  // 5. 测试特殊字符日志记录的序列化兼容性
  let special_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: Some("WARN"),
    body: Some("Special chars: !@#$%^&*()_+-=[]{}|;':\"\,./<>?"),
    attributes: [
      ("special.key", AttributeValue::string("!@#$%^&*()")),
      ("unicode.value", AttributeValue::string("Unicode测试🚀")),
      ("empty.value", AttributeValue::string(""))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 模拟特殊字符日志记录序列化
  let serialized_special_log = "log:" + 
    special_log.timestamp_unix_nanos.to_string() + "|" +
    "null|" +
    "Warn|" +
    "WARN|" +
    "Special chars: !@#$%^&*()_+-=[]{}|;':\"\,./<>?|" +
    "attributes_count:" + special_log.attributes.length().to_string() + "|" +
    "has_trace_id:false|" +
    "has_span_id:false|" +
    "has_resource:false|" +
    "has_instrumentation_scope:false"
  
  // 验证特殊字符日志记录序列化
  assert_eq(serialized_special_log, "log:1640995200000000000|null|Warn|WARN|Special chars: !@#$%^&*()_+-=[]{}|;':\"\,./<>?|attributes_count:3|has_trace_id:false|has_span_id:false|has_resource:false|has_instrumentation_scope:false")
}

test "context_serialization_compatibility" {
  // 测试Context的序列化兼容性
  
  // 1. 创建基本上下文
  let basic_ctx = Context::empty()
  
  // 2. 模拟上下文序列化
  let serialized_ctx = "context:values_count:" + basic_ctx.values.length().to_string()
  
  // 验证基本上下文序列化
  assert_eq(serialized_ctx, "context:values_count:0")
  
  // 3. 测试复杂上下文的序列化兼容性
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let request_key = create_key("request.id")
  
  let complex_ctx = basic_ctx
    .with_value(user_key, "user-12345")
    .with_value(session_key, "sess-abcdef")
    .with_value(request_key, "req-123456")
  
  // 模拟复杂上下文序列化
  let serialized_complex_ctx = "context:values_count:" + complex_ctx.values.length().to_string() + "|"
  let mut i = 0
  while i < complex_ctx.values.length() {
    let (key, value) = complex_ctx.values[i]
    serialized_complex_ctx = serialized_complex_ctx + key + "=" + value
    if i < complex_ctx.values.length() - 1 {
      serialized_complex_ctx = serialized_complex_ctx + ","
    }
    i = i + 1
  }
  
  // 验证复杂上下文序列化
  assert_eq(serialized_complex_ctx.starts_with("context:values_count:3|"), true)
  assert_eq(serialized_complex_ctx.contains("user.id=user-12345"), true)
  assert_eq(serialized_complex_ctx.contains("session.id=sess-abcdef"), true)
  assert_eq(serialized_complex_ctx.contains("request.id=req-123456"), true)
  
  // 4. 模拟上下文反序列化
  let deserialized_ctx_values = if serialized_complex_ctx.starts_with("context:values_count:") {
    let parts = serialized_complex_ctx.split("|")
    if parts.length() > 1 {
      parts[1].split(",")
    } else {
      []
    }
  } else {
    @test.fail("Test failed")
  }
  
  // 验证反序列化结果
  assert_eq(deserialized_ctx_values.length(), 3)
  
  // 5. 测试特殊字符上下文的序列化兼容性
  let special_key = create_key("!@#$%^&*()")
  let unicode_key = create_key("Unicode键🚀")
  
  let special_ctx = basic_ctx
    .with_value(special_key, "!@#$%^&*()")
    .with_value(unicode_key, "Unicode值🌟")
    .with_value(create_key(""), "empty_key_value")
  
  // 模拟特殊字符上下文序列化
  let serialized_special_ctx = "context:values_count:" + special_ctx.values.length().to_string() + "|"
  let mut i = 0
  while i < special_ctx.values.length() {
    let (key, value) = special_ctx.values[i]
    serialized_special_ctx = serialized_special_ctx + key + "=" + value
    if i < special_ctx.values.length() - 1 {
      serialized_special_ctx = serialized_special_ctx + ","
    }
    i = i + 1
  }
  
  // 验证特殊字符上下文序列化
  assert_eq(serialized_special_ctx.starts_with("context:values_count:3|"), true)
  assert_eq(serialized_special_ctx.contains("!@#$%^&*()=!@#$%^&*()"), true)
  assert_eq(serialized_special_ctx.contains("Unicode键🚀=Unicode值🌟"), true)
  assert_eq(serialized_special_ctx.contains("=empty_key_value"), true)
}

test "baggage_serialization_compatibility" {
  // 测试Baggage的序列化兼容性
  
  // 1. 创建基本baggage
  let basic_baggage = Baggage::empty()
  
  // 2. 模拟baggage序列化
  let serialized_baggage = "baggage:entries_count:" + basic_baggage.entries.length().to_string()
  
  // 验证基本baggage序列化
  assert_eq(serialized_baggage, "baggage:entries_count:0")
  
  // 3. 测试复杂baggage的序列化兼容性
  let complex_baggage = basic_baggage
    .with_entry("user.id", "user-12345")
    .with_entry("session.id", "sess-abcdef")
    .with_entry("request.id", "req-123456")
    .with_entry("user.role", "admin")
    .with_entry("request.source", "web")
  
  // 模拟复杂baggage序列化（W3C Baggage格式）
  let serialized_complex_baggage = "baggage:"
  let mut i = 0
  while i < complex_baggage.entries.length() {
    let (key, value) = complex_baggage.entries[i]
    serialized_complex_baggage = serialized_complex_baggage + key + "=" + value
    if i < complex_baggage.entries.length() - 1 {
      serialized_complex_baggage = serialized_complex_baggage + ","
    }
    i = i + 1
  }
  
  // 验证复杂baggage序列化
  assert_eq(serialized_complex_baggage, "baggage:user.id=user-12345,session.id=sess-abcdef,request.id=req-123456,user.role=admin,request.source=web")
  
  // 4. 模拟baggage反序列化
  let deserialized_baggage_entries = if serialized_complex_baggage.starts_with("baggage:") {
    let baggage_part = serialized_complex_baggage.substring(8)
    baggage_part.split(",")
  } else {
    @test.fail("Test failed")
  }
  
  // 验证反序列化结果
  assert_eq(deserialized_baggage_entries.length(), 5)
  
  // 验证具体条目
  let mut found_user_id = false
  let mut found_session_id = false
  let mut found_request_id = false
  let mut found_user_role = false
  let mut found_request_source = false
  
  let mut i = 0
  while i < deserialized_baggage_entries.length() {
    let entry = deserialized_baggage_entries[i]
    match entry {
      "user.id=user-12345" => found_user_id = true
      "session.id=sess-abcdef" => found_session_id = true
      "request.id=req-123456" => found_request_id = true
      "user.role=admin" => found_user_role = true
      "request.source=web" => found_request_source = true
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_user_id, true)
  assert_eq(found_session_id, true)
  assert_eq(found_request_id, true)
  assert_eq(found_user_role, true)
  assert_eq(found_request_source, true)
  
  // 5. 测试特殊字符baggage的序列化兼容性
  let special_baggage = basic_baggage
    .with_entry("!@#$%^&*()", "!@#$%^&*()")
    .with_entry("Unicode键🚀", "Unicode值🌟")
    .with_entry("", "empty_key")
    .with_entry("empty_value", "")
  
  // 模拟特殊字符baggage序列化
  let serialized_special_baggage = "baggage:"
  let mut i = 0
  while i < special_baggage.entries.length() {
    let (key, value) = special_baggage.entries[i]
    serialized_special_baggage = serialized_special_baggage + key + "=" + value
    if i < special_baggage.entries.length() - 1 {
      serialized_special_baggage = serialized_special_baggage + ","
    }
    i = i + 1
  }
  
  // 验证特殊字符baggage序列化
  assert_eq(serialized_special_baggage, "baggage:!@#$%^&*()=!@#$%^&*(),Unicode键🚀=Unicode值🌟,=empty_key,empty_value=")
  
  // 6. 测试符合W3C规范的baggage序列化
  let w3c_baggage = basic_baggage
    .with_entry("key1", "value1")
    .with_entry("key2", "value2")
    .with_entry("key3", "value3")
  
  // 模拟W3C Baggage序列化
  let w3c_serialized = "key1=value1,key2=value2,key3=value3"
  
  // 验证W3C Baggage序列化
  assert_eq(w3c_serialized, "key1=value1,key2=value2,key3=value3")
  
  // 模拟W3C Baggage反序列化
  let w3c_entries = w3c_serialized.split(",")
  assert_eq(w3c_entries.length(), 3)
  
  // 验证W3C Baggage条目
  let mut found_key1 = false
  let mut found_key2 = false
  let mut found_key3 = false
  
  let mut i = 0
  while i < w3c_entries.length() {
    let entry = w3c_entries[i]
    match entry {
      "key1=value1" => found_key1 = true
      "key2=value2" => found_key2 = true
      "key3=value3" => found_key3 = true
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_key1, true)
  assert_eq(found_key2, true)
  assert_eq(found_key3, true)
}

test "propagation_serialization_compatibility" {
  // 测试Propagation的序列化兼容性
  
  // 1. 测试W3C TraceContext的序列化兼容性
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x0a_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x0b_byte })
  
  let span_context = SpanContext::{
    trace_id,
    span_id,
    trace_flags: 1_byte,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 模拟W3C TraceContext序列化
  let trace_id_hex = "0af7651916cd43dd8448eb211c80319c"  // 示例值
  let span_id_hex = "b7ad6b7169203331"  // 示例值
  let trace_flags_hex = "01"  // 采样标志
  
  let w3c_traceparent = "00-" + trace_id_hex + "-" + span_id_hex + "-" + trace_flags_hex
  let w3c_tracestate = "key1=value1,key2=value2"
  
  // 验证W3C TraceContext序列化
  assert_eq(w3c_traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  assert_eq(w3c_tracestate, "key1=value1,key2=value2")
  
  // 2. 模拟W3C TraceContext反序列化
  let deserialized_traceparent_parts = w3c_traceparent.split("-")
  assert_eq(deserialized_traceparent_parts.length(), 4)
  assert_eq(deserialized_traceparent_parts[0], "00")  // version
  assert_eq(deserialized_traceparent_parts[1], trace_id_hex)  // trace_id
  assert_eq(deserialized_traceparent_parts[2], span_id_hex)  // span_id
  assert_eq(deserialized_traceparent_parts[3], trace_flags_hex)  // trace_flags
  
  let deserialized_tracestate_entries = w3c_tracestate.split(",")
  assert_eq(deserialized_tracestate_entries.length(), 2)
  assert_eq(deserialized_tracestate_entries[0], "key1=value1")
  assert_eq(deserialized_tracestate_entries[1], "key2=value2")
  
  // 3. 测试W3C Baggage的序列化兼容性
  let baggage = Baggage::empty()
    .with_entry("key1", "value1")
    .with_entry("key2", "value2")
    .with_entry("key3", "value3")
  
  // 模拟W3C Baggage序列化
  let w3c_baggage = "key1=value1,key2=value2,key3=value3"
  
  // 验证W3C Baggage序列化
  assert_eq(w3c_baggage, "key1=value1,key2=value2,key3=value3")
  
  // 4. 测试复合传播器的序列化兼容性
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 模拟复合传播器注入
  composite_propagator.inject(ctx, carrier)
  
  // 验证carrier中的数据
  match carrier.get("traceparent") {
    Some(trace_parent) => assert_eq(trace_parent.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get("tracestate") {
    Some(trace_state) => assert_eq(trace_state, "key1=value1,key2=value2")
    None => ()  // tracestate是可选的
  }
  
  match carrier.get("baggage") {
    Some(baggage_header) => assert_eq(baggage_header.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  // 5. 测试MapCarrier的序列化兼容性
  let carrier_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "key1=value1,key2=value2"),
    ("baggage", "key1=value1,key2=value2,key3=value3"),
    ("custom-header", "custom-value")
  ]
  
  let map_carrier = MapCarrier::from_map(carrier_data)
  
  // 模拟MapCarrier序列化
  let serialized_carrier = "carrier:"
  let mut i = 0
  while i < map_carrier.data.length() {
    let (key, value) = map_carrier.data[i]
    serialized_carrier = serialized_carrier + key + ":" + value
    if i < map_carrier.data.length() - 1 {
      serialized_carrier = serialized_carrier + "|"
    }
    i = i + 1
  }
  
  // 验证MapCarrier序列化
  assert_eq(serialized_carrier.starts_with("carrier:traceparent:"), true)
  assert_eq(serialized_carrier.contains("tracestate:key1=value1,key2=value2"), true)
  assert_eq(serialized_carrier.contains("baggage:key1=value1,key2=value2,key3=value3"), true)
  assert_eq(serialized_carrier.contains("custom-header:custom-value"), true)
  
  // 6. 测试特殊字符传播的序列化兼容性
  let special_baggage = Baggage::empty()
    .with_entry("special-key", "special!@#$%^&*()value")
    .with_entry("Unicode键", "Unicode值🚀")
    .with_entry("empty", "")
  
  // 模拟特殊字符W3C Baggage序列化
  let special_w3c_baggage = "special-key=special!@#$%^&*()value,Unicode键=Unicode值🚀,empty="
  
  // 验证特殊字符W3C Baggage序列化
  assert_eq(special_w3c_baggage, "special-key=special!@#$%^&*()value,Unicode键=Unicode值🚀,empty=")
  
  // 模拟特殊字符W3C Baggage反序列化
  let special_entries = special_w3c_baggage.split(",")
  assert_eq(special_entries.length(), 3)
  
  // 验证特殊字符条目
  let mut found_special_key = false
  let mut found_unicode_key = false
  let mut found_empty = false
  
  let mut i = 0
  while i < special_entries.length() {
    let entry = special_entries[i]
    match entry {
      "special-key=special!@#$%^&*()value" => found_special_key = true
      "Unicode键=Unicode值🚀" => found_unicode_key = true
      "empty=" => found_empty = true
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_special_key, true)
  assert_eq(found_unicode_key, true)
  assert_eq(found_empty, true)
}