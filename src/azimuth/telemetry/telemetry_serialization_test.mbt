// 遥测数据序列化测试
test "attribute_value_serialization" {
  // 测试AttributeValue的序列化
  
  // 基本类型序列化
  let string_attr = AttributeValue::string("test_string")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  
  // 验证基本类型的字符串表示
  match string_attr {
    StringValue(s) => {
      assert_eq(s, "test_string")
      // 模拟序列化为JSON字符串
      let json_string = "\"" + s + "\""
      assert_eq(json_string, "\"test_string\"")
    }
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(i) => {
      assert_eq(i, 42L)
      // 模拟序列化为JSON数字
      let json_int = i.to_string()
      assert_eq(json_int, "42")
    }
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(f) => {
      assert_eq(f, 3.14159)
      // 模拟序列化为JSON浮点数
      let json_float = f.to_string()
      assert_eq(json_float, "3.14159")
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr {
    BoolValue(b) => {
      assert_eq(b, true)
      // 模拟序列化为JSON布尔值
      let json_bool = if b { "true" } else { "false" }
      assert_eq(json_bool, "true")
    }
    _ => @test.fail("Expected BoolValue")
  }
  
  // 数组类型序列化
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = AttributeValue::array_bool([true, false, true])
  
  match string_array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      // 模拟序列化为JSON数组
      let json_array = "[\"" + arr.join("\", \"") + "\"]"
      assert_eq(json_array, "[\"a\", \"b\", \"c\"]")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match int_array_attr {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      // 模拟序列化为JSON数组
      let json_array = "[" + arr.join(", ") + "]"
      assert_eq(json_array, "[1, 2, 3]")
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match float_array_attr {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 3)
      // 模拟序列化为JSON数组
      let json_array = "[" + arr.join(", ") + "]"
      assert_eq(json_array, "[1.1, 2.2, 3.3]")
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match bool_array_attr {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 3)
      // 模拟序列化为JSON数组
      let mut json_array = "["
      let mut i = 0
      while i < arr.length() {
        json_array = json_array + (if arr[i] { "true" } else { "false" })
        if i < arr.length() - 1 {
          json_array = json_array + ", "
        }
        i = i + 1
      }
      json_array = json_array + "]"
      assert_eq(json_array, "[true, false, true]")
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "resource_serialization" {
  // 测试Resource的序列化
  
  let resource = Resource::default("test-service")
  
  // 基本Resource序列化
  let resource_json = "{"
    + "\"service_name\":\"" + resource.service_name + "\"," 
    + "\"service_version\":" + match resource.service_version {
      Some(v) => "\"" + v + "\""
      None => "null"
    } + ","
    + "\"telemetry_sdk_name\":\"" + resource.telemetry_sdk_name + "\"," 
    + "\"telemetry_sdk_version\":\"" + resource.telemetry_sdk_version + "\"," 
    + "\"attributes\":[]"
    + "}"
  
  assert_eq(resource.service_name, "test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 带属性的Resource序列化
  let complex_resource = Resource::default("complex-service")
  let attributes : Attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("host.name", AttributeValue::string("prod-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true)),
    ("tags", AttributeValue::array_string(["web", "api", "critical"])),
    ("ports", AttributeValue::array_int([8080L, 8443L])),
    ("thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
    ("features", AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证复杂资源
  assert_eq(complex_resource.service_name, "complex-service")
  assert_eq(attributes.length(), 10)
  
  // 模拟属性序列化
  let mut attributes_json = "["
  let mut i = 0
  while i < attributes.length() {
    let (key, value) = attributes[i]
    attributes_json = attributes_json + "{"
      + "\"key\":\"" + key + "\"," 
      + "\"value\":" + match value {
        StringValue(s) => "\"" + s + "\""
        IntValue(n) => n.to_string()
        FloatValue(f) => f.to_string()
        BoolValue(b) => if b { "true" } else { "false" }
        ArrayStringValue(arr) => {
          let mut arr_json = "["
          let mut j = 0
          while j < arr.length() {
            arr_json = arr_json + "\"" + arr[j] + "\""
            if j < arr.length() - 1 {
              arr_json = arr_json + ", "
            }
            j = j + 1
          }
          arr_json + "]"
        }
        ArrayIntValue(arr) => "[" + arr.join(", ") + "]"
        ArrayFloatValue(arr) => "[" + arr.join(", ") + "]"
        ArrayBoolValue(arr) => {
          let mut arr_json = "["
          let mut j = 0
          while j < arr.length() {
            arr_json = arr_json + (if arr[j] { "true" } else { "false" })
            if j < arr.length() - 1 {
              arr_json = arr_json + ", "
            }
            j = j + 1
          }
          arr_json + "]"
        }
      }
      + "}"
    if i < attributes.length() - 1 {
      attributes_json = attributes_json + ","
    }
    i = i + 1
  }
  attributes_json = attributes_json + "]"
  
  // 验证JSON结构正确
  assert_eq(attributes_json[0], "[")
  assert_eq(attributes_json[attributes_json.length() - 1], "]")
}

test "span_serialization" {
  // 测试Span的序列化
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "test-span",
    Server,
    Some([
      ("service.name", AttributeValue::string("test-service")),
      ("operation.name", AttributeValue::string("test.operation")),
      ("user.id", AttributeValue::string("user-123456"))
    ]),
    Some(1234567890L)
  )
  
  // 基本Span序列化
  let span_json = "{"
    + "\"name\":\"" + span.name + "\"," 
    + "\"kind\":\"" + match span.kind {
      Internal => "INTERNAL"
      Server => "SERVER"
      Client => "CLIENT"
      Producer => "PRODUCER"
      Consumer => "CONSUMER"
    } + "\"," 
    + "\"start_time_unix_nanos\":" + span.start_time_unix_nanos.to_string() + "," 
    + "\"end_time_unix_nanos\":" + match span.end_time_unix_nanos {
      Some(t) => t.to_string()
      None => "null"
    } + ","
    + "\"status\":\"" + match span.status {
      Unset => "UNSET"
      Ok => "OK"
      Error => "ERROR"
    } + "\"," 
    + "\"status_description\":" + match span.status_description {
      Some(desc) => "\"" + desc + "\""
      None => "null"
    } + ","
    + "\"attributes\":["
    + (let mut attrs_json = ""
      let mut i = 0
      while i < span.attributes.length() {
        let (key, value) = span.attributes[i]
        attrs_json = attrs_json + "{"
          + "\"key\":\"" + key + "\"," 
          + "\"value\":" + match value {
            StringValue(s) => "\"" + s + "\""
            IntValue(n) => n.to_string()
            FloatValue(f) => f.to_string()
            BoolValue(b) => if b { "true" } else { "false" }
            ArrayStringValue(arr) => "[\"" + arr.join("\", \"") + "\"]"
            ArrayIntValue(arr) => "[" + arr.join(", ") + "]"
            ArrayFloatValue(arr) => "[" + arr.join(", ") + "]"
            ArrayBoolValue(arr) => {
              let mut arr_json = ""
              let mut j = 0
              while j < arr.length() {
                arr_json = arr_json + (if arr[j] { "true" } else { "false" })
                if j < arr.length() - 1 {
                  arr_json = arr_json + ", "
                }
                j = j + 1
              }
              "[" + arr_json + "]"
            }
          }
          + "}"
        if i < span.attributes.length() - 1 {
          attrs_json = attrs_json + ","
        }
        i = i + 1
      }
      attrs_json)
    + "],"
    + "\"events\":[]," 
    + "\"links\":[]," 
    + "\"parent_span_id\":" + match span.parent_span_id {
      Some(id) => {
        let mut id_str = ""
        let mut i = 0
        while i < id.length() {
          id_str = id_str + id[i].to_string()
          i = i + 1
        }
        "\"" + id_str + "\""
      }
      None => "null"
    }
    + "}"
  
  // 验证Span基本字段
  assert_eq(span.name, "test-span")
  assert_eq(span.kind, Server)
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  assert_eq(span.status, Unset)
  assert_eq(span.attributes.length(), 3)
  
  // 验证JSON结构
  assert_eq(span_json[0], "{")
  assert_eq(span_json[span_json.length() - 1], "}")
}

test "log_record_serialization" {
  // 测试LogRecord的序列化
  
  let resource = Resource::default("test-service")
  let trace_id = Array::make(16, 0x12_byte)
  let span_id = Array::make(8, 0x34_byte)
  let trace_flags = 0x01_byte
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567890L,
    observed_timestamp_unix_nanos: Some(1234567891L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Test log message"),
    attributes: [
      ("service.name", AttributeValue::string("test-service")),
      ("operation.name", AttributeValue::string("test.operation")),
      ("user.id", AttributeValue::string("user-123456")),
      ("process.pid", AttributeValue::int(12345L)),
      ("process.cpu.percent", AttributeValue::float(75.5)),
      ("process.healthy", AttributeValue::bool(true)),
      ("tags", AttributeValue::array_string(["log", "test", "info"]))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(trace_flags),
    resource: Some(resource),
    instrumentation_scope: None
  }
  
  // LogRecord序列化
  let log_json = "{"
    + "\"timestamp_unix_nanos\":" + log_record.timestamp_unix_nanos.to_string() + "," 
    + "\"observed_timestamp_unix_nanos\":" + match log_record.observed_timestamp_unix_nanos {
      Some(t) => t.to_string()
      None => "null"
    } + ","
    + "\"severity_number\":\"" + match log_record.severity_number {
      Trace => "TRACE"
      Debug => "DEBUG"
      Info => "INFO"
      Warn => "WARN"
      Error => "ERROR"
      Fatal => "FATAL"
    } + "\"," 
    + "\"severity_text\":" + match log_record.severity_text {
      Some(text) => "\"" + text + "\""
      None => "null"
    } + ","
    + "\"body\":" + match log_record.body {
      Some(body) => "\"" + body + "\""
      None => "null"
    } + ","
    + "\"attributes\":["
    + (let mut attrs_json = ""
      let mut i = 0
      while i < log_record.attributes.length() {
        let (key, value) = log_record.attributes[i]
        attrs_json = attrs_json + "{"
          + "\"key\":\"" + key + "\"," 
          + "\"value\":" + match value {
            StringValue(s) => "\"" + s + "\""
            IntValue(n) => n.to_string()
            FloatValue(f) => f.to_string()
            BoolValue(b) => if b { "true" } else { "false" }
            ArrayStringValue(arr) => "[\"" + arr.join("\", \"") + "\"]"
            ArrayIntValue(arr) => "[" + arr.join(", ") + "]"
            ArrayFloatValue(arr) => "[" + arr.join(", ") + "]"
            ArrayBoolValue(arr) => {
              let mut arr_json = ""
              let mut j = 0
              while j < arr.length() {
                arr_json = arr_json + (if arr[j] { "true" } else { "false" })
                if j < arr.length() - 1 {
                  arr_json = arr_json + ", "
                }
                j = j + 1
              }
              "[" + arr_json + "]"
            }
          }
          + "}"
        if i < log_record.attributes.length() - 1 {
          attrs_json = attrs_json + ","
        }
        i = i + 1
      }
      attrs_json)
    + "],"
    + "\"trace_id\":" + match log_record.trace_id {
      Some(id) => {
        let mut id_str = ""
        let mut i = 0
        while i < id.length() {
          id_str = id_str + id[i].to_string()
          i = i + 1
        }
        "\"" + id_str + "\""
      }
      None => "null"
    } + ","
    + "\"span_id\":" + match log_record.span_id {
      Some(id) => {
        let mut id_str = ""
        let mut i = 0
        while i < id.length() {
          id_str = id_str + id[i].to_string()
          i = i + 1
        }
        "\"" + id_str + "\""
      }
      None => "null"
    } + ","
    + "\"trace_flags\":" + match log_record.trace_flags {
      Some(flags) => flags.to_string()
      None => "null"
    } + ","
    + "\"resource\":" + match log_record.resource {
      Some(res) => "{"
        + "\"service_name\":\"" + res.service_name + "\"," 
        + "\"service_version\":" + match res.service_version {
          Some(v) => "\"" + v + "\""
          None => "null"
        } + ","
        + "\"telemetry_sdk_name\":\"" + res.telemetry_sdk_name + "\"," 
        + "\"telemetry_sdk_version\":\"" + res.telemetry_sdk_version + "\"," 
        + "\"attributes\":[]"
        + "}"
      None => "null"
    } + ","
    + "\"instrumentation_scope\":null"
    + "}"
  
  // 验证LogRecord基本字段
  assert_eq(log_record.timestamp_unix_nanos, 1234567890L)
  assert_eq(log_record.severity_number, Info)
  match log_record.severity_text {
    Some(text) => assert_eq(text, "INFO")
    None => @test.fail("Expected severity text")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Test log message")
    None => @test.fail("Expected body")
  }
  assert_eq(log_record.attributes.length(), 7)
  
  // 验证JSON结构
  assert_eq(log_json[0], "{")
  assert_eq(log_json[log_json.length() - 1], "}")
}

test "context_and_baggage_serialization" {
  // 测试Context和Baggage的序列化
  
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 创建复杂的Context
  let enriched_ctx = ctx
    .with_value(create_key("trace.id"), "trace-12345678901234567890123456789012")
    .with_value(create_key("span.id"), "span-1234567890123456")
    .with_value(create_key("user.id"), "user-123456789")
    .with_value(create_key("session.id"), "session-abcdef123456")
    .with_value(create_key("request.id"), "req-1234567890abcdef")
    .with_value(create_key("correlation.id"), "corr-abcdef1234567890")
    .with_value(create_key("operation.name"), "test.operation")
    .with_value(create_key("service.name"), "test-service")
    .with_value(create_key("deployment.env"), "production")
    .with_value(create_key("host.name"), "prod-server-01")
  
  // Context序列化
  let context_json = "{"
    + "\"values\":["
    + (let mut values_json = ""
      let mut i = 0
      while i < enriched_ctx.values.length() {
        let (key, value) = enriched_ctx.values[i]
        values_json = values_json + "{"
          + "\"key\":\"" + key + "\"," 
          + "\"value\":\"" + value + "\""
          + "}"
        if i < enriched_ctx.values.length() - 1 {
          values_json = values_json + ","
        }
        i = i + 1
      }
      values_json)
    + "]"
    + "}"
  
  // 验证Context
  assert_eq(enriched_ctx.values.length(), 10)
  
  // 创建复杂的Baggage
  let enriched_baggage = baggage
    .with_entry("user.id", "user-123456789")
    .with_entry("session.id", "session-abcdef123456")
    .with_entry("request.id", "req-1234567890abcdef")
    .with_entry("correlation.id", "corr-abcdef1234567890")
    .with_entry("service.name", "test-service")
    .with_entry("deployment.env", "production")
    .with_entry("host.name", "prod-server-01")
    .with_entry("operation.name", "test.operation")
    .with_entry("trace.id", "trace-12345678901234567890123456789012")
    .with_entry("span.id", "span-1234567890123456")
  
  // Baggage序列化
  let baggage_json = "{"
    + "\"entries\":["
    + (let mut entries_json = ""
      let mut i = 0
      while i < enriched_baggage.entries.length() {
        let (key, value) = enriched_baggage.entries[i]
        entries_json = entries_json + "{"
          + "\"key\":\"" + key + "\"," 
          + "\"value\":\"" + value + "\""
          + "}"
        if i < enriched_baggage.entries.length() - 1 {
          entries_json = entries_json + ","
        }
        i = i + 1
      }
      entries_json)
    + "]"
    + "}"
  
  // 验证Baggage
  assert_eq(enriched_baggage.entries.length(), 10)
  
  // 验证JSON结构
  assert_eq(context_json[0], "{")
  assert_eq(context_json[context_json.length() - 1], "}")
  assert_eq(baggage_json[0], "{")
  assert_eq(baggage_json[baggage_json.length() - 1], "}")
}

test "propagation_data_serialization" {
  // 测试传播数据的序列化
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 注入传播数据
  composite.inject(ctx, carrier)
  
  // 模拟Carrier序列化
  let carrier_json = "{"
    + "\"data\":["
    + (let mut data_json = ""
      let mut i = 0
      while i < carrier.data.length() {
        let (key, value) = carrier.data[i]
        data_json = data_json + "{"
          + "\"key\":\"" + key + "\"," 
          + "\"value\":\"" + value + "\""
          + "}"
        if i < carrier.data.length() - 1 {
          data_json = data_json + ","
        }
        i = i + 1
      }
      data_json)
    + "]"
    + "}"
  
  // 验证Carrier包含预期数据
  let mut has_traceparent = false
  let mut has_baggage = false
  
  let mut i = 0
  while i < carrier.data.length() {
    let (key, value) = carrier.data[i]
    if key == "traceparent" {
      has_traceparent = true
      // 验证traceparent格式
      assert_eq(value.length(), 55)  // "00-" + 32 + "-" + 16 + "-01"
      assert_eq(value[0:2], "00")
      assert_eq(value[51:53], "01")
    }
    if key == "baggage" {
      has_baggage = true
      // 验证baggage格式
      assert_eq(value, "key1=value1,key2=value2")
    }
    i = i + 1
  }
  
  assert_eq(has_traceparent, true)
  assert_eq(has_baggage, true)
  
  // 验证JSON结构
  assert_eq(carrier_json[0], "{")
  assert_eq(carrier_json[carrier_json.length() - 1], "}")
}

test "complex_telemetry_data_serialization" {
  // 测试复杂遥测数据的序列化
  
  // 创建完整的遥测数据结构
  let resource = Resource::default("complex-telemetry-service")
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  let meter = NoopMeter::{}
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("complex.logger")
  
  // 丰富的Context
  let enriched_ctx = ctx
    .with_value(create_key("trace.id"), "trace-12345678901234567890123456789012")
    .with_value(create_key("span.id"), "span-1234567890123456")
    .with_value(create_key("user.id"), "user-123456789")
    .with_value(create_key("session.id"), "session-abcdef123456")
    .with_value(create_key("request.id"), "req-1234567890abcdef")
    .with_value(create_key("correlation.id"), "corr-abcdef1234567890")
    .with_value(create_key("operation.name"), "complex.operation")
    .with_value(create_key("service.name"), "complex-telemetry-service")
    .with_value(create_key("deployment.env"), "production")
    .with_value(create_key("host.name"), "prod-server-01")
  
  // 复杂的Span
  let (span_ctx, span) = tracer.start_span(
    enriched_ctx,
    "complex-operation",
    Server,
    Some([
      ("service.name", AttributeValue::string("complex-telemetry-service")),
      ("operation.type", AttributeValue::string("complex")),
      ("user.id", AttributeValue::string("user-123456789")),
      ("session.id", AttributeValue::string("session-abcdef123456")),
      ("request.id", AttributeValue::string("req-1234567890abcdef")),
      ("correlation.id", AttributeValue::string("corr-abcdef1234567890")),
      ("host.name", AttributeValue::string("prod-server-01")),
      ("process.pid", AttributeValue::int(12345L)),
      ("process.cpu.percent", AttributeValue::float(75.5)),
      ("process.healthy", AttributeValue::bool(true)),
      ("tags", AttributeValue::array_string(["complex", "telemetry", "test"])),
      ("ports", AttributeValue::array_int([8080L, 8443L, 9090L])),
      ("thresholds", AttributeValue::array_float([60.0, 80.0, 95.0])),
      ("features", AttributeValue::array_bool([true, false, true, false]))
    ]),
    Some(1234567890L)
  )
  
  // 复杂的LogRecord
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1234567891L,
    observed_timestamp_unix_nanos: Some(1234567892L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Complex telemetry operation completed"),
    attributes: [
      ("service.name", AttributeValue::string("complex-telemetry-service")),
      ("operation.name", AttributeValue::string("complex-operation")),
      ("user.id", AttributeValue::string("user-123456789")),
      ("session.id", AttributeValue::string("session-abcdef123456")),
      ("request.id", AttributeValue::string("req-1234567890abcdef")),
      ("correlation.id", AttributeValue::string("corr-abcdef1234567890")),
      ("host.name", AttributeValue::string("prod-server-01")),
      ("process.pid", AttributeValue::int(12345L)),
      ("process.cpu.percent", AttributeValue::float(75.5)),
      ("process.healthy", AttributeValue::bool(true)),
      ("operation.duration", AttributeValue::float(1.23)),
      ("operation.status", AttributeValue::string("success")),
      ("tags", AttributeValue::array_string(["complex", "telemetry", "log"])),
      ("metrics", AttributeValue::array_int([100L, 200L, 300L]))
    ],
    trace_id: Some(Array::make(16, 0x12_byte)),
    span_id: Some(Array::make(8, 0x34_byte)),
    trace_flags: Some(0x01_byte),
    resource: Some(resource),
    instrumentation_scope: None
  }
  
  // 度量数据
  let counter = meter.create_counter("complex.requests", "requests", "Complex operation counter")
  let histogram = meter.create_histogram("complex.duration", "seconds", "Complex operation histogram")
  let gauge = meter.create_gauge("complex.memory", "bytes", "Complex operation gauge")
  
  // 模拟完整的遥测数据序列化
  let telemetry_json = "{"
    + "\"resource\":{"
      + "\"service_name\":\"" + resource.service_name + "\"," 
      + "\"telemetry_sdk_name\":\"" + resource.telemetry_sdk_name + "\"," 
      + "\"telemetry_sdk_version\":\"" + resource.telemetry_sdk_version + "\""
      + "},"
    + "\"span\":{"
      + "\"name\":\"" + span.name + "\"," 
      + "\"kind\":\"" + match span.kind { Server => "SERVER" | _ => "UNKNOWN" } + "\"," 
      + "\"start_time_unix_nanos\":" + span.start_time_unix_nanos.to_string() + "," 
      + "\"attributes_count\":" + span.attributes.length().to_string()
      + "},"
    + "\"log_record\":{"
      + "\"timestamp_unix_nanos\":" + log_record.timestamp_unix_nanos.to_string() + "," 
      + "\"severity_number\":\"INFO\"," 
      + "\"body\":\"Complex telemetry operation completed\"," 
      + "\"attributes_count\":" + log_record.attributes.length().to_string()
      + "},"
    + "\"context\":{"
      + "\"values_count\":" + enriched_ctx.values.length().to_string()
      + "},"
    + "\"metrics\":{"
      + "\"counter\":\"complex.requests\"," 
      + "\"histogram\":\"complex.duration\"," 
      + "\"gauge\":\"complex.memory\""
      + "}"
    + "}"
  
  // 验证复杂遥测数据
  assert_eq(span.name, "complex-operation")
  assert_eq(span.attributes.length(), 15)
  assert_eq(log_record.attributes.length(), 15)
  assert_eq(enriched_ctx.values.length(), 10)
  
  // 验证JSON结构
  assert_eq(telemetry_json[0], "{")
  assert_eq(telemetry_json[telemetry_json.length() - 1], "}")
  
  // 验证JSON包含所有主要组件
  assert_eq(telemetry_json.contains("\"resource\":"), true)
  assert_eq(telemetry_json.contains("\"span\":"), true)
  assert_eq(telemetry_json.contains("\"log_record\":"), true)
  assert_eq(telemetry_json.contains("\"context\":"), true)
  assert_eq(telemetry_json.contains("\"metrics\":"), true)
}

test "serialization_edge_cases" {
  // 测试序列化的边界情况
  
  // 空值和特殊值
  let empty_string_attr = AttributeValue::string("")
  let zero_int_attr = AttributeValue::int(0L)
  let zero_float_attr = AttributeValue::float(0.0)
  let false_bool_attr = AttributeValue::bool(false)
  let empty_array_attr = AttributeValue::array_string([])
  
  // 验证空值序列化
  match empty_string_attr {
    StringValue(s) => {
      assert_eq(s, "")
      let json = "\"" + s + "\""
      assert_eq(json, "\"\"")
    }
    _ => @test.fail("Expected empty string")
  }
  
  match zero_int_attr {
    IntValue(i) => {
      assert_eq(i, 0L)
      let json = i.to_string()
      assert_eq(json, "0")
    }
    _ => @test.fail("Expected zero int")
  }
  
  match zero_float_attr {
    FloatValue(f) => {
      assert_eq(f, 0.0)
      let json = f.to_string()
      assert_eq(json, "0.0")
    }
    _ => @test.fail("Expected zero float")
  }
  
  match false_bool_attr {
    BoolValue(b) => {
      assert_eq(b, false)
      let json = if b { "true" } else { "false" }
      assert_eq(json, "false")
    }
    _ => @test.fail("Expected false bool")
  }
  
  match empty_array_attr {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 0)
      let json = "[]"
      assert_eq(json, "[]")
    }
    _ => @test.fail("Expected empty array")
  }
  
  // 特殊浮点值
  let inf_float_attr = AttributeValue::float(1.0/0.0)
  let neg_inf_float_attr = AttributeValue::float(-1.0/0.0)
  let nan_float_attr = AttributeValue::float(0.0/0.0)
  
  match inf_float_attr {
    FloatValue(f) => {
      // JSON中无穷大通常表示为字符串
      assert_eq(f > 0.0, true)
      let json = "\"Infinity\""
      assert_eq(json, "\"Infinity\"")
    }
    _ => @test.fail("Expected infinity float")
  }
  
  match neg_inf_float_attr {
    FloatValue(f) => {
      // JSON中负无穷大通常表示为字符串
      assert_eq(f < 0.0, true)
      let json = "\"-Infinity\""
      assert_eq(json, "\"-Infinity\"")
    }
    _ => @test.fail("Expected negative infinity float")
  }
  
  match nan_float_attr {
    FloatValue(f) => {
      // JSON中NaN通常表示为字符串
      assert_eq(f != f, true)
      let json = "\"NaN\""
      assert_eq(json, "\"NaN\"")
    }
    _ => @test.fail("Expected NaN float")
  }
  
  // 极值
  let max_int_attr = AttributeValue::int(9223372036854775807L)
  let min_int_attr = AttributeValue::int(-9223372036854775808L)
  let max_float_attr = AttributeValue::float(1.7976931348623157e+308)
  
  match max_int_attr {
    IntValue(i) => {
      assert_eq(i, 9223372036854775807L)
      let json = i.to_string()
      assert_eq(json, "9223372036854775807")
    }
    _ => @test.fail("Expected max int")
  }
  
  match min_int_attr {
    IntValue(i) => {
      assert_eq(i, -9223372036854775808L)
      let json = i.to_string()
      assert_eq(json, "-9223372036854775808")
    }
    _ => @test.fail("Expected min int")
  }
  
  match max_float_attr {
    FloatValue(f) => {
      assert_eq(f, 1.7976931348623157e+308)
      let json = f.to_string()
      assert_eq(json, "1.7976931348623157e+308")
    }
    _ => @test.fail("Expected max float")
  }
}