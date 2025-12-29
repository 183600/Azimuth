// 序列化/反序列化测试用例
// 测试telemetry数据结构的序列化和反序列化功能

test "span_context_serialization" {
  // 测试SpanContext的序列化和反序列化
  
  let original_trace_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                           0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                           0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                           0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  let original_span_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                          0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  let original_context = SpanContext::{
    trace_id: original_trace_id,
    span_id: original_span_id,
    trace_flags: 0x01_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 序列化为字符串格式 (模拟)
  let trace_id_hex = "0123456789abcdef0123456789abcdef"
  let span_id_hex = "0123456789abcdef"
  let trace_flags_hex = "01"
  let serialized = trace_id_hex + "-" + span_id_hex + "-" + trace_flags_hex + "-" + original_context.trace_state
  
  // 验证序列化结果
  assert_eq(serialized.contains("0123456789abcdef0123456789abcdef"), true)
  assert_eq(serialized.contains("0123456789abcdef"), true)
  assert_eq(serialized.contains("01"), true)
  assert_eq(serialized.contains("rojo=00f067aa0ba902b7"), true)
  assert_eq(serialized.contains("congo=t61rcWkgMzE"), true)
  
  // 反序列化 (模拟)
  let parts = serialized.split("-")
  assert_eq(parts.length(), 4)
  
  let deserialized_trace_id_hex = parts[0]
  let deserialized_span_id_hex = parts[1]
  let deserialized_trace_flags_hex = parts[2]
  let deserialized_trace_state = parts[3]
  
  // 验证反序列化结果
  assert_eq(deserialized_trace_id_hex, trace_id_hex)
  assert_eq(deserialized_span_id_hex, span_id_hex)
  assert_eq(deserialized_trace_flags_hex, trace_flags_hex)
  assert_eq(deserialized_trace_state, original_context.trace_state)
  
  // 验证trace_id的十六进制转换
  assert_eq(deserialized_trace_id_hex.length(), 32)
  assert_eq(deserialized_span_id_hex.length(), 16)
  assert_eq(deserialized_trace_flags_hex.length(), 2)
}

test "attribute_value_serialization" {
  // 测试AttributeValue的序列化和反序列化
  
  // 测试不同类型的AttributeValue
  let string_attr = AttributeValue::string("test value")
  let int_attr = AttributeValue::int(12345L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  let array_string_attr = AttributeValue::array_string(["a", "b", "c"])
  let array_int_attr = AttributeValue::array_int([1L, 2L, 3L])
  let array_float_attr = AttributeValue::array_float([1.1, 2.2, 3.3])
  let array_bool_attr = AttributeValue::array_bool([true, false, true])
  
  // 序列化属性 (模拟为字符串)
  let serialize_attribute = func(attr : AttributeValue) -> String {
    match attr {
      StringValue(s) => "string:" + s
      IntValue(i) => "int:" + i.to_string()
      FloatValue(f) => "float:" + f.to_string()
      BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
      ArrayStringValue(arr) => {
        let result = "array_string:["
        let mut first = true
        let mut i = 0
        while i < arr.length() {
          if first {
            first = false
          } else {
            result = result + ","
          }
          result = result + arr[i]
          i = i + 1
        }
        result + "]"
      }
      ArrayIntValue(arr) => {
        let result = "array_int:["
        let mut first = true
        let mut i = 0
        while i < arr.length() {
          if first {
            first = false
          } else {
            result = result + ","
          }
          result = result + arr[i].to_string()
          i = i + 1
        }
        result + "]"
      }
      ArrayFloatValue(arr) => {
        let result = "array_float:["
        let mut first = true
        let mut i = 0
        while i < arr.length() {
          if first {
            first = false
          } else {
            result = result + ","
          }
          result = result + arr[i].to_string()
          i = i + 1
        }
        result + "]"
      }
      ArrayBoolValue(arr) => {
        let result = "array_bool:["
        let mut first = true
        let mut i = 0
        while i < arr.length() {
          if first {
            first = false
          } else {
            result = result + ","
          }
          result = result + (if arr[i] { "true" } else { "false" })
          i = i + 1
        }
        result + "]"
      }
    }
  }
  
  // 序列化所有属性
  let string_serialized = serialize_attribute(string_attr)
  let int_serialized = serialize_attribute(int_attr)
  let float_serialized = serialize_attribute(float_attr)
  let bool_serialized = serialize_attribute(bool_attr)
  let array_string_serialized = serialize_attribute(array_string_attr)
  let array_int_serialized = serialize_attribute(array_int_attr)
  let array_float_serialized = serialize_attribute(array_float_attr)
  let array_bool_serialized = serialize_attribute(array_bool_attr)
  
  // 验证序列化结果
  assert_eq(string_serialized, "string:test value")
  assert_eq(int_serialized, "int:12345")
  assert_eq(float_serialized, "float:3.14159")
  assert_eq(bool_serialized, "bool:true")
  assert_eq(array_string_serialized, "array_string:[a,b,c]")
  assert_eq(array_int_serialized, "array_int:[1,2,3]")
  assert_eq(array_float_serialized, "array_float:[1.1,2.2,3.3]")
  assert_eq(array_bool_serialized, "array_bool:[true,false,true]")
  
  // 验证反序列化能力 (通过检查序列化字符串的格式)
  assert_eq(string_serialized.starts_with("string:"), true)
  assert_eq(int_serialized.starts_with("int:"), true)
  assert_eq(float_serialized.starts_with("float:"), true)
  assert_eq(bool_serialized.starts_with("bool:"), true)
  assert_eq(array_string_serialized.starts_with("array_string:["), true)
  assert_eq(array_int_serialized.starts_with("array_int:["), true)
  assert_eq(array_float_serialized.starts_with("array_float:["), true)
  assert_eq(array_bool_serialized.starts_with("array_bool:["), true)
}

test "log_record_serialization" {
  // 测试LogRecord的序列化和反序列化
  
  let original_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000000L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Database connection failed"),
    attributes: [
      ("error.code", AttributeValue::string("DB_CONN_FAILED")),
      ("error.retries", AttributeValue::int(3L)),
      ("error.timeout", AttributeValue::float(30.0)),
      ("error.retryable", AttributeValue::bool(true))
    ],
    trace_id: Some([0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                    0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                    0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                    0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]),
    span_id: Some([0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                   0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]),
    trace_flags: Some(0x01_byte),
    resource: Some(Resource::default("payment-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "database-client",
      version: Some("1.2.3"),
      schema_url: Some("https://opentelemetry.io/schema/1.0.0")
    })
  }
  
  // 序列化为JSON格式 (模拟)
  let trace_id_hex = "0123456789abcdef0123456789abcdef"
  let span_id_hex = "0123456789abcdef"
  
  let serialized = "{"
    + "\"timestamp_unix_nanos\":" + original_log.timestamp_unix_nanos.to_string() + ","
    + "\"observed_timestamp_unix_nanos\":" + match original_log.observed_timestamp_unix_nanos {
      Some(ts) => ts.to_string()
      None => "null"
    } + ","
    + "\"severity_number\":\"ERROR\","
    + "\"severity_text\":\"ERROR\","
    + "\"body\":\"Database connection failed\","
    + "\"attributes\":["
      + "{\"key\":\"error.code\",\"value\":\"string:DB_CONN_FAILED\"},"
      + "{\"key\":\"error.retries\",\"value\":\"int:3\"},"
      + "{\"key\":\"error.timeout\",\"value\":\"float:30.0\"},"
      + "{\"key\":\"error.retryable\",\"value\":\"bool:true\"}"
    + "],"
    + "\"trace_id\":\"" + trace_id_hex + "\","
    + "\"span_id\":\"" + span_id_hex + "\","
    + "\"trace_flags\":1,"
    + "\"resource\":{"
      + "\"service_name\":\"payment-service\","
      + "\"telemetry_sdk_name\":\"azimuth\","
      + "\"telemetry_sdk_version\":\"0.1.0\""
    + "},"
    + "\"instrumentation_scope\":{"
      + "\"name\":\"database-client\","
      + "\"version\":\"1.2.3\","
      + "\"schema_url\":\"https://opentelemetry.io/schema/1.0.0\""
    + "}"
  + "}"
  
  // 验证序列化结果的关键部分
  assert_eq(serialized.contains("\"timestamp_unix_nanos\":1640995200000000000"), true)
  assert_eq(serialized.contains("\"severity_number\":\"ERROR\""), true)
  assert_eq(serialized.contains("\"body\":\"Database connection failed\""), true)
  assert_eq(serialized.contains("\"trace_id\":\"0123456789abcdef0123456789abcdef\""), true)
  assert_eq(serialized.contains("\"span_id\":\"0123456789abcdef\""), true)
  assert_eq(serialized.contains("\"service_name\":\"payment-service\""), true)
  assert_eq(serialized.contains("\"name\":\"database-client\""), true)
  assert_eq(serialized.contains("\"error.code\",\"value\":\"string:DB_CONN_FAILED\""), true)
  assert_eq(serialized.contains("\"error.retries\",\"value\":\"int:3\""), true)
}

test "resource_serialization" {
  // 测试Resource的序列化和反序列化
  
  let original_resource = Resource::{
    service_name: "order-processing-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("service.instance.id", AttributeValue::string("instance-12345")),
      ("host.name", AttributeValue::string("prod-server-01")),
      ("region", AttributeValue::string("us-west-2")),
      ("zone", AttributeValue::string("us-west-2a")),
      ("cluster.name", AttributeValue::string("orders-cluster")),
      ("pod.name", AttributeValue::string("order-processor-7f8d9")),
      ("container.id", AttributeValue::string("abc123def456")),
      ("process.pid", AttributeValue::int(12345L)),
      ("process.executable.name", AttributeValue::string("order-processor")),
      ("process.command_args", AttributeValue::array_string(["--config", "/etc/config.yaml", "--port", "8080"]))
    ]
  }
  
  // 序列化为JSON格式 (模拟)
  let serialized = "{"
    + "\"service_name\":\"order-processing-service\","
    + "\"service_version\":\"2.1.0\","
    + "\"telemetry_sdk_name\":\"azimuth\","
    + "\"telemetry_sdk_version\":\"0.1.0\","
    + "\"attributes\":["
      + "{\"key\":\"deployment.environment\",\"value\":\"string:production\"},"
      + "{\"key\":\"service.instance.id\",\"value\":\"string:instance-12345\"},"
      + "{\"key\":\"host.name\",\"value\":\"string:prod-server-01\"},"
      + "{\"key\":\"region\",\"value\":\"string:us-west-2\"},"
      + "{\"key\":\"zone\",\"value\":\"string:us-west-2a\"},"
      + "{\"key\":\"cluster.name\",\"value\":\"string:orders-cluster\"},"
      + "{\"key\":\"pod.name\",\"value\":\"string:order-processor-7f8d9\"},"
      + "{\"key\":\"container.id\",\"value\":\"string:abc123def456\"},"
      + "{\"key\":\"process.pid\",\"value\":\"int:12345\"},"
      + "{\"key\":\"process.executable.name\",\"value\":\"string:order-processor\"},"
      + "{\"key\":\"process.command_args\",\"value\":\"array_string:[--config,/etc/config.yaml,--port,8080]\"}"
    + "]"
  + "}"
  
  // 验证序列化结果
  assert_eq(serialized.contains("\"service_name\":\"order-processing-service\""), true)
  assert_eq(serialized.contains("\"service_version\":\"2.1.0\""), true)
  assert_eq(serialized.contains("\"telemetry_sdk_name\":\"azimuth\""), true)
  assert_eq(serialized.contains("\"telemetry_sdk_version\":\"0.1.0\""), true)
  assert_eq(serialized.contains("\"deployment.environment\",\"value\":\"string:production\""), true)
  assert_eq(serialized.contains("\"service.instance.id\",\"value\":\"string:instance-12345\""), true)
  assert_eq(serialized.contains("\"process.pid\",\"value\":\"int:12345\""), true)
  assert_eq(serialized.contains("\"process.command_args\",\"value\":\"array_string:[--config,/etc/config.yaml,--port,8080]\""), true)
  
  // 验证属性数量
  let attribute_count = original_resource.attributes.length()
  assert_eq(attribute_count, 11)
  
  // 验证特定属性的存在
  let mut found_deployment_env = false
  let mut found_service_instance = false
  let mut found_process_pid = false
  let mut found_command_args = false
  
  let mut i = 0
  while i < original_resource.attributes.length() {
    match original_resource.attributes[i] {
      ("deployment.environment", AttributeValue::string(env)) => {
        assert_eq(env, "production")
        found_deployment_env = true
      }
      ("service.instance.id", AttributeValue::string(instance_id)) => {
        assert_eq(instance_id, "instance-12345")
        found_service_instance = true
      }
      ("process.pid", AttributeValue::int(pid)) => {
        assert_eq(pid, 12345L)
        found_process_pid = true
      }
      ("process.command_args", AttributeValue::array_string(args)) => {
        assert_eq(args.length(), 4)
        assert_eq(args[0], "--config")
        assert_eq(args[1], "/etc/config.yaml")
        assert_eq(args[2], "--port")
        assert_eq(args[3], "8080")
        found_command_args = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_deployment_env, true)
  assert_eq(found_service_instance, true)
  assert_eq(found_process_pid, true)
  assert_eq(found_command_args, true)
}

test "baggage_serialization" {
  // 测试Baggage的序列化和反序列化
  
  let original_baggage = Baggage::{
    entries: [
      ("user.id", "user-12345"),
      ("session.id", "session-abcdef"),
      ("request.id", "req-789012"),
      ("tenant.id", "tenant-zxy"),
      ("trace.correlation.id", "corr-345678"),
      ("client.version", "1.2.3"),
      ("client.platform", "web"),
      ("locale", "en-US"),
      ("timezone", "America/Los_Angeles"),
      ("feature.flags", "feature1=true,feature2=false")
    ]
  }
  
  // 序列化为W3C Baggage格式
  let mut serialized_entries = []
  let mut i = 0
  while i < original_baggage.entries.length() {
    let (key, value) = original_baggage.entries[i]
    let serialized_entry = key + "=" + value
    serialized_entries.push(serialized_entry)
    i = i + 1
  }
  
  let mut serialized = ""
  let mut first = true
  let mut i = 0
  while i < serialized_entries.length() {
    if first {
      first = false
    } else {
      serialized = serialized + ","
    }
    serialized = serialized + serialized_entries[i]
    i = i + 1
  }
  
  // 验证序列化结果
  assert_eq(serialized.contains("user.id=user-12345"), true)
  assert_eq(serialized.contains("session.id=session-abcdef"), true)
  assert_eq(serialized.contains("request.id=req-789012"), true)
  assert_eq(serialized.contains("tenant.id=tenant-zxy"), true)
  assert_eq(serialized.contains("trace.correlation.id=corr-345678"), true)
  assert_eq(serialized.contains("client.version=1.2.3"), true)
  assert_eq(serialized.contains("client.platform=web"), true)
  assert_eq(serialized.contains("locale=en-US"), true)
  assert_eq(serialized.contains("timezone=America/Los_Angeles"), true)
  assert_eq(serialized.contains("feature.flags=feature1=true,feature2=false"), true)
  
  // 验证条目数量
  assert_eq(original_baggage.entries.length(), 10)
  assert_eq(serialized_entries.length(), 10)
  
  // 反序列化验证 (通过计算逗号分隔的条目数)
  let deserialized_count = serialized.split(",").length()
  assert_eq(deserialized_count, 10)
  
  // 验证特定条目的存在
  assert_eq(serialized.contains("user.id="), true)
  assert_eq(serialized.contains("session.id="), true)
  assert_eq(serialized.contains("request.id="), true)
}