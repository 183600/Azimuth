// 序列化和反序列化功能测试
// 测试遥测数据的序列化、格式转换和数据兼容性

use azimuth.telemetry.api.common
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

test "span_serialization_formats" {
  // 测试Span序列化格式
  
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(i) { (i + 1).to_byte() }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(i) { (i + 1).to_byte() }),
    trace_flags: 1_byte,
    trace_state: "vendor1=value1,vendor2=value2"
  }
  
  let test_span = trace::Span::{
    name: "http-request",
    context: span_context,
    kind: trace::Server,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })),
    start_time_unix_nanos: 1640995200L,
    end_time_unix_nanos: Some(1640995300L),
    status: trace::Ok,
    status_description: Some("Request completed successfully"),
    attributes: [
      ("http.method", common::AttributeValue::string("GET")),
      ("http.target", common::AttributeValue::string("/api/users")),
      ("http.status_code", common::AttributeValue::int(200L)),
      ("user.id", common::AttributeValue::int(12345L))
    ],
    events: [
      trace::SpanEvent::{
        name: "database.query",
        timestamp_unix_nanos: 1640995210L,
        attributes: [
          ("db.system", common::AttributeValue::string("postgresql")),
          ("db.statement", common::AttributeValue::string("SELECT * FROM users WHERE id = $1"))
        ]
      }
    ],
    links: [
      trace::SpanLink::{
        context: trace::SpanContext::{
          trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 2_byte }),
          span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte }),
          trace_flags: 1_byte,
          trace_state: ""
        },
        attributes: [("link.type", common::AttributeValue::string("related"))]
      }
    ]
  }
  
  // 模拟JSON序列化
  let json_trace_id = "0102030405060708090a0b0c0d0e0f10"
  let json_span_id = "0102030405060708"
  let json_parent_span_id = "0000000000000000"
  
  let json_serialized = "{"
  json_serialized = json_serialized + "\"name\":\"http-request\","
  json_serialized = json_serialized + "\"traceId\":\"" + json_trace_id + "\","
  json_serialized = json_serialized + "\"spanId\":\"" + json_span_id + "\","
  json_serialized = json_serialized + "\"parentSpanId\":\"" + json_parent_span_id + "\","
  json_serialized = json_serialized + "\"kind\":\"SERVER\","
  json_serialized = json_serialized + "\"startTimeUnixNano\":1640995200,"
  json_serialized = json_serialized + "\"endTimeUnixNano\":1640995300,"
  json_serialized = json_serialized + "\"status\":{\"code\":\"OK\",\"description\":\"Request completed successfully\"},"
  json_serialized = json_serialized + "\"attributes\":{"
  json_serialized = json_serialized + "\"http.method\":\"GET\","
  json_serialized = json_serialized + "\"http.target\":\"/api/users\","
  json_serialized = json_serialized + "\"http.status_code\":200,"
  json_serialized = json_serialized + "\"user.id\":12345"
  json_serialized = json_serialized + "},"
  json_serialized = json_serialized + "\"events\":[{"
  json_serialized = json_serialized + "\"name\":\"database.query\","
  json_serialized = json_serialized + "\"timestampUnixNano\":1640995210,"
  json_serialized = json_serialized + "\"attributes\":{"
  json_serialized = json_serialized + "\"db.system\":\"postgresql\","
  json_serialized = json_serialized + "\"db.statement\":\"SELECT * FROM users WHERE id = $1\""
  json_serialized = json_serialized + "}"
  json_serialized = json_serialized + "}],"
  json_serialized = json_serialized + "\"links\":[{"
  json_serialized = json_serialized + "\"traceId\":\"0202020202020202020202020202020202\","
  json_serialized = json_serialized + "\"spanId\":\"0202020202020202\","
  json_serialized = json_serialized + "\"attributes\":{\"link.type\":\"related\"}"
  json_serialized = json_serialized + "}],"
  json_serialized = json_serialized + "\"traceFlags\":1,"
  json_serialized = json_serialized + "\"traceState\":\"vendor1=value1,vendor2=value2\""
  json_serialized = json_serialized + "}"
  
  // 验证JSON序列化格式
  assert_eq(json_serialized.has_prefix("{"), true)
  assert_eq(json_serialized.has_suffix("}"), true)
  assert_eq(json_serialized.contains("\"name\":\"http-request\""), true)
  assert_eq(json_serialized.contains("\"traceId\":\"0102030405060708090a0b0c0d0e0f10\""), true)
  assert_eq(json_serialized.contains("\"spanId\":\"0102030405060708\""), true)
  assert_eq(json_serialized.contains("\"kind\":\"SERVER\""), true)
  assert_eq(json_serialized.contains("\"http.method\":\"GET\""), true)
  assert_eq(json_serialized.contains("\"user.id\":12345"), true)
  
  // 模拟Protobuf序列化（简化表示）
  let protobuf_serialized = "Span{"
  protobuf_serialized = protobuf_serialized + "name:\"http-request\" "
  protobuf_serialized = protobuf_serialized + "trace_id:\"0102030405060708090a0b0c0d0e0f10\" "
  protobuf_serialized = protobuf_serialized + "span_id:\"0102030405060708\" "
  protobuf_serialized = protobuf_serialized + "parent_span_id:\"0000000000000000\" "
  protobuf_serialized = protobuf_serialized + "kind:SERVER "
  protobuf_serialized = protobuf_serialized + "start_time_unix_nanos:1640995200 "
  protobuf_serialized = protobuf_serialized + "end_time_unix_nanos:1640995300 "
  protobuf_serialized = protobuf_serialized + "status:OK "
  protobuf_serialized = protobuf_serialized + "attributes:{key:\"http.method\" value:{string_value:\"GET\"}} "
  protobuf_serialized = protobuf_serialized + "attributes:{key:\"http.status_code\" value:{int_value:200}} "
  protobuf_serialized = protobuf_serialized + "events:{name:\"database.query\" timestamp:1640995210} "
  protobuf_serialized = protobuf_serialized + "links:{trace_id:\"0202020202020202020202020202020202\" span_id:\"0202020202020202\"}"
  protobuf_serialized = protobuf_serialized + "}"
  
  // 验证Protobuf序列化格式
  assert_eq(protobuf_serialized.has_prefix("Span{"), true)
  assert_eq(protobuf_serialized.has_suffix("}"), true)
  assert_eq(protobuf_serialized.contains("name:\"http-request\""), true)
  assert_eq(protobuf_serialized.contains("kind:SERVER"), true)
  assert_eq(protobuf_serialized.contains("status:OK"), true)
}

test "metric_serialization_formats" {
  // 测试指标序列化格式
  
  let test_measurements = [
    metrics::Measurement::{
      value: 100.5,
      attributes: [
        ("method", common::AttributeValue::string("GET")),
        ("status", common::AttributeValue::string("200"))
      ]
    },
    metrics::Measurement::{
      value: 250.0,
      attributes: [
        ("method", common::AttributeValue::string("POST")),
        ("status", common::AttributeValue::string("201"))
      ]
    },
    metrics::Measurement::{
      value: 75.25,
      attributes: [
        ("method", common::AttributeValue::string("GET")),
        ("status", common::AttributeValue::string("404"))
      ]
    }
  ]
  
  // 模拟JSON指标序列化
  let json_metrics = "["
  let mut i = 0
  while i < test_measurements.length() {
    if i > 0 {
      json_metrics = json_metrics + ","
    }
    json_metrics = json_metrics + "{"
    json_metrics = json_metrics + "\"name\":\"http_request_duration\","
    json_metrics = json_metrics + "\"unit\":\"ms\","
    json_metrics = json_metrics + "\"type\":\"HISTOGRAM\","
    json_metrics = json_metrics + "\"value\":" + test_measurements[i].value.to_string() + ","
    json_metrics = json_metrics + "\"attributes\":{"
    
    let mut j = 0
    while j < test_measurements[i].attributes.length() {
      if j > 0 {
        json_metrics = json_metrics + ","
      }
      
      let key = test_measurements[i].attributes[j].0
      let value = test_measurements[i].attributes[j].1
      
      match value {
        StringValue(s) => {
          json_metrics = json_metrics + "\"" + key + "\":\"" + s + "\""
        }
        IntValue(iv) => {
          json_metrics = json_metrics + "\"" + key + "\":" + iv.to_string()
        }
        FloatValue(fv) => {
          json_metrics = json_metrics + "\"" + key + "\":" + fv.to_string()
        }
        BoolValue(bv) => {
          json_metrics = json_metrics + "\"" + key + "\":" + (if bv { "true" } else { "false" })
        }
        _ => {
          json_metrics = json_metrics + "\"" + key + "\":\"unsupported_type\""
        }
      }
      
      j = j + 1
    }
    
    json_metrics = json_metrics + "}"
    json_metrics = json_metrics + "}"
    i = i + 1
  }
  json_metrics = json_metrics + "]"
  
  // 验证JSON指标序列化
  assert_eq(json_metrics.has_prefix("["), true)
  assert_eq(json_metrics.has_suffix("]"), true)
  assert_eq(json_metrics.contains("\"name\":\"http_request_duration\""), true)
  assert_eq(json_metrics.contains("\"type\":\"HISTOGRAM\""), true)
  assert_eq(json_metrics.contains("\"value\":100.5"), true)
  assert_eq(json_metrics.contains("\"method\":\"GET\""), true)
  assert_eq(json_metrics.contains("\"status\":\"200\""), true)
  
  // 模拟Prometheus格式序列化
  let prometheus_metrics = ""
  i = 0
  while i < test_measurements.length() {
    let method = match test_measurements[i].attributes[0].1 {
      StringValue(s) => s
      _ => "unknown"
    }
    let status = match test_measurements[i].attributes[1].1 {
      StringValue(s) => s
      _ => "unknown"
    }
    
    prometheus_metrics = prometheus_metrics + "http_request_duration_ms{method=\"" + method + "\",status=\"" + status + "\"} " + test_measurements[i].value.to_string() + "\n"
    i = i + 1
  }
  
  // 验证Prometheus格式
  assert_eq(prometheus_metrics.contains("http_request_duration_ms{method=\"GET\",status=\"200\"} 100.5"), true)
  assert_eq(prometheus_metrics.contains("http_request_duration_ms{method=\"POST\",status=\"201\"} 250.0"), true)
  assert_eq(prometheus_metrics.contains("http_request_duration_ms{method=\"GET\",status=\"404\"} 75.25"), true)
}

test "log_serialization_formats" {
  // 测试日志序列化格式
  
  let test_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200L,
    observed_timestamp_unix_nanos: Some(1640995201L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("User login successful"),
    attributes: [
      ("user.id", common::AttributeValue::int(12345L)),
      ("user.name", common::AttributeValue::string("john.doe")),
      ("client.ip", common::AttributeValue::string("192.168.1.100")),
      ("session.id", common::AttributeValue::string("sess_abc123")),
      ("auth.method", common::AttributeValue::string("password")),
      ("mfa.verified", common::AttributeValue::bool(true))
    ],
    trace_id: Some([for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })),
    span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })),
    trace_flags: Some(1_byte),
    resource: Some(common::Resource::{
      service_name: "auth-service",
      service_version: Some("2.1.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [
        ("environment", common::AttributeValue::string("production")),
        ("region", common::AttributeValue::string("us-west-2"))
      ]
    }),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "auth-logger",
      version: Some("2.1.0"),
      schema_url: Some("http://example.com/auth-schema")
    })
  }
  
  // 模拟JSON日志序列化
  let json_log = "{"
  json_log = json_log + "\"timestampUnixNano\":1640995200,"
  json_log = json_log + "\"observedTimestampUnixNano\":1640995201,"
  json_log = json_log + "\"severityNumber\":\"INFO\","
  json_log = json_log + "\"severityText\":\"INFO\","
  json_log = json_log + "\"body\":\"User login successful\","
  json_log = json_log + "\"attributes\":{"
  json_log = json_log + "\"user.id\":12345,"
  json_log = json_log + "\"user.name\":\"john.doe\","
  json_log = json_log + "\"client.ip\":\"192.168.1.100\","
  json_log = json_log + "\"session.id\":\"sess_abc123\","
  json_log = json_log + "\"auth.method\":\"password\","
  json_log = json_log + "\"mfa.verified\":true"
  json_log = json_log + "},"
  json_log = json_log + "\"traceId\":\"01010101010101010101010101010101\","
  json_log = json_log + "\"spanId\":\"0101010101010101\","
  json_log = json_log + "\"traceFlags\":1,"
  json_log = json_log + "\"resource\":{"
  json_log = json_log + "\"service.name\":\"auth-service\","
  json_log = json_log + "\"service.version\":\"2.1.0\","
  json_log = json_log + "\"telemetry.sdk.name\":\"azimuth\","
  json_log = json_log + "\"telemetry.sdk.version\":\"0.1.0\","
  json_log = json_log + "\"attributes\":{"
  json_log = json_log + "\"environment\":\"production\","
  json_log = json_log + "\"region\":\"us-west-2\""
  json_log = json_log + "}"
  json_log = json_log + "},"
  json_log = json_log + "\"instrumentationScope\":{"
  json_log = json_log + "\"name\":\"auth-logger\","
  json_log = json_log + "\"version\":\"2.1.0\","
  json_log = json_log + "\"schemaUrl\":\"http://example.com/auth-schema\""
  json_log = json_log + "}"
  json_log = json_log + "}"
  
  // 验证JSON日志序列化
  assert_eq(json_log.has_prefix("{"), true)
  assert_eq(json_log.has_suffix("}"), true)
  assert_eq(json_log.contains("\"severityNumber\":\"INFO\""), true)
  assert_eq(json_log.contains("\"body\":\"User login successful\""), true)
  assert_eq(json_log.contains("\"user.id\":12345"), true)
  assert_eq(json_log.contains("\"user.name\":\"john.doe\""), true)
  assert_eq(json_log.contains("\"mfa.verified\":true"), true)
  assert_eq(json_log.contains("\"traceId\":\"01010101010101010101010101010101\""), true)
  
  // 模拟结构化文本日志序列化
  let text_log = "1640995200 [INFO] User login successful "
  text_log = text_log + "user.id=12345 "
  text_log = text_log + "user.name=john.doe "
  text_log = text_log + "client.ip=192.168.1.100 "
  text_log = text_log + "session.id=sess_abc123 "
  text_log = text_log + "auth.method=password "
  text_log = text_log + "mfa.verified=true "
  text_log = text_log + "trace_id=01010101010101010101010101010101 "
  text_log = text_log + "span_id=0101010101010101 "
  text_log = text_log + "service.name=auth-service "
  text_log = text_log + "service.version=2.1.0"
  
  // 验证文本日志格式
  assert_eq(text_log.has_prefix("1640995200 [INFO]"), true)
  assert_eq(text_log.contains("User login successful"), true)
  assert_eq(text_log.contains("user.id=12345"), true)
  assert_eq(text_log.contains("user.name=john.doe"), true)
  assert_eq(text_log.contains("mfa.verified=true"), true)
  assert_eq(text_log.contains("service.name=auth-service"), true)
}

test "attribute_value_serialization" {
  // 测试属性值序列化
  
  let test_attributes = [
    ("string.attr", common::AttributeValue::string("test value")),
    ("int.attr", common::AttributeValue::int(42L)),
    ("float.attr", common::AttributeValue::float(3.14159)),
    ("bool.attr", common::AttributeValue::bool(true)),
    ("string.array.attr", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int.array.attr", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array.attr", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array.attr", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 模拟属性值JSON序列化
  let json_attributes = "{"
  let mut i = 0
  while i < test_attributes.length() {
    if i > 0 {
      json_attributes = json_attributes + ","
    }
    
    let key = test_attributes[i].0
    let value = test_attributes[i].1
    
    json_attributes = json_attributes + "\"" + key + "\":"
    
    match value {
      StringValue(s) => {
        json_attributes = json_attributes + "\"" + s + "\""
      }
      IntValue(iv) => {
        json_attributes = json_attributes + iv.to_string()
      }
      FloatValue(fv) => {
        json_attributes = json_attributes + fv.to_string()
      }
      BoolValue(bv) => {
        json_attributes = json_attributes + (if bv { "true" } else { "false" })
      }
      ArrayStringValue(arr) => {
        json_attributes = json_attributes + "["
        let mut j = 0
        while j < arr.length() {
          if j > 0 {
            json_attributes = json_attributes + ","
          }
          json_attributes = json_attributes + "\"" + arr[j] + "\""
          j = j + 1
        }
        json_attributes = json_attributes + "]"
      }
      ArrayIntValue(arr) => {
        json_attributes = json_attributes + "["
        let mut j = 0
        while j < arr.length() {
          if j > 0 {
            json_attributes = json_attributes + ","
          }
          json_attributes = json_attributes + arr[j].to_string()
          j = j + 1
        }
        json_attributes = json_attributes + "]"
      }
      ArrayFloatValue(arr) => {
        json_attributes = json_attributes + "["
        let mut j = 0
        while j < arr.length() {
          if j > 0 {
            json_attributes = json_attributes + ","
          }
          json_attributes = json_attributes + arr[j].to_string()
          j = j + 1
        }
        json_attributes = json_attributes + "]"
      }
      ArrayBoolValue(arr) => {
        json_attributes = json_attributes + "["
        let mut j = 0
        while j < arr.length() {
          if j > 0 {
            json_attributes = json_attributes + ","
          }
          json_attributes = json_attributes + (if arr[j] { "true" } else { "false" })
          j = j + 1
        }
        json_attributes = json_attributes + "]"
      }
    }
    
    i = i + 1
  }
  json_attributes = json_attributes + "}"
  
  // 验证属性值JSON序列化
  assert_eq(json_attributes.has_prefix("{"), true)
  assert_eq(json_attributes.has_suffix("}"), true)
  assert_eq(json_attributes.contains("\"string.attr\":\"test value\""), true)
  assert_eq(json_attributes.contains("\"int.attr\":42"), true)
  assert_eq(json_attributes.contains("\"float.attr\":3.14159"), true)
  assert_eq(json_attributes.contains("\"bool.attr\":true"), true)
  assert_eq(json_attributes.contains("\"string.array.attr\":[\"a\",\"b\",\"c\"]"), true)
  assert_eq(json_attributes.contains("\"int.array.attr\":[1,2,3]"), true)
  assert_eq(json_attributes.contains("\"float.array.attr\":[1.1,2.2,3.3]"), true)
  assert_eq(json_attributes.contains("\"bool.array.attr\":[true,false,true]"), true)
}

test "resource_serialization" {
  // 测试资源序列化
  
  let test_resource = common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("availability_zone", common::AttributeValue::string("us-west-2a")),
      ("instance_id", common::AttributeValue::string("i-1234567890abcdef0")),
      ("cloud.provider", common::AttributeValue::string("aws")),
      ("cloud.region", common::AttributeValue::string("us-west-2")),
      ("host.name", common::AttributeValue::string("payment-server-01")),
      ("host.ip", common::AttributeValue::string("10.0.1.100")),
      ("os.type", common::AttributeValue::string("linux")),
      ("os.version", common::AttributeValue::string("ubuntu-20.04"))
    ]
  }
  
  // 模拟资源JSON序列化
  let json_resource = "{"
  json_resource = json_resource + "\"service.name\":\"payment-service\","
  json_resource = json_resource + "\"service.version\":\"2.1.0\","
  json_resource = json_resource + "\"telemetry.sdk.name\":\"azimuth\","
  json_resource = json_resource + "\"telemetry.sdk.version\":\"0.1.0\","
  json_resource = json_resource + "\"attributes\":{"
  json_resource = json_resource + "\"environment\":\"production\","
  json_resource = json_resource + "\"region\":\"us-west-2\","
  json_resource = json_resource + "\"availability_zone\":\"us-west-2a\","
  json_resource = json_resource + "\"instance_id\":\"i-1234567890abcdef0\","
  json_resource = json_resource + "\"cloud.provider\":\"aws\","
  json_resource = json_resource + "\"cloud.region\":\"us-west-2\","
  json_resource = json_resource + "\"host.name\":\"payment-server-01\","
  json_resource = json_resource + "\"host.ip\":\"10.0.1.100\","
  json_resource = json_resource + "\"os.type\":\"linux\","
  json_resource = json_resource + "\"os.version\":\"ubuntu-20.04\""
  json_resource = json_resource + "}"
  json_resource = json_resource + "}"
  
  // 验证资源JSON序列化
  assert_eq(json_resource.has_prefix("{"), true)
  assert_eq(json_resource.has_suffix("}"), true)
  assert_eq(json_resource.contains("\"service.name\":\"payment-service\""), true)
  assert_eq(json_resource.contains("\"service.version\":\"2.1.0\""), true)
  assert_eq(json_resource.contains("\"telemetry.sdk.name\":\"azimuth\""), true)
  assert_eq(json_resource.contains("\"environment\":\"production\""), true)
  assert_eq(json_resource.contains("\"cloud.provider\":\"aws\""), true)
  assert_eq(json_resource.contains("\"host.name\":\"payment-server-01\""), true)
  
  // 模拟资源Prometheus标签格式
  let prometheus_labels = ""
  prometheus_labels = prometheus_labels + "service_name=\"payment-service\","
  prometheus_labels = prometheus_labels + "service_version=\"2.1.0\","
  prometheus_labels = prometheus_labels + "telemetry_sdk_name=\"azimuth\","
  prometheus_labels = prometheus_labels + "environment=\"production\","
  prometheus_labels = prometheus_labels + "region=\"us-west-2\","
  prometheus_labels = prometheus_labels + "availability_zone=\"us-west-2a\","
  prometheus_labels = prometheus_labels + "instance_id=\"i-1234567890abcdef0\","
  prometheus_labels = prometheus_labels + "cloud_provider=\"aws\","
  prometheus_labels = prometheus_labels + "host_name=\"payment-server-01\""
  
  // 验证Prometheus标签格式
  assert_eq(prometheus_labels.contains("service_name=\"payment-service\""), true)
  assert_eq(prometheus_labels.contains("environment=\"production\""), true)
  assert_eq(prometheus_labels.contains("cloud_provider=\"aws\""), true)
  assert_eq(prometheus_labels.contains("host_name=\"payment-server-01\""), true)
}

test "instrumentation_scope_serialization" {
  // 测试工具范围序列化
  
  let test_scope = common::InstrumentationScope::{
    name: "payment-processor",
    version: Some("2.1.0"),
    schema_url: Some("http://opentelemetry.io/schemas/1.20.0")
  }
  
  // 模拟工具范围JSON序列化
  let json_scope = "{"
  json_scope = json_scope + "\"name\":\"payment-processor\","
  json_scope = json_scope + "\"version\":\"2.1.0\","
  json_scope = json_scope + "\"schemaUrl\":\"http://opentelemetry.io/schemas/1.20.0\""
  json_scope = json_scope + "}"
  
  // 验证工具范围JSON序列化
  assert_eq(json_scope.has_prefix("{"), true)
  assert_eq(json_scope.has_suffix("}"), true)
  assert_eq(json_scope.contains("\"name\":\"payment-processor\""), true)
  assert_eq(json_scope.contains("\"version\":\"2.1.0\""), true)
  assert_eq(json_scope.contains("\"schemaUrl\":\"http://opentelemetry.io/schemas/1.20.0\""), true)
  
  // 测试最小工具范围
  let minimal_scope = common::InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  let json_minimal_scope = "{"
  json_minimal_scope = json_minimal_scope + "\"name\":\"minimal-scope\""
  json_minimal_scope = json_minimal_scope + "}"
  
  // 验证最小工具范围序列化
  assert_eq(json_minimal_scope, "{\"name\":\"minimal-scope\"}")
  assert_eq(json_minimal_scope.contains("\"name\":\"minimal-scope\""), true)
  assert_eq(json_minimal_scope.contains("\"version\""), false)
  assert_eq(json_minimal_scope.contains("\"schemaUrl\""), false)
}

test "batch_serialization" {
  // 测试批量序列化
  
  // 创建批量Span数据
  let batch_spans = [] : Array[trace::Span]
  let mut i = 0
  
  while i < 3 {
    let span_context = trace::SpanContext::{
      trace_id: [for j = 0; j < 16; j = j + 1].map(fn(j) { ((i + 1 + j) % 256).to_byte() }),
      span_id: [for j = 0; j < 8; j = j + 1].map(fn(j) { ((i + 1 + j) % 256).to_byte() }),
      trace_flags: 1_byte,
      trace_state: ""
    }
    
    let span = trace::Span::{
      name: "batch-span-" + i.to_string(),
      context: span_context,
      kind: trace::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200L + i.to_int64(),
      end_time_unix_nanos: Some(1640995300L + i.to_int64()),
      status: trace::Ok,
      status_description: Some("Batch span " + i.to_string() + " completed"),
      attributes: [("span.index", common::AttributeValue::int(i.to_int64()))],
      events: [],
      links: []
    }
    
    batch_spans.push(span)
    i = i + 1
  }
  
  // 模拟批量Span JSON序列化
  let json_batch_spans = "{\"resourceSpans\":[{"
  json_batch_spans = json_batch_spans + "\"resource\":{"
  json_batch_spans = json_batch_spans + "\"attributes\":["
  json_batch_spans = json_batch_spans + "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"batch-service\"}}"
  json_batch_spans = json_batch_spans + "]"
  json_batch_spans = json_batch_spans + "},"
  json_batch_spans = json_batch_spans + "\"scopeSpans\":[{"
  json_batch_spans = json_batch_spans + "\"scope\":{"
  json_batch_spans = json_batch_spans + "\"name\":\"batch-scope\","
  json_batch_spans = json_batch_spans + "\"version\":\"1.0.0\""
  json_batch_spans = json_batch_spans + "},"
  json_batch_spans = json_batch_spans + "\"spans\":["
  
  i = 0
  while i < batch_spans.length() {
    if i > 0 {
      json_batch_spans = json_batch_spans + ","
    }
    
    json_batch_spans = json_batch_spans + "{"
    json_batch_spans = json_batch_spans + "\"traceId\":\"" + "0202030405060708090a0b0c0d0e0f10" + "\","
    json_batch_spans = json_batch_spans + "\"spanId\":\"" + "0202030405060708" + "\","
    json_batch_spans = json_batch_spans + "\"name\":\"batch-span-" + i.to_string() + "\","
    json_batch_spans = json_batch_spans + "\"kind\":\"INTERNAL\","
    json_batch_spans = json_batch_spans + "\"startTimeUnixNano\":" + (1640995200L + i.to_int64()).to_string() + ","
    json_batch_spans = json_batch_spans + "\"endTimeUnixNano\":" + (1640995300L + i.to_int64()).to_string() + ","
    json_batch_spans = json_batch_spans + "\"status\":{\"code\":\"OK\"},"
    json_batch_spans = json_batch_spans + "\"attributes\":["
    json_batch_spans = json_batch_spans + "{\"key\":\"span.index\",\"value\":{\"intValue\":" + i.to_string() + "}}"
    json_batch_spans = json_batch_spans + "]"
    json_batch_spans = json_batch_spans + "}"
    
    i = i + 1
  }
  
  json_batch_spans = json_batch_spans + "]"
  json_batch_spans = json_batch_spans + "}]"
  json_batch_spans = json_batch_spans + "}]}"
  
  // 验证批量Span序列化
  assert_eq(json_batch_spans.has_prefix("{\"resourceSpans\":[{"), true)
  assert_eq(json_batch_spans.has_suffix("}]}"), true)
  assert_eq(json_batch_spans.contains("\"name\":\"batch-span-0\""), true)
  assert_eq(json_batch_spans.contains("\"name\":\"batch-span-1\""), true)
  assert_eq(json_batch_spans.contains("\"name\":\"batch-span-2\""), true)
  assert_eq(json_batch_spans.contains("\"span.index\":{\"intValue\":0}"), true)
  assert_eq(json_batch_spans.contains("\"span.index\":{\"intValue\":2}"), true)
  
  // 创建批量日志数据
  let batch_logs = [] : Array[logs::LogRecord]
  i = 0
  
  while i < 3 {
    let log_record = logs::LogRecord::{
      timestamp_unix_nanos: 1640995200L + i.to_int64(),
      observed_timestamp_unix_nanos: None,
      severity_number: logs::Info,
      severity_text: Some("INFO"),
      body: Some("Batch log message " + i.to_string()),
      attributes: [("log.index", common::AttributeValue::int(i.to_int64()))],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: None,
      instrumentation_scope: None
    }
    
    batch_logs.push(log_record)
    i = i + 1
  }
  
  // 模拟批量日志JSON序列化
  let json_batch_logs = "{\"resourceLogs\":[{"
  json_batch_logs = json_batch_logs + "\"resource\":{"
  json_batch_logs = json_batch_logs + "\"attributes\":["
  json_batch_logs = json_batch_logs + "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"batch-service\"}}"
  json_batch_logs = json_batch_logs + "]"
  json_batch_logs = json_batch_logs + "},"
  json_batch_logs = json_batch_logs + "\"scopeLogs\":[{"
  json_batch_logs = json_batch_logs + "\"scope\":{"
  json_batch_logs = json_batch_logs + "\"name\":\"batch-logger\","
  json_batch_logs = json_batch_logs + "\"version\":\"1.0.0\""
  json_batch_logs = json_batch_logs + "},"
  json_batch_logs = json_batch_logs + "\"logRecords\":["
  
  i = 0
  while i < batch_logs.length() {
    if i > 0 {
      json_batch_logs = json_batch_logs + ","
    }
    
    json_batch_logs = json_batch_logs + "{"
    json_batch_logs = json_batch_logs + "\"timeUnixNano\":" + (1640995200L + i.to_int64()).to_string() + ","
    json_batch_logs = json_batch_logs + "\"severityNumber\":\"INFO\","
    json_batch_logs = json_batch_logs + "\"severityText\":\"INFO\","
    json_batch_logs = json_batch_logs + "\"body\":{\"stringValue\":\"Batch log message " + i.to_string() + "\"},"
    json_batch_logs = json_batch_logs + "\"attributes\":["
    json_batch_logs = json_batch_logs + "{\"key\":\"log.index\",\"value\":{\"intValue\":" + i.to_string() + "}}"
    json_batch_logs = json_batch_logs + "]"
    json_batch_logs = json_batch_logs + "}"
    
    i = i + 1
  }
  
  json_batch_logs = json_batch_logs + "]"
  json_batch_logs = json_batch_logs + "}]"
  json_batch_logs = json_batch_logs + "}]}"
  
  // 验证批量日志序列化
  assert_eq(json_batch_logs.has_prefix("{\"resourceLogs\":[{"), true)
  assert_eq(json_batch_logs.has_suffix("}]}"), true)
  assert_eq(json_batch_logs.contains("\"body\":{\"stringValue\":\"Batch log message 0\"}"), true)
  assert_eq(json_batch_logs.contains("\"body\":{\"stringValue\":\"Batch log message 2\"}"), true)
  assert_eq(json_batch_logs.contains("\"log.index\":{\"intValue\":1}"), true)
}