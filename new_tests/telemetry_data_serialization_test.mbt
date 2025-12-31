// 遥测数据序列化与反序列化测试用例

test "telemetry_attribute_value_serialization" {
  // 测试遥测属性值序列化与反序列化
  
  // 测试字符串属性
  let string_attr = ("http.method", common::AttributeValue::string("GET"))
  assert_eq(string_attr.0, "http.method")
  assert_eq(string_attr.1 |> common::AttributeValue::string_value |> some, "GET")
  
  // 测试整数属性
  let int_attr = ("http.status_code", common::AttributeValue::int(200L))
  assert_eq(int_attr.0, "http.status_code")
  assert_eq(int_attr.1 |> common::AttributeValue::int_value |> some, 200L)
  
  // 测试浮点数属性
  let float_attr = ("response.time", common::AttributeValue::float(125.5))
  assert_eq(float_attr.0, "response.time")
  assert_eq(float_attr.1 |> common::AttributeValue::float_value |> some, 125.5)
  
  // 测试布尔属性
  let bool_attr = ("cache.hit", common::AttributeValue::bool(true))
  assert_eq(bool_attr.0, "cache.hit")
  assert_eq(bool_attr.1 |> common::AttributeValue::bool_value |> some, true)
  
  // 测试数组属性
  let string_array_attr = ("tags", common::AttributeValue::array_string(["web", "api", "v1"]))
  assert_eq(string_array_attr.0, "tags")
  let array_value = string_array_attr.1 |> common::AttributeValue::array_string_value |> some
  assert_eq(array_value.length(), 3)
  assert_eq(array_value[0], "web")
  assert_eq(array_value[1], "api")
  assert_eq(array_value[2], "v1")
  
  // 测试整数数组属性
  let int_array_attr = ("status_codes", common::AttributeValue::array_int([200L, 404L, 500L]))
  assert_eq(int_array_attr.0, "status_codes")
  let int_array_value = int_array_attr.1 |> common::AttributeValue::array_int_value |> some
  assert_eq(int_array_value.length(), 3)
  assert_eq(int_array_value[0], 200L)
  assert_eq(int_array_value[1], 404L)
  assert_eq(int_array_value[2], 500L)
  
  // 验证属性数量
  let all_attributes = [string_attr, int_attr, float_attr, bool_attr, string_array_attr, int_array_attr]
  assert_eq(all_attributes.length(), 6)
  
  // 模拟序列化过程
  let mut serialized_data = []
  let mut i = 0
  while i < all_attributes.length() {
    let attr = all_attributes[i]
    let serialized_item = attr.0 + ":" + match attr.1 {
      common::StringValue(v) => "string:" + v
      common::IntValue(v) => "int:" + v.to_string()
      common::FloatValue(v) => "float:" + v.to_string()
      common::BoolValue(v) => "bool:" + (if v { "true" } else { "false" })
      common::ArrayStringValue(arr) => {
        let mut joined = ""
        let mut j = 0
        while j < arr.length() {
          if j > 0 { joined = joined + "," }
          joined = joined + arr[j]
          j = j + 1
        }
        "array_string:" + joined
      }
      common::ArrayIntValue(arr) => {
        let mut joined = ""
        let mut j = 0
        while j < arr.length() {
          if j > 0 { joined = joined + "," }
          joined = joined + arr[j].to_string()
          j = j + 1
        }
        "array_int:" + joined
      }
      _ => "unknown"
    }
    serialized_data.push(serialized_item)
    i = i + 1
  }
  
  // 验证序列化结果
  assert_eq(serialized_data.length(), 6)
  assert_eq(serialized_data[0], "http.method:string:GET")
  assert_eq(serialized_data[1], "http.status_code:int:200")
  assert_eq(serialized_data[2], "response.time:float:125.5")
  assert_eq(serialized_data[3], "cache.hit:bool:true")
  assert_eq(serialized_data[4], "tags:array_string:web,api,v1")
  assert_eq(serialized_data[5], "status_codes:array_int:200,404,500")
}

test "telemetry_span_serialization" {
  // 测试Span序列化
  
  let span_context = trace::SpanContext::{
    trace_id: [0x0a_byte, 0xf7_byte, 0x65_byte, 0x19_byte, 0x16_byte, 0xcd_byte, 0x43_byte, 0xdd_byte,
               0x84_byte, 0x48_byte, 0xeb_byte, 0x21_byte, 0x1c_byte, 0x80_byte, 0x31_byte, 0x9c_byte],
    span_id: [0xb7_byte, 0xad_byte, 0x6b_byte, 0x71_byte, 0x69_byte, 0x20_byte, 0x33_byte, 0x31_byte],
    trace_flags: 0x01_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  let span = trace::Span::{
    name: "http.request",
    context: span_context,
    kind: trace::Server,
    parent_span_id: Some([0x00_byte, 0x00_byte, 0x00_byte, 0x00_byte, 0x00_byte, 0x00_byte, 0x00_byte, 0x00_byte]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200500000000L),
    status: trace::Ok,
    status_description: Some("Request completed successfully"),
    attributes: [
      ("http.method", common::AttributeValue::string("POST")),
      ("http.url", common::AttributeValue::string("https://api.example.com/users")),
      ("http.status_code", common::AttributeValue::int(201L)),
      ("user.id", common::AttributeValue::int(12345L))
    ],
    events: [
      trace::SpanEvent::{
        name: "database.query",
        timestamp_unix_nanos: 1640995200100000000L,
        attributes: [
          ("db.statement", common::AttributeValue::string("SELECT * FROM users WHERE id = ?")),
          ("db.duration_ms", common::AttributeValue::float(25.3))
        ]
      }
    ],
    links: []
  }
  
  // 验证Span基本属性
  assert_eq(span.name, "http.request")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.status, trace::Ok)
  assert_eq(span.attributes.length(), 4)
  assert_eq(span.events.length(), 1)
  assert_eq(span.links.length(), 0)
  
  // 验证时间戳
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  assert_eq(span.end_time_unix_nanos |> some, 1640995200500000000L)
  
  // 验证Span ID和Trace ID长度
  assert_eq(span.context.trace_id.length(), 16)
  assert_eq(span.context.span_id.length(), 8)
  
  // 模拟Span序列化为JSON格式
  let span_json = "{"
    + "\"name\":\"" + span.name + "\"," 
    + "\"kind\":\"Server\"," 
    + "\"status\":\"Ok\"," 
    + "\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\"," 
    + "\"span_id\":\"b7ad6b7169203331\"," 
    + "\"start_time_unix_nanos\":" + span.start_time_unix_nanos.to_string() + "," 
    + "\"end_time_unix_nanos\":" + (span.end_time_unix_nanos |> some).to_string() + "," 
    + "\"attributes_count\":" + span.attributes.length().to_string() + "," 
    + "\"events_count\":" + span.events.length().to_string() 
    + "}"
  
  // 验证JSON包含必要字段
  assert_eq(span_json.contains("\"name\":\"http.request\""), true)
  assert_eq(span_json.contains("\"kind\":\"Server\""), true)
  assert_eq(span_json.contains("\"status\":\"Ok\""), true)
  assert_eq(span_json.contains("\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\""), true)
  assert_eq(span_json.contains("\"span_id\":\"b7ad6b7169203331\""), true)
  assert_eq(span_json.contains("\"attributes_count\":4"), true)
  assert_eq(span_json.contains("\"events_count\":1"), true)
}

test "telemetry_metric_serialization" {
  // 测试Metric序列化
  
  // 创建测试指标
  let counter_instrument = metrics::NoopCounter::{}
  let histogram_instrument = metrics::NoopHistogram::{}
  let gauge_instrument = metrics::NoopGauge::{}
  
  // 模拟指标测量数据
  let counter_measurements = [
    metrics::Measurement::{
      value: 100.0,
      attributes: [
        ("http.method", common::AttributeValue::string("GET")),
        ("http.status_code", common::AttributeValue::int(200L))
      ]
    },
    metrics::Measurement::{
      value: 50.0,
      attributes: [
        ("http.method", common::AttributeValue::string("POST")),
        ("http.status_code", common::AttributeValue::int(201L))
      ]
    }
  ]
  
  let histogram_measurements = [
    metrics::Measurement::{
      value: 125.5,
      attributes: [
        ("operation", common::AttributeValue::string("database.query")),
        ("table", common::AttributeValue::string("users"))
      ]
    },
    metrics::Measurement::{
      value: 89.2,
      attributes: [
        ("operation", common::AttributeValue::string("database.query")),
        ("table", common::AttributeValue::string("orders"))
      ]
    },
    metrics::Measurement::{
      value: 234.7,
      attributes: [
        ("operation", common::AttributeValue::string("api.call")),
        ("endpoint", common::AttributeValue::string("/users"))
      ]
    }
  ]
  
  let gauge_measurements = [
    metrics::Measurement::{
      value: 1024.0,
      attributes: [
        ("metric.type", common::AttributeValue::string("memory.usage")),
        ("unit", common::AttributeValue::string("bytes"))
      ]
    },
    metrics::Measurement::{
      value: 75.2,
      attributes: [
        ("metric.type", common::AttributeValue::string("cpu.usage")),
        ("unit", common::AttributeValue::string("percent"))
      ]
    }
  ]
  
  // 验证测量数据
  assert_eq(counter_measurements.length(), 2)
  assert_eq(histogram_measurements.length(), 3)
  assert_eq(gauge_measurements.length(), 2)
  
  // 验证具体测量值
  assert_eq(counter_measurements[0].value, 100.0)
  assert_eq(histogram_measurements[1].value, 89.2)
  assert_eq(gauge_measurements[0].value, 1024.0)
  
  // 验证属性
  assert_eq(counter_measurements[0].attributes.length(), 2)
  assert_eq(histogram_measurements[0].attributes.length(), 2)
  assert_eq(gauge_measurements[0].attributes.length(), 2)
  
  // 模拟指标序列化
  let serialize_measurements = fn(measurements: Array[metrics::Measurement]) -> String {
    let mut result = "["
    let mut i = 0
    while i < measurements.length() {
      if i > 0 { result = result + "," }
      let m = measurements[i]
      result = result + "{\"value\":" + m.value.to_string() + ",\"attributes\":["
      let mut j = 0
      while j < m.attributes.length() {
        if j > 0 { result = result + "," }
        let attr = m.attributes[j]
        result = result + "{\"key\":\"" + attr.0 + "\",\"value\":\""
        result = result + match attr.1 {
          common::StringValue(v) => "string:" + v
          common::IntValue(v) => "int:" + v.to_string()
          common::FloatValue(v) => "float:" + v.to_string()
          common::BoolValue(v) => "bool:" + (if v { "true" } else { "false" })
          _ => "unknown"
        }
        result = result + "\"}"
        j = j + 1
      }
      result = result + "]}"
      i = i + 1
    }
    result = result + "]"
    result
  }
  
  // 序列化各类指标
  let counter_json = serialize_measurements(counter_measurements)
  let histogram_json = serialize_measurements(histogram_measurements)
  let gauge_json = serialize_measurements(gauge_measurements)
  
  // 验证序列化结果
  assert_eq(counter_json.contains("\"value\":100.0"), true)
  assert_eq(counter_json.contains("\"value\":50.0"), true)
  assert_eq(counter_json.contains("http.method"), true)
  assert_eq(counter_json.contains("http.status_code"), true)
  
  assert_eq(histogram_json.contains("\"value\":125.5"), true)
  assert_eq(histogram_json.contains("\"value\":89.2"), true)
  assert_eq(histogram_json.contains("\"value\":234.7"), true)
  assert_eq(histogram_json.contains("database.query"), true)
  assert_eq(histogram_json.contains("api.call"), true)
  
  assert_eq(gauge_json.contains("\"value\":1024.0"), true)
  assert_eq(gauge_json.contains("\"value\":75.2"), true)
  assert_eq(gauge_json.contains("memory.usage"), true)
  assert_eq(gauge_json.contains("cpu.usage"), true)
  
  // 验证JSON格式正确性
  assert_eq(counter_json.starts_with("["), true)
  assert_eq(counter_json.ends_with("]"), true)
  assert_eq(histogram_json.starts_with("["), true)
  assert_eq(histogram_json.ends_with("]"), true)
  assert_eq(gauge_json.starts_with("["), true)
  assert_eq(gauge_json.ends_with("]"), true)
}

test "telemetry_resource_serialization" {
  // 测试Resource序列化
  
  let basic_resource = common::Resource::default("payment-service")
  assert_eq(basic_resource.service_name, "payment-service")
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.service_version |> some |> is_none, true)
  assert_eq(basic_resource.attributes.length(), 0)
  
  let enhanced_resource = common::Resource::{
    service_name: "user-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", common::AttributeValue::string("instance-001")),
      ("deployment.environment", common::AttributeValue::string("production")),
      ("host.name", common::AttributeValue::string("web-server-01")),
      ("process.pid", common::AttributeValue::int(12345L)),
      ("process.command", common::AttributeValue::string("/usr/bin/node")),
      ("availability.zone", common::AttributeValue::string("us-west-2a")),
      ("cloud.provider", common::AttributeValue::string("aws")),
      ("cloud.region", common::AttributeValue::string("us-west-2")),
      ("kubernetes.namespace", common::AttributeValue::string("default")),
      ("kubernetes.pod.name", common::AttributeValue::string("user-service-7d4f8c9b-xyz"))
    ]
  }
  
  // 验证增强资源
  assert_eq(enhanced_resource.service_name, "user-service")
  assert_eq(enhanced_resource.service_version |> some, "2.1.0")
  assert_eq(enhanced_resource.attributes.length(), 10)
  
  // 验证特定属性
  assert_eq(enhanced_resource.attributes[0].0, "service.instance.id")
  assert_eq(enhanced_resource.attributes[0].1 |> common::AttributeValue::string_value |> some, "instance-001")
  assert_eq(enhanced_resource.attributes[3].0, "process.pid")
  assert_eq(enhanced_resource.attributes[3].1 |> common::AttributeValue::int_value |> some, 12345L)
  
  // 模拟Resource序列化
  let serialize_resource = fn(resource: common::Resource) -> String {
    let mut result = "{"
    result = result + "\"service_name\":\"" + resource.service_name + "\"," 
    result = result + "\"telemetry_sdk_name\":\"" + resource.telemetry_sdk_name + "\"," 
    result = result + "\"telemetry_sdk_version\":\"" + resource.telemetry_sdk_version + "\"," 
    
    match resource.service_version {
      Some(version) => result = result + "\"service_version\":\"" + version + "\"," 
      None => result = result + "\"service_version\":null," 
    }
    
    result = result + "\"attributes\":["
    let mut i = 0
    while i < resource.attributes.length() {
      if i > 0 { result = result + "," }
      let attr = resource.attributes[i]
      result = result + "{\"key\":\"" + attr.0 + "\",\"value\":\""
      result = result + match attr.1 {
        common::StringValue(v) => "string:" + v
        common::IntValue(v) => "int:" + v.to_string()
        common::FloatValue(v) => "float:" + v.to_string()
        common::BoolValue(v) => "bool:" + (if v { "true" } else { "false" })
        _ => "unknown"
      }
      result = result + "\"}"
      i = i + 1
    }
    result = result + "]}"
    result
  }
  
  // 序列化资源
  let basic_json = serialize_resource(basic_resource)
  let enhanced_json = serialize_resource(enhanced_resource)
  
  // 验证基本资源序列化
  assert_eq(basic_json.contains("\"service_name\":\"payment-service\""), true)
  assert_eq(basic_json.contains("\"telemetry_sdk_name\":\"azimuth\""), true)
  assert_eq(basic_json.contains("\"telemetry_sdk_version\":\"0.1.0\""), true)
  assert_eq(basic_json.contains("\"service_version\":null"), true)
  assert_eq(basic_json.contains("\"attributes\":[]"), true)
  
  // 验证增强资源序列化
  assert_eq(enhanced_json.contains("\"service_name\":\"user-service\""), true)
  assert_eq(enhanced_json.contains("\"service_version\":\"2.1.0\""), true)
  assert_eq(enhanced_json.contains("\"attributes\":[{"), true)
  assert_eq(enhanced_json.contains("service.instance.id"), true)
  assert_eq(enhanced_json.contains("deployment.environment"), true)
  assert_eq(enhanced_json.contains("kubernetes.pod.name"), true)
  
  // 验证属性数量
  let basic_attr_count = basic_json.split("\"key\"").length() - 1
  let enhanced_attr_count = enhanced_json.split("\"key\"").length() - 1
  assert_eq(basic_attr_count, 0)
  assert_eq(enhanced_attr_count, 10)
}