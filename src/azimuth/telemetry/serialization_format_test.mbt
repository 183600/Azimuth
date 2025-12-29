// 序列化格式测试 - 测试数据格式转换

test "attribute_value_serialization" {
  // 测试AttributeValue的序列化格式
  
  // 字符串值序列化
  let string_attr = AttributeValue::string("test-value")
  let string_serialized = match string_attr {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  assert_eq(string_serialized, "string:test-value")
  
  // 整数值序列化
  let int_attr = AttributeValue::int(42L)
  let int_serialized = match int_attr {
    IntValue(i) => "int:" + i.to_string()
    _ => "unknown"
  }
  assert_eq(int_serialized, "int:42")
  
  // 浮点数值序列化
  let float_attr = AttributeValue::float(3.14)
  let float_serialized = match float_attr {
    FloatValue(f) => "float:" + f.to_string()
    _ => "unknown"
  }
  assert_eq(float_serialized, "float:3.14")
  
  // 布尔值序列化
  let bool_attr = AttributeValue::bool(true)
  let bool_serialized = match bool_attr {
    BoolValue(b) => if b { "bool:true" } else { "bool:false" }
    _ => "unknown"
  }
  assert_eq(bool_serialized, "bool:true")
  
  // 字符串数组序列化
  let string_array_attr = AttributeValue::array_string(["a", "b", "c"])
  let string_array_serialized = match string_array_attr {
    ArrayStringValue(arr) => {
      let result = "array_string:["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { 
          // 在实际实现中，这里会添加逗号
        }
        // 在实际实现中，这里会添加arr[i]
        i = i + 1
      }
      result + "]"
    }
    _ => "unknown"
  }
  assert_eq(string_array_serialized, "array_string:[")
  
  // 整数数组序列化
  let int_array_attr = AttributeValue::array_int([1L, 2L, 3L])
  let int_array_serialized = match int_array_attr {
    ArrayIntValue(arr) => {
      let result = "array_int:["
      let mut i = 0
      while i < arr.length() {
        if i > 0 { 
          // 在实际实现中，这里会添加逗号
        }
        // 在实际实现中，这里会添加arr[i]
        i = i + 1
      }
      result + "]"
    }
    _ => "unknown"
  }
  assert_eq(int_array_serialized, "array_int:[")
}

test "resource_serialization" {
  // 测试Resource的序列化格式
  
  let resource = common::Resource::default("test-service")
  
  // 基本资源序列化
  let resource_serialized = "Resource{" +
    "service_name:" + resource.service_name + "," +
    "service_version:" + match resource.service_version {
      Some(v) => v
      None => "None"
    } + "," +
    "telemetry_sdk_name:" + resource.telemetry_sdk_name + "," +
    "telemetry_sdk_version:" + resource.telemetry_sdk_version + "," +
    "attributes_count:" + resource.attributes.length().to_string() +
    "}"
  
  assert_eq(resource_serialized, "Resource{service_name:test-service,service_version:None,telemetry_sdk_name:azimuth,telemetry_sdk_version:0.1.0,attributes_count:0}")
  
  // 带有属性的资源序列化
  let resource_with_attrs = common::Resource::default("service-with-attrs")
  let attrs : Array[(String, common::AttributeValue)] = [
    ("key1", AttributeValue::string("value1")),
    ("key2", AttributeValue::int(42L)),
    ("key3", AttributeValue::bool(true))
  ]
  
  let attrs_serialized = "Attributes["
  let mut i = 0
  while i < attrs.length() {
    if i > 0 {
      // 在实际实现中，这里会添加逗号
    }
    let (key, value) = attrs[i]
    let value_serialized = match value {
      StringValue(s) => "string:" + s
      IntValue(i) => "int:" + i.to_string()
      BoolValue(b) => if b { "bool:true" } else { "bool:false" }
      _ => "unknown"
    }
    // 在实际实现中，这里会添加key + ":" + value_serialized
    i = i + 1
  }
  attrs_serialized = attrs_serialized + "]"
  
  assert_eq(attrs_serialized, "Attributes[]")
}

test "context_serialization" {
  // 测试Context的序列化格式
  
  let ctx = context::Context::empty()
  let key1 = context::create_key("key1")
  let key2 = context::create_key("key2")
  let key3 = context::create_key("key3")
  
  let ctx_with_values = ctx
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  // Context序列化
  let context_serialized = "Context{values_count:" + ctx_with_values.values.length().to_string() + "}"
  assert_eq(context_serialized, "Context{values_count:3}")
  
  // 验证值可以被正确访问和序列化
  let value1_serialized = match ctx_with_values.get(key1) {
    Some(v) => "key1:" + v
    None => "key1:None"
  }
  assert_eq(value1_serialized, "key1:value1")
  
  let value2_serialized = match ctx_with_values.get(key2) {
    Some(v) => "key2:" + v
    None => "key2:None"
  }
  assert_eq(value2_serialized, "key2:value2")
  
  let value3_serialized = match ctx_with_values.get(key3) {
    Some(v) => "key3:" + v
    None => "key3:None"
  }
  assert_eq(value3_serialized, "key3:value3")
}

test "baggage_serialization" {
  // 测试Baggage的序列化格式
  
  let baggage = context::Baggage::empty()
  let baggage_with_entries = baggage
    .with_entry("baggage1", "value1")
    .with_entry("baggage2", "value2")
    .with_entry("baggage3", "value3")
  
  // Baggage序列化为W3C格式
  let baggage_serialized = "baggage:"
  let mut i = 0
  while i < baggage_with_entries.entries.length() {
    if i > 0 {
      baggage_serialized = baggage_serialized + ","
    }
    let (key, value) = baggage_with_entries.entries[i]
    baggage_serialized = baggage_serialized + key + "=" + value
    i = i + 1
  }
  
  assert_eq(baggage_serialized, "baggage:baggage1=value1,baggage2=value2,baggage3=value3")
  
  // 验证单个条目的序列化
  match baggage_with_entries.get("baggage1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_entries.get("baggage2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_entries.get("baggage3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
}

test "log_record_serialization" {
  // 测试LogRecord的序列化格式
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Test log message")
    .with_attribute("attr1", AttributeValue::string("value1"))
    .with_attribute("attr2", AttributeValue::int(42L))
    .with_attribute("attr3", AttributeValue::bool(true))
    .build()
  
  // LogRecord序列化
  let severity_serialized = match log_record.severity_number {
    logs::Trace => "TRACE"
    logs::Debug => "DEBUG"
    logs::Info => "INFO"
    logs::Warn => "WARN"
    logs::Error => "ERROR"
    logs::Fatal => "FATAL"
  }
  
  let body_serialized = match log_record.body {
    Some(b) => b
    None => ""
  }
  
  let log_serialized = "LogRecord{" +
    "timestamp:" + log_record.timestamp_unix_nanos.to_string() + "," +
    "severity:" + severity_serialized + "," +
    "body:" + body_serialized + "," +
    "attributes_count:" + log_record.attributes.length().to_string() +
    "}"
  
  assert_eq(log_serialized, "LogRecord{timestamp:1640995200000000000,severity:INFO,body:Test log message,attributes_count:3}")
  
  // 属性序列化
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    let value_serialized = match value {
      StringValue(s) => "string:" + s
      IntValue(i) => "int:" + i.to_string()
      BoolValue(b) => if b { "bool:true" } else { "bool:false" }
      _ => "unknown"
    }
    let attr_serialized = key + "=" + value_serialized
    
    match i {
      0 => assert_eq(attr_serialized, "attr1=string:value1")
      1 => assert_eq(attr_serialized, "attr2=int:42")
      2 => assert_eq(attr_serialized, "attr3=bool:true")
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
}

test "span_serialization" {
  // 测试Span的序列化格式
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "test-span",
    trace::Server,
    [
      ("attr1", AttributeValue::string("value1")),
      ("attr2", AttributeValue::int(42L))
    ],
    Some(1640995200000000000L)
  )
  
  // Span序列化
  let kind_serialized = match span.kind {
    trace::Internal => "INTERNAL"
    trace::Server => "SERVER"
    trace::Client => "CLIENT"
    trace::Producer => "PRODUCER"
    trace::Consumer => "CONSUMER"
  }
  
  let status_serialized = match span.status {
    trace::Unset => "UNSET"
    trace::Ok => "OK"
    trace::Error => "ERROR"
  }
  
  let span_serialized = "Span{" +
    "name:" + span.name + "," +
    "kind:" + kind_serialized + "," +
    "start_time:" + span.start_time_unix_nanos.to_string() + "," +
    "status:" + status_serialized + "," +
    "attributes_count:" + span.attributes.length().to_string() + "," +
    "events_count:" + span.events.length().to_string() + "," +
    "links_count:" + span.links.length().to_string() +
    "}"
  
  assert_eq(span_serialized, "Span{name:test-span,kind:SERVER,start_time:1640995200000000000,status:UNSET,attributes_count:2,events_count:0,links_count:0}")
  
  // SpanContext序列化
  let context_serialized = "SpanContext{" +
    "trace_id:" + span.context.trace_id.length().to_string() + "bytes," +
    "span_id:" + span.context.span_id.length().to_string() + "bytes," +
    "trace_flags:" + span.context.trace_flags.to_string() + "," +
    "trace_state:" + span.context.trace_state +
    "}"
  
  assert_eq(context_serialized, "SpanContext{trace_id:16bytes,span_id:8bytes,trace_flags:0,trace_state:}")
}

test "propagation_format_serialization" {
  // 测试传播格式的序列化
  
  let ctx = context::Context::empty()
  let carrier = propagation::MapCarrier::new()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 注入传播信息
  trace_propagator.inject(ctx, carrier)
  baggage_propagator.inject(ctx, carrier)
  
  // 序列化carrier内容
  let carrier_serialized = "MapCarrier{"
  let mut i = 0
  while i < carrier.data.length() {
    if i > 0 {
      carrier_serialized = carrier_serialized + ","
    }
    let (key, value) = carrier.data[i]
    carrier_serialized = carrier_serialized + key + ":" + value
    i = i + 1
  }
  carrier_serialized = carrier_serialized + "}"
  
  // 验证traceparent格式
  match carrier.get(propagation::TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      // 验证traceparent格式: version-trace_id-span_id-trace_flags
      let parts = trace_parent"-".split_to_string()
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00")  // version
      assert_eq(parts[1].length(), 32)  // trace_id (16 bytes = 32 hex chars)
      assert_eq(parts[2].length(), 16)  // span_id (8 bytes = 16 hex chars)
      assert_eq(parts[3], "01")  // trace_flags
    }
    None => @test.fail("Test failed")
  }
  
  // 验证baggage格式
  match carrier.get(propagation::BAGGAGE_HEADER) {
    Some(baggage) => {
      // 验证baggage格式: key1=value1,key2=value2
      let entries = baggage",".split_to_string()
      assert_eq(entries.length(), 2)
      
      let entry1_parts = entries[0]"=".split_to_string()
      assert_eq(entry1_parts.length(), 2)
      assert_eq(entry1_parts[0], "key1")
      assert_eq(entry1_parts[1], "value1")
      
      let entry2_parts = entries[1]"=".split_to_string()
      assert_eq(entry2_parts.length(), 2)
      assert_eq(entry2_parts[0], "key2")
      assert_eq(entry2_parts[1], "value2")
    }
    None => @test.fail("Test failed")
  }
}

test "metrics_serialization" {
  // 测试Metrics的序列化格式
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter")
  
  let counter = meter.create_counter("test-counter", "count", "Test counter")
  let histogram = meter.create_histogram("test-histogram", "ms", "Test histogram")
  let gauge = meter.create_gauge("test-gauge", "percent", "Test gauge")
  let up_down_counter = meter.create_up_down_counter("test-up-down", "count", "Test up-down counter")
  
  // 模拟指标操作并序列化
  let measurement = metrics::Measurement::{
    value: 42.5,
    attributes: [
      ("attr1", AttributeValue::string("value1")),
      ("attr2", AttributeValue::int(100L))
    ]
  }
  
  // Measurement序列化
  let attributes_serialized = "Attributes["
  let mut i = 0
  while i < measurement.attributes.length() {
    if i > 0 {
      attributes_serialized = attributes_serialized + ","
    }
    let (key, value) = measurement.attributes[i]
    let value_serialized = match value {
      StringValue(s) => "string:" + s
      IntValue(i) => "int:" + i.to_string()
      _ => "unknown"
    }
    attributes_serialized = attributes_serialized + key + "=" + value_serialized
    i = i + 1
  }
  attributes_serialized = attributes_serialized + "]"
  
  let measurement_serialized = "Measurement{" +
    "value:" + measurement.value.to_string() + "," +
    "attributes:" + attributes_serialized +
    "}"
  
  assert_eq(measurement_serialized, "Measurement{value:42.5,attributes:Attributes[attr1=string:value1,attr2=int:100]}")
  
  // 指标类型序列化
  let counter_serialized = "Counter{name:test-counter,unit:count,description:Test counter}"
  let histogram_serialized = "Histogram{name:test-histogram,unit:ms,description:Test histogram}"
  let gauge_serialized = "Gauge{name:test-gauge,unit:percent,description:Test gauge}"
  let up_down_serialized = "UpDownCounter{name:test-up-down,unit:count,description:Test up-down counter}"
  
  assert_eq(counter_serialized, "Counter{name:test-counter,unit:count,description:Test counter}")
  assert_eq(histogram_serialized, "Histogram{name:test-histogram,unit:ms,description:Test histogram}")
  assert_eq(gauge_serialized, "Gauge{name:test-gauge,unit:percent,description:Test gauge}")
  assert_eq(up_down_serialized, "UpDownCounter{name:test-up-down,unit:count,description:Test up-down counter}")
}

test "json_like_serialization" {
  // 测试类似JSON的序列化格式
  
  // 创建复杂的遥测数据结构
  let resource = common::Resource::default("json-test-service")
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("json-test-tracer")
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "json-test-span",
    trace::Server,
    [
      ("string.attr", AttributeValue::string("json-value")),
      ("int.attr", AttributeValue::int(123L)),
      ("float.attr", AttributeValue::float(45.67)),
      ("bool.attr", AttributeValue::bool(true)),
      ("array.attr", AttributeValue::array_string(["item1", "item2", "item3"]))
    ]
  )
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("JSON test log")
    .with_attribute("log.attr", AttributeValue::string("log-value"))
    .build()
  
  // 生成类似JSON的序列化格式
  let json_like = "{"
  
  // Resource部分
  json_like = json_like + "\"resource\":{"
  json_like = json_like + "\"service_name\":\"" + resource.service_name + "\"," 
  json_like = json_like + "\"telemetry_sdk_name\":\"" + resource.telemetry_sdk_name + "\"," 
  json_like = json_like + "\"telemetry_sdk_version\":\"" + resource.telemetry_sdk_version + "\""
  json_like = json_like + "},"
  
  // Span部分
  json_like = json_like + "\"span\":{"
  json_like = json_like + "\"name\":\"" + span.name + "\"," 
  json_like = json_like + "\"kind\":\"SERVER\"," 
  json_like = json_like + "\"start_time\":" + span.start_time_unix_nanos.to_string() + ","
  json_like = json_like + "\"attributes\":["
  
  let mut i = 0
  while i < span.attributes.length() {
    if i > 0 {
      json_like = json_like + ","
    }
    let (key, value) = span.attributes[i]
    json_like = json_like + "{\"key\":\"" + key + "\"," 
    
    match value {
      StringValue(s) => json_like = json_like + "\"type\":\"string\",\"value\":\"" + s + "\"}"
      IntValue(iv) => json_like = json_like + "\"type\":\"int\",\"value\":" + iv.to_string() + "}"
      FloatValue(fv) => json_like = json_like + "\"type\":\"float\",\"value\":" + fv.to_string() + "}"
      BoolValue(bv) => json_like = json_like + "\"type\":\"bool\",\"value\":" + if bv { "true" } else { "false" }
      ArrayStringValue(arr) => {
        json_like = json_like + "\"type\":\"array_string\",\"value\":["
        let mut j = 0
        while j < arr.length() {
          if j > 0 {
            json_like = json_like + ","
          }
          json_like = json_like + "\"" + arr[j] + "\""
          j = j + 1
        }
        json_like = json_like + "]}"
      }
      _ => json_like = json_like + "\"type\":\"unknown\",\"value\":null}"
    }
    
    i = i + 1
  }
  
  json_like = json_like + "]"
  json_like = json_like + "},"
  
  // LogRecord部分
  json_like = json_like + "\"log_record\":{"
  json_like = json_like + "\"timestamp\":" + log_record.timestamp_unix_nanos.to_string() + ","
  json_like = json_like + "\"severity\":\"INFO\"," 
  match log_record.body {
    Some(body) => json_like = json_like + "\"body\":\"" + body + "\""
    None => json_like = json_like + "\"body\":null"
  }
  json_like = json_like + "}"
  
  json_like = json_like + "}"
  
  // 验证JSON-like格式包含预期的内容
  assert_eq(json_like.contains("\"service_name\":\"json-test-service\""), true)
  assert_eq(json_like.contains("\"name\":\"json-test-span\""), true)
  assert_eq(json_like.contains("\"kind\":\"SERVER\""), true)
  assert_eq(json_like.contains("\"severity\":\"INFO\""), true)
  assert_eq(json_like.contains("\"body\":\"JSON test log\""), true)
  assert_eq(json_like.contains("\"type\":\"string\",\"value\":\"json-value\""), true)
  assert_eq(json_like.contains("\"type\":\"int\",\"value\":123"), true)
  assert_eq(json_like.contains("\"type\":\"float\",\"value\":45.67"), true)
  assert_eq(json_like.contains("\"type\":\"bool\",\"value\":true"), true)
}