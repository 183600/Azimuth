// 数据完整性测试 - 测试数据在传输、存储和处理过程中的一致性
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.context.{Context, create_key}
use azimuth.telemetry.api.logs.{LogRecord, SeverityNumber}
use azimuth.telemetry.api.metrics.{Measurement}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, SpanContext}
use azimuth.telemetry.api.propagation.{MapCarrier, W3CTraceContextPropagator}

test "data_integrity_attribute_consistency" {
  // 测试属性数据一致性
  
  // 1. 基本属性类型一致性
  let string_attr = AttributeValue::string("consistency-test")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14159)
  let bool_attr = AttributeValue::bool(true)
  
  // 验证属性值一致性
  match string_attr {
    StringValue(value) => {
      assert_eq(value, "consistency-test")
      assert_eq(value.length(), 17)
    }
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    IntValue(value) => {
      assert_eq(value, 42L)
      assert_eq(value > 0L, true)
    }
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    FloatValue(value) => {
      assert_eq(value > 3.0 && value < 4.0, true)
      assert_eq(value != 0.0, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr {
    BoolValue(value) => {
      assert_eq(value, true)
      assert_eq(value != false, true)
    }
    _ => @test.fail("Expected BoolValue")
  }
  
  // 2. 数组属性类型一致性
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  // 验证数组属性一致性
  match string_array {
    ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match int_array {
    ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1L)
      assert_eq(values[1], 2L)
      assert_eq(values[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match float_array {
    ArrayFloatValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1.1)
      assert_eq(values[1], 2.2)
      assert_eq(values[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match bool_array {
    ArrayBoolValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "data_integrity_resource_consistency" {
  // 测试Resource数据一致性
  
  // 1. 基本Resource一致性
  let resource = Resource::default("consistency-test-service")
  
  assert_eq(resource.service_name, "consistency-test-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 2. Resource复制一致性
  let resource_copy = resource
  assert_eq(resource_copy.service_name, resource.service_name)
  assert_eq(resource_copy.service_version, resource.service_version)
  assert_eq(resource_copy.telemetry_sdk_name, resource.telemetry_sdk_name)
  assert_eq(resource_copy.telemetry_sdk_version, resource.telemetry_sdk_version)
  assert_eq(resource_copy.attributes.length(), resource.attributes.length())
  
  // 3. 复杂Resource一致性
  let complex_resource = Resource::default("complex-consistency-service")
  let complex_attrs = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("web-server-01")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.healthy", AttributeValue::bool(true))
  ]
  
  // 验证复杂属性一致性
  let mut i = 0
  while i < complex_attrs.length() {
    let (key, value) = complex_attrs[i]
    
    match value {
      StringValue(s) => {
        assert_eq(s.length() > 0, true)
        assert_eq(key.contains("."), true)
      }
      IntValue(v) => {
        assert_eq(v >= 0L, true)
        assert_eq(key.contains("."), true)
      }
      FloatValue(v) => {
        assert_eq(v >= 0.0, true)
        assert_eq(key.contains("."), true)
      }
      BoolValue(b) => {
        assert_eq(b == true || b == false, true)
        assert_eq(key.contains("."), true)
      }
      _ => @test.fail("Unexpected attribute type")
    }
    
    i = i + 1
  }
}

test "data_integrity_context_consistency" {
  // 测试Context数据一致性
  
  // 1. 基本Context一致性
  let empty_context = Context::empty()
  assert_eq(empty_context.values.length(), 0)
  
  // 2. Context操作一致性
  let key1 = create_key("consistency.key1")
  let key2 = create_key("consistency.key2")
  let key3 = create_key("consistency.key3")
  
  let context_with_values = empty_context
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  // 验证Context值一致性
  match context_with_values.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value for key1")
  }
  
  match context_with_values.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value for key2")
  }
  
  match context_with_values.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected value for key3")
  }
  
  // 3. Context链式操作一致性
  let chained_context = empty_context
    .with_value(key1, "chained1")
    .with_value(key2, "chained2")
    .with_value(key3, "chained3")
  
  match chained_context.get(key1) {
    Some(value) => assert_eq(value, "chained1")
    None => @test.fail("Expected chained value for key1")
  }
  
  // 4. Context独立性一致性
  let independent_context1 = empty_context.with_value(key1, "independent1")
  let independent_context2 = empty_context.with_value(key1, "independent2")
  
  match independent_context1.get(key1) {
    Some(value) => assert_eq(value, "independent1")
    None => @test.fail("Expected independent value 1")
  }
  
  match independent_context2.get(key1) {
    Some(value) => assert_eq(value, "independent2")
    None => @test.fail("Expected independent value 2")
  }
}

test "data_integrity_log_record_consistency" {
  // 测试LogRecord数据一致性
  
  // 1. 基本LogRecord一致性
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Consistency test log")
    .build()
  
  assert_eq(log_record.severity_number, SeverityNumber::Info)
  assert_eq(log_record.body.unwrap_or(""), "Consistency test log")
  assert_eq(log_record.attributes.length(), 0)
  
  // 2. 复杂LogRecord一致性
  let complex_log_record = LogRecord::builder()
    .severity(SeverityNumber::Error)
    .body("Complex consistency test")
    .severity_text("ERROR")
    .with_attribute("error.type", AttributeValue::string("ConsistencyError"))
    .with_attribute("error.code", AttributeValue::int(500L))
    .with_attribute("error.retryable", AttributeValue::bool(false))
    .with_attribute("error.timestamp", AttributeValue::int(1234567890L))
    .build()
  
  // 验证复杂LogRecord一致性
  assert_eq(complex_log_record.severity_number, SeverityNumber::Error)
  assert_eq(complex_log_record.severity_text.unwrap_or(""), "ERROR")
  assert_eq(complex_log_record.body.unwrap_or(""), "Complex consistency test")
  assert_eq(complex_log_record.attributes.length(), 4)
  
  // 3. LogRecord复制一致性
  let log_record_copy = complex_log_record
  assert_eq(log_record_copy.severity_number, complex_log_record.severity_number)
  assert_eq(log_record_copy.severity_text, complex_log_record.severity_text)
  assert_eq(log_record_copy.body, complex_log_record.body)
  assert_eq(log_record_copy.attributes.length(), complex_log_record.attributes.length())
  
  // 4. LogRecord属性一致性验证
  let mut found_error_type = false
  let mut found_error_code = false
  let mut found_retryable = false
  let mut found_timestamp = false
  
  let mut i = 0
  while i < log_record_copy.attributes.length() {
    let (key, value) = log_record_copy.attributes[i]
    
    match key {
      "error.type" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "ConsistencyError")
            found_error_type = true
          }
          _ => @test.fail("Expected StringValue for error.type")
        }
      }
      "error.code" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 500L)
            found_error_code = true
          }
          _ => @test.fail("Expected IntValue for error.code")
        }
      }
      "error.retryable" => {
        match value {
          BoolValue(v) => {
            assert_eq(v, false)
            found_retryable = true
          }
          _ => @test.fail("Expected BoolValue for error.retryable")
        }
      }
      "error.timestamp" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 1234567890L)
            found_timestamp = true
          }
          _ => @test.fail("Expected IntValue for error.timestamp")
        }
      }
      _ => @test.fail("Unexpected attribute key: " + key)
    }
    
    i = i + 1
  }
  
  assert_eq(found_error_type, true)
  assert_eq(found_error_code, true)
  assert_eq(found_retryable, true)
  assert_eq(found_timestamp, true)
}

test "data_integrity_measurement_consistency" {
  // 测试Measurement数据一致性
  
  // 1. 基本Measurement一致性
  let measurement = Measurement::{
    value: 123.45,
    attributes: [
      ("unit", AttributeValue::string("ms")),
      ("type", AttributeValue::string("response_time"))
    ]
  }
  
  assert_eq(measurement.value, 123.45)
  assert_eq(measurement.attributes.length(), 2)
  
  // 2. 复杂Measurement一致性
  let complex_measurement = Measurement::{
    value: 999.999,
    attributes: [
      ("service.name", AttributeValue::string("consistency-service")),
      ("endpoint", AttributeValue::string("/api/v1/consistency")),
      ("method", AttributeValue::string("GET")),
      ("status_code", AttributeValue::int(200L)),
      ("success", AttributeValue::bool(true)),
      ("tags", AttributeValue::array_string(["consistency", "test", "api"])),
      ("thresholds", AttributeValue::array_float([100.0, 200.0, 500.0])),
      ("retry_counts", AttributeValue::array_int([1L, 2L, 3L])),
      ("feature_flags", AttributeValue::array_bool([true, false, true]))
    ]
  }
  
  // 验证复杂Measurement一致性
  assert_eq(complex_measurement.value, 999.999)
  assert_eq(complex_measurement.attributes.length(), 8)
  
  // 3. Measurement属性类型一致性
  let mut string_attr_count = 0
  let mut int_attr_count = 0
  let mut float_attr_count = 0
  let mut bool_attr_count = 0
  let mut string_array_count = 0
  let mut float_array_count = 0
  let mut int_array_count = 0
  let mut bool_array_count = 0
  
  let mut i = 0
  while i < complex_measurement.attributes.length() {
    let (key, value) = complex_measurement.attributes[i]
    
    match value {
      StringValue(_) => string_attr_count = string_attr_count + 1
      IntValue(_) => int_attr_count = int_attr_count + 1
      FloatValue(_) => float_attr_count = float_attr_count + 1
      BoolValue(_) => bool_attr_count = bool_attr_count + 1
      ArrayStringValue(_) => string_array_count = string_array_count + 1
      ArrayFloatValue(_) => float_array_count = float_array_count + 1
      ArrayIntValue(_) => int_array_count = int_array_count + 1
      ArrayBoolValue(_) => bool_array_count = bool_array_count + 1
      _ => @test.fail("Unexpected attribute type")
    }
    
    i = i + 1
  }
  
  assert_eq(string_attr_count, 3)
  assert_eq(int_attr_count, 1)
  assert_eq(float_attr_count, 0)
  assert_eq(bool_attr_count, 1)
  assert_eq(string_array_count, 1)
  assert_eq(float_array_count, 1)
  assert_eq(int_array_count, 1)
  assert_eq(bool_array_count, 1)
}

test "data_integrity_span_context_consistency" {
  // 测试SpanContext数据一致性
  
  // 1. 基本SpanContext一致性
  let span_context = SpanContext::{
    trace_id: Array::make(16, 0_byte),
    span_id: Array::make(8, 0_byte),
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 0_byte)
  assert_eq(span_context.trace_state, "")
  
  // 2. 带值的SpanContext一致性
  let trace_id_with_value = Array::make(16, 1_byte)
  let span_id_with_value = Array::make(8, 1_byte)
  let span_context_with_values = SpanContext::{
    trace_id: trace_id_with_value,
    span_id: span_id_with_value,
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证带值的SpanContext一致性
  assert_eq(span_context_with_values.trace_id.length(), 16)
  assert_eq(span_context_with_values.span_id.length(), 8)
  assert_eq(span_context_with_values.trace_flags, 1_byte)
  assert_eq(span_context_with_values.trace_state.length() > 0, true)
  
  // 3. SpanContext复制一致性
  let span_context_copy = span_context_with_values
  assert_eq(span_context_copy.trace_id.length(), span_context_with_values.trace_id.length())
  assert_eq(span_context_copy.span_id.length(), span_context_with_values.span_id.length())
  assert_eq(span_context_copy.trace_flags, span_context_with_values.trace_flags)
  assert_eq(span_context_copy.trace_state, span_context_with_values.trace_state)
}

test "data_integrity_propagation_consistency" {
  // 测试传播数据一致性
  
  // 1. 基本传播一致性
  let propagator = W3CTraceContextPropagator::{}
  let context = Context::empty()
  let key = create_key("consistency.propagation.key")
  let context_with_data = context.with_value(key, "consistency.propagation.value")
  
  let carrier = MapCarrier::new()
  propagator.inject(context_with_data, carrier)
  
  let extracted_context = propagator.extract(Context::empty(), carrier)
  
  // 验证传播一致性
  match extracted_context.get(key) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted context value")
  }
  
  // 2. 复杂传播一致性
  let complex_context = context
    .with_value(create_key("key1"), "value1")
    .with_value(create_key("key2"), "value2")
    .with_value(create_key("key3"), "value3")
  
  let complex_carrier = MapCarrier::new()
  propagator.inject(complex_context, complex_carrier)
  
  let complex_extracted_context = propagator.extract(Context::empty(), complex_carrier)
  
  // 验证复杂传播一致性
  match complex_extracted_context.get(create_key("key1")) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted key1 value")
  }
  
  match complex_extracted_context.get(create_key("key2")) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted key2 value")
  }
  
  match complex_extracted_context.get(create_key("key3")) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted key3 value")
  }
  
  // 3. 传播链式一致性
  let chained_context1 = context.with_value(create_key("chain1"), "chain1")
  let chained_context2 = chained_context1.with_value(create_key("chain2"), "chain2")
  let chained_context3 = chained_context2.with_value(create_key("chain3"), "chain3")
  
  let chained_carrier = MapCarrier::new()
  propagator.inject(chained_context3, chained_carrier)
  
  let chained_extracted_context = propagator.extract(Context::empty(), chained_carrier)
  
  // 验证链式传播一致性
  match chained_extracted_context.get(create_key("chain1")) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted chain1 value")
  }
  
  match chained_extracted_context.get(create_key("chain2")) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted chain2 value")
  }
  
  match chained_extracted_context.get(create_key("chain3")) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted chain3 value")
  }
}

test "data_integrity_cross_module_consistency" {
  // 测试跨模块数据一致性
  
  // 1. Resource与LogRecord一致性
  let resource = Resource::default("cross-module-consistency-service")
  let log_record = LogRecord::builder()
    .severity(SeverityNumber::Info)
    .body("Cross-module consistency test")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .with_attribute("sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
    .with_attribute("sdk.version", AttributeValue::string(resource.telemetry_sdk_version))
    .build()
  
  // 验证Resource与LogRecord一致性
  let mut found_service_name = false
  let mut found_sdk_name = false
  let mut found_sdk_version = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, resource.service_name)
            found_service_name = true
          }
          _ => @test.fail("Expected StringValue for service.name")
        }
      }
      "sdk.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, resource.telemetry_sdk_name)
            found_sdk_name = true
          }
          _ => @test.fail("Expected StringValue for sdk.name")
        }
      }
      "sdk.version" => {
        match value {
          StringValue(v) => {
            assert_eq(v, resource.telemetry_sdk_version)
            found_sdk_version = true
          }
          _ => @test.fail("Expected StringValue for sdk.version")
        }
      }
      _ => @test.fail("Unexpected attribute key: " + key)
    }
    
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_sdk_name, true)
  assert_eq(found_sdk_version, true)
  
  // 2. Context与Span一致性
  let tracer = NoopTracer::{}
  let context = Context::empty()
  let context_key = create_key("cross.module.key")
  let context_with_data = context.with_value(context_key, "cross.module.value")
  
  let (span_context, span) = tracer.start_span(
    context_with_data,
    "cross-module-span",
    Some(SpanKind::Internal),
    [("cross.module", AttributeValue::bool(true))]
  )
  
  // 验证Context与Span一致性
  assert_eq(span.name, "cross-module-span")
  assert_eq(span.kind, SpanKind::Internal)
  assert_eq(span.attributes.length(), 1)
  
  match context_with_data.get(context_key) {
    Some(value) => assert_eq(value, "cross.module.value")
    None => @test.fail("Expected context value")
  }
  
  // 3. 传播与Measurement一致性
  let meter = NoopMeter::{}
  let counter = meter.create_counter("cross.module.counter", "count", "Cross-module counter")
  let propagator = W3CTraceContextPropagator::{}
  
  let carrier = MapCarrier::new()
  propagator.inject(context_with_data, carrier)
  
  let extracted_context = propagator.extract(Context::empty(), carrier)
  
  counter.add(1L, [
    ("cross.module", AttributeValue::bool(true)),
    ("context.extracted", AttributeValue::bool(extracted_context.values.length() > 0))
  ])
  
  // 验证传播与Measurement一致性
  match extracted_context.get(context_key) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected extracted context value")
  }
}