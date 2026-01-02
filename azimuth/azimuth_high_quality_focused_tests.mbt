// Azimuth High Quality Focused Test Suite
// This file contains high quality focused test cases for the Azimuth telemetry system

// Test 1: AttributeValue type conversion and boundary conditions
pub test "属性值类型转换和边界条件测试" {
  // Test string attribute value
  let string_val = azimuth::StringValue("test_value")
  match string_val {
    azimuth::StringValue(s) => assert_eq(s, "test_value")
    _ => assert_true(false)
  }
  
  // Test int attribute value with boundary conditions
  let int_val = azimuth::IntValue(2147483647)
  match int_val {
    azimuth::IntValue(i) => assert_eq(i, 2147483647)
    _ => assert_true(false)
  }
  
  // Test float attribute value with precision
  let float_val = azimuth::FloatValue(3.14159265359)
  match float_val {
    azimuth::FloatValue(f) => assert_true(f > 3.14 && f < 3.15)
    _ => assert_true(false)
  }
  
  // Test boolean attribute values
  let bool_true = azimuth::BoolValue(true)
  let bool_false = azimuth::BoolValue(false)
  match bool_true {
    azimuth::BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  match bool_false {
    azimuth::BoolValue(b) => assert_false(b)
    _ => assert_true(false)
  }
  
  // Test array attribute values
  let string_array = azimuth::ArrayStringValue(["a", "b", "c"])
  match string_array {
    azimuth::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
  
  let int_array = azimuth::ArrayIntValue([1, 2, 3, 4, 5])
  match int_array {
    azimuth::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false)
  }
}

// Test 2: Attributes operations and consistency
pub test "属性操作和一致性测试" {
  // Create new attributes
  let attrs = azimuth::Attributes::new()
  
  // Test setting and getting attributes
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("test_value"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(42))
  azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(3.14))
  azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(true))
  
  // Test getting attributes
  let string_result = azimuth::Attributes::get(attrs, "string.key")
  let int_result = azimuth::Attributes::get(attrs, "int.key")
  let float_result = azimuth::Attributes::get(attrs, "float.key")
  let bool_result = azimuth::Attributes::get(attrs, "bool.key")
  
  // Verify results
  match string_result {
    Some(azimuth::StringValue(s)) => assert_eq(s, "test_value")
    _ => assert_true(false)
  }
  
  match int_result {
    Some(azimuth::IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match float_result {
    Some(azimuth::FloatValue(f)) => assert_true(f > 3.13 && f < 3.15)
    _ => assert_true(false)
  }
  
  match bool_result {
    Some(azimuth::BoolValue(b)) => assert_true(b)
    _ => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = azimuth::Attributes::get(attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

// Test 3: Span lifecycle management
pub test "跨度生命周期管理测试" {
  // Create span context
  let span_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  
  // Create spans with different kinds
  let internal_span = azimuth::Span::new("internal-operation", azimuth::Internal, span_ctx)
  let server_span = azimuth::Span::new("server-operation", azimuth::Server, span_ctx)
  let client_span = azimuth::Span::new("client-operation", azimuth::Client, span_ctx)
  let producer_span = azimuth::Span::new("producer-operation", azimuth::Producer, span_ctx)
  let consumer_span = azimuth::Span::new("consumer-operation", azimuth::Consumer, span_ctx)
  
  // Verify span properties
  assert_eq(internal_span.name, "internal-operation")
  assert_eq(server_span.name, "server-operation")
  assert_eq(client_span.name, "client-operation")
  assert_eq(producer_span.name, "producer-operation")
  assert_eq(consumer_span.name, "consumer-operation")
  
  // Test span recording state
  assert_true(internal_span.recording)
  assert_true(server_span.recording)
  assert_true(client_span.recording)
  assert_true(producer_span.recording)
  assert_true(consumer_span.recording)
  
  // Test span context consistency
  assert_eq(internal_span.span_context.trace_id, "trace-12345")
  assert_eq(server_span.span_context.trace_id, "trace-12345")
  assert_eq(client_span.span_context.trace_id, "trace-12345")
  assert_eq(producer_span.span_context.trace_id, "trace-12345")
  assert_eq(consumer_span.span_context.trace_id, "trace-12345")
  
  assert_eq(internal_span.span_context.span_id, "span-67890")
  assert_eq(server_span.span_context.span_id, "span-67890")
  assert_eq(client_span.span_context.span_id, "span-67890")
  assert_eq(producer_span.span_context.span_id, "span-67890")
  assert_eq(consumer_span.span_context.span_id, "span-67890")
}

// Test 4: Baggage propagation and operations
pub test "行李传播和操作测试" {
  // Create new baggage
  let baggage = azimuth::Baggage::new()
  
  // Test setting baggage entries
  azimuth::Baggage::set(baggage, "key1", "value1")
  azimuth::Baggage::set(baggage, "key2", "value2")
  azimuth::Baggage::set(baggage, "key3", "value3")
  
  // Test getting baggage entries
  let value1 = azimuth::Baggage::get(baggage, "key1")
  let value2 = azimuth::Baggage::get(baggage, "key2")
  let value3 = azimuth::Baggage::get(baggage, "key3")
  
  // Verify results
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = azimuth::Baggage::get(baggage, "non.existent.key")
  assert_eq(non_existent, None)
  
  // Test baggage with special characters
  azimuth::Baggage::set(baggage, "special.key", "special=value&with=chars")
  let special_value = azimuth::Baggage::get(baggage, "special.key")
  match special_value {
    Some(v) => assert_eq(v, "special=value&with=chars")
    None => assert_true(false)
  }
}

// Test 5: Context key-value management
pub test "上下文键值管理测试" {
  // Create context key
  let key = azimuth::ContextKey::new("test.key")
  assert_eq(key.key, "test.key")
  
  // Create context with data
  let context = azimuth::Context::new(Some(("test.key", "test.value")))
  
  // Test context data retrieval
  match context.data {
    Some((k, v)) => {
      assert_eq(k, "test.key")
      assert_eq(v, "test.value")
    }
    None => assert_true(false)
  }
  
  // Test empty context
  let empty_context = azimuth::Context::new(None)
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Instrument creation and usage
pub test "仪器创建和使用测试" {
  // Test Counter instrument
  let counter = azimuth::Counter::new("test.counter", Some("test.description"), Some("test.unit"))
  assert_eq(counter.name, "test.counter")
  
  // Test Histogram instrument
  let histogram = azimuth::Histogram::new("test.histogram", Some("test.description"), Some("test.unit"))
  assert_eq(histogram.name, "test.histogram")
  
  // Test UpDownCounter instrument
  let updown_counter = azimuth::UpDownCounter::new("test.updown.counter", Some("test.description"), Some("test.unit"))
  assert_eq(updown_counter.name, "test.updown.counter")
  
  // Test Gauge instrument
  let gauge = azimuth::Gauge::new("test.gauge", Some("test.description"), Some("test.unit"))
  assert_eq(gauge.name, "test.gauge")
}

// Test 7: Resource attribute management
pub test "资源属性管理测试" {
  // Create new resource
  let resource = azimuth::Resource::new()
  
  // Test setting resource attributes
  azimuth::Resource::set_attribute(resource, "service.name", azimuth::StringValue("test.service"))
  azimuth::Resource::set_attribute(resource, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Resource::set_attribute(resource, "service.instance.id", azimuth::StringValue("instance-123"))
  
  // Test getting resource attributes
  let service_name = azimuth::Resource::get_attribute(resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(resource, "service.version")
  let service_instance_id = azimuth::Resource::get_attribute(resource, "service.instance.id")
  
  // Verify results
  match service_name {
    Some(azimuth::StringValue(s)) => assert_eq(s, "test.service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(azimuth::StringValue(s)) => assert_eq(s, "1.0.0")
    _ => assert_true(false)
  }
  
  match service_instance_id {
    Some(azimuth::StringValue(s)) => assert_eq(s, "instance-123")
    _ => assert_true(false)
  }
  
  // Test non-existent attribute
  let non_existent = azimuth::Resource::get_attribute(resource, "non.existent.attribute")
  assert_eq(non_existent, None)
}

// Test 8: SpanContext creation and validation
pub test "跨度上下文创建和验证测试" {
  // Create span context with valid parameters
  let span_ctx = azimuth::SpanContext::new("trace-1234567890123456", "span-1234567890123456", true, "key1=value1,key2=value2")
  
  // Verify span context properties
  assert_eq(span_ctx.trace_id, "trace-1234567890123456")
  assert_eq(span_ctx.span_id, "span-1234567890123456")
  assert_true(span_ctx.sampled)
  assert_eq(span_ctx.trace_state, "key1=value1,key2=value2")
  
  // Create span context with different sampling flag
  let unsampled_span_ctx = azimuth::SpanContext::new("trace-1234567890123456", "span-1234567890123456", false, "key1=value1")
  assert_false(unsampled_span_ctx.sampled)
  assert_eq(unsampled_span_ctx.trace_state, "key1=value1")
  
  // Create span context with empty trace state
  let empty_trace_state_ctx = azimuth::SpanContext::new("trace-1234567890123456", "span-1234567890123456", true, "")
  assert_eq(empty_trace_state_ctx.trace_state, "")
}

// Test 9: TextMapCarrier header operations
pub test "文本映射载体头部操作测试" {
  // Create new text map carrier
  let carrier = azimuth::TextMapCarrier::new()
  
  // Test setting headers
  azimuth::TextMapCarrier::set(carrier, "traceparent", "00-trace-id-span-id-01")
  azimuth::TextMapCarrier::set(carrier, "tracestate", "key1=value1,key2=value2")
  azimuth::TextMapCarrier::set(carrier, "baggage", "key1=value1;key2=value2")
  
  // Test getting headers
  let traceparent = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let tracestate = azimuth::TextMapCarrier::get(carrier, "tracestate")
  let baggage = azimuth::TextMapCarrier::get(carrier, "baggage")
  
  // Verify results
  match traceparent {
    Some(v) => assert_eq(v, "00-trace-id-span-id-01")
    None => assert_true(false)
  }
  
  match tracestate {
    Some(v) => assert_eq(v, "key1=value1,key2=value2")
    None => assert_true(false)
  }
  
  match baggage {
    Some(v) => assert_eq(v, "key1=value1;key2=value2")
    None => assert_true(false)
  }
  
  // Test non-existent header
  let non_existent = azimuth::TextMapCarrier::get(carrier, "non.existent.header")
  assert_eq(non_existent, None)
}

// Test 10: CompositePropagator composite propagation
pub test "复合传播器复合传播测试" {
  // Create individual propagators
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // Create composite propagator
  let composite_propagator = azimuth::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Verify composite propagator has the correct number of propagators
  assert_eq(composite_propagator.propagators.length(), 2)
  
  // Test injection with composite propagator
  let span_ctx = azimuth::SpanContext::new("trace-1234567890123456", "span-1234567890123456", true, "key1=value1")
  let baggage = azimuth::Baggage::new()
  azimuth::Baggage::set(baggage, "baggage.key1", "baggage.value1")
  
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, carrier, span_ctx, baggage)
  
  // Verify injection results
  let traceparent = azimuth::TextMapCarrier::get(carrier, "traceparent")
  let baggage_header = azimuth::TextMapCarrier::get(carrier, "baggage")
  
  match traceparent {
    Some(v) => assert_true(v.length() > 0)
    None => assert_true(false)
  }
  
  match baggage_header {
    Some(v) => assert_eq(v, "baggage.key1=baggage.value1")
    None => assert_true(false)
  }
  
  // Test extraction with composite propagator
  let extract_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(extract_carrier, "traceparent", "00-trace-1234567890123456-span-1234567890123456-01")
  azimuth::TextMapCarrier::set(extract_carrier, "baggage", "extract.key1=extract.value1")
  
  let extracted_span_ctx = azimuth::CompositePropagator::extract_span_context(composite_propagator, extract_carrier)
  let extracted_baggage = azimuth::CompositePropagator::extract_baggage(composite_propagator, extract_carrier)
  
  // Verify extraction results
  match extracted_span_ctx {
    Some(ctx) => {
      assert_eq(ctx.trace_id, "trace-1234567890123456")
      assert_eq(ctx.span_id, "span-1234567890123456")
      assert_true(ctx.sampled)
    }
    None => assert_true(false)
  }
  
  match extracted_baggage {
    Some(b) => {
      let value = azimuth::Baggage::get(b, "extract.key1")
      match value {
        Some(v) => assert_eq(v, "extract.value1")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}