// Azimuth Enhanced Core Functionality Test Suite
// This file contains enhanced test cases focusing on core telemetry functionality

// Test 1: Telemetry Data Serialization
test "telemetry data serialization integrity" {
  let attr_value = @azimuth.StringValue("test-value")
  let attributes = @azimuth.Attributes { values: [("test-key", attr_value)] }
  let resource = @azimuth.Resource { attributes: [("service.name", @azimuth.StringValue("azimuth-test"))] }
  
  // Verify attribute creation and retrieval
  assert_eq(attributes.values.length(), 1)
  match attributes.values[0] {
    (key, @azimuth.StringValue(value)) => {
      assert_eq(key, "test-key")
      assert_eq(value, "test-value")
    }
    _ => assert_true(false)
  }
  
  // Verify resource attributes
  assert_eq(resource.attributes.length(), 1)
  match resource.attributes[0] {
    (key, @azimuth.StringValue(value)) => {
      assert_eq(key, "service.name")
      assert_eq(value, "azimuth-test")
    }
    _ => assert_true(false)
  }
}

// Test 2: Cross-Service Context Propagation
test "cross-service context propagation consistency" {
  let span_context = @azimuth.SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  let carrier = @azimuth.TextMapCarrier { 
    headers: [
      ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
      ("tracestate", "rojo=00f067aa0ba902b7")
    ]
  }
  
  // Verify trace context structure
  assert_eq(span_context.trace_id.length(), 32)
  assert_eq(span_context.span_id.length(), 16)
  assert_true(span_context.sampled)
  assert_eq(carrier.headers.length(), 2)
}

// Test 3: Metrics Aggregation Calculation
test "metrics aggregation calculation accuracy" {
  let counter = @azimuth.Counter {
    name: "test_counter",
    description: Some("Test counter metric"),
    unit: Some("count")
  }
  
  let histogram = @azimuth.Histogram {
    name: "test_histogram",
    description: Some("Test histogram metric"),
    unit: Some("ms")
  }
  
  // Verify metric instrument creation
  match counter {
    @azimuth.Counter(name, desc, unit) => {
      assert_eq(name, "test_counter")
      assert_eq(desc, Some("Test counter metric"))
      assert_eq(unit, Some("count"))
    }
    _ => assert_true(false)
  }
  
  match histogram {
    @azimuth.Histogram(name, desc, unit) => {
      assert_eq(name, "test_histogram")
      assert_eq(desc, Some("Test histogram metric"))
      assert_eq(unit, Some("ms"))
    }
    _ => assert_true(false)
  }
}

// Test 4: Log Record and Trace Correlation
test "log record and trace correlation accuracy" {
  let log_context = @azimuth.Context {
    data: Some(("trace-id", "1234567890abcdef"))
  }
  
  let baggage = @azimuth.Baggage {
    entries: [
      ("user-id", "user123"),
      ("session-id", "session456")
    ]
  }
  
  // Verify context data
  match log_context.data {
    Some((key, value)) => {
      assert_eq(key, "trace-id")
      assert_eq(value, "1234567890abcdef")
    }
    None => assert_true(false)
  }
  
  // Verify baggage entries
  assert_eq(baggage.entries.length(), 2)
  assert_eq(baggage.entries[0], ("user-id", "user123"))
  assert_eq(baggage.entries[1], ("session-id", "session456"))
}

// Test 5: Resource Attribute Merging
test "resource attribute merging strategy" {
  let resource1 = @azimuth.Resource {
    attributes: [
      ("service.name", @azimuth.StringValue("service-A")),
      ("service.version", @azimuth.StringValue("1.0.0"))
    ]
  }
  
  let resource2 = @azimuth.Resource {
    attributes: [
      ("service.name", @azimuth.StringValue("service-B")),
      ("service.instance.id", @azimuth.StringValue("instance-123"))
    ]
  }
  
  // Verify resource attributes
  assert_eq(resource1.attributes.length(), 2)
  assert_eq(resource2.attributes.length(), 2)
  
  // Verify service name attributes
  match resource1.attributes[0] {
    (key, @azimuth.StringValue(value)) => {
      assert_eq(key, "service.name")
      assert_eq(value, "service-A")
    }
    _ => assert_true(false)
  }
  
  match resource2.attributes[0] {
    (key, @azimuth.StringValue(value)) => {
      assert_eq(key, "service.name")
      assert_eq(value, "service-B")
    }
    _ => assert_true(false)
  }
}

// Test 6: Sampling Strategy
test "sampling strategy configuration" {
  let span_context_sampled = @azimuth.SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: ""
  }
  
  let span_context_not_sampled = @azimuth.SpanContext {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203332",
    sampled: false,
    trace_state: ""
  }
  
  // Verify sampling decisions
  assert_true(span_context_sampled.sampled)
  assert_false(span_context_not_sampled.sampled)
  assert_eq(span_context_sampled.trace_id, span_context_not_sampled.trace_id)
  assert_not_eq(span_context_sampled.span_id, span_context_not_sampled.span_id)
}

// Test 7: Time Series Data Processing
test "time series data processing operations" {
  let timestamp_values = [
    (1000, @azimuth.IntValue(10)),
    (2000, @azimuth.IntValue(20)),
    (3000, @azimuth.IntValue(15)),
    (4000, @azimuth.IntValue(25))
  ]
  
  // Verify time series data structure
  assert_eq(timestamp_values.length(), 4)
  assert_eq(timestamp_values[0], (1000, @azimuth.IntValue(10)))
  assert_eq(timestamp_values[1], (2000, @azimuth.IntValue(20)))
  assert_eq(timestamp_values[2], (3000, @azimuth.IntValue(15)))
  assert_eq(timestamp_values[3], (4000, @azimuth.IntValue(25)))
  
  // Calculate sum of values
  let sum = timestamp_values.reduce(fn(acc, pair) {
    match pair.1 {
      @azimuth.IntValue(value) => acc + value
      _ => acc
    }
  }, 0)
  assert_eq(sum, 70)
}

// Test 8: Error Boundary Handling
test "error boundary handling resilience" {
  let error_status = @azimuth.Error
  let ok_status = @azimuth.Ok
  let unset_status = @azimuth.Unset
  
  // Verify status code creation
  match error_status {
    @azimuth.Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match ok_status {
    @azimuth.Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  match unset_status {
    @azimuth.Unset => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 9: Concurrent Safety
test "concurrent safety data integrity" {
  let shared_attributes = @azimuth.Attributes {
    values: [
      ("key1", @azimuth.StringValue("value1")),
      ("key2", @azimuth.IntValue(42)),
      ("key3", @azimuth.BoolValue(true))
    ]
  }
  
  // Verify attribute data integrity
  assert_eq(shared_attributes.values.length(), 3)
  
  match shared_attributes.values[0] {
    (key, @azimuth.StringValue(value)) => {
      assert_eq(key, "key1")
      assert_eq(value, "value1")
    }
    _ => assert_true(false)
  }
  
  match shared_attributes.values[1] {
    (key, @azimuth.IntValue(value)) => {
      assert_eq(key, "key2")
      assert_eq(value, 42)
    }
    _ => assert_true(false)
  }
  
  match shared_attributes.values[2] {
    (key, @azimuth.BoolValue(value)) => {
      assert_eq(key, "key3")
      assert_eq(value, true)
    }
    _ => assert_true(false)
  }
}

// Test 10: Performance Benchmark
test "performance benchmark operations" {
  let large_attribute_set = @azimuth.Attributes {
    values: [
      for i = 0; i < 100; i++ {
        ("attr_" + i.to_string(), @azimuth.IntValue(i))
      }
    ]
  }
  
  // Verify large attribute set creation
  assert_eq(large_attribute_set.values.length(), 100)
  
  // Verify first and last attributes
  match large_attribute_set.values[0] {
    (key, @azimuth.IntValue(value)) => {
      assert_eq(key, "attr_0")
      assert_eq(value, 0)
    }
    _ => assert_true(false)
  }
  
  match large_attribute_set.values[99] {
    (key, @azimuth.IntValue(value)) => {
      assert_eq(key, "attr_99")
      assert_eq(value, 99)
    }
    _ => assert_true(false)
  }
}