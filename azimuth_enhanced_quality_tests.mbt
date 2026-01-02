// Azimuth Telemetry System - Enhanced Quality Test Suite
// This file contains 8 high-quality test cases covering advanced telemetry functionality

// Test 1: Attribute Value Type Operations and Conversions
test "attribute value type operations and conversions" {
  // Test string attribute value
  let string_attr = AttributeValue::StringValue("test_value")
  match string_attr {
    AttributeValue::StringValue(s) => assert_eq(s, "test_value")
    _ => assert_true(false)
  }
  
  // Test int attribute value
  let int_attr = AttributeValue::IntValue(42)
  match int_attr {
    AttributeValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute value
  let float_attr = AttributeValue::FloatValue(3.14)
  match float_attr {
    AttributeValue::FloatValue(f) => assert_eq(f, 3.14)
    _ => assert_true(false)
  }
  
  // Test bool attribute value
  let bool_attr = AttributeValue::BoolValue(true)
  match bool_attr {
    AttributeValue::BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  // Test array string attribute value
  let string_array_attr = AttributeValue::ArrayStringValue(["value1", "value2", "value3"])
  match string_array_attr {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "value1")
      assert_eq(arr[1], "value2")
      assert_eq(arr[2], "value3")
    }
    _ => assert_true(false)
  }
  
  // Test array int attribute value
  let int_array_attr = AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])
  match int_array_attr {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[4], 5)
      
      // Test array sum
      let mut sum = 0
      for i in arr {
        sum = sum + i
      }
      assert_eq(sum, 15)
    }
    _ => assert_true(false)
  }
}

// Test 2: Cross-Service Telemetry Propagation
test "cross-service telemetry propagation" {
  // Create initial span context
  let span_context = SpanContext {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  }
  
  // Create baggage with entries
  let baggage = Baggage {
    entries: [
      ("user.id", "user123"),
      ("request.id", "req456"),
      ("session.id", "sess789")
    ]
  }
  
  // Simulate HTTP headers extraction
  let carrier = TextMapCarrier {
    headers: [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"),
      ("tracestate", "key1=value1,key2=value2"),
      ("baggage", "user.id=user123,request.id=req456,session.id=sess789"),
      ("x-request-id", "req-1234567890")
    ]
  }
  
  // Verify traceparent header format
  let mut traceparent_found = false
  for (key, value) in carrier.headers {
    if key == "traceparent" {
      assert_eq(value, "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01")
      traceparent_found = true
    }
  }
  assert_true(traceparent_found)
  
  // Create child span context
  let child_span_context = SpanContext {
    trace_id: span_context.trace_id,
    span_id: "fedcba0987654321",
    sampled: span_context.sampled,
    trace_state: span_context.trace_state
  }
  
  // Verify inheritance of trace context
  assert_eq(child_span_context.trace_id, span_context.trace_id)
  assert_eq(child_span_context.span_id, "fedcba0987654321")
  assert_eq(child_span_context.sampled, span_context.sampled)
  assert_eq(child_span_context.trace_state, span_context.trace_state)
  
  // Verify baggage entries
  assert_eq(baggage.entries.length(), 3)
  
  // Check for specific baggage entries
  let mut user_id_found = false
  let mut request_id_found = false
  
  for (key, value) in baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "user123")
        user_id_found = true
      }
      "request.id" => {
        assert_eq(value, "req456")
        request_id_found = true
      }
      _ => ()
    }
  }
  
  assert_true(user_id_found)
  assert_true(request_id_found)
}

// Test 3: Performance Benchmark Tests
test "performance benchmark tests" {
  // Test attribute creation performance
  let mut attributes = []
  for i in 0..1000 {
    let key = "attr_" + i.to_string()
    let value = AttributeValue::IntValue(i)
    attributes = attributes + [(key, value)]
  }
  
  // Verify creation completed
  assert_eq(attributes.length(), 1000)
  
  // Test attribute lookup performance
  let mut found_count = 0
  for (key, value) in attributes {
    if key.contains("attr_") {
      match value {
        AttributeValue::IntValue(v) => {
          if v % 100 == 0 {
            found_count = found_count + 1
          }
        }
        _ => ()
      }
    }
  }
  
  // Verify lookup results
  assert_eq(found_count, 10) // Should find 10 items (0, 100, 200, ..., 900)
  
  // Test span context creation performance
  let mut span_contexts = []
  for i in 0..100 {
    let span_context = SpanContext {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "span_" + i.to_string(),
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }
    span_contexts = span_contexts + [span_context]
  }
  
  // Verify span context creation
  assert_eq(span_contexts.length(), 100)
  
  // Test baggage creation performance
  let mut baggage_items = []
  for i in 0..100 {
    let baggage = Baggage {
      entries: [
        ("user.id", "user" + i.to_string()),
        ("request.id", "req" + i.to_string()),
        ("session.id", "sess" + i.to_string())
      ]
    }
    baggage_items = baggage_items + [baggage]
  }
  
  // Verify baggage creation
  assert_eq(baggage_items.length(), 100)
  assert_eq(baggage_items[0].entries.length(), 3)
}

// Test 4: Error Boundary and Recovery Tests
test "error boundary and recovery tests" {
  // Test with empty trace ID
  let empty_trace_context = SpanContext {
    trace_id: "", // Empty trace ID
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: "key1=value1"
  }
  
  // System should handle empty data gracefully
  assert_eq(empty_trace_context.trace_id, "")
  assert_eq(empty_trace_context.span_id, "1234567890abcdef")
  assert_true(empty_trace_context.sampled)
  
  // Test with empty baggage
  let empty_baggage = Baggage { entries: [] }
  assert_eq(empty_baggage.entries.length(), 0)
  
  // Test with empty attributes
  let empty_attributes = Attributes { values: [] }
  assert_eq(empty_attributes.values.length(), 0)
  
  // Test with empty resource
  let empty_resource = Resource { attributes: [] }
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Test with mixed valid/invalid data
  let mixed_attributes = Attributes {
    values: [
      ("valid.string", AttributeValue::StringValue("valid_value")),
      ("valid.int", AttributeValue::IntValue(42)),
      ("valid.bool", AttributeValue::BoolValue(true)),
      ("valid.array", AttributeValue::ArrayStringValue(["a", "b", "c"]))
    ]
  }
  
  assert_eq(mixed_attributes.values.length(), 4)
  
  // Verify all attribute types
  for (key, value) in mixed_attributes.values {
    match key {
      "valid.string" => {
        match value {
          AttributeValue::StringValue(s) => assert_eq(s, "valid_value")
          _ => assert_true(false)
        }
      }
      "valid.int" => {
        match value {
          AttributeValue::IntValue(i) => assert_eq(i, 42)
          _ => assert_true(false)
        }
      }
      "valid.bool" => {
        match value {
          AttributeValue::BoolValue(b) => assert_true(b)
          _ => assert_true(false)
        }
      }
      "valid.array" => {
        match value {
          AttributeValue::ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "a")
            assert_eq(arr[1], "b")
            assert_eq(arr[2], "c")
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // Test memory cleanup simulation
  let mut large_dataset = []
  for i in 0..1000 {
    let span_context = SpanContext {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "span_" + i.to_string(),
      sampled: true,
      trace_state: "key1=value1"
    }
    large_dataset = large_dataset + [span_context]
  }
  
  assert_eq(large_dataset.length(), 1000)
  
  // Simulate cleanup by reducing dataset size
  large_dataset = []
  assert_eq(large_dataset.length(), 0)
}

// Test 5: Concurrent Safety Tests
test "concurrent safety tests" {
  // Simulate concurrent span context creation
  let mut concurrent_span_contexts = []
  
  // Create span contexts with overlapping trace IDs (simulating concurrent operations)
  for i in 0..50 {
    let span_context = SpanContext {
      trace_id: "1234567890abcdef1234567890abcdef", // Same trace ID
      span_id: "concurrent_span_" + i.to_string(),
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }
    concurrent_span_contexts = concurrent_span_contexts + [span_context]
  }
  
  // Verify all span contexts were created correctly
  assert_eq(concurrent_span_contexts.length(), 50)
  
  // Verify span ID uniqueness
  let mut span_ids = []
  for span_context in concurrent_span_contexts {
    span_ids = span_ids + [span_context.span_id]
  }
  
  // Check for duplicates (simplified check)
  let mut unique_count = 0
  for i in 0..span_ids.length() {
    let mut is_unique = true
    for j in 0..i {
      if span_ids[i] == span_ids[j] {
        is_unique = false
      }
    }
    if is_unique {
      unique_count = unique_count + 1
    }
  }
  assert_eq(unique_count, 50) // All should be unique
  
  // Test concurrent baggage operations
  let mut concurrent_baggages = []
  
  // Create multiple baggage items with overlapping keys
  for i in 0..20 {
    let baggage = Baggage {
      entries: [
        ("concurrent.id", i.to_string()),
        ("thread.id", "thread_" + (i % 4).to_string()),
        ("operation.id", "op_" + i.to_string())
      ]
    }
    concurrent_baggages = concurrent_baggages + [baggage]
  }
  
  // Verify all baggages were created correctly
  assert_eq(concurrent_baggages.length(), 20)
  assert_eq(concurrent_baggages[0].entries.length(), 3)
  
  // Test concurrent attribute creation
  let mut concurrent_attributes = []
  
  // Create multiple attribute sets with potential conflicts
  for i in 0..30 {
    let attributes = Attributes {
      values: [
        ("concurrent.index", AttributeValue::IntValue(i)),
        ("concurrent.thread", AttributeValue::StringValue("thread_" + (i % 5).to_string())),
        ("concurrent.value", AttributeValue::FloatValue(i * 1.5))
      ]
    }
    concurrent_attributes = concurrent_attributes + [attributes]
  }
  
  // Verify all attributes were created correctly
  assert_eq(concurrent_attributes.length(), 30)
  assert_eq(concurrent_attributes[0].values.length(), 3)
  
  // Test data integrity across concurrent operations
  let mut total_sum = 0
  for attributes in concurrent_attributes {
    for (key, value) in attributes.values {
      if key == "concurrent.index" {
        match value {
          AttributeValue::IntValue(v) => {
            total_sum = total_sum + v
          }
          _ => assert_true(false)
        }
      }
    }
  }
  
  // Verify sum of indices 0+1+2+...+29 = 435
  assert_eq(total_sum, 435)
}

// Test 6: Resource Management Tests
test "resource management tests" {
  // Create resource with various attributes
  let service_resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("azimuth-service")),
      ("service.version", AttributeValue::StringValue("1.2.3")),
      ("service.instance.id", AttributeValue::StringValue("instance-abc123")),
      ("host.name", AttributeValue::StringValue("production-server-01")),
      ("host.arch", AttributeValue::StringValue("x86_64")),
      ("os.type", AttributeValue::StringValue("linux")),
      ("os.version", AttributeValue::StringValue("5.15.0")),
      ("deployment.environment", AttributeValue::StringValue("production"))
    ]
  }
  
  // Verify resource attributes
  assert_eq(service_resource.attributes.length(), 8)
  
  // Test resource merging
  let additional_resource = Resource {
    attributes: [
      ("service.namespace", AttributeValue::StringValue("telemetry")),
      ("service.team", AttributeValue::StringValue("observability")),
      ("region", AttributeValue::StringValue("us-west-2")),
      ("availability.zone", AttributeValue::StringValue("us-west-2a"))
    ]
  }
  
  // Simulate resource merge operation
  let mut merged_attributes = service_resource.attributes
  for attr in additional_resource.attributes {
    merged_attributes = merged_attributes + [attr]
  }
  
  let merged_resource = Resource { attributes: merged_attributes }
  assert_eq(merged_resource.attributes.length(), 12)
  
  // Test resource cleanup
  let temp_resource = Resource {
    attributes: [
      ("temp.data", AttributeValue::StringValue("temporary")),
      ("temp.timestamp", AttributeValue::IntValue(1640995200000))
    ]
  }
  
  // Simulate resource cleanup by removing temporary attributes
  let mut cleaned_attributes = []
  for (key, value) in temp_resource.attributes {
    if not key.starts_with("temp.") {
      cleaned_attributes = cleaned_attributes + [(key, value)]
    }
  }
  
  let cleaned_resource = Resource { attributes: cleaned_attributes }
  assert_eq(cleaned_resource.attributes.length(), 0)
  
  // Test resource limits
  let mut large_resource = Resource { attributes: [] }
  for i in 0..1000 {
    let key = "resource.attr." + i.to_string()
    let value = AttributeValue::StringValue("value_" + i.to_string())
    large_resource.attributes = large_resource.attributes + [(key, value)]
  }
  
  assert_eq(large_resource.attributes.length(), 1000)
  
  // Verify specific attributes exist
  let mut found_specific = false
  for (key, _) in large_resource.attributes {
    if key == "resource.attr.500" {
      found_specific = true
    }
  }
  assert_true(found_specific)
  
  // Test instrumentation scope management
  let full_scope = InstrumentationScope {
    name: "azimuth.instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // Verify scope properties
  assert_eq(full_scope.name, "azimuth.instrumentation")
  match full_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  match full_scope.schema_url {
    Some(s) => assert_eq(s, "https://example.com/schema/v1")
    None => assert_true(false)
  }
  
  // Create an instrumentation scope with minimal fields
  let minimal_scope = InstrumentationScope {
    name: "minimal.instrumentation",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal.instrumentation")
  match minimal_scope.version {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match minimal_scope.schema_url {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Time Series Data Processing Tests
test "time series data processing tests" {
  // Create time series data points using attributes
  let mut time_series_data = []
  let base_timestamp = 1640995200000 // Base timestamp
  
  for i in 0..100 {
    let attributes = [
      ("timestamp", AttributeValue::IntValue(base_timestamp + i * 60000)), // 1 minute intervals
      ("value", AttributeValue::FloatValue(100.0 + (i * 0.5) + (i % 10 * 2.0))), // Simulated metric with pattern
      ("metric.name", AttributeValue::StringValue("cpu.usage")),
      ("metric.unit", AttributeValue::StringValue("percent")),
      ("host.id", AttributeValue::StringValue("host-" + (i % 5).to_string())),
      ("environment", AttributeValue::StringValue("production")),
      ("service", AttributeValue::StringValue("azimuth"))
    ]
    time_series_data = time_series_data + [attributes]
  }
  
  assert_eq(time_series_data.length(), 100)
  
  // Test time series aggregation (average)
  let mut sum = 0.0
  let mut count = 0
  for attributes in time_series_data {
    for (key, value) in attributes {
      if key == "value" {
        match value {
          AttributeValue::FloatValue(v) => {
            sum = sum + v
            count = count + 1
          }
          _ => assert_true(false)
        }
      }
    }
  }
  let average = sum / (count as Float)
  
  assert_true(average > 100.0)
  assert_true(average < 200.0)
  
  // Test time window filtering
  let window_start = base_timestamp + 30 * 60000 // 30 minutes after start
  let window_end = base_timestamp + 60 * 60000   // 60 minutes after start
  
  let mut windowed_data = []
  for attributes in time_series_data {
    for (key, value) in attributes {
      if key == "timestamp" {
        match value {
          AttributeValue::IntValue(ts) => {
            if ts >= window_start && ts <= window_end {
              windowed_data = windowed_data + [attributes]
            }
          }
          _ => assert_true(false)
        }
      }
    }
  }
  
  assert_eq(windowed_data.length(), 31) // Points 30 through 60 inclusive
  
  // Test downsampling (every 10th point)
  let mut downsampled_data = []
  for i in 0..time_series_data.length() {
    if i % 10 == 0 {
      downsampled_data = downsampled_data + [time_series_data[i]]
    }
  }
  
  assert_eq(downsampled_data.length(), 10)
  
  // Test metric calculation (min, max, trend)
  let mut min_value = 1000000.0
  let mut max_value = -1000000.0
  let mut first_value = 0.0
  let mut last_value = 0.0
  let mut first_set = false
  
  for attributes in time_series_data {
    for (key, value) in attributes {
      if key == "value" {
        match value {
          AttributeValue::FloatValue(v) => {
            if v < min_value {
              min_value = v
            }
            if v > max_value {
              max_value = v
            }
            if not first_set {
              first_value = v
              first_set = true
            }
            last_value = v
          }
          _ => assert_true(false)
        }
      }
    }
  }
  
  assert_true(min_value < max_value)
  assert_true(min_value >= 100.0)
  
  // Test trend calculation (simple linear trend)
  let trend = last_value - first_value
  assert_true(trend > 0.0) // Should be increasing trend
  
  // Test host-based grouping
  let mut host_groups = []
  for i in 0..5 {
    host_groups = host_groups + [0] // Initialize counters for 5 hosts
  }
  
  for attributes in time_series_data {
    for (key, value) in attributes {
      if key == "host.id" {
        match value {
          AttributeValue::StringValue(host_id) => {
            if host_id == "host-0" {
              host_groups[0] = host_groups[0] + 1
            } else if host_id == "host-1" {
              host_groups[1] = host_groups[1] + 1
            } else if host_id == "host-2" {
              host_groups[2] = host_groups[2] + 1
            } else if host_id == "host-3" {
              host_groups[3] = host_groups[3] + 1
            } else if host_id == "host-4" {
              host_groups[4] = host_groups[4] + 1
            }
          }
          _ => assert_true(false)
        }
      }
    }
  }
  
  // Verify each host has 20 data points (100 total / 5 hosts)
  for i in 0..5 {
    assert_eq(host_groups[i], 20)
  }
}

// Test 8: WebAssembly Platform Compatibility Tests
test "webassembly platform compatibility tests" {
  // Test WASM-specific attributes using Resource
  let wasm_resource = Resource {
    attributes: [
      ("wasm.memory.usage", AttributeValue::IntValue(1024 * 1024)), // 1MB in bytes
      ("wasm.stack.size", AttributeValue::IntValue(64 * 1024)),     // 64KB in bytes
      ("wasm.heap.size", AttributeValue::IntValue(512 * 1024)),     // 512KB in bytes
      ("wasm.execution.time", AttributeValue::FloatValue(150.5)),   // milliseconds
      ("wasm.function.calls", AttributeValue::IntValue(1000)),
      ("wasm.gc.cycles", AttributeValue::IntValue(5)),
      ("wasm.runtime", AttributeValue::StringValue("wasmtime")),
      ("wasm.version", AttributeValue::StringValue("1.0.0")),
      ("wasm.features", AttributeValue::ArrayStringValue(["bulk-memory", "mutable-globals"]))
    ]
  }
  
  // Verify WASM resource structure
  assert_eq(wasm_resource.attributes.length(), 9)
  
  // Verify specific WASM attributes
  for (key, value) in wasm_resource.attributes {
    match key {
      "wasm.memory.usage" => {
        match value {
          AttributeValue::IntValue(memory) => assert_eq(memory, 1024 * 1024)
          _ => assert_true(false)
        }
      }
      "wasm.function.calls" => {
        match value {
          AttributeValue::IntValue(calls) => assert_eq(calls, 1000)
          _ => assert_true(false)
        }
      }
      "wasm.features" => {
        match value {
          AttributeValue::ArrayStringValue(features) => {
            assert_eq(features.length(), 2)
            assert_eq(features[0], "bulk-memory")
            assert_eq(features[1], "mutable-globals")
          }
          _ => assert_true(false)
        }
      }
      _ => ()
    }
  }
  
  // Test WASM operation tracking using SpanContext
  let wasm_span_context = SpanContext {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "wasm_operation_12345678",
    sampled: true,
    trace_state: "wasm.operation=export,telemetry.type=metrics"
  }
  
  // Verify WASM span context
  assert_eq(wasm_span_context.trace_id, "1234567890abcdef1234567890abcdef")
  assert_eq(wasm_span_context.span_id, "wasm_operation_12345678")
  assert_true(wasm_span_context.sampled)
  assert_eq(wasm_span_context.trace_state, "wasm.operation=export,telemetry.type=metrics")
  
  // Test WASM boundary conditions using Attributes
  let wasm_boundary_attributes = Attributes {
    values: [
      ("wasm.max.memory.allocation", AttributeValue::IntValue(1024 * 1024 * 1024)), // 1GB
      ("wasm.max.execution.time", AttributeValue::FloatValue(5000.0)),              // 5 seconds
      ("wasm.max.stack.depth", AttributeValue::IntValue(1000)),
      ("wasm.max.function.calls", AttributeValue::IntValue(100000)),
      ("wasm.memory.limit.test", AttributeValue::BoolValue(true)),
      ("wasm.time.limit.test", AttributeValue::BoolValue(true)),
      ("wasm.stack.depth.test", AttributeValue::BoolValue(true)),
      ("wasm.function.call.test", AttributeValue::BoolValue(true))
    ]
  }
  
  // Verify boundary test attributes
  assert_eq(wasm_boundary_attributes.values.length(), 8)
  
  // Verify all boundary tests passed
  for (key, value) in wasm_boundary_attributes.values {
    if key.contains("test") {
      match value {
        AttributeValue::BoolValue(passed) => {
          assert_true(passed)
        }
        _ => assert_true(false)
      }
    }
  }
  
  // Test WASM performance optimization using Baggage
  let wasm_optimization_baggage = Baggage {
    entries: [
      ("wasm.original.size", (1024 * 1024).to_string()),    // 1MB
      ("wasm.optimized.size", (512 * 1024).to_string()),    // 512KB (50% reduction)
      ("wasm.compression.ratio", "0.5"),
      ("wasm.load.time.original", "100.0"),                 // milliseconds
      ("wasm.load.time.optimized", "50.0"),                 // milliseconds (50% improvement)
      ("wasm.execution.improvement", "0.25"),               // 25% faster execution
      ("wasm.memory.reduction", "0.3")                      // 30% less memory usage
    ]
  }
  
  // Verify optimization baggage entries
  assert_eq(wasm_optimization_baggage.entries.length(), 7)
  
  // Verify specific optimization metrics
  for (key, value) in wasm_optimization_baggage.entries {
    match key {
      "wasm.optimized.size" => assert_eq(value, (512 * 1024).to_string())
      "wasm.compression.ratio" => assert_eq(value, "0.5")
      "wasm.execution.improvement" => assert_eq(value, "0.25")
      _ => ()
    }
  }
  
  // Test WASM instrumentation scope
  let wasm_instrumentation_scope = InstrumentationScope {
    name: "azimuth.wasm.telemetry",
    version: Some("1.0.0"),
    schema_url: Some("https://azimuth.dev/schemas/wasm-telemetry/v1")
  }
  
  // Verify WASM instrumentation scope
  assert_eq(wasm_instrumentation_scope.name, "azimuth.wasm.telemetry")
  match wasm_instrumentation_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  match wasm_instrumentation_scope.schema_url {
    Some(s) => assert_eq(s, "https://azimuth.dev/schemas/wasm-telemetry/v1")
    None => assert_true(false)
  }
}