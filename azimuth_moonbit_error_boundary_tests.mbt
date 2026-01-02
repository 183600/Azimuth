// Azimuth Telemetry System - MoonBit Error Boundary Tests
// This file contains test cases for error boundary handling and recovery

// Define error types for testing
pub enum TelemetryError {
  InvalidAttribute(String)
  SerializationError(String)
  DeserializationError(String)
  ContextPropagationError(String)
  ResourceError(String)
  SpanError(String)
}

// Define result type for error handling
pub type Result[T] = Ok(T) | Error(TelemetryError)

// Test 1: Attribute validation error handling
test "attribute validation error handling" {
  // Test empty key validation
  let validate_empty_key = fn(key : String, value : AttributeValue) -> Result[AttributeValue] {
    if key.length() == 0 {
      Error(InvalidAttribute("Empty key not allowed"))
    } else {
      Ok(value)
    }
  }
  
  // Valid case
  let valid_result = validate_empty_key("valid.key", StringValue("value"))
  match valid_result {
    Ok(v) => {
      match v {
        StringValue(str) => assert_eq(str, "value")
        _ => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Invalid case
  let invalid_result = validate_empty_key("", StringValue("value"))
  match invalid_result {
    Ok(_) => assert_true(false)
    Error(InvalidAttribute(msg)) => assert_eq(msg, "Empty key not allowed")
    Error(_) => assert_true(false)
  }
  
  // Test null/None value validation
  let validate_none_value = fn(value : Option[String]) -> Result[String] {
    match value {
      Some(v) => Ok(v)
      None => Error(InvalidAttribute("None value not allowed"))
    }
  }
  
  let some_result = validate_none_value(Some("value"))
  match some_result {
    Ok(v) => assert_eq(v, "value")
    Error(_) => assert_true(false)
  }
  
  let none_result = validate_none_value(None)
  match none_result {
    Ok(_) => assert_true(false)
    Error(InvalidAttribute(msg)) => assert_eq(msg, "None value not allowed")
    Error(_) => assert_true(false)
  }
}

// Test 2: Serialization error handling
test "serialization error handling" {
  // Test string serialization with special characters
  let safe_serialize_string = fn(value : String) -> Result[String] {
    if value.contains("\0") {
      Error(SerializationError("String contains null character"))
    } else if value.length() > 1000 {
      Error(SerializationError("String too long for serialization"))
    } else {
      Ok(value)
    }
  }
  
  // Valid case
  let valid_string = safe_serialize_string("normal string")
  match valid_string {
    Ok(v) => assert_eq(v, "normal string")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - null character
  let null_string = safe_serialize_string("string\0with\0nulls")
  match null_string {
    Ok(_) => assert_true(false)
    Error(SerializationError(msg)) => assert_eq(msg, "String contains null character")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - too long
  let long_string = "x".repeat(1001)
  let too_long_result = safe_serialize_string(long_string)
  match too_long_result {
    Ok(_) => assert_true(false)
    Error(SerializationError(msg)) => assert_eq(msg, "String too long for serialization")
    Error(_) => assert_true(false)
  }
  
  // Test array serialization with empty arrays
  let safe_serialize_array = fn(arr : Array[String]) -> Result[String] {
    if arr.length() == 0 {
      Error(SerializationError("Empty array cannot be serialized"))
    } else {
      let mut result = "["
      let mut first = true
      for item in arr {
        if first {
          result = result + "\"" + item + "\""
          first = false
        } else {
          result = result + ",\"" + item + "\""
        }
      }
      result = result + "]"
      Ok(result)
    }
  }
  
  // Valid case
  let valid_array = safe_serialize_array(["a", "b", "c"])
  match valid_array {
    Ok(v) => assert_eq(v, "[\"a\",\"b\",\"c\"]")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - empty array
  let empty_array = safe_serialize_array([])
  match empty_array {
    Ok(_) => assert_true(false)
    Error(SerializationError(msg)) => assert_eq(msg, "Empty array cannot be serialized")
    Error(_) => assert_true(false)
  }
}

// Test 3: Deserialization error handling
test "deserialization error handling" {
  // Test string deserialization with invalid format
  let safe_deserialize_string = fn(value : String) -> Result[String] {
    if value.length() >= 7 && value[:7] == "string:" {
      Ok(value[7:])
    } else {
      Error(DeserializationError("Invalid string format"))
    }
  }
  
  // Valid case
  let valid_deserialize = safe_deserialize_string("string:value")
  match valid_deserialize {
    Ok(v) => assert_eq(v, "value")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - wrong format
  let invalid_deserialize = safe_deserialize_string("wrong:format")
  match invalid_deserialize {
    Ok(_) => assert_true(false)
    Error(DeserializationError(msg)) => assert_eq(msg, "Invalid string format")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - empty string
  let empty_deserialize = safe_deserialize_string("")
  match empty_deserialize {
    Ok(_) => assert_true(false)
    Error(DeserializationError(msg)) => assert_eq(msg, "Invalid string format")
    Error(_) => assert_true(false)
  }
  
  // Test integer deserialization with invalid values
  let safe_deserialize_int = fn(value : String) -> Result[Int] {
    if value.length() >= 4 && value[:4] == "int:" {
      let int_str = value[4:]
      // Check if string contains only digits
      if int_str.length() == 0 {
        Error(DeserializationError("Empty integer value"))
      } else {
        let mut all_digits = true
        for i in 0..int_str.length() {
          let char = int_str[i]
          if char < '0' || char > '9' {
            all_digits = false
            break
          }
        }
        
        if all_digits {
          // In real implementation, would parse string to int
          Ok(42)
        } else {
          Error(DeserializationError("Invalid integer format"))
        }
      }
    } else {
      Error(DeserializationError("Invalid integer format"))
    }
  }
  
  // Valid case
  let valid_int = safe_deserialize_int("int:123")
  match valid_int {
    Ok(v) => assert_eq(v, 42)  // Using fixed value for simplicity
    Error(_) => assert_true(false)
  }
  
  // Invalid case - non-digit characters
  let invalid_int = safe_deserialize_int("int:abc")
  match invalid_int {
    Ok(_) => assert_true(false)
    Error(DeserializationError(msg)) => assert_eq(msg, "Invalid integer format")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - empty value
  let empty_int = safe_deserialize_int("int:")
  match empty_int {
    Ok(_) => assert_true(false)
    Error(DeserializationError(msg)) => assert_eq(msg, "Empty integer value")
    Error(_) => assert_true(false)
  }
}

// Test 4: Context propagation error handling
test "context propagation error handling" {
  // Test baggage entry validation
  let validate_baggage_entry = fn(key : String, value : String) -> Result[(String, String)] {
    if key.length() == 0 {
      Error(ContextPropagationError("Empty baggage key not allowed"))
    } else if key.contains("=") || key.contains(",") {
      Error(ContextPropagationError("Baggage key contains invalid characters"))
    } else if value.contains(",") {
      Error(ContextPropagationError("Baggage value contains invalid characters"))
    } else {
      Ok((key, value))
    }
  }
  
  // Valid case
  let valid_entry = validate_baggage_entry("user.id", "12345")
  match valid_entry {
    Ok((k, v)) => {
      assert_eq(k, "user.id")
      assert_eq(v, "12345")
    }
    Error(_) => assert_true(false)
  }
  
  // Invalid case - empty key
  let empty_key = validate_baggage_entry("", "value")
  match empty_key {
    Ok(_) => assert_true(false)
    Error(ContextPropagationError(msg)) => assert_eq(msg, "Empty baggage key not allowed")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - key with invalid characters
  let invalid_key = validate_baggage_entry("key=with=equals", "value")
  match invalid_key {
    Ok(_) => assert_true(false)
    Error(ContextPropagationError(msg)) => assert_eq(msg, "Baggage key contains invalid characters")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - value with invalid characters
  let invalid_value = validate_baggage_entry("key", "value,with,commas")
  match invalid_value {
    Ok(_) => assert_true(false)
    Error(ContextPropagationError(msg)) => assert_eq(msg, "Baggage value contains invalid characters")
    Error(_) => assert_true(false)
  }
  
  // Test traceparent format validation
  let validate_traceparent = fn(traceparent : String) -> Result[String] {
    let parts = traceparent.split("-")
    if parts.length() != 4 {
      Error(ContextPropagationError("Invalid traceparent format"))
    } else if parts[0] != "00" {
      Error(ContextPropagationError("Unsupported traceparent version"))
    } else if parts[1].length() != 32 {
      Error(ContextPropagationError("Invalid trace ID length"))
    } else if parts[2].length() != 16 {
      Error(ContextPropagationError("Invalid span ID length"))
    } else if parts[3].length() != 2 {
      Error(ContextPropagationError("Invalid trace flags length"))
    } else {
      Ok(traceparent)
    }
  }
  
  // Valid case
  let valid_traceparent = validate_traceparent("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  match valid_traceparent {
    Ok(v) => assert_eq(v, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - wrong number of parts
  let wrong_parts = validate_traceparent("00-trace-id")
  match wrong_parts {
    Ok(_) => assert_true(false)
    Error(ContextPropagationError(msg)) => assert_eq(msg, "Invalid traceparent format")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - wrong version
  let wrong_version = validate_traceparent("01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  match wrong_version {
    Ok(_) => assert_true(false)
    Error(ContextPropagationError(msg)) => assert_eq(msg, "Unsupported traceparent version")
    Error(_) => assert_true(false)
  }
}

// Test 5: Resource error handling
test "resource error handling" {
  // Test resource attribute limit
  let validate_resource_size = fn(resource : Resource) -> Result[Resource] {
    if resource.attributes.length() > 100 {
      Error(ResourceError("Resource has too many attributes"))
    } else {
      Ok(resource)
    }
  }
  
  // Valid case
  let valid_resource = Resource({ attributes = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0"))
  ] })
  
  let valid_result = validate_resource_size(valid_resource)
  match valid_result {
    Ok(r) => assert_eq(r.attributes.length(), 2)
    Error(_) => assert_true(false)
  }
  
  // Invalid case - too many attributes
  let mut too_many_attrs = []
  for i in 1..=101 {
    too_many_attrs = too_many_attrs @ [("key" + i.to_string(), StringValue("value" + i.to_string()))]
  }
  
  let too_big_resource = Resource({ attributes = too_many_attrs })
  let invalid_result = validate_resource_size(too_big_resource)
  match invalid_result {
    Ok(_) => assert_true(false)
    Error(ResourceError(msg)) => assert_eq(msg, "Resource has too many attributes")
    Error(_) => assert_true(false)
  }
  
  // Test resource merge with conflicting types
  let safe_merge_resources = fn(resource1 : Resource, resource2 : Resource) -> Result[Resource] {
    let mut merged_attributes = resource1.attributes
    
    for (key, value2) in resource2.attributes {
      let mut found = false
      let mut index = 0
      
      // Find if key exists in resource1
      for i in 0..resource1.attributes.length() {
        let (key1, _) = resource1.attributes[i]
        if key1 == key {
          found = true
          index = i
          break
        }
      }
      
      if found {
        let (_, value1) = resource1.attributes[index]
        
        // Check for type conflicts
        match (value1, value2) {
          (StringValue(_), StringValue(_)) => {
            merged_attributes[index] = (key, value2)
          }
          (IntValue(_), IntValue(_)) => {
            merged_attributes[index] = (key, value2)
          }
          (BoolValue(_), BoolValue(_)) => {
            merged_attributes[index] = (key, value2)
          }
          (FloatValue(_), FloatValue(_)) => {
            merged_attributes[index] = (key, value2)
          }
          (ArrayStringValue(_), ArrayStringValue(_)) => {
            merged_attributes[index] = (key, value2)
          }
          (ArrayIntValue(_), ArrayIntValue(_)) => {
            merged_attributes[index] = (key, value2)
          }
          _ => {
            return Error(ResourceError("Type conflict in resource merge"))
          }
        }
      } else {
        merged_attributes = merged_attributes @ [(key, value2)]
      }
    }
    
    Ok(Resource({ attributes = merged_attributes }))
  }
  
  // Valid case - same types
  let resource1 = Resource({ attributes = [
    ("service.name", StringValue("service1")),
    ("service.port", IntValue(8080))
  ] })
  
  let resource2 = Resource({ attributes = [
    ("service.name", StringValue("service2")),  // Same type
    ("service.ssl", BoolValue(true))
  ] })
  
  let valid_merge = safe_merge_resources(resource1, resource2)
  match valid_merge {
    Ok(r) => assert_eq(r.attributes.length(), 3)
    Error(_) => assert_true(false)
  }
  
  // Invalid case - type conflict
  let resource3 = Resource({ attributes = [
    ("service.name", IntValue(123))  // Different type
  ] })
  
  let invalid_merge = safe_merge_resources(resource1, resource3)
  match invalid_merge {
    Ok(_) => assert_true(false)
    Error(ResourceError(msg)) => assert_eq(msg, "Type conflict in resource merge")
    Error(_) => assert_true(false)
  }
}

// Test 6: Span error handling
test "span error handling" {
  // Test span timing validation
  let validate_span_timing = fn(span : Span) -> Result[Span] {
    match span.end_time {
      None => Ok(span)  // Active span
      Some(end_time) => {
        if end_time < span.start_time {
          Error(SpanError("Span end time before start time"))
        } else {
          Ok(span)
        }
      }
    }
  }
  
  let span_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-111111111",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  // Valid case - active span
  let active_span = Span({
    name: "test-span",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time: 1640995200000,
    end_time: None,
    status: Unset,
    attributes: Attributes({ values = [] }),
    events: [],
    links: []
  })
  
  let valid_active = validate_span_timing(active_span)
  match valid_active {
    Ok(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Valid case - completed span with valid timing
  let completed_span = Span({
    name: "test-span",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time: 1640995200000,
    end_time: Some(1640995200500),
    status: Ok,
    attributes: Attributes({ values = [] }),
    events: [],
    links: []
  })
  
  let valid_completed = validate_span_timing(completed_span)
  match valid_completed {
    Ok(_) => assert_true(true)
    Error(_) => assert_true(false)
  }
  
  // Invalid case - end time before start time
  let invalid_span = Span({
    name: "test-span",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time: 1640995200000,
    end_time: Some(1640995199000),  // Before start time
    status: Ok,
    attributes: Attributes({ values = [] }),
    events: [],
    links: []
  })
  
  let invalid_timing = validate_span_timing(invalid_span)
  match invalid_timing {
    Ok(_) => assert_true(false)
    Error(SpanError(msg)) => assert_eq(msg, "Span end time before start time")
    Error(_) => assert_true(false)
  }
  
  // Test span name validation
  let validate_span_name = fn(name : String) -> Result[String] {
    if name.length() == 0 {
      Error(SpanError("Empty span name not allowed"))
    } else if name.length() > 128 {
      Error(SpanError("Span name too long"))
    } else {
      Ok(name)
    }
  }
  
  // Valid case
  let valid_name = validate_span_name("valid-span-name")
  match valid_name {
    Ok(n) => assert_eq(n, "valid-span-name")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - empty name
  let empty_name = validate_span_name("")
  match empty_name {
    Ok(_) => assert_true(false)
    Error(SpanError(msg)) => assert_eq(msg, "Empty span name not allowed")
    Error(_) => assert_true(false)
  }
  
  // Invalid case - too long
  let long_name = "x".repeat(129)
  let too_long_name = validate_span_name(long_name)
  match too_long_name {
    Ok(_) => assert_true(false)
    Error(SpanError(msg)) => assert_eq(msg, "Span name too long")
    Error(_) => assert_true(false)
  }
}

// Test 7: Error recovery mechanisms
test "error recovery mechanisms" {
  // Test fallback value mechanism
  let get_with_fallback = fn(result : Result[String], fallback : String) -> String {
    match result {
      Ok(value) => value
      Error(_) => fallback
    }
  }
  
  let valid_result = Ok("valid_value")
  let valid_with_fallback = get_with_fallback(valid_result, "fallback")
  assert_eq(valid_with_fallback, "valid_value")
  
  let invalid_result = Error(InvalidAttribute("Invalid"))
  let invalid_with_fallback = get_with_fallback(invalid_result, "fallback")
  assert_eq(invalid_with_fallback, "fallback")
  
  // Test retry mechanism
  let retry_operation = fn(operation : () -> Result[String], max_attempts : Int) -> Result[String] {
    let mut attempts = 0
    let mut last_error = Error(InvalidAttribute("Unknown"))
    
    while attempts < max_attempts {
      let result = operation()
      match result {
        Ok(value) => return Ok(value)
        Error(e) => {
          last_error = e
          attempts = attempts + 1
        }
      }
    }
    
    last_error
  }
  
  let mut attempt_count = 0
  let sometimes_fails = fn() -> Result[String] {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error(InvalidAttribute("Failed attempt " + attempt_count.to_string()))
    } else {
      Ok("success")
    }
  }
  
  let retry_result = retry_operation(sometimes_fails, 5)
  match retry_result {
    Ok(value) => assert_eq(value, "success")
    Error(_) => assert_true(false)
  }
  
  // Reset for next test
  attempt_count = 0
  
  let always_fails = fn() -> Result[String] {
    attempt_count = attempt_count + 1
    Error(InvalidAttribute("Always fails"))
  }
  
  let fail_retry = retry_operation(always_fails, 3)
  match fail_retry {
    Ok(_) => assert_true(false)
    Error(InvalidAttribute(msg)) => assert_eq(msg, "Always fails")
    Error(_) => assert_true(false)
  }
  
  // Test partial recovery
  let safe_process_array = fn(arr : Array[String]) -> Result[Array[String]] {
    let mut results = []
    let mut has_errors = false
    
    for item in arr {
      if item.length() > 0 {
        results = results @ [item]
      } else {
        has_errors = true
      }
    }
    
    if has_errors {
      Error(InvalidAttribute("Some items were empty"))
    } else {
      Ok(results)
    }
  }
  
  let valid_array = ["a", "b", "c"]
  let valid_process = safe_process_array(valid_array)
  match valid_process {
    Ok(result) => {
      assert_eq(result.length(), 3)
      assert_eq(result[0], "a")
      assert_eq(result[1], "b")
      assert_eq(result[2], "c")
    }
    Error(_) => assert_true(false)
  }
  
  let mixed_array = ["a", "", "c"]
  let mixed_process = safe_process_array(mixed_array)
  match mixed_process {
    Ok(_) => assert_true(false)
    Error(InvalidAttribute(msg)) => assert_eq(msg, "Some items were empty")
    Error(_) => assert_true(false)
  }
}

// Test 8: Error boundary isolation
test "error boundary isolation" {
  // Test that errors in one component don't affect others
  let process_component1 = fn() -> Result[String] {
    // This component always succeeds
    Ok("component1_success")
  }
  
  let process_component2 = fn() -> Result[String] {
    // This component always fails
    Error(SerializationError("Component2 failed"))
  }
  
  let process_component3 = fn() -> Result[String] {
    // This component always succeeds
    Ok("component3_success")
  }
  
  // Process all components with isolation
  let result1 = process_component1()
  let result2 = process_component2()
  let result3 = process_component3()
  
  // Component 1 should succeed
  match result1 {
    Ok(value) => assert_eq(value, "component1_success")
    Error(_) => assert_true(false)
  }
  
  // Component 2 should fail
  match result2 {
    Ok(_) => assert_true(false)
    Error(SerializationError(msg)) => assert_eq(msg, "Component2 failed")
    Error(_) => assert_true(false)
  }
  
  // Component 3 should succeed despite component 2 failure
  match result3 {
    Ok(value) => assert_eq(value, "component3_success")
    Error(_) => assert_true(false)
  }
  
  // Test error aggregation
  let process_all_components = fn() -> Array[Result[String]] {
    [result1, result2, result3]
  }
  
  let all_results = process_all_components()
  assert_eq(all_results.length(), 3)
  
  // Count successes and failures
  let mut success_count = 0
  let mut error_count = 0
  
  for result in all_results {
    match result {
      Ok(_) => success_count = success_count + 1
      Error(_) => error_count = error_count + 1
    }
  }
  
  assert_eq(success_count, 2)
  assert_eq(error_count, 1)
}

// Test 9: Graceful degradation
test "graceful degradation" {
  // Test fallback to simpler functionality
  let advanced_feature = fn(input : String) -> Result[String] {
    if input.contains("complex") {
      Error(InvalidAttribute("Complex input not supported"))
    } else {
      Ok("Advanced processing: " + input)
    }
  }
  
  let basic_feature = fn(input : String) -> String {
    "Basic processing: " + input
  }
  
  let process_with_fallback = fn(input : String) -> String {
    match advanced_feature(input) {
      Ok(result) => result
      Error(_) => basic_feature(input)
    }
  }
  
  // Test with simple input (should use advanced feature)
  let simple_result = process_with_fallback("simple input")
  assert_eq(simple_result, "Advanced processing: simple input")
  
  // Test with complex input (should fall back to basic feature)
  let complex_result = process_with_fallback("complex input")
  assert_eq(complex_result, "Basic processing: complex input")
  
  // Test partial functionality degradation
  let process_multiple_features = fn(inputs : Array[String]) -> Array[String] {
    let mut results = []
    
    for input in inputs {
      let result = match advanced_feature(input) {
        Ok(advanced) => advanced
        Error(_) => basic_feature(input)
      }
      results = results @ [result]
    }
    
    results
  }
  
  let mixed_inputs = ["simple1", "complex1", "simple2", "complex2"]
  let mixed_results = process_multiple_features(mixed_inputs)
  
  assert_eq(mixed_results.length(), 4)
  assert_eq(mixed_results[0], "Advanced processing: simple1")
  assert_eq(mixed_results[1], "Basic processing: complex1")
  assert_eq(mixed_results[2], "Advanced processing: simple2")
  assert_eq(mixed_results[3], "Basic processing: complex2")
}

// Test 10: Error logging and monitoring
test "error logging and monitoring" {
  // Test error categorization and logging
  let categorize_and_log_error = fn(error : TelemetryError) -> String {
    match error {
      InvalidAttribute(msg) => "ATTR_ERROR: " + msg
      SerializationError(msg) => "SER_ERROR: " + msg
      DeserializationError(msg) => "DESER_ERROR: " + msg
      ContextPropagationError(msg) => "CTX_ERROR: " + msg
      ResourceError(msg) => "RES_ERROR: " + msg
      SpanError(msg) => "SPAN_ERROR: " + msg
    }
  }
  
  let attr_error = InvalidAttribute("Invalid attribute")
  let attr_log = categorize_and_log_error(attr_error)
  assert_eq(attr_log, "ATTR_ERROR: Invalid attribute")
  
  let ser_error = SerializationError("Serialization failed")
  let ser_log = categorize_and_log_error(ser_error)
  assert_eq(ser_log, "SER_ERROR: Serialization failed")
  
  let span_error = SpanError("Span error")
  let span_log = categorize_and_log_error(span_error)
  assert_eq(span_log, "SPAN_ERROR: Span error")
  
  // Test error metrics collection
  let mut error_counts = {
    "InvalidAttribute": 0,
    "SerializationError": 0,
    "DeserializationError": 0,
    "ContextPropagationError": 0,
    "ResourceError": 0,
    "SpanError": 0
  }
  
  let record_error = fn(error : TelemetryError) -> Unit {
    match error {
      InvalidAttribute(_) => error_counts["InvalidAttribute"] = error_counts["InvalidAttribute"] + 1
      SerializationError(_) => error_counts["SerializationError"] = error_counts["SerializationError"] + 1
      DeserializationError(_) => error_counts["DeserializationError"] = error_counts["DeserializationError"] + 1
      ContextPropagationError(_) => error_counts["ContextPropagationError"] = error_counts["ContextPropagationError"] + 1
      ResourceError(_) => error_counts["ResourceError"] = error_counts["ResourceError"] + 1
      SpanError(_) => error_counts["SpanError"] = error_counts["SpanError"] + 1
    }
  }
  
  // Record various errors
  record_error(InvalidAttribute("Error 1"))
  record_error(InvalidAttribute("Error 2"))
  record_error(SerializationError("Error 3"))
  record_error(SpanError("Error 4"))
  record_error(SpanError("Error 5"))
  record_error(SpanError("Error 6"))
  
  // Verify error counts
  assert_eq(error_counts["InvalidAttribute"], 2)
  assert_eq(error_counts["SerializationError"], 1)
  assert_eq(error_counts["DeserializationError"], 0)
  assert_eq(error_counts["ContextPropagationError"], 0)
  assert_eq(error_counts["ResourceError"], 0)
  assert_eq(error_counts["SpanError"], 3)
}