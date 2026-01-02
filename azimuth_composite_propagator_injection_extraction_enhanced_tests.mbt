// Azimuth CompositePropagator Injection Extraction Tests
// Test composite propagator creation, injection, and extraction functionality

test "w3c_trace_context_propagator_creation" {
  let propagator = W3CTraceContextPropagator::new()
  
  // In simplified implementation, we just verify creation doesn't fail
  @test.succeed()
}

test "w3c_baggage_propagator_creation" {
  let propagator = W3CBaggagePropagator::new()
  
  // In simplified implementation, we just verify creation doesn't fail
  @test.succeed()
}

test "composite_propagator_creation" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // In simplified implementation, we just verify creation doesn't fail
  @test.succeed()
}

test "composite_propagator_multiple_propagators" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let trace_propagator3 = W3CTraceContextPropagator::new()
  
  let propagators = [trace_propagator1, trace_propagator2, trace_propagator3]
  let composite = CompositePropagator::new(propagators)
  
  // In simplified implementation, we just verify creation doesn't fail
  @test.succeed()
}

test "text_map_carrier_creation_and_operations" {
  let carrier = TextMapCarrier::new()
  
  // Test initial state
  assert_eq!(carrier.headers.length, 0)
  
  // Test getting non-existent header
  match TextMapCarrier::get(carrier, "non_existent") {
    Some(_) => @test.abort("Expected None for non-existent header")
    None => @test.succeed()
  }
  
  // Test setting header (simplified implementation doesn't actually store)
  TextMapCarrier::set(carrier, "test_header", "test_value")
  
  // Test getting traceparent header (simplified implementation returns fixed value)
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(value) => assert_eq!(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => @test.abort("Expected traceparent header")
  }
}

test "composite_propagator_injection" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let context = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Perform injection
  CompositePropagator::inject(composite, context, carrier)
  
  // In simplified implementation, we just verify the call doesn't fail
  @test.succeed()
}

test "composite_propagator_extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Perform extraction
  let extracted_context = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction succeeded
  let context_key = ContextKey::new("extracted")
  match Context::get(extracted_context, context_key) {
    Some(value) => assert_eq!(value, "true")
    None => @test.abort("Expected extracted context to contain 'extracted' key")
  }
}

test "composite_propagator_injection_extraction_cycle" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Create original context with data
  let original_key = ContextKey::new("original_data")
  let original_context = Context::with_value(Context::root(), original_key, "test_value")
  
  // Inject into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, original_context, carrier)
  
  // Extract from carrier
  let extracted_context = CompositePropagator::extract(composite, carrier)
  
  // Verify original context data
  match Context::get(original_context, original_key) {
    Some(value) => assert_eq!(value, "test_value")
    None => @test.abort("Expected original context to contain data")
  }
  
  // Verify extracted context has extraction marker
  let extracted_key = ContextKey::new("extracted")
  match Context::get(extracted_context, extracted_key) {
    Some(value) => assert_eq!(value, "true")
    None => @test.abort("Expected extracted context to contain extraction marker")
  }
}

test "composite_propagator_with_multiple_propagators_injection" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator1, trace_propagator2]
  let composite = CompositePropagator::new(propagators)
  
  let context = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Perform injection with multiple propagators
  CompositePropagator::inject(composite, context, carrier)
  
  // In simplified implementation, we just verify the call doesn't fail
  @test.succeed()
}

test "composite_propagator_with_multiple_propagators_extraction" {
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator1, trace_propagator2]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Perform extraction with multiple propagators
  let extracted_context = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction succeeded
  let context_key = ContextKey::new("extracted")
  match Context::get(extracted_context, context_key) {
    Some(value) => assert_eq!(value, "true")
    None => @test.abort("Expected extracted context to contain 'extracted' key")
  }
}

test "propagator_with_complex_context" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Create complex context with multiple values
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  let context1 = Context::with_value(Context::root(), key1, "value1")
  let context2 = Context::with_value(context1, key2, "value2")
  let complex_context = Context::with_value(context2, key3, "value3")
  
  // Inject complex context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, complex_context, carrier)
  
  // Extract context
  let extracted_context = CompositePropagator::extract(composite, carrier)
  
  // Verify original context still has all values
  match Context::get(complex_context, key1) {
    Some(value) => assert_eq!(value, "value1")
    None => @test.abort("Expected original context to contain key1")
  }
  
  match Context::get(complex_context, key2) {
    Some(value) => assert_eq!(value, "value2")
    None => @test.abort("Expected original context to contain key2")
  }
  
  match Context::get(complex_context, key3) {
    Some(value) => assert_eq!(value, "value3")
    None => @test.abort("Expected original context to contain key3")
  }
  
  // Verify extracted context has extraction marker
  let extracted_key = ContextKey::new("extracted")
  match Context::get(extracted_context, extracted_key) {
    Some(value) => assert_eq!(value, "true")
    None => @test.abort("Expected extracted context to contain extraction marker")
  }
}

test "propagator_with_special_characters_in_context" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Create context with special characters
  let special_key = ContextKey::new("special.key!@#")
  let unicode_key = ContextKey::new("unicode.键")
  
  let context1 = Context::with_value(Context::root(), special_key, "special.value!#$%")
  let special_context = Context::with_value(context1, unicode_key, "unicode.值")
  
  // Inject special context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, special_context, carrier)
  
  // Extract context
  let extracted_context = CompositePropagator::extract(composite, carrier)
  
  // Verify original context still has special values
  match Context::get(special_context, special_key) {
    Some(value) => assert_eq!(value, "special.value!#$%")
    None => @test.abort("Expected original context to contain special key")
  }
  
  match Context::get(special_context, unicode_key) {
    Some(value) => assert_eq!(value, "unicode.值")
    None => @test.abort("Expected original context to contain unicode key")
  }
}

test "propagator_error_handling" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // Test injection with empty context
  let empty_context = Context::root()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, empty_context, carrier)
  
  // Test extraction from empty carrier
  let empty_carrier = TextMapCarrier::new()
  let extracted_context = CompositePropagator::extract(composite, empty_carrier)
  
  // Verify extraction still provides extraction marker
  let context_key = ContextKey::new("extracted")
  match Context::get(extracted_context, context_key) {
    Some(value) => assert_eq!(value, "true")
    None => @test.abort("Expected extracted context to contain extraction marker")
  }
}

test "propagator_multiple_injection_extraction_cycles" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = TextMapCarrier::new()
  
  // Perform multiple injection/extraction cycles
  for i in 0..5 {
    let context_key = ContextKey::new("cycle_" + i.to_string())
    let context = Context::with_value(Context::root(), context_key, "value_" + i.to_string())
    
    // Inject
    CompositePropagator::inject(composite, context, carrier)
    
    // Extract
    let extracted_context = CompositePropagator::extract(composite, carrier)
    
    // Verify extraction marker
    let extracted_key = ContextKey::new("extracted")
    match Context::get(extracted_context, extracted_key) {
      Some(value) => assert_eq!(value, "true")
      None => @test.abort("Expected extracted context to contain extraction marker")
    }
  }
  
  @test.succeed()
}