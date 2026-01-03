// Azimuth Telemetry System - Memory Leak Protection Tests
// This file contains comprehensive memory leak protection and optimization test cases

// Test 1: Span Memory Leak Detection
test "span memory leak detection" {
  let initial_memory = memory::used()
  
  // Create and destroy spans in a loop
  for iteration in 0..10 {
    let spans = []
    
    // Create many spans
    for i in 0..1000 {
      let trace_id = "leak_test_trace_" + iteration.to_string() + "_" + i.to_string()
      let span_id = "leak_test_span_" + iteration.to_string() + "_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "leak_test")
      let span = Span::new("leak_test_span", Internal, span_ctx)
      
      // Add events and attributes
      Span::add_event(span, "leak_test_event", Some(Attributes::with([
        ("iteration", IntValue(iteration)),
        ("index", IntValue(i)),
        ("data", StringValue("test_data_" + i.to_string()))
      ])))
      
      spans.push(span)
    }
    
    // End all spans
    for span in spans {
      Span::end(span)
    }
    
    // Clear span array
    spans = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 10MB)
  assert_true(memory_growth < 10 * 1024 * 1024)
}

// Test 2: Attribute Memory Leak Detection
test "attribute memory leak detection" {
  let initial_memory = memory::used()
  
  // Create and destroy attributes in a loop
  for iteration in 0..20 {
    let attributes_collection = []
    
    // Create many attribute collections
    for i in 0..500 {
      let attrs = Attributes::new()
      
      // Add many attributes to each collection
      for j in 0..50 {
        let key = "leak_test_key_" + iteration.to_string() + "_" + i.to_string() + "_" + j.to_string()
        let value = StringValue("leak_test_value_" + iteration.to_string() + "_" + i.to_string() + "_" + j.to_string())
        Attributes::set(attrs, key, value)
      }
      
      attributes_collection.push(attrs)
    }
    
    // Access all attributes
    for (i, attrs) in attributes_collection.enumerate() {
      for j in 0..50 {
        let key = "leak_test_key_" + iteration.to_string() + "_" + i.to_string() + "_" + j.to_string()
        let expected_value = "leak_test_value_" + iteration.to_string() + "_" + i.to_string() + "_" + j.to_string()
        
        let retrieved = Attributes::get(attrs, key)
        match retrieved {
          Some(StringValue(v)) => assert_eq(v, expected_value)
          None => assert_true(false)
        }
      }
    }
    
    // Clear attributes collection
    attributes_collection = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 20MB due to many strings)
  assert_true(memory_growth < 20 * 1024 * 1024)
}

// Test 3: Context Memory Leak Detection
test "context memory leak detection" {
  let initial_memory = memory::used()
  
  // Create and destroy contexts in a loop
  for iteration in 0..15 {
    let contexts = []
    
    // Create nested contexts
    for i in 0..200 {
      let base_ctx = if i == 0 { Context::root() } else { contexts[i-1] }
      let key = ContextKey::new("leak_test_key_" + iteration.to_string() + "_" + i.to_string())
      let value = "leak_test_value_" + iteration.to_string() + "_" + i.to_string()
      let new_ctx = Context::with_value(base_ctx, key, value)
      contexts.push(new_ctx)
    }
    
    // Access all context values
    for (i, ctx) in contexts.enumerate() {
      for j in 0..=i {
        let key = ContextKey::new("leak_test_key_" + iteration.to_string() + "_" + j.to_string())
        let expected_value = "leak_test_value_" + iteration.to_string() + "_" + j.to_string()
        
        let retrieved = Context::get(ctx, key)
        match retrieved {
          Some(value) => assert_eq(value, expected_value)
          None => assert_true(false)
        }
      }
    }
    
    // Clear contexts
    contexts = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 15MB)
  assert_true(memory_growth < 15 * 1024 * 1024)
}

// Test 4: Metrics Memory Leak Detection
test "metrics memory leak detection" {
  let initial_memory = memory::used()
  
  // Create and destroy metrics in a loop
  for iteration in 0..10 {
    let providers = []
    let meters = []
    let instruments = []
    
    // Create many providers, meters, and instruments
    for i in 0..100 {
      let provider = MeterProvider::default()
      providers.push(provider)
      
      let meter = MeterProvider::get_meter(provider, "leak_test_meter_" + iteration.to_string() + "_" + i.to_string())
      meters.push(meter)
      
      let counter = Meter::create_counter(meter, "leak_test_counter_" + i.to_string(), None, None)
      let histogram = Meter::create_histogram(meter, "leak_test_histogram_" + i.to_string(), None, None)
      let updown_counter = Meter::create_updown_counter(meter, "leak_test_updown_" + i.to_string(), None, None)
      let gauge = Meter::create_gauge(meter, "leak_test_gauge_" + i.to_string(), None, None)
      
      instruments.push(counter)
      instruments.push(histogram)
      instruments.push(updown_counter)
      instruments.push(gauge)
    }
    
    // Use all instruments
    for (i, instrument) in instruments.enumerate() {
      match i % 4 {
        0 => Counter::add(instrument, 1.0)
        1 => Histogram::record(instrument, 100.0)
        2 => UpDownCounter::add(instrument, 1.0)
        3 => Gauge::record(instrument, 42.0)
        _ => assert_true(false)
      }
    }
    
    // Shutdown all providers
    for provider in providers {
      MeterProvider::shutdown(provider)
    }
    
    // Clear arrays
    providers = []
    meters = []
    instruments = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 10MB)
  assert_true(memory_growth < 10 * 1024 * 1024)
}

// Test 5: Logger Memory Leak Detection
test "logger memory leak detection" {
  let initial_memory = memory::used()
  
  // Create and destroy loggers in a loop
  for iteration in 0..10 {
    let providers = []
    let loggers = []
    let log_records = []
    
    // Create many providers and loggers
    for i in 0..100 {
      let provider = LoggerProvider::default()
      providers.push(provider)
      
      let logger = LoggerProvider::get_logger(provider, "leak_test_logger_" + iteration.to_string() + "_" + i.to_string())
      loggers.push(logger)
      
      // Create log records
      for j in 0..10 {
        let log_record = LogRecord::new_with_context(
          Info,
          Some("Leak test log message " + iteration.to_string() + "_" + i.to_string() + "_" + j.to_string()),
          Some(Attributes::with([
            ("iteration", IntValue(iteration)),
            ("logger_index", IntValue(i)),
            ("log_index", IntValue(j)),
            ("data", StringValue("test_data_" + j.to_string()))
          ])),
          Some((iteration * 1000000 + i * 10000 + j * 100).to_int64()),
          Some((iteration * 1000000 + i * 10000 + j * 100 + 1).to_int64()),
          Some("leak_test_trace_" + i.to_string()),
          Some("leak_test_span_" + i.to_string()),
          Some(Context::root())
        )
        log_records.push(log_record)
      }
    }
    
    // Emit all log records
    for (i, logger) in loggers.enumerate() {
      for j in 0..10 {
        let log_record_index = i * 10 + j
        if log_record_index < log_records.length() {
          Logger::emit(logger, log_records[log_record_index])
        }
      }
    }
    
    // Shutdown all providers
    for provider in providers {
      LoggerProvider::shutdown(provider)
    }
    
    // Clear arrays
    providers = []
    loggers = []
    log_records = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 15MB due to log strings)
  assert_true(memory_growth < 15 * 1024 * 1024)
}

// Test 6: Large Object Memory Management
test "large object memory management" {
  let initial_memory = memory::used()
  
  // Create and destroy large objects
  for iteration in 0..5 {
    let large_objects = []
    
    // Create large string objects
    for i in 0..50 {
      let large_string = "x".repeat(100000) // 100KB string
      let attrs = Attributes::new()
      
      // Add large attribute
      Attributes::set(attrs, "large_attr_" + i.to_string(), StringValue(large_string))
      
      // Add many more attributes
      for j in 0..20 {
        let key = "attr_" + i.to_string() + "_" + j.to_string()
        let value = StringValue("value_" + i.to_string() + "_" + j.to_string())
        Attributes::set(attrs, key, value)
      }
      
      large_objects.push(attrs)
    }
    
    // Access large attributes
    for (i, attrs) in large_objects.enumerate() {
      let large_attr = Attributes::get(attrs, "large_attr_" + i.to_string())
      match large_attr {
        Some(StringValue(v)) => assert_eq(v.length(), 100000)
        None => assert_true(false)
      }
    }
    
    // Clear large objects
    large_objects = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 50MB due to large strings)
  assert_true(memory_growth < 50 * 1024 * 1024)
}

// Test 7: Memory Pressure Handling
test "memory pressure handling" {
  let initial_memory = memory::used()
  
  // Create objects under memory pressure
  let pressure_objects = []
  
  // Create many objects to simulate memory pressure
  for i in 0..1000 {
    let attrs = Attributes::new()
    
    // Add many attributes to each object
    for j in 0..10 {
      let key = "pressure_key_" + i.to_string() + "_" + j.to_string()
      let value = StringValue("pressure_value_" + i.to_string() + "_" + j.to_string())
      Attributes::set(attrs, key, value)
    }
    
    pressure_objects.push(attrs)
    
    // Periodically clean up to test memory pressure handling
    if i % 100 == 99 {
      // Remove some objects
      for k in 0..50 {
        if pressure_objects.length() > 0 {
          pressure_objects.pop()
        }
      }
      
      // Force garbage collection if available
      memory::gc()
    }
  }
  
  // Final cleanup
  pressure_objects = []
  memory::gc()
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 30MB)
  assert_true(memory_growth < 30 * 1024 * 1024)
}

// Test 8: Circular Reference Memory Leak Detection
test "circular reference memory leak detection" {
  let initial_memory = memory::used()
  
  // Create objects with potential circular references
  for iteration in 0..10 {
    let circular_objects = []
    
    // Create objects that might have circular references
    for i in 0..100 {
      let attrs1 = Attributes::new()
      let attrs2 = Attributes::new()
      
      // Create cross-references (simulated)
      Attributes::set(attrs1, "ref_to_2", StringValue("attrs2_" + i.to_string()))
      Attributes::set(attrs2, "ref_to_1", StringValue("attrs1_" + i.to_string()))
      
      // Add self-references (simulated)
      Attributes::set(attrs1, "self_ref", StringValue("attrs1_" + i.to_string()))
      Attributes::set(attrs2, "self_ref", StringValue("attrs2_" + i.to_string()))
      
      circular_objects.push((attrs1, attrs2))
    }
    
    // Access circular references
    for (i, (attrs1, attrs2)) in circular_objects.enumerate() {
      let ref1 = Attributes::get(attrs1, "ref_to_2")
      let ref2 = Attributes::get(attrs2, "ref_to_1")
      let self1 = Attributes::get(attrs1, "self_ref")
      let self2 = Attributes::get(attrs2, "self_ref")
      
      match ref1 {
        Some(StringValue(v)) => assert_eq(v, "attrs2_" + i.to_string())
        None => assert_true(false)
      }
      
      match ref2 {
        Some(StringValue(v)) => assert_eq(v, "attrs1_" + i.to_string())
        None => assert_true(false)
      }
      
      match self1 {
        Some(StringValue(v)) => assert_eq(v, "attrs1_" + i.to_string())
        None => assert_true(false)
      }
      
      match self2 {
        Some(StringValue(v)) => assert_eq(v, "attrs2_" + i.to_string())
        None => assert_true(false)
      }
    }
    
    // Clear circular objects
    circular_objects = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 10MB)
  assert_true(memory_growth < 10 * 1024 * 1024)
}

// Test 9: Memory Fragmentation Test
test "memory fragmentation test" {
  let initial_memory = memory::used()
  
  // Create objects of varying sizes to test fragmentation
  for iteration in 0..10 {
    let fragment_objects = []
    
    // Create objects of different sizes
    for i in 0..200 {
      let attrs = Attributes::new()
      
      // Vary the number and size of attributes
      let attr_count = (i % 10) + 1
      let string_size = (i % 1000) + 100
      
      for j in 0..attr_count {
        let key = "frag_key_" + i.to_string() + "_" + j.to_string()
        let value = "x".repeat(string_size) + "value_" + i.to_string() + "_" + j.to_string()
        Attributes::set(attrs, key, StringValue(value))
      }
      
      fragment_objects.push(attrs)
    }
    
    // Randomly remove objects to create fragmentation
    for i in 0..100 {
      if fragment_objects.length() > 0 && i % 3 == 0 {
        let index = i % fragment_objects.length()
        fragment_objects.remove(index)
      }
    }
    
    // Add more objects
    for i in 0..50 {
      let attrs = Attributes::new()
      Attributes::set(attrs, "frag_additional", StringValue("additional_value_" + i.to_string()))
      fragment_objects.push(attrs)
    }
    
    // Clear all objects
    fragment_objects = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 25MB due to varying string sizes)
  assert_true(memory_growth < 25 * 1024 * 1024)
}

// Test 10: Comprehensive Memory Stress Test
test "comprehensive memory stress test" {
  let initial_memory = memory::used()
  
  // Comprehensive memory stress test with all object types
  for iteration in 0..5 {
    let spans = []
    let attributes = []
    let contexts = []
    let providers = []
    let loggers = []
    let resources = []
    
    // Create mixed object types
    for i in 0..200 {
      // Spans
      let trace_id = "stress_trace_" + iteration.to_string() + "_" + i.to_string()
      let span_id = "stress_span_" + iteration.to_string() + "_" + i.to_string()
      let span_ctx = SpanContext::new(trace_id, span_id, true, "stress_test")
      let span = Span::new("stress_span", Internal, span_ctx)
      spans.push(span)
      
      // Attributes
      let attrs = Attributes::new()
      for j in 0..10 {
        let key = "stress_key_" + i.to_string() + "_" + j.to_string()
        let value = StringValue("stress_value_" + i.to_string() + "_" + j.to_string())
        Attributes::set(attrs, key, value)
      }
      attributes.push(attrs)
      
      // Contexts
      let ctx = Context::with_value(
        Context::root(),
        ContextKey::new("stress_key_" + i.to_string()),
        "stress_value_" + i.to_string()
      )
      contexts.push(ctx)
      
      // Providers
      if i % 10 == 0 {
        let meter_provider = MeterProvider::default()
        let logger_provider = LoggerProvider::default()
        providers.push((meter_provider, logger_provider))
        
        let meter = MeterProvider::get_meter(meter_provider, "stress_meter_" + i.to_string())
        let logger = LoggerProvider::get_logger(logger_provider, "stress_logger_" + i.to_string())
        
        let counter = Meter::create_counter(meter, "stress_counter", None, None)
        Counter::add(counter, 1.0)
        
        let log_record = LogRecord::new(Info, "Stress log message " + i.to_string())
        Logger::emit(logger, log_record)
        
        loggers.push((meter, logger))
      }
      
      // Resources
      if i % 5 == 0 {
        let resource = Resource::new()
        let resource_attrs = [
          ("stress.resource.id", IntValue(i)),
          ("stress.resource.name", StringValue("stress_resource_" + i.to_string()))
        ]
        let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
        resources.push(resource_with_attrs)
      }
    }
    
    // Use all objects
    for (i, span) in spans.enumerate() {
      Span::add_event(span, "stress_event", Some(attributes[i % attributes.length()]))
    }
    
    // Clean up all objects
    for span in spans {
      Span::end(span)
    }
    
    for (meter_provider, logger_provider) in providers {
      MeterProvider::shutdown(meter_provider)
      LoggerProvider::shutdown(logger_provider)
    }
    
    spans = []
    attributes = []
    contexts = []
    providers = []
    loggers = []
    resources = []
    
    // Force garbage collection if available
    memory::gc()
  }
  
  let final_memory = memory::used()
  let memory_growth = final_memory - initial_memory
  
  // Memory growth should be minimal (less than 40MB)
  assert_true(memory_growth < 40 * 1024 * 1024)
}