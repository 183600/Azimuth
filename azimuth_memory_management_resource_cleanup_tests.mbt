// Memory Management and Resource Cleanup Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Span Memory Management
test "span memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create a large number of spans
  let mut spans = []
  for i in 0..10000 {
    let span_name = "memory_test_span_" + i.to_string()
    let span_ctx = SpanContext::new("memory_test_trace", "span_" + i.to_string(), true, "")
    let span = Span::new(span_name, Internal, span_ctx)
    
    // Add attributes and events
    Span::add_attribute(span, "index", IntValue(i))
    Span::add_event(span, "test_event", Some([("index", IntValue(i))]))
    
    spans = spans + [span]
  }
  
  // Check memory after span creation
  let after_creation = MemoryTracker::get_current_usage(memory_tracker)
  let memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_creation)
  
  // Verify memory usage is reasonable
  let memory_per_span = memory_increase / 10000
  assert_true(memory_per_span < 1000) // Less than 1KB per span
  
  // Clear references to spans
  spans = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let memory_decrease = MemoryTracker::calculate_decrease(after_creation, after_cleanup)
  
  // Verify most memory is reclaimed
  let reclamation_rate = (memory_decrease as Float) / (memory_increase as Float)
  assert_true(reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
  
  // Verify no memory leaks
  let final_memory = MemoryTracker::get_current_usage(memory_tracker)
  let final_increase = MemoryTracker::calculate_increase(baseline_memory, final_memory)
  assert_true(final_increase < 1024 * 1024) // Less than 1MB increase from baseline
}

// Test 2: Metrics Memory Management
test "metrics memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create metrics provider with many instruments
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "memory_test_meter")
  
  let mut counters = []
  let mut histograms = []
  let mut updown_counters = []
  
  // Create many metric instruments
  for i in 0..1000 {
    let counter = Meter::create_counter(meter, "counter_" + i.to_string(), None, None)
    counters = counters + [counter]
    
    let histogram = Meter::create_histogram(meter, "histogram_" + i.to_string(), None, None)
    histograms = histograms + [histogram]
    
    let updown_counter = Meter::create_updown_counter(meter, "updown_" + i.to_string(), None, None)
    updown_counters = updown_counters + [updown_counter]
  }
  
  // Record many metric values
  for i in 0..1000 {
    for j in 0..100 {
      Counter::add(counters[i], j.to_int() as Float)
      Histogram::record(histograms[i], j.to_int() as Float)
      UpDownCounter::add(updown_counters[i], j.to_int() as Float)
    }
  }
  
  // Check memory after metrics operations
  let after_metrics = MemoryTracker::get_current_usage(memory_tracker)
  let metrics_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_metrics)
  
  // Verify memory usage is reasonable
  let memory_per_instrument = metrics_memory_increase / 3000 // 1000 of each type
  assert_true(memory_per_instrument < 2000) // Less than 2KB per instrument
  
  // Clear references
  counters = []
  histograms = []
  updown_counters = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_metrics_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let metrics_memory_decrease = MemoryTracker::calculate_decrease(after_metrics, after_metrics_cleanup)
  
  // Verify most memory is reclaimed
  let metrics_reclamation_rate = (metrics_memory_decrease as Float) / (metrics_memory_increase as Float)
  assert_true(metrics_reclamation_rate > 0.7) // At least 70% of memory should be reclaimed
}

// Test 3: Log Record Memory Management
test "log record memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create logger
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "memory_test_logger")
  
  // Create and emit many log records
  for i in 0..10000 {
    let message = "Log message " + i.to_string() + " with some additional content to increase memory usage"
    let log_record = LogRecord::new(Info, message)
    
    // Add attributes
    LogRecord::add_attribute(log_record, "index", IntValue(i))
    LogRecord::add_attribute(log_record, "thread", StringValue("test_thread"))
    
    Logger::emit(logger, log_record)
  }
  
  // Check memory after logging
  let after_logging = MemoryTracker::get_current_usage(memory_tracker)
  let logging_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_logging)
  
  // Verify memory usage is reasonable
  let memory_per_log = logging_memory_increase / 10000
  assert_true(memory_per_log < 500) // Less than 500 bytes per log
  
  // Test log buffer cleanup
  Logger::clear_buffers(logger)
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_log_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let log_memory_decrease = MemoryTracker::calculate_decrease(after_logging, after_log_cleanup)
  
  // Verify most memory is reclaimed
  let log_reclamation_rate = (log_memory_decrease as Float) / (logging_memory_increase as Float)
  assert_true(log_reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
}

// Test 4: Context and Baggage Memory Management
test "context and baggage memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many contexts with baggage
  let mut contexts = []
  let mut baggage_items = []
  
  for i in 0..5000 {
    // Create context
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    
    // Create baggage with many entries
    let baggage = Baggage::new()
    let mut updated_baggage = baggage
    
    for j in 0..10 {
      let key = "baggage_key_" + j.to_string()
      let value = "baggage_value_" + i.to_string() + "_" + j.to_string()
      updated_baggage = Baggage::set_entry(updated_baggage, key, value)
    }
    
    // Create context with baggage
    let ctx_with_baggage = SpanContext::with_baggage(trace_id, span_id, true, updated_baggage)
    
    contexts = contexts + [ctx_with_baggage]
    baggage_items = baggage_items + [updated_baggage]
  }
  
  // Check memory after context creation
  let after_contexts = MemoryTracker::get_current_usage(memory_tracker)
  let context_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_contexts)
  
  // Verify memory usage is reasonable
  let memory_per_context = context_memory_increase / 5000
  assert_true(memory_per_context < 1500) // Less than 1.5KB per context
  
  // Clear references
  contexts = []
  baggage_items = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_context_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let context_memory_decrease = MemoryTracker::calculate_decrease(after_contexts, after_context_cleanup)
  
  // Verify most memory is reclaimed
  let context_reclamation_rate = (context_memory_decrease as Float) / (context_memory_increase as Float)
  assert_true(context_reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
}

// Test 5: Resource Memory Management
test "resource memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many resources with attributes
  let mut resources = []
  
  for i in 0..2000 {
    // Create resource with many attributes
    let mut attrs = []
    
    for j in 0..20 {
      let key = "attr_" + j.to_string()
      let value = StringValue("value_" + i.to_string() + "_" + j.to_string())
      attrs = attrs + [(key, value)]
    }
    
    let resource = Resource::with_attributes(Resource::new(), attrs)
    resources = resources + [resource]
  }
  
  // Check memory after resource creation
  let after_resources = MemoryTracker::get_current_usage(memory_tracker)
  let resource_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_resources)
  
  // Verify memory usage is reasonable
  let memory_per_resource = resource_memory_increase / 2000
  assert_true(memory_per_resource < 2000) // Less than 2KB per resource
  
  // Test resource merging
  let merged_resources = []
  for i in 0..1000 {
    let merged = Resource::merge(resources[i * 2], resources[i * 2 + 1])
    merged_resources = merged_resources + [merged]
  }
  
  // Check memory after merging
  let after_merging = MemoryTracker::get_current_usage(memory_tracker)
  let merging_memory_increase = MemoryTracker::calculate_increase(after_resources, after_merging)
  
  // Clear references
  resources = []
  merged_resources = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_resource_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let resource_memory_decrease = MemoryTracker::calculate_decrease(after_merging, after_resource_cleanup)
  
  // Verify most memory is reclaimed
  let resource_reclamation_rate = (resource_memory_decrease as Float) / 
    ((resource_memory_increase + merging_memory_increase) as Float)
  assert_true(resource_reclamation_rate > 0.7) // At least 70% of memory should be reclaimed
}

// Test 6: Attribute Memory Management
test "attribute memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many attributes of different types
  let mut attributes = []
  
  for i in 0..10000 {
    // String attributes
    let string_attr = StringValue("string_value_" + i.to_string())
    attributes = attributes + [string_attr]
    
    // Int attributes
    let int_attr = IntValue(i)
    attributes = attributes + [int_attr]
    
    // Float attributes
    let float_attr = FloatValue(i.to_int() as Float * 1.5)
    attributes = attributes + [float_attr]
    
    // Bool attributes
    let bool_attr = BoolValue(i % 2 == 0)
    attributes = attributes + [bool_attr]
    
    // Array attributes
    let array_attr = ArrayStringValue(["item1", "item2", "item3"])
    attributes = attributes + [array_attr]
  }
  
  // Check memory after attribute creation
  let after_attributes = MemoryTracker::get_current_usage(memory_tracker)
  let attribute_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_attributes)
  
  // Verify memory usage is reasonable
  let memory_per_attribute = attribute_memory_increase / 50000 // 10000 * 5 attributes each
  assert_true(memory_per_attribute < 200) // Less than 200 bytes per attribute
  
  // Clear references
  attributes = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_attribute_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let attribute_memory_decrease = MemoryTracker::calculate_decrease(after_attributes, after_attribute_cleanup)
  
  // Verify most memory is reclaimed
  let attribute_reclamation_rate = (attribute_memory_decrease as Float) / (attribute_memory_increase as Float)
  assert_true(attribute_reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
}

// Test 7: Serialization Buffer Memory Management
test "serialization buffer memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create spans for serialization
  let mut spans = []
  for i in 0..1000 {
    let span_name = "serialization_test_span_" + i.to_string()
    let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
    let span = Span::new(span_name, Internal, span_ctx)
    
    // Add many attributes and events
    for j in 0..10 {
      Span::add_attribute(span, "attr_" + j.to_string(), StringValue("value_" + j.to_string()))
      Span::add_event(span, "event_" + j.to_string(), Some([("event_attr", StringValue("event_value"))]))
    }
    
    spans = spans + [span]
  }
  
  // Serialize spans to JSON
  let mut json_buffers = []
  for span in spans {
    let json = SpanSerializer::to_json(span)
    json_buffers = json_buffers + [json]
  }
  
  // Check memory after JSON serialization
  let after_json_serialization = MemoryTracker::get_current_usage(memory_tracker)
  let json_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_json_serialization)
  
  // Verify memory usage is reasonable
  let memory_per_json_buffer = json_memory_increase / 1000
  assert_true(memory_per_json_buffer < 5000) // Less than 5KB per JSON buffer
  
  // Serialize spans to binary
  let mut binary_buffers = []
  for span in spans {
    let binary = SpanSerializer::to_binary(span)
    binary_buffers = binary_buffers + [binary]
  }
  
  // Check memory after binary serialization
  let after_binary_serialization = MemoryTracker::get_current_usage(memory_tracker)
  let binary_memory_increase = MemoryTracker::calculate_increase(after_json_serialization, after_binary_serialization)
  
  // Verify binary is more compact than JSON
  let memory_per_binary_buffer = binary_memory_increase / 1000
  assert_true(memory_per_binary_buffer < memory_per_json_buffer) // Binary should be more compact
  
  // Clear references
  spans = []
  json_buffers = []
  binary_buffers = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_serialization_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let serialization_memory_decrease = MemoryTracker::calculate_decrease(after_binary_serialization, after_serialization_cleanup)
  
  // Verify most memory is reclaimed
  let total_serialization_memory = json_memory_increase + binary_memory_increase
  let serialization_reclamation_rate = (serialization_memory_decrease as Float) / (total_serialization_memory as Float)
  assert_true(serialization_reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
}

// Test 8: Telemetry Pipeline Memory Management
test "telemetry pipeline memory management" {
  let memory_tracker = MemoryTracker::new()
  
  // Record baseline memory
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create telemetry pipeline
  let pipeline = TelemetryPipeline::new()
  
  // Add processors
  TelemetryPipeline::add_processor(pipeline, MemoryFilterProcessor::new())
  TelemetryPipeline::add_processor(pipeline, MemoryTransformProcessor::new())
  TelemetryPipeline::add_processor(pipeline, MemoryAggregatorProcessor::new())
  
  // Process large amount of telemetry data
  for i in 0..5000 {
    let data = TelemetryData::new("metric_" + i.to_string(), i.to_int() as Float, "unit", 
      Some([("index", IntValue(i))]))
    
    TelemetryPipeline::process(pipeline, data)
  }
  
  // Check memory after processing
  let after_processing = MemoryTracker::get_current_usage(memory_tracker)
  let processing_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_processing)
  
  // Verify memory usage is reasonable
  let memory_per_item = processing_memory_increase / 5000
  assert_true(memory_per_item < 1000) // Less than 1KB per processed item
  
  // Clear pipeline buffers
  TelemetryPipeline::clear_buffers(pipeline)
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_pipeline_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let pipeline_memory_decrease = MemoryTracker::calculate_decrease(after_processing, after_pipeline_cleanup)
  
  // Verify most memory is reclaimed
  let pipeline_reclamation_rate = (pipeline_memory_decrease as Float) / (processing_memory_increase as Float)
  assert_true(pipeline_reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
}

// Test 9: Memory Leak Detection
test "memory leak detection" {
  let leak_detector = MemoryLeakDetector::new()
  
  // Start leak detection
  MemoryLeakDetector::start(leak_detector)
  
  // Perform operations that might leak memory
  let mut potential_leaks = []
  
  for i in 0..1000 {
    // Create objects that might not be properly cleaned up
    let span = Span::new("leak_test_span", Internal, SpanContext::new("trace_id", "span_id", true, ""))
    let attrs = Attributes::new()
    Attributes::set(attrs, "index", IntValue(i))
    
    // Simulate potential leak by not properly cleaning up
    potential_leaks = potential_leaks + [(span, attrs)]
  }
  
  // Intentionally don't clean up half of the objects
  let cleaned_objects = potential_leaks.slice(0, 500)
  let leaked_objects = potential_leaks.slice(500, 1000)
  
  // Clean up only half of the objects
  for (span, attrs) in cleaned_objects {
    // Proper cleanup
    Span::end(span)
    Attributes::clear(attrs)
  }
  
  // Run leak detection
  let leak_report = MemoryLeakDetector::analyze(leak_detector)
  
  // Verify leak detection
  assert_true(MemoryLeakReport::potential_leaks_detected(leak_report))
  assert_eq(MemoryLeakReport::leaked_object_count(leak_report), 500) // Should detect 500 leaked objects
  
  // Clean up remaining objects
  for (span, attrs) in leaked_objects {
    Span::end(span)
    Attributes::clear(attrs)
  }
  
  // Run leak detection again
  let final_leak_report = MemoryLeakDetector::analyze(leak_detector)
  
  // Verify no more leaks
  assert_false(MemoryLeakReport::potential_leaks_detected(final_leak_report))
  assert_eq(MemoryLeakReport::leaked_object_count(final_leak_report), 0)
}

// Test 10: Resource Pool Management
test "resource pool management" {
  let resource_pool = ResourcePool::new(100) // Pool with 100 resources
  
  // Test resource acquisition and release
  let acquired_resources = []
  
  // Acquire all resources
  for i in 0..100 {
    let resource = ResourcePool::acquire(resource_pool)
    assert_true(resource.is_some()) // Should be able to acquire
    acquired_resources = acquired_resources + [resource.unwrap()]
  }
  
  // Verify pool is empty
  assert_true(ResourcePool::acquire(resource_pool).is_none()) // Should not be able to acquire more
  
  // Release half of the resources
  for i in 0..50 {
    ResourcePool::release(resource_pool, acquired_resources[i])
  }
  
  // Verify pool has available resources
  assert_eq(ResourcePool::available_count(resource_pool), 50)
  
  // Acquire resources again
  let reacquired_resources = []
  for i in 0..50 {
    let resource = ResourcePool::acquire(resource_pool)
    assert_true(resource.is_some()) // Should be able to acquire
    reacquired_resources = reacquired_resources + [resource.unwrap()]
  }
  
  // Verify pool is empty again
  assert_true(ResourcePool::acquire(resource_pool).is_none()) // Should not be able to acquire more
  
  // Release all resources
  for i in 50..100 {
    ResourcePool::release(resource_pool, acquired_resources[i])
  }
  
  for resource in reacquired_resources {
    ResourcePool::release(resource_pool, resource)
  }
  
  // Verify all resources are available
  assert_eq(ResourcePool::available_count(resource_pool), 100)
  
  // Test resource pool memory efficiency
  let memory_tracker = MemoryTracker::new()
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many resource pools
  let mut pools = []
  for i in 0..10 {
    let pool = ResourcePool::new(1000) // Large pools
    pools = pools + [pool]
  }
  
  // Check memory after pool creation
  let after_pools = MemoryTracker::get_current_usage(memory_tracker)
  let pools_memory_increase = MemoryTracker::calculate_increase(baseline_memory, after_pools)
  
  // Verify memory usage is reasonable
  let memory_per_pool = pools_memory_increase / 10
  assert_true(memory_per_pool < 1024 * 1024) // Less than 1MB per pool
  
  // Clear references
  pools = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  // Check memory after cleanup
  let after_pool_cleanup = MemoryTracker::get_current_usage(memory_tracker)
  let pool_memory_decrease = MemoryTracker::calculate_decrease(after_pools, after_pool_cleanup)
  
  // Verify most memory is reclaimed
  let pool_reclamation_rate = (pool_memory_decrease as Float) / (pools_memory_increase as Float)
  assert_true(pool_reclamation_rate > 0.8) // At least 80% of memory should be reclaimed
}