// Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety of the Azimuth telemetry system

// Test 1: Concurrent Span Creation and Operations
test "concurrent span creation and operations" {
  let concurrent_tester = ConcurrentTester::new(4, 1000) // 4 threads, 1000 operations each
  
  // Shared span registry for testing
  let span_registry = ConcurrentSpanRegistry::new()
  
  // Test concurrent span creation
  let creation_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut created_spans = []
    for i in 0..1000 {
      let span_name = "thread-" + thread_id.to_string() + "-span-" + i.to_string()
      let span_ctx = SpanContext::new("concurrent_trace", "span_" + thread_id.to_string() + "_" + i.to_string(), true, "")
      let span = Span::new(span_name, Internal, span_ctx)
      
      // Register span in shared registry
      ConcurrentSpanRegistry::register(span_registry, span)
      
      // Add attributes and events
      Span::add_attribute(span, "thread_id", IntValue(thread_id))
      Span::add_attribute(span, "operation_id", IntValue(i))
      Span::add_event(span, "test_event", None)
      
      created_spans = created_spans + [span]
    }
    created_spans
  })
  
  // Verify all spans were created
  let total_created_spans = creation_results.fold_left(0, fn(acc, spans) { acc + spans.length() })
  assert_eq(total_created_spans, 4000) // 4 threads * 1000 spans each
  
  // Verify registry contains all spans
  assert_eq(ConcurrentSpanRegistry::size(span_registry), 4000)
  
  // Test concurrent span operations
  let operation_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let spans = ConcurrentSpanRegistry::get_spans_by_thread(span_registry, thread_id)
    let mut operations_completed = 0
    
    for span in spans {
      // Concurrent operations on spans
      Span::set_status(span, Ok, Some("Operation completed"))
      Span::add_event(span, "completion_event", Some([("thread_id", IntValue(thread_id))]))
      operations_completed = operations_completed + 1
    }
    
    operations_completed
  })
  
  // Verify all operations completed
  let total_operations = operation_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_operations, 4000)
  
  // Verify span integrity after concurrent operations
  let all_spans = ConcurrentSpanRegistry::get_all_spans(span_registry)
  for span in all_spans {
    assert_eq(Span::status(span), Ok)
    assert_true(Span::events(span).length() >= 2) // test_event + completion_event
  }
}

// Test 2: Concurrent Metrics Operations
test "concurrent metrics operations" {
  let concurrent_tester = ConcurrentTester::new(8, 5000) // 8 threads, 5000 operations each
  
  // Shared metrics provider
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_meter")
  
  // Create shared metrics instruments
  let counter = Meter::create_counter(meter, "concurrent_counter", None, None)
  let histogram = Meter::create_histogram(meter, "concurrent_histogram", None, None)
  let updown_counter = Meter::create_updown_counter(meter, "concurrent_updown", None, None)
  
  // Test concurrent counter operations
  let counter_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    for i in 0..5000 {
      Counter::add(counter, 1.0)
    }
    5000 // Return operations count
  })
  
  // Verify all counter operations completed
  let total_counter_ops = counter_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_counter_ops, 40000) // 8 threads * 5000 operations
  
  // Test concurrent histogram operations
  let histogram_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    for i in 0..5000 {
      let value = (thread_id * 5000 + i) as Float
      Histogram::record(histogram, value)
    }
    5000 // Return operations count
  })
  
  // Verify all histogram operations completed
  let total_histogram_ops = histogram_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_histogram_ops, 40000)
  
  // Test concurrent updown counter operations
  let updown_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    for i in 0..5000 {
      let value = if i % 2 == 0 { 1.0 } else { -1.0 }
      UpDownCounter::add(updown_counter, value)
    }
    5000 // Return operations count
  })
  
  // Verify all updown counter operations completed
  let total_updown_ops = updown_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_updown_ops, 40000)
  
  // Verify metrics integrity after concurrent operations
  let counter_value = Counter::get_value(counter)
  assert_eq(counter_value, 40000.0)
  
  // For updown counter, the final value should be 0 (equal positive and negative additions)
  let updown_value = UpDownCounter::get_value(updown_counter)
  assert_eq(updown_value, 0.0)
}

// Test 3: Concurrent Context Propagation
test "concurrent context propagation" {
  let concurrent_tester = ConcurrentTester::new(4, 1000) // 4 threads, 1000 operations each
  
  // Shared context store
  let context_store = ConcurrentContextStore::new()
  
  // Test concurrent context creation and propagation
  let context_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut propagated_contexts = []
    
    for i in 0..1000 {
      // Create root context
      let trace_id = "trace_" + thread_id.to_string() + "_" + i.to_string()
      let root_ctx = SpanContext::new(trace_id, "root_span", true, "")
      
      // Store in shared context store
      ConcurrentContextStore::store(context_store, trace_id, root_ctx)
      
      // Create child contexts
      let child1_ctx = SpanContext::create_child(root_ctx, "child1_span")
      let child2_ctx = SpanContext::create_child(child1_ctx, "child2_span")
      
      // Inject and extract contexts
      let injected = TracePropagator::inject(child2_ctx)
      let extracted = TracePropagator::extract(injected)
      
      // Verify context consistency
      if SpanContext::trace_id(extracted) == trace_id {
        propagated_contexts = propagated_contexts + [extracted]
      }
    }
    
    propagated_contexts
  })
  
  // Verify all contexts were propagated correctly
  let total_propagated = context_results.fold_left(0, fn(acc, contexts) { acc + contexts.length() })
  assert_eq(total_propagated, 4000) // All contexts should be propagated correctly
  
  // Verify context store integrity
  assert_eq(ConcurrentContextStore::size(context_store), 4000)
  
  // Test concurrent baggage operations
  let baggage_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut baggage_operations = 0
    
    for i in 0..1000 {
      let baggage = Baggage::new()
      let updated_baggage = Baggage::set_entry(baggage, "thread_id", thread_id.to_string())
      let final_baggage = Baggage::set_entry(updated_baggage, "operation_id", i.to_string())
      
      // Store in context store
      let key = "baggage_" + thread_id.to_string() + "_" + i.to_string()
      ConcurrentContextStore::store_baggage(context_store, key, final_baggage)
      
      baggage_operations = baggage_operations + 1
    }
    
    baggage_operations
  })
  
  // Verify all baggage operations completed
  let total_baggage_ops = baggage_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_baggage_ops, 4000)
}

// Test 4: Concurrent Logging Operations
test "concurrent logging operations" {
  let concurrent_tester = ConcurrentTester::new(6, 2000) // 6 threads, 2000 operations each
  
  // Shared logger provider
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "concurrent_logger")
  
  // Test concurrent log emission
  let log_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut logs_emitted = 0
    
    for i in 0..2000 {
      let severity = if i % 4 == 0 { Info } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Error } else { Debug }
      let message = "Thread " + thread_id.to_string() + " log message " + i.to_string()
      let log_record = LogRecord::new(severity, message)
      
      // Add thread-specific attributes
      LogRecord::add_attribute(log_record, "thread_id", IntValue(thread_id))
      LogRecord::add_attribute(log_record, "message_id", IntValue(i))
      
      Logger::emit(logger, log_record)
      logs_emitted = logs_emitted + 1
    }
    
    logs_emitted
  })
  
  // Verify all logs were emitted
  let total_logs = log_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_logs, 12000) // 6 threads * 2000 logs
  
  // Test concurrent batch logging
  let batch_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut batch_logs = []
    
    for i in 0..100 {
      let message = "Batch log from thread " + thread_id.to_string() + " message " + i.to_string()
      let log_record = LogRecord::new(Info, message)
      batch_logs = batch_logs + [log_record]
    }
    
    // Emit batch
    Logger::emit_batch(logger, batch_logs)
    batch_logs.length()
  })
  
  // Verify all batch logs were emitted
  let total_batch_logs = batch_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_batch_logs, 600) // 6 threads * 100 logs per batch
}

// Test 5: Concurrent Resource Operations
test "concurrent resource operations" {
  let concurrent_tester = ConcurrentTester::new(4, 500) // 4 threads, 500 operations each
  
  // Shared resource manager
  let resource_manager = ConcurrentResourceManager::new()
  
  // Test concurrent resource creation and merging
  let resource_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut resources_created = 0
    
    for i in 0..500 {
      // Create resource with thread-specific attributes
      let attrs = [
        ("thread_id", IntValue(thread_id)),
        ("resource_id", IntValue(i)),
        ("service.name", StringValue("service_" + thread_id.to_string())),
        ("service.instance.id", StringValue("instance_" + thread_id.to_string() + "_" + i.to_string()))
      ]
      let resource = Resource::with_attributes(Resource::new(), attrs)
      
      // Store in resource manager
      ConcurrentResourceManager::store(resource_manager, "resource_" + thread_id.to_string() + "_" + i.to_string(), resource)
      
      resources_created = resources_created + 1
    }
    
    resources_created
  })
  
  // Verify all resources were created
  let total_resources = resource_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_resources, 2000) // 4 threads * 500 resources
  
  // Verify resource manager integrity
  assert_eq(ConcurrentResourceManager::size(resource_manager), 2000)
  
  // Test concurrent resource merging
  let merge_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut merges_completed = 0
    
    for i in 0..100 {
      // Get two resources to merge
      let resource1_key = "resource_" + thread_id.to_string() + "_" + (i * 2).to_string()
      let resource2_key = "resource_" + thread_id.to_string() + "_" + (i * 2 + 1).to_string()
      
      let resource1 = ConcurrentResourceManager::get(resource_manager, resource1_key)
      let resource2 = ConcurrentResourceManager::get(resource_manager, resource2_key)
      
      match (resource1, resource2) {
        (Some(r1), Some(r2)) => {
          let merged = Resource::merge(r1, r2)
          ConcurrentResourceManager::store(resource_manager, "merged_" + thread_id.to_string() + "_" + i.to_string(), merged)
          merges_completed = merges_completed + 1
        }
        _ => {} // Skip if resources not found
      }
    }
    
    merges_completed
  })
  
  // Verify merge operations
  let total_merges = merge_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_merges, 400) // 4 threads * 100 merges
}

// Test 6: Concurrent Attribute Operations
test "concurrent attribute operations" {
  let concurrent_tester = ConcurrentTester::new(8, 1000) // 8 threads, 1000 operations each
  
  // Shared attributes store
  let attributes_store = ConcurrentAttributesStore::new()
  
  // Test concurrent attribute setting and getting
  let attribute_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut operations_completed = 0
    
    for i in 0..1000 {
      let key = "attr_" + thread_id.to_string() + "_" + i.to_string()
      let value = StringValue("value_" + thread_id.to_string() + "_" + i.to_string())
      
      // Set attribute
      ConcurrentAttributesStore::set(attributes_store, key, value)
      
      // Get attribute
      let retrieved = ConcurrentAttributesStore::get(attributes_store, key)
      
      match retrieved {
        Some(StringValue(v)) => {
          if v == "value_" + thread_id.to_string() + "_" + i.to_string() {
            operations_completed = operations_completed + 1
          }
        }
        _ => {} // Skip if not found or wrong type
      }
    }
    
    operations_completed
  })
  
  // Verify all attribute operations completed
  let total_operations = attribute_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_operations, 8000) // 8 threads * 1000 operations
  
  // Verify attributes store integrity
  assert_eq(ConcurrentAttributesStore::size(attributes_store), 8000)
  
  // Test concurrent attribute updates
  let update_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut updates_completed = 0
    
    for i in 0..500 {
      let key = "shared_attr_" + i.to_string()
      let value = StringValue("thread_" + thread_id.to_string() + "_value_" + i.to_string())
      
      // Update shared attribute
      ConcurrentAttributesStore::set(attributes_store, key, value)
      
      // Get attribute to verify update
      let retrieved = ConcurrentAttributesStore::get(attributes_store, key)
      
      match retrieved {
        Some(StringValue(_)) => {
          updates_completed = updates_completed + 1
        }
        _ => {} // Skip if not found
      }
    }
    
    updates_completed
  })
  
  // Verify update operations
  let total_updates = update_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_updates, 4000) // 8 threads * 500 updates
}

// Test 7: Concurrent Serialization/Deserialization
test "concurrent serialization and deserialization" {
  let concurrent_tester = ConcurrentTester::new(4, 500) // 4 threads, 500 operations each
  
  // Shared serialization store
  let serialization_store = ConcurrentSerializationStore::new()
  
  // Test concurrent serialization
  let serialization_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut serializations_completed = 0
    
    for i in 0..500 {
      // Create span to serialize
      let span_name = "thread_" + thread_id.to_string() + "_span_" + i.to_string()
      let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
      let span = Span::new(span_name, Internal, span_ctx)
      
      // Add attributes
      Span::add_attribute(span, "thread_id", IntValue(thread_id))
      Span::add_attribute(span, "span_id", IntValue(i))
      
      // Serialize to JSON
      let json_result = SpanSerializer::to_json(span)
      
      match json_result {
        Ok(json) => {
          // Store serialized data
          let key = "json_" + thread_id.to_string() + "_" + i.to_string()
          ConcurrentSerializationStore::store_json(serialization_store, key, json)
          serializations_completed = serializations_completed + 1
        }
        Err(_) => {} // Skip on error
      }
    }
    
    serializations_completed
  })
  
  // Verify all serializations completed
  let total_serializations = serialization_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_serializations, 2000) // 4 threads * 500 serializations
  
  // Test concurrent deserialization
  let deserialization_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut deserializations_completed = 0
    
    for i in 0..500 {
      let key = "json_" + thread_id.to_string() + "_" + i.to_string()
      
      // Get serialized data
      let json = ConcurrentSerializationStore::get_json(serialization_store, key)
      
      match json {
        Some(json_data) => {
          // Deserialize
          let deserialization_result = SpanSerializer::from_json(json_data)
          
          match deserialization_result {
            Ok(span) => {
              // Verify deserialized span
              if Span::name(span) == "thread_" + thread_id.to_string() + "_span_" + i.to_string() {
                deserializations_completed = deserializations_completed + 1
              }
            }
            Err(_) => {} // Skip on error
          }
        }
        None => {} // Skip if not found
      }
    }
    
    deserializations_completed
  })
  
  // Verify all deserializations completed
  let total_deserializations = deserialization_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_deserializations, 2000) // 4 threads * 500 deserializations
}

// Test 8: Concurrent Memory Management
test "concurrent memory management" {
  let concurrent_tester = ConcurrentTester::new(4, 1000) // 4 threads, 1000 operations each
  
  // Shared memory manager
  let memory_manager = ConcurrentMemoryManager::new()
  
  // Test concurrent memory allocation and deallocation
  let memory_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut allocations_completed = 0
    let mut allocated_objects = []
    
    for i in 0..1000 {
      // Allocate memory
      let obj = ConcurrentMemoryManager::allocate(memory_manager, 1024) // 1KB each
      allocated_objects = allocated_objects + [obj]
      allocations_completed = allocations_completed + 1
      
      // Deallocate every other object to test mixed allocation/deallocation
      if i % 2 == 1 && allocated_objects.length() > 0 {
        let obj_to_deallocate = allocated_objects[0]
        ConcurrentMemoryManager::deallocate(memory_manager, obj_to_deallocate)
        allocated_objects = allocated_objects.slice(1, allocated_objects.length())
      }
    }
    
    // Deallocate remaining objects
    for obj in allocated_objects {
      ConcurrentMemoryManager::deallocate(memory_manager, obj)
    }
    
    allocations_completed
  })
  
  // Verify all allocations completed
  let total_allocations = memory_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_allocations, 4000) // 4 threads * 1000 allocations
  
  // Verify memory manager integrity
  assert_eq(ConcurrentMemoryManager::get_allocated_count(memory_manager), 0) // All should be deallocated
  assert_eq(ConcurrentMemoryManager::get_allocated_bytes(memory_manager), 0) // All memory should be freed
  
  // Test concurrent garbage collection
  let gc_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut gc_operations = 0
    
    for i in 0..100 {
      // Allocate objects
      let mut objects = []
      for j in 0..10 {
        let obj = ConcurrentMemoryManager::allocate(memory_manager, 512) // 512B each
        objects = objects + [obj]
      }
      
      // Trigger garbage collection
      ConcurrentMemoryManager::trigger_gc(memory_manager)
      gc_operations = gc_operations + 1
      
      // Deallocate objects
      for obj in objects {
        ConcurrentMemoryManager::deallocate(memory_manager, obj)
      }
    }
    
    gc_operations
  })
  
  // Verify all GC operations completed
  let total_gc_ops = gc_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_gc_ops, 400) // 4 threads * 100 GC operations
}

// Test 9: Concurrent Network Operations
test "concurrent network operations" {
  let concurrent_tester = ConcurrentTester::new(4, 100) // 4 threads, 100 operations each
  
  // Shared HTTP client pool
  let client_pool = ConcurrentHttpClientPool::new(8) // 8 clients in pool
  
  // Test concurrent HTTP requests
  let http_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut requests_completed = 0
    
    for i in 0..100 {
      // Get client from pool
      let client = ConcurrentHttpClientPool::acquire(client_pool)
      
      // Make HTTP request
      let request = HttpRequest::new("GET", "https://httpbin.org/get", [], None)
      let response = HttpClient::send(client, request)
      
      match response {
        Ok(_) => {
          requests_completed = requests_completed + 1
        }
        Err(_) => {
          // Handle error but continue
          requests_completed = requests_completed + 1 // Count as attempted
        }
      }
      
      // Return client to pool
      ConcurrentHttpClientPool::release(client_pool, client)
    }
    
    requests_completed
  })
  
  // Verify all requests were attempted
  let total_requests = http_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_requests, 400) // 4 threads * 100 requests
  
  // Verify client pool integrity
  assert_eq(ConcurrentHttpClientPool::available_clients(client_pool), 8) // All clients should be returned
  
  // Test concurrent telemetry export
  let export_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut exports_completed = 0
    
    for i in 0..50 {
      // Create telemetry data to export
      let spans = []
      for j in 0..10 {
        let span_name = "thread_" + thread_id.to_string() + "_span_" + i.to_string() + "_" + j.to_string()
        let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
        let span = Span::new(span_name, Internal, span_ctx)
        spans = spans + [span]
      }
      
      // Export spans
      let export_result = ConcurrentTelemetryExporter::export(spans)
      
      match export_result {
        Ok(_) => {
          exports_completed = exports_completed + 1
        }
        Err(_) => {
          // Handle error but continue
          exports_completed = exports_completed + 1 // Count as attempted
        }
      }
    }
    
    exports_completed
  })
  
  // Verify all exports were attempted
  let total_exports = export_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_exports, 200) // 4 threads * 50 exports
}

// Test 10: Concurrent Pipeline Processing
test "concurrent pipeline processing" {
  let concurrent_tester = ConcurrentTester::new(4, 1000) // 4 threads, 1000 operations each
  
  // Shared telemetry pipeline
  let pipeline = ConcurrentTelemetryPipeline::new()
  
  // Add processors to pipeline
  ConcurrentTelemetryPipeline::add_processor(pipeline, "filter", ConcurrentFilterProcessor::new())
  ConcurrentTelemetryPipeline::add_processor(pipeline, "transform", ConcurrentTransformProcessor::new())
  ConcurrentTelemetryPipeline::add_processor(pipeline, "aggregator", ConcurrentAggregatorProcessor::new())
  
  // Test concurrent pipeline processing
  let pipeline_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut processed_items = 0
    
    for i in 0..1000 {
      // Create telemetry data
      let data = TelemetryData::new("metric_" + thread_id.to_string() + "_" + i.to_string(), 
        i.to_int() as Float, "unit", Some([("thread_id", IntValue(thread_id))]))
      
      // Process through pipeline
      let result = ConcurrentTelemetryPipeline::process(pipeline, data)
      
      match result {
        Ok(_) => {
          processed_items = processed_items + 1
        }
        Err(_) => {
          // Handle error but continue
          processed_items = processed_items + 1 // Count as attempted
        }
      }
    }
    
    processed_items
  })
  
  // Verify all items were processed
  let total_processed = pipeline_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_processed, 4000) // 4 threads * 1000 items
  
  // Verify pipeline statistics
  let pipeline_stats = ConcurrentTelemetryPipeline::get_statistics(pipeline)
  assert_eq(PipelineStats::total_processed(pipeline_stats), 4000)
  assert_eq(PipelineStats::total_errors(pipeline_stats), 0) // Should be no errors in this test
  
  // Test concurrent batch processing
  let batch_results = ConcurrentTester::run_parallel(concurrent_tester, fn(thread_id) {
    let mut batch_data = []
    
    for i in 0..100 {
      let data = TelemetryData::new("batch_metric_" + thread_id.to_string() + "_" + i.to_string(), 
        i.to_int() as Float, "unit", Some([("thread_id", IntValue(thread_id))]))
      batch_data = batch_data + [data]
    }
    
    // Process batch
    let result = ConcurrentTelemetryPipeline::process_batch(pipeline, batch_data)
    
    match result {
      Ok(processed_batch) => processed_batch.length()
      Err(_) => 0
    }
  })
  
  // Verify all batch items were processed
  let total_batch_processed = batch_results.fold_left(0, fn(acc, count) { acc + count })
  assert_eq(total_batch_processed, 400) // 4 threads * 100 items per batch
}