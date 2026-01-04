// Concurrent Processing Tests for Azimuth Telemetry System
// This file contains test cases for concurrent processing and thread safety

// Test 1: Concurrent Span Processing
test "concurrent span processing" {
  let processor = ConcurrentSpanProcessor::new(1000, 2000) // 1000 batch size, 2s timeout
  
  // Create concurrent tasks for span processing
  let concurrent_tasks = 10
  let spans_per_task = 500
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    for task_id in 1..=spans_per_task {
      let span_ctx = SpanContext::new(
        "concurrent_trace_" + ConcurrentTask::current_id().to_string(),
        "concurrent_span_" + ConcurrentTask::current_id().to_string() + "_" + task_id.to_string(),
        true,
        "concurrent_state"
      )
      
      let span = Span::new("concurrent_test", Server, span_ctx)
      
      // Add attributes
      Span::set_attribute(span, "task_id", IntValue(ConcurrentTask::current_id()))
      Span::set_attribute(span, "span_id", IntValue(task_id))
      Span::set_attribute(span, "thread_id", StringValue(Thread::current_id().to_string()))
      
      // Process span concurrently
      SpanProcessor::on_start_concurrent(processor, span)
      
      // Simulate some work
      Thread::sleep(1) // 1ms
      
      Span::end(span)
      SpanProcessor::on_end_concurrent(processor, span)
    }
  })
  
  // Wait for all tasks to complete
  let results = ConcurrentTaskRunner::wait_all(task_results, 10000) // 10s timeout
  
  // Verify all tasks completed successfully
  assert_eq(results.length(), concurrent_tasks)
  
  for result in results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
  
  // Force flush and verify no data loss
  let flush_result = SpanProcessor::force_flush_concurrent(processor)
  match flush_result {
    Success => assert_true(true)
    Error(_) => assert_true(false) // Flush should succeed
  }
  
  // Verify total number of spans processed
  let total_spans = concurrent_tasks * spans_per_task
  let processed_spans = SpanProcessor::get_processed_count(processor)
  assert_eq(processed_spans, total_spans)
}

// Test 2: Thread-Safe Metrics Collection
test "thread-safe metrics collection" {
  let meter_provider = MeterProvider::new_concurrent()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent_metrics_test")
  
  // Create metrics
  let counter = Meter::create_counter_concurrent(meter, "concurrent_counter", Some("Concurrent counter"), Some("operations"))
  let histogram = Meter::create_histogram_concurrent(meter, "concurrent_histogram", Some("Concurrent histogram"), Some("ms"))
  let updown_counter = Meter::create_updown_counter_concurrent(meter, "concurrent_updown", Some("Concurrent updown counter"), Some("value"))
  
  // Create concurrent tasks for metrics collection
  let concurrent_tasks = 20
  let measurements_per_task = 100
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    
    for i in 1..=measurements_per_task {
      // Record counter measurements
      Counter::add_concurrent(counter, 1.0)
      
      // Record histogram measurements with task-specific values
      Histogram::record_concurrent(histogram, (task_id * 10.0 + i as Float))
      
      // Record updown counter measurements
      if i % 2 == 0 {
        UpDownCounter::add_concurrent(updown_counter, 1.0)
      } else {
        UpDownCounter::add_concurrent(updown_counter, -1.0)
      }
      
      // Add some attributes
      let attrs = Attributes::new()
      Attributes::set(attrs, "task_id", IntValue(task_id))
      Attributes::set(attrs, "measurement_id", IntValue(i))
      
      Counter::add_concurrent(counter, 0.5, Some(attrs))
      
      // Simulate some work
      Thread::sleep(1) // 1ms
    }
  })
  
  // Wait for all tasks to complete
  let results = ConcurrentTaskRunner::wait_all(task_results, 15000) // 15s timeout
  
  // Verify all tasks completed successfully
  assert_eq(results.length(), concurrent_tasks)
  
  for result in results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
  
  // Verify metrics values
  let total_measurements = concurrent_tasks * measurements_per_task
  let total_counter_value = total_measurements as Float * 1.5 // 1.0 + 0.5 per measurement
  
  let counter_value = Counter::get_value_concurrent(counter)
  assert_eq(counter_value, total_counter_value)
  
  // Updown counter should be close to 0 (equal additions and subtractions)
  let updown_value = UpDownCounter::get_value_concurrent(updown_counter)
  assert_true(updown_value.abs() < 100.0) // Should be close to 0
  
  // Verify histogram has correct number of measurements
  let histogram_count = Histogram::get_count_concurrent(histogram)
  assert_eq(histogram_count, total_measurements as Int)
  
  // Verify histogram statistics
  let histogram_sum = Histogram::get_sum_concurrent(histogram)
  assert_true(histogram_sum > 0.0)
  
  let histogram_avg = histogram_sum / (histogram_count as Float)
  assert_true(histogram_avg > 50.0) // Average should be reasonable
}

// Test 3: Concurrent Context Propagation
test "concurrent context propagation" {
  let propagator = TraceContextPropagator::new_concurrent()
  
  // Create concurrent tasks for context propagation
  let concurrent_tasks = 15
  let contexts_per_task = 50
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    
    for i in 1..=contexts_per_task {
      // Create span context
      let span_ctx = SpanContext::new(
        "concurrent_trace_" + task_id.to_string(),
        "concurrent_span_" + task_id.to_string() + "_" + i.to_string(),
        true,
        "concurrent_state"
      )
      
      // Create context with span
      let ctx = Context::with_value_concurrent(
        Context::root(),
        ContextKey::new("span_context"),
        span_ctx
      )
      
      // Add baggage
      let baggage = Baggage::new()
      let baggage_with_entry = Baggage::set_entry(baggage, "task_id", task_id.to_string())
      let ctx_with_baggage = Context::with_value_concurrent(
        ctx,
        ContextKey::new("baggage"),
        baggage_with_entry
      )
      
      // Create carrier
      let carrier = TextMapCarrier::new()
      
      // Inject context
      Propagator::inject_concurrent(propagator, ctx_with_baggage, carrier)
      
      // Simulate some work
      Thread::sleep(1) // 1ms
      
      // Extract context
      let extracted_ctx = Propagator::extract_concurrent(propagator, carrier)
      
      // Verify extracted context
      let extracted_span_ctx = Context::get_concurrent(extracted_ctx, ContextKey::new("span_context"))
      match extracted_span_ctx {
        Some(ctx) => {
          assert_eq(SpanContext::trace_id(ctx), "concurrent_trace_" + task_id.to_string())
          assert_eq(SpanContext::span_id(ctx), "concurrent_span_" + task_id.to_string() + "_" + i.to_string())
        }
        None => assert_true(false) // Span context should be found
      }
      
      let extracted_baggage = Context::get_concurrent(extracted_ctx, ContextKey::new("baggage"))
      match extracted_baggage {
        Some(baggage) => {
          let task_id_value = Baggage::get_entry(baggage, "task_id")
          match task_id_value {
            Some(value) => assert_eq(value, task_id.to_string())
            None => assert_true(false) // Task ID should be found in baggage
          }
        }
        None => assert_true(false) // Baggage should be found
      }
    }
  })
  
  // Wait for all tasks to complete
  let results = ConcurrentTaskRunner::wait_all(task_results, 15000) // 15s timeout
  
  // Verify all tasks completed successfully
  assert_eq(results.length(), concurrent_tasks)
  
  for result in results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
}

// Test 4: Concurrent Resource Management
test "concurrent resource management" {
  let resource_manager = ConcurrentResourceManager::new()
  
  // Create concurrent tasks for resource operations
  let concurrent_tasks = 12
  let operations_per_task = 100
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    
    for i in 1..=operations_per_task {
      // Acquire resource
      let resource = ConcurrentResourceManager::acquire(resource_manager, "test_resource", 5000)
      match resource {
        Some(res) => {
          // Use resource
          let result = Resource::use_concurrent(res, {
            // Simulate resource usage
            Thread::sleep(2) // 2ms
            
            // Update resource state
            Resource::set_attribute_concurrent(res, "task_id", IntValue(task_id))
            Resource::set_attribute_concurrent(res, "operation_id", IntValue(i))
            Resource::set_attribute_concurrent(res, "thread_id", StringValue(Thread::current_id().to_string()))
            
            // Return some value
            task_id * 1000 + i
          })
          
          match result {
            Some(value) => assert_eq(value, task_id * 1000 + i)
            None => assert_true(false) // Resource usage should succeed
          }
          
          // Release resource
          ConcurrentResourceManager::release(resource_manager, res)
        }
        None => assert_true(false) // Should be able to acquire resource
      }
      
      // Simulate some work between operations
      Thread::sleep(1) // 1ms
    }
  })
  
  // Wait for all tasks to complete
  let results = ConcurrentTaskRunner::wait_all(task_results, 20000) // 20s timeout
  
  // Verify all tasks completed successfully
  assert_eq(results.length(), concurrent_tasks)
  
  for result in results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
  
  // Verify resource manager statistics
  let stats = ConcurrentResourceManager::get_statistics(resource_manager)
  assert_eq(stats.total_acquisitions, concurrent_tasks * operations_per_task)
  assert_eq(stats.total_releases, concurrent_tasks * operations_per_task)
  assert_eq(stats.current_resources, 0) // All resources should be released
  assert_eq(stats.max_concurrent_resources, concurrent_tasks) // Max concurrent resources used
}

// Test 5: Concurrent Data Processing Pipeline
test "concurrent data processing pipeline" {
  let pipeline = ConcurrentProcessingPipeline::new(4) // 4 processing stages
  
  // Create concurrent data source
  let data_source = ConcurrentDataSource::new()
  
  // Create concurrent data sink
  let data_sink = ConcurrentDataSink::new()
  
  // Configure pipeline stages
  pipeline.add_stage(ConcurrentStage::new("validation", {
    let data = ConcurrentStage::get_input()
    DataValidator::validate_concurrent(data)
  }))
  
  pipeline.add_stage(ConcurrentStage::new("transformation", {
    let data = ConcurrentStage::get_input()
    DataTransformer::transform_concurrent(data)
  }))
  
  pipeline.add_stage(ConcurrentStage::new("aggregation", {
    let data = ConcurrentStage::get_input()
    DataAggregator::aggregate_concurrent(data)
  }))
  
  pipeline.add_stage(ConcurrentStage::new("enrichment", {
    let data = ConcurrentStage::get_input()
    DataEnricher::enrich_concurrent(data)
  }))
  
  // Create concurrent tasks for data generation
  let concurrent_tasks = 8
  let data_items_per_task = 200
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    
    for i in 1..=data_items_per_task {
      // Create data item
      let attrs = Attributes::new()
      Attributes::set(attrs, "task_id", IntValue(task_id))
      Attributes::set(attrs, "item_id", IntValue(i))
      Attributes::set(attrs, "timestamp", IntValue(1234567890 + task_id * 1000 + i))
      
      let data_item = TelemetryData::new(
        (task_id * 100.0 + i as Float),
        attrs,
        1234567890L + (task_id * 1000L + i as Long)
      )
      
      // Add data to source
      ConcurrentDataSource::add(data_source, data_item)
      
      // Simulate data generation interval
      Thread::sleep(1) // 1ms
    }
  })
  
  // Start pipeline processing
  let pipeline_result = ConcurrentProcessingPipeline::start(pipeline, data_source, data_sink)
  match pipeline_result {
    Success => assert_true(true)
    Error(_) => assert_true(false) // Pipeline should start successfully
  }
  
  // Wait for data generation to complete
  let generation_results = ConcurrentTaskRunner::wait_all(task_results, 15000) // 15s timeout
  
  // Verify all generation tasks completed successfully
  assert_eq(generation_results.length(), concurrent_tasks)
  
  for result in generation_results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
  
  // Wait for pipeline to process all data
  Thread::sleep(5000) // 5s for processing
  
  // Stop pipeline
  let stop_result = ConcurrentProcessingPipeline::stop(pipeline)
  match stop_result {
    Success => assert_true(true)
    Error(_) => assert_true(false) // Pipeline should stop successfully
  }
  
  // Verify data processing results
  let processed_data = ConcurrentDataSink::get_all(data_sink)
  let expected_data_count = concurrent_tasks * data_items_per_task
  
  assert_eq(processed_data.length(), expected_data_count)
  
  // Verify data integrity
  for data in processed_data {
    let attrs = TelemetryData::attributes(data)
    
    // Check that required attributes exist
    let task_id = Attributes::get(attrs, "task_id")
    let item_id = Attributes::get(attrs, "item_id")
    let timestamp = Attributes::get(attrs, "timestamp")
    
    assert_true(task_id.is_some())
    assert_true(item_id.is_some())
    assert_true(timestamp.is_some())
    
    // Check that processing stages added their attributes
    let validation_status = Attributes::get(attrs, "validation_status")
    let transformation_status = Attributes::get(attrs, "transformation_status")
    let aggregation_status = Attributes::get(attrs, "aggregation_status")
    let enrichment_status = Attributes::get(attrs, "enrichment_status")
    
    assert_true(validation_status.is_some())
    assert_true(transformation_status.is_some())
    assert_true(aggregation_status.is_some())
    assert_true(enrichment_status.is_some())
  }
  
  // Verify pipeline statistics
  let pipeline_stats = ConcurrentProcessingPipeline::get_statistics(pipeline)
  assert_eq(pipeline_stats.items_processed, expected_data_count)
  assert_eq(pipeline_stats.items_dropped, 0) // No items should be dropped
  assert_true(pipeline_stats.average_processing_time > 0.0)
  assert_true(pipeline_stats.max_concurrent_items <= concurrent_tasks * 10) // Reasonable concurrency
}

// Test 6: Concurrent Batch Processing
test "concurrent batch processing" {
  let batch_processor = ConcurrentBatchProcessor::new(100, 1000) // 100 batch size, 1s timeout
  
  // Create concurrent tasks for data generation
  let concurrent_tasks = 10
  let data_items_per_task = 300
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    
    for i in 1..=data_items_per_task {
      // Create data item
      let attrs = Attributes::new()
      Attributes::set(attrs, "task_id", IntValue(task_id))
      Attributes::set(attrs, "item_id", IntValue(i))
      
      let data_item = TelemetryData::new(
        (task_id * 10.0 + i as Float),
        attrs,
        1234567890L + (task_id * 1000L + i as Long)
      )
      
      // Add data to batch processor
      ConcurrentBatchProcessor::add(batch_processor, data_item)
      
      // Simulate data generation interval
      Thread::sleep(1) // 1ms
    }
  })
  
  // Wait for data generation to complete
  let generation_results = ConcurrentTaskRunner::wait_all(task_results, 10000) // 10s timeout
  
  // Verify all generation tasks completed successfully
  assert_eq(generation_results.length(), concurrent_tasks)
  
  for result in generation_results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
  
  // Wait for batch processing to complete
  Thread::sleep(3000) // 3s for batch processing
  
  // Force flush remaining batches
  let flush_result = ConcurrentBatchProcessor::flush(batch_processor)
  match flush_result {
    Success => assert_true(true)
    Error(_) => assert_true(false) // Flush should succeed
  }
  
  // Verify batch processing results
  let stats = ConcurrentBatchProcessor::get_statistics(batch_processor)
  let expected_data_count = concurrent_tasks * data_items_per_task
  
  assert_eq(stats.items_added, expected_data_count)
  assert_eq(stats.items_processed, expected_data_count)
  assert_eq(stats.batches_processed, (expected_data_count + 99) / 100) // Ceiling division
  assert_true(stats.average_batch_time > 0.0)
  assert_true(stats.max_concurrent_batches > 0)
  
  // Verify no data was lost during processing
  assert_eq(stats.items_dropped, 0)
}

// Test 7: Concurrent Error Handling
test "concurrent error handling" {
  let error_handler = ConcurrentErrorHandler::new()
  
  // Create concurrent tasks that may generate errors
  let concurrent_tasks = 15
  let operations_per_task = 50
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    let error_count = 0
    
    for i in 1..=operations_per_task {
      // Simulate operation that may fail
      let operation_result = if i % 7 == 0 {
        // Every 7th operation fails
        Error(TelemetryError::new(InternalError, "Simulated error from task " + task_id.to_string() + " operation " + i.to_string()))
      } else {
        Success("operation_result_" + i.to_string())
      }
      
      match operation_result {
        Success(result) => {
          // Handle successful operation
          ConcurrentErrorHandler::handle_success(error_handler, result)
        }
        Error(error) => {
          // Handle error
          ConcurrentErrorHandler::handle_error(error_handler, error)
          error_count = error_count + 1
        }
      }
      
      // Simulate operation time
      Thread::sleep(2) // 2ms
    }
    
    // Return error count for this task
    error_count
  })
  
  // Wait for all tasks to complete
  let results = ConcurrentTaskRunner::wait_all(task_results, 15000) // 15s timeout
  
  // Verify all tasks completed successfully
  assert_eq(results.length(), concurrent_tasks)
  
  // Calculate total expected errors
  let expected_total_errors = concurrent_tasks * (operations_per_task / 7)
  
  // Verify error handling statistics
  let error_stats = ConcurrentErrorHandler::get_statistics(error_handler)
  assert_eq(error_stats.total_operations, concurrent_tasks * operations_per_task)
  assert_eq(error_stats.successful_operations, concurrent_tasks * operations_per_task - expected_total_errors)
  assert_eq(error_stats.failed_operations, expected_total_errors)
  
  // Verify error details
  let error_details = ConcurrentErrorHandler::get_error_details(error_handler)
  assert_eq(error_details.length(), expected_total_errors)
  
  for error in error_details {
    assert_eq(error.code, InternalError)
    assert_true(error.message.contains("Simulated error from task"))
    assert_true(error.message.contains("operation"))
  }
  
  // Verify error recovery
  let recovery_result = ConcurrentErrorHandler::attempt_recovery(error_handler)
  match recovery_result {
    Success(recovered_count) => {
      assert_eq(recovered_count, expected_total_errors)
    }
    Error(_) => assert_true(false) // Recovery should succeed
  }
  
  // Verify error statistics after recovery
  let post_recovery_stats = ConcurrentErrorHandler::get_statistics(error_handler)
  assert_eq(post_recovery_stats.recovered_errors, expected_total_errors)
}

// Test 8: Concurrent Resource Pool with Deadlock Prevention
test "concurrent resource pool with deadlock prevention" {
  let resource_pool = DeadlockSafeResourcePool::new(20) // 20 resources in pool
  
  // Create concurrent tasks that acquire multiple resources
  let concurrent_tasks = 16
  let operations_per_task = 30
  
  let task_results = ConcurrentTaskRunner::run(concurrent_tasks, {
    let task_id = ConcurrentTask::current_id()
    
    for i in 1..=operations_per_task {
      // Determine which resources to acquire (randomized to prevent lock ordering)
      let resource_ids = []
      
      let resource1_id = (task_id + i) % 20
      let resource2_id = (task_id + i * 2) % 20
      
      // Ensure different resource IDs
      if resource1_id != resource2_id {
        resource_ids.push(resource1_id)
        resource_ids.push(resource2_id)
      } else {
        resource_ids.push(resource1_id)
      }
      
      // Acquire resources with deadlock prevention
      let acquired_resources = []
      let acquisition_success = true
      
      for resource_id in resource_ids {
        let resource = DeadlockSafeResourcePool::acquire_with_timeout(resource_pool, resource_id, 1000)
        match resource {
          Some(res) => acquired_resources.push(res)
          None => {
            acquisition_success = false
            break
          }
        }
      }
      
      if acquisition_success {
        // Use resources
        for res in acquired_resources {
          let result = Resource::use_concurrent(res, {
            // Simulate resource usage
            Thread::sleep(3) // 3ms
            
            // Set resource-specific data
            Resource::set_attribute_concurrent(res, "task_id", IntValue(task_id))
            Resource::set_attribute_concurrent(res, "operation_id", IntValue(i))
            
            task_id * 1000 + i
          })
          
          match result {
            Some(value) => assert_eq(value, task_id * 1000 + i)
            None => assert_true(false) // Resource usage should succeed
          }
        }
        
        // Release resources in reverse order (important for deadlock prevention)
        for res in acquired_resources.reverse() {
          DeadlockSafeResourcePool::release(resource_pool, res)
        }
      } else {
        // Release any partially acquired resources
        for res in acquired_resources {
          DeadlockSafeResourcePool::release(resource_pool, res)
        }
      }
      
      // Simulate time between operations
      Thread::sleep(1) // 1ms
    }
  })
  
  // Wait for all tasks to complete
  let results = ConcurrentTaskRunner::wait_all(task_results, 30000) // 30s timeout
  
  // Verify all tasks completed successfully
  assert_eq(results.length(), concurrent_tasks)
  
  for result in results {
    match result {
      Success => assert_true(true)
      Error(_) => assert_true(false) // No task should fail
    }
  }
  
  // Verify no deadlocks occurred
  let deadlock_stats = DeadlockSafeResourcePool::get_deadlock_statistics(resource_pool)
  assert_eq(deadlock_stats.deadlock_preventions, 0) // No deadlock prevention should be needed with proper usage
  assert_eq(deadlock_stats.deadlock_detections, 0) // No deadlocks should be detected
  assert_eq(deadlock_stats.timeout_acquisitions, 0) // No timeout acquisitions should occur
  
  // Verify all resources are available
  assert_eq(DeadlockSafeResourcePool::available_count(resource_pool), 20)
  
  // Verify resource usage statistics
  let usage_stats = DeadlockSafeResourcePool::get_usage_statistics(resource_pool)
  assert_eq(usage_stats.total_acquisitions, concurrent_tasks * operations_per_task * 2) // 2 resources per operation
  assert_eq(usage_stats.total_releases, concurrent_tasks * operations_per_task * 2)
  assert_eq(usage_stats.max_concurrent_resources, concurrent_tasks * 2) // Max concurrent resources used
}