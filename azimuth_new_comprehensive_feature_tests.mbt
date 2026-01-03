// Azimuth Telemetry System - New Comprehensive Feature Tests
// This file contains 10 new comprehensive test cases for various Azimuth features

// Test 1: Advanced Network Communication Tests
test "advanced network communication with retry and timeout" {
  // Test network client configuration
  let config = NetworkConfig::new()
    .with_timeout(5000)
    .with_retry_count(3)
    .with_retry_backoff(1000)
    .with_circuit_breaker_threshold(5)
  
  let client = NetworkClient::with_config(config)
  
  // Test successful request
  let request = NetworkRequest::new("GET", "https://api.example.com/telemetry")
    .with_header("Content-Type", "application/json")
    .with_header("Authorization", "Bearer token123")
    .with_timeout(3000)
  
  let response = NetworkClient::send(client, request)
  
  // Test response handling
  match response {
    Success(resp) => {
      assert_eq(Response::status_code(resp), 200)
      assert_true(Response::has_header(resp, "Content-Type"))
    }
    RetryExceeded(attempts) => {
      assert_true(attempts >= 3)
    }
    Timeout => {
      assert_true(true) // Expected for timeout scenarios
    }
    ConnectionError(error) => {
      assert_true(error.length() > 0)
    }
  }
  
  // Test circuit breaker functionality
  for i = 0; i < 6; i = i + 1 {
    let failing_request = NetworkRequest::new("GET", "https://api.example.com/fail")
    let failing_response = NetworkClient::send(client, failing_request)
    
    if i >= 5 {
      match failing_response {
        CircuitBreakerOpen => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
}

// Test 2: Data Serialization Performance Tests
test "data serialization performance with large datasets" {
  // Create large dataset for performance testing
  let mut large_dataset = []
  for i = 0; i < 10000; i = i + 1 {
    let entry = {
      "id": i,
      "timestamp": 1234567890L + i,
      "trace_id": "trace_" + i.to_string(),
      "span_id": "span_" + i.to_string(),
      "attributes": [
        ("key1", "value1_" + i.to_string()),
        ("key2", "value2_" + i.to_string()),
        ("key3", i * 10)
      ]
    }
    large_dataset = large_dataset + [entry]
  }
  
  // Test JSON serialization performance
  let start_time = Time::now()
  let json_result = JsonSerializer::serialize(large_dataset)
  let json_time = Time::now() - start_time
  
  match json_result {
    Success(json_data) => {
      assert_true(json_data.length() > 100000) // Large JSON string
      assert_true(json_time < 5000) // Should complete within 5 seconds
    }
    Error(error) => {
      assert_true(false) // Serialization should not fail
    }
  }
  
  // Test binary serialization performance
  let start_time = Time::now()
  let binary_result = BinarySerializer::serialize(large_dataset)
  let binary_time = Time::now() - start_time
  
  match binary_result {
    Success(binary_data) => {
      assert_true(binary_data.length() > 50000) // Binary should be smaller than JSON
      assert_true(binary_time < 3000) // Should be faster than JSON
    }
    Error(error) => {
      assert_true(false) // Serialization should not fail
    }
  }
  
  // Test deserialization performance
  match json_result {
    Success(json_data) => {
      let start_time = Time::now()
      let deserialized_result = JsonSerializer::deserialize(json_data)
      let deserialization_time = Time::now() - start_time
      
      match deserialized_result {
        Success(data) => {
          assert_eq(data.length(), 10000)
          assert_true(deserialization_time < 5000)
        }
        Error(_) => {
          assert_true(false)
        }
      }
    }
    _ => assert_true(false)
  }
}

// Test 3: Distributed Tracing Consistency Tests
test "distributed tracing consistency across service boundaries" {
  // Create trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, parent_span_id, true, "active")
  
  // Service A: Create parent span
  let service_a_tracer = TracerProvider::get_tracer("service-a")
  let parent_span = Tracer::start_span(service_a_tracer, "service-a-operation", span_context)
  
  // Add attributes and events
  Span::add_attribute(parent_span, "service.name", "service-a")
  Span::add_attribute(parent_span, "operation.type", "database.query")
  Span::add_event(parent_span, "query.started", Some([("query.sql", "SELECT * FROM users")]))
  
  // Simulate service A calling service B
  let service_b_headers = []
  let propagator = TraceContextPropagator::new()
  TraceContextPropagator::inject(propagator, span_context, service_b_headers)
  
  // Service B: Extract context and create child span
  let extracted_context = TraceContextPropagator::extract(propagator, service_b_headers)
  let service_b_tracer = TracerProvider::get_tracer("service-b")
  let child_span = Tracer::start_span(service_b_tracer, "service-b-operation", extracted_context)
  
  // Add attributes and events to child span
  Span::add_attribute(child_span, "service.name", "service-b")
  Span::add_attribute(child_span, "operation.type", "cache.get")
  Span::add_event(child_span, "cache.hit", Some([("cache.key", "user:123")]))
  
  // Verify trace consistency
  assert_eq(SpanContext::trace_id(Span::span_context(child_span)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(parent_span)), trace_id)
  assert_ne(SpanContext::span_id(Span::span_context(child_span)), parent_span_id)
  
  // End spans in correct order
  Span::end(child_span)
  Span::end(parent_span)
  
  // Verify span relationships
  let trace_data = TraceExporter::get_trace_data(trace_id)
  match trace_data {
    Some(spans) => {
      assert_eq(spans.length(), 2)
      
      let parent_span_data = spans[0]
      let child_span_data = spans[1]
      
      assert_eq(SpanData::name(parent_span_data), "service-a-operation")
      assert_eq(SpanData::name(child_span_data), "service-b-operation")
      assert_eq(SpanData::trace_id(parent_span_data), trace_id)
      assert_eq(SpanData::trace_id(child_span_data), trace_id)
      assert_eq(SpanData::parent_span_id(child_span_data), Some(parent_span_id))
    }
    None => {
      assert_true(false) // Trace data should be available
    }
  }
}

// Test 4: Concurrent Safety Boundary Tests
test "concurrent safety with shared resources and race conditions" {
  // Create shared resource
  let shared_counter = AtomicCounter::new(0)
  let shared_attributes = ConcurrentAttributes::new()
  let shared_spans = ConcurrentSpanCollection::new()
  
  // Create multiple threads to test concurrent access
  let thread_count = 10
  let operations_per_thread = 1000
  let threads = []
  
  for thread_id = 0; thread_id < thread_count; thread_id = thread_id + 1 {
    let thread = Thread::spawn(fn() {
      // Test atomic counter operations
      for i = 0; i < operations_per_thread; i = i + 1 {
        AtomicCounter::increment(shared_counter)
        AtomicCounter::add(shared_counter, thread_id)
        
        // Test concurrent attribute operations
        let key = "thread_" + thread_id.to_string() + "_attr_" + i.to_string()
        let value = "value_" + i.to_string()
        ConcurrentAttributes::set(shared_attributes, key, StringValue(value))
        
        // Test concurrent span operations
        let span_name = "thread_" + thread_id.to_string() + "_span_" + i.to_string()
        let span = ConcurrentSpanCollection::create_span(shared_spans, span_name)
        ConcurrentSpanCollection::add_attribute(span, "thread_id", IntValue(thread_id))
        ConcurrentSpanCollection::add_attribute(span, "operation_id", IntValue(i))
      }
    })
    threads = threads + [thread]
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify final state consistency
  let expected_counter_value = thread_count * operations_per_thread + 
                              (thread_count * (thread_count - 1) / 2) * operations_per_thread
  
  assert_eq(AtomicCounter::get(shared_counter), expected_counter_value)
  
  // Verify attributes consistency
  let attribute_count = ConcurrentAttributes::size(shared_attributes)
  assert_eq(attribute_count, thread_count * operations_per_thread)
  
  // Verify spans consistency
  let span_count = ConcurrentSpanCollection::size(shared_spans)
  assert_eq(span_count, thread_count * operations_per_thread)
  
  // Test race condition detection
  let race_detector = RaceConditionDetector::new()
  let detected_races = RaceConditionDetector::analyze(race_detector, shared_attributes)
  
  // No races should be detected with proper synchronization
  assert_eq(detected_races.length(), 0)
}

// Test 5: Exception Recovery Mechanism Tests
test "exception recovery mechanisms with fallback strategies" {
  // Create recovery manager with multiple fallback strategies
  let recovery_manager = RecoveryManager::new()
    .with_fallback_strategy(RetryStrategy::exponential_backoff(100, 3))
    .with_fallback_strategy(FailoverStrategy::primary_to_secondary())
    .with_fallback_strategy(CircuitBreakerStrategy::threshold_based(5))
    .with_fallback_strategy(GracefulDegradationStrategy::feature_based())
  
  // Test operation that fails initially but succeeds after retries
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error("Temporary failure")
    } else {
      Success("Operation succeeded after retries")
    }
  }
  
  let result = RecoveryManager::execute_with_recovery(recovery_manager, flaky_operation)
  match result {
    Success(value) => {
      assert_eq(value, "Operation succeeded after retries")
      assert_eq(attempt_count, 3)
    }
    Error(_) => {
      assert_true(false) // Should succeed after retries
    }
  }
  
  // Test operation that always fails and triggers fallback
  attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    Error("Persistent failure")
  }
  
  let fallback_operation = fn() {
    Success("Fallback operation succeeded")
  }
  
  let result_with_fallback = RecoveryManager::execute_with_fallback(
    recovery_manager, 
    failing_operation, 
    fallback_operation
  )
  
  match result_with_fallback {
    Success(value) => {
      assert_eq(value, "Fallback operation succeeded")
      assert_true(attempt_count >= 3) // Should retry before fallback
    }
    Error(_) => {
      assert_true(false) // Fallback should succeed
    }
  }
  
  // Test circuit breaker activation
  let circuit_breaker_manager = RecoveryManager::new()
    .with_fallback_strategy(CircuitBreakerStrategy::threshold_based(3))
  
  for i = 0; i < 5; i = i + 1 {
    let result = RecoveryManager::execute_with_recovery(
      circuit_breaker_manager, 
      failing_operation
    )
    
    if i >= 3 {
      match result {
        CircuitBreakerOpen => assert_true(true) // Expected after threshold
        _ => assert_true(false)
      }
    }
  }
  
  // Test graceful degradation
  let degradation_manager = RecoveryManager::new()
    .with_fallback_strategy(GracefulDegradationStrategy::feature_based())
  
  let degraded_result = RecoveryManager::execute_with_degradation(
    degradation_manager,
    failing_operation,
    ["feature1", "feature2"] // Disable these features on failure
  )
  
  match degraded_result {
    DegradedResponse(features, partial_data) => {
      assert_eq(features, ["feature1", "feature2"])
      assert_true(partial_data.length() > 0)
    }
    _ => assert_true(false)
  }
}

// Test 6: Resource Lifecycle Management Tests
test "resource lifecycle management with proper cleanup" {
  // Create resource manager
  let resource_manager = ResourceManager::new()
  
  // Register various resources
  let db_connection = ResourceManager::register(
    resource_manager, 
    "database.connection", 
    DatabaseConnection::new("postgresql://localhost:5432/azimuth")
  )
  
  let cache_client = ResourceManager::register(
    resource_manager, 
    "cache.client", 
    CacheClient::new("redis://localhost:6379")
  )
  
  let file_handle = ResourceManager::register(
    resource_manager, 
    "file.handle", 
    FileHandle::open("/tmp/azimuth.log")
  )
  
  let network_socket = ResourceManager::register(
    resource_manager, 
    "network.socket", 
    NetworkSocket::connect("localhost", 8080)
  )
  
  // Test resource acquisition
  let acquired_db = ResourceManager::acquire(resource_manager, "database.connection")
  match acquired_db {
    Some(conn) => {
      assert_true(DatabaseConnection::is_connected(conn))
      
      // Use resource
      let result = DatabaseConnection::query(conn, "SELECT 1")
      match result {
        Success(rows) => assert_eq(rows.length(), 1)
        Error(_) => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test resource pool behavior
  let pool = ResourcePool::new(5, fn() { DatabaseConnection::new("postgresql://localhost:5432/azimuth") })
  
  // Acquire multiple connections
  let connections = []
  for i = 0; i < 3; i = i + 1 {
    let conn = ResourcePool::acquire(pool)
    connections = connections + [conn]
  }
  
  assert_eq(connections.length(), 3)
  assert_eq(ResourcePool::available_count(pool), 2)
  
  // Return connections to pool
  for conn in connections {
    ResourcePool::release(pool, conn)
  }
  
  assert_eq(ResourcePool::available_count(pool), 5)
  
  // Test resource cleanup on shutdown
  let cleanup_tracker = CleanupTracker::new()
  ResourceManager::register_cleanup_handler(resource_manager, cleanup_tracker)
  
  // Simulate shutdown process
  ResourceManager::shutdown(resource_manager)
  
  // Verify all resources were properly cleaned up
  let cleaned_resources = CleanupTracker::cleaned_resources(cleanup_tracker)
  assert_eq(cleaned_resources.length(), 4)
  assert_true(cleaned_resources.contains("database.connection"))
  assert_true(cleaned_resources.contains("cache.client"))
  assert_true(cleaned_resources.contains("file.handle"))
  assert_true(cleaned_resources.contains("network.socket"))
  
  // Test resource leak detection
  let leak_detector = ResourceLeakDetector::new()
  let leaked_resources = ResourceLeakDetector::detect_leaks(leak_detector, resource_manager)
  
  // No leaks should be detected after proper shutdown
  assert_eq(leaked_resources.length(), 0)
}

// Test 7: Real-time Stream Processing Integrity Tests
test "real-time stream processing with integrity guarantees" {
  // Create stream processor
  let stream_processor = StreamProcessor::new()
    .with_buffer_size(1000)
    .with_batch_size(100)
    .with_processing_timeout(5000)
    .with_integrity_checking(true)
  
  // Create test data stream
  let test_events = []
  for i = 0; i < 1000; i = i + 1 {
    let event = StreamEvent::new("telemetry.data", i)
      .with_attribute("event_id", i.to_string())
      .with_attribute("timestamp", (1234567890L + i).to_string())
      .with_attribute("source", "test-generator")
    test_events = test_events + [event]
  }
  
  // Set up processing pipeline
  let pipeline = StreamPipeline::new()
    .add_stage(FilterStage::new(fn(event) { 
      StreamEvent::attribute(event, "event_id").to_int() % 2 == 0 
    }))
    .add_stage(TransformStage::new(fn(event) {
      let event_id = StreamEvent::attribute(event, "event_id").to_int()
      StreamEvent::with_attribute(event, "processed", "true")
        .with_attribute("doubled_id", (event_id * 2).to_string())
    }))
    .add_stage(AggregationStage::new("1min", fn(events) {
      let count = events.length()
      let sum = events.fold(0, fn(acc, event) {
        acc + StreamEvent::attribute(event, "event_id").to_int()
      })
      AggregatedData::new("1min", count, sum)
    }))
  
  // Process stream
  let start_time = Time::now()
  let processing_result = StreamProcessor::process(stream_processor, test_events, pipeline)
  let processing_time = Time::now() - start_time
  
  match processing_result {
    Success(processed_events) => {
      // Verify filtering worked (only even events should remain)
      assert_eq(processed_events.length(), 500)
      
      // Verify transformation worked
      for event in processed_events {
        assert_eq(StreamEvent::attribute(event, "processed"), "true")
        let event_id = StreamEvent::attribute(event, "event_id").to_int()
        assert_eq(StreamEvent::attribute(event, "doubled_id").to_int(), event_id * 2)
      }
      
      // Verify processing time is reasonable
      assert_true(processing_time < 10000)
    }
    Error(error) => {
      assert_true(false) // Processing should not fail
    }
  }
  
  // Test integrity checking
  let integrity_checker = StreamIntegrityChecker::new()
  let integrity_result = StreamIntegrityChecker::verify(integrity_checker, test_events, processed_events)
  
  match integrity_result {
    IntegrityReport(missing_events, duplicate_events, corrupted_events) => {
      assert_eq(missing_events.length(), 500) // Odd events filtered out
      assert_eq(duplicate_events.length(), 0) // No duplicates
      assert_eq(corrupted_events.length(), 0) // No corruption
    }
  }
  
  // Test backpressure handling
  let slow_consumer = SlowConsumer::new(100) // 100ms processing delay
  let backpressure_result = StreamProcessor::process_with_consumer(
    stream_processor, 
    test_events, 
    slow_consumer
  )
  
  match backpressure_result {
    Success(events) => {
      assert_eq(events.length(), 1000) // All events should be processed
    }
    BackpressureApplied(dropped_count) => {
      assert_true(dropped_count < 100) // Some events may be dropped under extreme backpressure
    }
  }
  
  // Test exactly-once processing guarantee
  let exactly_once_processor = StreamProcessor::new()
    .with_processing_mode(ExactlyOnce)
    .with_checkpoint_store(CheckpointStore::in_memory())
  
  let exactly_once_result = StreamProcessor::process(
    exactly_once_processor, 
    test_events, 
    pipeline
  )
  
  match exactly_once_result {
    Success(events) => {
      assert_eq(events.length(), 500)
      
      // Verify no duplicate processing
      let processed_ids = []
      for event in events {
        let event_id = StreamEvent::attribute(event, "event_id")
        assert_false(processed_ids.contains(event_id))
        processed_ids = processed_ids + [event_id]
      }
    }
    Error(_) => {
      assert_true(false)
    }
  }
}

// Test 8: Configuration Dynamic Update Tests
test "configuration dynamic updates without service restart" {
  // Create configuration manager
  let config_manager = ConfigurationManager::new()
    .with_source(FileConfigurationSource::new("/etc/azimuth/config.json"))
    .with_source(EnvironmentConfigurationSource::new())
    .with_source(RemoteConfigurationSource::new("https://config.azimuth.com/api"))
  
  // Initialize with default configuration
  let default_config = {
    "telemetry.enabled": true,
    "telemetry.sampling_rate": 0.1,
    "telemetry.batch_size": 100,
    "network.timeout": 5000,
    "network.retry_count": 3,
    "logging.level": "INFO",
    "logging.format": "json"
  }
  
  ConfigurationManager::load_defaults(config_manager, default_config)
  
  // Verify initial configuration
  assert_eq(ConfigurationManager::get_bool(config_manager, "telemetry.enabled"), true)
  assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate"), 0.1)
  assert_eq(ConfigurationManager::get_int(config_manager, "telemetry.batch_size"), 100)
  assert_eq(ConfigurationManager::get_int(config_manager, "network.timeout"), 5000)
  assert_eq(ConfigurationManager::get_int(config_manager, "network.retry_count"), 3)
  assert_eq(ConfigurationManager::get_string(config_manager, "logging.level"), "INFO")
  assert_eq(ConfigurationManager::get_string(config_manager, "logging.format"), "json")
  
  // Register configuration change listeners
  let telemetry_listener = ConfigurationChangeListener::new(
    ["telemetry.sampling_rate", "telemetry.batch_size"],
    fn(key, old_value, new_value) {
      // Apply telemetry configuration changes
      if key == "telemetry.sampling_rate" {
        TelemetryManager::update_sampling_rate(new_value.to_float())
      } else if key == "telemetry.batch_size" {
        TelemetryManager::update_batch_size(new_value.to_int())
      }
    }
  )
  
  let network_listener = ConfigurationChangeListener::new(
    ["network.timeout", "network.retry_count"],
    fn(key, old_value, new_value) {
      // Apply network configuration changes
      if key == "network.timeout" {
        NetworkManager::update_timeout(new_value.to_int())
      } else if key == "network.retry_count" {
        NetworkManager::update_retry_count(new_value.to_int())
      }
    }
  )
  
  ConfigurationManager::add_listener(config_manager, telemetry_listener)
  ConfigurationManager::add_listener(config_manager, network_listener)
  
  // Test dynamic configuration update
  let updated_config = {
    "telemetry.sampling_rate": 0.5,
    "telemetry.batch_size": 200,
    "network.timeout": 10000,
    "network.retry_count": 5,
    "logging.level": "DEBUG"
  }
  
  ConfigurationManager::update(config_manager, updated_config)
  
  // Verify configuration changes were applied
  assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate"), 0.5)
  assert_eq(ConfigurationManager::get_int(config_manager, "telemetry.batch_size"), 200)
  assert_eq(ConfigurationManager::get_int(config_manager, "network.timeout"), 10000)
  assert_eq(ConfigurationManager::get_int(config_manager, "network.retry_count"), 5)
  assert_eq(ConfigurationManager::get_string(config_manager, "logging.level"), "DEBUG")
  assert_eq(ConfigurationManager::get_string(config_manager, "logging.format"), "json") // Unchanged
  
  // Verify configuration change notifications were sent
  let change_history = ConfigurationManager::get_change_history(config_manager)
  assert_eq(change_history.length(), 5)
  
  // Test configuration validation
  let invalid_config = {
    "telemetry.sampling_rate": 1.5, // Invalid: should be between 0 and 1
    "network.timeout": -1000, // Invalid: should be positive
    "network.retry_count": -1 // Invalid: should be non-negative
  }
  
  let validation_result = ConfigurationManager::validate_update(config_manager, invalid_config)
  match validation_result {
    ValidationSuccess => assert_true(false) // Should fail validation
    ValidationError(errors) => {
      assert_eq(errors.length(), 3)
      assert_true(errors.contains("telemetry.sampling_rate must be between 0 and 1"))
      assert_true(errors.contains("network.timeout must be positive"))
      assert_true(errors.contains("network.retry_count must be non-negative"))
    }
  }
  
  // Test configuration rollback
  let rollback_result = ConfigurationManager::rollback(config_manager)
  match rollback_result {
    Success(rolled_back_config) => {
      assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate"), 0.1)
      assert_eq(ConfigurationManager::get_int(config_manager, "telemetry.batch_size"), 100)
      assert_eq(ConfigurationManager::get_int(config_manager, "network.timeout"), 5000)
      assert_eq(ConfigurationManager::get_int(config_manager, "network.retry_count"), 3)
    }
    Error(_) => {
      assert_true(false) // Rollback should succeed
    }
  }
  
  // Test configuration hot-reload from file
  let temp_config_file = TempFile::create()
  TempFile::write(temp_config_file, """
  {
    "telemetry.enabled": false,
    "telemetry.sampling_rate": 0.2,
    "logging.level": "WARN"
  }
  """)
  
  let reload_result = ConfigurationManager::reload_from_file(config_manager, TempFile::path(temp_config_file))
  match reload_result {
    Success(_) => {
      assert_eq(ConfigurationManager::get_bool(config_manager, "telemetry.enabled"), false)
      assert_eq(ConfigurationManager::get_float(config_manager, "telemetry.sampling_rate"), 0.2)
      assert_eq(ConfigurationManager::get_string(config_manager, "logging.level"), "WARN")
    }
    Error(_) => {
      assert_true(false) // Reload should succeed
    }
  }
}

// Test 9: Memory Leak Prevention Tests
test "memory leak prevention with automatic resource cleanup" {
  // Create memory leak detector
  let leak_detector = MemoryLeakDetector::new()
  MemoryLeakDetector::start_monitoring(leak_detector)
  
  // Test 1: Large object creation and cleanup
  let large_objects = []
  for i = 0; i < 1000; i = i + 1 {
    let large_object = LargeObject::new(10000) // 10KB each
    large_objects = large_objects + [large_object]
  }
  
  let memory_before_cleanup = MemoryLeakDetector::get_memory_usage(leak_detector)
  
  // Explicitly clean up large objects
  for obj in large_objects {
    LargeObject::cleanup(obj)
  }
  large_objects = []
  
  // Force garbage collection if available
  System::gc()
  
  let memory_after_cleanup = MemoryLeakDetector::get_memory_usage(leak_detector)
  let memory_freed = memory_before_cleanup - memory_after_cleanup
  
  assert_true(memory_freed > 8000000) // At least 8MB should be freed
  
  // Test 2: Event listener cleanup
  let event_emitter = EventEmitter::new()
  let listeners = []
  
  // Register many listeners
  for i = 0; i < 1000; i = i + 1 {
    let listener = EventListener::new("test.event", fn(event) {
      // Process event
    })
    EventEmitter::add_listener(event_emitter, listener)
    listeners = listeners + [listener]
  }
  
  // Remove all listeners
  for listener in listeners {
    EventEmitter::remove_listener(event_emitter, listener)
  }
  listeners = []
  
  // Verify listeners were cleaned up
  assert_eq(EventEmitter::listener_count(event_emitter), 0)
  
  // Test 3: Cache cleanup
  let cache = LRUCache::new(1000)
  
  // Fill cache beyond capacity
  for i = 0; i < 2000; i = i + 1 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    LRUCache::put(cache, key, value)
  }
  
  // Verify cache size is maintained
  assert_eq(LRUCache::size(cache), 1000)
  
  // Clear cache
  LRUCache::clear(cache)
  
  // Verify cache is empty
  assert_eq(LRUCache::size(cache), 0)
  
  // Test 4: Connection pool cleanup
  let connection_pool = ConnectionPool::new(100, fn() {
    DatabaseConnection::new("postgresql://localhost:5432/azimuth")
  })
  
  // Acquire many connections
  let connections = []
  for i = 0; i < 50; i = i + 1 {
    let conn = ConnectionPool::acquire(connection_pool)
    connections = connections + [conn]
  }
  
  // Return all connections
  for conn in connections {
    ConnectionPool::release(connection_pool, conn)
  }
  connections = []
  
  // Close connection pool
  ConnectionPool::close(connection_pool)
  
  // Verify connections were closed
  assert_eq(ConnectionPool::active_connections(connection_pool), 0)
  
  // Test 5: Buffer cleanup
  let buffers = []
  
  // Create many buffers
  for i = 0; i < 1000; i = i + 1 {
    let buffer = Buffer::new(10000) // 10KB each
    Buffer::fill(buffer, i.to_int() % 256)
    buffers = buffers + [buffer]
  }
  
  // Process and clean up buffers
  for buffer in buffers {
    // Process buffer data
    let checksum = Buffer::checksum(buffer)
    assert_true(checksum > 0)
    
    // Clean up buffer
    Buffer::cleanup(buffer)
  }
  buffers = []
  
  // Test 6: Memory leak detection
  let leak_report = MemoryLeakDetector::generate_report(leak_detector)
  
  match leak_report {
    LeakReport(leaked_objects, total_leaked_bytes) => {
      // Verify no significant memory leaks
      assert_true(total_leaked_bytes < 1000000) // Less than 1MB
      
      // Check specific object types
      let large_object_leaks = LeakReport::objects_of_type(leak_report, "LargeObject")
      let listener_leaks = LeakReport::objects_of_type(leak_report, "EventListener")
      let buffer_leaks = LeakReport::objects_of_type(leak_report, "Buffer")
      
      assert_eq(large_object_leaks.length(), 0)
      assert_eq(listener_leaks.length(), 0)
      assert_eq(buffer_leaks.length(), 0)
    }
  }
  
  // Test 7: Memory pressure handling
  let memory_monitor = MemoryMonitor::new()
  MemoryMonitor::set_threshold(memory_monitor, 0.8) // 80% of available memory
  
  // Simulate memory pressure
  let memory_intensive_objects = []
  for i = 0; i < 10000; i = i + 1 {
    let obj = LargeObject::new(100000) // 100KB each
    
    // Check memory pressure
    if MemoryMonitor::is_pressure_detected(memory_monitor) {
      // Trigger cleanup
      for pressure_obj in memory_intensive_objects {
        LargeObject::cleanup(pressure_obj)
      }
      memory_intensive_objects = []
      System::gc()
      break
    }
    
    memory_intensive_objects = memory_intensive_objects + [obj]
  }
  
  // Verify memory pressure was handled
  assert_true(MemoryMonitor::pressure_events(memory_monitor) > 0)
  
  // Clean up remaining objects
  for obj in memory_intensive_objects {
    LargeObject::cleanup(obj)
  }
  
  // Stop monitoring
  MemoryLeakDetector::stop_monitoring(leak_detector)
}

// Test 10: Internationalization and Localization Tests
test "internationalization and localization with multiple locales" {
  // Create internationalization manager
  let i18n_manager = I18nManager::new()
    .with_default_locale("en-US")
    .with_fallback_locale("en")
    .with_supported_locales(["en-US", "zh-CN", "ja-JP", "fr-FR", "de-DE"])
  
  // Load translation resources
  I18nManager::load_translations(i18n_manager, "en-US", {
    "telemetry.enabled": "Telemetry is enabled",
    "telemetry.disabled": "Telemetry is disabled",
    "telemetry.sampling_rate": "Sampling rate is {rate}",
    "error.connection_failed": "Connection to {server} failed: {reason}",
    "metric.count": "{count, plural, =0 {No items} =1 {One item} other {# items}}",
    "duration.seconds": "{seconds, plural, =1 {1 second} other {# seconds}}"
  })
  
  I18nManager::load_translations(i18n_manager, "zh-CN", {
    "telemetry.enabled": "遥测已启用",
    "telemetry.disabled": "遥测已禁用",
    "telemetry.sampling_rate": "采样率为 {rate}",
    "error.connection_failed": "连接到 {server} 失败: {reason}",
    "metric.count": "{count, plural, =0 {无项目} =1 {一个项目} other {# 个项目}}",
    "duration.seconds": "{seconds, plural, =1 {1 秒} other {# 秒}}"
  })
  
  I18nManager::load_translations(i18n_manager, "ja-JP", {
    "telemetry.enabled": "テレメトリが有効です",
    "telemetry.disabled": "テレメトリが無効です",
    "telemetry.sampling_rate": "サンプリングレートは {rate} です",
    "error.connection_failed": "{server} への接続に失敗しました: {reason}",
    "metric.count": "{count, plural, =0 {アイテムなし} =1 {1つのアイテム} other {#つのアイテム}}",
    "duration.seconds": "{seconds, plural, =1 {1秒} other {#秒}}"
  })
  
  // Test basic translation
  I18nManager::set_locale(i18n_manager, "en-US")
  assert_eq(I18nManager::translate(i18n_manager, "telemetry.enabled"), "Telemetry is enabled")
  
  I18nManager::set_locale(i18n_manager, "zh-CN")
  assert_eq(I18nManager::translate(i18n_manager, "telemetry.enabled"), "遥测已启用")
  
  I18nManager::set_locale(i18n_manager, "ja-JP")
  assert_eq(I18nManager::translate(i18n_manager, "telemetry.enabled"), "テレメトリが有効です")
  
  // Test parameter substitution
  I18nManager::set_locale(i18n_manager, "en-US")
  let en_result = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.sampling_rate", 
    [("rate", "0.1")]
  )
  assert_eq(en_result, "Sampling rate is 0.1")
  
  I18nManager::set_locale(i18n_manager, "zh-CN")
  let zh_result = I18nManager::translate_with_params(
    i18n_manager, 
    "telemetry.sampling_rate", 
    [("rate", "0.1")]
  )
  assert_eq(zh_result, "采样率为 0.1")
  
  // Test pluralization
  I18nManager::set_locale(i18n_manager, "en-US")
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "metric.count", [("count", "0")]),
    "No items"
  )
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "metric.count", [("count", "1")]),
    "One item"
  )
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "metric.count", [("count", "5")]),
    "5 items"
  )
  
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "duration.seconds", [("seconds", "1")]),
    "1 second"
  )
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "duration.seconds", [("seconds", "10")]),
    "10 seconds"
  )
  
  // Test fallback mechanism
  I18nManager::set_locale(i18n_manager, "es-ES") // Not supported
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.enabled"), 
    "Telemetry is enabled" // Should fallback to en-US
  )
  
  // Test missing translation
  I18nManager::set_locale(i18n_manager, "zh-CN")
  assert_eq(
    I18nManager::translate(i18n_manager, "nonexistent.key"), 
    "nonexistent.key" // Should return key when translation is missing
  )
  
  // Test locale-specific formatting
  I18nManager::set_locale(i18n_manager, "en-US")
  let en_formatted_number = I18nManager::format_number(i18n_manager, 1234567.89)
  assert_eq(en_formatted_number, "1,234,567.89")
  
  let en_formatted_date = I18nManager::format_date(i18n_manager, 1234567890L)
  assert_eq(en_formatted_date, "2/13/2009") // MM/DD/YYYY format
  
  I18nManager::set_locale(i18n_manager, "zh-CN")
  let zh_formatted_number = I18nManager::format_number(i18n_manager, 1234567.89)
  assert_eq(zh_formatted_number, "1,234,567.89") // Chinese uses same decimal separator
  
  let zh_formatted_date = I18nManager::format_date(i18n_manager, 1234567890L)
  assert_eq(zh_formatted_date, "2009/2/13") // YYYY/MM/DD format
  
  I18nManager::set_locale(i18n_manager, "ja-JP")
  let ja_formatted_number = I18nManager::format_number(i18n_manager, 1234567.89)
  assert_eq(ja_formatted_number, "1,234,567.89") // Japanese uses same decimal separator
  
  let ja_formatted_date = I18nManager::format_date(i18n_manager, 1234567890L)
  assert_eq(ja_formatted_date, "2009/02/13") // YYYY/MM/DD format with leading zeros
  
  // Test RTL (Right-to-Left) language support
  I18nManager::load_translations(i18n_manager, "ar-SA", {
    "telemetry.enabled": "القياس عن بعد مفعّل",
    "error.connection_failed": "فشل الاتصال بالخادم {server}: {reason}"
  })
  
  I18nManager::add_supported_locale(i18n_manager, "ar-SA")
  I18nManager::set_locale(i18n_manager, "ar-SA")
  
  let ar_result = I18nManager::translate_with_params(
    i18n_manager, 
    "error.connection_failed", 
    [("server", "example.com"), ("reason", "timeout")]
  )
  assert_eq(ar_result, "فشل الاتصال بالخادم example.com: timeout")
  
  // Test locale detection from request
  let request_headers = [
    ("Accept-Language", "zh-CN,zh;q=0.9,en;q=0.8")
  ]
  
  let detected_locale = I18nManager::detect_locale(i18n_manager, request_headers)
  assert_eq(detected_locale, "zh-CN")
  
  let request_headers_en = [
    ("Accept-Language", "en-US,en;q=0.9")
  ]
  
  let detected_locale_en = I18nManager::detect_locale(i18n_manager, request_headers_en)
  assert_eq(detected_locale_en, "en-US")
  
  // Test locale-specific error messages
  I18nManager::set_locale(i18n_manager, "en-US")
  let en_error = I18nManager::localize_error(i18n_manager, "CONNECTION_FAILED", [
    ("server", "api.example.com"),
    ("reason", "timeout")
  ])
  assert_eq(en_error, "Connection to api.example.com failed: timeout")
  
  I18nManager::set_locale(i18n_manager, "zh-CN")
  let zh_error = I18nManager::localize_error(i18n_manager, "CONNECTION_FAILED", [
    ("server", "api.example.com"),
    ("reason", "timeout")
  ])
  assert_eq(zh_error, "连接到 api.example.com 失败: timeout")
}