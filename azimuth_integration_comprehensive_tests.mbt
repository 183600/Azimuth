// Azimuth Telemetry System - Integration Comprehensive Tests
// This file contains comprehensive integration tests for the telemetry system

// Test 1: End-to-End Telemetry Pipeline Integration
test "end-to-end telemetry pipeline integration" {
  // Initialize telemetry system components
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "integration-test-service")
  TelemetryConfig::set(config, "service.version", "1.0.0")
  TelemetryConfig::set(config, "exporter.endpoint", "http://localhost:4318")
  TelemetryConfig::set(config, "sampling.rate", "1.0")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "integration-test-tracer")
  let meter = TelemetryProvider::get_meter(provider, "integration-test-meter")
  let logger = TelemetryProvider::get_logger(provider, "integration-test-logger")
  
  // Test span creation and propagation
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  Span::set_attribute(parent_span, "operation.type", "integration")
  Span::set_attribute(parent_span, "user.id", "user-123")
  
  // Create child span
  let child_span = Tracer::start_span(tracer, "child-operation", Some(parent_span))
  Span::set_attribute(child_span, "operation.type", "integration")
  Span::set_attribute(child_span, "database.query", "SELECT * FROM users")
  
  // Add events to child span
  Span::add_event(child_span, "query.start", Some([
    ("query", "SELECT * FROM users"),
    ("timestamp", "1234567890")
  ]))
  
  // Simulate database operation
  Thread::sleep(100) // 100ms
  
  Span::add_event(child_span, "query.complete", Some([
    ("rows", "100"),
    ("duration", "100ms")
  ]))
  
  // End child span
  Span::end(child_span)
  
  // Create metrics
  let counter = Meter::create_counter(meter, "operations.count", "Total operations")
  Counter::add(counter, 1.0, Some([
    ("operation.type", "integration"),
    ("status", "success")
  ]))
  
  let histogram = Meter::create_histogram(meter, "operation.duration", "Operation duration", "ms")
  Histogram::record(histogram, 100.0, Some([
    ("operation.type", "integration"),
    ("database", "postgresql")
  ]))
  
  // Create log record
  let log_record = Logger::emit(logger, Info, "Integration test completed", Some([
    ("operation.type", "integration"),
    ("user.id", "user-123"),
    ("duration", "100ms")
  ]))
  
  // End parent span
  Span::end(parent_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data was collected
  let telemetry_collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(telemetry_collector)
  let metrics = TelemetryCollector::get_metrics(telemetry_collector)
  let logs = TelemetryCollector::get_logs(telemetry_collector)
  
  assert_true(spans.length() >= 2) // Parent and child spans
  assert_true(metrics.length() >= 2) // Counter and histogram
  assert_true(logs.length() >= 1) // Log record
  
  // Verify span relationships
  let parent_span_data = spans[0]
  let child_span_data = spans[1]
  
  assert_eq(SpanData::name(parent_span_data), "parent-operation")
  assert_eq(SpanData::name(child_span_data), "child-operation")
  assert_eq(SpanData::trace_id(child_span_data), SpanData::trace_id(parent_span_data))
  assert_eq(SpanData::parent_span_id(child_span_data), Some(SpanData::span_id(parent_span_data)))
  
  // Verify attributes
  let parent_attributes = SpanData::attributes(parent_span_data)
  assert_true(parent_attributes.contains(("operation.type", StringValue("integration"))))
  assert_true(parent_attributes.contains(("user.id", StringValue("user-123"))))
  
  let child_attributes = SpanData::attributes(child_span_data)
  assert_true(child_attributes.contains(("operation.type", StringValue("integration"))))
  assert_true(child_attributes.contains(("database.query", StringValue("SELECT * FROM users"))))
  
  // Verify events
  let child_events = SpanData::events(child_span_data)
  assert_eq(child_events.length(), 2)
  assert_eq(EventData::name(child_events[0]), "query.start")
  assert_eq(EventData::name(child_events[1]), "query.complete")
  
  // Verify metrics
  for metric in metrics {
    match metric {
      CounterMetric(name, data_points) => {
        assert_eq(name, "operations.count")
        assert_true(data_points.length() > 0)
      }
      HistogramMetric(name, data_points) => {
        assert_eq(name, "operation.duration")
        assert_true(data_points.length() > 0)
      }
    }
  }
  
  // Verify logs
  assert_eq(LogData::severity_number(logs[0]), Info)
  match LogData::body(logs[0]) {
    Some(body) => assert_eq(body, "Integration test completed")
    None => assert_true(false)
  }
  
  // Export telemetry data
  let exporter = TelemetryProvider::get_exporter(provider)
  let export_result = TelemetryExporter::export(exporter, telemetry_collector)
  assert_true(export_result)
  
  // Shutdown telemetry provider
  TelemetryProvider::shutdown(provider)
}

// Test 2: Cross-Service Telemetry Propagation Integration
test "cross-service telemetry propagation integration" {
  // Initialize service A
  let service_a_config = TelemetryConfig::new()
  TelemetryConfig::set(service_a_config, "service.name", "service-a")
  TelemetryConfig::set(service_a_config, "service.version", "1.0.0")
  
  let service_a_provider = TelemetryProvider::new(service_a_config)
  let service_a_tracer = TelemetryProvider::get_tracer(service_a_provider, "service-a-tracer")
  
  // Initialize service B
  let service_b_config = TelemetryConfig::new()
  TelemetryConfig::set(service_b_config, "service.name", "service-b")
  TelemetryConfig::set(service_b_config, "service.version", "1.0.0")
  
  let service_b_provider = TelemetryProvider::new(service_b_config)
  let service_b_tracer = TelemetryProvider::get_tracer(service_b_provider, "service-b-tracer")
  
  // Service A creates a span
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a-operation")
  Span::set_attribute(service_a_span, "user.id", "user-123")
  
  // Service A injects context into headers
  let headers = []
  let propagator = TraceContextPropagator::new()
  let injected_headers = propagator.inject(Span::span_context(service_a_span), headers)
  
  // Verify headers contain trace context
  let traceparent_found = injected_headers.any(fn(header) {
    header.0 == "traceparent"
  })
  assert_true(traceparent_found)
  
  // Service B extracts context from headers
  let extracted_context = propagator.extract(injected_headers)
  assert_true(Context::is_valid(extracted_context))
  
  // Service B creates a child span
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b-operation", Some(extracted_context))
  Span::set_attribute(service_b_span, "user.id", "user-123")
  Span::set_attribute(service_b_span, "service", "service-b")
  
  // Service B adds events
  Span::add_event(service_b_span, "service-b-event", Some([
    ("operation", "process-data"),
    ("timestamp", "1234567890")
  ]))
  
  // End spans
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify trace continuity
  let service_a_collector = TelemetryProvider::get_collector(service_a_provider)
  let service_a_spans = TelemetryCollector::get_spans(service_a_collector)
  
  let service_b_collector = TelemetryProvider::get_collector(service_b_provider)
  let service_b_spans = TelemetryCollector::get_spans(service_b_collector)
  
  assert_true(service_a_spans.length() >= 1)
  assert_true(service_b_spans.length() >= 1)
  
  let service_a_span_data = service_a_spans[0]
  let service_b_span_data = service_b_spans[0]
  
  // Verify trace ID is the same
  assert_eq(SpanData::trace_id(service_a_span_data), SpanData::trace_id(service_b_span_data))
  
  // Verify parent-child relationship
  assert_eq(SpanData::parent_span_id(service_b_span_data), Some(SpanData::span_id(service_a_span_data)))
  
  // Verify service names are different
  let service_a_resource = TelemetryCollector::get_resource(service_a_collector)
  let service_b_resource = TelemetryCollector::get_resource(service_b_collector)
  
  let service_a_name = Resource::get_attribute(service_a_resource, "service.name")
  let service_b_name = Resource::get_attribute(service_b_resource, "service.name")
  
  match service_a_name {
    Some(StringValue(name)) => assert_eq(name, "service-a")
    _ => assert_true(false)
  }
  
  match service_b_name {
    Some(StringValue(name)) => assert_eq(name, "service-b")
    _ => assert_true(false)
  }
  
  // Shutdown providers
  TelemetryProvider::shutdown(service_a_provider)
  TelemetryProvider::shutdown(service_b_provider)
}

// Test 3: Database Integration with Telemetry
test "database integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "database-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "database-tracer")
  let meter = TelemetryProvider::get_meter(provider, "database-meter")
  
  // Initialize database connection
  let db_config = DatabaseConfig::new()
  DatabaseConfig::set(db_config, "connection_string", "postgresql://localhost:5432/telemetry")
  DatabaseConfig::set(db_config, "max_connections", "10")
  
  let db_client = DatabaseClient::new(db_config)
  
  // Create database metrics
  let query_counter = Meter::create_counter(meter, "db.query.count", "Database query count")
  let query_duration = Meter::create_histogram(meter, "db.query.duration", "Database query duration", "ms")
  let connection_pool = Meter::create_updown_counter(meter, "db.connection.pool", "Database connection pool size")
  
  // Test database operation with telemetry
  let db_span = Tracer::start_span(tracer, "database.query")
  Span::set_attribute(db_span, "db.system", "postgresql")
  Span::set_attribute(db_span, "db.operation", "SELECT")
  Span::set_attribute(db_span, "db.statement", "SELECT * FROM users WHERE id = $1")
  
  // Record query start
  Span::add_event(db_span, "query.start", Some([
    ("statement", "SELECT * FROM users WHERE id = $1")
  ]))
  
  // Get connection from pool
  let start_time = Time::now()
  let connection = DatabaseClient::get_connection(db_client)
  
  // Update connection pool metric
  match connection {
    Some(_) => {
      let pool_size = DatabaseClient::get_pool_size(db_client)
      UpDownCounter::set(connection_pool, pool_size as Float)
    }
    None => assert_true(false)
  }
  
  // Execute query
  let query_result = DatabaseClient::execute_query(db_client, connection.unwrap(), "SELECT * FROM users WHERE id = $1", [123])
  
  // Record query completion
  let end_time = Time::now()
  let duration = end_time - start_time
  
  Span::add_event(db_span, "query.complete", Some([
    ("rows.returned", "1"),
    ("duration", duration.to_string() + "ms")
  ]))
  
  // Update metrics
  Counter::add(query_counter, 1.0, Some([
    ("db.operation", "SELECT"),
    ("db.status", "success")
  ]))
  
  Histogram::record(query_duration, duration, Some([
    ("db.operation", "SELECT"),
    ("db.table", "users")
  ]))
  
  // Process query result
  match query_result {
    Some(rows) => {
      Span::set_attribute(db_span, "db.rows_affected", rows.length().to_string())
      
      for row in rows {
        let user_id = row.get("id")
        let username = row.get("username")
        
        // Create span for row processing
        let row_span = Tracer::start_span(tracer, "process.user.row", Some(db_span))
        Span::set_attribute(row_span, "user.id", user_id)
        Span::set_attribute(row_span, "user.username", username)
        
        // Simulate processing
        Thread::sleep(10) // 10ms
        
        Span::end(row_span)
      }
    }
    None => {
      Span::set_status(db_span, Error, Some("Query failed"))
      Counter::add(query_counter, 1.0, Some([
        ("db.operation", "SELECT"),
        ("db.status", "error")
      ]))
    }
  }
  
  // Release connection
  DatabaseClient::release_connection(db_client, connection.unwrap())
  
  // End span
  Span::end(db_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 2) // Main query span and row processing spans
  assert_true(metrics.length() >= 3) // Counter, histogram, and updown counter
  
  // Verify main span
  let main_span = spans[0]
  assert_eq(SpanData::name(main_span), "database.query")
  assert_true(SpanData::attributes(main_span).contains(("db.system", StringValue("postgresql"))))
  
  // Shutdown
  DatabaseClient::close(db_client)
  TelemetryProvider::shutdown(provider)
}

// Test 4: HTTP Client Integration with Telemetry
test "http client integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "http-client-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "http-tracer")
  let meter = TelemetryProvider::get_meter(provider, "http-meter")
  
  // Initialize HTTP client
  let http_config = HttpClientConfig::new()
  HttpClientConfig::set(http_config, "base_url", "https://api.example.com")
  HttpClientConfig::set(http_config, "timeout", "5000")
  
  let http_client = HttpClient::new(http_config)
  
  // Create HTTP metrics
  let request_counter = Meter::create_counter(meter, "http.request.count", "HTTP request count")
  let request_duration = Meter::create_histogram(meter, "http.request.duration", "HTTP request duration", "ms")
  
  // Test HTTP request with telemetry
  let http_span = Tracer::start_span(tracer, "http.request")
  Span::set_attribute(http_span, "http.method", "GET")
  Span::set_attribute(http_span, "http.url", "https://api.example.com/users/123")
  Span::set_attribute(http_span, "http.scheme", "https")
  Span::set_attribute(http_span, "http.target", "/users/123")
  
  // Record request start
  Span::add_event(http_span, "request.start", Some([
    ("method", "GET"),
    ("url", "https://api.example.com/users/123")
  ]))
  
  let start_time = Time::now()
  
  // Make HTTP request
  let request = HttpRequest::new("GET", "https://api.example.com/users/123", [
    ("Authorization", "Bearer token123"),
    ("Content-Type", "application/json")
  ], None)
  
  let response = HttpClient::send_request(http_client, request)
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Process response
  match response {
    Some(resp) => {
      let status_code = HttpResponse::status_code(resp)
      
      Span::set_attribute(http_span, "http.status_code", status_code.to_string())
      
      if status_code >= 200 && status_code < 300 {
        Span::set_status(http_span, Ok, None)
      } else {
        Span::set_status(http_span, Error, Some("HTTP error"))
      }
      
      // Record response completion
      Span::add_event(http_span, "response.complete", Some([
        ("status_code", status_code.to_string()),
        ("duration", duration.to_string() + "ms")
      ]))
      
      // Update metrics
      Counter::add(request_counter, 1.0, Some([
        ("http.method", "GET"),
        ("http.status_code", status_code.to_string()),
        ("http.scheme", "https")
      ]))
      
      Histogram::record(request_duration, duration, Some([
        ("http.method", "GET"),
        ("http.scheme", "https")
      ]))
      
      // Process response body
      let response_body = HttpResponse::body(resp)
      match response_body {
        Some(body) => {
          Span::set_attribute(http_span, "http.response_content_length", body.length().to_string())
          
          // Create span for response processing
          let processing_span = Tracer::start_span(tracer, "process.response", Some(http_span))
          
          // Simulate response processing
          Thread::sleep(50) // 50ms
          
          Span::end(processing_span)
        }
        None => assert_true(false)
      }
    }
    None => {
      Span::set_status(http_span, Error, Some("Request failed"))
      Counter::add(request_counter, 1.0, Some([
        ("http.method", "GET"),
        ("http.status_code", "error"),
        ("http.scheme", "https")
      ]))
    }
  }
  
  // End span
  Span::end(http_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 1)
  assert_true(metrics.length() >= 2)
  
  // Shutdown
  HttpClient::close(http_client)
  TelemetryProvider::shutdown(provider)
}

// Test 5: Message Queue Integration with Telemetry
test "message queue integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "message-queue-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "queue-tracer")
  let meter = TelemetryProvider::get_meter(provider, "queue-meter")
  
  // Initialize message queue
  let queue_config = MessageQueueConfig::new()
  MessageQueueConfig::set(queue_config, "broker_url", "amqp://localhost:5672")
  MessageQueueConfig::set(queue_config, "queue_name", "telemetry-queue")
  
  let message_queue = MessageQueue::new(queue_config)
  
  // Create queue metrics
  let publish_counter = Meter::create_counter(meter, "message.publish.count", "Message publish count")
  let consume_counter = Meter::create_counter(meter, "message.consume.count", "Message consume count")
  let message_size = Meter::create_histogram(meter, "message.size", "Message size", "bytes")
  
  // Test message publishing with telemetry
  let publish_span = Tracer::start_span(tracer, "message.publish")
  Span::set_attribute(publish_span, "messaging.system", "rabbitmq")
  Span::set_attribute(publish_span, "messaging.destination", "telemetry-queue")
  Span::set_attribute(publish_span, "messaging.destination_kind", "queue")
  
  // Create message
  let message_payload = "{\"user_id\": \"123\", \"action\": \"login\", \"timestamp\": 1234567890}"
  let message = Message::new("telemetry-queue", message_payload, [
    ("content-type", "application/json"),
    ("message-id", "msg-123")
  ])
  
  // Record message size
  Histogram::record(message_size, message_payload.length() as Float, Some([
    ("messaging.destination", "telemetry-queue")
  ]))
  
  // Publish message
  let publish_result = MessageQueue::publish(message_queue, message)
  
  match publish_result {
    Success(_) => {
      Span::set_status(publish_span, Ok, None)
      Counter::add(publish_counter, 1.0, Some([
        ("messaging.system", "rabbitmq"),
        ("messaging.destination", "telemetry-queue"),
        ("messaging.operation", "publish")
      ]))
    }
    Error(_) => {
      Span::set_status(publish_span, Error, Some("Publish failed"))
      Counter::add(publish_counter, 1.0, Some([
        ("messaging.system", "rabbitmq"),
        ("messaging.destination", "telemetry-queue"),
        ("messaging.operation", "publish"),
        ("status", "error")
      ]))
    }
  }
  
  // End publish span
  Span::end(publish_span)
  
  // Test message consumption with telemetry
  let consume_span = Tracer::start_span(tracer, "message.consume")
  Span::set_attribute(consume_span, "messaging.system", "rabbitmq")
  Span::set_attribute(consume_span, "messaging.destination", "telemetry-queue")
  Span::set_attribute(consume_span, "messaging.destination_kind", "queue")
  Span::set_attribute(consume_span, "messaging.operation", "receive")
  
  // Consume message
  let consume_result = MessageQueue::consume(message_queue)
  
  match consume_result {
    Success(received_message) => {
      Span::set_attribute(consume_span, "messaging.message_id", Message::get_attribute(received_message, "message-id"))
      
      // Create span for message processing
      let process_span = Tracer::start_span(tracer, "message.process", Some(consume_span))
      Span::set_attribute(process_span, "messaging.operation", "process")
      
      // Simulate message processing
      Thread::sleep(100) // 100ms
      
      // Process message payload
      let payload = Message::payload(received_message)
      Span::set_attribute(process_span, "messaging.message_payload_size", payload.length().to_string())
      
      Span::end(process_span)
      
      Counter::add(consume_counter, 1.0, Some([
        ("messaging.system", "rabbitmq"),
        ("messaging.destination", "telemetry-queue"),
        ("messaging.operation", "consume")
      ]))
    }
    Error(_) => {
      Span::set_status(consume_span, Error, Some("Consume failed"))
      Counter::add(consume_counter, 1.0, Some([
        ("messaging.system", "rabbitmq"),
        ("messaging.destination", "telemetry-queue"),
        ("messaging.operation", "consume"),
        ("status", "error")
      ]))
    }
  }
  
  // End consume span
  Span::end(consume_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 2) // Publish and consume spans
  assert_true(metrics.length() >= 3) // Publish counter, consume counter, and message size histogram
  
  // Shutdown
  MessageQueue::close(message_queue)
  TelemetryProvider::shutdown(provider)
}

// Test 6: Cache Integration with Telemetry
test "cache integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "cache-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "cache-tracer")
  let meter = TelemetryProvider::get_meter(provider, "cache-meter")
  
  // Initialize cache
  let cache_config = CacheConfig::new()
  CacheConfig::set(cache_config, "backend", "redis")
  CacheConfig::set(cache_config, "host", "localhost")
  CacheConfig::set(cache_config, "port", "6379")
  
  let cache = Cache::new(cache_config)
  
  // Create cache metrics
  let hit_counter = Meter::create_counter(meter, "cache.hit.count", "Cache hit count")
  let miss_counter = Meter::create_counter(meter, "cache.miss.count", "Cache miss count")
  let operation_duration = Meter::create_histogram(meter, "cache.operation.duration", "Cache operation duration", "ms")
  
  // Test cache operations with telemetry
  let key = "user:123"
  let value = "{\"id\": 123, \"name\": \"John Doe\", \"email\": \"john@example.com\"}"
  
  // Test cache miss
  let get_span = Tracer::start_span(tracer, "cache.get")
  Span::set_attribute(get_span, "cache.system", "redis")
  Span::set_attribute(get_span, "cache.operation", "get")
  Span::set_attribute(get_span, "cache.key", key)
  
  let start_time = Time::now()
  
  let get_result = Cache::get(cache, key)
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  match get_result {
    Some(_) => {
      Span::set_attribute(get_span, "cache.hit", "true")
      Counter::add(hit_counter, 1.0, Some([
        ("cache.system", "redis"),
        ("cache.operation", "get")
      ]))
    }
    None => {
      Span::set_attribute(get_span, "cache.hit", "false")
      Counter::add(miss_counter, 1.0, Some([
        ("cache.system", "redis"),
        ("cache.operation", "get")
      ]))
    }
  }
  
  Histogram::record(operation_duration, duration, Some([
    ("cache.system", "redis"),
    ("cache.operation", "get")
  ]))
  
  Span::end(get_span)
  
  // Test cache set
  let set_span = Tracer::start_span(tracer, "cache.set")
  Span::set_attribute(set_span, "cache.system", "redis")
  Span::set_attribute(set_span, "cache.operation", "set")
  Span::set_attribute(set_span, "cache.key", key)
  
  let set_start_time = Time::now()
  
  let set_result = Cache::set(cache, key, value, 3600) // 1 hour TTL
  
  let set_end_time = Time::now()
  let set_duration = set_end_time - set_start_time
  
  match set_result {
    Success(_) => {
      Span::set_status(set_span, Ok, None)
    }
    Error(_) => {
      Span::set_status(set_span, Error, Some("Set failed"))
    }
  }
  
  Histogram::record(operation_duration, set_duration, Some([
    ("cache.system", "redis"),
    ("cache.operation", "set")
  ]))
  
  Span::end(set_span)
  
  // Test cache hit
  let get_span_2 = Tracer::start_span(tracer, "cache.get")
  Span::set_attribute(get_span_2, "cache.system", "redis")
  Span::set_attribute(get_span_2, "cache.operation", "get")
  Span::set_attribute(get_span_2, "cache.key", key)
  
  let get_start_time_2 = Time::now()
  
  let get_result_2 = Cache::get(cache, key)
  
  let get_end_time_2 = Time::now()
  let get_duration_2 = get_end_time_2 - get_start_time_2
  
  match get_result_2 {
    Some(cached_value) => {
      assert_eq(cached_value, value)
      Span::set_attribute(get_span_2, "cache.hit", "true")
      Counter::add(hit_counter, 1.0, Some([
        ("cache.system", "redis"),
        ("cache.operation", "get")
      ]))
    }
    None => {
      Span::set_attribute(get_span_2, "cache.hit", "false")
      Counter::add(miss_counter, 1.0, Some([
        ("cache.system", "redis"),
        ("cache.operation", "get")
      ]))
    }
  }
  
  Histogram::record(operation_duration, get_duration_2, Some([
    ("cache.system", "redis"),
    ("cache.operation", "get")
  ]))
  
  Span::end(get_span_2)
  
  // Test cache delete
  let delete_span = Tracer::start_span(tracer, "cache.delete")
  Span::set_attribute(delete_span, "cache.system", "redis")
  Span::set_attribute(delete_span, "cache.operation", "delete")
  Span::set_attribute(delete_span, "cache.key", key)
  
  let delete_start_time = Time::now()
  
  let delete_result = Cache::delete(cache, key)
  
  let delete_end_time = Time::now()
  let delete_duration = delete_end_time - delete_start_time
  
  match delete_result {
    Success(_) => {
      Span::set_status(delete_span, Ok, None)
    }
    Error(_) => {
      Span::set_status(delete_span, Error, Some("Delete failed"))
    }
  }
  
  Histogram::record(operation_duration, delete_duration, Some([
    ("cache.system", "redis"),
    ("cache.operation", "delete")
  ]))
  
  Span::end(delete_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 4) // Get, set, get, delete spans
  assert_true(metrics.length() >= 3) // Hit counter, miss counter, and operation duration histogram
  
  // Shutdown
  Cache::close(cache)
  TelemetryProvider::shutdown(provider)
}

// Test 7: File System Integration with Telemetry
test "file system integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "file-system-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "file-tracer")
  let meter = TelemetryProvider::get_meter(provider, "file-meter")
  
  // Create file metrics
  let operation_counter = Meter::create_counter(meter, "file.operation.count", "File operation count")
  let operation_duration = Meter::create_histogram(meter, "file.operation.duration", "File operation duration", "ms")
  let file_size = Meter::create_histogram(meter, "file.size", "File size", "bytes")
  
  // Test file operations with telemetry
  let file_path = "/tmp/telemetry-test-file.txt"
  let file_content = "This is a test file for telemetry integration.\n".repeat(100)
  
  // Test file write
  let write_span = Tracer::start_span(tracer, "file.write")
  Span::set_attribute(write_span, "file.path", file_path)
  Span::set_attribute(write_span, "file.operation", "write")
  
  let write_start_time = Time::now()
  
  let write_result = FileSystem::write_file(file_path, file_content)
  
  let write_end_time = Time::now()
  let write_duration = write_end_time - write_start_time
  
  match write_result {
    Success(_) => {
      Span::set_status(write_span, Ok, None)
      Counter::add(operation_counter, 1.0, Some([
        ("file.operation", "write"),
        ("status", "success")
      ]))
      
      Histogram::record(operation_duration, write_duration, Some([
        ("file.operation", "write")
      ]))
      
      Histogram::record(file_size, file_content.length() as Float, Some([
        ("file.operation", "write")
      ]))
    }
    Error(_) => {
      Span::set_status(write_span, Error, Some("Write failed"))
      Counter::add(operation_counter, 1.0, Some([
        ("file.operation", "write"),
        ("status", "error")
      ]))
    }
  }
  
  Span::end(write_span)
  
  // Test file read
  let read_span = Tracer::start_span(tracer, "file.read")
  Span::set_attribute(read_span, "file.path", file_path)
  Span::set_attribute(read_span, "file.operation", "read")
  
  let read_start_time = Time::now()
  
  let read_result = FileSystem::read_file(file_path)
  
  let read_end_time = Time::now()
  let read_duration = read_end_time - read_start_time
  
  match read_result {
    Some(content) => {
      assert_eq(content, file_content)
      Span::set_status(read_span, Ok, None)
      Counter::add(operation_counter, 1.0, Some([
        ("file.operation", "read"),
        ("status", "success")
      ]))
      
      Histogram::record(operation_duration, read_duration, Some([
        ("file.operation", "read")
      ]))
      
      Histogram::record(file_size, content.length() as Float, Some([
        ("file.operation", "read")
      ]))
    }
    None => {
      Span::set_status(read_span, Error, Some("Read failed"))
      Counter::add(operation_counter, 1.0, Some([
        ("file.operation", "read"),
        ("status", "error")
      ]))
    }
  }
  
  Span::end(read_span)
  
  // Test file delete
  let delete_span = Tracer::start_span(tracer, "file.delete")
  Span::set_attribute(delete_span, "file.path", file_path)
  Span::set_attribute(delete_span, "file.operation", "delete")
  
  let delete_start_time = Time::now()
  
  let delete_result = FileSystem::delete_file(file_path)
  
  let delete_end_time = Time::now()
  let delete_duration = delete_end_time - delete_start_time
  
  match delete_result {
    Success(_) => {
      Span::set_status(delete_span, Ok, None)
      Counter::add(operation_counter, 1.0, Some([
        ("file.operation", "delete"),
        ("status", "success")
      ]))
      
      Histogram::record(operation_duration, delete_duration, Some([
        ("file.operation", "delete")
      ]))
    }
    Error(_) => {
      Span::set_status(delete_span, Error, Some("Delete failed"))
      Counter::add(operation_counter, 1.0, Some([
        ("file.operation", "delete"),
        ("status", "error")
      ]))
    }
  }
  
  Span::end(delete_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 3) // Write, read, delete spans
  assert_true(metrics.length() >= 3) // Operation counter, duration histogram, and size histogram
  
  // Shutdown
  TelemetryProvider::shutdown(provider)
}

// Test 8: Authentication and Authorization Integration with Telemetry
test "authentication and authorization integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "auth-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "auth-tracer")
  let meter = TelemetryProvider::get_meter(provider, "auth-meter")
  
  // Initialize auth service
  let auth_config = AuthServiceConfig::new()
  AuthServiceConfig::set(auth_config, "jwt_secret", "secret-key")
  AuthServiceConfig::set(auth_config, "token_expiry", "3600")
  
  let auth_service = AuthService::new(auth_config)
  
  // Create auth metrics
  let auth_counter = Meter::create_counter(meter, "auth.operation.count", "Authentication operation count")
  let auth_duration = Meter::create_histogram(meter, "auth.operation.duration", "Authentication operation duration", "ms")
  
  // Test authentication with telemetry
  let auth_span = Tracer::start_span(tracer, "auth.authenticate")
  Span::set_attribute(auth_span, "auth.operation", "authenticate")
  Span::set_attribute(auth_span, "auth.user", "user-123")
  
  let auth_start_time = Time::now()
  
  let auth_result = AuthService::authenticate(auth_service, "user-123", "password123")
  
  let auth_end_time = Time::now()
  let auth_duration_time = auth_end_time - auth_start_time
  
  match auth_result {
    Success(token) => {
      Span::set_status(auth_span, Ok, None)
      Span::set_attribute(auth_span, "auth.success", "true")
      Counter::add(auth_counter, 1.0, Some([
        ("auth.operation", "authenticate"),
        ("auth.status", "success")
      ]))
      
      Histogram::record(auth_duration, auth_duration_time, Some([
        ("auth.operation", "authenticate")
      ]))
      
      // Test authorization with telemetry
      let authz_span = Tracer::start_span(tracer, "auth.authorize")
      Span::set_attribute(authz_span, "auth.operation", "authorize")
      Span::set_attribute(authz_span, "auth.user", "user-123")
      Span::set_attribute(authz_span, "auth.resource", "users")
      Span::set_attribute(authz_span, "auth.action", "read")
      
      let authz_start_time = Time::now()
      
      let authz_result = AuthService::authorize(auth_service, token, "users", "read")
      
      let authz_end_time = Time::now()
      let authz_duration_time = authz_end_time - authz_start_time
      
      match authz_result {
        Success(_) => {
          Span::set_status(authz_span, Ok, None)
          Span::set_attribute(authz_span, "auth.success", "true")
          Counter::add(auth_counter, 1.0, Some([
            ("auth.operation", "authorize"),
            ("auth.status", "success")
          ]))
          
          Histogram::record(auth_duration, authz_duration_time, Some([
            ("auth.operation", "authorize")
          ]))
        }
        Error(_) => {
          Span::set_status(authz_span, Error, Some("Authorization failed"))
          Span::set_attribute(authz_span, "auth.success", "false")
          Counter::add(auth_counter, 1.0, Some([
            ("auth.operation", "authorize"),
            ("auth.status", "error")
          ]))
        }
      }
      
      Span::end(authz_span)
    }
    Error(_) => {
      Span::set_status(auth_span, Error, Some("Authentication failed"))
      Span::set_attribute(auth_span, "auth.success", "false")
      Counter::add(auth_counter, 1.0, Some([
        ("auth.operation", "authenticate"),
        ("auth.status", "error")
      ]))
    }
  }
  
  Span::end(auth_span)
  
  // Test failed authentication
  let failed_auth_span = Tracer::start_span(tracer, "auth.authenticate")
  Span::set_attribute(failed_auth_span, "auth.operation", "authenticate")
  Span::set_attribute(failed_auth_span, "auth.user", "user-456")
  
  let failed_auth_start_time = Time::now()
  
  let failed_auth_result = AuthService::authenticate(auth_service, "user-456", "wrong-password")
  
  let failed_auth_end_time = Time::now()
  let failed_auth_duration_time = failed_auth_end_time - failed_auth_start_time
  
  match failed_auth_result {
    Success(_) => assert_true(false) // Should not succeed
    Error(_) => {
      Span::set_status(failed_auth_span, Error, Some("Authentication failed"))
      Span::set_attribute(failed_auth_span, "auth.success", "false")
      Counter::add(auth_counter, 1.0, Some([
        ("auth.operation", "authenticate"),
        ("auth.status", "error")
      ]))
      
      Histogram::record(auth_duration, failed_auth_duration_time, Some([
        ("auth.operation", "authenticate")
      ]))
    }
  }
  
  Span::end(failed_auth_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 3) // Auth, authz, failed auth spans
  assert_true(metrics.length() >= 2) // Auth counter and duration histogram
  
  // Shutdown
  AuthService::close(auth_service)
  TelemetryProvider::shutdown(provider)
}

// Test 9: Configuration Management Integration with Telemetry
test "configuration management integration with telemetry" {
  // Initialize telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "config-service")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "config-tracer")
  let meter = TelemetryProvider::get_meter(provider, "config-meter")
  
  // Initialize configuration manager
  let config_manager_config = ConfigManagerConfig::new()
  ConfigManagerConfig::set(config_manager_config, "source", "file")
  ConfigManagerConfig::set(config_manager_config, "path", "/etc/telemetry/config.json")
  
  let config_manager = ConfigManager::new(config_manager_config)
  
  // Create config metrics
  let reload_counter = Meter::create_counter(meter, "config.reload.count", "Configuration reload count")
  let reload_duration = Meter::create_histogram(meter, "config.reload.duration", "Configuration reload duration", "ms")
  
  // Test configuration loading with telemetry
  let load_span = Tracer::start_span(tracer, "config.load")
  Span::set_attribute(load_span, "config.operation", "load")
  Span::set_attribute(load_span, "config.source", "file")
  
  let load_start_time = Time::now()
  
  let load_result = ConfigManager::load(config_manager)
  
  let load_end_time = Time::now()
  let load_duration = load_end_time - load_start_time
  
  match load_result {
    Success(config_data) => {
      Span::set_status(load_span, Ok, None)
      Span::set_attribute(load_span, "config.keys", config_data.length().to_string())
      Counter::add(reload_counter, 1.0, Some([
        ("config.operation", "load"),
        ("config.status", "success")
      ]))
      
      Histogram::record(reload_duration, load_duration, Some([
        ("config.operation", "load")
      ]))
    }
    Error(_) => {
      Span::set_status(load_span, Error, Some("Load failed"))
      Counter::add(reload_counter, 1.0, Some([
        ("config.operation", "load"),
        ("config.status", "error")
      ]))
    }
  }
  
  Span::end(load_span)
  
  // Test configuration reload with telemetry
  let reload_span = Tracer::start_span(tracer, "config.reload")
  Span::set_attribute(reload_span, "config.operation", "reload")
  Span::set_attribute(reload_span, "config.source", "file")
  
  let reload_start_time = Time::now()
  
  let reload_result = ConfigManager::reload(config_manager)
  
  let reload_end_time = Time::now()
  let reload_duration_time = reload_end_time - reload_start_time
  
  match reload_result {
    Success(config_data) => {
      Span::set_status(reload_span, Ok, None)
      Span::set_attribute(reload_span, "config.keys", config_data.length().to_string())
      Counter::add(reload_counter, 1.0, Some([
        ("config.operation", "reload"),
        ("config.status", "success")
      ]))
      
      Histogram::record(reload_duration, reload_duration_time, Some([
        ("config.operation", "reload")
      ]))
    }
    Error(_) => {
      Span::set_status(reload_span, Error, Some("Reload failed"))
      Counter::add(reload_counter, 1.0, Some([
        ("config.operation", "reload"),
        ("config.status", "error")
      ]))
    }
  }
  
  Span::end(reload_span)
  
  // Test configuration watch with telemetry
  let watch_span = Tracer::start_span(tracer, "config.watch")
  Span::set_attribute(watch_span, "config.operation", "watch")
  Span::set_attribute(watch_span, "config.source", "file")
  
  // Start watching for changes
  let watch_result = ConfigManager::watch(config_manager, fn(config_data) {
    // Create span for config change event
    let change_span = Tracer::start_span(tracer, "config.change")
    Span::set_attribute(change_span, "config.operation", "change")
    Span::set_attribute(change_span, "config.keys", config_data.length().to_string())
    
    // Simulate processing config change
    Thread::sleep(50) // 50ms
    
    Span::end(change_span)
  })
  
  match watch_result {
    Success(_) => {
      Span::set_status(watch_span, Ok, None)
    }
    Error(_) => {
      Span::set_status(watch_span, Error, Some("Watch failed"))
    }
  }
  
  Span::end(watch_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(1000) // 1 second
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  
  assert_true(spans.length() >= 3) // Load, reload, watch spans
  assert_true(metrics.length() >= 2) // Reload counter and duration histogram
  
  // Shutdown
  ConfigManager::close(config_manager)
  TelemetryProvider::shutdown(provider)
}

// Test 10: Full System Integration Test
test "full system integration test" {
  // Initialize all system components with telemetry
  let config = TelemetryConfig::new()
  TelemetryConfig::set(config, "service.name", "full-system-test")
  TelemetryConfig::set(config, "service.version", "1.0.0")
  TelemetryConfig::set(config, "environment", "test")
  
  let provider = TelemetryProvider::new(config)
  let tracer = TelemetryProvider::get_tracer(provider, "system-tracer")
  let meter = TelemetryProvider::get_meter(provider, "system-meter")
  let logger = TelemetryProvider::get_logger(provider, "system-logger")
  
  // Create system metrics
  let operation_counter = Meter::create_counter(meter, "system.operation.count", "System operation count")
  let operation_duration = Meter::create_histogram(meter, "system.operation.duration", "System operation duration", "ms")
  
  // Initialize subsystems
  let db_config = DatabaseConfig::new()
  DatabaseConfig::set(db_config, "connection_string", "postgresql://localhost:5432/telemetry")
  let db_client = DatabaseClient::new(db_config)
  
  let cache_config = CacheConfig::new()
  CacheConfig::set(cache_config, "backend", "redis")
  CacheConfig::set(cache_config, "host", "localhost")
  let cache = Cache::new(cache_config)
  
  let http_config = HttpClientConfig::new()
  HttpClientConfig::set(http_config, "base_url", "https://api.example.com")
  let http_client = HttpClient::new(http_config)
  
  let auth_config = AuthServiceConfig::new()
  AuthServiceConfig::set(auth_config, "jwt_secret", "secret-key")
  let auth_service = AuthService::new(auth_config)
  
  // Simulate a complete user request flow
  let request_span = Tracer::start_span(tracer, "user.request")
  Span::set_attribute(request_span, "user.id", "user-123")
  Span::set_attribute(request_span, "request.path", "/api/users/123")
  Span::set_attribute(request_span, "request.method", "GET")
  
  let request_start_time = Time::now()
  
  // Step 1: Authenticate user
  let auth_span = Tracer::start_span(tracer, "authenticate.user", Some(request_span))
  let auth_start_time = Time::now()
  
  let auth_result = AuthService::authenticate(auth_service, "user-123", "password123")
  
  let auth_end_time = Time::now()
  let auth_duration = auth_end_time - auth_start_time
  
  match auth_result {
    Success(token) => {
      Span::set_status(auth_span, Ok, None)
      Counter::add(operation_counter, 1.0, Some([
        ("system.operation", "authenticate"),
        ("status", "success")
      ]))
      
      Histogram::record(operation_duration, auth_duration, Some([
        ("system.operation", "authenticate")
      ]))
      
      // Step 2: Check cache for user data
      let cache_span = Tracer::start_span(tracer, "cache.check", Some(request_span))
      let cache_start_time = Time::now()
      
      let cache_result = Cache::get(cache, "user:123")
      
      let cache_end_time = Time::now()
      let cache_duration = cache_end_time - cache_start_time
      
      let user_data = match cache_result {
        Some(data) => {
          Span::set_status(cache_span, Ok, None)
          Span::set_attribute(cache_span, "cache.hit", "true")
          Counter::add(operation_counter, 1.0, Some([
            ("system.operation", "cache.check"),
            ("cache.hit", "true")
          ]))
          
          Histogram::record(operation_duration, cache_duration, Some([
            ("system.operation", "cache.check")
          ]))
          
          data
        }
        None => {
          Span::set_attribute(cache_span, "cache.hit", "false")
          Counter::add(operation_counter, 1.0, Some([
            ("system.operation", "cache.check"),
            ("cache.hit", "false")
          ]))
          
          Histogram::record(operation_duration, cache_duration, Some([
            ("system.operation", "cache.check")
          ]))
          
          // Step 3: Fetch from database
          let db_span = Tracer::start_span(tracer, "database.query", Some(request_span))
          let db_start_time = Time::now()
          
          let connection = DatabaseClient::get_connection(db_client).unwrap()
          let db_result = DatabaseClient::execute_query(db_client, connection, "SELECT * FROM users WHERE id = $1", [123])
          
          let db_end_time = Time::now()
          let db_duration = db_end_time - db_start_time
          
          match db_result {
            Some(rows) => {
              Span::set_status(db_span, Ok, None)
              Span::set_attribute(db_span, "db.rows", rows.length().to_string())
              Counter::add(operation_counter, 1.0, Some([
                ("system.operation", "database.query"),
                ("status", "success")
              ]))
              
              Histogram::record(operation_duration, db_duration, Some([
                ("system.operation", "database.query")
              ]))
              
              // Convert to JSON
              let user_json = "{\"id\": 123, \"name\": \"John Doe\", \"email\": \"john@example.com\"}"
              
              // Step 4: Cache the result
              let cache_set_span = Tracer::start_span(tracer, "cache.set", Some(request_span))
              let cache_set_start_time = Time::now()
              
              let cache_set_result = Cache::set(cache, "user:123", user_json, 3600)
              
              let cache_set_end_time = Time::now()
              let cache_set_duration = cache_set_end_time - cache_set_start_time
              
              match cache_set_result {
                Success(_) => {
                  Span::set_status(cache_set_span, Ok, None)
                  Counter::add(operation_counter, 1.0, Some([
                    ("system.operation", "cache.set"),
                    ("status", "success")
                  ]))
                  
                  Histogram::record(operation_duration, cache_set_duration, Some([
                    ("system.operation", "cache.set")
                  ]))
                }
                Error(_) => {
                  Span::set_status(cache_set_span, Error, Some("Cache set failed"))
                  Counter::add(operation_counter, 1.0, Some([
                    ("system.operation", "cache.set"),
                    ("status", "error")
                  ]))
                }
              }
              
              Span::end(cache_set_span)
              
              user_json
            }
            None => {
              Span::set_status(db_span, Error, Some("Database query failed"))
              Counter::add(operation_counter, 1.0, Some([
                ("system.operation", "database.query"),
                ("status", "error")
              ]))
              
              Histogram::record(operation_duration, db_duration, Some([
                ("system.operation", "database.query")
              ]))
              
              "{}" // Empty JSON
            }
          }
          
          DatabaseClient::release_connection(db_client, connection)
          Span::end(db_span)
        }
      }
      
      Span::end(cache_span)
      
      // Step 5: Call external API
      let api_span = Tracer::start_span(tracer, "api.call", Some(request_span))
      let api_start_time = Time::now()
      
      let api_request = HttpRequest::new("POST", "https://api.example.com/analytics", [
        ("Authorization", "Bearer " + token),
        ("Content-Type", "application/json")
      ], Some(user_data))
      
      let api_response = HttpClient::send_request(http_client, api_request)
      
      let api_end_time = Time::now()
      let api_duration = api_end_time - api_start_time
      
      match api_response {
        Some(resp) => {
          let status_code = HttpResponse::status_code(resp)
          Span::set_attribute(api_span, "http.status_code", status_code.to_string())
          
          if status_code >= 200 && status_code < 300 {
            Span::set_status(api_span, Ok, None)
            Counter::add(operation_counter, 1.0, Some([
              ("system.operation", "api.call"),
              ("status", "success")
            ]))
          } else {
            Span::set_status(api_span, Error, Some("API error"))
            Counter::add(operation_counter, 1.0, Some([
              ("system.operation", "api.call"),
              ("status", "error")
            ]))
          }
          
          Histogram::record(operation_duration, api_duration, Some([
            ("system.operation", "api.call")
          ]))
        }
        None => {
          Span::set_status(api_span, Error, Some("API call failed"))
          Counter::add(operation_counter, 1.0, Some([
            ("system.operation", "api.call"),
            ("status", "error")
          ]))
        }
      }
      
      Span::end(api_span)
      
      // Step 6: Log completion
      Logger::emit(logger, Info, "User request completed successfully", Some([
        ("user.id", "user-123"),
        ("request.path", "/api/users/123")
      ]))
    }
    Error(_) => {
      Span::set_status(auth_span, Error, Some("Authentication failed"))
      Counter::add(operation_counter, 1.0, Some([
        ("system.operation", "authenticate"),
        ("status", "error")
      ]))
      
      Logger::emit(logger, Error, "User request failed: Authentication error", Some([
        ("user.id", "user-123"),
        ("request.path", "/api/users/123")
      ]))
    }
  }
  
  let request_end_time = Time::now()
  let request_duration = request_end_time - request_start_time
  
  Span::set_attribute(request_span, "request.duration", request_duration.to_string() + "ms")
  Histogram::record(operation_duration, request_duration, Some([
    ("system.operation", "user.request")
  ]))
  
  Span::end(request_span)
  
  // Wait for telemetry data to be processed
  Thread::sleep(2000) // 2 seconds
  
  // Verify telemetry data
  let collector = TelemetryProvider::get_collector(provider)
  let spans = TelemetryCollector::get_spans(collector)
  let metrics = TelemetryCollector::get_metrics(collector)
  let logs = TelemetryCollector::get_logs(collector)
  
  assert_true(spans.length() >= 6) // Request, auth, cache, db, cache set, API spans
  assert_true(metrics.length() >= 2) // Operation counter and duration histogram
  assert_true(logs.length() >= 1) // Log record
  
  // Verify trace continuity
  let trace_id = SpanData::trace_id(spans[0])
  for span in spans {
    assert_eq(SpanData::trace_id(span), trace_id)
  }
  
  // Shutdown all subsystems
  DatabaseClient::close(db_client)
  Cache::close(cache)
  HttpClient::close(http_client)
  AuthService::close(auth_service)
  TelemetryProvider::shutdown(provider)
}