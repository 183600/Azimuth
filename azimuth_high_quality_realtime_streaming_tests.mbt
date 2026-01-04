// Azimuth Telemetry System - High Quality Real-time Streaming Tests
// This file contains comprehensive test cases for real-time streaming functionality

// Test 1: Basic Stream Creation and Management
test "basic stream creation and management" {
  // Create a real-time telemetry stream
  let stream = Stream::new("telemetry_stream", StreamType::Telemetry)
  
  // Verify stream properties
  assert_eq(Stream::name(stream), "telemetry_stream")
  assert_eq(Stream::type(stream), StreamType::Telemetry)
  assert_true(Stream::is_active(stream))
  assert_eq(Stream::subscriber_count(stream), 0)
  
  // Test stream lifecycle
  Stream::pause(stream)
  assert_false(Stream::is_active(stream))
  
  Stream::resume(stream)
  assert_true(Stream::is_active(stream))
  
  // Test stream metadata
  Stream::set_metadata(stream, "description", "Test telemetry stream")
  Stream::set_metadata(stream, "version", "1.0.0")
  
  let description = Stream::get_metadata(stream, "description")
  match description {
    Some(StringValue(v)) => assert_eq(v, "Test telemetry stream")
    _ => assert_true(false)
  }
  
  let version = Stream::get_metadata(stream, "version")
  match version {
    Some(StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
  
  // Clean up
  Stream::close(stream)
  assert_false(Stream::is_active(stream))
}

// Test 2: Stream Subscription and Publishing
test "stream subscription and publishing" {
  let stream = Stream::new("pub_sub_stream", StreamType::Telemetry)
  let mut received_messages = []
  
  // Create subscriber
  let subscriber = Stream::subscribe(stream)
  assert_eq(Stream::subscriber_count(stream), 1)
  
  // Set up message handler
  Subscriber::set_handler(subscriber, fn(message) {
    received_messages = received_messages.push(message)
  })
  
  // Publish messages
  let message_count = 100
  for i in 0..message_count {
    let message = StreamMessage::new("test_message_" + i.to_string(), Some([
      ("message_id", IntValue(i)),
      ("timestamp", IntValue(Time::now()))
    ]))
    
    Stream::publish(stream, message)
  }
  
  // Wait for messages to be processed
  Stream::flush(stream)
  
  // Verify all messages were received
  assert_eq(received_messages.length(), message_count)
  
  for i in 0..message_count {
    let message = received_messages[i]
    assert_eq(StreamMessage::type(message), "test_message_" + i.to_string())
    
    let message_id_attr = StreamMessage::get_attribute(message, "message_id")
    match message_id_attr {
      Some(IntValue(v)) => assert_eq(v, i)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  assert_eq(Stream::subscriber_count(stream), 0)
  Stream::close(stream)
}

// Test 3: Real-time Metrics Streaming
test "real-time metrics streaming" {
  let metrics_stream = Stream::new("metrics_stream", StreamType::Metrics)
  let subscriber = Stream::subscribe(metrics_stream)
  let mut metric_events = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    metric_events = metric_events.push(message)
  })
  
  // Create metrics producer
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "streaming_meter")
  let counter = Meter::create_counter(meter, "stream_counter", None, None)
  let histogram = Meter::create_histogram(meter, "stream_histogram", None, None)
  
  // Enable streaming for metrics
  Meter::enable_streaming(meter, metrics_stream)
  
  // Generate metrics
  for i in 0..50 {
    Counter::add(counter, 1.0)
    Histogram::record(histogram, i.to_float() * 10.0)
    
    // Small delay to simulate real-time
    Time::sleep(10)
  }
  
  // Wait for streaming to complete
  Stream::flush(metrics_stream)
  
  // Verify metric events were streamed
  assert_true(metric_events.length() > 0)
  
  // Check counter events
  let counter_events = metric_events.filter(fn(msg) { StreamMessage::type(msg) == "counter" })
  assert_true(counter_events.length() > 0)
  
  // Check histogram events
  let histogram_events = metric_events.filter(fn(msg) { StreamMessage::type(msg) == "histogram" })
  assert_true(histogram_events.length() > 0)
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(metrics_stream)
}

// Test 4: Real-time Log Streaming
test "real-time log streaming" {
  let log_stream = Stream::new("log_stream", StreamType::Logs)
  let subscriber = Stream::subscribe(log_stream)
  let mut log_events = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    log_events = log_events.push(message)
  })
  
  // Create logger with streaming enabled
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "streaming_logger")
  Logger::enable_streaming(logger, log_stream)
  
  // Generate log events
  let log_levels = [Info, Warn, Error, Debug]
  let log_count = 40
  
  for i in 0..log_count {
    let level = log_levels[i % log_levels.length()]
    let message = "Streaming log message " + i.to_string()
    
    let log_record = LogRecord::new(level, Some(message))
    Logger::emit(logger, log_record)
    
    // Small delay to simulate real-time
    Time::sleep(10)
  }
  
  // Wait for streaming to complete
  Stream::flush(log_stream)
  
  // Verify log events were streamed
  assert_eq(log_events.length(), log_count)
  
  for i in 0..log_count {
    let event = log_events[i]
    assert_eq(StreamMessage::type(event), "log")
    
    let level = log_levels[i % log_levels.length()]
    let event_level = StreamMessage::get_attribute(event, "severity")
    match event_level {
      Some(IntValue(v)) => assert_eq(v, level.to_int())
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(log_stream)
}

// Test 5: Real-time Span Streaming
test "real-time span streaming" {
  let span_stream = Stream::new("span_stream", StreamType::Traces)
  let subscriber = Stream::subscribe(span_stream)
  let mut span_events = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    span_events = span_events.push(message)
  })
  
  // Enable span streaming
  Tracer::enable_streaming(span_stream)
  
  // Create spans
  let span_count = 30
  for i in 0..span_count {
    let trace_id = "stream_trace_" + (i % 5).to_string()
    let span_id = "stream_span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "streaming_test")
    let span = Span::new("streaming_operation", Internal, span_ctx)
    
    Span::set_attribute(span, "span_index", IntValue(i))
    Span::add_event(span, "span_event", Some([
      ("event_index", IntValue(i))
    ]))
    
    Span::end(span)
    
    // Small delay to simulate real-time
    Time::sleep(10)
  }
  
  // Wait for streaming to complete
  Stream::flush(span_stream)
  
  // Verify span events were streamed
  assert_eq(span_events.length(), span_count)
  
  for i in 0..span_count {
    let event = span_events[i]
    assert_eq(StreamMessage::type(event), "span")
    
    let span_index_attr = StreamMessage::get_attribute(event, "span_index")
    match span_index_attr {
      Some(IntValue(v)) => assert_eq(v, i)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(span_stream)
}

// Test 6: Stream Filtering and Transformation
test "stream filtering and transformation" {
  let source_stream = Stream::new("source_stream", StreamType::Telemetry)
  let filtered_stream = Stream::new("filtered_stream", StreamType::Telemetry)
  let transformed_stream = Stream::new("transformed_stream", StreamType::Telemetry)
  
  // Set up filter: only pass messages with even IDs
  Stream::set_filter(source_stream, filtered_stream, fn(message) {
    match StreamMessage::get_attribute(message, "message_id") {
      Some(IntValue(id)) => id % 2 == 0
      _ => false
    }
  })
  
  // Set up transformer: add timestamp prefix to message type
  Stream::set_transformer(filtered_stream, transformed_stream, fn(message) {
    let timestamp = Time::now()
    let new_type = timestamp.to_string() + "_" + StreamMessage::type(message)
    StreamMessage::with_type(message, new_type)
  })
  
  // Subscribe to transformed stream
  let subscriber = Stream::subscribe(transformed_stream)
  let mut transformed_messages = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    transformed_messages = transformed_messages.push(message)
  })
  
  // Publish messages to source stream
  let message_count = 20
  for i in 0..message_count {
    let message = StreamMessage::new("test_message", Some([
      ("message_id", IntValue(i))
    ]))
    
    Stream::publish(source_stream, message)
  }
  
  // Wait for processing
  Stream::flush(source_stream)
  Stream::flush(filtered_stream)
  Stream::flush(transformed_stream)
  
  // Verify filtering and transformation
  // Should only have even IDs (0, 2, 4, ..., 18)
  assert_eq(transformed_messages.length(), message_count / 2)
  
  for i in 0..transformed_messages.length() {
    let message = transformed_messages[i]
    let message_type = StreamMessage::type(message)
    
    // Should have timestamp prefix
    assert_true(message_type.contains("_"))
    
    // Should have even ID
    let message_id_attr = StreamMessage::get_attribute(message, "message_id")
    match message_id_attr {
      Some(IntValue(id)) => assert_eq(id % 2, 0)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(source_stream)
  Stream::close(filtered_stream)
  Stream::close(transformed_stream)
}

// Test 7: Stream Aggregation and Windowing
test "stream aggregation and windowing" {
  let source_stream = Stream::new("source_stream", StreamType::Metrics)
  let aggregated_stream = Stream::new("aggregated_stream", StreamType::Metrics)
  
  // Set up time window aggregation (1 second windows)
  Stream::set_time_window_aggregator(
    source_stream,
    aggregated_stream,
    1000, // 1 second window
    fn(messages) {
      let count = messages.length()
      let sum = messages.fold(0, fn(acc, msg) {
        match StreamMessage::get_attribute(msg, "value") {
          Some(IntValue(v)) => acc + v
          _ => acc
        }
      })
      
      let avg = if count > 0 { sum / count } else { 0 }
      
      StreamMessage::new("aggregated_metrics", Some([
        ("count", IntValue(count)),
        ("sum", IntValue(sum)),
        ("average", IntValue(avg)),
        ("window_timestamp", IntValue(Time::now()))
      ]))
    }
  )
  
  // Subscribe to aggregated stream
  let subscriber = Stream::subscribe(aggregated_stream)
  let mut aggregated_messages = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    aggregated_messages = aggregated_messages.push(message)
  })
  
  // Publish messages over time
  for i in 0..50 {
    let message = StreamMessage::new("metric", Some([
      ("value", IntValue(i))
    ]))
    
    Stream::publish(source_stream, message)
    
    // Small delay to create time windows
    Time::sleep(50)
  }
  
  // Wait for aggregation
  Time::sleep(1100) // Wait for final window
  Stream::flush(aggregated_stream)
  
  // Verify aggregation
  assert_true(aggregated_messages.length() > 0)
  
  for message in aggregated_messages {
    assert_eq(StreamMessage::type(message), "aggregated_metrics")
    
    let count_attr = StreamMessage::get_attribute(message, "count")
    match count_attr {
      Some(IntValue(count)) => assert_true(count > 0)
      _ => assert_true(false)
    }
    
    let sum_attr = StreamMessage::get_attribute(message, "sum")
    match sum_attr {
      Some(IntValue(sum)) => assert_true(sum >= 0)
      _ => assert_true(false)
    }
    
    let avg_attr = StreamMessage::get_attribute(message, "average")
    match avg_attr {
      Some(IntValue(avg)) => assert_true(avg >= 0)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(source_stream)
  Stream::close(aggregated_stream)
}

// Test 8: Stream Backpressure Handling
test "stream backpressure handling" {
  let stream = Stream::new("backpressure_stream", StreamType::Telemetry)
  
  // Set low buffer size to trigger backpressure
  Stream::set_buffer_size(stream, 10)
  
  let subscriber = Stream::subscribe(stream)
  let mut received_count = 0
  
  // Slow consumer
  Subscriber::set_handler(subscriber, fn(message) {
    Time::sleep(100) // Slow processing
    received_count = received_count + 1
  })
  
  // Fast producer
  let message_count = 50
  let mut published_count = 0
  
  for i in 0..message_count {
    let message = StreamMessage::new("backpressure_test", Some([
      ("message_id", IntValue(i))
    ]))
    
    let result = Stream::publish(stream, message)
    if result {
      published_count = published_count + 1
    } else {
      // Backpressure triggered
      assert_true(true)
    }
  }
  
  // Wait for processing
  Time::sleep(6000) // Wait for slow consumer
  Stream::flush(stream)
  
  // Verify backpressure handling
  // Not all messages should be published due to backpressure
  assert_true(published_count < message_count)
  assert_true(received_count <= published_count)
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(stream)
}

// Test 9: Stream Persistence and Recovery
test "stream persistence and recovery" {
  let stream = Stream::new("persistent_stream", StreamType::Telemetry)
  
  // Enable persistence
  Stream::enable_persistence(stream, "/tmp/test_stream_persistence")
  
  // Publish messages
  let message_count = 30
  for i in 0..message_count {
    let message = StreamMessage::new("persistent_test", Some([
      ("message_id", IntValue(i)),
      ("timestamp", IntValue(Time::now()))
    ]))
    
    Stream::publish(stream, message)
  }
  
  Stream::flush(stream)
  
  // Close stream
  Stream::close(stream)
  
  // Recover stream
  let recovered_stream = Stream::recover("persistent_stream", "/tmp/test_stream_persistence")
  assert_true(Stream::is_active(recovered_stream))
  
  // Verify recovered messages
  let subscriber = Stream::subscribe(recovered_stream)
  let mut recovered_messages = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    recovered_messages = recovered_messages.push(message)
  })
  
  // Wait for recovery
  Time::sleep(1000)
  Stream::flush(recovered_stream)
  
  // Verify all messages were recovered
  assert_eq(recovered_messages.length(), message_count)
  
  for i in 0..message_count {
    let message = recovered_messages[i]
    assert_eq(StreamMessage::type(message), "persistent_test")
    
    let message_id_attr = StreamMessage::get_attribute(message, "message_id")
    match message_id_attr {
      Some(IntValue(v)) => assert_eq(v, i)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(recovered_stream)
  
  // Clean up persistence files
  File::delete_recursive("/tmp/test_stream_persistence")
}

// Test 10: Multi-Stream Coordination
test "multi-stream coordination" {
  let trace_stream = Stream::new("trace_stream", StreamType::Traces)
  let metric_stream = Stream::new("metric_stream", StreamType::Metrics)
  let log_stream = Stream::new("log_stream", StreamType::Logs)
  let coordinated_stream = Stream::new("coordinated_stream", StreamType::Telemetry)
  
  // Set up stream coordination
  Stream::coordinate([trace_stream, metric_stream, log_stream], coordinated_stream, fn(messages) {
    let trace_count = messages.filter(fn(msg) { StreamMessage::type(msg) == "span" }).length()
    let metric_count = messages.filter(fn(msg) { StreamMessage::type(msg) == "metric" }).length()
    let log_count = messages.filter(fn(msg) { StreamMessage::type(msg) == "log" }).length()
    
    StreamMessage::new("coordinated_telemetry", Some([
      ("trace_count", IntValue(trace_count)),
      ("metric_count", IntValue(metric_count)),
      ("log_count", IntValue(log_count)),
      ("total_count", IntValue(messages.length())),
      ("coordination_timestamp", IntValue(Time::now()))
    ]))
  })
  
  // Subscribe to coordinated stream
  let subscriber = Stream::subscribe(coordinated_stream)
  let mut coordinated_messages = []
  
  Subscriber::set_handler(subscriber, fn(message) {
    coordinated_messages = coordinated_messages.push(message)
  })
  
  // Enable streaming for all telemetry types
  Tracer::enable_streaming(trace_stream)
  Meter::enable_streaming(metric_stream)
  Logger::enable_streaming(log_stream)
  
  // Generate mixed telemetry
  for i in 0..20 {
    // Create span
    let trace_id = "coord_trace_" + i.to_string()
    let span_id = "coord_span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "coordination_test")
    let span = Span::new("coordination_operation", Internal, span_ctx)
    Span::end(span)
    
    // Create metric
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "coordination_meter")
    let counter = Meter::create_counter(meter, "coord_counter", None, None)
    Counter::add(counter, 1.0)
    
    // Create log
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "coordination_logger")
    let log_record = LogRecord::new(Info, Some("Coordination log " + i.to_string()))
    Logger::emit(logger, log_record)
    
    Time::sleep(50)
  }
  
  // Wait for coordination
  Time::sleep(2000)
  Stream::flush(coordinated_stream)
  
  // Verify coordination
  assert_true(coordinated_messages.length() > 0)
  
  for message in coordinated_messages {
    assert_eq(StreamMessage::type(message), "coordinated_telemetry")
    
    let trace_count_attr = StreamMessage::get_attribute(message, "trace_count")
    match trace_count_attr {
      Some(IntValue(v)) => assert_true(v > 0)
      _ => assert_true(false)
    }
    
    let metric_count_attr = StreamMessage::get_attribute(message, "metric_count")
    match metric_count_attr {
      Some(IntValue(v)) => assert_true(v > 0)
      _ => assert_true(false)
    }
    
    let log_count_attr = StreamMessage::get_attribute(message, "log_count")
    match log_count_attr {
      Some(IntValue(v)) => assert_true(v > 0)
      _ => assert_true(false)
    }
  }
  
  // Clean up
  Subscriber::unsubscribe(subscriber)
  Stream::close(trace_stream)
  Stream::close(metric_stream)
  Stream::close(log_stream)
  Stream::close(coordinated_stream)
}