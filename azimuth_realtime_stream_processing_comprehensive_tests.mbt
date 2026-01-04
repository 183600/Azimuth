// Azimuth Telemetry System - Comprehensive Real-time Stream Processing Tests
// This file contains comprehensive test cases for real-time stream processing capabilities

// Test 1: Real-time Stream Creation and Basic Operations
test "realtime stream creation and basic operations" {
  // Create a real-time stream
  let stream = RealtimeStream::new("test_stream", Some("Test stream for basic operations"))
  
  // Verify stream properties
  assert_eq(RealtimeStream::name(stream), "test_stream")
  match RealtimeStream::description(stream) {
    Some(desc) => assert_eq(desc, "Test stream for basic operations")
    None => assert_true(false)
  }
  assert_true(RealtimeStream::is_active(stream))
  assert_eq(RealtimeStream::subscriber_count(stream), 0)
  
  // Create stream attributes
  let stream_attrs = Attributes::new()
  Attributes::set(stream_attrs, "stream.type", StringValue("test"))
  Attributes::set(stream_attrs, "stream.source", StringValue("unit_test"))
  
  // Add attributes to stream
  RealtimeStream::add_attributes(stream, stream_attrs)
  
  // Verify attributes are added
  let retrieved_attrs = RealtimeStream::get_attributes(stream)
  let stream_type = Attributes::get(retrieved_attrs, "stream.type")
  match stream_type {
    Some(StringValue(t)) => assert_eq(t, "test")
    None => assert_true(false)
  }
  
  // Close stream
  RealtimeStream::close(stream)
  assert_false(RealtimeStream::is_active(stream))
}

// Test 2: Stream Subscription and Publishing
test "stream subscription and publishing" {
  // Create stream
  let stream = RealtimeStream::new("pubsub_stream", Some("Stream for pub/sub testing"))
  
  // Create subscriber
  let subscriber = RealtimeStream::subscribe(stream)
  
  // Verify subscription
  assert_eq(RealtimeStream::subscriber_count(stream), 1)
  assert_true(RealtimeStream::is_subscribed(stream, subscriber))
  
  // Create publisher
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Publish telemetry data
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_attribute(telemetry_data, "test.id", IntValue(123))
  TelemetryData::add_attribute(telemetry_data, "test.type", StringValue("stream_test"))
  
  let publish_result = RealtimeStream::publish(publisher, telemetry_data)
  assert_true(publish_result)
  
  // Receive data from subscriber
  let received_data = RealtimeStream::receive(subscriber, 1000)  // 1 second timeout
  match received_data {
    Some(data) => {
      let test_id = TelemetryData::get_attribute(data, "test.id")
      match test_id {
        Some(IntValue(id)) => assert_eq(id, 123)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Unsubscribe
  RealtimeStream::unsubscribe(stream, subscriber)
  assert_eq(RealtimeStream::subscriber_count(stream), 0)
  assert_false(RealtimeStream::is_subscribed(stream, subscriber))
  
  // Close stream
  RealtimeStream::close(stream)
}

// Test 3: Stream Filtering and Transformation
test "stream filtering and transformation" {
  // Create stream
  let stream = RealtimeStream::new("filter_stream", Some("Stream for filtering testing"))
  
  // Create filter for error events only
  let error_filter = RealtimeStream::create_filter(fn(data: TelemetryData) -> Bool {
    match TelemetryData::get_attribute(data, "event.type") {
      Some(StringValue(event_type)) => event_type == "error"
      _ => false
    }
  })
  
  // Create filtered subscriber
  let error_subscriber = RealtimeStream::subscribe_with_filter(stream, error_filter)
  
  // Create publisher
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Publish error event
  let error_data = TelemetryData::new()
  TelemetryData::add_attribute(error_data, "event.type", StringValue("error"))
  TelemetryData::add_attribute(error_data, "error.code", IntValue(500))
  
  RealtimeStream::publish(publisher, error_data)
  
  // Publish non-error event
  let normal_data = TelemetryData::new()
  TelemetryData::add_attribute(normal_data, "event.type", StringValue("info"))
  TelemetryData::add_attribute(normal_data, "message", StringValue("Normal operation"))
  
  RealtimeStream::publish(publisher, normal_data)
  
  // Receive filtered data (should only get error event)
  let received_error = RealtimeStream::receive(error_subscriber, 1000)
  match received_error {
    Some(data) => {
      let event_type = TelemetryData::get_attribute(data, "event.type")
      match event_type {
        Some(StringValue(t)) => assert_eq(t, "error")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Should not receive normal event due to filter
  let received_normal = RealtimeStream::receive(error_subscriber, 500)  // Short timeout
  match received_normal {
    Some(_) => assert_true(false)  // Should not receive normal event
    None => assert_true(true)       // Expected - no normal event
  }
  
  // Create transformation function
  let transform_fn = fn(data: TelemetryData) -> TelemetryData {
    let transformed = TelemetryData::new()
    
    // Copy all original attributes
    let original_attrs = TelemetryData::get_all_attributes(data)
    for (key, value) in original_attrs {
      TelemetryData::add_attribute(transformed, key, value)
    }
    
    // Add transformation metadata
    TelemetryData::add_attribute(transformed, "transformed", BoolValue(true))
    TelemetryData::add_attribute(transformed, "transform.timestamp", IntValue(1234567890))
    
    transformed
  }
  
  // Create transformed subscriber
  let transform_subscriber = RealtimeStream::subscribe_with_transform(stream, transform_fn)
  
  // Publish data for transformation
  let transform_data = TelemetryData::new()
  TelemetryData::add_attribute(transform_data, "original.data", StringValue("test"))
  
  RealtimeStream::publish(publisher, transform_data)
  
  // Receive transformed data
  let received_transformed = RealtimeStream::receive(transform_subscriber, 1000)
  match received_transformed {
    Some(data) => {
      let original_data = TelemetryData::get_attribute(data, "original.data")
      match original_data {
        Some(StringValue(value)) => assert_eq(value, "test")
        None => assert_true(false)
      }
      
      let transformed = TelemetryData::get_attribute(data, "transformed")
      match transformed {
        Some(BoolValue(t)) => assert_true(t)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Close stream
  RealtimeStream::close(stream)
}

// Test 4: Stream Aggregation and Windowing
test "stream aggregation and windowing" {
  // Create stream
  let stream = RealtimeStream::new("aggregation_stream", Some("Stream for aggregation testing"))
  
  // Create time window aggregator (5-second windows)
  let window_aggregator = RealtimeStream::create_window_aggregator(5000)  // 5 seconds
  
  // Create aggregation function (count events)
  let count_fn = fn(data: Array<TelemetryData>) -> TelemetryData {
    let result = TelemetryData::new()
    TelemetryData::add_attribute(result, "aggregation.type", StringValue("count"))
    TelemetryData::add_attribute(result, "event.count", IntValue(data.length()))
    TelemetryData::add_attribute(result, "window.start", IntValue(1234567890))
    TelemetryData::add_attribute(result, "window.end", IntValue(1234567895))
    result
  }
  
  // Create aggregation subscriber
  let aggregation_subscriber = RealtimeStream::subscribe_with_aggregation(stream, window_aggregator, count_fn)
  
  // Create publisher
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Publish multiple events
  for i in 0..10 {
    let event_data = TelemetryData::new()
    TelemetryData::add_attribute(event_data, "event.id", IntValue(i))
    TelemetryData::add_attribute(event_data, "event.type", StringValue("test"))
    RealtimeStream::publish(publisher, event_data)
  }
  
  // Wait for aggregation (simulate time passing)
  // In real implementation, this would wait for the window to close
  // For testing, we'll trigger manual aggregation
  RealtimeStream::trigger_aggregation(stream, window_aggregator)
  
  // Receive aggregated data
  let received_aggregation = RealtimeStream::receive(aggregation_subscriber, 2000)
  match received_aggregation {
    Some(data) => {
      let aggregation_type = TelemetryData::get_attribute(data, "aggregation.type")
      match aggregation_type {
        Some(StringValue(t)) => assert_eq(t, "count")
        None => assert_true(false)
      }
      
      let event_count = TelemetryData::get_attribute(data, "event.count")
      match event_count {
        Some(IntValue(count)) => assert_eq(count, 10)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Close stream
  RealtimeStream::close(stream)
}

// Test 5: Stream Backpressure and Flow Control
test "stream backpressure and flow control" {
  // Create stream with limited buffer
  let stream = RealtimeStream::new_with_buffer("backpressure_stream", Some("Stream for backpressure testing"), 100)  // 100 item buffer
  
  // Create slow subscriber (simulates slow processing)
  let slow_subscriber = RealtimeStream::subscribe(stream)
  
  // Create publisher
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Publish many items quickly (should trigger backpressure)
  let mut publish_count = 0
  let mut backpressure_triggered = false
  
  for i in 0..200 {  // More than buffer capacity
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "event.id", IntValue(i))
    
    if RealtimeStream::publish(publisher, data) {
      publish_count = publish_count + 1
    } else {
      backpressure_triggered = true
      break
    }
  }
  
  // Verify backpressure was triggered
  assert_true(backpressure_triggered)
  assert_true(publish_count < 200)
  
  // Process some items from subscriber
  for i in 0..50 {
    let received = RealtimeStream::receive(slow_subscriber, 100)
    match received {
      Some(_) => {}  // Process item
      None => break
    }
  }
  
  // Should be able to publish more items now
  let additional_publish_count = 0
  for i in 0..50 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "additional.id", IntValue(i))
    
    if RealtimeStream::publish(publisher, data) {
      additional_publish_count = additional_publish_count + 1
    } else {
      break
    }
  }
  
  // Verify additional items could be published
  assert_true(additional_publish_count > 0)
  
  // Close stream
  RealtimeStream::close(stream)
}

// Test 6: Stream Persistence and Recovery
test "stream persistence and recovery" {
  // Create persistent stream
  let stream = RealtimeStream::new_persistent("persistent_stream", Some("Persistent stream for testing"))
  
  // Create subscriber
  let subscriber = RealtimeStream::subscribe(stream)
  
  // Create publisher
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Publish some data
  for i in 0..10 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "persistent.id", IntValue(i))
    TelemetryData::add_attribute(data, "persistent.data", StringValue("persistent_data_" + i.to_string()))
    RealtimeStream::publish(publisher, data)
  }
  
  // Force persistence
  RealtimeStream::persist(stream)
  
  // Close stream
  RealtimeStream::close(stream)
  
  // Recover stream
  let recovered_stream = RealtimeStream::recover("persistent_stream")
  
  // Verify recovery
  assert_true(RealtimeStream::is_active(recovered_stream))
  assert_eq(RealtimeStream::name(recovered_stream), "persistent_stream")
  
  // Create new subscriber for recovered stream
  let recovered_subscriber = RealtimeStream::subscribe(recovered_stream)
  
  // Should receive persisted data
  let mut received_count = 0
  for i in 0..10 {
    let received = RealtimeStream::receive(recovered_subscriber, 1000)
    match received {
      Some(data) => {
        let persistent_id = TelemetryData::get_attribute(data, "persistent.id")
        match persistent_id {
          Some(IntValue(id)) => assert_eq(id, i)
          None => assert_true(false)
        }
        received_count = received_count + 1
      }
      None => break
    }
  }
  
  // Verify all persisted data was recovered
  assert_eq(received_count, 10)
  
  // Close recovered stream
  RealtimeStream::close(recovered_stream)
}

// Test 7: Multi-stream Joins and Merges
test "multi-stream joins and merges" {
  // Create multiple streams
  let stream1 = RealtimeStream::new("stream1", Some("First stream for join testing"))
  let stream2 = RealtimeStream::new("stream2", Some("Second stream for join testing"))
  
  // Create publishers
  let publisher1 = RealtimeStream::create_publisher(stream1)
  let publisher2 = RealtimeStream::create_publisher(stream2)
  
  // Publish to stream1
  for i in 0..5 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "stream", StringValue("stream1"))
    TelemetryData::add_attribute(data, "join.key", StringValue("key_" + (i % 3).to_string()))  // Create some matching keys
    TelemetryData::add_attribute(data, "value", IntValue(i))
    RealtimeStream::publish(publisher1, data)
  }
  
  // Publish to stream2
  for i in 0..5 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "stream", StringValue("stream2"))
    TelemetryData::add_attribute(data, "join.key", StringValue("key_" + (i % 3).to_string()))  // Create some matching keys
    TelemetryData::add_attribute(data, "value", IntValue(i * 10))
    RealtimeStream::publish(publisher2, data)
  }
  
  // Create join function
  let join_fn = fn(data1: TelemetryData, data2: TelemetryData) -> TelemetryData {
    let joined = TelemetryData::new()
    
    // Get join key
    let key1 = TelemetryData::get_attribute(data1, "join.key")
    let key2 = TelemetryData::get_attribute(data2, "join.key")
    
    match (key1, key2) {
      (Some(StringValue(k1)), Some(StringValue(k2))) => {
        if k1 == k2 {
          TelemetryData::add_attribute(joined, "join.key", StringValue(k1))
          
          let value1 = TelemetryData::get_attribute(data1, "value")
          let value2 = TelemetryData::get_attribute(data2, "value")
          
          match (value1, value2) {
            (Some(IntValue(v1)), Some(IntValue(v2))) => {
              TelemetryData::add_attribute(joined, "stream1.value", IntValue(v1))
              TelemetryData::add_attribute(joined, "stream2.value", IntValue(v2))
              TelemetryData::add_attribute(joined, "sum", IntValue(v1 + v2))
            }
            _ => {}
          }
        }
      }
      _ => {}
    }
    
    joined
  }
  
  // Create joined stream
  let joined_stream = RealtimeStream::join_streams([stream1, stream2], join_fn)
  
  // Create subscriber for joined stream
  let joined_subscriber = RealtimeStream::subscribe(joined_stream)
  
  // Receive joined data
  let mut join_count = 0
  for i in 0..10 {
    let received = RealtimeStream::receive(joined_subscriber, 1000)
    match received {
      Some(data) => {
        join_count = join_count + 1
        
        let join_key = TelemetryData::get_attribute(data, "join.key")
        match join_key {
          Some(StringValue(_)) => {}  // Valid join
          None => assert_true(false)
        }
      }
      None => break
    }
  }
  
  // Verify joins occurred
  assert_true(join_count > 0)
  
  // Create merged stream
  let merged_stream = RealtimeStream::merge_streams([stream1, stream2])
  
  // Create subscriber for merged stream
  let merged_subscriber = RealtimeStream::subscribe(merged_stream)
  
  // Receive merged data
  let mut merge_count = 0
  for i in 0..15 {  // Should receive all items from both streams
    let received = RealtimeStream::receive(merged_subscriber, 1000)
    match received {
      Some(data) => {
        merge_count = merge_count + 1
        
        let stream_name = TelemetryData::get_attribute(data, "stream")
        match stream_name {
          Some(StringValue(name)) => assert_true(name == "stream1" || name == "stream2")
          None => assert_true(false)
        }
      }
      None => break
    }
  }
  
  // Verify all items were merged
  assert_eq(merge_count, 10)  // 5 from each stream
  
  // Close all streams
  RealtimeStream::close(stream1)
  RealtimeStream::close(stream2)
  RealtimeStream::close(joined_stream)
  RealtimeStream::close(merged_stream)
}

// Test 8: Stream Metrics and Monitoring
test "stream metrics and monitoring" {
  // Create stream
  let stream = RealtimeStream::new("metrics_stream", Some("Stream for metrics testing"))
  
  // Enable metrics collection
  RealtimeStream::enable_metrics(stream)
  
  // Create subscriber and publisher
  let subscriber = RealtimeStream::subscribe(stream)
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Get initial metrics
  let initial_metrics = RealtimeStream::get_metrics(stream)
  let initial_publish_count = RealtimeStreamMetrics::get_publish_count(initial_metrics)
  let initial_receive_count = RealtimeStreamMetrics::get_receive_count(initial_metrics)
  
  assert_eq(initial_publish_count, 0)
  assert_eq(initial_receive_count, 0)
  
  // Publish data
  for i in 0..10 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "metrics.test", IntValue(i))
    RealtimeStream::publish(publisher, data)
  }
  
  // Receive some data
  for i in 0..5 {
    let received = RealtimeStream::receive(subscriber, 1000)
    match received {
      Some(_) => {}  // Process data
      None => break
    }
  }
  
  // Get updated metrics
  let updated_metrics = RealtimeStream::get_metrics(stream)
  let updated_publish_count = RealtimeStreamMetrics::get_publish_count(updated_metrics)
  let updated_receive_count = RealtimeStreamMetrics::get_receive_count(updated_metrics)
  
  // Verify metrics updated
  assert_eq(updated_publish_count, 10)
  assert_eq(updated_receive_count, 5)
  
  // Verify additional metrics
  let subscriber_count = RealtimeStreamMetrics::get_subscriber_count(updated_metrics)
  assert_eq(subscriber_count, 1)
  
  let buffer_size = RealtimeStreamMetrics::get_buffer_size(updated_metrics)
  let buffer_usage = RealtimeStreamMetrics::get_buffer_usage(updated_metrics)
  
  assert_true(buffer_size > 0)
  assert_true(buffer_usage >= 0 && buffer_usage <= buffer_size)
  
  // Get throughput metrics
  let publish_throughput = RealtimeStreamMetrics::get_publish_throughput(updated_metrics)
  let receive_throughput = RealtimeStreamMetrics::get_receive_throughput(updated_metrics)
  
  assert_true(publish_throughput >= 0.0)
  assert_true(receive_throughput >= 0.0)
  
  // Get latency metrics
  let average_latency = RealtimeStreamMetrics::get_average_latency(updated_metrics)
  assert_true(average_latency >= 0.0)
  
  // Close stream
  RealtimeStream::close(stream)
}

// Test 9: Stream Error Handling and Recovery
test "stream error handling and recovery" {
  // Create stream
  let stream = RealtimeStream::new("error_stream", Some("Stream for error testing"))
  
  // Create subscriber and publisher
  let subscriber = RealtimeStream::subscribe(stream)
  let publisher = RealtimeStream::create_publisher(stream)
  
  // Create error handler
  let error_handler = RealtimeStream::create_error_handler(fn(error: StreamError) {
    match StreamError::type(error) {
      PublishError => {
        // Handle publish error
        StreamError::recover(error)
      }
      SubscribeError => {
        // Handle subscribe error
        StreamError::recover(error)
      }
      ProcessingError => {
        // Handle processing error
        StreamError::retry(error, 3)  // Retry up to 3 times
      }
    }
  })
  
  // Set error handler
  RealtimeStream::set_error_handler(stream, error_handler)
  
  // Publish normal data
  let normal_data = TelemetryData::new()
  TelemetryData::add_attribute(normal_data, "test.type", StringValue("normal"))
  let normal_result = RealtimeStream::publish(publisher, normal_data)
  assert_true(normal_result)
  
  // Simulate error condition
  let error_data = TelemetryData::new()
  TelemetryData::add_attribute(error_data, "test.type", StringValue("error_trigger"))
  
  // This should trigger error handling
  let error_result = RealtimeStream::publish(publisher, error_data)
  
  // Error handler should recover, so publish might succeed or fail gracefully
  // The important thing is that the stream continues to function
  
  // Publish more data after error
  let recovery_data = TelemetryData::new()
  TelemetryData::add_attribute(recovery_data, "test.type", StringValue("recovery"))
  let recovery_result = RealtimeStream::publish(publisher, recovery_data)
  
  // Stream should still be functional
  assert_true(RealtimeStream::is_active(stream))
  
  // Receive data
  let received = RealtimeStream::receive(subscriber, 1000)
  match received {
    Some(data) => {
      let test_type = TelemetryData::get_attribute(data, "test.type")
      match test_type {
        Some(StringValue(t)) => assert_true(t == "normal" || t == "recovery")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Get error metrics
  let error_metrics = RealtimeStream::get_error_metrics(stream)
  let error_count = RealtimeStreamErrorMetrics::get_error_count(error_metrics)
  let recovery_count = RealtimeStreamErrorMetrics::get_recovery_count(error_metrics)
  
  assert_true(error_count >= 0)
  assert_true(recovery_count >= 0)
  
  // Close stream
  RealtimeStream::close(stream)
}

// Test 10: Comprehensive Real-time Processing Scenario
test "comprehensive realtime processing scenario" {
  // Create a comprehensive real-time processing pipeline
  
  // 1. Create input streams from different sources
  let user_activity_stream = RealtimeStream::new("user_activity", Some("User activity events"))
  let system_metrics_stream = RealtimeStream::new("system_metrics", Some("System metrics"))
  let error_log_stream = RealtimeStream::new("error_logs", Some("Error log events"))
  
  // 2. Create filters for each stream
  let high_activity_filter = RealtimeStream::create_filter(fn(data: TelemetryData) -> Bool {
    match TelemetryData::get_attribute(data, "activity.level") {
      Some(StringValue(level)) => level == "high"
      _ => false
    }
  })
  
  let critical_metrics_filter = RealtimeStream::create_filter(fn(data: TelemetryData) -> Bool {
    match TelemetryData::get_attribute(data, "metric.severity") {
      Some(StringValue(severity)) => severity == "critical"
      _ => false
    }
  })
  
  let error_filter = RealtimeStream::create_filter(fn(data: TelemetryData) -> Bool {
    match TelemetryData::get_attribute(data, "log.level") {
      Some(StringValue(level)) => level == "error"
      _ => false
    }
  })
  
  // 3. Create filtered subscribers
  let high_activity_subscriber = RealtimeStream::subscribe_with_filter(user_activity_stream, high_activity_filter)
  let critical_metrics_subscriber = RealtimeStream::subscribe_with_filter(system_metrics_stream, critical_metrics_filter)
  let error_subscriber = RealtimeStream::subscribe_with_filter(error_log_stream, error_filter)
  
  // 4. Create aggregation functions
  let activity_aggregator = RealtimeStream::create_window_aggregator(10000)  // 10 second window
  let activity_aggregation_fn = fn(data: Array<TelemetryData>) -> TelemetryData {
    let result = TelemetryData::new()
    TelemetryData::add_attribute(result, "aggregation.type", StringValue("user_activity"))
    TelemetryData::add_attribute(result, "activity.count", IntValue(data.length()))
    TelemetryData::add_attribute(result, "aggregation.timestamp", IntValue(1234567890))
    result
  }
  
  let metrics_aggregator = RealtimeStream::create_window_aggregator(5000)  // 5 second window
  let metrics_aggregation_fn = fn(data: Array<TelemetryData>) -> TelemetryData {
    let result = TelemetryData::new()
    TelemetryData::add_attribute(result, "aggregation.type", StringValue("system_metrics"))
    TelemetryData::add_attribute(result, "metrics.count", IntValue(data.length()))
    TelemetryData::add_attribute(result, "aggregation.timestamp", IntValue(1234567890))
    result
  }
  
  // 5. Create aggregation subscribers
  let activity_aggregation_subscriber = RealtimeStream::subscribe_with_aggregation(
    user_activity_stream,
    activity_aggregator,
    activity_aggregation_fn
  )
  
  let metrics_aggregation_subscriber = RealtimeStream::subscribe_with_aggregation(
    system_metrics_stream,
    metrics_aggregator,
    metrics_aggregation_fn
  )
  
  // 6. Create alert function for error events
  let alert_fn = fn(data: TelemetryData) -> TelemetryData {
    let alert = TelemetryData::new()
    TelemetryData::add_attribute(alert, "alert.type", StringValue("error"))
    TelemetryData::add_attribute(alert, "alert.severity", StringValue("high"))
    
    let error_message = TelemetryData::get_attribute(data, "error.message")
    match error_message {
      Some(StringValue(message)) => TelemetryData::add_attribute(alert, "alert.message", StringValue(message))
      None => {}
    }
    
    TelemetryData::add_attribute(alert, "alert.timestamp", IntValue(1234567890))
    alert
  }
  
  // 7. Create alert stream
  let alert_stream = RealtimeStream::new("alerts", Some("Alert stream"))
  let alert_subscriber = RealtimeStream::subscribe(alert_stream)
  
  // 8. Create publishers
  let user_activity_publisher = RealtimeStream::create_publisher(user_activity_stream)
  let system_metrics_publisher = RealtimeStream::create_publisher(system_metrics_stream)
  let error_log_publisher = RealtimeStream::create_publisher(error_log_stream)
  let alert_publisher = RealtimeStream::create_publisher(alert_stream)
  
  // 9. Simulate data publishing
  
  // User activity events
  for i in 0..20 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "user.id", StringValue("user_" + i.to_string()))
    TelemetryData::add_attribute(data, "activity.level", if i % 3 == 0 { StringValue("high") } else { StringValue("normal") })
    TelemetryData::add_attribute(data, "activity.type", StringValue("page_view"))
    RealtimeStream::publish(user_activity_publisher, data)
  }
  
  // System metrics
  for i in 0..15 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "metric.name", StringValue("cpu_usage"))
    TelemetryData::add_attribute(data, "metric.value", FloatValue(i * 5.0))
    TelemetryData::add_attribute(data, "metric.severity", if i % 5 == 0 { StringValue("critical") } else { StringValue("normal") })
    RealtimeStream::publish(system_metrics_publisher, data)
  }
  
  // Error logs
  for i in 0..5 {
    let data = TelemetryData::new()
    TelemetryData::add_attribute(data, "log.level", StringValue("error"))
    TelemetryData::add_attribute(data, "error.message", StringValue("Error " + i.to_string()))
    TelemetryData::add_attribute(data, "error.code", IntValue(500 + i))
    
    // Publish to error log stream
    RealtimeStream::publish(error_log_publisher, data)
    
    // Create and publish alert
    let alert = alert_fn(data)
    RealtimeStream::publish(alert_publisher, alert)
  }
  
  // 10. Trigger aggregations
  RealtimeStream::trigger_aggregation(user_activity_stream, activity_aggregator)
  RealtimeStream::trigger_aggregation(system_metrics_stream, metrics_aggregator)
  
  // 11. Verify results
  
  // Receive high activity events
  let mut high_activity_count = 0
  for i in 0..10 {  // Should receive about 1/3 of events
    let received = RealtimeStream::receive(high_activity_subscriber, 1000)
    match received {
      Some(data) => {
        high_activity_count = high_activity_count + 1
        let activity_level = TelemetryData::get_attribute(data, "activity.level")
        match activity_level {
          Some(StringValue(level)) => assert_eq(level, "high")
          None => assert_true(false)
        }
      }
      None => break
    }
  }
  assert_true(high_activity_count > 0)
  
  // Receive critical metrics
  let mut critical_metrics_count = 0
  for i in 0..5 {  // Should receive about 1/5 of events
    let received = RealtimeStream::receive(critical_metrics_subscriber, 1000)
    match received {
      Some(data) => {
        critical_metrics_count = critical_metrics_count + 1
        let metric_severity = TelemetryData::get_attribute(data, "metric.severity")
        match metric_severity {
          Some(StringValue(severity)) => assert_eq(severity, "critical")
          None => assert_true(false)
        }
      }
      None => break
    }
  }
  assert_true(critical_metrics_count > 0)
  
  // Receive error events
  let mut error_count = 0
  for i in 0..5 {
    let received = RealtimeStream::receive(error_subscriber, 1000)
    match received {
      Some(data) => {
        error_count = error_count + 1
        let log_level = TelemetryData::get_attribute(data, "log.level")
        match log_level {
          Some(StringValue(level)) => assert_eq(level, "error")
          None => assert_true(false)
        }
      }
      None => break
    }
  }
  assert_eq(error_count, 5)
  
  // Receive activity aggregation
  let activity_aggregation = RealtimeStream::receive(activity_aggregation_subscriber, 2000)
  match activity_aggregation {
    Some(data) => {
      let aggregation_type = TelemetryData::get_attribute(data, "aggregation.type")
      match aggregation_type {
        Some(StringValue(t)) => assert_eq(t, "user_activity")
        None => assert_true(false)
      }
      
      let activity_count = TelemetryData::get_attribute(data, "activity.count")
      match activity_count {
        Some(IntValue(count)) => assert_eq(count, 20)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Receive metrics aggregation
  let metrics_aggregation = RealtimeStream::receive(metrics_aggregation_subscriber, 2000)
  match metrics_aggregation {
    Some(data) => {
      let aggregation_type = TelemetryData::get_attribute(data, "aggregation.type")
      match aggregation_type {
        Some(StringValue(t)) => assert_eq(t, "system_metrics")
        None => assert_true(false)
      }
      
      let metrics_count = TelemetryData::get_attribute(data, "metrics.count")
      match metrics_count {
        Some(IntValue(count)) => assert_eq(count, 15)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Receive alerts
  let mut alert_count = 0
  for i in 0..5 {
    let received = RealtimeStream::receive(alert_subscriber, 1000)
    match received {
      Some(data) => {
        alert_count = alert_count + 1
        let alert_type = TelemetryData::get_attribute(data, "alert.type")
        match alert_type {
          Some(StringValue(t)) => assert_eq(t, "error")
          None => assert_true(false)
        }
      }
      None => break
    }
  }
  assert_eq(alert_count, 5)
  
  // 12. Get stream metrics
  let user_activity_metrics = RealtimeStream::get_metrics(user_activity_stream)
  let system_metrics_metrics = RealtimeStream::get_metrics(system_metrics_stream)
  let error_log_metrics = RealtimeStream::get_metrics(error_log_stream)
  let alert_metrics = RealtimeStream::get_metrics(alert_stream)
  
  // Verify metrics
  assert_eq(RealtimeStreamMetrics::get_publish_count(user_activity_metrics), 20)
  assert_eq(RealtimeStreamMetrics::get_publish_count(system_metrics_metrics), 15)
  assert_eq(RealtimeStreamMetrics::get_publish_count(error_log_metrics), 5)
  assert_eq(RealtimeStreamMetrics::get_publish_count(alert_metrics), 5)
  
  // 13. Close all streams
  RealtimeStream::close(user_activity_stream)
  RealtimeStream::close(system_metrics_stream)
  RealtimeStream::close(error_log_stream)
  RealtimeStream::close(alert_stream)
}