// Azimuth Telemetry System - Real-time Stream Processing Tests
// This file contains comprehensive test cases for real-time stream processing functionality

// Test 1: Stream Creation and Basic Operations
test "stream creation and basic operations" {
  // Create a stream
  let stream = Stream::new("telemetry_stream", 1000) // Buffer size of 1000
  
  // Verify stream properties
  assert_eq(Stream::name(stream), "telemetry_stream")
  assert_eq(Stream::capacity(stream), 1000)
  assert_eq(Stream::size(stream), 0)
  assert_true(Stream::is_empty(stream))
  assert_false(Stream::is_full(stream))
  
  // Add telemetry data to stream
  let data1 = TelemetryData::new("metric1", 10.5, "ms", 1234567890L)
  let data2 = TelemetryData::new("metric2", 20.3, "count", 1234567891L)
  let data3 = TelemetryData::new("metric3", 30.7, "ratio", 1234567892L)
  
  assert_true(Stream::push(stream, data1))
  assert_true(Stream::push(stream, data2))
  assert_true(Stream::push(stream, data3))
  
  // Verify stream state after adding data
  assert_eq(Stream::size(stream), 3)
  assert_false(Stream::is_empty(stream))
  assert_false(Stream::is_full(stream))
  
  // Peek at data without removing
  let peeked_data = Stream::peek(stream)
  match peeked_data {
    Some(data) => assert_eq(data.metric_name, "metric1")
    None => assert_true(false)
  }
  
  // Verify size hasn't changed after peek
  assert_eq(Stream::size(stream), 3)
  
  // Pop data from stream
  let popped_data = Stream::pop(stream)
  match popped_data {
    Some(data) => assert_eq(data.metric_name, "metric1")
    None => assert_true(false)
  }
  
  // Verify size decreased after pop
  assert_eq(Stream::size(stream), 2)
  
  // Peek again to see next item
  let peeked_data = Stream::peek(stream)
  match peeked_data {
    Some(data) => assert_eq(data.metric_name, "metric2")
    None => assert_true(false)
  }
}

// Test 2: Stream Windowing Operations
test "stream windowing operations" {
  // Create a stream
  let stream = Stream::new("windowed_stream", 100)
  
  // Add telemetry data with different timestamps
  let base_time = 1234567890L
  for i in 0..=10 {
    let data = TelemetryData::new("metric", i.to_float(), "count", base_time + i * 100L)
    Stream::push(stream, data)
  }
  
  // Create time-based window (1 second)
  let time_window = StreamWindow::time_based(1000L) // 1 second window
  
  // Get data in the first window
  let window1_data = Stream::get_window(stream, time_window, base_time)
  assert_eq(window1_data.length(), 10) // Should include all data points within 1 second
  
  // Get data in a smaller window (500ms)
  let small_window = StreamWindow::time_based(500L) // 500ms window
  let window2_data = Stream::get_window(stream, small_window, base_time)
  assert_eq(window2_data.length(), 5) // Should include first 5 data points
  
  // Create count-based window
  let count_window = StreamWindow::count_based(5) // 5 items window
  let window3_data = Stream::get_window(stream, count_window, base_time)
  assert_eq(window3_data.length(), 5) // Should include first 5 data points
  
  // Create sliding window
  let sliding_window = StreamWindow::sliding(3, 1000L) // 3 items, 1 second slide
  let window4_data = Stream::get_window(stream, sliding_window, base_time + 200L)
  assert_eq(window4_data.length(), 3) // Should include 3 items starting from index 2
}

// Test 3: Stream Filtering and Transformation
test "stream filtering and transformation" {
  // Create a stream
  let stream = Stream::new("filter_transform_stream", 100)
  
  // Add telemetry data with various metrics
  Stream::push(stream, TelemetryData::new("cpu_usage", 75.5, "percent", 1234567890L))
  Stream::push(stream, TelemetryData::new("memory_usage", 60.2, "percent", 1234567891L))
  Stream::push(stream, TelemetryData::new("disk_usage", 45.8, "percent", 1234567892L))
  Stream::push(stream, TelemetryData::new("network_latency", 25.3, "ms", 1234567893L))
  Stream::push(stream, TelemetryData::new("error_rate", 0.05, "ratio", 1234567894L))
  
  // Filter by metric name
  let filtered_stream1 = Stream::filter(stream, fn(data) {
    data.metric_name.contains("usage")
  })
  
  let filtered_data1 = Stream::collect(filtered_stream1)
  assert_eq(filtered_data1.length(), 3) // cpu_usage, memory_usage, disk_usage
  
  // Filter by value
  let filtered_stream2 = Stream::filter(stream, fn(data) {
    data.value > 50.0
  })
  
  let filtered_data2 = Stream::collect(filtered_stream2)
  assert_eq(filtered_data2.length(), 2) // cpu_usage, memory_usage
  
  // Transform data
  let transformed_stream = Stream::map(stream, fn(data) {
    TelemetryData::new(
      "transformed_" + data.metric_name,
      data.value * 2.0,
      data.unit,
      data.timestamp
    )
  })
  
  let transformed_data = Stream::collect(transformed_stream)
  assert_eq(transformed_data.length(), 5)
  assert_eq(transformed_data[0].metric_name, "transformed_cpu_usage")
  assert_eq(transformed_data[0].value, 151.0) // 75.5 * 2
  
  // Chain filter and transform
  let chained_stream = Stream::filter(stream, fn(data) {
    data.unit == "percent"
  }).map(fn(data) {
    TelemetryData::new(
      data.metric_name + "_normalized",
      data.value / 100.0,
      "ratio",
      data.timestamp
    )
  })
  
  let chained_data = Stream::collect(chained_stream)
  assert_eq(chained_data.length(), 3)
  assert_eq(chained_data[0].metric_name, "cpu_usage_normalized")
  assert_eq(chained_data[0].value, 0.755) // 75.5 / 100
  assert_eq(chained_data[0].unit, "ratio")
}

// Test 4: Stream Aggregation
test "stream aggregation" {
  // Create a stream
  let stream = Stream::new("aggregation_stream", 100)
  
  // Add telemetry data
  for i in 0..=10 {
    let data = TelemetryData::new("metric", (i * 10).to_float(), "count", 1234567890L + i.to_long())
    Stream::push(stream, data)
  }
  
  // Count aggregation
  let count = Stream::count(stream)
  assert_eq(count, 11)
  
  // Sum aggregation
  let sum = Stream::sum(stream, fn(data) { data.value })
  assert_eq(sum, 550.0) // Sum of 0, 10, 20, ..., 100
  
  // Average aggregation
  let avg = Stream::average(stream, fn(data) { data.value })
  assert_eq(avg, 50.0) // 550 / 11
  
  // Min aggregation
  let min = Stream::min(stream, fn(data) { data.value })
  assert_eq(min, 0.0)
  
  // Max aggregation
  let max = Stream::max(stream, fn(data) { data.value })
  assert_eq(max, 100.0)
  
  // Group by metric name aggregation
  let stream2 = Stream::new("multi_metric_stream", 100)
  Stream::push(stream2, TelemetryData::new("cpu", 50.0, "percent", 1234567890L))
  Stream::push(stream2, TelemetryData::new("memory", 60.0, "percent", 1234567891L))
  Stream::push(stream2, TelemetryData::new("cpu", 70.0, "percent", 1234567892L))
  Stream::push(stream2, TelemetryData::new("memory", 80.0, "percent", 1234567893L))
  
  let grouped = Stream::group_by(stream2, fn(data) { data.metric_name })
  let cpu_group = grouped["cpu"]
  let memory_group = grouped["memory"]
  
  assert_eq(cpu_group.length(), 2)
  assert_eq(memory_group.length(), 2)
  
  let cpu_avg = Stream::average_values(cpu_group)
  assert_eq(cpu_avg, 60.0) // (50 + 70) / 2
  
  let memory_avg = Stream::average_values(memory_group)
  assert_eq(memory_avg, 70.0) // (60 + 80) / 2
}

// Test 5: Stream Join Operations
test "stream join operations" {
  // Create two streams
  let stream1 = Stream::new("join_stream1", 100)
  let stream2 = Stream::new("join_stream2", 100)
  
  // Add data to stream1
  Stream::push(stream1, TelemetryData::with_attributes("metric1", 10.0, "count", 1234567890L, [
    ("join_key", "key1"),
    ("source", "stream1")
  ]))
  Stream::push(stream1, TelemetryData::with_attributes("metric2", 20.0, "count", 1234567891L, [
    ("join_key", "key2"),
    ("source", "stream1")
  ]))
  Stream::push(stream1, TelemetryData::with_attributes("metric3", 30.0, "count", 1234567892L, [
    ("join_key", "key3"),
    ("source", "stream1")
  ]))
  
  // Add data to stream2
  Stream::push(stream2, TelemetryData::with_attributes("metricA", 100.0, "count", 1234567890L, [
    ("join_key", "key1"),
    ("source", "stream2")
  ]))
  Stream::push(stream2, TelemetryData::with_attributes("metricB", 200.0, "count", 1234567891L, [
    ("join_key", "key2"),
    ("source", "stream2")
  ]))
  Stream::push(stream2, TelemetryData::with_attributes("metricC", 300.0, "count", 1234567893L, [
    ("join_key", "key4"),
    ("source", "stream2")
  ]))
  
  // Inner join on join_key
  let inner_joined = Stream::inner_join(
    stream1,
    stream2,
    fn(data1) { TelemetryData::get_attribute(data1, "join_key") },
    fn(data2) { TelemetryData::get_attribute(data2, "join_key") }
  )
  
  let inner_joined_data = Stream::collect(inner_joined)
  assert_eq(inner_joined_data.length(), 2) // key1 and key2 should match
  
  // Left join on join_key
  let left_joined = Stream::left_join(
    stream1,
    stream2,
    fn(data1) { TelemetryData::get_attribute(data1, "join_key") },
    fn(data2) { TelemetryData::get_attribute(data2, "join_key") }
  )
  
  let left_joined_data = Stream::collect(left_joined)
  assert_eq(left_joined_data.length(), 3) // All from stream1, with matching from stream2 where available
  
  // Time-based join (within 1 second)
  let time_joined = Stream::time_based_join(
    stream1,
    stream2,
    1000L, // 1 second window
    fn(data1) { TelemetryData::get_attribute(data1, "join_key") },
    fn(data2) { TelemetryData::get_attribute(data2, "join_key") }
  )
  
  let time_joined_data = Stream::collect(time_joined)
  assert_eq(time_joined_data.length(), 2) // key1 and key2 have matching timestamps
}

// Test 6: Stream Processing with Backpressure
test "stream processing with backpressure" {
  // Create a stream with small buffer to test backpressure
  let stream = Stream::new("backpressure_stream", 5) // Small buffer
  
  // Create a processor that processes slowly
  let processor = StreamProcessor::new(stream)
  
  // Start processing in background
  let processing_thread = Thread::spawn(fn() {
    StreamProcessor::process(processor, fn(data) {
      Thread::sleep(100) // Simulate slow processing
      TelemetryData::new(
        "processed_" + data.metric_name,
        data.value * 2.0,
        data.unit,
        data.timestamp
      )
    })
  })
  
  // Try to add more data than buffer can hold
  let mut push_success_count = 0
  for i in 0..=10 {
    let data = TelemetryData::new("metric" + i.to_string(), i.to_float(), "count", 1234567890L + i.to_long())
    if Stream::push_with_timeout(stream, data, 50) { // 50ms timeout
      push_success_count = push_success_count + 1
    }
  }
  
  // Some pushes should fail due to backpressure
  assert_true(push_success_count < 10)
  assert_true(push_success_count <= 5) // Buffer size
  
  // Wait for processing to complete
  Thread::sleep(2000) // Wait for processing to catch up
  
  // Now we should be able to push more data
  for i in 0..=4 {
    let data = TelemetryData::new("metric_after" + i.to_string(), i.to_float(), "count", 1234567900L + i.to_long())
    assert_true(Stream::push_with_timeout(stream, data, 50))
  }
  
  // Stop processing
  StreamProcessor::stop(processor)
  Thread::join(processing_thread)
}

// Test 7: Stream Anomaly Detection
test "stream anomaly detection" {
  // Create a stream
  let stream = Stream::new("anomaly_stream", 100)
  
  // Add normal telemetry data
  for i in 0..=20 {
    let data = TelemetryData::new("response_time", 50.0 + (Random::float() * 10.0 - 5.0), "ms", 1234567890L + i.to_long())
    Stream::push(stream, data)
  }
  
  // Add anomaly data
  Stream::push(stream, TelemetryData::new("response_time", 200.0, "ms", 1234567911L)) // High value anomaly
  Stream::push(stream, TelemetryData::new("response_time", 5.0, "ms", 1234567912L))   // Low value anomaly
  
  // Create anomaly detector
  let detector = AnomalyDetector::new(AnomalyDetectionMethod::Statistical)
  
  // Configure detector
  AnomalyDetector::set_threshold(detector, 2.0) // 2 standard deviations
  AnomalyDetector::set_window_size(detector, 10) // Use last 10 data points
  
  // Process stream for anomalies
  let anomalies = Stream::detect_anomalies(stream, detector)
  
  // Should detect the anomalies
  assert_true(anomalies.length() >= 2)
  
  // Verify anomalies are the high and low values
  assert_true(anomalies.any(fn(anomaly) { anomaly.value > 150.0 })) // High anomaly
  assert_true(anomalies.any(fn(anomaly) { anomaly.value < 20.0 }))  // Low anomaly
  
  // Test with different detection method
  let ml_detector = AnomalyDetector::new(AnomalyDetectionMethod::MachineLearning)
  AnomalyDetector::train(ml_detector, stream) // Train on existing data
  
  // Add more data including anomalies
  Stream::push(stream, TelemetryData::new("response_time", 55.0, "ms", 1234567913L))
  Stream::push(stream, TelemetryData::new("response_time", 300.0, "ms", 1234567914L)) // Another anomaly
  
  let ml_anomalies = Stream::detect_anomalies(stream, ml_detector)
  assert_true(ml_anomalies.length() >= 1)
}

// Test 8: Stream Persistence and Recovery
test "stream persistence and recovery" {
  // Create a stream
  let stream = Stream::new("persistent_stream", 100)
  
  // Add telemetry data
  for i in 0..=10 {
    let data = TelemetryData::new("metric", i.to_float(), "count", 1234567890L + i.to_long())
    Stream::push(stream, data)
  }
  
  // Persist stream to storage
  let storage = InMemoryStorage::new()
  let result = Stream::persist(stream, storage)
  assert_true(result)
  
  // Verify stream was persisted
  assert_true(Storage::contains(storage, "persistent_stream"))
  
  // Create new stream and recover from storage
  let recovered_stream = Stream::recover("persistent_stream", storage)
  
  // Verify recovered stream has same data
  assert_eq(Stream::name(recovered_stream), "persistent_stream")
  assert_eq(Stream::size(recovered_stream), 11)
  
  // Verify data integrity
  let recovered_data = Stream::collect(recovered_stream)
  for i in 0..=10 {
    assert_eq(recovered_data[i].metric_name, "metric")
    assert_eq(recovered_data[i].value, i.to_float())
    assert_eq(recovered_data[i].unit, "count")
    assert_eq(recovered_data[i].timestamp, 1234567890L + i.to_long())
  }
  
  // Test incremental persistence
  for i in 11..=15 {
    let data = TelemetryData::new("metric", i.to_float(), "count", 1234567890L + i.to_long())
    Stream::push(stream, data)
  }
  
  // Persist incrementally
  let result = Stream::persist_incremental(stream, storage)
  assert_true(result)
  
  // Recover again
  let recovered_stream2 = Stream::recover("persistent_stream", storage)
  assert_eq(Stream::size(recovered_stream2), 16)
  
  // Verify new data was added
  let recovered_data2 = Stream::collect(recovered_stream2)
  for i in 11..=15 {
    assert_eq(recovered_data2[i].value, i.to_float())
  }
}