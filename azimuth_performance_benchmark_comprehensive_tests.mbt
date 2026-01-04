// Azimuth Telemetry System - Performance Benchmark Tests
// This file contains test cases for performance benchmarking

// Test 1: Telemetry Data Creation Performance
test "telemetry data creation performance" {
  let benchmark = Benchmark::new("telemetry_data_creation")
  
  // Benchmark basic telemetry data creation
  let basic_result = Benchmark::measure(benchmark, 10000, fn() {
    let telemetry_data = TelemetryData::new("test_metric", 42.5)
    telemetry_data
  })
  
  // Basic telemetry data creation should be fast
  assert_true(BenchmarkResult::average_time(basic_result) < 0.001) // Less than 1ms per operation
  assert_eq(BenchmarkResult::operation_count(basic_result), 10000)
  
  // Benchmark telemetry data with attributes
  let attrs_result = Benchmark::measure(benchmark, 10000, fn() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "host", StringValue("server-01"))
    Attributes::set(attrs, "region", StringValue("us-west-1"))
    let telemetry_data = TelemetryData::with_attributes("test_metric", 42.5, attrs)
    telemetry_data
  })
  
  // Telemetry data with attributes should still be reasonably fast
  assert_true(BenchmarkResult::average_time(attrs_result) < 0.005) // Less than 5ms per operation
  
  // Benchmark complex telemetry data
  let complex_result = Benchmark::measure(benchmark, 1000, fn() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "host", StringValue("server-01"))
    Attributes::set(attrs, "region", StringValue("us-west-1"))
    Attributes::set(attrs, "service", StringValue("auth-service"))
    Attributes::set(attrs, "version", StringValue("1.2.3"))
    Attributes::set(attrs, "environment", StringValue("production"))
    
    let telemetry_data = TelemetryData::with_attributes("test_metric", 42.5, attrs)
    TelemetryData::add_tag(telemetry_data, "importance", "high")
    TelemetryData::add_metadata(telemetry_data, "description", "Important metric")
    telemetry_data
  })
  
  // Complex telemetry data creation should complete in reasonable time
  assert_true(BenchmarkResult::average_time(complex_result) < 0.01) // Less than 10ms per operation
}

// Test 2: Attributes Operations Performance
test "attributes operations performance" {
  let benchmark = Benchmark::new("attributes_operations")
  
  // Benchmark attribute setting
  let set_result = Benchmark::measure(benchmark, 10000, fn() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "test_key", StringValue("test_value"))
    attrs
  })
  
  // Attribute setting should be fast
  assert_true(BenchmarkResult::average_time(set_result) < 0.001) // Less than 1ms per operation
  
  // Benchmark attribute getting
  let attrs = Attributes::new()
  Attributes::set(attrs, "test_key", StringValue("test_value"))
  
  let get_result = Benchmark::measure(benchmark, 10000, fn() {
    Attributes::get(attrs, "test_key")
  })
  
  // Attribute getting should be very fast
  assert_true(BenchmarkResult::average_time(get_result) < 0.0005) // Less than 0.5ms per operation
  
  // Benchmark multiple attributes operations
  let multi_result = Benchmark::measure(benchmark, 1000, fn() {
    let attrs = Attributes::new()
    
    // Set multiple attributes
    for i in 0..=9 {
      let key = "key_" + i.to_string()
      let value = "value_" + i.to_string()
      Attributes::set(attrs, key, StringValue(value))
    }
    
    // Get multiple attributes
    for i in 0..=9 {
      let key = "key_" + i.to_string()
      Attributes::get(attrs, key) |> ignore
    }
    
    attrs
  })
  
  // Multiple attributes operations should complete in reasonable time
  assert_true(BenchmarkResult::average_time(multi_result) < 0.01) // Less than 10ms per operation
}

// Test 3: Serialization Performance
test "serialization performance" {
  let benchmark = Benchmark::new("serialization")
  
  // Create test data
  let telemetry_data = TelemetryData::with_attributes(
    "test_metric",
    42.5,
    Attributes::from_list([
      ("host", StringValue("server-01")),
      ("region", StringValue("us-west-1")),
      ("service", StringValue("auth-service"))
    ])
  )
  
  // Benchmark JSON serialization
  let json_serialize_result = Benchmark::measure(benchmark, 10000, fn() {
    Serialization::to_json(telemetry_data)
  })
  
  // JSON serialization should be reasonably fast
  assert_true(BenchmarkResult::average_time(json_serialize_result) < 0.005) // Less than 5ms per operation
  
  // Benchmark JSON deserialization
  let json_data = Serialization::to_json(telemetry_data)
  let json_deserialize_result = Benchmark::measure(benchmark, 10000, fn() {
    Serialization::from_json(json_data)
  })
  
  // JSON deserialization should be reasonably fast
  assert_true(BenchmarkResult::average_time(json_deserialize_result) < 0.01) // Less than 10ms per operation
  
  // Benchmark binary serialization
  let binary_serialize_result = Benchmark::measure(benchmark, 10000, fn() {
    Serialization::to_binary(telemetry_data)
  })
  
  // Binary serialization should be faster than JSON
  assert_true(BenchmarkResult::average_time(binary_serialize_result) < BenchmarkResult::average_time(json_serialize_result))
  
  // Benchmark binary deserialization
  let binary_data = Serialization::to_binary(telemetry_data)
  let binary_deserialize_result = Benchmark::measure(benchmark, 10000, fn() {
    Serialization::from_binary(binary_data)
  })
  
  // Binary deserialization should be faster than JSON
  assert_true(BenchmarkResult::average_time(binary_deserialize_result) < BenchmarkResult::average_time(json_deserialize_result))
}

// Test 4: Metrics Collection Performance
test "metrics collection performance" {
  let benchmark = Benchmark::new("metrics_collection")
  
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Benchmark counter operations
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  let counter_result = Benchmark::measure(benchmark, 10000, fn() {
    Counter::add(counter, 1.0)
  })
  
  // Counter operations should be very fast
  assert_true(BenchmarkResult::average_time(counter_result) < 0.001) // Less than 1ms per operation
  
  // Benchmark histogram operations
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  let histogram_result = Benchmark::measure(benchmark, 10000, fn() {
    Histogram::record(histogram, Random::next() * 100.0)
  })
  
  // Histogram operations should be fast
  assert_true(BenchmarkResult::average_time(histogram_result) < 0.002) // Less than 2ms per operation
  
  // Benchmark gauge operations
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("value"))
  
  let gauge_result = Benchmark::measure(benchmark, 10000, fn() {
    Gauge::set(gauge, Random::next() * 100.0)
  })
  
  // Gauge operations should be very fast
  assert_true(BenchmarkResult::average_time(gauge_result) < 0.001) // Less than 1ms per operation
}

// Test 5: Time Series Processing Performance
test "time series processing performance" {
  let benchmark = Benchmark::new("time_series_processing")
  
  // Create a time series with many data points
  let time_series = TimeSeries::new("performance_test_series")
  
  // Benchmark adding data points
  let add_result = Benchmark::measure(benchmark, 10000, fn() {
    let timestamp = Time::now()
    let value = Random::next() * 100.0
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  })
  
  // Adding data points should be fast
  assert_true(BenchmarkResult::average_time(add_result) < 0.001) // Less than 1ms per operation
  
  // Benchmark aggregation operations
  for i in 0..=999 {
    let timestamp = Time::now() + (i as Int64)
    let value = Random::next() * 100.0
    TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
  }
  
  let aggregation_result = Benchmark::measure(benchmark, 1000, fn() {
    TimeSeries::aggregate(time_series, Average)
  })
  
  // Aggregation should complete in reasonable time
  assert_true(BenchmarkResult::average_time(aggregation_result) < 0.01) // Less than 10ms per operation
  
  // Benchmark windowing operations
  let windowing_result = Benchmark::measure(benchmark, 100, fn() {
    TimeSeries::window_by_time(time_series, 10000) // 10-second windows
  })
  
  // Windowing should complete in reasonable time
  assert_true(BenchmarkResult::average_time(windowing_result) < 0.05) // Less than 50ms per operation
  
  // Benchmark resampling operations
  let resampling_result = Benchmark::measure(benchmark, 100, fn() {
    TimeSeries::resample(time_series, 1000, LinearInterpolation) // 1-second intervals
  })
  
  // Resampling should complete in reasonable time
  assert_true(BenchmarkResult::average_time(resampling_result) < 0.1) // Less than 100ms per operation
}

// Test 6: Memory Usage Performance
test "memory usage performance" {
  let memory_profiler = MemoryProfiler::new()
  
  // Profile memory usage for telemetry data creation
  let telemetry_data_memory = MemoryProfiler::profile(memory_profiler, 1000, fn() {
    let attrs = Attributes::new()
    Attributes::set(attrs, "host", StringValue("server-01"))
    Attributes::set(attrs, "region", StringValue("us-west-1"))
    TelemetryData::with_attributes("test_metric", 42.5, attrs)
  })
  
  // Memory usage should be reasonable
  assert_true(MemoryProfile::average_memory_per_item(telemetry_data_memory) < 1024) // Less than 1KB per item
  
  // Profile memory usage for time series
  let time_series_memory = MemoryProfiler::profile(memory_profiler, 100, fn() {
    let time_series = TimeSeries::new("memory_test_series")
    
    for i in 0..=99 {
      let timestamp = Time::now() + (i as Int64)
      let value = Random::next() * 100.0
      TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
    }
    
    time_series
  })
  
  // Memory usage for time series should be reasonable
  assert_true(MemoryProfile::average_memory_per_item(time_series_memory) < 10240) // Less than 10KB per time series
  
  // Profile memory usage for attributes
  let attributes_memory = MemoryProfiler::profile(memory_profiler, 1000, fn() {
    let attrs = Attributes::new()
    
    for i in 0..=9 {
      let key = "key_" + i.to_string()
      let value = "value_" + i.to_string()
      Attributes::set(attrs, key, StringValue(value))
    }
    
    attrs
  })
  
  // Memory usage for attributes should be reasonable
  assert_true(MemoryProfile::average_memory_per_item(attributes_memory) < 512) // Less than 512B per attributes object
}

// Test 7: Throughput Performance
test "throughput performance" {
  let throughput_tester = ThroughputTester::new()
  
  // Test telemetry data processing throughput
  let telemetry_throughput = ThroughputTester::measure(throughput_tester, Duration::seconds(5), fn() {
    let telemetry_data = TelemetryData::new("test_metric", Random::next() * 100.0)
    TelemetryProcessor::process(telemetry_data)
  })
  
  // Should process at least 1000 telemetry data points per second
  assert_true(ThroughputResult::operations_per_second(telemetry_throughput) >= 1000)
  
  // Test metrics collection throughput
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "throughput_test_meter")
  let counter = Meter::create_counter(meter, "throughput_counter", Some("Throughput test"), Some("count"))
  
  let metrics_throughput = ThroughputTester::measure(throughput_tester, Duration::seconds(5), fn() {
    Counter::add(counter, 1.0)
  })
  
  // Should collect at least 5000 metrics per second
  assert_true(ThroughputResult::operations_per_second(metrics_throughput) >= 5000)
  
  // Test serialization throughput
  let telemetry_data = TelemetryData::with_attributes(
    "test_metric",
    42.5,
    Attributes::from_list([
      ("host", StringValue("server-01")),
      ("region", StringValue("us-west-1"))
    ])
  )
  
  let serialization_throughput = ThroughputTester::measure(throughput_tester, Duration::seconds(5), fn() {
    let json_data = Serialization::to_json(telemetry_data)
    Serialization::from_json(json_data) |> ignore
  })
  
  // Should serialize/deserialize at least 500 telemetry data points per second
  assert_true(ThroughputResult::operations_per_second(serialization_throughput) >= 500)
}

// Test 8: Latency Performance
test "latency performance" {
  let latency_tester = LatencyTester::new()
  
  // Test telemetry data creation latency
  let creation_latency = LatencyTester::measure(latency_tester, 1000, fn() {
    TelemetryData::new("test_metric", 42.5)
  })
  
  // 99th percentile latency should be low
  assert_true(LatencyResult::percentile(creation_latency, 0.99) < 0.01) // Less than 10ms
  
  // Test attribute operations latency
  let attrs = Attributes::new()
  Attributes::set(attrs, "test_key", StringValue("test_value"))
  
  let attribute_latency = LatencyTester::measure(latency_tester, 1000, fn() {
    Attributes::get(attrs, "test_key")
  })
  
  // 99th percentile latency should be very low
  assert_true(LatencyResult::percentile(attribute_latency, 0.99) < 0.001) // Less than 1ms
  
  // Test serialization latency
  let telemetry_data = TelemetryData::with_attributes(
    "test_metric",
    42.5,
    Attributes::from_list([
      ("host", StringValue("server-01")),
      ("region", StringValue("us-west-1"))
    ])
  )
  
  let serialization_latency = LatencyTester::measure(latency_tester, 1000, fn() {
    Serialization::to_json(telemetry_data)
  })
  
  // 99th percentile latency should be reasonable
  assert_true(LatencyResult::percentile(serialization_latency, 0.99) < 0.01) // Less than 10ms
}

// Test 9: Scalability Performance
test "scalability performance" {
  let scalability_tester = ScalabilityTester::new()
  
  // Test scalability with increasing data volume
  let volume_scalability = scalability_tester.test_with_increasing_volume(
    fn(size) {
      let time_series = TimeSeries::new("scalability_test")
      
      for i in 0..=(size - 1) {
        let timestamp = Time::now() + (i as Int64)
        let value = Random::next() * 100.0
        TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
      }
      
      TimeSeries::aggregate(time_series, Average)
    },
    [100, 1000, 10000, 100000]
  )
  
  // Performance should scale linearly or better
  assert_true(ScalabilityResult::is_linear_or_better(volume_scalability))
  
  // Test scalability with increasing complexity
  let complexity_scalability = scalability_tester.test_with_increasing_complexity(
    fn(complexity) {
      let attrs = Attributes::new()
      
      for i in 0..=(complexity - 1) {
        let key = "key_" + i.to_string()
        let value = "value_" + i.to_string()
        Attributes::set(attrs, key, StringValue(value))
      }
      
      for i in 0..=(complexity - 1) {
        let key = "key_" + i.to_string()
        Attributes::get(attrs, key) |> ignore
      }
      
      attrs
    },
    [10, 50, 100, 500]
  )
  
  // Performance should scale reasonably with complexity
  assert_true(ScalabilityResult::is_acceptable(complexity_scalability))
}

// Test 10: Resource Utilization Performance
test "resource utilization performance" {
  let resource_monitor = ResourceMonitor::new()
  
  // Monitor CPU usage during telemetry processing
  let cpu_usage = ResourceMonitor::measure_cpu_usage(resource_monitor, Duration::seconds(5), fn() {
    for i in 0..=9999 {
      let telemetry_data = TelemetryData::new("test_metric", Random::next() * 100.0)
      TelemetryProcessor::process(telemetry_data) |> ignore
    }
  })
  
  // CPU usage should be reasonable
  assert_true(ResourceUsage::average_cpu_percentage(cpu_usage) < 80.0) // Less than 80% CPU
  
  // Monitor memory usage during time series processing
  let memory_usage = ResourceMonitor::measure_memory_usage(resource_monitor, Duration::seconds(5), fn() {
    let time_series = TimeSeries::new("resource_test_series")
    
    for i in 0..=9999 {
      let timestamp = Time::now() + (i as Int64)
      let value = Random::next() * 100.0
      TimeSeries::add_data_point(time_series, TimeSeriesDataPoint::new(timestamp, value))
    }
    
    // Perform various operations
    TimeSeries::aggregate(time_series, Average) |> ignore
    TimeSeries::window_by_time(time_series, 1000) |> ignore
    TimeSeries::resample(time_series, 500, LinearInterpolation) |> ignore
  })
  
  // Memory usage should be reasonable
  assert_true(ResourceUsage::peak_memory_mb(memory_usage) < 100.0) // Less than 100MB
  
  // Monitor I/O usage during serialization
  let telemetry_data = TelemetryData::with_attributes(
    "test_metric",
    42.5,
    Attributes::from_list([
      ("host", StringValue("server-01")),
      ("region", StringValue("us-west-1")),
      ("service", StringValue("auth-service")),
      ("version", StringValue("1.2.3")),
      ("environment", StringValue("production"))
    ])
  )
  
  let io_usage = ResourceMonitor::measure_io_usage(resource_monitor, Duration::seconds(5), fn() {
    for i in 0..=9999 {
      let json_data = Serialization::to_json(telemetry_data)
      Serialization::from_json(json_data) |> ignore
    }
  })
  
  // I/O usage should be reasonable
  assert_true(ResourceUsage::average_io_operations_per_second(io_usage) >= 100) // At least 100 ops/sec
}