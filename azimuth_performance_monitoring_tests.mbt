// Azimuth Telemetry System - Performance Monitoring Tests
// This file contains comprehensive test cases for performance monitoring and metrics

// Test 1: CPU Performance Monitoring
test "cpu performance monitoring" {
  let cpu_monitor = CpuMonitor::new()
  
  // Test CPU usage measurement
  let cpu_usage = CpuMonitor::get_current_usage(cpu_monitor)
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // Test CPU load average
  let load_averages = CpuMonitor::get_load_averages(cpu_monitor)
  assert_eq(load_averages.length(), 3) // 1min, 5min, 15min averages
  
  // Test CPU core utilization
  let core_count = CpuMonitor::get_core_count(cpu_monitor)
  assert_true(core_count > 0)
  
  let core_utilizations = CpuMonitor::get_core_utilizations(cpu_monitor)
  assert_eq(core_utilizations.length(), core_count)
  
  // Test CPU frequency monitoring
  let frequencies = CpuMonitor::get_cpu_frequencies(cpu_monitor)
  assert_true(frequencies.length() > 0)
  for freq in frequencies {
    assert_true(freq > 0.0)
  }
}

// Test 2: Memory Performance Monitoring
test "memory performance monitoring" {
  let memory_monitor = MemoryMonitor::new()
  
  // Test total memory
  let total_memory = MemoryMonitor::get_total_memory(memory_monitor)
  assert_true(total_memory > 0L)
  
  // Test available memory
  let available_memory = MemoryMonitor::get_available_memory(memory_monitor)
  assert_true(available_memory >= 0L && available_memory <= total_memory)
  
  // Test used memory calculation
  let used_memory = MemoryMonitor::get_used_memory(memory_monitor)
  assert_true(used_memory >= 0L && used_memory <= total_memory)
  
  // Test memory usage percentage
  let memory_usage_percent = MemoryMonitor::get_usage_percentage(memory_monitor)
  assert_true(memory_usage_percent >= 0.0 && memory_usage_percent <= 100.0)
  
  // Test memory allocation tracking
  let allocation_tracker = MemoryAllocationTracker::new()
  let block1 = AllocationTracker::allocate(allocation_tracker, 1024)
  let block2 = AllocationTracker::allocate(allocation_tracker, 2048)
  
  assert_eq(AllocationTracker::get_allocated_bytes(allocation_tracker), 3072)
  assert_eq(AllocationTracker::get_allocation_count(allocation_tracker), 2)
  
  AllocationTracker::deallocate(allocation_tracker, block1)
  assert_eq(AllocationTracker::get_allocated_bytes(allocation_tracker), 2048)
  assert_eq(AllocationTracker::get_allocation_count(allocation_tracker), 1)
}

// Test 3: Disk I/O Performance Monitoring
test "disk io performance monitoring" {
  let disk_monitor = DiskMonitor::new()
  
  // Test disk space information
  let disk_info = DiskMonitor::get_disk_info(disk_monitor, "/")
  assert_true(DiskInfo::total_space(disk_info) > 0L)
  assert_true(DiskInfo::free_space(disk_info) >= 0L)
  assert_true(DiskInfo::used_space(disk_info) >= 0L)
  
  // Test disk usage percentage
  let usage_percent = DiskInfo::usage_percentage(disk_info)
  assert_true(usage_percent >= 0.0 && usage_percent <= 100.0)
  
  // Test disk I/O statistics
  let io_stats = DiskMonitor::get_io_statistics(disk_monitor)
  assert_true(IOStats::read_bytes(io_stats) >= 0L)
  assert_true(IOStats::write_bytes(io_stats) >= 0L)
  assert_true(IOStats::read_operations(io_stats) >= 0L)
  assert_true(IOStats::write_operations(io_stats) >= 0L)
  
  // Test disk throughput calculation
  let read_throughput = DiskMonitor::calculate_read_throughput(disk_monitor, 1000L)
  let write_throughput = DiskMonitor::calculate_write_throughput(disk_monitor, 1000L)
  assert_true(read_throughput >= 0.0)
  assert_true(write_throughput >= 0.0)
}

// Test 4: Network Performance Monitoring
test "network performance monitoring" {
  let network_monitor = NetworkMonitor::new()
  
  // Test network interface enumeration
  let interfaces = NetworkMonitor::get_interfaces(network_monitor)
  assert_true(interfaces.length() > 0)
  
  // Test network interface statistics
  for interface in interfaces {
    let stats = NetworkMonitor::get_interface_stats(network_monitor, interface)
    assert_true(InterfaceStats::bytes_received(stats) >= 0L)
    assert_true(InterfaceStats::bytes_sent(stats) >= 0L)
    assert_true(InterfaceStats::packets_received(stats) >= 0L)
    assert_true(InterfaceStats::packets_sent(stats) >= 0L)
  }
  
  // Test network connection monitoring
  let connections = NetworkMonitor::get_active_connections(network_monitor)
  assert_true(connections.length() >= 0)
  
  // Test network latency measurement
  let latency = NetworkMonitor::measure_latency(network_monitor, "8.8.8.8", 53)
  assert_true(latency >= 0.0)
  
  // Test bandwidth measurement
  let bandwidth = NetworkMonitor::measure_bandwidth(network_monitor, 1000L)
  assert_true(bandwidth >= 0.0)
}

// Test 5: Application Performance Monitoring
test "application performance monitoring" {
  let app_monitor = ApplicationMonitor::new()
  
  // Test request latency tracking
  let latency_tracker = LatencyTracker::new()
  LatencyTracker::record_request(latency_tracker, "/api/users", 150.5)
  LatencyTracker::record_request(latency_tracker, "/api/users", 200.3)
  LatencyTracker::record_request(latency_tracker, "/api/posts", 75.2)
  
  let user_api_stats = LatencyTracker::get_endpoint_stats(latency_tracker, "/api/users")
  assert_eq(EndpointStats::request_count(user_api_stats), 2)
  assert_true(EndpointStats::average_latency(user_api_stats) > 0.0)
  
  // Test throughput monitoring
  let throughput_monitor = ThroughputMonitor::new()
  ThroughputMonitor::record_requests(throughput_monitor, 100)
  let current_throughput = ThroughputMonitor::get_current_throughput(throughput_monitor)
  assert_true(current_throughput >= 0.0)
  
  // Test error rate monitoring
  let error_monitor = ErrorRateMonitor::new()
  ErrorRateMonitor::record_request(error_monitor, true) // Success
  ErrorRateMonitor::record_request(error_monitor, false) // Error
  ErrorRateMonitor::record_request(error_monitor, true)  // Success
  
  let error_rate = ErrorRateMonitor::get_error_rate(error_monitor)
  assert_eq(error_rate, 0.3333333333333333) // 1 error out of 3 requests
}

// Test 6: Performance Thresholds and Alerts
test "performance thresholds and alerts" {
  let alert_manager = PerformanceAlertManager::new()
  
  // Test CPU threshold alert
  let cpu_threshold = PerformanceThreshold::new(
    "cpu_usage",
    ThresholdType::Percentage,
    80.0,
    ComparisonOperator::GreaterThan
  )
  
  PerformanceAlertManager::add_threshold(alert_manager, cpu_threshold)
  
  // Test memory threshold alert
  let memory_threshold = PerformanceThreshold::new(
    "memory_usage",
    ThresholdType::Percentage,
    90.0,
    ComparisonOperator::GreaterThan
  )
  
  PerformanceAlertManager::add_threshold(alert_manager, memory_threshold)
  
  // Test threshold evaluation
  let metrics = [
    ("cpu_usage", 85.5),
    ("memory_usage", 75.2),
    ("disk_usage", 45.0)
  ]
  
  let alerts = PerformanceAlertManager::evaluate_thresholds(alert_manager, metrics)
  assert_eq(alerts.length(), 1) // Only CPU threshold exceeded
  
  // Test alert cooldown period
  let cooled_alerts = PerformanceAlertManager::apply_cooldown(alert_manager, alerts, 300000L) // 5 minutes
  assert_true(cooled_alerts.length() <= alerts.length())
}

// Test 7: Performance Profiling
test "performance profiling" {
  let profiler = PerformanceProfiler::new()
  
  // Test function execution profiling
  Profiler::start_profiling(profiler, "test_function")
  
  // Simulate some work
  let mut sum = 0
  for i = 0; i < 1000; i = i + 1 {
    sum = sum + i
  }
  
  Profiler::end_profiling(profiler, "test_function")
  
  let profile_data = Profiler::get_profile_data(profiler, "test_function")
  assert_true(ProfileData::execution_time(profile_data) > 0.0)
  assert_true(ProfileData::call_count(profile_data) >= 1)
  
  // Test memory profiling
  Profiler::start_memory_profiling(profiler)
  
  // Simulate memory allocation
  let large_array = [0; 10000]
  let processed_array = process_large_array(large_array)
  
  Profiler::end_memory_profiling(profiler)
  
  let memory_profile = Profiler::get_memory_profile(profiler)
  assert_true(MemoryProfile::peak_memory_usage(memory_profile) > 0)
  assert_true(MemoryProfile::allocation_count(memory_profile) > 0)
  
  // Test flame graph generation
  let flame_graph_data = Profiler::generate_flame_graph_data(profiler)
  assert_true(flame_graph_data.length() > 0)
}

// Test 8: Performance Baseline and Anomaly Detection
test "performance baseline and anomaly detection" {
  let baseline_manager = BaselineManager::new()
  
  // Test baseline establishment
  let metrics_samples = [
    [("cpu_usage", 25.5), ("memory_usage", 45.2), ("response_time", 120.3)],
    [("cpu_usage", 26.1), ("memory_usage", 44.8), ("response_time", 125.7)],
    [("cpu_usage", 24.9), ("memory_usage", 45.5), ("response_time", 118.9)],
    [("cpu_usage", 25.8), ("memory_usage", 45.1), ("response_time", 122.4)],
    [("cpu_usage", 25.3), ("memory_usage", 44.9), ("response_time", 121.1)]
  ]
  
  for sample in metrics_samples {
    BaselineManager::add_sample(baseline_manager, sample)
  }
  
  let baseline = BaselineManager::calculate_baseline(baseline_manager)
  assert_true(Baseline::contains_metric(baseline, "cpu_usage"))
  assert_true(Baseline::contains_metric(baseline, "memory_usage"))
  assert_true(Baseline::contains_metric(baseline, "response_time"))
  
  // Test anomaly detection
  let anomaly_detector = AnomalyDetector::new(2.0) // 2 standard deviations
  
  // Normal metrics (should not be anomalous)
  let normal_metrics = [("cpu_usage", 26.2), ("memory_usage", 45.3), ("response_time", 123.5)]
  let normal_anomalies = AnomalyDetector::detect_anomalies(anomaly_detector, normal_metrics, baseline)
  assert_eq(normal_anomalies.length(), 0)
  
  // Anomalous metrics (should be detected)
  let anomalous_metrics = [("cpu_usage", 85.7), ("memory_usage", 45.1), ("response_time", 124.2)]
  let detected_anomalies = AnomalyDetector::detect_anomalies(anomaly_detector, anomalous_metrics, baseline)
  assert_true(detected_anomalies.length() > 0)
  
  // Test trend analysis
  let trend_analyzer = TrendAnalyzer::new()
  let trend_data = [
    25.5, 26.1, 24.9, 25.8, 25.3, 26.5, 27.2, 28.1, 29.0, 30.5
  ]
  
  let trend = TrendAnalyzer::analyze_trend(trend_analyzer, trend_data)
  assert_eq(trend, TrendDirection::Increasing)
}

// Helper function for performance profiling test
fn process_large_array(array : Array[Int]) -> Array[Int] {
  let mut result = []
  for i in 0..array.length() {
    result = result @ [array[i] * 2]
  }
  result
}