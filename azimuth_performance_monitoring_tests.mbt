// Azimuth Telemetry System - Performance Monitoring Tests
// This file contains comprehensive test cases for performance monitoring functionality

// Test 1: CPU Performance Monitoring
test "cpu performance monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Start CPU monitoring
  let cpu_monitor = PerformanceMonitor::start_cpu_monitoring(monitor, 100) // 100ms interval
  
  // Simulate CPU-intensive work
  let start_time = Time::now()
  let mut result = 1
  
  for i in 1..=100000 {
    result = result * i
    if result > 1000000 {
      result = result / 1000
    }
  }
  
  let end_time = Time::now()
  let work_duration = end_time - start_time
  
  // Stop CPU monitoring
  let cpu_metrics = PerformanceMonitor::stop_cpu_monitoring(cpu_monitor)
  
  // Verify CPU metrics
  match cpu_metrics {
    CpuMetrics(usage_percentage, user_time, system_time, idle_time) => {
      assert_true(usage_percentage >= 0.0 && usage_percentage <= 100.0)
      assert_true(user_time > 0)
      assert_true(system_time >= 0)
      assert_true(idle_time >= 0)
    }
    _ => assert_true(false)
  }
  
  // Verify work duration is reasonable
  assert_true(work_duration > 0L)
  assert_true(work_duration < 10000L) // Less than 10 seconds
}

// Test 2: Memory Performance Monitoring
test "memory performance monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Start memory monitoring
  let memory_monitor = PerformanceMonitor::start_memory_monitoring(monitor, 100) // 100ms interval
  
  // Simulate memory-intensive work
  let large_arrays = []
  
  for i in 0..=10 {
    let large_array = []
    for j in 0..=10000 {
      large_array.push(j * i)
    }
    large_arrays.push(large_array)
  }
  
  // Process arrays
  let mut total_sum = 0
  for array in large_arrays {
    for value in array {
      total_sum = total_sum + value
    }
  }
  
  // Stop memory monitoring
  let memory_metrics = PerformanceMonitor::stop_memory_monitoring(memory_monitor)
  
  // Verify memory metrics
  match memory_metrics {
    MemoryMetrics(total_used, heap_used, stack_used, gc_count) => {
      assert_true(total_used > 0)
      assert_true(heap_used > 0)
      assert_true(stack_used >= 0)
      assert_true(gc_count >= 0)
    }
    _ => assert_true(false)
  }
  
  // Verify calculation result
  assert_true(total_sum > 0)
}

// Test 3: Response Time Monitoring
test "response time monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Create response time tracker
  let tracker = PerformanceMonitor::create_response_time_tracker(monitor, "api_operations")
  
  // Simulate operations with different response times
  let operations = [
    ("fast_operation", 10L),
    ("medium_operation", 100L),
    ("slow_operation", 500L),
    ("very_slow_operation", 1000L)
  ]
  
  for (operation_name, duration) in operations {
    let start_time = Time::now()
    
    // Simulate operation
    Time::sleep(duration)
    
    let end_time = Time::now()
    let actual_duration = end_time - start_time
    
    // Record response time
    ResponseTimeTracker::record(tracker, operation_name, actual_duration)
  }
  
  // Get response time statistics
  let stats = ResponseTimeTracker::get_statistics(tracker, "medium_operation")
  
  match stats {
    ResponseTimeStats(count, min, max, average, p50, p95, p99) => {
      assert_eq(count, 1)
      assert_eq(min, 100L)
      assert_eq(max, 100L)
      assert_eq(average, 100L)
      assert_eq(p50, 100L)
      assert_eq(p95, 100L)
      assert_eq(p99, 100L)
    }
    _ => assert_true(false)
  }
  
  // Get overall statistics
  let overall_stats = ResponseTimeTracker::get_overall_statistics(tracker)
  
  match overall_stats {
    OverallStats(total_operations, total_duration, average_duration) => {
      assert_eq(total_operations, 4)
      assert_eq(total_duration, 1610L) // 10 + 100 + 500 + 1000
      assert_eq(average_duration, 402.5) // 1610 / 4
    }
    _ => assert_true(false)
  }
}

// Test 4: Throughput Monitoring
test "throughput monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Create throughput tracker
  let tracker = PerformanceMonitor::create_throughput_tracker(monitor, "data_processing")
  
  // Simulate data processing
  let start_time = Time::now()
  let processed_items = 0
  
  for i in 0..=1000 {
    // Simulate processing an item
    Time::sleep(1L) // 1ms per item
    ThroughputTracker::record_item(tracker, "item_" + i.to_string())
    processed_items = processed_items + 1
  }
  
  let end_time = Time::now()
  let total_duration = (end_time - start_time) as Float / 1000.0 // Convert to seconds
  
  // Get throughput statistics
  let stats = ThroughputTracker::get_statistics(tracker)
  
  match stats {
    ThroughputStats(items_processed, duration_seconds, items_per_second) => {
      assert_eq(items_processed, 1001)
      assert_true(duration_seconds >= total_duration * 0.9 && duration_seconds <= total_duration * 1.1)
      assert_true(items_per_second > 0.0)
    }
    _ => assert_true(false)
  }
}

// Test 5: Error Rate Monitoring
test "error rate monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Create error rate tracker
  let tracker = PerformanceMonitor::create_error_rate_tracker(monitor, "api_errors")
  
  // Simulate operations with errors
  let operations = [
    ("success_operation", true),
    ("error_operation", false),
    ("success_operation", true),
    ("success_operation", true),
    ("error_operation", false),
    ("success_operation", true),
    ("error_operation", false),
    ("success_operation", true),
    ("success_operation", true),
    ("success_operation", true)
  ]
  
  for (operation_name, success) in operations {
    ErrorRateTracker::record_operation(tracker, operation_name, success)
  }
  
  // Get error rate statistics
  let stats = ErrorRateTracker::get_statistics(tracker, "error_operation")
  
  match stats {
    ErrorRateStats(total_operations, error_count, success_count, error_rate) => {
      assert_eq(total_operations, 3)
      assert_eq(error_count, 3)
      assert_eq(success_count, 0)
      assert_eq(error_rate, 1.0) // 100% error rate
    }
    _ => assert_true(false)
  }
  
  // Get overall statistics
  let overall_stats = ErrorRateTracker::get_overall_statistics(tracker)
  
  match overall_stats {
    OverallErrorRateStats(total_operations, total_errors, overall_error_rate) => {
      assert_eq(total_operations, 10)
      assert_eq(total_errors, 3)
      assert_eq(overall_error_rate, 0.3) // 30% error rate
    }
    _ => assert_true(false)
  }
}

// Test 6: Resource Utilization Monitoring
test "resource utilization monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Start resource monitoring
  let resource_monitor = PerformanceMonitor::start_resource_monitoring(monitor, 200) // 200ms interval
  
  // Simulate resource-intensive operations
  let threads = []
  
  for i in 0..=5 {
    let thread_data = {
      thread_id: i,
      work_items: 1000,
      results: []
    }
    threads.push(thread_data)
  }
  
  // Process work in each thread
  for thread in threads {
    for i in 0..=thread.work_items {
      // Simulate CPU and memory work
      let result = i * thread.thread_id
      thread.results.push(result)
    }
  }
  
  // Stop resource monitoring
  let resource_metrics = PerformanceMonitor::stop_resource_monitoring(resource_monitor)
  
  // Verify resource metrics
  match resource_metrics {
    ResourceMetrics(cpu_usage, memory_usage, disk_io, network_io, thread_count) => {
      assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
      assert_true(memory_usage > 0)
      assert_true(disk_io.read_bytes >= 0)
      assert_true(disk_io.write_bytes >= 0)
      assert_true(network_io.bytes_sent >= 0)
      assert_true(network_io.bytes_received >= 0)
      assert_true(thread_count >= 0)
    }
    _ => assert_true(false)
  }
}

// Test 7: Performance Threshold Alerting
test "performance threshold alerting" {
  let monitor = PerformanceMonitor::new()
  
  // Create alerting system
  let alerting = PerformanceMonitor::create_alerting_system(monitor)
  
  // Set performance thresholds
  AlertingSystem::set_response_time_threshold(alerting, "api_operation", 500L) // 500ms
  AlertingSystem::set_error_rate_threshold(alerting, "api_operation", 0.1) // 10%
  AlertingSystem::set_cpu_usage_threshold(alerting, 80.0) // 80%
  AlertingSystem::set_memory_usage_threshold(alerting, 90.0) // 90%
  
  // Simulate normal operations
  for i in 0..=5 {
    let start_time = Time::now()
    Time::sleep(100L) // 100ms operation
    let end_time = Time::now()
    
    AlertingSystem::record_operation(alerting, "api_operation", end_time - start_time, true)
  }
  
  // Check for alerts (should be none)
  let alerts1 = AlertingSystem::get_active_alerts(alerting)
  assert_eq(alerts1.length(), 0)
  
  // Simulate slow operations
  for i in 0..=3 {
    let start_time = Time::now()
    Time::sleep(600L) // 600ms operation (exceeds threshold)
    let end_time = Time::now()
    
    AlertingSystem::record_operation(alerting, "api_operation", end_time - start_time, true)
  }
  
  // Check for response time alerts
  let alerts2 = AlertingSystem::get_active_alerts(alerting)
  assert_true(alerts2.length() > 0)
  
  let response_time_alerts = alerts2.filter(|a| Alert::type(a) == ResponseTimeThreshold)
  assert_true(response_time_alerts.length() > 0)
  
  // Simulate errors
  for i in 0..=5 {
    let start_time = Time::now()
    Time::sleep(100L)
    let end_time = Time::now()
    
    AlertingSystem::record_operation(alerting, "api_operation", end_time - start_time, false)
  }
  
  // Check for error rate alerts
  let alerts3 = AlertingSystem::get_active_alerts(alerting)
  let error_rate_alerts = alerts3.filter(|a| Alert::type(a) == ErrorRateThreshold)
  assert_true(error_rate_alerts.length() > 0)
}

// Test 8: Performance Trend Analysis
test "performance trend analysis" {
  let monitor = PerformanceMonitor::new()
  
  // Create trend analyzer
  let analyzer = PerformanceMonitor::create_trend_analyzer(monitor, "performance_trends")
  
  // Simulate performance data over time
  let base_time = 1609459200L // 2021-01-01 00:00:00
  
  for day in 0..=30 {
    let timestamp = base_time + (day * 86400L) // Add days
    
    // Simulate daily performance metrics
    let response_time = 100L + (day * 2L) // Gradually increasing
    let error_rate = 0.05 + (day as Float * 0.001) // Gradually increasing
    let throughput = 1000.0 - (day as Float * 10.0) // Gradually decreasing
    
    TrendAnalyzer::add_data_point(analyzer, timestamp, response_time, error_rate, throughput)
  }
  
  // Analyze trends
  let response_time_trend = TrendAnalyzer::analyze_response_time_trend(analyzer)
  match response_time_trend {
    Trend(slope, correlation, direction) => {
      assert_true(slope > 0.0) // Increasing trend
      assert_true(correlation > 0.8) // Strong correlation
      assert_eq(direction, Increasing)
    }
    _ => assert_true(false)
  }
  
  let error_rate_trend = TrendAnalyzer::analyze_error_rate_trend(analyzer)
  match error_rate_trend {
    Trend(slope, correlation, direction) => {
      assert_true(slope > 0.0) // Increasing trend
      assert_true(correlation > 0.8) // Strong correlation
      assert_eq(direction, Increasing)
    }
    _ => assert_true(false)
  }
  
  let throughput_trend = TrendAnalyzer::analyze_throughput_trend(analyzer)
  match throughput_trend {
    Trend(slope, correlation, direction) => {
      assert_true(slope < 0.0) // Decreasing trend
      assert_true(correlation > 0.8) // Strong correlation
      assert_eq(direction, Decreasing)
    }
    _ => assert_true(false)
  }
  
  // Get trend summary
  let summary = TrendAnalyzer::get_trend_summary(analyzer)
  match summary {
    TrendSummary(response_trend, error_trend, throughput_trend, overall_health) => {
      assert_eq(response_trend, Deteriorating)
      assert_eq(error_trend, Deteriorating)
      assert_eq(throughput_trend, Deteriorating)
      assert_eq(overall_health, Poor)
    }
    _ => assert_true(false)
  }
}

// Test 9: Performance Profiling
test "performance profiling" {
  let monitor = PerformanceMonitor::new()
  
  // Create profiler
  let profiler = PerformanceMonitor::create_profiler(monitor, "function_profiler")
  
  // Start profiling
  Profiler::start(profiler)
  
  // Simulate functions with different performance characteristics
  let fast_function = || {
    let mut result = 0
    for i in 0..=1000 {
      result = result + i
    }
    result
  }
  
  let slow_function = || {
    let mut result = 1
    for i in 1..=10000 {
      result = result * i
      if result > 1000000 {
        result = result / 1000
      }
    }
    result
  }
  
  let memory_intensive_function = || {
    let arrays = []
    for i in 0..=10 {
      let array = []
      for j in 0..=5000 {
        array.push(i * j)
      }
      arrays.push(array)
    }
    arrays.length()
  }
  
  // Profile functions
  for i in 0..=10 {
    Profiler::profile_function(profiler, "fast_function", fast_function)
  }
  
  for i in 0..=5 {
    Profiler::profile_function(profiler, "slow_function", slow_function)
  }
  
  for i in 0..=3 {
    Profiler::profile_function(profiler, "memory_intensive_function", memory_intensive_function)
  }
  
  // Stop profiling
  let profile_results = Profiler::stop(profiler)
  
  // Analyze profile results
  let fast_stats = ProfileResults::get_function_stats(profile_results, "fast_function")
  match fast_stats {
    FunctionProfileStats(call_count, total_time, average_time, self_time) => {
      assert_eq(call_count, 11)
      assert_true(total_time > 0)
      assert_true(average_time > 0)
      assert_true(self_time > 0)
    }
    _ => assert_true(false)
  }
  
  let slow_stats = ProfileResults::get_function_stats(profile_results, "slow_function")
  match slow_stats {
    FunctionProfileStats(call_count, total_time, average_time, self_time) => {
      assert_eq(call_count, 6)
      assert_true(total_time > 0)
      assert_true(average_time > 0)
      assert_true(self_time > 0)
    }
    _ => assert_true(false)
  }
  
  // Compare function performance
  let comparison = ProfileResults::compare_functions(profile_results, "fast_function", "slow_function")
  match comparison {
    FunctionComparison(ratio, faster_function) => {
      assert_true(ratio > 1.0) // slow_function should be slower
      assert_eq(faster_function, "fast_function")
    }
    _ => assert_true(false)
  }
}

// Test 10: Performance Baseline Comparison
test "performance baseline comparison" {
  let monitor = PerformanceMonitor::new()
  
  // Create baseline comparator
  let comparator = PerformanceMonitor::create_baseline_comparator(monitor, "baseline_comparison")
  
  // Set performance baselines
  let baseline_metrics = PerformanceBaseline::new([
    ("api_response_time", BaselineValue::ResponseTime(100L)),
    ("error_rate", BaselineValue::ErrorRate(0.05)),
    ("throughput", BaselineValue::Throughput(1000.0)),
    ("cpu_usage", BaselineValue::CpuUsage(50.0)),
    ("memory_usage", BaselineValue::MemoryUsage(60.0))
  ])
  
  BaselineComparator::set_baseline(comparator, baseline_metrics)
  
  // Simulate current performance metrics
  let current_metrics = CurrentMetrics::new([
    ("api_response_time", CurrentValue::ResponseTime(120L)), // 20% slower
    ("error_rate", CurrentValue::ErrorRate(0.08)), // 60% higher
    ("throughput", CurrentValue::Throughput(900.0)), // 10% lower
    ("cpu_usage", CurrentValue::CpuUsage(65.0)), // 30% higher
    ("memory_usage", CurrentValue::MemoryUsage(55.0)) // 8% lower
  ])
  
  // Compare with baseline
  let comparison_results = BaselineComparator::compare_with_baseline(comparator, current_metrics)
  
  // Verify comparison results
  assert_eq(comparison_results.length(), 5)
  
  let response_time_result = comparison_results.find(|r| MetricComparison::metric_name(r) == "api_response_time")
  match response_time_result {
    Some(result) => {
      assert_eq(MetricComparison::deviation_percentage(result), 20.0)
      assert_eq(MetricComparison::status(result), Degraded)
    }
    None => assert_true(false)
  }
  
  let error_rate_result = comparison_results.find(|r| MetricComparison::metric_name(r) == "error_rate")
  match error_rate_result {
    Some(result) => {
      assert_eq(MetricComparison::deviation_percentage(result), 60.0)
      assert_eq(MetricComparison::status(result), Degraded)
    }
    None => assert_true(false)
  }
  
  let memory_usage_result = comparison_results.find(|r| MetricComparison::metric_name(r) == "memory_usage")
  match memory_usage_result {
    Some(result) => {
      assert_eq(MetricComparison::deviation_percentage(result), -8.33) // Negative means improvement
      assert_eq(MetricComparison::status(result), Improved)
    }
    None => assert_true(false)
  }
  
  // Get overall comparison summary
  let summary = BaselineComparator::get_comparison_summary(comparator)
  match summary {
    ComparisonSummary(total_metrics, improved_count, degraded_count, unchanged_count, overall_status) => {
      assert_eq(total_metrics, 5)
      assert_eq(improved_count, 1)
      assert_eq(degraded_count, 4)
      assert_eq(unchanged_count, 0)
      assert_eq(overall_status, Degraded)
    }
    _ => assert_true(false)
  }
}