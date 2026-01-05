// Azimuth Telemetry System - Performance Monitoring Tests
// This file contains test cases for performance monitoring functionality

// Test 1: Performance Metrics Collection
test "performance metrics collection" {
  let monitor = PerformanceMonitor::new()
  
  // Start monitoring
  PerformanceMonitor::start(monitor)
  
  // Simulate some operations with different performance characteristics
  let start_time = Time::now()
  
  // Fast operation
  PerformanceMonitor::begin_operation(monitor, "fast_operation")
  Time::sleep(10) // 10ms
  PerformanceMonitor::end_operation(monitor, "fast_operation")
  
  // Medium operation
  PerformanceMonitor::begin_operation(monitor, "medium_operation")
  Time::sleep(50) // 50ms
  PerformanceMonitor::end_operation(monitor, "medium_operation")
  
  // Slow operation
  PerformanceMonitor::begin_operation(monitor, "slow_operation")
  Time::sleep(100) // 100ms
  PerformanceMonitor::end_operation(monitor, "slow_operation")
  
  let end_time = Time::now()
  
  // Stop monitoring
  PerformanceMonitor::stop(monitor)
  
  // Verify metrics were collected
  let fast_metrics = PerformanceMonitor::get_metrics(monitor, "fast_operation")
  match fast_metrics {
    Some(metrics) => {
      assert_eq(metrics.count, 1)
      assert_true(metrics.total_time >= 10.0)
      assert_eq(metrics.average_time, metrics.total_time)
    }
    None => assert_true(false)
  }
  
  let medium_metrics = PerformanceMonitor::get_metrics(monitor, "medium_operation")
  match medium_metrics {
    Some(metrics) => {
      assert_eq(metrics.count, 1)
      assert_true(metrics.total_time >= 50.0)
      assert_eq(metrics.average_time, metrics.total_time)
    }
    None => assert_true(false)
  }
  
  let slow_metrics = PerformanceMonitor::get_metrics(monitor, "slow_operation")
  match slow_metrics {
    Some(metrics) => {
      assert_eq(metrics.count, 1)
      assert_true(metrics.total_time >= 100.0)
      assert_eq(metrics.average_time, metrics.total_time)
    }
    None => assert_true(false)
  }
}

// Test 2: Performance Threshold Monitoring
test "performance threshold monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Configure performance thresholds
  PerformanceMonitor::set_threshold(monitor, "api_response_time", 100.0) // 100ms threshold
  PerformanceMonitor::set_threshold(monitor, "database_query_time", 50.0) // 50ms threshold
  
  // Start monitoring
  PerformanceMonitor::start(monitor)
  
  // Normal operation (within threshold)
  PerformanceMonitor::begin_operation(monitor, "api_response_time")
  Time::sleep(80) // 80ms, within threshold
  PerformanceMonitor::end_operation(monitor, "api_response_time")
  
  // Slow operation (exceeds threshold)
  PerformanceMonitor::begin_operation(monitor, "database_query_time")
  Time::sleep(75) // 75ms, exceeds threshold
  PerformanceMonitor::end_operation(monitor, "database_query_time")
  
  // Stop monitoring
  PerformanceMonitor::stop(monitor)
  
  // Check for threshold violations
  let violations = PerformanceMonitor::get_threshold_violations(monitor)
  assert_eq(violations.length(), 1)
  
  match violations[0] {
    ThresholdViolation { operation: "database_query_time", threshold: 50.0, actual_time: time } => {
      assert_true(time >= 75.0)
    }
    _ => assert_true(false)
  }
  
  // Verify normal operation doesn't generate violation
  let api_metrics = PerformanceMonitor::get_metrics(monitor, "api_response_time")
  match api_metrics {
    Some(metrics) => {
      assert_eq(metrics.violations, 0)
    }
    None => assert_true(false)
  }
  
  // Verify slow operation generates violation
  let db_metrics = PerformanceMonitor::get_metrics(monitor, "database_query_time")
  match db_metrics {
    Some(metrics) => {
      assert_eq(metrics.violations, 1)
    }
    None => assert_true(false)
  }
}

// Test 3: Resource Utilization Monitoring
test "resource utilization monitoring" {
  let monitor = PerformanceMonitor::new()
  
  // Start resource monitoring
  PerformanceMonitor::start_resource_monitoring(monitor)
  
  // Simulate resource usage
  let initial_memory = PerformanceMonitor::get_memory_usage(monitor)
  let initial_cpu = PerformanceMonitor::get_cpu_usage(monitor)
  
  // Allocate memory
  let large_array = Array::create(10000, 0)
  for i in 0..=9999 {
    large_array[i] = i
  }
  
  // Perform CPU-intensive operation
  let mut result = 0
  for i in 0..=100000 {
    result = result + (i * i) % 1000
  }
  
  // Get updated resource usage
  let updated_memory = PerformanceMonitor::get_memory_usage(monitor)
  let updated_cpu = PerformanceMonitor::get_cpu_usage(monitor)
  
  // Stop resource monitoring
  PerformanceMonitor::stop_resource_monitoring(monitor)
  
  // Verify resource usage increased
  assert_true(updated_memory > initial_memory)
  
  // CPU usage might vary, but should have some activity
  assert_true(updated_cpu >= 0.0 && updated_cpu <= 100.0)
  
  // Get resource utilization history
  let memory_history = PerformanceMonitor::get_memory_history(monitor)
  let cpu_history = PerformanceMonitor::get_cpu_history(monitor)
  
  assert_true(memory_history.length() >= 2)
  assert_true(cpu_history.length() >= 2)
  
  // Verify history contains initial and updated values
  assert_eq(memory_history[0], initial_memory)
  assert_eq(memory_history[memory_history.length() - 1], updated_memory)
}