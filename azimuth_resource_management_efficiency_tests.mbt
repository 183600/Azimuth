// Azimuth Telemetry System - Resource Management Efficiency Tests
// This file contains comprehensive test cases for resource management functionality

// Test 1: Memory Pool Management
test "memory pool management" {
  let memory_manager = MemoryPoolManager::new()
  
  // Test memory pool allocation and deallocation
  let pool = MemoryPoolManager::create_pool(memory_manager, 1000, 1024) // 1000 blocks of 1KB each
  
  let mut allocated_blocks = []
  
  // Allocate blocks
  for i in 0..=500 {
    let block = MemoryPool::allocate(pool)
    match block {
      Some(ptr) => allocated_blocks.push(ptr),
      None => assert_true(false) // Should allocate successfully
    }
  }
  
  assert_eq(allocated_blocks.length(), 500)
  
  // Verify pool statistics
  let stats = MemoryPool::get_stats(pool)
  assert_eq(stats.allocated_blocks, 500)
  assert_eq(stats.free_blocks, 500)
  assert_eq(stats.total_blocks, 1000)
  
  // Deallocate blocks
  for block in allocated_blocks {
    MemoryPool::deallocate(pool, block)
  }
  
  // Verify pool statistics after deallocation
  let deallocated_stats = MemoryPool::get_stats(pool)
  assert_eq(deallocated_stats.allocated_blocks, 0)
  assert_eq(deallocated_stats.free_blocks, 1000)
  assert_eq(deallocated_stats.total_blocks, 1000)
  
  // Test pool exhaustion
  let mut exhaustion_blocks = []
  
  for i in 0..=1000 {
    let block = MemoryPool::allocate(pool)
    match block {
      Some(ptr) => exhaustion_blocks.push(ptr),
      None => {} // Expected when pool is exhausted
    }
  }
  
  assert_eq(exhaustion_blocks.length(), 1000)
  
  // Should fail to allocate more blocks
  let exhausted_block = MemoryPool::allocate(pool)
  match exhausted_block {
    Some(_) => assert_true(false), // Should not allocate
    None => assert_true(true) // Expected when pool is exhausted
  }
  
  // Test pool reset
  MemoryPool::reset(pool)
  
  let reset_stats = MemoryPool::get_stats(pool)
  assert_eq(reset_stats.allocated_blocks, 0)
  assert_eq(reset_stats.free_blocks, 1000)
  assert_eq(reset_stats.total_blocks, 1000)
  
  // Test pool growth
  let growing_pool = MemoryPoolManager::create_growable_pool(memory_manager, 100, 1024)
  
  let mut growing_blocks = []
  
  for i in 0..=200 {
    let block = MemoryPool::allocate(growing_pool)
    match block {
      Some(ptr) => growing_blocks.push(ptr),
      None => assert_true(false) // Should allocate successfully
    }
  }
  
  // Pool should have grown to accommodate all blocks
  let growing_stats = MemoryPool::get_stats(growing_pool)
  assert_eq(growing_stats.allocated_blocks, 200)
  assert_true(growing_stats.total_blocks >= 200)
}

// Test 2: File Handle Management
test "file handle management" {
  let file_manager = FileManager::new()
  
  // Test file handle tracking
  let file1 = FileManager::open(file_manager, "test1.txt", "r")
  let file2 = FileManager::open(file_manager, "test2.txt", "w")
  let file3 = FileManager::open(file_manager, "test3.txt", "a")
  
  match (file1, file2, file3) {
    (Ok(f1), Ok(f2), Ok(f3)) => {
      // Verify file handles are tracked
      let open_handles = FileManager::get_open_handles(file_manager)
      assert_eq(open_handles.length(), 3)
      
      // Test file handle limits
      let max_handles = FileManager::get_max_handles(file_manager)
      FileManager::set_max_handles(file_manager, 5)
      
      // Open more files up to the limit
      let file4 = FileManager::open(file_manager, "test4.txt", "r")
      let file5 = FileManager::open(file_manager, "test5.txt", "r")
      
      match (file4, file5) {
        (Ok(f4), Ok(f5)) => {
          let handles_after_limit = FileManager::get_open_handles(file_manager)
          assert_eq(handles_after_limit.length(), 5)
          
          // Should fail to open more files beyond the limit
          let file6 = FileManager::open(file_manager, "test6.txt", "r")
          match file6 {
            Ok(_) => assert_true(false), // Should not open
            Error(_) => assert_true(true) // Expected when limit is reached
          }
          
          // Close some files
          FileManager::close(file_manager, f1)
          FileManager::close(file_manager, f2)
          
          let handles_after_close = FileManager::get_open_handles(file_manager)
          assert_eq(handles_after_close.length(), 3)
          
          // Should be able to open more files after closing
          let file6 = FileManager::open(file_manager, "test6.txt", "r")
          match file6 {
            Ok(_) => assert_true(true), // Should open now
            Error(_) => assert_true(false)
          }
          
          FileManager::close(file_manager, f3)
          FileManager::close(file_manager, f4)
          FileManager::close(file_manager, f5)
          match file6 {
            Ok(f6) => FileManager::close(file_manager, f6),
            Error(_) => {}
          }
        }
        _ => assert_true(false) // Should open successfully
      }
    }
    _ => assert_true(false) // Should open successfully
  }
  
  // Test automatic file handle cleanup
  FileManager::enable_auto_cleanup(file_manager, 1000) // Cleanup after 1 second
  
  let cleanup_file = FileManager::open(file_manager, "cleanup_test.txt", "r")
  match cleanup_file {
    Ok(f) => {
      // Don't close the file explicitly
      Time::sleep(1100) // Wait longer than cleanup timeout
      
      // File should be automatically closed
      let handles_after_cleanup = FileManager::get_open_handles(file_manager)
      assert_eq(handles_after_cleanup.length(), 0)
    }
    Error(_) => assert_true(false) // Should open successfully
  }
}

// Test 3: Database Connection Management
test "database connection management" {
  let db_manager = DatabaseConnectionManager::new()
  
  // Test connection pool creation
  let pool_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_min_connections(pool_config, 2)
  ConnectionPoolConfig::set_max_connections(pool_config, 10)
  ConnectionPoolConfig::set_connection_timeout(pool_config, 5000)
  ConnectionPoolConfig::set_idle_timeout(pool_config, 30000)
  
  let connection_pool = DatabaseConnectionManager::create_pool(db_manager, "postgresql://localhost/telemetry", pool_config)
  
  // Test connection acquisition
  let mut connections = []
  
  for i in 0..=5 {
    let conn = ConnectionPool::get_connection(connection_pool)
    match conn {
      Ok(c) => connections.push(c),
      Error(_) => assert_true(false) // Should get connection
    }
  }
  
  assert_eq(connections.length(), 6)
  
  // Verify pool statistics
  let pool_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(pool_stats.active_connections, 6)
  assert_true(pool_stats.total_connections >= 6)
  
  // Test connection release
  for conn in connections {
    ConnectionPool::release_connection(connection_pool, conn)
  }
  
  let release_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(release_stats.active_connections, 0)
  assert_true(release_stats.idle_connections >= 6)
  
  // Test connection timeout
  let timeout_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_min_connections(timeout_config, 1)
  ConnectionPoolConfig::set_max_connections(timeout_config, 1)
  ConnectionPoolConfig::set_connection_timeout(timeout_config, 100)
  
  let timeout_pool = DatabaseConnectionManager::create_pool(db_manager, "postgresql://localhost/telemetry", timeout_config)
  
  let conn1 = ConnectionPool::get_connection(timeout_pool)
  match conn1 {
    Ok(c) => {
      // Try to get another connection (should timeout)
      let conn2 = ConnectionPool::get_connection(timeout_pool)
      match conn2 {
        Ok(_) => assert_true(false), // Should not get connection
        Error(err) => {
          match err {
            ConnectionTimeout => assert_true(true),
            _ => assert_true(false)
          }
        }
      }
      
      ConnectionPool::release_connection(timeout_pool, c)
    }
    Error(_) => assert_true(false) // Should get connection
  }
  
  // Test connection validation
  let validation_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_min_connections(validation_config, 2)
  ConnectionPoolConfig::set_max_connections(validation_config, 5)
  ConnectionPoolConfig::set_validation_interval(validation_config, 10000)
  ConnectionPoolConfig::set_validation_query(validation_config, "SELECT 1")
  
  let validation_pool = DatabaseConnectionManager::create_pool(db_manager, "postgresql://localhost/telemetry", validation_config)
  
  let validation_conn = ConnectionPool::get_connection(validation_pool)
  match validation_conn {
    Ok(c) => {
      // Test connection validation
      let is_valid = ConnectionPool::validate_connection(validation_pool, c)
      assert_true(is_valid)
      
      ConnectionPool::release_connection(validation_pool, c)
    }
    Error(_) => assert_true(false) // Should get connection
  }
}

// Test 4: Thread Pool Management
test "thread pool management" {
  let thread_manager = ThreadPoolManager::new()
  
  // Test thread pool creation
  let thread_pool = ThreadPoolManager::create_pool(thread_manager, 4) // 4 worker threads
  
  // Test task submission
  let mut futures = []
  
  for i in 0..=10 {
    let future = ThreadPool::submit(thread_pool, func() {
      // Simulate work
      Time::sleep(100)
      return i * 2
    })
    futures.push(future)
  }
  
  // Test task completion
  let mut results = []
  
  for future in futures {
    let result = Future::await(future, 5000)
    match result {
      Some(value) => results.push(value),
      None => assert_true(false) // Should complete
    }
  }
  
  assert_eq(results.length(), 11)
  
  // Verify results
  for i in 0..=10 {
    assert_eq(results[i], i * 2)
  }
  
  // Test thread pool statistics
  let pool_stats = ThreadPool::get_stats(thread_pool)
  assert_eq(pool_stats.completed_tasks, 11)
  assert_true(pool_stats.active_threads <= 4)
  
  // Test thread pool shutdown
  ThreadPool::shutdown(thread_pool)
  
  let shutdown_stats = ThreadPool::get_stats(thread_pool)
  assert_eq(shutdown_stats.active_threads, 0)
  
  // Test task rejection after shutdown
  let shutdown_future = ThreadPool::submit(thread_pool, func() {
    return "should not execute"
  })
  
  match shutdown_future {
    Ok(_) => assert_true(false), // Should not submit
    Error(_) => assert_true(true) // Expected when pool is shutdown
  }
  
  // Test scheduled thread pool
  let scheduled_pool = ThreadPoolManager::create_scheduled_pool(thread_manager, 2)
  
  let scheduled_future = ScheduledThreadPool::schedule(scheduled_pool, func() {
    return "scheduled task"
  }, 1000) // Schedule after 1 second
  
  let scheduled_result = Future::await(scheduled_future, 2000)
  match scheduled_result {
    Some(value) => assert_eq(value, "scheduled task"),
    None => assert_true(false) // Should complete
  }
  
  // Test periodic task scheduling
  let counter = AtomicCounter::new(0)
  
  let periodic_future = ScheduledThreadPool::schedule_at_fixed_rate(scheduled_pool, func() {
    AtomicCounter::increment(counter)
  }, 500, 500) // Every 500ms
  
  Time::sleep(1600) // Wait for about 3 executions
  
  ScheduledThreadPool::cancel(scheduled_pool, periodic_future)
  
  let count = AtomicCounter::get(counter)
  assert_true(count >= 3 && count <= 4) // Should have executed 3-4 times
}

// Test 5: Network Socket Management
test "network socket management" {
  let socket_manager = SocketManager::new()
  
  // Test socket creation
  let server_socket = SocketManager::create_server_socket(socket_manager, "127.0.0.1", 0) // Random port
  
  match server_socket {
    Ok(server) => {
      let server_port = Socket::get_local_port(server)
      assert_true(server_port > 0)
      
      // Test client socket connection
      let client_socket = SocketManager::connect(socket_manager, "127.0.0.1", server_port)
      match client_socket {
        Ok(client) => {
          // Test socket tracking
          let active_sockets = SocketManager::get_active_sockets(socket_manager)
          assert_eq(active_sockets.length(), 2)
          
          // Test socket timeout
          Socket::set_read_timeout(client, 1000)
          Socket::set_write_timeout(client, 1000)
          
          // Test socket statistics
          let client_stats = Socket::get_stats(client)
          assert_eq(client_stats.bytes_sent, 0)
          assert_eq(client_stats.bytes_received, 0)
          
          // Test data transmission
          let test_data = "Hello, World!"
          let send_result = Socket::send(client, test_data)
          match send_result {
            Ok(bytes_sent) => assert_eq(bytes_sent, test_data.length()),
            Error(_) => assert_true(false) // Should send
          }
          
          let receive_buffer = ByteArray::new(1024)
          let receive_result = Socket::receive(client, receive_buffer)
          match receive_result {
            Ok(bytes_received) => assert_true(bytes_received > 0),
            Error(_) => assert_true(false) // Should receive
          }
          
          // Test socket closure
          Socket::close(client)
          Socket::close(server)
          
          let sockets_after_close = SocketManager::get_active_sockets(socket_manager)
          assert_eq(sockets_after_close.length(), 0)
        }
        Error(_) => assert_true(false) // Should connect
      }
    }
    Error(_) => assert_true(false) // Should create server socket
  }
  
  // Test socket pool
  let socket_pool = SocketManager::create_pool(socket_manager, 10)
  
  let mut pooled_sockets = []
  
  for i in 0..=5 {
    let socket = SocketPool::acquire(socket_pool)
    match socket {
      Ok(s) => pooled_sockets.push(s),
      Error(_) => assert_true(false) // Should acquire
    }
  }
  
  assert_eq(pooled_sockets.length(), 6)
  
  // Release sockets back to pool
  for socket in pooled_sockets {
    SocketPool::release(socket_pool, socket)
  }
  
  let pool_stats = SocketPool::get_stats(socket_pool)
  assert_eq(pool_stats.available_sockets, 6)
  assert_eq(pool_stats.active_sockets, 0)
  
  // Test socket pool cleanup
  SocketPool::cleanup(socket_pool)
  
  let cleanup_stats = SocketPool::get_stats(socket_pool)
  assert_eq(cleanup_stats.available_sockets, 0)
}

// Test 6: Resource Quota Management
test "resource quota management" {
  let quota_manager = ResourceQuotaManager::new()
  
  // Test CPU quota
  let cpu_quota = ResourceQuota::new("cpu", 80.0) // 80% CPU limit
  ResourceQuotaManager::set_quota(quota_manager, cpu_quota)
  
  let current_cpu = ResourceQuotaManager::get_current_usage(quota_manager, "cpu")
  assert_true(current_cpu < 80.0)
  
  // Test memory quota
  let memory_quota = ResourceQuota::new("memory", 1024 * 1024 * 1024) // 1GB limit
  ResourceQuotaManager::set_quota(quota_manager, memory_quota)
  
  let current_memory = ResourceQuotaManager::get_current_usage(quota_manager, "memory")
  assert_true(current_memory < 1024 * 1024 * 1024)
  
  // Test disk quota
  let disk_quota = ResourceQuota::new("disk", 10 * 1024 * 1024 * 1024) // 10GB limit
  ResourceQuotaManager::set_quota(quota_manager, disk_quota)
  
  let current_disk = ResourceQuotaManager::get_current_usage(quota_manager, "disk")
  assert_true(current_disk < 10 * 1024 * 1024 * 1024)
  
  // Test quota enforcement
  let enforcement_result = ResourceQuotaManager::check_quota(quota_manager, "memory", 500 * 1024 * 1024)
  match enforcement_result {
    Ok(_) => assert_true(true), // Should allow
    Error(_) => assert_true(false) // Should not deny
  }
  
  // Test quota violation
  let violation_result = ResourceQuotaManager::check_quota(quota_manager, "memory", 2 * 1024 * 1024 * 1024)
  match violation_result {
    Ok(_) => assert_true(false), // Should not allow
    Error(err) => {
      match err {
        QuotaExceeded => assert_true(true),
        _ => assert_true(false)
      }
    }
  }
  
  // Test quota warnings
  ResourceQuotaManager::set_warning_threshold(quota_manager, "memory", 0.8) // 80% warning
  
  // Simulate memory usage at 85%
  ResourceQuotaManager::simulate_usage(quota_manager, "memory", 1024 * 1024 * 1024 * 0.85)
  
  let warnings = ResourceQuotaManager::get_warnings(quota_manager)
  assert_true(warnings.length() > 0)
  
  // Test quota adjustment
  let new_memory_quota = ResourceQuota::new("memory", 2 * 1024 * 1024 * 1024) // 2GB limit
  ResourceQuotaManager::adjust_quota(quota_manager, new_memory_quota)
  
  let adjusted_quota = ResourceQuotaManager::get_quota(quota_manager, "memory")
  match adjusted_quota {
    Some(quota) => assert_eq(quota.limit, 2 * 1024 * 1024 * 1024),
    None => assert_true(false) // Should have quota
  }
}

// Test 7: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = ResourceLeakDetector::new()
  
  // Enable leak detection for different resource types
  LeakDetector::enable_detection(leak_detector, "memory")
  LeakDetector::enable_detection(leak_detector, "file")
  LeakDetector::enable_detection(leak_detector, "socket")
  
  // Simulate resource allocation without proper deallocation
  let leaked_memory = LeakDetector::allocate_memory(leak_detector, 1024 * 1024) // 1MB
  let leaked_file = LeakDetector::open_file(leak_detector, "leak_test.txt")
  let leaked_socket = LeakDetector::create_socket(leak_detector)
  
  // Don't deallocate resources to simulate leaks
  
  // Run leak detection
  let leak_report = LeakDetector::detect_leaks(leak_detector)
  
  assert_eq(leak_report.memory_leaks, 1)
  assert_eq(leak_report.file_leaks, 1)
  assert_eq(leak_report.socket_leaks, 1)
  assert_true(leak_report.total_leaked_memory > 0)
  
  // Test leak prevention
  LeakDetector::enable_auto_cleanup(leak_detector, 1000) // Auto cleanup after 1 second
  
  // Allocate more resources
  let auto_memory = LeakDetector::allocate_memory(leak_detector, 512 * 1024) // 512KB
  let auto_file = LeakDetector::open_file(leak_detector, "auto_test.txt")
  
  // Don't explicitly deallocate, but wait for auto cleanup
  Time::sleep(1100)
  
  // Run leak detection again
  let auto_leak_report = LeakDetector::detect_leaks(leak_detector)
  
  // Auto cleanup should have cleaned up the new resources
  assert_eq(auto_leak_report.memory_leaks, 1) // Still the original leak
  assert_eq(auto_leak_report.file_leaks, 1) // Still the original leak
  
  // Test resource tracking
  let resource_tracker = ResourceTracker::new()
  ResourceTracker::track_allocation(resource_tracker, "test_resource", 1024)
  
  let allocation_info = ResourceTracker::get_info(resource_tracker, "test_resource")
  match allocation_info {
    Some(info) => {
      assert_eq(info.size, 1024)
      assert_true(info.allocation_time > 0)
    }
    None => assert_true(false) // Should track allocation
  }
  
  ResourceTracker::track_deallocation(resource_tracker, "test_resource")
  
  let deallocation_info = ResourceTracker::get_info(resource_tracker, "test_resource")
  match deallocation_info {
    Some(info) => {
      assert_true(info.deallocated)
      assert_true(info.deallocation_time > 0)
    }
    None => assert_true(false) // Should track deallocation
  }
}

// Test 8: Resource Prioritization
test "resource prioritization" {
  let resource_prioritizer = ResourcePrioritizer::new()
  
  // Test priority levels
  ResourcePrioritizer::add_priority_level(resource_prioritizer, "critical", 10)
  ResourcePrioritizer::add_priority_level(resource_prioritizer, "high", 7)
  ResourcePrioritizer::add_priority_level(resource_prioritizer, "normal", 5)
  ResourcePrioritizer::add_priority_level(resource_prioritizer, "low", 3)
  ResourcePrioritizer::add_priority_level(resource_prioritizer, "background", 1)
  
  // Test resource allocation with priorities
  let critical_request = ResourceRequest::new("memory", 512 * 1024 * 1024, "critical") // 512MB
  let high_request = ResourceRequest::new("memory", 256 * 1024 * 1024, "high") // 256MB
  let normal_request = ResourceRequest::new("memory", 128 * 1024 * 1024, "normal") // 128MB
  let low_request = ResourceRequest::new("memory", 64 * 1024 * 1024, "low") // 64MB
  
  // Submit requests
  ResourcePrioritizer::submit_request(resource_prioritizer, critical_request)
  ResourcePrioritizer::submit_request(resource_prioritizer, high_request)
  ResourcePrioritizer::submit_request(resource_prioritizer, normal_request)
  ResourcePrioritizer::submit_request(resource_prioritizer, low_request)
  
  // Get prioritized requests
  let prioritized_requests = ResourcePrioritizer::get_prioritized_requests(resource_prioritizer)
  
  assert_eq(prioritized_requests.length(), 4)
  assert_eq(prioritized_requests[0].priority, "critical")
  assert_eq(prioritized_requests[1].priority, "high")
  assert_eq(prioritized_requests[2].priority, "normal")
  assert_eq(prioritized_requests[3].priority, "low")
  
  // Test resource preemption
  ResourcePrioritizer::set_total_resources(resource_prioritizer, "memory", 512 * 1024 * 1024) // 512MB total
  
  // Allocate critical request (should succeed)
  let critical_result = ResourcePrioritizer::allocate(resource_prioritizer, critical_request)
  match critical_result {
    Ok(_) => assert_true(true),
    Error(_) => assert_true(false) // Should allocate
  }
  
  // Try to allocate high request (should fail due to insufficient resources)
  let high_result = ResourcePrioritizer::allocate(resource_prioritizer, high_request)
  match high_result {
    Ok(_) => assert_true(false), // Should not allocate
    Error(_) => assert_true(true) // Expected due to insufficient resources
  }
  
  // Test resource reallocation
  ResourcePrioritizer::release_resources(resource_prioritizer, critical_request)
  
  // Now high request should succeed
  let high_retry = ResourcePrioritizer::allocate(resource_prioritizer, high_request)
  match high_retry {
    Ok(_) => assert_true(true),
    Error(_) => assert_true(false) // Should allocate
  }
  
  // Test priority adjustment
  ResourcePrioritizer::adjust_priority(resource_prioritizer, normal_request, "high")
  
  let adjusted_requests = ResourcePrioritizer::get_prioritized_requests(resource_prioritizer)
  
  // Normal request should now be treated as high priority
  assert_eq(adjusted_requests[0].priority, "high") // Original high request
  assert_eq(adjusted_requests[1].priority, "high") // Adjusted normal request
}

// Test 9: Resource Monitoring and Metrics
test "resource monitoring and metrics" {
  let resource_monitor = ResourceMonitor::new()
  
  // Enable monitoring for different resources
  ResourceMonitor::enable_monitoring(resource_monitor, "cpu")
  ResourceMonitor::enable_monitoring(resource_monitor, "memory")
  ResourceMonitor::enable_monitoring(resource_monitor, "disk")
  ResourceMonitor::enable_monitoring(resource_monitor, "network")
  
  // Start monitoring
  ResourceMonitor::start(resource_monitor)
  
  // Simulate resource usage
  Time::sleep(2000) // Let monitoring run for 2 seconds
  
  // Get resource metrics
  let cpu_metrics = ResourceMonitor::get_metrics(resource_monitor, "cpu")
  let memory_metrics = ResourceMonitor::get_metrics(resource_monitor, "memory")
  let disk_metrics = ResourceMonitor::get_metrics(resource_monitor, "disk")
  let network_metrics = ResourceMonitor::get_metrics(resource_monitor, "network")
  
  // Verify metrics are collected
  assert_true(cpu_metrics.current_usage > 0)
  assert_true(cpu_metrics.average_usage > 0)
  assert_true(cpu_metrics.peak_usage > 0)
  
  assert_true(memory_metrics.current_usage > 0)
  assert_true(memory_metrics.average_usage > 0)
  assert_true(memory_metrics.peak_usage > 0)
  
  assert_true(disk_metrics.current_usage > 0)
  assert_true(network_metrics.current_usage >= 0)
  
  // Test metric history
  let cpu_history = ResourceMonitor::get_metric_history(resource_monitor, "cpu", 1000)
  assert_true(cpu_history.length() > 0)
  
  // Test resource alerts
  ResourceMonitor::set_alert_threshold(resource_monitor, "cpu", 90.0) // Alert at 90% CPU
  
  // Simulate high CPU usage
  ResourceMonitor::simulate_usage(resource_monitor, "cpu", 95.0)
  
  let alerts = ResourceMonitor::get_alerts(resource_monitor)
  assert_true(alerts.length() > 0)
  
  // Test resource predictions
  let cpu_prediction = ResourceMonitor::predict_usage(resource_monitor, "cpu", 3600) // Predict 1 hour ahead
  match cpu_prediction {
    Some(prediction) => {
      assert_true(prediction.predicted_usage > 0)
      assert_true(prediction.confidence > 0.0)
      assert_true(prediction.confidence <= 1.0)
    }
    None => assert_true(false) // Should make prediction
  }
  
  // Stop monitoring
  ResourceMonitor::stop(resource_monitor)
  
  // Verify monitoring is stopped
  let stopped_metrics = ResourceMonitor::get_metrics(resource_monitor, "cpu")
  assert_eq(stopped_metrics.monitoring_active, false)
}

// Test 10: Resource Optimization
test "resource optimization" {
  let resource_optimizer = ResourceOptimizer::new()
  
  // Test memory optimization
  let memory_optimizer = MemoryOptimizer::new()
  MemoryOptimizer::enable_gc_optimization(memory_optimizer, true)
  MemoryOptimizer::enable_pool_optimization(memory_optimizer, true)
  MemoryOptimizer::enable_compression_optimization(memory_optimizer, true)
  
  // Create memory pressure
  let large_data = Array::new(1000000) // Large array
  for i in 0..=1000000 {
    large_data.push(i)
  }
  
  let before_optimization = MemoryStats::current_usage()
  
  // Run memory optimization
  let optimization_result = ResourceOptimizer::optimize_memory(resource_optimizer, memory_optimizer)
  match optimization_result {
    Ok(report) => {
      assert_true(report.memory_freed > 0)
      assert_true(report.optimization_time > 0)
    }
    Error(_) => assert_true(false) // Should optimize
  }
  
  let after_optimization = MemoryStats::current_usage()
  
  // Memory usage should be reduced
  assert_true(after_optimization < before_optimization)
  
  // Test CPU optimization
  let cpu_optimizer = CPUOptimizer::new()
  CPUOptimizer::enable_thread_optimization(cpu_optimizer, true)
  CPUOptimizer::enable_priority_optimization(cpu_optimizer, true)
  
  // Create CPU load
  let mut handles = []
  for i in 0..=10 {
    let handle = Thread::spawn(func() {
      // CPU-intensive task
      let mut result = 0
      for j in 0..=1000000 {
        result = result + j
      }
      return result
    })
    handles.push(handle)
  }
  
  let before_cpu_optimization = CPUStats::current_usage()
  
  // Run CPU optimization
  let cpu_optimization_result = ResourceOptimizer::optimize_cpu(resource_optimizer, cpu_optimizer)
  match cpu_optimization_result {
    Ok(report) => {
      assert_true(report.cpu_time_saved > 0)
      assert_true(report.optimization_time > 0)
    }
    Error(_) => assert_true(false) // Should optimize
  }
  
  let after_cpu_optimization = CPUStats::current_usage()
  
  // Wait for threads to complete
  for handle in handles {
    Thread::join(handle)
  }
  
  // Test I/O optimization
  let io_optimizer = IOOptimizer::new()
  IOOptimizer::enable_buffering_optimization(io_optimizer, true)
  IOOptimizer::enable_batching_optimization(io_optimizer, true)
  IOOptimizer::enable_caching_optimization(io_optimizer, true)
  
  // Create I/O operations
  let io_start_time = Time::now()
  
  for i in 0..=100 {
    let file = File::open("io_test_" + i.to_string() + ".txt", "w")
    match file {
      Ok(f) => {
        File::write(f, "Test data " + i.to_string())
        File::close(f)
      }
      Error(_) => assert_true(false) // Should open and write
    }
  }
  
  let before_io_optimization = Time::duration(io_start_time, Time::now())
  
  // Run I/O optimization
  let io_optimization_result = ResourceOptimizer::optimize_io(resource_optimizer, io_optimizer)
  match io_optimization_result {
    Ok(report) => {
      assert_true(report.io_time_saved > 0)
      assert_true(report.optimization_time > 0)
    }
    Error(_) => assert_true(false) // Should optimize
  }
  
  // Test with optimized I/O
  let optimized_io_start = Time::now()
  
  for i in 0..=100 {
    let file = File::open("io_optimized_test_" + i.to_string() + ".txt", "w")
    match file {
      Ok(f) => {
        File::write(f, "Test data " + i.to_string())
        File::close(f)
      }
      Error(_) => assert_true(false) // Should open and write
    }
  }
  
  let after_io_optimization = Time::duration(optimized_io_start, Time::now())
  
  // Optimized I/O should be faster
  assert_true(after_io_optimization < before_io_optimization)
  
  // Test comprehensive resource optimization
  let comprehensive_result = ResourceOptimizer::optimize_all(resource_optimizer)
  match comprehensive_result {
    Ok(report) => {
      assert_true(report.memory_optimized)
      assert_true(report.cpu_optimized)
      assert_true(report.io_optimized)
      assert_true(report.total_time_saved > 0)
    }
    Error(_) => assert_true(false) // Should optimize
  }
}