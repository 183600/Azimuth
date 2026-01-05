// Azimuth Telemetry System - Resource Management Tests
// This file contains comprehensive test cases for resource management functionality

// Test 1: Memory Management Tests
test "memory management operations" {
  // Test memory allocation and deallocation
  let memory_manager = MemoryManager::new()
  
  // Allocate memory blocks
  let block1 = MemoryManager::allocate(memory_manager, 1024)  // 1KB
  let block2 = MemoryManager::allocate(memory_manager, 2048)  // 2KB
  let block3 = MemoryManager::allocate(memory_manager, 4096)  // 4KB
  
  assert_eq(MemoryManager::get_block_size(memory_manager, block1), 1024)
  assert_eq(MemoryManager::get_block_size(memory_manager, block2), 2048)
  assert_eq(MemoryManager::get_block_size(memory_manager, block3), 4096)
  
  // Check total allocated memory
  assert_eq(MemoryManager::get_allocated_memory(memory_manager), 7168)  // 1024 + 2048 + 4096
  
  // Deallocate memory blocks
  MemoryManager::deallocate(memory_manager, block2)
  assert_eq(MemoryManager::get_allocated_memory(memory_manager), 5120)  // 1024 + 4096
  
  MemoryManager::deallocate(memory_manager, block1)
  MemoryManager::deallocate(memory_manager, block3)
  assert_eq(MemoryManager::get_allocated_memory(memory_manager), 0)
  
  // Test memory pool
  let memory_pool = MemoryPool::new(1024 * 1024)  // 1MB pool
  
  // Allocate from pool
  let pool_block1 = MemoryPool::allocate(memory_pool, 1024)
  let pool_block2 = MemoryPool::allocate(memory_pool, 2048)
  let pool_block3 = MemoryPool::allocate(memory_pool, 4096)
  
  assert_eq(MemoryPool::get_used_memory(memory_pool), 7168)
  
  // Return blocks to pool
  MemoryPool::deallocate(memory_pool, pool_block1)
  MemoryPool::deallocate(memory_pool, pool_block2)
  MemoryPool::deallocate(memory_pool, pool_block3)
  
  assert_eq(MemoryPool::get_used_memory(memory_pool), 0)
  
  // Test garbage collection
  let gc_manager = GCManager::new()
  
  // Create objects that will be garbage collected
  let objects = []
  for i in 0..100 {
    let obj = GCObject::new("object_" + i.to_string())
    ArrayUtil::push(objects, obj)
  }
  
  assert_eq(GCManager::get_object_count(gc_manager), 100)
  
  // Clear references to objects
  objects = []
  
  // Run garbage collection
  GCManager::collect(gc_manager)
  
  assert_eq(GCManager::get_object_count(gc_manager), 0)
  
  // Test memory leak detection
  let leak_detector = LeakDetector::new()
  
  // Simulate memory leak
  LeakDetector::start_tracking(leak_detector)
  
  let leaked_objects = []
  for i in 0..50 {
    let obj = LeakObject::new("leaked_" + i.to_string())
    ArrayUtil::push(leaked_objects, obj)
  }
  
  // Clear references but don't deallocate
  leaked_objects = []
  
  // Detect leaks
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 50)
  
  // Clean up
  LeakDetector::cleanup_leaks(leak_detector)
  LeakDetector::stop_tracking(leak_detector)
}

// Test 2: File Handle Management Tests
test "file handle management operations" {
  // Test file handle allocation and deallocation
  let file_manager = FileManager::new()
  
  // Open file handles
  let file1 = FileManager::open(file_manager, "/tmp/test1.txt")
  let file2 = FileManager::open(file_manager, "/tmp/test2.txt")
  let file3 = FileManager::open(file_manager, "/tmp/test3.txt")
  
  assert_eq(FileManager::get_open_handle_count(file_manager), 3)
  
  // Write to files
  FileManager::write(file_manager, file1, "Content for file 1")
  FileManager::write(file_manager, file2, "Content for file 2")
  FileManager::write(file_manager, file3, "Content for file 3")
  
  // Read from files
  let content1 = FileManager::read(file_manager, file1)
  let content2 = FileManager::read(file_manager, file2)
  let content3 = FileManager::read(file_manager, file3)
  
  assert_eq(content1, "Content for file 1")
  assert_eq(content2, "Content for file 2")
  assert_eq(content3, "Content for file 3")
  
  // Close file handles
  FileManager::close(file_manager, file1)
  FileManager::close(file_manager, file2)
  FileManager::close(file_manager, file3)
  
  assert_eq(FileManager::get_open_handle_count(file_manager), 0)
  
  // Test automatic file handle cleanup
  let auto_file_manager = AutoFileManager::new()
  
  // Open files with automatic cleanup
  let auto_file1 = AutoFileManager::open(auto_file_manager, "/tmp/auto1.txt")
  let auto_file2 = AutoFileManager::open(auto_file_manager, "/tmp/auto2.txt")
  
  assert_eq(AutoFileManager::get_open_handle_count(auto_file_manager), 2)
  
  // Simulate going out of scope
  AutoFileManager::cleanup_all(auto_file_manager)
  
  assert_eq(AutoFileManager::get_open_handle_count(auto_file_manager), 0)
  
  // Test file handle pooling
  let file_pool = FilePool::new(10)  // Pool of 10 handles
  
  // Acquire handles from pool
  let pooled_file1 = FilePool::acquire(file_pool, "/tmp/pooled1.txt")
  let pooled_file2 = FilePool::acquire(file_pool, "/tmp/pooled2.txt")
  
  assert_eq(FilePool::get_available_handles(file_pool), 8)
  
  // Return handles to pool
  FilePool::release(file_pool, pooled_file1)
  FilePool::release(file_pool, pooled_file2)
  
  assert_eq(FilePool::get_available_handles(file_pool), 10)
  
  // Test file handle leak detection
  let handle_leak_detector = HandleLeakDetector::new()
  
  HandleLeakDetector::start_tracking(handle_leak_detector)
  
  // Simulate handle leak
  let leaked_handle = FileManager::open(file_manager, "/tmp/leaked.txt")
  
  // Don't close handle
  let leaks = HandleLeakDetector::detect_leaks(handle_leak_detector)
  assert_eq(leaks.length(), 1)
  
  // Clean up
  FileManager::close(file_manager, leaked_handle)
  HandleLeakDetector::cleanup_leaks(handle_leak_detector)
  HandleLeakDetector::stop_tracking(handle_leak_detector)
}

// Test 3: Database Connection Management Tests
test "database connection management operations" {
  // Test connection pool
  let connection_pool = ConnectionPool::new("test_db_url", 5)  // Max 5 connections
  
  // Acquire connections
  let conn1 = ConnectionPool::acquire(connection_pool)
  let conn2 = ConnectionPool::acquire(connection_pool)
  let conn3 = ConnectionPool::acquire(connection_pool)
  
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 3)
  assert_eq(ConnectionPool::get_available_connections(connection_pool), 2)
  
  // Use connections
  let result1 = DatabaseUtil::execute_query(conn1, "SELECT 1")
  let result2 = DatabaseUtil::execute_query(conn2, "SELECT 2")
  let result3 = DatabaseUtil::execute_query(conn3, "SELECT 3")
  
  assert_eq(result1[0]["value"], "1")
  assert_eq(result2[0]["value"], "2")
  assert_eq(result3[0]["value"], "3")
  
  // Release connections
  ConnectionPool::release(connection_pool, conn1)
  ConnectionPool::release(connection_pool, conn2)
  ConnectionPool::release(connection_pool, conn3)
  
  assert_eq(ConnectionPool::get_active_connections(connection_pool), 0)
  assert_eq(ConnectionPool::get_available_connections(connection_pool), 5)
  
  // Test connection timeout
  let timeout_pool = ConnectionPool::new("test_db_url", 2)  // Max 2 connections
  
  // Acquire all connections
  let timeout_conn1 = ConnectionPool::acquire(timeout_pool)
  let timeout_conn2 = ConnectionPool::acquire(timeout_pool)
  
  // Try to acquire with timeout
  let start_time = TimeUtil::current_time_millis()
  let timeout_result = ConnectionPool::try_acquire_for(timeout_pool, 100)  // 100ms timeout
  let elapsed = TimeUtil::current_time_millis() - start_time
  
  assert_false(timeout_result.success)
  assert_true(elapsed >= 90 && elapsed <= 200)
  
  // Release one connection and try again
  ConnectionPool::release(timeout_pool, timeout_conn1)
  
  let retry_result = ConnectionPool::try_acquire_for(timeout_pool, 100)
  assert_true(retry_result.success)
  
  ConnectionPool::release(timeout_pool, timeout_conn2)
  ConnectionPool::release(timeout_pool, retry_result.connection)
  
  // Test connection health checking
  let health_pool = ConnectionPool::new("test_db_url", 3)
  
  // Acquire connection
  let health_conn = ConnectionPool::acquire(health_pool)
  
  // Simulate connection failure
  DatabaseUtil::simulate_failure(health_conn)
  
  // Return unhealthy connection
  ConnectionPool::release(health_pool, health_conn)
  
  // Health check should detect and replace unhealthy connection
  ConnectionPool::health_check(health_pool)
  
  assert_eq(ConnectionPool::get_active_connections(health_pool), 0)
  assert_eq(ConnectionPool::get_available_connections(health_pool), 3)
  
  // Test connection validation
  let validation_pool = ConnectionPool::new("test_db_url", 2)
  
  let validation_conn = ConnectionPool::acquire(validation_pool)
  
  // Validate connection
  let is_valid = ConnectionPool::validate_connection(validation_pool, validation_conn)
  assert_true(is_valid)
  
  // Simulate connection loss
  DatabaseUtil::simulate_failure(validation_conn)
  
  // Validate again
  let is_invalid = ConnectionPool::validate_connection(validation_pool, validation_conn)
  assert_false(is_invalid)
  
  ConnectionPool::release(validation_pool, validation_conn)
}

// Test 4: Thread Resource Management Tests
test "thread resource management operations" {
  // Test thread pool management
  let thread_pool = ThreadPool::new(4)  // 4 threads in pool
  
  assert_eq(ThreadPool::get_active_threads(thread_pool), 0)
  assert_eq(ThreadPool::get_available_threads(thread_pool), 4)
  
  // Submit tasks
  let futures = []
  for i in 0..10 {
    let future = ThreadPool::submit(thread_pool, () -> {
      TimeUtil::sleep(100)  // Simulate work
      return "Task_" + i.to_string() + "_completed"
    })
    ArrayUtil::push(futures, future)
  }
  
  assert_eq(ThreadPool::get_active_threads(thread_pool), 4)
  assert_eq(ThreadPool::get_available_threads(thread_pool), 0)
  
  // Wait for tasks to complete
  let results = []
  for future in futures {
    let result = Future::get(future)
    ArrayUtil::push(results, result)
  }
  
  assert_eq(results.length(), 10)
  assert_eq(ThreadPool::get_active_threads(thread_pool), 0)
  assert_eq(ThreadPool::get_available_threads(thread_pool), 4)
  
  // Test thread lifecycle management
  let thread_manager = ThreadManager::new()
  
  // Create managed thread
  let managed_thread = ThreadManager::create(thread_manager, () -> {
    TimeUtil::sleep(200)
    return "Managed thread result"
  })
  
  assert_eq(ThreadManager::get_active_thread_count(thread_manager), 1)
  
  // Wait for thread to complete
  let managed_result = ThreadManager::join(thread_manager, managed_thread)
  assert_eq(managed_result, "Managed thread result")
  
  assert_eq(ThreadManager::get_active_thread_count(thread_manager), 0)
  
  // Test thread resource cleanup
  let cleanup_manager = ThreadCleanupManager::new()
  
  // Create threads
  let cleanup_threads = []
  for i in 0..5 {
    let thread = ThreadCleanupManager::create(cleanup_manager, () -> {
      TimeUtil::sleep(100)
      return "Cleanup thread " + i.to_string()
    })
    ArrayUtil::push(cleanup_threads, thread)
  }
  
  assert_eq(ThreadCleanupManager::get_active_thread_count(cleanup_manager), 5)
  
  // Force cleanup all threads
  ThreadCleanupManager::cleanup_all(cleanup_manager)
  
  assert_eq(ThreadCleanupManager::get_active_thread_count(cleanup_manager), 0)
  
  // Test thread resource limits
  let limited_pool = ThreadPool::new(2)  // Max 2 threads
  
  // Submit more tasks than threads
  let limited_futures = []
  for i in 0..10 {
    let future = ThreadPool::submit(limited_pool, () -> {
      TimeUtil::sleep(50)
      return "Limited task " + i.to_string()
    })
    ArrayUtil::push(limited_futures, future)
  }
  
  // Should still complete all tasks, but with queueing
  let limited_results = []
  for future in limited_futures {
    let result = Future::get(future)
    ArrayUtil::push(limited_results, result)
  }
  
  assert_eq(limited_results.length(), 10)
}

// Test 5: Network Socket Management Tests
test "network socket management operations" {
  // Test socket pool
  let socket_pool = SocketPool::new(5)  // Max 5 sockets
  
  // Acquire sockets
  let socket1 = SocketPool::acquire(socket_pool, "tcp", "example.com", 80)
  let socket2 = SocketPool::acquire(socket_pool, "tcp", "example.com", 443)
  let socket3 = SocketPool::acquire(socket_pool, "udp", "example.com", 53)
  
  assert_eq(SocketPool::get_active_sockets(socket_pool), 3)
  assert_eq(SocketPool::get_available_sockets(socket_pool), 2)
  
  // Use sockets
  SocketPool::send(socket_pool, socket1, "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
  let response1 = SocketPool::receive(socket_pool)
  
  SocketPool::send(socket_pool, socket2, "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
  let response2 = SocketPool::receive(socket_pool)
  
  SocketPool::send(socket_pool, socket3, "DNS query")
  let response3 = SocketPool::receive(socket_pool)
  
  assert_true(response1.contains("HTTP"))
  assert_true(response2.contains("HTTP"))
  assert_true(response3.length() > 0)
  
  // Release sockets
  SocketPool::release(socket_pool, socket1)
  SocketPool::release(socket_pool, socket2)
  SocketPool::release(socket_pool, socket3)
  
  assert_eq(SocketPool::get_active_sockets(socket_pool), 0)
  assert_eq(SocketPool::get_available_sockets(socket_pool), 5)
  
  // Test socket timeout
  let timeout_socket = SocketPool::acquire_with_timeout(socket_pool, "tcp", "slow.example.com", 80, 1000)  // 1s timeout
  
  match timeout_socket {
    Some(socket) => {
      SocketPool::release(socket_pool, socket)
    }
    None => {
      // Timeout occurred
    }
  }
  
  // Test socket health checking
  let health_socket = SocketPool::acquire(socket_pool, "tcp", "example.com", 80)
  
  // Simulate socket failure
  SocketPool::simulate_failure(socket_pool, health_socket)
  
  // Health check should detect failure
  let is_healthy = SocketPool::health_check(socket_pool, health_socket)
  assert_false(is_healthy)
  
  // Release unhealthy socket (should be discarded)
  SocketPool::release(socket_pool, health_socket)
  
  assert_eq(SocketPool::get_active_sockets(socket_pool), 0)
  assert_eq(SocketPool::get_available_sockets(socket_pool), 5)
  
  // Test automatic socket cleanup
  let auto_socket_manager = AutoSocketManager::new()
  
  // Create sockets with automatic cleanup
  let auto_socket1 = AutoSocketManager::create(auto_socket_manager, "tcp", "example.com", 80)
  let auto_socket2 = AutoSocketManager::create(auto_socket_manager, "tcp", "example.com", 443)
  
  assert_eq(AutoSocketManager::get_active_socket_count(auto_socket_manager), 2)
  
  // Simulate going out of scope
  AutoSocketManager::cleanup_all(auto_socket_manager)
  
  assert_eq(AutoSocketManager::get_active_socket_count(auto_socket_manager), 0)
}

// Test 6: CPU Resource Management Tests
test "cpu resource management operations" {
  // Test CPU quota management
  let cpu_manager = CPUManager::new()
  
  // Set CPU quota for process
  CPUManager::set_quota(cpu_manager, 50.0)  // 50% CPU
  
  assert_eq(CPUManager::get_quota(cpu_manager), 50.0)
  
  // Check current usage
  let initial_usage = CPUManager::get_usage(cpu_manager)
  assert_true(initial_usage >= 0.0 && initial_usage <= 100.0)
  
  // Simulate CPU intensive work
  let cpu_intensive_thread = ThreadUtil::spawn(() -> {
    let start_time = TimeUtil::current_time_millis()
    while TimeUtil::current_time_millis() - start_time < 1000 {
      // CPU intensive work
      let mut result = 0
      for i in 0..1000000 {
        result = result + i
      }
    }
    return result
  })
  
  ThreadUtil::join(cpu_intensive_thread)
  
  // Check usage after work
  let final_usage = CPUManager::get_usage(cpu_manager)
  
  // Usage should be higher than initial
  assert_true(final_usage > initial_usage)
  
  // Test CPU affinity
  let affinity_manager = CPUAffinityManager::new()
  
  // Get available CPUs
  let available_cpus = CPUAffinityManager::get_available_cpus(affinity_manager)
  assert_true(available_cpus.length() > 0)
  
  // Set affinity for current thread
  if available_cpus.length() >= 2 {
    let target_cpus = [available_cpus[0], available_cpus[1]]
    CPUAffinityManager::set_affinity(affinity_manager, target_cpus)
    
    let current_affinity = CPUAffinityManager::get_affinity(affinity_manager)
    assert_eq(current_affinity.length(), 2)
    assert_true(current_affinity.contains(available_cpus[0]))
    assert_true(current_affinity.contains(available_cpus[1]))
  }
  
  // Test CPU throttling
  let throttle_manager = CPUThrottleManager::new()
  
  // Set throttling limits
  CPUThrottleManager::set_limits(throttle_manager, {
    "max_usage": 80.0,  // Max 80% CPU
    "check_interval": 1000,  // Check every 1 second
    "throttle_duration": 500  // Throttle for 500ms if over limit
  })
  
  // Simulate high CPU usage
  let high_cpu_thread = ThreadUtil::spawn(() -> {
    CPUThrottleManager::start_monitoring(throttle_manager)
    
    let start_time = TimeUtil::current_time_millis()
    while TimeUtil::current_time_millis() - start_time < 2000 {
      // High CPU work
      let mut result = 0
      for i in 0..2000000 {
        result = result + i
      }
    }
    
    CPUThrottleManager::stop_monitoring(throttle_manager)
    return result
  })
  
  ThreadUtil::join(high_cpu_thread)
  
  // Check if throttling was applied
  let throttle_stats = CPUThrottleManager::get_stats(throttle_manager)
  assert_true(throttle_stats.throttle_count > 0)
  assert_true(throttle_stats.total_throttle_time > 0)
}

// Test 7: Disk Space Management Tests
test "disk space management operations" {
  // Test disk space monitoring
  let disk_manager = DiskManager::new("/tmp")
  
  // Get disk space info
  let disk_info = DiskManager::get_space_info(disk_manager)
  
  assert_true(disk_info.total_space > 0)
  assert_true(disk_info.free_space > 0)
  assert_true(disk_info.used_space > 0)
  assert_true(disk_info.free_space <= disk_info.total_space)
  assert_true(disk_info.used_space <= disk_info.total_space)
  assert_eq(disk_info.free_space + disk_info.used_space, disk_info.total_space)
  
  // Test disk usage tracking
  let usage_tracker = DiskUsageTracker::new("/tmp")
  
  // Create test files
  DiskUsageTracker::create_file(usage_tracker, "test1.txt", 1024)  // 1KB
  DiskUsageTracker::create_file(usage_tracker, "test2.txt", 2048)  // 2KB
  DiskUsageTracker::create_file(usage_tracker, "test3.txt", 4096)  // 4KB
  
  // Track usage
  DiskUsageTracker::track_usage(usage_tracker)
  
  let usage_info = DiskUsageTracker::get_usage_info(usage_tracker)
  assert_eq(usage_info.tracked_files, 3)
  assert_eq(usage_info.tracked_size, 7168)  // 1024 + 2048 + 4096
  
  // Delete files and update tracking
  DiskUsageTracker::delete_file(usage_tracker, "test2.txt")
  DiskUsageTracker::update_tracking(usage_tracker)
  
  let updated_usage = DiskUsageTracker::get_usage_info(usage_tracker)
  assert_eq(updated_usage.tracked_files, 2)
  assert_eq(updated_usage.tracked_size, 5120)  // 1024 + 4096
  
  // Clean up
  DiskUsageTracker::delete_file(usage_tracker, "test1.txt")
  DiskUsageTracker::delete_file(usage_tracker, "test3.txt")
  DiskUsageTracker::update_tracking(usage_tracker)
  
  // Test disk space cleanup
  let cleanup_manager = DiskCleanupManager::new("/tmp")
  
  // Create files for cleanup
  DiskCleanupManager::create_temp_file(cleanup_manager, "temp1.txt", 1024)
  DiskCleanupManager::create_temp_file(cleanup_manager, "temp2.txt", 2048)
  DiskCleanupManager::create_old_file(cleanup_manager, "old.txt", 4096, 7)  // 7 days old
  
  // Configure cleanup rules
  DiskCleanupManager::add_rule(cleanup_manager, {
    "name": "temp_files",
    "pattern": "temp*.txt",
    "max_age": 0,  // Delete immediately
    "min_size": 0
  })
  
  DiskCleanupManager::add_rule(cleanup_manager, {
    "name": "old_files",
    "pattern": "*.txt",
    "max_age": 5,  // 5 days
    "min_size": 0
  })
  
  // Run cleanup
  let cleanup_result = DiskCleanupManager::cleanup(cleanup_manager)
  
  assert_true(cleanup_result.deleted_files >= 2)  // temp1.txt and temp2.txt
  assert_true(cleanup_result.freed_space >= 3072)  // At least 1KB + 2KB
  
  // Test disk space alerts
  let alert_manager = DiskAlertManager::new("/tmp")
  
  // Configure alerts
  DiskAlertManager::set_threshold(alert_manager, "warning", 80.0)  // 80% usage
  DiskAlertManager::set_threshold(alert_manager, "critical", 90.0)  // 90% usage
  
  // Check current usage
  let current_usage = DiskAlertManager::get_usage_percentage(alert_manager)
  
  // Should not trigger alerts unless usage is high
  if current_usage < 80.0 {
    assert_false(DiskAlertManager::has_alerts(alert_manager))
  }
  
  // Simulate high disk usage
  DiskAlertManager::simulate_usage(alert_manager, 85.0)
  assert_true(DiskAlertManager::has_alerts(alert_manager))
  
  let alerts = DiskAlertManager::get_alerts(alert_manager)
  assert_true(alerts.any((alert) => alert.level == "warning"))
  
  DiskAlertManager::simulate_usage(alert_manager, 95.0)
  let critical_alerts = DiskAlertManager::get_alerts(alert_manager)
  assert_true(critical_alerts.any((alert) => alert.level == "critical"));
}

// Test 8: Resource Pool Management Tests
test "resource pool management operations" {
  // Test generic resource pool
  let resource_pool = ResourcePool::new(() -> { TestResource::new() }, 5)  // Max 5 resources
  
  // Acquire resources
  let resource1 = ResourcePool::acquire(resource_pool)
  let resource2 = ResourcePool::acquire(resource_pool)
  let resource3 = ResourcePool::acquire(resource_pool)
  
  assert_eq(ResourcePool::get_active_resources(resource_pool), 3)
  assert_eq(ResourcePool::get_available_resources(resource_pool), 2)
  
  // Use resources
  let result1 = TestResource::process(resource1, "input1")
  let result2 = TestResource::process(resource2, "input2")
  let result3 = TestResource::process(resource3, "input3")
  
  assert_eq(result1, "processed_input1")
  assert_eq(result2, "processed_input2")
  assert_eq(result3, "processed_input3")
  
  // Release resources
  ResourcePool::release(resource_pool, resource1)
  ResourcePool::release(resource_pool, resource2)
  ResourcePool::release(resource_pool, resource3)
  
  assert_eq(ResourcePool::get_active_resources(resource_pool), 0)
  assert_eq(ResourcePool::get_available_resources(resource_pool), 5)
  
  // Test resource pool with factory
  let factory_pool = FactoryResourcePool::new(
    () -> { ExpensiveResource::new() },
    (resource) -> { ExpensiveResource::reset(resource) },
    3
  )
  
  // Acquire and release resources multiple times
  for i in 0..10 {
    let resource = FactoryResourcePool::acquire(factory_pool)
    ExpensiveResource::use(resource)
    FactoryResourcePool::release(factory_pool, resource)
  }
  
  // Should reuse resources
  assert_eq(FactoryResourcePool::get_creation_count(factory_pool), 3)
  assert_eq(FactoryResourcePool::get_reset_count(factory_pool), 10)
  
  // Test resource pool with validation
  let validating_pool = ValidatingResourcePool::new(
    () -> { ValidatableResource::new() },
    (resource) -> { ValidatableResource::is_valid(resource) },
    (resource) -> { ValidatableResource::repair(resource) },
    4
  )
  
  // Acquire resource
  let valid_resource = ValidatingResourcePool::acquire(validating_pool)
  
  // Simulate resource becoming invalid
  ValidatableResource::invalidate(valid_resource)
  
  // Return invalid resource
  ValidatingResourcePool::release(validating_pool, valid_resource)
  
  // Next acquire should repair or replace resource
  let repaired_resource = ValidatingResourcePool::acquire(validating_pool)
  assert_true(ValidatableResource::is_valid(repaired_resource))
  
  ValidatingResourcePool::release(validating_pool, repaired_resource)
  
  // Test resource pool with timeout
  let timeout_pool = TimeoutResourcePool::new(
    () -> { SlowResource::new() },
    2,  // Max 2 resources
    100  // 100ms timeout
  )
  
  // Acquire all resources
  let timeout_resource1 = TimeoutResourcePool::acquire(timeout_pool)
  let timeout_resource2 = TimeoutResourcePool::acquire(timeout_pool)
  
  // Try to acquire with timeout (should fail)
  let start_time = TimeUtil::current_time_millis()
  let timeout_result = TimeoutResourcePool::try_acquire_for(timeout_pool, 50)
  let elapsed = TimeUtil::current_time_millis() - start_time
  
  assert_false(timeout_result.success)
  assert_true(elapsed >= 40 && elapsed <= 100)
  
  // Release one resource and try again
  TimeoutResourcePool::release(timeout_pool, timeout_resource1)
  
  let retry_result = TimeoutResourcePool::try_acquire_for(timeout_pool, 50)
  assert_true(retry_result.success)
  
  TimeoutResourcePool::release(timeout_pool, timeout_resource2)
  TimeoutResourcePool::release(timeout_pool, retry_result.resource)
}

// Test 9: Resource Quota Management Tests
test "resource quota management operations" {
  // Test resource quota manager
  let quota_manager = QuotaManager::new()
  
  // Set quotas for different resources
  QuotaManager::set_quota(quota_manager, "memory", 1024 * 1024 * 1024)  // 1GB
  QuotaManager::set_quota(quota_manager, "cpu", 80.0)  // 80% CPU
  QuotaManager::set_quota(quota_manager, "disk", 10 * 1024 * 1024 * 1024)  // 10GB
  QuotaManager::set_quota(quota_manager, "connections", 100)  // 100 connections
  
  // Check quotas
  assert_eq(QuotaManager::get_quota(quota_manager, "memory"), 1024 * 1024 * 1024)
  assert_eq(QuotaManager::get_quota(quota_manager, "cpu"), 80.0)
  assert_eq(QuotaManager::get_quota(quota_manager, "disk"), 10 * 1024 * 1024 * 1024)
  assert_eq(QuotaManager::get_quota(quota_manager, "connections"), 100)
  
  // Test quota usage tracking
  QuotaManager::track_usage(quota_manager, "memory", 512 * 1024 * 1024)  // Use 512MB
  QuotaManager::track_usage(quota_manager, "cpu", 40.0)  // Use 40% CPU
  QuotaManager::track_usage(quota_manager, "disk", 5 * 1024 * 1024 * 1024)  // Use 5GB
  QuotaManager::track_usage(quota_manager, "connections", 50)  // Use 50 connections
  
  // Check usage
  assert_eq(QuotaManager::get_usage(quota_manager, "memory"), 512 * 1024 * 1024)
  assert_eq(QuotaManager::get_usage(quota_manager, "cpu"), 40.0)
  assert_eq(QuotaManager::get_usage(quota_manager, "disk"), 5 * 1024 * 1024 * 1024)
  assert_eq(QuotaManager::get_usage(quota_manager, "connections"), 50)
  
  // Check remaining quota
  assert_eq(QuotaManager::get_remaining(quota_manager, "memory"), 512 * 1024 * 1024)
  assert_eq(QuotaManager::get_remaining(quota_manager, "cpu"), 40.0)
  assert_eq(QuotaManager::get_remaining(quota_manager, "disk"), 5 * 1024 * 1024 * 1024)
  assert_eq(QuotaManager::get_remaining(quota_manager, "connections"), 50)
  
  // Test quota limits
  assert_true(QuotaManager::check_quota(quota_manager, "memory", 512 * 1024 * 1024))  // Within limit
  assert_false(QuotaManager::check_quota(quota_manager, "memory", 1024 * 1024 * 1024))  // Exceeds limit
  
  assert_true(QuotaManager::check_quota(quota_manager, "cpu", 40.0))  // Within limit
  assert_false(QuotaManager::check_quota(quota_manager, "cpu", 50.0))  // Exceeds limit
  
  // Test quota reservation
  let memory_reservation = QuotaManager::reserve(quota_manager, "memory", 256 * 1024 * 1024)
  assert_true(memory_reservation.success)
  
  // Usage should include reservation
  assert_eq(QuotaManager::get_usage(quota_manager, "memory"), 768 * 1024 * 1024)  // 512MB + 256MB
  
  // Release reservation
  QuotaManager::release_reservation(quota_manager, memory_reservation)
  
  assert_eq(QuotaManager::get_usage(quota_manager, "memory"), 512 * 1024 * 1024)  // Back to 512MB
  
  // Test quota alerts
  QuotaManager::set_alert_threshold(quota_manager, "memory", 80.0)  // Alert at 80% usage
  QuotaManager::set_alert_threshold(quota_manager, "cpu", 75.0)  // Alert at 75% usage
  
  // Use more resources to trigger alerts
  QuotaManager::track_usage(quota_manager, "memory", 512 * 1024 * 1024)  // Total 1GB (100%)
  QuotaManager::track_usage(quota_manager, "cpu", 30.0)  // Total 70% CPU
  
  // Check for alerts
  let memory_alerts = QuotaManager::get_alerts(quota_manager, "memory")
  assert_eq(memory_alerts.length(), 1)
  assert_eq(memory_alerts[0].threshold, 80.0)
  assert_eq(memory_alerts[0].current_usage, 100.0)
  
  let cpu_alerts = QuotaManager::get_alerts(quota_manager, "cpu")
  assert_eq(cpu_alerts.length(), 0)  // 70% is below 75% threshold
  
  // Use more CPU to trigger alert
  QuotaManager::track_usage(quota_manager, "cpu", 10.0)  // Total 80% CPU
  
  let cpu_alerts_after = QuotaManager::get_alerts(quota_manager, "cpu")
  assert_eq(cpu_alerts_after.length(), 1)
  assert_eq(cpu_alerts_after[0].threshold, 75.0)
  assert_eq(cpu_alerts_after[0].current_usage, 80.0)
}

// Test 10: Resource Monitoring and Reporting Tests
test "resource monitoring and reporting operations" {
  // Test resource monitor
  let resource_monitor = ResourceMonitor::new()
  
  // Start monitoring
  ResourceMonitor::start(resource_monitor)
  
  // Simulate resource usage
  let monitor_thread = ThreadUtil::spawn(() -> {
    TimeUtil::sleep(1000)
    
    // Use some memory
    let large_array = ArrayUtil::with_capacity(1024 * 1024)  // 1MB array
    
    // Use CPU
    let start_time = TimeUtil::current_time_millis()
    while TimeUtil::current_time_millis() - start_time < 1000 {
      let mut result = 0
      for i in 0..1000000 {
        result = result + i
      }
    }
    
    // Use disk
    let temp_file = FileUtil::create_temp("/tmp/monitor_test")
    FileUtil::write(temp_file, "x".repeat(1024 * 10))  // 10KB
    FileUtil::delete(temp_file)
  })
  
  ThreadUtil::join(monitor_thread)
  
  // Stop monitoring
  ResourceMonitor::stop(resource_monitor)
  
  // Get monitoring results
  let monitoring_results = ResourceMonitor::get_results(resource_monitor)
  
  assert_true(monitoring_results.duration > 0)
  assert_true(monitoring_results.memory_usage.peak > 0)
  assert_true(monitoring_results.cpu_usage.average > 0)
  assert_true(monitoring_results.disk_usage.bytes_written > 0)
  
  // Test resource reporter
  let resource_reporter = ResourceReporter::new()
  
  // Configure report
  ResourceReporter::configure(resource_reporter, {
    "include_memory": true,
    "include_cpu": true,
    "include_disk": true,
    "include_network": true,
    "format": "json",
    "output_file": "/tmp/resource_report.json"
  })
  
  // Generate report
  let report = ResourceReporter::generate(resource_reporter)
  
  assert_true(report.contains("memory"))
  assert_true(report.contains("cpu"))
  assert_true(report.contains("disk"))
  
  // Test resource alerts
  let alert_manager = ResourceAlertManager::new()
  
  // Configure alerts
  ResourceAlertManager::add_alert(alert_manager, {
    "name": "high_memory",
    "resource": "memory",
    "condition": "usage > 80%",
    "severity": "warning",
    "action": "log"
  })
  
  ResourceAlertManager::add_alert(alert_manager, {
    "name": "critical_cpu",
    "resource": "cpu",
    "condition": "usage > 90%",
    "severity": "critical",
    "action": "notify"
  })
  
  // Simulate high resource usage
  ResourceAlertManager::report_usage(alert_manager, "memory", 85.0)
  ResourceAlertManager::report_usage(alert_manager, "cpu", 95.0)
  
  // Check alerts
  let alerts = ResourceAlertManager::get_active_alerts(alert_manager)
  assert_eq(alerts.length(), 2)
  
  let memory_alert = ArrayUtil::find(alerts, (alert) => { alert.name == "high_memory" })
  assert_eq(memory_alert.severity, "warning")
  
  let cpu_alert = ArrayUtil::find(alerts, (alert) => { alert.name == "critical_cpu" })
  assert_eq(cpu_alert.severity, "critical")
  
  // Test resource trends
  let trend_analyzer = ResourceTrendAnalyzer::new()
  
  // Add historical data
  let now = TimeUtil::current_time_millis()
  for i in 0..24 {
    let timestamp = now - (23 - i) * 3600 * 1000  // Last 24 hours
    let memory_usage = 50.0 + (i % 10) * 5.0  // Varying memory usage
    let cpu_usage = 30.0 + (i % 8) * 7.5  // Varying CPU usage
    
    ResourceTrendAnalyzer::add_data_point(trend_analyzer, timestamp, {
      "memory": memory_usage,
      "cpu": cpu_usage
    })
  }
  
  // Analyze trends
  let memory_trend = ResourceTrendAnalyzer::analyze_trend(trend_analyzer, "memory")
  let cpu_trend = ResourceTrendAnalyzer::analyze_trend(trend_analyzer, "cpu")
  
  assert_true(memory_trend.slope != 0.0)  // Should have some trend
  assert_true(cpu_trend.slope != 0.0)  // Should have some trend
  
  // Test resource predictions
  let prediction = ResourceTrendAnalyzer::predict(trend_analyzer, "memory", 3600 * 1000)  // 1 hour ahead
  
  assert_true(prediction.predicted_value > 0)
  assert_true(prediction.confidence > 0.0 && prediction.confidence <= 1.0)
  
  // Test resource optimization suggestions
  let optimizer = ResourceOptimizer::new()
  
  // Analyze current usage
  let current_usage = {
    "memory": 75.0,
    "cpu": 60.0,
    "disk": 40.0,
    "network": 30.0
  }
  
  let suggestions = ResourceOptimizer::get_suggestions(optimizer, current_usage)
  
  assert_eq(suggestions.length(), 4)
  
  let memory_suggestion = ArrayUtil::find(suggestions, (s) -> { s.resource == "memory" })
  assert_eq(memory_suggestion.resource, "memory")
  assert_true(memory_suggestion.suggestion.length() > 0)
  
  let cpu_suggestion = ArrayUtil::find(suggestions, (s) => { s.resource == "cpu" })
  assert_eq(cpu_suggestion.resource, "cpu")
  assert_true(cpu_suggestion.suggestion.length() > 0)
}