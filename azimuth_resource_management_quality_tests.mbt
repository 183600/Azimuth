// Azimuth Telemetry System - Resource Management Quality Tests
// This file contains comprehensive test cases for resource management, cleanup, and lifecycle

// Test 1: Memory Resource Management
test "memory resource management" {
  let memory_manager = MemoryManager::new()
  
  // Test initial memory state
  let initial_memory = MemoryManager::get_memory_info(memory_manager)
  assert_true(initial_memory.total_heap > 0)
  assert_eq(initial_memory.used_heap, 0)
  assert_eq(initial_memory.allocations, 0)
  
  // Test memory allocation
  let block1 = MemoryManager::allocate(memory_manager, 1024)  // 1KB
  let block2 = MemoryManager::allocate(memory_manager, 2048)  // 2KB
  
  let allocated_memory = MemoryManager::get_memory_info(memory_manager)
  assert_eq(allocated_memory.allocations, 2)
  assert_eq(allocated_memory.used_heap, 3072)  // 1024 + 2048
  
  // Test memory deallocation
  MemoryManager::deallocate(memory_manager, block1)
  
  let deallocated_memory = MemoryManager::get_memory_info(memory_manager)
  assert_eq(deallocated_memory.allocations, 1)
  assert_eq(deallocated_memory.used_heap, 2048)  // Only block2 remains
  
  // Test memory pool
  let memory_pool = MemoryManager::create_pool(memory_manager, 4096)  // 4KB pool
  
  let pool_block1 = MemoryManager::allocate_from_pool(memory_manager, memory_pool, 1024)
  let pool_block2 = MemoryManager::allocate_from_pool(memory_manager, memory_pool, 1024)
  
  let pool_info = MemoryManager::get_pool_info(memory_manager, memory_pool)
  assert_eq(pool_info.used_blocks, 2)
  assert_eq(pool_info.free_blocks, 2)  // 4096 / 1024 = 4 blocks total
  
  // Test pool fragmentation
  MemoryManager::deallocate_to_pool(memory_manager, memory_pool, pool_block1)
  
  let fragmented_pool = MemoryManager::get_pool_info(memory_manager, memory_pool)
  assert_eq(fragmented_pool.used_blocks, 1)
  assert_eq(fragmented_pool.free_blocks, 3)
  assert_true(fragmented_pool.fragmentation_ratio > 0.0)
  
  // Test pool compaction
  MemoryManager::compact_pool(memory_manager, memory_pool)
  
  let compacted_pool = MemoryManager::get_pool_info(memory_manager, memory_pool)
  assert_eq(compacted_pool.fragmentation_ratio, 0.0)
}

// Test 2: File Resource Management
test "file resource management" {
  let file_manager = FileManager::new()
  
  // Test file creation and tracking
  let file1 = FileManager::create_file(file_manager, "test_file1.txt")
  let file2 = FileManager::create_file(file_manager, "test_file2.txt")
  
  assert_true(FileManager::file_exists(file_manager, "test_file1.txt"))
  assert_true(FileManager::file_exists(file_manager, "test_file2.txt"))
  
  let file_info = FileManager::get_file_info(file_manager)
  assert_eq(file_info.open_files, 2)
  assert_eq(file_info.total_size, 0)  // Empty files
  
  // Test file writing
  FileManager::write_to_file(file_manager, file1, "Test content for file 1")
  FileManager::write_to_file(file_manager, file2, "Test content for file 2")
  
  let updated_info = FileManager::get_file_info(file_manager)
  assert_true(updated_info.total_size > 0)
  
  // Test file reading
  let content1 = FileManager::read_from_file(file_manager, file1)
  let content2 = FileManager::read_from_file(file_manager, file2)
  
  assert_eq(content1, "Test content for file 1")
  assert_eq(content2, "Test content for file 2")
  
  // Test file cleanup
  FileManager::close_file(file_manager, file1)
  FileManager::close_file(file_manager, file2)
  
  let closed_info = FileManager::get_file_info(file_manager)
  assert_eq(closed_info.open_files, 0)
  
  // Test automatic cleanup on resource manager destruction
  let temp_manager = FileManager::new()
  let temp_file = FileManager::create_file(temp_manager, "temp_file.txt")
  FileManager::write_to_file(temp_manager, temp_file, "Temporary content")
  
  assert_true(FileManager::file_exists(temp_manager, "temp_file.txt"))
  
  // In a real scenario, when temp_manager goes out of scope,
  // all its resources would be automatically cleaned up
  FileManager::cleanup_all(temp_manager)
  assert_false(FileManager::file_exists(temp_manager, "temp_file.txt"))
}

// Test 3: Network Resource Management
test "network resource management" {
  let network_manager = NetworkManager::new()
  
  // Test connection creation
  let connection1 = NetworkManager::create_connection(network_manager, "example.com", 80)
  let connection2 = NetworkManager::create_connection(network_manager, "api.example.com", 443)
  
  assert_true(NetworkManager::is_connected(network_manager, connection1))
  assert_true(NetworkManager::is_connected(network_manager, connection2))
  
  let network_info = NetworkManager::get_network_info(network_manager)
  assert_eq(network_info.active_connections, 2)
  assert_eq(network_info.total_bytes_sent, 0)
  assert_eq(network_info.total_bytes_received, 0)
  
  // Test data transmission
  NetworkManager::send_data(network_manager, connection1, "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
  NetworkManager::send_data(network_manager, connection2, "POST /api HTTP/1.1\r\nHost: api.example.com\r\n\r\n")
  
  let sent_info = NetworkManager::get_network_info(network_manager)
  assert_true(sent_info.total_bytes_sent > 0)
  
  // Test data reception
  let response1 = NetworkManager::receive_data(network_manager, connection1)
  let response2 = NetworkManager::receive_data(network_manager, connection2)
  
  let received_info = NetworkManager::get_network_info(network_manager)
  assert_true(received_info.total_bytes_received > 0)
  
  // Test connection pooling
  let connection_pool = NetworkManager::create_connection_pool(network_manager, "pool.example.com", 80, 5)
  
  let pooled_conn1 = NetworkManager::get_pooled_connection(network_manager, connection_pool)
  let pooled_conn2 = NetworkManager::get_pooled_connection(network_manager, connection_pool)
  
  let pool_info = NetworkManager::get_pool_info(network_manager, connection_pool)
  assert_eq(pool_info.active_connections, 2)
  assert_eq(pool_info.available_connections, 3)
  
  // Test connection return to pool
  NetworkManager::return_pooled_connection(network_manager, connection_pool, pooled_conn1)
  
  let returned_pool_info = NetworkManager::get_pool_info(network_manager, connection_pool)
  assert_eq(returned_pool_info.active_connections, 1)
  assert_eq(returned_pool_info.available_connections, 4)
  
  // Test connection cleanup
  NetworkManager::close_connection(network_manager, connection1)
  NetworkManager::close_connection(network_manager, connection2)
  
  let closed_network_info = NetworkManager::get_network_info(network_manager)
  assert_eq(closed_network_info.active_connections, 1)  // Only pooled_conn2 is still active
}

// Test 4: Database Connection Management
test "database connection management" {
  let db_manager = DatabaseManager::new()
  
  // Test database connection
  let db_connection = DatabaseManager::create_connection(db_manager, "postgresql://localhost:5432/testdb")
  
  assert_true(DatabaseManager::is_connected(db_manager, db_connection))
  
  let db_info = DatabaseManager::get_connection_info(db_manager)
  assert_eq(db_info.active_connections, 1)
  assert_eq(db_info.idle_connections, 0)
  
  // Test connection pool
  let db_pool = DatabaseManager::create_connection_pool(db_manager, "postgresql://localhost:5432/testdb", 10)
  
  let pool_conn1 = DatabaseManager::get_connection(db_manager, db_pool)
  let pool_conn2 = DatabaseManager::get_connection(db_manager, db_pool)
  
  let pool_info = DatabaseManager::get_pool_info(db_manager, db_pool)
  assert_eq(pool_info.active_connections, 2)
  assert_eq(pool_info.idle_connections, 8)
  
  // Test query execution
  let result1 = DatabaseManager::execute_query(db_manager, pool_conn1, "SELECT * FROM users WHERE id = 1")
  let result2 = DatabaseManager::execute_query(db_manager, pool_conn2, "SELECT * FROM products WHERE id = 1")
  
  assert_true(DatabaseManager::query_success(result1))
  assert_true(DatabaseManager::query_success(result2))
  
  // Test transaction management
  let transaction = DatabaseManager::begin_transaction(db_manager, pool_conn1)
  
  DatabaseManager::execute_in_transaction(db_manager, transaction, "INSERT INTO users (name) VALUES ('test')")
  DatabaseManager::execute_in_transaction(db_manager, transaction, "UPDATE users SET name = 'updated' WHERE id = 1")
  
  DatabaseManager::commit_transaction(db_manager, transaction)
  
  // Test connection return to pool
  DatabaseManager::return_connection(db_manager, db_pool, pool_conn1)
  DatabaseManager::return_connection(db_manager, db_pool, pool_conn2)
  
  let returned_pool_info = DatabaseManager::get_pool_info(db_manager, db_pool)
  assert_eq(returned_pool_info.active_connections, 0)
  assert_eq(returned_pool_info.idle_connections, 10)
  
  // Test connection cleanup
  DatabaseManager::close_connection(db_manager, db_connection)
  DatabaseManager::cleanup_pool(db_manager, db_pool)
  
  let cleaned_db_info = DatabaseManager::get_connection_info(db_manager)
  assert_eq(cleaned_db_info.active_connections, 0)
}

// Test 5: Thread Resource Management
test "thread resource management" {
  let thread_manager = ThreadManager::new()
  
  // Test thread creation
  let thread1 = ThreadManager::create_thread(thread_manager, "worker_thread_1", fn() {
    // Simulate work
    ThreadManager::sleep(100)  // 100ms
    return "thread1_result"
  })
  
  let thread2 = ThreadManager::create_thread(thread_manager, "worker_thread_2", fn() {
    // Simulate work
    ThreadManager::sleep(200)  // 200ms
    return "thread2_result"
  })
  
  let thread_info = ThreadManager::get_thread_info(thread_manager)
  assert_eq(thread_info.active_threads, 2)
  assert_eq(thread_info.completed_threads, 0)
  
  // Test thread pool
  let thread_pool = ThreadManager::create_thread_pool(thread_manager, "worker_pool", 4)
  
  let task1 = ThreadManager::submit_task(thread_manager, thread_pool, fn() {
    return "task1_result"
  })
  
  let task2 = ThreadManager::submit_task(thread_manager, thread_pool, fn() {
    return "task2_result"
  })
  
  let pool_info = ThreadManager::get_pool_info(thread_manager, thread_pool)
  assert_eq(pool_info.active_threads, 4)
  assert_eq(pool_info.pending_tasks, 0)  // Tasks should be picked up immediately
  
  // Test task results
  let result1 = ThreadManager::get_task_result(thread_manager, task1)
  let result2 = ThreadManager::get_task_result(thread_manager, task2)
  
  assert_eq(result1, "task1_result")
  assert_eq(result2, "task2_result")
  
  // Test thread cleanup
  ThreadManager::join_thread(thread_manager, thread1)
  ThreadManager::join_thread(thread_manager, thread2)
  
  let joined_info = ThreadManager::get_thread_info(thread_manager)
  assert_eq(joined_info.active_threads, 0)
  assert_eq(joined_info.completed_threads, 2)
  
  // Test thread pool shutdown
  ThreadManager::shutdown_pool(thread_manager, thread_pool)
  
  let shutdown_pool_info = ThreadManager::get_pool_info(thread_manager, thread_pool)
  assert_eq(shutdown_pool_info.active_threads, 0)
}

// Test 6: Resource Lifecycle Management
test "resource lifecycle management" {
  let lifecycle_manager = LifecycleManager::new()
  
  // Test resource lifecycle registration
  let resource = TestResource::new("test_resource")
  
  LifecycleManager::register_resource(lifecycle_manager, resource, fn(res) {
    TestResource::cleanup(res)
  })
  
  assert_true(LifecycleManager::is_resource_registered(lifecycle_manager, resource))
  
  // Test resource state tracking
  let initial_state = LifecycleManager::get_resource_state(lifecycle_manager, resource)
  assert_eq(initial_state, ResourceState::Initialized)
  
  // Test resource activation
  LifecycleManager::activate_resource(lifecycle_manager, resource)
  
  let active_state = LifecycleManager::get_resource_state(lifecycle_manager, resource)
  assert_eq(active_state, ResourceState::Active)
  
  // Test resource deactivation
  LifecycleManager::deactivate_resource(lifecycle_manager, resource)
  
  let deactivated_state = LifecycleManager::get_resource_state(lifecycle_manager, resource)
  assert_eq(deactivated_state, ResourceState::Inactive)
  
  // Test resource cleanup
  LifecycleManager::cleanup_resource(lifecycle_manager, resource)
  
  let cleaned_state = LifecycleManager::get_resource_state(lifecycle_manager, resource)
  assert_eq(cleaned_state, ResourceState::Cleaned)
  
  // Test automatic cleanup on manager shutdown
  let auto_resource = TestResource::new("auto_resource")
  
  LifecycleManager::register_resource(lifecycle_manager, auto_resource, fn(res) {
    TestResource::cleanup(res)
  })
  
  LifecycleManager::activate_resource(lifecycle_manager, auto_resource)
  
  // Simulate manager shutdown
  LifecycleManager::shutdown_all(lifecycle_manager)
  
  let shutdown_state = LifecycleManager::get_resource_state(lifecycle_manager, auto_resource)
  assert_eq(shutdown_state, ResourceState::Cleaned)
}

// Test 7: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = LeakDetector::new()
  
  // Test initial state
  assert_false(LeakDetector::has_leaks(leak_detector))
  assert_eq(LeakDetector::get_leak_count(leak_detector), 0)
  
  // Test resource tracking
  let resource1 = LeakDetector::track_resource(leak_detector, "memory_block", 1024)
  let resource2 = LeakDetector::track_resource(leak_detector, "file_handle", "test.txt")
  let resource3 = LeakDetector::track_resource(leak_detector, "network_connection", "example.com:80")
  
  assert_eq(LeakDetector::get_tracked_count(leak_detector), 3)
  
  // Test resource release
  LeakDetector::release_resource(leak_detector, resource1)
  
  assert_eq(LeakDetector::get_tracked_count(leak_detector), 2)
  
  // Simulate leak by not releasing resource2 and resource3
  // In a real scenario, this would be detected when the program exits
  // or when explicitly checking for leaks
  
  // Test leak detection
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 2)
  
  let file_leak = LeakDetector::find_leak_by_type(leak_detector, "file_handle")
  match file_leak {
    Some(leak) => {
      assert_eq(leak.resource_type, "file_handle")
      assert_eq(leak.resource_id, "test.txt")
    }
    None => assert_true(false)
  }
  
  let connection_leak = LeakDetector::find_leak_by_type(leak_detector, "network_connection")
  match connection_leak {
    Some(leak) => {
      assert_eq(leak.resource_type, "network_connection")
      assert_eq(leak.resource_id, "example.com:80")
    }
    None => assert_true(false)
  }
  
  // Test leak report generation
  let leak_report = LeakDetector::generate_leak_report(leak_detector)
  assert_true(leak_report.contains("Resource Leak Report"))
  assert_true(leak_report.contains("file_handle"))
  assert_true(leak_report.contains("network_connection"))
  
  // Test automatic leak cleanup
  LeakDetector::cleanup_all_leaks(leak_detector)
  
  assert_eq(LeakDetector::get_tracked_count(leak_detector), 0)
  assert_false(LeakDetector::has_leaks(leak_detector))
}

// Test 8: Resource Quota Management
test "resource quota management" {
  let quota_manager = QuotaManager::new()
  
  // Test quota definition
  QuotaManager::set_quota(quota_manager, "memory", 1024 * 1024)  // 1MB
  QuotaManager::set_quota(quota_manager, "file_handles", 10)
  QuotaManager::set_quota(quota_manager, "network_connections", 5)
  
  assert_eq(QuotaManager::get_quota(quota_manager, "memory"), 1024 * 1024)
  assert_eq(QuotaManager::get_quota(quota_manager, "file_handles"), 10)
  assert_eq(QuotaManager::get_quota(quota_manager, "network_connections"), 5)
  
  // Test quota usage tracking
  QuotaManager::allocate(quota_manager, "memory", 512 * 1024)  // 512KB
  QuotaManager::allocate(quota_manager, "file_handles", 3)
  QuotaManager::allocate(quota_manager, "network_connections", 2)
  
  assert_eq(QuotaManager::get_usage(quota_manager, "memory"), 512 * 1024)
  assert_eq(QuotaManager::get_usage(quota_manager, "file_handles"), 3)
  assert_eq(QuotaManager::get_usage(quota_manager, "network_connections"), 2)
  
  // Test quota availability
  assert_true(QuotaManager::is_available(quota_manager, "memory", 512 * 1024))  // 512KB available
  assert_true(QuotaManager::is_available(quota_manager, "file_handles", 7))      // 7 handles available
  assert_true(QuotaManager::is_available(quota_manager, "network_connections", 3)) // 3 connections available
  
  assert_false(QuotaManager::is_available(quota_manager, "memory", 1024 * 1024))  // Not enough memory
  assert_false(QuotaManager::is_available(quota_manager, "file_handles", 8))      // Not enough handles
  assert_false(QuotaManager::is_available(quota_manager, "network_connections", 4)) // Not enough connections
  
  // Test quota enforcement
  let memory_allocation = QuotaManager::try_allocate(quota_manager, "memory", 512 * 1024)
  assert_true(memory_allocation.success)
  
  let exceeded_memory_allocation = QuotaManager::try_allocate(quota_manager, "memory", 1024 * 1024)
  assert_false(exceeded_memory_allocation.success)
  assert_eq(exceeded_memory_allocation.reason, AllocationFailureReason::QuotaExceeded)
  
  // Test quota release
  QuotaManager::release(quota_manager, "memory", 512 * 1024)
  
  assert_eq(QuotaManager::get_usage(quota_manager, "memory"), 512 * 1024)
  
  // Test quota exceeded state
  QuotaManager::allocate(quota_manager, "file_handles", 8)  // Total: 11 handles
  
  assert_true(QuotaManager::is_quota_exceeded(quota_manager, "file_handles"))
  
  // Test quota report
  let quota_report = QuotaManager::generate_report(quota_manager)
  assert_true(quota_report.contains("Resource Quota Report"))
  assert_true(quota_report.contains("memory"))
  assert_true(quota_report.contains("file_handles"))
  assert_true(quota_report.contains("network_connections"))
}

// Test 9: Resource Pool Management
test "resource pool management" {
  let pool_manager = PoolManager::new()
  
  // Test resource pool creation
  let resource_pool = PoolManager::create_pool(pool_manager, "test_pool", 5, fn() {
    return PooledResource::new()
  })
  
  assert_eq(PoolManager::get_pool_size(pool_manager, resource_pool), 5)
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 5)
  assert_eq(PoolManager::get_in_use_count(pool_manager, resource_pool), 0)
  
  // Test resource acquisition
  let resource1 = PoolManager::acquire_resource(pool_manager, resource_pool)
  let resource2 = PoolManager::acquire_resource(pool_manager, resource_pool)
  let resource3 = PoolManager::acquire_resource(pool_manager, resource_pool)
  
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 2)
  assert_eq(PoolManager::get_in_use_count(pool_manager, resource_pool), 3)
  
  // Test resource release
  PoolManager::release_resource(pool_manager, resource_pool, resource1)
  
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 3)
  assert_eq(PoolManager::get_in_use_count(pool_manager, resource_pool), 2)
  
  // Test pool exhaustion
  let resources = []
  for i in 0..=4 {
    let res = PoolManager::acquire_resource(pool_manager, resource_pool)
    resources.push(res)
  }
  
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 0)
  assert_eq(PoolManager::get_in_use_count(pool_manager, resource_pool), 5)
  
  // Test pool expansion
  PoolManager::expand_pool(pool_manager, resource_pool, 3)
  
  assert_eq(PoolManager::get_pool_size(pool_manager, resource_pool), 8)
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 3)
  
  // Test resource validation
  let invalid_resource = PooledResource::new()
  PooledResource::set_invalid(invalid_resource)
  
  PoolManager::release_resource(pool_manager, resource_pool, invalid_resource)
  
  // Invalid resources should not be returned to the pool
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 2)  // One less than expected
  assert_eq(PoolManager::get_invalid_count(pool_manager, resource_pool), 1)
  
  // Test pool cleanup
  PoolManager::cleanup_invalid_resources(pool_manager, resource_pool)
  
  assert_eq(PoolManager::get_invalid_count(pool_manager, resource_pool), 0)
  
  // Release all resources
  for res in resources {
    PoolManager::release_resource(pool_manager, resource_pool, res)
  }
  
  assert_eq(PoolManager::get_available_count(pool_manager, resource_pool), 7)
  assert_eq(PoolManager::get_in_use_count(pool_manager, resource_pool), 0)
}

// Test 10: Resource Dependency Management
test "resource dependency management" {
  let dependency_manager = DependencyManager::new()
  
  // Test resource creation with dependencies
  let database_resource = DatabaseResource::new("primary_db")
  let cache_resource = CacheResource::new("primary_cache")
  let application_resource = ApplicationResource::new("main_app")
  
  // Define dependencies
  DependencyManager::add_dependency(dependency_manager, cache_resource, database_resource)
  DependencyManager::add_dependency(dependency_manager, application_resource, cache_resource)
  
  // Test dependency graph
  let dependencies = DependencyManager::get_dependencies(dependency_manager, application_resource)
  assert_eq(dependencies.length(), 1)
  assert_true(dependencies.contains(cache_resource))
  
  let transitive_dependencies = DependencyManager::get_all_dependencies(dependency_manager, application_resource)
  assert_eq(transitive_dependencies.length(), 2)
  assert_true(transitive_dependencies.contains(cache_resource))
  assert_true(transitive_dependencies.contains(database_resource))
  
  // Test dependency order calculation
  let startup_order = DependencyManager::calculate_startup_order(dependency_manager, [
    application_resource,
    database_resource,
    cache_resource
  ])
  
  // Database should start first, then cache, then application
  assert_eq(startup_order[0], database_resource)
  assert_eq(startup_order[1], cache_resource)
  assert_eq(startup_order[2], application_resource)
  
  // Test shutdown order calculation
  let shutdown_order = DependencyManager::calculate_shutdown_order(dependency_manager, [
    application_resource,
    database_resource,
    cache_resource
  ])
  
  // Application should shutdown first, then cache, then database
  assert_eq(shutdown_order[0], application_resource)
  assert_eq(shutdown_order[1], cache_resource)
  assert_eq(shutdown_order[2], database_resource)
  
  // Test circular dependency detection
  let circular_resource1 = CircularResource::new("circular1")
  let circular_resource2 = CircularResource::new("circular2")
  
  DependencyManager::add_dependency(dependency_manager, circular_resource1, circular_resource2)
  DependencyManager::add_dependency(dependency_manager, circular_resource2, circular_resource1)
  
  let circular_dependencies = DependencyManager::detect_circular_dependencies(dependency_manager)
  assert_eq(circular_dependencies.length(), 1)
  assert_true(circular_dependencies.contains([circular_resource1, circular_resource2]))
  
  // Test dependency validation
  let valid_dependencies = DependencyManager::validate_dependencies(dependency_manager)
  assert_false(valid_dependencies)  // Should be invalid due to circular dependency
  
  // Remove circular dependency
  DependencyManager::remove_dependency(dependency_manager, circular_resource2, circular_resource1)
  
  let valid_dependencies_after_removal = DependencyManager::validate_dependencies(dependency_manager)
  assert_true(valid_dependencies_after_removal)  // Should be valid now
  
  // Test resource initialization with dependencies
  DependencyManager::initialize_with_dependencies(dependency_manager, application_resource)
  
  assert_true(DatabaseResource::is_initialized(database_resource))
  assert_true(CacheResource::is_initialized(cache_resource))
  assert_true(ApplicationResource::is_initialized(application_resource))
  
  // Test resource cleanup with dependencies
  DependencyManager::cleanup_with_dependencies(dependency_manager, application_resource)
  
  assert_false(ApplicationResource::is_initialized(application_resource))
  assert_false(CacheResource::is_initialized(cache_resource))
  assert_false(DatabaseResource::is_initialized(database_resource))
}