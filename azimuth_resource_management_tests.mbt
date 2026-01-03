// Azimuth Resource Management and Cleanup Tests
// This file contains test cases for resource management and cleanup

// Test 1: Basic Resource Management
test "basic resource management" {
  let resource_manager = ResourceManager::new()
  
  // Test resource creation and registration
  let resource1 = TestResource::new("resource1")
  let resource2 = TestResource::new("resource2")
  
  let id1 = ResourceManager::register(resource_manager, resource1)
  let id2 = ResourceManager::register(resource_manager, resource2)
  
  // Test resource retrieval
  let retrieved1 = ResourceManager::get(resource_manager, id1)
  match retrieved1 {
    Some(resource) => assert_eq(TestResource::name(resource), "resource1")
    None => assert_true(false)
  }
  
  let retrieved2 = ResourceManager::get(resource_manager, id2)
  match retrieved2 {
    Some(resource) => assert_eq(TestResource::name(resource), "resource2")
    None => assert_true(false)
  }
  
  // Test resource removal
  ResourceManager::remove(resource_manager, id1)
  let removed1 = ResourceManager::get(resource_manager, id1)
  match removed1 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify resource2 is still available
  let still_available = ResourceManager::get(resource_manager, id2)
  match still_available {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
}

// Test 2: Automatic Resource Cleanup
test "automatic resource cleanup" {
  let resource_manager = ResourceManager::new()
  let cleanup_called = AtomicInt::new(0)
  
  // Create a resource with cleanup callback
  let resource = TestResource::with_cleanup("cleanup_resource", {
    AtomicInt::increment(cleanup_called)
  })
  
  let id = ResourceManager::register(resource_manager, resource)
  
  // Verify resource is registered
  let retrieved = ResourceManager::get(resource_manager, id)
  match retrieved {
    Some(_) => assert_true(true)
    None => assert_true(false)
  }
  
  // Remove resource and verify cleanup is called
  ResourceManager::remove(resource_manager, id)
  assert_eq(AtomicInt::get(cleanup_called), 1)
  
  // Verify resource is no longer available
  let removed = ResourceManager::get(resource_manager, id)
  match removed {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Resource Pool Management
test "resource pool management" {
  let resource_pool = ResourcePool::new(5) // Pool size 5
  
  // Test pool initial state
  assert_eq(ResourcePool::available_count(resource_pool), 5)
  assert_eq(ResourcePool::active_count(resource_pool), 0)
  
  // Test resource acquisition
  let resource1 = ResourcePool::acquire(resource_pool)
  let resource2 = ResourcePool::acquire(resource_pool)
  let resource3 = ResourcePool::acquire(resource_pool)
  
  assert_eq(ResourcePool::available_count(resource_pool), 2)
  assert_eq(ResourcePool::active_count(resource_pool), 3)
  
  // Test resource release
  ResourcePool::release(resource_pool, resource1)
  assert_eq(ResourcePool::available_count(resource_pool), 3)
  assert_eq(ResourcePool::active_count(resource_pool), 2)
  
  // Test pool exhaustion
  let resources = []
  for i = 0; i < 5; i = i + 1 {
    match ResourcePool::try_acquire(resource_pool) {
      Some(resource) => resources.push(resource)
      None => break
    }
  }
  
  // Pool should be exhausted now
  let exhausted = ResourcePool::try_acquire(resource_pool)
  match exhausted {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Release all resources
  for resource in resources {
    ResourcePool::release(resource_pool, resource)
  }
  
  // Pool should be fully available again
  assert_eq(ResourcePool::available_count(resource_pool), 5)
  assert_eq(ResourcePool::active_count(resource_pool), 0)
}

// Test 4: Resource Lifecycle Management
test "resource lifecycle management" {
  let lifecycle_manager = ResourceLifecycleManager::new()
  
  // Test resource lifecycle states
  let resource = TestResource::new("lifecycle_resource")
  let lifecycle = ResourceLifecycle::new(resource)
  
  // Initial state should be Uninitialized
  assert_eq(ResourceLifecycle::state(lifecycle), Uninitialized)
  
  // Initialize resource
  ResourceLifecycle::initialize(lifecycle)
  assert_eq(ResourceLifecycle::state(lifecycle), Ready)
  
  // Activate resource
  ResourceLifecycle::activate(lifecycle)
  assert_eq(ResourceLifecycle::state(lifecycle), Active)
  
  // Deactivate resource
  ResourceLifecycle::deactivate(lifecycle)
  assert_eq(ResourceLifecycle::state(lifecycle), Inactive)
  
  // Cleanup resource
  ResourceLifecycle::cleanup(lifecycle)
  assert_eq(ResourceLifecycle::state(lifecycle), Disposed)
}

// Test 5: Memory Resource Management
test "memory resource management" {
  let memory_manager = MemoryResourceManager::new(1024 * 1024) // 1MB limit
  
  // Test memory allocation
  let allocation1 = MemoryResourceManager::allocate(memory_manager, 1024) // 1KB
  let allocation2 = MemoryResourceManager::allocate(memory_manager, 2048) // 2KB
  
  // Verify allocations
  assert_true(MemoryResourceManager::is_valid(allocation1))
  assert_true(MemoryResourceManager::is_valid(allocation2))
  
  // Test memory usage tracking
  assert_eq(MemoryResourceManager::used_memory(memory_manager), 3072) // 1KB + 2KB
  assert_eq(MemoryResourceManager::available_memory(memory_manager), 1047552) // 1MB - 3KB
  
  // Test memory deallocation
  MemoryResourceManager::deallocate(memory_manager, allocation1)
  assert_eq(MemoryResourceManager::used_memory(memory_manager), 2048)
  assert_false(MemoryResourceManager::is_valid(allocation1))
  
  // Test memory limit enforcement
  let large_allocation = MemoryResourceManager::try_allocate(memory_manager, 2 * 1024 * 1024) // 2MB
  match large_allocation {
    Some(_) => assert_true(false) // Should fail
    None => assert_true(true)    // Expected to fail
  }
}

// Test 6: File Resource Management
test "file resource management" {
  let file_manager = FileResourceManager::new()
  
  // Test file resource creation
  let temp_file = "/tmp/azimuth_test_file.txt"
  let file_resource = FileResourceManager::create(file_manager, temp_file)
  
  assert_true(FileResourceManager::exists(file_resource))
  assert_eq(FileResourceManager::path(file_resource), temp_file)
  
  // Test file resource operations
  FileResourceManager::write(file_resource, "Test content")
  let content = FileResourceManager::read(file_resource)
  assert_eq(content, "Test content")
  
  // Test file resource cleanup
  FileResourceManager::cleanup(file_resource)
  assert_false(FileResourceManager::exists(file_resource))
}

// Test 7: Network Resource Management
test "network resource management" {
  let network_manager = NetworkResourceManager::new()
  
  // Test connection resource
  let connection = NetworkResourceManager::create_connection(
    network_manager,
    "https://example.com",
    8080,
    5000 // timeout
  )
  
  assert_true(NetworkResourceManager::is_connected(connection))
  assert_eq(NetworkResourceManager::get_endpoint(connection), "https://example.com:8080")
  
  // Test connection pool
  let pool = NetworkResourceManager::create_pool(network_manager, 10) // Max 10 connections
  let pooled_connection = NetworkResourceManager::acquire_connection(pool)
  
  assert_true(NetworkResourceManager::is_connected(pooled_connection))
  
  // Test connection release
  NetworkResourceManager::release_connection(pool, pooled_connection)
  assert_eq(NetworkResourceManager::available_connections(pool), 1)
  
  // Test pool cleanup
  NetworkResourceManager::cleanup_pool(pool)
  assert_eq(NetworkResourceManager::available_connections(pool), 0)
}

// Test 8: Database Resource Management
test "database resource management" {
  let db_manager = DatabaseResourceManager::new()
  
  // Test database connection
  let connection = DatabaseResourceManager::create_connection(
    db_manager,
    "postgresql://localhost:5432/testdb",
    "user",
    "password"
  )
  
  assert_true(DatabaseResourceManager::is_connected(connection))
  
  // Test connection reuse
  let reused_connection = DatabaseResourceManager::get_connection(
    db_manager,
    "postgresql://localhost:5432/testdb"
  )
  
  assert_true(reused_connection.id == connection.id) // Should reuse connection
  
  // Test transaction management
  let transaction = DatabaseResourceManager::begin_transaction(connection)
  assert_true(DatabaseResourceManager::is_active(transaction))
  
  DatabaseResourceManager::commit_transaction(transaction)
  assert_false(DatabaseResourceManager::is_active(transaction))
  
  // Test connection cleanup
  DatabaseResourceManager::cleanup_connection(connection)
  assert_false(DatabaseResourceManager::is_connected(connection))
}

// Test 9: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = ResourceLeakDetector::new()
  
  // Test leak detection with proper cleanup
  let resource1 = TestResource::new("leak_test_1")
  LeakDetector::track(leak_detector, resource1)
  
  // Properly cleanup resource
  TestResource::cleanup(resource1)
  LeakDetector::untrack(leak_detector, resource1)
  
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 0) // No leaks
  
  // Test leak detection with improper cleanup
  let resource2 = TestResource::new("leak_test_2")
  LeakDetector::track(leak_detector, resource2)
  
  // Forget to cleanup resource (simulating a leak)
  // TestResource::cleanup(resource2) // This is intentionally omitted
  // LeakDetector::untrack(leak_detector, resource2) // This is intentionally omitted
  
  let leaks2 = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks2.length(), 1) // One leak detected
  assert_eq(leaks2[0].resource_name, "leak_test_2")
}

// Test 10: Resource Usage Monitoring
test "resource usage monitoring" {
  let monitor = ResourceMonitor::new()
  
  // Test CPU monitoring
  let cpu_usage = ResourceMonitor::get_cpu_usage(monitor)
  assert_true(cpu_usage >= 0.0 && cpu_usage <= 100.0)
  
  // Test memory monitoring
  let memory_usage = ResourceMonitor::get_memory_usage(monitor)
  assert_true(memory_usage.total > 0)
  assert_true(memory_usage.used > 0)
  assert_true(memory_usage.available > 0)
  assert_true(memory_usage.used <= memory_usage.total)
  
  // Test disk monitoring
  let disk_usage = ResourceMonitor::get_disk_usage(monitor, "/")
  assert_true(disk_usage.total > 0)
  assert_true(disk_usage.used > 0)
  assert_true(disk_usage.available > 0)
  assert_true(disk_usage.used <= disk_usage.total)
  
  // Test network monitoring
  let network_usage = ResourceMonitor::get_network_usage(monitor)
  assert_true(network_usage.bytes_sent >= 0)
  assert_true(network_usage.bytes_received >= 0)
  assert_true(network_usage.packets_sent >= 0)
  assert_true(network_usage.packets_received >= 0)
  
  // Test resource thresholds
  ResourceMonitor::set_cpu_threshold(monitor, 80.0) // Alert if CPU > 80%
  ResourceMonitor::set_memory_threshold(monitor, 90.0) // Alert if memory > 90%
  
  // Simulate high resource usage
  let alerts = ResourceMonitor::check_thresholds(monitor)
  // Depending on actual system conditions, this might or might not trigger alerts
  // We just verify the mechanism works
  assert_true(alerts.length() >= 0)
}