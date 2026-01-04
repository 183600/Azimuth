// Azimuth Telemetry System - Resource Management Comprehensive Tests
// This file contains comprehensive tests for resource management and cleanup

// Test 1: Memory Resource Management
test "memory resource management" {
  let memory_manager = MemoryManager::new()
  
  // Get initial memory state
  let initial_memory = MemoryManager::get_memory_usage(memory_manager)
  let initial_allocations = MemoryManager::get_allocation_count(memory_manager)
  
  // Allocate memory resources
  let allocations = []
  for i in 0..=100 {
    let allocation_id = "test_allocation_" + i.to_string()
    let allocation_size = 1024 * (i % 10 + 1) // 1KB to 10KB
    
    let allocation = MemoryManager::allocate(memory_manager, allocation_id, allocation_size)
    match allocation {
      Some(alloc) => {
        allocations = allocations.push(alloc)
        
        // Verify allocation was successful
        assert_true(MemoryManager::is_allocated(memory_manager, allocation_id))
        assert_eq(MemoryManager::get_allocation_size(memory_manager, allocation_id), allocation_size)
      }
      None => assert_true(false) // Allocation should not fail
    }
  }
  
  // Verify memory usage increased
  let after_allocation_memory = MemoryManager::get_memory_usage(memory_manager)
  let after_allocation_count = MemoryManager::get_allocation_count(memory_manager)
  
  assert_true(after_allocation_memory > initial_memory)
  assert_eq(after_allocation_count, initial_allocations + 101)
  
  // Use allocated memory
  for i in 0..=allocations.length() - 1 {
    let allocation = allocations[i]
    let test_data = "test data for allocation " + i.to_string()
    
    // Write data to allocation
    let write_result = MemoryManager::write_data(memory_manager, allocation, test_data)
    assert_true(write_result)
    
    // Read data from allocation
    let read_data = MemoryManager::read_data(memory_manager, allocation)
    assert_eq(read_data, test_data)
  }
  
  // Deallocate half of the allocations
  for i in 0..=50 {
    let allocation = allocations[i]
    let allocation_id = MemoryManager::get_allocation_id(memory_manager, allocation)
    
    let deallocation_result = MemoryManager::deallocate(memory_manager, allocation)
    assert_true(deallocation_result)
    assert_false(MemoryManager::is_allocated(memory_manager, allocation_id))
  }
  
  // Verify memory usage decreased
  let after_deallocation_memory = MemoryManager::get_memory_usage(memory_manager)
  let after_deallocation_count = MemoryManager::get_allocation_count(memory_manager)
  
  assert_true(after_deallocation_memory < after_allocation_memory)
  assert_eq(after_deallocation_count, initial_allocations + 50)
  
  // Force garbage collection
  MemoryManager::force_gc(memory_manager)
  
  // Verify no memory leaks
  let leak检测结果 = MemoryManager::detect_leaks(memory_manager)
  assert_eq(leak检测结果.leaked_allocations, 0)
  
  // Deallocate remaining allocations
  for i in 51..=allocations.length() - 1 {
    let allocation = allocations[i]
    MemoryManager::deallocate(memory_manager, allocation)
  }
  
  // Verify memory usage returned to initial state (approximately)
  let final_memory = MemoryManager::get_memory_usage(memory_manager)
  let final_count = MemoryManager::get_allocation_count(memory_manager)
  
  assert_true(final_memory < initial_memory + 1024 * 1024) // Within 1MB of initial
  assert_eq(final_count, initial_allocations)
}

// Test 2: File Handle Resource Management
test "file handle resource management" {
  let file_manager = FileManager::new()
  
  // Get initial file handle state
  let initial_handles = FileManager::get_open_handle_count(file_manager)
  
  // Create and open multiple files
  let file_handles = []
  for i in 0..=50 {
    let file_path = "/tmp/test_file_" + i.to_string() + ".tmp"
    
    let file_handle = FileManager::open_file(file_manager, file_path, "w")
    match file_handle {
      Some(handle) => {
        file_handles = file_handles.push(handle)
        
        // Verify file is open
        assert_true(FileManager::is_file_open(file_manager, file_path))
        
        // Write data to file
        let test_data = "test data for file " + i.to_string()
        let write_result = FileManager::write_file(file_manager, handle, test_data)
        assert_true(write_result)
      }
      None => assert_true(false) // File open should not fail
    }
  }
  
  // Verify file handle count increased
  let after_open_handles = FileManager::get_open_handle_count(file_manager)
  assert_eq(after_open_handles, initial_handles + 51)
  
  // Read from files
  for i in 0..=file_handles.length() - 1 {
    let handle = file_handles[i]
    
    // Reset file pointer to beginning
    FileManager::seek_file(file_manager, handle, 0)
    
    // Read data from file
    let read_data = FileManager::read_file(file_manager, handle)
    let expected_data = "test data for file " + i.to_string()
    assert_eq(read_data, expected_data)
  }
  
  // Close half of the files
  for i in 0..=25 {
    let handle = file_handles[i]
    let file_path = FileManager::get_file_path(file_manager, handle)
    
    let close_result = FileManager::close_file(file_manager, handle)
    assert_true(close_result)
    assert_false(FileManager::is_file_open(file_manager, file_path))
  }
  
  // Verify file handle count decreased
  let after_close_handles = FileManager::get_open_handle_count(file_manager)
  assert_eq(after_close_handles, initial_handles + 26)
  
  // Detect file handle leaks
  let leak检测结果 = FileManager::detect_handle_leaks(file_manager)
  assert_eq(leak检测结果.leaked_handles, 26) // 26 files still open
  
  // Close remaining files
  for i in 26..=file_handles.length() - 1 {
    let handle = file_handles[i]
    FileManager::close_file(file_manager, handle)
  }
  
  // Verify all files are closed
  let final_handles = FileManager::get_open_handle_count(file_manager)
  assert_eq(final_handles, initial_handles)
  
  // Clean up test files
  for i in 0..=50 {
    let file_path = "/tmp/test_file_" + i.to_string() + ".tmp"
    FileManager::delete_file(file_manager, file_path)
  }
}

// Test 3: Network Connection Resource Management
test "network connection resource management" {
  let connection_manager = ConnectionManager::new()
  
  // Get initial connection state
  let initial_connections = ConnectionManager::get_active_connection_count(connection_manager)
  
  // Create multiple network connections
  let connections = []
  for i in 0..=20 {
    let connection_id = "conn_" + i.to_string()
    let endpoint = "endpoint_" + i.to_string() + ".example.com"
    
    let connection = ConnectionManager::create_connection(connection_manager, connection_id, endpoint)
    match connection {
      Some(conn) => {
        connections = connections.push(conn)
        
        // Verify connection is active
        assert_true(ConnectionManager::is_connection_active(connection_manager, connection_id))
        assert_eq(ConnectionManager::get_connection_endpoint(connection_manager, connection_id), endpoint)
      }
      None => assert_true(false) // Connection creation should not fail
    }
  }
  
  // Verify connection count increased
  let after_create_connections = ConnectionManager::get_active_connection_count(connection_manager)
  assert_eq(after_create_connections, initial_connections + 21)
  
  // Use connections for data transfer
  for i in 0..=connections.length() - 1 {
    let connection = connections[i]
    
    // Send data through connection
    let test_data = "test data for connection " + i.to_string()
    let send_result = ConnectionManager::send_data(connection_manager, connection, test_data)
    assert_true(send_result)
    
    // Receive data from connection
    let received_data = ConnectionManager::receive_data(connection_manager, connection)
    assert_eq(received_data, test_data)
  }
  
  // Close half of the connections
  for i in 0..=10 {
    let connection = connections[i]
    let connection_id = ConnectionManager::get_connection_id(connection_manager, connection)
    
    let close_result = ConnectionManager::close_connection(connection_manager, connection)
    assert_true(close_result)
    assert_false(ConnectionManager::is_connection_active(connection_manager, connection_id))
  }
  
  // Verify connection count decreased
  let after_close_connections = ConnectionManager::get_active_connection_count(connection_manager)
  assert_eq(after_close_connections, initial_connections + 11)
  
  // Detect connection leaks
  let leak检测结果 = ConnectionManager::detect_connection_leaks(connection_manager)
  assert_eq(leak检测结果.leaked_connections, 11) // 11 connections still active
  
  // Close remaining connections
  for i in 11..=connections.length() - 1 {
    let connection = connections[i]
    ConnectionManager::close_connection(connection_manager, connection)
  }
  
  // Verify all connections are closed
  let final_connections = ConnectionManager::get_active_connection_count(connection_manager)
  assert_eq(final_connections, initial_connections)
}

// Test 4: Database Connection Pool Resource Management
test "database connection pool resource management" {
  let connection_pool = DatabaseConnectionPool::new("postgresql://localhost:5432/telemetry", 10)
  
  // Get initial pool state
  let initial_available = DatabaseConnectionPool::get_available_connections(connection_pool)
  let initial_in_use = DatabaseConnectionPool::get_in_use_connections(connection_pool)
  
  // Acquire connections from pool
  let acquired_connections = []
  for i in 0..=8 {
    let connection = DatabaseConnectionPool::acquire_connection(connection_pool)
    match connection {
      Some(conn) => {
        acquired_connections = acquired_connections.push(conn)
        
        // Verify connection is in use
        assert_true(DatabaseConnectionPool::is_connection_in_use(connection_pool, conn))
      }
      None => assert_true(false) // Connection acquisition should not fail
    }
  }
  
  // Verify pool state changed
  let after_acquire_available = DatabaseConnectionPool::get_available_connections(connection_pool)
  let after_acquire_in_use = DatabaseConnectionPool::get_in_use_connections(connection_pool)
  
  assert_eq(after_acquire_available, initial_available - 8)
  assert_eq(after_acquire_in_use, initial_in_use + 8)
  
  // Use connections for database operations
  for i in 0..=acquired_connections.length() - 1 {
    let connection = acquired_connections[i]
    
    // Execute query
    let query_result = DatabaseConnectionPool::execute_query(
      connection_pool, 
      connection, 
      "SELECT 1 as test_value"
    )
    match query_result {
      Some(results) => {
        // Verify query result
        assert_true(results.length() > 0)
        assert_eq(results[0]["test_value"], "1")
      }
      None => assert_true(false)
    }
  }
  
  // Release half of the connections
  for i in 0..=4 {
    let connection = acquired_connections[i]
    
    let release_result = DatabaseConnectionPool::release_connection(connection_pool, connection)
    assert_true(release_result)
    assert_false(DatabaseConnectionPool::is_connection_in_use(connection_pool, connection))
  }
  
  // Verify pool state changed
  let after_release_available = DatabaseConnectionPool::get_available_connections(connection_pool)
  let after_release_in_use = DatabaseConnectionPool::get_in_use_connections(connection_pool)
  
  assert_eq(after_release_available, initial_available - 3) // 8 acquired, 5 released
  assert_eq(after_release_in_use, initial_in_use + 3)
  
  // Test connection timeout and recovery
  for i in 5..=acquired_connections.length() - 1 {
    let connection = acquired_connections[i]
    
    // Simulate connection timeout
    DatabaseConnectionPool::simulate_connection_timeout(connection_pool, connection)
    
    // Verify connection is marked as invalid
    assert_false(DatabaseConnectionPool::is_connection_valid(connection_pool, connection))
    
    // Release invalid connection (should be discarded)
    DatabaseConnectionPool::release_connection(connection_pool, connection)
  }
  
  // Verify pool health
  let pool_health = DatabaseConnectionPool::get_pool_health(connection_pool)
  assert_true(pool_health.healthy_connections >= pool_health.total_connections * 0.8) // At least 80% healthy
  
  // Release all remaining connections
  for connection in acquired_connections {
    if DatabaseConnectionPool::is_connection_in_use(connection_pool, connection) {
      DatabaseConnectionPool::release_connection(connection_pool, connection)
    }
  }
  
  // Verify all connections are available
  let final_available = DatabaseConnectionPool::get_available_connections(connection_pool)
  let final_in_use = DatabaseConnectionPool::get_in_use_connections(connection_pool)
  
  assert_eq(final_available, initial_available)
  assert_eq(final_in_use, initial_in_use)
  
  // Close connection pool
  DatabaseConnectionPool::close(connection_pool)
  assert_true(DatabaseConnectionPool::is_closed(connection_pool))
}

// Test 5: Thread Resource Management
test "thread resource management" {
  let thread_manager = ThreadManager::new()
  
  // Get initial thread state
  let initial_threads = ThreadManager::get_active_thread_count(thread_manager)
  
  // Create multiple threads
  let threads = []
  for i in 0..=10 {
    let thread_id = "thread_" + i.to_string()
    
    let thread = ThreadManager::create_thread(thread_manager, thread_id, fn() {
      // Simulate work
      Thread::sleep(100) // 100ms
      
      // Return result
      return "thread_" + i.to_string() + "_completed"
    })
    match thread {
      Some(t) => {
        threads = threads.push(t)
        
        // Verify thread is active
        assert_true(ThreadManager::is_thread_active(thread_manager, thread_id))
      }
      None => assert_true(false) // Thread creation should not fail
    }
  }
  
  // Verify thread count increased
  let after_create_threads = ThreadManager::get_active_thread_count(thread_manager)
  assert_eq(after_create_threads, initial_threads + 11)
  
  // Wait for threads to complete
  let thread_results = []
  for i in 0..=threads.length() - 1 {
    let thread = threads[i]
    
    let result = ThreadManager::wait_for_thread(thread_manager, thread)
    match result {
      Some(thread_result) => {
        thread_results = thread_results.push(thread_result)
        
        // Verify thread is no longer active
        let thread_id = ThreadManager::get_thread_id(thread_manager, thread)
        assert_false(ThreadManager::is_thread_active(thread_manager, thread_id))
      }
      None => assert_true(false) // Thread wait should not fail
    }
  }
  
  // Verify thread results
  assert_eq(thread_results.length(), 11)
  for i in 0..=10 {
    assert_eq(thread_results[i], "thread_" + i.to_string() + "_completed")
  }
  
  // Verify thread count returned to initial
  let final_threads = ThreadManager::get_active_thread_count(thread_manager)
  assert_eq(final_threads, initial_threads)
  
  // Detect thread leaks
  let leak检测结果 = ThreadManager::detect_thread_leaks(thread_manager)
  assert_eq(leak检测结果.leaked_threads, 0)
}

// Test 6: Cache Resource Management
test "cache resource management" {
  let cache_manager = CacheManager::new(1000) // 1000 item limit
  
  // Get initial cache state
  let initial_memory_usage = CacheManager::get_memory_usage(cache_manager)
  let initial_item_count = CacheManager::get_item_count(cache_manager)
  
  // Add items to cache
  let cache_keys = []
  for i in 0..=500 {
    let key = "cache_key_" + i.to_string()
    let value = "cache_value_" + i.to_string() + "_with_additional_data_to_increase_size"
    
    let put_result = CacheManager::put(cache_manager, key, value)
    assert_true(put_result)
    
    cache_keys = cache_keys.push(key)
    
    // Verify item is in cache
    assert_true(CacheManager::contains(cache_manager, key))
  }
  
  // Verify cache state changed
  let after_put_memory_usage = CacheManager::get_memory_usage(cache_manager)
  let after_put_item_count = CacheManager::get_item_count(cache_manager)
  
  assert_true(after_put_memory_usage > initial_memory_usage)
  assert_eq(after_put_item_count, initial_item_count + 501)
  
  // Retrieve items from cache
  for i in 0..=cache_keys.length() - 1 {
    let key = cache_keys[i]
    let expected_value = "cache_value_" + i.to_string() + "_with_additional_data_to_increase_size"
    
    let retrieved_value = CacheManager::get(cache_manager, key)
    match retrieved_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false) // Value should be found
    }
  }
  
  // Remove half of the items
  for i in 0..=250 {
    let key = cache_keys[i]
    
    let remove_result = CacheManager::remove(cache_manager, key)
    assert_true(remove_result)
    assert_false(CacheManager::contains(cache_manager, key))
  }
  
  // Verify cache state changed
  let after_remove_memory_usage = CacheManager::get_memory_usage(cache_manager)
  let after_remove_item_count = CacheManager::get_item_count(cache_manager)
  
  assert_true(after_remove_memory_usage < after_put_memory_usage)
  assert_eq(after_remove_item_count, initial_item_count + 250)
  
  // Test cache eviction
  for i in 501..=1500 {
    let key = "cache_key_" + i.to_string()
    let value = "cache_value_" + i.to_string()
    
    CacheManager::put(cache_manager, key, value)
  }
  
  // Verify cache eviction occurred (item count should not exceed limit)
  let after_eviction_item_count = CacheManager::get_item_count(cache_manager)
  assert_true(after_eviction_item_count <= 1000)
  
  // Clear cache
  CacheManager::clear(cache_manager)
  
  // Verify cache is empty
  let final_memory_usage = CacheManager::get_memory_usage(cache_manager)
  let final_item_count = CacheManager::get_item_count(cache_manager)
  
  assert_true(final_memory_usage < initial_memory_usage + 1024) // Within 1KB of initial
  assert_eq(final_item_count, 0)
}

// Test 7: Lock Resource Management
test "lock resource management" {
  let lock_manager = LockManager::new()
  
  // Get initial lock state
  let initial_locks = LockManager::get_active_lock_count(lock_manager)
  
  // Create multiple locks
  let locks = []
  for i in 0..=20 {
    let lock_id = "lock_" + i.to_string()
    
    let lock = LockManager::create_lock(lock_manager, lock_id)
    match lock {
      Some(l) => {
        locks = locks.push(l)
        
        // Verify lock exists
        assert_true(LockManager::lock_exists(lock_manager, lock_id))
      }
      None => assert_true(false) // Lock creation should not fail
    }
  }
  
  // Verify lock count increased
  let after_create_locks = LockManager::get_active_lock_count(lock_manager)
  assert_eq(after_create_locks, initial_locks + 21)
  
  // Acquire locks
  for i in 0..=locks.length() - 1 {
    let lock = locks[i]
    
    let acquire_result = LockManager::acquire_lock(lock_manager, lock)
    assert_true(acquire_result)
    assert_true(LockManager::is_lock_held(lock_manager, lock))
  }
  
  // Use locks for critical sections
  for i in 0..=locks.length() - 1 {
    let lock = locks[i]
    
    // Enter critical section
    LockManager::enter_critical_section(lock_manager, lock)
    
    // Perform critical operations
    let critical_data = "critical_data_" + i.to_string()
    let processed_data = critical_data + "_processed"
    
    // Exit critical section
    LockManager::exit_critical_section(lock_manager, lock)
    
    // Verify data was processed correctly
    assert_eq(processed_data, "critical_data_" + i.to_string() + "_processed")
  }
  
  // Release half of the locks
  for i in 0..=10 {
    let lock = locks[i]
    
    let release_result = LockManager::release_lock(lock_manager, lock)
    assert_true(release_result)
    assert_false(LockManager::is_lock_held(lock_manager, lock))
  }
  
  // Verify lock state changed
  let after_release_locks = LockManager::get_active_lock_count(lock_manager)
  assert_eq(after_release_locks, initial_locks + 21) // Locks still exist, just not held
  
  // Detect deadlocks
  let deadlock_result = LockManager::detect_deadlocks(lock_manager)
  assert_false(deadlock_result.has_deadlocks)
  
  // Release remaining locks
  for i in 11..=locks.length() - 1 {
    let lock = locks[i]
    LockManager::release_lock(lock_manager, lock)
  }
  
  // Destroy locks
  for i in 0..=locks.length() - 1 {
    let lock = locks[i]
    let lock_id = LockManager::get_lock_id(lock_manager, lock)
    
    let destroy_result = LockManager::destroy_lock(lock_manager, lock)
    assert_true(destroy_result)
    assert_false(LockManager::lock_exists(lock_manager, lock_id))
  }
  
  // Verify all locks are destroyed
  let final_locks = LockManager::get_active_lock_count(lock_manager)
  assert_eq(final_locks, initial_locks)
}

// Test 8: Timer Resource Management
test "timer resource management" {
  let timer_manager = TimerManager::new()
  
  // Get initial timer state
  let initial_timers = TimerManager::get_active_timer_count(timer_manager)
  
  // Create multiple timers
  let timers = []
  for i in 0..=15 {
    let timer_id = "timer_" + i.to_string()
    let delay_ms = 100 * (i + 1) // 100ms to 1600ms
    
    let timer = TimerManager::create_timer(timer_manager, timer_id, delay_ms, fn() {
      return "timer_" + i.to_string() + "_fired"
    })
    match timer {
      Some(t) => {
        timers = timers.push(t)
        
        // Verify timer exists
        assert_true(TimerManager::timer_exists(timer_manager, timer_id))
      }
      None => assert_true(false) // Timer creation should not fail
    }
  }
  
  // Verify timer count increased
  let after_create_timers = TimerManager::get_active_timer_count(timer_manager)
  assert_eq(after_create_timers, initial_timers + 16)
  
  // Wait for shorter timers to fire
  Thread::sleep(600) // Wait for first 6 timers (100ms to 600ms)
  
  // Check which timers have fired
  let fired_timers = []
  for i in 0..=5 {
    let timer = timers[i]
    
    if TimerManager::has_fired(timer_manager, timer) {
      let result = TimerManager::get_result(timer_manager, timer)
      match result {
        Some(timer_result) => {
          fired_timers = fired_timers.push(timer_result)
          assert_eq(timer_result, "timer_" + i.to_string() + "_fired")
        }
        None => assert_true(false)
      }
    }
  }
  
  // Verify expected timers fired
  assert_eq(fired_timers.length(), 6)
  
  // Cancel remaining timers
  for i in 6..=timers.length() - 1 {
    let timer = timers[i]
    
    let cancel_result = TimerManager::cancel_timer(timer_manager, timer)
    assert_true(cancel_result)
    assert_false(TimerManager::is_active(timer_manager, timer))
  }
  
  // Verify timer state changed
  let after_cancel_timers = TimerManager::get_active_timer_count(timer_manager)
  assert_eq(after_cancel_timers, initial_timers + 6) // 6 fired, 10 cancelled
  
  // Wait for any remaining timers to complete
  Thread::sleep(2000)
  
  // Verify all timers are completed
  let final_timers = TimerManager::get_active_timer_count(timer_manager)
  assert_eq(final_timers, initial_timers)
  
  // Detect timer leaks
  let leak检测结果 = TimerManager::detect_timer_leaks(timer_manager)
  assert_eq(leak检测结果.leaked_timers, 0)
}

// Test 9: Event Resource Management
test "event resource management" {
  let event_manager = EventManager::new()
  
  // Get initial event state
  let initial_events = EventManager::get_pending_event_count(event_manager)
  
  // Create multiple events
  let events = []
  for i in 0..=25 {
    let event_id = "event_" + i.to_string()
    let event_data = "event_data_" + i.to_string()
    
    let event = EventManager::create_event(event_manager, event_id, event_data)
    match event {
      Some(e) => {
        events = events.push(e)
        
        // Verify event exists
        assert_true(EventManager::event_exists(event_manager, event_id))
      }
      None => assert_true(false) // Event creation should not fail
    }
  }
  
  // Verify event count increased
  let after_create_events = EventManager::get_pending_event_count(event_manager)
  assert_eq(after_create_events, initial_events + 26)
  
  // Process events
  let processed_events = []
  for i in 0..=events.length() - 1 {
    let event = events[i]
    
    let process_result = EventManager::process_event(event_manager, event)
    match process_result {
      Some(processed_data) => {
        processed_events = processed_events.push(processed_data)
        
        // Verify event is processed
        assert_false(EventManager::is_pending(event_manager, event))
      }
      None => assert_true(false) // Event processing should not fail
    }
  }
  
  // Verify all events were processed
  assert_eq(processed_events.length(), 26)
  for i in 0..=25 {
    assert_eq(processed_events[i], "event_data_" + i.to_string())
  }
  
  // Verify event state changed
  let after_process_events = EventManager::get_pending_event_count(event_manager)
  assert_eq(after_process_events, initial_events)
  
  // Create more events to test event queue limits
  let queue_limit = EventManager::get_queue_limit(event_manager)
  let additional_events = []
  
  for i in 0..=queue_limit {
    let event_id = "additional_event_" + i.to_string()
    let event_data = "additional_event_data_" + i.to_string()
    
    let event = EventManager::create_event(event_manager, event_id, event_data)
    match event {
      Some(e) => additional_events = additional_events.push(e)
      None => {
        // Event creation might fail due to queue limit
        // This is acceptable
      }
    }
  }
  
  // Verify queue limit is enforced
  let current_pending = EventManager::get_pending_event_count(event_manager)
  assert_true(current_pending <= queue_limit)
  
  // Process additional events
  for event in additional_events {
    EventManager::process_event(event_manager, event)
  }
  
  // Verify all events are processed
  let final_events = EventManager::get_pending_event_count(event_manager)
  assert_eq(final_events, initial_events)
  
  // Detect event leaks
  let leak检测结果 = EventManager::detect_event_leaks(event_manager)
  assert_eq(leak检测结果.leaked_events, 0)
}

// Test 10: Comprehensive Resource Cleanup
test "comprehensive resource cleanup" {
  let resource_cleanup_manager = ResourceCleanupManager::new()
  
  // Register various resource managers
  let memory_manager = MemoryManager::new()
  let file_manager = FileManager::new()
  let connection_manager = ConnectionManager::new()
  let thread_manager = ThreadManager::new()
  let cache_manager = CacheManager::new(100)
  let lock_manager = LockManager::new()
  let timer_manager = TimerManager::new()
  let event_manager = EventManager::new()
  
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "memory", memory_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "file", file_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "connection", connection_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "thread", thread_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "cache", cache_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "lock", lock_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "timer", timer_manager)
  ResourceCleanupManager::register_manager(resource_cleanup_manager, "event", event_manager)
  
  // Create resources in each manager
  let memory_allocations = []
  for i in 0..=10 {
    let allocation = MemoryManager::allocate(memory_manager, "alloc_" + i.to_string(), 1024)
    match allocation {
      Some(alloc) => memory_allocations = memory_allocations.push(alloc)
      None => assert_true(false)
    }
  }
  
  let file_handles = []
  for i in 0..=5 {
    let handle = FileManager::open_file(file_manager, "/tmp/cleanup_test_" + i.to_string() + ".tmp", "w")
    match handle {
      Some(h) => file_handles = file_handles.push(h)
      None => assert_true(false)
    }
  }
  
  let connections = []
  for i in 0..=3 {
    let conn = ConnectionManager::create_connection(connection_manager, "conn_" + i.to_string(), "endpoint_" + i.to_string())
    match conn {
      Some(c) => connections = connections.push(c)
      None => assert_true(false)
    }
  }
  
  let threads = []
  for i in 0..=2 {
    let thread = ThreadManager::create_thread(thread_manager, "cleanup_thread_" + i.to_string(), fn() {
      Thread::sleep(1000) // 1 second
      return "thread_" + i.to_string() + "_completed"
    })
    match thread {
      Some(t) => threads = threads.push(t)
      None => assert_true(false)
    }
  }
  
  for i in 0..=20 {
    CacheManager::put(cache_manager, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  let locks = []
  for i in 0..=5 {
    let lock = LockManager::create_lock(lock_manager, "cleanup_lock_" + i.to_string())
    match lock {
      Some(l) => {
        LockManager::acquire_lock(lock_manager, l)
        locks = locks.push(l)
      }
      None => assert_true(false)
    }
  }
  
  let timers = []
  for i in 0..=3 {
    let timer = TimerManager::create_timer(timer_manager, "cleanup_timer_" + i.to_string(), 5000, fn() {
      return "timer_" + i.to_string() + "_fired"
    })
    match timer {
      Some(t) => timers = timers.push(t)
      None => assert_true(false)
    }
  }
  
  let events = []
  for i in 0..=10 {
    let event = EventManager::create_event(event_manager, "cleanup_event_" + i.to_string(), "event_data_" + i.to_string())
    match event {
      Some(e) => events = events.push(e)
      None => assert_true(false)
    }
  }
  
  // Verify resources are created
  assert_eq(MemoryManager::get_allocation_count(memory_manager), 11)
  assert_eq(FileManager::get_open_handle_count(file_manager), 6)
  assert_eq(ConnectionManager::get_active_connection_count(connection_manager), 4)
  assert_eq(ThreadManager::get_active_thread_count(thread_manager), 3)
  assert_eq(CacheManager::get_item_count(cache_manager), 21)
  assert_eq(LockManager::get_active_lock_count(lock_manager), 6)
  assert_eq(TimerManager::get_active_timer_count(timer_manager), 4)
  assert_eq(EventManager::get_pending_event_count(event_manager), 11)
  
  // Perform comprehensive resource cleanup
  let cleanup_result = ResourceCleanupManager::cleanup_all(resource_cleanup_manager)
  assert_true(cleanup_result.success)
  
  // Wait for threads to complete
  for thread in threads {
    ThreadManager::wait_for_thread(thread_manager, thread)
  }
  
  // Verify all resources are cleaned up
  assert_eq(MemoryManager::get_allocation_count(memory_manager), 0)
  assert_eq(FileManager::get_open_handle_count(file_manager), 0)
  assert_eq(ConnectionManager::get_active_connection_count(connection_manager), 0)
  assert_eq(ThreadManager::get_active_thread_count(thread_manager), 0)
  assert_eq(CacheManager::get_item_count(cache_manager), 0)
  assert_eq(LockManager::get_active_lock_count(lock_manager), 0)
  assert_eq(TimerManager::get_active_timer_count(timer_manager), 0)
  assert_eq(EventManager::get_pending_event_count(event_manager), 0)
  
  // Verify cleanup statistics
  let cleanup_stats = ResourceCleanupManager::get_cleanup_statistics(resource_cleanup_manager)
  assert_eq(cleanup_stats.memory_allocations_cleaned, 11)
  assert_eq(cleanup_stats.file_handles_cleaned, 6)
  assert_eq(cleanup_stats.connections_cleaned, 4)
  assert_eq(cleanup_stats.threads_cleaned, 3)
  assert_eq(cleanup_stats.cache_items_cleaned, 21)
  assert_eq(cleanup_stats.locks_cleaned, 6)
  assert_eq(cleanup_stats.timers_cleaned, 4)
  assert_eq(cleanup_stats.events_cleaned, 11)
  
  // Verify no resource leaks
  let leak检测结果 = ResourceCleanupManager::detect_all_leaks(resource_cleanup_manager)
  assert_eq(leak检测结果.total_leaks, 0)
}