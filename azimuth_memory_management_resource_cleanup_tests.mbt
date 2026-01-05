// Azimuth Memory Management and Resource Cleanup Test Suite
// This file contains test cases for memory management and resource cleanup

// Test 1: Memory Pool Management
test "memory pool management" {
  // Test memory pool creation
  let pool_config = MemoryPoolConfig::new(1024 * 1024) // 1MB pool
  let memory_pool = MemoryPool::with_config(pool_config)
  
  assert_eq(MemoryPool::total_size(memory_pool), 1024 * 1024)
  assert_eq(MemoryPool::used_size(memory_pool), 0)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024)
  
  // Test memory allocation
  let allocation1 = MemoryPool::allocate(memory_pool, 1024) // 1KB
  assert_true(MemoryPool::is_allocation_valid(memory_pool, allocation1))
  assert_eq(MemoryPool::used_size(memory_pool), 1024)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024 - 1024)
  
  let allocation2 = MemoryPool::allocate(memory_pool, 2048) // 2KB
  assert_true(MemoryPool::is_allocation_valid(memory_pool, allocation2))
  assert_eq(MemoryPool::used_size(memory_pool), 1024 + 2048)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024 - 1024 - 2048)
  
  // Test memory deallocation
  MemoryPool::deallocate(memory_pool, allocation1)
  assert_false(MemoryPool::is_allocation_valid(memory_pool, allocation1))
  assert_eq(MemoryPool::used_size(memory_pool), 2048)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024 - 2048)
  
  // Test memory reuse
  let allocation3 = MemoryPool::allocate(memory_pool, 1024) // Should reuse freed space
  assert_true(MemoryPool::is_allocation_valid(memory_pool, allocation3))
  assert_eq(MemoryPool::used_size(memory_pool), 2048 + 1024)
  
  // Test pool fragmentation
  let small_allocations = []
  for i = 0; i < 100; i = i + 1 {
    let allocation = MemoryPool::allocate(memory_pool, 64) // 64 bytes each
    small_allocations.push(allocation)
  }
  
  // Deallocate every other allocation to create fragmentation
  for i = 0; i < small_allocations.length(); i = i + 2 {
    MemoryPool::deallocate(memory_pool, small_allocations[i])
  }
  
  // Test defragmentation
  let fragmentation_before = MemoryPool::fragmentation_ratio(memory_pool)
  MemoryPool::defragment(memory_pool)
  let fragmentation_after = MemoryPool::fragmentation_ratio(memory_pool)
  
  assert_true(fragmentation_after <= fragmentation_before)
  
  // Test pool statistics
  let stats = MemoryPool::get_statistics(memory_pool)
  assert_eq(MemoryStats::total_allocations(stats), 102) // allocation1, allocation2, allocation3, and 100 small allocations
  assert_eq(MemoryStats::active_allocations(stats), 52) // allocation2, allocation3, and 50 small allocations
  assert_eq(MemoryStats::peak_usage(stats), 1024 + 2048 + 64 * 100)
  
  // Clean up remaining allocations
  MemoryPool::deallocate(memory_pool, allocation2)
  MemoryPool::deallocate(memory_pool, allocation3)
  for allocation in small_allocations {
    if MemoryPool::is_allocation_valid(memory_pool, allocation) {
      MemoryPool::deallocate(memory_pool, allocation)
    }
  }
  
  assert_eq(MemoryPool::used_size(memory_pool), 0)
  assert_eq(MemoryPool::available_size(memory_pool), 1024 * 1024)
}

// Test 2: Object Pool Management
test "object pool management" {
  // Test object pool creation
  let object_pool = ObjectPool::new(|| TelemetrySpan::new(), 100) // Max 100 objects
  
  assert_eq(ObjectPool::size(object_pool), 0)
  assert_eq(ObjectPool::capacity(object_pool), 100)
  
  // Test object acquisition
  let span1 = ObjectPool::acquire(object_pool)
  assert_true(Option::is_some(span1))
  assert_eq(ObjectPool::size(object_pool), 0) // Pool is empty
  
  let span2 = ObjectPool::acquire(object_pool)
  assert_true(Option::is_some(span2))
  assert_eq(ObjectPool::size(object_pool), 0) // Pool is still empty
  
  // Test object release
  ObjectPool::release(object_pool, Option::unwrap(span1))
  assert_eq(ObjectPool::size(object_pool), 1)
  
  ObjectPool::release(object_pool, Option::unwrap(span2))
  assert_eq(ObjectPool::size(object_pool), 2)
  
  // Test object reuse
  let span3 = ObjectPool::acquire(object_pool)
  assert_true(Option::is_some(span3))
  assert_eq(ObjectPool::size(object_pool), 1) // One object left in pool
  
  let span4 = ObjectPool::acquire(object_pool)
  assert_true(Option::is_some(span4))
  assert_eq(ObjectPool::size(object_pool), 0) // Pool is empty again
  
  // Test pool capacity limits
  let spans = []
  for i = 0; i < 150; i = i + 1 {
    let span = ObjectPool::acquire(object_pool)
    spans.push(span)
  }
  
  // Release all spans
  for span in spans {
    match span {
      Some(s) => ObjectPool::release(object_pool, s),
      None => ()
    }
  }
  
  // Pool should not exceed capacity
  assert_eq(ObjectPool::size(object_pool), 100)
  
  // Test object reset on release
  let span5 = ObjectPool::acquire(object_pool)
  match span5 {
    Some(s) => {
      TelemetrySpan::set_name(s, "test.span")
      TelemetrySpan::set_attribute(s, "test.attr", "test.value")
      
      ObjectPool::release(object_pool, s)
      
      // Reacquire to verify reset
      let span6 = ObjectPool::acquire(object_pool)
      match span6 {
        Some(reset_span) => {
          assert_eq(TelemetrySpan::name(reset_span), "") // Should be reset
          assert_eq(TelemetrySpan::get_attribute(reset_span, "test.attr"), None) // Should be reset
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test pool statistics
  let stats = ObjectPool::get_statistics(object_pool)
  assert_eq(ObjectPoolStats::total_acquisitions(stats), 152) // 150 + 2 earlier
  assert_eq(ObjectPoolStats::total_releases(stats), 152)
  assert_eq(ObjectPoolStats::pool_hits(stats), 100) // Objects from pool
  assert_eq(ObjectPoolStats::pool_misses(stats), 52) // New objects created
}

// Test 3: Resource Reference Management
test "resource reference management" {
  // Test resource creation with reference counting
  let resource = Resource::new("test_resource", 1024) // 1KB resource
  assert_eq(Resource::ref_count(resource), 1)
  
  // Test reference increment
  Resource::add_ref(resource)
  assert_eq(Resource::ref_count(resource), 2)
  
  Resource::add_ref(resource)
  assert_eq(Resource::ref_count(resource), 3)
  
  // Test reference decrement
  Resource::release(resource)
  assert_eq(Resource::ref_count(resource), 2)
  
  Resource::release(resource)
  assert_eq(Resource::ref_count(resource), 1)
  
  // Test resource cleanup
  let cleanup_called = RefCell::new(false)
  let resource_with_cleanup = Resource::with_cleanup(
    "test_resource_with_cleanup",
    1024,
    || cleanup_called.set(true)
  )
  
  assert_eq(Resource::ref_count(resource_with_cleanup), 1)
  assert_false(cleanup_called.borrow())
  
  Resource::release(resource_with_cleanup)
  assert_true(cleanup_called.borrow()) // Cleanup should be called when ref count reaches 0
  
  // Test weak references
  let strong_resource = Resource::new("strong_resource", 2048)
  let weak_resource = Resource::downgrade(strong_resource)
  
  assert_true(Resource::is_valid(weak_resource))
  
  // Upgrade weak reference
  let upgraded_resource = Resource::upgrade(weak_resource)
  assert_true(Option::is_some(upgraded_resource))
  assert_eq(Resource::ref_count(strong_resource), 2) // Original + upgraded
  
  // Drop strong reference
  Resource::release(strong_resource)
  assert_eq(Resource::ref_count(Option::unwrap(upgraded_resource)), 1)
  
  // Drop upgraded reference
  Resource::release(Option::unwrap(upgraded_resource))
  
  // Weak reference should now be invalid
  assert_false(Resource::is_valid(weak_resource))
  
  // Test cyclic reference detection
  let resource_a = Resource::new("resource_a", 1024)
  let resource_b = Resource::new("resource_b", 1024)
  
  // Create a cycle
  Resource::set_reference(resource_a, "b", resource_b);
  Resource::set_reference(resource_b, "a", resource_a);
  
  // Both resources should have ref count of 2 (1 external + 1 from the other resource)
  assert_eq(Resource::ref_count(resource_a), 2)
  assert_eq(Resource::ref_count(resource_b), 2)
  
  // Release external references
  Resource::release(resource_a)
  Resource::release(resource_b)
  
  // Resources should still have ref count of 1 due to the cycle
  assert_eq(Resource::ref_count(resource_a), 1)
  assert_eq(Resource::ref_count(resource_b), 1)
  
  // Break the cycle
  Resource::clear_reference(resource_a, "b");
  
  // Now both resources should be cleaned up
  assert_eq(Resource::ref_count(resource_a), 0)
  assert_eq(Resource::ref_count(resource_b), 0)
}

// Test 4: Garbage Collection
test "garbage collection" {
  // Test garbage collector creation
  let gc_config = GCConfig::new()
  GCConfig::set_threshold(gc_config, 1024 * 1024) // 1MB threshold
  GCConfig::set_max_heap_size(gc_config, 10 * 1024 * 1024) // 10MB max heap
  
  let garbage_collector = GarbageCollector::with_config(gc_config)
  
  // Test object allocation
  let objects = []
  for i = 0; i < 1000; i = i + 1 {
    let object = LargeObject::new(1024) // 1KB each
    objects.push(object)
  }
  
  let heap_size_before_gc = GarbageCollector::heap_size(garbage_collector)
  assert_true(heap_size_before_gc >= 1024 * 1000) // At least 1000KB
  
  // Release references to some objects
  for i = 0; i < 500; i = i + 1 {
    objects[i] = None
  }
  
  // Run garbage collection
  GarbageCollector::collect(garbage_collector)
  
  let heap_size_after_gc = GarbageCollector::heap_size(garbage_collector)
  assert_true(heap_size_after_gc < heap_size_before_gc) // Heap should have shrunk
  
  // Test generational garbage collection
  let generational_gc = GenerationalGC::new()
  
  // Create young generation objects
  let young_objects = []
  for i = 0; i < 100; i = i + 1 {
    let object = SmallObject::new(64) // 64 bytes each
    young_objects.push(object)
  }
  
  // Create old generation objects (simulate long-lived objects)
  let old_objects = []
  for i = 0; i < 10; i = i + 1 {
    let object = LargeObject::new(2048) // 2KB each
    old_objects.push(object)
    
    // Simulate multiple GC cycles to promote to old generation
    for j = 0; j < 10; j = j + 1 {
      GenerationalGC::minor_collect(generational_gc)
    }
  }
  
  let young_gen_size_before = GenerationalGC::young_generation_size(generational_gc)
  let old_gen_size_before = GenerationalGC::old_generation_size(generational_gc)
  
  // Release some young objects
  for i = 0; i < 50; i = i + 1 {
    young_objects[i] = None
  }
  
  // Run minor GC (should only collect young generation)
  GenerationalGC::minor_collect(generational_gc)
  
  let young_gen_size_after = GenerationalGC::young_generation_size(generational_gc)
  let old_gen_size_after = GenerationalGC::old_generation_size(generational_gc)
  
  assert_true(young_gen_size_after < young_gen_size_before) // Young gen should have shrunk
  assert_eq(old_gen_size_after, old_gen_size_before) // Old gen should be unchanged
  
  // Test GC statistics
  let gc_stats = GarbageCollector::get_statistics(garbage_collector)
  assert_eq(GCStats::total_collections(gc_stats), 1)
  assert_eq(GCStats::total_bytes_freed(gc_stats), heap_size_before_gc - heap_size_after_gc)
  assert_true(GCStats::total_collection_time(gc_stats) >= 0)
  
  // Test GC tuning
  GCConfig::set_threshold(gc_config, 512 * 1024) // Lower threshold
  GCConfig::set_max_heap_size(gc_config, 5 * 1024 * 1024) // Smaller max heap
  
  GarbageCollector::update_config(garbage_collector, gc_config)
  
  let updated_config = GarbageCollector::get_config(garbage_collector)
  assert_eq(GCConfig::threshold(updated_config), 512 * 1024)
  assert_eq(GCConfig::max_heap_size(updated_config), 5 * 1024 * 1024)
}

// Test 5: Buffer Management
test "buffer management" {
  // Test buffer creation
  let buffer = Buffer::new(1024) // 1KB buffer
  assert_eq(Buffer::capacity(buffer), 1024)
  assert_eq(Buffer::size(buffer), 0)
  
  // Test buffer writing
  let data = "Hello, World!"
  let bytes_written = Buffer::write(buffer, data)
  assert_eq(bytes_written, String::length(data))
  assert_eq(Buffer::size(buffer), String::length(data))
  
  // Test buffer reading
  let read_data = Buffer::read(buffer, String::length(data))
  assert_eq(read_data, data)
  assert_eq(Buffer::position(buffer), String::length(data))
  
  // Test buffer seeking
  Buffer::seek(buffer, 0)
  assert_eq(Buffer::position(buffer), 0)
  
  // Test buffer expansion
  let large_data = "A".repeat(2048) // 2KB of data
  let bytes_written_large = Buffer::write(buffer, large_data)
  assert_eq(bytes_written_large, 2048)
  assert_eq(Buffer::capacity(buffer), 1024 + 2048) // Should have expanded
  assert_eq(Buffer::size(buffer), String::length(data) + 2048)
  
  // Test buffer clearing
  Buffer::clear(buffer)
  assert_eq(Buffer::size(buffer), 0)
  assert_eq(Buffer::position(buffer), 0)
  assert_eq(Buffer::capacity(buffer), 1024 + 2048) // Capacity remains the same
  
  // Test buffer pool
  let buffer_pool = BufferPool::new(10, 1024) // 10 buffers of 1KB each
  
  let buffer1 = BufferPool::acquire(buffer_pool)
  assert_true(Option::is_some(buffer1))
  assert_eq(BufferPool::available_buffers(buffer_pool), 9)
  
  let buffer2 = BufferPool::acquire(buffer_pool)
  assert_true(Option::is_some(buffer2))
  assert_eq(BufferPool::available_buffers(buffer_pool), 8)
  
  // Release buffers back to pool
  BufferPool::release(buffer_pool, Option::unwrap(buffer1))
  assert_eq(BufferPool::available_buffers(buffer_pool), 9)
  
  BufferPool::release(buffer_pool, Option::unwrap(buffer2))
  assert_eq(BufferPool::available_buffers(buffer_pool), 10)
  
  // Test buffer pool exhaustion
  let buffers = []
  for i = 0; i < 15; i = i + 1 {
    let buffer = BufferPool::acquire(buffer_pool)
    buffers.push(buffer)
  }
  
  // First 10 should be from pool, next 5 should be newly allocated
  assert_eq(BufferPool::available_buffers(buffer_pool), 0)
  assert_eq(BufferPool::total_buffers(buffer_pool), 15)
  
  // Test zero-copy buffer operations
  let source_buffer = Buffer::new(1024)
  Buffer::write(source_buffer, "Source data")
  
  let destination_buffer = Buffer::new(1024)
  
  // Zero-copy transfer
  Buffer::transfer(source_buffer, destination_buffer, 11) // Transfer "Source data"
  
  assert_eq(Buffer::size(destination_buffer), 11)
  assert_eq(Buffer::read(destination_buffer, 11), "Source data")
  
  // Test buffer statistics
  let buffer_stats = BufferPool::get_statistics(buffer_pool)
  assert_eq(BufferPoolStats::total_acquisitions(buffer_stats), 15)
  assert_eq(BufferPoolStats::total_releases(buffer_stats), 2)
  assert_eq(BufferPoolStats::pool_hits(buffer_stats), 10)
  assert_eq(BufferPoolStats::pool_misses(buffer_stats), 5)
}

// Test 6: File Handle Management
test "file handle management" {
  // Test file handle creation
  let file_manager = FileManager::new()
  
  let file1 = FileManager::open(file_manager, "test1.txt", FileMode::Read)
  assert_true(FileManager::is_handle_valid(file_manager, file1))
  assert_eq(FileManager::open_handles_count(file_manager), 1)
  
  let file2 = FileManager::open(file_manager, "test2.txt", FileMode::Write)
  assert_true(FileManager::is_handle_valid(file_manager, file2))
  assert_eq(FileManager::open_handles_count(file_manager), 2)
  
  // Test file handle reuse
  FileManager::close(file_manager, file1)
  assert_false(FileManager::is_handle_valid(file_manager, file1))
  assert_eq(FileManager::open_handles_count(file_manager), 1)
  
  let file3 = FileManager::open(file_manager, "test3.txt", FileMode::Read)
  assert_true(FileManager::is_handle_valid(file_manager, file3))
  assert_eq(FileManager::open_handles_count(file_manager), 2)
  
  // Test file handle limit
  let file_config = FileManagerConfig::new()
  FileManagerConfig::set_max_open_files(file_config, 5)
  FileManager::update_config(file_manager, file_config)
  
  let files = []
  for i = 0; i < 10; i = i + 1 {
    let file = FileManager::open(file_manager, "test" + i.to_string() + ".txt", FileMode::Read)
    files.push(file)
  }
  
  // Should not exceed the limit
  assert_true(FileManager::open_handles_count(file_manager) <= 5)
  
  // Test file handle caching
  FileManager::enable_caching(file_manager, true)
  
  let cached_file = FileManager::open(file_manager, "cached.txt", FileMode::Read)
  FileManager::close(file_manager, cached_file)
  
  // Reopen the same file
  let reopened_file = FileManager::open(file_manager, "cached.txt", FileMode::Read)
  assert_true(FileManager::is_cached(file_manager, reopened_file))
  
  // Test file handle cleanup on scope exit
  let cleanup_tracker = RefCell::new(0)
  
  {
    let scoped_file = FileManager::open_with_cleanup(
      file_manager,
      "scoped.txt",
      FileMode::Read,
      || cleanup_tracker.set(cleanup_tracker.borrow() + 1)
    )
    
    assert_true(FileManager::is_handle_valid(file_manager, scoped_file))
    assert_eq(cleanup_tracker.borrow(), 0)
  } // scoped_file goes out of scope here
    
  assert_eq(cleanup_tracker.borrow(), 1) // Cleanup should have been called
  
  // Test file handle statistics
  let file_stats = FileManager::get_statistics(file_manager)
  assert_eq(FileManagerStats::total_opens(file_stats), 13) // 2 initial + 10 + 1 cached
  assert_eq(FileManagerStats::total_closes(file_stats), 4) // 1 initial + 1 scoped + 2 from limit enforcement
  assert_eq(FileManagerStats::peak_open_handles(file_stats), 5) // Max limit
}

// Test 7: Network Connection Management
test "network connection management" {
  // Test connection pool creation
  let connection_pool_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_max_connections(connection_pool_config, 10)
  ConnectionPoolConfig::set_connection_timeout(connection_pool_config, 5000) // 5 seconds
  ConnectionPoolConfig::set_idle_timeout(connection_pool_config, 30000) // 30 seconds
  
  let connection_pool = ConnectionPool::with_config(connection_pool_config)
  
  // Test connection acquisition
  let connection1 = ConnectionPool::get_connection(connection_pool, "https://example.com")
  assert_true(Connection::is_valid(connection1))
  assert_eq(ConnectionPool::active_connections(connection_pool), 1)
  
  let connection2 = ConnectionPool::get_connection(connection_pool, "https://example.com")
  assert_true(Connection::is_valid(connection2))
  assert_eq(ConnectionPool::active_connections(connection_pool), 2)
  
  // Test connection reuse
  ConnectionPool::release_connection(connection_pool, connection1)
  assert_eq(ConnectionPool::active_connections(connection_pool), 1)
  assert_eq(ConnectionPool::idle_connections(connection_pool), 1)
  
  let connection3 = ConnectionPool::get_connection(connection_pool, "https://example.com")
  assert_true(Connection::is_valid(connection3))
  assert_eq(ConnectionPool::active_connections(connection_pool), 2)
  assert_eq(ConnectionPool::idle_connections(connection_pool), 0) // Should reuse idle connection
  
  // Test connection limit
  let connections = []
  for i = 0; i < 15; i = i + 1 {
    let connection = ConnectionPool::get_connection(connection_pool, "https://example" + i.to_string() + ".com")
    connections.push(connection)
  }
  
  // Should not exceed the limit
  assert_true(ConnectionPool::active_connections(connection_pool) <= 10)
  
  // Test connection health checking
  let unhealthy_connection = connections[0]
  Connection::simulate_failure(unhealthy_connection)
  
  let is_healthy = ConnectionPool::is_connection_healthy(connection_pool, unhealthy_connection)
  assert_false(is_healthy)
  
  // Test connection cleanup
  ConnectionPool::cleanup_unhealthy(connection_pool)
  assert_true(ConnectionPool::active_connections(connection_pool) < 15)
  
  // Test connection statistics
  let pool_stats = ConnectionPool::get_statistics(connection_pool)
  assert_eq(ConnectionPoolStats::total_connections_created(pool_stats), 10)
  assert_eq(ConnectionPoolStats::connection_reuse_count(pool_stats), 1)
  assert_eq(ConnectionPoolStats::connection_failures(pool_stats), 1)
}

// Test 8: Event Listener Management
test "event listener management" {
  // Test event system creation
  let event_system = EventSystem::new()
  
  // Test event listener registration
  let listener1_id = EventSystem::register_listener(event_system, "test.event", |event| {
    assert_eq(Event::type(event), "test.event")
    assert_eq(Event::data(event), "test_data")
  })
  
  let listener2_id = EventSystem::register_listener(event_system, "test.event", |event| {
    assert_eq(Event::type(event), "test.event")
    assert_eq(Event::data(event), "test_data")
  })
  
  // Test event dispatch
  let test_event = Event::new("test.event", "test_data")
  EventSystem::dispatch(event_system, test_event)
  
  // Test listener removal
  EventSystem::remove_listener(event_system, listener1_id)
  
  // Dispatch again - only listener2 should be called
  EventSystem::dispatch(event_system, test_event)
  
  // Test once-only listeners
  let once_listener_id = EventSystem::register_once_listener(event_system, "test.event", |event| {
    assert_eq(Event::type(event), "test.event")
    assert_eq(Event::data(event), "test_data")
  })
  
  // Dispatch twice - once listener should only be called once
  EventSystem::dispatch(event_system, test_event)
  EventSystem::dispatch(event_system, test_event)
  
  // Test wildcard listeners
  let wildcard_listener_id = EventSystem::register_listener(event_system, "*", |event| {
    assert_true(Event::type(event) == "test.event" || Event::type(event) == "other.event")
  })
  
  let other_event = Event::new("other.event", "other_data")
  EventSystem::dispatch(event_system, other_event)
  
  // Test event system cleanup
  EventSystem::remove_listener(event_system, listener2_id)
  EventSystem::remove_listener(event_system, wildcard_listener_id)
  
  // All listeners should be removed
  assert_eq(EventSystem::listener_count(event_system), 0)
  
  // Test event system statistics
  let event_stats = EventSystem::get_statistics(event_system)
  assert_eq(EventSystemStats::total_events_dispatched(event_stats), 5)
  assert_eq(EventSystemStats::total_listeners_registered(event_stats), 4)
  assert_eq(EventSystemStats::total_listeners_removed(event_stats), 4)
}

// Test 9: Timer and Scheduler Management
test "timer and scheduler management" {
  // Test timer creation
  let timer_manager = TimerManager::new()
  
  let timer1_id = TimerManager::set_timer(timer_manager, 1000, || {
    // Timer callback
  })
  
  let timer2_id = TimerManager::set_timer(timer_manager, 2000, || {
    // Timer callback
  })
  
  // Test timer status
  assert_true(TimerManager::is_timer_active(timer_manager, timer1_id))
  assert_true(TimerManager::is_timer_active(timer_manager, timer2_id))
  
  // Test timer cancellation
  TimerManager::cancel_timer(timer_manager, timer1_id)
  assert_false(TimerManager::is_timer_active(timer_manager, timer1_id))
  assert_true(TimerManager::is_timer_active(timer_manager, timer2_id))
  
  // Test repeating timer
  let repeat_timer_id = TimerManager::set_repeating_timer(timer_manager, 500, || {
    // Repeating timer callback
  })
  
  assert_true(TimerManager::is_timer_active(timer_manager, repeat_timer_id))
  
  // Cancel after a few executions (simulated)
  TimerManager::cancel_timer(timer_manager, repeat_timer_id)
  assert_false(TimerManager::is_timer_active(timer_manager, repeat_timer_id))
  
  // Test scheduler creation
  let scheduler = Scheduler::new()
  
  // Test task scheduling
  let task1_id = Scheduler::schedule_task(scheduler, || {
    // Task 1
  }, 1000) // Execute after 1 second
  
  let task2_id = Scheduler::schedule_task(scheduler, || {
    // Task 2
  }, 2000) // Execute after 2 seconds
  
  // Test task status
  assert_true(Scheduler::is_task_pending(scheduler, task1_id))
  assert_true(Scheduler::is_task_pending(scheduler, task2_id))
  
  // Test task cancellation
  Scheduler::cancel_task(scheduler, task1_id)
  assert_false(Scheduler::is_task_pending(scheduler, task1_id))
  assert_true(Scheduler::is_task_pending(scheduler, task2_id))
  
  // Test periodic task scheduling
  let periodic_task_id = Scheduler::schedule_periodic_task(scheduler, || {
    // Periodic task
  }, 1000, 5000) // Start after 1 second, repeat every 5 seconds
  
  assert_true(Scheduler::is_task_pending(scheduler, periodic_task_id))
  
  // Cancel periodic task
  Scheduler::cancel_task(scheduler, periodic_task_id)
  assert_false(Scheduler::is_task_pending(scheduler, periodic_task_id))
  
  // Test timer and scheduler cleanup
  TimerManager::cleanup(timer_manager)
  Scheduler::cleanup(scheduler)
  
  // Test timer statistics
  let timer_stats = TimerManager::get_statistics(timer_manager)
  assert_eq(TimerStats::total_timers_created(timer_stats), 4)
  assert_eq(TimerStats::total_timers_cancelled(timer_stats), 4)
  
  // Test scheduler statistics
  let scheduler_stats = Scheduler::get_statistics(scheduler)
  assert_eq(SchedulerStats::total_tasks_scheduled(scheduler_stats), 4)
  assert_eq(SchedulerStats::total_tasks_cancelled(scheduler_stats), 4)
}

// Test 10: Resource Leak Detection
test "resource leak detection" {
  // Test leak detector creation
  let leak_detector = LeakDetector::new()
  
  // Enable leak detection
  LeakDetector::enable(leak_detector, true)
  
  // Test memory leak detection
  LeakDetector::track_allocation(leak_detector, "test_allocation", 1024)
  
  let allocations = LeakDetector::get_allocations(leak_detector)
  assert_eq(allocations.length(), 1)
  assert_eq(allocations[0].type, "test_allocation")
  assert_eq(allocations[0].size, 1024)
  
  // Simulate a leak by not deallocating
  // In a real scenario, this would be detected at program exit
  
  LeakDetector::track_deallocation(leak_detector, "test_allocation", 1024)
  
  let allocations_after_dealloc = LeakDetector::get_allocations(leak_detector)
  assert_eq(allocations_after_dealloc.length(), 0)
  
  // Test file handle leak detection
  LeakDetector::track_file_open(leak_detector, "test_file.txt")
  
  let open_files = LeakDetector::get_open_files(leak_detector)
  assert_eq(open_files.length(), 1)
  assert_eq(open_files[0], "test_file.txt")
  
  // Simulate a leak by not closing the file
  // In a real scenario, this would be detected at program exit
  
  LeakDetector::track_file_close(leak_detector, "test_file.txt")
  
  let open_files_after_close = LeakDetector::get_open_files(leak_detector)
  assert_eq(open_files_after_close.length(), 0)
  
  // Test network connection leak detection
  LeakDetector::track_connection_open(leak_detector, "https://example.com")
  
  let open_connections = LeakDetector::get_open_connections(leak_detector)
  assert_eq(open_connections.length(), 1)
  assert_eq(open_connections[0], "https://example.com")
  
  // Simulate a leak by not closing the connection
  // In a real scenario, this would be detected at program exit
  
  LeakDetector::track_connection_close(leak_detector, "https://example.com")
  
  let open_connections_after_close = LeakDetector::get_open_connections(leak_detector)
  assert_eq(open_connections_after_close.length(), 0)
  
  // Test event listener leak detection
  LeakDetector::track_listener_registration(leak_detector, "test.event", "listener_id")
  
  let active_listeners = LeakDetector::get_active_listeners(leak_detector)
  assert_eq(active_listeners.length(), 1)
  assert_eq(active_listeners[0].event_type, "test.event")
  assert_eq(active_listeners[0].listener_id, "listener_id")
  
  // Simulate a leak by not unregistering the listener
  // In a real scenario, this would be detected at program exit
  
  LeakDetector::track_listener_unregistration(leak_detector, "test.event", "listener_id")
  
  let active_listeners_after_unregister = LeakDetector::get_active_listeners(leak_detector)
  assert_eq(active_listeners_after_unregister.length(), 0)
  
  // Test leak report generation
  // Simulate some leaks
  LeakDetector::track_allocation(leak_detector, "leaked_memory", 2048)
  LeakDetector::track_file_open(leak_detector, "leaked_file.txt")
  LeakDetector::track_connection_open(leak_detector, "https://leaked.example.com")
  LeakDetector::track_listener_registration(leak_detector, "leaked.event", "leaked_listener")
  
  let leak_report = LeakDetector::generate_report(leak_detector)
  
  assert_eq(LeakReport::memory_leaks(leak_report).length(), 1)
  assert_eq(LeakReport::file_handle_leaks(leak_report).length(), 1)
  assert_eq(LeakReport::connection_leaks(leak_report).length(), 1)
  assert_eq(LeakReport::listener_leaks(leak_report).length(), 1)
  
  // Test automatic leak detection
  LeakDetector::enable_automatic_detection(leak_detector, true)
  LeakDetector::set_detection_interval(leak_detector, 5000) // Check every 5 seconds
  
  // In a real scenario, the detector would automatically check for leaks at intervals
  let automatic_detection_enabled = LeakDetector::is_automatic_detection_enabled(leak_detector)
  assert_true(automatic_detection_enabled)
  
  let detection_interval = LeakDetector::get_detection_interval(leak_detector)
  assert_eq(detection_interval, 5000)
  
  // Test leak statistics
  let leak_stats = LeakDetector::get_statistics(leak_detector)
  assert_eq(LeakStats::total_allocations_tracked(leak_stats), 2) // One tracked and deallocated, one leaked
  assert_eq(LeakStats::total_deallocations_tracked(leak_stats), 1)
  assert_eq(LeakStats::total_files_opened_tracked(leak_stats), 2) // One tracked and closed, one leaked
  assert_eq(LeakStats::total_files_closed_tracked(leak_stats), 1)
  assert_eq(LeakStats::total_connections_opened_tracked(leak_stats), 2) // One tracked and closed, one leaked
  assert_eq(LeakStats::total_connections_closed_tracked(leak_stats), 1)
  assert_eq(LeakStats::total_listeners_registered_tracked(leak_stats), 2) // One tracked and unregistered, one leaked
  assert_eq(LeakStats::total_listeners_unregistered_tracked(leak_stats), 1)
}