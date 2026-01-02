// Azimuth Concurrent Safety Tests
// This file contains comprehensive test cases for concurrent safety and thread-safety

// Test 1: Concurrent Telemetry Data Processing
test "concurrent telemetry data processing thread safety" {
  // Create thread-safe telemetry processor
  let processor = azimuth::ConcurrentTelemetryProcessor::new(
    max_workers = 8,
    queue_size = 10000,
    enable_thread_safety_checks = true
  )
  
  // Generate test data for concurrent processing
  let total_data_points = 10000
  let test_data = []
  
  for i = 0; i < total_data_points; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i * 1000,
      service_name = "concurrent-test-service",
      metric_name = "concurrent.metric",
      metric_value = i.to_double(),
      attributes = [
        ("thread_id", (i % 8).to_string()),
        ("batch_id", (i / 1000).to_string()),
        ("data_id", i.to_string())
      ]
    )
    test_data.push(data_point)
  }
  
  // Process data concurrently
  let start_time = azimuth::Time::now()
  let result = azimuth::ConcurrentTelemetryProcessor::process_concurrent(processor, test_data)
  let end_time = azimuth::Time::now()
  
  // Verify concurrent processing results
  assert_true(result.success)
  assert_eq(result.processed_count, total_data_points)
  assert_eq(result.error_count, 0)
  assert_true(result.processing_time_ms > 0)
  
  // Verify thread safety - no data corruption or loss
  let processed_data = result.processed_data
  assert_eq(processed_data.length(), total_data_points)
  
  // Check for duplicate processing
  let unique_ids = processed_data.map(|d| d.attributes.find(|(k, _)| k == "data_id").unwrap().1)
  let unique_count = azimuth::CollectionUtils::unique_count(unique_ids)
  assert_eq(unique_count, total_data_points)
  
  // Verify data integrity
  for data_point in processed_data {
    assert_true(data_point.metric_value >= 0.0)
    assert_true(data_point.metric_value < total_data_points.to_double())
    assert_true(data_point.service_name == "concurrent-test-service")
    assert_true(data_point.metric_name == "concurrent.metric")
  }
  
  // Check for race conditions in attributes
  let attribute_counts = azimuth::CollectionUtils::count_occurrences(
    processed_data.map(|d| d.attributes.find(|(k, _)| k == "thread_id").unwrap().1)
  )
  
  // Each thread should have processed approximately equal amount of data
  for (_, count) in attribute_counts {
    assert_true(count >= total_data_points / 8 - 100) // Allow some variance
    assert_true(count <= total_data_points / 8 + 100) // Allow some variance
  }
}

// Test 2: Concurrent Counter Operations
test "concurrent counter operations atomicity" {
  // Create shared counter
  let shared_counter = azimuth::AtomicCounter::new(0)
  
  // Create multiple threads to increment counter
  let thread_count = 10
  let increments_per_thread = 1000
  let threads = []
  
  for i = 0; i < thread_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < increments_per_thread; j = j + 1 {
        azimuth::AtomicCounter::increment(shared_counter)
        
        // Occasionally add more complex operations
        if j % 100 == 0 {
          azimuth::AtomicCounter::add(shared_counter, 10)
        }
        
        // Simulate some processing time
        if j % 50 == 0 {
          azimuth::Thread::sleep(1) // 1ms
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify final counter value
  let expected_value = thread_count * increments_per_thread + (thread_count * (increments_per_thread / 100) * 10)
  let actual_value = azimuth::AtomicCounter::get_value(shared_counter)
  
  assert_eq(actual_value, expected_value)
  
  // Test compare-and-swap operations
  let cas_counter = azimuth::AtomicCounter::new(100)
  
  // Create threads that perform CAS operations
  let cas_threads = []
  
  for i = 0; i < 5; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < 100; j = j + 1 {
        let current = azimuth::AtomicCounter::get_value(cas_counter)
        let new_value = current + 1
        
        // Try to update if value hasn't changed
        let success = azimuth::AtomicCounter::compare_and_swap(cas_counter, current, new_value)
        
        if success {
          // Successfully updated
        } else {
          // Value changed, retry
          j = j - 1 // Retry this iteration
        }
      }
    })
    cas_threads.push(thread)
  }
  
  // Wait for CAS threads to complete
  for thread in cas_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify CAS operations
  let final_cas_value = azimuth::AtomicCounter::get_value(cas_counter)
  assert_true(final_cas_value > 100)
  assert_true(final_cas_value <= 100 + 5 * 100) // Maximum possible value
}

// Test 3: Concurrent Map Operations
test "concurrent map operations thread safety" {
  // Create thread-safe map
  let concurrent_map = azimuth::ConcurrentMap::new()
  
  // Test concurrent insertions
  let insert_threads = []
  let insert_count = 10
  let items_per_thread = 1000
  
  for i = 0; i < insert_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < items_per_thread; j = j + 1 {
        let key = "key-" + i.to_string() + "-" + j.to_string()
        let value = {
          "thread_id": i,
          "item_id": j,
          "timestamp": azimuth::Time::now(),
          "data": "test-data-" + j.to_string()
        }
        
        azimuth::ConcurrentMap::put(concurrent_map, key, value)
      }
    })
    insert_threads.push(thread)
  }
  
  // Wait for all insertions to complete
  for thread in insert_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify all items were inserted
  let map_size = azimuth::ConcurrentMap::size(concurrent_map)
  assert_eq(map_size, insert_count * items_per_thread)
  
  // Test concurrent reads
  let read_threads = []
  let read_count = 5
  
  for i = 0; i < read_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let read_count = 0
      let keys = azimuth::ConcurrentMap::keys(concurrent_map)
      
      for key in keys {
        let value = azimuth::ConcurrentMap::get(concurrent_map, key)
        if value.is_some {
          read_count = read_count + 1
          
          // Verify value structure
          let data = value.unwrap()
          assert_true(data.contains("thread_id"))
          assert_true(data.contains("item_id"))
          assert_true(data.contains("timestamp"))
          assert_true(data.contains("data"))
        }
      }
      
      return read_count
    })
    read_threads.push(thread)
  }
  
  // Wait for all reads to complete and verify results
  let total_reads = 0
  for thread in read_threads {
    let read_count = azimuth::Thread::join(thread)
    total_reads = total_reads + read_count
  }
  
  // Each thread should read all items
  assert_eq(total_reads, read_count * map_size)
  
  // Test concurrent updates
  let update_threads = []
  let update_count = 3
  
  for i = 0; i < update_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let keys = azimuth::ConcurrentMap::keys(concurrent_map)
      let updated_count = 0
      
      for (index, key) in keys.enumerate() {
        if index % update_count == i {
          // Update only items assigned to this thread
          let current_value = azimuth::ConcurrentMap::get(concurrent_map, key)
          if current_value.is_some {
            let mut new_value = current_value.unwrap()
            new_value["updated_by"] = i.to_string()
            new_value["update_timestamp"] = azimuth::Time::now().to_string()
            
            azimuth::ConcurrentMap::put(concurrent_map, key, new_value)
            updated_count = updated_count + 1
          }
        }
      }
      
      return updated_count
    })
    update_threads.push(thread)
  }
  
  // Wait for all updates to complete
  let total_updates = 0
  for thread in update_threads {
    let updated_count = azimuth::Thread::join(thread)
    total_updates = total_updates + updated_count
  }
  
  // Verify all items were updated exactly once
  assert_eq(total_updates, map_size)
  
  // Verify update integrity
  let keys = azimuth::ConcurrentMap::keys(concurrent_map)
  let updated_items = 0
  
  for key in keys {
    let value = azimuth::ConcurrentMap::get(concurrent_map, key)
    if value.is_some {
      let data = value.unwrap()
      if data.contains("updated_by") && data.contains("update_timestamp") {
        updated_items = updated_items + 1
      }
    }
  }
  
  assert_eq(updated_items, map_size)
  
  // Test concurrent deletions
  let delete_threads = []
  let delete_count = 2
  
  for i = 0; i < delete_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let keys = azimuth::ConcurrentMap::keys(concurrent_map)
      let deleted_count = 0
      
      for (index, key) in keys.enumerate() {
        if index % delete_count == i {
          // Delete only items assigned to this thread
          let removed = azimuth::ConcurrentMap::remove(concurrent_map, key)
          if removed {
            deleted_count = deleted_count + 1
          }
        }
      }
      
      return deleted_count
    })
    delete_threads.push(thread)
  }
  
  // Wait for all deletions to complete
  let total_deletions = 0
  for thread in delete_threads {
    let deleted_count = azimuth::Thread::join(thread)
    total_deletions = total_deletions + deleted_count
  }
  
  // Verify all items were deleted
  assert_eq(total_deletions, map_size)
  assert_eq(azimuth::ConcurrentMap::size(concurrent_map), 0)
}

// Test 4: Concurrent Queue Operations
test "concurrent queue operations thread safety" {
  // Create thread-safe queue
  let concurrent_queue = azimuth::ConcurrentQueue::new(10000) // Max 10000 items
  
  // Test concurrent producers and consumers
  let producer_count = 3
  let consumer_count = 2
  let items_per_producer = 2000
  
  let producer_threads = []
  let consumer_threads = []
  
  // Start producer threads
  for i = 0; i < producer_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < items_per_producer; j = j + 1 {
        let item = {
          "producer_id": i,
          "item_id": j,
          "timestamp": azimuth::Time::now(),
          "data": "producer-" + i.to_string() + "-item-" + j.to_string()
        }
        
        // Try to enqueue, with timeout if queue is full
        let enqueued = azimuth::ConcurrentQueue::enqueue_with_timeout(concurrent_queue, item, 100)
        
        if !enqueued {
          // Queue was full, retry
          j = j - 1
        }
      }
    })
    producer_threads.push(thread)
  }
  
  // Start consumer threads
  let consumed_items = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < consumer_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let items_consumed = 0
      
      while items_consumed < (producer_count * items_per_producer) / consumer_count {
        let item = azimuth::ConcurrentQueue::dequeue_with_timeout(concurrent_queue, 100)
        
        if item.is_some {
          let data = item.unwrap()
          
          // Verify item structure
          assert_true(data.contains("producer_id"))
          assert_true(data.contains("item_id"))
          assert_true(data.contains("timestamp"))
          assert_true(data.contains("data"))
          
          items_consumed = items_consumed + 1
          azimuth::AtomicCounter::increment(consumed_items)
        } else {
          // Queue was empty, check if producers are done
          if azimuth::ConcurrentQueue::is_empty(concurrent_queue) {
            break
          }
        }
      }
    })
    consumer_threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in producer_threads {
    azimuth::Thread::join(thread)
  }
  
  for thread in consumer_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify all items were produced and consumed
  let expected_items = producer_count * items_per_producer
  let actual_consumed = azimuth::AtomicCounter::get_value(consumed_items)
  
  assert_eq(actual_consumed, expected_items)
  assert_true(azimuth::ConcurrentQueue::is_empty(concurrent_queue))
  
  // Test queue operations under high contention
  let contention_queue = azimuth::ConcurrentQueue::new(1000)
  let contention_threads = []
  let contention_operations = 10000
  
  for i = 0; i < 10; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < contention_operations; j = j + 1 {
        if j % 2 == 0 {
          // Enqueue operation
          let item = {"thread": i, "operation": j, "type": "enqueue"}
          azimuth::ConcurrentQueue::enqueue(contention_queue, item)
        } else {
          // Dequeue operation
          azimuth::ConcurrentQueue::dequeue(contention_queue)
        }
      }
    })
    contention_threads.push(thread)
  }
  
  // Wait for contention test threads
  for thread in contention_threads {
    azimuth::Thread::join(thread)
  }
  
  // Queue should still be functional after high contention
  let test_item = {"test": "contention"}
  let enqueued = azimuth::ConcurrentQueue::enqueue(contention_queue, test_item)
  assert_true(enqueued)
  
  let dequeued = azimuth::ConcurrentQueue::dequeue(contention_queue)
  assert_true(dequeued.is_some)
  assert_eq(dequeued.unwrap["test"], "contention")
}

// Test 5: Concurrent Resource Pool Management
test "concurrent resource pool management thread safety" {
  // Create resource pool
  let resource_pool = azimuth::ResourcePool::new(
    factory = || { azimuth::TestResource::new() },
    max_size = 10,
    initial_size = 5
  )
  
  // Test concurrent resource acquisition and release
  let thread_count = 20
  let operations_per_thread = 100
  let threads = []
  
  let acquired_resources = azimuth::AtomicCounter::new(0)
  let released_resources = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < thread_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < operations_per_thread; j = j + 1 {
        // Acquire resource with timeout
        let resource = azimuth::ResourcePool::acquire_with_timeout(resource_pool, 100)
        
        if resource.is_some {
          let res = resource.unwrap()
          azimuth::AtomicCounter::increment(acquired_resources)
          
          // Use resource
          azimuth::TestResource::use(res)
          
          // Simulate some work
          azimuth::Thread::sleep(1) // 1ms
          
          // Release resource
          azimuth::ResourcePool::release(resource_pool, res)
          azimuth::AtomicCounter::increment(released_resources)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify resource management
  let total_acquired = azimuth::AtomicCounter::get_value(acquired_resources)
  let total_released = azimuth::AtomicCounter::get_value(released_resources)
  
  assert_eq(total_acquired, total_released)
  assert_true(total_acquired > 0)
  
  // Verify pool state
  assert_eq(azimuth::ResourcePool::available_count(resource_pool), 5) // Should return to initial size
  assert_eq(azimuth::ResourcePool::acquired_count(resource_pool), 0) // All should be released
  
  // Test pool exhaustion and recovery
  let exhaustion_pool = azimuth::ResourcePool::new(
    factory = || { azimuth::TestResource::new() },
    max_size = 3,
    initial_size = 1
  )
  
  let exhaustion_threads = []
  
  // Create threads that will exhaust the pool
  for i = 0; i < 5; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      // Try to acquire more resources than available
      let resource = azimuth::ResourcePool::acquire_with_timeout(exhaustion_pool, 50)
      
      if resource.is_some {
        // Hold resource for a while
        azimuth::Thread::sleep(100) // 100ms
        
        // Release resource
        azimuth::ResourcePool::release(exhaustion_pool, resource.unwrap())
      }
    })
    exhaustion_threads.push(thread)
  }
  
  // Wait for exhaustion test threads
  for thread in exhaustion_threads {
    azimuth::Thread::join(thread)
  }
  
  // Pool should be in valid state after exhaustion
  assert_eq(azimuth::ResourcePool::acquired_count(exhaustion_pool), 0)
  assert_true(azimuth::ResourcePool::available_count(exhaustion_pool) >= 1)
}

// Test 6: Concurrent Lock Management
test "concurrent lock management and deadlock prevention" {
  // Create multiple lockable resources
  let resource_a = azimuth::LockableResource::new("resource-a")
  let resource_b = azimuth::LockableResource::new("resource-b")
  let resource_c = azimuth::LockableResource::new("resource-c")
  
  // Test concurrent access with proper locking
  let access_threads = []
  let access_count = 10
  let operations_per_thread = 100
  
  let successful_operations = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < access_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < operations_per_thread; j = j + 1 {
        // Always acquire locks in the same order to prevent deadlock
        let lock_a = azimuth::LockableResource::try_lock(resource_a, 50)
        if lock_a.is_some {
          let lock_b = azimuth::LockableResource::try_lock(resource_b, 50)
          if lock_b.is_some {
            let lock_c = azimuth::LockableResource::try_lock(resource_c, 50)
            if lock_c.is_some {
              // Successfully acquired all locks
              azimuth::LockableResource::perform_operation(resource_a, "operation-a")
              azimuth::LockableResource::perform_operation(resource_b, "operation-b")
              azimuth::LockableResource::perform_operation(resource_c, "operation-c")
              
              azimuth::AtomicCounter::increment(successful_operations)
              
              // Release locks in reverse order
              azimuth::LockableResource::unlock(resource_c, lock_c.unwrap())
              azimuth::LockableResource::unlock(resource_b, lock_b.unwrap())
              azimuth::LockableResource::unlock(resource_a, lock_a.unwrap())
            } else {
              // Couldn't acquire resource_c, release others
              azimuth::LockableResource::unlock(resource_b, lock_b.unwrap())
              azimuth::LockableResource::unlock(resource_a, lock_a.unwrap())
            }
          } else {
            // Couldn't acquire resource_b, release resource_a
            azimuth::LockableResource::unlock(resource_a, lock_a.unwrap())
          }
        }
        
        // Small delay between operations
        azimuth::Thread::sleep(1) // 1ms
      }
    })
    access_threads.push(thread)
  }
  
  // Wait for all access threads
  for thread in access_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify operations completed successfully
  let total_successful = azimuth::AtomicCounter::get_value(successful_operations)
  assert_true(total_successful > 0)
  
  // Verify all resources are unlocked
  assert_false(azimuth::LockableResource::is_locked(resource_a))
  assert_false(azimuth::LockableResource::is_locked(resource_b))
  assert_false(azimuth::LockableResource::is_locked(resource_c))
  
  // Test deadlock detection and prevention
  let deadlock_detector = azimuth::DeadlockDetector::new()
  azimuth::DeadlockDetector::enable_monitoring(deadlock_detector)
  
  let deadlock_threads = []
  
  // Create threads that could potentially deadlock
  for i = 0; i < 3; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      if i == 0 {
        // Thread 0: A -> B -> C
        let lock_a = azimuth::LockableResource::lock(resource_a)
        azimuth::Thread::sleep(10) // Increase deadlock probability
        let lock_b = azimuth::LockableResource::lock(resource_b)
        azimuth::Thread::sleep(10)
        let lock_c = azimuth::LockableResource::lock(resource_c)
        
        azimuth::LockableResource::unlock(resource_c, lock_c)
        azimuth::LockableResource::unlock(resource_b, lock_b)
        azimuth::LockableResource::unlock(resource_a, lock_a)
      } else if i == 1 {
        // Thread 1: B -> C -> A (potential deadlock)
        let lock_b = azimuth::LockableResource::lock(resource_b)
        azimuth::Thread::sleep(10)
        let lock_c = azimuth::LockableResource::lock(resource_c)
        azimuth::Thread::sleep(10)
        
        // Use timeout to prevent actual deadlock
        let lock_a = azimuth::LockableResource::try_lock(resource_a, 100)
        if lock_a.is_some {
          azimuth::LockableResource::unlock(resource_a, lock_a.unwrap())
        }
        
        azimuth::LockableResource::unlock(resource_c, lock_c)
        azimuth::LockableResource::unlock(resource_b, lock_b)
      } else {
        // Thread 2: C -> A -> B (potential deadlock)
        let lock_c = azimuth::LockableResource::lock(resource_c)
        azimuth::Thread::sleep(10)
        
        // Use timeout to prevent actual deadlock
        let lock_a = azimuth::LockableResource::try_lock(resource_a, 100)
        if lock_a.is_some {
          azimuth::Thread::sleep(10)
          let lock_b = azimuth::LockableResource::try_lock(resource_b, 100)
          if lock_b.is_some {
            azimuth::LockableResource::unlock(resource_b, lock_b.unwrap())
          }
          azimuth::LockableResource::unlock(resource_a, lock_a.unwrap())
        }
        
        azimuth::LockableResource::unlock(resource_c, lock_c)
      }
    })
    deadlock_threads.push(thread)
  }
  
  // Wait with timeout to prevent hanging test
  let deadlock_results = []
  for thread in deadlock_threads {
    let result = azimuth::Thread::join_with_timeout(thread, 5000) // 5 second timeout
    deadlock_results.push(result)
  }
  
  // Check for deadlock detection
  let deadlock_detected = azimuth::DeadlockDetector::was_deadlock_detected(deadlock_detector)
  let completed_threads = deadlock_results.filter(|r| *r).length()
  
  // At least some threads should complete (due to timeout usage)
  assert_true(completed_threads > 0)
  
  // Verify resources are not permanently locked
  assert_false(azimuth::LockableResource::is_locked(resource_a))
  assert_false(azimuth::LockableResource::is_locked(resource_b))
  assert_false(azimuth::LockableResource::is_locked(resource_c))
}

// Test 7: Concurrent Cache Operations
test "concurrent cache operations thread safety" {
  // Create thread-safe cache
  let concurrent_cache = azimuth::ConcurrentCache::new(
    max_size = 1000,
    ttl_ms = 60000 // 1 minute TTL
  )
  
  // Test concurrent cache operations
  let thread_count = 10
  let operations_per_thread = 500
  let threads = []
  
  let cache_hits = azimuth::AtomicCounter::new(0)
  let cache_misses = azimuth::AtomicCounter::new(0)
  let cache_puts = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < thread_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < operations_per_thread; j = j + 1 {
        let key = "cache-key-" + (j % 100).to_string() // 100 unique keys
        let value = {
          "thread_id": i,
          "operation_id": j,
          "timestamp": azimuth::Time::now(),
          "data": "cache-data-" + j.to_string()
        }
        
        // Mix of cache operations
        if j % 3 == 0 {
          // Put operation
          azimuth::ConcurrentCache::put(concurrent_cache, key, value)
          azimuth::AtomicCounter::increment(cache_puts)
        } else {
          // Get operation
          let cached_value = azimuth::ConcurrentCache::get(concurrent_cache, key)
          
          if cached_value.is_some {
            azimuth::AtomicCounter::increment(cache_hits)
            
            // Verify cached value structure
            let data = cached_value.unwrap()
            assert_true(data.contains("thread_id"))
            assert_true(data.contains("operation_id"))
            assert_true(data.contains("timestamp"))
            assert_true(data.contains("data"))
          } else {
            azimuth::AtomicCounter::increment(cache_misses)
            
            // Put value if not found
            azimuth::ConcurrentCache::put(concurrent_cache, key, value)
            azimuth::AtomicCounter::increment(cache_puts)
          }
        }
        
        // Occasionally remove items
        if j % 20 == 0 {
          azimuth::ConcurrentCache::remove(concurrent_cache, key)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify cache statistics
  let total_hits = azimuth::AtomicCounter::get_value(cache_hits)
  let total_misses = azimuth::AtomicCounter::get_value(cache_misses)
  let total_puts = azimuth::AtomicCounter::get_value(cache_puts)
  
  assert_true(total_hits > 0)
  assert_true(total_misses > 0)
  assert_true(total_puts > 0)
  
  let hit_rate = total_hits.to_double() / (total_hits + total_misses).to_double()
  assert_true(hit_rate > 0.1) // At least 10% hit rate
  
  // Verify cache size is within limits
  let cache_size = azimuth::ConcurrentCache::size(concurrent_cache)
  assert_true(cache_size <= 1000)
  
  // Test cache eviction under concurrent access
  let eviction_cache = azimuth::ConcurrentCache::new(
    max_size = 100, // Small cache to trigger eviction
    ttl_ms = 60000
  )
  
  let eviction_threads = []
  
  // Fill cache beyond capacity
  for i = 0; i < 5; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < 100; j = j + 1 {
        let key = "eviction-key-" + (i * 100 + j).to_string()
        let value = {"data": "eviction-test-" + j.to_string()}
        
        azimuth::ConcurrentCache::put(eviction_cache, key, value)
      }
    })
    eviction_threads.push(thread)
  }
  
  // Wait for eviction threads
  for thread in eviction_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify cache size is maintained
  let eviction_cache_size = azimuth::ConcurrentCache::size(eviction_cache)
  assert_true(eviction_cache_size <= 100)
  
  // Verify cache is still functional after eviction
  let test_key = "test-key"
  let test_value = {"test": "after-eviction"}
  azimuth::ConcurrentCache::put(eviction_cache, test_key, test_value)
  
  let retrieved = azimuth::ConcurrentCache::get(eviction_cache, test_key)
  assert_true(retrieved.is_some)
  assert_eq(retrieved.unwrap["test"], "after-eviction")
}

// Test 8: Concurrent Event System
test "concurrent event system thread safety" {
  // Create thread-safe event system
  let event_system = azimuth::ConcurrentEventSystem::new()
  
  // Test concurrent event publishing and subscribing
  let publisher_count = 5
  let subscriber_count = 3
  let events_per_publisher = 200
  
  let publisher_threads = []
  let subscriber_threads = []
  
  let published_events = azimuth::AtomicCounter::new(0)
  let received_events = azimuth::AtomicCounter::new(0)
  
  // Create subscribers
  for i = 0; i < subscriber_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let subscription_id = azimuth::ConcurrentEventSystem::subscribe(
        event_system, 
        "test-event", 
        |event| {
          // Verify event structure
          assert_true(event.contains("event_type"))
          assert_true(event.contains("timestamp"))
          assert_true(event.contains("data"))
          
          azimuth::AtomicCounter::increment(received_events)
        }
      )
      
      // Keep subscription alive for test duration
      azimuth::Thread::sleep(2000) // 2 seconds
      
      azimuth::ConcurrentEventSystem::unsubscribe(event_system, subscription_id)
    })
    subscriber_threads.push(thread)
  }
  
  // Small delay to ensure subscribers are ready
  azimuth::Thread::sleep(100)
  
  // Create publishers
  for i = 0; i < publisher_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < events_per_publisher; j = j + 1 {
        let event = {
          "event_type": "test-event",
          "publisher_id": i,
          "event_id": j,
          "timestamp": azimuth::Time::now(),
          "data": "event-data-" + j.to_string()
        }
        
        azimuth::ConcurrentEventSystem::publish(event_system, event)
        azimuth::AtomicCounter::increment(published_events)
        
        // Small delay between events
        azimuth::Thread::sleep(1) // 1ms
      }
    })
    publisher_threads.push(thread)
  }
  
  // Wait for all publishers
  for thread in publisher_threads {
    azimuth::Thread::join(thread)
  }
  
  // Wait for all subscribers
  for thread in subscriber_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify event delivery
  let total_published = azimuth::AtomicCounter::get_value(published_events)
  let total_received = azimuth::AtomicCounter::get_value(received_events)
  
  assert_eq(total_published, publisher_count * events_per_publisher)
  assert_true(total_received > 0)
  
  // Each event should be received by each subscriber (approximately)
  let expected_received = total_published * subscriber_count
  let delivery_rate = total_received.to_double() / expected_received.to_double()
  
  // Allow some delivery loss due to timing
  assert_true(delivery_rate > 0.8) // At least 80% delivery rate
  
  // Test event filtering under concurrent access
  let filter_system = azimuth::ConcurrentEventSystem::new()
  
  let filter_threads = []
  let filtered_received = azimuth::AtomicCounter::new(0)
  
  // Create subscriber with filter
  let filter_subscription = azimuth::ConcurrentEventSystem::subscribe_with_filter(
    filter_system,
    "filtered-event",
    |event| { event["priority"] == "high" },
    |event| {
      azimuth::AtomicCounter::increment(filtered_received)
    }
  )
  
  // Create publishers with different priorities
  for i = 0; i < 3; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < 100; j = j + 1 {
        let priority = if j % 3 == 0 { "high" } else { "low" }
        
        let event = {
          "event_type": "filtered-event",
          "priority": priority,
          "publisher_id": i,
          "event_id": j,
          "timestamp": azimuth::Time::now()
        }
        
        azimuth::ConcurrentEventSystem::publish(filter_system, event)
      }
    })
    filter_threads.push(thread)
  }
  
  // Wait for filter test threads
  for thread in filter_threads {
    azimuth::Thread::join(thread)
  }
  
  // Wait for events to be processed
  azimuth::Thread::sleep(500)
  
  // Unsubscribe and verify results
  azimuth::ConcurrentEventSystem::unsubscribe(filter_system, filter_subscription)
  
  let total_filtered_received = azimuth::AtomicCounter::get_value(filtered_received)
  
  // Should receive only high priority events (approximately 1/3 of total)
  let total_events = 3 * 100 // 3 publishers * 100 events each
  let expected_high_priority = total_events / 3
  
  assert_true(total_filtered_received >= expected_high_priority * 0.8) // Allow some variance
  assert_true(total_filtered_received <= expected_high_priority * 1.2) // Allow some variance
}

// Test 9: Concurrent Metrics Collection
test "concurrent metrics collection thread safety" {
  // Create thread-safe metrics collector
  let metrics_collector = azimuth::ConcurrentMetricsCollector::new()
  
  // Register metrics
  azimuth::ConcurrentMetricsCollector::register_counter(metrics_collector, "requests.total")
  azimuth::ConcurrentMetricsCollector::register_histogram(metrics_collector, "request.duration")
  azimuth::ConcurrentMetricsCollector::register_gauge(metrics_collector, "active.connections")
  
  // Test concurrent metric updates
  let thread_count = 10
  let operations_per_thread = 1000
  let threads = []
  
  let total_counter_updates = azimuth::AtomicCounter::new(0)
  let total_histogram_updates = azimuth::AtomicCounter::new(0)
  let total_gauge_updates = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < thread_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < operations_per_thread; j = j + 1 {
        // Update counter
        azimuth::ConcurrentMetricsCollector::increment_counter(metrics_collector, "requests.total")
        azimuth::AtomicCounter::increment(total_counter_updates)
        
        // Update histogram
        let duration = 10.0 + (j % 100).to_double()
        azimuth::ConcurrentMetricsCollector::record_histogram(metrics_collector, "request.duration", duration)
        azimuth::AtomicCounter::increment(total_histogram_updates)
        
        // Update gauge (simulate connection activity)
        if j % 10 == 0 {
          let connections = 10 + (j % 50)
          azimuth::ConcurrentMetricsCollector::set_gauge(metrics_collector, "active.connections", connections.to_double())
          azimuth::AtomicCounter::increment(total_gauge_updates)
        }
        
        // Occasionally read metrics
        if j % 100 == 0 {
          let counter_value = azimuth::ConcurrentMetricsCollector::get_counter_value(metrics_collector, "requests.total")
          let gauge_value = azimuth::ConcurrentMetricsCollector::get_gauge_value(metrics_collector, "active.connections")
          let histogram_stats = azimuth::ConcurrentMetricsCollector::get_histogram_stats(metrics_collector, "request.duration")
          
          // Verify metric values are reasonable
          assert_true(counter_value >= 0.0)
          assert_true(gauge_value >= 0.0)
          assert_true(histogram_stats.count >= 0)
          assert_true(histogram_stats.sum >= 0.0)
        }
      }
    })
    threads.push(thread)
  }
  
  // Wait for all threads
  for thread in threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify metric values
  let expected_counter_value = azimuth::AtomicCounter::get_value(total_counter_updates)
  let actual_counter_value = azimuth::ConcurrentMetricsCollector::get_counter_value(metrics_collector, "requests.total")
  
  assert_eq(actual_counter_value, expected_counter_value)
  
  let expected_histogram_count = azimuth::AtomicCounter::get_value(total_histogram_updates)
  let histogram_stats = azimuth::ConcurrentMetricsCollector::get_histogram_stats(metrics_collector, "request.duration")
  
  assert_eq(histogram_stats.count, expected_histogram_count)
  assert_true(histogram_stats.sum > 0.0)
  assert_true(histogram_stats.avg >= 10.0)
  
  // Test concurrent metric snapshots
  let snapshot_threads = []
  let snapshots_taken = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < 5; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < 20; j = j + 1 {
        let snapshot = azimuth::ConcurrentMetricsCollector::take_snapshot(metrics_collector)
        
        // Verify snapshot contains all registered metrics
        assert_true(snapshot.contains("requests.total"))
        assert_true(snapshot.contains("request.duration"))
        assert_true(snapshot.contains("active.connections"))
        
        // Verify snapshot values are consistent
        let counter = snapshot["requests.total"]
        let histogram = snapshot["request.duration"]
        let gauge = snapshot["active.connections"]
        
        assert_true(counter >= 0.0)
        assert_true(histogram["count"] >= 0)
        assert_true(gauge >= 0.0)
        
        azimuth::AtomicCounter::increment(snapshots_taken)
        
        azimuth::Thread::sleep(10) // 10ms between snapshots
      }
    })
    snapshot_threads.push(thread)
  }
  
  // Wait for snapshot threads
  for thread in snapshot_threads {
    azimuth::Thread::join(thread)
  }
  
  let total_snapshots = azimuth::AtomicCounter::get_value(snapshots_taken)
  assert_eq(total_snapshots, 5 * 20) // 5 threads * 20 snapshots each
  
  // Test metric reset under concurrent access
  let reset_collector = azimuth::ConcurrentMetricsCollector::new()
  azimuth::ConcurrentMetricsCollector::register_counter(reset_collector, "reset.counter")
  
  let reset_threads = []
  
  // Create threads that update and reset metrics
  for i = 0; i < 3; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < 100; j = j + 1 {
        // Update counter
        azimuth::ConcurrentMetricsCollector::increment_counter(reset_collector, "reset.counter")
        
        // Occasionally reset
        if j % 30 == 0 {
          azimuth::ConcurrentMetricsCollector::reset_metric(reset_collector, "reset.counter")
        }
      }
    })
    reset_threads.push(thread)
  }
  
  // Wait for reset threads
  for thread in reset_threads {
    azimuth::Thread::join(thread)
  }
  
  // Final counter value should be reasonable (not corrupted)
  let final_counter_value = azimuth::ConcurrentMetricsCollector::get_counter_value(reset_collector, "reset.counter")
  assert_true(final_counter_value >= 0.0)
  assert_true(final_counter_value < 300.0) // Maximum possible if no resets
}

// Test 10: Concurrent Configuration Management
test "concurrent configuration management thread safety" {
  // Create thread-safe configuration manager
  let config_manager = azimuth::ConcurrentConfigManager::new()
  
  // Initialize configuration
  let initial_config = {
    "service.name": "test-service",
    "service.version": "1.0.0",
    "telemetry.enabled": "true",
    "telemetry.sample_rate": "0.1",
    "network.timeout_ms": "5000",
    "cache.max_size": "1000"
  }
  
  azimuth::ConcurrentConfigManager::load_config(config_manager, initial_config)
  
  // Test concurrent configuration reads
  let reader_count = 10
  let reads_per_thread = 1000
  let reader_threads = []
  
  let total_reads = azimuth::AtomicCounter::new(0)
  let successful_reads = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < reader_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < reads_per_thread; j = j + 1 {
        let keys = ["service.name", "telemetry.enabled", "network.timeout_ms"]
        let key = keys[j % keys.length()]
        
        let value = azimuth::ConcurrentConfigManager::get(config_manager, key)
        azimuth::AtomicCounter::increment(total_reads)
        
        if value.is_some {
          azimuth::AtomicCounter::increment(successful_reads)
          
          // Verify value is not empty
          assert_true(value.unwrap().length() > 0)
        }
        
        // Occasionally read full config
        if j % 100 == 0 {
          let full_config = azimuth::ConcurrentConfigManager::get_all(config_manager)
          
          // Verify full config contains all initial keys
          assert_true(full_config.contains("service.name"))
          assert_true(full_config.contains("service.version"))
          assert_true(full_config.contains("telemetry.enabled"))
          assert_true(full_config.contains("telemetry.sample_rate"))
          assert_true(full_config.contains("network.timeout_ms"))
          assert_true(full_config.contains("cache.max_size"))
        }
      }
    })
    reader_threads.push(thread)
  }
  
  // Test concurrent configuration updates
  let writer_count = 3
  let updates_per_thread = 100
  let writer_threads = []
  
  let total_updates = azimuth::AtomicCounter::new(0)
  let successful_updates = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < writer_count; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      for j = 0; j < updates_per_thread; j = j + 1 {
        let key = "dynamic.key." + (j % 10).to_string()
        let value = "thread-" + i.to_string() + "-value-" + j.to_string()
        
        let updated = azimuth::ConcurrentConfigManager::set(config_manager, key, value)
        azimuth::AtomicCounter::increment(total_updates)
        
        if updated {
          azimuth::AtomicCounter::increment(successful_updates)
          
          // Verify the update was applied
          let retrieved = azimuth::ConcurrentConfigManager::get(config_manager, key)
          assert_true(retrieved.is_some)
          assert_eq(retrieved.unwrap(), value)
        }
        
        // Small delay between updates
        azimuth::Thread::sleep(1) // 1ms
      }
    })
    writer_threads.push(thread)
  }
  
  // Test configuration subscription under concurrent access
  let subscription_threads = []
  let notifications_received = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < 2; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let subscription_id = azimuth::ConcurrentConfigManager::subscribe(
        config_manager,
        |key, old_value, new_value| {
          // Verify notification parameters
          assert_true(key.length() > 0)
          
          azimuth::AtomicCounter::increment(notifications_received)
        }
      )
      
      // Keep subscription alive for test duration
      azimuth::Thread::sleep(2000) // 2 seconds
      
      azimuth::ConcurrentConfigManager::unsubscribe(config_manager, subscription_id)
    })
    subscription_threads.push(thread)
  }
  
  // Small delay to ensure subscriptions are active
  azimuth::Thread::sleep(100)
  
  // Trigger configuration changes to generate notifications
  for i = 0; i < 10; i = i + 1 {
    let key = "notification.key." + i.to_string()
    let value = "notification-value-" + i.to_string()
    
    azimuth::ConcurrentConfigManager::set(config_manager, key, value)
    azimuth::Thread::sleep(50) // 50ms between changes
  }
  
  // Wait for all threads
  for thread in reader_threads {
    azimuth::Thread::join(thread)
  }
  
  for thread in writer_threads {
    azimuth::Thread::join(thread)
  }
  
  for thread in subscription_threads {
    azimuth::Thread::join(thread)
  }
  
  // Verify read operations
  let total_reads_completed = azimuth::AtomicCounter::get_value(total_reads)
  let successful_reads_completed = azimuth::AtomicCounter::get_value(successful_reads)
  
  assert_eq(total_reads_completed, reader_count * reads_per_thread)
  assert_eq(successful_reads_completed, total_reads_completed) // All reads should succeed
  
  // Verify update operations
  let total_updates_completed = azimuth::AtomicCounter::get_value(total_updates)
  let successful_updates_completed = azimuth::AtomicCounter::get_value(successful_updates)
  
  assert_eq(total_updates_completed, writer_count * updates_per_thread)
  assert_eq(successful_updates_completed, total_updates_completed) // All updates should succeed
  
  // Verify notifications
  let total_notifications = azimuth::AtomicCounter::get_value(notifications_received)
  assert_true(total_notifications > 0)
  
  // Verify final configuration state
  let final_config = azimuth::ConcurrentConfigManager::get_all(config_manager)
  
  // Should contain original keys
  assert_true(final_config.contains("service.name"))
  assert_true(final_config.contains("service.version"))
  assert_true(final_config.contains("telemetry.enabled"))
  assert_true(final_config.contains("telemetry.sample_rate"))
  assert_true(final_config.contains("network.timeout_ms"))
  assert_true(final_config.contains("cache.max_size"))
  
  // Should contain dynamic keys
  for i = 0; i < 10; i = i + 1 {
    let key = "dynamic.key." + i.to_string()
    assert_true(final_config.contains(key))
  }
  
  // Should contain notification keys
  for i = 0; i < 10; i = i + 1 {
    let key = "notification.key." + i.to_string()
    assert_true(final_config.contains(key))
  }
  
  // Test configuration reload under concurrent access
  let reload_threads = []
  let reloads_completed = azimuth::AtomicCounter::new(0)
  
  for i = 0; i < 3; i = i + 1 {
    let thread = azimuth::Thread::spawn(|| {
      let new_config = {
        "reloaded.key": "reloaded-value-" + i.to_string(),
        "reload.timestamp": azimuth::Time::now().to_string()
      }
      
      let reloaded = azimuth::ConcurrentConfigManager::reload_config(config_manager, new_config)
      
      if reloaded {
        azimuth::AtomicCounter::increment(reloads_completed)
      }
    })
    reload_threads.push(thread)
  }
  
  // Wait for reload threads
  for thread in reload_threads {
    azimuth::Thread::join(thread)
  }
  
  let total_reloads = azimuth::AtomicCounter::get_value(reloads_completed)
  assert_eq(total_reloads, 3) // All reloads should succeed
  
  // Verify configuration is still consistent after reloads
  let post_reload_config = azimuth::ConcurrentConfigManager::get_all(config_manager)
  assert_true(post_reload_config.length() > 0)
  
  // Original keys should still be present (reload merges, doesn't replace)
  assert_true(post_reload_config.contains("service.name"))
  assert_true(post_reload_config.contains("telemetry.enabled"))
  
  // Reloaded keys should be present
  for i = 0; i < 3; i = i + 1 {
    let key = "reloaded.key"
    assert_true(post_reload_config.contains(key))
  }
}