// Azimuth Resource Management Comprehensive Test Suite
// This file contains test cases for resource management mechanisms

// Test 1: Memory Pool Simulation
test "memory pool simulation" {
  // Simulate a memory pool with fixed-size blocks
  type MemoryPool = {
    blocks: Array<Bool>,  // true = allocated, false = free
    block_size: Int
  }
  
  let create_pool = fn(num_blocks: Int, block_size: Int) -> MemoryPool {
    let mut blocks = []
    for i in 0..<num_blocks {
      blocks = blocks.push(false)
    }
    { blocks: blocks, block_size: block_size }
  }
  
  let pool_allocate = fn(pool: MemoryPool) -> Result<(MemoryPool, Int), String> {
    for i in 0..<pool.blocks.length() {
      if !pool.blocks[i] {
        let new_blocks = pool.blocks.with(i, true)
        return Ok(({ blocks: new_blocks, block_size: pool.block_size }, i))
      }
    }
    Err("No free blocks available")
  }
  
  let pool_deallocate = fn(pool: MemoryPool, block_index: Int) -> Result<MemoryPool, String> {
    if block_index >= 0 && block_index < pool.blocks.length() && pool.blocks[block_index] {
      let new_blocks = pool.blocks.with(block_index, false)
      Ok({ blocks: new_blocks, block_size: pool.block_size })
    } else {
      Err("Invalid block index")
    }
  }
  
  let pool_get_usage = fn(pool: MemoryPool) -> (Int, Int) {
    let mut allocated = 0
    for block in pool.blocks {
      if block {
        allocated = allocated + 1
      }
    }
    (allocated, pool.blocks.length())
  }
  
  // Test pool operations
  let pool1 = create_pool(5, 1024)
  let (allocated1, total1) = pool_get_usage(pool1)
  assert_eq(allocated1, 0)
  assert_eq(total1, 5)
  
  // Allocate blocks
  let pool2_result = pool_allocate(pool1)
  match pool2_result {
    Ok((pool2, index1)) => {
      assert_eq(index1, 0)
      let (allocated2, total2) = pool_get_usage(pool2)
      assert_eq(allocated2, 1)
      assert_eq(total2, 5)
      
      let pool3_result = pool_allocate(pool2)
      match pool3_result {
        Ok((pool3, index2)) => {
          assert_eq(index2, 1)
          let (allocated3, total3) = pool_get_usage(pool3)
          assert_eq(allocated3, 2)
          assert_eq(total3, 5)
          
          // Deallocate blocks
          let pool4_result = pool_deallocate(pool3, index1)
          match pool4_result {
            Ok(pool4) => {
              let (allocated4, total4) = pool_get_usage(pool4)
              assert_eq(allocated4, 1)
              assert_eq(total4, 5)
              
              // Allocate again - should reuse the freed block
              let pool5_result = pool_allocate(pool4)
              match pool5_result {
                Ok((pool5, index3)) => {
                  assert_eq(index3, 0)  // Should reuse the first freed block
                  let (allocated5, total5) = pool_get_usage(pool5)
                  assert_eq(allocated5, 2)
                  assert_eq(total5, 5)
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test pool exhaustion
  let pool_exhaust = create_pool(2, 512)
  let pool_ex1_result = pool_allocate(pool_exhaust)
  match pool_ex1_result {
    Ok((pool_ex1, _)) => {
      let pool_ex2_result = pool_allocate(pool_ex1)
      match pool_ex2_result {
        Ok((pool_ex2, _)) => {
          // Pool should be full now
          let pool_ex3_result = pool_allocate(pool_ex2)
          match pool_ex3_result {
            Ok(_) => assert_true(false)
            Err(msg) => assert_eq(msg, "No free blocks available")
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 2: Resource Reference Counting
test "resource reference counting" {
  // Simulate reference counting for resources
  type RefCountedResource = {
    data: String,
    ref_count: Int
  }
  
  type ResourceManager = Array<(String, RefCountedResource)>
  
  let create_manager = fn() -> ResourceManager {
    []
  }
  
  let manager_create_resource = fn(manager: ResourceManager, id: String, data: String) -> ResourceManager {
    let resource = { data: data, ref_count: 1 }
    manager.push((id, resource))
  }
  
  let manager_acquire = fn(manager: ResourceManager, id: String) -> Result<ResourceManager, String> {
    let mut found = false
    let mut result = manager
    
    for i in 0..<manager.length() {
      let (current_id, resource) = manager[i]
      if current_id == id {
        found = true
        let updated_resource = { data: resource.data, ref_count: resource.ref_count + 1 }
        result = result.with(i, (current_id, updated_resource))
        break
      }
    }
    
    if found {
      Ok(result)
    } else {
      Err("Resource not found")
    }
  }
  
  let manager_release = fn(manager: ResourceManager, id: String) -> Result<ResourceManager, String> {
    let mut found = false
    let mut result = manager
    
    for i in 0..<manager.length() {
      let (current_id, resource) = manager[i]
      if current_id == id {
        found = true
        if resource.ref_count > 1 {
          let updated_resource = { data: resource.data, ref_count: resource.ref_count - 1 }
          result = result.with(i, (current_id, updated_resource))
        } else {
          // Remove resource when ref count reaches 0
          result = result.slice(0, i) + result.slice(i + 1, result.length())
        }
        break
      }
    }
    
    if found {
      Ok(result)
    } else {
      Err("Resource not found")
    }
  }
  
  let manager_get_ref_count = fn(manager: ResourceManager, id: String) -> Option<Int> {
    for (current_id, resource) in manager {
      if current_id == id {
        return Some(resource.ref_count)
      }
    }
    None
  }
  
  // Test resource management
  let manager1 = create_manager()
  let manager2 = manager_create_resource(manager1, "resource1", "data1")
  
  match manager_get_ref_count(manager2, "resource1") {
    Some(count) => assert_eq(count, 1)
    None => assert_true(false)
  }
  
  // Acquire resource
  let manager3_result = manager_acquire(manager2, "resource1")
  match manager3_result {
    Ok(manager3) => {
      match manager_get_ref_count(manager3, "resource1") {
        Some(count) => assert_eq(count, 2)
        None => assert_true(false)
      }
      
      // Acquire again
      let manager4_result = manager_acquire(manager3, "resource1")
      match manager4_result {
        Ok(manager4) => {
          match manager_get_ref_count(manager4, "resource1") {
            Some(count) => assert_eq(count, 3)
            None => assert_true(false)
          }
          
          // Release resource
          let manager5_result = manager_release(manager4, "resource1")
          match manager5_result {
            Ok(manager5) => {
              match manager_get_ref_count(manager5, "resource1") {
                Some(count) => assert_eq(count, 2)
                None => assert_true(false)
              }
              
              // Release again
              let manager6_result = manager_release(manager5, "resource1")
              match manager6_result {
                Ok(manager6) => {
                  match manager_get_ref_count(manager6, "resource1") {
                    Some(count) => assert_eq(count, 1)
                    None => assert_true(false)
                  }
                  
                  // Final release - resource should be removed
                  let manager7_result = manager_release(manager6, "resource1")
                  match manager7_result {
                    Ok(manager7) => {
                      match manager_get_ref_count(manager7, "resource1") {
                        Some(_) => assert_true(false)
                        None => assert_true(true)
                      }
                    }
                    Err(_) => assert_true(false)
                  }
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test error cases
  let manager_error = create_manager()
  let acquire_error_result = manager_acquire(manager_error, "nonexistent")
  match acquire_error_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Resource not found")
  }
  
  let release_error_result = manager_release(manager_error, "nonexistent")
  match release_error_result {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Resource not found")
  }
}

// Test 3: Resource Cleanup Simulation
test "resource cleanup simulation" {
  // Simulate resource cleanup with finalizers
  type Resource = {
    id: String,
    data: String,
    cleanup_called: Bool
  }
  
  type ResourceTracker = Array<Resource>
  
  let create_tracker = fn() -> ResourceTracker {
    []
  }
  
  let tracker_create_resource = fn(tracker: ResourceTracker, id: String, data: String) -> ResourceTracker {
    let resource = { id: id, data: data, cleanup_called: false }
    tracker.push(resource)
  }
  
  let tracker_cleanup_resource = fn(tracker: ResourceTracker, id: String) -> ResourceTracker {
    let mut result = tracker
    
    for i in 0..<tracker.length() {
      let resource = tracker[i]
      if resource.id == id && !resource.cleanup_called {
        let cleaned_resource = { id: resource.id, data: resource.data, cleanup_called: true }
        result = result.with(i, cleaned_resource)
        break
      }
    }
    
    result
  }
  
  let tracker_remove_resource = fn(tracker: ResourceTracker, id: String) -> ResourceTracker {
    let mut result = tracker
    
    for i in 0..<tracker.length() {
      let resource = tracker[i]
      if resource.id == id {
        result = result.slice(0, i) + result.slice(i + 1, result.length())
        break
      }
    }
    
    result
  }
  
  let tracker_is_cleanup_called = fn(tracker: ResourceTracker, id: String) -> Bool {
    for resource in tracker {
      if resource.id == id {
        return resource.cleanup_called
      }
    }
    false
  }
  
  let tracker_cleanup_all = fn(tracker: ResourceTracker) -> ResourceTracker {
    let mut result = tracker
    
    for i in 0..<tracker.length() {
      let resource = tracker[i]
      if !resource.cleanup_called {
        let cleaned_resource = { id: resource.id, data: resource.data, cleanup_called: true }
        result = result.with(i, cleaned_resource)
      }
    }
    
    result
  }
  
  // Test resource cleanup
  let tracker1 = create_tracker()
  let tracker2 = tracker_create_resource(tracker1, "res1", "data1")
  let tracker3 = tracker_create_resource(tracker2, "res2", "data2")
  
  assert_false(tracker_is_cleanup_called(tracker3, "res1"))
  assert_false(tracker_is_cleanup_called(tracker3, "res2"))
  
  // Cleanup individual resource
  let tracker4 = tracker_cleanup_resource(tracker3, "res1")
  assert_true(tracker_is_cleanup_called(tracker4, "res1"))
  assert_false(tracker_is_cleanup_called(tracker4, "res2"))
  
  // Cleanup all resources
  let tracker5 = tracker_cleanup_all(tracker4)
  assert_true(tracker_is_cleanup_called(tracker5, "res1"))
  assert_true(tracker_is_cleanup_called(tracker5, "res2"))
  
  // Remove resource
  let tracker6 = tracker_remove_resource(tracker5, "res1")
  assert_false(tracker_is_cleanup_called(tracker6, "res1"))
  assert_true(tracker_is_cleanup_called(tracker6, "res2"))
}

// Test 4: Resource Pool with Timeout Simulation
test "resource pool with timeout simulation" {
  // Simulate a resource pool with timeout for acquiring resources
  type PooledResource = {
    id: String,
    in_use: Bool
  }
  
  type ResourcePool = {
    resources: Array<PooledResource>,
    waiting_queue: Array<String>  // Requester IDs
  }
  
  let create_pool = fn(resource_ids: Array<String>) -> ResourcePool {
    let mut resources = []
    for id in resource_ids {
      resources = resources.push({ id: id, in_use: false })
    }
    { resources: resources, waiting_queue: [] }
  }
  
  let pool_acquire = fn(pool: ResourcePool, requester_id: String, timeout: Int) -> Result<ResourcePool, String> {
    // Find an available resource
    let mut available_index = -1
    for i in 0..<pool.resources.length() {
      if !pool.resources[i].in_use {
        available_index = i
        break
      }
    }
    
    if available_index >= 0 {
      // Resource available, allocate it
      let updated_resource = { id: pool.resources[available_index].id, in_use: true }
      let updated_resources = pool.resources.with(available_index, updated_resource)
      Ok({ resources: updated_resources, waiting_queue: pool.waiting_queue })
    } else if timeout > 0 {
      // No resource available, add to waiting queue
      let updated_queue = pool.waiting_queue.push(requester_id)
      Ok({ resources: pool.resources, waiting_queue: updated_queue })
    } else {
      // Timeout reached
      Err("Resource acquisition timeout")
    }
  }
  
  let pool_release = fn(pool: ResourcePool, resource_id: String) -> ResourcePool {
    let mut result = pool
    
    // Mark resource as not in use
    for i in 0..<pool.resources.length() {
      let resource = pool.resources[i]
      if resource.id == resource_id && resource.in_use {
        let updated_resource = { id: resource.id, in_use: false }
        result = { resources: result.resources.with(i, updated_resource), waiting_queue: result.waiting_queue }
        break
      }
    }
    
    // Process waiting queue if not empty
    if result.waiting_queue.length() > 0 {
      let requester_id = result.waiting_queue[0]
      let updated_queue = result.waiting_queue.slice(1, result.waiting_queue.length())
      
      // Find an available resource
      for i in 0..<result.resources.length() {
        if !result.resources[i].in_use {
          let updated_resource = { id: result.resources[i].id, in_use: true }
          result = { 
            resources: result.resources.with(i, updated_resource), 
            waiting_queue: updated_queue 
          }
          break
        }
      }
    }
    
    result
  }
  
  let pool_get_available_count = fn(pool: ResourcePool) -> Int {
    let mut count = 0
    for resource in pool.resources {
      if !resource.in_use {
        count = count + 1
      }
    }
    count
  }
  
  let pool_get_waiting_count = fn(pool: ResourcePool) -> Int {
    pool.waiting_queue.length()
  }
  
  // Test pool operations
  let pool1 = create_pool(["res1", "res2", "res3"])
  assert_eq(pool_get_available_count(pool1), 3)
  assert_eq(pool_get_waiting_count(pool1), 0)
  
  // Acquire resources
  let pool2_result = pool_acquire(pool1, "requester1", 10)
  match pool2_result {
    Ok(pool2) => {
      assert_eq(pool_get_available_count(pool2), 2)
      assert_eq(pool_get_waiting_count(pool2), 0)
      
      let pool3_result = pool_acquire(pool2, "requester2", 10)
      match pool3_result {
        Ok(pool3) => {
          assert_eq(pool_get_available_count(pool3), 1)
          assert_eq(pool_get_waiting_count(pool3), 0)
          
          let pool4_result = pool_acquire(pool3, "requester3", 10)
          match pool4_result {
            Ok(pool4) => {
              assert_eq(pool_get_available_count(pool4), 0)
              assert_eq(pool_get_waiting_count(pool4), 0)
              
              // Try to acquire with no resources available
              let pool5_result = pool_acquire(pool4, "requester4", 10)
              match pool5_result {
                Ok(pool5) => {
                  assert_eq(pool_get_available_count(pool5), 0)
                  assert_eq(pool_get_waiting_count(pool5), 1)
                  
                  // Try to acquire with timeout
                  let pool6_result = pool_acquire(pool5, "requester5", 0)
                  match pool6_result {
                    Ok(_) => assert_true(false)
                    Err(msg) => assert_eq(msg, "Resource acquisition timeout")
                  }
                  
                  // Release a resource
                  let pool7 = pool_release(pool5, "res1")
                  assert_eq(pool_get_available_count(pool7), 0)  // Should be allocated to waiting requester
                  assert_eq(pool_get_waiting_count(pool7), 0)
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: Resource Usage Limits
test "resource usage limits" {
  // Simulate resource usage limits and quotas
  type ResourceUsage = {
    resource_id: String,
    usage_count: Int,
    last_used: Int  // Simulated timestamp
  }
  
  type UsageTracker = {
    usages: Array<ResourceUsage>,
    limits: Array<(String, Int)>  // (resource_id, max_usage)
    current_time: Int
  }
  
  let create_tracker = fn(limits: Array<(String, Int)>) -> UsageTracker {
    { usages: [], limits: limits, current_time: 0 }
  }
  
  let tracker_advance_time = fn(tracker: UsageTracker, amount: Int) -> UsageTracker {
    { usages: tracker.usages, limits: tracker.limits, current_time: tracker.current_time + amount }
  }
  
  let tracker_get_limit = fn(tracker: UsageTracker, resource_id: String) -> Option<Int> {
    for (id, limit) in tracker.limits {
      if id == resource_id {
        return Some(limit)
      }
    }
    None
  }
  
  let tracker_get_usage = fn(tracker: UsageTracker, resource_id: String) -> Int {
    for usage in tracker.usages {
      if usage.resource_id == resource_id {
        return usage.usage_count
      }
    }
    0
  }
  
  let tracker_can_use = fn(tracker: UsageTracker, resource_id: String) -> Bool {
    match tracker_get_limit(tracker, resource_id) {
      Some(limit) => tracker_get_usage(tracker, resource_id) < limit
      None => false
    }
  }
  
  let tracker_use_resource = fn(tracker: UsageTracker, resource_id: String) -> Result<UsageTracker, String> {
    if !tracker_can_use(tracker, resource_id) {
      return Err("Resource usage limit exceeded")
    }
    
    let mut found = false
    let mut result = tracker
    
    for i in 0..<tracker.usages.length() {
      let usage = tracker.usages[i]
      if usage.resource_id == resource_id {
        let updated_usage = { 
          resource_id: usage.resource_id, 
          usage_count: usage.usage_count + 1, 
          last_used: tracker.current_time 
        }
        result = { 
          usages: result.usages.with(i, updated_usage), 
          limits: result.limits, 
          current_time: result.current_time 
        }
        found = true
        break
      }
    }
    
    if !found {
      let new_usage = { resource_id: resource_id, usage_count: 1, last_used: tracker.current_time }
      result = { 
        usages: result.usages.push(new_usage), 
        limits: result.limits, 
        current_time: result.current_time 
      }
    }
    
    Ok(result)
  }
  
  // Test usage limits
  let limits = [("res1", 3), ("res2", 2)]
  let tracker1 = create_tracker(limits)
  
  assert_true(tracker_can_use(tracker1, "res1"))
  assert_true(tracker_can_use(tracker1, "res2"))
  assert_false(tracker_can_use(tracker1, "res3"))  // Not in limits
  
  // Use resource1
  let tracker2_result = tracker_use_resource(tracker1, "res1")
  match tracker2_result {
    Ok(tracker2) => {
      assert_eq(tracker_get_usage(tracker2, "res1"), 1)
      assert_true(tracker_can_use(tracker2, "res1"))
      
      // Use resource1 again
      let tracker3_result = tracker_use_resource(tracker2, "res1")
      match tracker3_result {
        Ok(tracker3) => {
          assert_eq(tracker_get_usage(tracker3, "res1"), 2)
          assert_true(tracker_can_use(tracker3, "res1"))
          
          // Use resource1 third time
          let tracker4_result = tracker_use_resource(tracker3, "res1")
          match tracker4_result {
            Ok(tracker4) => {
              assert_eq(tracker_get_usage(tracker4, "res1"), 3)
              assert_false(tracker_can_use(tracker4, "res1"))  // Limit reached
              
              // Try to use resource1 again - should fail
              let tracker5_result = tracker_use_resource(tracker4, "res1")
              match tracker5_result {
                Ok(_) => assert_true(false)
                Err(msg) => assert_eq(msg, "Resource usage limit exceeded")
              }
              
              // Use resource2
              let tracker6_result = tracker_use_resource(tracker4, "res2")
              match tracker6_result {
                Ok(tracker6) => {
                  assert_eq(tracker_get_usage(tracker6, "res2"), 1)
                  assert_true(tracker_can_use(tracker6, "res2"))
                  
                  // Use resource2 again
                  let tracker7_result = tracker_use_resource(tracker6, "res2")
                  match tracker7_result {
                    Ok(tracker7) => {
                      assert_eq(tracker_get_usage(tracker7, "res2"), 2)
                      assert_false(tracker_can_use(tracker7, "res2"))  // Limit reached
                    }
                    Err(_) => assert_true(false)
                  }
                }
                Err(_) => assert_true(false)
              }
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 6: Resource Dependency Management
test "resource dependency management" {
  // Simulate resource dependencies and cleanup order
  type ResourceDependency = {
    resource_id: String,
    dependencies: Array<String>  // IDs of resources this depends on
  }
  
  type DependencyManager = {
    resources: Array<ResourceDependency>,
    cleanup_order: Array<String>
  }
  
  let create_manager = fn() -> DependencyManager {
    { resources: [], cleanup_order: [] }
  }
  
  let manager_add_resource = fn(manager: DependencyManager, resource_id: String, dependencies: Array<String>) -> DependencyManager {
    let resource = { resource_id: resource_id, dependencies: dependencies }
    { resources: manager.resources.push(resource), cleanup_order: manager.cleanup_order }
  }
  
  let manager_compute_cleanup_order = fn(manager: DependencyManager) -> Array<String> {
    // Topological sort for cleanup order (resources with no dependents first)
    let mut result = []
    let mut remaining_resources = manager.resources
    let mut processed = []
    
    while remaining_resources.length() > 0 {
      let mut found = false
      
      for i in 0..<remaining_resources.length() {
        let resource = remaining_resources[i]
        
        // Check if any other resource depends on this one
        let mut has_dependents = false
        for other in remaining_resources {
          if other.resource_id != resource.resource_id && other.dependencies.contains(resource.resource_id) {
            has_dependents = true
            break
          }
        }
        
        if !has_dependents && !processed.contains(resource.resource_id) {
          result = result.push(resource.resource_id)
          processed = processed.push(resource.resource_id)
          remaining_resources = remaining_resources.slice(0, i) + remaining_resources.slice(i + 1, remaining_resources.length())
          found = true
          break
        }
      }
      
      if !found {
        // Circular dependency - add remaining resources in any order
        for resource in remaining_resources {
          if !processed.contains(resource.resource_id) {
            result = result.push(resource.resource_id)
            processed = processed.push(resource.resource_id)
          }
        }
        break
      }
    }
    
    result
  }
  
  // Test dependency management
  let manager1 = create_manager()
  let manager2 = manager_add_resource(manager1, "res1", [])
  let manager3 = manager_add_resource(manager2, "res2", ["res1"])
  let manager4 = manager_add_resource(manager3, "res3", ["res1", "res2"])
  let manager5 = manager_add_resource(manager4, "res4", ["res3"])
  
  let cleanup_order = manager_compute_cleanup_order(manager5)
  assert_eq(cleanup_order.length(), 4)
  
  // res4 should be first (no dependents)
  assert_eq(cleanup_order[0], "res4")
  
  // res3 should be second (only res4 depends on it)
  assert_eq(cleanup_order[1], "res3")
  
  // res2 should be third (res3 depends on it)
  assert_eq(cleanup_order[2], "res2")
  
  // res1 should be last (res2 and res3 depend on it)
  assert_eq(cleanup_order[3], "res1")
  
  // Test with more complex dependencies
  let manager_complex = create_manager()
  let manager_complex1 = manager_add_resource(manager_complex, "db", [])
  let manager_complex2 = manager_add_resource(manager_complex1, "cache", ["db"])
  let manager_complex3 = manager_add_resource(manager_complex2, "api", ["cache", "db"])
  let manager_complex4 = manager_add_resource(manager_complex3, "ui", ["api"])
  
  let complex_cleanup_order = manager_compute_cleanup_order(manager_complex4)
  assert_eq(complex_cleanup_order.length(), 4)
  assert_eq(complex_cleanup_order[0], "ui")
  assert_eq(complex_cleanup_order[1], "api")
  assert_eq(complex_cleanup_order[2], "cache")
  assert_eq(complex_cleanup_order[3], "db")
}

// Test 7: Resource Leaks Detection
test "resource leaks detection" {
  // Simulate resource leak detection
  type ResourceLeak = {
    resource_id: String,
    allocated: Int,
    freed: Int,
    leak_detected: Bool
  }
  
  type LeakDetector = Array<ResourceLeak>
  
  let create_detector = fn() -> LeakDetector {
    []
  }
  
  let detector_track_allocation = fn(detector: LeakDetector, resource_id: String) -> LeakDetector {
    let mut found = false
    let mut result = detector
    
    for i in 0..<detector.length() {
      let leak = detector[i]
      if leak.resource_id == resource_id {
        let updated_leak = { 
          resource_id: leak.resource_id, 
          allocated: leak.allocated + 1, 
          freed: leak.freed, 
          leak_detected: leak.leak_detected 
        }
        result = result.with(i, updated_leak)
        found = true
        break
      }
    }
    
    if !found {
      let new_leak = { resource_id: resource_id, allocated: 1, freed: 0, leak_detected: false }
      result = result.push(new_leak)
    }
    
    result
  }
  
  let detector_track_free = fn(detector: LeakDetector, resource_id: String) -> LeakDetector {
    let mut result = detector
    
    for i in 0..<detector.length() {
      let leak = detector[i]
      if leak.resource_id == resource_id {
        let updated_leak = { 
          resource_id: leak.resource_id, 
          allocated: leak.allocated, 
          freed: leak.freed + 1, 
          leak_detected: leak.leak_detected 
        }
        result = result.with(i, updated_leak)
        break
      }
    }
    
    result
  }
  
  let detector_detect_leaks = fn(detector: LeakDetector) -> LeakDetector {
    let mut result = detector
    
    for i in 0..<detector.length() {
      let leak = detector[i]
      if leak.allocated > leak.freed {
        let updated_leak = { 
          resource_id: leak.resource_id, 
          allocated: leak.allocated, 
          freed: leak.freed, 
          leak_detected: true 
        }
        result = result.with(i, updated_leak)
      }
    }
    
    result
  }
  
  let detector_has_leaks = fn(detector: LeakDetector) -> Bool {
    for leak in detector {
      if leak.leak_detected {
        return true
      }
    }
    false
  }
  
  let detector_get_leaked_resources = fn(detector: LeakDetector) -> Array<String> {
    let mut result = []
    for leak in detector {
      if leak.leak_detected {
        result = result.push(leak.resource_id)
      }
    }
    result
  }
  
  // Test leak detection
  let detector1 = create_detector()
  let detector2 = detector_track_allocation(detector1, "res1")
  let detector3 = detector_track_allocation(detector2, "res1")
  let detector4 = detector_track_allocation(detector3, "res2")
  
  // Free some resources
  let detector5 = detector_track_free(detector4, "res1")
  
  // Detect leaks
  let detector6 = detector_detect_leaks(detector5)
  
  // res1 should have a leak (2 allocated, 1 freed)
  // res2 should have a leak (1 allocated, 0 freed)
  assert_true(detector_has_leaks(detector6))
  
  let leaked_resources = detector_get_leaked_resources(detector6)
  assert_eq(leaked_resources.length(), 2)
  assert_true(leaked_resources.contains("res1"))
  assert_true(leaked_resources.contains("res2"))
  
  // Free remaining resources
  let detector7 = detector_track_free(detector6, "res1")
  let detector8 = detector_track_free(detector7, "res2")
  
  // Detect leaks again
  let detector9 = detector_detect_leaks(detector8)
  
  // No leaks should be detected
  assert_false(detector_has_leaks(detector9))
  assert_eq(detector_get_leaked_resources(detector9).length(), 0)
}

// Test 8: Resource Priority Management
test "resource priority management" {
  // Simulate resource allocation based on priority
  type PriorityResource = {
    id: String,
    priority: Int,
    allocated: Bool
  }
  
  type PriorityManager = {
    resources: Array<PriorityResource>,
    allocation_order: Array<String>
  }
  
  let create_manager = fn() -> PriorityManager {
    { resources: [], allocation_order: [] }
  }
  
  let manager_add_resource = fn(manager: PriorityManager, id: String, priority: Int) -> PriorityManager {
    let resource = { id: id, priority: priority, allocated: false }
    { resources: manager.resources.push(resource), allocation_order: manager.allocation_order }
  }
  
  let manager_allocate_by_priority = fn(manager: PriorityManager, count: Int) -> PriorityManager {
    // Sort resources by priority (higher priority first)
    let mut sorted_resources = manager.resources
    // Simple bubble sort for demonstration
    let n = sorted_resources.length()
    for i in 0..<n {
      for j in 0..<(n - i - 1) {
        if sorted_resources[j].priority < sorted_resources[j + 1].priority {
          let temp = sorted_resources[j]
          sorted_resources = sorted_resources.with(j, sorted_resources[j + 1])
          sorted_resources = sorted_resources.with(j + 1, temp)
        }
      }
    }
    
    let mut result = manager
    let mut allocation_order = []
    let mut allocated = 0
    
    for resource in sorted_resources {
      if allocated >= count {
        break
      }
      
      // Find and update the resource in the original array
      for i in 0..<result.resources.length() {
        let res = result.resources[i]
        if res.id == resource.id && !res.allocated {
          let updated_resource = { id: res.id, priority: res.priority, allocated: true }
          result = { 
            resources: result.resources.with(i, updated_resource), 
            allocation_order: result.allocation_order 
          }
          allocation_order = allocation_order.push(res.id)
          allocated = allocated + 1
          break
        }
      }
    }
    
    { resources: result.resources, allocation_order: allocation_order }
  }
  
  let manager_get_allocated_resources = fn(manager: PriorityManager) -> Array<String> {
    let mut result = []
    for resource in manager.resources {
      if resource.allocated {
        result = result.push(resource.id)
      }
    }
    result
  }
  
  // Test priority-based allocation
  let manager1 = create_manager()
  let manager2 = manager_add_resource(manager1, "low1", 1)
  let manager3 = manager_add_resource(manager2, "high1", 10)
  let manager4 = manager_add_resource(manager3, "medium1", 5)
  let manager5 = manager_add_resource(manager4, "high2", 10)
  let manager6 = manager_add_resource(manager5, "low2", 1)
  
  // Allocate 3 resources with highest priority
  let manager7 = manager_allocate_by_priority(manager6, 3)
  let allocated_resources = manager_get_allocated_resources(manager7)
  
  assert_eq(allocated_resources.length(), 3)
  
  // Should allocate high1, high2 (both priority 10) and medium1 (priority 5)
  assert_true(allocated_resources.contains("high1"))
  assert_true(allocated_resources.contains("high2"))
  assert_true(allocated_resources.contains("medium1"))
  assert_false(allocated_resources.contains("low1"))
  assert_false(allocated_resources.contains("low2"))
  
  // Check allocation order
  assert_eq(manager7.allocation_order.length(), 3)
  assert_eq(manager7.allocation_order[0], "high1")
  assert_eq(manager7.allocation_order[1], "high2")
  assert_eq(manager7.allocation_order[2], "medium1")
}