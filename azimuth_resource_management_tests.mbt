// Azimuth Resource Management Tests
// This file contains comprehensive test cases for resource management and cleanup

// Test 1: Resource Pool Management
test "resource pool management and reuse" {
  type Resource[T] {
    id : Int
    data : T
    mut in_use : Bool
    mut last_used : Int
  }
  
  type ResourcePool[T] {
    mut resources : Array[Resource[T]]
    mut next_id : Int
    max_size : Int
    mut current_time : Int
  }
  
  let create_resource = fn[T](id : Int, data : T) : Resource[T] {
    {
      id: id,
      data: data,
      in_use: false,
      last_used: 0
    }
  }
  
  let create_pool = fn[T](max_size : Int) : ResourcePool[T] {
    {
      resources: [],
      next_id: 1,
      max_size: max_size,
      current_time: 0
    }
  }
  
  let advance_time = fn[T](pool : ResourcePool[T], amount : Int) {
    pool.current_time = pool.current_time + amount
  }
  
  let find_available_resource = fn[T](pool : ResourcePool[T]) : Option[Resource[T]] {
    for resource in pool.resources {
      if !resource.in_use {
        return Some(resource)
      }
    }
    None
  }
  
  let acquire_resource = fn[T](pool : ResourcePool[T], factory : () -> T) : Option[Resource[T]] {
    // Try to find an available resource
    match find_available_resource(pool) {
      Some(resource) => {
        resource.in_use = true
        resource.last_used = pool.current_time
        Some(resource)
      }
      None => {
        // Try to create a new resource if pool not full
        if pool.resources.length() < pool.max_size {
          let resource = create_resource(pool.next_id, factory())
          resource.in_use = true
          resource.last_used = pool.current_time
          pool.resources = pool.resources.push(resource)
          pool.next_id = pool.next_id + 1
          Some(resource)
        } else {
          None
        }
      }
    }
  }
  
  let release_resource = fn[T](pool : ResourcePool[T], resource_id : Int) : Bool {
    for resource in pool.resources {
      if resource.id == resource_id && resource.in_use {
        resource.in_use = false
        resource.last_used = pool.current_time
        return true
      }
    }
    false
  }
  
  let get_available_count = fn[T](pool : ResourcePool[T]) : Int {
    let mut count = 0
    for resource in pool.resources {
      if !resource.in_use {
        count = count + 1
      }
    }
    count
  }
  
  let get_in_use_count = fn[T](pool : ResourcePool[T]) : Int {
    let mut count = 0
    for resource in pool.resources {
      if resource.in_use {
        count = count + 1
      }
    }
    count
  }
  
  let cleanup_expired_resources = fn[T](pool : ResourcePool[T], max_idle_time : Int) {
    let mut new_resources = []
    
    for resource in pool.resources {
      if resource.in_use || (pool.current_time - resource.last_used) <= max_idle_time {
        new_resources = new_resources.push(resource)
      }
    }
    
    pool.resources = new_resources
  }
  
  // Test resource pool
  let pool = create_pool[String](3)
  
  // Test initial state
  assert_eq(pool.resources.length(), 0)
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 0)
  
  // Test acquiring resources
  let resource1 = acquire_resource(pool, fn() { "Resource 1" })
  assert_true(resource1.is_some())
  assert_eq(pool.resources.length(), 1)
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 1)
  
  let resource2 = acquire_resource(pool, fn() { "Resource 2" })
  assert_true(resource2.is_some())
  assert_eq(pool.resources.length(), 2)
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 2)
  
  // Test releasing resources
  match resource1 {
    Some(r1) => {
      assert_true(release_resource(pool, r1.id))
      assert_eq(get_available_count(pool), 1)
      assert_eq(get_in_use_count(pool), 1)
    }
    None => assert_true(false)
  }
  
  // Test reusing released resources
  let resource3 = acquire_resource(pool, fn() { "Resource 3" })
  assert_true(resource3.is_some())
  assert_eq(pool.resources.length(), 2)  // Should not create new resource
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 2)
  
  // Test pool capacity limit
  let resource4 = acquire_resource(pool, fn() { "Resource 4" })
  assert_true(resource4.is_some())
  assert_eq(pool.resources.length(), 3)  // Should create new resource
  assert_eq(get_available_count(pool), 0)
  assert_eq(get_in_use_count(pool), 3)
  
  let resource5 = acquire_resource(pool, fn() { "Resource 5" })
  assert_true(resource5.is_none())  // Pool is full
  
  // Test cleanup of expired resources
  advance_time(pool, 100)
  
  match resource2 {
    Some(r2) => {
      release_resource(pool, r2.id)
    }
    None => assert_true(false)
  }
  
  match resource3 {
    Some(r3) => {
      r3.last_used = pool.current_time - 50  // Make it appear older
    }
    None => assert_true(false)
  }
  
  advance_time(pool, 50)
  cleanup_expired_resources(pool, 60)  // Clean up resources idle for more than 60 time units
  
  assert_eq(pool.resources.length(), 2)  // One resource should be cleaned up
}

// Test 2: Memory Management and Leak Detection
test "memory management and leak detection" {
  type MemoryBlock {
    id : Int
    size : Int
    mut allocated : Bool
    mut allocation_time : Int
  }
  
  type MemoryManager {
    mut blocks : Array[MemoryBlock]
    mut total_allocated : Int
    mut peak_usage : Int
    mut current_time : Int
  }
  
  let create_memory_manager = fn() : MemoryManager {
    {
      blocks: [],
      total_allocated: 0,
      peak_usage: 0,
      current_time: 0
    }
  }
  
  let advance_time = fn(manager : MemoryManager, amount : Int) {
    manager.current_time = manager.current_time + amount
  }
  
  let allocate_block = fn(manager : MemoryManager, size : Int) : Option[Int] {
    let block_id = manager.blocks.length() + 1
    let block = {
      id: block_id,
      size: size,
      allocated: true,
      allocation_time: manager.current_time
    }
    
    manager.blocks = manager.blocks.push(block)
    manager.total_allocated = manager.total_allocated + size
    
    if manager.total_allocated > manager.peak_usage {
      manager.peak_usage = manager.total_allocated
    }
    
    Some(block_id)
  }
  
  let deallocate_block = fn(manager : MemoryManager, block_id : Int) : Bool {
    let mut found = false
    
    for block in manager.blocks {
      if block.id == block_id && block.allocated {
        block.allocated = false
        manager.total_allocated = manager.total_allocated - block.size
        found = true
        break
      }
    }
    
    found
  }
  
  let get_allocated_count = fn(manager : MemoryManager) : Int {
    let mut count = 0
    for block in manager.blocks {
      if block.allocated {
        count = count + 1
      }
    }
    count
  }
  
  let get_total_allocated = fn(manager : MemoryManager) : Int {
    manager.total_allocated
  }
  
  let get_peak_usage = fn(manager : MemoryManager) : Int {
    manager.peak_usage
  }
  
  let detect_leaks = fn(manager : MemoryManager, max_age : Int) : Array[Int] {
    let mut leaked_blocks = []
    
    for block in manager.blocks {
      if block.allocated && (manager.current_time - block.allocation_time) > max_age {
        leaked_blocks = leaked_blocks.push(block.id)
      }
    }
    
    leaked_blocks
  }
  
  let get_block_info = fn(manager : MemoryManager, block_id : Int) : Option[(Int, Int, Int, Bool)] {
    for block in manager.blocks {
      if block.id == block_id {
        return Some((block.id, block.size, block.allocation_time, block.allocated))
      }
    }
    None
  }
  
  // Test memory management
  let manager = create_memory_manager()
  
  // Test initial state
  assert_eq(get_allocated_count(manager), 0)
  assert_eq(get_total_allocated(manager), 0)
  assert_eq(get_peak_usage(manager), 0)
  
  // Test allocation
  let block1_id = allocate_block(manager, 100)
  let block2_id = allocate_block(manager, 200)
  let block3_id = allocate_block(manager, 150)
  
  assert_true(block1_id.is_some())
  assert_true(block2_id.is_some())
  assert_true(block3_id.is_some())
  
  assert_eq(get_allocated_count(manager), 3)
  assert_eq(get_total_allocated(manager), 450)
  assert_eq(get_peak_usage(manager), 450)
  
  // Test deallocation
  match block2_id {
    Some(id) => {
      assert_true(deallocate_block(manager, id))
      assert_eq(get_allocated_count(manager), 2)
      assert_eq(get_total_allocated(manager), 250)
      assert_eq(get_peak_usage(manager), 450)  // Peak should not decrease
    }
    None => assert_true(false)
  }
  
  // Test block info
  match block1_id {
    Some(id) => {
      match get_block_info(manager, id) {
        Some((block_id, size, alloc_time, allocated)) => {
          assert_eq(block_id, id)
          assert_eq(size, 100)
          assert_true(allocated)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test leak detection
  advance_time(manager, 100)
  
  match block1_id {
    Some(id) => {
      assert_true(deallocate_block(manager, id))
    }
    None => assert_true(false)
  }
  
  advance_time(manager, 100)
  
  // Only block3 should be left and should be considered a leak
  let leaks = detect_leaks(manager, 150)
  assert_eq(leaks.length(), 1)
  
  match block3_id {
    Some(id) => {
      assert_true(leaks.contains(id))
    }
    None => assert_true(false)
  }
  
  // Test deallocation of leaked block
  match block3_id {
    Some(id) => {
      assert_true(deallocate_block(manager, id))
      assert_eq(get_allocated_count(manager), 0)
      assert_eq(get_total_allocated(manager), 0)
    }
    None => assert_true(false)
  }
}

// Test 3: Resource Lifecycle Management
test "resource lifecycle management" {
  type ResourceState {
    Created
    Initialized
    Active
    Deactivating
    Deactivated
    Destroyed
  }
  
  type ManagedResource {
    id : Int
    name : String
    mut state : ResourceState
    mut created_time : Int
    mut initialized_time : Option[Int]
    mut activated_time : Option[Int]
    mut deactivated_time : Option[Int]
    mut destroyed_time : Option[Int]
  }
  
  type ResourceManager {
    mut resources : Array[ManagedResource]
    mut next_id : Int
    mut current_time : Int
  }
  
  let create_resource_manager = fn() : ResourceManager {
    {
      resources: [],
      next_id: 1,
      current_time: 0
    }
  }
  
  let advance_time = fn(manager : ResourceManager, amount : Int) {
    manager.current_time = manager.current_time + amount
  }
  
  let create_resource = fn(manager : ResourceManager, name : String) : ManagedResource {
    let resource = {
      id: manager.next_id,
      name: name,
      state: ResourceState::Created,
      created_time: manager.current_time,
      initialized_time: None,
      activated_time: None,
      deactivated_time: None,
      destroyed_time: None
    }
    
    manager.resources = manager.resources.push(resource)
    manager.next_id = manager.next_id + 1
    resource
  }
  
  let initialize_resource = fn(resource : ManagedResource, current_time : Int) : Bool {
    match resource.state {
      ResourceState::Created => {
        resource.state = ResourceState::Initialized
        resource.initialized_time = Some(current_time)
        true
      }
      _ => false
    }
  }
  
  let activate_resource = fn(resource : ManagedResource, current_time : Int) : Bool {
    match resource.state {
      ResourceState::Initialized => {
        resource.state = ResourceState::Active
        resource.activated_time = Some(current_time)
        true
      }
      _ => false
    }
  }
  
  let deactivate_resource = fn(resource : ManagedResource, current_time : Int) : Bool {
    match resource.state {
      ResourceState::Active => {
        resource.state = ResourceState::Deactivating
        resource.deactivated_time = Some(current_time)
        true
      }
      _ => false
    }
  }
  
  let complete_deactivation = fn(resource : ManagedResource, current_time : Int) : Bool {
    match resource.state {
      ResourceState::Deactivating => {
        resource.state = ResourceState::Deactivated
        true
      }
      _ => false
    }
  }
  
  let destroy_resource = fn(resource : ManagedResource, current_time : Int) : Bool {
    match resource.state {
      ResourceState::Deactivated => {
        resource.state = ResourceState::Destroyed
        resource.destroyed_time = Some(current_time)
        true
      }
      _ => false
    }
  }
  
  let get_resource_by_id = fn(manager : ResourceManager, id : Int) : Option[ManagedResource] {
    for resource in manager.resources {
      if resource.id == id {
        return Some(resource)
      }
    }
    None
  }
  
  let get_resources_by_state = fn(manager : ResourceManager, state : ResourceState) : Array[ManagedResource> {
    let mut result = []
    for resource in manager.resources {
      match resource.state {
        ResourceState::Created => {
          match state {
            ResourceState::Created => result = result.push(resource)
            _ => ()
          }
        }
        ResourceState::Initialized => {
          match state {
            ResourceState::Initialized => result = result.push(resource)
            _ => ()
          }
        }
        ResourceState::Active => {
          match state {
            ResourceState::Active => result = result.push(resource)
            _ => ()
          }
        }
        ResourceState::Deactivating => {
          match state {
            ResourceState::Deactivating => result = result.push(resource)
            _ => ()
          }
        }
        ResourceState::Deactivated => {
          match state {
            ResourceState::Deactivated => result = result.push(resource)
            _ => ()
          }
        }
        ResourceState::Destroyed => {
          match state {
            ResourceState::Destroyed => result = result.push(resource)
            _ => ()
          }
        }
      }
    }
    result
  }
  
  // Test resource lifecycle
  let manager = create_resource_manager()
  
  // Test resource creation
  let resource1 = create_resource(manager, "TestResource1")
  let resource2 = create_resource(manager, "TestResource2")
  
  assert_eq(resource1.state, ResourceState::Created)
  assert_eq(resource2.state, ResourceState::Created)
  assert_eq(get_resources_by_state(manager, ResourceState::Created).length(), 2)
  
  advance_time(manager, 10)
  
  // Test resource initialization
  assert_true(initialize_resource(resource1, manager.current_time))
  assert_eq(resource1.state, ResourceState::Initialized)
  assert_true(resource1.initialized_time.is_some())
  
  advance_time(manager, 5)
  
  assert_true(initialize_resource(resource2, manager.current_time))
  assert_eq(resource2.state, ResourceState::Initialized)
  assert_true(resource2.initialized_time.is_some())
  
  assert_eq(get_resources_by_state(manager, ResourceState::Initialized).length(), 2)
  
  // Test resource activation
  advance_time(manager, 5)
  
  assert_true(activate_resource(resource1, manager.current_time))
  assert_eq(resource1.state, ResourceState::Active)
  assert_true(resource1.activated_time.is_some())
  
  advance_time(manager, 5)
  
  assert_true(activate_resource(resource2, manager.current_time))
  assert_eq(resource2.state, ResourceState::Active)
  assert_true(resource2.activated_time.is_some())
  
  assert_eq(get_resources_by_state(manager, ResourceState::Active).length(), 2)
  
  // Test invalid state transitions
  assert_false(initialize_resource(resource1, manager.current_time))  // Already initialized
  assert_false(activate_resource(resource1, manager.current_time))  // Already active
  
  // Test resource deactivation
  advance_time(manager, 10)
  
  assert_true(deactivate_resource(resource1, manager.current_time))
  assert_eq(resource1.state, ResourceState::Deactivating)
  assert_true(resource1.deactivated_time.is_some())
  
  advance_time(manager, 5)
  
  assert_true(complete_deactivation(resource1, manager.current_time))
  assert_eq(resource1.state, ResourceState::Deactivated)
  
  assert_eq(get_resources_by_state(manager, ResourceState::Active).length(), 1)
  assert_eq(get_resources_by_state(manager, ResourceState::Deactivated).length(), 1)
  
  // Test resource destruction
  advance_time(manager, 5)
  
  assert_true(destroy_resource(resource1, manager.current_time))
  assert_eq(resource1.state, ResourceState::Destroyed)
  assert_true(resource1.destroyed_time.is_some())
  
  assert_eq(get_resources_by_state(manager, ResourceState::Destroyed).length(), 1)
  
  // Test invalid destruction
  assert_false(destroy_resource(resource2, manager.current_time))  // Not deactivated yet
}

// Test 4: Resource Usage Tracking
test "resource usage tracking and metrics" {
  type UsageMetrics {
    total_requests : Int
    successful_requests : Int
    failed_requests : Int
    total_usage_time : Int
    peak_concurrent_usage : Int
    mut current_concurrent_usage : Int
  }
  
  type TrackedResource {
    id : Int
    mut in_use : Bool
    mut usage_count : Int
    mut total_usage_time : Int
    mut last_used_time : Int
  }
  
  type ResourceTracker {
    mut resources : Array[TrackedResource]
    mut metrics : UsageMetrics
    mut current_time : Int
  }
  
  let create_resource_tracker = fn() : ResourceTracker {
    {
      resources: [],
      metrics: {
        total_requests: 0,
        successful_requests: 0,
        failed_requests: 0,
        total_usage_time: 0,
        peak_concurrent_usage: 0,
        current_concurrent_usage: 0
      },
      current_time: 0
    }
  }
  
  let advance_time = fn(tracker : ResourceTracker, amount : Int) {
    tracker.current_time = tracker.current_time + amount
  }
  
  let add_resource = fn(tracker : ResourceTracker) : Int {
    let resource_id = tracker.resources.length() + 1
    let resource = {
      id: resource_id,
      in_use: false,
      usage_count: 0,
      total_usage_time: 0,
      last_used_time: 0
    }
    
    tracker.resources = tracker.resources.push(resource)
    resource_id
  }
  
  let acquire_tracked_resource = fn(tracker : ResourceTracker, resource_id : Int) : Bool {
    tracker.metrics.total_requests = tracker.metrics.total_requests + 1
    
    for resource in tracker.resources {
      if resource.id == resource_id && !resource.in_use {
        resource.in_use = true
        resource.usage_count = resource.usage_count + 1
        resource.last_used_time = tracker.current_time
        
        tracker.metrics.current_concurrent_usage = tracker.metrics.current_concurrent_usage + 1
        if tracker.metrics.current_concurrent_usage > tracker.metrics.peak_concurrent_usage {
          tracker.metrics.peak_concurrent_usage = tracker.metrics.current_concurrent_usage
        }
        
        tracker.metrics.successful_requests = tracker.metrics.successful_requests + 1
        return true
      }
    }
    
    tracker.metrics.failed_requests = tracker.metrics.failed_requests + 1
    false
  }
  
  let release_tracked_resource = fn(tracker : ResourceTracker, resource_id : Int) : Bool {
    for resource in tracker.resources {
      if resource.id == resource_id && resource.in_use {
        let usage_time = tracker.current_time - resource.last_used_time
        resource.total_usage_time = resource.total_usage_time + usage_time
        resource.in_use = false
        
        tracker.metrics.total_usage_time = tracker.metrics.total_usage_time + usage_time
        tracker.metrics.current_concurrent_usage = tracker.metrics.current_concurrent_usage - 1
        return true
      }
    }
    false
  }
  
  let get_resource_metrics = fn(tracker : ResourceTracker, resource_id : Int) : Option[(Int, Int, Int, Bool)] {
    for resource in tracker.resources {
      if resource.id == resource_id {
        return Some((resource.usage_count, resource.total_usage_time, resource.last_used_time, resource.in_use))
      }
    }
    None
  }
  
  let get_success_rate = fn(tracker : ResourceTracker) : Float {
    if tracker.metrics.total_requests == 0 {
      0.0
    } else {
      tracker.metrics.successful_requests.to_float() / tracker.metrics.total_requests.to_float()
    }
  }
  
  let get_average_usage_time = fn(tracker : ResourceTracker) : Float {
    if tracker.metrics.successful_requests == 0 {
      0.0
    } else {
      tracker.metrics.total_usage_time.to_float() / tracker.metrics.successful_requests.to_float()
    }
  }
  
  // Test resource usage tracking
  let tracker = create_resource_tracker()
  
  // Add resources
  let resource1_id = add_resource(tracker)
  let resource2_id = add_resource(tracker)
  let resource3_id = add_resource(tracker)
  
  // Test initial metrics
  assert_eq(tracker.metrics.total_requests, 0)
  assert_eq(tracker.metrics.successful_requests, 0)
  assert_eq(tracker.metrics.failed_requests, 0)
  assert_eq(tracker.metrics.current_concurrent_usage, 0)
  assert_eq(tracker.metrics.peak_concurrent_usage, 0)
  
  // Test resource acquisition
  assert_true(acquire_tracked_resource(tracker, resource1_id))
  assert_eq(tracker.metrics.total_requests, 1)
  assert_eq(tracker.metrics.successful_requests, 1)
  assert_eq(tracker.metrics.current_concurrent_usage, 1)
  assert_eq(tracker.metrics.peak_concurrent_usage, 1)
  
  assert_true(acquire_tracked_resource(tracker, resource2_id))
  assert_eq(tracker.metrics.total_requests, 2)
  assert_eq(tracker.metrics.successful_requests, 2)
  assert_eq(tracker.metrics.current_concurrent_usage, 2)
  assert_eq(tracker.metrics.peak_concurrent_usage, 2)
  
  // Test failed acquisition (already in use)
  assert_false(acquire_tracked_resource(tracker, resource1_id))
  assert_eq(tracker.metrics.total_requests, 3)
  assert_eq(tracker.metrics.successful_requests, 2)
  assert_eq(tracker.metrics.failed_requests, 1)
  assert_eq(tracker.metrics.current_concurrent_usage, 2)
  
  advance_time(tracker, 10)
  
  // Test resource release
  assert_true(release_tracked_resource(tracker, resource1_id))
  assert_eq(tracker.metrics.current_concurrent_usage, 1)
  assert_eq(tracker.metrics.total_usage_time, 10)
  
  advance_time(tracker, 5)
  
  assert_true(release_tracked_resource(tracker, resource2_id))
  assert_eq(tracker.metrics.current_concurrent_usage, 0)
  assert_eq(tracker.metrics.total_usage_time, 15)
  
  // Test resource metrics
  match get_resource_metrics(tracker, resource1_id) {
    Some((usage_count, total_usage_time, last_used_time, in_use)) => {
      assert_eq(usage_count, 1)
      assert_eq(total_usage_time, 10)
      assert_eq(last_used_time, 10)
      assert_false(in_use)
    }
    None => assert_true(false)
  }
  
  // Test more usage cycles
  assert_true(acquire_tracked_resource(tracker, resource1_id))
  advance_time(tracker, 8)
  assert_true(release_tracked_resource(tracker, resource1_id))
  
  assert_true(acquire_tracked_resource(tracker, resource3_id))
  advance_time(tracker, 12)
  assert_true(release_tracked_resource(tracker, resource3_id))
  
  // Test final metrics
  assert_eq(tracker.metrics.total_requests, 6)
  assert_eq(tracker.metrics.successful_requests, 5)
  assert_eq(tracker.metrics.failed_requests, 1)
  assert_eq(tracker.metrics.total_usage_time, 35)
  assert_eq(tracker.metrics.current_concurrent_usage, 0)
  assert_eq(tracker.metrics.peak_concurrent_usage, 2)
  
  // Test calculated metrics
  let success_rate = get_success_rate(tracker)
  assert_true(success_rate > 0.8 && success_rate < 0.9)  // 5/6 ≈ 0.833
  
  let avg_usage_time = get_average_usage_time(tracker)
  assert_true(avg_usage_time > 6.0 && avg_usage_time < 8.0)  // 35/5 = 7.0
  
  // Test resource metrics after multiple uses
  match get_resource_metrics(tracker, resource1_id) {
    Some((usage_count, total_usage_time, last_used_time, in_use)) => {
      assert_eq(usage_count, 2)
      assert_eq(total_usage_time, 18)  // 10 + 8
      assert_eq(last_used_time, 33)   // 10 + 5 + 8 + 10
      assert_false(in_use)
    }
    None => assert_true(false)
  }
}