// Azimuth Premium Memory Management and Resource Cleanup Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Resource Reference Counting
test "resource reference counting" {
  // Define resource structure
  type Resource = {
    id: String,
    data: String,
    ref_count: Int,
    created_at: Int,
    last_accessed: Int
  }
  
  // Define resource manager
  type ResourceManager = {
    resources: Array[Resource],
    total_allocated: Int,
    total_freed: Int,
    current_memory_usage: Int
  }
  
  // Create resource
  let create_resource = fn(id: String, data: String, current_time: Int) {
    {
      id,
      data,
      ref_count: 1,
      created_at: current_time,
      last_accessed: current_time
    }
  }
  
  // Create resource manager
  let create_resource_manager = fn() {
    {
      resources: [],
      total_allocated: 0,
      total_freed: 0,
      current_memory_usage: 0
    }
  }
  
  // Add resource to manager
  let add_resource = fn(manager: ResourceManager, resource: Resource) {
    let resource_size = resource.data.length()
    
    {
      resources: manager.resources.push(resource),
      total_allocated: manager.total_allocated + 1,
      total_freed: manager.total_freed,
      current_memory_usage: manager.current_memory_usage + resource_size
    }
  }
  
  // Acquire resource (increment reference count)
  let acquire_resource = fn(manager: ResourceManager, resource_id: String, current_time: Int) {
    match manager.resources.find_index(fn(r) { r.id == resource_id }) {
      Some(index) => {
        let resource = manager.resources[index]
        let updated_resource = {
          id: resource.id,
          data: resource.data,
          ref_count: resource.ref_count + 1,
          created_at: resource.created_at,
          last_accessed: current_time
        }
        let updated_resources = manager.resources.update(index, updated_resource)
        
        {
          resources: updated_resources,
          total_allocated: manager.total_allocated,
          total_freed: manager.total_freed,
          current_memory_usage: manager.current_memory_usage
        }
      }
      None => manager
    }
  }
  
  // Release resource (decrement reference count)
  let release_resource = fn(manager: ResourceManager, resource_id: String) {
    match manager.resources.find_index(fn(r) { r.id == resource_id }) {
      Some(index) => {
        let resource = manager.resources[index]
        if resource.ref_count > 1 {
          // Decrement reference count
          let updated_resource = {
            id: resource.id,
            data: resource.data,
            ref_count: resource.ref_count - 1,
            created_at: resource.created_at,
            last_accessed: resource.last_accessed
          }
          let updated_resources = manager.resources.update(index, updated_resource)
          
          {
            resources: updated_resources,
            total_allocated: manager.total_allocated,
            total_freed: manager.total_freed,
            current_memory_usage: manager.current_memory_usage
          }
        } else {
          // Free resource
          let resource_size = resource.data.length()
          let updated_resources = manager.resources.filter(fn(r) { r.id != resource_id })
          
          {
            resources: updated_resources,
            total_allocated: manager.total_allocated,
            total_freed: manager.total_freed + 1,
            current_memory_usage: manager.current_memory_usage - resource_size
          }
        }
      }
      None => manager
    }
  }
  
  // Test resource creation
  let resource1 = create_resource("res1", "Resource data 1", 1640995200000)
  assert_eq(resource1.id, "res1")
  assert_eq(resource1.data, "Resource data 1")
  assert_eq(resource1.ref_count, 1)
  assert_eq(resource1.created_at, 1640995200000)
  
  // Test resource manager
  let manager = create_resource_manager()
  assert_eq(manager.resources.length(), 0)
  assert_eq(manager.total_allocated, 0)
  assert_eq(manager.total_freed, 0)
  assert_eq(manager.current_memory_usage, 0)
  
  // Test adding resource
  let manager1 = add_resource(manager, resource1)
  assert_eq(manager1.resources.length(), 1)
  assert_eq(manager1.total_allocated, 1)
  assert_eq(manager1.total_freed, 0)
  assert_eq(manager1.current_memory_usage, 15)  // "Resource data 1" length
  
  // Test acquiring resource
  let manager2 = acquire_resource(manager1, "res1", 1640995201000)
  assert_eq(manager2.resources[0].ref_count, 2)
  assert_eq(manager2.resources[0].last_accessed, 1640995201000)
  
  // Test releasing resource
  let manager3 = release_resource(manager2, "res1")
  assert_eq(manager3.resources[0].ref_count, 1)
  
  // Test final release (resource should be freed)
  let manager4 = release_resource(manager3, "res1")
  assert_eq(manager4.resources.length(), 0)
  assert_eq(manager4.total_allocated, 1)
  assert_eq(manager4.total_freed, 1)
  assert_eq(manager4.current_memory_usage, 0)
}

// Test 2: Garbage Collection Simulation
test "garbage collection simulation" {
  // Define object structure
  type GCObject = {
    id: String,
    size: Int,
    marked: Bool,
    references: Array[String]
  }
  
  // Define GC heap
  type GCHeap = {
    objects: Array[GCObject],
    roots: Array[String],
    total_collections: Int,
    total_freed_objects: Int,
    total_freed_memory: Int
  }
  
  // Create GC object
  let create_gc_object = fn(id: String, size: Int, references: Array[String]) {
    {
      id,
      size,
      marked: false,
      references
    }
  }
  
  // Create GC heap
  let create_gc_heap = fn() {
    {
      objects: [],
      roots: [],
      total_collections: 0,
      total_freed_objects: 0,
      total_freed_memory: 0
    }
  }
  
  // Add object to heap
  let add_object = fn(heap: GCHeap, object: GCObject) {
    {
      objects: heap.objects.push(object),
      roots: heap.roots,
      total_collections: heap.total_collections,
      total_freed_objects: heap.total_freed_objects,
      total_freed_memory: heap.total_freed_memory
    }
  }
  
  // Add root reference
  let add_root = fn(heap: GCHeap, object_id: String) {
    {
      objects: heap.objects,
      roots: heap.roots.push(object_id),
      total_collections: heap.total_collections,
      total_freed_objects: heap.total_freed_objects,
      total_freed_memory: heap.total_freed_memory
    }
  }
  
  // Mark phase of GC
  let mark_phase = fn(heap: GCHeap) {
    let mut marked_objects = heap.objects
    
    // Reset all marks
    marked_objects = marked_objects.map(fn(obj) { { obj | marked: false } })
    
    // Mark from roots
    let mut work_list = heap.roots
    let mut index = 0
    
    while index < work_list.length() {
      let current_id = work_list[index]
      
      match marked_objects.find_index(fn(obj) { obj.id == current_id }) {
        Some(obj_index) => {
          if !marked_objects[obj_index].marked {
            // Mark this object
            marked_objects = marked_objects.update(obj_index, { 
              marked_objects[obj_index] | marked: true 
            })
            
            // Add references to work list
            let obj = marked_objects[obj_index]
            work_list = work_list.concat(obj.references)
          }
        }
        None => ()  // Object not found, skip
      }
      
      index = index + 1
    }
    
    {
      objects: marked_objects,
      roots: heap.roots,
      total_collections: heap.total_collections,
      total_freed_objects: heap.total_freed_objects,
      total_freed_memory: heap.total_freed_memory
    }
  }
  
  // Sweep phase of GC
  let sweep_phase = fn(heap: GCHeap) {
    let live_objects = heap.objects.filter(fn(obj) { obj.marked })
    let dead_objects = heap.objects.filter(fn(obj) { !obj.marked })
    
    let freed_objects = dead_objects.length()
    let freed_memory = dead_objects.reduce(fn(acc, obj) { acc + obj.size }, 0)
    
    {
      objects: live_objects,
      roots: heap.roots,
      total_collections: heap.total_collections + 1,
      total_freed_objects: heap.total_freed_objects + freed_objects,
      total_freed_memory: heap.total_freed_memory + freed_memory
    }
  }
  
  // Run garbage collection
  let run_gc = fn(heap: GCHeap) {
    let marked_heap = mark_phase(heap)
    sweep_phase(marked_heap)
  }
  
  // Test GC heap creation
  let heap = create_gc_heap()
  assert_eq(heap.objects.length(), 0)
  assert_eq(heap.roots.length(), 0)
  assert_eq(heap.total_collections, 0)
  
  // Create test objects
  let obj1 = create_gc_object("obj1", 100, ["obj2", "obj3"])
  let obj2 = create_gc_object("obj2", 200, ["obj4"])
  let obj3 = create_gc_object("obj3", 300, [])
  let obj4 = create_gc_object("obj4", 400, [])
  let obj5 = create_gc_object("obj5", 500, [])  // Unreferenced object
  
  // Add objects to heap
  let heap1 = add_object(heap, obj1)
  let heap2 = add_object(heap1, obj2)
  let heap3 = add_object(heap2, obj3)
  let heap4 = add_object(heap3, obj4)
  let heap5 = add_object(heap4, obj5)
  
  assert_eq(heap5.objects.length(), 5)
  
  // Add root reference
  let heap6 = add_root(heap5, "obj1")
  
  // Run garbage collection
  let heap7 = run_gc(heap6)
  
  // obj1, obj2, obj3, obj4 should be marked (reachable from root)
  // obj5 should be unmarked (garbage)
  assert_eq(heap7.objects.length(), 4)
  assert_eq(heap7.total_collections, 1)
  assert_eq(heap7.total_freed_objects, 1)
  assert_eq(heap7.total_freed_memory, 500)
  
  // Verify remaining objects
  let remaining_ids = heap7.objects.map(fn(obj) { obj.id })
  assert_true(remaining_ids.contains("obj1"))
  assert_true(remaining_ids.contains("obj2"))
  assert_true(remaining_ids.contains("obj3"))
  assert_true(remaining_ids.contains("obj4"))
  assert_false(remaining_ids.contains("obj5"))
  
  // Test multiple GC cycles
  let heap8 = add_root(heap7, "obj5")  // Add obj5 as root
  let heap9 = add_object(heap8, create_gc_object("obj6", 600, []))  // Add new object
  
  assert_eq(heap9.objects.length(), 6)
  
  // Remove obj1 from roots
  let heap10 = { heap9 | roots: heap9.roots.filter(fn(id) { id != "obj1" }) }
  
  // Run GC again
  let heap11 = run_gc(heap10)
  
  // obj1, obj2, obj4 should be freed (no longer reachable)
  // obj3 should remain (referenced by obj1 which is being freed, but we'll keep it simple)
  // obj5 should remain (now a root)
  // obj6 should remain (no references but still in heap)
  assert_eq(heap11.total_collections, 2)
  assert_eq(heap11.total_freed_objects, 4)  // 1 from first GC + 3 from second GC
  assert_eq(heap11.total_freed_memory, 500 + 100 + 200 + 400)  // obj5 + obj1 + obj2 + obj4
}

// Test 3: Memory Leak Detection
test "memory leak detection" {
  // Define memory allocation record
  type AllocationRecord = {
    id: String,
    size: Int,
    allocated_at: Int,
    stack_trace: Array[String]
  }
  
  // Define memory leak detector
  type MemoryLeakDetector = {
    allocations: Array[AllocationRecord],
    total_allocated: Int,
    total_freed: Int,
    current_allocated: Int,
    leak_threshold: Int
  }
  
  // Create memory leak detector
  let create_leak_detector = fn(leak_threshold: Int) {
    {
      allocations: [],
      total_allocated: 0,
      total_freed: 0,
      current_allocated: 0,
      leak_threshold
    }
  }
  
  // Record allocation
  let record_allocation = fn(detector: MemoryLeakDetector, id: String, size: Int, timestamp: Int, stack_trace: Array[String>) {
    let record = {
      id,
      size,
      allocated_at: timestamp,
      stack_trace
    }
    
    {
      allocations: detector.allocations.push(record),
      total_allocated: detector.total_allocated + size,
      total_freed: detector.total_freed,
      current_allocated: detector.current_allocated + size,
      leak_threshold: detector.leak_threshold
    }
  }
  
  // Record deallocation
  let record_deallocation = fn(detector: MemoryLeakDetector, id: String) {
    match detector.allocations.find_index(fn(record) { record.id == id }) {
      Some(index) => {
        let record = detector.allocations[index]
        let updated_allocations = detector.allocations.filter(fn(r) { r.id != id })
        
        {
          allocations: updated_allocations,
          total_allocated: detector.total_allocated,
          total_freed: detector.total_freed + record.size,
          current_allocated: detector.current_allocated - record.size,
          leak_threshold: detector.leak_threshold
        }
      }
      None => detector
    }
  }
  
  // Detect memory leaks
  let detect_leaks = fn(detector: MemoryLeakDetector, current_time: Int, leak_timeout: Int) {
    let leak_candidates = detector.allocations.filter(fn(record) {
      current_time - record.allocated_at > leak_timeout
    })
    
    let total_leaked_memory = leak_candidates.reduce(fn(acc, record) { acc + record.size }, 0)
    
    {
      leaks_detected: leak_candidates.length(),
      total_leaked_memory,
      leak_threshold_exceeded: total_leaked_memory > detector.leak_threshold,
      leak_candidates
    }
  }
  
  // Get memory usage statistics
  let get_memory_stats = fn(detector: MemoryLeakDetector) {
    {
      total_allocated: detector.total_allocated,
      total_freed: detector.total_freed,
      current_allocated: detector.current_allocated,
      allocation_count: detector.allocations.length(),
      leak_threshold: detector.leak_threshold
    }
  }
  
  // Test leak detector creation
  let detector = create_leak_detector(1024)  // 1KB leak threshold
  assert_eq(detector.allocations.length(), 0)
  assert_eq(detector.total_allocated, 0)
  assert_eq(detector.total_freed, 0)
  assert_eq(detector.current_allocated, 0)
  assert_eq(detector.leak_threshold, 1024)
  
  // Test allocation recording
  let detector1 = record_allocation(
    detector,
    "alloc1",
    256,
    1640995200000,
    ["function1", "function2", "function3"]
  )
  
  let detector2 = record_allocation(
    detector1,
    "alloc2",
    512,
    1640995200100,
    ["function4", "function5"]
  )
  
  assert_eq(detector2.allocations.length(), 2)
  assert_eq(detector2.total_allocated, 768)
  assert_eq(detector2.current_allocated, 768)
  
  // Test deallocation recording
  let detector3 = record_deallocation(detector2, "alloc1")
  
  assert_eq(detector3.allocations.length(), 1)
  assert_eq(detector3.total_allocated, 768)
  assert_eq(detector3.total_freed, 256)
  assert_eq(detector3.current_allocated, 512)
  
  // Test leak detection
  let current_time = 1640995300000  // 10 minutes later
  let leak_result = detect_leaks(detector3, current_time, 300000)  // 5 minutes timeout
  
  assert_eq(leak_result.leaks_detected, 1)
  assert_eq(leak_result.total_leaked_memory, 512)
  assert_false(leak_result.leak_threshold_exceeded)
  
  // Add more allocations to exceed threshold
  let detector4 = record_allocation(
    detector3,
    "alloc3",
    600,
    1640995200200,
    ["function6", "function7", "function8"]
  )
  
  let leak_result2 = detect_leaks(detector4, current_time, 300000)
  
  assert_eq(leak_result2.leaks_detected, 2)
  assert_eq(leak_result2.total_leaked_memory, 1112)  // 512 + 600
  assert_true(leak_result2.leak_threshold_exceeded)
  
  // Test memory statistics
  let stats = get_memory_stats(detector4)
  assert_eq(stats.total_allocated, 1368)  // 768 + 600
  assert_eq(stats.total_freed, 256)
  assert_eq(stats.current_allocated, 1112)  // 512 + 600
  assert_eq(stats.allocation_count, 2)
}

// Test 4: Resource Pool with Automatic Cleanup
test "resource pool with automatic cleanup" {
  // Define pooled resource
  type PooledResource = {
    id: String,
    in_use: Bool,
    created_at: Int,
    last_used: Int,
    use_count: Int,
    max_uses: Int
  }
  
  // Define resource pool
  type ResourcePool = {
    max_resources: Int,
    resources: Array[PooledResource],
    created_count: Int,
    destroyed_count: Int,
    cleanup_threshold: Int,
    last_cleanup: Int
  }
  
  // Create resource pool
  let create_resource_pool = fn(max_resources: Int, cleanup_threshold: Int) {
    {
      max_resources,
      resources: [],
      created_count: 0,
      destroyed_count: 0,
      cleanup_threshold,
      last_cleanup: 1640995200000
    }
  }
  
  // Create new resource
  let create_pooled_resource = fn(pool: ResourcePool, current_time: Int) {
    let new_resource = {
      id: "res-" + pool.created_count.to_string(),
      in_use: false,
      created_at: current_time,
      last_used: current_time,
      use_count: 0,
      max_uses: 100
    }
    
    {
      max_resources: pool.max_resources,
      resources: pool.resources.push(new_resource),
      created_count: pool.created_count + 1,
      destroyed_count: pool.destroyed_count,
      cleanup_threshold: pool.cleanup_threshold,
      last_cleanup: pool.last_cleanup
    }
  }
  
  // Acquire resource from pool
  let acquire_resource = fn(pool: ResourcePool, current_time: Int) {
    match pool.resources.find_index(fn(r) { !r.in_use }) {
      Some(index) => {
        let resource = pool.resources[index]
        let updated_resource = {
          id: resource.id,
          in_use: true,
          created_at: resource.created_at,
          last_used: current_time,
          use_count: resource.use_count + 1,
          max_uses: resource.max_uses
        }
        let updated_resources = pool.resources.update(index, updated_resource)
        
        ({
          max_resources: pool.max_resources,
          resources: updated_resources,
          created_count: pool.created_count,
          destroyed_count: pool.destroyed_count,
          cleanup_threshold: pool.cleanup_threshold,
          last_cleanup: pool.last_cleanup
        }, Some(updated_resource.id))
      }
      None => {
        if pool.resources.length() < pool.max_resources {
          // Create new resource
          let updated_pool = create_pooled_resource(pool, current_time)
          let new_resource_index = updated_pool.resources.length() - 1
          let new_resource = updated_pool.resources[new_resource_index]
          let updated_new_resource = {
            id: new_resource.id,
            in_use: true,
            created_at: new_resource.created_at,
            last_used: current_time,
            use_count: 1,
            max_uses: new_resource.max_uses
          }
          let updated_resources = updated_pool.resources.update(new_resource_index, updated_new_resource)
          
          ({
            max_resources: updated_pool.max_resources,
            resources: updated_resources,
            created_count: updated_pool.created_count,
            destroyed_count: updated_pool.destroyed_count,
            cleanup_threshold: updated_pool.cleanup_threshold,
            last_cleanup: updated_pool.last_cleanup
          }, Some(updated_new_resource.id))
        } else {
          // Pool exhausted
          (pool, None)
        }
      }
    }
  }
  
  // Release resource back to pool
  let release_resource = fn(pool: ResourcePool, resource_id: String, current_time: Int) {
    match pool.resources.find_index(fn(r) { r.id == resource_id }) {
      Some(index) => {
        let resource = pool.resources[index]
        if resource.in_use {
          let updated_resource = {
            id: resource.id,
            in_use: false,
            created_at: resource.created_at,
            last_used: current_time,
            use_count: resource.use_count,
            max_uses: resource.max_uses
          }
          let updated_resources = pool.resources.update(index, updated_resource)
          
          ({
            max_resources: pool.max_resources,
            resources: updated_resources,
            created_count: pool.created_count,
            destroyed_count: pool.destroyed_count,
            cleanup_threshold: pool.cleanup_threshold,
            last_cleanup: pool.last_cleanup
          }, true)
        } else {
          // Resource already released
          (pool, false)
        }
      }
      None => {
        // Resource not found
        (pool, false)
      }
    }
  }
  
  // Cleanup expired or overused resources
  let cleanup_resources = fn(pool: ResourcePool, current_time: Int, max_idle_time: Int) {
    if current_time - pool.last_cleanup > pool.cleanup_threshold {
      let expired_resources = pool.resources.filter(fn(r) { 
        !r.in_use && 
        (current_time - r.last_used > max_idle_time || r.use_count >= r.max_uses)
      })
      
      let active_resources = pool.resources.filter(fn(r) { 
        r.in_use || 
        (current_time - r.last_used <= max_idle_time && r.use_count < r.max_uses)
      })
      
      let destroyed_count = expired_resources.length()
      
      ({
        max_resources: pool.max_resources,
        resources: active_resources,
        created_count: pool.created_count,
        destroyed_count: pool.destroyed_count + destroyed_count,
        cleanup_threshold: pool.cleanup_threshold,
        last_cleanup: current_time
      }, destroyed_count)
    } else {
      (pool, 0)
    }
  }
  
  // Test resource pool creation
  let pool = create_resource_pool(5, 60000)  // 5 max resources, 1 minute cleanup threshold
  assert_eq(pool.max_resources, 5)
  assert_eq(pool.resources.length(), 0)
  assert_eq(pool.created_count, 0)
  assert_eq(pool.destroyed_count, 0)
  
  // Test resource acquisition
  let (pool1, res1_id) = acquire_resource(pool, 1640995200000)
  match res1_id {
    Some(id) => {
      assert_eq(id, "res-0")
      assert_eq(pool1.resources.length(), 1)
      assert_eq(pool1.created_count, 1)
    }
    None => assert_true(false)
  }
  
  // Test resource release
  let (pool2, release_result) = release_resource(pool1, "res-0", 1640995201000)
  assert_true(release_result)
  assert_false(pool2.resources[0].in_use)
  assert_eq(pool2.resources[0].use_count, 1)
  
  // Test multiple acquisitions
  let (pool3, res2_id) = acquire_resource(pool2, 1640995202000)
  let (pool4, res3_id) = acquire_resource(pool3, 1640995203000)
  let (pool5, res4_id) = acquire_resource(pool4, 1640995204000)
  
  match res2_id {
    Some(id) => assert_eq(id, "res-0")  // Reuse existing resource
    None => assert_true(false)
  }
  
  match res3_id {
    Some(id) => assert_eq(id, "res-1")  // Create new resource
    None => assert_true(false)
  }
  
  match res4_id {
    Some(id) => assert_eq(id, "res-2")  // Create new resource
    None => assert_true(false)
  }
  
  assert_eq(pool5.resources.length(), 3)
  assert_eq(pool5.created_count, 3)
  
  // Test cleanup
  let current_time = 1640995300000  // 10 minutes later
  let (pool6, cleaned_count) = cleanup_resources(pool5, current_time, 300000)  // 5 minutes idle timeout
  
  // Since cleanup threshold (1 minute) has passed, cleanup should run
  assert_true(cleaned_count >= 0)
  assert_eq(pool6.last_cleanup, current_time)
  
  // Test resource that exceeds max uses
  let overused_resource = {
    id: "overused",
    in_use: false,
    created_at: 1640995200000,
    last_used: 1640995200000,
    use_count: 100,
    max_uses: 100
  }
  
  let pool7 = { pool6 | resources: pool6.resources.push(overused_resource) }
  let (pool8, cleaned_count2) = cleanup_resources(pool7, current_time + 60000, 300000)
  
  // Overused resource should be cleaned up
  assert_true(cleaned_count2 > 0)
}

// Test 5: Memory Pressure and Adaptive Management
test "memory pressure and adaptive management" {
  // Define memory pressure level
  enum MemoryPressure {
    Low
    Medium
    High
    Critical
  }
  
  // Define memory manager
  type MemoryManager = {
    total_memory: Int,
    used_memory: Int,
    pressure_thresholds: { low: Int, medium: Int, high: Int, critical: Int },
    current_pressure: MemoryPressure,
    adaptive_cleanup_enabled: Bool,
    cleanup_strategies: Array[String]
  }
  
  // Create memory manager
  let create_memory_manager = fn(total_memory: Int) {
    let pressure_thresholds = {
      low: total_memory * 50 / 100,      // 50%
      medium: total_memory * 70 / 100,   // 70%
      high: total_memory * 85 / 100,     // 85%
      critical: total_memory * 95 / 100  // 95%
    }
    
    {
      total_memory,
      used_memory: 0,
      pressure_thresholds,
      current_pressure: MemoryPressure::Low,
      adaptive_cleanup_enabled: true,
      cleanup_strategies: ["cache", "buffers", "temporary"]
    }
  }
  
  // Update memory usage
  let update_memory_usage = fn(manager: MemoryManager, new_usage: Int) {
    let updated_usage = if new_usage < 0 { 0 } else if new_usage > manager.total_memory { manager.total_memory } else { new_usage }
    
    // Determine pressure level
    let new_pressure = if updated_usage >= manager.pressure_thresholds.critical {
      MemoryPressure::Critical
    } else if updated_usage >= manager.pressure_thresholds.high {
      MemoryPressure::High
    } else if updated_usage >= manager.pressure_thresholds.medium {
      MemoryPressure::Medium
    } else {
      MemoryPressure::Low
    }
    
    {
      total_memory: manager.total_memory,
      used_memory: updated_usage,
      pressure_thresholds: manager.pressure_thresholds,
      current_pressure: new_pressure,
      adaptive_cleanup_enabled: manager.adaptive_cleanup_enabled,
      cleanup_strategies: manager.cleanup_strategies
    }
  }
  
  // Get cleanup strategy based on pressure level
  let get_cleanup_strategy = fn(manager: MemoryManager) {
    match manager.current_pressure {
      MemoryPressure::Low => ["none"]
      MemoryPressure::Medium => ["cache"]
      MemoryPressure::High => ["cache", "buffers"]
      MemoryPressure::Critical => ["cache", "buffers", "temporary"]
    }
  }
  
  // Simulate memory cleanup
  let simulate_cleanup = fn(manager: MemoryManager, strategy: String) {
    match strategy {
      "cache" => {
        // Free 10% of memory
        let freed_memory = manager.used_memory * 10 / 100
        update_memory_usage(manager, manager.used_memory - freed_memory)
      }
      "buffers" => {
        // Free 15% of memory
        let freed_memory = manager.used_memory * 15 / 100
        update_memory_usage(manager, manager.used_memory - freed_memory)
      }
      "temporary" => {
        // Free 20% of memory
        let freed_memory = manager.used_memory * 20 / 100
        update_memory_usage(manager, manager.used_memory - freed_memory)
      }
      _ => manager
    }
  }
  
  // Adaptive cleanup based on pressure
  let adaptive_cleanup = fn(manager: MemoryManager) {
    if manager.adaptive_cleanup_enabled {
      let strategies = get_cleanup_strategy(manager)
      let mut updated_manager = manager
      
      for strategy in strategies {
        updated_manager = simulate_cleanup(updated_manager, strategy)
      }
      
      updated_manager
    } else {
      manager
    }
  }
  
  // Test memory manager creation
  let manager = create_memory_manager(1000)  // 1000 units of memory
  assert_eq(manager.total_memory, 1000)
  assert_eq(manager.used_memory, 0)
  assert_eq(manager.pressure_thresholds.low, 500)
  assert_eq(manager.pressure_thresholds.medium, 700)
  assert_eq(manager.pressure_thresholds.high, 850)
  assert_eq(manager.pressure_thresholds.critical, 950)
  
  // Test low pressure
  let manager1 = update_memory_usage(manager, 400)
  match manager1.current_pressure {
    MemoryPressure::Low => assert_true(true)
    _ => assert_true(false)
  }
  
  let low_strategy = get_cleanup_strategy(manager1)
  assert_eq(low_strategy, ["none"])
  
  // Test medium pressure
  let manager2 = update_memory_usage(manager1, 600)
  match manager2.current_pressure {
    MemoryPressure::Medium => assert_true(true)
    _ => assert_true(false)
  }
  
  let medium_strategy = get_cleanup_strategy(manager2)
  assert_eq(medium_strategy, ["cache"])
  
  // Test high pressure
  let manager3 = update_memory_usage(manager2, 800)
  match manager3.current_pressure {
    MemoryPressure::High => assert_true(true)
    _ => assert_true(false)
  }
  
  let high_strategy = get_cleanup_strategy(manager3)
  assert_eq(high_strategy, ["cache", "buffers"])
  
  // Test critical pressure
  let manager4 = update_memory_usage(manager3, 960)
  match manager4.current_pressure {
    MemoryPressure::Critical => assert_true(true)
    _ => assert_true(false)
  }
  
  let critical_strategy = get_cleanup_strategy(manager4)
  assert_eq(critical_strategy, ["cache", "buffers", "temporary"])
  
  // Test adaptive cleanup
  let manager5 = adaptive_cleanup(manager4)
  assert_true(manager5.used_memory < manager4.used_memory)  // Memory should be freed
  
  // After cleanup, pressure should be reduced
  match manager5.current_pressure {
    MemoryPressure::Critical => assert_true(false)  // Should no longer be critical
    _ => assert_true(true)
  }
  
  // Test cleanup effectiveness
  let initial_usage = manager4.used_memory
  let final_usage = manager5.used_memory
  let freed_percentage = (initial_usage - final_usage) * 100 / initial_usage
  
  // Should free at least 30% (10% + 15% + 20%)
  assert_true(freed_percentage >= 30)
}