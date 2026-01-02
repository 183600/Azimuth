// Azimuth Resource Lifecycle Management Test Suite
// This file contains comprehensive test cases for resource lifecycle management

// Test 1: Basic Resource Management
test "basic resource management" {
  type Resource {
    id : Int
    name : String
    is_allocated : Bool
    allocation_time : Int
    reference_count : Int
  }
  
  type ResourceManager {
    resources : Array[Resource]
    next_id : Int
    current_time : Int
  }
  
  let create_resource_manager = fn() {
    { resources: [], next_id: 1, current_time: 0 }
  }
  
  let allocate_resource = fn(manager, name) {
    let resource = {
      id: manager.next_id,
      name: name,
      is_allocated: true,
      allocation_time: manager.current_time,
      reference_count: 1
    }
    
    {
      resources: manager.resources @ [resource],
      next_id: manager.next_id + 1,
      current_time: manager.current_time
    }
  }
  
  let release_resource = fn(manager, resource_id) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in manager.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_allocated: false,
          allocation_time: resource.allocation_time,
          reference_count: 0
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { resources: updated_resources, next_id: manager.next_id, current_time: manager.current_time }
    } else {
      manager  // Resource not found
    }
  }
  
  let acquire_reference = fn(manager, resource_id) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in manager.resources {
      if resource.id == resource_id and resource.is_allocated {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_allocated: resource.is_allocated,
          allocation_time: resource.allocation_time,
          reference_count: resource.reference_count + 1
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { resources: updated_resources, next_id: manager.next_id, current_time: manager.current_time }
    } else {
      manager  // Resource not found or not allocated
    }
  }
  
  let release_reference = fn(manager, resource_id) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in manager.resources {
      if resource.id == resource_id and resource.reference_count > 0 {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_allocated: resource.is_allocated,
          allocation_time: resource.allocation_time,
          reference_count: resource.reference_count - 1
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { resources: updated_resources, next_id: manager.next_id, current_time: manager.current_time }
    } else {
      manager  // Resource not found
    }
  }
  
  // Test resource allocation
  let manager = create_resource_manager()
  let manager1 = allocate_resource(manager, "test_resource")
  
  assert_eq(manager1.resources.length(), 1)
  assert_eq(manager1.resources[0].id, 1)
  assert_eq(manager1.resources[0].name, "test_resource")
  assert_true(manager1.resources[0].is_allocated)
  assert_eq(manager1.resources[0].reference_count, 1)
  
  // Test reference acquisition
  let manager2 = acquire_reference(manager1, 1)
  assert_eq(manager2.resources[0].reference_count, 2)
  
  // Test reference release
  let manager3 = release_reference(manager2, 1)
  assert_eq(manager3.resources[0].reference_count, 1)
  
  // Test resource release
  let manager4 = release_resource(manager3, 1)
  assert_false(manager4.resources[0].is_allocated)
  assert_eq(manager4.resources[0].reference_count, 0)
  
  // Test operations on non-existent resource
  let manager5 = release_reference(manager4, 999)
  assert_eq(manager5.resources.length(), 1)  // No change
}

// Test 2: Resource Pool Management
test "resource pool management" {
  type PooledResource {
    id : Int
    is_in_use : Bool
    last_used : Int
    use_count : Int
  }
  
  type ResourcePool {
    resources : Array[PooledResource]
    max_size : Int
    created_count : Int
  }
  
  let create_resource_pool = fn(max_size) {
    { resources: [], max_size: max_size, created_count: 0 }
  }
  
  let acquire_resource = fn(pool) {
    // Find an available resource
    for i in 0..pool.resources.length() {
      if not pool.resources[i].is_in_use {
        let updated_resource = {
          id: pool.resources[i].id,
          is_in_use: true,
          last_used: 1000,  // Simulated timestamp
          use_count: pool.resources[i].use_count + 1
        }
        
        let mut updated_resources = pool.resources
        updated_resources[i] = updated_resource
        
        return (Some(updated_resource.id), {
          resources: updated_resources,
          max_size: pool.max_size,
          created_count: pool.created_count
        })
      }
    }
    
    // No available resource, create a new one if pool not full
    if pool.resources.length() < pool.max_size {
      let new_resource = {
        id: pool.created_count + 1,
        is_in_use: true,
        last_used: 1000,  // Simulated timestamp
        use_count: 1
      }
      
      (Some(new_resource.id), {
        resources: pool.resources @ [new_resource],
        max_size: pool.max_size,
        created_count: pool.created_count + 1
      })
    } else {
      (None, pool)  // Pool is full
    }
  }
  
  let release_resource = fn(pool, resource_id) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in pool.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          is_in_use: false,
          last_used: 1000,  // Simulated timestamp
          use_count: resource.use_count
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { resources: updated_resources, max_size: pool.max_size, created_count: pool.created_count }
    } else {
      pool  // Resource not found
    }
  }
  
  let get_pool_stats = fn(pool) {
    let mut in_use = 0
    let mut available = 0
    
    for resource in pool.resources {
      if resource.is_in_use {
        in_use = in_use + 1
      } else {
        available = available + 1
      }
    }
    
    { in_use: in_use, available: available, created: pool.created_count }
  }
  
  // Test resource pool
  let pool = create_resource_pool(3)
  
  // Acquire resources
  let (resource1_id, pool1) = acquire_resource(pool)
  match resource1_id {
    Some(id) => assert_eq(id, 1)
    None => assert_true(false)
  }
  
  let (resource2_id, pool2) = acquire_resource(pool1)
  match resource2_id {
    Some(id) => assert_eq(id, 2)
    None => assert_true(false)
  }
  
  let (resource3_id, pool3) = acquire_resource(pool2)
  match resource3_id {
    Some(id) => assert_eq(id, 3)
    None => assert_true(false)
  }
  
  // Pool should be full now
  let (resource4_id, _) = acquire_resource(pool3)
  match resource4_id {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Release a resource
  let pool4 = release_resource(pool3, 2)
  
  // Should be able to acquire again
  let (resource5_id, pool5) = acquire_resource(pool4)
  match resource5_id {
    Some(id) => assert_eq(id, 2)  // Should reuse the released resource
    None => assert_true(false)
  }
  
  // Test pool stats
  let stats = get_pool_stats(pool5)
  assert_eq(stats.in_use, 3)
  assert_eq(stats.available, 0)
  assert_eq(stats.created, 3)
  
  // Release all resources
  let pool6 = release_resource(release_resource(release_resource(pool5, 1), 2), 3)
  let stats2 = get_pool_stats(pool6)
  assert_eq(stats2.in_use, 0)
  assert_eq(stats2.available, 3)
}

// Test 3: Resource Cleanup and Garbage Collection
test "resource cleanup and garbage collection" {
  type ManagedResource {
    id : Int
    size : Int
    last_accessed : Int
    cleanup_required : Bool
  }
  
  type GarbageCollector {
    resources : Array[ManagedResource]
    memory_limit : Int
    current_memory : Int
    cleanup_threshold : Float  // Percentage of memory limit to trigger cleanup
  }
  
  let create_garbage_collector = fn(memory_limit, cleanup_threshold) {
    { 
      resources: [], 
      memory_limit: memory_limit, 
      current_memory: 0, 
      cleanup_threshold: cleanup_threshold 
    }
  }
  
  let allocate_resource = fn(gc, size) {
    if gc.current_memory + size <= gc.memory_limit {
      let resource = {
        id: gc.resources.length() + 1,
        size: size,
        last_accessed: 1000,  // Simulated timestamp
        cleanup_required: false
      }
      
      {
        resources: gc.resources @ [resource],
        memory_limit: gc.memory_limit,
        current_memory: gc.current_memory + size,
        cleanup_threshold: gc.cleanup_threshold
      }
    } else {
      gc  // Allocation failed
    }
  }
  
  let access_resource = fn(gc, resource_id) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in gc.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          size: resource.size,
          last_accessed: 1000,  // Simulated timestamp
          cleanup_required: resource.cleanup_required
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { 
        resources: updated_resources, 
        memory_limit: gc.memory_limit, 
        current_memory: gc.current_memory, 
        cleanup_threshold: gc.cleanup_threshold 
      }
    } else {
      gc  // Resource not found
    }
  }
  
  let mark_for_cleanup = fn(gc, resource_id) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in gc.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          size: resource.size,
          last_accessed: resource.last_accessed,
          cleanup_required: true
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { 
        resources: updated_resources, 
        memory_limit: gc.memory_limit, 
        current_memory: gc.current_memory, 
        cleanup_threshold: gc.cleanup_threshold 
      }
    } else {
      gc  // Resource not found
    }
  }
  
  let run_garbage_collection = fn(gc) {
    let memory_usage_ratio = gc.current_memory.to_float() / gc.memory_limit.to_float()
    
    if memory_usage_ratio > gc.cleanup_threshold {
      // Find resources marked for cleanup
      let mut resources_to_keep = []
      let mut freed_memory = 0
      
      for resource in gc.resources {
        if resource.cleanup_required {
          freed_memory = freed_memory + resource.size
        } else {
          resources_to_keep = resources_to_keep @ [resource]
        }
      }
      
      { 
        resources: resources_to_keep, 
        memory_limit: gc.memory_limit, 
        current_memory: gc.current_memory - freed_memory, 
        cleanup_threshold: gc.cleanup_threshold 
      }
    } else {
      gc  // No cleanup needed
    }
  }
  
  let run_lru_cleanup = fn(gc, target_memory) {
    if gc.current_memory > target_memory {
      // Sort resources by last accessed time (LRU first)
      let mut sorted_resources = gc.resources
      // Simple bubble sort for demonstration
      for i in 0..sorted_resources.length() {
        for j in i + 1..sorted_resources.length() {
          if sorted_resources[i].last_accessed > sorted_resources[j].last_accessed {
            let temp = sorted_resources[i]
            sorted_resources[i] = sorted_resources[j]
            sorted_resources[j] = temp
          }
        }
      }
      
      let mut resources_to_keep = []
      let mut freed_memory = 0
      let mut current_memory = gc.current_memory
      
      for resource in sorted_resources {
        if current_memory - resource.size >= target_memory {
          freed_memory = freed_memory + resource.size
          current_memory = current_memory - resource.size
        } else {
          resources_to_keep = resources_to_keep @ [resource]
        }
      }
      
      { 
        resources: resources_to_keep, 
        memory_limit: gc.memory_limit, 
        current_memory: current_memory, 
        cleanup_threshold: gc.cleanup_threshold 
      }
    } else {
      gc  // No cleanup needed
    }
  }
  
  // Test garbage collection
  let gc = create_garbage_collector(1000, 0.8)  // 1000 bytes limit, 80% threshold
  
  // Allocate resources
  let gc1 = allocate_resource(gc, 300)
  let gc2 = allocate_resource(gc1, 200)
  let gc3 = allocate_resource(gc2, 400)
  
  assert_eq(gc3.current_memory, 900)
  assert_eq(gc3.resources.length(), 3)
  
  // Mark a resource for cleanup
  let gc4 = mark_for_cleanup(gc3, 2)
  assert_true(gc4.resources[1].cleanup_required)
  
  // Run garbage collection (shouldn't trigger yet, 900/1000 = 90% > 80%)
  let gc5 = run_garbage_collection(gc4)
  assert_eq(gc5.current_memory, 700)  // 300 + 400 = 700 (200 freed)
  assert_eq(gc5.resources.length(), 2)
  
  // Allocate more resources to trigger LRU cleanup
  let gc6 = allocate_resource(gc5, 200)
  let gc7 = allocate_resource(gc6, 200)
  
  assert_eq(gc7.current_memory, 1100)  // Exceeds limit
  
  // Run LRU cleanup to free up to 800 bytes
  let gc8 = run_lru_cleanup(gc7, 800)
  assert_eq(gc8.current_memory, 700)  // Should be at or below target
  assert_true(gc8.resources.length() < gc7.resources.length())
}

// Test 4: Resource Dependencies
test "resource dependencies" {
  type ResourceDependency {
    resource_id : Int
    depends_on : Int
  }
  
  type ResourceWithDeps {
    id : Int
    name : String
    is_active : Bool
    dependencies : Array[Int]
  }
  
  type DependencyManager {
    resources : Array[ResourceWithDeps]
    dependency_graph : Array[ResourceDependency]
  }
  
  let create_dependency_manager = fn() {
    { resources: [], dependency_graph: [] }
  }
  
  let add_resource = fn(manager, id, name) {
    let resource = {
      id: id,
      name: name,
      is_active: false,
      dependencies: []
    }
    
    { 
      resources: manager.resources @ [resource], 
      dependency_graph: manager.dependency_graph 
    }
  }
  
  let add_dependency = fn(manager, resource_id, depends_on) {
    let dependency = { resource_id: resource_id, depends_on: depends_on }
    
    // Update resource dependencies
    let mut updated_resources = []
    let mut found = false
    
    for resource in manager.resources {
      if resource.id == resource_id {
        found = true
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_active: resource.is_active,
          dependencies: resource.dependencies @ [depends_on]
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    if found {
      { 
        resources: updated_resources, 
        dependency_graph: manager.dependency_graph @ [dependency] 
      }
    } else {
      manager  // Resource not found
    }
  }
  
  let find_dependencies = fn(manager, resource_id) {
    let mut dependencies = []
    
    for dependency in manager.dependency_graph {
      if dependency.resource_id == resource_id {
        dependencies = dependencies @ [dependency.depends_on]
      }
    }
    
    dependencies
  }
  
  let find_dependents = fn(manager, resource_id) {
    let mut dependents = []
    
    for dependency in manager.dependency_graph {
      if dependency.depends_on == resource_id {
        dependents = dependents @ [dependency.resource_id]
      }
    }
    
    dependents
  }
  
  let can_activate = fn(manager, resource_id) {
    let dependencies = find_dependencies(manager, resource_id)
    
    for dep_id in dependencies {
      let mut found = false
      let mut is_active = false
      
      for resource in manager.resources {
        if resource.id == dep_id {
          found = true
          is_active = resource.is_active
          break
        }
      }
      
      if not found or not is_active {
        return false
      }
    }
    
    true
  }
  
  let activate_resource = fn(manager, resource_id) {
    if can_activate(manager, resource_id) {
      let mut updated_resources = []
      
      for resource in manager.resources {
        if resource.id == resource_id {
          updated_resources = updated_resources @ [{
            id: resource.id,
            name: resource.name,
            is_active: true,
            dependencies: resource.dependencies
          }]
        } else {
          updated_resources = updated_resources @ [resource]
        }
      }
      
      { 
        resources: updated_resources, 
        dependency_graph: manager.dependency_graph 
      }
    } else {
      manager  // Cannot activate
    }
  }
  
  let deactivate_resource = fn(manager, resource_id) {
    // Check if any active resources depend on this one
    let dependents = find_dependents(manager, resource_id)
    
    for dependent_id in dependents {
      for resource in manager.resources {
        if resource.id == dependent_id and resource.is_active {
          return manager  // Cannot deactivate, has active dependents
        }
      }
    }
    
    // Safe to deactivate
    let mut updated_resources = []
    
    for resource in manager.resources {
      if resource.id == resource_id {
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_active: false,
          dependencies: resource.dependencies
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    { 
      resources: updated_resources, 
      dependency_graph: manager.dependency_graph 
    }
  }
  
  // Test dependency management
  let manager = create_dependency_manager()
  
  // Add resources
  let manager1 = add_resource(add_resource(add_resource(manager, 1, "Database"), 2, "Cache"), 3, "API")
  
  // Add dependencies (API depends on Cache, Cache depends on Database)
  let manager2 = add_dependency(add_dependency(manager1, 2, 1), 3, 2)
  
  // Test dependency lookup
  let api_deps = find_dependencies(manager2, 3)
  assert_eq(api_deps.length(), 1)
  assert_eq(api_deps[0], 2)
  
  let cache_deps = find_dependencies(manager2, 2)
  assert_eq(cache_deps.length(), 1)
  assert_eq(cache_deps[0], 1)
  
  let db_dependents = find_dependents(manager2, 1)
  assert_eq(db_dependents.length(), 1)
  assert_eq(db_dependents[0], 2)
  
  // Test activation order
  // Cannot activate API first (dependencies not active)
  let manager3 = activate_resource(manager2, 3)
  
  let mut api_active = false
  for resource in manager3.resources {
    if resource.id == 3 and resource.is_active {
      api_active = true
      break
    }
  }
  assert_false(api_active)
  
  // Activate Database
  let manager4 = activate_resource(manager3, 1)
  
  let mut db_active = false
  for resource in manager4.resources {
    if resource.id == 1 and resource.is_active {
      db_active = true
      break
    }
  }
  assert_true(db_active)
  
  // Activate Cache (Database is active)
  let manager5 = activate_resource(manager4, 2)
  
  let mut cache_active = false
  for resource in manager5.resources {
    if resource.id == 2 and resource.is_active {
      cache_active = true
      break
    }
  }
  assert_true(cache_active)
  
  // Activate API (Cache is active)
  let manager6 = activate_resource(manager5, 3)
  
  let mut api_active2 = false
  for resource in manager6.resources {
    if resource.id == 3 and resource.is_active {
      api_active2 = true
      break
    }
  }
  assert_true(api_active2)
  
  // Test deactivation order
  // Cannot deactivate Database (API and Cache depend on it)
  let manager7 = deactivate_resource(manager6, 1)
  
  let mut db_still_active = false
  for resource in manager7.resources {
    if resource.id == 1 and resource.is_active {
      db_still_active = true
      break
    }
  }
  assert_true(db_still_active)
  
  // Deactivate API
  let manager8 = deactivate_resource(manager7, 3)
  
  // Deactivate Cache
  let manager9 = deactivate_resource(manager8, 2)
  
  // Now can deactivate Database
  let manager10 = deactivate_resource(manager9, 1)
  
  let mut db_deactivated = false
  for resource in manager10.resources {
    if resource.id == 1 and not resource.is_active {
      db_deactivated = true
      break
    }
  }
  assert_true(db_deactivated)
}

// Test 5: Resource Leaks Detection
test "resource leaks detection" {
  type ResourceLeak {
    resource_id : Int
    resource_type : String
    allocation_time : Int
    leak_detected_time : Int
    size : Int
  }
  
  type LeakDetector {
    allocated_resources : Array[Int]
    resource_types : Array[Int -> String]  // resource_id -> type
    allocation_times : Array[Int -> Int]   // resource_id -> time
    resource_sizes : Array[Int -> Int]     // resource_id -> size
    detected_leaks : Array[ResourceLeak]
    current_time : Int
    leak_threshold : Int  // Time in ms before considering a resource leaked
  }
  
  let create_leak_detector = fn(leak_threshold) {
    { 
      allocated_resources: [], 
      resource_types: [], 
      allocation_times: [], 
      resource_sizes: [], 
      detected_leaks: [], 
      current_time: 0, 
      leak_threshold: leak_threshold 
    }
  }
  
  let allocate_resource = fn(detector, resource_id, resource_type, size) {
    let get_type = fn(id) {
      if id == resource_id { resource_type } else { "unknown" }
    }
    
    let get_time = fn(id) {
      if id == resource_id { detector.current_time } else { 0 }
    }
    
    let get_size = fn(id) {
      if id == resource_id { size } else { 0 }
    }
    
    { 
      allocated_resources: detector.allocated_resources @ [resource_id], 
      resource_types: detector.resource_types @ [get_type], 
      allocation_times: detector.allocation_times @ [get_time], 
      resource_sizes: detector.resource_sizes @ [get_size], 
      detected_leaks: detector.detected_leaks, 
      current_time: detector.current_time, 
      leak_threshold: detector.leak_threshold 
    }
  }
  
  let deallocate_resource = fn(detector, resource_id) {
    let mut updated_resources = []
    let mut updated_types = []
    let mut updated_times = []
    let mut updated_sizes = []
    
    for i in 0..detector.allocated_resources.length() {
      if detector.allocated_resources[i] != resource_id {
        updated_resources = updated_resources @ [detector.allocated_resources[i]]
        updated_types = updated_types @ [detector.resource_types[i]]
        updated_times = updated_times @ [detector.allocation_times[i]]
        updated_sizes = updated_sizes @ [detector.resource_sizes[i]]
      }
    }
    
    { 
      allocated_resources: updated_resources, 
      resource_types: updated_types, 
      allocation_times: updated_times, 
      resource_sizes: updated_sizes, 
      detected_leaks: detector.detected_leaks, 
      current_time: detector.current_time, 
      leak_threshold: detector.leak_threshold 
    }
  }
  
  let advance_time = fn(detector, time_delta) {
    { 
      allocated_resources: detector.allocated_resources, 
      resource_types: detector.resource_types, 
      allocation_times: detector.allocation_times, 
      resource_sizes: detector.resource_sizes, 
      detected_leaks: detector.detected_leaks, 
      current_time: detector.current_time + time_delta, 
      leak_threshold: detector.leak_threshold 
    }
  }
  
  let detect_leaks = fn(detector) {
    let mut new_leaks = []
    
    for i in 0..detector.allocated_resources.length() {
      let resource_id = detector.allocated_resources[i]
      let resource_type = detector.resource_types[i](resource_id)
      let allocation_time = detector.allocation_times[i](resource_id)
      let resource_size = detector.resource_sizes[i](resource_id)
      
      let age = detector.current_time - allocation_time
      
      if age > detector.leak_threshold {
        // Check if this leak was already detected
        let mut already_detected = false
        
        for leak in detector.detected_leaks {
          if leak.resource_id == resource_id {
            already_detected = true
            break
          }
        }
        
        if not already_detected {
          new_leaks = new_leaks @ [{
            resource_id: resource_id,
            resource_type: resource_type,
            allocation_time: allocation_time,
            leak_detected_time: detector.current_time,
            size: resource_size
          }]
        }
      }
    }
    
    { 
      allocated_resources: detector.allocated_resources, 
      resource_types: detector.resource_types, 
      allocation_times: detector.allocation_times, 
      resource_sizes: detector.resource_sizes, 
      detected_leaks: detector.detected_leaks @ new_leaks, 
      current_time: detector.current_time, 
      leak_threshold: detector.leak_threshold 
    }
  }
  
  let get_leak_report = fn(detector) {
    let mut total_leaked_resources = 0
    let mut total_leaked_memory = 0
    let mut leaks_by_type = []
    
    for leak in detector.detected_leaks {
      total_leaked_resources = total_leaked_resources + 1
      total_leaked_memory = total_leaked_memory + leak.size
      
      let mut found_type = false
      for i in 0..leaks_by_type.length() {
        if leaks_by_type[i].0 == leak.resource_type {
          let (type_name, count, size) = leaks_by_type[i]
          leaks_by_type[i] = (type_name, count + 1, size + leak.size)
          found_type = true
          break
        }
      }
      
      if not found_type {
        leaks_by_type = leaks_by_type @ [(leak.resource_type, 1, leak.size)]
      }
    }
    
    {
      total_leaks: total_leaked_resources,
      total_memory: total_leaked_memory,
      leaks_by_type: leaks_by_type
    }
  }
  
  // Test leak detection
  let detector = create_leak_detector(1000)  // 1000ms threshold
  
  // Allocate some resources
  let detector1 = allocate_resource(detector, 1, "DatabaseConnection", 100)
  let detector2 = allocate_resource(detector1, 2, "FileHandle", 50)
  let detector3 = allocate_resource(detector2, 3, "MemoryBuffer", 200)
  
  assert_eq(detector3.allocated_resources.length(), 3)
  
  // Advance time but not enough to trigger leak detection
  let detector4 = advance_time(detector3, 500)
  let detector5 = detect_leaks(detector4)
  
  assert_eq(detector5.detected_leaks.length(), 0)
  
  // Deallocate one resource
  let detector6 = deallocate_resource(detector5, 2)
  assert_eq(detector6.allocated_resources.length(), 2)
  
  // Advance time past threshold
  let detector7 = advance_time(detector6, 600)
  let detector8 = detect_leaks(detector7)
  
  // Should detect 2 leaks (resources 1 and 3)
  assert_eq(detector8.detected_leaks.length(), 2)
  
  // Verify leak details
  let leak_report = get_leak_report(detector8)
  assert_eq(leak_report.total_leaks, 2)
  assert_eq(leak_report.total_memory, 300)  // 100 + 200
  assert_eq(leak_report.leaks_by_type.length(), 2)
  
  // Deallocate a leaked resource
  let detector9 = deallocate_resource(detector8, 1)
  let detector10 = detect_leaks(detector9)
  
  // Should still have 1 leak (resource 3)
  assert_eq(detector10.detected_leaks.length(), 2)  // Still shows historical leaks
}

// Test 6: Resource Health Monitoring
test "resource health monitoring" {
  type ResourceHealth {
    resource_id : Int
    status : String  // "healthy", "degraded", "unhealthy"
    last_check : Int
    metrics : Array[(String, Float)]
  }
  
  type HealthMonitor {
    resources : Array[ResourceHealth]
    check_interval : Int
    current_time : Int
    thresholds : Array[(String, Float)]  // (metric_name, threshold)
  }
  
  let create_health_monitor = fn(check_interval) {
    { 
      resources: [], 
      check_interval: check_interval, 
      current_time: 0, 
      thresholds: [] 
    }
  }
  
  let add_resource = fn(monitor, resource_id) {
    let health = {
      resource_id: resource_id,
      status: "healthy",
      last_check: 0,
      metrics: []
    }
    
    { 
      resources: monitor.resources @ [health], 
      check_interval: monitor.check_interval, 
      current_time: monitor.current_time, 
      thresholds: monitor.thresholds 
    }
  }
  
  let set_threshold = fn(monitor, metric_name, threshold) {
    { 
      resources: monitor.resources, 
      check_interval: monitor.check_interval, 
      current_time: monitor.current_time, 
      thresholds: monitor.thresholds @ [(metric_name, threshold)] 
    }
  }
  
  let update_metrics = fn(monitor, resource_id, metrics) {
    let mut updated_resources = []
    
    for resource in monitor.resources {
      if resource.resource_id == resource_id {
        updated_resources = updated_resources @ [{
          resource_id: resource_id,
          status: resource.status,
          last_check: monitor.current_time,
          metrics: metrics
        }]
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    { 
      resources: updated_resources, 
      check_interval: monitor.check_interval, 
      current_time: monitor.current_time, 
      thresholds: monitor.thresholds 
    }
  }
  
  let evaluate_health = fn(monitor, resource_id) {
    let mut resource_health = None
    
    for resource in monitor.resources {
      if resource.resource_id == resource_id {
        resource_health = Some(resource)
        break
      }
    }
    
    match resource_health {
      Some(health) => {
        let mut status = "healthy"
        
        for (metric_name, value) in health.metrics {
          for (threshold_name, threshold) in monitor.thresholds {
            if metric_name == threshold_name {
              if value > threshold {
                status = "unhealthy"
                break
              } else if value > threshold * 0.8 {
                status = "degraded"
              }
            }
          }
          
          if status == "unhealthy" {
            break
          }
        }
        
        let mut updated_resources = []
        
        for resource in monitor.resources {
          if resource.resource_id == resource_id {
            updated_resources = updated_resources @ [{
              resource_id: resource_id,
              status: status,
              last_check: resource.last_check,
              metrics: resource.metrics
            }]
          } else {
            updated_resources = updated_resources @ [resource]
          }
        }
        
        { 
          resources: updated_resources, 
          check_interval: monitor.check_interval, 
          current_time: monitor.current_time, 
          thresholds: monitor.thresholds 
        }
      }
      None => monitor
    }
  }
  
  let get_resource_status = fn(monitor, resource_id) {
    for resource in monitor.resources {
      if resource.resource_id == resource_id {
        return resource.status
      }
    }
    "unknown"
  }
  
  let get_health_summary = fn(monitor) {
    let mut healthy = 0
    let mut degraded = 0
    let mut unhealthy = 0
    
    for resource in monitor.resources {
      match resource.status {
        "healthy" => healthy = healthy + 1
        "degraded" => degraded = degraded + 1
        "unhealthy" => unhealthy = unhealthy + 1
        _ => ()
      }
    }
    
    { healthy: healthy, degraded: degraded, unhealthy: unhealthy }
  }
  
  // Test health monitoring
  let monitor = create_health_monitor(100)  // Check every 100ms
  
  // Add resources
  let monitor1 = add_resource(add_resource(monitor, 1), 2)
  
  // Set thresholds
  let monitor2 = set_threshold(set_threshold(monitor1, "cpu_usage", 80.0), "memory_usage", 90.0)
  
  // Update resource 1 with healthy metrics
  let metrics1 = [("cpu_usage", 50.0), ("memory_usage", 60.0)]
  let monitor3 = update_metrics(monitor2, 1, metrics1)
  
  // Evaluate health for resource 1
  let monitor4 = evaluate_health(monitor3, 1)
  assert_eq(get_resource_status(monitor4, 1), "healthy")
  
  // Update resource 2 with degraded metrics
  let metrics2 = [("cpu_usage", 75.0), ("memory_usage", 85.0)]
  let monitor5 = update_metrics(monitor4, 2, metrics2)
  
  // Evaluate health for resource 2
  let monitor6 = evaluate_health(monitor5, 2)
  assert_eq(get_resource_status(monitor6, 2), "degraded")
  
  // Update resource 1 with unhealthy metrics
  let metrics3 = [("cpu_usage", 85.0), ("memory_usage", 95.0)]
  let monitor7 = update_metrics(monitor6, 1, metrics3)
  
  // Evaluate health for resource 1
  let monitor8 = evaluate_health(monitor7, 1)
  assert_eq(get_resource_status(monitor8, 1), "unhealthy")
  
  // Get health summary
  let summary = get_health_summary(monitor8)
  assert_eq(summary.healthy, 0)
  assert_eq(summary.degraded, 1)
  assert_eq(summary.unhealthy, 1)
}

// Test 7: Resource Quotas and Limits
test "resource quotas and limits" {
  type ResourceQuota {
    resource_type : String
    limit : Int
    current_usage : Int
  }
  
  type QuotaManager {
    quotas : Array[ResourceQuota]
    allocations : Array[(String, Int)]  // (resource_type, amount)
  }
  
  let create_quota_manager = fn() {
    { quotas: [], allocations: [] }
  }
  
  let set_quota = fn(manager, resource_type, limit) {
    let mut updated_quotas = []
    let mut found = false
    
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        updated_quotas = updated_quotas @ [{
          resource_type: resource_type,
          limit: limit,
          current_usage: quota.current_usage
        }]
        found = true
      } else {
        updated_quotas = updated_quotas @ [quota]
      }
    }
    
    if not found {
      updated_quotas = updated_quotas @ [{
        resource_type: resource_type,
        limit: limit,
        current_usage: 0
      }]
    }
    
    { quotas: updated_quotas, allocations: manager.allocations }
  }
  
  let request_allocation = fn(manager, resource_type, amount) {
    // Find quota for this resource type
    let mut quota_found = false
    let mut quota_limit = 0
    let mut current_usage = 0
    
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        quota_found = true
        quota_limit = quota.limit
        current_usage = quota.current_usage
        break
      }
    }
    
    if quota_found and current_usage + amount <= quota_limit {
      // Allocation approved
      let mut updated_quotas = []
      
      for quota in manager.quotas {
        if quota.resource_type == resource_type {
          updated_quotas = updated_quotas @ [{
            resource_type: resource_type,
            limit: quota.limit,
            current_usage: current_usage + amount
          }]
        } else {
          updated_quotas = updated_quotas @ [quota]
        }
      }
      
      (true, {
        quotas: updated_quotas,
        allocations: manager.allocations @ [(resource_type, amount)]
      })
    } else {
      // Allocation denied
      (false, manager)
    }
  }
  
  let release_allocation = fn(manager, resource_type, amount) {
    let mut updated_quotas = []
    let mut found = false
    
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        found = true
        updated_quotas = updated_quotas @ [{
          resource_type: resource_type,
          limit: quota.limit,
          current_usage: (quota.current_usage - amount).max(0)
        }]
      } else {
        updated_quotas = updated_quotas @ [quota]
      }
    }
    
    if found {
      { quotas: updated_quotas, allocations: manager.allocations }
    } else {
      manager  // Quota not found
    }
  }
  
  let get_quota_usage = fn(manager, resource_type) {
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        return (quota.current_usage, quota.limit)
      }
    }
    (0, 0)
  }
  
  let get_quota_status = fn(manager) {
    let mut status = []
    
    for quota in manager.quotas {
      let usage_percentage = if quota.limit > 0 {
        (quota.current_usage.to_float() / quota.limit.to_float()) * 100.0
      } else {
        0.0
      }
      
      status = status @ [{
        resource_type: quota.resource_type,
        usage: quota.current_usage,
        limit: quota.limit,
        usage_percentage: usage_percentage
      }]
    }
    
    status
  }
  
  // Test quota management
  let manager = create_quota_manager()
  
  // Set quotas
  let manager1 = set_quota(set_quota(set_quota(manager, "cpu", 100), "memory", 1000), "storage", 10000)
  
  // Test allocations
  let (approved1, manager2) = request_allocation(manager1, "cpu", 30)
  assert_true(approved1)
  
  let (approved2, manager3) = request_allocation(manager2, "memory", 500)
  assert_true(approved2)
  
  let (approved3, manager4) = request_allocation(manager3, "cpu", 80)
  assert_false(approved3)  // Would exceed quota (30 + 80 > 100)
  
  // Check quota usage
  let (cpu_usage, cpu_limit) = get_quota_usage(manager4, "cpu")
  assert_eq(cpu_usage, 30)
  assert_eq(cpu_limit, 100)
  
  // Release some allocation
  let manager5 = release_allocation(manager4, "cpu", 10)
  
  let (cpu_usage2, cpu_limit2) = get_quota_usage(manager5, "cpu")
  assert_eq(cpu_usage2, 20)
  
  // Now the previous allocation should be approved
  let (approved4, manager6) = request_allocation(manager5, "cpu", 80)
  assert_true(approved4)
  
  // Get quota status
  let status = get_quota_status(manager6)
  assert_eq(status.length(), 3)
  
  // Find CPU status
  let mut cpu_status = None
  for s in status {
    if s.resource_type == "cpu" {
      cpu_status = Some(s)
      break
    }
  }
  
  match cpu_status {
    Some(status) => {
      assert_eq(status.usage, 100)  // 20 + 80
      assert_eq(status.limit, 100)
      assert_eq(status.usage_percentage, 100.0)
    }
    None => assert_true(false)
  }
}

// Test 8: Resource Versioning
test "resource versioning" {
  type ResourceVersion {
    version : Int
    timestamp : Int
    changes : Array[String]
    data : String
  }
  
  type VersionedResource {
    id : Int
    name : String
    current_version : Int
    versions : Array[ResourceVersion]
  }
  
  let create_versioned_resource = fn(id, name, initial_data) {
    let initial_version = {
      version: 1,
      timestamp: 1000,  // Simulated timestamp
      changes: ["Initial creation"],
      data: initial_data
    }
    
    {
      id: id,
      name: name,
      current_version: 1,
      versions: [initial_version]
    }
  }
  
  let update_resource = fn(resource, changes, new_data) {
    let new_version = {
      version: resource.current_version + 1,
      timestamp: 1000 + resource.current_version,  // Simulated timestamp
      changes: changes,
      data: new_data
    }
    
    {
      id: resource.id,
      name: resource.name,
      current_version: resource.current_version + 1,
      versions: resource.versions @ [new_version]
    }
  }
  
  let get_version = fn(resource, version) {
    for v in resource.versions {
      if v.version == version {
        return Some(v)
      }
    }
    None
  }
  
  let rollback_to_version = fn(resource, version) {
    match get_version(resource, version) {
      Some(target_version) => {
        {
          id: resource.id,
          name: resource.name,
          current_version: version,
          versions: resource.versions
        }
      }
      None => resource  // Version not found
    }
  }
  
  let get_version_history = fn(resource) {
    let mut history = []
    
    for version in resource.versions {
      history = history @ [{
        version: version.version,
        timestamp: version.timestamp,
        changes: version.changes
      }]
    }
    
    history
  }
  
  let compare_versions = fn(resource, version1, version2) {
    match (get_version(resource, version1), get_version(resource, version2)) {
      (Some(v1), Some(v2)) => {
        let mut differences = []
        
        if v1.data != v2.data {
          differences = differences @ ["Data changed"]
        }
        
        if v1.changes.length() != v2.changes.length() {
          differences = differences @ ["Change count differs"]
        }
        
        differences
      }
      _ => []  // One or both versions not found
    }
  }
  
  // Test resource versioning
  let resource = create_versioned_resource(1, "config", "port=8080")
  
  assert_eq(resource.current_version, 1)
  assert_eq(resource.versions.length(), 1)
  
  // Update resource
  let resource1 = update_resource(resource, ["Changed port"], "port=9090")
  assert_eq(resource1.current_version, 2)
  assert_eq(resource1.versions.length(), 2)
  
  let resource2 = update_resource(resource1, ["Added debug flag"], "port=9090\ndebug=true")
  assert_eq(resource2.current_version, 3)
  assert_eq(resource2.versions.length(), 3)
  
  // Get specific version
  match get_version(resource2, 2) {
    Some(version) => {
      assert_eq(version.version, 2)
      assert_eq(version.data, "port=9090")
      assert_eq(version.changes.length(), 1)
      assert_eq(version.changes[0], "Changed port")
    }
    None => assert_true(false)
  }
  
  // Rollback to version 2
  let resource3 = rollback_to_version(resource2, 2)
  assert_eq(resource3.current_version, 2)
  
  // Get current version data
  match get_version(resource3, resource3.current_version) {
    Some(version) => assert_eq(version.data, "port=9090")
    None => assert_true(false)
  }
  
  // Get version history
  let history = get_version_history(resource2)
  assert_eq(history.length(), 3)
  assert_eq(history[0].version, 1)
  assert_eq(history[1].version, 2)
  assert_eq(history[2].version, 3)
  
  // Compare versions
  let differences = compare_versions(resource2, 1, 3)
  assert_eq(differences.length(), 1)
  assert_eq(differences[0], "Data changed")
}

// Test 9: Resource Backup and Recovery
test "resource backup and recovery" {
  type ResourceBackup {
    backup_id : Int
    timestamp : Int
    resource_data : String
    metadata : Array[(String, String)]
  }
  
  type BackupManager {
    backups : Array[ResourceBackup]
    next_backup_id : Int
  }
  
  let create_backup_manager = fn() {
    { backups: [], next_backup_id: 1 }
  }
  
  let create_backup = fn(manager, resource_data, metadata) {
    let backup = {
      backup_id: manager.next_backup_id,
      timestamp: 1000,  // Simulated timestamp
      resource_data: resource_data,
      metadata: metadata
    }
    
    {
      backups: manager.backups @ [backup],
      next_backup_id: manager.next_backup_id + 1
    }
  }
  
  let restore_from_backup = fn(manager, backup_id) {
    for backup in manager.backups {
      if backup.backup_id == backup_id {
        return Some(backup)
      }
    }
    None
  }
  
  let delete_backup = fn(manager, backup_id) {
    let mut updated_backups = []
    
    for backup in manager.backups {
      if backup.backup_id != backup_id {
        updated_backups = updated_backups @ [backup]
      }
    }
    
    { backups: updated_backups, next_backup_id: manager.next_backup_id }
  }
  
  let get_backup_list = fn(manager) {
    let mut list = []
    
    for backup in manager.backups {
      list = list @ [{
        backup_id: backup.backup_id,
        timestamp: backup.timestamp,
        size: backup.resource_data.length()
      }]
    }
    
    list
  }
  
  let find_backup_by_metadata = fn(manager, key, value) {
    let mut matching_backups = []
    
    for backup in manager.backups {
      for (k, v) in backup.metadata {
        if k == key and v == value {
          matching_backups = matching_backups @ [backup]
          break
        }
      }
    }
    
    matching_backups
  }
  
  // Test backup and recovery
  let manager = create_backup_manager()
  
  // Create backups
  let metadata1 = [("type", "config"), ("version", "1.0")]
  let manager1 = create_backup(manager, "port=8080\nhost=localhost", metadata1)
  
  let metadata2 = [("type", "config"), ("version", "1.1")]
  let manager2 = create_backup(manager1, "port=9090\nhost=localhost", metadata2)
  
  let metadata3 = [("type", "database"), ("version", "1.0")]
  let manager3 = create_backup(manager2, "users=100\nposts=500", metadata3)
  
  assert_eq(manager3.backups.length(), 3)
  
  // Test restore from backup
  match restore_from_backup(manager3, 2) {
    Some(backup) => {
      assert_eq(backup.backup_id, 2)
      assert_eq(backup.resource_data, "port=9090\nhost=localhost")
      assert_eq(backup.metadata.length(), 2)
    }
    None => assert_true(false)
  }
  
  // Test backup list
  let backup_list = get_backup_list(manager3)
  assert_eq(backup_list.length(), 3)
  
  // Test find backup by metadata
  let config_backups = find_backup_by_metadata(manager3, "type", "config")
  assert_eq(config_backups.length(), 2)
  
  let version_1_backups = find_backup_by_metadata(manager3, "version", "1.0")
  assert_eq(version_1_backups.length(), 2)
  
  // Test delete backup
  let manager4 = delete_backup(manager3, 2)
  assert_eq(manager4.backups.length(), 2)
  
  // Verify backup is deleted
  match restore_from_backup(manager4, 2) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Resource Migration
test "resource migration" {
  type MigrationStep {
    step_id : Int
    description : String
    from_version : Int
    to_version : Int
    migration_fn : String -> String  // Simplified migration function
  }
  
  type ResourceMigration {
    resource_id : Int
    current_version : Int
    target_version : Int
    steps : Array[MigrationStep]
    status : String  // "pending", "in_progress", "completed", "failed"
  }
  
  type MigrationManager {
    migrations : Array[ResourceMigration]
    available_steps : Array[MigrationStep]
  }
  
  let create_migration_manager = fn() {
    { migrations: [], available_steps: [] }
  }
  
  let add_migration_step = fn(manager, step_id, description, from_version, to_version, migration_fn) {
    let step = {
      step_id: step_id,
      description: description,
      from_version: from_version,
      to_version: to_version,
      migration_fn: migration_fn
    }
    
    { 
      migrations: manager.migrations, 
      available_steps: manager.available_steps @ [step] 
    }
  }
  
  let create_migration = fn(manager, resource_id, current_version, target_version) {
    // Find migration steps
    let mut applicable_steps = []
    
    for step in manager.available_steps {
      if step.from_version >= current_version and step.to_version <= target_version {
        applicable_steps = applicable_steps @ [step]
      }
    }
    
    let migration = {
      resource_id: resource_id,
      current_version: current_version,
      target_version: target_version,
      steps: applicable_steps,
      status: "pending"
    }
    
    { 
      migrations: manager.migrations @ [migration], 
      available_steps: manager.available_steps 
    }
  }
  
  let execute_migration = fn(manager, migration_id, initial_data) {
    let mut updated_migrations = []
    let mut result_data = initial_data
    let mut migration_found = false
    
    for migration in manager.migrations {
      if migration.resource_id == migration_id {
        migration_found = true
        
        // Execute migration steps
        for step in migration.steps {
          // Apply migration function
          result_data = step.migration_fn(result_data)
        }
        
        updated_migrations = updated_migrations @ [{
          resource_id: migration.resource_id,
          current_version: migration.target_version,
          target_version: migration.target_version,
          steps: migration.steps,
          status: "completed"
        }]
      } else {
        updated_migrations = updated_migrations @ [migration]
      }
    }
    
    if migration_found {
      (result_data, {
        migrations: updated_migrations,
        available_steps: manager.available_steps
      })
    } else {
      (initial_data, manager)  // Migration not found
    }
  }
  
  let get_migration_status = fn(manager, resource_id) {
    for migration in manager.migrations {
      if migration.resource_id == resource_id {
        return migration.status
      }
    }
    "not_found"
  }
  
  // Test resource migration
  let manager = create_migration_manager()
  
  // Add migration steps
  let manager1 = add_migration_step(manager, 1, "Add port field", 1, 2, fn(data) {
    data + "\nport=8080"
  })
  
  let manager2 = add_migration_step(manager1, 2, "Add host field", 2, 3, fn(data) {
    data + "\nhost=localhost"
  })
  
  let manager3 = add_migration_step(manager2, 3, "Add debug field", 3, 4, fn(data) {
    data + "\ndebug=false"
  })
  
  // Create migration from version 1 to 4
  let manager4 = create_migration(manager3, 1, 1, 4)
  
  assert_eq(manager4.migrations.length(), 1)
  assert_eq(manager4.migrations[0].steps.length(), 3)
  assert_eq(manager4.migrations[0].status, "pending")
  
  // Execute migration
  let initial_data = "version=1"
  let (migrated_data, manager5) = execute_migration(manager4, 1, initial_data)
  
  assert_eq(migrated_data, "version=1\nport=8080\nhost=localhost\ndebug=false")
  
  // Check migration status
  let status = get_migration_status(manager5, 1)
  assert_eq(status, "completed")
}