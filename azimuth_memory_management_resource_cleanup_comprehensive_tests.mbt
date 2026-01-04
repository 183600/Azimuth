// Azimuth Memory Management Resource Cleanup Comprehensive Tests
// This file contains comprehensive memory management and resource cleanup tests

// Test 1: Memory Pool Management
test "memory pool management for telemetry data allocation" {
  // Define memory block
  type MemoryBlock = {
    block_id: Int,
    size_bytes: Int,
    allocated: Bool,
    data: Option<String>,
    allocation_time: Int,
    last_accessed: Int
  }
  
  // Define memory pool
  type MemoryPool = {
    pool_id: String,
    total_memory_mb: Int,
    block_size_bytes: Int,
    blocks: Array<MemoryBlock>,
    allocated_blocks: Int,
    free_blocks: Int,
    fragmentation_ratio: Float
  }
  
  // Create memory pool
  let create_memory_pool = fn(pool_id: String, total_memory_mb: Int, block_size_bytes: Int) {
    let total_blocks = (total_memory_mb * 1024 * 1024) / block_size_bytes
    let mut blocks = []
    
    for i in 0..total_blocks {
      blocks = blocks.push({
        block_id: i,
        size_bytes: block_size_bytes,
        allocated: false,
        data: None,
        allocation_time: 0,
        last_accessed: 0
      })
    }
    
    {
      pool_id,
      total_memory_mb,
      block_size_bytes,
      blocks,
      allocated_blocks: 0,
      free_blocks: total_blocks,
      fragmentation_ratio: 0.0
    }
  }
  
  // Allocate memory block
  let allocate_block = fn(pool: MemoryPool, data: String) {
    let mut updated_blocks = pool.blocks
    let mut allocated_block = None
    
    // Find first free block
    for i in 0..updated_blocks.length() {
      if not(updated_blocks[i].allocated) {
        updated_blocks[i] = {
          block_id: updated_blocks[i].block_id,
          size_bytes: updated_blocks[i].size_bytes,
          allocated: true,
          data: Some(data),
          allocation_time: 1640995200,
          last_accessed: 1640995200
        }
        allocated_block = Some(updated_blocks[i])
        break
      }
    }
    
    match allocated_block {
      Some(block) => {
        {
          pool_id: pool.pool_id,
          total_memory_mb: pool.total_memory_mb,
          block_size_bytes: pool.block_size_bytes,
          blocks: updated_blocks,
          allocated_blocks: pool.allocated_blocks + 1,
          free_blocks: pool.free_blocks - 1,
          fragmentation_ratio: pool.fragmentation_ratio
        }
      }
      None => pool  # No free blocks available
    }
  }
  
  // Free memory block
  let free_block = fn(pool: MemoryPool, block_id: Int) {
    let mut updated_blocks = pool.blocks
    let mut found = false
    
    for i in 0..updated_blocks.length() {
      if updated_blocks[i].block_id == block_id && updated_blocks[i].allocated {
        updated_blocks[i] = {
          block_id: updated_blocks[i].block_id,
          size_bytes: updated_blocks[i].size_bytes,
          allocated: false,
          data: None,
          allocation_time: updated_blocks[i].allocation_time,
          last_accessed: updated_blocks[i].last_accessed
        }
        found = true
        break
      }
    }
    
    if found {
      {
        pool_id: pool.pool_id,
        total_memory_mb: pool.total_memory_mb,
        block_size_bytes: pool.block_size_bytes,
        blocks: updated_blocks,
        allocated_blocks: pool.allocated_blocks - 1,
        free_blocks: pool.free_blocks + 1,
        fragmentation_ratio: pool.fragmentation_ratio
      }
    } else {
      pool  # Block not found or already free
    }
  }
  
  // Calculate fragmentation ratio
  let calculate_fragmentation = fn(pool: MemoryPool) {
    let mut consecutive_free = 0
    let mut max_consecutive_free = 0
    let mut total_free_blocks = 0
    
    for block in pool.blocks {
      if not(block.allocated) {
        consecutive_free = consecutive_free + 1
        total_free_blocks = total_free_blocks + 1
        if consecutive_free > max_consecutive_free {
          max_consecutive_free = consecutive_free
        }
      } else {
        consecutive_free = 0
      }
    }
    
    if total_free_blocks > 0 {
      (total_free_blocks.to_float() - max_consecutive_free.to_float()) / total_free_blocks.to_float()
    } else {
      0.0
    }
  }
  
  // Test memory pool creation
  let pool = create_memory_pool("telemetry-pool", 10, 1024)  # 10MB pool with 1KB blocks
  assert_eq(pool.pool_id, "telemetry-pool")
  assert_eq(pool.total_memory_mb, 10)
  assert_eq(pool.block_size_bytes, 1024)
  assert_eq(pool.blocks.length(), 10240)  # 10MB / 1KB = 10240 blocks
  assert_eq(pool.allocated_blocks, 0)
  assert_eq(pool.free_blocks, 10240)
  assert_eq(pool.fragmentation_ratio, 0.0)
  
  // Test block allocation
  let pool1 = allocate_block(pool, "telemetry-data-1")
  assert_eq(pool1.allocated_blocks, 1)
  assert_eq(pool1.free_blocks, 10239)
  
  let pool2 = allocate_block(pool1, "telemetry-data-2")
  assert_eq(pool2.allocated_blocks, 2)
  assert_eq(pool2.free_blocks, 10238)
  
  // Verify allocated blocks
  let allocated_blocks = pool2.blocks.filter(fn(b) { b.allocated })
  assert_eq(allocated_blocks.length(), 2)
  assert_eq(allocated_blocks[0].data, Some("telemetry-data-1"))
  assert_eq(allocated_blocks[1].data, Some("telemetry-data-2"))
  
  // Test block deallocation
  let pool3 = free_block(pool2, allocated_blocks[0].block_id)
  assert_eq(pool3.allocated_blocks, 1)
  assert_eq(pool3.free_blocks, 10239)
  
  // Verify block is freed
  let freed_block = pool3.blocks.filter(fn(b) { b.block_id == allocated_blocks[0].block_id })[0]
  assert_false(freed_block.allocated)
  assert_eq(freed_block.data, None)
  
  // Test fragmentation calculation
  let fragmentation = calculate_fragmentation(pool3)
  assert_true(fragmentation >= 0.0 && fragmentation <= 1.0)
  
  // Test pool exhaustion
  let mut exhausted_pool = pool
  for i in 0..10240 {
    exhausted_pool = allocate_block(exhausted_pool, "data-" + i.to_string())
  }
  
  assert_eq(exhausted_pool.allocated_blocks, 10240)
  assert_eq(exhausted_pool.free_blocks, 0)
  
  // Try to allocate when pool is full
  let full_pool = allocate_block(exhausted_pool, "should-fail")
  assert_eq(full_pool.allocated_blocks, 10240)  # No change
  assert_eq(full_pool.free_blocks, 0)
  
  // Test pool reset
  let reset_pool = fn(pool: MemoryPool) {
    let mut reset_blocks = pool.blocks
    for i in 0..reset_blocks.length() {
      reset_blocks[i] = {
        block_id: reset_blocks[i].block_id,
        size_bytes: reset_blocks[i].size_bytes,
        allocated: false,
        data: None,
        allocation_time: 0,
        last_accessed: 0
      }
    }
    
    {
      pool_id: pool.pool_id,
      total_memory_mb: pool.total_memory_mb,
      block_size_bytes: pool.block_size_bytes,
      blocks: reset_blocks,
      allocated_blocks: 0,
      free_blocks: reset_blocks.length(),
      fragmentation_ratio: 0.0
    }
  }
  
  let reset_result = reset_pool(exhausted_pool)
  assert_eq(reset_result.allocated_blocks, 0)
  assert_eq(reset_result.free_blocks, 10240)
}

// Test 2: Garbage Collection for Telemetry Objects
test "garbage collection for telemetry objects and lifecycle management" {
  // Define telemetry object with GC metadata
  type GCtelemetryObject = {
    object_id: String,
    object_type: String,
    data: String,
    creation_time: Int,
    last_access_time: Int,
    access_count: Int,
    marked_for_gc: Bool,
    references: Array<String>  # IDs of objects this object references
  }
  
  // Define garbage collector
  type GarbageCollector = {
    objects: Array<GCtelemetryObject>,
    gc_threshold_seconds: Int,
    collection_count: Int,
    last_collection_time: Int,
    objects_collected: Int
  }
  
  // Create garbage collector
  let create_gc = fn(threshold_seconds: Int) {
    {
      objects: [],
      gc_threshold_seconds: threshold_seconds,
      collection_count: 0,
      last_collection_time: 0,
      objects_collected: 0
    }
  }
  
  // Add object to GC
  let add_object = fn(gc: GarbageCollector, object_id: String, object_type: String, data: String, references: Array<String>) {
    let new_object = {
      object_id: object_id,
      object_type: object_type,
      data: data,
      creation_time: 1640995200,
      last_access_time: 1640995200,
      access_count: 1,
      marked_for_gc: false,
      references: references
    }
    
    {
      objects: gc.objects.push(new_object),
      gc_threshold_seconds: gc.gc_threshold_seconds,
      collection_count: gc.collection_count,
      last_collection_time: gc.last_collection_time,
      objects_collected: gc.objects_collected
    }
  }
  
  // Access object (update access time and count)
  let access_object = fn(gc: GarbageCollector, object_id: String) {
    let mut updated_objects = gc.objects
    
    for i in 0..updated_objects.length() {
      if updated_objects[i].object_id == object_id {
        updated_objects[i] = {
          object_id: updated_objects[i].object_id,
          object_type: updated_objects[i].object_type,
          data: updated_objects[i].data,
          creation_time: updated_objects[i].creation_time,
          last_access_time: 1640995300,  # Current time
          access_count: updated_objects[i].access_count + 1,
          marked_for_gc: false,
          references: updated_objects[i].references
        }
        break
      }
    }
    
    {
      objects: updated_objects,
      gc_threshold_seconds: gc.gc_threshold_seconds,
      collection_count: gc.collection_count,
      last_collection_time: gc.last_collection_time,
      objects_collected: gc.objects_collected
    }
  }
  
  // Mark objects for GC (mark phase)
  let mark_objects = fn(gc: GarbageCollector, current_time: Int) {
    let mut updated_objects = gc.objects
    
    for i in 0..updated_objects.length() {
      let object = updated_objects[i]
      let time_since_last_access = current_time - object.last_access_time
      
      # Mark for GC if object hasn't been accessed for longer than threshold
      if time_since_last_access > gc.gc_threshold_seconds {
        updated_objects[i] = {
          object_id: object.object_id,
          object_type: object.object_type,
          data: object.data,
          creation_time: object.creation_time,
          last_access_time: object.last_access_time,
          access_count: object.access_count,
          marked_for_gc: true,
          references: object.references
        }
      }
    }
    
    {
      objects: updated_objects,
      gc_threshold_seconds: gc.gc_threshold_seconds,
      collection_count: gc.collection_count,
      last_collection_time: gc.last_collection_time,
      objects_collected: gc.objects_collected
    }
  }
  
  // Sweep marked objects (sweep phase)
  let sweep_objects = fn(gc: GarbageCollector) {
    let mut remaining_objects = []
    let mut collected_count = 0
    
    for object in gc.objects {
      if object.marked_for_gc {
        collected_count = collected_count + 1
      } else {
        remaining_objects = remaining_objects.push(object)
      }
    }
    
    {
      objects: remaining_objects,
      gc_threshold_seconds: gc.gc_threshold_seconds,
      collection_count: gc.collection_count + 1,
      last_collection_time: 1640995300,
      objects_collected: gc.objects_collected + collected_count
    }
  }
  
  // Full garbage collection cycle
  let run_gc_cycle = fn(gc: GarbageCollector, current_time: Int) {
    let marked_gc = mark_objects(gc, current_time)
    sweep_objects(marked_gc)
  }
  
  // Test garbage collection
  let gc = create_gc(300)  # 5 minutes threshold
  
  # Add objects
  let gc1 = add_object(gc, "span-1", "span", "telemetry-span-data-1", [])
  let gc2 = add_object(gc1, "span-2", "span", "telemetry-span-data-2", ["span-1"])
  let gc3 = add_object(gc2, "metric-1", "metric", "telemetry-metric-data-1", [])
  let gc4 = add_object(gc3, "trace-1", "trace", "telemetry-trace-data-1", ["span-1", "span-2"])
  
  assert_eq(gc4.objects.length(), 4)
  
  # Access some objects to update their access time
  let gc5 = access_object(gc4, "span-1")
  let gc6 = access_object(gc5, "trace-1")
  
  # Verify access time and count updated
  let span1 = gc6.objects.filter(fn(o) { o.object_id == "span-1" })[0]
  let trace1 = gc6.objects.filter(fn(o) { o.object_id == "trace-1" })[0]
  let span2 = gc6.objects.filter(fn(o) { o.object_id == "span-2" })[0]
  let metric1 = gc6.objects.filter(fn(o) { o.object_id == "metric-1" })[0]
  
  assert_eq(span1.last_access_time, 1640995300)
  assert_eq(span1.access_count, 2)
  assert_eq(trace1.last_access_time, 1640995300)
  assert_eq(trace1.access_count, 2)
  
  assert_eq(span2.last_access_time, 1640995200)  # Not accessed
  assert_eq(span2.access_count, 1)
  assert_eq(metric1.last_access_time, 1640995200)  # Not accessed
  assert_eq(metric1.access_count, 1)
  
  # Run garbage collection (current time is 1640995300, threshold is 300 seconds)
  # Since all objects were created at 1640995200 and threshold is 300 seconds,
  # and current time is 1640995300 (100 seconds later), no objects should be collected
  let gc7 = run_gc_cycle(gc6, 1640995300)
  assert_eq(gc7.objects.length(), 4)  # No objects collected
  assert_eq(gc7.collection_count, 1)
  assert_eq(gc7.objects_collected, 0)
  
  # Simulate time passing (400 seconds later)
  let gc8 = run_gc_cycle(gc7, 1640995600)  # 400 seconds after creation
  
  # Objects not accessed since 1640995300 should be collected
  # span-1 and trace-1 were accessed at 1640995300, so they should remain
  # span-2 and metric-1 were last accessed at 1640995200, so they should be collected
  assert_eq(gc8.objects.length(), 2)
  assert_eq(gc8.collection_count, 2)
  assert_eq(gc8.objects_collected, 2)
  
  # Verify remaining objects
  let remaining_objects = gc8.objects
  let remaining_ids = remaining_objects.map(fn(o) { o.object_id })
  assert_true(remaining_ids.contains("span-1"))
  assert_true(remaining_ids.contains("trace-1"))
  assert_false(remaining_ids.contains("span-2"))
  assert_false(remaining_ids.contains("metric-1"))
  
  # Test reference handling
  let gc9 = add_object(gc8, "span-3", "span", "telemetry-span-data-3", ["span-1"])
  let gc10 = access_object(gc9, "span-3")
  
  # Even though span-3 references span-1, span-1 should remain because it was accessed recently
  let gc11 = run_gc_cycle(gc10, 1640995700)  # 500 seconds after creation
  assert_eq(gc11.objects.length(), 3)  # span-1, trace-1, span-3
}

// Test 3: Resource Leak Detection
test "resource leak detection and prevention in telemetry operations" {
  // Define resource tracker
  type ResourceTracker = {
    resource_id: String,
    resource_type: String,
    allocated: Bool,
    allocation_time: Int,
    allocation_stack: Array<String>,  # Simulated call stack
    leak_detected: Bool,
    leak_reason: Option<String>
  }
  
  // Define leak detector
  type LeakDetector = {
    resources: Array<ResourceTracker>,
    leak_threshold_seconds: Int,
    detection_count: Int,
    leaks_detected: Int
  }
  
  // Create leak detector
  let create_leak_detector = fn(threshold_seconds: Int) {
    {
      resources: [],
      leak_threshold_seconds: threshold_seconds,
      detection_count: 0,
      leaks_detected: 0
    }
  }
  
  // Allocate resource
  let allocate_resource = fn(detector: LeakDetector, resource_id: String, resource_type: String, allocation_stack: Array<String>) {
    # Check if resource already exists
    let existing_resource = detector.resources.filter(fn(r) { r.resource_id == resource_id })
    
    if existing_resource.length() > 0 {
      # Resource already allocated, potential double allocation
      detector
    } else {
      let new_resource = {
        resource_id: resource_id,
        resource_type: resource_type,
        allocated: true,
        allocation_time: 1640995200,
        allocation_stack: allocation_stack,
        leak_detected: false,
        leak_reason: None
      }
      
      {
        resources: detector.resources.push(new_resource),
        leak_threshold_seconds: detector.leak_threshold_seconds,
        detection_count: detector.detection_count,
        leaks_detected: detector.leaks_detected
      }
    }
  }
  
  // Deallocate resource
  let deallocate_resource = fn(detector: LeakDetector, resource_id: String) {
    let mut updated_resources = []
    let mut found = false
    
    for resource in detector.resources {
      if resource.resource_id == resource_id {
        found = true
        # Mark as deallocated (remove from tracking)
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    if found {
      {
        resources: updated_resources,
        leak_threshold_seconds: detector.leak_threshold_seconds,
        detection_count: detector.detection_count,
        leaks_detected: detector.leaks_detected
      }
    } else {
      detector  # Resource not found
    }
  }
  
  // Detect leaks
  let detect_leaks = fn(detector: LeakDetector, current_time: Int) {
    let mut updated_resources = detector.resources
    let mut new_leaks = 0
    
    for i in 0..updated_resources.length() {
      let resource = updated_resources[i]
      let allocation_age = current_time - resource.allocation_time
      
      if resource.allocated && allocation_age > detector.leak_threshold_seconds {
        # Potential leak detected
        updated_resources[i] = {
          resource_id: resource.resource_id,
          resource_type: resource.resource_type,
          allocated: resource.allocated,
          allocation_time: resource.allocation_time,
          allocation_stack: resource.allocation_stack,
          leak_detected: true,
          leak_reason: Some("Resource allocated for " + allocation_age.to_string() + " seconds, exceeding threshold of " + detector.leak_threshold_seconds.to_string() + " seconds")
        }
        new_leaks = new_leaks + 1
      }
    }
    
    {
      resources: updated_resources,
      leak_threshold_seconds: detector.leak_threshold_seconds,
      detection_count: detector.detection_count + 1,
      leaks_detected: detector.leaks_detected + new_leaks
    }
  }
  
  // Get leak report
  let get_leak_report = fn(detector: LeakDetector) {
    let leaked_resources = detector.resources.filter(fn(r) { r.leak_detected })
    
    {
      total_resources: detector.resources.length(),
      leaked_resources: leaked_resources.length(),
      leak_details: leaked_resources.map(fn(r) {
        {
          resource_id: r.resource_id,
          resource_type: r.resource_type,
          allocation_age: 1640995600 - r.allocation_time,  # Current time - allocation time
          allocation_stack: r.allocation_stack,
          leak_reason: r.leak_reason
        }
      })
    }
  }
  
  // Test leak detection
  let detector = create_leak_detector(300)  # 5 minutes threshold
  
  # Allocate resources
  let detector1 = allocate_resource(detector, "conn-1", "database-connection", ["telemetry-service", "handle-request"])
  let detector2 = allocate_resource(detector1, "conn-2", "database-connection", ["telemetry-service", "process-data"])
  let detector3 = allocate_resource(detector2, "buffer-1", "memory-buffer", ["telemetry-service", "serialize-data"])
  let detector4 = allocate_resource(detector3, "file-1", "file-handle", ["telemetry-service", "write-log"])
  
  assert_eq(detector4.resources.length(), 4)
  
  # Deallocate some resources
  let detector5 = deallocate_resource(detector4, "conn-2")
  let detector6 = deallocate_resource(detector5, "buffer-1")
  
  assert_eq(detector6.resources.length(), 2)  # conn-1 and file-1 remain
  
  # Run leak detection (no leaks yet, as not enough time has passed)
  let detector7 = detect_leaks(detector6, 1640995300)  # 100 seconds after allocation
  assert_eq(detector7.leaks_detected, 0)
  
  # Simulate time passing (400 seconds later)
  let detector8 = detect_leaks(detector7, 1640995600)  # 400 seconds after allocation
  
  # Should detect leaks for conn-1 and file-1
  assert_eq(detector8.leaks_detected, 2)
  
  # Get leak report
  let leak_report = get_leak_report(detector8)
  assert_eq(leak_report.total_resources, 2)
  assert_eq(leak_report.leaked_resources, 2)
  assert_eq(leak_report.leak_details.length(), 2)
  
  # Verify leak details
  let leaked_resource_ids = leak_report.leak_details.map(fn(d) { d.resource_id })
  assert_true(leaked_resource_ids.contains("conn-1"))
  assert_true(leaked_resource_ids.contains("file-1"))
  
  # Test double allocation detection
  let detector9 = allocate_resource(detector8, "conn-1", "database-connection", ["telemetry-service", "handle-request"])
  assert_eq(detector9.resources.length(), 2)  # No change, conn-1 already exists
  
  # Test resource cleanup
  let cleanup_leaked_resources = fn(detector: LeakDetector) {
    let mut cleaned_resources = []
    let mut cleaned_count = 0
    
    for resource in detector.resources {
      if resource.leak_detected {
        # Simulate cleanup
        cleaned_count = cleaned_count + 1
      } else {
        cleaned_resources = cleaned_resources.push(resource)
      }
    }
    
    {
      resources: cleaned_resources,
      leak_threshold_seconds: detector.leak_threshold_seconds,
      detection_count: detector.detection_count,
      leaks_detected: detector.leaks_detected
    }
  }
  
  let detector10 = cleanup_leaked_resources(detector8)
  assert_eq(detector10.resources.length(), 0)  # All leaked resources cleaned up
}

// Test 4: Memory Usage Monitoring
test "memory usage monitoring and optimization for telemetry systems" {
  // Define memory usage snapshot
  type MemorySnapshot = {
    timestamp: Int,
    total_memory_mb: Float,
    used_memory_mb: Float,
    free_memory_mb: Float,
    telemetry_memory_mb: Float,
    heap_memory_mb: Float,
    stack_memory_mb: Float,
    gc_memory_mb: Float
  }
  
  // Define memory monitor
  type MemoryMonitor = {
    snapshots: Array<MemorySnapshot>,
    alert_threshold_mb: Float,
    optimization_threshold_mb: Float,
    alerts_triggered: Int,
    optimizations_triggered: Int
  }
  
  // Create memory monitor
  let create_memory_monitor = fn(alert_threshold: Float, optimization_threshold: Float) {
    {
      snapshots: [],
      alert_threshold_mb: alert_threshold,
      optimization_threshold_mb: optimization_threshold,
      alerts_triggered: 0,
      optimizations_triggered: 0
    }
  }
  
  // Take memory snapshot
  let take_snapshot = fn(monitor: MemoryMonitor, total_memory: Float, used_memory: Float, telemetry_memory: Float, heap_memory: Float, stack_memory: Float, gc_memory: Float) {
    let snapshot = {
      timestamp: 1640995200 + monitor.snapshots.length(),
      total_memory_mb: total_memory,
      used_memory_mb: used_memory,
      free_memory_mb: total_memory - used_memory,
      telemetry_memory_mb: telemetry_memory,
      heap_memory_mb: heap_memory,
      stack_memory_mb: stack_memory,
      gc_memory_mb: gc_memory
    }
    
    let updated_snapshots = monitor.snapshots.push(snapshot)
    
    # Check for alerts
    let alerts_triggered = if used_memory > monitor.alert_threshold_mb {
      monitor.alerts_triggered + 1
    } else {
      monitor.alerts_triggered
    }
    
    # Check for optimization triggers
    let optimizations_triggered = if used_memory > monitor.optimization_threshold_mb {
      monitor.optimizations_triggered + 1
    } else {
      monitor.optimizations_triggered
    }
    
    {
      snapshots: updated_snapshots,
      alert_threshold_mb: monitor.alert_threshold_mb,
      optimization_threshold_mb: monitor.optimization_threshold_mb,
      alerts_triggered: alerts_triggered,
      optimizations_triggered: optimizations_triggered
    }
  }
  
  // Analyze memory trends
  let analyze_memory_trends = fn(monitor: MemoryMonitor) {
    if monitor.snapshots.length() < 2 {
      {
        trend: "insufficient_data",
        growth_rate_mb_per_min: 0.0,
        telemetry_percentage: 0.0,
        heap_percentage: 0.0,
        gc_efficiency: 0.0
      }
    } else {
      let first = monitor.snapshots[0]
      let last = monitor.snapshots[monitor.snapshots.length() - 1]
      
      let time_diff_minutes = (last.timestamp - first.timestamp) / 60.0
      let memory_diff = last.used_memory_mb - first.used_memory_mb
      let growth_rate = memory_diff / time_diff_minutes
      
      let telemetry_percentage = (last.telemetry_memory_mb / last.used_memory_mb) * 100.0
      let heap_percentage = (last.heap_memory_mb / last.used_memory_mb) * 100.0
      let gc_efficiency = if last.gc_memory_mb > 0 {
        (last.gc_memory_mb / last.used_memory_mb) * 100.0
      } else {
        0.0
      }
      
      let trend = if growth_rate > 1.0 {
        "increasing"
      } else if growth_rate < -1.0 {
        "decreasing"
      } else {
        "stable"
      }
      
      {
        trend: trend,
        growth_rate_mb_per_min: growth_rate,
        telemetry_percentage: telemetry_percentage,
        heap_percentage: heap_percentage,
        gc_efficiency: gc_efficiency
      }
    }
  }
  
  // Test memory monitoring
  let monitor = create_memory_monitor(800.0, 600.0)  # Alert at 800MB, optimize at 600MB
  
  # Take snapshots over time
  let monitor1 = take_snapshot(monitor, 1024.0, 400.0, 150.0, 200.0, 50.0, 50.0)
  let monitor2 = take_snapshot(monitor1, 1024.0, 450.0, 160.0, 220.0, 55.0, 55.0)
  let monitor3 = take_snapshot(monitor2, 1024.0, 500.0, 170.0, 240.0, 60.0, 60.0)
  let monitor4 = take_snapshot(monitor3, 1024.0, 550.0, 180.0, 260.0, 65.0, 65.0)
  let monitor5 = take_snapshot(monitor4, 1024.0, 620.0, 190.0, 280.0, 70.0, 70.0)
  let monitor6 = take_snapshot(monitor5, 1024.0, 650.0, 200.0, 290.0, 75.0, 75.0)
  let monitor7 = take_snapshot(monitor6, 1024.0, 750.0, 210.0, 300.0, 80.0, 80.0)
  let monitor8 = take_snapshot(monitor7, 1024.0, 820.0, 220.0, 310.0, 85.0, 85.0)
  
  # Verify snapshots
  assert_eq(monitor8.snapshots.length(), 8)
  
  # Verify alerts and optimizations
  assert_true(monitor8.alerts_triggered > 0)  # Should have triggered alerts
  assert_true(monitor8.optimizations_triggered > 0)  # Should have triggered optimizations
  
  # Analyze memory trends
  let trends = analyze_memory_trends(monitor8)
  assert_eq(trends.trend, "increasing")  # Memory is growing
  assert_true(trends.growth_rate_mb_per_min > 0)  # Positive growth rate
  assert_true(trends.telemetry_percentage > 0)  # Telemetry uses some memory
  assert_true(trends.heap_percentage > 0)  # Heap uses some memory
  assert_true(trends.gc_efficiency > 0)  # GC uses some memory
  
  # Test memory optimization recommendations
  let get_optimization_recommendations = fn(monitor: MemoryMonitor) {
    let trends = analyze_memory_trends(monitor)
    let last = monitor.snapshots[monitor.snapshots.length() - 1]
    let mut recommendations = []
    
    if trends.growth_rate_mb_per_min > 10.0 {
      recommendations = recommendations.push("High memory growth rate detected. Consider implementing more aggressive garbage collection.")
    }
    
    if trends.telemetry_percentage > 50.0 {
      recommendations = recommendations.push("Telemetry data is using more than 50% of memory. Consider implementing data compression or reducing retention period.")
    }
    
    if trends.heap_percentage > 70.0 {
      recommendations = recommendations.push("Heap usage is high. Consider optimizing object allocation patterns.")
    }
    
    if trends.gc_efficiency > 20.0 {
      recommendations = recommendations.push("GC overhead is high. Consider reducing object churn or increasing heap size.")
    }
    
    if last.free_memory_mb < 100.0 {
      recommendations = recommendations.push("Low free memory available. Consider increasing total memory or reducing memory usage.")
    }
    
    recommendations
  }
  
  let recommendations = get_optimization_recommendations(monitor8)
  assert_true(recommendations.length() > 0)  # Should have some recommendations
  
  # Test memory pressure simulation
  let simulate_memory_pressure = fn(monitor: MemoryMonitor, pressure_level: String) {
    match pressure_level {
      "low" => {
        # Slight memory increase
        take_snapshot(monitor, 1024.0, 550.0, 175.0, 265.0, 67.5, 67.5)
      }
      "medium" => {
        # Moderate memory increase
        take_snapshot(monitor, 1024.0, 700.0, 195.0, 335.0, 85.0, 85.0)
      }
      "high" => {
        # High memory increase
        take_snapshot(monitor, 1024.0, 900.0, 215.0, 430.0, 102.5, 102.5)
      }
      _ => monitor
    }
  }
  
  let low_pressure = simulate_memory_pressure(monitor7, "low")
  let medium_pressure = simulate_memory_pressure(monitor7, "medium")
  let high_pressure = simulate_memory_pressure(monitor7, "high")
  
  # Verify pressure levels
  assert_true(low_pressure.used_memory_mb < medium_pressure.used_memory_mb)
  assert_true(medium_pressure.used_memory_mb < high_pressure.used_memory_mb)
  
  # High pressure should trigger more alerts
  assert_true(high_pressure.alerts_triggered >= medium_pressure.alerts_triggered)
  assert_true(medium_pressure.alerts_triggered >= low_pressure.alerts_triggered)
}

// Test 5: Resource Cleanup Validation
test "resource cleanup validation and verification" {
  // Define cleanup validator
  type CleanupValidator = {
    validation_rules: Array<CleanupRule>,
    validation_results: Array<ValidationResult>,
    total_validations: Int,
    passed_validations: Int,
    failed_validations: Int
  }
  
  // Define cleanup rule
  type CleanupRule = {
    rule_id: String,
    resource_type: String,
    condition: String,
    action: String,
    expected_result: String
  }
  
  // Define validation result
  type ValidationResult = {
    rule_id: String,
    resource_id: String,
    passed: Bool,
    actual_result: String,
    expected_result: String,
    validation_time: Int
  }
  
  // Define resource state
  type ResourceState = {
    resource_id: String,
    resource_type: String,
    properties: Array<(String, String)>
  }
  
  // Create cleanup validator
  let create_cleanup_validator = fn() {
    {
      validation_rules: [
        {
          rule_id: "db-connection-closed",
          resource_type: "database-connection",
          condition: "after_cleanup",
          action: "close_connection",
          expected_result: "connection_closed"
        },
        {
          rule_id: "file-handle-closed",
          resource_type: "file-handle",
          condition: "after_cleanup",
          action: "close_handle",
          expected_result: "handle_closed"
        },
        {
          rule_id: "memory-freed",
          resource_type: "memory-buffer",
          condition: "after_cleanup",
          action: "free_memory",
          expected_result: "memory_freed"
        },
        {
          rule_id: "network-socket-closed",
          resource_type: "network-socket",
          condition: "after_cleanup",
          action: "close_socket",
          expected_result: "socket_closed"
        }
      ],
      validation_results: [],
      total_validations: 0,
      passed_validations: 0,
      failed_validations: 0
    }
  }
  
  // Simulate resource cleanup
  let cleanup_resource = fn(resource: ResourceState, rule: CleanupRule) {
    match rule.resource_type {
      "database-connection" => {
        # Simulate closing database connection
        {
          resource_id: resource.resource_id,
          resource_type: resource.resource_type,
          properties: [
            ("status", "closed"),
            ("cleanup_time", "1640995300")
          ]
        }
      }
      "file-handle" => {
        # Simulate closing file handle
        {
          resource_id: resource.resource_id,
          resource_type: resource.resource_type,
          properties: [
            ("status", "closed"),
            ("cleanup_time", "1640995300")
          ]
        }
      }
      "memory-buffer" => {
        # Simulate freeing memory buffer
        {
          resource_id: resource.resource_id,
          resource_type: resource.resource_type,
          properties: [
            ("status", "freed"),
            ("cleanup_time", "1640995300")
          ]
        }
      }
      "network-socket" => {
        # Simulate closing network socket
        {
          resource_id: resource.resource_id,
          resource_type: resource.resource_type,
          properties: [
            ("status", "closed"),
            ("cleanup_time", "1640995300")
          ]
        }
      }
      _ => resource
    }
  }
  
  // Validate cleanup result
  let validate_cleanup = fn(validator: CleanupValidator, resource: ResourceState, rule: CleanupRule) {
    let cleaned_resource = cleanup_resource(resource, rule)
    let actual_result = match cleaned_resource.properties.filter(fn(p) { p[0] == "status" })[0] {
      prop => prop[1]
      _ => "unknown"
    }
    
    let passed = actual_result == rule.expected_result
    
    let result = {
      rule_id: rule.rule_id,
      resource_id: resource.resource_id,
      passed: passed,
      actual_result: actual_result,
      expected_result: rule.expected_result,
      validation_time: 1640995300
    }
    
    let updated_results = validator.validation_results.push(result)
    let total_validations = validator.total_validations + 1
    let passed_validations = if passed {
      validator.passed_validations + 1
    } else {
      validator.passed_validations
    }
    let failed_validations = if not(passed) {
      validator.failed_validations + 1
    } else {
      validator.failed_validations
    }
    
    {
      validation_rules: validator.validation_rules,
      validation_results: updated_results,
      total_validations: total_validations,
      passed_validations: passed_validations,
      failed_validations: failed_validations
    }
  }
  
  // Run comprehensive cleanup validation
  let run_cleanup_validation = fn(validator: CleanupValidator, resources: Array<ResourceState>) {
    let mut result_validator = validator
    
    for resource in resources {
      # Find applicable rule
      let applicable_rules = validator.validation_rules.filter(fn(r) { r.resource_type == resource.resource_type })
      
      for rule in applicable_rules {
        result_validator = validate_cleanup(result_validator, resource, rule)
      }
    }
    
    result_validator
  }
  
  // Test cleanup validation
  let validator = create_cleanup_validator()
  
  # Create test resources
  let resources = [
    {
      resource_id: "conn-1",
      resource_type: "database-connection",
      properties: [
        ("status", "open"),
        ("host", "localhost"),
        ("port", "5432")
      ]
    },
    {
      resource_id: "file-1",
      resource_type: "file-handle",
      properties: [
        ("status", "open"),
        ("path", "/tmp/telemetry.log"),
        ("mode", "write")
      ]
    },
    {
      resource_id: "buffer-1",
      resource_type: "memory-buffer",
      properties: [
        ("status", "allocated"),
        ("size", "1024"),
        ("type", "telemetry-data")
      ]
    },
    {
      resource_id: "socket-1",
      resource_type: "network-socket",
      properties: [
        ("status", "connected"),
        ("host", "telemetry.example.com"),
        ("port", "8080")
      ]
    }
  ]
  
  # Run validation
  let result_validator = run_cleanup_validation(validator, resources)
  
  # Verify validation results
  assert_eq(result_validator.total_validations, 4)  # One validation per resource
  assert_eq(result_validator.passed_validations, 4)  # All should pass
  assert_eq(result_validator.failed_validations, 0)
  assert_eq(result_validator.validation_results.length(), 4)
  
  # Verify individual validation results
  let db_validation = result_validator.validation_results.filter(fn(r) { r.rule_id == "db-connection-closed" })[0]
  assert_eq(db_validation.resource_id, "conn-1")
  assert_true(db_validation.passed)
  assert_eq(db_validation.actual_result, "closed")
  assert_eq(db_validation.expected_result, "connection_closed")
  
  # Test validation failure scenario
  let create_failing_validator = fn() {
    {
      validation_rules: [
        {
          rule_id: "db-connection-closed",
          resource_type: "database-connection",
          condition: "after_cleanup",
          action: "close_connection",
          expected_result: "connection_closed"  # This won't match actual result
        }
      ],
      validation_results: [],
      total_validations: 0,
      passed_validations: 0,
      failed_validations: 0
    }
  }
  
  let failing_validator = create_failing_validator()
  let failing_resources = [
    {
      resource_id: "conn-bad",
      resource_type: "database-connection",
      properties: [
        ("status", "open"),  # This won't be cleaned up properly
        ("host", "localhost"),
        ("port", "5432")
      ]
    }
  ]
  
  # Simulate failed cleanup
  let cleanup_resource_failing = fn(resource: ResourceState, rule: CleanupRule) {
    # This cleanup fails, status remains "open"
    {
      resource_id: resource.resource_id,
      resource_type: resource.resource_type,
      properties: [
        ("status", "open"),  # Cleanup failed
        ("error", "cleanup_failed")
      ]
    }
  }
  
  # Manually create a failing validation result
  let failing_result = {
    rule_id: "db-connection-closed",
    resource_id: "conn-bad",
    passed: false,
    actual_result: "open",
    expected_result: "connection_closed",
    validation_time: 1640995300
  }
  
  let failing_validator_result = {
    validation_rules: failing_validator.validation_rules,
    validation_results: [failing_result],
    total_validations: 1,
    passed_validations: 0,
    failed_validations: 1
  }
  
  # Verify failure detection
  assert_eq(failing_validator_result.total_validations, 1)
  assert_eq(failing_validator_result.passed_validations, 0)
  assert_eq(failing_validator_result.failed_validations, 1)
  
  # Test validation report generation
  let generate_validation_report = fn(validator: CleanupValidator) {
    let success_rate = if validator.total_validations > 0 {
      (validator.passed_validations.to_float() / validator.total_validations.to_float()) * 100.0
    } else {
      0.0
    }
    
    let failed_validations = validator.validation_results.filter(fn(r) { not(r.passed) })
    
    {
      total_validations: validator.total_validations,
      passed_validations: validator.passed_validations,
      failed_validations: validator.failed_validations,
      success_rate: success_rate,
      failed_rule_details: failed_validations.map(fn(r) {
        {
          rule_id: r.rule_id,
          resource_id: r.resource_id,
          expected: r.expected_result,
          actual: r.actual_result
        }
      })
    }
  }
  
  let report = generate_validation_report(result_validator)
  assert_eq(report.total_validations, 4)
  assert_eq(report.passed_validations, 4)
  assert_eq(report.failed_validations, 0)
  assert_eq(report.success_rate, 100.0)
  assert_eq(report.failed_rule_details.length(), 0)
  
  let failing_report = generate_validation_report(failing_validator_result)
  assert_eq(failing_report.total_validations, 1)
  assert_eq(failing_report.passed_validations, 0)
  assert_eq(failing_report.failed_validations, 1)
  assert_eq(failing_report.success_rate, 0.0)
  assert_eq(failing_report.failed_rule_details.length(), 1)
}