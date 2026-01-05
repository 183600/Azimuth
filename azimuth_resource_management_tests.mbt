// Azimuth Resource Management and Cleanup Test Suite
// This file contains test cases for resource management and cleanup functionality

// Test 1: Memory Resource Management
test "memory resource management" {
  type MemoryBlock = {
    id: String,
    size: Int,
    allocated: Bool,
    data: Array[Int]  // Simulated memory data
  }
  
  type MemoryManager = {
    total_memory: Int,
    allocated_memory: Int,
    blocks: Array[MemoryBlock],
    lock_acquired: Bool
  }
  
  let create_memory_block = fn(id: String, size: Int) {
    {
      id,
      size,
      allocated: false,
      data: []  // Initially empty
    }
  }
  
  let create_memory_manager = fn(total_memory: Int) {
    {
      total_memory,
      allocated_memory: 0,
      blocks: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(manager: MemoryManager) {
    if not(manager.lock_acquired) {
      { manager | lock_acquired: true }
    } else {
      manager  // Lock already acquired
    }
  }
  
  let release_lock = fn(manager: MemoryManager) {
    { manager | lock_acquired: false }
  }
  
  let allocate_memory = fn(manager: MemoryManager, id: String, size: Int) {
    if manager.lock_acquired && (manager.allocated_memory + size) <= manager.total_memory {
      let block = create_memory_block(id, size)
      let filled_block = { block | 
        allocated: true,
        data: [0] * size  // Fill with zeros
      }
      
      {
        total_memory: manager.total_memory,
        allocated_memory: manager.allocated_memory + size,
        blocks: manager.blocks + [filled_block],
        lock_acquired: true
      }
    } else {
      manager  // Cannot allocate without lock or not enough memory
    }
  }
  
  let deallocate_memory = fn(manager: MemoryManager, id: String) {
    if manager.lock_acquired {
      let mut found = false
      let mut freed_size = 0
      let updated = manager.blocks.filter(fn(block) {
        if block.id == id {
          found = true
          freed_size = block.size
          false  // Remove this block
        } else {
          true
        }
      })
      
      if found {
        {
          total_memory: manager.total_memory,
          allocated_memory: manager.allocated_memory - freed_size,
          blocks: updated,
          lock_acquired: true
        }
      } else {
        manager  // Block not found
      }
    } else {
      manager  // Cannot deallocate without lock
    }
  }
  
  let safe_allocate_memory = fn(manager: MemoryManager, id: String, size: Int) {
    let locked_manager = acquire_lock(manager)
    let result = allocate_memory(locked_manager, id, size)
    release_lock(result)
  }
  
  let safe_deallocate_memory = fn(manager: MemoryManager, id: String) {
    let locked_manager = acquire_lock(manager)
    let result = deallocate_memory(locked_manager, id)
    release_lock(result)
  }
  
  let manager = create_memory_manager(1024)  // 1024 bytes total
  assert_eq(manager.total_memory, 1024)
  assert_eq(manager.allocated_memory, 0)
  assert_eq(manager.blocks.length(), 0)
  
  // Test safe allocate
  let manager1 = safe_allocate_memory(manager, "block1", 256)
  assert_eq(manager1.allocated_memory, 256)
  assert_eq(manager1.blocks.length(), 1)
  
  let manager2 = safe_allocate_memory(manager1, "block2", 512)
  assert_eq(manager2.allocated_memory, 768)
  assert_eq(manager2.blocks.length(), 2)
  
  // Test allocation failure (not enough memory)
  let manager3 = safe_allocate_memory(manager2, "block3", 512)
  assert_eq(manager3.allocated_memory, 768)  // No change
  assert_eq(manager3.blocks.length(), 2)
  
  // Test safe deallocate
  let manager4 = safe_deallocate_memory(manager3, "block1")
  assert_eq(manager4.allocated_memory, 512)
  assert_eq(manager4.blocks.length(), 1)
  
  // Can allocate again after deallocation
  let manager5 = safe_allocate_memory(manager4, "block3", 400)
  assert_eq(manager5.allocated_memory, 912)
  assert_eq(manager5.blocks.length(), 2)
}

// Test 2: File Handle Management
test "file handle management" {
  type FileHandle = {
    id: String,
    path: String,
    mode: String,  // "read", "write", "append"
    open: Bool,
    position: Int
  }
  
  type FileManager = {
    max_handles: Int,
    handles: Array[FileHandle],
    lock_acquired: Bool
  }
  
  let create_file_handle = fn(id: String, path: String, mode: String) {
    {
      id,
      path,
      mode,
      open: true,
      position: 0
    }
  }
  
  let create_file_manager = fn(max_handles: Int) {
    {
      max_handles,
      handles: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(manager: FileManager) {
    if not(manager.lock_acquired) {
      { manager | lock_acquired: true }
    } else {
      manager  // Lock already acquired
    }
  }
  
  let release_lock = fn(manager: FileManager) {
    { manager | lock_acquired: false }
  }
  
  let open_file = fn(manager: FileManager, id: String, path: String, mode: String) {
    if manager.lock_acquired && manager.handles.length() < manager.max_handles {
      let handle = create_file_handle(id, path, mode)
      {
        max_handles: manager.max_handles,
        handles: manager.handles + [handle],
        lock_acquired: true
      }
    } else {
      manager  // Cannot open without lock or at max handles
    }
  }
  
  let close_file = fn(manager: FileManager, id: String) {
    if manager.lock_acquired {
      let updated = manager.handles.map(fn(handle) {
        if handle.id == id {
          { handle | open: false }
        } else {
          handle
        }
      })
      
      {
        max_handles: manager.max_handles,
        handles: updated,
        lock_acquired: true
      }
    } else {
      manager  // Cannot close without lock
    }
  }
  
  let remove_closed_handles = fn(manager: FileManager) {
    if manager.lock_acquired {
      let open_handles = manager.handles.filter(fn(handle) { handle.open })
      {
        max_handles: manager.max_handles,
        handles: open_handles,
        lock_acquired: true
      }
    } else {
      manager  // Cannot remove without lock
    }
  }
  
  let safe_open_file = fn(manager: FileManager, id: String, path: String, mode: String) {
    let locked_manager = acquire_lock(manager)
    let result = open_file(locked_manager, id, path, mode)
    release_lock(result)
  }
  
  let safe_close_file = fn(manager: FileManager, id: String) {
    let locked_manager = acquire_lock(manager)
    let result = close_file(locked_manager, id)
    release_lock(result)
  }
  
  let safe_remove_closed_handles = fn(manager: FileManager) {
    let locked_manager = acquire_lock(manager)
    let result = remove_closed_handles(locked_manager)
    release_lock(result)
  }
  
  let manager = create_file_manager(5)
  assert_eq(manager.max_handles, 5)
  assert_eq(manager.handles.length(), 0)
  
  // Test safe open
  let manager1 = safe_open_file(manager, "file1", "/path/to/file1.txt", "read")
  assert_eq(manager1.handles.length(), 1)
  
  let manager2 = safe_open_file(manager1, "file2", "/path/to/file2.txt", "write")
  assert_eq(manager2.handles.length(), 2)
  
  let manager3 = safe_open_file(manager2, "file3", "/path/to/file3.txt", "append")
  assert_eq(manager3.handles.length(), 3)
  
  // Test safe close
  let manager4 = safe_close_file(manager3, "file1")
  let file1 = manager4.handles.filter(fn(h) { h.id == "file1" })[0]
  assert_false(file1.open)
  
  // Test remove closed handles
  let manager5 = safe_remove_closed_handles(manager4)
  assert_eq(manager5.handles.length(), 2)
  assert_true(manager5.handles.all(fn(h) { h.open }))
}

// Test 3: Connection Pool Management
test "connection pool management" {
  type Connection = {
    id: String,
    host: String,
    port: Int,
    in_use: Bool,
    created_at: Int,
    last_used: Int
  }
  
  type ConnectionPool = {
    max_connections: Int,
    connection_timeout: Int,  // in seconds
    connections: Array[Connection],
    lock_acquired: Bool
  }
  
  let create_connection = fn(id: String, host: String, port: Int, current_time: Int) {
    {
      id,
      host,
      port,
      in_use: false,
      created_at: current_time,
      last_used: current_time
    }
  }
  
  let create_connection_pool = fn(max_connections: Int, connection_timeout: Int) {
    {
      max_connections,
      connection_timeout,
      connections: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(pool: ConnectionPool) {
    if not(pool.lock_acquired) {
      { pool | lock_acquired: true }
    } else {
      pool  // Lock already acquired
    }
  }
  
  let release_lock = fn(pool: ConnectionPool) {
    { pool | lock_acquired: false }
  }
  
  let add_connection = fn(pool: ConnectionPool, connection: Connection) {
    if pool.lock_acquired && pool.connections.length() < pool.max_connections {
      {
        max_connections: pool.max_connections,
        connection_timeout: pool.connection_timeout,
        connections: pool.connections + [connection],
        lock_acquired: true
      }
    } else {
      pool  // Cannot add without lock or at max connections
    }
  }
  
  let acquire_connection = fn(pool: ConnectionPool) {
    if pool.lock_acquired {
      let mut found = false
      let mut updated = []
      
      for connection in pool.connections {
        if not(connection.in_use) && not(found) {
          updated = updated.push({ connection | in_use: true })
          found = true
        } else {
          updated = updated.push(connection)
        }
      }
      
      if found {
        {
          max_connections: pool.max_connections,
          connection_timeout: pool.connection_timeout,
          connections: updated,
          lock_acquired: true
        }
      } else {
        pool  // No available connections
      }
    } else {
      pool  // Cannot acquire without lock
    }
  }
  
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    if pool.lock_acquired {
      let updated = pool.connections.map(fn(connection) {
        if connection.id == connection_id {
          { connection | in_use: false, last_used: current_time }
        } else {
          connection
        }
      })
      
      {
        max_connections: pool.max_connections,
        connection_timeout: pool.connection_timeout,
        connections: updated,
        lock_acquired: true
      }
    } else {
      pool  // Cannot release without lock
    }
  }
  
  let remove_expired_connections = fn(pool: ConnectionPool, current_time: Int) {
    if pool.lock_acquired {
      let active = pool.connections.filter(fn(connection) { 
        not(connection.in_use) && 
        (current_time - connection.last_used) < pool.connection_timeout 
      })
      
      {
        max_connections: pool.max_connections,
        connection_timeout: pool.connection_timeout,
        connections: active,
        lock_acquired: true
      }
    } else {
      pool  // Cannot remove without lock
    }
  }
  
  let safe_add_connection = fn(pool: ConnectionPool, connection: Connection) {
    let locked_pool = acquire_lock(pool)
    let result = add_connection(locked_pool, connection)
    release_lock(result)
  }
  
  let safe_acquire_connection = fn(pool: ConnectionPool) {
    let locked_pool = acquire_lock(pool)
    let result = acquire_connection(locked_pool)
    release_lock(result)
  }
  
  let safe_release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    let locked_pool = acquire_lock(pool)
    let result = release_connection(locked_pool, connection_id, current_time)
    release_lock(result)
  }
  
  let safe_remove_expired_connections = fn(pool: ConnectionPool, current_time: Int) {
    let locked_pool = acquire_lock(pool)
    let result = remove_expired_connections(locked_pool, current_time)
    release_lock(result)
  }
  
  let pool = create_connection_pool(5, 300)  // 5 max connections, 300s timeout
  assert_eq(pool.max_connections, 5)
  assert_eq(pool.connections.length(), 0)
  
  let base_time = 1640995200
  
  // Add connections
  let conn1 = create_connection("conn1", "db.example.com", 5432, base_time)
  let conn2 = create_connection("conn2", "db.example.com", 5432, base_time + 10)
  
  let pool1 = safe_add_connection(pool, conn1)
  assert_eq(pool1.connections.length(), 1)
  
  let pool2 = safe_add_connection(pool1, conn2)
  assert_eq(pool2.connections.length(), 2)
  
  // Acquire connections
  let pool3 = safe_acquire_connection(pool2)
  let in_use_count = pool3.connections.filter(fn(c) { c.in_use }).length()
  assert_eq(in_use_count, 1)
  
  let pool4 = safe_acquire_connection(pool3)
  let in_use_count2 = pool4.connections.filter(fn(c) { c.in_use }).length()
  assert_eq(in_use_count2, 2)
  
  // Release connections
  let pool5 = safe_release_connection(pool4, "conn1", base_time + 100)
  let conn1_updated = pool5.connections.filter(fn(c) { c.id == "conn1" })[0]
  assert_false(conn1_updated.in_use)
  assert_eq(conn1_updated.last_used, base_time + 100)
  
  // Remove expired connections
  let pool6 = safe_remove_expired_connections(pool5, base_time + 500)  // 500s later
  assert_eq(pool6.connections.length(), 2)  // No connections expired yet
  
  let pool7 = safe_remove_expired_connections(pool6, base_time + 1000)  // 1000s later
  assert_eq(pool7.connections.length(), 0)  // All connections expired
}

// Test 4: Resource Cleanup on Shutdown
test "resource cleanup on shutdown" {
  type Resource = {
    id: String,
    type: String,  // "memory", "file", "connection"
    allocated: Bool,
    cleanup_called: Bool
  }
  
  type ResourceManager = {
    resources: Array[Resource],
    shutting_down: Bool,
    lock_acquired: Bool
  }
  
  let create_resource = fn(id: String, type: String) {
    {
      id,
      type,
      allocated: true,
      cleanup_called: false
    }
  }
  
  let create_resource_manager = fn() {
    {
      resources: [],
      shutting_down: false,
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(manager: ResourceManager) {
    if not(manager.lock_acquired) {
      { manager | lock_acquired: true }
    } else {
      manager  // Lock already acquired
    }
  }
  
  let release_lock = fn(manager: ResourceManager) {
    { manager | lock_acquired: false }
  }
  
  let allocate_resource = fn(manager: ResourceManager, resource: Resource) {
    if manager.lock_acquired && not(manager.shutting_down) {
      {
        resources: manager.resources + [resource],
        shutting_down: manager.shutting_down,
        lock_acquired: true
      }
    } else {
      manager  // Cannot allocate during shutdown or without lock
    }
  }
  
  let cleanup_resource = fn(resource: Resource) {
    { resource | allocated: false, cleanup_called: true }
  }
  
  let cleanup_all_resources = fn(manager: ResourceManager) {
    if manager.lock_acquired {
      let cleaned = manager.resources.map(cleanup_resource)
      {
        resources: cleaned,
        shutting_down: true,
        lock_acquired: true
      }
    } else {
      manager  // Cannot cleanup without lock
    }
  }
  
  let shutdown = fn(manager: ResourceManager) {
    if manager.lock_acquired {
      let cleaned = cleanup_all_resources(manager)
      { cleaned | lock_acquired: false }
    } else {
      manager  // Cannot shutdown without lock
    }
  }
  
  let safe_allocate_resource = fn(manager: ResourceManager, resource: Resource) {
    let locked_manager = acquire_lock(manager)
    let result = allocate_resource(locked_manager, resource)
    release_lock(result)
  }
  
  let safe_shutdown = fn(manager: ResourceManager) {
    let locked_manager = acquire_lock(manager)
    let result = shutdown(locked_manager)
    result
  }
  
  let manager = create_resource_manager()
  assert_false(manager.shutting_down)
  assert_eq(manager.resources.length(), 0)
  
  // Allocate resources
  let resource1 = create_resource("res1", "memory")
  let resource2 = create_resource("res2", "file")
  let resource3 = create_resource("res3", "connection")
  
  let manager1 = safe_allocate_resource(manager, resource1)
  assert_eq(manager1.resources.length(), 1)
  
  let manager2 = safe_allocate_resource(manager1, resource2)
  assert_eq(manager2.resources.length(), 2)
  
  let manager3 = safe_allocate_resource(manager2, resource3)
  assert_eq(manager3.resources.length(), 3)
  
  // Test shutdown
  let manager4 = safe_shutdown(manager3)
  assert_true(manager4.shutting_down)
  assert_false(manager4.lock_acquired)  // Lock should be released after shutdown
  
  // Check all resources are cleaned up
  assert_true(manager4.resources.all(fn(r) { not(r.allocated) && r.cleanup_called }))
  
  // Cannot allocate resources after shutdown
  let resource4 = create_resource("res4", "memory")
  let manager5 = safe_allocate_resource(manager4, resource4)
  assert_eq(manager5.resources.length(), 3)  // No change
}

// Test 5: Resource Leak Detection
test "resource leak detection" {
  type ResourceLeak = {
    resource_id: String,
    resource_type: String,
    allocated_at: Int,
    stack_trace: Array[String],
    size: Int
  }
  
  type LeakDetector = {
    allocated_resources: Array[ResourceLeak],
    leak_threshold: Int,  // in seconds
    lock_acquired: Bool
  }
  
  let create_resource_leak = fn(resource_id: String, resource_type: String, allocated_at: Int, stack_trace: Array[String], size: Int) {
    {
      resource_id,
      resource_type,
      allocated_at,
      stack_trace,
      size
    }
  }
  
  let create_leak_detector = fn(leak_threshold: Int) {
    {
      allocated_resources: [],
      leak_threshold,
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(detector: LeakDetector) {
    if not(detector.lock_acquired) {
      { detector | lock_acquired: true }
    } else {
      detector  // Lock already acquired
    }
  }
  
  let release_lock = fn(detector: LeakDetector) {
    { detector | lock_acquired: false }
  }
  
  let track_allocation = fn(detector: LeakDetector, resource_id: String, resource_type: String, allocated_at: Int, stack_trace: Array[String>, size: Int) {
    if detector.lock_acquired {
      let leak = create_resource_leak(resource_id, resource_type, allocated_at, stack_trace, size)
      {
        allocated_resources: detector.allocated_resources + [leak],
        leak_threshold: detector.leak_threshold,
        lock_acquired: true
      }
    } else {
      detector  // Cannot track without lock
    }
  }
  
  let track_deallocation = fn(detector: LeakDetector, resource_id: String) {
    if detector.lock_acquired {
      let filtered = detector.allocated_resources.filter(fn(leak) { leak.resource_id != resource_id })
      {
        allocated_resources: filtered,
        leak_threshold: detector.leak_threshold,
        lock_acquired: true
      }
    } else {
      detector  // Cannot track without lock
    }
  }
  
  let detect_leaks = fn(detector: LeakDetector, current_time: Int) {
    if detector.lock_acquired {
      let leaks = detector.allocated_resources.filter(fn(leak) { 
        (current_time - leak.allocated_at) > detector.leak_threshold 
      })
      leaks
    } else {
      []  // Cannot detect without lock
    }
  }
  
  let safe_track_allocation = fn(detector: LeakDetector, resource_id: String, resource_type: String, allocated_at: Int, stack_trace: Array[String], size: Int) {
    let locked_detector = acquire_lock(detector)
    let result = track_allocation(locked_detector, resource_id, resource_type, allocated_at, stack_trace, size)
    release_lock(result)
  }
  
  let safe_track_deallocation = fn(detector: LeakDetector, resource_id: String) {
    let locked_detector = acquire_lock(detector)
    let result = track_deallocation(locked_detector, resource_id)
    release_lock(result)
  }
  
  let safe_detect_leaks = fn(detector: LeakDetector, current_time: Int) {
    let locked_detector = acquire_lock(detector)
    let result = detect_leaks(locked_detector, current_time)
    release_lock(locked_detector)
    result
  }
  
  let detector = create_leak_detector(60)  // 60 seconds threshold
  assert_eq(detector.leak_threshold, 60)
  assert_eq(detector.allocated_resources.length(), 0)
  
  let base_time = 1640995200
  
  // Track allocations
  let detector1 = safe_track_allocation(detector, "res1", "memory", base_time, ["func1", "func2"], 1024)
  assert_eq(detector1.allocated_resources.length(), 1)
  
  let detector2 = safe_track_allocation(detector1, "res2", "file", base_time + 10, ["func3", "func4"], 512)
  assert_eq(detector2.allocated_resources.length(), 2)
  
  let detector3 = safe_track_allocation(detector2, "res3", "connection", base_time + 20, ["func5", "func6"], 256)
  assert_eq(detector3.allocated_resources.length(), 3)
  
  // Track deallocation
  let detector4 = safe_track_deallocation(detector3, "res2")
  assert_eq(detector4.allocated_resources.length(), 2)
  
  // Detect leaks (before threshold)
  let leaks1 = safe_detect_leaks(detector4, base_time + 50)
  assert_eq(leaks1.length(), 0)
  
  // Detect leaks (after threshold)
  let leaks2 = safe_detect_leaks(detector4, base_time + 100)
  assert_eq(leaks2.length(), 2)  // res1 and res3 should be detected as leaks
  assert_true(leaks2.some(fn(l) { l.resource_id == "res1" }))
  assert_true(leaks2.some(fn(l) { l.resource_id == "res3" }))
}

// Test 6: Resource Usage Monitoring
test "resource usage monitoring" {
  type ResourceUsage = {
    resource_type: String,
    used: Int,
    total: Int,
    timestamp: Int
  }
  
  type ResourceMonitor = {
    usages: Array[ResourceUsage],
    history_size: Int,
    lock_acquired: Bool
  }
  
  let create_resource_usage = fn(resource_type: String, used: Int, total: Int, timestamp: Int) {
    {
      resource_type,
      used,
      total,
      timestamp
    }
  }
  
  let create_resource_monitor = fn(history_size: Int) {
    {
      usages: [],
      history_size,
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(monitor: ResourceMonitor) {
    if not(monitor.lock_acquired) {
      { monitor | lock_acquired: true }
    } else {
      monitor  // Lock already acquired
    }
  }
  
  let release_lock = fn(monitor: ResourceMonitor) {
    { monitor | lock_acquired: false }
  }
  
  let record_usage = fn(monitor: ResourceMonitor, resource_type: String, used: Int, total: Int, timestamp: Int) {
    if monitor.lock_acquired {
      let usage = create_resource_usage(resource_type, used, total, timestamp)
      let updated = monitor.usages + [usage]
      
      // Keep only the most recent history_size entries
      let trimmed = if updated.length() > monitor.history_size {
        updated.slice(updated.length() - monitor.history_size, updated.length())
      } else {
        updated
      }
      
      {
        usages: trimmed,
        history_size: monitor.history_size,
        lock_acquired: true
      }
    } else {
      monitor  // Cannot record without lock
    }
  }
  
  let get_latest_usage = fn(monitor: ResourceMonitor, resource_type: String) {
    let mut latest = None
    
    for usage in monitor.usages {
      if usage.resource_type == resource_type {
        match latest {
          Some(l) => {
            if usage.timestamp > l.timestamp {
              latest = Some(usage)
            }
          }
          None => latest = Some(usage)
        }
      }
    }
    
    latest
  }
  
  let get_usage_trend = fn(monitor: ResourceMonitor, resource_type: String) {
    let filtered = monitor.usages.filter(fn(u) { u.resource_type == resource_type })
    
    if filtered.length() < 2 {
      "stable"
    } else {
      let sorted = filtered.sort(fn(a, b) { a.timestamp <= b.timestamp })
      let first = sorted[0]
      let last = sorted[sorted.length() - 1]
      
      let first_ratio = first.used.to_float() / first.total.to_float()
      let last_ratio = last.used.to_float() / last.total.to_float()
      
      if last_ratio > first_ratio + 0.1 {
        "increasing"
      } else if last_ratio < first_ratio - 0.1 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  let safe_record_usage = fn(monitor: ResourceMonitor, resource_type: String, used: Int, total: Int, timestamp: Int) {
    let locked_monitor = acquire_lock(monitor)
    let result = record_usage(locked_monitor, resource_type, used, total, timestamp)
    release_lock(result)
  }
  
  let monitor = create_resource_monitor(10)
  assert_eq(monitor.history_size, 10)
  assert_eq(monitor.usages.length(), 0)
  
  let base_time = 1640995200
  
  // Record usages
  let monitor1 = safe_record_usage(monitor, "memory", 512, 1024, base_time)
  assert_eq(monitor1.usages.length(), 1)
  
  let monitor2 = safe_record_usage(monitor1, "memory", 600, 1024, base_time + 10)
  assert_eq(monitor2.usages.length(), 2)
  
  let monitor3 = safe_record_usage(monitor2, "disk", 2048, 4096, base_time + 20)
  assert_eq(monitor3.usages.length(), 3)
  
  let monitor4 = safe_record_usage(monitor3, "memory", 700, 1024, base_time + 30)
  assert_eq(monitor4.usages.length(), 4)
  
  // Test get latest usage
  let latest_memory = get_latest_usage(monitor4, "memory")
  assert_true(latest_memory.is_some())
  
  match latest_memory {
    Some(usage) => {
      assert_eq(usage.resource_type, "memory")
      assert_eq(usage.used, 700)
      assert_eq(usage.total, 1024)
      assert_eq(usage.timestamp, base_time + 30)
    }
    None => assert_true(false)
  }
  
  // Test usage trend
  let memory_trend = get_usage_trend(monitor4, "memory")
  assert_eq(memory_trend, "increasing")  // Usage went from 512 to 700
  
  let disk_trend = get_usage_trend(monitor4, "disk")
  assert_eq(disk_trend, "stable")  // Only one data point
}

// Test 7: Resource Quota Management
test "resource quota management" {
  type ResourceQuota = {
    resource_type: String,
    limit: Int,
    used: Int
  }
  
  type QuotaManager = {
    quotas: Array[ResourceQuota],
    lock_acquired: Bool
  }
  
  let create_resource_quota = fn(resource_type: String, limit: Int) {
    {
      resource_type,
      limit,
      used: 0
    }
  }
  
  let create_quota_manager = fn() {
    {
      quotas: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(manager: QuotaManager) {
    if not(manager.lock_acquired) {
      { manager | lock_acquired: true }
    } else {
      manager  // Lock already acquired
    }
  }
  
  let release_lock = fn(manager: QuotaManager) {
    { manager | lock_acquired: false }
  }
  
  let add_quota = fn(manager: QuotaManager, quota: ResourceQuota) {
    if manager.lock_acquired {
      let updated = manager.quotas.map(fn(q) {
        if q.resource_type == quota.resource_type {
          quota  // Replace existing quota
        } else {
          q
        }
      })
      
      let exists = manager.quotas.some(fn(q) { q.resource_type == quota.resource_type })
      
      if exists {
        {
          quotas: updated,
          lock_acquired: true
        }
      } else {
        {
          quotas: manager.quotas + [quota],
          lock_acquired: true
        }
      }
    } else {
      manager  // Cannot add without lock
    }
  }
  
  let check_quota = fn(manager: QuotaManager, resource_type: String, amount: Int) {
    if manager.lock_acquired {
      let mut allowed = false
      
      for quota in manager.quotas {
        if quota.resource_type == resource_type && (quota.used + amount) <= quota.limit {
          allowed = true
        }
      }
      
      allowed
    } else {
      false  // Cannot check without lock
    }
  }
  
  let consume_quota = fn(manager: QuotaManager, resource_type: String, amount: Int) {
    if manager.lock_acquired && check_quota(manager, resource_type, amount) {
      let updated = manager.quotas.map(fn(quota) {
        if quota.resource_type == resource_type {
          { quota | used: quota.used + amount }
        } else {
          quota
        }
      })
      
      {
        quotas: updated,
        lock_acquired: true
      }
    } else {
      manager  // Cannot consume without lock or quota exceeded
    }
  }
  
  let release_quota = fn(manager: QuotaManager, resource_type: String, amount: Int) {
    if manager.lock_acquired {
      let updated = manager.quotas.map(fn(quota) {
        if quota.resource_type == resource_type {
          { quota | used: (quota.used - amount).max(0) }
        } else {
          quota
        }
      })
      
      {
        quotas: updated,
        lock_acquired: true
      }
    } else {
      manager  // Cannot release without lock
    }
  }
  
  let safe_add_quota = fn(manager: QuotaManager, quota: ResourceQuota) {
    let locked_manager = acquire_lock(manager)
    let result = add_quota(locked_manager, quota)
    release_lock(result)
  }
  
  let safe_consume_quota = fn(manager: QuotaManager, resource_type: String, amount: Int) {
    let locked_manager = acquire_lock(manager)
    let result = consume_quota(locked_manager, resource_type, amount)
    release_lock(result)
  }
  
  let safe_release_quota = fn(manager: QuotaManager, resource_type: String, amount: Int) {
    let locked_manager = acquire_lock(manager)
    let result = release_quota(locked_manager, resource_type, amount)
    release_lock(result)
  }
  
  let manager = create_quota_manager()
  assert_eq(manager.quotas.length(), 0)
  
  // Add quotas
  let memory_quota = create_resource_quota("memory", 1024)
  let disk_quota = create_resource_quota("disk", 4096)
  
  let manager1 = safe_add_quota(manager, memory_quota)
  assert_eq(manager1.quotas.length(), 1)
  
  let manager2 = safe_add_quota(manager1, disk_quota)
  assert_eq(manager2.quotas.length(), 2)
  
  // Consume quotas
  let manager3 = safe_consume_quota(manager2, "memory", 512)
  let memory_quota_updated = manager3.quotas.filter(fn(q) { q.resource_type == "memory" })[0]
  assert_eq(memory_quota_updated.used, 512)
  
  let manager4 = safe_consume_quota(manager3, "disk", 2048)
  let disk_quota_updated = manager4.quotas.filter(fn(q) { q.resource_type == "disk" })[0]
  assert_eq(disk_quota_updated.used, 2048)
  
  // Test quota exceeded
  let manager5 = safe_consume_quota(manager4, "memory", 1024)
  let memory_quota_updated2 = manager5.quotas.filter(fn(q) { q.resource_type == "memory" })[0]
  assert_eq(memory_quota_updated2.used, 512)  // No change
  
  // Release quotas
  let manager6 = safe_release_quota(manager5, "memory", 256)
  let memory_quota_updated3 = manager6.quotas.filter(fn(q) { q.resource_type == "memory" })[0]
  assert_eq(memory_quota_updated3.used, 256)
  
  // Can consume again after release
  let manager7 = safe_consume_quota(manager6, "memory", 500)
  let memory_quota_updated4 = manager7.quotas.filter(fn(q) { q.resource_type == "memory" })[0]
  assert_eq(memory_quota_updated4.used, 756)
}

// Test 8: Resource Cleanup Strategies
test "resource cleanup strategies" {
  type CleanupStrategy = {
    name: String,
    priority: Int,  // Lower number = higher priority
    condition: String,  // Description of when to apply
    action: String  // Description of what to do
  }
  
  type Resource = {
    id: String,
    type: String,
    size: Int,
    last_accessed: Int,
    cleanup_priority: Int
  }
  
  type CleanupManager = {
    resources: Array[Resource],
    strategies: Array[CleanupStrategy],
    lock_acquired: Bool
  }
  
  let create_resource = fn(id: String, type: String, size: Int, last_accessed: Int, cleanup_priority: Int) {
    {
      id,
      type,
      size,
      last_accessed,
      cleanup_priority
    }
  }
  
  let create_cleanup_strategy = fn(name: String, priority: Int, condition: String, action: String) {
    {
      name,
      priority,
      condition,
      action
    }
  }
  
  let create_cleanup_manager = fn() {
    {
      resources: [],
      strategies: [],
      lock_acquired: false
    }
  }
  
  let acquire_lock = fn(manager: CleanupManager) {
    if not(manager.lock_acquired) {
      { manager | lock_acquired: true }
    } else {
      manager  // Lock already acquired
    }
  }
  
  let release_lock = fn(manager: CleanupManager) {
    { manager | lock_acquired: false }
  }
  
  let add_resource = fn(manager: CleanupManager, resource: Resource) {
    if manager.lock_acquired {
      {
        resources: manager.resources + [resource],
        strategies: manager.strategies,
        lock_acquired: true
      }
    } else {
      manager  // Cannot add without lock
    }
  }
  
  let add_strategy = fn(manager: CleanupManager, strategy: CleanupStrategy) {
    if manager.lock_acquired {
      {
        resources: manager.resources,
        strategies: manager.strategies + [strategy],
        lock_acquired: true
      }
    } else {
      manager  // Cannot add without lock
    }
  }
  
  let apply_cleanup_strategy = fn(manager: CleanupManager, strategy_name: String, current_time: Int) {
    if manager.lock_acquired {
      let strategy = manager.strategies.find(fn(s) { s.name == strategy_name })
      
      match strategy {
        Some(s) => {
          let mut cleaned = []
          
          // Apply different cleanup strategies based on strategy name
          if s.name == "lru" {  // Least Recently Used
            let sorted = manager.resources.sort(fn(a, b) { a.last_accessed <= b.last_accessed })
            // Remove the oldest 25% of resources
            let remove_count = (sorted.length() / 4).max(1)
            cleaned = sorted.slice(remove_count, sorted.length())
          } else if s.name == "largest_first" {  // Clean up largest resources first
            let sorted = manager.resources.sort(fn(a, b) { a.size >= b.size })
            // Remove the largest 25% of resources
            let remove_count = (sorted.length() / 4).max(1)
            cleaned = sorted.slice(remove_count, sorted.length())
          } else if s.name == "high_priority_first" {  // Clean up high priority resources first
            let sorted = manager.resources.sort(fn(a, b) { a.cleanup_priority >= b.cleanup_priority })
            // Remove the highest priority 25% of resources
            let remove_count = (sorted.length() / 4).max(1)
            cleaned = sorted.slice(remove_count, sorted.length())
          }
          
          {
            resources: cleaned,
            strategies: manager.strategies,
            lock_acquired: true
          }
        }
        None => manager  // Strategy not found
      }
    } else {
      manager  // Cannot apply without lock
    }
  }
  
  let safe_add_resource = fn(manager: CleanupManager, resource: Resource) {
    let locked_manager = acquire_lock(manager)
    let result = add_resource(locked_manager, resource)
    release_lock(result)
  }
  
  let safe_add_strategy = fn(manager: CleanupManager, strategy: CleanupStrategy) {
    let locked_manager = acquire_lock(manager)
    let result = add_strategy(locked_manager, strategy)
    release_lock(result)
  }
  
  let safe_apply_cleanup_strategy = fn(manager: CleanupManager, strategy_name: String, current_time: Int) {
    let locked_manager = acquire_lock(manager)
    let result = apply_cleanup_strategy(locked_manager, strategy_name, current_time)
    release_lock(result)
  }
  
  let manager = create_cleanup_manager()
  assert_eq(manager.resources.length(), 0)
  assert_eq(manager.strategies.length(), 0)
  
  let base_time = 1640995200
  
  // Add resources
  let resource1 = create_resource("res1", "memory", 512, base_time, 2)
  let resource2 = create_resource("res2", "memory", 256, base_time + 100, 1)
  let resource3 = create_resource("res3", "disk", 1024, base_time + 200, 3)
  let resource4 = create_resource("res4", "memory", 128, base_time + 300, 1)
  
  let manager1 = safe_add_resource(manager, resource1)
  assert_eq(manager1.resources.length(), 1)
  
  let manager2 = safe_add_resource(manager1, resource2)
  assert_eq(manager2.resources.length(), 2)
  
  let manager3 = safe_add_resource(manager2, resource3)
  assert_eq(manager3.resources.length(), 3)
  
  let manager4 = safe_add_resource(manager3, resource4)
  assert_eq(manager4.resources.length(), 4)
  
  // Add cleanup strategies
  let lru_strategy = create_cleanup_strategy("lru", 1, "When memory is low", "Remove least recently used resources")
  let largest_strategy = create_cleanup_strategy("largest_first", 2, "When disk is full", "Remove largest resources first")
  
  let manager5 = safe_add_strategy(manager4, lru_strategy)
  assert_eq(manager5.strategies.length(), 1)
  
  let manager6 = safe_add_strategy(manager5, largest_strategy)
  assert_eq(manager6.strategies.length(), 2)
  
  // Apply LRU strategy (should remove oldest 25% = 1 resource)
  let manager7 = safe_apply_cleanup_strategy(manager6, "lru", base_time + 500)
  assert_eq(manager7.resources.length(), 3)
  assert_false(manager7.resources.some(fn(r) { r.id == "res1" }))  // res1 should be removed (oldest)
  
  // Apply largest first strategy (should remove largest 25% = 1 resource)
  let manager8 = safe_apply_cleanup_strategy(manager7, "largest_first", base_time + 600)
  assert_eq(manager8.resources.length(), 2)
  assert_false(manager8.resources.some(fn(r) { r.id == "res3" }))  // res3 should be removed (largest)
}