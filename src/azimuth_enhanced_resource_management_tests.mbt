// Azimuth Enhanced Resource Management Tests
// This file contains comprehensive test cases for resource management and optimization

// Test 1: Memory Resource Management
test "memory resource management" {
  // Resource pool for memory management
  type MemoryPool = {
    allocated: Int
    capacity: Int
    free_blocks: Array[Int]  // Sizes of free blocks
  }
  
  fn create_memory_pool(capacity: Int) -> MemoryPool {
    {
      allocated: 0,
      capacity: capacity,
      free_blocks: [capacity]  // Initially one large free block
    }
  }
  
  fn allocate(pool: MemoryPool, size: Int) -> Option[(Int, MemoryPool)] {
    // Find a free block that can accommodate the requested size
    let mut i = 0
    while i < pool.free_blocks.length() {
      if pool.free_blocks[i] >= size {
        // Found a suitable block
        let block_size = pool.free_blocks[i]
        let new_allocated = pool.allocated + size
        
        // Update free blocks
        let mut new_free_blocks = []
        for j in 0..pool.free_blocks.length() {
          if j != i {
            new_free_blocks.push(pool.free_blocks[j])
          }
        }
        
        // Add remaining space if any
        if block_size > size {
          new_free_blocks.push(block_size - size)
        }
        
        return Some((pool.allocated, {
          allocated: new_allocated,
          capacity: pool.capacity,
          free_blocks: new_free_blocks
        }))
      }
      i = i + 1
    }
    
    None // No suitable block found
  }
  
  fn deallocate(pool: MemoryPool, offset: Int, size: Int) -> MemoryPool {
    // Add the freed block back to the free list
    let mut new_free_blocks = pool.free_blocks
    new_free_blocks.push(size)
    
    {
      allocated: pool.allocated - size,
      capacity: pool.capacity,
      free_blocks: new_free_blocks
    }
  }
  
  // Test memory pool operations
  let pool = create_memory_pool(1024)
  assert_eq(pool.capacity, 1024)
  assert_eq(pool.allocated, 0)
  assert_eq(pool.free_blocks.length(), 1)
  assert_eq(pool.free_blocks[0], 1024)
  
  // Allocate memory
  let (offset1, pool1) = allocate(pool, 256)
  match offset1 {
    Some(offset) => assert_eq(offset, 0)
    None => assert_true(false)
  }
  assert_eq(pool1.allocated, 256)
  
  let (offset2, pool2) = allocate(pool1, 512)
  match offset2 {
    Some(offset) => assert_eq(offset, 256)
    None => assert_true(false)
  }
  assert_eq(pool2.allocated, 768)
  
  // Allocate more than available
  let (offset3, pool3) = allocate(pool2, 500)
  match offset3 {
    Some(_) => assert_true(false) // Should fail
    None => assert_true(true) // Expected to fail
  }
  assert_eq(pool3.allocated, 768) // Unchanged
  
  // Deallocate memory
  let pool4 = deallocate(pool2, 256, 512)
  assert_eq(pool4.allocated, 256)
  
  // Try allocating the deallocated space
  let (offset4, pool5) = allocate(pool4, 400)
  match offset4 {
    Some(offset) => assert_eq(offset, 256)
    None => assert_true(false)
  }
  assert_eq(pool5.allocated, 656)
}

// Test 2: File Resource Management
test "file resource management" {
  // File handle management
  type FileHandle = {
    id: Int
    path: String
    mode: String  // "r", "w", "a"
    is_open: Bool
  }
  
  type FileManager = {
    next_handle_id: Int
    open_handles: Map[Int, FileHandle]
    max_open_files: Int
  }
  
  fn create_file_manager(max_open_files: Int) -> FileManager {
    {
      next_handle_id: 1,
      open_handles: {},
      max_open_files: max_open_files
    }
  }
  
  fn open_file(manager: FileManager, path: String, mode: String) -> Option[(Int, FileManager)] {
    if manager.open_handles.length() >= manager.max_open_files {
      None // Too many open files
    } else {
      let handle_id = manager.next_handle_id
      let handle = {
        id: handle_id,
        path: path,
        mode: mode,
        is_open: true
      }
      
      let mut new_open_handles = manager.open_handles
      new_open_handles[handle_id] = handle
      
      Some((handle_id, {
        next_handle_id: handle_id + 1,
        open_handles: new_open_handles,
        max_open_files: manager.max_open_files
      }))
    }
  }
  
  fn close_file(manager: FileManager, handle_id: Int) -> FileManager {
    match manager.open_handles.get(handle_id) {
      Some(handle) => {
        if handle.is_open {
          let mut new_open_handles = manager.open_handles
          new_open_handles[handle_id] = {
            id: handle.id,
            path: handle.path,
            mode: handle.mode,
            is_open: false
          }
          
          // Remove from open handles (simplified - would keep track of closed handles)
          let mut final_open_handles = {}
          for (id, h) in new_open_handles {
            if h.is_open {
              final_open_handles[id] = h
            }
          }
          
          {
            next_handle_id: manager.next_handle_id,
            open_handles: final_open_handles,
            max_open_files: manager.max_open_files
          }
        } else {
          manager // Already closed
        }
      }
      None => manager // Handle not found
    }
  }
  
  // Test file manager operations
  let manager = create_file_manager(3)
  assert_eq(manager.max_open_files, 3)
  assert_eq(manager.open_handles.length(), 0)
  
  // Open files
  let (handle1, manager1) = open_file(manager, "/tmp/file1.txt", "r")
  match handle1 {
    Some(id) => assert_eq(id, 1)
    None => assert_true(false)
  }
  assert_eq(manager1.open_handles.length(), 1)
  
  let (handle2, manager2) = open_file(manager1, "/tmp/file2.txt", "w")
  match handle2 {
    Some(id) => assert_eq(id, 2)
    None => assert_true(false)
  }
  assert_eq(manager2.open_handles.length(), 2)
  
  let (handle3, manager3) = open_file(manager2, "/tmp/file3.txt", "a")
  match handle3 {
    Some(id) => assert_eq(id, 3)
    None => assert_true(false)
  }
  assert_eq(manager3.open_handles.length(), 3)
  
  // Try to open more files than allowed
  let (handle4, manager4) = open_file(manager3, "/tmp/file4.txt", "r")
  match handle4 {
    Some(_) => assert_true(false) // Should fail
    None => assert_true(true) // Expected to fail
  }
  assert_eq(manager4.open_handles.length(), 3) // Unchanged
  
  // Close a file
  let manager5 = close_file(manager3, 2)
  assert_eq(manager5.open_handles.length(), 2)
  
  // Now we can open another file
  let (handle5, manager6) = open_file(manager5, "/tmp/file4.txt", "r")
  match handle5 {
    Some(id) => assert_eq(id, 4)
    None => assert_true(false)
  }
  assert_eq(manager6.open_handles.length(), 3)
}

// Test 3: Network Connection Pool Management
test "network connection pool management" {
  // Network connection representation
  type Connection = {
    id: Int
    host: String
    port: Int
    in_use: Bool
    last_used: Int  // Timestamp
  }
  
  type ConnectionPool = {
    connections: Array[Connection]
    max_connections: Int
    next_id: Int
    current_time: Int
  }
  
  fn create_connection_pool(max_connections: Int) -> ConnectionPool {
    {
      connections: [],
      max_connections: max_connections,
      next_id: 1,
      current_time: 0
    }
  }
  
  fn get_connection(pool: ConnectionPool, host: String, port: Int) -> Option[(Connection, ConnectionPool)] {
    // Look for existing connection to the same host/port
    let mut i = 0
    while i < pool.connections.length() {
      let conn = pool.connections[i]
      if conn.host == host && conn.port == port && not conn.in_use {
        // Found an existing connection
        let updated_conn = {
          id: conn.id,
          host: conn.host,
          port: conn.port,
          in_use: true,
          last_used: pool.current_time
        }
        
        let mut new_connections = []
        for j in 0..pool.connections.length() {
          if j != i {
            new_connections.push(pool.connections[j])
          } else {
            new_connections.push(updated_conn)
          }
        }
        
        return Some((updated_conn, {
          connections: new_connections,
          max_connections: pool.max_connections,
          next_id: pool.next_id,
          current_time: pool.current_time
        }))
      }
      i = i + 1
    }
    
    // No existing connection, create a new one if possible
    if pool.connections.length() < pool.max_connections {
      let new_conn = {
        id: pool.next_id,
        host: host,
        port: port,
        in_use: true,
        last_used: pool.current_time
      }
      
      let mut new_connections = pool.connections
      new_connections.push(new_conn)
      
      Some((new_conn, {
        connections: new_connections,
        max_connections: pool.max_connections,
        next_id: pool.next_id + 1,
        current_time: pool.current_time
      }))
    } else {
      None // Pool is full
    }
  }
  
  fn release_connection(pool: ConnectionPool, conn_id: Int) -> ConnectionPool {
    let mut new_connections = []
    for conn in pool.connections {
      if conn.id == conn_id {
        new_connections.push({
          id: conn.id,
          host: conn.host,
          port: conn.port,
          in_use: false,
          last_used: pool.current_time
        })
      } else {
        new_connections.push(conn)
      }
    }
    
    {
      connections: new_connections,
      max_connections: pool.max_connections,
      next_id: pool.next_id,
      current_time: pool.current_time
    }
  }
  
  // Test connection pool operations
  let pool = create_connection_pool(2)
  assert_eq(pool.max_connections, 2)
  assert_eq(pool.connections.length(), 0)
  
  // Get connections
  let (conn1, pool1) = get_connection(pool, "example.com", 80)
  match conn1 {
    Some(conn) => {
      assert_eq(conn.id, 1)
      assert_eq(conn.host, "example.com")
      assert_eq(conn.port, 80)
      assert_true(conn.in_use)
    }
    None => assert_true(false)
  }
  assert_eq(pool1.connections.length(), 1)
  
  let (conn2, pool2) = get_connection(pool1, "example.org", 443)
  match conn2 {
    Some(conn) => {
      assert_eq(conn.id, 2)
      assert_eq(conn.host, "example.org")
      assert_eq(conn.port, 443)
      assert_true(conn.in_use)
    }
    None => assert_true(false)
  }
  assert_eq(pool2.connections.length(), 2)
  
  // Try to get another connection (pool is full)
  let (conn3, pool3) = get_connection(pool2, "example.net", 80)
  match conn3 {
    Some(_) => assert_true(false) // Should fail
    None => assert_true(true) // Expected to fail
  }
  assert_eq(pool3.connections.length(), 2) // Unchanged
  
  // Release a connection
  let pool4 = release_connection(pool2, 1)
  match pool4.connections[0] {
    conn => assert_false(conn.in_use)
  }
  
  // Now we can reuse the released connection
  let (conn4, pool5) = get_connection(pool4, "example.com", 80)
  match conn4 {
    Some(conn) => {
      assert_eq(conn.id, 1) // Reused connection
      assert_eq(conn.host, "example.com")
      assert_eq(conn.port, 80)
      assert_true(conn.in_use)
    }
    None => assert_true(false)
  }
}

// Test 4: Resource Cleanup and Finalization
test "resource cleanup and finalization" {
  // Resource with cleanup callback
  type Resource[T] = {
    data: T
    cleanup: T -> Unit
    cleaned: Bool
  }
  
  fn create_resource[T](data: T, cleanup: T -> Unit) -> Resource[T] {
    {
      data: data,
      cleanup: cleanup,
      cleaned: false
    }
  }
  
  fn cleanup_resource[T](resource: Resource[T]) -> Resource[T] {
    if not resource.cleaned {
      resource.cleanup(resource.data)
      {
        data: resource.data,
        cleanup: resource.cleanup,
        cleaned: true
      }
    } else {
      resource // Already cleaned
    }
  }
  
  // Test resource cleanup
  let mut cleanup_called = false
  
  fn test_cleanup(data: String) {
    if data == "test_data" {
      cleanup_called = true
    }
  }
  
  let resource = create_resource("test_data", test_cleanup)
  assert_false(resource.cleaned)
  assert_false(cleanup_called)
  
  let cleaned_resource = cleanup_resource(resource)
  assert_true(cleaned_resource.cleaned)
  assert_true(cleanup_called)
  
  // Test double cleanup (should not call cleanup twice)
  cleanup_called = false
  let double_cleaned = cleanup_resource(cleaned_resource)
  assert_true(double_cleaned.cleaned)
  assert_false(cleanup_called) // Should not be called again
}

// Test 5: Resource Leaks Detection
test "resource leaks detection" {
  // Resource tracker for leak detection
  type ResourceTracker = {
    allocated_resources: Map[String, Int]
    leaked_resources: Map[String, Int]
  }
  
  fn create_tracker() -> ResourceTracker {
    {
      allocated_resources: {},
      leaked_resources: {}
    }
  }
  
  fn allocate_resource(tracker: ResourceTracker, resource_type: String, resource_id: Int) -> ResourceTracker {
    let mut new_allocated = tracker.allocated_resources
    new_allocated[resource_type + ":" + resource_id.to_string()] = resource_id
    
    {
      allocated_resources: new_allocated,
      leaked_resources: tracker.leaked_resources
    }
  }
  
  fn deallocate_resource(tracker: ResourceTracker, resource_type: String, resource_id: Int) -> ResourceTracker {
    let key = resource_type + ":" + resource_id.to_string()
    let mut new_allocated = {}
    
    // Remove from allocated resources
    for (k, v) in tracker.allocated_resources {
      if k != key {
        new_allocated[k] = v
      }
    }
    
    {
      allocated_resources: new_allocated,
      leaked_resources: tracker.leaked_resources
    }
  }
  
  fn detect_leaks(tracker: ResourceTracker) -> ResourceTracker {
    // Any remaining allocated resources are considered leaked
    {
      allocated_resources: {},
      leaked_resources: tracker.allocated_resources
    }
  }
  
  // Test leak detection
  let tracker = create_tracker()
  assert_eq(tracker.allocated_resources.length(), 0)
  assert_eq(tracker.leaked_resources.length(), 0)
  
  // Allocate resources
  let tracker1 = allocate_resource(tracker, "file", 1)
  let tracker2 = allocate_resource(tracker1, "connection", 1)
  let tracker3 = allocate_resource(tracker2, "memory", 1)
  
  assert_eq(tracker3.allocated_resources.length(), 3)
  
  // Deallocate some resources
  let tracker4 = deallocate_resource(tracker3, "file", 1)
  assert_eq(tracker4.allocated_resources.length(), 2)
  
  // Detect leaks
  let tracker_with_leaks = detect_leaks(tracker4)
  assert_eq(tracker_with_leaks.leaked_resources.length(), 2)
  assert_true(tracker_with_leaks.leaked_resources.contains("connection:1"))
  assert_true(tracker_with_leaks.leaked_resources.contains("memory:1"))
}

// Test 6: Resource Usage Monitoring
test "resource usage monitoring" {
  // Resource usage metrics
  type ResourceMetrics = {
    cpu_usage: Float
    memory_usage: Int
    disk_usage: Int
    network_usage: Int
    timestamp: Int
  }
  
  type ResourceMonitor = {
    metrics_history: Array[ResourceMetrics]
    max_history_size: Int
  }
  
  fn create_monitor(max_history_size: Int) -> ResourceMonitor {
    {
      metrics_history: [],
      max_history_size: max_history_size
    }
  }
  
  fn record_metrics(monitor: ResourceMonitor, metrics: ResourceMetrics) -> ResourceMonitor {
    let mut new_history = monitor.metrics_history
    new_history.push(metrics)
    
    // Trim history if it exceeds max size
    if new_history.length() > monitor.max_history_size {
      let mut trimmed_history = []
      for i in 1..new_history.length() {
        trimmed_history.push(new_history[i])
      }
      new_history = trimmed_history
    }
    
    {
      metrics_history: new_history,
      max_history_size: monitor.max_history_size
    }
  }
  
  fn get_average_cpu_usage(monitor: ResourceMonitor) -> Float {
    if monitor.metrics_history.length() == 0 {
      0.0
    } else {
      let mut total = 0.0
      for metrics in monitor.metrics_history {
        total = total + metrics.cpu_usage
      }
      total / monitor.metrics_history.length().to_float()
    }
  }
  
  fn get_peak_memory_usage(monitor: ResourceMonitor) -> Int {
    if monitor.metrics_history.length() == 0 {
      0
    } else {
      let mut peak = 0
      for metrics in monitor.metrics_history {
        if metrics.memory_usage > peak {
          peak = metrics.memory_usage
        }
      }
      peak
    }
  }
  
  // Test resource monitoring
  let monitor = create_monitor(5)
  assert_eq(monitor.max_history_size, 5)
  assert_eq(monitor.metrics_history.length(), 0)
  
  // Record metrics
  let metrics1 = {
    cpu_usage: 25.5,
    memory_usage: 1024,
    disk_usage: 2048,
    network_usage: 512,
    timestamp: 1000
  }
  
  let metrics2 = {
    cpu_usage: 50.0,
    memory_usage: 1536,
    disk_usage: 2560,
    network_usage: 768,
    timestamp: 2000
  }
  
  let metrics3 = {
    cpu_usage: 75.5,
    memory_usage: 2048,
    disk_usage: 3072,
    network_usage: 1024,
    timestamp: 3000
  }
  
  let monitor1 = record_metrics(monitor, metrics1)
  let monitor2 = record_metrics(monitor1, metrics2)
  let monitor3 = record_metrics(monitor2, metrics3)
  
  assert_eq(monitor3.metrics_history.length(), 3)
  
  // Test metrics calculations
  let avg_cpu = get_average_cpu_usage(monitor3)
  assert_eq(avg_cpu, 50.333333333333336) // (25.5 + 50.0 + 75.5) / 3
  
  let peak_memory = get_peak_memory_usage(monitor3)
  assert_eq(peak_memory, 2048)
  
  // Test history trimming
  let metrics4 = {
    cpu_usage: 30.0,
    memory_usage: 1280,
    disk_usage: 2304,
    network_usage: 640,
    timestamp: 4000
  }
  
  let metrics5 = {
    cpu_usage: 60.0,
    memory_usage: 1792,
    disk_usage: 2816,
    network_usage: 896,
    timestamp: 5000
  }
  
  let metrics6 = {
    cpu_usage: 90.0,
    memory_usage: 2304,
    disk_usage: 3328,
    network_usage: 1152,
    timestamp: 6000
  }
  
  let monitor4 = record_metrics(monitor3, metrics4)
  let monitor5 = record_metrics(monitor4, metrics5)
  let monitor6 = record_metrics(monitor5, metrics6)
  
  assert_eq(monitor6.metrics_history.length(), 5) // Should be trimmed to max size
  assert_eq(monitor6.metrics_history[0].timestamp, 2000) // First entry should be removed
  assert_eq(monitor6.metrics_history[4].timestamp, 6000) // Last entry should be the newest
}

// Test 7: Resource Quota Management
test "resource quota management" {
  // Resource quota definition
  type ResourceQuota = {
    max_memory: Int
    max_cpu_time: Int
    max_disk_space: Int
    max_network_bandwidth: Int
  }
  
  type ResourceUsage = {
    memory_used: Int
    cpu_time_used: Int
    disk_space_used: Int
    network_bandwidth_used: Int
  }
  
  type QuotaManager = {
    quota: ResourceQuota
    usage: ResourceUsage
  }
  
  fn create_quota_manager(quota: ResourceQuota) -> QuotaManager {
    {
      quota: quota,
      usage: {
        memory_used: 0,
        cpu_time_used: 0,
        disk_space_used: 0,
        network_bandwidth_used: 0
      }
    }
  }
  
  fn check_memory_quota(manager: QuotaManager, request: Int) -> Bool {
    manager.usage.memory_used + request <= manager.quota.max_memory
  }
  
  fn check_cpu_quota(manager: QuotaManager, request: Int) -> Bool {
    manager.usage.cpu_time_used + request <= manager.quota.max_cpu_time
  }
  
  fn check_disk_quota(manager: QuotaManager, request: Int) -> Bool {
    manager.usage.disk_space_used + request <= manager.quota.max_disk_space
  }
  
  fn check_network_quota(manager: QuotaManager, request: Int) -> Bool {
    manager.usage.network_bandwidth_used + request <= manager.quota.max_network_bandwidth
  }
  
  fn allocate_memory(manager: QuotaManager, amount: Int) -> Option[QuotaManager] {
    if check_memory_quota(manager, amount) {
      Some({
        quota: manager.quota,
        usage: {
          memory_used: manager.usage.memory_used + amount,
          cpu_time_used: manager.usage.cpu_time_used,
          disk_space_used: manager.usage.disk_space_used,
          network_bandwidth_used: manager.usage.network_bandwidth_used
        }
      })
    } else {
      None
    }
  }
  
  fn release_memory(manager: QuotaManager, amount: Int) -> QuotaManager {
    {
      quota: manager.quota,
      usage: {
        memory_used: max(0, manager.usage.memory_used - amount),
        cpu_time_used: manager.usage.cpu_time_used,
        disk_space_used: manager.usage.disk_space_used,
        network_bandwidth_used: manager.usage.network_bandwidth_used
      }
    }
  }
  
  // Test quota management
  let quota = {
    max_memory: 1024,
    max_cpu_time: 100,
    max_disk_space: 2048,
    max_network_bandwidth: 512
  }
  
  let manager = create_quota_manager(quota)
  assert_eq(manager.usage.memory_used, 0)
  assert_eq(manager.usage.cpu_time_used, 0)
  assert_eq(manager.usage.disk_space_used, 0)
  assert_eq(manager.usage.network_bandwidth_used, 0)
  
  // Test quota checks
  assert_true(check_memory_quota(manager, 512))
  assert_true(check_memory_quota(manager, 1024))
  assert_false(check_memory_quota(manager, 1025))
  
  // Test resource allocation
  let manager1_opt = allocate_memory(manager, 512)
  match manager1_opt {
    Some(manager1) => {
      assert_eq(manager1.usage.memory_used, 512)
      
      // Try to allocate more
      let manager2_opt = allocate_memory(manager1, 256)
      match manager2_opt {
        Some(manager2) => {
          assert_eq(manager2.usage.memory_used, 768)
          
          // Try to allocate beyond quota
          let manager3_opt = allocate_memory(manager2, 300)
          match manager3_opt {
            Some(_) => assert_true(false) // Should fail
            None => assert_true(true) // Expected to fail
          }
          
          // Release some memory
          let manager4 = release_memory(manager2, 100)
          assert_eq(manager4.usage.memory_used, 668)
          
          // Now allocation should succeed
          let manager5_opt = allocate_memory(manager4, 300)
          match manager5_opt {
            Some(manager5) => {
              assert_eq(manager5.usage.memory_used, 968)
            }
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 8: Resource Prioritization
test "resource prioritization" {
  // Resource request with priority
  type ResourceRequest = {
    id: Int
    priority: Int  // Higher number = higher priority
    amount: Int
    description: String
  }
  
  type ResourceScheduler = {
    available_resources: Int
    pending_requests: Array[ResourceRequest]
    allocated_requests: Map[Int, Int]  // Request ID -> allocated amount
  }
  
  fn create_scheduler(available_resources: Int) -> ResourceScheduler {
    {
      available_resources: available_resources,
      pending_requests: [],
      allocated_requests: {}
    }
  }
  
  fn submit_request(scheduler: ResourceScheduler, request: ResourceRequest) -> ResourceScheduler {
    let mut new_requests = scheduler.pending_requests
    new_requests.push(request)
    
    // Sort by priority (higher priority first)
    // Simplified - in real implementation would use proper sorting
    let mut sorted_requests = []
    let mut remaining_requests = new_requests
    
    while remaining_requests.length() > 0 {
      let mut highest_priority_index = 0
      let mut highest_priority = remaining_requests[0].priority
      
      for i in 1..remaining_requests.length() {
        if remaining_requests[i].priority > highest_priority {
          highest_priority = remaining_requests[i].priority
          highest_priority_index = i
        }
      }
      
      sorted_requests.push(remaining_requests[highest_priority_index])
      
      let mut temp_requests = []
      for i in 0..remaining_requests.length() {
        if i != highest_priority_index {
          temp_requests.push(remaining_requests[i])
        }
      }
      remaining_requests = temp_requests
    }
    
    {
      available_resources: scheduler.available_resources,
      pending_requests: sorted_requests,
      allocated_requests: scheduler.allocated_requests
    }
  }
  
  fn process_requests(scheduler: ResourceScheduler) -> ResourceScheduler {
    let mut available = scheduler.available_resources
    let mut allocated = scheduler.allocated_requests
    let mut remaining_requests = []
    
    for request in scheduler.pending_requests {
      if request.amount <= available {
        // Can allocate
        available = available - request.amount
        allocated[request.id] = request.amount
      } else {
        // Not enough resources, keep in pending
        remaining_requests.push(request)
      }
    }
    
    {
      available_resources: available,
      pending_requests: remaining_requests,
      allocated_requests: allocated
    }
  }
  
  // Test resource prioritization
  let scheduler = create_scheduler(100)
  assert_eq(scheduler.available_resources, 100)
  assert_eq(scheduler.pending_requests.length(), 0)
  
  // Submit requests with different priorities
  let request1 = {
    id: 1,
    priority: 1,
    amount: 30,
    description: "Low priority task"
  }
  
  let request2 = {
    id: 2,
    priority: 3,
    amount: 50,
    description: "High priority task"
  }
  
  let request3 = {
    id: 3,
    priority: 2,
    amount: 40,
    description: "Medium priority task"
  }
  
  let scheduler1 = submit_request(scheduler, request1)
  let scheduler2 = submit_request(scheduler1, request2)
  let scheduler3 = submit_request(scheduler2, request3)
  
  // Process requests (should prioritize high priority)
  let scheduler4 = process_requests(scheduler3)
  
  // High priority request (50) should be allocated first
  match scheduler4.allocated_requests.get(2) {
    Some(amount) => assert_eq(amount, 50)
    None => assert_true(false)
  }
  
  // Medium priority request (40) should be allocated next
  match scheduler4.allocated_requests.get(3) {
    Some(amount) => assert_eq(amount, 40)
    None => assert_true(false)
  }
  
  // Low priority request should not be allocated (not enough resources left)
  match scheduler4.allocated_requests.get(1) {
    Some(_) => assert_true(false) // Should not be allocated
    None => assert_true(true) // Expected
  }
  
  // Should have 10 resources left (100 - 50 - 40)
  assert_eq(scheduler4.available_resources, 10)
  
  // One request should remain pending
  assert_eq(scheduler4.pending_requests.length(), 1)
  assert_eq(scheduler4.pending_requests[0].id, 1) // Low priority request
}