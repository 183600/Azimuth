// Azimuth Resource Management Test Suite
// This file contains high-quality test cases for resource management

// Test 1: Memory Pool Management
test "memory pool management" {
  // Define memory block
  type MemoryBlock = {
    id: String,
    size: Int,
    in_use: Bool,
    data: Array[Int]
  }
  
  // Define memory pool
  type MemoryPool = {
    blocks: Array[MemoryBlock>,
    total_size: Int,
    used_size: Int,
    free_blocks: Int,
    next_id: Int
  }
  
  // Create memory pool
  let create_memory_pool = fn(initial_blocks: Int, block_size: Int) {
    let mut blocks = []
    for i in 0..initial_blocks {
      blocks = blocks.push({
        id: "block-" + i.to_string(),
        size: block_size,
        in_use: false,
        data: [0; block_size]
      })
    }
    
    {
      blocks: blocks,
      total_size: initial_blocks * block_size,
      used_size: 0,
      free_blocks: initial_blocks,
      next_id: initial_blocks
    }
  }
  
  // Allocate memory block
  let allocate_block = fn(pool: MemoryPool, size: Int) {
    // Find a free block of sufficient size
    let block_index = pool.blocks.find_index(fn(block) {
      not(block.in_use) and block.size >= size
    })
    
    match block_index {
      Some(index) => {
        let block = pool.blocks[index]
        let updated_block = { block | in_use: true }
        let updated_blocks = pool.blocks.update(index, updated_block)
        
        ({
          blocks: updated_blocks,
          total_size: pool.total_size,
          used_size: pool.used_size + block.size,
          free_blocks: pool.free_blocks - 1,
          next_id: pool.next_id
        }, Some(updated_block))
      }
      None => {
        // No suitable block found
        (pool, None)
      }
    }
  }
  
  // Free memory block
  let free_block = fn(pool: MemoryPool, block_id: String) {
    let block_index = pool.blocks.find_index(fn(block) { block.id == block_id })
    
    match block_index {
      Some(index) => {
        let block = pool.blocks[index]
        
        if block.in_use {
          let updated_block = { block | in_use: false }
          let updated_blocks = pool.blocks.update(index, updated_block)
          
          {
            blocks: updated_blocks,
            total_size: pool.total_size,
            used_size: pool.used_size - block.size,
            free_blocks: pool.free_blocks + 1,
            next_id: pool.next_id
          }
        } else {
          // Block already free
          pool
        }
      }
      None => {
        // Block not found
        pool
      }
    }
  }
  
  // Expand memory pool
  let expand_pool = fn(pool: MemoryPool, additional_blocks: Int, block_size: Int) {
    let mut new_blocks = []
    let start_id = pool.next_id
    
    for i in 0..additional_blocks {
      new_blocks = new_blocks.push({
        id: "block-" + (start_id + i).to_string(),
        size: block_size,
        in_use: false,
        data: [0; block_size]
      })
    }
    
    {
      blocks: pool.blocks + new_blocks,
      total_size: pool.total_size + additional_blocks * block_size,
      used_size: pool.used_size,
      free_blocks: pool.free_blocks + additional_blocks,
      next_id: start_id + additional_blocks
    }
  }
  
  // Test memory pool management
  let pool = create_memory_pool(5, 1024)
  
  assert_eq(pool.blocks.length(), 5)
  assert_eq(pool.total_size, 5120)
  assert_eq(pool.used_size, 0)
  assert_eq(pool.free_blocks, 5)
  
  // Allocate blocks
  let (pool1, block1) = allocate_block(pool, 512)
  match block1 {
    Some(b) => {
      assert_true(b.in_use)
      assert_eq(b.id, "block-0")
    }
    None => assert_true(false)
  }
  
  assert_eq(pool1.used_size, 1024)
  assert_eq(pool1.free_blocks, 4)
  
  let (pool2, block2) = allocate_block(pool1, 1024)
  let (pool3, block3) = allocate_block(pool2, 256)
  let (pool4, block4) = allocate_block(pool3, 2048)  // Should fail - no block large enough
  
  match block4 {
    Some(_) => assert_true(false)  // Should not succeed
    None => assert_true(true)     // Expected to fail
  }
  
  assert_eq(pool3.used_size, 3072)  // 3 blocks * 1024
  assert_eq(pool3.free_blocks, 2)
  
  // Free a block
  let pool5 = free_block(pool3, "block-0")
  assert_eq(pool5.used_size, 2048)
  assert_eq(pool5.free_blocks, 3)
  
  // Reallocate the freed block
  let (pool6, block5) = allocate_block(pool5, 512)
  match block5 {
    Some(b) => assert_eq(b.id, "block-0")  // Should reuse the freed block
    None => assert_true(false)
  }
  
  // Expand the pool
  let pool7 = expand_pool(pool6, 3, 1024)
  assert_eq(pool7.blocks.length(), 8)
  assert_eq(pool7.total_size, 8192)
  assert_eq(pool7.free_blocks, 5)
  
  // Allocate from expanded pool
  let (pool8, block6) = allocate_block(pool7, 2048)
  match block6 {
    Some(_) => assert_true(false)  // Should still fail - blocks are only 1024
    None => assert_true(true)
  }
}

// Test 2: Connection Pool Management
test "connection pool management" {
  // Define connection
  type Connection = {
    id: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    usage_count: Int
  }
  
  // Define connection pool
  type ConnectionPool = {
    connections: Array[Connection],
    max_connections: Int,
    connection_timeout: Int,
    next_id: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(max_connections: Int, connection_timeout: Int) {
    {
      connections: [],
      max_connections: max_connections,
      connection_timeout: connection_timeout,
      next_id: 1
    }
  }
  
  // Acquire connection
  let acquire_connection = fn(pool: ConnectionPool, current_time: Int) {
    // Find an available connection
    let available_index = pool.connections.find_index(fn(conn) { not(conn.in_use) })
    
    match available_index {
      Some(index) => {
        let conn = pool.connections[index]
        let updated_conn = {
          id: conn.id,
          created_at: conn.created_at,
          last_used: current_time,
          in_use: true,
          usage_count: conn.usage_count + 1
        }
        
        let updated_connections = pool.connections.update(index, updated_conn)
        
        ({
          connections: updated_connections,
          max_connections: pool.max_connections,
          connection_timeout: pool.connection_timeout,
          next_id: pool.next_id
        }, Some(updated_conn))
      }
      None => {
        if pool.connections.length() < pool.max_connections {
          // Create new connection
          let new_conn = {
            id: "conn-" + pool.next_id.to_string(),
            created_at: current_time,
            last_used: current_time,
            in_use: true,
            usage_count: 1
          }
          
          let updated_connections = pool.connections.push(new_conn)
          
          ({
            connections: updated_connections,
            max_connections: pool.max_connections,
            connection_timeout: pool.connection_timeout,
            next_id: pool.next_id + 1
          }, Some(new_conn))
        } else {
          // Pool is full
          (pool, None)
        }
      }
    }
  }
  
  // Release connection
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    let conn_index = pool.connections.find_index(fn(conn) { conn.id == connection_id })
    
    match conn_index {
      Some(index) => {
        let conn = pool.connections[index]
        
        if conn.in_use {
          let updated_conn = {
            id: conn.id,
            created_at: conn.created_at,
            last_used: current_time,
            in_use: false,
            usage_count: conn.usage_count
          }
          
          let updated_connections = pool.connections.update(index, updated_conn)
          
          {
            connections: updated_connections,
            max_connections: pool.max_connections,
            connection_timeout: pool.connection_timeout,
            next_id: pool.next_id
          }
        } else {
          // Connection already released
          pool
        }
      }
      None => {
        // Connection not found
        pool
      }
    }
  }
  
  // Clean up idle connections
  let cleanup_idle_connections = fn(pool: ConnectionPool, current_time: Int) {
    let mut updated_connections = []
    let mut removed_count = 0
    
    for i in 0..pool.connections.length() {
      let conn = pool.connections[i]
      
      if not(conn.in_use) and (current_time - conn.last_used) > pool.connection_timeout {
        // Remove idle connection
        removed_count = removed_count + 1
      } else {
        updated_connections = updated_connections.push(conn)
      }
    }
    
    {
      connections: updated_connections,
      max_connections: pool.max_connections,
      connection_timeout: pool.connection_timeout,
      next_id: pool.next_id
    }
  }
  
  // Test connection pool management
  let pool = create_connection_pool(3, 60000)  // 3 max connections, 60s timeout
  
  assert_eq(pool.connections.length(), 0)
  
  // Acquire connections
  let (pool1, conn1) = acquire_connection(pool, 1000)
  match conn1 {
    Some(c) => {
      assert_eq(c.id, "conn-1")
      assert_true(c.in_use)
      assert_eq(c.usage_count, 1)
    }
    None => assert_true(false)
  }
  
  let (pool2, conn2) = acquire_connection(pool1, 1005)
  let (pool3, conn3) = acquire_connection(pool2, 1010)
  
  // Pool should now have 3 connections, all in use
  assert_eq(pool3.connections.length(), 3)
  assert_true(pool3.connections.all(fn(conn) { conn.in_use }))
  
  // Try to acquire when pool is full
  let (pool4, conn4) = acquire_connection(pool3, 1015)
  match conn4 {
    Some(_) => assert_true(false)  // Should not succeed
    None => assert_true(true)     // Expected to fail
  }
  
  // Release a connection
  let pool5 = release_connection(pool3, "conn-1", 1020)
  assert_false(pool5.connections[0].in_use)
  assert_eq(pool5.connections[0].last_used, 1020)
  
  // Acquire again (should reuse released connection)
  let (pool6, conn5) = acquire_connection(pool5, 1030)
  match conn5 {
    Some(c) => {
      assert_eq(c.id, "conn-1")  // Reused connection
      assert_true(c.in_use)
      assert_eq(c.usage_count, 2)  // Used twice
    }
    None => assert_true(false)
  }
  
  // Release all connections
  let pool7 = release_connection(pool6, "conn-1", 2000)
  let pool8 = release_connection(pool7, "conn-2", 2005)
  let pool9 = release_connection(pool8, "conn-3", 2010)
  
  assert_true(pool9.connections.all(fn(conn) { not(conn.in_use) }))
  
  // Clean up idle connections
  let pool10 = cleanup_idle_connections(pool9, 70000)  // 70s from start (past timeout)
  
  // All connections should be removed
  assert_eq(pool10.connections.length(), 0)
}

// Test 3: Resource Leak Detection
test "resource leak detection" {
  // Define resource type
  enum ResourceType {
    Memory
    Connection
    FileHandle
    Thread
  }
  
  // Define resource allocation record
  type ResourceRecord = {
    id: String,
    resource_type: ResourceType,
    allocated_at: Int,
    allocated_by: String,
    size: Int
  }
  
  // Define resource tracker
  type ResourceTracker = {
    allocated_resources: Array[ResourceRecord],
    total_allocated: Int,
    total_freed: Int,
    current_usage: Int,
    max_usage: Int
  }
  
  // Create resource tracker
  let create_resource_tracker = fn() {
    {
      allocated_resources: [],
      total_allocated: 0,
      total_freed: 0,
      current_usage: 0,
      max_usage: 0
    }
  }
  
  // Allocate resource
  let allocate_resource = fn(tracker: ResourceTracker, resource_type: ResourceType, size: Int, allocated_by: String, timestamp: Int) {
    let resource_id = resource_type.to_string() + "-" + tracker.total_allocated.to_string()
    
    let record = {
      id: resource_id,
      resource_type: resource_type,
      allocated_at: timestamp,
      allocated_by: allocated_by,
      size: size
    }
    
    let new_usage = tracker.current_usage + size
    let new_max = if new_usage > tracker.max_usage { new_usage } else { tracker.max_usage }
    
    {
      allocated_resources: tracker.allocated_resources.push(record),
      total_allocated: tracker.total_allocated + 1,
      total_freed: tracker.total_freed,
      current_usage: new_usage,
      max_usage: new_max
    }
  }
  
  // Free resource
  let free_resource = fn(tracker: ResourceTracker, resource_id: String) {
    let resource_index = tracker.allocated_resources.find_index(fn(r) { r.id == resource_id })
    
    match resource_index {
      Some(index) => {
        let resource = tracker.allocated_resources[index]
        let updated_resources = tracker.allocated_resources.filter(fn(r) { r.id != resource_id })
        
        {
          allocated_resources: updated_resources,
          total_allocated: tracker.total_allocated,
          total_freed: tracker.total_freed + 1,
          current_usage: tracker.current_usage - resource.size,
          max_usage: tracker.max_usage
        }
      }
      None => {
        // Resource not found
        tracker
      }
    }
  }
  
  // Detect leaks
  let detect_leaks = fn(tracker: ResourceTracker, current_time: Int, leak_threshold: Int) {
    let mut leaks = []
    
    for i in 0..tracker.allocated_resources.length() {
      let resource = tracker.allocated_resources[i]
      let age = current_time - resource.allocated_at
      
      if age > leak_threshold {
        leaks = leaks.push({
          resource: resource,
          age: age,
          severity: if age > leak_threshold * 2 { "high" } else { "medium" }
        })
      }
    }
    
    leaks
  }
  
  // Get resource statistics
  let get_resource_stats = fn(tracker: ResourceTracker) {
    let mut stats = []
    
    let memory_resources = tracker.allocated_resources.filter(fn(r) { 
      match r.resource_type { Memory => true, _ => false }
    })
    let connection_resources = tracker.allocated_resources.filter(fn(r) { 
      match r.resource_type { Connection => true, _ => false }
    })
    let file_resources = tracker.allocated_resources.filter(fn(r) { 
      match r.resource_type { FileHandle => true, _ => false }
    })
    let thread_resources = tracker.allocated_resources.filter(fn(r) { 
      match r.resource_type { Thread => true, _ => false }
    })
    
    stats = stats.push(("memory", memory_resources.length()))
    stats = stats.push(("connections", connection_resources.length()))
    stats = stats.push(("file_handles", file_resources.length()))
    stats = stats.push(("threads", thread_resources.length()))
    
    stats
  }
  
  // Test resource leak detection
  let tracker = create_resource_tracker()
  
  // Allocate resources
  let tracker1 = allocate_resource(tracker, Memory, 1024, "module-A", 1000)
  let tracker2 = allocate_resource(tracker1, Connection, 512, "module-B", 1005)
  let tracker3 = allocate_resource(tracker2, FileHandle, 256, "module-C", 1010)
  let tracker4 = allocate_resource(tracker3, Thread, 128, "module-D", 1015)
  
  assert_eq(tracker4.total_allocated, 4)
  assert_eq(tracker4.total_freed, 0)
  assert_eq(tracker4.current_usage, 1920)  // 1024 + 512 + 256 + 128
  assert_eq(tracker4.max_usage, 1920)
  
  // Free some resources
  let tracker5 = free_resource(tracker4, "Connection-1")
  let tracker6 = free_resource(tracker5, "Thread-3")
  
  assert_eq(tracker6.total_allocated, 4)
  assert_eq(tracker6.total_freed, 2)
  assert_eq(tracker6.current_usage, 1280)  // 1920 - 512 - 128
  
  // Get resource statistics
  let stats = get_resource_stats(tracker6)
  assert_eq(stats.find(fn(pair) { pair.0 == "memory" }), Some(("memory", 1)))
  assert_eq(stats.find(fn(pair) { pair.0 == "connections" }), Some(("connections", 0)))
  assert_eq(stats.find(fn(pair) { pair.0 == "file_handles" }), Some(("file_handles", 1)))
  assert_eq(stats.find(fn(pair) { pair.0 == "threads" }), Some(("threads", 0)))
  
  // Detect leaks (with threshold of 100 time units)
  let current_time = 1200
  let leaks = detect_leaks(tracker6, current_time, 100)
  
  // All remaining resources should be detected as leaks (older than 100 units)
  assert_eq(leaks.length(), 2)
  
  let memory_leak = leaks.find(fn(leak) { leak.resource.resource_type == Memory })
  match memory_leak {
    Some(leak) => {
      assert_eq(leak.resource.id, "Memory-0")
      assert_eq(leak.age, 200)  // 1200 - 1000
      assert_eq(leak.severity, "high")  // Age > 200 (2 * threshold)
    }
    None => assert_true(false)
  }
  
  let file_leak = leaks.find(fn(leak) { leak.resource.resource_type == FileHandle })
  match file_leak {
    Some(leak) => {
      assert_eq(leak.resource.id, "FileHandle-2")
      assert_eq(leak.age, 190)  // 1200 - 1010
      assert_eq(leak.severity, "high")  // Age > 200 (2 * threshold)
    }
    None => assert_true(false)
  }
}

// Test 4: Resource Quota Management
test "resource quota management" {
  // Define resource quota
  type ResourceQuota = {
    resource_type: String,
    max_amount: Int,
    current_usage: Int,
    allocation_strategy: String
  }
  
  // Define quota manager
  type QuotaManager = {
    quotas: Array[ResourceQuota],
    allocation_history: Array[String],
    denied_requests: Int
  }
  
  // Create quota manager
  let create_quota_manager = fn() {
    {
      quotas: [
        { resource_type: "memory", max_amount: 1048576, current_usage: 0, allocation_strategy: "fair" },
        { resource_type: "connections", max_amount: 100, current_usage: 0, allocation_strategy: "fifo" },
        { resource_type: "cpu_time", max_amount: 3600000, current_usage: 0, allocation_strategy: "priority" },
        { resource_type: "file_handles", max_amount: 256, current_usage: 0, allocation_strategy: "fair" }
      ],
      allocation_history: [],
      denied_requests: 0
    }
  }
  
  // Request resource allocation
  let request_allocation = fn(manager: QuotaManager, resource_type: String, amount: Int, requester: String) {
    let quota_index = manager.quotas.find_index(fn(q) { q.resource_type == resource_type })
    
    match quota_index {
      Some(index) => {
        let quota = manager.quotas[index]
        
        if quota.current_usage + amount <= quota.max_amount {
          // Allocation approved
          let updated_quota = {
            resource_type: quota.resource_type,
            max_amount: quota.max_amount,
            current_usage: quota.current_usage + amount,
            allocation_strategy: quota.allocation_strategy
          }
          
          let updated_quotas = manager.quotas.update(index, updated_quota)
          let history_entry = requester + " allocated " + amount.to_string() + " " + resource_type
          
          ({
            quotas: updated_quotas,
            allocation_history: manager.allocation_history.push(history_entry),
            denied_requests: manager.denied_requests
          }, true)
        } else {
          // Allocation denied
          let history_entry = requester + " denied " + amount.to_string() + " " + resource_type + " (quota exceeded)"
          
          ({
            quotas: manager.quotas,
            allocation_history: manager.allocation_history.push(history_entry),
            denied_requests: manager.denied_requests + 1
          }, false)
        }
      }
      None => {
        // Resource type not found
        (manager, false)
      }
    }
  }
  
  // Release resource allocation
  let release_allocation = fn(manager: QuotaManager, resource_type: String, amount: Int, requester: String) {
    let quota_index = manager.quotas.find_index(fn(q) { q.resource_type == resource_type })
    
    match quota_index {
      Some(index) => {
        let quota = manager.quotas[index]
        
        if quota.current_usage >= amount {
          // Release approved
          let updated_quota = {
            resource_type: quota.resource_type,
            max_amount: quota.max_amount,
            current_usage: quota.current_usage - amount,
            allocation_strategy: quota.allocation_strategy
          }
          
          let updated_quotas = manager.quotas.update(index, updated_quota)
          let history_entry = requester + " released " + amount.to_string() + " " + resource_type
          
          {
            quotas: updated_quotas,
            allocation_history: manager.allocation_history.push(history_entry),
            denied_requests: manager.denied_requests
          }
        } else {
          // Release amount exceeds current usage
          manager
        }
      }
      None => {
        // Resource type not found
        manager
      }
    }
  }
  
  // Get quota utilization
  let get_quota_utilization = fn(manager: QuotaManager, resource_type: String) {
    match manager.quotas.find(fn(q) { q.resource_type == resource_type }) {
      Some(quota) => {
        let utilization = (quota.current_usage as Float / quota.max_amount as Float) * 100.0
        Some(utilization)
      }
      None => None
    }
  }
  
  // Adjust quota
  let adjust_quota = fn(manager: QuotaManager, resource_type: String, new_max: Int) {
    let quota_index = manager.quotas.find_index(fn(q) { q.resource_type == resource_type })
    
    match quota_index {
      Some(index) => {
        let quota = manager.quotas[index]
        let updated_quota = {
          resource_type: quota.resource_type,
          max_amount: new_max,
          current_usage: quota.current_usage,
          allocation_strategy: quota.allocation_strategy
        }
        
        let updated_quotas = manager.quotas.update(index, updated_quota)
        
        {
          quotas: updated_quotas,
          allocation_history: manager.allocation_history,
          denied_requests: manager.denied_requests
        }
      }
      None => {
        manager
      }
    }
  }
  
  // Test resource quota management
  let manager = create_quota_manager()
  
  // Request allocations
  let (manager1, approved1) = request_allocation(manager, "memory", 524288, "service-A")  // 512KB
  assert_true(approved1)
  
  let (manager2, approved2) = request_allocation(manager1, "memory", 524288, "service-B")  // 512KB
  assert_true(approved2)
  
  let (manager3, approved3) = request_allocation(manager2, "memory", 1048576, "service-C")  // 1MB
  assert_false(approved3)  // Should be denied - would exceed quota
  
  let (manager4, approved4) = request_allocation(manager3, "connections", 50, "service-A")
  assert_true(approved4)
  
  let (manager5, approved5) = request_allocation(manager4, "connections", 60, "service-B")
  assert_false(approved5)  // Should be denied - would exceed quota
  
  // Check quota utilization
  let memory_util = get_quota_utilization(manager5, "memory")
  match memory_util {
    Some(util) => assert_eq(util, 100.0)  // 1MB / 1MB = 100%
    None => assert_true(false)
  }
  
  let connections_util = get_quota_utilization(manager5, "connections")
  match connections_util {
    Some(util) => assert_eq(util, 50.0)  // 50 / 100 = 50%
    None => assert_true(false)
  }
  
  // Release some resources
  let manager6 = release_allocation(manager5, "memory", 262144, "service-A")  // 256KB
  
  let memory_util_after = get_quota_utilization(manager6, "memory")
  match memory_util_after {
    Some(util) => assert_eq(util, 75.0)  // 768KB / 1MB = 75%
    None => assert_true(false)
  }
  
  // Adjust quota
  let manager7 = adjust_quota(manager6, "memory", 2097152)  // Increase to 2MB
  
  let memory_quota = manager7.quotas.find(fn(q) { q.resource_type == "memory" })
  match memory_quota {
    Some(quota) => assert_eq(quota.max_amount, 2097152)
    None => assert_true(false)
  }
  
  // Request allocation with increased quota
  let (manager8, approved6) = request_allocation(manager7, "memory", 1048576, "service-C")
  assert_true(approved6)  // Should now be approved
  
  // Check denied requests count
  assert_eq(manager8.denied_requests, 2)
}

// Test 5: Resource Cleanup and Garbage Collection
test "resource cleanup and garbage collection" {
  // Define garbage collectable object
  type GCObject = {
    id: String,
    size: Int,
    references: Int,
    last_accessed: Int,
    marked: Bool
  }
  
  // Define garbage collector
  type GarbageCollector = {
    objects: Array[GCObject],
    collection_threshold: Int,
    total_collected: Int,
    total_freed: Int
  }
  
  // Create garbage collector
  let create_garbage_collector = fn(collection_threshold: Int) {
    {
      objects: [],
      collection_threshold: collection_threshold,
      total_collected: 0,
      total_freed: 0
    }
  }
  
  // Allocate object
  let allocate_object = fn(gc: GarbageCollector, size: Int, timestamp: Int) {
    let object = {
      id: "obj-" + gc.objects.length().to_string(),
      size: size,
      references: 1,
      last_accessed: timestamp,
      marked: false
    }
    
    {
      objects: gc.objects.push(object),
      collection_threshold: gc.collection_threshold,
      total_collected: gc.total_collected,
      total_freed: gc.total_freed
    }
  }
  
  // Add reference to object
  let add_reference = fn(gc: GarbageCollector, object_id: String) {
    let object_index = gc.objects.find_index(fn(obj) { obj.id == object_id })
    
    match object_index {
      Some(index) => {
        let obj = gc.objects[index]
        let updated_obj = { obj | references: obj.references + 1 }
        let updated_objects = gc.objects.update(index, updated_obj)
        
        {
          objects: updated_objects,
          collection_threshold: gc.collection_threshold,
          total_collected: gc.total_collected,
          total_freed: gc.total_freed
        }
      }
      None => gc
    }
  }
  
  // Remove reference from object
  let remove_reference = fn(gc: GarbageCollector, object_id: String) {
    let object_index = gc.objects.find_index(fn(obj) { obj.id == object_id })
    
    match object_index {
      Some(index) => {
        let obj = gc.objects[index]
        let updated_obj = { obj | references: obj.references - 1 }
        let updated_objects = gc.objects.update(index, updated_obj)
        
        {
          objects: updated_objects,
          collection_threshold: gc.collection_threshold,
          total_collected: gc.total_collected,
          total_freed: gc.total_freed
        }
      }
      None => gc
    }
  }
  
  // Mark phase of garbage collection
  let mark_objects = fn(gc: GarbageCollector, root_objects: Array[String]) {
    let mut updated_objects = gc.objects
    
    // Mark root objects
    for i in 0..root_objects.length() {
      let root_id = root_objects[i]
      let root_index = updated_objects.find_index(fn(obj) { obj.id == root_id })
      
      match root_index {
        Some(index) => {
          let obj = updated_objects[index]
          let marked_obj = { obj | marked: true }
          updated_objects = updated_objects.update(index, marked_obj)
        }
        None => {}
      }
    }
    
    // Mark all objects with references > 0
    for i in 0..updated_objects.length() {
      let obj = updated_objects[i]
      if obj.references > 0 {
        let marked_obj = { obj | marked: true }
        updated_objects = updated_objects.update(i, marked_obj)
      }
    }
    
    {
      objects: updated_objects,
      collection_threshold: gc.collection_threshold,
      total_collected: gc.total_collected,
      total_freed: gc.total_freed
    }
  }
  
  // Sweep phase of garbage collection
  let sweep_objects = fn(gc: GarbageCollector) {
    let mut kept_objects = []
    let mut collected_size = 0
    let mut collected_count = 0
    
    for i in 0..gc.objects.length() {
      let obj = gc.objects[i]
      
      if obj.marked {
        // Keep the object, but unmark for next collection
        let unmarked_obj = { obj | marked: false }
        kept_objects = kept_objects.push(unmarked_obj)
      } else {
        // Collect the object
        collected_size = collected_size + obj.size
        collected_count = collected_count + 1
      }
    }
    
    {
      objects: kept_objects,
      collection_threshold: gc.collection_threshold,
      total_collected: gc.total_collected + collected_count,
      total_freed: gc.total_freed + collected_size
    }
  }
  
  // Run garbage collection
  let run_garbage_collection = fn(gc: GarbageCollector, root_objects: Array[String]) {
    let marked_gc = mark_objects(gc, root_objects)
    sweep_objects(marked_gc)
  }
  
  // Test garbage collection
  let gc = create_garbage_collector(10)
  
  // Allocate objects
  let gc1 = allocate_object(gc, 1024, 1000)
  let gc2 = allocate_object(gc1, 2048, 1005)
  let gc3 = allocate_object(gc2, 512, 1010)
  let gc4 = allocate_object(gc3, 1024, 1015)
  let gc5 = allocate_object(gc4, 2048, 1020)
  
  assert_eq(gc5.objects.length(), 5)
  
  // Add references to some objects
  let gc6 = add_reference(gc5, "obj-0")
  let gc7 = add_reference(gc6, "obj-2")
  
  // Remove reference from one object
  let gc8 = remove_reference(gc7, "obj-1")  // obj-1 now has 0 references
  
  // Run garbage collection with obj-0 and obj-2 as roots
  let gc9 = run_garbage_collection(gc8, ["obj-0", "obj-2"])
  
  // obj-0, obj-2 should be kept (marked)
  // obj-1, obj-3, obj-4 should be collected (unmarked)
  assert_eq(gc9.objects.length(), 2)
  assert_eq(gc9.total_collected, 3)
  assert_eq(gc9.total_freed, 5120)  // 2048 + 1024 + 2048
  
  // Check which objects remain
  let remaining_ids = gc9.objects.map(fn(obj) { obj.id })
  assert_true(remaining_ids.contains("obj-0"))
  assert_true(remaining_ids.contains("obj-2"))
  assert_false(remaining_ids.contains("obj-1"))
  assert_false(remaining_ids.contains("obj-3"))
  assert_false(remaining_ids.contains("obj-4"))
}

// Test 6: Resource Monitoring and Metrics
test "resource monitoring and metrics" {
  // Define resource metric
  type ResourceMetric = {
    name: String,
    value: Float,
    unit: String,
    timestamp: Int,
    tags: Map[String, String]
  }
  
  // Define resource monitor
  type ResourceMonitor = {
    metrics: Array[ResourceMetric>,
    alerts: Array[String],
    thresholds: Map[String, Float]
  }
  
  // Create resource monitor
  let create_resource_monitor = fn() {
    {
      metrics: [],
      alerts: [],
      thresholds: [
        ("memory_usage", 0.8),
        ("cpu_usage", 0.9),
        ("connection_usage", 0.75),
        ("disk_usage", 0.9)
      ]
    }
  }
  
  // Record metric
  let record_metric = fn(monitor: ResourceMonitor, name: String, value: Float, unit: String, timestamp: Int, tags: Map[String, String]) {
    let metric = {
      name: name,
      value: value,
      unit: unit,
      timestamp: timestamp,
      tags: tags
    }
    
    let updated_metrics = monitor.metrics.push(metric)
    
    // Check for alerts
    let threshold_opt = monitor.thresholds.find(fn(pair) { pair.0 == name })
    let mut new_alerts = monitor.alerts
    
    match threshold_opt {
      Some((_, threshold)) => {
        if value > threshold {
          let alert = name + " exceeded threshold: " + value.to_string() + " > " + threshold.to_string()
          new_alerts = new_alerts.push(alert)
        }
      }
      None => {}
    }
    
    {
      metrics: updated_metrics,
      alerts: new_alerts,
      thresholds: monitor.thresholds
    }
  }
  
  // Get latest metric value
  let get_latest_metric = fn(monitor: ResourceMonitor, name: String) {
    let matching_metrics = monitor.metrics.filter(fn(m) { m.name == name })
    
    if matching_metrics.length() > 0 {
      let latest = matching_metrics[matching_metrics.length() - 1]
      Some(latest.value)
    } else {
      None
    }
  }
  
  // Get average metric value over time range
  let get_average_metric = fn(monitor: ResourceMonitor, name: String, start_time: Int, end_time: Int) {
    let matching_metrics = monitor.metrics.filter(fn(m) { 
      m.name == name and m.timestamp >= start_time and m.timestamp <= end_time
    })
    
    if matching_metrics.length() > 0 {
      let sum = matching_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
      Some(sum / matching_metrics.length() as Float)
    } else {
      None
    }
  }
  
  // Get metrics by tags
  let get_metrics_by_tags = fn(monitor: ResourceMonitor, tags: Map[String, String]) {
    monitor.metrics.filter(fn(m) {
      let mut all_match = true
      
      for i in 0..tags.length() {
        let tag = tags[i]
        let tag_value = match m.tags.find(fn(pair) { pair.0 == tag.0 }) {
          Some((_, value)) => value
          None => ""
        }
        
        if tag_value != tag.1 {
          all_match = false
        }
      }
      
      all_match
    })
  }
  
  // Test resource monitoring
  let monitor = create_resource_monitor()
  
  // Record metrics
  let monitor1 = record_metric(monitor, "memory_usage", 0.5, "ratio", 1000, [("service", "api-gateway")])
  let monitor2 = record_metric(monitor1, "cpu_usage", 0.3, "ratio", 1005, [("service", "api-gateway")])
  let monitor3 = record_metric(monitor2, "memory_usage", 0.85, "ratio", 1010, [("service", "user-service")])
  let monitor4 = record_metric(monitor3, "connection_usage", 0.8, "ratio", 1015, [("service", "user-service")])
  let monitor5 = record_metric(monitor4, "memory_usage", 0.7, "ratio", 1020, [("service", "api-gateway")])
  
  // Check for alerts
  assert_eq(monitor5.alerts.length(), 2)  // memory_usage (0.85) and connection_usage (0.8) exceeded thresholds
  
  // Get latest metrics
  let latest_memory = get_latest_metric(monitor5, "memory_usage")
  match latest_memory {
    Some(value) => assert_eq(value, 0.7)
    None => assert_true(false)
  }
  
  let latest_cpu = get_latest_metric(monitor5, "cpu_usage")
  match latest_cpu {
    Some(value) => assert_eq(value, 0.3)
    None => assert_true(false)
  }
  
  // Get average metrics
  let avg_memory = get_average_metric(monitor5, "memory_usage", 1000, 1020)
  match avg_memory {
    Some(value) => assert_eq(value, (0.5 + 0.85 + 0.7) / 3.0)
    None => assert_true(false)
  }
  
  // Get metrics by tags
  let api_gateway_metrics = get_metrics_by_tags(monitor5, [("service", "api-gateway")])
  assert_eq(api_gateway_metrics.length(), 3)
  
  let user_service_metrics = get_metrics_by_tags(monitor5, [("service", "user-service")])
  assert_eq(user_service_metrics.length(), 2)
}