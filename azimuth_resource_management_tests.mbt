// Azimuth Resource Management Tests
// This file contains test cases for resource management functionality

// Test 1: Memory Resource Management
test "memory resource management" {
  // Define memory resource types
  type MemoryBlock = {
    id: String,
    size_bytes: Int,
    allocated: Bool,
    allocation_time: Int,
    owner: Option[String>
  }
  
  type MemoryPool = {
    total_size_bytes: Int,
    used_size_bytes: Int,
    available_size_bytes: Int,
    blocks: Array[MemoryBlock>
  }
  
  // Create a memory pool
  let create_memory_pool = fn(total_size_bytes: Int) -> MemoryPool {
    {
      total_size_bytes: total_size_bytes,
      used_size_bytes: 0,
      available_size_bytes: total_size_bytes,
      blocks: []
    }
  }
  
  // Allocate memory from the pool
  let allocate_memory = fn(pool: MemoryPool, size_bytes: Int, owner: String) -> (MemoryPool, Option[String]) {
    if pool.available_size_bytes >= size_bytes {
      let block_id = "block_" + (1640995200 + pool.blocks.length()).to_string()
      let new_block = {
        id: block_id,
        size_bytes: size_bytes,
        allocated: true,
        allocation_time: 1640995200,
        owner: Some(owner)
      }
      
      let updated_pool = {
        total_size_bytes: pool.total_size_bytes,
        used_size_bytes: pool.used_size_bytes + size_bytes,
        available_size_bytes: pool.available_size_bytes - size_bytes,
        blocks: pool.blocks.push(new_block)
      }
      
      (updated_pool, Some(block_id))
    } else {
      // Not enough memory available
      (pool, None)
    }
  }
  
  // Free memory back to the pool
  let free_memory = fn(pool: MemoryPool, block_id: String) -> MemoryPool {
    let mut updated_pool = pool
    let mut found = false
    
    let updated_blocks = []
    for block in pool.blocks {
      if block.id == block_id and block.allocated {
        found = true
        updated_pool.used_size_bytes = updated_pool.used_size_bytes - block.size_bytes
        updated_pool.available_size_bytes = updated_pool.available_size_bytes + block.size_bytes
        
        // Mark block as freed
        updated_blocks = updated_blocks.push({
          id: block.id,
          size_bytes: block.size_bytes,
          allocated: false,
          allocation_time: block.allocation_time,
          owner: None
        })
      } else {
        updated_blocks = updated_blocks.push(block)
      }
    }
    
    if found {
      updated_pool.blocks = updated_blocks
    }
    
    updated_pool
  }
  
  // Test memory pool operations
  let pool1 = create_memory_pool(1024)  // 1KB memory pool
  assert_eq(pool1.total_size_bytes, 1024)
  assert_eq(pool1.used_size_bytes, 0)
  assert_eq(pool1.available_size_bytes, 1024)
  assert_eq(pool1.blocks.length(), 0)
  
  // Allocate memory
  let (pool2, block1_id) = allocate_memory(pool1, 256, "process1")
  assert_eq(pool2.used_size_bytes, 256)
  assert_eq(pool2.available_size_bytes, 768)
  assert_eq(pool2.blocks.length(), 1)
  assert_eq(block1_id, Some("block_1640995200"))
  
  // Allocate more memory
  let (pool3, block2_id) = allocate_memory(pool2, 512, "process2")
  assert_eq(pool3.used_size_bytes, 768)
  assert_eq(pool3.available_size_bytes, 256)
  assert_eq(pool3.blocks.length(), 2)
  assert_eq(block2_id, Some("block_1640995201"))
  
  // Try to allocate more memory than available
  let (pool4, block3_id) = allocate_memory(pool3, 512, "process3")
  assert_eq(pool4.used_size_bytes, 768)  // Unchanged
  assert_eq(pool4.available_size_bytes, 256)  // Unchanged
  assert_eq(pool4.blocks.length(), 2)  // Unchanged
  assert_eq(block3_id, None)
  
  // Free memory
  let pool5 = free_memory(pool4, "block_1640995200")
  assert_eq(pool5.used_size_bytes, 512)
  assert_eq(pool5.available_size_bytes, 512)
  assert_eq(pool5.blocks.length(), 2)
  
  // Now we can allocate the previously failed request
  let (pool6, block4_id) = allocate_memory(pool5, 512, "process3")
  assert_eq(pool6.used_size_bytes, 1024)
  assert_eq(pool6.available_size_bytes, 0)
  assert_eq(pool6.blocks.length(), 3)
  assert_eq(block4_id, Some("block_1640995202"))
  
  // Test memory fragmentation simulation
  let pool7 = create_memory_pool(2048)
  let (pool8, _) = allocate_memory(pool7, 512, "process1")
  let (pool9, _) = allocate_memory(pool8, 256, "process2")
  let (pool10, _) = allocate_memory(pool9, 768, "process3")
  
  // Free middle block (fragmentation)
  let pool11 = free_memory(pool10, "block_1640995201")
  
  // We have 256 bytes available, but it's fragmented
  assert_eq(pool11.used_size_bytes, 1280)
  assert_eq(pool11.available_size_bytes, 768)
  
  // We can allocate a block that fits in the freed space
  let (pool12, _) = allocate_memory(pool11, 256, "process4")
  assert_eq(pool12.used_size_bytes, 1536)
  assert_eq(pool12.available_size_bytes, 512)
  
  // But we can't allocate a block larger than any single free space
  let (pool13, _) = allocate_memory(pool12, 600, "process5")
  assert_eq(pool13.used_size_bytes, 1536)  // Unchanged
  assert_eq(pool13.available_size_bytes, 512)  // Unchanged
}

// Test 2: File Handle Management
test "file handle management" {
  // Define file handle types
  type FileHandle = {
    id: String,
    path: String,
    mode: String,  // "read", "write", "append"
    opened: Bool,
    open_time: Int,
    owner: Option[String>
  }
  
  type FileHandleManager = {
    max_handles: Int,
    active_handles: Int,
    handles: Array[FileHandle>
  }
  
  // Create a file handle manager
  let create_file_manager = fn(max_handles: Int) -> FileHandleManager {
    {
      max_handles: max_handles,
      active_handles: 0,
      handles: []
    }
  }
  
  // Open a file handle
  let open_file = fn(manager: FileHandleManager, path: String, mode: String, owner: String) -> (FileHandleManager, Option[String]) {
    if manager.active_handles < manager.max_handles {
      let handle_id = "handle_" + (1640995200 + manager.handles.length()).to_string()
      let new_handle = {
        id: handle_id,
        path: path,
        mode: mode,
        opened: true,
        open_time: 1640995200,
        owner: Some(owner)
      }
      
      let updated_manager = {
        max_handles: manager.max_handles,
        active_handles: manager.active_handles + 1,
        handles: manager.handles.push(new_handle)
      }
      
      (updated_manager, Some(handle_id))
    } else {
      // Too many open handles
      (manager, None)
    }
  }
  
  // Close a file handle
  let close_file = fn(manager: FileHandleManager, handle_id: String) -> FileHandleManager {
    let mut updated_manager = manager
    let mut found = false
    
    let updated_handles = []
    for handle in manager.handles {
      if handle.id == handle_id and handle.opened {
        found = true
        updated_manager.active_handles = updated_manager.active_handles - 1
        
        // Mark handle as closed
        updated_handles = updated_handles.push({
          id: handle.id,
          path: handle.path,
          mode: handle.mode,
          opened: false,
          open_time: handle.open_time,
          owner: None
        })
      } else {
        updated_handles = updated_handles.push(handle)
      }
    }
    
    if found {
      updated_manager.handles = updated_handles
    }
    
    updated_manager
  }
  
  // Test file handle management
  let manager1 = create_file_manager(5)
  assert_eq(manager1.max_handles, 5)
  assert_eq(manager1.active_handles, 0)
  assert_eq(manager1.handles.length(), 0)
  
  // Open files
  let (manager2, handle1_id) = open_file(manager1, "/tmp/file1.txt", "read", "process1")
  assert_eq(manager2.active_handles, 1)
  assert_eq(manager2.handles.length(), 1)
  assert_eq(handle1_id, Some("handle_1640995200"))
  
  let (manager3, handle2_id) = open_file(manager2, "/tmp/file2.txt", "write", "process2")
  assert_eq(manager3.active_handles, 2)
  assert_eq(manager3.handles.length(), 2)
  assert_eq(handle2_id, Some("handle_1640995201"))
  
  // Close a file
  let manager4 = close_file(manager3, "handle_1640995200")
  assert_eq(manager4.active_handles, 1)
  assert_eq(manager4.handles.length(), 2)
  
  // Open more files
  let (manager5, handle3_id) = open_file(manager4, "/tmp/file3.txt", "read", "process3")
  let (manager6, handle4_id) = open_file(manager5, "/tmp/file4.txt", "write", "process4")
  let (manager7, handle5_id) = open_file(manager6, "/tmp/file5.txt", "append", "process5")
  
  assert_eq(manager7.active_handles, 4)
  assert_eq(manager7.handles.length(), 5)
  
  // Try to open another file (should fail due to handle limit)
  let (manager8, handle6_id) = open_file(manager7, "/tmp/file6.txt", "read", "process6")
  assert_eq(manager8.active_handles, 4)  // Unchanged
  assert_eq(manager8.handles.length(), 5)  // Unchanged
  assert_eq(handle6_id, None)
  
  // Close a file and try again
  let manager9 = close_file(manager8, "handle_1640995201")
  let (manager10, handle7_id) = open_file(manager9, "/tmp/file6.txt", "read", "process6")
  
  assert_eq(manager10.active_handles, 4)
  assert_eq(manager10.handles.length(), 6)
  assert_eq(handle7_id, Some("handle_1640995205"))
}

// Test 3: Database Connection Pool Management
test "database connection pool management" {
  // Define database connection types
  type DatabaseConnection = {
    id: String,
    database: String,
    connected: Bool,
    in_use: Bool,
    created_time: Int,
    last_used_time: Int,
    user: Option<String>
  }
  
  type ConnectionPool = {
    max_connections: Int,
    min_connections: Int,
    active_connections: Int,
    idle_connections: Int,
    connections: Array[DatabaseConnection>
  }
  
  // Create a connection pool
  let create_connection_pool = fn(max_connections: Int, min_connections: Int) -> ConnectionPool {
    let mut initial_connections = []
    
    // Create minimum connections
    for i in 0..min_connections {
      let connection_id = "conn_" + i.to_string()
      initial_connections = initial_connections.push({
        id: connection_id,
        database: "azimuth_db",
        connected: true,
        in_use: false,
        created_time: 1640995200,
        last_used_time: 1640995200,
        user: None
      })
    }
    
    {
      max_connections: max_connections,
      min_connections: min_connections,
      active_connections: min_connections,
      idle_connections: min_connections,
      connections: initial_connections
    }
  }
  
  // Get a connection from the pool
  let get_connection = fn(pool: ConnectionPool, user: String) -> (ConnectionPool, Option[String]) {
    // Find an idle connection
    let mut found_idle = false
    let mut updated_connections = []
    let mut connection_id = None
    
    for connection in pool.connections {
      if connection.connected and not(connection.in_use) and not(found_idle) {
        found_idle = true
        connection_id = Some(connection.id)
        
        updated_connections = updated_connections.push({
          id: connection.id,
          database: connection.database,
          connected: connection.connected,
          in_use: true,
          created_time: connection.created_time,
          last_used_time: 1640995300,
          user: Some(user)
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    if found_idle {
      let updated_pool = {
        max_connections: pool.max_connections,
        min_connections: pool.min_connections,
        active_connections: pool.active_connections,
        idle_connections: pool.idle_connections - 1,
        connections: updated_connections
      }
      
      (updated_pool, connection_id)
    } else if pool.active_connections < pool.max_connections {
      // Create a new connection
      let new_connection_id = "conn_" + pool.active_connections.to_string()
      let new_connection = {
        id: new_connection_id,
        database: "azimuth_db",
        connected: true,
        in_use: true,
        created_time: 1640995300,
        last_used_time: 1640995300,
        user: Some(user)
      }
      
      let updated_pool = {
        max_connections: pool.max_connections,
        min_connections: pool.min_connections,
        active_connections: pool.active_connections + 1,
        idle_connections: pool.idle_connections,
        connections: pool.connections.push(new_connection)
      }
      
      (updated_pool, Some(new_connection_id))
    } else {
      // No available connections
      (pool, None)
    }
  }
  
  // Release a connection back to the pool
  let release_connection = fn(pool: ConnectionPool, connection_id: String) -> ConnectionPool {
    let mut updated_connections = []
    let mut found = false
    
    for connection in pool.connections {
      if connection.id == connection_id and connection.in_use {
        found = true
        
        updated_connections = updated_connections.push({
          id: connection.id,
          database: connection.database,
          connected: connection.connected,
          in_use: false,
          created_time: connection.created_time,
          last_used_time: 1640995400,
          user: None
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    if found {
      {
        max_connections: pool.max_connections,
        min_connections: pool.min_connections,
        active_connections: pool.active_connections,
        idle_connections: pool.idle_connections + 1,
        connections: updated_connections
      }
    } else {
      pool
    }
  }
  
  // Test connection pool operations
  let pool1 = create_connection_pool(5, 2)
  assert_eq(pool1.max_connections, 5)
  assert_eq(pool1.min_connections, 2)
  assert_eq(pool1.active_connections, 2)
  assert_eq(pool1.idle_connections, 2)
  assert_eq(pool1.connections.length(), 2)
  
  // Get connections
  let (pool2, conn1_id) = get_connection(pool1, "user1")
  assert_eq(pool2.active_connections, 2)
  assert_eq(pool2.idle_connections, 1)
  assert_eq(conn1_id, Some("conn_0"))
  
  let (pool3, conn2_id) = get_connection(pool2, "user2")
  assert_eq(pool3.active_connections, 2)
  assert_eq(pool3.idle_connections, 0)
  assert_eq(conn2_id, Some("conn_1"))
  
  // Get more connections (will create new ones)
  let (pool4, conn3_id) = get_connection(pool3, "user3")
  assert_eq(pool4.active_connections, 3)
  assert_eq(pool4.idle_connections, 0)
  assert_eq(conn3_id, Some("conn_2"))
  
  let (pool5, conn4_id) = get_connection(pool4, "user4")
  assert_eq(pool5.active_connections, 4)
  assert_eq(pool5.idle_connections, 0)
  assert_eq(conn4_id, Some("conn_3"))
  
  let (pool6, conn5_id) = get_connection(pool5, "user5")
  assert_eq(pool6.active_connections, 5)
  assert_eq(pool6.idle_connections, 0)
  assert_eq(conn5_id, Some("conn_4"))
  
  // Try to get another connection (should fail)
  let (pool7, conn6_id) = get_connection(pool6, "user6")
  assert_eq(pool7.active_connections, 5)  // Unchanged
  assert_eq(pool7.idle_connections, 0)  // Unchanged
  assert_eq(conn6_id, None)
  
  // Release connections
  let pool8 = release_connection(pool7, "conn_0")
  assert_eq(pool8.active_connections, 5)
  assert_eq(pool8.idle_connections, 1)
  
  let pool9 = release_connection(pool8, "conn_1")
  assert_eq(pool9.active_connections, 5)
  assert_eq(pool9.idle_connections, 2)
  
  // Now we can get connections again
  let (pool10, conn7_id) = get_connection(pool9, "user6")
  assert_eq(pool10.active_connections, 5)
  assert_eq(pool10.idle_connections, 1)
  assert_eq(conn7_id, Some("conn_0"))
}

// Test 4: Resource Cleanup and Garbage Collection
test "resource cleanup and garbage collection" {
  // Define resource types
  type Resource = {
    id: String,
    type: String,
    created_time: Int,
    last_accessed_time: Int,
    access_count: Int,
    size_bytes: Int,
    in_use: Bool
  }
  
  type ResourceManager = {
    resources: Array[Resource>,
    max_memory_bytes: Int,
    used_memory_bytes: Int,
    cleanup_threshold_seconds: Int
  }
  
  // Create a resource manager
  let create_resource_manager = fn(max_memory_bytes: Int, cleanup_threshold_seconds: Int) -> ResourceManager {
    {
      resources: [],
      max_memory_bytes: max_memory_bytes,
      used_memory_bytes: 0,
      cleanup_threshold_seconds: cleanup_threshold_seconds
    }
  }
  
  // Add a resource
  let add_resource = fn(manager: ResourceManager, resource_type: String, size_bytes: Int, current_time: Int) -> ResourceManager {
    let resource_id = "resource_" + manager.resources.length().to_string()
    let new_resource = {
      id: resource_id,
      type: resource_type,
      created_time: current_time,
      last_accessed_time: current_time,
      access_count: 1,
      size_bytes: size_bytes,
      in_use: false
    }
    
    {
      resources: manager.resources.push(new_resource),
      max_memory_bytes: manager.max_memory_bytes,
      used_memory_bytes: manager.used_memory_bytes + size_bytes,
      cleanup_threshold_seconds: manager.cleanup_threshold_seconds
    }
  }
  
  // Access a resource
  let access_resource = fn(manager: ResourceManager, resource_id: String, current_time: Int) -> ResourceManager {
    let updated_resources = []
    
    for resource in manager.resources {
      if resource.id == resource_id {
        updated_resources = updated_resources.push({
          id: resource.id,
          type: resource.type,
          created_time: resource.created_time,
          last_accessed_time: current_time,
          access_count: resource.access_count + 1,
          size_bytes: resource.size_bytes,
          in_use: true
        })
      } else {
        updated_resources = updated_resources.push({
          id: resource.id,
          type: resource.type,
          created_time: resource.created_time,
          last_accessed_time: resource.last_accessed_time,
          access_count: resource.access_count,
          size_bytes: resource.size_bytes,
          in_use: false
        })
      }
    }
    
    {
      resources: updated_resources,
      max_memory_bytes: manager.max_memory_bytes,
      used_memory_bytes: manager.used_memory_bytes,
      cleanup_threshold_seconds: manager.cleanup_threshold_seconds
    }
  }
  
  // Cleanup unused resources
  let cleanup_resources = fn(manager: ResourceManager, current_time: Int) -> ResourceManager {
    let mut cleaned_resources = []
    let mut freed_memory = 0
    
    for resource in manager.resources {
      let time_since_last_access = current_time - resource.last_accessed_time
      
      if not(resource.in_use) and time_since_last_access >= manager.cleanup_threshold_seconds {
        // Clean up this resource
        freed_memory = freed_memory + resource.size_bytes
      } else {
        cleaned_resources = cleaned_resources.push(resource)
      }
    }
    
    {
      resources: cleaned_resources,
      max_memory_bytes: manager.max_memory_bytes,
      used_memory_bytes: manager.used_memory_bytes - freed_memory,
      cleanup_threshold_seconds: manager.cleanup_threshold_seconds
    }
  }
  
  // Test resource management
  let manager1 = create_resource_manager(1024, 300)  // 1KB max, 5 min cleanup
  assert_eq(manager1.max_memory_bytes, 1024)
  assert_eq(manager1.used_memory_bytes, 0)
  assert_eq(manager1.resources.length(), 0)
  
  // Add resources
  let manager2 = add_resource(manager1, "cache", 256, 1640995200)
  let manager3 = add_resource(manager2, "temp_file", 128, 1640995200)
  let manager4 = add_resource(manager3, "buffer", 512, 1640995200)
  
  assert_eq(manager4.used_memory_bytes, 896)
  assert_eq(manager4.resources.length(), 3)
  
  // Access some resources
  let manager5 = access_resource(manager4, "resource_0", 1640995300)
  let manager6 = access_resource(manager5, "resource_1", 1640995400)
  
  // Check that access times and counts are updated
  assert_eq(manager6.resources[0].last_accessed_time, 1640995300)
  assert_eq(manager6.resources[0].access_count, 2)
  assert_eq(manager6.resources[0].in_use, true)
  
  assert_eq(manager6.resources[1].last_accessed_time, 1640995400)
  assert_eq(manager6.resources[1].access_count, 2)
  assert_eq(manager6.resources[1].in_use, true)
  
  assert_eq(manager6.resources[2].last_accessed_time, 1640995200)
  assert_eq(manager6.resources[2].access_count, 1)
  assert_eq(manager6.resources[2].in_use, false)
  
  // Cleanup after some time (simulate 10 minutes later)
  let manager7 = cleanup_resources(manager6, 1640995800)
  
  // resource_2 should be cleaned up (not accessed for 10 minutes)
  assert_eq(manager7.used_memory_bytes, 384)  // 896 - 512
  assert_eq(manager7.resources.length(), 2)
  
  // Add more resources
  let manager8 = add_resource(manager7, "new_cache", 200, 1640995800)
  let manager9 = add_resource(manager8, "new_buffer", 300, 1640995800)
  
  assert_eq(manager9.used_memory_bytes, 884)  // 384 + 200 + 300
  assert_eq(manager9.resources.length(), 4)
  
  // Cleanup again after more time
  let manager10 = cleanup_resources(manager9, 1640996200)
  
  // resource_0 and resource_1 should still be there (accessed more recently)
  assert_eq(manager10.used_memory_bytes, 884)
  assert_eq(manager10.resources.length(), 4)
}

// Test 5: Resource Quota Management
test "resource quota management" {
  // Define quota types
  type ResourceQuota = {
    resource_type: String,
    max_amount: Int,
    current_usage: Int,
    unit: String
  }
  
  type QuotaManager = {
    quotas: Array[ResourceQuota>
  }
  
  // Create a quota manager
  let create_quota_manager = fn() -> QuotaManager {
    {
      quotas: [
        {
          resource_type: "cpu_time",
          max_amount: 3600,  // 1 hour in seconds
          current_usage: 0,
          unit: "seconds"
        },
        {
          resource_type: "memory",
          max_amount: 1024,  // 1GB in MB
          current_usage: 0,
          unit: "MB"
        },
        {
          resource_type: "network_bandwidth",
          max_amount: 10240,  // 10GB in MB
          current_usage: 0,
          unit: "MB"
        },
        {
          resource_type: "api_calls",
          max_amount: 10000,
          current_usage: 0,
          unit: "calls"
        }
      ]
    }
  }
  
  // Check if a resource request can be granted
  let can_grant_request = fn(manager: QuotaManager, resource_type: String, amount: Int) -> Bool {
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        return quota.current_usage + amount <= quota.max_amount
      }
    }
    false  // Resource type not found
  }
  
  // Grant a resource request
  let grant_request = fn(manager: QuotaManager, resource_type: String, amount: Int) -> QuotaManager {
    let updated_quotas = []
    
    for quota in manager.quotas {
      if quota.resource_type == resource_type and quota.current_usage + amount <= quota.max_amount {
        updated_quotas = updated_quotas.push({
          resource_type: quota.resource_type,
          max_amount: quota.max_amount,
          current_usage: quota.current_usage + amount,
          unit: quota.unit
        })
      } else {
        updated_quotas = updated_quotas.push(quota)
      }
    }
    
    {
      quotas: updated_quotas
    }
  }
  
  // Release a resource
  let release_resource = fn(manager: QuotaManager, resource_type: String, amount: Int) -> QuotaManager {
    let updated_quotas = []
    
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        updated_quotas = updated_quotas.push({
          resource_type: quota.resource_type,
          max_amount: quota.max_amount,
          current_usage: if quota.current_usage >= amount { quota.current_usage - amount } else { 0 },
          unit: quota.unit
        })
      } else {
        updated_quotas = updated_quotas.push(quota)
      }
    }
    
    {
      quotas: updated_quotas
    }
  }
  
  // Get current usage for a resource type
  let get_usage = fn(manager: QuotaManager, resource_type: String) -> Option[Int] {
    for quota in manager.quotas {
      if quota.resource_type == resource_type {
        return Some(quota.current_usage)
      }
    }
    None
  }
  
  // Test quota management
  let manager1 = create_quota_manager()
  
  // Check initial usage
  assert_eq(get_usage(manager1, "cpu_time"), Some(0))
  assert_eq(get_usage(manager1, "memory"), Some(0))
  assert_eq(get_usage(manager1, "network_bandwidth"), Some(0))
  assert_eq(get_usage(manager1, "api_calls"), Some(0))
  
  // Grant some requests
  assert_true(can_grant_request(manager1, "cpu_time", 1800))
  let manager2 = grant_request(manager1, "cpu_time", 1800)
  assert_eq(get_usage(manager2, "cpu_time"), Some(1800))
  
  assert_true(can_grant_request(manager2, "memory", 512))
  let manager3 = grant_request(manager2, "memory", 512)
  assert_eq(get_usage(manager3, "memory"), Some(512))
  
  assert_true(can_grant_request(manager3, "api_calls", 5000))
  let manager4 = grant_request(manager3, "api_calls", 5000)
  assert_eq(get_usage(manager4, "api_calls"), Some(5000))
  
  // Try to exceed quota
  assert_false(can_grant_request(manager4, "cpu_time", 2000))  // Would exceed 3600 limit
  assert_true(can_grant_request(manager4, "cpu_time", 1800))   // Exactly at limit
  
  let manager5 = grant_request(manager4, "cpu_time", 1800)
  assert_eq(get_usage(manager5, "cpu_time"), Some(3600))
  
  assert_false(can_grant_request(manager5, "cpu_time", 1))     // Exceeds limit
  assert_false(can_grant_request(manager5, "memory", 600))     // Would exceed 1024 limit
  assert_true(can_grant_request(manager5, "memory", 512))      // Exactly at limit
  
  // Release some resources
  let manager6 = release_resource(manager5, "cpu_time", 1800)
  assert_eq(get_usage(manager6, "cpu_time"), Some(1800))
  
  let manager7 = release_resource(manager6, "memory", 256)
  assert_eq(get_usage(manager7, "memory"), Some(256))
  
  // Now we can grant more requests
  assert_true(can_grant_request(manager7, "cpu_time", 1800))
  let manager8 = grant_request(manager7, "cpu_time", 1800)
  assert_eq(get_usage(manager8, "cpu_time"), Some(3600))
  
  assert_true(can_grant_request(manager8, "memory", 768))
  let manager9 = grant_request(manager8, "memory", 768)
  assert_eq(get_usage(manager9, "memory"), Some(1024))
  
  // Test with multiple resource types
  assert_true(can_grant_request(manager9, "network_bandwidth", 5000))
  let manager10 = grant_request(manager9, "network_bandwidth", 5000)
  assert_eq(get_usage(manager10, "network_bandwidth"), Some(5000))
  
  assert_true(can_grant_request(manager10, "network_bandwidth", 5000))
  let manager11 = grant_request(manager10, "network_bandwidth", 5000)
  assert_eq(get_usage(manager11, "network_bandwidth"), Some(10000))
  
  assert_false(can_grant_request(manager11, "network_bandwidth", 500))  // Exceeds limit
  
  // Check final state
  assert_eq(get_usage(manager11, "cpu_time"), Some(3600))
  assert_eq(get_usage(manager11, "memory"), Some(1024))
  assert_eq(get_usage(manager11, "network_bandwidth"), Some(10000))
  assert_eq(get_usage(manager11, "api_calls"), Some(5000))
}