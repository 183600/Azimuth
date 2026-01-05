// Azimuth Resource Management Tests
// This file contains test cases for resource management functionality

// Test 1: Memory Pool Management
test "memory pool management" {
  // Define memory block
  type MemoryBlock = {
    id: Int,
    size: Int,
    allocated: Bool,
    data: Array[Int>  // Simplified representation of memory content
  }
  
  // Define memory pool
  type MemoryPool = {
    blocks: Array[MemoryBlock>,
    total_size: Int,
    allocated_size: Int,
    free_blocks: Array<Int>  // Indices of free blocks
  }
  
  // Create memory pool
  let create_memory_pool = fn(block_count: Int, block_size: Int) {
    let mut blocks = []
    let mut free_blocks = []
    
    for i in 0 .. block_count {
      blocks = blocks.push({
        id: i,
        size: block_size,
        allocated: false,
        data: []
      })
      free_blocks = free_blocks.push(i)
    }
    
    {
      blocks,
      total_size: block_count * block_size,
      allocated_size: 0,
      free_blocks
    }
  }
  
  // Allocate block
  let allocate_block = fn(pool: MemoryPool) {
    if pool.free_blocks.length() > 0 {
      let block_id = pool.free_blocks[0]
      let updated_blocks = pool.blocks.map(fn(block) {
        if block.id == block_id {
          { block | allocated: true }
        } else {
          block
        }
      })
      
      let updated_free_blocks = pool.free_blocks.slice(1)
      let allocated_block = pool.blocks[block_id]
      
      (
        Some(allocated_block),
        {
          blocks: updated_blocks,
          total_size: pool.total_size,
          allocated_size: pool.allocated_size + allocated_block.size,
          free_blocks: updated_free_blocks
        }
      )
    } else {
      (None, pool)
    }
  }
  
  // Free block
  let free_block = fn(pool: MemoryPool, block_id: Int) {
    let block = pool.blocks[block_id]
    
    if block.allocated {
      let updated_blocks = pool.blocks.map(fn(b) {
        if b.id == block_id {
          { b | allocated: false, data: [] }
        } else {
          b
        }
      })
      
      let updated_free_blocks = pool.free_blocks.push(block_id)
      
      {
        blocks: updated_blocks,
        total_size: pool.total_size,
        allocated_size: pool.allocated_size - block.size,
        free_blocks: updated_free_blocks
      }
    } else {
      pool
    }
  }
  
  // Get pool statistics
  let get_pool_stats = fn(pool: MemoryPool) {
    {
      total_blocks: pool.blocks.length(),
      allocated_blocks: pool.blocks.filter(fn(b) { b.allocated }).length(),
      free_blocks: pool.free_blocks.length(),
      total_size: pool.total_size,
      allocated_size: pool.allocated_size,
      free_size: pool.total_size - pool.allocated_size,
      utilization_rate: pool.allocated_size.to_float() / pool.total_size.to_float()
    }
  }
  
  // Create memory pool with 10 blocks of 1024 bytes each
  let pool = create_memory_pool(10, 1024)
  assert_eq(pool.blocks.length(), 10)
  assert_eq(pool.total_size, 10240)
  assert_eq(pool.allocated_size, 0)
  assert_eq(pool.free_blocks.length(), 10)
  
  // Get initial statistics
  let initial_stats = get_pool_stats(pool)
  assert_eq(initial_stats.total_blocks, 10)
  assert_eq(initial_stats.allocated_blocks, 0)
  assert_eq(initial_stats.free_blocks, 10)
  assert_eq(initial_stats.utilization_rate, 0.0)
  
  // Allocate blocks
  let (block1, pool_after_alloc1) = allocate_block(pool)
  match block1 {
    Some(b) => {
      assert_eq(b.id, 0)
      assert_true(b.allocated)
      assert_eq(b.size, 1024)
    }
    None => assert_true(false)
  }
  
  let (block2, pool_after_alloc2) = allocate_block(pool_after_alloc1)
  match block2 {
    Some(b) => {
      assert_eq(b.id, 1)
      assert_true(b.allocated)
    }
    None => assert_true(false)
  }
  
  // Check statistics after allocations
  let stats_after_alloc = get_pool_stats(pool_after_alloc2)
  assert_eq(stats_after_alloc.allocated_blocks, 2)
  assert_eq(stats_after_alloc.free_blocks, 8)
  assert_eq(stats_after_alloc.allocated_size, 2048)
  assert_eq(stats_after_alloc.utilization_rate, 0.2)
  
  // Free a block
  let pool_after_free = free_block(pool_after_alloc2, 0)
  let stats_after_free = get_pool_stats(pool_after_free)
  assert_eq(stats_after_free.allocated_blocks, 1)
  assert_eq(stats_after_free.free_blocks, 9)
  assert_eq(stats_after_free.allocated_size, 1024)
  assert_eq(stats_after_free.utilization_rate, 0.1)
  
  // Allocate all remaining blocks
  let mut current_pool = pool_after_free
  let mut allocated_blocks = []
  
  for i in 0 .. 9 {
    let (block, updated_pool) = allocate_block(current_pool)
    match block {
      Some(b) => allocated_blocks = allocated_blocks.push(b)
      None => assert_true(false)
    }
    current_pool = updated_pool
  }
  
  // Pool should be full now
  let full_stats = get_pool_stats(current_pool)
  assert_eq(full_stats.allocated_blocks, 10)
  assert_eq(full_stats.free_blocks, 0)
  assert_eq(full_stats.allocated_size, 10240)
  assert_eq(full_stats.utilization_rate, 1.0)
  
  // Try to allocate from full pool
  let (no_block, _) = allocate_block(current_pool)
  assert_eq(no_block, None)
  
  // Free all blocks
  let mut empty_pool = current_pool
  for block in allocated_blocks {
    empty_pool = free_block(empty_pool, block.id)
  }
  
  // Pool should be empty now
  let empty_stats = get_pool_stats(empty_pool)
  assert_eq(empty_stats.allocated_blocks, 0)
  assert_eq(empty_stats.free_blocks, 10)
  assert_eq(empty_stats.allocated_size, 0)
  assert_eq(empty_stats.utilization_rate, 0.0)
}

// Test 2: Connection Pool Management
test "connection pool management" {
  // Define connection
  type Connection = {
    id: String,
    host: String,
    port: Int,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    active: Bool
  }
  
  // Define connection pool
  type ConnectionPool = {
    connections: Array[Connection>,
    max_connections: Int,
    min_connections: Int,
    connection_timeout_ms: Int,
    idle_timeout_ms: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(
    max_connections: Int,
    min_connections: Int,
    connection_timeout_ms: Int,
    idle_timeout_ms: Int
  ) {
    {
      connections: [],
      max_connections,
      min_connections,
      connection_timeout_ms,
      idle_timeout_ms
    }
  }
  
  // Create connection
  let create_connection = fn(id: String, host: String, port: Int, current_time: Int) {
    {
      id,
      host,
      port,
      created_at: current_time,
      last_used: current_time,
      in_use: False,
      active: True
    }
  }
  
  // Get available connection
  let get_available_connection = fn(pool: ConnectionPool, current_time: Int) {
    let available_connections = pool.connections.filter(fn(c) { 
      not(c.in_use) and c.active and 
      (current_time - c.last_used) < pool.idle_timeout_ms
    })
    
    if available_connections.length() > 0 {
      let connection = available_connections[0]
      let updated_connections = pool.connections.map(fn(c) {
        if c.id == connection.id {
          { c | in_use: true, last_used: current_time }
        } else {
          c
        }
      })
      
      Some((
        connection,
        { pool | connections: updated_connections }
      ))
    } else if pool.connections.length() < pool.max_connections {
      // Create new connection
      let new_id = "conn-" + pool.connections.length().to_string()
      let new_connection = create_connection(new_id, "localhost", 5432, current_time)
      let updated_pool = { pool | connections: pool.connections.push({ new_connection | in_use: true }) }
      
      Some((new_connection, updated_pool))
    } else {
      None
    }
  }
  
  // Release connection
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    let updated_connections = pool.connections.map(fn(c) {
      if c.id == connection_id {
        { c | in_use: False, last_used: current_time }
      } else {
        c
      }
    })
    
    { pool | connections: updated_connections }
  }
  
  // Close connection
  let close_connection = fn(pool: ConnectionPool, connection_id: String) {
    let updated_connections = pool.connections.map(fn(c) {
      if c.id == connection_id {
        { c | active: False, in_use: False }
      } else {
        c
      }
    })
    
    { pool | connections: updated_connections }
  }
  
  // Clean up idle connections
  let cleanup_idle_connections = fn(pool: ConnectionPool, current_time: Int) {
    let active_connections = pool.connections.filter(fn(c) { 
      c.active or (not(c.in_use) and (current_time - c.last_used) < pool.idle_timeout_ms)
    })
    
    { pool | connections: active_connections }
  }
  
  // Get pool statistics
  let get_pool_statistics = fn(pool: ConnectionPool) {
    let active_connections = pool.connections.filter(fn(c) { c.active })
    let in_use_connections = pool.connections.filter(fn(c) { c.in_use })
    let idle_connections = pool.connections.filter(fn(c) { not(c.in_use) and c.active })
    
    {
      total_connections: pool.connections.length(),
      active_connections: active_connections.length(),
      in_use_connections: in_use_connections.length(),
      idle_connections: idle_connections.length(),
      max_connections: pool.max_connections,
      min_connections: pool.min_connections,
      utilization_rate: in_use_connections.length().to_float() / pool.max_connections.to_float()
    }
  }
  
  // Create connection pool
  let pool = create_connection_pool(10, 2, 5000, 30000)
  assert_eq(pool.max_connections, 10)
  assert_eq(pool.min_connections, 2)
  
  // Get initial statistics
  let initial_stats = get_pool_statistics(pool)
  assert_eq(initial_stats.total_connections, 0)
  assert_eq(initial_stats.active_connections, 0)
  assert_eq(initial_stats.in_use_connections, 0)
  assert_eq(initial_stats.idle_connections, 0)
  
  // Get connection (should create new one)
  match get_available_connection(pool, 1640995200) {
    Some((connection, updated_pool)) => {
      assert_eq(connection.id, "conn-0")
      assert_true(connection.in_use)
      assert_eq(updated_pool.connections.length(), 1)
    }
    None => assert_true(false)
  }
  
  // Get another connection
  let pool_after_first = match get_available_connection(pool, 1640995200) {
    Some((_, updated)) => updated
    None => pool
  }
  
  match get_available_connection(pool_after_first, 1640995200) {
    Some((connection, updated_pool)) => {
      assert_eq(connection.id, "conn-1")
      assert_true(connection.in_use)
      assert_eq(updated_pool.connections.length(), 2)
    }
    None => assert_true(false)
  }
  
  // Release first connection
  let pool_after_second = match get_available_connection(pool_after_first, 1640995200) {
    Some((_, updated)) => updated
    None => pool_after_first
  }
  
  let pool_after_release = release_connection(pool_after_second, "conn-0", 1640995300)
  
  // Check statistics after release
  let stats_after_release = get_pool_statistics(pool_after_release)
  assert_eq(stats_after_release.total_connections, 2)
  assert_eq(stats_after_release.in_use_connections, 1)
  assert_eq(stats_after_release.idle_connections, 1)
  
  // Get connection again (should reuse the released one)
  match get_available_connection(pool_after_release, 1640995300) {
    Some((connection, _)) => {
      assert_eq(connection.id, "conn-0")  // Should reuse the released connection
      assert_true(connection.in_use)
    }
    None => assert_true(false)
  }
  
  // Close connection
  let pool_with_closed = close_connection(pool_after_release, "conn-1")
  let stats_after_close = get_pool_statistics(pool_with_closed)
  assert_eq(stats_after_close.active_connections, 1)
  
  // Clean up idle connections
  let pool_after_cleanup = cleanup_idle_connections(pool_with_closed, 1640995600)  // 5 minutes later
  let stats_after_cleanup = get_pool_statistics(pool_after_cleanup)
  // Should remove connections idle for more than 30 seconds
  assert_eq(stats_after_cleanup.active_connections, 1)
}

// Test 3: Resource Quota Management
test "resource quota management" {
  // Define resource type
  enum ResourceType {
    CPU
    Memory
    Disk
    Network
  }
  
  // Define resource quota
  type ResourceQuota = {
    resource_type: ResourceType,
    limit: Float,
    used: Float,
    unit: String
  }
  
  // Define resource manager
  type ResourceManager = {
    quotas: Array[ResourceQuota>,
    alerts: Array[String]
  }
  
  // Create resource manager
  let create_resource_manager = fn() {
    { quotas: [], alerts: [] }
  }
  
  // Add quota
  let add_quota = fn(manager: ResourceManager, resource_type: ResourceType, limit: Float, unit: String) {
    let quota = {
      resource_type,
      limit,
      used: 0.0,
      unit
    }
    { manager | quotas: manager.quotas.push(quota) }
  }
  
  // Use resource
  let use_resource = fn(manager: ResourceManager, resource_type: ResourceType, amount: Float) {
    let updated_quotas = manager.quotas.map(fn(quota) {
      if quota.resource_type == resource_type {
        let new_used = quota.used + amount
        let updated_quota = { quota | used: new_used }
        
        // Check if quota is exceeded
        if new_used > quota.limit {
          (updated_quota, Some("Resource quota exceeded for " + resource_type.to_string()))
        } else {
          (updated_quota, None)
        }
      } else {
        (quota, None)
      }
    })
    
    let mut alerts = manager.alerts
    for quota in updated_quotas {
      match quota {
        (q, Some(alert)) => alerts = alerts.push(alert)
        (q, None) => ()  // No alert
      }
    }
    
    let final_quotas = updated_quotas.map(fn(pair) { pair.0 })
    { manager | quotas: final_quotas, alerts }
  }
  
  // Release resource
  let release_resource = fn(manager: ResourceManager, resource_type: ResourceType, amount: Float) {
    let updated_quotas = manager.quotas.map(fn(quota) {
      if quota.resource_type == resource_type {
        let new_used = (quota.used - amount).max(0.0)
        { quota | used: new_used }
      } else {
        quota
      }
    })
    
    { manager | quotas: updated_quotas }
  }
  
  // Get quota usage
  let get_quota_usage = fn(manager: ResourceManager, resource_type: ResourceType) {
    match manager.quotas.find(fn(q) { q.resource_type == resource_type }) {
      Some(quota) => Some({
        resource_type: quota.resource_type,
        limit: quota.limit,
        used: quota.used,
        available: quota.limit - quota.used,
        utilization: quota.used / quota.limit,
        unit: quota.unit
      })
      None => None
    }
  }
  
  // Check for alerts
  let check_alerts = fn(manager: ResourceManager) {
    manager.alerts
  }
  
  // Clear alerts
  let clear_alerts = fn(manager: ResourceManager) {
    { manager | alerts: [] }
  }
  
  // Create resource manager
  let manager = create_resource_manager()
  
  // Add quotas
  let manager_with_quotas = manager
    |> add_quota(ResourceType::CPU, 1000.0, "mCPU")
    |> add_quota(ResourceType::Memory, 2048.0, "MB")
    |> add_quota(ResourceType::Disk, 10240.0, "MB")
    |> add_quota(ResourceType::Network, 100.0, "Mbps")
  
  assert_eq(manager_with_quotas.quotas.length(), 4)
  
  // Get initial quota usage
  match get_quota_usage(manager_with_quotas, ResourceType::CPU) {
    Some(usage) => {
      assert_eq(usage.limit, 1000.0)
      assert_eq(usage.used, 0.0)
      assert_eq(usage.available, 1000.0)
      assert_eq(usage.utilization, 0.0)
    }
    None => assert_true(false)
  }
  
  // Use resources
  let manager_after_use = manager_with_quotas
    |> use_resource(ResourceType::CPU, 500.0)
    |> use_resource(ResourceType::Memory, 1024.0)
    |> use_resource(ResourceType::Disk, 2048.0)
    |> use_resource(ResourceType::Network, 50.0)
  
  // Check quota usage after use
  match get_quota_usage(manager_after_use, ResourceType::CPU) {
    Some(usage) => {
      assert_eq(usage.used, 500.0)
      assert_eq(usage.available, 500.0)
      assert_eq(usage.utilization, 0.5)
    }
    None => assert_true(false)
  }
  
  // Use more resources (exceed CPU quota)
  let manager_after_exceed = use_resource(manager_after_use, ResourceType::CPU, 600.0)
  
  // Check for alerts
  let alerts = check_alerts(manager_after_exceed)
  assert_eq(alerts.length(), 1)
  assert_true(alerts[0].contains("Resource quota exceeded"))
  
  // Check CPU usage after exceeding quota
  match get_quota_usage(manager_after_exceed, ResourceType::CPU) {
    Some(usage) => {
      assert_eq(usage.used, 1100.0)  // 500 + 600
      assert_eq(usage.available, -100.0)  // Over limit
      assert_eq(usage.utilization, 1.1)  // 110% utilization
    }
    None => assert_true(false)
  }
  
  // Release some resources
  let manager_after_release = release_resource(manager_after_exceed, ResourceType::CPU, 200.0)
  
  // Check CPU usage after release
  match get_quota_usage(manager_after_release, ResourceType::CPU) {
    Some(usage) => {
      assert_eq(usage.used, 900.0)  // 1100 - 200
      assert_eq(usage.available, 100.0)
      assert_eq(usage.utilization, 0.9)
    }
    None => assert_true(false)
  }
  
  // Clear alerts
  let manager_after_clear = clear_alerts(manager_after_release)
  let cleared_alerts = check_alerts(manager_after_clear)
  assert_eq(cleared_alerts.length(), 0)
}

// Test 4: Resource Scheduling and Allocation
test "resource scheduling and allocation" {
  // Define resource requirement
  type ResourceRequirement = {
    cpu: Float,
    memory: Float,
    disk: Float,
    network: Float
  }
  
  // Define task
  type Task = {
    id: String,
    name: String,
    priority: Int,
    resource_requirement: ResourceRequirement,
    status: String
  }
  
  // Define node
  type Node = {
    id: String,
    name: String,
    total_resources: ResourceRequirement,
    used_resources: ResourceRequirement,
    available_resources: ResourceRequirement,
    tasks: Array[String>
  }
  
  // Define scheduler
  type Scheduler = {
    nodes: Array[Node>,
    pending_tasks: Array[Task>,
    scheduled_tasks: Array[Task>
  }
  
  // Create scheduler
  let create_scheduler = fn() {
    { nodes: [], pending_tasks: [], scheduled_tasks: [] }
  }
  
  // Add node
  let add_node = fn(scheduler: Scheduler, node: Node) {
    { scheduler | nodes: scheduler.nodes.push(node) }
  }
  
  // Calculate available resources
  let calculate_available_resources = fn(total: ResourceRequirement, used: ResourceRequirement) {
    {
      cpu: total.cpu - used.cpu,
      memory: total.memory - used.memory,
      disk: total.disk - used.disk,
      network: total.network - used.network
    }
  }
  
  // Check if node can accommodate task
  let can_accommodate = fn(node: Node, requirement: ResourceRequirement) {
    requirement.cpu <= node.available_resources.cpu and
    requirement.memory <= node.available_resources.memory and
    requirement.disk <= node.available_resources.disk and
    requirement.network <= node.available_resources.network
  }
  
  // Find best node for task
  let find_best_node = fn(scheduler: Scheduler, requirement: ResourceRequirement) {
    let suitable_nodes = scheduler.nodes.filter(fn(node) { 
      can_accommodate(node, requirement) 
    })
    
    if suitable_nodes.length() > 0 {
      // Sort by available resources (prefer nodes with more available resources)
      let sorted_nodes = suitable_nodes.sort(fn(a, b) { 
        let a_total = a.available_resources.cpu + a.available_resources.memory + 
                     a.available_resources.disk + a.available_resources.network
        let b_total = b.available_resources.cpu + b.available_resources.memory + 
                     b.available_resources.disk + b.available_resources.network
        a_total >= b_total
      })
      
      Some(sorted_nodes[0])
    } else {
      None
    }
  }
  
  // Schedule task
  let schedule_task = fn(scheduler: Scheduler, task: Task) {
    match find_best_node(scheduler, task.resource_requirement) {
      Some(node) => {
        // Update node resources
        let new_used_resources = {
          cpu: node.used_resources.cpu + task.resource_requirement.cpu,
          memory: node.used_resources.memory + task.resource_requirement.memory,
          disk: node.used_resources.disk + task.resource_requirement.disk,
          network: node.used_resources.network + task.resource_requirement.network
        }
        
        let new_available_resources = calculate_available_resources(
          node.total_resources, 
          new_used_resources
        )
        
        let updated_node = {
          id: node.id,
          name: node.name,
          total_resources: node.total_resources,
          used_resources: new_used_resources,
          available_resources: new_available_resources,
          tasks: node.tasks.push(task.id)
        }
        
        // Update scheduler
        let updated_nodes = scheduler.nodes.map(fn(n) {
          if n.id == node.id { updated_node } else { n }
        })
        
        let updated_task = { task | status: "scheduled" }
        
        {
          nodes: updated_nodes,
          pending_tasks: scheduler.pending_tasks.filter(fn(t) { t.id != task.id }),
          scheduled_tasks: scheduler.scheduled_tasks.push(updated_task)
        }
      }
      None => {
        // Cannot schedule, keep in pending
        { scheduler | pending_tasks: scheduler.pending_tasks.push(task) }
      }
    }
  }
  
  // Submit task
  let submit_task = fn(scheduler: Scheduler, task: Task) {
    schedule_task(scheduler, task)
  }
  
  // Complete task
  let complete_task = fn(scheduler: Scheduler, task_id: String) {
    // Find the task
    match scheduler.scheduled_tasks.find(fn(t) { t.id == task_id }) {
      Some(task) => {
        // Find the node running the task
        match scheduler.nodes.find(fn(n) { n.tasks.contains(task_id) }) {
          Some(node) => {
            // Update node resources
            let new_used_resources = {
              cpu: node.used_resources.cpu - task.resource_requirement.cpu,
              memory: node.used_resources.memory - task.resource_requirement.memory,
              disk: node.used_resources.disk - task.resource_requirement.disk,
              network: node.used_resources.network - task.resource_requirement.network
            }
            
            let new_available_resources = calculate_available_resources(
              node.total_resources, 
              new_used_resources
            )
            
            let updated_node = {
              id: node.id,
              name: node.name,
              total_resources: node.total_resources,
              used_resources: new_used_resources,
              available_resources: new_available_resources,
              tasks: node.tasks.filter(fn(t) { t != task_id })
            }
            
            // Update scheduler
            let updated_nodes = scheduler.nodes.map(fn(n) {
              if n.id == node.id { updated_node } else { n }
            })
            
            let completed_task = { task | status: "completed" }
            
            {
              nodes: updated_nodes,
              pending_tasks: scheduler.pending_tasks,
              scheduled_tasks: scheduler.scheduled_tasks.filter(fn(t) { t.id != task_id })
            }
          }
          None => scheduler
        }
      }
      None => scheduler
    }
  }
  
  // Create scheduler
  let scheduler = create_scheduler()
  
  // Add nodes
  let node1 = {
    id: "node-1",
    name: "worker-node-1",
    total_resources: { cpu: 2000.0, memory: 4096.0, disk: 10240.0, network: 100.0 },
    used_resources: { cpu: 0.0, memory: 0.0, disk: 0.0, network: 0.0 },
    available_resources: { cpu: 2000.0, memory: 4096.0, disk: 10240.0, network: 100.0 },
    tasks: []
  }
  
  let node2 = {
    id: "node-2",
    name: "worker-node-2",
    total_resources: { cpu: 1000.0, memory: 2048.0, disk: 5120.0, network: 50.0 },
    used_resources: { cpu: 0.0, memory: 0.0, disk: 0.0, network: 0.0 },
    available_resources: { cpu: 1000.0, memory: 2048.0, disk: 5120.0, network: 50.0 },
    tasks: []
  }
  
  let scheduler_with_nodes = scheduler
    |> add_node(node1)
    |> add_node(node2)
  
  assert_eq(scheduler_with_nodes.nodes.length(), 2)
  
  // Submit tasks
  let task1 = {
    id: "task-1",
    name: "data-processing",
    priority: 1,
    resource_requirement: { cpu: 500.0, memory: 1024.0, disk: 2048.0, network: 10.0 },
    status: "pending"
  }
  
  let task2 = {
    id: "task-2",
    name: "web-server",
    priority: 2,
    resource_requirement: { cpu: 200.0, memory: 512.0, disk: 1024.0, network: 20.0 },
    status: "pending"
  }
  
  let task3 = {
    id: "task-3",
    name: "batch-job",
    priority: 1,
    resource_requirement: { cpu: 1500.0, memory: 3072.0, disk: 4096.0, network: 30.0 },
    status: "pending"
  }
  
  let scheduler_with_tasks = scheduler_with_nodes
    |> submit_task(task1)
    |> submit_task(task2)
    |> submit_task(task3)
  
  // Check task scheduling
  assert_eq(scheduler_with_tasks.scheduled_tasks.length(), 3)
  assert_eq(scheduler_with_tasks.pending_tasks.length(), 0)
  
  // Check node resource usage
  let node1_after = scheduler_with_tasks.nodes.filter(fn(n) { n.id == "node-1" })[0]
  let node2_after = scheduler_with_tasks.nodes.filter(fn(n) { n.id == "node-2" })[0]
  
  // Task 3 should be scheduled on node-1 (larger capacity)
  assert_true(node1_after.tasks.contains("task-3"))
  assert_eq(node1_after.used_resources.cpu, 1500.0)
  
  // Task 1 and 2 should be scheduled on node-2
  assert_true(node2_after.tasks.contains("task-1"))
  assert_true(node2_after.tasks.contains("task-2"))
  assert_eq(node2_after.used_resources.cpu, 700.0)
  
  // Complete task
  let scheduler_after_complete = complete_task(scheduler_with_tasks, "task-1")
  
  // Check node resource usage after completion
  let node2_after_complete = scheduler_after_complete.nodes.filter(fn(n) { n.id == "node-2" })[0]
  assert_false(node2_after_complete.tasks.contains("task-1"))
  assert_eq(node2_after_complete.used_resources.cpu, 500.0)
  
  // Submit a task that cannot be scheduled
  let large_task = {
    id: "task-large",
    name: "large-computation",
    priority: 1,
    resource_requirement: { cpu: 3000.0, memory: 8192.0, disk: 20480.0, network: 200.0 },
    status: "pending"
  }
  
  let scheduler_with_large = submit_task(scheduler_after_complete, large_task)
  
  // Task should remain pending
  assert_eq(scheduler_with_large.pending_tasks.length(), 1)
  assert_eq(scheduler_with_large.pending_tasks[0].id, "task-large")
}

// Test 5: Resource Monitoring and Metrics
test "resource monitoring and metrics" {
  // Define metric type
  enum MetricType {
    Counter
    Gauge
    Histogram
  }
  
  // Define metric
  type Metric = {
    name: String,
    metric_type: MetricType,
    value: Float,
    labels: Array[(String, String)],
    timestamp: Int
  }
  
  // Define resource monitor
  type ResourceMonitor = {
    metrics: Array[Metric>,
    alerts: Array[String],
    thresholds: Array[(String, Float)]
  }
  
  // Create resource monitor
  let create_resource_monitor = fn() {
    { metrics: [], alerts: [], thresholds: [] }
  }
  
  // Add threshold
  let add_threshold = fn(monitor: ResourceMonitor, metric_name: String, threshold: Float) {
    { monitor | thresholds: monitor.thresholds.push((metric_name, threshold)) }
  }
  
  // Record metric
  let record_metric = fn(monitor: ResourceMonitor, name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)>, timestamp: Int) {
    let metric = {
      name,
      metric_type,
      value,
      labels,
      timestamp
    }
    
    // Check threshold
    let threshold_option = monitor.thresholds.find(fn(t) { t.0 == name })
    let new_alerts = match threshold_option {
      Some((_, threshold)) => {
        if value > threshold {
          monitor.alerts.push("Alert: " + name + " exceeded threshold " + threshold.to_string())
        } else {
          monitor.alerts
        }
      }
      None => monitor.alerts
    }
    
    {
      metrics: monitor.metrics.push(metric),
      alerts: new_alerts,
      thresholds: monitor.thresholds
    }
  }
  
  // Get metrics by name
  let get_metrics_by_name = fn(monitor: ResourceMonitor, name: String) {
    monitor.metrics.filter(fn(m) { m.name == name })
  }
  
  // Get latest metric value
  let get_latest_metric = fn(monitor: ResourceMonitor, name: String) {
    let metrics = get_metrics_by_name(monitor, name)
    if metrics.length() > 0 {
      let sorted_metrics = metrics.sort(fn(a, b) { a.timestamp >= b.timestamp })
      Some(sorted_metrics[0])
    } else {
      None
    }
  }
  
  // Calculate average metric value
  let calculate_average = fn(monitor: ResourceMonitor, name: String, time_range: (Int, Int)) {
    let filtered_metrics = monitor.metrics.filter(fn(m) { 
      m.name == name and m.timestamp >= time_range.0 and m.timestamp <= time_range.1
    })
    
    if filtered_metrics.length() > 0 {
      let sum = filtered_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
      Some(sum / filtered_metrics.length().to_float())
    } else {
      None
    }
  }
  
  // Calculate percentile
  let calculate_percentile = fn(monitor: ResourceMonitor, name: String, percentile: Float, time_range: (Int, Int)) {
    let filtered_metrics = monitor.metrics.filter(fn(m) { 
      m.name == name and m.timestamp >= time_range.0 and m.timestamp <= time_range.1
    })
    
    if filtered_metrics.length() > 0 {
      let sorted_values = filtered_metrics.map(fn(m) { m.value }).sort(fn(a, b) { a <= b })
      let index = ((sorted_values.length().to_float() * percentile) / 100.0).to_int().min(sorted_values.length() - 1)
      Some(sorted_values[index])
    } else {
      None
    }
  }
  
  // Get alerts
  let get_alerts = fn(monitor: ResourceMonitor) {
    monitor.alerts
  }
  
  // Clear alerts
  let clear_alerts = fn(monitor: ResourceMonitor) {
    { monitor | alerts: [] }
  }
  
  // Create resource monitor
  let monitor = create_resource_monitor()
  
  // Add thresholds
  let monitor_with_thresholds = monitor
    |> add_threshold("cpu_usage", 80.0)
    |> add_threshold("memory_usage", 90.0)
    |> add_threshold("disk_usage", 85.0)
    |> add_threshold("network_usage", 75.0)
  
  assert_eq(monitor_with_thresholds.thresholds.length(), 4)
  
  // Record metrics
  let monitor_with_metrics = monitor_with_thresholds
    |> record_metric("cpu_usage", MetricType::Gauge, 45.5, [("service", "user-service")], 1640995200)
    |> record_metric("memory_usage", MetricType::Gauge, 60.2, [("service", "user-service")], 1640995200)
    |> record_metric("disk_usage", MetricType::Gauge, 30.0, [("service", "user-service")], 1640995200)
    |> record_metric("network_usage", MetricType::Gauge, 25.5, [("service", "user-service")], 1640995200)
    |> record_metric("cpu_usage", MetricType::Gauge, 85.0, [("service", "user-service")], 1640995260)
    |> record_metric("memory_usage", MetricType::Gauge, 92.0, [("service", "user-service")], 1640995260)
    |> record_metric("request_count", MetricType::Counter, 1000.0, [("service", "user-service")], 1640995200)
    |> record_metric("response_time", MetricType::Histogram, 150.0, [("service", "user-service")], 1640995200)
  
  assert_eq(monitor_with_metrics.metrics.length(), 8)
  
  // Check for alerts
  let alerts = get_alerts(monitor_with_metrics)
  assert_eq(alerts.length(), 2)  // CPU and memory exceeded thresholds
  
  // Get metrics by name
  let cpu_metrics = get_metrics_by_name(monitor_with_metrics, "cpu_usage")
  assert_eq(cpu_metrics.length(), 2)
  
  // Get latest metric
  match get_latest_metric(monitor_with_metrics, "cpu_usage") {
    Some(metric) => {
      assert_eq(metric.value, 85.0)
      assert_eq(metric.timestamp, 1640995260)
    }
    None => assert_true(false)
  }
  
  // Calculate average
  match calculate_average(monitor_with_metrics, "cpu_usage", (1640995200, 1640995300)) {
    Some(average) => assert_eq(average, 65.25)  // (45.5 + 85.0) / 2
    None => assert_true(false)
  }
  
  // Calculate percentile
  match calculate_percentile(monitor_with_metrics, "cpu_usage", 90.0, (1640995200, 1640995300)) {
    Some(percentile) => assert_eq(percentile, 85.0)  // 90th percentile is the higher value
    None => assert_true(false)
  }
  
  // Clear alerts
  let monitor_after_clear = clear_alerts(monitor_with_metrics)
  let cleared_alerts = get_alerts(monitor_after_clear)
  assert_eq(cleared_alerts.length(), 0)
}