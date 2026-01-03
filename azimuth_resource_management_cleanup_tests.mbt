// Azimuth 资源管理和清理测试
// 测试系统对各种资源的正确管理和及时清理

// 测试1: 内存资源管理
test "内存资源管理" {
  // 定义内存资源类型
  type MemoryResource = {
    id: String,
    size_bytes: Int,
    allocated: Bool,
    allocation_time: Int
  }
  
  // 定义内存池
  type MemoryPool = {
    total_capacity: Int,
    used_capacity: Int,
    resources: Array[MemoryResource>
  }
  
  // 创建内存池
  let create_memory_pool = fn(total_capacity: Int) -> MemoryPool {
    {
      total_capacity: total_capacity,
      used_capacity: 0,
      resources: []
    }
  }
  
  // 分配内存资源
  let allocate_memory = fn(pool: MemoryPool, id: String, size_bytes: Int) -> MemoryPool {
    // 检查是否有足够容量
    if pool.used_capacity + size_bytes > pool.total_capacity {
      return pool  // 分配失败
    }
    
    // 创建新资源
    let resource = {
      id: id,
      size_bytes: size_bytes,
      allocated: true,
      allocation_time: Time::now()
    }
    
    {
      total_capacity: pool.total_capacity,
      used_capacity: pool.used_capacity + size_bytes,
      resources: pool.resources.push(resource)
    }
  }
  
  // 释放内存资源
  let free_memory = fn(pool: MemoryPool, id: String) -> MemoryPool {
    // 查找资源
    match pool.resources.find(fn(r) { r.id == id && r.allocated }) {
      Some(resource) => {
        // 移除资源或标记为未分配
        let updated_resources = pool.resources.map(fn(r) {
          if r.id == id {
            { id: r.id, size_bytes: r.size_bytes, allocated: false, allocation_time: r.allocation_time }
          } else {
            r
          }
        })
        
        {
          total_capacity: pool.total_capacity,
          used_capacity: pool.used_capacity - resource.size_bytes,
          resources: updated_resources
        }
      }
      None => pool  // 资源不存在或已释放
    }
  }
  
  // 创建内存池
  let mut memory_pool = create_memory_pool(1024 * 1024)  // 1MB
  
  // 分配多个内存资源
  memory_pool = allocate_memory(memory_pool, "buffer1", 1024)      // 1KB
  memory_pool = allocate_memory(memory_pool, "buffer2", 2048)      // 2KB
  memory_pool = allocate_memory(memory_pool, "buffer3", 4096)      // 4KB
  memory_pool = allocate_memory(memory_pool, "large_buffer", 10240) // 10KB
  
  // 验证内存分配
  assert_eq(memory_pool.used_capacity, 1024 + 2048 + 4096 + 10240)  // 总共17KB
  assert_eq(memory_pool.resources.length(), 4)
  
  // 验证所有资源都已分配
  assert_true(memory_pool.resources.all(fn(r) { r.allocated }))
  
  // 释放部分资源
  memory_pool = free_memory(memory_pool, "buffer2")
  memory_pool = free_memory(memory_pool, "large_buffer")
  
  // 验证内存释放
  assert_eq(memory_pool.used_capacity, 1024 + 4096)  // 剩余5KB
  
  // 验证资源状态
  let buffer1 = memory_pool.resources.find(fn(r) { r.id == "buffer1" })
  let buffer2 = memory_pool.resources.find(fn(r) { r.id == "buffer2" })
  let buffer3 = memory_pool.resources.find(fn(r) { r.id == "buffer3" })
  let large_buffer = memory_pool.resources.find(fn(r) { r.id == "large_buffer" })
  
  match buffer1 {
    Some(r) => assert_true(r.allocated)
    None => assert_true(false)
  }
  
  match buffer2 {
    Some(r) => assert_false(r.allocated)
    None => assert_true(false)
  }
  
  match buffer3 {
    Some(r) => assert_true(r.allocated)
    None => assert_true(false)
  }
  
  match large_buffer {
    Some(r) => assert_false(r.allocated)
    None => assert_true(false)
  }
  
  // 测试内存泄漏检测
  let memory_leak_test = fn() -> Bool {
    let mut test_pool = create_memory_pool(1024 * 1024)
    
    // 分配和释放资源
    test_pool = allocate_memory(test_pool, "test1", 1024)
    test_pool = allocate_memory(test_pool, "test2", 2048)
    
    // 释放部分资源
    test_pool = free_memory(test_pool, "test1")
    
    // 检查是否有未释放的资源
    let leaked_resources = test_pool.resources.filter(fn(r) { r.allocated })
    
    // 应该只有一个资源未释放(test2)
    leaked_resources.length() == 1 && leaked_resources[0].id == "test2"
  }
  
  // 验证内存泄漏检测
  assert_true(memory_leak_test())
}

// 测试2: 文件句柄管理
test "文件句柄管理" {
  // 定义文件句柄类型
  type FileHandle = {
    id: String,
    path: String,
    mode: String,  // "read", "write", "append"
    open: Bool,
    open_time: Int
  }
  
  // 定义文件句柄管理器
  type FileHandleManager = {
    max_handles: Int,
    open_handles: Array[FileHandle>
  }
  
  // 创建文件句柄管理器
  let create_file_manager = fn(max_handles: Int) -> FileHandleManager {
    {
      max_handles: max_handles,
      open_handles: []
    }
  }
  
  // 打开文件
  let open_file = fn(manager: FileHandleManager, id: String, path: String, mode: String) -> FileHandleManager {
    // 检查是否已达到最大句柄数
    if manager.open_handles.length() >= manager.max_handles {
      return manager  // 无法打开更多文件
    }
    
    // 检查文件是否已经打开
    let already_open = manager.open_handles.any(fn(h) { h.path == path && h.open })
    if already_open {
      return manager  // 文件已打开
    }
    
    // 创建新文件句柄
    let handle = {
      id: id,
      path: path,
      mode: mode,
      open: true,
      open_time: Time::now()
    }
    
    {
      max_handles: manager.max_handles,
      open_handles: manager.open_handles.push(handle)
    }
  }
  
  // 关闭文件
  let close_file = fn(manager: FileHandleManager, id: String) -> FileHandleManager {
    // 查找并关闭文件句柄
    let updated_handles = manager.open_handles.map(fn(h) {
      if h.id == id {
        { id: h.id, path: h.path, mode: h.mode, open: false, open_time: h.open_time }
      } else {
        h
      }
    })
    
    {
      max_handles: manager.max_handles,
      open_handles: updated_handles
    }
  }
  
  // 关闭所有文件
  let close_all_files = fn(manager: FileHandleManager) -> FileHandleManager {
    let closed_handles = manager.open_handles.map(fn(h) {
      { id: h.id, path: h.path, mode: h.mode, open: false, open_time: h.open_time }
    })
    
    {
      max_handles: manager.max_handles,
      open_handles: closed_handles
    }
  }
  
  // 创建文件管理器
  let mut file_manager = create_file_manager(5)
  
  // 打开多个文件
  file_manager = open_file(file_manager, "handle1", "/tmp/file1.txt", "read")
  file_manager = open_file(file_manager, "handle2", "/tmp/file2.txt", "write")
  file_manager = open_file(file_manager, "handle3", "/tmp/file3.txt", "read")
  
  // 验证文件打开
  assert_eq(file_manager.open_handles.length(), 3)
  
  // 验证所有文件都已打开
  assert_true(file_manager.open_handles.all(fn(h) { h.open }))
  
  // 关闭部分文件
  file_manager = close_file(file_manager, "handle2")
  
  // 验证文件关闭
  let handle1 = file_manager.open_handles.find(fn(h) { h.id == "handle1" })
  let handle2 = file_manager.open_handles.find(fn(h) { h.id == "handle2" })
  let handle3 = file_manager.open_handles.find(fn(h) { h.id == "handle3" })
  
  match handle1 {
    Some(h) => assert_true(h.open)
    None => assert_true(false)
  }
  
  match handle2 {
    Some(h) => assert_false(h.open)
    None => assert_true(false)
  }
  
  match handle3 {
    Some(h) => assert_true(h.open)
    None => assert_true(false)
  }
  
  // 尝试打开已打开的文件
  let original_count = file_manager.open_handles.filter(fn(h) { h.open }).length()
  file_manager = open_file(file_manager, "handle4", "/tmp/file1.txt", "write")  // 同一路径
  let new_count = file_manager.open_handles.filter(fn(h) { h.open }).length()
  
  // 应该没有新文件被打开
  assert_eq(original_count, new_count)
  
  // 测试最大句柄数限制
  file_manager = open_file(file_manager, "handle4", "/tmp/file4.txt", "read")
  file_manager = open_file(file_manager, "handle5", "/tmp/file5.txt", "read")
  
  // 现在应该有4个打开的文件(handle1, handle3, handle4, handle5)
  assert_eq(file_manager.open_handles.filter(fn(h) { h.open }).length(), 4)
  
  // 尝试打开第6个文件(应该失败)
  let before_open = file_manager.open_handles.length()
  file_manager = open_file(file_manager, "handle6", "/tmp/file6.txt", "read")
  let after_open = file_manager.open_handles.length()
  
  // 应该没有新文件被打开
  assert_eq(before_open, after_open)
  
  // 关闭所有文件
  file_manager = close_all_files(file_manager)
  
  // 验证所有文件都已关闭
  assert_true(file_manager.open_handles.all(fn(h) { !h.open }))
}

// 测试3: 网络连接管理
test "网络连接管理" {
  // 定义网络连接类型
  type NetworkConnection = {
    id: String,
    remote_address: String,
    port: Int,
    protocol: String,  // "tcp", "udp"
    connected: Bool,
    connect_time: Int,
    last_activity: Int
  }
  
  // 定义连接池
  type ConnectionPool = {
    max_connections: Int,
    connection_timeout_ms: Int,
    connections: Array[NetworkConnection>
  }
  
  // 创建连接池
  let create_connection_pool = fn(max_connections: Int, timeout_ms: Int) -> ConnectionPool {
    {
      max_connections: max_connections,
      connection_timeout_ms: timeout_ms,
      connections: []
    }
  }
  
  // 建立连接
  let establish_connection = fn(
    pool: ConnectionPool, 
    id: String, 
    address: String, 
    port: Int, 
    protocol: String
  ) -> ConnectionPool {
    // 检查是否已达到最大连接数
    if pool.connections.filter(fn(c) { c.connected }).length() >= pool.max_connections {
      return pool  // 无法建立更多连接
    }
    
    // 检查是否已有到同一地址的连接
    let already_connected = pool.connections.any(fn(c) { 
      c.remote_address == address && c.port == port && c.connected 
    })
    if already_connected {
      return pool  // 已有连接
    }
    
    // 创建新连接
    let current_time = Time::now()
    let connection = {
      id: id,
      remote_address: address,
      port: port,
      protocol: protocol,
      connected: true,
      connect_time: current_time,
      last_activity: current_time
    }
    
    {
      max_connections: pool.max_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      connections: pool.connections.push(connection)
    }
  }
  
  // 关闭连接
  let close_connection = fn(pool: ConnectionPool, id: String) -> ConnectionPool {
    let updated_connections = pool.connections.map(fn(c) {
      if c.id == id {
        { 
          id: c.id, 
          remote_address: c.remote_address, 
          port: c.port, 
          protocol: c.protocol, 
          connected: false, 
          connect_time: c.connect_time, 
          last_activity: c.last_activity 
        }
      } else {
        c
      }
    })
    
    {
      max_connections: pool.max_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      connections: updated_connections
    }
  }
  
  // 更新连接活动时间
  let update_activity = fn(pool: ConnectionPool, id: String) -> ConnectionPool {
    let current_time = Time::now()
    let updated_connections = pool.connections.map(fn(c) {
      if c.id == id && c.connected {
        { 
          id: c.id, 
          remote_address: c.remote_address, 
          port: c.port, 
          protocol: c.protocol, 
          connected: c.connected, 
          connect_time: c.connect_time, 
          last_activity: current_time 
        }
      } else {
        c
      }
    })
    
    {
      max_connections: pool.max_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      connections: updated_connections
    }
  }
  
  // 清理超时连接
  let cleanup_timeouts = fn(pool: ConnectionPool) -> ConnectionPool {
    let current_time = Time::now()
    let updated_connections = pool.connections.map(fn(c) {
      if c.connected && (current_time - c.last_activity) > pool.connection_timeout_ms {
        { 
          id: c.id, 
          remote_address: c.remote_address, 
          port: c.port, 
          protocol: c.protocol, 
          connected: false, 
          connect_time: c.connect_time, 
          last_activity: c.last_activity 
        }
      } else {
        c
      }
    })
    
    {
      max_connections: pool.max_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      connections: updated_connections
    }
  }
  
  // 创建连接池
  let mut connection_pool = create_connection_pool(3, 5000)  // 最大3个连接，5秒超时
  
  // 建立多个连接
  connection_pool = establish_connection(connection_pool, "conn1", "192.168.1.100", 8080, "tcp")
  connection_pool = establish_connection(connection_pool, "conn2", "192.168.1.101", 8080, "tcp")
  connection_pool = establish_connection(connection_pool, "conn3", "192.168.1.102", 8080, "tcp")
  
  // 验证连接建立
  assert_eq(connection_pool.connections.filter(fn(c) { c.connected }).length(), 3)
  
  // 尝试建立第4个连接(应该失败)
  let before_connect = connection_pool.connections.filter(fn(c) { c.connected }).length()
  connection_pool = establish_connection(connection_pool, "conn4", "192.168.1.103", 8080, "tcp")
  let after_connect = connection_pool.connections.filter(fn(c) { c.connected }).length()
  
  // 应该没有新连接被建立
  assert_eq(before_connect, after_connect)
  
  // 关闭一个连接
  connection_pool = close_connection(connection_pool, "conn2")
  
  // 验证连接关闭
  let conn1 = connection_pool.connections.find(fn(c) { c.id == "conn1" })
  let conn2 = connection_pool.connections.find(fn(c) { c.id == "conn2" })
  let conn3 = connection_pool.connections.find(fn(c) { c.id == "conn3" })
  
  match conn1 {
    Some(c) => assert_true(c.connected)
    None => assert_true(false)
  }
  
  match conn2 {
    Some(c) => assert_false(c.connected)
    None => assert_true(false)
  }
  
  match conn3 {
    Some(c) => assert_true(c.connected)
    None => assert_true(false)
  }
  
  // 现在应该可以建立新连接
  connection_pool = establish_connection(connection_pool, "conn4", "192.168.1.103", 8080, "tcp")
  
  // 验证新连接建立
  let conn4 = connection_pool.connections.find(fn(c) { c.id == "conn4" })
  match conn4 {
    Some(c) => assert_true(c.connected)
    None => assert_true(false)
  }
  
  // 更新连接活动
  connection_pool = update_activity(connection_pool, "conn1")
  
  // 验证活动时间更新
  let updated_conn1 = connection_pool.connections.find(fn(c) { c.id == "conn1" })
  match updated_conn1 {
    Some(c) => assert_true(c.last_activity > c.connect_time)
    None => assert_true(false)
  }
  
  // 测试超时清理(模拟超时)
  let current_time = Time::now()
  
  // 创建一个模拟超时的连接池
  let timeout_test_pool = {
    max_connections: connection_pool.max_connections,
    connection_timeout_ms: 1,  // 1ms超时
    connections: connection_pool.connections.map(fn(c) {
      if c.connected {
        { 
          id: c.id, 
          remote_address: c.remote_address, 
          port: c.port, 
          protocol: c.protocol, 
          connected: c.connected, 
          connect_time: current_time - 100,  // 100ms前连接
          last_activity: current_time - 50   // 50ms前活动
        }
      } else {
        c
      }
    })
  }
  
  // 执行超时清理
  let cleaned_pool = cleanup_timeouts(timeout_test_pool)
  
  // 验证超时连接被关闭
  let active_connections = cleaned_pool.connections.filter(fn(c) { c.connected })
  assert_eq(active_connections.length(), 0)  // 所有连接都应该超时
}

// 测试4: 数据库连接管理
test "数据库连接管理" {
  // 定义数据库连接类型
  type DatabaseConnection = {
    id: String,
    database: String,
    host: String,
    port: Int,
    username: String,
    connected: Bool,
    in_use: Bool,
    connect_time: Int,
    last_used: Int
  }
  
  // 定义连接池
  type DatabaseConnectionPool = {
    max_connections: Int,
    min_connections: Int,
    connection_timeout_ms: Int,
    idle_timeout_ms: Int,
    connections: Array[DatabaseConnection>
  }
  
  // 创建数据库连接池
  let create_db_pool = fn(
    max_connections: Int, 
    min_connections: Int, 
    connection_timeout_ms: Int, 
    idle_timeout_ms: Int
  ) -> DatabaseConnectionPool {
    {
      max_connections: max_connections,
      min_connections: min_connections,
      connection_timeout_ms: connection_timeout_ms,
      idle_timeout_ms: idle_timeout_ms,
      connections: []
    }
  }
  
  // 初始化连接池
  let initialize_pool = fn(pool: DatabaseConnectionPool, database: String, host: String, port: Int, username: String) -> DatabaseConnectionPool {
    let current_time = Time::now()
    let mut connections = []
    
    // 创建最小数量的连接
    for i = 0; i < pool.min_connections; i = i + 1 {
      let connection = {
        id: "db_conn_" + i.to_string(),
        database: database,
        host: host,
        port: port,
        username: username,
        connected: true,
        in_use: false,
        connect_time: current_time,
        last_used: current_time
      }
      connections = connections.push(connection)
    }
    
    {
      max_connections: pool.max_connections,
      min_connections: pool.min_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      idle_timeout_ms: pool.idle_timeout_ms,
      connections: connections
    }
  }
  
  // 获取连接
  let get_connection = fn(pool: DatabaseConnectionPool) -> (DatabaseConnectionPool, Option[String]) {
    // 查找可用的连接
    let available_connection = pool.connections.find(fn(c) { c.connected && !c.in_use })
    
    match available_connection {
      Some(conn) => {
        // 标记连接为使用中
        let current_time = Time::now()
        let updated_connections = pool.connections.map(fn(c) {
          if c.id == conn.id {
            { 
              id: c.id, 
              database: c.database, 
              host: c.host, 
              port: c.port, 
              username: c.username, 
              connected: c.connected, 
              in_use: true, 
              connect_time: c.connect_time, 
              last_used: current_time 
            }
          } else {
            c
          }
        })
        
        ({
          max_connections: pool.max_connections,
          min_connections: pool.min_connections,
          connection_timeout_ms: pool.connection_timeout_ms,
          idle_timeout_ms: pool.idle_timeout_ms,
          connections: updated_connections
        }, Some(conn.id))
      }
      None => {
        // 检查是否可以创建新连接
        let active_connections = pool.connections.filter(fn(c) { c.connected }).length()
        if active_connections < pool.max_connections {
          // 创建新连接(简化实现)
          let current_time = Time::now()
          let new_connection = {
            id: "db_conn_new_" + current_time.to_string(),
            database: "test_db",
            host: "localhost",
            port: 5432,
            username: "user",
            connected: true,
            in_use: true,
            connect_time: current_time,
            last_used: current_time
          }
          
          let updated_connections = pool.connections.push(new_connection)
          
          ({
            max_connections: pool.max_connections,
            min_connections: pool.min_connections,
            connection_timeout_ms: pool.connection_timeout_ms,
            idle_timeout_ms: pool.idle_timeout_ms,
            connections: updated_connections
          }, Some(new_connection.id))
        } else {
          // 无法创建更多连接
          (pool, None)
        }
      }
    }
  }
  
  // 释放连接
  let release_connection = fn(pool: DatabaseConnectionPool, connection_id: String) -> DatabaseConnectionPool {
    let current_time = Time::now()
    let updated_connections = pool.connections.map(fn(c) {
      if c.id == connection_id {
        { 
          id: c.id, 
          database: c.database, 
          host: c.host, 
          port: c.port, 
          username: c.username, 
          connected: c.connected, 
          in_use: false, 
          connect_time: c.connect_time, 
          last_used: current_time 
        }
      } else {
        c
      }
    })
    
    {
      max_connections: pool.max_connections,
      min_connections: pool.min_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      idle_timeout_ms: pool.idle_timeout_ms,
      connections: updated_connections
    }
  }
  
  // 清理空闲连接
  let cleanup_idle_connections = fn(pool: DatabaseConnectionPool) -> DatabaseConnectionPool {
    let current_time = Time::now()
    let active_connections = pool.connections.filter(fn(c) { c.connected }).length()
    
    // 确保不低于最小连接数
    let min_connections_to_keep = pool.min_connections
    
    // 找出空闲时间过长的连接
    let idle_connections = pool.connections.filter(fn(c) { 
      c.connected && !c.in_use && (current_time - c.last_used) > pool.idle_timeout_ms 
    })
    
    // 计算可以关闭的连接数
    let connections_to_close = if active_connections - idle_connections.length() >= min_connections_to_keep {
      idle_connections.length()
    } else {
      let excess = active_connections - min_connections_to_keep
      if excess > 0 { excess } else { 0 }
    }
    
    // 关闭空闲连接
    let updated_connections = pool.connections.map(fn(c) {
      if c.connected && !c.in_use && (current_time - c.last_used) > pool.idle_timeout_ms {
        // 检查是否可以关闭此连接
        let can_close = connections_to_close > 0
        if can_close {
          { 
            id: c.id, 
            database: c.database, 
            host: c.host, 
            port: c.port, 
            username: c.username, 
            connected: false, 
            in_use: false, 
            connect_time: c.connect_time, 
            last_used: c.last_used 
          }
        } else {
          c
        }
      } else {
        c
      }
    })
    
    {
      max_connections: pool.max_connections,
      min_connections: pool.min_connections,
      connection_timeout_ms: pool.connection_timeout_ms,
      idle_timeout_ms: pool.idle_timeout_ms,
      connections: updated_connections
    }
  }
  
  // 创建数据库连接池
  let mut db_pool = create_db_pool(5, 2, 5000, 30000)  // 最大5个，最小2个，5秒连接超时，30秒空闲超时
  
  // 初始化连接池
  db_pool = initialize_pool(db_pool, "test_db", "localhost", 5432, "user")
  
  // 验证初始化
  assert_eq(db_pool.connections.filter(fn(c) { c.connected }).length(), 2)
  assert_true(db_pool.connections.all(fn(c) { !c.in_use }))
  
  // 获取连接
  let (new_pool1, conn1_id) = get_connection(db_pool)
  db_pool = new_pool1
  
  let (new_pool2, conn2_id) = get_connection(db_pool)
  db_pool = new_pool2
  
  // 验证连接获取
  match conn1_id {
    Some(id) => {
      let conn = db_pool.connections.find(fn(c) { c.id == id })
      match conn {
        Some(c) => assert_true(c.in_use)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  match conn2_id {
    Some(id) => {
      let conn = db_pool.connections.find(fn(c) { c.id == id })
      match conn {
        Some(c) => assert_true(c.in_use)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 释放连接
  match conn1_id {
    Some(id) => db_pool = release_connection(db_pool, id)
    None => assert_true(false)
  }
  
  // 验证连接释放
  match conn1_id {
    Some(id) => {
      let conn = db_pool.connections.find(fn(c) { c.id == id })
      match conn {
        Some(c) => assert_false(c.in_use)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 获取更多连接(测试连接扩展)
  let (new_pool3, conn3_id) = get_connection(db_pool)
  db_pool = new_pool3
  
  let (new_pool4, conn4_id) = get_connection(db_pool)
  db_pool = new_pool4
  
  let (new_pool5, conn5_id) = get_connection(db_pool)
  db_pool = new_pool5
  
  // 验证连接扩展
  assert_eq(db_pool.connections.filter(fn(c) { c.connected }).length(), 5)  // 应该有5个连接
  
  // 尝试获取第6个连接(应该失败)
  let (new_pool6, conn6_id) = get_connection(db_pool)
  db_pool = new_pool6
  
  assert_eq(conn6_id, None)  // 无法获取更多连接
  
  // 测试空闲连接清理
  let current_time = Time::now()
  
  // 创建一个模拟空闲超时的连接池
  let idle_test_pool = {
    max_connections: db_pool.max_connections,
    min_connections: db_pool.min_connections,
    connection_timeout_ms: db_pool.connection_timeout_ms,
    idle_timeout_ms: 1,  // 1ms空闲超时
    connections: db_pool.connections.map(fn(c) {
      if c.connected && !c.in_use {
        { 
          id: c.id, 
          database: c.database, 
          host: c.host, 
          port: c.port, 
          username: c.username, 
          connected: c.connected, 
          in_use: c.in_use, 
          connect_time: c.connect_time, 
          last_used: current_time - 100  // 100ms前使用
        }
      } else {
        c
      }
    })
  }
  
  // 执行空闲连接清理
  let cleaned_pool = cleanup_idle_connections(idle_test_pool)
  
  // 验证空闲连接被清理，但保留最小连接数
  let active_connections = cleaned_pool.connections.filter(fn(c) { c.connected })
  assert_eq(active_connections.length(), 2)  // 应该只保留最小连接数
}

// 测试5: 资源生命周期管理
test "资源生命周期管理" {
  // 定义资源状态
  enum ResourceState {
    Created
    Initialized
    Active
    Inactive
    Disposed
  }
  
  // 定义资源类型
  type Resource = {
    id: String,
    resource_type: String,
    state: ResourceState,
    created_time: Int,
    initialized_time: Option[Int],
    activated_time: Option[Int>,
    deactivated_time: Option[Int],
    disposed_time: Option[Int>
  }
  
  // 定义资源管理器
  type ResourceManager = {
    resources: Array[Resource>,
    auto_cleanup_enabled: Bool,
    max_lifetime_ms: Int
  }
  
  // 创建资源管理器
  let create_resource_manager = fn(auto_cleanup: Bool, max_lifetime: Int) -> ResourceManager {
    {
      resources: [],
      auto_cleanup_enabled: auto_cleanup,
      max_lifetime_ms: max_lifetime
    }
  }
  
  // 创建资源
  let create_resource = fn(manager: ResourceManager, id: String, resource_type: String) -> ResourceManager {
    let current_time = Time::now()
    let resource = {
      id: id,
      resource_type: resource_type,
      state: ResourceState::Created,
      created_time: current_time,
      initialized_time: None,
      activated_time: None,
      deactivated_time: None,
      disposed_time: None
    }
    
    {
      resources: manager.resources.push(resource),
      auto_cleanup_enabled: manager.auto_cleanup_enabled,
      max_lifetime_ms: manager.max_lifetime_ms
    }
  }
  
  // 初始化资源
  let initialize_resource = fn(manager: ResourceManager, id: String) -> ResourceManager {
    let current_time = Time::now()
    let updated_resources = manager.resources.map(fn(r) {
      if r.id == id && r.state == ResourceState::Created {
        { 
          id: r.id, 
          resource_type: r.resource_type, 
          state: ResourceState::Initialized, 
          created_time: r.created_time, 
          initialized_time: Some(current_time), 
          activated_time: r.activated_time, 
          deactivated_time: r.deactivated_time, 
          disposed_time: r.disposed_time 
        }
      } else {
        r
      }
    })
    
    {
      resources: updated_resources,
      auto_cleanup_enabled: manager.auto_cleanup_enabled,
      max_lifetime_ms: manager.max_lifetime_ms
    }
  }
  
  // 激活资源
  let activate_resource = fn(manager: ResourceManager, id: String) -> ResourceManager {
    let current_time = Time::now()
    let updated_resources = manager.resources.map(fn(r) {
      if r.id == id && r.state == ResourceState::Initialized {
        { 
          id: r.id, 
          resource_type: r.resource_type, 
          state: ResourceState::Active, 
          created_time: r.created_time, 
          initialized_time: r.initialized_time, 
          activated_time: Some(current_time), 
          deactivated_time: r.deactivated_time, 
          disposed_time: r.disposed_time 
        }
      } else {
        r
      }
    })
    
    {
      resources: updated_resources,
      auto_cleanup_enabled: manager.auto_cleanup_enabled,
      max_lifetime_ms: manager.max_lifetime_ms
    }
  }
  
  // 停用资源
  let deactivate_resource = fn(manager: ResourceManager, id: String) -> ResourceManager {
    let current_time = Time::now()
    let updated_resources = manager.resources.map(fn(r) {
      if r.id == id && r.state == ResourceState::Active {
        { 
          id: r.id, 
          resource_type: r.resource_type, 
          state: ResourceState::Inactive, 
          created_time: r.created_time, 
          initialized_time: r.initialized_time, 
          activated_time: r.activated_time, 
          deactivated_time: Some(current_time), 
          disposed_time: r.disposed_time 
        }
      } else {
        r
      }
    })
    
    {
      resources: updated_resources,
      auto_cleanup_enabled: manager.auto_cleanup_enabled,
      max_lifetime_ms: manager.max_lifetime_ms
    }
  }
  
  // 释放资源
  let dispose_resource = fn(manager: ResourceManager, id: String) -> ResourceManager {
    let current_time = Time::now()
    let updated_resources = manager.resources.map(fn(r) {
      if r.id == id && (r.state == ResourceState::Active || r.state == ResourceState::Inactive) {
        { 
          id: r.id, 
          resource_type: r.resource_type, 
          state: ResourceState::Disposed, 
          created_time: r.created_time, 
          initialized_time: r.initialized_time, 
          activated_time: r.activated_time, 
          deactivated_time: r.deactivated_time, 
          disposed_time: Some(current_time) 
        }
      } else {
        r
      }
    })
    
    {
      resources: updated_resources,
      auto_cleanup_enabled: manager.auto_cleanup_enabled,
      max_lifetime_ms: manager.max_lifetime_ms
    }
  }
  
  // 清理过期资源
  let cleanup_expired_resources = fn(manager: ResourceManager) -> ResourceManager {
    if !manager.auto_cleanup_enabled {
      return manager
    }
    
    let current_time = Time::now()
    let updated_resources = manager.resources.map(fn(r) {
      let resource_age = current_time - r.created_time
      if resource_age > manager.max_lifetime_ms && r.state != ResourceState::Disposed {
        { 
          id: r.id, 
          resource_type: r.resource_type, 
          state: ResourceState::Disposed, 
          created_time: r.created_time, 
          initialized_time: r.initialized_time, 
          activated_time: r.activated_time, 
          deactivated_time: r.deactivated_time, 
          disposed_time: Some(current_time) 
        }
      } else {
        r
      }
    })
    
    {
      resources: updated_resources,
      auto_cleanup_enabled: manager.auto_cleanup_enabled,
      max_lifetime_ms: manager.max_lifetime_ms
    }
  }
  
  // 创建资源管理器
  let mut resource_manager = create_resource_manager(true, 10000)  // 启用自动清理，10秒最大生命周期
  
  // 创建资源
  resource_manager = create_resource(resource_manager, "resource1", "database_connection")
  resource_manager = create_resource(resource_manager, "resource2", "file_handle")
  resource_manager = create_resource(resource_manager, "resource3", "network_socket")
  
  // 验证资源创建
  assert_eq(resource_manager.resources.length(), 3)
  assert_true(resource_manager.resources.all(fn(r) { r.state == ResourceState::Created }))
  
  // 初始化资源
  resource_manager = initialize_resource(resource_manager, "resource1")
  resource_manager = initialize_resource(resource_manager, "resource2")
  
  // 验证资源初始化
  let resource1 = resource_manager.resources.find(fn(r) { r.id == "resource1" })
  let resource2 = resource_manager.resources.find(fn(r) { r.id == "resource2" })
  let resource3 = resource_manager.resources.find(fn(r) { r.id == "resource3" })
  
  match resource1 {
    Some(r) => assert_eq(r.state, ResourceState::Initialized)
    None => assert_true(false)
  }
  
  match resource2 {
    Some(r) => assert_eq(r.state, ResourceState::Initialized)
    None => assert_true(false)
  }
  
  match resource3 {
    Some(r) => assert_eq(r.state, ResourceState::Created)
    None => assert_true(false)
  }
  
  // 激活资源
  resource_manager = activate_resource(resource_manager, "resource1")
  
  // 验证资源激活
  match resource_manager.resources.find(fn(r) { r.id == "resource1" }) {
    Some(r) => assert_eq(r.state, ResourceState::Active)
    None => assert_true(false)
  }
  
  // 停用资源
  resource_manager = deactivate_resource(resource_manager, "resource1")
  
  // 验证资源停用
  match resource_manager.resources.find(fn(r) { r.id == "resource1" }) {
    Some(r) => assert_eq(r.state, ResourceState::Inactive)
    None => assert_true(false)
  }
  
  // 释放资源
  resource_manager = dispose_resource(resource_manager, "resource1")
  resource_manager = dispose_resource(resource_manager, "resource2")
  
  // 验证资源释放
  match resource_manager.resources.find(fn(r) { r.id == "resource1" }) {
    Some(r) => assert_eq(r.state, ResourceState::Disposed)
    None => assert_true(false)
  }
  
  match resource_manager.resources.find(fn(r) { r.id == "resource2" }) {
    Some(r) => assert_eq(r.state, ResourceState::Disposed)
    None => assert_true(false)
  }
  
  // 测试自动清理
  let current_time = Time::now()
  
  // 创建一个模拟过期的资源管理器
  let expired_test_manager = {
    resources: resource_manager.resources.map(fn(r) {
      if r.id == "resource3" {
        { 
          id: r.id, 
          resource_type: r.resource_type, 
          state: ResourceState::Created, 
          created_time: current_time - 15000,  // 15秒前创建
          initialized_time: None,
          activated_time: None,
          deactivated_time: None,
          disposed_time: None
        }
      } else {
        r
      }
    }),
    auto_cleanup_enabled: true,
    max_lifetime_ms: 10000  // 10秒最大生命周期
  }
  
  // 执行自动清理
  let cleaned_manager = cleanup_expired_resources(expired_test_manager)
  
  // 验证过期资源被自动释放
  match cleaned_manager.resources.find(fn(r) { r.id == "resource3" }) {
    Some(r) => assert_eq(r.state, ResourceState::Disposed)
    None => assert_true(false)
  }
}