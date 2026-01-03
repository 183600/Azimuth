// Azimuth 资源管理测试
// 专注于测试遥测系统的资源分配、使用和释放

// 测试1: 内存资源管理
test "内存资源管理" {
  // 创建内存管理器
  let memory_manager = MemoryManager({
    allocated_objects: [],
    total_allocated: 0,
    max_memory_limit: 10 * 1024 * 1024, // 10MB
    gc_threshold: 8 * 1024 * 1024      // 8MB
  })
  
  // 分配内存对象的函数
  let allocate_memory_object = fn(manager, size, object_id) {
    let current_usage = get_memory_usage(manager)
    
    // 检查是否会超过内存限制
    if current_usage + size > manager.max_memory_limit {
      // 触发垃圾回收
      trigger_garbage_collection(manager)
      
      // 再次检查内存使用
      let new_usage = get_memory_usage(manager)
      if new_usage + size > manager.max_memory_limit {
        return Error("内存不足，无法分配对象")
      }
    }
    
    // 分配对象
    let memory_object = MemoryObject({
      id: object_id,
      size: size,
      data: "data_of_size_" + size.to_string(),
      created_at: get_current_time_ms(),
      last_accessed: get_current_time_ms()
    })
    
    allocate_object(manager, memory_object)
    Success(memory_object)
  }
  
  // 释放内存对象的函数
  let deallocate_memory_object = fn(manager, object_id) {
    deallocate_object(manager, object_id)
  }
  
  // 测试内存分配
  let object1_result = allocate_memory_object(memory_manager, 1024, "obj1")
  let object2_result = allocate_memory_object(memory_manager, 2048, "obj2")
  let object3_result = allocate_memory_object(memory_manager, 4096, "obj3")
  
  // 验证分配成功
  assert_true(object1_result.is_success(), "对象1分配失败")
  assert_true(object2_result.is_success(), "对象2分配失败")
  assert_true(object3_result.is_success(), "对象3分配失败")
  
  // 验证内存使用量
  let current_usage = get_memory_usage(memory_manager)
  let expected_usage = 1024 + 2048 + 4096
  assert_eq(current_usage, expected_usage, 
    "内存使用量计算错误: 期望 " + expected_usage.to_string() + 
    ", 实际 " + current_usage.to_string())
  
  // 测试内存释放
  deallocate_memory_object(memory_manager, "obj2")
  
  // 验证内存释放
  let usage_after_deallocation = get_memory_usage(memory_manager)
  let expected_usage_after_deallocation = 1024 + 4096
  assert_eq(usage_after_deallocation, expected_usage_after_deallocation, 
    "内存释放后使用量错误: 期望 " + expected_usage_after_deallocation.to_string() + 
    ", 实际 " + usage_after_deallocation.to_string())
  
  // 测试垃圾回收阈值
  let large_object_result = allocate_memory_object(memory_manager, 5 * 1024 * 1024, "large_obj")
  assert_true(large_object_result.is_success(), "大对象分配失败，应该触发垃圾回收")
}

// 测试2: 连接池资源管理
test "连接池资源管理" {
  // 创建连接池
  let connection_pool = ConnectionPool({
    max_connections: 10,
    min_connections: 2,
    active_connections: [],
    idle_connections: [],
    total_created: 0,
    total_closed: 0
  })
  
  // 初始化最小连接数
  initialize_connection_pool(connection_pool)
  
  // 获取连接的函数
  let get_connection = fn(pool) {
    // 检查是否有空闲连接
    if pool.idle_connections.length() > 0 {
      // 从空闲连接中取出一个
      let connection = pool.idle_connections[0]
      let remaining_idle = pool.idle_connections.slice(1, pool.idle_connections.length())
      
      // 添加到活跃连接
      let updated_active = pool.active_connections + [connection]
      
      update_connection_pool(pool, {
        active_connections: updated_active,
        idle_connections: remaining_idle
      })
      
      Success(connection)
    } else if pool.active_connections.length() < pool.max_connections {
      // 创建新连接
      let new_connection = create_new_connection(pool)
      let updated_active = pool.active_connections + [new_connection]
      
      update_connection_pool(pool, {
        active_connections: updated_active,
        total_created: pool.total_created + 1
      })
      
      Success(new_connection)
    } else {
      // 连接池已满
      Error("连接池已满，无法获取连接")
    }
  }
  
  // 释放连接的函数
  let release_connection = fn(pool, connection) {
    // 从活跃连接中移除
    let updated_active = pool.active_connections.filter(fn(conn) { 
      conn.id != connection.id 
    })
    
    // 添加到空闲连接
    let updated_idle = pool.idle_connections + [connection]
    
    update_connection_pool(pool, {
      active_connections: updated_active,
      idle_connections: updated_idle
    })
  }
  
  // 验证初始状态
  assert_eq(connection_pool.active_connections.length(), 0, "初始活跃连接数不为0")
  assert_eq(connection_pool.idle_connections.length(), connection_pool.min_connections, 
    "初始空闲连接数不等于最小连接数")
  
  // 测试获取连接
  let conn1_result = get_connection(connection_pool)
  let conn2_result = get_connection(connection_pool)
  let conn3_result = get_connection(connection_pool)
  
  // 验证获取成功
  assert_true(conn1_result.is_success(), "获取连接1失败")
  assert_true(conn2_result.is_success(), "获取连接2失败")
  assert_true(conn3_result.is_success(), "获取连接3失败")
  
  // 验证连接池状态
  assert_eq(connection_pool.active_connections.length(), 3, "活跃连接数不正确")
  assert_eq(connection_pool.idle_connections.length(), 0, "空闲连接数不正确")
  
  // 测试释放连接
  match conn1_result {
    Success(conn1) => release_connection(connection_pool, conn1)
    Error(_) => assert_true(false, "连接1获取失败，无法释放")
  }
  
  // 验证释放后的状态
  assert_eq(connection_pool.active_connections.length(), 2, "释放后活跃连接数不正确")
  assert_eq(connection_pool.idle_connections.length(), 1, "释放后空闲连接数不正确")
  
  // 测试连接池满的情况
  let connections = []
  for i = 0; i < 8; i = i + 1 {
    let conn_result = get_connection(connection_pool)
    match conn_result {
      Success(conn) => connections = connections + [conn]
      Error(_) => assert_true(false, "获取连接失败")
    }
  }
  
  // 现在连接池应该满了
  let full_pool_result = get_connection(connection_pool)
  assert_true(full_pool_result.is_error(), "连接池已满，但仍然成功获取连接")
}

// 测试3: 文件句柄资源管理
test "文件句柄资源管理" {
  // 创建文件句柄管理器
  let file_handle_manager = FileHandleManager({
    max_handles: 100,
    open_handles: [],
    handle_counter: 0
  })
  
  // 打开文件的函数
  let open_file = fn(manager, file_path) {
    if manager.open_handles.length() >= manager.max_handles {
      return Error("文件句柄数已达上限")
    }
    
    let handle_id = manager.handle_counter + 1
    let file_handle = FileHandle({
      id: handle_id,
      path: file_path,
      opened_at: get_current_time_ms(),
      last_accessed: get_current_time_ms(),
      size: 1024, // 模拟文件大小
      is_writable: false
    })
    
    update_file_handle_manager(manager, {
      open_handles: manager.open_handles + [file_handle],
      handle_counter: handle_id
    })
    
    Success(file_handle)
  }
  
  // 关闭文件的函数
  let close_file = fn(manager, handle_id) {
    let updated_handles = manager.open_handles.filter(fn(handle) { 
      handle.id != handle_id 
    })
    
    update_file_handle_manager(manager, {
      open_handles: updated_handles
    })
  }
  
  // 测试打开文件
  let file1_result = open_file(file_handle_manager, "/tmp/test1.txt")
  let file2_result = open_file(file_handle_manager, "/tmp/test2.txt")
  let file3_result = open_file(file_handle_manager, "/tmp/test3.txt")
  
  // 验证打开成功
  assert_true(file1_result.is_success(), "打开文件1失败")
  assert_true(file2_result.is_success(), "打开文件2失败")
  assert_true(file3_result.is_success(), "打开文件3失败")
  
  // 验证文件句柄数量
  assert_eq(file_handle_manager.open_handles.length(), 3, "打开的文件句柄数不正确")
  
  // 测试关闭文件
  match file1_result {
    Success(handle1) => close_file(file_handle_manager, handle1.id)
    Error(_) => assert_true(false, "文件1打开失败，无法关闭")
  }
  
  // 验证关闭后的状态
  assert_eq(file_handle_manager.open_handles.length(), 2, "关闭文件后句柄数不正确")
  
  // 测试句柄数限制
  let handles = []
  for i = 0; i < 98; i = i + 1 {
    let file_result = open_file(file_handle_manager, "/tmp/test" + (i + 4).to_string() + ".txt")
    match file_result {
      Success(handle) => handles = handles + [handle]
      Error(_) => assert_true(false, "打开文件失败")
    }
  }
  
  // 现在应该有100个打开的句柄
  assert_eq(file_handle_manager.open_handles.length(), 100, "文件句柄数不正确")
  
  // 尝试打开第101个文件，应该失败
  let overflow_result = open_file(file_handle_manager, "/tmp/overflow.txt")
  assert_true(overflow_result.is_error(), "文件句柄数已达上限，但仍然成功打开文件")
}

// 测试4: 缓存资源管理
test "缓存资源管理" {
  // 创建缓存管理器
  let cache_manager = CacheManager({
    max_size: 50, // 最大缓存项数
    max_memory: 5 * 1024 * 1024, // 最大内存使用量 5MB
    items: [],
    eviction_policy: "LRU" // 最近最少使用
  })
  
  // 添加缓存项的函数
  let add_cache_item = fn(manager, key, value, size) {
    // 检查是否已存在
    let existing_index = find_cache_item_index(manager, key)
    
    if existing_index >= 0 {
      // 更新现有项
      let updated_items = manager.items.map_with_index(fn(i, item) {
        if i == existing_index {
          CacheItem({
            key: key,
            value: value,
            size: size,
            created_at: item.created_at,
            last_accessed: get_current_time_ms(),
            access_count: item.access_count + 1
          })
        } else {
          item
        }
      })
      
      update_cache_manager(manager, { items: updated_items })
    } else {
      // 检查是否需要驱逐项
      let current_size = get_cache_item_count(manager)
      let current_memory = get_cache_memory_usage(manager)
      
      if current_size >= manager.max_size || current_memory + size > manager.max_memory {
        // 驱逐最少使用的项
        evict_cache_items(manager, size)
      }
      
      // 添加新项
      let new_item = CacheItem({
        key: key,
        value: value,
        size: size,
        created_at: get_current_time_ms(),
        last_accessed: get_current_time_ms(),
        access_count: 1
      })
      
      update_cache_manager(manager, {
        items: manager.items + [new_item]
      })
    }
  }
  
  // 获取缓存项的函数
  let get_cache_item = fn(manager, key) {
    match find_cache_item(manager, key) {
      Some(item) => {
        // 更新访问时间
        update_cache_item_access_time(manager, key)
        Some(item.value)
      }
      None => None
    }
  }
  
  // 测试添加缓存项
  add_cache_item(cache_manager, "key1", "value1", 1024)
  add_cache_item(cache_manager, "key2", "value2", 2048)
  add_cache_item(cache_manager, "key3", "value3", 1536)
  
  // 验证缓存项数量
  assert_eq(cache_manager.items.length(), 3, "缓存项数量不正确")
  
  // 测试获取缓存项
  let value1 = get_cache_item(cache_manager, "key1")
  let value2 = get_cache_item(cache_manager, "key2")
  let value3 = get_cache_item(cache_manager, "key3")
  let value4 = get_cache_item(cache_manager, "key4") // 不存在
  
  // 验证获取结果
  assert_eq(value1, Some("value1"), "获取key1失败")
  assert_eq(value2, Some("value2"), "获取key2失败")
  assert_eq(value3, Some("value3"), "获取key3失败")
  assert_eq(value4, None, "获取不存在的key4应该返回None")
  
  // 测试缓存更新
  add_cache_item(cache_manager, "key1", "new_value1", 1024)
  let updated_value1 = get_cache_item(cache_manager, "key1")
  assert_eq(updated_value1, Some("new_value1"), "更新缓存项失败")
  
  // 测试缓存驱逐
  // 添加更多项直到触发驱逐
  for i = 0; i < 60; i = i + 1 {
    let key = "key" + (i + 10).to_string()
    let value = "value" + (i + 10).to_string()
    add_cache_item(cache_manager, key, value, 1024)
  }
  
  // 验证缓存大小不超过限制
  assert_true(cache_manager.items.length() <= cache_manager.max_size, 
    "缓存项数量超过限制: " + cache_manager.items.length().to_string())
  
  // 验证内存使用不超过限制
  let memory_usage = get_cache_memory_usage(cache_manager)
  assert_true(memory_usage <= cache_manager.max_memory, 
    "缓存内存使用超过限制: " + memory_usage.to_string())
}

// 测试5: 资源泄漏检测
test "资源泄漏检测" {
  // 创建资源泄漏检测器
  let leak_detector = ResourceLeakDetector({
    tracked_resources: [],
    leak_threshold: 100, // 100个资源后开始检测
    detection_interval: 1000 // 1秒检测间隔
  })
  
  // 创建资源的函数
  let create_resource = fn(detector, resource_type, resource_id) {
    let resource = TrackedResource({
      id: resource_id,
      type: resource_type,
      created_at: get_current_time_ms(),
      last_accessed: get_current_time_ms(),
      is_leaked: false
    })
    
    track_resource(detector, resource)
    resource
  }
  
  // 释放资源的函数
  let release_resource = fn(detector, resource_id) {
    untrack_resource(detector, resource_id)
  }
  
  // 检测资源泄漏的函数
  let detect_resource_leaks = fn(detector) {
    let current_time = get_current_time_ms()
    let leak_threshold_ms = 5000 // 5秒未访问视为泄漏
    
    let leaked_resources = detector.tracked_resources.filter(fn(resource) {
      (current_time - resource.last_accessed) > leak_threshold_ms
    })
    
    // 标记泄漏的资源
    let updated_resources = detector.tracked_resources.map(fn(resource) {
      if leaked_resources.any(fn(leaked) { leaked.id == resource.id }) {
        { resource with is_leaked: true }
      } else {
        resource
      }
    })
    
    update_leak_detector(detector, { tracked_resources: updated_resources })
    leaked_resources.length()
  }
  
  // 创建一些资源
  let resource1 = create_resource(leak_detector, "memory", "mem1")
  let resource2 = create_resource(leak_detector, "connection", "conn1")
  let resource3 = create_resource(leak_detector, "file", "file1")
  
  // 验证资源被跟踪
  assert_eq(leak_detector.tracked_resources.length(), 3, "跟踪的资源数量不正确")
  
  // 释放一个资源
  release_resource(leak_detector, resource2.id)
  
  // 验证资源被取消跟踪
  assert_eq(leak_detector.tracked_resources.length(), 2, "释放后跟踪的资源数量不正确")
  
  // 创建更多资源以触发检测
  for i = 0; i < 120; i = i + 1 {
    let resource_id = "resource" + i.to_string()
    create_resource(leak_detector, "test", resource_id)
  }
  
  // 模拟时间流逝（在实际环境中会有真实的时间流逝）
  let old_resources = leak_detector.tracked_resources.map(fn(resource) {
    { resource with last_accessed: resource.last_accessed - 6000 } // 6秒前
  })
  
  update_leak_detector(leak_detector, { tracked_resources: old_resources })
  
  // 检测资源泄漏
  let leaked_count = detect_resource_leaks(leak_detector)
  
  // 验证泄漏检测
  assert_true(leaked_count > 0, "应该检测到资源泄漏")
  
  // 验证泄漏资源被标记
  let leaked_resources = leak_detector.tracked_resources.filter(fn(resource) {
    resource.is_leaked
  })
  
  assert_eq(leaked_resources.length(), leaked_count, "泄漏资源标记数量不正确")
}

// 类型定义（用于测试）
type MemoryManager {
  MemoryManager({
    allocated_objects: Array[MemoryObject],
    total_allocated: Int,
    max_memory_limit: Int,
    gc_threshold: Int
  })
}

type MemoryObject {
  MemoryObject({
    id: String,
    size: Int,
    data: String,
    created_at: Int,
    last_accessed: Int
  })
}

type ConnectionPool {
  ConnectionPool({
    max_connections: Int,
    min_connections: Int,
    active_connections: Array[Connection],
    idle_connections: Array[Connection],
    total_created: Int,
    total_closed: Int
  })
}

type Connection {
  Connection({
    id: String,
    created_at: Int,
    last_used: Int,
    is_active: Bool
  })
}

type FileHandleManager {
  FileHandleManager({
    max_handles: Int,
    open_handles: Array[FileHandle],
    handle_counter: Int
  })
}

type FileHandle {
  FileHandle({
    id: Int,
    path: String,
    opened_at: Int,
    last_accessed: Int,
    size: Int,
    is_writable: Bool
  })
}

type CacheManager {
  CacheManager({
    max_size: Int,
    max_memory: Int,
    items: Array[CacheItem],
    eviction_policy: String
  })
}

type CacheItem {
  CacheItem({
    key: String,
    value: String,
    size: Int,
    created_at: Int,
    last_accessed: Int,
    access_count: Int
  })
}

type ResourceLeakDetector {
  ResourceLeakDetector({
    tracked_resources: Array[TrackedResource],
    leak_threshold: Int,
    detection_interval: Int
  })
}

type TrackedResource {
  TrackedResource({
    id: String,
    type: String,
    created_at: Int,
    last_accessed: Int,
    is_leaked: Bool
  })
}

type Result<T, E> {
  Success(T)
  Error(E)
}

// 辅助函数（用于测试）
let get_memory_usage = fn(manager) {
  match manager {
    MemoryManager(data) => data.total_allocated
  }
}

let trigger_garbage_collection = fn(manager) {
  // 简化的垃圾回收实现
  match manager {
    MemoryManager(data) => {
      // 释放一半的对象
      let objects_to_keep = data.allocated_objects.slice(0, data.allocated_objects.length() / 2)
      let new_allocated = objects_to_keep.fold(0, fn(acc, obj) { acc + obj.size })
      
      MemoryManager({
        allocated_objects: objects_to_keep,
        total_allocated: new_allocated,
        max_memory_limit: data.max_memory_limit,
        gc_threshold: data.gc_threshold
      })
    }
  }
}

let allocate_object = fn(manager, object) {
  match manager {
    MemoryManager(data) => {
      MemoryManager({
        allocated_objects: data.allocated_objects + [object],
        total_allocated: data.total_allocated + object.size,
        max_memory_limit: data.max_memory_limit,
        gc_threshold: data.gc_threshold
      })
    }
  }
}

let deallocate_object = fn(manager, object_id) {
  match manager {
    MemoryManager(data) => {
      let remaining_objects = data.allocated_objects.filter(fn(obj) { obj.id != object_id })
      let new_allocated = remaining_objects.fold(0, fn(acc, obj) { acc + obj.size })
      
      MemoryManager({
        allocated_objects: remaining_objects,
        total_allocated: new_allocated,
        max_memory_limit: data.max_memory_limit,
        gc_threshold: data.gc_threshold
      })
    }
  }
}

let initialize_connection_pool = fn(pool) {
  // 初始化最小连接数
  match pool {
    ConnectionPool(data) => {
      let initial_connections = (0..data.min_connections).map(fn(i) {
        Connection({
          id: "conn" + i.to_string(),
          created_at: get_current_time_ms(),
          last_used: get_current_time_ms(),
          is_active: false
        })
      })
      
      ConnectionPool({
        max_connections: data.max_connections,
        min_connections: data.min_connections,
        active_connections: [],
        idle_connections: initial_connections,
        total_created: data.min_connections,
        total_closed: 0
      })
    }
  }
}

let create_new_connection = fn(pool) {
  match pool {
    ConnectionPool(data) => {
      Connection({
        id: "conn" + (data.total_created + 1).to_string(),
        created_at: get_current_time_ms(),
        last_used: get_current_time_ms(),
        is_active: true
      })
    }
  }
}

let update_connection_pool = fn(pool, updates) {
  match pool {
    ConnectionPool(data) => {
      ConnectionPool({
        max_connections: data.max_connections,
        min_connections: data.min_connections,
        active_connections: updates.active_connections.getOrElse(data.active_connections),
        idle_connections: updates.idle_connections.getOrElse(data.idle_connections),
        total_created: updates.total_created.getOrElse(data.total_created),
        total_closed: updates.total_closed.getOrElse(data.total_closed)
      })
    }
  }
}

let update_file_handle_manager = fn(manager, updates) {
  match manager {
    FileHandleManager(data) => {
      FileHandleManager({
        max_handles: data.max_handles,
        open_handles: updates.open_handles.getOrElse(data.open_handles),
        handle_counter: updates.handle_counter.getOrElse(data.handle_counter)
      })
    }
  }
}

let find_cache_item_index = fn(manager, key) {
  match manager {
    CacheManager(data) => {
      for i = 0; i < data.items.length(); i = i + 1 {
        if data.items[i].key == key {
          return i
        }
      }
      -1
    }
  }
}

let find_cache_item = fn(manager, key) {
  match manager {
    CacheManager(data) => {
      for i = 0; i < data.items.length(); i = i + 1 {
        if data.items[i].key == key {
          return Some(data.items[i])
        }
      }
      None
    }
  }
}

let get_cache_item_count = fn(manager) {
  match manager {
    CacheManager(data) => data.items.length()
  }
}

let get_cache_memory_usage = fn(manager) {
  match manager {
    CacheManager(data) => {
      data.items.fold(0, fn(acc, item) { acc + item.size })
    }
  }
}

let evict_cache_items = fn(manager, required_size) {
  // 简化的LRU驱逐实现
  match manager {
    CacheManager(data) => {
      // 按最后访问时间排序
      let sorted_items = data.items.sort(fn(a, b) { a.last_accessed < b.last_accessed })
      
      // 驱逐最少使用的项直到有足够空间
      let evicted_count = 0
      let freed_memory = 0
      let remaining_items = []
      
      for i = 0; i < sorted_items.length(); i = i + 1 {
        if freed_memory < required_size {
          freed_memory = freed_memory + sorted_items[i].size
          evicted_count = evicted_count + 1
        } else {
          remaining_items = remaining_items + [sorted_items[i]]
        }
      }
      
      CacheManager({
        max_size: data.max_size,
        max_memory: data.max_memory,
        items: remaining_items,
        eviction_policy: data.eviction_policy
      })
    }
  }
}

let update_cache_manager = fn(manager, updates) {
  match manager {
    CacheManager(data) => {
      CacheManager({
        max_size: data.max_size,
        max_memory: data.max_memory,
        items: updates.items.getOrElse(data.items),
        eviction_policy: data.eviction_policy
      })
    }
  }
}

let update_cache_item_access_time = fn(manager, key) {
  match manager {
    CacheManager(data) => {
      let updated_items = data.items.map(fn(item) {
        if item.key == key {
          { item with last_accessed: get_current_time_ms() }
        } else {
          item
        }
      })
      
      CacheManager({
        max_size: data.max_size,
        max_memory: data.max_memory,
        items: updated_items,
        eviction_policy: data.eviction_policy
      })
    }
  }
}

let track_resource = fn(detector, resource) {
  match detector {
    ResourceLeakDetector(data) => {
      ResourceLeakDetector({
        tracked_resources: data.tracked_resources + [resource],
        leak_threshold: data.leak_threshold,
        detection_interval: data.detection_interval
      })
    }
  }
}

let untrack_resource = fn(detector, resource_id) {
  match detector {
    ResourceLeakDetector(data) => {
      let remaining_resources = data.tracked_resources.filter(fn(resource) {
        resource.id != resource_id
      })
      
      ResourceLeakDetector({
        tracked_resources: remaining_resources,
        leak_threshold: data.leak_threshold,
        detection_interval: data.detection_interval
      })
    }
  }
}

let update_leak_detector = fn(detector, updates) {
  match detector {
    ResourceLeakDetector(data) => {
      ResourceLeakDetector({
        tracked_resources: updates.tracked_resources.getOrElse(data.tracked_resources),
        leak_threshold: data.leak_threshold,
        detection_interval: data.detection_interval
      })
    }
  }
}

let get_current_time_ms = fn() { 1640995200000 }

// 扩展Option类型的方法（用于测试）
let Option::is_success = fn(self) {
  match self {
    Success(_) => true
    Error(_) => false
  }
}

let Option::is_error = fn(self) {
  match self {
    Success(_) => false
    Error(_) => true
  }
}

let Option::getOrElse = fn(self, default) {
  match self {
    Some(value) => value
    None => default
  }
}

// 扩展Array类型的方法（用于测试）
let Array::slice = fn(self, start, end) {
  // 简化的slice实现
  let result = []
  for i = start; i < end && i < self.length(); i = i + 1 {
    result = result + [self[i]]
  }
  result
}

let Array::sort = fn(self, compare) {
  // 简化的排序实现（冒泡排序）
  let result = self
  let n = result.length()
  
  for i = 0; i < n - 1; i = i + 1 {
    for j = 0; j < n - i - 1; j = j + 1 {
      if compare(result[j], result[j + 1]) {
        let temp = result[j]
        result[j] = result[j + 1]
        result[j + 1] = temp
      }
    }
  }
  
  result
}

let Array::fold = fn(self, initial, accumulator) {
  // 简化的fold实现
  let result = initial
  for i = 0; i < self.length(); i = i + 1 {
    result = accumulator(result, self[i])
  }
  result
}

let Array::filter = fn(self, predicate) {
  // 简化的filter实现
  let result = []
  for i = 0; i < self.length(); i = i + 1 {
    if predicate(self[i]) {
      result = result + [self[i]]
    }
  }
  result
}

let Array::map = fn(self, transform) {
  // 简化的map实现
  let result = []
  for i = 0; i < self.length(); i = i + 1 {
    result = result + [transform(self[i])]
  }
  result
}

let Array::map_with_index = fn(self, transform) {
  // 简化的带索引map实现
  let result = []
  for i = 0; i < self.length(); i = i + 1 {
    result = result + [transform(i, self[i])]
  }
  result
}

let Array::any = fn(self, predicate) {
  // 简化的any实现
  for i = 0; i < self.length(); i = i + 1 {
    if predicate(self[i]) {
      return true
    }
  }
  false
}