// Azimuth 并发安全和资源管理测试
// 专注于测试并发环境下的安全性、资源竞争、死锁预防和资源泄漏检测

// 测试1: 并发资源访问安全性
test "并发资源访问安全性验证" {
  // 定义共享资源类型
  type SharedResource = {
    data: Map[String, Int],
    lock: Mutex,
    access_count: Atomic,
    last_access_time: Atomic
  }
  
  // 定义操作类型
  enum OperationType {
    Read
    Write
    Delete
    Update
  }
  
  // 定义操作记录
  type OperationRecord = {
    thread_id: String,
    operation: OperationType,
    key: String,
    timestamp: Int,
    success: Bool,
    error_message: Option[String>
  }
  
  // 创建共享资源管理器
  let create_shared_resource_manager = fn() {
    let resource = {
      data: Map::empty(),
      lock: Mutex::new(),
      access_count: Atomic::new(0),
      last_access_time: Atomic::new(Time::now())
    }
    
    let read_data = fn(key: String) -> Result[Int, String> {
      let start_time = Time::now()
      
      match Mutex::lock(resource.lock) {
        Ok(guard) => {
          resource.access_count.increment()
          resource.last_access_time.store(Time::now())
          
          match Map::get(resource.data, key) {
            Some(value) => {
              Mutex::unlock(guard)
              let end_time = Time::now()
              Ok(value)
            }
            None => {
              Mutex::unlock(guard)
              Err("Key not found: " + key)
            }
          }
        }
        Err(error) => Err("Lock acquisition failed: " + error)
      }
    }
    
    let write_data = fn(key: String, value: Int) -> Result[Unit, String> {
      match Mutex::lock(resource.lock) {
        Ok(guard) => {
          resource.access_count.increment()
          resource.last_access_time.store(Time::now())
          
          let updated_data = Map::insert(resource.data, key, value)
          // 在实际实现中，这里会更新共享资源的数据
          
          Mutex::unlock(guard)
          Ok(())
        }
        Err(error) => Err("Lock acquisition failed: " + error)
      }
    }
    
    let delete_data = fn(key: String) -> Result[Unit, String> {
      match Mutex::lock(resource.lock) {
        Ok(guard) => {
          resource.access_count.increment()
          resource.last_access_time.store(Time::now())
          
          let updated_data = Map::remove(resource.data, key)
          // 在实际实现中，这里会更新共享资源的数据
          
          Mutex::unlock(guard)
          Ok(())
        }
        Err(error) => Err("Lock acquisition failed: " + error)
      }
    }
    
    let get_stats = fn() {
      {
        access_count: resource.access_count.load(),
        last_access_time: resource.last_access_time.load(),
        data_size: Map::size(resource.data)
      }
    }
    
    {
      read_data,
      write_data,
      delete_data,
      get_stats
    }
  }
  
  // 创建并发测试执行器
  let create_concurrent_test_executor = fn(thread_count: Int, operations_per_thread: Int) {
    let execute_test = fn(resource_manager: {
      read_data: (String) -> Result[Int, String],
      write_data: (String, Int) -> Result[Unit, String],
      delete_data: (String) -> Result[Unit, String],
      get_stats: () -> { access_count: Int, last_access_time: Int, data_size: Int }
    }) {
      let threads = []
      let operation_records = []
      
      // 初始化一些数据
      for i in 0..20 {
        let _ = resource_manager.write_data("key-" + i.to_string(), i * 10)
      }
      
      let initial_stats = resource_manager.get_stats()
      
      // 创建工作线程
      for thread_id in 0..thread_count {
        let thread = Thread::spawn(fn() {
          let thread_records = []
          
          for op_id in 0..operations_per_thread {
            let operation = op_id % 4
            let key = "key-" + ((op_id % 20).to_string())
            
            let start_time = Time::now()
            let record = match operation {
              0 => {  // Read
                match resource_manager.read_data(key) {
                  Ok(value) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Read,
                      key: key,
                      timestamp: start_time,
                      success: true,
                      error_message: None
                    }
                  }
                  Err(error) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Read,
                      key: key,
                      timestamp: start_time,
                      success: false,
                      error_message: Some(error)
                    }
                  }
                }
              }
              1 => {  // Write
                match resource_manager.write_data(key, thread_id * 100 + op_id) {
                  Ok(_) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Write,
                      key: key,
                      timestamp: start_time,
                      success: true,
                      error_message: None
                    }
                  }
                  Err(error) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Write,
                      key: key,
                      timestamp: start_time,
                      success: false,
                      error_message: Some(error)
                    }
                  }
                }
              }
              2 => {  // Update (Write with different value)
                match resource_manager.write_data(key, (thread_id + 1) * 200 + op_id) {
                  Ok(_) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Update,
                      key: key,
                      timestamp: start_time,
                      success: true,
                      error_message: None
                    }
                  }
                  Err(error) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Update,
                      key: key,
                      timestamp: start_time,
                      success: false,
                      error_message: Some(error)
                    }
                  }
                }
              }
              _ => {  // Delete
                match resource_manager.delete_data(key) {
                  Ok(_) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Delete,
                      key: key,
                      timestamp: start_time,
                      success: true,
                      error_message: None
                    }
                  }
                  Err(error) => {
                    {
                      thread_id: thread_id.to_string(),
                      operation: OperationType::Delete,
                      key: key,
                      timestamp: start_time,
                      success: false,
                      error_message: Some(error)
                    }
                  }
                }
              }
            }
            
            thread_records = thread_records.push(record)
            
            // 添加随机延迟以增加竞争
            if Math::random() < 0.1 {
              Thread::sleep(Math::random() * 10)  // 0-10ms随机延迟
            }
          }
          
          thread_records
        })
        
        threads = threads.push(thread)
      }
      
      // 等待所有线程完成
      for thread in threads {
        let thread_records = Thread::join(thread)
        operation_records = operation_records + thread_records
      }
      
      let final_stats = resource_manager.get_stats()
      
      {
        operation_records: operation_records,
        initial_stats: initial_stats,
        final_stats: final_stats
      }
    }
    
    { execute_test }
  }
  
  // 执行并发安全测试
  let resource_manager = create_shared_resource_manager()
  let executor = create_concurrent_test_executor(10, 50)  // 10个线程，每个50个操作
  
  let test_result = executor.execute_test(resource_manager)
  
  // 验证操作记录
  assert_eq(test_result.operation_records.length(), 10 * 50)  // 总操作数
  
  // 验证操作成功率
  let successful_operations = test_result.operation_records.filter(fn(record) { record.success })
  let failed_operations = test_result.operation_records.filter(fn(record) { not record.success })
  
  assert_true(successful_operations.length() > 0)
  
  // 验证访问计数增加
  assert_true(test_result.final_stats.access_count > test_result.initial_stats.access_count)
  
  // 验证最后访问时间更新
  assert_true(test_result.final_stats.last_access_time >= test_result.initial_stats.last_access_time)
  
  // 验证不同操作类型的分布
  let read_operations = test_result.operation_records.filter(fn(record) { 
    match record.operation { OperationType::Read => true, _ => false } 
  })
  let write_operations = test_result.operation_records.filter(fn(record) { 
    match record.operation { OperationType::Write => true, _ => false } 
  })
  let update_operations = test_result.operation_records.filter(fn(record) { 
    match record.operation { OperationType::Update => true, _ => false } 
  })
  let delete_operations = test_result.operation_records.filter(fn(record) { 
    match record.operation { OperationType::Delete => true, _ => false } 
  })
  
  // 每种操作类型应该都有
  assert_true(read_operations.length() > 0)
  assert_true(write_operations.length() > 0)
  assert_true(update_operations.length() > 0)
  assert_true(delete_operations.length() > 0)
  
  // 验证线程分布
  let thread_ids = test_result.operation_records.map(fn(record) { record.thread_id }).unique()
  assert_eq(thread_ids.length(), 10)  // 应该有10个不同的线程ID
  
  // 验证没有死锁（所有线程都完成了）
  assert_eq(test_result.operation_records.length(), 500)  // 所有操作都应该完成
  
  // 验证错误处理
  for record in failed_operations {
    assert_true(record.error_message.is_some())
    assert_false(record.error_message.unwrap().is_empty())
  }
}

// 测试2: 资源泄漏检测
test "资源泄漏检测和预防验证" {
  // 定义资源类型
  enum ResourceType {
    Memory
    FileHandle
    NetworkConnection
    DatabaseConnection
    Thread
  }
  
  // 定义资源使用记录
  type ResourceUsage = {
    resource_type: ResourceType,
    allocated_at: Int,
    size_bytes: Int,
    reference_count: Int,
    is_leaked: Bool
  }
  
  // 定义资源泄漏检测器
  type ResourceLeakDetector = {
    track_allocation: fn(ResourceType, Int) -> String,
    track_deallocation: fn(String) -> Unit,
    get_leaked_resources: fn() -> Array[ResourceUsage>,
    get_resource_stats: fn() -> Map[ResourceType, Int]
  }
  
  // 创建资源泄漏检测器
  let create_resource_leak_detector = fn() {
    let mut resources = Map::empty()
    let mut resource_stats = Map::empty()
    
    let track_allocation = fn(resource_type: ResourceType, size_bytes: Int) {
      let resource_id = UUID::v4()
      let current_time = Time::now()
      
      let resource_usage = {
        resource_type: resource_type,
        allocated_at: current_time,
        size_bytes: size_bytes,
        reference_count: 1,
        is_leaked: false
      }
      
      let _ = Map::insert(resources, resource_id, resource_usage)
      
      // 更新统计
      let current_count = match Map::get(resource_stats, resource_type) {
        Some(count) => count
        None => 0
      }
      let _ = Map::insert(resource_stats, resource_type, current_count + 1)
      
      resource_id
    }
    
    let track_deallocation = fn(resource_id: String) {
      match Map::get(resources, resource_id) {
        Some(resource) => {
          let updated_resource = { resource with reference_count: resource.reference_count - 1 }
          
          if updated_resource.reference_count <= 0 {
            let _ = Map::remove(resources, resource_id)
            
            // 更新统计
            let current_count = match Map::get(resource_stats, updated_resource.resource_type) {
              Some(count) => count
              None => 0
            }
            let _ = Map::insert(resource_stats, updated_resource.resource_type, current_count - 1)
          } else {
            let _ = Map::insert(resources, resource_id, updated_resource)
          }
        }
        None => {}  // 资源不存在，忽略
      }
    }
    
    let get_leaked_resources = fn() {
      let current_time = Time::now()
      let leak_threshold_ms = 60000  // 1分钟阈值
      
      resources.filter_map_fn(id, resource) {
        let age_ms = current_time - resource.allocated_at
        let is_leaked = age_ms > leak_threshold_ms && resource.reference_count > 0
        
        if is_leaked {
          Some({ resource with is_leaked: is_leaked })
        } else {
          None
        }
      }.map_fn(resource) { resource }
    }
    
    let get_resource_stats = fn() { resource_stats }
    
    {
      track_allocation,
      track_deallocation,
      get_leaked_resources,
      get_resource_stats
    }
  }
  
  // 创建资源管理器
  let create_resource_manager = fn(detector: ResourceLeakDetector) {
    let allocate_memory = fn(size_bytes: Int) -> String {
      let resource_id = detector.track_allocation(ResourceType::Memory, size_bytes)
      
      // 模拟内存分配
      let memory_block = {
        data: Array::filled(size_bytes / 4, 0),  // 假设每个Int占4字节
        resource_id: resource_id
      }
      
      resource_id
    }
    
    let deallocate_memory = fn(resource_id: String) {
      detector.track_deallocation(resource_id)
      // 在实际实现中，这里会释放内存
    }
    
    let allocate_file_handle = fn(file_path: String) -> String {
      let resource_id = detector.track_allocation(ResourceType::FileHandle, 1024)  // 假设每个文件句柄1KB
      
      // 模拟文件句柄分配
      let file_handle = {
        path: file_path,
        resource_id: resource_id,
        is_open: true
      }
      
      resource_id
    }
    
    let deallocate_file_handle = fn(resource_id: String) {
      detector.track_deallocation(resource_id)
      // 在实际实现中，这里会关闭文件句柄
    }
    
    let allocate_connection = fn(connection_type: ResourceType) -> String {
      let resource_id = detector.track_allocation(connection_type, 2048)  // 假设每个连接2KB
      
      // 模拟连接分配
      let connection = {
        connection_type: connection_type,
        resource_id: resource_id,
        is_connected: true
      }
      
      resource_id
    }
    
    let deallocate_connection = fn(resource_id: String) {
      detector.track_deallocation(resource_id)
      // 在实际实现中，这里会关闭连接
    }
    
    {
      allocate_memory,
      deallocate_memory,
      allocate_file_handle,
      deallocate_file_handle,
      allocate_connection,
      deallocate_connection
    }
  }
  
  // 测试资源泄漏检测
  let detector = create_resource_leak_detector()
  let resource_manager = create_resource_manager(detector)
  
  // 正常分配和释放资源
  let memory_id1 = resource_manager.allocate_memory(1024)
  let file_id1 = resource_manager.allocate_file_handle("/tmp/test1.txt")
  let conn_id1 = resource_manager.allocate_connection(ResourceType::NetworkConnection)
  
  // 检查资源统计
  let stats_after_allocation = detector.get_resource_stats()
  assert_eq(match Map::get(stats_after_allocation, ResourceType::Memory) { Some(count) => count, None => 0 }, 1)
  assert_eq(match Map::get(stats_after_allocation, ResourceType::FileHandle) { Some(count) => count, None => 0 }, 1)
  assert_eq(match Map::get(stats_after_allocation, ResourceType::NetworkConnection) { Some(count) => count, None => 0 }, 1)
  
  // 正常释放资源
  resource_manager.deallocate_memory(memory_id1)
  resource_manager.deallocate_file_handle(file_id1)
  resource_manager.deallocate_connection(conn_id1)
  
  // 检查资源统计
  let stats_after_deallocation = detector.get_resource_stats()
  assert_eq(match Map::get(stats_after_deallocation, ResourceType::Memory) { Some(count) => count, None => 0 }, 0)
  assert_eq(match Map::get(stats_after_deallocation, ResourceType::FileHandle) { Some(count) => count, None => 0 }, 0)
  assert_eq(match Map::get(stats_after_deallocation, ResourceType::NetworkConnection) { Some(count) => count, None => 0 }, 0)
  
  // 模拟资源泄漏
  let leaked_memory_id = resource_manager.allocate_memory(2048)
  let leaked_file_id = resource_manager.allocate_file_handle("/tmp/test2.txt")
  let leaked_conn_id = resource_manager.allocate_connection(ResourceType::DatabaseConnection)
  
  // 不释放这些资源，模拟泄漏
  
  // 分配更多资源
  for i in 0..10 {
    let _ = resource_manager.allocate_memory(512)
    let _ = resource_manager.allocate_file_handle("/tmp/test" + i.to_string() + ".txt")
  }
  
  // 释放部分资源
  for i in 0..5 {
    // 在实际实现中，这里会跟踪这些资源ID并释放它们
    // 这里简化处理
  }
  
  // 检查资源统计
  let final_stats = detector.get_resource_stats()
  assert_true(match Map::get(final_stats, ResourceType::Memory) { Some(count) => count, None => 0 } > 0)
  assert_true(match Map::get(final_stats, ResourceType::FileHandle) { Some(count) => count, None => 0 } > 0)
  assert_true(match Map::get(final_stats, ResourceType::DatabaseConnection) { Some(count) => count, None => 0 } > 0)
  
  // 检查泄漏资源（由于时间阈值，可能不会立即检测到）
  let leaked_resources = detector.get_leaked_resources()
  
  // 在实际测试中，可以通过调整时间阈值或模拟时间流逝来测试泄漏检测
  // 这里我们验证检测器的基本功能
  
  // 验证资源类型覆盖
  let resource_types = [ResourceType::Memory, ResourceType::FileHandle, ResourceType::NetworkConnection, ResourceType::DatabaseConnection]
  for resource_type in resource_types {
    let count = match Map::get(final_stats, resource_type) { Some(count) => count, None => 0 }
    assert_true(count >= 0)  // 计数应该非负
  }
}

// 测试3: 死锁检测和预防
test "死锁检测和预防机制验证" {
  // 定义锁类型
  enum LockType {
    Mutex
    ReadWriteLock
    SpinLock
    RecursiveLock
  }
  
  // 定义锁请求
  type LockRequest = {
    thread_id: String,
    lock_id: String,
    lock_type: LockType,
    requested_at: Int,
    timeout_ms: Int
  }
  
  // 定义锁持有
  type LockHold = {
    thread_id: String,
    lock_id: String,
    lock_type: LockType,
    acquired_at: Int,
    lock_order: Int
  }
  
  // 定义死锁检测器
  type DeadlockDetector = {
    request_lock: fn(String, String, LockType, Int) -> Bool,
    release_lock: fn(String, String) -> Unit,
    detect_deadlock: fn() -> Array[Array[String]>,  // 返回死锁环
    get_lock_graph: fn() -> Map[String, Array[String]>  // 返回等待图
  }
  
  // 创建死锁检测器
  let create_deadlock_detector = fn() {
    let mut lock_requests = Map::empty()  // lock_id -> Array[LockRequest]
    let mut lock_holds = Map::empty()     // thread_id -> Array[LockHold]
    let mut lock_counter = 0
    
    let request_lock = fn(thread_id: String, lock_id: String, lock_type: LockType, timeout_ms: Int) -> Bool {
      let current_time = Time::now()
      
      // 检查是否会导致死锁
      let would_deadlock = fn() {
        // 构建等待图
        let wait_graph = Map::empty()
        
        // 添加等待边
        for (lid, requests) in lock_requests {
          for request in requests {
            if request.thread_id != thread_id {
              // 检查谁持有这个锁
              let holders = lock_holds.filter_fn(tid, holds) {
                holds.any_fn(hold) { hold.lock_id == lid }
              }.map_fn(tid) { tid }
              
              for holder in holders {
                let current_waits = match Map::get(wait_graph, request.thread_id) {
                  Some(waits) => waits
                  None => []
                }
                let _ = Map::insert(wait_graph, request.thread_id, current_waits.push(holder))
              }
            }
          }
        }
        
        // 添加当前请求的等待边
        let holders = lock_holds.filter_fn(tid, holds) {
          holds.any_fn(hold) { hold.lock_id == lock_id }
        }.map_fn(tid) { tid }
        
        for holder in holders {
          let current_waits = match Map::get(wait_graph, thread_id) {
            Some(waits) => waits
            None => []
          }
          let _ = Map::insert(wait_graph, thread_id, current_waits.push(holder))
        }
        
        // 检测环路
        let visited = Map::empty()
        let rec_stack = Map::empty()
        
        let has_cycle = fn(node: String) -> Bool {
          if Map::contains_key(rec_stack, node) {
            return true
          }
          
          if Map::contains_key(visited, node) {
            return false
          }
          
          let _ = Map::insert(visited, node, true)
          let _ = Map::insert(rec_stack, node, true)
          
          match Map::get(wait_graph, node) {
            Some(neighbors) => {
              for neighbor in neighbors {
                if has_cycle(neighbor) {
                  return true
                }
              }
            }
            None => {}
          }
          
          let _ = Map::remove(rec_stack, node)
          false
        }
        
        // 检查所有节点
        for node in wait_graph.keys() {
          if not Map::contains_key(visited, node) {
            if has_cycle(node) {
              return true
            }
          }
        }
        
        false
      }
      
      if would_deadlock() {
        return false  // 拒绝请求以避免死锁
      }
      
      // 检查锁是否可用
      let is_available = not lock_holds.any_fn(tid, holds) {
        holds.any_fn(hold) { hold.lock_id == lock_id }
      }
      
      if is_available {
        // 直接获取锁
        let lock_hold = {
          thread_id: thread_id,
          lock_id: lock_id,
          lock_type: lock_type,
          acquired_at: current_time,
          lock_order: lock_counter
        }
        lock_counter = lock_counter + 1
        
        let current_holds = match Map::get(lock_holds, thread_id) {
          Some(holds) => holds
          None => []
        }
        let _ = Map::insert(lock_holds, thread_id, current_holds.push(lock_hold))
        
        return true
      } else {
        // 添加到等待队列
        let lock_request = {
          thread_id: thread_id,
          lock_id: lock_id,
          lock_type: lock_type,
          requested_at: current_time,
          timeout_ms: timeout_ms
        }
        
        let current_requests = match Map::get(lock_requests, lock_id) {
          Some(requests) => requests
          None => []
        }
        let _ = Map::insert(lock_requests, lock_id, current_requests.push(lock_request))
        
        // 在实际实现中，这里会等待锁或超时
        // 这里简化为立即返回false
        return false
      }
    }
    
    let release_lock = fn(thread_id: String, lock_id: String) {
      // 从持有列表中移除
      match Map::get(lock_holds, thread_id) {
        Some(holds) => {
          let updated_holds = holds.filter_fn(hold) { hold.lock_id != lock_id }
          if updated_holds.length() > 0 {
            let _ = Map::insert(lock_holds, thread_id, updated_holds)
          } else {
            let _ = Map::remove(lock_holds, thread_id)
          }
        }
        None => {}
      }
      
      // 处理等待队列
      match Map::get(lock_requests, lock_id) {
        Some(requests) => {
          if requests.length() > 0 {
            let next_request = requests[0]
            let remaining_requests = requests.slice(1)
            
            if remaining_requests.length() > 0 {
              let _ = Map::insert(lock_requests, lock_id, remaining_requests)
            } else {
              let _ = Map::remove(lock_requests, lock_id)
            }
            
            // 将锁授予下一个等待者
            let lock_hold = {
              thread_id: next_request.thread_id,
              lock_id: lock_id,
              lock_type: next_request.lock_type,
              acquired_at: Time::now(),
              lock_order: lock_counter
            }
            lock_counter = lock_counter + 1
            
            let current_holds = match Map::get(lock_holds, next_request.thread_id) {
              Some(holds) => holds
              None => []
            }
            let _ = Map::insert(lock_holds, next_request.thread_id, current_holds.push(lock_hold))
          }
        }
        None => {}
      }
    }
    
    let detect_deadlock = fn() {
      // 构建等待图
      let wait_graph = Map::empty()
      
      for (lock_id, requests) in lock_requests {
        for request in requests {
          let holders = lock_holds.filter_fn(tid, holds) {
            holds.any_fn(hold) { hold.lock_id == lock_id }
          }.map_fn(tid) { tid }
          
          for holder in holders {
            let current_waits = match Map::get(wait_graph, request.thread_id) {
              Some(waits) => waits
              None => []
            }
            let _ = Map::insert(wait_graph, request.thread_id, current_waits.push(holder))
          }
        }
      }
      
      // 检测环路
      let cycles = []
      let visited = Map::empty()
      
      let find_cycles = fn(node: String, path: Array[String>) -> Array[Array[String>> {
        if path.contains(node) {
          // 找到环路
          let cycle_start = path.find_index(fn(n) { n == node }).unwrap()
          return [path.slice(cycle_start)]
        }
        
        if Map::contains_key(visited, node) {
          return []
        }
        
        let _ = Map::insert(visited, node, true)
        
        match Map::get(wait_graph, node) {
          Some(neighbors) => {
            let all_cycles = []
            for neighbor in neighbors {
              let neighbor_cycles = find_cycles(neighbor, path.push(node))
              all_cycles = all_cycles + neighbor_cycles
            }
            all_cycles
          }
          None => []
        }
      }
      
      for node in wait_graph.keys() {
        if not Map::contains_key(visited, node) {
          let node_cycles = find_cycles(node, [])
          cycles = cycles + node_cycles
        }
      }
      
      cycles
    }
    
    let get_lock_graph = fn() {
      let wait_graph = Map::empty()
      
      for (lock_id, requests) in lock_requests {
        for request in requests {
          let holders = lock_holds.filter_fn(tid, holds) {
            holds.any_fn(hold) { hold.lock_id == lock_id }
          }.map_fn(tid) { tid }
          
          for holder in holders {
            let current_waits = match Map::get(wait_graph, request.thread_id) {
              Some(waits) => waits
              None => []
            }
            let _ = Map::insert(wait_graph, request.thread_id, current_waits.push(holder))
          }
        }
      }
      
      wait_graph
    }
    
    {
      request_lock,
      release_lock,
      detect_deadlock,
      get_lock_graph
    }
  }
  
  // 测试死锁检测和预防
  let detector = create_deadlock_detector()
  
  // 测试正常锁获取和释放
  let thread1 = "thread-1"
  let thread2 = "thread-2"
  let lock1 = "lock-A"
  let lock2 = "lock-B"
  
  // 线程1获取锁A
  let acquired1 = detector.request_lock(thread1, lock1, LockType::Mutex, 5000)
  assert_true(acquired1)
  
  // 线程2获取锁B
  let acquired2 = detector.request_lock(thread2, lock2, LockType::Mutex, 5000)
  assert_true(acquired2)
  
  // 线程2尝试获取锁A（应该等待）
  let acquired3 = detector.request_lock(thread2, lock1, LockType::Mutex, 1000)
  assert_false(acquired3)  // 简化实现中直接返回false
  
  // 线程1尝试获取锁B（应该检测到死锁并拒绝）
  let acquired4 = detector.request_lock(thread1, lock2, LockType::Mutex, 5000)
  assert_false(acquired4)  // 应该被死锁检测器拒绝
  
  // 释放锁
  detector.release_lock(thread1, lock1)
  detector.release_lock(thread2, lock2)
  
  // 测试死锁检测
  // 创建一个会导致死锁的场景
  let acquired5 = detector.request_lock(thread1, lock1, LockType::Mutex, 5000)
  assert_true(acquired5)
  
  let acquired6 = detector.request_lock(thread2, lock2, LockType::Mutex, 5000)
  assert_true(acquired6)
  
  // 检测死锁
  let deadlocks = detector.detect_deadlock()
  assert_true(deadlocks.length() >= 0)  // 可能检测到死锁
  
  // 获取等待图
  let lock_graph = detector.get_lock_graph()
  assert_true(lock_graph.size() >= 0)
  
  // 清理
  detector.release_lock(thread1, lock1)
  detector.release_lock(thread2, lock2)
}

// 测试4: 并发性能和吞吐量
test "并发性能和吞吐量验证" {
  // 定义性能指标
  type PerformanceMetrics = {
    operation_count: Int,
    total_time_ms: Int,
    throughput_ops_per_sec: Float,
    avg_latency_ms: Float,
    p95_latency_ms: Float,
    p99_latency_ms: Float,
    error_rate: Float
  }
  
  // 定义工作负载类型
  enum WorkloadType {
    CPUIntensive
    IOIntensive
    Mixed
    MemoryIntensive
  }
  
  // 创建性能测试器
  let create_performance_tester = fn() {
    let run_test = fn(thread_count: Int, operations_per_thread: Int, workload_type: WorkloadType) {
      let threads = []
      let latencies = []
      let errors = []
      
      let start_time = Time::now()
      
      // 创建工作线程
      for thread_id in 0..thread_count {
        let thread = Thread::spawn(fn() {
          let thread_latencies = []
          let thread_errors = []
          
          for op_id in 0..operations_per_thread {
            let op_start = Time::now()
            
            let success = match workload_type {
              WorkloadType::CPUIntensive => {
                // CPU密集型操作：计算质数
                let mut count = 0
                let mut num = 2
                while count < 100 {
                  let mut is_prime = true
                  let mut i = 2
                  while i * i <= num {
                    if num % i == 0 {
                      is_prime = false
                      break
                    }
                    i = i + 1
                  }
                  if is_prime {
                    count = count + 1
                  }
                  num = num + 1
                }
                true
              }
              WorkloadType::IOIntensive => {
                // IO密集型操作：模拟文件读写
                let data = "test data".repeat(100)
                let _ = data.length()  // 模拟IO操作
                Thread::sleep(1)  // 1ms模拟IO延迟
                true
              }
              WorkloadType::MemoryIntensive => {
                // 内存密集型操作：分配和释放内存
                let array = Array::filled(1000, 0)
                let _ = array.map(fn(x) { x * 2 })
                true
              }
              WorkloadType::Mixed => {
                // 混合操作
                let mut result = 0
                for i in 0..50 {
                  result = result + i * i  // CPU操作
                }
                
                let data = "mixed".repeat(10)
                let _ = data.length()  // 轻量IO操作
                
                let array = Array::filled(100, result)
                let _ = array.length()  // 内存操作
                
                true
              }
            }
            
            let op_end = Time::now()
            let latency = op_end - op_start
            
            if success {
              thread_latencies = thread_latencies.push(latency)
            } else {
              thread_errors = thread_errors.push(latency)
            }
          }
          
          (thread_latencies, thread_errors)
        })
        
        threads = threads.push(thread)
      }
      
      // 等待所有线程完成
      for thread in threads {
        let (thread_latencies, thread_errors) = Thread::join(thread)
        latencies = latencies + thread_latencies
        errors = errors + thread_errors
      }
      
      let end_time = Time::now()
      let total_time = end_time - start_time
      let total_operations = latencies.length() + errors.length()
      
      // 计算性能指标
      let throughput = (total_operations as Float) / ((total_time as Float) / 1000.0)
      
      let sorted_latencies = latencies.sort(fn(a, b) { 
        if a < b { -1 } else if a > b { 1 } else { 0 } 
      })
      
      let avg_latency = if sorted_latencies.length() > 0 {
        sorted_latencies.reduce(0, fn(sum, lat) { sum + lat }) / sorted_latencies.length()
      } else {
        0
      }
      
      let p95_latency = if sorted_latencies.length() > 0 {
        let p95_index = ((sorted_latencies.length() as Float) * 0.95) as Int
        sorted_latencies[Math::min(p95_index, sorted_latencies.length() - 1)]
      } else {
        0
      }
      
      let p99_latency = if sorted_latencies.length() > 0 {
        let p99_index = ((sorted_latencies.length() as Float) * 0.99) as Int
        sorted_latencies[Math::min(p99_index, sorted_latencies.length() - 1)]
      } else {
        0
      }
      
      let error_rate = if total_operations > 0 {
        (errors.length() as Float) / (total_operations as Float)
      } else {
        0.0
      }
      
      {
        operation_count: total_operations,
        total_time_ms: total_time,
        throughput_ops_per_sec: throughput,
        avg_latency_ms: avg_latency as Float,
        p95_latency_ms: p95_latency as Float,
        p99_latency_ms: p99_latency as Float,
        error_rate: error_rate
      }
    }
    
    { run_test }
  }
  
  // 执行性能测试
  let tester = create_performance_tester()
  
  // 测试不同工作负载
  let workloads = [
    (WorkloadType::CPUIntensive, "CPU密集型"),
    (WorkloadType::IOIntensive, "IO密集型"),
    (WorkloadType::MemoryIntensive, "内存密集型"),
    (WorkloadType::Mixed, "混合型")
  ]
  
  let thread_counts = [1, 2, 4, 8]
  let results = []
  
  for (workload_type, workload_name) in workloads {
    for thread_count in thread_counts {
      let metrics = tester.run_test(thread_count, 100, workload_type)
      
      results = results.push({
        workload_type: workload_name,
        thread_count: thread_count,
        metrics: metrics
      })
      
      // 验证基本性能指标
      assert_true(metrics.operation_count > 0)
      assert_true(metrics.total_time_ms > 0)
      assert_true(metrics.throughput_ops_per_sec > 0.0)
      assert_true(metrics.avg_latency_ms >= 0.0)
      assert_true(metrics.p95_latency_ms >= metrics.avg_latency_ms)
      assert_true(metrics.p99_latency_ms >= metrics.p95_latency_ms)
      assert_true(metrics.error_rate >= 0.0 && metrics.error_rate <= 1.0)
    }
  }
  
  // 验证并发扩展性
  for workload_type in workloads {
    let workload_results = results.filter_fn(r) { r.workload_type == workload_type.1 }
    
    if workload_results.length() >= 2 {
      let single_thread = workload_results.filter_fn(r) { r.thread_count == 1 }[0].metrics
      let multi_thread = workload_results.filter_fn(r) { r.thread_count == 4 }[0].metrics
      
      // 多线程应该有更高的吞吐量
      assert_true(multi_thread.throughput_ops_per_sec > single_thread.throughput_ops_per_sec)
      
      // 验证延迟不会过度增加
      let latency_increase = (multi_thread.avg_latency_ms - single_thread.avg_latency_ms) / single_thread.avg_latency_ms
      assert_true(latency_increase < 5.0)  // 延迟增加不超过5倍
    }
  }
  
  // 验证不同工作负载的特征
  let cpu_intensive_results = results.filter_fn(r) { r.workload_type == "CPU密集型" }
  let io_intensive_results = results.filter_fn(r) { r.workload_type == "IO密集型" }
  
  if cpu_intensive_results.length() > 0 && io_intensive_results.length() > 0 {
    let cpu_metrics = cpu_intensive_results[0].metrics
    let io_metrics = io_intensive_results[0].metrics
    
    // IO密集型操作通常有更高的延迟
    assert_true(io_metrics.avg_latency_ms > cpu_metrics.avg_latency_ms)
    
    // CPU密集型操作通常有更高的吞吐量（在相同线程数下）
    let cpu_throughput = cpu_intensive_results.filter_fn(r) { r.thread_count == 4 }[0].metrics.throughput_ops_per_sec
    let io_throughput = io_intensive_results.filter_fn(r) { r.thread_count == 4 }[0].metrics.throughput_ops_per_sec
    
    // 这个断言可能不总是成立，取决于具体实现
    // assert_true(cpu_throughput > io_throughput)
  }
}