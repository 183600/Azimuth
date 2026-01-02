// Azimuth 资源管理综合测试
// 测试遥测系统的资源管理功能，包括内存管理、连接池、缓冲区等

test "内存管理测试" {
  // 模拟内存池
  struct MemoryPool {
    total_size: Int
    used_size: Int
    allocated_blocks: Array[String]
  }
  
  // 创建内存池
  let create_memory_pool = fn(size: Int) -> MemoryPool {
    MemoryPool {
      total_size: size,
      used_size: 0,
      allocated_blocks: []
    }
  }
  
  // 模拟内存分配
  let allocate_memory = fn(pool: MemoryPool, block_id: String, size: Int) -> MemoryPool {
    if pool.used_size + size <= pool.total_size {
      MemoryPool {
        total_size: pool.total_size,
        used_size: pool.used_size + size,
        allocated_blocks: pool.allocated_blocks.push(block_id)
      }
    } else {
      pool // 分配失败，返回原池
    }
  }
  
  // 模拟内存释放
  let free_memory = fn(pool: MemoryPool, block_id: String, size: Int) -> MemoryPool {
    if pool.allocated_blocks.some(fn(id) { id == block_id }) {
      let new_blocks = pool.allocated_blocks.filter(fn(id) { id != block_id })
      MemoryPool {
        total_size: pool.total_size,
        used_size: pool.used_size - size,
        allocated_blocks: new_blocks
      }
    } else {
      pool // 块不存在，返回原池
    }
  }
  
  // 创建内存池
  let pool = create_memory_pool(1000)
  
  // 分配内存块
  let pool1 = allocate_memory(pool, "block1", 100)
  assert_eq(pool1.used_size, 100)
  assert_eq(pool1.allocated_blocks.length(), 1)
  
  let pool2 = allocate_memory(pool1, "block2", 200)
  assert_eq(pool2.used_size, 300)
  assert_eq(pool2.allocated_blocks.length(), 2)
  
  // 分配过多内存（应该失败）
  let pool3 = allocate_memory(pool2, "block3", 800) // 总共需要1100，超过1000
  assert_eq(pool3.used_size, 300) // 保持不变
  assert_eq(pool3.allocated_blocks.length(), 2) // 保持不变
  
  // 释放内存块
  let pool4 = free_memory(pool2, "block1", 100)
  assert_eq(pool4.used_size, 200)
  assert_eq(pool4.allocated_blocks.length(), 1)
  assert_false(pool4.allocated_blocks.some(fn(id) { id == "block1" }))
  
  // 释放不存在的块（应该失败）
  let pool5 = free_memory(pool4, "nonexistent", 50)
  assert_eq(pool5.used_size, 200) // 保持不变
  assert_eq(pool5.allocated_blocks.length(), 1) // 保持不变
}

test "连接池管理测试" {
  // 模拟连接
  struct Connection {
    id: String
    in_use: Bool
    created_at: Int
    last_used: Int
  }
  
  // 模拟连接池
  struct ConnectionPool {
    max_connections: Int
    connections: Array[Connection]
    current_time: Int
  }
  
  // 创建连接池
  let create_connection_pool = fn(max_size: Int) -> ConnectionPool {
    ConnectionPool {
      max_connections: max_size,
      connections: [],
      current_time: 0
    }
  }
  
  // 获取连接
  let get_connection = fn(pool: ConnectionPool) -> (ConnectionPool, Option[Connection]) {
    // 查找可用连接
    let available_connection = pool.connections.find(fn(conn) { !conn.in_use })
    
    match available_connection {
      Some(conn) => {
        // 标记为使用中
        let updated_connections = pool.connections.map(fn(c) {
          if c.id == conn.id {
            { ...c, in_use: true, last_used: pool.current_time }
          } else {
            c
          }
        })
        
        ( { ...pool, connections: updated_connections }, Some(conn) )
      }
      None => {
        // 如果没有可用连接且未达到最大连接数，创建新连接
        if pool.connections.length() < pool.max_connections {
          let new_conn = Connection {
            id: "conn_" + pool.connections.length().to_string(),
            in_use: true,
            created_at: pool.current_time,
            last_used: pool.current_time
          }
          
          ( { ...pool, connections: pool.connections.push(new_conn) }, Some(new_conn) )
        } else {
          // 无法获取连接
          ( pool, None )
        }
      }
    }
  }
  
  // 释放连接
  let release_connection = fn(pool: ConnectionPool, conn_id: String) -> ConnectionPool {
    let updated_connections = pool.connections.map(fn(conn) {
      if conn.id == conn_id {
        { ...conn, in_use: false }
      } else {
        conn
      }
    })
    
    { ...pool, connections: updated_connections }
  }
  
  // 创建连接池
  let pool = create_connection_pool(3)
  
  // 获取连接
  let (pool1, conn1) = get_connection(pool)
  assert_true(conn1.is_some())
  assert_eq(pool1.connections.length(), 1)
  assert_true(conn1.unwrap().in_use)
  
  // 获取另一个连接
  let (pool2, conn2) = get_connection(pool1)
  assert_true(conn2.is_some())
  assert_eq(pool2.connections.length(), 2)
  assert_true(conn2.unwrap().in_use)
  assert_ne(conn1.unwrap().id, conn2.unwrap().id)
  
  // 释放第一个连接
  let pool3 = release_connection(pool2, conn1.unwrap().id)
  let released_conn = pool3.connections.find(fn(c) { c.id == conn1.unwrap().id })
  assert_false(released_conn.unwrap().in_use)
  
  // 再次获取连接，应该重用已释放的连接
  let (pool4, conn3) = get_connection(pool3)
  assert_true(conn3.is_some())
  assert_eq(pool4.connections.length(), 2) // 连接数不应增加
  assert_eq(conn3.unwrap().id, conn1.unwrap().id) // 重用连接
  
  // 获取所有连接
  let (pool5, conn4) = get_connection(pool4)
  assert_true(conn4.is_some())
  assert_eq(pool5.connections.length(), 3)
  
  // 尝试获取超出最大连接数的连接
  let (pool6, conn5) = get_connection(pool5)
  assert_true(conn5.is_none()) // 无法获取连接
  assert_eq(pool6.connections.length(), 3) // 连接数不变
}

test "缓冲区管理测试" {
  // 模拟缓冲区
  struct Buffer {
    data: Array[String]
    max_size: Int
    current_size: Int
  }
  
  // 创建缓冲区
  let create_buffer = fn(max_size: Int) -> Buffer {
    Buffer {
      data: [],
      max_size: max_size,
      current_size: 0
    }
  }
  
  // 写入缓冲区
  let write_to_buffer = fn(buffer: Buffer, item: String) -> Buffer {
    if buffer.current_size < buffer.max_size {
      Buffer {
        data: buffer.data.push(item),
        max_size: buffer.max_size,
        current_size: buffer.current_size + 1
      }
    } else {
      // 缓冲区满，移除最旧的项（FIFO）
      let new_data = buffer.data.slice(1, buffer.data.length()).push(item)
      Buffer {
        data: new_data,
        max_size: buffer.max_size,
        current_size: buffer.max_size
      }
    }
  }
  
  // 读取缓冲区
  let read_from_buffer = fn(buffer: Buffer) -> (Buffer, Option[String]) {
    if buffer.data.length() > 0 {
      let item = buffer.data[0]
      let new_data = buffer.data.slice(1, buffer.data.length())
      
      ( Buffer {
        data: new_data,
        max_size: buffer.max_size,
        current_size: buffer.current_size - 1
      }, Some(item) )
    } else {
      ( buffer, None )
    }
  }
  
  // 创建缓冲区
  let buffer = create_buffer(3)
  
  // 写入数据
  let buffer1 = write_to_buffer(buffer, "item1")
  assert_eq(buffer1.current_size, 1)
  assert_eq(buffer1.data[0], "item1")
  
  let buffer2 = write_to_buffer(buffer1, "item2")
  assert_eq(buffer2.current_size, 2)
  assert_eq(buffer2.data[0], "item1")
  assert_eq(buffer2.data[1], "item2")
  
  let buffer3 = write_to_buffer(buffer2, "item3")
  assert_eq(buffer3.current_size, 3)
  assert_eq(buffer3.data[0], "item1")
  assert_eq(buffer3.data[1], "item2")
  assert_eq(buffer3.data[2], "item3")
  
  // 缓冲区满时写入，应移除最旧的项
  let buffer4 = write_to_buffer(buffer3, "item4")
  assert_eq(buffer4.current_size, 3) // 大小不变
  assert_eq(buffer4.data[0], "item2") // item1被移除
  assert_eq(buffer4.data[1], "item3")
  assert_eq(buffer4.data[2], "item4")
  
  // 读取数据
  let (buffer5, item1) = read_from_buffer(buffer4)
  assert_true(item1.is_some())
  assert_eq(item1.unwrap(), "item2")
  assert_eq(buffer5.current_size, 2)
  assert_eq(buffer5.data[0], "item3")
  assert_eq(buffer5.data[1], "item4")
  
  // 读取所有数据
  let (buffer6, item2) = read_from_buffer(buffer5)
  assert_true(item2.is_some())
  assert_eq(item2.unwrap(), "item3")
  
  let (buffer7, item3) = read_from_buffer(buffer6)
  assert_true(item3.is_some())
  assert_eq(item3.unwrap(), "item4")
  
  let (buffer8, item4) = read_from_buffer(buffer7)
  assert_true(item4.is_none()) // 缓冲区空
  assert_eq(buffer8.current_size, 0)
  assert_eq(buffer8.data.length(), 0)
}

test "文件资源管理测试" {
  // 模拟文件句柄
  struct FileHandle {
    id: String
    path: String
    mode: String // "r", "w", "a"
    is_open: Bool
    position: Int
  }
  
  // 模拟文件管理器
  struct FileManager {
    open_handles: Array[FileHandle]
    max_open_files: Int
  }
  
  // 创建文件管理器
  let create_file_manager = fn(max_files: Int) -> FileManager {
    FileManager {
      open_handles: [],
      max_open_files: max_files
    }
  }
  
  // 打开文件
  let open_file = fn(manager: FileManager, path: String, mode: String) -> (FileManager, Option[FileHandle]) {
    // 检查文件是否已打开
    let existing_handle = manager.open_handles.find(fn(handle) { handle.path == path && handle.is_open })
    
    match existing_handle {
      Some(handle) => {
        // 文件已打开，返回现有句柄
        ( manager, Some(handle) )
      }
      None => {
        // 检查是否达到最大打开文件数
        if manager.open_handles.length() < manager.max_open_files {
          let new_handle = FileHandle {
            id: "handle_" + manager.open_handles.length().to_string(),
            path: path,
            mode: mode,
            is_open: true,
            position: 0
          }
          
          ( { ...manager, open_handles: manager.open_handles.push(new_handle) }, Some(new_handle) )
        } else {
          // 无法打开更多文件
          ( manager, None )
        }
      }
    }
  }
  
  // 关闭文件
  let close_file = fn(manager: FileManager, handle_id: String) -> FileManager {
    let updated_handles = manager.open_handles.map(fn(handle) {
      if handle.id == handle_id {
        { ...handle, is_open: false }
      } else {
        handle
      }
    })
    
    { ...manager, open_handles: updated_handles }
  }
  
  // 清理已关闭的句柄
  let cleanup_closed_handles = fn(manager: FileManager) -> FileManager {
    let open_handles = manager.open_handles.filter(fn(handle) { handle.is_open })
    { ...manager, open_handles: open_handles }
  }
  
  // 创建文件管理器
  let manager = create_file_manager(3)
  
  // 打开文件
  let (manager1, handle1) = open_file(manager, "/tmp/file1.txt", "r")
  assert_true(handle1.is_some())
  assert_eq(manager1.open_handles.length(), 1)
  assert_true(handle1.unwrap().is_open)
  
  let (manager2, handle2) = open_file(manager1, "/tmp/file2.txt", "w")
  assert_true(handle2.is_some())
  assert_eq(manager2.open_handles.length(), 2)
  
  let (manager3, handle3) = open_file(manager2, "/tmp/file3.txt", "a")
  assert_true(handle3.is_some())
  assert_eq(manager3.open_handles.length(), 3)
  
  // 尝试打开更多文件（应该失败）
  let (manager4, handle4) = open_file(manager3, "/tmp/file4.txt", "r")
  assert_true(handle4.is_none())
  assert_eq(manager4.open_handles.length(), 3)
  
  // 关闭一个文件
  let manager5 = close_file(manager4, handle1.unwrap().id)
  let closed_handle = manager5.open_handles.find(fn(h) { h.id == handle1.unwrap().id })
  assert_false(closed_handle.unwrap().is_open)
  
  // 现在可以打开新文件
  let (manager6, handle5) = open_file(manager5, "/tmp/file4.txt", "r")
  assert_true(handle5.is_some())
  assert_eq(manager6.open_handles.length(), 4) // 包括已关闭的句柄
  
  // 清理已关闭的句柄
  let manager7 = cleanup_closed_handles(manager6)
  assert_eq(manager7.open_handles.length(), 3) // 只有打开的句柄
  
  // 尝试重复打开同一文件
  let (manager8, handle6) = open_file(manager7, "/tmp/file2.txt", "r")
  assert_true(handle6.is_some())
  assert_eq(handle6.unwrap().path, "/tmp/file2.txt")
  assert_eq(manager8.open_handles.length(), 3) // 不应增加
}

test "线程资源管理测试" {
  // 模拟线程
  struct Thread {
    id: String
    name: String
    status: String // "running", "idle", "stopped"
    created_at: Int
    task: Option[String]
  }
  
  // 模拟线程池
  struct ThreadPool {
    threads: Array[Thread]
    max_threads: Int
    current_time: Int
  }
  
  // 创建线程池
  let create_thread_pool = fn(max_size: Int) -> ThreadPool {
    ThreadPool {
      threads: [],
      max_threads: max_size,
      current_time: 0
    }
  }
  
  // 创建线程
  let create_thread = fn(pool: ThreadPool, name: String) -> ThreadPool {
    if pool.threads.length() < pool.max_threads {
      let new_thread = Thread {
        id: "thread_" + pool.threads.length().to_string(),
        name: name,
        status: "idle",
        created_at: pool.current_time,
        task: None
      }
      
      { ...pool, threads: pool.threads.push(new_thread) }
    } else {
      pool // 达到最大线程数
    }
  }
  
  // 分配任务给线程
  let assign_task = fn(pool: ThreadPool, task: String) -> ThreadPool {
    // 查找空闲线程
    let idle_thread_index = pool.threads.index_of(fn(thread) { thread.status == "idle" })
    
    match idle_thread_index {
      Some(index) => {
        let updated_threads = pool.threads.map_with_index(fn(i, thread) {
          if i == index {
            { ...thread, status: "running", task: Some(task) }
          } else {
            thread
          }
        })
        
        { ...pool, threads: updated_threads }
      }
      None => {
        pool // 没有空闲线程
      }
    }
  }
  
  // 完成任务
  let complete_task = fn(pool: ThreadPool, thread_id: String) -> ThreadPool {
    let updated_threads = pool.threads.map(fn(thread) {
      if thread.id == thread_id {
        { ...thread, status: "idle", task: None }
      } else {
        thread
      }
    })
    
    { ...pool, threads: updated_threads }
  }
  
  // 停止线程
  let stop_thread = fn(pool: ThreadPool, thread_id: String) -> ThreadPool {
    let updated_threads = pool.threads.map(fn(thread) {
      if thread.id == thread_id {
        { ...thread, status: "stopped", task: None }
      } else {
        thread
      }
    })
    
    { ...pool, threads: updated_threads }
  }
  
  // 创建线程池
  let pool = create_thread_pool(3)
  
  // 创建线程
  let pool1 = create_thread(pool, "worker1")
  assert_eq(pool1.threads.length(), 1)
  assert_eq(pool1.threads[0].status, "idle")
  
  let pool2 = create_thread(pool1, "worker2")
  assert_eq(pool2.threads.length(), 2)
  
  let pool3 = create_thread(pool2, "worker3")
  assert_eq(pool3.threads.length(), 3)
  
  // 尝试创建更多线程（应该失败）
  let pool4 = create_thread(pool3, "worker4")
  assert_eq(pool4.threads.length(), 3)
  
  // 分配任务
  let pool5 = assign_task(pool4, "task1")
  let running_thread = pool5.threads.find(fn(t) { t.status == "running" })
  assert_true(running_thread.is_some())
  assert_eq(running_thread.unwrap().task.unwrap(), "task1")
  
  let pool6 = assign_task(pool5, "task2")
  let pool7 = assign_task(pool6, "task3")
  
  // 所有线程都在运行
  let running_threads = pool7.threads.filter(fn(t) { t.status == "running" })
  assert_eq(running_threads.length(), 3)
  
  // 尝试分配更多任务（应该失败）
  let pool8 = assign_task(pool7, "task4")
  let idle_threads = pool8.threads.filter(fn(t) { t.status == "idle" })
  assert_eq(idle_threads.length(), 0)
  
  // 完成任务
  let pool9 = complete_task(pool8, pool8.threads[0].id)
  let completed_thread = pool9.threads.find(fn(t) { t.id == pool8.threads[0].id })
  assert_eq(completed_thread.unwrap().status, "idle")
  assert_true(completed_thread.unwrap().task.is_none())
  
  // 现在可以分配新任务
  let pool10 = assign_task(pool9, "task4")
  let new_task_thread = pool10.threads.find(fn(t) { t.task.unwrap() == "task4" })
  assert_true(new_task_thread.is_some())
  
  // 停止线程
  let pool11 = stop_thread(pool10, pool10.threads[1].id)
  let stopped_thread = pool11.threads.find(fn(t) { t.id == pool10.threads[1].id })
  assert_eq(stopped_thread.unwrap().status, "stopped")
  
  // 停止的线程不应接受任务
  let pool12 = assign_task(pool11, "task5")
  let still_stopped = pool12.threads.find(fn(t) { t.id == pool10.threads[1].id })
  assert_eq(still_stopped.unwrap().status, "stopped")
}

test "定时器资源管理测试" {
  // 模拟定时器
  struct Timer {
    id: String
    interval: Int // 毫秒
    remaining: Int
    is_active: Bool
    callback: String
  }
  
  // 模拟定时器管理器
  struct TimerManager {
    timers: Array[Timer]
    current_time: Int
  }
  
  // 创建定时器管理器
  let create_timer_manager = fn() -> TimerManager {
    TimerManager {
      timers: [],
      current_time: 0
    }
  }
  
  // 创建定时器
  let create_timer = fn(manager: TimerManager, interval: Int, callback: String) -> TimerManager {
    let new_timer = Timer {
      id: "timer_" + manager.timers.length().to_string(),
      interval: interval,
      remaining: interval,
      is_active: true,
      callback: callback
    }
    
    { ...manager, timers: manager.timers.push(new_timer) }
  }
  
  // 更新定时器
  let update_timers = fn(manager: TimerManager, delta_time: Int) -> (TimerManager, Array[String]) {
    let mut triggered_callbacks = []
    let updated_timers = manager.timers.map(fn(timer) {
      if timer.is_active {
        let new_remaining = timer.remaining - delta_time
        
        if new_remaining <= 0 {
          // 触发定时器
          triggered_callbacks = triggered_callbacks.push(timer.callback)
          
          // 重置定时器
          { ...timer, remaining: timer.interval }
        } else {
          { ...timer, remaining: new_remaining }
        }
      } else {
        timer
      }
    })
    
    ( { ...manager, timers: updated_timers, current_time: manager.current_time + delta_time }, triggered_callbacks )
  }
  
  // 停止定时器
  let stop_timer = fn(manager: TimerManager, timer_id: String) -> TimerManager {
    let updated_timers = manager.timers.map(fn(timer) {
      if timer.id == timer_id {
        { ...timer, is_active: false }
      } else {
        timer
      }
    })
    
    { ...manager, timers: updated_timers }
  }
  
  // 启动定时器
  let start_timer = fn(manager: TimerManager, timer_id: String) -> TimerManager {
    let updated_timers = manager.timers.map(fn(timer) {
      if timer.id == timer_id {
        { ...timer, is_active: true, remaining: timer.interval }
      } else {
        timer
      }
    })
    
    { ...manager, timers: updated_timers }
  }
  
  // 创建定时器管理器
  let manager = create_timer_manager()
  
  // 创建定时器
  let manager1 = create_timer(manager, 100, "callback1")
  let manager2 = create_timer(manager1, 200, "callback2")
  let manager3 = create_timer(manager2, 50, "callback3")
  
  assert_eq(manager3.timers.length(), 3)
  
  // 更新定时器（50ms后）
  let (manager4, callbacks1) = update_timers(manager3, 50)
  assert_eq(callbacks1.length(), 1) // 只有50ms定时器触发
  assert_eq(callbacks1[0], "callback3")
  
  // 检查定时器状态
  let timer3 = manager4.timers.find(fn(t) { t.callback == "callback3" })
  assert_eq(timer3.unwrap().remaining, 50) // 重置为50ms
  
  let timer1 = manager4.timers.find(fn(t) { t.callback == "callback1" })
  assert_eq(timer1.unwrap().remaining, 50) // 100 - 50 = 50
  
  let timer2 = manager4.timers.find(fn(t) { t.callback == "callback2" })
  assert_eq(timer2.unwrap().remaining, 150) // 200 - 50 = 150
  
  // 再更新50ms（总共100ms）
  let (manager5, callbacks2) = update_timers(manager4, 50)
  assert_eq(callbacks2.length(), 2) // 50ms和100ms定时器触发
  assert_true(callbacks2.some(fn(c) { c == "callback3" }))
  assert_true(callbacks2.some(fn(c) { c == "callback1" }))
  
  // 停止一个定时器
  let manager6 = stop_timer(manager5, manager5.timers[0].id)
  let stopped_timer = manager6.timers.find(fn(t) { t.id == manager5.timers[0].id })
  assert_false(stopped_timer.unwrap().is_active)
  
  // 再更新50ms（总共150ms）
  let (manager7, callbacks3) = update_timers(manager6, 50)
  assert_eq(callbacks3.length(), 1) // 只有50ms定时器触发，100ms定时器已停止
  
  // 重新启动定时器
  let manager8 = start_timer(manager7, manager7.timers[0].id)
  let restarted_timer = manager8.timers.find(fn(t) { t.id == manager7.timers[0].id })
  assert_true(restarted_timer.unwrap().is_active)
  assert_eq(restarted_timer.unwrap().remaining, restarted_timer.unwrap().interval)
}