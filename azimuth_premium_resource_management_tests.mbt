// Azimuth Premium Test Suite - Resource Management
// This file contains comprehensive test cases for resource management, memory handling, and lifecycle control

// Test 1: Memory Allocation and Deallocation
test "memory_allocation_and_deallocation" {
  // Test memory pool management
  let memory_pool = |pool_size| {
    let mut pool = []
    let mut allocated = []
    let mut allocation_count = 0
    
    // Initialize pool with memory blocks
    for i = 0; i < pool_size; i = i + 1 {
      pool.push({
        id: i,
        size: 1024,  // 1KB blocks
        allocated: false,
        data: null
      })
    }
    
    let allocate = |size| {
      // Find a free block of sufficient size
      for i = 0; i < pool.length(); i = i + 1 {
        if !pool[i].allocated && pool[i].size >= size {
          pool[i].allocated = true
          pool[i].data = "data_" + i.to_string()
          allocated.push(i)
          allocation_count = allocation_count + 1
          return i
        }
      }
      -1  // No available block
    }
    
    let deallocate = |block_id| {
      if block_id >= 0 && block_id < pool.length() {
        pool[block_id].allocated = false
        pool[block_id].data = null
        
        // Remove from allocated list
        let mut new_allocated = []
        for id in allocated {
          if id != block_id {
            new_allocated.push(id)
          }
        }
        allocated = new_allocated
        return true
      }
      false
    }
    
    let get_pool_stats = || {
      let allocated_count = pool.reduce(|acc, block| if block.allocated { acc + 1 } else { acc }, 0)
      let free_count = pool.length() - allocated_count
      let total_allocated_size = pool.reduce(|acc, block| if block.allocated { acc + block.size } else { acc }, 0)
      
      {
        total_blocks: pool.length(),
        allocated_blocks: allocated_count,
        free_blocks: free_count,
        total_allocated_size: total_allocated_size,
        allocation_count: allocation_count
      }
    }
    
    let compact = || {
      // Defragment memory by moving allocated blocks together
      let mut compacted_pool = []
      let mut compacted_allocated = []
      
      for block in pool {
        if block.allocated {
          compacted_pool.push(block)
          compacted_allocated.push(block.id)
        }
      }
      
      for block in pool {
        if !block.allocated {
          compacted_pool.push(block)
        }
      }
      
      pool = compacted_pool
      allocated = compacted_allocated
    }
    
    (allocate, deallocate, get_pool_stats, compact)
  }
  
  let (allocate, deallocate, get_stats, compact) = memory_pool(10)
  
  // Test memory allocation
  let block1 = allocate(512)
  let block2 = allocate(1024)
  let block3 = allocate(2048)
  
  assert_eq(block1, 0)
  assert_eq(block2, 1)
  assert_eq(block3, 2)
  
  let stats = get_stats()
  assert_eq(stats.allocated_blocks, 3)
  assert_eq(stats.free_blocks, 7)
  assert_eq(stats.total_allocated_size, 3 * 1024)
  
  // Test memory deallocation
  assert_true(deallocate(block2))
  assert_false(deallocate(99))  // Invalid block ID
  
  let stats2 = get_stats()
  assert_eq(stats2.allocated_blocks, 2)
  assert_eq(stats2.free_blocks, 8)
  
  // Test reallocation after deallocation
  let block4 = allocate(512)
  assert_eq(block4, 1)  // Should reuse the deallocated block
  
  // Test garbage collection simulation
  let garbage_collector = || {
    let mut objects = []
    let mut gc_roots = []
    
    let create_object = |data| {
      let obj = {
        id: objects.length(),
        data: data,
        references: [],
        marked: false
      }
      objects.push(obj)
      objects.length() - 1
    }
    
    let add_reference = |from_id, to_id| {
      if from_id < objects.length() && to_id < objects.length() {
        objects[from_id].references.push(to_id)
      }
    }
    
    let add_root = |id| {
      if id < objects.length() {
        gc_roots.push(id)
      }
    }
    
    let collect_garbage = || {
      // Mark phase
      for id in gc_roots {
        mark_object(id)
      }
      
      // Sweep phase
      let mut surviving_objects = []
      let mut freed_count = 0
      
      for i = 0; i < objects.length(); i = i + 1 {
        if objects[i].marked {
          // Reset mark for next GC cycle
          objects[i].marked = false
          surviving_objects.push(objects[i])
        } else {
          freed_count = freed_count + 1
        }
      }
      
      objects = surviving_objects
      
      // Update root references
      let mut new_roots = []
      for root_id in gc_roots {
        // Find new ID after compaction
        for i = 0; i < objects.length(); i = i + 1 {
          if objects[i].id == root_id {
            new_roots.push(i)
            break
          }
        }
      }
      gc_roots = new_roots
      
      freed_count
    }
    
    let mark_object = |id| {
      if id < objects.length() && !objects[id].marked {
        objects[id].marked = true
        
        // Mark all referenced objects
        for ref_id in objects[id].references {
          mark_object(ref_id)
        }
      }
    }
    
    let get_gc_stats = || {
      {
        total_objects: objects.length(),
        root_objects: gc_roots.length()
      }
    }
    
    (create_object, add_reference, add_root, collect_garbage, get_gc_stats)
  }
  
  let (create_obj, add_ref, add_root, collect_gc, get_gc_stats) = garbage_collector()
  
  // Test garbage collection
  let obj1 = create_obj("root object")
  let obj2 = create_obj("child object")
  let obj3 = create_obj("orphaned object")
  
  add_ref(obj1, obj2)  // obj1 references obj2
  add_root(obj1)       // obj1 is a root
  
  let stats_gc = get_gc_stats()
  assert_eq(stats_gc.total_objects, 3)
  assert_eq(stats_gc.root_objects, 1)
  
  let freed_count = collect_gc()
  assert_eq(freed_count, 1)  // obj3 should be garbage collected
  
  let stats_gc2 = get_gc_stats()
  assert_eq(stats_gc2.total_objects, 2)  // obj1 and obj2 remain
  
  // Test memory leak detection
  let memory_leak_detector = || {
    let mut allocations = {}
    let mut deallocations = {}
    
    let track_allocation = |id, size| {
      allocations[id] = size
    }
    
    let track_deallocation = |id| {
      deallocations[id] = true
    }
    
    let detect_leaks = || {
      let leaks = []
      
      for id in allocations {
        if !deallocations.contains(id) {
          leaks.push({
            id: id,
            size: allocations[id]
          })
        }
      }
      
      leaks
    }
    
    (track_allocation, track_deallocation, detect_leaks)
  }
  
  let (track_alloc, track_dealloc, detect_leaks) = memory_leak_detector()
  
  // Test leak detection
  track_alloc(1, 1024)
  track_alloc(2, 2048)
  track_alloc(3, 4096)
  
  track_dealloc(2)  // Only deallocate block 2
  
  let leaks = detect_leaks()
  assert_eq(leaks.length(), 2)
  assert_eq(leaks[0].id, 1)
  assert_eq(leaks[0].size, 1024)
  assert_eq(leaks[1].id, 3)
  assert_eq(leaks[1].size, 4096)
}

// Test 2: Resource Pool Management
test "resource_pool_management" {
  // Test connection pool
  let connection_pool = |max_connections| {
    let mut pool = []
    let mut active = []
    let mut connection_id_counter = 0
    
    // Initialize pool
    for i = 0; i < max_connections; i = i + 1 {
      pool.push({
        id: connection_id_counter,
        created_at: 1640995200000L + i.to_long(),
        last_used: 1640995200000L + i.to_long(),
        in_use: false,
        connection_string: "conn_" + i.to_string()
      })
      connection_id_counter = connection_id_counter + 1
    }
    
    let get_connection = || {
      // Find an available connection
      for i = 0; i < pool.length(); i = i + 1 {
        if !pool[i].in_use {
          pool[i].in_use = true
          pool[i].last_used = 1640995200000L + 1000  // Current time
          active.push(pool[i].id)
          return Some(pool[i])
        }
      }
      None  // No available connections
    }
    
    let release_connection = |connection_id| {
      // Find and release the connection
      for i = 0; i < pool.length(); i = i + 1 {
        if pool[i].id == connection_id {
          pool[i].in_use = false
          pool[i].last_used = 1640995200000L + 1000  // Current time
          
          // Remove from active list
          let mut new_active = []
          for id in active {
            if id != connection_id {
              new_active.push(id)
            }
          }
          active = new_active
          return true
        }
      }
      false
    }
    
    let get_pool_stats = || {
      let active_count = pool.reduce(|acc, conn| if conn.in_use { acc + 1 } else { acc }, 0)
      let idle_count = pool.length() - active_count
      
      {
        total_connections: pool.length(),
        active_connections: active_count,
        idle_connections: idle_count,
        active_ids: active
      }
    }
    
    let cleanup_idle_connections = |max_idle_time| {
      let current_time = 1640995200000L + 1000
      let mut removed = []
      
      let mut new_pool = []
      for conn in pool {
        let idle_time = current_time - conn.last_used
        if !conn.in_use && idle_time > max_idle_time {
          removed.push(conn.id)
        } else {
          new_pool.push(conn)
        }
      }
      pool = new_pool
      
      removed
    }
    
    (get_connection, release_connection, get_pool_stats, cleanup_idle_connections)
  }
  
  let (get_conn, release_conn, get_stats, cleanup) = connection_pool(5)
  
  // Test connection pool
  let conn1 = get_conn()
  let conn2 = get_conn()
  let conn3 = get_conn()
  
  match conn1 {
    Some(conn) => assert_eq(conn.id, 0)
    None => assert_true(false)
  }
  
  match conn2 {
    Some(conn) => assert_eq(conn.id, 1)
    None => assert_true(false)
  }
  
  let stats = get_stats()
  assert_eq(stats.total_connections, 5)
  assert_eq(stats.active_connections, 3)
  assert_eq(stats.idle_connections, 2)
  
  // Test connection release
  match conn1 {
    Some(conn) => assert_true(release_conn(conn.id))
    None => assert_true(false)
  }
  
  let stats2 = get_stats()
  assert_eq(stats2.active_connections, 2)
  assert_eq(stats2.idle_connections, 3)
  
  // Test thread pool
  let thread_pool = |pool_size| {
    let mut workers = []
    let mut task_queue = []
    let mut worker_id_counter = 0
    
    // Initialize workers
    for i = 0; i < pool_size; i = i + 1 {
      workers.push({
        id: worker_id_counter,
        busy: false,
        task_count: 0,
        current_task: null
      })
      worker_id_counter = worker_id_counter + 1
    }
    
    let submit_task = |task| {
      // Find an available worker
      for i = 0; i < workers.length(); i = i + 1 {
        if !workers[i].busy {
          workers[i].busy = true
          workers[i].current_task = task
          workers[i].task_count = workers[i].task_count + 1
          return workers[i].id
        }
      }
      
      // No available workers, add to queue
      task_queue.push(task)
      -1
    }
    
    let complete_task = |worker_id| {
      // Find the worker and mark as available
      for i = 0; i < workers.length(); i = i + 1 {
        if workers[i].id == worker_id {
          workers[i].busy = false
          workers[i].current_task = null
          
          // Process next task in queue if available
          if task_queue.length() > 0 {
            let next_task = task_queue.shift()
            workers[i].busy = true
            workers[i].current_task = next_task
            workers[i].task_count = workers[i].task_count + 1
          }
          return true
        }
      }
      false
    }
    
    let get_pool_stats = || {
      let busy_workers = workers.reduce(|acc, worker| if worker.busy { acc + 1 } else { acc }, 0)
      let idle_workers = workers.length() - busy_workers
      let total_tasks = workers.reduce(|acc, worker| acc + worker.task_count, 0)
      
      {
        total_workers: workers.length(),
        busy_workers: busy_workers,
        idle_workers: idle_workers,
        queued_tasks: task_queue.length(),
        total_tasks_processed: total_tasks
      }
    }
    
    (submit_task, complete_task, get_pool_stats)
  }
  
  let (submit_task, complete_task, get_thread_stats) = thread_pool(3)
  
  // Test thread pool
  let task1 = submit_task("task1")
  let task2 = submit_task("task2")
  let task3 = submit_task("task3")
  let task4 = submit_task("task4")  // Should be queued
  
  assert_eq(task1, 0)
  assert_eq(task2, 1)
  assert_eq(task3, 2)
  assert_eq(task4, -1)  // Queued
  
  let thread_stats = get_thread_stats()
  assert_eq(thread_stats.total_workers, 3)
  assert_eq(thread_stats.busy_workers, 3)
  assert_eq(thread_stats.idle_workers, 0)
  assert_eq(thread_stats.queued_tasks, 1)
  
  // Complete a task
  assert_true(complete_task(task2))
  
  let thread_stats2 = get_thread_stats()
  assert_eq(thread_stats2.busy_workers, 3)  // Still 3 busy (queued task picked up)
  assert_eq(thread_stats2.queued_tasks, 0)
  
  // Test object pool
  let object_pool = |factory, reset_function, initial_size| {
    let mut pool = []
    
    // Initialize pool
    for i = 0; i < initial_size; i = i + 1 {
      pool.push(factory())
    }
    
    let acquire = || {
      if pool.length() > 0 {
        pool.pop()
      } else {
        factory()
      }
    }
    
    let release = |obj| {
      reset_function(obj)
      pool.push(obj)
    }
    
    let get_pool_stats = || {
      {
        available_objects: pool.length()
      }
    }
    
    (acquire, release, get_pool_stats)
  }
  
  // Test object pool
  let create_string_buffer = || {
    { buffer: "", capacity: 1024 }
  }
  
  let reset_string_buffer = |obj| {
    obj.buffer = ""
  }
  
  let (acquire_buffer, release_buffer, get_buffer_stats) = object_pool(create_string_buffer, reset_string_buffer, 3)
  
  let buffer1 = acquire_buffer()
  let buffer2 = acquire_buffer()
  let buffer3 = acquire_buffer()
  let buffer4 = acquire_buffer()  // Should create a new one
  
  buffer1.buffer = "hello"
  buffer2.buffer = "world"
  
  release_buffer(buffer1)
  release_buffer(buffer2)
  
  let buffer5 = acquire_buffer()  // Should reuse released buffer
  
  let buffer_stats = get_buffer_stats()
  assert_eq(buffer_stats.available_objects, 2)  // buffer3 and buffer4
}

// Test 3: File Resource Management
test "file_resource_management" {
  // Test file handle manager
  let file_handle_manager = |max_handles| {
    let mut handles = {}
    let mut open_handles = []
    let mut handle_id_counter = 0
    
    let open_file = |filename, mode| {
      if open_handles.length() >= max_handles {
        return None  // Too many open files
      }
      
      let handle_id = handle_id_counter
      handle_id_counter = handle_id_counter + 1
      
      let handle = {
        id: handle_id,
        filename: filename,
        mode: mode,
        position: 0,
        open: true,
        created_at: 1640995200000L + handle_id.to_long()
      }
      
      handles[handle_id] = handle
      open_handles.push(handle_id)
      
      Some(handle_id)
    }
    
    let close_file = |handle_id| {
      if handles.contains(handle_id) {
        handles[handle_id].open = false
        
        // Remove from open handles list
        let mut new_open = []
        for id in open_handles {
          if id != handle_id {
            new_open.push(id)
          }
        }
        open_handles = new_open
        
        return true
      }
      false
    }
    
    let read_file = |handle_id, size| {
      if handles.contains(handle_id) && handles[handle_id].open {
        let handle = handles[handle_id]
        if handle.mode == "r" || handle.mode == "r+" {
          handle.position = handle.position + size
          "data_of_size_" + size.to_string()  // Simulated read
        } else {
          "error: file not opened for reading"
        }
      } else {
        "error: invalid handle"
      }
    }
    
    let write_file = |handle_id, data| {
      if handles.contains(handle_id) && handles[handle_id].open {
        let handle = handles[handle_id]
        if handle.mode == "w" || handle.mode == "a" || handle.mode == "r+" {
          handle.position = handle.position + data.length()
          data.length()  // Return bytes written
        } else {
          -1  // Error
        }
      } else {
        -1  // Error
      }
    }
    
    let get_manager_stats = || {
      {
        max_handles: max_handles,
        open_handles: open_handles.length(),
        total_handles: handles.length()
      }
    }
    
    let cleanup_closed_handles = || {
      let mut new_handles = {}
      
      for handle_id in handles {
        if handles[handle_id].open {
          new_handles[handle_id] = handles[handle_id]
        }
      }
      
      handles = new_handles
    }
    
    (open_file, close_file, read_file, write_file, get_manager_stats, cleanup_closed_handles)
  }
  
  let (open_file, close_file, read_file, write_file, get_stats, cleanup) = file_handle_manager(5)
  
  // Test file handle management
  let file1 = open_file("test1.txt", "r")
  let file2 = open_file("test2.txt", "w")
  let file3 = open_file("test3.txt", "r+")
  
  match file1 {
    Some(id) => {
      assert_eq(read_file(id, 10), "data_of_size_10")
      assert_eq(write_file(id, "hello"), -1)  // Error: read-only mode
    }
    None => assert_true(false)
  }
  
  match file2 {
    Some(id) => {
      assert_eq(write_file(id, "hello"), 5)
      assert_eq(read_file(id, 10), "error: file not opened for reading")
    }
    None => assert_true(false)
  }
  
  let stats = get_stats()
  assert_eq(stats.max_handles, 5)
  assert_eq(stats.open_handles, 3)
  
  // Test file closing
  match file1 {
    Some(id) => assert_true(close_file(id))
    None => assert_true(false)
  }
  
  let stats2 = get_stats()
  assert_eq(stats2.open_handles, 2)
  
  // Test temporary file manager
  let temp_file_manager = || {
    let mut temp_files = []
    let mut file_id_counter = 0
    
    let create_temp_file = |prefix, suffix| {
      let file_id = file_id_counter
      file_id_counter = file_id_counter + 1
      
      let temp_file = {
        id: file_id,
        name: prefix + file_id.to_string() + suffix,
        path: "/tmp/" + prefix + file_id.to_string() + suffix,
        created_at: 1640995200000L + file_id.to_long(),
        auto_delete: true
      }
      
      temp_files.push(temp_file)
      temp_file.id
    }
    
    let delete_temp_file = |file_id| {
      for i = 0; i < temp_files.length(); i = i + 1 {
        if temp_files[i].id == file_id {
          temp_files.remove(i)
          return true
        }
      }
      false
    }
    
    let cleanup_all_temp_files = || {
      let count = temp_files.length()
      temp_files = []
      count
    }
    
    let get_temp_file_info = |file_id| {
      for file in temp_files {
        if file.id == file_id {
          return Some(file)
        }
      }
      None
    }
    
    let get_temp_files_stats = || {
      {
        total_temp_files: temp_files.length(),
        auto_delete_count: temp_files.reduce(|acc, file| if file.auto_delete { acc + 1 } else { acc }, 0)
      }
    }
    
    (create_temp_file, delete_temp_file, cleanup_all_temp_files, get_temp_file_info, get_temp_files_stats)
  }
  
  let (create_temp, delete_temp, cleanup_all, get_temp_info, get_temp_stats) = temp_file_manager()
  
  // Test temporary file management
  let temp1 = create_temp("temp_", ".txt")
  let temp2 = create_temp("temp_", ".log")
  
  let temp_info = get_temp_info(temp1)
  match temp_info {
    Some(info) => {
      assert_true(info.name.contains("temp_"))
      assert_true(info.name.contains(".txt"))
      assert_eq(info.path, "/tmp/" + info.name)
      assert_true(info.auto_delete)
    }
    None => assert_true(false)
  }
  
  let temp_stats = get_temp_stats()
  assert_eq(temp_stats.total_temp_files, 2)
  assert_eq(temp_stats.auto_delete_count, 2)
  
  // Test file locking
  let file_lock_manager = || {
    let mut locks = {}
    let mut lock_id_counter = 0
    
    let acquire_lock = |filename, lock_type| {
      // Check if file is already locked
      if locks.contains(filename) {
        let existing_lock = locks[filename]
        if existing_lock.type == "exclusive" || lock_type == "exclusive" {
          return None  // Can't acquire lock
        }
        
        // Can acquire shared lock
        let lock_id = lock_id_counter
        lock_id_counter = lock_id_counter + 1
        
        locks[filename] = {
          id: lock_id,
          filename: filename,
          type: lock_type,
          acquired_at: 1640995200000L + lock_id.to_long()
        }
        
        Some(lock_id)
      } else {
        // File is not locked
        let lock_id = lock_id_counter
        lock_id_counter = lock_id_counter + 1
        
        locks[filename] = {
          id: lock_id,
          filename: filename,
          type: lock_type,
          acquired_at: 1640995200000L + lock_id.to_long()
        }
        
        Some(lock_id)
      }
    }
    
    let release_lock = |filename| {
      if locks.contains(filename) {
        locks.remove(filename)
        return true
      }
      false
    }
    
    let is_locked = |filename| {
      locks.contains(filename)
    }
    
    let get_lock_info = |filename| {
      if locks.contains(filename) {
        Some(locks[filename])
      } else {
        None
      }
    }
    
    (acquire_lock, release_lock, is_locked, get_lock_info)
  }
  
  let (acquire_lock, release_lock, is_locked, get_lock_info) = file_lock_manager()
  
  // Test file locking
  let lock1 = acquire_lock("file1.txt", "exclusive")
  let lock2 = acquire_lock("file1.txt", "shared")  // Should fail
  let lock3 = acquire_lock("file2.txt", "shared")  // Should succeed
  
  match lock1 {
    Some(id) => {
      assert_true(is_locked("file1.txt"))
      let lock_info = get_lock_info("file1.txt")
      match lock_info {
        Some(info) => assert_eq(info.type, "exclusive")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  assert_eq(lock2, None)  // Should fail
  assert_true(is_locked("file1.txt"))
  
  match lock3 {
    Some(id) => {
      assert_true(is_locked("file2.txt"))
      let lock_info = get_lock_info("file2.txt")
      match lock_info {
        Some(info) => assert_eq(info.type, "shared")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  assert_true(release_lock("file1.txt"))
  assert_false(is_locked("file1.txt"))
}

// Test 4: Network Resource Management
test "network_resource_management" {
  // Test socket manager
  let socket_manager = |max_sockets| {
    let mut sockets = {}
    let mut active_sockets = []
    let mut socket_id_counter = 0
    
    let create_socket = |socket_type, address| {
      if active_sockets.length() >= max_sockets {
        return None  // Too many sockets
      }
      
      let socket_id = socket_id_counter
      socket_id_counter = socket_id_counter + 1
      
      let socket = {
        id: socket_id,
        type: socket_type,
        address: address,
        state: "created",
        created_at: 1640995200000L + socket_id.to_long(),
        bytes_sent: 0,
        bytes_received: 0
      }
      
      sockets[socket_id] = socket
      active_sockets.push(socket_id)
      
      Some(socket_id)
    }
    
    let connect_socket = |socket_id| {
      if sockets.contains(socket_id) {
        sockets[socket_id].state = "connected"
        return true
      }
      false
    }
    
    let close_socket = |socket_id| {
      if sockets.contains(socket_id) {
        sockets[socket_id].state = "closed"
        
        // Remove from active list
        let mut new_active = []
        for id in active_sockets {
          if id != socket_id {
            new_active.push(id)
          }
        }
        active_sockets = new_active
        
        return true
      }
      false
    }
    
    let send_data = |socket_id, data| {
      if sockets.contains(socket_id) && sockets[socket_id].state == "connected" {
        sockets[socket_id].bytes_sent = sockets[socket_id].bytes_sent + data.length()
        data.length()  // Return bytes sent
      } else {
        -1  // Error
      }
    }
    
    let receive_data = |socket_id, size| {
      if sockets.contains(socket_id) && sockets[socket_id].state == "connected" {
        sockets[socket_id].bytes_received = sockets[socket_id].bytes_received + size
        "received_data_of_size_" + size.to_string()  // Simulated data
      } else {
        "error: socket not connected"
      }
    }
    
    let get_socket_stats = || {
      {
        max_sockets: max_sockets,
        active_sockets: active_sockets.length(),
        total_sockets: sockets.length()
      }
    }
    
    let get_socket_info = |socket_id| {
      if sockets.contains(socket_id) {
        Some(sockets[socket_id])
      } else {
        None
      }
    }
    
    (create_socket, connect_socket, close_socket, send_data, receive_data, get_socket_stats, get_socket_info)
  }
  
  let (create_socket, connect_socket, close_socket, send_data, receive_data, get_stats, get_info) = socket_manager(10)
  
  // Test socket management
  let socket1 = create_socket("tcp", "192.168.1.1:8080")
  let socket2 = create_socket("udp", "192.168.1.2:9090")
  
  match socket1 {
    Some(id) => {
      assert_true(connect_socket(id))
      assert_eq(send_data(id, "hello world"), 11)
      assert_eq(receive_data(id, 10), "received_data_of_size_10")
    }
    None => assert_true(false)
  }
  
  let stats = get_stats()
  assert_eq(stats.max_sockets, 10)
  assert_eq(stats.active_sockets, 2)
  
  let socket_info = get_info(socket1.unwrap())
  match socket_info {
    Some(info) => {
      assert_eq(info.type, "tcp")
      assert_eq(info.address, "192.168.1.1:8080")
      assert_eq(info.state, "connected")
      assert_eq(info.bytes_sent, 11)
      assert_eq(info.bytes_received, 10)
    }
    None => assert_true(false)
  }
  
  // Test HTTP connection pool
  let http_connection_pool = |max_connections| {
    let mut connections = []
    let mut active_connections = []
    let mut connection_id_counter = 0
    
    // Initialize pool
    for i = 0; i < max_connections; i = i + 1 {
      connections.push({
        id: connection_id_counter,
        host: "",
        port: 0,
        in_use: false,
        created_at: 1640995200000L + i.to_long(),
        last_used: 1640995200000L + i.to_long(),
        request_count: 0
      })
      connection_id_counter = connection_id_counter + 1
    }
    
    let get_connection = |host, port| {
      // Find an available connection or reuse one for the same host
      for i = 0; i < connections.length(); i = i + 1 {
        if !connections[i].in_use && connections[i].host == host && connections[i].port == port {
          connections[i].in_use = true
          connections[i].last_used = 1640995200000L + 1000  // Current time
          connections[i].request_count = connections[i].request_count + 1
          active_connections.push(connections[i].id)
          return Some(connections[i].id)
        }
      }
      
      // Find any available connection
      for i = 0; i < connections.length(); i = i + 1 {
        if !connections[i].in_use {
          connections[i].in_use = true
          connections[i].host = host
          connections[i].port = port
          connections[i].last_used = 1640995200000L + 1000  // Current time
          connections[i].request_count = connections[i].request_count + 1
          active_connections.push(connections[i].id)
          return Some(connections[i].id)
        }
      }
      
      None  // No available connections
    }
    
    let release_connection = |connection_id| {
      for i = 0; i < connections.length(); i = i + 1 {
        if connections[i].id == connection_id {
          connections[i].in_use = false
          connections[i].last_used = 1640995200000L + 1000  // Current time
          
          // Remove from active list
          let mut new_active = []
          for id in active_connections {
            if id != connection_id {
              new_active.push(id)
            }
          }
          active_connections = new_active
          return true
        }
      }
      false
    }
    
    let execute_request = |connection_id, method, path| {
      for i = 0; i < connections.length(); i = i + 1 {
        if connections[i].id == connection_id && connections[i].in_use {
          // Simulate HTTP request
          let response = {
            status: 200,
            body: "Response to " + method + " " + path + " from " + connections[i].host + ":" + connections[i].port.to_string()
          }
          connections[i].request_count = connections[i].request_count + 1
          return Some(response)
        }
      }
      None
    }
    
    let get_pool_stats = || {
      let active_count = connections.reduce(|acc, conn| if conn.in_use { acc + 1 } else { acc }, 0)
      let idle_count = connections.length() - active_count
      
      {
        total_connections: connections.length(),
        active_connections: active_count,
        idle_connections: idle_count
      }
    }
    
    (get_connection, release_connection, execute_request, get_pool_stats)
  }
  
  let (get_conn, release_conn, exec_req, get_pool_stats) = http_connection_pool(5)
  
  // Test HTTP connection pool
  let conn1 = get_conn("example.com", 80)
  let conn2 = get_conn("example.com", 80)
  let conn3 = get_conn("api.example.com", 443)
  
  match conn1 {
    Some(id) => {
      let response = exec_req(id, "GET", "/api/data")
      match response {
        Some(resp) => {
          assert_eq(resp.status, 200)
          assert_true(resp.body.contains("GET /api/data"))
          assert_true(resp.body.contains("example.com:80"))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let pool_stats = get_pool_stats()
  assert_eq(pool_stats.total_connections, 5)
  assert_eq(pool_stats.active_connections, 3)
  
  // Test bandwidth throttling
  let bandwidth_throttler = |max_bytes_per_second| {
    let mut bytes_sent = 0
    let mut bytes_received = 0
    let mut last_reset_time = 1640995200000L
    
    let send_data = |data_size| {
      let current_time = 1640995200000L + 1000  // Current time
      
      // Reset counter if enough time has passed
      if current_time - last_reset_time >= 1000 {
        bytes_sent = 0
        bytes_received = 0
        last_reset_time = current_time
      }
      
      if bytes_sent + data_size <= max_bytes_per_second {
        bytes_sent = bytes_sent + data_size
        return data_size  // All data sent
      } else {
        let remaining = max_bytes_per_second - bytes_sent
        bytes_sent = max_bytes_per_second
        return remaining  // Partial data sent
      }
    }
    
    let receive_data = |data_size| {
      let current_time = 1640995200000L + 1000  // Current time
      
      // Reset counter if enough time has passed
      if current_time - last_reset_time >= 1000 {
        bytes_sent = 0
        bytes_received = 0
        last_reset_time = current_time
      }
      
      if bytes_received + data_size <= max_bytes_per_second {
        bytes_received = bytes_received + data_size
        return data_size  // All data received
      } else {
        let remaining = max_bytes_per_second - bytes_received
        bytes_received = max_bytes_per_second
        return remaining  // Partial data received
      }
    }
    
    let get_throttle_stats = || {
      {
        max_bytes_per_second: max_bytes_per_second,
        bytes_sent: bytes_sent,
        bytes_received: bytes_received,
        remaining_send_quota: max_bytes_per_second - bytes_sent,
        remaining_receive_quota: max_bytes_per_second - bytes_received
      }
    }
    
    (send_data, receive_data, get_throttle_stats)
  }
  
  let (send_data, receive_data, get_throttle_stats) = bandwidth_throttler(1024)  // 1KB/s
  
  // Test bandwidth throttling
  assert_eq(send_data(512), 512)  // Send 512 bytes
  assert_eq(send_data(600), 512)  // Only 512 bytes remaining in quota
  
  let throttle_stats = get_throttle_stats()
  assert_eq(throttle_stats.bytes_sent, 1024)
  assert_eq(throttle_stats.remaining_send_quota, 0)
  
  assert_eq(receive_data(256), 256)
  assert_eq(receive_data(800), 768)  // Only 768 bytes remaining in quota
  
  let throttle_stats2 = get_throttle_stats()
  assert_eq(throttle_stats2.bytes_received, 1024)
  assert_eq(throttle_stats2.remaining_receive_quota, 0)
}

// Test 5: Resource Lifecycle Management
test "resource_lifecycle_management" {
  // Test resource lifecycle manager
  let resource_lifecycle_manager = || {
    let mut resources = {}
    let mut resource_id_counter = 0
    
    let create_resource = |resource_type, initial_state| {
      let resource_id = resource_id_counter
      resource_id_counter = resource_id_counter + 1
      
      let resource = {
        id: resource_id,
        type: resource_type,
        state: initial_state,
        created_at: 1640995200000L + resource_id.to_long(),
        last_updated: 1640995200000L + resource_id.to_long(),
        metadata: {}
      }
      
      resources[resource_id] = resource
      resource_id
    }
    
    let update_resource_state = |resource_id, new_state| {
      if resources.contains(resource_id) {
        resources[resource_id].state = new_state
        resources[resource_id].last_updated = 1640995200000L + 1000  // Current time
        return true
      }
      false
    }
    
    let set_resource_metadata = |resource_id, key, value| {
      if resources.contains(resource_id) {
        resources[resource_id].metadata[key] = value
        resources[resource_id].last_updated = 1640995200000L + 1000  // Current time
        return true
      }
      false
    }
    
    let delete_resource = |resource_id| {
      if resources.contains(resource_id) {
        resources.remove(resource_id)
        return true
      }
      false
    }
    
    let get_resource = |resource_id| {
      if resources.contains(resource_id) {
        Some(resources[resource_id])
      } else {
        None
      }
    }
    
    let find_resources_by_type = |resource_type| {
      let mut result = []
      
      for resource_id in resources {
        if resources[resource_id].type == resource_type {
          result.push(resources[resource_id])
        }
      }
      
      result
    }
    
    let find_resources_by_state = |state| {
      let mut result = []
      
      for resource_id in resources {
        if resources[resource_id].state == state {
          result.push(resources[resource_id])
        }
      }
      
      result
    }
    
    let get_lifecycle_stats = || {
      let mut state_counts = {}
      
      for resource_id in resources {
        let state = resources[resource_id].state
        if state_counts.contains(state) {
          state_counts[state] = state_counts[state] + 1
        } else {
          state_counts[state] = 1
        }
      }
      
      {
        total_resources: resources.length(),
        state_distribution: state_counts
      }
    }
    
    (create_resource, update_resource_state, set_resource_metadata, delete_resource,
     get_resource, find_resources_by_type, find_resources_by_state, get_lifecycle_stats)
  }
  
  let (create_res, update_state, set_metadata, delete_res, get_res,
       find_by_type, find_by_state, get_stats) = resource_lifecycle_manager()
  
  // Test resource lifecycle
  let res1 = create_res("database", "initializing")
  let res2 = create_res("cache", "ready")
  let res3 = create_res("database", "ready")
  
  assert_true(update_state(res1, "ready"))
  assert_true(set_metadata(res1, "host", "db.example.com"))
  assert_true(set_metadata(res1, "port", 5432))
  
  let resource = get_res(res1)
  match resource {
    Some(res) => {
      assert_eq(res.type, "database")
      assert_eq(res.state, "ready")
      assert_eq(res.metadata["host"], "db.example.com")
      assert_eq(res.metadata["port"], 5432)
    }
    None => assert_true(false)
  }
  
  let db_resources = find_by_type("database")
  assert_eq(db_resources.length(), 2)
  
  let ready_resources = find_by_state("ready")
  assert_eq(ready_resources.length(), 2)
  
  assert_true(delete_res(res2))
  assert_eq(find_by_type("cache").length(), 0)
  
  let lifecycle_stats = get_stats()
  assert_eq(lifecycle_stats.total_resources, 2)
  
  // Test state machine
  let state_machine = |states, transitions| {
    let mut current_state = states[0]
    let mut state_history = [current_state]
    
    let transition_to = |new_state| {
      let transition_key = current_state + "->" + new_state
      
      if transitions.contains(transition_key) {
        current_state = new_state
        state_history.push(current_state)
        return true
      }
      
      false
    }
    
    let get_current_state = || {
      current_state
    }
    
    let get_state_history = || {
      state_history
    }
    
    let get_valid_transitions = || {
      let mut valid = []
      
      for transition in transitions {
        let parts = transition.split("->")
        if parts.length() == 2 && parts[0] == current_state {
          valid.push(parts[1])
        }
      }
      
      valid
    }
    
    (transition_to, get_current_state, get_state_history, get_valid_transitions)
  }
  
  let states = ["idle", "connecting", "connected", "disconnecting", "error"]
  let transitions = [
    "idle->connecting",
    "connecting->connected",
    "connecting->error",
    "connected->disconnecting",
    "connected->error",
    "disconnecting->idle",
    "error->connecting",
    "error->idle"
  ]
  
  let (transition, get_state, get_history, get_valid) = state_machine(states, transitions)
  
  // Test state machine
  assert_eq(get_state(), "idle")
  
  assert_true(transition("connecting"))
  assert_eq(get_state(), "connecting")
  
  assert_true(transition("connected"))
  assert_eq(get_state(), "connected")
  
  assert_false(transition("idle"))  // Invalid transition
  assert_eq(get_state(), "connected")
  
  assert_true(transition("error"))
  assert_eq(get_state(), "error")
  
  let history = get_history()
  assert_eq(history, ["idle", "connecting", "connected", "error"])
  
  let valid_transitions = get_valid()
  assert_true(valid_transitions.contains("connecting"))
  assert_true(valid_transitions.contains("idle"))
  assert_false(valid_transitions.contains("disconnecting"))
  
  // Test resource dependency manager
  let dependency_manager = || {
    let mut dependencies = {}  // resource_id -> [dependency_ids]
    let mut dependents = {}    // resource_id -> [dependent_ids]
    
    let add_dependency = |resource_id, dependency_id| {
      if !dependencies.contains(resource_id) {
        dependencies[resource_id] = []
      }
      dependencies[resource_id].push(dependency_id)
      
      if !dependents.contains(dependency_id) {
        dependents[dependency_id] = []
      }
      dependents[dependency_id].push(resource_id)
    }
    
    let remove_dependency = |resource_id, dependency_id| {
      if dependencies.contains(resource_id) {
        let mut new_deps = []
        for dep in dependencies[resource_id] {
          if dep != dependency_id {
            new_deps.push(dep)
          }
        }
        dependencies[resource_id] = new_deps
      }
      
      if dependents.contains(dependency_id) {
        let mut new_deps = []
        for dep in dependents[dependency_id] {
          if dep != resource_id {
            new_deps.push(dep)
          }
        }
        dependents[dependency_id] = new_deps
      }
    }
    
    let get_dependencies = |resource_id| {
      if dependencies.contains(resource_id) {
        dependencies[resource_id]
      } else {
        []
      }
    }
    
    let get_dependents = |resource_id| {
      if dependents.contains(resource_id) {
        dependents[resource_id]
      } else {
        []
      }
    }
    
    let get_dependency_order = |resource_ids| {
      let mut ordered = []
      let mut visited = {}
      let mut visiting = {}
      
      let visit = |resource_id| {
        if visiting.contains(resource_id) {
          return false  // Circular dependency
        }
        
        if visited.contains(resource_id) {
          return true
        }
        
        visiting[resource_id] = true
        
        let deps = get_dependencies(resource_id)
        for dep in deps {
          if !visit(dep) {
            return false
          }
        }
        
        visiting.remove(resource_id)
        visited[resource_id] = true
        ordered.push(resource_id)
        
        true
      }
      
      for resource_id in resource_ids {
        if !visited.contains(resource_id) {
          if !visit(resource_id) {
            return []  // Circular dependency detected
          }
        }
      }
      
      ordered
    }
    
    (add_dependency, remove_dependency, get_dependencies, get_dependents, get_dependency_order)
  }
  
  let (add_dep, remove_dep, get_deps, get_dependents, get_order) = dependency_manager()
  
  // Test dependency management
  add_dep(3, 1)  // 3 depends on 1
  add_dep(3, 2)  // 3 depends on 2
  add_dep(4, 3)  // 4 depends on 3
  
  assert_eq(get_deps(3), [1, 2])
  assert_eq(get_deps(4), [3])
  assert_eq(get_dependents(1), [3])
  assert_eq(get_dependents(3), [4])
  
  let order = get_order([4, 3, 2, 1])
  assert_eq(order, [1, 2, 3, 4])  // Dependencies first
  
  // Test circular dependency detection
  add_dep(1, 4)  // Create circular dependency: 1->4->3->1
  let circular_order = get_order([1, 2, 3, 4])
  assert_eq(circular_order.length(), 0)  // Empty result indicates circular dependency
}

// Test 6: Resource Monitoring and Metrics
test "resource_monitoring_and_metrics" {
  // Test resource monitor
  let resource_monitor = || {
    let mut metrics = {
      "cpu_usage": [],
      "memory_usage": [],
      "disk_io": [],
      "network_io": []
    }
    let mut alerts = []
    
    let record_metric = |metric_name, value, timestamp| {
      if metrics.contains(metric_name) {
        metrics[metric_name].push({
          value: value,
          timestamp: timestamp
        })
      }
    }
    
    let get_metrics = |metric_name, start_time, end_time| {
      if metrics.contains(metric_name) {
        let mut filtered = []
        
        for metric in metrics[metric_name] {
          if metric.timestamp >= start_time && metric.timestamp <= end_time {
            filtered.push(metric)
          }
        }
        
        filtered
      } else {
        []
      }
    }
    
    let calculate_average = |metric_name, start_time, end_time| {
      let metrics_data = get_metrics(metric_name, start_time, end_time)
      
      if metrics_data.length() > 0 {
        let sum = metrics_data.reduce(|acc, metric| acc + metric.value, 0)
        sum / metrics_data.length()
      } else {
        0
      }
    }
    
    let calculate_maximum = |metric_name, start_time, end_time| {
      let metrics_data = get_metrics(metric_name, start_time, end_time)
      
      if metrics_data.length() > 0 {
        let mut max = metrics_data[0].value
        for metric in metrics_data {
          if metric.value > max {
            max = metric.value
          }
        }
        max
      } else {
        0
      }
    }
    
    let set_alert = |metric_name, threshold, operator| {
      alerts.push({
        metric: metric_name,
        threshold: threshold,
        operator: operator,
        triggered: false
      })
    }
    
    let check_alerts = |current_metrics| {
      let mut triggered_alerts = []
      
      for i = 0; i < alerts.length(); i = i + 1 {
        let alert = alerts[i]
        let current_value = current_metrics[alert.metric]
        
        let triggered = if alert.operator == ">" {
          current_value > alert.threshold
        } else if alert.operator == "<" {
          current_value < alert.threshold
        } else if alert.operator == "=" {
          current_value == alert.threshold
        } else {
          false
        }
        
        if triggered && !alert.triggered {
          triggered_alerts.push({
            metric: alert.metric,
            threshold: alert.threshold,
            current_value: current_value,
            message: alert.metric + " " + alert.operator + " " + alert.threshold.to_string() + " (current: " + current_value.to_string() + ")"
          })
          
          alerts[i].triggered = true
        } else if !triggered && alert.triggered {
          alerts[i].triggered = false
        }
      }
      
      triggered_alerts
    }
    
    let get_monitoring_summary = || {
      let mut summary = {}
      
      for metric_name in metrics {
        let metric_data = metrics[metric_name]
        if metric_data.length() > 0 {
          let latest = metric_data[metric_data.length() - 1].value
          let sum = metric_data.reduce(|acc, metric| acc + metric.value, 0)
          let avg = sum / metric_data.length()
          
          summary[metric_name] = {
            latest: latest,
            average: avg,
            count: metric_data.length()
          }
        }
      }
      
      summary
    }
    
    (record_metric, get_metrics, calculate_average, calculate_maximum,
     set_alert, check_alerts, get_monitoring_summary)
  }
  
  let (record_metric, get_metrics, calc_avg, calc_max, set_alert, check_alerts, get_summary) = resource_monitor()
  
  // Test resource monitoring
  let base_time = 1640995200000L
  
  // Record some metrics
  record_metric("cpu_usage", 25.5, base_time)
  record_metric("cpu_usage", 30.2, base_time + 60000L)  // 1 minute later
  record_metric("cpu_usage", 45.8, base_time + 120000L) // 2 minutes later
  
  record_metric("memory_usage", 1024, base_time)
  record_metric("memory_usage", 1080, base_time + 60000L)
  record_metric("memory_usage", 1152, base_time + 120000L)
  
  // Test metric retrieval
  let cpu_metrics = get_metrics("cpu_usage", base_time, base_time + 120000L)
  assert_eq(cpu_metrics.length(), 3)
  assert_eq(cpu_metrics[0].value, 25.5)
  assert_eq(cpu_metrics[2].value, 45.8)
  
  let memory_metrics = get_metrics("memory_usage", base_time + 30000L, base_time + 90000L)
  assert_eq(memory_metrics.length(), 2)
  assert_eq(memory_metrics[0].value, 1024)
  assert_eq(memory_metrics[1].value, 1080)
  
  // Test metric calculations
  let cpu_avg = calc_avg("cpu_usage", base_time, base_time + 120000L)
  assert_true(cpu_avg > 33.0 && cpu_avg < 34.0)  // (25.5 + 30.2 + 45.8) / 3
  
  let cpu_max = calc_max("cpu_usage", base_time, base_time + 120000L)
  assert_eq(cpu_max, 45.8)
  
  let memory_avg = calc_avg("memory_usage", base_time, base_time + 120000L)
  assert_eq(memory_avg, (1024 + 1080 + 1152) / 3)
  
  // Test alerts
  set_alert("cpu_usage", 40.0, ">")
  set_alert("memory_usage", 1200, ">")
  
  let current_metrics = {
    "cpu_usage": 45.5,
    "memory_usage": 1100
  }
  
  let triggered_alerts = check_alerts(current_metrics)
  assert_eq(triggered_alerts.length(), 1)
  assert_eq(triggered_alerts[0].metric, "cpu_usage")
  assert_true(triggered_alerts[0].message.contains("cpu_usage"))
  
  // Test monitoring summary
  let summary = get_summary()
  assert_true(summary.contains("cpu_usage"))
  assert_true(summary.contains("memory_usage"))
  
  assert_eq(summary["cpu_usage"].latest, 45.8)
  assert_eq(summary["cpu_usage"].count, 3)
  
  // Test performance profiler
  let performance_profiler = || {
    let mut profiles = {}
    let mut current_profile = null
    
    let start_profile = |profile_name| {
      current_profile = {
        name: profile_name,
        start_time: 1640995200000L,
        end_time: null,
        operations: []
      }
    }
    
    let record_operation = |operation_name, duration| {
      if current_profile != null {
        current_profile.operations.push({
          name: operation_name,
          duration: duration,
          timestamp: 1640995200000L
        })
      }
    }
    
    let end_profile = || {
      if current_profile != null {
        current_profile.end_time = 1640995200000L + 1000
        profiles[current_profile.name] = current_profile
        current_profile = null
      }
    }
    
    let get_profile = |profile_name| {
      if profiles.contains(profile_name) {
        Some(profiles[profile_name])
      } else {
        None
      }
    }
    
    let get_operation_stats = |profile_name, operation_name| {
      if profiles.contains(profile_name) {
        let profile = profiles[profile_name]
        let mut matching_operations = []
        
        for operation in profile.operations {
          if operation.name == operation_name {
            matching_operations.push(operation)
          }
        }
        
        if matching_operations.length() > 0 {
          let total_duration = matching_operations.reduce(|acc, op| acc + op.duration, 0)
          let avg_duration = total_duration / matching_operations.length()
          
          let mut min_duration = matching_operations[0].duration
          let mut max_duration = matching_operations[0].duration
          
          for operation in matching_operations {
            if operation.duration < min_duration {
              min_duration = operation.duration
            }
            if operation.duration > max_duration {
              max_duration = operation.duration
            }
          }
          
          Some({
            count: matching_operations.length(),
            total_duration: total_duration,
            average_duration: avg_duration,
            min_duration: min_duration,
            max_duration: max_duration
          })
        } else {
          None
        }
      } else {
        None
      }
    }
    
    (start_profile, record_operation, end_profile, get_profile, get_operation_stats)
  }
  
  let (start_profile, record_op, end_profile, get_profile, get_op_stats) = performance_profiler()
  
  // Test performance profiling
  start_profile("database_operations")
  
  record_op("connect", 50)
  record_op("query", 120)
  record_op("query", 95)
  record_op("disconnect", 30)
  
  end_profile()
  
  let profile = get_profile("database_operations")
  match profile {
    Some(p) => {
      assert_eq(p.name, "database_operations")
      assert_eq(p.operations.length(), 4)
      assert_eq(p.operations[0].name, "connect")
      assert_eq(p.operations[0].duration, 50)
    }
    None => assert_true(false)
  }
  
  let query_stats = get_op_stats("database_operations", "query")
  match query_stats {
    Some(stats) => {
      assert_eq(stats.count, 2)
      assert_eq(stats.total_duration, 215)
      assert_eq(stats.average_duration, 107.5)
      assert_eq(stats.min_duration, 95)
      assert_eq(stats.max_duration, 120)
    }
    None => assert_true(false)
  }
  
  // Test resource quota manager
  let quota_manager = |quotas| {
    let mut usage = {}
    
    // Initialize usage
    for resource in quotas {
      usage[resource] = 0
    }
    
    let check_quota = |resource, amount| {
      if quotas.contains(resource) && usage.contains(resource) {
        return usage[resource] + amount <= quotas[resource]
      }
      false
    }
    
    let consume_quota = |resource, amount| {
      if check_quota(resource, amount) {
        usage[resource] = usage[resource] + amount
        return true
      }
      false
    }
    
    let release_quota = |resource, amount| {
      if usage.contains(resource) {
        usage[resource] = usage[resource] - amount
        if usage[resource] < 0 {
          usage[resource] = 0
        }
        return true
      }
      false
    }
    
    let get_quota_usage = |resource| {
      if quotas.contains(resource) && usage.contains(resource) {
        {
          quota: quotas[resource],
          used: usage[resource],
          remaining: quotas[resource] - usage[resource]
        }
      } else {
        {
          quota: 0,
          used: 0,
          remaining: 0
        }
      }
    }
    
    let get_all_quota_usage = || {
      let mut all_usage = {}
      
      for resource in quotas {
        all_usage[resource] = get_quota_usage(resource)
      }
      
      all_usage
    }
    
    (check_quota, consume_quota, release_quota, get_quota_usage, get_all_quota_usage)
  }
  
  let quotas = {
    "cpu_time": 1000,
    "memory": 2048,
    "disk_io": 5000,
    "network_io": 10000
  }
  
  let (check_quota, consume_quota, release_quota, get_usage, get_all_usage) = quota_manager(quotas)
  
  // Test quota management
  assert_true(check_quota("cpu_time", 500))
  assert_true(consume_quota("cpu_time", 500))
  
  assert_true(check_quota("cpu_time", 400))
  assert_true(consume_quota("cpu_time", 400))
  
  assert_false(check_quota("cpu_time", 200))  // Only 100 remaining
  assert_false(consume_quota("cpu_time", 200))
  
  assert_true(release_quota("cpu_time", 300))
  assert_true(check_quota("cpu_time", 200))  // Now 300 remaining
  assert_true(consume_quota("cpu_time", 200))
  
  let cpu_usage = get_usage("cpu_time")
  assert_eq(cpu_usage.quota, 1000)
  assert_eq(cpu_usage.used, 400)
  assert_eq(cpu_usage.remaining, 600)
  
  let all_usage = get_all_usage()
  assert_eq(all_usage["cpu_time"].used, 400)
  assert_eq(all_usage["memory"].used, 0)
  assert_eq(all_usage["disk_io"].remaining, 5000)
}

// Test 7: Resource Cleanup and Finalization
test "resource_cleanup_and_finalization" {
  // Test cleanup manager
  let cleanup_manager = || {
    let mut cleanup_tasks = []
    let mut executed_tasks = []
    
    let register_cleanup_task = |task_name, cleanup_function| {
      cleanup_tasks.push({
        name: task_name,
        function: cleanup_function,
        executed: false
      })
    }
    
    let execute_cleanup_task = |task_name| {
      for i = 0; i < cleanup_tasks.length(); i = i + 1 {
        if cleanup_tasks[i].name == task_name && !cleanup_tasks[i].executed {
          let result = cleanup_tasks[i].function()
          cleanup_tasks[i].executed = true
          executed_tasks.push(task_name)
          return result
        }
      }
      false
    }
    
    let execute_all_cleanup_tasks = || {
      let mut results = []
      
      for task in cleanup_tasks {
        if !task.executed {
          let result = task.function()
          task.executed = true
          executed_tasks.push(task.name)
          results.push({
            name: task.name,
            result: result
          })
        }
      }
      
      results
    }
    
    let get_cleanup_status = || {
      let mut status = {}
      
      for task in cleanup_tasks {
        status[task.name] = task.executed
      }
      
      status
    }
    
    (register_cleanup_task, execute_cleanup_task, execute_all_cleanup_tasks, get_cleanup_status)
  }
  
  let (register_task, execute_task, execute_all, get_status) = cleanup_manager()
  
  // Test cleanup tasks
  register_task("close_files", || {
    // Simulate closing files
    true
  })
  
  register_task("release_memory", || {
    // Simulate releasing memory
    true
  })
  
  register_task("disconnect_network", || {
    // Simulate disconnecting network
    true
  })
  
  // Test individual task execution
  assert_true(execute_task("close_files"))
  assert_false(execute_task("close_files"))  // Already executed
  
  let status = get_status()
  assert_true(status["close_files"])
  assert_false(status["release_memory"])
  assert_false(status["disconnect_network"])
  
  // Test all tasks execution
  let results = execute_all()
  assert_eq(results.length(), 2)  // Two remaining tasks
  
  let final_status = get_status()
  assert_true(final_status["close_files"])
  assert_true(final_status["release_memory"])
  assert_true(final_status["disconnect_network"])
  
  // Test finalization queue
  let finalization_queue = || {
    let mut queue = []
    let mut processed = []
    
    let enqueue = |item, priority| {
      queue.push({
        item: item,
        priority: priority,
        enqueued_at: 1640995200000L
      })
      
      // Sort by priority (higher first)
      queue.sort_by(|a, b| b.priority - a.priority)
    }
    
    let dequeue = || {
      if queue.length() > 0 {
        let item = queue.shift()
        processed.push(item)
        return Some(item.item)
      }
      None
    }
    
    let process_all = || {
      let mut items = []
      
      while queue.length() > 0 {
        let item = queue.shift()
        items.push(item.item)
        processed.push(item)
      }
      
      items
    }
    
    let get_queue_status = || {
      {
        queue_size: queue.length(),
        processed_count: processed.length()
      }
    }
    
    (enqueue, dequeue, process_all, get_queue_status)
  }
  
  let (enqueue, dequeue, process_all, get_status) = finalization_queue()
  
  // Test finalization queue
  enqueue("cleanup1", 1)
  enqueue("cleanup2", 3)
  enqueue("cleanup3", 2)
  
  let item1 = dequeue()
  match item1 {
    Some(item) => assert_eq(item, "cleanup2")  // Highest priority
    None => assert_true(false)
  }
  
  let item2 = dequeue()
  match item2 {
    Some(item) => assert_eq(item, "cleanup3")  // Second highest priority
    None => assert_true(false)
  }
  
  let item3 = dequeue()
  match item3 {
    Some(item) => assert_eq(item, "cleanup3")  // Lowest priority
    None => assert_true(false)
  }
  
  // Test resource reference counting
  let reference_counter = || {
    let mut references = {}
    
    let acquire = |resource_id| {
      if references.contains(resource_id) {
        references[resource_id] = references[resource_id] + 1
      } else {
        references[resource_id] = 1
      }
      references[resource_id]
    }
    
    let release = |resource_id| {
      if references.contains(resource_id) {
        references[resource_id] = references[resource_id] - 1
        
        if references[resource_id] <= 0 {
          references.remove(resource_id)
          return 0  // Reference count reached zero
        }
        
        return references[resource_id]
      }
      0  // Resource doesn't exist
    }
    
    let get_reference_count = |resource_id| {
      if references.contains(resource_id) {
        references[resource_id]
      } else {
        0
      }
    }
    
    let get_all_reference_counts = || {
      references
    }
    
    (acquire, release, get_count, get_all_counts)
  }
  
  let (acquire, release, get_count, get_all_counts) = reference_counter()
  
  // Test reference counting
  assert_eq(acquire("resource1"), 1)
  assert_eq(acquire("resource1"), 2)
  assert_eq(acquire("resource1"), 3)
  
  assert_eq(get_count("resource1"), 3)
  
  assert_eq(release("resource1"), 2)
  assert_eq(release("resource1"), 1)
  
  assert_eq(get_count("resource1"), 1)
  
  assert_eq(release("resource1"), 0)  // Reference count reached zero
  assert_eq(get_count("resource1"), 0)
  
  // Test multiple resources
  assert_eq(acquire("resource2"), 1)
  assert_eq(acquire("resource3"), 2)
  
  let all_counts = get_all_counts()
  assert_eq(all_counts["resource2"], 1)
  assert_eq(all_counts["resource3"], 2)
  
  // Test resource finalizer
  let resource_finalizer = || {
    let mut finalizers = {}
    let mut finalized = []
    
    let register_finalizer = |resource_id, finalizer_function| {
      finalizers[resource_id] = finalizer_function
    }
    
    let finalize_resource = |resource_id| {
      if finalizers.contains(resource_id) {
        let result = finalizers[resource_id]()
        finalizers.remove(resource_id)
        finalized.push(resource_id)
        return result
      }
      false
    }
    
    let is_finalized = |resource_id| {
      finalized.contains(resource_id)
    }
    
    let get_finalization_status = || {
      {
        pending_finalizers: finalizers.keys().length(),
        finalized_resources: finalized.length(),
        finalized_ids: finalized
      }
    }
    
    (register_finalizer, finalize_resource, is_finalized, get_status)
  }
  
  let (register_finalizer, finalize, is_finalized, get_status) = resource_finalizer()
  
  // Test resource finalization
  register_finalizer("resource1", || {
    // Finalization logic for resource1
    true
  })
  
  register_finalizer("resource2", || {
    // Finalization logic for resource2
    true
  })
  
  assert_true(finalize("resource1"))
  assert_true(is_finalized("resource1"))
  assert_false(is_finalized("resource2"))
  
  let status = get_status()
  assert_eq(status.pending_finalizers, 1)
  assert_eq(status.finalized_resources, 1)
  assert_true(status.finalized_ids.contains("resource1"))
  
  // Test automatic resource management
  let auto_resource_manager = || {
    let mut resources = {}
    let mut resource_id_counter = 0
    
    let create_resource = |factory, finalizer| {
      let resource_id = resource_id_counter
      resource_id_counter = resource_id_counter + 1
      
      let resource = factory()
      resources[resource_id] = {
        data: resource,
        finalizer: finalizer,
        reference_count: 1
      }
      
      resource_id
    }
    
    let acquire_resource = |resource_id| {
      if resources.contains(resource_id) {
        resources[resource_id].reference_count = resources[resource_id].reference_count + 1
        return Some(resources[resource_id].data)
      }
      None
    }
    
    let release_resource = |resource_id| {
      if resources.contains(resource_id) {
        resources[resource_id].reference_count = resources[resource_id].reference_count - 1
        
        if resources[resource_id].reference_count <= 0 {
          // Finalize and remove resource
          resources[resource_id].finalizer(resources[resource_id].data)
          resources.remove(resource_id)
          return true
        }
      }
      false
    }
    
    let get_resource_info = |resource_id| {
      if resources.contains(resource_id) {
        Some({
          reference_count: resources[resource_id].reference_count
        })
      } else {
        None
      }
    }
    
    (create_resource, acquire_resource, release_resource, get_resource_info)
  }
  
  let (create_res, acquire_res, release_res, get_res_info) = auto_resource_manager()
  
  // Test automatic resource management
  let res1 = create_res(
    || { "resource_data" },
    || { /* cleanup logic */ }
  )
  
  let data = acquire_res(res1)
  match data {
    Some(d) => assert_eq(d, "resource_data")
    None => assert_true(false)
  }
  
  let info = get_res_info(res1)
  match info {
    Some(i) => assert_eq(i.reference_count, 2)  // 1 from create + 1 from acquire
    None => assert_true(false)
  }
  
  assert_false(release_res(res1))  // Still has 1 reference
  assert_true(release_res(res1))   // Finalized
  
  let info2 = get_res_info(res1)
  assert_eq(info2, None)  // Resource no longer exists
}

// Test 8: Resource Allocation Strategies
test "resource_allocation_strategies" {
  // Test first-fit allocation strategy
  let first_fit_allocator = |blocks| {
    let allocate = |size| {
      for i = 0; i < blocks.length(); i = i + 1 {
        if !blocks[i].allocated && blocks[i].size >= size {
          blocks[i].allocated = true
          return i
        }
      }
      -1  // No suitable block found
    }
    
    allocate
  }
  
  // Test best-fit allocation strategy
  let best_fit_allocator = |blocks| {
    let allocate = |size| {
      let mut best_index = -1
      let mut best_size = 999999
      
      for i = 0; i < blocks.length(); i = i + 1 {
        if !blocks[i].allocated && blocks[i].size >= size && blocks[i].size < best_size {
          best_index = i
          best_size = blocks[i].size
        }
      }
      
      if best_index >= 0 {
        blocks[best_index].allocated = true
      }
      
      best_index
    }
    
    allocate
  }
  
  // Test worst-fit allocation strategy
  let worst_fit_allocator = |blocks| {
    let allocate = |size| {
      let mut worst_index = -1
      let mut worst_size = 0
      
      for i = 0; i < blocks.length(); i = i + 1 {
        if !blocks[i].allocated && blocks[i].size >= size && blocks[i].size > worst_size {
          worst_index = i
          worst_size = blocks[i].size
        }
      }
      
      if worst_index >= 0 {
        blocks[worst_index].allocated = true
      }
      
      worst_index
    }
    
    allocate
  }
  
  // Test allocation strategies
  let memory_blocks = [
    { size: 100, allocated: false },
    { size: 200, allocated: false },
    { size: 50, allocated: false },
    { size: 300, allocated: false },
    { size: 150, allocated: false }
  ]
  
  let first_fit = first_fit_allocator(memory_blocks.copy())
  let best_fit = best_fit_allocator(memory_blocks.copy())
  let worst_fit = worst_fit_allocator(memory_blocks.copy())
  
  // Test first-fit
  let ff_index = first_fit(120)
  assert_eq(ff_index, 1)  // Second block (size 200)
  
  // Test best-fit
  let bf_index = best_fit(120)
  assert_eq(bf_index, 4)  // Fifth block (size 150) - smallest that fits
  
  // Test worst-fit
  let wf_index = worst_fit(120)
  assert_eq(wf_index, 3)  // Fourth block (size 300) - largest that fits
  
  // Test buddy system allocation
  let buddy_system_allocator = |total_size| {
    let mut free_blocks = {}
    let mut allocated_blocks = {}
    let mut block_id_counter = 0
    
    // Initialize with the whole memory as one free block
    free_blocks[total_size] = [0]  // size -> [block_ids]
    
    let allocate = |size| {
      // Find the smallest power of 2 that can accommodate the request
      let mut block_size = 1
      while block_size < size {
        block_size = block_size * 2
      }
      
      // Find a free block of the required size
      if free_blocks.contains(block_size) && free_blocks[block_size].length() > 0 {
        let block_id = free_blocks[block_size].shift()
        allocated_blocks[block_id] = block_size
        return block_id
      }
      
      // Try to split larger blocks
      let mut current_size = block_size * 2
      while current_size <= total_size {
        if free_blocks.contains(current_size) && free_blocks[current_size].length() > 0 {
          let parent_block_id = free_blocks[current_size].shift()
          
          // Split the block
          let left_block_id = block_id_counter
          block_id_counter = block_id_counter + 1
          let right_block_id = block_id_counter
          block_id_counter = block_id_counter + 1
          
          // Add the two halves to the free list
          let half_size = current_size / 2
          
          if !free_blocks.contains(half_size) {
            free_blocks[half_size] = []
          }
          free_blocks[half_size].push(left_block_id)
          free_blocks[half_size].push(right_block_id)
          
          // Try again with the smaller size
          current_size = half_size
          continue
        }
        current_size = current_size * 2
      }
      
      -1  // No suitable block found
    }
    
    let deallocate = |block_id| {
      if allocated_blocks.contains(block_id) {
        let block_size = allocated_blocks[block_id]
        allocated_blocks.remove(block_id)
        
        // Add back to free list
        if !free_blocks.contains(block_size) {
          free_blocks[block_size] = []
        }
        free_blocks[block_size].push(block_id)
        
        // Try to merge with buddy
        // This is simplified - a real implementation would check for buddies
        return true
      }
      false
    }
    
    let get_allocation_stats = || {
      let total_free = 0
      let total_allocated = 0
      
      for size in free_blocks {
        total_free = total_free + size * free_blocks[size].length()
      }
      
      for block_id in allocated_blocks {
        total_allocated = total_allocated + allocated_blocks[block_id]
      }
      
      {
        total_size: total_size,
        free_size: total_free,
        allocated_size: total_allocated
      }
    }
    
    (allocate, deallocate, get_allocation_stats)
  }
  
  let (allocate, deallocate, get_stats) = buddy_system_allocator(1024)
  
  // Test buddy system
  let block1 = allocate(100)
  let block2 = allocate(200)
  let block3 = allocate(300)
  
  assert_ne(block1, -1)
  assert_ne(block2, -1)
  assert_ne(block3, -1)
  
  assert_true(deallocate(block2))
  
  let stats = get_stats()
  assert_eq(stats.total_size, 1024)
  assert_true(stats.free_size > 0)
  assert_true(stats.allocated_size > 0)
  
  // Test slab allocator
  let slab_allocator = |slab_size, object_size| {
    let mut slabs = []
    let mut free_objects = []
    let mut slab_id_counter = 0
    
    let allocate = || {
      // If there's a free object, use it
      if free_objects.length() > 0 {
        return free_objects.pop()
      }
      
      // Create a new slab
      let slab_id = slab_id_counter
      slab_id_counter = slab_id_counter + 1
      
      let slab = {
        id: slab_id,
        objects: []
      }
      
      // Create objects in the slab
      for i = 0; i < slab_size; i = i + 1 {
        let object = {
          slab_id: slab_id,
          object_id: i,
          data: "object_data"
        }
        slab.objects.push(object)
        free_objects.push(object)
      }
      
      slabs.push(slab)
      free_objects.pop()  // Return one object
    }
    
    let deallocate = |object| {
      free_objects.push(object)
    }
    
    let get_allocator_stats = || {
      let total_objects = slabs.reduce(|acc, slab| acc + slab.objects.length(), 0)
      let free_objects_count = free_objects.length()
      let allocated_objects = total_objects - free_objects_count
      
      {
        total_slabs: slabs.length(),
        total_objects: total_objects,
        free_objects: free_objects_count,
        allocated_objects: allocated_objects
      }
    }
    
    (allocate, deallocate, get_allocator_stats)
  }
  
  let (allocate, deallocate, get_stats) = slab_allocator(10, 64)
  
  // Test slab allocator
  let obj1 = allocate()
  let obj2 = allocate()
  let obj3 = allocate()
  
  deallocate(obj2)
  
  let obj4 = allocate()  // Should reuse obj2
  
  let stats = get_stats()
  assert_eq(stats.total_slabs, 1)
  assert_eq(stats.total_objects, 10)
  assert_eq(stats.allocated_objects, 3)
  assert_eq(stats.free_objects, 7)
  
  // Test resource pooling with eviction
  let evicting_pool = |max_size, eviction_policy| {
    let mut pool = {}
    let mut access_order = []
    
    let get = |key| {
      if pool.contains(key) {
        // Update access order
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
        
        return Some(pool[key])
      }
      None
    }
    
    let put = |key, value| {
      if pool.contains(key) {
        // Update existing
        pool[key] = value
        
        // Update access order
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
      } else {
        // Add new
        if pool.length() < max_size {
          pool[key] = value
          access_order.push(key)
        } else {
          // Evict based on policy
          let key_to_evict = if eviction_policy == "LRU" {
            access_order[0]  // Least recently used
          } else if eviction_policy == "MRU" {
            access_order[access_order.length() - 1]  // Most recently used
          } else {
            access_order[0]  // Default to LRU
          }
          
          pool.remove(key_to_evict)
          
          // Remove from access order
          let mut new_order = []
          for k in access_order {
            if k != key_to_evict {
              new_order.push(k)
            }
          }
          access_order = new_order
          
          // Add new item
          pool[key] = value
          access_order.push(key)
        }
      }
    }
    
    let get_pool_stats = || {
      {
        size: pool.length(),
        max_size: max_size,
        eviction_policy: eviction_policy
      }
    }
    
    (get, put, get_pool_stats)
  }
  
  let (lru_get, lru_put, lru_stats) = evicting_pool(3, "LRU")
  let (mru_get, mru_put, mru_stats) = evicting_pool(3, "MRU")
  
  // Test LRU eviction
  lru_put("key1", "value1")
  lru_put("key2", "value2")
  lru_put("key3", "value3")
  
  assert_eq(lru_get("key1"), Some("value1"))
  assert_eq(lru_get("key2"), Some("value2"))
  
  lru_put("key4", "value4")  // Should evict key3 (least recently used)
  
  assert_eq(lru_get("key1"), Some("value1"))
  assert_eq(lru_get("key2"), Some("value2"))
  assert_eq(lru_get("key3"), None)  // Evicted
  assert_eq(lru_get("key4"), Some("value4"))
  
  // Test MRU eviction
  mru_put("key1", "value1")
  mru_put("key2", "value2")
  mru_put("key3", "value3")
  
  assert_eq(mru_get("key1"), Some("value1"))
  assert_eq(mru_get("key2"), Some("value2"))
  
  mru_put("key4", "value4")  // Should evict key3 (most recently used)
  
  assert_eq(mru_get("key1"), Some("value1"))
  assert_eq(mru_get("key2"), Some("value2"))
  assert_eq(mru_get("key3"), None)  // Evicted
  assert_eq(mru_get("key4"), Some("value4"))
}

// Test 9: Resource Scheduling and Prioritization
test "resource_scheduling_and_prioritization" {
  // Test priority queue
  let priority_queue = || {
    let mut queue = []
    
    let enqueue = |item, priority| {
      queue.push({
        item: item,
        priority: priority,
        enqueued_at: 1640995200000L
      })
      
      // Sort by priority (higher first)
      queue.sort_by(|a, b| b.priority - a.priority)
    }
    
    let dequeue = || {
      if queue.length() > 0 {
        let item = queue.shift()
        return Some(item.item)
      }
      None
    }
    
    let peek = || {
      if queue.length() > 0 {
        return Some(queue[0].item)
      }
      None
    }
    
    let get_queue_stats = || {
      {
        size: queue.length(),
        highest_priority: if queue.length() > 0 { Some(queue[0].priority) } else { None }
      }
    }
    
    (enqueue, dequeue, peek, get_stats)
  }
  
  let (enqueue, dequeue, peek, get_stats) = priority_queue()
  
  // Test priority queue
  enqueue("task1", 2)
  enqueue("task2", 5)
  enqueue("task3", 1)
  enqueue("task4", 3)
  
  let stats = get_stats()
  assert_eq(stats.size, 4)
  assert_eq(stats.highest_priority, Some(5))
  
  assert_eq(peek(), Some("task2"))  // Highest priority
  
  assert_eq(dequeue(), Some("task2"))  // Dequeue highest priority
  assert_eq(dequeue(), Some("task4"))
  assert_eq(dequeue(), Some("task1"))
  assert_eq(dequeue(), Some("task3"))
  assert_eq(dequeue(), None)  // Queue is empty
  
  // Test fair scheduler
  let fair_scheduler = || {
    let mut queues = {
      "high": [],
      "medium": [],
      "low": []
    }
    let mut last_served = "low"
    
    let enqueue = |item, priority| {
      if queues.contains(priority) {
        queues[priority].push({
          item: item,
          enqueued_at: 1640995200000L
        })
      }
    }
    
    let dequeue = || {
      // Round-robin between priority levels
      let priorities = ["high", "medium", "low"]
      let start_index = if last_served == "high" { 1 } else if last_served == "medium" { 2 } else { 0 }
      
      for i = 0; i < priorities.length(); i = i + 1 {
        let priority = priorities[(start_index + i) % priorities.length()]
        
        if queues[priority].length() > 0 {
          let item = queues[priority].shift()
          last_served = priority
          return Some(item.item)
        }
      }
      
      None
    }
    
    let get_scheduler_stats = || {
      {
        high_priority_count: queues["high"].length(),
        medium_priority_count: queues["medium"].length(),
        low_priority_count: queues["low"].length(),
        last_served: last_served
      }
    }
    
    (enqueue, dequeue, get_scheduler_stats)
  }
  
  let (enqueue, dequeue, get_stats) = fair_scheduler()
  
  // Test fair scheduler
  enqueue("task1", "high")
  enqueue("task2", "low")
  enqueue("task3", "medium")
  enqueue("task4", "high")
  enqueue("task5", "low")
  
  let stats = get_stats()
  assert_eq(stats.high_priority_count, 2)
  assert_eq(stats.medium_priority_count, 1)
  assert_eq(stats.low_priority_count, 2)
  
  // Should serve from high priority first (round-robin starts with high)
  assert_eq(dequeue(), Some("task1"))
  
  let stats2 = get_stats()
  assert_eq(stats2.last_served, "high")
  
  // Next should serve from medium
  assert_eq(dequeue(), Some("task3"))
  
  let stats3 = get_stats()
  assert_eq(stats3.last_served, "medium")
  
  // Next should serve from low
  assert_eq(dequeue(), Some("task2"))
  
  // Test weighted fair queueing
  let weighted_fair_queueing = || {
    let mut queues = {
      "queue1": { weight: 3, items: [], last_served: 0 },
      "queue2": { weight: 2, items: [], last_served: 0 },
      "queue3": { weight: 1, items: [], last_served: 0 }
    }
    
    let enqueue = |queue_name, item| {
      if queues.contains(queue_name) {
        queues[queue_name].items.push({
          item: item,
          enqueued_at: 1640995200000L
        })
      }
    }
    
    let dequeue = || {
      // Find the queue with the highest deficit
      let mut selected_queue = ""
      let mut max_deficit = -1
      
      for queue_name in queues {
        let queue = queues[queue_name]
        let deficit = queue.last_served - queue.items.length() * queue.weight
        
        if deficit > max_deficit && queue.items.length() > 0 {
          max_deficit = deficit
          selected_queue = queue_name
        }
      }
      
      if selected_queue != "" {
        let item = queues[selected_queue].items.shift()
        queues[selected_queue].last_served = queues[selected_queue].last_served + queues[selected_queue].weight
        return Some(item.item)
      }
      
      None
    }
    
    let get_queue_stats = || {
      let mut stats = {}
      
      for queue_name in queues {
        stats[queue_name] = {
          weight: queues[queue_name].weight,
          item_count: queues[queue_name].items.length(),
          last_served: queues[queue_name].last_served
        }
      }
      
      stats
    }
    
    (enqueue, dequeue, get_queue_stats)
  }
  
  let (enqueue, dequeue, get_stats) = weighted_fair_queueing()
  
  // Test weighted fair queueing
  enqueue("queue1", "item1")
  enqueue("queue1", "item2")
  enqueue("queue2", "item3")
  enqueue("queue3", "item4")
  
  // Queue1 has highest weight (3), so should be served first
  assert_eq(dequeue(), Some("item1"))
  
  // Queue1 should still have priority due to weight
  assert_eq(dequeue(), Some("item2"))
  
  // Now queue2 should be served (weight 2)
  assert_eq(dequeue(), Some("item3"))
  
  // Finally queue3 (weight 1)
  assert_eq(dequeue(), Some("item4"))
  
  // Test resource reservation system
  let reservation_system = |total_resources| {
    let mut reservations = []
    let mut allocated_resources = 0
    
    let make_reservation = |requester, amount, priority| {
      if allocated_resources + amount <= total_resources {
        let reservation = {
          id: reservations.length(),
          requester: requester,
          amount: amount,
          priority: priority,
          granted: true,
          created_at: 1640995200000L
        }
        
        reservations.push(reservation)
        allocated_resources = allocated_resources + amount
        
        return reservations.length() - 1
      } else {
        // Check if we can preempt lower priority reservations
        let mut potential_preemptions = []
        let mut preemptable_amount = 0
        
        for i = 0; i < reservations.length(); i = i + 1 {
          if reservations[i].granted && reservations[i].priority < priority {
            potential_preemptions.push(i)
            preemptable_amount = preemptable_amount + reservations[i].amount
          }
        }
        
        if allocated_resources - preemptable_amount + amount <= total_resources {
          // Preempt lower priority reservations
          for index in potential_preemptions {
            reservations[index].granted = false
            allocated_resources = allocated_resources - reservations[index].amount
          }
          
          let reservation = {
            id: reservations.length(),
            requester: requester,
            amount: amount,
            priority: priority,
            granted: true,
            created_at: 1640995200000L
          }
          
          reservations.push(reservation)
          allocated_resources = allocated_resources + amount
          
          return reservations.length() - 1
        }
        
        // Cannot fulfill request
        let reservation = {
          id: reservations.length(),
          requester: requester,
          amount: amount,
          priority: priority,
          granted: false,
          created_at: 1640995200000L
        }
        
        reservations.push(reservation)
        return reservations.length() - 1
      }
    }
    
    let release_reservation = |reservation_id| {
      if reservation_id < reservations.length() {
        let reservation = reservations[reservation_id]
        
        if reservation.granted {
          allocated_resources = allocated_resources - reservation.amount
          reservation.granted = false
          return true
        }
      }
      false
    }
    
    let get_reservation_status = |reservation_id| {
      if reservation_id < reservations.length() {
        Some(reservations[reservation_id])
      } else {
        None
      }
    }
    
    let get_system_stats = || {
      let granted_count = reservations.reduce(|acc, res| if res.granted { acc + 1 } else { acc }, 0)
      let denied_count = reservations.length() - granted_count
      
      {
        total_resources: total_resources,
        allocated_resources: allocated_resources,
        available_resources: total_resources - allocated_resources,
        total_reservations: reservations.length(),
        granted_reservations: granted_count,
        denied_reservations: denied_count
      }
    }
    
    (make_reservation, release_reservation, get_reservation_status, get_system_stats)
  }
  
  let (make_res, release_res, get_status, get_sys_stats) = reservation_system(100)
  
  // Test reservation system
  let res1 = make_res("user1", 30, 1)  // Low priority
  let res2 = make_res("user2", 20, 2)  // Medium priority
  let res3 = make_res("user3", 40, 3)  // High priority
  
  let status1 = get_status(res1)
  match status1 {
    Some(s) => assert_true(s.granted)
    None => assert_true(false)
  }
  
  let stats = get_sys_stats()
  assert_eq(stats.allocated_resources, 90)
  assert_eq(stats.available_resources, 10)
  
  // Try to make a large reservation that requires preemption
  let res4 = make_res("user4", 50, 5)  // Very high priority
  
  let status4 = get_status(res4)
  match status4 {
    Some(s) => {
      assert_true(s.granted)  // Should be granted by preempting lower priority
      assert_eq(s.amount, 50)
    }
    None => assert_true(false)
  }
  
  let stats2 = get_sys_stats()
  assert_eq(stats2.allocated_resources, 70)  // 50 (new) + 20 (res2)
  
  // Check that res1 was preempted
  let status1_2 = get_status(res1)
  match status1_2 {
    Some(s) => assert_false(s.granted)
    None => assert_true(false)
  }
}

// Test 10: Resource Deadlock Detection and Prevention
test "resource_deadlock_detection_and_prevention" {
  // Test resource allocation graph
  let resource_allocation_graph = || {
    let mut nodes = {}
    let mut edges = []
    
    let add_node = |node_id, node_type| {
      nodes[node_id] = {
        id: node_id,
        type: node_type,
        resources: [],
        waiting_for: []
      }
    }
    
    let add_edge = |from_node, to_node| {
      edges.push({
        from: from_node,
        to: to_node
      })
      
      // Update node waiting lists
      if nodes.contains(from_node) {
        nodes[from_node].waiting_for.push(to_node)
      }
    }
    
    let remove_edge = |from_node, to_node| {
      let mut new_edges = []
      for edge in edges {
        if edge.from != from_node || edge.to != to_node {
          new_edges.push(edge)
        }
      }
      edges = new_edges
      
      // Update node waiting lists
      if nodes.contains(from_node) {
        let mut new_waiting = []
        for waiting in nodes[from_node].waiting_for {
          if waiting != to_node {
            new_waiting.push(waiting)
          }
        }
        nodes[from_node].waiting_for = new_waiting
      }
    }
    
    let allocate_resource = |node_id, resource_id| {
      if nodes.contains(node_id) {
        nodes[node_id].resources.push(resource_id)
      }
    }
    
    let release_resource = |node_id, resource_id| {
      if nodes.contains(node_id) {
        let mut new_resources = []
        for resource in nodes[node_id].resources {
          if resource != resource_id {
            new_resources.push(resource)
          }
        }
        nodes[node_id].resources = new_resources
      }
    }
    
    let detect_deadlock = || {
      // Use cycle detection to find deadlocks
      let visited = {}
      let rec_stack = {}
      let mut deadlocks = []
      
      let has_cycle = |node_id, path| {
        if rec_stack.contains(node_id) {
          // Found a cycle
          let cycle_start = path.index_of(node_id)
          if cycle_start >= 0 {
            let cycle = path.slice(cycle_start, path.length())
            deadlocks.push(cycle)
          }
          return true
        }
        
        if visited.contains(node_id) {
          return false
        }
        
        visited[node_id] = true
        rec_stack[node_id] = true
        
        if nodes.contains(node_id) {
          for waiting in nodes[node_id].waiting_for {
            let mut new_path = []
            for p in path {
              new_path.push(p)
            }
            new_path.push(waiting)
            
            if has_cycle(waiting, new_path) {
              return true
            }
          }
        }
        
        rec_stack.remove(node_id)
        return false
      }
      
      for node_id in nodes {
        if !visited.contains(node_id) {
          has_cycle(node_id, [node_id])
        }
      }
      
      deadlocks
    }
    
    let get_graph_info = || {
      {
        node_count: nodes.length(),
        edge_count: edges.length(),
        nodes: nodes,
        edges: edges
      }
    }
    
    (add_node, add_edge, remove_edge, allocate_resource, release_resource, detect_deadlock, get_graph_info)
  }
  
  let (add_node, add_edge, remove_edge, alloc_res, release_res, detect_deadlock, get_info) = resource_allocation_graph()
  
  // Test deadlock detection
  add_node("P1", "process")
  add_node("P2", "process")
  add_node("P3", "process")
  
  alloc_res("P1", "R1")
  alloc_res("P2", "R2")
  alloc_res("P3", "R3")
  
  // Create a circular wait: P1 -> R2 -> P2 -> R3 -> P3 -> R1 -> P1
  add_edge("P1", "R2")
  add_edge("P2", "R3")
  add_edge("P3", "R1")
  
  let deadlocks = detect_deadlock()
  assert_eq(deadlocks.length(), 1)
  assert_eq(deadlocks[0].length(), 3)  // P1 -> P2 -> P3 -> P1
  
  // Test banker's algorithm for deadlock avoidance
  let bankers_algorithm = |processes, resources, available, maximum, allocation| {
    let work = available.copy()
    let mut finish = {}
    
    // Initialize finish array
    for process in processes {
      finish[process] = false
    }
    
    // Find a process whose needs can be satisfied
    let find_safe_process = || {
      for process in processes {
        if !finish[process] {
          let need = []
          for i = 0; i < resources.length(); i = i + 1 {
            need.push(maximum[process][i] - allocation[process][i])
          }
          
          let mut can_satisfy = true
          for i = 0; i < resources.length(); i = i + 1 {
            if need[i] > work[i] {
              can_satisfy = false
              break
            }
          }
          
          if can_satisfy {
            return Some(process)
          }
        }
      }
      None
    }
    
    // Safety algorithm
    let mut safe_sequence = []
    
    while true {
      let process = find_safe_process()
      
      match process {
        Some(p) => {
          // Simulate process execution
          for i = 0; i < resources.length(); i = i + 1 {
            work[i] = work[i] + allocation[p][i]
          }
          
          finish[p] = true
          safe_sequence.push(p)
        }
        None => {
          break
        }
      }
    }
    
    // Check if all processes finished
    let mut all_finished = true
    for process in processes {
      if !finish[process] {
        all_finished = false
        break
      }
    }
    
    {
      safe: all_finished,
      safe_sequence: safe_sequence
    }
  }
  
  // Test banker's algorithm
  let processes = ["P0", "P1", "P2", "P3", "P4"]
  let resources = ["R1", "R2", "R3"]
  
  let available = [3, 3, 2]
  
  let maximum = {
    "P0": [7, 5, 3],
    "P1": [3, 2, 2],
    "P2": [9, 0, 2],
    "P3": [2, 2, 2],
    "P4": [4, 3, 3]
  }
  
  let allocation = {
    "P0": [0, 1, 0],
    "P1": [2, 0, 0],
    "P2": [3, 0, 2],
    "P3": [2, 1, 1],
    "P4": [0, 0, 2]
  }
  
  let result = bankers_algorithm(processes, resources, available, maximum, allocation)
  assert_true(result.safe)
  assert_eq(result.safe_sequence.length(), 5)
  
  // Test resource request algorithm
  let resource_request = |process, request, available, maximum, allocation| {
    // Check if request exceeds need
    let need = []
    for i = 0; i < request.length(); i = i + 1 {
      need.push(maximum[process][i] - allocation[process][i])
    }
    
    for i = 0; i < request.length(); i = i + 1 {
      if request[i] > need[i] {
        return {
          safe: false,
          reason: "Request exceeds maximum need"
        }
      }
    }
    
    // Check if request exceeds available
    for i = 0; i < request.length(); i = i + 1 {
      if request[i] > available[i] {
        return {
          safe: false,
          reason: "Resources not available"
        }
      }
    }
    
    // Simulate allocation
    let mut new_available = []
    let mut new_allocation = allocation.copy()
    
    for i = 0; i < available.length(); i = i + 1 {
      new_available.push(available[i] - request[i])
      new_allocation[process][i] = allocation[process][i] + request[i]
    }
    
    // Check if system is still safe
    let safety_result = bankers_algorithm([process], resources, new_available, maximum, new_allocation)
    
    if safety_result.safe {
      return {
        safe: true,
        reason: "Request granted, system remains safe"
      }
    } else {
      return {
        safe: false,
        reason: "Request would lead to unsafe state"
      }
    }
  }
  
  // Test resource request
  let request = [1, 0, 2]
  let request_result = resource_request("P1", request, available, maximum, allocation)
  
  // Test wait-die and wound-wait deadlock prevention
  let wait_die_prevention = || {
    let mut transactions = {}
    let mut transaction_id_counter = 0
    
    let begin_transaction = || {
      let id = transaction_id_counter
      transaction_id_counter = transaction_id_counter + 1
      
      transactions[id] = {
        id: id,
        timestamp: 1640995200000L + id.to_long(),
        state: "active",
        resources_held: []
      }
      
      id
    }
    
    let request_resource = |transaction_id, resource_id| {
      if transactions.contains(transaction_id) {
        let transaction = transactions[transaction_id]
        
        // Check if resource is held by another transaction
        for other_id in transactions {
          if other_id != transaction_id {
            let other_transaction = transactions[other_id]
            
            if other_transaction.resources_held.contains(resource_id) {
              // Resource is held by another transaction
              if transaction.id < other_transaction.id {
                // Older transaction waits
                transaction.state = "waiting"
                return false
              } else {
                // Younger transaction dies (aborts)
                transaction.state = "aborted"
                return false
              }
            }
          }
        }
        
        // Resource is available, grant it
        transactions[transaction_id].resources_held.push(resource_id)
        return true
      }
      false
    }
    
    let release_resource = |transaction_id, resource_id| {
      if transactions.contains(transaction_id) {
        let resources = transactions[transaction_id].resources_held
        
        let mut new_resources = []
        for res in resources {
          if res != resource_id {
            new_resources.push(res)
          }
        }
        
        transactions[transaction_id].resources_held = new_resources
        return true
      }
      false
    }
    
    let get_transaction_state = |transaction_id| {
      if transactions.contains(transaction_id) {
        Some(transactions[transaction_id].state)
      } else {
        None
      }
    }
    
    (begin_transaction, request_resource, release_resource, get_transaction_state)
  }
  
  let (begin_tx, request_res, release_res, get_state) = wait_die_prevention()
  
  // Test wait-die
  let tx1 = begin_tx()
  let tx2 = begin_tx()
  
  assert_true(request_res(tx1, "R1"))  // tx1 gets R1
  assert_false(request_res(tx2, "R1"))  // tx2 waits (younger)
  
  let state1 = get_state(tx1)
  let state2 = get_state(tx2)
  
  match state1 {
    Some(s) => assert_eq(s, "active")
    None => assert_true(false)
  }
  
  match state2 {
    Some(s) => assert_eq(s, "waiting")
    None => assert_true(false)
  }
  
  // Test timeout-based deadlock detection
  let timeout_deadlock_detector = |timeout_ms| {
    let mut lock_requests = {}
    let mut current_time = 1640995200000L
    
    let request_lock = |transaction_id, resource_id| {
      let timestamp = current_time
      
      if lock_requests.contains(resource_id) {
        // Resource is already locked
        lock_requests[resource_id].waiters.push({
          transaction_id: transaction_id,
          timestamp: timestamp
        })
        
        return {
          granted: false,
          waiters: lock_requests[resource_id].waiters.length()
        }
      } else {
        // Grant the lock
        lock_requests[resource_id] = {
          holder: transaction_id,
          timestamp: timestamp,
          waiters: []
        }
        
        return {
          granted: true,
          waiters: 0
        }
      }
    }
    
    let release_lock = |resource_id| {
      if lock_requests.contains(resource_id) {
        let lock = lock_requests[resource_id]
        
        if lock.waiters.length() > 0 {
          // Grant lock to first waiter
          let next_waiter = lock.waiters[0]
          let remaining_waiters = []
          
          for i = 1; i < lock.waiters.length(); i = i + 1 {
            remaining_waiters.push(lock.waiters[i])
          }
          
          lock_requests[resource_id] = {
            holder: next_waiter.transaction_id,
            timestamp: next_waiter.timestamp,
            waiters: remaining_waiters
          }
          
          return Some(next_waiter.transaction_id)
        } else {
          lock_requests.remove(resource_id)
          return None
        }
      }
      None
    }
    
    let advance_time = |delta_ms| {
      current_time = current_time + delta_ms.to_long()
    }
    
    let detect_timeouts = || {
      let mut timed_out_transactions = []
      
      for resource_id in lock_requests {
        let lock = lock_requests[resource_id]
        
        for waiter in lock.waiters {
          let wait_time = current_time - waiter.timestamp
          
          if wait_time > timeout_ms.to_long() {
            timed_out_transactions.push({
              transaction_id: waiter.transaction_id,
              resource_id: resource_id,
              wait_time: wait_time
            })
          }
        }
      }
      
      timed_out_transactions
    }
    
    (request_lock, release_lock, advance_time, detect_timeouts)
  }
  
  let (request_lock, release_lock, advance_time, detect_timeouts) = timeout_deadlock_detector(5000)
  
  // Test timeout detection
  let result1 = request_lock("T1", "R1")
  assert_true(result1.granted)
  
  let result2 = request_lock("T2", "R1")
  assert_false(result2.granted)
  assert_eq(result2.waiters, 1)
  
  advance_time(6000)  // Advance time by 6 seconds
  
  let timeouts = detect_timeouts()
  assert_eq(timeouts.length(), 1)
  assert_eq(timeouts[0].transaction_id, "T2")
  assert_eq(timeouts[0].resource_id, "R1")
}