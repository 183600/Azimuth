// Azimuth 高质量综合测试套件
// 包含10个高质量测试用例，涵盖高级系统特性和边界条件

// 测试1: 高级并发控制和线程安全
test "高级并发控制和线程安全机制" {
  // 定义线程安全的计数器
  type AtomicCounter = {
    value: Int,
    lock: Mutex
  }
  
  // 创建原子计数器
  let create_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      lock: Mutex::new()
    }
  }
  
  // 线程安全的递增操作
  let atomic_increment = fn(counter: AtomicCounter) {
    let _ = Mutex::lock(counter.lock)
    counter.value = counter.value + 1
    let _ = Mutex::unlock(counter.lock)
  }
  
  // 线程安全的递减操作
  let atomic_decrement = fn(counter: AtomicCounter) {
    let _ = Mutex::lock(counter.lock)
    counter.value = counter.value - 1
    let _ = Mutex::unlock(counter.lock)
  }
  
  // 线程安全的读取操作
  let atomic_read = fn(counter: AtomicCounter) {
    let _ = Mutex::lock(counter.lock)
    let result = counter.value
    let _ = Mutex::unlock(counter.lock)
    result
  }
  
  // 创建计数器
  let counter = create_counter(0)
  
  // 模拟并发操作
  let threads = []
  for i in 0..10 {
    let thread = Thread::spawn(fn() {
      for j in 0..100 {
        if i % 2 == 0 {
          atomic_increment(counter)
        } else {
          atomic_decrement(counter)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    let _ = Thread::join(thread)
  }
  
  // 验证最终结果
  let final_value = atomic_read(counter)
  assert_eq(final_value, 0)  // 5个线程递增，5个线程递减，每个操作100次
  
  // 测试读写锁
  type ReadWriteCounter = {
    value: Int,
    rw_lock: RwLock
  }
  
  let create_rw_counter = fn(initial_value: Int) {
    {
      value: initial_value,
      rw_lock: RwLock::new()
    }
  }
  
  let rw_read = fn(counter: ReadWriteCounter) {
    let _ = RwLock::read_lock(counter.rw_lock)
    let result = counter.value
    let _ = RwLock::read_unlock(counter.rw_lock)
    result
  }
  
  let rw_write = fn(counter: ReadWriteCounter, delta: Int) {
    let _ = RwLock::write_lock(counter.rw_lock)
    counter.value = counter.value + delta
    let _ = RwLock::write_unlock(counter.rw_lock)
  }
  
  let rw_counter = create_rw_counter(42)
  
  // 测试读写操作
  assert_eq(rw_read(rw_counter), 42)
  rw_write(rw_counter, 8)
  assert_eq(rw_read(rw_counter), 50)
  
  // 测试条件变量
  type BoundedBuffer = {
    buffer: Array[Int],
    capacity: Int,
    head: Int,
    tail: Int,
    count: Int,
    mutex: Mutex,
    not_empty: CondVar,
    not_full: CondVar
  }
  
  let create_buffer = fn(capacity: Int) {
    {
      buffer: Array::make(capacity, 0),
      capacity,
      head: 0,
      tail: 0,
      count: 0,
      mutex: Mutex::new(),
      not_empty: CondVar::new(),
      not_full: CondVar::new()
    }
  }
  
  let buffer_put = fn(buf: BoundedBuffer, item: Int) {
    let _ = Mutex::lock(buf.mutex)
    
    while buf.count == buf.capacity {
      let _ = CondVar::wait(buf.not_full, buf.mutex)
    }
    
    buf.buffer[buf.tail] = item
    buf.tail = (buf.tail + 1) % buf.capacity
    buf.count = buf.count + 1
    
    let _ = CondVar::signal(buf.not_empty)
    let _ = Mutex::unlock(buf.mutex)
  }
  
  let buffer_get = fn(buf: BoundedBuffer) {
    let _ = Mutex::lock(buf.mutex)
    
    while buf.count == 0 {
      let _ = CondVar::wait(buf.not_empty, buf.mutex)
    }
    
    let item = buf.buffer[buf.head]
    buf.head = (buf.head + 1) % buf.capacity
    buf.count = buf.count - 1
    
    let _ = CondVar::signal(buf.not_full)
    let _ = Mutex::unlock(buf.mutex)
    
    item
  }
  
  let bounded_buffer = create_buffer(5)
  
  // 测试有界缓冲区
  buffer_put(bounded_buffer, 1)
  buffer_put(bounded_buffer, 2)
  buffer_put(bounded_buffer, 3)
  
  assert_eq(buffer_get(bounded_buffer), 1)
  assert_eq(buffer_get(bounded_buffer), 2)
  
  buffer_put(bounded_buffer, 4)
  buffer_put(bounded_buffer, 5)
  buffer_put(bounded_buffer, 6)
  
  assert_eq(buffer_get(bounded_buffer), 3)
  assert_eq(buffer_get(bounded_buffer), 4)
  assert_eq(buffer_get(bounded_buffer), 5)
  assert_eq(buffer_get(bounded_buffer), 6)
}

// 测试2: 内存管理和资源清理
test "内存管理和资源清理机制" {
  // 定义资源管理器
  type ResourceManager = {
    resources: Map[String, Resource],
    cleanup_queue: Array[String]
  }
  
  type Resource = {
    id: String,
    data: Array[Byte],
    is_active: Bool,
    ref_count: Int
  }
  
  // 创建资源管理器
  let create_manager = fn() {
    {
      resources: Map::empty(),
      cleanup_queue: []
    }
  }
  
  // 分配资源
  let allocate_resource = fn(manager: ResourceManager, id: String, size: Int) {
    let resource = {
      id,
      data: Array::make(size, 0),
      is_active: true,
      ref_count: 1
    }
    let _ = Map::insert(manager.resources, id, resource)
    id
  }
  
  // 增加引用计数
  let add_ref = fn(manager: ResourceManager, id: String) {
    match Map::get(manager.resources, id) {
      Some(mut resource) => {
        resource.ref_count = resource.ref_count + 1
        let _ = Map::insert(manager.resources, id, resource)
        true
      }
      None => false
    }
  }
  
  // 释放资源
  let release_resource = fn(manager: ResourceManager, id: String) {
    match Map::get(manager.resources, id) {
      Some(mut resource) => {
        resource.ref_count = resource.ref_count - 1
        if resource.ref_count <= 0 {
          resource.is_active = false
          manager.cleanup_queue = manager.cleanup_queue.push(id)
        }
        let _ = Map::insert(manager.resources, id, resource)
        true
      }
      None => false
    }
  }
  
  // 清理资源
  let cleanup_resources = fn(manager: ResourceManager) {
    let cleaned = []
    for id in manager.cleanup_queue {
      match Map::get(manager.resources, id) {
        Some(resource) => {
          if not resource.is_active {
            let _ = Map::remove(manager.resources, id)
            cleaned = cleaned.push(id)
          }
        }
        None => ()
      }
    }
    manager.cleanup_queue = []
    cleaned
  }
  
  // 测试资源管理
  let manager = create_manager()
  
  // 分配资源
  let resource1 = allocate_resource(manager, "res1", 1024)
  let resource2 = allocate_resource(manager, "res2", 2048)
  let resource3 = allocate_resource(manager, "res3", 4096)
  
  // 增加引用
  assert_true(add_ref(manager, resource1))
  assert_true(add_ref(manager, resource2))
  
  // 释放资源
  assert_true(release_resource(manager, resource1))  // ref_count: 1 -> 0
  assert_true(release_resource(manager, resource2))  // ref_count: 2 -> 1
  assert_true(release_resource(manager, resource3))  // ref_count: 1 -> 0
  
  // 检查资源状态
  let res1 = match Map::get(manager.resources, resource1) {
    Some(r) => r
    None => { id: "", data: [], is_active: false, ref_count: 0 }
  }
  assert_false(res1.is_active)
  assert_eq(res1.ref_count, 0)
  
  let res2 = match Map::get(manager.resources, resource2) {
    Some(r) => r
    None => { id: "", data: [], is_active: false, ref_count: 0 }
  }
  assert_true(res2.is_active)
  assert_eq(res2.ref_count, 1)
  
  // 清理资源
  let cleaned = cleanup_resources(manager)
  assert_eq(cleaned.length(), 2)
  assert_true(cleaned.contains(resource1))
  assert_true(cleaned.contains(resource3))
  
  // 验证资源已被清理
  assert_false(Map::contains(manager.resources, resource1))
  assert_true(Map::contains(manager.resources, resource2))
  assert_false(Map::contains(manager.resources, resource3))
  
  // 测试内存池
  type MemoryPool = {
    free_blocks: Array[Int],
    allocated_blocks: Map[Int, Int],
    total_size: Int,
    block_size: Int
  }
  
  let create_pool = fn(total_size: Int, block_size: Int) {
    let block_count = total_size / block_size
    let free_blocks = []
    for i in 0..block_count {
      free_blocks = free_blocks.push(i * block_size)
    }
    
    {
      free_blocks,
      allocated_blocks: Map::empty(),
      total_size,
      block_size
    }
  }
  
  let pool_allocate = fn(pool: MemoryPool, size: Int) {
    if size > pool.block_size {
      return None
    }
    
    if pool.free_blocks.length() > 0 {
      let address = pool.free_blocks[0]
      pool.free_blocks = pool.free_blocks.slice(1)
      let _ = Map::insert(pool.allocated_blocks, address, size)
      Some(address)
    } else {
      None
    }
  }
  
  let pool_deallocate = fn(pool: MemoryPool, address: Int) {
    match Map::get(pool.allocated_blocks, address) {
      Some(size) => {
        let _ = Map::remove(pool.allocated_blocks, address)
        pool.free_blocks = pool.free_blocks.push(address)
        true
      }
      None => false
    }
  }
  
  let pool = create_pool(1024, 64)
  
  // 测试内存池分配
  let addr1 = pool_allocate(pool, 32)
  let addr2 = pool_allocate(pool, 64)
  let addr3 = pool_allocate(pool, 48)
  
  assert_true(addr1.is_some())
  assert_true(addr2.is_some())
  assert_true(addr3.is_some())
  
  // 测试过大分配
  let large_addr = pool_allocate(pool, 128)
  assert_false(large_addr.is_some())
  
  // 测试释放
  assert_true(pool_deallocate(pool, addr1.unwrap()))
  assert_true(pool_deallocate(pool, addr2))
  assert_false(pool_deallocate(pool, 9999))  // 无效地址
  
  // 重新分配应该成功
  let addr4 = pool_allocate(pool, 32)
  assert_true(addr4.is_some())
  assert_eq(addr4.unwrap(), addr1.unwrap())  // 应该重用刚刚释放的块
}

// 测试3: 高性能数据处理
test "高性能数据处理和优化算法" {
  // 定义高效的数据结构
  type BloomFilter = {
    bit_array: Array[Bool],
    hash_functions: Array[(String) -> Int],
    size: Int
  }
  
  // 创建布隆过滤器
  let create_bloom_filter = fn(size: Int, hash_count: Int) {
    let hash_functions = []
    for i in 0..hash_count {
      let seed = i * 31 + 17
      let hash_fn = fn(key: String) {
        let mut hash = seed
        for c in key.chars() {
          hash = (hash * 31 + c.to_int()) % size
        }
        if hash < 0 { hash = hash + size }
        hash
      }
      hash_functions = hash_functions.push(hash_fn)
    }
    
    {
      bit_array: Array::make(size, false),
      hash_functions,
      size
    }
  }
  
  // 添加元素到布隆过滤器
  let bloom_add = fn(filter: BloomFilter, key: String) {
    for hash_fn in filter.hash_functions {
      let index = hash_fn(key)
      filter.bit_array[index] = true
    }
  }
  
  // 检查元素是否可能存在
  let bloom_contains = fn(filter: BloomFilter, key: String) {
    for hash_fn in filter.hash_functions {
      let index = hash_fn(key)
      if not filter.bit_array[index] {
        return false
      }
    }
    true
  }
  
  // 测试布隆过滤器
  let filter = create_bloom_filter(1000, 3)
  
  bloom_add(filter, "apple")
  bloom_add(filter, "banana")
  bloom_add(filter, "cherry")
  
  assert_true(bloom_contains(filter, "apple"))
  assert_true(bloom_contains(filter, "banana"))
  assert_true(bloom_contains(filter, "cherry"))
  
  // 假阴性不应该发生
  assert_false(bloom_contains(filter, "durian"))
  assert_false(bloom_contains(filter, "elderberry"))
  
  // 测试Skip List（跳表）
  type SkipNode = {
    key: Int,
    value: String,
    forward: Array[Option[SkipNode]]
  }
  
  type SkipList = {
    head: SkipNode,
    max_level: Int,
    level: Int,
    probability: Float
  }
  
  let create_skip_list = fn(max_level: Int, probability: Float) {
    let head = {
      key: Int::min_value(),
      value: "",
      forward: Array::make(max_level, None)
    }
    
    {
      head,
      max_level,
      level: 1,
      probability
    }
  }
  
  let random_level = fn(list: SkipList) {
    let mut level = 1
    let random = Random::next_float()
    while random < list.probability && level < list.max_level {
      level = level + 1
    }
    level
  }
  
  let skip_insert = fn(list: SkipList, key: Int, value: String) {
    let update = Array::make(list.max_level, None)
    let mut current = list.head
    
    // 从最高层开始查找插入位置
    for i in (list.level - 1)..0 {
      match current.forward[i] {
        Some(node) => {
          while node.key < key {
            current = node
            match current.forward[i] {
              Some(next_node) => node = next_node
              None => break
            }
          }
        }
        None => ()
      }
      update[i] = Some(current)
    }
    
    // 检查键是否已存在
    match current.forward[0] {
      Some(node) => {
        if node.key == key {
          node.value = value
          return
        }
      }
      None => ()
    }
    
    // 创建新节点
    let new_level = random_level(list)
    if new_level > list.level {
      for i in list.level..new_level {
        update[i] = Some(list.head)
      }
      list.level = new_level
    }
    
    let new_node = {
      key,
      value,
      forward: Array::make(new_level, None)
    }
    
    // 更新前向指针
    for i in 0..new_level {
      match update[i] {
        Some(prev_node) => {
          new_node.forward[i] = prev_node.forward[i]
          prev_node.forward[i] = Some(new_node)
        }
        None => ()
      }
    }
  }
  
  let skip_search = fn(list: SkipList, key: Int) {
    let mut current = list.head
    
    for i in (list.level - 1)..0 {
      match current.forward[i] {
        Some(node) => {
          while node.key < key {
            current = node
            match current.forward[i] {
              Some(next_node) => node = next_node
              None => break
            }
          }
        }
        None => ()
      }
    }
    
    match current.forward[0] {
      Some(node) => {
        if node.key == key {
          Some(node.value)
        } else {
          None
        }
      }
      None => None
    }
  }
  
  // 测试跳表
  let skip_list = create_skip_list(4, 0.5)
  
  skip_insert(skip_list, 10, "ten")
  skip_insert(skip_list, 20, "twenty")
  skip_insert(skip_list, 5, "five")
  skip_insert(skip_list, 15, "fifteen")
  skip_insert(skip_list, 25, "twenty-five")
  
  assert_eq(skip_search(skip_list, 10), Some("ten"))
  assert_eq(skip_search(skip_list, 20), Some("twenty"))
  assert_eq(skip_search(skip_list, 5), Some("five"))
  assert_eq(skip_search(skip_list, 15), Some("fifteen"))
  assert_eq(skip_search(skip_list, 25), Some("twenty-five"))
  assert_eq(skip_search(skip_list, 30), None)
  
  // 测试高效排序算法
  let quick_sort = fn(arr: Array[Int]) {
    let sort = fn(start: Int, end: Int) {
      if start < end {
        let pivot_index = partition(start, end)
        sort(start, pivot_index - 1)
        sort(pivot_index + 1, end)
      }
    }
    
    let partition = fn(start: Int, end: Int) {
      let pivot = arr[end]
      let mut i = start - 1
      
      for j in start..(end - 1) {
        if arr[j] <= pivot {
          i = i + 1
          // 交换arr[i]和arr[j]
          let temp = arr[i]
          arr[i] = arr[j]
          arr[j] = temp
        }
      }
      
      // 交换arr[i+1]和arr[end]
      let temp = arr[i + 1]
      arr[i + 1] = arr[end]
      arr[end] = temp
      
      i + 1
    }
    
    sort(0, arr.length() - 1)
  }
  
  // 测试快速排序
  let unsorted = [64, 34, 25, 12, 22, 11, 90, 88, 45, 50]
  let sorted = unsorted.copy()
  quick_sort(sorted)
  
  assert_eq(sorted, [11, 12, 22, 25, 34, 45, 50, 64, 88, 90])
  
  // 测试归并排序
  let merge_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      return arr
    }
    
    let mid = arr.length() / 2
    let left = merge_sort(arr.slice(0, mid))
    let right = merge_sort(arr.slice(mid))
    
    merge(left, right)
  }
  
  let merge = fn(left: Array[Int], right: Array[Int]) {
    let result = []
    let mut i = 0
    let mut j = 0
    
    while i < left.length() && j < right.length() {
      if left[i] <= right[j] {
        result = result.push(left[i])
        i = i + 1
      } else {
        result = result.push(right[j])
        j = j + 1
      }
    }
    
    while i < left.length() {
      result = result.push(left[i])
      i = i + 1
    }
    
    while j < right.length() {
      result = result.push(right[j])
      j = j + 1
    }
    
    result
  }
  
  let unsorted2 = [64, 34, 25, 12, 22, 11, 90, 88, 45, 50]
  let sorted2 = merge_sort(unsorted2)
  
  assert_eq(sorted2, [11, 12, 22, 25, 34, 45, 50, 64, 88, 90])
}

// 测试4: 分布式系统一致性
test "分布式系统一致性算法" {
  // 定义Raft共识算法的核心结构
  type LogEntry = {
    term: Int,
    index: Int,
    command: String
  }
  
  type NodeState = {
    id: Int,
    term: Int,
    voted_for: Option[Int],
    log: Array[LogEntry],
    commit_index: Int,
    last_applied: Int,
    state: String,  // "follower", "candidate", "leader"
    votes_received: Array[Int],
    election_timeout: Int
  }
  
  // 创建节点
  let create_node = fn(id: Int) {
    {
      id,
      term: 0,
      voted_for: None,
      log: [],
      commit_index: 0,
      last_applied: 0,
      state: "follower",
      votes_received: [],
      election_timeout: Random::range(150, 300)
    }
  }
  
  // 开始选举
  let start_election = fn(node: NodeState) {
    node.term = node.term + 1
    node.voted_for = Some(node.id)
    node.state = "candidate"
    node.votes_received = [node.id]
    node.election_timeout = Random::range(150, 300)
  }
  
  // 处理投票请求
  let handle_vote_request = fn(node: NodeState, candidate_term: Int, candidate_id: Int) {
    let mut vote_granted = false
    
    if candidate_term > node.term {
      node.term = candidate_term
      node.voted_for = Some(candidate_id)
      node.state = "follower"
      vote_granted = true
    } else if candidate_term == node.term {
      match node.voted_for {
        None => {
          node.voted_for = Some(candidate_id)
          vote_granted = true
        }
        Some(voted_for_id) => {
          vote_granted = voted_for_id == candidate_id
        }
      }
    }
    
    vote_granted
  }
  
  // 处理投票响应
  let handle_vote_response = fn(node: NodeState, voter_id: Int, vote_granted: Bool) {
    if node.state == "candidate" && vote_granted {
      node.votes_received = node.votes_received.push(voter_id)
      
      // 简化：假设3节点集群，获得2票即成为leader
      if node.votes_received.length() >= 2 {
        node.state = "leader"
      }
    }
  }
  
  // 添加日志条目
  let append_entry = fn(node: NodeState, command: String) {
    let entry = {
      term: node.term,
      index: node.log.length() + 1,
      command
    }
    node.log = node.log.push(entry)
  }
  
  // 提交日志条目
  let commit_entries = fn(node: NodeState, commit_index: Int) {
    if commit_index > node.commit_index {
      node.commit_index = commit_index
    }
  }
  
  // 创建集群
  let node1 = create_node(1)
  let node2 = create_node(2)
  let node3 = create_node(3)
  
  // 测试选举过程
  start_election(node1)
  assert_eq(node1.state, "candidate")
  assert_eq(node1.term, 1)
  assert_eq(node1.voted_for, Some(1))
  
  // 节点2和节点3处理投票请求
  let vote2 = handle_vote_request(node2, node1.term, node1.id)
  let vote3 = handle_vote_request(node3, node1.term, node1.id)
  
  assert_true(vote2)
  assert_true(vote3)
  assert_eq(node2.term, 1)
  assert_eq(node2.voted_for, Some(1))
  assert_eq(node2.state, "follower")
  
  // 节点1处理投票响应
  handle_vote_response(node1, node2.id, true)
  assert_eq(node1.state, "candidate")  // 还需要一票
  
  handle_vote_response(node1, node3.id, true)
  assert_eq(node1.state, "leader")  // 成为leader
  
  // 测试日志复制
  append_entry(node1, "set x = 5")
  append_entry(node1, "set y = 10")
  
  assert_eq(node1.log.length(), 2)
  assert_eq(node1.log[0].command, "set x = 5")
  assert_eq(node1.log[1].command, "set y = 10")
  
  // 提交日志条目
  commit_entries(node1, 2)
  assert_eq(node1.commit_index, 2)
  
  // 测试分布式锁
  type DistributedLock = {
    name: String,
    owner: Option[String],
    expiration: Int,
    version: Int
  }
  
  type LockService = {
    locks: Map[String, DistributedLock],
    current_time: Int
  }
  
  let create_lock_service = fn() {
    {
      locks: Map::empty(),
      current_time: 0
    }
  }
  
  let acquire_lock = fn(service: LockService, lock_name: String, owner: String, ttl: Int) {
    let current_time = service.current_time
    let lock = match Map::get(service.locks, lock_name) {
      Some(existing_lock) => existing_lock
      None => {
        let new_lock = {
          name: lock_name,
          owner: None,
          expiration: 0,
          version: 0
        }
        let _ = Map::insert(service.locks, lock_name, new_lock)
        new_lock
      }
    }
    
    // 检查锁是否可用
    let is_available = match lock.owner {
      None => true
      Some(lock_owner) => {
        lock_owner == owner || lock.expiration <= current_time
      }
    }
    
    if is_available {
      let updated_lock = {
        name: lock_name,
        owner: Some(owner),
        expiration: current_time + ttl,
        version: lock.version + 1
      }
      let _ = Map::insert(service.locks, lock_name, updated_lock)
      true
    } else {
      false
    }
  }
  
  let release_lock = fn(service: LockService, lock_name: String, owner: String) {
    match Map::get(service.locks, lock_name) {
      Some(lock) => {
        match lock.owner {
          Some(lock_owner) => {
            if lock_owner == owner {
              let updated_lock = {
                name: lock_name,
                owner: None,
                expiration: 0,
                version: lock.version + 1
              }
              let _ = Map::insert(service.locks, lock_name, updated_lock)
              true
            } else {
              false
            }
          }
          None => false
        }
      }
      None => false
    }
  }
  
  // 测试分布式锁
  let lock_service = create_lock_service()
  
  // 获取锁
  assert_true(acquire_lock(lock_service, "resource1", "client1", 1000))
  
  // 同一客户端再次获取锁应该成功
  assert_true(acquire_lock(lock_service, "resource1", "client1", 1000))
  
  // 不同客户端获取锁应该失败
  assert_false(acquire_lock(lock_service, "resource1", "client2", 1000))
  
  // 释放锁
  assert_true(release_lock(lock_service, "resource1", "client1"))
  
  // 释放后其他客户端可以获取
  assert_true(acquire_lock(lock_service, "resource1", "client2", 1000))
  
  // 原客户端无法释放其他客户端的锁
  assert_false(release_lock(lock_service, "resource1", "client1"))
  
  // 锁的所有者可以释放
  assert_true(release_lock(lock_service, "resource1", "client2"))
}

// 测试5: 安全性和加密测试
test "安全性和加密功能测试" {
  // 简化的哈希函数
  let simple_hash = fn(data: String) {
    let mut hash = 5381
    for c in data.chars() {
      hash = ((hash << 5) + hash) + c.to_int()
    }
    hash
  }
  
  // 测试哈希函数
  let hash1 = simple_hash("hello")
  let hash2 = simple_hash("hello")
  let hash3 = simple_hash("world")
  
  assert_eq(hash1, hash2)  // 相同输入应产生相同哈希
  assert_ne(hash1, hash3)  // 不同输入应产生不同哈希
  
  // 简化的HMAC实现
  let hmac = fn(key: String, message: String) {
    let block_size = 64
    let key_hash = if key.length() > block_size {
      simple_hash(key).to_string()
    } else {
      key
    }
    
    let key_padded = key_hash + String::make(block_size - key_hash.length(), '\0')
    
    let inner_key_pad = ""
    let outer_key_pad = ""
    
    for i in 0..block_size {
      let byte = key_padded[i]
      inner_key_pad = inner_key_pad + String::from_char(byte.to_int() ^ 0x36)
      outer_key_pad = outer_key_pad + String::from_char(byte.to_int() ^ 0x5C)
    }
    
    let inner_hash = simple_hash(inner_key_pad + message)
    simple_hash(outer_key_pad + inner_hash.to_string())
  }
  
  // 测试HMAC
  let key = "secret_key"
  let message = "test_message"
  let hmac1 = hmac(key, message)
  let hmac2 = hmac(key, message)
  let hmac3 = hmac("different_key", message)
  
  assert_eq(hmac1, hmac2)  // 相同密钥和消息应产生相同HMAC
  assert_ne(hmac1, hmac3)  // 不同密钥应产生不同HMAC
  
  // 简化的对称加密
  type CipherResult = {
    data: Array[Byte],
    iv: Array[Byte]
  }
  
  let simple_encrypt = fn(plaintext: String, key: String) {
    let key_bytes = key.bytes()
    let plaintext_bytes = plaintext.bytes()
    
    // 生成初始化向量
    let iv = []
    for i in 0..16 {
      iv = iv.push(Random::range(0, 256) as Byte)
    }
    
    // 简化的XOR加密
    let encrypted = []
    for i in 0..plaintext_bytes.length() {
      let key_byte = key_bytes[i % key_bytes.length()]
      let iv_byte = iv[i % iv.length()]
      let encrypted_byte = plaintext_bytes[i] ^ key_byte ^ iv_byte
      encrypted = encrypted.push(encrypted_byte)
    }
    
    {
      data: encrypted,
      iv
    }
  }
  
  let simple_decrypt = fn(cipher: CipherResult, key: String) {
    let key_bytes = key.bytes()
    let iv = cipher.iv
    let encrypted = cipher.data
    
    // 简化的XOR解密
    let decrypted = []
    for i in 0..encrypted.length() {
      let key_byte = key_bytes[i % key_bytes.length()]
      let iv_byte = iv[i % iv.length()]
      let decrypted_byte = encrypted[i] ^ key_byte ^ iv_byte
      decrypted = decrypted.push(decrypted_byte)
    }
    
    String::from_bytes(decrypted)
  }
  
  // 测试加密解密
  let plaintext = "This is a secret message"
  let key = "encryption_key"
  
  let encrypted = simple_encrypt(plaintext, key)
  let decrypted = simple_decrypt(encrypted, key)
  
  assert_eq(plaintext, decrypted)  // 解密后应恢复原始文本
  
  // 使用不同密钥解密应该失败
  let wrong_key = "wrong_key"
  let wrong_decrypted = simple_decrypt(encrypted, wrong_key)
  assert_ne(plaintext, wrong_decrypted)
  
  // 数字签名
  type DigitalSignature = {
    message: String,
    signature: String,
    public_key: String
  }
  
  let generate_key_pair = fn() {
    let private_key = Random::range(1000000, 9999999).to_string()
    let public_key = simple_hash(private_key).to_string()
    (private_key, public_key)
  }
  
  let sign_message = fn(message: String, private_key: String) {
    simple_hash(message + private_key).to_string()
  }
  
  let verify_signature = fn(signature: DigitalSignature) {
    let expected_signature = sign_message(signature.message, signature.public_key)
    signature.signature == expected_signature
  }
  
  // 测试数字签名
  let (private_key, public_key) = generate_key_pair()
  let message = "Important message to sign"
  let signature = sign_message(message, private_key)
  
  let digital_signature = {
    message,
    signature,
    public_key
  }
  
  assert_true(verify_signature(digital_signature))
  
  // 修改消息后验证应该失败
  let tampered_signature = {
    message: "Tampered message",
    signature,
    public_key
  }
  
  assert_false(verify_signature(tampered_signature))
  
  // 证书验证
  type Certificate = {
    subject: String,
    issuer: String,
    public_key: String,
    valid_from: Int,
    valid_to: Int,
    signature: String
  }
  
  type CertificateAuthority = {
    name: String,
    private_key: String,
    public_key: String
  }
  
  let create_ca = fn(name: String) {
    let (private_key, public_key) = generate_key_pair()
    {
      name,
      private_key,
      public_key
    }
  }
  
  let issue_certificate = fn(ca: CertificateAuthority, subject: String, subject_public_key: String, valid_days: Int) {
    let current_time = Time::now()
    let valid_from = current_time
    let valid_to = current_time + (valid_days * 24 * 60 * 60)
    
    let cert_data = subject + ca.name + subject_public_key + valid_from.to_string() + valid_to.to_string()
    let signature = sign_message(cert_data, ca.private_key)
    
    {
      subject,
      issuer: ca.name,
      public_key: subject_public_key,
      valid_from,
      valid_to,
      signature
    }
  }
  
  let verify_certificate = fn(cert: Certificate, ca_public_key: String, current_time: Int) {
    // 检查有效期
    if current_time < cert.valid_from || current_time > cert.valid_to {
      return false
    }
    
    // 验证签名
    let cert_data = cert.subject + cert.issuer + cert.public_key + cert.valid_from.to_string() + cert.valid_to.to_string()
    let expected_signature = sign_message(cert_data, ca_public_key)
    
    cert.signature == expected_signature
  }
  
  // 测试证书
  let ca = create_ca("Azimuth CA")
  let (server_private_key, server_public_key) = generate_key_pair()
  
  let cert = issue_certificate(ca, "azimuth-server", server_public_key, 365)
  
  assert_eq(cert.subject, "azimuth-server")
  assert_eq(cert.issuer, "Azimuth CA")
  assert_eq(cert.public_key, server_public_key)
  
  // 验证有效证书
  let current_time = Time::now()
  assert_true(verify_certificate(cert, ca.public_key, current_time))
  
  // 验证过期证书
  let future_time = current_time + (400 * 24 * 60 * 60)  // 400天后
  assert_false(verify_certificate(cert, ca.public_key, future_time))
  
  // 验证未生效证书
  let past_time = current_time - (24 * 60 * 60)  // 1天前
  assert_false(verify_certificate(cert, ca.public_key, past_time))
}

// 测试6: 实时流处理
test "实时流处理和事件驱动架构" {
  // 定义事件类型
  type Event = {
    id: String,
    type: String,
    timestamp: Int,
    data: Map[String, String],
    source: String
  }
  
  // 定义流处理器
  type StreamProcessor = {
    name: String,
    filters: Array[(Event) -> Bool],
    transformers: Array[(Event) -> Event],
    sinks: Array[(Event) -> Unit]
  }
  
  // 创建事件
  let create_event = fn(event_type: String, data: Map[String, String], source: String) {
    {
      id: Random::uuid4(),
      type: event_type,
      timestamp: Time::now(),
      data,
      source
    }
  }
  
  // 创建流处理器
  let create_processor = fn(name: String) {
    {
      name,
      filters: [],
      transformers: [],
      sinks: []
    }
  }
  
  // 添加过滤器
  let add_filter = fn(processor: StreamProcessor, filter: (Event) -> Bool) {
    processor.filters = processor.filters.push(filter)
  }
  
  // 添加转换器
  let add_transformer = fn(processor: StreamProcessor, transformer: (Event) -> Event) {
    processor.transformers = processor.transformers.push(transformer)
  }
  
  // 添加输出
  let add_sink = fn(processor: StreamProcessor, sink: (Event) -> Unit) {
    processor.sinks = processor.sinks.push(sink)
  }
  
  // 处理事件
  let process_event = fn(processor: StreamProcessor, event: Event) {
    let mut current_event = event
    
    // 应用过滤器
    for filter in processor.filters {
      if not filter(current_event) {
        return  // 事件被过滤掉
      }
    }
    
    // 应用转换器
    for transformer in processor.transformers {
      current_event = transformer(current_event)
    }
    
    // 发送到输出
    for sink in processor.sinks {
      sink(current_event)
    }
  }
  
  // 测试流处理器
  let processor = create_processor("test_processor")
  
  // 添加过滤器：只处理"metric"类型的事件
  add_filter(processor, fn(event) { event.type == "metric" })
  
  // 添加过滤器：只处理包含"service"属性的事件
  add_filter(processor, fn(event) { 
    match Map::get(event.data, "service") {
      Some(_) => true
      None => false
    }
  })
  
  // 添加转换器：添加处理时间戳
  add_transformer(processor, fn(event) {
    let updated_data = Map::insert(event.data, "processed_at", Time::now().to_string())
    {
      id: event.id,
      type: event.type,
      timestamp: event.timestamp,
      data: updated_data,
      source: event.source
    }
  })
  
  // 添加转换器：增强事件类型
  add_transformer(processor, fn(event) {
    {
      id: event.id,
      type: event.type + "_processed",
      timestamp: event.timestamp,
      data: event.data,
      source: event.source
    }
  })
  
  // 添加输出：收集处理后的事件
  let processed_events = []
  add_sink(processor, fn(event) {
    processed_events = processed_events.push(event)
  })
  
  // 创建测试事件
  let metric_event = create_event("metric", Map::from([("service", "api"), ("value", "42")]), "service-a")
  let log_event = create_event("log", Map::from([("service", "web"), ("message", "error")]), "service-b")
  let metric_event_no_service = create_event("metric", Map::from([("value", "100")]), "service-c")
  
  // 处理事件
  process_event(processor, metric_event)
  process_event(processor, log_event)  // 应该被过滤掉（类型不匹配）
  process_event(processor, metric_event_no_service)  // 应该被过滤掉（缺少service属性）
  
  // 验证处理结果
  assert_eq(processed_events.length(), 1)
  assert_eq(processed_events[0].type, "metric_processed")
  assert_true(Map::contains(processed_events[0].data, "processed_at"))
  assert_eq(Map::get(processed_events[0].data, "service"), Some("api"))
  
  // 测试窗口聚合
  type WindowAggregator = {
    window_size_ms: Int,
    events: Array[Event],
    aggregations: Map[String, Float]
  }
  
  let create_aggregator = fn(window_size_ms: Int) {
    {
      window_size_ms,
      events: [],
      aggregations: Map::empty()
    }
  }
  
  let add_event = fn(aggregator: WindowAggregator, event: Event) {
    aggregator.events = aggregator.events.push(event)
    
    // 清理过期事件
    let current_time = Time::now()
    let cutoff_time = current_time - aggregator.window_size_ms
    
    aggregator.events = aggregator.events.filter(fn(e) { e.timestamp >= cutoff_time })
  }
  
  let calculate_aggregations = fn(aggregator: WindowAggregator) {
    if aggregator.events.length() == 0 {
      aggregator.aggregations = Map::empty()
      return
    }
    
    // 按事件类型分组
    let grouped = Map::empty()
    for event in aggregator.events {
      let values = match Map::get(grouped, event.type) {
        Some(vals) => vals
        None => []
      }
      
      match Map::get(event.data, "value") {
        Some(value_str) => {
          let value = value_str.parse_float()
          let updated_values = values.push(value)
          let _ = Map::insert(grouped, event.type, updated_values)
        }
        None => ()
      }
    }
    
    // 计算每种事件类型的平均值
    let aggregations = Map::empty()
    for (event_type, values) in grouped {
      if values.length() > 0 {
        let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
        let avg = sum / (values.length() as Float)
        let _ = Map::insert(aggregations, event_type + "_avg", avg)
        let _ = Map::insert(aggregations, event_type + "_count", values.length() as Float)
      }
    }
    
    aggregator.aggregations = aggregations
  }
  
  // 测试窗口聚合
  let aggregator = create_aggregator(60000)  // 1分钟窗口
  
  let current_time = Time::now()
  
  // 添加事件
  let event1 = create_event("metric", Map::from([("value", "10")]), "service-a")
  event1.timestamp = current_time - 30000  // 30秒前
  add_event(aggregator, event1)
  
  let event2 = create_event("metric", Map::from([("value", "20")]), "service-b")
  event2.timestamp = current_time - 20000  // 20秒前
  add_event(aggregator, event2)
  
  let event3 = create_event("metric", Map::from([("value", "30")]), "service-c")
  event3.timestamp = current_time - 10000  // 10秒前
  add_event(aggregator, event3)
  
  let event4 = create_event("log", Map::from([("value", "1")]), "service-d")
  event4.timestamp = current_time - 5000   // 5秒前
  add_event(aggregator, event4)
  
  let event5 = create_event("metric", Map::from([("value", "40")]), "service-e")
  event5.timestamp = current_time - 70000  // 70秒前（超出窗口）
  add_event(aggregator, event5)
  
  // 计算聚合
  calculate_aggregations(aggregator)
  
  // 验证聚合结果
  assert_eq(Map::get(aggregator.aggregations, "metric_avg"), Some(20.0))  // (10+20+30)/3
  assert_eq(Map::get(aggregator.aggregations, "metric_count"), Some(3.0))
  assert_eq(Map::get(aggregator.aggregations, "log_avg"), Some(1.0))
  assert_eq(Map::get(aggregator.aggregations, "log_count"), Some(1.0))
  
  // 测试复杂事件处理（CEP）
  type Pattern = {
    name: String,
    conditions: Array[(Event) -> Bool],
    time_window_ms: Int,
    required_count: Int
  }
  
  type PatternMatcher = {
    patterns: Array[Pattern],
    matched_events: Array[Array[Event]]
  }
  
  let create_pattern_matcher = fn() {
    {
      patterns: [],
      matched_events: []
    }
  }
  
  let add_pattern = fn(matcher: PatternMatcher, pattern: Pattern) {
    matcher.patterns = matcher.patterns.push(pattern)
  }
  
  let match_patterns = fn(matcher: PatternMatcher, events: Array[Event]) {
    for pattern in matcher.patterns {
      let matching_events = []
      
      for event in events {
        let mut matches_all = true
        
        for condition in pattern.conditions {
          if not condition(event) {
            matches_all = false
            break
          }
        }
        
        if matches_all {
          matching_events = matching_events.push(event)
        }
      }
      
      // 检查时间窗口和数量要求
      if matching_events.length() >= pattern.required_count {
        let sorted_events = matching_events.sort(fn(a, b) { 
          if a.timestamp < b.timestamp { -1 } 
          else if a.timestamp > b.timestamp { 1 } 
          else { 0 } 
        })
        
        let mut window_start = 0
        let mut current_window = []
        
        for event in sorted_events {
          current_window = current_window.push(event)
          
          // 检查窗口大小
          while current_window.length() > 0 && 
                event.timestamp - current_window[0].timestamp > pattern.time_window_ms {
            current_window = current_window.slice(1)
          }
          
          if current_window.length() >= pattern.required_count {
            matcher.matched_events = matcher.matched_events.push(current_window.copy())
          }
        }
      }
    }
  }
  
  // 测试模式匹配
  let matcher = create_pattern_matcher()
  
  // 添加模式：检测5秒内3个错误事件
  let error_pattern = {
    name: "error_spike",
    conditions: [
      fn(event) { event.type == "log" },
      fn(event) { 
        match Map::get(event.data, "level") {
          Some(level) => level == "error"
          None => false
        }
      }
    ],
    time_window_ms: 5000,
    required_count: 3
  }
  
  add_pattern(matcher, error_pattern)
  
  // 创建测试事件
  let error_events = []
  for i in 0..5 {
    let event = create_event("log", Map::from([("level", "error"), ("message", "Error " + i.to_string())]), "service-error")
    event.timestamp = current_time + (i * 1000)  // 每秒一个错误
    error_events = error_events.push(event)
  }
  
  // 添加一些非错误事件
  let info_event = create_event("log", Map::from([("level", "info"), ("message", "Info")]), "service-info")
  info_event.timestamp = current_time + 2500
  error_events = error_events.push(info_event)
  
  // 匹配模式
  match_patterns(matcher, error_events)
  
  // 验证匹配结果
  assert_eq(matcher.matched_events.length(), 3)  // 5个错误事件在5秒窗口内，应该有3个匹配（事件0-2，事件1-3，事件2-4）
  assert_eq(matcher.matched_events[0].length(), 3)
  assert_eq(matcher.matched_events[1].length(), 3)
  assert_eq(matcher.matched_events[2].length(), 3)
}

// 测试7: 自适应性能调优
test "自适应性能调优和动态优化" {
  // 定义性能指标
  type PerformanceMetrics = {
    cpu_usage: Float,
    memory_usage: Float,
    throughput: Float,
    latency: Float,
    error_rate: Float,
    timestamp: Int
  }
  
  // 定义调优策略
  type TuningStrategy = {
    name: String,
    condition: (PerformanceMetrics) -> Bool,
    action: (Configuration) -> Configuration,
    priority: Int
  }
  
  // 定义配置
  type Configuration = {
    thread_pool_size: Int,
    batch_size: Int,
    cache_size: Int,
    timeout_ms: Int,
    retry_count: Int,
    compression_enabled: Bool
  }
  
  // 创建默认配置
  let default_config = {
    thread_pool_size: 4,
    batch_size: 100,
    cache_size: 1000,
    timeout_ms: 5000,
    retry_count: 3,
    compression_enabled: true
  }
  
  // 创建性能指标
  let create_metrics = fn(cpu: Float, memory: Float, throughput: Float, latency: Float, error_rate: Float) {
    {
      cpu_usage: cpu,
      memory_usage: memory,
      throughput,
      latency,
      error_rate,
      timestamp: Time::now()
    }
  }
  
  // 创建调优策略
  let create_strategies = fn() {
    [
      {
        name: "high_cpu_increase_threads",
        condition: fn(metrics) { metrics.cpu_usage > 80.0 },
        action: fn(config) { { thread_pool_size: config.thread_pool_size + 2, batch_size: config.batch_size, cache_size: config.cache_size, timeout_ms: config.timeout_ms, retry_count: config.retry_count, compression_enabled: config.compression_enabled } },
        priority: 1
      },
      {
        name: "high_memory_reduce_cache",
        condition: fn(metrics) { metrics.memory_usage > 85.0 },
        action: fn(config) { { thread_pool_size: config.thread_pool_size, batch_size: config.batch_size, cache_size: config.cache_size / 2, timeout_ms: config.timeout_ms, retry_count: config.retry_count, compression_enabled: config.compression_enabled } },
        priority: 2
      },
      {
        name: "low_throughput_increase_batch",
        condition: fn(metrics) { metrics.throughput < 100.0 },
        action: fn(config) { { thread_pool_size: config.thread_pool_size, batch_size: config.batch_size + 50, cache_size: config.cache_size, timeout_ms: config.timeout_ms, retry_count: config.retry_count, compression_enabled: config.compression_enabled } },
        priority: 3
      },
      {
        name: "high_latency_increase_timeout",
        condition: fn(metrics) { metrics.latency > 1000.0 },
        action: fn(config) { { thread_pool_size: config.thread_pool_size, batch_size: config.batch_size, cache_size: config.cache_size, timeout_ms: config.timeout_ms + 2000, retry_count: config.retry_count, compression_enabled: config.compression_enabled } },
        priority: 4
      },
      {
        name: "high_error_rate_increase_retry",
        condition: fn(metrics) { metrics.error_rate > 5.0 },
        action: fn(config) { { thread_pool_size: config.thread_pool_size, batch_size: config.batch_size, cache_size: config.cache_size, timeout_ms: config.timeout_ms, retry_count: config.retry_count + 2, compression_enabled: config.compression_enabled } },
        priority: 5
      }
    ]
  }
  
  // 自适应调优器
  type AdaptiveTuner = {
    current_config: Configuration,
    strategies: Array[TuningStrategy],
    metrics_history: Array[PerformanceMetrics],
    tuning_history: Array[(String, Configuration, PerformanceMetrics)]
  }
  
  let create_tuner = fn(initial_config: Configuration, strategies: Array[TuningStrategy]) {
    {
      current_config: initial_config,
      strategies,
      metrics_history: [],
      tuning_history: []
    }
  }
  
  let add_metrics = fn(tuner: AdaptiveTuner, metrics: PerformanceMetrics) {
    tuner.metrics_history = tuner.metrics_history.push(metrics)
    
    // 保持历史记录在合理范围内
    if tuner.metrics_history.length() > 100 {
      tuner.metrics_history = tuner.metrics_history.slice(-100)
    }
  }
  
  let evaluate_and_tune = fn(tuner: AdaptiveTuner) {
    if tuner.metrics_history.length() == 0 {
      return
    }
    
    // 获取最新的性能指标
    let latest_metrics = tuner.metrics_history[tuner.metrics_history.length() - 1]
    
    // 找到匹配的策略
    let applicable_strategies = tuner.strategies.filter(fn(strategy) {
      strategy.condition(latest_metrics)
    })
    
    // 按优先级排序
    let sorted_strategies = applicable_strategies.sort(fn(a, b) {
      if a.priority < b.priority { -1 }
      else if a.priority > b.priority { 1 }
      else { 0 }
    })
    
    // 应用最高优先级的策略
    if sorted_strategies.length() > 0 {
      let strategy = sorted_strategies[0]
      let old_config = tuner.current_config
      tuner.current_config = strategy.action(tuner.current_config)
      
      // 记录调整历史
      tuner.tuning_history = tuner.tuning_history.push((strategy.name, old_config, latest_metrics))
      
      // 保持历史记录在合理范围内
      if tuner.tuning_history.length() > 50 {
        tuner.tuning_history = tuner.tuning_history.slice(-50)
      }
    }
  }
  
  // 测试自适应调优
  let strategies = create_strategies()
  let tuner = create_tuner(default_config, strategies)
  
  // 初始配置
  assert_eq(tuner.current_config.thread_pool_size, 4)
  assert_eq(tuner.current_config.batch_size, 100)
  assert_eq(tuner.current_config.cache_size, 1000)
  
  // 添加高CPU使用率指标
  let high_cpu_metrics = create_metrics(85.0, 50.0, 200.0, 500.0, 1.0)
  add_metrics(tuner, high_cpu_metrics)
  evaluate_and_tune(tuner)
  
  // 验证线程数增加
  assert_eq(tuner.current_config.thread_pool_size, 6)
  assert_eq(tuner.current_config.batch_size, 100)
  assert_eq(tuner.current_config.cache_size, 1000)
  
  // 添加高内存使用率指标
  let high_memory_metrics = create_metrics(60.0, 90.0, 200.0, 500.0, 1.0)
  add_metrics(tuner, high_memory_metrics)
  evaluate_and_tune(tuner)
  
  // 验证缓存大小减少
  assert_eq(tuner.current_config.thread_pool_size, 6)  // 保持不变
  assert_eq(tuner.current_config.batch_size, 100)     // 保持不变
  assert_eq(tuner.current_config.cache_size, 500)     // 减半
  
  // 添加低吞吐量指标
  let low_throughput_metrics = create_metrics(60.0, 70.0, 80.0, 500.0, 1.0)
  add_metrics(tuner, low_throughput_metrics)
  evaluate_and_tune(tuner)
  
  // 验证批处理大小增加
  assert_eq(tuner.current_config.thread_pool_size, 6)
  assert_eq(tuner.current_config.batch_size, 150)    // 增加
  assert_eq(tuner.current_config.cache_size, 500)
  
  // 添加高延迟指标
  let high_latency_metrics = create_metrics(60.0, 70.0, 120.0, 1500.0, 1.0)
  add_metrics(tuner, high_latency_metrics)
  evaluate_and_tune(tuner)
  
  // 验证超时时间增加
  assert_eq(tuner.current_config.thread_pool_size, 6)
  assert_eq(tuner.current_config.batch_size, 150)
  assert_eq(tuner.current_config.cache_size, 500)
  assert_eq(tuner.current_config.timeout_ms, 7000)   // 增加
  
  // 添加高错误率指标
  let high_error_rate_metrics = create_metrics(60.0, 70.0, 120.0, 800.0, 8.0)
  add_metrics(tuner, high_error_rate_metrics)
  evaluate_and_tune(tuner)
  
  // 验证重试次数增加
  assert_eq(tuner.current_config.thread_pool_size, 6)
  assert_eq(tuner.current_config.batch_size, 150)
  assert_eq(tuner.current_config.cache_size, 500)
  assert_eq(tuner.current_config.timeout_ms, 7000)
  assert_eq(tuner.current_config.retry_count, 5)      // 增加
  
  // 验证调整历史
  assert_eq(tuner.tuning_history.length(), 5)
  assert_eq(tuner.tuning_history[0].0, "high_cpu_increase_threads")
  assert_eq(tuner.tuning_history[1].0, "high_memory_reduce_cache")
  assert_eq(tuner.tuning_history[2].0, "low_throughput_increase_batch")
  assert_eq(tuner.tuning_history[3].0, "high_latency_increase_timeout")
  assert_eq(tuner.tuning_history[4].0, "high_error_rate_increase_retry")
  
  // 测试预测性调优
  let predict_performance = fn(tuner: AdaptiveTuner) {
    if tuner.metrics_history.length() < 10 {
      return None
    }
    
    // 简单的线性趋势预测
    let recent_metrics = tuner.metrics_history.slice(-10)
    
    let cpu_trend = calculate_trend(recent_metrics.map(fn(m) { m.cpu_usage }))
    let memory_trend = calculate_trend(recent_metrics.map(fn(m) { m.memory_usage }))
    let throughput_trend = calculate_trend(recent_metrics.map(fn(m) { m.throughput }))
    let latency_trend = calculate_trend(recent_metrics.map(fn(m) { m.latency }))
    
    let latest = tuner.metrics_history[tuner.metrics_history.length() - 1]
    
    // 预测5个时间单位后的性能
    let predicted = create_metrics(
      latest.cpu_usage + (cpu_trend * 5),
      latest.memory_usage + (memory_trend * 5),
      latest.throughput + (throughput_trend * 5),
      latest.latency + (latency_trend * 5),
      latest.error_rate  // 简化：不预测错误率
    )
    
    Some(predicted)
  }
  
  let calculate_trend = fn(values: Array[Float]) {
    if values.length() < 2 {
      return 0.0
    }
    
    let n = values.length() as Float
    let sum_x = (0..(values.length() - 1)).reduce(fn(acc, i) { acc + (i as Float) }, 0.0)
    let sum_y = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let sum_xy = (0..(values.length() - 1)).reduce(fn(acc, i) { acc + ((i as Float) * values[i]) }, 0.0)
    let sum_x2 = (0..(values.length() - 1)).reduce(fn(acc, i) { acc + ((i as Float) * (i as Float)) }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    slope
  }
  
  // 添加更多历史数据
  for i in 0..15 {
    let cpu = 60.0 + (i as Float) * 2.0  // CPU使用率逐渐增加
    let memory = 50.0 + (i as Float) * 1.0
    let throughput = 200.0 - (i as Float) * 5.0  // 吞吐量逐渐减少
    let latency = 500.0 + (i as Float) * 20.0  // 延迟逐渐增加
    let error_rate = 1.0
    
    let metrics = create_metrics(cpu, memory, throughput, latency, error_rate)
    add_metrics(tuner, metrics)
  }
  
  // 预测性能
  match predict_performance(tuner) {
    Some(predicted) => {
      // 验证预测结果
      assert_true(predicted.cpu_usage > 90.0)  // CPU使用率预测会很高
      assert_true(predicted.memory_usage > 65.0)
      assert_true(predicted.throughput < 150.0)  // 吞吐量预测会降低
      assert_true(predicted.latency > 800.0)     // 延迟预测会增加
    }
    None => assert_true(false)  // 应该有预测结果
  }
  
  // 基于预测进行预防性调优
  match predict_performance(tuner) {
    Some(predicted) => {
      // 模拟基于预测的调优
      if predicted.cpu_usage > 90.0 {
        tuner.current_config.thread_pool_size = tuner.current_config.thread_pool_size + 4
      }
      
      if predicted.memory_usage > 80.0 {
        tuner.current_config.cache_size = tuner.current_config.cache_size / 2
      }
      
      if predicted.throughput < 150.0 {
        tuner.current_config.batch_size = tuner.current_config.batch_size + 100
      }
      
      if predicted.latency > 1000.0 {
        tuner.current_config.timeout_ms = tuner.current_config.timeout_ms + 3000
      }
    }
    None => ()
  }
  
  // 验证预防性调优结果
  assert_eq(tuner.current_config.thread_pool_size, 10)  // 6 + 4
  assert_eq(tuner.current_config.batch_size, 250)       // 150 + 100
  assert_eq(tuner.current_config.cache_size, 250)       // 500 / 2
  assert_eq(tuner.current_config.timeout_ms, 10000)     // 7000 + 3000
}

// 测试8: 容错和弹性机制
test "容错和弹性机制测试" {
  // 定义断路器状态
  enum CircuitBreakerState {
    Closed
    Open
    HalfOpen
  }
  
  // 定义断路器
  type CircuitBreaker = {
    state: CircuitBreakerState,
    failure_count: Int,
    success_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    timeout_ms: Int,
    last_failure_time: Int,
    request_count: Int
  }
  
  // 创建断路器
  let create_circuit_breaker = fn(failure_threshold: Int, success_threshold: Int, timeout_ms: Int) {
    {
      state: CircuitBreakerState::Closed,
      failure_count: 0,
      success_count: 0,
      failure_threshold,
      success_threshold,
      timeout_ms,
      last_failure_time: 0,
      request_count: 0
    }
  }
  
  // 通过断路器执行操作
  let execute_with_circuit_breaker = fn(breaker: CircuitBreaker, operation: () -> Result[String, String]) {
    breaker.request_count = breaker.request_count + 1
    
    match breaker.state {
      CircuitBreakerState::Open => {
        // 检查是否应该转为半开状态
        let current_time = Time::now()
        if current_time - breaker.last_failure_time > breaker.timeout_ms {
          breaker.state = CircuitBreakerState::HalfOpen
          breaker.success_count = 0
        } else {
          return Err("Circuit breaker is open")
        }
      }
      CircuitBreakerState::HalfOpen => {
        // 在半开状态下，允许少量请求通过
      }
      CircuitBreakerState::Closed => {
        // 正常状态，允许所有请求
      }
    }
    
    // 执行操作
    let result = operation()
    
    match result {
      Ok(value) => {
        match breaker.state {
          CircuitBreakerState::HalfOpen => {
            breaker.success_count = breaker.success_count + 1
            if breaker.success_count >= breaker.success_threshold {
              breaker.state = CircuitBreakerState::Closed
              breaker.failure_count = 0
            }
          }
          CircuitBreakerState::Closed => {
            // 在关闭状态下成功，可以重置部分失败计数
            if breaker.failure_count > 0 {
              breaker.failure_count = breaker.failure_count - 1
            }
          }
          CircuitBreakerState::Open => {
            // 不应该到达这里
          }
        }
        result
      }
      Err(error) => {
        match breaker.state {
          CircuitBreakerState::HalfOpen => {
            // 在半开状态下失败，立即打开断路器
            breaker.state = CircuitBreakerState::Open
            breaker.last_failure_time = Time::now()
          }
          CircuitBreakerState::Closed => {
            breaker.failure_count = breaker.failure_count + 1
            if breaker.failure_count >= breaker.failure_threshold {
              breaker.state = CircuitBreakerState::Open
              breaker.last_failure_time = Time::now()
            }
          }
          CircuitBreakerState::Open => {
            // 不应该到达这里
          }
        }
        result
      }
    }
  }
  
  // 测试断路器
  let breaker = create_circuit_breaker(3, 2, 5000)
  
  // 初始状态应该是关闭的
  match breaker.state {
    CircuitBreakerState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  
  // 模拟成功操作
  let success_op = fn() { Ok("Success") }
  let result1 = execute_with_circuit_breaker(breaker, success_op)
  assert_eq(result1, Ok("Success"))
  
  // 模拟失败操作
  let failure_op = fn() { Err("Service unavailable") }
  
  // 连续失败直到断路器打开
  let result2 = execute_with_circuit_breaker(breaker, failure_op)
  let result3 = execute_with_circuit_breaker(breaker, failure_op)
  let result4 = execute_with_circuit_breaker(breaker, failure_op)
  
  assert_eq(result2, Err("Service unavailable"))
  assert_eq(result3, Err("Service unavailable"))
  assert_eq(result4, Err("Service unavailable"))
  
  // 断路器应该打开
  match breaker.state {
    CircuitBreakerState::Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // 断路器打开时，请求应该被拒绝
  let result5 = execute_with_circuit_breaker(breaker, success_op)
  assert_eq(result5, Err("Circuit breaker is open"))
  
  // 测试重试机制
  type RetryPolicy = {
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float,
    retryable_errors: Array[String]
  }
  
  let execute_with_retry = fn(policy: RetryPolicy, operation: () -> Result[String, String]) {
    let mut attempt = 0
    let mut delay = policy.base_delay_ms
    
    while attempt < policy.max_attempts {
      attempt = attempt + 1
      let result = operation()
      
      match result {
        Ok(value) => return Ok(value),
        Err(error) => {
          if attempt >= policy.max_attempts {
            return Err(error)
          }
          
          // 检查是否是可重试的错误
          let is_retryable = policy.retryable_errors.any(fn(retryable_error) {
            error.contains(retryable_error)
          })
          
          if not is_retryable {
            return Err(error)
          }
          
          // 等待重试延迟
          Thread::sleep(delay)
          delay = ((delay as Float) * policy.backoff_multiplier) as Int
          if delay > policy.max_delay_ms {
            delay = policy.max_delay_ms
          }
        }
      }
    }
    
    Err("Max retry attempts exceeded")
  }
  
  // 测试重试机制
  let retry_policy = {
    max_attempts: 3,
    base_delay_ms: 100,
    max_delay_ms: 1000,
    backoff_multiplier: 2.0,
    retryable_errors: ["timeout", "network", "temporary"]
  }
  
  let mut attempt_count = 0
  let retry_op = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err("temporary failure")
    } else {
      Ok("Success after retries")
    }
  }
  
  let retry_result = execute_with_retry(retry_policy, retry_op)
  assert_eq(retry_result, Ok("Success after retries"))
  assert_eq(attempt_count, 3)
  
  // 测试不可重试的错误
  let mut non_retryable_count = 0
  let non_retryable_op = fn() {
    non_retryable_count = non_retryable_count + 1
    Err("permanent failure")
  }
  
  let non_retryable_result = execute_with_retry(retry_policy, non_retryable_op)
  assert_eq(non_retryable_result, Err("permanent failure"))
  assert_eq(non_retryable_count, 1)
  
  // 测试超时机制
  type TimeoutConfig = {
    timeout_ms: Int,
    on_timeout: () -> Unit
  }
  
  let execute_with_timeout = fn(config: TimeoutConfig, operation: () -> String) {
    let start_time = Time::now()
    let result = operation()
    let end_time = Time::now()
    
    if end_time - start_time > config.timeout_ms {
      config.on_timeout()
      "Operation timed out"
    } else {
      result
    }
  }
  
  // 测试超时
  let timeout_config = {
    timeout_ms: 1000,
    on_timeout: fn() { println("Operation timed out") }
  }
  
  let fast_op = fn() { "Quick result" }
  let fast_result = execute_with_timeout(timeout_config, fast_op)
  assert_eq(fast_result, "Quick result")
  
  let slow_op = fn() {
    Thread::sleep(2000)  // 睡眠2秒
    "Slow result"
  }
  let slow_result = execute_with_timeout(timeout_config, slow_op)
  assert_eq(slow_result, "Operation timed out")
  
  // 测试舱壁隔离模式
  type Bulkhead = {
    name: String,
    max_concurrent: Int,
    max_queue: Int,
    active_count: Int,
    queue: Array<() -> String>
  }
  
  let create_bulkhead = fn(name: String, max_concurrent: Int, max_queue: Int) {
    {
      name,
      max_concurrent,
      max_queue,
      active_count: 0,
      queue: []
    }
  }
  
  let execute_with_bulkhead = fn(bulkhead: Bulkhead, operation: () -> String) {
    if bulkhead.active_count >= bulkhead.max_concurrent {
      if bulkhead.queue.length() >= bulkhead.max_queue {
        return "Bulkhead rejected - queue full"
      } else {
        bulkhead.queue = bulkhead.queue.push(operation)
        return "Operation queued"
      }
    }
    
    bulkhead.active_count = bulkhead.active_count + 1
    
    // 执行操作
    let result = operation()
    
    bulkhead.active_count = bulkhead.active_count - 1
    
    // 处理队列中的下一个操作
    if bulkhead.queue.length() > 0 && bulkhead.active_count < bulkhead.max_concurrent {
      let next_op = bulkhead.queue[0]
      bulkhead.queue = bulkhead.queue.slice(1)
      // 在实际实现中，这里会异步执行下一个操作
      // 为了测试，我们只是模拟减少活跃计数
    }
    
    result
  }
  
  // 测试舱壁隔离
  let bulkhead = create_bulkhead("test_bulkhead", 2, 3)
  
  assert_eq(bulkhead.active_count, 0)
  assert_eq(bulkhead.queue.length(), 0)
  
  let op1 = fn() { "Operation 1" }
  let result1 = execute_with_bulkhead(bulkhead, op1)
  assert_eq(result1, "Operation 1")
  assert_eq(bulkhead.active_count, 0)  // 操作完成后减少
  
  let op2 = fn() { "Operation 2" }
  let result2 = execute_with_bulkhead(bulkhead, op2)
  assert_eq(result2, "Operation 2")
  
  // 模拟并发操作（简化测试）
  bulkhead.active_count = 2  // 模拟两个活跃操作
  
  let op3 = fn() { "Operation 3" }
  let result3 = execute_with_bulkhead(bulkhead, op3)
  assert_eq(result3, "Operation queued")
  assert_eq(bulkhead.queue.length(), 1)
  
  let op4 = fn() { "Operation 4" }
  let result4 = execute_with_bulkhead(bulkhead, op4)
  assert_eq(result4, "Operation queued")
  assert_eq(bulkhead.queue.length(), 2)
  
  let op5 = fn() { "Operation 5" }
  let result5 = execute_with_bulkhead(bulkhead, op5)
  assert_eq(result5, "Operation queued")
  assert_eq(bulkhead.queue.length(), 3)
  
  let op6 = fn() { "Operation 6" }
  let result6 = execute_with_bulkhead(bulkhead, op6)
  assert_eq(result6, "Bulkhead rejected - queue full")
  assert_eq(bulkhead.queue.length(), 3)  // 队列已满
}

// 测试9: 可扩展性测试
test "系统可扩展性和负载均衡测试" {
  // 定义负载均衡策略
  enum LoadBalancingStrategy {
    RoundRobin
    LeastConnections
    WeightedRoundRobin
    Random
    IPHash
  }
  
  // 定义服务器节点
  type ServerNode = {
    id: String,
    address: String,
    weight: Int,
    active_connections: Int,
    is_healthy: Bool,
    total_requests: Int
  }
  
  // 定义负载均衡器
  type LoadBalancer = {
    strategy: LoadBalancingStrategy,
    nodes: Array[ServerNode],
    current_index: Int,
    health_check_interval_ms: Int
  }
  
  // 创建负载均衡器
  let create_load_balancer = fn(strategy: LoadBalancingStrategy, nodes: Array[ServerNode]) {
    {
      strategy,
      nodes,
      current_index: 0,
      health_check_interval_ms: 30000
    }
  }
  
  // 创建服务器节点
  let create_node = fn(id: String, address: String, weight: Int) {
    {
      id,
      address,
      weight,
      active_connections: 0,
      is_healthy: true,
      total_requests: 0
    }
  }
  
  // 选择节点
  let select_node = fn(balancer: LoadBalancer, client_ip: String) {
    let healthy_nodes = balancer.nodes.filter(fn(node) { node.is_healthy })
    
    if healthy_nodes.length() == 0 {
      return None
    }
    
    match balancer.strategy {
      LoadBalancingStrategy::RoundRobin => {
        let node = healthy_nodes[balancer.current_index % healthy_nodes.length()]
        balancer.current_index = balancer.current_index + 1
        Some(node)
      }
      LoadBalancingStrategy::LeastConnections => {
        let mut selected = healthy_nodes[0]
        for node in healthy_nodes {
          if node.active_connections < selected.active_connections {
            selected = node
          }
        }
        Some(selected)
      }
      LoadBalancingStrategy::WeightedRoundRobin => {
        let total_weight = healthy_nodes.reduce(fn(acc, node) { acc + node.weight }, 0)
        let mut weighted_index = balancer.current_index % total_weight
        
        let mut selected = healthy_nodes[0]
        for node in healthy_nodes {
          if weighted_index < node.weight {
            selected = node
            break
          }
          weighted_index = weighted_index - node.weight
        }
        
        balancer.current_index = balancer.current_index + 1
        Some(selected)
      }
      LoadBalancingStrategy::Random => {
        let random_index = Random::range(0, healthy_nodes.length())
        Some(healthy_nodes[random_index])
      }
      LoadBalancingStrategy::IPHash => {
        let hash = simple_hash(client_ip)
        let index = hash.abs() % healthy_nodes.length()
        Some(healthy_nodes[index])
      }
    }
  }
  
  let simple_hash = fn(input: String) {
    let mut hash = 5381
    for c in input.chars() {
      hash = ((hash << 5) + hash) + c.to_int()
    }
    hash
  }
  
  // 测试负载均衡器
  let nodes = [
    create_node("node1", "192.168.1.10", 1),
    create_node("node2", "192.168.1.11", 2),
    create_node("node3", "192.168.1.12", 1)
  ]
  
  let balancer = create_load_balancer(LoadBalancingStrategy::RoundRobin, nodes)
  
  // 测试轮询策略
  let selected1 = select_node(balancer, "192.168.1.100")
  let selected2 = select_node(balancer, "192.168.1.100")
  let selected3 = select_node(balancer, "192.168.1.100")
  let selected4 = select_node(balancer, "192.168.1.100")
  
  assert_eq(selected1.unwrap().id, "node1")
  assert_eq(selected2.unwrap().id, "node2")
  assert_eq(selected3.unwrap().id, "node3")
  assert_eq(selected4.unwrap().id, "node1")  // 回到第一个节点
  
  // 测试最少连接策略
  let least_conn_balancer = create_load_balancer(LoadBalancingStrategy::LeastConnections, nodes)
  
  // 设置不同的连接数
  nodes[0].active_connections = 5
  nodes[1].active_connections = 2
  nodes[2].active_connections = 10
  
  let least_conn_selected = select_node(least_conn_balancer, "192.168.1.100")
  assert_eq(least_conn_selected.unwrap().id, "node2")  // 连接数最少
  
  // 测试加权轮询策略
  let weighted_balancer = create_load_balancer(LoadBalancingStrategy::WeightedRoundRobin, nodes)
  
  let weighted_selected1 = select_node(weighted_balancer, "192.168.1.100")
  let weighted_selected2 = select_node(weighted_balancer, "192.168.1.100")
  let weighted_selected3 = select_node(weighted_balancer, "192.168.1.100")
  let weighted_selected4 = select_node(weighted_balancer, "192.168.1.100")
  
  // node2权重为2，应该被选中两次
  assert_eq(weighted_selected1.unwrap().id, "node1")
  assert_eq(weighted_selected2.unwrap().id, "node2")
  assert_eq(weighted_selected3.unwrap().id, "node2")
  assert_eq(weighted_selected4.unwrap().id, "node3")
  
  // 测试IP哈希策略
  let ip_hash_balancer = create_load_balancer(LoadBalancingStrategy::IPHash, nodes)
  
  let ip_hash_selected1 = select_node(ip_hash_balancer, "192.168.1.100")
  let ip_hash_selected2 = select_node(ip_hash_balancer, "192.168.1.100")
  let ip_hash_selected3 = select_node(ip_hash_balancer, "192.168.1.200")
  
  // 相同IP应该总是选择相同节点
  assert_eq(ip_hash_selected1.unwrap().id, ip_hash_selected2.unwrap().id)
  // 不同IP可能选择不同节点
  // 注意：这取决于哈希函数的具体实现
  
  // 测试水平扩展
  type ClusterManager = {
    nodes: Array[ServerNode],
    auto_scaling_enabled: Bool,
    min_nodes: Int,
    max_nodes: Int,
    scale_up_threshold: Float,
    scale_down_threshold: Float,
    scale_up_cooldown_ms: Int,
    scale_down_cooldown_ms: Int,
    last_scale_time: Int
  }
  
  let create_cluster_manager = fn(initial_nodes: Array[ServerNode], min_nodes: Int, max_nodes: Int) {
    {
      nodes: initial_nodes,
      auto_scaling_enabled: true,
      min_nodes,
      max_nodes,
      scale_up_threshold: 80.0,  // CPU使用率超过80%时扩展
      scale_down_threshold: 20.0,  // CPU使用率低于20%时收缩
      scale_up_cooldown_ms: 300000,  // 5分钟冷却时间
      scale_down_cooldown_ms: 600000,  // 10分钟冷却时间
      last_scale_time: 0
    }
  }
  
  let check_and_scale = fn(manager: ClusterManager, avg_cpu_usage: Float) {
    let current_time = Time::now()
    
    // 检查冷却时间
    if current_time - manager.last_scale_time < manager.scale_up_cooldown_ms {
      return
    }
    
    if avg_cpu_usage > manager.scale_up_threshold && manager.nodes.length() < manager.max_nodes {
      // 扩展节点
      let new_node_id = "node" + (manager.nodes.length() + 1).to_string()
      let new_node = create_node(new_node_id, "192.168.1." + (100 + manager.nodes.length()).to_string(), 1)
      manager.nodes = manager.nodes.push(new_node)
      manager.last_scale_time = current_time
    } else if avg_cpu_usage < manager.scale_down_threshold && manager.nodes.length() > manager.min_nodes {
      // 收缩节点
      if current_time - manager.last_scale_time >= manager.scale_down_cooldown_ms {
        manager.nodes = manager.nodes.slice(0, -1)
        manager.last_scale_time = current_time
      }
    }
  }
  
  // 测试自动扩展
  let initial_nodes = [create_node("node1", "192.168.1.10", 1), create_node("node2", "192.168.1.11", 1)]
  let cluster = create_cluster_manager(initial_nodes, 2, 5)
  
  assert_eq(cluster.nodes.length(), 2)
  
  // 高负载，应该扩展
  check_and_scale(cluster, 85.0)
  assert_eq(cluster.nodes.length(), 3)
  assert_eq(cluster.nodes[2].id, "node3")
  
  // 继续高负载
  check_and_scale(cluster, 90.0)
  assert_eq(cluster.nodes.length(), 4)
  assert_eq(cluster.nodes[3].id, "node4")
  
  // 达到最大节点数
  check_and_scale(cluster, 95.0)
  assert_eq(cluster.nodes.length(), 5)
  assert_eq(cluster.nodes[4].id, "node5")
  
  // 超过最大节点数，不应再扩展
  check_and_scale(cluster, 98.0)
  assert_eq(cluster.nodes.length(), 5)
  
  // 模拟冷却时间过去
  cluster.last_scale_time = Time::now() - 700000  // 超过scale_down_cooldown_ms
  
  // 低负载，应该收缩
  check_and_scale(cluster, 15.0)
  assert_eq(cluster.nodes.length(), 4)
  
  // 测试分片策略
  type Shard = {
    id: String,
    range: (Int, Int),
    node: ServerNode
  }
  
  type ShardingStrategy = {
    shards: Array[Shard],
    hash_function: (String) -> Int
  }
  
  let create_sharding_strategy = fn(nodes: Array[ServerNode], shard_count: Int) {
    let shards = []
    let shard_size = 1000000 / shard_count  // 简化：假设哈希空间为0-999999
    
    for i in 0..shard_count {
      let start = i * shard_size
      let end = if i == shard_count - 1 { 999999 } else { (i + 1) * shard_size - 1 }
      let node = nodes[i % nodes.length()]
      
      let shard = {
        id: "shard" + i.to_string(),
        range: (start, end),
        node
      }
      shards = shards.push(shard)
    }
    
    {
      shards,
      hash_function: simple_hash
    }
  }
  
  let get_shard_for_key = fn(strategy: ShardingStrategy, key: String) {
    let hash = strategy.hash_function(key).abs() % 1000000
    
    for shard in strategy.shards {
      let (start, end) = shard.range
      if hash >= start && hash <= end {
        return Some(shard)
      }
    }
    
    None
  }
  
  // 测试分片
  let shard_nodes = [
    create_node("shard_node1", "192.168.1.10", 1),
    create_node("shard_node2", "192.168.1.11", 1)
  ]
  
  let sharding_strategy = create_sharding_strategy(shard_nodes, 4)
  
  assert_eq(sharding_strategy.shards.length(), 4)
  
  // 测试键的分片分配
  let shard1 = get_shard_for_key(sharding_strategy, "user123")
  let shard2 = get_shard_for_key(sharding_strategy, "user456")
  let shard3 = get_shard_for_key(sharding_strategy, "user789")
  let shard4 = get_shard_for_key(sharding_strategy, "user123")  // 相同键应该分配到相同分片
  
  assert_true(shard1.is_some())
  assert_true(shard2.is_some())
  assert_true(shard3.is_some())
  assert_true(shard4.is_some())
  
  assert_eq(shard1.unwrap().id, shard4.unwrap().id)  // 相同键应该分配到相同分片
  
  // 测试缓存一致性
  type CacheNode = {
    id: String,
    data: Map[String, String],
    version: Int
  }
  
  type ConsistentCache = {
    nodes: Array[CacheNode],
    replication_factor: Int,
    hash_ring: Array[(Int, String)]
  }
  
  let create_consistent_cache = fn(nodes: Array[CacheNode], replication_factor: Int) {
    // 创建哈希环
    let hash_ring = []
    for node in nodes {
      // 每个节点在环上有多个虚拟节点
      for i in 0..100 {
        let virtual_node_id = node.id + "#" + i.to_string()
        let hash = simple_hash(virtual_node_id)
        hash_ring = hash_ring.push((hash, node.id))
      }
    }
    
    // 按哈希值排序
    let sorted_ring = hash_ring.sort(fn(a, b) {
      if a.0 < b.0 { -1 } else if a.0 > b.0 { 1 } else { 0 }
    })
    
    {
      nodes,
      replication_factor,
      hash_ring: sorted_ring
    }
  }
  
  let get_nodes_for_key = fn(cache: ConsistentCache, key: String) {
    let key_hash = simple_hash(key)
    
    // 在环上找到第一个大于等于key_hash的节点
    let mut start_index = 0
    for i in 0..cache.hash_ring.length() {
      if cache.hash_ring[i].0 >= key_hash {
        start_index = i
        break
      }
    }
    
    // 获取replication_factor个不同的节点
    let selected_nodes = []
    let mut current_index = start_index
    let mut seen_nodes = []
    
    while selected_nodes.length() < cache.replication_factor && seen_nodes.length() < cache.nodes.length() {
      let node_id = cache.hash_ring[current_index].1
      
      if not seen_nodes.contains(node_id) {
        seen_nodes = seen_nodes.push(node_id)
        
        match cache.nodes.find(fn(node) { node.id == node_id }) {
          Some(node) => selected_nodes = selected_nodes.push(node)
          None => ()
        }
      }
      
      current_index = (current_index + 1) % cache.hash_ring.length()
    }
    
    selected_nodes
  }
  
  // 测试一致性哈希缓存
  let cache_nodes = [
    { id: "cache1", data: Map::empty(), version: 1 },
    { id: "cache2", data: Map::empty(), version: 1 },
    { id: "cache3", data: Map::empty(), version: 1 }
  ]
  
  let cache = create_consistent_cache(cache_nodes, 2)
  
  // 测试键的节点分配
  let nodes_for_key1 = get_nodes_for_key(cache, "user123")
  let nodes_for_key2 = get_nodes_for_key(cache, "user456")
  
  assert_eq(nodes_for_key1.length(), 2)
  assert_eq(nodes_for_key2.length(), 2)
  
  // 添加节点后，大部分键应该仍然映射到相同的节点
  let new_cache_nodes = cache_nodes + [{ id: "cache4", data: Map::empty(), version: 1 }]
  let new_cache = create_consistent_cache(new_cache_nodes, 2)
  
  let new_nodes_for_key1 = get_nodes_for_key(new_cache, "user123")
  let new_nodes_for_key2 = get_nodes_for_key(new_cache, "user456")
  
  // 至少应该有一个节点保持不变
  let common_nodes1 = nodes_for_key1.filter(fn(node) { 
    new_nodes_for_key1.any(fn(new_node) { new_node.id == node.id })
  })
  let common_nodes2 = nodes_for_key2.filter(fn(node) { 
    new_nodes_for_key2.any(fn(new_node) { new_node.id == node.id })
  })
  
  assert_true(common_nodes1.length() >= 1)
  assert_true(common_nodes2.length() >= 1)
}

// 测试10: 跨平台兼容性测试
test "跨平台兼容性和平台特定功能测试" {
  // 定义平台类型
  enum Platform {
    Windows
    Linux
    MacOS
    Unknown
  }
  
  // 定义平台特定功能
  type PlatformFeatures = {
    file_separator: String,
    line_separator: String,
    path_separator: String,
    executable_extension: String,
    case_sensitive: Bool,
    max_path_length: Int,
    supports_symlinks: Bool
  }
  
  // 获取当前平台
  let get_current_platform = fn() {
    let os_name = System::os_name()
    
    if os_name.contains("Windows") {
      Platform::Windows
    } else if os_name.contains("Linux") {
      Platform::Linux
    } else if os_name.contains("Mac") {
      Platform::MacOS
    } else {
      Platform::Unknown
    }
  }
  
  // 获取平台特定功能
  let get_platform_features = fn(platform: Platform) {
    match platform {
      Platform::Windows => {
        {
          file_separator: "\\",
          line_separator: "\r\n",
          path_separator: ";",
          executable_extension: ".exe",
          case_sensitive: false,
          max_path_length: 260,
          supports_symlinks: true
        }
      }
      Platform::Linux => {
        {
          file_separator: "/",
          line_separator: "\n",
          path_separator: ":",
          executable_extension: "",
          case_sensitive: true,
          max_path_length: 4096,
          supports_symlinks: true
        }
      }
      Platform::MacOS => {
        {
          file_separator: "/",
          line_separator: "\n",
          path_separator: ":",
          executable_extension: "",
          case_sensitive: false,  // macOS文件系统默认不区分大小写
          max_path_length: 1024,
          supports_symlinks: true
        }
      }
      Platform::Unknown => {
        {
          file_separator: "/",
          line_separator: "\n",
          path_separator: ":",
          executable_extension: "",
          case_sensitive: true,
          max_path_length: 255,
          supports_symlinks: false
        }
      }
    }
  }
  
  // 测试平台检测
  let current_platform = get_current_platform()
  let platform_features = get_platform_features(current_platform)
  
  // 根据平台验证功能
  match current_platform {
    Platform::Windows => {
      assert_eq(platform_features.file_separator, "\\")
      assert_eq(platform_features.line_separator, "\r\n")
      assert_eq(platform_features.path_separator, ";")
      assert_eq(platform_features.executable_extension, ".exe")
      assert_false(platform_features.case_sensitive)
      assert_eq(platform_features.max_path_length, 260)
    }
    Platform::Linux => {
      assert_eq(platform_features.file_separator, "/")
      assert_eq(platform_features.line_separator, "\n")
      assert_eq(platform_features.path_separator, ":")
      assert_eq(platform_features.executable_extension, "")
      assert_true(platform_features.case_sensitive)
      assert_eq(platform_features.max_path_length, 4096)
    }
    Platform::MacOS => {
      assert_eq(platform_features.file_separator, "/")
      assert_eq(platform_features.line_separator, "\n")
      assert_eq(platform_features.path_separator, ":")
      assert_eq(platform_features.executable_extension, "")
      assert_false(platform_features.case_sensitive)
      assert_eq(platform_features.max_path_length, 1024)
    }
    Platform::Unknown => {
      // 使用默认值
      assert_eq(platform_features.file_separator, "/")
      assert_eq(platform_features.line_separator, "\n")
    }
  }
  
  // 测试路径处理
  let normalize_path = fn(path: String, features: PlatformFeatures) {
    let separator = features.file_separator
    
    // 替换所有路径分隔符
    let normalized = if separator == "/" {
      path.replace("\\", "/")
    } else {
      path.replace("/", "\\")
    }
    
    // 移除重复的分隔符
    let mut result = ""
    let mut i = 0
    
    while i < normalized.length() {
      let char = normalized[i]
      
      if char == separator_char(separator) {
        result = result + char
        
        // 跳过连续的分隔符
        while i + 1 < normalized.length() && normalized[i + 1] == char {
          i = i + 1
        }
      } else {
        result = result + char
      }
      
      i = i + 1
    }
    
    result
  }
  
  let separator_char = fn(separator: String) {
    if separator == "/" {
      '/'
    } else {
      '\\'
    }
  }
  
  // 测试路径标准化
  let test_path = "folder\\subfolder//file.txt"
  let normalized_path = normalize_path(test_path, platform_features)
  
  if platform_features.file_separator == "/" {
    assert_eq(normalized_path, "folder/subfolder/file.txt")
  } else {
    assert_eq(normalized_path, "folder\\subfolder\\file.txt")
  }
  
  // 测试环境变量处理
  let expand_environment_variables = fn(input: String, platform: Platform) {
    let mut result = input
    let env_var_pattern = if platform == Platform::Windows {
      "%([^%]+)%"
    } else {
      "\\$([A-Za-z_][A-Za-z0-9_]*)"
    }
    
    // 简化：只处理几个常见环境变量
    if platform == Platform::Windows {
      result = result.replace("%HOME%", System::env("HOME"))
      result = result.replace("%PATH%", System::env("PATH"))
      result = result.replace("%USER%", System::env("USER"))
    } else {
      result = result.replace("$HOME", System::env("HOME"))
      result = result.replace("$PATH", System::env("PATH"))
      result = result.replace("$USER", System::env("USER"))
    }
    
    result
  }
  
  // 测试环境变量扩展
  let test_env_path = if current_platform == Platform::Windows {
    "%HOME%\\Documents"
  } else {
    "$HOME/Documents"
  }
  
  let expanded_path = expand_environment_variables(test_env_path, current_platform)
  assert_true(expanded_path.contains(System::env("HOME")))
  
  // 测试命令行参数处理
  type CommandLineArgs = {
    program: String,
    arguments: Array[String],
    options: Map[String, String]
  }
  
  let parse_command_line = fn(args: Array[String], platform: Platform) {
    if args.length() == 0 {
      return {
        program: "",
        arguments: [],
        options: Map::empty()
      }
    }
    
    let program = args[0]
    let mut arguments = []
    let mut options = Map::empty()
    
    let mut i = 1
    while i < args.length() {
      let arg = args[i]
      
      if arg.starts_with("--") {
        // 长选项格式：--option=value 或 --option value
        let option = arg.slice(2)
        if option.contains("=") {
          let parts = option.split("=")
          let key = parts[0]
          let value = parts[1]
          let _ = Map::insert(options, key, value)
        } else {
          // 下一个参数是值
          if i + 1 < args.length() && not args[i + 1].starts_with("-") {
            let key = option
            let value = args[i + 1]
            let _ = Map::insert(options, key, value)
            i = i + 1
          } else {
            // 布尔选项
            let _ = Map::insert(options, option, "true")
          }
        }
      } else if arg.starts_with("-") && platform != Platform::Windows {
        // Unix风格短选项：-a value 或 -abc
        let option = arg.slice(1)
        if option.length() == 1 {
          // 单字符选项
          if i + 1 < args.length() && not args[i + 1].starts_with("-") {
            let key = option
            let value = args[i + 1]
            let _ = Map::insert(options, key, value)
            i = i + 1
          } else {
            // 布尔选项
            let _ = Map::insert(options, option, "true")
          }
        } else {
          // 多个布尔选项
          for c in option.chars() {
            let _ = Map::insert(options, c.to_string(), "true")
          }
        }
      } else {
        // 普通参数
        arguments = arguments.push(arg)
      }
      
      i = i + 1
    }
    
    {
      program,
      arguments,
      options
    }
  }
  
  // 测试命令行解析
  let test_args = ["program", "--output=file.txt", "--verbose", "-a", "value", "-xyz", "arg1", "arg2"]
  let parsed = parse_command_line(test_args, Platform::Linux)
  
  assert_eq(parsed.program, "program")
  assert_eq(parsed.arguments, ["arg1", "arg2"])
  assert_eq(Map::get(parsed.options, "output"), Some("file.txt"))
  assert_eq(Map::get(parsed.options, "verbose"), Some("true"))
  assert_eq(Map::get(parsed.options, "a"), Some("value"))
  assert_eq(Map::get(parsed.options, "x"), Some("true"))
  assert_eq(Map::get(parsed.options, "y"), Some("true"))
  assert_eq(Map::get(parsed.options, "z"), Some("true"))
  
  // 测试Windows风格命令行
  let windows_args = ["program.exe", "/output", "file.txt", "/verbose", "arg1"]
  let windows_parsed = parse_command_line(windows_args, Platform::Windows)
  
  assert_eq(windows_parsed.program, "program.exe")
  assert_eq(windows_parsed.arguments, ["/output", "file.txt", "/verbose", "arg1"])
  assert_eq(windows_parsed.options.length(), 0)  // Windows风格选项不被特殊处理
  
  // 测试平台特定的文件权限
  type FilePermissions = {
    read: Bool,
    write: Bool,
    execute: Bool,
    owner_read: Bool,
    owner_write: Bool,
    owner_execute: Bool,
    group_read: Bool,
    group_write: Bool,
    group_execute: Bool,
    other_read: Bool,
    other_write: Bool,
    other_execute: Bool
  }
  
  let get_default_permissions = fn(platform: Platform) {
    match platform {
      Platform::Windows => {
        // Windows使用ACL，这里简化处理
        {
          read: true,
          write: true,
          execute: false,
          owner_read: true,
          owner_write: true,
          owner_execute: false,
          group_read: true,
          group_write: false,
          group_execute: false,
          other_read: true,
          other_write: false,
          other_execute: false
        }
      }
      Platform::Linux | Platform::MacOS => {
        // Unix风格权限
        {
          read: true,
          write: true,
          execute: false,
          owner_read: true,
          owner_write: true,
          owner_execute: false,
          group_read: true,
          group_write: false,
          group_execute: false,
          other_read: true,
          other_write: false,
          other_execute: false
        }
      }
      Platform::Unknown => {
        // 默认权限
        {
          read: true,
          write: false,
          execute: false,
          owner_read: true,
          owner_write: true,
          owner_execute: false,
          group_read: true,
          group_write: false,
          group_execute: false,
          other_read: false,
          other_write: false,
          other_execute: false
        }
      }
    }
  }
  
  // 测试文件权限
  let default_permissions = get_default_permissions(current_platform)
  
  match current_platform {
    Platform::Windows => {
      assert_true(default_permissions.read)
      assert_true(default_permissions.write)
      assert_false(default_permissions.execute)
      assert_true(default_permissions.owner_read)
      assert_true(default_permissions.owner_write)
      assert_false(default_permissions.owner_execute)
    }
    Platform::Linux | Platform::MacOS => {
      assert_true(default_permissions.read)
      assert_true(default_permissions.write)
      assert_false(default_permissions.execute)
      assert_true(default_permissions.owner_read)
      assert_true(default_permissions.owner_write)
      assert_false(default_permissions.owner_execute)
      assert_true(default_permissions.group_read)
      assert_false(default_permissions.group_write)
      assert_false(default_permissions.group_execute)
      assert_true(default_permissions.other_read)
      assert_false(default_permissions.other_write)
      assert_false(default_permissions.other_execute)
    }
    Platform::Unknown => {
      assert_true(default_permissions.read)
      assert_false(default_permissions.write)
      assert_false(default_permissions.execute)
    }
  }
  
  // 测试平台特定的进程管理
  type ProcessInfo = {
    pid: Int,
    name: String,
    command_line: String,
    start_time: Int,
    parent_pid: Int
  }
  
  let get_current_process_info = fn(platform: Platform) {
    let pid = System::current_pid()
    let command_line = System::command_line()
    
    // 根据平台获取进程信息
    match platform {
      Platform::Windows => {
        {
          pid,
          name: command_line.split(" ")[0],
          command_line,
          start_time: System::process_start_time(pid),
          parent_pid: System::parent_pid(pid)
        }
      }
      Platform::Linux | Platform::MacOS => {
        {
          pid,
          name: command_line.split(" ")[0],
          command_line,
          start_time: System::process_start_time(pid),
          parent_pid: System::parent_pid(pid)
        }
      }
      Platform::Unknown => {
        {
          pid,
          name: "unknown",
          command_line,
          start_time: Time::now(),
          parent_pid: 0
        }
      }
    }
  }
  
  // 测试进程信息
  let process_info = get_current_process_info(current_platform)
  
  assert_true(process_info.pid > 0)
  assert_true(process_info.name.length() > 0)
  assert_true(process_info.command_line.length() > 0)
  assert_true(process_info.start_time > 0)
  
  // 父进程ID可能为0（对于init进程）或大于0
  assert_true(process_info.parent_pid >= 0)
}
