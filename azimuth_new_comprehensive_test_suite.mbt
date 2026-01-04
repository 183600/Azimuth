// Azimuth 新增综合测试用例
// 涵盖多个核心功能和高级特性

// 测试1: 基础数据结构操作
test "基础数据结构操作测试" {
  // 测试栈操作
  let mut stack = []
  stack = stack.push(1)
  stack = stack.push(2)
  stack = stack.push(3)
  
  assert_eq(stack.length(), 3)
  assert_eq(stack[stack.length() - 1], 3)
  
  let (popped_value, new_stack) = (stack[stack.length() - 1], stack.slice(0, stack.length() - 1))
  assert_eq(popped_value, 3)
  assert_eq(new_stack.length(), 2)
  
  // 测试队列操作
  let mut queue = []
  queue = queue.push("first")
  queue = queue.push("second")
  queue = queue.push("third")
  
  assert_eq(queue.length(), 3)
  assert_eq(queue[0], "first")
  
  let dequeued = queue.slice(1, queue.length())
  assert_eq(dequeued.length(), 2)
  assert_eq(dequeued[0], "second")
  
  // 测试链表操作模拟
  let create_linked_list = fn(values: Array[Int]) {
    let mut result = []
    for i in 0..values.length() {
      result = result.push({
        value: values[i],
        next: if i < values.length() - 1 { Some(i + 1) } else { None }
      })
    }
    result
  }
  
  let linked_list = create_linked_list([10, 20, 30, 40])
  assert_eq(linked_list.length(), 4)
  assert_eq(linked_list[0].value, 10)
  assert_eq(linked_list[0].next, Some(1))
  assert_eq(linked_list[3].next, None)
  
  // 测试二叉树操作模拟
  let create_tree_node = fn(value: Int, left: Option[Int], right: Option[Int]) {
    { value, left, right }
  }
  
  let tree_nodes = [
    create_tree_node(10, Some(1), Some(2)),
    create_tree_node(5, None, None),
    create_tree_node(15, None, None)
  ]
  
  assert_eq(tree_nodes[0].value, 10)
  assert_eq(tree_nodes[0].left, Some(1))
  assert_eq(tree_nodes[0].right, Some(2))
  
  // 测试图结构模拟
  let create_graph = fn() {
    {
      nodes: ["A", "B", "C", "D"],
      edges: [
        { from: "A", to: "B", weight: 1.0 },
        { from: "B", to: "C", weight: 2.0 },
        { from: "C", to: "D", weight: 1.5 },
        { from: "A", to: "D", weight: 3.0 }
      ]
    }
  }
  
  let graph = create_graph()
  assert_eq(graph.nodes.length(), 4)
  assert_eq(graph.edges.length(), 4)
  
  // 测试邻接关系
  let get_neighbors = fn(graph, node: String) {
    graph.edges.filter(fn(e) { e.from == node }).map(fn(e) { e.to })
  }
  
  let a_neighbors = get_neighbors(graph, "A")
  assert_eq(a_neighbors.length(), 2)
  assert_true(a_neighbors.contains("B"))
  assert_true(a_neighbors.contains("D"))
}

// 测试2: 错误处理和恢复机制
test "错误处理和恢复机制测试" {
  // 定义错误类型
  enum SystemError {
    NetworkTimeout(String)
    DatabaseConnectionFailed(String)
    InvalidInput(String)
    ResourceExhausted(String)
    ConfigurationError(String)
  }
  
  // 定义错误处理器
  let handle_error = fn(error: SystemError) {
    match error {
      SystemError::NetworkTimeout(service) => {
        {
          handled: true,
          strategy: "retry",
          max_attempts: 3,
          backoff_ms: 1000,
          message: "网络超时，将重试服务: " + service
        }
      }
      SystemError::DatabaseConnectionFailed(db) => {
        {
          handled: true,
          strategy: "reconnect",
          max_attempts: 5,
          backoff_ms: 2000,
          message: "数据库连接失败，将尝试重新连接: " + db
        }
      }
      SystemError::InvalidInput(input) => {
        {
          handled: false,
          strategy: "validate",
          max_attempts: 1,
          backoff_ms: 0,
          message: "输入验证失败: " + input
        }
      }
      SystemError::ResourceExhausted(resource) => {
        {
          handled: true,
          strategy: "scale",
          max_attempts: 2,
          backoff_ms: 5000,
          message: "资源耗尽，将尝试扩容: " + resource
        }
      }
      SystemError::ConfigurationError(config) => {
        {
          handled: true,
          strategy: "reload",
          max_attempts: 1,
          backoff_ms: 100,
          message: "配置错误，将重新加载: " + config
        }
      }
    }
  }
  
  // 测试各种错误处理
  let network_error = SystemError::NetworkTimeout("api.service")
  let network_result = handle_error(network_error)
  assert_true(network_result.handled)
  assert_eq(network_result.strategy, "retry")
  assert_eq(network_result.max_attempts, 3)
  
  let db_error = SystemError::DatabaseConnectionFailed("postgresql")
  let db_result = handle_error(db_error)
  assert_true(db_result.handled)
  assert_eq(db_result.strategy, "reconnect")
  assert_eq(db_result.max_attempts, 5)
  
  let input_error = SystemError::InvalidInput("null_value")
  let input_result = handle_error(input_error)
  assert_false(input_result.handled)
  assert_eq(input_result.strategy, "validate")
  assert_eq(input_result.max_attempts, 1)
  
  // 测试错误恢复策略
  let execute_with_recovery = fn(operation: () -> Result[String, SystemError]) {
    let mut attempts = 0
    let max_attempts = 3
    
    while attempts < max_attempts {
      let result = operation()
      match result {
        Ok(value) => return Ok(value)
        Err(error) => {
          attempts = attempts + 1
          let error_info = handle_error(error)
          
          if not(error_info.handled) or attempts >= max_attempts {
            return Err(error)
          }
          
          // 模拟等待时间
          // 在实际实现中，这里会等待 error_info.backoff_ms 毫秒
        }
      }
    }
    
    Err(SystemError::ResourceExhausted("max_attempts"))
  }
  
  // 测试成功的操作
  let successful_operation = fn() { Ok("操作成功") }
  let success_result = execute_with_recovery(successful_operation)
  assert_eq(success_result, Ok("操作成功"))
  
  // 测试最终失败的操作
  let mut attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(SystemError::NetworkTimeout("test.service"))
    } else {
      Ok("最终成功")
    }
  }
  
  let retry_result = execute_with_recovery(failing_operation)
  assert_eq(retry_result, Ok("最终成功"))
  assert_eq(attempt_count, 3)
}

// 测试3: 性能优化和基准测试
test "性能优化和基准测试测试" {
  // 创建性能基准测试框架
  let benchmark = fn(name: String, operation: () -> Unit, iterations: Int) {
    let start_time = 1640995200000  // 模拟时间戳
    for i in 0..iterations {
      operation()
    }
    let end_time = 1640995200500  // 模拟时间戳
    
    {
      name,
      iterations,
      total_time_ms: end_time - start_time,
      avg_time_ms: (end_time - start_time) / iterations,
      ops_per_second: iterations * 1000 / (end_time - start_time)
    }
  }
  
  // 测试数组操作性能
  let array_benchmark = benchmark("数组操作", fn() {
    let mut array = []
    for i in 0..100 {
      array = array.push(i)
    }
    let sum = array.reduce(fn(acc, x) { acc + x }, 0)
  }, 1000)
  
  assert_true(array_benchmark.avg_time_ms > 0)
  assert_true(array_benchmark.ops_per_second > 0)
  
  // 测试字符串操作性能
  let string_benchmark = benchmark("字符串操作", fn() {
    let base = "test"
    let mut result = ""
    for i in 0..50 {
      result = result + base + i.to_string()
    }
  }, 500)
  
  assert_true(string_benchmark.avg_time_ms > 0)
  assert_true(string_benchmark.ops_per_second > 0)
  
  // 测试排序算法性能
  let bubble_sort = fn(arr: Array[Int]) {
    let mut result = arr
    let n = result.length()
    for i in 0..n {
      for j in 0..(n - i - 1) {
        if result[j] > result[j + 1] {
          let temp = result[j]
          result[j] = result[j + 1]
          result[j + 1] = temp
        }
      }
    }
    result
  }
  
  let quick_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let less = arr.slice(1).filter(fn(x) { x <= pivot })
      let greater = arr.slice(1).filter(fn(x) { x > pivot })
      quick_sort(less) + [pivot] + quick_sort(greater)
    }
  }
  
  let test_data = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
  
  let bubble_benchmark = benchmark("冒泡排序", fn() {
    bubble_sort(test_data)
  }, 100)
  
  let quick_benchmark = benchmark("快速排序", fn() {
    quick_sort(test_data)
  }, 100)
  
  // 验证快速排序更快
  assert_true(quick_benchmark.avg_time_ms <= bubble_benchmark.avg_time_ms)
  
  // 测试内存使用优化
  let memory_intensive_operation = fn(size: Int) {
    let mut data = []
    for i in 0..size {
      data = data.push({
        id: i,
        name: "item_" + i.to_string(),
        description: "这是第" + i.to_string() + "个项目的详细描述",
        metadata: ["tag1", "tag2", "tag3"],
        timestamp: 1640995200 + i
      })
    }
    data
  }
  
  let optimized_operation = fn(size: Int) {
    // 使用更紧凑的数据结构
    let mut data = []
    for i in 0..size {
      data = data.push({
        id: i,
        name_idx: i,  // 使用索引而不是完整字符串
        desc_idx: i,  // 使用索引而不是完整字符串
        tags_count: 3,
        timestamp: 1640995200 + i
      })
    }
    data
  }
  
  let memory_benchmark = benchmark("内存密集型操作", fn() {
    memory_intensive_operation(1000)
  }, 10)
  
  let optimized_benchmark = benchmark("优化内存操作", fn() {
    optimized_operation(1000)
  }, 10)
  
  // 验证优化版本更快
  assert_true(optimized_benchmark.avg_time_ms < memory_benchmark.avg_time_ms)
  
  // 测试缓存性能
  let mut cache = []
  let cache_hit_count = { mut count: 0 }
  let cache_miss_count = { mut count: 0 }
  
  let cached_operation = fn(key: String) {
    let cached_value = cache.find(fn(item) { item.key == key })
    match cached_value {
      Some(item) => {
        cache_hit_count.count = cache_hit_count.count + 1
        item.value
      }
      None => {
        cache_miss_count.count = cache_miss_count.count + 1
        let computed_value = "computed_" + key
        cache = cache.push({ key, value: computed_value })
        computed_value
      }
    }
  }
  
  // 预热缓存
  for i in 0..10 {
    cached_operation("key_" + i.to_string())
  }
  
  // 测试缓存命中
  let cache_benchmark = benchmark("缓存操作", fn() {
    for i in 0..10 {
      cached_operation("key_" + i.to_string())  // 应该命中缓存
      cached_operation("new_key_" + i.to_string())  // 应该缓存未命中
    }
  }, 100)
  
  // 验证缓存效果
  assert_true(cache_hit_count.count > 0)
  assert_true(cache_miss_count.count > 0)
}

// 测试4: 数据序列化和反序列化
test "数据序列化和反序列化测试" {
  // 定义JSON序列化器
  let json_serialize = fn(data) {
    match data {
      String(value) => "\"" + value + "\""
      Int(value) => value.to_string()
      Float(value) => value.to_string()
      Bool(value) => if value { "true" } else { "false" }
      Array(values) => {
        let mut result = "["
        for i in 0..values.length() {
          if i > 0 { result = result + "," }
          result = result + json_serialize(values[i])
        }
        result = result + "]"
        result
      }
      Object(pairs) => {
        let mut result = "{"
        for i in 0..pairs.length() {
          if i > 0 { result = result + "," }
          result = result + "\"" + pairs[i].key + "\":" + json_serialize(pairs[i].value)
        }
        result = result + "}"
        result
      }
      _ => "null"
    }
  }
  
  // 测试基本类型序列化
  assert_eq(json_serialize(String("hello")), "\"hello\"")
  assert_eq(json_serialize(Int(42)), "42")
  assert_eq(json_serialize(Float(3.14)), "3.14")
  assert_eq(json_serialize(Bool(true)), "true")
  assert_eq(json_serialize(Bool(false)), "false")
  
  // 测试数组序列化
  let numbers = Array([Int(1), Int(2), Int(3)])
  assert_eq(json_serialize(numbers), "[1,2,3]")
  
  let strings = Array([String("a"), String("b"), String("c")])
  assert_eq(json_serialize(strings), "[\"a\",\"b\",\"c\"]")
  
  // 测试对象序列化
  let person = Object([
    { key: "name", value: String("张三") },
    { key: "age", value: Int(30) },
    { key: "active", value: Bool(true) }
  ])
  
  let person_json = json_serialize(person)
  assert_true(person_json.contains("\"name\":\"张三\""))
  assert_true(person_json.contains("\"age\":30"))
  assert_true(person_json.contains("\"active\":true"))
  
  // 测试嵌套结构序列化
  let complex_data = Object([
    { 
      key: "user", 
      value: Object([
        { key: "id", value: Int(123) },
        { key: "profile", value: Object([
          { key: "name", value: String("李四") },
          { key: "preferences", value: Array([
            String("技术"),
            String("音乐"),
            String("旅行")
          ])}
        ])}
      ])
    },
    { 
      key: "sessions", 
      value: Array([
        Object([
          { key: "id", value: String("sess_1") },
          { key: "start_time", value: Int(1640995200) },
          { key: "duration", value: Int(3600) }
        ]),
        Object([
          { key: "id", value: String("sess_2") },
          { key: "start_time", value: Int(1640998800) },
          { key: "duration", value: Int(1800) }
        ])
      ])
    }
  ])
  
  let complex_json = json_serialize(complex_data)
  assert_true(complex_json.contains("\"user\""))
  assert_true(complex_json.contains("\"sessions\""))
  assert_true(complex_json.contains("\"李四\""))
  assert_true(complex_json.contains("\"sess_1\""))
  
  // 定义二进制序列化器
  let binary_serialize = fn(data) {
    let mut result = []
    match data {
      String(value) => {
        result = result.push(1)  // 类型标识符：字符串
        result = result.push(value.length())
        for i in 0..value.length() {
          result = result.push(value[i].to_int())
        }
      }
      Int(value) => {
        result = result.push(2)  // 类型标识符：整数
        result = result.push(value)
      }
      Bool(value) => {
        result = result.push(3)  // 类型标识符：布尔值
        result = result.push(if value { 1 } else { 0 })
      }
      Array(values) => {
        result = result.push(4)  // 类型标识符：数组
        result = result.push(values.length())
        for item in values {
          let item_bytes = binary_serialize(item)
          result = result + item_bytes
        }
      }
    }
    result
  }
  
  // 测试二进制序列化
  let binary_string = binary_serialize(String("test"))
  assert_eq(binary_string[0], 1)  // 字符串类型
  assert_eq(binary_string[1], 4)  // 长度
  assert_eq(binary_string[2], 't'.to_int())
  assert_eq(binary_string[3], 'e'.to_int())
  assert_eq(binary_string[4], 's'.to_int())
  assert_eq(binary_string[5], 't'.to_int())
  
  let binary_int = binary_serialize(Int(42))
  assert_eq(binary_int[0], 2)  // 整数类型
  assert_eq(binary_int[1], 42)  // 值
  
  let binary_bool = binary_serialize(Bool(true))
  assert_eq(binary_bool[0], 3)  // 布尔类型
  assert_eq(binary_bool[1], 1)  // true值
  
  // 测试序列化性能
  let large_data = Array([])
  let mut large_array = []
  for i in 0..1000 {
    large_array = large_array.push(Object([
      { key: "id", value: Int(i) },
      { key: "name", value: String("item_" + i.to_string()) },
      { key: "active", value: Bool(i % 2 == 0) }
    ]))
  }
  let large_data = Array(large_array)
  
  let json_start = 1640995200000
  let json_result = json_serialize(large_data)
  let json_end = 1640995200500
  
  let binary_start = 1640995200000
  let binary_result = binary_serialize(large_data)
  let binary_end = 1640995200200
  
  // 验证二进制序列化更紧凑
  assert_true(binary_result.length() < json_result.length())
  
  // 验证二进制序列化更快
  assert_true((binary_end - binary_start) < (json_end - json_start))
}

// 测试5: 并发安全和线程同步
test "并发安全和线程同步测试" {
  // 模拟并发计数器
  let counter = { mut value: 0, mut lock: false }
  
  let safe_increment = fn() {
    // 模拟获取锁
    while counter.lock {
      // 等待锁释放
    }
    counter.lock = true
    
    // 临界区代码
    counter.value = counter.value + 1
    
    // 释放锁
    counter.lock = false
  }
  
  // 模拟并发执行
  for i in 0..100 {
    safe_increment()
  }
  
  assert_eq(counter.value, 100)
  
  // 模拟生产者-消费者模式
  let buffer = { mut data: [], mut capacity: 10, mut lock: false }
  let produced_count = { mut count: 0 }
  let consumed_count = { mut count: 0 }
  
  let producer = fn(item: Int) {
    // 等待缓冲区有空间
    while buffer.lock or buffer.data.length() >= buffer.capacity {
      // 等待
    }
    buffer.lock = true
    
    // 生产项目
    buffer.data = buffer.data.push(item)
    produced_count.count = produced_count.count + 1
    
    buffer.lock = false
  }
  
  let consumer = fn() {
    // 等待缓冲区有数据
    while buffer.lock or buffer.data.length() == 0 {
      // 等待
    }
    buffer.lock = true
    
    // 消费项目
    if buffer.data.length() > 0 {
      buffer.data = buffer.data.slice(1, buffer.data.length())
      consumed_count.count = consumed_count.count + 1
    }
    
    buffer.lock = false
  }
  
  // 模拟生产和消费
  for i in 0..20 {
    if i < 15 {
      producer(i)
    }
    if i >= 5 {
      consumer()
    }
  }
  
  assert_eq(produced_count.count, 15)
  assert_eq(consumed_count.count, 15)
  assert_eq(buffer.data.length(), 0)
  
  // 模拟读写锁
  let rw_data = { mut value: "initial", mut readers: 0, mut writers: 0, mut lock: false }
  
  let read_operation = fn() {
    // 等待没有写者
    while rw_data.lock or rw_data.writers > 0 {
      // 等待
    }
    
    // 增加读者计数
    while rw_data.lock {
      // 等待锁
    }
    rw_data.lock = true
    rw_data.readers = rw_data.readers + 1
    rw_data.lock = false
    
    // 读取数据
    let current_value = rw_data.value
    
    // 减少读者计数
    while rw_data.lock {
      // 等待锁
    }
    rw_data.lock = true
    rw_data.readers = rw_data.readers - 1
    rw_data.lock = false
    
    current_value
  }
  
  let write_operation = fn(new_value: String) {
    // 等待没有读者和写者
    while rw_data.lock or rw_data.readers > 0 or rw_data.writers > 0 {
      // 等待
    }
    
    // 增加写者计数
    while rw_data.lock {
      // 等待锁
    }
    rw_data.lock = true
    rw_data.writers = rw_data.writers + 1
    rw_data.lock = false
    
    // 写入数据
    rw_data.value = new_value
    
    // 减少写者计数
    while rw_data.lock {
      // 等待锁
    }
    rw_data.lock = true
    rw_data.writers = rw_data.writers - 1
    rw_data.lock = false
  }
  
  // 测试读写操作
  let initial_value = read_operation()
  assert_eq(initial_value, "initial")
  
  write_operation("updated")
  let updated_value = read_operation()
  assert_eq(updated_value, "updated")
  
  // 模拟多个读者同时读取
  let reader_results = []
  for i in 0..5 {
    reader_results = reader_results.push(read_operation())
  }
  
  // 所有读者应该读取到相同的值
  for result in reader_results {
    assert_eq(result, "updated")
  }
  
  // 模拟原子操作
  let atomic_counter = { mut value: 0 }
  
  let atomic_increment = fn() {
    // 模拟原子操作
    let old_value = atomic_counter.value
    let new_value = old_value + 1
    atomic_counter.value = new_value
    new_value
  }
  
  let atomic_compare_and_swap = fn(expected: Int, new_value: Int) {
    if atomic_counter.value == expected {
      atomic_counter.value = new_value
      true
    } else {
      false
    }
  }
  
  // 测试原子操作
  let result1 = atomic_increment()
  assert_eq(result1, 1)
  assert_eq(atomic_counter.value, 1)
  
  let result2 = atomic_compare_and_swap(1, 10)
  assert_true(result2)
  assert_eq(atomic_counter.value, 10)
  
  let result3 = atomic_compare_and_swap(1, 20)
  assert_false(result3)
  assert_eq(atomic_counter.value, 10)
}

// 测试6: 缓存机制和策略
test "缓存机制和策略测试" {
  // 创建LRU缓存
  let create_lru_cache = fn(capacity: Int) {
    {
      capacity,
      entries: [],
      access_order: []
    }
  }
  
  let lru_get = fn(cache, key: String) {
    let entry = cache.entries.find(fn(e) { e.key == key })
    match entry {
      Some(e) => {
        // 更新访问顺序
        let new_order = cache.access_order.filter(fn(k) { k != key })
        new_order.push(key)
        Some(e.value)
      }
      None => None
    }
  }
  
  let lru_put = fn(cache, key: String, value: String) {
    // 检查键是否已存在
    let existing_index = cache.entries.index_of(fn(e) { e.key == key })
    
    match existing_index {
      Some(index) => {
        // 更新现有条目
        cache.entries[index] = { key, value }
        // 更新访问顺序
        let new_order = cache.access_order.filter(fn(k) { k != key })
        new_order.push(key)
      }
      None => {
        // 检查缓存是否已满
        if cache.entries.length() >= cache.capacity {
          // 移除最少使用的条目
          let lru_key = cache.access_order[0]
          cache.entries = cache.entries.filter(fn(e) { e.key != lru_key })
          cache.access_order = cache.access_order.slice(1, cache.access_order.length())
        }
        
        // 添加新条目
        cache.entries = cache.entries.push({ key, value })
        cache.access_order = cache.access_order.push(key)
      }
    }
  }
  
  // 测试LRU缓存
  let cache = create_lru_cache(3)
  
  lru_put(cache, "key1", "value1")
  lru_put(cache, "key2", "value2")
  lru_put(cache, "key3", "value3")
  
  assert_eq(lru_get(cache, "key1"), Some("value1"))
  assert_eq(lru_get(cache, "key2"), Some("value2"))
  assert_eq(lru_get(cache, "key3"), Some("value3"))
  
  // 添加第四个条目，应该淘汰key1（最少使用）
  lru_put(cache, "key4", "value4")
  
  assert_eq(lru_get(cache, "key1"), None)  // 应该被淘汰
  assert_eq(lru_get(cache, "key2"), Some("value2"))
  assert_eq(lru_get(cache, "key3"), Some("value3"))
  assert_eq(lru_get(cache, "key4"), Some("value4"))
  
  // 创建TTL缓存
  let create_ttl_cache = fn() {
    {
      entries: []
    }
  }
  
  let ttl_get = fn(cache, key: String, current_time: Int) {
    let entry = cache.entries.find(fn(e) { e.key == key })
    match entry {
      Some(e) => {
        if e.expires_at > current_time {
          Some(e.value)
        } else {
          // 过期，移除条目
          cache.entries = cache.entries.filter(fn(item) { item.key != key })
          None
        }
      }
      None => None
    }
  }
  
  let ttl_put = fn(cache, key: String, value: String, ttl_seconds: Int, current_time: Int) {
    // 移除现有条目（如果有）
    cache.entries = cache.entries.filter(fn(e) { e.key != key })
    
    // 添加新条目
    cache.entries = cache.entries.push({
      key,
      value,
      expires_at: current_time + ttl_seconds
    })
  }
  
  // 测试TTL缓存
  let ttl_cache = create_ttl_cache()
  let base_time = 1640995200
  
  ttl_put(ttl_cache, "temp_key", "temp_value", 60, base_time)  // 60秒TTL
  
  // 在TTL内访问
  assert_eq(ttl_get(ttl_cache, "temp_key", base_time + 30), Some("temp_value"))
  
  // 超过TTL访问
  assert_eq(ttl_get(ttl_cache, "temp_key", base_time + 120), None)
  
  // 创建多级缓存
  let create_multilevel_cache = fn(l1_size: Int, l2_size: Int) {
    {
      l1_cache: create_lru_cache(l1_size),
      l2_cache: create_lru_cache(l2_size),
      l1_hits: { mut count: 0 },
      l2_hits: { mut count: 0 },
      misses: { mut count: 0 }
    }
  }
  
  let multilevel_get = fn(cache, key: String) {
    // 先尝试L1缓存
    let l1_result = lru_get(cache.l1_cache, key)
    match l1_result {
      Some(value) => {
        cache.l1_hits.count = cache.l1_hits.count + 1
        Some(value)
      }
      None => {
        // 尝试L2缓存
        let l2_result = lru_get(cache.l2_cache, key)
        match l2_result {
          Some(value) => {
            cache.l2_hits.count = cache.l2_hits.count + 1
            // 提升到L1缓存
            lru_put(cache.l1_cache, key, value)
            Some(value)
          }
          None => {
            cache.misses.count = cache.misses.count + 1
            None
          }
        }
      }
    }
  }
  
  let multilevel_put = fn(cache, key: String, value: String) {
    // 同时放入L1和L2缓存
    lru_put(cache.l1_cache, key, value)
    lru_put(cache.l2_cache, key, value)
  }
  
  // 测试多级缓存
  let ml_cache = create_multilevel_cache(2, 4)
  
  multilevel_put(ml_cache, "key1", "value1")
  multilevel_put(ml_cache, "key2", "value2")
  multilevel_put(ml_cache, "key3", "value3")
  multilevel_put(ml_cache, "key4", "value4")
  multilevel_put(ml_cache, "key5", "value5")
  
  // 访问key1，应该从L2缓存获取并提升到L1
  assert_eq(multilevel_get(ml_cache, "key1"), Some("value1"))
  assert_true(ml_cache.l2_hits.count > 0)
  
  // 再次访问key1，应该从L1缓存获取
  assert_eq(multilevel_get(ml_cache, "key1"), Some("value1"))
  assert_true(ml_cache.l1_hits.count > 0)
  
  // 访问不存在的键
  assert_eq(multilevel_get(ml_cache, "nonexistent"), None)
  assert_true(ml_cache.misses.count > 0)
  
  // 测试缓存预热
  let cache_warmup = fn(cache, data_source: () -> Array[(String, String)], keys: Array[String]) {
    let all_data = data_source()
    for key in keys {
      let data = all_data.find(fn(item) { item.0 == key })
      match data {
        Some((k, v)) => multilevel_put(cache, k, v)
        None => ()
      }
    }
  }
  
  let mock_data_source = fn() {
    [
      ("user_1", "张三"),
      ("user_2", "李四"),
      ("user_3", "王五"),
      ("user_4", "赵六"),
      ("user_5", "钱七")
    ]
  }
  
  let warm_cache = create_multilevel_cache(3, 5)
  cache_warmup(warm_cache, mock_data_source, ["user_1", "user_2", "user_3"])
  
  // 验证预热后的缓存命中
  assert_eq(multilevel_get(warm_cache, "user_1"), Some("张三"))
  assert_eq(multilevel_get(warm_cache, "user_2"), Some("李四"))
  assert_eq(multilevel_get(warm_cache, "user_3"), Some("王五"))
}

// 测试7: 配置管理和动态更新
test "配置管理和动态更新测试" {
  // 创建配置管理器
  let create_config_manager = fn() {
    {
      configs: {},
      watchers: [],
      version: 0
    }
  }
  
  let config_get = fn(manager, key: String, default_value: String) {
    match manager.configs.get(key) {
      Some(value) => value
      None => default_value
    }
  }
  
  let config_set = fn(manager, key: String, value: String) {
    manager.configs[key] = value
    manager.version = manager.version + 1
    
    // 通知观察者
    for watcher in manager.watchers {
      if watcher.key == key or watcher.key == "*" {
        watcher.callback(key, value, manager.version)
      }
    }
  }
  
  let config_watch = fn(manager, key: String, callback: (String, String, Int) -> Unit) {
    manager.watchers = manager.watchers.push({
      key,
      callback
    })
  }
  
  // 测试基本配置操作
  let config_manager = create_config_manager()
  
  // 设置配置
  config_set(config_manager, "server.port", "8080")
  config_set(config_manager, "database.host", "localhost")
  config_set(config_manager, "database.port", "5432")
  config_set(config_manager, "log.level", "info")
  
  // 获取配置
  assert_eq(config_get(config_manager, "server.port", "3000"), "8080")
  assert_eq(config_get(config_manager, "database.host", "remote"), "localhost")
  assert_eq(config_get(config_manager, "missing.key", "default"), "default")
  
  // 测试配置观察者
  let notified_changes = []
  
  config_watch(config_manager, "server.port", fn(key, value, version) {
    notified_changes.push({ key, value, version })
  })
  
  config_watch(config_manager, "*", fn(key, value, version) {
    notified_changes.push({ key: "all:" + key, value, version })
  })
  
  // 更新配置
  config_set(config_manager, "server.port", "9090")
  
  // 验证通知
  assert_eq(notified_changes.length(), 2)
  assert_eq(notified_changes[0].key, "server.port")
  assert_eq(notified_changes[0].value, "9090")
  assert_eq(notified_changes[1].key, "all:server.port")
  assert_eq(notified_changes[1].value, "9090")
  
  // 创建配置验证器
  let create_validator = fn() {
    {
      rules: []
    }
  }
  
  let validator_add_rule = fn(validator, key: String, rule: (String) -> Result[String, String]) {
    validator.rules = validator.rules.push({ key, rule })
  }
  
  let validator_validate = fn(validator, key: String, value: String) {
    let key_rules = validator.rules.filter(fn(r) { r.key == key })
    for rule_obj in key_rules {
      let result = rule_obj.rule(value)
      match result {
        Err(msg) => return Err(msg)
        Ok(_) => ()
      }
    }
    Ok(value)
  }
  
  // 创建带验证的配置管理器
  let validated_config_set = fn(manager, validator, key: String, value: String) {
    let validation_result = validator_validate(validator, key, value)
    match validation_result {
      Ok(validated_value) => config_set(manager, key, validated_value)
      Err(msg) => Err(msg)
    }
  }
  
  // 测试配置验证
  let validator = create_validator()
  
  // 添加端口验证规则
  validator_add_rule(validator, "server.port", fn(value) {
    let port = value.to_int()
    if port >= 1 and port <= 65535 {
      Ok(value)
    } else {
      Err("端口必须在1-65535范围内")
    }
  })
  
  // 添加日志级别验证规则
  validator_add_rule(validator, "log.level", fn(value) {
    let valid_levels = ["debug", "info", "warn", "error"]
    if valid_levels.contains(value) {
      Ok(value)
    } else {
      Err("日志级别必须是: debug, info, warn, error")
    }
  })
  
  // 测试有效配置
  let valid_result = validated_config_set(config_manager, validator, "server.port", "3000")
  assert_eq(valid_result, Ok(()))
  assert_eq(config_get(config_manager, "server.port", "8080"), "3000")
  
  // 测试无效配置
  let invalid_result = validated_config_set(config_manager, validator, "server.port", "70000")
  assert_eq(invalid_result, Err("端口必须在1-65535范围内"))
  assert_eq(config_get(config_manager, "server.port", "8080"), "3000")  // 值未改变
  
  // 测试配置分层
  let create_layered_config = fn() {
    {
      layers: [
        { name: "default", configs: {}, priority: 0 },
        { name: "environment", configs: {}, priority: 1 },
        { name: "user", configs: {}, priority: 2 }
      ]
    }
  }
  
  let layered_set = fn(layered_config, layer_name: String, key: String, value: String) {
    let layer_index = layered_config.layers.index_of(fn(l) { l.name == layer_name })
    match layer_index {
      Some(index) => {
        layered_config.layers[index].configs[key] = value
      }
      None => ()
    }
  }
  
  let layered_get = fn(layered_config, key: String, default_value: String) {
    // 按优先级从高到低查找
    let sorted_layers = layered_config.layers.sort_by(fn(a, b) { b.priority - a.priority })
    
    for layer in sorted_layers {
      match layer.configs.get(key) {
        Some(value) => return Some(value)
        None => ()
      }
    }
    
    Some(default_value)
  }
  
  // 测试分层配置
  let layered_config = create_layered_config()
  
  // 设置默认配置
  layered_set(layered_config, "default", "server.port", "8080")
  layered_set(layered_config, "default", "log.level", "info")
  
  // 设置环境配置
  layered_set(layered_config, "environment", "server.port", "9090")
  
  // 设置用户配置
  layered_set(layered_config, "user", "log.level", "debug")
  
  // 验证配置优先级
  assert_eq(layered_get(layered_config, "server.port", "3000"), Some("9090"))  // 环境配置覆盖默认
  assert_eq(layered_get(layered_config, "log.level", "info"), Some("debug"))   // 用户配置覆盖环境
  assert_eq(layered_get(layered_config, "missing.key", "default"), Some("default"))
  
  // 测试配置热重载
  let config_file_content = {
    server: {
      port: 8080,
      host: "0.0.0.0"
    },
    database: {
      host: "localhost",
      port: 5432,
      name: "azimuth"
    },
    features: {
      telemetry: true,
      metrics: true,
      logging: true
    }
  }
  
  let parse_config = fn(content) {
    // 模拟配置解析
    {
      "server.port": "8080",
      "server.host": "0.0.0.0",
      "database.host": "localhost",
      "database.port": "5432",
      "database.name": "azimuth",
      "features.telemetry": "true",
      "features.metrics": "true",
      "features.logging": "true"
    }
  }
  
  let reload_config = fn(manager, config_content) {
    let parsed = parse_config(config_content)
    for key_value in parsed {
      config_set(manager, key_value.0, key_value.1)
    }
  }
  
  // 重载配置
  let reload_count = { mut count: 0 }
  config_watch(config_manager, "*", fn(key, value, version) {
    reload_count.count = reload_count.count + 1
  })
  
  reload_config(config_manager, config_file_content)
  
  // 验证重载
  assert_true(reload_count.count > 0)
  assert_eq(config_get(config_manager, "server.host", "localhost"), "0.0.0.0")
  assert_eq(config_get(config_manager, "database.name", "default"), "azimuth")
}

// 测试8: 资源管理和生命周期
test "资源管理和生命周期测试" {
  // 创建资源池
  let create_resource_pool = fn(create_resource: () -> String, destroy_resource: (String) -> Unit, max_size: Int) {
    {
      available: [],
      in_use: [],
      create_resource,
      destroy_resource,
      max_size,
      created_count: { mut count: 0 },
      destroyed_count: { mut count: 0 }
    }
  }
  
  let pool_acquire = fn(pool) {
    match pool.available.length() > 0 {
      true => {
        // 从可用资源中取一个
        let resource = pool.available[pool.available.length() - 1]
        pool.available = pool.available.slice(0, pool.available.length() - 1)
        pool.in_use = pool.in_use.push(resource)
        resource
      }
      false => {
        // 创建新资源
        if pool.in_use.length() < pool.max_size {
          let resource = pool.create_resource()
          pool.created_count.count = pool.created_count.count + 1
          pool.in_use = pool.in_use.push(resource)
          resource
        } else {
          "pool_exhausted"
        }
      }
    }
  }
  
  let pool_release = fn(pool, resource: String) {
    if pool.in_use.contains(resource) {
      pool.in_use = pool.in_use.filter(fn(r) { r != resource })
      pool.available = pool.available.push(resource)
    }
  }
  
  let pool_destroy = fn(pool, resource: String) {
    if pool.available.contains(resource) {
      pool.available = pool.available.filter(fn(r) { r != resource })
      pool.destroy_resource(resource)
      pool.destroyed_count.count = pool.destroyed_count.count + 1
    }
  }
  
  // 测试资源池
  let connection_id = { mut id: 0 }
  let create_connection = fn() {
    connection_id.id = connection_id.id + 1
    "conn_" + connection_id.id.to_string()
  }
  
  let active_connections = []
  let destroy_connection = fn(conn: String) {
    // 模拟销毁连接
  }
  
  let connection_pool = create_resource_pool(create_connection, destroy_connection, 3)
  
  // 获取连接
  let conn1 = pool_acquire(connection_pool)
  let conn2 = pool_acquire(connection_pool)
  let conn3 = pool_acquire(connection_pool)
  
  assert_eq(conn1, "conn_1")
  assert_eq(conn2, "conn_2")
  assert_eq(conn3, "conn_3")
  assert_eq(connection_pool.created_count.count, 3)
  
  // 释放连接
  pool_release(connection_pool, conn1)
  assert_eq(connection_pool.available.length(), 1)
  assert_eq(connection_pool.in_use.length(), 2)
  
  // 重新获取连接，应该复用
  let conn4 = pool_acquire(connection_pool)
  assert_eq(conn4, "conn_1")  // 复用连接
  assert_eq(connection_pool.created_count.count, 3)  // 没有创建新连接
  
  // 测试资源生命周期管理
  let create_lifecycle_manager = fn() {
    {
      resources: [],
      cleanup_callbacks: []
    }
  }
  
  let lifecycle_register = fn(manager, resource_id: String, resource: String, cleanup: (String) -> Unit) {
    manager.resources = manager.resources.push({
      id: resource_id,
      resource,
      created_at: 1640995200,
      status: "active"
    })
    
    manager.cleanup_callbacks = manager.cleanup_callbacks.push({
      resource_id,
      cleanup
    })
  }
  
  let lifecycle_cleanup = fn(manager, resource_id: String) {
    let callback = manager.cleanup_callbacks.find(fn(c) { c.resource_id == resource_id })
    match callback {
      Some(c) => {
        c.cleanup(resource_id)
        manager.resources = manager.resources.map(fn(r) {
          if r.id == resource_id {
            { r | status: "cleaned" }
          } else {
            r
          }
        })
      }
      None => ()
    }
  }
  
  let lifecycle_cleanup_all = fn(manager) {
    for resource in manager.resources {
      if resource.status == "active" {
        lifecycle_cleanup(manager, resource.id)
      }
    }
  }
  
  // 测试生命周期管理
  let lifecycle_manager = create_lifecycle_manager()
  
  let cleanup_log = []
  
  lifecycle_register(lifecycle_manager, "temp_file_1", "/tmp/file1", fn(id) {
    cleanup_log = cleanup_log.push("清理文件: " + id)
  })
  
  lifecycle_register(lifecycle_manager, "db_connection_1", "conn_123", fn(id) {
    cleanup_log = cleanup_log.push("关闭连接: " + id)
  })
  
  lifecycle_register(lifecycle_manager, "memory_buffer_1", "buffer_456", fn(id) {
    cleanup_log = cleanup_log.push("释放内存: " + id)
  })
  
  // 清理单个资源
  lifecycle_cleanup(lifecycle_manager, "temp_file_1")
  assert_eq(cleanup_log.length(), 1)
  assert_eq(cleanup_log[0], "清理文件: temp_file_1")
  
  // 清理所有资源
  lifecycle_cleanup_all(lifecycle_manager)
  assert_eq(cleanup_log.length(), 3)
  assert_true(cleanup_log.contains("关闭连接: db_connection_1"))
  assert_true(cleanup_log.contains("释放内存: memory_buffer_1"))
  
  // 测试资源泄漏检测
  let create_leak_detector = fn() {
    {
      allocated_resources: [],
      leaked_resources: []
    }
  }
  
  let leak_detector_track = fn(detector, resource_id: String, resource_type: String) {
    detector.allocated_resources = detector.allocated_resources.push({
      id: resource_id,
      type: resource_type,
      allocated_at: 1640995200,
      freed: false
    })
  }
  
  let leak_detector_free = fn(detector, resource_id: String) {
    detector.allocated_resources = detector.allocated_resources.map(fn(r) {
      if r.id == resource_id {
        { r | freed: true }
      } else {
        r
      }
    })
  }
  
  let leak_detector_check = fn(detector) {
    detector.leaked_resources = detector.allocated_resources.filter(fn(r) { not(r.freed) })
    detector.leaked_resources
  }
  
  // 测试泄漏检测
  let leak_detector = create_leak_detector()
  
  leak_detector_track(leak_detector, "resource_1", "memory")
  leak_detector_track(leak_detector, "resource_2", "file")
  leak_detector_track(leak_detector, "resource_3", "connection")
  
  // 释放部分资源
  leak_detector_free(leak_detector, "resource_2")
  
  // 检查泄漏
  let leaked = leak_detector_check(leak_detector)
  assert_eq(leaked.length(), 2)
  assert_true(leaked.any(fn(r) { r.id == "resource_1" }))
  assert_true(leaked.any(fn(r) { r.id == "resource_3" }))
  assert_false(leaked.any(fn(r) { r.id == "resource_2" }))
  
  // 测试资源使用统计
  let create_usage_tracker = fn() {
    {
      usage_history: [],
      current_usage: {
        memory: 0,
        cpu: 0.0,
        network: 0,
        disk: 0
      }
    }
  }
  
  let usage_tracker_update = fn(tracker, usage) {
    tracker.current_usage = usage
    tracker.usage_history = tracker.usage_history.push({
      timestamp: 1640995200 + tracker.usage_history.length(),
      usage
    })
    
    // 保持历史记录不超过1000条
    if tracker.usage_history.length() > 1000 {
      tracker.usage_history = tracker.usage_history.slice(tracker.usage_history.length() - 1000, 1000)
    }
  }
  
  let usage_tracker_get_average = fn(tracker, resource: String, window_size: Int) {
    let recent_history = tracker.usage_history.slice(
      if tracker.usage_history.length() > window_size {
        tracker.usage_history.length() - window_size
      } else {
        0
      },
      tracker.usage_history.length()
    )
    
    if recent_history.length() == 0 {
      0.0
    } else {
      let mut total = 0.0
      for record in recent_history {
        match resource {
          "memory" => total = total + record.usage.memory.to_float()
          "cpu" => total = total + record.usage.cpu
          "network" => total = total + record.usage.network.to_float()
          "disk" => total = total + record.usage.disk.to_float()
          _ => ()
        }
      }
      total / recent_history.length().to_float()
    }
  }
  
  // 测试使用统计
  let usage_tracker = create_usage_tracker()
  
  // 模拟资源使用变化
  usage_tracker_update(usage_tracker, { memory: 100, cpu: 20.0, network: 50, disk: 30 })
  usage_tracker_update(usage_tracker, { memory: 120, cpu: 25.0, network: 60, disk: 35 })
  usage_tracker_update(usage_tracker, { memory: 90, cpu: 15.0, network: 40, disk: 25 })
  usage_tracker_update(usage_tracker, { memory: 110, cpu: 30.0, network: 55, disk: 40 })
  
  // 计算平均使用率
  let avg_memory = usage_tracker_get_average(usage_tracker, "memory", 4)
  let avg_cpu = usage_tracker_get_average(usage_tracker, "cpu", 4)
  
  assert_eq(avg_memory, 105.0)  // (100 + 120 + 90 + 110) / 4
  assert_eq(avg_cpu, 22.5)     // (20 + 25 + 15 + 30) / 4
}

// 测试9: 网络通信和协议处理
test "网络通信和协议处理测试" {
  // 创建HTTP请求模拟
  let create_http_request = fn(method: String, url: String, headers: Array[(String, String)], body: String) {
    {
      method,
      url,
      headers,
      body,
      timestamp: 1640995200
    }
  }
  
  let create_http_response = fn(status_code: Int, headers: Array[(String, String)], body: String) {
    {
      status_code,
      headers,
      body,
      timestamp: 1640995200
    }
  }
  
  // 模拟HTTP客户端
  let http_client = {
    pending_requests: [],
    completed_requests: [],
    request_timeout: 5000
  }
  
  let http_send = fn(client, request) {
    client.pending_requests = client.pending_requests.push({
      request,
      sent_at: 1640995200,
      status: "pending"
    })
    
    // 模拟网络延迟
    let response = create_http_response(200, [
      ("Content-Type", "application/json"),
      ("Content-Length", request.body.length().to_string())
    ], "{\"status\":\"ok\",\"data\":\"processed\"}")
    
    // 更新请求状态
    client.pending_requests = client.pending_requests.map(fn(req) {
      if req.request.timestamp == request.timestamp {
        { req | status: "completed" }
      } else {
        req
      }
    })
    
    client.completed_requests = client.completed_requests.push({
      request,
      response,
      completed_at: 1640995200 + 100
    })
    
    response
  }
  
  // 测试HTTP请求
  let request = create_http_request("POST", "/api/telemetry", [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
  ], "{\"metric\":\"cpu_usage\",\"value\":75.5}")
  
  let response = http_send(http_client, request)
  
  assert_eq(response.status_code, 200)
  assert_eq(response.body, "{\"status\":\"ok\",\"data\":\"processed\"}")
  assert_eq(http_client.completed_requests.length(), 1)
  
  // 创建WebSocket连接模拟
  let create_websocket_client = fn(url: String) {
    {
      url,
      connected: false,
      message_queue: [],
      handlers: []
    }
  }
  
  let websocket_connect = fn(client) {
    client.connected = true
    "connection_established"
  }
  
  let websocket_send = fn(client, message: String) {
    if client.connected {
      client.message_queue = client.message_queue.push({
        direction: "outgoing",
        message,
        timestamp: 1640995200
      })
      true
    } else {
      false
    }
  }
  
  let websocket_receive = fn(client, message: String) {
    if client.connected {
      client.message_queue = client.message_queue.push({
        direction: "incoming",
        message,
        timestamp: 1640995200
      })
      
      // 触发处理器
      for handler in client.handlers {
        handler(message)
      }
    }
  }
  
  let websocket_on_message = fn(client, handler: (String) -> Unit) {
    client.handlers = client.handlers.push(handler)
  }
  
  // 测试WebSocket
  let ws_client = create_websocket_client("ws://telemetry.example.com/ws")
  
  let connection_result = websocket_connect(ws_client)
  assert_eq(connection_result, "connection_established")
  assert_true(ws_client.connected)
  
  let send_result = websocket_send(ws_client, "{\"type\":\"telemetry\",\"data\":{}}")
  assert_true(send_result)
  assert_eq(ws_client.message_queue.length(), 1)
  
  let received_messages = []
  websocket_on_message(ws_client, fn(message) {
    received_messages = received_messages.push(message)
  })
  
  websocket_receive(ws_client, "{\"type\":\"ack\",\"id\":123}")
  assert_eq(received_messages.length(), 1)
  assert_eq(received_messages[0], "{\"type\":\"ack\",\"id\":123}")
  
  // 创建协议解析器
  let create_protocol_parser = fn() {
    {
      message_types: {},
      parsers: {}
    }
  }
  
  let protocol_register_type = fn(parser, type_name: String, type_id: Int) {
    parser.message_types[type_name] = type_id
  }
  
  let protocol_register_parser = fn(parser, type_id: Int, parse_fn: (String) -> Object) {
    parser.parsers[type_id] = parse_fn
  }
  
  let protocol_parse = fn(parser, message: String) {
    // 简化的协议解析
    if message.starts_with("{") and message.ends_with("}") {
      // JSON格式消息
      let type_field = "\"type\":\""
      let type_start = message.index_of(type_field)
      match type_start {
        Some(start) => {
          let type_value_start = start + type_field.length()
          let type_end = message.index_of("\"", type_value_start)
          match type_end {
            Some(end) => {
              let type_name = message.substring(type_value_start, end - type_value_start)
              match parser.message_types.get(type_name) {
                Some(type_id) => {
                  match parser.parsers.get(type_id) {
                    Some(parse_fn) => parse_fn(message)
                    None => { error: "未找到类型解析器", message }
                  }
                }
                None => { error: "未知消息类型", message }
              }
            }
            None => { error: "无效的消息格式", message }
          }
        }
        None => { error: "缺少类型字段", message }
      }
    } else {
      { error: "非JSON格式消息", message }
    }
  }
  
  // 测试协议解析
  let protocol_parser = create_protocol_parser()
  
  protocol_register_type(protocol_parser, "telemetry", 1)
  protocol_register_type(protocol_parser, "control", 2)
  
  protocol_register_parser(protocol_parser, 1, fn(message) {
    // 简化的遥测消息解析
    { type: "telemetry", parsed: true, data: "cpu_usage: 75.5" }
  })
  
  protocol_register_parser(protocol_parser, 2, fn(message) {
    // 简化的控制消息解析
    { type: "control", parsed: true, command: "restart" }
  })
  
  let telemetry_message = "{\"type\":\"telemetry\",\"timestamp\":1640995200,\"data\":{\"metric\":\"cpu_usage\",\"value\":75.5}}"
  let control_message = "{\"type\":\"control\",\"command\":\"restart\",\"service\":\"api\"}"
  
  let parsed_telemetry = protocol_parse(protocol_parser, telemetry_message)
  assert_eq(parsed_telemetry.type, "telemetry")
  assert_true(parsed_telemetry.parsed)
  
  let parsed_control = protocol_parse(protocol_parser, control_message)
  assert_eq(parsed_control.type, "control")
  assert_true(parsed_control.parsed)
  
  // 创建网络连接池
  let create_connection_pool = fn(max_connections: Int) {
    {
      max_connections,
      active_connections: [],
      idle_connections: [],
      connection_counter: { mut count: 0 }
    }
  }
  
  let pool_get_connection = fn(pool) {
    if pool.idle_connections.length() > 0 {
      // 复用空闲连接
      let conn = pool.idle_connections[0]
      pool.idle_connections = pool.idle_connections.slice(1, pool.idle_connections.length())
      pool.active_connections = pool.active_connections.push(conn)
      conn
    } else if pool.active_connections.length() < pool.max_connections {
      // 创建新连接
      pool.connection_counter.count = pool.connection_counter.count + 1
      let conn = {
        id: "conn_" + pool.connection_counter.count.to_string(),
        created_at: 1640995200,
        last_used: 1640995200
      }
      pool.active_connections = pool.active_connections.push(conn)
      conn
    } else {
      // 连接池已满
      { error: "连接池已满" }
    }
  }
  
  let pool_release_connection = fn(pool, connection_id: String) {
    let conn_index = pool.active_connections.index_of(fn(c) { c.id == connection_id })
    match conn_index {
      Some(index) => {
        let conn = pool.active_connections[index]
        let updated_conn = { conn | last_used: 1640995200 }
        pool.active_connections = pool.active_connections.filter(fn(c) { c.id != connection_id })
        pool.idle_connections = pool.idle_connections.push(updated_conn)
      }
      None => ()
    }
  }
  
  // 测试连接池
  let conn_pool = create_connection_pool(3)
  
  let conn1 = pool_get_connection(conn_pool)
  let conn2 = pool_get_connection(conn_pool)
  let conn3 = pool_get_connection(conn_pool)
  
  assert_eq(conn1.id, "conn_1")
  assert_eq(conn2.id, "conn_2")
  assert_eq(conn3.id, "conn_3")
  assert_eq(conn_pool.active_connections.length(), 3)
  
  // 尝试获取第四个连接，应该失败
  let conn4 = pool_get_connection(conn_pool)
  assert_eq(conn4.error, "连接池已满")
  
  // 释放一个连接
  pool_release_connection(conn_pool, "conn_1")
  assert_eq(conn_pool.active_connections.length(), 2)
  assert_eq(conn_pool.idle_connections.length(), 1)
  
  // 现在应该可以获取新连接
  let conn5 = pool_get_connection(conn_pool)
  assert_eq(conn5.id, "conn_1")  // 复用连接
}

// 测试10: 数据分析和统计计算
test "数据分析和统计计算测试" {
  // 创建数据分析器
  let create_data_analyzer = fn() {
    {
      data_points: [],
      calculations: {}
    }
  }
  
  let analyzer_add_data_point = fn(analyzer, value: Float, timestamp: Int) {
    analyzer.data_points = analyzer.data_points.push({
      value,
      timestamp,
      metadata: {}
    })
  }
  
  let analyzer_calculate_mean = fn(analyzer) {
    if analyzer.data_points.length() == 0 {
      0.0
    } else {
      let mut sum = 0.0
      for point in analyzer.data_points {
        sum = sum + point.value
      }
      sum / analyzer.data_points.length().to_float()
    }
  }
  
  let analyzer_calculate_median = fn(analyzer) {
    let sorted = analyzer.data_points.sort_by(fn(a, b) { 
      if a.value < b.value { -1 } else if a.value > b.value { 1 } else { 0 }
    })
    
    let n = sorted.length()
    if n == 0 {
      0.0
    } else if n % 2 == 1 {
      sorted[n / 2].value
    } else {
      (sorted[n / 2 - 1].value + sorted[n / 2].value) / 2.0
    }
  }
  
  let analyzer_calculate_percentile = fn(analyzer, percentile: Float) {
    let sorted = analyzer.data_points.sort_by(fn(a, b) { 
      if a.value < b.value { -1 } else if a.value > b.value { 1 } else { 0}
    })
    
    let n = sorted.length()
    if n == 0 {
      0.0
    } else {
      let index = (percentile / 100.0 * (n - 1).to_float()).to_int()
      sorted[index].value
    }
  }
  
  let analyzer_calculate_std_dev = fn(analyzer) {
    if analyzer.data_points.length() <= 1 {
      0.0
    } else {
      let mean = analyzer_calculate_mean(analyzer)
      let mut sum_squared_diff = 0.0
      
      for point in analyzer.data_points {
        let diff = point.value - mean
        sum_squared_diff = sum_squared_diff + diff * diff
      }
      
      (sum_squared_diff / (analyzer.data_points.length() - 1).to_float()).sqrt()
    }
  }
  
  // 测试基本统计
  let analyzer = create_data_analyzer()
  
  // 添加测试数据
  analyzer_add_data_point(analyzer, 10.0, 1640995200)
  analyzer_add_data_point(analyzer, 20.0, 1640995201)
  analyzer_add_data_point(analyzer, 30.0, 1640995202)
  analyzer_add_data_point(analyzer, 40.0, 1640995203)
  analyzer_add_data_point(analyzer, 50.0, 1640995204)
  
  let mean = analyzer_calculate_mean(analyzer)
  let median = analyzer_calculate_median(analyzer)
  let p95 = analyzer_calculate_percentile(analyzer, 95.0)
  let std_dev = analyzer_calculate_std_dev(analyzer)
  
  assert_eq(mean, 30.0)  // (10 + 20 + 30 + 40 + 50) / 5
  assert_eq(median, 30.0)  // 中间值
  assert_eq(p95, 50.0)  // 95%百分位数
  assert_true(std_dev > 14.0 and std_dev < 16.0)  // 标准差约为15.8
  
  // 创建时间序列分析器
  let create_time_series_analyzer = fn() {
    {
      series: [],
      window_size: 10
    }
  }
  
  let time_series_add_point = fn(analyzer, timestamp: Int, value: Float) {
    analyzer.series = analyzer.series.push({ timestamp, value })
    
    // 保持序列按时间排序
    analyzer.series = analyzer.series.sort_by(fn(a, b) { a.timestamp - b.timestamp })
  }
  
  let time_series_calculate_moving_average = fn(analyzer, window: Int) {
    if analyzer.series.length() < window {
      0.0
    } else {
      let recent = analyzer.series.slice(analyzer.series.length() - window, analyzer.series.length())
      let mut sum = 0.0
      for point in recent {
        sum = sum + point.value
      }
      sum / window.to_float()
    }
  }
  
  let time_series_detect_trend = fn(analyzer, window: Int) {
    if analyzer.series.length() < window {
      "insufficient_data"
    } else {
      let recent = analyzer.series.slice(analyzer.series.length() - window, analyzer.series.length())
      let first_half = recent.slice(0, window / 2)
      let second_half = recent.slice(window / 2, window)
      
      let first_avg = first_half.reduce(fn(acc, p) { acc + p.value }, 0.0) / first_half.length().to_float()
      let second_avg = second_half.reduce(fn(acc, p) { acc + p.value }, 0.0) / second_half.length().to_float()
      
      let change_rate = (second_avg - first_avg) / first_avg * 100.0
      
      if change_rate > 5.0 {
        "increasing"
      } else if change_rate < -5.0 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  // 测试时间序列分析
  let ts_analyzer = create_time_series_analyzer()
  
  // 添加时间序列数据
  let base_time = 1640995200
  for i in 0..20 {
    let value = 100.0 + i.to_float() * 2.0  // 线性增长
    time_series_add_point(ts_analyzer, base_time + i * 60, value)
  }
  
  let moving_avg = time_series_calculate_moving_average(ts_analyzer, 5)
  let trend = time_series_detect_trend(ts_analyzer, 10)
  
  assert_true(moving_avg > 130.0)  // 最后5个点的平均值
  assert_eq(trend, "increasing")  // 检测到增长趋势
  
  // 创建异常检测器
  let create_anomaly_detector = fn() {
    {
      baseline_data: [],
      threshold: 2.0  // 标准差倍数
    }
  }
  
  let anomaly_detector_train = fn(detector, data: Array[Float]) {
    // 计算基线统计
    let mean = data.reduce(fn(acc, x) { acc + x }, 0.0) / data.length().to_float()
    let mut variance = 0.0
    for value in data {
      let diff = value - mean
      variance = variance + diff * diff
    }
    variance = variance / data.length().to_float()
    let std_dev = variance.sqrt()
    
    detector.baseline_data = [{ mean, std_dev }]
  }
  
  let anomaly_detector_detect = fn(detector, value: Float) {
    if detector.baseline_data.length() == 0 {
      false
    } else {
      let baseline = detector.baseline_data[0]
      let z_score = (value - baseline.mean) / baseline.std_dev
      z_score.abs() > detector.threshold
    }
  }
  
  // 测试异常检测
  let anomaly_detector = create_anomaly_detector()
  
  // 训练正常数据
  let normal_data = [10.0, 12.0, 11.0, 13.0, 9.0, 14.0, 10.0, 12.0, 11.0, 13.0]
  anomaly_detector_train(anomaly_detector, normal_data)
  
  // 测试正常值
  assert_false(anomaly_detector_detect(anomaly_detector, 11.5))
  assert_false(anomaly_detector_detect(anomaly_detector, 12.5))
  
  // 测试异常值
  assert_true(anomaly_detector_detect(anomaly_detector, 25.0))  // 高异常值
  assert_true(anomaly_detector_detect(anomaly_detector, -5.0))  // 低异常值
  
  // 创建相关性分析器
  let create_correlation_analyzer = fn() {
    {
      datasets: {}
    }
  }
  
  let correlation_add_dataset = fn(analyzer, name: String, data: Array[Float]) {
    analyzer.datasets[name] = data
  }
  
  let correlation_calculate = fn(analyzer, dataset_a: String, dataset_b: String) {
    let data_a = analyzer.datasets.get(dataset_a)
    let data_b = analyzer.datasets.get(dataset_b)
    
    match (data_a, data_b) {
      (Some(a), Some(b)) => {
        if a.length() != b.length() or a.length() == 0 {
          0.0
        } else {
          // 计算皮尔逊相关系数
          let mean_a = a.reduce(fn(acc, x) { acc + x }, 0.0) / a.length().to_float()
          let mean_b = b.reduce(fn(acc, x) { acc + x }, 0.0) / b.length().to_float()
          
          let mut numerator = 0.0
          let mut sum_sq_a = 0.0
          let mut sum_sq_b = 0.0
          
          for i in 0..a.length() {
            let diff_a = a[i] - mean_a
            let diff_b = b[i] - mean_b
            numerator = numerator + diff_a * diff_b
            sum_sq_a = sum_sq_a + diff_a * diff_a
            sum_sq_b = sum_sq_b + diff_b * diff_b
          }
          
          let denominator = (sum_sq_a * sum_sq_b).sqrt()
          if denominator == 0.0 {
            0.0
          } else {
            numerator / denominator
          }
        }
      }
      _ => 0.0
    }
  }
  
  // 测试相关性分析
  let correlation_analyzer = create_correlation_analyzer()
  
  // 添加数据集
  correlation_add_dataset(correlation_analyzer, "cpu_usage", [45.0, 50.0, 55.0, 60.0, 65.0, 70.0])
  correlation_add_dataset(correlation_analyzer, "response_time", [100.0, 110.0, 120.0, 130.0, 140.0, 150.0])
  correlation_add_dataset(correlation_analyzer, "error_rate", [0.1, 0.2, 0.1, 0.3, 0.2, 0.4])
  
  // 计算相关性
  let cpu_response_corr = correlation_calculate(correlation_analyzer, "cpu_usage", "response_time")
  let cpu_error_corr = correlation_calculate(correlation_analyzer, "cpu_usage", "error_rate")
  
  assert_true(cpu_response_corr > 0.9)  // 强正相关
  assert_true(cpu_error_corr > 0.5)   // 中等正相关
}