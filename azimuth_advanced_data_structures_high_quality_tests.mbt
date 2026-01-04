// Azimuth 高级数据结构测试用例
// 专注于复杂数据结构操作和算法实现

// 测试1: 高效树结构操作
test "高效树结构操作测试" {
  // 定义树节点类型
  type TreeNode = {
    value: String,
    children: Array[TreeNode],
    metadata: Map[String, String]
  }
  
  // 创建树节点
  let create_node = fn(value: String) {
    {
      value,
      children: [],
      metadata: Map::new()
    }
  }
  
  // 添加子节点
  let add_child = fn(parent: TreeNode, child: TreeNode) {
    { parent | children: parent.children.push(child) }
  }
  
  // 构建测试树结构
  let root = create_node("root")
  let child1 = create_node("child1")
  let child2 = create_node("child2")
  let child3 = create_node("child3")
  let grandchild1 = create_node("grandchild1")
  let grandchild2 = create_node("grandchild2")
  
  // 构建树结构
  let tree_with_child1 = add_child(root, child1)
  let tree_with_child2 = add_child(tree_with_child1, child2)
  let tree_with_child3 = add_child(tree_with_child2, child3)
  
  let child1_with_grandchild1 = add_child(child1, grandchild1)
  let child2_with_grandchild2 = add_child(child2, grandchild2)
  
  // 验证树结构
  assert_eq(tree_with_child3.children.length(), 3)
  assert_eq(tree_with_child3.children[0].value, "child1")
  assert_eq(tree_with_child3.children[1].value, "child2")
  assert_eq(tree_with_child3.children[2].value, "child3")
  
  // 深度优先遍历
  let dfs_traverse = fn(node: TreeNode) {
    let mut result = []
    let mut stack = [node]
    
    while stack.length() > 0 {
      let current = stack.pop()
      result = result.push(current.value)
      
      // 反向添加子节点以保持顺序
      for i in reverse(0..current.children.length()) {
        stack = stack.push(current.children[i])
      }
    }
    
    result
  }
  
  let dfs_result = dfs_traverse(tree_with_child3)
  assert_eq(dfs_result.length(), 6)  // root + 3 children + 2 grandchildren
  assert_eq(dfs_result[0], "root")
  
  // 广度优先遍历
  let bfs_traverse = fn(node: TreeNode) {
    let mut result = []
    let mut queue = [node]
    
    while queue.length() > 0 {
      let current = queue.shift()
      result = result.push(current.value)
      
      for child in current.children {
        queue = queue.push(child)
      }
    }
    
    result
  }
  
  let bfs_result = bfs_traverse(tree_with_child3)
  assert_eq(bfs_result[0], "root")
  assert_eq(bfs_result[1], "child1")
  assert_eq(bfs_result[2], "child2")
  assert_eq(bfs_result[3], "child3")
  
  // 计算树深度
  let tree_depth = fn(node: TreeNode) {
    if node.children.length() == 0 {
      1
    } else {
      let max_child_depth = node.children
        .map(tree_depth)
        .reduce(fn(max, depth) { if depth > max { depth } else { max } }, 0)
      max_child_depth + 1
    }
  }
  
  assert_eq(tree_depth(tree_with_child3), 3)  // root -> child -> grandchild
  
  // 查找节点
  let find_node = fn(node: TreeNode, target: String) {
    let mut result = None
    let mut stack = [node]
    
    while stack.length() > 0 and result == None {
      let current = stack.pop()
      if current.value == target {
        result = Some(current)
      } else {
        for child in current.children {
          stack = stack.push(child)
        }
      }
    }
    
    result
  }
  
  let found = find_node(tree_with_child3, "child2")
  assert_true(found != None)
  
  match found {
    Some(node) => assert_eq(node.value, "child2")
    None => assert_true(false)
  }
  
  let not_found = find_node(tree_with_child3, "nonexistent")
  assert_eq(not_found, None)
}

// 测试2: 高性能图算法
test "高性能图算法测试" {
  // 定义图结构
  type Graph = {
    nodes: Map[String, Map[String, Float]],
    directed: Bool
  }
  
  // 创建图
  let create_graph = fn(directed: Bool) {
    {
      nodes: Map::new(),
      directed
    }
  }
  
  // 添加节点
  let add_node = fn(graph: Graph, node: String) {
    if not(graph.nodes.contains(node)) {
      { graph | nodes: graph.nodes.set(node, Map::new()) }
    } else {
      graph
    }
  }
  
  // 添加边
  let add_edge = fn(graph: Graph, from: String, to: String, weight: Float) {
    let graph_with_nodes = add_node(add_node(graph, from), to)
    let from_edges = graph_with_nodes.nodes.get(from)
    
    match from_edges {
      Some(edges) => {
        let updated_edges = edges.set(to, weight)
        let updated_nodes = graph_with_nodes.nodes.set(from, updated_edges)
        { graph_with_nodes | nodes: updated_nodes }
      }
      None => graph_with_nodes
    }
  }
  
  // 构建测试图
  let initial_graph = create_graph(false)  // 无向图
  let graph = add_edge(add_edge(add_edge(initial_graph, "A", "B", 1.0), "B", "C", 2.0), "C", "D", 1.5)
  let graph_with_extra = add_edge(add_edge(graph, "A", "C", 4.0), "B", "D", 3.0)
  
  // 验证图结构
  assert_true(graph_with_extra.nodes.contains("A"))
  assert_true(graph_with_extra.nodes.contains("B"))
  assert_true(graph_with_extra.nodes.contains("C"))
  assert_true(graph_with_extra.nodes.contains("D"))
  
  // Dijkstra最短路径算法
  let dijkstra = fn(graph: Graph, start: String, end: String) {
    let mut distances = Map::new()
    let mut visited = Set::new()
    let mut previous = Map::new()
    
    // 初始化距离
    for node in graph.nodes.keys() {
      distances = distances.set(node, if node == start { 0.0 } else { Float::infinity })
    }
    
    while visited.size() < graph.nodes.size() {
      // 找到未访问节点中距离最小的
      let mut current = None
      let mut min_distance = Float::infinity()
      
      for node in graph.nodes.keys() {
        if not(visited.contains(node)) {
          let distance = distances.get(node)
          match distance {
            Some(d) => {
              if d < min_distance {
                min_distance = d
                current = Some(node)
              }
            }
            None => {}
          }
        }
      }
      
      match current {
        Some(curr) => {
          visited = visited.add(curr)
          
          // 更新邻居距离
          let neighbors = graph.nodes.get(curr)
          match neighbors {
            Some(edges) => {
              for (neighbor, weight) in edges {
                if not(visited.contains(neighbor)) {
                  let alt = distances.get(curr) + weight
                  match alt {
                    Some(alt_dist) => {
                      let current_dist = distances.get(neighbor)
                      match current_dist {
                        Some(curr_dist) => {
                          if alt_dist < curr_dist {
                            distances = distances.set(neighbor, alt_dist)
                            previous = previous.set(neighbor, curr)
                          }
                        }
                        None => {}
                      }
                    }
                    None => {}
                  }
                }
              }
            }
            None => {}
          }
        }
        None => break
      }
    }
    
    // 构建路径
    let mut path = []
    let mut current = end
    
    while previous.contains(current) {
      path = path.push(current)
      match previous.get(current) {
        Some(prev) => current = prev
        None => break
      }
    }
    
    if current == start {
      path = path.push(start)
      path.reverse()
      Some(path)
    } else {
      None
    }
  }
  
  // 测试最短路径
  let shortest_path = dijkstra(graph_with_extra, "A", "D")
  assert_true(shortest_path != None)
  
  match shortest_path {
    Some(path) => {
      assert_eq(path.length(), 3)  // A -> B -> C -> D or A -> C -> D
      assert_eq(path[0], "A")
      assert_eq(path[path.length() - 1], "D")
    }
    None => assert_true(false)
  }
  
  // 检测环
  let has_cycle = fn(graph: Graph) {
    let mut visited = Set::new()
    let mut recursion_stack = Set::new()
    
    let detect_cycle_util = fn(node: String) {
      if recursion_stack.contains(node) {
        true
      } else if visited.contains(node) {
        false
      } else {
        visited = visited.add(node)
        recursion_stack = recursion_stack.add(node)
        
        let neighbors = graph.nodes.get(node)
        match neighbors {
          Some(edges) => {
            for neighbor in edges.keys() {
              if detect_cycle_util(neighbor) {
                return true
              }
            }
          }
          None => {}
        }
        
        recursion_stack = recursion_stack.remove(node)
        false
      }
    }
    
    for node in graph.nodes.keys() {
      if not(visited.contains(node)) {
        if detect_cycle_util(node) {
          return true
        }
      }
    }
    
    false
  }
  
  // 创建有环图测试
  let cyclic_graph = add_edge(add_edge(add_edge(initial_graph, "X", "Y", 1.0), "Y", "Z", 1.0), "Z", "X", 1.0)
  assert_true(has_cycle(cyclic_graph))
  assert_false(has_cycle(graph_with_extra))
}

// 测试3: 高级优先队列实现
test "高级优先队列实现测试" {
  // 定义优先队列项
  type PriorityQueueItem[T] = {
    value: T,
    priority: Int,
    insertion_order: Int
  }
  
  // 定义优先队列
  type PriorityQueue[T] = {
    items: Array[PriorityQueueItem[T]],
    counter: Int
  }
  
  // 创建优先队列
  let create_priority_queue = fn() {
    {
      items: [],
      counter: 0
    }
  }
  
  // 添加元素
  let enqueue = fn(queue: PriorityQueue[T], value: T, priority: Int) {
    let item = {
      value,
      priority,
      insertion_order: queue.counter
    }
    
    let mut new_items = queue.items
    let mut inserted = false
    
    // 按优先级插入，相同优先级按插入顺序
    for i in 0..new_items.length() {
      if priority > new_items[i].priority or 
         (priority == new_items[i].priority and queue.counter < new_items[i].insertion_order) {
        new_items = new_items.insert(i, item)
        inserted = true
        break
      }
    }
    
    if not(inserted) {
      new_items = new_items.push(item)
    }
    
    {
      items: new_items,
      counter: queue.counter + 1
    }
  }
  
  // 移除元素
  let dequeue = fn(queue: PriorityQueue[T]) {
    if queue.items.length() == 0 {
      (queue, None)
    } else {
      let first = queue.items[0]
      let remaining_items = queue.items.slice(1, queue.items.length())
      (
        {
          items: remaining_items,
          counter: queue.counter
        },
        Some(first.value)
      )
    }
  }
  
  // 查看元素
  let peek = fn(queue: PriorityQueue[T]) {
    if queue.items.length() == 0 {
      None
    } else {
      Some(queue.items[0].value)
    }
  }
  
  // 测试优先队列操作
  let mut queue = create_priority_queue()
  
  // 添加不同优先级的元素
  queue = enqueue(queue, "low priority task", 1)
  queue = enqueue(queue, "high priority task", 3)
  queue = enqueue(queue, "medium priority task", 2)
  queue = enqueue(queue, "another high priority task", 3)
  
  // 验证队列顺序
  assert_eq(peek(queue), Some("high priority task"))
  
  // 出队测试
  let (queue1, item1) = dequeue(queue)
  assert_eq(item1, Some("high priority task"))
  assert_eq(peek(queue1), Some("another high priority task"))
  
  let (queue2, item2) = dequeue(queue1)
  assert_eq(item2, Some("another high priority task"))
  assert_eq(peek(queue2), Some("medium priority task"))
  
  let (queue3, item3) = dequeue(queue2)
  assert_eq(item3, Some("medium priority task"))
  assert_eq(peek(queue3), Some("low priority task"))
  
  let (queue4, item4) = dequeue(queue3)
  assert_eq(item4, Some("low priority task"))
  assert_eq(peek(queue4), None)
  
  // 测试复杂数据类型
  type Task = {
    id: Int,
    name: String,
    estimated_time: Float
  }
  
  let task_queue = create_priority_queue()
  let task1 = { id: 1, name: "Database backup", estimated_time: 30.0 }
  let task2 = { id: 2, name: "Security scan", estimated_time: 15.0 }
  let task3 = { id: 3, name: "Log cleanup", estimated_time: 5.0 }
  
  let task_queue1 = enqueue(enqueue(enqueue(task_queue, task1, 2), task2, 3), task3, 1)
  
  let (task_queue2, next_task) = dequeue(task_queue1)
  match next_task {
    Some(task) => assert_eq(task.id, 2)  // Security scan has highest priority
    None => assert_true(false)
  }
}

// 测试4: 智能缓存系统
test "智能缓存系统测试" {
  // 定义缓存项
  type CacheItem[T] = {
    value: T,
    access_count: Int,
    last_accessed: Int,
    created_at: Int,
    size: Int
  }
  
  // 定义缓存配置
  type CacheConfig = {
    max_size: Int,
    max_memory: Int,
    ttl_seconds: Int,
    eviction_policy: String  // "LRU", "LFU", "FIFO"
  }
  
  // 定义缓存
  type Cache[T] = {
    items: Map[String, CacheItem[T]],
    config: CacheConfig,
    current_memory: Int,
    access_order: Array[String]
  }
  
  // 创建缓存
  let create_cache = fn(config: CacheConfig) {
    {
      items: Map::new(),
      config,
      current_memory: 0,
      access_order: []
    }
  }
  
  // 计算对象大小（简化版）
  let calculate_size = fn(value: T) {
    // 简化实现，实际应用中需要更复杂的计算
    match value {
      String(s) => s.length(),
      Int(_) => 8,
      Float(_) => 8,
      _ => 16  // 默认大小
    }
  }
  
  // 获取当前时间戳
  let current_time = fn() { 1640995200 }  // 固定时间戳用于测试
  
  // 添加缓存项
  let put = fn(cache: Cache[T], key: String, value: T) {
    let now = current_time()
    let size = calculate_size(value)
    let item = {
      value,
      access_count: 1,
      last_accessed: now,
      created_at: now,
      size
    }
    
    // 检查是否需要驱逐
    let needs_eviction = cache.current_memory + size > cache.config.max_memory or
                       cache.items.size() >= cache.config.max_size
    
    let updated_cache = if needs_eviction {
      // 简化的LRU驱逐策略
      if cache.access_order.length() > 0 {
        let oldest_key = cache.access_order[0]
        let removed_item = cache.items.get(oldest_key)
        
        match removed_item {
          Some(removed) => {
            let new_items = cache.items.remove(oldest_key)
            let new_access_order = cache.access_order.filter(fn(k) { k != oldest_key })
            {
              items: new_items,
              config: cache.config,
              current_memory: cache.current_memory - removed.size,
              access_order: new_access_order
            }
          }
          None => cache
        }
      } else {
        cache
      }
    } else {
      cache
    }
    
    // 更新访问顺序
    let new_access_order = updated_cache.access_order.filter(fn(k) { k != key }).push(key)
    
    // 添加新项
    let new_items = updated_cache.items.set(key, item)
    let new_memory = updated_cache.current_memory + size
    
    {
      items: new_items,
      config: updated_cache.config,
      current_memory: new_memory,
      access_order: new_access_order
    }
  }
  
  // 获取缓存项
  let get = fn(cache: Cache[T], key: String) {
    let item = cache.items.get(key)
    match item {
      Some(cache_item) => {
        let now = current_time()
        
        // 检查TTL
        if now - cache_item.created_at > cache.config.ttl_seconds {
          (cache, None)  // 已过期
        } else {
          // 更新访问信息
          let updated_item = {
            value: cache_item.value,
            access_count: cache_item.access_count + 1,
            last_accessed: now,
            created_at: cache_item.created_at,
            size: cache_item.size
          }
          
          let new_items = cache.items.set(key, updated_item)
          let new_access_order = cache.access_order.filter(fn(k) { k != key }).push(key)
          
          let updated_cache = {
            items: new_items,
            config: cache.config,
            current_memory: cache.current_memory,
            access_order: new_access_order
          }
          
          (updated_cache, Some(cache_item.value))
        }
      }
      None => (cache, None)
    }
  }
  
  // 创建缓存配置
  let cache_config = {
    max_size: 3,
    max_memory: 100,
    ttl_seconds: 3600,
    eviction_policy: "LRU"
  }
  
  // 创建缓存
  let mut cache = create_cache(cache_config)
  
  // 添加缓存项
  cache = put(cache, "key1", "value1")
  cache = put(cache, "key2", "value2")
  cache = put(cache, "key3", "value3")
  
  // 验证缓存状态
  assert_eq(cache.items.size(), 3)
  assert_eq(cache.current_memory, 18)  // 3个字符串的长度之和
  
  // 获取缓存项
  let (cache1, value1) = get(cache, "key2")
  assert_eq(value1, Some("value2"))
  
  // 验证访问计数增加
  let key2_item = cache1.items.get("key2")
  match key2_item {
    Some(item) => assert_eq(item.access_count, 2)
    None => assert_true(false)
  }
  
  // 添加第4个项，应该驱逐最旧的
  let cache2 = put(cache1, "key4", "value4")
  assert_eq(cache2.items.size(), 3)  // 仍然是3个项
  assert_true(cache2.items.contains("key4"))
  assert_false(cache2.items.contains("key1"))  // key1应该被驱逐
  
  // 测试缓存统计
  let get_cache_stats = fn(cache: Cache[T]) {
    let mut total_access_count = 0
    let mut oldest_item = None
    let mut newest_item = None
    
    for (key, item) in cache.items {
      total_access_count = total_access_count + item.access_count
      
      match oldest_item {
        None => oldest_item = Some((key, item.created_at))
        Some((_, oldest_time)) => {
          if item.created_at < oldest_time {
            oldest_item = Some((key, item.created_at))
          }
        }
      }
      
      match newest_item {
        None => newest_item = Some((key, item.created_at))
        Some((_, newest_time)) => {
          if item.created_at > newest_time {
            newest_item = Some((key, item.created_at))
          }
        }
      }
    }
    
    {
      total_items: cache.items.size(),
      total_memory: cache.current_memory,
      total_access_count,
      oldest_key: match oldest_item {
        Some((key, _)) => Some(key)
        None => None
      },
      newest_key: match newest_item {
        Some((key, _)) => Some(key)
        None => None
      }
    }
  }
  
  let stats = get_cache_stats(cache2)
  assert_eq(stats.total_items, 3)
  assert_eq(stats.total_memory, 18)
  assert_eq(stats.oldest_key, Some("key2"))
  assert_eq(stats.newest_key, Some("key4"))
}

// 测试5: 高效字符串匹配算法
test "高效字符串匹配算法测试" {
  // KMP算法实现
  let compute_lps_array = fn(pattern: String) {
    let chars = pattern.to_char_array()
    let mut lps = []
    let mut length = 0
    let mut i = 1
    
    // 初始化lps数组
    for _ in 0..chars.length() {
      lps = lps.push(0)
    }
    
    while i < chars.length() {
      if chars[i] == chars[length] {
        length = length + 1
        lps = lps.set(i, length)
        i = i + 1
      } else {
        if length != 0 {
          length = lps[length - 1]
        } else {
          lps = lps.set(i, 0)
          i = i + 1
        }
      }
    }
    
    lps
  }
  
  let kmp_search = fn(text: String, pattern: String) {
    let text_chars = text.to_char_array()
    let pattern_chars = pattern.to_char_array()
    let lps = compute_lps_array(pattern)
    
    let mut i = 0  // text的索引
    let mut j = 0  // pattern的索引
    let mut matches = []
    
    while i < text_chars.length() {
      if pattern_chars[j] == text_chars[i] {
        i = i + 1
        j = j + 1
        
        if j == pattern_chars.length() {
          matches = matches.push(i - j)
          j = lps[j - 1]
        }
      } else {
        if j != 0 {
          j = lps[j - 1]
        } else {
          i = i + 1
        }
      }
    }
    
    matches
  }
  
  // 测试KMP算法
  let text = "ababcabcabababd"
  let pattern = "ababd"
  
  let matches = kmp_search(text, pattern)
  assert_eq(matches.length(), 1)
  assert_eq(matches[0], 10)
  
  // 测试多个匹配
  let text2 = "testtesttest"
  let pattern2 = "test"
  
  let matches2 = kmp_search(text2, pattern2)
  assert_eq(matches2.length(), 3)
  assert_eq(matches2[0], 0)
  assert_eq(matches2[1], 4)
  assert_eq(matches2[2], 8)
  
  // 测试无匹配
  let text3 = "abcdefg"
  let pattern3 = "xyz"
  
  let matches3 = kmp_search(text3, pattern3)
  assert_eq(matches3.length(), 0)
  
  // Rabin-Karp算法实现
  let rabin_karp_search = fn(text: String, pattern: String) {
    let text_chars = text.to_char_array()
    let pattern_chars = pattern.to_char_array()
    let prime = 101  // 质数
    let d = 256      // 字符集大小
    
    let n = text_chars.length()
    let m = pattern_chars.length()
    let mut matches = []
    
    if m > n {
      return matches
    }
    
    // 计算h = d^(m-1) % prime
    let mut h = 1
    for _ in 0..m-1 {
      h = (h * d) % prime
    }
    
    // 计算模式和文本第一个窗口的哈希值
    let mut pattern_hash = 0
    let mut text_hash = 0
    
    for i in 0..m {
      pattern_hash = (d * pattern_hash + pattern_chars[i].to_int()) % prime
      text_hash = (d * text_hash + text_chars[i].to_int()) % prime
    }
    
    // 滑动窗口
    for i in 0..=(n - m) {
      if pattern_hash == text_hash {
        // 检查字符是否匹配
        let mut match_found = true
        for j in 0..m {
          if text_chars[i + j] != pattern_chars[j] {
            match_found = false
            break
          }
        }
        
        if match_found {
          matches = matches.push(i)
        }
      }
      
      if i < n - m {
        text_hash = (d * (text_hash - text_chars[i].to_int() * h) + text_chars[i + m].to_int()) % prime
        
        if text_hash < 0 {
          text_hash = text_hash + prime
        }
      }
    }
    
    matches
  }
  
  // 测试Rabin-Karp算法
  let rk_matches = rabin_karp_search(text, pattern)
  assert_eq(rk_matches.length(), 1)
  assert_eq(rk_matches[0], 10)
  
  let rk_matches2 = rabin_karp_search(text2, pattern2)
  assert_eq(rk_matches2.length(), 3)
  
  // 比较两种算法的结果
  assert_eq(matches, rk_matches)
  assert_eq(matches2, rk_matches2)
  
  // 测试性能敏感场景
  let long_text = "a".repeat(10000) + "b" + "a".repeat(10000)
  let pattern_in_long = "b"
  
  let long_matches_kmp = kmp_search(long_text, pattern_in_long)
  let long_matches_rk = rabin_karp_search(long_text, pattern_in_long)
  
  assert_eq(long_matches_kmp.length(), 1)
  assert_eq(long_matches_kmp[0], 10000)
  assert_eq(long_matches_rk.length(), 1)
  assert_eq(long_matches_rk[0], 10000)
}