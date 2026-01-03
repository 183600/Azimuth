// Azimuth Premium Caching Mechanism Tests
// 高级缓存机制测试用例，专注于遥测系统的缓存策略和性能优化

// 测试 1: LRU 缓存实现
test "LRU cache implementation" {
  // 定义缓存节点
  type CacheNode[K, V] = {
    key: K,
    value: V,
    prev: Option[CacheNode[K, V]],
    next: Option[CacheNode[K, V]]
  }
  
  // 定义LRU缓存
  type LRUCache[K, V] = {
    capacity: Int,
    size: Int,
    hashmap: Map[K, CacheNode[K, V]],
    head: Option[CacheNode[K, V]],
    tail: Option[CacheNode[K, V]]
  }
  
  // 创建LRU缓存
  let create_lru_cache = fn[K, V](capacity: Int) {
    // 创建虚拟头尾节点
    let head = {
      key: panic("dummy key"),
      value: panic("dummy value"),
      prev: None,
      next: None
    }
    
    let tail = {
      key: panic("dummy key"),
      value: panic("dummy value"),
      prev: None,
      next: None
    }
    
    // 连接头尾节点
    let linked_head = {
      key: panic("dummy key"),
      value: panic("dummy value"),
      prev: None,
      next: Some(tail)
    }
    
    let linked_tail = {
      key: panic("dummy key"),
      value: panic("dummy value"),
      prev: Some(linked_head),
      next: None
    }
    
    {
      capacity,
      size: 0,
      hashmap: Map::empty(),
      head: Some(linked_head),
      tail: Some(linked_tail)
    }
  }
  
  // 添加节点到头部
  let add_to_head = fn[K, V](cache: LRUCache[K, V], node: CacheNode[K, V]) {
    match cache.head {
      Some(head) => {
        let updated_node = {
          key: node.key,
          value: node.value,
          prev: None,
          next: Some(head)
        }
        
        let updated_head = {
          key: head.key,
          value: head.value,
          prev: Some(updated_node),
          next: head.next
        }
        
        {
          capacity: cache.capacity,
          size: cache.size,
          hashmap: cache.hashmap,
          head: Some(updated_node),
          tail: cache.tail
        }
      }
      None => {
        cache
      }
    }
  }
  
  // 移除节点
  let remove_node = fn[K, V](cache: LRUCache[K, V], node: CacheNode[K, V]) {
    match (node.prev, node.next) {
      (Some(prev), Some(next)) => {
        let updated_prev = {
          key: prev.key,
          value: prev.value,
          prev: prev.prev,
          next: Some(next)
        }
        
        let updated_next = {
          key: next.key,
          value: next.value,
          prev: Some(updated_prev),
          next: next.next
        }
        
        // 更新哈希表中的引用
        let updated_hashmap = Map::insert(cache.hashmap, prev.key, updated_prev)
        let final_hashmap = Map::insert(updated_hashmap, next.key, updated_next)
        
        {
          capacity: cache.capacity,
          size: cache.size,
          hashmap: final_hashmap,
          head: cache.head,
          tail: cache.tail
        }
      }
      (Some(prev), None) => {
        // 移除尾节点
        let updated_prev = {
          key: prev.key,
          value: prev.value,
          prev: prev.prev,
          next: None
        }
        
        let updated_hashmap = Map::insert(cache.hashmap, prev.key, updated_prev)
        
        {
          capacity: cache.capacity,
          size: cache.size,
          hashmap: updated_hashmap,
          head: cache.head,
          tail: Some(updated_prev)
        }
      }
      (None, Some(next)) => {
        // 移除头节点
        let updated_next = {
          key: next.key,
          value: next.value,
          prev: None,
          next: next.next
        }
        
        let updated_hashmap = Map::insert(cache.hashmap, next.key, updated_next)
        
        {
          capacity: cache.capacity,
          size: cache.size,
          hashmap: updated_hashmap,
          head: Some(updated_next),
          tail: cache.tail
        }
      }
      (None, None) => {
        // 空链表
        cache
      }
    }
  }
  
  // 移动节点到头部
  let move_to_head = fn[K, V](cache: LRUCache[K, V], node: CacheNode[K, V]) {
    let cache_without_node = remove_node(cache, node)
    add_to_head(cache_without_node, node)
  }
  
  // 移除尾部节点
  let remove_tail = fn[K, V](cache: LRUCache[K, V]) {
    match cache.tail {
      Some(tail) => {
        let removed_key = tail.key
        let updated_hashmap = Map::remove(cache.hashmap, removed_key)
        
        let updated_cache = {
          capacity: cache.capacity,
          size: cache.size - 1,
          hashmap: updated_hashmap,
          head: cache.head,
          tail: tail.prev
        }
        
        (updated_cache, removed_key)
      }
      None => {
        (cache, panic("empty cache"))
      }
    }
  }
  
  // 获取缓存值
  let get = fn[K, V](cache: LRUCache[K, V], key: K) {
    match Map::get(cache.hashmap, key) {
      Some(node) => {
        // 移动到头部
        let updated_cache = move_to_head(cache, node)
        (updated_cache, Some(node.value))
      }
      None => {
        (cache, None)
      }
    }
  }
  
  // 设置缓存值
  let put = fn[K, V](cache: LRUCache[K, V], key: K, value: V) {
    match Map::get(cache.hashmap, key) {
      Some(node) => {
        // 更新现有节点
        let updated_node = {
          key: node.key,
          value,
          prev: node.prev,
          next: node.next
        }
        
        let updated_hashmap = Map::insert(cache.hashmap, key, updated_node)
        let updated_cache = {
          capacity: cache.capacity,
          size: cache.size,
          hashmap: updated_hashmap,
          head: cache.head,
          tail: cache.tail
        }
        
        move_to_head(updated_cache, updated_node)
      }
      None => {
        // 添加新节点
        let new_node = {
          key,
          value,
          prev: None,
          next: None
        }
        
        if cache.size >= cache.capacity {
          // 缓存已满，移除尾部节点
          let (cache_without_tail, removed_key) = remove_tail(cache)
          
          // 添加新节点到头部
          let updated_hashmap = Map::insert(cache_without_tail.hashmap, key, new_node)
          let final_cache = {
            capacity: cache_without_tail.capacity,
            size: cache_without_tail.size,
            hashmap: updated_hashmap,
            head: cache_without_tail.head,
            tail: cache_without_tail.tail
          }
          
          add_to_head(final_cache, new_node)
        } else {
          // 缓存未满，直接添加
          let updated_hashmap = Map::insert(cache.hashmap, key, new_node)
          let updated_cache = {
            capacity: cache.capacity,
            size: cache.size + 1,
            hashmap: updated_hashmap,
            head: cache.head,
            tail: cache.tail
          }
          
          add_to_head(updated_cache, new_node)
        }
      }
    }
  }
  
  // 测试LRU缓存
  let cache = create_lru_cache[String, Int](3)
  
  // 添加元素
  let cache1 = put(cache, "key1", 1)
  let cache2 = put(cache1, "key2", 2)
  let cache3 = put(cache2, "key3", 3)
  
  // 获取元素
  let (cache4, value1) = get(cache3, "key1")
  assert_eq(value1, Some(1))
  
  // 添加新元素，应该移除最久未使用的元素
  let cache5 = put(cache4, "key4", 4)
  
  // key2应该被移除
  let (_, value2) = get(cache5, "key2")
  assert_eq(value2, None)
  
  // key1和key3应该还在
  let (_, value1_again) = get(cache5, "key1")
  let (_, value3) = get(cache5, "key3")
  
  assert_eq(value1_again, Some(1))
  assert_eq(value3, Some(3))
}

// 测试 2: 缓存淘汰策略
test "cache eviction policies" {
  // 定义缓存项
  type CacheItem[V] = {
    value: V,
    access_count: Int,
    last_accessed: Int,
    created_at: Int
  }
  
  // 定义缓存
  type Cache[K, V] = {
    capacity: Int,
    items: Map[K, CacheItem[V]],
    current_time: Int
  }
  
  // 创建缓存
  let create_cache = fn[K, V](capacity: Int) {
    {
      capacity,
      items: Map::empty(),
      current_time: 0
    }
  }
  
  // LRU淘汰策略
  let evict_lru = fn[K, V](cache: Cache[K, V]) {
    if cache.items.size() == 0 {
      return cache
    }
    
    // 找到最久未访问的项
    let mut oldest_key = panic("no items")
    let mut oldest_time = cache.current_time
    
    for (key, item) in cache.items {
      if item.last_accessed < oldest_time {
        oldest_time = item.last_accessed
        oldest_key = key
      }
    }
    
    let updated_items = Map::remove(cache.items, oldest_key)
    
    {
      capacity: cache.capacity,
      items: updated_items,
      current_time: cache.current_time
    }
  }
  
  // LFU淘汰策略
  let evict_lfu = fn[K, V](cache: Cache[K, V]) {
    if cache.items.size() == 0 {
      return cache
    }
    
    // 找到访问次数最少的项
    let mut least_used_key = panic("no items")
    let mut least_count = 999999
    
    for (key, item) in cache.items {
      if item.access_count < least_count {
        least_count = item.access_count
        least_used_key = key
      }
    }
    
    let updated_items = Map::remove(cache.items, least_used_key)
    
    {
      capacity: cache.capacity,
      items: updated_items,
      current_time: cache.current_time
    }
  }
  
  // TTL淘汰策略
  let evict_ttl = fn[K, V](cache: Cache[K, V], ttl_ms: Int) {
    let mut updated_items = cache.items
    let mut keys_to_remove = []
    
    for (key, item) in cache.items {
      if cache.current_time - item.last_accessed > ttl_ms {
        keys_to_remove = keys_to_remove.push(key)
      }
    }
    
    for key in keys_to_remove {
      updated_items = Map::remove(updated_items, key)
    }
    
    {
      capacity: cache.capacity,
      items: updated_items,
      current_time: cache.current_time
    }
  }
  
  // 获取缓存值
  let get_cache = fn[K, V](cache: Cache[K, V], key: K, current_time: Int) {
    match Map::get(cache.items, key) {
      Some(item) => {
        // 更新访问信息
        let updated_item = {
          value: item.value,
          access_count: item.access_count + 1,
          last_accessed: current_time,
          created_at: item.created_at
        }
        
        let updated_items = Map::insert(cache.items, key, updated_item)
        
        let updated_cache = {
          capacity: cache.capacity,
          items: updated_items,
          current_time
        }
        
        (updated_cache, Some(item.value))
      }
      None => {
        let updated_cache = {
          capacity: cache.capacity,
          items: cache.items,
          current_time
        }
        
        (updated_cache, None)
      }
    }
  }
  
  // 设置缓存值
  let put_cache = fn[K, V](cache: Cache[K, V], key: K, value: V, current_time: Int) {
    let item = {
      value,
      access_count: 1,
      last_accessed: current_time,
      created_at: current_time
    }
    
    let updated_items = Map::insert(cache.items, key, item)
    
    {
      capacity: cache.capacity,
      items: updated_items,
      current_time
    }
  }
  
  // 测试LRU策略
  let lru_cache = create_cache[String, Int>(2)
  
  let lru_cache1 = put_cache(lru_cache, "a", 1, 1000)
  let lru_cache2 = put_cache(lru_cache1, "b", 2, 2000)
  
  // 访问a
  let (lru_cache3, _) = get_cache(lru_cache2, "a", 3000)
  
  // 添加c，应该淘汰b（最久未访问）
  let lru_cache4 = put_cache(lru_cache3, "c", 3, 4000)
  
  // b应该被淘汰
  let (_, b_value) = get_cache(lru_cache4, "b", 5000)
  assert_eq(b_value, None)
  
  // a和c应该还在
  let (_, a_value) = get_cache(lru_cache4, "a", 5000)
  let (_, c_value) = get_cache(lru_cache4, "c", 5000)
  
  assert_eq(a_value, Some(1))
  assert_eq(c_value, Some(3))
  
  // 测试LFU策略
  let lfu_cache = create_cache[String, Int](2)
  
  let lfu_cache1 = put_cache(lfu_cache, "x", 10, 1000)
  let lfu_cache2 = put_cache(lfu_cache1, "y", 20, 2000)
  
  // 多次访问x
  let (lfu_cache3, _) = get_cache(lfu_cache2, "x", 3000)
  let (lfu_cache4, _) = get_cache(lfu_cache3, "x", 4000)
  
  // 添加z，应该淘汰y（访问次数最少）
  let lfu_cache5 = put_cache(lfu_cache4, "z", 30, 5000)
  
  // y应该被淘汰
  let (_, y_value) = get_cache(lfu_cache5, "y", 6000)
  assert_eq(y_value, None)
  
  // x和z应该还在
  let (_, x_value) = get_cache(lfu_cache5, "x", 6000)
  let (_, z_value) = get_cache(lfu_cache5, "z", 6000)
  
  assert_eq(x_value, Some(10))
  assert_eq(z_value, Some(30))
  
  // 测试TTL策略
  let ttl_cache = create_cache[String, Int](3)
  
  let ttl_cache1 = put_cache(ttl_cache, "p", 100, 1000)
  let ttl_cache2 = put_cache(ttl_cache1, "q", 200, 2000)
  let ttl_cache3 = put_cache(ttl_cache2, "r", 300, 3000)
  
  // 访问q，更新最后访问时间
  let (ttl_cache4, _) = get_cache(ttl_cache3, "q", 4000)
  
  // 应用TTL淘汰（TTL为1000ms，当前时间为5000）
  let ttl_cache5 = evict_ttl(ttl_cache4, 1000)
  
  // p和r应该被淘汰（最后访问时间早于4000）
  let (_, p_value) = get_cache(ttl_cache5, "p", 5000)
  let (_, r_value) = get_cache(ttl_cache5, "r", 5000)
  
  assert_eq(p_value, None)
  assert_eq(r_value, None)
  
  // q应该还在
  let (_, q_value) = get_cache(ttl_cache5, "q", 5000)
  assert_eq(q_value, Some(200))
}

// 测试 3: 分布式缓存
test "distributed cache" {
  // 定义缓存节点
  type CacheNode = {
    id: String,
    address: String,
    status: String  // "active", "inactive"
  }
  
  // 定义缓存项
  type CacheItem[V] = {
    value: V,
    node_id: String,
    version: Int,
    timestamp: Int
  }
  
  // 定义分布式缓存
  type DistributedCache[K, V] = {
    nodes: Array[CacheNode],
    local_cache: Map[K, CacheItem[V]],
    hash_ring: Array[String],  // 简化的哈希环
    replication_factor: Int
  }
  
  // 创建分布式缓存
  let create_distributed_cache = fn[K, V](nodes: Array[CacheNode>, replication_factor: Int) {
    // 简化的哈希环（实际实现应使用一致性哈希）
    let hash_ring = nodes.map(fn(node) { node.id })
    
    {
      nodes,
      local_cache: Map::empty(),
      hash_ring,
      replication_factor
    }
  }
  
  // 简化的哈希函数
  let hash_key = fn(key: String) {
    let mut hash = 0
    for c in key.chars() {
      hash = (hash * 31 + c.to_int()) % 1000  // 简化哈希
    }
    hash
  }
  
  // 查找负责键的节点
  let find_nodes_for_key = fn[K, V](cache: DistributedCache[K, V], key: String) {
    let key_hash = hash_key(key)
    let node_count = cache.hash_ring.length()
    
    // 简化的节点选择
    let primary_index = key_hash % node_count
    let mut nodes = [cache.hash_ring[primary_index]]
    
    // 添加副本节点
    for i in 1..cache.replication_factor {
      let replica_index = (primary_index + i) % node_count
      nodes = nodes.push(cache.hash_ring[replica_index])
    }
    
    nodes
  }
  
  // 获取缓存值
  let get_distributed = fn[K, V](cache: DistributedCache[K, V], key: String, current_time: Int) {
    match Map::get(cache.local_cache, key) {
      Some(item) => {
        // 检查版本是否最新（简化版）
        let nodes = find_nodes_for_key(cache, key)
        
        // 模拟从其他节点检查版本
        let is_latest = true  // 简化
        
        if is_latest {
          // 更新访问时间
          let updated_item = {
            value: item.value,
            node_id: item.node_id,
            version: item.version,
            timestamp: current_time
          }
          
          let updated_cache = {
            nodes: cache.nodes,
            local_cache: Map::insert(cache.local_cache, key, updated_item),
            hash_ring: cache.hash_ring,
            replication_factor: cache.replication_factor
          }
          
          (updated_cache, Some(item.value))
        } else {
          // 需要从其他节点更新
          (cache, None)
        }
      }
      None => {
        // 从其他节点获取
        let nodes = find_nodes_for_key(cache, key)
        
        // 模拟从主节点获取
        if nodes.length() > 0 {
          let primary_node = nodes[0]
          
          // 检查主节点是否活跃
          let node_active = cache.nodes.any(fn(node) { 
            node.id == primary_node && node.status == "active" 
          })
          
          if node_active {
            // 模拟获取值
            let value = panic("模拟获取值")  // 实际实现中从节点获取
            
            let item = {
              value,
              node_id: primary_node,
              version: 1,
              timestamp: current_time
            }
            
            let updated_cache = {
              nodes: cache.nodes,
              local_cache: Map::insert(cache.local_cache, key, item),
              hash_ring: cache.hash_ring,
              replication_factor: cache.replication_factor
            }
            
            (updated_cache, Some(value))
          } else {
            (cache, None)
          }
        } else {
          (cache, None)
        }
      }
    }
  }
  
  // 设置缓存值
  let put_distributed = fn[K, V](cache: DistributedCache[K, V], key: String, value: V, current_time: Int) {
    let nodes = find_nodes_for_key(cache, key)
    
    if nodes.length() > 0 {
      let primary_node = nodes[0]
      
      // 检查主节点是否活跃
      let node_active = cache.nodes.any(fn(node) { 
        node.id == primary_node && node.status == "active" 
      })
      
      if node_active {
        // 模拟写入主节点和副本节点
        let item = {
          value,
          node_id: primary_node,
          version: 1,
          timestamp: current_time
        }
        
        let updated_cache = {
          nodes: cache.nodes,
          local_cache: Map::insert(cache.local_cache, key, item),
          hash_ring: cache.hash_ring,
          replication_factor: cache.replication_factor
        }
        
        updated_cache
      } else {
        cache
      }
    } else {
      cache
    }
  }
  
  // 测试分布式缓存
  let nodes = [
    { id: "node1", address: "127.0.0.1:8001", status: "active" },
    { id: "node2", address: "127.0.0.1:8002", status: "active" },
    { id: "node3", address: "127.0.0.1:8003", status: "active" }
  ]
  
  let cache = create_distributed_cache[String, String>(nodes, 2)
  
  // 设置缓存值
  let cache1 = put_distributed(cache, "user:123", "user_data_123", 1000)
  
  // 获取缓存值
  let (cache2, value) = get_distributed(cache1, "user:123", 2000)
  
  assert_eq(value, Some("user_data_123"))
  
  // 测试节点选择
  let nodes_for_key1 = find_nodes_for_key(cache, "key1")
  let nodes_for_key2 = find_nodes_for_key(cache, "key2")
  
  assert_eq(nodes_for_key1.length(), 2)  // 主节点 + 1个副本
  assert_eq(nodes_for_key2.length(), 2)  // 主节点 + 1个副本
  
  // 测试节点故障处理
  let inactive_nodes = [
    { id: "node1", address: "127.0.0.1:8001", status: "inactive" },
    { id: "node2", address: "127.0.0.1:8002", status: "active" },
    { id: "node3", address: "127.0.0.1:8003", status: "active" }
  ]
  
  let cache_with_inactive = create_distributed_cache[String, String](inactive_nodes, 2)
  
  // 尝试写入到非活跃主节点
  let cache3 = put_distributed(cache_with_inactive, "key_on_inactive", "value", 3000)
  
  // 获取值，应该失败
  let (_, value_on_inactive) = get_distributed(cache3, "key_on_inactive", 4000)
  assert_eq(value_on_inactive, None)
}

// 测试 4: 缓存预热和刷新
test "cache warmup and refresh" {
  // 定义缓存项
  type CacheItem[V] = {
    value: V,
    last_refreshed: Int,
    refresh_interval: Int,
    source: String  // "warmup", "refresh", "user"
  }
  
  // 定义缓存
  type Cache[K, V] = {
    items: Map[K, CacheItem[V]],
    current_time: Int,
    warmup_keys: Array[K]
  }
  
  // 创建缓存
  let create_cache = fn[K, V]() {
    {
      items: Map::empty(),
      current_time: 0,
      warmup_keys: []
    }
  }
  
  // 添加预热键
  let add_warmup_key = fn[K, V](cache: Cache[K, V], key: K) {
    {
      items: cache.items,
      current_time: cache.current_time,
      warmup_keys: cache.warmup_keys.push(key)
    }
  }
  
  // 执行预热
  let warmup = fn[K, V](cache: Cache[K, V], data_loader: K -> Option[V], refresh_interval: Int) {
    let mut updated_items = cache.items
    
    for key in cache.warmup_keys {
      match data_loader(key) {
        Some(value) => {
          let item = {
            value,
            last_refreshed: cache.current_time,
            refresh_interval,
            source: "warmup"
          }
          
          updated_items = Map::insert(updated_items, key, item)
        }
        None => {
          // 预热失败
        }
      }
    }
    
    {
      items: updated_items,
      current_time: cache.current_time,
      warmup_keys: []
    }
  }
  
  // 获取缓存值
  let get = fn[K, V](cache: Cache[K, V], key: K, data_loader: K -> Option[V]) {
    match Map::get(cache.items, key) {
      Some(item) => {
        // 检查是否需要刷新
        if cache.current_time - item.last_refreshed >= item.refresh_interval {
          // 需要刷新
          match data_loader(key) {
            Some(new_value) => {
              let updated_item = {
                value: new_value,
                last_refreshed: cache.current_time,
                refresh_interval: item.refresh_interval,
                source: "refresh"
              }
              
              let updated_items = Map::insert(cache.items, key, updated_item)
              
              let updated_cache = {
                items: updated_items,
                current_time: cache.current_time,
                warmup_keys: cache.warmup_keys
              }
              
              (updated_cache, Some(new_value))
            }
            None => {
              // 刷新失败，返回旧值
              (cache, Some(item.value))
            }
          }
        } else {
          // 不需要刷新
          (cache, Some(item.value))
        }
      }
      None => {
        // 缓存中没有，尝试加载
        match data_loader(key) {
        Some(value) => {
          let item = {
            value,
            last_refreshed: cache.current_time,
            refresh_interval: 5000,  // 默认5秒刷新间隔
            source: "user"
          }
          
          let updated_items = Map::insert(cache.items, key, item)
          
          let updated_cache = {
            items: updated_items,
            current_time: cache.current_time,
            warmup_keys: cache.warmup_keys
          }
          
          (updated_cache, Some(value))
        }
        None => {
          (cache, None)
        }
      }
    }
  }
  
  // 批量刷新
  let refresh_all = fn[K, V](cache: Cache[K, V], data_loader: K -> Option[V]) {
    let mut updated_items = cache.items
    
    for (key, item) in cache.items {
      if cache.current_time - item.last_refreshed >= item.refresh_interval {
        match data_loader(key) {
          Some(new_value) => {
            let updated_item = {
              value: new_value,
              last_refreshed: cache.current_time,
              refresh_interval: item.refresh_interval,
              source: "refresh"
            }
            
            updated_items = Map::insert(updated_items, key, updated_item)
          }
          None => {
            // 刷新失败
          }
        }
      }
    }
    
    {
      items: updated_items,
      current_time: cache.current_time,
      warmup_keys: cache.warmup_keys
    }
  }
  
  // 测试缓存预热和刷新
  let cache = create_cache[String, String>()
  
  // 添加预热键
  let cache1 = add_warmup_key(cache, "user:1")
  let cache2 = add_warmup_key(cache1, "user:2")
  let cache3 = add_warmup_key(cache2, "config:app")
  
  // 模拟数据加载器
  let data_loader = fn(key: String) {
    match key {
      "user:1" => Some("user_data_1")
      "user:2" => Some("user_data_2")
      "config:app" => Some("config_data_app")
      "user:3" => Some("user_data_3")
      _ => None
    }
  }
  
  // 执行预热
  let cache4 = warmup(cache3, data_loader, 10000)
  
  // 验证预热结果
  let (_, user1_value) = get(cache4, "user:1", data_loader)
  let (_, user2_value) = get(cache4, "user:2", data_loader)
  let (_, config_value) = get(cache4, "config:app", data_loader)
  
  assert_eq(user1_value, Some("user_data_1"))
  assert_eq(user2_value, Some("user_data_2"))
  assert_eq(config_value, Some("config_data_app"))
  
  // 获取未预热的键
  let (cache5, user3_value) = get(cache4, "user:3", data_loader)
  assert_eq(user3_value, Some("user_data_3"))
  
  // 推进时间，超过刷新间隔
  let cache6 = {
    items: cache5.items,
    current_time: 15000,  // 15秒
    warmup_keys: cache5.warmup_keys
  }
  
  // 获取值，应该触发刷新
  let (cache7, refreshed_user1) = get(cache6, "user:1", data_loader)
  assert_eq(refreshed_user1, Some("user_data_1"))
  
  // 验证刷新标记
  match Map::get(cache7.items, "user:1") {
    Some(item) => assert_eq(item.source, "refresh")
    None => assert_true(false)
  }
  
  // 批量刷新
  let cache8 = {
    items: cache7.items,
    current_time: 25000,  // 25秒
    warmup_keys: cache7.warmup_keys
  }
  
  let cache9 = refresh_all(cache8, data_loader)
  
  // 验证所有项都被刷新
  for (_, item) in cache9.items {
    assert_eq(item.last_refreshed, 25000)
    assert_eq(item.source, "refresh")
  }
}

// 测试 5: 缓存性能监控
test "cache performance monitoring" {
  // 定义缓存统计
  type CacheStats = {
    hits: Int,
    misses: Int,
    sets: Int,
    evictions: Int,
    total_requests: Int,
    hit_rate: Float,
    avg_load_time: Float
  }
  
  // 定义缓存
  type Cache[K, V] = {
    items: Map[K, V],
    capacity: Int,
    stats: CacheStats
  }
  
  // 创建缓存
  let create_cache = fn[K, V](capacity: Int) {
    {
      items: Map::empty(),
      capacity,
      stats: {
        hits: 0,
        misses: 0,
        sets: 0,
        evictions: 0,
        total_requests: 0,
        hit_rate: 0.0,
        avg_load_time: 0.0
      }
    }
  }
  
  // 更新统计
  let update_stats = fn(stats: CacheStats) {
    let total_requests = stats.hits + stats.misses
    let hit_rate = if total_requests > 0 {
      (stats.hits as Float) / (total_requests as Float)
    } else {
      0.0
    }
    
    {
      hits: stats.hits,
      misses: stats.misses,
      sets: stats.sets,
      evictions: stats.evictions,
      total_requests,
      hit_rate,
      avg_load_time: stats.avg_load_time
    }
  }
  
  // 获取缓存值
  let get = fn[K, V](cache: Cache[K, V], key: K, load_time: Float) {
    let updated_stats = {
      hits: cache.stats.hits + 1,
      misses: cache.stats.misses,
      sets: cache.stats.sets,
      evictions: cache.stats.evictions,
      total_requests: cache.stats.total_requests + 1,
      hit_rate: cache.stats.hit_rate,
      avg_load_time: (cache.stats.avg_load_time * (cache.stats.total_requests as Float) + load_time) / ((cache.stats.total_requests + 1) as Float)
    }
    
    let final_stats = update_stats(updated_stats)
    
    match Map::get(cache.items, key) {
      Some(value) => {
        // 缓存命中
        let updated_cache = {
          items: cache.items,
          capacity: cache.capacity,
          stats: final_stats
        }
        
        (updated_cache, Some(value))
      }
      None => {
        // 缓存未命中
        let miss_stats = {
          hits: final_stats.hits - 1,
          misses: final_stats.misses + 1,
          sets: final_stats.sets,
          evictions: final_stats.evictions,
          total_requests: final_stats.total_requests,
          hit_rate: final_stats.hit_rate,
          avg_load_time: final_stats.avg_load_time
        }
        
        let updated_cache = {
          items: cache.items,
          capacity: cache.capacity,
          stats: update_stats(miss_stats)
        }
        
        (updated_cache, None)
      }
    }
  }
  
  // 设置缓存值
  let put = fn[K, V](cache: Cache[K, V], key: K, value: V) {
    let updated_items = Map::insert(cache.items, key, value)
    
    let updated_stats = {
      hits: cache.stats.hits,
      misses: cache.stats.misses,
      sets: cache.stats.sets + 1,
      evictions: cache.stats.evictions,
      total_requests: cache.stats.total_requests,
      hit_rate: cache.stats.hit_rate,
      avg_load_time: cache.stats.avg_load_time
    }
    
    {
      items: updated_items,
      capacity: cache.capacity,
      stats: update_stats(updated_stats)
    }
  }
  
  // 测试缓存性能监控
  let cache = create_cache<String, Int>(3)
  
  // 设置值
  let cache1 = put(cache, "key1", 1)
  let cache2 = put(cache1, "key2", 2)
  let cache3 = put(cache2, "key3", 3)
  
  // 验证设置统计
  assert_eq(cache3.stats.sets, 3)
  assert_eq(cache3.stats.hits, 0)
  assert_eq(cache3.stats.misses, 0)
  assert_eq(cache3.stats.total_requests, 0)
  assert_eq(cache3.stats.hit_rate, 0.0)
  
  // 获取值（命中）
  let (cache4, value1) = get(cache3, "key1", 0.5)
  assert_eq(value1, Some(1))
  
  // 验证命中统计
  assert_eq(cache4.stats.hits, 1)
  assert_eq(cache4.stats.misses, 0)
  assert_eq(cache4.stats.total_requests, 1)
  assert_eq(cache4.stats.hit_rate, 1.0)
  assert_eq(cache4.stats.avg_load_time, 0.5)
  
  // 获取不存在的值（未命中）
  let (cache5, value4) = get(cache4, "key4", 1.0)
  assert_eq(value4, None)
  
  // 验证未命中统计
  assert_eq(cache5.stats.hits, 1)
  assert_eq(cache5.stats.misses, 1)
  assert_eq(cache5.stats.total_requests, 2)
  assert_eq(cache5.stats.hit_rate, 0.5)
  assert_eq(cache5.stats.avg_load_time, (0.5 + 1.0) / 2.0)
  
  // 继续操作
  let (cache6, _) = get(cache5, "key2", 0.3)
  let (cache7, _) = get(cache5, "key3", 0.2)
  let (cache8, _) = get(cache5, "key4", 0.8)
  
  // 验证最终统计
  assert_eq(cache8.stats.hits, 3)  // key1, key2, key3
  assert_eq(cache8.stats.misses, 2)  // key4 (两次)
  assert_eq(cache8.stats.total_requests, 5)
  assert_eq(cache8.stats.hit_rate, 3.0 / 5.0)
  assert_eq(cache8.stats.avg_load_time, (0.5 + 1.0 + 0.3 + 0.2 + 0.8) / 5.0)
}