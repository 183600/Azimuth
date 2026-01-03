// Azimuth 缓存机制和策略测试
// 测试系统各种缓存实现和策略的有效性

// 测试1: 基础LRU缓存实现
test "基础LRU缓存实现" {
  // 定义缓存节点
  type CacheNode[K, V] = {
    key: K,
    value: V,
    prev: Option[Int],  // 前一个节点的索引
    next: Option[Int>   // 下一个节点的索引
  }
  
  // 定义LRU缓存
  type LRUCache[K, V] = {
    capacity: Int,
    nodes: Array[CacheNode[K, V]>,
    head_index: Option[Int>,
    tail_index: Option[Int>,
    key_to_index: Map[K, Int]
  }
  
  // 创建LRU缓存
  let create_lru_cache = fn(capacity: Int) -> LRUCache[String, String] {
    {
      capacity: capacity,
      nodes: [],
      head_index: None,
      tail_index: None,
      key_to_index: Map::new()
    }
  }
  
  // 将节点移动到链表头部
  let move_to_head = fn(cache: LRUCache[String, String], index: Int) -> LRUCache[String, String] {
    if cache.head_index == Some(index) {
      return cache  // 已经在头部
    }
    
    let node = cache.nodes[index]
    let mut updated_nodes = cache.nodes
    
    // 移除节点当前位置
    match node.prev {
      Some(prev_index) => {
        updated_nodes = updated_nodes.map_index(fn(i, n) {
          if i == prev_index {
            { key: n.key, value: n.value, prev: n.prev, next: node.next }
          } else {
            n
          }
        })
      }
      None => ()  // 节点没有前驱，说明是尾部节点
    }
    
    match node.next {
      Some(next_index) => {
        updated_nodes = updated_nodes.map_index(fn(i, n) {
          if i == next_index {
            { key: n.key, value: n.value, prev: node.prev, next: n.next }
          } else {
            n
          }
        })
      }
      None => ()  // 节点没有后继
    }
    
    // 更新尾部索引
    let new_tail_index = if cache.tail_index == Some(index) {
      node.prev
    } else {
      cache.tail_index
    }
    
    // 将节点添加到头部
    match cache.head_index {
      Some(head_index) => {
        updated_nodes = updated_nodes.map_index(fn(i, n) {
          if i == head_index {
            { key: n.key, value: n.value, prev: Some(index), next: n.next }
          } else if i == index {
            { key: n.key, value: n.value, prev: None, next: Some(head_index) }
          } else {
            n
          }
        })
      }
      None => {
        updated_nodes = updated_nodes.map_index(fn(i, n) {
          if i == index {
            { key: n.key, value: n.value, prev: None, next: None }
          } else {
            n
          }
        })
      }
    }
    
    {
      capacity: cache.capacity,
      nodes: updated_nodes,
      head_index: Some(index),
      tail_index: new_tail_index,
      key_to_index: cache.key_to_index
    }
  }
  
  // 添加新节点到头部
  let add_to_head = fn(cache: LRUCache[String, String], key: String, value: String) -> LRUCache[String, String] {
    let new_node = {
      key: key,
      value: value,
      prev: None,
      next: cache.head_index
    }
    
    let new_index = cache.nodes.length()
    let mut updated_nodes = cache.nodes.push(new_node)
    
    // 更新原头节点
    match cache.head_index {
      Some(head_index) => {
        updated_nodes = updated_nodes.map_index(fn(i, n) {
          if i == head_index {
            { key: n.key, value: n.value, prev: Some(new_index), next: n.next }
          } else {
            n
          }
        })
      }
      None => ()  // 空链表
    }
    
    // 更新尾部索引
    let new_tail_index = if cache.tail_index.is_none() {
      Some(new_index)
    } else {
      cache.tail_index
    }
    
    let updated_key_map = cache.key_to_index.set(key, new_index)
    
    {
      capacity: cache.capacity,
      nodes: updated_nodes,
      head_index: Some(new_index),
      tail_index: new_tail_index,
      key_to_index: updated_key_map
    }
  }
  
  // 移除尾部节点
  let remove_tail = fn(cache: LRUCache[String, String>) -> LRUCache[String, String] {
    match cache.tail_index {
      Some(tail_index) => {
        let tail_node = cache.nodes[tail_index]
        let mut updated_nodes = cache.nodes
        let mut updated_key_map = cache.key_to_index
        
        // 移除尾部节点
        updated_key_map = updated_key_map.remove(tail_node.key)
        
        // 更新前驱节点
        match tail_node.prev {
          Some(prev_index) => {
            updated_nodes = updated_nodes.map_index(fn(i, n) {
              if i == prev_index {
                { key: n.key, value: n.value, prev: n.prev, next: None }
              } else {
                n
              }
            })
          }
          None => ()  // 没有前驱节点
        }
        
        {
          capacity: cache.capacity,
          nodes: updated_nodes,
          head_index: cache.head_index,
          tail_index: tail_node.prev,
          key_to_index: updated_key_map
        }
      }
      None => cache  // 空链表
    }
  }
  
  // 获取缓存值
  let get = fn(cache: LRUCache[String, String], key: String) -> (LRUCache[String, String], Option[String]) {
    match cache.key_to_index.get(key) {
      Some(index) => {
        // 移动到头部
        let updated_cache = move_to_head(cache, index)
        let node = updated_cache.nodes[index]
        (updated_cache, Some(node.value))
      }
      None => (cache, None)
    }
  }
  
  // 设置缓存值
  let put = fn(cache: LRUCache[String, String], key: String, value: String) -> LRUCache[String, String] {
    match cache.key_to_index.get(key) {
      Some(index) => {
        // 更新现有节点
        let updated_nodes = cache.nodes.map_index(fn(i, n) {
          if i == index {
            { key: n.key, value: value, prev: n.prev, next: n.next }
          } else {
            n
          }
        })
        
        let updated_cache = {
          capacity: cache.capacity,
          nodes: updated_nodes,
          head_index: cache.head_index,
          tail_index: cache.tail_index,
          key_to_index: cache.key_to_index
        }
        
        // 移动到头部
        move_to_head(updated_cache, index)
      }
      None => {
        // 添加新节点
        let mut updated_cache = add_to_head(cache, key, value)
        
        // 检查容量限制
        if updated_cache.nodes.length() > updated_cache.capacity {
          updated_cache = remove_tail(updated_cache)
        }
        
        updated_cache
      }
    }
  }
  
  // 创建LRU缓存
  let mut cache = create_lru_cache(3)
  
  // 添加键值对
  cache = put(cache, "key1", "value1")
  cache = put(cache, "key2", "value2")
  cache = put(cache, "key3", "value3")
  
  // 验证缓存内容
  assert_eq(cache.nodes.length(), 3)
  
  // 获取key1，应该将其移动到头部
  let (updated_cache, value) = get(cache, "key1")
  cache = updated_cache
  match value {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 验证key1现在是头部
  match cache.head_index {
    Some(head_index) => {
      let head_node = cache.nodes[head_index]
      assert_eq(head_node.key, "key1")
    }
    None => assert_true(false)
  }
  
  // 添加新键值对，应该淘汰尾部节点(key2)
  cache = put(cache, "key4", "value4")
  
  // 验证缓存大小
  assert_eq(cache.nodes.length(), 3)
  
  // 验证key2已被淘汰
  let (_, value2) = get(cache, "key2")
  assert_eq(value2, None)
  
  // 验证key3, key1, key4仍然存在
  let (_, value3) = get(cache, "key3")
  let (_, value1) = get(cache, "key1")
  let (_, value4) = get(cache, "key4")
  
  match (value3, value1, value4) {
    (Some(v3), Some(v1), Some(v4)) => {
      assert_eq(v3, "value3")
      assert_eq(v1, "value1")
      assert_eq(v4, "value4")
    }
    _ => assert_true(false)
  }
}

// 测试2: TTL(Time-To-Live)缓存实现
test "TTL缓存实现" {
  // 定义TTL缓存节点
  type TTLNode[K, V] = {
    key: K,
    value: V,
    expire_time: Int  // 过期时间戳
  }
  
  // 定义TTL缓存
  type TTLCache[K, V] = {
    nodes: Array[TTLNode[K, V]>,
    key_to_index: Map[K, Int]
  }
  
  // 创建TTL缓存
  let create_ttl_cache = fn() -> TTLCache[String, String] {
    {
      nodes: [],
      key_to_index: Map::new()
    }
  }
  
  // 清理过期节点
  let cleanup_expired = fn(cache: TTLCache[String, String>) -> TTLCache[String, String] {
    let current_time = Time::now()
    let mut valid_nodes = []
    let mut updated_key_map = cache.key_to_index
    
    for i = 0; i < cache.nodes.length(); i = i + 1 {
      let node = cache.nodes[i]
      if node.expire_time > current_time {
        // 节点未过期，保留
        valid_nodes = valid_nodes.push(node)
        updated_key_map = updated_key_map.set(node.key, valid_nodes.length() - 1)
      } else {
        // 节点已过期，移除
        updated_key_map = updated_key_map.remove(node.key)
      }
    }
    
    {
      nodes: valid_nodes,
      key_to_index: updated_key_map
    }
  }
  
  // 获取缓存值
  let get_ttl = fn(cache: TTLCache[String, String], key: String) -> (TTLCache[String, String], Option[String>) {
    // 先清理过期节点
    let cleaned_cache = cleanup_expired(cache)
    
    match cleaned_cache.key_to_index.get(key) {
      Some(index) => {
        let node = cleaned_cache.nodes[index]
        (cleaned_cache, Some(node.value))
      }
      None => (cleaned_cache, None)
    }
  }
  
  // 设置缓存值
  let put_ttl = fn(cache: TTLCache[String, String], key: String, value: String, ttl_ms: Int) -> TTLCache[String, String] {
    // 先清理过期节点
    let cleaned_cache = cleanup_expired(cache)
    
    let current_time = Time::now()
    let expire_time = current_time + ttl_ms
    
    let new_node = {
      key: key,
      value: value,
      expire_time: expire_time
    }
    
    let updated_nodes = cleaned_cache.nodes.push(new_node)
    let updated_key_map = cleaned_cache.key_to_index.set(key, updated_nodes.length() - 1)
    
    {
      nodes: updated_nodes,
      key_to_index: updated_key_map
    }
  }
  
  // 创建TTL缓存
  let mut cache = create_ttl_cache()
  
  // 添加键值对，TTL为1秒
  cache = put_ttl(cache, "key1", "value1", 1000)
  cache = put_ttl(cache, "key2", "value2", 1000)
  cache = put_ttl(cache, "key3", "value3", 2000)  // 2秒TTL
  
  // 验证缓存内容
  assert_eq(cache.nodes.length(), 3)
  
  // 获取key1，应该存在
  let (updated_cache, value1) = get_ttl(cache, "key1")
  cache = updated_cache
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 添加一个已过期的键值对(模拟)
  let current_time = Time::now()
  let expired_node = {
    key: "key4",
    value: "value4",
    expire_time: current_time - 1000  // 已过期
  }
  
  cache = {
    nodes: cache.nodes.push(expired_node),
    key_to_index: cache.key_to_index.set("key4", cache.nodes.length())
  }
  
  // 验证缓存包含4个节点(包括过期的)
  assert_eq(cache.nodes.length(), 4)
  
  // 获取key2，应该触发清理过期节点
  let (updated_cache, value2) = get_ttl(cache, "key2")
  cache = updated_cache
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // 验证过期节点已被清理
  assert_eq(cache.nodes.length(), 3)  // key4应该被清理
  
  // 验证key4不存在
  let (_, value4) = get_ttl(cache, "key4")
  assert_eq(value4, None)
  
  // 验证key1, key2, key3仍然存在
  let (_, value1_check) = get_ttl(cache, "key1")
  let (_, value2_check) = get_ttl(cache, "key2")
  let (_, value3_check) = get_ttl(cache, "key3")
  
  match (value1_check, value2_check, value3_check) {
    (Some(v1), Some(v2), Some(v3)) => {
      assert_eq(v1, "value1")
      assert_eq(v2, "value2")
      assert_eq(v3, "value3")
    }
    _ => assert_true(false)
  }
}

// 测试3: 多级缓存实现
test "多级缓存实现" {
  // 定义缓存级别
  enum CacheLevel {
    L1  // 内存缓存(最快)
    L2  // 本地缓存(较快)
    L3  // 分布式缓存(较慢)
  }
  
  // 定义缓存项
  type CacheItem[V] = {
    value: V,
    level: CacheLevel,
    timestamp: Int,
    hit_count: Int
  }
  
  // 定义多级缓存
  type MultiLevelCache[K, V] = {
    l1_cache: Map[K, CacheItem[V]>,
    l2_cache: Map[K, CacheItem[V]>,
    l3_cache: Map[K, CacheItem[V]>,
    l1_max_size: Int,
    l2_max_size: Int,
    l3_max_size: Int
  }
  
  // 创建多级缓存
  let create_multi_level_cache = fn(l1_size: Int, l2_size: Int, l3_size: Int) -> MultiLevelCache[String, String] {
    {
      l1_cache: Map::new(),
      l2_cache: Map::new(),
      l3_cache: Map::new(),
      l1_max_size: l1_size,
      l2_max_size: l2_size,
      l3_max_size: l3_size
    }
  }
  
  // 从指定级别获取值
  let get_from_level = fn(cache: MultiLevelCache[String, String], key: String, level: CacheLevel) -> Option[CacheItem[String]] {
    match level {
      CacheLevel::L1 => cache.l1_cache.get(key)
      CacheLevel::L2 => cache.l2_cache.get(key)
      CacheLevel::L3 => cache.l3_cache.get(key)
    }
  }
  
  // 将值放入指定级别
  let put_to_level = fn(cache: MultiLevelCache[String, String], key: String, value: String, level: CacheLevel) -> MultiLevelCache[String, String] {
    let current_time = Time::now()
    let item = {
      value: value,
      level: level,
      timestamp: current_time,
      hit_count: 0
    }
    
    match level {
      CacheLevel::L1 => {
        // 检查L1缓存大小限制
        if cache.l1_cache.size() >= cache.l1_max_size {
          // 简化实现：直接移除一个元素
          let first_key = cache.l1_cache.keys()[0]
          let new_l1_cache = cache.l1_cache.remove(first_key).set(key, item)
          { l1_cache: new_l1_cache, l2_cache: cache.l2_cache, l3_cache: cache.l3_cache, 
            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        } else {
          { l1_cache: cache.l1_cache.set(key, item), l2_cache: cache.l2_cache, l3_cache: cache.l3_cache, 
            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        }
      }
      CacheLevel::L2 => {
        // 检查L2缓存大小限制
        if cache.l2_cache.size() >= cache.l2_max_size {
          // 简化实现：直接移除一个元素
          let first_key = cache.l2_cache.keys()[0]
          let new_l2_cache = cache.l2_cache.remove(first_key).set(key, item)
          { l1_cache: cache.l1_cache, l2_cache: new_l2_cache, l3_cache: cache.l3_cache, 
            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        } else {
          { l1_cache: cache.l1_cache, l2_cache: cache.l2_cache.set(key, item), l3_cache: cache.l3_cache, 
            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        }
      }
      CacheLevel::L3 => {
        // 检查L3缓存大小限制
        if cache.l3_cache.size() >= cache.l3_max_size {
          // 简化实现：直接移除一个元素
          let first_key = cache.l3_cache.keys()[0]
          let new_l3_cache = cache.l3_cache.remove(first_key).set(key, item)
          { l1_cache: cache.l1_cache, l2_cache: cache.l2_cache, l3_cache: new_l3_cache, 
            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        } else {
          { l1_cache: cache.l1_cache, l2_cache: cache.l2_cache, l3_cache: cache.l3_cache.set(key, item), 
            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        }
      }
    }
  }
  
  // 多级缓存获取
  let get_multi_level = fn(cache: MultiLevelCache[String, String], key: String) -> (MultiLevelCache[String, String], Option<String>) {
    // 先尝试L1缓存
    match get_from_level(cache, key, CacheLevel::L1) {
      Some(item) => {
        // 更新命中计数
        let updated_item = { value: item.value, level: item.level, timestamp: item.timestamp, hit_count: item.hit_count + 1 }
        let updated_cache = { l1_cache: cache.l1_cache.set(key, updated_item), l2_cache: cache.l2_cache, l3_cache: cache.l3_cache, 
                            l1_max_size: cache.l1_max_size, l2_max_size: cache.l2_max_size, l3_max_size: cache.l3_max_size }
        return (updated_cache, Some(item.value))
      }
      None => ()
    }
    
    // 尝试L2缓存
    match get_from_level(cache, key, CacheLevel::L2) {
      Some(item) => {
        // 提升到L1缓存
        let mut updated_cache = put_to_level(cache, key, item.value, CacheLevel::L1)
        
        // 更新命中计数
        let updated_item = { value: item.value, level: item.level, timestamp: item.timestamp, hit_count: item.hit_count + 1 }
        updated_cache = { l1_cache: updated_cache.l1_cache, l2_cache: updated_cache.l2_cache.set(key, updated_item), l3_cache: updated_cache.l3_cache, 
                         l1_max_size: updated_cache.l1_max_size, l2_max_size: updated_cache.l2_max_size, l3_max_size: updated_cache.l3_max_size }
        
        return (updated_cache, Some(item.value))
      }
      None => ()
    }
    
    // 尝试L3缓存
    match get_from_level(cache, key, CacheLevel::L3) {
      Some(item) => {
        // 提升到L2缓存
        let mut updated_cache = put_to_level(cache, key, item.value, CacheLevel::L2)
        
        // 更新命中计数
        let updated_item = { value: item.value, level: item.level, timestamp: item.timestamp, hit_count: item.hit_count + 1 }
        updated_cache = { l1_cache: updated_cache.l1_cache, l2_cache: updated_cache.l2_cache, l3_cache: updated_cache.l3_cache.set(key, updated_item), 
                         l1_max_size: updated_cache.l1_max_size, l2_max_size: updated_cache.l2_max_size, l3_max_size: updated_cache.l3_max_size }
        
        return (updated_cache, Some(item.value))
      }
      None => ()
    }
    
    // 所有级别都没有找到
    (cache, None)
  }
  
  // 多级缓存设置
  let put_multi_level = fn(cache: MultiLevelCache[String, String], key: String, value: String, level: CacheLevel) -> MultiLevelCache[String, String] {
    put_to_level(cache, key, value, level)
  }
  
  // 创建多级缓存
  let mut cache = create_multi_level_cache(2, 3, 5)
  
  // 添加键值对到不同级别
  cache = put_multi_level(cache, "key1", "value1", CacheLevel::L1)
  cache = put_multi_level(cache, "key2", "value2", CacheLevel::L2)
  cache = put_multi_level(cache, "key3", "value3", CacheLevel::L3)
  
  // 验证缓存内容
  assert_eq(cache.l1_cache.size(), 1)
  assert_eq(cache.l2_cache.size(), 1)
  assert_eq(cache.l3_cache.size(), 1)
  
  // 获取key1，应该从L1缓存获取
  let (updated_cache, value1) = get_multi_level(cache, "key1")
  cache = updated_cache
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 验证key1的命中计数增加
  match cache.l1_cache.get("key1") {
    Some(item) => assert_eq(item.hit_count, 1)
    None => assert_true(false)
  }
  
  // 获取key2，应该从L2缓存获取并提升到L1
  let (updated_cache, value2) = get_multi_level(cache, "key2")
  cache = updated_cache
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // 验证key2现在在L1缓存中
  assert_true(cache.l1_cache.contains_key("key2"))
  assert_true(cache.l2_cache.contains_key("key2"))
  
  // 获取key3，应该从L3缓存获取并提升到L2
  let (updated_cache, value3) = get_multi_level(cache, "key3")
  cache = updated_cache
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // 验证key3现在在L2缓存中
  assert_true(cache.l2_cache.contains_key("key3"))
  assert_true(cache.l3_cache.contains_key("key3"))
  
  // 测试缓存大小限制
  cache = put_multi_level(cache, "key4", "value4", CacheLevel::L1)
  cache = put_multi_level(cache, "key5", "value5", CacheLevel::L1)
  
  // 验证L1缓存大小不超过限制
  assert_true(cache.l1_cache.size() <= cache.l1_max_size)
}

// 测试4: 缓存预热和批量操作
test "缓存预热和批量操作" {
  // 定义缓存预热策略
  enum WarmupStrategy {
    LoadAll        // 预加载所有数据
    LoadRecent     // 预加载最近数据
    LoadPopular    // 预加载热门数据
    LoadPredictive // 预测性预加载
  }
  
  // 定义缓存统计
  type CacheStats = {
    hits: Int,
    misses: Int,
    total_requests: Int,
    hit_rate: Float
  }
  
  // 定义预热缓存
  type WarmupCache[K, V] = {
    cache: Map[K, V],
    stats: CacheStats,
    warmup_strategy: WarmupStrategy
  }
  
  // 创建预热缓存
  let create_warmup_cache = fn(strategy: WarmupStrategy) -> WarmupCache[String, String] {
    {
      cache: Map::new(),
      stats: { hits: 0, misses: 0, total_requests: 0, hit_rate: 0.0 },
      warmup_strategy: strategy
    }
  }
  
  // 更新统计信息
  let update_stats = fn(stats: CacheStats, is_hit: Bool) -> CacheStats {
    let new_hits = if is_hit { stats.hits + 1 } else { stats.hits }
    let new_misses = if !is_hit { stats.misses + 1 } else { stats.misses }
    let new_total = stats.total_requests + 1
    let new_hit_rate = if new_total > 0 {
      (new_hits as Float) / (new_total as Float)
    } else {
      0.0
    }
    
    { hits: new_hits, misses: new_misses, total_requests: new_total, hit_rate: new_hit_rate }
  }
  
  // 缓存预热
  let warmup_cache = fn(cache: WarmupCache[String, String>, data: Array[(String, String)>) -> WarmupCache[String, String] {
    let mut warmed_cache = cache.cache
    
    match cache.warmup_strategy {
      WarmupStrategy::LoadAll => {
        // 预加载所有数据
        for (key, value) in data {
          warmed_cache = warmed_cache.set(key, value)
        }
      }
      WarmupStrategy::LoadRecent => {
        // 预加载最近的数据(简化：取前一半)
        let recent_count = data.length() / 2
        for i = 0; i < recent_count; i = i + 1 {
          let (key, value) = data[i]
          warmed_cache = warmed_cache.set(key, value)
        }
      }
      WarmupStrategy::LoadPopular => {
        // 预加载热门数据(简化：取偶数索引)
        for i = 0; i < data.length(); i = i + 2 {
          let (key, value) = data[i]
          warmed_cache = warmed_cache.set(key, value)
        }
      }
      WarmupStrategy::LoadPredictive => {
        // 预测性预加载(简化：取能被3整除的索引)
        for i = 0; i < data.length(); i = i + 1 {
          if i % 3 == 0 {
            let (key, value) = data[i]
            warmed_cache = warmed_cache.set(key, value)
          }
        }
      }
    }
    
    {
      cache: warmed_cache,
      stats: cache.stats,
      warmup_strategy: cache.warmup_strategy
    }
  }
  
  // 批量获取
  let batch_get = fn(cache: WarmupCache[String, String], keys: Array[String>) -> (WarmupCache[String, String], Array[(String, Option[String>)>) {
    let mut results = []
    let mut updated_cache = cache
    
    for key in keys {
      match updated_cache.cache.get(key) {
        Some(value) => {
          results = results.push((key, Some(value)))
          updated_cache = {
            cache: updated_cache.cache,
            stats: update_stats(updated_cache.stats, true),
            warmup_strategy: updated_cache.warmup_strategy
          }
        }
        None => {
          results = results.push((key, None))
          updated_cache = {
            cache: updated_cache.cache,
            stats: update_stats(updated_cache.stats, false),
            warmup_strategy: updated_cache.warmup_strategy
          }
        }
      }
    }
    
    (updated_cache, results)
  }
  
  // 批量设置
  let batch_put = fn(cache: WarmupCache[String, String], items: Array[(String, String)>) -> WarmupCache[String, String] {
    let mut updated_cache = cache.cache
    
    for (key, value) in items {
      updated_cache = updated_cache.set(key, value)
    }
    
    {
      cache: updated_cache,
      stats: cache.stats,
      warmup_strategy: cache.warmup_strategy
    }
  }
  
  // 创建测试数据
  let test_data = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3"),
    ("key4", "value4"),
    ("key5", "value5"),
    ("key6", "value6"),
    ("key7", "value7"),
    ("key8", "value8"),
    ("key9", "value9"),
    ("key10", "value10")
  ]
  
  // 测试LoadAll策略
  let mut cache = create_warmup_cache(WarmupStrategy::LoadAll)
  cache = warmup_cache(cache, test_data)
  
  // 验证所有数据都已预加载
  assert_eq(cache.cache.size(), test_data.length())
  
  // 批量获取测试
  let batch_keys = ["key1", "key3", "key5", "key11"]  // key11不存在
  let (updated_cache, batch_results) = batch_get(cache, batch_keys)
  cache = updated_cache
  
  // 验证批量获取结果
  assert_eq(batch_results.length(), 4)
  
  match batch_results[0] {
    (k, Some(v)) => {
      assert_eq(k, "key1")
      assert_eq(v, "value1")
    }
    _ => assert_true(false)
  }
  
  match batch_results[1] {
    (k, Some(v)) => {
      assert_eq(k, "key3")
      assert_eq(v, "value3")
    }
    _ => assert_true(false)
  }
  
  match batch_results[2] {
    (k, Some(v)) => {
      assert_eq(k, "key5")
      assert_eq(v, "value5")
    }
    _ => assert_true(false)
  }
  
  match batch_results[3] {
    (k, None) => assert_eq(k, "key11")
    _ => assert_true(false)
  }
  
  // 验证统计信息
  assert_eq(cache.stats.hits, 3)
  assert_eq(cache.stats.misses, 1)
  assert_eq(cache.stats.total_requests, 4)
  assert_eq(cache.stats.hit_rate, 0.75)
  
  // 测试LoadRecent策略
  cache = create_warmup_cache(WarmupStrategy::LoadRecent)
  cache = warmup_cache(cache, test_data)
  
  // 验证只有最近的数据被预加载
  assert_eq(cache.cache.size(), test_data.length() / 2)
  
  // 测试LoadPopular策略
  cache = create_warmup_cache(WarmupStrategy::LoadPopular)
  cache = warmup_cache(cache, test_data)
  
  // 验证热门数据被预加载(偶数索引)
  assert_eq(cache.cache.size(), (test_data.length() + 1) / 2)
  assert_true(cache.cache.contains_key("key1"))  // 索引0
  assert_true(cache.cache.contains_key("key3"))  // 索引2
  assert_true(cache.cache.contains_key("key5"))  // 索引4
  assert_false(cache.cache.contains_key("key2")) // 索引1
  
  // 测试LoadPredictive策略
  cache = create_warmup_cache(WarmupStrategy::LoadPredictive)
  cache = warmup_cache(cache, test_data)
  
  // 验证预测性数据被预加载(能被3整除的索引)
  assert_true(cache.cache.contains_key("key1"))  // 索引0
  assert_true(cache.cache.contains_key("key4"))  // 索引3
  assert_true(cache.cache.contains_key("key7"))  // 索引6
  assert_true(cache.cache.contains_key("key10")) // 索引9
  assert_false(cache.cache.contains_key("key2")) // 索引1
  
  // 测试批量设置
  let batch_items = [
    ("new_key1", "new_value1"),
    ("new_key2", "new_value2"),
    ("new_key3", "new_value3")
  ]
  
  cache = batch_put(cache, batch_items)
  
  // 验证批量设置结果
  assert_true(cache.cache.contains_key("new_key1"))
  assert_true(cache.cache.contains_key("new_key2"))
  assert_true(cache.cache.contains_key("new_key3"))
  
  match cache.cache.get("new_key1") {
    Some(value) => assert_eq(value, "new_value1")
    None => assert_true(false)
  }
}

// 测试5: 缓存一致性策略
test "缓存一致性策略" {
  // 定义一致性策略
  enum ConsistencyStrategy {
    WriteThrough    // 写入时同时更新缓存和存储
    WriteBack       // 只更新缓存，稍后写入存储
    WriteAround     // 只写入存储，不更新缓存
    RefreshAhead    // 预测性刷新缓存
  }
  
  // 定义缓存项
  type ConsistentCacheItem[V] = {
    value: V,
    version: Int,
    dirty: Bool,  // 是否需要写回存储
    last_accessed: Int
  }
  
  // 定义一致性缓存
  type ConsistentCache[K, V] = {
    cache: Map[K, ConsistentCacheItem[V]>,
    storage: Map[K, (V, Int)>,  // (value, version)
    strategy: ConsistencyStrategy
  }
  
  // 创建一致性缓存
  let create_consistent_cache = fn(strategy: ConsistencyStrategy) -> ConsistentCache[String, String] {
    {
      cache: Map::new(),
      storage: Map::new(),
      strategy: strategy
    }
  }
  
  // 从存储获取
  let get_from_storage = fn(cache: ConsistentCache[String, String], key: String) -> Option[(String, Int)] {
    cache.storage.get(key)
  }
  
  // 写入存储
  let write_to_storage = fn(cache: ConsistentCache[String, String], key: String, value: String, version: Int) -> ConsistentCache[String, String] {
    {
      cache: cache.cache,
      storage: cache.storage.set(key, (value, version)),
      strategy: cache.strategy
    }
  }
  
  // 获取缓存值
  let get_consistent = fn(cache: ConsistentCache[String, String], key: String) -> (ConsistentCache[String, String], Option<String>) {
    match cache.cache.get(key) {
      Some(item) => {
        // 更新访问时间
        let current_time = Time::now()
        let updated_item = {
          value: item.value,
          version: item.version,
          dirty: item.dirty,
          last_accessed: current_time
        }
        
        let updated_cache = {
          cache: cache.cache.set(key, updated_item),
          storage: cache.storage,
          strategy: cache.strategy
        }
        
        (updated_cache, Some(item.value))
      }
      None => {
        // 缓存未命中，从存储获取
        match get_from_storage(cache, key) {
          Some((value, version)) => {
            let current_time = Time::now()
            let item = {
              value: value,
              version: version,
              dirty: false,
              last_accessed: current_time
            }
            
            let updated_cache = {
              cache: cache.cache.set(key, item),
              storage: cache.storage,
              strategy: cache.strategy
            }
            
            (updated_cache, Some(value))
          }
          None => (cache, None)
        }
      }
    }
  }
  
  // 设置缓存值
  let put_consistent = fn(cache: ConsistentCache[String, String], key: String, value: String) -> ConsistentCache[String, String] {
    let current_time = Time::now()
    
    match cache.strategy {
      ConsistencyStrategy::WriteThrough => {
        // 写入时同时更新缓存和存储
        let new_version = match cache.cache.get(key) {
          Some(item) => item.version + 1
          None => 1
        }
        
        let item = {
          value: value,
          version: new_version,
          dirty: false,
          last_accessed: current_time
        }
        
        let updated_cache = {
          cache: cache.cache.set(key, item),
          storage: cache.storage.set(key, (value, new_version)),
          strategy: cache.strategy
        }
        
        updated_cache
      }
      ConsistencyStrategy::WriteBack => {
        // 只更新缓存，标记为脏
        let new_version = match cache.cache.get(key) {
          Some(item) => item.version + 1
          None => 1
        }
        
        let item = {
          value: value,
          version: new_version,
          dirty: true,
          last_accessed: current_time
        }
        
        {
          cache: cache.cache.set(key, item),
          storage: cache.storage,
          strategy: cache.strategy
        }
      }
      ConsistencyStrategy::WriteAround => {
        // 只写入存储，不更新缓存
        let new_version = match cache.storage.get(key) {
          Some((_, version)) => version + 1
          None => 1
        }
        
        write_to_storage(cache, key, value, new_version)
      }
      ConsistencyStrategy::RefreshAhead => {
        // 预测性刷新缓存(简化为WriteThrough)
        let new_version = match cache.cache.get(key) {
          Some(item) => item.version + 1
          None => 1
        }
        
        let item = {
          value: value,
          version: new_version,
          dirty: false,
          last_accessed: current_time
        }
        
        let updated_cache = {
          cache: cache.cache.set(key, item),
          storage: cache.storage.set(key, (value, new_version)),
          strategy: cache.strategy
        }
        
        updated_cache
      }
    }
  }
  
  // 刷新脏缓存项到存储
  let flush_dirty = fn(cache: ConsistentCache[String, String>) -> ConsistentCache[String, String] {
    let mut updated_cache = cache
    
    for (key, item) in cache.cache {
      if item.dirty {
        updated_cache = write_to_storage(updated_cache, key, item.value, item.version)
        
        // 更新缓存项，标记为非脏
        let clean_item = {
          value: item.value,
          version: item.version,
          dirty: false,
          last_accessed: item.last_accessed
        }
        
        updated_cache = {
          cache: updated_cache.cache.set(key, clean_item),
          storage: updated_cache.storage,
          strategy: updated_cache.strategy
        }
      }
    }
    
    updated_cache
  }
  
  // 测试WriteThrough策略
  let mut cache = create_consistent_cache(ConsistencyStrategy::WriteThrough)
  
  // 设置值
  cache = put_consistent(cache, "key1", "value1")
  
  // 验证缓存和存储都已更新
  match cache.cache.get("key1") {
    Some(item) => {
      assert_eq(item.value, "value1")
      assert_eq(item.version, 1)
      assert_false(item.dirty)
    }
    None => assert_true(false)
  }
  
  match cache.storage.get("key1") {
    Some((value, version)) => {
      assert_eq(value, "value1")
      assert_eq(version, 1)
    }
    None => assert_true(false)
  }
  
  // 测试WriteBack策略
  cache = create_consistent_cache(ConsistencyStrategy::WriteBack)
  
  // 设置值
  cache = put_consistent(cache, "key1", "value1")
  
  // 验证只有缓存已更新，存储未更新
  match cache.cache.get("key1") {
    Some(item) => {
      assert_eq(item.value, "value1")
      assert_eq(item.version, 1)
      assert_true(item.dirty)  // 标记为脏
    }
    None => assert_true(false)
  }
  
  assert_eq(cache.storage.get("key1"), None)  // 存储中不存在
  
  // 刷新脏缓存项
  cache = flush_dirty(cache)
  
  // 验证存储现在已更新
  match cache.cache.get("key1") {
    Some(item) => {
      assert_eq(item.value, "value1")
      assert_eq(item.version, 1)
      assert_false(item.dirty)  // 不再是脏
    }
    None => assert_true(false)
  }
  
  match cache.storage.get("key1") {
    Some((value, version)) => {
      assert_eq(value, "value1")
      assert_eq(version, 1)
    }
    None => assert_true(false)
  }
  
  // 测试WriteAround策略
  cache = create_consistent_cache(ConsistencyStrategy::WriteAround)
  
  // 先在缓存中设置一个值
  cache = {
    cache: cache.cache.set("key1", {
      value: "old_value",
      version: 1,
      dirty: false,
      last_accessed: Time::now()
    }),
    storage: cache.storage,
    strategy: cache.strategy
  }
  
  // 使用WriteAround策略设置新值
  cache = put_consistent(cache, "key1", "new_value")
  
  // 验证只有存储已更新，缓存未更新
  match cache.cache.get("key1") {
    Some(item) => assert_eq(item.value, "old_value")  // 仍然是旧值
    None => assert_true(false)
  }
  
  match cache.storage.get("key1") {
    Some((value, version)) => {
      assert_eq(value, "new_value")  // 存储中是新值
      assert_eq(version, 1)
    }
    None => assert_true(false)
  }
  
  // 测试缓存一致性：获取时应该从存储加载新值
  let (updated_cache, value) = get_consistent(cache, "key1")
  cache = updated_cache
  
  match value {
    Some(v) => assert_eq(v, "new_value")  // 应该是存储中的新值
    None => assert_true(false)
  }
  
  match cache.cache.get("key1") {
    Some(item) => assert_eq(item.value, "new_value")  // 缓存已更新
    None => assert_true(false)
  }
}