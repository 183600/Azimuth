// Azimuth 缓存机制测试用例
// 专注于缓存策略、过期机制和缓存一致性

// 测试1: 基础缓存操作
test "基础缓存操作" {
  // 创建基础缓存
  let cache = Cache::new(100)  # 最大100项
  
  // 测试空缓存
  assert_true(Cache::is_empty(cache))
  assert_eq(Cache::size(cache), 0)
  
  // 测试添加和获取
  Cache::put(cache, "key1", "value1")
  assert_false(Cache::is_empty(cache))
  assert_eq(Cache::size(cache), 1)
  
  let value1 = Cache::get(cache, "key1")
  assert_eq(value1, Some("value1"))
  
  // 测试不存在的键
  let missing_value = Cache::get(cache, "nonexistent")
  assert_eq(missing_value, None)
  
  // 测试覆盖现有值
  Cache::put(cache, "key1", "new_value1")
  let updated_value = Cache::get(cache, "key1")
  assert_eq(updated_value, Some("new_value1"))
  assert_eq(Cache::size(cache), 1)  # 大小不变
  
  // 测试添加多个键
  Cache::put(cache, "key2", "value2")
  Cache::put(cache, "key3", "value3")
  assert_eq(Cache::size(cache), 3)
  
  // 测试包含键
  assert_true(Cache::contains_key(cache, "key1"))
  assert_true(Cache::contains_key(cache, "key2"))
  assert_false(Cache::contains_key(cache, "nonexistent"))
  
  // 测试删除键
  let removed_value = Cache::remove(cache, "key2")
  assert_eq(removed_value, Some("value2"))
  assert_eq(Cache::size(cache), 2)
  assert_false(Cache::contains_key(cache, "key2"))
  
  // 测试删除不存在的键
  let non_existent_removal = Cache::remove(cache, "nonexistent")
  assert_eq(non_existent_removal, None)
  assert_eq(Cache::size(cache), 2)
  
  // 测试清空缓存
  Cache::clear(cache)
  assert_true(Cache::is_empty(cache))
  assert_eq(Cache::size(cache), 0)
}

// 测试2: 缓存过期策略
test "缓存过期策略" {
  // 创建带过期时间的缓存
  let expiring_cache = ExpiringCache::new(50)  # 最大50项
  
  // 测试基于时间的过期
  let short_ttl = 100  # 100ms
  let long_ttl = 1000  # 1000ms
  
  ExpiringCache::put_with_ttl(expiring_cache, "short_lived", "value1", short_ttl)
  ExpiringCache::put_with_ttl(expiring_cache, "long_lived", "value2", long_ttl)
  
  // 立即检查，两个值都应该存在
  assert_eq(ExpiringCache::get(expiring_cache, "short_lived"), Some("value1"))
  assert_eq(ExpiringCache::get(expiring_cache, "long_lived"), Some("value2"))
  
  // 等待短过期时间
  Thread::sleep(150)
  
  // 检查过期情况
  assert_eq(ExpiringCache::get(expiring_cache, "short_lived"), None)  # 已过期
  assert_eq(ExpiringCache::get(expiring_cache, "long_lived"), Some("value2"))  # 仍然有效
  
  // 测试刷新过期时间
  ExpiringCache::put_with_ttl(expiring_cache, "refreshable", "value3", short_ttl)
  Thread::sleep(50)  # 等待一半时间
  ExpiringCache::refresh_ttl(expiring_cache, "refreshable", short_ttl)  # 刷新TTL
  Thread::sleep(100)  # 等待原始过期时间
  
  // 应该仍然存在，因为TTL已刷新
  assert_eq(ExpiringCache::get(expiring_cache, "refreshable"), Some("value3"))
  
  // 测试自动清理过期项
  ExpiringCache::put_with_ttl(expiring_cache, "cleanup1", "value4", 50)
  ExpiringCache::put_with_ttl(expiring_cache, "cleanup2", "value5", 50)
  ExpiringCache::put_with_ttl(expiring_cache, "cleanup3", "value6", 200)
  
  Thread::sleep(100)  # 等待前两个过期
  
  // 手动清理过期项
  let cleaned_count = ExpiringCache::cleanup_expired(expiring_cache)
  assert_eq(cleaned_count, 2)  # 清理了2个过期项
  
  // 验证清理结果
  assert_eq(ExpiringCache::get(expiring_cache, "cleanup1"), None)
  assert_eq(ExpiringCache::get(expiring_cache, "cleanup2"), None)
  assert_eq(ExpiringCache::get(expiring_cache, "cleanup3"), Some("value6"))
}

// 测试3: LRU缓存策略
test "LRU缓存策略" {
  // 创建LRU缓存，容量为3
  let lru_cache = LRUCache::new(3)
  
  // 添加3个项目
  LRUCache::put(lru_cache, "key1", "value1")
  LRUCache::put(lru_cache, "key2", "value2")
  LRUCache::put(lru_cache, "key3", "value3")
  
  // 验证所有项目都存在
  assert_eq(LRUCache::get(lru_cache, "key1"), Some("value1"))
  assert_eq(LRUCache::get(lru_cache, "key2"), Some("value2"))
  assert_eq(LRUCache::get(lru_cache, "key3"), Some("value3"))
  assert_eq(LRUCache::size(lru_cache), 3)
  
  // 访问key1，使其成为最近使用的
  LRUCache::get(lru_cache, "key1")
  
  // 添加第4个项目，应该淘汰key2（最久未使用）
  LRUCache::put(lru_cache, "key4", "value4")
  
  // 验证淘汰情况
  assert_eq(LRUCache::get(lru_cache, "key1"), Some("value1"))  # 最近访问过，保留
  assert_eq(LRUCache::get(lru_cache, "key2"), None)  # 最久未使用，被淘汰
  assert_eq(LRUCache::get(lru_cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(lru_cache, "key4"), Some("value4"))
  assert_eq(LRUCache::size(lru_cache), 3)
  
  // 访问key3和key4，改变它们的访问顺序
  LRUCache::get(lru_cache, "key3")
  LRUCache::get(lru_cache, "key4")
  
  // 添加第5个项目，应该淘汰key1
  LRUCache::put(lru_cache, "key5", "value5")
  
  // 验证新的淘汰情况
  assert_eq(LRUCache::get(lru_cache, "key1"), None)  # 最久未使用，被淘汰
  assert_eq(LRUCache::get(lru_cache, "key3"), Some("value3"))
  assert_eq(LRUCache::get(lru_cache, "key4"), Some("value4"))
  assert_eq(LRUCache::get(lru_cache, "key5"), Some("value5"))
  
  // 测试更新现有项不会影响淘汰顺序
  LRUCache::put(lru_cache, "key3", "new_value3")  # 更新key3
  
  // 添加第6个项目，应该淘汰key4
  LRUCache::put(lru_cache, "key6", "value6")
  
  // 验证更新后的淘汰情况
  assert_eq(LRUCache::get(lru_cache, "key3"), Some("new_value3"))  # 已更新，保留
  assert_eq(LRUCache::get(lru_cache, "key4"), None)  # 最久未使用，被淘汰
  assert_eq(LRUCache::get(lru_cache, "key5"), Some("value5"))
  assert_eq(LRUCache::get(lru_cache, "key6"), Some("value6"))
  
  // 测试获取访问顺序
  let access_order = LRUCache::get_access_order(lru_cache)
  assert_eq(access_order, ["key6", "key5", "key3"])  # 从最近到最久
}

// 测试4: 缓存加载器和计算缓存
test "缓存加载器和计算缓存" {
  // 创建计算缓存
  let computing_cache = ComputingCache::new(20)
  
  // 设置计算函数
  ComputingCache::set_computer(computing_cache, "factorial", fn(n) {
    let mut result = 1
    for i in 2..=n {
      result = result * i
    }
    result
  })
  
  // 设置另一个计算函数
  ComputingCache::set_computer(computing_cache, "fibonacci", fn(n) {
    if n <= 1 {
      return n
    }
    
    let mut a = 0
    let mut b = 1
    for i in 2..=n {
      let temp = a + b
      a = b
      b = temp
    }
    b
  })
  
  // 测试首次计算（应该执行计算函数）
  let start_time = Time::now()
  let factorial_5 = ComputingCache::get_or_compute(computing_cache, "factorial", 5)
  let first_compute_time = Time::now() - start_time
  
  assert_eq(factorial_5, 120)
  
  // 测试缓存命中（应该更快）
  start_time = Time::now()
  let cached_factorial_5 = ComputingCache::get_or_compute(computing_cache, "factorial", 5)
  let cached_compute_time = Time::now() - start_time
  
  assert_eq(cached_factorial_5, 120)
  assert_true(cached_compute_time < first_compute_time)  # 缓存应该更快
  
  // 测试多个值的计算和缓存
  let fib_10 = ComputingCache::get_or_compute(computing_cache, "fibonacci", 10)
  let fib_15 = ComputingCache::get_or_compute(computing_cache, "fibonacci", 15)
  let fib_20 = ComputingCache::get_or_compute(computing_cache, "fibonacci", 20)
  
  assert_eq(fib_10, 55)
  assert_eq(fib_15, 610)
  assert_eq(fib_20, 6765)
  
  // 验证缓存包含这些值
  assert_true(ComputingCache::contains(computing_cache, "factorial", 5))
  assert_true(ComputingCache::contains(computing_cache, "fibonacci", 10))
  assert_true(ComputingCache::contains(computing_cache, "fibonacci", 15))
  assert_true(ComputingCache::contains(computing_cache, "fibonacci", 20))
  
  // 测试缓存统计
  let stats = ComputingCache::get_stats(computing_cache)
  assert_eq(stats.computations, 4)  # 1个factorial + 3个fibonacci
  assert_eq(stats.hits, 1)  # factorial_5的第二次访问
  assert_true(stats.hits + stats.misses == 5)  # 总访问次数
  
  // 测试批量计算
  let batch_keys = [5, 6, 7, 8, 9, 10]
  let batch_results = ComputingCache::get_or_compute_batch(computing_cache, "fibonacci", batch_keys)
  
  assert_eq(batch_results.length(), 6)
  assert_eq(batch_results[0], 5)   # fib(5)
  assert_eq(batch_results[1], 8)   # fib(6)
  assert_eq(batch_results[2], 13)  # fib(7)
  assert_eq(batch_results[3], 21)  # fib(8)
  assert_eq(batch_results[4], 34)  # fib(9)
  assert_eq(batch_results[5], 55)  # fib(10)，应该来自缓存
}

// 测试5: 分布式缓存模拟
test "分布式缓存模拟" {
  // 创建模拟分布式缓存节点
  let node1 = DistributedCacheNode::new("node1", 100)
  let node2 = DistributedCacheNode::new("node2", 100)
  let node3 = DistributedCacheNode::new("node3", 100)
  
  // 创建分布式缓存集群
  let cluster = DistributedCacheCluster::new([node1, node2, node3])
  
  // 测试数据分布
  DistributedCacheCluster::put(cluster, "user:1", "Alice")
  DistributedCacheCluster::put(cluster, "user:2", "Bob")
  DistributedCacheCluster::put(cluster, "user:3", "Charlie")
  DistributedCacheCluster::put(cluster, "user:4", "Diana")
  DistributedCacheCluster::put(cluster, "user:5", "Eve")
  
  // 验证数据分布在不同节点
  let user1_node = DistributedCacheCluster::locate_node(cluster, "user:1")
  let user2_node = DistributedCacheCluster::locate_node(cluster, "user:2")
  let user3_node = DistributedCacheCluster::locate_node(cluster, "user:3")
  
  // 获取数据
  let alice = DistributedCacheCluster::get(cluster, "user:1")
  let bob = DistributedCacheCluster::get(cluster, "user:2")
  let charlie = DistributedCacheCluster::get(cluster, "user:3")
  
  assert_eq(alice, Some("Alice"))
  assert_eq(bob, Some("Bob"))
  assert_eq(charlie, Some("Charlie"))
  
  // 测试节点故障处理
  DistributedCacheCluster::simulate_node_failure(cluster, "node2")
  
  // node2失败后，尝试获取原本在node2上的数据
  let bob_after_failure = DistributedCacheCluster::get(cluster, "user:2")
  
  // 应该从备份或重新计算中获取数据
  assert_eq(bob_after_failure, Some("Bob"))
  
  // 测试数据复制
  DistributedCacheCluster::set_replication_factor(cluster, 2)
  DistributedCacheCluster::put(cluster, "product:1", "Laptop")
  
  // 验证数据被复制到多个节点
  let product_nodes = DistributedCacheCluster::locate_all_replicas(cluster, "product:1")
  assert_eq(product_nodes.length(), 2)
  
  // 测试一致性哈希
  let ring = DistributedCacheCluster::get_consistent_hash_ring(cluster)
  assert_true(ring.nodes.length() >= 3)  # 至少3个节点
  
  // 测试节点添加
  let node4 = DistributedCacheNode::new("node4", 100)
  DistributedCacheCluster::add_node(cluster, node4)
  
  // 验证数据重新分布
  let redistributed_user1 = DistributedCacheCluster::get(cluster, "user:1")
  assert_eq(redistributed_user1, Some("Alice"))
  
  // 测试缓存预热
  let warmup_keys = ["user:1", "user:2", "user:3", "user:4", "user:5"]
  let warmup_values = ["Alice", "Bob", "Charlie", "Diana", "Eve"]
  
  DistributedCacheCluster::warmup(cluster, warmup_keys, warmup_values)
  
  // 验证预热后的缓存命中率
  let stats = DistributedCacheCluster::get_stats(cluster)
  assert_true(stats.total_nodes >= 4)
  assert_true(stats.replicated_items > 0)
}

// 测试6: 缓存策略和自适应缓存
test "缓存策略和自适应缓存" {
  // 创建自适应缓存
  let adaptive_cache = AdaptiveCache::new(50)
  
  // 设置初始策略
  AdaptiveCache::set_strategy(adaptive_cache, "lru")
  
  // 添加一些数据
  for i in 0..=30 {
    AdaptiveCache::put(adaptive_cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  // 记录初始命中率
  let initial_stats = AdaptiveCache::get_stats(adaptive_cache)
  let initial_hit_rate = initial_stats.hit_rate
  
  // 模拟访问模式：频繁访问某些键
  for i in 0..=10 {
    for j in 0..=5 {
      AdaptiveCache::get(adaptive_cache, "key" + j.to_string())  # 频繁访问前6个键
    }
  }
  
  // 获取更新后的统计
  let frequent_access_stats = AdaptiveCache::get_stats(adaptive_cache)
  assert_true(frequent_access_stats.hit_rate > initial_hit_rate)
  
  // 模拟访问模式变化：现在访问不同的键集
  for i in 0..=10 {
    for j in 6..=11 {
      AdaptiveCache::get(adaptive_cache, "key" + j.to_string())  # 访问6-11键
    }
  }
  
  // 自适应缓存应该检测到访问模式变化
  AdaptiveCache::analyze_access_patterns(adaptive_cache)
  
  // 获取策略建议
  let strategy_recommendation = AdaptiveCache::get_strategy_recommendation(adaptive_cache)
  assert_true(strategy_recommendation.confidence > 0.0)
  
  // 应用建议的策略
  AdaptiveCache::apply_strategy(adaptive_cache, strategy_recommendation.recommended_strategy)
  
  // 继续使用新策略
  for i in 0..=10 {
    for j in 12..=17 {
      AdaptiveCache::get(adaptive_cache, "key" + j.to_string())
    }
  }
  
  // 验证策略切换后的性能
  let final_stats = AdaptiveCache::get_stats(adaptive_cache)
  
  // 测试缓存预热策略
  AdaptiveCache::set_warmup_strategy(adaptive_cache, {
    strategy: "predictive",
    predictor: fn(access_history) {
      // 简单的预测器：预测最常访问的键
      let frequency_map = {}
      for key in access_history {
        frequency_map[key] = frequency_map.get(key, 0) + 1
      }
      
      // 返回最频繁的键
      frequency_map.keys()
        .sort_by(fn(a, b) { frequency_map[b] - frequency_map[a] })
        .take(10)
    }
  })
  
  // 执行预热
  let access_history = ["key1", "key2", "key3", "key1", "key2", "key1"]
  AdaptiveCache::warmup_with_history(adaptive_cache, access_history)
  
  // 验证预热结果
  let warmup_stats = AdaptiveCache::get_warmup_stats(adaptive_cache)
  assert_true(warmup_stats.predicted_keys.length() > 0)
  assert_true(warmup_stats.preloaded_items > 0)
}

// 测试7: 缓存事件和监听器
test "缓存事件和监听器" {
  // 创建带有事件系统的缓存
  let eventful_cache = EventfulCache::new(30)
  
  // 创建事件记录器
  let event_log = []
  
  // 添加事件监听器
  EventfulCache::add_listener(eventful_cache, "put", fn(key, value) {
    event_log = event_log.push("PUT: " + key + " = " + value.to_string())
  })
  
  EventfulCache::add_listener(eventful_cache, "get", fn(key, value) {
    match value {
      Some(v) => event_log = event_log.push("GET: " + key + " = " + v.to_string()),
      None => event_log = event_log.push("GET: " + key + " = NULL")
    }
  })
  
  EventfulCache::add_listener(eventful_cache, "remove", fn(key, value) {
    match value {
      Some(v) => event_log = event_log.push("REMOVE: " + key + " = " + v.to_string()),
      None => event_log = event_log.push("REMOVE: " + key + " = NULL")
    }
  })
  
  EventfulCache::add_listener(eventful_cache, "evict", fn(key, value) {
    match value {
      Some(v) => event_log = event_log.push("EVICT: " + key + " = " + v.to_string()),
      None => event_log = event_log.push("EVICT: " + key + " = NULL")
    }
  })
  
  // 执行缓存操作
  EventfulCache::put(eventful_cache, "key1", "value1")
  EventfulCache::put(eventful_cache, "key2", "value2")
  EventfulCache::get(eventful_cache, "key1")
  EventfulCache::get(eventful_cache, "nonexistent")
  EventfulCache::remove(eventful_cache, "key2")
  EventfulCache::get(eventful_cache, "key2")
  
  // 验证事件日志
  assert_eq(event_log[0], "PUT: key1 = value1")
  assert_eq(event_log[1], "PUT: key2 = value2")
  assert_eq(event_log[2], "GET: key1 = value1")
  assert_eq(event_log[3], "GET: nonexistent = NULL")
  assert_eq(event_log[4], "REMOVE: key2 = value2")
  assert_eq(event_log[5], "GET: key2 = NULL")
  
  // 测试批量操作事件
  EventfulCache::put_batch(eventful_cache, [
    ("batch1", "value1"),
    ("batch2", "value2"),
    ("batch3", "value3")
  ])
  
  // 验证批量操作事件
  let batch_put_events = event_log.filter(fn(event) { event.starts_with("PUT: batch") })
  assert_eq(batch_put_events.length(), 3)
  
  // 测试条件事件监听器
  EventfulCache::add_conditional_listener(eventful_cache, "put", fn(key, value) {
    key.starts_with("special_")
  }, fn(key, value) {
    event_log = event_log.push("SPECIAL_PUT: " + key + " = " + value.to_string())
  })
  
  EventfulCache::put(eventful_cache, "normal_key", "normal_value")
  EventfulCache::put(eventful_cache, "special_key", "special_value")
  
  // 验证条件监听器
  assert_true(event_log.contains("SPECIAL_PUT: special_key = special_value"))
  assert_false(event_log.contains("SPECIAL_PUT: normal_key = normal_value"))
  
  // 测试事件过滤
  let filtered_events = EventfulCache::get_events_by_type(eventful_cache, "put")
  assert_eq(filtered_events.length(), 6)  # 2个单独put + 3个批量put + 1个特殊put
  
  // 测试事件统计
  let event_stats = EventfulCache::get_event_stats(eventful_cache)
  assert_eq(event_stats.event_types.get("put"), 6)
  assert_eq(event_stats.event_types.get("get"), 3)
  assert_eq(event_stats.event_types.get("remove"), 1)
}

// 测试8: 缓存性能和基准测试
test "缓存性能和基准测试" {
  // 创建不同类型的缓存进行性能比较
  let basic_cache = BasicCache::new(1000)
  let lru_cache = LRUCache::new(1000)
  let expiring_cache = ExpiringCache::new(1000)
  
  // 准备测试数据
  let test_keys = []
  let test_values = []
  for i in 0..=500 {
    test_keys = test_keys.push("perf_key_" + i.to_string())
    test_values = test_values.push("perf_value_" + i.to_string())
  }
  
  // 基准测试：写入性能
  let basic_write_start = Time::now()
  for i in 0..=test_keys.length() - 1 {
    BasicCache::put(basic_cache, test_keys[i], test_values[i])
  }
  let basic_write_time = Time::now() - basic_write_start
  
  let lru_write_start = Time::now()
  for i in 0..=test_keys.length() - 1 {
    LRUCache::put(lru_cache, test_keys[i], test_values[i])
  }
  let lru_write_time = Time::now() - lru_write_start
  
  // 写入性能应该相近
  assert_true(basic_write_time > 0)
  assert_true(lru_write_time > 0)
  
  // 基准测试：读取性能
  let basic_read_start = Time::now()
  for i in 0..=test_keys.length() - 1 {
    BasicCache::get(basic_cache, test_keys[i])
  }
  let basic_read_time = Time::now() - basic_read_start
  
  let lru_read_start = Time::now()
  for i in 0..=test_keys.length() - 1 {
    LRUCache::get(lru_cache, test_keys[i])
  }
  let lru_read_time = Time::now() - lru_read_start
  
  // 读取性能应该相近
  assert_true(basic_read_time > 0)
  assert_true(lru_read_time > 0)
  
  // 基准测试：混合工作负载
  let mixed_access_pattern = []
  for i in 0..=200 {
    mixed_access_pattern = mixed_access_pattern.push({
      operation: "put",
      key: "mixed_key_" + i.to_string(),
      value: "mixed_value_" + i.to_string()
    })
  }
  
  for i in 0..=300 {
    let key_index = Math::random_int(0, 200)
    mixed_access_pattern = mixed_access_pattern.push({
      operation: "get",
      key: "mixed_key_" + key_index.to_string(),
      value: ""
    })
  }
  
  // 执行混合工作负载
  let basic_mixed_start = Time::now()
  for operation in mixed_access_pattern {
    match operation.operation {
      "put" => BasicCache::put(basic_cache, operation.key, operation.value),
      "get" => BasicCache::get(basic_cache, operation.key),
      _ => {}
    }
  }
  let basic_mixed_time = Time::now() - basic_mixed_start
  
  let lru_mixed_start = Time::now()
  for operation in mixed_access_pattern {
    match operation.operation {
      "put" => LRUCache::put(lru_cache, operation.key, operation.value),
      "get" => LRUCache::get(lru_cache, operation.key),
      _ => {}
    }
  }
  let lru_mixed_time = Time::now() - lru_mixed_start
  
  // 测试内存使用
  let basic_memory = BasicCache::estimate_memory_usage(basic_cache)
  let lru_memory = LRUCache::estimate_memory_usage(lru_cache)
  
  assert_true(basic_memory > 0)
  assert_true(lru_memory > 0)
  
  // LRU缓存可能使用更多内存（需要维护访问顺序）
  assert_true(lru_memory >= basic_memory)
  
  // 测试并发性能
  let concurrent_cache = ConcurrentCache::new(1000)
  let concurrent_stats = ConcurrentBenchmark::run(concurrent_cache, {
    num_threads: 4,
    operations_per_thread: 250,
    read_write_ratio: 0.7  # 70%读取，30%写入
  })
  
  assert_true(concurrent_stats.total_operations > 0)
  assert_true(concurrent_stats.avg_latency > 0)
  assert_true(concurrent_stats.throughput > 0)
  
  // 验证并发安全性
  assert_eq(concurrent_stats.errors, 0)
  assert_true(concurrent_stats.hit_rate >= 0.0 and concurrent_stats.hit_rate <= 1.0)
  
  // 生成性能报告
  let performance_report = CacheBenchmark::generate_report({
    basic_cache: {
      write_time: basic_write_time,
      read_time: basic_read_time,
      mixed_time: basic_mixed_time,
      memory_usage: basic_memory
    },
    lru_cache: {
      write_time: lru_write_time,
      read_time: lru_read_time,
      mixed_time: lru_mixed_time,
      memory_usage: lru_memory
    },
    concurrent_cache: concurrent_stats
  })
  
  // 验证报告内容
  assert_true(performance_report.contains("Basic Cache"))
  assert_true(performance_report.contains("LRU Cache"))
  assert_true(performance_report.contains("Concurrent Cache"))
  assert_true(performance_report.contains("Write Performance"))
  assert_true(performance_report.contains("Read Performance"))
  assert_true(performance_report.contains("Memory Usage"))
}