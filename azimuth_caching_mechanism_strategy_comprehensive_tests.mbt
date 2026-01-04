// Azimuth Caching Mechanism and Strategy Comprehensive Test Suite
// 测试缓存机制和策略，确保系统的高效数据缓存和检索

test "基础缓存操作测试" {
  // 创建基础缓存
  let cache = Cache::new(100) // 最大100个条目
  
  // 测试空缓存
  assert_eq(Cache::size(cache), 0)
  assert_true(Cache::is_empty(cache))
  
  // 测试添加单个条目
  Cache::set(cache, "key1", "value1")
  assert_eq(Cache::size(cache), 1)
  assert_false(Cache::is_empty(cache))
  
  // 测试获取条目
  let value = Cache::get(cache, "key1")
  match value {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 测试获取不存在的条目
  let non_existent = Cache::get(cache, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试更新条目
  Cache::set(cache, "key1", "updated_value1")
  let updated_value = Cache::get(cache, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // 测试删除条目
  Cache::remove(cache, "key1")
  assert_eq(Cache::size(cache), 0)
  let removed_value = Cache::get(cache, "key1")
  match removed_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试清空缓存
  Cache::set(cache, "key1", "value1")
  Cache::set(cache, "key2", "value2")
  assert_eq(Cache::size(cache), 2)
  
  Cache::clear(cache)
  assert_eq(Cache::size(cache), 0)
  assert_true(Cache::is_empty(cache))
}

test "LRU缓存策略测试" {
  // 创建LRU缓存
  let lru_cache = LRUCache::new(3) // 最大3个条目
  
  // 添加3个条目
  LRUCache::set(lru_cache, "key1", "value1")
  LRUCache::set(lru_cache, "key2", "value2")
  LRUCache::set(lru_cache, "key3", "value3")
  
  // 验证所有条目存在
  assert_eq(LRUCache::size(lru_cache), 3)
  match LRUCache::get(lru_cache, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 添加第4个条目，应淘汰最久未使用的key1
  LRUCache::set(lru_cache, "key4", "value4")
  
  // 验证缓存大小仍为3
  assert_eq(LRUCache::size(lru_cache), 3)
  
  // 验证key1被淘汰
  match LRUCache::get(lru_cache, "key1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 验证其他条目仍存在
  match LRUCache::get(lru_cache, "key2") {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  match LRUCache::get(lru_cache, "key3") {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  match LRUCache::get(lru_cache, "key4") {
    Some(v) => assert_eq(v, "value4")
    None => assert_true(false)
  }
  
  // 访问key2，使其成为最近使用
  LRUCache::get(lru_cache, "key2")
  
  // 添加第5个条目，应淘汰最久未使用的key3
  LRUCache::set(lru_cache, "key5", "value5")
  
  // 验证key3被淘汰
  match LRUCache::get(lru_cache, "key3") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 验证key2仍存在（因为最近被访问）
  match LRUCache::get(lru_cache, "key2") {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
}

test "TTL缓存策略测试" {
  // 创建TTL缓存
  let ttl_cache = TTLCache::new(100, 2000) // 最大100个条目，TTL为2秒
  
  // 添加条目
  TTLCache::set(ttl_cache, "key1", "value1")
  TTLCache::set(ttl_cache, "key2", "value2")
  
  // 验证条目存在
  match TTLCache::get(ttl_cache, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 等待TTL过期
  Time::sleep(2100) // 等待2.1秒
  
  // 验证条目已过期
  match TTLCache::get(ttl_cache, "key1") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match TTLCache::get(ttl_cache, "key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 添加新条目
  TTLCache::set(ttl_cache, "key3", "value3")
  
  // 立即访问，应存在
  match TTLCache::get(ttl_cache, "key3") {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // 刷新TTL
  TTLCache::refresh_ttl(ttl_cache, "key3")
  
  // 等待原TTL过期时间
  Time::sleep(2100) // 等待2.1秒
  
  // 验证条目仍存在（因为TTL已刷新）
  match TTLCache::get(ttl_cache, "key3") {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
}

test "缓存统计和监控测试" {
  // 创建带统计的缓存
  let stats_cache = StatsCache::new(100)
  
  // 初始统计
  let initial_stats = StatsCache::get_stats(stats_cache)
  assert_eq(initial_stats.hits, 0)
  assert_eq(initial_stats.misses, 0)
  assert_eq(initial_stats.sets, 0)
  assert_eq(initial_stats.evictions, 0)
  assert_eq(initial_stats.size, 0)
  
  // 添加条目
  StatsCache::set(stats_cache, "key1", "value1")
  StatsCache::set(stats_cache, "key2", "value2")
  
  let after_sets_stats = StatsCache::get_stats(stats_cache)
  assert_eq(after_sets_stats.sets, 2)
  assert_eq(after_sets_stats.size, 2)
  
  // 命中缓存
  StatsCache::get(stats_cache, "key1")
  StatsCache::get(stats_cache, "key2")
  
  let after_hits_stats = StatsCache::get_stats(stats_cache)
  assert_eq(after_hits_stats.hits, 2)
  assert_eq(after_hits_stats.misses, 0)
  
  // 未命中缓存
  StatsCache::get(stats_cache, "non_existent1")
  StatsCache::get(stats_cache, "non_existent2")
  
  let after_misses_stats = StatsCache::get_stats(stats_cache)
  assert_eq(after_misses_stats.hits, 2)
  assert_eq(after_misses_stats.misses, 2)
  
  // 计算命中率
  let hit_rate = StatsCache::get_hit_rate(stats_cache)
  assert_eq(hit_rate, 0.5) // 2命中 / (2命中 + 2未命中) = 0.5
  
  // 填满缓存以触发淘汰
  for i in 0..=100 {
    StatsCache::set(stats_cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  let after_evictions_stats = StatsCache::get_stats(stats_cache)
  assert_true(after_evictions_stats.evictions > 0)
  assert_eq(after_evictions_stats.size, 100) // 缓存大小不超过限制
}

test "多级缓存测试" {
  // 创建多级缓存
  let l1_cache = LRUCache::new(10) // 一级缓存，10个条目
  let l2_cache = LRUCache::new(100) // 二级缓存，100个条目
  let multi_cache = MultiLevelCache::new([l1_cache, l2_cache])
  
  // 添加条目到缓存
  MultiLevelCache::set(multi_cache, "key1", "value1")
  MultiLevelCache::set(multi_cache, "key2", "value2")
  
  // 验证条目在一级缓存中
  match LRUCache::get(l1_cache, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 获取条目（应从一级缓存获取）
  let value1 = MultiLevelCache::get(multi_cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 填满一级缓存
  for i in 0..=10 {
    MultiLevelCache::set(multi_cache, "l1_key_" + i.to_string(), "l1_value_" + i.to_string())
  }
  
  // 此时key1和key2应该被淘汰到二级缓存
  match LRUCache::get(l1_cache, "key1") {
    Some(_) => assert_true(false) // 不在一级缓存
    None => assert_true(true)
  }
  
  match LRUCache::get(l2_cache, "key1") {
    Some(v) => assert_eq(v, "value1") // 在二级缓存
    None => assert_true(false)
  }
  
  // 获取条目（应从二级缓存获取并提升到一级缓存）
  let value1 = MultiLevelCache::get(multi_cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 验证条目已提升到一级缓存
  match LRUCache::get(l1_cache, "key1") {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 获取多级缓存统计
  let stats = MultiLevelCache::get_stats(multi_cache)
  assert_eq(stats.l1_hits, 1)
  assert_eq(stats.l2_hits, 1)
  assert_eq(stats.total_hits, 2)
}

test "分布式缓存测试" {
  // 创建分布式缓存节点
  let node1 = DistributedCacheNode::new("node1", 6379)
  let node2 = DistributedCacheNode::new("node2", 6380)
  let node3 = DistributedCacheNode::new("node3", 6381)
  
  // 创建分布式缓存集群
  let cluster = DistributedCacheCluster::new([node1, node2, node3])
  
  // 添加条目到集群
  DistributedCacheCluster::set(cluster, "key1", "value1")
  DistributedCacheCluster::set(cluster, "key2", "value2")
  DistributedCacheCluster::set(cluster, "key3", "value3")
  
  // 验证条目分布
  let key1_node = DistributedCacheCluster::get_node_for_key(cluster, "key1")
  let key2_node = DistributedCacheCluster::get_node_for_key(cluster, "key2")
  let key3_node = DistributedCacheCluster::get_node_for_key(cluster, "key3")
  
  // 验证不同键可能分布到不同节点
  assert_true(key1_node != key2_node || key2_node != key3_node || key1_node != key3_node)
  
  // 从集群获取条目
  let value1 = DistributedCacheCluster::get(cluster, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 测试节点故障转移
  DistributedCacheCluster::simulate_node_failure(cluster, key1_node)
  
  // 重新设置条目（应路由到可用节点）
  DistributedCacheCluster::set(cluster, "key1", "new_value1")
  
  // 获取更新后的条目
  let new_value1 = DistributedCacheCluster::get(cluster, "key1")
  match new_value1 {
    Some(v) => assert_eq(v, "new_value1")
    None => assert_true(false)
  }
  
  // 恢复节点
  DistributedCacheCluster::recover_node(cluster, key1_node)
  
  // 获取集群统计
  let cluster_stats = DistributedCacheCluster::get_stats(cluster)
  assert_true(cluster_stats.total_keys > 0)
  assert_true(cluster_stats.active_nodes <= 3)
}

test "缓存预热测试" {
  // 创建缓存
  let cache = Cache::new(100)
  
  // 创建预热数据
  let warmup_data = []
  for i in 0..=50 {
    warmup_data = warmup_data + [("key_" + i.to_string(), "value_" + i.to_string())]
  }
  
  // 预热缓存
  Cache::warmup(cache, warmup_data)
  
  // 验证缓存已预热
  assert_eq(Cache::size(cache), 51)
  
  // 验证预热数据存在
  for i in 0..=50 {
    let key = "key_" + i.to_string()
    let expected_value = "value_" + i.to_string()
    let value = Cache::get(cache, key)
    match value {
      Some(v) => assert_eq(v, expected_value)
      None => assert_true(false)
    }
  }
  
  // 测试部分预热
  let partial_warmup_data = []
  for i in 0..=20 {
    partial_warmup_data = partial_warmup_data + [("partial_key_" + i.to_string(), "partial_value_" + i.to_string())]
  }
  
  Cache::warmup_partial(cache, partial_warmup_data)
  
  // 验证部分预热数据存在
  for i in 0..=20 {
    let key = "partial_key_" + i.to_string()
    let expected_value = "partial_value_" + i.to_string()
    let value = Cache::get(cache, key)
    match value {
      Some(v) => assert_eq(v, expected_value)
      None => assert_true(false)
    }
  }
}

test "缓存一致性测试" {
  // 创建缓存集群
  let cache1 = Cache::new(100)
  let cache2 = Cache::new(100)
  let cache3 = Cache::new(100)
  
  let cache_cluster = CacheCluster::new([cache1, cache2, cache3])
  
  // 设置缓存一致性策略
  CacheCluster::set_consistency_strategy(cache_cluster, ConsistencyStrategy::Eventual)
  
  // 添加条目到集群
  CacheCluster::set(cache_cluster, "key1", "value1")
  
  // 验证条目在所有节点中（最终一致性）
  let value1_node1 = Cache::get(cache1, "key1")
  let value1_node2 = Cache::get(cache2, "key1")
  let value1_node3 = Cache::get(cache3, "key1")
  
  // 至少有一个节点应该有该条目（最终一致性）
  assert_true(value1_node1.is_some() || value1_node2.is_some() || value1_node3.is_some())
  
  // 等待一致性同步
  Time::sleep(1000) // 等待1秒
  
  // 验证所有节点现在都有该条目
  let value1_node1_after = Cache::get(cache1, "key1")
  let value1_node2_after = Cache::get(cache2, "key1")
  let value1_node3_after = Cache::get(cache3, "key1")
  
  match value1_node1_after {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value1_node2_after {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  match value1_node3_after {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 更新条目
  CacheCluster::set(cache_cluster, "key1", "updated_value1")
  
  // 等待一致性同步
  Time::sleep(1000) // 等待1秒
  
  // 验证所有节点都有更新后的值
  let updated_value1_node1 = Cache::get(cache1, "key1")
  let updated_value1_node2 = Cache::get(cache2, "key1")
  let updated_value1_node3 = Cache::get(cache3, "key1")
  
  match updated_value1_node1 {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  match updated_value1_node2 {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  match updated_value1_node3 {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
}

test "缓存性能基准测试" {
  // 创建不同类型的缓存
  let basic_cache = Cache::new(1000)
  let lru_cache = LRUCache::new(1000)
  let ttl_cache = TTLCache::new(1000, 60000) // 1分钟TTL
  
  // 测试写入性能
  let start_time = Time::now()
  for i in 0..=10000 {
    Cache::set(basic_cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  let end_time = Time::now()
  let basic_write_time = end_time - start_time
  
  let start_time = Time::now()
  for i in 0..=10000 {
    LRUCache::set(lru_cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  let end_time = Time::now()
  let lru_write_time = end_time - start_time
  
  let start_time = Time::now()
  for i in 0..=10000 {
    TTLCache::set(ttl_cache, "key_" + i.to_string(), "value_" + i.to_string())
  }
  let end_time = Time::now()
  let ttl_write_time = end_time - start_time
  
  // 验证写入性能在合理范围内（小于1秒）
  assert_true(basic_write_time < 1000)
  assert_true(lru_write_time < 1000)
  assert_true(ttl_write_time < 1000)
  
  // 测试读取性能
  let start_time = Time::now()
  for i in 0..=10000 {
    Cache::get(basic_cache, "key_" + i.to_string())
  }
  let end_time = Time::now()
  let basic_read_time = end_time - start_time
  
  let start_time = Time::now()
  for i in 0..=10000 {
    LRUCache::get(lru_cache, "key_" + i.to_string())
  }
  let end_time = Time::now()
  let lru_read_time = end_time - start_time
  
  let start_time = Time::now()
  for i in 0..=10000 {
    TTLCache::get(ttl_cache, "key_" + i.to_string())
  }
  let end_time = Time::now()
  let ttl_read_time = end_time - start_time
  
  // 验证读取性能在合理范围内（小于500ms）
  assert_true(basic_read_time < 500)
  assert_true(lru_read_time < 500)
  assert_true(ttl_read_time < 500)
  
  // 生成性能报告
  let performance_report = CachePerformanceReport::generate([
    ("Basic Cache", basic_write_time, basic_read_time),
    ("LRU Cache", lru_write_time, lru_read_time),
    ("TTL Cache", ttl_write_time, ttl_read_time)
  ])
  
  // 验证性能报告包含必要信息
  assert_true(performance_report.contains("Basic Cache"))
  assert_true(performance_report.contains("LRU Cache"))
  assert_true(performance_report.contains("TTL Cache"))
  assert_true(performance_report.contains("Write Time"))
  assert_true(performance_report.contains("Read Time"))
}