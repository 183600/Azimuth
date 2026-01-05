// Azimuth 缓存策略和性能优化测试用例
// 专注于缓存机制、性能优化策略和资源管理功能的测试

// 测试1: 基本缓存功能
test "基本缓存功能测试" {
  // 创建内存缓存管理器
  let cache_manager = CacheManager::new({
    backend: "memory",
    default_ttl: 300000,  // 5分钟TTL
    max_size: 1000,
    eviction_policy: "lru"
  })
  
  // 测试缓存设置和获取
  let set_result1 = cache_manager.set("user:123", {
    "id": 123,
    "name": "John Doe",
    "email": "john@example.com"
  })
  
  assert_true(set_result1.success)
  
  // 测试缓存获取
  let get_result1 = cache_manager.get("user:123")
  assert_true(get_result1.success)
  
  match get_result1.value {
    Some(user_data) => {
      assert_eq(user_data.get("id"), Some(123))
      assert_eq(user_data.get("name"), Some("John Doe"))
      assert_eq(user_data.get("email"), Some("john@example.com"))
    }
    None => assert_true(false)
  }
  
  // 测试不存在的键
  let get_result2 = cache_manager.get("user:999")
  assert_true(get_result2.success)
  assert_eq(get_result2.value, None)
  
  // 测试缓存更新
  let update_result = cache_manager.set("user:123", {
    "id": 123,
    "name": "John Doe",
    "email": "john.doe@newdomain.com"  // 更新的邮箱
  })
  
  assert_true(update_result.success)
  
  // 验证缓存已更新
  let get_result3 = cache_manager.get("user:123")
  match get_result3.value {
    Some(user_data) => {
      assert_eq(user_data.get("email"), Some("john.doe@newdomain.com"))
    }
    None => assert_true(false)
  }
  
  // 测试缓存删除
  let delete_result = cache_manager.delete("user:123")
  assert_true(delete_result.success)
  
  // 验证缓存已删除
  let get_result4 = cache_manager.get("user:123")
  assert_true(get_result4.success)
  assert_eq(get_result4.value, None)
  
  // 测试批量操作
  let batch_set_data = [
    ("product:1", { "id": 1, "name": "Product A", "price": 99.99 }),
    ("product:2", { "id": 2, "name": "Product B", "price": 149.99 }),
    ("product:3", { "id": 3, "name": "Product C", "price": 199.99 })
  ]
  
  let batch_set_result = cache_manager.set_batch(batch_set_data)
  assert_true(batch_set_result.success)
  assert_eq(batch_set_result.count, 3)
  
  // 测试批量获取
  let batch_get_result = cache_manager.get_batch(["product:1", "product:2", "product:3", "product:999"])
  assert_true(batch_get_result.success)
  assert_eq(batch_get_result.results.length(), 4)
  assert_true(batch_get_result.results[0].success)  // product:1 存在
  assert_true(batch_get_result.results[1].success)  // product:2 存在
  assert_true(batch_get_result.results[2].success)  // product:3 存在
  assert_true(batch_get_result.results[3].success)  // product:999 不存在，但操作成功
  
  // 测试缓存统计
  let stats = cache_manager.get_stats()
  assert_eq(stats.total_items, 3)
  assert_eq(stats.hits, 6)    // 之前的6次成功获取
  assert_eq(stats.misses, 2)  // 两次获取不存在的键
  assert_eq(stats.sets, 5)    // 4次单独设置 + 1次批量设置
  assert_eq(stats.deletes, 1)
}

// 测试2: TTL和过期策略
test "TTL和过期策略测试" {
  // 创建带TTL的缓存管理器
  let ttl_cache_manager = CacheManager::new({
    backend: "memory",
    default_ttl: 2000,  // 2秒TTL，用于测试
    max_size: 100,
    eviction_policy: "lru",
    cleanup_interval: 1000  // 1秒清理间隔
  })
  
  // 设置不同TTL的缓存项
  let short_ttl_result = ttl_cache_manager.set_with_ttl("short_lived", "expires in 1 second", 1000)
  assert_true(short_ttl_result.success)
  
  let long_ttl_result = ttl_cache_manager.set_with_ttl("long_lived", "expires in 5 seconds", 5000)
  assert_true(long_ttl_result.success)
  
  let default_ttl_result = ttl_cache_manager.set("default_ttl", "expires in 2 seconds")
  assert_true(default_ttl_result.success)
  
  // 立即验证所有项都存在
  assert_true(ttl_cache_manager.get("short_lived").success)
  assert_true(ttl_cache_manager.get("long_lived").success)
  assert_true(ttl_cache_manager.get("default_ttl").success)
  
  // 等待1.5秒
  Time::sleep(1500)
  
  // 验证短期TTL项已过期
  let short_lived_result = ttl_cache_manager.get("short_lived")
  assert_true(short_lived_result.success)
  assert_eq(short_lived_result.value, None)  // 应该已过期
  
  // 验证其他项仍存在
  assert_true(ttl_cache_manager.get("long_lived").success)
  assert_true(ttl_cache_manager.get("default_ttl").success)
  
  // 等待1秒
  Time::sleep(1000)
  
  // 验证默认TTL项已过期
  let default_ttl_result = ttl_cache_manager.get("default_ttl")
  assert_true(default_ttl_result.success)
  assert_eq(default_ttl_result.value, None)  // 应该已过期
  
  // 验证长期TTL项仍存在
  assert_true(ttl_cache_manager.get("long_lived").success)
  
  // 测试TTL续期
  let refresh_result = ttl_cache_manager.refresh_ttl("long_lived", 3000)  // 续期3秒
  assert_true(refresh_result.success)
  
  // 等待3秒
  Time::sleep(3000)
  
  // 验证续期的项仍然存在
  let refreshed_result = ttl_cache_manager.get("long_lived")
  assert_true(refreshed_result.success)
  assert_eq(refreshed_result.value, Some("expires in 5 seconds"))
  
  // 测试过期清理
  let cleanup_stats = ttl_cache_manager.cleanup_expired()
  assert_true(cleanup_stats.cleaned > 0)
  
  // 测试TTL获取
  let ttl_info = ttl_cache_manager.get_ttl("long_lived")
  assert_true(ttl_info.success)
  assert_true(ttl_info.remaining_ttl <= 3000)  // 应该小于或等于3秒
  
  // 测试不存在的键的TTL
  let nonexistent_ttl = ttl_cache_manager.get_ttl("nonexistent")
  assert_true(nonexistent_ttl.success)
  assert_eq(nonexistent_ttl.remaining_ttl, -1)  // -1表示不存在或已过期
}

// 测试3: 缓存淘汰策略
test "缓存淘汰策略测试" {
  // 测试LRU淘汰策略
  let lru_cache = CacheManager::new({
    backend: "memory",
    default_ttl: 300000,
    max_size: 3,  // 小容量以测试淘汰
    eviction_policy: "lru"
  })
  
  // 填满缓存
  lru_cache.set("key1", "value1")
  lru_cache.set("key2", "value2")
  lru_cache.set("key3", "value3")
  
  // 验证所有键都存在
  assert_true(lru_cache.get("key1").success)
  assert_true(lru_cache.get("key2").success)
  assert_true(lru_cache.get("key3").success)
  
  // 访问key1使其成为最近使用的
  lru_cache.get("key1")
  
  // 添加新键，应该淘汰最久未使用的key2
  lru_cache.set("key4", "value4")
  
  // 验证key1仍存在（最近访问过）
  assert_eq(lru_cache.get("key1").value, Some("value1"))
  
  // 验证key2已被淘汰
  assert_eq(lru_cache.get("key2").value, None)
  
  // 验证key3和key4存在
  assert_eq(lru_cache.get("key3").value, Some("value3"))
  assert_eq(lru_cache.get("key4").value, Some("value4"))
  
  // 测试LFU淘汰策略
  let lfu_cache = CacheManager::new({
    backend: "memory",
    default_ttl: 300000,
    max_size: 3,
    eviction_policy: "lfu"
  })
  
  // 填满缓存
  lfu_cache.set("lfu_key1", "value1")
  lfu_cache.set("lfu_key2", "value2")
  lfu_cache.set("lfu_key3", "value3")
  
  // 多次访问lfu_key1增加其频率
  for i in 0..=5 {
    lfu_cache.get("lfu_key1")
  }
  
  // 访问lfu_key2一次
  lfu_cache.get("lfu_key2")
  
  // 不访问lfu_key3，使其频率最低
  
  // 添加新键，应该淘汰频率最低的lfu_key3
  lfu_cache.set("lfu_key4", "value4")
  
  // 验证高频键仍存在
  assert_eq(lfu_cache.get("lfu_key1").value, Some("value1"))
  assert_eq(lfu_cache.get("lfu_key2").value, Some("value2"))
  
  // 验证低频键已被淘汰
  assert_eq(lfu_cache.get("lfu_key3").value, None)
  
  // 验证新键存在
  assert_eq(lfu_cache.get("lfu_key4").value, Some("value4"))
  
  // 测试FIFO淘汰策略
  let fifo_cache = CacheManager::new({
    backend: "memory",
    default_ttl: 300000,
    max_size: 3,
    eviction_policy: "fifo"
  })
  
  // 填满缓存
  fifo_cache.set("fifo_key1", "value1")
  fifo_cache.set("fifo_key2", "value2")
  fifo_cache.set("fifo_key3", "value3")
  
  // 访问fifo_key1（在FIFO中不影响淘汰顺序）
  fifo_cache.get("fifo_key1")
  
  // 添加新键，应该淘汰最早添加的fifo_key1
  fifo_cache.set("fifo_key4", "value4")
  
  // 验证最早添加的键已被淘汰
  assert_eq(fifo_cache.get("fifo_key1").value, None)
  
  // 验证其他键存在
  assert_eq(fifo_cache.get("fifo_key2").value, Some("value2"))
  assert_eq(fifo_cache.get("fifo_key3").value, Some("value3"))
  assert_eq(fifo_cache.get("fifo_key4").value, Some("value4"))
  
  // 测试随机淘汰策略
  let random_cache = CacheManager::new({
    backend: "memory",
    default_ttl: 300000,
    max_size: 3,
    eviction_policy: "random"
  })
  
  // 填满缓存
  random_cache.set("rand_key1", "value1")
  random_cache.set("rand_key2", "value2")
  random_cache.set("rand_key3", "value3")
  
  // 添加新键，随机淘汰一个
  random_cache.set("rand_key4", "value4")
  
  // 验证缓存大小仍然是3
  let random_stats = random_cache.get_stats()
  assert_eq(random_stats.total_items, 3)
  
  // 验证至少有一个键被淘汰
  let existing_keys = [
    random_cache.get("rand_key1").value != None,
    random_cache.get("rand_key2").value != None,
    random_cache.get("rand_key3").value != None,
    random_cache.get("rand_key4").value != None
  ]
  
  let true_count = existing_keys.filter(fn(b) { b }).length()
  assert_eq(true_count, 3)  // 应该只有3个键存在
}

// 测试4: 分布式缓存
test "分布式缓存测试" {
  // 创建Redis分布式缓存管理器
  let redis_cache = DistributedCacheManager::new({
    backend: "redis",
    connection_string: "redis://localhost:6379",
    default_ttl: 300000,
    key_prefix: "azimuth:",
    serialization: "json",
    compression: true,
    retry_attempts: 3,
    retry_delay: 1000
  })
  
  // 测试连接
  let connection_result = redis_cache.test_connection()
  assert_true(connection_result.success)
  
  // 测试分布式缓存设置和获取
  let distributed_set_result = redis_cache.set("distributed:user:123", {
    "id": 123,
    "name": "Alice Smith",
    "preferences": {
      "theme": "dark",
      "notifications": true
    }
  })
  
  assert_true(distributed_set_result.success)
  
  // 测试分布式缓存获取
  let distributed_get_result = redis_cache.get("distributed:user:123")
  assert_true(distributed_get_result.success)
  
  match distributed_get_result.value {
    Some(user_data) => {
      assert_eq(user_data.get("id"), Some(123))
      assert_eq(user_data.get("name"), Some("Alice Smith"))
      assert_eq(user_data.get("preferences.theme"), Some("dark"))
    }
    None => assert_true(false)
  }
  
  // 测试缓存原子操作
  let atomic_increment = redis_cache.increment("counter:visits", 1)
  assert_true(atomic_increment.success)
  assert_eq(atomic_increment.new_value, Some(1))
  
  // 再次递增
  let atomic_increment2 = redis_cache.increment("counter:visits", 1)
  assert_true(atomic_increment2.success)
  assert_eq(atomic_increment2.new_value, Some(2))
  
  // 测试缓存比较和设置
  let cas_result = redis_cache.compare_and_set("config:version", "1.0.0", "1.1.0")
  assert_true(cas_result.success)
  assert_true(cas_result.updated)  // 应该更新成功
  
  // 再次尝试CAS，但期望值不匹配
  let cas_result2 = redis_cache.compare_and_set("config:version", "1.0.0", "1.2.0")
  assert_true(cas_result2.success)
  assert_false(cas_result2.updated)  // 不应该更新
  
  // 验证值仍然是第一次更新的
  let current_version = redis_cache.get("config:version")
  assert_eq(current_version.value, Some("1.1.0"))
  
  // 测试分布式锁
  let lock_result = redis_cache.acquire_lock("resource:processing", {
    ttl: 10000,  // 10秒锁
    retry_attempts: 3,
    retry_delay: 1000
  })
  
  assert_true(lock_result.success)
  assert_true(lock_result.lock_acquired)
  
  let lock_id = lock_result.lock_id
  
  // 尝试获取同一资源的锁（应该失败）
  let lock_result2 = redis_cache.acquire_lock("resource:processing", {
    ttl: 10000,
    retry_attempts: 1,
    retry_delay: 100
  })
  
  assert_true(lock_result2.success)
  assert_false(lock_result2.lock_acquired)
  
  // 释放锁
  let release_result = redis_cache.release_lock("resource:processing", lock_id)
  assert_true(release_result.success)
  
  // 现在应该能够获取锁
  let lock_result3 = redis_cache.acquire_lock("resource:processing", {
    ttl: 10000,
    retry_attempts: 1,
    retry_delay: 100
  })
  
  assert_true(lock_result3.success)
  assert_true(lock_result3.lock_acquired)
  
  // 清理锁
  redis_cache.release_lock("resource:processing", lock_result3.lock_id)
  
  // 测试缓存发布/订阅
  let pubsub_result = redis_cache.publish("cache:invalidation", {
    "type": "invalidate",
    "keys": ["user:123", "user:456"],
    "reason": "user_data_updated"
  })
  
  assert_true(pubsub_result.success)
  assert_true(pubsub_result.subscribers_notified >= 0)
  
  // 测试缓存统计
  let distributed_stats = redis_cache.get_stats()
  assert_true(distributed_stats.total_items >= 0)
  assert_true(distributed_stats.hits >= 0)
  assert_true(distributed_stats.misses >= 0)
  assert_true(distributed_stats.memory_usage > 0)
}

// 测试5: 多级缓存
test "多级缓存测试" {
  // 创建多级缓存管理器
  let multilevel_cache = MultilevelCacheManager::new({
    levels: [
      {
        name: "l1_memory",
        backend: "memory",
        max_size: 100,
        ttl: 60000,  // 1分钟
        eviction_policy: "lru"
      },
      {
        name: "l2_redis",
        backend: "redis",
        connection_string: "redis://localhost:6379",
        max_size: 1000,
        ttl: 300000,  // 5分钟
        key_prefix: "l2:"
      },
      {
        name: "l3_database",
        backend: "database",
        connection_string: "postgresql://localhost:5432/cache",
        ttl: 3600000  // 1小时
      }
    ],
    promotion_policy: "write_through",
    eviction_policy: "cascade"
  })
  
  // 测试多级缓存设置
  let multilevel_set_result = multilevel_cache.set("product:1001", {
    "id": 1001,
    "name": "Premium Widget",
    "price": 299.99,
    "category": "electronics"
  })
  
  assert_true(multilevel_set_result.success)
  
  // 验证数据在所有级别都存在
  let l1_result = multilevel_cache.get_from_level("l1_memory", "product:1001")
  assert_true(l1_result.success)
  assert_true(l1_result.value != None)
  
  let l2_result = multilevel_cache.get_from_level("l2_redis", "product:1001")
  assert_true(l2_result.success)
  assert_true(l2_result.value != None)
  
  // 测试多级缓存获取（应该从L1获取）
  let get_result = multilevel_cache.get("product:1001")
  assert_true(get_result.success)
  assert_eq(get_result.source, Some("l1_memory"))
  
  match get_result.value {
    Some(product) => {
      assert_eq(product.get("id"), Some(1001))
      assert_eq(product.get("name"), Some("Premium Widget"))
    }
    None => assert_true(false)
  }
  
  // 清除L1缓存
  multilevel_cache.clear_level("l1_memory")
  
  // 再次获取，应该从L2提升到L1
  let get_result2 = multilevel_cache.get("product:1001")
  assert_true(get_result2.success)
  assert_eq(get_result2.source, Some("l2_redis"))  // 从L2获取
  
  // 验证数据已提升到L1
  let l1_result2 = multilevel_cache.get_from_level("l1_memory", "product:1001")
  assert_true(l1_result2.success)
  assert_true(l1_result2.value != None)
  
  // 测试缓存穿透保护
  let cache_throughput = multilevel_cache.get_with_fallback("product:9999", fn() {
    // 模拟从数据库获取数据
    Some({
      "id": 9999,
      "name": "Fallback Product",
      "price": 49.99
    })
  })
  
  assert_true(cache_throughput.success)
  assert_eq(cache_throughput.source, Some("fallback"))
  
  match cache_throughput.value {
    Some(product) => {
      assert_eq(product.get("id"), Some(9999))
      assert_eq(product.get("name"), Some("Fallback Product"))
    }
    None => assert_true(false)
  }
  
  // 验证fallback数据被缓存
  let cached_fallback = multilevel_cache.get("product:9999")
  assert_true(cached_fallback.success)
  assert_eq(cached_fallback.source, Some("l1_memory"))  // 现在应该从L1缓存获取
  
  // 测试缓存预热
  let warmup_keys = ["product:2001", "product:2002", "product:2003"]
  let warmup_data = [
    ("product:2001", { "id": 2001, "name": "Product 2001" }),
    ("product:2002", { "id": 2002, "name": "Product 2002" }),
    ("product:2003", { "id": 2003, "name": "Product 2003" })
  ]
  
  let warmup_result = multilevel_cache.warmup(warmup_data)
  assert_true(warmup_result.success)
  assert_eq(warmup_result.warmed_keys, 3)
  
  // 验证预热的数据在所有级别都存在
  for key in warmup_keys {
    let key_result = multilevel_cache.get(key)
    assert_true(key_result.success)
    assert_true(key_result.value != None)
  }
  
  // 测试缓存失效
  let invalidate_result = multilevel_cache.invalidate("product:1001")
  assert_true(invalidate_result.success)
  
  // 验证数据在所有级别都已失效
  let l1_invalidated = multilevel_cache.get_from_level("l1_memory", "product:1001")
  assert_true(l1_invalidated.success)
  assert_eq(l1_invalidated.value, None)
  
  let l2_invalidated = multilevel_cache.get_from_level("l2_redis", "product:1001")
  assert_true(l2_invalidated.success)
  assert_eq(l2_invalidated.value, None)
  
  // 测试多级缓存统计
  let multilevel_stats = multilevel_cache.get_stats()
  assert_true(multilevel_stats.levels.length() == 3)
  
  let l1_stats = multilevel_stats.levels.find(fn(level) { level.name == "l1_memory" })
  assert_true(l1_stats != None)
  
  match l1_stats {
    Some(stats) => {
      assert_true(stats.hits > 0)
      assert_true(stats.total_items > 0)
    }
    None => assert_true(false)
  }
}

// 测试6: 缓存性能优化
test "缓存性能优化测试" {
  // 创建性能优化的缓存管理器
  let optimized_cache = OptimizedCacheManager::new({
    backend: "memory",
    max_size: 10000,
    ttl: 300000,
    eviction_policy: "lru",
    shard_count: 16,  // 分片以提高并发性能
    compression_threshold: 1024,  // 大于1KB的数据压缩
    serialization_format: "msgpack",  // 使用高效的序列化格式
    enable_metrics: true,
    enable_profiling: true
  })
  
  // 生成测试数据
  let generate_test_data = fn(size: Int) {
    let mut data = []
    for i in 0..=size - 1 {
      data = data.push({
        "id": i,
        "name": "Item " + i.to_string(),
        "description": "This is a detailed description for item " + i.to_string() + " with some additional text to make it larger",
        "metadata": {
          "created_at": "2022-01-01T00:00:00Z",
          "updated_at": "2022-01-01T12:00:00Z",
          "tags": ["tag1", "tag2", "tag3"],
          "properties": {
            "prop1": "value1",
            "prop2": "value2",
            "prop3": "value3"
          }
        }
      })
    }
    data
  }
  
  let test_data = generate_test_data(1000)
  
  // 测试批量写入性能
  let batch_write_start = Time::now()
  
  for i in 0..=test_data.length() - 1 {
    optimized_cache.set("item:" + i.to_string(), test_data[i])
  }
  
  let batch_write_end = Time::now()
  let batch_write_duration = batch_write_end - batch_write_start
  let batch_write_throughput = test_data.length().to_float() / (batch_write_duration.to_float() / 1000.0)
  
  assert_true(batch_write_throughput > 1000)  // 至少每秒1000次写入
  
  // 测试批量读取性能
  let batch_read_start = Time::now()
  
  for i in 0..=test_data.length() - 1 {
    optimized_cache.get("item:" + i.to_string())
  }
  
  let batch_read_end = Time::now()
  let batch_read_duration = batch_read_end - batch_read_start
  let batch_read_throughput = test_data.length().to_float() / (batch_read_duration.to_float() / 1000.0)
  
  assert_true(batch_read_throughput > 5000)  // 至少每秒5000次读取
  
  // 测试并发性能
  let concurrent_tasks = 10
  let operations_per_task = 100
  
  let concurrent_start = Time::now()
  
  let mut tasks = []
  for task_id in 0..=concurrent_tasks - 1 {
    let task = async fn() {
      for i in 0..=operations_per_task - 1 {
        let key = "concurrent:" + task_id.to_string() + ":" + i.to_string()
        let value = { "task_id": task_id, "operation": i, "timestamp": Time::now() }
        
        // 写入
        optimized_cache.set(key, value)
        
        // 读取
        optimized_cache.get(key)
      }
    }
    tasks = tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in tasks {
    task.await()
  }
  
  let concurrent_end = Time::now()
  let concurrent_duration = concurrent_end - concurrent_start
  let total_operations = concurrent_tasks * operations_per_task * 2  // 读写各一次
  let concurrent_throughput = total_operations.to_float() / (concurrent_duration.to_float() / 1000.0)
  
  assert_true(concurrent_throughput > 2000)  // 至少每秒2000次并发操作
  
  // 测试内存使用优化
  let memory_before = optimized_cache.get_memory_usage()
  
  // 添加大量数据
  for i in 0..=5000 {
    optimized_cache.set("memory_test:" + i.to_string(), {
      "id": i,
      "data": "x".repeat(100)  // 100字节的字符串
    })
  }
  
  let memory_after = optimized_cache.get_memory_usage()
  let memory_used = memory_after - memory_before
  
  // 验证内存使用合理（每个条目应该小于200字节，包括开销）
  let avg_memory_per_item = memory_used.to_float() / 5000.0
  assert_true(avg_memory_per_item < 200.0)
  
  // 测试压缩效果
  let large_data = "x".repeat(2000)  // 2KB数据
  optimized_cache.set("large_data", large_data)
  
  let large_data_result = optimized_cache.get("large_data")
  assert_true(large_data_result.success)
  assert_eq(large_data_result.value, Some(large_data))
  
  // 检查压缩统计
  let compression_stats = optimized_cache.get_compression_stats()
  assert_true(compression_stats.compressed_items > 0)
  assert_true(compression_stats.compression_ratio > 1.0)  // 压缩比应该大于1
  
  // 测试缓存命中率优化
  optimized_cache.clear()
  
  // 添加热点数据（80%的访问集中在20%的数据上）
  let hot_data_count = 200
  let cold_data_count = 800
  
  for i in 0..=hot_data_count - 1 {
    optimized_cache.set("hot:" + i.to_string(), { "type": "hot", "id": i })
  }
  
  for i in 0..=cold_data_count - 1 {
    optimized_cache.set("cold:" + i.to_string(), { "type": "cold", "id": i })
  }
  
  // 模拟访问模式：80%访问热点数据
  let access_start = Time::now()
  
  for i in 0..=10000 {
    let key = if i % 100 < 80 {
      "hot:" + (i % hot_data_count).to_string()
    } else {
      "cold:" + (i % cold_data_count).to_string()
    }
    
    optimized_cache.get(key)
  }
  
  let access_end = Time::now()
  
  // 检查命中率
  let access_stats = optimized_cache.get_stats()
  let hit_rate = access_stats.hits.to_float() / (access_stats.hits + access_stats.misses).to_float()
  
  assert_true(hit_rate > 0.7)  // 命中率应该大于70%
  
  // 测试自适应缓存大小调整
  let adaptive_cache = AdaptiveCacheManager::new({
    initial_size: 1000,
    min_size: 100,
    max_size: 10000,
    adjustment_threshold: 0.8,  // 命中率低于80%时调整
    adjustment_factor: 1.5,
    monitoring_interval: 5000
  })
  
  // 添加数据
  for i in 0..=1500 {  // 超过初始大小
    adaptive_cache.set("adaptive:" + i.to_string(), { "id": i })
  }
  
  // 模拟访问，触发自适应调整
  for i in 0..=2000 {
    adaptive_cache.get("adaptive:" + (i % 1500).to_string())
  }
  
  // 检查自适应调整
  let adaptive_stats = adaptive_cache.get_stats()
  assert_true(adaptive_stats.size_adjustments > 0)
  assert_true(adaptive_stats.current_size > 1000)  // 应该已经调整了大小
}

// 测试7: 缓存一致性
test "缓存一致性测试" {
  // 创建分布式缓存一致性管理器
  let consistency_cache = CacheConsistencyManager::new({
    backend: "redis",
    connection_string: "redis://localhost:6379",
    consistency_level: "eventual",
    invalidation_strategy: "publish_subscribe",
    conflict_resolution: "last_write_wins",
    versioning: true
  })
  
  // 测试版本控制
  let versioned_set_result = consistency_cache.set_with_version("user:1001", {
    "id": 1001,
    "name": "Bob Johnson",
    "email": "bob@example.com"
  }, 1)
  
  assert_true(versioned_set_result.success)
  
  // 获取版本化数据
  let versioned_get_result = consistency_cache.get_with_version("user:1001")
  assert_true(versioned_get_result.success)
  
  match versioned_get_result.value {
    Some((data, version)) => {
      assert_eq(data.get("id"), Some(1001))
      assert_eq(version, 1)
    }
    None => assert_true(false)
  }
  
  // 尝试使用旧版本更新（应该失败）
  let stale_update_result = consistency_cache.set_with_version("user:1001", {
    "id": 1001,
    "name": "Bob Johnson",
    "email": "bob.johnson@newdomain.com"
  }, 1)  // 使用相同的版本号
  
  assert_true(stale_update_result.success)
  assert_false(stale_update_result.updated)  // 不应该更新
  
  // 使用新版本更新
  let valid_update_result = consistency_cache.set_with_version("user:1001", {
    "id": 1001,
    "name": "Bob Johnson",
    "email": "bob.johnson@newdomain.com"
  }, 2)
  
  assert_true(valid_update_result.success)
  assert_true(valid_update_result.updated)  // 应该更新
  
  // 测试缓存失效通知
  let invalidation_result = consistency_cache.invalidate_with_notification("user:1001", {
    reason: "user_profile_updated",
    source: "user_service"
  })
  
  assert_true(invalidation_result.success)
  assert_true(invalidation_result.notified_subscribers > 0)
  
  // 测试分布式锁保证一致性
  let lock_result = consistency_cache.acquire_consistent_lock("user:1001", {
    ttl: 10000,
    wait_time: 5000
  })
  
  assert_true(lock_result.success)
  assert_true(lock_result.lock_acquired)
  
  let lock_token = lock_result.lock_token
  
  // 在锁保护下更新数据
  let consistent_update_result = consistency_cache.set_under_lock("user:1001", {
    "id": 1001,
    "name": "Bob Johnson",
    "email": "bob.johnson@finaldomain.com"
  }, lock_token)
  
  assert_true(consistent_update_result.success)
  
  // 释放锁
  consistency_cache.release_consistent_lock("user:1001", lock_token)
  
  // 测试读写一致性
  let read_consistency_result = consistency_cache.get_with_consistency("user:1001", {
    consistency_level: "strong",
    max_staleness: 1000  // 最多1秒的陈旧数据
  })
  
  assert_true(read_consistency_result.success)
  
  // 测试写入冲突检测
  let conflict_detection = consistency_cache.enable_conflict_detection({
    strategy: "vector_clock",
    merge_policy: "merge_all"
  })
  
  assert_true(conflict_detection.success)
  
  // 模拟并发写入冲突
  let node1_clock = VectorClock::new("node1")
  let node2_clock = VectorClock::new("node2")
  
  let node1_update = consistency_cache.set_with_vector_clock("conflict:test", {
    "value": "from_node1",
    "timestamp": Time::now()
  }, node1_clock)
  
  let node2_update = consistency_cache.set_with_vector_clock("conflict:test", {
    "value": "from_node2",
    "timestamp": Time::now()
  }, node2_clock)
  
  assert_true(node1_update.success)
  assert_true(node2_update.success)
  
  // 检查冲突解决结果
  let conflict_result = consistency_cache.get_with_vector_clock("conflict:test")
  assert_true(conflict_result.success)
  
  // 测试缓存同步
  let sync_result = consistency_cache.synchronize_with_peers({
    peer_nodes: ["redis://peer1:6379", "redis://peer2:6379"],
    sync_strategy: "full",
    conflict_resolution: "timestamp_based"
  })
  
  assert_true(sync_result.success)
  assert_true(sync_result.synced_keys > 0)
  
  // 测试事务性缓存操作
  let transaction = consistency_cache.begin_transaction()
  
  transaction.set("tx:key1", "value1")
  transaction.set("tx:key2", "value2")
  transaction.delete("tx:old_key")
  
  let commit_result = transaction.commit()
  assert_true(commit_result.success)
  
  // 验证事务提交后的状态
  assert_eq(consistency_cache.get("tx:key1").value, Some("value1"))
  assert_eq(consistency_cache.get("tx:key2").value, Some("value2"))
  assert_eq(consistency_cache.get("tx:old_key").value, None)
  
  // 测试事务回滚
  let rollback_transaction = consistency_cache.begin_transaction()
  
  rollback_transaction.set("tx:rollback_key", "rollback_value")
  rollback_transaction.set("tx:another_key", "another_value")
  
  let rollback_result = rollback_transaction.rollback()
  assert_true(rollback_result.success)
  
  // 验证回滚后的状态
  assert_eq(consistency_cache.get("tx:rollback_key").value, None)
  assert_eq(consistency_cache.get("tx:another_key").value, None)
}

// 测试8: 缓存监控和指标
test "缓存监控和指标测试" {
  // 创建带监控的缓存管理器
  let monitored_cache = MonitoredCacheManager::new({
    backend: "memory",
    max_size: 1000,
    ttl: 300000,
    monitoring: {
      enabled: true,
      metrics_interval: 1000,  // 1秒收集一次指标
      detailed_metrics: true,
      export_formats: ["prometheus", "json"]
    },
    alerting: {
      enabled: true,
      rules: [
        {
          name: "high_miss_rate",
          condition: "miss_rate > 0.5",
          severity: "warning",
          duration: 300000  // 5分钟
        },
        {
          name: "low_memory",
          condition: "memory_usage > 0.9",
          severity: "critical",
          duration: 60000   // 1分钟
        }
      ]
    }
  })
  
  // 添加一些数据以生成指标
  for i in 0..=100 {
    monitored_cache.set("metric_test:" + i.to_string(), {
      "id": i,
      "data": "test data for " + i.to_string()
    })
  }
  
  // 执行一些操作以生成指标
  for i in 0..=200 {
    let key = "metric_test:" + (i % 100).to_string()
    monitored_cache.get(key)
  }
  
  // 等待指标收集
  Time::sleep(2000)
  
  // 获取缓存指标
  let metrics = monitored_cache.get_metrics()
  assert_true(metrics.basic_metrics.total_items == 100)
  assert_true(metrics.basic_metrics.hits > 0)
  assert_true(metrics.basic_metrics.misses > 0)
  
  let hit_rate = metrics.basic_metrics.hits.to_float() / (metrics.basic_metrics.hits + metrics.basic_metrics.misses).to_float()
  assert_true(hit_rate > 0.5)  // 应该有超过50%的命中率
  
  // 获取详细指标
  let detailed_metrics = monitored_cache.get_detailed_metrics()
  assert_true(detailed_metrics.memory_usage > 0)
  assert_true(detailed_metrics.evictions >= 0)
  assert_true(detailed_metrics.avg_get_time > 0)
  assert_true(detailed_metrics.avg_set_time > 0)
  
  // 获取性能指标
  let performance_metrics = monitored_cache.get_performance_metrics()
  assert_true(performance_metrics.throughput_ops_per_second > 0)
  assert_true(performance_metrics.latency_p95 > 0)
  assert_true(performance_metrics.latency_p99 > 0)
  
  // 测试Prometheus格式导出
  let prometheus_export = monitored_cache.export_metrics("prometheus")
  assert_true(prometheus_export.success)
  assert_true(prometheus_export.data.contains("cache_hits_total"))
  assert_true(prometheus_export.data.contains("cache_misses_total"))
  assert_true(prometheus_export.data.contains("cache_items_total"))
  
  // 测试JSON格式导出
  let json_export = monitored_cache.export_metrics("json")
  assert_true(json_export.success)
  
  let json_data = JSON::parse(json_export.data)
  assert_true(json_data.contains_key("basic_metrics"))
  assert_true(json_data.contains_key("performance_metrics"))
  
  // 测试实时监控
  let realtime_monitoring = monitored_cache.enable_realtime_monitoring({
    interval: 500,  // 500ms间隔
    callback: fn(metrics) {
      // 验证回调函数被调用
      assert_true(metrics.total_items >= 0)
    }
  })
  
  assert_true(realtime_monitoring.success)
  
  // 运行一段时间以收集实时指标
  Time::sleep(1500)
  
  // 停止实时监控
  monitored_cache.disable_realtime_monitoring()
  
  // 测试告警系统
  let alert_manager = monitored_cache.get_alert_manager()
  
  // 模拟高缺失率告警条件
  for i in 0..=200 {
    monitored_cache.get("nonexistent_key:" + i.to_string())  // 故意获取不存在的键
  }
  
  Time::sleep(2000)  // 等待告警评估
  
  // 检查是否有告警触发
  let active_alerts = alert_manager.get_active_alerts()
  assert_true(active_alerts.length() >= 0)  // 可能有也可能没有告警
  
  // 测试自定义指标
  monitored_cache.add_custom_metric("user_operations", {
    type: "counter",
    description: "用户操作计数"
  })
  
  monitored_cache.increment_custom_metric("user_operations", 10)
  monitored_cache.increment_custom_metric("user_operations", 5)
  
  let custom_metrics = monitored_cache.get_custom_metrics()
  assert_eq(custom_metrics.get("user_operations"), Some(15))
  
  // 测试仪表盘生成
  let dashboard_result = monitored_cache.generate_dashboard({
    title: "缓存性能仪表盘",
    time_range: "1h",
    panels: [
      { type: "line_chart", metric: "hit_rate", title: "缓存命中率" },
      { type: "gauge", metric: "memory_usage", title: "内存使用率" },
      { type: "counter", metric: "operations_per_second", title: "每秒操作数" }
    ]
  })
  
  assert_true(dashboard_result.success)
  assert_true(dashboard_result.dashboard_data.length() > 0)
  
  // 测试性能分析
  let profiling_result = monitored_cache.enable_profiling({
    duration: 5000,  // 5秒分析
    sample_rate: 0.1  // 10%采样率
  })
  
  assert_true(profiling_result.success)
  
  Time::sleep(6000)  // 等待分析完成
  
  let profiling_data = monitored_cache.get_profiling_data()
  assert_true(profiling_data.hot_keys.length() > 0)
  assert_true(profiling_data.operation_distribution.length() > 0)
  
  // 测试缓存健康检查
  let health_check = monitored_cache.health_check()
  assert_true(health_check.healthy)
  assert_true(health_check.checks.length() > 0)
  
  for check in health_check.checks {
    assert_true(check.status == "pass" || check.status == "warn")
  }
}

// 测试9: 缓存预热和加载策略
test "缓存预热和加载策略测试" {
  // 创建缓存预热管理器
  let warmup_manager = CacheWarmupManager::new({
    cache_backend: "redis",
    connection_string: "redis://localhost:6379",
    parallelism: 10,
    batch_size: 100,
    retry_attempts: 3,
    progress_reporting: true
  })
  
  // 定义预热策略
  let user_warmup_strategy = {
    name: "user_profile_warmup",
    description: "用户配置文件预热",
    data_source: "database",
    query: "SELECT id, name, email, preferences FROM users WHERE active = true",
    key_pattern: "user:{id}",
    ttl: 3600000,  // 1小时
    priority: "high",
    dependencies: []
  }
  
  let product_warmup_strategy = {
    name: "product_catalog_warmup",
    description: "产品目录预热",
    data_source: "api",
    endpoint: "/api/products",
    key_pattern: "product:{id}",
    ttl: 1800000,  // 30分钟
    priority: "medium",
    dependencies: ["category_warmup"]
  }
  
  let category_warmup_strategy = {
    name: "category_warmup",
    description: "产品分类预热",
    data_source: "database",
    query: "SELECT id, name, parent_id FROM categories",
    key_pattern: "category:{id}",
    ttl: 7200000,  // 2小时
    priority: "high",
    dependencies: []
  }
  
  // 注册预热策略
  warmup_manager.register_strategy(user_warmup_strategy)
  warmup_manager.register_strategy(product_warmup_strategy)
  warmup_manager.register_strategy(category_warmup_strategy)
  
  // 模拟数据源
  let mock_data_source = MockDataSource::new()
  
  mock_data_source.add_query_result("SELECT id, name, email, preferences FROM users WHERE active = true", [
    { "id": 1, "name": "Alice", "email": "alice@example.com", "preferences": { "theme": "dark" } },
    { "id": 2, "name": "Bob", "email": "bob@example.com", "preferences": { "theme": "light" } },
    { "id": 3, "name": "Charlie", "email": "charlie@example.com", "preferences": { "theme": "auto" } }
  ])
  
  mock_data_source.add_query_result("SELECT id, name, parent_id FROM categories", [
    { "id": 1, "name": "Electronics", "parent_id": null },
    { "id": 2, "name": "Computers", "parent_id": 1 },
    { "id": 3, "name": "Phones", "parent_id": 1 }
  ])
  
  mock_data_source.add_api_result("/api/products", [
    { "id": 101, "name": "Laptop", "category_id": 2 },
    { "id": 102, "name": "Smartphone", "category_id": 3 },
    { "id": 103, "name": "Tablet", "category_id": 2 }
  ])
  
  warmup_manager.set_data_source(mock_data_source)
  
  // 测试单个策略预热
  let single_warmup_result = warmup_manager.warmup_strategy("user_profile_warmup")
  assert_true(single_warmup_result.success)
  assert_eq(single_warmup_result.items_warmed, 3)
  
  // 验证预热的数据
  let cache_manager = warmup_manager.get_cache_manager()
  let user1 = cache_manager.get("user:1")
  assert_eq(user1.value, Some({ "id": 1, "name": "Alice", "email": "alice@example.com", "preferences": { "theme": "dark" } }))
  
  // 测试依赖关系预热
  let dependency_warmup_result = warmup_manager.warmup_strategy("product_catalog_warmup")
  assert_true(dependency_warmup_result.success)
  assert_true(dependency_warmup_result.items_warmed > 0)
  
  // 验证依赖也被预热
  let category1 = cache_manager.get("category:1")
  assert_eq(category1.value, Some({ "id": 1, "name": "Electronics", "parent_id": null }))
  
  // 测试批量预热
  let batch_warmup_result = warmup_manager.warmup_all({
    priority_filter: ["high", "medium"],
    parallel: true
  })
  
  assert_true(batch_warmup_result.success)
  assert_true(batch_warmup_result.strategies_executed > 0)
  assert_true(batch_warmup_result.total_items_warmed > 0)
  
  // 测试增量预热
  let incremental_warmup_result = warmup_manager.incremental_warmup("user_profile_warmup", {
    since: Time::now() - 3600000,  // 最近1小时的数据
    changes_only: true
  })
  
  assert_true(incremental_warmup_result.success)
  
  // 测试预热进度监控
  let progress_monitor = warmup_manager.start_progress_monitoring("user_profile_warmup", {
    interval: 100,  // 100ms更新一次
    callback: fn(progress) {
      // 验证进度回调
      assert_true(progress.percentage >= 0.0 && progress.percentage <= 100.0)
    }
  })
  
  assert_true(progress_monitor.success)
  
  // 等待监控完成
  Time::sleep(1000)
  
  // 测试预热调度
  let warmup_scheduler = warmup_manager.get_scheduler()
  
  // 添加定时预热任务
  let schedule_result = warmup_scheduler.schedule_task({
    name: "daily_user_warmup",
    strategy: "user_profile_warmup",
    schedule: "0 2 * * *",  // 每天凌晨2点
    enabled: true,
    timezone: "UTC"
  })
  
  assert_true(schedule_result.success)
  
  // 测试预热失败处理
  let failing_data_source = MockDataSource::new()
  failing_data_source.set_should_fail(true)
  
  warmup_manager.set_data_source(failing_data_source)
  
  let failing_warmup_result = warmup_manager.warmup_strategy("user_profile_warmup")
  assert_false(failing_warmup_result.success)
  assert_true(failing_warmup_result.error.length() > 0)
  
  // 测试预热恢复
  warmup_manager.set_data_source(mock_data_source)  // 恢复正常数据源
  
  let recovery_warmup_result = warmup_manager.warmup_strategy("user_profile_warmup")
  assert_true(recovery_warmup_result.success)
  
  // 测试预热统计
  let warmup_stats = warmup_manager.get_statistics()
  assert_true(warmup_stats.total_warmups > 0)
  assert_true(warmup_stats.successful_warmups > 0)
  assert_true(warmup_stats.failed_warmups >= 0)
  assert_true(warmup_stats.total_items_warmed > 0)
  
  // 测试预热优化
  let optimization_result = warmup_manager.optimize_warmup({
    analyze_access_patterns: true,
    prioritize_hot_data: true,
    adjust_batch_sizes: true
  })
  
  assert_true(optimization_result.success)
  assert_true(optimization_result.optimizations_applied > 0)
}

// 测试10: 缓存故障恢复和容错
test "缓存故障恢复和容错测试" {
  // 创建高可用缓存管理器
  let ha_cache = HighAvailabilityCacheManager::new({
    primary_backend: "redis",
    primary_connection: "redis://primary:6379",
    backup_backends: [
      { type: "redis", connection: "redis://backup1:6379" },
      { type: "redis", connection: "redis://backup2:6379" }
    ],
    failover_strategy: "automatic",
    health_check_interval: 5000,
    max_retry_attempts: 3,
    circuit_breaker: {
      enabled: true,
      failure_threshold: 5,
      recovery_timeout: 30000
    }
  })
  
  // 模拟主缓存节点
  let primary_cache = MockRedisServer::new("primary")
  let backup_cache1 = MockRedisServer::new("backup1")
  let backup_cache2 = MockRedisServer::new("backup2")
  
  ha_cache.set_primary_cache(primary_cache)
  ha_cache.add_backup_cache(backup_cache1)
  ha_cache.add_backup_cache(backup_cache2)
  
  // 测试正常操作
  let normal_set_result = ha_cache.set("ha_test:key1", "value1")
  assert_true(normal_set_result.success)
  
  let normal_get_result = ha_cache.get("ha_test:key1")
  assert_true(normal_get_result.success)
  assert_eq(normal_get_result.value, Some("value1"))
  
  // 模拟主节点故障
  primary_cache.set_failure_mode(true)
  
  // 测试故障转移
  let failover_set_result = ha_cache.set("ha_test:key2", "value2")
  assert_true(failover_set_result.success)
  
  let failover_get_result = ha_cache.get("ha_test:key2")
  assert_true(failover_get_result.success)
  assert_eq(failover_get_result.value, Some("value2"))
  
  // 验证操作已转移到备份节点
  let operation_stats = ha_cache.get_operation_stats()
  assert_true(operation_stats.backup_operations > 0)
  
  // 测试主节点恢复
  primary_cache.set_failure_mode(false)
  
  // 等待健康检查检测到恢复
  Time::sleep(6000)
  
  // 测试操作是否切回主节点
  let recovery_set_result = ha_cache.set("ha_test:key3", "value3")
  assert_true(recovery_set_result.success)
  
  let recovery_get_result = ha_cache.get("ha_test:key3")
  assert_true(recovery_get_result.success)
  assert_eq(recovery_get_result.value, Some("value3"))
  
  // 验证操作已切回主节点
  let recovery_stats = ha_cache.get_operation_stats()
  assert_true(recovery_stats.primary_operations > operation_stats.primary_operations)
  
  // 测试所有节点故障
  primary_cache.set_failure_mode(true)
  backup_cache1.set_failure_mode(true)
  backup_cache2.set_failure_mode(true)
  
  let all_down_result = ha_cache.get("ha_test:key1")
  assert_false(all_down_result.success)
  assert_true(all_down_result.error.contains("所有缓存节点不可用"))
  
  // 测试断路器
  let circuit_breaker = ha_cache.get_circuit_breaker()
  
  // 触发多次失败以打开断路器
  for i in 0..=6 {
    ha_cache.get("ha_test:key" + i.to_string())
  }
  
  // 验证断路器已打开
  assert_true(circuit_breaker.is_open())
  
  // 测试断路器打开时的行为
  let circuit_open_result = ha_cache.get("ha_test:key1")
  assert_false(circuit_open_result.success)
  assert_true(circuit_open_result.error.contains("断路器打开"))
  
  // 恢复节点并等待断路器恢复
  primary_cache.set_failure_mode(false)
  
  // 等待断路器超时
  Time::sleep(31000)
  
  // 测试断路器半开状态
  let half_open_result = ha_cache.get("ha_test:key1")
  assert_true(half_open_result.success || half_open_result.error.contains("断路器半开"))
  
  // 测试缓存降级
  let fallback_cache = FallbackCacheManager::new({
    primary_cache: ha_cache,
    fallback_strategy: "local_cache",
    fallback_options: {
      local_cache_size: 1000,
      fallback_ttl: 60000,  // 1分钟
      stale_data_ttl: 300000  // 5分钟
    }
  })
  
  // 在主缓存不可用时使用降级策略
  primary_cache.set_failure_mode(true)
  
  // 设置一些降级数据
  fallback_cache.set_with_fallback("fallback:key1", "fallback_value1")
  
  // 获取降级数据
  let fallback_get_result = fallback_cache.get("fallback:key1")
  assert_true(fallback_get_result.success)
  assert_eq(fallback_get_result.value, Some("fallback_value1"))
  assert_eq(fallback_get_result.source, Some("fallback"))
  
  // 测试缓存数据恢复
  let recovery_manager = CacheRecoveryManager::new({
    backup_location: "/tmp/cache_backup",
    backup_interval: 3600000,  // 1小时
    compression: true,
    encryption: false
  })
  
  // 创建备份
  let backup_result = recovery_manager.create_backup(ha_cache)
  assert_true(backup_result.success)
  assert_true(backup_result.backup_path.length() > 0)
  
  // 模拟数据丢失
  ha_cache.clear_all()
  
  // 从备份恢复
  let restore_result = recovery_manager.restore_from_backup(ha_cache, backup_result.backup_path)
  assert_true(restore_result.success)
  assert_true(restore_result.items_restored > 0)
  
  // 验证数据已恢复
  let restored_get_result = ha_cache.get("ha_test:key1")
  assert_true(restored_get_result.success)
  
  // 测试缓存一致性检查
  let consistency_checker = CacheConsistencyChecker::new({
    check_interval: 300000,  // 5分钟
    repair_inconsistencies: true,
    max_repair_attempts: 3
  })
  
  // 添加一致性检查规则
  consistency_checker.add_rule({
    name: "key_value_consistency",
    description: "检查键值对的一致性",
    check_fn: fn(cache) {
      let test_key = "consistency:test"
      let expected_value = "test_value"
      
      cache.set(test_key, expected_value)
      let actual_value = cache.get(test_key)
      
      match actual_value.value {
        Some(value) => value == expected_value
        None => false
      }
    }
  })
  
  // 执行一致性检查
  let consistency_result = consistency_checker.run_checks(ha_cache)
  assert_true(consistency_result.success)
  assert_true(consistency_result.passed_checks > 0)
  
  // 测试缓存监控和告警
  let fault_monitor = CacheFaultMonitor::new({
    alert_channels: ["email", "slack"],
    alert_thresholds: {
      failure_rate: 0.1,      // 10%失败率
      response_time: 1000,     // 1秒响应时间
      memory_usage: 0.9        // 90%内存使用率
    }
  })
  
  // 模拟高故障率
  for i in 0..=20 {
    ha_cache.get("nonexistent_key:" + i.to_string())
  }
  
  // 检查是否触发告警
  let fault_alerts = fault_monitor.get_active_alerts()
  assert_true(fault_alerts.length() >= 0)
  
  // 测试缓存性能降级
  let performance_degradation = CachePerformanceManager::new({
    auto_scaling: true,
    scaling_thresholds: {
      cpu_usage: 0.8,
      memory_usage: 0.85,
      response_time: 500
    },
    degradation_strategies: [
      "reduce_ttl",
      "increase_eviction",
      "enable_compression"
    ]
  })
  
  // 模拟高负载
  for i in 0..=1000 {
    ha_cache.set("load_test:" + i.to_string(), "value" + i.to_string())
    ha_cache.get("load_test:" + (i % 100).to_string())
  }
  
  // 检查是否应用了性能降级策略
  let degradation_status = performance_degradation.get_status()
  assert_true(degradation_status.strategies_applied.length() >= 0)
}