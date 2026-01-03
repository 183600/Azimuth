// 缓存机制测试
// 测试Azimuth遥测系统的缓存机制功能

test "内存缓存基础操作" {
  // 测试内存缓存基础操作
  let memory_cache = MemoryCache::new()
  
  // 配置缓存
  MemoryCache::set_max_size(memory_cache, 1000)
  MemoryCache::set_ttl(memory_cache, 60000) // 60秒TTL
  MemoryCache::set_eviction_policy(memory_cache, LRU)
  
  // 测试添加和获取
  MemoryCache::put(memory_cache, "key1", "value1")
  MemoryCache::put(memory_cache, "key2", "value2")
  MemoryCache::put(memory_cache, "key3", "value3")
  
  // 验证获取
  let value1 = MemoryCache::get(memory_cache, "key1")
  let value2 = MemoryCache::get(memory_cache, "key2")
  let value3 = MemoryCache::get(memory_cache, "key3")
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
  assert_eq(value3, Some("value3"))
  
  // 测试不存在的键
  let non_existent = MemoryCache::get(memory_cache, "non_existent")
  assert_eq(non_existent, None)
  
  // 测试更新
  MemoryCache::put(memory_cache, "key1", "new_value1")
  let updated_value1 = MemoryCache::get(memory_cache, "key1")
  assert_eq(updated_value1, Some("new_value1"))
  
  // 测试删除
  MemoryCache::remove(memory_cache, "key2")
  let removed_value2 = MemoryCache::get(memory_cache, "key2")
  assert_eq(removed_value2, None)
  
  // 验证缓存大小
  assert_eq(MemoryCache::size(memory_cache), 2)
  
  // 测试清空
  MemoryCache::clear(memory_cache)
  assert_eq(MemoryCache::size(memory_cache), 0)
  
  let cleared_value1 = MemoryCache::get(memory_cache, "key1")
  let cleared_value3 = MemoryCache::get(memory_cache, "key3")
  
  assert_eq(cleared_value1, None)
  assert_eq(cleared_value3, None)
}

test "缓存TTL和过期机制" {
  // 测试缓存TTL和过期机制
  let ttl_cache = MemoryCache::new()
  
  // 设置短TTL用于测试
  MemoryCache::set_ttl(ttl_cache, 1000) // 1秒TTL
  
  // 添加缓存项
  MemoryCache::put(ttl_cache, "short_lived", "expires_soon")
  
  // 立即获取应该成功
  let immediate_value = MemoryCache::get(ttl_cache, "short_lived")
  assert_eq(immediate_value, Some("expires_soon"))
  
  // 等待过期
  Thread::sleep(1200) // 等待1.2秒
  
  // 过期后获取应该失败
  let expired_value = MemoryCache::get(ttl_cache, "short_lived")
  assert_eq(expired_value, None)
  
  // 测试自定义TTL
  MemoryCache::put_with_ttl(ttl_cache, "custom_ttl", "custom_value", 5000) // 5秒TTL
  
  // 立即获取应该成功
  let custom_immediate = MemoryCache::get(ttl_cache, "custom_ttl")
  assert_eq(custom_immediate, Some("custom_value"))
  
  // 等待默认TTL过期
  Thread::sleep(1200)
  
  // 自定义TTL的项应该仍然存在
  let custom_after_wait = MemoryCache::get(ttl_cache, "custom_ttl")
  assert_eq(custom_after_wait, Some("custom_value"))
  
  // 等待自定义TTL过期
  Thread::sleep(4000)
  
  // 自定义TTL的项现在应该过期
  let custom_expired = MemoryCache::get(ttl_cache, "custom_ttl")
  assert_eq(custom_expired, None)
  
  // 测试TTL刷新
  MemoryCache::put(ttl_cache, "refreshable", "refresh_me")
  
  // 多次获取以刷新TTL
  for i in 0..3 {
    Thread::sleep(400) // 每次等待400ms
    let refreshed_value = MemoryCache::get(ttl_cache, "refreshable")
    assert_eq(refreshed_value, Some("refresh_me"))
  }
  
  // 最后一次等待后应该仍然有效
  let final_refreshed = MemoryCache::get(ttl_cache, "refreshable")
  assert_eq(final_refreshed, Some("refresh_me"))
}

test "缓存淘汰策略" {
  // 测试缓存淘汰策略
  let lru_cache = MemoryCache::new()
  MemoryCache::set_max_size(lru_cache, 3)
  MemoryCache::set_eviction_policy(lru_cache, LRU)
  
  // 添加3个项，填满缓存
  MemoryCache::put(lru_cache, "key1", "value1")
  MemoryCache::put(lru_cache, "key2", "value2")
  MemoryCache::put(lru_cache, "key3", "value3")
  
  // 验证缓存大小
  assert_eq(MemoryCache::size(lru_cache), 3)
  
  // 访问key1，使其成为最近使用
  MemoryCache::get(lru_cache, "key1")
  
  // 添加新项，应该淘汰key2（最久未使用）
  MemoryCache::put(lru_cache, "key4", "value4")
  
  // 验证缓存大小仍为3
  assert_eq(MemoryCache::size(lru_cache), 3)
  
  // 验证key1仍在（最近访问过）
  let value1 = MemoryCache::get(lru_cache, "key1")
  assert_eq(value1, Some("value1"))
  
  // 验证key2已被淘汰
  let value2 = MemoryCache::get(lru_cache, "key2")
  assert_eq(value2, None)
  
  // 验证key3和key4仍在
  let value3 = MemoryCache::get(lru_cache, "key3")
  let value4 = MemoryCache::get(lru_cache, "key4")
  
  assert_eq(value3, Some("value3"))
  assert_eq(value4, Some("value4"))
  
  // 测试LFU策略
  let lfu_cache = MemoryCache::new()
  MemoryCache::set_max_size(lfu_cache, 3)
  MemoryCache::set_eviction_policy(lfu_cache, LFU)
  
  // 添加3个项
  MemoryCache::put(lfu_cache, "lfu_key1", "lfu_value1")
  MemoryCache::put(lfu_cache, "lfu_key2", "lfu_value2")
  MemoryCache::put(lfu_cache, "lfu_key3", "lfu_value3")
  
  // 多次访问lfu_key1，增加其频率
  for i in 0..5 {
    MemoryCache::get(lfu_cache, "lfu_key1")
  }
  
  // 访问lfu_key2一次
  MemoryCache::get(lfu_cache, "lfu_key2")
  
  // 添加新项，应该淘汰lfu_key3（频率最低）
  MemoryCache::put(lfu_cache, "lfu_key4", "lfu_value4")
  
  // 验证lfu_key1和lfu_key2仍在
  let lfu_value1 = MemoryCache::get(lfu_cache, "lfu_key1")
  let lfu_value2 = MemoryCache::get(lfu_cache, "lfu_key2")
  
  assert_eq(lfu_value1, Some("lfu_value1"))
  assert_eq(lfu_value2, Some("lfu_value2"))
  
  // 验证lfu_key3已被淘汰
  let lfu_value3 = MemoryCache::get(lfu_cache, "lfu_key3")
  assert_eq(lfu_value3, None)
  
  // 验证lfu_key4已添加
  let lfu_value4 = MemoryCache::get(lfu_cache, "lfu_key4")
  assert_eq(lfu_value4, Some("lfu_value4"))
}

test "分布式缓存操作" {
  // 测试分布式缓存操作
  let distributed_cache = DistributedCache::new()
  
  // 配置分布式缓存
  let nodes = [
    CacheNode::new("node1", "127.0.0.1", 7001),
    CacheNode::new("node2", "127.0.0.1", 7002),
    CacheNode::new("node3", "127.0.0.1", 7003)
  ]
  
  for node in nodes {
    DistributedCache::add_node(distributed_cache, node)
  }
  
  DistributedCache::set_replication_factor(distributed_cache, 2)
  DistributedCache::set_consistency_level(distributed_cache, Eventual)
  
  // 测试添加和获取
  DistributedCache::put(distributed_cache, "distributed_key1", "distributed_value1")
  DistributedCache::put(distributed_cache, "distributed_key2", "distributed_value2")
  
  // 获取值
  let value1 = DistributedCache::get(distributed_cache, "distributed_key1")
  let value2 = DistributedCache::get(distributed_cache, "distributed_key2")
  
  assert_eq(value1, Some("distributed_value1"))
  assert_eq(value2, Some("distributed_value2"))
  
  // 测试批量操作
  let batch_items = [
    ("batch_key1", "batch_value1"),
    ("batch_key2", "batch_value2"),
    ("batch_key3", "batch_value3")
  ]
  
  DistributedCache::put_batch(distributed_cache, batch_items)
  
  // 批量获取
  let batch_keys = ["batch_key1", "batch_key2", "batch_key3", "non_existent_key"]
  let batch_results = DistributedCache::get_batch(distributed_cache, batch_keys)
  
  assert_eq(Array::length(batch_results), 4)
  assert_eq(Array::get(batch_results, 0), Some("batch_value1"))
  assert_eq(Array::get(batch_results, 1), Some("batch_value2"))
  assert_eq(Array::get(batch_results, 2), Some("batch_value3"))
  assert_eq(Array::get(batch_results, 3), None)
  
  // 测试缓存统计
  let stats = DistributedCache::get_statistics(distributed_cache)
  
  assert_true(DistributedStats::has_total_keys(stats))
  assert_true(DistributedStats::has_total_memory_usage(stats))
  assert_true(DistributedStats::has_hit_rate(stats))
  assert_true(DistributedStats::has_miss_rate(stats))
  assert_true(DistributedStats::has_node_statistics(stats))
  
  // 验证统计值
  let total_keys = DistributedStats::get_total_keys(stats)
  let hit_rate = DistributedStats::get_hit_rate(stats)
  let miss_rate = DistributedStats::get_miss_rate(stats)
  
  assert_true(total_keys >= 5)
  assert_true(hit_rate >= 0.0 && hit_rate <= 1.0)
  assert_true(miss_rate >= 0.0 && miss_rate <= 1.0)
  assert_true((hit_rate + miss_rate) <= 1.0)
  
  // 测试节点状态
  let node_stats = DistributedStats::get_node_statistics(stats)
  assert_eq(Array::length(node_stats), 3)
  
  for node_stat in node_stats {
    assert_true(NodeStats::has_node_id(node_stat))
    assert_true(NodeStats::has_key_count(node_stat))
    assert_true(NodeStats::has_memory_usage(node_stat))
    assert_true(NodeStats::has_is_healthy(node_stat))
    
    assert_true(NodeStats::get_key_count(node_stat) >= 0)
    assert_true(NodeStats::get_memory_usage(node_stat) >= 0)
    assert_true(NodeStats::get_is_healthy(node_stat))
  }
}

test "缓存预热和加载" {
  // 测试缓存预热和加载
  let cache_warmup = CacheWarmupManager::new()
  
  // 创建数据加载器
  let data_loader = TestDataLoader::new()
  
  // 配置预热策略
  let warmup_config = WarmupConfig::new()
  WarmupConfig::add_data_source(warmup_config, "user_profiles", data_loader)
  WarmupConfig::add_key_pattern(warmup_config, "user_profile_.*")
  WarmupConfig::set_batch_size(warmup_config, 100)
  WarmupConfig::set_concurrency(warmup_config, 5)
  
  CacheWarmupManager::configure(cache_warmup, warmup_config)
  
  // 开始预热
  CacheWarmupManager::start_warmup(cache_warmup)
  
  // 等待预热完成
  CacheWarmupManager::wait_for_completion(cache_warmup)
  
  // 获取预热结果
  let warmup_result = CacheWarmupManager::get_result(cache_warmup)
  
  // 验证预热结果
  assert_true(WarmupResult::is_successful(warmup_result))
  assert_true(WarmupResult::has_loaded_keys(warmup_result))
  assert_true(WarmupResult::has_failed_keys(warmup_result))
  assert_true(WarmupResult::has_statistics(warmup_result))
  
  // 验证加载的键
  let loaded_keys = WarmupResult::get_loaded_keys(warmup_result)
  assert_true(Array::length(loaded_keys) > 0)
  
  // 验证失败的键
  let failed_keys = WarmupResult::get_failed_keys(warmup_result)
  assert_true(Array::length(failed_keys) >= 0)
  
  // 验证统计信息
  let warmup_stats = WarmupResult::get_statistics(warmup_result)
  assert_true(WarmupStats::has_total_requested(warmup_stats))
  assert_true(WarmupStats::has_successfully_loaded(warmup_stats))
  assert_true(WarmupStats::has_failed_to_load(warmup_stats))
  assert_true(WarmupStats::has_load_time(warmup_stats))
  
  let total_requested = WarmupStats::get_total_requested(warmup_stats)
  let successfully_loaded = WarmupStats::get_successfully_loaded(warmup_stats)
  let failed_to_load = WarmupStats::get_failed_to_load(warmup_stats)
  
  assert_eq(total_requested, successfully_loaded + failed_to_load)
  assert_true(WarmupStats::get_load_time(warmup_stats) > 0)
  
  // 测试增量预热
  CacheWarmupManager::start_incremental_warmup(cache_warmup, ["new_user_profile_1", "new_user_profile_2"])
  CacheWarmupManager::wait_for_completion(cache_warmup)
  
  let incremental_result = CacheWarmupManager::get_result(cache_warmup)
  let incremental_loaded = WarmupResult::get_loaded_keys(incremental_result)
  
  assert_true(Array::contains(incremental_loaded, "new_user_profile_1"))
  assert_true(Array::contains(incremental_loaded, "new_user_profile_2"))
  
  // 测试预热计划
  let warmup_scheduler = WarmupScheduler::new()
  
  // 添加定时预热任务
  WarmupScheduler::add_scheduled_warmup(warmup_scheduler, "daily_user_warmup", "0 2 * * *", data_loader) // 每天凌晨2点
  WarmupScheduler::add_scheduled_warmup(warmup_scheduler, "hourly_stats_warmup", "0 * * * *", data_loader) // 每小时
  
  // 启动调度器
  WarmupScheduler::start(warmup_scheduler)
  
  // 获取调度状态
  let schedule_status = WarmupScheduler::get_status(warmup_scheduler)
  assert_true(ScheduleStatus::is_running(schedule_status))
  assert_true(ScheduleStatus::has_active_tasks(schedule_status))
  
  let active_tasks = ScheduleStatus::get_active_tasks(schedule_status)
  assert_eq(Array::length(active_tasks), 2)
  
  // 停止调度器
  WarmupScheduler::stop(warmup_scheduler)
  
  let stopped_status = WarmupScheduler::get_status(warmup_scheduler);
  assert_false(ScheduleStatus::is_running(stopped_status))
}

test "缓存性能监控和优化" {
  // 测试缓存性能监控和优化
  let cache_monitor = CacheMonitor::new()
  
  // 创建监控的缓存
  let monitored_cache = MonitoredCache::new(cache_monitor)
  MonitoredCache::set_max_size(monitored_cache, 1000)
  MonitoredCache::set_ttl(monitored_cache, 60000)
  
  // 执行大量缓存操作
  for i in 0..10000 {
    let key = "perf_key_" + (i % 1000).to_string() // 使用1000个不同的键
    let value = "perf_value_" + i.to_string()
    
    MonitoredCache::put(monitored_cache, key, value)
    
    // 随机获取一些值
    if i % 3 == 0 {
      let random_key = "perf_key_" + (Math::random() * 1000).to_int().to_string()
      MonitoredCache::get(monitored_cache, random_key)
    }
  }
  
  // 获取性能指标
  let performance_metrics = CacheMonitor::get_performance_metrics(cache_monitor)
  
  // 验证性能指标
  assert_true(PerformanceMetrics::has_total_operations(performance_metrics))
  assert_true(PerformanceMetrics::has_hit_count(performance_metrics))
  assert_true(PerformanceMetrics::has_miss_count(performance_metrics))
  assert_true(PerformanceMetrics::has_put_count(performance_metrics))
  assert_true(PerformanceMetrics::has_eviction_count(performance_metrics))
  assert_true(PerformanceMetrics::has_average_get_time(performance_metrics))
  assert_true(PerformanceMetrics::has_average_put_time(performance_metrics))
  
  // 验证指标值
  let total_ops = PerformanceMetrics::get_total_operations(performance_metrics)
  let hit_count = PerformanceMetrics::get_hit_count(performance_metrics)
  let miss_count = PerformanceMetrics::get_miss_count(performance_metrics)
  let put_count = PerformanceMetrics::get_put_count(performance_metrics)
  
  assert_true(total_ops > 0)
  assert_true(hit_count >= 0)
  assert_true(miss_count >= 0)
  assert_true(put_count > 0)
  
  // 计算命中率
  let hit_rate = hit_count.to_float() / (hit_count + miss_count).to_float()
  assert_true(hit_rate >= 0.0 && hit_rate <= 1.0)
  
  // 测试缓存优化器
  let cache_optimizer = CacheOptimizer::new()
  
  // 分析当前缓存配置
  let optimization_analysis = CacheOptimizer::analyze_configuration(cache_optimizer, monitored_cache)
  
  // 验证分析结果
  assert_true(OptimizationAnalysis::has_recommendations(optimization_analysis))
  assert_true(OptimizationAnalysis::has_current_performance(optimization_analysis))
  assert_true(OptimizationAnalysis::has_potential_improvements(optimization_analysis))
  
  // 获取优化建议
  let recommendations = OptimizationAnalysis::get_recommendations(optimization_analysis)
  assert_true(Array::length(recommendations) >= 0)
  
  // 应用优化建议
  for recommendation in recommendations {
    if Recommendation::is_applicable(recommendation) {
      let applied = CacheOptimizer::apply_recommendation(cache_optimizer, monitored_cache, recommendation)
      if applied {
        // 验证建议已应用
        assert_true(Recommendation::is_applied(recommendation))
      }
    }
  }
  
  // 测试热点数据分析
  let hot_data_analyzer = HotDataAnalyzer::new()
  
  // 分析热点数据
  let hot_data_analysis = HotDataAnalyzer::analyze(hot_data_analyzer, monitored_cache)
  
  // 验证热点数据
  assert_true(HotDataAnalysis::has_hot_keys(hot_data_analysis))
  assert_true(HotDataAnalysis::has_access_patterns(hot_data_analysis))
  
  let hot_keys = HotDataAnalysis::get_hot_keys(hot_data_analysis)
  assert_true(Array::length(hot_keys) >= 0)
  
  // 验证热点键信息
  for hot_key in hot_keys {
    assert_true(HotKey::has_key(hot_key))
    assert_true(HotKey::has_access_count(hot_key))
    assert_true(HotKey::has_access_frequency(hot_key))
    
    assert_true(HotKey::get_access_count(hot_key) > 0)
    assert_true(HotKey::get_access_frequency(hot_key) > 0.0)
  }
  
  // 测试缓存容量规划
  let capacity_planner = CapacityPlanner::new()
  
  // 分析当前使用情况
  let capacity_analysis = CapacityPlanner::analyze_usage(capacity_planner, monitored_cache)
  
  // 验证容量分析
  assert_true(CapacityAnalysis::has_current_usage(capacity_analysis))
  assert_true(CapacityAnalysis::has_growth_projection(capacity_analysis))
  assert_true(CapacityAnalysis::has_optimal_size_recommendation(capacity_analysis))
  
  // 获取容量建议
  let optimal_size = CapacityAnalysis::get_optimal_size_recommendation(capacity_analysis)
  assert_true(optimal_size > 0)
  
  // 测试缓存分区
  let partitioned_cache = PartitionedCache::new()
  
  // 创建分区
  PartitionedCache::create_partition(partitioned_cache, "user_data", 500)
  PartitionedCache::create_partition(partitioned_cache, "product_data", 300)
  PartitionedCache::create_partition(partitioned_cache, "order_data", 200)
  
  // 在不同分区中添加数据
  PartitionedCache::put(partitioned_cache, "user_data", "user_123", "user_data_123")
  PartitionedCache::put(partitioned_cache, "product_data", "product_456", "product_data_456")
  PartitionedCache::put(partitioned_cache, "order_data", "order_789", "order_data_789")
  
  // 从不同分区获取数据
  let user_data = PartitionedCache::get(partitioned_cache, "user_data", "user_123")
  let product_data = PartitionedCache::get(partitioned_cache, "product_data", "product_456")
  let order_data = PartitionedCache::get(partitioned_cache, "order_data", "order_789")
  
  assert_eq(user_data, Some("user_data_123"))
  assert_eq(product_data, Some("product_data_456"))
  assert_eq(order_data, Some("order_data_789"))
  
  // 获取分区统计
  let partition_stats = PartitionedCache::get_partition_statistics(partitioned_cache)
  assert_eq(Array::length(partition_stats), 3)
  
  for stat in partition_stats {
    assert_true(PartitionStats::has_partition_name(stat))
    assert_true(PartitionStats::has_size(stat))
    assert_true(PartitionStats::has_max_size(stat))
    assert_true(PartitionStats::has_utilization(stat))
    
    let utilization = PartitionStats::get_utilization(stat)
    assert_true(utilization >= 0.0 && utilization <= 1.0)
  }
}