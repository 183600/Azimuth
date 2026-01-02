// Azimuth 遥测缓存策略测试用例
// 专注于遥测数据的缓存管理和优化策略

// 测试1: 基础缓存策略
test "基础缓存策略测试" {
  let cache = TelemetryCache::new("基础缓存", MemoryBackend::new())
  
  // 配置缓存策略
  let eviction_policy = LRUEvictionPolicy::new(1000)  // 最多1000条目，LRU淘汰
  let ttl_policy = TTLPolicy::new(300)  // 5分钟TTL
  let size_policy = SizePolicy::new(100 * 1024 * 1024)  // 100MB大小限制
  
  TelemetryCache::set_eviction_policy(cache, eviction_policy)
  TelemetryCache::set_ttl_policy(cache, ttl_policy)
  TelemetryCache::set_size_policy(cache, size_policy)
  
  // 测试数据存储
  let metric1 = Metric::new("cpu.usage", 65.5)
  let metric2 = Metric::new("memory.usage", 72.3)
  let metric3 = Metric::new("disk.usage", 45.2)
  
  TelemetryCache::put(cache, "metric1", metric1)
  TelemetryCache::put(cache, "metric2", metric2)
  TelemetryCache::put(cache, "metric3", metric3)
  
  // 验证数据检索
  let cached_metric1 = TelemetryCache::get(cache, "metric1")
  let cached_metric2 = TelemetryCache::get(cache, "metric2")
  let cached_metric3 = TelemetryCache::get(cache, "metric3")
  
  assert_true(cached_metric1.is_some())
  assert_true(cached_metric2.is_some())
  assert_true(cached_metric3.is_some())
  
  assert_eq(cached_metric1.unwrap().value, 65.5)
  assert_eq(cached_metric2.unwrap().value, 72.3)
  assert_eq(cached_metric3.unwrap().value, 45.2)
  
  // 验证缓存统计
  let stats = TelemetryCache::get_stats(cache)
  assert_eq(stats.entries_count, 3)
  assert_eq(stats.hits, 3)
  assert_eq(stats.misses, 0)
  assert_eq(stats.hit_rate, 1.0)
}

// 测试2: 多级缓存架构
test "多级缓存架构测试" {
  // 创建多级缓存
  let l1_cache = TelemetryCache::new("L1缓存", MemoryBackend::new())
  let l2_cache = TelemetryCache::new("L2缓存", RedisBackend::new("redis://localhost:6379"))
  let l3_cache = TelemetryCache::new("L3缓存", DiskBackend::new("/tmp/telemetry_cache"))
  
  // 配置各级缓存策略
  TelemetryCache::set_max_entries(l1_cache, 100)    // L1: 100条目
  TelemetryCache::set_max_entries(l2_cache, 1000)   // L2: 1000条目
  TelemetryCache::set_max_entries(l3_cache, 10000)  // L3: 10000条目
  
  TelemetryCache::set_ttl(l1_cache, 60)    // L1: 1分钟
  TelemetryCache::set_ttl(l2_cache, 600)   // L2: 10分钟
  TelemetryCache::set_ttl(l3_cache, 3600)  // L3: 1小时
  
  // 创建多级缓存管理器
  let multi_cache = MultiLevelCache::new()
  MultiLevelCache::add_level(multi_cache, l1_cache)
  MultiLevelCache::add_level(multi_cache, l2_cache)
  MultiLevelCache::add_level(multi_cache, l3_cache)
  
  // 测试数据存储和检索
  let trace = Trace::new("trace-12345", "api.request", Ok, 250)
  Trace::add_attribute(trace, "service.name", "payment.service")
  Trace::add_attribute(trace, "user.id", "user-123")
  
  MultiLevelCache::put(multi_cache, "trace-12345", trace)
  
  // 从多级缓存检索
  let cached_trace = MultiLevelCache::get(multi_cache, "trace-12345")
  assert_true(cached_trace.is_some())
  assert_eq(cached_trace.unwrap().trace_id, "trace-12345")
  
  // 验证缓存分布
  let l1_stats = TelemetryCache::get_stats(l1_cache)
  let l2_stats = TelemetryCache::get_stats(l2_cache)
  let l3_stats = TelemetryCache::get_stats(l3_cache)
  
  assert_eq(l1_stats.entries_count, 1)
  assert_eq(l2_stats.entries_count, 1)
  assert_eq(l3_stats.entries_count, 1)
  
  // 验证多级缓存统计
  let multi_stats = MultiLevelCache::get_stats(multi_cache)
  assert_eq(multi_stats.total_hits, 1)
  assert_eq(multi_stats.l1_hits, 1)
  assert_eq(multi_stats.l2_hits, 0)
  assert_eq(multi_stats.l3_hits, 0)
}

// 测试3: 缓存预热和预加载
test "缓存预热和预加载测试" {
  let cache = TelemetryCache::new("预热缓存", MemoryBackend::new())
  let preloader = CachePreloader::new()
  
  // 配置预加载策略
  let hot_data_preloader = HotDataPreloader::new()
  let pattern_preloader = PatternPreloader::new("trace.*")
  let time_window_preloader = TimeWindowPreloader::new(3600)  // 最近1小时
  
  CachePreloader::add_strategy(preloader, hot_data_preloader)
  CachePreloader::add_strategy(preloader, pattern_preloader)
  CachePreloader::add_strategy(preloader, time_window_preloader)
  
  // 模拟热数据源
  let data_source = MockDataSource::new()
  MockDataSource::add_trace(data_source, Trace::new("trace-001", "hot.operation", Ok, 100))
  MockDataSource::add_trace(data_source, Trace::new("trace-002", "hot.operation", Ok, 150))
  MockDataSource::add_trace(data_source, Trace::new("trace-003", "cold.operation", Ok, 200))
  
  // 执行预热
  CachePreloader::preload(preloader, cache, data_source)
  
  // 验证预热结果
  let stats = TelemetryCache::get_stats(cache)
  assert_true(stats.entries_count >= 2)  // 至少预热了热数据
  
  // 验证热数据在缓存中
  let hot_trace1 = TelemetryCache::get(cache, "trace-001")
  let hot_trace2 = TelemetryCache::get(cache, "trace-002")
  
  assert_true(hot_trace1.is_some())
  assert_true(hot_trace2.is_some())
  
  // 验证预热策略统计
  let preloader_stats = CachePreloader::get_stats(preloader)
  assert_true(preloader_stats.total_preloaded > 0)
  assert_true(preloader_stats.hot_data_preloaded > 0)
}

// 测试4: 缓存一致性保证
test "缓存一致性保证测试" {
  let cache1 = TelemetryCache::new("缓存节点1", MemoryBackend::new())
  let cache2 = TelemetryCache::new("缓存节点2", MemoryBackend::new())
  let cache3 = TelemetryCache::new("缓存节点3", MemoryBackend::new())
  
  // 创建一致性管理器
  let consistency_manager = CacheConsistencyManager::new()
  
  // 配置一致性策略
  let strong_consistency = StrongConsistencyStrategy::new()
  let eventual_consistency = EventualConsistencyStrategy::new(5000)  // 5秒内最终一致
  
  CacheConsistencyManager::add_cache(consistency_manager, cache1)
  CacheConsistencyManager::add_cache(consistency_manager, cache2)
  CacheConsistencyManager::add_cache(consistency_manager, cache3)
  
  CacheConsistencyManager::set_consistency_strategy(consistency_manager, strong_consistency)
  
  // 测试写入一致性
  let metric = Metric::new("consistency.test", 42.0)
  Metric::add_attribute(metric, "version", "1.0")
  
  CacheConsistencyManager::put(consistency_manager, "test.metric", metric)
  
  // 验证所有缓存节点都有相同数据
  let metric1 = TelemetryCache::get(cache1, "test.metric")
  let metric2 = TelemetryCache::get(cache2, "test.metric")
  let metric3 = TelemetryCache::get(cache3, "test.metric")
  
  assert_true(metric1.is_some())
  assert_true(metric2.is_some())
  assert_true(metric3.is_some())
  
  assert_eq(metric1.unwrap().value, 42.0)
  assert_eq(metric2.unwrap().value, 42.0)
  assert_eq(metric3.unwrap().value, 42.0)
  
  // 测试更新一致性
  let updated_metric = Metric::new("consistency.test", 84.0)
  Metric::add_attribute(updated_metric, "version", "2.0")
  
  CacheConsistencyManager::put(consistency_manager, "test.metric", updated_metric)
  
  // 验证所有缓存节点都更新
  let updated_metric1 = TelemetryCache::get(cache1, "test.metric")
  let updated_metric2 = TelemetryCache::get(cache2, "test.metric")
  let updated_metric3 = TelemetryCache::get(cache3, "test.metric")
  
  assert_eq(updated_metric1.unwrap().value, 84.0)
  assert_eq(updated_metric2.unwrap().value, 84.0)
  assert_eq(updated_metric3.unwrap().value, 84.0)
  
  // 验证版本一致性
  let version1 = Metric::get_attribute(updated_metric1.unwrap(), "version")
  let version2 = Metric::get_attribute(updated_metric2.unwrap(), "version")
  let version3 = Metric::get_attribute(updated_metric3.unwrap(), "version")
  
  assert_eq(version1, Some(StringValue("2.0")))
  assert_eq(version2, Some(StringValue("2.0")))
  assert_eq(version3, Some(StringValue("2.0")))
}

// 测试5: 缓存性能优化
test "缓存性能优化测试" {
  let cache = TelemetryCache::new("性能优化缓存", MemoryBackend::new())
  let performance_optimizer = CachePerformanceOptimizer::new()
  
  // 配置性能优化策略
  let compression_optimizer = CompressionOptimizer::new(Gzip)
  let serialization_optimizer = SerializationOptimizer::new(Binary)
  let batch_optimizer = BatchOptimizer::new(100, 50)  // 批量大小100，50ms超时
  
  CachePerformanceOptimizer::add_optimizer(performance_optimizer, compression_optimizer)
  CachePerformanceOptimizer::add_optimizer(performance_optimizer, serialization_optimizer)
  CachePerformanceOptimizer::add_optimizer(performance_optimizer, batch_optimizer)
  
  // 应用优化
  CachePerformanceOptimizer::apply_optimizations(performance_optimizer, cache)
  
  // 性能基准测试
  let start_time = Time::now()
  
  // 批量写入测试
  for i in 0..=1000 {
    let metric = Metric::new("perf.metric." + i.to_string(), i.to_float())
    CachePerformanceOptimizer::put_batch(performance_optimizer, cache, "metric." + i.to_string(), metric)
  }
  
  let write_time = Time::now() - start_time
  
  // 批量读取测试
  let read_start_time = Time::now()
  
  for i in 0..=1000 {
    TelemetryCache::get(cache, "metric." + i.to_string())
  }
  
  let read_time = Time::now() - read_start_time
  
  // 验证性能指标
  let performance_stats = CachePerformanceOptimizer::get_performance_stats(performance_optimizer)
  
  assert_true(write_time < 5000)  // 写入应在5秒内完成
  assert_true(read_time < 1000)   // 读取应在1秒内完成
  assert_true(performance_stats.avg_write_latency_ms < 10)
  assert_true(performance_stats.avg_read_latency_ms < 2)
  assert_true(performance_stats.compression_ratio > 0.5)
  
  // 验证缓存命中率
  let cache_stats = TelemetryCache::get_stats(cache)
  assert_eq(cache_stats.entries_count, 1001)
  assert_eq(cache_stats.hits, 1001)
  assert_eq(cache_stats.hit_rate, 1.0)
}

// 测试6: 缓存失效和刷新
test "缓存失效和刷新测试" {
  let cache = TelemetryCache::new("失效刷新缓存", MemoryBackend::new())
  let invalidation_manager = CacheInvalidationManager::new()
  
  // 配置失效策略
  let ttl_invalidation = TTLInvalidationStrategy::new()
  let event_invalidation = EventInvalidationStrategy::new()
  let manual_invalidation = ManualInvalidationStrategy::new()
  
  CacheInvalidationManager::add_strategy(invalidation_manager, ttl_invalidation)
  CacheInvalidationManager::add_strategy(invalidation_manager, event_invalidation)
  CacheInvalidationManager::add_strategy(invalidation_manager, manual_invalidation)
  
  // 设置TTL策略
  let short_ttl_policy = TTLPolicy::new(2)  // 2秒TTL
  let long_ttl_policy = TTLPolicy::new(3600)  // 1小时TTL
  
  // 添加测试数据
  let short_lived_metric = Metric::new("short.lived", 100.0)
  let long_lived_metric = Metric::new("long.lived", 200.0)
  
  TelemetryCache::put_with_ttl(cache, "short.metric", short_lived_metric, short_ttl_policy)
  TelemetryCache::put_with_ttl(cache, "long.metric", long_lived_metric, long_ttl_policy)
  
  // 验证初始状态
  assert_true(TelemetryCache::get(cache, "short.metric").is_some())
  assert_true(TelemetryCache::get(cache, "long.metric").is_some())
  
  // 等待短期TTL过期
  Time::sleep(3000)
  
  // 验证TTL失效
  assert_false(TelemetryCache::get(cache, "short.metric").is_some())
  assert_true(TelemetryCache::get(cache, "long.metric").is_some())
  
  // 测试事件失效
  let event_metric = Metric::new("event.based", 300.0)
  TelemetryCache::put(cache, "event.metric", event_metric)
  
  // 触发失效事件
  let invalidation_event = InvalidationEvent::new("event.metric", "data.updated")
  CacheInvalidationManager::invalidate(invalidation_manager, cache, invalidation_event)
  
  // 验证事件失效
  assert_false(TelemetryCache::get(cache, "event.metric").is_some())
  
  // 测试手动失效
  let manual_metric = Metric::new("manual.based", 400.0)
  TelemetryCache::put(cache, "manual.metric", manual_metric)
  
  CacheInvalidationManager::invalidate_key(invalidation_manager, cache, "manual.metric")
  
  // 验证手动失效
  assert_false(TelemetryCache::get(cache, "manual.metric").is_some())
  
  // 验证失效统计
  let invalidation_stats = CacheInvalidationManager::get_stats(invalidation_manager)
  assert_eq(invalidation_stats.ttl_invalidations, 1)
  assert_eq(invalidation_stats.event_invalidations, 1)
  assert_eq(invalidation_stats.manual_invalidations, 1)
}

// 测试7: 缓存监控和告警
test "缓存监控和告警测试" {
  let cache = TelemetryCache::new("监控缓存", MemoryBackend::new())
  let monitor = CacheMonitor::new()
  let alert_manager = CacheAlertManager::new()
  
  // 配置监控指标
  CacheMonitor::enable_performance_monitoring(monitor)
  CacheMonitor::enable_capacity_monitoring(monitor)
  CacheMonitor::enable_hit_rate_monitoring(monitor)
  
  // 配置告警规则
  let low_hit_rate_alert = HitRateAlertRule::new(LessThan, 0.8)  // 命中率低于80%
  let high_memory_alert = MemoryUsageAlertRule::new(GreaterThan, 0.9)  // 内存使用超过90%
  let high_latency_alert = LatencyAlertRule::new(GreaterThan, 100)  // 延迟超过100ms
  
  CacheAlertManager::add_rule(alert_manager, low_hit_rate_alert)
  CacheAlertManager::add_rule(alert_manager, high_memory_alert)
  CacheAlertManager::add_rule(alert_manager, high_latency_alert)
  
  // 启动监控
  CacheMonitor::start_monitoring(monitor, cache)
  CacheAlertManager::start_monitoring(alert_manager, cache)
  
  // 模拟缓存操作
  for i in 0..=100 {
    let metric = Metric::new("monitor.metric", i.to_float())
    TelemetryCache::put(cache, "metric." + i.to_string(), metric)
  }
  
  // 模拟低命中率场景
  for i in 200..=300 {
    TelemetryCache::get(cache, "nonexistent.metric." + i.to_string())
  }
  
  // 获取监控指标
  let monitoring_metrics = CacheMonitor::get_metrics(monitor)
  
  // 验证监控数据
  assert_true(monitoring_metrics.total_operations > 0)
  assert_true(monitoring_metrics.hit_rate < 1.0)
  assert_true(monitoring_metrics.memory_usage_mb > 0)
  assert_true(monitoring_metrics.avg_latency_ms > 0)
  
  // 检查告警
  let active_alerts = CacheAlertManager::get_active_alerts(alert_manager)
  
  // 由于命中率可能低于80%，可能触发低命中率告警
  if monitoring_metrics.hit_rate < 0.8 {
    assert_true(active_alerts.any(fn(alert) { alert.type == HitRateAlert }))
  }
  
  // 验证监控历史
  let monitoring_history = CacheMonitor::get_history(monitor, 3600)  // 最近1小时
  assert_true(monitoring_history.length() > 0)
}

// 测试8: 缓存分布式架构
test "缓存分布式架构测试" {
  // 创建分布式缓存节点
  let node1 = DistributedCacheNode::new("node1", "localhost:8001")
  let node2 = DistributedCacheNode::new("node2", "localhost:8002")
  let node3 = DistributedCacheNode::new("node3", "localhost:8003")
  
  // 创建分布式缓存集群
  let cluster = DistributedCacheCluster::new()
  DistributedCacheCluster::add_node(cluster, node1)
  DistributedCacheCluster::add_node(cluster, node2)
  DistributedCacheCluster::add_node(cluster, node3)
  
  // 配置分布式策略
  let consistent_hashing = ConsistentHashingStrategy::new()
  let replication_strategy = ReplicationStrategy::new(2)  // 2副本
  let failover_strategy = FailoverStrategy::new()
  
  DistributedCacheCluster::set_distribution_strategy(cluster, consistent_hashing)
  DistributedCacheCluster::set_replication_strategy(cluster, replication_strategy)
  DistributedCacheCluster::set_failover_strategy(cluster, failover_strategy)
  
  // 测试分布式写入
  let trace = Trace::new("distributed.trace", "test.operation", Ok, 150)
  Trace::add_attribute(trace, "user.id", "user-123")
  
  DistributedCacheCluster::put(cluster, "distributed.trace", trace)
  
  // 测试分布式读取
  let cached_trace = DistributedCacheCluster::get(cluster, "distributed.trace")
  assert_true(cached_trace.is_some())
  assert_eq(cached_trace.unwrap().trace_id, "distributed.trace")
  
  // 验证数据分布
  let distribution_stats = DistributedCacheCluster::get_distribution_stats(cluster)
  assert_true(distribution_stats.total_replicas >= 2)
  assert_true(distribution_stats.nodes_with_data >= 2)
  
  // 测试节点故障处理
  DistributedCacheCluster::simulate_node_failure(cluster, "node2")
  
  // 验证故障转移
  let failover_trace = DistributedCacheCluster::get(cluster, "distributed.trace")
  assert_true(failover_trace.is_some())  // 数据应该仍然可用
  
  // 验证故障转移统计
  let failover_stats = DistributedCacheCluster::get_failover_stats(cluster)
  assert_eq(failover_stats.failed_nodes, 1)
  assert_true(failover_stats.successful_failovers > 0)
}

// 测试9: 缓存数据压缩
test "缓存数据压缩测试" {
  let cache = TelemetryCache::new("压缩缓存", MemoryBackend::new())
  let compression_manager = CacheCompressionManager::new()
  
  // 配置压缩策略
  let gzip_compression = GzipCompressionStrategy::new(6)  // 压缩级别6
  let lz4_compression = LZ4CompressionStrategy::new()     // 快速压缩
  let adaptive_compression = AdaptiveCompressionStrategy::new()
  
  CacheCompressionManager::add_strategy(compression_manager, gzip_compression)
  CacheCompressionManager::add_strategy(compression_manager, lz4_compression)
  CacheCompressionManager::add_strategy(compression_manager, adaptive_compression)
  
  // 设置自适应压缩规则
  AdaptiveCompressionStrategy::add_rule(adaptive_compression, 
    fn(data) { data.size > 1024 }, gzip_compression)  // 大于1KB使用gzip
  AdaptiveCompressionStrategy::add_rule(adaptive_compression, 
    fn(data) { data.size <= 1024 }, lz4_compression)  // 小于等于1KB使用lz4
  
  // 应用压缩
  CacheCompressionManager::enable_compression(compression_manager, cache)
  
  // 测试大数据压缩
  let large_trace = create_large_trace()  // 创建大的追踪数据
  let original_size = large_trace.size
  
  CacheCompressionManager::put_compressed(compression_manager, cache, "large.trace", large_trace)
  
  let compressed_trace = CacheCompressionManager::get_compressed(compression_manager, cache, "large.trace")
  assert_true(compressed_trace.is_some())
  
  // 验证压缩效果
  let compression_stats = CacheCompressionManager::get_compression_stats(compression_manager)
  assert_true(compression_stats.compression_ratio < 1.0)  // 压缩后应该更小
  assert_true(compression_stats.space_saved_mb > 0)
  
  // 测试小数据压缩
  let small_metric = Metric::new("small.metric", 42.0)
  let original_small_size = small_metric.size
  
  CacheCompressionManager::put_compressed(compression_manager, cache, "small.metric", small_metric)
  
  let compressed_metric = CacheCompressionManager::get_compressed(compression_manager, cache, "small.metric")
  assert_true(compressed_metric.is_some())
  
  // 验证压缩策略选择
  let strategy_stats = CacheCompressionManager::get_strategy_stats(compression_manager)
  assert_true(strategy_stats.gzip_usage > 0)
  assert_true(strategy_stats.lz4_usage > 0)
}

// 测试10: 缓存安全和加密
test "缓存安全和加密测试" {
  let cache = TelemetryCache::new("安全缓存", MemoryBackend::new())
  let security_manager = CacheSecurityManager::new()
  
  // 配置加密策略
  let aes_encryption = AESEncryptionStrategy::new("secure-key-123")
  let field_level_encryption = FieldLevelEncryptionStrategy::new()
  let tokenization = TokenizationStrategy::new()
  
  CacheSecurityManager::add_encryption_strategy(security_manager, aes_encryption)
  CacheSecurityManager::add_encryption_strategy(security_manager, field_level_encryption)
  CacheSecurityManager::add_encryption_strategy(security_manager, tokenization)
  
  // 配置字段级加密规则
  FieldLevelEncryptionStrategy::add_sensitive_field(field_level_encryption, "user.id")
  FieldLevelEncryptionStrategy::add_sensitive_field(field_level_encryption, "credit.card")
  FieldLevelEncryptionStrategy::add_sensitive_field(field_level_encryption, "email")
  
  // 配置令牌化规则
  TokenizationStrategy::add_tokenizable_field(tokenization, "phone.number")
  TokenizationStrategy::add_tokenizable_field(tokenization, "social.security")
  
  // 应用安全策略
  CacheSecurityManager::apply_security(security_manager, cache)
  
  // 创建包含敏感数据的追踪
  let sensitive_trace = Trace::new("sensitive.trace", "payment.process", Ok, 500)
  Trace::add_attribute(sensitive_trace, "user.id", "user-12345")
  Trace::add_attribute(sensitive_trace, "credit.card", "4111-1111-1111-1111")
  Trace::add_attribute(sensitive_trace, "email", "user@example.com")
  Trace::add_attribute(sensitive_trace, "phone.number", "+1-555-123-4567")
  Trace::add_attribute(sensitive_trace, "social.security", "123-45-6789")
  Trace::add_attribute(sensitive_trace, "non.sensitive", "public.data")
  
  // 安全存储
  CacheSecurityManager::put_secure(security_manager, cache, "sensitive.trace", sensitive_trace)
  
  // 安全检索
  let retrieved_trace = CacheSecurityManager::get_secure(security_manager, cache, "sensitive.trace")
  assert_true(retrieved_trace.is_some())
  
  // 验证敏感数据已加密/令牌化
  let retrieved = retrieved_trace.unwrap()
  
  // 敏感字段应该被加密
  let user_id = Trace::get_attribute(retrieved, "user.id")
  let credit_card = Trace::get_attribute(retrieved, "credit.card")
  let email = Trace::get_attribute(retrieved, "email")
  
  assert_ne(user_id, Some(StringValue("user-12345")))
  assert_ne(credit_card, Some(StringValue("4111-1111-1111-1111")))
  assert_ne(email, Some(StringValue("user@example.com")))
  
  // 令牌化字段应该被替换为令牌
  let phone_number = Trace::get_attribute(retrieved, "phone.number")
  let social_security = Trace::get_attribute(retrieved, "social.security")
  
  assert_true(phone_number.is_some() && phone_number.unwrap().to_string().starts_with("tok_"))
  assert_true(social_security.is_some() && social_security.unwrap().to_string().starts_with("tok_"))
  
  // 非敏感字段应该保持原样
  let non_sensitive = Trace::get_attribute(retrieved, "non.sensitive")
  assert_eq(non_sensitive, Some(StringValue("public.data")))
  
  // 验证安全统计
  let security_stats = CacheSecurityManager::get_security_stats(security_manager)
  assert_eq(security_stats.encrypted_fields, 3)
  assert_eq(security_stats.tokenized_fields, 2)
  assert_eq(security_stats.secure_operations, 2)  // 1 put + 1 get
}

// 辅助函数：创建大的追踪数据
fn create_large_trace() -> Trace {
  let trace = Trace::new("large.trace", "large.operation", Ok, 1000)
  
  // 添加大量span
  for i in 0..=100 {
    let span = Span::new("span-" + i.to_string(), "operation." + i.to_string(), Server)
    Span::add_attribute(span, "large.data", "x".repeat(1000))  // 1KB数据
    Trace::add_span(trace, span)
  }
  
  // 添加大量属性
  for i in 0..=100 {
    Trace::add_attribute(trace, "attr." + i.to_string(), "value." + i.to_string())
  }
  
  trace
}