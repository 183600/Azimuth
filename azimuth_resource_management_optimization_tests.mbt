// Azimuth 资源管理优化测试用例
// 测试资源管理功能，包括内存优化、连接池管理和缓存策略

// 测试1: 内存池管理
test "内存池管理功能" {
  // 创建内存池管理器
  let memory_pool = MemoryPoolManager::new("telemetry.memory.pool")
  
  // 配置内存池
  MemoryPoolManager::configure_pool(memory_pool, 
    PoolConfig::new(1024, 100, 10, 20)) // 块大小, 初始数量, 最小数量, 最大数量
  
  // 测试内存分配
  let allocated_blocks = []
  for i = 0; i < 50; i = i + 1 {
    let block = MemoryPoolManager::allocate(memory_pool, 1024)
    assert_true(MemoryBlock::is_valid(block))
    allocated_blocks.push(block)
  }
  
  // 验证池状态
  let pool_stats = MemoryPoolManager::get_pool_statistics(memory_pool)
  assert_eq(pool_stats.allocated_count, 50)
  assert_eq(pool_stats.available_count, 50) // 初始100 - 已分配50
  assert_eq(pool_stats.total_count, 100)
  
  // 测试内存释放
  for i = 0; i < 25; i = i + 1 {
    MemoryPoolManager::deallocate(memory_pool, allocated_blocks[i])
  }
  
  // 验证释放后的池状态
  let updated_stats = MemoryPoolManager::get_pool_statistics(memory_pool)
  assert_eq(updated_stats.allocated_count, 25)
  assert_eq(updated_stats.available_count, 75)
  
  // 测试内存池扩展
  let additional_blocks = []
  for i = 0; i < 80; i = i + 1 {
    let block = MemoryPoolManager::allocate(memory_pool, 1024)
    additional_blocks.push(block)
  }
  
  // 验证池扩展
  let expanded_stats = MemoryPoolManager::get_pool_statistics(memory_pool)
  assert_eq(expanded_stats.allocated_count, 105) // 25 + 80
  assert_true(expanded_stats.total_count > 100) // 池应该已扩展
  
  // 测试内存池收缩
  // 释放所有块
  for block in allocated_blocks {
    MemoryPoolManager::deallocate(memory_pool, block)
  }
  for block in additional_blocks {
    MemoryPoolManager::deallocate(memory_pool, block)
  }
  
  // 强制收缩
  MemoryPoolManager::shrink_pool(memory_pool)
  
  let shrunk_stats = MemoryPoolManager::get_pool_statistics(memory_pool)
  assert_eq(shrunk_stats.allocated_count, 0)
  assert_true(shrunk_stats.total_count <= 20) // 不应超过最大数量
  
  // 测试内存使用统计
  let memory_stats = MemoryPoolManager::get_memory_statistics(memory_pool)
  assert_true(memory_stats.total_allocated_bytes > 0)
  assert_true(memory_stats.peak_usage_bytes > 0)
  assert_true(memory_stats.allocation_count > 0)
  assert_true(memory_stats.deallocation_count > 0)
}

// 测试2: 对象池管理
test "对象池管理功能" {
  // 创建对象池管理器
  let object_pool = ObjectPoolManager::new("telemetry.object.pool")
  
  // 配置span对象池
  ObjectPoolManager::configure_pool(object_pool, "span", 
    ObjectPoolConfig::new(fn() { SpanData::empty() }, 50, 5, 100))
  
  // 测试对象借用
  let borrowed_spans = []
  for i = 0; i < 30; i = i + 1 {
    let span = ObjectPoolManager::borrow(object_pool, "span")
    assert_true(SpanData::is_empty(span))
    SpanData::set_name(span, "test.operation." + i.to_string())
    borrowed_spans.push(span)
  }
  
  // 验证池状态
  let pool_stats = ObjectPoolManager::get_pool_statistics(object_pool, "span")
  assert_eq(pool_stats.borrowed_count, 30)
  assert_eq(pool_stats.available_count, 20) // 初始50 - 已借用30
  
  // 测试对象归还
  for i = 0; i < 15; i = i + 1 {
    ObjectPoolManager::return_object(object_pool, "span", borrowed_spans[i])
  }
  
  // 验证归还后的池状态
  let updated_stats = ObjectPoolManager::get_pool_statistics(object_pool, "span")
  assert_eq(updated_stats.borrowed_count, 15)
  assert_eq(updated_stats.available_count, 35)
  
  // 测试对象重置
  let returned_span = borrowed_spans[0]
  assert_eq(SpanData::name(returned_span), "test.operation.0")
  
  ObjectPoolManager::return_object(object_pool, "span", returned_span)
  
  // 借用新对象应该得到重置的对象
  let reset_span = ObjectPoolManager::borrow(object_pool, "span")
  assert_true(SpanData::is_empty(reset_span)) // 对象应该已被重置
  
  // 测试对象池扩展
  let additional_spans = []
  for i = 0; i < 60; i = i + 1 {
    let span = ObjectPoolManager::borrow(object_pool, "span")
    additional_spans.push(span)
  }
  
  // 验证池扩展
  let expanded_stats = ObjectPoolManager::get_pool_statistics(object_pool, "span")
  assert_eq(expanded_stats.borrowed_count, 75) // 15 + 60
  assert_true(expanded_stats.total_count > 50) // 池应该已扩展
  
  // 清理
  for span in borrowed_spans {
    ObjectPoolManager::return_object(object_pool, "span", span)
  }
  for span in additional_spans {
    ObjectPoolManager::return_object(object_pool, "span", span)
  }
}

// 测试3: 连接池管理
test "连接池管理功能" {
  // 创建连接池管理器
  let connection_pool = ConnectionPoolManager::new("telemetry.connection.pool")
  
  // 配置HTTP连接池
  ConnectionPoolManager::configure_http_pool(connection_pool, 
    HttpPoolConfig::new("http://localhost:8080", 10, 2, 50, 30000)) // URL, 最大连接, 最小连接, 超时
  
  // 测试连接获取
  let connections = []
  for i = 0; i < 8; i = i + 1 {
    let connection = ConnectionPoolManager::get_connection(connection_pool, "http://localhost:8080")
    assert_true(HttpConnection::is_valid(connection))
    connections.push(connection)
  }
  
  // 验证池状态
  let pool_stats = ConnectionPoolManager::get_pool_statistics(connection_pool, "http://localhost:8080")
  assert_eq(pool_stats.active_connections, 8)
  assert_eq(pool_stats.idle_connections, 2) // 最小连接数
  assert_eq(pool_stats.total_connections, 10)
  
  // 测试连接释放
  for i = 0; i < 4; i = i + 1 {
    ConnectionPoolManager::release_connection(connection_pool, connections[i])
  }
  
  // 验证释放后的池状态
  let updated_stats = ConnectionPoolManager::get_pool_statistics(connection_pool, "http://localhost:8080")
  assert_eq(updated_stats.active_connections, 4)
  assert_eq(updated_stats.idle_connections, 6)
  
  // 测试连接健康检查
  let unhealthy_connection = connections[4]
  HttpConnection::mark_unhealthy(unhealthy_connection)
  
  ConnectionPoolManager::release_connection(connection_pool, unhealthy_connection)
  
  // 不健康的连接应该被移除
  let health_check_stats = ConnectionPoolManager::get_pool_statistics(connection_pool, "http://localhost:8080")
  assert_true(health_check_stats.total_connections < 10) // 不健康的连接应该被移除
  
  // 测试连接超时处理
  let timeout_connection = connections[5]
  HttpConnection::set_last_used(timeout_connection, 0) // 设置为很久以前
  
  ConnectionPoolManager::release_connection(connection_pool, timeout_connection)
  
  // 触发超时清理
  ConnectionPoolManager::cleanup_expired_connections(connection_pool)
  
  let timeout_stats = ConnectionPoolManager::get_pool_statistics(connection_pool, "http://localhost:8080")
  assert_true(timeout_stats.total_connections < health_check_stats.total_connections) // 超时连接应该被清理
  
  // 清理剩余连接
  for i = 6; i < 8; i = i + 1 {
    ConnectionPoolManager::release_connection(connection_pool, connections[i])
  }
}

// 测试4: 缓存管理
test "缓存管理功能" {
  // 创建缓存管理器
  let cache_manager = CacheManager::new("telemetry.cache")
  
  // 配置LRU缓存
  CacheManager::configure_lru_cache(cache_manager, "span.cache", 1000, 300) // 容量, TTL(秒)
  
  // 测试缓存写入
  for i = 0; i < 500; i = i + 1 {
    let key = "span." + i.to_string()
    let value = SpanData::new("test.operation." + i.to_string(), 1000 + i, 1100 + i, [])
    CacheManager::put(cache_manager, "span.cache", key, value)
  }
  
  // 验证缓存状态
  let cache_stats = CacheManager::get_cache_statistics(cache_manager, "span.cache")
  assert_eq(cache_stats.size, 500)
  assert_eq(cache_stats.hits, 0)
  assert_eq(cache_stats.misses, 0)
  
  // 测试缓存读取
  let hit_count = 0
  for i = 0; i < 100; i = i + 1 {
    let key = "span." + i.to_string()
    let cached_value = CacheManager::get(cache_manager, "span.cache", key)
    match cached_value {
      Some(span) => {
        assert_eq(SpanData::name(span), "test.operation." + i.to_string())
        hit_count = hit_count + 1
      }
      None => assert_true(false)
    }
  }
  
  // 测试缓存未命中
  let miss_count = 0
  for i = 500; i < 550; i = i + 1 {
    let key = "span." + i.to_string()
    let cached_value = CacheManager::get(cache_manager, "span.cache", key)
    match cached_value {
      Some(_) => assert_true(false)
      None => miss_count = miss_count + 1
    }
  }
  
  // 验证缓存统计
  let updated_stats = CacheManager::get_cache_statistics(cache_manager, "span.cache")
  assert_eq(updated_stats.hits, hit_count)
  assert_eq(updated_stats.misses, miss_count)
  assert_true(updated_stats.hit_rate > 0.0)
  
  // 测试缓存淘汰
  for i = 500; i < 1200; i = i + 1 {
    let key = "span." + i.to_string()
    let value = SpanData::new("test.operation." + i.to_string(), 1000 + i, 1100 + i, [])
    CacheManager::put(cache_manager, "span.cache", key, value)
  }
  
  // 验证缓存容量限制
  let eviction_stats = CacheManager::get_cache_statistics(cache_manager, "span.cache")
  assert_eq(eviction_stats.size, 1000) // 不应超过容量
  assert_true(eviction_stats.evictions > 0) // 应该有淘汰
  
  // 测试TTL过期
  CacheManager::configure_lru_cache(cache_manager, "ttl.cache", 100, 1) // 1秒TTL
  
  let ttl_key = "ttl.test"
  let ttl_value = "test.value"
  CacheManager::put(cache_manager, "ttl.cache", ttl_key, ttl_value)
  
  // 立即读取应该成功
  let immediate_result = CacheManager::get(cache_manager, "ttl.cache", ttl_key)
  assert_true(immediate_result.is_some())
  
  // 等待TTL过期（模拟）
  CacheManager::advance_time(cache_manager, 2000) // 前进2秒
  
  // 过期后读取应该失败
  let expired_result = CacheManager::get(cache_manager, "ttl.cache", ttl_key)
  assert_true(expired_result.is_none())
  
  // 验证过期统计
  let ttl_stats = CacheManager::get_cache_statistics(cache_manager, "ttl.cache")
  assert_true(ttl_stats.expirations > 0)
}

// 测试5: 资源限制和配额管理
test "资源限制和配额管理功能" {
  // 创建资源限制管理器
  let quota_manager = ResourceQuotaManager::new("telemetry.quotas")
  
  // 配置内存配额
  QuotaManager::set_memory_quota(quota_manager, "spans", 100 * 1024 * 1024) // 100MB
  QuotaManager::set_memory_quota(quota_manager, "metrics", 50 * 1024 * 1024)  // 50MB
  QuotaManager::set_memory_quota(quota_manager, "logs", 200 * 1024 * 1024)   // 200MB
  
  // 配置对象数量配额
  QuotaManager::set_object_quota(quota_manager, "spans", 10000)
  QuotaManager::set_object_quota(quota_manager, "metrics", 50000)
  QuotaManager::set_object_quota(quota_manager, "logs", 100000)
  
  // 测试内存配额检查
  let span_memory_usage = 1024 // 假设每个span占用1KB
  let can_allocate_spans = QuotaManager::check_memory_quota(quota_manager, "spans", span_memory_usage * 1000)
  assert_true(can_allocate_spans) // 1000个span应该可以分配
  
  let can_allocate_too_many_spans = QuotaManager::check_memory_quota(quota_manager, "spans", 200 * 1024 * 1024)
  assert_false(can_allocate_too_many_spans) // 超过配额应该失败
  
  // 测试对象数量配额检查
  let can_create_spans = QuotaManager::check_object_quota(quota_manager, "spans", 5000)
  assert_true(can_create_spans) // 5000个span应该可以创建
  
  let can_create_too_many_spans = QuotaManager::check_object_quota(quota_manager, "spans", 15000)
  assert_false(can_create_too_many_spans) // 超过配额应该失败
  
  // 测试配额使用跟踪
  QuotaManager::allocate_memory(quota_manager, "spans", span_memory_usage * 1000)
  QuotaManager::allocate_objects(quota_manager, "spans", 1000)
  
  let span_quota_usage = QuotaManager::get_quota_usage(quota_manager, "spans")
  assert_eq(span_quota_usage.memory_used, span_memory_usage * 1000)
  assert_eq(span_quota_usage.objects_used, 1000)
  
  // 测试配额释放
  QuotaManager::release_memory(quota_manager, "spans", span_memory_usage * 500)
  QuotaManager::release_objects(quota_manager, "spans", 500)
  
  let updated_usage = QuotaManager::get_quota_usage(quota_manager, "spans")
  assert_eq(updated_usage.memory_used, span_memory_usage * 500)
  assert_eq(updated_usage.objects_used, 500)
  
  // 测试配额超限处理
  let alert_triggered = false
  QuotaManager::set_quota_alert_handler(quota_manager, fn(resource_type, usage, limit) {
    if resource_type == "spans" && usage.memory_used > limit.memory * 0.8 {
      alert_triggered = true
    }
  })
  
  // 分配接近配额限制
  QuotaManager::allocate_memory(quota_manager, "spans", 80 * 1024 * 1024)
  
  assert_true(alert_triggered) // 应该触发警报
  
  // 测试配额优先级管理
  QuotaManager::set_quota_priority(quota_manager, "spans", Priority::High)
  QuotaManager::set_quota_priority(quota_manager, "metrics", Priority::Medium)
  QuotaManager::set_quota_priority(quota_manager, "logs", Priority::Low)
  
  // 在资源紧张时，高优先级资源应该有更多分配机会
  let memory_pressure = true
  let high_priority_allocation = QuotaManager::allocate_with_priority(quota_manager, "spans", 1024, memory_pressure)
  let low_priority_allocation = QuotaManager::allocate_with_priority(quota_manager, "logs", 1024, memory_pressure)
  
  assert_true(high_priority_allocation) // 高优先级应该成功
  assert_false(low_priority_allocation) // 低优先级可能失败
}

// 测试6: 资源监控和报告
test "资源监控和报告功能" {
  // 创建资源监控器
  let resource_monitor = ResourceMonitor::new("telemetry.monitor")
  
  // 启动监控
  ResourceMonitor::start_monitoring(resource_monitor, 1000) // 1秒间隔
  
  // 创建一些资源使用
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "monitor.test")
  
  let spans = []
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(tracer, "monitor.operation." + i.to_string())
    Span::set_attribute(span, "operation.id", i.to_string())
    spans.push(span)
  }
  
  // 等待监控收集数据
  ResourceMonitor::wait_for_collection(resource_monitor, 2000)
  
  // 获取资源使用报告
  let resource_report = ResourceMonitor::generate_report(resource_monitor)
  
  // 验证报告内容
  assert_true(resource_report.contains("内存使用"))
  assert_true(resource_report.contains("对象数量"))
  assert_true(resource_report.contains("CPU使用"))
  assert_true(resource_report.contains("网络IO"))
  
  // 获取详细统计
  let detailed_stats = ResourceMonitor::get_detailed_statistics(resource_monitor)
  assert_true(detailed_stats.memory_usage > 0)
  assert_true(detailed_stats.object_count > 0)
  assert_true(detailed_stats.cpu_usage >= 0.0)
  assert_true(detailed_stats.cpu_usage <= 100.0)
  
  // 测试资源使用趋势
  let memory_trend = ResourceMonitor::get_memory_trend(resource_monitor, 5) // 最近5个数据点
  assert_true(memory_trend.length() <= 5)
  
  // 测试资源使用预测
  let memory_prediction = ResourceMonitor::predict_memory_usage(resource_monitor, 3600) // 预测1小时后
  assert_true(memory_prediction > detailed_stats.memory_usage) // 预测值应该大于当前值（如果趋势向上）
  
  // 测试资源使用警报
  let alert_triggered = false
  ResourceMonitor::set_memory_alert_threshold(resource_monitor, 50 * 1024 * 1024) // 50MB
  ResourceMonitor::set_alert_handler(resource_monitor, fn(alert_type, current_value, threshold) {
    if alert_type == AlertType::MemoryUsage && current_value > threshold {
      alert_triggered = true
    }
  })
  
  // 创建更多资源使用以触发警报
  let additional_spans = []
  for i = 0; i < 1000; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory.stress.operation." + i.to_string())
    Span::set_attribute(span, "large.data", "x".repeat(1000)) // 增加内存使用
    additional_spans.push(span)
  }
  
  // 等待警报触发
  ResourceMonitor::wait_for_collection(resource_monitor, 2000)
  
  assert_true(alert_triggered) // 应该触发内存警报
  
  // 测试资源优化建议
  let optimization_suggestions = ResourceMonitor::get_optimization_suggestions(resource_monitor)
  assert_true(optimization_suggestions.length() > 0)
  
  // 清理
  for span in spans {
    Span::end(span)
  }
  for span in additional_spans {
    Span::end(span)
  }
  
  // 停止监控
  ResourceMonitor::stop_monitoring(resource_monitor)
}