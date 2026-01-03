// Azimuth 内存管理和资源清理测试用例
// 测试遥测系统的内存管理效率和资源清理能力

test "内存池管理和复用" {
  // 创建内存池管理器
  let memory_pool = MemoryPoolManager::new()
  MemoryPoolManager::set_pool_size(memory_pool, 1024 * 1024) // 1MB池
  MemoryPoolManager::set_block_size(memory_pool, 1024) // 1KB块
  MemoryPoolManager::set_max_blocks(memory_pool, 1024) // 最大1024块
  
  // 验证内存池初始化
  assert_true(MemoryPoolManager::is_initialized(memory_pool), "内存池应该被正确初始化")
  assert_eq(MemoryPoolManager::get_available_blocks(memory_pool), 1024, "初始应该有1024个可用块")
  assert_eq(MemoryPoolManager::get_used_blocks(memory_pool), 0, "初始应该没有使用的块")
  
  // 创建遥测提供者并启用内存池
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_memory_pool_enabled(config, true)
  TelemetryConfiguration::set_memory_pool_manager(config, memory_pool)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.pool.test")
  
  // 创建大量span以测试内存池复用
  let spans = []
  let initial_available_blocks = MemoryPoolManager::get_available_blocks(memory_pool)
  
  for i = 0; i < 500; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory.pool.span." + i.to_string())
    
    // 为每个span添加属性以使用内存
    Span::set_attribute(span, "test.index", i.to_string())
    Span::set_attribute(span, "pool.test", "true")
    Span::set_attribute(span, "memory.efficient", "true")
    
    spans = spans + [span]
    
    // 每100个span检查一次内存池状态
    if i % 100 == 0 {
      let current_used_blocks = MemoryPoolManager::get_used_blocks(memory_pool)
      assert_true(current_used_blocks <= 500, "内存池使用应该被控制")
    }
  }
  
  // 结束所有span以释放内存回池
  for span in spans {
    Span::end(span)
  }
  
  // 验证内存被正确释放回池
  let final_available_blocks = MemoryPoolManager::get_available_blocks(memory_pool)
  assert_true(final_available_blocks >= initial_available_blocks - 10, 
    "大部分内存块应该被释放回池") // 允许一些块被保留用于缓存
  
  // 测试内存池碎片整理
  let before_fragmentation = MemoryPoolManager::get_fragmentation_ratio(memory_pool)
  MemoryPoolManager::defragment(memory_pool)
  let after_fragmentation = MemoryPoolManager::get_fragmentation_ratio(memory_pool)
  
  assert_true(after_fragmentation <= before_fragmentation, 
    "碎片整理应该减少或保持碎片率")
  
  // 测试内存池扩容
  MemoryPoolManager::expand_pool(memory_pool, 512 * 1024) // 扩展512KB
  let expanded_size = MemoryPoolManager::get_pool_size(memory_pool)
  assert_eq(expanded_size, 1536 * 1024, "内存池应该被正确扩展")
  
  // 测试内存池收缩
  MemoryPoolManager::shrink_pool(memory_pool, 256 * 1024) // 收缩256KB
  let shrunk_size = MemoryPoolManager::get_pool_size(memory_pool)
  assert_eq(shrunk_size, 1280 * 1024, "内存池应该被正确收缩")
  
  // 验证内存池统计信息
  let pool_stats = MemoryPoolManager::get_statistics(memory_pool)
  assert_true(pool_stats.total_allocations > 0, "应该有分配记录")
  assert_true(pool_stats.total_deallocations > 0, "应该有释放记录")
  assert_true(pool_stats.peak_usage > 0, "应该记录峰值使用")
  assert_true(pool_stats.efficiency_score > 0.8, "内存池效率应该高")
  
  assert_true(true)
}

test "智能垃圾回收机制" {
  // 创建智能垃圾回收管理器
  let gc_manager = SmartGCManager::new()
  SmartGCManager::set_strategy(gc_manager, GenerationalGC)
  SmartGCManager::set_threshold(gc_manager, 80.0) // 80%内存使用率触发GC
  SmartGCManager::set_max_pause_time(gc_manager, 100) // 最大暂停时间100ms
  SmartGCManager::set_adaptive_gc(gc_manager, true) // 启用自适应GC
  
  // 创建遥测提供者并启用智能GC
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_smart_gc_enabled(config, true)
  TelemetryConfiguration::set_gc_manager(config, gc_manager)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "smart.gc.test")
  
  // 记录初始内存状态
  let initial_memory = SmartGCManager::get_memory_usage(gc_manager)
  let initial_gc_count = SmartGCManager::get_gc_count(gc_manager)
  
  // 创建大量临时对象以触发GC
  let temporary_objects = []
  for i = 0; i < 1000; i = i + 1 {
    let temp_span = Tracer::start_span(tracer, "temp.span." + i.to_string())
    
    // 创建大量临时属性
    for j = 0; j < 10; j = j + 1 {
      let temp_value = "temp.value." + i.to_string() + "." + j.to_string()
      Span::set_attribute(temp_span, "temp.attr." + j.to_string(), temp_value)
    }
    
    temporary_objects = temporary_objects + [temp_span]
    
    // 每100个对象检查一次内存使用
    if i % 100 == 0 {
      let current_memory = SmartGCManager::get_memory_usage(gc_manager)
      if current_memory > initial_memory * 1.5 {
        // 内存使用增长过快，应该触发GC
        break
      }
    }
  }
  
  // 释放临时对象的引用
  temporary_objects = []
  
  // 等待GC完成
  let gc_timeout = 10000 // 10秒超时
  let start_time = Timestamp::now()
  let gc_triggered = false
  
  while Timestamp::difference(Timestamp::now(), start_time).to_millis() < gc_timeout {
    let current_gc_count = SmartGCManager::get_gc_count(gc_manager)
    if current_gc_count > initial_gc_count {
      gc_triggered = true
      break
    }
    simulate_delay(100) // 等待100ms
  }
  
  assert_true(gc_triggered, "释放大量对象后应该触发GC")
  
  // 验证GC效果
  let final_memory = SmartGCManager::get_memory_usage(gc_manager)
  assert_true(final_memory < initial_memory * 1.3, "GC后内存使用应该显著减少")
  
  // 验证GC暂停时间
  let gc_stats = SmartGCManager::get_gc_statistics(gc_manager)
  assert_true(gc_stats.average_pause_time < 100, "平均GC暂停时间应该小于100ms")
  assert_true(gc_stats.max_pause_time < 200, "最大GC暂停时间应该小于200ms")
  
  // 测试分代GC效率
  let generational_stats = SmartGCManager::get_generational_stats(gc_manager)
  assert_true(generational_stats.young_generation_collections > 0, "应该有年轻代收集")
  assert_true(generational_stats.old_generation_collections >= 0, "可能有老年代收集")
  assert_true(generational_stats.promotion_rate > 0.0, "应该有对象晋升")
  
  // 测试自适应GC
  let adaptive_stats = SmartGCManager::get_adaptive_stats(gc_manager)
  assert_true(adaptive_stats.threshold_adjustments > 0, "自适应GC应该调整阈值")
  assert_true(adaptive_stats.strategy_changes >= 0, "可能有策略变化")
  
  // 测试GC在压力下的表现
  let stress_test_spans = []
  for i = 0; i < 2000; i = i + 1 {
    let stress_span = Tracer::start_span(tracer, "stress.span." + i.to_string())
    
    // 创建大量属性以增加内存压力
    for j = 0; j < 20; j = j + 1 {
      let large_value = "x" * 100 // 100字符字符串
      Span::set_attribute(stress_span, "stress.attr." + j.to_string(), large_value)
    }
    
    stress_test_spans = stress_test_spans + [stress_span]
    
    // 在高内存压力下，GC应该更频繁
    if i % 200 == 0 {
      let current_memory = SmartGCManager::get_memory_usage(gc_manager)
      if current_memory > initial_memory * 2.0 {
        // 强制触发GC
        SmartGCManager::force_gc(gc_manager)
        break
      }
    }
  }
  
  // 清理压力测试对象
  for stress_span in stress_test_spans {
    Span::end(stress_span)
  }
  
  // 验证最终内存状态
  let final_gc_count = SmartGCManager::get_gc_count(gc_manager)
  assert_true(final_gc_count > initial_gc_count, "压力测试期间应该有多次GC")
  
  let final_memory_after_cleanup = SmartGCManager::get_memory_usage(gc_manager)
  assert_true(final_memory_after_cleanup < final_memory, "清理后内存应该进一步减少")
  
  // 生成GC性能报告
  let gc_performance_report = SmartGCManager::generate_performance_report(gc_manager)
  assert_true(gc_performance_report.total_gc_time > 0, "应该有GC时间统计")
  assert_true(gc_performance_report.gc_efficiency > 0.7, "GC效率应该高")
  assert_true(gc_performance_report.memory_recovery_rate > 0.6, "内存回收率应该高")
  
  assert_true(true)
}

test "资源生命周期管理" {
  // 创建资源生命周期管理器
  let lifecycle_manager = ResourceLifecycleManager::new()
  ResourceLifecycleManager::set_auto_cleanup(lifecycle_manager, true)
  ResourceLifecycleManager::set_cleanup_interval(lifecycle_manager, 5000)
  ResourceLifecycleManager::set_max_resource_age(lifecycle_manager, 30000) // 30秒最大资源年龄
  
  // 创建遥测提供者并启用生命周期管理
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_lifecycle_management_enabled(config, true)
  TelemetryConfiguration::set_lifecycle_manager(config, lifecycle_manager)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  // 创建具有不同生命周期的资源
  let short_lived_spans = []
  let medium_lived_spans = []
  let long_lived_spans = []
  
  // 创建短期资源（5秒生命周期）
  for i = 0; i < 50; i = i + 1 {
    let span = Tracer::start_span(tracer, "short.lived.span." + i.to_string())
    Span::set_attribute(span, "lifecycle.type", "short")
    Span::set_attribute(span, "expected.lifetime", "5s")
    ResourceLifecycleManager::set_resource_lifetime(lifecycle_manager, span, 5000)
    short_lived_spans = short_lived_spans + [span]
  }
  
  // 创建中期资源（15秒生命周期）
  for i = 0; i < 30; i = i + 1 {
    let span = Tracer::start_span(tracer, "medium.lived.span." + i.to_string())
    Span::set_attribute(span, "lifecycle.type", "medium")
    Span::set_attribute(span, "expected.lifetime", "15s")
    ResourceLifecycleManager::set_resource_lifetime(lifecycle_manager, span, 15000)
    medium_lived_spans = medium_lived_spans + [span]
  }
  
  // 创建长期资源（60秒生命周期）
  for i = 0; i < 20; i = i + 1 {
    let span = Tracer::start_span(tracer, "long.lived.span." + i.to_string())
    Span::set_attribute(span, "lifecycle.type", "long")
    Span::set_attribute(span, "expected.lifetime", "60s")
    ResourceLifecycleManager::set_resource_lifetime(lifecycle_manager, span, 60000)
    long_lived_spans = long_lived_spans + [span]
  }
  
  // 验证资源被正确注册
  let initial_resource_count = ResourceLifecycleManager::get_resource_count(lifecycle_manager)
  assert_eq(initial_resource_count, 100, "应该注册100个资源")
  
  // 等待短期资源过期
  simulate_delay(6000) // 等待6秒
  
  // 触发生命周期检查
  ResourceLifecycleManager::check_lifecycles(lifecycle_manager)
  
  // 验证短期资源被清理
  let after_short_cleanup = ResourceLifecycleManager::get_resource_count(lifecycle_manager)
  assert_eq(after_short_cleanup, 50, "短期资源应该被清理，剩余50个")
  
  // 验证清理统计
  let cleanup_stats = ResourceLifecycleManager::get_cleanup_statistics(lifecycle_manager)
  assert_eq(cleanup_stats.short_lived_cleaned, 50, "应该清理50个短期资源")
  
  // 等待中期资源过期
  simulate_delay(10000) // 再等待10秒（总共16秒）
  
  // 再次触发生命周期检查
  ResourceLifecycleManager::check_lifecycles(lifecycle_manager)
  
  // 验证中期资源被清理
  let after_medium_cleanup = ResourceLifecycleManager::get_resource_count(lifecycle_manager)
  assert_eq(after_medium_cleanup, 20, "中期资源应该被清理，剩余20个")
  
  // 测试资源依赖关系
  let parent_span = Tracer::start_span(tracer, "parent.span")
  let child_spans = []
  
  for i = 0; i < 10; i = i + 1 {
    let child_span = Tracer::start_span(tracer, "child.span." + i.to_string())
    ResourceLifecycleManager::add_dependency(lifecycle_manager, child_span, parent_span)
    child_spans = child_spans + [child_span]
  }
  
  // 设置父span为短期资源
  ResourceLifecycleManager::set_resource_lifetime(lifecycle_manager, parent_span, 3000)
  
  // 等待父span过期
  simulate_delay(4000)
  
  // 触发生命周期检查
  ResourceLifecycleManager::check_lifecycles(lifecycle_manager)
  
  // 验证依赖资源也被清理
  let final_resource_count = ResourceLifecycleManager::get_resource_count(lifecycle_manager)
  assert_true(final_resource_count < 20, "依赖资源应该随父资源一起被清理")
  
  // 测试资源优先级
  let high_priority_span = Tracer::start_span(tracer, "high.priority.span")
  let low_priority_span = Tracer::start_span(tracer, "low.priority.span")
  
  ResourceLifecycleManager::set_resource_priority(lifecycle_manager, high_priority_span, High)
  ResourceLifecycleManager::set_resource_priority(lifecycle_manager, low_priority_span, Low)
  
  // 在内存压力下测试优先级清理
  ResourceLifecycleManager::simulate_memory_pressure(lifecycle_manager, 90.0) // 90%内存使用率
  ResourceLifecycleManager::check_lifecycles(lifecycle_manager)
  
  // 验证低优先级资源先被清理
  let high_priority_exists = ResourceLifecycleManager::resource_exists(lifecycle_manager, high_priority_span)
  let low_priority_exists = ResourceLifecycleManager::resource_exists(lifecycle_manager, low_priority_span)
  
  assert_true(high_priority_exists, "高优先级资源应该保留")
  assert_false(low_priority_exists, "低优先级资源应该被清理")
  
  // 测试手动资源清理
  let manual_cleanup_span = Tracer::start_span(tracer, "manual.cleanup.span")
  ResourceLifecycleManager::set_resource_lifetime(lifecycle_manager, manual_cleanup_span, 60000) // 60秒
  
  // 手动清理资源
  ResourceLifecycleManager::cleanup_resource(lifecycle_manager, manual_cleanup_span)
  
  // 验证资源被立即清理
  let manual_cleanup_exists = ResourceLifecycleManager::resource_exists(lifecycle_manager, manual_cleanup_span)
  assert_false(manual_cleanup_exists, "手动清理应该立即生效")
  
  // 生成生命周期管理报告
  let lifecycle_report = ResourceLifecycleManager::generate_lifecycle_report(lifecycle_manager)
  assert_true(lifecycle_report.total_resources_managed > 0, "应该管理了资源")
  assert_true(lifecycle_report.total_resources_cleaned > 0, "应该清理了资源")
  assert_true(lifecycle_report.cleanup_efficiency > 0.8, "清理效率应该高")
  
  assert_true(true)
}

test "文件句柄和网络连接管理" {
  // 创建文件和网络资源管理器
  let file_manager = FileResourceManager::new()
  FileResourceManager::set_max_open_files(file_manager, 100)
  FileResourceManager::set_auto_close(file_manager, true)
  FileResourceManager::set_idle_timeout(file_manager, 30000) // 30秒空闲超时
  
  let network_manager = NetworkResourceManager::new()
  NetworkResourceManager::set_max_connections(network_manager, 50)
  NetworkResourceManager::set_connection_timeout(network_manager, 10000) // 10秒连接超时
  NetworkResourceManager::set_idle_timeout(network_manager, 15000) // 15秒空闲超时
  
  // 创建遥测提供者并启用资源管理
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_file_resource_manager(config, file_manager)
  TelemetryConfiguration::set_network_resource_manager(config, network_manager)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.management.test")
  
  // 测试文件句柄管理
  let file_handles = []
  for i = 0; i < 80; i = i + 1 {
    let file_path = "/tmp/test_file_" + i.to_string() + ".log"
    let file_handle = FileResourceManager::open_file(file_manager, file_path, "w")
    
    if file_handle.is_some() {
      file_handles = file_handles + [file_handle.unwrap()]
      
      // 写入一些测试数据
      FileResourceManager::write_file(file_manager, file_handle.unwrap(), 
        "Test log entry " + i.to_string() + "\n")
    }
  }
  
  // 验证文件句柄被正确管理
  let open_files = FileResourceManager::get_open_files_count(file_manager)
  assert_true(open_files <= 100, "打开的文件数不应超过限制")
  
  // 测试文件句柄自动关闭
  for file_handle in file_handles.slice(0, 40) {
    FileResourceManager::mark_idle(file_manager, file_handle)
  }
  
  // 等待空闲超时的一半时间
  simulate_delay(15000)
  
  // 手动触发资源清理
  FileResourceManager::cleanup_idle_resources(file_manager)
  
  // 验证部分文件被关闭
  let files_after_partial_cleanup = FileResourceManager::get_open_files_count(file_manager)
  assert_true(files_after_partial_cleanup < open_files, "空闲文件应该被部分关闭")
  
  // 等待剩余空闲超时
  simulate_delay(15000)
  
  // 再次触发清理
  FileResourceManager::cleanup_idle_resources(file_manager)
  
  // 验证更多文件被关闭
  let files_after_full_cleanup = FileResourceManager::get_open_files_count(file_manager)
  assert_true(files_after_full_cleanup < files_after_partial_cleanup, 
    "更多空闲文件应该被关闭")
  
  // 测试网络连接管理
  let connections = []
  for i = 0; i < 40; i = i + 1 {
    let endpoint = "http://telemetry.server" + i.to_string() + ".example.com/api/v1/traces"
    let connection = NetworkResourceManager::create_connection(network_manager, endpoint)
    
    if connection.is_some() {
      connections = connections + [connection.unwrap()]
      
      // 发送一些测试数据
      NetworkResourceManager::send_data(network_manager, connection.unwrap(), 
        "test telemetry data " + i.to_string())
    }
  }
  
  // 验证连接被正确管理
  let active_connections = NetworkResourceManager::get_active_connections_count(network_manager)
  assert_true(active_connections <= 50, "活跃连接数不应超过限制")
  
  // 测试连接池
  let connection_pool = NetworkResourceManager::get_connection_pool(network_manager)
  assert_true(NetworkResourceManager::is_connection_pool_enabled(network_manager), 
    "连接池应该被启用")
  
  // 测试连接复用
  for connection in connections.slice(0, 10) {
    NetworkResourceManager::return_to_pool(network_manager, connection)
  }
  
  // 从池中获取连接
  let pooled_connections = []
  for i = 0; i < 10; i = i + 1 {
    let pooled_connection = NetworkResourceManager::get_from_pool(network_manager, 
      "http://telemetry.server" + i.to_string() + ".example.com/api/v1/traces")
    
    if pooled_connection.is_some() {
      pooled_connections = pooled_connections + [pooled_connection.unwrap()]
    }
  }
  
  // 验证连接复用
  assert_true(pooled_connections.length() > 0, "应该能够从池中获取连接")
  
  // 测试连接超时
  for connection in connections.slice(10, 20) {
    NetworkResourceManager::mark_idle(network_manager, connection)
  }
  
  // 等待连接空闲超时
  simulate_delay(15000)
  
  // 触发连接清理
  NetworkResourceManager::cleanup_idle_connections(network_manager)
  
  // 验证空闲连接被关闭
  let connections_after_cleanup = NetworkResourceManager::get_active_connections_count(network_manager)
  assert_true(connections_after_cleanup < active_connections, "空闲连接应该被关闭")
  
  // 测试资源泄漏检测
  let leak_detector = ResourceLeakDetector::new()
  ResourceLeakDetector::add_file_manager(leak_detector, file_manager)
  ResourceLeakDetector::add_network_manager(leak_detector, network_manager)
  
  // 检测资源泄漏
  let leak_report = ResourceLeakDetector::detect_leaks(leak_detector)
  assert_true(leak_report.total_resources > 0, "应该检测到资源使用")
  
  // 如果有泄漏，触发清理
  if leak_report.leaked_resources > 0 {
    ResourceLeakDetector::cleanup_leaks(leak_detector)
    
    let cleanup_leak_report = ResourceLeakDetector::detect_leaks(leak_detector)
    assert_true(cleanup_leak_report.leaked_resources < leak_report.leaked_resources, 
      "清理后泄漏应该减少")
  }
  
  // 测试资源压力下的行为
  ResourceLeakDetector::simulate_resource_pressure(leak_detector, 95.0) // 95%资源使用率
  
  // 在资源压力下创建更多资源
  let pressure_file_handles = []
  for i = 0; i < 30; i = i + 1 {
    let file_path = "/tmp/pressure_test_file_" + i.to_string() + ".log"
    let file_handle = FileResourceManager::open_file(file_manager, file_path, "w")
    
    if file_handle.is_some() {
      pressure_file_handles = pressure_file_handles + [file_handle.unwrap()]
    }
  }
  
  // 验证资源压力下的管理策略
  let pressure_stats = ResourceLeakDetector::get_pressure_stats(leak_detector)
  assert_true(pressure_stats.pressure_relief_applied, "资源压力下应该应用缓解策略")
  assert_true(pressure_stats.resources_closed > 0, "应该关闭一些资源以缓解压力")
  
  // 生成资源管理报告
  let resource_report = FileResourceManager::generate_report(file_manager)
  assert_true(resource_report.total_files_opened > 0, "应该有文件打开记录")
  assert_true(resource_report.total_files_closed > 0, "应该有文件关闭记录")
  assert_true(resource_report.peak_open_files > 0, "应该记录峰值文件数")
  
  let network_report = NetworkResourceManager::generate_report(network_manager)
  assert_true(network_report.total_connections_created > 0, "应该有连接创建记录")
  assert_true(network_report.total_connections_closed > 0, "应该有连接关闭记录")
  assert_true(network_report.peak_connections > 0, "应该记录峰值连接数")
  assert_true(network_report.connection_pool_hits > 0, "应该有连接池命中记录")
  
  assert_true(true)
}

test "缓存管理和内存优化" {
  // 创建缓存管理器
  let cache_manager = CacheManager::new()
  CacheManager::set_max_memory_usage(cache_manager, 50 * 1024 * 1024) // 50MB最大内存
  CacheManager::set_max_entries(cache_manager, 10000) // 最大10000条目
  CacheManager::set_eviction_policy(cache_manager, LRU) // LRU淘汰策略
  CacheManager::set_ttl(cache_manager, 300000) // 5分钟TTL
  
  // 创建遥测提供者并启用缓存管理
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_cache_manager(config, cache_manager)
  TelemetryConfiguration::set_smart_caching_enabled(config, true)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "cache.management.test")
  
  // 测试缓存基本功能
  let cache_keys = []
  let cache_values = []
  
  // 添加缓存条目
  for i = 0; i < 5000; i = i + 1 {
    let key = "cache_key_" + i.to_string()
    let value = "cache_value_" + i.to_string() + "_with_some_additional_data_to_increase_size"
    
    let cache_result = CacheManager::put(cache_manager, key, value)
    assert_true(cache_result.success, "应该能够添加缓存条目")
    
    cache_keys = cache_keys + [key]
    cache_values = cache_values + [value]
  }
  
  // 验证缓存统计
  let cache_stats = CacheManager::get_statistics(cache_manager)
  assert_eq(cache_stats.entries, 5000, "应该有5000个缓存条目")
  assert_true(cache_stats.memory_usage > 0, "应该使用内存")
  assert_true(cache_stats.memory_usage <= 50 * 1024 * 1024, "内存使用不应超过限制")
  
  // 测试缓存检索
  for i = 0; i < 100; i = i + 1 {
    let key = cache_keys[i]
    let cached_value = CacheManager::get(cache_manager, key)
    
    assert_true(cached_value.is_some(), "应该能够检索缓存值")
    assert_eq(cached_value.unwrap(), cache_values[i], "检索的值应该与存储的值匹配")
  }
  
  // 测试缓存命中率
  let hit_rate_before = cache_stats.hit_rate
  assert_true(hit_rate_before > 0.9, "初始缓存命中率应该高")
  
  // 测试缓存更新
  let update_key = cache_keys[0]
  let update_value = "updated_value_for_" + update_key
  let update_result = CacheManager::put(cache_manager, update_key, update_value)
  assert_true(update_result.success, "应该能够更新缓存条目")
  
  let updated_cached_value = CacheManager::get(cache_manager, update_key)
  assert_eq(updated_cached_value.unwrap(), update_value, "更新的值应该被正确存储")
  
  // 测试缓存删除
  let delete_key = cache_keys[1]
  let delete_result = CacheManager::remove(cache_manager, delete_key)
  assert_true(delete_result.success, "应该能够删除缓存条目")
  
  let deleted_cached_value = CacheManager::get(cache_manager, delete_key)
  assert_true(deleted_cached_value.is_none(), "删除的条目应该无法检索")
  
  // 测试缓存淘汰策略
  // 添加更多条目直到超过最大条目数
  for i = 5000; i < 15000; i = i + 1 {
    let key = "overflow_key_" + i.to_string()
    let value = "overflow_value_" + i.to_string()
    
    CacheManager::put(cache_manager, key, value)
  }
  
  // 验证条目数量不超过最大值
  let entries_after_overflow = CacheManager::get_statistics(cache_manager).entries
  assert_true(entries_after_overflow <= 10000, "缓存条目数不应超过最大值")
  
  // 验证旧条目被淘汰
  let old_key_evicted = CacheManager::get(cache_manager, cache_keys[100]).is_none()
  assert_true(old_key_evicted, "旧条目应该被LRU策略淘汰")
  
  // 测试缓存TTL过期
  let ttl_key = "ttl_test_key"
  let ttl_value = "ttl_test_value"
  
  // 设置短TTL的缓存条目
  CacheManager::put_with_ttl(cache_manager, ttl_key, ttl_value, 2000) // 2秒TTL
  
  // 立即检索应该成功
  let immediate_retrieval = CacheManager::get(cache_manager, ttl_key)
  assert_true(immediate_retrieval.is_some(), "TTL条目应该立即可用")
  
  // 等待TTL过期
  simulate_delay(2500)
  
  // 过期后检索应该失败
  let expired_retrieval = CacheManager::get(cache_manager, ttl_key)
  assert_true(expired_retrieval.is_none(), "TTL过期条目应该无法检索")
  
  // 测试缓存内存压力处理
  let memory_usage_before = CacheManager::get_statistics(cache_manager).memory_usage
  
  // 添加大对象以增加内存压力
  for i = 0; i < 100; i = i + 1 {
    let large_key = "large_key_" + i.to_string()
    let large_value = "x" * 100000 // 100KB字符串
    
    CacheManager::put(cache_manager, large_key, large_value)
  }
  
  // 验证内存使用被控制
  let memory_usage_after = CacheManager::get_statistics(cache_manager).memory_usage
  assert_true(memory_usage_after <= 50 * 1024 * 1024, "内存使用不应超过限制")
  
  // 验证内存压力下条目被淘汰
  let entries_after_memory_pressure = CacheManager::get_statistics(cache_manager).entries
  assert_true(entries_after_memory_pressure < entries_after_overflow, 
    "内存压力下应该淘汰更多条目")
  
  // 测试智能缓存预热
  let cache_warmer = CacheWarmer::new(cache_manager)
  let warmup_keys = ["warmup_key_1", "warmup_key_2", "warmup_key_3"]
  let warmup_values = ["warmup_value_1", "warmup_value_2", "warmup_value_3"]
  
  CacheWarmer::preload_data(cache_warmer, warmup_keys, warmup_values)
  
  // 验证预热数据可用
  for i = 0; i < warmup_keys.length(); i = i + 1 {
    let warmup_retrieval = CacheManager::get(cache_manager, warmup_keys[i])
    assert_true(warmup_retrieval.is_some(), "预热数据应该可用")
    assert_eq(warmup_retrieval.unwrap(), warmup_values[i], "预热数据应该正确")
  }
  
  // 测试缓存性能优化
  let cache_optimizer = CacheOptimizer::new(cache_manager)
  CacheOptimizer::analyze_usage_patterns(cache_optimizer)
  CacheOptimizer::optimize_cache_structure(cache_optimizer)
  
  // 验证优化效果
  let optimization_report = CacheOptimizer::get_optimization_report(cache_optimizer)
  assert_true(optimization_report.optimizations_applied > 0, "应该应用缓存优化")
  assert_true(optimization_report.performance_improvement > 0.0, "应该有性能改进")
  
  // 生成缓存管理报告
  let cache_report = CacheManager::generate_report(cache_manager)
  assert_true(cache_report.total_operations > 0, "应该有操作记录")
  assert_true(cache_report.hit_rate > 0.8, "缓存命中率应该高")
  assert_true(cache_report.memory_efficiency > 0.7, "内存效率应该高")
  assert_true(cache_report.evictions > 0, "应该有淘汰记录")
  
  assert_true(true)
}

test "内存泄漏检测和预防" {
  // 创建内存泄漏检测器
  let leak_detector = MemoryLeakDetector::new()
  MemoryLeakDetector::set_detection_threshold(leak_detector, 10 * 1024 * 1024) // 10MB阈值
  MemoryLeakDetector::set_detection_interval(leak_detector, 5000) // 5秒间隔
  MemoryLeakDetector::set_auto_prevention(leak_detector, true)
  
  // 创建遥测提供者并启用泄漏检测
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_leak_detection_enabled(config, true)
  TelemetryConfiguration::set_leak_detector(config, leak_detector)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "leak.detection.test")
  
  // 记录初始内存状态
  let initial_memory = MemoryLeakDetector::get_memory_usage(leak_detector)
  let initial_objects = MemoryLeakDetector::get_object_count(leak_detector)
  
  // 测试正常使用情况下的内存管理
  let normal_spans = []
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(tracer, "normal.span." + i.to_string())
    Span::set_attribute(span, "test.type", "normal")
    Span::set_attribute(span, "index", i.to_string())
    normal_spans = normal_spans + [span]
    Span::end(span) // 正常结束span
  }
  
  // 验证正常使用后内存恢复
  simulate_delay(2000) // 等待GC
  let memory_after_normal = MemoryLeakDetector::get_memory_usage(leak_detector)
  assert_true(memory_after_normal <= initial_memory * 1.1, "正常使用后内存应该恢复")
  
  // 测试潜在的内存泄漏场景
  let leaked_spans = []
  let leaked_objects = []
  
  // 创建但不结束span（模拟泄漏）
  for i = 0; i < 200; i = i + 1 {
    let span = Tracer::start_span(tracer, "leaked.span." + i.to_string())
    Span::set_attribute(span, "test.type", "leaked")
    Span::set_attribute(span, "index", i.to_string())
    
    // 创建大对象但不释放
    let large_object = "x" * 10000 // 10KB字符串
    leaked_objects = leaked_objects + [large_object]
    
    leaked_spans = leaked_spans + [span]
    // 故意不调用Span::end(span)
  }
  
  // 等待泄漏检测
  simulate_delay(6000) // 等待检测间隔
  
  // 验证泄漏被检测
  let leak_report = MemoryLeakDetector::get_leak_report(leak_detector)
  assert_true(leak_report.memory_leak_detected, "内存泄漏应该被检测")
  assert_true(leak_report.leaked_memory > 0, "应该检测到泄漏的内存")
  assert_true(leak_report.leaked_objects > 0, "应该检测到泄漏的对象")
  
  // 测试自动泄漏预防
  let prevention_enabled = MemoryLeakDetector::is_prevention_enabled(leak_detector)
  assert_true(prevention_enabled, "自动泄漏预防应该被启用")
  
  // 创建更多对象以触发预防机制
  for i = 0; i < 300; i = i + 1 {
    let span = Tracer::start_span(tracer, "prevention.test.span." + i.to_string())
    Span::set_attribute(span, "test.type", "prevention")
    
    // 创建更多大对象
    let large_object = "y" * 15000 // 15KB字符串
    leaked_objects = leaked_objects + [large_object]
    
    // 泄漏检测器应该自动结束span以防止泄漏
    // simulate_delay(100) // 给检测器一些时间
  }
  
  // 等待预防机制生效
  simulate_delay(5000)
  
  // 验证预防机制效果
  let prevention_report = MemoryLeakDetector::get_prevention_report(leak_detector)
  assert_true(prevention_report.preventive_actions_taken > 0, "应该采取预防行动")
  assert_true(prevention_report.memory_leaks_prevented > 0, "应该防止内存泄漏")
  
  // 测试内存泄漏模式识别
  let pattern_analyzer = MemoryLeakPatternAnalyzer::new(leak_detector)
  let patterns = MemoryLeakPatternAnalyzer::analyze_leak_patterns(pattern_analyzer)
  
  assert_true(patterns.length() > 0, "应该识别泄漏模式")
  assert_true(patterns.any(fn(p) { p.type == "span_leak" }), "应该识别span泄漏模式")
  
  // 测试智能泄漏修复
  let leak_repairer = MemoryLeakRepairer::new(leak_detector)
  
  // 手动触发泄漏修复
  let repair_result = MemoryLeakRepairer::repair_leaks(leak_repairer)
  assert_true(repair_result.success, "泄漏修复应该成功")
  assert_true(repair_result.repaired_objects > 0, "应该修复泄漏对象")
  
  // 验证修复后内存使用减少
  let memory_after_repair = MemoryLeakDetector::get_memory_usage(leak_detector)
  let objects_after_repair = MemoryLeakDetector::get_object_count(leak_detector)
  
  assert_true(memory_after_repair < memory_after_normal + (initial_memory * 0.5), 
    "修复后内存使用应该显著减少")
  assert_true(objects_after_repair < initial_objects + 100, 
    "修复后对象数量应该减少")
  
  // 测试循环引用泄漏检测
  let circular_ref_test = CircularReferenceTest::new()
  CircularReferenceTest::create_circular_references(circular_ref_test, 100)
  
  // 检测循环引用
  let circular_refs = MemoryLeakDetector::detect_circular_references(leak_detector)
  assert_true(circular_refs.length() > 0, "应该检测到循环引用")
  
  // 修复循环引用
  let circular_repair_result = MemoryLeakRepairer::repair_circular_references(leak_repairer, circular_refs)
  assert_true(circular_repair_result.success, "循环引用修复应该成功")
  
  // 测试长期运行的内存稳定性
  let stability_tester = MemoryStabilityTester::new(leak_detector)
  MemoryStabilityTester::start_stability_test(stability_tester, 60000) // 60秒测试
  
  // 在稳定性测试期间创建和释放对象
  let test_start_time = Timestamp::now()
  while Timestamp::difference(Timestamp::now(), test_start_time).to_millis() < 60000 {
    // 创建对象
    let temp_spans = []
    for i = 0; i < 10; i = i + 1 {
      let span = Tracer::start_span(tracer, "stability.test.span." + i.to_string())
      Span::set_attribute(span, "stability.test", "true")
      temp_spans = temp_spans + [span]
    }
    
    // 释放对象
    for span in temp_spans {
      Span::end(span)
    }
    
    simulate_delay(100) // 短暂等待
  }
  
  // 获取稳定性测试结果
  let stability_result = MemoryStabilityTester::get_result(stability_tester)
  assert_true(stability_result.memory_stable, "长期运行应该保持内存稳定")
  assert_true(stability_result.memory_growth_rate < 0.1, "内存增长率应该低")
  assert_true(stability_result.gc_efficiency > 0.8, "GC效率应该高")
  
  // 生成内存泄漏检测报告
  let leak_detection_report = MemoryLeakDetector::generate_comprehensive_report(leak_detector)
  assert_true(leak_detection_report.total_leaks_detected > 0, "应该检测到泄漏")
  assert_true(leak_detection_report.total_leaks_repaired > 0, "应该修复泄漏")
  assert_true(leak_detection_report.prevention_effectiveness > 0.8, "预防机制应该有效")
  
  assert_true(true)
}