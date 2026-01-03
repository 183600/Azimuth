// 资源管理和内存泄漏检测综合测试
// 测试Azimuth遥测系统的资源管理和内存泄漏检测功能

test "资源生命周期管理" {
  // 测试资源的创建、使用和销毁
  let resource_manager = ResourceManager::new()
  
  // 创建资源
  let resource1 = ResourceManager::create_resource(resource_manager, "database.connection")
  let resource2 = ResourceManager::create_resource(resource_manager, "cache.client")
  let resource3 = ResourceManager::create_resource(resource_manager, "message.queue")
  
  // 设置资源属性
  ResourceManager::set_attribute(resource1, "connection.timeout", 30000)
  ResourceManager::set_attribute(resource1, "connection.pool.size", 10)
  ResourceManager::set_attribute(resource1, "connection.retry.count", 3)
  
  ResourceManager::set_attribute(resource2, "cache.size", "1GB")
  ResourceManager::set_attribute(resource2, "cache.ttl", 3600)
  ResourceManager::set_attribute(resource2, "cache.eviction.policy", "LRU")
  
  ResourceManager::set_attribute(resource3, "queue.name", "events")
  ResourceManager::set_attribute(resource3, "queue.max.size", 10000)
  ResourceManager::set_attribute(resource3, "queue.consumer.count", 5)
  
  // 验证资源状态
  assert_eq(ResourceManager::get_resource_count(resource_manager), 3)
  assert_eq(ResourceManager::get_resource_status(resource1), "active")
  assert_eq(ResourceManager::get_resource_status(resource2), "active")
  assert_eq(ResourceManager::get_resource_status(resource3), "active")
  
  // 释放资源
  ResourceManager::release_resource(resource1)
  ResourceManager::release_resource(resource2)
  ResourceManager::release_resource(resource3)
  
  // 验证资源已释放
  assert_eq(ResourceManager::get_resource_count(resource_manager), 0)
  assert_eq(ResourceManager::get_resource_status(resource1), "released")
  assert_eq(ResourceManager::get_resource_status(resource2), "released")
  assert_eq(ResourceManager::get_resource_status(resource3), "released")
}

test "内存使用监控和限制" {
  // 测试内存使用监控和限制功能
  let memory_monitor = MemoryMonitor::new()
  
  // 设置内存限制
  MemoryMonitor::set_limit(memory_monitor, 100 * 1024 * 1024) // 100MB
  
  // 创建内存密集型操作
  let large_data = Array::with_capacity(10000)
  for i in 0..10000 {
    Array::push(large_data, "large.data.item." + i.to_string())
  }
  
  // 监控内存使用
  let memory_usage = MemoryMonitor::get_current_usage(memory_monitor)
  let memory_limit = MemoryMonitor::get_limit(memory_monitor)
  
  assert_true(memory_usage < memory_limit)
  
  // 测试内存警告阈值
  MemoryMonitor::set_warning_threshold(memory_monitor, 80 * 1024 * 1024) // 80MB
  let warning_threshold = MemoryMonitor::get_warning_threshold(memory_monitor)
  
  assert_eq(warning_threshold, 80 * 1024 * 1024)
  
  // 清理内存
  Array::clear(large_data)
  let memory_after_cleanup = MemoryMonitor::get_current_usage(memory_monitor)
  
  assert_true(memory_after_cleanup <= memory_usage)
}

test "资源泄漏检测" {
  // 测试资源泄漏检测功能
  let leak_detector = LeakDetector::new()
  
  // 开始泄漏检测
  LeakDetector::start_detection(leak_detector)
  
  // 创建一些资源
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "leak.test")
  
  // 创建span但不结束它们（模拟泄漏）
  let leaked_span1 = Tracer::start_span(tracer, "leaked.operation.1")
  let leaked_span2 = Tracer::start_span(tracer, "leaked.operation.2")
  
  // 添加属性
  Span::set_attribute(leaked_span1, "operation.type", "leaked")
  Span::set_attribute(leaked_span2, "operation.type", "leaked")
  
  // 创建正常span并正确结束
  let normal_span = Tracer::start_span(tracer, "normal.operation")
  Span::set_attribute(normal_span, "operation.type", "normal")
  Span::end(normal_span)
  
  // 检测泄漏
  let leak_report = LeakDetector::generate_report(leak_detector)
  
  // 验证泄漏报告
  assert_true(LeakReport::get_leaked_span_count(leak_report) >= 2)
  assert_true(LeakReport::get_active_span_count(leak_report) >= 2)
  
  // 清理泄漏的span
  Span::end(leaked_span1)
  Span::end(leaked_span2)
  
  // 重新检测
  let cleanup_report = LeakDetector::generate_report(leak_detector)
  
  // 验证清理后的报告
  assert_eq(LeakReport::get_leaked_span_count(cleanup_report), 0)
}

test "资源池管理和复用" {
  // 测试资源池管理和复用功能
  let resource_pool = ResourcePool::new("database.connections", 5)
  
  // 从池中获取资源
  let conn1 = ResourcePool::acquire(resource_pool)
  let conn2 = ResourcePool::acquire(resource_pool)
  let conn3 = ResourcePool::acquire(resource_pool)
  
  // 验证资源状态
  assert_true(ResourcePool::is_active(conn1))
  assert_true(ResourcePool::is_active(conn2))
  assert_true(ResourcePool::is_active(conn3))
  assert_eq(ResourcePool::get_available_count(resource_pool), 2)
  
  // 使用资源
  ResourcePool::execute(conn1, "SELECT * FROM users")
  ResourcePool::execute(conn2, "SELECT * FROM orders")
  ResourcePool::execute(conn3, "SELECT * FROM products")
  
  // 释放资源回池
  ResourcePool::release(conn1)
  ResourcePool::release(conn2)
  ResourcePool::release(conn3)
  
  // 验证资源已返回池中
  assert_eq(ResourcePool::get_available_count(resource_pool), 5)
  assert_false(ResourcePool::is_active(conn1))
  assert_false(ResourcePool::is_active(conn2))
  assert_false(ResourcePool::is_active(conn3))
  
  // 测试资源复用
  let reused_conn1 = ResourcePool::acquire(resource_pool)
  let reused_conn2 = ResourcePool::acquire(resource_pool)
  
  assert_true(ResourcePool::is_active(reused_conn1))
  assert_true(ResourcePool::is_active(reused_conn2))
  assert_eq(ResourcePool::get_available_count(resource_pool), 3)
}

test "垃圾回收和内存优化" {
  // 测试垃圾回收和内存优化功能
  let gc_manager = GCManager::new()
  
  // 创建大量临时对象
  let temp_objects = []
  for i in 0..1000 {
    let temp_obj = {
      "id": i,
      "data": "temporary.data." + i.to_string(),
      "timestamp": 1704067200000L + i.to_long()
    }
    Array::push(temp_objects, temp_obj)
  }
  
  // 监控内存使用
  let memory_before_gc = GCManager::get_memory_usage(gc_manager)
  
  // 触发垃圾回收
  GCManager::collect(gc_manager)
  
  // 清理临时对象引用
  Array::clear(temp_objects)
  
  // 再次触发垃圾回收
  GCManager::collect(gc_manager)
  
  // 监控内存使用
  let memory_after_gc = GCManager::get_memory_usage(gc_manager)
  
  // 验证内存已释放
  assert_true(memory_after_gc < memory_before_gc)
  
  // 测试内存优化建议
  let optimization_report = GCManager::generate_optimization_report(gc_manager)
  
  assert_true(OptimizationReport::has_suggestions(optimization_report))
  assert_true(OptimizationReport::get_memory_saved(optimization_report) > 0)
}

test "资源依赖关系管理" {
  // 测试资源依赖关系管理
  let dependency_manager = DependencyManager::new()
  
  // 创建资源
  let db_resource = DependencyManager::create_resource(dependency_manager, "database")
  let cache_resource = DependencyManager::create_resource(dependency_manager, "cache")
  let service_resource = DependencyManager::create_resource(dependency_manager, "service")
  
  // 设置依赖关系
  DependencyManager::add_dependency(dependency_manager, service_resource, db_resource)
  DependencyManager::add_dependency(dependency_manager, service_resource, cache_resource)
  
  // 验证依赖关系
  assert_true(DependencyManager::has_dependency(dependency_manager, service_resource, db_resource))
  assert_true(DependencyManager::has_dependency(dependency_manager, service_resource, cache_resource))
  
  let service_deps = DependencyManager::get_dependencies(dependency_manager, service_resource)
  assert_eq(Array::length(service_deps), 2)
  
  // 测试依赖释放顺序
  DependencyManager::release_resource(dependency_manager, service_resource)
  
  // 验证依赖资源也被释放
  assert_eq(DependencyManager::get_resource_status(dependency_manager, service_resource), "released")
  assert_eq(DependencyManager::get_resource_status(dependency_manager, db_resource), "released")
  assert_eq(DependencyManager::get_resource_status(dependency_manager, cache_resource), "released")
}

test "并发资源管理" {
  // 测试并发资源管理
  let concurrent_manager = ConcurrentResourceManager::new()
  
  // 创建多个并发资源
  let resources = []
  for i in 0..10 {
    let resource = ConcurrentResourceManager::create_resource(concurrent_manager, "concurrent.resource." + i.to_string())
    Array::push(resources, resource)
  }
  
  // 并发操作资源
  for i in 0..10 {
    let resource = Array::get(resources, i)
    ConcurrentResourceManager::set_attribute(concurrent_manager, resource, "thread.id", i.to_string())
    ConcurrentResourceManager::set_attribute(concurrent_manager, resource, "operation.count", i * 10)
  }
  
  // 验证并发操作结果
  assert_eq(ConcurrentResourceManager::get_resource_count(concurrent_manager), 10)
  
  // 并发释放资源
  for i in 0..10 {
    let resource = Array::get(resources, i)
    ConcurrentResourceManager::release_resource(concurrent_manager, resource)
  }
  
  // 验证所有资源已释放
  assert_eq(ConcurrentResourceManager::get_resource_count(concurrent_manager), 0)
}

test "资源监控和指标收集" {
  // 测试资源监控和指标收集
  let resource_monitor = ResourceMonitor::new()
  
  // 创建监控指标
  let resource_counter = ResourceMonitor::create_counter(resource_monitor, "resource.created")
  let resource_gauge = ResourceMonitor::create_gauge(resource_monitor, "resource.active")
  let resource_histogram = ResourceMonitor::create_histogram(resource_monitor, "resource.lifetime")
  
  // 创建资源并记录指标
  for i in 0..5 {
    let resource = ResourceMonitor::track_resource(resource_monitor, "test.resource." + i.to_string())
    
    // 记录创建指标
    ResourceMonitor::increment_counter(resource_counter)
    ResourceMonitor::set_gauge(resource_gauge, ResourceMonitor::get_active_count(resource_monitor))
    
    // 模拟资源生命周期
    let lifetime = 1000 + i * 100
    ResourceMonitor::record_histogram(resource_histogram, lifetime.to_int())
    
    // 释放资源
    ResourceMonitor::release_tracked_resource(resource_monitor, resource)
    ResourceMonitor::set_gauge(resource_gauge, ResourceMonitor::get_active_count(resource_monitor))
  }
  
  // 验证指标
  assert_eq(ResourceMonitor::get_counter_value(resource_counter), 5)
  assert_eq(ResourceMonitor::get_gauge_value(resource_gauge), 0)
  assert_true(ResourceMonitor::get_histogram_count(resource_histogram) > 0)
  
  // 生成监控报告
  let monitor_report = ResourceMonitor::generate_report(resource_monitor)
  
  assert_true(MonitorReport::contains_metric(monitor_report, "resource.created"))
  assert_true(MonitorReport::contains_metric(monitor_report, "resource.active"))
  assert_true(MonitorReport::contains_metric(monitor_report, "resource.lifetime"))
}