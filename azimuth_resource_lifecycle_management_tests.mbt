// Azimuth 资源生命周期管理测试用例
// 测试资源的创建、初始化、使用、清理和销毁全过程

test "资源创建和初始化" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 定义资源配置
  let config = azimuth::ResourceConfig {
    name: "database-connection-pool",
    resource_type: azimuth::ResourceType::ConnectionPool,
    initial_size: 5,
    max_size: 20,
    timeout_ms: 5000,
    health_check_interval_ms: 30000,
    auto_cleanup: true,
    metadata: [
      ("connection_string", "postgresql://localhost:5432/mydb"),
      ("username", "admin"),
      ("pool_name", "main_pool")
    ]
  }
  
  // 创建资源
  let resource_id = resource_manager.create_resource(config)
  assert_true(resource_id.length() > 0)
  
  // 验证资源状态
  let resource = resource_manager.get_resource(resource_id)
  assert_true(resource.is_some())
  
  match resource {
    Some(res) => {
      assert_eq(res.config.name, "database-connection-pool")
      assert_eq(res.state, azimuth::ResourceState::Initialized)
      assert_eq(res.metrics.creation_time > 0, true)
      assert_eq(res.active_connections, 5)
    }
    None => assert_true(false)
  }
}

test "资源获取和释放" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建连接池资源
  let config = azimuth::ResourceConfig {
    name: "http-client-pool",
    resource_type: azimuth::ResourceType::HttpClientPool,
    initial_size: 3,
    max_size: 10,
    timeout_ms: 2000,
    health_check_interval_ms: 15000,
    auto_cleanup: false,
    metadata: [
      ("base_url", "https://api.example.com"),
      ("max_connections", "10")
    ]
  }
  
  let resource_id = resource_manager.create_resource(config)
  
  // 获取资源实例
  let instance1 = resource_manager.acquire_instance(resource_id)
  assert_true(instance1.is_some())
  
  match instance1 {
    Some(instance) => {
      assert_eq(instance.resource_id, resource_id)
      assert_eq(instance.instance_id.length() > 0, true)
      assert_eq(instance.state, azimuth::InstanceState::InUse)
    }
    None => assert_true(false)
  }
  
  // 验证资源状态更新
  let resource = resource_manager.get_resource(resource_id)
  match resource {
    Some(res) => {
      assert_eq(res.active_instances, 1)
      assert_eq(res.available_instances, 2)
    }
    None => assert_true(false)
  }
  
  // 释放资源实例
  let release_result = resource_manager.release_instance(resource_id, instance1.unwrap().instance_id)
  assert_true(release_result)
  
  // 验证资源状态恢复
  let resource_after_release = resource_manager.get_resource(resource_id)
  match resource_after_release {
    Some(res) => {
      assert_eq(res.active_instances, 0)
      assert_eq(res.available_instances, 3)
    }
    None => assert_true(false)
  }
}

test "资源健康检查" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建带健康检查的资源
  let config = azimuth::ResourceConfig {
    name: "cache-cluster",
    resource_type: azimuth::ResourceType::CacheCluster,
    initial_size: 2,
    max_size: 5,
    timeout_ms: 1000,
    health_check_interval_ms: 5000,
    auto_cleanup: true,
    metadata: [
      ("cluster_nodes", "node1:6379,node2:6379"),
      ("replication_factor", "2")
    ]
  }
  
  let resource_id = resource_manager.create_resource(config)
  
  // 模拟健康检查
  let health_status = resource_manager.check_resource_health(resource_id)
  assert_eq(health_status.status, azimuth::HealthStatus::Healthy)
  assert_eq(health_status.healthy_instances, 2)
  assert_eq(health_status.unhealthy_instances, 0)
  
  // 模拟实例不健康
  resource_manager.simulate_instance_failure(resource_id, "instance-1")
  
  // 再次检查健康状态
  let health_status_after_failure = resource_manager.check_resource_health(resource_id)
  assert_eq(health_status_after_failure.status, azimuth::HealthStatus::Degraded)
  assert_eq(health_status_after_failure.healthy_instances, 1)
  assert_eq(health_status_after_failure.unhealthy_instances, 1)
  
  // 验证自动恢复机制
  let recovery_result = resource_manager.attempt_recovery(resource_id)
  assert_true(recovery_result)
  
  // 检查恢复后的状态
  let health_status_after_recovery = resource_manager.check_resource_health(resource_id)
  assert_eq(health_status_after_recovery.status, azimuth::HealthStatus::Healthy)
  assert_eq(health_status_after_recovery.healthy_instances, 2)
}

test "资源扩展和收缩" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建可扩展的资源
  let config = azimuth::ResourceConfig {
    name: "worker-thread-pool",
    resource_type: azimuth::ResourceType::ThreadPool,
    initial_size: 2,
    max_size: 8,
    timeout_ms: 3000,
    health_check_interval_ms: 10000,
    auto_cleanup: false,
    metadata: [
      ("thread_priority", "normal"),
      ("queue_size", "100")
    ]
  }
  
  let resource_id = resource_manager.create_resource(config)
  
  // 获取初始状态
  let initial_resource = resource_manager.get_resource(resource_id)
  match initial_resource {
    Some(res) => {
      assert_eq(res.total_instances, 2)
      assert_eq(res.active_instances, 0)
      assert_eq(res.available_instances, 2)
    }
    None => assert_true(false)
  }
  
  // 扩展资源
  let expand_result = resource_manager.expand_resource(resource_id, 3)
  assert_true(expand_result)
  
  // 验证扩展后的状态
  let expanded_resource = resource_manager.get_resource(resource_id)
  match expanded_resource {
    Some(res) => {
      assert_eq(res.total_instances, 5)
      assert_eq(res.available_instances, 5)
    }
    None => assert_true(false)
  }
  
  // 获取一些实例
  let instance1 = resource_manager.acquire_instance(resource_id)
  let instance2 = resource_manager.acquire_instance(resource_id)
  let instance3 = resource_manager.acquire_instance(resource_id)
  
  // 收缩资源（不能收缩已使用的实例）
  let shrink_result = resource_manager.shrink_resource(resource_id, 2)
  assert_true(shrink_result)
  
  // 验证收缩后的状态
  let shrunk_resource = resource_manager.get_resource(resource_id)
  match shrunk_resource {
    Some(res) => {
      assert_eq(res.total_instances, 3)
      assert_eq(res.active_instances, 3)
      assert_eq(res.available_instances, 0)
    }
    None => assert_true(false)
  }
  
  // 释放实例
  resource_manager.release_instance(resource_id, instance1.unwrap().instance_id)
  resource_manager.release_instance(resource_id, instance2.unwrap().instance_id)
  resource_manager.release_instance(resource_id, instance3.unwrap().instance_id)
  
  // 再次收缩
  let final_shrink_result = resource_manager.shrink_resource(resource_id, 1)
  assert_true(final_shrink_result)
  
  // 验证最终状态
  let final_resource = resource_manager.get_resource(resource_id)
  match final_resource {
    Some(res) => {
      assert_eq(res.total_instances, 2)
      assert_eq(res.active_instances, 0)
      assert_eq(res.available_instances, 2)
    }
    None => assert_true(false)
  }
}

test "资源依赖管理" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建数据库资源
  let db_config = azimuth::ResourceConfig {
    name: "primary-database",
    resource_type: azimuth::ResourceType::Database,
    initial_size: 1,
    max_size: 1,
    timeout_ms: 5000,
    health_check_interval_ms: 30000,
    auto_cleanup: false,
    metadata: [
      ("connection_string", "postgresql://localhost:5432/app"),
      ("database_name", "app_db")
    ]
  }
  
  let db_resource_id = resource_manager.create_resource(db_config)
  
  // 创建缓存资源（依赖数据库）
  let cache_config = azimuth::ResourceConfig {
    name: "application-cache",
    resource_type: azimuth::ResourceType::Cache,
    initial_size: 1,
    max_size: 3,
    timeout_ms: 2000,
    health_check_interval_ms: 15000,
    auto_cleanup: false,
    metadata: [
      ("cache_type", "redis"),
      ("backend_database", db_resource_id)
    ]
  }
  
  let cache_resource_id = resource_manager.create_resource(cache_config)
  
  // 添加依赖关系
  let dependency_added = resource_manager.add_dependency(cache_resource_id, db_resource_id)
  assert_true(dependency_added)
  
  // 验证依赖关系
  let dependencies = resource_manager.get_dependencies(cache_resource_id)
  assert_eq(dependencies.length(), 1)
  assert_eq(dependencies[0], db_resource_id)
  
  // 获取依赖的资源
  let dependents = resource_manager.get_dependents(db_resource_id)
  assert_eq(dependents.length(), 1)
  assert_eq(dependents[0], cache_resource_id)
  
  // 尝试删除被依赖的资源（应该失败）
  let delete_result = resource_manager.delete_resource(db_resource_id)
  assert_false(delete_result)
  
  // 先删除依赖的资源
  let delete_cache_result = resource_manager.delete_resource(cache_resource_id)
  assert_true(delete_cache_result)
  
  // 现在可以删除被依赖的资源
  let delete_db_result = resource_manager.delete_resource(db_resource_id)
  assert_true(delete_db_result)
}

test "资源清理和销毁" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建多个资源
  let resources = []
  
  for i in 0..=4 {
    let config = azimuth::ResourceConfig {
      name: "test-resource-" + i.to_string(),
      resource_type: azimuth::ResourceType::MemoryPool,
      initial_size: 2,
      max_size: 5,
      timeout_ms: 1000,
      health_check_interval_ms: 5000,
      auto_cleanup: true,
      metadata: [
        ("pool_size", "100MB"),
        ("allocation_strategy", "buddy")
      ]
    }
    
    let resource_id = resource_manager.create_resource(config)
    resources.push(resource_id)
  }
  
  // 获取一些资源实例
  let instances = []
  for resource_id in resources {
    let instance = resource_manager.acquire_instance(resource_id)
    if instance.is_some() {
      instances.push(instance.unwrap())
    }
  }
  
  // 验证资源在使用中
  for resource_id in resources {
    let resource = resource_manager.get_resource(resource_id)
    match resource {
      Some(res) => {
        assert_eq(res.state, azimuth::ResourceState::Active)
      }
      None => assert_true(false)
    }
  }
  
  // 释放所有实例
  for instance in instances {
    resource_manager.release_instance(instance.resource_id, instance.instance_id)
  }
  
  // 清理所有资源
  let cleanup_result = resource_manager.cleanup_all_resources()
  assert_true(cleanup_result)
  
  // 验证资源已清理
  for resource_id in resources {
    let resource = resource_manager.get_resource(resource_id)
    match resource {
      Some(res) => {
        assert_eq(res.state, azimuth::ResourceState::Terminated)
      }
      None => assert_true(false)
    }
  }
  
  // 强制删除所有资源
  let delete_all_result = resource_manager.delete_all_resources()
  assert_true(delete_all_result)
  
  // 验证资源已删除
  for resource_id in resources {
    let resource = resource_manager.get_resource(resource_id)
    assert_true(resource.is_none())
  }
}

test "资源监控和指标收集" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建资源
  let config = azimuth::ResourceConfig {
    name: "monitored-resource",
    resource_type: azimuth::ResourceType::Queue,
    initial_size: 1,
    max_size: 3,
    timeout_ms: 2000,
    health_check_interval_ms: 10000,
    auto_cleanup: false,
    metadata: [
      ("queue_type", "priority"),
      ("max_size", "1000")
    ]
  }
  
  let resource_id = resource_manager.create_resource(config)
  
  // 启用监控
  let monitoring_enabled = resource_manager.enable_monitoring(resource_id)
  assert_true(monitoring_enabled)
  
  // 执行一些操作以生成指标
  let instance1 = resource_manager.acquire_instance(resource_id)
  let instance2 = resource_manager.acquire_instance(resource_id)
  
  // 模拟一些工作负载
  resource_manager.simulate_workload(resource_id, 100)
  
  resource_manager.release_instance(resource_id, instance1.unwrap().instance_id)
  resource_manager.release_instance(resource_id, instance2.unwrap().instance_id)
  
  // 获取资源指标
  let metrics = resource_manager.get_resource_metrics(resource_id)
  
  // 验证指标收集
  assert_true(metrics.contains_key("total_acquisitions"))
  assert_true(metrics.contains_key("total_releases"))
  assert_true(metrics.contains_key("peak_active_instances"))
  assert_true(metrics.contains_key("average_wait_time_ms"))
  assert_true(metrics.contains_key("error_rate"))
  
  match metrics["total_acquisitions"] {
    Some(azimuth::MetricValue::IntValue(value)) => assert_eq(value, 2)
    _ => assert_true(false)
  }
  
  match metrics["total_releases"] {
    Some(azimuth::MetricValue::IntValue(value)) => assert_eq(value, 2)
    _ => assert_true(false)
  }
  
  match metrics["peak_active_instances"] {
    Some(azimuth::MetricValue::IntValue(value)) => assert_eq(value, 2)
    _ => assert_true(false)
  }
  
  // 获取全局指标
  let global_metrics = resource_manager.get_global_metrics()
  
  // 验证全局指标
  assert_true(global_metrics.contains_key("total_resources"))
  assert_true(global_metrics.contains_key("active_resources"))
  assert_true(global_metrics.contains_key("total_instances"))
  assert_true(global_metrics.contains_key("active_instances"))
  
  match global_metrics["total_resources"] {
    Some(azimuth::MetricValue::IntValue(value)) => assert_eq(value, 1)
    _ => assert_true(false)
  }
}

test "资源生命周期事件" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建事件监听器
  let event_listener = azimuth::TestEventListener::new()
  resource_manager.add_event_listener(event_listener)
  
  // 创建资源
  let config = azimuth::ResourceConfig {
    name: "event-test-resource",
    resource_type: azimuth::ResourceType::FileHandle,
    initial_size: 1,
    max_size: 2,
    timeout_ms: 1000,
    health_check_interval_ms: 5000,
    auto_cleanup: true,
    metadata: [
      ("file_path", "/tmp/test.log"),
      ("access_mode", "read-write")
    ]
  }
  
  let resource_id = resource_manager.create_resource(config)
  
  // 验证创建事件
  let create_events = event_listener.get_events_of_type(azimuth::ResourceEventType::Created)
  assert_eq(create_events.length(), 1)
  assert_eq(create_events[0].resource_id, resource_id)
  
  // 获取资源实例
  let instance = resource_manager.acquire_instance(resource_id)
  
  // 验证获取事件
  let acquire_events = event_listener.get_events_of_type(azimuth::ResourceEventType::InstanceAcquired)
  assert_eq(acquire_events.length(), 1)
  assert_eq(acquire_events[0].resource_id, resource_id)
  
  // 释放资源实例
  resource_manager.release_instance(resource_id, instance.unwrap().instance_id)
  
  // 验证释放事件
  let release_events = event_listener.get_events_of_type(azimuth::ResourceEventType::InstanceReleased)
  assert_eq(release_events.length(), 1)
  assert_eq(release_events[0].resource_id, resource_id)
  
  // 删除资源
  let delete_result = resource_manager.delete_resource(resource_id)
  assert_true(delete_result)
  
  // 验证删除事件
  let delete_events = event_listener.get_events_of_type(azimuth::ResourceEventType::Deleted)
  assert_eq(delete_events.length(), 1)
  assert_eq(delete_events[0].resource_id, resource_id)
  
  // 验证事件顺序
  let all_events = event_listener.get_all_events()
  assert_eq(all_events.length(), 4)
  assert_eq(all_events[0].event_type, azimuth::ResourceEventType::Created)
  assert_eq(all_events[1].event_type, azimuth::ResourceEventType::InstanceAcquired)
  assert_eq(all_events[2].event_type, azimuth::ResourceEventType::InstanceReleased)
  assert_eq(all_events[3].event_type, azimuth::ResourceEventType::Deleted)
}

test "资源生命周期异常处理" {
  // 创建资源管理器
  let resource_manager = azimuth::ResourceManager::new()
  
  // 创建会失败的资源配置
  let invalid_config = azimuth::ResourceConfig {
    name: "invalid-resource",
    resource_type: azimuth::ResourceType::Database,
    initial_size: 0, // 无效的初始大小
    max_size: -1,    // 无效的最大大小
    timeout_ms: -1,  // 无效的超时时间
    health_check_interval_ms: 0,
    auto_cleanup: true,
    metadata: [] // 空元数据
  }
  
  // 尝试创建资源（应该失败）
  let resource_id = resource_manager.create_resource(invalid_config)
  assert_eq(resource_id, "") // 空字符串表示创建失败
  
  // 创建正常资源
  let valid_config = azimuth::ResourceConfig {
    name: "valid-resource",
    resource_type: azimuth::ResourceType::MemoryPool,
    initial_size: 1,
    max_size: 3,
    timeout_ms: 1000,
    health_check_interval_ms: 5000,
    auto_cleanup: true,
    metadata: [
      ("pool_size", "50MB")
    ]
  }
  
  let valid_resource_id = resource_manager.create_resource(valid_config)
  assert_true(valid_resource_id.length() > 0)
  
  // 尝试获取不存在的资源
  let non_existent_resource = resource_manager.get_resource("non-existent-id")
  assert_true(non_existent_resource.is_none())
  
  // 尝试从不存在的资源获取实例
  let invalid_acquire = resource_manager.acquire_instance("non-existent-id")
  assert_true(invalid_acquire.is_none())
  
  // 尝试释放不存在的实例
  let invalid_release = resource_manager.release_instance(valid_resource_id, "non-existent-instance")
  assert_false(invalid_release)
  
  // 模拟资源故障
  resource_manager.simulate_resource_failure(valid_resource_id)
  
  // 验证资源状态变为故障
  let failed_resource = resource_manager.get_resource(valid_resource_id)
  match failed_resource {
    Some(res) => {
      assert_eq(res.state, azimuth::ResourceState::Failed)
    }
    None => assert_true(false)
  }
  
  // 尝试从故障资源获取实例（应该失败）
  let failed_acquire = resource_manager.acquire_instance(valid_resource_id)
  assert_true(failed_acquire.is_none())
  
  // 尝试恢复资源
  let recovery_result = resource_manager.attempt_recovery(valid_resource_id)
  assert_true(recovery_result)
  
  // 验证资源恢复
  let recovered_resource = resource_manager.get_resource(valid_resource_id)
  match recovered_resource {
    Some(res) => {
      assert_eq(res.state, azimuth::ResourceState::Active)
    }
    None => assert_true(false)
  }
}