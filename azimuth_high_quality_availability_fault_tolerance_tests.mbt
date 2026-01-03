// Azimuth Telemetry System - High Quality Availability and Fault Tolerance Tests
// 高可用性和容错测试

test "遥测服务故障转移" {
  // 创建高可用性遥测集群
  let ha_cluster = azimuth::HACluster::new()
  
  // 添加主节点和备份节点
  let primary_node = azimuth::Node::new("primary_node", "192.168.1.10", 8080)
  let backup_node1 = azimuth::Node::new("backup_node1", "192.168.1.11", 8080)
  let backup_node2 = azimuth::Node::new("backup_node2", "192.168.1.12", 8080)
  
  azimuth::HACluster::add_node(ha_cluster, primary_node, azimuth::NodeRole::Primary)
  azimuth::HACluster::add_node(ha_cluster, backup_node1, azimuth::NodeRole::Backup)
  azimuth::HACluster::add_node(ha_cluster, backup_node2, azimuth::NodeRole::Backup)
  
  // 配置故障转移策略
  let failover_policy = azimuth::FailoverPolicy::new()
  azimuth::FailoverPolicy::set_health_check_interval(failover_policy, 5000) // 5秒
  azimuth::FailoverPolicy::set_failure_threshold(failover_policy, 3) // 连续3次失败
  azimuth::FailoverPolicy::set_auto_failover_enabled(failover_policy, true)
  azimuth::FailoverPolicy::set_failover_timeout(failover_policy, 10000) // 10秒
  
  // 应用故障转移策略
  azimuth::HACluster::apply_failover_policy(ha_cluster, failover_policy)
  
  // 验证初始状态
  assert_eq(azimuth::HACluster::get_primary_node(ha_cluster), primary_node)
  assert_true(azimuth::Node::is_healthy(primary_node))
  assert_true(azimuth::Node::is_healthy(backup_node1))
  assert_true(azimuth::Node::is_healthy(backup_node2))
  
  // 创建遥测客户端
  let telemetry_client = azimuth::TelemetryClient::new()
  azimuth::TelemetryClient::set_cluster(telemetry_client, ha_cluster)
  
  // 发送遥测数据到主节点
  let telemetry_data = azimuth::TelemetryData::new(
    "test_data",
    1609459200000L,
    azimuth::Attributes::new()
  )
  
  let send_result = azimuth::TelemetryClient::send(telemetry_client, telemetry_data)
  match send_result {
    azimuth::SendResult::Success => assert_true(true)
    _ => assert_true(false)
  }
  
  // 模拟主节点故障
  azimuth::Node::simulate_failure(primary_node)
  
  // 等待健康检查检测到故障
  azimuth::HACluster::wait_for_health_check(ha_cluster)
  
  // 验证故障转移
  let new_primary = azimuth::HACluster::get_primary_node(ha_cluster)
  assert_ne(new_primary, primary_node)
  assert_true(new_primary == backup_node1 || new_primary == backup_node2)
  assert_true(azimuth::Node::is_healthy(new_primary))
  
  // 验证故障转移后仍可发送数据
  let telemetry_data2 = azimuth::TelemetryData::new(
    "test_data_after_failover",
    1609459201000L,
    azimuth::Attributes::new()
  )
  
  let send_result2 = azimuth::TelemetryClient::send(telemetry_client, telemetry_data2)
  match send_result2 {
    azimuth::SendResult::Success => assert_true(true)
    _ => assert_true(false)
  }
  
  // 恢复原始主节点
  azimuth::Node::recover(primary_node)
  
  // 等待健康检查检测到恢复
  azimuth::HACluster::wait_for_health_check(ha_cluster)
  
  // 验证原始主节点已恢复为备份节点
  let current_primary = azimuth::HACluster::get_primary_node(ha_cluster)
  assert_true(current_primary == backup_node1 || current_primary == backup_node2)
  assert_true(azimuth::Node::is_healthy(primary_node))
  assert_eq(azimuth::HACluster::get_node_role(ha_cluster, primary_node), azimuth::NodeRole::Backup)
}

test "数据持久化和恢复" {
  // 创建持久化管理器
  let persistence_manager = azimuth::PersistenceManager::new()
  
  // 配置持久化策略
  let persistence_policy = azimuth::PersistencePolicy::new()
  azimuth::PersistencePolicy::set_storage_type(persistence_policy, azimuth::StorageType::Distributed)
  azimuth::PersistencePolicy::set_replication_factor(persistence_policy, 3) // 3副本
  azimuth::PersistencePolicy::set_consistency_level(persistence_policy, azimuth::ConsistencyLevel::Quorum)
  azimuth::PersistencePolicy::set_backup_interval(persistence_policy, 60000) // 1分钟
  azimuth::PersistencePolicy::set_retention_period(persistence_policy, 7 * 24 * 60 * 60 * 1000L) // 7天
  
  // 应用持久化策略
  azimuth::PersistenceManager::apply_policy(persistence_manager, persistence_policy)
  
  // 创建持久化存储
  let persistent_storage = azimuth::PersistentStorage::new()
  azimuth::PersistentStorage::set_persistence_manager(persistent_storage, persistence_manager)
  
  // 创建大量遥测数据
  let mut telemetry_data_batch = []
  let base_time = 1609459200000L // 2021-01-01 00:00:00 UTC
  
  for i in 0..1000 {
    let data = azimuth::TelemetryData::new(
      "batch_data_" + i.to_string(),
      base_time + (i * 1000L),
      azimuth::Attributes::new()
    )
    
    let attrs = azimuth::TelemetryData::attributes(data)
    azimuth::Attributes::set(attrs, "batch.id", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("test_service"))
    azimuth::Attributes::set(attrs, "metric.value", azimuth::FloatValue(i * 1.5))
    
    telemetry_data_batch = telemetry_data_batch + [data]
  }
  
  // 批量存储遥测数据
  let store_result = azimuth::PersistentStorage::store_batch(persistent_storage, telemetry_data_batch)
  match store_result {
    azimuth::StorageResult::Success(count) => assert_eq(count, 1000)
    _ => assert_true(false)
  }
  
  // 验证数据已持久化
  let retrieve_result = azimuth::PersistentStorage::retrieve_all(persistent_storage)
  match retrieve_result {
    azimuth::RetrieveResult::Success(data) => assert_eq(data.length(), 1000)
    _ => assert_true(false)
  }
  
  // 模拟存储节点故障
  azimuth::PersistentStorage::simulate_node_failure(persistent_storage, 1) // 故障一个节点
  
  // 验证数据仍然可访问（由于副本）
  let retrieve_after_failure = azimuth::PersistentStorage::retrieve_all(persistent_storage)
  match retrieve_after_failure {
    azimuth::RetrieveResult::Success(data) => assert_eq(data.length(), 1000)
    _ => assert_true(false)
  }
  
  // 模拟多个节点故障（超过副本数）
  azimuth::PersistentStorage::simulate_node_failure(persistent_storage, 2) // 再故障两个节点
  
  // 验证数据可能不可访问（由于一致性级别）
  let retrieve_after_multiple_failures = azimuth::PersistentStorage::retrieve_all(persistent_storage)
  match retrieve_after_multiple_failures {
    azimuth::RetrieveResult::Success(data) => assert_true(data.length() <= 1000) // 可能部分数据可用
    azimuth::RetrieveResult::Unavailable => assert_true(true) // 可能不可用
    _ => assert_true(false)
  }
  
  // 恢复故障节点
  azimuth::PersistentStorage::recover_node(persistent_storage, 1)
  azimuth::PersistentStorage::recover_node(persistent_storage, 2)
  
  // 等待数据同步
  azimuth::PersistentStorage::wait_for_sync(persistent_storage)
  
  // 验证数据完全恢复
  let retrieve_after_recovery = azimuth::PersistentStorage::retrieve_all(persistent_storage)
  match retrieve_after_recovery {
    azimuth::RetrieveResult::Success(data) => assert_eq(data.length(), 1000)
    _ => assert_true(false)
  }
  
  // 测试数据备份和恢复
  let backup_result = azimuth::PersistentStorage::create_backup(persistent_storage)
  match backup_result {
    azimuth::BackupResult::Success(backup_id) => {
      // 删除部分数据
      let delete_result = azimuth::PersistentStorage::delete_by_pattern(persistent_storage, "batch_data_[0-4]")
      match delete_result {
        azimuth::DeleteResult::Success(count) => assert_eq(count, 5)
        _ => assert_true(false)
      }
      
      // 验证数据已删除
      let retrieve_after_delete = azimuth::PersistentStorage::retrieve_all(persistent_storage)
      match retrieve_after_delete {
        azimuth::RetrieveResult::Success(data) => assert_eq(data.length(), 995)
        _ => assert_true(false)
      }
      
      // 从备份恢复
      let restore_result = azimuth::PersistentStorage::restore_from_backup(persistent_storage, backup_id)
      match restore_result {
        azimuth::RestoreResult::Success(count) => assert_eq(count, 5)
        _ => assert_true(false)
      }
      
      // 验证数据已恢复
      let retrieve_after_restore = azimuth::PersistentStorage::retrieve_all(persistent_storage)
      match retrieve_after_restore {
        azimuth::RetrieveResult::Success(data) => assert_eq(data.length(), 1000)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

test "网络分区容错" {
  // 创建网络分区模拟器
  let network_partition_simulator = azimuth::NetworkPartitionSimulator::new()
  
  // 创建分布式遥测系统
  let distributed_system = azimuth::DistributedTelemetrySystem::new()
  
  // 添加节点到分布式系统
  let node1 = azimuth::Node::new("node1", "10.0.0.1", 8080)
  let node2 = azimuth::Node::new("node2", "10.0.0.2", 8080)
  let node3 = azimuth::Node::new("node3", "10.0.0.3", 8080)
  let node4 = azimuth::Node::new("node4", "10.0.0.4", 8080)
  
  azimuth::DistributedTelemetrySystem::add_node(distributed_system, node1)
  azimuth::DistributedTelemetrySystem::add_node(distributed_system, node2)
  azimuth::DistributedTelemetrySystem::add_node(distributed_system, node3)
  azimuth::DistributedTelemetrySystem::add_node(distributed_system, node4)
  
  // 配置分区容错策略
  let partition_tolerance_policy = azimuth::PartitionTolerancePolicy::new()
  azimuth::PartitionTolerancePolicy::set_quorum_size(partition_tolerance_policy, 3) // 需要3个节点
  azimuth::PartitionTolerancePolicy::set_operation_timeout(partition_tolerance_policy, 5000) // 5秒超时
  azimuth::PartitionTolerancePolicy::enable_read_from_local(partition_tolerance_policy, true)
  azimuth::PartitionTolerancePolicy::enable_write_buffering(partition_tolerance_policy, true)
  
  // 应用分区容错策略
  azimuth::DistributedTelemetrySystem::apply_partition_tolerance_policy(distributed_system, partition_tolerance_policy)
  
  // 在正常情况下写入数据
  let telemetry_data = azimuth::TelemetryData::new(
    "partition_test_data",
    1609459200000L,
    azimuth::Attributes::new()
  )
  
  let write_result = azimuth::DistributedTelemetrySystem::write(distributed_system, telemetry_data)
  match write_result {
    azimuth::WriteResult::Success => assert_true(true)
    _ => assert_true(false)
  }
  
  // 读取数据验证
  let read_result = azimuth::DistributedTelemetrySystem::read(distributed_system, "partition_test_data")
  match read_result {
    azimuth::ReadResult::Success(data) => assert_eq(azimuth::TelemetryData::name(data), "partition_test_data")
    _ => assert_true(false)
  }
  
  // 模拟网络分区（node1和node2与node3和node4隔离）
  let partition1 = [node1, node2]
  let partition2 = [node3, node4]
  
  azimuth::NetworkPartitionSimulator::create_partition(network_partition_simulator, partition1, partition2)
  
  // 等待分区检测
  azimuth::DistributedTelemetrySystem::wait_for_partition_detection(distributed_system)
  
  // 验证分区状态
  assert_true(azimuth::DistributedTelemetrySystem::is_partitioned(distributed_system))
  assert_eq(azimuth::DistributedTelemetrySystem::get_partition_size(distributed_system, partition1), 2)
  assert_eq(azimuth::DistributedTelemetrySystem::get_partition_size(distributed_system, partition2), 2)
  
  // 在分区1中尝试写入数据（应该失败，因为不满足法定人数）
  let partition1_data = azimuth::TelemetryData::new(
    "partition1_data",
    1609459201000L,
    azimuth::Attributes::new()
  )
  
  let partition1_write_result = azimuth::DistributedTelemetrySystem::write_to_partition(distributed_system, partition1_data, partition1)
  match partition1_write_result {
    azimuth::WriteResult::QuorumNotReached => assert_true(true)
    _ => assert_true(false)
  }
  
  // 在分区1中尝试读取数据（应该从本地读取）
  let partition1_read_result = azimuth::DistributedTelemetrySystem::read_from_partition(distributed_system, "partition_test_data", partition1)
  match partition1_read_result {
    azimuth::ReadResult::Success(data) => assert_eq(azimuth::TelemetryData::name(data), "partition_test_data")
    azimuth::ReadResult::StaleData => assert_true(true) // 可能读取到陈旧数据
    _ => assert_true(false)
  }
  
  // 修复网络分区
  azimuth::NetworkPartitionSimulator::heal_partition(network_partition_simulator)
  
  // 等待分区愈合
  azimuth::DistributedTelemetrySystem::wait_for_partition_heal(distributed_system)
  
  // 验证分区已愈合
  assert_false(azimuth::DistributedTelemetrySystem::is_partitioned(distributed_system))
  
  // 验证数据一致性
  let heal_read_result = azimuth::DistributedTelemetrySystem::read(distributed_system, "partition_test_data")
  match heal_read_result {
    azimuth::ReadResult::Success(data) => assert_eq(azimuth::TelemetryData::name(data), "partition_test_data")
    _ => assert_true(false)
  }
  
  // 验证缓冲的写入操作已应用
  let buffered_writes = azimuth::DistributedTelemetrySystem::get_buffered_writes(distributed_system)
  assert_true(buffered_writes.length() > 0)
  
  // 应用缓冲的写入操作
  let apply_buffered_result = azimuth::DistributedTelemetrySystem::apply_buffered_writes(distributed_system)
  match apply_buffered_result {
    azimuth::ApplyResult::Success(count) => assert_true(count > 0)
    _ => assert_true(false)
  }
}

test "优雅降级和断路器" {
  // 创建断路器管理器
  let circuit_breaker_manager = azimuth::CircuitBreakerManager::new()
  
  // 创建遥测服务
  let telemetry_service = azimuth::TelemetryService::new()
  azimuth::TelemetryService::set_circuit_breaker_manager(telemetry_service, circuit_breaker_manager)
  
  // 为不同操作创建断路器
  let storage_circuit_breaker = azimuth::CircuitBreaker::new(
    "storage_operations",
    5, // 失败阈值
    10000, // 超时时间（毫秒）
    30000 // 重置时间（毫秒）
  )
  
  let network_circuit_breaker = azimuth::CircuitBreaker::new(
    "network_operations",
    3, // 失败阈值
    5000, // 超时时间（毫秒）
    15000 // 重置时间（毫秒）
  )
  
  let processing_circuit_breaker = azimuth::CircuitBreaker::new(
    "processing_operations",
    10, // 失败阈值
    15000, // 超时时间（毫秒）
    60000 // 重置时间（毫秒）
  )
  
  // 注册断路器
  azimuth::CircuitBreakerManager::register_circuit_breaker(circuit_breaker_manager, storage_circuit_breaker)
  azimuth::CircuitBreakerManager::register_circuit_breaker(circuit_breaker_manager, network_circuit_breaker)
  azimuth::CircuitBreakerManager::register_circuit_breaker(circuit_breaker_manager, processing_circuit_breaker)
  
  // 配置优雅降级策略
  let degradation_policy = azimuth::DegradationPolicy::new()
  azimuth::DegradationPolicy::enable_sampling_reduction(degradation_policy, true)
  azimuth::DegradationPolicy::enable_batch_size_reduction(degradation_policy, true)
  azimuth::DegradationPolicy::enable_feature_disabling(degradation_policy, true)
  azimuth::DegradationPolicy::set_sampling_reduction_factor(degradation_policy, 0.5) // 减少到50%
  azimuth::DegradationPolicy::set_batch_size_reduction_factor(degradation_policy, 0.7) // 减少到70%
  
  // 应用降级策略
  azimuth::TelemetryService::apply_degradation_policy(telemetry_service, degradation_policy)
  
  // 正常操作
  for i in 0..10 {
    let operation_result = azimuth::TelemetryService::process_telemetry_data(telemetry_service, "test_data_" + i.to_string())
    match operation_result {
      azimuth::OperationResult::Success => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 验证所有断路器都处于关闭状态
  assert_eq(azimuth::CircuitBreaker::get_state(storage_circuit_breaker), azimuth::CircuitBreakerState::Closed)
  assert_eq(azimuth::CircuitBreaker::get_state(network_circuit_breaker), azimuth::CircuitBreakerState::Closed)
  assert_eq(azimuth::CircuitBreaker::get_state(processing_circuit_breaker), azimuth::CircuitBreakerState::Closed)
  
  // 模拟存储操作失败
  for i in 0..6 {
    azimuth::TelemetryService::simulate_storage_failure(telemetry_service)
    let operation_result = azimuth::TelemetryService::process_telemetry_data(telemetry_service, "test_data_storage_failure_" + i.to_string())
    match operation_result {
      azimuth::OperationResult::Failure => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 验证存储断路器已打开
  assert_eq(azimuth::CircuitBreaker::get_state(storage_circuit_breaker), azimuth::CircuitBreakerState::Open)
  
  // 验证优雅降级已启用
  assert_true(azimuth::TelemetryService::is_degradation_active(telemetry_service))
  
  // 测试降级后的操作
  let degraded_operation_result = azimuth::TelemetryService::process_telemetry_data(telemetry_service, "test_data_degraded")
  match degraded_operation_result {
    azimuth::OperationResult::SuccessWithDegradation => assert_true(true)
    azimuth::OperationResult::Success => assert_true(true) // 可能仍然成功
    _ => assert_true(false)
  }
  
  // 验证采样率已降低
  let current_sampling_rate = azimuth::TelemetryService::get_sampling_rate(telemetry_service)
  assert_true(current_sampling_rate < 1.0)
  
  // 验证批量处理大小已减少
  let current_batch_size = azimuth::TelemetryService::get_batch_size(telemetry_service)
  assert_true(current_batch_size < 100) // 假设原始批量大小为100
  
  // 模拟网络操作失败
  for i in 0..4 {
    azimuth::TelemetryService::simulate_network_failure(telemetry_service)
    let operation_result = azimuth::TelemetryService::process_telemetry_data(telemetry_service, "test_data_network_failure_" + i.to_string())
    match operation_result {
      azimuth::OperationResult::Failure => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 验证网络断路器已打开
  assert_eq(azimuth::CircuitBreaker::get_state(network_circuit_breaker), azimuth::CircuitBreakerState::Open)
  
  // 等待断路器重置
  azimuth::CircuitBreaker::wait_for_reset_timeout(storage_circuit_breaker)
  
  // 尝试半开状态操作
  let half_open_result = azimuth::TelemetryService::process_telemetry_data(telemetry_service, "test_data_half_open")
  match half_open_result {
    azimuth::OperationResult::Success => {
      // 成功操作应该关闭断路器
      assert_eq(azimuth::CircuitBreaker::get_state(storage_circuit_breaker), azimuth::CircuitBreakerState::Closed)
    }
    azimuth::OperationResult::Failure => {
      // 失败操作应该保持断路器打开
      assert_eq(azimuth::CircuitBreaker::get_state(storage_circuit_breaker), azimuth::CircuitBreakerState::Open)
    }
    _ => assert_true(false)
  }
  
  // 恢复所有服务
  azimuth::TelemetryService::recover_all_services(telemetry_service)
  
  // 验证所有断路器都已关闭
  assert_eq(azimuth::CircuitBreaker::get_state(storage_circuit_breaker), azimuth::CircuitBreakerState::Closed)
  assert_eq(azimuth::CircuitBreaker::get_state(network_circuit_breaker), azimuth::CircuitBreakerState::Closed)
  
  // 验证降级已禁用
  assert_false(azimuth::TelemetryService::is_degradation_active(telemetry_service))
  
  // 验证采样率已恢复
  let recovered_sampling_rate = azimuth::TelemetryService::get_sampling_rate(telemetry_service)
  assert_eq(recovered_sampling_rate, 1.0)
  
  // 验证批量处理大小已恢复
  let recovered_batch_size = azimuth::TelemetryService::get_batch_size(telemetry_service)
  assert_eq(recovered_batch_size, 100) // 假设原始批量大小为100
  
  // 测试正常操作
  let normal_operation_result = azimuth::TelemetryService::process_telemetry_data(telemetry_service, "test_data_normal")
  match normal_operation_result {
    azimuth::OperationResult::Success => assert_true(true)
    _ => assert_true(false)
  }
}

test "负载均衡和流量分发" {
  // 创建负载均衡器
  let load_balancer = azimuth::LoadBalancer::new()
  
  // 配置负载均衡策略
  let lb_policy = azimuth::LoadBalancingPolicy::new()
  azimuth::LoadBalancingPolicy::set_algorithm(lb_policy, azimuth::LoadBalancingAlgorithm::WeightedRoundRobin)
  azimuth::LoadBalancingPolicy::set_health_check_enabled(lb_policy, true)
  azimuth::LoadBalancingPolicy::set_health_check_interval(lb_policy, 10000) // 10秒
  azimuth::LoadBalancingPolicy::set_unhealthy_threshold(lb_policy, 3) // 连续3次失败
  azimuth::LoadBalancingPolicy::set_healthy_threshold(lb_policy, 2) // 连续2次成功
  
  // 应用负载均衡策略
  azimuth::LoadBalancer::apply_policy(load_balancer, lb_policy)
  
  // 创建后端服务节点
  let backend1 = azimuth::BackendNode::new("backend1", "192.168.1.10", 8080)
  let backend2 = azimuth::BackendNode::new("backend2", "192.168.1.11", 8080)
  let backend3 = azimuth::BackendNode::new("backend3", "192.168.1.12", 8080)
  
  // 设置节点权重
  azimuth::BackendNode::set_weight(backend1, 1)
  azimuth::BackendNode::set_weight(backend2, 2) // 更高权重
  azimuth::BackendNode::set_weight(backend3, 3) // 最高权重
  
  // 添加节点到负载均衡器
  azimuth::LoadBalancer::add_backend(load_balancer, backend1)
  azimuth::LoadBalancer::add_backend(load_balancer, backend2)
  azimuth::LoadBalancer::add_backend(load_balancer, backend3)
  
  // 验证所有节点都健康
  assert_true(azimuth::BackendNode::is_healthy(backend1))
  assert_true(azimuth::BackendNode::is_healthy(backend2))
  assert_true(azimuth::BackendNode::is_healthy(backend3))
  
  // 创建流量分发器
  let traffic_distributor = azimuth::TrafficDistributor::new()
  azimuth::TrafficDistributor::set_load_balancer(traffic_distributor, load_balancer)
  
  // 模拟流量分发
  let mut backend_requests = [("backend1", 0), ("backend2", 0), ("backend3", 0)]
  
  for i in 0..100 {
    let request = azimuth::TelemetryRequest::new("request_" + i.to_string())
    let selected_backend = azimuth::TrafficDistributor::distribute_request(traffic_distributor, request)
    
    // 统计请求分发
    if selected_backend == backend1 {
      backend_requests[0] = (backend_requests[0].0, backend_requests[0].1 + 1)
    } else if selected_backend == backend2 {
      backend_requests[1] = (backend_requests[1].0, backend_requests[1].1 + 1)
    } else if selected_backend == backend3 {
      backend_requests[2] = (backend_requests[2].0, backend_requests[2].1 + 1)
    }
  }
  
  // 验证流量按权重分发
  let total_requests = backend_requests[0].1 + backend_requests[1].1 + backend_requests[2].1
  assert_eq(total_requests, 100)
  
  // 权重为1:2:3，所以请求应该大致按1:2:3的比例分发
  let ratio1 = backend_requests[0].1 as Float / total_requests as Float
  let ratio2 = backend_requests[1].1 as Float / total_requests as Float
  let ratio3 = backend_requests[2].1 as Float / total_requests as Float
  
  assert_true(ratio1 > 0.1 && ratio1 < 0.2) // 约1/6
  assert_true(ratio2 > 0.2 && ratio2 < 0.4) // 约2/6
  assert_true(ratio3 > 0.4 && ratio3 < 0.6) // 约3/6
  
  // 模拟backend2故障
  azimuth::BackendNode::simulate_failure(backend2)
  
  // 等待健康检查检测到故障
  azimuth::LoadBalancer::wait_for_health_check(load_balancer)
  
  // 验证backend2被标记为不健康
  assert_false(azimuth::BackendNode::is_healthy(backend2))
  assert_true(azimuth::BackendNode::is_healthy(backend1))
  assert_true(azimuth::BackendNode::is_healthy(backend3))
  
  // 继续流量分发，应该只分发到健康节点
  backend_requests = [("backend1", 0), ("backend2", 0), ("backend3", 0)]
  
  for i in 0..50 {
    let request = azimuth::TelemetryRequest::new("post_failure_request_" + i.to_string())
    let selected_backend = azimuth::TrafficDistributor::distribute_request(traffic_distributor, request)
    
    // 统计请求分发
    if selected_backend == backend1 {
      backend_requests[0] = (backend_requests[0].0, backend_requests[0].1 + 1)
    } else if selected_backend == backend2 {
      backend_requests[1] = (backend_requests[1].0, backend_requests[1].1 + 1)
    } else if selected_backend == backend3 {
      backend_requests[2] = (backend_requests[2].0, backend_requests[2].1 + 1)
    }
  }
  
  // 验证流量只分发到健康节点
  assert_eq(backend_requests[1].1, 0) // backend2应该没有请求
  assert_eq(backend_requests[0].1 + backend_requests[2].1, 50) // 所有请求都分发到backend1和backend3
  
  // 恢复backend2
  azimuth::BackendNode::recover(backend2)
  
  // 等待健康检查检测到恢复
  azimuth::LoadBalancer::wait_for_health_check(load_balancer)
  
  // 验证backend2恢复健康
  assert_true(azimuth::BackendNode::is_healthy(backend2))
  
  // 继续流量分发，应该恢复到所有节点
  backend_requests = [("backend1", 0), ("backend2", 0), ("backend3", 0)]
  
  for i in 0..50 {
    let request = azimuth::TelemetryRequest::new("post_recovery_request_" + i.to_string())
    let selected_backend = azimuth::TrafficDistributor::distribute_request(traffic_distributor, request)
    
    // 统计请求分发
    if selected_backend == backend1 {
      backend_requests[0] = (backend_requests[0].0, backend_requests[0].1 + 1)
    } else if selected_backend == backend2 {
      backend_requests[1] = (backend_requests[1].0, backend_requests[1].1 + 1)
    } else if selected_backend == backend3 {
      backend_requests[2] = (backend_requests[2].0, backend_requests[2].1 + 1)
    }
  }
  
  // 验证流量恢复到所有节点
  assert_eq(backend_requests[0].1 + backend_requests[1].1 + backend_requests[2].1, 50)
  assert_true(backend_requests[1].1 > 0) // backend2应该有请求
  
  // 测试会话亲和性
  azimuth::LoadBalancingPolicy::set_enable_session_affinity(lb_policy, true)
  azimuth::LoadBalancer::apply_policy(load_balancer, lb_policy)
  
  // 创建会话
  let session1 = azimuth::Session::new("session1")
  let session2 = azimuth::Session::new("session2")
  
  // 为会话分发请求
  let session1_backend = azimuth::TrafficDistributor::distribute_request_with_session(traffic_distributor, azimuth::TelemetryRequest::new("session1_request1"), session1)
  let session2_backend = azimuth::TrafficDistributor::distribute_request_with_session(traffic_distributor, azimuth::TelemetryRequest::new("session2_request1"), session2)
  
  // 为同一会话分发更多请求
  for i in 0..10 {
    let session1_request = azimuth::TelemetryRequest::new("session1_request_" + i.to_string())
    let session1_selected_backend = azimuth::TrafficDistributor::distribute_request_with_session(traffic_distributor, session1_request, session1)
    assert_eq(session1_selected_backend, session1_backend) // 应该始终是相同的后端
    
    let session2_request = azimuth::TelemetryRequest::new("session2_request_" + i.to_string())
    let session2_selected_backend = azimuth::TrafficDistributor::distribute_request_with_session(traffic_distributor, session2_request, session2)
    assert_eq(session2_selected_backend, session2_backend) // 应该始终是相同的后端
  }
}