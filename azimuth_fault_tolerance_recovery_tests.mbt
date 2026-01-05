// Azimuth 容错和恢复测试
// 专注于系统在故障情况下的容错能力和恢复机制验证

// 测试1: 节点故障检测和恢复
test "节点故障检测和恢复测试" {
  // 节点健康状态
  type NodeHealth = {
    node_id: String,
    status: String,  // "healthy", "suspected", "failed", "recovering"
    last_heartbeat: Int,
    failure_count: Int,
    max_failures: Int
  }
  
  type ClusterManager = {
    nodes: Map[String, NodeHealth],
    current_time: Int,
    heartbeat_timeout: Int
  }
  
  fn create_cluster_manager(node_count: Int) -> ClusterManager {
    let nodes = Map::new()
    for i in 0..<node_count {
      let node_id = "node" + i.to_string()
      let node = {
        node_id: node_id,
        status: "healthy",
        last_heartbeat: 1609459200,
        failure_count: 0,
        max_failures: 3
      }
      nodes.insert(node_id, node)
    }
    
    {
      nodes: nodes,
      current_time: 1609459200,
      heartbeat_timeout: 30  // 30秒超时
    }
  }
  
  fn update_heartbeat(manager: ClusterManager, node_id: String) -> ClusterManager {
    match manager.nodes.get(node_id) {
      Some(node) => {
        let updated_node = {
          node_id: node.node_id,
          status: if node.status == "failed" || node.status == "recovering" { "recovering" } else { "healthy" },
          last_heartbeat: manager.current_time,
          failure_count: if node.status == "failed" || node.status == "recovering" { 0 } else { node.failure_count },
          max_failures: node.max_failures
        }
        
        let updated_nodes = Map::new()
        for (id, n) in manager.nodes {
          if id == node_id {
            updated_nodes.insert(id, updated_node)
          } else {
            updated_nodes.insert(id, n)
          }
        }
        
        {
          nodes: updated_nodes,
          current_time: manager.current_time,
          heartbeat_timeout: manager.heartbeat_timeout
        }
      }
      None => manager
    }
  }
  
  fn check_node_health(manager: ClusterManager) -> ClusterManager {
    let updated_nodes = Map::new()
    for (id, node) in manager.nodes {
      let time_since_heartbeat = manager.current_time - node.last_heartbeat
      let (new_status, new_failure_count) = 
        if time_since_heartbeat > manager.heartbeat_timeout {
          if node.status == "healthy" {
            ("suspected", node.failure_count + 1)
          } else if node.status == "suspected" {
            if node.failure_count + 1 >= node.max_failures {
              ("failed", node.failure_count + 1)
            } else {
              ("suspected", node.failure_count + 1)
            }
          } else {
            (node.status, node.failure_count)
          }
        } else {
          (node.status, node.failure_count)
        }
      
      let updated_node = {
        node_id: node.node_id,
        status: new_status,
        last_heartbeat: node.last_heartbeat,
        failure_count: new_failure_count,
        max_failures: node.max_failures
      }
      
      updated_nodes.insert(id, updated_node)
    }
    
    {
      nodes: updated_nodes,
      current_time: manager.current_time,
      heartbeat_timeout: manager.heartbeat_timeout
    }
  }
  
  fn advance_time(manager: ClusterManager, seconds: Int) -> ClusterManager {
    {
      nodes: manager.nodes,
      current_time: manager.current_time + seconds,
      heartbeat_timeout: manager.heartbeat_timeout
    }
  }
  
  // 创建集群管理器
  let cluster_manager = create_cluster_manager(3)
  
  // 验证初始状态
  let node0 = cluster_manager.nodes.get("node0").unwrap()
  let node1 = cluster_manager.nodes.get("node1").unwrap()
  let node2 = cluster_manager.nodes.get("node2").unwrap()
  
  assert_eq(node0.status, "healthy")
  assert_eq(node1.status, "healthy")
  assert_eq(node2.status, "healthy")
  
  // 模拟节点0停止发送心跳
  let manager_after_10s = advance_time(cluster_manager, 10)
  let manager_after_10s_checked = check_node_health(manager_after_10s)
  
  // 节点0仍然健康（未超时）
  let node0_after_10s = manager_after_10s_checked.nodes.get("node0").unwrap()
  assert_eq(node0_after_10s.status, "healthy")
  
  // 超过心跳超时时间
  let manager_after_40s = advance_time(cluster_manager, 40)
  let manager_after_40s_checked = check_node_health(manager_after_40s);
  
  // 节点0状态变为suspected
  let node0_after_40s = manager_after_40s_checked.nodes.get("node0").unwrap()
  assert_eq(node0_after_40s.status, "suspected");
  assert_eq(node0_after_40s.failure_count, 1);
  
  // 再次超时，节点0失败计数增加
  let manager_after_70s = advance_time(manager_after_40s_checked, 30)
  let manager_after_70s_checked = check_node_health(manager_after_70s);
  
  let node0_after_70s = manager_after_70s_checked.nodes.get("node0").unwrap()
  assert_eq(node0_after_70s.status, "suspected")
  assert_eq(node0_after_70s.failure_count, 2)
  
  // 第三次超时，节点0被标记为失败
  let manager_after_100s = advance_time(manager_after_70s_checked, 30)
  let manager_after_100s_checked = check_node_health(manager_after_100s);
  
  let node0_after_100s = manager_after_100s_checked.nodes.get("node0").unwrap()
  assert_eq(node0_after_100s.status, "failed")
  assert_eq(node0_after_100s.failure_count, 3)
  
  // 节点0恢复，发送心跳
  let manager_with_recovery = update_heartbeat(manager_after_100s_checked, "node0")
  
  let node0_recovered = manager_with_recovery.nodes.get("node0").unwrap()
  assert_eq(node0_recovered.status, "recovering")
  assert_eq(node0_recovered.failure_count, 0)
  
  // 再次检查健康状态，节点0恢复为健康状态
  let manager_after_recovery_check = check_node_health(manager_with_recovery)
  
  let node0_final = manager_after_recovery_check.nodes.get("node0").unwrap()
  assert_eq(node0_final.status, "healthy")
}

// 测试2: 数据备份和恢复
test "数据备份和恢复测试" {
  // 备份系统
  type DataBackup = {
    id: String,
    timestamp: Int,
    data: String,
    checksum: Int
  }
  
  type BackupSystem = {
    primary_data: String,
    backups: Array[DataBackup],
    max_backups: Int
  }
  
  fn create_backup_system(initial_data: String, max_backups: Int) -> BackupSystem {
    {
      primary_data: initial_data,
      backups: [],
      max_backups: max_backups
    }
  }
  
  fn calculate_checksum(data: String) -> Int {
    let sum = 0
    for i in 0..<data.length() {
      sum = sum + data.code_point_at(i)
    }
    sum % 1000000
  }
  
  fn create_backup(system: BackupSystem) -> BackupSystem {
    let backup_id = "backup_" + (1609459200).to_string()
    let checksum = calculate_checksum(system.primary_data)
    
    let new_backup = {
      id: backup_id,
      timestamp: 1609459200,
      data: system.primary_data,
      checksum: checksum
    }
    
    let updated_backups = [] : Array[DataBackup>
    for backup in system.backups {
      updated_backups.push(backup)
    }
    updated_backups.push(new_backup)
    
    // 如果备份数量超过最大值，删除最旧的备份
    let final_backups = if updated_backups.length() > system.max_backups {
      let mut result = [] : Array[DataBackup>
      for i in 1..<updated_backups.length() {
        result.push(updated_backups[i])
      }
      result
    } else {
      updated_backups
    }
    
    {
      primary_data: system.primary_data,
      backups: final_backups,
      max_backups: system.max_backups
    }
  }
  
  fn update_primary_data(system: BackupSystem, new_data: String) -> BackupSystem {
    {
      primary_data: new_data,
      backups: system.backups,
      max_backups: system.max_backups
    }
  }
  
  fn restore_from_backup(system: BackupSystem, backup_id: String) -> BackupSystem {
    for backup in system.backups {
      if backup.id == backup_id {
        // 验证备份完整性
        if backup.checksum == calculate_checksum(backup.data) {
          return {
            primary_data: backup.data,
            backups: system.backups,
            max_backups: system.max_backups
          }
        }
      }
    }
    
    // 未找到有效备份，返回原系统
    system
  }
  
  // 创建备份系统
  let backup_system = create_backup_system("Initial data", 3)
  
  // 创建备份
  let system_with_backup1 = create_backup(backup_system)
  assert_eq(system_with_backup1.backups.length(), 1)
  assert_eq(system_with_backup1.backups[0].data, "Initial data")
  
  // 更新主数据
  let system_with_updated_data = update_primary_data(system_with_backup1, "Updated data v1")
  
  // 创建另一个备份
  let system_with_backup2 = create_backup(system_with_updated_data)
  assert_eq(system_with_backup2.backups.length(), 2)
  assert_eq(system_with_backup2.backups[1].data, "Updated data v1")
  
  // 再次更新主数据
  let system_with_updated_data2 = update_primary_data(system_with_backup2, "Updated data v2")
  
  // 创建第三个备份
  let system_with_backup3 = create_backup(system_with_updated_data2)
  assert_eq(system_with_backup3.backups.length(), 3)
  
  // 创建第四个备份，应该删除最旧的备份
  let system_with_backup4 = create_backup(system_with_backup3)
  assert_eq(system_with_backup4.backups.length(), 3)  // 仍然最多3个备份
  assert_eq(system_with_backup4.backups[0].data, "Updated data v1")  // 最旧的备份被删除
  
  // 模拟数据损坏
  let system_with_corrupted_data = update_primary_data(system_with_backup4, "Corrupted data")
  
  // 从备份恢复
  let restored_system = restore_from_backup(system_with_corrupted_data, system_with_backup4.backups[1].id)
  assert_eq(restored_system.primary_data, "Updated data v2")
  
  // 测试从不存在的备份恢复
  let unchanged_system = restore_from_backup(restored_system, "nonexistent_backup")
  assert_eq(unchanged_system.primary_data, "Updated data v2")
}

// 测试3: 自动故障转移
test "自动故障转移测试" {
  // 故障转移系统
  type ServiceInstance = {
    id: String,
    address: String,
    status: String,  // "active", "standby", "failed"
    priority: Int,
    last_health_check: Int
  }
  
  type FailoverManager = {
    instances: Map[String, ServiceInstance],
    active_instance: Option<String>,
    current_time: Int,
    health_check_interval: Int
  }
  
  fn create_failover_manager(instance_count: Int) -> FailoverManager {
    let instances = Map::new()
    let mut active_instance = None : Option<String>
    
    for i in 0..<instance_count {
      let instance_id = "instance" + i.to_string()
      let address = "192.168.1." + (100 + i).to_string()
      let status = if i == 0 { "active" } else { "standby" }
      let priority = instance_count - i  // 优先级递减
      
      if i == 0 {
        active_instance = Some(instance_id)
      }
      
      let instance = {
        id: instance_id,
        address: address,
        status: status,
        priority: priority,
        last_health_check: 1609459200
      }
      instances.insert(instance_id, instance)
    }
    
    {
      instances: instances,
      active_instance: active_instance,
      current_time: 1609459200,
      health_check_interval: 10  // 10秒健康检查间隔
    }
  }
  
  fn update_instance_health(manager: FailoverManager, instance_id: String, is_healthy: Bool) -> FailoverManager {
    match manager.instances.get(instance_id) {
      Some(instance) => {
        let new_status = if is_healthy { 
          if instance.status == "failed" { "standby" } else { instance.status }
        } else { 
          "failed" 
        }
        
        let updated_instance = {
          id: instance.id,
          address: instance.address,
          status: new_status,
          priority: instance.priority,
          last_health_check: manager.current_time
        }
        
        let updated_instances = Map::new()
        for (id, inst) in manager.instances {
          if id == instance_id {
            updated_instances.insert(id, updated_instance)
          } else {
            updated_instances.insert(id, inst)
          }
        }
        
        {
          instances: updated_instances,
          active_instance: manager.active_instance,
          current_time: manager.current_time,
          health_check_interval: manager.health_check_interval
        }
      }
      None => manager
    }
  }
  
  fn perform_failover(manager: FailoverManager) -> FailoverManager {
    match manager.active_instance {
      Some(active_id) => {
        match manager.instances.get(active_id) {
          Some(active_instance) => {
            if active_instance.status == "failed" {
              // 需要故障转移
              let mut best_candidate = None : Option<ServiceInstance>
              let mut highest_priority = -1
              
              for (id, instance) in manager.instances {
                if instance.status == "standby" && instance.priority > highest_priority {
                  highest_priority = instance.priority
                  best_candidate = Some(instance)
                }
              }
              
              match best_candidate {
                Some(candidate) => {
                  // 更新候选实例为活跃状态
                  let updated_candidate = {
                    id: candidate.id,
                    address: candidate.address,
                    status: "active",
                    priority: candidate.priority,
                    last_health_check: candidate.last_health_check
                  }
                  
                  let updated_instances = Map::new()
                  for (id, instance) in manager.instances {
                    if id == candidate.id {
                      updated_instances.insert(id, updated_candidate)
                    } else {
                      updated_instances.insert(id, instance)
                    }
                  }
                  
                  {
                    instances: updated_instances,
                    active_instance: Some(candidate.id),
                    current_time: manager.current_time,
                    health_check_interval: manager.health_check_interval
                  }
                }
                None => {
                  // 没有可用的备用实例
                  manager
                }
              }
            } else {
              // 活跃实例正常，无需故障转移
              manager
            }
          }
          None => manager
        }
      }
      None => manager
    }
  }
  
  // 创建故障转移管理器
  let failover_manager = create_failover_manager(3)
  
  // 验证初始状态
  assert_eq(failover_manager.active_instance.unwrap_or(""), "instance0")
  
  let instance0 = failover_manager.instances.get("instance0").unwrap()
  let instance1 = failover_manager.instances.get("instance1").unwrap()
  let instance2 = failover_manager.instances.get("instance2").unwrap()
  
  assert_eq(instance0.status, "active")
  assert_eq(instance1.status, "standby")
  assert_eq(instance2.status, "standby")
  
  // 活跃实例失败
  let manager_with_failed_instance = update_instance_health(failover_manager, "instance0", false)
  
  // 执行故障转移
  let manager_after_failover = perform_failover(manager_with_failed_instance)
  
  // 验证故障转移结果
  assert_eq(manager_after_failover.active_instance.unwrap_or(""), "instance1")
  
  let updated_instance0 = manager_after_failover.instances.get("instance0").unwrap()
  let updated_instance1 = manager_after_failover.instances.get("instance1").unwrap()
  let updated_instance2 = manager_after_failover.instances.get("instance2").unwrap()
  
  assert_eq(updated_instance0.status, "failed")
  assert_eq(updated_instance1.status, "active")
  assert_eq(updated_instance2.status, "standby")
  
  // 新的活跃实例也失败
  let manager_with_second_failure = update_instance_health(manager_after_failover, "instance1", false)
  
  // 再次执行故障转移
  let manager_after_second_failover = perform_failover(manager_with_second_failure)
  
  // 验证第二次故障转移结果
  assert_eq(manager_after_second_failover.active_instance.unwrap_or(""), "instance2")
  
  let final_instance0 = manager_after_second_failover.instances.get("instance0").unwrap()
  let final_instance1 = manager_after_second_failover.instances.get("instance1").unwrap()
  let final_instance2 = manager_after_second_failover.instances.get("instance2").unwrap()
  
  assert_eq(final_instance0.status, "failed")
  assert_eq(final_instance1.status, "failed")
  assert_eq(final_instance2.status, "active")
  
  // 所有实例都失败的情况
  let manager_with_all_failed = update_instance_health(manager_after_second_failover, "instance2", false)
  let manager_after_final_failover = perform_failover(manager_with_all_failed);
  
  // 没有可用的备用实例，活跃实例仍为instance2（但状态为failed）
  assert_eq(manager_after_final_failover.active_instance.unwrap_or(""), "instance2")
  
  let final_failed_instance2 = manager_after_final_failover.instances.get("instance2").unwrap()
  assert_eq(final_failed_instance2.status, "failed")
}

// 测试4: 断路器模式
test "断路器模式测试" {
  // 断路器状态
  type CircuitState = {
    state: String,  // "closed", "open", "half_open"
    failure_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int,
    last_failure_time: Int,
    current_time: Int
  }
  
  fn create_circuit_breaker(failure_threshold: Int, success_threshold: Int, timeout: Int) -> CircuitState {
    {
      state: "closed",
      failure_count: 0,
      failure_threshold: failure_threshold,
      success_threshold: success_threshold,
      timeout: timeout,
      last_failure_time: 0,
      current_time: 1609459200
    }
  }
  
  fn call_service(circuit: CircuitState, should_succeed: Bool) -> (CircuitState, Bool) {
    match circuit.state {
      "closed" => {
        if should_succeed {
          // 成功调用，重置失败计数
          let updated_circuit = {
            state: circuit.state,
            failure_count: 0,
            failure_threshold: circuit.failure_threshold,
            success_threshold: circuit.success_threshold,
            timeout: circuit.timeout,
            last_failure_time: circuit.last_failure_time,
            current_time: circuit.current_time
          }
          (updated_circuit, true)
        } else {
          // 失败调用，增加失败计数
          let new_failure_count = circuit.failure_count + 1
          let new_state = if new_failure_count >= circuit.failure_threshold { "open" } else { "closed" }
          
          let updated_circuit = {
            state: new_state,
            failure_count: new_failure_count,
            failure_threshold: circuit.failure_threshold,
            success_threshold: circuit.success_threshold,
            timeout: circuit.timeout,
            last_failure_time: if new_state == "open" { circuit.current_time } else { circuit.last_failure_time },
            current_time: circuit.current_time
          }
          (updated_circuit, false)
        }
      }
      "open" => {
        // 检查是否超时
        if circuit.current_time - circuit.last_failure_time >= circuit.timeout {
          // 超时，转为半开状态
          let updated_circuit = {
            state: "half_open",
            failure_count: circuit.failure_count,
            failure_threshold: circuit.failure_threshold,
            success_threshold: circuit.success_threshold,
            timeout: circuit.timeout,
            last_failure_time: circuit.last_failure_time,
            current_time: circuit.current_time
          }
          (updated_circuit, should_succeed)
        } else {
          // 仍然在超时期间，直接拒绝
          (circuit, false)
        }
      }
      "half_open" => {
        if should_succeed {
          // 成功调用，检查是否达到成功阈值
          // 简化实现：一次成功就关闭断路器
          let updated_circuit = {
            state: "closed",
            failure_count: 0,
            failure_threshold: circuit.failure_threshold,
            success_threshold: circuit.success_threshold,
            timeout: circuit.timeout,
            last_failure_time: circuit.last_failure_time,
            current_time: circuit.current_time
          }
          (updated_circuit, true)
        } else {
          // 失败调用，重新打开断路器
          let updated_circuit = {
            state: "open",
            failure_count: circuit.failure_count + 1,
            failure_threshold: circuit.failure_threshold,
            success_threshold: circuit.success_threshold,
            timeout: circuit.timeout,
            last_failure_time: circuit.current_time,
            current_time: circuit.current_time
          }
          (updated_circuit, false)
        }
      }
      _ => (circuit, false)
    }
  }
  
  fn advance_time(circuit: CircuitState, seconds: Int) -> CircuitState {
    {
      state: circuit.state,
      failure_count: circuit.failure_count,
      failure_threshold: circuit.failure_threshold,
      success_threshold: circuit.success_threshold,
      timeout: circuit.timeout,
      last_failure_time: circuit.last_failure_time,
      current_time: circuit.current_time + seconds
    }
  }
  
  // 创建断路器
  let circuit_breaker = create_circuit_breaker(3, 2, 60)  // 3次失败阈值，2次成功阈值，60秒超时
  
  // 初始状态：关闭
  assert_eq(circuit_breaker.state, "closed")
  
  // 成功调用
  let (circuit1, result1) = call_service(circuit_breaker, true)
  assert_eq(circuit1.state, "closed")
  assert_eq(circuit1.failure_count, 0)
  assert_true(result1)
  
  // 失败调用1
  let (circuit2, result2) = call_service(circuit1, false)
  assert_eq(circuit2.state, "closed")
  assert_eq(circuit2.failure_count, 1)
  assert_false(result2)
  
  // 失败调用2
  let (circuit3, result3) = call_service(circuit2, false)
  assert_eq(circuit3.state, "closed")
  assert_eq(circuit3.failure_count, 2)
  assert_false(result3)
  
  // 失败调用3，达到阈值，断路器打开
  let (circuit4, result4) = call_service(circuit3, false)
  assert_eq(circuit4.state, "open")
  assert_eq(circuit4.failure_count, 3)
  assert_false(result4)
  
  // 断路器打开期间，所有调用被拒绝
  let (circuit5, result5) = call_service(circuit4, true)
  assert_eq(circuit5.state, "open")
  assert_false(result5)
  
  // 超时，断路器转为半开状态
  let circuit6 = advance_time(circuit5, 61)  // 超过60秒
  let (circuit7, result7) = call_service(circuit6, true)
  assert_eq(circuit7.state, "half_open")
  assert_true(result7)
  
  // 半开状态下成功调用，断路器关闭
  let (circuit8, result8) = call_service(circuit7, true)
  assert_eq(circuit8.state, "closed")
  assert_eq(circuit8.failure_count, 0)
  assert_true(result8)
  
  // 测试半开状态下失败调用
  let circuit9 = create_circuit_breaker(3, 2, 60)
  let (circuit10, _) = call_service(circuit9, false)
  let (circuit11, _) = call_service(circuit10, false)
  let (circuit12, _) = call_service(circuit11, false)  // 断路器打开
  
  let circuit13 = advance_time(circuit12, 61)  // 超时，半开
  let (circuit14, result14) = call_service(circuit13, false)  // 半开状态下失败
  assert_eq(circuit14.state, "open")
  assert_false(result14)
}

// 测试5: 重试机制
test "重试机制测试" {
  // 重试策略
  type RetryPolicy = {
    max_attempts: Int,
    base_delay: Int,
    max_delay: Int,
    backoff_multiplier: Double
  }
  
  type RetryResult = {
    success: Bool,
    attempts: Int,
    total_delay: Int
  }
  
  fn create_retry_policy(max_attempts: Int, base_delay: Int, max_delay: Int, backoff_multiplier: Double) -> RetryPolicy {
    {
      max_attempts: max_attempts,
      base_delay: base_delay,
      max_delay: max_delay,
      backoff_multiplier: backoff_multiplier
    }
  }
  
  fn execute_with_retry(policy: RetryPolicy, should_succeed_on_attempt: Int) -> RetryResult {
    let mut attempts = 0
    let mut total_delay = 0
    let mut success = false
    
    while attempts < policy.max_attempts && !success {
      attempts = attempts + 1
      
      // 模拟操作执行
      if attempts == should_succeed_on_attempt {
        success = true
      } else {
        // 计算延迟
        if attempts < policy.max_attempts {
          let delay = min(
            (policy.base_delay as Double) * (policy.backoff_multiplier.pow((attempts - 1) as Double)),
            policy.max_delay as Double
          ) as Int
          total_delay = total_delay + delay
        }
      }
    }
    
    {
      success: success,
      attempts: attempts,
      total_delay: total_delay
    }
  }
  
  // 创建重试策略
  let retry_policy = create_retry_policy(5, 1000, 10000, 2.0)  // 最多5次，基础延迟1秒，最大延迟10秒，指数退避
  
  // 测试第一次尝试就成功
  let result1 = execute_with_retry(retry_policy, 1)
  assert_true(result1.success)
  assert_eq(result1.attempts, 1)
  assert_eq(result1.total_delay, 0)
  
  // 测试第三次尝试成功
  let result2 = execute_with_retry(retry_policy, 3)
  assert_true(result2.success)
  assert_eq(result2.attempts, 3)
  assert_eq(result2.total_delay, 1000 + 2000)  // 第一次和第二次失败后的延迟
  
  // 测试所有尝试都失败
  let result3 = execute_with_retry(retry_policy, 10)  // 第10次尝试成功，但最多只有5次
  assert_false(result3.success)
  assert_eq(result3.attempts, 5)
  assert_eq(result3.total_delay, 1000 + 2000 + 4000 + 8000)  // 前4次失败后的延迟
  
  // 测试最大延迟限制
  let policy_with_max_delay = create_retry_policy(4, 3000, 5000, 3.0)  // 基础延迟3秒，最大延迟5秒，3倍退避
  let result4 = execute_with_retry(policy_with_max_delay, 4)  // 第4次尝试成功
  assert_true(result4.success)
  assert_eq(result4.attempts, 4)
  // 延迟应该是：3000 + 5000(限制) + 5000(限制) = 13000
  assert_eq(result4.total_delay, 13000)
}

// 测试6: 优雅降级
test "优雅降级测试" {
  // 服务降级策略
  type ServiceLevel = {
    name: String,
    is_available: Bool,
    fallback_function: () -> String
  }
  
  type DegradationManager = {
    services: Map[String, ServiceLevel>,
    current_level: String
  }
  
  fn create_degradation_manager() -> DegradationManager {
    let services = Map::new()
    
    // 完整功能服务
    services.insert("full", {
      name: "full",
      is_available: true,
      fallback_function: fn() { "Full functionality unavailable" }
    })
    
    // 基本功能服务
    services.insert("basic", {
      name: "basic",
      is_available: true,
      fallback_function: fn() { "Basic functionality unavailable" }
    })
    
    // 最小功能服务
    services.insert("minimal", {
      name: "minimal",
      is_available: true,
      fallback_function: fn() { "Minimal functionality unavailable" }
    })
    
    {
      services: services,
      current_level: "full"
    }
  }
  
  fn set_service_availability(manager: DegradationManager, service_name: String, is_available: Bool) -> DegradationManager {
    match manager.services.get(service_name) {
      Some(service) => {
        let updated_service = {
          name: service.name,
          is_available: is_available,
          fallback_function: service.fallback_function
        }
        
        let updated_services = Map::new()
        for (name, serv) in manager.services {
          if name == service_name {
            updated_services.insert(name, updated_service)
          } else {
            updated_services.insert(name, serv)
          }
        }
        
        {
          services: updated_services,
          current_level: manager.current_level
        }
      }
      None => manager
    }
  }
  
  fn execute_service(manager: DegradationManager) -> String {
    // 按优先级尝试服务
    let service_levels = ["full", "basic", "minimal"]
    
    for level in service_levels {
      match manager.services.get(level) {
        Some(service) => {
          if service.is_available {
            // 更新当前级别
            let updated_manager = {
              services: manager.services,
              current_level: level
            }
            
            match level {
              "full" => return "Full service response: Detailed telemetry data"
              "basic" => return "Basic service response: Essential telemetry data"
              "minimal" => return "Minimal service response: Critical telemetry data"
              _ => return service.fallback_function()
            }
          }
        }
        None => ()
      }
    }
    
    "All services unavailable"
  }
  
  // 创建降级管理器
  let degradation_manager = create_degradation_manager()
  
  // 所有服务可用，使用完整功能
  let result1 = execute_service(degradation_manager)
  assert_eq(result1, "Full service response: Detailed telemetry data")
  
  // 完整功能不可用，降级到基本功能
  let manager_without_full = set_service_availability(degradation_manager, "full", false)
  let result2 = execute_service(manager_without_full)
  assert_eq(result2, "Basic service response: Essential telemetry data")
  
  // 基本功能也不可用，降级到最小功能
  let manager_without_basic = set_service_availability(manager_without_full, "basic", false)
  let result3 = execute_service(manager_without_basic)
  assert_eq(result3, "Minimal service response: Critical telemetry data")
  
  // 所有服务不可用
  let manager_without_all = set_service_availability(manager_without_basic, "minimal", false)
  let result4 = execute_service(manager_without_all)
  assert_eq(result4, "All services unavailable")
  
  // 恢复基本功能
  let manager_with_basic_restored = set_service_availability(manager_without_all, "basic", true)
  let result5 = execute_service(manager_with_basic_restored)
  assert_eq(result5, "Basic service response: Essential telemetry data")
  
  // 恢复完整功能
  let manager_with_full_restored = set_service_availability(manager_with_basic_restored, "full", true)
  let result6 = execute_service(manager_with_full_restored)
  assert_eq(result6, "Full service response: Detailed telemetry data")
}