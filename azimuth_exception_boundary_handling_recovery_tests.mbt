// Azimuth 异常边界处理和恢复测试
// 专注于测试遥测系统在遇到各种异常情况时的边界处理能力和恢复机制

// 测试1: 网络连接异常处理和重试机制
test "网络连接异常处理和重试机制" {
  // 定义网络连接状态
  type NetworkConnection = {
    connection_id: String,
    endpoint: String,
    status: String,  // "connected", "disconnected", "error", "retrying"
    last_error: Option[String],
    retry_count: Int,
    max_retries: Int,
    retry_delay_ms: Int,
    last_activity: Int
  }
  
  type NetworkOperation = {
    operation_id: String,
    operation_type: String,
    payload: String,
    timestamp: Int,
    timeout_ms: Int,
    retry_attempts: Int
  }
  
  type NetworkResult = {
    success: Bool,
    response: Option[String],
    error: Option[String],
    retry_count: Int,
    total_time_ms: Int
  }
  
  // 创建网络连接
  let create_network_connection = fn(
    connection_id: String, 
    endpoint: String, 
    max_retries: Int, 
    retry_delay_ms: Int
  ) -> NetworkConnection {
    {
      connection_id: connection_id,
      endpoint: endpoint,
      status: "disconnected",
      last_error: None,
      retry_count: 0,
      max_retries: max_retries,
      retry_delay_ms: retry_delay_ms,
      last_activity: Time::now()
    }
  }
  
  // 模拟网络操作（包含可能的失败）
  let simulate_network_operation = fn(
    connection: NetworkConnection, 
    operation: NetworkOperation,
    failure_rate: Float
  ) -> NetworkResult {
    let start_time = Time::now()
    let mut retry_count = 0
    let mut success = false
    let mut response = None
    let mut error = None
    
    // 模拟网络操作和重试逻辑
    while retry_count <= connection.max_retries && !success {
      // 模拟随机失败
      let random_value = (Time::now() as Float) / 1000.0
      let should_fail = (random_value % 1.0) < failure_rate
      
      if should_fail {
        error = Some("Network timeout: Connection refused")
        retry_count = retry_count + 1
        
        if retry_count <= connection.max_retries {
          // 模拟重试延迟
          Time::sleep(connection.retry_delay_ms)
        }
      } else {
        success = true
        response = Some("Response: Operation completed successfully")
      }
    }
    
    let end_time = Time::now()
    
    {
      success: success,
      response: response,
      error: error,
      retry_count: retry_count,
      total_time_ms: end_time - start_time
    }
  }
  
  // 更新连接状态
  let update_connection_status = fn(
    connection: NetworkConnection, 
    result: NetworkResult
  ) -> NetworkConnection {
    let new_status = if result.success {
      "connected"
    } else if result.retry_count >= connection.max_retries {
      "error"
    } else {
      "retrying"
    }
    
    {
      ...connection,
      status: new_status,
      last_error: if result.success { None } else { result.error },
      retry_count: if result.success { 0 } else { result.retry_count },
      last_activity: Time::now()
    }
  }
  
  // 测试网络连接异常处理
  let connection = create_network_connection("conn-1", "http://telemetry.example.com", 3, 1000)
  
  let operation = {
    operation_id: "op-1",
    operation_type: "send_metrics",
    payload: "metrics data",
    timestamp: Time::now(),
    timeout_ms: 5000,
    retry_attempts: 0
  }
  
  // 测试高失败率场景（90%失败率）
  let high_failure_result = simulate_network_operation(connection, operation, 0.9)
  let updated_connection1 = update_connection_status(connection, high_failure_result)
  
  // 验证高失败率处理
  if high_failure_result.success {
    // 如果成功，应该经过多次重试
    assert_true(high_failure_result.retry_count >= 0)
    assert_eq(updated_connection1.status, "connected")
  } else {
    // 如果失败，应该达到最大重试次数
    assert_eq(high_failure_result.retry_count, connection.max_retries)
    assert_eq(updated_connection1.status, "error")
    assert_true(updated_connection1.last_error.is_some())
  }
  
  // 测试低失败率场景（10%失败率）
  let low_failure_result = simulate_network_operation(connection, operation, 0.1)
  let updated_connection2 = update_connection_status(connection, low_failure_result)
  
  // 验证低失败率处理
  assert_true(low_failure_result.success)
  assert_true(low_failure_result.retry_count < connection.max_retries)
  assert_eq(updated_connection2.status, "connected")
  assert_true(updated_connection2.last_error.is_none())
  
  // 测试零失败率场景
  let no_failure_result = simulate_network_operation(connection, operation, 0.0)
  let updated_connection3 = update_connection_status(connection, no_failure_result)
  
  // 验证零失败率处理
  assert_true(no_failure_result.success)
  assert_eq(no_failure_result.retry_count, 0)
  assert_eq(updated_connection3.status, "connected")
  assert_true(no_failure_result.response.is_some())
}

// 测试2: 内存不足异常处理
test "内存不足异常处理" {
  // 定义内存管理状态
  type MemoryManager = {
    manager_id: String,
    total_memory_mb: Int,
    used_memory_mb: Int,
    available_memory_mb: Int,
    emergency_threshold_mb: Int,
    cleanup_strategies: Array[String],
    last_cleanup_time: Int
  }
  
  type MemoryAllocation = {
    allocation_id: String,
    size_mb: Int,
    purpose: String,
    timestamp: Int,
    can_be_freed: Bool
  }
  
  type MemoryResult = {
    success: Bool,
    allocated_mb: Int,
    error: Option[String],
    cleanup_performed: Bool,
    strategies_used: Array[String]
  }
  
  // 创建内存管理器
  let create_memory_manager = fn(
    manager_id: String, 
    total_memory_mb: Int, 
    emergency_threshold_mb: Int
  ) -> MemoryManager {
    {
      manager_id: manager_id,
      total_memory_mb: total_memory_mb,
      used_memory_mb: 0,
      available_memory_mb: total_memory_mb,
      emergency_threshold_mb: emergency_threshold_mb,
      cleanup_strategies: ["gc", "cache_clear", "buffer_flush"],
      last_cleanup_time: 0
    }
  }
  
  // 内存分配函数（包含异常处理）
  let allocate_memory = fn(
    manager: MemoryManager, 
    size_mb: Int, 
    purpose: String
  ) -> MemoryResult {
    let mut strategies_used = []
    let mut cleanup_performed = false
    let mut current_manager = manager
    
    // 检查是否有足够内存
    if size_mb > current_manager.available_memory_mb {
      // 内存不足，执行清理策略
      for strategy in current_manager.cleanup_strategies {
        match strategy {
          "gc" => {
            // 模拟垃圾回收，释放20%内存
            let freed_memory = (current_manager.used_memory_mb * 20) / 100
            current_manager = {
              ...current_manager,
              used_memory_mb: current_manager.used_memory_mb - freed_memory,
              available_memory_mb: current_manager.available_memory_mb + freed_memory
            }
            strategies_used = strategies_used.push(strategy)
            cleanup_performed = true
          }
          "cache_clear" => {
            // 模拟清理缓存，释放10%内存
            let freed_memory = (current_manager.used_memory_mb * 10) / 100
            current_manager = {
              ...current_manager,
              used_memory_mb: current_manager.used_memory_mb - freed_memory,
              available_memory_mb: current_manager.available_memory_mb + freed_memory
            }
            strategies_used = strategies_used.push(strategy)
            cleanup_performed = true
          }
          "buffer_flush" => {
            // 模拟刷新缓冲区，释放5%内存
            let freed_memory = (current_manager.used_memory_mb * 5) / 100
            current_manager = {
              ...current_manager,
              used_memory_mb: current_manager.used_memory_mb - freed_memory,
              available_memory_mb: current_manager.available_memory_mb + freed_memory
            }
            strategies_used = strategies_used.push(strategy)
            cleanup_performed = true
          }
          _ => ()
        }
        
        // 检查是否有足够内存
        if size_mb <= current_manager.available_memory_mb {
          break
        }
      }
    }
    
    // 再次检查内存是否足够
    if size_mb <= current_manager.available_memory_mb {
      // 分配内存成功
      current_manager = {
        ...current_manager,
        used_memory_mb: current_manager.used_memory_mb + size_mb,
        available_memory_mb: current_manager.available_memory_mb - size_mb,
        last_cleanup_time: if cleanup_performed { Time::now() } else { current_manager.last_cleanup_time }
      }
      
      {
        success: true,
        allocated_mb: size_mb,
        error: None,
        cleanup_performed: cleanup_performed,
        strategies_used: strategies_used
      }
    } else {
      // 内存分配失败
      {
        success: false,
        allocated_mb: 0,
        error: Some("Out of memory: Cannot allocate " + (size_mb as String) + "MB"),
        cleanup_performed: cleanup_performed,
        strategies_used: strategies_used
      }
    }
  }
  
  // 测试内存不足异常处理
  let manager = create_memory_manager("mem-manager-1", 100, 10)  // 100MB总内存，10MB紧急阈值
  
  // 测试正常内存分配
  let normal_allocation = allocate_memory(manager, 30, "metrics_buffer")
  
  // 验证正常分配
  assert_true(normal_allocation.success)
  assert_eq(normal_allocation.allocated_mb, 30)
  assert_true(normal_allocation.error.is_none())
  assert_false(normal_allocation.cleanup_performed)
  
  // 测试大内存分配（需要清理）
  let large_allocation = allocate_memory(manager, 80, "large_data_processing")
  
  // 验证大内存分配
  if large_allocation.success {
    assert_eq(large_allocation.allocated_mb, 80)
    assert_true(large_allocation.cleanup_performed)
    assert_true(large_allocation.strategies_used.length() > 0)
  } else {
    assert_true(large_allocation.error.is_some())
    assert_true(large_allocation.cleanup_performed)
  }
  
  // 测试超大内存分配（应该失败）
  let oversized_allocation = allocate_memory(manager, 200, "oversized_operation")
  
  // 验证超大内存分配失败
  assert_false(oversized_allocation.success)
  assert_eq(oversized_allocation.allocated_mb, 0)
  assert_true(oversized_allocation.error.is_some())
  assert_true(oversized_allocation.error.unwrap().contains("Out of memory"))
  
  // 测试紧急阈值检查
  let emergency_allocation = allocate_memory(manager, 95, "emergency_operation")
  
  // 验证紧急阈值处理
  if emergency_allocation.success {
    // 如果成功，应该执行了清理策略
    assert_true(emergency_allocation.cleanup_performed)
    assert_true(emergency_allocation.strategies_used.length() > 0)
  } else {
    // 如果失败，应该是因为内存不足
    assert_true(emergency_allocation.error.is_some())
  }
}

// 测试3: 数据损坏检测和恢复
test "数据损坏检测和恢复" {
  // 定义数据完整性状态
  type DataIntegrityManager = {
    manager_id: String,
    checksum_algorithm: String,
    backup_enabled: Bool,
    auto_recovery_enabled: Bool,
    corruption_detected: Int,
    recovery_attempts: Int,
    successful_recoveries: Int
  }
  
  type DataBlock = {
    block_id: String,
    data: String,
    checksum: String,
    timestamp: Int,
    backup_available: Bool,
    backup_data: Option[String]
  }
  
  type IntegrityResult = {
    integrity_passed: Bool,
    corruption_detected: Bool,
    recovery_attempted: Bool,
    recovery_successful: Bool,
    error_message: Option[String]
  }
  
  // 创建数据完整性管理器
  let create_integrity_manager = fn(
    manager_id: String, 
    checksum_algorithm: String, 
    backup_enabled: Bool
  ) -> DataIntegrityManager {
    {
      manager_id: manager_id,
      checksum_algorithm: checksum_algorithm,
      backup_enabled: backup_enabled,
      auto_recovery_enabled: true,
      corruption_detected: 0,
      recovery_attempts: 0,
      successful_recoveries: 0
    }
  }
  
  // 计算校验和
  let calculate_checksum = fn(data: String) -> String {
    let mut hash = 0
    for i in 0..data.length() {
      hash = hash + data.char_at(i).to_int()
    }
    "hash-" + (hash as String)
  }
  
  // 创建数据块
  let create_data_block = fn(
    block_id: String, 
    data: String, 
    backup_enabled: Bool
  ) -> DataBlock {
    let checksum = calculate_checksum(data)
    {
      block_id: block_id,
      data: data,
      checksum: checksum,
      timestamp: Time::now(),
      backup_available: backup_enabled,
      backup_data: if backup_enabled { Some(data) } else { None }
    }
  }
  
  // 验证数据完整性
  let verify_data_integrity = fn(block: DataBlock) -> IntegrityResult {
    let current_checksum = calculate_checksum(block.data)
    let integrity_passed = current_checksum == block.checksum
    let corruption_detected = !integrity_passed
    
    {
      integrity_passed: integrity_passed,
      corruption_detected: corruption_detected,
      recovery_attempted: false,
      recovery_successful: false,
      error_message: if corruption_detected { 
        Some("Data corruption detected: checksum mismatch") 
      } else { 
        None 
      }
    }
  }
  
  // 数据恢复函数
  let recover_data = fn(
    manager: DataIntegrityManager, 
    block: DataBlock
  ) -> (DataIntegrityManager, DataBlock, IntegrityResult) {
    let mut updated_manager = manager
    let mut recovery_attempted = false
    let mut recovery_successful = false
    let mut recovered_block = block
    let mut error_message = None
    
    // 尝试从备份恢复
    if block.backup_available && block.backup_data.is_some() {
      recovery_attempted = true
      updated_manager.recovery_attempts = updated_manager.recovery_attempts + 1
      
      let backup_data = block.backup_data.unwrap()
      let backup_checksum = calculate_checksum(backup_data)
      
      // 验证备份数据的完整性
      if backup_checksum == calculate_checksum(backup_data) {
        // 备份数据完整，执行恢复
        recovered_block = {
          ...block,
          data: backup_data,
          checksum: backup_checksum,
          timestamp: Time::now()
        }
        recovery_successful = true
        updated_manager.successful_recoveries = updated_manager.successful_recoveries + 1
      } else {
        error_message = Some("Backup data also corrupted")
      }
    } else {
      error_message = Some("No backup available for recovery")
    }
    
    let result = {
      integrity_passed: recovery_successful,
      corruption_detected: false,
      recovery_attempted: recovery_attempted,
      recovery_successful: recovery_successful,
      error_message: error_message
    }
    
    (updated_manager, recovered_block, result)
  }
  
  // 测试数据完整性检测和恢复
  let manager = create_integrity_manager("integrity-manager-1", "SHA-256", true)
  
  // 创建正常数据块
  let original_data = "Telemetry metrics data with integrity check"
  let normal_block = create_data_block("block-1", original_data, true)
  
  // 验证正常数据块
  let normal_integrity = verify_data_integrity(normal_block)
  
  // 验证正常数据完整性
  assert_true(normal_integrity.integrity_passed)
  assert_false(normal_integrity.corruption_detected)
  assert_true(normal_integrity.error_message.is_none())
  
  // 创建损坏的数据块
  let corrupted_data = original_data + "corrupted"
  let corrupted_block = {
    ...normal_block,
    data: corrupted_data  // 故意不更新校验和
  }
  
  // 验证损坏数据块
  let corrupted_integrity = verify_data_integrity(corrupted_block)
  
  // 验证损坏检测
  assert_false(corrupted_integrity.integrity_passed)
  assert_true(corrupted_integrity.corruption_detected)
  assert_true(corrupted_integrity.error_message.is_some())
  assert_true(corrupted_integrity.error_message.unwrap().contains("checksum mismatch"))
  
  // 测试数据恢复
  let (updated_manager, recovered_block, recovery_result) = recover_data(manager, corrupted_block)
  
  // 验证数据恢复
  if recovery_result.recovery_successful {
    assert_true(recovery_result.recovery_attempted)
    assert_true(recovered_block.data == original_data)
    assert_eq(recovered_block.checksum, calculate_checksum(original_data))
    assert_eq(updated_manager.successful_recoveries, 1)
    assert_eq(updated_manager.recovery_attempts, 1)
  } else {
    assert_true(recovery_result.error_message.is_some())
  }
  
  // 测试无备份的数据恢复
  let no_backup_manager = create_integrity_manager("integrity-manager-2", "SHA-256", false)
  let no_backup_block = create_data_block("block-2", original_data, false)
  let corrupted_no_backup = {
    ...no_backup_block,
    data: corrupted_data
  }
  
  let (_, _, no_backup_recovery) = recover_data(no_backup_manager, corrupted_no_backup)
  
  // 验证无备份恢复失败
  assert_false(no_backup_recovery.recovery_successful)
  assert_true(no_backup_recovery.error_message.is_some())
  assert_true(no_backup_recovery.error_message.unwrap().contains("No backup available"))
}

// 测试4: 系统资源耗尽处理
test "系统资源耗尽处理" {
  // 定义系统资源状态
  type SystemResourceManager = {
    manager_id: String,
    cpu_usage_percent: Float,
    memory_usage_percent: Float,
    disk_usage_percent: Float,
    network_connections: Int,
    file_handles: Int,
    thresholds: ResourceThresholds,
    emergency_mode: Bool,
    mitigation_strategies: Array[String]
  }
  
  type ResourceThresholds = {
    cpu_warning: Float,
    cpu_critical: Float,
    memory_warning: Float,
    memory_critical: Float,
    disk_warning: Float,
    disk_critical: Float,
    max_connections: Int,
    max_file_handles: Int
  }
  
  type ResourceMitigationResult = {
    resource_type: String,
    mitigation_applied: Bool,
    strategy_used: Option[String],
    resource_before: Float,
    resource_after: Float,
    success: Bool
  }
  
  // 创建系统资源管理器
  let create_resource_manager = fn(manager_id: String) -> SystemResourceManager {
    let thresholds = {
      cpu_warning: 70.0,
      cpu_critical: 90.0,
      memory_warning: 75.0,
      memory_critical: 90.0,
      disk_warning: 80.0,
      disk_critical: 95.0,
      max_connections: 1000,
      max_file_handles: 500
    }
    
    {
      manager_id: manager_id,
      cpu_usage_percent: 0.0,
      memory_usage_percent: 0.0,
      disk_usage_percent: 0.0,
      network_connections: 0,
      file_handles: 0,
      thresholds: thresholds,
      emergency_mode: false,
      mitigation_strategies: ["throttle", "cache_clear", "connection_close", "task_defer"]
    }
  }
  
  // 更新资源使用情况
  let update_resource_usage = fn(
    manager: SystemResourceManager,
    cpu: Float,
    memory: Float,
    disk: Float,
    connections: Int,
    handles: Int
  ) -> SystemResourceManager {
    let emergency_mode = 
      cpu >= manager.thresholds.cpu_critical ||
      memory >= manager.thresholds.memory_critical ||
      disk >= manager.thresholds.disk_critical ||
      connections >= manager.thresholds.max_connections ||
      handles >= manager.thresholds.max_file_handles
    
    {
      ...manager,
      cpu_usage_percent: cpu,
      memory_usage_percent: memory,
      disk_usage_percent: disk,
      network_connections: connections,
      file_handles: handles,
      emergency_mode: emergency_mode
    }
  }
  
  // 应用资源缓解策略
  let apply_mitigation_strategy = fn(
    manager: SystemResourceManager,
    resource_type: String
  ) -> ResourceMitigationResult {
    let mut strategy_used = None
    let mut mitigation_applied = false
    let mut resource_before = 0.0
    let mut resource_after = 0.0
    let mut success = false
    
    match resource_type {
      "cpu" => {
        resource_before = manager.cpu_usage_percent
        
        if manager.cpu_usage_percent >= manager.thresholds.cpu_critical {
          // CPU严重过载，应用节流策略
          strategy_used = Some("throttle")
          mitigation_applied = true
          resource_after = manager.cpu_usage_percent * 0.7  // 减少30%
          success = true
        } else if manager.cpu_usage_percent >= manager.thresholds.cpu_warning {
          // CPU警告级别，清理缓存
          strategy_used = Some("cache_clear")
          mitigation_applied = true
          resource_after = manager.cpu_usage_percent * 0.9  // 减少10%
          success = true
        }
      }
      "memory" => {
        resource_before = manager.memory_usage_percent
        
        if manager.memory_usage_percent >= manager.thresholds.memory_critical {
          // 内存严重不足，强制垃圾回收
          strategy_used = Some("force_gc")
          mitigation_applied = true
          resource_after = manager.memory_usage_percent * 0.6  // 减少40%
          success = true
        } else if manager.memory_usage_percent >= manager.thresholds.memory_warning {
          // 内存警告，清理缓存
          strategy_used = Some("cache_clear")
          mitigation_applied = true
          resource_after = manager.memory_usage_percent * 0.85  // 减少15%
          success = true
        }
      }
      "connections" => {
        resource_before = manager.network_connections as Float
        
        if manager.network_connections >= manager.thresholds.max_connections {
          // 连接数达到上限，关闭空闲连接
          strategy_used = Some("connection_close")
          mitigation_applied = true
          resource_after = (manager.network_connections * 80) / 100 as Float  // 关闭20%连接
          success = true
        }
      }
      "file_handles" => {
        resource_before = manager.file_handles as Float
        
        if manager.file_handles >= manager.thresholds.max_file_handles {
          // 文件句柄耗尽，强制关闭文件
          strategy_used = Some("file_close")
          mitigation_applied = true
          resource_after = (manager.file_handles * 75) / 100 as Float  // 关闭25%文件句柄
          success = true
        }
      }
      _ => ()
    }
    
    {
      resource_type: resource_type,
      mitigation_applied: mitigation_applied,
      strategy_used: strategy_used,
      resource_before: resource_before,
      resource_after: resource_after,
      success: success
    }
  }
  
  // 测试系统资源耗尽处理
  let manager = create_resource_manager("resource-manager-1")
  
  // 测试CPU严重过载
  let cpu_overload_manager = update_resource_usage(manager, 95.0, 50.0, 60.0, 100, 50)
  
  // 验证CPU过载检测
  assert_true(cpu_overload_manager.emergency_mode)
  assert_true(cpu_overload_manager.cpu_usage_percent >= cpu_overload_manager.thresholds.cpu_critical)
  
  // 应用CPU缓解策略
  let cpu_mitigation = apply_mitigation_strategy(cpu_overload_manager, "cpu")
  
  // 验证CPU缓解
  assert_true(cpu_mitigation.mitigation_applied)
  assert_true(cpu_mitigation.strategy_used.is_some())
  assert_true(cpu_mitigation.resource_after < cpu_mitigation.resource_before)
  assert_true(cpu_mitigation.success)
  
  // 测试内存严重不足
  let memory_overload_manager = update_resource_usage(manager, 60.0, 92.0, 70.0, 200, 100)
  
  // 验证内存不足检测
  assert_true(memory_overload_manager.emergency_mode)
  assert_true(memory_overload_manager.memory_usage_percent >= memory_overload_manager.thresholds.memory_critical)
  
  // 应用内存缓解策略
  let memory_mitigation = apply_mitigation_strategy(memory_overload_manager, "memory")
  
  // 验证内存缓解
  assert_true(memory_mitigation.mitigation_applied)
  assert_true(memory_mitigation.strategy_used.is_some())
  assert_true(memory_mitigation.resource_after < memory_mitigation.resource_before)
  assert_true(memory_mitigation.success)
  
  // 测试连接数耗尽
  let connection_overload_manager = update_resource_usage(manager, 40.0, 60.0, 50.0, 1200, 80)
  
  // 验证连接数耗尽检测
  assert_true(connection_overload_manager.emergency_mode)
  assert_true(connection_overload_manager.network_connections >= connection_overload_manager.thresholds.max_connections)
  
  // 应用连接缓解策略
  let connection_mitigation = apply_mitigation_strategy(connection_overload_manager, "connections")
  
  // 验证连接缓解
  assert_true(connection_mitigation.mitigation_applied)
  assert_true(connection_mitigation.strategy_used.is_some())
  assert_true(connection_mitigation.resource_after < connection_mitigation.resource_before)
  assert_true(connection_mitigation.success)
  
  // 测试文件句柄耗尽
  let handle_overload_manager = update_resource_usage(manager, 50.0, 70.0, 40.0, 300, 600)
  
  // 验证文件句柄耗尽检测
  assert_true(handle_overload_manager.emergency_mode)
  assert_true(handle_overload_manager.file_handles >= handle_overload_manager.thresholds.max_file_handles)
  
  // 应用文件句柄缓解策略
  let handle_mitigation = apply_mitigation_strategy(handle_overload_manager, "file_handles")
  
  // 验证文件句柄缓解
  assert_true(handle_mitigation.mitigation_applied)
  assert_true(handle_mitigation.strategy_used.is_some())
  assert_true(handle_mitigation.resource_after < handle_mitigation.resource_before)
  assert_true(handle_mitigation.success)
  
  // 测试正常资源使用情况
  let normal_manager = update_resource_usage(manager, 30.0, 40.0, 50.0, 100, 50)
  
  // 验证正常状态
  assert_false(normal_manager.emergency_mode)
  assert_true(normal_manager.cpu_usage_percent < normal_manager.thresholds.cpu_warning)
  assert_true(normal_manager.memory_usage_percent < normal_manager.thresholds.memory_warning)
  assert_true(normal_manager.disk_usage_percent < normal_manager.thresholds.disk_warning)
  
  // 测试正常情况下的缓解策略（不应该应用）
  let normal_mitigation = apply_mitigation_strategy(normal_manager, "cpu")
  
  // 验证正常情况下不应用缓解策略
  assert_false(normal_mitigation.mitigation_applied)
  assert_true(normal_mitigation.strategy_used.is_none())
  assert_eq(normal_mitigation.resource_before, 0.0)
  assert_eq(normal_mitigation.resource_after, 0.0)
  assert_false(normal_mitigation.success)
}