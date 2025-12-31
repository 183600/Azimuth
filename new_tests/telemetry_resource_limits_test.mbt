// 遥测资源限制测试用例

test "telemetry_memory_usage_limits" {
  // 测试遥测系统内存使用限制
  
  let max_memory_mb = 1024           // 最大内存限制1GB
  let warning_threshold_mb = 768     // 警告阈值768MB
  let critical_threshold_mb = 896    // 严重阈值896MB
  let memory_check_interval_seconds = 30  // 内存检查间隔30秒
  
  // 验证内存限制配置
  assert_eq(max_memory_mb > warning_threshold_mb, true)
  assert_eq(warning_threshold_mb > critical_threshold_mb, false)  // critical应该比warning高
  assert_eq(critical_threshold_mb < max_memory_mb, true)
  assert_eq(memory_check_interval_seconds > 0, true)
  
  // 内存使用状态
  type MemoryState = {
    current_usage_mb: Double,
    peak_usage_mb: Double,
    allocation_count: Int,
    deallocation_count: Int,
    last_check_time: Int,
    warning_triggered: Bool,
    critical_triggered: Bool
  }
  
  // 内存分配记录
  type MemoryAllocation = {
    allocation_id: String,
    size_mb: Double,
    allocation_time: Int,
    data_type: String,
    is_active: Bool
  }
  
  // 初始化内存状态
  let mut memory_state = MemoryState {
    current_usage_mb: 0.0,
    peak_usage_mb: 0.0,
    allocation_count: 0,
    deallocation_count: 0,
    last_check_time: 0,
    warning_triggered: false,
    critical_triggered: false
  }
  
  // 模拟内存分配操作
  let memory_operations = [
    // 正常分配
    { operation: "allocate", size_mb: 50.0, data_type: "metric_buffer" },
    { operation: "allocate", size_mb: 100.0, data_type: "trace_storage" },
    { operation: "allocate", size_mb: 75.0, data_type: "log_cache" },
    { operation: "allocate", size_mb: 200.0, data_type: "aggregation_buffer" },
    // 释放部分内存
    { operation: "deallocate", size_mb: 50.0, data_type: "metric_buffer" },
    // 大量分配（可能触发限制）
    { operation: "allocate", size_mb: 300.0, data_type: "archive_buffer" },
    { operation: "allocate", size_mb: 250.0, data_type: "compression_cache" },
    { operation: "allocate", size_mb: 180.0, data_type: "index_buffer" },
    // 继续分配（超过限制）
    { operation: "allocate", size_mb: 150.0, data_type: "temp_processing" },
    // 释放内存
    { operation: "deallocate", size_mb: 200.0, data_type: "aggregation_buffer" },
    { operation: "deallocate", size_mb: 180.0, data_type: "index_buffer" }
  ]
  
  // 验证内存操作
  assert_eq(memory_operations.length(), 11)
  assert_eq(memory_operations[0].operation, "allocate")
  assert_eq(memory_operations[4].operation, "deallocate")
  
  // 执行内存操作
  let mut memory_allocations = []
  let mut allocation_counter = 0
  let mut deallocation_counter = 0
  let mut rejected_allocations = 0
  
  let mut i = 0
  while i < memory_operations.length() {
    let operation = memory_operations[i]
    let current_time = i * memory_check_interval_seconds
    
    match operation.operation {
      "allocate" => {
        // 检查是否超过内存限制
        let projected_usage = memory_state.current_usage_mb + operation.size_mb
        
        if projected_usage <= max_memory_mb.to_double() {
          // 执行分配
          memory_state.current_usage_mb = projected_usage
          memory_state.allocation_count = memory_state.allocation_count + 1
          allocation_counter = allocation_counter + 1
          
          // 更新峰值使用量
          if memory_state.current_usage_mb > memory_state.peak_usage_mb {
            memory_state.peak_usage_mb = memory_state.current_usage_mb
          }
          
          // 记录分配
          memory_allocations.push(MemoryAllocation {
            allocation_id: "alloc_" + allocation_counter.to_string(),
            size_mb: operation.size_mb,
            allocation_time: current_time,
            data_type: operation.data_type,
            is_active: true
          })
        } else {
          // 拒绝分配
          rejected_allocations = rejected_allocations + 1
        }
      }
      
      "deallocate" => {
        // 查找匹配的分配进行释放
        let mut allocation_found = false
        let mut j = 0
        while j < memory_allocations.length() {
          if memory_allocations[j].data_type == operation.data_type and 
             memory_allocations[j].is_active and
             memory_allocations[j].size_mb >= operation.size_mb - 0.1 and
             memory_allocations[j].size_mb <= operation.size_mb + 0.1 {
            
            // 释放内存
            memory_state.current_usage_mb = memory_state.current_usage_mb - memory_allocations[j].size_mb
            memory_state.deallocation_count = memory_state.deallocation_count + 1
            deallocation_counter = deallocation_counter + 1
            
            // 标记为非活跃
            let mut updated_allocation = memory_allocations[j]
            updated_allocation.is_active = false
            memory_allocations[j] = updated_allocation
            
            allocation_found = true
            break
          }
          j = j + 1
        }
        
        // 如果找不到对应的分配，假设成功释放
        if not allocation_found {
          memory_state.current_usage_mb = max(0.0, memory_state.current_usage_mb - operation.size_mb)
          memory_state.deallocation_count = memory_state.deallocation_count + 1
          deallocation_counter = deallocation_counter + 1
        }
      }
      
      _ => ()  // 忽略未知操作
    }
    
    // 检查内存阈值
    if memory_state.current_usage_mb >= critical_threshold_mb.to_double() {
      memory_state.critical_triggered = true
      memory_state.warning_triggered = true
    } else if memory_state.current_usage_mb >= warning_threshold_mb.to_double() {
      memory_state.warning_triggered = true
    }
    
    memory_state.last_check_time = current_time
    i = i + 1
  }
  
  // 验证内存管理结果
  assert_eq(memory_state.allocation_count, 7)  // 成功分配7次
  assert_eq(memory_state.deallocation_count, 3)  // 成功释放3次
  assert_eq(rejected_allocations, 1)  // 1次分配被拒绝
  assert_eq(memory_state.current_usage_mb > 0.0, true)
  assert_eq(memory_state.current_usage_mb < max_memory_mb.to_double(), true)
  
  // 验证峰值使用量
  assert_eq(memory_state.peak_usage_mb > warning_threshold_mb.to_double(), true)
  
  // 验证阈值触发
  assert_eq(memory_state.warning_triggered, true)  // 应该触发警告
  assert_eq(memory_state.critical_triggered, true)  // 应该触发严重警告
  
  // 计算内存利用率
  let memory_utilization = memory_state.current_usage_mb / max_memory_mb.to_double()
  
  // 验证内存利用率
  assert_eq(memory_utilization > 0.0, true)
  assert_eq(memory_utilization < 1.0, true)
  
  // 统计活跃分配
  let mut active_allocations = 0
  let mut total_allocated_size = 0.0
  
  i = 0
  while i < memory_allocations.length() {
    if memory_allocations[i].is_active {
      active_allocations = active_allocations + 1
      total_allocated_size = total_allocated_size + memory_allocations[i].size_mb
    }
    i = i + 1
  }
  
  // 验证活跃分配统计
  assert_eq(active_allocations, memory_state.allocation_count - memory_state.deallocation_count)
  assert_eq(total_allocated_size, memory_state.current_usage_mb)
}

test "telemetry_cpu_usage_throttling" {
  // 测试遥测系统CPU使用节流机制
  
  let max_cpu_usage_percent = 80.0    // 最大CPU使用率80%
  let throttling_threshold = 70.0     // 节流阈值70%
  let recovery_threshold = 50.0       // 恢复阈值50%
  let monitoring_interval_seconds = 10 // 监控间隔10秒
  
  // 验证CPU节流配置
  assert_eq(max_cpu_usage_percent > throttling_threshold, true)
  assert_eq(throttling_threshold > recovery_threshold, true)
  assert_eq(max_cpu_usage_percent <= 100.0, true)
  assert_eq(monitoring_interval_seconds > 0, true)
  
  // CPU使用状态
  type CpuState = {
    current_usage_percent: Double,
    average_usage_percent: Double,
    peak_usage_percent: Double,
    is_throttling_active: Bool,
    throttling_start_time: Int,
    total_throttling_periods: Int,
    processing_tasks_allowed: Int,
    processing_tasks_rejected: Int
  }
  
  // 处理任务
  type ProcessingTask = {
    task_id: String,
    task_type: String,
    cpu_cost: Double,
    priority: String,
    submission_time: Int,
    is_processed: Bool
  }
  
  // 初始化CPU状态
  let mut cpu_state = CpuState {
    current_usage_percent: 20.0,  // 初始CPU使用率20%
    average_usage_percent: 20.0,
    peak_usage_percent: 20.0,
    is_throttling_active: false,
    throttling_start_time: 0,
    total_throttling_periods: 0,
    processing_tasks_allowed: 0,
    processing_tasks_rejected: 0
  }
  
  // 模拟处理任务队列
  let processing_tasks = [
    ProcessingTask {
      task_id: "task_001",
      task_type: "metric_aggregation",
      cpu_cost: 15.0,
      priority: "high",
      submission_time: 0,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_002",
      task_type: "trace_processing",
      cpu_cost: 25.0,
      priority: "medium",
      submission_time: 10,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_003",
      task_type: "log_parsing",
      cpu_cost: 10.0,
      priority: "low",
      submission_time: 20,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_004",
      task_type: "data_compression",
      cpu_cost: 30.0,
      priority: "medium",
      submission_time: 30,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_005",
      task_type: "network_transmission",
      cpu_cost: 20.0,
      priority: "high",
      submission_time: 40,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_006",
      task_type: "storage_write",
      cpu_cost: 18.0,
      priority: "medium",
      submission_time: 50,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_007",
      task_type: "index_update",
      cpu_cost: 12.0,
      priority: "low",
      submission_time: 60,
      is_processed: false
    },
    ProcessingTask {
      task_id: "task_008",
      task_type: "alert_evaluation",
      cpu_cost: 8.0,
      priority: "high",
      submission_time: 70,
      is_processed: false
    }
  ]
  
  // 验证处理任务
  assert_eq(processing_tasks.length(), 8)
  assert_eq(processing_tasks[0].task_type, "metric_aggregation")
  assert_eq(processing_tasks[3].cpu_cost, 30.0)
  
  // 模拟CPU使用率变化
  let cpu_usage_scenarios = [
    25.0,   // 初始低使用率
    45.0,   // 逐渐增加
    60.0,   // 接近节流阈值
    75.0,   // 超过节流阈值，应激活节流
    85.0,   // 高使用率，节流活跃
    78.0,   // 仍然高于阈值
    65.0,   // 下降但仍高于恢复阈值
    55.0,   // 下降到恢复阈值附近
    45.0,   // 恢复到正常水平
    35.0    // 稳定在低水平
  ]
  
  // 验证CPU使用率场景
  assert_eq(cpu_usage_scenarios.length(), 10)
  assert_eq(cpu_usage_scenarios[0], 25.0)
  assert_eq(cpu_usage_scenarios[3], 75.0)
  
  // 执行任务处理和CPU节流
  let mut i = 0
  while i < processing_tasks.length() {
    let task = processing_tasks[i]
    let current_time = task.submission_time
    
    // 更新CPU使用率
    if i < cpu_usage_scenarios.length() {
      cpu_state.current_usage_percent = cpu_usage_scenarios[i]
      
      // 更新峰值使用率
      if cpu_state.current_usage_percent > cpu_state.peak_usage_percent {
        cpu_state.peak_usage_percent = cpu_state.current_usage_percent
      }
      
      // 更新平均使用率（简化计算）
      cpu_state.average_usage_percent = (cpu_state.average_usage_percent + cpu_state.current_usage_percent) / 2.0
    }
    
    // 检查节流状态
    let was_throttling = cpu_state.is_throttling_active
    
    if cpu_state.current_usage_percent > throttling_threshold and not cpu_state.is_throttling_active {
      // 激活节流
      cpu_state.is_throttling_active = true
      cpu_state.throttling_start_time = current_time
      cpu_state.total_throttling_periods = cpu_state.total_throttling_periods + 1
    } else if cpu_state.current_usage_percent < recovery_threshold and cpu_state.is_throttling_active {
      // 停用节流
      cpu_state.is_throttling_active = false
    }
    
    // 节流逻辑：根据优先级和CPU使用率决定是否处理任务
    let should_process = if cpu_state.current_usage_percent > max_cpu_usage_percent {
      // 超过最大限制，只处理高优先级任务
      task.priority == "high"
    } else if cpu_state.is_throttling_active {
      // 节流期间，限制处理
      task.priority == "high" or (task.priority == "medium" and task.cpu_cost < 20.0)
    } else {
      // 正常期间，处理所有任务
      true
    }
    
    // 检查任务是否会导致超过CPU限制
    let projected_usage = cpu_state.current_usage_percent + task.cpu_cost
    if projected_usage > max_cpu_usage_percent {
      // 如果会导致超过限制，降低处理概率
      let random_factor = task.task_id.length() * 7
      should_process = should_process and (random_factor % 100) < 50  // 50%概率
    }
    
    if should_process {
      cpu_state.processing_tasks_allowed = cpu_state.processing_tasks_allowed + 1
      // 模拟任务处理（暂时增加CPU使用率）
      cpu_state.current_usage_percent = cpu_state.current_usage_percent + task.cpu_cost * 0.1
    } else {
      cpu_state.processing_tasks_rejected = cpu_state.processing_tasks_rejected + 1
    }
    
    i = i + 1
  }
  
  // 验证CPU节流结果
  assert_eq(cpu_state.processing_tasks_allowed + cpu_state.processing_tasks_rejected, processing_tasks.length())
  assert_eq(cpu_state.processing_tasks_allowed > 0, true)
  assert_eq(cpu_state.processing_tasks_rejected > 0, true)  // 应该有被拒绝的任务
  
  // 验证节流状态
  assert_eq(cpu_state.is_throttling_active, false)  // 最终应该停止节流
  assert_eq(cpu_state.total_throttling_periods > 0, true)  // 应该有过节流周期
  
  // 验证CPU使用率统计
  assert_eq(cpu_state.peak_usage_percent > throttling_threshold, true)
  assert_eq(cpu_state.current_usage_percent < max_cpu_usage_percent, true)
  
  // 计算任务处理率
  let processing_rate = cpu_state.processing_tasks_allowed.to_double() / 
                       processing_tasks.length().to_double()
  
  // 验证处理率
  assert_eq(processing_rate > 0.0, true)
  assert_eq(processing_rate < 1.0, true)  // 不是所有任务都被处理
  
  // 按优先级统计处理情况
  let mut priority_stats = {
    "high": { submitted: 0, processed: 0 },
    "medium": { submitted: 0, processed: 0 },
    "low": { submitted: 0, processed: 0 }
  }
  
  i = 0
  while i < processing_tasks.length() {
    let task = processing_tasks[i]
    let priority = task.priority
    
    // 统计提交数
    let stats = priority_stats[priority]
    let updated_submitted = { submitted: stats.submitted + 1, processed: stats.processed }
    priority_stats[priority] = updated_submitted
    
    // 重新判断处理结果（简化版）
    let should_process = if cpu_usage_scenarios[i] > max_cpu_usage_percent {
      task.priority == "high"
    } else if cpu_usage_scenarios[i] > throttling_threshold {
      task.priority == "high" or (task.priority == "medium" and task.cpu_cost < 20.0)
    } else {
      true
    }
    
    if should_process {
      let current_stats = priority_stats[priority]
      let updated_processed = { submitted: current_stats.submitted, processed: current_stats.processed + 1 }
      priority_stats[priority] = updated_processed
    }
    
    i = i + 1
  }
  
  // 验证优先级处理统计
  assert_eq(priority_stats["high"].processed > 0, true)  // 高优先级任务应该被处理
  assert_eq(priority_stats["medium"].processed > 0, true)  // 中等优先级任务部分被处理
  
  // 验证高优先级任务的处理率更高
  let high_processing_rate = if priority_stats["high"].submitted > 0 {
    priority_stats["high"].processed.to_double() / priority_stats["high"].submitted.to_double()
  } else { 0.0 }
  
  let medium_processing_rate = if priority_stats["medium"].submitted > 0 {
    priority_stats["medium"].processed.to_double() / priority_stats["medium"].submitted.to_double()
  } else { 0.0 }
  
  assert_eq(high_processing_rate >= medium_processing_rate, true)
}

test "telemetry_network_bandwidth_limits" {
  // 测试遥测系统网络带宽限制
  
  let max_bandwidth_mbps = 100.0      // 最大带宽100Mbps
  let warning_threshold_mbps = 80.0   // 警告阈值80Mbps
  let burst_allowance_seconds = 10    // 突发允许时间10秒
  let rate_limit_window_seconds = 60  // 速率限制窗口60秒
  
  // 验证带宽限制配置
  assert_eq(max_bandwidth_mbps > warning_threshold_mbps, true)
  assert_eq(burst_allowance_seconds > 0, true)
  assert_eq(rate_limit_window_seconds > 0, true)
  
  // 网络传输状态
  type NetworkState = {
    current_bandwidth_mbps: Double,
    total_bytes_transmitted: Int,
    peak_bandwidth_mbps: Double,
    burst_active: Bool,
    burst_start_time: Int,
    rate_limited: Bool,
    transmitted_packets: Int,
    dropped_packets: Int
  }
  
  // 网络数据包
  type DataPacket = {
    packet_id: String,
    data_type: String,
    size_bytes: Int,
    priority: String,
    transmission_time: Int,
    is_transmitted: Bool
  }
  
  // 初始化网络状态
  let mut network_state = NetworkState {
    current_bandwidth_mbps: 0.0,
    total_bytes_transmitted: 0,
    peak_bandwidth_mbps: 0.0,
    burst_active: false,
    burst_start_time: 0,
    rate_limited: false,
    transmitted_packets: 0,
    dropped_packets: 0
  }
  
  // 模拟网络数据包传输
  let data_packets = [
    DataPacket {
      packet_id: "packet_001",
      data_type: "metric_batch",
      size_bytes: 10240,  // 10KB
      priority: "high",
      transmission_time: 0,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_002",
      data_type: "trace_data",
      size_bytes: 51200,  // 50KB
      priority: "medium",
      transmission_time: 5,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_003",
      data_type: "log_stream",
      size_bytes: 25600,  // 25KB
      priority: "low",
      transmission_time: 10,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_004",
      data_type: "alert_data",
      size_bytes: 2048,   // 2KB
      priority: "high",
      transmission_time: 15,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_005",
      data_type: "metric_batch",
      size_bytes: 15360,  // 15KB
      priority: "medium",
      transmission_time: 20,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_006",
      data_type: "trace_data",
      size_bytes: 76800,  // 75KB
      priority: "medium",
      transmission_time: 25,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_007",
      data_type: "log_stream",
      size_bytes: 38400,  // 38KB
      priority: "low",
      transmission_time: 30,
      is_transmitted: false
    },
    DataPacket {
      packet_id: "packet_008",
      data_type: "bulk_export",
      size_bytes: 204800, // 200KB
      priority: "low",
      transmission_time: 35,
      is_transmitted: false
    }
  ]
  
  // 验证数据包
  assert_eq(data_packets.length(), 8)
  assert_eq(data_packets[0].size_bytes, 10240)
  assert_eq(data_packets[7].size_bytes, 204800)
  
  // 模拟网络带宽使用模式
  let bandwidth_usage_pattern = [
    10.0,   // 初始低带宽使用
    25.0,   // 逐渐增加
    45.0,   // 中等使用
    75.0,   // 高使用但仍在限制内
    95.0,   // 接近最大带宽
    110.0,  // 超过最大带宽，应触发限制
    105.0,  // 仍然超过限制
    85.0,   // 回到警告阈值以下
    60.0,   // 中等使用
    40.0    // 稳定在低水平
  ]
  
  // 验证带宽使用模式
  assert_eq(bandwidth_usage_pattern.length(), 10)
  assert_eq(bandwidth_usage_pattern[5], 110.0)
  
  // 执行数据包传输和带宽限制
  let mut bytes_in_window = 0
  let mut window_start_time = 0
  let max_bytes_per_window = (max_bandwidth_mbps * 1024 * 1024 / 8.0 * rate_limit_window_seconds.to_double()).to_int()
  
  let mut i = 0
  while i < data_packets.length() {
    let packet = data_packets[i]
    let current_time = packet.transmission_time
    
    // 更新带宽使用率
    if i < bandwidth_usage_pattern.length() {
      network_state.current_bandwidth_mbps = bandwidth_usage_pattern[i]
      
      // 更新峰值带宽
      if network_state.current_bandwidth_mbps > network_state.peak_bandwidth_mbps {
        network_state.peak_bandwidth_mbps = network_state.current_bandwidth_mbps
      }
    }
    
    // 检查速率限制窗口
    if current_time - window_start_time >= rate_limit_window_seconds {
      // 重置窗口
      bytes_in_window = 0
      window_start_time = current_time
      network_state.rate_limited = false
    }
    
    // 检查突发模式
    if network_state.current_bandwidth_mbps > warning_threshold_mbps and not network_state.burst_active {
      network_state.burst_active = true
      network_state.burst_start_time = current_time
    } else if network_state.current_bandwidth_mbps < warning_threshold_mbps and network_state.burst_active {
      let burst_duration = current_time - network_state.burst_start_time
      if burst_duration > burst_allowance_seconds {
        network_state.burst_active = false
      }
    }
    
    // 带宽限制逻辑
    let can_transmit = if network_state.current_bandwidth_mbps > max_bandwidth_mbps {
      // 超过最大带宽，只传输高优先级数据包
      packet.priority == "high"
    } else if network_state.rate_limited {
      // 速率限制期间，限制传输
      packet.priority == "high" or packet.size_bytes < 50000
    } else if network_state.burst_active {
      // 突发期间，允许高优先级和中等优先级
      packet.priority == "high" or packet.priority == "medium"
    } else {
      // 正常期间，允许所有传输
      true
    }
    
    // 检查窗口内字节数限制
    let projected_bytes = bytes_in_window + packet.size_bytes
    if projected_bytes > max_bytes_per_window {
      // 超过窗口限制，降低传输概率
      let random_factor = packet.packet_id.length() * 11
      can_transmit = can_transmit and (random_factor % 100) < 70  // 70%概率
    }
    
    if can_transmit {
      // 传输数据包
      network_state.transmitted_packets = network_state.transmitted_packets + 1
      network_state.total_bytes_transmitted = network_state.total_bytes_transmitted + packet.size_bytes
      bytes_in_window = bytes_in_window + packet.size_bytes
      
      // 检查是否需要启动速率限制
      if bytes_in_window > max_bytes_per_window * 8 / 10 {  // 80%阈值
        network_state.rate_limited = true
      }
    } else {
      // 丢弃数据包
      network_state.dropped_packets = network_state.dropped_packets + 1
    }
    
    i = i + 1
  }
  
  // 验证网络传输结果
  assert_eq(network_state.transmitted_packets + network_state.dropped_packets, data_packets.length())
  assert_eq(network_state.transmitted_packets > 0, true)
  assert_eq(network_state.dropped_packets > 0, true)  // 应该有被丢弃的数据包
  
  // 验证带宽统计
  assert_eq(network_state.peak_bandwidth_mbps > max_bandwidth_mbps, true)
  assert_eq(network_state.total_bytes_transmitted > 0, true)
  
  // 计算传输率
  let transmission_rate = network_state.transmitted_packets.to_double() / 
                         data_packets.length().to_double()
  
  // 验证传输率
  assert_eq(transmission_rate > 0.0, true)
  assert_eq(transmission_rate < 1.0, true)
  
  // 计算带宽利用率
  let avg_bandwidth_usage = network_state.total_bytes_transmitted.to_double() * 8.0 / 
                           (1024.0 * 1024.0 * rate_limit_window_seconds.to_double())
  let bandwidth_utilization = avg_bandwidth_usage / max_bandwidth_mbps
  
  // 验证带宽利用率
  assert_eq(bandwidth_utilization > 0.0, true)
  assert_eq(bandwidth_utilization <= 1.0, true)
  
  // 按优先级统计传输情况
  let mut priority_transmission_stats = {
    "high": { submitted: 0, transmitted: 0 },
    "medium": { submitted: 0, transmitted: 0 },
    "low": { submitted: 0, transmitted: 0 }
  }
  
  i = 0
  while i < data_packets.length() {
    let packet = data_packets[i]
    let priority = packet.priority
    
    // 统计提交数
    let stats = priority_transmission_stats[priority]
    let updated_submitted = { submitted: stats.submitted + 1, transmitted: stats.transmitted }
    priority_transmission_stats[priority] = updated_submitted
    
    // 重新判断传输结果（简化版）
    let can_transmit = if bandwidth_usage_pattern[i] > max_bandwidth_mbps {
      packet.priority == "high"
    } else if bandwidth_usage_pattern[i] > warning_threshold_mbps {
      packet.priority == "high" or packet.priority == "medium"
    } else {
      true
    }
    
    if can_transmit {
      let current_stats = priority_transmission_stats[priority]
      let updated_transmitted = { submitted: current_stats.submitted, transmitted: current_stats.transmitted + 1 }
      priority_transmission_stats[priority] = updated_transmitted
    }
    
    i = i + 1
  }
  
  // 验证优先级传输统计
  assert_eq(priority_transmission_stats["high"].transmitted > 0, true)  // 高优先级应该被传输
  
  // 验证高优先级数据包的传输率最高
  let high_transmission_rate = if priority_transmission_stats["high"].submitted > 0 {
    priority_transmission_stats["high"].transmitted.to_double() / 
    priority_transmission_stats["high"].submitted.to_double()
  } else { 0.0 }
  
  let medium_transmission_rate = if priority_transmission_stats["medium"].submitted > 0 {
    priority_transmission_stats["medium"].transmitted.to_double() / 
    priority_transmission_stats["medium"].submitted.to_double()
  } else { 0.0 }
  
  let low_transmission_rate = if priority_transmission_stats["low"].submitted > 0 {
    priority_transmission_stats["low"].transmitted.to_double() / 
    priority_transmission_stats["low"].submitted.to_double()
  } else { 0.0 }
  
  assert_eq(high_transmission_rate >= medium_transmission_rate, true)
  assert_eq(medium_transmission_rate >= low_transmission_rate, true)
}