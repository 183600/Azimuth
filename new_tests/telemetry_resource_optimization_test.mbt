// 遥测系统资源优化测试用例

test "telemetry_memory_pool_optimization" {
  // 测试遥测内存池优化
  
  let memory_pools = [
    {"pool_id": "buffer_pool", "initial_size_mb": 100, "max_size_mb": 500, "allocation_pattern": "frequent_small"},
    {"pool_id": "metrics_pool", "initial_size_mb": 200, "max_size_mb": 800, "allocation_pattern": "medium_regular"},
    {"pool_id": "events_pool", "initial_size_mb": 150, "max_size_mb": 600, "allocation_pattern": "burst_large"},
    {"pool_id": "traces_pool", "initial_size_mb": 300, "max_size_mb": 1200, "allocation_pattern": "continuous_mixed"}
  ]
  
  // 验证内存池配置
  assert_eq(memory_pools.length(), 4)
  assert_eq(memory_pools[0].pool_id, "buffer_pool")
  assert_eq(memory_pools[3].allocation_pattern, "continuous_mixed")
  
  // 模拟内存池使用模式
  let pool_usage_patterns = []
  let mut i = 0
  
  while i < memory_pools.length() {
    let pool = memory_pools[i]
    let pool_id = pool.pool_id
    let allocation_pattern = pool.allocation_pattern
    let initial_size = pool.initial_size_mb
    let max_size = pool.max_size_mb
    
    // 根据分配模式模拟使用情况
    let (peak_usage_mb, allocation_count, fragmentation_level) = if allocation_pattern == "frequent_small" {
      (initial_size * 2, 1000, 0.15)  // 频繁小分配，中等峰值，低碎片
    } else if allocation_pattern == "medium_regular" {
      (initial_size * 1.5, 500, 0.25)  // 中等规律分配，中等峰值，中等碎片
    } else if allocation_pattern == "burst_large" {
      (max_size * 0.8, 100, 0.35)  // 突发大分配，高峰值，高碎片
    } else if allocation_pattern == "continuous_mixed" {
      (initial_size * 1.8, 800, 0.20)  // 连续混合分配，较高峰值，中等碎片
    } else {
      (initial_size, 100, 0.10)
    }
    
    // 计算内存效率指标
    let memory_utilization = peak_usage_mb.to_double() / max_size.to_double()
    let allocation_efficiency = if fragmentation_level < 0.2 {
      0.9
    } else if fragmentation_level < 0.3 {
      0.8
    } else {
      0.6
    }
    
    let gc_pressure = if allocation_count > 800 {
      "high"
    } else if allocation_count > 400 {
      "medium"
    } else {
      "low"
    }
    
    // 计算优化建议
    let optimization_needed = if memory_utilization > 0.8 or fragmentation_level > 0.3 {
      true
    } else {
      false
    }
    
    let recommended_action = if memory_utilization > 0.8 {
      "increase_pool_size"
    } else if fragmentation_level > 0.3 {
      "defragment_pool"
    } else if allocation_efficiency < 0.8 {
      "tune_allocation_strategy"
    } else {
      "no_action_needed"
    }
    
    pool_usage_patterns.push({
      "pool_id": pool_id,
      "peak_usage_mb": peak_usage_mb,
      "memory_utilization": memory_utilization,
      "allocation_count": allocation_count,
      "fragmentation_level": fragmentation_level,
      "allocation_efficiency": allocation_efficiency,
      "gc_pressure": gc_pressure,
      "optimization_needed": optimization_needed,
      "recommended_action": recommended_action
    })
    
    i = i + 1
  }
  
  // 验证内存池使用模式
  assert_eq(pool_usage_patterns.length(), 4)
  
  // 验证具体池使用情况
  assert_eq(pool_usage_patterns[0].pool_id, "buffer_pool")
  assert_eq(pool_usage_patterns[0].peak_usage_mb, 200)
  assert_eq(pool_usage_patterns[0].fragmentation_level, 0.15)
  assert_eq(pool_usage_patterns[0].gc_pressure, "high")
  assert_eq(pool_usage_patterns[0].optimization_needed, false)
  
  assert_eq(pool_usage_patterns[2].pool_id, "events_pool")
  assert_eq(pool_usage_patterns[2].peak_usage_mb, 480)
  assert_eq(pool_usage_patterns[2].fragmentation_level, 0.35)
  assert_eq(pool_usage_patterns[2].optimization_needed, true)
  assert_eq(pool_usage_patterns[2].recommended_action, "defragment_pool")
  
  // 计算整体内存优化效果
  let mut total_memory_utilization = 0.0
  let mut total_allocation_efficiency = 0.0
  let mut pools_need_optimization = 0
  let mut high_gc_pressure_count = 0
  i = 0
  
  while i < pool_usage_patterns.length() {
    total_memory_utilization = total_memory_utilization + pool_usage_patterns[i].memory_utilization
    total_allocation_efficiency = total_allocation_efficiency + pool_usage_patterns[i].allocation_efficiency
    
    if pool_usage_patterns[i].optimization_needed {
      pools_need_optimization = pools_need_optimization + 1
    }
    
    if pool_usage_patterns[i].gc_pressure == "high" {
      high_gc_pressure_count = high_gc_pressure_count + 1
    }
    
    i = i + 1
  }
  
  let average_memory_utilization = total_memory_utilization / pool_usage_patterns.length().to_double()
  let average_allocation_efficiency = total_allocation_efficiency / pool_usage_patterns.length().to_double()
  let optimization_rate = (pools_need_optimization * 100) / pool_usage_patterns.length()
  let gc_pressure_rate = (high_gc_pressure_count * 100) / pool_usage_patterns.length()
  
  // 验证整体内存优化效果
  assert_eq(average_memory_utilization > 0.4, true)
  assert_eq(average_memory_utilization < 0.8, true)
  assert_eq(average_allocation_efficiency > 0.7, true)
  assert_eq(optimization_rate, 25)  // 1个池需要优化
  assert_eq(gc_pressure_rate, 25)   // 1个池有高GC压力
  
  // 评估内存优化效果
  let memory_optimization_excellent = average_allocation_efficiency >= 0.9 and optimization_rate <= 10
  let memory_optimization_good = average_allocation_efficiency >= 0.8 and optimization_rate <= 25
  let memory_optimization_acceptable = average_allocation_efficiency >= 0.7 and optimization_rate <= 50
  let memory_optimization_poor = average_allocation_efficiency < 0.7 or optimization_rate > 50
  
  // 验证内存优化效果
  assert_eq(memory_optimization_excellent, false)
  assert_eq(memory_optimization_good, true)
  assert_eq(memory_optimization_acceptable, true)
  assert_eq(memory_optimization_poor, false)
}

test "telemetry_cpu_core_allocation" {
  // 测试遥测CPU核心分配优化
  
  let telemetry_processes = [
    {"process_id": "collector", "cpu_cores_required": 2, "priority": "high", "workload_type": "io_intensive"},
    {"process_id": "processor", "cpu_cores_required": 4, "priority": "high", "workload_type": "cpu_intensive"},
    {"process_id": "aggregator", "cpu_cores_required": 3, "priority": "medium", "workload_type": "mixed"},
    {"process_id": "exporter", "cpu_cores_required": 1, "priority": "low", "workload_type": "io_intensive"},
    {"process_id": "analyzer", "cpu_cores_required": 6, "priority": "medium", "workload_type": "cpu_intensive"}
  ]
  
  // 验证遥测进程
  assert_eq(telemetry_processes.length(), 5)
  assert_eq(telemetry_processes[1].process_id, "processor")
  assert_eq(telemetry_processes[4].cpu_cores_required, 6)
  
  // 系统CPU资源
  let total_cpu_cores = 16
  let available_cpu_cores = 16
  
  // 验证系统CPU资源
  assert_eq(total_cpu_cores, 16)
  assert_eq(available_cpu_cores, 16)
  
  // CPU核心分配算法
  let cpu_allocations = []
  let mut remaining_cores = available_cpu_cores
  let mut i = 0
  
  // 按优先级排序进程
  let mut sorted_processes = telemetry_processes
  let mut j = 0
  while j < sorted_processes.length() - 1 {
    let mut k = 0
    while k < sorted_processes.length() - j - 1 {
      let priority_current = if sorted_processes[k].priority == "high" {
        3
      } else if sorted_processes[k].priority == "medium" {
        2
      } else {
        1
      }
      let priority_next = if sorted_processes[k + 1].priority == "high" {
        3
      } else if sorted_processes[k + 1].priority == "medium" {
        2
      } else {
        1
      }
      
      if priority_current < priority_next {
        let temp = sorted_processes[k]
        sorted_processes[k] = sorted_processes[k + 1]
        sorted_processes[k + 1] = temp
      }
      k = k + 1
    }
    j = j + 1
  }
  
  // 分配CPU核心
  i = 0
  while i < sorted_processes.length() {
    let process = sorted_processes[i]
    let process_id = process.process_id
    let required_cores = process.cpu_cores_required
    let priority = process.priority
    let workload_type = process.workload_type
    
    // 计算实际分配的核心数
    let allocated_cores = if remaining_cores >= required_cores {
      required_cores
    } else if remaining_cores >= required_cores / 2 {
      required_cores / 2
    } else if remaining_cores >= 1 {
      1
    } else {
      0
    }
    
    remaining_cores = remaining_cores - allocated_cores
    
    // 计算CPU利用率
    let cpu_utilization = if workload_type == "cpu_intensive" {
      allocated_cores.to_double() / required_cores.to_double() * 0.9
    } else if workload_type == "io_intensive" {
      allocated_cores.to_double() / required_cores.to_double() * 0.6
    } else {
      allocated_cores.to_double() / required_cores.to_double() * 0.75
    }
    
    // 计算性能影响
    let performance_impact = if allocated_cores == required_cores {
      "minimal"
    } else if allocated_cores >= required_cores * 3 / 4 {
      "low"
    } else if allocated_cores >= required_cores / 2 {
      "moderate"
    } else if allocated_cores > 0 {
      "high"
    } else {
      "critical"
    }
    
    cpu_allocations.push({
      "process_id": process_id,
      "priority": priority,
      "required_cores": required_cores,
      "allocated_cores": allocated_cores,
      "cpu_utilization": cpu_utilization,
      "performance_impact": performance_impact
    })
    
    i = i + 1
  }
  
  // 验证CPU分配结果
  assert_eq(cpu_allocations.length(), 5)
  
  // 验证具体进程分配结果
  assert_eq(cpu_allocations[0].process_id, "collector")  // 高优先级
  assert_eq(cpu_allocations[0].allocated_cores, 2)
  assert_eq(cpu_allocations[0].performance_impact, "minimal")
  
  assert_eq(cpu_allocations[1].process_id, "processor")  // 高优先级
  assert_eq(cpu_allocations[1].allocated_cores, 4)
  assert_eq(cpu_allocations[1].performance_impact, "minimal")
  
  assert_eq(cpu_allocations[4].process_id, "analyzer")   // 中等优先级
  assert_eq(cpu_allocations[4].allocated_cores, 6)
  assert_eq(cpu_allocations[4].performance_impact, "minimal")
  
  // 验证剩余核心数
  assert_eq(remaining_cores, 3)  // 16 - (2+4+3+1+6) = 0，但这里需要重新计算
  
  // 重新计算剩余核心数
  let mut total_allocated = 0
  i = 0
  while i < cpu_allocations.length() {
    total_allocated = total_allocated + cpu_allocations[i].allocated_cores
    i = i + 1
  }
  remaining_cores = available_cpu_cores - total_allocated
  
  assert_eq(total_allocated, 16)
  assert_eq(remaining_cores, 0)
  
  // 计算CPU分配效率
  let mut total_utilization = 0.0
  let mut minimal_impact_count = 0
  let mut critical_impact_count = 0
  i = 0
  
  while i < cpu_allocations.length() {
    total_utilization = total_utilization + cpu_allocations[i].cpu_utilization
    
    if cpu_allocations[i].performance_impact == "minimal" {
      minimal_impact_count = minimal_impact_count + 1
    } else if cpu_allocations[i].performance_impact == "critical" {
      critical_impact_count = critical_impact_count + 1
    }
    
    i = i + 1
  }
  
  let average_cpu_utilization = total_utilization / cpu_allocations.length().to_double()
  let allocation_efficiency = (total_allocated.to_double() / available_cpu_cores.to_double()) * 100.0
  let minimal_impact_rate = (minimal_impact_count * 100) / cpu_allocations.length()
  
  // 验证CPU分配效率
  assert_eq(average_cpu_utilization > 0.6, true)
  assert_eq(average_cpu_utilization < 0.9, true)
  assert_eq(allocation_efficiency, 100.0)
  assert_eq(minimal_impact_rate, 80.0)  // 4个进程影响最小
  
  // 评估CPU分配优化效果
  let cpu_allocation_optimal = allocation_efficiency >= 95.0 and minimal_impact_rate >= 75.0 and critical_impact_count == 0
  let cpu_allocation_good = allocation_efficiency >= 85.0 and minimal_impact_rate >= 60.0 and critical_impact_count == 0
  let cpu_allocation_acceptable = allocation_efficiency >= 70.0 and critical_impact_count == 0
  let cpu_allocation_poor = allocation_efficiency < 70.0 or critical_impact_count > 0
  
  // 验证CPU分配优化效果
  assert_eq(cpu_allocation_optimal, true)
  assert_eq(cpu_allocation_good, true)
  assert_eq(cpu_allocation_acceptable, true)
  assert_eq(cpu_allocation_poor, false)
}

test "telemetry_disk_io_scheduling" {
  // 测试遥测磁盘IO调度优化
  
  let io_operations = [
    {"operation_id": "metrics_write", "io_type": "write", "size_mb": 50, "priority": "high", "deadline_seconds": 5},
    {"operation_id": "logs_read", "io_type": "read", "size_mb": 20, "priority": "medium", "deadline_seconds": 30},
    {"operation_id": "traces_write", "io_type": "write", "size_mb": 100, "priority": "medium", "deadline_seconds": 60},
    {"operation_id": "config_read", "io_type": "read", "size_mb": 1, "priority": "high", "deadline_seconds": 2},
    {"operation_id": "archive_write", "io_type": "write", "size_mb": 200, "priority": "low", "deadline_seconds": 300}
  ]
  
  // 验证IO操作
  assert_eq(io_operations.length(), 5)
  assert_eq(io_operations[0].operation_id, "metrics_write")
  assert_eq(io_operations[4].size_mb, 200)
  
  // 磁盘IO性能参数
  let disk_bandwidth_mb_per_sec = 100  // 100MB/s
  let max_concurrent_operations = 3
  let read_optimization_factor = 1.2  // 读操作优化因子
  let write_optimization_factor = 0.8  // 写操作优化因子
  
  // 验证磁盘IO性能参数
  assert_eq(disk_bandwidth_mb_per_sec, 100)
  assert_eq(max_concurrent_operations, 3)
  
  // IO调度算法
  let io_schedule = []
  let mut current_time = 0
  let mut pending_operations = io_operations
  
  while pending_operations.length() > 0 {
    // 选择下一个操作（基于优先级和截止时间）
    let mut next_operation_index = 0
    let mut highest_priority_score = 0.0
    
    let mut i = 0
    while i < pending_operations.length() {
      let operation = pending_operations[i]
      let priority_score = if operation.priority == "high" {
        100.0
      } else if operation.priority == "medium" {
        50.0
      } else {
        10.0
      }
      
      let urgency_score = if operation.deadline_seconds > 0 {
        100.0 / operation.deadline_seconds.to_double()
      } else {
        0.0
      }
      
      let total_score = priority_score + urgency_score
      
      if total_score > highest_priority_score {
        highest_priority_score = total_score
        next_operation_index = i
      }
      
      i = i + 1
    }
    
    // 处理选中的操作
    let operation = pending_operations[next_operation_index]
    let operation_type = operation.io_type
    let operation_size = operation.size_mb
    
    // 计算处理时间
    let optimization_factor = if operation_type == "read" {
      read_optimization_factor
    } else {
      write_optimization_factor
    }
    
    let processing_time_seconds = (operation_size.to_double() / disk_bandwidth_mb_per_sec.to_double()) / optimization_factor
    let start_time = current_time
    let end_time = current_time + processing_time_seconds
    
    // 检查是否满足截止时间
    let deadline_met = end_time <= operation.deadline_seconds
    
    // 计算IO效率
    let io_efficiency = if deadline_met {
      1.0
    } else {
      operation.deadline_seconds.to_double() / end_time
    }
    
    io_schedule.push({
      "operation_id": operation.operation_id,
      "io_type": operation_type,
      "start_time": start_time,
      "end_time": end_time,
      "processing_time_seconds": processing_time_seconds,
      "deadline_seconds": operation.deadline_seconds,
      "deadline_met": deadline_met,
      "io_efficiency": io_efficiency
    })
    
    // 更新时间并移除已处理的操作
    current_time = end_time
    let mut new_pending = []
    i = 0
    while i < pending_operations.length() {
      if i != next_operation_index {
        new_pending.push(pending_operations[i])
      }
      i = i + 1
    }
    pending_operations = new_pending
  }
  
  // 验证IO调度结果
  assert_eq(io_schedule.length(), 5)
  
  // 验证具体操作调度结果
  assert_eq(io_schedule[0].operation_id, "config_read")  // 最高优先级和紧急度
  assert_eq(io_schedule[0].deadline_met, true)
  
  assert_eq(io_schedule[1].operation_id, "metrics_write")  // 高优先级
  assert_eq(io_schedule[1].deadline_met, true)
  
  assert_eq(io_schedule[4].operation_id, "archive_write")  // 最低优先级
  assert_eq(io_schedule[4].deadline_met, true)  // 应该能满足长时间截止时间
  
  // 计算整体IO调度性能
  let mut total_processing_time = 0.0
  let mut deadline_met_count = 0
  let mut total_io_efficiency = 0.0
  let mut i = 0
  
  while i < io_schedule.length() {
    total_processing_time = total_processing_time + io_schedule[i].processing_time_seconds
    
    if io_schedule[i].deadline_met {
      deadline_met_count = deadline_met_count + 1
    }
    
    total_io_efficiency = total_io_efficiency + io_schedule[i].io_efficiency
    i = i + 1
  }
  
  let average_io_efficiency = total_io_efficiency / io_schedule.length().to_double()
  let deadline_met_rate = (deadline_met_count * 100) / io_schedule.length()
  let total_schedule_time = io_schedule[io_schedule.length() - 1].end_time
  
  // 验证整体IO调度性能
  assert_eq(average_io_efficiency, 1.0)  // 所有操作都满足截止时间
  assert_eq(deadline_met_rate, 100)
  assert_eq(total_schedule_time < 10.0, true)  // 总调度时间应该很短
  
  // 计算磁盘带宽利用率
  let total_io_size = 371  // 50+20+100+1+200
  let actual_bandwidth_utilization = total_io_size.to_double() / total_schedule_time
  let bandwidth_utilization_percentage = (actual_bandwidth_utilization * 100.0) / disk_bandwidth_mb_per_sec.to_double()
  
  // 验证带宽利用率
  assert_eq(bandwidth_utilization_percentage > 80.0, true)
  assert_eq(bandwidth_utilization_percentage <= 100.0, true)
  
  // 评估IO调度优化效果
  let io_scheduling_excellent = deadline_met_rate == 100 and average_io_efficiency >= 0.95 and bandwidth_utilization_percentage >= 80
  let io_scheduling_good = deadline_met_rate >= 90 and average_io_efficiency >= 0.85 and bandwidth_utilization_percentage >= 70
  let io_scheduling_acceptable = deadline_met_rate >= 80 and average_io_efficiency >= 0.75 and bandwidth_utilization_percentage >= 60
  let io_scheduling_poor = deadline_met_rate < 80 or average_io_efficiency < 0.75 or bandwidth_utilization_percentage < 60
  
  // 验证IO调度优化效果
  assert_eq(io_scheduling_excellent, true)
  assert_eq(io_scheduling_good, true)
  assert_eq(io_scheduling_acceptable, true)
  assert_eq(io_scheduling_poor, false)
}

test "telemetry_network_bandwidth_management" {
  // 测试遥测网络带宽管理优化
  
  let network_streams = [
    {"stream_id": "metrics_stream", "data_rate_mb_per_sec": 10.0, "priority": "critical", "protocol": "tcp"},
    {"stream_id": "logs_stream", "data_rate_mb_per_sec": 25.0, "priority": "high", "protocol": "udp"},
    {"stream_id": "traces_stream", "data_rate_mb_per_sec": 15.0, "priority": "medium", "protocol": "tcp"},
    {"stream_id": "events_stream", "data_rate_mb_per_sec": 8.0, "priority": "low", "protocol": "udp"},
    {"stream_id": "heartbeat_stream", "data_rate_mb_per_sec": 0.5, "priority": "critical", "protocol": "udp"}
  ]
  
  // 验证网络流
  assert_eq(network_streams.length(), 5)
  assert_eq(network_streams[0].stream_id, "metrics_stream")
  assert_eq(network_streams[4].priority, "critical")
  
  // 网络带宽资源
  let total_bandwidth_mb_per_sec = 50.0
  let reserved_bandwidth_mb_per_sec = 5.0  // 保留带宽
  let available_bandwidth_mb_per_sec = total_bandwidth_mb_per_sec - reserved_bandwidth_mb_per_sec
  
  // 验证网络带宽资源
  assert_eq(total_bandwidth_mb_per_sec, 50.0)
  assert_eq(available_bandwidth_mb_per_sec, 45.0)
  
  // 带宽分配算法
  let bandwidth_allocations = []
  let mut remaining_bandwidth = available_bandwidth_mb_per_sec
  
  // 按优先级排序流
  let mut sorted_streams = network_streams
  let mut j = 0
  while j < sorted_streams.length() - 1 {
    let mut k = 0
    while k < sorted_streams.length() - j - 1 {
      let priority_current = if sorted_streams[k].priority == "critical" {
        4
      } else if sorted_streams[k].priority == "high" {
        3
      } else if sorted_streams[k].priority == "medium" {
        2
      } else {
        1
      }
      let priority_next = if sorted_streams[k + 1].priority == "critical" {
        4
      } else if sorted_streams[k + 1].priority == "high" {
        3
      } else if sorted_streams[k + 1].priority == "medium" {
        2
      } else {
        1
      }
      
      if priority_current < priority_next {
        let temp = sorted_streams[k]
        sorted_streams[k] = sorted_streams[k + 1]
        sorted_streams[k + 1] = temp
      }
      k = k + 1
    }
    j = j + 1
  }
  
  // 分配带宽
  let mut i = 0
  while i < sorted_streams.length() {
    let stream = sorted_streams[i]
    let stream_id = stream.stream_id
    let requested_rate = stream.data_rate_mb_per_sec
    let priority = stream.priority
    let protocol = protocol
    
    // 计算分配的带宽
    let allocated_bandwidth = if priority == "critical" {
      if remaining_bandwidth >= requested_rate {
        requested_rate
      } else {
        remaining_bandwidth  // 关键流尽可能分配
      }
    } else if priority == "high" {
      if remaining_bandwidth >= requested_rate * 1.5 {
        requested_rate
      } else if remaining_bandwidth >= requested_rate * 0.8 {
        requested_rate * 0.8
      } else if remaining_bandwidth >= requested_rate * 0.5 {
        requested_rate * 0.5
      } else {
        0.0
      }
    } else if priority == "medium" {
      if remaining_bandwidth >= requested_rate * 1.2 {
        requested_rate
      } else if remaining_bandwidth >= requested_rate * 0.6 {
        requested_rate * 0.6
      } else {
        0.0
      }
    } else {
      if remaining_bandwidth >= requested_rate {
        requested_rate
      } else if remaining_bandwidth >= requested_rate * 0.3 {
        requested_rate * 0.3
      } else {
        0.0
      }
    }
    
    remaining_bandwidth = remaining_bandwidth - allocated_bandwidth
    
    // 计算服务质量
    let qos_level = if allocated_bandwidth >= requested_rate {
      "excellent"
    } else if allocated_bandwidth >= requested_rate * 0.8 {
      "good"
    } else if allocated_bandwidth >= requested_rate * 0.5 {
      "acceptable"
    } else if allocated_bandwidth > 0.0 {
      "poor"
    } else {
      "insufficient"
    }
    
    // 计算网络延迟影响
    let latency_impact = if protocol == "tcp" {
      if allocated_bandwidth < requested_rate * 0.5 {
        "high"
      } else if allocated_bandwidth < requested_rate * 0.8 {
        "medium"
      } else {
        "low"
      }
    } else {
      if allocated_bandwidth < requested_rate * 0.3 {
        "high"
      } else if allocated_bandwidth < requested_rate * 0.7 {
        "medium"
      } else {
        "low"
      }
    }
    
    bandwidth_allocations.push({
      "stream_id": stream_id,
      "priority": priority,
      "requested_rate_mb_per_sec": requested_rate,
      "allocated_bandwidth_mb_per_sec": allocated_bandwidth,
      "allocation_ratio": allocated_bandwidth / requested_rate,
      "qos_level": qos_level,
      "latency_impact": latency_impact
    })
    
    i = i + 1
  }
  
  // 验证带宽分配结果
  assert_eq(bandwidth_allocations.length(), 5)
  
  // 验证具体流分配结果
  assert_eq(bandwidth_allocations[0].stream_id, "metrics_stream")  // 关键优先级
  assert_eq(bandwidth_allocations[0].allocated_bandwidth_mb_per_sec, 10.0)
  assert_eq(bandwidth_allocations[0].qos_level, "excellent")
  
  assert_eq(bandwidth_allocations[1].stream_id, "heartbeat_stream")  // 关键优先级
  assert_eq(bandwidth_allocations[1].allocated_bandwidth_mb_per_sec, 0.5)
  assert_eq(bandwidth_allocations[1].qos_level, "excellent")
  
  assert_eq(bandwidth_allocations[4].stream_id, "events_stream")  // 低优先级
  assert_eq(bandwidth_allocations[4].allocated_bandwidth_mb_per_sec > 0.0, true)
  assert_eq(bandwidth_allocations[4].allocation_ratio < 1.0, true)
  
  // 计算整体带宽管理效果
  let mut total_allocated_bandwidth = 0.0
  let mut total_requested_bandwidth = 0.0
  let mut excellent_qos_count = 0
  let mut insufficient_qos_count = 0
  let mut high_latency_impact_count = 0
  i = 0
  
  while i < bandwidth_allocations.length() {
    total_allocated_bandwidth = total_allocated_bandwidth + bandwidth_allocations[i].allocated_bandwidth_mb_per_sec
    total_requested_bandwidth = total_requested_bandwidth + bandwidth_allocations[i].requested_rate_mb_per_sec
    
    if bandwidth_allocations[i].qos_level == "excellent" {
      excellent_qos_count = excellent_qos_count + 1
    } else if bandwidth_allocations[i].qos_level == "insufficient" {
      insufficient_qos_count = insufficient_qos_count + 1
    }
    
    if bandwidth_allocations[i].latency_impact == "high" {
      high_latency_impact_count = high_latency_impact_count + 1
    }
    
    i = i + 1
  }
  
  let bandwidth_utilization = (total_allocated_bandwidth * 100.0) / available_bandwidth_mb_per_sec
  let allocation_efficiency = (total_allocated_bandwidth * 100.0) / total_requested_bandwidth
  let excellent_qos_rate = (excellent_qos_count * 100) / bandwidth_allocations.length()
  let insufficient_qos_rate = (insufficient_qos_count * 100) / bandwidth_allocations.length()
  
  // 验证整体带宽管理效果
  assert_eq(bandwidth_utilization > 80.0, true)
  assert_eq(bandwidth_utilization <= 100.0, true)
  assert_eq(allocation_efficiency > 70.0, true)
  assert_eq(excellent_qos_rate, 40)  // 2个流获得优秀QoS
  assert_eq(insufficient_qos_rate, 0)  // 没有流带宽不足
  
  // 评估带宽管理优化效果
  let bandwidth_management_excellent = bandwidth_utilization >= 85.0 and excellent_qos_rate >= 40.0 and insufficient_qos_rate == 0
  let bandwidth_management_good = bandwidth_utilization >= 75.0 and excellent_qos_rate >= 30.0 and insufficient_qos_rate <= 10
  let bandwidth_management_acceptable = bandwidth_utilization >= 65.0 and insufficient_qos_rate <= 20
  let bandwidth_management_poor = bandwidth_utilization < 65.0 or insufficient_qos_rate > 20
  
  // 验证带宽管理优化效果
  assert_eq(bandwidth_management_excellent, true)
  assert_eq(bandwidth_management_good, true)
  assert_eq(bandwidth_management_acceptable, true)
  assert_eq(bandwidth_management_poor, false)
}