// Azimuth Resource Limit and Recovery Tests
// This file contains comprehensive test cases for resource limit handling and recovery

// Test 1: Memory Limit Handling and Recovery
test "memory limit handling and recovery mechanisms" {
  // Create memory limit manager
  let memory_manager = azimuth::MemoryLimitManager::new()
  
  // Configure memory limits
  azimuth::MemoryLimitManager::configure(memory_manager, {
    "max_heap_size_mb": 512,
    "warning_threshold_mb": 400,
    "critical_threshold_mb": 480,
    "auto_gc": true,
    "emergency_cleanup": true
  })
  
  // Test normal operation under memory limits
  let normal_data = []
  for i in 0..1000 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = azimuth::Time::now() + i * 1000,
      service_name = "memory-test-service",
      metric_name = "normal.metric",
      metric_value = i.to_double(),
      attributes = [
        ("data_id", i.to_string()),
        ("type", "normal")
      ]
    )
    normal_data.push(data_point)
  }
  
  let memory_usage_before = azimuth::MemoryLimitManager::get_memory_usage(memory_manager)
  
  // Process normal data
  let process_result = azimuth::MemoryLimitManager::process_data(memory_manager, normal_data)
  
  assert_true(process_result.success)
  assert_eq(process_result.processed_count, 1000)
  
  let memory_usage_after = azimuth::MemoryLimitManager::get_memory_usage(memory_manager)
  
  // Memory usage should be reasonable
  assert_true(memory_usage_after.heap_size_mb < 200)
  
  // Test memory pressure handling
  let large_data = []
  for i in 0..10000 {
    // Create larger data points to increase memory pressure
    let large_attributes = []
    for j in 0..100 {
      large_attributes.push(("attr_" + j.to_string(), "large_value_" + i.to_string() + "_" + j.to_string()))
    }
    
    let data_point = azimuth::TelemetryData::new(
      timestamp = azimuth::Time::now() + i * 1000,
      service_name = "memory-test-service",
      metric_name = "large.metric",
      metric_value = i.to_double(),
      attributes = large_attributes
    )
    large_data.push(data_point)
  }
  
  // Process large data to trigger memory pressure
  let large_process_result = azimuth::MemoryLimitManager::process_data(memory_manager, large_data)
  
  // Should either succeed with memory management or fail gracefully
  if large_process_result.success {
    assert_true(large_process_result.memory_management_triggered)
  } else {
    assert_true(large_process_result.reason.contains("memory_limit"))
  }
  
  // Test memory warning threshold
  let memory_status = azimuth::MemoryLimitManager::get_memory_status(memory_manager)
  
  if memory_status.heap_size_mb > 400 {
    assert_true(memory_status.warning_triggered)
  }
  
  // Test critical memory handling
  if memory_status.heap_size_mb > 480 {
    assert_true(memory_status.critical_triggered)
    assert_true(memory_status.emergency_cleanup_executed)
  }
  
  // Test memory recovery
  let recovery_result = azimuth::MemoryLimitManager::execute_memory_recovery(memory_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.memory_freed_mb > 0)
  
  let memory_after_recovery = azimuth::MemoryLimitManager::get_memory_usage(memory_manager)
  
  // Memory should be reduced after recovery
  assert_true(memory_after_recovery.heap_size_mb < memory_status.heap_size_mb)
  
  // Test garbage collection optimization
  let gc_result = azimuth::MemoryLimitManager::optimize_gc(memory_manager)
  
  assert_true(gc_result.success)
  assert_true(gc_result.collections_executed > 0)
  assert_true(gc_result.memory_freed_mb > 0)
  
  // Test memory leak detection
  let leak_detection = azimuth::MemoryLimitManager::detect_memory_leaks(memory_manager)
  
  assert_true(leak_detection.scan_completed)
  
  if leak_detection.potential_leaks_detected {
    assert_true(leak_detection.leak_reports.length() > 0)
    
    for leak_report in leak_detection.leak_reports {
      assert_true(leak_report.object_type.length() > 0)
      assert_true(leak_report.leak_size_bytes > 0)
      assert_true(leak_report.stack_trace.length() > 0)
    }
  }
  
  // Test memory pool management
  let pool_config = {
    "small_object_pool_size": 1000,
    "medium_object_pool_size": 500,
    "large_object_pool_size": 100,
    "pool_expansion_factor": 1.5
  }
  
  azimuth::MemoryLimitManager::configure_object_pools(memory_manager, pool_config)
  
  // Test object pool usage
  let pool_objects = []
  for i in 0..2000 {
    let obj = azimuth::MemoryLimitManager::get_from_pool(memory_manager, "small")
    pool_objects.push(obj)
  }
  
  // Return objects to pool
  for obj in pool_objects {
    azimuth::MemoryLimitManager::return_to_pool(memory_manager, obj)
  }
  
  let pool_stats = azimuth::MemoryLimitManager::get_pool_stats(memory_manager)
  
  assert_true(pool_stats.small_pool.hit_rate > 0.8) // High hit rate expected
  assert_true(pool_stats.total_pooled_objects > 0)
  
  // Test memory pressure backoff
  let backoff_config = {
    "enabled": true,
    "initial_delay_ms": 100,
    "max_delay_ms": 5000,
    "backoff_factor": 2.0
  }
  
  azimuth::MemoryLimitManager::configure_backoff(memory_manager, backoff_config)
  
  // Simulate continuous memory pressure
  let continuous_results = []
  
  for i in 0..10 {
    let pressure_data = []
    for j in 0..1000 {
      let data_point = azimuth::TelemetryData::new(
        timestamp = azimuth::Time::now() + j * 1000,
        service_name = "memory-pressure-service",
        metric_name = "pressure.metric",
        metric_value = (i * 1000 + j).to_double(),
        attributes = [("batch", i.to_string()), ("index", j.to_string())]
      )
      pressure_data.push(data_point)
    }
    
    let result = azimuth::MemoryLimitManager::process_data(memory_manager, pressure_data)
    continuous_results.push(result)
    
    if !result.success {
      // Should back off on failure
      assert_true(result.backoff_applied)
    }
    
    // Small delay between batches
    azimuth::Thread::sleep(50)
  }
  
  // Verify backoff behavior
  let failure_count = continuous_results.filter(|r| !r.success).length()
  let backoff_count = continuous_results.filter(|r| r.backoff_applied).length()
  
  if failure_count > 0 {
    assert_true(backoff_count > 0)
  }
  
  // Final memory check
  let final_memory_status = azimuth::MemoryLimitManager::get_memory_status(memory_manager)
  assert_true(final_memory_status.healthy)
  assert_true(final_memory_status.heap_size_mb < 512) // Should be under limit
}

// Test 2: CPU Limit Handling and Throttling
test "CPU limit handling and throttling mechanisms" {
  // Create CPU limit manager
  let cpu_manager = azimuth::CPULimitManager::new()
  
  // Configure CPU limits
  azimuth::CPULimitManager::configure(cpu_manager, {
    "max_cpu_usage_percent": 80,
    "warning_threshold_percent": 70,
    "critical_threshold_percent": 90,
    "throttling_enabled": true,
    "adaptive_throttling": true
  })
  
  // Test normal CPU usage
  let normal_tasks = []
  for i in 0..100 {
    let task = azimuth::CPUTask::new(
      id = "normal-task-" + i.to_string(),
      complexity = "low",
      estimated_cpu_ms = 10,
      priority = "normal"
    )
    normal_tasks.push(task)
  }
  
  let cpu_usage_before = azimuth::CPULimitManager::get_cpu_usage(cpu_manager)
  
  // Execute normal tasks
  let normal_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, normal_tasks)
  
  assert_true(normal_result.success)
  assert_eq(normal_result.completed_tasks, 100)
  assert_false(normal_result.throttling_applied)
  
  // Test CPU-intensive tasks
  let intensive_tasks = []
  for i in 0..50 {
    let task = azimuth::CPUTask::new(
      id = "intensive-task-" + i.to_string(),
      complexity = "high",
      estimated_cpu_ms = 100,
      priority = "normal"
    )
    intensive_tasks.push(task)
  }
  
  // Execute intensive tasks to trigger CPU pressure
  let intensive_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, intensive_tasks)
  
  assert_true(intensive_result.success)
  
  // Check if throttling was applied
  let cpu_usage_after = azimuth::CPULimitManager::get_cpu_usage(cpu_manager)
  
  if cpu_usage_after.current_usage_percent > 70 {
    assert_true(intensive_result.throttling_applied)
    assert_true(intensive_result.throttled_tasks > 0)
  }
  
  // Test adaptive throttling
  let adaptive_config = {
    "enabled": true,
    "initial_throttle_rate": 0.1,
    "max_throttle_rate": 0.8,
    "adjustment_interval_ms": 1000,
    "cpu_target_percent": 75
  }
  
  azimuth::CPULimitManager::configure_adaptive_throttling(cpu_manager, adaptive_config)
  
  // Generate sustained CPU load
  let sustained_tasks = []
  for i in 0..200 {
    let task = azimuth::CPUTask::new(
      id = "sustained-task-" + i.to_string(),
      complexity = "medium",
      estimated_cpu_ms = 50,
      priority = "normal"
    )
    sustained_tasks.push(task)
  }
  
  let sustained_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, sustained_tasks)
  
  assert_true(sustained_result.success)
  
  if sustained_result.adaptive_throttling_applied {
    assert_true(sustained_result.throttle_rate > 0.0)
    assert_true(sustained_result.throttle_rate <= adaptive_config.max_throttle_rate)
  }
  
  // Test CPU priority management
  let priority_tasks = []
  
  // Add high priority tasks
  for i in 0..10 {
    let task = azimuth::CPUTask::new(
      id = "priority-task-" + i.to_string(),
      complexity = "high",
      estimated_cpu_ms = 200,
      priority = "high"
    )
    priority_tasks.push(task)
  }
  
  // Add low priority tasks
  for i in 10..30 {
    let task = azimuth::CPUTask::new(
      id = "priority-task-" + i.to_string(),
      complexity = "medium",
      estimated_cpu_ms = 50,
      priority = "low"
    )
    priority_tasks.push(task)
  }
  
  let priority_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, priority_tasks)
  
  assert_true(priority_result.success)
  
  // High priority tasks should be completed first
  assert_true(priority_result.high_priority_completed >= 10)
  
  // Under CPU pressure, low priority tasks might be throttled more
  if priority_result.throttling_applied {
    assert_true(priority_result.low_priority_throttled >= priority_result.high_priority_throttled)
  }
  
  // Test CPU affinity and core management
  let core_config = {
    "available_cores": 4,
    "reserved_cores": 1,
    "telemetry_cores": 2,
    "enable_affinity": true
  }
  
  azimuth::CPULimitManager::configure_core_management(cpu_manager, core_config)
  
  let core_aware_tasks = []
  for i in 0..20 {
    let task = azimuth::CPUTask::new(
      id = "core-aware-task-" + i.to_string(),
      complexity = "medium",
      estimated_cpu_ms = 100,
      priority = "normal",
      preferred_core = (i % core_config.telemetry_cores).to_int()
    )
    core_aware_tasks.push(task)
  }
  
  let core_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, core_aware_tasks)
  
  assert_true(core_result.success)
  assert_true(core_result.core_affinity_applied)
  
  // Test CPU burst handling
  let burst_config = {
    "enabled": true,
    "burst_limit_percent": 95,
    "burst_duration_ms": 5000,
    "cooldown_period_ms": 10000
  }
  
  azimuth::CPULimitManager::configure_burst_handling(cpu_manager, burst_config)
  
  // Generate CPU burst
  let burst_tasks = []
  for i in 0..100 {
    let task = azimuth::CPUTask::new(
      id = "burst-task-" + i.to_string(),
      complexity = "high",
      estimated_cpu_ms = 150,
      priority = "normal"
    )
    burst_tasks.push(task)
  }
  
  let burst_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, burst_tasks)
  
  assert_true(burst_result.success)
  
  if burst_result.burst_mode_triggered {
    assert_true(burst_result.burst_duration_ms <= burst_config.burst_duration_ms)
  }
  
  // Test CPU monitoring and alerting
  let monitoring_config = {
    "monitoring_interval_ms": 1000,
    "alert_threshold_percent": 85,
    "sustained_threshold_ms": 5000
  }
  
  azimuth::CPULimitManager::configure_monitoring(cpu_manager, monitoring_config)
  
  // Start monitoring
  azimuth::CPULimitManager::start_monitoring(cpu_manager)
  
  // Generate sustained load
  let monitoring_tasks = []
  for i in 0..50 {
    let task = azimuth::CPUTask::new(
      id = "monitoring-task-" + i.to_string(),
      complexity = "high",
      estimated_cpu_ms = 200,
      priority = "normal"
    )
    monitoring_tasks.push(task)
  }
  
  let monitoring_result = azimuth::CPULimitManager::execute_tasks(cpu_manager, monitoring_tasks)
  
  // Wait for monitoring to collect data
  azimuth::Thread::sleep(2000)
  
  let monitoring_data = azimuth::CPULimitManager::get_monitoring_data(cpu_manager)
  
  assert_true(monitoring_data.samples.length() > 0)
  assert_true(monitoring_data.average_cpu_usage > 0)
  
  if monitoring_data.alerts_triggered {
    assert_true(monitoring_data.alerts.length() > 0)
  }
  
  // Stop monitoring
  azimuth::CPULimitManager::stop_monitoring(cpu_manager)
  
  // Test CPU recovery mechanisms
  let recovery_result = azimuth::CPULimitManager::execute_cpu_recovery(cpu_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.throttling_reduced)
  assert_true(recovery_result.tasks_recheduled > 0)
  
  // Final CPU check
  let final_cpu_status = azimuth::CPULimitManager::get_cpu_status(cpu_manager)
  assert_true(final_cpu_status.healthy)
  assert_true(final_cpu_status.current_usage_percent <= monitoring_config.alert_threshold_percent)
}

// Test 3: Network Bandwidth Limit and Recovery
test "network bandwidth limits and recovery mechanisms" {
  // Create network limit manager
  let network_manager = azimuth::NetworkLimitManager::new()
  
  // Configure network limits
  azimuth::NetworkLimitManager::configure(network_manager, {
    "max_bandwidth_mbps": 100,
    "warning_threshold_mbps": 80,
    "critical_threshold_mbps": 95,
    "throttling_enabled": true,
    "compression_enabled": true
  })
  
  // Test normal network usage
  let normal_packets = []
  for i in 0..100 {
    let packet = azimuth::NetworkPacket::new(
      id = "normal-packet-" + i.to_string(),
      size_bytes = 1024, // 1KB
      priority = "normal",
      destination = "telemetry-collector"
    )
    normal_packets.push(packet)
  }
  
  let bandwidth_before = azimuth::NetworkLimitManager::get_bandwidth_usage(network_manager)
  
  // Send normal packets
  let normal_result = azimuth::NetworkLimitManager::send_packets(network_manager, normal_packets)
  
  assert_true(normal_result.success)
  assert_eq(normal_result.sent_packets, 100)
  assert_false(normal_result.throttling_applied)
  
  // Test high bandwidth usage
  let large_packets = []
  for i in 0..1000 {
    let packet = azimuth::NetworkPacket::new(
      id = "large-packet-" + i.to_string(),
      size_bytes = 10240, // 10KB
      priority = "normal",
      destination = "telemetry-collector"
    )
    large_packets.push(packet)
  }
  
  // Send large packets to trigger bandwidth pressure
  let large_result = azimuth::NetworkLimitManager::send_packets(network_manager, large_packets)
  
  assert_true(large_result.success)
  
  // Check if throttling was applied
  let bandwidth_after = azimuth::NetworkLimitManager::get_bandwidth_usage(network_manager)
  
  if bandwidth_after.current_usage_mbps > 80 {
    assert_true(large_result.throttling_applied)
    assert_true(large_result.throttled_packets > 0)
  }
  
  // Test network compression
  let compression_config = {
    "enabled": true,
    "algorithm": "lz4",
    "compression_threshold_bytes": 2048,
    "min_compression_ratio": 1.5
  }
  
  azimuth::NetworkLimitManager::configure_compression(network_manager, compression_config)
  
  // Create compressible packets
  let compressible_packets = []
  for i in 0..100 {
    let packet = azimuth::NetworkPacket::new(
      id = "compressible-packet-" + i.to_string(),
      size_bytes = 5120, // 5KB
      priority = "normal",
      destination = "telemetry-collector"
    )
    
    // Add compressible data
    packet.data = "repeating_pattern_" + "x".repeat(5000)
    compressible_packets.push(packet)
  }
  
  let compression_result = azimuth::NetworkLimitManager::send_packets(network_manager, compressible_packets)
  
  assert_true(compression_result.success)
  
  if compression_result.compression_applied {
    assert_true(compression_result.compression_ratio > 1.5)
    assert_true(compression_result.bandwidth_saved_mbps > 0)
  }
  
  // Test network priority queue
  let priority_packets = []
  
  // Add high priority packets
  for i in 0..10 {
    let packet = azimuth::NetworkPacket::new(
      id = "priority-packet-" + i.to_string(),
      size_bytes = 2048,
      priority = "high",
      destination = "alert-collector"
    )
    priority_packets.push(packet)
  }
  
  // Add low priority packets
  for i in 10..50 {
    let packet = azimuth::NetworkPacket::new(
      id = "low-priority-packet-" + i.to_string(),
      size_bytes = 2048,
      priority = "low",
      destination = "analytics-collector"
    )
    priority_packets.push(packet)
  }
  
  let priority_result = azimuth::NetworkLimitManager::send_packets(network_manager, priority_packets)
  
  assert_true(priority_result.success)
  
  // High priority packets should be sent first
  assert_true(priority_result.high_priority_sent >= 10)
  
  // Under bandwidth pressure, low priority packets might be dropped more
  if priority_result.throttling_applied {
    assert_true(priority_result.low_priority_dropped >= priority_result.high_priority_dropped)
  }
  
  // Test network congestion control
  let congestion_config = {
    "enabled": true,
    "congestion_threshold_percent": 85,
    "backoff_factor": 0.5,
    "recovery_factor": 1.2,
    "min_window_size": 1024,
    "max_window_size": 65536
  }
  
  azimuth::NetworkLimitManager::configure_congestion_control(network_manager, congestion_config)
  
  // Simulate network congestion
  let congestion_packets = []
  for i in 0..200 {
    let packet = azimuth::NetworkPacket::new(
      id = "congestion-packet-" + i.to_string(),
      size_bytes = 4096,
      priority = "normal",
      destination = "telemetry-collector"
    )
    congestion_packets.push(packet)
  }
  
  let congestion_result = azimuth::NetworkLimitManager::send_packets(network_manager, congestion_packets)
  
  assert_true(congestion_result.success)
  
  if congestion_result.congestion_detected {
    assert_true(congestion_result.window_size_reduced)
    assert_true(congestion_result.backoff_applied)
  }
  
  // Test network recovery
  let recovery_result = azimuth::NetworkLimitManager::execute_network_recovery(network_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.congestion_reduced)
  assert_true(recovery_result.window_size_adjusted)
  
  // Test network monitoring
  let monitoring_config = {
    "monitoring_interval_ms": 1000,
    "metrics_collection": true,
    "alert_threshold_mbps": 90
  }
  
  azimuth::NetworkLimitManager::configure_monitoring(network_manager, monitoring_config)
  
  // Start monitoring
  azimuth::NetworkLimitManager::start_monitoring(network_manager)
  
  // Generate network traffic for monitoring
  let monitoring_packets = []
  for i in 0..100 {
    let packet = azimuth::NetworkPacket::new(
      id = "monitoring-packet-" + i.to_string(),
      size_bytes = 8192,
      priority = "normal",
      destination = "monitoring-collector"
    )
    monitoring_packets.push(packet)
  }
  
  azimuth::NetworkLimitManager::send_packets(network_manager, monitoring_packets)
  
  // Wait for monitoring to collect data
  azimuth::Thread::sleep(2000)
  
  let monitoring_data = azimuth::NetworkLimitManager::get_monitoring_data(network_manager)
  
  assert_true(monitoring_data.samples.length() > 0)
  assert_true(monitoring_data.average_bandwidth_mbps > 0)
  
  if monitoring_data.alerts_triggered {
    assert_true(monitoring_data.alerts.length() > 0)
  }
  
  // Stop monitoring
  azimuth::NetworkLimitManager::stop_monitoring(network_manager)
  
  // Test network failover
  let failover_config = {
    "enabled": true,
    "primary_endpoint": "primary-telemetry.example.com",
    "backup_endpoints": [
      "backup1-telemetry.example.com",
      "backup2-telemetry.example.com"
    ],
    "failover_timeout_ms": 5000,
    "health_check_interval_ms": 10000
  }
  
  azimuth::NetworkLimitManager::configure_failover(network_manager, failover_config)
  
  // Test failover by simulating primary failure
  azimuth::NetworkLimitManager::simulate_endpoint_failure(network_manager, "primary-telemetry.example.com")
  
  let failover_packets = []
  for i in 0..20 {
    let packet = azimuth::NetworkPacket::new(
      id = "failover-packet-" + i.to_string(),
      size_bytes = 1024,
      priority = "high",
      destination = "primary-telemetry.example.com"
    )
    failover_packets.push(packet)
  }
  
  let failover_result = azimuth::NetworkLimitManager::send_packets(network_manager, failover_packets)
  
  assert_true(failover_result.success)
  assert_true(failover_result.failover_triggered)
  assert_true(failover_result.backup_endpoint_used)
  
  // Test network quality adaptation
  let quality_config = {
    "enabled": true,
    "latency_threshold_ms": 100,
    "packet_loss_threshold_percent": 1,
    "jitter_threshold_ms": 50,
    "adaptation_interval_ms": 5000
  }
  
  azimuth::NetworkLimitManager::configure_quality_adaptation(network_manager, quality_config)
  
  // Simulate poor network quality
  azimuth::NetworkLimitManager::simulate_poor_quality(network_manager, {
    "latency_ms": 150,
    "packet_loss_percent": 2,
    "jitter_ms": 75
  })
  
  let quality_packets = []
  for i in 0..30 {
    let packet = azimuth::NetworkPacket::new(
      id = "quality-packet-" + i.to_string(),
      size_bytes = 2048,
      priority = "normal",
      destination = "telemetry-collector"
    )
    quality_packets.push(packet)
  }
  
  let quality_result = azimuth::NetworkLimitManager::send_packets(network_manager, quality_packets)
  
  assert_true(quality_result.success)
  
  if quality_result.quality_adaptation_applied {
    assert_true(quality_result.reduced_packet_size)
    assert_true(quality_result.increased_redundancy)
  }
  
  // Final network check
  let final_network_status = azimuth::NetworkLimitManager::get_network_status(network_manager)
  assert_true(final_network_status.healthy)
  assert_true(final_network_status.current_bandwidth_mbps <= failover_config.max_bandwidth_mbps)
}

// Test 4: Disk Space Limit and Cleanup
test "disk space limits and cleanup mechanisms" {
  // Create disk limit manager
  let disk_manager = azimuth::DiskLimitManager::new()
  
  // Configure disk limits
  azimuth::DiskLimitManager::configure(disk_manager, {
    "max_disk_usage_percent": 85,
    "warning_threshold_percent": 75,
    "critical_threshold_percent": 90,
    "auto_cleanup": true,
    "retention_days": 30
  })
  
  // Test normal disk usage
  let normal_files = []
  for i in 0..100 {
    let file = azimuth::DiskFile::new(
      path = "/telemetry/normal/file-" + i.to_string() + ".log",
      size_bytes = 1024, // 1KB
      created_at = azimuth::Time::now() - i * 60000, // 1 minute ago per file
      category = "logs"
    )
    normal_files.push(file)
  }
  
  let disk_usage_before = azimuth::DiskLimitManager::get_disk_usage(disk_manager)
  
  // Write normal files
  let normal_result = azimuth::DiskLimitManager::write_files(disk_manager, normal_files)
  
  assert_true(normal_result.success)
  assert_eq(normal_result.written_files, 100)
  assert_false(normal_result.cleanup_triggered)
  
  // Test high disk usage
  let large_files = []
  for i in 0..1000 {
    let file = azimuth::DiskFile::new(
      path = "/telemetry/large/file-" + i.to_string() + ".data",
      size_bytes = 10240, // 10KB
      created_at = azimuth::Time::now() - i * 1000, // 1 second ago per file
      category = "data"
    )
    large_files.push(file)
  }
  
  // Write large files to trigger disk pressure
  let large_result = azimuth::DiskLimitManager::write_files(disk_manager, large_files)
  
  assert_true(large_result.success)
  
  // Check if cleanup was triggered
  let disk_usage_after = azimuth::DiskLimitManager::get_disk_usage(disk_manager)
  
  if disk_usage_after.usage_percent > 75 {
    assert_true(large_result.cleanup_triggered)
    assert_true(large_result.files_deleted > 0)
  }
  
  // Test retention-based cleanup
  let retention_config = {
    "enabled": true,
    "retention_days": 7,
    "categories": ["logs", "temp", "cache"],
    "min_free_space_percent": 10
  }
  
  azimuth::DiskLimitManager::configure_retention(disk_manager, retention_config)
  
  // Create old files for cleanup
  let old_files = []
  for i in 0..50 {
    let file = azimuth::DiskFile::new(
      path = "/telemetry/old/old-file-" + i.to_string() + ".log",
      size_bytes = 2048,
      created_at = azimuth::Time::now() - (10 + i) * 86400000, // 10+ days ago
      category = "logs"
    )
    old_files.push(file)
  }
  
  azimuth::DiskLimitManager::write_files(disk_manager, old_files)
  
  // Execute retention cleanup
  let retention_result = azimuth::DiskLimitManager::execute_retention_cleanup(disk_manager)
  
  assert_true(retention_result.success)
  assert_true(retention_result.files_deleted > 0)
  assert_true(retention_result.space_freed_mb > 0)
  
  // Test category-based cleanup
  let category_config = {
    "categories": {
      "logs": {"max_size_gb": 5, "max_files": 10000},
      "data": {"max_size_gb": 20, "max_files": 50000},
      "cache": {"max_size_gb": 2, "max_files": 1000},
      "temp": {"max_size_gb": 1, "max_files": 500}
    }
  }
  
  azimuth::DiskLimitManager::configure_categories(disk_manager, category_config)
  
  // Create files that exceed category limits
  let category_files = []
  for i in 0..15000 { // Exceed logs max_files
    let file = azimuth::DiskFile::new(
      path = "/telemetry/logs/excess-" + i.to_string() + ".log",
      size_bytes = 1024,
      created_at = azimuth::Time::now() - i * 1000,
      category = "logs"
    )
    category_files.push(file)
  }
  
  azimuth::DiskLimitManager::write_files(disk_manager, category_files)
  
  // Execute category cleanup
  let category_result = azimuth::DiskLimitManager::execute_category_cleanup(disk_manager)
  
  assert_true(category_result.success)
  assert_true(category_result.categories_processed.contains("logs"))
  
  // Test disk space monitoring
  let monitoring_config = {
    "monitoring_interval_ms": 5000,
    "alert_threshold_percent": 80,
    "predictive_analysis": true
  }
  
  azimuth::DiskLimitManager::configure_monitoring(disk_manager, monitoring_config)
  
  // Start monitoring
  azimuth::DiskLimitManager::start_monitoring(disk_manager)
  
  // Generate disk activity for monitoring
  let monitoring_files = []
  for i in 0..100 {
    let file = azimuth::DiskFile::new(
      path = "/telemetry/monitoring/monitor-" + i.to_string() + ".data",
      size_bytes = 5120,
      created_at = azimuth::Time::now(),
      category = "data"
    )
    monitoring_files.push(file)
  }
  
  azimuth::DiskLimitManager::write_files(disk_manager, monitoring_files)
  
  // Wait for monitoring to collect data
  azimuth::Thread::sleep(6000)
  
  let monitoring_data = azimuth::DiskLimitManager::get_monitoring_data(disk_manager)
  
  assert_true(monitoring_data.samples.length() > 0)
  assert_true(monitoring_data.growth_rate_mb_per_hour >= 0)
  
  if monitoring_data.alerts_triggered {
    assert_true(monitoring_data.alerts.length() > 0)
  }
  
  if monitoring_data.predictive_analysis_available {
    assert_true(monitoring_data.predicted_fullness_date.length() > 0)
  }
  
  // Stop monitoring
  azimuth::DiskLimitManager::stop_monitoring(disk_manager)
  
  // Test disk space recovery
  let recovery_result = azimuth::DiskLimitManager::execute_disk_recovery(disk_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.space_freed_mb > 0)
  assert_true(recovery_result.optimizations_applied)
  
  // Test disk quota management
  let quota_config = {
    "enabled": true,
    "quotas": {
      "telemetry": {"max_size_gb": 50, "max_files": 100000},
      "logs": {"max_size_gb": 10, "max_files": 50000},
      "temp": {"max_size_gb": 5, "max_files": 10000}
    }
  }
  
  azimuth::DiskLimitManager::configure_quotas(disk_manager, quota_config)
  
  // Test quota enforcement
  let quota_files = []
  for i in 0..60000 { // Exceed logs quota
    let file = azimuth::DiskFile::new(
      path = "/telemetry/quota/quota-" + i.to_string() + ".log",
      size_bytes = 2048,
      created_at = azimuth::Time::now(),
      category = "logs",
      quota = "telemetry"
    )
    quota_files.push(file)
  }
  
  let quota_result = azimuth::DiskLimitManager::write_files(disk_manager, quota_files)
  
  assert_true(quota_result.success)
  
  if quota_result.quota_enforced {
    assert_true(quota_result.quota_exceeded)
    assert_true(quota_result.files_rejected > 0)
  }
  
  // Test disk space optimization
  let optimization_config = {
    "enabled": true,
    "compression": true,
    "deduplication": true,
    "archival": true
  }
  
  azimuth::DiskLimitManager::configure_optimization(disk_manager, optimization_config)
  
  // Execute optimization
  let optimization_result = azimuth::DiskLimitManager::execute_optimization(disk_manager)
  
  assert_true(optimization_result.success)
  
  if optimization_result.compression_applied {
    assert_true(optimization_result.compression_ratio > 1.0)
    assert_true(optimization_result.space_saved_mb > 0)
  }
  
  if optimization_result.deduplication_applied {
    assert_true(optimization_result.duplicate_files_found > 0)
    assert_true(optimization_result.duplicate_space_freed_mb > 0)
  }
  
  // Final disk check
  let final_disk_status = azimuth::DiskLimitManager::get_disk_status(disk_manager)
  assert_true(final_disk_status.healthy)
  assert_true(final_disk_status.usage_percent <= retention_config.max_disk_usage_percent)
}

// Test 5: Connection Pool Limit and Recovery
test "connection pool limits and recovery mechanisms" {
  // Create connection pool manager
  let pool_manager = azimuth::ConnectionPoolManager::new()
  
  // Configure connection pool limits
  azimuth::ConnectionPoolManager::configure(pool_manager, {
    "max_connections": 100,
    "min_connections": 10,
    "warning_threshold": 80,
    "critical_threshold": 95,
    "connection_timeout_ms": 5000,
    "idle_timeout_ms": 30000
  })
  
  // Test normal connection usage
  let normal_connections = []
  for i in 0..50 {
    let connection = azimuth::Connection::new(
      id = "normal-conn-" + i.to_string(),
      host = "telemetry-db.example.com",
      port = 5432,
      status = "active"
    )
    normal_connections.push(connection)
  }
  
  let pool_status_before = azimuth::ConnectionPoolManager::get_pool_status(pool_manager)
  
  // Acquire normal connections
  let normal_result = azimuth::ConnectionPoolManager::acquire_connections(pool_manager, normal_connections)
  
  assert_true(normal_result.success)
  assert_eq(normal_result.acquired_connections, 50)
  assert_false(normal_result.pool_exhausted)
  
  // Test connection pool exhaustion
  let excessive_connections = []
  for i in 0..150 { // More than max_connections
    let connection = azimuth::Connection::new(
      id = "excess-conn-" + i.to_string(),
      host = "telemetry-db.example.com",
      port = 5432,
      status = "pending"
    )
    excessive_connections.push(connection)
  }
  
  // Try to acquire excessive connections
  let excessive_result = azimuth::ConnectionPoolManager::acquire_connections(pool_manager, excessive_connections)
  
  assert_true(excessive_result.success)
  
  // Should have hit connection limit
  assert_true(excessive_result.pool_exhausted)
  assert_true(excessive_result.rejected_connections > 0)
  assert_true(excessive_result.waiting_connections > 0)
  
  // Test connection timeout handling
  let timeout_config = {
    "enabled": true,
    "acquisition_timeout_ms": 2000,
    "query_timeout_ms": 10000,
    "retry_attempts": 3
  }
  
  azimuth::ConnectionPoolManager::configure_timeouts(pool_manager, timeout_config)
  
  // Create connections that will timeout
  let timeout_connections = []
  for i in 0..20 {
    let connection = azimuth::Connection::new(
      id = "timeout-conn-" + i.to_string(),
      host = "slow-telemetry-db.example.com",
      port = 5432,
      status = "slow"
    )
    timeout_connections.push(connection)
  }
  
  let timeout_result = azimuth::ConnectionPoolManager::acquire_connections(pool_manager, timeout_connections)
  
  assert_true(timeout_result.success)
  
  if timeout_result.timeouts_occurred {
    assert_true(timeout_result.timed_out_connections > 0)
  }
  
  // Test connection health checking
  let health_config = {
    "enabled": true,
    "check_interval_ms": 10000,
    "check_timeout_ms": 1000,
    "unhealthy_threshold": 3
  }
  
  azimuth::ConnectionPoolManager::configure_health_checks(pool_manager, health_config)
  
  // Start health checking
  azimuth::ConnectionPoolManager::start_health_checks(pool_manager)
  
  // Simulate unhealthy connections
  let unhealthy_connections = []
  for i in 0..10 {
    let connection = azimuth::Connection::new(
      id = "unhealthy-conn-" + i.to_string(),
      host = "unhealthy-db.example.com",
      port = 5432,
      status = "unhealthy"
    )
    unhealthy_connections.push(connection)
  }
  
  azimuth::ConnectionPoolManager::acquire_connections(pool_manager, unhealthy_connections)
  
  // Wait for health checks
  azimuth::Thread::sleep(2000)
  
  let health_status = azimuth::ConnectionPoolManager::get_health_status(pool_manager)
  
  assert_true(health_status.checks_completed)
  
  if health_status.unhealthy_connections_detected {
    assert_true(health_status.unhealthy_connections.length() > 0)
    assert_true(health_status.connections_replaced > 0)
  }
  
  // Stop health checking
  azimuth::ConnectionPoolManager::stop_health_checks(pool_manager)
  
  // Test connection pool recovery
  let recovery_result = azimuth::ConnectionPoolManager::execute_pool_recovery(pool_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.connections_recovered > 0)
  assert_true(recovery_result.pool_size_optimized)
  
  // Test connection pool scaling
  let scaling_config = {
    "enabled": true,
    "scale_up_threshold": 80,
    "scale_down_threshold": 30,
    "scale_up_factor": 1.5,
    "scale_down_factor": 0.8,
    "min_scale_interval_ms": 30000
  }
  
  azimuth::ConnectionPoolManager::configure_scaling(pool_manager, scaling_config)
  
  // Generate load to trigger scaling
  let scaling_connections = []
  for i in 0..80 { // Approach scale_up_threshold
    let connection = azimuth::Connection::new(
      id = "scaling-conn-" + i.to_string(),
      host = "telemetry-db.example.com",
      port = 5432,
      status = "active"
    )
    scaling_connections.push(connection)
  }
  
  let scaling_result = azimuth::ConnectionPoolManager::acquire_connections(pool_manager, scaling_connections)
  
  assert_true(scaling_result.success)
  
  if scaling_result.scaling_triggered {
    assert_true(scaling_result.scaled_up)
    assert_true(scaling_result.new_connections_added > 0)
  }
  
  // Test connection pool monitoring
  let monitoring_config = {
    "enabled": true,
    "metrics_interval_ms": 5000,
    "alert_threshold": 90
  }
  
  azimuth::ConnectionPoolManager::configure_monitoring(pool_manager, monitoring_config)
  
  // Start monitoring
  azimuth::ConnectionPoolManager::start_monitoring(pool_manager)
  
  // Generate activity for monitoring
  let monitoring_connections = []
  for i in 0..30 {
    let connection = azimuth::Connection::new(
      id = "monitoring-conn-" + i.to_string(),
      host = "telemetry-db.example.com",
      port = 5432,
      status = "active"
    )
    monitoring_connections.push(connection)
  }
  
  azimuth::ConnectionPoolManager::acquire_connections(pool_manager, monitoring_connections)
  
  // Wait for monitoring to collect data
  azimuth::Thread::sleep(6000)
  
  let monitoring_data = azimuth::ConnectionPoolManager::get_monitoring_data(pool_manager)
  
  assert_true(monitoring_data.samples.length() > 0)
  assert_true(monitoring_data.average_active_connections >= 0)
  
  if monitoring_data.alerts_triggered {
    assert_true(monitoring_data.alerts.length() > 0)
  }
  
  // Stop monitoring
  azimuth::ConnectionPoolManager::stop_monitoring(pool_manager)
  
  // Test connection pool failover
  let failover_config = {
    "enabled": true,
    "primary_hosts": ["primary-db.example.com"],
    "backup_hosts": ["backup1-db.example.com", "backup2-db.example.com"],
    "failover_timeout_ms": 5000
  }
  
  azimuth::ConnectionPoolManager::configure_failover(pool_manager, failover_config)
  
  // Simulate primary host failure
  azimuth::ConnectionPoolManager::simulate_host_failure(pool_manager, "primary-db.example.com")
  
  let failover_connections = []
  for i in 0..20 {
    let connection = azimuth::Connection::new(
      id = "failover-conn-" + i.to_string(),
      host = "primary-db.example.com",
      port = 5432,
      status = "pending"
    )
    failover_connections.push(connection)
  }
  
  let failover_result = azimuth::ConnectionPoolManager::acquire_connections(pool_manager, failover_connections)
  
  assert_true(failover_result.success)
  assert_true(failover_result.failover_triggered)
  assert_true(failover_result.backup_host_used)
  
  // Test connection pool load balancing
  let load_balancing_config = {
    "enabled": true,
    "algorithm": "round_robin",
    "hosts": [
      "db1.example.com",
      "db2.example.com",
      "db3.example.com"
    ],
    "health_check_enabled": true
  }
  
  azimuth::ConnectionPoolManager::configure_load_balancing(pool_manager, load_balancing_config)
  
  let load_balancing_connections = []
  for i in 0..30 {
    let connection = azimuth::Connection::new(
      id = "lb-conn-" + i.to_string(),
      host = "", // Will be assigned by load balancer
      port = 5432,
      status = "pending"
    )
    load_balancing_connections.push(connection)
  }
  
  let lb_result = azimuth::ConnectionPoolManager::acquire_connections(pool_manager, load_balancing_connections)
  
  assert_true(lb_result.success)
  assert_true(lb_result.load_balancing_applied)
  
  // Verify connections are distributed across hosts
  let host_distribution = lb_result.host_distribution
  assert_true(host_distribution.length() > 1)
  
  for (host, count) in host_distribution {
    assert_true(count > 0)
    assert_true(load_balancing_config.hosts.contains(host))
  }
  
  // Release some connections to test pool shrinking
  let release_connections = normal_connections.slice(0, 25)
  azimuth::ConnectionPoolManager::release_connections(pool_manager, release_connections)
  
  // Wait for potential scale down
  azimuth::Thread::sleep(1000)
  
  let final_pool_status = azimuth::ConnectionPoolManager::get_pool_status(pool_manager)
  assert_true(final_pool_status.healthy)
  assert_true(final_pool_status.active_connections <= final_pool_status.max_connections)
}

// Test 6: File Descriptor Limit and Recovery
test "file descriptor limits and recovery mechanisms" {
  // Create file descriptor manager
  let fd_manager = azimuth::FileDescriptorManager::new()
  
  // Configure file descriptor limits
  azimuth::FileDescriptorManager::configure(fd_manager, {
    "max_file_descriptors": 1000,
    "warning_threshold": 800,
    "critical_threshold": 950,
    "auto_cleanup": true,
    "monitoring_interval_ms": 5000
  })
  
  // Test normal file descriptor usage
  let normal_files = []
  for i in 0..100 {
    let file_handle = azimuth::FileHandle::new(
      path = "/telemetry/normal/file-" + i.to_string() + ".log",
      mode = "read",
      descriptor = i + 100 // Start from a reasonable descriptor number
    )
    normal_files.push(file_handle)
  }
  
  let fd_usage_before = azimuth::FileDescriptorManager::get_fd_usage(fd_manager)
  
  // Open normal files
  let normal_result = azimuth::FileDescriptorManager::open_files(fd_manager, normal_files)
  
  assert_true(normal_result.success)
  assert_eq(normal_result.opened_files, 100)
  assert_false(normal_result.cleanup_triggered)
  
  // Test file descriptor exhaustion
  let excessive_files = []
  for i in 0..1200 { // More than max_file_descriptors
    let file_handle = azimuth::FileHandle::new(
      path = "/telemetry/excessive/file-" + i.to_string() + ".data",
      mode = "write",
      descriptor = i + 200
    )
    excessive_files.push(file_handle)
  }
  
  // Try to open excessive files
  let excessive_result = azimuth::FileDescriptorManager::open_files(fd_manager, excessive_files)
  
  assert_true(excessive_result.success)
  
  // Should have hit file descriptor limit
  assert_true(excessive_result.limit_exceeded)
  assert_true(excessive_result.rejected_files > 0)
  
  // Test file descriptor leak detection
  let leak_detection = azimuth::FileDescriptorManager::detect_leaks(fd_manager)
  
  assert_true(leak_detection.scan_completed)
  
  if leak_detection.potential_leaks_detected {
    assert_true(leak_detection.leaked_descriptors.length() > 0)
    
    for leak in leak_detection.leaked_descriptors {
      assert_true(leak.descriptor >= 0)
      assert_true(leak.path.length() > 0)
      assert_true(leak.open_duration_ms > 0)
    }
  }
  
  // Test file descriptor cleanup
  let cleanup_result = azimuth::FileDescriptorManager::execute_cleanup(fd_manager)
  
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.descriptors_freed > 0)
  assert_true(cleanup_result.files_closed > 0)
  
  // Test file descriptor pooling
  let pool_config = {
    "enabled": true,
    "pool_size": 100,
    "max_idle_time_ms": 30000,
    "cleanup_interval_ms": 10000
  }
  
  azimuth::FileDescriptorManager::configure_pooling(fd_manager, pool_config)
  
  // Use file descriptor pool
  let pooled_files = []
  for i in 0..50 {
    let file_handle = azimuth::FileHandle::new(
      path = "/telemetry/pooled/file-" + i.to_string() + ".log",
      mode = "read",
      descriptor = -1 // Will be assigned from pool
    )
    pooled_files.push(file_handle)
  }
  
  let pool_result = azimuth::FileDescriptorManager::open_files(fd_manager, pooled_files)
  
  assert_true(pool_result.success)
  assert_true(pool_result.pool_used)
  assert_true(pool_result.pool_hit_rate > 0.5)
  
  // Return files to pool
  azimuth::FileDescriptorManager::close_files(fd_manager, pooled_files)
  
  // Test file descriptor monitoring
  azimuth::FileDescriptorManager::start_monitoring(fd_manager)
  
  // Generate file descriptor activity
  let monitoring_files = []
  for i in 0..200 {
    let file_handle = azimuth::FileHandle::new(
      path = "/telemetry/monitoring/file-" + i.to_string() + ".data",
      mode = "read",
      descriptor = i + 500
    )
    monitoring_files.push(file_handle)
  }
  
  azimuth::FileDescriptorManager::open_files(fd_manager, monitoring_files)
  
  // Wait for monitoring to collect data
  azimuth::Thread::sleep(6000)
  
  let monitoring_data = azimuth::FileDescriptorManager::get_monitoring_data(fd_manager)
  
  assert_true(monitoring_data.samples.length() > 0)
  assert_true(monitoring_data.average_fd_usage >= 0)
  
  if monitoring_data.alerts_triggered {
    assert_true(monitoring_data.alerts.length() > 0)
  }
  
  azimuth::FileDescriptorManager::stop_monitoring(fd_manager)
  
  // Test file descriptor priority management
  let priority_config = {
    "enabled": true,
    "priorities": {
      "critical": {"max_descriptors": 100, "preemption": true},
      "high": {"max_descriptors": 200, "preemption": false},
      "normal": {"max_descriptors": 500, "preemption": false},
      "low": {"max_descriptors": 200, "preemption": false}
    }
  }
  
  azimuth::FileDescriptorManager::configure_priorities(fd_manager, priority_config)
  
  // Create priority-based file operations
  let priority_files = []
  
  // Add critical files
  for i in 0..50 {
    let file_handle = azimuth::FileHandle::new(
      path = "/telemetry/critical/file-" + i.to_string() + ".log",
      mode = "read",
      descriptor = i + 1000,
      priority = "critical"
    )
    priority_files.push(file_handle)
  }
  
  // Add low priority files
  for i in 50..200 {
    let file_handle = azimuth::FileHandle::new(
      path = "/telemetry/low/file-" + i.to_string() + ".log",
      mode = "read",
      descriptor = i + 1000,
      priority = "low"
    )
    priority_files.push(file_handle)
  }
  
  let priority_result = azimuth::FileDescriptorManager::open_files(fd_manager, priority_files)
  
  assert_true(priority_result.success)
  
  // Critical files should be prioritized
  assert_true(priority_result.critical_files_opened >= 50)
  
  // Under descriptor pressure, low priority files might be rejected
  if priority_result.pressure_detected {
    assert_true(priority_result.low_priority_rejected >= priority_result.critical_files_rejected)
  }
  
  // Test file descriptor recovery
  let recovery_result = azimuth::FileDescriptorManager::execute_recovery(fd_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.descriptors_recovered > 0)
  assert_true(recovery_result.leaks_plugged > 0)
  
  // Final file descriptor check
  let final_fd_status = azimuth::FileDescriptorManager::get_fd_status(fd_manager)
  assert_true(final_fd_status.healthy)
  assert_true(final_fd_status.active_descriptors <= final_fd_status.max_descriptors)
}

// Test 7: Thread Pool Limit and Recovery
test "thread pool limits and recovery mechanisms" {
  // Create thread pool manager
  let thread_manager = azimuth::ThreadPoolManager::new()
  
  // Configure thread pool limits
  azimuth::ThreadPoolManager::configure(thread_manager, {
    "max_threads": 50,
    "min_threads": 5,
    "warning_threshold": 40,
    "critical_threshold": 48,
    "queue_size": 100,
    "thread_timeout_ms": 30000
  })
  
  // Test normal thread usage
  let normal_tasks = []
  for i in 0..20 {
    let task = azimuth::ThreadTask::new(
      id = "normal-task-" + i.to_string(),
      priority = "normal",
      estimated_duration_ms = 100,
      cpu_intensive = false
    )
    normal_tasks.push(task)
  }
  
  let thread_status_before = azimuth::ThreadPoolManager::get_thread_status(thread_manager)
  
  // Execute normal tasks
  let normal_result = azimuth::ThreadPoolManager::execute_tasks(thread_manager, normal_tasks)
  
  assert_true(normal_result.success)
  assert_eq(normal_result.executed_tasks, 20)
  assert_false(normal_result.thread_exhaustion)
  
  // Test thread pool exhaustion
  let excessive_tasks = []
  for i in 0..100 { // More than max_threads + queue_size
    let task = azimuth::ThreadTask::new(
      id = "excess-task-" + i.to_string(),
      priority = "normal",
      estimated_duration_ms = 5000, // Long running
      cpu_intensive = true
    )
    excessive_tasks.push(task)
  }
  
  // Try to execute excessive tasks
  let excessive_result = azimuth::ThreadPoolManager::execute_tasks(thread_manager, excessive_tasks)
  
  assert_true(excessive_result.success)
  
  // Should have hit thread pool limit
  assert_true(excessive_result.thread_exhaustion)
  assert_true(excessive_result.rejected_tasks > 0)
  assert_true(excessive_result.queue_full)
  
  // Test thread pool scaling
  let scaling_config = {
    "enabled": true,
    "scale_up_threshold": 0.8,
    "scale_down_threshold": 0.3,
    "scale_up_factor": 1.5,
    "scale_down_factor": 0.7,
    "min_scale_interval_ms": 10000
  }
  
  azimuth::ThreadPoolManager::configure_scaling(thread_manager, scaling_config)
  
  // Generate load to trigger scaling
  let scaling_tasks = []
  for i in 0..40 { // Approach scale_up_threshold
    let task = azimuth::ThreadTask::new(
      id = "scaling-task-" + i.to_string(),
      priority = "normal",
      estimated_duration_ms = 200,
      cpu_intensive = false
    )
    scaling_tasks.push(task)
  }
  
  let scaling_result = azimuth::ThreadPoolManager::execute_tasks(thread_manager, scaling_tasks)
  
  assert_true(scaling_result.success)
  
  if scaling_result.scaling_triggered {
    assert_true(scaling_result.scaled_up)
    assert_true(scaling_result.threads_added > 0)
  }
  
  // Test thread priority management
  let priority_config = {
    "enabled": true,
    "priorities": {
      "critical": {"max_threads": 10, "preemption": true},
      "high": {"max_threads": 15, "preemption": false},
      "normal": {"max_threads": 20, "preemption": false},
      "low": {"max_threads": 5, "preemption": false}
    }
  }
  
  azimuth::ThreadPoolManager::configure_priorities(thread_manager, priority_config)
  
  // Create priority-based tasks
  let priority_tasks = []
  
  // Add critical tasks
  for i in 0..8 {
    let task = azimuth::ThreadTask::new(
      id = "critical-task-" + i.to_string(),
      priority = "critical",
      estimated_duration_ms = 1000,
      cpu_intensive = true
    )
    priority_tasks.push(task)
  }
  
  // Add low priority tasks
  for i in 8..30 {
    let task = azimuth::ThreadTask::new(
      id = "low-task-" + i.to_string(),
      priority = "low",
      estimated_duration_ms = 500,
      cpu_intensive = false
    )
    priority_tasks.push(task)
  }
  
  let priority_result = azimuth::ThreadPoolManager::execute_tasks(thread_manager, priority_tasks)
  
  assert_true(priority_result.success)
  
  // Critical tasks should be prioritized
  assert_true(priority_result.critical_tasks_executed >= 8)
  
  // Under thread pressure, low priority tasks might be rejected
  if priority_result.pressure_detected {
    assert_true(priority_result.low_priority_rejected >= priority_result.critical_tasks_rejected)
  }
  
  // Test thread timeout handling
  let timeout_config = {
    "enabled": true,
    "task_timeout_ms": 5000,
    "thread_timeout_ms": 30000,
    "interrupt_on_timeout": true
  }
  
  azimuth::ThreadPoolManager::configure_timeouts(thread_manager, timeout_config)
  
  // Create tasks that will timeout
  let timeout_tasks = []
  for i in 0..10 {
    let task = azimuth::ThreadTask::new(
      id = "timeout-task-" + i.to_string(),
      priority = "normal",
      estimated_duration_ms = 10000, // Longer than timeout
      cpu_intensive = false
    )
    timeout_tasks.push(task)
  }
  
  let timeout_result = azimuth::ThreadPoolManager::execute_tasks(thread_manager, timeout_tasks)
  
  assert_true(timeout_result.success)
  
  if timeout_result.timeouts_occurred {
    assert_true(timeout_result.timed_out_tasks > 0)
    assert_true(timeout_result.threads_interrupted > 0)
  }
  
  // Test thread pool monitoring
  let monitoring_config = {
    "enabled": true,
    "metrics_interval_ms": 2000,
    "alert_threshold": 0.9
  }
  
  azimuth::ThreadPoolManager::configure_monitoring(thread_manager, monitoring_config)
  
  // Start monitoring
  azimuth::ThreadPoolManager::start_monitoring(thread_manager)
  
  // Generate thread activity for monitoring
  let monitoring_tasks = []
  for i in 0..30 {
    let task = azimuth::ThreadTask::new(
      id = "monitoring-task-" + i.to_string(),
      priority = "normal",
      estimated_duration_ms = 1000,
      cpu_intensive = false
    )
    monitoring_tasks.push(task)
  }
  
  azimuth::ThreadPoolManager::execute_tasks(thread_manager, monitoring_tasks)
  
  // Wait for monitoring to collect data
  azimuth::Thread::sleep(3000)
  
  let monitoring_data = azimuth::ThreadPoolManager::get_monitoring_data(thread_manager)
  
  assert_true(monitoring_data.samples.length() > 0)
  assert_true(monitoring_data.average_active_threads >= 0)
  
  if monitoring_data.alerts_triggered {
    assert_true(monitoring_data.alerts.length() > 0)
  }
  
  // Stop monitoring
  azimuth::ThreadPoolManager::stop_monitoring(thread_manager)
  
  // Test thread pool recovery
  let recovery_result = azimuth::ThreadPoolManager::execute_recovery(thread_manager)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.threads_recovered > 0)
  assert_true(recovery_result.hung_threads_terminated > 0)
  
  // Test thread pool work stealing
  let work_stealing_config = {
    "enabled": true,
    "stealing_threshold": 0.5,
    "max_steal_attempts": 3
  }
  
  azimuth::ThreadPoolManager::configure_work_stealing(thread_manager, work_stealing_config)
  
  // Create imbalanced workload
  let imbalanced_tasks = []
  for i in 0..40 {
    let task = azimuth::ThreadTask::new(
      id = "imbalanced-task-" + i.to_string(),
      priority = "normal",
      estimated_duration_ms = 1000,
      cpu_intensive = false
    )
    imbalanced_tasks.push(task)
  }
  
  let work_stealing_result = azimuth::ThreadPoolManager::execute_tasks(thread_manager, imbalanced_tasks)
  
  assert_true(work_stealing_result.success)
  
  if work_stealing_result.work_stealing_applied {
    assert_true(work_stealing_result.tasks_stolen > 0)
    assert_true(work_stealing_result.load_balanced)
  }
  
  // Final thread pool check
  let final_thread_status = azimuth::ThreadPoolManager::get_thread_status(thread_manager)
  assert_true(final_thread_status.healthy)
  assert_true(final_thread_status.active_threads <= final_thread_status.max_threads)
}

// Test 8: Resource Limit Coordination
test "resource limit coordination and global recovery" {
  // Create resource coordinator
  let resource_coordinator = azimuth::ResourceCoordinator::new()
  
  // Configure resource limits
  let resource_limits = {
    "memory": {"max_percent": 85, "warning_percent": 75, "critical_percent": 90},
    "cpu": {"max_percent": 80, "warning_percent": 70, "critical_percent": 90},
    "disk": {"max_percent": 85, "warning_percent": 75, "critical_percent": 90},
    "network": {"max_bandwidth_mbps": 100, "warning_mbps": 80, "critical_mbps": 95},
    "connections": {"max_count": 100, "warning_count": 80, "critical_count": 95},
    "file_descriptors": {"max_count": 1000, "warning_count": 800, "critical_count": 950},
    "threads": {"max_count": 50, "warning_count": 40, "critical_count": 48}
  }
  
  azimuth::ResourceCoordinator::configure_limits(resource_coordinator, resource_limits)
  
  // Test resource monitoring
  azimuth::ResourceCoordinator::start_monitoring(resource_coordinator)
  
  // Generate multi-resource pressure
  let memory_pressure = azimuth::MemoryLimitManager::new()
  let cpu_pressure = azimuth::CPULimitManager::new()
  let disk_pressure = azimuth::DiskLimitManager::new()
  
  // Create resource-intensive operations
  let intensive_operations = []
  
  for i in 0..50 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "intensive-op-" + i.to_string(),
      memory_mb = 10,
      cpu_ms = 200,
      disk_mb = 5,
      network_mbps = 2,
      connections = 2,
      file_descriptors = 5,
      threads = 1
    )
    intensive_operations.push(operation)
  }
  
  let coordination_result = azimuth::ResourceCoordinator::execute_operations(
    resource_coordinator, 
    intensive_operations
  )
  
  assert_true(coordination_result.success)
  
  // Check resource coordination
  let resource_status = azimuth::ResourceCoordinator::get_resource_status(resource_coordinator)
  
  assert_true(resource_status.overall_status == "healthy" || 
             resource_status.overall_status == "warning" || 
             resource_status.overall_status == "critical")
  
  // Test resource prioritization
  let prioritization_config = {
    "enabled": true,
    "priority_order": ["critical", "high", "normal", "low"],
    "resource_weights": {
      "memory": 0.3,
      "cpu": 0.25,
      "disk": 0.2,
      "network": 0.15,
      "connections": 0.05,
      "file_descriptors": 0.03,
      "threads": 0.02
    }
  }
  
  azimuth::ResourceCoordinator::configure_prioritization(resource_coordinator, prioritization_config)
  
  // Create operations with different priorities
  let priority_operations = []
  
  // Add critical operations
  for i in 0..10 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "critical-op-" + i.to_string(),
      priority = "critical",
      memory_mb = 20,
      cpu_ms = 500,
      disk_mb = 10,
      network_mbps = 5,
      connections = 3,
      file_descriptors = 10,
      threads = 2
    )
    priority_operations.push(operation)
  }
  
  // Add low priority operations
  for i in 10..40 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "low-op-" + i.to_string(),
      priority = "low",
      memory_mb = 5,
      cpu_ms = 100,
      disk_mb = 2,
      network_mbps = 1,
      connections = 1,
      file_descriptors = 2,
      threads = 1
    )
    priority_operations.push(operation)
  }
  
  let priority_result = azimuth::ResourceCoordinator::execute_operations(
    resource_coordinator, 
    priority_operations
  )
  
  assert_true(priority_result.success)
  
  // Critical operations should be prioritized
  assert_true(priority_result.critical_operations_executed >= 10)
  
  // Under resource pressure, low priority operations might be rejected
  if priority_result.pressure_detected {
    assert_true(priority_result.low_priority_rejected >= priority_result.critical_operations_rejected)
  }
  
  // Test resource escalation
  let escalation_config = {
    "enabled": true,
    "escalation_thresholds": {
      "warning": {"escalate_after_ms": 10000, "actions": ["throttle", "cleanup"]},
      "critical": {"escalate_after_ms": 5000, "actions": ["throttle", "cleanup", "reject"]}
    }
  }
  
  azimuth::ResourceCoordinator::configure_escalation(resource_coordinator, escalation_config)
  
  // Generate sustained resource pressure
  let sustained_operations = []
  for i in 0..100 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "sustained-op-" + i.to_string(),
      priority = "normal",
      memory_mb = 15,
      cpu_ms = 300,
      disk_mb = 8,
      network_mbps = 3,
      connections = 2,
      file_descriptors = 8,
      threads = 1
    )
    sustained_operations.push(operation)
  }
  
  let sustained_result = azimuth::ResourceCoordinator::execute_operations(
    resource_coordinator, 
    sustained_operations
  )
  
  assert_true(sustained_result.success)
  
  if sustained_result.escalation_triggered {
    assert_true(sustained_result.escalation_actions.length() > 0)
    assert_true(sustained_result.escalation_level == "warning" || 
               sustained_result.escalation_level == "critical")
  }
  
  // Test global resource recovery
  let recovery_result = azimuth::ResourceCoordinator::execute_global_recovery(resource_coordinator)
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.resources_recovered > 0)
  assert_true(recovery_result.system_stabilized)
  
  // Test resource prediction
  let prediction_config = {
    "enabled": true,
    "prediction_window_ms": 300000, // 5 minutes
    "confidence_threshold": 0.8
  }
  
  azimuth::ResourceCoordinator::configure_prediction(resource_coordinator, prediction_config)
  
  // Generate resource usage patterns for prediction
  let pattern_operations = []
  for i in 0..30 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "pattern-op-" + i.to_string(),
      priority = "normal",
      memory_mb = 8 + (i % 5) * 2, // Pattern: 8, 10, 12, 14, 16, repeat
      cpu_ms = 150 + (i % 4) * 50, // Pattern: 150, 200, 250, 300, repeat
      disk_mb = 4 + (i % 3) * 2, // Pattern: 4, 6, 8, repeat
      network_mbps = 1 + (i % 2), // Pattern: 1, 2, repeat
      connections = 1 + (i % 2),
      file_descriptors = 3 + (i % 3),
      threads = 1
    )
    pattern_operations.push(operation)
  }
  
  azimuth::ResourceCoordinator::execute_operations(resource_coordinator, pattern_operations)
  
  // Wait for prediction analysis
  azimuth::Thread::sleep(2000)
  
  let prediction_result = azimuth::ResourceCoordinator::predict_resource_usage(resource_coordinator)
  
  assert_true(prediction_result.success)
  
  if prediction_result.prediction_available {
    assert_true(prediction_result.confidence >= prediction_config.confidence_threshold)
    assert_true(prediction_result.predicted_resource_usage.length() > 0)
    
    for prediction in prediction_result.predicted_resource_usage {
      assert_true(prediction.resource_type.length() > 0)
      assert_true(prediction.predicted_usage >= 0)
      assert_true(prediction.predicted_limit >= 0)
      assert_true(prediction.risk_level == "low" || 
                 prediction.risk_level == "medium" || 
                 prediction.risk_level == "high")
    }
  }
  
  // Test resource quota management
  let quota_config = {
    "enabled": true,
    "quotas": {
      "telemetry": {
        "memory_mb": 200,
        "cpu_percent": 40,
        "disk_mb": 50,
        "network_mbps": 20,
        "connections": 20,
        "file_descriptors": 200,
        "threads": 10
      },
      "analytics": {
        "memory_mb": 300,
        "cpu_percent": 50,
        "disk_mb": 100,
        "network_mbps": 30,
        "connections": 30,
        "file_descriptors": 300,
        "threads": 15
      }
    }
  }
  
  azimuth::ResourceCoordinator::configure_quotas(resource_coordinator, quota_config)
  
  // Create quota-aware operations
  let quota_operations = []
  
  // Telemetry operations
  for i in 0..20 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "telemetry-op-" + i.to_string(),
      priority = "normal",
      quota = "telemetry",
      memory_mb = 5,
      cpu_ms = 100,
      disk_mb = 2,
      network_mbps = 1,
      connections = 1,
      file_descriptors = 5,
      threads = 1
    )
    quota_operations.push(operation)
  }
  
  // Analytics operations
  for i in 20..40 {
    let operation = azimuth::ResourceIntensiveOperation::new(
      id = "analytics-op-" + i.to_string(),
      priority = "normal",
      quota = "analytics",
      memory_mb = 10,
      cpu_ms = 200,
      disk_mb = 5,
      network_mbps = 2,
      connections = 2,
      file_descriptors = 10,
      threads = 1
    )
    quota_operations.push(operation)
  }
  
  let quota_result = azimuth::ResourceCoordinator::execute_operations(
    resource_coordinator, 
    quota_operations
  )
  
  assert_true(quota_result.success)
  
  if quota_result.quota_enforcement_applied {
    assert_true(quota_result.quota_exceeded_operations > 0)
    assert_true(quota_result.quota_throttled_operations > 0)
  }
  
  // Stop monitoring
  azimuth::ResourceCoordinator::stop_monitoring(resource_coordinator)
  
  // Get final resource report
  let final_report = azimuth::ResourceCoordinator::generate_resource_report(resource_coordinator)
  
  assert_true(final_report.success)
  assert_true(final_report.summary.length() > 0)
  assert_true(final_report.resource_utilization.length() > 0)
  assert_true(final_report.recommendations.length() > 0)
  
  // Verify all resources are within limits
  let final_status = azimuth::ResourceCoordinator::get_resource_status(resource_coordinator)
  assert_true(final_status.overall_status == "healthy" || 
             final_status.overall_status == "warning")
}

// Test 9: Resource Limit Policy Enforcement
test "resource limit policy enforcement and compliance" {
  // Create policy manager
  let policy_manager = azimuth::ResourcePolicyManager::new()
  
  // Define resource policies
  let resource_policies = [
    {
      "name": "telemetry_data_retention",
      "description": "Policy for telemetry data retention and cleanup",
      "resource_type": "disk",
      "rules": [
        {
          "condition": "usage_percent > 75",
          "action": "cleanup",
          "parameters": {
            "retention_days": 7,
            "categories": ["logs", "temp"]
          },
          "priority": "high"
        },
        {
          "condition": "usage_percent > 90",
          "action": "emergency_cleanup",
          "parameters": {
            "retention_days": 1,
            "categories": ["logs", "temp", "cache"]
          },
          "priority": "critical"
        }
      ],
      "enabled": true
    },
    {
      "name": "memory_pressure_response",
      "description": "Policy for memory pressure handling",
      "resource_type": "memory",
      "rules": [
        {
          "condition": "usage_percent > 80",
          "action": "throttle",
          "parameters": {
            "throttle_rate": 0.5,
            "affected_operations": ["data_processing", "analytics"]
          },
          "priority": "high"
        },
        {
          "condition": "usage_percent > 90",
          "action": "emergency_gc",
          "parameters": {
            "force_collection": true,
            "clear_caches": true
          },
          "priority": "critical"
        }
      ],
      "enabled": true
    },
    {
      "name": "cpu_usage_management",
      "description": "Policy for CPU usage management",
      "resource_type": "cpu",
      "rules": [
        {
          "condition": "usage_percent > 75",
          "action": "throttle",
          "parameters": {
            "throttle_rate": 0.3,
            "affected_priorities": ["low", "normal"]
          },
          "priority": "medium"
        },
        {
          "condition": "usage_percent > 90",
          "action": "task_prioritization",
          "parameters": {
            "priority_order": ["critical", "high"],
            "reject_below": "normal"
          },
          "priority": "high"
        }
      ],
      "enabled": true
    }
  ]
  
  // Register policies
  for policy in resource_policies {
    azimuth::ResourcePolicyManager::register_policy(policy_manager, policy)
  }
  
  // Test policy evaluation
  let resource_states = [
    {
      "resource_type": "disk",
      "usage_percent": 78,
      "available_mb": 22000,
      "total_mb": 100000
    },
    {
      "resource_type": "memory",
      "usage_percent": 85,
      "available_mb": 750,
      "total_mb": 5000
    },
    {
      "resource_type": "cpu",
      "usage_percent": 92,
      "available_cores": 0.8,
      "total_cores": 10
    }
  ]
  
  let evaluation_result = azimuth::ResourcePolicyManager::evaluate_policies(
    policy_manager, 
    resource_states
  )
  
  assert_true(evaluation_result.success)
  assert_true(evaluation_result.triggered_policies.length() > 0)
  
  // Verify triggered policies
  assert_true(evaluation_result.triggered_policies.contains("telemetry_data_retention"))
  assert_true(evaluation_result.triggered_policies.contains("memory_pressure_response"))
  assert_true(evaluation_result.triggered_policies.contains("cpu_usage_management"))
  
  // Test policy execution
  let execution_result = azimuth::ResourcePolicyManager::execute_policies(
    policy_manager, 
    evaluation_result.triggered_policies
  )
  
  assert_true(execution_result.success)
  assert_true(execution_result.executed_actions.length() > 0)
  
  // Verify specific actions were executed
  assert_true(execution_result.executed_actions.contains("cleanup"))
  assert_true(execution_result.executed_actions.contains("throttle"))
  assert_true(execution_result.executed_actions.contains("task_prioritization"))
  
  // Test policy compliance monitoring
  let compliance_config = {
    "enabled": true,
    "monitoring_interval_ms": 10000,
    "compliance_threshold": 0.95,
    "violation_reporting": true
  }
  
  azimuth::ResourcePolicyManager::configure_compliance(policy_manager, compliance_config)
  
  // Start compliance monitoring
  azimuth::ResourcePolicyManager::start_compliance_monitoring(policy_manager)
  
  // Generate resource states that might violate policies
  let violation_states = [
    {
      "resource_type": "disk",
      "usage_percent": 95,
      "available_mb": 5000,
      "total_mb": 100000
    },
    {
      "resource_type": "memory",
      "usage_percent": 98,
      "available_mb": 100,
      "total_mb": 5000
    }
  ]
  
  let violation_evaluation = azimuth::ResourcePolicyManager::evaluate_policies(
    policy_manager, 
    violation_states
  )
  
  assert_true(violation_evaluation.success)
  assert_true(violation_evaluation.triggered_policies.length() > 0)
  
  // Wait for compliance monitoring
  azimuth::Thread::sleep(2000)
  
  let compliance_report = azimuth::ResourcePolicyManager::get_compliance_report(policy_manager)
  
  assert_true(compliance_report.generated)
  assert_true(compliance_report.overall_compliance_score <= compliance_config.compliance_threshold)
  assert_true(compliance_report.violations.length() > 0)
  
  // Stop compliance monitoring
  azimuth::ResourcePolicyManager::stop_compliance_monitoring(policy_manager)
  
  // Test policy conflict resolution
  let conflict_policies = [
    {
      "name": "aggressive_cleanup",
      "resource_type": "disk",
      "condition": "usage_percent > 70",
      "action": "cleanup",
      "parameters": {"retention_days": 1},
      "priority": "high"
    },
    {
      "name": "conservative_cleanup",
      "resource_type": "disk",
      "condition": "usage_percent > 80",
      "action": "cleanup",
      "parameters": {"retention_days": 7},
      "priority": "medium"
    }
  ]
  
  // Register conflicting policies
  for policy in conflict_policies {
    azimuth::ResourcePolicyManager::register_policy(policy_manager, policy)
  }
  
  let conflict_states = [
    {
      "resource_type": "disk",
      "usage_percent": 85,
      "available_mb": 15000,
      "total_mb": 100000
    }
  ]
  
  let conflict_evaluation = azimuth::ResourcePolicyManager::evaluate_policies(
    policy_manager, 
    conflict_states
  )
  
  assert_true(conflict_evaluation.success)
  
  // Should detect conflicts
  if conflict_evaluation.conflicts_detected {
    assert_true(conflict_evaluation.conflicts.length() > 0)
    
    // Resolve conflicts
    let resolution_result = azimuth::ResourcePolicyManager::resolve_conflicts(policy_manager)
    
    assert_true(resolution_result.success)
    assert_true(resolution_result.resolved_conflicts > 0)
    assert_true(resolution_result.resolution_strategy.length() > 0)
  }
  
  // Test policy adaptation
  let adaptation_config = {
    "enabled": true,
    "learning_period_days": 7,
    "adaptation_threshold": 0.1,
    "min_confidence": 0.8
  }
  
  azimuth::ResourcePolicyManager::configure_adaptation(policy_manager, adaptation_config)
  
  // Generate historical policy execution data
  let historical_data = []
  for i in 0..100 {
    let data_point = {
      "timestamp": azimuth::Time::now() - i * 3600000, // 1 hour ago per point
      "resource_type": "memory",
      "usage_percent": 75 + (i % 20),
      "policy_triggered": i % 3 == 0, // Trigger policy every 3rd point
      "policy_effectiveness": if i % 3 == 0 { 0.8 + (i % 3) * 0.1 } else { 0.0 }
    }
    historical_data.push(data_point)
  }
  
  azimuth::ResourcePolicyManager::feed_historical_data(policy_manager, historical_data)
  
  // Execute policy adaptation
  let adaptation_result = azimuth::ResourcePolicyManager::execute_adaptation(policy_manager)
  
  assert_true(adaptation_result.success)
  
  if adaptation_result.adaptation_applied {
    assert_true(adaptation_result.adapted_policies.length() > 0)
    assert_true(adaptation_result.improvement_confidence >= adaptation_config.min_confidence)
  }
  
  // Test policy audit trail
  let audit_config = {
    "enabled": true,
    "retention_days": 30,
    "include_context": true
  }
  
  azimuth::ResourcePolicyManager::configure_audit(policy_manager, audit_config)
  
  // Generate policy execution events
  let audit_events = []
  for i in 0..20 {
    let event = {
      "timestamp": azimuth::Time::now() - i * 60000, // 1 minute ago per event
      "policy_name": "memory_pressure_response",
      "action": "throttle",
      "resource_state": {"usage_percent": 85 + i % 10},
      "execution_result": "success",
      "context": {
        "trigger": "automated",
        "operator": "system",
        "request_id": "req-" + i.to_string()
      }
    }
    audit_events.push(event)
  }
  
  azimuth::ResourcePolicyManager::record_audit_events(policy_manager, audit_events)
  
  // Get audit trail
  let audit_trail = azimuth::ResourcePolicyManager::get_audit_trail(policy_manager)
  
  assert_true(audit_trail.events.length() >= 20)
  
  for event in audit_trail.events {
    assert_true(event.timestamp > 0)
    assert_true(event.policy_name.length() > 0)
    assert_true(event.action.length() > 0)
    assert_true(event.context.contains("trigger"))
  }
  
  // Test policy reporting
  let report_result = azimuth::ResourcePolicyManager::generate_policy_report(policy_manager)
  
  assert_true(report_result.success)
  assert_true(report_result.summary.length() > 0)
  assert_true(report_result.policy_effectiveness.length() > 0)
  assert_true(report_result.recommendations.length() > 0)
  
  // Verify report contains expected sections
  assert_true(report_result.summary.contains("total_policies"))
  assert_true(report_result.summary.contains("active_policies"))
  assert_true(report_result.summary.contains("compliance_rate"))
}

// Test 10: Resource Limit Emergency Response
test "resource limit emergency response and system protection" {
  // Create emergency response manager
  let emergency_manager = azimuth::EmergencyResponseManager::new()
  
  // Configure emergency response
  azimuth::EmergencyResponseManager::configure(emergency_manager, {
    "emergency_thresholds": {
      "memory": 95,
      "cpu": 95,
      "disk": 95,
      "network": 98
    },
    "response_actions": [
      "throttle_all",
      "clear_caches",
      "force_gc",
      "emergency_cleanup",
      "reject_non_critical",
      "alert_administrators"
    ],
    "auto_recovery": true,
    "recovery_timeout_ms": 30000
  })
  
  // Test emergency detection
  let emergency_scenarios = [
    {
      "name": "memory_emergency",
      "resource_states": [
        {
          "resource_type": "memory",
          "usage_percent": 98,
          "available_mb": 100,
          "total_mb": 5000
        }
      ],
      "expected_emergency": true
    },
    {
      "name": "cpu_emergency",
      "resource_states": [
        {
          "resource_type": "cpu",
          "usage_percent": 97,
          "available_cores": 0.3,
          "total_cores": 10
        }
      ],
      "expected_emergency": true
    },
    {
      "name": "disk_emergency",
      "resource_states": [
        {
          "resource_type": "disk",
          "usage_percent": 96,
          "available_mb": 4000,
          "total_mb": 100000
        }
      ],
      "expected_emergency": true
    },
    {
      "name": "multi_resource_emergency",
      "resource_states": [
        {
          "resource_type": "memory",
          "usage_percent": 94
        },
        {
          "resource_type": "cpu",
          "usage_percent": 93
        },
        {
          "resource_type": "disk",
          "usage_percent": 92
        }
      ],
      "expected_emergency": true
    },
    {
      "name": "normal_state",
      "resource_states": [
        {
          "resource_type": "memory",
          "usage_percent": 70
        },
        {
          "resource_type": "cpu",
          "usage_percent": 60
        },
        {
          "resource_type": "disk",
          "usage_percent": 65
        }
      ],
      "expected_emergency": false
    }
  ]
  
  for scenario in emergency_scenarios {
    let detection_result = azimuth::EmergencyResponseManager::detect_emergency(
      emergency_manager, 
      scenario.resource_states
    )
    
    assert_eq(detection_result.emergency_detected, scenario.expected_emergency)
    
    if scenario.expected_emergency {
      assert_true(detection_result.emergency_type.length() > 0)
      assert_true(detection_result.severity == "high" || detection_result.severity == "critical")
    }
  }
  
  // Test emergency response execution
  let emergency_states = [
    {
      "resource_type": "memory",
      "usage_percent": 97,
      "available_mb": 150,
      "total_mb": 5000
    },
    {
      "resource_type": "cpu",
      "usage_percent": 96,
      "available_cores": 0.4,
      "total_cores": 10
    }
  ]
  
  let emergency_detection = azimuth::EmergencyResponseManager::detect_emergency(
    emergency_manager, 
    emergency_states
  )
  
  assert_true(emergency_detection.emergency_detected)
  
  // Execute emergency response
  let response_result = azimuth::EmergencyResponseManager::execute_emergency_response(
    emergency_manager, 
    emergency_detection
  )
  
  assert_true(response_result.success)
  assert_true(response_result.actions_executed.length() > 0)
  
  // Verify emergency actions were executed
  assert_true(response_result.actions_executed.contains("throttle_all"))
  assert_true(response_result.actions_executed.contains("clear_caches"))
  assert_true(response_result.actions_executed.contains("force_gc"))
  
  // Test emergency monitoring
  azimuth::EmergencyResponseManager::start_emergency_monitoring(emergency_manager)
  
  // Simulate emergency conditions
  let emergency_simulation = azimuth::EmergencyResponseManager::simulate_emergency(
    emergency_manager, 
    {
      "resource_type": "memory",
      "usage_percent": 99,
      "duration_ms": 10000
    }
  )
  
  assert_true(emergency_simulation.initiated)
  
  // Wait for emergency response
  azimuth::Thread::sleep(2000)
  
  let emergency_status = azimuth::EmergencyResponseManager::get_emergency_status(emergency_manager)
  
  assert_true(emergency_status.emergency_active)
  assert_true(emergency_status.response_actions.length() > 0)
  
  // Test emergency recovery
  let recovery_result = azimuth::EmergencyResponseManager::execute_emergency_recovery(
    emergency_manager
  )
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.recovery_initiated)
  assert_true(recovery_result.system_stabilizing)
  
  // Wait for recovery
  azimuth::Thread::sleep(2000)
  
  let post_recovery_status = azimuth::EmergencyResponseManager::get_emergency_status(emergency_manager)
  
  assert_true(post_recovery_status.emergency_resolved || post_recovery_status.emergency_resolving)
  
  azimuth::EmergencyResponseManager::stop_emergency_monitoring(emergency_manager)
  
  // Test emergency alerting
  let alerting_config = {
    "enabled": true,
    "channels": ["email", "sms", "slack"],
    "escalation_rules": [
      {
        "condition": "severity == 'critical'",
        "escalate_after_minutes": 5,
        "escalate_to": ["on_call_engineer", "manager"]
      }
    ],
    "templates": {
      "memory_emergency": "Memory usage at {usage_percent}% - Immediate action required",
      "cpu_emergency": "CPU usage at {usage_percent}% - System overload detected"
    }
  }
  
  azimuth::EmergencyResponseManager::configure_alerting(emergency_manager, alerting_config)
  
  // Test alert generation
  let alert_result = azimuth::EmergencyResponseManager::generate_emergency_alerts(
    emergency_manager, 
    emergency_detection
  )
  
  assert_true(alert_result.success)
  assert_true(alert_result.alerts_generated.length() > 0)
  
  // Verify alert content
  for alert in alert_result.alerts_generated {
    assert_true(alert.channel.length() > 0)
    assert_true(alert.message.length() > 0)
    assert_true(alert.severity == "high" || alert.severity == "critical")
    assert_true(alert.timestamp > 0)
  }
  
  // Test emergency policy override
  let override_config = {
    "enabled": true,
    "override_policies": [
      "throttle_all_operations",
      "reject_all_non_critical",
      "clear_all_caches",
      "force_maximum_cleanup"
    ],
    "override_duration_ms": 60000
  }
  
  azimuth::EmergencyResponseManager::configure_policy_override(emergency_manager, override_config)
  
  let override_result = azimuth::EmergencyResponseManager::apply_policy_override(
    emergency_manager, 
    emergency_detection
  )
  
  assert_true(override_result.success)
  assert_true(override_result.policies_overridden.length() > 0)
  
  // Test emergency rollback
  let rollback_result = azimuth::EmergencyResponseManager::execute_emergency_rollback(
    emergency_manager
  )
  
  assert_true(rollback_result.success)
  assert_true(rollback_result.rollback_initiated)
  assert_true(rollback_result.normal_operations_resumed)
  
  // Test emergency metrics collection
  let metrics_config = {
    "enabled": true,
    "collect_detailed_metrics": true,
    "retention_days": 30
  }
  
  azimuth::EmergencyResponseManager::configure_metrics(emergency_manager, metrics_config)
  
  // Generate emergency metrics
  let metrics_result = azimuth::EmergencyResponseManager::collect_emergency_metrics(
    emergency_manager
  )
  
  assert_true(metrics_result.success)
  assert_true(metrics_result.metrics.length() > 0)
  
  // Verify metrics contain expected data
  for metric in metrics_result.metrics {
    assert_true(metric.name.length() > 0)
    assert_true(metric.value >= 0)
    assert_true(metric.unit.length() > 0)
    assert_true(metric.timestamp > 0)
  }
  
  // Test emergency post-mortem analysis
  let post_mortem_result = azimuth::EmergencyResponseManager::generate_post_mortem(
    emergency_manager
  )
  
  assert_true(post_mortem_result.success)
  assert_true(post_mortem_result.analysis.length() > 0)
  assert_true(post_mortem_result.timeline.length() > 0)
  assert_true(post_mortem_result.recommendations.length() > 0)
  
  // Verify post-mortem content
  assert_true(post_mortem_result.analysis.contains("root_cause"))
  assert_true(post_mortem_result.analysis.contains("impact_assessment"))
  assert_true(post_mortem_result.analysis.contains("lessons_learned"))
  
  // Test emergency drill simulation
  let drill_config = {
    "enabled": true,
    "scenarios": [
      {
        "name": "memory_exhaustion",
        "resource_type": "memory",
        "target_usage_percent": 98,
        "duration_ms": 15000
      },
      {
        "name": "cpu_overload",
        "resource_type": "cpu",
        "target_usage_percent": 97,
        "duration_ms": 10000
      }
    ],
    "auto_recovery": true,
    "evaluate_performance": true
  }
  
  azimuth::EmergencyResponseManager::configure_drills(emergency_manager, drill_config)
  
  let drill_result = azimuth::EmergencyResponseManager::execute_emergency_drill(
    emergency_manager, 
    "memory_exhaustion"
  )
  
  assert_true(drill_result.success)
  assert_true(drill_result.drill_completed)
  assert_true(drill_result.performance_metrics.length() > 0)
  
  // Verify drill performance
  for metric in drill_result.performance_metrics {
    assert_true(metric.metric_name.length() > 0)
    assert_true(metric.value >= 0)
    assert_true(metric.benchmark > 0)
  }
  
  // Final emergency system check
  let final_emergency_status = azimuth::EmergencyResponseManager::get_system_status(emergency_manager)
  
  assert_true(final_emergency_status.healthy)
  assert_true(final_emergency_status.emergency_systems_operational)
  assert_true(final_emergency_status.last_drill_success)
}