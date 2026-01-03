// Azimuth 错误处理和恢复测试
// 专注于遥测系统中的错误处理机制和自动恢复能力

// 测试1: 网连接断开自动恢复
test "网络连接断开自动恢复" {
  // 模拟网络连接状态变化
  let connection_events = [
    { timestamp: 1640995200000, event_type: "connection_established", status: "connected" },
    { timestamp: 1640995201000, event_type: "data_sent", data_size: 1024 },
    { timestamp: 1640995202000, event_type: "connection_lost", status: "disconnected", error: "Network timeout" },
    { timestamp: 1640995203000, event_type: "reconnect_attempt", attempt: 1, result: "failed" },
    { timestamp: 1640995204000, event_type: "reconnect_attempt", attempt: 2, result: "failed" },
    { timestamp: 1640995205000, event_type: "reconnect_attempt", attempt: 3, result: "success" },
    { timestamp: 1640995206000, event_type: "data_sent", data_size: 2048 },
    { timestamp: 1640995207000, event_type: "data_received", data_size: 512 }
  ]
  
  // 分析网络连接恢复过程
  let mut connection_state = "disconnected"
  let mut reconnect_attempts = 0
  let mut max_attempts = 0
  let mut recovery_time = 0
  let mut initial_disconnect_time = 0
  let mut final_reconnect_time = 0
  
  for event in connection_events {
    match event.event_type {
      "connection_lost" => {
        connection_state = "disconnected"
        initial_disconnect_time = event.timestamp
      }
      "reconnect_attempt" => {
        reconnect_attempts = reconnect_attempts + 1
        max_attempts = max_attempts + 1
        if event.result == "success" {
          connection_state = "connected"
          final_reconnect_time = event.timestamp
          recovery_time = final_reconnect_time - initial_disconnect_time
        }
      }
      "connection_established" => {
        connection_state = "connected"
      }
      _ => ()
    }
  }
  
  // 验证网络恢复过程
  assert_eq(connection_state, "connected")
  assert_eq(reconnect_attempts, 3)
  assert_eq(max_attempts, 3)
  assert_true(recovery_time > 0)
  assert_eq(recovery_time, 3000) // 5000 - 2000 = 3000ms
  
  // 验证重试策略（指数退避）
  let retry_intervals = [1000, 1000, 1000] // 简化的重试间隔
  let mut expected_intervals = []
  let mut base_interval = 1000
  for i in 1..max_attempts + 1 {
    expected_intervals = expected_intervals.push(base_interval * i)
  }
  
  // 验证恢复后的数据传输
  let mut data_sent_after_recovery = 0
  let mut data_received_after_recovery = 0
  
  for event in connection_events {
    if event.timestamp >= final_reconnect_time {
      match event.event_type {
        "data_sent" => data_sent_after_recovery = data_sent_after_recovery + event.data_size
        "data_received" => data_received_after_recovery = data_received_after_recovery + event.data_size
        _ => ()
      }
    }
  }
  
  assert_eq(data_sent_after_recovery, 2048)
  assert_eq(data_received_after_recovery, 512)
}

// 测试2: 数据处理管道错误恢复
test "数据处理管道错误恢复" {
  // 模拟数据处理管道的错误和恢复
  let pipeline_stages = [
    {
      stage_name: "data_ingestion",
      status: "success",
      input_count: 1000,
      output_count: 1000,
      error_count: 0,
      processing_time: 1200
    },
    {
      stage_name: "data_validation",
      status: "partial_failure",
      input_count: 1000,
      output_count: 950,
      error_count: 50,
      processing_time: 1800,
      errors: [
        { error_type: "validation_error", count: 30, recovery_action: "drop_invalid" },
        { error_type: "format_error", count: 20, recovery_action: "reformat" }
      ]
    },
    {
      stage_name: "data_transformation",
      status: "success",
      input_count: 950,
      output_count: 950,
      error_count: 0,
      processing_time: 2400
    },
    {
      stage_name: "data_aggregation",
      status: "failure",
      input_count: 950,
      output_count: 0,
      error_count: 950,
      processing_time: 500,
      errors: [
        { error_type: "memory_overflow", count: 950, recovery_action: "retry_with_batching" }
      ]
    },
    {
      stage_name: "data_aggregation", // 重试后的聚合阶段
      status: "success",
      input_count: 950,
      output_count: 950,
      error_count: 0,
      processing_time: 3200,
      retry_attempt: 1,
      batch_size: 100
    }
  ]
  
  // 分析管道错误恢复
  let mut total_input = 0
  let mut total_output = 0
  let mut total_errors = 0
  let mut failed_stages = 0
  let mut recovered_stages = 0
  
  for stage in pipeline_stages {
    total_input = total_input + stage.input_count
    total_output = total_output + stage.output_count
    total_errors = total_errors + stage.error_count
    
    match stage.status {
      "failure" => failed_stages = failed_stages + 1
      "success" => {
        // 检查是否是重试成功的阶段
        if stage.retry_attempt > 0 {
          recovered_stages = recovered_stages + 1
        }
      }
      _ => ()
    }
  }
  
  // 验证管道整体处理结果
  assert_eq(total_input, 4800) // 1000 + 1000 + 950 + 950 + 950
  assert_eq(total_output, 3850) // 1000 + 950 + 950 + 950
  assert_eq(total_errors, 1000) // 0 + 50 + 0 + 950 + 0
  assert_eq(failed_stages, 1)
  assert_eq(recovered_stages, 1)
  
  // 验证最终数据完整性
  let final_stage = pipeline_stages[pipeline_stages.length() - 1]
  assert_eq(final_stage.stage_name, "data_aggregation")
  assert_eq(final_stage.status, "success")
  assert_eq(final_stage.input_count, 950)
  assert_eq(final_stage.output_count, 950)
  assert_eq(final_stage.error_count, 0)
  assert_eq(final_stage.retry_attempt, 1)
  assert_eq(final_stage.batch_size, 100)
  
  // 验证错误恢复策略
  let validation_stage = pipeline_stages[1]
  assert_eq(validation_stage.status, "partial_failure")
  assert_eq(validation_stage.errors.length(), 2)
  
  let validation_error = validation_stage.errors[0]
  assert_eq(validation_error.error_type, "validation_error")
  assert_eq(validation_error.count, 30)
  assert_eq(validation_error.recovery_action, "drop_invalid")
  
  let format_error = validation_stage.errors[1]
  assert_eq(format_error.error_type, "format_error")
  assert_eq(format_error.count, 20)
  assert_eq(format_error.recovery_action, "reformat")
  
  // 验证数据丢失率
  let initial_data_count = 1000
  let final_data_count = final_stage.output_count
  let data_loss_rate = (initial_data_count - final_data_count).to_float() / initial_data_count.to_float() * 100.0
  assert_eq(data_loss_rate, 5.0) // (1000 - 950) / 1000 * 100 = 5%
}

// 测试3: 存储系统故障恢复
test "存储系统故障恢复" {
  // 模拟存储系统故障和恢复过程
  let storage_events = [
    { timestamp: 1640995200000, event: "write_operation", status: "success", data_size: 1024 },
    { timestamp: 1640995201000, event: "write_operation", status: "success", data_size: 2048 },
    { timestamp: 1640995202000, event: "write_operation", status: "failure", error: "disk_full", data_size: 4096 },
    { timestamp: 1640995203000, event: "cleanup_initiated", freed_space: 8192 },
    { timestamp: 1640995204000, event: "write_operation", status: "success", data_size: 4096 },
    { timestamp: 1640995205000, event: "read_operation", status: "success", data_size: 1024 },
    { timestamp: 1640995206000, event: "read_operation", status: "failure", error: "corruption_detected", data_size: 2048 },
    { timestamp: 1640995207000, event: "data_restoration", restored_from_backup: true, data_size: 2048 },
    { timestamp: 1640995208000, event: "read_operation", status: "success", data_size: 2048 }
  ]
  
  // 分析存储系统恢复过程
  let mut write_operations = []
  let mut read_operations = []
  let mut failures = []
  let mut recovery_actions = []
  
  for event in storage_events {
    match event.event {
      "write_operation" => write_operations = write_operations.push(event)
      "read_operation" => read_operations = read_operations.push(event)
      _ => {
        if event.event.starts_with("cleanup") || event.event.starts_with("data_restoration") {
          recovery_actions = recovery_actions.push(event)
        }
      }
    }
    
    if event.status == "failure" {
      failures = failures.push(event)
    }
  }
  
  // 验证操作统计
  assert_eq(write_operations.length(), 3)
  assert_eq(read_operations.length(), 3)
  assert_eq(failures.length(), 2)
  assert_eq(recovery_actions.length(), 2)
  
  // 验证写操作恢复
  let mut successful_writes = 0
  let mut failed_writes = 0
  let mut total_write_data = 0
  
  for write in write_operations {
    match write.status {
      "success" => {
        successful_writes = successful_writes + 1
        total_write_data = total_write_data + write.data_size
      }
      "failure" => failed_writes = failed_writes + 1
      _ => ()
    }
  }
  
  assert_eq(successful_writes, 2)
  assert_eq(failed_writes, 1)
  assert_eq(total_write_data, 7168) // 1024 + 2048 + 4096
  
  // 验证读操作恢复
  let mut successful_reads = 0
  let mut failed_reads = 0
  let mut total_read_data = 0
  
  for read in read_operations {
    match read.status {
      "success" => {
        successful_reads = successful_reads + 1
        total_read_data = total_read_data + read.data_size
      }
      "failure" => failed_reads = failed_reads + 1
      _ => ()
    }
  }
  
  assert_eq(successful_reads, 2)
  assert_eq(failed_reads, 1)
  assert_eq(total_read_data, 3072) // 1024 + 2048
  
  // 验证故障类型和恢复策略
  let disk_full_failure = failures[0]
  assert_eq(disk_full_failure.event, "write_operation")
  assert_eq(disk_full_failure.error, "disk_full")
  
  let corruption_failure = failures[1]
  assert_eq(corruption_failure.event, "read_operation")
  assert_eq(corruption_failure.error, "corruption_detected")
  
  // 验证恢复操作
  let cleanup_recovery = recovery_actions[0]
  assert_eq(cleanup_recovery.event, "cleanup_initiated")
  assert_eq(cleanup_recovery.freed_space, 8192)
  
  let backup_recovery = recovery_actions[1]
  assert_eq(backup_recovery.event, "data_restoration")
  assert_eq(backup_recovery.restored_from_backup, true)
  assert_eq(backup_recovery.data_size, 2048)
  
  // 验证恢复后的操作成功率
  let post_failure_writes = []
  let post_failure_reads = []
  
  for write in write_operations {
    if write.timestamp > disk_full_failure.timestamp {
      post_failure_writes = post_failure_writes.push(write)
    }
  }
  
  for read in read_operations {
    if read.timestamp > corruption_failure.timestamp {
      post_failure_reads = post_failure_reads.push(read)
    }
  }
  
  assert_eq(post_failure_writes.length(), 1)
  assert_eq(post_failure_reads.length(), 1)
  assert_eq(post_failure_writes[0].status, "success")
  assert_eq(post_failure_reads[0].status, "success")
}

// 测试4: 服务降级和熔断机制
test "服务降级和熔断机制" {
  // 模拟服务降级和熔断过程
  let service_states = [
    { timestamp: 1640995200000, service: "user-service", state: "normal", error_rate: 0.01, response_time: 120 },
    { timestamp: 1640995201000, service: "user-service", state: "normal", error_rate: 0.02, response_time: 130 },
    { timestamp: 1640995202000, service: "user-service", state: "degraded", error_rate: 0.05, response_time: 200 },
    { timestamp: 1640995203000, service: "user-service", state: "degraded", error_rate: 0.08, response_time: 250 },
    { timestamp: 1640995204000, service: "user-service", state: "circuit_open", error_rate: 0.15, response_time: 0 }, // 熔断打开
    { timestamp: 1640995205000, service: "user-service", state: "circuit_open", error_rate: 0.0, response_time: 0 }, // 快速失败
    { timestamp: 1640995206000, service: "user-service", state: "circuit_half_open", error_rate: 0.0, response_time: 150 }, // 半开状态
    { timestamp: 1640995207000, service: "user-service", state: "normal", error_rate: 0.01, response_time: 110 } // 恢复正常
  ]
  
  // 分析服务状态变化
  let mut state_transitions = []
  let mut normal_periods = []
  let mut degraded_periods = []
  let mut circuit_open_periods = []
  let mut circuit_half_open_periods = []
  
  for i in 0..service_states.length() {
    let state = service_states[i]
    match state.state {
      "normal" => normal_periods = normal_periods.push(state)
      "degraded" => degraded_periods = degraded_periods.push(state)
      "circuit_open" => circuit_open_periods = circuit_open_periods.push(state)
      "circuit_half_open" => circuit_half_open_periods = circuit_half_open_periods.push(state)
      _ => ()
    }
    
    if i > 0 {
      let prev_state = service_states[i-1]
      if prev_state.state != state.state {
        state_transitions = state_transitions.push({
          from: prev_state.state,
          to: state.state,
          timestamp: state.timestamp,
          error_rate: state.error_rate,
          response_time: state.response_time
        })
      }
    }
  }
  
  // 验证状态转换
  assert_eq(state_transitions.length(), 4)
  
  // 验证状态转换序列
  assert_eq(state_transitions[0].from, "normal")
  assert_eq(state_transitions[0].to, "degraded")
  
  assert_eq(state_transitions[1].from, "degraded")
  assert_eq(state_transitions[1].to, "circuit_open")
  
  assert_eq(state_transitions[2].from, "circuit_open")
  assert_eq(state_transitions[2].to, "circuit_half_open")
  
  assert_eq(state_transitions[3].from, "circuit_half_open")
  assert_eq(state_transitions[3].to, "normal")
  
  // 验证各状态持续时间
  assert_eq(normal_periods.length(), 2)
  assert_eq(degraded_periods.length(), 2)
  assert_eq(circuit_open_periods.length(), 2)
  assert_eq(circuit_half_open_periods.length(), 1)
  
  // 验证降级状态的错误率阈值
  for period in degraded_periods {
    assert_true(period.error_rate >= 0.05)
    assert_true(period.response_time >= 200)
  }
  
  // 验证熔断打开状态的行为
  for period in circuit_open_periods {
    assert_eq(period.state, "circuit_open")
    assert_true(period.response_time == 0) // 快速失败，不等待响应
  }
  
  // 验证半开状态的探测行为
  assert_eq(circuit_half_open_periods.length(), 1)
  let half_open_period = circuit_half_open_periods[0]
  assert_eq(half_open_period.state, "circuit_half_open")
  assert_eq(half_open_period.error_rate, 0.0)
  assert_true(half_open_period.response_time > 0)
  
  // 验证恢复后的状态
  let final_state = service_states[service_states.length() - 1]
  assert_eq(final_state.state, "normal")
  assert_true(final_state.error_rate < 0.05)
  assert_true(final_state.response_time < 150)
}

// 测试5: 数据备份和灾难恢复
test "数据备份和灾难恢复" {
  // 模拟数据备份和灾难恢复过程
  let backup_recovery_events = [
    { timestamp: 1640995200000, event: "backup_created", backup_id: "backup-001", data_size: 1048576, status: "success" },
    { timestamp: 1640995201000, event: "backup_created", backup_id: "backup-002", data_size: 1048576, status: "success" },
    { timestamp: 1640995202000, event: "backup_created", backup_id: "backup-003", data_size: 1048576, status: "success" },
    { timestamp: 1640995203000, event: "system_failure", failure_type: "data_corruption", affected_data: "primary_db" },
    { timestamp: 1640995204000, event: "recovery_initiated", recovery_type: "full_restore", target_backup: "backup-003" },
    { timestamp: 1640995205000, event: "data_restored", backup_id: "backup-003", restored_size: 1048576, integrity_check: "passed" },
    { timestamp: 1640995206000, event: "service_resumed", service: "telemetry_collection", status: "operational" },
    { timestamp: 1640995207000, event: "data_validation", validation_type: "consistency_check", result: "passed" }
  ]
  
  // 分析备份恢复过程
  let mut backup_events = []
  let mut failure_event = None
  let mut recovery_events = []
  let mut validation_events = []
  
  for event in backup_recovery_events {
    match event.event {
      "backup_created" => backup_events = backup_events.push(event)
      "system_failure" => failure_event = Some(event)
      _ => {
        if event.event.starts_with("recovery") || event.event.starts_with("data_restored") {
          recovery_events = recovery_events.push(event)
        } else if event.event.starts_with("data_validation") {
          validation_events = validation_events.push(event)
        }
      }
    }
  }
  
  // 验证备份事件
  assert_eq(backup_events.length(), 3)
  for backup in backup_events {
    assert_eq(backup.status, "success")
    assert_eq(backup.data_size, 1048576) // 1MB
    assert_true(backup.backup_id.starts_with("backup-"))
  }
  
  // 验证故障事件
  assert_true(failure_event.is_some())
  let failure = failure_event.unwrap()
  assert_eq(failure.event, "system_failure")
  assert_eq(failure.failure_type, "data_corruption")
  assert_eq(failure.affected_data, "primary_db")
  
  // 验证恢复事件
  assert_eq(recovery_events.length(), 2)
  let recovery_initiated = recovery_events[0]
  let data_restored = recovery_events[1]
  
  assert_eq(recovery_initiated.event, "recovery_initiated")
  assert_eq(recovery_initiated.recovery_type, "full_restore")
  assert_eq(recovery_initiated.target_backup, "backup-003")
  
  assert_eq(data_restored.event, "data_restored")
  assert_eq(data_restored.backup_id, "backup-003")
  assert_eq(data_restored.restored_size, 1048576)
  assert_eq(data_restored.integrity_check, "passed")
  
  // 验证恢复时间
  let recovery_start_time = recovery_initiated.timestamp
  let recovery_end_time = data_restored.timestamp
  let recovery_duration = recovery_end_time - recovery_start_time
  assert_eq(recovery_duration, 1000) // 1秒恢复时间
  
  // 验证数据完整性验证
  assert_eq(validation_events.length(), 1)
  let validation = validation_events[0]
  assert_eq(validation.event, "data_validation")
  assert_eq(validation.validation_type, "consistency_check")
  assert_eq(validation.result, "passed")
  
  // 验证服务恢复
  let service_resumed = backup_recovery_events[6]
  assert_eq(service_resumed.event, "service_resumed")
  assert_eq(service_resumed.service, "telemetry_collection")
  assert_eq(service_resumed.status, "operational")
  
  // 验证恢复策略（使用最新备份）
  let backup_ids = backup_events.map(fn(b) { b.backup_id })
  let latest_backup = backup_ids[backup_ids.length() - 1]
  assert_eq(latest_backup, "backup-003")
  assert_eq(recovery_initiated.target_backup, latest_backup)
  
  // 验证恢复时间线
  let failure_time = failure.timestamp
  let recovery_initiated_time = recovery_initiated.timestamp
  let service_resumed_time = service_resumed.timestamp
  
  assert_true(recovery_initiated_time > failure_time)
  assert_true(service_resumed_time > recovery_initiated_time)
  
  let total_downtime = service_resumed_time - failure_time
  assert_eq(total_downtime, 3000) // 3秒总停机时间
}

// 测试6: 自动故障检测和告警
test "自动故障检测和告警" {
  // 模拟自动故障检测和告警系统
  let monitoring_events = [
    { timestamp: 1640995200000, metric: "cpu_usage", value: 45.0, threshold: 80.0, status: "normal" },
    { timestamp: 1640995201000, metric: "memory_usage", value: 60.0, threshold: 85.0, status: "normal" },
    { timestamp: 1640995202000, metric: "cpu_usage", value: 85.0, threshold: 80.0, status: "critical" },
    { timestamp: 1640995202000, metric: "alert", alert_type: "threshold_exceeded", severity: "warning", message: "CPU usage exceeded threshold" },
    { timestamp: 1640995203000, metric: "disk_usage", value: 95.0, threshold: 90.0, status: "critical" },
    { timestamp: 1640995203000, metric: "alert", alert_type: "threshold_exceeded", severity: "critical", message: "Disk usage exceeded threshold" },
    { timestamp: 1640995204000, metric: "response_time", value: 2000.0, threshold: 1000.0, status: "critical" },
    { timestamp: 1640995204000, metric: "alert", alert_type: "threshold_exceeded", severity: "critical", message: "Response time exceeded threshold" },
    { timestamp: 1640995205000, metric: "cpu_usage", value: 75.0, threshold: 80.0, status: "normal" },
    { timestamp: 1640995205000, metric: "alert", alert_type: "threshold_normal", severity: "info", message: "CPU usage returned to normal" }
  ]
  
  // 分析监控事件和告警
  let mut metric_events = []
  let mut alert_events = []
  let mut critical_alerts = []
  let mut warning_alerts = []
  let mut info_alerts = []
  
  for event in monitoring_events {
    match event.metric {
      "alert" => {
        alert_events = alert_events.push(event)
        match event.severity {
          "critical" => critical_alerts = critical_alerts.push(event)
          "warning" => warning_alerts = warning_alerts.push(event)
          "info" => info_alerts = info_alerts.push(event)
          _ => ()
        }
      }
      _ => metric_events = metric_events.push(event)
    }
  }
  
  // 验证事件分类
  assert_eq(metric_events.length(), 6)
  assert_eq(alert_events.length(), 4)
  assert_eq(critical_alerts.length(), 2)
  assert_eq(warning_alerts.length(), 1)
  assert_eq(info_alerts.length(), 1)
  
  // 验证阈值检测
  let mut threshold_exceedances = []
  let mut normalizations = []
  
  for metric in metric_events {
    if metric.status == "critical" {
      threshold_exceedances = threshold_exceedances.push(metric)
    } else if metric.status == "normal" && metric.value > 0 { // 排除初始正常状态
      normalizations = normalizations.push(metric)
    }
  }
  
  assert_eq(threshold_exceedances.length(), 3)
  assert_eq(normalizations.length(), 3)
  
  // 验证CPU使用率阈值检测
  let cpu_critical = threshold_exceedances[0]
  assert_eq(cpu_critical.metric, "cpu_usage")
  assert_eq(cpu_critical.value, 85.0)
  assert_eq(cpu_critical.threshold, 80.0)
  assert_eq(cpu_critical.status, "critical")
  
  // 验证磁盘使用率阈值检测
  let disk_critical = threshold_exceedances[1]
  assert_eq(disk_critical.metric, "disk_usage")
  assert_eq(disk_critical.value, 95.0)
  assert_eq(disk_critical.threshold, 90.0)
  assert_eq(disk_critical.status, "critical")
  
  // 验证响应时间阈值检测
  let response_time_critical = threshold_exceedances[2]
  assert_eq(response_time_critical.metric, "response_time")
  assert_eq(response_time_critical.value, 2000.0)
  assert_eq(response_time_critical.threshold, 1000.0)
  assert_eq(response_time_critical.status, "critical")
  
  // 验证告警生成
  let cpu_alert = warning_alerts[0]
  assert_eq(cpu_alert.alert_type, "threshold_exceeded")
  assert_eq(cpu_alert.severity, "warning")
  assert_eq(cpu_alert.message, "CPU usage exceeded threshold")
  
  // 验证严重告警
  assert_eq(critical_alerts.length(), 2)
  let disk_alert = critical_alerts[0]
  assert_eq(disk_alert.alert_type, "threshold_exceeded")
  assert_eq(disk_alert.severity, "critical")
  assert_eq(disk_alert.message, "Disk usage exceeded threshold")
  
  let response_time_alert = critical_alerts[1]
  assert_eq(response_time_alert.alert_type, "threshold_exceeded")
  assert_eq(response_time_alert.severity, "critical")
  assert_eq(response_time_alert.message, "Response time exceeded threshold")
  
  // 验证恢复告警
  let recovery_alert = info_alerts[0]
  assert_eq(recovery_alert.alert_type, "threshold_normal")
  assert_eq(recovery_alert.severity, "info")
  assert_eq(recovery_alert.message, "CPU usage returned to normal")
  
  // 验证告警时间线
  let cpu_exceed_time = cpu_critical.timestamp
  let cpu_alert_time = cpu_alert.timestamp
  assert_eq(cpu_alert_time, cpu_exceed_time)
  
  let disk_exceed_time = disk_critical.timestamp
  let disk_alert_time = disk_alert.timestamp
  assert_eq(disk_alert_time, disk_exceed_time)
  
  let response_time_exceed_time = response_time_critical.timestamp
  let response_time_alert_time = response_time_alert.timestamp
  assert_eq(response_time_alert_time, response_time_exceed_time)
  
  // 验证恢复检测
  let cpu_recovery = normalizations[2]
  assert_eq(cpu_recovery.metric, "cpu_usage")
  assert_eq(cpu_recovery.value, 75.0)
  assert_eq(cpu_recovery.threshold, 80.0)
  assert_eq(cpu_recovery.status, "normal")
  
  let recovery_alert_time = recovery_alert.timestamp
  assert_eq(recovery_alert_time, cpu_recovery.timestamp)
}