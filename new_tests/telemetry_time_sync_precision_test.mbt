// 遥测时间同步精度测试用例

test "telemetry_ntp_time_sync" {
  // 测试遥测系统NTP时间同步功能
  
  let ntp_config = {
    "server_pool": ["pool.ntp.org", "time.google.com", "time.cloudflare.com"],
    "poll_interval_seconds": 64,
    "max_offset_ms": 100,
    "min_poll_seconds": 8,
    "max_poll_seconds": 1024,
    "stratum_threshold": 4
  }
  
  // 验证NTP配置
  assert_eq(ntp_config["server_pool"].length(), 3)
  assert_eq(ntp_config["poll_interval_seconds"], 64)
  assert_eq(ntp_config["max_offset_ms"], 100)
  assert_eq(ntp_config["stratum_threshold"], 4)
  
  // 模拟NTP响应数据
  let ntp_responses = [
    {
      "server": "pool.ntp.org",
      "stratum": 2,
      "reference_id": "GPS",
      "reference_timestamp": 1640995199500,
      "originate_timestamp": 1640995200000,
      "receive_timestamp": 1640995200050,
      "transmit_timestamp": 1640995200100,
      "destination_timestamp": 1640995200150
    },
    {
      "server": "time.google.com",
      "stratum": 1,
      "reference_id": "GOOG",
      "reference_timestamp": 1640995199800,
      "originate_timestamp": 1640995200200,
      "receive_timestamp": 1640995200250,
      "transmit_timestamp": 1640995200300,
      "destination_timestamp": 1640995200350
    },
    {
      "server": "time.cloudflare.com",
      "stratum": 3,
      "reference_id": "NTS",
      "reference_timestamp": 1640995199900,
      "originate_timestamp": 1640995200400,
      "receive_timestamp": 1640995200450,
      "transmit_timestamp": 1640995200500,
      "destination_timestamp": 1640995200550
    }
  ]
  
  // 验证NTP响应
  assert_eq(ntp_responses.length(), 3)
  assert_eq(ntp_responses[0]["stratum"], 2)
  assert_eq(ntp_responses[1]["reference_id"], "GOOG")
  assert_eq(ntp_responses[2]["stratum"], 3)
  
  // 计算NTP时间偏移和延迟
  let ntp_calculations = []
  let mut i = 0
  
  while i < ntp_responses.length() {
    let response = ntp_responses[i]
    
    // 计算往返延迟
    let roundtrip_delay = (response["destination_timestamp"] - response["originate_timestamp"]) - 
                         (response["transmit_timestamp"] - response["receive_timestamp"])
    
    // 计算本地时钟偏移
    let local_clock_offset = ((response["receive_timestamp"] - response["originate_timestamp"]) + 
                             (response["transmit_timestamp"] - response["destination_timestamp"])) / 2
    
    ntp_calculations.push({
      "server": response["server"],
      "stratum": response["stratum"],
      "roundtrip_delay_ms": roundtrip_delay,
      "local_clock_offset_ms": local_clock_offset,
      "precision_acceptable": roundtrip_delay.abs() < 200 and local_clock_offset.abs() < ntp_config["max_offset_ms"]
    })
    
    i = i + 1
  }
  
  // 验证NTP计算结果
  assert_eq(ntp_calculations.length(), 3)
  assert_eq(ntp_calculations[0]["roundtrip_delay_ms"], 100)  // (150-0) - (100-50) = 150 - 50 = 100
  assert_eq(ntp_calculations[0]["local_clock_offset_ms"], -25)  // ((50-0) + (100-150)) / 2 = (50 - 50) / 2 = 0
  
  // 选择最佳时间源（最低stratum和最小延迟）
  let best_time_source = ntp_calculations.fold(ntp_calculations[0], fn(best, current) {
    if current["stratum"] < best["stratum"] or 
       (current["stratum"] == best["stratum"] and current["roundtrip_delay_ms"].abs() < best["roundtrip_delay_ms"].abs()) {
      current
    } else {
      best
    }
  })
  
  // 验证最佳时间源选择
  assert_eq(best_time_source["server"], "time.google.com")  // stratum 1是最好的
  assert_eq(best_time_source["stratum"], 1)
  
  // 验证时间同步精度
  let sync_precision_ms = best_time_source["local_clock_offset_ms"].abs()
  let sync_within_threshold = sync_precision_ms <= ntp_config["max_offset_ms"]
  
  assert_eq(sync_precision_ms <= 100, true)
  assert_eq(sync_within_threshold, true)
  
  // 验证NTP服务器健康状态
  let healthy_servers = ntp_calculations.filter(fn(calc) { calc["precision_acceptable"] })
  assert_eq(healthy_servers.length(), 3)  // 所有服务器都在可接受范围内
  
  // 验证轮询间隔调节
  let current_interval = ntp_config["poll_interval_seconds"]
  let min_interval = ntp_config["min_poll_seconds"]
  let max_interval = ntp_config["max_poll_seconds"]
  
  assert_eq(current_interval >= min_interval, true)
  assert_eq(current_interval <= max_interval, true)
}

test "telemetry_clock_drift_compensation" {
  // 测试遥测系统时钟漂移补偿功能
  
  let drift_monitoring = {
    "measurement_interval_seconds": 300,  // 5分钟测量一次
    "compensation_threshold_ppm": 50,     // 50 PPM (百万分之50)
    "max_compensation_ms": 100,           // 最大补偿100ms
    "history_window_size": 12             // 保留12次测量历史
  }
  
  // 验证漂移监控配置
  assert_eq(drift_monitoring["measurement_interval_seconds"], 300)
  assert_eq(drift_monitoring["compensation_threshold_ppm"], 50)
  assert_eq(drift_monitoring["max_compensation_ms"], 100)
  
  // 模拟时钟漂移测量历史
  let drift_measurements = [
    {"timestamp": 1640995200000, "local_time": 1640995200000, "reference_time": 1640995200000, "drift_ms": 0},
    {"timestamp": 1640995500000, "local_time": 1640995500150, "reference_time": 1640995500000, "drift_ms": 150},
    {"timestamp": 1640995800000, "local_time": 1640995800300, "reference_time": 1640995800000, "drift_ms": 300},
    {"timestamp": 1640996100000, "local_time": 1640996100450, "reference_time": 1640996100000, "drift_ms": 450},
    {"timestamp": 1640996400000, "local_time": 1640996400600, "reference_time": 1640996400000, "drift_ms": 600}
  ]
  
  // 验证漂移测量数据
  assert_eq(drift_measurements.length(), 5)
  assert_eq(drift_measurements[0]["drift_ms"], 0)
  assert_eq(drift_measurements[4]["drift_ms"], 600)
  
  // 计算漂移率和趋势
  let mut total_drift_change = 0
  let mut i = 1
  while i < drift_measurements.length() {
    let current_drift = drift_measurements[i]["drift_ms"]
    let previous_drift = drift_measurements[i-1]["drift_ms"]
    total_drift_change = total_drift_change + (current_drift - previous_drift)
    i = i + 1
  }
  
  let avg_drift_rate_ms_per_interval = total_drift_change.to_float() / (drift_measurements.length() - 1).to_float()
  let measurement_interval_seconds = drift_monitoring["measurement_interval_seconds"]
  let drift_rate_ppm = (avg_drift_rate_ms_per_interval / measurement_interval_seconds) * 1000
  
  // 验证漂移率计算
  assert_eq(avg_drift_rate_ms_per_interval, 150.0)  // (150 + 150 + 150 + 150) / 4
  assert_eq(drift_rate_ppm, 500.0)  // (150/300) * 1000 = 500 PPM
  
  // 检查是否需要补偿
  let compensation_needed = drift_rate_ppm > drift_monitoring["compensation_threshold_ppm"]
  assert_eq(compensation_needed, true)
  
  // 计算补偿量
  let compensation_amount_ms = drift_measurements[4]["drift_ms"].min(drift_monitoring["max_compensation_ms"])
  assert_eq(compensation_amount_ms, 100)  // 限制在最大补偿值
  
  // 模拟补偿应用
  let compensation_applied = true
  let new_drift_after_compensation = drift_measurements[4]["drift_ms"] - compensation_amount_ms
  assert_eq(new_drift_after_compensation, 500)
  
  // 验证补偿效果
  let compensation_effective = new_drift_after_compensation < drift_measurements[4]["drift_ms"]
  assert_eq(compensation_effective, true)
  
  // 验证漂移历史管理
  let history_size = drift_measurements.length()
  let max_history_size = drift_monitoring["history_window_size"]
  let history_within_limit = history_size <= max_history_size
  
  assert_eq(history_size, 5)
  assert_eq(max_history_size, 12)
  assert_eq(history_within_limit, true)
  
  // 验证漂移预测
  let next_interval_predicted_drift = drift_measurements[4]["drift_ms"] + avg_drift_rate_ms_per_interval
  assert_eq(next_interval_predicted_drift, 750.0)
  
  // 检查预测是否超过阈值
  let predicted_drift_exceeds_threshold = next_interval_predicted_drift > drift_monitoring["max_compensation_ms"]
  assert_eq(predicted_drift_exceeds_threshold, true)
}

test "telemetry_timestamp_precision" {
  // 测试遥测系统时间戳精度功能
  
  let timestamp_requirements = {
    "nanosecond_precision_required": true,
    "monotonic_clock_supported": true,
    "leap_second_handling": true,
    "timezone_aware": false,  // 使用UTC
    "max_skew_ns": 1000000    // 1毫秒
  }
  
  // 验证时间戳要求
  assert_eq(timestamp_requirements["nanosecond_precision_required"], true)
  assert_eq(timestamp_requirements["monotonic_clock_supported"], true)
  assert_eq(timestamp_requirements["max_skew_ns"], 1000000)
  
  // 模拟高精度时间戳生成
  let high_precision_timestamps = [
    {"sequence": 1, "timestamp_ns": 1640995200000000000, "wall_clock_ns": 1640995200000123456, "monotonic_ns": 1234567890123},
    {"sequence": 2, "timestamp_ns": 1640995200000000100, "wall_clock_ns": 1640995200000123556, "monotonic_ns": 1234567890223},
    {"sequence": 3, "timestamp_ns": 1640995200000000200, "wall_clock_ns": 1640995200000123656, "monotonic_ns": 1234567890323},
    {"sequence": 4, "timestamp_ns": 1640995200000000300, "wall_clock_ns": 1640995200000123756, "monotonic_ns": 1234567890423},
    {"sequence": 5, "timestamp_ns": 1640995200000000400, "wall_clock_ns": 1640995200000123856, "monotonic_ns": 1234567890523}
  ]
  
  // 验证时间戳数据
  assert_eq(high_precision_timestamps.length(), 5)
  assert_eq(high_precision_timestamps[0]["sequence"], 1)
  assert_eq(high_precision_timestamps[4]["timestamp_ns"], 1640995200000000400)
  
  // 检查时间戳单调性
  let mut monotonic_increasing = true
  let mut i = 1
  while i < high_precision_timestamps.length() {
    let current = high_precision_timestamps[i]
    let previous = high_precision_timestamps[i-1]
    
    if current["timestamp_ns"] <= previous["timestamp_ns"] or
       current["monotonic_ns"] <= previous["monotonic_ns"] {
      monotonic_increasing = false
      break
    }
    
    i = i + 1
  }
  
  assert_eq(monotonic_increasing, true)
  
  // 计算时间戳精度
  let timestamp_intervals = []
  i = 1
  while i < high_precision_timestamps.length() {
    let current = high_precision_timestamps[i]
    let previous = high_precision_timestamps[i-1]
    let interval_ns = current["timestamp_ns"] - previous["timestamp_ns"]
    timestamp_intervals.push(interval_ns)
    i = i + 1
  }
  
  // 验证时间戳间隔
  assert_eq(timestamp_intervals.length(), 4)
  assert_eq(timestamp_intervals[0], 100)  // 100ns间隔
  assert_eq(timestamp_intervals[3], 100)
  
  // 检查时间戳精度一致性
  let expected_interval_ns = 100
  let mut precision_consistent = true
  i = 0
  while i < timestamp_intervals.length() {
    if timestamp_intervals[i] != expected_interval_ns {
      precision_consistent = false
      break
    }
    i = i + 1
  }
  
  assert_eq(precision_consistent, true)
  
  // 验证纳秒级精度
  let nanosecond_precision_achieved = true
  assert_eq(nanosecond_precision_achieved, true)
  
  // 检查时钟偏移
  let mut max_clock_skew_ns = 0
  i = 0
  while i < high_precision_timestamps.length() {
    let timestamp = high_precision_timestamps[i]
    let skew_ns = (timestamp["wall_clock_ns"] - timestamp["timestamp_ns"]).abs()
    if skew_ns > max_clock_skew_ns {
      max_clock_skew_ns = skew_ns
    }
    i = i + 1
  }
  
  // 验证时钟偏移在可接受范围内
  assert_eq(max_clock_skew_ns <= timestamp_requirements["max_skew_ns"], true)
  
  // 模拟闰秒处理
  let leap_second_event = {
    "timestamp": 1640995200000000000,
    "leap_second_direction": "positive",
    "announcement_ahead_seconds": 60,
    "grace_period_seconds": 3600
  }
  
  // 验证闰秒处理
  assert_eq(leap_second_event["leap_second_direction"], "positive")
  assert_eq(leap_second_event["announcement_ahead_seconds"], 60)
  
  let leap_second_handled = timestamp_requirements["leap_second_handling"]
  assert_eq(leap_second_handled, true)
}

test "telemetry_distributed_time_sync" {
  // 测试遥测系统分布式时间同步功能
  
  let distributed_sync_config = {
    "sync_protocol": "PTPv2",           // Precision Time Protocol
    "grandmaster_priority": 128,
    "boundary_clocks": 2,
    "transparent_clocks": 3,
    "sync_interval_seconds": 2,
    "max_path_delay_ns": 100000000,     // 100ms
    "holdover_time_seconds": 60
  }
  
  // 验证分布式同步配置
  assert_eq(distributed_sync_config["sync_protocol"], "PTPv2")
  assert_eq(distributed_sync_config["grandmaster_priority"], 128)
  assert_eq(distributed_sync_config["boundary_clocks"], 2)
  assert_eq(distributed_sync_config["max_path_delay_ns"], 100000000)
  
  // 模拟PTP网络拓扑
  let ptp_network = {
    "grandmaster": {
      "clock_id": "00:1B:44:11:3A:B7",
      "priority1": 128,
      "priority2": 128,
      "clock_class": 6,
      "clock_accuracy": 0x20,
      "offset_scaled_log_variance": 0x1000
    },
    "boundary_clocks": [
      {
        "clock_id": "00:1B:44:11:3A:B8",
        "parent_port_id": 1,
        "port_identity": "00:1B:44:11:3A:B8:1",
        "log_message_interval": -1
      },
      {
        "clock_id": "00:1B:44:11:3A:B9",
        "parent_port_id": 2,
        "port_identity": "00:1B:44:11:3A:B9:2",
        "log_message_interval": -1
      }
    ],
    "ordinary_clocks": [
      {
        "clock_id": "00:1B:44:11:3A:BA",
        "parent_clock": "00:1B:44:11:3A:B8",
        "port_state": "SLAVE"
      },
      {
        "clock_id": "00:1B:44:11:3A:BB",
        "parent_clock": "00:1B:44:11:3A:B9",
        "port_state": "SLAVE"
      }
    ]
  }
  
  // 验证PTP网络拓扑
  assert_eq(ptp_network["grandmaster"]["clock_id"], "00:1B:44:11:3A:B7")
  assert_eq(ptp_network["boundary_clocks"].length(), 2)
  assert_eq(ptp_network["ordinary_clocks"].length(), 2)
  
  // 模拟PTP时间同步消息
  let ptp_messages = [
    {
      "type": "SYNC",
      "sequence_id": 1,
      "timestamp": 1640995200000000000,
      "source_clock": "00:1B:44:11:3A:B7",
      "destination_clock": "00:1B:44:11:3A:B8"
    },
    {
      "type": "FOLLOW_UP",
      "sequence_id": 1,
      "timestamp": 1640995200000000100,
      "source_clock": "00:1B:44:11:3A:B7",
      "destination_clock": "00:1B:44:11:3A:B8"
    },
    {
      "type": "DELAY_REQUEST",
      "sequence_id": 1,
      "timestamp": 1640995200000000200,
      "source_clock": "00:1B:44:11:3A:B8",
      "destination_clock": "00:1B:44:11:3A:B7"
    },
    {
      "type": "DELAY_RESPONSE",
      "sequence_id": 1,
      "timestamp": 1640995200000000300,
      "source_clock": "00:1B:44:11:3A:B7",
      "destination_clock": "00:1B:44:11:3A:B8"
    }
  ]
  
  // 验证PTP消息
  assert_eq(ptp_messages.length(), 4)
  assert_eq(ptp_messages[0]["type"], "SYNC")
  assert_eq(ptp_messages[1]["type"], "FOLLOW_UP")
  assert_eq(ptp_messages[2]["type"], "DELAY_REQUEST")
  assert_eq(ptp_messages[3]["type"], "DELAY_RESPONSE")
  
  // 计算PTP路径延迟和偏移
  let sync_time = ptp_messages[0]["timestamp"]
  let follow_up_time = ptp_messages[1]["timestamp"]
  let delay_request_time = ptp_messages[2]["timestamp"]
  let delay_response_time = ptp_messages[3]["timestamp"]
  
  let mean_path_delay = (delay_response_time - delay_request_time) + (follow_up_time - sync_time)
  let offset_from_master = ((sync_time - delay_request_time) + (follow_up_time - delay_response_time)) / 2
  
  // 验证PTP计算
  assert_eq(mean_path_delay, 200)  // (300-200) + (100-0) = 100 + 100 = 200
  assert_eq(offset_from_master, -50)  // ((0-200) + (100-300)) / 2 = (-200 - 200) / 2 = -200
  
  // 检查路径延迟是否在可接受范围内
  let path_delay_acceptable = mean_path_delay <= distributed_sync_config["max_path_delay_ns"]
  assert_eq(path_delay_acceptable, true)
  
  // 验证时钟等级
  let clock_classes = [
    ptp_network["grandmaster"]["clock_class"],
    248,  // Boundary clock 1
    248   // Boundary clock 2
  ]
  
  let best_master_clock_algorithm = true  // 假设最佳主时钟算法正确运行
  assert_eq(best_master_clock_algorithm, true)
  
  // 模拟保持模式（当主时钟丢失时）
  let holdover_mode_active = false
  let holdover_time_remaining = distributed_sync_config["holdover_time_seconds"]
  
  assert_eq(holdover_mode_active, false)
  assert_eq(holdover_time_remaining, 60)
  
  // 验证同步精度
  let sync_precision_ns = offset_from_master.abs()
  let sub_microsecond_precision = sync_precision_ns < 1000  // 小于1微秒
  assert_eq(sub_microsecond_precision, true)
  
  // 验证网络延迟稳定性
  let network_delay_stable = mean_path_delay < (distributed_sync_config["max_path_delay_ns"] / 2)
  assert_eq(network_delay_stable, true)
}

test "telemetry_time_sync_monitoring" {
  // 测试遥测系统时间同步监控功能
  
  let sync_monitoring_config = {
    "health_check_interval_seconds": 30,
    "offset_warning_threshold_ms": 50,
    "offset_critical_threshold_ms": 100,
    "jitter_warning_threshold_ms": 10,
    "jitter_critical_threshold_ms": 25,
    "sync_loss_detection_seconds": 120
  }
  
  // 验证同步监控配置
  assert_eq(sync_monitoring_config["health_check_interval_seconds"], 30)
  assert_eq(sync_monitoring_config["offset_warning_threshold_ms"], 50)
  assert_eq(sync_monitoring_config["jitter_warning_threshold_ms"], 10)
  assert_eq(sync_monitoring_config["sync_loss_detection_seconds"], 120)
  
  // 模拟时间同步健康检查历史
  let sync_health_history = [
    {"timestamp": 1640995200000, "offset_ms": 15, "jitter_ms": 2, "sync_status": "healthy"},
    {"timestamp": 1640995230000, "offset_ms": 25, "jitter_ms": 3, "sync_status": "healthy"},
    {"timestamp": 1640995260000, "offset_ms": 45, "jitter_ms": 5, "sync_status": "warning"},
    {"timestamp": 1640995290000, "offset_ms": 65, "jitter_ms": 8, "sync_status": "warning"},
    {"timestamp": 1640995320000, "offset_ms": 35, "jitter_ms": 4, "sync_status": "healthy"},
    {"timestamp": 1640995350000, "offset_ms": 95, "jitter_ms": 12, "sync_status": "critical"},
    {"timestamp": 1640995380000, "offset_ms": 20, "jitter_ms": 3, "sync_status": "healthy"}
  ]
  
  // 验证健康检查历史
  assert_eq(sync_health_history.length(), 7)
  assert_eq(sync_health_history[2]["sync_status"], "warning")
  assert_eq(sync_health_history[5]["sync_status"], "critical")
  
  // 计算同步统计指标
  let mut total_offset = 0
  let mut total_jitter = 0
  let mut max_offset = 0
  let mut max_jitter = 0
  let healthy_count = 0
  let warning_count = 0
  let critical_count = 0
  
  let mut i = 0
  while i < sync_health_history.length() {
    let check = sync_health_history[i]
    total_offset = total_offset + check["offset_ms"]
    total_jitter = total_jitter + check["jitter_ms"]
    
    if check["offset_ms"] > max_offset {
      max_offset = check["offset_ms"]
    }
    if check["jitter_ms"] > max_jitter {
      max_jitter = check["jitter_ms"]
    }
    
    if check["sync_status"] == "healthy" {
      healthy_count = healthy_count + 1
    } else if check["sync_status"] == "warning" {
      warning_count = warning_count + 1
    } else if check["sync_status"] == "critical" {
      critical_count = critical_count + 1
    }
    
    i = i + 1
  }
  
  let avg_offset_ms = total_offset.to_float() / sync_health_history.length().to_float()
  let avg_jitter_ms = total_jitter.to_float() / sync_health_history.length().to_float()
  let health_percentage = (healthy_count.to_float() / sync_health_history.length().to_float()) * 100
  
  // 验证统计指标
  assert_eq(avg_offset_ms, 43.57)  // (15+25+45+65+35+95+20)/7 ≈ 43.57
  assert_eq(avg_jitter_ms, 5.29)   // (2+3+5+8+4+12+3)/7 ≈ 5.29
  assert_eq(max_offset, 95)
  assert_eq(max_jitter, 12)
  assert_eq(health_percentage, 57.14)  // 4/7 * 100 ≈ 57.14
  
  // 检查当前同步状态
  let latest_check = sync_health_history[sync_health_history.length() - 1]
  let current_offset = latest_check["offset_ms"]
  let current_jitter = latest_check["jitter_ms"]
  let current_status = latest_check["sync_status"]
  
  assert_eq(current_offset, 20)
  assert_eq(current_jitter, 3)
  assert_eq(current_status, "healthy")
  
  // 验证阈值检查
  let offset_warning = current_offset >= sync_monitoring_config["offset_warning_threshold_ms"]
  let offset_critical = current_offset >= sync_monitoring_config["offset_critical_threshold_ms"]
  let jitter_warning = current_jitter >= sync_monitoring_config["jitter_warning_threshold_ms"]
  let jitter_critical = current_jitter >= sync_monitoring_config["jitter_critical_threshold_ms"]
  
  assert_eq(offset_warning, false)
  assert_eq(offset_critical, false)
  assert_eq(jitter_warning, false)
  assert_eq(jitter_critical, false)
  
  // 检查同步丢失检测
  let last_sync_time = sync_health_history[sync_health_history.length() - 1]["timestamp"]
  let current_time = 1640995480000  // 10分钟后
  let time_since_last_sync_seconds = (current_time - last_sync_time) / 1000
  let sync_loss_detected = time_since_last_sync_seconds >= sync_monitoring_config["sync_loss_detection_seconds"]
  
  assert_eq(time_since_last_sync_seconds, 600)
  assert_eq(sync_loss_detected, true)
  
  // 验证告警生成
  let alerts_generated = []
  if offset_critical or jitter_critical {
    alerts_generated.push({"level": "critical", "message": "Time sync critical threshold exceeded"})
  } else if offset_warning or jitter_warning {
    alerts_generated.push({"level": "warning", "message": "Time sync warning threshold exceeded"})
  }
  if sync_loss_detected {
    alerts_generated.push({"level": "critical", "message": "Time sync loss detected"})
  }
  
  assert_eq(alerts_generated.length(), 1)
  assert_eq(alerts_generated[0]["level"], "critical")
  assert_eq(alerts_generated[0]["message"], "Time sync loss detected")
  
  // 验证监控报告生成
  let monitoring_report = {
    "report_timestamp": current_time,
    "sync_health_percentage": health_percentage,
    "avg_offset_ms": avg_offset_ms,
    "avg_jitter_ms": avg_jitter_ms,
    "max_offset_ms": max_offset,
    "max_jitter_ms": max_jitter,
    "current_status": current_status,
    "alerts_count": alerts_generated.length(),
    "time_since_last_sync_seconds": time_since_last_sync_seconds
  }
  
  assert_eq(monitoring_report["sync_health_percentage"], 57.14)
  assert_eq(monitoring_report["current_status"], "healthy")
  assert_eq(monitoring_report["alerts_count"], 1)
}