// 遥测边缘计算测试用例
// 测试遥测系统在边缘计算环境中的部署和运行

test "telemetry_edge_device_collection" {
  // 测试边缘设备遥测数据收集
  
  let edge_devices = [
    {
      "device_id": "edge-device-001",
      "location": "factory-floor-1",
      "device_type": "iot-sensor",
      "capabilities": ["cpu_monitor", "memory_monitor", "temperature_monitor"],
      "battery_level": 85.2,
      "network_quality": "good",
      "last_heartbeat": 1672531200
    },
    {
      "device_id": "edge-device-002",
      "location": "factory-floor-2",
      "device_type": "gateway",
      "capabilities": ["cpu_monitor", "memory_monitor", "network_monitor"],
      "battery_level": 92.7,
      "network_quality": "excellent",
      "last_heartbeat": 1672531205
    },
    {
      "device_id": "edge-device-003",
      "location": "warehouse-1",
      "device_type": "edge-server",
      "capabilities": ["cpu_monitor", "memory_monitor", "disk_monitor", "network_monitor"],
      "battery_level": 78.3,
      "network_quality": "fair",
      "last_heartbeat": 1672531195
    }
  ]
  
  // 验证边缘设备配置
  assert_eq(edge_devices.length(), 3)
  
  // 模拟边缘设备遥测数据收集
  let device_telemetry = [
    {
      "device_id": "edge-device-001",
      "timestamp": 1672531210,
      "metrics": {
        "cpu_usage": 25.5,
        "memory_usage": 45.8,
        "temperature": 42.3,
        "battery_voltage": 3.7
      }
    },
    {
      "device_id": "edge-device-002",
      "timestamp": 1672531210,
      "metrics": {
        "cpu_usage": 35.2,
        "memory_usage": 52.1,
        "network_latency_ms": 15,
        "packet_loss": 0.1
      }
    },
    {
      "device_id": "edge-device-003",
      "timestamp": 1672531210,
      "metrics": {
        "cpu_usage": 65.8,
        "memory_usage": 78.4,
        "disk_usage": 45.2,
        "network_throughput_mbps": 125.7
      }
    }
  ]
  
  // 验证设备遥测数据
  assert_eq(device_telemetry.length(), 3)
  
  // 分析设备健康状态
  let mut device_health_status = {}
  
  let mut i = 0
  while i < edge_devices.length() {
    let device = edge_devices[i]
    let device_id = device.get("device_id", "")
    let battery_level = device.get("battery_level", 0.0)
    let network_quality = device.get("network_quality", "")
    let last_heartbeat = device.get("last_heartbeat", 0)
    
    # 查找对应的遥测数据
    let mut device_metrics = {}
    let mut j = 0
    while j < device_telemetry.length() {
      if device_telemetry[j].get("device_id", "") == device_id {
        device_metrics = device_telemetry[j].get("metrics", {})
        break
      }
      j = j + 1
    }
    
    # 计算健康分数
    let battery_score = if battery_level > 80 { 100.0 }
                      else if battery_level > 60 { 80.0 }
                      else if battery_level > 40 { 60.0 }
                      else { 40.0 }
    
    let network_score = match network_quality {
      "excellent" => 100.0,
      "good" => 85.0,
      "fair" => 70.0,
      "poor" => 50.0,
      _ => 30.0
    }
    
    let cpu_usage = device_metrics.get("cpu_usage", 0.0)
    let cpu_score = if cpu_usage < 50 { 100.0 }
                   else if cpu_usage < 75 { 80.0 }
                   else if cpu_usage < 90 { 60.0 }
                   else { 40.0 }
    
    let memory_usage = device_metrics.get("memory_usage", 0.0)
    let memory_score = if memory_usage < 60 { 100.0 }
                      else if memory_usage < 80 { 80.0 }
                      else if memory_usage < 90 { 60.0 }
                      else { 40.0 }
    
    # 计算综合健康分数
    let overall_health = (battery_score + network_score + cpu_score + memory_score) / 4.0
    
    # 确定健康状态
    let health_status = if overall_health >= 85 { "excellent" }
                       else if overall_health >= 70 { "good" }
                       else if overall_health >= 55 { "fair" }
                       else { "poor" }
    
    device_health_status.set(device_id, {
      "health_score": overall_health,
      "health_status": health_status,
      "battery_score": battery_score,
      "network_score": network_score,
      "cpu_score": cpu_score,
      "memory_score": memory_score
    })
    
    i = i + 1
  }
  
  // 验证设备健康状态
  let device_001_health = device_health_status.get("edge-device-001", {})
  let device_002_health = device_health_status.get("edge-device-002", {})
  let device_003_health = device_health_status.get("edge-device-003", {})
  
  assert_eq(device_001_health.get("health_status", ""), "good")
  assert_eq(device_002_health.get("health_status", ""), "excellent")
  assert_eq(device_003_health.get("health_status", ""), "fair")
  
  // 验证健康分数排序
  let health_scores = [
    ("edge-device-001", device_001_health.get("health_score", 0.0)),
    ("edge-device-002", device_002_health.get("health_score", 0.0)),
    ("edge-device-003", device_003_health.get("health_score", 0.0))
  ]
  
  # 按健康分数排序
  let mut sorted = false
  let mut k = 0
  while k < health_scores.length() - 1 and not sorted {
    sorted = true
    let mut l = 0
    while l < health_scores.length() - 1 - k {
      if health_scores[l].1 < health_scores[l + 1].1 {
        let temp = health_scores[l]
        health_scores[l] = health_scores[l + 1]
        health_scores[l + 1] = temp
        sorted = false
      }
      l = l + 1
    }
    k = k + 1
  }
  
  # 验证排序结果
  assert_eq(health_scores[0].0, "edge-device-002")  # 最健康的设备
  assert_eq(health_scores[2].0, "edge-device-003")  # 最不健康的设备
}

test "telemetry_edge_data_processing" {
  // 测试边缘数据处理
  
  let edge_processing_config = {
    "processing_mode": "streaming",
    "batch_size": 50,
    "processing_interval_ms": 1000,
    "local_retention_hours": 24,
    "compression_enabled": true,
    "encryption_enabled": false
  }
  
  // 验证边缘处理配置
  assert_eq(edge_processing_config.get("processing_mode", ""), "streaming")
  assert_eq(edge_processing_config.get("batch_size", 0), 50)
  assert_eq(edge_processing_config.get("compression_enabled", false), true)
  
  // 模拟边缘数据流
  let edge_data_stream = [
    {"timestamp": 1672531200, "device_id": "sensor-001", "metric": "temperature", "value": 23.5},
    {"timestamp": 1672531201, "device_id": "sensor-001", "metric": "humidity", "value": 65.2},
    {"timestamp": 1672531202, "device_id": "sensor-002", "metric": "temperature", "value": 24.1},
    {"timestamp": 1672531203, "device_id": "sensor-002", "metric": "pressure", "value": 1013.2},
    {"timestamp": 1672531204, "device_id": "sensor-001", "metric": "temperature", "value": 23.7},
    {"timestamp": 1672531205, "device_id": "sensor-003", "metric": "vibration", "value": 0.05},
    {"timestamp": 1672531206, "device_id": "sensor-003", "metric": "noise", "value": 45.8},
    {"timestamp": 1672531207, "device_id": "sensor-002", "metric": "temperature", "value": 24.3}
  ]
  
  // 验证边缘数据流
  assert_eq(edge_data_stream.length(), 8)
  
  // 实现边缘数据聚合
  let mut device_aggregates = {}
  
  let mut i = 0
  while i < edge_data_stream.length() {
    let data_point = edge_data_stream[i]
    let device_id = data_point.get("device_id", "")
    let metric = data_point.get("metric", "")
    let value = data_point.get("value", 0.0)
    
    # 获取或创建设备聚合数据
    let device_data = device_aggregates.get(device_id, {
      "metrics": {},
      "count": 0,
      "first_timestamp": data_point.get("timestamp", 0),
      "last_timestamp": data_point.get("timestamp", 0)
    })
    
    # 更新指标聚合
    let metrics = device_data.get("metrics", {})
    let metric_data = metrics.get(metric, {
      "sum": 0.0,
      "count": 0,
      "min": 999999.9,
      "max": -999999.9
    })
    
    let new_sum = metric_data.get("sum", 0.0) + value
    let new_count = metric_data.get("count", 0) + 1
    let new_min = if value < metric_data.get("min", 999999.9) { value } else { metric_data.get("min", 0.0) }
    let new_max = if value > metric_data.get("max", -999999.9) { value } else { metric_data.get("max", 0.0) }
    
    metrics.set(metric, {
      "sum": new_sum,
      "count": new_count,
      "min": new_min,
      "max": new_max,
      "avg": new_sum / new_count.to_double()
    })
    
    # 更新设备数据
    device_data.set("metrics", metrics)
    device_data.set("count", device_data.get("count", 0) + 1)
    device_data.set("last_timestamp", data_point.get("timestamp", 0))
    
    device_aggregates.set(device_id, device_data)
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(device_aggregates.keys().length(), 3)  # 3个设备
  
  # 验证sensor-001的聚合
  let sensor_001_data = device_aggregates.get("sensor-001", {})
  let sensor_001_metrics = sensor_001_data.get("metrics", {})
  
  assert_eq(sensor_001_data.get("count", 0), 3)  # 3个数据点
  assert_eq(sensor_001_metrics.contains("temperature"), true)
  assert_eq(sensor_001_metrics.contains("humidity"), true)
  
  let temp_metrics = sensor_001_metrics.get("temperature", {})
  assert_eq(temp_metrics.get("count", 0), 2)  # 2个温度数据点
  assert_eq(temp_metrics.get("avg", 0.0), 23.6)  # (23.5 + 23.7) / 2
  assert_eq(temp_metrics.get("min", 0.0), 23.5)
  assert_eq(temp_metrics.get("max", 0.0), 23.7)
  
  # 验证sensor-002的聚合
  let sensor_002_data = device_aggregates.get("sensor-002", {})
  let sensor_002_metrics = sensor_002_data.get("metrics", {})
  
  assert_eq(sensor_002_data.get("count", 0), 3)  # 3个数据点
  assert_eq(sensor_002_metrics.contains("temperature"), true)
  assert_eq(sensor_002_metrics.contains("pressure"), true)
  
  let temp_002_metrics = sensor_002_metrics.get("temperature", {})
  assert_eq(temp_002_metrics.get("count", 0), 2)  # 2个温度数据点
  assert_eq(temp_002_metrics.get("avg", 0.0), 24.2)  # (24.1 + 24.3) / 2
  
  # 模拟边缘数据压缩
  let original_data_size = 1024  # 假设原始数据大小
  let compression_ratio = 0.3   # 70%压缩率
  let compressed_size = original_data_size * compression_ratio
  
  # 验证压缩效果
  assert_eq(compressed_size < original_data_size, true)
  assert_eq(compressed_size, 307)  # 1024 * 0.3
  
  # 计算压缩节省
  let space_saved = original_data_size - compressed_size
  let compression_savings_percent = (space_saved.to_double() / original_data_size.to_double()) * 100.0
  
  assert_eq(compression_savings_percent, 70.0)  # 70%空间节省
  
  # 模拟边缘数据过滤
  let filtering_rules = [
    {"metric": "temperature", "min_value": 20.0, "max_value": 30.0},
    {"metric": "humidity", "min_value": 40.0, "max_value": 80.0},
    {"metric": "pressure", "min_value": 1000.0, "max_value": 1020.0}
  ]
  
  let mut filtered_data = []
  let mut filtered_count = 0
  
  i = 0
  while i < edge_data_stream.length() {
    let data_point = edge_data_stream[i]
    let metric = data_point.get("metric", "")
    let value = data_point.get("value", 0.0)
    
    # 检查过滤规则
    let mut passes_filter = true
    let mut j = 0
    while j < filtering_rules.length() {
      let rule = filtering_rules[j]
      if rule.get("metric", "") == metric {
        let min_val = rule.get("min_value", 0.0)
        let max_val = rule.get("max_value", 0.0)
        
        if value < min_val or value > max_val {
          passes_filter = false
          break
        }
      }
      j = j + 1
    }
    
    if passes_filter {
      filtered_data.push(data_point)
    } else {
      filtered_count = filtered_count + 1
    }
    
    i = i + 1
  }
  
  # 验证过滤结果
  assert_eq(filtered_data.length(), 8)  # 所有数据都通过过滤
  assert_eq(filtered_count, 0)          # 没有数据被过滤
}

test "telemetry_edge_cloud_sync" {
  // 测试边缘云同步
  
  let sync_config = {
    "sync_mode": "batch",
    "sync_interval_seconds": 300,  # 5分钟
    "max_batch_size": 1000,
    "retry_attempts": 3,
    "retry_delay_seconds": 30,
    "compression_enabled": true,
    "encryption_enabled": true
  }
  
  // 验证同步配置
  assert_eq(sync_config.get("sync_mode", ""), "batch")
  assert_eq(sync_config.get("sync_interval_seconds", 0), 300)
  assert_eq(sync_config.get("max_batch_size", 0), 1000)
  assert_eq(sync_config.get("compression_enabled", false), true)
  assert_eq(sync_config.get("encryption_enabled", false), true)
  
  // 模拟边缘数据队列
  let edge_data_queue = [
    {"id": "data-001", "timestamp": 1672531200, "device_id": "edge-001", "synced": false},
    {"id": "data-002", "timestamp": 1672531205, "device_id": "edge-001", "synced": false},
    {"id": "data-003", "timestamp": 1672531210, "device_id": "edge-002", "synced": false},
    {"id": "data-004", "timestamp": 1672531215, "device_id": "edge-002", "synced": false},
    {"id": "data-005", "timestamp": 1672531220, "device_id": "edge-003", "synced": false},
    {"id": "data-006", "timestamp": 1672531225, "device_id": "edge-003", "synced": false},
    {"id": "data-007", "timestamp": 1672531230, "device_id": "edge-001", "synced": false},
    {"id": "data-008", "timestamp": 1672531235, "device_id": "edge-002", "synced": false}
  ]
  
  // 验证数据队列
  assert_eq(edge_data_queue.length(), 8)
  
  // 模拟同步过程
  let mut sync_batches = []
  let batch_size = sync_config.get("max_batch_size", 1000)
  
  # 将数据分批
  let mut current_batch = []
  let mut i = 0
  while i < edge_data_queue.length() {
    let data_item = edge_data_queue[i]
    if not data_item.get("synced", false) {
      current_batch.push(data_item)
      
      if current_batch.length() >= batch_size or i == edge_data_queue.length() - 1 {
        sync_batches.push(current_batch)
        current_batch = []
      }
    }
    i = i + 1
  }
  
  // 验证分批结果
  assert_eq(sync_batches.length(), 1)  # 所有数据在一个批次中
  assert_eq(sync_batches[0].length(), 8)  # 批次包含8个数据项
  
  // 模拟网络状况
  let network_conditions = [
    {"timestamp": 1672531240, "latency_ms": 50, "bandwidth_mbps": 10, "packet_loss": 0.0},
    {"timestamp": 1672531250, "latency_ms": 120, "bandwidth_mbps": 5, "packet_loss": 0.1},
    {"timestamp": 1672531260, "latency_ms": 200, "bandwidth_mbps": 2, "packet_loss": 0.5},
    {"timestamp": 1672531270, "latency_ms": 80, "bandwidth_mbps": 8, "packet_loss": 0.05}
  ]
  
  // 验证网络状况
  assert_eq(network_conditions.length(), 4)
  
  // 模拟同步尝试
  let mut sync_attempts = []
  let mut successful_syncs = []
  
  i = 0
  while i < sync_batches.length() {
    let batch = sync_batches[i]
    let batch_id = "batch-" + i.to_string()
    
    # 尝试同步
    let mut attempt = 1
    let mut sync_success = false
    let mut sync_result = {}
    
    while attempt <= sync_config.get("retry_attempts", 3) and not sync_success {
      # 选择网络状况
      let network_condition = network_conditions[i % network_conditions.length()]
      let latency = network_condition.get("latency_ms", 0)
      let bandwidth = network_condition.get("bandwidth_mbps", 0.0)
      let packet_loss = network_condition.get("packet_loss", 0.0)
      
      # 计算同步成功率
      let success_probability = if latency < 100 and bandwidth > 5 and packet_loss < 0.1 { 0.95 }
                               else if latency < 200 and bandwidth > 2 and packet_loss < 0.3 { 0.75 }
                               else { 0.5 }
      
      # 模拟同步结果
      sync_success = success_probability > 0.6  # 简化的成功判断
      
      sync_result = {
        "batch_id": batch_id,
        "attempt": attempt,
        "success": sync_success,
        "latency_ms": latency,
        "bandwidth_mbps": bandwidth,
        "packet_loss": packet_loss,
        "timestamp": network_condition.get("timestamp", 0)
      }
      
      if sync_success {
        successful_syncs.push(sync_result)
        # 标记数据为已同步
        let mut j = 0
        while j < batch.length() {
          batch[j].set("synced", true)
          j = j + 1
        }
      }
      
      attempt = attempt + 1
      
      if not sync_success and attempt <= sync_config.get("retry_attempts", 3) {
        # 等待重试延迟
        let retry_delay = sync_config.get("retry_delay_seconds", 0)
        # 实际实现中这里会有延迟
      }
    }
    
    sync_attempts.push(sync_result)
    i = i + 1
  }
  
  // 验证同步结果
  assert_eq(sync_attempts.length(), 1)  # 1个批次的同步尝试
  assert_eq(successful_syncs.length(), 1)  # 同步成功
  
  let sync_result = sync_attempts[0]
  assert_eq(sync_result.get("success", false), true)
  assert_eq(sync_result.get("batch_id", ""), "batch-0")
  
  // 验证数据同步状态
  let mut synced_count = 0
  i = 0
  while i < edge_data_queue.length() {
    if edge_data_queue[i].get("synced", false) {
      synced_count = synced_count + 1
    }
    i = i + 1
  }
  
  assert_eq(synced_count, 8)  # 所有数据都已同步
  
  // 计算同步统计
  let total_data_items = edge_data_queue.length()
  let sync_success_rate = synced_count.to_double() / total_data_items.to_double()
  
  assert_eq(sync_success_rate, 1.0)  # 100%同步成功率
  
  // 模拟同步性能指标
  let sync_performance = {
    "total_sync_time_ms": sync_result.get("latency_ms", 0),
    "data_transferred_mb": 2.5,
    "compression_ratio": 0.4,
    "encryption_overhead_ms": 15,
    "total_sync_cost": 0.05
  }
  
  // 验证同步性能
  assert_eq(sync_performance.get("total_sync_time_ms", 0) > 0, true)
  assert_eq(sync_performance.get("data_transferred_mb", 0.0), 2.5)
  assert_eq(sync_performance.get("compression_ratio", 0.0), 0.4)
  assert_eq(sync_performance.get("encryption_overhead_ms", 0), 15)
}

test "telemetry_edge_offline_capability" {
  // 测试边缘离线能力
  
  let offline_config = {
    "local_storage_size_mb": 1024,
    "offline_retention_days": 7,
    "auto_sync_on_reconnect": true,
    "data_priority_levels": ["critical", "high", "normal", "low"],
    "critical_data_buffer_size_mb": 100
  }
  
  // 验证离线配置
  assert_eq(offline_config.get("local_storage_size_mb", 0), 1024)
  assert_eq(offline_config.get("offline_retention_days", 0), 7)
  assert_eq(offline_config.get("auto_sync_on_reconnect", false), true)
  assert_eq(offline_config.get("data_priority_levels", []).length(), 4)
  
  // 模拟离线数据收集
  let offline_data_collection = [
    {"id": "offline-001", "timestamp": 1672531200, "priority": "critical", "size_kb": 5},
    {"id": "offline-002", "timestamp": 1672531205, "priority": "high", "size_kb": 8},
    {"id": "offline-003", "timestamp": 1672531210, "priority": "normal", "size_kb": 12},
    {"id": "offline-004", "timestamp": 1672531215, "priority": "low", "size_kb": 3},
    {"id": "offline-005", "timestamp": 1672531220, "priority": "critical", "size_kb": 6},
    {"id": "offline-006", "timestamp": 1672531225, "priority": "high", "size_kb": 10},
    {"id": "offline-007", "timestamp": 1672531230, "priority": "normal", "size_kb": 15},
    {"id": "offline-008", "timestamp": 1672531235, "priority": "low", "size_kb": 4}
  ]
  
  // 验证离线数据收集
  assert_eq(offline_data_collection.length(), 8)
  
  // 模拟存储空间管理
  let total_storage_kb = offline_config.get("local_storage_size_mb", 0) * 1024
  let critical_buffer_kb = offline_config.get("critical_data_buffer_size_mb", 0) * 1024
  
  let mut used_storage_kb = 0
  let mut critical_data_kb = 0
  let mut priority_groups = {}
  
  let mut i = 0
  while i < offline_data_collection.length() {
    let data_item = offline_data_collection[i]
    let priority = data_item.get("priority", "")
    let size_kb = data_item.get("size_kb", 0)
    
    used_storage_kb = used_storage_kb + size_kb
    
    if priority == "critical" {
      critical_data_kb = critical_data_kb + size_kb
    }
    
    # 按优先级分组
    let group = priority_groups.get(priority, [])
    priority_groups.set(priority, group + [data_item])
    
    i = i + 1
  }
  
  // 验证存储使用
  assert_eq(used_storage_kb, 63)  # 总共63KB
  assert_eq(critical_data_kb, 11)  # 关键数据11KB
  assert_eq(used_storage_kb < total_storage_kb, true)  # 未超出存储限制
  assert_eq(critical_data_kb < critical_buffer_kb, true)  # 关键数据缓冲区充足
  
  // 验证优先级分组
  assert_eq(priority_groups.get("critical", []).length(), 2)
  assert_eq(priority_groups.get("high", []).length(), 2)
  assert_eq(priority_groups.get("normal", []).length(), 2)
  assert_eq(priority_groups.get("low", []).length(), 2)
  
  // 模拟存储空间不足时的数据淘汰策略
  let simulated_storage_limit_kb = 50  # 模拟较小的存储限制
  let mut eviction_candidates = []
  
  if used_storage_kb > simulated_storage_limit_kb {
    # 按优先级和时间排序数据
    let priority_order = ["low", "normal", "high", "critical"]
    
    let mut priority_index = 0
    while used_storage_kb > simulated_storage_limit_kb and priority_index < priority_order.length() {
      let current_priority = priority_order[priority_index]
      let priority_data = priority_groups.get(current_priority, [])
      
      # 按时间排序（最旧的优先淘汰）
      let mut sorted_by_time = priority_data
      let mut j = 0
      while j < sorted_by_time.length() - 1 {
        let mut k = j + 1
        while k < sorted_by_time.length() {
          if sorted_by_time[j].get("timestamp", 0) > sorted_by_time[k].get("timestamp", 0) {
            let temp = sorted_by_time[j]
            sorted_by_time[j] = sorted_by_time[k]
            sorted_by_time[k] = temp
          }
          k = k + 1
        }
        j = j + 1
      }
      
      # 淘汰数据直到存储空间足够
      let mut l = 0
      while l < sorted_by_time.length() and used_storage_kb > simulated_storage_limit_kb {
        let item_to_evict = sorted_by_time[l]
        let size_kb = item_to_evict.get("size_kb", 0)
        
        eviction_candidates.push(item_to_evict)
        used_storage_kb = used_storage_kb - size_kb
        
        # 更新优先级分组
        let updated_group = []
        let mut m = 0
        while m < priority_data.length() {
          if priority_data[m].get("id", "") != item_to_evict.get("id", "") {
            updated_group.push(priority_data[m])
          }
          m = m + 1
        }
        priority_groups.set(current_priority, updated_group)
        
        l = l + 1
      }
      
      priority_index = priority_index + 1
    }
  }
  
  // 验证淘汰结果
  assert_eq(eviction_candidates.length(), 3)  # 需要淘汰3个数据项
  assert_eq(used_storage_kb <= simulated_storage_limit_kb, true)  # 存储空间现在足够
  
  # 验证淘汰优先级（应该先淘汰低优先级数据）
  let evicted_priorities = []
  i = 0
  while i < eviction_candidates.length() {
    evicted_priorities.push(eviction_candidates[i].get("priority", ""))
    i = i + 1
  }
  
  assert_eq(evicted_priorities.contains("low"), true)    # 低优先级数据被淘汰
  assert_eq(evicted_priorities.contains("normal"), true) # 普通优先级数据被淘汰
  assert_eq(evicted_priorities.contains("critical"), false) # 关键数据不会被淘汰
  
  // 模拟网络重连后的数据同步
  let reconnect_timestamp = 1672531300
  let mut sync_queue = []
  
  # 按优先级排序待同步数据
  let priority_order = ["critical", "high", "normal", "low"]
  i = 0
  while i < priority_order.length() {
    let priority = priority_order[i]
    let priority_data = priority_groups.get(priority, [])
    
    let mut j = 0
    while j < priority_data.length() {
      sync_queue.push(priority_data[j])
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证同步队列
  assert_eq(sync_queue.length(), 5)  # 剩余5个数据项待同步
  
  # 验证同步队列优先级顺序
  assert_eq(sync_queue[0].get("priority", ""), "critical")  # 关键数据优先
  assert_eq(sync_queue[1].get("priority", ""), "critical")
  assert_eq(sync_queue[2].get("priority", ""), "high")     # 高优先级次之
  assert_eq(sync_queue[3].get("priority", ""), "normal")
  assert_eq(sync_queue[4].get("priority", ""), "high")     # 另一个高优先级数据
  
  // 计算离线期间的统计信息
  let offline_duration_seconds = reconnect_timestamp - offline_data_collection[0].get("timestamp", 0)
  let data_points_collected = offline_data_collection.length()
  let data_points_retained = sync_queue.length()
  let data_retention_rate = data_points_retained.to_double() / data_points_collected.to_double()
  
  // 验证离线统计
  assert_eq(offline_duration_seconds, 100)  # 100秒离线时间
  assert_eq(data_points_collected, 8)       # 收集了8个数据点
  assert_eq(data_points_retained, 5)        # 保留了5个数据点
  assert_eq(data_retention_rate, 0.625)     # 62.5%的数据保留率
}