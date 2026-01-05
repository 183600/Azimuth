// Azimuth 边缘计算与IoT遥测测试用例
// 专注于边缘计算环境和IoT设备的遥测数据处理功能

// 测试1: 边缘设备遥测数据采集
test "边缘设备遥测数据采集" {
  // 模拟多个IoT设备的遥测数据
  let iot_devices = [
    { 
      device_id: "sensor-001", 
      device_type: "temperature", 
      location: "warehouse-a", 
      battery_level: 85, 
      signal_strength: -45, 
      last_seen: 1640995200 
    },
    { 
      device_id: "sensor-002", 
      device_type: "humidity", 
      location: "warehouse-a", 
      battery_level: 92, 
      signal_strength: -52, 
      last_seen: 1640995210 
    },
    { 
      device_id: "sensor-003", 
      device_type: "temperature", 
      location: "warehouse-b", 
      battery_level: 67, 
      signal_strength: -68, 
      last_seen: 1640995180 
    },
    { 
      device_id: "sensor-004", 
      device_type: "motion", 
      location: "entrance", 
      battery_level: 45, 
      signal_strength: -75, 
      last_seen: 1640995150 
    }
  ]
  
  // 模拟传感器读数
  let sensor_readings = [
    { device_id: "sensor-001", timestamp: 1640995200, value: 23.5, unit: "celsius" },
    { device_id: "sensor-002", timestamp: 1640995205, value: 65.2, unit: "percent" },
    { device_id: "sensor-001", timestamp: 1640995210, value: 23.7, unit: "celsius" },
    { device_id: "sensor-003", timestamp: 1640995215, value: 19.8, unit: "celsius" },
    { device_id: "sensor-004", timestamp: 1640995220, value: 1, unit: "boolean" },
    { device_id: "sensor-002", timestamp: 1640995225, value: 66.1, unit: "percent" },
    { device_id: "sensor-001", timestamp: 1640995230, value: 23.6, unit: "celsius" },
    { device_id: "sensor-003", timestamp: 1640995235, value: 20.1, unit: "celsius" }
  ]
  
  // 按设备类型分组
  let mut temperature_sensors = []
  let mut humidity_sensors = []
  let mut motion_sensors = []
  
  for device in iot_devices {
    match device.device_type {
      "temperature" => temperature_sensors = temperature_sensors.push(device)
      "humidity" => humidity_sensors = humidity_sensors.push(device)
      "motion" => motion_sensors = motion_sensors.push(device)
      _ => ()
    }
  }
  
  // 验证设备类型分组
  assert_eq(temperature_sensors.length(), 2)
  assert_eq(humidity_sensors.length(), 1)
  assert_eq(motion_sensors.length(), 1)
  
  // 按位置分组
  let mut warehouse_a_devices = []
  let mut warehouse_b_devices = []
  let mut entrance_devices = []
  
  for device in iot_devices {
    match device.location {
      "warehouse-a" => warehouse_a_devices = warehouse_a_devices.push(device)
      "warehouse-b" => warehouse_b_devices = warehouse_b_devices.push(device)
      "entrance" => entrance_devices = entrance_devices.push(device)
      _ => ()
    }
  }
  
  // 验证位置分组
  assert_eq(warehouse_a_devices.length(), 2)
  assert_eq(warehouse_b_devices.length(), 1)
  assert_eq(entrance_devices.length(), 1)
  
  // 识别低电量设备
  let low_battery_threshold = 50
  let mut low_battery_devices = []
  
  for device in iot_devices {
    if device.battery_level < low_battery_threshold {
      low_battery_devices = low_battery_devices.push({
        device_id: device.device_id,
        battery_level: device.battery_level,
        location: device.location
      })
    }
  }
  
  // 验证低电量设备识别
  assert_eq(low_battery_devices.length(), 1)
  assert_eq(low_battery_devices[0].device_id, "sensor-004")
  assert_eq(low_battery_devices[0].battery_level, 45)
  
  // 识别信号弱设备
  let weak_signal_threshold = -70
  let mut weak_signal_devices = []
  
  for device in iot_devices {
    if device.signal_strength < weak_signal_threshold {
      weak_signal_devices = weak_signal_devices.push({
        device_id: device.device_id,
        signal_strength: device.signal_strength,
        location: device.location
      })
    }
  }
  
  // 验证信号弱设备识别
  assert_eq(weak_signal_devices.length(), 2)
  assert_true(weak_signal_devices[0].device_id == "sensor-003" || 
              weak_signal_devices[0].device_id == "sensor-004")
  assert_true(weak_signal_devices[1].device_id == "sensor-003" || 
              weak_signal_devices[1].device_id == "sensor-004")
  
  // 计算传感器读数统计
  let calculate_sensor_stats = fn(readings: Array[SensorReading>, device_id: String) {
    let device_readings = readings.filter(fn(reading) { reading.device_id == device_id })
    
    if device_readings.length() == 0 {
      return {
        count: 0,
        avg_value: 0.0,
        min_value: 0.0,
        max_value: 0.0
      }
    }
    
    let mut sum = 0.0
    let mut min_value = device_readings[0].value.to_float()
    let mut max_value = device_readings[0].value.to_float()
    
    for reading in device_readings {
      let value = reading.value.to_float()
      sum = sum + value
      
      if value < min_value {
        min_value = value
      }
      
      if value > max_value {
        max_value = value
      }
    }
    
    {
      count: device_readings.length(),
      avg_value: sum / device_readings.length().to_float(),
      min_value: min_value,
      max_value: max_value
    }
  }
  
  // 计算各传感器统计
  let sensor_001_stats = calculate_sensor_stats(sensor_readings, "sensor-001")
  let sensor_002_stats = calculate_sensor_stats(sensor_readings, "sensor-002")
  let sensor_003_stats = calculate_sensor_stats(sensor_readings, "sensor-003")
  let sensor_004_stats = calculate_sensor_stats(sensor_readings, "sensor-004")
  
  // 验证传感器统计
  assert_eq(sensor_001_stats.count, 3)
  assert_eq(sensor_001_stats.avg_value, 23.6)  // (23.5+23.7+23.6)/3 = 23.6
  assert_eq(sensor_001_stats.min_value, 23.5)
  assert_eq(sensor_001_stats.max_value, 23.7)
  
  assert_eq(sensor_002_stats.count, 2)
  assert_eq(sensor_002_stats.avg_value, 65.65)  // (65.2+66.1)/2 = 65.65
  assert_eq(sensor_002_stats.min_value, 65.2)
  assert_eq(sensor_002_stats.max_value, 66.1)
  
  assert_eq(sensor_003_stats.count, 2)
  assert_eq(sensor_003_stats.avg_value, 19.95)  // (19.8+20.1)/2 = 19.95
  assert_eq(sensor_003_stats.min_value, 19.8)
  assert_eq(sensor_003_stats.max_value, 20.1)
  
  assert_eq(sensor_004_stats.count, 1)
  assert_eq(sensor_004_stats.avg_value, 1.0)
  assert_eq(sensor_004_stats.min_value, 1.0)
  assert_eq(sensor_004_stats.max_value, 1.0)
}

// 测试2: 边缘计算数据处理
test "边缘计算数据处理" {
  // 模拟边缘节点的处理能力
  let edge_nodes = [
    { 
      node_id: "edge-node-001", 
      location: "warehouse-a", 
      cpu_cores: 4, 
      memory_gb: 8, 
      storage_gb: 128, 
      network_bandwidth_mbps: 100,
      active_devices: 5,
      processing_load: 65
    },
    { 
      node_id: "edge-node-002", 
      location: "warehouse-b", 
      cpu_cores: 2, 
      memory_gb: 4, 
      storage_gb: 64, 
      network_bandwidth_mbps: 50,
      active_devices: 3,
      processing_load: 45
    },
    { 
      node_id: "edge-node-003", 
      location: "entrance", 
      cpu_cores: 8, 
      memory_gb: 16, 
      storage_gb: 256, 
      network_bandwidth_mbps: 200,
      active_devices: 8,
      processing_load: 35
    }
  ]
  
  // 模拟边缘处理任务
  let processing_tasks = [
    { 
      task_id: "task-001", 
      node_id: "edge-node-001", 
      task_type: "data-aggregation", 
      input_size_mb: 10, 
      output_size_mb: 2, 
      processing_time_ms: 150,
      cpu_usage: 15,
      memory_usage_mb: 256
    },
    { 
      task_id: "task-002", 
      node_id: "edge-node-001", 
      task_type: "anomaly-detection", 
      input_size_mb: 5, 
      output_size_mb: 1, 
      processing_time_ms: 300,
      cpu_usage: 25,
      memory_usage_mb: 512
    },
    { 
      task_id: "task-003", 
      node_id: "edge-node-002", 
      task_type: "data-filtering", 
      input_size_mb: 8, 
      output_size_mb: 3, 
      processing_time_ms: 120,
      cpu_usage: 10,
      memory_usage_mb: 128
    },
    { 
      task_id: "task-004", 
      node_id: "edge-node-003", 
      task_type: "ml-inference", 
      input_size_mb: 20, 
      output_size_mb: 5, 
      processing_time_ms: 500,
      cpu_usage: 40,
      memory_usage_mb: 2048
    }
  ]
  
  // 计算每个节点的资源利用率
  let calculate_node_utilization = fn(node: EdgeNode, tasks: Array[ProcessingTask>) {
    let node_tasks = tasks.filter(fn(task) { task.node_id == node.node_id })
    
    let mut total_cpu_usage = 0
    let mut total_memory_usage = 0
    let mut total_processing_time = 0
    let mut total_input_size = 0
    let mut total_output_size = 0
    
    for task in node_tasks {
      total_cpu_usage = total_cpu_usage + task.cpu_usage
      total_memory_usage = total_memory_usage + task.memory_usage_mb
      total_processing_time = total_processing_time + task.processing_time_ms
      total_input_size = total_input_size + task.input_size_mb
      total_output_size = total_output_size + task.output_size_mb
    }
    
    let cpu_utilization = if node.cpu_cores > 0 {
      (total_cpu_usage.to_float() / (node.cpu_cores * 100.0)) * 100.0
    } else {
      0.0
    }
    
    let memory_utilization = if node.memory_gb > 0 {
      (total_memory_usage.to_float() / (node.memory_gb * 1024.0)) * 100.0
    } else {
      0.0
    }
    
    let compression_ratio = if total_input_size > 0 {
      (total_output_size.to_float() / total_input_size.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      node_id: node.node_id,
      task_count: node_tasks.length(),
      cpu_utilization: cpu_utilization,
      memory_utilization: memory_utilization,
      avg_processing_time: if node_tasks.length() > 0 {
        total_processing_time / node_tasks.length()
      } else {
        0
      },
      compression_ratio: compression_ratio,
      total_data_processed: total_input_size
    }
  }
  
  // 计算各节点利用率
  let mut node_utilizations = []
  
  for node in edge_nodes {
    node_utilizations = node_utilizations.push(calculate_node_utilization(node, processing_tasks))
  }
  
  // 验证节点利用率计算
  let node_001_util = node_utilizations[0]
  let node_002_util = node_utilizations[1]
  let node_003_util = node_utilizations[2]
  
  assert_eq(node_001_util.node_id, "edge-node-001")
  assert_eq(node_001_util.task_count, 2)
  assert_eq(node_001_util.cpu_utilization, 10.0)  // (15+25)/(4*100) * 100 = 10%
  assert_eq(node_001_util.memory_utilization, 9.38)  // (256+512)/(8*1024) * 100 ≈ 9.38%
  assert_eq(node_001_util.avg_processing_time, 225)  // (150+300)/2 = 225
  assert_eq(node_001_util.compression_ratio, 30.0)  // (2+1)/(10+5) * 100 = 20%
  
  assert_eq(node_002_util.node_id, "edge-node-002")
  assert_eq(node_002_util.task_count, 1)
  assert_eq(node_002_util.cpu_utilization, 5.0)  // 10/(2*100) * 100 = 5%
  assert_eq(node_002_util.memory_utilization, 3.13)  // 128/(4*1024) * 100 ≈ 3.13%
  assert_eq(node_002_util.avg_processing_time, 120)
  assert_eq(node_002_util.compression_ratio, 37.5)  // 3/8 * 100 = 37.5%
  
  assert_eq(node_003_util.node_id, "edge-node-003")
  assert_eq(node_003_util.task_count, 1)
  assert_eq(node_003_util.cpu_utilization, 5.0)  // 40/(8*100) * 100 = 5%
  assert_eq(node_003_util.memory_utilization, 12.5)  // 2048/(16*1024) * 100 = 12.5%
  assert_eq(node_003_util.avg_processing_time, 500)
  assert_eq(node_003_util.compression_ratio, 25.0)  // 5/20 * 100 = 25%
  
  // 识别高负载节点
  let high_load_threshold = 60
  let mut high_load_nodes = []
  
  for node in edge_nodes {
    if node.processing_load > high_load_threshold {
      high_load_nodes = high_load_nodes.push({
        node_id: node.node_id,
        location: node.location,
        processing_load: node.processing_load
      })
    }
  }
  
  // 验证高负载节点识别
  assert_eq(high_load_nodes.length(), 1)
  assert_eq(high_load_nodes[0].node_id, "edge-node-001")
  assert_eq(high_load_nodes[0].location, "warehouse-a")
  assert_eq(high_load_nodes[0].processing_load, 65)
  
  // 计算任务类型性能指标
  let calculate_task_type_metrics = fn(tasks: Array[ProcessingTask>, task_type: String) {
    let type_tasks = tasks.filter(fn(task) { task.task_type == task_type })
    
    if type_tasks.length() == 0 {
      return {
        count: 0,
        avg_processing_time: 0,
        avg_cpu_usage: 0,
        avg_memory_usage: 0,
        throughput_mb_per_sec: 0.0
      }
    }
    
    let mut total_processing_time = 0
    let mut total_cpu_usage = 0
    let mut total_memory_usage = 0
    let mut total_input_size = 0
    
    for task in type_tasks {
      total_processing_time = total_processing_time + task.processing_time_ms
      total_cpu_usage = total_cpu_usage + task.cpu_usage
      total_memory_usage = total_memory_usage + task.memory_usage_mb
      total_input_size = total_input_size + task.input_size_mb
    }
    
    let total_processing_time_sec = total_processing_time.to_float() / 1000.0
    let throughput_mb_per_sec = if total_processing_time_sec > 0.0 {
      total_input_size.to_float() / total_processing_time_sec
    } else {
      0.0
    }
    
    {
      count: type_tasks.length(),
      avg_processing_time: total_processing_time / type_tasks.length(),
      avg_cpu_usage: total_cpu_usage / type_tasks.length(),
      avg_memory_usage: total_memory_usage / type_tasks.length(),
      throughput_mb_per_sec: throughput_mb_per_sec
    }
  }
  
  // 计算各任务类型性能指标
  let aggregation_metrics = calculate_task_type_metrics(processing_tasks, "data-aggregation")
  let anomaly_detection_metrics = calculate_task_type_metrics(processing_tasks, "anomaly-detection")
  let filtering_metrics = calculate_task_type_metrics(processing_tasks, "data-filtering")
  let ml_inference_metrics = calculate_task_type_metrics(processing_tasks, "ml-inference")
  
  // 验证任务类型性能指标
  assert_eq(aggregation_metrics.count, 1)
  assert_eq(aggregation_metrics.avg_processing_time, 150)
  assert_eq(aggregation_metrics.avg_cpu_usage, 15)
  assert_eq(aggregation_metrics.avg_memory_usage, 256)
  assert_eq(aggregation_metrics.throughput_mb_per_sec, 66.67)  // 10MB / 0.15s = 66.67 MB/s
  
  assert_eq(anomaly_detection_metrics.count, 1)
  assert_eq(anomaly_detection_metrics.avg_processing_time, 300)
  assert_eq(anomaly_detection_metrics.avg_cpu_usage, 25)
  assert_eq(anomaly_detection_metrics.avg_memory_usage, 512)
  assert_eq(anomaly_detection_metrics.throughput_mb_per_sec, 16.67)  // 5MB / 0.3s = 16.67 MB/s
  
  assert_eq(filtering_metrics.count, 1)
  assert_eq(filtering_metrics.avg_processing_time, 120)
  assert_eq(filtering_metrics.avg_cpu_usage, 10)
  assert_eq(filtering_metrics.avg_memory_usage, 128)
  assert_eq(filtering_metrics.throughput_mb_per_sec, 66.67)  // 8MB / 0.12s = 66.67 MB/s
  
  assert_eq(ml_inference_metrics.count, 1)
  assert_eq(ml_inference_metrics.avg_processing_time, 500)
  assert_eq(ml_inference_metrics.avg_cpu_usage, 40)
  assert_eq(ml_inference_metrics.avg_memory_usage, 2048)
  assert_eq(ml_inference_metrics.throughput_mb_per_sec, 40.0)  // 20MB / 0.5s = 40.0 MB/s
}

// 测试3: 边缘与云端数据同步
test "边缘与云端数据同步" {
  // 模拟边缘节点数据同步状态
  let sync_status = [
    { 
      node_id: "edge-node-001", 
      last_sync_time: 1640995200, 
      pending_data_mb: 15, 
      sync_success_rate: 0.95, 
      avg_sync_latency_ms: 250,
      sync_errors: 2
    },
    { 
      node_id: "edge-node-002", 
      last_sync_time: 1640995180, 
      pending_data_mb: 45, 
      sync_success_rate: 0.88, 
      avg_sync_latency_ms: 450,
      sync_errors: 8
    },
    { 
      node_id: "edge-node-003", 
      last_sync_time: 1640995210, 
      pending_data_mb: 5, 
      sync_success_rate: 0.99, 
      avg_sync_latency_ms: 180,
      sync_errors: 1
    }
  ]
  
  // 模拟同步策略配置
  let sync_strategies = [
    { 
      strategy_name: "real-time", 
      data_threshold_mb: 5, 
      sync_interval_seconds: 60, 
      compression_enabled: true, 
      encryption_enabled: true,
      bandwidth_limit_mbps: 10
    },
    { 
      strategy_name: "batch", 
      data_threshold_mb: 50, 
      sync_interval_seconds: 300, 
      compression_enabled: true, 
      encryption_enabled: true,
      bandwidth_limit_mbps: 20
    },
    { 
      strategy_name: "on-demand", 
      data_threshold_mb: 100, 
      sync_interval_seconds: 0, 
      compression_enabled: false, 
      encryption_enabled: true,
      bandwidth_limit_mbps: 50
    }
  ]
  
  // 当前时间
  let current_time = 1640995300
  
  // 计算同步延迟
  let calculate_sync_delay = fn(last_sync_time: Int, current_time: Int) {
    current_time - last_sync_time
  }
  
  // 识别需要同步的节点
  let identify_nodes_needing_sync = fn(sync_data: Array[SyncStatus>, strategies: Array[SyncStrategy>, current_time: Int) {
    let mut nodes_needing_sync = []
    
    for status in sync_data {
      let sync_delay = calculate_sync_delay(status.last_sync_time, current_time)
      
      // 检查各种同步策略
      for strategy in strategies {
        let needs_sync = 
          // 数据量超过阈值
          (status.pending_data_mb >= strategy.data_threshold_mb) or
          // 超过同步间隔
          (strategy.sync_interval_seconds > 0 and sync_delay >= strategy.sync_interval_seconds) or
          // 实时策略且有待处理数据
          (strategy.strategy_name == "real-time" and status.pending_data_mb > 0)
        
        if needs_sync {
          nodes_needing_sync = nodes_needing_sync.push({
            node_id: status.node_id,
            strategy: strategy.strategy_name,
            reason: if status.pending_data_mb >= strategy.data_threshold_mb {
              "data_threshold"
            } else if strategy.sync_interval_seconds > 0 and sync_delay >= strategy.sync_interval_seconds {
              "time_interval"
            } else {
              "real_time_policy"
            },
            pending_data_mb: status.pending_data_mb,
            sync_delay_seconds: sync_delay
          })
          break  // 找到一个匹配的策略后跳出
        }
      }
    }
    
    nodes_needing_sync
  }
  
  // 识别需要同步的节点
  let nodes_needing_sync = identify_nodes_needing_sync(sync_status, sync_strategies, current_time)
  
  // 验证同步需求识别
  assert_eq(nodes_needing_sync.length(), 3)  // 所有节点都需要同步
  
  // 验证每个节点的同步原因
  let node_001_sync = nodes_needing_sync.find(fn(sync) { sync.node_id == "edge-node-001" })
  let node_002_sync = nodes_needing_sync.find(fn(sync) { sync.node_id == "edge-node-002" })
  let node_003_sync = nodes_needing_sync.find(fn(sync) { sync.node_id == "edge-node-003" })
  
  assert_true(node_001_sync.is_some())
  assert_true(node_002_sync.is_some())
  assert_true(node_003_sync.is_some())
  
  // edge-node-001: 有15MB待处理数据，超过实时策略阈值(5MB)，需要实时同步
  assert_eq(node_001_sync.unwrap().strategy, "real-time")
  assert_eq(node_001_sync.unwrap().reason, "data_threshold")
  
  // edge-node-002: 有45MB待处理数据，超过实时策略阈值(5MB)，需要实时同步
  assert_eq(node_002_sync.unwrap().strategy, "real-time")
  assert_eq(node_002_sync.unwrap().reason, "data_threshold")
  
  // edge-node-003: 有5MB待处理数据，达到实时策略阈值(5MB)，需要实时同步
  assert_eq(node_003_sync.unwrap().strategy, "real-time")
  assert_eq(node_003_sync.unwrap().reason, "data_threshold")
  
  // 计算同步性能指标
  let calculate_sync_performance = fn(sync_data: Array[SyncStatus>) {
    let mut total_success_rate = 0.0
    let mut total_avg_latency = 0
    let mut total_errors = 0
    let mut total_pending_data = 0
    
    for status in sync_data {
      total_success_rate = total_success_rate + status.sync_success_rate
      total_avg_latency = total_avg_latency + status.avg_sync_latency_ms
      total_errors = total_errors + status.sync_errors
      total_pending_data = total_pending_data + status.pending_data_mb
    }
    
    let overall_success_rate = total_success_rate / sync_data.length().to_float()
    let overall_avg_latency = total_avg_latency / sync_data.length()
    let error_rate = if total_errors > 0 {
      (total_errors.to_float() / (total_errors + sync_data.length() * 10).to_float()) * 100.0  // 简化计算
    } else {
      0.0
    }
    
    {
      overall_success_rate: overall_success_rate,
      overall_avg_latency: overall_avg_latency,
      total_errors: total_errors,
      total_pending_data: total_pending_data,
      error_rate: error_rate
    }
  }
  
  // 计算同步性能指标
  let sync_performance = calculate_sync_performance(sync_status)
  
  // 验证同步性能指标
  assert_eq(sync_performance.overall_success_rate, 0.94)  // (0.95+0.88+0.99)/3 = 0.94
  assert_eq(sync_performance.overall_avg_latency, 293)   // (250+450+180)/3 = 293.33，取整数部分
  assert_eq(sync_performance.total_errors, 11)
  assert_eq(sync_performance.total_pending_data, 65)
  assert_true(sync_performance.error_rate > 0.0)
  
  // 识别同步问题节点
  let identify_problematic_nodes = fn(sync_data: Array[SyncStatus>) {
    let low_success_rate_threshold = 0.9
    let high_latency_threshold = 400
    let high_pending_data_threshold = 30
    
    let mut problematic_nodes = []
    
    for status in sync_data {
      let issues = []
      
      if status.sync_success_rate < low_success_rate_threshold {
        issues = issues.push("low_success_rate")
      }
      
      if status.avg_sync_latency_ms > high_latency_threshold {
        issues = issues.push("high_latency")
      }
      
      if status.pending_data_mb > high_pending_data_threshold {
        issues = issues.push("high_pending_data")
      }
      
      if status.sync_errors > 5 {
        issues = issues.push("frequent_errors")
      }
      
      if issues.length() > 0 {
        problematic_nodes = problematic_nodes.push({
          node_id: status.node_id,
          issues: issues,
          sync_success_rate: status.sync_success_rate,
          avg_sync_latency_ms: status.avg_sync_latency_ms,
          pending_data_mb: status.pending_data_mb,
          sync_errors: status.sync_errors
        })
      }
    }
    
    problematic_nodes
  }
  
  // 识别问题节点
  let problematic_nodes = identify_problematic_nodes(sync_status)
  
  // 验证问题节点识别
  assert_eq(problematic_nodes.length(), 1)
  assert_eq(problematic_nodes[0].node_id, "edge-node-002")
  assert_true(problematic_nodes[0].issues.contains("low_success_rate"))
  assert_true(problematic_nodes[0].issues.contains("high_latency"))
  assert_true(problematic_nodes[0].issues.contains("high_pending_data"))
  assert_true(problematic_nodes[0].issues.contains("frequent_errors"))
  
  // 计算同步优化建议
  let generate_sync_recommendations = fn(problematic_nodes: Array[ProblematicNode>) {
    let mut recommendations = []
    
    for node in problematic_nodes {
      let mut node_recommendations = []
      
      for issue in node.issues {
        match issue {
          "low_success_rate" => {
            node_recommendations = node_recommendations.push("Increase retry attempts and implement exponential backoff")
          }
          "high_latency" => {
            node_recommendations = node_recommendations.push("Optimize network configuration or consider edge-to-edge sync")
          }
          "high_pending_data" => {
            node_recommendations = node_recommendations.push("Increase sync frequency or enable data compression")
          }
          "frequent_errors" => {
            node_recommendations = node_recommendations.push("Investigate root cause of sync failures and implement error handling")
          }
          _ => {}
        }
      }
      
      recommendations = recommendations.push({
        node_id: node.node_id,
        recommendations: node_recommendations
      })
    }
    
    recommendations
  }
  
  // 生成同步优化建议
  let sync_recommendations = generate_sync_recommendations(problematic_nodes)
  
  // 验证同步优化建议
  assert_eq(sync_recommendations.length(), 1)
  assert_eq(sync_recommendations[0].node_id, "edge-node-002")
  assert_eq(sync_recommendations[0].recommendations.length(), 4)
}

// 测试4: 边缘设备故障检测
test "边缘设备故障检测" {
  // 模拟边缘设备健康状态数据
  let device_health_data = [
    { 
      device_id: "sensor-001", 
      timestamp: 1640995200, 
      cpu_usage: 45.0, 
      memory_usage: 60.0, 
      temperature: 35.5, 
      battery_voltage: 3.7,
      network_errors: 0,
      restart_count: 0,
      status: "healthy"
    },
    { 
      device_id: "sensor-001", 
      timestamp: 1640995260, 
      cpu_usage: 48.0, 
      memory_usage: 62.0, 
      temperature: 36.2, 
      battery_voltage: 3.68,
      network_errors: 0,
      restart_count: 0,
      status: "healthy"
    },
    { 
      device_id: "sensor-002", 
      timestamp: 1640995200, 
      cpu_usage: 75.0, 
      memory_usage: 85.0, 
      temperature: 42.5, 
      battery_voltage: 3.65,
      network_errors: 2,
      restart_count: 0,
      status: "degraded"
    },
    { 
      device_id: "sensor-002", 
      timestamp: 1640995260, 
      cpu_usage: 85.0, 
      memory_usage: 90.0, 
      temperature: 45.8, 
      battery_voltage: 3.62,
      network_errors: 5,
      restart_count: 1,
      status: "critical"
    },
    { 
      device_id: "sensor-003", 
      timestamp: 1640995200, 
      cpu_usage: 95.0, 
      memory_usage: 98.0, 
      temperature: 55.2, 
      battery_voltage: 3.5,
      network_errors: 10,
      restart_count: 3,
      status: "failed"
    }
  ]
  
  // 故障检测规则
  let detect_device_faults = fn(data: Array[DeviceHealthData>) {
    let mut fault_indicators = []
    
    // 按设备分组
    let device_ids = ["sensor-001", "sensor-002", "sensor-003"]
    
    for device_id in device_ids {
      let device_data = data.filter(fn(d) { d.device_id == device_id })
      
      if device_data.length() > 0 {
        let latest_data = device_data[device_data.length() - 1]
        
        let mut faults = []
        
        // CPU使用率检查
        if latest_data.cpu_usage > 80.0 {
          faults = faults.push({
            type: "high_cpu_usage",
            severity: if latest_data.cpu_usage > 95.0 { "critical" } else { "warning" },
            value: latest_data.cpu_usage,
            threshold: 80.0
          })
        }
        
        // 内存使用率检查
        if latest_data.memory_usage > 85.0 {
          faults = faults.push({
            type: "high_memory_usage",
            severity: if latest_data.memory_usage > 95.0 { "critical" } else { "warning" },
            value: latest_data.memory_usage,
            threshold: 85.0
          })
        }
        
        // 温度检查
        if latest_data.temperature > 45.0 {
          faults = faults.push({
            type: "high_temperature",
            severity: if latest_data.temperature > 55.0 { "critical" } else { "warning" },
            value: latest_data.temperature,
            threshold: 45.0
          })
        }
        
        // 电池电压检查
        if latest_data.battery_voltage < 3.6 {
          faults = faults.push({
            type: "low_battery",
            severity: if latest_data.battery_voltage < 3.5 { "critical" } else { "warning" },
            value: latest_data.battery_voltage,
            threshold: 3.6
          })
        }
        
        // 网络错误检查
        if latest_data.network_errors > 3 {
          faults = faults.push({
            type: "network_errors",
            severity: if latest_data.network_errors > 8 { "critical" } else { "warning" },
            value: latest_data.network_errors,
            threshold: 3
          })
        }
        
        // 重启次数检查
        if latest_data.restart_count > 2 {
          faults = faults.push({
            type: "frequent_restarts",
            severity: "critical",
            value: latest_data.restart_count,
            threshold: 2
          })
        }
        
        if faults.length() > 0 {
          fault_indicators = fault_indicators.push({
            device_id: device_id,
            status: latest_data.status,
            faults: faults,
            fault_count: faults.length()
          })
        }
      }
    }
    
    fault_indicators
  }
  
  // 执行故障检测
  let fault_indicators = detect_device_faults(device_health_data)
  
  // 验证故障检测结果
  assert_eq(fault_indicators.length(), 2)  // sensor-002和sensor-003有故障
  
  // 验证sensor-002的故障
  let sensor_002_faults = fault_indicators.find(fn(indicator) { indicator.device_id == "sensor-002" })
  assert_true(sensor_002_faults.is_some())
  assert_eq(sensor_002_faults.unwrap().status, "critical")
  assert_eq(sensor_002_faults.unwrap().fault_count, 4)
  
  // 验证sensor-003的故障
  let sensor_003_faults = fault_indicators.find(fn(indicator) { indicator.device_id == "sensor-003" })
  assert_true(sensor_003_faults.is_some())
  assert_eq(sensor_003_faults.unwrap().status, "failed")
  assert_eq(sensor_003_faults.unwrap().fault_count, 5)
  
  // 计算故障严重程度
  let calculate_fault_severity = fn(faults: Array[Fault>) {
    let mut critical_count = 0
    let mut warning_count = 0
    
    for fault in faults {
      match fault.severity {
        "critical" => critical_count = critical_count + 1
        "warning" => warning_count = warning_count + 1
        _ => {}
      }
    }
    
    let overall_severity = 
      if critical_count > 2 { "critical" }
      else if critical_count > 0 { "high" }
      else if warning_count > 3 { "medium" }
      else if warning_count > 0 { "low" }
      else { "healthy" }
    
    {
      critical_count: critical_count,
      warning_count: warning_count,
      overall_severity: overall_severity
    }
  }
  
  // 计算各设备的故障严重程度
  let mut device_severities = []
  
  for indicator in fault_indicators {
    let severity = calculate_fault_severity(indicator.faults)
    
    device_severities = device_severities.push({
      device_id: indicator.device_id,
      status: indicator.status,
      critical_count: severity.critical_count,
      warning_count: severity.warning_count,
      overall_severity: severity.overall_severity
    })
  }
  
  // 验证故障严重程度计算
  assert_eq(device_severities.length(), 2)
  
  // sensor-002应该有1个严重故障和3个警告
  let sensor_002_severity = device_severities.find_fn(severity) { severity.device_id == "sensor-002" }
  assert_true(sensor_002_severity.is_some())
  assert_eq(sensor_002_severity.unwrap().overall_severity, "high")
  
  // sensor-003应该有4个严重故障和1个警告
  let sensor_003_severity = device_severities.find_fn(severity) { severity.device_id == "sensor-003" }
  assert_true(sensor_003_severity.is_some())
  assert_eq(sensor_003_severity.unwrap().overall_severity, "critical")
  
  // 生成故障恢复建议
  let generate_recovery_recommendations = fn(fault_indicators: Array[FaultIndicator>) {
    let mut recommendations = []
    
    for indicator in fault_indicators {
      let mut device_recommendations = []
      
      for fault in indicator.faults {
        match fault.type {
          "high_cpu_usage" => {
            device_recommendations = device_recommendations.push("Reduce processing load or optimize CPU-intensive tasks")
          }
          "high_memory_usage" => {
            device_recommendations = device_recommendations.push("Free up memory or increase memory capacity")
          }
          "high_temperature" => {
            device_recommendations = device_recommendations.push("Improve cooling or reduce workload")
          }
          "low_battery" => {
            device_recommendations = device_recommendations.push("Replace battery or implement power-saving measures")
          }
          "network_errors" => {
            device_recommendations = device_recommendations.push("Check network connectivity and signal strength")
          }
          "frequent_restarts" => {
            device_recommendations = device_recommendations.push("Investigate root cause of crashes and update firmware")
          }
          _ => {}
        }
      }
      
      // 去重
      let unique_recommendations = []
      for rec in device_recommendations {
        if not unique_recommendations.contains(rec) {
          unique_recommendations = unique_recommendations.push(rec)
        }
      }
      
      recommendations = recommendations.push({
        device_id: indicator.device_id,
        severity: indicator.status,
        recommendations: unique_recommendations
      })
    }
    
    recommendations
  }
  
  // 生成故障恢复建议
  let recovery_recommendations = generate_recovery_recommendations(fault_indicators)
  
  // 验证故障恢复建议
  assert_eq(recovery_recommendations.length(), 2)
  
  // 验证sensor-002的建议
  let sensor_002_recommendations = recovery_recommendations.find_fn(rec) { rec.device_id == "sensor-002" }
  assert_true(sensor_002_recommendations.is_some())
  assert_eq(sensor_002_recommendations.unwrap().severity, "critical")
  assert_eq(sensor_002_recommendations.unwrap().recommendations.length(), 4)
  
  // 验证sensor-003的建议
  let sensor_003_recommendations = recovery_recommendations.find_fn(rec) { rec.device_id == "sensor-003" }
  assert_true(sensor_003_recommendations.is_some())
  assert_eq(sensor_003_recommendations.unwrap().severity, "failed")
  assert_eq(sensor_003_recommendations.unwrap().recommendations.length(), 5)
}