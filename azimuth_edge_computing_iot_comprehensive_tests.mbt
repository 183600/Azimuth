// Azimuth 边缘计算IoT遥测测试用例
// 专注于测试边缘设备和IoT环境下的遥测数据收集、处理和传输

// 测试1: 边缘设备资源约束处理
test "边缘设备资源约束处理" {
  // 模拟边缘设备的资源限制
  let edge_device_constraints = {
    "device1": { cpu_cores: 2, memory_mb: 512, storage_mb: 2048, network_kbps: 1000 },
    "device2": { cpu_cores: 4, memory_mb: 1024, storage_mb: 4096, network_kbps: 2000 },
    "device3": { cpu_cores: 1, memory_mb: 256, storage_mb: 1024, network_kbps: 500 }
  }
  
  // 模拟遥测数据处理任务的资源需求
  let telemetry_tasks = [
    { name: "数据收集", cpu_usage: 10, memory_usage: 50, duration_ms: 100 },
    { name: "数据压缩", cpu_usage: 30, memory_usage: 100, duration_ms: 200 },
    { name: "异常检测", cpu_usage: 50, memory_usage: 200, duration_ms: 300 },
    { name: "数据传输", cpu_usage: 5, memory_usage: 20, duration_ms: 500 }
  ]
  
  // 检查设备是否可以运行特定任务
  func can_run_task(device_id : String, task : { name : String, cpu_usage : Int, memory_usage : Int, duration_ms : Int }, constraints : Map[String, { cpu_cores : Int, memory_mb : Int, storage_mb : Int, network_kbps : Int }]) -> Bool {
    match constraints.get(device_id) {
      Some(device) => {
        // 简化的资源检查（假设单个任务不会超过设备总资源）
        return task.cpu_usage <= 100 && task.memory_usage <= device.memory_mb
      }
      None => false
    }
  }
  
  // 测试任务调度
  let mut task_assignments = []
  
  for device_id in ["device1", "device2", "device3"] {
    let mut device_tasks = []
    
    for task in telemetry_tasks {
      if can_run_task(device_id, task, edge_device_constraints) {
        device_tasks = device_tasks.push(task.name)
      }
    }
    
    task_assignments = task_assignments.push({
      device_id: device_id,
      available_tasks: device_tasks
    })
  }
  
  // 验证任务分配结果
  assert_eq(task_assignments.length(), 3)
  
  // device1 (512MB内存) 应该可以运行大部分任务
  let device1_tasks = task_assignments[0].available_tasks
  assert_eq(device1_tasks.length(), 4)
  assert_true(device1_tasks.contains("数据收集"))
  assert_true(device1_tasks.contains("数据压缩"))
  assert_true(device1_tasks.contains("异常检测"))
  assert_true(device1_tasks.contains("数据传输"))
  
  // device2 (1024MB内存) 应该可以运行所有任务
  let device2_tasks = task_assignments[1].available_tasks
  assert_eq(device2_tasks.length(), 4)
  
  // device3 (256MB内存) 可能无法运行内存密集型任务
  let device3_tasks = task_assignments[2].available_tasks
  assert_eq(device3_tasks.length(), 4)
  
  // 测试资源使用率计算
  func calculate_resource_usage(device_id : String, tasks : Array[{ name : String, cpu_usage : Int, memory_usage : Int, duration_ms : Int }], constraints : Map[String, { cpu_cores : Int, memory_mb : Int, storage_mb : Int, network_kbps : Int }]) -> { cpu_usage_percent : Float, memory_usage_percent : Float } {
    match constraints.get(device_id) {
      Some(device) => {
        let mut total_cpu = 0
        let mut total_memory = 0
        
        for task in tasks {
          total_cpu = total_cpu + task.cpu_usage
          total_memory = total_memory + task.memory_usage
        }
        
        // 假设每个CPU核心代表100%的CPU能力
        let max_cpu = device.cpu_cores * 100
        
        return {
          cpu_usage_percent: total_cpu.to_float() / max_cpu.to_float() * 100.0,
          memory_usage_percent: total_memory.to_float() / device.memory_mb.to_float() * 100.0
        }
      }
      None => { cpu_usage_percent: 0.0, memory_usage_percent: 0.0 }
    }
  }
  
  let device1_usage = calculate_resource_usage("device1", telemetry_tasks, edge_device_constraints)
  assert_true(device1_usage.cpu_usage_percent <= 100.0)
  assert_true(device1_usage.memory_usage_percent <= 100.0)
  
  let device3_usage = calculate_resource_usage("device3", telemetry_tasks, edge_device_constraints)
  assert_true(device3_usage.memory_usage_percent > device1_usage.memory_usage_percent)  // device3内存更少，使用率更高
}

// 测试2: 边缘设备数据采集
test "边缘设备数据采集" {
  // 模拟IoT传感器数据
  let iot_sensor_data = [
    { sensor_id: "temp_001", sensor_type: "temperature", value: 25.5, unit: "C", timestamp: 1640995200, location: "room_a" },
    { sensor_id: "hum_001", sensor_type: "humidity", value: 65.2, unit: "%", timestamp: 1640995200, location: "room_a" },
    { sensor_id: "press_001", sensor_type: "pressure", value: 1013.25, unit: "hPa", timestamp: 1640995200, location: "room_a" },
    { sensor_id: "light_001", sensor_type: "light", value: 450.0, unit: "lux", timestamp: 1640995200, location: "room_a" },
    { sensor_id: "motion_001", sensor_type: "motion", value: 1.0, unit: "bool", timestamp: 1640995260, location: "room_a" }
  ]
  
  // 模拟数据采集频率配置
  let collection_configs = {
    "temperature": { interval_ms: 5000, enabled: true },    // 每5秒
    "humidity": { interval_ms: 10000, enabled: true },      // 每10秒
    "pressure": { interval_ms: 30000, enabled: true },      // 每30秒
    "light": { interval_ms: 1000, enabled: true },          // 每1秒
    "motion": { interval_ms: 100, enabled: true }           // 每100毫秒
  }
  
  // 按传感器类型分组数据
  let mut grouped_data = {}
  
  for sensor_data in iot_sensor_data {
    match grouped_data.get(sensor_data.sensor_type) {
      Some(data_list) => {
        grouped_data = grouped_data.set(sensor_data.sensor_type, data_list.push(sensor_data))
      }
      None => {
        grouped_data = grouped_data.set(sensor_data.sensor_type, [sensor_data])
      }
    }
  }
  
  // 验证数据分组
  assert_eq(grouped_data.size(), 5)
  
  // 检查温度传感器数据
  let temp_data = grouped_data.get("temperature").unwrap()
  assert_eq(temp_data.length(), 1)
  assert_eq(temp_data[0].sensor_id, "temp_001")
  assert_eq(temp_data[0].value, 25.5)
  
  // 检查湿度传感器数据
  let hum_data = grouped_data.get("humidity").unwrap()
  assert_eq(hum_data.length(), 1)
  assert_eq(hum_data[0].sensor_id, "hum_001")
  assert_eq(hum_data[0].value, 65.2)
  
  // 测试数据采集调度
  func get_collection_schedule(sensor_type : String, configs : Map[String, { interval_ms : Int, enabled : Bool }]) -> { interval_ms : Int, enabled : Bool } {
    match configs.get(sensor_type) {
      Some(config) => config,
      None => { interval_ms: 60000, enabled: false }  // 默认每分钟，禁用
    }
  }
  
  let temp_schedule = get_collection_schedule("temperature", collection_configs)
  assert_eq(temp_schedule.interval_ms, 5000)
  assert_true(temp_schedule.enabled)
  
  let motion_schedule = get_collection_schedule("motion", collection_configs)
  assert_eq(motion_schedule.interval_ms, 100)
  assert_true(motion_schedule.enabled)
  
  let unknown_schedule = get_collection_schedule("unknown", collection_configs)
  assert_eq(unknown_schedule.interval_ms, 60000)
  assert_false(unknown_schedule.enabled)
  
  // 测试数据质量检查
  func validate_sensor_data(sensor_data : { sensor_id : String, sensor_type : String, value : Float, unit : String, timestamp : Int, location : String }) -> { is_valid : Bool, issues : Array[String] } {
    let mut issues = []
    let mut is_valid = true
    
    // 检查值是否在合理范围内
    match sensor_data.sensor_type {
      "temperature" => {
        if sensor_data.value < -50.0 || sensor_data.value > 100.0 {
          issues = issues.push("温度值超出合理范围")
          is_valid = false
        }
      }
      "humidity" => {
        if sensor_data.value < 0.0 || sensor_data.value > 100.0 {
          issues = issues.push("湿度值超出合理范围")
          is_valid = false
        }
      }
      "pressure" => {
        if sensor_data.value < 800.0 || sensor_data.value > 1200.0 {
          issues = issues.push("气压值超出合理范围")
          is_valid = false
        }
      }
      "light" => {
        if sensor_data.value < 0.0 || sensor_data.value > 100000.0 {
          issues = issues.push("光照值超出合理范围")
          is_valid = false
        }
      }
      "motion" => {
        if sensor_data.value != 0.0 && sensor_data.value != 1.0 {
          issues = issues.push("运动传感器值应为0或1")
          is_valid = false
        }
      }
      _ => ()
    }
    
    // 检查时间戳是否合理
    let current_time = 1640996000  // 模拟当前时间
    if sensor_data.timestamp > current_time {
      issues = issues.push("时间戳在未来")
      is_valid = false
    }
    
    return { is_valid: is_valid, issues: issues }
  }
  
  // 验证传感器数据质量
  let temp_validation = validate_sensor_data(iot_sensor_data[0])
  assert_true(temp_validation.is_valid)
  assert_eq(temp_validation.issues.length(), 0)
  
  let motion_validation = validate_sensor_data(iot_sensor_data[4])
  assert_true(motion_validation.is_valid)
  assert_eq(motion_validation.issues.length(), 0)
  
  // 测试无效数据
  let invalid_temp_data = { sensor_id: "temp_002", sensor_type: "temperature", value: 150.0, unit: "C", timestamp: 1640995200, location: "room_b" }
  let invalid_temp_validation = validate_sensor_data(invalid_temp_data)
  assert_false(invalid_temp_validation.is_valid)
  assert_eq(invalid_temp_validation.issues.length(), 1)
  assert_true(invalid_temp_validation.issues[0].contains("温度值超出合理范围"))
}

// 测试3: 边缘设备数据预处理
test "边缘设备数据预处理" {
  // 模拟原始传感器数据
  let raw_sensor_data = [
    { timestamp: 1640995200, temperature: 25.5, humidity: 65.2, pressure: 1013.25 },
    { timestamp: 1640995260, temperature: 25.7, humidity: 65.0, pressure: 1013.20 },
    { timestamp: 1640995320, temperature: 25.6, humidity: 65.1, pressure: 1013.22 },
    { timestamp: 1640995380, temperature: 25.8, humidity: 64.9, pressure: 1013.18 },
    { timestamp: 1640995440, temperature: 25.9, humidity: 64.8, pressure: 1013.15 }
  ]
  
  // 数据平滑处理（移动平均）
  func smooth_data(data : Array[{ timestamp : Int, temperature : Float, humidity : Float, pressure : Float }], window_size : Int) -> Array[{ timestamp : Int, temperature : Float, humidity : Float, pressure : Float }] {
    let mut smoothed_data = []
    
    for i in range(window_size - 1, data.length()) {
      let mut temp_sum = 0.0
      let mut hum_sum = 0.0
      let mut press_sum = 0.0
      
      for j in range(i - window_size + 1, i + 1) {
        temp_sum = temp_sum + data[j].temperature
        hum_sum = hum_sum + data[j].humidity
        press_sum = press_sum + data[j].pressure
      }
      
      let smoothed_point = {
        timestamp: data[i].timestamp,
        temperature: temp_sum / window_size.to_float(),
        humidity: hum_sum / window_size.to_float(),
        pressure: press_sum / window_size.to_float()
      }
      
      smoothed_data = smoothed_data.push(smoothed_point)
    }
    
    return smoothed_data
  }
  
  // 应用3点移动平均
  let smoothed_data = smooth_data(raw_sensor_data, 3)
  
  // 验证平滑结果
  assert_eq(smoothed_data.length(), 3)  // 原始5个点，3点窗口，剩下3个点
  
  // 检查第一个平滑点（基于原始数据点0,1,2）
  assert_eq(smoothed_data[0].timestamp, 1640995320)
  assert_eq(smoothed_data[0].temperature, (25.5 + 25.7 + 25.6) / 3.0)
  assert_eq(smoothed_data[0].humidity, (65.2 + 65.0 + 65.1) / 3.0)
  assert_eq(smoothed_data[0].pressure, (1013.25 + 1013.20 + 1013.22) / 3.0)
  
  // 数据降采样（减少数据点数量）
  func downsample_data(data : Array[{ timestamp : Int, temperature : Float, humidity : Float, pressure : Float }], factor : Int) -> Array[{ timestamp : Int, temperature : Float, humidity : Float, pressure : Float }] {
    let mut downsampled_data = []
    
    for i in range(0, data.length()) {
      if i % factor == 0 {
        downsampled_data = downsampled_data.push(data[i])
      }
    }
    
    return downsampled_data
  }
  
  // 应用2倍降采样
  let downsampled_data = downsample_data(raw_sensor_data, 2)
  
  // 验证降采样结果
  assert_eq(downsampled_data.length(), 3)  // 原始5个点，2倍降采样，得到3个点
  assert_eq(downsampled_data[0].timestamp, 1640995200)
  assert_eq(downsampled_data[1].timestamp, 1640995320)
  assert_eq(downsampled_data[2].timestamp, 1640995440)
  
  // 数据聚合（计算统计指标）
  func aggregate_data(data : Array[{ timestamp : Int, temperature : Float, humidity : Float, pressure : Float }]) -> { temperature : { min : Float, max : Float, avg : Float }, humidity : { min : Float, max : Float, avg : Float }, pressure : { min : Float, max : Float, avg : Float } } {
    let temperatures = data.map(d => d.temperature)
    let humidities = data.map(d => d.humidity)
    let pressures = data.map(d => d.pressure)
    
    // 计算温度统计
    let temp_min = temperatures.reduce((a, b) => if a < b { a } else { b })
    let temp_max = temperatures.reduce((a, b) => if a > b { a } else { b })
    let temp_sum = temperatures.reduce((a, b) => a + b)
    let temp_avg = temp_sum / temperatures.length().to_float()
    
    // 计算湿度统计
    let hum_min = humidities.reduce((a, b) => if a < b { a } else { b })
    let hum_max = humidities.reduce((a, b) => if a > b { a } else { b })
    let hum_sum = humidities.reduce((a, b) => a + b)
    let hum_avg = hum_sum / humidities.length().to_float()
    
    // 计算气压统计
    let press_min = pressures.reduce((a, b) => if a < b { a } else { b })
    let press_max = pressures.reduce((a, b) => if a > b { a } else { b })
    let press_sum = pressures.reduce((a, b) => a + b)
    let press_avg = press_sum / pressures.length().to_float()
    
    return {
      temperature: { min: temp_min, max: temp_max, avg: temp_avg },
      humidity: { min: hum_min, max: hum_max, avg: hum_avg },
      pressure: { min: press_min, max: press_max, avg: press_avg }
    }
  }
  
  // 计算数据聚合
  let aggregated_stats = aggregate_data(raw_sensor_data)
  
  // 验证聚合结果
  assert_eq(aggregated_stats.temperature.min, 25.5)
  assert_eq(aggregated_stats.temperature.max, 25.9)
  assert_eq(aggregated_stats.temperature.avg, (25.5 + 25.7 + 25.6 + 25.8 + 25.9) / 5.0)
  
  assert_eq(aggregated_stats.humidity.min, 64.8)
  assert_eq(aggregated_stats.humidity.max, 65.2)
  assert_eq(aggregated_stats.humidity.avg, (65.2 + 65.0 + 65.1 + 64.9 + 64.8) / 5.0)
  
  assert_eq(aggregated_stats.pressure.min, 1013.15)
  assert_eq(aggregated_stats.pressure.max, 1013.25)
  assert_eq(aggregated_stats.pressure.avg, (1013.25 + 1013.20 + 1013.22 + 1013.18 + 1013.15) / 5.0)
}

// 测试4: 边缘设备数据传输优化
test "边缘设备数据传输优化" {
  // 模拟边缘设备网络条件
  let network_conditions = [
    { device_id: "edge_001", bandwidth_kbps: 1000, latency_ms: 50, packet_loss: 0.01 },
    { device_id: "edge_002", bandwidth_kbps: 500, latency_ms: 100, packet_loss: 0.05 },
    { device_id: "edge_003", bandwidth_kbps: 200, latency_ms: 200, packet_loss: 0.1 }
  ]
  
  // 模拟待传输数据
  let telemetry_batches = [
    { batch_id: "batch_001", size_kb: 100, priority: "high", timestamp: 1640995200 },
    { batch_id: "batch_002", size_kb: 200, priority: "normal", timestamp: 1640995260 },
    { batch_id: "batch_003", size_kb: 50, priority: "low", timestamp: 1640995320 },
    { batch_id: "batch_004", size_kb: 150, priority: "high", timestamp: 1640995380 },
    { batch_id: "batch_005", size_kb: 75, priority: "normal", timestamp: 1640995440 }
  ]
  
  // 计算传输时间
  func calculate_transfer_time(size_kb : Int, bandwidth_kbps : Int, latency_ms : Int) -> Int {
    let transfer_time_ms = (size_kb * 1000) / bandwidth_kbps + latency_ms
    return transfer_time_ms
  }
  
  // 测试传输时间计算
  let edge_001_condition = network_conditions[0]
  let batch_001_transfer_time = calculate_transfer_time(telemetry_batches[0].size_kb, edge_001_condition.bandwidth_kbps, edge_001_condition.latency_ms)
  assert_eq(batch_001_transfer_time, (100 * 1000) / 1000 + 50)  // 100 + 50 = 150ms
  
  // 数据压缩优化
  func compress_data(original_size_kb : Int, compression_ratio : Float) -> Int {
    let compressed_size = original_size_kb.to_float() / compression_ratio
    return compressed_size.to_int()
  }
  
  // 测试数据压缩
  let original_size = 200  // KB
  let compression_ratio = 2.0  // 2倍压缩
  let compressed_size = compress_data(original_size, compression_ratio)
  assert_eq(compressed_size, 100)  // 200KB / 2 = 100KB
  
  // 计算压缩后的传输时间节省
  let original_transfer_time = calculate_transfer_time(original_size, edge_001_condition.bandwidth_kbps, edge_001_condition.latency_ms)
  let compressed_transfer_time = calculate_transfer_time(compressed_size, edge_001_condition.bandwidth_kbps, edge_001_condition.latency_ms)
  let time_saved = original_transfer_time - compressed_transfer_time
  
  assert_true(time_saved > 0)  // 压缩后应该节省传输时间
  
  // 数据优先级调度
  func schedule_transmissions(batches : Array[{ batch_id : String, size_kb : Int, priority : String, timestamp : Int }]) -> Array[{ batch_id : String, size_kb : Int, priority : String, timestamp : Int, order : Int }] {
    // 按优先级排序：high > normal > low
    let mut high_priority = []
    let mut normal_priority = []
    let mut low_priority = []
    
    for batch in batches {
      match batch.priority {
        "high" => high_priority = high_priority.push(batch),
        "normal" => normal_priority = normal_priority.push(batch),
        "low" => low_priority = low_priority.push(batch),
        _ => ()
      }
    }
    
    // 合并优先级队列
    let mut scheduled_batches = []
    let mut order = 1
    
    for batch in high_priority {
      scheduled_batches = scheduled_batches.push({ 
        batch_id: batch.batch_id, 
        size_kb: batch.size_kb, 
        priority: batch.priority, 
        timestamp: batch.timestamp,
        order: order
      })
      order = order + 1
    }
    
    for batch in normal_priority {
      scheduled_batches = scheduled_batches.push({ 
        batch_id: batch.batch_id, 
        size_kb: batch.size_kb, 
        priority: batch.priority, 
        timestamp: batch.timestamp,
        order: order
      })
      order = order + 1
    }
    
    for batch in low_priority {
      scheduled_batches = scheduled_batches.push({ 
        batch_id: batch.batch_id, 
        size_kb: batch.size_kb, 
        priority: batch.priority, 
        timestamp: batch.timestamp,
        order: order
      })
      order = order + 1
    }
    
    return scheduled_batches
  }
  
  // 测试传输调度
  let scheduled_transmissions = schedule_transmissions(telemetry_batches)
  
  // 验证调度结果
  assert_eq(scheduled_transmissions.length(), 5)
  
  // 高优先级批次应该排在前面
  assert_eq(scheduled_transmissions[0].priority, "high")
  assert_eq(scheduled_transmissions[0].batch_id, "batch_001")
  assert_eq(scheduled_transmissions[1].priority, "high")
  assert_eq(scheduled_transmissions[1].batch_id, "batch_004")
  
  // 普通优先级批次排在中间
  assert_eq(scheduled_transmissions[2].priority, "normal")
  assert_eq(scheduled_transmissions[2].batch_id, "batch_002")
  assert_eq(scheduled_transmissions[3].priority, "normal")
  assert_eq(scheduled_transmissions[3].batch_id, "batch_005")
  
  // 低优先级批次排在最后
  assert_eq(scheduled_transmissions[4].priority, "low")
  assert_eq(scheduled_transmissions[4].batch_id, "batch_003")
  
  // 网络自适应传输
  func adaptive_transmission(batch_size_kb : Int, network_condition : { bandwidth_kbps : Int, latency_ms : Int, packet_loss : Float }) -> { strategy : String, estimated_time_ms : Int, reliability : Float } {
    let base_transfer_time = calculate_transfer_time(batch_size_kb, network_condition.bandwidth_kbps, network_condition.latency_ms)
    
    // 根据网络条件选择传输策略
    if network_condition.packet_loss > 0.05 {
      // 高丢包率：使用重传机制
      let retry_factor = 1.5
      let estimated_time = (base_transfer_time.to_float() * retry_factor).to_int()
      let reliability = 1.0 - network_condition.packet_loss
      
      return { 
        strategy: "重传机制", 
        estimated_time_ms: estimated_time, 
        reliability: reliability 
      }
    } else if network_condition.bandwidth_kbps < 500 {
      // 低带宽：使用压缩传输
      let compression_ratio = 2.0
      let compressed_size = compress_data(batch_size_kb, compression_ratio)
      let compressed_time = calculate_transfer_time(compressed_size, network_condition.bandwidth_kbps, network_condition.latency_ms)
      let reliability = 1.0 - network_condition.packet_loss
      
      return { 
        strategy: "压缩传输", 
        estimated_time_ms: compressed_time, 
        reliability: reliability 
      }
    } else {
      // 良好网络：标准传输
      let reliability = 1.0 - network_condition.packet_loss
      
      return { 
        strategy: "标准传输", 
        estimated_time_ms: base_transfer_time, 
        reliability: reliability 
      }
    }
  }
  
  // 测试自适应传输
  let edge_002_condition = network_conditions[1]  // 500kbps, 100ms, 5%丢包
  let edge_003_condition = network_conditions[2]  // 200kbps, 200ms, 10%丢包
  
  let edge_002_adaptive = adaptive_transmission(100, edge_002_condition)
  assert_eq(edge_002_adaptive.strategy, "重传机制")  // 5%丢包率，使用重传
  assert_true(edge_002_adaptive.estimated_time_ms > calculate_transfer_time(100, edge_002_condition.bandwidth_kbps, edge_002_condition.latency_ms))
  
  let edge_003_adaptive = adaptive_transmission(100, edge_003_condition)
  assert_eq(edge_003_adaptive.strategy, "重传机制")  // 10%丢包率，使用重传
  assert_eq(edge_003_adaptive.reliability, 0.9)  // 1.0 - 0.1
}

// 测试5: 边缘设备离线处理
test "边缘设备离线处理" {
  // 模拟边缘设备离线状态
  let device_states = [
    { device_id: "edge_001", is_online: true, last_online: 1640995200, storage_available_mb: 1024 },
    { device_id: "edge_002", is_online: false, last_online: 1640995000, storage_available_mb: 512 },
    { device_id: "edge_003", is_online: true, last_online: 1640995300, storage_available_mb: 2048 }
  ]
  
  // 模拟离线期间生成的数据
  let offline_data = [
    { device_id: "edge_002", timestamp: 1640995100, data_size_kb: 20, data: "temperature:25.5" },
    { device_id: "edge_002", timestamp: 1640995200, data_size_kb: 20, data: "temperature:25.7" },
    { device_id: "edge_002", timestamp: 1640995300, data_size_kb: 20, data: "temperature:25.6" },
    { device_id: "edge_002", timestamp: 1640995400, data_size_kb: 20, data: "temperature:25.8" },
    { device_id: "edge_002", timestamp: 1640995500, data_size_kb: 20, data: "temperature:25.9" }
  ]
  
  // 计算离线数据存储需求
  func calculate_offline_storage_requirement(data : Array[{ device_id : String, timestamp : Int, data_size_kb : Int, data : String }]) -> Int {
    let mut total_size = 0
    
    for data_point in data {
      total_size = total_size + data_point.data_size_kb
    }
    
    return total_size
  }
  
  // 测试存储需求计算
  let storage_requirement = calculate_offline_storage_requirement(offline_data)
  assert_eq(storage_requirement, 100)  // 5个数据点，每个20KB，总共100KB
  
  // 检查设备是否有足够存储空间
  func has_enough_storage(device_id : String, required_kb : Int, device_states : Array[{ device_id : String, is_online : Bool, last_online : Int, storage_available_mb : Int }]) -> Bool {
    for device in device_states {
      if device.device_id == device_id {
        let available_kb = device.storage_available_mb * 1024
        return available_kb >= required_kb
      }
    }
    return false
  }
  
  // 测试存储空间检查
  assert_true(has_enough_storage("edge_002", storage_requirement, device_states))  // 512MB可用，需要100KB
  
  // 模拟数据缓存策略
  func cache_offline_data(data : Array[{ device_id : String, timestamp : Int, data_size_kb : Int, data : String }], max_storage_kb : Int) -> { cached_data : Array[{ device_id : String, timestamp : Int, data_size_kb : Int, data : String }], discarded_data : Array[{ device_id : String, timestamp : Int, data_size_kb : Int, data : String }] } {
    let mut cached_data = []
    let mut discarded_data = []
    let mut used_storage = 0
    
    // 按时间戳排序（最新的数据优先保留）
    let mut sorted_data = data
    // 这里简化处理，假设数据已经按时间戳排序
    
    // 从最新数据开始缓存
    for i in range(sorted_data.length() - 1, -1, -1) {
      let data_point = sorted_data[i]
      
      if used_storage + data_point.data_size_kb <= max_storage_kb {
        cached_data = [data_point] + cached_data
        used_storage = used_storage + data_point.data_size_kb
      } else {
        discarded_data = [data_point] + discarded_data
      }
    }
    
    return { cached_data: cached_data, discarded_data: discarded_data }
  }
  
  // 测试数据缓存（假设edge_002只有80KB可用）
  let max_storage_kb = 80
  let cache_result = cache_offline_data(offline_data, max_storage_kb)
  
  // 验证缓存结果
  assert_eq(cache_result.cached_data.length(), 4)  // 80KB / 20KB = 4个数据点
  assert_eq(cache_result.discarded_data.length(), 1)  // 1个数据点被丢弃
  
  // 检查缓存的数据是最新的
  assert_eq(cache_result.cached_data[0].timestamp, 1640995500)  // 最新的数据
  assert_eq(cache_result.cached_data[3].timestamp, 1640995200)  // 最早缓存的数据
  
  // 检查被丢弃的数据是最旧的
  assert_eq(cache_result.discarded_data[0].timestamp, 1640995100)  // 最旧的数据被丢弃
  
  // 模拟设备重新上线后的数据同步
  func sync_offline_data(cached_data : Array[{ device_id : String, timestamp : Int, data_size_kb : Int, data : String }], network_bandwidth_kbps : Int) -> { sync_time_ms : Int, sync_batches : Int } {
    let total_data_size = calculate_offline_storage_requirement(cached_data)
    let batch_size_kb = 50  // 每批50KB
    let sync_time_ms = (total_data_size * 1000) / network_bandwidth_kbps
    let sync_batches = (total_data_size + batch_size_kb - 1) / batch_size_kb  // 向上取整
    
    return { sync_time_ms: sync_time_ms, sync_batches: sync_batches }
  }
  
  // 测试数据同步
  let sync_result = sync_offline_data(cache_result.cached_data, 1000)  // 1Mbps带宽
  assert_eq(sync_result.sync_time_ms, (80 * 1000) / 1000)  // 80ms
  assert_eq(sync_result.sync_batches, 2)  // 80KB / 50KB = 1.6，向上取整为2批
}

// 测试6: 边缘设备能源管理
test "边缘设备能源管理" {
  // 模拟边缘设备能源状态
  let device_energy_states = [
    { device_id: "edge_001", battery_percent: 85.0, power_source: "battery", power_consumption_w: 5.0 },
    { device_id: "edge_002", battery_percent: 45.0, power_source: "battery", power_consumption_w: 8.0 },
    { device_id: "edge_003", battery_percent: 100.0, power_source: "mains", power_consumption_w: 10.0 }
  ]
  
  // 模拟不同操作模式的能耗
  let operation_modes = {
    "normal": { cpu_usage: 50, data_collection_interval_ms: 5000, power_multiplier: 1.0 },
    "power_save": { cpu_usage: 20, data_collection_interval_ms: 30000, power_multiplier: 0.6 },
    "performance": { cpu_usage: 80, data_collection_interval_ms: 1000, power_multiplier: 1.5 }
  }
  
  // 计算电池续航时间
  func calculate_battery_life(battery_percent : Float, power_consumption_w : Float, battery_capacity_wh : Float) -> Float {
    let remaining_capacity_wh = battery_capacity_wh * battery_percent / 100.0
    let battery_life_hours = remaining_capacity_wh / power_consumption_w
    return battery_life_hours
  }
  
  // 测试电池续航计算（假设电池容量为20Wh）
  let battery_capacity_wh = 20.0
  
  let edge_001_battery_life = calculate_battery_life(device_energy_states[0].battery_percent, device_energy_states[0].power_consumption_w, battery_capacity_wh)
  assert_eq(edge_001_battery_life, (20.0 * 85.0 / 100.0) / 5.0)  // 3.4小时
  
  let edge_002_battery_life = calculate_battery_life(device_energy_states[1].battery_percent, device_energy_states[1].power_consumption_w, battery_capacity_wh)
  assert_eq(edge_002_battery_life, (20.0 * 45.0 / 100.0) / 8.0)  // 1.125小时
  
  // 选择最优操作模式
  func select_optimal_mode(device_energy : { battery_percent : Float, power_source : String, power_consumption_w : Float }, modes : Map[String, { cpu_usage : Int, data_collection_interval_ms : Int, power_multiplier : Float }]) -> String {
    if device_energy.power_source == "mains" {
      return "performance"  // 使用市电时使用性能模式
    }
    
    if device_energy.battery_percent > 60.0 {
      return "normal"  // 电池充足时使用正常模式
    } else if device_energy.battery_percent > 20.0 {
      return "power_save"  // 电池不足时使用省电模式
    } else {
      return "power_save"  // 低电量时强制省电模式
    }
  }
  
  // 测试操作模式选择
  let edge_001_mode = select_optimal_mode(device_energy_states[0], operation_modes)
  assert_eq(edge_001_mode, "normal")  // 85%电池，正常模式
  
  let edge_002_mode = select_optimal_mode(device_energy_states[1], operation_modes)
  assert_eq(edge_002_mode, "power_save")  // 45%电池，省电模式
  
  let edge_003_mode = select_optimal_mode(device_energy_states[2], operation_modes)
  assert_eq(edge_003_mode, "performance")  // 使用市电，性能模式
  
  // 计算不同模式下的功耗
  func calculate_mode_power_consumption(base_power_w : Float, mode : { cpu_usage : Int, data_collection_interval_ms : Int, power_multiplier : Float }) -> Float {
    return base_power_w * mode.power_multiplier
  }
  
  // 测试功耗计算
  let normal_mode = operation_modes.get("normal").unwrap()
  let power_save_mode = operation_modes.get("power_save").unwrap()
  let performance_mode = operation_modes.get("performance").unwrap()
  
  let edge_002_normal_power = calculate_mode_power_consumption(device_energy_states[1].power_consumption_w, normal_mode)
  assert_eq(edge_002_normal_power, 8.0)  // 正常模式功耗不变
  
  let edge_002_power_save_power = calculate_mode_power_consumption(device_energy_states[1].power_consumption_w, power_save_mode)
  assert_eq(edge_002_power_save_power, 8.0 * 0.6)  // 省电模式功耗降低40%
  
  let edge_002_performance_power = calculate_mode_power_consumption(device_energy_states[1].power_consumption_w, performance_mode)
  assert_eq(edge_002_performance_power, 8.0 * 1.5)  // 性能模式功耗增加50%
  
  // 计算节能效果
  let power_savings_w = edge_002_normal_power - edge_002_power_save_power
  let power_savings_percent = power_savings_w / edge_002_normal_power * 100.0
  
  assert_eq(power_savings_w, 8.0 - 8.0 * 0.6)  // 3.2W
  assert_eq(power_savings_percent, 40.0)  // 40%
  
  // 计算省电模式下的续航延长
  let edge_002_normal_battery_life = calculate_battery_life(device_energy_states[1].battery_percent, edge_002_normal_power, battery_capacity_wh)
  let edge_002_power_save_battery_life = calculate_battery_life(device_energy_states[1].battery_percent, edge_002_power_save_power, battery_capacity_wh)
  
  let battery_life_extension = edge_002_power_save_battery_life - edge_002_normal_battery_life
  let battery_life_extension_percent = battery_life_extension / edge_002_normal_battery_life * 100.0
  
  assert_true(battery_life_extension > 0)  // 省电模式应该延长续航
  assert_eq(battery_life_extension_percent, 66.67)  // 续航延长约66.67%
}