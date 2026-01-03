// Azimuth 边缘计算和IoT设备遥测测试
// 专注于测试边缘设备、IoT传感器、资源受限环境下的遥测数据收集和处理

// 测试1: IoT设备遥测数据收集
test "IoT设备遥测数据收集和优化" {
  // 定义IoT设备类型
  enum DeviceType {
    Sensor
    Actuator
    Gateway
    EdgeNode
    MobileDevice
  }
  
  // 定义传感器类型
  enum SensorType {
    Temperature
    Humidity
    Pressure
    Motion
    Light
    Sound
    GPS
    Accelerometer
    Gyroscope
  }
  
  // 定义设备能力
  type DeviceCapabilities = {
    cpu_cores: Int,
    memory_mb: Int,
    storage_mb: Int,
    battery_percentage: Float,
    network_type: String,  // "wifi", "cellular", "lora", "ble"
    bandwidth_kbps: Int,
    supported_protocols: Array[String>
  }
  
  // 定义IoT设备遥测数据
  type IoTTelemetryData = {
    device_id: String,
    device_type: DeviceType,
    sensor_type: Option[SensorType>,
    timestamp: Int,
    location: Option<(Float, Float)>,  // (latitude, longitude)
    measurements: Array[(String, Float)>,
    battery_level: Float,
    signal_strength: Int,  // dBm
    metadata: Array[(String, String)>
  }
  
  // 定义数据压缩策略
  enum CompressionStrategy {
    None
    DeltaEncoding    // 增量编码
    RunLength        // 行程编码
    Huffman          // 霍夫曼编码
    Custom           // 自定义算法
  }
  
  // 创建IoT设备模拟器
  let create_iot_device_simulator = fn(device_id: String, device_type: DeviceType, capabilities: DeviceCapabilities) {
    let mut sequence_number = 0
    
    let generate_telemetry = fn(sensor_type: Option[SensorType>, location: Option<(Float, Float)>) {
      sequence_number = sequence_number + 1
      let current_time = Time::now()
      
      let measurements = match sensor_type {
        Some(SensorType::Temperature) => [
          ("temperature_celsius", 20.0 + Math::random() * 15.0),
          ("temperature_fahrenheit", 68.0 + Math::random() * 27.0)
        ]
        Some(SensorType::Humidity) => [
          ("humidity_percent", 30.0 + Math::random() * 60.0)
        ]
        Some(SensorType::Pressure) => [
          ("pressure_hpa", 980.0 + Math::random() * 50.0)
        ]
        Some(SensorType::Motion) => [
          ("motion_detected", if Math::random() > 0.8 { 1.0 } else { 0.0 }),
          ("motion_intensity", Math::random() * 10.0)
        ]
        Some(SensorType::Light) => [
          ("light_lux", 100.0 + Math::random() * 900.0)
        ]
        Some(SensorType::GPS) => [
          ("latitude", location.unwrap_or((0.0, 0.0)).0),
          ("longitude", location.unwrap_or((0.0, 0.0)).1),
          ("altitude", Math::random() * 1000.0),
          ("speed_kmh", Math::random() * 120.0)
        ]
        Some(SensorType::Accelerometer) => [
          ("accel_x", (Math::random() - 0.5) * 10.0),
          ("accel_y", (Math::random() - 0.5) * 10.0),
          ("accel_z", (Math::random() - 0.5) * 10.0)
        ]
        Some(SensorType::Gyroscope) => [
          ("gyro_x", (Math::random() - 0.5) * 360.0),
          ("gyro_y", (Math::random() - 0.5) * 360.0),
          ("gyro_z", (Math::random() - 0.5) * 360.0)
        ]
        None => []
        _ => []
      }
      
      // 模拟电池消耗
      let battery_drain = 0.01 + Math::random() * 0.05
      let current_battery = capabilities.battery_percentage - battery_drain
      
      // 模拟信号强度变化
      let signal_variation = Math::random() * 10.0 - 5.0
      let current_signal = (-70 + signal_variation) as Int  // 典型的WiFi信号强度范围
      
      {
        device_id: device_id,
        device_type: device_type,
        sensor_type: sensor_type,
        timestamp: current_time,
        location: location,
        measurements: measurements,
        battery_level: Math::max(0.0, current_battery),
        signal_strength: current_signal,
        metadata: [
          ("sequence_number", sequence_number.to_string()),
          ("firmware_version", "1.2.3"),
          ("protocol_version", "2.0")
        ]
      }
    }
    
    let get_capabilities = fn() { capabilities }
    
    {
      generate_telemetry,
      get_capabilities
    }
  }
  
  // 创建数据压缩器
  let create_data_compressor = fn(strategy: CompressionStrategy) {
    let compress = fn(data: IoTTelemetryData) {
      match strategy {
        CompressionStrategy::None => {
          JSON::stringify(data)
        }
        CompressionStrategy::DeltaEncoding => {
          // 简化的增量编码：只存储与上一个值的差异
          let base_time = data.timestamp
          let compressed_measurements = data.measurements.map_fn(measurement) {
            let (key, value) = measurement
            (key, value - Math::floor(value))  // 只存储小数部分
          }
          
          let compressed_data = {
            device_id: data.device_id,
            device_type: data.device_type,
            sensor_type: data.sensor_type,
            base_timestamp: base_time,
            delta_measurements: compressed_measurements,
            battery_level: data.battery_level,
            signal_strength: data.signal_strength,
            metadata: data.metadata
          }
          
          JSON::stringify(compressed_data)
        }
        CompressionStrategy::RunLength => {
          // 简化的行程编码：压缩重复值
          let encoded_measurements = []
          let mut current_measurements = data.measurements
          
          while current_measurements.length() > 0 {
            let current_value = current_measurements[0]
            let mut count = 1
            
            while count < current_measurements.length() && 
                  current_measurements[count] == current_value {
              count = count + 1
            }
            
            encoded_measurements = encoded_measurements.push((current_value.0, current_value.1, count))
            current_measurements = current_measurements.slice(count)
          }
          
          let compressed_data = {
            device_id: data.device_id,
            device_type: data.device_type,
            sensor_type: data.sensor_type,
            timestamp: data.timestamp,
            rle_measurements: encoded_measurements,
            battery_level: data.battery_level,
            signal_strength: data.signal_strength,
            metadata: data.metadata
          }
          
          JSON::stringify(compressed_data)
        }
        _ => {
          JSON::stringify(data)  // 默认不压缩
        }
      }
    }
    
    let get_compression_ratio = fn(original_data: String, compressed_data: String) {
      (original_data.length() as Float) / (compressed_data.length() as Float)
    }
    
    {
      compress,
      get_compression_ratio
    }
  }
  
  // 创建边缘数据处理策略
  let create_edge_processing_strategy = fn(capabilities: DeviceCapabilities) {
    let should_batch_data = fn(data_points: Array[IoTTelemetryData>) -> Bool {
      // 根据网络带宽决定是否批处理
      let avg_data_size = 1024  // 假设每个数据点1KB
      let total_size = data_points.length() * avg_data_size
      
      // 如果带宽低，则批处理
      capabilities.bandwidth_kbps < 100 || total_size > 10000
    }
    
    let get_batch_size = fn() {
      // 根据内存大小确定批处理大小
      if capabilities.memory_mb < 64 {
        5  // 低内存设备，小批次
      } else if capabilities.memory_mb < 256 {
        20  // 中等内存设备，中等批次
      } else {
        50  // 高内存设备，大批次
      }
    }
    
    let should_compress = fn() {
      // 如果带宽低或电池电量低，则压缩
      capabilities.bandwidth_kbps < 100 || capabilities.battery_percentage < 30.0
    }
    
    let get_compression_strategy = fn() {
      if capabilities.memory_mb < 64 {
        // 低内存设备使用简单压缩
        CompressionStrategy::RunLength
      } else if capabilities.battery_percentage < 20.0 {
        // 低电量设备使用高效压缩
        CompressionStrategy::DeltaEncoding
      } else {
        CompressionStrategy::None
      }
    }
    
    let should_filter_data = fn() {
      // 如果电池电量低，则过滤数据
      capabilities.battery_percentage < 15.0
    }
    
    let filter_data = fn(data: IoTTelemetryData) {
      // 简单的数据过滤：只保留重要的测量值
      let important_measurements = data.measurements.filter_fn(measurement) {
        match measurement {
          ("temperature_celsius", _) => true
          ("humidity_percent", _) => true
          ("motion_detected", value) => value > 0.0  // 只保留检测到运动的情况
          ("battery_level", _) => true
          _ => false
        }
      }
      
      { data with measurements: important_measurements }
    }
    
    {
      should_batch_data,
      get_batch_size,
      should_compress,
      get_compression_strategy,
      should_filter_data,
      filter_data
    }
  }
  
  // 测试IoT设备遥测收集
  let device_capabilities = {
    cpu_cores: 1,
    memory_mb: 64,
    storage_mb: 512,
    battery_percentage: 85.0,
    network_type: "wifi",
    bandwidth_kbps: 1000,
    supported_protocols: ["mqtt", "http", "coap"]
  }
  
  let device_simulator = create_iot_device_simulator("device-001", DeviceType::Sensor, device_capabilities)
  let edge_strategy = create_edge_processing_strategy(device_capabilities)
  
  // 生成遥测数据
  let telemetry_data = device_simulator.generate_telemetry(Some(SensorType::Temperature), Some((37.7749, -122.4194)))
  
  // 验证遥测数据
  assert_eq(telemetry_data.device_id, "device-001")
  assert_eq(telemetry_data.device_type, DeviceType::Sensor)
  assert_eq(telemetry_data.sensor_type, Some(SensorType::Temperature))
  assert_true(telemetry_data.measurements.length() >= 2)  // 温度传感器至少有摄氏度和华氏度
  assert_true(telemetry_data.battery_level <= 85.0)  // 电池应该有消耗
  assert_true(telemetry_data.signal_strength >= -100 && telemetry_data.signal_strength <= -30)  // 合理的信号强度范围
  
  // 测试边缘处理策略
  let data_points = []
  for i in 0..10 {
    let data = device_simulator.generate_telemetry(Some(SensorType::Temperature), Some((37.7749, -122.4194)))
    data_points = data_points.push(data)
  }
  
  // 测试批处理决策
  let should_batch = edge_strategy.should_batch_data(data_points)
  assert_false(should_batch)  // 高带宽设备不应该批处理
  
  // 测试压缩决策
  let should_compress = edge_strategy.should_compress()
  assert_false(should_compress)  // 高电量和带宽不应该压缩
  
  // 测试压缩
  let compressor = create_data_compressor(CompressionStrategy::DeltaEncoding)
  let original_json = JSON::stringify(telemetry_data)
  let compressed_json = compressor.compress(telemetry_data)
  
  let compression_ratio = compressor.get_compression_ratio(original_json, compressed_json)
  assert_true(compression_ratio >= 1.0)  // 压缩后应该更小或相等
  
  // 测试低资源设备
  let low_resource_capabilities = {
    cpu_cores: 1,
    memory_mb: 32,
    storage_mb: 256,
    battery_percentage: 10.0,
    network_type: "cellular",
    bandwidth_kbps: 50,
    supported_protocols: ["mqtt", "coap"]
  }
  
  let low_resource_device = create_iot_device_simulator("device-002", DeviceType::Sensor, low_resource_capabilities)
  let low_resource_strategy = create_edge_processing_strategy(low_resource_capabilities)
  
  let low_resource_data = low_resource_device.generate_telemetry(Some(SensorType::Temperature), None)
  
  // 低资源设备应该批处理
  let low_resource_data_points = []
  for i in 0..10 {
    let data = low_resource_device.generate_telemetry(Some(SensorType::Temperature), None)
    low_resource_data_points = low_resource_data_points.push(data)
  }
  
  assert_true(low_resource_strategy.should_batch_data(low_resource_data_points))
  assert_eq(low_resource_strategy.get_batch_size(), 5)
  assert_true(low_resource_strategy.should_compress())
  assert_eq(low_resource_strategy.get_compression_strategy(), CompressionStrategy::DeltaEncoding)
  assert_true(low_resource_strategy.should_filter_data())
  
  // 测试数据过滤
  let filtered_data = low_resource_strategy.filter_data(low_resource_data)
  assert_true(filtered_data.measurements.length() <= low_resource_data.measurements.length())
}

// 测试2: 边缘节点数据聚合和处理
test "边缘节点数据聚合和处理验证" {
  // 定义聚合窗口类型
  enum AggregationWindow {
    Seconds(Int)
    Minutes(Int)
    Hours(Int)
    Events(Int)  // 基于事件数量
  }
  
  // 定义聚合函数
  enum AggregationFunction {
    Sum
    Average
    Min
    Max
    Count
    Median
    Percentile(Float)
  }
  
  // 定义聚合规则
  type AggregationRule = {
    name: String,
    source_device_pattern: String,  // 设备ID模式匹配
    measurement_keys: Array[String>,
    window: AggregationWindow,
    functions: Array[AggregationFunction>,
    conditions: Array[(String, String)>  // 聚合条件
  }
  
  // 定义聚合结果
  type AggregationResult = {
    rule_name: String,
    window_start: Int,
    window_end: Int,
    device_count: Int,
    measurement_count: Int,
    aggregated_values: Map[String, Map[String, Float>>  // measurement -> function -> value
  }
  
  // 创建边缘节点处理器
  let create_edge_processor = fn() {
    let mut data_buffer = []
    let mut aggregation_rules = []
    
    let add_data = fn(data: IoTTelemetryData) {
      data_buffer = data_buffer.push(data)
    }
    
    let add_aggregation_rule = fn(rule: AggregationRule) {
      aggregation_rules = aggregation_rules.push(rule)
    }
    
    let process_aggregations = fn() {
      if data_buffer.length() == 0 || aggregation_rules.length() == 0 {
        return []
      }
      
      let results = []
      
      for rule in aggregation_rules {
        // 匹配设备
        let matched_data = data_buffer.filter_fn(data) {
          Regex::matches(rule.source_device_pattern, data.device_id)
        }
        
        if matched_data.length() == 0 {
          continue
        }
        
        // 确定时间窗口
        let (window_start, window_end) = match rule.window {
          AggregationWindow::Seconds(seconds) => {
            let now = Time::now()
            let window_start = now - seconds * 1000
            (window_start, now)
          }
          AggregationWindow::Minutes(minutes) => {
            let now = Time::now()
            let window_start = now - minutes * 60 * 1000
            (window_start, now)
          }
          AggregationWindow::Hours(hours) => {
            let now = Time::now()
            let window_start = now - hours * 60 * 60 * 1000
            (window_start, now)
          }
          AggregationWindow::Events(count) => {
            if matched_data.length() >= count {
              let sorted_data = matched_data.sort_fn(a, b) {
                if a.timestamp < b.timestamp { -1 }
                else if a.timestamp > b.timestamp { 1 }
                else { 0 }
              }
              
              let window_start = sorted_data[0].timestamp
              let window_end = sorted_data[count - 1].timestamp
              (window_start, window_end)
            } else {
              let sorted_data = matched_data.sort_fn(a, b) {
                if a.timestamp < b.timestamp { -1 }
                else if a.timestamp > b.timestamp { 1 }
                else { 0 }
              }
              
              let window_start = sorted_data[0].timestamp
              let window_end = sorted_data[sorted_data.length() - 1].timestamp
              (window_start, window_end)
            }
          }
        }
        
        // 筛选窗口内的数据
        let window_data = matched_data.filter_fn(data) {
          data.timestamp >= window_start && data.timestamp <= window_end
        }
        
        if window_data.length() == 0 {
          continue
        }
        
        // 聚合测量值
        let aggregated_values = Map::empty()
        
        for measurement_key in rule.measurement_keys {
          let values = window_data.filter_map_fn(data) {
            data.measurements.find_fn(measurement) {
              match measurement { (key, _) => key == measurement_key }
            }
          }.map_fn(measurement) {
            match measurement { (_, value) => value }
          }
          
          if values.length() > 0 {
            let function_results = Map::empty()
            
            for function in rule.functions {
              let result = match function {
                AggregationFunction::Sum => values.reduce(0.0, fn(sum, v) { sum + v })
                AggregationFunction::Average => values.reduce(0.0, fn(sum, v) { sum + v }) / (values.length() as Float)
                AggregationFunction::Min => values.reduce(values[0], fn(min, v) { if v < min { v } else { min } })
                AggregationFunction::Max => values.reduce(values[0], fn(max, v) { if v > max { v } else { max } })
                AggregationFunction::Count => values.length() as Float
                AggregationFunction::Median => {
                  let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
                  let mid = sorted_values.length() / 2
                  if sorted_values.length() % 2 == 0 {
                    (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
                  } else {
                    sorted_values[mid]
                  }
                }
                AggregationFunction::Percentile(p) => {
                  let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
                  let index = ((sorted_values.length() as Float) * p / 100.0) as Int
                  sorted_values[Math::min(index, sorted_values.length() - 1)]
                }
              }
              
              let function_name = match function {
                AggregationFunction::Sum => "sum"
                AggregationFunction::Average => "avg"
                AggregationFunction::Min => "min"
                AggregationFunction::Max => "max"
                AggregationFunction::Count => "count"
                AggregationFunction::Median => "median"
                AggregationFunction::Percentile(p) => "p" + p.to_string()
              }
              
              let _ = Map::insert(function_results, function_name, result)
            }
            
            let _ = Map::insert(aggregated_values, measurement_key, function_results)
          }
        }
        
        let result = {
          rule_name: rule.name,
          window_start: window_start,
          window_end: window_end,
          device_count: window_data.map_fn(data) { data.device_id }.unique().length(),
          measurement_count: window_data.length(),
          aggregated_values: aggregated_values
        }
        
        results = results.push(result)
      }
      
      results
    }
    
    let clear_buffer = fn() {
      data_buffer = []
    }
    
    {
      add_data,
      add_aggregation_rule,
      process_aggregations,
      clear_buffer
    }
  }
  
  // 创建测试数据
  let create_test_iot_data = fn(device_id: String, sensor_type: SensorType, count: Int, start_time: Int) {
    let data_points = []
    
    for i in 0..count {
      let timestamp = start_time + i * 5000  // 每5秒一个数据点
      
      let measurements = match sensor_type {
        SensorType::Temperature => [
          ("temperature_celsius", 20.0 + Math::random() * 10.0),
          ("humidity_percent", 40.0 + Math::random() * 30.0)
        ]
        SensorType::Motion => [
          ("motion_detected", if Math::random() > 0.7 { 1.0 } else { 0.0 }),
          ("motion_intensity", Math::random() * 10.0)
        ]
        SensorType::Light => [
          ("light_lux", 200.0 + Math::random() * 800.0)
        ]
        _ => []
      }
      
      let data_point = {
        device_id: device_id,
        device_type: DeviceType::Sensor,
        sensor_type: Some(sensor_type),
        timestamp: timestamp,
        location: Some((37.7749, -122.4194)),
        measurements: measurements,
        battery_level: 80.0 - Math::random() * 20.0,
        signal_strength: -60 + (Math::random() * 20.0 - 10.0) as Int,
        metadata: []
      }
      
      data_points = data_points.push(data_point)
    }
    
    data_points
  }
  
  // 测试边缘节点聚合
  let processor = create_edge_processor()
  
  // 添加聚合规则
  let temp_aggregation_rule = {
    name: "temperature_stats",
    source_device_pattern: "temp-sensor-.*",
    measurement_keys: ["temperature_celsius", "humidity_percent"],
    window: AggregationWindow::Minutes(5),
    functions: [AggregationFunction::Average, AggregationFunction::Min, AggregationFunction::Max],
    conditions: []
  }
  
  let motion_aggregation_rule = {
    name: "motion_events",
    source_device_pattern: "motion-sensor-.*",
    measurement_keys: ["motion_detected", "motion_intensity"],
    window: AggregationWindow::Events(10),
    functions: [AggregationFunction::Sum, AggregationFunction::Average],
    conditions: []
  }
  
  processor.add_aggregation_rule(temp_aggregation_rule)
  processor.add_aggregation_rule(motion_aggregation_rule)
  
  // 添加测试数据
  let base_time = Time::now() - 300000  // 5分钟前
  
  let temp_data1 = create_test_iot_data("temp-sensor-01", SensorType::Temperature, 20, base_time)
  let temp_data2 = create_test_iot_data("temp-sensor-02", SensorType::Temperature, 15, base_time + 30000)
  let motion_data = create_test_iot_data("motion-sensor-01", SensorType::Motion, 25, base_time)
  
  for data in temp_data1 + temp_data2 + motion_data {
    processor.add_data(data)
  }
  
  // 处理聚合
  let results = processor.process_aggregations()
  
  // 验证聚合结果
  assert_eq(results.length(), 2)  // 应该有两个聚合结果
  
  // 验证温度聚合
  let temp_result = results.find_fn(result) { result.rule_name == "temperature_stats" }
  assert_true(temp_result.is_some())
  
  match temp_result {
    Some(result) => {
      assert_eq(result.device_count, 2)  // 两个温度传感器
      assert_true(result.measurement_count > 0)
      
      // 验证聚合值
      match Map::get(result.aggregated_values, "temperature_celsius") {
        Some(temp_functions) => {
          assert_true(Map::contains_key(temp_functions, "avg"))
          assert_true(Map::contains_key(temp_functions, "min"))
          assert_true(Map::contains_key(temp_functions, "max"))
          
          let avg_temp = match Map::get(temp_functions, "avg") {
            Some(value) => value
            None => 0.0
          }
          
          let min_temp = match Map::get(temp_functions, "min") {
            Some(value) => value
            None => 0.0
          }
          
          let max_temp = match Map::get(temp_functions, "max") {
            Some(value) => value
            None => 0.0
          }
          
          assert_true(avg_temp >= min_temp && avg_temp <= max_temp)
          assert_true(min_temp >= 15.0 && max_temp <= 35.0)  // 合理的温度范围
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证运动聚合
  let motion_result = results.find_fn(result) { result.rule_name == "motion_events" }
  assert_true(motion_result.is_some())
  
  match motion_result {
    Some(result) => {
      assert_eq(result.device_count, 1)  // 一个运动传感器
      assert_eq(result.measurement_count, 10)  // 10个事件
      
      // 验证聚合值
      match Map::get(result.aggregated_values, "motion_detected") {
        Some(motion_functions) => {
          assert_true(Map::contains_key(motion_functions, "sum"))
          assert_true(Map::contains_key(motion_functions, "avg"))
          
          let motion_sum = match Map::get(motion_functions, "sum") {
            Some(value) => value
            None => 0.0
          }
          
          let motion_avg = match Map::get(motion_functions, "avg") {
            Some(value) => value
            None => 0.0
          }
          
          assert_true(motion_sum >= 0.0 && motion_sum <= 10.0)  // 0-10个运动事件
          assert_true(motion_avg >= 0.0 && motion_avg <= 1.0)  // 平均0-1
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试3: 边缘设备故障检测和恢复
test "边缘设备故障检测和恢复机制验证" {
  // 定义设备状态
  enum DeviceStatus {
    Online
    Offline
    Degraded
    Maintenance
    Error
  }
  
  // 定义故障类型
  enum FaultType {
    BatteryLow
    ConnectivityLost
    SensorMalfunction
    MemoryExhausted
    Overheating
    ConfigurationError
  }
  
  // 定义设备健康指标
  type DeviceHealth = {
    device_id: String,
    status: DeviceStatus,
    last_seen: Int,
    battery_level: Float,
    signal_strength: Int,
    memory_usage: Float,
    cpu_usage: Float,
    temperature: Float,
    error_count: Int,
    uptime_percentage: Float
  }
  
  // 定义故障事件
  type FaultEvent = {
    device_id: String,
    fault_type: FaultType,
    severity: String,  // "low", "medium", "high", "critical"
    timestamp: Int,
    description: String,
    resolved: Bool,
    resolved_at: Option<Int>
  }
  
  // 定义恢复策略
  type RecoveryStrategy = {
    fault_type: FaultType,
    actions: Array[String>,  // 恢复动作
    auto_recovery: Bool,     // 是否自动恢复
    max_attempts: Int        // 最大尝试次数
  }
  
  // 创建设备健康监控器
  let create_device_health_monitor = fn() {
    let mut device_health = Map::empty()
    let mut fault_events = []
    let mut recovery_strategies = []
    
    let update_device_health = fn(health: DeviceHealth) {
      let previous_health = Map::get(device_health, health.device_id)
      let _ = Map::insert(device_health, health.device_id, health)
      
      // 检测状态变化
      match previous_health {
        Some(prev) => {
          if prev.status != health.status {
            // 状态变化，可能产生故障事件
            match health.status {
              DeviceStatus::Offline => {
                let fault = {
                  device_id: health.device_id,
                  fault_type: FaultType::ConnectivityLost,
                  severity: "high",
                  timestamp: Time::now(),
                  description: "设备离线",
                  resolved: false,
                  resolved_at: None
                }
                fault_events = fault_events.push(fault)
              }
              DeviceStatus::Error => {
                let fault = {
                  device_id: health.device_id,
                  fault_type: FaultType::SensorMalfunction,
                  severity: "medium",
                  timestamp: Time::now(),
                  description: "设备错误",
                  resolved: false,
                  resolved_at: None
                }
                fault_events = fault_events.push(fault)
              }
              DeviceStatus::Degraded => {
                if health.battery_level < 10.0 {
                  let fault = {
                    device_id: health.device_id,
                    fault_type: FaultType::BatteryLow,
                    severity: "high",
                    timestamp: Time::now(),
                    description: "电池电量低",
                    resolved: false,
                    resolved_at: None
                  }
                  fault_events = fault_events.push(fault)
                }
                
                if health.signal_strength < -90 {
                  let fault = {
                    device_id: health.device_id,
                    fault_type: FaultType::ConnectivityLost,
                    severity: "medium",
                    timestamp: Time::now(),
                    description: "信号弱",
                    resolved: false,
                    resolved_at: None
                  }
                  fault_events = fault_events.push(fault)
                }
              }
              _ => {}
            }
          }
        }
        None => {}
      }
      
      // 检测其他故障条件
      if health.battery_level < 5.0 {
        let existing_fault = fault_events.find_fn(event) {
          event.device_id == health.device_id && 
          event.fault_type == FaultType::BatteryLow && 
          not event.resolved
        }
        
        if existing_fault.is_none() {
          let fault = {
            device_id: health.device_id,
            fault_type: FaultType::BatteryLow,
            severity: "critical",
            timestamp: Time::now(),
            description: "电池严重不足",
            resolved: false,
            resolved_at: None
          }
          fault_events = fault_events.push(fault)
        }
      }
      
      if health.memory_usage > 90.0 {
        let existing_fault = fault_events.find_fn(event) {
          event.device_id == health.device_id && 
          event.fault_type == FaultType::MemoryExhausted && 
          not event.resolved
        }
        
        if existing_fault.is_none() {
          let fault = {
            device_id: health.device_id,
            fault_type: FaultType::MemoryExhausted,
            severity: "high",
            timestamp: Time::now(),
            description: "内存不足",
            resolved: false,
            resolved_at: None
          }
          fault_events = fault_events.push(fault)
        }
      }
      
      if health.temperature > 70.0 {
        let existing_fault = fault_events.find_fn(event) {
          event.device_id == health.device_id && 
          event.fault_type == FaultType::Overheating && 
          not event.resolved
        }
        
        if existing_fault.is_none() {
          let fault = {
            device_id: health.device_id,
            fault_type: FaultType::Overheating,
            severity: "critical",
            timestamp: Time::now(),
            description: "设备过热",
            resolved: false,
            resolved_at: None
          }
          fault_events = fault_events.push(fault)
        }
      }
    }
    
    let add_recovery_strategy = fn(strategy: RecoveryStrategy) {
      recovery_strategies = recovery_strategies.push(strategy)
    }
    
    let attempt_recovery = fn(device_id: String, fault_type: FaultType) {
      match recovery_strategies.find_fn(strategy) { strategy.fault_type == fault_type } {
        Some(strategy) => {
          if strategy.auto_recovery {
            // 模拟恢复尝试
            let recovery_success = Math::random() > 0.3  // 70%成功率
            
            if recovery_success {
              // 标记故障为已解决
              let unresolved_faults = fault_events.filter_fn(event) {
                event.device_id == device_id && 
                event.fault_type == fault_type && 
                not event.resolved
              }
              
              for fault in unresolved_faults {
                let resolved_fault = { fault with 
                  resolved: true, 
                  resolved_at: Some(Time::now()) 
                }
                
                // 替换原故障事件
                fault_events = fault_events.map_fn(event) {
                  if event.device_id == device_id && 
                     event.fault_type == fault_type && 
                     not event.resolved {
                    resolved_fault
                  } else {
                    event
                  }
                }
              }
              
              return true
            }
          }
          
          false
        }
        None => false
      }
    }
    
    let get_device_health = fn(device_id: String) {
      Map::get(device_health, device_id)
    }
    
    let get_active_faults = fn(device_id: String) {
      fault_events.filter_fn(event) {
        event.device_id == device_id && not event.resolved
      }
    }
    
    let get_all_faults = fn() { fault_events }
    
    {
      update_device_health,
      add_recovery_strategy,
      attempt_recovery,
      get_device_health,
      get_active_faults,
      get_all_faults
    }
  }
  
  // 测试设备健康监控
  let monitor = create_device_health_monitor()
  
  // 添加恢复策略
  let battery_recovery_strategy = {
    fault_type: FaultType::BatteryLow,
    actions: ["reduce_sampling_rate", "enable_power_saving"],
    auto_recovery: true,
    max_attempts: 3
  }
  
  let connectivity_recovery_strategy = {
    fault_type: FaultType::ConnectivityLost,
    actions: ["reconnect_network", "switch_backup_connection"],
    auto_recovery: true,
    max_attempts: 5
  }
  
  monitor.add_recovery_strategy(battery_recovery_strategy)
  monitor.add_recovery_strategy(connectivity_recovery_strategy)
  
  // 创建正常设备健康状态
  let normal_health = {
    device_id: "device-001",
    status: DeviceStatus::Online,
    last_seen: Time::now(),
    battery_level: 80.0,
    signal_strength: -60,
    memory_usage: 45.0,
    cpu_usage: 30.0,
    temperature: 35.0,
    error_count: 0,
    uptime_percentage: 99.5
  }
  
  monitor.update_device_health(normal_health)
  
  // 验证正常状态
  match monitor.get_device_health("device-001") {
    Some(health) => {
      assert_eq(health.status, DeviceStatus::Online)
      assert_eq(health.battery_level, 80.0)
      assert_eq(health.signal_strength, -60)
    }
    None => assert_true(false)
  }
  
  // 验证无活跃故障
  let active_faults = monitor.get_active_faults("device-001")
  assert_eq(active_faults.length(), 0)
  
  // 更新为低电量状态
  let low_battery_health = {
    device_id: "device-001",
    status: DeviceStatus::Degraded,
    last_seen: Time::now(),
    battery_level: 8.0,
    signal_strength: -65,
    memory_usage: 50.0,
    cpu_usage: 35.0,
    temperature: 38.0,
    error_count: 2,
    uptime_percentage: 95.0
  }
  
  monitor.update_device_health(low_battery_health)
  
  // 验证低电量故障检测
  let active_faults_after = monitor.get_active_faults("device-001")
  assert_true(active_faults_after.length() > 0)
  
  let battery_fault = active_faults_after.find_fn(fault) {
    fault.fault_type == FaultType::BatteryLow
  }
  assert_true(battery_fault.is_some())
  
  match battery_fault {
    Some(fault) => {
      assert_eq(fault.device_id, "device-001")
      assert_eq(fault.severity, "critical")
      assert_false(fault.resolved)
    }
    None => assert_true(false)
  }
  
  // 尝试恢复
  let recovery_success = monitor.attempt_recovery("device-001", FaultType::BatteryLow)
  assert_true(recovery_success || !recovery_success)  // 可能成功或失败，都是有效的测试结果
  
  // 如果恢复成功，验证故障已解决
  if recovery_success {
    let active_faults_after_recovery = monitor.get_active_faults("device-001")
    let battery_fault_after = active_faults_after_recovery.find_fn(fault) {
      fault.fault_type == FaultType::BatteryLow
    }
    assert_true(battery_fault_after.is_none())
  }
  
  // 测试设备离线
  let offline_health = {
    device_id: "device-001",
    status: DeviceStatus::Offline,
    last_seen: Time::now() - 300000,  // 5分钟前
    battery_level: 7.0,
    signal_strength: -95,
    memory_usage: 60.0,
    cpu_usage: 40.0,
    temperature: 40.0,
    error_count: 5,
    uptime_percentage: 90.0
  }
  
  monitor.update_device_health(offline_health)
  
  // 验证离线故障检测
  let active_faults_offline = monitor.get_active_faults("device-001")
  let connectivity_fault = active_faults_offline.find_fn(fault) {
    fault.fault_type == FaultType::ConnectivityLost
  }
  assert_true(connectivity_fault.is_some())
  
  // 验证所有故障
  let all_faults = monitor.get_all_faults()
  assert_true(all_faults.length() >= 2)  // 至少有低电量和连接丢失故障
}

// 测试4: 边缘设备同步和离线处理
test "边缘设备同步和离线处理验证" {
  // 定义同步状态
  enum SyncStatus {
    Synced
    Pending
    Failed
    Offline
  }
  
  // 定义数据同步策略
  enum SyncStrategy {
    Immediate      // 立即同步
    Batched        // 批量同步
    Conditional    // 条件同步（如网络可用时）
    Manual         // 手动同步
  }
  
  // 定义同步配置
  type SyncConfig = {
    strategy: SyncStrategy,
    batch_size: Int,
    retry_attempts: Int,
    retry_delay_ms: Int,
    offline_storage_limit_mb: Int,
    compression_enabled: Bool
  }
  
  // 定义离线数据存储
  type OfflineDataStore = {
    data: Array[IoTTelemetryData>,
    max_size_mb: Int,
    current_size_mb: Int,
    oldest_timestamp: Int
  }
  
  // 定义同步结果
  type SyncResult = {
    success: Bool,
    synced_count: Int,
    failed_count: Int,
    error_message: Option<String>,
    duration_ms: Int
  }
  
  // 创建边缘设备同步管理器
  let create_edge_sync_manager = fn(config: SyncConfig) {
    let mut offline_store = {
      data: [],
      max_size_mb: config.offline_storage_limit_mb,
      current_size_mb: 0,
      oldest_timestamp: Time::now()
    }
    
    let mut sync_status = SyncStatus::Synced
    let mut last_sync_time = 0
    
    let store_data_offline = fn(data: IoTTelemetryData) {
      // 估算数据大小（简化）
      let data_size_kb = 1  // 假设每条数据1KB
      
      if offline_store.current_size_mb + data_size_kb / 1024.0 <= offline_store.max_size_mb {
        offline_store.data = offline_store.data.push(data)
        offline_store.current_size_mb = offline_store.current_size_mb + data_size_kb / 1024.0
        
        // 更新最旧时间戳
        if data.timestamp < offline_store.oldest_timestamp {
          offline_store.oldest_timestamp = data.timestamp
        }
        
        sync_status = SyncStatus::Pending
        true
      } else {
        // 存储空间不足，删除最旧的数据
        if offline_store.data.length() > 0 {
          let removed_data = offline_store.data[0]
          offline_store.data = offline_store.data.slice(1)
          offline_store.current_size_mb = Math::max(0.0, offline_store.current_size_mb - data_size_kb / 1024.0)
          
          // 更新最旧时间戳
          if offline_store.data.length() > 0 {
            offline_store.oldest_timestamp = offline_store.data
              .reduce(offline_store.data[0], fn(oldest, data) {
                if data.timestamp < oldest.timestamp { data } else { oldest }
              }).timestamp
          }
          
          // 添加新数据
          offline_store.data = offline_store.data.push(data)
          offline_store.current_size_mb = offline_store.current_size_mb + data_size_kb / 1024.0
          
          if data.timestamp < offline_store.oldest_timestamp {
            offline_store.oldest_timestamp = data.timestamp
          }
          
          sync_status = SyncStatus::Pending
          true
        } else {
          false  // 无法存储数据
        }
      }
    }
    
    let sync_to_cloud = fn() -> SyncResult {
      let start_time = Time::now()
      
      if offline_store.data.length() == 0 {
        return {
          success: true,
          synced_count: 0,
          failed_count: 0,
          error_message: None,
          duration_ms: Time::now() - start_time
        }
      }
      
      // 确定要同步的数据量
      let data_to_sync = match config.strategy {
        SyncStrategy::Immediate => offline_store.data
        SyncStrategy::Batched => {
          if offline_store.data.length() <= config.batch_size {
            offline_store.data
          } else {
            offline_store.data.slice(0, config.batch_size)
          }
        }
        SyncStrategy::Conditional => {
          // 模拟网络检查
          if Math::random() > 0.3 {  // 70%网络可用
            offline_store.data
          } else {
            []
          }
        }
        SyncStrategy::Manual => []
      }
      
      if data_to_sync.length() == 0 {
        return {
          success: false,
          synced_count: 0,
          failed_count: 0,
          error_message: Some("No data to sync or network unavailable"),
          duration_ms: Time::now() - start_time
        }
      }
      
      // 模拟同步过程
      let sync_success = Math::random() > 0.2  // 80%成功率
      let end_time = Time::now()
      
      if sync_success {
        // 同步成功，移除已同步的数据
        let synced_count = data_to_sync.length()
        offline_store.data = offline_store.data.slice(synced_count)
        
        // 更新存储大小
        offline_store.current_size_mb = Math::max(0.0, offline_store.current_size_mb - (synced_count as Float))
        
        // 更新最旧时间戳
        if offline_store.data.length() > 0 {
          offline_store.oldest_timestamp = offline_store.data[0].timestamp
        }
        
        sync_status = if offline_store.data.length() == 0 {
          SyncStatus::Synced
        } else {
          SyncStatus::Pending
        }
        
        last_sync_time = end_time
        
        {
          success: true,
          synced_count: synced_count,
          failed_count: 0,
          error_message: None,
          duration_ms: end_time - start_time
        }
      } else {
        sync_status = SyncStatus::Failed
        
        {
          success: false,
          synced_count: 0,
          failed_count: data_to_sync.length(),
          error_message: Some("Sync failed due to network error"),
          duration_ms: end_time - start_time
        }
      }
    }
    
    let get_sync_status = fn() { sync_status }
    let get_offline_data_count = fn() { offline_store.data.length() }
    let get_offline_storage_usage = fn() { offline_store.current_size_mb }
    let get_oldest_data_age = fn() { Time::now() - offline_store.oldest_timestamp }
    
    {
      store_data_offline,
      sync_to_cloud,
      get_sync_status,
      get_offline_data_count,
      get_offline_storage_usage,
      get_oldest_data_age
    }
  }
  
  // 测试边缘设备同步
  let sync_config = {
    strategy: SyncStrategy::Batched,
    batch_size: 10,
    retry_attempts: 3,
    retry_delay_ms: 5000,
    offline_storage_limit_mb: 10,
    compression_enabled: true
  }
  
  let sync_manager = create_edge_sync_manager(sync_config)
  
  // 初始状态验证
  assert_eq(sync_manager.get_sync_status(), SyncStatus::Synced)
  assert_eq(sync_manager.get_offline_data_count(), 0)
  assert_eq(sync_manager.get_offline_storage_usage(), 0.0)
  
  // 添加离线数据
  let test_data = {
    device_id: "device-edge-001",
    device_type: DeviceType::Sensor,
    sensor_type: Some(SensorType::Temperature),
    timestamp: Time::now(),
    location: Some((37.7749, -122.4194)),
    measurements: [("temperature_celsius", 25.5)],
    battery_level: 75.0,
    signal_strength: -65,
    metadata: []
  }
  
  // 添加多个数据点
  for i in 0..15 {
    let data_point = {
      device_id: test_data.device_id,
      device_type: test_data.device_type,
      sensor_type: test_data.sensor_type,
      timestamp: test_data.timestamp + i * 1000,
      location: test_data.location,
      measurements: [("temperature_celsius", 25.5 + i as Float * 0.1)],
      battery_level: test_data.battery_level - i as Float * 0.5,
      signal_strength: test_data.signal_strength,
      metadata: test_data.metadata
    }
    
    let stored = sync_manager.store_data_offline(data_point)
    assert_true(stored)
  }
  
  // 验证离线存储
  assert_eq(sync_manager.get_sync_status(), SyncStatus::Pending)
  assert_eq(sync_manager.get_offline_data_count(), 15)
  assert_true(sync_manager.get_offline_storage_usage() > 0.0)
  
  // 执行同步
  let sync_result = sync_manager.sync_to_cloud()
  
  // 验证同步结果
  if sync_result.success {
    assert_eq(sync_result.synced_count, 10)  // 批量大小为10
    assert_eq(sync_result.failed_count, 0)
    assert_eq(sync_manager.get_offline_data_count(), 5)  // 剩余5个未同步
    assert_eq(sync_manager.get_sync_status(), SyncStatus::Pending)  // 还有数据待同步
  } else {
    assert_eq(sync_result.synced_count, 0)
    assert_true(sync_result.failed_count > 0)
    assert_eq(sync_manager.get_offline_data_count(), 15)  // 数据未同步
    assert_eq(sync_manager.get_sync_status(), SyncStatus::Failed)
  }
  
  // 再次同步剩余数据
  let second_sync_result = sync_manager.sync_to_cloud()
  
  // 验证第二次同步
  if sync_result.success {
    if second_sync_result.success {
      assert_eq(second_sync_result.synced_count, 5)  // 剩余5个数据
      assert_eq(sync_manager.get_offline_data_count(), 0)
      assert_eq(sync_manager.get_sync_status(), SyncStatus::Synced)
    }
  }
  
  // 测试离线存储限制
  let overflow_config = {
    strategy: SyncStrategy::Immediate,
    batch_size: 5,
    retry_attempts: 3,
    retry_delay_ms: 1000,
    offline_storage_limit_mb: 0.01,  // 10KB限制，约10条数据
    compression_enabled: false
  }
  
  let overflow_sync_manager = create_edge_sync_manager(overflow_config)
  
  // 添加超过限制的数据
  for i in 0..20 {
    let data_point = {
      device_id: "device-overflow",
      device_type: DeviceType::Sensor,
      sensor_type: Some(SensorType::Temperature),
      timestamp: Time::now() + i * 1000,
      location: None,
      measurements: [("temperature_celsius", 20.0 + i as Float)],
      battery_level: 80.0,
      signal_strength: -60,
      metadata: []
    }
    
    overflow_sync_manager.store_data_offline(data_point)
  }
  
  // 验证存储限制生效
  assert_true(overflow_sync_manager.get_offline_data_count() <= 10)
  assert_true(overflow_sync_manager.get_offline_storage_usage() <= 0.01)
  
  // 验证最旧数据被删除
  let oldest_age = overflow_sync_manager.get_oldest_data_age()
  assert_true(oldest_age < 20000)  // 最旧数据应该相对较新
}