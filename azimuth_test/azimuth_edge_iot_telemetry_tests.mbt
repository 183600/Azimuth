// Azimuth Edge IoT Telemetry Test Suite
// This file contains test cases for edge computing and IoT telemetry functionality

// Test 1: Edge Device Telemetry Collection
test "edge device telemetry collection" {
  // Define edge device structure
  type EdgeDevice = {
    device_id: String,
    device_type: String,
    location: String,
    firmware_version: String,
    last_seen: Int,
    status: String,
    capabilities: Array[String],
    metadata: Array[(String, String)]
  }
  
  type DeviceTelemetry = {
    device_id: String,
    timestamp: Int,
    metrics: Array[(String, Float)],
    events: Array[DeviceEvent],
    battery_level: Option[Int],
    signal_strength: Option[Int],
    location: Option[(Float, Float)]
  }
  
  type DeviceEvent = {
    event_type: String,
    timestamp: Int,
    severity: String,
    message: String,
    data: Array[(String, String)]
  }
  
  // Create edge devices
  let edge_devices = [
    {
      device_id: "sensor-001",
      device_type: "temperature_sensor",
      location: "warehouse-a",
      firmware_version: "1.2.3",
      last_seen: 1640995200,
      status: "online",
      capabilities: ["temperature", "humidity", "pressure"],
      metadata: [
        ("model", "DHT22"),
        ("manufacturer", "SensorTech"),
        ("installation_date", "2022-01-01")
      ]
    },
    {
      device_id: "gateway-001",
      device_type: "iot_gateway",
      location: "warehouse-a",
      firmware_version: "2.1.0",
      last_seen: 1640995195,
      status: "online",
      capabilities: ["wifi", "bluetooth", "lora"],
      metadata: [
        ("model", "Gateway-X1"),
        ("manufacturer", "IoT Solutions"),
        ("installation_date", "2021-12-15")
      ]
    },
    {
      device_id: "actuator-001",
      device_type: "smart_valve",
      location: "warehouse-b",
      firmware_version: "1.0.5",
      last_seen: 1640995100,
      status: "offline",
      capabilities: ["open", "close", "position"],
      metadata: [
        ("model", "Valve-Pro"),
        ("manufacturer", "Industrial Controls"),
        ("installation_date", "2022-01-10")
      ]
    }
  ]
  
  // Create telemetry data
  let telemetry_data = [
    {
      device_id: "sensor-001",
      timestamp: 1640995200,
      metrics: [
        ("temperature", 23.5),
        ("humidity", 65.2),
        ("pressure", 1013.25)
      ],
      events: [
        {
          event_type: "reading_complete",
          timestamp: 1640995200,
          severity: "info",
          message: "Sensor reading completed successfully",
          data: [
            ("reading_id", "read-001"),
            ("duration_ms", "150")
          ]
        }
      ],
      battery_level: Some(85),
      signal_strength: Some(75),
      location: Some((47.6062, -122.3321))
    },
    {
      device_id: "gateway-001",
      timestamp: 1640995195,
      metrics: [
        ("cpu_usage", 15.3),
        ("memory_usage", 45.7),
        ("network_throughput", 1024.5)
      ],
      events: [
        {
          event_type: "device_connected",
          timestamp: 1640995100,
          severity: "info",
          message: "New device connected to gateway",
          data: [
            ("connected_device", "sensor-001"),
            ("connection_type", "bluetooth")
          ]
        }
      ],
      battery_level: None,  // Gateway is powered
      signal_strength: Some(90),
      location: Some((47.6062, -122.3321))
    }
  ]
  
  // Filter devices by status
  let filter_devices_by_status = fn(devices: Array[EdgeDevice], status: String) {
    devices.filter(fn(d) { d.status == status })
  }
  
  let online_devices = filter_devices_by_status(edge_devices, "online")
  assert_eq(online_devices.length(), 2)
  assert_true(online_devices.any(fn(d) { d.device_id == "sensor-001" }))
  assert_true(online_devices.any(fn(d) { d.device_id == "gateway-001" }))
  
  let offline_devices = filter_devices_by_status(edge_devices, "offline")
  assert_eq(offline_devices.length(), 1)
  assert_eq(offline_devices[0].device_id, "actuator-001")
  
  // Filter devices by capability
  let filter_devices_by_capability = fn(devices: Array[EdgeDevice], capability: String) {
    devices.filter(fn(d) { d.capabilities.contains(capability) })
  }
  
  let temp_sensors = filter_devices_by_capability(edge_devices, "temperature")
  assert_eq(temp_sensors.length(), 1)
  assert_eq(temp_sensors[0].device_id, "sensor-001")
  
  let wifi_devices = filter_devices_by_capability(edge_devices, "wifi")
  assert_eq(wifi_devices.length(), 1)
  assert_eq(wifi_devices[0].device_id, "gateway-001")
  
  // Find devices with low battery
  let find_low_battery_devices = fn(telemetry: Array[DeviceTelemetry], threshold: Int) {
    telemetry.filter(fn(t) {
      match t.battery_level {
        Some(level) => level < threshold
        None => false
      }
    }).map(fn(t) { t.device_id })
  }
  
  let low_battery_devices = find_low_battery_devices(telemetry_data, 30)
  assert_eq(low_battery_devices.length(), 0)  // No devices below 30%
  
  let critical_battery_devices = find_low_battery_devices(telemetry_data, 90)
  assert_eq(critical_battery_devices.length(), 0)  // No devices below 90%
  
  // Calculate average metrics by device type
  let calculate_avg_metrics_by_device_type = fn(devices: Array[EdgeDevice], telemetry: Array[DeviceTelemetry], metric_name: String) {
    let mut results = []
    let mut processed_types = []
    
    for device in devices {
      if not(processed_types.contains(device.device_type)) {
        processed_types = processed_types.push(device.device_type)
        
        let device_ids = devices
          .filter(fn(d) { d.device_type == device.device_type })
          .map(fn(d) { d.device_id })
        
        let device_telemetry = telemetry.filter(fn(t) { device_ids.contains(t.device_id) })
        
        let metric_values = device_telemetry.flat_map(fn(t) {
          t.metrics.filter(fn(m) { m.0 == metric_name }).map(fn(m) { m.1 })
        })
        
        let avg_value = if metric_values.length() > 0 {
          metric_values.reduce(fn(acc, v) { acc + v }, 0.0) / metric_values.length().to_float()
        } else {
          0.0
        }
        
        results = results.push((device.device_type, avg_value))
      }
    }
    
    results
  }
  
  let avg_temps = calculate_avg_metrics_by_device_type(edge_devices, telemetry_data, "temperature")
  assert_eq(avg_temps.length(), 1)
  assert_eq(avg_temps[0].0, "temperature_sensor")
  assert_eq(avg_temps[0].1, 23.5)
  
  // Find devices with critical events
  let find_devices_with_critical_events = fn(telemetry: Array[DeviceTelemetry]) {
    telemetry.filter(fn(t) {
      t.events.any(fn(e) { e.severity == "critical" })
    }).map(fn(t) { t.device_id })
  }
  
  let critical_event_devices = find_devices_with_critical_events(telemetry_data)
  assert_eq(critical_event_devices.length(), 0)  // No critical events
  
  // Add a critical event and test again
  let telemetry_with_critical = telemetry_data.map(fn(t) {
    if t.device_id == "sensor-001" {
      let critical_event = {
        event_type: "sensor_failure",
        timestamp: 1640995250,
        severity: "critical",
        message: "Sensor component failure detected",
        data: [
          ("component", "temperature_sensor"),
          ("error_code", "E001")
        ]
      }
      
      { t | events: t.events.push(critical_event) }
    } else {
      t
    }
  })
  
  let critical_devices = find_devices_with_critical_events(telemetry_with_critical)
  assert_eq(critical_devices.length(), 1)
  assert_eq(critical_devices[0], "sensor-001")
}

// Test 2: Edge Computing Data Processing
test "edge computing data processing" {
  // Define edge processing pipeline
  type ProcessingPipeline = {
    pipeline_id: String,
    name: String,
    stages: Array[ProcessingStage],
    enabled: Bool
  }
  
  type ProcessingStage = {
    stage_id: String,
    name: String,
    type: String,
    config: Array[(String, String)],
    enabled: Bool
  }
  
  type ProcessingResult = {
    pipeline_id: String,
    input_data: Array[(String, Float)],
    output_data: Array[(String, Float)],
    processing_time: Int,
    stages_executed: Array[String],
    errors: Array[String]
  }
  
  // Create processing pipelines
  let pipelines = [
    {
      pipeline_id: "temp-filtering",
      name: "Temperature Data Filtering",
      stages: [
        {
          stage_id: "validation",
          name: "Data Validation",
          type: "filter",
          config: [
            ("min_temp", "-50.0"),
            ("max_temp", "100.0")
          ],
          enabled: true
        },
        {
          stage_id: "smoothing",
          name: "Data Smoothing",
          type: "transform",
          config: [
            ("window_size", "5"),
            ("method", "moving_average")
          ],
          enabled: true
        },
        {
          stage_id: "threshold",
          name: "Threshold Detection",
          type: "alert",
          config: [
            ("min_threshold", "10.0"),
            ("max_threshold", "30.0")
          ],
          enabled: true
        }
      ],
      enabled: true
    },
    {
      pipeline_id: "battery-monitoring",
      name: "Battery Level Monitoring",
      stages: [
        {
          stage_id: "level-check",
          name: "Battery Level Check",
          type: "filter",
          config: [
            ("critical_level", "20"),
            ("low_level", "50")
          ],
          enabled: true
        },
        {
          stage_id: "trend-analysis",
          name: "Battery Trend Analysis",
          type: "transform",
          config: [
            ("time_window", "24h"),
            ("trend_threshold", "5")
          ],
          enabled: true
        }
      ],
      enabled: true
    }
  ]
  
  // Simulate input data
  let input_data = [
    ("temperature", 25.3),
    ("humidity", 65.2),
    ("battery_level", 85.0),
    ("signal_strength", 75.0),
    ("pressure", 1013.25)
  ]
  
  // Simulate data validation stage
  let validate_data = fn(data: Array[(String, Float)], min_temp: Float, max_temp: Float) {
    data.filter(fn(item) {
      let (key, value) = item
      if key == "temperature" {
        value >= min_temp and value <= max_temp
      } else {
        true
      }
    })
  }
  
  // Simulate data smoothing stage
  let smooth_data = fn(data: Array[(String, Float)], window_size: Int) {
    // Simplified smoothing - just return the data as-is
    // In a real implementation, this would apply moving average
    data
  }
  
  // Simulate threshold detection stage
  let detect_thresholds = fn(data: Array[(String, Float)], min_threshold: Float, max_threshold: Float) {
    let alerts = []
    
    for item in data {
      let (key, value) = item
      if key == "temperature" {
        if value < min_threshold {
          alerts = alerts.push("Temperature below minimum threshold: " + value.to_string())
        } else if value > max_threshold {
          alerts = alerts.push("Temperature above maximum threshold: " + value.to_string())
        }
      }
    }
    
    alerts
  }
  
  // Execute temperature filtering pipeline
  let execute_temp_pipeline = fn(pipeline: ProcessingPipeline, data: Array[(String, Float)]) {
    let mut current_data = data
    let mut stages_executed = []
    let mut errors = []
    let mut alerts = []
    
    for stage in pipeline.stages {
      if stage.enabled {
        stages_executed = stages_executed.push(stage.stage_id)
        
        match stage.type {
          "filter" => {
            if stage.stage_id == "validation" {
              let min_temp = stage.config.filter(fn(c) { c.0 == "min_temp" })[0].1.to_float()
              let max_temp = stage.config.filter(fn(c) { c.0 == "max_temp" })[0].1.to_float()
              current_data = validate_data(current_data, min_temp, max_temp)
            }
          }
          
          "transform" => {
            if stage.stage_id == "smoothing" {
              let window_size = stage.config.filter(fn(c) { c.0 == "window_size" })[0].1.to_int()
              current_data = smooth_data(current_data, window_size)
            }
          }
          
          "alert" => {
            if stage.stage_id == "threshold" {
              let min_threshold = stage.config.filter(fn(c) { c.0 == "min_threshold" })[0].1.to_float()
              let max_threshold = stage.config.filter(fn(c) { c.0 == "max_threshold" })[0].1.to_float()
              alerts = detect_thresholds(current_data, min_threshold, max_threshold)
            }
          }
          
          _ => {
            errors = errors.push("Unknown stage type: " + stage.type)
          }
        }
      }
    }
    
    {
      pipeline_id: pipeline.pipeline_id,
      input_data: data,
      output_data: current_data,
      processing_time: 100,  // Simulated processing time
      stages_executed,
      errors
    }
  }
  
  let temp_pipeline = pipelines.filter(fn(p) { p.pipeline_id == "temp-filtering" })[0]
  let temp_result = execute_temp_pipeline(temp_pipeline, input_data)
  
  assert_eq(temp_result.pipeline_id, "temp-filtering")
  assert_eq(temp_result.stages_executed.length(), 3)
  assert_true(temp_result.stages_executed.contains("validation"))
  assert_true(temp_result.stages_executed.contains("smoothing"))
  assert_true(temp_result.stages_executed.contains("threshold"))
  assert_eq(temp_result.errors.length(), 0)
  
  // Test with out-of-range temperature
  let out_of_range_data = [
    ("temperature", 105.0),  // Above max temp (100.0)
    ("humidity", 65.2),
    ("battery_level", 85.0)
  ]
  
  let out_of_range_result = execute_temp_pipeline(temp_pipeline, out_of_range_data)
  
  // The validation stage should filter out the out-of-range temperature
  assert_eq(out_of_range_result.output_data.length(), 2)  // temperature filtered out
  assert_false(out_of_range_result.output_data.any(fn(item) { item.0 == "temperature" }))
  
  // Test with below-range temperature
  let below_range_data = [
    ("temperature", -60.0),  // Below min temp (-50.0)
    ("humidity", 65.2),
    ("battery_level", 85.0)
  ]
  
  let below_range_result = execute_temp_pipeline(temp_pipeline, below_range_data)
  
  // The validation stage should filter out the below-range temperature
  assert_eq(below_range_result.output_data.length(), 2)  // temperature filtered out
  assert_false(below_range_result.output_data.any(fn(item) { item.0 == "temperature" }))
  
  // Execute battery monitoring pipeline
  let execute_battery_pipeline = fn(pipeline: ProcessingPipeline, data: Array[(String, Float)]) {
    let mut current_data = data
    let mut stages_executed = []
    let mut errors = []
    let mut alerts = []
    
    for stage in pipeline.stages {
      if stage.enabled {
        stages_executed = stages_executed.push(stage.stage_id)
        
        match stage.type {
          "filter" => {
            if stage.stage_id == "level-check" {
              let critical_level = stage.config.filter(fn(c) { c.0 == "critical_level" })[0].1.to_float()
              let low_level = stage.config.filter(fn(c) { c.0 == "low_level" })[0].1.to_float()
              
              for item in current_data {
                let (key, value) = item
                if key == "battery_level" {
                  if value <= critical_level {
                    alerts = alerts.push("Battery at critical level: " + value.to_string())
                  } else if value <= low_level {
                    alerts = alerts.push("Battery at low level: " + value.to_string())
                  }
                }
              }
            }
          }
          
          "transform" => {
            if stage.stage_id == "trend-analysis" {
              // Simulate trend analysis
              for item in current_data {
                let (key, value) = item
                if key == "battery_level" {
                  if value < 30.0 {
                    alerts = alerts.push("Battery declining trend detected")
                  }
                }
              }
            }
          }
          
          _ => {
            errors = errors.push("Unknown stage type: " + stage.type)
          }
        }
      }
    }
    
    {
      pipeline_id: pipeline.pipeline_id,
      input_data: data,
      output_data: current_data,
      processing_time: 50,  // Simulated processing time
      stages_executed,
      errors
    }
  }
  
  let battery_pipeline = pipelines.filter(fn(p) { p.pipeline_id == "battery-monitoring" })[0]
  let battery_result = execute_battery_pipeline(battery_pipeline, input_data)
  
  assert_eq(battery_result.pipeline_id, "battery-monitoring")
  assert_eq(battery_result.stages_executed.length(), 2)
  assert_true(battery_result.stages_executed.contains("level-check"))
  assert_true(battery_result.stages_executed.contains("trend-analysis"))
  
  // Test with low battery
  let low_battery_data = [
    ("temperature", 25.3),
    ("humidity", 65.2),
    ("battery_level", 25.0),  // Low battery
    ("signal_strength", 75.0)
  ]
  
  let low_battery_result = execute_battery_pipeline(battery_pipeline, low_battery_data)
  
  assert_eq(low_battery_result.pipeline_id, "battery-monitoring")
  assert_eq(low_battery_result.stages_executed.length(), 2)
  
  // Test with critical battery
  let critical_battery_data = [
    ("temperature", 25.3),
    ("humidity", 65.2),
    ("battery_level", 15.0),  // Critical battery
    ("signal_strength", 75.0)
  ]
  
  let critical_battery_result = execute_battery_pipeline(battery_pipeline, critical_battery_data)
  
  assert_eq(critical_battery_result.pipeline_id, "battery-monitoring")
  assert_eq(critical_battery_result.stages_executed.length(), 2)
  
  // Calculate pipeline performance metrics
  let calculate_pipeline_metrics = fn(results: Array[ProcessingResult]) {
    let total_pipelines = results.length()
    let total_processing_time = results.reduce(fn(acc, r) { acc + r.processing_time }, 0)
    let avg_processing_time = if total_pipelines > 0 {
      total_processing_time / total_pipelines
    } else {
      0
    }
    
    let total_errors = results.reduce(fn(acc, r) { acc + r.errors.length() }, 0)
    let error_rate = if total_pipelines > 0 {
      (total_errors.to_float() / total_pipelines.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      total_pipelines,
      avg_processing_time,
      total_errors,
      error_rate
    }
  }
  
  let all_results = [temp_result, out_of_range_result, below_range_result, battery_result, low_battery_result, critical_battery_result]
  let metrics = calculate_pipeline_metrics(all_results)
  
  assert_eq(metrics.total_pipelines, 6)
  assert_eq(metrics.avg_processing_time, 75)  // (100 + 100 + 100 + 50 + 50 + 50) / 6
  assert_eq(metrics.total_errors, 0)
  assert_eq(metrics.error_rate, 0.0)
}