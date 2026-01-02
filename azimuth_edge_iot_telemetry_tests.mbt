// Azimuth Edge Computing and IoT Telemetry Tests
// 边缘计算和IoT遥测测试用例 - 专注于资源受限环境、离线处理和设备管理

// Test 1: 边缘设备遥测数据收集
test "edge device telemetry data collection" {
  // 创建边缘设备管理器
  let device_manager = EdgeDeviceManager::new()
  
  // 注册边缘设备
  let devices = [
    ("sensor-001", "temperature_sensor", "factory-floor-a"),
    ("sensor-002", "humidity_sensor", "factory-floor-a"),
    ("sensor-003", "pressure_sensor", "factory-floor-b"),
    ("gateway-001", "iot_gateway", "factory-floor-a"),
    ("edge-node-001", "edge_compute_node", "factory-floor-b")
  ]
  
  let registered_devices = []
  for (device_id, device_type, location) in devices {
    let device = EdgeDevice::new(device_id, device_type)
    EdgeDevice::set_location(device, location)
    EdgeDevice::set_capabilities(device, [
      "telemetry_collection",
      "local_processing",
      "batch_upload"
    ])
    EdgeDevice::set_resource_limits(device, ResourceLimits::new(
      memory_mb = 128,
      storage_mb = 512,
      cpu_percent = 80.0
    ))
    
    let registration = DeviceManager::register_device(device_manager, device)
    match registration {
      Ok(device_token) => {
        registered_devices = registered_devices.push((device_id, device_token))
        assert_true(device_token.length() > 0)
      }
      Err(_) => assert_true(false)
    }
  }
  
  // 创建遥测收集器
  let telemetry_collector = EdgeTelemetryCollector::new(device_manager)
  
  // 模拟传感器数据收集
  let base_time = Time::now()
  for i in 0..<100 {
    for (device_id, _) in registered_devices {
      if device_id.contains("sensor") {
        // 传感器数据
        let temperature = 20.0 + Math::sin(i.to_float() * 0.1) * 5.0 + Math::random() * 2.0
        let humidity = 50.0 + Math::cos(i.to_float() * 0.1) * 10.0 + Math::random() * 5.0
        let pressure = 1013.25 + Math::sin(i.to_float() * 0.05) * 20.0 + Math::random() * 10.0
        
        let attrs = Attributes::new()
        Attributes::set(attrs, "device.id", StringValue(device_id))
        Attributes::set(attrs, "device.type", StringValue("sensor"))
        Attributes::set(attrs, "location", StringValue(if device_id.contains("001") { "factory-floor-a" } else { "factory-floor-b" }))
        Attributes::set(attrs, "timestamp", IntValue(base_time + i * 1000))
        
        if device_id.contains("temperature") {
          Collector::record_metric(telemetry_collector, "temperature", temperature, attrs)
        } else if device_id.contains("humidity") {
          Collector::record_metric(telemetry_collector, "humidity", humidity, attrs)
        } else if device_id.contains("pressure") {
          Collector::record_metric(telemetry_collector, "pressure", pressure, attrs)
        }
      } else if device_id.contains("gateway") {
        // 网关状态数据
        let cpu_usage = 30.0 + Math::random() * 40.0
        let memory_usage = 40.0 + Math::random() * 30.0
        let network_throughput = 1000.0 + Math::random() * 5000.0
        
        let attrs = Attributes::new()
        Attributes::set(attrs, "device.id", StringValue(device_id))
        Attributes::set(attrs, "device.type", StringValue("gateway"))
        Attributes::set(attrs, "location", StringValue("factory-floor-a"))
        Attributes::set(attrs, "timestamp", IntValue(base_time + i * 1000))
        
        Collector::record_metric(telemetry_collector, "cpu_usage", cpu_usage, attrs)
        Collector::record_metric(telemetry_collector, "memory_usage", memory_usage, attrs)
        Collector::record_metric(telemetry_collector, "network_throughput", network_throughput, attrs)
      }
    }
  }
  
  // 验证数据收集
  let collected_data = Collector::get_collected_data(telemetry_collector)
  assert_true(collected_data.length() > 0)
  
  // 验证设备数据分布
  let device_data = Collector::group_by_device(telemetry_collector)
  assert_eq(device_data.length(), 5)
  
  for (device_id, metrics) in device_data {
    assert_true(metrics.length() > 0)
    assert_true(metrics[0].attributes.contains("device.id"))
    assert_true(metrics[0].attributes.contains("timestamp"))
  }
  
  // 测试本地数据处理
  let local_processor = LocalDataProcessor::new()
  let processed_data = Processor::process_metrics(local_processor, collected_data)
  
  // 验证数据处理结果
  assert_true(processed_data.length() > 0)
  for data in processed_data {
    assert_true(data.processed)
    assert_true(data.aggregations.length() > 0)
  }
}

// Test 2: 离线数据处理和同步
test "offline data processing and synchronization" {
  // 创建离线数据管理器
  let offline_manager = OfflineDataManager::new()
  
  // 配置离线存储
  let storage_config = OfflineStorageConfig::new(
    max_size_mb = 100,
    max_files = 1000,
    compression_enabled = true,
    encryption_enabled = true
  )
  OfflineDataManager::configure_storage(offline_manager, storage_config)
  
  // 模拟网络断开情况下的数据收集
  let device_id = "edge-device-offline"
  let offline_collector = OfflineTelemetryCollector::new(device_id, offline_manager)
  
  // 收集离线数据
  let base_time = Time::now()
  for i in 0..<200 {
    let temperature = 25.0 + Math::sin(i.to_float() * 0.1) * 3.0
    let humidity = 60.0 + Math::cos(i.to_float() * 0.1) * 8.0
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "device.id", StringValue(device_id))
    Attributes::set(attrs, "timestamp", IntValue(base_time + i * 5000)) // 每5秒一个数据点
    Attributes::set(attrs, "offline_mode", BoolValue(true))
    
    OfflineCollector::record_metric(offline_collector, "temperature", temperature, attrs)
    OfflineCollector::record_metric(offline_collector, "humidity", humidity, attrs)
    
    // 每10个数据点保存一次
    if i % 10 == 0 {
      OfflineCollector::save_to_local_storage(offline_collector)
    }
  }
  
  // 验证离线数据存储
  let stored_files = OfflineDataManager::get_stored_files(offline_manager)
  assert_true(stored_files.length() > 0)
  
  let total_size = OfflineDataManager::get_total_storage_size(offline_manager)
  assert_true(total_size > 0)
  assert_true(total_size <= storage_config.max_size_mb * 1024 * 1024)
  
  // 测试本地数据聚合
  let local_aggregator = OfflineDataAggregator::new(offline_manager)
  let aggregated_data = Aggregator::aggregate_by_time_window(local_aggregator, 60000) // 1分钟窗口
  
  assert_true(aggregated_data.length() > 0)
  for aggregation in aggregated_data {
    assert_true(aggregation.min <= aggregation.max)
    assert_true(aggregation.count > 0)
    assert_true(aggregation.average >= aggregation.min && aggregation.average <= aggregation.max)
  }
  
  // 模拟网络恢复后的数据同步
  let sync_manager = DataSyncManager::new()
  SyncManager::configure_batch_size(sync_manager, 50) // 每批50个数据点
  SyncManager::configure_retry_policy(sync_manager, RetryPolicy::new(
    max_attempts = 3,
    backoff_ms = 1000,
    max_backoff_ms = 10000
  ))
  
  // 模拟同步过程
  let sync_result = SyncManager::sync_offline_data(sync_manager, offline_manager)
  match sync_result {
    Ok(sync_stats) => {
      assert_true(sync_stats.total_data_points > 0)
      assert_true(sync_stats.synced_data_points > 0)
      assert_true(sync_stats.failed_data_points == 0)
      assert_true(sync_stats.sync_duration_ms > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 验证同步后的清理
  let cleanup_result = OfflineDataManager::cleanup_synced_data(offline_manager)
  assert_true(cleanup_result.files_deleted > 0)
  
  let remaining_files = OfflineDataManager::get_stored_files(offline_manager)
  assert_true(remaining_files.length() < stored_files.length())
}

// Test 3: 边缘设备资源优化
test "edge device resource optimization" {
  // 创建资源优化管理器
  let resource_manager = EdgeResourceManager::new()
  
  // 配置资源限制
  let resource_limits = ResourceLimits::new(
    memory_mb = 64,
    storage_mb = 256,
    cpu_percent = 70.0,
    network_bandwidth_kbps = 1000
  )
  ResourceManager::set_limits(resource_manager, resource_limits)
  
  // 创建资源监控器
  let resource_monitor = ResourceMonitor::new(resource_manager)
  
  // 模拟资源使用情况
  let initial_usage = ResourceUsage::new(
    memory_mb = 32,
    storage_mb = 128,
    cpu_percent = 40.0,
    network_bandwidth_kbps = 500
  )
  
  Monitor::record_usage(resource_monitor, initial_usage)
  
  // 测试资源优化策略
  let optimization_strategies = [
    OptimizationStrategy::DataCompression,
    OptimizationStrategy::BatchProcessing,
    OptimizationStrategy::AdaptiveSampling,
    OptimizationStrategy::LocalAggregation,
    OptimizationStrategy::SmartCaching
  ]
  
  for strategy in optimization_strategies {
    ResourceManager::add_optimization_strategy(resource_manager, strategy)
  }
  
  // 模拟高负载情况
  for i in 0..<50 {
    let memory_usage = 32 + i * 0.6 // 逐渐增加内存使用
    let storage_usage = 128 + i * 2.5 // 逐渐增加存储使用
    let cpu_usage = 40 + i * 0.8 // 逐渐增加CPU使用
    let network_usage = 500 + i * 10 // 逐渐增加网络使用
    
    let current_usage = ResourceUsage::new(
      memory_mb = memory_usage,
      storage_mb = storage_usage,
      cpu_percent = cpu_usage,
      network_bandwidth_kbps = network_usage
    )
    
    Monitor::record_usage(resource_monitor, current_usage)
    
    // 检查资源优化触发
    let optimization_actions = ResourceManager::check_optimization_triggers(resource_manager)
    if optimization_actions.length() > 0 {
      for action in optimization_actions {
        assert_true(action.strategy != OptimizationStrategy::None)
        assert_true(action.priority >= 0)
        
        // 应用优化措施
        let apply_result = ResourceManager::apply_optimization(resource_manager, action)
        assert_true(apply_result.success)
      }
    }
  }
  
  // 验证资源优化效果
  let final_usage = Monitor::get_current_usage(resource_monitor)
  assert_true(final_usage.memory_mb <= resource_limits.memory_mb)
  assert_true(final_usage.storage_mb <= resource_limits.storage_mb)
  assert_true(final_usage.cpu_percent <= resource_limits.cpu_percent)
  
  // 验证优化策略效果
  let optimization_stats = ResourceManager::get_optimization_statistics(resource_manager)
  assert_true(optimization_stats.total_optimizations > 0)
  assert_true(optimization_stats.memory_saved_mb > 0)
  assert_true(optimization_stats.storage_saved_mb > 0)
  assert_true(optimization_stats.cpu_reduction_percent > 0)
}

// Test 4: IoT设备生命周期管理
test "IoT device lifecycle management" {
  // 创建设备生命周期管理器
  let lifecycle_manager = DeviceLifecycleManager::new()
  
  // 设备注册阶段
  let device_id = "iot-sensor-lifecycle"
  let device_info = DeviceInfo::new(
    device_id = device_id,
    device_type = "environmental_sensor",
    manufacturer = "SensorCorp",
    model = "ENV-2000",
    firmware_version = "1.2.3",
    hardware_version = "2.1"
  )
  
  let registration_result = LifecycleManager::register_device(lifecycle_manager, device_info)
  match registration_result {
    Ok(device_credential) => {
      assert_true(device_credential.device_id == device_id)
      assert_true(device_credential.access_token.length() > 0)
      assert_true(device_credential.expires_at > Time::now())
    }
    Err(_) => assert_true(false)
  }
  
  // 设备配置阶段
  let device_config = DeviceConfig::new()
  Config::set_telemetry_interval(device_config, 30) // 30秒间隔
  Config::set_batch_size(device_config, 10) // 批量大小10
  Config::enable_compression(device_config, true)
  Config::enable_encryption(device_config, true)
  Config::set_sampling_rate(device_config, 0.8) // 80%采样率
  
  let config_result = LifecycleManager::configure_device(lifecycle_manager, device_id, device_config)
  match config_result {
    Ok(config_version) => {
      assert_true(config_version > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 设备激活阶段
  let activation_result = LifecycleManager::activate_device(lifecycle_manager, device_id)
  match activation_result {
    Ok(activation_time) => {
      assert_true(activation_time > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 验证设备状态
  let device_status = LifecycleManager::get_device_status(lifecycle_manager, device_id)
  match device_status {
    Some(status) => {
      assert_eq(status.state, DeviceState::Active)
      assert_eq(status.last_seen > 0, true)
      assert_eq(status.config_applied, true)
    }
    None => assert_true(false)
  }
  
  // 模拟设备运行
  let telemetry_simulator = DeviceTelemetrySimulator::new(device_id)
  for i in 0..<20 {
    let temperature = 22.0 + Math::sin(i.to_float() * 0.2) * 4.0
    let humidity = 55.0 + Math::cos(i.to_float() * 0.2) * 10.0
    let battery_level = 100.0 - i.to_float() * 2.0 // 模拟电池消耗
    
    Simulator::send_telemetry(telemetry_simulator, [
      ("temperature", temperature),
      ("humidity", humidity),
      ("battery_level", battery_level)
    ])
    
    // 更新设备状态
    LifecycleManager::update_device_heartbeat(lifecycle_manager, device_id)
  }
  
  // 设备更新阶段
  let firmware_update = FirmwareUpdate::new(
    version = "1.3.0",
    download_url = "https://updates.sensorcorp.com/firmware/ENV-2000-1.3.0.bin",
    checksum = "sha256:abcdef1234567890",
    size_bytes = 1048576,
    release_notes = "Bug fixes and performance improvements"
  )
  
  let update_result = LifecycleManager::schedule_firmware_update(lifecycle_manager, device_id, firmware_update)
  match update_result {
    Ok(update_id) => {
      assert_true(update_id.length() > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 模拟固件更新过程
  let update_progress = [
    (UpdateState::Downloading, 0.25),
    (UpdateState::Downloading, 0.50),
    (UpdateState::Downloading, 1.0),
    (UpdateState::Installing, 0.5),
    (UpdateState::Installing, 1.0),
    (UpdateState::Rebooting, 0.0),
    (UpdateState::Completed, 1.0)
  ]
  
  for (state, progress) in update_progress {
    LifecycleManager::update_firmware_progress(lifecycle_manager, device_id, state, progress)
  }
  
  // 验证更新结果
  let updated_device_info = LifecycleManager::get_device_info(lifecycle_manager, device_id)
  match updated_device_info {
    Some(info) => {
      assert_eq(info.firmware_version, "1.3.0")
    }
    None => assert_true(false)
  }
  
  // 设备停用阶段
  let deactivation_result = LifecycleManager::deactivate_device(lifecycle_manager, device_id)
  match deactivation_result {
    Ok(deactivation_time) => {
      assert_true(deactivation_time > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 验证停用状态
  let deactivated_status = LifecycleManager::get_device_status(lifecycle_manager, device_id)
  match deactivated_status {
    Some(status) => {
      assert_eq(status.state, DeviceState::Inactive)
    }
    None => assert_true(false)
  }
  
  // 设备注销阶段
  let unregistration_result = LifecycleManager::unregister_device(lifecycle_manager, device_id)
  match unregistration_result {
    Ok(_) => {
      assert_true(true)
    }
    Err(_) => assert_true(false)
  }
  
  // 验证设备已注销
  let final_status = LifecycleManager::get_device_status(lifecycle_manager, device_id)
  assert_eq(final_status, None)
}

// Test 5: 边缘计算数据分析
test "edge computing data analysis" {
  // 创建边缘数据分析器
  let edge_analyzer = EdgeDataAnalyzer::new()
  
  // 配置分析规则
  let analysis_rules = [
    AnalysisRule::new("temperature_anomaly", "Temperature Anomaly Detection")
      .with_metric("temperature")
      .with_condition(Condition::OutOfRange(15.0, 35.0))
      .with_severity(AlertSeverity::Warning),
    
    AnalysisRule::new("humidity_anomaly", "Humidity Anomaly Detection")
      .with_metric("humidity")
      .with_condition(Condition::OutOfRange(30.0, 80.0))
      .with_severity(AlertSeverity::Warning),
    
    AnalysisRule::new("battery_low", "Low Battery Detection")
      .with_metric("battery_level")
      .with_condition(Condition::LessThan(20.0))
      .with_severity(AlertSeverity::Critical),
    
    AnalysisRule::new("sensor_offline", "Sensor Offline Detection")
      .with_metric("heartbeat")
      .with_condition(Condition::OlderThan(300000)) // 5分钟无数据
      .with_severity(AlertSeverity::Critical)
  ]
  
  for rule in analysis_rules {
    Analyzer::add_rule(edge_analyzer, rule)
  }
  
  // 生成测试数据
  let base_time = Time::now()
  let test_data = []
  
  for i in 0..<100 {
    let timestamp = base_time + i * 30000 // 每30秒一个数据点
    
    // 正常数据
    let temperature = 22.0 + Math::sin(i.to_float() * 0.1) * 3.0
    let humidity = 55.0 + Math::cos(i.to_float() * 0.1) * 10.0
    let battery_level = 100.0 - i.to_float() * 0.5
    
    // 插入异常数据
    let final_temperature = if i == 25 { 40.0 } else if i == 50 { 10.0 } else { temperature }
    let final_humidity = if i == 30 { 90.0 } else if i == 60 { 25.0 } else { humidity }
    let final_battery = if i >= 80 { 15.0 } else { battery_level }
    
    let data_point = DataPoint::new(timestamp)
    DataPoint::add_metric(data_point, "temperature", final_temperature)
    DataPoint::add_metric(data_point, "humidity", final_humidity)
    DataPoint::add_metric(data_point, "battery_level", final_battery)
    DataPoint::add_metric(data_point, "heartbeat", timestamp)
    
    test_data = test_data.push(data_point)
  }
  
  // 执行数据分析
  let analysis_result = Analyzer::analyze_data(edge_analyzer, test_data)
  
  // 验证分析结果
  assert_true(analysis_result.total_data_points == 100)
  assert_true(analysis_result.anomalies_detected > 0)
  assert_true(analysis_result.alerts_generated > 0)
  
  // 验证温度异常检测
  let temperature_anomalies = AnalysisResult::get_anomalies_by_rule(analysis_result, "temperature_anomaly")
  assert_eq(temperature_anomalies.length(), 2)
  
  for anomaly in temperature_anomalies {
    assert_true(anomaly.value == 40.0 || anomaly.value == 10.0)
    assert_eq(anomaly.severity, AlertSeverity::Warning)
  }
  
  // 验证湿度异常检测
  let humidity_anomalies = AnalysisResult::get_anomalies_by_rule(analysis_result, "humidity_anomaly")
  assert_eq(humidity_anomalies.length(), 2)
  
  for anomaly in humidity_anomalies {
    assert_true(anomaly.value == 90.0 || anomaly.value == 25.0)
    assert_eq(anomaly.severity, AlertSeverity::Warning)
  }
  
  // 验证低电量检测
  let battery_alerts = AnalysisResult::get_alerts_by_rule(analysis_result, "battery_low")
  assert_eq(battery_alerts.length(), 20) // 从索引80到99
  
  for alert in battery_alerts {
    assert_true(alert.value <= 15.0)
    assert_eq(alert.severity, AlertSeverity::Critical)
  }
  
  // 测试趋势分析
  let trend_analyzer = TrendAnalyzer::new()
  let temperature_trend = TrendAnalyzer::analyze_trend(trend_analyzer, test_data, "temperature")
  assert_true(temperature_trend.direction == "stable") // 温度相对稳定
  
  let battery_trend = TrendAnalyzer::analyze_trend(trend_analyzer, test_data, "battery_level")
  assert_true(battery_trend.direction == "decreasing") // 电池电量持续下降
  assert_true(battery_trend.strength > 0.9) // 强趋势
  
  // 测试预测分析
  let predictive_analyzer = PredictiveAnalyzer::new()
  let battery_prediction = PredictiveAnalyzer::predict_depletion(predictive_analyzer, test_data, "battery_level")
  assert_true(battery_prediction.time_to_depletion > 0)
  assert_true(battery_prediction.confidence > 0.8)
  
  // 测试本地决策制定
  let decision_engine = LocalDecisionEngine::new()
  
  // 添加决策规则
  DecisionEngine::add_rule(decision_engine, DecisionRule::new("reduce_frequency_on_low_battery")
    .with_condition(Condition::MetricLessThan("battery_level", 30.0))
    .with_action(Action::AdjustTelemetryInterval(60)) // 增加到60秒间隔
    .with_priority(1))
  
  DecisionEngine::add_rule(decision_engine, DecisionRule::new("enable_compression_on_high_load")
    .with_condition(Condition::MetricGreaterThan("cpu_usage", 80.0))
    .with_action(Action::EnableCompression())
    .with_priority(2))
  
  // 模拟决策场景
  let current_state = DeviceState::new()
  DeviceState::set_metric(current_state, "battery_level", 25.0)
  DeviceState::set_metric(current_state, "cpu_usage", 75.0)
  
  let decisions = DecisionEngine::make_decisions(decision_engine, current_state)
  assert_eq(decisions.length(), 1) // 只有低电量规则触发
  
  let battery_decision = decisions[0]
  assert_eq(battery_decision.rule_name, "reduce_frequency_on_low_battery")
  assert_eq(battery_decision.action.type, ActionType::AdjustTelemetryInterval)
  assert_eq(battery_decision.action.new_interval, 60)
}