// Azimuth Edge Computing and IoT Telemetry Test Suite
// 边缘计算和物联网遥测测试套件

// Test 1: 资源受限环境下的遥测收集
test "telemetry collection in resource-constrained environments" {
  let edge_collector = @azimuth.EdgeCollector::new()
  
  // 配置资源限制
  let resource_limits = @azimuth.ResourceLimits::new(
    50 * 1024 * 1024,  // 50MB内存限制
    100 * 1024 * 1024, // 100MB存储限制
    2                  // 2个CPU核心限制
  )
  
  @azimuth.EdgeCollector::configure_limits(edge_collector, resource_limits)
  
  // 创建轻量级遥测配置
  let telemetry_config = @azimuth.TelemetryConfig::new()
  @azimuth.TelemetryConfig::set_sampling_rate(telemetry_config, 0.1) // 10%采样率
  @azimuth.TelemetryConfig::set_batch_size(telemetry_config, 10)     // 小批次大小
  @azimuth.TelemetryConfig::set_flush_interval(telemetry_config, 5000) // 5秒刷新间隔
  @azimuth.TelemetryConfig::enable_compression(telemetry_config, true) // 启用压缩
  @azimuth.TelemetryConfig::set_compression_level(telemetry_config, 6) // 中等压缩级别
  
  @azimuth.EdgeCollector::configure_telemetry(edge_collector, telemetry_config)
  
  // 模拟IoT设备遥测数据
  let mut device_metrics = []
  for device_id in 0..=20 {
    for reading in 0..=50 {
      let metric = @azimuth.IoTMetric::new(
        "device-#{device_id}",
        "temperature",
        @azimuth.FloatValue(20.0 + @azimuth.Random::next_float() * 15.0),
        @azimuth.Time::now_unix_nanos(),
        [
          ("device.type", @azimuth.StringValue("sensor")),
          ("location", @azimuth.StringValue("warehouse-#{device_id % 5}")),
          ("firmware.version", @azimuth.StringValue("1.2.3"))
        ]
      )
      device_metrics = device_metrics.push(metric)
    }
  }
  
  // 测试资源使用情况
  let initial_memory = @azimuth.EdgeCollector::get_memory_usage(edge_collector)
  let initial_storage = @azimuth.EdgeCollector::get_storage_usage(edge_collector)
  
  // 收集遥测数据
  for metric in device_metrics {
    @azimuth.EdgeCollector::collect_metric(edge_collector, metric)
  }
  
  let current_memory = @azimuth.EdgeCollector::get_memory_usage(edge_collector)
  let current_storage = @azimuth.EdgeCollector::get_storage_usage(edge_collector)
  
  // 验证资源使用在限制范围内
  assert_true(current_memory <= resource_limits.memory_limit)
  assert_true(current_storage <= resource_limits.storage_limit)
  
  // 测试自适应采样
  let adaptive_sampling = @azimuth.AdaptiveSampling::new()
  @azimuth.AdaptiveSampling::configure_memory_threshold(adaptive_sampling, 40 * 1024 * 1024) // 40MB阈值
  
  if current_memory > 40 * 1024 * 1024 {
    @azimuth.AdaptiveSampling::reduce_sampling_rate(adaptive_sampling, 0.05) // 降低到5%
  }
  
  // 验证自适应采样效果
  let adjusted_sampling_rate = @azimuth.AdaptiveSampling::get_current_rate(adaptive_sampling)
  assert_true(adjusted_sampling_rate <= 0.1)
}

// Test 2: 离线模式和数据缓存
test "offline mode and data caching" {
  let edge_processor = @azimuth.EdgeProcessor::new()
  
  // 配置离线模式
  let offline_config = @azimuth.OfflineConfig::new()
  @azimuth.OfflineConfig::set_cache_size(offline_config, 10 * 1024 * 1024) // 10MB缓存
  @azimuth.OfflineConfig::set_max_cache_duration(offline_config, 24 * 60 * 60 * 1000) // 24小时
  @azimuth.OfflineConfig::enable_persistence(offline_config, true) // 启用持久化
  @azimuth.OfflineConfig::set_persistence_path(offline_config, "/tmp/telemetry-cache")
  
  @azimuth.EdgeProcessor::configure_offline_mode(edge_processor, offline_config)
  
  // 模拟网络断开
  @azimuth.EdgeProcessor::simulate_network_disconnect(edge_processor)
  
  // 在离线模式下收集数据
  let offline_spans = []
  for i in 0..=100 {
    let span = @azimuth.Span::new("offline-operation-#{i}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
    @azimuth.Span::set_attribute(span, "mode", @azimuth.StringValue("offline"))
    @azimuth.Span::set_attribute(span, "device.id", @azimuth.StringValue("edge-device-001"))
    @azimuth.Span::add_event(span, "offline-event-#{i}", None)
    offline_spans = offline_spans.push(span)
  }
  
  // 处理离线数据
  for span in offline_spans {
    @azimuth.EdgeProcessor::process_span_offline(edge_processor, span)
  }
  
  // 验证数据缓存
  let cached_data = @azimuth.EdgeProcessor::get_cached_data(edge_processor)
  assert_true(cached_data.length() > 0)
  assert_eq(cached_data.length(), offline_spans.length())
  
  // 验证缓存大小
  let cache_size = @azimuth.EdgeProcessor::get_cache_size(edge_processor)
  assert_true(cache_size <= offline_config.cache_size)
  
  // 模拟网络恢复
  @azimuth.EdgeProcessor::simulate_network_reconnect(edge_processor)
  
  // 测试数据同步
  let sync_result = @azimuth.EdgeProcessor::sync_cached_data(edge_processor)
  
  // 验证同步结果
  assert_true(sync_result.success)
  assert_eq(sync_result.synced_count, cached_data.length())
  assert_eq(sync_result.failed_count, 0)
  
  // 验证缓存清理
  let remaining_cache = @azimuth.EdgeProcessor::get_cached_data(edge_processor)
  assert_eq(remaining_cache.length(), 0)
}

// Test 3: 边缘AI分析和异常检测
test "edge AI analysis and anomaly detection" {
  let edge_ai = @azimuth.EdgeAI::new()
  
  // 配置AI模型
  let ai_config = @azimuth.AIConfig::new()
  @azimuth.AIConfig::set_model_type(ai_config, @azimuth.ModelType::AnomalyDetection)
  @azimuth.AIConfig::set_model_path(ai_config, "/models/anomaly-detection.onnx")
  @azimuth.AIConfig::set_inference_threads(ai_config, 1) // 单线程推理以节省资源
  @azimuth.AIConfig::enable_quantization(ai_config, true) // 启用量化以减少内存使用
  
  @azimuth.EdgeAI::configure(edge_ai, ai_config)
  
  // 创建传感器数据流
  let sensor_data_stream = @azimuth.SensorDataStream::new()
  
  // 生成正常数据
  for i in 0..=200 {
    let sensor_reading = @azimuth.SensorReading::new(
      "temp-sensor-001",
      @azimuth.FloatValue(25.0 + @azimuth.Random::next_float() * 2.0), // 正常温度范围
      @azimuth.Time::now_unix_nanos(),
      @azimuth.SensorType::Temperature
    )
    @azimuth.SensorDataStream::add_reading(sensor_data_stream, sensor_reading)
  }
  
  // 生成异常数据
  for i in 0..=10 {
    let anomalous_reading = @azimuth.SensorReading::new(
      "temp-sensor-001",
      @azimuth.FloatValue(50.0 + @azimuth.Random::next_float() * 10.0), // 异常高温
      @azimuth.Time::now_unix_nanos(),
      @azimuth.SensorType::Temperature
    )
    @azimuth.SensorDataStream::add_reading(sensor_data_stream, anomalous_reading)
  }
  
  // 执行异常检测
  let anomaly_results = @azimuth.EdgeAI::detect_anomalies(edge_ai, sensor_data_stream)
  
  // 验证异常检测结果
  assert_true(anomaly_results.length() > 0)
  assert_true(anomaly_results.length() <= 10) // 最多检测到10个异常
  
  // 验证异常详情
  for anomaly in anomaly_results {
    assert_eq(@azimuth.AnomalyResult::sensor_id(anomaly), "temp-sensor-001")
    assert_true(@azimuth.AnomalyResult::anomaly_score(anomaly) > 0.8) // 高异常分数
    assert_eq(@azimuth.AnomalyResult::anomaly_type(anomaly), @azimuth.AnomalyType::TemperatureSpike)
  }
  
  // 创建异常警报
  for anomaly in anomaly_results {
    let alert = @azimuth.Alert::new(
      @azimuth.AlertSeverity::High,
      "Temperature anomaly detected",
      @azimuth.AnomalyResult::to_string(anomaly)
    )
    @azimuth.EdgeAI::send_alert(edge_ai, alert)
  }
  
  // 验证警报发送
  let alert_count = @azimuth.EdgeAI::get_alert_count(edge_ai)
  assert_eq(alert_count, anomaly_results.length())
}

// Test 4: 多协议支持和设备兼容性
test "multi-protocol support and device compatibility" {
  let protocol_manager = @azimuth.ProtocolManager::new()
  
  // 配置支持的协议
  let protocols = [
    @azimuth.Protocol::new("MQTT", 1883, @azimuth.ProtocolType::PublishSubscribe),
    @azimuth.Protocol::new("CoAP", 5683, @azimuth.ProtocolType::RequestResponse),
    @azimuth.Protocol::new("HTTP", 80, @azimuth.ProtocolType::RequestResponse),
    @azimuth.Protocol::new("LoRaWAN", 868, @azimuth.ProtocolType::WideAreaNetwork),
    @azimuth.Protocol::new("Zigbee", 2405, @azimuth.ProtocolType::PersonalAreaNetwork)
  ]
  
  for protocol in protocols {
    @azimuth.ProtocolManager::register_protocol(protocol_manager, protocol)
  }
  
  // 创建不同类型的设备
  let mqtt_device = @azimuth.IoTDevice::new("mqtt-sensor-001", "MQTT Temperature Sensor")
  @azimuth.IoTDevice::configure_protocol(mqtt_device, "MQTT", [
    ("broker.host", "mqtt.edge.local"),
    ("broker.port", "1883"),
    ("topic.prefix", "sensors/temperature")
  ])
  
  let coap_device = @azimuth.IoTDevice::new("coap-actuator-001", "CoAP Smart Actuator")
  @azimuth.IoTDevice::configure_protocol(coap_device, "CoAP", [
    ("server.host", "coap.edge.local"),
    ("server.port", "5683"),
    ("resource.path", "/actuator/control")
  ])
  
  let lorawan_device = @azimuth.IoTDevice::new("lorawan-gateway-001", "LoRaWAN Gateway")
  @azimuth.IoTDevice::configure_protocol(lorawan_device, "LoRaWAN", [
    ("network.server", "lorawan.edge.local"),
    ("app.key", "00112233445566778899AABBCCDDEEFF"),
    ("dev.eui", "1122334455667788")
  ])
  
  // 测试协议兼容性
  let mqtt_compatible = @azimuth.ProtocolManager::check_compatibility(protocol_manager, mqtt_device)
  let coap_compatible = @azimuth.ProtocolManager::check_compatibility(protocol_manager, coap_device)
  let lorawan_compatible = @azimuth.ProtocolManager::check_compatibility(protocol_manager, lorawan_device)
  
  assert_true(mqtt_compatible)
  assert_true(coap_compatible)
  assert_true(lorawan_compatible)
  
  // 模拟设备数据传输
  let mqtt_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("device.id", @azimuth.StringValue("mqtt-sensor-001")),
      ("protocol", @azimuth.StringValue("MQTT")),
      ("temperature", @azimuth.FloatValue(23.5)),
      ("humidity", @azimuth.FloatValue(45.2))
    ]
  )
  
  let coap_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("device.id", @azimuth.StringValue("coap-actuator-001")),
      ("protocol", @azimuth.StringValue("CoAP")),
      ("actuator.state", @azimuth.StringValue("active")),
      ("power.consumption", @azimuth.FloatValue(12.3))
    ]
  )
  
  let lorawan_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("device.id", @azimuth.StringValue("lorawan-gateway-001")),
      ("protocol", @azimuth.StringValue("LoRaWAN")),
      ("signal.strength", @azimuth.IntValue(-85)),
      ("packet.count", @azimuth.IntValue(150))
    ]
  )
  
  // 通过不同协议传输数据
  let mqtt_result = @azimuth.ProtocolManager::transmit_data(protocol_manager, mqtt_device, mqtt_data)
  let coap_result = @azimuth.ProtocolManager::transmit_data(protocol_manager, coap_device, coap_data)
  let lorawan_result = @azimuth.ProtocolManager::transmit_data(protocol_manager, lorawan_device, lorawan_data)
  
  // 验证传输结果
  assert_true(mqtt_result.success)
  assert_true(coap_result.success)
  assert_true(lorawan_result.success)
  
  // 验证协议统计
  let mqtt_stats = @azimuth.ProtocolManager::get_protocol_stats(protocol_manager, "MQTT")
  let coap_stats = @azimuth.ProtocolManager::get_protocol_stats(protocol_manager, "CoAP")
  let lorawan_stats = @azimuth.ProtocolManager::get_protocol_stats(protocol_manager, "LoRaWAN")
  
  assert_eq(mqtt_stats.messages_sent, 1)
  assert_eq(coap_stats.messages_sent, 1)
  assert_eq(lorawan_stats.messages_sent, 1)
  
  assert_eq(mqtt_stats.bytes_transferred, @azimuth.TelemetryData::calculate_size(mqtt_data))
  assert_eq(coap_stats.bytes_transferred, @azimuth.TelemetryData::calculate_size(coap_data))
  assert_eq(lorawan_stats.bytes_transferred, @azimuth.TelemetryData::calculate_size(lorawan_data))
}

// Test 5: 边缘计算集群和负载均衡
test "edge computing cluster and load balancing" {
  let edge_cluster = @azimuth.EdgeCluster::new()
  
  // 创建边缘节点
  let nodes = [
    @azimuth.EdgeNode::new("edge-node-001", "192.168.1.10", 8080, @azimuth.NodeCapabilities::HighMemory),
    @azimuth.EdgeNode::new("edge-node-002", "192.168.1.11", 8080, @azimuth.NodeCapabilities::HighCPU),
    @azimuth.EdgeNode::new("edge-node-003", "192.168.1.12", 8080, @azimuth.NodeCapabilities::Balanced),
    @azimuth.EdgeNode::new("edge-node-004", "192.168.1.13", 8080, @azimuth.NodeCapabilities::LowPower)
  ]
  
  for node in nodes {
    @azimuth.EdgeCluster::add_node(edge_cluster, node)
  }
  
  // 配置负载均衡策略
  let load_balancer = @azimuth.LoadBalancer::new()
  @azimuth.LoadBalancer::set_strategy(load_balancer, @azimuth.LoadBalancingStrategy::WeightedRoundRobin)
  
  // 设置节点权重
  @azimuth.LoadBalancer::set_node_weight(load_balancer, "edge-node-001", 4) // 高内存节点权重高
  @azimuth.LoadBalancer::set_node_weight(load_balancer, "edge-node-002", 3) // 高CPU节点权重中等
  @azimuth.LoadBalancer::set_node_weight(load_balancer, "edge-node-003", 2) // 平衡节点权重较低
  @azimuth.LoadBalancer::set_node_weight(load_balancer, "edge-node-004", 1) // 低功耗节点权重最低
  
  @azimuth.EdgeCluster::set_load_balancer(edge_cluster, load_balancer)
  
  // 模拟工作负载
  let workloads = []
  for i in 0..=100 {
    let workload = @azimuth.Workload::new(
      "workload-#{i}",
      @azimuth.WorkloadType::TelemetryProcessing,
      @azimuth.Intensity::Medium,
      [
        ("data.size", @azimuth.IntValue(1024 * (i % 10 + 1))),
        ("processing.time", @azimuth.IntValue(100 * (i % 5 + 1))),
        ("memory.required", @azimuth.IntValue(50 * (i % 8 + 1)))
      ]
    )
    workloads = workloads.push(workload)
  }
  
  // 分配工作负载
  let mut node_assignments = @azimuth.StringMap::new()
  for workload in workloads {
    let assigned_node = @azimuth.EdgeCluster::assign_workload(edge_cluster, workload)
    let current_count = @azimuth.StringMap::get(node_assignments, assigned_node)
    let new_count = match current_count {
      Some(count) => count + 1
      None => 1
    }
    @azimuth.StringMap::set(node_assignments, assigned_node, new_count)
  }
  
  // 验证负载分布
  let node001_count = @azimuth.StringMap::get(node_assignments, "edge-node-001")
  let node002_count = @azimuth.StringMap::get(node_assignments, "edge-node-002")
  let node003_count = @azimuth.StringMap::get(node_assignments, "edge-node-003")
  let node004_count = @azimuth.StringMap::get(node_assignments, "edge-node-004")
  
  // 高权重节点应该分配更多工作负载
  assert_true(node001_count > node002_count)
  assert_true(node002_count > node003_count)
  assert_true(node003_count > node004_count)
  
  // 验证总工作负载分配
  let total_assigned = node001_count + node002_count + node003_count + node004_count
  assert_eq(total_assigned, workloads.length())
  
  // 测试节点故障转移
  @azimuth.EdgeCluster::simulate_node_failure(edge_cluster, "edge-node-002")
  
  // 重新分配故障节点的工作负载
  let failover_result = @azimuth.EdgeCluster::redistribute_workload(edge_cluster, "edge-node-002")
  
  // 验证故障转移结果
  assert_true(failover_result.success)
  assert_eq(failover_result.redistributed_count, node002_count)
  
  // 验证集群健康状态
  let cluster_health = @azimuth.EdgeCluster::get_health_status(edge_cluster)
  assert_eq(cluster_health.total_nodes, 4)
  assert_eq(cluster_health.healthy_nodes, 3)
  assert_eq(cluster_health.failed_nodes, 1)
  assert_false(cluster_health.all_healthy)
}

// Test 6: 边缘云协同和数据同步
test "edge-cloud collaboration and data synchronization" {
  let edge_cloud_sync = @azimuth.EdgeCloudSync::new()
  
  // 配置边缘节点
  let edge_node = @azimuth.EdgeNode::new("edge-primary", "10.0.1.100", 8080, @azimuth.NodeCapabilities::Balanced)
  
  // 配置云连接
  let cloud_config = @azimuth.CloudConfig::new(
    @azimuth.CloudProvider::AWS,
    "us-west-2",
    ["otel-collector.cloud.example.com:4317"],
    @azimuth.AuthConfig::new(@azimuth.AuthType::API_Key, "cloud-api-key-12345")
  )
  
  @azimuth.EdgeCloudSync::configure_cloud_connection(edge_cloud_sync, cloud_config)
  
  // 配置同步策略
  let sync_strategy = @azimuth.SyncStrategy::new()
  @azimuth.SyncStrategy::set_sync_mode(sync_strategy, @azimuth.SyncMode::Batch)
  @azimuth.SyncStrategy::set_batch_size(sync_strategy, 50)
  @azimuth.SyncStrategy::set_sync_interval(sync_strategy, 30000) // 30秒
  @azimuth.SyncStrategy::enable_compression(sync_strategy, true)
  @azimuth.SyncStrategy::enable_encryption(sync_strategy, true)
  @azimuth.SyncStrategy::set_priority_attributes(sync_strategy, ["error", "critical", "high"])
  
  @azimuth.EdgeCloudSync::configure_sync_strategy(edge_cloud_sync, sync_strategy)
  
  // 创建边缘遥测数据
  let edge_data = []
  for i in 0..=200 {
    let data_point = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("edge.node", @azimuth.StringValue("edge-primary")),
        ("source", @azimuth.StringValue("iot-sensor")),
        ("sensor.id", @azimuth.StringValue("sensor-#{i % 20}")),
        ("metric.type", @azimuth.StringValue(if i % 3 == 0 { "temperature" } else if i % 3 == 1 { "humidity" } else { "pressure" })),
        ("metric.value", @azimuth.FloatValue(20.0 + @azimuth.Random::next_float() * 30.0)),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
        ("priority", @azimuth.StringValue(if i % 10 == 0 { "high" } else if i % 10 == 1 { "critical" } else { "normal" })),
        ("location", @azimuth.StringValue("warehouse-#{i % 5}"))
      ]
    )
    edge_data = edge_data.push(data_point)
  }
  
  // 处理边缘数据
  for data in edge_data {
    @azimuth.EdgeCloudSync::process_edge_data(edge_cloud_sync, data)
  }
  
  // 验证数据预处理
  let processed_data = @azimuth.EdgeCloudSync::get_processed_data(edge_cloud_sync)
  assert_eq(processed_data.length(), edge_data.length())
  
  // 执行数据同步
  let sync_result = @azimuth.EdgeCloudSync::sync_to_cloud(edge_cloud_sync)
  
  // 验证同步结果
  assert_true(sync_result.success)
  assert_eq(sync_result.total_records, edge_data.length())
  assert_eq(sync_result.synced_records, edge_data.length())
  assert_eq(sync_result.failed_records, 0)
  
  // 验证数据优先级处理
  let high_priority_count = edge_data.filter(fn(data) {
    match @azimuth.TelemetryData::get_attribute(data, "priority") {
      Some(@azimuth.StringValue(p)) => p == "high" || p == "critical"
      _ => false
    }
  }).length()
  
  assert_true(sync_result.priority_records >= high_priority_count)
  
  // 测试增量同步
  let new_edge_data = []
  for i in 201..=250 {
    let data_point = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("edge.node", @azimuth.StringValue("edge-primary")),
        ("source", @azimuth.StringValue("iot-sensor")),
        ("sensor.id", @azimuth.StringValue("sensor-#{i % 20}")),
        ("metric.type", @azimuth.StringValue("temperature")),
        ("metric.value", @azimuth.FloatValue(20.0 + @azimuth.Random::next_float() * 30.0)),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos()))
      ]
    )
    new_edge_data = new_edge_data.push(data_point)
  }
  
  // 处理新数据
  for data in new_edge_data {
    @azimuth.EdgeCloudSync::process_edge_data(edge_cloud_sync, data)
  }
  
  // 执行增量同步
  let incremental_sync_result = @azimuth.EdgeCloudSync::incremental_sync(edge_cloud_sync)
  
  // 验证增量同步结果
  assert_true(incremental_sync_result.success)
  assert_eq(incremental_sync_result.total_records, new_edge_data.length())
  assert_eq(incremental_sync_result.synced_records, new_edge_data.length())
  assert_true(incremental_sync_result.is_incremental)
  
  // 验证同步统计
  let sync_stats = @azimuth.EdgeCloudSync::get_sync_statistics(edge_cloud_sync)
  assert_true(sync_stats.total_syncs >= 2)
  assert_true(sync_stats.total_records_synced >= edge_data.length() + new_edge_data.length())
  assert_true(sync_stats.compression_ratio > 1.0)
  assert_true(sync_stats.avg_sync_time > 0)
}

// Test 7: 低功耗模式和能源管理
test "low-power mode and energy management" {
  let energy_manager = @azimuth.EnergyManager::new()
  
  // 配置能源管理
  let energy_config = @azimuth.EnergyConfig::new()
  @azimuth.EnergyConfig::set_power_source(energy_config, @azimuth.PowerSource::Battery)
  @azimuth.EnergyConfig::set_battery_capacity(energy_config, 5000) // 5000mAh
  @azimuth.EnergyConfig::set_low_power_threshold(energy_config, 20) // 20%电量阈值
  @azimuth.EnergyConfig::enable_adaptive_scaling(energy_config, true)
  
  @azimuth.EnergyManager::configure(energy_manager, energy_config)
  
  // 模拟电池状态
  @azimuth.EnergyManager::set_battery_level(energy_manager, 85) // 85%电量
  
  // 正常模式下的遥测收集
  let normal_mode_config = @azimuth.TelemetryConfig::new()
  @azimuth.TelemetryConfig::set_sampling_rate(normal_mode_config, 1.0) // 100%采样
  @azimuth.TelemetryConfig::set_batch_size(normal_mode_config, 100)
  @azimuth.TelemetryConfig::set_flush_interval(normal_mode_config, 5000) // 5秒
  @azimuth.TelemetryConfig::enable_compression(normal_mode_config, false)
  
  // 低功耗模式下的遥测收集
  let low_power_mode_config = @azimuth.TelemetryConfig::new()
  @azimuth.TelemetryConfig::set_sampling_rate(low_power_mode_config, 0.1) // 10%采样
  @azimuth.TelemetryConfig::set_batch_size(low_power_mode_config, 20)
  @azimuth.TelemetryConfig::set_flush_interval(low_power_mode_config, 30000) // 30秒
  @azimuth.TelemetryConfig::enable_compression(low_power_mode_config, true)
  @azimuth.TelemetryConfig::set_compression_level(low_power_mode_config, 9) // 最高压缩
  
  // 测试正常模式
  let normal_power_consumption = @azimuth.EnergyManager::measure_consumption(energy_manager, normal_mode_config)
  
  // 模拟低电量情况
  @azimuth.EnergyManager::set_battery_level(energy_manager, 15) // 15%电量
  
  // 自动切换到低功耗模式
  let adaptive_config = @azimuth.EnergyManager::get_adaptive_config(energy_manager)
  
  // 验证自适应配置
  let adaptive_sampling_rate = @azimuth.TelemetryConfig::get_sampling_rate(adaptive_config)
  let adaptive_batch_size = @azimuth.TelemetryConfig::get_batch_size(adaptive_config)
  let adaptive_flush_interval = @azimuth.TelemetryConfig::get_flush_interval(adaptive_config)
  
  assert_true(adaptive_sampling_rate < 1.0)
  assert_true(adaptive_batch_size < 100)
  assert_true(adaptive_flush_interval > 5000)
  assert_true(@azimuth.TelemetryConfig::is_compression_enabled(adaptive_config))
  
  // 测试低功耗模式
  let low_power_consumption = @azimuth.EnergyManager::measure_consumption(energy_manager, adaptive_config)
  
  // 验证功耗降低
  assert_true(low_power_consumption < normal_power_consumption)
  let power_reduction = (normal_power_consumption - low_power_consumption) / normal_power_consumption
  assert_true(power_reduction > 0.3) // 至少30%的功耗降低
  
  // 测试能源状态监控
  let energy_status = @azimuth.EnergyManager::get_energy_status(energy_manager)
  assert_eq(energy_status.battery_level, 15)
  assert_eq(energy_status.power_source, @azimuth.PowerSource::Battery)
  assert_eq(energy_status.mode, @azimuth.PowerMode::LowPower)
  assert_true(energy_status.estimated_runtime_hours > 0)
  
  // 测试节能策略
  let power_saving_strategies = @azimuth.EnergyManager::get_power_saving_strategies(energy_manager)
  assert_true(power_saving_strategies.length() > 0)
  
  // 验证节能策略包含以下措施
  let has_reduced_sampling = power_saving_strategies.some(fn(strategy) {
    @azimuth.PowerSavingStrategy::type(strategy) == @azimuth.StrategyType::ReducedSampling
  })
  let has_increased_batching = power_saving_strategies.some(fn(strategy) {
    @azimuth.PowerSavingStrategy::type(strategy) == @azimuth.StrategyType::IncreasedBatching
  })
  let has_compression = power_saving_strategies.some(fn(strategy) {
    @azimuth.PowerSavingStrategy::type(strategy) == @azimuth.StrategyType::DataCompression
  })
  
  assert_true(has_reduced_sampling)
  assert_true(has_increased_batching)
  assert_true(has_compression)
}