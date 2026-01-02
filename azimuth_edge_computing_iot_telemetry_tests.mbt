// Azimuth边缘计算场景测试用例
// 测试Azimuth遥测系统在边缘计算环境中的特性和优化

test "边缘设备资源受限环境下的遥测收集" {
  // 模拟边缘设备资源受限环境
  let edge_device = EdgeDevice::new()
  EdgeDevice::set_memory_limit(edge_device, 64 * 1024 * 1024)  // 64MB内存限制
  EdgeDevice::set_storage_limit(edge_device, 512 * 1024 * 1024)  // 512MB存储限制
  EdgeDevice::set_cpu_limit(edge_device, 2)  // 2个CPU核心
  EdgeDevice::set_network_type(edge_device, "cellular")  // 蜂窝网络
  EdgeDevice::set_bandwidth_limit(edge_device, 1048576)  // 1MB/s带宽限制
  
  // 创建边缘优化遥测配置
  let edge_config = EdgeTelemetryConfig::new(edge_device)
  EdgeTelemetryConfig::enable_memory_optimization(edge_config, true)
  EdgeTelemetryConfig::enable_storage_optimization(edge_config, true)
  EdgeTelemetryConfig::enable_network_optimization(edge_config, true)
  EdgeTelemetryConfig::set_batch_size(edge_config, 50)  // 小批量处理
  EdgeTelemetryConfig::set_compression_level(edge_config, "high")  // 高压缩级别
  EdgeTelemetryConfig::set_sampling_rate(edge_config, 0.1)  // 10%采样率
  
  // 创建边缘遥测收集器
  let edge_collector = EdgeTelemetryCollector::new(edge_config)
  
  // 测试内存使用监控
  let initial_memory = EdgeTelemetryCollector::get_memory_usage(edge_collector)
  
  // 生成大量遥测数据
  for i in 0..=999 {
    let tracer = EdgeTelemetryCollector::get_tracer(edge_collector, "edge.test")
    let span = EdgeTracer::start_span(tracer, "edge.operation." + i.to_string())
    
    // 设置少量属性以节省内存
    Span::set_attribute(span, "op.id", i.to_string())
    Span::set_attribute(span, "device.id", "edge-device-001")
    
    // 记录度量
    let meter = EdgeTelemetryCollector::get_meter(edge_collector, "edge.test")
    let counter = EdgeMeter::create_counter(meter, "edge.operations", Some("Edge operations"), Some("count"))
    Counter::add(counter, 1.0)
    
    // 记录日志（仅在错误情况下）
    if i % 100 == 0 {
      let logger = EdgeTelemetryCollector::get_logger(edge_collector, "edge.test")
      let log = LogRecord::new(Info, "Periodic status update")
      LogRecord::add_attribute(log, "cycle", (i / 100).to_string())
      EdgeLogger::emit(logger, log)
    }
    
    Span::end(span)
    
    // 每100个操作检查一次内存使用
    if i % 100 == 0 {
      let current_memory = EdgeTelemetryCollector::get_memory_usage(edge_collector)
      let memory_growth = current_memory - initial_memory
      
      // 验证内存增长在可控范围内
      assert_true(memory_growth < 10 * 1024 * 1024)  // 内存增长应小于10MB
      
      // 如果内存使用接近限制，触发清理
      if current_memory > EdgeDevice::get_memory_limit(edge_device) * 0.8 {
        EdgeTelemetryCollector::trigger_cleanup(edge_collector)
      }
    }
  }
  
  // 验证最终内存使用
  let final_memory = EdgeTelemetryCollector::get_memory_usage(edge_collector)
  assert_true(final_memory < EdgeDevice::get_memory_limit(edge_device) * 0.9)  // 内存使用应小于限制的90%
  
  // 测试存储使用优化
  let storage_usage = EdgeTelemetryCollector::get_storage_usage(edge_collector)
  assert_true(storage_usage < EdgeDevice::get_storage_limit(edge_device) * 0.5)  // 存储使用应小于限制的50%
  
  // 验证数据压缩效果
  let raw_data_size = EdgeTelemetryCollector::get_raw_data_size(edge_collector)
  let compressed_data_size = EdgeTelemetryCollector::get_compressed_data_size(edge_collector)
  let compression_ratio = compressed_data_size.to_float() / raw_data_size.to_float()
  
  assert_true(compression_ratio < 0.3)  // 压缩率应小于30%
}

test "网络不稳定环境下的遥测传输" {
  // 模拟网络不稳定的边缘环境
  let unstable_network = UnstableNetwork::new()
  UnstableNetwork::set_latency(unstable_network, 500)  // 500ms延迟
  UnstableNetwork::set_packet_loss(unstable_network, 0.05)  // 5%丢包率
  UnstableNetwork::set_bandwidth(unstable_network, 512000)  // 512KB/s带宽
  UnstableNetwork::set_connectivity_pattern(unstable_network, "intermittent")  // 间歇性连接
  
  // 创建网络感知遥测配置
  let network_config = NetworkAwareTelemetryConfig::new()
  NetworkAwareTelemetryConfig::set_retry_policy(network_config, "exponential_backoff")
  NetworkAwareTelemetryConfig::set_max_retry_attempts(network_config, 5)
  NetworkAwareTelemetryConfig::set_initial_retry_delay(network_config, 1000)  // 1秒
  NetworkAwareTelemetryConfig::set_max_retry_delay(network_config, 30000)  // 30秒
  NetworkAwareTelemetryConfig::enable_offline_mode(network_config, true)
  NetworkAwareTelemetryConfig::set_offline_storage_limit(network_config, 100 * 1024 * 1024)  // 100MB离线存储
  
  // 创建网络感知遥测传输器
  let network_transporter = NetworkAwareTelemetryTransporter::new(network_config, unstable_network)
  
  // 生成遥测数据
  let telemetry_batches = []
  for batch_id in 0..=19 {  // 20个批次
    let batch_data = []
    for i in 0..=49 {  // 每批次50个数据点
      let data_point = TelemetryDataPoint::new(
        "edge.metric." + i.to_string(),
        @random() * 100.0,
        [("batch.id", batch_id.to_string()), ("point.id", i.to_string())]
      )
      batch_data = batch_data.push(data_point)
    }
    telemetry_batches = telemetry_batches.push(batch_data)
  }
  
  // 测试正常网络条件下的传输
  UnstableNetwork::simulate_normal_conditions(unstable_network)
  
  let normal_transmission_results = []
  for batch_id in 0..=4 {  // 前5个批次
    let batch = telemetry_batches[batch_id]
    let transmission_result = NetworkAwareTelemetryTransporter::transmit_batch(network_transporter, batch)
    normal_transmission_results = normal_transmission_results.push(transmission_result)
    
    // 验证正常条件下的传输成功率
    assert_true(transmission_result.success)
    assert_true(transmission_result.attempts <= 2)  // 正常条件下应很少重试
  }
  
  // 测试网络不稳定条件下的传输
  UnstableNetwork::simulate_unstable_conditions(unstable_network)
  
  let unstable_transmission_results = []
  for batch_id in 5..=14 {  // 中间10个批次
    let batch = telemetry_batches[batch_id]
    let transmission_result = NetworkAwareTelemetryTransporter::transmit_batch(network_transporter, batch)
    unstable_transmission_results = unstable_transmission_results.push(transmission_result)
    
    // 记录传输结果（可能失败）
    if transmission_result.success {
      assert_true(transmission_result.attempts >= 1)  // 不稳定条件下可能需要重试
    }
  }
  
  // 测试完全离线条件下的传输
  UnstableNetwork::simulate_offline_conditions(unstable_network)
  
  let offline_transmission_results = []
  for batch_id in 15..=19 {  // 后5个批次
    let batch = telemetry_batches[batch_id]
    let transmission_result = NetworkAwareTelemetryTransporter::transmit_batch(network_transporter, batch)
    offline_transmission_results = offline_transmission_results.push(transmission_result)
    
    // 验证离线条件下的行为
    assert_false(transmission_result.success)  // 离线条件下应失败
    assert_true(transmission_result.stored_for_retry)  // 数据应存储以便重试
  }
  
  // 测试网络恢复后的重传
  UnstableNetwork::simulate_normal_conditions(unstable_network)
  let retry_results = NetworkAwareTelemetryTransporter::retry_stored_data(network_transporter)
  
  // 验证重传结果
  let successful_retries = retry_results.filter(fn(r) { r.success })
  let retry_success_rate = successful_retries.length().to_float() / retry_results.length().to_float()
  
  assert_true(retry_success_rate > 0.8)  // 重传成功率应大于80%
  
  // 验证离线存储使用情况
  let offline_storage_usage = NetworkAwareTelemetryTransporter::get_offline_storage_usage(network_transporter)
  assert_true(offline_storage_usage < NetworkAwareTelemetryConfig::get_offline_storage_limit(network_config))
}

test "边缘计算中的分布式追踪" {
  // 创建边缘计算环境
  let edge_environment = EdgeComputingEnvironment::new()
  EdgeComputingEnvironment::add_edge_node(edge_environment, "edge-node-001", "factory-floor-1")
  EdgeComputingEnvironment::add_edge_node(edge_environment, "edge-node-002", "factory-floor-2")
  EdgeComputingEnvironment::add_edge_node(edge_environment, "edge-node-003", "factory-floor-3")
  EdgeComputingEnvironment::add_cloud_gateway(edge_environment, "cloud-gateway-001")
  
  // 创建边缘分布式追踪配置
  let edge_trace_config = EdgeDistributedTraceConfig::new()
  EdgeDistributedTraceConfig::enable_edge_to_cloud_propagation(edge_trace_config, true)
  EdgeDistributedTraceConfig::enable_edge_to_edge_propagation(edge_trace_config, true)
  EdgeDistributedTraceConfig::set_trace_sampling_rate(edge_trace_config, 0.5)  // 50%采样率
  EdgeDistributedTraceConfig::enable_compression(edge_trace_config, true)
  EdgeDistributedTraceConfig::set_batch_size(edge_trace_config, 25)
  
  // 创建边缘分布式追踪器
  let edge_tracer = EdgeDistributedTracer::new(edge_trace_config, edge_environment)
  
  // 模拟边缘到边缘的分布式操作
  let root_span = EdgeDistributedTracer::start_span(edge_tracer, "edge-node-001", "manufacturing.process")
  
  // 设置根span属性
  Span::set_attribute(root_span, "process.type", "assembly")
  Span::set_attribute(root_span, "product.id", "prod-12345")
  Span::set_attribute(root_span, "workstation.id", "ws-001")
  
  // 模拟边缘节点间的操作链
  let edge_to_edge_span = EdgeDistributedTracer::start_span_with_parent(
    edge_tracer, 
    "edge-node-002", 
    "quality.check",
    Span::span_context(root_span)
  )
  
  Span::set_attribute(edge_to_edge_span, "check.type", "dimensional")
  Span::set_attribute(edge_to_edge_span, "check.result", "pass")
  
  // 继续到下一个边缘节点
  let edge_to_edge_span2 = EdgeDistributedTracer::start_span_with_parent(
    edge_tracer, 
    "edge-node-003", 
    "packaging.process",
    Span::span_context(edge_to_edge_span)
  )
  
  Span::set_attribute(edge_to_edge_span2, "package.type", "standard")
  Span::set_attribute(edge_to_edge_span2, "package.weight", "2.5kg")
  
  // 模拟边缘到云的操作
  let edge_to_cloud_span = EdgeDistributedTracer::start_span_with_parent(
    edge_tracer, 
    "cloud-gateway-001", 
    "data.upload",
    Span::span_context(edge_to_edge_span2)
  )
  
  Span::set_attribute(edge_to_cloud_span, "data.type", "telemetry")
  Span::set_attribute(edge_to_cloud_span, "data.size", "1.2MB")
  
  // 结束所有span
  Span::end(edge_to_cloud_span)
  Span::end(edge_to_edge_span2)
  Span::end(edge_to_edge_span)
  Span::end(root_span)
  
  // 验证分布式追踪的完整性
  let trace_id = SpanContext::trace_id(Span::span_context(root_span))
  let all_spans = EdgeDistributedTracer::get_trace_spans(edge_tracer, trace_id)
  
  assert_eq(all_spans.length(), 4)  // 应该有4个span
  
  // 验证span的父子关系
  let root_context = Span::span_context(root_span)
  let edge_to_edge_context = Span::span_context(edge_to_edge_span)
  let edge_to_edge_context2 = Span::span_context(edge_to_edge_span2)
  let edge_to_cloud_context = Span::span_context(edge_to_cloud_span)
  
  assert_eq(SpanContext::trace_id(root_context), trace_id)
  assert_eq(SpanContext::trace_id(edge_to_edge_context), trace_id)
  assert_eq(SpanContext::trace_id(edge_to_edge_context2), trace_id)
  assert_eq(SpanContext::trace_id(edge_to_cloud_context), trace_id)
  
  assert_eq(SpanContext::parent_span_id(edge_to_edge_context), SpanContext::span_id(root_context))
  assert_eq(SpanContext::parent_span_id(edge_to_edge_context2), SpanContext::span_id(edge_to_edge_span))
  assert_eq(SpanContext::parent_span_id(edge_to_cloud_context), SpanContext::span_id(edge_to_edge_span2))
  
  // 验证跨节点的上下文传播
  let node001_spans = all_spans.filter(fn(s) { s.node_id == "edge-node-001" })
  let node002_spans = all_spans.filter(fn(s) { s.node_id == "edge-node-002" })
  let node003_spans = all_spans.filter(fn(s) { s.node_id == "edge-node-003" })
  let cloud_spans = all_spans.filter(fn(s) { s.node_id == "cloud-gateway-001" })
  
  assert_eq(node001_spans.length(), 1)
  assert_eq(node002_spans.length(), 1)
  assert_eq(node003_spans.length(), 1)
  assert_eq(cloud_spans.length(), 1)
  
  // 测试边缘追踪数据的压缩和传输
  let compressed_trace_data = EdgeDistributedTracer::compress_trace_data(edge_tracer, trace_id)
  let compression_ratio = compressed_trace_data.length().to_float() / 
                          (all_spans.length() * 1000).to_float()  // 假设每个span约1KB
  
  assert_true(compression_ratio < 0.3)  // 压缩率应小于30%
}

test "边缘设备上的本地数据处理和分析" {
  // 创建边缘数据处理配置
  let edge_processing_config = EdgeDataProcessingConfig::new()
  EdgeDataProcessingConfig::enable_local_aggregation(edge_processing_config, true)
  EdgeDataProcessingConfig::enable_anomaly_detection(edge_processing_config, true)
  EdgeDataProcessingConfig::enable_trend_analysis(edge_processing_config, true)
  EdgeDataProcessingConfig::set_aggregation_window(edge_processing_config, 300)  // 5分钟窗口
  EdgeDataProcessingConfig::set_anomaly_threshold(edge_processing_config, 2.0)  // 2倍标准差
  
  // 创建边缘数据处理器
  let edge_processor = EdgeDataProcessor::new(edge_processing_config)
  
  // 生成传感器数据流
  let sensor_data_stream = []
  let base_timestamp = 1735689600000000000L  // 基准时间戳
  
  for i in 0..=599 {  // 10分钟的数据，每秒一个数据点
    let timestamp = base_timestamp + (i * 1000000000L)
    
    // 模拟温度传感器数据（正常范围20-30度）
    let normal_temp = 25.0 + (5.0 * @sin(i.to_float() * 0.1)) + ((@random() * 2.0) - 1.0)
    
    // 模拟压力传感器数据（正常范围100-150 PSI）
    let normal_pressure = 125.0 + (25.0 * @cos(i.to_float() * 0.05)) + ((@random() * 10.0) - 5.0)
    
    // 模拟振动传感器数据（正常范围0-5 mm/s）
    let normal_vibration = 2.5 + (2.5 * @sin(i.to_float() * 0.2)) + ((@random() * 1.0) - 0.5)
    
    // 插入一些异常值
    let temp = if i == 150 || i == 450 { 45.0 } else { normal_temp }  // 高温异常
    let pressure = if i == 300 { 200.0 } else { normal_pressure }  // 高压异常
    let vibration = if i == 200 || i == 400 { 8.0 } else { normal_vibration }  // 高振动异常
    
    let sensor_reading = SensorReading::new(timestamp, [
      ("temperature", temp),
      ("pressure", pressure),
      ("vibration", vibration)
    ])
    
    sensor_data_stream = sensor_data_stream.push(sensor_reading)
  }
  
  // 处理传感器数据流
  let processing_results = []
  for reading in sensor_data_stream {
    let result = EdgeDataProcessor::process_sensor_reading(edge_processor, reading)
    processing_results = processing_results.push(result)
    
    // 每60个数据点（1分钟）检查一次聚合结果
    if processing_results.length() % 60 == 0 {
      let aggregation_result = EdgeDataProcessor::get_aggregation_result(edge_processor, "temperature")
      
      // 验证聚合结果
      assert_true(aggregation_result.count > 0)
      assert_true(aggregation_result.average >= 20.0 && aggregation_result.average <= 30.0 || 
                 aggregation_result.count <= 5)  // 允许少量异常值影响平均值
      
      let pressure_aggregation = EdgeDataProcessor::get_aggregation_result(edge_processor, "pressure")
      assert_true(pressure_aggregation.count > 0)
      assert_true(pressure_aggregation.average >= 100.0 && pressure_aggregation.average <= 150.0 ||
                 pressure_aggregation.count <= 5)
      
      let vibration_aggregation = EdgeDataProcessor::get_aggregation_result(edge_processor, "vibration")
      assert_true(vibration_aggregation.count > 0)
      assert_true(vibration_aggregation.average >= 0.0 && vibration_aggregation.average <= 5.0 ||
                 vibration_aggregation.count <= 5)
    }
  }
  
  // 检测异常
  let detected_anomalies = EdgeDataProcessor::get_detected_anomalies(edge_processor)
  
  // 验证异常检测结果
  assert_true(detected_anomalies.length() >= 3)  // 至少应检测到3个异常（温度、压力、振动）
  
  // 验证异常类型
  let temp_anomalies = detected_anomalies.filter(fn(a) { a.sensor_type == "temperature" })
  let pressure_anomalies = detected_anomalies.filter(fn(a) { a.sensor_type == "pressure" })
  let vibration_anomalies = detected_anomalies.filter(fn(a) { a.sensor_type == "vibration" })
  
  assert_true(temp_anomalies.length() >= 1)
  assert_true(pressure_anomalies.length() >= 1)
  assert_true(vibration_anomalies.length() >= 1)
  
  // 验证异常时间点
  let temp_anomaly_timestamps = temp_anomalies.map(fn(a) { a.timestamp })
  let pressure_anomaly_timestamps = pressure_anomalies.map(fn(a) { a.timestamp })
  let vibration_anomaly_timestamps = vibration_anomalies.map(fn(a) { a.timestamp })
  
  assert_true(temp_anomaly_timestamps.contains(base_timestamp + (150 * 1000000000L)))
  assert_true(temp_anomaly_timestamps.contains(base_timestamp + (450 * 1000000000L)))
  assert_true(pressure_anomaly_timestamps.contains(base_timestamp + (300 * 1000000000L)))
  assert_true(vibration_anomaly_timestamps.contains(base_timestamp + (200 * 1000000000L)))
  assert_true(vibration_anomaly_timestamps.contains(base_timestamp + (400 * 1000000000L)))
  
  // 测试趋势分析
  let trend_analysis = EdgeDataProcessor::get_trend_analysis(edge_processor, "temperature")
  
  // 验证趋势分析结果
  assert_true(trend_analysis.contains_key("trend_direction"))
  assert_true(trend_analysis.contains_key("trend_strength"))
  assert_true(trend_analysis.contains_key("seasonality_detected"))
  
  // 测试本地预测
  let prediction_result = EdgeDataProcessor::predict_next_values(edge_processor, "temperature", 60)  // 预测未来1分钟
  
  // 验证预测结果
  assert_eq(prediction_result.values.length(), 60)
  assert_true(prediction_result.confidence_score > 0.0 && prediction_result.confidence_score <= 1.0)
  
  // 验证预测值的合理性
  for predicted_value in prediction_result.values {
    assert_true(predicted_value >= 15.0 && predicted_value <= 35.0)  // 预测值应在合理范围内
  }
}

test "边缘设备上的自适应遥测策略" {
  // 创建自适应遥测配置
  let adaptive_config = AdaptiveTelemetryConfig::new()
  AdaptiveTelemetryConfig::enable_resource_based_adaptation(adaptive_config, true)
  AdaptiveTelemetryConfig::enable_network_based_adaptation(adaptive_config, true)
  AdaptiveTelemetryConfig::enable_battery_based_adaptation(adaptive_config, true)
  AdaptiveTelemetryConfig::enable_workload_based_adaptation(adaptive_config, true)
  
  // 创建自适应遥测管理器
  let adaptive_manager = AdaptiveTelemetryManager::new(adaptive_config)
  
  // 模拟不同的边缘设备状态
  let normal_state = EdgeDeviceState::new()
  EdgeDeviceState::set_cpu_usage(normal_state, 30.0)  // 30% CPU使用率
  EdgeDeviceState::set_memory_usage(normal_state, 40.0)  // 40% 内存使用率
  EdgeDeviceState::set_battery_level(normal_state, 80.0)  // 80% 电量
  EdgeDeviceState::set_network_quality(normal_state, "good")  // 良好网络
  EdgeDeviceState::set_workload_level(normal_state, "medium")  // 中等工作负载
  
  // 测试正常状态下的遥测策略
  let normal_strategy = AdaptiveTelemetryManager::calculate_strategy(adaptive_manager, normal_state)
  
  assert_eq(normal_strategy.sampling_rate, 0.5)  // 正常采样率
  assert_eq(normal_strategy.batch_size, 50)  // 正常批量大小
  assert_eq(normal_strategy.compression_level, "medium")  // 中等压缩
  assert_eq(normal_strategy.transmission_frequency, 30)  // 30秒传输一次
  
  // 模拟资源受限状态
  let resource_constrained_state = EdgeDeviceState::new()
  EdgeDeviceState::set_cpu_usage(resource_constrained_state, 85.0)  // 85% CPU使用率
  EdgeDeviceState::set_memory_usage(resource_constrained_state, 90.0)  // 90% 内存使用率
  EdgeDeviceState::set_battery_level(resource_constrained_state, 80.0)  // 80% 电量
  EdgeDeviceState::set_network_quality(resource_constrained_state, "good")  // 良好网络
  EdgeDeviceState::set_workload_level(resource_constrained_state, "high")  // 高工作负载
  
  // 测试资源受限状态下的遥测策略
  let resource_constrained_strategy = AdaptiveTelemetryManager::calculate_strategy(adaptive_manager, resource_constrained_state)
  
  assert_true(resource_constrained_strategy.sampling_rate < normal_strategy.sampling_rate)  // 降低采样率
  assert_true(resource_constrained_strategy.batch_size < normal_strategy.batch_size)  // 减小批量大小
  assert_eq(resource_constrained_strategy.compression_level, "high")  // 提高压缩级别
  assert_true(resource_constrained_strategy.transmission_frequency > normal_strategy.transmission_frequency)  // 降低传输频率
  
  // 模拟低电量状态
  let low_battery_state = EdgeDeviceState::new()
  EdgeDeviceState::set_cpu_usage(low_battery_state, 30.0)  // 30% CPU使用率
  EdgeDeviceState::set_memory_usage(low_battery_state, 40.0)  // 40% 内存使用率
  EdgeDeviceState::set_battery_level(low_battery_state, 15.0)  // 15% 电量
  EdgeDeviceState::set_network_quality(low_battery_state, "good")  // 良好网络
  EdgeDeviceState::set_workload_level(low_battery_state, "medium")  // 中等工作负载
  
  // 测试低电量状态下的遥测策略
  let low_battery_strategy = AdaptiveTelemetryManager::calculate_strategy(adaptive_manager, low_battery_state)
  
  assert_true(low_battery_strategy.sampling_rate < normal_strategy.sampling_rate)  // 降低采样率
  assert_true(low_battery_strategy.transmission_frequency > normal_strategy.transmission_frequency)  // 降低传输频率
  assert_true(low_battery_strategy.enable_power_saving)  // 启用省电模式
  
  // 模拟网络质量差状态
  let poor_network_state = EdgeDeviceState::new()
  EdgeDeviceState::set_cpu_usage(poor_network_state, 30.0)  // 30% CPU使用率
  EdgeDeviceState::set_memory_usage(poor_network_state, 40.0)  // 40% 内存使用率
  EdgeDeviceState::set_battery_level(poor_network_state, 80.0)  // 80% 电量
  EdgeDeviceState::set_network_quality(poor_network_state, "poor")  // 差网络
  EdgeDeviceState::set_workload_level(poor_network_state, "medium")  // 中等工作负载
  
  // 测试网络质量差状态下的遥测策略
  let poor_network_strategy = AdaptiveTelemetryManager::calculate_strategy(adaptive_manager, poor_network_state)
  
  assert_eq(poor_network_strategy.compression_level, "high")  // 提高压缩级别
  assert_true(poor_network_strategy.batch_size > normal_strategy.batch_size)  // 增大批量大小以减少传输次数
  assert_true(poor_network_strategy.enable_offline_mode)  // 启用离线模式
  assert_true(poor_network_strategy.retry_attempts > 3)  // 增加重试次数
  
  // 测试策略动态切换
  let strategy_history = []
  
  // 模拟设备状态变化
  let states = [normal_state, resource_constrained_state, low_battery_state, poor_network_state, normal_state]
  
  for state in states {
    let strategy = AdaptiveTelemetryManager::calculate_strategy(adaptive_manager, state)
    strategy_history = strategy_history.push(strategy)
    
    // 应用策略
    AdaptiveTelemetryManager::apply_strategy(adaptive_manager, strategy)
    
    // 验证策略已应用
    let current_strategy = AdaptiveTelemetryManager::get_current_strategy(adaptive_manager)
    assert_eq(current_strategy.sampling_rate, strategy.sampling_rate)
    assert_eq(current_strategy.batch_size, strategy.batch_size)
    assert_eq(current_strategy.compression_level, strategy.compression_level)
  }
  
  // 验证策略历史记录
  assert_eq(strategy_history.length(), 5)
  
  // 验证策略切换的性能影响
  let strategy_switch_times = []
  
  for i in 1..=4 {
    let switch_start_time = Time::now()
    AdaptiveTelemetryManager::apply_strategy(adaptive_manager, strategy_history[i])
    let switch_end_time = Time::now()
    let switch_duration = switch_end_time - switch_start_time
    strategy_switch_times = strategy_switch_times.push(switch_duration)
    
    // 验证策略切换时间在合理范围内
    assert_true(switch_duration < 100000000L)  // 策略切换应小于100ms
  }
  
  // 计算平均策略切换时间
  let avg_switch_time = strategy_switch_times.reduce(fn(acc, time) { acc + time }, 0L) / strategy_switch_times.length()
  assert_true(avg_switch_time < 50000000L)  // 平均切换时间应小于50ms
}