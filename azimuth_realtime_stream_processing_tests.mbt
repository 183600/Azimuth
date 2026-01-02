// Azimuth实时流处理测试用例
// 测试Azimuth遥测系统的实时流处理能力和特性

test "实时遥测数据流创建和基本操作" {
  // 创建实时流处理配置
  let stream_config = RealtimeStreamConfig::new()
  RealtimeStreamConfig::set_buffer_size(stream_config, 10000)  // 10000个数据点缓冲区
  RealtimeStreamConfig::set_batch_size(stream_config, 100)  // 100个数据点一批
  RealtimeStreamConfig::set_processing_interval(stream_config, 1000)  // 1秒处理间隔
  RealtimeStreamConfig::enable_auto_scaling(stream_config, true)
  
  // 创建实时遥测数据流
  let telemetry_stream = RealtimeTelemetryStream::new(stream_config)
  
  // 测试流的基本属性
  assert_eq(RealtimeTelemetryStream::get_buffer_size(telemetry_stream), 10000)
  assert_eq(RealtimeTelemetryStream::get_batch_size(telemetry_stream), 100)
  assert_eq(RealtimeTelemetryStream::get_processing_interval(telemetry_stream), 1000)
  
  // 测试流的初始状态
  assert_eq(RealtimeTelemetryStream::get_status(telemetry_stream), "initialized")
  assert_eq(RealtimeTelemetryStream::get_buffer_usage(telemetry_stream), 0)
  assert_eq(RealtimeTelemetryStream::get_total_processed(telemetry_stream), 0)
  
  // 启动流处理
  RealtimeTelemetryStream::start(telemetry_stream)
  assert_eq(RealtimeTelemetryStream::get_status(telemetry_stream), "running")
  
  // 生成测试数据并添加到流中
  let base_timestamp = 1735689600000000000L  // 基准时间戳
  
  for i in 0..=999 {  // 1000个数据点
    let timestamp = base_timestamp + (i * 1000000000L)  // 每秒一个数据点
    let telemetry_data = TelemetryData::new(
      "realtime.metric." + (i % 10).to_string(),  // 10种不同的度量
      @random() * 100.0,
      [("source", "sensor-" + (i % 5).to_string()), ("index", i.to_string())]
    )
    
    RealtimeTelemetryStream::add_data(telemetry_stream, telemetry_data)
  }
  
  // 等待处理完成
  Thread::sleep(2000)  // 等待2秒
  
  // 验证数据处理
  assert_eq(RealtimeTelemetryStream::get_total_received(telemetry_stream), 1000)
  assert_true(RealtimeTelemetryStream::get_total_processed(telemetry_stream) > 0)
  assert_true(RealtimeTelemetryStream::get_buffer_usage(telemetry_stream) < 10000)
  
  // 停止流处理
  RealtimeTelemetryStream::stop(telemetry_stream)
  assert_eq(RealtimeTelemetryStream::get_status(telemetry_stream), "stopped")
  
  // 获取处理统计信息
  let stats = RealtimeTelemetryStream::get_processing_stats(telemetry_stream)
  
  assert_true(stats.contains_key("total_received"))
  assert_true(stats.contains_key("total_processed"))
  assert_true(stats.contains_key("processing_rate"))
  assert_true(stats.contains_key("average_latency"))
  assert_true(stats.contains_key("error_count"))
}

test "实时流窗口操作和聚合" {
  // 创建窗口流处理配置
  let window_config = WindowStreamConfig::new()
  WindowStreamConfig::set_window_type(window_config, "tumbling")  // 滚动窗口
  WindowStreamConfig::set_window_size(window_config, 5000)  // 5秒窗口
  WindowStreamConfig::set_slide_interval(window_config, 5000)  // 5秒滑动间隔
  WindowStreamConfig::set_allowed_lateness(window_config, 1000)  // 允许1秒延迟
  
  // 创建窗口流处理器
  let window_processor = WindowStreamProcessor::new(window_config)
  
  // 注册窗口聚合函数
  WindowStreamProcessor::register_aggregator(window_processor, "average", fn(values) {
    if values.length() == 0 { 0.0 } else {
      values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
    }
  })
  
  WindowStreamProcessor::register_aggregator(window_processor, "max", fn(values) {
    if values.length() == 0 { 0.0 } else {
      values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, 0.0)
    }
  })
  
  WindowStreamProcessor::register_aggregator(window_processor, "min", fn(values) {
    if values.length() == 0 { 0.0 } else {
      values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, 999999.0)
    }
  })
  
  WindowStreamProcessor::register_aggregator(window_processor, "count", fn(values) {
    values.length().to_float()
  })
  
  // 启动窗口处理器
  WindowStreamProcessor::start(window_processor)
  
  // 生成带有时间戳的测试数据
  let base_timestamp = 1735689600000000000L
  let window_results = []
  
  // 生成20秒的数据，每秒10个数据点
  for second in 0..=19 {
    for point in 0..=9 {
      let timestamp = base_timestamp + ((second * 10 + point) * 100000000L)  // 每100ms一个点
      let value = 50.0 + (10.0 * @sin((second + point/10.0).to_float() * 0.5)) + ((@random() * 5.0) - 2.5)
      
      let telemetry_data = TelemetryData::new(
        "window.test.metric",
        value,
        [("second", second.to_string()), ("point", point.to_string())]
      )
      
      WindowStreamProcessor::add_data(window_processor, telemetry_data)
    }
    
    // 每秒等待一下以模拟实时数据流
    Thread::sleep(100)
    
    // 检查是否有窗口完成
    let completed_windows = WindowStreamProcessor::get_completed_windows(window_processor)
    for window in completed_windows {
      let window_start = window.start_time
      let window_end = window.end_time
      let window_data = window.data
      
      // 计算聚合结果
      let values = window_data.map(fn(d) { d.value })
      let average = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
      let max = values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, 0.0)
      let min = values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, 999999.0)
      let count = values.length().to_float()
      
      let window_result = {
        "window_start": window_start,
        "window_end": window_end,
        "average": average,
        "max": max,
        "min": min,
        "count": count
      }
      
      window_results = window_results.push(window_result)
    }
  }
  
  // 等待所有窗口处理完成
  Thread::sleep(2000)
  
  // 停止窗口处理器
  WindowStreamProcessor::stop(window_processor)
  
  // 验证窗口结果
  assert_true(window_results.length() >= 3)  // 至少应该有3个完整的5秒窗口
  assert_true(window_results.length() <= 4)  // 最多4个窗口（20秒/5秒）
  
  // 验证每个窗口的数据
  for window_result in window_results {
    assert_true(window_result.count > 0)  // 每个窗口应该有数据
    assert_true(window_result.max >= window_result.average)  // 最大值应该大于等于平均值
    assert_true(window_result.min <= window_result.average)  // 最小值应该小于等于平均值
    assert_true(window_result.average >= 35.0 && window_result.average <= 65.0)  // 平均值应该在合理范围内
  }
  
  // 验证窗口时间间隔
  for i in 1..=window_results.length()-1 {
    let prev_window = window_results[i-1]
    let curr_window = window_results[i]
    
    let window_duration = curr_window.window_end - curr_window.window_start
    let window_gap = curr_window.window_start - prev_window.window_end
    
    assert_eq(window_duration, 5000000000L)  // 窗口大小应该是5秒
    assert_eq(window_gap, 5000000000L)  // 窗口间隔应该是5秒
  }
}

test "实时流异常检测和警报" {
  // 创建异常检测配置
  let anomaly_config = AnomalyDetectionConfig::new()
  AnomalyDetectionConfig::set_detection_method(anomaly_config, "statistical")  // 统计方法
  AnomalyDetectionConfig::set_threshold(anomaly_config, 2.5)  // 2.5倍标准差
  AnomalyDetectionConfig::set_window_size(anomaly_config, 100)  // 100个数据点的窗口
  AnomalyDetectionConfig::set_min_samples(anomaly_config, 30)  // 最少30个样本才开始检测
  
  // 创建实时异常检测器
  let anomaly_detector = RealtimeAnomalyDetector::new(anomaly_config)
  
  // 注册警报处理器
  let detected_anomalies = []
  RealtimeAnomalyDetector::register_alert_handler(anomaly_detector, fn(anomaly) {
    detected_anomalies.push(anomaly)
  })
  
  // 启动异常检测器
  RealtimeAnomalyDetector::start(anomaly_detector)
  
  // 生成包含异常的测试数据
  let base_timestamp = 1735689600000000000L
  
  // 先生成正常数据建立基线
  for i in 0..=199 {  // 200个正常数据点
    let timestamp = base_timestamp + (i * 500000000L)  // 每500ms一个点
    let normal_value = 50.0 + (10.0 * @sin(i.to_float() * 0.1)) + ((@random() * 4.0) - 2.0)
    
    let telemetry_data = TelemetryData::new(
      "anomaly.test.metric",
      normal_value,
      [("type", "normal"), ("index", i.to_string())]
    )
    
    RealtimeAnomalyDetector::add_data(anomaly_detector, telemetry_data)
  }
  
  // 等待基线建立
  Thread::sleep(1000)
  
  // 插入异常值
  let anomaly_indices = [250, 350, 450, 550]  // 异常值的位置
  
  for i in 200..=599 {
    let timestamp = base_timestamp + (i * 500000000L)
    
    let value = if anomaly_indices.contains(i) {
      // 生成异常值（超出正常范围）
      if i % 2 == 0 { 90.0 } else { 10.0 }  // 高值和低值异常
    } else {
      // 继续生成正常值
      50.0 + (10.0 * @sin(i.to_float() * 0.1)) + ((@random() * 4.0) - 2.0)
    }
    
    let data_type = if anomaly_indices.contains(i) { "anomaly" } else { "normal" }
    
    let telemetry_data = TelemetryData::new(
      "anomaly.test.metric",
      value,
      [("type", data_type), ("index", i.to_string())]
    )
    
    RealtimeAnomalyDetector::add_data(anomaly_detector, telemetry_data)
    
    // 每50个数据点检查一次异常检测结果
    if i % 50 == 0 {
      Thread::sleep(100)
    }
  }
  
  // 等待异常检测完成
  Thread::sleep(2000)
  
  // 停止异常检测器
  RealtimeAnomalyDetector::stop(anomaly_detector)
  
  // 验证异常检测结果
  assert_true(detected_anomalies.length() >= 3)  // 至少应该检测到3个异常
  
  // 验证检测到的异常的时间戳和值
  for anomaly in detected_anomalies {
    assert_true(anomaly.timestamp >= base_timestamp)
    assert_true(anomaly.severity_score > 2.5)  // 严重程度分数应超过阈值
    assert_true(anomaly.metric_name == "anomaly.test.metric")
    
    // 验证异常值确实超出正常范围
    assert_true(anomaly.value > 70.0 || anomaly.value < 30.0)
  }
  
  // 验证异常检测的统计信息
  let detection_stats = RealtimeAnomalyDetector::get_detection_stats(anomaly_detector)
  
  assert_true(detection_stats.contains_key("total_data_points"))
  assert_true(detection_stats.contains_key("total_anomalies"))
  assert_true(detection_stats.contains_key("detection_rate"))
  assert_true(detection_stats.contains_key("average_detection_time"))
  
  // 验证检测率在合理范围内
  match detection_stats.get("detection_rate") {
    Some(rate) => {
      let rate_value = rate.to_float()
      assert_true(rate_value > 0.005 && rate_value < 0.02)  // 检测率应该在0.5%到2%之间
    }
    None => assert_true(false)
  }
}

test "实时流数据转换和过滤" {
  // 创建数据流转换配置
  let transformation_config = StreamTransformationConfig::new()
  StreamTransformationConfig::set_parallelism(transformation_config, 4)  // 4个并行处理线程
  StreamTransformationConfig::set_buffer_size(transformation_config, 5000)  // 5000个数据点缓冲区
  
  // 创建实时流转换器
  let stream_transformer = RealtimeStreamTransformer::new(transformation_config)
  
  // 注册数据转换函数
  // 1. 数据标准化转换
  RealtimeStreamTransformer::register_transformation(stream_transformer, "normalize", fn(data) {
    let normalized_value = (data.value - 50.0) / 25.0  // 标准化到[-1, 1]范围
    TelemetryData::new(
      data.metric_name + ".normalized",
      normalized_value,
      data.attributes + [("transformation", "normalize")]
    )
  })
  
  // 2. 数据过滤转换
  RealtimeStreamTransformer::register_transformation(stream_transformer, "filter", fn(data) {
    if data.value >= 30.0 && data.value <= 70.0 {
      Some(data)  // 保留在[30, 70]范围内的数据
    } else {
      None  // 过滤掉超出范围的数据
    }
  })
  
  // 3. 数据聚合转换
  let aggregation_state = @ref([])
  RealtimeStreamTransformer::register_transformation(stream_transformer, "aggregate", fn(data) {
    let current_batch = aggregation_state[]
    let updated_batch = current_batch.push(data)
    
    if updated_batch.length() >= 10 {  // 每10个数据点聚合一次
      let values = updated_batch.map(fn(d) { d.value })
      let avg_value = values.reduce(fn(acc, val) { acc + val }, 0.0) / values.length().to_float()
      
      aggregation_state := []  // 重置批次
      
      Some(TelemetryData::new(
        data.metric_name + ".aggregated",
        avg_value,
        data.attributes + [("transformation", "aggregate"), ("batch_size", "10")]
      ))
    } else {
      aggregation_state := updated_batch
      None  // 还未达到聚合条件
    }
  })
  
  // 启动流转换器
  RealtimeStreamTransformer::start(stream_transformer)
  
  // 创建输出收集器
  let normalize_output = []
  let filter_output = []
  let aggregate_output = []
  
  // 注册输出处理器
  RealtimeStreamTransformer::register_output_handler(stream_transformer, "normalize", fn(data) {
    normalize_output.push(data)
  })
  
  RealtimeStreamTransformer::register_output_handler(stream_transformer, "filter", fn(data) {
    filter_output.push(data)
  })
  
  RealtimeStreamTransformer::register_output_handler(stream_transformer, "aggregate", fn(data) {
    aggregate_output.push(data)
  })
  
  // 生成测试数据
  let base_timestamp = 1735689600000000000L
  
  for i in 0..=99 {  // 100个数据点
    let timestamp = base_timestamp + (i * 1000000000L)
    let value = 50.0 + (30.0 * @sin(i.to_float() * 0.1)) + ((@random() * 10.0) - 5.0)
    
    let telemetry_data = TelemetryData::new(
      "transformation.test.metric",
      value,
      [("index", i.to_string())]
    )
    
    RealtimeStreamTransformer::add_data(stream_transformer, telemetry_data)
  }
  
  // 等待处理完成
  Thread::sleep(2000)
  
  // 停止流转换器
  RealtimeStreamTransformer::stop(stream_transformer)
  
  // 验证标准化转换结果
  assert_eq(normalize_output.length(), 100)  // 所有数据都应该被标准化
  
  for normalized_data in normalize_output {
    assert_eq(normalized_data.metric_name, "transformation.test.metric.normalized")
    assert_true(normalized_data.attributes.contains_key("transformation"))
    assert_eq(normalized_data.attributes.get("transformation"), Some("normalize"))
    assert_true(normalized_data.value >= -1.0 && normalized_data.value <= 1.0)  // 标准化后的值应在[-1, 1]范围内
  }
  
  // 验证过滤转换结果
  assert_true(filter_output.length() < 100)  // 过滤后数据应该减少
  assert_true(filter_output.length() > 0)  // 但不应该全部被过滤
  
  for filtered_data in filter_output {
    assert_eq(filtered_data.metric_name, "transformation.test.metric")
    assert_true(filtered_data.value >= 30.0 && filtered_data.value <= 70.0)  // 过滤后的值应在[30, 70]范围内
  }
  
  // 验证聚合转换结果
  assert_eq(aggregate_output.length(), 10)  // 100个数据点应该产生10个聚合结果
  
  for aggregated_data in aggregate_output {
    assert_eq(aggregated_data.metric_name, "transformation.test.metric.aggregated")
    assert_true(aggregated_data.attributes.contains_key("transformation"))
    assert_eq(aggregated_data.attributes.get("transformation"), Some("aggregate"))
    assert_eq(aggregated_data.attributes.get("batch_size"), Some("10"))
  }
  
  // 验证转换统计信息
  let transform_stats = RealtimeStreamTransformer::get_transformation_stats(stream_transformer)
  
  assert_true(transform_stats.contains_key("total_input"))
  assert_true(transform_stats.contains_key("normalize_output"))
  assert_true(transform_stats.contains_key("filter_output"))
  assert_true(transform_stats.contains_key("aggregate_output"))
  assert_true(transform_stats.contains_key("average_processing_time"))
  
  // 验证处理时间在合理范围内
  match transform_stats.get("average_processing_time") {
    Some(time) => {
      let time_value = time.to_float()
      assert_true(time_value < 1000000.0)  // 平均处理时间应小于1ms
    }
    None => assert_true(false)
  }
}

test "实时流多源数据融合" {
  // 创建多源数据融合配置
  let fusion_config = MultiSourceFusionConfig::new()
  MultiSourceFusionConfig::set_source_count(fusion_config, 3)  // 3个数据源
  MultiSourceFusionConfig::set_sync_window(fusion_config, 2000)  // 2秒同步窗口
  MultiSourceFusionConfig::set_fusion_strategy(fusion_config, "time_based")  // 基于时间的融合策略
  
  // 创建多源数据融合器
  let data_fusion = MultiSourceDataFusion::new(fusion_config)
  
  // 注册数据源
  MultiSourceDataFusion::register_source(data_fusion, "source-1", "sensor.temperature")
  MultiSourceDataFusion::register_source(data_fusion, "source-2", "sensor.pressure")
  MultiSourceDataFusion::register_source(data_fusion, "source-3", "sensor.humidity")
  
  // 注册融合函数
  MultiSourceDataFusion::register_fusion_function(data_fusion, "correlation", fn(data_points) {
    // 计算不同传感器数据之间的相关性
    if data_points.length() < 2 {
      None
    } else {
      let temp_data = data_points.filter(fn(d) { d.metric_name.contains("temperature") })
      let pressure_data = data_points.filter(fn(d) { d.metric_name.contains("pressure") })
      let humidity_data = data_points.filter(fn(d) { d.metric_name.contains("humidity") })
      
      if temp_data.length() > 0 && pressure_data.length() > 0 && humidity_data.length() > 0 {
        let temp_value = temp_data[0].value
        let pressure_value = pressure_data[0].value
        let humidity_value = humidity_data[0].value
        
        // 计算一个综合的环境舒适度指标
        let comfort_score = 100.0 - 
                           (@abs(temp_value - 22.0) * 2.0) -  // 温度偏离22度的程度
                           (@abs(pressure_value - 101.3) * 1.0) -  // 压力偏离101.3kPa的程度
                           (@abs(humidity_value - 45.0) * 0.5)  // 湿度偏离45%的程度
        
        Some(TelemetryData::new(
          "environment.comfort.score",
          comfort_score,
          [
            ("fusion_type", "correlation"),
            ("temp", temp_value.to_string()),
            ("pressure", pressure_value.to_string()),
            ("humidity", humidity_value.to_string())
          ]
        ))
      } else {
        None
      }
    }
  })
  
  // 启动数据融合器
  MultiSourceDataFusion::start(data_fusion)
  
  // 创建融合结果收集器
  let fusion_results = []
  
  // 注册融合结果处理器
  MultiSourceDataFusion::register_result_handler(data_fusion, fn(result) {
    fusion_results.push(result)
  })
  
  // 生成多源测试数据
  let base_timestamp = 1735689600000000000L
  
  for i in 0..=49 {  // 50个时间点
    let timestamp = base_timestamp + (i * 2000000000L)  // 每2秒一个时间点
    
    // 生成温度数据（20-30度）
    let temp_value = 22.0 + (8.0 * @sin(i.to_float() * 0.1)) + ((@random() * 2.0) - 1.0)
    let temp_data = TelemetryData::new(
      "sensor.temperature",
      temp_value,
      [("source", "source-1"), ("timestamp", timestamp.to_string())]
    )
    
    // 生成压力数据（95-105 kPa）
    let pressure_value = 101.3 + (3.7 * @cos(i.to_float() * 0.15)) + ((@random() * 1.0) - 0.5)
    let pressure_data = TelemetryData::new(
      "sensor.pressure",
      pressure_value,
      [("source", "source-2"), ("timestamp", timestamp.to_string())]
    )
    
    // 生成湿度数据（40-50%）
    let humidity_value = 45.0 + (5.0 * @sin(i.to_float() * 0.2 + 1.0)) + ((@random() * 2.0) - 1.0)
    let humidity_data = TelemetryData::new(
      "sensor.humidity",
      humidity_value,
      [("source", "source-3"), ("timestamp", timestamp.to_string())]
    )
    
    // 添加数据到融合器（带有一些随机延迟模拟真实网络）
    MultiSourceDataFusion::add_data(data_fusion, temp_data)
    Thread::sleep((@random() * 200).to_int())  // 0-200ms随机延迟
    
    MultiSourceDataFusion::add_data(data_fusion, pressure_data)
    Thread::sleep((@random() * 200).to_int())  // 0-200ms随机延迟
    
    MultiSourceDataFusion::add_data(data_fusion, humidity_data)
    Thread::sleep((@random() * 200).to_int())  // 0-200ms随机延迟
  }
  
  // 等待融合处理完成
  Thread::sleep(5000)
  
  // 停止数据融合器
  MultiSourceDataFusion::stop(data_fusion)
  
  // 验证融合结果
  assert_true(fusion_results.length() > 0)  // 应该有融合结果
  assert_true(fusion_results.length() <= 50)  // 但不应该超过输入时间点数量
  
  // 验证融合结果的数据结构
  for fusion_result in fusion_results {
    assert_eq(fusion_result.metric_name, "environment.comfort.score")
    assert_true(fusion_result.attributes.contains_key("fusion_type"))
    assert_eq(fusion_result.attributes.get("fusion_type"), Some("correlation"))
    assert_true(fusion_result.attributes.contains_key("temp"))
    assert_true(fusion_result.attributes.contains_key("pressure"))
    assert_true(fusion_result.attributes.contains_key("humidity"))
    
    // 验证舒适度分数在合理范围内
    assert_true(fusion_result.value >= 0.0 && fusion_result.value <= 100.0)
  }
  
  // 验证融合统计信息
  let fusion_stats = MultiSourceDataFusion::get_fusion_stats(data_fusion)
  
  assert_true(fusion_stats.contains_key("total_input_data"))
  assert_true(fusion_stats.contains_key("total_fusion_results"))
  assert_true(fusion_stats.contains_key("fusion_rate"))
  assert_true(fusion_stats.contains_key("average_fusion_time"))
  assert_true(fusion_stats.contains_key("sync_window_misses"))
  
  // 验证融合率
  match fusion_stats.get("fusion_rate") {
    Some(rate) => {
      let rate_value = rate.to_float()
      assert_true(rate_value > 0.5 && rate_value <= 1.0)  // 融合率应该在50%到100%之间
    }
    None => assert_true(false)
  }
}