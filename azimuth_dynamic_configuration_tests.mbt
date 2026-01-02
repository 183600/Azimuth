// Azimuth Dynamic Configuration Tests
// 测试遥测配置动态更新功能

test "dynamic sampling configuration update" {
  // 创建采样配置
  let sampling_config = @azimuth.SamplingConfig::builder()
    .with_sample_rate(0.1)  // 10%采样率
    .build()
  
  // 创建采样器
  let sampler = @azimuth.TraceIdRatioBasedSampler::new(sampling_config)
  
  // 测试初始采样决策
  let sampled_count = 0
  for i = 0; i < 1000; i = i + 1 {
    let trace_id = @azimuth.TraceId::generate()
    if sampler.should_sample(trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证约10%的采样率（允许一定误差）
  assert_true(sampled_count > 50 && sampled_count < 150)
  
  // 动态更新采样率
  let new_config = @azimuth.SamplingConfig::builder()
    .with_sample_rate(0.5)  // 50%采样率
    .build()
  
  sampler.update_config(new_config)
  
  // 测试更新后的采样决策
  let new_sampled_count = 0
  for i = 0; i < 1000; i = i + 1 {
    let trace_id = @azimuth.TraceId::generate()
    if sampler.should_sample(trace_id) {
      new_sampled_count = new_sampled_count + 1
    }
  }
  
  // 验证约50%的采样率
  assert_true(new_sampled_count > 450 && new_sampled_count < 550)
}

test "dynamic batch processor configuration" {
  // 创建批处理器配置
  let batch_config = @azimuth.BatchConfig::builder()
    .with_max_queue_size(100)
    .with_max_export_batch_size(10)
    .with_schedule_delay_millis(5000)
    .build()
  
  // 创建批处理器
  let processor = @azimuth.BatchSpanProcessor::with_config(batch_config)
  
  // 验证初始配置
  assert_eq(processor.get_max_queue_size(), 100)
  assert_eq(processor.get_max_export_batch_size(), 10)
  assert_eq(processor.get_schedule_delay_millis(), 5000)
  
  // 动态更新配置
  let new_config = @azimuth.BatchConfig::builder()
    .with_max_queue_size(200)
    .with_max_export_batch_size(20)
    .with_schedule_delay_millis(2000)
    .build()
  
  processor.update_config(new_config)
  
  // 验证更新后的配置
  assert_eq(processor.get_max_queue_size(), 200)
  assert_eq(processor.get_max_export_batch_size(), 20)
  assert_eq(processor.get_schedule_delay_millis(), 2000)
}

test "dynamic metric export interval update" {
  // 创建度量导出器配置
  let export_config = @azimuth.MetricExportConfig::builder()
    .with_export_interval_millis(60000)  // 1分钟
    .with_timeout_millis(30000)
    .build()
  
  // 创建度量导出器
  let exporter = @azimuth.PeriodicMetricExporter::with_config(export_config)
  
  // 验证初始导出间隔
  assert_eq(exporter.get_export_interval_millis(), 60000)
  
  // 动态更新导出间隔
  let new_config = @azimuth.MetricExportConfig::builder()
    .with_export_interval_millis(30000)  // 30秒
    .with_timeout_millis(30000)
    .build()
  
  exporter.update_config(new_config)
  
  // 验证更新后的导出间隔
  assert_eq(exporter.get_export_interval_millis(), 30000)
}

test "dynamic log level configuration" {
  // 创建日志配置
  let log_config = @azimuth.LogConfig::builder()
    .with_level(@azimuth.LogLevel::INFO)
    .with_include_timestamp(true)
    .build()
  
  // 创建日志记录器
  let logger = @azimuth.Logger::with_config(log_config)
  
  // 验证初始日志级别
  assert_eq(logger.get_level(), @azimuth.LogLevel::INFO)
  
  // 记录不同级别的日志
  logger.debug("Debug message")  // 不应被记录
  logger.info("Info message")    // 应被记录
  logger.error("Error message")  // 应被记录
  
  // 验证日志记录数量
  assert_eq(logger.get_log_count(), 2)
  
  // 动态更新日志级别
  let new_config = @azimuth.LogConfig::builder()
    .with_level(@azimuth.LogLevel::DEBUG)
    .with_include_timestamp(true)
    .build()
  
  logger.update_config(new_config)
  
  // 验证更新后的日志级别
  assert_eq(logger.get_level(), @azimuth.LogLevel::DEBUG)
  
  // 记录不同级别的日志
  logger.debug("Debug message")  // 现在应被记录
  logger.info("Info message")    // 应被记录
  logger.error("Error message")  // 应被记录
  
  // 验证日志记录数量
  assert_eq(logger.get_log_count(), 5)
}

test "dynamic service resource attributes" {
  // 创建服务资源
  let resource = @azimuth.Resource::builder()
    .with_service_name("my-service")
    .with_service_version("1.0.0")
    .with_attribute("environment", "production")
    .build()
  
  // 验证初始资源属性
  assert_eq(resource.get_attribute("service.name"), Some("my-service"))
  assert_eq(resource.get_attribute("service.version"), Some("1.0.0"))
  assert_eq(resource.get_attribute("environment"), Some("production"))
  
  // 动态更新资源属性
  resource.merge(@azimuth.Resource::builder()
    .with_service_version("1.1.0")
    .with_attribute("environment", "staging")
    .with_attribute("region", "us-west")
    .build())
  
  // 验证更新后的属性
  assert_eq(resource.get_attribute("service.name"), Some("my-service"))  // 不变
  assert_eq(resource.get_attribute("service.version"), Some("1.1.0"))   // 更新
  assert_eq(resource.get_attribute("environment"), Some("staging"))     // 更新
  assert_eq(resource.get_attribute("region"), Some("us-west"))          // 新增
}

test "configuration change notifications" {
  // 创建配置管理器
  let config_manager = @azimuth.ConfigurationManager::new()
  
  // 注册配置变更监听器
  let notification_received = @azimuth.AtomicBool::new(false)
  
  let listener = @azimuth.ConfigChangeListener::new(fn(old_config, new_config) {
    notification_received.set(true)
  })
  
  config_manager.add_listener(listener)
  
  // 更新配置
  let new_config = @azimuth.TelemetryConfig::builder()
    .with_service_name("updated-service")
    .build()
  
  config_manager.update_config(new_config)
  
  // 验证监听器收到通知
  assert_true(notification_received.get())
  
  // 验证配置已更新
  let current_config = config_manager.get_config()
  assert_eq(current_config.service_name, "updated-service")
}

test "configuration persistence and reload" {
  // 创建配置
  let config = @azimuth.TelemetryConfig::builder()
    .with_service_name("persistent-service")
    .with_sampling_rate(0.2)
    .with_batch_size(50)
    .build()
  
  // 保存配置到文件
  let config_file = "/tmp/azimuth_config.json"
  @azimuth.ConfigPersistence::save_to_file(config, config_file)
  
  // 从文件加载配置
  let loaded_config = @azimuth.ConfigPersistence::load_from_file(config_file)
  
  // 验证加载的配置
  assert_true(loaded_config.is_some())
  match loaded_config {
    Some(cfg) => {
      assert_eq(cfg.service_name, "persistent-service")
      assert_eq(cfg.sampling_rate, 0.2)
      assert_eq(cfg.batch_size, 50)
    }
    None => assert_true(false)
  }
}