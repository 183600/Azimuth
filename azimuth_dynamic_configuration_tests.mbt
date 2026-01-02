// 动态配置更新测试用例
// 测试Azimuth遥测系统的动态配置更新功能

test "运行时采样率动态调整" {
  // 创建初始配置
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(config, 0.1)  // 初始10%采样率
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "dynamic.sampling.test")
  
  // 创建大量span测试初始采样率
  let initial_spans = []
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(tracer, "initial.sample.span." + i.to_string())
    initial_spans = initial_spans + [span]
    Span::end(span)
  }
  
  // 动态更新采样率
  let new_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(new_config, 1.0)  // 更新为100%采样率
  
  TracerProvider::update_config(tracer_provider, new_config)
  
  // 验证配置已更新
  let current_config = TracerProvider::get_config(tracer_provider)
  let current_sampling_rate = TelemetryConfiguration::get_sampling_rate(current_config)
  assert_eq(current_sampling_rate, 1.0)
  
  // 创建新span测试更新后的采样率
  let updated_spans = []
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(tracer, "updated.sample.span." + i.to_string())
    updated_spans = updated_spans + [span]
    Span::end(span)
  }
  
  // 验证采样率变化的影响（通过检查导出的span数量）
  let exported_spans = TracerProvider::get_exported_spans(tracer_provider)
  
  // 更新后的span应该全部被采样，而初始的span只有10%被采样
  let initial_exported = exported_spans.filter(fn(s) { 
    Span::name(s).contains("initial.sample.span") 
  })
  let updated_exported = exported_spans.filter(fn(s) { 
    Span::name(s).contains("updated.sample.span") 
  })
  
  assert_true(initial_exported.length() < updated_exported.length())
  assert_true(updated_exported.length() >= 90)  // 允许一些误差
  
  assert_true(true)
}

test "动态批处理大小调整" {
  // 创建初始配置
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_batch_size(config, 10)  // 初始批处理大小为10
  
  let meter_provider = MeterProvider::builder()
    .with_config(config)
    .build()
  
  let meter = MeterProvider::get_meter(meter_provider, "dynamic.batch.test")
  let counter = Meter::create_counter(meter, "dynamic.batch.counter")
  
  // 添加度量数据
  for i = 0; i < 25; i = i + 1 {
    Counter::add_with_attributes(counter, 1.0, [
      ("iteration", i.to_string()),
      ("batch", "initial")
    ])
  }
  
  // 检查初始批处理行为
  let initial_batches = MeterProvider::get_batch_count(meter_provider)
  
  // 动态更新批处理大小
  let new_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_batch_size(new_config, 5)  // 减小批处理大小
  
  MeterProvider::update_config(meter_provider, new_config)
  
  // 验证配置已更新
  let current_config = MeterProvider::get_config(meter_provider)
  let current_batch_size = TelemetryConfiguration::get_batch_size(current_config)
  assert_eq(current_batch_size, 5)
  
  // 添加更多度量数据
  for i = 25; i < 50; i = i + 1 {
    Counter::add_with_attributes(counter, 1.0, [
      ("iteration", i.to_string()),
      ("batch", "updated")
    ])
  }
  
  // 强制刷新以处理剩余数据
  MeterProvider::flush(meter_provider)
  
  // 检查更新后的批处理行为
  let final_batches = MeterProvider::get_batch_count(meter_provider)
  
  // 批处理数量应该增加（因为批处理大小减小）
  assert_true(final_batches > initial_batches)
  
  assert_true(true)
}

test "动态日志级别调整" {
  // 创建初始配置
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_log_level(config, LogLevel::WARN)  // 初始只记录WARN及以上级别
  
  let logger_provider = LoggerProvider::builder()
    .with_config(config)
    .build()
  
  let logger = LoggerProvider::get_logger(logger_provider, "dynamic.log.level.test")
  
  // 记录不同级别的日志
  Logger::emit_log(logger, LogLevel::DEBUG, "Debug message - should be filtered", [])
  Logger::emit_log(logger, LogLevel::INFO, "Info message - should be filtered", [])
  Logger::emit_log(logger, LogLevel::WARN, "Warning message - should be recorded", [])
  Logger::emit_log(logger, LogLevel::ERROR, "Error message - should be recorded", [])
  
  // 检查初始日志记录
  let initial_logs = LoggerProvider::get_recorded_logs(logger_provider)
  assert_eq(initial_logs.length(), 2)  // 只有WARN和ERROR
  
  // 动态更新日志级别
  let new_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_log_level(new_config, LogLevel::DEBUG)  // 记录所有级别
  
  LoggerProvider::update_config(logger_provider, new_config)
  
  // 验证配置已更新
  let current_config = LoggerProvider::get_config(logger_provider)
  let current_log_level = TelemetryConfiguration::get_log_level(current_config)
  assert_eq(current_log_level, LogLevel::DEBUG)
  
  // 记录更多日志
  Logger::emit_log(logger, LogLevel::DEBUG, "Debug message - should now be recorded", [])
  Logger::emit_log(logger, LogLevel::INFO, "Info message - should now be recorded", [])
  Logger::emit_log(logger, LogLevel::WARN, "Warning message - should be recorded", [])
  Logger::emit_log(logger, LogLevel::ERROR, "Error message - should be recorded", [])
  
  // 检查更新后的日志记录
  let final_logs = LoggerProvider::get_recorded_logs(logger_provider)
  assert_eq(final_logs.length(), 6)  // 之前的2个 + 新的4个
  
  assert_true(true)
}

test "动态资源属性更新" {
  // 创建初始资源
  let initial_resource = Resource::builder()
    .with_service_name("azimuth-service")
    .with_service_version("1.0.0")
    .with_attribute("environment", "development")
    .with_attribute("region", "us-west")
    .build()
  
  let tracer_provider = TracerProvider::builder()
    .with_resource(initial_resource)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "dynamic.resource.test")
  
  // 创建span验证初始资源
  let span1 = Tracer::start_span(tracer, "operation.with.initial.resource")
  let resource1 = Span::get_resource(span1)
  
  assert_eq(Resource::get_attribute(resource1, "service.version"), Some("1.0.0"))
  assert_eq(Resource::get_attribute(resource1, "environment"), Some("development"))
  
  Span::end(span1)
  
  // 动态更新资源
  let updated_resource = Resource::builder()
    .with_service_name("azimuth-service")  // 保持相同
    .with_service_version("2.0.0")        // 更新版本
    .with_attribute("environment", "production")  // 更新环境
    .with_attribute("region", "us-east")  // 更新区域
    .with_attribute("deployment.type", "kubernetes")  // 新属性
    .build()
  
  TracerProvider::update_resource(tracer_provider, updated_resource)
  
  // 创建新span验证更新后的资源
  let span2 = Tracer::start_span(tracer, "operation.with.updated.resource")
  let resource2 = Span::get_resource(span2)
  
  assert_eq(Resource::get_attribute(resource2, "service.version"), Some("2.0.0"))
  assert_eq(Resource::get_attribute(resource2, "environment"), Some("production"))
  assert_eq(Resource::get_attribute(resource2, "region"), Some("us-east"))
  assert_eq(Resource::get_attribute(resource2, "deployment.type"), Some("kubernetes"))
  
  Span::end(span2)
  
  assert_true(true)
}

test "配置热重载事件处理" {
  // 创建配置监听器
  let config_listener = ConfigChangeListener::new()
  
  // 创建初始配置
  let config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(config, 0.5)
  TelemetryConfiguration::set_batch_size(config, 20)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(config)
    .with_config_listener(config_listener)
    .build()
  
  // 注册配置变更回调
  ConfigChangeListener::on_sampling_rate_changed(config_listener, fn(old_rate, new_rate) {
    // 验证采样率变更事件
    assert_true(old_rate != new_rate)
  })
  
  ConfigChangeListener::on_batch_size_changed(config_listener, fn(old_size, new_size) {
    // 验证批处理大小变更事件
    assert_true(old_size != new_size)
  })
  
  // 动态更新配置
  let new_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(new_config, 0.8)
  TelemetryConfiguration::set_batch_size(new_config, 50)
  
  TracerProvider::update_config(tracer_provider, new_config)
  
  // 验证事件被触发
  let triggered_events = ConfigChangeListener::get_triggered_events(config_listener)
  assert_true(triggered_events.length() >= 2)
  
  // 验证配置确实被更新
  let current_config = TracerProvider::get_config(tracer_provider)
  assert_eq(TelemetryConfiguration::get_sampling_rate(current_config), 0.8)
  assert_eq(TelemetryConfiguration::get_batch_size(current_config), 50)
  
  assert_true(true)
}

test "配置验证和回滚机制" {
  // 创建有效配置
  let valid_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(valid_config, 0.5)
  TelemetryConfiguration::set_batch_size(valid_config, 100)
  TelemetryConfiguration::set_max_spans(valid_config, 1000)
  
  let tracer_provider = TracerProvider::builder()
    .with_config(valid_config)
    .build()
  
  // 尝试更新为无效配置
  let invalid_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(invalid_config, 2.0)  // 无效：> 1.0
  TelemetryConfiguration::set_batch_size(invalid_config, -1)      // 无效：< 0
  TelemetryConfiguration::set_max_spans(invalid_config, 0)        // 无效：= 0
  
  // 验证无效配置被拒绝
  let update_result = TracerProvider::try_update_config(tracer_provider, invalid_config)
  match update_result {
    Ok(_) => assert_true(false)  // 不应该成功
    Error(_) => assert_true(true) // 应该返回错误
  }
  
  // 验证配置未被更改
  let current_config = TracerProvider::get_config(tracer_provider)
  assert_eq(TelemetryConfiguration::get_sampling_rate(current_config), 0.5)
  assert_eq(TelemetryConfiguration::get_batch_size(current_config), 100)
  assert_eq(TelemetryConfiguration::get_max_spans(current_config), 1000)
  
  // 测试部分无效配置的自动修正
  let partially_invalid_config = TelemetryConfiguration::new()
  TelemetryConfiguration::set_sampling_rate(partially_invalid_config, 0.8)  // 有效
  TelemetryConfiguration::set_batch_size(partially_invalid_config, -10)    // 无效
  
  // 使用自动修正模式更新配置
  let corrected_config = TelemetryConfiguration::validate_and_correct(partially_invalid_config)
  TracerProvider::update_config(tracer_provider, corrected_config)
  
  // 验证有效值被保留，无效值被修正
  let final_config = TracerProvider::get_config(tracer_provider)
  assert_eq(TelemetryConfiguration::get_sampling_rate(final_config), 0.8)   // 保留
  assert_true(TelemetryConfiguration::get_batch_size(final_config) > 0)     // 被修正
  
  assert_true(true)
}