// Azimuth 额外功能测试用例
// 专注于遥测系统的扩展功能测试

// 测试1: 数据转换和格式化功能
test "数据转换和格式化功能测试" {
  // 创建遥测数据转换器
  let converter = TelemetryDataConverter::new()
  
  // 测试时间戳转换
  let timestamp = 1640995200  // Unix时间戳
  let formatted_time = TelemetryDataConverter::format_timestamp(converter, timestamp, "ISO8601")
  assert_true(formatted_time.contains("2022-01-01"))
  assert_true(formatted_time.contains("00:00:00"))
  
  // 测试字节单位转换
  let bytes = 1048576  // 1MB
  let formatted_bytes = TelemetryDataConverter::format_bytes(converter, bytes)
  assert_eq(formatted_bytes, "1.0 MB")
  
  // 测试持续时间格式化
  let duration_ms = 1250  // 1.25秒
  let formatted_duration = TelemetryDataConverter::format_duration(converter, duration_ms)
  assert_eq(formatted_duration, "1.25s")
  
  // 测试百分比格式化
  let ratio = 0.7542
  let formatted_percentage = TelemetryDataConverter::format_percentage(converter, ratio, 2)
  assert_eq(formatted_percentage, "75.42%")
}

// 测试2: 缓存机制和性能优化
test "缓存机制和性能优化测试" {
  // 创建缓存管理器
  let cache_manager = TelemetryCacheManager::new(100)  // 最大100项
  
  // 测试缓存存储
  let span_data = SpanData::new("cached.span", "trace-123", "span-456")
  let cache_key = "span-456"
  TelemetryCacheManager::put(cache_manager, cache_key, span_data)
  
  // 测试缓存检索
  let cached_span = TelemetryCacheManager::get(cache_manager, cache_key)
  assert_eq(SpanData::name(cached_span), "cached.span")
  assert_eq(SpanData::trace_id(cached_span), "trace-123")
  
  // 测试缓存统计
  let cache_stats = TelemetryCacheManager::get_stats(cache_manager)
  assert_eq(CacheStats::hits(cache_stats), 1)
  assert_eq(CacheStats::misses(cache_stats), 0)
  assert_eq(CacheStats::size(cache_stats), 1)
  
  // 测试缓存淘汰
  for i in 0..=150 {
    let temp_key = "temp-" + i.to_string()
    let temp_data = SpanData::new("temp.span", "trace-" + i.to_string(), "span-" + i.to_string())
    TelemetryCacheManager::put(cache_manager, temp_key, temp_data)
  }
  
  // 验证缓存大小限制
  let updated_stats = TelemetryCacheManager::get_stats(cache_manager)
  assert_le(CacheStats::size(updated_stats), 100)
  assert_gt(CacheStats::evictions(updated_stats), 0)
}

// 测试3: 并发处理和线程安全
test "并发处理和线程安全测试" {
  // 创建并发处理器
  let concurrent_processor = ConcurrentTelemetryProcessor::new(4)  // 4个工作线程
  
  // 创建并发任务
  let task_count = 100
  let completion_counter = AtomicInt::new(0)
  
  for i in 0..=task_count {
    let task_data = SpanData::new("concurrent.span", "trace-" + i.to_string(), "span-" + i.to_string())
    ConcurrentTelemetryProcessor::submit(concurrent_processor, task_data, fn(data) {
      // 模拟处理延迟
      Thread::sleep(10)  // 10ms
      AtomicInt::increment(completion_counter)
    })
  }
  
  // 等待所有任务完成
  ConcurrentTelemetryProcessor::wait_for_completion(concurrent_processor, 5000)  // 5秒超时
  
  // 验证所有任务完成
  assert_eq(AtomicInt::value(completion_counter), task_count + 1)
  
  // 验证处理器统计
  let processor_stats = ConcurrentTelemetryProcessor::get_stats(concurrent_processor)
  assert_eq(ProcessorStats::tasks_submitted(processor_stats), task_count + 1)
  assert_eq(ProcessorStats::tasks_completed(processor_stats), task_count + 1)
  assert_eq(ProcessorStats::active_workers(processor_stats), 4)
}

// 测试4: 异步操作和事件驱动
test "异步操作和事件驱动测试" {
  // 创建事件总线
  let event_bus = TelemetryEventBus::new()
  
  // 创建事件计数器
  let event_counter = AtomicInt::new(0)
  let event_data_store = AtomicRef::new(None)
  
  // 注册事件监听器
  TelemetryEventBus::subscribe(event_bus, "span.started", fn(event) {
    AtomicInt::increment(event_counter)
    let span_data = Event::data(event)
    AtomicRef::set(event_data_store, Some(span_data))
  })
  
  // 发布事件
  let test_span = SpanData::new("async.span", "trace-789", "span-123")
  let span_started_event = Event::new("span.started", test_span)
  TelemetryEventBus::publish(event_bus, span_started_event)
  
  // 等待事件处理
  Thread::sleep(100)  // 100ms
  
  // 验证事件处理
  assert_eq(AtomicInt::value(event_counter), 1)
  
  let stored_span_data = AtomicRef::value(event_data_store)
  match stored_span_data {
    Some(span) => {
      assert_eq(SpanData::name(span), "async.span")
      assert_eq(SpanData::trace_id(span), "trace-789")
      assert_eq(SpanData::span_id(span), "span-123")
    }
    None => assert_true(false)
  }
  
  // 测试异步批量处理
  let batch_processor = AsyncBatchProcessor::new(10, 1000)  // 批量大小10，1秒超时
  
  for i in 0..=15 {
    let batch_span = SpanData::new("batch.span", "trace-batch", "span-" + i.to_string())
    AsyncBatchProcessor::add(batch_processor, batch_span)
  }
  
  // 等待批量处理
  Thread::sleep(1200)  // 1.2秒，确保超时触发
  
  // 验证批量处理
  let batch_stats = AsyncBatchProcessor::get_stats(batch_processor)
  assert_gt(BatchStats::batches_processed(batch_stats), 0)
  assert_eq(BatchStats::items_processed(batch_stats), 16)
}

// 测试5: 数据验证和完整性检查
test "数据验证和完整性检查测试" {
  // 创建验证器
  let validator = TelemetryDataValidator::new()
  
  // 测试有效的span数据
  let valid_span = SpanData::new("valid.span", "trace-123", "span-456")
  SpanData::set_start_time(valid_span, 1640995200)
  SpanData::set_end_time(valid_span, 1640995250)
  SpanData::set_status(valid_span, Ok)
  
  let validation_result = TelemetryDataValidator::validate_span(validator, valid_span)
  assert_true(ValidationResult::is_valid(validation_result))
  assert_eq(ValidationResult::errors(validation_result).length(), 0)
  
  // 测试无效的span数据（缺少必要字段）
  let invalid_span = SpanData::new("", "", "")  // 空的必要字段
  let invalid_result = TelemetryDataValidator::validate_span(validator, invalid_span)
  assert_false(ValidationResult::is_valid(invalid_result))
  assert_gt(ValidationResult::errors(invalid_result).length(), 0)
  
  // 测试时间范围验证
  let time_invalid_span = SpanData::new("time.invalid", "trace-789", "span-123")
  SpanData::set_start_time(time_invalid_span, 1640995250)  // 开始时间晚于结束时间
  SpanData::set_end_time(time_invalid_span, 1640995200)
  
  let time_validation_result = TelemetryDataValidator::validate_span(validator, time_invalid_span)
  assert_false(ValidationResult::is_valid(time_validation_result))
  assert_true(ValidationResult::has_error(time_validation_result, "invalid_time_range"))
  
  // 测试自定义验证规则
  let custom_validator = TelemetryDataValidator::with_rules([
    ValidationRule::new("name_length", fn(span) { SpanData::name(span).length() >= 3 }),
    ValidationRule::new("trace_id_format", fn(span) { SpanData::trace_id(span).starts_with("trace-") }),
    ValidationRule::new("span_id_format", fn(span) { SpanData::span_id(span).starts_with("span-") })
  ])
  
  let custom_span = SpanData::new("ab", "invalid-trace", "span-456")  // 名字太短，trace_id格式错误
  let custom_result = TelemetryDataValidator::validate_span(custom_validator, custom_span)
  assert_false(ValidationResult::is_valid(custom_result))
  assert_true(ValidationResult::has_error(custom_result, "name_length"))
  assert_true(ValidationResult::has_error(custom_result, "trace_id_format"))
  assert_false(ValidationResult::has_error(custom_result, "span_id_format"))
}

// 测试6: 资源管理和生命周期
test "资源管理和生命周期测试" {
  // 创建资源管理器
  let resource_manager = TelemetryResourceManager::new()
  
  // 注册资源
  let tracer_resource = TelemetryResourceManager::register(resource_manager, "tracer", fn() {
    Tracer::new("test.tracer")
  }, fn(tracer) {
    Tracer::shutdown(tracer)
  })
  
  let meter_resource = TelemetryResourceManager::register(resource_manager, "meter", fn() {
    Meter::new("test.meter")
  }, fn(meter) {
    Meter::shutdown(meter)
  })
  
  // 获取资源
  let tracer = TelemetryResourceManager::get(resource_manager, "tracer")
  let meter = TelemetryResourceManager::get(resource_manager, "meter")
  
  assert_not_eq(tracer, None)
  assert_not_eq(meter, None)
  
  // 测试资源统计
  let resource_stats = TelemetryResourceManager::get_stats(resource_manager)
  assert_eq(ResourceStats::registered_count(resource_stats), 2)
  assert_eq(ResourceStats::active_count(resource_stats), 2)
  
  // 释放资源
  TelemetryResourceManager::release(resource_manager, "tracer")
  let updated_stats = TelemetryResourceManager::get_stats(resource_manager)
  assert_eq(ResourceStats::active_count(updated_stats), 1)
  
  // 测试自动清理
  TelemetryResourceManager::register(resource_manager, "temp_resource", fn() {
    "temp_value"
  }, fn(resource) {
    // 清理逻辑
  })
  
  // 模拟资源过期
  TelemetryResourceManager::cleanup_expired(resource_manager, 0)  // 立即清理所有资源
  let final_stats = TelemetryResourceManager::get_stats(resource_manager)
  assert_eq(ResourceStats::active_count(final_stats), 1)  // 只剩下meter
}

// 测试7: 策略模式和可配置行为
test "策略模式和可配置行为测试" {
  // 创建采样策略管理器
  let sampling_manager = SamplingStrategyManager::new()
  
  // 注册不同采样策略
  SamplingStrategyManager::register_strategy(sampling_manager, "always_on", AlwaysOnSamplingStrategy::new())
  SamplingStrategyManager::register_strategy(sampling_manager, "always_off", AlwaysOffSamplingStrategy::new())
  SamplingStrategyManager::register_strategy(sampling_manager, "probability", ProbabilitySamplingStrategy::new(0.1))
  SamplingStrategyManager::register_strategy(sampling_manager, "rate_limiting", RateLimitingSamplingStrategy::new(100))
  
  // 测试始终开启策略
  SamplingStrategyManager::set_active_strategy(sampling_manager, "always_on")
  let always_on_result = SamplingStrategyManager::should_sample(sampling_manager, "trace-123", "span-456")
  assert_true(always_on_result)
  
  // 测试始终关闭策略
  SamplingStrategyManager::set_active_strategy(sampling_manager, "always_off")
  let always_off_result = SamplingStrategyManager::should_sample(sampling_manager, "trace-123", "span-456")
  assert_false(always_off_result)
  
  // 测试概率策略
  SamplingStrategyManager::set_active_strategy(sampling_manager, "probability")
  let probability_results = []
  for i in 0..=1000 {
    let result = SamplingStrategyManager::should_sample(sampling_manager, "trace-" + i.to_string(), "span-" + i.to_string())
    probability_results = probability_results.push(result)
  }
  
  // 验证概率采样大约10%的采样率
  let sampled_count = probability_results.filter(fn(r) { r }).length()
  let sampling_rate = sampled_count.to_float() / probability_results.length().to_float()
  assert_gt(sampling_rate, 0.05)  // 至少5%
  assert_lt(sampling_rate, 0.15)  // 不超过15%
  
  // 测试限流策略
  SamplingStrategyManager::set_active_strategy(sampling_manager, "rate_limiting")
  let rate_limiting_results = []
  for i in 0..=150 {
    let result = SamplingStrategyManager::should_sample(sampling_manager, "trace-" + i.to_string(), "span-" + i.to_string())
    rate_limiting_results = rate_limiting_results.push(result)
  }
  
  // 验证限流策略最多采样100个
  let rate_limited_count = rate_limiting_results.filter(fn(r) { r }).length()
  assert_le(rate_limited_count, 100)
}

// 测试8: 装饰器模式和功能增强
test "装饰器模式和功能增强测试" {
  // 创建基础遥测处理器
  let base_processor = BaseTelemetryProcessor::new()
  
  // 创建装饰器
  let retry_decorator = RetryDecorator::new(3, 100)  // 最多重试3次，间隔100ms
  let cache_decorator = CacheDecorator::new(50)     // 缓存50个结果
  let metrics_decorator = MetricsDecorator::new()   // 添加指标收集
  
  // 装饰基础处理器
  let decorated_processor = MetricsDecorator::decorate(
    metrics_decorator,
    CacheDecorator::decorate(
      cache_decorator,
      RetryDecorator::decorate(retry_decorator, base_processor)
    )
  )
  
  // 创建测试数据
  let test_span = SpanData::new("decorated.span", "trace-789", "span-123")
  
  // 测试首次处理
  let result1 = TelemetryProcessor::process(decorated_processor, test_span)
  assert_true(result1)
  
  // 测试缓存命中
  let result2 = TelemetryProcessor::process(decorated_processor, test_span)
  assert_true(result2)
  
  // 验证缓存统计
  let cache_stats = CacheDecorator::get_stats(cache_decorator)
  assert_eq(CacheStats::hits(cache_stats), 1)
  assert_eq(CacheStats::misses(cache_stats), 1)
  
  // 验证指标统计
  let metrics_stats = MetricsDecorator::get_stats(metrics_decorator)
  assert_eq(MetricsStats::process_count(metrics_stats), 2)
  assert_eq(MetricsStats::success_count(metrics_stats), 2)
  assert_eq(MetricsStats::cache_hit_count(metrics_stats), 1)
  
  // 测试错误处理和重试
  let failing_processor = FailingTelemetryProcessor::new(2)  // 前2次失败
  let retry_decorated_failing = RetryDecorator::decorate(retry_decorator, failing_processor)
  
  let retry_result = TelemetryProcessor::process(retry_decorated_failing, test_span)
  assert_true(retry_result)  // 应该在第3次尝试成功
  
  // 验证重试统计
  let retry_stats = RetryDecorator::get_stats(retry_decorator)
  assert_eq(RetryStats::total_attempts(retry_stats), 3)
  assert_eq(RetryStats::successful_retries(retry_stats), 1)
}