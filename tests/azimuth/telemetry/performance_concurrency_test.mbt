// 性能和并发相关测试用例
// 测试系统在高负载、大量数据和并发场景下的性能表现

test "大量属性创建和访问性能测试" {
  // 测试创建大量属性
  let large_attrs = Attributes::new()
  
  // 创建1000个属性
  for i = 0; i < 1000; i = i + 1 {
    let key = "performance.test.key.${i}"
    let value = StringValue("value_${i}_with_中文和特殊字符!@#$%")
    Attributes::set(large_attrs, key, value)
  }
  
  // 测试属性访问性能
  for i = 0; i < 1000; i = i + 1 {
    let key = "performance.test.key.${i}"
    let retrieved = Attributes::get(large_attrs, key)
    assert_true(retrieved.is_some())
  }
  
  // 测试不存在属性的访问性能
  for i = 1000; i < 1100; i = i + 1 {
    let key = "non.existent.key.${i}"
    let retrieved = Attributes::get(large_attrs, key)
    assert_eq(retrieved, None)
  }
}

test "大量Span创建和管理性能测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance_test", Some("1.0.0"))
  
  // 创建大量Span
  let spans = Array[Span]()
  for i = 0; i < 100; i = i + 1 {
    let span_name = "performance_span_${i}_中文"
    let span = Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  // 批量设置Span状态
  for i = 0; i < spans.length(); i = i + 1 {
    Span::set_status(spans[i], Ok, Some("Span ${i} 操作成功"))
  }
  
  // 批量添加事件
  for i = 0; i < spans.length(); i = i + 1 {
    Span::add_event(spans[i], "事件_${i}", Some([
      ("event.index", IntValue(i)),
      ("event.message", StringValue("这是第${i}个事件")),
      ("event.timestamp", IntValue(1735689600 + i))
    ]))
  }
  
  // 批量结束Span
  for i = 0; i < spans.length(); i = i + 1 {
    Span::end(spans[i])
  }
}

test "高频指标记录性能测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance_metrics")
  
  // 创建多种指标
  let counter = Meter::create_counter(meter, "performance_counter")
  let histogram = Meter::create_histogram(meter, "performance_histogram", Some("性能测试直方图"), Some("ms"))
  let updown_counter = Meter::create_counter(meter, "performance_updown")
  
  // 高频记录指标数据
  for i = 0; i < 10000; i = i + 1 {
    // Counter指标
    Counter::add(counter, 1.0, Some(Attributes::new()))
    
    // Histogram指标
    Histogram::record(histogram, (i % 1000).to_double(), None)
    
    // UpDownCounter指标
    if i % 2 == 0 {
      Counter::add(updown_counter, 1.0, None)
    } else {
      Counter::add(updown_counter, -1.0, None)
    }
  }
}

test "大量日志记录性能测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance_logger")
  
  // 创建不同严重级别的日志记录
  let severity_levels = [Trace, Debug, Info, Warn, Error, Fatal]
  let log_messages = [
    "系统启动完成",
    "用户登录成功",
    "数据库连接建立",
    "缓存初始化完成",
    "API请求处理中",
    "性能监控启动",
    "配置加载完成",
    "服务注册成功"
  ]
  
  // 批量创建日志记录
  for i = 0; i < 5000; i = i + 1 {
    let severity = severity_levels[i % severity_levels.length()]
    let message = log_messages[i % log_messages.length()] + " - 批次 ${i}"
    let log_record = LogRecord::new(severity, message)
    
    // 添加复杂属性
    let attrs = Attributes::new()
    Attributes::set(attrs, "log.index", IntValue(i))
    Attributes::set(attrs, "log.batch", StringValue("batch_${i / 100}"))
    Attributes::set(attrs, "log.thread", StringValue("thread_${i % 10}"))
    
    let complex_log = LogRecord::new_with_context(
      severity,
      Some(message),
      Some(attrs),
      Some((1735689600000000000L + (i * 1000000L))),
      None,
      Some("trace_${i}"),
      Some("span_${i}"),
      None
    )
    
    Logger::emit(logger, complex_log)
  }
}

test "大量上下文操作性能测试" {
  let base_ctx = Context::root()
  
  // 创建大量上下文键值对
  let keys = Array[ContextKey[String]]()
  for i = 0; i < 1000; i = i + 1 {
    let key = ContextKey::new("performance.context.key.${i}")
    keys.push(key)
  }
  
  // 链式设置上下文值
  let current_ctx = base_ctx
  for i = 0; i < keys.length(); i = i + 1 {
    current_ctx = Context::with_value(current_ctx, keys[i], "value_${i}_with_中文")
  }
  
  // 批量访问上下文值
  for i = 0; i < keys.length(); i = i + 1 {
    let value = Context::get(current_ctx, keys[i])
    assert_true(value.is_some())
  }
  
  // 测试深层上下文链的访问性能
  for i = 0; i < 100; i = i + 1 {
    let random_key = keys[i % keys.length()]
    let value = Context::get(current_ctx, random_key)
    assert_true(value.is_some())
  }
}

test "大量Baggage操作性能测试" {
  let baggage = Baggage::new()
  
  // 添加大量Baggage条目
  let current_baggage = baggage
  for i = 0; i < 500; i = i + 1 {
    let key = "baggage.performance.key.${i}"
    let value = "baggage_value_${i}_包含中文和特殊字符!@#$%"
    current_baggage = Baggage::set_entry(current_baggage, key, value)
  }
  
  // 批量访问Baggage条目
  for i = 0; i < 500; i = i + 1 {
    let key = "baggage.performance.key.${i}"
    let value = Baggage::get_entry(current_baggage, key)
    assert_true(value.is_some())
  }
  
  // 测试Baggage条目删除性能
  for i = 0; i < 100; i = i + 1 {
    let key = "baggage.performance.key.${i}"
    current_baggage = Baggage::remove_entry(current_baggage, key)
  }
}

test "大量传播操作性能测试" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建大量传播测试
  for i = 0; i < 100; i = i + 1 {
    let carrier = TextMapCarrier::new()
    
    // 设置复杂的carrier数据
    TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    TextMapCarrier::set(carrier, "tracestate", "rojo=00-4bf92f3577ed34a170dc3b6f0f3b4d00-00f067aa0ba902b7-01")
    TextMapCarrier::set(carrier, "baggage", "key1=value1;key2=value2_with_中文;key3=value3_with_!@#$%")
    TextMapCarrier::set(carrier, "x-custom-header-${i}", "custom_value_${i}_with_特殊字符")
    
    // 执行注入操作
    let ctx = Context::with_value(Context::root(), ContextKey::new("test.key"), "test_value_${i}")
    CompositePropagator::inject(composite_propagator, ctx, carrier)
    
    // 执行提取操作
    let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
    let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
    assert_true(extracted_value.is_some())
  }
}

test "资源合并操作性能测试" {
  // 创建大型资源
  let base_attrs = Array[(String, AttributeValue)]()
  for i = 0; i < 500; i = i + 1 {
    base_attrs.push(("base.resource.attr.${i}", StringValue("base_value_${i}_中文")))
  }
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // 创建覆盖资源
  let override_attrs = Array[(String, AttributeValue)]()
  for i = 0; i < 300; i = i + 1 {
    override_attrs.push(("override.resource.attr.${i}", StringValue("override_value_${i}_特殊字符")))
  }
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // 执行多次资源合并操作
  for i = 0; i < 50; i = i + 1 {
    let merged_resource = Resource::merge(base_resource, override_resource)
    
    // 验证合并结果
    let merged_attr = Resource::get_attribute(merged_resource, "override.resource.attr.0")
    assert_true(merged_attr.is_some())
  }
}

test "随机数生成性能测试" {
  let random = Random::system()
  
  // 大量随机数生成
  let random_values = Array[UInt64]()
  for i = 0; i < 10000; i = i + 1 {
    let value = Random::next_u64(random)
    random_values.push(value)
  }
  
  // 大量随机字节生成
  for i = 0; i < 1000; i = i + 1 {
    let bytes = Random::next_bytes(random, 1024)  // 1KB随机数据
    assert_true(bytes.length() <= 1024)
  }
}

test "时间戳生成性能测试" {
  let clock = Clock::system()
  
  // 大量时间戳生成
  let timestamps = Array[Int64]()
  for i = 0; i < 10000; i = i + 1 {
    let timestamp = Clock::now_unix_nanos(clock)
    timestamps.push(timestamp)
  }
  
  // 验证时间戳的递增性（在合理范围内）
  for i = 1; i < timestamps.length(); i = i + 1 {
    assert_true(timestamps[i] >= timestamps[i-1])
  }
}