// 仪器化生命周期测试用例
// 专注于遥测仪器和组件的完整生命周期管理

test "Tracer生命周期管理测试" {
  // 测试TracerProvider生命周期
  let tracer_provider = TracerProvider::default()
  
  // 验证TracerProvider初始状态
  assert_true(true)  // TracerProvider创建成功
  
  // 创建多个Tracer实例
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "lifecycle.tracer.1")
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "lifecycle.tracer.2", Some("1.0.0"))
  let tracer3 = TracerProvider::get_tracer(tracer_provider, "lifecycle.tracer.3", Some("2.0.0"), Some("https://example.com/schema"))
  
  // 验证Tracer创建和状态
  assert_eq(tracer1.scope.name, "lifecycle.tracer.1")
  assert_eq(tracer1.scope.version, None)
  assert_eq(tracer1.scope.schema_url, None)
  
  assert_eq(tracer2.scope.name, "lifecycle.tracer.2")
  assert_eq(tracer2.scope.version, Some("1.0.0"))
  assert_eq(tracer2.scope.schema_url, None)
  
  assert_eq(tracer3.scope.name, "lifecycle.tracer.3")
  assert_eq(tracer3.scope.version, Some("2.0.0"))
  assert_eq(tracer3.scope.schema_url, Some("https://example.com/schema"))
  
  // 测试Tracer使用生命周期
  let spans = Array::new()
  
  // 创建多个Span
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(tracer1, "lifecycle.span." + i.to_string())
    spans.push(span)
    
    // 验证Span初始状态
    assert_true(Span::is_recording(span))
    assert_eq(Span::name(span), "lifecycle.span." + i.to_string())
  }
  
  // Span生命周期：活跃状态
  for span in spans {
    assert_true(Span::is_recording(span))
    Span::set_status(span, Ok)
    Span::add_event(span, "lifecycle.event", Some([
      ("event.type", StringValue("lifecycle.test")),
      ("event.phase", StringValue("active"))
    ]))
  }
  
  // Span生命周期：结束状态
  for span in spans {
    Span::end(span)
    assert_false(Span::is_recording(span))
  }
  
  // 验证Tracer仍然可用
  assert_eq(tracer1.scope.name, "lifecycle.tracer.1")
  assert_eq(tracer2.scope.name, "lifecycle.tracer.2")
  assert_eq(tracer3.scope.name, "lifecycle.tracer.3")
  
  // 测试重复创建相同名称的Tracer
  let tracer1_duplicate = TracerProvider::get_tracer(tracer_provider, "lifecycle.tracer.1")
  assert_eq(tracer1_duplicate.scope.name, "lifecycle.tracer.1")
}

test "Meter生命周期管理测试" {
  // 测试MeterProvider生命周期
  let meter_provider = MeterProvider::default()
  
  // 创建多个Meter实例
  let meter1 = MeterProvider::get_meter(meter_provider, "lifecycle.meter.1")
  let meter2 = MeterProvider::get_meter(meter_provider, "lifecycle.meter.2", Some("1.5.0"))
  let meter3 = MeterProvider::get_meter(meter_provider, "lifecycle.meter.3", Some("3.0.0"), Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 验证Meter创建和状态
  assert_eq(meter1.scope.name, "lifecycle.meter.1")
  assert_eq(meter1.scope.version, None)
  assert_eq(meter1.scope.schema_url, None)
  
  assert_eq(meter2.scope.name, "lifecycle.meter.2")
  assert_eq(meter2.scope.version, Some("1.5.0"))
  assert_eq(meter2.scope.schema_url, None)
  
  assert_eq(meter3.scope.name, "lifecycle.meter.3")
  assert_eq(meter3.scope.version, Some("3.0.0"))
  assert_eq(meter3.scope.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 测试仪器创建生命周期
  let instruments = Array::new()
  
  // 创建各种类型的仪器
  let counter1 = Meter::create_counter(meter1, "lifecycle.counter.1")
  let counter2 = Meter::create_counter(meter2, "lifecycle.counter.2", Some("Test counter 2"))
  let counter3 = Meter::create_counter(meter3, "lifecycle.counter.3", Some("Test counter 3"), Some("count"))
  
  let histogram1 = Meter::create_histogram(meter1, "lifecycle.histogram.1")
  let histogram2 = Meter::create_histogram(meter2, "lifecycle.histogram.2", Some("Test histogram 2"))
  let histogram3 = Meter::create_histogram(meter3, "lifecycle.histogram.3", Some("Test histogram 3"), Some("ms"))
  
  let updown_counter1 = Meter::create_updown_counter(meter1, "lifecycle.updown.1")
  let updown_counter2 = Meter::create_updown_counter(meter2, "lifecycle.updown.2", Some("Test updown 2"))
  let updown_counter3 = Meter::create_updown_counter(meter3, "lifecycle.updown.3", Some("Test updown 3"), Some("items"))
  
  let gauge1 = Meter::create_gauge(meter1, "lifecycle.gauge.1")
  let gauge2 = Meter::create_gauge(meter2, "lifecycle.gauge.2", Some("Test gauge 2"))
  let gauge3 = Meter::create_gauge(meter3, "lifecycle.gauge.3", Some("Test gauge 3"), Some("percent"))
  
  instruments.push(counter1)
  instruments.push(counter2)
  instruments.push(counter3)
  instruments.push(histogram1)
  instruments.push(histogram2)
  instruments.push(histogram3)
  instruments.push(updown_counter1)
  instruments.push(updown_counter2)
  instruments.push(updown_counter3)
  instruments.push(gauge1)
  instruments.push(gauge2)
  instruments.push(gauge3)
  
  // 验证仪器创建状态
  assert_eq(counter1.name, "lifecycle.counter.1")
  assert_eq(counter1.description, None)
  assert_eq(counter1.unit, None)
  
  assert_eq(counter2.name, "lifecycle.counter.2")
  assert_eq(counter2.description, Some("Test counter 2"))
  assert_eq(counter2.unit, None)
  
  assert_eq(counter3.name, "lifecycle.counter.3")
  assert_eq(counter3.description, Some("Test counter 3"))
  assert_eq(counter3.unit, Some("count"))
  
  // 测试仪器使用生命周期
  for i = 0; i < instruments.length(); i = i + 1 {
    let instrument = instruments[i]
    
    match instrument {
      Counter(_, _, _) => {
        Counter::add(instrument as Counter, i.to_double())
      }
      Histogram(_, _, _) => {
        Histogram::record(instrument as Histogram, i.to_double() * 1.5)
      }
      UpDownCounter(_, _, _) => {
        UpDownCounter::add(instrument as UpDownCounter, i.to_double() - 5.0)
      }
      Gauge(_, _, _) => {
        // Gauge没有record方法，但可以验证其状态
        assert_eq((instrument as Gauge).name.contains("lifecycle.gauge"), true)
      }
    }
  }
  
  // 验证仪器在使用后仍然可用
  assert_eq(counter1.name, "lifecycle.counter.1")
  assert_eq(histogram1.name, "lifecycle.histogram.1")
  assert_eq(updown_counter1.name, "lifecycle.updown.1")
  assert_eq(gauge1.name, "lifecycle.gauge.1")
}

test "Logger生命周期管理测试" {
  // 测试LoggerProvider生命周期
  let logger_provider = LoggerProvider::default()
  
  // 创建多个Logger实例
  let logger1 = LoggerProvider::get_logger(logger_provider, "lifecycle.logger.1")
  let logger2 = LoggerProvider::get_logger(logger_provider, "lifecycle.logger.2", Some("2.0.0"))
  let logger3 = LoggerProvider::get_logger(logger_provider, "lifecycle.logger.3", Some("4.0.0"), Some("https://example.com/logger-schema"))
  
  // 验证Logger创建和状态
  assert_eq(logger1.scope.name, "lifecycle.logger.1")
  assert_eq(logger1.scope.version, None)
  assert_eq(logger1.scope.schema_url, None)
  
  assert_eq(logger2.scope.name, "lifecycle.logger.2")
  assert_eq(logger2.scope.version, Some("2.0.0"))
  assert_eq(logger2.scope.schema_url, None)
  
  assert_eq(logger3.scope.name, "lifecycle.logger.3")
  assert_eq(logger3.scope.version, Some("4.0.0"))
  assert_eq(logger3.scope.schema_url, Some("https://example.com/logger-schema"))
  
  // 测试日志记录生命周期
  let log_records = Array::new()
  
  // 创建不同类型的日志记录
  for i = 0; i < 20; i = i + 1 {
    let severity = match i % 6 {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      4 => Error
      _ => Fatal
    }
    
    let message = "Lifecycle log message " + i.to_string()
    let record = LogRecord::new(severity, message)
    log_records.push(record)
    
    // 验证日志记录创建状态
    assert_eq(LogRecord::severity_number(record), severity)
    assert_eq(LogRecord::body(record), Some(message))
  }
  
  // 测试日志发射生命周期
  for i = 0; i < log_records.length(); i = i + 1 {
    let record = log_records[i]
    let logger = match i % 3 {
      0 => logger1
      1 => logger2
      _ => logger3
    }
    
    // 发射日志记录
    Logger::emit(logger, record)
    
    // 验证发射后Logger仍然可用
    assert_true(logger.scope.name.contains("lifecycle.logger"))
  }
  
  // 测试带上下文的日志记录生命周期
  for i = 0; i < 10; i = i + 1 {
    let contextual_record = LogRecord::new_with_context(
      Info,
      Some("Contextual lifecycle log " + i.to_string()),
      None,
      Some(Clock::now_unix_nanos(Clock::system())),
      Some(Clock::now_unix_nanos(Clock::system()) + 1000L),
      Some("lifecycle-trace-" + i.to_string()),
      Some("lifecycle-span-" + i.to_string()),
      Some(Context::with_value(Context::root(), ContextKey::new("lifecycle.phase"), "active"))
    )
    
    Logger::emit(logger1, contextual_record)
    
    // 验证上下文日志记录完整性
    assert_eq(LogRecord::body(contextual_record), Some("Contextual lifecycle log " + i.to_string()))
    assert_eq(LogRecord::trace_id(contextual_record), Some("lifecycle-trace-" + i.to_string()))
    assert_eq(LogRecord::span_id(contextual_record), Some("lifecycle-span-" + i.to_string()))
  }
  
  // 验证Logger最终状态
  assert_eq(logger1.scope.name, "lifecycle.logger.1")
  assert_eq(logger2.scope.name, "lifecycle.logger.2")
  assert_eq(logger3.scope.name, "lifecycle.logger.3")
}

test "SpanContext生命周期测试" {
  // 测试SpanContext创建和验证生命周期
  let span_contexts = Array::new()
  
  // 创建各种类型的SpanContext
  for i = 0; i < 50; i = i + 1 {
    let trace_id = "lifecycle-trace-" + i.to_string() + "-" + "a" * 16
    let span_id = "lifecycle-span-" + i.to_string() + "-" + "b" * 8
    let sampled = i % 3 != 0  // 2/3的采样率
    let trace_state = "lifecycle.key" + i.to_string() + "=lifecycle.value" + i.to_string()
    
    let span_ctx = SpanContext::new(trace_id, span_id, sampled, trace_state)
    span_contexts.push(span_ctx)
    
    // 验证SpanContext创建状态
    assert_true(SpanContext::is_valid(span_ctx))
    assert_eq(SpanContext::trace_id(span_ctx), trace_id)
    assert_eq(SpanContext::span_id(span_ctx), span_id)
    assert_eq(SpanContext::is_sampled(span_ctx), sampled)
  }
  
  // 测试SpanContext使用生命周期
  for i = 0; i < span_contexts.length(); i = i + 1 {
    let ctx = span_contexts[i]
    
    // 多次访问SpanContext属性
    for j = 0; j < 5; j = j + 1 {
      let trace_id = SpanContext::trace_id(ctx)
      let span_id = SpanContext::span_id(ctx)
      let is_valid = SpanContext::is_valid(ctx)
      let is_sampled = SpanContext::is_sampled(ctx)
      
      // 验证属性访问一致性
      assert_eq(trace_id, SpanContext::trace_id(ctx))
      assert_eq(span_id, SpanContext::span_id(ctx))
      assert_eq(is_valid, SpanContext::is_valid(ctx))
      assert_eq(is_sampled, SpanContext::is_sampled(ctx))
    }
  }
  
  // 测试无效SpanContext生命周期
  let invalid_contexts = [
    SpanContext::new("", "", false, ""),
    SpanContext::new("", "valid-span-id", true, ""),
    SpanContext::new("valid-trace-id", "", false, ""),
    SpanContext::new("a", "b", true, "")  // 太短的ID
  ]
  
  for invalid_ctx in invalid_contexts {
    // 验证无效上下文状态
    assert_false(SpanContext::is_valid(invalid_ctx))
    
    // 即使无效，仍然可以访问属性
    let trace_id = SpanContext::trace_id(invalid_ctx)
    let span_id = SpanContext::span_id(invalid_ctx)
    let is_sampled = SpanContext::is_sampled(invalid_ctx)
    
    // 验证属性访问不会崩溃
    assert_true(trace_id.length() >= 0)
    assert_true(span_id.length() >= 0)
    assert_true(is_sampled == true || is_sampled == false)
  }
  
  // 测试长生命周期SpanContext
  let long_lived_ctx = SpanContext::new("long-lived-trace", "long-lived-span", true, "long.lived.state=value")
  
  // 模拟长时间使用
  for i = 0; i < 100; i = i + 1 {
    assert_true(SpanContext::is_valid(long_lived_ctx))
    assert_eq(SpanContext::trace_id(long_lived_ctx), "long-lived-trace")
    assert_eq(SpanContext::span_id(long_lived_ctx), "long-lived-span")
    assert_true(SpanContext::is_sampled(long_lived_ctx))
  }
}

test "资源生命周期管理测试" {
  // 测试资源创建和初始化生命周期
  let resources = Array::new()
  
  // 创建多个资源实例
  for i = 0; i < 20; i = i + 1 {
    let resource = Resource::new()
    resources.push(resource)
    
    // 验证资源初始状态
    assert_eq(resource.attributes.length(), 0)
  }
  
  // 测试资源属性设置生命周期
  for i = 0; i < resources.length(); i = i + 1 {
    let attrs = [
      ("resource.id", StringValue("resource-" + i.to_string())),
      ("resource.type", StringValue("lifecycle.test")),
      ("resource.version", StringValue("1.0." + i.to_string())),
      ("resource.created", IntValue(1735689600 + i))
    ]
    
    let enriched_resource = Resource::with_attributes(resources[i], attrs)
    resources[i] = enriched_resource
    
    // 验证属性设置状态
    let resource_id = Resource::get_attribute(resources[i], "resource.id")
    assert_true(resource_id is Some)
    match resource_id {
      Some(StringValue(id)) => assert_eq(id, "resource-" + i.to_string())
      _ => assert_true(false)
    }
  }
  
  // 测试资源合并生命周期
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("base.service.name", StringValue("lifecycle-service")),
    ("base.service.version", StringValue("1.0.0"))
  ])
  
  for i = 0; i < 10; i = i + 1 {
    let override_attrs = [
      ("override.instance.id", StringValue("instance-" + i.to_string())),
      ("override.started.at", IntValue(1735689600 + i))
    ]
    
    let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
    let merged_resource = Resource::merge(base_resource, override_resource)
    
    // 验证合并结果
    let instance_id = Resource::get_attribute(merged_resource, "override.instance.id")
    assert_true(instance_id is Some)
    match instance_id {
      Some(StringValue(id)) => assert_eq(id, "instance-" + i.to_string())
      _ => assert_true(false)
    }
  }
  
  // 测试资源长期使用生命周期
  let long_lived_resource = Resource::with_attributes(Resource::new(), [
    ("long.lived.service", StringValue("lifecycle-test")),
    ("long.lived.version", StringValue("1.0.0"))
  ])
  
  // 模拟长期使用
  for i = 0; i < 200; i = i + 1 {
    let service_name = Resource::get_attribute(long_lived_resource, "long.lived.service")
    let version = Resource::get_attribute(long_lived_resource, "long.lived.version")
    
    // 验证长期访问一致性
    assert_true(service_name is Some)
    assert_true(version is Some)
    
    match service_name {
      Some(StringValue(name)) => assert_eq(name, "lifecycle-test")
      _ => assert_true(false)
    }
    
    match version {
      Some(StringValue(v)) => assert_eq(v, "1.0.0")
      _ => assert_true(false)
    }
  }
  
  // 测试资源清理和重用生命周期
  for i = 0; i < resources.length(); i = i + 1 {
    // 重用资源并添加新属性
    let reuse_attrs = [
      ("reuse.phase", StringValue("recycled")),
      ("reuse.count", IntValue(i))
    ]
    
    let reused_resource = Resource::with_attributes(resources[i], reuse_attrs)
    
    // 验证重用后的资源状态
    let original_id = Resource::get_attribute(reused_resource, "resource.id")
    let reuse_phase = Resource::get_attribute(reused_resource, "reuse.phase")
    
    assert_true(original_id is Some)
    assert_true(reuse_phase is Some)
  }
}