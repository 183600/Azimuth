// 并发和线程安全测试
// Concurrency and thread safety tests for Azimuth telemetry system

test "多提供者并发访问测试" {
  // 测试多个提供者的并发访问（模拟）
  let tracer_providers = []
  let meter_providers = []
  let logger_providers = []
  
  // 创建多个提供者
  for i = 0; i < 3; i = i + 1 {
    let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
    let meter_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
    let logger_provider = @azimuth.telemetry.sdk.logs.LoggerProvider::default()
    
    tracer_providers = Array::push(tracer_providers, tracer_provider)
    meter_providers = Array::push(meter_providers, meter_provider)
    logger_providers = Array::push(logger_providers, logger_provider)
  }
  
  // 模拟并发访问：交替使用不同的提供者
  for i = 0; i < 30; i = i + 1 {
    let provider_index = i % 3
    
    // 使用对应的提供者创建仪器
    let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(
      tracer_providers[provider_index], 
      "concurrent-tracer-" + provider_index.to_string()
    )
    let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(
      meter_providers[provider_index], 
      "concurrent-meter-" + provider_index.to_string()
    )
    let logger = @azimuth.telemetry.sdk.logs.LoggerProvider::get_logger(
      logger_providers[provider_index], 
      "concurrent-logger-" + provider_index.to_string()
    )
    
    // 创建和使用遥测对象
    let ctx = @azimuth.telemetry.api.context.Context::root()
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(tracer, ctx, "concurrent-span-" + i.to_string())
    let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "concurrent-counter")
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(@azimuth.telemetry.api.logs.Info, "Concurrent log " + i.to_string())
    
    @azimuth.telemetry.sdk.metrics.Counter::add(counter, 1.0)
    @azimuth.telemetry.sdk.logs.Logger::emit(logger, log_record)
    @azimuth.telemetry.api.trace.Span::end(span)
  }
}

test "上下文传播并发测试" {
  // 测试上下文传播的并发安全性
  let propagators = []
  let carriers = []
  
  // 创建多个传播器和载体
  for i = 0; i < 2; i = i + 1 {
    let propagator = @azimuth.telemetry.api.propagation.CompositePropagator::new([
      @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new()
    ])
    let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
    
    propagators = Array::push(propagators, propagator)
    carriers = Array::push(carriers, carrier)
  }
  
  // 模拟并发传播操作
  for i = 0; i < 20; i = i + 1 {
    let propagator_index = i % 2
    let propagator = propagators[propagator_index]
    let carrier = carriers[propagator_index]
    
    // 注入上下文
    let ctx = @azimuth.telemetry.api.context.Context::root()
    @azimuth.telemetry.api.propagation.CompositePropagator::inject(propagator, ctx, carrier)
    
    // 提取上下文
    let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(propagator, carrier)
    
    // 验证提取的上下文不为空
    let key = @azimuth.telemetry.api.context.ContextKey::new("extracted")
    let value = @azimuth.telemetry.api.context.Context::get(extracted_ctx, key)
    assert_eq(value, Some("true"))
  }
}

test "Span链式创建并发测试" {
  // 测试Span链式创建的并发安全性
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider::default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider::get_tracer(tracer_provider, "span-chain-test")
  
  let span_chains = []
  
  // 创建多个Span链
  for chain_id = 0; chain_id < 2; chain_id = chain_id + 1 {
    let chain = []
    let ctx = @azimuth.telemetry.api.context.Context::root()
    
    // 创建Span链
    for span_id = 0; span_id < 3; span_id = span_id + 1 {
      let (new_ctx, span) = @azimuth.telemetry.sdk.trace.Tracer::start_span(
        tracer, 
        ctx, 
        "chain-" + chain_id.to_string() + "-span-" + span_id.to_string()
      )
      chain = Array::push(chain, span)
      ctx = new_ctx
    }
    
    span_chains = Array::push(span_chains, chain)
  }
  
  // 并发结束所有Span链
  for chain in span_chains {
    for span in chain {
      @azimuth.telemetry.api.trace.Span::end(span)
    }
  }
  
  // 验证所有Span都有有效的上下文
  for chain in span_chains {
    for span in chain {
      let span_ctx = @azimuth.telemetry.api.trace.Span::span_context(span)
      assert_true(@azimuth.telemetry.api.trace.SpanContext::is_valid(span_ctx))
    }
  }
}