// Azimuth 高级功能测试 - 第一部分
// 包含WebAssembly兼容性、实时流处理和安全性测试

// 测试1: WebAssembly环境基础遥测功能
test "WebAssembly环境基础遥测功能测试" {
  // 模拟WASM环境的内存限制
  let wasm_memory_limit = 1024 * 1024  // 1MB
  
  // 创建轻量级资源对象，适应WASM环境
  let resource = @azimuth.telemetry.Resource.create({
    "service.name": "wasm-test-service",
    "service.version": "1.0.0",
    "telemetry.sdk.name": "azimuth",
    "telemetry.sdk.version": "0.1.0",
    "wasm.target": "wasm-gc"
  })
  
  // 验证资源创建成功
  assert_true(@azimuth.telemetry.Resource.is_valid(resource))
  
  // 创建适合WASM环境的轻量级Span
  let span = @azimuth.telemetry.SpanBuilder.create("wasm-operation")
    .set_resource(resource)
    .set_attribute("wasm.memory.limit", wasm_memory_limit)
    .start_span()
  
  // 验证Span创建成功
  assert_true(@azimuth.telemetry.Span.is_valid(span))
  
  // 结束Span
  span.end()
  
  // 验证Span已结束
  assert_true(@azimuth.telemetry.Span.is_ended(span))
}

// 测试2: 浏览器环境遥测兼容性
test "浏览器环境遥测兼容性测试" {
  // 模拟浏览器环境的全局对象
  let browser_context = @azimuth.platform.browser.create_context()
  
  // 创建适合浏览器环境的遥测提供者
  let provider = @azimuth.telemetry.TelemetryProvider.create_for_browser({
    "browser.name": "chrome",
    "browser.version": "120.0.0",
    "user.agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  })
  
  // 验证提供者创建成功
  assert_true(@azimuth.telemetry.TelemetryProvider.is_valid(provider))
  
  // 创建浏览器特定的度量工具
  let counter = @azimuth.telemetry.Counter.create("browser.page.views")
    .set_description("页面浏览次数")
    .set_unit("count")
  
  // 记录度量
  counter.record(1, {"page": "home", "referrer": "direct"})
  
  // 验证度量记录成功
  let metrics = counter.collect()
  assert_true(metrics.length() > 0)
  assert_eq(metrics[0].value, 1L)
  assert_eq(metrics[0].attributes["page"], "home")
}

// 测试3: 实时度量流处理
test "实时度量流处理测试" {
  // 创建实时流处理器
  let stream_processor = @azimuth.stream.RealtimeProcessor.create({
    "buffer.size": 10000,
    "batch.size": 100,
    "flush.interval.ms": 50
  })
  
  // 验证处理器创建成功
  assert_true(@azimuth.stream.RealtimeProcessor.is_valid(stream_processor))
  
  // 创建度量流
  let metrics_stream = stream_processor.create_metrics_stream("realtime.metrics")
  
  // 模拟实时度量数据流
  for i = 0; i < 1000; i = i + 1 {
    let timestamp = @azimuth.time.now()
    let metric_data = {
      "name": "cpu.usage",
      "value": @azimuth.math.random() * 100.0,
      "timestamp": timestamp,
      "attributes": {
        "host": "server-" + (@azimuth.math.random_int(5) |> @azimuth.string.from_int),
        "region": ["us-east-1", "us-west-2", "eu-west-1"][@azimuth.math.random_int(3)]
      }
    }
    
    // 添加到流中
    metrics_stream.add(metric_data)
  }
  
  // 启动流处理
  stream_processor.start()
  
  // 等待处理完成
  @azimuth.time.sleep(100)  // 100ms
  
  // 获取处理结果
  let results = metrics_stream.get_aggregated_results()
  
  // 验证处理结果
  assert_true(results.length() > 0)
  assert_true(results[0].count > 0)
  assert_true(results[0].avg >= 0.0)
  assert_true(results[0].avg <= 100.0)
  
  // 停止处理器
  stream_processor.stop()
}

// 测试4: 敏感数据加密和脱敏
test "敏感数据加密和脱敏测试" {
  // 创建安全管理器
  let security_manager = @azimuth.security.SecurityManager.create({
    "encryption.key.source": "environment",
    "encryption.algorithm": "AES-256-GCM",
    "data.masking.enabled": true
  })
  
  // 验证安全管理器创建成功
  assert_true(@azimuth.security.SecurityManager.is_valid(security_manager))
  
  // 创建包含敏感数据的遥测数据
  let sensitive_data = {
    "user.id": "12345678-1234-1234-1234-123456789012",
    "user.email": "user@example.com",
    "user.phone": "+1-555-123-4567",
    "credit.card": "4111-1111-1111-1111",
    "ssn": "123-45-6789",
    "api.key": "sk-1234567890abcdef",
    "password": "SuperSecretPassword123!",
    "public.info": "This is public information"
  }
  
  // 应用数据脱敏
  let masked_data = security_manager.mask_sensitive_attributes(sensitive_data)
  
  // 验证敏感数据已被脱敏
  assert_eq(masked_data["user.id"], "****-****-****-****-************")
  assert_eq(masked_data["user.email"], "****@example.com")
  assert_eq(masked_data["user.phone"], "+*-***-***-****")
  assert_eq(masked_data["credit.card"], "****-****-****-1111")
  assert_eq(masked_data["ssn"], "***-**-****")
  assert_eq(masked_data["api.key"], "sk-****************")
  assert_eq(masked_data["password"], "********************")
  
  // 验证公开信息未被修改
  assert_eq(masked_data["public.info"], "This is public information")
}

// 测试5: 安全的上下文传播
test "安全的上下文传播测试" {
  // 创建安全传播器
  let secure_propagator = @azimuth.security.SecurePropagator.create({
    "signing.enabled": true,
    "encryption.enabled": true,
    "signature.algorithm": "HMAC-SHA256",
    "key.rotation.enabled": true
  })
  
  // 验证传播器创建成功
  assert_true(@azimuth.security.SecurePropagator.is_valid(secure_propagator))
  
  // 创建原始上下文
  let original_context = @azimuth.telemetry.Context.create()
    .set_trace_id(@azimuth.trace.generate_id())
    .set_span_id(@azimuth.trace.generate_span_id())
    .set_attribute("user.id", "secure-user-123")
    .set_attribute("security.level", "high")
  
  // 注射到载体
  let carrier = {}
  let inject_result = secure_propagator.inject(original_context, carrier)
  
  // 验证注射成功
  assert_true(inject_result.success)
  assert_true(carrier.has_key("traceparent"))
  assert_true(carrier.has_key("tracestate"))
  assert_true(carrier.has_key("azimuth-signature"))
  assert_true(carrier.has_key("azimuth-encrypted"))
  
  // 从载体提取上下文
  let extract_result = secure_propagator.extract(carrier)
  
  // 验证提取成功
  assert_true(extract_result.success)
  assert_true(extract_result.verified)  // 签名验证成功
  assert_eq(extract_result.context.trace_id, original_context.trace_id)
  assert_eq(extract_result.context.span_id, original_context.span_id)
  assert_eq(extract_result.context.attributes["user.id"], "secure-user-123")
  assert_eq(extract_result.context.attributes["security.level"], "high")
}