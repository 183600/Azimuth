// Azimuth 高级遥测测试用例
// 专注于遥测系统的核心功能、性能和可靠性

// 测试1: 分布式追踪上下文传播
test "分布式追踪上下文传播测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "distributed.tracing")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "root.operation")
  let root_context = Span::context(root_span)
  
  // 设置根span属性
  Span::set_attribute(root_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(root_span, "operation.type", StringValue("request.processing"))
  Span::set_attribute(root_span, "user.id", StringValue("user-12345"))
  
  // 创建子span1 - 数据库查询
  let db_span = Tracer::start_span_with_context(tracer, "database.query", root_context)
  Span::set_attribute(db_span, "db.system", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = $1"))
  Span::set_attribute(db_span, "db.connection_string", StringValue("postgresql://localhost:5432/mydb"))
  
  // 创建子span2 - 外部API调用
  let api_span = Tracer::start_span_with_context(tracer, "external.api.call", root_context)
  Span::set_attribute(api_span, "http.method", StringValue("POST"))
  Span::set_attribute(api_span, "http.url", StringValue("https://api.payment.com/charge"))
  Span::set_attribute(api_span, "http.status_code", IntValue(200))
  
  // 创建嵌套子span - 缓存操作
  let cache_span = Tracer::start_span_with_context(tracer, "cache.operation", db_context)
  Span::set_attribute(cache_span, "cache.system", StringValue("redis"))
  Span::set_attribute(cache_span, "cache.operation", StringValue("get"))
  Span::set_attribute(cache_span, "cache.key", StringValue("user:12345"))
  
  // 完成所有span
  Span::end(cache_span)
  Span::end(db_span)
  Span::end(api_span)
  Span::end(root_span)
  
  // 验证span状态
  assert_eq(Span::status(root_span), Ok)
  assert_eq(Span::status(db_span), Ok)
  assert_eq(Span::status(api_span), Ok)
  assert_eq(Span::status(cache_span), Ok)
  
  // 验证追踪ID一致性
  assert_eq(Span::trace_id(root_span), Span::trace_id(db_span))
  assert_eq(Span::trace_id(root_span), Span::trace_id(api_span))
  assert_eq(Span::trace_id(root_span), Span::trace_id(cache_span))
}

// 测试2: 指标聚合和统计
test "指标聚合和统计测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "metrics.aggregation")
  
  // 创建各种类型的指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_time = Meter::create_histogram(meter, "http.response.time.ms")
  let active_connections = Meter::create_gauge(meter, "http.active.connections")
  let error_rate = Meter::create_gauge(meter, "http.error.rate")
  
  // 模拟请求处理
  let mut total_requests = 0
  let mut error_count = 0
  let mut total_response_time = 0.0
  
  // 处理100个请求
  let mut i = 0
  while i < 100 {
    // 开始处理请求
    Gauge::increment(active_connections)
    
    // 模拟请求处理时间
    let processing_time = 10.0 + (Random::float() * 90.0) // 10-100ms
    Histogram::record(response_time, processing_time)
    total_response_time = total_response_time + processing_time
    
    // 模拟错误（10%概率）
    if Random::float() < 0.1 {
      error_count = error_count + 1
    }
    
    // 记录请求
    Counter::add(request_counter, 1.0)
    total_requests = total_requests + 1
    
    // 完成请求处理
    Gauge::decrement(active_connections)
    
    i = i + 1
  }
  
  // 计算错误率
  let current_error_rate = (error_count * 100) / total_requests
  Gauge::set(error_rate, FloatValue(current_error_rate))
  
  // 验证指标
  assert_eq(Counter::value(request_counter), 100.0)
  assert_eq(Gauge::value(active_connections), 0.0) // 所有连接应该已关闭
  assert_true(Gauge::value(error_rate) >= 0.0 && Gauge::value(error_rate) <= 100.0)
  
  // 验证直方图统计
  let histogram_stats = Histogram::statistics(response_time)
  assert_true(histogram_stats.count == 100)
  assert_true(histogram_stats.sum > 0.0)
  assert_true(histogram_stats.min >= 10.0)
  assert_true(histogram_stats.max <= 100.0)
}

// 测试3: 结构化日志记录
test "结构化日志记录测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "structured.logging")
  
  // 创建不同级别的日志记录器
  let debug_logger = Logger::with_level(logger, Debug)
  let info_logger = Logger::with_level(logger, Info)
  let warn_logger = Logger::with_level(logger, Warn)
  let error_logger = Logger::with_level(logger, Error)
  
  // 记录调试日志
  Logger::log(debug_logger, "用户会话开始", [
    ("session.id", StringValue("sess-abc123")),
    ("user.id", StringValue("user-456")),
    ("ip.address", StringValue("192.168.1.100")),
    ("user.agent", StringValue("Mozilla/5.0..."))
  ])
  
  // 记录信息日志
  Logger::log(info_logger, "订单处理完成", [
    ("order.id", StringValue("order-789")),
    ("order.amount", FloatValue(99.99)),
    ("payment.method", StringValue("credit_card")),
    ("processing.time.ms", IntValue(250))
  ])
  
  // 记录警告日志
  Logger::log(warn_logger, "数据库连接池接近上限", [
    ("pool.active", IntValue(95)),
    ("pool.max", IntValue(100)),
    ("pool.waiting", IntValue(5))
  ])
  
  // 记录错误日志
  Logger::log(error_logger, "支付处理失败", [
    ("error.code", StringValue("PAYMENT_DECLINED")),
    ("error.message", StringValue("信用卡被拒绝")),
    ("order.id", StringValue("order-789")),
    ("retry.count", IntValue(3))
  ])
  
  // 验证日志记录
  assert_eq(Logger::log_count(debug_logger), 1)
  assert_eq(Logger::log_count(info_logger), 1)
  assert_eq(Logger::log_count(warn_logger), 1)
  assert_eq(Logger::log_count(error_logger), 1)
  
  // 验证日志内容
  let debug_logs = Logger::get_logs(debug_logger)
  assert_true(debug_logs.length() == 1)
  assert_eq(debug_logs[0].message, "用户会话开始")
  assert_eq(debug_logs[0].level, Debug)
  
  let error_logs = Logger::get_logs(error_logger)
  assert_true(error_logs.length() == 1)
  assert_eq(error_logs[0].message, "支付处理失败")
  assert_eq(error_logs[0].level, Error)
}

// 测试4: 遥测数据采样策略
test "遥测数据采样策略测试" {
  let tracer_provider = TracerProvider::with_sampler(ProbabilitySampler::new(0.1)) // 10%采样率
  let tracer = TracerProvider::get_tracer(tracer_provider, "sampling.strategy")
  
  // 创建大量span来测试采样策略
  let mut total_spans = 0
  let mut sampled_spans = 0
  
  // 创建1000个span
  let mut i = 0
  while i < 1000 {
    let span = Tracer::start_span(tracer, "sampled.span." + i.to_string())
    Span::set_attribute(span, "span.index", IntValue(i))
    
    total_spans = total_spans + 1
    
    // 检查是否被采样
    if Span::is_sampled(span) {
      sampled_spans = sampled_spans + 1
      // 为采样的span添加更多详细信息
      Span::set_attribute(span, "detailed.info", StringValue("这个span被采样了"))
      Span::add_event(span, "采样事件", [
        ("event.type", StringValue("sampling")),
        ("timestamp", IntValue(Time::now()))
      ])
    }
    
    Span::end(span)
    i = i + 1
  }
  
  // 验证采样率（允许一定的误差范围）
  let actual_sample_rate = (sampled_spans * 100) / total_spans
  assert_true(actual_sample_rate >= 5 && actual_sample_rate <= 15) // 10% ± 5%
  
  // 测试不同采样策略
  let always_on_tracer = TracerProvider::with_sampler(AlwaysOnSampler::new())
  let always_on_span = Tracer::start_span(always_on_tracer, "always.on.span")
  assert_true(Span::is_sampled(always_on_span))
  Span::end(always_on_span)
  
  let always_off_tracer = TracerProvider::with_sampler(AlwaysOffSampler::new())
  let always_off_span = Tracer::start_span(always_off_tracer, "always.off.span")
  assert_false(Span::is_sampled(always_off_span))
  Span::end(always_off_span)
}

// 测试5: 遥测数据批处理和导出
test "遥测数据批处理和导出测试" {
  let exporter = BatchExporter::new()
  let tracer_provider = TracerProvider::with_exporter(exporter)
  let tracer = TracerProvider::get_tracer(tracer_provider, "batch.export")
  
  // 配置批处理参数
  BatchExporter::set_max_batch_size(exporter, 100)
  BatchExporter::set_max_export_timeout(exporter, 5000) // 5秒
  BatchExporter::set_max_queue_size(exporter, 1000)
  
  // 创建大量span
  let mut i = 0
  while i < 500 {
    let span = Tracer::start_span(tracer, "batch.test.span." + i.to_string())
    Span::set_attribute(span, "batch.index", IntValue(i))
    Span::set_attribute(span, "batch.group", StringValue("test.group"))
    
    // 添加一些事件
    if i % 10 == 0 {
      Span::add_event(span, "里程碑事件", [
        ("milestone", StringValue("重要节点")),
        ("progress", IntValue(i))
      ])
    }
    
    Span::end(span)
    i = i + 1
  }
  
  // 等待批处理完成
  Time::sleep(1000) // 等待1秒
  
  // 验证导出状态
  let export_stats = BatchExporter::get_statistics(exporter)
  assert_true(export_stats.total_spans == 500)
  assert_true(export_stats.exported_batches > 0)
  assert_true(export_stats.dropped_spans == 0)
  
  // 验证批处理效率
  let avg_batch_size = export_stats.total_spans / export_stats.exported_batches
  assert_true(avg_batch_size >= 50 && avg_batch_size <= 100) // 应该接近配置的最大批处理大小
}

// 测试6: 遥测数据压缩和序列化
test "遥测数据压缩和序列化测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "compression.test")
  
  // 创建包含大量数据的span
  let large_span = Tracer::start_span(tracer, "large.data.span")
  
  // 添加大量属性
  let mut i = 0
  while i < 100 {
    Span::set_attribute(large_span, "large.attr." + i.to_string(), StringValue("这是很长的属性值" + i.to_string() + "包含大量数据用于测试压缩效果"))
    i = i + 1
  }
  
  // 添加大量事件
  let mut i = 0
  while i < 50 {
    Span::add_event(large_span, "大量事件" + i.to_string(), [
      ("event.data", StringValue("事件包含大量数据" + i.to_string() + "用于测试序列化和压缩性能")),
      ("event.timestamp", IntValue(Time::now())),
      ("event.index", IntValue(i))
    ])
    i = i + 1
  }
  
  Span::end(large_span)
  
  // 测试序列化
  let serialized_data = Span::serialize(large_span)
  assert_true(serialized_data.length() > 0)
  
  // 测试压缩
  let compressed_data = Compression::compress(serialized_data)
  let compression_ratio = (compressed_data.length() * 100) / serialized_data.length()
  
  // 验证压缩效果
  assert_true(compression_ratio < 80) // 压缩后应该小于原大小的80%
  
  // 测试解压缩和反序列化
  let decompressed_data = Compression::decompress(compressed_data)
  let deserialized_span = Span::deserialize(decompressed_data)
  
  // 验证数据完整性
  assert_eq(Span::name(deserialized_span), Span::name(large_span))
  assert_eq(Span::trace_id(deserialized_span), Span::trace_id(large_span))
  assert_eq(Span::span_id(deserialized_span), Span::span_id(large_span))
}

// 测试7: 遥测系统资源监控
test "遥测系统资源监控测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource.monitoring")
  
  // 创建资源监控指标
  let memory_usage = Meter::create_gauge(meter, "telemetry.memory.usage.bytes")
  let cpu_usage = Meter::create_gauge(meter, "telemetry.cpu.usage.percent")
  let buffer_size = Meter::create_gauge(meter, "telemetry.buffer.size")
  let export_queue_size = Meter::create_gauge(meter, "telemetry.export.queue.size")
  
  // 模拟遥测系统运行
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.test")
  
  // 创建大量span来测试资源使用
  let mut i = 0
  while i < 1000 {
    let span = Tracer::start_span(tracer, "resource.test.span")
    Span::set_attribute(span, "index", IntValue(i))
    
    // 模拟资源使用情况
    if i % 100 == 0 {
      // 模拟内存使用增长
      let current_memory = 1024 * 1024 * (i / 100) // 每次增加1MB
      Gauge::set(memory_usage, IntValue(current_memory))
      
      // 模拟CPU使用
      let cpu_percent = 20.0 + (Random::float() * 60.0) // 20-80%
      Gauge::set(cpu_usage, FloatValue(cpu_percent))
      
      // 模拟缓冲区大小
      let buffer_size_value = 1000 + (Random::int() % 5000)
      Gauge::set(buffer_size, IntValue(buffer_size_value))
      
      // 模拟导出队列大小
      let queue_size = 100 + (Random::int() % 900)
      Gauge::set(export_queue_size, IntValue(queue_size))
    }
    
    Span::end(span)
    i = i + 1
  }
  
  // 验证资源监控指标
  assert_true(Gauge::value(memory_usage) > 0)
  assert_true(Gauge::value(cpu_usage) >= 20.0 && Gauge::value(cpu_usage) <= 80.0)
  assert_true(Gauge::value(buffer_size) > 0)
  assert_true(Gauge::value(export_queue_size) > 0)
  
  // 测试资源限制
  let max_memory = 50 * 1024 * 1024 // 50MB
  assert_true(Gauge::value(memory_usage) < max_memory) // 确保内存使用在限制内
}

// 测试8: 遥测数据关联和链接
test "遥测数据关联和链接测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.correlation")
  
  // 创建主业务流程span
  let business_span = Tracer::start_span(tracer, "business.process")
  let business_context = Span::context(business_span)
  
  // 设置业务流程属性
  Span::set_attribute(business_span, "business.process.id", StringValue("proc-123"))
  Span::set_attribute(business_span, "business.process.type", StringValue("order.fulfillment"))
  Span::set_attribute(business_span, "customer.id", StringValue("cust-456"))
  
  // 创建子流程span - 库存检查
  let inventory_span = Tracer::start_span_with_context(tracer, "inventory.check", business_context)
  Span::set_attribute(inventory_span, "inventory.item.id", StringValue("item-789"))
  Span::set_attribute(inventory_span, "inventory.quantity", IntValue(5))
  
  // 创建子流程span - 支付处理
  let payment_span = Tracer::start_span_with_context(tracer, "payment.process", business_context)
  Span::set_attribute(payment_span, "payment.transaction.id", StringValue("txn-abc"))
  Span::set_attribute(payment_span, "payment.amount", FloatValue(99.99))
  
  // 创建关联span - 物流安排（与业务流程关联但不是直接子流程）
  let logistics_span = Tracer::start_span(tracer, "logistics.arrangement")
  Span::set_attribute(logistics_span, "logistics.order.id", StringValue("order-123"))
  Span::set_attribute(logistics_span, "logistics.method", StringValue("express"))
  
  // 建立链接关系
  Span::add_link(logistics_span, business_context, [
    ("link.type", StringValue("business.association")),
    ("link.reason", StringValue("order.fulfillment.requires.shipping"))
  ])
  
  // 创建通知span（与多个流程关联）
  let notification_span = Tracer::start_span(tracer, "customer.notification")
  Span::add_link(notification_span, Span::context(inventory_span), [
    ("link.type", StringValue("inventory.confirmation"))
  ])
  Span::add_link(notification_span, Span::context(payment_span), [
    ("link.type", StringValue("payment.confirmation"))
  ])
  
  // 完成所有span
  Span::end(logistics_span)
  Span::end(inventory_span)
  Span::end(payment_span)
  Span::end(notification_span)
  Span::end(business_span)
  
  // 验证关联关系
  assert_eq(Span::link_count(logistics_span), 1)
  assert_eq(Span::link_count(notification_span), 2)
  
  // 验证链接属性
  let logistics_links = Span::get_links(logistics_span)
  assert_eq(logistics_links[0].attributes[0].0, "link.type")
  assert_eq(logistics_links[0].attributes[0].1, StringValue("business.association"))
}

// 测试9: 遥测系统故障恢复
test "遥测系统故障恢复测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "fault.recovery")
  
  // 创建故障注入器
  let fault_injector = FaultInjector::new()
  
  // 创建正常span
  let normal_span = Tracer::start_span(tracer, "normal.operation")
  Span::set_attribute(normal_span, "operation.type", StringValue("normal"))
  Span::end(normal_span)
  
  // 注入内存不足故障
  FaultInjector::inject_memory_error(fault_injector, "out.of.memory")
  let memory_error_span = Tracer::start_span(tracer, "memory.error.operation")
  Span::set_attribute(memory_error_span, "operation.type", StringValue("memory.error"))
  
  // 系统应该能够处理错误并继续运行
  let recovered_span = Tracer::start_span(tracer, "recovered.operation")
  Span::set_attribute(recovered_span, "operation.type", StringValue("recovered"))
  Span::end(recovered_span)
  
  // 注入网络故障
  FaultInjector::inject_network_error(fault_injector, "connection.timeout")
  let network_error_span = Tracer::start_span(tracer, "network.error.operation")
  Span::set_attribute(network_error_span, "operation.type", StringValue("network.error"))
  
  // 验证故障恢复能力
  let final_span = Tracer::start_span(tracer, "final.operation")
  Span::set_attribute(final_span, "operation.type", StringValue("final"))
  Span::end(final_span)
  
  // 验证系统状态
  assert_eq(Span::status(normal_span), Ok)
  assert_eq(Span::status(recovered_span), Ok)
  assert_eq(Span::status(final_span), Ok)
  
  // 验证故障统计
  let fault_stats = FaultInjector::get_statistics(fault_injector)
  assert_true(fault_stats.injected_faults == 2)
  assert_true(fault_stats.recovered_operations >= 2)
  
  // 清理故障注入器
  FaultInjector::cleanup(fault_injector)
}

// 测试10: 遥测系统性能基准测试
test "遥测系统性能基准测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.benchmark")
  
  // 性能测试参数
  let span_count = 10000
  let attributes_per_span = 10
  let events_per_span = 5
  
  // 开始性能测试
  let start_time = Time::now()
  
  // 创建大量span
  let mut i = 0
  while i < span_count {
    let span = Tracer::start_span(tracer, "benchmark.span." + i.to_string())
    
    // 添加属性
    let mut j = 0
    while j < attributes_per_span {
      Span::set_attribute(span, "attr." + j.to_string(), StringValue("value." + j.to_string()))
      j = j + 1
    }
    
    // 添加事件
    let mut k = 0
    while k < events_per_span {
      Span::add_event(span, "event." + k.to_string(), [
        ("event.index", IntValue(k)),
        ("event.data", StringValue("benchmark.event.data"))
      ])
      k = k + 1
    }
    
    Span::end(span)
    i = i + 1
  }
  
  let end_time = Time::now()
  let total_time = Time::difference(end_time, start_time)
  
  // 计算性能指标
  let spans_per_second = (span_count * 1000) / total_time
  let avg_time_per_span = total_time / span_count
  
  // 验证性能要求
  assert_true(spans_per_second > 1000) // 每秒至少处理1000个span
  assert_true(avg_time_per_span < 1.0) // 每个span处理时间小于1ms
  
  // 测试内存使用效率
  let memory_before = Memory::used()
  
  // 创建额外的span来测试内存使用
  let memory_test_span = Tracer::start_span(tracer, "memory.test")
  Span::set_attribute(memory_test_span, "large.data", StringValue("x" * 10000))
  Span::end(memory_test_span)
  
  let memory_after = Memory::used()
  let memory_increase = memory_after - memory_before
  
  // 验证内存使用效率
  assert_true(memory_increase < 10 * 1024 * 1024) // 内存增长小于10MB
  
  // 输出性能基准结果
  let benchmark_result = {
    "span_count": span_count,
    "total_time_ms": total_time,
    "spans_per_second": spans_per_second,
    "avg_time_per_span_ms": avg_time_per_span,
    "memory_increase_bytes": memory_increase
  }
  
  // 记录基准测试结果
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark.metrics")
  let benchmark_gauge = Meter::create_gauge(meter, "telemetry.benchmark.result")
  Gauge::set(benchmark_gauge, StringValue(benchmark_result.to_string()))
}