// Azimuth Premium Quality Enhanced Test Suite
// 高质量增强测试套件 - 专注于遥测系统的关键功能和边界条件

// 测试1: 遥测数据完整性和一致性验证
test "遥测数据完整性和一致性验证" {
  let provider = TelemetryProvider::initialize()
  let tracer = TelemetryProvider::get_tracer(provider, "data.integrity.test")
  
  // 创建具有复杂属性的span
  let span = Tracer::start_span(tracer, "complex.operation")
  Span::set_attribute(span, "service.name", StringValue("payment.service"))
  Span::set_attribute(span, "operation.type", StringValue("transaction.processing"))
  Span::set_attribute(span, "transaction.id", StringValue("txn_123456789"))
  Span::set_attribute(span, "amount", DoubleValue(1250.75))
  Span::set_attribute(span, "currency", StringValue("USD"))
  Span::set_attribute(span, "customer.tier", StringValue("premium"))
  Span::set_attribute(span, "processing.time.ms", LongValue(250))
  Span::set_attribute(span, "success", BoolValue(true))
  
  // 添加带有时间戳的事件
  let event_attributes = Attributes::new()
  Attributes::set(event_attributes, "event.type", StringValue("validation.completed"))
  Attributes::set(event_attributes, "validation.result", StringValue("passed"))
  Span::add_event_with_timestamp(span, "validation", event_attributes, 1640995200000000000)
  
  // 验证span属性完整性
  let attributes = Span::attributes(span)
  assert_eq(attributes.length(), 8)
  assert_true(attributes.contains("service.name"))
  assert_true(attributes.contains("transaction.id"))
  assert_true(attributes.contains("amount"))
  
  Span::end(span)
  
  // 验证span结束后数据仍然可访问
  let span_data = Span::to_data(span)
  assert_eq(span_data.name, "complex.operation")
  assert_eq(span_data.status, Ok)
  assert_true(span_data.end_time > span_data.start_time)
}

// 测试2: 高并发遥测数据收集性能测试
test "高并发遥测数据收集性能测试" {
  let provider = TelemetryProvider::initialize_with_config(
    Config::new()
      ::with_max_concurrent_spans(1000)
      ::with_batch_size(100)
      ::with_export_timeout(5000)
  )
  
  let meter = TelemetryProvider::get_meter(provider, "performance.test")
  let tracer = TelemetryProvider::get_tracer(provider, "performance.test")
  
  // 创建性能指标
  let operation_counter = Meter::create_counter(meter, "operations.total")
  let latency_histogram = Meter::create_histogram(meter, "operation.latency")
  let active_gauge = Meter::create_gauge(meter, "active.operations")
  
  // 模拟高并发操作
  let concurrent_operations = 100
  let start_time = Time::now()
  
  for i = 0; i < concurrent_operations; i = i + 1 {
    let span = Tracer::start_span(tracer, "concurrent.operation." + i.to_string())
    Gauge::increment(active_gauge)
    
    // 模拟操作延迟
    let operation_start = Time::now()
    Time::sleep(Random::int_range(1, 10))
    let operation_duration = Time::since(operation_start)
    
    Span::set_attribute(span, "operation.id", LongValue(i))
    Span::set_attribute(span, "operation.duration", DoubleValue(operation_duration))
    Counter::add(operation_counter, 1.0)
    Histogram::record(latency_histogram, operation_duration)
    
    Span::end(span)
    Gauge::decrement(active_gauge)
  }
  
  let total_time = Time::since(start_time)
  
  // 验证性能指标
  assert_eq(Counter::value(operation_counter), Int::to_float(concurrent_operations))
  assert_eq(Gauge::value(active_gauge), 0.0)
  assert_true(total_time < 5000.0) // 应该在5秒内完成
  
  // 验证直方图统计数据
  let histogram_data = Histogram::data(latency_histogram)
  assert_eq(histogram_data.count, concurrent_operations)
  assert_true(histogram_data.sum > 0.0)
  assert_true(histogram_data.min >= 0.0)
  assert_true(histogram_data.max >= histogram_data.min)
}

// 测试3: 分布式追踪传播和注入机制
test "分布式追踪传播和注入机制" {
  let provider = TelemetryProvider::initialize()
  let tracer = TelemetryProvider::get_tracer(provider, "propagation.test")
  
  // 创建根span和上下文
  let root_span = Tracer::start_span(tracer, "root.service.operation")
  let root_context = Span::context(root_span)
  
  // 设置关键追踪属性
  Span::set_attribute(root_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(root_span, "service.version", StringValue("1.2.3"))
  Span::set_attribute(root_span, "deployment.environment", StringValue("production"))
  
  // 创建子服务span
  let auth_span = Tracer::start_span_with_context(tracer, "auth.service.validate", root_context)
  Span::set_attribute(auth_span, "service.name", StringValue("auth.service"))
  Span::set_attribute(auth_span, "user.id", StringValue("user_789"))
  Span::set_attribute(auth_span, "auth.method", StringValue("jwt"))
  
  // 创建更深层级的span
  let db_span = Tracer::start_span_with_context(tracer, "database.query", Span::context(auth_span))
  Span::set_attribute(db_span, "service.name", StringValue("database.service"))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = $1"))
  Span::set_attribute(db_span, "db.rows_affected", LongValue(1))
  
  // 模拟baggage传播
  let baggage = Baggage::new()
  Baggage::set(baggage, "user.role", StringValue("admin"))
  Baggage::set(baggage, "request.id", StringValue("req_12345"))
  Baggage::set(baggage, "trace.sampled", StringValue("true"))
  
  let propagated_context = Context::with_baggage(Span::context(db_span), baggage)
  
  // 验证上下文传播
  assert_true(Context::has_span(propagated_context))
  assert_true(Context::has_baggage(propagated_context))
  
  let propagated_baggage = Context::baggage(propagated_context)
  assert_eq(propagated_baggage.length(), 3)
  assert_true(propagated_baggage.contains("user.role"))
  assert_true(propagated_baggage.contains("request.id"))
  
  Span::end(db_span)
  Span::end(auth_span)
  Span::end(root_span)
  
  // 验证追踪链完整性
  let trace_id = Span::trace_id(root_span)
  assert_eq(Span::trace_id(auth_span), trace_id)
  assert_eq(Span::trace_id(db_span), trace_id)
  
  // 验证父子关系
  assert_eq(Span::parent_span_id(auth_span), Span::span_id(root_span))
  assert_eq(Span::parent_span_id(db_span), Span::span_id(auth_span))
}

// 测试4: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化" {
  let provider = TelemetryProvider::initialize()
  let tracer = TelemetryProvider::get_tracer(provider, "serialization.test")
  let meter = TelemetryProvider::get_meter(provider, "serialization.test")
  
  // 创建复杂的span数据
  let span = Tracer::start_span(tracer, "serialization.test.span")
  
  // 设置各种类型的属性
  let complex_attributes = Attributes::new()
  Attributes::set(complex_attributes, "string.value", StringValue("test.string"))
  Attributes::set(complex_attributes, "int.value", LongValue(42))
  Attributes::set(complex_attributes, "float.value", DoubleValue(3.14159))
  Attributes::set(complex_attributes, "bool.value", BoolValue(true))
  Attributes::set(complex_attributes, "array.value", ArrayValue([StringValue("a"), StringValue("b"), StringValue("c")]))
  
  Span::set_attributes(span, complex_attributes)
  
  // 添加事件
  let event_attributes = Attributes::new()
  Attributes::set(event_attributes, "event.data", StringValue("sample.event.data"))
  Span::add_event(span, "test.event", Some(event_attributes))
  
  // 创建指标数据
  let counter = Meter::create_counter(meter, "serialization.test.counter")
  Counter::add(counter, 100.0)
  
  let histogram = Meter::create_histogram(meter, "serialization.test.histogram")
  Histogram::record(histogram, 1.5)
  Histogram::record(histogram, 2.5)
  Histogram::record(histogram, 3.5)
  
  let gauge = Meter::create_gauge(meter, "serialization.test.gauge")
  Gauge::set(gauge, 75.0)
  
  Span::end(span)
  
  // 序列化span数据
  let span_data = Span::to_data(span)
  let serialized_span = Serialization::serialize_span(span_data)
  
  // 反序列化span数据
  let deserialized_span_data = Serialization::deserialize_span(serialized_span)
  
  // 验证序列化/反序列化正确性
  assert_eq(deserialized_span_data.name, span_data.name)
  assert_eq(deserialized_span_data.trace_id, span_data.trace_id)
  assert_eq(deserialized_span_data.span_id, span_data.span_id)
  assert_eq(deserialized_span_data.attributes.length(), span_data.attributes.length())
  assert_eq(deserialized_span_data.events.length(), span_data.events.length())
  
  // 序列化指标数据
  let metrics_data = Metrics::collect_all(meter)
  let serialized_metrics = Serialization::serialize_metrics(metrics_data)
  
  // 反序列化指标数据
  let deserialized_metrics_data = Serialization::deserialize_metrics(serialized_metrics)
  
  // 验证指标数据完整性
  assert_eq(deserialized_metrics_data.length(), metrics_data.length())
  
  let deserialized_counter = Metrics::find_by_name(deserialized_metrics_data, "serialization.test.counter")
  assert_true(deserialized_counter.is_some())
  
  match deserialized_counter {
    Some(counter_data) => {
      assert_eq(counter_data.value, 100.0)
    }
    None => assert_true(false)
  }
}

// 测试5: 错误处理和异常情况测试
test "错误处理和异常情况测试" {
  let provider = TelemetryProvider::initialize()
  let tracer = TelemetryProvider::get_tracer(provider, "error.handling.test")
  let meter = TelemetryProvider::get_meter(provider, "error.handling.test")
  
  // 测试span错误状态处理
  let error_span = Tracer::start_span(tracer, "error.prone.operation")
  Span::set_attribute(error_span, "operation.type", StringValue("database.write"))
  
  // 模拟错误情况
  let error_attributes = Attributes::new()
  Attributes::set(error_attributes, "error.type", StringValue("DatabaseConnectionError"))
  Attributes::set(error_attributes, "error.message", StringValue("Connection timeout after 30 seconds"))
  Attributes::set(error_attributes, "error.code", LongValue(503))
  Attributes::set(error_attributes, "retry.count", LongValue(3))
  
  Span::record_exception(error_span, "DatabaseConnectionError", "Connection timeout", Some(error_attributes))
  Span::set_status(error_span, Error("Connection timeout"))
  
  // 验证错误状态
  let span_data = Span::to_data(error_span)
  assert_eq(span_data.status, Error("Connection timeout"))
  assert_true(span_data.events.length() > 0)
  
  Span::end(error_span)
  
  // 测试指标操作错误处理
  let error_counter = Meter::create_counter(meter, "errors.total")
  let error_gauge = Meter::create_gauge(meter, "error.rate")
  
  // 模拟大量错误记录
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(error_counter, 1.0)
  }
  
  // 验证错误指标
  assert_eq(Counter::value(error_counter), 1000.0)
  
  // 测试边界条件
  let zero_gauge = Meter::create_gauge(meter, "zero.gauge")
  Gauge::set(zero_gauge, 0.0)
  assert_eq(Gauge::value(zero_gauge), 0.0)
  
  let negative_gauge = Meter::create_gauge(meter, "negative.gauge")
  Gauge::set(negative_gauge, -100.0)
  assert_eq(Gauge::value(negative_gauge), -100.0)
  
  // 测试空值和null处理
  let empty_attributes = Attributes::new()
  assert_eq(empty_attributes.length(), 0)
  
  // 测试极值处理
  let max_counter = Meter::create_counter(meter, "max.counter")
  let max_value = 18446744073709551615.0 // 2^64 - 1
  Counter::add(max_counter, max_value)
  assert_eq(Counter::value(max_counter), max_value)
}

// 测试6: 内存管理和资源清理测试
test "内存管理和资源清理测试" {
  // 测试大量span创建和销毁
  let provider = TelemetryProvider::initialize_with_config(
    Config::new()
      ::with_max_spans_per_trace(100)
      ::with_span_ttl(60000) // 60秒TTL
  )
  
  let tracer = TelemetryProvider::get_tracer(provider, "memory.test")
  
  // 创建大量span测试内存管理
  let span_count = 500
  let spans = Array::empty()
  
  for i = 0; i < span_count; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory.test.span." + i.to_string())
    Span::set_attribute(span, "span.index", LongValue(i))
    Array::push(spans, span)
  }
  
  // 验证span创建
  assert_eq(Array::length(spans), span_count)
  
  // 批量结束span
  for span in spans {
    Span::end(span)
  }
  
  // 强制垃圾回收
  GC::collect()
  
  // 验证资源清理
  let memory_usage = Memory::usage()
  assert_true(memory_usage.heap_size < 100 * 1024 * 1024) // 小于100MB
  
  // 测试指标资源管理
  let meter = TelemetryProvider::get_meter(provider, "memory.test")
  let metrics = Array::empty()
  
  // 创建大量指标
  for i = 0; i < 100; i = i + 1 {
    let counter = Meter::create_counter(meter, "counter." + i.to_string())
    let histogram = Meter::create_histogram(meter, "histogram." + i.to_string())
    let gauge = Meter::create_gauge(meter, "gauge." + i.to_string())
    
    Array::push(metrics, counter)
    Array::push(metrics, histogram)
    Array::push(metrics, gauge)
  }
  
  // 使用指标
  for i = 0; i < Array::length(metrics); i = i + 1 {
    match Array::get(metrics, i) {
      Some(counter) => Counter::add(counter, 1.0),
      Some(histogram) => Histogram::record(histogram, 1.0),
      Some(gauge) => Gauge::set(gauge, 1.0),
      _ => void
    }
  }
  
  // 清理指标
  Metrics::cleanup(meter)
  
  // 再次检查内存使用
  let final_memory_usage = Memory::usage()
  assert_true(final_memory_usage.heap_size <= memory_usage.heap_size + 10 * 1024 * 1024) // 增长不超过10MB
}

// 测试7: 多线程并发安全测试
test "多线程并发安全测试" {
  let provider = TelemetryProvider::initialize_with_config(
    Config::new()
      ::with_thread_safe(true)
      ::with_concurrent_exports(5)
  )
  
  let tracer = TelemetryProvider::get_tracer(provider, "concurrency.test")
  let meter = TelemetryProvider::get_meter(provider, "concurrency.test")
  
  // 创建共享指标
  let shared_counter = Meter::create_counter(meter, "shared.operations")
  let shared_gauge = Meter::create_gauge(meter, "active.threads")
  
  // 模拟多线程环境
  let thread_count = 10
  let operations_per_thread = 50
  let threads = Array::empty()
  
  for thread_id = 0; thread_id < thread_count; thread_id = thread_id + 1 {
    let thread = Thread::spawn(fn() {
      Gauge::increment(shared_gauge)
      
      for op_id = 0; op_id < operations_per_thread; op_id = op_id + 1 {
        let span = Tracer::start_span(tracer, "thread." + thread_id.to_string() + ".operation." + op_id.to_string())
        Span::set_attribute(span, "thread.id", LongValue(thread_id))
        Span::set_attribute(span, "operation.id", LongValue(op_id))
        
        // 模拟工作负载
        Time::sleep(Random::int_range(1, 5))
        
        Counter::add(shared_counter, 1.0)
        Span::end(span)
      }
      
      Gauge::decrement(shared_gauge)
    })
    
    Array::push(threads, thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证并发操作结果
  let expected_operations = Int::to_float(thread_count * operations_per_thread)
  assert_eq(Counter::value(shared_counter), expected_operations)
  assert_eq(Gauge::value(shared_gauge), 0.0)
  
  // 验证数据一致性
  let all_spans = Tracer::get_all_spans(tracer)
  assert_eq(Array::length(all_spans), thread_count * operations_per_thread)
  
  // 验证没有数据竞争或损坏
  for span in all_spans {
    let attributes = Span::attributes(span)
    assert_true(attributes.contains("thread.id"))
    assert_true(attributes.contains("operation.id"))
  }
}

// 测试8: 配置管理和动态更新测试
test "配置管理和动态更新测试" {
  // 创建初始配置
  let initial_config = Config::new()
    ::with_service_name("test.service")
    ::with_service_version("1.0.0")
    ::with_max_concurrent_spans(100)
    ::with_batch_size(50)
    ::with_export_timeout(3000)
    ::with_sampling_rate(1.0)
  
  let provider = TelemetryProvider::initialize_with_config(initial_config)
  
  // 验证初始配置
  let current_config = TelemetryProvider::get_config(provider)
  assert_eq(current_config.service_name, "test.service")
  assert_eq(current_config.service_version, "1.0.0")
  assert_eq(current_config.max_concurrent_spans, 100)
  assert_eq(current_config.batch_size, 50)
  assert_eq(current_config.export_timeout, 3000)
  assert_eq(current_config.sampling_rate, 1.0)
  
  // 动态更新配置
  let updated_config = Config::new()
    ::with_service_name("updated.service")
    ::with_service_version("2.0.0")
    ::with_max_concurrent_spans(200)
    ::with_batch_size(100)
    ::with_export_timeout(5000)
    ::with_sampling_rate(0.5)
  
  TelemetryProvider::update_config(provider, updated_config)
  
  // 验证配置更新
  let new_config = TelemetryProvider::get_config(provider)
  assert_eq(new_config.service_name, "updated.service")
  assert_eq(new_config.service_version, "2.0.0")
  assert_eq(new_config.max_concurrent_spans, 200)
  assert_eq(new_config.batch_size, 100)
  assert_eq(new_config.export_timeout, 5000)
  assert_eq(new_config.sampling_rate, 0.5)
  
  // 测试配置验证
  let invalid_config = Config::new()
    ::with_max_concurrent_spans(-1) // 无效值
    ::with_sampling_rate(2.0) // 超出范围
  
  let validation_result = Config::validate(invalid_config)
  assert_true(validation_result.has_errors)
  assert_true(validation_result.errors.contains("max_concurrent_spans must be positive"))
  assert_true(validation_result.errors.contains("sampling_rate must be between 0.0 and 1.0"))
  
  // 测试配置持久化
  let config_file = "/tmp/azimuth_test_config.json"
  Config::save_to_file(new_config, config_file)
  
  let loaded_config = Config::load_from_file(config_file)
  assert_eq(loaded_config.service_name, "updated.service")
  assert_eq(loaded_config.service_version, "2.0.0")
  
  // 清理
  FileSystem::remove_file(config_file)
}

// 测试9: 跨平台兼容性测试
test "跨平台兼容性测试" {
  let provider = TelemetryProvider::initialize()
  let tracer = TelemetryProvider::get_tracer(provider, "platform.test")
  
  // 检测平台信息
  let platform_info = Platform::detect()
  let span = Tracer::start_span(tracer, "platform.compatibility.test")
  
  Span::set_attribute(span, "platform.os", StringValue(platform_info.os))
  Span::set_attribute(span, "platform.arch", StringValue(platform_info.arch))
  Span::set_attribute(span, "platform.version", StringValue(platform_info.version))
  
  // 测试平台特定的功能
  match platform_info.os {
    "linux" => {
      // Linux特定测试
      let memory_info = Platform::linux_memory_info()
      Span::set_attribute(span, "platform.memory.total", LongValue(memory_info.total))
      Span::set_attribute(span, "platform.memory.available", LongValue(memory_info.available))
    }
    "windows" => {
      // Windows特定测试
      let process_info = Platform::windows_process_info()
      Span::set_attribute(span, "platform.process.id", LongValue(process_info.id))
      Span::set_attribute(span, "platform.process.memory", LongValue(process_info.memory_usage))
    }
    "macos" => {
      // macOS特定测试
      let system_info = Platform::macos_system_info()
      Span::set_attribute(span, "platform.system.uptime", LongValue(system_info.uptime))
      Span::set_attribute(span, "platform.system.load", DoubleValue(system_info.load_average))
    }
    _ => {
      // 通用测试
      Span::set_attribute(span, "platform.generic", BoolValue(true))
    }
  }
  
  // 测试时间精度
  let high_res_time = Platform::high_resolution_time()
  Span::set_attribute(span, "platform.time.precision", LongValue(high_res_time.precision))
  Span::set_attribute(span, "platform.time.frequency", LongValue(high_res_time.frequency))
  
  // 测试文件系统操作
  let temp_dir = Platform::temp_directory()
  let test_file = temp_dir + "/azimuth_platform_test.txt"
  
  let file_handle = FileSystem::open_write(test_file)
  FileSystem::write_string(file_handle, "platform test content")
  FileSystem::close(file_handle)
  
  let read_handle = FileSystem::open_read(test_file)
  let content = FileSystem::read_string(read_handle)
  FileSystem::close(read_handle)
  
  assert_eq(content, "platform test content")
  FileSystem::remove_file(test_file)
  
  Span::set_attribute(span, "platform.filesystem.working", BoolValue(true))
  
  // 测试网络功能（如果可用）
  if Platform::has_network_support() {
    let network_info = Platform::network_info()
    Span::set_attribute(span, "platform.network.available", BoolValue(true))
    Span::set_attribute(span, "platform.network.hostname", StringValue(network_info.hostname))
  } else {
    Span::set_attribute(span, "platform.network.available", BoolValue(false))
  }
  
  Span::end(span)
  
  // 验证跨平台数据一致性
  let span_data = Span::to_data(span)
  assert_true(span_data.attributes.contains("platform.os"))
  assert_true(span_data.attributes.contains("platform.arch"))
  assert_true(span_data.attributes.contains("platform.filesystem.working"))
}

// 测试10: 端到端遥测工作流测试
test "端到端遥测工作流测试" {
  // 初始化完整的遥测系统
  let config = Config::new()
    ::with_service_name("e2e.test.service")
    ::with_service_version("1.0.0")
    ::with_sampling_rate(1.0)
    ::with_batch_size(10)
    ::with_export_timeout(5000)
  
  let provider = TelemetryProvider::initialize_with_config(config)
  let tracer = TelemetryProvider::get_tracer(provider, "e2e.test")
  let meter = TelemetryProvider::get_meter(provider, "e2e.test")
  let logger = TelemetryProvider::get_logger(provider, "e2e.test")
  
  // 创建工作流指标
  let workflow_counter = Meter::create_counter(meter, "workflows.completed")
  let workflow_duration = Meter::create_histogram(meter, "workflow.duration")
  let active_workflows = Meter::create_gauge(meter, "workflows.active")
  
  // 模拟完整的业务工作流
  let workflow_start = Time::now()
  Gauge::increment(active_workflows)
  
  // 步骤1: 请求接收
  let request_span = Tracer::start_span(tracer, "workflow.request.receive")
  Span::set_attribute(request_span, "http.method", StringValue("POST"))
  Span::set_attribute(request_span, "http.url", StringValue("/api/v1/process"))
  Span::set_attribute(request_span, "http.user_agent", StringValue("Azimuth-Test/1.0"))
  
  Logger::info(logger, "Received workflow request", 
    Map::from_array([("request_id", "req_12345"), ("method", "POST")]))
  
  Time::sleep(10) // 模拟处理时间
  Span::end(request_span)
  
  // 步骤2: 身份验证
  let auth_span = Tracer::start_span_with_context(tracer, "workflow.auth.validate", Span::context(request_span))
  Span::set_attribute(auth_span, "auth.type", StringValue("JWT"))
  Span::set_attribute(auth_span, "auth.user_id", StringValue("user_789"))
  
  Logger::info(logger, "Authentication successful", 
    Map::from_array([("user_id", "user_789"), ("auth_duration", "5ms")]))
  
  Time::sleep(5)
  Span::end(auth_span)
  
  // 步骤3: 业务逻辑处理
  let business_span = Tracer::start_span_with_context(tracer, "workflow.business.process", Span::context(auth_span))
  Span::set_attribute(business_span, "business.operation", StringValue("order.processing"))
  Span::set_attribute(business_span, "business.order_id", StringValue("order_456"))
  
  // 子步骤3.1: 数据库操作
  let db_span = Tracer::start_span_with_context(tracer, "workflow.database.query", Span::context(business_span))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM orders WHERE id = $1"))
  Span::set_attribute(db_span, "db.rows_returned", LongValue(1))
  
  Time::sleep(15)
  Span::end(db_span)
  
  // 子步骤3.2: 外部服务调用
  let external_span = Tracer::start_span_with_context(tracer, "workflow.external.call", Span::context(business_span))
  Span::set_attribute(external_span, "external.service", StringValue("payment.gateway"))
  Span::set_attribute(external_span, "external.endpoint", StringValue("/api/payments"))
  Span::set_attribute(external_span, "external.method", StringValue("POST"))
  
  Time::sleep(20)
  Span::set_status(external_span, Ok)
  Span::end(external_span)
  
  Logger::info(logger, "Business logic completed", 
    Map::from_array([("order_id", "order_456"), ("processing_time", "35ms")]))
  
  Span::end(business_span)
  
  // 步骤4: 响应生成
  let response_span = Tracer::start_span_with_context(tracer, "workflow.response.generate", Span::context(business_span))
  Span::set_attribute(response_span, "http.status_code", LongValue(200))
  Span::set_attribute(response_span, "response.size", LongValue(1024))
  
  Time::sleep(5)
  Span::end(response_span)
  
  // 工作流完成
  let workflow_total_time = Time::since(workflow_start)
  Counter::add(workflow_counter, 1.0)
  Histogram::record(workflow_duration, workflow_total_time)
  Gauge::decrement(active_workflows)
  
  Logger::info(logger, "Workflow completed successfully", 
    Map::from_array([("request_id", "req_12345"), ("total_time", workflow_total_time.to_string() + "ms")]))
  
  // 验证端到端追踪链
  let all_spans = Tracer::get_all_spans(tracer)
  assert_eq(Array::length(all_spans), 6) // request, auth, business, db, external, response
  
  // 验证追踪链完整性
  let trace_ids = Array::map(all_spans, fn(span) { Span::trace_id(span) })
  let unique_trace_ids = Array::unique(trace_ids)
  assert_eq(Array::length(unique_trace_ids), 1) // 所有span应该有相同的trace_id
  
  // 验证指标数据
  assert_eq(Counter::value(workflow_counter), 1.0)
  assert_eq(Gauge::value(active_workflows), 0.0)
  
  let histogram_data = Histogram::data(workflow_duration)
  assert_eq(histogram_data.count, 1)
  assert_true(histogram_data.sum >= 50.0) // 至少50ms的处理时间
  
  // 验证日志记录
  let log_entries = Logger::get_entries(logger)
  assert_eq(Array::length(log_entries), 4) // 4个info日志
  
  // 导出遥测数据
  let export_result = TelemetryProvider::export(provider)
  assert_true(export_result.success)
  assert_eq(export_result.spans_exported, 6)
  assert_eq(export_result.metrics_exported, 3)
  assert_eq(export_result.logs_exported, 4)
}