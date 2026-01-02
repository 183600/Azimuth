// Azimuth New Telemetry Feature Tests
// 新的遥测功能测试套件，专注于遥测系统的高级特性和实际应用场景

// 测试1: 遥测数据聚合和分析
test "遥测数据聚合和分析测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "aggregation-test")
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-metrics")
  
  // 创建聚合器
  let aggregator = azimuth::TelemetryAggregator::new()
  
  // 创建多个Span用于聚合
  let spans = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "aggregation-span-" + i.to_string())
    
    // 添加不同类型的属性
    azimuth::Span::set_attribute(span, "operation.type", 
      azimuth::StringValue(if i % 3 == 0 { "read" } else if i % 3 == 1 { "write" } else { "update" }))
    azimuth::Span::set_attribute(span, "service.component", 
      azimuth::StringValue("component-" + (i % 5).to_string()))
    azimuth::Span::set_attribute(span, "latency.ms", 
        azimuth::IntValue(10 + (i % 100)))
    
    // 添加事件
    azimuth::Span::add_event(span, "operation-completed", 
      Some([("result", azimuth::StringValue(if i % 10 == 0 { "error" } else { "success" }))]))
    
    spans.push(span)
  }
  
  // 执行聚合
  let aggregation_result = azimuth::TelemetryAggregator::aggregate_spans(aggregator, spans)
  
  // 验证聚合结果
  let operation_stats = azimuth::AggregationResult::get_attribute_stats(aggregation_result, "operation.type")
  assert_true(azimuth::AttributeStats::count(operation_stats, "read") > 0)
  assert_true(azimuth::AttributeStats::count(operation_stats, "write") > 0)
  assert_true(azimuth::AttributeStats::count(operation_stats, "update") > 0)
  
  let component_stats = azimuth::AggregationResult::get_attribute_stats(aggregation_result, "service.component")
  assert_eq(azimuth::AttributeStats::unique_values(component_stats), 5)
  
  let latency_stats = azimuth::AggregationResult::get_numeric_stats(aggregation_result, "latency.ms")
  assert_true(azimuth::NumericStats::average(latency_stats) >= 10.0)
  assert_true(azimuth::NumericStats::average(latency_stats) <= 109.0)
  
  // 验证事件统计
  let event_stats = azimuth::AggregationResult::get_event_stats(aggregation_result)
  assert_eq(azimuth::EventStats::total_events(event_stats), 50)
  assert_true(azimuth::EventStats::event_count(event_stats, "operation-completed") > 0)
}

// 测试2: 分布式追踪的上下文传播
test "分布式追踪上下文传播测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  
  // 创建父服务追踪器
  let parent_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "parent-service")
  let parent_span = azimuth::Tracer::start_span(parent_tracer, "parent-operation")
  let parent_context = azimuth::Span::span_context(parent_span)
  
  // 创建上下文传播器
  let propagator = azimuth::TraceContextPropagator::new()
  
  // 注入上下文到载体
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::TraceContextPropagator::inject(propagator, parent_context, carrier)
  
  // 验证上下文被正确注入
  let trace_id_header = azimuth::TextMapCarrier::get(carrier, "traceparent")
  assert_true(trace_id_header != None)
  
  // 创建子服务追踪器
  let child_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "child-service")
  
  // 从载体提取上下文
  let extracted_context = azimuth::TraceContextPropagator::extract(propagator, carrier)
  
  // 验证上下文被正确提取
  assert_eq(azimuth::SpanContext::trace_id(extracted_context), 
    azimuth::SpanContext::trace_id(parent_context))
  
  // 创建子Span，使用提取的上下文作为父上下文
  let child_span = azimuth::Tracer::start_span_with_context(child_tracer, "child-operation", extracted_context)
  let child_context = azimuth::Span::span_context(child_span)
  
  // 验证父子关系
  assert_eq(azimuth::SpanContext::trace_id(child_context), 
    azimuth::SpanContext::trace_id(parent_context))
  assert_true(azimuth::SpanContext::is_remote_parent(child_context))
  
  // 验证Span链接
  let links = azimuth::Span::links(child_span)
  assert_true(links.length() > 0)
  
  // 结束Span
  azimuth::Span::end(child_span)
  azimuth::Span::end(parent_span)
}

// 测试3: 度量数据的实时计算
test "度量数据实时计算测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "realtime-metrics")
  
  // 创建实时计算器
  let realtime_calculator = azimuth::RealtimeCalculator::new()
  
  // 创建各种度量仪器
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total")
  let response_time_histogram = azimuth::Meter::create_histogram(meter, "http.response.time")
  let active_connections_gauge = azimuth::Meter::create_gauge(meter, "http.active.connections")
  let error_rate_meter = azimuth::Meter::create_meter(meter, "http.error.rate")
  
  // 模拟实时数据流
  for i in 0..100 {
    // 模拟HTTP请求
    azimuth::Counter::add(request_counter, 1.0)
    
    // 模拟响应时间
    let response_time = 50.0 + (@random() * 200.0)
    azimuth::Histogram::record(response_time_histogram, response_time)
    
    // 模拟活跃连接数变化
    let active_connections = 10 + (@random() * 90.0)
    azimuth::Gauge::record(active_connections_gauge, active_connections)
    
    // 模拟错误率
    if @random() < 0.05 {  // 5%错误率
      azimuth::Meter::record(error_rate_meter, 1.0)
    }
    
    // 每10个数据点执行一次实时计算
    if i % 10 == 0 {
      let calculation_result = azimuth::RealtimeCalculator::calculate_metrics(realtime_calculator, [
        request_counter,
        response_time_histogram,
        active_connections_gauge,
        error_rate_meter
      ])
      
      // 验证计算结果
      let total_requests = azimuth::CalculationResult::get_counter_value(calculation_result, "http.requests.total")
      assert_true(total_requests > 0.0)
      
      let avg_response_time = azimuth::CalculationResult::get_histogram_average(calculation_result, "http.response.time")
      assert_true(avg_response_time >= 50.0)
      assert_true(avg_response_time <= 250.0)
      
      let current_connections = azimuth::CalculationResult::get_gauge_value(calculation_result, "http.active.connections")
      assert_true(current_connections >= 10.0)
      assert_true(current_connections <= 100.0)
    }
  }
}

// 测试4: 日志记录的结构化处理
test "日志记录结构化处理测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "structured-logging-test")
  
  // 创建结构化日志处理器
  let structured_handler = azimuth::StructuredLogHandler::new()
  
  // 创建不同级别的结构化日志
  let debug_log = azimuth::LogRecord::new_with_attributes(
    azimuth::Debug,
    Some("调试信息"),
    Some([
      ("component", azimuth::StringValue("auth-service")),
      ("user.id", azimuth::StringValue("user-123")),
      ("operation", azimuth::StringValue("token-validation")),
      ("debug.details", azimuth::StringValue("Token signature verified"))
    ])
  )
  
  let info_log = azimuth::LogRecord::new_with_attributes(
    azimuth::Info,
    Some("用户登录成功"),
    Some([
      ("component", azimuth::StringValue("auth-service")),
      ("user.id", azimuth::StringValue("user-123")),
      ("ip.address", azimuth::StringValue("192.168.1.100")),
      ("user.agent", azimuth::StringValue("Mozilla/5.0...")),
      ("session.id", azimuth::StringValue("sess-456"))
    ])
  )
  
  let warning_log = azimuth::LogRecord::new_with_attributes(
    azimuth::Warn,
    Some("API调用频率接近限制"),
    Some([
      ("component", azimuth::StringValue("rate-limiter")),
      ("client.id", azimuth::StringValue("client-789")),
      ("current.rate", azimuth::IntValue(95)),
      ("limit.rate", azimuth::IntValue(100)),
      ("time.window", azimuth::StringValue("1m"))
    ])
  )
  
  let error_log = azimuth::LogRecord::new_with_attributes(
    azimuth::Error,
    Some("数据库连接失败"),
    Some([
      ("component", azimuth::StringValue("database")),
      ("error.code", azimuth::StringValue("DB_CONN_FAILED")),
      ("retry.count", azimuth::IntValue(3)),
      ("connection.pool", azimuth::StringValue("primary")),
      ("error.stack", azimuth::StringValue("ConnectionTimeoutException..."))
    ])
  )
  
  // 处理结构化日志
  let processed_logs = []
  processed_logs.push(azimuth::StructuredLogHandler::process(structured_handler, debug_log))
  processed_logs.push(azimuth::StructuredLogHandler::process(structured_handler, info_log))
  processed_logs.push(azimuth::StructuredLogHandler::process(structured_handler, warning_log))
  processed_logs.push(azimuth::StructuredLogHandler::process(structured_handler, error_log))
  
  // 发出日志
  for log in processed_logs {
    azimuth::Logger::emit(logger, log)
  }
  
  // 验证日志结构化处理
  let log_analyzer = azimuth::LogAnalyzer::new()
  let analysis_result = azimuth::LogAnalyzer::analyze_logs(log_analyzer, processed_logs)
  
  // 验证日志级别分布
  let level_distribution = azimuth::AnalysisResult::get_level_distribution(analysis_result)
  assert_eq(azimuth::LevelDistribution::count(level_distribution, azimuth::Debug), 1)
  assert_eq(azimuth::LevelDistribution::count(level_distribution, azimuth::Info), 1)
  assert_eq(azimuth::LevelDistribution::count(level_distribution, azimuth::Warn), 1)
  assert_eq(azimuth::LevelDistribution::count(level_distribution, azimuth::Error), 1)
  
  // 验证组件分布
  let component_distribution = azimuth::AnalysisResult::get_component_distribution(analysis_result)
  assert_eq(azimuth::ComponentDistribution::count(component_distribution, "auth-service"), 2)
  assert_eq(azimuth::ComponentDistribution::count(component_distribution, "rate-limiter"), 1)
  assert_eq(azimuth::ComponentDistribution::count(component_distribution, "database"), 1)
}

// 测试5: 资源监控和性能指标
test "资源监控和性能指标测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "resource-monitoring")
  
  // 创建资源监控器
  let resource_monitor = azimuth::ResourceMonitor::new()
  
  // 创建资源指标
  let cpu_usage = azimuth::Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage = azimuth::Meter::create_gauge(meter, "system.memory.usage")
  let disk_io = azimuth::Meter::create_counter(meter, "system.disk.io")
  let network_io = azimuth::Meter::create_counter(meter, "system.network.io")
  let thread_count = azimuth::Meter::create_gauge(meter, "system.thread.count")
  
  // 模拟资源监控数据收集
  for i in 0..20 {
    // 模拟CPU使用率
    let cpu_percent = 20.0 + (@random() * 60.0)
    azimuth::Gauge::record(cpu_usage, cpu_percent)
    
    // 模拟内存使用率
    let memory_percent = 30.0 + (@random() * 50.0)
    azimuth::Gauge::record(memory_usage, memory_percent)
    
    // 模拟磁盘I/O
    let disk_read_bytes = (@random() * 1000000.0).to_int()
    let disk_write_bytes = (@random() * 500000.0).to_int()
    azimuth::Counter::add(disk_io, (disk_read_bytes + disk_write_bytes).to_double())
    
    // 模拟网络I/O
    let network_recv_bytes = (@random() * 500000.0).to_int()
    let network_sent_bytes = (@random() * 300000.0).to_int()
    azimuth::Counter::add(network_io, (network_recv_bytes + network_sent_bytes).to_double())
    
    // 模拟线程数
    let thread_num = 50 + (@random() * 100.0)
    azimuth::Gauge::record(thread_count, thread_num)
    
    // 每隔几个周期进行资源分析
    if i % 5 == 0 {
      let resource_snapshot = azimuth::ResourceMonitor::create_snapshot(resource_monitor, [
        cpu_usage,
        memory_usage,
        disk_io,
        network_io,
        thread_count
      ])
      
      // 验证资源快照
      assert_true(azimuth::ResourceSnapshot::cpu_usage(resource_snapshot) >= 20.0)
      assert_true(azimuth::ResourceSnapshot::cpu_usage(resource_snapshot) <= 80.0)
      
      assert_true(azimuth::ResourceSnapshot::memory_usage(resource_snapshot) >= 30.0)
      assert_true(azimuth::ResourceSnapshot::memory_usage(resource_snapshot) <= 80.0)
      
      assert_true(azimuth::ResourceSnapshot::thread_count(resource_snapshot) >= 50.0)
      assert_true(azimuth::ResourceSnapshot::thread_count(resource_snapshot) <= 150.0)
      
      // 检查资源警告
      let warnings = azimuth::ResourceMonitor::check_resource_warnings(resource_monitor, resource_snapshot)
      if azimuth::ResourceSnapshot::cpu_usage(resource_snapshot) > 70.0 {
        assert_true(warnings.length() > 0)
      }
    }
  }
  
  // 生成资源使用报告
  let resource_report = azimuth::ResourceMonitor::generate_report(resource_monitor)
  
  // 验证报告内容
  assert_true(azimuth::ResourceReport::contains_metric(resource_report, "system.cpu.usage"))
  assert_true(azimuth::ResourceReport::contains_metric(resource_report, "system.memory.usage"))
  assert_true(azimuth::ResourceReport::contains_metric(resource_report, "system.disk.io"))
  assert_true(azimuth::ResourceReport::contains_metric(resource_report, "system.network.io"))
  assert_true(azimuth::ResourceReport::contains_metric(resource_report, "system.thread.count"))
}

// 测试6: 错误处理和异常追踪
test "错误处理和异常追踪测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-tracking-test")
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "error-logging")
  
  // 创建错误追踪器
  let error_tracker = azimuth::ErrorTracker::new()
  
  // 模拟不同类型的错误
  let errors = []
  
  // 网络错误
  let network_error_span = azimuth::Tracer::start_span(tracer, "network-operation")
  azimuth::Span::set_attribute(network_error_span, "error.type", azimuth::StringValue("NetworkTimeout"))
  azimuth::Span::set_attribute(network_error_span, "error.message", 
    azimuth::StringValue("Connection timeout after 30 seconds"))
  azimuth::Span::set_attribute(network_error_span, "retry.count", azimuth::IntValue(3))
  azimuth::Span::set_status(network_error_span, azimuth::Error, 
    Some("Network operation failed"))
  azimuth::Span::add_event(network_error_span, "exception", 
    Some([
      ("exception.type", azimuth::StringValue("TimeoutException")),
      ("exception.message", azimuth::StringValue("Read timed out")),
      ("stack.trace", azimuth::StringValue("at NetworkService.request..."))
    ]))
  errors.push(network_error_span)
  
  // 数据库错误
  let db_error_span = azimuth::Tracer::start_span(tracer, "database-operation")
  azimuth::Span::set_attribute(db_error_span, "error.type", azimuth::StringValue("DatabaseError"))
  azimuth::Span::set_attribute(db_error_span, "error.message", 
    azimuth::StringValue("Connection pool exhausted"))
  azimuth::Span::set_attribute(db_error_span, "pool.size", azimuth::IntValue(10))
  azimuth::Span::set_attribute(db_error_span, "active.connections", azimuth::IntValue(10))
  azimuth::Span::set_status(db_error_span, azimuth::Error, 
    Some("Database operation failed"))
  errors.push(db_error_span)
  
  // 业务逻辑错误
  let business_error_span = azimuth::Tracer::start_span(tracer, "business-operation")
  azimuth::Span::set_attribute(business_error_span, "error.type", azimuth::StringValue("ValidationError"))
  azimuth::Span::set_attribute(business_error_span, "error.message", 
    azimuth::StringValue("Invalid user input"))
  azimuth::Span::set_attribute(business_error_span, "field.name", azimuth::StringValue("email"))
  azimuth::Span::set_attribute(business_error_span, "field.value", azimuth::StringValue("invalid-email"))
  azimuth::Span::set_status(business_error_span, azimuth::Error, 
    Some("Validation failed"))
  errors.push(business_error_span)
  
  // 分析错误模式
  let error_analysis = azimuth::ErrorTracker::analyze_errors(error_tracker, errors)
  
  // 验证错误分析结果
  let error_types = azimuth::ErrorAnalysis::get_error_types(error_analysis)
  assert_true(azimuth::ErrorTypeDistribution::count(error_types, "NetworkTimeout") > 0)
  assert_true(azimuth::ErrorTypeDistribution::count(error_types, "DatabaseError") > 0)
  assert_true(azimuth::ErrorTypeDistribution::count(error_types, "ValidationError") > 0)
  
  // 验证错误严重性分析
  let severity_analysis = azimuth::ErrorAnalysis::get_severity_analysis(error_analysis)
  assert_true(azimuth::SeverityAnalysis::error_count(severity_analysis, azimuth::Error) >= 3)
  
  // 生成错误报告
  let error_report = azimuth::ErrorTracker::generate_error_report(error_tracker, errors)
  
  // 验证错误报告
  assert_true(azimuth::ErrorReport::contains_error_type(error_report, "NetworkTimeout"))
  assert_true(azimuth::ErrorReport::contains_error_type(error_report, "DatabaseError"))
  assert_true(azimuth::ErrorReport::contains_error_type(error_report, "ValidationError"))
  
  // 结束所有错误Span
  for error in errors {
    azimuth::Span::end(error)
  }
}

// 测试7: 自定义遥测仪表板
test "自定义遥测仪表板测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "dashboard-metrics")
  
  // 创建仪表板构建器
  let dashboard_builder = azimuth::DashboardBuilder::new()
  
  // 创建度量数据
  let request_rate = azimuth::Meter::create_counter(meter, "http.requests.rate")
  let error_rate = azimuth::Meter::create_counter(meter, "http.errors.rate")
  let response_time = azimuth::Meter::create_histogram(meter, "http.response.time")
  let active_users = azimuth::Meter::create_gauge(meter, "users.active")
  
  // 生成示例数据
  for i in 0..60 {  // 模拟60个时间点
    // 请求率
    let requests = 100.0 + (@random() * 50.0)
    azimuth::Counter::add(request_rate, requests)
    
    // 错误率
    if @random() < 0.02 {  // 2%错误率
      azimuth::Counter::add(error_rate, 1.0)
    }
    
    // 响应时间
    let resp_time = 50.0 + (@random() * 150.0)
    azimuth::Histogram::record(response_time, resp_time)
    
    // 活跃用户
    let users = 500.0 + (@random() * 200.0)
    azimuth::Gauge::record(active_users, users)
  }
  
  // 添加仪表板组件
  azimuth::DashboardBuilder::add_timeseries_chart(dashboard_builder, 
    "请求率趋势", "http.requests.rate", "requests/second")
  azimuth::DashboardBuilder::add_timeseries_chart(dashboard_builder, 
    "错误率趋势", "http.errors.rate", "errors/second")
  azimuth::DashboardBuilder::add_histogram_chart(dashboard_builder, 
    "响应时间分布", "http.response.time", "milliseconds")
  azimuth::DashboardBuilder::add_gauge_chart(dashboard_builder, 
    "活跃用户", "users.active", "users")
  
  // 添加自定义指标计算
  azimuth::DashboardBuilder::add_calculated_metric(dashboard_builder, 
    "成功率", fn(metrics) {
      let total_requests = azimuth::MetricData::get_counter_value(metrics, "http.requests.rate")
      let total_errors = azimuth::MetricData::get_counter_value(metrics, "http.errors.rate")
      if total_requests > 0.0 {
        ((total_requests - total_errors) / total_requests) * 100.0
      } else {
        100.0
      }
    }, "percent")
  
  // 构建仪表板
  let dashboard = azimuth::DashboardBuilder::build(dashboard_builder)
  
  // 验证仪表板结构
  let charts = azimuth::Dashboard::get_charts(dashboard)
  assert_eq(charts.length(), 4)  // 4个图表
  
  let calculated_metrics = azimuth::Dashboard::get_calculated_metrics(dashboard)
  assert_eq(calculated_metrics.length(), 1)  // 1个计算指标
  
  // 生成仪表板数据
  let dashboard_data = azimuth::Dashboard::generate_data(dashboard)
  
  // 验证仪表板数据
  assert_true(azimuth::DashboardData::contains_metric(dashboard_data, "http.requests.rate"))
  assert_true(azimuth::DashboardData::contains_metric(dashboard_data, "http.errors.rate"))
  assert_true(azimuth::DashboardData::contains_metric(dashboard_data, "http.response.time"))
  assert_true(azimuth::DashboardData::contains_metric(dashboard_data, "users.active"))
  assert_true(azimuth::DashboardData::contains_calculated_metric(dashboard_data, "成功率"))
  
  // 验证计算指标
  let success_rate = azimuth::DashboardData::get_calculated_metric_value(dashboard_data, "成功率")
  assert_true(success_rate >= 0.0)
  assert_true(success_rate <= 100.0)
}

// 测试8: 遥测数据的质量保证
test "遥测数据质量保证测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "quality-assurance-test")
  
  // 创建数据质量检查器
  let quality_checker = azimuth::DataQualityChecker::new()
  
  // 配置质量规则
  azimuth::DataQualityChecker::add_rule(quality_checker, 
    azimuth::QualityRule::required_attribute("service.name"))
  azimuth::DataQualityChecker::add_rule(quality_checker, 
    azimuth::QualityRule::required_attribute("operation.name"))
  azimuth::DataQualityChecker::add_rule(quality_checker, 
    azimuth::QualityRule::max_span_duration(60000))  // 最大60秒
  azimuth::DataQualityChecker::add_rule(quality_checker, 
    azimuth::QualityRule::max_attributes_count(50))  // 最多50个属性
  
  // 创建不同质量的Span
  let good_span = azimuth::Tracer::start_span(tracer, "good-quality-span")
  azimuth::Span::set_attribute(good_span, "service.name", azimuth::StringValue("user-service"))
  azimuth::Span::set_attribute(good_span, "operation.name", azimuth::StringValue("get-user"))
  azimuth::Span::set_attribute(good_span, "user.id", azimuth::StringValue("user-123"))
  
  let bad_span_missing_attrs = azimuth::Tracer::start_span(tracer, "bad-missing-attrs")
  azimuth::Span::set_attribute(bad_span_missing_attrs, "some.attr", azimuth::StringValue("some.value"))
  // 缺少必需的service.name和operation.name
  
  let bad_span_too_long = azimuth::Tracer::start_span(tracer, "bad-too-long")
  azimuth::Span::set_attribute(bad_span_too_long, "service.name", azimuth::StringValue("order-service"))
  azimuth::Span::set_attribute(bad_span_too_long, "operation.name", azimuth::StringValue("process-order"))
  // 模拟长时间运行的Span（在实际实现中需要特殊处理）
  
  let bad_span_too_many_attrs = azimuth::Tracer::start_span(tracer, "bad-too-many-attrs")
  azimuth::Span::set_attribute(bad_span_too_many_attrs, "service.name", azimuth::StringValue("payment-service"))
  azimuth::Span::set_attribute(bad_span_too_many_attrs, "operation.name", azimuth::StringValue("process-payment"))
  // 添加过多属性
  for i in 0..60 {
    azimuth::Span::set_attribute(bad_span_too_many_attrs, 
      "attr." + i.to_string(), azimuth::StringValue("value-" + i.to_string()))
  }
  
  let spans = [good_span, bad_span_missing_attrs, bad_span_too_long, bad_span_too_many_attrs]
  
  // 执行质量检查
  let quality_report = azimuth::DataQualityChecker::check_spans(quality_checker, spans)
  
  // 验证质量报告
  let total_spans = azimuth::QualityReport::total_spans(quality_report)
  assert_eq(total_spans, 4)
  
  let passed_spans = azimuth::QualityReport::passed_spans(quality_report)
  assert_eq(passed_spans, 1)  // 只有good_span应该通过
  
  let failed_spans = azimuth::QualityReport::failed_spans(quality_report)
  assert_eq(failed_spans, 3)  // 其他3个应该失败
  
  // 验证具体的质量问题
  let missing_attr_issues = azimuth::QualityReport::get_issues_by_type(quality_report, "missing_attribute")
  assert_true(missing_attr_issues.length() > 0)
  
  let too_long_issues = azimuth::QualityReport::get_issues_by_type(quality_report, "span_too_long")
  assert_true(too_long_issues.length() > 0)
  
  let too_many_attrs_issues = azimuth::QualityReport::get_issues_by_type(quality_report, "too_many_attributes")
  assert_true(too_many_attrs_issues.length() > 0)
  
  // 结束所有Span
  for span in spans {
    azimuth::Span::end(span)
  }
}

// 测试9: 多环境遥测配置
test "多环境遥测配置测试" {
  // 创建不同环境的配置
  let dev_config = azimuth::EnvironmentConfig::new("development")
  azimuth::EnvironmentConfig::set_sampling_rate(dev_config, 1.0)  // 开发环境100%采样
  azimuth::EnvironmentConfig::set_export_interval(dev_config, 5000)  // 5秒导出间隔
  azimuth::EnvironmentConfig::enable_debug_mode(dev_config, true)
  
  let staging_config = azimuth::EnvironmentConfig::new("staging")
  azimuth::EnvironmentConfig::set_sampling_rate(staging_config, 0.5)  // 预发布环境50%采样
  azimuth::EnvironmentConfig::set_export_interval(staging_config, 10000)  // 10秒导出间隔
  azimuth::EnvironmentConfig::enable_debug_mode(staging_config, false)
  
  let prod_config = azimuth::EnvironmentConfig::new("production")
  azimuth::EnvironmentConfig::set_sampling_rate(prod_config, 0.1)  // 生产环境10%采样
  azimuth::EnvironmentConfig::set_export_interval(prod_config, 30000)  // 30秒导出间隔
  azimuth::EnvironmentConfig::enable_debug_mode(prod_config, false)
  
  // 创建多环境配置管理器
  let config_manager = azimuth::MultiEnvironmentConfigManager::new()
  azimuth::MultiEnvironmentConfigManager::add_config(config_manager, "dev", dev_config)
  azimuth::MultiEnvironmentConfigManager::add_config(config_manager, "staging", staging_config)
  azimuth::MultiEnvironmentConfigManager::add_config(config_manager, "prod", prod_config)
  
  // 测试不同环境的配置
  let environments = ["dev", "staging", "prod"]
  let telemetry_data = []
  
  for env in environments {
    let config = azimuth::MultiEnvironmentConfigManager::get_config(config_manager, env)
    let env_name = azimuth::EnvironmentConfig::environment_name(config)
    
    // 创建环境特定的追踪器
    let tracer_provider = azimuth::TracerProvider::with_config(config)
    let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, env + "-service")
    
    // 创建测试Span
    let span = azimuth::Tracer::start_span(tracer, env + "-operation")
    azimuth::Span::set_attribute(span, "environment", azimuth::StringValue(env_name))
    azimuth::Span::set_attribute(span, "sampling.rate", 
      azimuth::FloatValue(azimuth::EnvironmentConfig::sampling_rate(config)))
    azimuth::Span::set_attribute(span, "export.interval", 
      azimuth::IntValue(azimuth::EnvironmentConfig::export_interval(config)))
    
    telemetry_data.push(span)
  }
  
  // 验证不同环境的配置差异
  let dev_span = telemetry_data[0]
  let staging_span = telemetry_data[1]
  let prod_span = telemetry_data[2]
  
  let dev_attrs = azimuth::Span::attributes(dev_span)
  let staging_attrs = azimuth::Span::attributes(staging_span)
  let prod_attrs = azimuth::Span::attributes(prod_span)
  
  // 验证采样率
  assert_eq(azimuth::Attributes::get(dev_attrs, "sampling.rate"), 
    Some(azimuth::FloatValue(1.0)))
  assert_eq(azimuth::Attributes::get(staging_attrs, "sampling.rate"), 
    Some(azimuth::FloatValue(0.5)))
  assert_eq(azimuth::Attributes::get(prod_attrs, "sampling.rate"), 
    Some(azimuth::FloatValue(0.1)))
  
  // 验证导出间隔
  assert_eq(azimuth::Attributes::get(dev_attrs, "export.interval"), 
    Some(azimuth::IntValue(5000)))
  assert_eq(azimuth::Attributes::get(staging_attrs, "export.interval"), 
    Some(azimuth::IntValue(10000)))
  assert_eq(azimuth::Attributes::get(prod_attrs, "export.interval"), 
    Some(azimuth::IntValue(30000)))
  
  // 结束所有Span
  for span in telemetry_data {
    azimuth::Span::end(span)
  }
}

// 测试10: 遥测数据的生命周期管理
test "遥测数据生命周期管理测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-test")
  
  // 创建生命周期管理器
  let lifecycle_manager = azimuth::TelemetryLifecycleManager::new()
  
  // 配置生命周期策略
  let lifecycle_policy = azimuth::LifecyclePolicy::new()
  azimuth::LifecyclePolicy::set_hot_data_ttl(lifecycle_policy, 3600)  // 热数据1小时
  azimuth::LifecyclePolicy::set_warm_data_ttl(lifecycle_policy, 86400)  // 温数据1天
  azimuth::LifecyclePolicy::set_cold_data_ttl(lifecycle_policy, 2592000)  // 冷数据30天
  azimuth::LifecyclePolicy::enable_auto_cleanup(lifecycle_policy, true)
  
  azimuth::TelemetryLifecycleManager::set_policy(lifecycle_manager, lifecycle_policy)
  
  // 创建不同时间的Span数据
  let current_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let hour_in_nanos = 60 * 60 * 1000000000L
  let day_in_nanos = 24 * hour_in_nanos
  
  let hot_data_spans = []
  let warm_data_spans = []
  let cold_data_spans = []
  let expired_data_spans = []
  
  // 生成热数据 (最近1小时内)
  for i in 0..10 {
    let span_time = current_time - (i.to_long() * hour_in_nanos / 10)
    let span = azimuth::Tracer::start_span_with_time(tracer, "hot-data-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("hot"))
    hot_data_spans.push(span)
  }
  
  // 生成温数据 (1-24小时前)
  for i in 0..10 {
    let span_time = current_time - ((1 + i).to_long() * hour_in_nanos)
    let span = azimuth::Tracer::start_span_with_time(tracer, "warm-data-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("warm"))
    warm_data_spans.push(span)
  }
  
  // 生成冷数据 (1-30天前)
  for i in 0..10 {
    let span_time = current_time - ((1 + i).to_long() * day_in_nanos)
    let span = azimuth::Tracer::start_span_with_time(tracer, "cold-data-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("cold"))
    cold_data_spans.push(span)
  }
  
  // 生成过期数据 (超过30天前)
  for i in 0..5 {
    let span_time = current_time - ((31 + i).to_long() * day_in_nanos)
    let span = azimuth::Tracer::start_span_with_time(tracer, "expired-data-" + i.to_string(), span_time)
    azimuth::Span::set_attribute(span, "data.tier", azimuth::StringValue("expired"))
    expired_data_spans.push(span)
  }
  
  // 执行生命周期管理
  let all_spans = hot_data_spans + warm_data_spans + cold_data_spans + expired_data_spans
  let lifecycle_result = azimuth::TelemetryLifecycleManager::process_spans(lifecycle_manager, all_spans)
  
  // 验证生命周期处理结果
  let hot_data_count = azimuth::LifecycleResult::get_data_count(lifecycle_result, "hot")
  let warm_data_count = azimuth::LifecycleResult::get_data_count(lifecycle_result, "warm")
  let cold_data_count = azimuth::LifecycleResult::get_data_count(lifecycle_result, "cold")
  let expired_data_count = azimuth::LifecycleResult::get_data_count(lifecycle_result, "expired")
  
  assert_eq(hot_data_count, 10)
  assert_eq(warm_data_count, 10)
  assert_eq(cold_data_count, 10)
  assert_eq(expired_data_count, 0)  // 过期数据应该被清理
  
  // 验证清理操作
  let cleanup_operations = azimuth::LifecycleResult::get_cleanup_operations(lifecycle_result)
  assert_true(cleanup_operations.length() >= 5)  // 至少5个过期数据被清理
  
  // 验证数据迁移
  let migration_operations = azimuth::LifecycleResult::get_migration_operations(lifecycle_result)
  assert_true(migration_operations.length() > 0)  // 应该有数据迁移操作
  
  // 结束所有Span
  for span in all_spans {
    azimuth::Span::end(span)
  }
}