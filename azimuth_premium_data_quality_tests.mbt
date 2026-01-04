// Azimuth 高级数据质量验证测试用例
// 专注于验证遥测数据的完整性、一致性和准确性

// 测试1: 数据完整性验证
test "遥测数据完整性验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.integrity")
  
  // 创建数据完整性验证器
  let integrity_validator = DataIntegrityValidator::new()
  
  // 模拟完整的遥测数据流
  let trace_id = TraceId::generate()
  let span_id = SpanId::generate()
  
  // 创建带有完整性校验的span
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "integrity.test")
  let span = Tracer::start_span_with_trace_id(tracer, "data.integrity.test", trace_id, span_id)
  
  // 添加必需的属性
  Span::set_attribute(span, "service.name", StringValue("azimuth.test"))
  Span::set_attribute(span, "service.version", StringValue("1.0.0"))
  Span::set_attribute(span, "service.instance.id", StringValue("instance-001"))
  
  // 添加事件
  Span::add_event_with_timestamp(span, "operation.started", Timestamp::now(), [
    ("operation.type", StringValue("data.validation")),
    ("operation.id", StringValue("op-001"))
  ])
  
  // 模拟数据处理
  let data_points = [
    ("metric.cpu", 45.2, Timestamp::now()),
    ("metric.memory", 1024.0, Timestamp::now()),
    ("metric.disk", 85.6, Timestamp::now())
  ]
  
  for (name, value, timestamp) in data_points {
    Span::add_event_with_timestamp(span, "metric.recorded", timestamp, [
      ("metric.name", StringValue(name)),
      ("metric.value", DoubleValue(value)),
      ("metric.timestamp", IntValue(timestamp.to_millis()))
    ])
  }
  
  // 完成span
  Span::set_status(span, Ok)
  Span::end(span)
  
  // 验证数据完整性
  let integrity_check = DataIntegrityValidator::validate_span(integrity_validator, span)
  assert_true(integrity_check.is_complete)
  assert_true(integrity_check.has_trace_id)
  assert_true(integrity_check.has_span_id)
  assert_true(integrity_check.has_timestamp)
  assert_true(integrity_check.has_service_attributes)
  
  // 测试不完整数据
  let incomplete_span = Tracer::start_span(tracer, "incomplete.data.test")
  // 故意不添加必需属性
  
  let incomplete_check = DataIntegrityValidator::validate_span(integrity_validator, incomplete_span)
  assert_false(incomplete_check.is_complete)
  assert_eq(incomplete_check.missing_fields.length(), 3) // 缺少service属性
  
  Span::end(incomplete_span)
}

// 测试2: 数据一致性验证
test "遥测数据一致性验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.consistency")
  
  // 创建数据一致性验证器
  let consistency_validator = DataConsistencyValidator::new()
  
  // 模拟相关的遥测数据
  let trace_id = TraceId::generate()
  
  // 创建父span
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "consistency.test")
  let parent_span = Tracer::start_span_with_trace_id(tracer, "parent.operation", trace_id, SpanId::generate())
  Span::set_attribute(parent_span, "operation.type", StringValue("parent"))
  Span::set_attribute(parent_span, "operation.id", StringValue("op-parent-001"))
  
  // 创建子span
  let child_span1 = Tracer::start_span_with_context(tracer, "child.operation.1", Span::context(parent_span))
  Span::set_attribute(child_span1, "operation.type", StringValue("child"))
  Span::set_attribute(child_span1, "operation.id", StringValue("op-child-001"))
  Span::set_attribute(child_span1, "parent.operation.id", StringValue("op-parent-001"))
  
  let child_span2 = Tracer::start_span_with_context(tracer, "child.operation.2", Span::context(parent_span))
  Span::set_attribute(child_span2, "operation.type", StringValue("child"))
  Span::set_attribute(child_span2, "operation.id", StringValue("op-child-002"))
  Span::set_attribute(child_span2, "parent.operation.id", StringValue("op-parent-001"))
  
  // 结束span（注意顺序）
  Span::end(child_span1)
  Span::end(child_span2)
  Span::end(parent_span)
  
  // 验证span层次结构一致性
  let hierarchy_check = DataConsistencyValidator::validate_span_hierarchy(consistency_validator, [
    parent_span, child_span1, child_span2
  ])
  
  assert_true(hierarchy_check.is_valid_hierarchy)
  assert_eq(hierarchy_check.root_span_id, Span::span_id(parent_span))
  assert_eq(hierarchy_check.child_spans.length(), 2)
  assert_true(hierarchy_check.all_spans_same_trace)
  
  // 验证时间顺序一致性
  let temporal_check = DataConsistencyValidator::validate_temporal_consistency(consistency_validator, [
    parent_span, child_span1, child_span2
  ])
  
  assert_true(temporal_check.is_valid_sequence)
  assert_true(temporal_check.parent_starts_before_children)
  assert_true(temporal_check.parent_ends_after_children)
  
  // 测试不一致的数据
  let inconsistent_span = Tracer::start_span_with_trace_id(tracer, "inconsistent.operation", TraceId::generate(), SpanId::generate())
  Span::set_attribute(inconsistent_span, "operation.type", StringValue("orphan"))
  Span::set_attribute(inconsistent_span, "parent.operation.id", StringValue("non-existent-parent"))
  Span::end(inconsistent_span)
  
  let orphan_check = DataConsistencyValidator::validate_span_hierarchy(consistency_validator, [
    parent_span, child_span1, child_span2, inconsistent_span
  ])
  
  assert_true(orphan_check.has_orphan_spans)
  assert_eq(orphan_check.orphan_span_count, 1)
}

// 测试3: 数据准确性验证
test "遥测数据准确性验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.accuracy")
  
  // 创建数据准确性验证器
  let accuracy_validator = DataAccuracyValidator::new()
  
  // 创建已知准确性的基准数据
  let基准指标 = Meter::create_counter(meter, "benchmark.requests.total")
  let基准直方图 = Meter::create_histogram(meter, "benchmark.response.duration")
  let基准仪表 = Meter::create_gauge(meter, "benchmark.cpu.utilization")
  
  // 记录准确的基准数据
  let request_count = 100
  for i in 1..=request_count {
    Counter::add(基准指标, 1.0)
    
    // 模拟不同响应时间
    let response_time = if i % 4 == 0 { 0.1 } else if i % 2 == 0 { 0.05 } else { 0.02 }
    Histogram::record(基准直方图, response_time)
    
    // 模拟CPU使用率变化
    let cpu_usage = 20.0 + (i.to_float() * 0.5)
    Gauge::set(基准仪表, cpu_usage)
  }
  
  // 创建测试数据收集器
  let test_collector = TestDataCollector::new()
  
  // 收集基准数据
  let collected_metrics = TestDataCollector::collect_metrics(test_collector, meter)
  
  // 验证计数器准确性
  let counter_accuracy = DataAccuracyValidator::validate_counter(accuracy_validator, 基准指标, request_count.to_float())
  assert_true(counter_accuracy.is_accurate)
  assert_eq(counter_accuracy.expected_value, request_count.to_float())
  assert_eq(counter_accuracy.actual_value, request_count.to_float())
  assert_eq(counter_accuracy.accuracy_percentage, 100.0)
  
  // 验证直方图准确性
  let histogram_accuracy = DataAccuracyValidator::validate_histogram(accuracy_validator, 基准直方图)
  assert_true(histogram_accuracy.is_accurate)
  assert_eq(histogram_accuracy.sample_count, request_count.to_float())
  assert_true(histogram_accuracy.min_value >= 0.0)
  assert_true(histogram_accuracy.max_value <= 0.1)
  assert_true(histogram_accuracy.mean_value > 0.02 && histogram_accuracy.mean_value < 0.1)
  
  // 验证仪表准确性
  let gauge_accuracy = DataAccuracyValidator::validate_gauge(accuracy_validator, 基准仪表)
  assert_true(gauge_accuracy.is_accurate)
  assert_true(gauge_accuracy.current_value >= 20.0)
  assert_true(gauge_accuracy.current_value <= 70.0)
  
  // 测试数据范围验证
  let range_validator = DataRangeValidator::new()
  DataRangeValidator::add_range(range_validator, "benchmark.response.duration", 0.0, 1.0)
  DataRangeValidator::add_range(range_validator, "benchmark.cpu.utilization", 0.0, 100.0)
  
  let range_check = DataRangeValidator::validate_all_metrics(range_validator, collected_metrics)
  assert_true(range_check.all_in_range)
  assert_eq(range_check.out_of_range_count, 0)
  
  // 测试超出范围的数据
  let invalid_metric = Meter::create_gauge(meter, "invalid.metric")
  Gauge::set(invalid_metric, 150.0) // 超出CPU使用率范围
  
  let invalid_collected = TestDataCollector::collect_metrics(test_collector, meter)
  let invalid_range_check = DataRangeValidator::validate_all_metrics(range_validator, invalid_collected)
  
  assert_false(invalid_range_check.all_in_range)
  assert_true(invalid_range_check.out_of_range_count > 0)
}

// 测试4: 数据时效性验证
test "遥测数据时效性验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.timeliness")
  
  // 创建数据时效性验证器
  let timeliness_validator = DataTimelinessValidator::new()
  
  // 设置时效性要求
  DataTimelinessValidator::set_max_delay(timeliness_validator, 5000) // 5秒最大延迟
  DataTimelinessValidator::set_max_age(timeliness_validator, 60000) // 60秒最大数据年龄
  
  // 创建实时数据流
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "timeliness.test")
  
  // 记录实时事件
  let start_time = Timestamp::now()
  let realtime_span = Tracer::start_span(tracer, "realtime.operation")
  
  // 模拟实时数据处理
  Span::add_event_with_timestamp(realtime_span, "data.received", start_time, [])
  Span::add_event_with_timestamp(realtime_span, "data.processed", Timestamp::now(), [])
  
  // 计算处理延迟
  let processing_delay = Timestamp::now().to_millis() - start_time.to_millis()
  Span::set_attribute(realtime_span, "processing.delay.ms", IntValue(processing_delay))
  
  Span::end(realtime_span)
  
  // 验证实时数据时效性
  let realtime_check = DataTimelinessValidator::validate_realtime_data(timeliness_validator, realtime_span)
  assert_true(realtime_check.is_within_delay_threshold)
  assert_true(realtime_check.processing_delay_ms < 5000)
  assert_true(realtime_check.data_age_ms < 60000)
  
  // 测试历史数据
  let historical_timestamp = Timestamp::now() - Duration::from_seconds(120) // 2分钟前
  let historical_span = Tracer::start_span_with_timestamp(tracer, "historical.operation", historical_timestamp)
  Span::end(historical_span)
  
  let historical_check = DataTimelinessValidator::validate_historical_data(timeliness_validator, historical_span)
  assert_false(historical_check.is_within_age_threshold) // 超过最大年龄限制
  assert_true(historical_check.data_age_ms > 60000)
  
  // 测试批处理数据时效性
  let batch_start = Timestamp::now()
  let batch_events = []
  
  for i in 1..=10 {
    let event_time = Timestamp::now()
    batch_events = batch_events.push(("batch.event." + i.to_string(), event_time))
  }
  
  let batch_end = Timestamp::now()
  let batch_processing_time = batch_end.to_millis() - batch_start.to_millis()
  
  // 验证批处理时效性
  let batch_check = DataTimelinessValidator::validate_batch_processing(
    timeliness_validator, 
    batch_events, 
    batch_start, 
    batch_end
  )
  
  assert_true(batch_check.is_within_processing_window)
  assert_true(batch_check.total_processing_time_ms < 10000) // 假设批处理窗口为10秒
  assert_eq(batch_check.event_count, 10)
  assert_true(batch_check.average_event_delay_ms >= 0)
}

// 测试5: 数据格式和模式验证
test "遥测数据格式和模式验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.format")
  
  // 创建数据格式验证器
  let format_validator = DataFormatValidator::new()
  
  // 定义遥测数据模式
  let telemetry_schema = TelemetrySchema::new("azimuth.standard.v1")
  
  // 添加必需的属性
  TelemetrySchema::add_required_attribute(telemetry_schema, "service.name", "string")
  TelemetrySchema::add_required_attribute(telemetry_schema, "service.version", "string")
  TelemetrySchema::add_optional_attribute(telemetry_schema, "service.instance.id", "string")
  
  // 添加事件模式
  TelemetrySchema::add_event_pattern(telemetry_schema, "request.received", [
    ("request.id", "string"),
    ("request.method", "string"),
    ("request.url", "string")
  ])
  
  TelemetrySchema::add_event_pattern(telemetry_schema, "request.completed", [
    ("request.id", "string"),
    ("response.status", "int"),
    ("response.duration_ms", "int")
  ])
  
  // 创建符合模式的span
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "format.test")
  let compliant_span = Tracer::start_span(tracer, "compliant.operation")
  
  // 添加必需属性
  Span::set_attribute(compliant_span, "service.name", StringValue("azimuth.api"))
  Span::set_attribute(compliant_span, "service.version", StringValue("2.1.0"))
  Span::set_attribute(compliant_span, "service.instance.id", StringValue("api-server-003"))
  
  // 添加符合模式的事件
  let request_id = "req-12345"
  Span::add_event_with_attributes(compliant_span, "request.received", [
    ("request.id", StringValue(request_id)),
    ("request.method", StringValue("GET")),
    ("request.url", StringValue("/api/v1/telemetry"))
  ])
  
  Span::add_event_with_attributes(compliant_span, "request.completed", [
    ("request.id", StringValue(request_id)),
    ("response.status", IntValue(200)),
    ("response.duration_ms", IntValue(45))
  ])
  
  Span::end(compliant_span)
  
  // 验证符合模式的数据
  let compliant_check = DataFormatValidator::validate_against_schema(format_validator, compliant_span, telemetry_schema)
  assert_true(compliant_check.is_compliant)
  assert_eq(compliant_check.missing_attributes.length(), 0)
  assert_eq(compliant_check.invalid_events.length(), 0)
  assert_true(compliant_check.all_required_attributes_present)
  
  // 创建不符合模式的span
  let non_compliant_span = Tracer::start_span(tracer, "non.compliant.operation")
  
  // 缺少必需属性
  Span::set_attribute(non_compliant_span, "service.name", StringValue("azimuth.api"))
  // 缺少 service.version
  
  // 添加不符合模式的事件
  Span::add_event_with_attributes(non_compliant_span, "request.received", [
    ("request.id", StringValue("req-67890")),
    ("request.method", StringValue("POST")),
    // 缺少 request.url
    ("invalid.field", StringValue("should.not.exist"))
  ])
  
  Span::end(non_compliant_span)
  
  // 验证不符合模式的数据
  let non_compliant_check = DataFormatValidator::validate_against_schema(format_validator, non_compliant_span, telemetry_schema)
  assert_false(non_compliant_check.is_compliant)
  assert_true(non_compliant_check.missing_attributes.length() > 0)
  assert_true(non_compliant_check.invalid_events.length() > 0)
  assert_false(non_compliant_check.all_required_attributes_present)
  
  // 测试数据类型验证
  let type_validator = DataTypeValidator::new()
  DataTypeValidator::add_type_rule(type_validator, "response.status", "int")
  DataTypeValidator::add_type_rule(type_validator, "service.name", "string")
  DataTypeValidator::add_type_rule(type_validator, "cpu.usage", "double")
  DataTypeValidator::add_type_rule(type_validator, "is.enabled", "bool")
  
  // 创建类型正确的span
  let type_correct_span = Tracer::start_span(tracer, "type.correct.operation")
  Span::set_attribute(type_correct_span, "response.status", IntValue(200))
  Span::set_attribute(type_correct_span, "service.name", StringValue("test.service"))
  Span::set_attribute(type_correct_span, "cpu.usage", DoubleValue(45.7))
  Span::set_attribute(type_correct_span, "is.enabled", BoolValue(true))
  Span::end(type_correct_span)
  
  let type_check = DataTypeValidator::validate_span_types(type_validator, type_correct_span)
  assert_true(type_check.all_types_correct)
  assert_eq(type_check.type_errors.length(), 0)
  
  // 创建类型错误的span
  let type_error_span = Tracer::start_span(tracer, "type.error.operation")
  Span::set_attribute(type_error_span, "response.status", StringValue("200")) // 应该是int
  Span::set_attribute(type_error_span, "cpu.usage", StringValue("45.7"))   // 应该是double
  Span::end(type_error_span)
  
  let type_error_check = DataTypeValidator::validate_span_types(type_validator, type_error_span)
  assert_false(type_error_check.all_types_correct)
  assert_true(type_error_check.type_errors.length() > 0)
}

// 测试6: 数据关联性和依赖验证
test "遥测数据关联性和依赖验证测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.correlation")
  
  // 创建数据关联性验证器
  let correlation_validator = DataCorrelationValidator::new()
  
  // 定义数据关联规则
  DataCorrelationValidator::add_correlation_rule(correlation_validator, "request.id", [
    "request.received", "request.processed", "request.completed"
  ])
  
  DataCorrelationValidator::add_dependency_rule(correlation_validator, "database.connection", [
    "database.query.started", "database.query.completed"
  ])
  
  // 创建有关联性的追踪链
  let tracer = TracerProvider::get_tracer(TracerProvider::default(), "correlation.test")
  let main_span = Tracer::start_span(tracer, "request.processing")
  
  let request_id = "req-98765"
  Span::set_attribute(main_span, "request.id", StringValue(request_id))
  
  // 开始数据库操作
  let db_span = Tracer::start_span_with_context(tracer, "database.operation", Span::context(main_span))
  Span::set_attribute(db_span, "request.id", StringValue(request_id))
  Span::set_attribute(db_span, "database.connection", StringValue("conn-001"))
  
  Span::add_event_with_attributes(db_span, "database.query.started", [
    ("request.id", StringValue(request_id)),
    ("query.type", StringValue("SELECT"))
  ])
  
  // 模拟查询执行
  Span::add_event_with_attributes(db_span, "database.query.completed", [
    ("request.id", StringValue(request_id)),
    ("rows.affected", IntValue(5))
  ])
  
  Span::end(db_span)
  Span::end(main_span)
  
  // 验证数据关联性
  let correlation_check = DataCorrelationValidator::validate_trace_correlation(correlation_validator, [main_span, db_span])
  assert_true(correlation_check.has_valid_correlations)
  assert_true(correlation_check.all_request_ids_consistent)
  assert_eq(correlation_check.correlated_spans.length(), 2)
  
  // 验证依赖关系
  let dependency_check = DataCorrelationValidator::validate_dependencies(correlation_validator, [main_span, db_span])
  assert_true(dependency_check.all_dependencies_satisfied)
  assert_true(dependency_check.database_operations_properly_sequenced)
  
  // 测试缺少关联性的数据
  let orphan_span = Tracer::start_span(tracer, "orphan.operation")
  Span::set_attribute(orphan_span, "operation.id", StringValue("op-orphan"))
  // 缺少 request.id
  Span::end(orphan_span)
  
  let orphan_check = DataCorrelationValidator::validate_trace_correlation(correlation_validator, [main_span, db_span, orphan_span])
  assert_false(orphan_check.all_spans_correlated)
  assert_true(orphan_check.uncorrelated_span_count > 0)
  
  // 测试跨服务关联性
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service.a")
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service.b")
  
  let trace_id = TraceId::generate()
  let service_a_span = Tracer::start_span_with_trace_id(service_a_tracer, "service.a.operation", trace_id, SpanId::generate())
  Span::set_attribute(service_a_span, "request.id", StringValue("cross-service-001"))
  Span::set_attribute(service_a_span, "service.name", StringValue("service.a"))
  
  let service_b_span = Tracer::start_span_with_trace_id(service_b_tracer, "service.b.operation", trace_id, SpanId::generate())
  Span::set_attribute(service_b_span, "request.id", StringValue("cross-service-001"))
  Span::set_attribute(service_b_span, "service.name", StringValue("service.b"))
  Span::set_attribute(service_b_span, "parent.service", StringValue("service.a"))
  
  Span::end(service_a_span)
  Span::end(service_b_span)
  
  let cross_service_check = DataCorrelationValidator::validate_cross_service_correlation(correlation_validator, [service_a_span, service_b_span])
  assert_true(cross_service_check.services_properly_correlated)
  assert_true(cross_service_check.same_trace_id)
  assert_true(cross_service_check.same_request_id)
  assert_eq(cross_service_check.involved_services.length(), 2)
}

// 测试7: 数据质量评分和报告
test "数据质量评分和报告测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "data.quality.scoring")
  
  // 创建数据质量评分器
  let quality_scorer = DataQualityScorer::new()
  
  // 设置评分权重
  DataQualityScorer::set_weight(quality_scorer, "completeness", 0.25)
  DataQualityScorer::set_weight(quality_scorer, "consistency", 0.20)
  DataQualityScorer::set_weight(quality_scorer, "accuracy", 0.25)
  DataQualityScorer::set_weight(quality_scorer, "timeliness", 0.15)
  DataQualityScorer::set_weight(quality_scorer, "format_compliance", 0.15)
  
  // 创建测试数据集
  let test_dataset = TestDataGenerator::generate_comprehensive_dataset()
  
  // 评估各个质量维度
  let completeness_score = DataQualityScorer::evaluate_completeness(quality_scorer, test_dataset)
  let consistency_score = DataQualityScorer::evaluate_consistency(quality_scorer, test_dataset)
  let accuracy_score = DataQualityScorer::evaluate_accuracy(quality_scorer, test_dataset)
  let timeliness_score = DataQualityScorer::evaluate_timeliness(quality_scorer, test_dataset)
  let format_score = DataQualityScorer::evaluate_format_compliance(quality_scorer, test_dataset)
  
  // 验证评分范围
  assert_true(completeness_score >= 0.0 && completeness_score <= 100.0)
  assert_true(consistency_score >= 0.0 && consistency_score <= 100.0)
  assert_true(accuracy_score >= 0.0 && accuracy_score <= 100.0)
  assert_true(timeliness_score >= 0.0 && timeliness_score <= 100.0)
  assert_true(format_score >= 0.0 && format_score <= 100.0)
  
  // 计算综合质量评分
  let overall_score = DataQualityScorer::calculate_overall_score(quality_scorer, [
    ("completeness", completeness_score),
    ("consistency", consistency_score),
    ("accuracy", accuracy_score),
    ("timeliness", timeliness_score),
    ("format_compliance", format_score)
  ])
  
  // 验证综合评分
  assert_true(overall_score >= 0.0 && overall_score <= 100.0)
  
  // 生成质量报告
  let quality_report = DataQualityReporter::generate_report(quality_scorer, test_dataset)
  
  // 验证报告内容
  assert_true(quality_report.overall_score >= 0.0)
  assert_true(quality_report.dimension_scores.length() >= 5)
  assert_true(quality_report.issues_identified.length() >= 0)
  assert_true(quality_report.recommendations.length() >= 0)
  
  // 测试质量趋势分析
  let historical_scores = [
    (Timestamp::now() - Duration::from_days(7), 85.5),
    (Timestamp::now() - Duration::from_days(6), 87.2),
    (Timestamp::now() - Duration::from_days(5), 86.8),
    (Timestamp::now() - Duration::from_days(4), 89.1),
    (Timestamp::now() - Duration::from_days(3), 90.3),
    (Timestamp::now() - Duration::from_days(2), 88.7),
    (Timestamp::now() - Duration::from_days(1), 91.2),
    (Timestamp::now(), overall_score)
  ]
  
  let trend_analysis = DataQualityReporter::analyze_trend(quality_report, historical_scores)
  
  // 验证趋势分析
  assert_true(trend_analysis.trend_direction == "improving" || trend_analysis.trend_direction == "stable" || trend_analysis.trend_direction == "declining")
  assert_true(trend_analysis.average_score >= 0.0)
  assert_true(trend_analysis.score_variance >= 0.0)
  
  // 创建质量警报
  let alert_manager = DataQualityAlertManager::new()
  DataQualityAlertManager::set_threshold(alert_manager, "warning", 80.0)
  DataQualityAlertManager::set_threshold(alert_manager, "critical", 70.0)
  
  // 测试警报触发
  let alert_status = DataQualityAlertManager::evaluate_score(alert_manager, overall_score)
  
  if overall_score >= 80.0 {
    assert_eq(alert_status, "normal")
  } else if overall_score >= 70.0 {
    assert_eq(alert_status, "warning")
  } else {
    assert_eq(alert_status, "critical")
  }
  
  // 测试质量改进建议
  let recommendations = DataQualityReporter::generate_recommendations(quality_report)
  assert_true(recommendations.length() > 0)
  
  // 验证建议的相关性
  for recommendation in recommendations {
    assert_true(recommendation.priority == "high" || recommendation.priority == "medium" || recommendation.priority == "low")
    assert_true(recommendation.description.length() > 0)
    assert_true(recommendation.expected_impact >= 0.0)
  }
}