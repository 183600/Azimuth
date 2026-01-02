// 遥测数据完整性和一致性测试用例
// 测试Azimuth遥测系统的数据完整性和一致性功能

test "span数据完整性验证" {
  // 测试span数据完整性验证
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "data.integrity.test")
  
  // 创建span并设置完整性校验器
  let span = Tracer::start_span(tracer, "integrity.test.span")
  Span::set_attribute(span, "operation.type", "test")
  Span::set_attribute(span, "operation.id", "op-12345")
  Span::set_attribute(span, "service.name", "integrity-test-service")
  Span::set_attribute(span, "service.version", "1.0.0")
  
  // 添加事件
  Span::add_event(span, "operation.started", [
    ("timestamp", "2025-01-02T10:00:00Z"),
    ("event.data", "operation initialization")
  ])
  
  Span::add_event(span, "operation.completed", [
    ("timestamp", "2025-01-02T10:00:05Z"),
    ("event.data", "operation finished successfully")
  ])
  
  // 设置状态
  Span::set_status(span, Ok, None)
  
  // 创建完整性校验器
  let integrity_validator = SpanIntegrityValidator::new()
  
  // 验证span完整性
  let validation_result = SpanIntegrityValidator::validate(integrity_validator, span)
  
  // 验证基本字段完整性
  assert_true(validation_result.name_valid)
  assert_true(validation_result.context_valid)
  assert_true(validation_result.attributes_valid)
  assert_true(validation_result.events_valid)
  assert_true(validation_result.status_valid)
  
  // 验证必需属性存在
  assert_true(validation_result.required_attributes.contains("operation.type"))
  assert_true(validation_result.required_attributes.contains("service.name"))
  
  // 验证时间戳一致性
  assert_true(validation_result.timestamp_consistent)
  
  // 验证事件顺序
  assert_true(validation_result.events_ordered)
  
  // 模拟数据损坏
  let corrupted_span = Tracer::start_span(tracer, "") // 空名称
  Span::set_attribute(corrupted_span, "invalid.attribute", "") // 空属性值
  
  // 添加无效事件
  Span::add_event(corrupted_span, "", []) // 空事件名称
  
  // 验证损坏的span
  let corrupted_validation = SpanIntegrityValidator::validate(integrity_validator, corrupted_span)
  assert_false(corrupted_validation.name_valid)
  assert_false(corrupted_validation.attributes_valid)
  assert_false(corrupted_validation.events_valid)
  
  // 生成完整性报告
  let integrity_report = SpanIntegrityValidator::generate_report(integrity_validator, [span, corrupted_span])
  assert_eq(integrity_report.total_spans, 2)
  assert_eq(integrity_report.valid_spans, 1)
  assert_eq(integrity_report.invalid_spans, 1)
  assert_true(integrity_report.integrity_score > 0.0)
  assert_true(integrity_report.integrity_score <= 1.0)
  
  Span::end(span)
  Span::end(corrupted_span)
  
  assert_true(true)
}

test "度量数据一致性检查" {
  // 测试度量数据一致性检查
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "data.consistency.test")
  
  // 创建一致性检查器
  let consistency_checker = MetricConsistencyChecker::new()
  
  // 设置一致性规则
  ConsistencyChecker::add_rule(consistency_checker, ConsistencyRule::new(
    "counter_monotonic", // 规则名称
    "counters", // 适用度量类型
    fn(metric) { Counter::is_monotonic(metric) }, // 验证函数
    "Counter values must be monotonic" // 错误消息
  ))
  
  ConsistencyChecker::add_rule(consistency_checker, ConsistencyRule::new(
    "histogram_buckets", // 规则名称
    "histograms", // 适用度量类型
    fn(metric) { Histogram::has_valid_buckets(metric) }, // 验证函数
    "Histogram must have valid bucket boundaries" // 错误消息
  ))
  
  // 创建计数器并记录数据
  let counter = Meter::create_counter(meter, "consistency.counter", Some("Consistency counter"), Some("count"))
  
  // 记录递增值（一致）
  Counter::add(counter, 10.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 15.0)
  
  // 验证计数器一致性
  let counter_consistency = ConsistencyChecker::check_metric(consistency_checker, counter)
  assert_true(counter_consistency.is_consistent)
  assert_eq(counter_consistency.violated_rules.length(), 0)
  
  // 创建直方图并记录数据
  let histogram = Meter::create_histogram(meter, "consistency.histogram", Some("Consistency histogram"), Some("ms"))
  
  // 记录有效的直方图数据
  Histogram::record(histogram, 10.5)
  Histogram::record(histogram, 25.7)
  Histogram::record(histogram, 45.2)
  
  // 验证直方图一致性
  let histogram_consistency = ConsistencyChecker::check_metric(consistency_checker, histogram)
  assert_true(histogram_consistency.is_consistent)
  
  // 模拟不一致的度量数据
  let inconsistent_counter = Meter::create_counter(meter, "inconsistent.counter", Some("Inconsistent counter"), Some("count"))
  
  // 手动设置非单调值（模拟不一致）
  Counter::set_value(inconsistent_counter, 50.0) // 初始值
  Counter::set_value(inconsistent_counter, 30.0) // 减少值（违反单调性）
  
  // 验证不一致的计数器
  let inconsistent_consistency = ConsistencyChecker::check_metric(consistency_checker, inconsistent_counter)
  assert_false(inconsistent_consistency.is_consistent)
  assert_true(inconsistent_consistency.violated_rules.length() > 0)
  assert_true(inconsistent_consistency.violated_rules.contains("counter_monotonic"))
  
  // 测试跨度量一致性
  let related_metrics = [counter, histogram, inconsistent_counter]
  let cross_consistency = ConsistencyChecker::check_cross_metric_consistency(consistency_checker, related_metrics)
  
  // 验证跨度量一致性报告
  assert_false(cross_consistency.all_consistent)
  assert_true(cross_consistency.consistent_metrics.contains(counter))
  assert_true(cross_consistency.consistent_metrics.contains(histogram))
  assert_true(cross_consistency.inconsistent_metrics.contains(inconsistent_counter))
  
  assert_true(true)
}

test "日志数据完整性验证" {
  // 测试日志数据完整性验证
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "data.integrity.test")
  
  // 创建日志完整性验证器
  let log_validator = LogIntegrityValidator::new()
  
  // 设置完整性规则
  LogIntegrityValidator::require_field(log_validator, "timestamp")
  LogIntegrityValidator::require_field(log_validator, "severity")
  LogIntegrityValidator::require_field(log_validator, "message")
  
  LogIntegrityValidator::set_timestamp_format(log_validator, "ISO8601")
  LogIntegrityValidator::set_message_max_length(log_validator, 10000)
  
  // 创建有效的日志记录
  let valid_log = LogRecord::new(Info, "Valid log message with proper structure")
  LogRecord::add_attribute(valid_log, "timestamp", "2025-01-02T10:00:00Z")
  LogRecord::add_attribute(valid_log, "request.id", "req-12345")
  LogRecord::add_attribute(valid_log, "user.id", "user-67890")
  LogRecord::add_attribute(valid_log, "service.name", "integrity-test-service")
  
  // 验证有效日志
  let valid_validation = LogIntegrityValidator::validate(log_validator, valid_log)
  assert_true(valid_validation.is_valid)
  assert_eq(valid_validation.missing_fields.length(), 0)
  assert_eq(valid_validation.invalid_fields.length(), 0)
  
  // 创建无效的日志记录
  let invalid_log = LogRecord::new(Info, "") // 空消息
  LogRecord::add_attribute(invalid_log, "timestamp", "invalid-timestamp") // 无效时间戳
  LogRecord::add_attribute(invalid_log, "severity", "invalid-severity") // 无效严重性
  
  // 验证无效日志
  let invalid_validation = LogIntegrityValidator::validate(log_validator, invalid_log)
  assert_false(invalid_validation.is_valid)
  assert_true(invalid_validation.missing_fields.length() > 0)
  assert_true(invalid_validation.invalid_fields.length() > 0)
  
  // 测试日志序列完整性
  let log_sequence = []
  for i in 1..=10 {
    let log = LogRecord::new(Info, "Log message " + i.to_string())
    LogRecord::add_attribute(log, "sequence.number", i)
    LogRecord::add_attribute(log, "timestamp", "2025-01-02T10:00:" + (i - 1).to_string() + "0Z")
    log_sequence = log_sequence.push(log)
  }
  
  // 验证日志序列完整性
  let sequence_validation = LogIntegrityValidator::validate_sequence(log_validator, log_sequence)
  assert_true(sequence_validation.sequence_complete)
  assert_true(sequence_validation.sequence_ordered)
  assert_eq(sequence_validation.expected_count, 10)
  assert_eq(sequence_validation.actual_count, 10)
  
  // 测试损坏的日志序列
  let corrupted_sequence = []
  for i in [1, 2, 4, 5, 7, 8, 10] { // 缺少3, 6, 9
    let log = LogRecord::new(Info, "Log message " + i.to_string())
    LogRecord::add_attribute(log, "sequence.number", i)
    corrupted_sequence = corrupted_sequence.push(log)
  }
  
  // 验证损坏的日志序列
  let corrupted_validation = LogIntegrityValidator::validate_sequence(log_validator, corrupted_sequence)
  assert_false(corrupted_validation.sequence_complete)
  assert_true(corrupted_validation.missing_sequence_numbers.length() > 0)
  assert_true(corrupted_validation.missing_sequence_numbers.contains(3))
  assert_true(corrupted_validation.missing_sequence_numbers.contains(6))
  assert_true(corrupted_validation.missing_sequence_numbers.contains(9))
  
  // 生成完整性报告
  let integrity_report = LogIntegrityValidator::generate_report(log_validator, [valid_log, invalid_log])
  assert_eq(integrity_report.total_logs, 2)
  assert_eq(integrity_report.valid_logs, 1)
  assert_eq(integrity_report.invalid_logs, 1)
  assert_true(integrity_report.integrity_score > 0.0)
  assert_true(integrity_report.integrity_score <= 1.0)
  
  assert_true(true)
}

test "跨服务数据一致性验证" {
  // 测试跨服务数据一致性验证
  let cross_service_validator = CrossServiceConsistencyValidator::new()
  
  // 模拟服务A的遥测数据
  let service_a_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service-a.operation")
  Span::set_attribute(service_a_span, "service.name", "service-a")
  Span::set_attribute(service_a_span, "trace.id", "trace-12345")
  Span::set_attribute(service_a_span, "request.id", "req-67890")
  Span::set_attribute(service_a_span, "operation.type", "process_order")
  
  // 模拟服务B的遥测数据
  let service_b_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service-b.operation")
  Span::set_attribute(service_b_span, "service.name", "service-b")
  Span::set_attribute(service_b_span, "trace.id", "trace-12345") // 相同的trace ID
  Span::set_attribute(service_b_span, "request.id", "req-67890") // 相同的request ID
  Span::set_attribute(service_b_span, "operation.type", "validate_payment")
  
  // 模拟服务C的遥测数据（不一致）
  let service_c_tracer = TracerProvider::get_tracer(TracerProvider::default(), "service-c")
  let service_c_span = Tracer::start_span(service_c_tracer, "service-c.operation")
  Span::set_attribute(service_c_span, "service.name", "service-c")
  Span::set_attribute(service_c_span, "trace.id", "trace-99999") // 不同的trace ID
  Span::set_attribute(service_c_span, "request.id", "req-67890") // 相同的request ID
  Span::set_attribute(service_c_span, "operation.type", "send_notification")
  
  // 设置跨服务一致性规则
  CrossServiceConsistencyValidator::add_rule(cross_service_validator, CrossServiceRule::new(
    "trace_consistency", // 规则名称
    fn(spans) { // 验证函数
      let trace_ids = spans.map(fn(span) { Span::get_attribute(span, "trace.id") })
      let valid_ids = trace_ids.filter(fn(id) { id != None })
      let unique_ids = valid_ids.map(fn(id) { match id { Some(v) => v, None => "" } }).unique()
      return unique_ids.length() == 1
    },
    "All spans in the same operation should have the same trace ID"
  ))
  
  CrossServiceConsistencyValidator::add_rule(cross_service_validator, CrossServiceRule::new(
    "request_consistency", // 规则名称
    fn(spans) { // 验证函数
      let request_ids = spans.map(fn(span) { Span::get_attribute(span, "request.id") })
      let valid_ids = request_ids.filter(fn(id) { id != None })
      let unique_ids = valid_ids.map(fn(id) { match id { Some(v) => v, None => "" } }).unique()
      return unique_ids.length() == 1
    },
    "All spans in the same operation should have the same request ID"
  ))
  
  // 验证服务A和服务B的一致性（应该一致）
  let consistent_spans = [service_a_span, service_b_span]
  let consistent_validation = CrossServiceConsistencyValidator::validate(cross_service_validator, consistent_spans)
  assert_true(consistent_validation.is_consistent)
  assert_eq(consistent_validation.violated_rules.length(), 0)
  
  // 验证服务A、B和C的一致性（应该不一致）
  let inconsistent_spans = [service_a_span, service_b_span, service_c_span]
  let inconsistent_validation = CrossServiceConsistencyValidator::validate(cross_service_validator, inconsistent_spans)
  assert_false(inconsistent_validation.is_consistent)
  assert_true(inconsistent_validation.violated_rules.length() > 0)
  assert_true(inconsistent_validation.violated_rules.contains("trace_consistency"))
  
  // 测试跨服务度量一致性
  let meter_provider = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(meter_provider, "service-a")
  let meter_b = MeterProvider::get_meter(meter_provider, "service-b")
  
  let counter_a = Meter::create_counter(meter_a, "operation.count", Some("Operation count"), Some("count"))
  let counter_b = Meter::create_counter(meter_b, "operation.count", Some("Operation count"), Some("count"))
  
  // 记录一致的度量
  Counter::add_with_attributes(counter_a, 1.0, [("operation", "process_order"), ("request.id", "req-67890")])
  Counter::add_with_attributes(counter_b, 1.0, [("operation", "validate_payment"), ("request.id", "req-67890")])
  
  // 验证跨服务度量一致性
  let metric_consistency = CrossServiceConsistencyValidator::validate_metrics(cross_service_validator, [counter_a, counter_b])
  assert_true(metric_consistency.is_consistent)
  
  // 生成跨服务一致性报告
  let consistency_report = CrossServiceConsistencyValidator::generate_report(cross_service_validator, inconsistent_spans)
  assert_true(consistency_report.total_services == 3)
  assert_true(consistency_report.consistent_services == 2)
  assert_true(consistency_report.inconsistent_services == 1)
  assert_true(consistency_report.consistency_score < 1.0)
  
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
  
  assert_true(true)
}

test "数据传输完整性保障" {
  // 测试数据传输完整性保障
  let integrity_guardian = DataIntegrityGuardian::new()
  
  // 设置完整性保障策略
  DataIntegrityGuardian::enable_checksum(integrity_guardian, true)
  DataIntegrityGuardian::enable_acknowledgment(integrity_guardian, true)
  DataIntegrityGuardian::set_retry_strategy(integrity_guardian, RetryStrategy::ExponentialBackoff)
  DataIntegrityGuardian::set_max_retries(integrity_guardian, 3)
  
  // 创建测试数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "transmission.test")
  
  let test_span = Tracer::start_span(tracer, "transmission.test.span")
  Span::set_attribute(test_span, "operation.type", "test")
  Span::set_attribute(test_span, "payload.size", 1024)
  Span::set_attribute(test_span, "transmission.id", "tx-12345")
  
  // 序列化数据
  let serializer = JsonSerializer::new()
  let serialized_data = serializer.serialize_span(test_span)
  
  // 添加完整性保障
  let protected_data = DataIntegrityGuardian::protect(integrity_guardian, serialized_data)
  
  // 验证数据包含完整性信息
  assert_true(protected_data.contains("checksum"))
  assert_true(protected_data.contains("timestamp"))
  assert_true(protected_data.contains("transmission_id"))
  
  // 模拟数据传输
  let transmitter = DataTransmitter::new()
  DataTransmitter::set_integrity_guardian(transmitter, integrity_guardian)
  
  // 传输数据
  let transmission_result = DataTransmitter::transmit(transmitter, protected_data, "http://telemetry.example.com/api/v1/spans")
  assert_eq(transmission_result.status, TransmissionStatus::Success)
  
  // 模拟接收端验证
  let receiver = DataReceiver::new()
  DataReceiver::set_integrity_guardian(receiver, integrity_guardian)
  
  // 接收并验证数据
  let received_data = DataReceiver::receive(receiver, transmission_result.data)
  let verification_result = DataIntegrityGuardian::verify(integrity_guardian, received_data)
  
  assert_true(verification_result.is_valid)
  assert_true(verification_result.checksum_valid)
  assert_true(verification_result.timestamp_valid)
  
  // 模拟数据损坏
  let corrupted_data = DataIntegrityGuardian::corrupt_data(integrity_guardian, protected_data, 0.1) // 10%损坏率
  let corrupted_verification = DataIntegrityGuardian::verify(integrity_guardian, corrupted_data)
  
  assert_false(corrupted_verification.is_valid)
  assert_false(corrupted_verification.checksum_valid)
  
  // 测试自动重传机制
  let retry_transmitter = DataTransmitter::new()
  DataTransmitter::set_integrity_guardian(retry_transmitter, integrity_guardian)
  DataTransmitter::enable_auto_retry(retry_transmitter, true)
  
  // 模拟传输失败
  let retry_result = DataTransmitter::transmit_with_failure(retry_transmitter, corrupted_data, "http://telemetry.example.com/api/v1/spans", 2) // 前2次失败
  assert_eq(retry_result.status, TransmissionStatus::Success)
  assert_eq(retry_result.attempt_count, 3) // 应该重试3次
  
  // 测试数据完整性报告
  let integrity_report = DataIntegrityGuardian::generate_integrity_report(integrity_guardian)
  assert_true(integrity_report.total_transmissions > 0)
  assert_true(integrity_report.successful_transmissions > 0)
  assert_true(integrity_report.failed_transmissions >= 0)
  assert_true(integrity_report.corruption_detections > 0)
  assert_true(integrity_report.integrity_score >= 0.0)
  assert_true(integrity_report.integrity_score <= 1.0)
  
  Span::end(test_span)
  
  assert_true(true)
}

test "数据一致性修复机制" {
  // 测试数据一致性修复机制
  let consistency_repairer = DataConsistencyRepairer::new()
  
  // 设置修复策略
  DataConsistencyRepairer::set_auto_repair(consistency_repairer, true)
  DataConsistencyRepairer::set_repair_strategy(consistency_repairer, RepairStrategy::BestEffort)
  DataConsistencyRepairer::set_max_repair_attempts(consistency_repairer, 3)
  
  // 创建不一致的遥测数据
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "repair.test")
  
  let inconsistent_span = Tracer::start_span(tracer, "inconsistent.span")
  Span::set_attribute(inconsistent_span, "operation.type", "test")
  Span::set_attribute(inconsistent_span, "timestamp", "invalid-timestamp") // 无效时间戳
  Span::set_attribute(inconsistent_span, "duration", -100) // 负持续时间
  
  // 添加无序事件
  Span::add_event(inconsistent_span, "event.completed", [("timestamp", "2025-01-02T10:00:10Z")])
  Span::add_event(inconsistent_span, "event.started", [("timestamp", "2025-01-02T10:00:00Z")]) // 顺序错误
  
  // 检测不一致性
  let inconsistency_detector = InconsistencyDetector::new()
  let detected_issues = InconsistencyDetector::detect(inconsistency_detector, inconsistent_span)
  
  assert_true(detected_issues.length() > 0)
  assert_true(detected_issues.any(fn(issue) { issue.type == "invalid_timestamp" }))
  assert_true(detected_issues.any(fn(issue) { issue.type == "negative_duration" }))
  assert_true(detected_issues.any(fn(issue) { issue.type == "unordered_events" }))
  
  // 修复不一致性
  let repair_result = DataConsistencyRepairer::repair(consistency_repairer, inconsistent_span, detected_issues)
  
  assert_true(repair_result.success)
  assert_true(repair_result.repaired_issues.length() > 0)
  
  // 验证修复后的数据
  let repaired_span = repair_result.repaired_data
  let repaired_validation = InconsistencyDetector::detect(inconsistency_detector, repaired_span)
  
  assert_true(repaired_validation.length() == 0) // 所有问题应该已修复
  
  // 验证具体修复
  assert_true(Span::get_attribute(repaired_span, "timestamp") != Some("invalid-timestamp"))
  assert_true(Span::get_attribute(repaired_span, "duration") != Some(-100))
  
  // 验证事件顺序已修复
  let events = Span::get_events(repaired_span)
  if events.length() >= 2 {
    let first_event = events[0]
    let second_event = events[1]
    assert_true(Event::get_name(first_event) == "event.started")
    assert_true(Event::get_name(second_event) == "event.completed")
  }
  
  // 测试度量数据修复
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "repair.test")
  
  let inconsistent_counter = Meter::create_counter(meter, "inconsistent.counter", Some("Inconsistent counter"), Some("count"))
  
  // 手动设置不一致的值
  Counter::set_value(inconsistent_counter, 100.0)
  Counter::set_value(inconsistent_counter, 50.0) // 违反单调性
  
  // 检测度量不一致性
  let metric_issues = InconsistencyDetector::detect_metric(inconsistency_detector, inconsistent_counter)
  assert_true(metric_issues.length() > 0)
  
  // 修复度量不一致性
  let metric_repair_result = DataConsistencyRepairer::repair_metric(consistency_repairer, inconsistent_counter, metric_issues)
  assert_true(metric_repair_result.success)
  
  // 验证修复后的度量
  let repaired_counter = metric_repair_result.repaired_metric
  let repaired_metric_issues = InconsistencyDetector::detect_metric(inconsistency_detector, repaired_counter)
  assert_true(repaired_metric_issues.length() == 0)
  
  // 测试批量修复
  let batch_inconsistencies = [
    (inconsistent_span, detected_issues),
    (inconsistent_counter, metric_issues)
  ]
  
  let batch_repair_result = DataConsistencyRepairer::repair_batch(consistency_repairer, batch_inconsistencies)
  assert_true(batch_repair_result.success)
  assert_eq(batch_repair_result.repaired_items.length(), 2)
  
  // 生成修复报告
  let repair_report = DataConsistencyRepairer::generate_repair_report(consistency_repairer)
  assert_true(repair_report.total_inconsistencies > 0)
  assert_true(repair_report.successful_repairs > 0)
  assert_true(repair_report.failed_repairs >= 0)
  assert_true(repair_report.repair_rate > 0.0)
  
  Span::end(inconsistent_span)
  Span::end(repaired_span)
  
  assert_true(true)
}