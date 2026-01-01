// Azimuth Telemetry Data Quality Assurance Tests
// 遥测数据质量保证测试用例
// 测试遥测数据的完整性、一致性、准确性和可靠性

import "azimuth/azimuth"

// Test 1: 遥测数据完整性验证测试
pub test "遥测数据完整性验证测试" {
  // 创建数据质量监控器
  let quality_monitor = azimuth::DataQualityMonitor::new()
  
  // 配置完整性检查规则
  azimuth::DataQualityMonitor::add_integrity_rule(quality_monitor, {
    "name": "span.trace.id.required",
    "description": "Span必须有有效的trace ID",
    "check": fn(data) { data.has("trace_id") && data["trace_id"] != "" },
    "severity": "error"
  })
  
  azimuth::DataQualityMonitor::add_integrity_rule(quality_monitor, {
    "name": "span.timestamp.valid",
    "description": "Span时间戳必须在合理范围内",
    "check": fn(data) { 
      let timestamp = data["timestamp"]
      let now = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      timestamp > 0L && timestamp <= now + 60000000000L  // 允许1分钟的未来时间
    },
    "severity": "warning"
  })
  
  azimuth::DataQualityMonitor::add_integrity_rule(quality_monitor, {
    "name": "metric.value.numeric",
    "description": "度量值必须是数值",
    "check": fn(data) { 
      match data["value"] {
        Float(_) => true,
        Int(_) => true,
        _ => false
      }
    },
    "severity": "error"
  })
  
  // 创建测试数据
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "quality-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "quality-test")
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "quality-test")
  
  // 生成高质量的测试数据
  let valid_spans = []
  let valid_metrics = []
  let valid_logs = []
  
  for i in 0..100 {
    // 创建有效的Span
    let span = azimuth::Tracer::start_span(tracer, "quality-span-" + i.to_string())
    azimuth::Span::add_event(span, "quality.test.event", Some([
      ("event.index", azimuth::IntValue(i)),
      ("test.type", azimuth::StringValue("integrity"))
    ]))
    azimuth::Span::set_status(span, azimuth::Ok)
    
    let span_data = {
      "trace_id": azimuth::SpanContext::trace_id(azimuth::Span::span_context(span)),
      "span_id": azimuth::SpanContext::span_id(azimuth::Span::span_context(span)),
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      "name": azimuth::Span::name(span),
      "status": "ok"
    }
    
    valid_spans.push(span_data)
    
    // 创建有效的度量
    let counter = azimuth::Meter::create_counter(meter, "quality.counter")
    azimuth::Counter::add(counter, i.to_double())
    
    let metric_data = {
      "name": "quality.counter",
      "value": i.to_double(),
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      "type": "counter"
    }
    
    valid_metrics.push(metric_data)
    
    // 创建有效的日志
    let log_record = azimuth::LogRecord::new(azimuth::Info, "Quality test log " + i.to_string())
    azimuth::Logger::emit(logger, log_record)
    
    let log_data = {
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      "severity": "info",
      "message": "Quality test log " + i.to_string(),
      "logger": "quality-test"
    }
    
    valid_logs.push(log_data)
    
    azimuth::Span::end(span)
  }
  
  // 生成低质量的测试数据
  let invalid_spans = []
  let invalid_metrics = []
  let invalid_logs = []
  
  // 创建无效的Span（缺少trace_id）
  invalid_spans.push({
    "span_id": "invalid-span-1",
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
    "name": "invalid-span-1",
    "status": "ok"
    // 缺少trace_id
  })
  
  // 创建无效的Span（无效时间戳）
  invalid_spans.push({
    "trace_id": "invalid-trace-2",
    "span_id": "invalid-span-2",
    "timestamp": -1L,  // 无效时间戳
    "name": "invalid-span-2",
    "status": "ok"
  })
  
  // 创建无效的度量（非数值）
  invalid_metrics.push({
    "name": "invalid.metric",
    "value": "not-a-number",  // 非数值
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
    "type": "counter"
  })
  
  // 运行完整性检查
  let valid_span_results = []
  for span in valid_spans {
    let result = azimuth::DataQualityMonitor::check_integrity(quality_monitor, span)
    valid_span_results.push(result)
  }
  
  let invalid_span_results = []
  for span in invalid_spans {
    let result = azimuth::DataQualityMonitor::check_integrity(quality_monitor, span)
    invalid_span_results.push(result)
  }
  
  let valid_metric_results = []
  for metric in valid_metrics {
    let result = azimuth::DataQualityMonitor::check_integrity(quality_monitor, metric)
    valid_metric_results.push(result)
  }
  
  let invalid_metric_results = []
  for metric in invalid_metrics {
    let result = azimuth::DataQualityMonitor::check_integrity(quality_monitor, metric)
    invalid_metric_results.push(result)
  }
  
  // 验证检查结果
  // 有效数据应该通过所有检查
  for result in valid_span_results {
    assert_true(azimuth::DataQualityResult::is_passed(result))
  }
  
  for result in valid_metric_results {
    assert_true(azimuth::DataQualityResult::is_passed(result))
  }
  
  // 无效数据应该被检测出来
  for result in invalid_span_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  for result in invalid_metric_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  // 创建质量度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "data-quality")
  let integrity_pass_rate_gauge = azimuth::Meter::create_gauge(meter, "integrity.pass.rate")
  let integrity_violation_counter = azimuth::Meter::create_counter(meter, "integrity.violations.total")
  
  // 计算完整性通过率
  let total_checks = valid_span_results.length() + valid_metric_results.length() + invalid_span_results.length() + invalid_metric_results.length()
  let passed_checks = valid_span_results.length() + valid_metric_results.length()
  let pass_rate = passed_checks.to_double() / total_checks.to_double()
  
  azimuth::Gauge::record(integrity_pass_rate_gauge, pass_rate)
  azimuth::Counter::add(integrity_violation_counter, (total_checks - passed_checks).to_double())
  
  // 验证质量度量
  assert_true(pass_rate > 0.9)  // 至少90%的数据应该通过完整性检查
}

// Test 2: 遥测数据一致性验证测试
pub test "遥测数据一致性验证测试" {
  // 创建一致性验证器
  let consistency_validator = azimuth::DataConsistencyValidator::new()
  
  // 配置一致性检查规则
  azimuth::DataConsistencyValidator::add_consistency_rule(consistency_validator, {
    "name": "span.parent.child.consistency",
    "description": "父Span和子Span的trace ID必须一致",
    "check": fn(parent, child) { parent["trace_id"] == child["trace_id"] },
    "severity": "error"
  })
  
  azimuth::DataConsistencyValidator::add_consistency_rule(consistency_validator, {
    "name": "span.timeline.consistency",
    "description": "子Span的开始时间应该在父Span的时间范围内",
    "check": fn(parent, child) { 
      child["start_time"] >= parent["start_time"] && 
      child["end_time"] <= parent["end_time"]
    },
    "severity": "warning"
  })
  
  azimuth::DataConsistencyValidator::add_consistency_rule(consistency_validator, {
    "name": "metric.unit.consistency",
    "description": "相同名称的度量必须有相同的单位",
    "check": fn(metric1, metric2) { 
      metric1["name"] != metric2["name"] || metric1["unit"] == metric2["unit"]
    },
    "severity": "error"
  })
  
  // 创建一致的Span层次结构
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "consistency-test")
  
  let consistent_spans = []
  
  // 创建根Span
  let root_span = azimuth::Tracer::start_span(tracer, "root-operation")
  let root_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let root_span_data = {
    "trace_id": azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span)),
    "span_id": azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span)),
    "parent_span_id": "",
    "start_time": root_start_time,
    "end_time": 0L,  // 暂时设置为0，稍后更新
    "name": "root-operation"
  }
  
  consistent_spans.push(root_span_data)
  
  // 创建子Span
  let child_spans = []
  for i in 0..5 {
    let child_span = azimuth::Tracer::start_span(tracer, "child-operation-" + i.to_string())
    let child_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let child_span_data = {
      "trace_id": azimuth::SpanContext::trace_id(azimuth::Span::span_context(child_span)),
      "span_id": azimuth::SpanContext::span_id(azimuth::Span::span_context(child_span)),
      "parent_span_id": azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span)),
      "start_time": child_start_time,
      "end_time": 0L,  // 暂时设置为0，稍后更新
      "name": "child-operation-" + i.to_string()
    }
    
    child_spans.push(child_span_data)
    consistent_spans.push(child_span_data)
    
    azimuth::Span::end(child_span)
  }
  
  // 结束根Span
  azimuth::Span::end(root_span)
  
  // 更新结束时间
  let root_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  consistent_spans[0]["end_time"] = root_end_time
  
  for i in 0..5 {
    consistent_spans[i + 1]["end_time"] = consistent_spans[i + 1]["start_time"] + 1000000000L  // 1秒后结束
  }
  
  // 创建不一致的Span层次结构
  let inconsistent_spans = []
  
  // 创建根Span
  let inconsistent_root = {
    "trace_id": "inconsistent-trace-1",
    "span_id": "inconsistent-root-1",
    "parent_span_id": "",
    "start_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
    "end_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 5000000000L,  // 5秒后结束
    "name": "inconsistent-root"
  }
  
  inconsistent_spans.push(inconsistent_root)
  
  // 创建不一致的子Span（不同的trace_id）
  let inconsistent_child = {
    "trace_id": "different-trace-id",  // 不同的trace_id
    "span_id": "inconsistent-child-1",
    "parent_span_id": "inconsistent-root-1",
    "start_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000000L,
    "end_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 2000000000L,
    "name": "inconsistent-child"
  }
  
  inconsistent_spans.push(inconsistent_child)
  
  // 创建时间线不一致的子Span
  let timeline_inconsistent_child = {
    "trace_id": "inconsistent-trace-1",  // 相同的trace_id
    "span_id": "timeline-inconsistent-child",
    "parent_span_id": "inconsistent-root-1",
    "start_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 6000000000L,  // 在父Span结束后开始
    "end_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 7000000000L,
    "name": "timeline-inconsistent-child"
  }
  
  inconsistent_spans.push(timeline_inconsistent_child)
  
  // 创建一致的度量数据
  let consistent_metrics = []
  
  for i in 0..10 {
    let metric = {
      "name": "response.time",
      "value": 100.0 + i.to_double() * 10.0,
      "unit": "ms",
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L)
    }
    consistent_metrics.push(metric)
  }
  
  // 创建不一致的度量数据
  let inconsistent_metrics = []
  
  // 相同名称但不同单位的度量
  let metric1 = {
    "name": "response.time",
    "value": 100.0,
    "unit": "ms",
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  }
  
  let metric2 = {
    "name": "response.time",
    "value": 0.1,
    "unit": "s",  // 不同的单位
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000L
  }
  
  inconsistent_metrics.push(metric1)
  inconsistent_metrics.push(metric2)
  
  // 运行一致性检查
  let consistent_span_results = []
  for i in 0..consistent_spans.length() {
    for j in 0..consistent_spans.length() {
      if i != j && consistent_spans[i]["parent_span_id"] == consistent_spans[j]["span_id"] {
        let result = azimuth::DataConsistencyValidator::check_span_consistency(
          consistency_validator, 
          consistent_spans[j], 
          consistent_spans[i]
        )
        consistent_span_results.push(result)
      }
    }
  }
  
  let inconsistent_span_results = []
  for i in 0..inconsistent_spans.length() {
    for j in 0..inconsistent_spans.length() {
      if i != j && inconsistent_spans[i]["parent_span_id"] == inconsistent_spans[j]["span_id"] {
        let result = azimuth::DataConsistencyValidator::check_span_consistency(
          consistency_validator, 
          inconsistent_spans[j], 
          inconsistent_spans[i]
        )
        inconsistent_span_results.push(result)
      }
    }
  }
  
  let consistent_metric_results = []
  for i in 0..consistent_metrics.length() {
    for j in 0..consistent_metrics.length() {
      if i != j {
        let result = azimuth::DataConsistencyValidator::check_metric_consistency(
          consistency_validator, 
          consistent_metrics[i], 
          consistent_metrics[j]
        )
        consistent_metric_results.push(result)
      }
    }
  }
  
  let inconsistent_metric_results = []
  for i in 0..inconsistent_metrics.length() {
    for j in 0..inconsistent_metrics.length() {
      if i != j {
        let result = azimuth::DataConsistencyValidator::check_metric_consistency(
          consistency_validator, 
          inconsistent_metrics[i], 
          inconsistent_metrics[j]
        )
        inconsistent_metric_results.push(result)
      }
    }
  }
  
  // 验证检查结果
  // 一致的数据应该通过所有检查
  for result in consistent_span_results {
    assert_true(azimuth::DataQualityResult::is_passed(result))
  }
  
  for result in consistent_metric_results {
    assert_true(azimuth::DataQualityResult::is_passed(result))
  }
  
  // 不一致的数据应该被检测出来
  for result in inconsistent_span_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  for result in inconsistent_metric_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  // 创建一致性度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "data-consistency")
  let consistency_pass_rate_gauge = azimuth::Meter::create_gauge(meter, "consistency.pass.rate")
  let consistency_violation_counter = azimuth::Meter::create_counter(meter, "consistency.violations.total")
  
  // 计算一致性通过率
  let total_checks = consistent_span_results.length() + consistent_metric_results.length() + inconsistent_span_results.length() + inconsistent_metric_results.length()
  let passed_checks = consistent_span_results.length() + consistent_metric_results.length()
  let pass_rate = passed_checks.to_double() / total_checks.to_double()
  
  azimuth::Gauge::record(consistency_pass_rate_gauge, pass_rate)
  azimuth::Counter::add(consistency_violation_counter, (total_checks - passed_checks).to_double())
  
  // 验证一致性度量
  assert_true(pass_rate > 0.8)  // 至少80%的数据应该通过一致性检查
}

// Test 3: 遥测数据准确性验证测试
pub test "遥测数据准确性验证测试" {
  // 创建准确性验证器
  let accuracy_validator = azimuth::DataAccuracyValidator::new()
  
  // 配置准确性检查规则
  azimuth::DataAccuracyValidator::add_accuracy_rule(accuracy_validator, {
    "name": "metric.value.range",
    "description": "度量值必须在合理范围内",
    "check": fn(metric) {
      match metric["name"] {
        "cpu.usage" => metric["value"] >= 0.0 && metric["value"] <= 100.0,
        "memory.usage" => metric["value"] >= 0.0 && metric["value"] <= 100.0,
        "response.time" => metric["value"] > 0.0 && metric["value"] < 60000.0,  // 小于60秒
        "error.rate" => metric["value"] >= 0.0 && metric["value"] <= 1.0,
        _ => true
      }
    },
    "severity": "error"
  })
  
  azimuth::DataAccuracyValidator::add_accuracy_rule(accuracy_validator, {
    "name": "timestamp.monotonic",
    "description": "时间戳应该是单调递增的",
    "check": fn(data_points) {
      for i in 1..data_points.length() {
        if data_points[i]["timestamp"] <= data_points[i-1]["timestamp"] {
          return false
        }
      }
      true
    },
    "severity": "warning"
  })
  
  azimuth::DataAccuracyValidator::add_accuracy_rule(accuracy_validator, {
    "name": "span.duration.reasonable",
    "description": "Span持续时间应该在合理范围内",
    "check": fn(span) {
      let duration = span["end_time"] - span["start_time"]
      duration > 0L && duration < 3600000000000L  // 大于0且小于1小时
    },
    "severity": "warning"
  })
  
  // 创建准确的度量数据
  let accurate_metrics = []
  
  // CPU使用率（0-100%）
  for i in 0..50 {
    let cpu_usage = 20.0 + 30.0 * azimuth::Math::sin(i.to_double() * 0.1) + azimuth::Random::next_double(azimuth::Random::system()) * 10.0
    let metric = {
      "name": "cpu.usage",
      "value": cpu_usage,
      "unit": "percent",
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L)
    }
    accurate_metrics.push(metric)
  }
  
  // 内存使用率（0-100%）
  for i in 0..50 {
    let memory_usage = 50.0 + 20.0 * azimuth::Math::sin(i.to_double() * 0.05) + azimuth::Random::next_double(azimuth::Random::system()) * 8.0
    let metric = {
      "name": "memory.usage",
      "value": memory_usage,
      "unit": "percent",
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L)
    }
    accurate_metrics.push(metric)
  }
  
  // 响应时间（0-60秒）
  for i in 0..50 {
    let response_time = 100.0 + 50.0 * azimuth::Math::sin(i.to_double() * 0.2) + azimuth::Random::next_double(azimuth::Random::system()) * 20.0
    let metric = {
      "name": "response.time",
      "value": response_time,
      "unit": "ms",
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L)
    }
    accurate_metrics.push(metric)
  }
  
  // 错误率（0-1）
  for i in 0..50 {
    let error_rate = azimuth::Math::max(0.0, 0.05 * azimuth::Math::sin(i.to_double() * 0.15) + azimuth::Random::next_double(azimuth::Random::system()) * 0.02)
    let metric = {
      "name": "error.rate",
      "value": error_rate,
      "unit": "ratio",
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L)
    }
    accurate_metrics.push(metric)
  }
  
  // 创建不准确的度量数据
  let inaccurate_metrics = []
  
  // 超出范围的CPU使用率
  inaccurate_metrics.push({
    "name": "cpu.usage",
    "value": 150.0,  // 超出0-100%范围
    "unit": "percent",
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  })
  
  // 负的内存使用率
  inaccurate_metrics.push({
    "name": "memory.usage",
    "value": -10.0,  // 负值
    "unit": "percent",
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  })
  
  // 过长的响应时间
  inaccurate_metrics.push({
    "name": "response.time",
    "value": 120000.0,  // 120秒，超过60秒限制
    "unit": "ms",
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  })
  
  // 超出范围的错误率
  inaccurate_metrics.push({
    "name": "error.rate",
    "value": 1.5,  // 超出0-1范围
    "unit": "ratio",
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  })
  
  // 创建准确的Span数据
  let accurate_spans = []
  
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "accuracy-test")
  
  for i in 0..20 {
    let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let span = azimuth::Tracer::start_span(tracer, "accuracy-span-" + i.to_string())
    
    // 模拟不同的处理时间
    azimuth::Clock::sleep(10 + (i % 20))
    
    let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    azimuth::Span::end(span)
    
    let span_data = {
      "trace_id": azimuth::SpanContext::trace_id(azimuth::Span::span_context(span)),
      "span_id": azimuth::SpanContext::span_id(azimuth::Span::span_context(span)),
      "start_time": start_time,
      "end_time": end_time,
      "name": "accuracy-span-" + i.to_string()
    }
    
    accurate_spans.push(span_data)
  }
  
  // 创建不准确的Span数据
  let inaccurate_spans = []
  
  // 负持续时间的Span
  inaccurate_spans.push({
    "trace_id": "inaccurate-trace-1",
    "span_id": "inaccurate-span-1",
    "start_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
    "end_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - 1000000000L,  // 结束时间早于开始时间
    "name": "inaccurate-span-1"
  })
  
  // 过长持续时间的Span
  inaccurate_spans.push({
    "trace_id": "inaccurate-trace-2",
    "span_id": "inaccurate-span-2",
    "start_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - 7200000000000L,  // 2小时前
    "end_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
    "name": "inaccurate-span-2"
  })
  
  // 运行准确性检查
  let accurate_metric_results = []
  for metric in accurate_metrics {
    let result = azimuth::DataAccuracyValidator::check_metric_accuracy(accuracy_validator, metric)
    accurate_metric_results.push(result)
  }
  
  let inaccurate_metric_results = []
  for metric in inaccurate_metrics {
    let result = azimuth::DataAccuracyValidator::check_metric_accuracy(accuracy_validator, metric)
    inaccurate_metric_results.push(result)
  }
  
  let accurate_span_results = []
  for span in accurate_spans {
    let result = azimuth::DataAccuracyValidator::check_span_accuracy(accuracy_validator, span)
    accurate_span_results.push(result)
  }
  
  let inaccurate_span_results = []
  for span in inaccurate_spans {
    let result = azimuth::DataAccuracyValidator::check_span_accuracy(accuracy_validator, span)
    inaccurate_span_results.push(result)
  }
  
  // 检查时间戳单调性
  let timestamp_data = []
  for i in 0..20 {
    timestamp_data.push({
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L),
      "value": i.to_double()
    })
  }
  
  let monotonic_result = azimuth::DataAccuracyValidator::check_timestamp_monotonicity(accuracy_validator, timestamp_data)
  
  // 创建非单调的时间戳数据
  let non_monotonic_data = []
  for i in 0..10 {
    non_monotonic_data.push({
      "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + (i * 1000000L),
      "value": i.to_double()
    })
  }
  
  // 添加一个时间戳较小的数据点
  non_monotonic_data.push({
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 5000000L,  // 比前面一些点的时间戳小
    "value": 10.0
  })
  
  let non_monotonic_result = azimuth::DataAccuracyValidator::check_timestamp_monotonicity(accuracy_validator, non_monotonic_data)
  
  // 验证检查结果
  // 准确的数据应该通过所有检查
  for result in accurate_metric_results {
    assert_true(azimuth::DataQualityResult::is_passed(result))
  }
  
  for result in accurate_span_results {
    assert_true(azimuth::DataQualityResult::is_passed(result))
  }
  
  assert_true(azimuth::DataQualityResult::is_passed(monotonic_result))
  
  // 不准确的数据应该被检测出来
  for result in inaccurate_metric_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  for result in inaccurate_span_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  assert_false(azimuth::DataQualityResult::is_passed(non_monotonic_result))
  
  // 创建准确性度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "data-accuracy")
  let accuracy_pass_rate_gauge = azimuth::Meter::create_gauge(meter, "accuracy.pass.rate")
  let accuracy_violation_counter = azimuth::Meter::create_counter(meter, "accuracy.violations.total")
  
  // 计算准确性通过率
  let total_checks = accurate_metric_results.length() + accurate_span_results.length() + inaccurate_metric_results.length() + inaccurate_span_results.length() + 2
  let passed_checks = accurate_metric_results.length() + accurate_span_results.length() + 1
  let pass_rate = passed_checks.to_double() / total_checks.to_double()
  
  azimuth::Gauge::record(accuracy_pass_rate_gauge, pass_rate)
  azimuth::Counter::add(accuracy_violation_counter, (total_checks - passed_checks).to_double())
  
  // 验证准确性度量
  assert_true(pass_rate > 0.85)  // 至少85%的数据应该通过准确性检查
}

// Test 4: 遥测数据可靠性验证测试
pub test "遥测数据可靠性验证测试" {
  // 创建可靠性验证器
  let reliability_validator = azimuth::DataReliabilityValidator::new()
  
  // 配置可靠性检查规则
  azimuth::DataReliabilityValidator::add_reliability_rule(reliability_validator, {
    "name": "data.completeness",
    "description": "数据应该是完整的，没有缺失的关键字段",
    "check": fn(data) {
      let required_fields = ["timestamp", "source"]
      for field in required_fields {
        if !data.has(field) || data[field] == "" {
          return false
        }
      }
      true
    },
    "severity": "error"
  })
  
  azimuth::DataReliabilityValidator::add_reliability_rule(reliability_validator, {
    "name": "data.timeliness",
    "description": "数据应该是及时的，延迟不超过5分钟",
    "check": fn(data) {
      let now = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      let data_age = now - data["timestamp"]
      data_age < 300000000000L  // 5分钟
    },
    "severity": "warning"
  })
  
  azimuth::DataReliabilityValidator::add_reliability_rule(reliability_validator, {
    "name": "data.frequency",
    "description": "数据频率应该在预期范围内",
    "check": fn(data_points, expected_interval) {
      if data_points.length() < 2 {
        return true
      }
      
      for i in 1..data_points.length() {
        let actual_interval = data_points[i]["timestamp"] - data_points[i-1]["timestamp"]
        let min_expected = expected_interval * 0.8
        let max_expected = expected_interval * 1.2
        
        if actual_interval < min_expected || actual_interval > max_expected {
          return false
        }
      }
      true
    },
    "severity": "warning"
  })
  
  // 创建可靠的数据源
  let reliable_sources = ["service-A", "service-B", "service-C"]
  let reliable_data = []
  
  for source in reliable_sources {
    for i in 0..30 {
      let data_point = {
        "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - (i * 1000000000L),  // 每秒一个数据点，过去30秒
        "source": source,
        "metric": "cpu.usage",
        "value": 50.0 + azimuth::Random::next_double(azimuth::Random::system()) * 20.0,
        "status": "ok"
      }
      reliable_data.push(data_point)
    }
  }
  
  // 创建不可靠的数据源
  let unreliable_data = []
  
  // 缺失关键字段的数据
  unreliable_data.push({
    "metric": "memory.usage",
    "value": 60.0,
    "status": "ok"
    // 缺少timestamp和source字段
  })
  
  // 延迟过大的数据
  unreliable_data.push({
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - 400000000000L,  // 400秒前
    "source": "delayed-service",
    "metric": "response.time",
    "value": 200.0,
    "status": "ok"
  })
  
  // 空值数据
  unreliable_data.push({
    "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
    "source": "",  // 空的source
    "metric": "error.rate",
    "value": 0.05,
    "status": "ok"
  })
  
  // 创建频率不一致的数据
  let inconsistent_frequency_data = []
  let base_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 前10个数据点间隔1秒
  for i in 0..10 {
    inconsistent_frequency_data.push({
      "timestamp": base_time - (i * 1000000000L),
      "source": "inconsistent-service",
      "metric": "cpu.usage",
      "value": 50.0
    })
  }
  
  // 后10个数据点间隔3秒
  for i in 0..10 {
    inconsistent_frequency_data.push({
      "timestamp": base_time - 10000000000L - (i * 3000000000L),
      "source": "inconsistent-service",
      "metric": "cpu.usage",
      "value": 50.0
    })
  }
  
  // 运行可靠性检查
  let reliable_results = []
  for data_point in reliable_data {
    let result = azimuth::DataReliabilityValidator::check_data_reliability(reliability_validator, data_point)
    reliable_results.push(result)
  }
  
  let unreliable_results = []
  for data_point in unreliable_data {
    let result = azimuth::DataReliabilityValidator::check_data_reliability(reliability_validator, data_point)
    unreliable_results.push(result)
  }
  
  // 检查数据频率一致性
  let frequency_result = azimuth::DataReliabilityValidator::check_data_frequency(
    reliability_validator, 
    reliable_data.filter(d => d["source"] == "service-A"), 
    1000000000L  // 预期间隔1秒
  )
  
  let inconsistent_frequency_result = azimuth::DataReliabilityValidator::check_data_frequency(
    reliability_validator, 
    inconsistent_frequency_data, 
    1000000000L  // 预期间隔1秒
  )
  
  // 检查数据源可用性
  let source_availability = azimuth::DataReliabilityValidator::check_source_availability(
    reliability_validator, 
    reliable_data, 
    60000000000L  // 1分钟内
  )
  
  // 模拟数据源故障
  let failed_source_data = reliable_data.filter(d => d["source"] != "service-C")  // 移除service-C的数据
  let reduced_availability = azimuth::DataReliabilityValidator::check_source_availability(
    reliability_validator, 
    failed_source_data, 
    60000000000L  // 1分钟内
  )
  
  // 验证检查结果
  // 可靠的数据应该通过大多数检查
  let reliable_passed = 0
  for result in reliable_results {
    if azimuth::DataQualityResult::is_passed(result) {
      reliable_passed = reliable_passed + 1
    }
  }
  
  // 至少90%的可靠数据应该通过检查
  assert_true(reliable_passed.to_double() / reliable_results.length().to_double() > 0.9)
  
  // 不可靠的数据应该被检测出来
  for result in unreliable_results {
    assert_false(azimuth::DataQualityResult::is_passed(result))
    assert_true(azimuth::DataQualityResult::get_violation_count(result) > 0)
  }
  
  // 频率检查结果
  assert_true(azimuth::DataQualityResult::is_passed(frequency_result))
  assert_false(azimuth::DataQualityResult::is_passed(inconsistent_frequency_result))
  
  // 可用性检查结果
  assert_true(azimuth::DataReliabilityValidator::get_availability_score(source_availability) > 0.9)
  assert_true(azimuth::DataReliabilityValidator::get_availability_score(reduced_availability) < 0.9)
  
  // 创建可靠性度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "data-reliability")
  let reliability_score_gauge = azimuth::Meter::create_gauge(meter, "reliability.score")
  let source_availability_gauge = azimuth::Meter::create_gauge(meter, "source.availability")
  let data_freshness_gauge = azimuth::Meter::create_gauge(meter, "data.freshness")
  
  // 计算可靠性分数
  let total_reliable_checks = reliable_results.length() + unreliable_results.length() + 2
  let total_passed_checks = reliable_passed + 2
  let reliability_score = total_passed_checks.to_double() / total_reliable_checks.to_double()
  
  azimuth::Gauge::record(reliability_score_gauge, reliability_score)
  azimuth::Gauge::record(source_availability_gauge, azimuth::DataReliabilityValidator::get_availability_score(source_availability))
  
  // 计算数据新鲜度（最近数据点的比例）
  let fresh_data_count = reliable_data.filter(d => 
    azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) - d["timestamp"] < 60000000000L
  ).length()
  
  let freshness_score = fresh_data_count.to_double() / reliable_data.length().to_double()
  azimuth::Gauge::record(data_freshness_gauge, freshness_score)
  
  // 验证可靠性度量
  assert_true(reliability_score > 0.8)  // 至少80%的可靠性分数
  assert_true(freshness_score > 0.9)   // 至少90%的数据是新鲜的
}