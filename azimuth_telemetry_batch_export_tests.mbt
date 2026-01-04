// Azimuth 遥测数据批处理和导出测试用例
// 专注于测试遥测数据的批处理机制和导出功能

// 测试1: 基础批处理功能
test "基础批处理功能测试" {
  // 创建批处理器
  let batch_processor = BatchProcessor::new(100, 5000)  // 100条记录或5秒超时
  
  // 创建测试span数据
  let spans = []
  for i in 1..=50 {
    let span = Span::new("test-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    spans = spans.push(span)
  }
  
  // 添加span到批处理器
  for span in spans {
    BatchProcessor::add_span(batch_processor, span)
  }
  
  // 验证批处理器状态
  assert_eq(BatchProcessor::get_span_count(batch_processor), 50)
  assert_false(BatchProcessor::is_ready(batch_processor))  // 未达到批处理阈值
  
  // 添加更多span直到达到阈值
  for i in 51..=100 {
    let span = Span::new("test-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    BatchProcessor::add_span(batch_processor, span)
  }
  
  // 验证批处理器已准备就绪
  assert_eq(BatchProcessor::get_span_count(batch_processor), 100)
  assert_true(BatchProcessor::is_ready(batch_processor))
  
  // 获取批次数据
  let batch = BatchProcessor::get_batch(batch_processor)
  assert_eq(batch.length(), 100)
  
  // 验证批处理器已重置
  assert_eq(BatchProcessor::get_span_count(batch_processor), 0)
  assert_false(BatchProcessor::is_ready(batch_processor))
}

// 测试2: 批处理超时机制
test "批处理超时机制测试" {
  // 创建短超时的批处理器
  let batch_processor = BatchProcessor::new(1000, 100)  // 1000条记录或100ms超时
  
  // 添加少量span
  for i in 1..=10 {
    let span = Span::new("timeout-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    BatchProcessor::add_span(batch_processor, span)
  }
  
  // 验证初始状态
  assert_eq(BatchProcessor::get_span_count(batch_processor), 10)
  assert_false(BatchProcessor::is_ready(batch_processor))
  
  // 等待超时
  Clock::sleep(150)  // 等待150ms，超过100ms超时
  
  // 验证超时后批处理器准备就绪
  assert_true(BatchProcessor::is_ready_by_timeout(batch_processor))
  
  // 获取超时批次
  let timeout_batch = BatchProcessor::get_batch(batch_processor)
  assert_eq(timeout_batch.length(), 10)
  
  // 验证批处理器已重置
  assert_eq(BatchProcessor::get_span_count(batch_processor), 0)
}

// 测试3: 多类型数据批处理
test "多类型数据批处理测试" {
  // 创建多类型批处理器
  let batch_processor = MultiTypeBatchProcessor::new(50)  // 每种类型50条记录
  
  // 添加span数据
  for i in 1..=30 {
    let span = Span::new("span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    MultiTypeBatchProcessor::add_span(batch_processor, span)
  }
  
  // 添加metric数据
  for i in 1..=40 {
    let metric = Metric::new("metric-" + i.to_string(), Counter, i.to_float())
    MultiTypeBatchProcessor::add_metric(batch_processor, metric)
  }
  
  // 添加log数据
  for i in 1..=60 {
    let log = LogRecord::new(Info, "log message " + i.to_string())
    MultiTypeBatchProcessor::add_log(batch_processor, log)
  }
  
  // 验证各类型数据计数
  assert_eq(MultiTypeBatchProcessor::get_span_count(batch_processor), 30)
  assert_eq(MultiTypeBatchProcessor::get_metric_count(batch_processor), 40)
  assert_eq(MultiTypeBatchProcessor::get_log_count(batch_processor), 60)
  
  // 验证哪些类型已准备就绪
  assert_false(MultiTypeBatchProcessor::is_spans_ready(batch_processor))   // 30 < 50
  assert_false(MultiTypeBatchProcessor::is_metrics_ready(batch_processor))  // 40 < 50
  assert_true(MultiTypeBatchProcessor::is_logs_ready(batch_processor))     // 60 >= 50
  
  // 获取准备就绪的日志批次
  let log_batch = MultiTypeBatchProcessor::get_log_batch(batch_processor)
  assert_eq(log_batch.length(), 60)
  
  // 验证日志计数已重置
  assert_eq(MultiTypeBatchProcessor::get_log_count(batch_processor), 0)
}

// 测试4: 基于优先级的批处理
test "基于优先级的批处理测试" {
  // 创建优先级批处理器
  let batch_processor = PriorityBatchProcessor::new(100)
  
  // 添加不同优先级的span
  let high_priority_span = Span::new("high-priority", Internal, SpanContext::new("trace-1", "span-1", true, ""))
  Span::set_attribute(high_priority_span, "priority", StringValue("high"))
  
  let normal_priority_span = Span::new("normal-priority", Internal, SpanContext::new("trace-2", "span-2", true, ""))
  Span::set_attribute(normal_priority_span, "priority", StringValue("normal"))
  
  let low_priority_span = Span::new("low-priority", Internal, SpanContext::new("trace-3", "span-3", true, ""))
  Span::set_attribute(low_priority_span, "priority", StringValue("low"))
  
  // 按非优先级顺序添加
  PriorityBatchProcessor::add_span(batch_processor, normal_priority_span)
  PriorityBatchProcessor::add_span(batch_processor, low_priority_span)
  PriorityBatchProcessor::add_span(batch_processor, high_priority_span)
  
  // 添加更多span
  for i in 4..=50 {
    let span = Span::new("priority-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    let priority = if i % 3 == 0 { "high" } else if i % 3 == 1 { "normal" } else { "low" }
    Span::set_attribute(span, "priority", StringValue(priority))
    PriorityBatchProcessor::add_span(batch_processor, span)
  }
  
  // 获取批次
  let batch = PriorityBatchProcessor::get_batch(batch_processor)
  
  // 验证高优先级span排在前面
  let first_span = batch[0]
  let first_priority = Span::get_attribute(first_span, "priority")
  match first_priority {
    Some(StringValue(p)) => assert_eq(p, "high")
    _ => assert_true(false)
  }
  
  // 验证所有高优先级span都在前面
  let mut found_normal = false
  let mut found_low = false
  let mut priority_order_correct = true
  
  for span in batch {
    let priority = Span::get_attribute(span, "priority")
    match priority {
      Some(StringValue(p)) => {
        if p == "normal" and not(found_normal) {
          found_normal = true
        } else if p == "low" and not(found_low) {
          found_low = true
        } else if p == "high" and (found_normal or found_low) {
          priority_order_correct = false  // 高优先级在普通或低优先级后面
        } else if p == "normal" and found_low {
          priority_order_correct = false  // 普通优先级在低优先级后面
        }
      }
      _ => {}
    }
  }
  
  assert_true(priority_order_correct)
}

// 测试5: 基于条件的批处理
test "基于条件的批处理测试" {
  // 创建条件批处理器
  let error_condition = BatchCondition::new(fn(span) {
    match Span::get_attribute(span, "error.type") {
      Some(_) => true
      None => false
    }
  })
  
  let batch_processor = ConditionalBatchProcessor::new(50, [error_condition])
  
  // 添加正常span
  for i in 1..=40 {
    let span = Span::new("normal-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    ConditionalBatchProcessor::add_span(batch_processor, span)
  }
  
  // 添加错误span
  for i in 1..=10 {
    let span = Span::new("error-span-" + i.to_string(), Internal, SpanContext::new("trace-error-" + i.to_string(), "span-error-" + i.to_string(), true, ""))
    Span::set_attribute(span, "error.type", StringValue("DatabaseError"))
    ConditionalBatchProcessor::add_span(batch_processor, span)
  }
  
  // 验证总计数
  assert_eq(ConditionalBatchProcessor::get_total_span_count(batch_processor), 50)
  
  // 验证条件匹配计数
  assert_eq(ConditionalBatchProcessor::get_matching_span_count(batch_processor), 10)
  
  // 验证条件批次是否准备就绪（错误span达到阈值）
  assert_true(ConditionalBatchProcessor::is_condition_ready(batch_processor, error_condition))
  
  // 获取条件批次
  let error_batch = ConditionalBatchProcessor::get_condition_batch(batch_processor, error_condition)
  assert_eq(error_batch.length(), 10)
  
  // 验证所有批次都是错误span
  for span in error_batch {
    let error_type = Span::get_attribute(span, "error.type")
    match error_type {
      Some(StringValue(_)) => assert_true(true)
      None => assert_true(false)
    }
  }
}

// 测试6: 导出器基础功能
test "导出器基础功能测试" {
  // 创建内存导出器
  let exporter = MemoryExporter::new()
  
  // 创建测试span
  let span1 = Span::new("export-span-1", Internal, SpanContext::new("trace-1", "span-1", true, ""))
  let span2 = Span::new("export-span-2", Internal, SpanContext::new("trace-2", "span-2", true, ""))
  
  // 导出span
  let export_result1 = Exporter::export_span(exporter, span1)
  let export_result2 = Exporter::export_span(exporter, span2)
  
  // 验证导出结果
  assert_eq(export_result1, ExportSuccess)
  assert_eq(export_result2, ExportSuccess)
  
  // 验证导出的span
  let exported_spans = MemoryExporter::get_exported_spans(exporter)
  assert_eq(exported_spans.length(), 2)
  assert_eq(Span::name(exported_spans[0]), "export-span-1")
  assert_eq(Span::name(exported_spans[1]), "export-span-2")
  
  // 测试批量导出
  let batch_spans = []
  for i in 3..=7 {
    let span = Span::new("batch-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    batch_spans = batch_spans.push(span)
  }
  
  let batch_export_result = Exporter::export_spans(exporter, batch_spans)
  assert_eq(batch_export_result, ExportSuccess)
  
  // 验证批量导出结果
  let all_exported_spans = MemoryExporter::get_exported_spans(exporter)
  assert_eq(all_exported_spans.length(), 7)
}

// 测试7: HTTP导出器功能
test "HTTP导出器功能测试" {
  // 创建模拟HTTP导出器
  let http_exporter = HttpExporter::new("https://otel-collector.example.com:4318/v1/traces", "application/json")
  
  // 配置HTTP头
  HttpExporter::set_header(http_exporter, "Authorization", "Bearer token123")
  HttpExporter::set_header(http_exporter, "User-Agent", "Azimuth-Telemetry/1.0")
  
  // 创建测试span
  let span = Span::new("http-export-span", Internal, SpanContext::new("trace-1", "span-1", true, ""))
  Span::set_attribute(span, "http.method", StringValue("GET"))
  Span::set_attribute(span, "http.url", StringValue("https://api.example.com/data"))
  Span::set_attribute(span, "http.status_code", IntValue(200))
  
  // 导出span
  let export_result = Exporter::export_span(http_exporter, span)
  
  // 验证导出结果（模拟成功）
  assert_eq(export_result, ExportSuccess)
  
  // 验证导出统计
  let stats = HttpExporter::get_export_stats(http_exporter)
  assert_eq(stats.success_count, 1)
  assert_eq(stats.failure_count, 0)
  assert_eq(stats.total_attempts, 1)
  
  // 测试批量导出
  let batch_spans = []
  for i in 1..=10 {
    let batch_span = Span::new("batch-http-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    batch_spans = batch_spans.push(batch_span)
  }
  
  let batch_export_result = Exporter::export_spans(http_exporter, batch_spans)
  assert_eq(batch_export_result, ExportSuccess)
  
  // 验证批量导出统计
  let updated_stats = HttpExporter::get_export_stats(http_exporter)
  assert_eq(updated_stats.success_count, 11)  // 1 + 10
  assert_eq(updated_stats.failure_count, 0)
  assert_eq(updated_stats.total_attempts, 11)
}

// 测试8: 导出器重试机制
test "导出器重试机制测试" {
  // 创建会失败的导出器
  let failing_exporter = FailingHttpExporter::new("https://invalid.example.com:4318/v1/traces", 3)  // 最多重试3次
  
  // 创建测试span
  let span = Span::new("retry-span", Internal, SpanContext::new("trace-1", "span-1", true, ""))
  
  // 导出span（会失败并重试）
  let export_result = Exporter::export_span(failing_exporter, span)
  
  // 验证最终失败
  assert_eq(export_result, ExportFailure)
  
  // 验证重试统计
  let retry_stats = FailingHttpExporter::get_retry_stats(failing_exporter)
  assert_eq(retry_stats.total_attempts, 4)  // 1次初始尝试 + 3次重试
  assert_eq(retry_stats.success_count, 0)
  assert_eq(retry_stats.failure_count, 1)
  
  // 测试部分成功的重试
  let partial_exporter = PartialFailingExporter::new("https://flaky.example.com:4318/v1/traces", 3, 2)  // 最多重试3次，第2次成功
  
  let partial_span = Span::new("partial-retry-span", Internal, SpanContext::new("trace-2", "span-2", true, ""))
  
  let partial_export_result = Exporter::export_span(partial_exporter, partial_span)
  
  // 验证最终成功
  assert_eq(partial_export_result, ExportSuccess)
  
  // 验证重试统计
  let partial_retry_stats = PartialFailingExporter::get_retry_stats(partial_exporter)
  assert_eq(partial_retry_stats.total_attempts, 2)  // 1次初始尝试 + 1次重试
  assert_eq(partial_retry_stats.success_count, 1)
  assert_eq(partial_retry_stats.failure_count, 0)
}

// 测试9: 导出器限流机制
test "导出器限流机制测试" {
  // 创建限流导出器
  let rate_limited_exporter = RateLimitedExporter::new(MemoryExporter::new(), 5, 1000)  // 每秒最多5次导出
  
  // 创建测试span
  let spans = []
  for i in 1..=10 {
    let span = Span::new("rate-limited-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    spans = spans.push(span)
  }
  
  // 快速导出多个span
  let mut success_count = 0
  let mut failure_count = 0
  
  for span in spans {
    let export_result = Exporter::export_span(rate_limited_exporter, span)
    match export_result {
      ExportSuccess => success_count = success_count + 1
      ExportFailure => failure_count = failure_count + 1
    }
  }
  
  // 验证限流效果
  assert_eq(success_count, 5)   // 前5次应该成功
  assert_eq(failure_count, 5)  // 后5次应该被限流
  
  // 等待限流重置
  Clock::sleep(1100)  // 等待1.1秒，超过限流周期
  
  // 再次导出span
  let after_limit_span = Span::new("after-limit-span", Internal, SpanContext::new("trace-after", "span-after", true, ""))
  let after_limit_result = Exporter::export_span(rate_limited_exporter, after_limit_span)
  
  // 验证限流重置后可以导出
  assert_eq(after_limit_result, ExportSuccess)
  
  // 验证限流统计
  let rate_limit_stats = RateLimitedExporter::get_rate_limit_stats(rate_limited_exporter)
  assert_eq(rate_limit_stats.total_attempts, 11)
  assert_eq(rate_limit_stats.success_count, 6)
  assert_eq(rate_limit_stats.rate_limited_count, 5)
}

// 测试10: 导出器压缩功能
test "导出器压缩功能测试" {
  // 创建压缩导出器
  let memory_exporter = MemoryExporter::new()
  let compressed_exporter = CompressedExporter::new(memory_exporter, "gzip")
  
  // 创建大量属性的span
  let span = Span::new("compressed-span", Internal, SpanContext::new("trace-1", "span-1", true, ""))
  
  // 添加大量属性
  for i in 1..=100 {
    Span::set_attribute(span, "attr-" + i.to_string(), StringValue("value-" + i.to_string()))
  }
  
  // 导出span
  let export_result = Exporter::export_span(compressed_exporter, span)
  assert_eq(export_result, ExportSuccess)
  
  // 验证压缩统计
  let compression_stats = CompressedExporter::get_compression_stats(compressed_exporter)
  assert_eq(compression_stats.total_exports, 1)
  assert_true(compression_stats.original_size > compression_stats.compressed_size)
  assert_true(compression_stats.compression_ratio > 0.0)
  
  // 测试批量导出压缩
  let batch_spans = []
  for i in 1..=50 {
    let batch_span = Span::new("batch-compressed-span-" + i.to_string(), Internal, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    
    // 为每个span添加属性
    for j in 1..=20 {
      Span::set_attribute(batch_span, "attr-" + j.to_string(), StringValue("value-" + i.to_string() + "-" + j.to_string()))
    }
    
    batch_spans = batch_spans.push(batch_span)
  }
  
  let batch_export_result = Exporter::export_spans(compressed_exporter, batch_spans)
  assert_eq(batch_export_result, ExportSuccess)
  
  // 验证批量压缩统计
  let batch_compression_stats = CompressedExporter::get_compression_stats(compressed_exporter)
  assert_eq(batch_compression_stats.total_exports, 51)  // 1 + 50
  assert_true(batch_compression_stats.original_size > batch_compression_stats.compressed_size)
  
  // 验证压缩效果
  let overall_compression_ratio = batch_compression_stats.compressed_size.to_float() / batch_compression_stats.original_size.to_float()
  assert_true(overall_compression_ratio < 0.8)  // 压缩后应该至少减少20%
}