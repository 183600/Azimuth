// Azimuth High Concurrency Telemetry Consistency Tests
// 高并发场景下的遥测一致性测试用例
// 测试高并发环境下遥测数据的收集、处理和存储的一致性

import "azimuth/azimuth"

// Test 1: 并发Span创建和管理一致性测试
pub test "并发Span创建和管理一致性测试" {
  // 创建并发测试环境
  let concurrent_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "concurrency-test")
  let concurrency_level = 50  // 50个并发线程
  let spans_per_thread = 100  // 每个线程创建100个Span
  
  // 创建同步机制
  let span_counter = azimuth::AtomicCounter::new(0)
  let completed_spans = azimuth::AtomicCounter::new(0)
  let span_ids = azimuth::ConcurrentSet::new()
  let trace_ids = azimuth::ConcurrentSet::new()
  
  // 记录测试开始时间
  let test_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建并发任务
  let concurrent_tasks = []
  for thread_id in 0..concurrency_level {
    let task = azimuth::ConcurrentTask::new(fn() {
      // 每个线程创建多个Span
      let thread_spans = []
      
      for span_id in 0..spans_per_thread {
        // 创建Span
        let span_name = "concurrent-span-" + thread_id.to_string() + "-" + span_id.to_string()
        let span = azimuth::Tracer::start_span(concurrent_tracer, span_name)
        
        // 记录Span信息
        let span_context = azimuth::Span::span_context(span)
        let current_span_id = azimuth::SpanContext::span_id(span_context)
        let current_trace_id = azimuth::SpanContext::trace_id(span_context)
        
        // 线程安全地添加到集合
        azimuth::ConcurrentSet::add(span_ids, current_span_id)
        azimuth::ConcurrentSet::add(trace_ids, current_trace_id)
        
        // 添加属性和事件
        azimuth::Span::add_event(span, "concurrent.test.event", Some([
          ("thread.id", azimuth::IntValue(thread_id)),
          ("span.id", azimuth::IntValue(span_id)),
          "span.name", azimuth::StringValue(span_name)
        ]))
        
        // 模拟处理时间
        azimuth::Clock::sleep(1 + (span_id % 5))
        
        // 设置状态
        if span_id % 10 == 0 {
          azimuth::Span::set_status(span, azimuth::Error)
        } else {
          azimuth::Span::set_status(span, azimuth::Ok)
        }
        
        thread_spans.push(span)
        azimuth::AtomicCounter::increment(span_counter)
      }
      
      // 结束所有Span
      for span in thread_spans {
        azimuth::Span::end(span)
        azimuth::AtomicCounter::increment(completed_spans)
      }
      
      thread_spans.length()
    })
    
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let task_results = azimuth::ConcurrentTaskExecutor::execute_all(concurrent_tasks)
  
  // 记录测试结束时间
  let test_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let test_duration = test_end_time - test_start_time
  
  // 验证并发执行结果
  let total_spans_created = azimuth::AtomicCounter::get(span_counter)
  let total_spans_completed = azimuth::AtomicCounter::get(completed_spans)
  let unique_span_ids = azimuth::ConcurrentSet::size(span_ids)
  let unique_trace_ids = azimuth::ConcurrentSet::size(trace_ids)
  
  // 验证Span数量
  assert_eq(total_spans_created, concurrency_level * spans_per_thread)
  assert_eq(total_spans_completed, concurrency_level * spans_per_thread)
  
  // 验证Span ID唯一性
  assert_eq(unique_span_ids, concurrency_level * spans_per_thread)
  
  // 验证Trace ID数量（应该小于等于Span数量，因为可能有相同的Trace ID）
  assert_true(unique_trace_ids <= unique_span_ids)
  assert_true(unique_trace_ids > 0)
  
  // 验证所有任务都成功完成
  for result in task_results {
    assert_true(azimuth::TaskResult::is_success(result))
    assert_eq(azimuth::TaskResult::get_result(result), spans_per_thread)
  }
  
  // 创建并发性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrency-test")
  let span_creation_rate_gauge = azimuth::Meter::create_gauge(meter, "span.creation.rate")
  let span_completion_rate_gauge = azimuth::Meter::create_gauge(meter, "span.completion.rate")
  let concurrency_overhead_histogram = azimuth::Meter::create_histogram(meter, "concurrency.overhead", Some("Concurrency overhead"), Some("ms"))
  
  // 计算性能指标
  let span_creation_rate = total_spans_created.to_double() / (test_duration.to_double() / 1000000000.0)  // Span/秒
  let span_completion_rate = total_spans_completed.to_double() / (test_duration.to_double() / 1000000000.0)  // Span/秒
  
  azimuth::Gauge::record(span_creation_rate_gauge, span_creation_rate)
  azimuth::Gauge::record(span_completion_rate_gauge, span_completion_rate)
  azimuth::Histogram::record(concurrency_overhead_histogram, test_duration.to_double() / 1000000.0)
  
  // 验证性能指标
  assert_true(span_creation_rate > 100.0)  // 至少每秒创建100个Span
  assert_true(span_completion_rate > 100.0)  // 至少每秒完成100个Span
}

// Test 2: 并发度量操作一致性测试
pub test "并发度量操作一致性测试" {
  // 创建并发度量测试环境
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrent-metrics")
  let concurrency_level = 30
  let operations_per_thread = 200
  
  // 创建多种类型的度量
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram", Some("Concurrent histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "concurrent.gauge")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "concurrent.updown")
  
  // 创建同步机制
  let operation_counter = azimuth::AtomicCounter::new(0)
  let counter_sum = azimuth::AtomicDouble::new(0.0)
  let histogram_sum = azimuth::AtomicDouble::new(0.0)
  let gauge_values = azimuth::ConcurrentList::new()
  let updown_sum = azimuth::AtomicDouble::new(0.0)
  
  // 记录测试开始时间
  let test_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建并发任务
  let concurrent_tasks = []
  for thread_id in 0..concurrency_level {
    let task = azimuth::ConcurrentTask::new(fn() {
      let thread_counter_sum = 0.0
      let thread_histogram_sum = 0.0
      let thread_updown_sum = 0.0
      
      for operation_id in 0..operations_per_thread {
        // Counter操作
        let counter_value = (thread_id * operations_per_thread + operation_id).to_double()
        azimuth::Counter::add(counter, counter_value, Some([
          ("thread.id", azimuth::IntValue(thread_id)),
          ("operation.id", azimuth::IntValue(operation_id))
        ]))
        thread_counter_sum = thread_counter_sum + counter_value
        
        // Histogram操作
        let histogram_value = 10.0 + (operation_id % 100).to_double() + azimuth::Random::next_double(azimuth::Random::system()) * 10.0
        azimuth::Histogram::record(histogram, histogram_value, Some([
          ("thread.id", azimuth::IntValue(thread_id))
        ]))
        thread_histogram_sum = thread_histogram_sum + histogram_value
        
        // Gauge操作
        let gauge_value = 50.0 + azimuth::Math::sin(operation_id.to_double() * 0.1) * 20.0
        azimuth::Gauge::record(gauge, gauge_value, Some([
          ("thread.id", azimuth::IntValue(thread_id)),
          ("operation.id", azimuth::IntValue(operation_id))
        ]))
        
        // 线程安全地记录Gauge值
        azimuth::ConcurrentList::add(gauge_values, {
          "thread_id": thread_id,
          "operation_id": operation_id,
          "value": gauge_value,
          "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
        })
        
        // UpDownCounter操作
        let updown_value = if operation_id % 2 == 0 { 1.0 } else { -1.0 }
        azimuth::UpDownCounter::add(updown_counter, updown_value, Some([
          ("thread.id", azimuth::IntValue(thread_id))
        ]))
        thread_updown_sum = thread_updown_sum + updown_value
        
        // 原子递增操作计数器
        azimuth::AtomicCounter::increment(operation_counter)
        
        // 模拟处理时间
        azimuth::Clock::sleep(1)
      }
      
      // 线程结束时更新总和
      azimuth::AtomicDouble::add(counter_sum, thread_counter_sum)
      azimuth::AtomicDouble::add(histogram_sum, thread_histogram_sum)
      azimuth::AtomicDouble::add(updown_sum, thread_updown_sum)
      
      operations_per_thread
    })
    
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let task_results = azimuth::ConcurrentTaskExecutor::execute_all(concurrent_tasks)
  
  // 记录测试结束时间
  let test_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let test_duration = test_end_time - test_start_time
  
  // 验证并发执行结果
  let total_operations = azimuth::AtomicCounter::get(operation_counter)
  let expected_counter_sum = azimuth::AtomicDouble::get(counter_sum)
  let expected_histogram_sum = azimuth::AtomicDouble::get(histogram_sum)
  let expected_updown_sum = azimuth::AtomicDouble::get(updown_sum)
  let total_gauge_values = azimuth::ConcurrentList::size(gauge_values)
  
  // 验证操作数量
  assert_eq(total_operations, concurrency_level * operations_per_thread)
  assert_eq(total_gauge_values, concurrency_level * operations_per_thread)
  
  // 验证所有任务都成功完成
  for result in task_results {
    assert_true(azimuth::TaskResult::is_success(result))
    assert_eq(azimuth::TaskResult::get_result(result), operations_per_thread)
  }
  
  // 等待度量数据聚合
  azimuth::Clock::sleep(100)
  
  // 获取聚合的度量数据
  let aggregated_metrics = azimuth::MeterProvider::get_aggregated_metrics(meter)
  
  // 验证Counter聚合结果
  let counter_data = azimuth::AggregatedMetrics::get_counter_data(aggregated_metrics, "concurrent.counter")
  assert_true(counter_data.length() > 0)
  
  // 计算预期的Counter总和
  let expected_total = 0.0
  for i in 0..concurrency_level {
    for j in 0..operations_per_thread {
      expected_total = expected_total + (i * operations_per_thread + j).to_double()
    }
  }
  
  // 验证Counter总和（允许小的误差）
  let actual_counter_sum = azimuth::CounterData::get_sum(counter_data[0])
  assert_true(azimuth::FloatUtils::almost_equal(actual_counter_sum, expected_total, expected_total * 0.001))
  
  // 验证Histogram聚合结果
  let histogram_data = azimuth::AggregatedMetrics::get_histogram_data(aggregated_metrics, "concurrent.histogram")
  assert_true(histogram_data.length() > 0)
  
  let histogram_count = azimuth::HistogramData::get_count(histogram_data[0])
  assert_eq(histogram_count, concurrency_level * operations_per_thread)
  
  // 验证Gauge聚合结果
  let gauge_data = azimuth::AggregatedMetrics::get_gauge_data(aggregated_metrics, "concurrent.gauge")
  assert_true(gauge_data.length() > 0)
  
  // 验证UpDownCounter聚合结果
  let updown_data = azimuth::AggregatedMetrics::get_updown_counter_data(aggregated_metrics, "concurrent.updown")
  assert_true(updown_data.length() > 0)
  
  // 创建并发度量性能度量
  let performance_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrency-performance")
  let operation_rate_gauge = azimuth::Meter::create_gauge(performance_meter, "metric.operation.rate")
  let concurrency_latency_histogram = azimuth::Meter::create_histogram(performance_meter, "concurrency.latency", Some("Concurrency latency"), Some("ms"))
  
  // 计算性能指标
  let operation_rate = total_operations.to_double() / (test_duration.to_double() / 1000000000.0)  // 操作/秒
  
  azimuth::Gauge::record(operation_rate_gauge, operation_rate)
  azimuth::Histogram::record(concurrency_latency_histogram, test_duration.to_double() / 1000000.0)
  
  // 验证性能指标
  assert_true(operation_rate > 1000.0)  // 至少每秒1000个操作
}

// Test 3: 并发日志记录一致性测试
pub test "并发日志记录一致性测试" {
  // 创建并发日志测试环境
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "concurrent-logger")
  let concurrency_level = 40
  let logs_per_thread = 150
  
  // 创建同步机制
  let log_counter = azimuth::AtomicCounter::new(0)
  let log_messages = azimuth::ConcurrentSet::new()
  let log_severities = azimuth::ConcurrentMap::new()
  
  // 记录测试开始时间
  let test_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建并发任务
  let concurrent_tasks = []
  for thread_id in 0..concurrency_level {
    let task = azimuth::ConcurrentTask::new(fn() {
      for log_id in 0..logs_per_thread {
        // 确定日志严重级别
        let severity = match log_id % 6 {
          0 => azimuth::Trace,
          1 => azimuth::Debug,
          2 => azimuth::Info,
          3 => azimuth::Warn,
          4 => azimuth::Error,
          _ => azimuth::Fatal
        }
        
        // 创建日志消息
        let log_message = "Concurrent log message from thread " + thread_id.to_string() + " log " + log_id.to_string()
        
        // 创建日志记录
        let log_record = azimuth::LogRecord::new_with_context(
          severity,
          Some(log_message),
          Some([
            ("thread.id", azimuth::IntValue(thread_id)),
            ("log.id", azimuth::IntValue(log_id)),
            ("component", azimuth::StringValue("concurrency-test"))
          ]),
          Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
          None,
          Some("concurrent-trace-" + (thread_id % 10).to_string()),
          Some("concurrent-span-" + (log_id % 20).to_string()),
          Some(azimuth::Context::root())
        )
        
        // 发出日志
        azimuth::Logger::emit(logger, log_record)
        
        // 线程安全地记录日志信息
        azimuth::ConcurrentSet::add(log_messages, log_message)
        
        let severity_key = match severity {
          azimuth::Trace => "trace",
          azimuth::Debug => "debug",
          azimuth::Info => "info",
          azimuth::Warn => "warn",
          azimuth::Error => "error",
          azimuth::Fatal => "fatal"
        }
        
        azimuth::ConcurrentMap::increment(log_severities, severity_key)
        
        // 原子递增日志计数器
        azimuth::AtomicCounter::increment(log_counter)
        
        // 模拟处理时间
        azimuth::Clock::sleep(1)
      }
      
      logs_per_thread
    })
    
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let task_results = azimuth::ConcurrentTaskExecutor::execute_all(concurrent_tasks)
  
  // 记录测试结束时间
  let test_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let test_duration = test_end_time - test_start_time
  
  // 等待日志处理完成
  azimuth::Clock::sleep(200)
  
  // 验证并发执行结果
  let total_logs = azimuth::AtomicCounter::get(log_counter)
  let unique_log_messages = azimuth::ConcurrentSet::size(log_messages)
  
  // 验证日志数量
  assert_eq(total_logs, concurrency_level * logs_per_thread)
  assert_eq(unique_log_messages, concurrency_level * logs_per_thread)  // 所有日志消息应该是唯一的
  
  // 验证日志严重级别分布
  let expected_severity_count = (concurrency_level * logs_per_thread) / 6
  let actual_trace_count = azimuth::ConcurrentMap::get(log_severities, "trace")
  let actual_debug_count = azimuth::ConcurrentMap::get(log_severities, "debug")
  let actual_info_count = azimuth::ConcurrentMap::get(log_severities, "info")
  let actual_warn_count = azimuth::ConcurrentMap::get(log_severities, "warn")
  let actual_error_count = azimuth::ConcurrentMap::get(log_severities, "error")
  let actual_fatal_count = azimuth::ConcurrentMap::get(log_severities, "fatal")
  
  // 验证严重级别计数（允许小的误差）
  assert_true(azimuth::IntUtils::almost_equal(actual_trace_count, expected_severity_count, expected_severity_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_debug_count, expected_severity_count, expected_severity_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_info_count, expected_severity_count, expected_severity_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_warn_count, expected_severity_count, expected_severity_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_error_count, expected_severity_count, expected_severity_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_fatal_count, expected_severity_count, expected_severity_count / 10))
  
  // 验证所有任务都成功完成
  for result in task_results {
    assert_true(azimuth::TaskResult::is_success(result))
    assert_eq(azimuth::TaskResult::get_result(result), logs_per_thread)
  }
  
  // 获取聚合的日志数据
  let log_statistics = azimuth::LoggerProvider::get_log_statistics(logger)
  
  // 验证日志统计
  assert_eq(azimuth::LogStatistics::get_total_logs(log_statistics), total_logs)
  
  // 验证按严重级别的日志计数
  assert_eq(azimuth::LogStatistics::get_log_count_by_severity(log_statistics, azimuth::Trace), actual_trace_count)
  assert_eq(azimuth::LogStatistics::get_log_count_by_severity(log_statistics, azimuth::Debug), actual_debug_count)
  assert_eq(azimuth::LogStatistics::get_log_count_by_severity(log_statistics, azimuth::Info), actual_info_count)
  assert_eq(azimuth::LogStatistics::get_log_count_by_severity(log_statistics, azimuth::Warn), actual_warn_count)
  assert_eq(azimuth::LogStatistics::get_log_count_by_severity(log_statistics, azimuth::Error), actual_error_count)
  assert_eq(azimuth::LogStatistics::get_log_count_by_severity(log_statistics, azimuth::Fatal), actual_fatal_count)
  
  // 创建并发日志性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrency-logging")
  let log_rate_gauge = azimuth::Meter::create_gauge(meter, "log.emission.rate")
  let log_latency_histogram = azimuth::Meter::create_histogram(meter, "log.processing.latency", Some("Log processing latency"), Some("ms"))
  
  // 计算性能指标
  let log_rate = total_logs.to_double() / (test_duration.to_double() / 1000000000.0)  // 日志/秒
  
  azimuth::Gauge::record(log_rate_gauge, log_rate)
  azimuth::Histogram::record(log_latency_histogram, test_duration.to_double() / 1000000.0)
  
  // 验证性能指标
  assert_true(log_rate > 1000.0)  // 至少每秒1000条日志
}

// Test 4: 并发上下文传播一致性测试
pub test "并发上下文传播一致性测试" {
  // 创建并发上下文传播测试环境
  let concurrency_level = 25
  let context_depth = 5
  
  // 创建同步机制
  let context_counter = azimuth::AtomicCounter::new(0)
  let propagated_contexts = azimuth::ConcurrentList::new()
  let context_values = azimuth::ConcurrentMap::new()
  
  // 记录测试开始时间
  let test_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建根上下文
  let root_context = azimuth::Context::root()
  let root_key = azimuth::ContextKey::new("root.key")
  let root_context_with_value = azimuth::Context::with_value(root_context, root_key, "root.value")
  
  // 创建并发任务
  let concurrent_tasks = []
  for thread_id in 0..concurrency_level {
    let task = azimuth::ConcurrentTask::new(fn() {
      // 每个线程创建上下文链
      let thread_context = azimuth::Context::with_value(root_context_with_value, 
        azimuth::ContextKey::new("thread.id"), 
        thread_id.to_string())
      
      let context_chain = [thread_context]
      
      // 创建多层嵌套上下文
      let current_context = thread_context
      for depth in 0..context_depth {
        let key = azimuth::ContextKey::new("depth." + depth.to_string())
        let value = "thread-" + thread_id.to_string() + "-depth-" + depth.to_string()
        current_context = azimuth::Context::with_value(current_context, key, value)
        context_chain.push(current_context)
      }
      
      // 验证上下文链的完整性
      let context_data = {
        "thread_id": thread_id,
        "context_chain_length": context_chain.length,
        "root_value": azimuth::Context::get(context_chain[context_chain.length - 1], root_key),
        "thread_id_value": azimuth::Context::get(context_chain[context_chain.length - 1], azimuth::ContextKey::new("thread.id")),
        "depth_values": []
      }
      
      // 验证每个深度的值
      for depth in 0..context_depth {
        let key = azimuth::ContextKey::new("depth." + depth.to_string())
        let value = azimuth::Context::get(context_chain[context_chain.length - 1], key)
        context_data["depth_values"].push({
          "depth": depth,
          "value": value
        })
        
        // 线程安全地记录上下文值
        let value_key = "thread-" + thread_id.to_string() + "-depth-" + depth.to_string()
        azimuth::ConcurrentMap::set(context_values, value_key, value)
      }
      
      // 线程安全地记录传播的上下文
      azimuth::ConcurrentList::add(propagated_contexts, context_data)
      
      // 原子递增上下文计数器
      azimuth::AtomicCounter::increment(context_counter)
      
      // 模拟处理时间
      azimuth::Clock::sleep(10)
      
      context_chain.length
    })
    
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let task_results = azimuth::ConcurrentTaskExecutor::execute_all(concurrent_tasks)
  
  // 记录测试结束时间
  let test_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let test_duration = test_end_time - test_start_time
  
  // 验证并发执行结果
  let total_contexts = azimuth::AtomicCounter::get(context_counter)
  let total_propagated_contexts = azimuth::ConcurrentList::size(propagated_contexts)
  
  // 验证上下文数量
  assert_eq(total_contexts, concurrency_level)
  assert_eq(total_propagated_contexts, concurrency_level)
  
  // 验证所有任务都成功完成
  for result in task_results {
    assert_true(azimuth::TaskResult::is_success(result))
    assert_eq(azimuth::TaskResult::get_result(result), context_depth + 1)  // 根上下文 + 深度上下文
  }
  
  // 验证上下文传播的完整性
  let propagated_contexts_list = azimuth::ConcurrentList::to_array(propagated_contexts)
  
  for context_data in propagated_contexts_list {
    // 验证根值传播
    assert_eq(context_data["root_value"], Some("root.value"))
    
    // 验证线程ID传播
    assert_eq(context_data["thread_id_value"], Some(context_data["thread_id"].to_string()))
    
    // 验证深度值传播
    for depth_value in context_data["depth_values"] {
      let expected_value = "thread-" + context_data["thread_id"].to_string() + "-depth-" + depth_value["depth"].to_string()
      assert_eq(depth_value["value"], Some(expected_value))
      
      // 验证上下文值映射中的值
      let value_key = "thread-" + context_data["thread_id"].to_string() + "-depth-" + depth_value["depth"].to_string()
      let stored_value = azimuth::ConcurrentMap::get(context_values, value_key)
      assert_eq(stored_value, Some(expected_value))
    }
  }
  
  // 测试并发Baggage传播
  let baggage_tasks = []
  let baggage_results = azimuth::ConcurrentList::new()
  
  for thread_id in 0..concurrency_level {
    let task = azimuth::ConcurrentTask::new(fn() {
      // 创建Baggage
      let baggage = azimuth::Baggage::new()
      let updated_baggage = azimuth::Baggage::set_entry(baggage, "thread.id", thread_id.to_string())
      
      // 添加更多条目
      let final_baggage = azimuth::Baggage::set_entry(updated_baggage, "thread.data", "data-" + thread_id.to_string())
      
      // 验证Baggage内容
      let thread_id_value = azimuth::Baggage::get_entry(final_baggage, "thread.id")
      let thread_data_value = azimuth::Baggage::get_entry(final_baggage, "thread.data")
      
      let result = {
        "thread_id": thread_id,
        "thread_id_value": thread_id_value,
        "thread_data_value": thread_data_value
      }
      
      // 线程安全地记录结果
      azimuth::ConcurrentList::add(baggage_results, result)
      
      1  // 返回1表示成功
    })
    
    baggage_tasks.push(task)
  }
  
  // 执行Baggage任务
  let baggage_task_results = azimuth::ConcurrentTaskExecutor::execute_all(baggage_tasks)
  
  // 验证Baggage传播结果
  let baggage_results_list = azimuth::ConcurrentList::to_array(baggage_results)
  
  for result in baggage_results_list {
    assert_eq(result["thread_id_value"], Some(result["thread_id"].to_string()))
    assert_eq(result["thread_data_value"], Some("data-" + result["thread_id"].to_string()))
  }
  
  // 验证所有Baggage任务都成功完成
  for result in baggage_task_results {
    assert_true(azimuth::TaskResult::is_success(result))
    assert_eq(azimuth::TaskResult::get_result(result), 1)
  }
  
  // 创建并发上下文性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrency-context")
  let context_creation_rate_gauge = azimuth::Meter::create_gauge(meter, "context.creation.rate")
  let context_propagation_latency_histogram = azimuth::Meter::create_histogram(meter, "context.propagation.latency", Some("Context propagation latency"), Some("ms"))
  
  // 计算性能指标
  let context_creation_rate = total_contexts.to_double() / (test_duration.to_double() / 1000000000.0)  // 上下文/秒
  
  azimuth::Gauge::record(context_creation_rate_gauge, context_creation_rate)
  azimuth::Histogram::record(context_propagation_latency_histogram, test_duration.to_double() / 1000000.0)
  
  // 验证性能指标
  assert_true(context_creation_rate > 10.0)  // 至少每秒10个上下文
}

// Test 5: 并发资源管理一致性测试
pub test "并发资源管理一致性测试" {
  // 创建并发资源管理测试环境
  let concurrency_level = 20
  let resources_per_thread = 50
  
  // 创建同步机制
  let resource_counter = azimuth::AtomicCounter::new(0)
  let created_resources = azimuth::ConcurrentList::new()
  let resource_types = azimuth::ConcurrentMap::new()
  
  // 记录测试开始时间
  let test_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建并发任务
  let concurrent_tasks = []
  for thread_id in 0..concurrency_level {
    let task = azimuth::ConcurrentTask::new(fn() {
      let thread_resources = []
      
      for resource_id in 0..resources_per_thread {
        // 创建不同类型的资源
        let resource_type = match resource_id % 4 {
          0 => "tracer",
          1 => "meter",
          2 => "logger",
          _ => "resource"
        }
        
        let resource_name = "concurrent-" + resource_type + "-" + thread_id.to_string() + "-" + resource_id.to_string()
        
        let resource = match resource_type {
          "tracer" => {
            let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), resource_name)
            {
              "type": "tracer",
              "name": resource_name,
              "instance": tracer
            }
          }
          "meter" => {
            let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), resource_name)
            {
              "type": "meter",
              "name": resource_name,
              "instance": meter
            }
          }
          "logger" => {
            let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), resource_name)
            {
              "type": "logger",
              "name": resource_name,
              "instance": logger
            }
          }
          _ => {
            let resource = azimuth::Resource::new()
            {
              "type": "resource",
              "name": resource_name,
              "instance": resource
            }
          }
        }
        
        thread_resources.push(resource)
        
        // 线程安全地记录创建的资源
        azimuth::ConcurrentList::add(created_resources, {
          "thread_id": thread_id,
          "resource_id": resource_id,
          "resource_type": resource_type,
          "resource_name": resource_name,
          "creation_time": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
        })
        
        // 线程安全地更新资源类型计数
        azimuth::ConcurrentMap::increment(resource_types, resource_type)
        
        // 原子递增资源计数器
        azimuth::AtomicCounter::increment(resource_counter)
        
        // 模拟处理时间
        azimuth::Clock::sleep(2)
      }
      
      // 使用资源进行操作
      for resource in thread_resources {
        match resource["type"] {
          "tracer" => {
            let tracer = resource["instance"] as azimuth::Tracer
            let span = azimuth::Tracer::start_span(tracer, "resource-test-span")
            azimuth::Span::add_event(span, "resource.test", Some([
              ("resource.type", azimuth::StringValue("tracer"))
            ]))
            azimuth::Span::end(span)
          }
          "meter" => {
            let meter = resource["instance"] as azimuth::Meter
            let counter = azimuth::Meter::create_counter(meter, "resource-test-counter")
            azimuth::Counter::add(counter, 1.0)
          }
          "logger" => {
            let logger = resource["instance"] as azimuth::Logger
            let log_record = azimuth::LogRecord::new(azimuth::Info, "Resource test log")
            azimuth::Logger::emit(logger, log_record)
          }
          _ => {
            let resource = resource["instance"] as azimuth::Resource
            let _ = azimuth::Resource::with_attributes(resource, [
              ("test.attribute", azimuth::StringValue("test.value"))
            ])
          }
        }
      }
      
      thread_resources.length()
    })
    
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let task_results = azimuth::ConcurrentTaskExecutor::execute_all(concurrent_tasks)
  
  // 记录测试结束时间
  let test_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let test_duration = test_end_time - test_start_time
  
  // 验证并发执行结果
  let total_resources = azimuth::AtomicCounter::get(resource_counter)
  let total_created_resources = azimuth::ConcurrentList::size(created_resources)
  
  // 验证资源数量
  assert_eq(total_resources, concurrency_level * resources_per_thread)
  assert_eq(total_created_resources, concurrency_level * resources_per_thread)
  
  // 验证所有任务都成功完成
  for result in task_results {
    assert_true(azimuth::TaskResult::is_success(result))
    assert_eq(azimuth::TaskResult::get_result(result), resources_per_thread)
  }
  
  // 验证资源类型分布
  let expected_type_count = (concurrency_level * resources_per_thread) / 4
  let actual_tracer_count = azimuth::ConcurrentMap::get(resource_types, "tracer")
  let actual_meter_count = azimuth::ConcurrentMap::get(resource_types, "meter")
  let actual_logger_count = azimuth::ConcurrentMap::get(resource_types, "logger")
  let actual_resource_count = azimuth::ConcurrentMap::get(resource_types, "resource")
  
  // 验证资源类型计数（允许小的误差）
  assert_true(azimuth::IntUtils::almost_equal(actual_tracer_count, expected_type_count, expected_type_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_meter_count, expected_type_count, expected_type_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_logger_count, expected_type_count, expected_type_count / 10))
  assert_true(azimuth::IntUtils::almost_equal(actual_resource_count, expected_type_count, expected_type_count / 10))
  
  // 验证资源名称唯一性
  let created_resources_list = azimuth::ConcurrentList::to_array(created_resources)
  let resource_names = azimuth::ConcurrentSet::new()
  
  for resource in created_resources_list {
    azimuth::ConcurrentSet::add(resource_names, resource["resource_name"])
  }
  
  assert_eq(azimuth::ConcurrentSet::size(resource_names), total_created_resources)  // 所有资源名称应该是唯一的
  
  // 验证资源创建时间顺序
  let sorted_resources = created_resources_list.sort((a, b) => a["creation_time"] - b["creation_time"])
  
  // 检查时间戳是否单调递增（允许并发导致的小范围乱序）
  let out_of_order_count = 0
  for i in 1..sorted_resources.length() {
    if sorted_resources[i]["creation_time"] < sorted_resources[i-1]["creation_time"] {
      out_of_order_count = out_of_order_count + 1
    }
  }
  
  // 允许少量乱序（由于并发）
  assert_true(out_of_order_count < total_created_resources / 10)
  
  // 创建并发资源管理性能度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "concurrency-resources")
  let resource_creation_rate_gauge = azimuth::Meter::create_gauge(meter, "resource.creation.rate")
  let resource_management_latency_histogram = azimuth::Meter::create_histogram(meter, "resource.management.latency", Some("Resource management latency"), Some("ms"))
  
  // 计算性能指标
  let resource_creation_rate = total_resources.to_double() / (test_duration.to_double() / 1000000000.0)  // 资源/秒
  
  azimuth::Gauge::record(resource_creation_rate_gauge, resource_creation_rate)
  azimuth::Histogram::record(resource_management_latency_histogram, test_duration.to_double() / 1000000.0)
  
  // 验证性能指标
  assert_true(resource_creation_rate > 50.0)  // 至少每秒50个资源
}