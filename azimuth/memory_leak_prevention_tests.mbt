// 内存泄漏防护测试
// 测试遥测系统的内存管理和泄漏防护机制

import "azimuth/azimuth"

pub test "Span生命周期内存管理测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "memory-leak-test")
  
  // 记录初始内存状态
  let initial_memory = azimuth::MemoryMonitor::current_usage()
  let initial_span_count = azimuth::MemoryMonitor::active_span_count()
  
  // 创建大量Span并测试清理
  let spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "memory-test-span-" + i.to_string())
    
    // 添加大量属性和事件
    azimuth::Span::add_event(span, "test.event", Some([
      ("iteration", azimuth::StringValue(i.to_string())),
      ("large.payload", azimuth::StringValue("x".repeat(1000)))  // 1KB payload
    ]))
    
    spans.push(span)
  }
  
  // 验证Span创建后的内存使用
  let peak_memory = azimuth::MemoryMonitor::current_usage()
  let peak_span_count = azimuth::MemoryMonitor::active_span_count()
  
  assert_true(peak_memory > initial_memory)
  assert_eq(peak_span_count, initial_span_count + 1000)
  
  // 结束所有Span并触发垃圾回收
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 强制内存清理
  azimuth::MemoryMonitor::force_garbage_collection()
  
  // 验证内存恢复
  let final_memory = azimuth::MemoryMonitor::current_usage()
  let final_span_count = azimuth::MemoryMonitor::active_span_count()
  
  // 内存应该恢复到接近初始状态
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < initial_memory / 10)  // 增长不超过10%
  assert_eq(final_span_count, initial_span_count)     // 所有Span应该被清理
}

pub test "度量数据内存管理测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "memory-metrics")
  
  let initial_memory = azimuth::MemoryMonitor::current_usage()
  
  // 创建大量度量仪器
  let instruments = []
  for i in 0..500 {
    let counter = azimuth::Meter::create_counter(meter, "counter." + i.to_string())
    let histogram = azimuth::Meter::create_histogram(meter, "histogram." + i.to_string())
    let gauge = azimuth::Meter::create_gauge(meter, "gauge." + i.to_string())
    
    instruments.push(counter)
    instruments.push(histogram)
    instruments.push(gauge)
  }
  
  // 记录大量度量数据
  for i in 0..5000 {
    let counter_index = i % 500
    let counter = instruments[counter_index * 3]  // 每3个仪器中的第一个是counter
    azimuth::Counter::add(counter, i.to_double())
    
    let histogram_index = i % 500
    let histogram = instruments[histogram_index * 3 + 1]  // 每组中的第二个是histogram
    azimuth::Histogram::record(histogram, i.to_double() * 0.1)
  }
  
  let peak_memory = azimuth::MemoryMonitor::current_usage()
  
  // 清理度量数据
  azimuth::MeterProvider::reset_metrics(meter_provider)
  azimuth::MemoryMonitor::force_garbage_collection()
  
  let final_memory = azimuth::MemoryMonitor::current_usage()
  
  // 验证内存恢复
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < initial_memory / 5)  // 增长不超过20%
}

pub test "日志记录内存管理测试" {
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "memory-logger")
  
  let initial_memory = azimuth::MemoryMonitor::current_usage()
  
  // 创建大量日志记录
  for i in 0..2000 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Memory test log entry " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("memory-test-trace"),
      Some("memory-test-span"),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
  }
  
  let peak_memory = azimuth::MemoryMonitor::current_usage()
  
  // 清理日志缓冲区
  azimuth::LoggerProvider::clear_buffers(azimuth::LoggerProvider::default())
  azimuth::MemoryMonitor::force_garbage_collection()
  
  let final_memory = azimuth::MemoryMonitor::current_usage()
  
  // 验证内存恢复
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < initial_memory / 8)  // 增长不超过12.5%
}

pub test "属性和上下文内存泄漏测试" {
  let initial_memory = azimuth::MemoryMonitor::current_usage()
  
  // 创建大量属性对象
  let attributes_list = []
  for i in 0..100 {
    let attrs = azimuth::Attributes::new()
    
    // 添加大量属性
    for j in 0..100 {
      let key = "attr." + i.to_string() + "." + j.to_string()
      let value = azimuth::StringValue("value-" + i.to_string() + "-" + j.to_string() + "-".repeat(100))
      azimuth::Attributes::set(attrs, key, value)
    }
    
    attributes_list.push(attrs)
  }
  
  // 创建大量上下文对象
  let contexts = []
  for i in 0..100 {
    let ctx = azimuth::Context::root()
    let key = azimuth::ContextKey::new("context.key." + i.to_string())
    let ctx_with_value = azimuth::Context::with_value(ctx, key, "context-value-".repeat(100))
    contexts.push(ctx_with_value)
  }
  
  let peak_memory = azimuth::MemoryMonitor::current_usage()
  
  // 清理对象
  attributes_list = []  // 清空数组
  contexts = []         // 清空数组
  azimuth::MemoryMonitor::force_garbage_collection()
  
  let final_memory = azimuth::MemoryMonitor::current_usage()
  
  // 验证内存恢复
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < initial_memory / 3)  // 增长不超过33%
}

pub test "传播器内存管理测试" {
  let initial_memory = azimuth::MemoryMonitor::current_usage()
  
  // 创建多个传播器实例
  let propagators = []
  for i in 0..50 {
    let trace_propagator = azimuth::W3CTraceContextPropagator::new()
    let baggage_propagator = azimuth::W3CBaggagePropagator::new()
    let composite_propagator = azimuth::CompositePropagator::new([trace_propagator, baggage_propagator])
    propagators.push(composite_propagator)
  }
  
  // 执行大量传播操作
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::root()
  
  for propagator in propagators {
    for i in 0..100 {
      azimuth::CompositePropagator::inject(propagator, ctx, carrier)
      let extracted_ctx = azimuth::CompositePropagator::extract(propagator, carrier)
    }
  }
  
  let peak_memory = azimuth::MemoryMonitor::current_usage()
  
  // 清理传播器
  propagators = []
  azimuth::MemoryMonitor::force_garbage_collection()
  
  let final_memory = azimuth::MemoryMonitor::current_usage()
  
  // 验证内存恢复
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < initial_memory / 4)  // 增长不超过25%
}

pub test "长期运行内存稳定性测试" {
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "stability-test")
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "stability-meter")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "stability-logger")
  
  let counter = azimuth::Meter::create_counter(meter, "stability.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "stability.histogram")
  
  let initial_memory = azimuth::MemoryMonitor::current_usage()
  let memory_samples = []
  
  // 模拟长期运行场景
  for cycle in 0..100 {
    // 每个周期创建和销毁资源
    let spans = []
    for i in 0..50 {
      let span = azimuth::Tracer::start_span(tracer, "cycle-" + cycle.to_string() + "-span-" + i.to_string())
      azimuth::Span::add_event(span, "cycle.event", Some([("cycle", azimuth::StringValue(cycle.to_string()))]))
      spans.push(span)
    }
    
    // 记录度量
    for i in 0..100 {
      azimuth::Counter::add(counter, 1.0)
      azimuth::Histogram::record(histogram, i.to_double())
    }
    
    // 发出日志
    for i in 0..20 {
      let log_record = azimuth::LogRecord::new(azimuth::Info, "Cycle " + cycle.to_string() + " log " + i.to_string())
      azimuth::Logger::emit(logger, log_record)
    }
    
    // 清理Span
    for span in spans {
      azimuth::Span::end(span)
    }
    
    // 每10个周期记录一次内存使用
    if (cycle % 10 == 0) {
      let current_memory = azimuth::MemoryMonitor::current_usage()
      memory_samples.push(current_memory)
      
      // 每20个周期执行一次垃圾回收
      if (cycle % 20 == 0) {
        azimuth::MemoryMonitor::force_garbage_collection()
      }
    }
  }
  
  // 最终清理
  azimuth::MemoryMonitor::force_garbage_collection()
  let final_memory = azimuth::MemoryMonitor::current_usage()
  
  // 分析内存稳定性
  let max_memory = memory_samples.reduce(initial_memory, fn(acc, mem) { if (mem > acc) mem else acc })
  let min_memory = memory_samples.reduce(final_memory, fn(acc, mem) { if (mem < acc) mem else acc })
  let memory_variance = max_memory - min_memory
  
  // 验证内存稳定性
  let memory_increase = final_memory - initial_memory
  assert_true(memory_increase < initial_memory / 2)      // 总增长不超过50%
  assert_true(memory_variance < initial_memory / 3)      // 方差不超过33%
}