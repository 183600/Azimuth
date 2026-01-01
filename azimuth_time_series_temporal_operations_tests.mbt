// 时间序列和时序操作测试
// 测试Azimuth遥测系统的时间序列数据处理和时序操作功能

// 测试1: 时间戳生成和精度测试
pub test "时间戳生成和精度测试" {
  let clock = azimuth::Clock::system()
  
  // 测试时间戳生成
  let timestamp1 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp2 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp3 = azimuth::Clock::now_unix_nanos(clock)
  
  // 验证时间戳格式（纳秒级精度）
  assert_true(timestamp1 > 0L)
  assert_true(timestamp2 > 0L)
  assert_true(timestamp3 > 0L)
  
  // 验证时间戳长度（至少16位数字，表示纳秒级精度）
  assert_true(timestamp1.toString().length() >= 16)
  assert_true(timestamp2.toString().length() >= 16)
  assert_true(timestamp3.toString().length() >= 16)
  
  // 验证时间戳单调递增
  assert_true(timestamp2 >= timestamp1)
  assert_true(timestamp3 >= timestamp2)
  
  // 测试时间戳精度
  let precision_start = azimuth::Clock::now_unix_nanos(clock)
  
  // 连续获取时间戳
  let precision_timestamps = []
  for i in 0..100 {
    let ts = azimuth::Clock::now_unix_nanos(clock)
    precision_timestamps.push(ts)
  }
  
  let precision_end = azimuth::Clock::now_unix_nanos(clock)
  
  // 验证时间戳精度变化
  let min_precision_ts = precision_timestamps[0]
  let max_precision_ts = precision_timestamps[99]
  
  assert_true(max_precision_ts >= min_precision_ts)
  assert_true((max_precision_ts - min_precision_ts) < (precision_end - precision_start))
  
  // 测试时间戳纳秒级精度
  let nano_start = azimuth::Clock::now_unix_nanos(clock)
  let nano_end = azimuth::Clock::now_unix_nanos(clock)
  
  // 验证纳秒级精度（即使两次调用间隔很小，也应该能检测到差异）
  assert_true(nano_end >= nano_start)
}

// 测试2: 时间差计算测试
pub test "时间差计算测试" {
  let clock = azimuth::Clock::system()
  
  // 测试基本时间差计算
  let start_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 模拟一些操作
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "test.key", azimuth::StringValue("test.value"))
  
  let mid_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 模拟更多操作
  let span_ctx = azimuth::SpanContext::new("trace-time-test", "span-time-test", true, "")
  let span = azimuth::Span::new("time-test-span", azimuth::Internal, span_ctx)
  azimuth::Span::add_event(span, "time.test.event", Some([("test.attr", azimuth::StringValue("test.value"))]))
  
  let end_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 计算时间差
  let total_duration = end_time - start_time
  let first_half_duration = mid_time - start_time
  let second_half_duration = end_time - mid_time
  
  // 验证时间差计算
  assert_true(total_duration > 0L)
  assert_true(first_half_duration > 0L)
  assert_true(second_half_duration > 0L)
  assert_true(total_duration == first_half_duration + second_half_duration)
  
  // 测试时间差精度
  let precision_start = azimuth::Clock::now_unix_nanos(clock)
  let precision_end = azimuth::Clock::now_unix_nanos(clock)
  let precision_diff = precision_end - precision_start
  
  // 验证时间差精度（纳秒级）
  assert_true(precision_diff >= 0L)
  
  // 测试大时间差
  let large_start = azimuth::Clock::now_unix_nanos(clock)
  
  // 模拟较长时间的操作
  let large_attrs = azimuth::Attributes::new()
  for i in 0..1000 {
    azimuth::Attributes::set(large_attrs, "key." + i.to_string(), azimuth::StringValue("value." + i.to_string()))
  }
  
  let large_end = azimuth::Clock::now_unix_nanos(clock)
  let large_duration = large_end - large_start
  
  // 验证大时间差
  assert_true(large_duration > 0L)
  assert_true(large_duration > precision_diff)  // 应该比精度测试的时间差大
}

// 测试3: 时间序列日志记录测试
pub test "时间序列日志记录测试" {
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "time-series-test-logger")
  let clock = azimuth::Clock::system()
  
  // 创建时间序列日志
  let log_timestamps = []
  let log_messages = []
  let base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建时间序列日志（每1毫秒间隔）
  for i in 0..10 {
    let log_time = base_time + (i * 1000000L)  // 每个日志间隔1毫秒
    let message = "Time series log " + i.to_string()
    
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some(message),
      Some(azimuth::Attributes::new()),
      Some(log_time),
      Some(log_time + 1000L),  // observed_time稍晚
      Some("time-series-trace"),
      Some("time-series-span"),
      Some(azimuth::Context::root())
    )
    
    log_timestamps.push(log_time)
    log_messages.push(message)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证时间序列的顺序性
  for i in 1..log_timestamps.length() {
    assert_true(log_timestamps[i] > log_timestamps[i-1])
    
    // 验证时间间隔（大约1毫秒）
    let expected_interval = 1000000L  // 1毫秒
    let actual_interval = log_timestamps[i] - log_timestamps[i-1]
    assert_true(actual_interval >= expected_interval)
  }
  
  // 验证日志消息与时间戳的对应关系
  for i in 0..log_messages.length() {
    let expected_message = "Time series log " + i.to_string()
    assert_eq(log_messages[i], expected_message)
  }
  
  // 测试不同时间间隔的日志序列
  let variable_interval_timestamps = []
  let variable_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建可变间隔的日志序列
  let intervals = [500000L, 1000000L, 2000000L, 500000L, 3000000L]  // 0.5ms, 1ms, 2ms, 0.5ms, 3ms
  let cumulative_time = variable_base_time
  
  for i in 0..intervals.length() {
    let log_time = cumulative_time + intervals[i]
    variable_interval_timestamps.push(log_time)
    
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Debug,
      Some("Variable interval log " + i.to_string()),
      None,
      Some(log_time),
      None,
      None,
      None,
      None
    )
    
    azimuth::Logger::emit(logger, log_record)
    cumulative_time = log_time
  }
  
  // 验证可变间隔的顺序性
  for i in 1..variable_interval_timestamps.length() {
    assert_true(variable_interval_timestamps[i] > variable_interval_timestamps[i-1])
  }
}

// 测试4: 时间序列度量操作测试
pub test "时间序列度量操作测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "time-series-meter")
  let clock = azimuth::Clock::system()
  
  // 创建时间序列度量
  let histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  let counter = azimuth::Meter::create_counter(meter, "request.count", Some("Total request count"), Some("requests"))
  
  let measurement_times = []
  let measurement_values = []
  let count_times = []
  
  // 创建时间序列度量（递增的响应时间）
  let base_time = azimuth::Clock::now_unix_nanos(clock)
  
  for i in 0..20 {
    let measure_time = azimuth::Clock::now_unix_nanos(clock)
    let value = 100.0 + (i.to_double() * 10.0)  // 递增的响应时间：100ms, 110ms, 120ms...
    
    azimuth::Histogram::record(histogram, value)
    azimuth::Counter::add(counter, 1.0)
    
    measurement_times.push(measure_time)
    measurement_values.push(value)
    count_times.push(measure_time)
  }
  
  // 验证度量时间序列
  assert_true(measurement_times.length() == 20)
  assert_true(measurement_values.length() == 20)
  assert_true(count_times.length() == 20)
  
  // 验证测量值的时间顺序性
  for i in 1..measurement_times.length() {
    assert_true(measurement_times[i] >= measurement_times[i-1])
  }
  
  // 验证测量值的递增性
  for i in 1..measurement_values.length() {
    assert_true(measurement_values[i] > measurement_values[i-1])
  }
  
  // 验证计数时间的时间顺序性
  for i in 1..count_times.length() {
    assert_true(count_times[i] >= count_times[i-1])
  }
  
  // 测试周期性度量模式
  let periodic_histogram = azimuth::Meter::create_histogram(meter, "periodic.metric", Some("Periodic metric"), Some("units"))
  let periodic_times = []
  let periodic_values = []
  
  // 创建周期性度量模式（模拟周期性负载）
  for cycle in 0..5 {
    for i in 0..10 {
      let measure_time = azimuth::Clock::now_unix_nanos(clock)
      let value = 50.0 + (i.to_double() * 20.0)  // 50, 70, 90, ..., 230
      
      azimuth::Histogram::record(periodic_histogram, value)
      
      periodic_times.push(measure_time)
      periodic_values.push(value)
    }
  }
  
  // 验证周期性模式
  assert_true(periodic_times.length() == 50)
  assert_true(periodic_values.length() == 50)
  
  // 验证每个周期的模式
  for cycle in 0..5 {
    let cycle_start = cycle * 10
    let cycle_end = (cycle + 1) * 10
    
    // 验证周期内的递增模式
    for i in cycle_start..(cycle_end - 1) {
      if i + 1 < cycle_end {
        assert_true(periodic_values[i + 1] > periodic_values[i])
      }
    }
  }
}

// 测试5: Span时间序列操作测试
pub test "Span时间序列操作测试" {
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "time-series-tracer")
  let clock = azimuth::Clock::system()
  
  // 创建时间序列Span
  let span_start_times = []
  let span_end_times = []
  let span_durations = []
  
  // 创建多个Span并记录时间
  for i in 0..10 {
    let start_time = azimuth::Clock::now_unix_nanos(clock)
    let span = azimuth::Tracer::start_span(tracer, "time-series-span-" + i.to_string())
    
    // 模拟Span持续时间（递增）
    let work_duration = 1000000L * (i + 1)  // 1ms, 2ms, 3ms, ..., 10ms
    let simulated_end_time = start_time + work_duration
    
    // 添加事件
    azimuth::Span::add_event(span, "span.event", Some([("iteration", azimuth::StringValue(i.to_string()))]))
    
    let end_time = azimuth::Clock::now_unix_nanos(clock)
    azimuth::Span::end(span)
    
    span_start_times.push(start_time)
    span_end_times.push(end_time)
    span_durations.push(end_time - start_time)
  }
  
  // 验证Span时间序列
  assert_true(span_start_times.length() == 10)
  assert_true(span_end_times.length() == 10)
  assert_true(span_durations.length() == 10)
  
  // 验证Span开始时间的顺序性
  for i in 1..span_start_times.length() {
    assert_true(span_start_times[i] >= span_start_times[i-1])
  }
  
  // 验证Span结束时间的顺序性
  for i in 1..span_end_times.length() {
    assert_true(span_end_times[i] >= span_end_times[i-1])
  }
  
  // 验证Span持续时间的合理性
  for i in 0..span_durations.length() {
    assert_true(span_durations[i] > 0L)
  }
  
  // 测试嵌套Span的时间序列
  let parent_span = azimuth::Tracer::start_span(tracer, "parent-span")
  let parent_start = azimuth::Clock::now_unix_nanos(clock)
  
  let child_spans_data = []
  
  for i in 0..5 {
    let child_start = azimuth::Clock::now_unix_nanos(clock)
    let child_span = azimuth::Tracer::start_span(tracer, "child-span-" + i.to_string())
    
    // 模拟子Span工作
    let child_work = 500000L  // 0.5ms
    let child_simulated_end = child_start + child_work
    
    azimuth::Span::add_event(child_span, "child.event", Some([("child.id", azimuth::StringValue(i.to_string()))]))
    
    let child_end = azimuth::Clock::now_unix_nanos(clock)
    azimuth::Span::end(child_span)
    
    child_spans_data.push((child_start, child_end, child_end - child_start))
  }
  
  let parent_end = azimuth::Clock::now_unix_nanos(clock)
  azimuth::Span::end(parent_span)
  
  // 验证嵌套Span的时间关系
  let parent_duration = parent_end - parent_start
  
  for (child_start, child_end, child_duration) in child_spans_data {
    // 子Span应该在父Span开始之后开始
    assert_true(child_start >= parent_start)
    
    // 子Span应该在父Span结束之前结束
    assert_true(child_end <= parent_end)
    
    // 子Span持续时间应该为正
    assert_true(child_duration > 0L)
  }
}

// 测试6: 时间窗口聚合测试
pub test "时间窗口聚合测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "time-window-meter")
  let clock = azimuth::Clock::system()
  
  // 创建时间窗口聚合测试
  let window_histogram = azimuth::Meter::create_histogram(meter, "window.metric", Some("Window metric"), Some("units"))
  let window_counter = azimuth::Meter::create_counter(meter, "window.count", Some("Window count"), Some("items"))
  
  let window_measurements = []
  let window_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 模拟时间窗口内的度量（1秒窗口）
  let window_size = 1000000000L  // 1秒
  let measurements_per_window = 100
  
  for window in 0..5 {
    let window_start = window_base_time + (window * window_size)
    
    for i in 0..measurements_per_window {
      let measure_time = window_start + (i * (window_size / measurements_per_window))
      let value = 100.0 + (i.to_double() * 2.0)  // 递增值
      
      azimuth::Histogram::record(window_histogram, value)
      azimuth::Counter::add(window_counter, 1.0)
      
      window_measurements.push((measure_time, value, window))
    }
  }
  
  // 验证时间窗口聚合
  assert_true(window_measurements.length() == 500)  // 5个窗口 × 100个测量
  
  // 验证每个窗口内的测量
  for window in 0..5 {
    let window_start = window_base_time + (window * window_size)
    let window_end = window_start + window_size
    
    // 统计窗口内的测量
    let window_count = 0
    let window_sum = 0.0
    
    for (measure_time, value, measure_window) in window_measurements {
      if measure_window == window {
        window_count = window_count + 1
        window_sum = window_sum + value
        
        // 验证测量时间在窗口范围内
        assert_true(measure_time >= window_start)
        assert_true(measure_time < window_end)
      }
    }
    
    // 验证窗口统计
    assert_eq(window_count, measurements_per_window)
    
    // 验证窗口内值的总和
    let expected_sum = 0.0
    for i in 0..measurements_per_window {
      expected_sum = expected_sum + (100.0 + (i.to_double() * 2.0))
    }
    
    assert_true(window_sum == expected_sum)
  }
  
  // 测试滑动时间窗口
  let sliding_histogram = azimuth::Meter::create_histogram(meter, "sliding.metric", Some("Sliding window metric"), Some("units"))
  let sliding_measurements = []
  let sliding_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建滑动窗口测量（重叠窗口）
  let sliding_window_size = 500000000L  // 0.5秒
  let sliding_step = 250000000L  // 0.25秒步长
  
  for step in 0..10 {
    let window_start = sliding_base_time + (step * sliding_step)
    
    // 在每个滑动窗口内添加测量
    for i in 0..20 {
      let measure_time = window_start + (i * (sliding_window_size / 20))
      let value = 50.0 + (step.to_double() * 5.0) + (i.to_double() * 2.5)
      
      azimuth::Histogram::record(sliding_histogram, value)
      sliding_measurements.push((measure_time, value, step))
    }
  }
  
  // 验证滑动窗口测量
  assert_true(sliding_measurements.length() == 200)  // 10个滑动窗口 × 20个测量
  
  // 验证滑动窗口的重叠特性
  for step in 1..10 {
    let prev_window_start = sliding_base_time + ((step - 1) * sliding_step)
    let curr_window_start = sliding_base_time + (step * sliding_step)
    
    // 验证窗口重叠
    assert_true(curr_window_start < prev_window_start + sliding_window_size)
  }
}

// 测试7: 时间序列趋势分析测试
pub test "时间序列趋势分析测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "trend-analysis-meter")
  let clock = azimuth::Clock::system()
  
  // 创建趋势分析测试
  let trend_histogram = azimuth::Meter::create_histogram(meter, "trend.metric", Some("Trend analysis metric"), Some("units"))
  
  let trend_measurements = []
  let trend_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建具有明显趋势的时间序列（线性增长）
  for i in 0..100 {
    let measure_time = trend_base_time + (i * 10000000L)  // 每10ms一个测量
    let value = 10.0 + (i.to_double() * 0.5)  // 线性增长：10, 10.5, 11, 11.5, ...
    
    azimuth::Histogram::record(trend_histogram, value)
    trend_measurements.push((measure_time, value))
  }
  
  // 验证趋势分析
  assert_true(trend_measurements.length() == 100)
  
  // 验证线性趋势
  for i in 1..trend_measurements.length() {
    let (prev_time, prev_value) = trend_measurements[i-1]
    let (curr_time, curr_value) = trend_measurements[i]
    
    // 验证时间顺序
    assert_true(curr_time > prev_time)
    
    // 验证值增长趋势
    assert_true(curr_value > prev_value)
    
    // 验证增长速率（大约0.5每个测量）
    let growth_rate = curr_value - prev_value
    assert_true(growth_rate >= 0.5 && growth_rate <= 0.5)  // 精确的线性增长
  }
  
  // 创建周期性趋势
  let periodic_trend_histogram = azimuth::Meter::create_histogram(meter, "periodic.trend.metric", Some("Periodic trend metric"), Some("units"))
  let periodic_trend_measurements = []
  let periodic_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建周期性趋势（正弦波模拟）
  for i in 0..200 {
    let measure_time = periodic_base_time + (i * 5000000L)  // 每5ms一个测量
    let value = 50.0 + (20.0 * ((i * 3.14159 / 50.0).sin()))  // 正弦波，周期为50个测量
    
    azimuth::Histogram::record(periodic_trend_histogram, value)
    periodic_trend_measurements.push((measure_time, value))
  }
  
  // 验证周期性趋势
  assert_true(periodic_trend_measurements.length() == 200)
  
  // 验证周期性（4个完整周期）
  let cycles = 4
  let measurements_per_cycle = 50
  
  for cycle in 0..cycles {
    let cycle_start = cycle * measurements_per_cycle
    let cycle_end = (cycle + 1) * measurements_per_cycle
    
    // 找到周期内的最大值和最小值
    let mut max_value = 0.0
    let mut min_value = 100.0
    let max_index = 0
    let min_index = 0
    
    for i in cycle_start..cycle_end {
      let (_, value) = periodic_trend_measurements[i]
      if value > max_value {
        max_value = value
        max_index = i
      }
      if value < min_value {
        min_value = value
        min_index = i
      }
    }
    
    // 验证周期内的极值
    assert_true(max_value > 60.0)  // 大约70
    assert_true(min_value < 40.0)  // 大约30
    
    // 验证极值位置（最大值在1/4周期处，最小值在3/4周期处）
    let expected_max_pos = cycle_start + measurements_per_cycle / 4
    let expected_min_pos = cycle_start + (3 * measurements_per_cycle) / 4
    
    assert_true(max_index >= expected_max_pos - 2 && max_index <= expected_max_pos + 2)
    assert_true(min_index >= expected_min_pos - 2 && min_index <= expected_min_pos + 2)
  }
}

// 测试8: 时间序列异常检测测试
pub test "时间序列异常检测测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "anomaly-detection-meter")
  let clock = azimuth::Clock::system()
  
  // 创建异常检测测试
  let anomaly_histogram = azimuth::Meter::create_histogram(meter, "anomaly.metric", Some("Anomaly detection metric"), Some("units"))
  
  let anomaly_measurements = []
  let anomaly_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建正常时间序列（稳定在100左右）
  for i in 0..50 {
    let measure_time = anomaly_base_time + (i * 10000000L)  // 每10ms一个测量
    let value = 100.0 + ((i % 10).to_double() * 2.0)  // 在100-118之间波动
    
    azimuth::Histogram::record(anomaly_histogram, value)
    anomaly_measurements.push((measure_time, value, "normal"))
  }
  
  // 插入异常值
  let anomaly_time = anomaly_base_time + (50 * 10000000L)
  azimuth::Histogram::record(anomaly_histogram, 500.0)  // 异常高值
  anomaly_measurements.push((anomaly_time, 500.0, "anomaly_high"))
  
  let anomaly_time2 = anomaly_base_time + (51 * 10000000L)
  azimuth::Histogram::record(anomaly_histogram, 10.0)   // 异常低值
  anomaly_measurements.push((anomaly_time2, 10.0, "anomaly_low"))
  
  // 恢复正常时间序列
  for i in 52..100 {
    let measure_time = anomaly_base_time + (i * 10000000L)
    let value = 100.0 + ((i % 10).to_double() * 2.0)
    
    azimuth::Histogram::record(anomaly_histogram, value)
    anomaly_measurements.push((measure_time, value, "normal"))
  }
  
  // 验证异常检测
  assert_true(anomaly_measurements.length() == 100)
  
  // 计算正常值的统计特性
  let normal_values = []
  let anomaly_indices = []
  
  for i in 0..anomaly_measurements.length() {
    let (_, value, category) = anomaly_measurements[i]
    
    if category == "normal" {
      normal_values.push(value)
    } else {
      anomaly_indices.push(i)
    }
  }
  
  // 计算正常值的平均值和标准差
  let normal_sum = 0.0
  for value in normal_values {
    normal_sum = normal_sum + value
  }
  let normal_mean = normal_sum / normal_values.length()
  
  let variance_sum = 0.0
  for value in normal_values {
    variance_sum = variance_sum + ((value - normal_mean) * (value - normal_mean))
  }
  let normal_stddev = (variance_sum / normal_values.length()).sqrt()
  
  // 验证正常值统计特性
  assert_true(normal_mean >= 100.0 && normal_mean <= 120.0)  // 平均值应该在合理范围内
  assert_true(normal_stddev >= 0.0 && normal_stddev <= 10.0)  // 标准差应该在合理范围内
  
  // 验证异常值检测
  for index in anomaly_indices {
    let (_, anomaly_value, category) = anomaly_measurements[index]
    
    // 异常值应该距离均值超过3个标准差
    let z_score = (anomaly_value - normal_mean) / normal_stddev
    
    if category == "anomaly_high" {
      assert_true(anomaly_value > normal_mean + (3.0 * normal_stddev))
      assert_true(z_score > 3.0)
    } else if category == "anomaly_low" {
      assert_true(anomaly_value < normal_mean - (3.0 * normal_stddev))
      assert_true(z_score < -3.0)
    }
  }
  
  // 验证异常值的实际值
  assert_eq(anomaly_measurements[50].2, "anomaly_high")
  assert_eq(anomaly_measurements[50].1, 500.0)
  
  assert_eq(anomaly_measurements[51].2, "anomaly_low")
  assert_eq(anomaly_measurements[51].1, 10.0)
}

// 测试9: 时间序列预测测试
pub test "时间序列预测测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "prediction-meter")
  let clock = azimuth::Clock::system()
  
  // 创建时间序列预测测试
  let prediction_histogram = azimuth::Meter::create_histogram(meter, "prediction.metric", Some("Prediction metric"), Some("units"))
  
  let historical_measurements = []
  let prediction_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建历史数据（线性趋势）
  for i in 0..60 {
    let measure_time = prediction_base_time + (i * 10000000L)  // 每10ms一个测量
    let value = 50.0 + (i.to_double() * 1.5)  // 线性增长：50, 51.5, 53, ...
    
    azimuth::Histogram::record(prediction_histogram, value)
    historical_measurements.push((measure_time, value))
  }
  
  // 验证历史数据
  assert_true(historical_measurements.length() == 60)
  
  // 基于历史数据预测未来值（简单线性回归）
  let n = historical_measurements.length().to_double()
  let sum_x = 0.0
  let sum_y = 0.0
  let sum_xy = 0.0
  let sum_x2 = 0.0
  
  for i in 0..historical_measurements.length() {
    let x = i.to_double()
    let (_, y) = historical_measurements[i]
    
    sum_x = sum_x + x
    sum_y = sum_y + y
    sum_xy = sum_xy + (x * y)
    sum_x2 = sum_x2 + (x * x)
  }
  
  // 计算线性回归参数
  let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
  let intercept = (sum_y - slope * sum_x) / n
  
  // 验证回归参数
  assert_true(slope >= 1.4 && slope <= 1.6)  // 应该接近1.5
  assert_true(intercept >= 45.0 && intercept <= 55.0)  // 应该接近50
  
  // 预测未来10个值
  let predictions = []
  let prediction_times = []
  
  for i in 60..70 {
    let predict_time = prediction_base_time + (i * 10000000L)
    let predicted_value = slope * i.to_double() + intercept
    
    predictions.push(predicted_value)
    prediction_times.push(predict_time)
  }
  
  // 验证预测
  assert_true(predictions.length() == 10)
  assert_true(prediction_times.length() == 10)
  
  // 验证预测值的递增性
  for i in 1..predictions.length() {
    assert_true(predictions[i] > predictions[i-1])
  }
  
  // 验证预测时间的递增性
  for i in 1..prediction_times.length() {
    assert_true(prediction_times[i] > prediction_times[i-1])
  }
  
  // 验证预测值的合理性
  let last_historical_value = historical_measurements[59].1
  let first_predicted_value = predictions[0]
  
  // 第一个预测值应该接近最后一个历史值
  assert_true(first_predicted_value >= last_historical_value - 2.0)
  assert_true(first_predicted_value <= last_historical_value + 2.0)
  
  // 验证预测趋势的一致性
  let historical_growth_rate = slope
  let predicted_growth_rate = predictions[1] - predictions[0]
  
  assert_true(predicted_growth_rate >= historical_growth_rate - 0.5)
  assert_true(predicted_growth_rate <= historical_growth_rate + 0.5)
}

// 测试10: 时间序列压缩和采样测试
pub test "时间序列压缩和采样测试" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "compression-meter")
  let clock = azimuth::Clock::system()
  
  // 创建时间序列压缩和采样测试
  let compression_histogram = azimuth::Meter::create_histogram(meter, "compression.metric", Some("Compression metric"), Some("units"))
  
  let raw_measurements = []
  let compression_base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建高频率原始数据（每1ms一个测量）
  for i in 0..1000 {
    let measure_time = compression_base_time + (i * 1000000L)  // 每1ms一个测量
    let value = 100.0 + (10.0 * ((i * 3.14159 / 100).sin())) + (i.to_double() * 0.01)  // 正弦波 + 线性趋势
    
    azimuth::Histogram::record(compression_histogram, value)
    raw_measurements.push((measure_time, value))
  }
  
  // 验证原始数据
  assert_true(raw_measurements.length() == 1000)
  
  // 时间下采样（每10ms取一个点）
  let downsampled_measurements = []
  let downsample_rate = 10
  
  for i in 0..(raw_measurements.length() / downsample_rate) {
    let original_index = i * downsample_rate
    downsampled_measurements.push(raw_measurements[original_index])
  }
  
  // 验证下采样
  assert_true(downsampled_measurements.length() == 100)
  
  // 验证下采样后的时间间隔
  for i in 1..downsampled_measurements.length() {
    let (prev_time, _) = downsampled_measurements[i-1]
    let (curr_time, _) = downsampled_measurements[i]
    
    let expected_interval = downsample_rate * 1000000L  // 10ms
    let actual_interval = curr_time - prev_time
    
    assert_true(actual_interval == expected_interval)
  }
  
  // 时间窗口聚合（每100ms计算平均值）
  let aggregated_measurements = []
  let window_size = 100  // 100个原始测量点 = 100ms
  
  for i in 0..(raw_measurements.length() / window_size) {
    let window_start = i * window_size
    let window_end = (i + 1) * window_size
    
    let mut window_sum = 0.0
    let window_count = 0
    
    for j in window_start..window_end {
      let (_, value) = raw_measurements[j]
      window_sum = window_sum + value
      window_count = window_count + 1
    }
    
    let window_avg = window_sum / window_count.to_double()
    let window_start_time = raw_measurements[window_start].0
    
    aggregated_measurements.push((window_start_time, window_avg))
  }
  
  // 验证聚合
  assert_true(aggregated_measurements.length() == 10)
  
  // 验证聚合后的时间间隔
  for i in 1..aggregated_measurements.length() {
    let (prev_time, _) = aggregated_measurements[i-1]
    let (curr_time, _) = aggregated_measurements[i]
    
    let expected_interval = window_size * 1000000L  // 100ms
    let actual_interval = curr_time - prev_time
    
    assert_true(actual_interval == expected_interval)
  }
  
  // 验证聚合值的准确性
  for i in 0..aggregated_measurements.length() {
    let (_, avg_value) = aggregated_measurements[i]
    
    // 聚合值应该在原始数据范围内
    let mut min_value = raw_measurements[0].1
    let mut max_value = raw_measurements[0].1
    
    for j in 0..raw_measurements.length() {
      let (_, value) = raw_measurements[j]
      if value < min_value {
        min_value = value
      }
      if value > max_value {
        max_value = value
      }
    }
    
    assert_true(avg_value >= min_value)
    assert_true(avg_value <= max_value)
  }
  
  // 验证压缩效果
  let compression_ratio = raw_measurements.length().to_double() / aggregated_measurements.length().to_double()
  assert_true(compression_ratio == 100.0)  // 1000:10 = 100:1的压缩比
}