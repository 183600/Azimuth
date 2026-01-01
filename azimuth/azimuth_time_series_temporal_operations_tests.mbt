// Azimuth Time Series and Temporal Operations Tests
// This file contains tests for time series and temporal operations in the Azimuth telemetry system

// Test 1: Clock precision and monotonicity
pub test "时间序列和时序操作测试：时钟精度和单调性" {
  // 测试时钟精度
  let clock = azimuth::Clock::system()
  let timestamp1 = azimuth::Clock::now_unix_nanos(clock)
  
  // 验证时间戳格式（纳秒级精度）
  assert_true(timestamp1 > 0L)
  assert_true(timestamp1.toString().length() >= 16)  // 至少16位数字
  
  // 测试时间戳单调递增
  let timestamp2 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp3 = azimuth::Clock::now_unix_nanos(clock)
  
  assert_true(timestamp2 >= timestamp1)
  assert_true(timestamp3 >= timestamp2)
  
  // 测试时间差计算
  let time_diff1 = timestamp2 - timestamp1
  let time_diff2 = timestamp3 - timestamp2
  
  assert_true(time_diff1 >= 0L)
  assert_true(time_diff2 >= 0L)
  
  // 测试时间戳的合理范围
  let current_year = 2025
  let min_timestamp = 1672531200000000000L  // 2023-01-01 00:00:00 UTC
  let max_timestamp = 1735689599999999999L  // 2025-01-01 23:59:59 UTC
  
  assert_true(timestamp1 >= min_timestamp)
  assert_true(timestamp1 <= max_timestamp)
  
  // 测试高频时间戳采集
  let timestamps = []
  for i in 0..100 {
    timestamps.push(azimuth::Clock::now_unix_nanos(clock))
  }
  
  // 验证时间戳序列的单调性
  for i in 1..timestamps.length() {
    assert_true(timestamps[i] >= timestamps[i-1])
  }
  
  // 计算平均时间间隔
  let total_diff = timestamps[timestamps.length()-1] - timestamps[0]
  let avg_interval = total_diff.to_double() / (timestamps.length() - 1).to_double()
  
  // 验证平均间隔在合理范围内（应该很小）
  assert_true(avg_interval >= 0.0)
  assert_true(avg_interval < 1000000000.0)  // 小于1秒
}

// Test 2: Log record time series
pub test "时间序列和时序操作测试：日志记录时间序列" {
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "time-series-test")
  let clock = azimuth::Clock::system()
  
  let log_timestamps = []
  let base_time = azimuth::Clock::now_unix_nanos(clock)
  
  // 创建时间序列日志
  for i in 0..50 {
    let log_time = base_time + (i * 1000000L)  // 每个日志间隔1毫秒
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Time series log " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(log_time),
      Some(log_time + 1000L),  // observed_time稍晚
      Some("time-series-trace"),
      Some("time-series-span"),
      Some(azimuth::Context::root())
    )
    log_timestamps.push(log_time)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证时间序列的顺序性
  for i in 1..log_timestamps.length() {
    assert_true(log_timestamps[i] > log_timestamps[i-1])
  }
  
  // 验证时间间隔的一致性
  for i in 1..log_timestamps.length() {
    let interval = log_timestamps[i] - log_timestamps[i-1]
    assert_eq(interval, 1000000L)  // 应该是1毫秒
  }
  
  // 测试不同严重级别的时间序列
  let severity_timestamps = []
  let severity_levels = [azimuth::Trace, azimuth::Debug, azimuth::Info, azimuth::Warn, azimuth::Error, azimuth::Fatal]
  
  for i in 0..severity_levels.length() {
    let severity_time = base_time + (i * 2000000L)  // 每个日志间隔2毫秒
    let log_record = azimuth::LogRecord::new_with_context(
      severity_levels[i],
      Some("Severity " + i.to_string() + " log"),
      Some(azimuth::Attributes::new()),
      Some(severity_time),
      Some(severity_time + 500L),
      Some("severity-trace"),
      Some("severity-span"),
      Some(azimuth::Context::root())
    )
    severity_timestamps.push(severity_time)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证严重级别时间序列
  for i in 1..severity_timestamps.length() {
    assert_true(severity_timestamps[i] > severity_timestamps[i-1])
    let interval = severity_timestamps[i] - severity_timestamps[i-1]
    assert_eq(interval, 2000000L)  // 应该是2毫秒
  }
}

// Test 3: Metrics time series
pub test "时间序列和时序操作测试：度量时间序列" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "time-series-meter")
  let clock = azimuth::Clock::system()
  
  let histogram = azimuth::Meter::create_histogram(meter, "response.time", Some("Response time histogram"), Some("ms"))
  let counter = azimuth::Meter::create_counter(meter, "request.count", Some("Request counter"), Some("count"))
  
  let measurement_times = []
  let measurement_values = []
  let counter_times = []
  
  // 创建时间序列度量
  for i in 0..30 {
    let measure_time = azimuth::Clock::now_unix_nanos(clock)
    let value = 100.0 + (i.to_double() * 10.0)  // 递增的响应时间
    
    azimuth::Histogram::record(histogram, value)
    
    measurement_times.push(measure_time)
    measurement_values.push(value)
    
    // 每5次测量记录一次计数器
    if i % 5 == 0 {
      azimuth::Counter::add(counter, 1.0)
      counter_times.push(measure_time)
    }
  }
  
  // 验证度量时间序列
  assert_true(measurement_times.length() == 30)
  assert_true(measurement_values.length() == 30)
  assert_true(counter_times.length() == 6)  // 0, 5, 10, 15, 20, 25
  
  // 验证时间序列的单调性
  for i in 1..measurement_times.length() {
    assert_true(measurement_times[i] >= measurement_times[i-1])
  }
  
  // 验证值的递增性
  for i in 1..measurement_values.length() {
    assert_true(measurement_values[i] > measurement_values[i-1])
  }
  
  // 测试周期性度量
  let periodic_histogram = azimuth::Meter::create_histogram(meter, "periodic.metric", Some("Periodic metric"), Some("units"))
  let periodic_times = []
  let periodic_values = []
  
  for i in 0..20 {
    let periodic_time = azimuth::Clock::now_unix_nanos(clock)
    let periodic_value = 50.0 + (30.0 * (i.to_double() / 4.0).sin())  // 周期性值
    
    azimuth::Histogram::record(periodic_histogram, periodic_value)
    
    periodic_times.push(periodic_time)
    periodic_values.push(periodic_value)
  }
  
  // 验证周期性度量的时间序列
  assert_true(periodic_times.length() == 20)
  assert_true(periodic_values.length() == 20)
  
  // 验证时间序列的单调性
  for i in 1..periodic_times.length() {
    assert_true(periodic_times[i] >= periodic_times[i-1])
  }
}

// Test 4: Span lifecycle time series
pub test "时间序列和时序操作测试：Span生命周期时间序列" {
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "time-series-tracer")
  let clock = azimuth::Clock::system()
  
  let span_start_times = []
  let span_durations = []
  let span_names = []
  
  // 创建时间序列Span
  for i in 0..20 {
    let start_time = azimuth::Clock::now_unix_nanos(clock)
    let span = azimuth::Tracer::start_span(tracer, "time-series-span-" + i.to_string())
    
    // 模拟Span操作
    azimuth::Span::add_event(span, "operation-" + i.to_string(), None)
    
    // 模拟不同的处理时间
    let processing_time = (i + 1) * 100000L  // 递增的处理时间
    let end_time = start_time + processing_time
    
    azimuth::Span::end(span)
    
    span_start_times.push(start_time)
    span_durations.push(processing_time)
    span_names.push("time-series-span-" + i.to_string())
  }
  
  // 验证Span时间序列
  assert_true(span_start_times.length() == 20)
  assert_true(span_durations.length() == 20)
  assert_true(span_names.length() == 20)
  
  // 验证开始时间的单调性
  for i in 1..span_start_times.length() {
    assert_true(span_start_times[i] >= span_start_times[i-1])
  }
  
  // 验证持续时间的递增性
  for i in 1..span_durations.length() {
    assert_true(span_durations[i] > span_durations[i-1])
  }
  
  // 测试嵌套Span的时间序列
  let root_span = azimuth::Tracer::start_span(tracer, "root-span")
  let root_start_time = azimuth::Clock::now_unix_nanos(clock)
  
  let child_spans = []
  let child_start_times = []
  
  for i in 0..5 {
    let child_start = azimuth::Clock::now_unix_nanos(clock)
    let child_span = azimuth::Tracer::start_span(tracer, "child-span-" + i.to_string())
    
    azimuth::Span::add_event(child_span, "child-operation-" + i.to_string(), None)
    
    let child_end = child_start + 50000L  // 固定的子Span持续时间
    azimuth::Span::end(child_span)
    
    child_spans.push(child_span)
    child_start_times.push(child_start)
  }
  
  let root_end_time = azimuth::Clock::now_unix_nanos(clock)
  azimuth::Span::end(root_span)
  
  // 验证嵌套Span的时间关系
  assert_true(child_start_times.length() == 5)
  
  // 验证子Span在根Span的生命周期内
  for child_start in child_start_times {
    assert_true(child_start >= root_start_time)
    assert_true(child_start <= root_end_time)
  }
  
  // 验证子Span开始时间的单调性
  for i in 1..child_start_times.length() {
    assert_true(child_start_times[i] >= child_start_times[i-1])
  }
}

// Test 5: Temporal aggregation operations
pub test "时间序列和时序操作测试：时间聚合操作" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "aggregation-meter")
  let clock = azimuth::Clock::system()
  
  let histogram = azimuth::Meter::create_histogram(meter, "aggregation.test", Some("Aggregation test"), Some("units"))
  
  let measurement_times = []
  let measurement_values = []
  
  // 创建用于聚合的时间序列数据
  for i in 0..100 {
    let measure_time = azimuth::Clock::now_unix_nanos(clock)
    let value = 100.0 + (50.0 * (i.to_double() / 10.0).sin()) + (i.to_double() * 0.5)  // 带噪声的趋势数据
    
    azimuth::Histogram::record(histogram, value)
    
    measurement_times.push(measure_time)
    measurement_values.push(value)
  }
  
  // 验证数据生成
  assert_true(measurement_times.length() == 100)
  assert_true(measurement_values.length() == 100)
  
  // 计算时间窗口聚合
  let window_size = 10  // 每10个数据点一个窗口
  let num_windows = measurement_values.length() / window_size
  
  for window in 0..num_windows {
    let start_idx = window * window_size
    let end_idx = start_idx + window_size
    
    // 计算窗口内的统计值
    let window_values = measurement_values.slice(start_idx, end_idx)
    let sum = 0.0
    let min = window_values[0]
    let max = window_values[0]
    
    for value in window_values {
      sum = sum + value
      if value < min {
        min = value
      }
      if value > max {
        max = value
      }
    }
    
    let avg = sum / window_values.length().to_double()
    
    // 验证聚合结果
    assert_true(avg >= min)
    assert_true(avg <= max)
    assert_true(min <= max)
  }
  
  // 测试时间分桶聚合
  let bucket_duration = 1000000000L  // 1秒的桶
  let first_time = measurement_times[0]
  let last_time = measurement_times[measurement_times.length() - 1]
  
  let num_buckets = ((last_time - first_time) / bucket_duration).to_int() + 1
  let bucket_counts = [0] * num_buckets
  let bucket_sums = [0.0] * num_buckets
  
  // 将数据分配到时间桶
  for i in 0..measurement_times.length() {
    let bucket_idx = ((measurement_times[i] - first_time) / bucket_duration).to_int()
    bucket_counts[bucket_idx] = bucket_counts[bucket_idx] + 1
    bucket_sums[bucket_idx] = bucket_sums[bucket_idx] + measurement_values[i]
  }
  
  // 验证时间分桶结果
  let total_count = 0
  let total_sum = 0.0
  
  for i in 0..bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    total_sum = total_sum + bucket_sums[i]
  }
  
  assert_eq(total_count, measurement_values.length())
  
  // 验证总和
  let original_sum = 0.0
  for value in measurement_values {
    original_sum = original_sum + value
  }
  
  assert_true((total_sum - original_sum).abs() < 0.001)
}

// Test 6: Temporal correlation analysis
pub test "时间序列和时序操作测试：时间相关性分析" {
  let meter_a = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "correlation-meter-a")
  let meter_b = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "correlation-meter-b")
  
  let histogram_a = azimuth::Meter::create_histogram(meter_a, "metric.a", Some("Metric A"), Some("units"))
  let histogram_b = azimuth::Meter::create_histogram(meter_b, "metric.b", Some("Metric B"), Some("units"))
  
  let times = []
  let values_a = []
  let values_b = []
  
  // 创建相关的时间序列数据
  for i in 0..50 {
    let time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 创建相关的数据：B = A * 2 + 噪声
    let value_a = 100.0 + (i.to_double() * 2.0)
    let value_b = value_a * 2.0 + (10.0 * (i.to_double() * 0.1).sin())
    
    azimuth::Histogram::record(histogram_a, value_a)
    azimuth::Histogram::record(histogram_b, value_b)
    
    times.push(time)
    values_a.push(value_a)
    values_b.push(value_b)
  }
  
  // 验证数据生成
  assert_true(times.length() == 50)
  assert_true(values_a.length() == 50)
  assert_true(values_b.length() == 50)
  
  // 计算简单的相关性指标
  let sum_a = 0.0
  let sum_b = 0.0
  let sum_ab = 0.0
  let sum_a2 = 0.0
  let sum_b2 = 0.0
  
  for i in 0..values_a.length() {
    sum_a = sum_a + values_a[i]
    sum_b = sum_b + values_b[i]
    sum_ab = sum_ab + (values_a[i] * values_b[i])
    sum_a2 = sum_a2 + (values_a[i] * values_a[i])
    sum_b2 = sum_b2 + (values_b[i] * values_b[i])
  }
  
  let n = values_a.length().to_double()
  let mean_a = sum_a / n
  let mean_b = sum_b / n
  
  // 计算相关系数
  let numerator = sum_ab - (n * mean_a * mean_b)
  let denominator = ((sum_a2 - (n * mean_a * mean_a)) * (sum_b2 - (n * mean_b * mean_b))).sqrt()
  
  // 避免除零
  let correlation = if denominator > 0.0 { numerator / denominator } else { 0.0 }
  
  // 验证相关性（应该接近1，因为数据是强相关的）
  assert_true(correlation > 0.8)
  
  // 测试时间滞后相关性
  let lag_correlations = []
  
  for lag in 0..5 {
    let lag_sum = 0.0
    let lag_count = 0
    
    for i in lag..values_a.length() {
      lag_sum = lag_sum + (values_a[i - lag] * values_b[i])
      lag_count = lag_count + 1
    }
    
    let lag_correlation = if lag_count > 0 { lag_sum / lag_count.to_double() } else { 0.0 }
    lag_correlations.push(lag_correlation)
  }
  
  // 验证滞后相关性
  assert_true(lag_correlations.length() == 6)
  
  // 通常lag=0的相关性应该最高，因为数据是同步的
  assert_true(lag_correlations[0] >= lag_correlations[1])
}

// Test 7: Temporal pattern detection
pub test "时间序列和时序操作测试：时间模式检测" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "pattern-meter")
  let histogram = azimuth::Meter::create_histogram(meter, "pattern.test", Some("Pattern test"), Some("units"))
  
  let times = []
  let values = []
  
  // 创建带有模式的时间序列数据
  for i in 0..60 {
    let time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 创建周期性模式：每日模式（24小时周期）
    let hour_of_day = (i % 24).to_double()
    let daily_pattern = 50.0 + (30.0 * ((hour_of_day - 6.0) * 3.14159 / 12.0).cos())
    
    // 创建每周模式（7天周期）
    let day_of_week = (i / 24).to_double() % 7.0
    let weekly_pattern = 10.0 * ((day_of_week - 3.5) * 3.14159 / 3.5).sin()
    
    // 创建趋势
    let trend = i.to_double() * 0.1
    
    // 组合所有模式
    let value = daily_pattern + weekly_pattern + trend
    
    azimuth::Histogram::record(histogram, value)
    
    times.push(time)
    values.push(value)
  }
  
  // 验证数据生成
  assert_true(times.length() == 60)
  assert_true(values.length() == 60)
  
  // 检测每日模式
  let hourly_averages = [0.0] * 24
  let hourly_counts = [0] * 24
  
  for i in 0..values.length() {
    let hour = i % 24
    hourly_averages[hour] = hourly_averages[hour] + values[i]
    hourly_counts[hour] = hourly_counts[hour] + 1
  }
  
  // 计算每小时平均值
  for hour in 0..24 {
    if hourly_counts[hour] > 0 {
      hourly_averages[hour] = hourly_averages[hour] / hourly_counts[hour].to_double()
    }
  }
  
  // 验证每日模式（应该有明显的峰值和谷值）
  let max_hourly = hourly_averages[0]
  let min_hourly = hourly_averages[0]
  
  for avg in hourly_averages {
    if avg > max_hourly {
      max_hourly = avg
    }
    if avg < min_hourly {
      min_hourly = avg
    }
  }
  
  // 验证有明显的日变化
  assert_true(max_hourly - min_hourly > 20.0)
  
  // 检测趋势
  let first_day_avg = 0.0
  let last_day_avg = 0.0
  let first_day_count = 0
  let last_day_count = 0
  
  for i in 0..values.length() {
    if i < 24 {  // 第一天
      first_day_avg = first_day_avg + values[i]
      first_day_count = first_day_count + 1
    }
    if i >= 36 {  // 第三天
      last_day_avg = last_day_avg + values[i]
      last_day_count = last_day_count + 1
    }
  }
  
  first_day_avg = first_day_avg / first_day_count.to_double()
  last_day_avg = last_day_avg / last_day_count.to_double()
  
  // 验证上升趋势
  assert_true(last_day_avg > first_day_avg)
}

// Test 8: Temporal anomaly detection
pub test "时间序列和时序操作测试：时间异常检测" {
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "anomaly-meter")
  let histogram = azimuth::Meter::create_histogram(meter, "anomaly.test", Some("Anomaly test"), Some("units"))
  
  let times = []
  let values = []
  
  // 创建带有异常的时间序列数据
  for i in 0..40 {
    let time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 基础模式：正弦波
    let base_value = 100.0 + (20.0 * (i.to_double() * 0.2).sin())
    
    // 添加一些噪声
    let noise = 5.0 * ((i.to_double() * 0.7).sin())
    
    let value = base_value + noise
    
    // 在特定位置添加异常
    if i == 10 {
      value = value + 50.0  // 尖峰异常
    }
    if i == 25 {
      value = value - 40.0  // 谷值异常
    }
    
    azimuth::Histogram::record(histogram, value)
    
    times.push(time)
    values.push(value)
  }
  
  // 验证数据生成
  assert_true(times.length() == 40)
  assert_true(values.length() == 40)
  
  // 计算统计量用于异常检测
  let sum = 0.0
  let sum_squares = 0.0
  
  for value in values {
    sum = sum + value
    sum_squares = sum_squares + (value * value)
  }
  
  let n = values.length().to_double()
  let mean = sum / n
  let variance = (sum_squares / n) - (mean * mean)
  let std_dev = variance.sqrt()
  
  // 验证统计量
  assert_true(std_dev > 0.0)
  
  // 检测异常（3-sigma规则）
  let anomaly_indices = []
  
  for i in 0..values.length() {
    let z_score = (values[i] - mean) / std_dev
    if z_score.abs() > 3.0 {
      anomaly_indices.push(i)
    }
  }
  
  // 验证异常检测
  assert_true(anomaly_indices.length() >= 2)  // 至少检测到2个异常
  
  // 验证检测到的异常包含我们插入的异常
  let has_spike = false
  let has_drop = false
  
  for idx in anomaly_indices {
    if idx == 10 {
      has_spike = true
    }
    if idx == 25 {
      has_drop = true
    }
  }
  
  assert_true(has_spike || has_drop)  // 至少检测到一个插入的异常
  
  // 测试滑动窗口异常检测
  let window_size = 10
  let window_anomalies = []
  
  for i in window_size..values.length() {
    // 计算窗口内的统计量
    let window_sum = 0.0
    let window_sum_squares = 0.0
    
    for j in (i - window_size)..i {
      window_sum = window_sum + values[j]
      window_sum_squares = window_sum_squares + (values[j] * values[j])
    }
    
    let window_n = window_size.to_double()
    let window_mean = window_sum / window_n
    let window_variance = (window_sum_squares / window_n) - (window_mean * window_mean)
    let window_std_dev = window_variance.sqrt()
    
    // 检查窗口外的点是否异常
    if i < values.length() {
      let z_score = (values[i] - window_mean) / window_std_dev
      if z_score.abs() > 2.5 {  // 使用更宽松的阈值
        window_anomalies.push(i)
      }
    }
  }
  
  // 验证滑动窗口异常检测
  assert_true(window_anomalies.length() >= 1)  // 至少检测到一个异常
}