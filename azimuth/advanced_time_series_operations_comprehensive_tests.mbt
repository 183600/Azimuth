// 时序操作高级测试
// 测试时间序列数据处理、时序分析和时间相关的遥测操作

test "时序数据基础操作测试" {
  // 创建时钟实例
  let clock = Clock::system()
  
  // 获取当前时间戳
  let base_timestamp = Clock::now_unix_nanos(clock)
  assert_true(base_timestamp > 0L, "时间戳应该大于0")
  
  // 创建时序数据点
  let timestamp1 = base_timestamp
  let timestamp2 = base_timestamp + 1000000L // 1ms后
  let timestamp3 = base_timestamp + 2000000L // 2ms后
  let timestamp4 = base_timestamp + 3000000L // 3ms后
  let timestamp5 = base_timestamp + 4000000L // 4ms后
  
  // 验证时间戳的递增性
  assert_true(timestamp2 > timestamp1, "时间戳应该递增")
  assert_true(timestamp3 > timestamp2, "时间戳应该递增")
  assert_true(timestamp4 > timestamp3, "时间戳应该递增")
  assert_true(timestamp5 > timestamp4, "时间戳应该递增")
  
  // 计算时间间隔
  let interval1 = timestamp2 - timestamp1
  let interval2 = timestamp3 - timestamp2
  let interval3 = timestamp4 - timestamp3
  let interval4 = timestamp5 - timestamp4
  
  assert_eq(interval1, 1000000L, "第一个时间间隔应该是1ms")
  assert_eq(interval2, 1000000L, "第二个时间间隔应该是1ms")
  assert_eq(interval3, 1000000L, "第三个时间间隔应该是1ms")
  assert_eq(interval4, 1000000L, "第四个时间间隔应该是1ms")
  
  // 创建带时间戳的日志记录
  let log1 = LogRecord::new_with_context(
    Info, Some("Time series log 1"), None,
    Some(timestamp1), Some(timestamp1),
    None, None, None
  )
  
  let log2 = LogRecord::new_with_context(
    Warn, Some("Time series log 2"), None,
    Some(timestamp2), Some(timestamp2),
    None, None, None
  )
  
  let log3 = LogRecord::new_with_context(
    Error, Some("Time series log 3"), None,
    Some(timestamp3), Some(timestamp3),
    None, None, None
  )
  
  // 验证时序日志的时间顺序
  match (LogRecord::timestamp(log1), LogRecord::timestamp(log2), LogRecord::timestamp(log3)) {
    (Some(ts1), Some(ts2), Some(ts3)) => {
      assert_true(ts1 < ts2, "日志1时间应该早于日志2")
      assert_true(ts2 < ts3, "日志2时间应该早于日志3")
      assert_eq(ts1, timestamp1, "日志1时间戳应该匹配")
      assert_eq(ts2, timestamp2, "日志2时间戳应该匹配")
      assert_eq(ts3, timestamp3, "日志3时间戳应该匹配")
    }
    _ => assert_true(false, "期望所有日志都有时间戳")
  }
}

test "时序度量聚合测试" {
  // 创建度量提供者和仪表
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time-series-test")
  
  // 创建时序计数器
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  
  // 创建时序直方图
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  let size_histogram = Meter::create_histogram(meter, "http.response.size")
  
  // 模拟时序数据记录
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 第一轮度量记录
  Counter::add(request_counter, 100.0)
  Counter::add(error_counter, 5.0)
  Histogram::record(response_histogram, 150.5)
  Histogram::record(size_histogram, 1024.0)
  
  // 等待一段时间（模拟）
  let second_time = base_time + 1000000000L // 1秒后
  
  // 第二轮度量记录
  Counter::add(request_counter, 150.0)
  Counter::add(error_counter, 8.0)
  Histogram::record(response_histogram, 200.75)
  Histogram::record(size_histogram, 2048.0)
  
  // 第三轮度量记录
  let third_time = base_time + 2000000000L // 2秒后
  Counter::add(request_counter, 120.0)
  Counter::add(error_counter, 3.0)
  Histogram::record(response_histogram, 125.25)
  Histogram::record(size_histogram, 512.0)
  
  // 验证度量属性
  assert_eq(request_counter.name, "http.requests.total", "请求计数器名称应该正确")
  assert_eq(error_counter.name, "http.errors.total", "错误计数器名称应该正确")
  assert_eq(response_histogram.name, "http.response.duration", "响应时间直方图名称应该正确")
  assert_eq(size_histogram.name, "http.response.size", "响应大小直方图名称应该正确")
  
  // 创建时序上下文
  let time_series_context = Context::root()
  let time_key = ContextKey::new("time.series")
  let enriched_context = Context::with_value(time_series_context, time_key, "aggregated.data")
  
  // 验证时序上下文
  let extracted_time_series = Context::get(enriched_context, time_key)
  assert_eq(extracted_time_series, Some("aggregated.data"), "时序上下文应该正确")
}

test "时序窗口操作测试" {
  // 创建时间窗口数据
  let clock = Clock::system()
  let start_time = Clock::now_unix_nanos(clock)
  
  // 定义窗口大小（1秒）
  let window_size = 1000000000L // 1秒 = 1,000,000,000纳秒
  
  // 创建窗口内的数据点
  let window_start = start_time
  let window_end = start_time + window_size
  
  // 窗口内的数据点
  let point1_time = window_start + 100000000L  // 0.1秒
  let point2_time = window_start + 300000000L  // 0.3秒
  let point3_time = window_start + 500000000L  // 0.5秒
  let point4_time = window_start + 700000000L  // 0.7秒
  let point5_time = window_start + 900000000L  // 0.9秒
  
  // 验证数据点在窗口内
  assert_true(point1_time >= window_start && point1_time <= window_end, "点1应该在窗口内")
  assert_true(point2_time >= window_start && point2_time <= window_end, "点2应该在窗口内")
  assert_true(point3_time >= window_start && point3_time <= window_end, "点3应该在窗口内")
  assert_true(point4_time >= window_start && point4_time <= window_end, "点4应该在窗口内")
  assert_true(point5_time >= window_start && point5_time <= window_end, "点5应该在窗口内")
  
  // 窗口外的数据点
  let outside_point1_time = window_start - 100000000L  // 窗口前0.1秒
  let outside_point2_time = window_end + 100000000L   // 窗口后0.1秒
  
  // 验证数据点在窗口外
  assert_true(outside_point1_time < window_start, "外部点1应该在窗口前")
  assert_true(outside_point2_time > window_end, "外部点2应该在窗口后")
  
  // 创建窗口内的日志记录
  let window_logs = [
    LogRecord::new_with_context(
      Info, Some("Window point 1"), None,
      Some(point1_time), Some(point1_time),
      None, None, None
    ),
    LogRecord::new_with_context(
      Info, Some("Window point 2"), None,
      Some(point2_time), Some(point2_time),
      None, None, None
    ),
    LogRecord::new_with_context(
      Info, Some("Window point 3"), None,
      Some(point3_time), Some(point3_time),
      None, None, None
    ),
    LogRecord::new_with_context(
      Info, Some("Window point 4"), None,
      Some(point4_time), Some(point4_time),
      None, None, None
    ),
    LogRecord::new_with_context(
      Info, Some("Window point 5"), None,
      Some(point5_time), Some(point5_time),
      None, None, None
    )
  ]
  
  // 验证窗口内日志的时间顺序
  for i = 0; i < window_logs.length() - 1; i = i + 1 {
    let current_log = window_logs[i]
    let next_log = window_logs[i + 1]
    
    match (LogRecord::timestamp(current_log), LogRecord::timestamp(next_log)) {
      (Some(current_ts), Some(next_ts)) => {
        assert_true(current_ts < next_ts, "日志" + i.to_string() + "应该早于日志" + (i + 1).to_string())
      }
      _ => assert_true(false, "期望所有日志都有时间戳")
    }
  }
  
  // 计算窗口统计信息
  let window_duration = window_end - window_start
  let point_interval = (point5_time - point1_time) / 4 // 4个间隔
  
  assert_eq(window_duration, window_size, "窗口持续时间应该等于窗口大小")
  assert_eq(point_interval, 200000000L, "数据点间隔应该是0.2秒")
}

test "时序趋势分析测试" {
  // 创建趋势数据
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 模拟上升趋势的数据点
  let trend_data = [
    (base_time + 0L, 100.0),      // 基准值
    (base_time + 1000000000L, 120.0),  // +20%
    (base_time + 2000000000L, 145.0),  // +20.8%
    (base_time + 3000000000L, 175.0),  // +20.7%
    (base_time + 4000000000L, 210.0),  // +20%
    (base_time + 5000000000L, 250.0)   // +19%
  ]
  
  // 验证趋势数据的时间顺序
  for i = 0; i < trend_data.length() - 1; i = i + 1 {
    let current_point = trend_data[i]
    let next_point = trend_data[i + 1]
    
    assert_true(current_point.0 < next_point.0, "趋势数据点" + i.to_string() + "时间应该早于点" + (i + 1).to_string())
    assert_true(current_point.1 < next_point.1, "趋势数据点" + i.to_string() + "值应该小于点" + (i + 1).to_string())
  }
  
  // 计算趋势变化率
  let initial_value = trend_data[0].1
  let final_value = trend_data[trend_data.length() - 1].1
  let total_change = final_value - initial_value
  let change_rate = total_change / initial_value
  
  assert_eq(initial_value, 100.0, "初始值应该正确")
  assert_eq(final_value, 250.0, "最终值应该正确")
  assert_eq(total_change, 150.0, "总变化量应该正确")
  assert_true(change_rate > 1.49 && change_rate < 1.51, "变化率应该约为150%")
  
  // 创建带趋势的度量数据
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "trend-analysis")
  let trend_counter = Meter::create_counter(meter, "trend.metric")
  let trend_histogram = Meter::create_histogram(meter, "trend.distribution")
  
  // 记录趋势数据
  for i = 0; i < trend_data.length(); i = i + 1 {
    let point = trend_data[i]
    Counter::add(trend_counter, point.1)
    Histogram::record(trend_histogram, point.1)
  }
  
  // 验证趋势度量属性
  assert_eq(trend_counter.name, "trend.metric", "趋势计数器名称应该正确")
  assert_eq(trend_histogram.name, "trend.distribution", "趋势直方图名称应该正确")
}

test "时序异常检测测试" {
  // 创建包含异常的时序数据
  let clock = Clock::system()
  let base_time = Clock::now_unix_nanos(clock)
  
  // 正常数据点（稳定在100左右）
  let normal_data = [
    (base_time + 0L, 98.0),
    (base_time + 1000000000L, 102.0),
    (base_time + 2000000000L, 99.0),
    (base_time + 3000000000L, 101.0),
    (base_time + 4000000000L, 100.0)
  ]
  
  // 异常数据点
  let anomaly_data = [
    (base_time + 5000000000L, 500.0),  // 异常高值
    (base_time + 6000000000L, 10.0),   // 异常低值
    (base_time + 7000000000L, 150.0),  // 边界异常
    (base_time + 8000000000L, 50.0)    // 边界异常
  ]
  
  // 恢复正常数据点
  let recovery_data = [
    (base_time + 9000000000L, 99.0),
    (base_time + 10000000000L, 101.0)
  ]
  
  // 计算正常数据的统计特征
  let mut normal_sum = 0.0
  let mut normal_count = 0
  
  for i = 0; i < normal_data.length(); i = i + 1 {
    normal_sum = normal_sum + normal_data[i].1
    normal_count = normal_count + 1
  }
  
  let normal_mean = normal_sum / normal_count.to_double()
  
  // 验证正常数据统计
  assert_eq(normal_count, 5, "正常数据点数量应该正确")
  assert_true(normal_mean > 99.0 && normal_mean < 101.0, "正常数据均值应该在100左右")
  
  // 检测异常值（简单阈值法）
  let upper_threshold = normal_mean + 50.0  // 上阈值
  let lower_threshold = normal_mean - 50.0  // 下阈值
  
  let mut detected_anomalies = 0
  
  // 检查异常数据
  for i = 0; i < anomaly_data.length(); i = i + 1 {
    let anomaly_value = anomaly_data[i].1
    if anomaly_value > upper_threshold || anomaly_value < lower_threshold {
      detected_anomalies = detected_anomalies + 1
    }
  }
  
  // 验证异常检测
  assert_eq(detected_anomalies, 4, "应该检测到所有4个异常值")
  
  // 创建异常检测相关的日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "anomaly-detector")
  
  // 记录异常检测日志
  let anomaly_log = LogRecord::new_with_context(
    Warn, 
    Some("Detected " + detected_anomalies.to_string() + " anomalies in time series data"),
    None,
    Some(base_time + 8500000000L),
    Some(base_time + 8500000000L),
    None, None, None
  )
  
  Logger::emit(logger, anomaly_log)
  
  // 验证异常检测日志
  assert_eq(LogRecord::severity_number(anomaly_log), Warn, "异常检测日志应该是警告级别")
  match LogRecord::body(anomaly_log) {
    Some(body) => assert_true(body.contains("4 anomalies"), "日志应该包含异常数量信息")
    _ => assert_true(false, "期望日志有内容")
  }
}

test "时序性能和精度测试" {
  // 测试高精度时间戳
  let clock = Clock::system()
  let high_precision_time = Clock::now_unix_nanos(clock)
  
  // 验证时间戳精度（纳秒级）
  assert_true(high_precision_time > 0L, "高精度时间戳应该大于0")
  assert_true(high_precision_time % 1000L != 0L || high_precision_time > 1000000000000L, "时间戳应该具有纳秒精度")
  
  // 测试连续时间获取的性能
  let mut time_samples = []
  let sample_count = 10
  
  // 快速连续获取时间戳
  for i = 0; i < sample_count; i = i + 1 {
    let sample_time = Clock::now_unix_nanos(clock)
    time_samples.push(sample_time)
  }
  
  // 验证时间戳的单调性
  for i = 0; i < time_samples.length() - 1; i = i + 1 {
    let current = time_samples[i]
    let next = time_samples[i + 1]
    assert_true(current <= next, "时间戳应该单调递增")
  }
  
  // 计算时间采样间隔
  if time_samples.length() >= 2 {
    let first_sample = time_samples[0]
    let last_sample = time_samples[time_samples.length() - 1]
    let total_duration = last_sample - first_sample
    let average_interval = total_duration / (time_samples.length() - 1).to_int64()
    
    assert_true(total_duration > 0L, "总采样时间应该大于0")
    assert_true(average_interval >= 0L, "平均采样间隔应该非负")
  }
  
  // 测试时序数据的内存效率
  let large_time_series_start = Clock::now_unix_nanos(clock)
  let mut large_time_series = []
  
  // 创建大量时序数据点
  for i = 0; i < 100; i = i + 1 {
    let timestamp = large_time_series_start + (i.to_int64() * 10000000L) // 每10ms一个点
    let value = i.to_double() * 1.5
    large_time_series.push((timestamp, value))
  }
  
  // 验证大规模时序数据
  assert_eq(large_time_series.length(), 100, "大规模时序数据点数量应该正确")
  
  // 验证时序数据的顺序性
  for i = 0; i < large_time_series.length() - 1; i = i + 1 {
    let current = large_time_series[i]
    let next = large_time_series[i + 1]
    
    assert_true(current.0 < next.0, "大规模时序数据时间应该递增")
    assert_true(current.1 < next.1, "大规模时序数据值应该递增")
  }
  
  // 验证时序数据的时间跨度
  let series_start = large_time_series[0].0
  let series_end = large_time_series[large_time_series.length() - 1].0
  let series_duration = series_end - series_start
  let expected_duration = 990000000L // 99 * 10ms
  
  assert_eq(series_duration, expected_duration, "时序数据时间跨度应该正确")
  
  // 创建性能测试相关的度量
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance-test")
  let performance_counter = Meter::create_counter(meter, "time.series.operations")
  let performance_histogram = Meter::create_histogram(meter, "time.series.processing.time")
  
  // 记录性能测试度量
  Counter::add(performance_counter, large_time_series.length().to_double())
  Histogram::record(performance_histogram, series_duration.to_double())
  
  // 验证性能测试度量
  assert_eq(performance_counter.name, "time.series.operations", "性能计数器名称应该正确")
  assert_eq(performance_histogram.name, "time.series.processing.time", "性能直方图名称应该正确")
}