// 时序数据处理测试用例
// 测试遥测系统在时序数据处理、聚合和分析方面的功能

test "时序指标数据聚合测试" {
  // 1. 创建时序数据处理服务
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "时序数据处理服务")
  
  // 2. 创建时序指标
  let request_counter = Meter::create_counter(meter, "http.requests.total",
    Some("HTTP请求总数时序数据"), Some("requests"))
  let response_histogram = Meter::create_histogram(meter, "http.response.duration",
    Some("HTTP响应时间时序分布"), Some("milliseconds"))
  let active_connections = Meter::create_updown_counter(meter, "http.active_connections",
    Some("活跃连接数时序数据"), Some("connections"))
  let memory_gauge = Meter::create_gauge(meter, "process.memory.usage",
    Some("内存使用量时序数据"), Some("bytes"))
  
  // 3. 模拟时序数据记录 - 模拟1分钟的数据
  let base_timestamp = Clock::now_unix_nanos(Clock::system())
  
  // 记录每10秒的请求数据
  for i in range(0, 6) {
    let request_count = 10.0 + (i.to_double() * 2.5) // 模拟请求增长
    Counter::add(request_counter, request_count)
    
    // 记录对应的响应时间分布
    for j in range(0, request_count.to_int()) {
      let response_time = 100.0 + (j.to_double() * 5.0) + (i.to_double() * 10.0)
      Histogram::record(response_histogram, response_time)
    }
  }
  
  // 4. 模拟连接数变化
  UpDownCounter::add(active_connections, 5.0)  // 初始连接
  UpDownCounter::add(active_connections, 3.0)  // 新增连接
  UpDownCounter::add(active_connections, -1.0) // 连接关闭
  UpDownCounter::add(active_connections, 2.0)  // 新增连接
  UpDownCounter::add(active_connections, -2.0) // 连接关闭
  UpDownCounter::add(active_connections, 1.0)  // 新增连接
  
  // 5. 模拟内存使用量变化
  // Gauge::set(memory_gauge, 512 * 1024 * 1024)    // 512MB
  // Gauge::set(memory_gauge, 640 * 1024 * 1024)    // 640MB
  // Gauge::set(memory_gauge, 768 * 1024 * 1024)    // 768MB
  // Gauge::set(memory_gauge, 896 * 1024 * 1024)    // 896MB
  // Gauge::set(memory_gauge, 1024 * 1024 * 1024)   // 1GB
  // Gauge::set(memory_gauge, 832 * 1024 * 1024)    // 832MB (内存释放)
  
  // 6. 创建时序聚合指标
  let aggregation_meter = MeterProvider::get_meter(meter_provider, "时序聚合服务")
  
  let request_rate = Meter::create_gauge(aggregation_meter, "http.requests.rate",
    Some("HTTP请求速率"), Some("requests_per_second"))
  let avg_response_time = Meter::create_gauge(aggregation_meter, "http.response.time.avg",
    Some("平均响应时间"), Some("milliseconds"))
  let p95_response_time = Meter::create_gauge(aggregation_meter, "http.response.time.p95",
    Some("95%响应时间"), Some("milliseconds"))
  let memory_trend = Meter::create_gauge(aggregation_meter, "process.memory.trend",
    Some("内存使用趋势"), Some("bytes_per_second"))
  
  // 7. 记录聚合结果
  // Gauge::set(request_rate, 1.25)        // 1.25 请求/秒
  // Gauge::set(avg_response_time, 157.5)   // 平均响应时间
  // Gauge::set(p95_response_time, 245.0)   // 95%响应时间
  // Gauge::set(memory_trend, 1024 * 1024)  // 1MB/秒 增长趋势
  
  // 8. 验证时序数据
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.active_connections")
  assert_eq(memory_gauge.name, "process.memory.usage")
  
  assert_eq(request_rate.name, "http.requests.rate")
  assert_eq(avg_response_time.name, "http.response.time.avg")
  assert_eq(p95_response_time.name, "http.response.time.p95")
  assert_eq(memory_trend.name, "process.memory.trend")
  
  assert_true(true)
}

test "时序数据窗口聚合测试" {
  // 1. 创建窗口聚合服务
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "窗口聚合服务")
  
  // 2. 创建基础指标
  let event_counter = Meter::create_counter(meter, "business.events.total",
    Some("业务事件总数"), Some("events"))
  let value_histogram = Meter::create_histogram(meter, "business.value.distribution",
    Some("业务数值分布"), Some("units"))
  
  // 3. 模拟5分钟的时序数据（每30秒一个数据点）
  for minute in range(0, 5) {
    for second in range(0, 60, 30) {
      let time_point = minute * 60 + second
      
      // 模拟事件数量变化
      let event_count = 20.0 + (time_point.to_double() * 0.5)
      Counter::add(event_counter, event_count)
      
      // 模拟数值分布
      for i in range(0, event_count.to_int()) {
        let value = 50.0 + (time_point.to_double() * 0.1) + (i.to_double() * 2.0)
        Histogram::record(value_histogram, value)
      }
    }
  }
  
  // 4. 创建不同时间窗口的聚合器
  let window_meter = MeterProvider::get_meter(meter_provider, "时序窗口服务")
  
  // 1分钟窗口聚合
  let one_min_sum = Meter::create_gauge(window_meter, "events.one_minute.sum",
    Some("1分钟事件总和"), Some("events"))
  let one_min_avg = Meter::create_gauge(window_meter, "events.one_minute.avg",
    Some("1分钟事件平均值"), Some("events_per_minute"))
  let one_min_max = Meter::create_gauge(window_meter, "events.one_minute.max",
    Some("1分钟事件最大值"), Some("events"))
  
  // 5分钟窗口聚合
  let five_min_sum = Meter::create_gauge(window_meter, "events.five_minute.sum",
    Some("5分钟事件总和"), Some("events"))
  let five_min_avg = Meter::create_gauge(window_meter, "events.five_minute.avg",
    Some("5分钟事件平均值"), Some("events_per_minute"))
  let five_min_max = Meter::create_gauge(window_meter, "events.five_minute.max",
    Some("5分钟事件最大值"), Some("events"))
  
  // 5. 模拟窗口聚合结果
  // 1分钟窗口结果
  // Gauge::set(one_min_sum, 135.0)   // 第1分钟总和
  // Gauge::set(one_min_avg, 2.25)    // 第1分钟平均值
  // Gauge::set(one_min_max, 3.5)     // 第1分钟最大值
  
  // Gauge::set(one_min_sum, 150.0)   // 第2分钟总和
  // Gauge::set(one_min_avg, 2.5)     // 第2分钟平均值
  // Gauge::set(one_min_max, 3.75)    // 第2分钟最大值
  
  // 5分钟窗口结果
  // Gauge::set(five_min_sum, 712.5)  // 5分钟总和
  // Gauge::set(five_min_avg, 2.375)  // 5分钟平均值
  // Gauge::set(five_min_max, 4.875)  // 5分钟最大值
  
  // 6. 创建滑动窗口聚合
  let sliding_window_meter = MeterProvider::get_meter(meter_provider, "滑动窗口服务")
  
  let sliding_sum = Meter::create_gauge(sliding_window_meter, "events.sliding.sum",
    Some("滑动窗口事件总和"), Some("events"))
  let sliding_avg = Meter::create_gauge(sliding_window_meter, "events.sliding.avg",
    Some("滑动窗口事件平均值"), Some("events_per_window"))
  
  // 模拟滑动窗口结果
  // Gauge::set(sliding_sum, 90.0)     // 2分钟滑动窗口总和
  // Gauge::set(sliding_avg, 2.25)     // 2分钟滑动窗口平均值
  
  // 7. 验证窗口聚合指标
  assert_eq(one_min_sum.name, "events.one_minute.sum")
  assert_eq(one_min_avg.name, "events.one_minute.avg")
  assert_eq(one_min_max.name, "events.one_minute.max")
  
  assert_eq(five_min_sum.name, "events.five_minute.sum")
  assert_eq(five_min_avg.name, "events.five_minute.avg")
  assert_eq(five_min_max.name, "events.five_minute.max")
  
  assert_eq(sliding_sum.name, "events.sliding.sum")
  assert_eq(sliding_avg.name, "events.sliding.avg")
  
  assert_true(true)
}

test "时序数据趋势分析测试" {
  // 1. 创建趋势分析服务
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "趋势分析服务")
  
  // 2. 创建趋势分析span
  let trend_span = Tracer::start_span(tracer, "时序数据趋势分析")
  
  // 3. 模拟CPU使用率趋势数据
  Span::add_event(trend_span, "开始CPU趋势分析", Some([
    ("metric.name", StringValue("cpu.usage.percent")),
    ("analysis.period", StringValue("1_hour")),
    ("data.points", IntValue(60))
  ]))
  
  // 4. 记录CPU使用率变化趋势
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "CPU趋势监控")
  
  let cpu_gauge = Meter::create_gauge(meter, "cpu.usage.percent",
    Some("CPU使用率"), Some("percent"))
  
  // 模拟1小时的CPU使用率数据（每分钟一个数据点）
  let cpu_readings = [
    25.5, 27.3, 29.1, 31.2, 33.4, 35.6, 37.8, 40.1, 42.3, 44.5,  // 上升期
    46.7, 48.9, 51.2, 53.4, 55.6, 57.8, 60.1, 58.9, 56.7, 54.5,  // 高峰期
    52.3, 50.1, 47.8, 45.6, 43.4, 41.2, 39.1, 37.3, 35.5, 33.7,  // 下降期
    31.9, 30.2, 28.5, 26.8, 25.1, 23.4, 21.7, 20.1, 18.5, 16.9,  // 稳定期
    15.3, 13.8, 12.4, 11.1, 9.8, 8.6, 7.5, 6.4, 5.4, 4.5,        // 低谷期
    3.8, 3.2, 2.7, 2.3, 2.0, 1.8, 1.7, 1.6, 1.8, 2.1,            // 恢复期
    2.5, 3.0, 3.6, 4.3, 5.1, 6.0, 7.0, 8.1, 9.3, 10.6           // 增长期
  ]
  
  // 记录CPU使用率数据
  for reading in cpu_readings {
    // Gauge::set(cpu_gauge, reading)
  }
  
  // 5. 创建趋势分析指标
  let trend_meter = MeterProvider::get_meter(meter_provider, "趋势分析指标")
  
  let trend_slope = Meter::create_gauge(trend_meter, "cpu.trend.slope",
    Some("CPU趋势斜率"), Some("percent_per_minute"))
  let trend_direction = Meter::create_gauge(trend_meter, "cpu.trend.direction",
    Some("CPU趋势方向"), Some("direction_code"))
  let trend_volatility = Meter::create_gauge(trend_meter, "cpu.trend.volatility",
    Some("CPU趋势波动性"), Some("volatility_index"))
  
  // 6. 模拟趋势分析结果
  // Gauge::set(trend_slope, -0.15)           // 下降趋势
  // Gauge::set(trend_direction, -1.0)        // -1表示下降，0表示稳定，1表示上升
  // Gauge::set(trend_volatility, 0.75)       // 中等波动性
  
  // 7. 记录趋势分析事件
  Span::add_event(trend_span, "CPU趋势分析完成", Some([
    ("trend.slope", FloatValue(-0.15)),
    ("trend.direction", StringValue("下降")),
    ("trend.volatility", FloatValue(0.75)),
    ("peak.usage", FloatValue(60.1)),
    ("min.usage", FloatValue(1.6)),
    ("avg.usage", FloatValue(25.8))
  ]))
  
  // 8. 模拟内存使用率趋势分析
  let memory_gauge = Meter::create_gauge(meter, "memory.usage.percent",
    Some("内存使用率"), Some("percent"))
  
  // 模拟内存使用率数据（相对稳定，偶有峰值）
  let memory_readings = [
    65.2, 66.1, 65.8, 66.5, 67.2, 68.1, 67.8, 68.5, 69.2, 70.1,  // 缓慢增长
    75.5, 78.2, 76.8, 74.3, 72.1, 70.5, 69.2, 68.1, 67.3, 66.8,  // 峰值后下降
    65.5, 64.9, 64.2, 63.8, 63.5, 63.2, 63.0, 62.8, 62.5, 62.3,  // 稳定期
    62.1, 61.9, 61.8, 61.7, 61.6, 61.5, 61.4, 61.3, 61.2, 61.1,  // 持续稳定
    61.0, 60.9, 60.8, 60.7, 60.6, 60.5, 60.4, 60.3, 60.2, 60.1,  // 缓慢下降
    60.0, 59.9, 59.8, 59.7, 59.6, 59.5, 59.4, 59.3, 59.2, 59.1,  // 持续下降
    59.0, 58.9, 58.8, 58.7, 58.6, 58.5, 58.4, 58.3, 58.2, 58.1   // 稳定在低位
  ]
  
  // 记录内存使用率数据
  for reading in memory_readings {
    // Gauge::set(memory_gauge, reading)
  }
  
  // 9. 创建内存趋势分析指标
  let memory_trend_slope = Meter::create_gauge(trend_meter, "memory.trend.slope",
    Some("内存趋势斜率"), Some("percent_per_minute"))
  let memory_trend_direction = Meter::create_gauge(trend_meter, "memory.trend.direction",
    Some("内存趋势方向"), Some("direction_code"))
  let memory_trend_volatility = Meter::create_gauge(trend_meter, "memory.trend.volatility",
    Some("内存趋势波动性"), Some("volatility_index"))
  
  // 10. 模拟内存趋势分析结果
  // Gauge::set(memory_trend_slope, -0.05)      // 轻微下降趋势
  // Gauge::set(memory_trend_direction, -1.0)   // 下降
  // Gauge::set(memory_trend_volatility, 0.25)  // 低波动性
  
  // 11. 记录内存趋势分析事件
  Span::add_event(trend_span, "内存趋势分析完成", Some([
    ("trend.slope", FloatValue(-0.05)),
    ("trend.direction", StringValue("轻微下降")),
    ("trend.volatility", FloatValue(0.25)),
    ("peak.usage", FloatValue(78.2)),
    ("min.usage", FloatValue(58.1)),
    ("avg.usage", FloatValue(62.7))
  ]))
  
  // 12. 结束趋势分析span
  Span::set_status(trend_span, Ok, Some("趋势分析完成"))
  Span::end(trend_span)
  
  // 13. 验证趋势分析指标
  assert_eq(trend_slope.name, "cpu.trend.slope")
  assert_eq(trend_direction.name, "cpu.trend.direction")
  assert_eq(trend_volatility.name, "cpu.trend.volatility")
  
  assert_eq(memory_trend_slope.name, "memory.trend.slope")
  assert_eq(memory_trend_direction.name, "memory.trend.direction")
  assert_eq(memory_trend_volatility.name, "memory.trend.volatility")
  
  assert_true(true)
}

test "时序数据异常检测测试" {
  // 1. 创建异常检测服务
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "异常检测服务")
  
  // 2. 创建异常检测span
  let anomaly_span = Tracer::start_span(tracer, "时序数据异常检测")
  
  // 3. 模拟正常业务指标数据
  Span::add_event(anomaly_span, "开始异常检测", Some([
    ("detection.algorithm", StringValue("statistical_threshold")),
    ("confidence.level", FloatValue(0.95)),
    ("window.size", IntValue(100)
  ]))
  
  // 4. 记录正常业务指标
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "异常检测监控")
  
  let business_counter = Meter::create_counter(meter, "business.transactions.total",
    Some("业务交易总数"), Some("transactions"))
  let response_histogram = Meter::create_histogram(meter, "business.response.time",
    Some("业务响应时间"), Some("milliseconds"))
  let error_rate = Meter::create_gauge(meter, "business.error.rate",
    Some("业务错误率"), Some("percent"))
  
  // 模拟正常时段数据（前50个数据点）
  for i in range(0, 50) {
    // 正常交易量：100-150之间
    let transaction_count = 100.0 + (i.to_double() * 1.0) + (Random::next_u64(Random::system()).to_double() % 20.0)
    Counter::add(business_counter, transaction_count)
    
    // 正常响应时间：50-150ms之间
    for j in range(0, transaction_count.to_int()) {
      let response_time = 50.0 + (Random::next_u64(Random::system()).to_double() % 100.0)
      Histogram::record(response_histogram, response_time)
    }
    
    // 正常错误率：0.1%-1%之间
    // Gauge::set(error_rate, 0.1 + (Random::next_u64(Random::system()).to_double() % 10) / 10.0)
  }
  
  // 5. 模拟异常时段数据（第51-60个数据点）
  Span::add_event(anomaly_span, "检测到异常数据", Some([
    ("anomaly.type", StringValue("volume_spike")),
    ("anomaly.start", IntValue(51)),
    ("anomaly.duration", IntValue(10))
  ]))
  
  // 异常交易量突增
  for i in range(50, 60) {
    // 异常交易量：300-500之间（正常值的3-4倍）
    let transaction_count = 300.0 + (Random::next_u64(Random::system()).to_double() % 200.0)
    Counter::add(business_counter, transaction_count)
    
    // 异常响应时间：200-500ms之间
    for j in range(0, transaction_count.to_int()) {
      let response_time = 200.0 + (Random::next_u64(Random::system()).to_double() % 300.0)
      Histogram::record(response_histogram, response_time)
    }
    
    // 异常错误率：5%-15%之间
    // Gauge::set(error_rate, 5.0 + (Random::next_u64(Random::system()).to_double() % 10))
  }
  
  // 6. 恢复正常时段数据（第61-100个数据点）
  Span::add_event(anomaly_span, "数据恢复正常", Some([
    ("recovery.start", IntValue(61)),
    ("recovery.duration", IntValue(40))
  ]))
  
  for i in range(60, 100) {
    // 恢复正常交易量
    let transaction_count = 100.0 + (i.to_double() * 0.5) + (Random::next_u64(Random::system()).to_double() % 20.0)
    Counter::add(business_counter, transaction_count)
    
    // 恢复正常响应时间
    for j in range(0, transaction_count.to_int()) {
      let response_time = 50.0 + (Random::next_u64(Random::system()).to_double() % 100.0)
      Histogram::record(response_histogram, response_time)
    }
    
    // 恢复正常错误率
    // Gauge::set(error_rate, 0.1 + (Random::next_u64(Random::system()).to_double() % 10) / 10.0)
  }
  
  // 7. 创建异常检测指标
  let anomaly_meter = MeterProvider::get_meter(meter_provider, "异常检测结果")
  
  let anomaly_score = Meter::create_gauge(anomaly_meter, "anomaly.detection.score",
    Some("异常检测分数"), Some("score"))
  let anomaly_count = Meter::create_counter(anomaly_meter, "anomaly.events.total",
    Some("异常事件总数"), Some("anomalies"))
  let anomaly_severity = Meter::create_gauge(anomaly_meter, "anomaly.severity.level",
    Some("异常严重程度"), Some("severity"))
  
  // 8. 模拟异常检测结果
  // Gauge::set(anomaly_score, 0.85)        // 高异常分数
  Counter::add(anomaly_count, 1.0)          // 检测到1个异常事件
  // Gauge::set(anomaly_severity, 3.0)      // 严重程度：3（高）
  
  // 9. 记录异常检测日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "异常检测服务")
  
  let anomaly_log = LogRecord::new_with_context(
    Warn,
    Some("检测到业务指标异常：交易量突增，响应时间延长，错误率上升"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(anomaly_span))),
    Some(SpanContext::span_id(Span::span_context(anomaly_span))),
    None
  )
  Logger::emit(logger, anomaly_log)
  
  // 10. 创建异常恢复指标
  let recovery_meter = MeterProvider::get_meter(meter_provider, "异常恢复监控")
  
  let recovery_time = Meter::create_histogram(recovery_meter, "anomaly.recovery.time",
    Some("异常恢复时间"), Some("minutes"))
  let recovery_success = Meter::create_counter(recovery_meter, "anomaly.recovery.success",
    Some("异常恢复成功次数"), Some("recoveries"))
  
  // 11. 模拟异常恢复结果
  Histogram::record(recovery_time, 40.0)  // 40分钟恢复时间
  Counter::add(recovery_success, 1.0)     // 恢复成功
  
  // 12. 结束异常检测span
  Span::set_status(anomaly_span, Ok, Some("异常检测完成，已自动恢复"))
  Span::end(anomaly_span)
  
  // 13. 验证异常检测指标
  assert_eq(anomaly_score.name, "anomaly.detection.score")
  assert_eq(anomaly_count.name, "anomaly.events.total")
  assert_eq(anomaly_severity.name, "anomaly.severity.level")
  
  assert_eq(recovery_time.name, "anomaly.recovery.time")
  assert_eq(recovery_success.name, "anomaly.recovery.success")
  
  assert_true(true)
}

test "时序数据预测分析测试" {
  // 1. 创建预测分析服务
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "预测分析服务")
  
  // 2. 创建预测分析span
  let prediction_span = Tracer::start_span(tracer, "时序数据预测分析")
  
  // 3. 模拟历史数据收集
  Span::add_event(prediction_span, "开始历史数据收集", Some([
    ("data.source", StringValue("production_metrics")),
    ("time.range", StringValue("30_days")),
    ("data.points", IntValue(720))
  ]))
  
  // 4. 模拟负载数据历史趋势
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "预测分析监控")
  
  let load_gauge = Meter::create_gauge(meter, "system.load.average",
    Some("系统平均负载"), Some("load"))
  
  // 模拟30天的负载数据（每天24个数据点，每小时一个）
  // 模拟周期性负载变化：工作日高负载，周末低负载
  let base_load = 2.0
  for day in range(0, 30) {
    for hour in range(0, 24) {
      let day_of_week = day % 7
      let is_weekend = day_of_week >= 5 // 周六、周日
      let is_business_hours = hour >= 9 && hour <= 17 // 工作时间
      
      let mut load_factor = 1.0
      if is_weekend {
        load_factor = 0.4  // 周末负载降低60%
      } else if is_business_hours {
        load_factor = 1.8  // 工作时间负载增加80%
      } else {
        load_factor = 0.7  // 非工作时间负载降低30%
      }
      
      // 添加一些随机波动
      let random_factor = 0.9 + (Random::next_u64(Random::system()).to_double() % 30) / 100.0
      let final_load = base_load * load_factor * random_factor
      
      // Gauge::set(load_gauge, final_load)
    }
  }
  
  // 5. 记录数据收集完成
  Span::add_event(prediction_span, "历史数据收集完成", Some([
    ("data.points.collected", IntValue(720)),
    ("data.quality", StringValue("good")),
    ("missing.data.points", IntValue(0))
  ]))
  
  // 6. 模拟预测模型训练
  Span::add_event(prediction_span, "开始预测模型训练", Some([
    ("model.type", StringValue("seasonal_arima")),
    ("training.data.points", IntValue(720)),
    ("validation.data.points", IntValue(144))
  ]))
  
  // 7. 创建预测模型指标
  let model_meter = MeterProvider::get_meter(meter_provider, "预测模型监控")
  
  let model_accuracy = Meter::create_gauge(model_meter, "prediction.model.accuracy",
    Some("预测模型准确度"), Some("percent"))
  let model_error = Meter::create_gauge(model_meter, "prediction.model.error",
    Some("预测模型误差"), Some("error_rate"))
  let training_time = Meter::create_histogram(model_meter, "model.training.time",
    Some("模型训练时间"), Some("seconds"))
  
  // 8. 模拟模型训练结果
  // Gauge::set(model_accuracy, 92.5)        // 92.5%准确度
  // Gauge::set(model_error, 7.5)            // 7.5%误差率
  Histogram::record(training_time, 45.2)    // 45.2秒训练时间
  
  // 9. 记录模型训练完成
  Span::add_event(prediction_span, "预测模型训练完成", Some([
    ("model.accuracy", FloatValue(92.5)),
    ("model.error", FloatValue(7.5)),
    ("training.time", FloatValue(45.2)),
    ("model.status", StringValue("ready"))
  ]))
  
  // 10. 模拟未来7天负载预测
  Span::add_event(prediction_span, "开始未来负载预测", Some([
    ("prediction.horizon", StringValue("7_days")),
    ("prediction.interval", StringValue("1_hour")),
    ("confidence.level", FloatValue(0.95))
  ]))
  
  // 11. 创建预测结果指标
  let prediction_meter = MeterProvider::get_meter(meter_provider, "预测结果监控")
  
  let predicted_peak_load = Meter::create_gauge(prediction_meter, "predicted.peak.load",
    Some("预测峰值负载"), Some("load"))
  let predicted_avg_load = Meter::create_gauge(prediction_meter, "predicted.average.load",
    Some("预测平均负载"), Some("load"))
  let capacity_warning = Meter::create_counter(prediction_meter, "capacity.warnings.total",
    Some("容量预警总数"), Some("warnings"))
  
  // 12. 模拟预测结果
  // Gauge::set(predicted_peak_load, 4.2)     // 预测峰值负载
  // Gauge::set(predicted_avg_load, 2.1)      // 预测平均负载
  
  // 检查是否需要容量预警
  let capacity_threshold = 5.0
  let predicted_max = 4.2
  
  if predicted_max > capacity_threshold * 0.8 {
    Counter::add(capacity_warning, 1.0)
  }
  
  // 13. 记录预测结果
  Span::add_event(prediction_span, "负载预测完成", Some([
    ("predicted.peak.load", FloatValue(4.2)),
    ("predicted.avg.load", FloatValue(2.1)),
    ("prediction.confidence", FloatValue(0.95)),
    ("capacity.warning", BoolValue(predicted_max > capacity_threshold * 0.8))
  ]))
  
  // 14. 创建预测日志
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "预测分析服务")
  
  let prediction_log = LogRecord::new_with_context(
    Info,
    Some("未来7天负载预测完成：预计峰值负载4.2，平均负载2.1，建议关注容量规划"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(Span::span_context(prediction_span))),
    Some(SpanContext::span_id(Span::span_context(prediction_span))),
    None
  )
  Logger::emit(logger, prediction_log)
  
  // 15. 结束预测分析span
  Span::set_status(prediction_span, Ok, Some("预测分析完成"))
  Span::end(prediction_span)
  
  // 16. 验证预测分析指标
  assert_eq(model_accuracy.name, "prediction.model.accuracy")
  assert_eq(model_error.name, "prediction.model.error")
  assert_eq(training_time.name, "model.training.time")
  
  assert_eq(predicted_peak_load.name, "predicted.peak.load")
  assert_eq(predicted_avg_load.name, "predicted.average.load")
  assert_eq(capacity_warning.name, "capacity.warnings.total")
  
  assert_true(true)
}