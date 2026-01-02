// Azimuth 时间序列数据操作测试用例
// 专注于遥测系统的时间序列数据处理和聚合功能

// 测试1: 时间序列数据聚合
test "时间序列数据聚合测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "time.series.aggregation")
  
  // 创建时间序列直方图
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time")
  
  // 记录不同时间点的响应时间数据
  let timestamps = [1000, 2000, 3000, 4000, 5000] // Unix时间戳
  let response_times = [0.1, 0.15, 0.12, 0.2, 0.08]
  
  for i in 0..timestamps.length() {
    Histogram::record_with_timestamp(response_time_histogram, response_times[i], timestamps[i])
  }
  
  // 创建时间序列聚合器
  let aggregator = TimeSeriesAggregator::new()
  TimeSeriesAggregator::set_time_window(aggregator, 5000) // 5秒窗口
  
  // 计算聚合统计
  let avg_response_time = TimeSeriesAggregator::average(aggregator, response_time_histogram)
  let max_response_time = TimeSeriesAggregator::maximum(aggregator, response_time_histogram)
  let min_response_time = TimeSeriesAggregator::minimum(aggregator, response_time_histogram)
  let p95_response_time = TimeSeriesAggregator::percentile(aggregator, response_time_histogram, 0.95)
  
  // 验证聚合结果
  assert_true(avg_response_time > 0.1 && avg_response_time < 0.2)
  assert_eq(max_response_time, 0.2)
  assert_eq(min_response_time, 0.08)
  assert_true(p95_response_time >= 0.15 && p95_response_time <= 0.2)
}
