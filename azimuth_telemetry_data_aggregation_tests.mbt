// Azimuth 遥测数据聚合测试用例
// 测试遥测数据的聚合功能，包括时间序列数据聚合、度量聚合和追踪数据聚合

// 测试1: 时间序列数据聚合
test "时间序列数据聚合功能" {
  // 创建时间序列聚合器
  let aggregator = TimeSeriesAggregator::new("cpu.usage", 60) // 60秒窗口
  
  // 添加时间序列数据点
  TimeSeriesAggregator::add_point(aggregator, 1000, 45.5)  // 时间戳, 值
  TimeSeriesAggregator::add_point(aggregator, 1010, 48.2)
  TimeSeriesAggregator::add_point(aggregator, 1020, 52.1)
  TimeSeriesAggregator::add_point(aggregator, 1030, 49.8)
  TimeSeriesAggregator::add_point(aggregator, 1040, 51.3)
  
  // 测试聚合结果
  let avg_result = TimeSeriesAggregator::average(aggregator)
  let max_result = TimeSeriesAggregator::maximum(aggregator)
  let min_result = TimeSeriesAggregator::minimum(aggregator)
  let sum_result = TimeSeriesAggregator::sum(aggregator)
  
  assert_true(avg_result > 45.0 && avg_result < 55.0)
  assert_eq(max_result, 52.1)
  assert_eq(min_result, 45.5)
  assert_true(sum_result > 200.0 && sum_result < 300.0)
  
  // 测试百分位数计算
  let p95_result = TimeSeriesAggregator::percentile(aggregator, 95)
  let p50_result = TimeSeriesAggregator::percentile(aggregator, 50)
  
  assert_true(p95_result > 50.0)
  assert_true(p50_result > 45.0 && p50_result < 55.0)
}

// 测试2: 度量数据聚合
test "度量数据聚合功能" {
  // 创建度量聚合器
  let metric_aggregator = MetricAggregator::new()
  
  // 添加计数器数据
  MetricAggregator::add_counter(metric_aggregator, "http.requests", 10.0, [("method", "GET"), ("status", "200")])
  MetricAggregator::add_counter(metric_aggregator, "http.requests", 5.0, [("method", "POST"), ("status", "201")])
  MetricAggregator::add_counter(metric_aggregator, "http.requests", 3.0, [("method", "GET"), ("status", "404")])
  
  // 添加仪表数据
  MetricAggregator::set_gauge(metric_aggregator, "system.memory", 1024.5, [("type", "used")])
  MetricAggregator::set_gauge(metric_aggregator, "system.memory", 2048.0, [("type", "total")])
  
  // 添加直方图数据
  MetricAggregator::record_histogram(metric_aggregator, "response.time", 120.5, [("endpoint", "/api/data")])
  MetricAggregator::record_histogram(metric_aggregator, "response.time", 85.3, [("endpoint", "/api/data")])
  MetricAggregator::record_histogram(metric_aggregator, "response.time", 200.1, [("endpoint", "/api/data")])
  
  // 测试聚合查询
  let total_requests = MetricAggregator::get_counter_sum(metric_aggregator, "http.requests")
  let get_requests = MetricAggregator::get_counter_sum_with_attributes(metric_aggregator, "http.requests", [("method", "GET")])
  let avg_response_time = MetricAggregator::get_histogram_average(metric_aggregator, "response.time", [("endpoint", "/api/data")])
  
  assert_eq(total_requests, 18.0)
  assert_eq(get_requests, 13.0)
  assert_true(avg_response_time > 100.0 && avg_response_time < 150.0)
}

// 测试3: 追踪数据聚合
test "追踪数据聚合功能" {
  // 创建追踪聚合器
  let trace_aggregator = TraceAggregator::new()
  
  // 添加span数据
  let span1 = SpanData::new("operation1", 1000, 1100, [("service", "auth")]) // 名称, 开始时间, 结束时间, 属性
  let span2 = SpanData::new("operation2", 1050, 1150, [("service", "auth")])
  let span3 = SpanData::new("operation3", 1200, 1250, [("service", "data")])
  let span4 = SpanData::new("operation4", 1300, 1400, [("service", "data")])
  
  TraceAggregator::add_span(trace_aggregator, span1)
  TraceAggregator::add_span(trace_aggregator, span2)
  TraceAggregator::add_span(trace_aggregator, span3)
  TraceAggregator::add_span(trace_aggregator, span4)
  
  // 测试聚合统计
  let total_spans = TraceAggregator::get_span_count(trace_aggregator)
  let auth_spans = TraceAggregator::get_span_count_by_attribute(trace_aggregator, "service", "auth")
  let data_spans = TraceAggregator::get_span_count_by_attribute(trace_aggregator, "service", "data")
  let avg_duration = TraceAggregator::get_average_duration(trace_aggregator)
  
  assert_eq(total_spans, 4)
  assert_eq(auth_spans, 2)
  assert_eq(data_spans, 2)
  assert_true(avg_duration > 50.0 && avg_duration < 150.0)
  
  // 测试错误率计算
  let error_span1 = SpanData::new("error_operation1", 1500, 1600, [("service", "auth"), ("error", "true")])
  let error_span2 = SpanData::new("error_operation2", 1600, 1650, [("service", "auth"), ("error", "true")])
  let normal_span = SpanData::new("normal_operation", 1700, 1750, [("service", "auth")])
  
  TraceAggregator::add_span(trace_aggregator, error_span1)
  TraceAggregator::add_span(trace_aggregator, error_span2)
  TraceAggregator::add_span(trace_aggregator, normal_span)
  
  let auth_error_rate = TraceAggregator::get_error_rate_by_attribute(trace_aggregator, "service", "auth")
  assert_true(auth_error_rate > 0.3 && auth_error_rate < 0.5) // 2个错误/4个auth操作
}

// 测试4: 多维度数据聚合
test "多维度数据聚合功能" {
  // 创建多维度聚合器
  let multi_dim_aggregator = MultiDimensionalAggregator::new()
  
  // 添加多维度数据点
  MultiDimensionalAggregator::add_point(multi_dim_aggregator, 
    [("service", "api"), ("region", "us-east"), ("method", "GET")], 
    100.5, 1000) // 值, 时间戳
  MultiDimensionalAggregator::add_point(multi_dim_aggregator, 
    [("service", "api"), ("region", "us-east"), ("method", "POST")], 
    150.2, 1010)
  MultiDimensionalAggregator::add_point(multi_dim_aggregator, 
    [("service", "api"), ("region", "us-west"), ("method", "GET")], 
    120.8, 1020)
  MultiDimensionalAggregator::add_point(multi_dim_aggregator, 
    [("service", "web"), ("region", "us-east"), ("method", "GET")], 
    80.3, 1030)
  
  // 测试多维度查询
  let api_avg = MultiDimensionalAggregator::average_by_attributes(multi_dim_aggregator, [("service", "api")])
  let us_east_avg = MultiDimensionalAggregator::average_by_attributes(multi_dim_aggregator, [("region", "us-east")])
  let get_method_avg = MultiDimensionalAggregator::average_by_attributes(multi_dim_aggregator, [("method", "GET")])
  let api_us_east_get_avg = MultiDimensionalAggregator::average_by_attributes(multi_dim_aggregator, 
    [("service", "api"), ("region", "us-east"), ("method", "GET")])
  
  assert_true(api_avg > 100.0 && api_avg < 150.0)
  assert_true(us_east_avg > 90.0 && us_east_avg < 120.0)
  assert_true(get_method_avg > 80.0 && get_method_avg < 110.0)
  assert_eq(api_us_east_get_avg, 100.5)
  
  // 测试维度组合查询
  let combinations = MultiDimensionalAggregator::get_attribute_combinations(multi_dim_aggregator, ["service", "region"])
  assert_true(combinations.length() >= 3) // api+us-east, api+us-west, web+us-east
}

// 测试5: 实时数据流聚合
test "实时数据流聚合功能" {
  // 创建实时流聚合器
  let stream_aggregator = StreamAggregator::new(100) // 100个数据点的滑动窗口
  
  // 模拟实时数据流
  for i = 0; i < 50; i = i + 1 {
    let timestamp = 1000 + i * 10
    let value = 50.0 + (i % 10).to_float() * 2.5
    StreamAggregator::add_data_point(stream_aggregator, timestamp, value, [("source", "sensor1")])
  }
  
  // 测试流聚合统计
  let current_avg = StreamAggregator::get_current_average(stream_aggregator)
  let current_max = StreamAggregator::get_current_maximum(stream_aggregator)
  let current_min = StreamAggregator::get_current_minimum(stream_aggregator)
  let trend = StreamAggregator::get_trend(stream_aggregator) // 计算趋势
  
  assert_true(current_avg > 50.0 && current_avg < 70.0)
  assert_true(current_max > 70.0)
  assert_true(current_min < 60.0)
  assert_true(trend >= -1.0 && trend <= 1.0) // 趋势值在合理范围内
  
  // 测试滑动窗口更新
  for i = 50; i < 150; i = i + 1 {
    let timestamp = 1000 + i * 10
    let value = 60.0 + (i % 15).to_float() * 3.0
    StreamAggregator::add_data_point(stream_aggregator, timestamp, value, [("source", "sensor2")])
  }
  
  let updated_avg = StreamAggregator::get_current_average(stream_aggregator)
  let sensor1_count = StreamAggregator::get_count_by_attributes(stream_aggregator, [("source", "sensor1")])
  let sensor2_count = StreamAggregator::get_count_by_attributes(stream_aggregator, [("source", "sensor2")])
  
  assert_true(updated_avg > current_avg) // 平均值应该增加
  assert_eq(sensor1_count, 50) // 最初的50个数据点
  assert_eq(sensor2_count, 50) // 新的50个数据点
}