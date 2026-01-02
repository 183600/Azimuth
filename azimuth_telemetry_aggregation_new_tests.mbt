// Azimuth Telemetry Data Aggregation Tests
// 测试遥测数据聚合功能

test "metric aggregation with sum operation" {
  // 创建指标聚合器
  let aggregator = @azimuth.MetricAggregator::new_sum_aggregator()
  
  // 添加数据点
  aggregator.add_data_point(10.5)
  aggregator.add_data_point(20.3)
  aggregator.add_data_point(15.7)
  
  // 验证聚合结果
  let result = aggregator.get_result()
  assert_eq(result.value, 46.5)
  assert_eq(result.count, 3)
  assert_eq(result.operation, "sum")
}

test "metric aggregation with average operation" {
  // 创建平均值聚合器
  let aggregator = @azimuth.MetricAggregator::new_average_aggregator()
  
  // 添加数据点
  aggregator.add_data_point(10.0)
  aggregator.add_data_point(20.0)
  aggregator.add_data_point(30.0)
  
  // 验证聚合结果
  let result = aggregator.get_result()
  assert_eq(result.value, 20.0)
  assert_eq(result.count, 3)
  assert_eq(result.operation, "average")
}

test "metric aggregation with min/max operations" {
  // 创建最小值聚合器
  let min_aggregator = @azimuth.MetricAggregator::new_min_aggregator()
  min_aggregator.add_data_point(15.2)
  min_aggregator.add_data_point(8.7)
  min_aggregator.add_data_point(23.1)
  
  let min_result = min_aggregator.get_result()
  assert_eq(min_result.value, 8.7)
  
  // 创建最大值聚合器
  let max_aggregator = @azimuth.MetricAggregator::new_max_aggregator()
  max_aggregator.add_data_point(15.2)
  max_aggregator.add_data_point(8.7)
  max_aggregator.add_data_point(23.1)
  
  let max_result = max_aggregator.get_result()
  assert_eq(max_result.value, 23.1)
}

test "histogram aggregation with buckets" {
  // 创建直方图聚合器
  let buckets = [0.0, 10.0, 20.0, 30.0, 40.0]
  let histogram = @azimuth.HistogramAggregator::new(buckets)
  
  // 添加数据点
  histogram.observe(5.5)
  histogram.observe(15.2)
  histogram.observe(25.8)
  histogram.observe(35.1)
  histogram.observe(12.3)
  
  // 验证桶计数
  let counts = histogram.get_bucket_counts()
  assert_eq(counts[0], 0)  // 0-10
  assert_eq(counts[1], 2)  // 10-20
  assert_eq(counts[2], 1)  // 20-30
  assert_eq(counts[3], 1)  // 30-40
  assert_eq(counts[4], 1)  // 40+
  
  // 验证统计信息
  let stats = histogram.get_statistics()
  assert_eq(stats.count, 5)
  assert_eq(stats.sum, 93.9)
  assert_true(stats.min > 5.0 && stats.min < 6.0)
  assert_true(stats.max > 35.0 && stats.max < 36.0)
}

test "time windowed aggregation" {
  // 创建时间窗口聚合器
  let window_size = 60000  // 60秒窗口
  let time_aggregator = @azimuth.TimeWindowedAggregator::new(window_size)
  
  // 模拟不同时间点的数据
  let base_time = @azimuth.Time::now()
  time_aggregator.add_data_point_at(10.0, base_time)
  time_aggregator.add_data_point_at(20.0, base_time + 30000)  // 30秒后
  time_aggregator.add_data_point_at(15.0, base_time + 90000)  // 90秒后，超出窗口
  
  // 验证窗口内的数据
  let window_result = time_aggregator.get_window_result(base_time + 60000)
  assert_eq(window_result.count, 2)  // 只有前两个数据点在窗口内
  assert_eq(window_result.sum, 30.0)
  assert_eq(window_result.average, 15.0)
}

test "multi-dimensional aggregation with attributes" {
  // 创建多维聚合器
  let multi_aggregator = @azimuth.MultiDimensionalAggregator::new()
  
  // 添加带属性的数据点
  let attrs1 = @azimuth.Attributes {
    values : [
      ("service", @azimuth.StringValue("auth")),
      ("endpoint", @azimuth.StringValue("/login"))
    ]
  }
  
  let attrs2 = @azimuth.Attributes {
    values : [
      ("service", @azimuth.StringValue("auth")),
      ("endpoint", @azimuth.StringValue("/logout"))
    ]
  }
  
  multi_aggregator.add_data_point_with_attributes(100.0, attrs1)
  multi_aggregator.add_data_point_with_attributes(150.0, attrs1)
  multi_aggregator.add_data_point_with_attributes(80.0, attrs2)
  
  // 验证按属性分组的聚合结果
  let auth_login_result = multi_aggregator.get_result_for_attributes(attrs1)
  assert_eq(auth_login_result.count, 2)
  assert_eq(auth_login_result.sum, 250.0)
  
  let auth_logout_result = multi_aggregator.get_result_for_attributes(attrs2)
  assert_eq(auth_logout_result.count, 1)
  assert_eq(auth_logout_result.sum, 80.0)
}