// Azimuth Telemetry Data Aggregation and Analysis Tests
// 专注于遥测数据聚合、分析和统计操作的高级测试用例

test "遥测数据时间窗口聚合测试" {
  // 创建时间序列数据点
  let data_points = [
    (1000, 10.5),  // (timestamp, value)
    (2000, 15.2),
    (3000, 12.8),
    (4000, 18.3),
    (5000, 14.7),
    (6000, 16.9),
    (7000, 13.4),
    (8000, 19.1)
  ]
  
  // 测试按时间窗口聚合（2秒窗口）
  let time_windows = azimuth::aggregate_by_time_window(data_points, 2000)
  
  // 验证窗口数量
  assert_eq(time_windows.length(), 4)
  
  // 验证第一个窗口 (1000-3000)
  let window1 = time_windows[0]
  assert_eq(window1.start_time, 1000)
  assert_eq(window1.end_time, 3000)
  assert_eq(window1.count, 3)
  assert_true(window1.avg > 12.0 && window1.avg < 13.0)
  
  // 验证最后一个窗口 (7000-9000)
  let window4 = time_windows[3]
  assert_eq(window4.start_time, 7000)
  assert_eq(window4.end_time, 9000)
  assert_eq(window4.count, 2)
  assert_true(window4.avg > 16.0 && window4.avg < 17.0)
}

test "遥测指标百分位数计算测试" {
  // 创建响应时间数据集
  let response_times = [
    10, 15, 23, 42, 67, 89, 95, 120, 145, 167,
    189, 210, 245, 267, 289, 310, 345, 367, 389, 410,
    445, 467, 489, 510, 545, 567, 589, 610, 645, 667,
    689, 710, 745, 767, 789, 810, 845, 867, 889, 910,
    945, 967, 989, 1010, 1045, 1067, 1089, 1110, 1145, 1167
  ]
  
  // 计算百分位数
  let p50 = azimuth::calculate_percentile(response_times, 50.0)
  let p90 = azimuth::calculate_percentile(response_times, 90.0)
  let p95 = azimuth::calculate_percentile(response_times, 95.0)
  let p99 = azimuth::calculate_percentile(response_times, 99.0)
  
  // 验证百分位数
  assert_true(p50 > 400 && p50 < 500)  // 中位数应该在400-500之间
  assert_true(p90 > 1000 && p90 < 1100)  // 90%应该在1000-1100之间
  assert_true(p95 > 1100 && p95 < 1200)  // 95%应该在1100-1200之间
  assert_true(p99 > 1150 && p99 < 1200)  // 99%应该在1150-1200之间
}

test "遥测数据异常检测测试" {
  // 创建包含异常值的数据集
  let normal_data = [
    10.2, 11.5, 9.8, 10.7, 11.2, 10.9, 10.3, 11.1, 
    10.6, 10.4, 11.3, 10.8, 10.1, 11.4, 10.5
  ]
  
  // 添加异常值
  let data_with_outliers = normal_data + [45.2, 0.1, 38.7]
  
  // 使用IQR方法检测异常值
  let outliers = azimuth::detect_outliers_iqr(data_with_outliers)
  
  // 验证异常值检测
  assert_eq(outliers.length(), 3)
  assert_true(outliers.contains(45.2))
  assert_true(outliers.contains(0.1))
  assert_true(outliers.contains(38.7))
  
  // 验证正常值不被标记为异常
  for value in normal_data {
    assert_false(outliers.contains(value))
  }
}

test "遥测数据趋势分析测试" {
  // 创建时间序列数据（上升趋势）
  let increasing_data = [
    (1000, 10.0), (2000, 12.5), (3000, 15.2), (4000, 18.1),
    (5000, 21.3), (6000, 24.8), (7000, 28.5), (8000, 32.4)
  ]
  
  // 计算趋势
  let increasing_trend = azimuth::calculate_trend(increasing_data)
  assert_true(increasing_trend.slope > 0.0)
  assert_true(increasing_trend.correlation > 0.9)
  
  // 创建时间序列数据（下降趋势）
  let decreasing_data = [
    (1000, 50.0), (2000, 42.5), (3000, 35.2), (4000, 28.1),
    (5000, 21.3), (6000, 14.8), (7000, 8.5), (8000, 2.4)
  ]
  
  let decreasing_trend = azimuth::calculate_trend(decreasing_data)
  assert_true(decreasing_trend.slope < 0.0)
  assert_true(decreasing_trend.correlation < -0.9)
  
  // 创建时间序列数据（平稳趋势）
  let stable_data = [
    (1000, 20.0), (2000, 20.5), (3000, 19.8), (4000, 20.2),
    (5000, 20.1), (6000, 19.9), (7000, 20.3), (8000, 20.0)
  ]
  
  let stable_trend = azimuth::calculate_trend(stable_data)
  assert_true(stable_trend.slope > -0.1 && stable_trend.slope < 0.1)
  assert_true(stable_trend.correlation > -0.3 && stable_trend.correlation < 0.3)
}

test "遥测数据多维聚合测试" {
  // 创建多维遥测数据
  let multi_dimensional_data = [
    ("service-A", "endpoint-1", "success", 100),
    ("service-A", "endpoint-1", "error", 5),
    ("service-A", "endpoint-2", "success", 200),
    ("service-A", "endpoint-2", "error", 10),
    ("service-B", "endpoint-1", "success", 150),
    ("service-B", "endpoint-1", "error", 8),
    ("service-B", "endpoint-2", "success", 120),
    ("service-B", "endpoint-2", "error", 3)
  ]
  
  // 按服务名称聚合
  let service_aggregation = azimuth::aggregate_by_dimension(multi_dimensional_data, 0)
  assert_eq(service_aggregation.length(), 2)
  
  let service_a = service_aggregation.find(fn(item) { item.key == "service-A" })
  match service_a {
    Some(result) => {
      assert_eq(result.total_count, 315)  // 100+5+200+10
      assert_eq(result.success_count, 300)  // 100+200
      assert_eq(result.error_count, 15)  // 5+10
    }
    None => assert_true(false)
  }
  
  // 按端点聚合
  let endpoint_aggregation = azimuth::aggregate_by_dimension(multi_dimensional_data, 1)
  assert_eq(endpoint_aggregation.length(), 2)
  
  let endpoint1 = endpoint_aggregation.find(fn(item) { item.key == "endpoint-1" })
  match endpoint1 {
    Some(result) => {
      assert_eq(result.total_count, 263)  // 100+5+150+8
      assert_eq(result.success_count, 250)  // 100+150
      assert_eq(result.error_count, 13)  // 5+8
    }
    None => assert_true(false)
  }
  
  // 按状态聚合
  let status_aggregation = azimuth::aggregate_by_dimension(multi_dimensional_data, 2)
  assert_eq(status_aggregation.length(), 2)
  
  let success_status = status_aggregation.find(fn(item) { item.key == "success" })
  match success_status {
    Some(result) => {
      assert_eq(result.total_count, 570)  // 100+200+150+120
      assert_eq(result.success_count, 570)
      assert_eq(result.error_count, 0)
    }
    None => assert_true(false)
  }
}

test "遥测数据实时滑动窗口统计测试" {
  // 创建时间序列数据流
  let data_stream = [
    (1000, 10), (1500, 15), (2000, 12), (2500, 18), (3000, 14),
    (3500, 20), (4000, 16), (4500, 22), (5000, 19), (5500, 25)
  ]
  
  // 使用滑动窗口计算实时统计
  let window_size = 2000  // 2秒窗口
  let sliding_stats = azimuth::calculate_sliding_window_stats(data_stream, window_size)
  
  // 验证滑动窗口统计结果
  assert_eq(sliding_stats.length(), data_stream.length())
  
  // 验证第一个窗口（只有一个数据点）
  let first_stat = sliding_stats[0]
  assert_eq(first_stat.count, 1)
  assert_eq(first_stat.sum, 10)
  assert_eq(first_stat.avg, 10.0)
  assert_eq(first_stat.min, 10)
  assert_eq(first_stat.max, 10)
  
  // 验证中间窗口（多个数据点）
  let middle_stat = sliding_stats[4]  // 时间点3000
  assert_eq(middle_stat.count, 3)  // 2000, 2500, 3000的数据点
  assert_eq(middle_stat.sum, 44)  // 12+18+14
  assert_eq(middle_stat.avg, 44.0 / 3.0)
  assert_eq(middle_stat.min, 12)
  assert_eq(middle_stat.max, 18)
  
  // 验证最后一个窗口
  let last_stat = sliding_stats[9]
  assert_eq(last_stat.count, 4)  // 3500, 4000, 4500, 5000的数据点
  assert_eq(last_stat.sum, 77)  // 20+16+22+19
  assert_eq(last_stat.avg, 77.0 / 4.0)
  assert_eq(last_stat.min, 16)
  assert_eq(last_stat.max, 22)
}

test "遥测数据相关性分析测试" {
  // 创建两个相关的数据集
  let cpu_usage = [10, 15, 20, 25, 30, 35, 40, 45, 50, 55]
  let memory_usage = [100, 150, 200, 250, 300, 350, 400, 450, 500, 550]
  
  // 计算相关性
  let correlation = azimuth::calculate_correlation(cpu_usage, memory_usage)
  assert_true(correlation > 0.99)  // 强正相关
  
  // 创建负相关的数据集
  let response_time = [100, 90, 80, 70, 60, 50, 40, 30, 20, 10]
  let throughput = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  
  let negative_correlation = azimuth::calculate_correlation(response_time, throughput)
  assert_true(negative_correlation < -0.99)  // 强负相关
  
  // 创建无关的数据集
  let random_data1 = [10, 25, 15, 30, 20, 35, 18, 40, 22, 45]
  let random_data2 = [100, 80, 120, 90, 110, 85, 115, 95, 105, 88]
  
  let no_correlation = azimuth::calculate_correlation(random_data1, random_data2)
  assert_true(no_correlation > -0.3 && no_correlation < 0.3)  // 几乎无相关性
}