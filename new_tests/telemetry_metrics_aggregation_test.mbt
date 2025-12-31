// 遥测指标聚合测试用例

test "metrics_sum_aggregation" {
  // 测试指标求和聚合
  
  let metric_values = [10, 20, 30, 40, 50]
  let expected_sum = 150
  
  // 计算总和
  let mut sum = 0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(sum, expected_sum)
  assert_eq(sum > 0, true)
  assert_eq(sum >= metric_values[0], true)
}

test "metrics_average_aggregation" {
  // 测试指标平均值聚合
  
  let metric_values = [15, 25, 35, 45, 55]
  let expected_average = 35
  
  // 计算平均值
  let mut sum = 0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let average = sum / metric_values.length()
  
  // 验证平均值计算
  assert_eq(average, expected_average)
  assert_eq(average > metric_values[0], true)
  assert_eq(average < metric_values[4], true)
}

test "metrics_min_max_aggregation" {
  // 测试指标最小值和最大值聚合
  
  let metric_values = [5, 12, 3, 18, 7, 9, 15]
  let expected_min = 3
  let expected_max = 18
  
  // 查找最小值
  let mut min_value = metric_values[0]
  let mut i = 1
  while i < metric_values.length() {
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 查找最大值
  let mut max_value = metric_values[0]
  i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    i = i + 1
  }
  
  // 验证最小值和最大值
  assert_eq(min_value, expected_min)
  assert_eq(max_value, expected_max)
  assert_eq(max_value > min_value, true)
}

test "metrics_percentile_aggregation" {
  // 测试指标百分位数聚合
  
  let metric_values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let expected_p50 = 5  // 中位数
  let expected_p90 = 9  // 90百分位数
  
  // 计算中位数（简化版本）
  let middle_index = metric_values.length() / 2
  let p50 = metric_values[middle_index]
  
  // 计算90百分位数（简化版本）
  let p90_index = (metric_values.length() * 9) / 10
  let p90 = metric_values[p90_index]
  
  // 验证百分位数计算
  assert_eq(p50, expected_p50)
  assert_eq(p90, expected_p90)
  assert_eq(p90 > p50, true)
}

test "metrics_histogram_aggregation" {
  // 测试指标直方图聚合
  
  let metric_values = [1, 2, 2, 3, 3, 3, 4, 4, 5]
  let buckets = [0, 2, 4, 6]
  
  // 创建直方图计数
  let histogram = [0, 0, 0]  // 三个桶: (0-2], (2-4], (4-6]
  
  let mut i = 0
  while i < metric_values.length() {
    let value = metric_values[i]
    if value > 0 && value <= 2 {
      histogram[0] = histogram[0] + 1
    } else if value > 2 && value <= 4 {
      histogram[1] = histogram[1] + 1
    } else if value > 4 && value <= 6 {
      histogram[2] = histogram[2] + 1
    }
    i = i + 1
  }
  
  // 验证直方图结果
  assert_eq(histogram[0], 3)  // 值1, 2, 2
  assert_eq(histogram[1], 4)  // 值3, 3, 3, 4, 4
  assert_eq(histogram[2], 2)  // 值5
  assert_eq(histogram[0] + histogram[1] + histogram[2], metric_values.length())
}

test "metrics_time_window_aggregation" {
  // 测试指标时间窗口聚合
  
  let time_series = [
    (1000, 10),  // (timestamp, value)
    (1500, 15),
    (2000, 12),
    (2500, 18),
    (3000, 20),
    (3500, 14)
  ]
  let window_start = 1500
  let window_end = 3000
  
  // 计算时间窗口内的平均值
  let mut window_sum = 0
  let mut window_count = 0
  
  let mut i = 0
  while i < time_series.length() {
    let (timestamp, value) = time_series[i]
    if timestamp >= window_start && timestamp <= window_end {
      window_sum = window_sum + value
      window_count = window_count + 1
    }
    i = i + 1
  }
  
  let window_average = window_sum / window_count
  
  // 验证时间窗口聚合
  assert_eq(window_count, 4)  // 1500, 2000, 2500, 3000
  assert_eq(window_sum, 65)   // 15 + 12 + 18 + 20
  assert_eq(window_average, 16)  // 65 / 4
}