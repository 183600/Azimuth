// 遥测数据聚合测试用例

test "telemetry_time_window_aggregation" {
  // 测试遥测数据时间窗口聚合
  
  let window_size_seconds = 60        // 窗口大小（秒）
  let max_window_count = 1440         // 最大窗口数量（24小时）
  let aggregation_functions = ["sum", "avg", "min", "max", "count"]
  
  // 验证时间窗口配置
  assert_eq(window_size_seconds > 0, true)
  assert_eq(max_window_count > 0, true)
  assert_eq(aggregation_functions.length(), 5)
  
  // 遥测数据点
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Double,
    tags: Array[String]
  }
  
  // 聚合窗口
  type AggregationWindow = {
    window_start: Int,
    window_end: Int,
    data_points: Array[TelemetryPoint],
    aggregated_values: Array[Double]
  }
  
  // 创建测试数据点
  let mut data_points = []
  let base_timestamp = 1640995200  // 基准时间戳
  
  let mut i = 0
  while i < 300 {  // 创建5分钟的数据点（每秒一个）
    let point = TelemetryPoint {
      timestamp: base_timestamp + i,
      metric_name: "cpu_usage",
      value: 50.0 + (i % 20).to_double() * 2.5,  // 50-100之间的值
      tags: ["host:server1", "env:production"]
    }
    data_points.push(point)
    i = i + 1
  }
  
  // 验证数据点
  assert_eq(data_points.length(), 300)
  assert_eq(data_points[0].timestamp, base_timestamp)
  assert_eq(data_points[299].timestamp, base_timestamp + 299)
  
  // 时间窗口聚合
  let mut aggregation_windows = []
  let current_time = base_timestamp
  
  while current_time < base_timestamp + 300 {
    let window_start = current_time
    let window_end = current_time + window_size_seconds
    
    // 收集窗口内的数据点
    let mut window_data = []
    let mut j = 0
    while j < data_points.length() {
      let point = data_points[j]
      if point.timestamp >= window_start and point.timestamp < window_end {
        window_data.push(point)
      }
      j = j + 1
    }
    
    // 计算聚合值
    let mut aggregated_values = []
    
    if window_data.length() > 0 {
      // Sum
      let mut sum = 0.0
      let mut j = 0
      while j < window_data.length() {
        sum = sum + window_data[j].value
        j = j + 1
      }
      aggregated_values.push(sum)
      
      // Average
      let avg = sum / window_data.length().to_double()
      aggregated_values.push(avg)
      
      // Min
      let mut min = window_data[0].value
      j = 0
      while j < window_data.length() {
        if window_data[j].value < min {
          min = window_data[j].value
        }
        j = j + 1
      }
      aggregated_values.push(min)
      
      // Max
      let mut max = window_data[0].value
      j = 0
      while j < window_data.length() {
        if window_data[j].value > max {
          max = window_data[j].value
        }
        j = j + 1
      }
      aggregated_values.push(max)
      
      // Count
      aggregated_values.push(window_data.length().to_double())
    } else {
      // 空窗口的默认值
      let mut j = 0
      while j < aggregation_functions.length() {
        if aggregation_functions[j] == "count" {
          aggregated_values.push(0.0)
        } else {
          aggregated_values.push(0.0)
        }
        j = j + 1
      }
    }
    
    // 创建聚合窗口
    let window = AggregationWindow {
      window_start: window_start,
      window_end: window_end,
      data_points: window_data,
      aggregated_values: aggregated_values
    }
    
    aggregation_windows.push(window)
    current_time = current_time + window_size_seconds
  }
  
  // 验证聚合窗口
  assert_eq(aggregation_windows.length(), 5)  // 5分钟 / 1分钟窗口 = 5个窗口
  
  // 验证第一个窗口（0-60秒）
  let first_window = aggregation_windows[0]
  assert_eq(first_window.window_start, base_timestamp)
  assert_eq(first_window.window_end, base_timestamp + 60)
  assert_eq(first_window.data_points.length(), 60)
  assert_eq(first_window.aggregated_values.length(), 5)
  
  // 验证聚合值计算
  assert_eq(first_window.aggregated_values[4], 60.0)  // count应该是60
  
  // 验证平均值在合理范围内
  let avg_value = first_window.aggregated_values[1]
  assert_eq(avg_value >= 50.0, true)
  assert_eq(avg_value <= 100.0, true)
  
  // 验证最小值和最大值
  let min_value = first_window.aggregated_values[2]
  let max_value = first_window.aggregated_values[3]
  assert_eq(min_value <= avg_value, true)
  assert_eq(max_value >= avg_value, true)
}

test "telemetry_tag_based_aggregation" {
  // 测试基于标签的遥测数据聚合
  
  let tag_keys = ["host", "service", "region", "env"]
  let max_tag_combinations = 1000
  
  // 验证标签配置
  assert_eq(tag_keys.length(), 4)
  assert_eq(max_tag_combinations > 0, true)
  
  // 标签组合聚合结果
  type TagAggregation = {
    tag_combination: String,
    count: Int,
    sum_value: Double,
    avg_value: Double,
    min_value: Double,
    max_value: Double
  }
  
  // 创建带有不同标签的测试数据
  let telemetry_data = [
    TelemetryPoint {
      timestamp: 1640995200,
      metric_name: "response_time",
      value: 120.5,
      tags: ["host:server1", "service:api", "region:us-east", "env:prod"]
    },
    TelemetryPoint {
      timestamp: 1640995201,
      metric_name: "response_time",
      value: 95.3,
      tags: ["host:server1", "service:api", "region:us-east", "env:prod"]
    },
    TelemetryPoint {
      timestamp: 1640995202,
      metric_name: "response_time",
      value: 150.7,
      tags: ["host:server2", "service:web", "region:us-west", "env:prod"]
    },
    TelemetryPoint {
      timestamp: 1640995203,
      metric_name: "response_time",
      value: 85.2,
      tags: ["host:server2", "service:web", "region:us-west", "env:prod"]
    },
    TelemetryPoint {
      timestamp: 1640995204,
      metric_name: "response_time",
      value: 200.1,
      tags: ["host:server1", "service:db", "region:eu-central", "env:staging"]
    },
    TelemetryPoint {
      timestamp: 1640995205,
      metric_name: "response_time",
      value: 180.9,
      tags: ["host:server1", "service:db", "region:eu-central", "env:staging"]
    }
  ]
  
  // 验证测试数据
  assert_eq(telemetry_data.length(), 6)
  
  // 按标签组合聚合
  let mut tag_aggregations = {}
  
  let mut i = 0
  while i < telemetry_data.length() {
    let point = telemetry_data[i]
    
    // 创建标签组合键
    let mut tag_combination = ""
    let mut j = 0
    while j < point.tags.length() {
      if j > 0 {
        tag_combination = tag_combination + ","
      }
      tag_combination = tag_combination + point.tags[j]
      j = j + 1
    }
    
    // 初始化或更新聚合
    if tag_aggregations.contains_key(tag_combination) {
      let existing = tag_aggregations[tag_combination]
      let updated = TagAggregation {
        tag_combination: existing.tag_combination,
        count: existing.count + 1,
        sum_value: existing.sum_value + point.value,
        avg_value: 0.0,  // 稍后计算
        min_value: min(existing.min_value, point.value),
        max_value: max(existing.max_value, point.value)
      }
      tag_aggregations[tag_combination] = updated
    } else {
      let new_aggregation = TagAggregation {
        tag_combination: tag_combination,
        count: 1,
        sum_value: point.value,
        avg_value: point.value,
        min_value: point.value,
        max_value: point.value
      }
      tag_aggregations[tag_combination] = new_aggregation
    }
    
    i = i + 1
  }
  
  // 计算平均值
  let mut keys = []
  let mut iter = tag_aggregations.keys()
  while iter.has_next() {
    keys.push(iter.next())
  }
  
  let mut i = 0
  while i < keys.length() {
    let key = keys[i]
    let aggregation = tag_aggregations[key]
    let avg = aggregation.sum_value / aggregation.count.to_double()
    let updated = TagAggregation {
      tag_combination: aggregation.tag_combination,
      count: aggregation.count,
      sum_value: aggregation.sum_value,
      avg_value: avg,
      min_value: aggregation.min_value,
      max_value: aggregation.max_value
    }
    tag_aggregations[key] = updated
    i = i + 1
  }
  
  // 验证标签聚合结果
  assert_eq(keys.length(), 3)  // 应该有3个不同的标签组合
  
  // 验证每个聚合的完整性
  i = 0
  while i < keys.length() {
    let key = keys[i]
    let aggregation = tag_aggregations[key]
    
    assert_eq(aggregation.count > 0, true)
    assert_eq(aggregation.sum_value > 0.0, true)
    assert_eq(aggregation.avg_value > 0.0, true)
    assert_eq(aggregation.min_value <= aggregation.avg_value, true)
    assert_eq(aggregation.max_value >= aggregation.avg_value, true)
    assert_eq(aggregation.min_value <= aggregation.max_value, true)
    
    // 验证平均值计算正确
    let expected_avg = aggregation.sum_value / aggregation.count.to_double()
    assert_eq(abs(aggregation.avg_value - expected_avg) < 0.001, true)
    
    i = i + 1
  }
  
  // 验证特定标签组合
  let api_prod_key = "host:server1,service:api,region:us-east,env:prod"
  if tag_aggregations.contains_key(api_prod_key) {
    let api_aggregation = tag_aggregations[api_prod_key]
    assert_eq(api_aggregation.count, 2)
    assert_eq(api_aggregation.sum_value, 120.5 + 95.3)
    assert_eq(api_aggregation.min_value, 95.3)
    assert_eq(api_aggregation.max_value, 120.5)
  }
}

test "telemetry_percentile_aggregation" {
  // 测试遥测数据百分位数聚合
  
  let percentiles = [50.0, 90.0, 95.0, 99.0, 99.9]
  let sample_size = 1000
  
  // 验证百分位数配置
  assert_eq(percentiles.length(), 5)
  assert_eq(sample_size > 0, true)
  
  // 百分位数聚合结果
  type PercentileResult = {
    percentile: Double,
    value: Double,
    sample_count: Int
  }
  
  // 创建测试数据（模拟响应时间分布）
  let mut response_times = []
  let mut i = 0
  while i < sample_size {
    // 生成偏向较小值的响应时间（模拟真实分布）
    let base_value = 50.0
    let random_factor = (i % 100).to_double()
    let response_time = base_value + random_factor * 2.0 + 
                       if i % 10 == 0 { random_factor * 10.0 } else { 0.0 }
    response_times.push(response_time)
    i = i + 1
  }
  
  // 验证测试数据
  assert_eq(response_times.length(), sample_size)
  
  // 排序数据（简化排序算法）
  let mut sorted_times = response_times
  let mut i = 0
  while i < sorted_times.length() {
    let mut j = i + 1
    while j < sorted_times.length() {
      if sorted_times[i] > sorted_times[j] {
        let temp = sorted_times[i]
        sorted_times[i] = sorted_times[j]
        sorted_times[j] = temp
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证排序
  assert_eq(sorted_times[0] <= sorted_times[sorted_times.length() - 1], true)
  
  // 计算百分位数
  let mut percentile_results = []
  
  let mut i = 0
  while i < percentiles.length() {
    let percentile = percentiles[i]
    let index = ((percentile / 100.0) * (sample_size - 1).to_double()).to_int()
    let percentile_value = sorted_times[index]
    
    let result = PercentileResult {
      percentile: percentile,
      value: percentile_value,
      sample_count: sample_size
    }
    
    percentile_results.push(result)
    i = i + 1
  }
  
  // 验证百分位数结果
  assert_eq(percentile_results.length(), percentiles.length())
  
  // 验证百分位数顺序
  let mut i = 0
  while i < percentile_results.length() - 1 {
    assert_eq(percentile_results[i].percentile < percentile_results[i + 1].percentile, true)
    assert_eq(percentile_results[i].value <= percentile_results[i + 1].value, true)
    i = i + 1
  }
  
  // 验证特定百分位数
  let mut p50_found = false
  let mut p90_found = false
  let mut p99_found = false
  
  i = 0
  while i < percentile_results.length() {
    let result = percentile_results[i]
    
    if result.percentile == 50.0 {
      p50_found = true
      // P50应该在数据的中位数附近
      let median_index = (sample_size - 1) / 2
      assert_eq(abs(result.value - sorted_times[median_index]) < 1.0, true)
    }
    
    if result.percentile == 90.0 {
      p90_found = true
      // P90应该比P50大
      let p50_value = percentile_results[0].value  // 假设第一个是P50
      assert_eq(result.value > p50_value, true)
    }
    
    if result.percentile == 99.0 {
      p99_found = true
      // P99应该接近最大值
      let max_value = sorted_times[sorted_times.length() - 1]
      assert_eq(result.value <= max_value, true)
      assert_eq(result.value > sorted_times[sorted_times.length() - 20], true)
    }
    
    i = i + 1
  }
  
  assert_eq(p50_found, true)
  assert_eq(p90_found, true)
  assert_eq(p99_found, true)
  
  // 验证所有结果的样本计数一致
  i = 0
  while i < percentile_results.length() {
    assert_eq(percentile_results[i].sample_count, sample_size)
    i = i + 1
  }
  
  // 测试滑动窗口百分位数计算
  let window_size = 100
  let mut sliding_percentiles = []
  
  let mut i = 0
  while i <= sample_size - window_size {
    // 提取窗口数据
    let mut window_data = []
    let mut j = 0
    while j < window_size {
      window_data.push(sorted_times[i + j])
      j = j + 1
    }
    
    // 计算窗口P95
    let p95_index = ((95.0 / 100.0) * (window_size - 1).to_double()).to_int()
    let window_p95 = window_data[p95_index]
    
    sliding_percentiles.push(window_p95)
    i = i + 1
  }
  
  // 验证滑动窗口百分位数
  assert_eq(sliding_percentiles.length(), sample_size - window_size + 1)
  
  // 验证滑动百分位数的变化趋势
  let mut increasing_count = 0
  let mut decreasing_count = 0
  
  let mut i = 0
  while i < sliding_percentiles.length() - 1 {
    if sliding_percentiles[i + 1] > sliding_percentiles[i] {
      increasing_count = increasing_count + 1
    } else if sliding_percentiles[i + 1] < sliding_percentiles[i] {
      decreasing_count = decreasing_count + 1
    }
    i = i + 1
  }
  
  // 由于数据是排序的，滑动窗口的P95应该总体呈上升趋势
  assert_eq(increasing_count > decreasing_count, true)
}