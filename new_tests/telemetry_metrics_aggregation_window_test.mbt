// 遥测指标聚合窗口测试
// 测试指标数据在不同时间窗口下的聚合功能

test "metrics_aggregation_time_window_basic" {
  // 基础时间窗口聚合测试
  let window_size_ms = 60000L  // 1分钟窗口
  let current_time = 1640995200000L  // 2022-01-01 00:00:00 UTC
  
  // 创建测试数据点
  let data_points = [
    (current_time - 30000L, 10.0),  // 窗口内
    (current_time - 10000L, 20.0),  // 窗口内
    (current_time + 10000L, 30.0),  // 窗口内
    (current_time - 70000L, 5.0),   // 窗口外
    (current_time + 70000L, 15.0)   // 窗口外
  ]
  
  // 过滤窗口内的数据点
  let window_start = current_time - window_size_ms / 2L
  let window_end = current_time + window_size_ms / 2L
  
  let in_window_data = data_points.filter(fn((timestamp, _)) {
    timestamp >= window_start && timestamp <= window_end
  })
  
  @assertion.assert_eq(in_window_data.length, 3)?
  @assertion.assert_eq(in_window_data[0].1, 10.0)?
  @assertion.assert_eq(in_window_data[1].1, 20.0)?
  @assertion.assert_eq(in_window_data[2].1, 30.0)?
}

test "metrics_aggregation_sliding_window" {
  // 滑动窗口聚合测试
  let window_size_ms = 30000L  // 30秒滑动窗口
  
  // 模拟时间序列数据
  let time_series = [
    (1000L, 5.0),
    (5000L, 10.0),
    (10000L, 15.0),
    (15000L, 20.0),
    (20000L, 25.0),
    (25000L, 30.0),
    (30000L, 35.0),
    (35000L, 40.0),
    (40000L, 45.0),
    (45000L, 50.0)
  ]
  
  // 计算滑动窗口平均值
  let calculate_sliding_average = fn(window_center : Int64) -> Double {
    let window_start = window_center - window_size_ms / 2L
    let window_end = window_center + window_size_ms / 2L
    
    let window_data = time_series.filter(fn((timestamp, _)) {
      timestamp >= window_start && timestamp <= window_end
    })
    
    if window_data.length == 0 {
      0.0
    } else {
      let sum = window_data.map(fn((_, value)) { value }).reduce(fn(acc, val) { acc + val }, 0.0)
      sum / @float.from_int(window_data.length)
    }
  }
  
  // 测试不同时间点的滑动平均值
  let avg_at_15s = calculate_sliding_average(15000L)
  let avg_at_30s = calculate_sliding_average(30000L)
  
  @assertion.assert_true(avg_at_15s > 0.0)?
  @assertion.assert_true(avg_at_30s > avg_at_15s)?  // 随着时间推移，平均值应该增加
}

test "metrics_aggregation_multiple_windows" {
  // 多窗口聚合测试
  let window_sizes = [10000L, 30000L, 60000L]  // 10秒、30秒、1分钟窗口
  let current_time = 1640995200000L
  
  // 生成测试数据
  let generate_test_data = fn() -> Array[(Int64, Double)] {
    let mut data = []
    for i = 0; i < 100; i = i + 1 {
      let timestamp = current_time - 60000L + @int.to_int64(i * 1000)
      let value = @float.from_int(i) * 1.5
      data = data.push((timestamp, value))
    }
    data
  }
  
  let test_data = generate_test_data()
  
  // 对每个窗口大小计算聚合统计
  let calculate_aggregation = fn(window_size : Int64) -> (Int, Double, Double, Double) {
    let window_start = current_time - window_size / 2L
    let window_end = current_time + window_size / 2L
    
    let window_data = test_data.filter(fn((timestamp, _)) {
      timestamp >= window_start && timestamp <= window_end
    })
    
    if window_data.length == 0 {
      (0, 0.0, 0.0, 0.0)
    } else {
      let values = window_data.map(fn((_, value)) { value })
      let count = values.length
      let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
      let avg = sum / @float.from_int(count)
      let max_val = values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, 0.0)
      (count, avg, max_val, sum)
    }
  }
  
  let results = window_sizes.map(calculate_aggregation)
  
  // 验证结果：更大的窗口应该包含更多数据点
  @assertion.assert_true(results[0].0 < results[1].0)?
  @assertion.assert_true(results[1].0 < results[2].0)?
}

test "metrics_aggregation_window_boundary" {
  // 窗口边界条件测试
  let window_size = 60000L
  let test_time = 1640995200000L
  
  // 边界时间点测试
  let boundary_points = [
    (test_time - window_size / 2L, true),      // 窗口开始边界
    (test_time - window_size / 2L - 1L, false), // 刚好在窗口外
    (test_time + window_size / 2L, true),      // 窗口结束边界
    (test_time + window_size / 2L + 1L, false)  // 刚好在窗口外
  ]
  
  let is_in_window = fn(timestamp : Int64) -> Bool {
    let window_start = test_time - window_size / 2L
    let window_end = test_time + window_size / 2L
    timestamp >= window_start && timestamp <= window_end
  }
  
  for (timestamp, expected) in boundary_points {
    let actual = is_in_window(timestamp)
    @assertion.assert_eq(actual, expected)?
  }
}

test "metrics_aggregation_empty_window" {
  // 空窗口处理测试
  let window_size = 30000L
  let current_time = 1640995200000L
  
  // 创建一个没有数据点的时间范围
  let empty_data : Array[(Int64, Double)] = []
  
  let window_start = current_time - window_size / 2L
  let window_end = current_time + window_size / 2L
  
  let window_data = empty_data.filter(fn((timestamp, _)) {
    timestamp >= window_start && timestamp <= window_end
  })
  
  @assertion.assert_eq(window_data.length, 0)?
  
  // 测试空窗口的聚合函数
  let safe_average = fn(data : Array[Double]) -> Double {
    if data.length == 0 {
      0.0
    } else {
      data.reduce(fn(acc, val) { acc + val }, 0.0) / @float.from_int(data.length)
    }
  }
  
  let values = window_data.map(fn((_, value)) { value })
  let avg = safe_average(values)
  
  @assertion.assert_eq(avg, 0.0)?
}