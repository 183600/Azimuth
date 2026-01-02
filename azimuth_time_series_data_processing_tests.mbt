// Azimuth Time Series Data Processing Tests
// 时间序列数据处理测试用例

test "time series data aggregation operations" {
  // 测试时间序列数据聚合操作
  let time_series_data = @azimuth.TimeSeriesData {
    metric_name : "http.request.duration",
    metric_type : @azimuth.MetricType::Histogram,
    unit : "milliseconds",
    data_points : [
      @azimuth.DataPoint {
        timestamp : 1640995200000L, // 2022-01-01 00:00:00
        value : @azimuth.FloatValue(120.5),
        attributes : [("endpoint", @azimuth.StringValue("/api/users"))]
      },
      @azimuth.DataPoint {
        timestamp : 164099520060000L, // 2022-01-01 00:01:00
        value : @azimuth.FloatValue(95.3),
        attributes : [("endpoint", @azimuth.StringValue("/api/users"))]
      },
      @azimuth.DataPoint {
        timestamp : 164099520120000L, // 2022-01-01 00:02:00
        value : @azimuth.FloatValue(150.7),
        attributes : [("endpoint", @azimuth.StringValue("/api/users"))]
      },
      @azimuth.DataPoint {
        timestamp : 164099520180000L, // 2022-01-01 00:03:00
        value : @azimuth.FloatValue(85.2),
        attributes : [("endpoint", @azimuth.StringValue("/api/users"))]
      },
      @azimuth.DataPoint {
        timestamp : 164099520240000L, // 2022-01-01 00:04:00
        value : @azimuth.FloatValue(110.8),
        attributes : [("endpoint", @azimuth.StringValue("/api/users"))]
      }
    ]
  }
  
  // 计算平均值
  let avg_value = @azimuth.calculate_average(time_series_data)
  assert_true(abs(avg_value - 112.5) < 0.1)
  
  // 计算最大值
  let max_value = @azimuth.calculate_max(time_series_data)
  assert_true(abs(max_value - 150.7) < 0.1)
  
  // 计算最小值
  let min_value = @azimuth.calculate_min(time_series_data)
  assert_true(abs(min_value - 85.2) < 0.1)
  
  // 计算总和
  let sum_value = @azimuth.calculate_sum(time_series_data)
  assert_true(abs(sum_value - 562.5) < 0.1)
  
  // 计算百分位数
  let p50_value = @azimuth.calculate_percentile(time_series_data, 50.0)
  assert_true(abs(p50_value - 110.8) < 0.1)
  
  let p95_value = @azimuth.calculate_percentile(time_series_data, 95.0)
  assert_true(abs(p95_value - 150.7) < 0.1)
  
  let p99_value = @azimuth.calculate_percentile(time_series_data, 99.0)
  assert_true(abs(p99_value - 150.7) < 0.1)
}

test "time series data resampling and interpolation" {
  // 测试时间序列数据重采样和插值
  let high_frequency_data = @azimuth.TimeSeriesData {
    metric_name : "cpu.usage",
    metric_type : @azimuth.MetricType::Gauge,
    unit : "percent",
    data_points : [
      @azimuth.DataPoint {
        timestamp : 1640995200000L, // 00:00:00
        value : @azimuth.FloatValue(25.5),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520300000L, // 00:00:30
        value : @azimuth.FloatValue(35.2),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520600000L, // 00:01:00
        value : @azimuth.FloatValue(45.8),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520900000L, // 00:01:30
        value : @azimuth.FloatValue(40.1),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099521200000L, // 00:02:00
        value : @azimuth.FloatValue(30.3),
        attributes : []
      }
    ]
  }
  
  // 下采样到1分钟间隔
  let downsampled_data = @azimuth.downsample_time_series(
    high_frequency_data, 
    @azimuth.ResamplingInterval::Minute, 
    @azimuth.AggregationMethod::Average
  )
  
  // 验证下采样结果
  assert_eq(downsampled_data.data_points.length(), 3) // 3分钟的数据
  
  // 验证第一个数据点 (00:00:00-00:00:59的平均值)
  let first_point = downsampled_data.data_points[0]
  assert_eq(first_point.timestamp, 1640995200000L)
  match first_point.value {
    @azimuth.FloatValue(v) => assert_true(abs(v - 30.35) < 0.1) // (25.5 + 35.2) / 2
    _ => assert_true(false)
  }
  
  // 验证第二个数据点 (00:01:00-00:01:59的平均值)
  let second_point = downsampled_data.data_points[1]
  assert_eq(second_point.timestamp, 164099520600000L)
  match second_point.value {
    @azimuth.FloatValue(v) => assert_true(abs(v - 42.95) < 0.1) // (45.8 + 40.1) / 2
    _ => assert_true(false)
  }
  
  // 验证第三个数据点 (00:02:00-00:02:59)
  let third_point = downsampled_data.data_points[2]
  assert_eq(third_point.timestamp, 164099521200000L)
  match third_point.value {
    @azimuth.FloatValue(v) => assert_eq(v, 30.3)
    _ => assert_true(false)
  }
  
  // 上采样到15秒间隔并使用线性插值
  let upsampled_data = @azimuth.upsample_time_series(
    high_frequency_data,
    @azimuth.ResamplingInterval::Second(15),
    @azimuth.InterpolationMethod::Linear
  )
  
  // 验证上采样结果
  assert_true(upsampled_data.data_points.length() > high_frequency_data.data_points.length())
  
  // 验证原始数据点保持不变
  let original_timestamps = high_frequency_data.data_points.map(fn(dp) { dp.timestamp })
  for timestamp in original_timestamps {
    let upsampled_point = upsampled_data.data_points.filter(fn(dp) { dp.timestamp == timestamp })
    assert_eq(upsampled_point.length(), 1)
  }
  
  // 验证插值点在合理范围内
  for point in upsampled_data.data_points {
    match point.value {
      @azimuth.FloatValue(v) => assert_true(v >= 25.0 && v <= 50.0)
      _ => assert_true(false)
    }
  }
}

test "time series data compression and encoding" {
  // 测试时间序列数据压缩和编码
  let large_time_series = @azimuth.TimeSeriesData {
    metric_name : "memory.heap.usage",
    metric_type : @azimuth.MetricType::Gauge,
    unit : "bytes",
    data_points : [
      @azimuth.DataPoint {
        timestamp : 1640995200000L,
        value : @azimuth.IntValue(1073741824), // 1GB
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520060000L,
        value : @azimuth.IntValue(1082130432), // 1.01GB
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520120000L,
        value : @azimuth.IntValue(1090519040), // 1.02GB
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520180000L,
        value : @azimuth.IntValue(1077936128), // 1.003GB
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520240000L,
        value : @azimuth.IntValue(1065353216), // 0.99GB
        attributes : []
      }
    ]
  }
  
  // 使用增量编码压缩
  let delta_encoded = @azimuth.delta_encode_time_series(large_time_series)
  
  // 验证增量编码结果
  assert_eq(delta_encoded.data_points.length(), large_time_series.data_points.length())
  
  // 第一个数据点应保持不变
  match delta_encoded.data_points[0].value {
    @azimuth.IntValue(v) => assert_eq(v, 1073741824)
    _ => assert_true(false)
  }
  
  // 后续数据点应为增量值
  match delta_encoded.data_points[1].value {
    @azimuth.IntValue(v) => assert_eq(v, 8388608) // 1082130432 - 1073741824
    _ => assert_true(false)
  }
  
  match delta_encoded.data_points[2].value {
    @azimuth.IntValue(v) => assert_eq(v, 8388608) // 1090519040 - 1082130432
    _ => assert_true(false)
  }
  
  // 解码验证
  let decoded_data = @azimuth.delta_decode_time_series(delta_encoded)
  
  // 验证解码结果与原始数据一致
  assert_eq(decoded_data.data_points.length(), large_time_series.data_points.length())
  for i in 0..large_time_series.data_points.length() {
    assert_eq(decoded_data.data_points[i].timestamp, large_time_series.data_points[i].timestamp)
    match decoded_data.data_points[i].value {
      @azimuth.IntValue(decoded_val) => {
        match large_time_series.data_points[i].value {
          @azimuth.IntValue(original_val) => assert_eq(decoded_val, original_val)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 使用XOR编码压缩
  let xor_encoded = @azimuth.xor_encode_time_series(large_time_series)
  
  // 验证XOR编码结果
  assert_eq(xor_encoded.data_points.length(), large_time_series.data_points.length())
  
  // 解码验证
  let xor_decoded = @azimuth.xor_decode_time_series(xor_encoded)
  
  // 验证解码结果与原始数据一致
  assert_eq(xor_decoded.data_points.length(), large_time_series.data_points.length())
  for i in 0..large_time_series.data_points.length() {
    assert_eq(xor_decoded.data_points[i].timestamp, large_time_series.data_points[i].timestamp)
    match xor_decoded.data_points[i].value {
      @azimuth.IntValue(decoded_val) => {
        match large_time_series.data_points[i].value {
          @azimuth.IntValue(original_val) => assert_eq(decoded_val, original_val)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
}

test "time series data windowing and tumbling operations" {
  // 测试时间序列数据窗口和滚动操作
  let streaming_data = @azimuth.TimeSeriesData {
    metric_name : "request.count",
    metric_type : @azimuth.MetricType::Counter,
    unit : "requests",
    data_points : [
      @azimuth.DataPoint {
        timestamp : 1640995200000L, // 00:00:00
        value : @azimuth.IntValue(10),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520300000L, // 00:00:30
        value : @azimuth.IntValue(15),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520600000L, // 00:01:00
        value : @azimuth.IntValue(12),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520900000L, // 00:01:30
        value : @azimuth.IntValue(18),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099521200000L, // 00:02:00
        value : @azimuth.IntValue(25),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099521500000L, // 00:02:30
        value : @azimuth.IntValue(20),
        attributes : []
      }
    ]
  }
  
  // 滚动窗口操作 (1分钟窗口)
  let tumbling_windows = @azimuth.create_tumbling_windows(
    streaming_data,
    @azimuth.WindowDuration::Minute(1),
    @azimuth.AggregationMethod::Sum
  )
  
  // 验证滚动窗口结果
  assert_eq(tumbling_windows.length(), 3) // 3个1分钟窗口
  
  // 验证第一个窗口 (00:00:00-00:00:59)
  let first_window = tumbling_windows[0]
  assert_eq(first_window.start_time, 1640995200000L)
  assert_eq(first_window.end_time, 164099520059999L)
  match first_window.aggregated_value {
    @azimuth.IntValue(v) => assert_eq(v, 25) // 10 + 15
    _ => assert_true(false)
  }
  
  // 验证第二个窗口 (00:01:00-00:01:59)
  let second_window = tumbling_windows[1]
  assert_eq(second_window.start_time, 164099520600000L)
  assert_eq(second_window.end_time, 164099520659999L)
  match second_window.aggregated_value {
    @azimuth.IntValue(v) => assert_eq(v, 30) // 12 + 18
    _ => assert_true(false)
  }
  
  // 验证第三个窗口 (00:02:00-00:02:59)
  let third_window = tumbling_windows[2]
  assert_eq(third_window.start_time, 164099521200000L)
  assert_eq(third_window.end_time, 164099521259999L)
  match third_window.aggregated_value {
    @azimuth.IntValue(v) => assert_eq(v, 45) // 25 + 20
    _ => assert_true(false)
  }
  
  // 滑动窗口操作 (1分钟窗口，30秒滑动)
  let sliding_windows = @azimuth.create_sliding_windows(
    streaming_data,
    @azimuth.WindowDuration::Minute(1),
    @azimuth.SlideDuration::Second(30),
    @azimuth.AggregationMethod::Average
  )
  
  // 验证滑动窗口结果
  assert_true(sliding_windows.length() > tumbling_windows.length())
  
  // 验证窗口重叠
  for i in 1..sliding_windows.length() {
    let current_window = sliding_windows[i]
    let previous_window = sliding_windows[i-1]
    
    // 验证窗口时间顺序
    assert_true(current_window.start_time > previous_window.start_time)
    assert_true(current_window.end_time > previous_window.end_time)
    
    // 验证滑动间隔
    assert_eq(current_window.start_time - previous_window.start_time, 30000L) // 30秒
  }
  
  // 会话窗口操作 (基于超时的会话)
  let session_windows = @azimuth.create_session_windows(
    streaming_data,
    @azimuth.SessionTimeout::Second(45), // 45秒超时
    @azimuth.AggregationMethod::Sum
  )
  
  // 验证会话窗口结果
  assert_eq(session_windows.length(), 2) // 2个会话窗口
  
  // 验证第一个会话窗口 (00:00:00-00:01:30)
  let first_session = session_windows[0]
  assert_eq(first_session.start_time, 1640995200000L)
  assert_eq(first_session.end_time, 164099520900000L)
  match first_session.aggregated_value {
    @azimuth.IntValue(v) => assert_eq(v, 55) // 10 + 15 + 12 + 18
    _ => assert_true(false)
  }
  
  // 验证第二个会话窗口 (00:02:00-00:02:30)
  let second_session = session_windows[1]
  assert_eq(second_session.start_time, 164099521200000L)
  assert_eq(second_session.end_time, 164099521500000L)
  match second_session.aggregated_value {
    @azimuth.IntValue(v) => assert_eq(v, 45) // 25 + 20
    _ => assert_true(false)
  }
}

test "time series data anomaly detection" {
  // 测试时间序列数据异常检测
  let data_with_anomalies = @azimuth.TimeSeriesData {
    metric_name : "response.time",
    metric_type : @azimuth.MetricType::Gauge,
    unit : "milliseconds",
    data_points : [
      @azimuth.DataPoint {
        timestamp : 1640995200000L,
        value : @azimuth.FloatValue(120.5),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520060000L,
        value : @azimuth.FloatValue(125.3),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520120000L,
        value : @azimuth.FloatValue(118.7),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520180000L,
        value : @azimuth.FloatValue(450.2), // 异常值
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520240000L,
        value : @azimuth.FloatValue(122.8),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520300000L,
        value : @azimuth.FloatValue(119.4),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520360000L,
        value : @azimuth.FloatValue(121.1),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520420000L,
        value : @azimuth.FloatValue(35.5), // 异常值
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520480000L,
        value : @azimuth.FloatValue(123.9),
        attributes : []
      },
      @azimuth.DataPoint {
        timestamp : 164099520540000L,
        value : @azimuth.FloatValue(120.2),
        attributes : []
      }
    ]
  }
  
  // 使用Z-score方法检测异常
  let zscore_threshold = 2.5
  let zscore_anomalies = @azimuth.detect_anomalies_zscore(
    data_with_anomalies,
    zscore_threshold
  )
  
  // 验证Z-score异常检测结果
  assert_eq(zscore_anomalies.length(), 2) // 应该检测到2个异常值
  
  // 验证第一个异常值 (450.2)
  let first_anomaly = zscore_anomalies[0]
  assert_eq(first_anomaly.data_point.timestamp, 164099520180000L)
  match first_anomaly.data_point.value {
    @azimuth.FloatValue(v) => assert_eq(v, 450.2)
    _ => assert_true(false)
  }
  assert_true(first_anomaly.z_score > zscore_threshold)
  
  // 验证第二个异常值 (35.5)
  let second_anomaly = zscore_anomalies[1]
  assert_eq(second_anomaly.data_point.timestamp, 164099520420000L)
  match second_anomaly.data_point.value {
    @azimuth.FloatValue(v) => assert_eq(v, 35.5)
    _ => assert_true(false)
  }
  assert_true(abs(second_anomaly.z_score) > zscore_threshold)
  
  // 使用IQR方法检测异常
  let iqr_anomalies = @azimuth.detect_anomalies_iqr(data_with_anomalies, 1.5)
  
  // 验证IQR异常检测结果
  assert_true(iqr_anomalies.length() >= 1) // 至少检测到1个异常值
  
  // 使用移动平均方法检测异常
  let moving_avg_anomalies = @azimuth.detect_anomalies_moving_average(
    data_with_anomalies,
    3, // 窗口大小
    2.0 // 阈值倍数
  )
  
  // 验证移动平均异常检测结果
  assert_true(moving_avg_anomalies.length() >= 1) // 至少检测到1个异常值
  
  // 验证异常检测结果一致性
  let anomaly_timestamps = zscore_anomalies.map(fn(a) { a.data_point.timestamp })
  for anomaly in moving_avg_anomalies {
    let timestamp = anomaly.data_point.timestamp
    assert_true(anomaly_timestamps.contains(timestamp))
  }
}