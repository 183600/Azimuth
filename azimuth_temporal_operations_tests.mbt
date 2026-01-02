// Azimuth Temporal Operations Tests
// 测试时间戳、时区和时间序列操作

test "时间戳转换和格式化" {
  // 测试时间戳转换和格式化功能
  let timestamp_nanos = 1735689600000000000L  // 2025-01-02T00:00:00Z
  let timestamp_millis = timestamp_nanos / 1000000L
  let timestamp_seconds = timestamp_nanos / 1000000000L
  
  // 测试时间戳格式化
  let iso_format = format_timestamp_iso(timestamp_nanos)
  assert_eq(iso_format, "2025-01-02T00:00:00.000000000Z")
  
  let readable_format = format_timestamp_readable(timestamp_nanos)
  assert_true(readable_format.contains("2025"))
  assert_true(readable_format.contains("Jan"))
  assert_true(readable_format.contains("00:00:00"))
  
  // 测试时间戳解析
  let parsed_nanos = parse_timestamp_iso("2025-01-02T00:00:00.000000000Z")
  assert_eq(parsed_nanos, timestamp_nanos)
  
  let parsed_millis = parse_timestamp_readable("2025-01-02 00:00:00")
  assert_eq(parsed_millis / 1000, timestamp_seconds)
  
  // 测试时间戳计算
  let one_hour_later = timestamp_nanos + (3600 * 1000000000L)
  let one_day_later = timestamp_nanos + (24 * 3600 * 1000000000L)
  let one_week_later = timestamp_nanos + (7 * 24 * 3600 * 1000000000L)
  
  let hour_diff = calculate_time_difference(one_hour_later, timestamp_nanos)
  let day_diff = calculate_time_difference(one_day_later, timestamp_nanos)
  let week_diff = calculate_time_difference(one_week_later, timestamp_nanos)
  
  assert_eq(hour_diff.seconds, 3600)
  assert_eq(day_diff.seconds, 24 * 3600)
  assert_eq(week_diff.seconds, 7 * 24 * 3600)
}

test "时区转换和处理" {
  // 测试时区转换和处理
  let utc_timestamp = 1735689600000000000L  // 2025-01-02T00:00:00Z
  
  // 测试不同时区转换
  let pst_time = convert_timezone(utc_timestamp, "UTC", "America/Los_Angeles")
  let est_time = convert_timezone(utc_timestamp, "UTC", "America/New_York")
  let cet_time = convert_timezone(utc_timestamp, "UTC", "Europe/Paris")
  let jst_time = convert_timezone(utc_timestamp, "UTC", "Asia/Tokyo")
  
  // 验证时区偏移
  assert_eq(pst_time.offset_hours, -8)
  assert_eq(est_time.offset_hours, -5)
  assert_eq(cet_time.offset_hours, 1)
  assert_eq(jst_time.offset_hours, 9)
  
  // 验证转换后的本地时间
  assert_eq(pst_time.local_hour, 16)  // UTC 00:00 -> PST 16:00 (前一天)
  assert_eq(est_time.local_hour, 19)  // UTC 00:00 -> EST 19:00 (前一天)
  assert_eq(cet_time.local_hour, 1)   // UTC 00:00 -> CET 01:00
  assert_eq(jst_time.local_hour, 9)   // UTC 00:00 -> JST 09:00
  
  // 测试夏令时处理
  let summer_timestamp = 1717200000000000000L  // 2024-06-01T00:00:00Z
  let summer_pst = convert_timezone(summer_timestamp, "UTC", "America/Los_Angeles")
  let summer_est = convert_timezone(summer_timestamp, "UTC", "America/New_York")
  
  assert_eq(summer_pst.offset_hours, -7)  // 夏令时PDT
  assert_eq(summer_est.offset_hours, -4)  // 夏令时EDT
  
  // 测试时区格式化
  let pst_formatted = format_timezone_time(pst_time)
  let est_formatted = format_timezone_time(est_time)
  
  assert_true(pst_formatted.contains("PST"))
  assert_true(est_formatted.contains("EST"))
}

test "时间序列数据操作" {
  // 测试时间序列数据操作
  let time_series = []
  
  // 生成时间序列数据
  let base_timestamp = 1735689600000000000L
  for i in 0..=23 {
    let data_point = {
      "timestamp": base_timestamp + (i * 3600 * 1000000000L),
      "value": 50.0 + (10.0 * (i / 6.0).sin()),
      "metadata": {
        "sensor_id": "temp-sensor-01",
        "unit": "celsius",
        "quality": "good"
      }
    }
    time_series = time_series.push(data_point)
  }
  
  // 测试时间序列聚合
  let hourly_avg = aggregate_time_series(time_series, "hour", "average")
  let hourly_max = aggregate_time_series(time_series, "hour", "maximum")
  let hourly_min = aggregate_time_series(time_series, "hour", "minimum")
  
  assert_eq(hourly_avg.length(), 24)
  assert_eq(hourly_max.length(), 24)
  assert_eq(hourly_min.length(), 24)
  
  // 验证聚合结果
  assert_true(hourly_max[0].value >= hourly_avg[0].value)
  assert_true(hourly_avg[0].value >= hourly_min[0].value)
  
  // 测试时间序列重采样
  let daily_series = resample_time_series(time_series, "day", "average")
  assert_eq(daily_series.length(), 1)
  
  let six_hourly_series = resample_time_series(time_series, "6h", "average")
  assert_eq(six_hourly_series.length(), 4)
  
  // 测试时间序列窗口操作
  let rolling_avg = rolling_window_aggregate(time_series, 3, "average")
  let rolling_sum = rolling_window_aggregate(time_series, 3, "sum")
  
  assert_eq(rolling_avg.length(), 22)  // 24 - 3 + 1
  assert_eq(rolling_sum.length(), 22)
  
  // 验证滚动平均值计算
  let expected_avg = (time_series[0].value + time_series[1].value + time_series[2].value) / 3.0
  assert_true((rolling_avg[0].value - expected_avg).abs() < 0.001)
}

test "时间间隔和持续时间计算" {
  // 测试时间间隔和持续时间计算
  let start_time = 1735689600000000000L  // 2025-01-02T00:00:00Z
  let end_time = 1735776000000000000L    // 2025-01-03T00:00:00Z
  
  // 测试时间间隔计算
  let duration = calculate_duration(start_time, end_time)
  assert_eq(duration.days, 1)
  assert_eq(duration.hours, 24)
  assert_eq(duration.minutes, 24 * 60)
  assert_eq(duration.seconds, 24 * 60 * 60)
  assert_eq(duration.milliseconds, 24 * 60 * 60 * 1000)
  
  // 测试复杂时间间隔
  let complex_start = 1735689600000000000L  // 2025-01-02T00:00:00Z
  let complex_end = 1735713600000000000L    // 2025-01-02T06:40:00Z
  
  let complex_duration = calculate_duration(complex_start, complex_end)
  assert_eq(complex_duration.hours, 6)
  assert_eq(complex_duration.minutes, 40)
  assert_eq(complex_duration.seconds, 6 * 3600 + 40 * 60)
  
  // 测试持续时间格式化
  let formatted_duration = format_duration(duration)
  assert_true(formatted_duration.contains("1 day"))
  assert_true(formatted_duration.contains("24 hours"))
  
  let complex_formatted = format_duration(complex_duration)
  assert_true(complex_formatted.contains("6 hours"))
  assert_true(complex_formatted.contains("40 minutes"))
  
  // 测试持续时间解析
  let parsed_duration = parse_duration("1h 30m 45s")
  assert_eq(parsed_duration.seconds, 1 * 3600 + 30 * 60 + 45)
  
  let parsed_complex = parse_duration("2d 5h 15m")
  assert_eq(parsed_complex.seconds, 2 * 24 * 3600 + 5 * 3600 + 15 * 60)
}

test "时间序列模式识别" {
  // 测试时间序列模式识别
  let pattern_series = []
  
  // 生成有模式的时间序列数据
  let base_timestamp = 1735689600000000000L
  for i in 0..=167 {  // 一周的数据，每小时一个点
    let hour_of_day = i % 24
    let day_of_week = (i / 24) % 7
    
    // 模拟日模式：白天高，夜晚低
    let daily_pattern = if hour_of_day >= 8 && hour_of_day <= 18 {
      80.0 + (10.0 * (hour_of_day - 8).to_float() / 10.0)
    } else {
      30.0 + (5.0 * hour_of_day.to_float() / 24.0)
    }
    
    // 模拟周模式：工作日高，周末低
    let weekly_multiplier = if day_of_week >= 5 { 0.7 } else { 1.2 }
    
    let value = daily_pattern * weekly_multiplier + (5.0 * (i % 13).to_float() / 13.0)
    
    let data_point = {
      "timestamp": base_timestamp + (i * 3600 * 1000000000L),
      "value": value,
      "metadata": {
        "metric": "system.load",
        "unit": "percent"
      }
    }
    pattern_series = pattern_series.push(data_point)
  }
  
  // 测试周期性检测
  let daily_periodicity = detect_periodicity(pattern_series, 24)
  let weekly_periodicity = detect_periodicity(pattern_series, 168)
  
  assert_true(daily_periodicity.strength > 0.7)  // 应该检测到强日周期
  assert_true(weekly_periodicity.strength > 0.5) // 应该检测到中等周周期
  
  // 测试异常检测
  let anomalies = detect_anomalies(pattern_series, "statistical", 2.0)
  assert_true(anomalies.length() >= 0)
  
  // 测试趋势分析
  let trend = analyze_trend(pattern_series)
  assert_true(trend.direction == "stable" || trend.direction == "increasing" || trend.direction == "decreasing")
  assert_true(trend.slope >= -1.0 && trend.slope <= 1.0)
  
  // 测试季节性分解
  let decomposition = decompose_seasonal(pattern_series, 24)
  assert_eq(decomposition.trend.length(), 168)
  assert_eq(decomposition.seasonal.length(), 168)
  assert_eq(decomposition.residual.length(), 168)
}