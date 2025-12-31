// 高级指标聚合测试
// 测试复杂指标数据的聚合计算和统计功能

test "multi_dimensional_metrics_aggregation" {
  // 测试多维指标聚合
  
  // 创建多维指标数据
  let raw_metrics = [
    MetricPoint{
      name: "http_request_duration",
      value: 120.5,
      timestamp: 1609459200L,
      labels: [
        ("service", "api-gateway"),
        ("method", "GET"),
        ("status", "200"),
        ("region", "us-east-1")
      ]
    },
    MetricPoint{
      name: "http_request_duration", 
      value: 250.3,
      timestamp: 1609459205L,
      labels: [
        ("service", "api-gateway"),
        ("method", "POST"),
        ("status", "201"), 
        ("region", "us-east-1")
      ]
    },
    MetricPoint{
      name: "http_request_duration",
      value: 95.7,
      timestamp: 1609459210L,
      labels: [
        ("service", "user-service"),
        ("method", "GET"),
        ("status", "200"),
        ("region", "us-west-2")
      ]
    },
    MetricPoint{
      name: "http_request_duration",
      value: 180.2,
      timestamp: 1609459215L,
      labels: [
        ("service", "user-service"),
        ("method", "GET"),
        ("status", "500"),
        ("region", "us-west-2") 
      ]
    }
  ]
  
  // 按服务聚合
  let service_aggregation = aggregate_by_dimension(raw_metrics, "service")
  assert_eq(service_aggregation.length(), 2)
  
  let api_gateway_stats = service_aggregation.find(|agg| agg.dimension == "api-gateway")
  match api_gateway_stats {
    Some(stats) => {
      assert_eq(stats.count, 2)
      assert_eq(stats.sum, 370.8)
      assert_eq(stats.avg, 185.4)
      assert_eq(stats.min, 120.5)
      assert_eq(stats.max, 250.3)
    }
    None => @test.fail("API Gateway aggregation not found")
  }
  
  // 按HTTP状态码聚合
  let status_aggregation = aggregate_by_dimension(raw_metrics, "status")
  assert_eq(status_aggregation.length(), 3)
  
  let success_stats = status_aggregation.find(|agg| agg.dimension == "200")
  match success_stats {
    Some(stats) => {
      assert_eq(stats.count, 2)
      assert_eq(stats.avg, 108.1) // (120.5 + 95.7) / 2
    }
    None => @test.fail("Success status aggregation not found")
  }
}

test "time_window_metrics_aggregation" {
  // 测试时间窗口指标聚合
  
  // 创建时间序列指标数据
  let time_series_metrics = []
  let base_time = 1609459200L // 2021-01-01 00:00:00
  
  for i = 0; i < 1440; i = i + 1 { // 24小时，每分钟一个数据点
    time_series_metrics.push(MetricPoint{
      name: "cpu_usage",
      value: 30.0 + (i % 60).to_float() * 0.5 + @random.float(20.0), // 模拟CPU使用率变化
      timestamp: base_time + i.to_int64() * 60L,
      labels: [
        ("host", "server-01"),
        ("datacenter", "dc-east")
      ]
    })
  }
  
  // 按小时聚合
  let hourly_aggregation = aggregate_by_time_window(time_series_metrics, 3600L) // 1小时窗口
  assert_eq(hourly_aggregation.length(), 24)
  
  // 验证第一个小时的聚合结果
  let first_hour = hourly_aggregation[0]
  assert_eq(first_hour.start_time, base_time)
  assert_eq(first_hour.end_time, base_time + 3600L)
  assert_eq(first_hour.count, 60)
  assert_eq(first_hour.avg > 20.0 && first_hour.avg < 80.0, true)
  
  // 按天聚合
  let daily_aggregation = aggregate_by_time_window(time_series_metrics, 86400L) // 24小时窗口
  assert_eq(daily_aggregation.length(), 1)
  
  let daily_stats = daily_aggregation[0]
  assert_eq(daily_stats.count, 1440)
  assert_eq(daily_stats.start_time, base_time)
  assert_eq(daily_stats.end_time, base_time + 86400L)
}

test "percentile_calculation" {
  // 测试百分位数计算
  
  // 创建有序测试数据
  let test_values = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  
  // 计算各种百分位数
  let p50 = calculate_percentile(test_values, 0.5)
  let p90 = calculate_percentile(test_values, 0.9) 
  let p95 = calculate_percentile(test_values, 0.95)
  let p99 = calculate_percentile(test_values, 0.99)
  
  assert_eq(p50, 55.0) // 中位数
  assert_eq(p90, 91.0)
  assert_eq(p95, 95.5)
  assert_eq(p99, 99.1)
  
  // 测试大量随机数据
  let large_dataset = []
  for i = 0; i < 10000; i = i + 1 {
    large_dataset.push(@random.float(1000.0))
  }
  
  let large_p50 = calculate_percentile(large_dataset, 0.5)
  let large_p95 = calculate_percentile(large_dataset, 0.95)
  let large_p99 = calculate_percentile(large_dataset, 0.99)
  
  // 验证百分位数的合理性
  assert_eq(large_p50 >= 400.0 && large_p50 <= 600.0, true) // 中位数应该在中间范围
  assert_eq(large_p95 >= 900.0, true) // 95%应该在较高范围
  assert_eq(large_p99 >= 950.0, true) // 99%应该更高
}

test "metrics_rate_calculation" {
  // 测试指标变化率计算
  
  // 创建计数器类型指标
  let counter_metrics = [
    MetricPoint{
      name: "http_requests_total",
      value: 1000.0,
      timestamp: 1609459200L,
      labels: [("service", "web-server")]
    },
    MetricPoint{
      name: "http_requests_total", 
      value: 1500.0,
      timestamp: 1609459260L, // 1分钟后
      labels: [("service", "web-server")]
    },
    MetricPoint{
      name: "http_requests_total",
      value: 2800.0, 
      timestamp: 1609459380L, // 3分钟后
      labels: [("service", "web-server")]
    },
    MetricPoint{
      name: "http_requests_total",
      value: 3200.0,
      timestamp: 1609459500L, // 5分钟后
      labels: [("service", "web-server")]
    }
  ]
  
  // 计算请求速率
  let rates = calculate_rate(counter_metrics)
  assert_eq(rates.length(), 3) // 3个间隔
  
  // 验证第一个间隔的速率 (1500-1000)/60s = 8.33 req/s
  assert_eq(rates[0].rate >= 8.0 && rates[0].rate <= 9.0, true)
  assert_eq(rates[0].interval_seconds, 60L)
  
  // 验证第二个间隔的速率 (2800-1500)/120s = 10.83 req/s  
  assert_eq(rates[1].rate >= 10.0 && rates[1].rate <= 12.0, true)
  assert_eq(rates[1].interval_seconds, 120L)
  
  // 验证第三个间隔的速率 (3200-2800)/120s = 3.33 req/s
  assert_eq(rates[2].rate >= 3.0 && rates[2].rate <= 4.0, true)
  assert_eq(rates[2].interval_seconds, 120L)
}

test "metrics_anomaly_detection" {
  // 测试指标异常检测
  
  // 创建包含异常值的时间序列
  let normal_metrics = []
  let base_time = 1609459200L
  
  // 生成正常数据点 (平均值50，标准差5)
  for i = 0; i < 100; i = i + 1 {
    normal_metrics.push(MetricPoint{
      name: "response_time",
      value: 50.0 + @random.gaussian(0.0, 5.0),
      timestamp: base_time + i.to_int64() * 60L,
      labels: [("endpoint", "/api/users")]
    })
  }
  
  // 插入异常值
  let metrics_with_anomalies = normal_metrics.slice(0, 50) + [
    MetricPoint{
      name: "response_time",
      value: 200.0, // 异常高值
      timestamp: base_time + 50.to_int64() * 60L,
      labels: [("endpoint", "/api/users")]
    }
  ] + normal_metrics.slice(51, 100)
  
  // 检测异常
  let anomalies = detect_anomalies(metrics_with_anomalies, 2.0) // 2倍标准差阈值
  assert_eq(anomalies.length() >= 1, true)
  
  // 验证检测到的异常
  let detected_anomaly = anomalies.find(|a| a.timestamp == base_time + 50.to_int64() * 60L)
  match detected_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.value, 200.0)
      assert_eq(anomaly.severity, "high")
      assert_eq(anomaly.reason.contains("超出正常范围"), true)
    }
    None => @test.fail("Expected anomaly not detected")
  }
  
  // 计算异常统计
  let anomaly_stats = calculate_anomaly_statistics(metrics_with_anomalies, anomalies)
  assert_eq(anomaly_stats.total_points, 101)
  assert_eq(anomaly_stats.anomaly_count >= 1, true)
  assert_eq(anomaly_stats.anomaly_percentage <= 5.0, true) // 异常比例应该很小
}