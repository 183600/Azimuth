// Azimuth 遥测数据处理测试
// 专注于测试遥测数据的收集、处理和分析功能

// 测试1: 遥测数据收集
test "遥测数据收集测试" {
  // 创建遥测数据收集器
  let collector = TelemetryCollector({
    name: "azimuth.collector",
    enabled: true,
    buffer_size: 1000
  })
  
  // 验证收集器配置
  assert_eq(collector.name, "azimuth.collector")
  assert_true(collector.enabled)
  assert_eq(collector.buffer_size, 1000)
  
  // 创建遥测数据点
  let data_point1 = DataPoint({
    timestamp: 1640995200000, // 2022-01-01 00:00:00
    metric_name: "cpu.usage",
    value: FloatValue(75.5),
    attributes: [("host", "server-1"), ("region", "us-west")]
  })
  
  let data_point2 = DataPoint({
    timestamp: 1640995201000, // 2022-01-01 00:00:01
    metric_name: "memory.usage",
    value: FloatValue(60.2),
    attributes: [("host", "server-1"), ("region", "us-west")]
  })
  
  // 验证数据点
  match data_point1.value {
    FloatValue(v) => assert_eq(v, 75.5)
    _ => assert_true(false)
  }
  
  match data_point2.value {
    FloatValue(v) => assert_eq(v, 60.2)
    _ => assert_true(false)
  }
  
  assert_eq(data_point1.metric_name, "cpu.usage")
  assert_eq(data_point2.metric_name, "memory.usage")
  assert_eq(data_point1.attributes.length(), 2)
  assert_eq(data_point2.attributes.length(), 2)
}

// 测试2: 遥测数据聚合
test "遥测数据聚合测试" {
  // 创建多个数据点
  let data_points = [
    DataPoint({
      timestamp: 1640995200000,
      metric_name: "response.time",
      value: FloatValue(100.0),
      attributes: [("endpoint", "/api/users")]
    }),
    DataPoint({
      timestamp: 1640995201000,
      metric_name: "response.time",
      value: FloatValue(150.0),
      attributes: [("endpoint", "/api/users")]
    }),
    DataPoint({
      timestamp: 1640995202000,
      metric_name: "response.time",
      value: FloatValue(200.0),
      attributes: [("endpoint", "/api/users")]
    })
  ]
  
  // 计算平均值
  let mut sum = 0.0
  let mut count = 0
  
  for point in data_points {
    match point.value {
      FloatValue(v) => {
        sum = sum + v
        count = count + 1
      }
      _ => assert_true(false)
    }
  }
  
  let average = sum / count.to_float()
  assert_eq(average, 150.0)
  
  // 查找最大值和最小值
  let mut max_value = 0.0
  let mut min_value = 999999.0
  
  for point in data_points {
    match point.value {
      FloatValue(v) => {
        if v > max_value { max_value = v }
        if v < min_value { min_value = v }
      }
      _ => assert_true(false)
    }
  }
  
  assert_eq(max_value, 200.0)
  assert_eq(min_value, 100.0)
}

// 测试3: 遥测数据过滤
test "遥测数据过滤测试" {
  // 创建包含不同属性的数据点
  let data_points = [
    DataPoint({
      timestamp: 1640995200000,
      metric_name: "error.rate",
      value: FloatValue(0.1),
      attributes: [("service", "auth"), ("environment", "production")]
    }),
    DataPoint({
      timestamp: 1640995201000,
      metric_name: "error.rate",
      value: FloatValue(0.5),
      attributes: [("service", "payment"), ("environment", "staging")]
    }),
    DataPoint({
      timestamp: 1640995202000,
      metric_name: "error.rate",
      value: FloatValue(0.05),
      attributes: [("service", "auth"), ("environment", "production")]
    })
  ]
  
  // 过滤生产环境的数据
  let production_data = data_points.filter(fn(point) {
    let mut is_production = false
    for (key, value) in point.attributes {
      if key == "environment" && value == "production" {
        is_production = true
      }
    }
    is_production
  })
  
  assert_eq(production_data.length(), 2)
  
  // 验证过滤后的数据
  for point in production_data {
    let mut is_production = false
    for (key, value) in point.attributes {
      if key == "environment" && value == "production" {
        is_production = true
      }
    }
    assert_true(is_production)
  }
  
  // 过滤认证服务的数据
  let auth_data = data_points.filter(fn(point) {
    let mut is_auth = false
    for (key, value) in point.attributes {
      if key == "service" && value == "auth" {
        is_auth = true
      }
    }
    is_auth
  })
  
  assert_eq(auth_data.length(), 2)
}

// 测试4: 遥测数据时间窗口分析
test "遥测数据时间窗口分析测试" {
  // 创建时间序列数据
  let time_series_data = [
    DataPoint({
      timestamp: 1640995200000, // 2022-01-01 00:00:00
      metric_name: "request.count",
      value: IntValue(100),
      attributes: []
    }),
    DataPoint({
      timestamp: 16409952060000, // 2022-01-01 00:01:00
      metric_name: "request.count",
      value: IntValue(150),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995212000, // 2022-01-01 00:02:00
      metric_name: "request.count",
      value: IntValue(120),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995218000, // 2022-01-01 00:03:00
      metric_name: "request.count",
      value: IntValue(180),
      attributes: []
    })
  ]
  
  // 定义时间窗口（1分钟）
  let window_start = 1640995200000
  let window_end = 1640995259999 // 1分钟后的时间
  
  // 过滤时间窗口内的数据
  let window_data = time_series_data.filter(fn(point) {
    point.timestamp >= window_start && point.timestamp <= window_end
  })
  
  assert_eq(window_data.length(), 4)
  
  // 计算时间窗口内的总请求数
  let mut total_requests = 0
  
  for point in window_data {
    match point.value {
      IntValue(v) => total_requests = total_requests + v
      _ => assert_true(false)
    }
  }
  
  assert_eq(total_requests, 550)
  
  // 计算平均每分钟请求数
  let avg_requests_per_minute = total_requests / 4
  assert_eq(avg_requests_per_minute, 137)
}

// 测试5: 遥测数据异常检测
test "遥测数据异常检测测试" {
  // 创建包含异常值的数据集
  let data_points = [
    DataPoint({
      timestamp: 1640995200000,
      metric_name: "latency",
      value: FloatValue(50.0),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995201000,
      metric_name: "latency",
      value: FloatValue(55.0),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995202000,
      metric_name: "latency",
      value: FloatValue(52.0),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995203000,
      metric_name: "latency",
      value: FloatValue(500.0), // 异常值
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995204000,
      metric_name: "latency",
      value: FloatValue(48.0),
      attributes: []
    })
  ]
  
  // 计算平均值和标准差
  let mut sum = 0.0
  let mut count = 0
  
  for point in data_points {
    match point.value {
      FloatValue(v) => {
        sum = sum + v
        count = count + 1
      }
      _ => assert_true(false)
    }
  }
  
  let mean = sum / count.to_float()
  assert_eq(mean, 141.0)
  
  // 计算方差
  let mut variance_sum = 0.0
  for point in data_points {
    match point.value {
      FloatValue(v) => {
        let diff = v - mean
        variance_sum = variance_sum + diff * diff
      }
      _ => assert_true(false)
    }
  }
  
  let variance = variance_sum / count.to_float()
  let std_dev = variance.sqrt()
  
  // 检测异常值（超过3个标准差）
  let threshold = 3.0 * std_dev
  let anomalies = data_points.filter(fn(point) {
    match point.value {
      FloatValue(v) => {
        let diff = (v - mean).abs()
        diff > threshold
      }
      _ => false
    }
  })
  
  assert_eq(anomalies.length(), 1)
  
  // 验证异常值
  match anomalies[0].value {
    FloatValue(v) => assert_eq(v, 500.0)
    _ => assert_true(false)
  }
}

// 测试6: 遥测数据序列化和反序列化
test "遥测数据序列化和反序列化测试" {
  // 创建遥测数据
  let original_data = TelemetryData({
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    metrics: [
      Metric({
        name: "cpu.usage",
        value: FloatValue(75.5),
        unit: "percent"
      }),
      Metric({
        name: "memory.usage",
        value: IntValue(1024),
        unit: "megabytes"
      })
    ],
    attributes: [
      ("service.name", "api-service"),
      ("host.name", "server-1")
    ],
    timestamp: 1640995200000
  })
  
  // 序列化为字符串
  let serialized = original_data.to_json_string()
  
  // 验证序列化结果包含关键信息
  assert_true(serialized.contains("trace-123456789"))
  assert_true(serialized.contains("span-987654321"))
  assert_true(serialized.contains("cpu.usage"))
  assert_true(serialized.contains("memory.usage"))
  assert_true(serialized.contains("api-service"))
  assert_true(serialized.contains("server-1"))
  
  // 反序列化
  let deserialized_data = TelemetryData.from_json_string(serialized)
  
  // 验证反序列化结果
  assert_eq(deserialized_data.trace_id, original_data.trace_id)
  assert_eq(deserialized_data.span_id, original_data.span_id)
  assert_eq(deserialized_data.metrics.length(), original_data.metrics.length())
  assert_eq(deserialized_data.attributes.length(), original_data.attributes.length())
  assert_eq(deserialized_data.timestamp, original_data.timestamp)
}

// 测试7: 遥测数据压缩
test "遥测数据压缩测试" {
  // 创建大量遥测数据
  let mut large_data_set = []
  
  for i in 0..=100 {
    let data_point = DataPoint({
      timestamp: 1640995200000 + i * 1000,
      metric_name: "system.metric",
      value: FloatValue(i.to_float()),
      attributes: [("host", "server-" + i.to_string())]
    })
    large_data_set = large_data_set.push(data_point)
  }
  
  assert_eq(large_data_set.length(), 101)
  
  // 压缩数据
  let compressed_data = compress_telemetry_data(large_data_set)
  
  // 验证压缩后的数据大小更小
  assert_true(compressed_data.length() < large_data_set.length() * 10)
  
  // 解压缩数据
  let decompressed_data = decompress_telemetry_data(compressed_data)
  
  // 验证解压缩后的数据完整性
  assert_eq(decompressed_data.length(), large_data_set.length())
  
  for i in 0..=100 {
    assert_eq(decompressed_data[i].timestamp, large_data_set[i].timestamp)
    assert_eq(decompressed_data[i].metric_name, large_data_set[i].metric_name)
    
    match decompressed_data[i].value {
      FloatValue(v1) => {
        match large_data_set[i].value {
          FloatValue(v2) => assert_eq(v1, v2)
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
}

// 测试8: 遥测数据实时处理
test "遥测数据实时处理测试" {
  // 创建实时处理器
  let processor = RealTimeProcessor({
    buffer_size: 100,
    processing_interval: 1000, // 1秒
    alert_thresholds: [
      ("cpu.usage", 80.0),
      ("memory.usage", 90.0),
      ("error.rate", 0.1)
    ]
  })
  
  // 模拟实时数据流
  let real_time_data = [
    DataPoint({
      timestamp: 1640995200000,
      metric_name: "cpu.usage",
      value: FloatValue(75.0),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995201000,
      metric_name: "memory.usage",
      value: FloatValue(85.0),
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995202000,
      metric_name: "cpu.usage",
      value: FloatValue(95.0), // 超过阈值
      attributes: []
    }),
    DataPoint({
      timestamp: 1640995203000,
      metric_name: "error.rate",
      value: FloatValue(0.15), // 超过阈值
      attributes: []
    })
  ]
  
  // 处理数据并检测告警
  let alerts = processor.process_data(real_time_data)
  
  // 验证告警数量
  assert_eq(alerts.length(), 2)
  
  // 验证告警内容
  let mut cpu_alert_found = false
  let mut error_alert_found = false
  
  for alert in alerts {
    match alert.metric_name {
      "cpu.usage" => {
        assert_eq(alert.severity, "warning")
        assert_true(alert.message.contains("95.0"))
        cpu_alert_found = true
      }
      "error.rate" => {
        assert_eq(alert.severity, "critical")
        assert_true(alert.message.contains("0.15"))
        error_alert_found = true
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(cpu_alert_found)
  assert_true(error_alert_found)
}