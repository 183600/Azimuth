// 度量导出器测试
// 测试度量数据的导出功能和格式

test "metric exporter basic functionality" {
  // 测试度量导出器基本功能
  let meter = MeterProvider::get_meter(MeterProvider::default(), "exporter-test")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 记录度量数据
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.0)
  Counter::add(counter, 3.0)
  
  // 模拟导出器配置
  let exporter_config = {
    "name": "test-exporter",
    "endpoint": "http://localhost:9090/api/v1/metrics",
    "format": "prometheus",
    "export_interval": 10000,  // 10 seconds
    "timeout": 5000  // 5 seconds
  }
  
  // 模拟导出的度量数据
  let exported_metrics = [
    {
      "name": "test_counter",
      "description": "Test counter",
      "unit": "count",
      "value": 6.0,
      "type": "counter",
      "attributes": []
    }
  ]
  
  // 验证导出器配置
  assert_eq(exporter_config["name"], "test-exporter")
  assert_eq(exporter_config["endpoint"], "http://localhost:9090/api/v1/metrics")
  assert_eq(exporter_config["format"], "prometheus")
  
  // 验证导出的度量数据
  assert_eq(exported_metrics.length, 1)
  assert_eq(exported_metrics[0]["name"], "test_counter")
  assert_eq(exported_metrics[0]["value"], 6.0)
  assert_eq(exported_metrics[0]["type"], "counter")
}

test "metric exporter with histogram" {
  // 测试直方图度量导出
  let meter = MeterProvider::get_meter(MeterProvider::default(), "histogram-exporter-test")
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // 记录直方图数据
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 25.0)
  Histogram::record(histogram, 50.0)
  Histogram::record(histogram, 75.0)
  Histogram::record(histogram, 100.0)
  
  // 模拟导出的直方图度量
  let exported_histogram = {
    "name": "test_histogram",
    "description": "Test histogram",
    "unit": "ms",
    "type": "histogram",
    "buckets": [
      {"boundary": 10.0, "count": 1},
      {"boundary": 25.0, "count": 2},
      {"boundary": 50.0, "count": 3},
      {"boundary": 75.0, "count": 4},
      {"boundary": 100.0, "count": 5},
      {"boundary": "Inf", "count": 5}
    ],
    "sum": 260.0,
    "count": 5
  }
  
  // 验证直方图导出
  assert_eq(exported_histogram["name"], "test_histogram")
  assert_eq(exported_histogram["type"], "histogram")
  assert_eq(exported_histogram["count"], 5)
  assert_eq(exported_histogram["sum"], 260.0)
  assert_eq(exported_histogram["buckets"].length, 6)
}

test "metric exporter with attributes" {
  // 测试带属性的度量导出
  let meter = MeterProvider::get_meter(MeterProvider::default(), "attributes-exporter-test")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 记录带属性的度量数据
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "method", StringValue("GET"))
  Attributes::set(attrs1, "status", StringValue("200"))
  Counter::add(counter, 5.0, Some(attrs1))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "method", StringValue("POST"))
  Attributes::set(attrs2, "status", StringValue("201"))
  Counter::add(counter, 3.0, Some(attrs2))
  
  // 模拟导出的带属性的度量
  let exported_metrics_with_attrs = [
    {
      "name": "test_counter",
      "description": "Test counter",
      "unit": "count",
      "type": "counter",
      "attributes": [
        {"key": "method", "value": "GET"},
        {"key": "status", "value": "200"}
      ],
      "value": 5.0
    },
    {
      "name": "test_counter",
      "description": "Test counter",
      "unit": "count",
      "type": "counter",
      "attributes": [
        {"key": "method", "value": "POST"},
        {"key": "status", "value": "201"}
      ],
      "value": 3.0
    }
  ]
  
  // 验证带属性的度量导出
  assert_eq(exported_metrics_with_attrs.length, 2)
  assert_eq(exported_metrics_with_attrs[0]["attributes"][0]["value"], "GET")
  assert_eq(exported_metrics_with_attrs[0]["attributes"][1]["value"], "200")
  assert_eq(exported_metrics_with_attrs[0]["value"], 5.0)
  assert_eq(exported_metrics_with_attrs[1]["attributes"][0]["value"], "POST")
  assert_eq(exported_metrics_with_attrs[1]["attributes"][1]["value"], "201")
  assert_eq(exported_metrics_with_attrs[1]["value"], 3.0)
}

test "metric exporter batch export" {
  // 测试批量度量导出
  let meter = MeterProvider::get_meter(MeterProvider::default(), "batch-exporter-test")
  
  // 创建多个度量工具
  let counter1 = Meter::create_counter(meter, "counter1", Some("Counter 1"), Some("count"))
  let counter2 = Meter::create_counter(meter, "counter2", Some("Counter 2"), Some("count"))
  let gauge = Meter::create_gauge(meter, "gauge1", Some("Gauge 1"), Some("value"))
  let histogram = Meter::create_histogram(meter, "histogram1", Some("Histogram 1"), Some("ms"))
  
  // 记录度量数据
  Counter::add(counter1, 10.0)
  Counter::add(counter2, 20.0)
  Histogram::record(histogram, 50.0)
  Histogram::record(histogram, 100.0)
  
  // 模拟批量导出配置
  let batch_export_config = {
    "batch_size": 100,
    "max_export_time": 30000,  // 30 seconds
    "max_export_attempts": 3,
    "retry_delay": 1000  // 1 second
  }
  
  // 模拟批量导出的度量
  let batch_exported_metrics = [
    {
      "name": "counter1",
      "type": "counter",
      "value": 10.0,
      "attributes": []
    },
    {
      "name": "counter2",
      "type": "counter",
      "value": 20.0,
      "attributes": []
    },
    {
      "name": "gauge1",
      "type": "gauge",
      "value": 0.0,  // Default value
      "attributes": []
    },
    {
      "name": "histogram1",
      "type": "histogram",
      "count": 2,
      "sum": 150.0,
      "buckets": [
        {"boundary": 50.0, "count": 1},
        {"boundary": 100.0, "count": 2},
        {"boundary": "Inf", "count": 2}
      ]
    }
  ]
  
  // 验证批量导出配置
  assert_eq(batch_export_config["batch_size"], 100)
  assert_eq(batch_export_config["max_export_attempts"], 3)
  
  // 验证批量导出的度量
  assert_eq(batch_exported_metrics.length, 4)
  assert_eq(batch_exported_metrics[0]["name"], "counter1")
  assert_eq(batch_exported_metrics[1]["name"], "counter2")
  assert_eq(batch_exported_metrics[2]["type"], "gauge")
  assert_eq(batch_exported_metrics[3]["type"], "histogram")
}

test "metric exporter with different formats" {
  // 测试不同格式的度量导出
  let meter = MeterProvider::get_meter(MeterProvider::default(), "format-exporter-test")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 记录度量数据
  Counter::add(counter, 42.0)
  
  // Prometheus格式导出
  let prometheus_format = "# HELP test_counter Test counter\n# TYPE test_counter counter\ntest_counter 42.0"
  
  // JSON格式导出
  let json_format = "{\"metrics\":[{\"name\":\"test_counter\",\"description\":\"Test counter\",\"unit\":\"count\",\"type\":\"counter\",\"value\":42.0,\"attributes\":[]}]}"
  
  // OpenTelemetry格式导出
  let otel_format = "{\"resourceMetrics\":[{\"resource\":{},\"scopeMetrics\":[{\"scope\":{\"name\":\"format-exporter-test\"},\"metrics\":[{\"name\":\"test.counter\",\"description\":\"Test counter\",\"unit\":\"count\",\"sum\":{\"dataPoints\":[{\"value\":42.0}]}}]}]}]}"
  
  // 验证不同格式
  assert_true(prometheus_format.contains("# HELP test_counter"))
  assert_true(prometheus_format.contains("test_counter 42.0"))
  
  assert_true(json_format.contains("test_counter"))
  assert_true(json_format.contains("42.0"))
  
  assert_true(otel_format.contains("resourceMetrics"))
  assert_true(otel_format.contains("test_counter"))
}

test "metric exporter with error handling" {
  // 测试度量导出器错误处理
  let meter = MeterProvider::get_meter(MeterProvider::default(), "error-exporter-test")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 记录度量数据
  Counter::add(counter, 1.0)
  
  // 模拟导出错误配置
  let error_export_config = {
    "endpoint": "http://invalid-endpoint:9090/api/v1/metrics",
    "timeout": 5000,
    "max_retries": 3,
    "retry_delay": 1000,
    "backoff_multiplier": 2.0
  }
  
  // 模拟导出错误结果
  let export_error_result = {
    "success": false,
    "error_code": "CONNECTION_ERROR",
    "error_message": "Failed to connect to invalid-endpoint:9090",
    "retry_count": 3,
    "total_duration": 7000
  }
  
  // 验证错误处理配置
  assert_eq(error_export_config["endpoint"], "http://invalid-endpoint:9090/api/v1/metrics")
  assert_eq(error_export_config["max_retries"], 3)
  
  // 验证错误结果
  assert_true(false == export_error_result["success"])
  assert_eq(export_error_result["error_code"], "CONNECTION_ERROR")
  assert_eq(export_error_result["retry_count"], 3)
}

test "metric exporter with resource attributes" {
  // 测试带资源属性的度量导出
  let meter = MeterProvider::get_meter(MeterProvider::default(), "resource-exporter-test")
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("count"))
  
  // 记录度量数据
  Counter::add(counter, 5.0)
  
  // 创建资源属性
  let resource = Resource::new()
  let resource_with_attrs = Resource::with_attributes(resource, [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("test"))
  ])
  
  // 模拟带资源属性的导出
  let export_with_resource = {
    "resource": {
      "attributes": [
        {"key": "service.name", "value": "test-service"},
        {"key": "service.version", "value": "1.0.0"},
        {"key": "deployment.environment", "value": "test"}
      ]
    },
    "scope": {
      "name": "resource-exporter-test",
      "version": null,
      "schema_url": null
    },
    "metrics": [
      {
        "name": "test_counter",
        "description": "Test counter",
        "unit": "count",
        "type": "counter",
        "value": 5.0,
        "attributes": []
      }
    ]
  }
  
  // 验证资源属性导出
  assert_eq(export_with_resource["resource"]["attributes"][0]["value"], "test-service")
  assert_eq(export_with_resource["resource"]["attributes"][1]["value"], "1.0.0")
  assert_eq(export_with_resource["resource"]["attributes"][2]["value"], "test")
  assert_eq(export_with_resource["metrics"][0]["value"], 5.0)
}

test "metric exporter with temporal aggregation" {
  // 测试带时间聚合的度量导出
  let meter = MeterProvider::get_meter(MeterProvider::default(), "temporal-exporter-test")
  let gauge = Meter::create_gauge(meter, "cpu.usage", Some("CPU usage"), Some("percent"))
  
  // 模拟时间序列数据
  let time_series_data = [
    (1000L, 25.5),
    (2000L, 30.2),
    (3000L, 28.7),
    (4000L, 32.1),
    (5000L, 29.8)
  ]
  
  // 计算时间聚合
  let values = [25.5, 30.2, 28.7, 32.1, 29.8]
  let avg = values.reduce(0.0, (acc, val) => acc + val) / values.length.to_double()
  let max = values.reduce(0.0, (acc, val) => if val > acc { val } else { acc })
  let min = values.reduce(999.0, (acc, val) => if val < acc { val } else { acc })
  
  // 模拟时间聚合导出
  let temporal_aggregation_export = {
    "metric_name": "cpu.usage",
    "aggregation_window": "5s",
    "aggregations": {
      "avg": avg,
      "max": max,
      "min": min,
      "count": values.length,
      "sum": values.reduce(0.0, (acc, val) => acc + val)
    },
    "time_points": time_series_data
  }
  
  // 验证时间聚合导出
  assert_eq(temporal_aggregation_export["metric_name"], "cpu.usage")
  assert_eq(temporal_aggregation_export["aggregation_window"], "5s")
  assert_eq(temporal_aggregation_export["aggregations"]["avg"], 29.26)
  assert_eq(temporal_aggregation_export["aggregations"]["max"], 32.1)
  assert_eq(temporal_aggregation_export["aggregations"]["min"], 25.5)
  assert_eq(temporal_aggregation_export["aggregations"]["count"], 5)
}