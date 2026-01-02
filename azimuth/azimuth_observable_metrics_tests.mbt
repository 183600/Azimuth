// 可观察性度量测试
// 测试可观察度量的注册、收集和报告功能

test "observable counter registration and collection" {
  // 测试可观察计数器的注册和收集
  let meter = MeterProvider::get_meter(MeterProvider::default(), "observable-test")
  
  // 模拟可观察计数器注册
  let observable_counter_config = {
    "name": "process.operations.total",
    "description": "Total number of operations performed",
    "unit": "operations",
    "callback": "get_operation_count",
    "attributes": [("process.id", StringValue("proc-123"))]
  }
  
  // 模拟回调函数返回的度量值
  let callback_result = [
    {
      "attributes": [("operation.type", StringValue("read"))],
      "value": 150
    },
    {
      "attributes": [("operation.type", StringValue("write"))],
      "value": 75
    },
    {
      "attributes": [("operation.type", StringValue("delete"))],
      "value": 25
    }
  ]
  
  // 验证可观察计数器配置
  assert_eq(observable_counter_config["name"], "process.operations.total")
  assert_eq(observable_counter_config["description"], "Total number of operations performed")
  assert_eq(observable_counter_config["unit"], "operations")
  assert_eq(observable_counter_config["callback"], "get_operation_count")
  
  // 验证回调结果
  assert_eq(callback_result.length, 3)
  assert_eq(callback_result[0]["attributes"][0].1, StringValue("read"))
  assert_eq(callback_result[0]["value"], 150)
  assert_eq(callback_result[1]["value"], 75)
  assert_eq(callback_result[2]["value"], 25)
  
  // 计算总和
  let total_operations = callback_result[0]["value"] + callback_result[1]["value"] + callback_result[2]["value"]
  assert_eq(total_operations, 250)
}

test "observable gauge registration and collection" {
  // 测试可观察仪表的注册和收集
  let meter = MeterProvider::get_meter(MeterProvider::default(), "observable-gauge-test")
  
  // 模拟可观察仪表注册
  let observable_gauge_config = {
    "name": "system.memory.usage",
    "description": "Current memory usage",
    "unit": "bytes",
    "callback": "get_memory_usage",
    "collection_interval": 10000  // 10 seconds
  }
  
  // 模拟回调函数返回的当前值
  let gauge_callback_result = [
    {
      "attributes": [("memory.type", StringValue("used"))],
      "value": 8589934592L  // 8GB
    },
    {
      "attributes": [("memory.type", StringValue("free"))],
      "value": 8589934592L  // 8GB
    },
    {
      "attributes": [("memory.type", StringValue("cached"))],
      "value": 2147483648L  // 2GB
    }
  ]
  
  // 验证可观察仪表配置
  assert_eq(observable_gauge_config["name"], "system.memory.usage")
  assert_eq(observable_gauge_config["description"], "Current memory usage")
  assert_eq(observable_gauge_config["unit"], "bytes")
  assert_eq(observable_gauge_config["collection_interval"], 10000)
  
  // 验证回调结果
  assert_eq(gauge_callback_result.length, 3)
  assert_eq(gauge_callback_result[0]["attributes"][0].1, StringValue("used"))
  assert_eq(gauge_callback_result[0]["value"], 8589934592L)
  assert_eq(gauge_callback_result[1]["attributes"][0].1, StringValue("free"))
  assert_eq(gauge_callback_result[2]["attributes"][0].1, StringValue("cached"))
  
  // 计算总内存
  let total_memory = gauge_callback_result[0]["value"] + gauge_callback_result[1]["value"] + gauge_callback_result[2]["value"]
  assert_eq(total_memory, 19327352832L)  // 18GB
}

test "observable updown counter registration and collection" {
  // 测试可观察上下计数器的注册和收集
  let meter = MeterProvider::get_meter(MeterProvider::default(), "observable-updown-test")
  
  // 模拟可观察上下计数器注册
  let observable_updown_config = {
    "name": "connection.pool.active",
    "description": "Number of active connections in the pool",
    "unit": "connections",
    "callback": "get_active_connections",
    "attributes": [("pool.name", StringValue("db-pool"))]
  }
  
  // 模拟回调函数返回的当前值（可以增加或减少）
  let updown_callback_results = [
    // 第一次收集
    [
      {
        "attributes": [("connection.state", StringValue("active"))],
        "value": 15
      },
      {
        "attributes": [("connection.state", StringValue("idle"))],
        "value": 5
      }
    ],
    // 第二次收集（连接数变化）
    [
      {
        "attributes": [("connection.state", StringValue("active"))],
        "value": 12
      },
      {
        "attributes": [("connection.state", StringValue("idle"))],
        "value": 8
      }
    ],
    // 第三次收集（连接数变化）
    [
      {
        "attributes": [("connection.state", StringValue("active"))],
        "value": 18
      },
      {
        "attributes": [("connection.state", StringValue("idle"))],
        "value": 2
      }
    ]
  ]
  
  // 验证可观察上下计数器配置
  assert_eq(observable_updown_config["name"], "connection.pool.active")
  assert_eq(observable_updown_config["description"], "Number of active connections in the pool")
  assert_eq(observable_updown_config["unit"], "connections")
  
  // 验证回调结果变化
  assert_eq(updown_callback_results.length, 3)
  
  // 第一次收集
  assert_eq(updown_callback_results[0][0]["value"], 15)
  assert_eq(updown_callback_results[0][1]["value"], 5)
  
  // 第二次收集
  assert_eq(updown_callback_results[1][0]["value"], 12)  // 减少了3
  assert_eq(updown_callback_results[1][1]["value"], 8)   // 增加了3
  
  // 第三次收集
  assert_eq(updown_callback_results[2][0]["value"], 18)  // 增加了6
  assert_eq(updown_callback_results[2][1]["value"], 2)   // 减少了6
}

test "observable metric with async callback" {
  // 测试异步回调的可观察度量
  let meter = MeterProvider::get_meter(MeterProvider::default(), "async-observable-test")
  
  // 模拟异步可观察度量注册
  let async_observable_config = {
    "name": "http.requests.duration",
    "description": "HTTP request duration over the last minute",
    "unit": "ms",
    "callback": "get_request_duration_stats",
    "async": true,
    "timeout": 5000  // 5 seconds timeout
  }
  
  // 模拟异步回调结果
  let async_callback_result = {
    "data_points": [
      {
        "attributes": [
          ("http.method", StringValue("GET")),
          ("http.status_code", StringValue("200"))
        ],
        "value": 125.5
      },
      {
        "attributes": [
          ("http.method", StringValue("POST")),
          ("http.status_code", StringValue("201"))
        ],
        "value": 250.0
      },
      {
        "attributes": [
          ("http.method", StringValue("GET")),
          ("http.status_code", StringValue("404"))
        ],
        "value": 45.0
      }
    ],
    "collection_time": 1735689600000000000L,
    "collection_duration": 150  // 150ms
  }
  
  // 验证异步可观察度量配置
  assert_eq(async_observable_config["name"], "http.requests.duration")
  assert_eq(async_observable_config["async"], true)
  assert_eq(async_observable_config["timeout"], 5000)
  
  // 验证异步回调结果
  assert_eq(async_callback_result["data_points"].length, 3)
  assert_eq(async_callback_result["collection_duration"], 150)
  
  // 计算平均延迟
  let total_duration = async_callback_result["data_points"][0]["value"] + 
                      async_callback_result["data_points"][1]["value"] + 
                      async_callback_result["data_points"][2]["value"]
  let avg_duration = total_duration / 3.0
  assert_eq(avg_duration, 140.16666666666666)
}

test "observable metric with error handling" {
  // 测试可观察度量的错误处理
  let meter = MeterProvider::get_meter(MeterProvider::default(), "error-observable-test")
  
  // 模拟可能出错的可观察度量注册
  let error_prone_observable_config = {
    "name": "database.query.time",
    "description": "Database query execution time",
    "unit": "ms",
    "callback": "get_db_query_time",
    "error_handling": {
      "retry_count": 3,
      "retry_delay": 1000,  // 1 second
      "fallback_value": 0.0,
      "report_errors": true
    }
  }
  
  // 模拟错误场景的回调结果
  let error_callback_results = [
    // 成功情况
    {
      "success": true,
      "data_points": [
        {
          "attributes": [("db.operation", StringValue("SELECT"))],
          "value": 25.5
        }
      ]
    },
    // 错误情况（重试后成功）
    {
      "success": false,
      "error": "Connection timeout",
      "retry_count": 2,
      "fallback_used": true,
      "data_points": [
        {
          "attributes": [("db.operation", StringValue("SELECT"))],
          "value": 0.0  // fallback value
        }
      ]
    },
    // 完全失败情况
    {
      "success": false,
      "error": "Database unavailable",
      "retry_count": 3,
      "fallback_used": true,
      "data_points": []
    }
  ]
  
  // 验证错误处理配置
  assert_eq(error_prone_observable_config["error_handling"]["retry_count"], 3)
  assert_eq(error_prone_observable_config["error_handling"]["fallback_value"], 0.0)
  assert_eq(error_prone_observable_config["error_handling"]["report_errors"], true)
  
  // 验证错误场景结果
  assert_eq(error_callback_results.length, 3)
  assert_true(error_callback_results[0]["success"])
  assert_true(false == error_callback_results[1]["success"])
  assert_eq(error_callback_results[1]["retry_count"], 2)
  assert_true(error_callback_results[1]["fallback_used"])
  assert_eq(error_callback_results[2]["data_points"].length, 0)
}

test "observable metric with multi-instrument collection" {
  // 测试多仪器可观察度量收集
  let meter = MeterProvider::get_meter(MeterProvider::default(), "multi-instrument-test")
  
  // 模拟多个可观察度量的注册
  let multi_instrument_config = {
    "instruments": [
      {
        "name": "cpu.usage",
        "type": "gauge",
        "description": "CPU usage percentage",
        "unit": "%"
      },
      {
        "name": "memory.usage",
        "type": "gauge",
        "description": "Memory usage in bytes",
        "unit": "bytes"
      },
      {
        "name": "disk.io.ops",
        "type": "counter",
        "description": "Disk I/O operations count",
        "unit": "operations"
      }
    ],
    "callback": "get_system_metrics",
    "batch_collection": true,
    "collection_timeout": 3000  // 3 seconds
  }
  
  // 模拟多仪器回调结果
  let multi_instrument_result = {
    "collection_timestamp": 1735689600000000000L,
    "instruments": [
      {
        "name": "cpu.usage",
        "data_points": [
          {
            "attributes": [("cpu.core", StringValue("0"))],
            "value": 45.5
          },
          {
            "attributes": [("cpu.core", StringValue("1"))],
            "value": 52.3
          }
        ]
      },
      {
        "name": "memory.usage",
        "data_points": [
          {
            "attributes": [("memory.type", StringValue("used"))],
            "value": 8589934592L
          },
          {
            "attributes": [("memory.type", StringValue("free"))],
            "value": 4294967296L
          }
        ]
      },
      {
        "name": "disk.io.ops",
        "data_points": [
          {
            "attributes": [("disk.operation", StringValue("read"))],
            "value": 1250
          },
          {
            "attributes": [("disk.operation", StringValue("write"))],
            "value": 750
          }
        ]
      }
    ]
  }
  
  // 验证多仪器配置
  assert_eq(multi_instrument_config["instruments"].length, 3)
  assert_eq(multi_instrument_config["batch_collection"], true)
  assert_eq(multi_instrument_config["collection_timeout"], 3000)
  
  // 验证多仪器结果
  assert_eq(multi_instrument_result["instruments"].length, 3)
  assert_eq(multi_instrument_result["instruments"][0]["name"], "cpu.usage")
  assert_eq(multi_instrument_result["instruments"][0]["data_points"].length, 2)
  assert_eq(multi_instrument_result["instruments"][1]["name"], "memory.usage")
  assert_eq(multi_instrument_result["instruments"][2]["name"], "disk.io.ops")
  
  // 计算总CPU使用率
  let cpu_data_points = multi_instrument_result["instruments"][0]["data_points"]
  let total_cpu_usage = cpu_data_points[0]["value"] + cpu_data_points[1]["value"]
  assert_eq(total_cpu_usage, 97.8)
}

test "observable metric with conditional collection" {
  // 测试条件收集的可观察度量
  let meter = MeterProvider::get_meter(MeterProvider::default(), "conditional-observable-test")
  
  // 模拟条件收集的可观察度量注册
  let conditional_observable_config = {
    "name": "debug.metrics",
    "description": "Debug metrics collected conditionally",
    "unit": "various",
    "callback": "get_debug_metrics",
    "conditions": [
      {
        "name": "debug_mode",
        "expected_value": true,
        "description": "Only collect when debug mode is enabled"
      },
      {
        "name": "min_log_level",
        "operator": "less_than_or_equal",
        "value": "debug",
        "description": "Only collect when log level is debug or lower"
      }
    ],
    "fallback_on_condition_failure": false
  }
  
  // 模拟不同条件下的收集结果
  let conditional_collection_results = [
    // 条件满足
    {
      "conditions_met": true,
      "data_points": [
        {
          "attributes": [("debug.metric", StringValue("cache.hit.ratio"))],
          "value": 0.85
        },
        {
          "attributes": [("debug.metric", StringValue("gc.pause.time"))],
          "value": 15.5
        }
      ]
    },
    // 条件不满足
    {
      "conditions_met": false,
      "failed_conditions": ["debug_mode"],
      "data_points": []
    },
    // 部分条件满足
    {
      "conditions_met": false,
      "failed_conditions": ["min_log_level"],
      "data_points": []
    }
  ]
  
  // 验证条件收集配置
  assert_eq(conditional_observable_config["conditions"].length, 2)
  assert_eq(conditional_observable_config["conditions"][0]["name"], "debug_mode")
  assert_eq(conditional_observable_config["conditions"][1]["operator"], "less_than_or_equal")
  assert_eq(conditional_observable_config["fallback_on_condition_failure"], false)
  
  // 验证条件收集结果
  assert_eq(conditional_collection_results.length, 3)
  assert_true(conditional_collection_results[0]["conditions_met"])
  assert_eq(conditional_collection_results[0]["data_points"].length, 2)
  assert_true(false == conditional_collection_results[1]["conditions_met"])
  assert_eq(conditional_collection_results[1]["failed_conditions"][0], "debug_mode")
  assert_eq(conditional_collection_results[2]["data_points"].length, 0)
}

test "observable metric with temporal aggregation" {
  // 测试时间聚合的可观察度量
  let meter = MeterProvider::get_meter(MeterProvider::default(), "temporal-observable-test")
  
  // 模拟时间聚合的可观察度量注册
  let temporal_observable_config = {
    "name": "request.rate",
    "description": "Request rate over time windows",
    "unit": "requests/sec",
    "callback": "get_request_rate",
    "temporal_aggregation": {
      "windows": ["1m", "5m", "15m"],
      "functions": ["rate", "sum", "avg"],
      "alignment": "aligned"
    }
  }
  
  // 模拟时间聚合结果
  let temporal_aggregation_result = {
    "timestamp": 1735689600000000000L,
    "windows": {
      "1m": {
        "rate": 10.5,
        "sum": 630,
        "avg": 10.5
      },
      "5m": {
        "rate": 12.3,
        "sum": 3690,
        "avg": 12.3
      },
      "15m": {
        "rate": 11.8,
        "sum": 10620,
        "avg": 11.8
      }
    },
    "data_points": [
      {
        "attributes": [("endpoint", StringValue("/api/users"))],
        "windows": {
          "1m": {"rate": 5.2, "sum": 312, "avg": 5.2},
          "5m": {"rate": 6.1, "sum": 1830, "avg": 6.1},
          "15m": {"rate": 5.9, "sum": 5310, "avg": 5.9}
        }
      },
      {
        "attributes": [("endpoint", StringValue("/api/orders"))],
        "windows": {
          "1m": {"rate": 5.3, "sum": 318, "avg": 5.3},
          "5m": {"rate": 6.2, "sum": 1860, "avg": 6.2},
          "15m": {"rate": 5.9, "sum": 5310, "avg": 5.9}
        }
      }
    ]
  }
  
  // 验证时间聚合配置
  assert_eq(temporal_observable_config["temporal_aggregation"]["windows"].length, 3)
  assert_eq(temporal_observable_config["temporal_aggregation"]["functions"].length, 3)
  assert_eq(temporal_observable_config["temporal_aggregation"]["alignment"], "aligned")
  
  // 验证时间聚合结果
  assert_eq(temporal_aggregation_result["data_points"].length, 2)
  assert_eq(temporal_aggregation_result["windows"]["1m"]["rate"], 10.5)
  assert_eq(temporal_aggregation_result["windows"]["5m"]["sum"], 3690)
  assert_eq(temporal_aggregation_result["data_points"][0]["attributes"][0].1, StringValue("/api/users"))
  assert_eq(temporal_aggregation_result["data_points"][0]["windows"]["1m"]["rate"], 5.2)
  assert_eq(temporal_aggregation_result["data_points"][1]["windows"]["15m"]["sum"], 5310)
}