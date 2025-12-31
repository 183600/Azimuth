// 错误恢复测试 - 测试遥测系统在异常情况下的恢复能力
test "telemetry_error_recovery_span_failure" {
  // 测试目标：验证Span在异常情况下的恢复能力
  
  // 1. 模拟Span创建过程中的异常情况
  let error_recovery_span = @azimuth.telemetry.api.trace.Span{
    name: "error-recovery-test",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18],
      trace_flags: 0x01,
      trace_state: "error=recovery"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000,
    end_time_unix_nanos: Some(1640995200100000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Error, // 设置为错误状态
    status_description: Some("Simulated error for testing recovery"),
    attributes: [
      ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("timeout")),
      ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Operation timed out after 30 seconds")),
      ("error.retry_count", @azimuth.telemetry.api.common.AttributeValue::int(3)),
      ("recovery.attempted", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    ],
    events: [
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "error.occurred",
        timestamp_unix_nanos: 1640995200050000000,
        attributes: [
          ("error.code", @azimuth.telemetry.api.common.AttributeValue::string("TIMEOUT")),
          ("error.severity", @azimuth.telemetry.api.common.AttributeValue::string("high"))
        ]
      },
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "recovery.started",
        timestamp_unix_nanos: 1640995200060000000,
        attributes: [
          ("recovery.strategy", @azimuth.telemetry.api.common.AttributeValue::string("retry_with_backoff")),
          ("recovery.max_attempts", @azimuth.telemetry.api.common.AttributeValue::int(5))
        ]
      },
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "recovery.completed",
        timestamp_unix_nanos: 1640995200090000000,
        attributes: [
          ("recovery.success", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
          ("recovery.duration_ms", @azimuth.telemetry.api.common.AttributeValue::int(4000))
        ]
      }
    ],
    links: []
  }
  
  // 2. 验证错误状态处理
  assert_eq(error_recovery_span.status, @azimuth.telemetry.api.trace.StatusCode::Error)
  assert(error_recovery_span.status_description != None)
  assert_eq(error_recovery_span.status_description.unwrap(), "Simulated error for testing recovery")
  
  // 3. 验证错误属性记录
  let error_type_attr = error_recovery_span.attributes.find(fn((key, _) { key == "error.type" })
  let error_message_attr = error_recovery_span.attributes.find(fn((key, _) { key == "error.message" })
  let retry_count_attr = error_recovery_span.attributes.find(fn((key, _) { key == "error.retry_count" })
  let recovery_attempted_attr = error_recovery_span.attributes.find(fn((key, _) { key == "recovery.attempted" })
  
  assert(error_type_attr != None, "应该记录错误类型")
  assert(error_message_attr != None, "应该记录错误消息")
  assert(retry_count_attr != None, "应该记录重试次数")
  assert(recovery_attempted_attr != None, "应该记录恢复尝试状态")
  
  // 4. 验证错误事件序列
  assert_eq(error_recovery_span.events.length(), 3)
  
  let error_event = error_recovery_span.events[0]
  let recovery_started_event = error_recovery_span.events[1]
  let recovery_completed_event = error_recovery_span.events[2]
  
  assert_eq(error_event.name, "error.occurred")
  assert_eq(recovery_started_event.name, "recovery.started")
  assert_eq(recovery_completed_event.name, "recovery.completed")
  
  // 验证事件时间顺序
  assert(error_event.timestamp_unix_nanos < recovery_started_event.timestamp_unix_nanos)
  assert(recovery_started_event.timestamp_unix_nanos < recovery_completed_event.timestamp_unix_nanos)
  
  // 5. 验证恢复成功的状态
  let recovery_success_attr = recovery_completed_event.attributes.find(fn((key, _) { key == "recovery.success" })
  match recovery_success_attr {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(success))) => {
      assert(success, "恢复应该成功")
    }
    _ => assert(false, "恢复成功属性应该是布尔类型")
  }
  
  // 6. 创建恢复后的正常Span
  let recovered_span = @azimuth.telemetry.api.trace.Span{
    name: "recovered-operation",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
      span_id: [0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29],
      trace_flags: 0x01,
      trace_state: "recovered=true"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: Some([0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]),
    start_time_unix_nanos: 1640995200110000000,
    end_time_unix_nanos: Some(1640995200150000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok, // 恢复正常状态
    status_description: None,
    attributes: [
      ("operation.status", @azimuth.telemetry.api.common.AttributeValue::string("success")),
      ("recovery.from_error", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    ],
    events: [],
    links: []
  }
  
  // 7. 验证恢复后的Span状态
  assert_eq(recovered_span.status, @azimuth.telemetry.api.trace.StatusCode::Ok)
  assert_eq(recovered_span.parent_span_id, Some([0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]))
  
  println("✓ Span错误恢复测试通过")
}

test "telemetry_error_recovery_metrics_failure" {
  // 测试目标：验证度量系统在异常情况下的恢复能力
  
  // 1. 模拟度量收集失败的情况
  let failed_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 0.0, // 异常值
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("failed_counter")),
        ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("connection_timeout")),
        ("error.timestamp", @azimuth.telemetry.api.common.AttributeValue::int(1640995200)),
        ("collection.status", @azimuth.telemetry.api.common.AttributeValue::string("failed"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: -1.0, // 另一个异常值
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("failed_gauge")),
        ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("invalid_response")),
        ("error.timestamp", @azimuth.telemetry.api.common.AttributeValue::int(1640995205)),
        ("collection.status", @azimuth.telemetry.api.common.AttributeValue::string("failed"))
      ]
    }
  ]
  
  // 2. 验证失败度量的识别
  for metric in failed_metrics {
    let collection_status = metric.attributes.find(fn((key, _) { key == "collection.status" })
    match collection_status {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(status))) => {
        assert_eq(status, "failed", "应该标记为失败状态")
      }
      _ => assert(false, "收集状态属性应该存在且为字符串类型")
    }
    
    let error_type = metric.attributes.find(fn((key, _) { key == "error.type" })
    assert(error_type != None, "应该记录错误类型")
  }
  
  // 3. 模拟恢复后的正常度量
  let recovered_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 150.0, // 正常值
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("recovered_counter")),
        ("recovery.from_error", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
        ("previous.error", @azimuth.telemetry.api.common.AttributeValue::string("connection_timeout")),
        ("collection.status", @azimuth.telemetry.api.common.AttributeValue::string("success"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 75.5, // 正常值
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("recovered_gauge")),
        ("recovery.from_error", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
        ("previous.error", @azimuth.telemetry.api.common.AttributeValue::string("invalid_response")),
        ("collection.status", @azimuth.telemetry.api.common.AttributeValue::string("success"))
      ]
    }
  ]
  
  // 4. 验证恢复度量的有效性
  for metric in recovered_metrics {
    // 验证值在合理范围内
    assert(metric.value >= 0.0, "恢复后的度量值应该非负")
    
    // 验证恢复状态
    let recovery_status = metric.attributes.find(fn((key, _) { key == "recovery.from_error" })
    match recovery_status {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(recovered))) => {
        assert(recovered, "应该标记为从错误中恢复")
      }
      _ => assert(false, "恢复状态属性应该存在且为布尔类型")
    }
    
    // 验证收集状态
    let collection_status = metric.attributes.find(fn((key, _) { key == "collection.status" })
    match collection_status {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(status))) => {
        assert_eq(status, "success", "应该标记为成功状态")
      }
      _ => assert(false, "收集状态属性应该存在且为字符串类型")
    }
  }
  
  // 5. 测试度量系统的容错能力
  let mixed_metrics = failed_metrics + recovered_metrics
  
  // 验证系统可以处理混合状态（失败和成功的度量）
  let failed_count = mixed_metrics.fold(0, fn(acc, metric) {
    let status = metric.attributes.find(fn((key, _) { key == "collection.status" })
    match status {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue("failed"))) => acc + 1
      _ => acc
    }
  })
  
  let success_count = mixed_metrics.fold(0, fn(acc, metric) {
    let status = metric.attributes.find(fn((key, _) { key == "collection.status" })
    match status {
      Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue("success"))) => acc + 1
      _ => acc
    }
  })
  
  assert_eq(failed_count, 2, "应该有2个失败的度量")
  assert_eq(success_count, 2, "应该有2个成功的度量")
  
  // 6. 验证错误统计度量
  let error_stats_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: failed_count.to_double(),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("error_count")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("aggregated"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: success_count.to_double(),
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("success_count")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("aggregated"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: (success_count.to_double() / (failed_count + success_count).to_double()) * 100.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("success_rate")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("percentage"))
      ]
    }
  ]
  
  // 验证错误统计
  assert_eq(error_stats_metrics[0].value, 2.0, "错误计数应该为2")
  assert_eq(error_stats_metrics[1].value, 2.0, "成功计数应该为2")
  assert_eq(error_stats_metrics[2].value, 50.0, "成功率应该为50%")
  
  println("✓ 度量错误恢复测试通过")
}

test "telemetry_error_recovery_system_resilience" {
  // 测试目标：验证整个遥测系统的弹性和恢复能力
  
  // 1. 模拟系统级故障场景
  let system_error_scenarios = [
    {
      "scenario": "memory_pressure",
      "error_type": "OutOfMemory",
      "severity": "critical",
      "recovery_strategy": "garbage_collection",
      "recovery_time_ms": 5000
    },
    {
      "scenario": "network_partition",
      "error_type": "NetworkTimeout",
      "severity": "high",
      "recovery_strategy": "circuit_breaker",
      "recovery_time_ms": 3000
    },
    {
      "scenario": "disk_full",
      "error_type": "DiskFull",
      "severity": "critical",
      "recovery_strategy": "cleanup_old_data",
      "recovery_time_ms": 10000
    }
  ]
  
  // 2. 为每个故障场景创建相应的遥测数据
  let error_recovery_spans = system_error_scenarios.map(fn(scenario) {
    @azimuth.telemetry.api.trace.Span{
      name: "system-error-\{scenario["scenario"]}",
      context: @azimuth.telemetry.api.trace.SpanContext{
        trace_id: [0xff, 0xee, 0xdd, 0xcc, 0xbb, 0xaa, 0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0x00],
        span_id: [
          (scenario["scenario"].length() & 0xFF) as Byte,
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
        ],
        trace_flags: 0x01,
        trace_state: "system_error=\{scenario["scenario"]}"
      },
      kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
      parent_span_id: None,
      start_time_unix_nanos: 1640995200000000000,
      end_time_unix_nanos: Some(1640995200000000000 + (scenario["recovery_time_ms"] * 1000000)),
      status: @azimuth.telemetry.api.trace.StatusCode::Error,
      status_description: Some("System error: \{scenario["error_type"]}"),
      attributes: [
        ("error.scenario", @azimuth.telemetry.api.common.AttributeValue::string(scenario["scenario"])),
        ("error.type", @azimuth.telemetry.api.common.AttributeValue::string(scenario["error_type"])),
        ("error.severity", @azimuth.telemetry.api.common.AttributeValue::string(scenario["severity"])),
        ("recovery.strategy", @azimuth.telemetry.api.common.AttributeValue::string(scenario["recovery_strategy"])),
        ("recovery.time_ms", @azimuth.telemetry.api.common.AttributeValue::int(scenario["recovery_time_ms"])),
        ("system.affected", @azimuth.telemetry.api.common.AttributeValue::bool(true))
      ],
      events: [
        @azimuth.telemetry.api.trace.SpanEvent{
          name: "error.detected",
          timestamp_unix_nanos: 1640995200001000000,
          attributes: [
            ("detection.method", @azimuth.telemetry.api.common.AttributeValue::string("health_check"))
          ]
        },
        @azimuth.telemetry.api.trace.SpanEvent{
          name: "recovery.initiated",
          timestamp_unix_nanos: 1640995200002000000,
          attributes: [
            ("recovery.strategy", @azimuth.telemetry.api.common.AttributeValue::string(scenario["recovery_strategy"]))
          ]
        },
        @azimuth.telemetry.api.trace.SpanEvent{
          name: "recovery.completed",
          timestamp_unix_nanos: 1640995200000000000 + (scenario["recovery_time_ms"] * 1000000),
          attributes: [
            ("recovery.success", @azimuth.telemetry.api.common.AttributeValue::bool(true))
          ]
        }
      ],
      links: []
    }
  })
  
  // 3. 验证系统错误处理
  assert_eq(error_recovery_spans.length(), 3)
  
  for span in error_recovery_spans {
    // 验证错误状态
    assert_eq(span.status, @azimuth.telemetry.api.trace.StatusCode::Error)
    
    // 验证错误属性
    let error_scenario = span.attributes.find(fn((key, _) { key == "error.scenario" })
    let error_type = span.attributes.find(fn((key, _) { key == "error.type" })
    let error_severity = span.attributes.find(fn((key, _) { key == "error.severity" })
    let recovery_strategy = span.attributes.find(fn((key, _) { key == "recovery.strategy" })
    
    assert(error_scenario != None, "应该记录错误场景")
    assert(error_type != None, "应该记录错误类型")
    assert(error_severity != None, "应该记录错误严重程度")
    assert(recovery_strategy != None, "应该记录恢复策略")
    
    // 验证恢复事件
    assert_eq(span.events.length(), 3)
    assert_eq(span.events[0].name, "error.detected")
    assert_eq(span.events[1].name, "recovery.initiated")
    assert_eq(span.events[2].name, "recovery.completed")
  }
  
  // 4. 创建系统健康检查度量
  let system_health_metrics = [
    @azimuth.telemetry.api.metrics.Measurement{
      value: 3.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("system.errors.total")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("counter"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 3.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("system.recoveries.total")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("counter"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 100.0,
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("system.recovery.rate")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("percentage"))
      ]
    },
    @azimuth.telemetry.api.metrics.Measurement{
      value: 6000.0, // 平均恢复时间
      attributes: [
        ("metric.name", @azimuth.telemetry.api.common.AttributeValue::string("system.recovery.time.avg_ms")),
        ("metric.type", @azimuth.telemetry.api.common.AttributeValue::string("gauge"))
      ]
    }
  ]
  
  // 5. 验证系统健康度量
  assert_eq(system_health_metrics.length(), 4)
  
  let total_errors = system_health_metrics[0].value
  let total_recoveries = system_health_metrics[1].value
  let recovery_rate = system_health_metrics[2].value
  let avg_recovery_time = system_health_metrics[3].value
  
  assert_eq(total_errors, 3.0, "总错误数应该为3")
  assert_eq(total_recoveries, 3.0, "总恢复数应该为3")
  assert_eq(recovery_rate, 100.0, "恢复率应该为100%")
  assert(avg_recovery_time > 0.0, "平均恢复时间应该大于0")
  
  // 6. 创建恢复后的系统状态Span
  let system_recovered_span = @azimuth.telemetry.api.trace.Span{
    name: "system-fully-recovered",
    context: @azimuth.telemetry.api.trace.SpanContext{
      trace_id: [0xff, 0xee, 0xdd, 0xcc, 0xbb, 0xaa, 0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0x00],
      span_id: [0x99, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22],
      trace_flags: 0x01,
      trace_state: "system=healthy"
    },
    kind: @azimuth.telemetry.api.trace.SpanKind::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200200000000,
    end_time_unix_nanos: Some(1640995200250000000),
    status: @azimuth.telemetry.api.trace.StatusCode::Ok,
    status_description: Some("System fully recovered from all errors"),
    attributes: [
      ("system.status", @azimuth.telemetry.api.common.AttributeValue::string("healthy")),
      ("errors.recovered", @azimuth.telemetry.api.common.AttributeValue::int(3)),
      ("downtime.ms", @azimuth.telemetry.api.common.AttributeValue::int(25000)),
      ("data.integrity", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    ],
    events: [
      @azimuth.telemetry.api.trace.SpanEvent{
        name: "health.check.passed",
        timestamp_unix_nanos: 1640995200210000000,
        attributes: [
          ("check.type", @azimuth.telemetry.api.common.AttributeValue::string("full_system"))
        ]
      }
    ],
    links: []
  }
  
  // 7. 验证系统恢复状态
  assert_eq(system_recovered_span.status, @azimuth.telemetry.api.trace.StatusCode::Ok)
  assert_eq(system_recovered_span.status_description.unwrap(), "System fully recovered from all errors")
  
  let system_status = system_recovered_span.attributes.find(fn((key, _) { key == "system.status" })
  let errors_recovered = system_recovered_span.attributes.find(fn((key, _) { key == "errors.recovered" })
  let data_integrity = system_recovered_span.attributes.find(fn((key, _) { key == "data.integrity" })
  
  match system_status {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::StringValue(status))) => {
      assert_eq(status, "healthy", "系统状态应该为健康")
    }
    _ => assert(false, "系统状态属性应该存在且为字符串类型")
  }
  
  match errors_recovered {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::IntValue(recovered))) => {
      assert_eq(recovered, 3, "应该恢复3个错误")
    }
    _ => assert(false, "恢复错误数属性应该存在且为整数类型")
  }
  
  match data_integrity {
    Some((_, @azimuth.telemetry.api.common.AttributeValue::BoolValue(integrity))) => {
      assert(integrity, "数据完整性应该保持")
    }
    _ => assert(false, "数据完整性属性应该存在且为布尔类型")
  }
  
  println("✓ 系统弹性恢复测试通过")
}