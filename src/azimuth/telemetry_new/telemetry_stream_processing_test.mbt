// 遥测数据流处理测试用例

test "stream_data_ingestion" {
  // 测试流数据摄取
  
  let stream_sources = [
    {
      "source_id": "source_001",
      "source_type": "kafka",
      "topic": "telemetry.traces",
      "partition_count": 3,
      "replication_factor": 2,
      "batch_size": 100,
      "compression": "gzip",
      "status": "active"
    },
    {
      "source_id": "source_002",
      "source_type": "kafka",
      "topic": "telemetry.metrics",
      "partition_count": 6,
      "replication_factor": 2,
      "batch_size": 500,
      "compression": "snappy",
      "status": "active"
    },
    {
      "source_id": "source_003",
      "source_type": "rabbitmq",
      "queue": "telemetry.logs",
      "durable": true,
      "batch_size": 50,
      "priority": "normal",
      "status": "active"
    }
  ]
  
  // 验证流数据源
  assert_eq(stream_sources.length(), 3)
  
  let ingestion_metrics = [
    {
      "source_id": "source_001",
      "timestamp": 1641018000000L,
      "metrics": {
        "messages_received": 10000,
        "messages_processed": 9950,
        "messages_failed": 50,
        "avg_processing_time_ms": 15,
        "throughput_msg_per_sec": 1000,
        "lag_messages": 150
      }
    },
    {
      "source_id": "source_002",
      "timestamp": 1641018000000L,
      "metrics": {
        "messages_received": 25000,
        "messages_processed": 24800,
        "messages_failed": 200,
        "avg_processing_time_ms": 8,
        "throughput_msg_per_sec": 2500,
        "lag_messages": 300
      }
    },
    {
      "source_id": "source_003",
      "timestamp": 1641018000000L,
      "metrics": {
        "messages_received": 5000,
        "messages_processed": 4950,
        "messages_failed": 50,
        "avg_processing_time_ms": 25,
        "throughput_msg_per_sec": 500,
        "lag_messages": 75
      }
    }
  ]
  
  // 验证摄取指标
  assert_eq(ingestion_metrics.length(), 3)
  
  // 分析摄取性能
  let mut ingestion_performance = []
  
  for metrics in ingestion_metrics {
    let source_id = metrics["source_id"]
    let current_metrics = metrics["metrics"]
    
    // 计算成功率
    let total_messages = current_metrics["messages_received"]
    let processed_messages = current_metrics["messages_processed"]
    let failed_messages = current_metrics["messages_failed"]
    
    let success_rate = processed_messages.to_double() / total_messages.to_double() * 100.0
    let failure_rate = failed_messages.to_double() / total_messages.to_double() * 100.0
    
    // 计算效率指标
    let processing_efficiency = current_metrics["throughput_msg_per_sec"].to_double() / current_metrics["avg_processing_time_ms"].to_double()
    
    // 确定健康状态
    let mut health_status = "healthy"
    if success_rate < 95.0 or current_metrics["lag_messages"] > 1000 {
      health_status = "warning"
    }
    if success_rate < 90.0 or current_metrics["lag_messages"] > 5000 {
      health_status = "critical"
    }
    
    ingestion_performance.push((
      source_id,
      success_rate,
      failure_rate,
      processing_efficiency,
      health_status
    ))
  }
  
  // 验证摄取性能
  assert_eq(ingestion_performance.length(), 3)
  
  // 验证第一个源性能
  let source_001_perf = ingestion_performance[0]
  assert_eq(source_001_perf.0, "source_001")
  assert_eq(source_001_perf.1, 99.5)  // 9950/10000 * 100
  assert_eq(source_001_perf.2, 0.5)   // 50/10000 * 100
  assert_eq(source_001_perf.3, 66.67) // 1000/15
  assert_eq(source_001_perf.4, "healthy")
  
  // 验证第二个源性能
  let source_002_perf = ingestion_performance[1]
  assert_eq(source_002_perf.0, "source_002")
  assert_eq(source_002_perf.1, 99.2)  // 24800/25000 * 100
  assert_eq(source_002_perf.2, 0.8)   // 200/25000 * 100
  assert_eq(source_002_perf.3, 312.5) // 2500/8
  assert_eq(source_002_perf.4, "healthy")
  
  // 验证第三个源性能
  let source_003_perf = ingestion_performance[2]
  assert_eq(source_003_perf.0, "source_003")
  assert_eq(source_003_perf.1, 99.0)  // 4950/5000 * 100
  assert_eq(source_003_perf.2, 1.0)   // 50/5000 * 100
  assert_eq(source_003_perf.3, 20.0)  // 500/25
  assert_eq(source_003_perf.4, "healthy")
  
  // 计算总体摄取统计
  let mut total_received = 0
  let mut total_processed = 0
  let mut total_failed = 0
  let mut total_lag = 0
  
  for metrics in ingestion_metrics {
    let current_metrics = metrics["metrics"]
    total_received = total_received + current_metrics["messages_received"]
    total_processed = total_processed + current_metrics["messages_processed"]
    total_failed = total_failed + current_metrics["messages_failed"]
    total_lag = total_lag + current_metrics["lag_messages"]
  }
  
  // 验证总体统计
  assert_eq(total_received, 40000)  // 10000 + 25000 + 5000
  assert_eq(total_processed, 39700) // 9950 + 24800 + 4950
  assert_eq(total_failed, 300)      // 50 + 200 + 50
  assert_eq(total_lag, 525)         // 150 + 300 + 75
  
  let overall_success_rate = total_processed.to_double() / total_received.to_double() * 100.0
  assert_eq(overall_success_rate, 99.25) // 39700/40000 * 100
}

test "stream_data_transformation" {
  // 测试流数据转换
  
  let transformation_pipelines = [
    {
      "pipeline_id": "pipeline_001",
      "name": "Trace Enrichment",
      "input_source": "source_001",
      "output_sink": "sink_001",
      "transformations": [
        {
          "step": 1,
          "type": "field_enrichment",
          "config": {"add_fields": {"environment": "production", "version": "1.2.3"}}
        },
        {
          "step": 2,
          "type": "data_validation",
          "config": {"required_fields": ["trace_id", "span_id", "service_name"]}
        },
        {
          "step": 3,
          "type": "format_conversion",
          "config": {"from": "json", "to": "protobuf"}
        }
      ],
      "status": "running"
    },
    {
      "pipeline_id": "pipeline_002",
      "name": "Metric Aggregation",
      "input_source": "source_002",
      "output_sink": "sink_002",
      "transformations": [
        {
          "step": 1,
          "type": "time_window_aggregation",
          "config": {"window_size": "1m", "aggregation": "avg"}
        },
        {
          "step": 2,
          "type": "dimension_reduction",
          "config": {"group_by": ["service", "operation"]}
        },
        {
          "step": 3,
          "type": "data_compression",
          "config": {"algorithm": "gzip"}
        }
      ],
      "status": "running"
    }
  ]
  
  // 验证转换管道
  assert_eq(transformation_pipelines.length(), 2)
  
  let transformation_metrics = [
    {
      "pipeline_id": "pipeline_001",
      "timestamp": 1641018000000L,
      "metrics": {
        "input_records": 10000,
        "output_records": 9850,
        "dropped_records": 150,
        "error_records": 0,
        "avg_processing_time_ms": 5,
        "throughput_rec_per_sec": 2000
      }
    },
    {
      "pipeline_id": "pipeline_002",
      "timestamp": 1641018000000L,
      "metrics": {
        "input_records": 25000,
        "output_records": 1250,
        "dropped_records": 0,
        "error_records": 50,
        "avg_processing_time_ms": 12,
        "throughput_rec_per_sec": 2083
      }
    }
  ]
  
  // 验证转换指标
  assert_eq(transformation_metrics.length(), 2)
  
  // 分析转换效率
  let mut transformation_efficiency = []
  
  for metrics in transformation_metrics {
    let pipeline_id = metrics["pipeline_id"]
    let current_metrics = metrics["metrics"]
    
    // 计算转换率
    let input_records = current_metrics["input_records"]
    let output_records = current_metrics["output_records"]
    let dropped_records = current_metrics["dropped_records"]
    let error_records = current_metrics["error_records"]
    
    let transformation_rate = output_records.to_double() / input_records.to_double()
    let error_rate = error_records.to_double() / input_records.to_double()
    let drop_rate = dropped_records.to_double() / input_records.to_double()
    
    // 计算处理效率
    let processing_efficiency = current_metrics["throughput_rec_per_sec"].to_double() / current_metrics["avg_processing_time_ms"].to_double()
    
    transformation_efficiency.push((
      pipeline_id,
      transformation_rate,
      error_rate,
      drop_rate,
      processing_efficiency
    ))
  }
  
  // 验证转换效率
  assert_eq(transformation_efficiency.length(), 2)
  
  // 验证第一个管道效率
  let pipeline_001_eff = transformation_efficiency[0]
  assert_eq(pipeline_001_eff.0, "pipeline_001")
  assert_eq(pipeline_001_eff.1, 0.985)  // 9850/10000
  assert_eq(pipeline_001_eff.2, 0.0)    // 0/10000
  assert_eq(pipeline_001_eff.3, 0.015)  // 150/10000
  assert_eq(pipeline_001_eff.4, 400.0)  // 2000/5
  
  // 验证第二个管道效率
  let pipeline_002_eff = transformation_efficiency[1]
  assert_eq(pipeline_002_eff.0, "pipeline_002")
  assert_eq(pipeline_002_eff.1, 0.05)   // 1250/25000 (聚合导致记录减少)
  assert_eq(pipeline_002_eff.2, 0.002)  // 50/25000
  assert_eq(pipeline_002_eff.3, 0.0)    // 0/25000
  assert_eq(pipeline_002_eff.4, 173.58) // 2083/12
  
  // 测试转换步骤性能
  let step_performance_tests = [
    {
      "pipeline_id": "pipeline_001",
      "step": 1,
      "type": "field_enrichment",
      "avg_time_ms": 1,
      "records_processed": 10000
    },
    {
      "pipeline_id": "pipeline_001",
      "step": 2,
      "type": "data_validation",
      "avg_time_ms": 2,
      "records_processed": 9950
    },
    {
      "pipeline_id": "pipeline_001",
      "step": 3,
      "type": "format_conversion",
      "avg_time_ms": 2,
      "records_processed": 9850
    }
  ]
  
  // 验证步骤性能测试
  assert_eq(step_performance_tests.length(), 3)
  
  // 分析步骤性能瓶颈
  let mut slowest_step = step_performance_tests[0]
  for step in step_performance_tests {
    if step["avg_time_ms"] > slowest_step["avg_time_ms"] {
      slowest_step = step
    }
  }
  
  // 验证最慢步骤
  assert_eq(slowest_step["avg_time_ms"], 2)  // 验证和转换步骤
  
  // 计算管道总处理时间
  let mut total_pipeline_time = 0
  for step in step_performance_tests {
    total_pipeline_time = total_pipeline_time + step["avg_time_ms"]
  }
  assert_eq(total_pipeline_time, 5)  // 1 + 2 + 2
  
  // 生成转换报告
  let transformation_report = {
    "total_pipelines": transformation_pipelines.length(),
    "average_transformation_rate": (pipeline_001_eff.1 + pipeline_002_eff.1) / 2.0,
    "average_error_rate": (pipeline_001_eff.2 + pipeline_002_eff.2) / 2.0,
    "slowest_step_type": slowest_step["type"],
    "slowest_step_time_ms": slowest_step["avg_time_ms"],
    "overall_pipeline_health": "healthy"
  }
  
  // 验证转换报告
  assert_eq(transformation_report["total_pipelines"], 2)
  assert_eq(transformation_report["average_transformation_rate"], 0.5175)  // (0.985 + 0.05) / 2
  assert_eq(transformation_report["average_error_rate"], 0.001)         // (0.0 + 0.002) / 2
  assert_eq(transformation_report["slowest_step_time_ms"], 2)
  assert_eq(transformation_report["overall_pipeline_health"], "healthy")
}

test "real_time_stream_analytics" {
  // 测试实时流分析
  
  let analytics_rules = [
    {
      "rule_id": "rule_001",
      "name": "Error Rate Alert",
      "description": "Alert when error rate exceeds 5%",
      "condition": "error_rate > 0.05",
      "time_window": "5m",
      "aggregation": "avg",
      "severity": "warning",
      "enabled": true
    },
    {
      "rule_id": "rule_002",
      "name": "High Latency Alert",
      "description": "Alert when response time exceeds 1s",
      "condition": "response_time_p95 > 1000",
      "time_window": "1m",
      "aggregation": "max",
      "severity": "critical",
      "enabled": true
    },
    {
      "rule_id": "rule_003",
      "name": "Traffic Spike Detection",
      "description": "Detect unusual traffic spikes",
      "condition": "request_rate > baseline * 2.0",
      "time_window": "10m",
      "aggregation": "sum",
      "severity": "info",
      "enabled": true
    }
  ]
  
  // 验证分析规则
  assert_eq(analytics_rules.length(), 3)
  
  let real_time_metrics = [
    {
      "timestamp": 1641018000000L,
      "service": "api-gateway",
      "metrics": {
        "error_rate": 0.08,
        "response_time_p95": 850,
        "request_rate": 1200,
        "baseline_request_rate": 500
      }
    },
    {
      "timestamp": 1641018001000L,
      "service": "user-service",
      "metrics": {
        "error_rate": 0.03,
        "response_time_p95": 1200,
        "request_rate": 800,
        "baseline_request_rate": 400
      }
    },
    {
      "timestamp": 1641018002000L,
      "service": "order-service",
      "metrics": {
        "error_rate": 0.02,
        "response_time_p95": 600,
        "request_rate": 2500,
        "baseline_request_rate": 800
      }
    }
  ]
  
  // 验证实时指标
  assert_eq(real_time_metrics.length(), 3)
  
  // 执行实时分析
  let mut analytics_results = []
  
  for metrics in real_time_metrics {
    let timestamp = metrics["timestamp"]
    let service = metrics["service"]
    let current_metrics = metrics["metrics"]
    
    // 检查每个规则
    let mut triggered_alerts = []
    
    for rule in analytics_rules {
      if not rule["enabled"] {
        continue
      }
      
      let rule_id = rule["rule_id"]
      let condition = rule["condition"]
      let severity = rule["severity"]
      
      // 简化的条件评估
      let mut triggered = false
      let mut actual_value = 0.0
      
      match rule_id {
        "rule_001" => {
          actual_value = current_metrics["error_rate"]
          triggered = actual_value > 0.05
        }
        "rule_002" => {
          actual_value = current_metrics["response_time_p95"]
          triggered = actual_value > 1000
        }
        "rule_003" => {
          let request_rate = current_metrics["request_rate"]
          let baseline_rate = current_metrics["baseline_request_rate"]
          actual_value = request_rate.to_double() / baseline_rate.to_double()
          triggered = actual_value > 2.0
        }
        _ => ()
      }
      
      if triggered {
        triggered_alerts.push((
          rule_id,
          rule["name"],
          severity,
          condition,
          actual_value
        ))
      }
    }
    
    analytics_results.push((
      timestamp,
      service,
      triggered_alerts
    ))
  }
  
  // 验证分析结果
  assert_eq(analytics_results.length(), 3)
  
  // 验证第一个服务分析结果（API网关）
  let gateway_analysis = analytics_results[0]
  assert_eq(gateway_analysis.1, "api-gateway")
  assert_eq(gateway_analysis.2.length(), 2)  // 触发2个告警
  
  // 验证错误率告警
  let error_alert = gateway_analysis.2[0]
  assert_eq(error_alert.0, "rule_001")
  assert_eq(error_alert.1, "Error Rate Alert")
  assert_eq(error_alert.2, "warning")
  assert_eq(error_alert.4, 0.08)
  
  // 验证流量激增告警
  let traffic_alert = gateway_analysis.2[1]
  assert_eq(traffic_alert.0, "rule_003")
  assert_eq(traffic_alert.1, "Traffic Spike Detection")
  assert_eq(traffic_alert.2, "info")
  assert_eq(traffic_alert.4, 2.4)  // 1200/500
  
  // 验证第二个服务分析结果（用户服务）
  let user_service_analysis = analytics_results[1]
  assert_eq(user_service_analysis.1, "user-service")
  assert_eq(user_service_analysis.2.length(), 2)  // 触发2个告警
  
  // 验证高延迟告警
  let latency_alert = user_service_analysis.2[0]
  assert_eq(latency_alert.0, "rule_002")
  assert_eq(latency_alert.1, "High Latency Alert")
  assert_eq(latency_alert.2, "critical")
  assert_eq(latency_alert.4, 1200)
  
  // 验证第三个服务分析结果（订单服务）
  let order_service_analysis = analytics_results[2]
  assert_eq(order_service_analysis.1, "order-service")
  assert_eq(order_service_analysis.2.length(), 1)  // 触发1个告警
  
  // 统计告警严重性分布
  let mut alert_severity_counts = []
  
  for result in analytics_results {
    for alert in result.2 {
      let severity = alert.2
      
      // 查找严重性计数
      let mut found_severity = false
      let mut i = 0
      while i < alert_severity_counts.length() {
        if alert_severity_counts[i].0 == severity {
          alert_severity_counts[i] = (alert_severity_counts[i].0, alert_severity_counts[i].1 + 1)
          found_severity = true
          break
        }
        i = i + 1
      }
      
      if not found_severity {
        alert_severity_counts.push((severity, 1))
      }
    }
  }
  
  // 验证告警严重性分布
  assert_eq(alert_severity_counts.length(), 3)
  
  // 验证严重性计数
  let critical_count = 0
  let warning_count = 0
  let info_count = 0
  
  for severity_count in alert_severity_counts {
    match severity_count.0 {
      "critical" => assert_eq(severity_count.1, 1)
      "warning" => assert_eq(severity_count.1, 1)
      "info" => assert_eq(severity_count.1, 1)
      _ => ()
    }
  }
  
  // 生成实时分析报告
  let analytics_report = {
    "total_rules": analytics_rules.length(),
    "enabled_rules": 3,
    "services_analyzed": real_time_metrics.length(),
    "total_alerts_triggered": 5,
    "critical_alerts": 1,
    "warning_alerts": 1,
    "info_alerts": 1,
    "analysis_latency_ms": 100,  // 模拟分析延迟
    "overall_system_health": "degraded"
  }
  
  // 验证分析报告
  assert_eq(analytics_report["total_rules"], 3)
  assert_eq(analytics_report["enabled_rules"], 3)
  assert_eq(analytics_report["services_analyzed"], 3)
  assert_eq(analytics_report["total_alerts_triggered"], 5)
  assert_eq(analytics_report["critical_alerts"], 1)
  assert_eq(analytics_report["warning_alerts"], 1)
  assert_eq(analytics_report["info_alerts"], 1)
  assert_eq(analytics_report["overall_system_health"], "degraded")
}

test "stream_data_sinks" {
  // 测试流数据输出
  
  let data_sinks = [
    {
      "sink_id": "sink_001",
      "sink_type": "elasticsearch",
      "endpoint": "http://elasticsearch:9200",
      "index_pattern": "telemetry-traces-{YYYY.MM.DD}",
      "batch_size": 1000,
      "flush_interval_sec": 10,
      "retry_policy": "exponential_backoff",
      "status": "active"
    },
    {
      "sink_id": "sink_002",
      "sink_type": "influxdb",
      "endpoint": "http://influxdb:8086",
      "database": "telemetry_metrics",
      "retention_policy": "autogen",
      "batch_size": 500,
      "flush_interval_sec": 5,
      "retry_policy": "fixed_delay",
      "status": "active"
    },
    {
      "sink_id": "sink_003",
      "sink_type": "kafka",
      "endpoint": "kafka:9092",
      "topic": "telemetry-alerts",
      "partition_count": 3,
      "replication_factor": 2,
      "batch_size": 100,
      "flush_interval_sec": 1,
      "retry_policy": "exponential_backoff",
      "status": "active"
    }
  ]
  
  // 验证数据输出
  assert_eq(data_sinks.length(), 3)
  
  let sink_metrics = [
    {
      "sink_id": "sink_001",
      "timestamp": 1641018000000L,
      "metrics": {
        "records_sent": 9850,
        "records_acknowledged": 9800,
        "records_failed": 50,
        "avg_send_time_ms": 25,
        "throughput_rec_per_sec": 985,
        "queue_depth": 150
      }
    },
    {
      "sink_id": "sink_002",
      "timestamp": 1641018000000L,
      "metrics": {
        "records_sent": 1250,
        "records_acknowledged": 1245,
        "records_failed": 5,
        "avg_send_time_ms": 15,
        "throughput_rec_per_sec": 125,
        "queue_depth": 25
      }
    },
    {
      "sink_id": "sink_003",
      "timestamp": 1641018000000L,
      "metrics": {
        "records_sent": 5,
        "records_acknowledged": 5,
        "records_failed": 0,
        "avg_send_time_ms": 5,
        "throughput_rec_per_sec": 0.5,
        "queue_depth": 0
      }
    }
  ]
  
  // 验证输出指标
  assert_eq(sink_metrics.length(), 3)
  
  // 分析输出性能
  let mut sink_performance = []
  
  for metrics in sink_metrics {
    let sink_id = metrics["sink_id"]
    let current_metrics = metrics["metrics"]
    
    // 计算成功率
    let records_sent = current_metrics["records_sent"]
    let records_acknowledged = current_metrics["records_acknowledged"]
    let records_failed = current_metrics["records_failed"]
    
    let success_rate = records_acknowledged.to_double() / records_sent.to_double() * 100.0
    let failure_rate = records_failed.to_double() / records_sent.to_double() * 100.0
    
    // 计算延迟指标
    let avg_send_time = current_metrics["avg_send_time_ms"]
    let throughput = current_metrics["throughput_rec_per_sec"]
    let queue_depth = current_metrics["queue_depth"]
    
    // 确定健康状态
    let mut health_status = "healthy"
    if success_rate < 95.0 or queue_depth > 1000 {
      health_status = "warning"
    }
    if success_rate < 90.0 or queue_depth > 5000 {
      health_status = "critical"
    }
    
    sink_performance.push((
      sink_id,
      success_rate,
      failure_rate,
      avg_send_time,
      throughput,
      queue_depth,
      health_status
    ))
  }
  
  // 验证输出性能
  assert_eq(sink_performance.length(), 3)
  
  // 验证第一个输出性能
  let sink_001_perf = sink_performance[0]
  assert_eq(sink_001_perf.0, "sink_001")
  assert_eq(sink_001_perf.1, 99.49)  // 9800/9850 * 100
  assert_eq(sink_001_perf.2, 0.51)   // 50/9850 * 100
  assert_eq(sink_001_perf.3, 25)
  assert_eq(sink_001_perf.4, 985)
  assert_eq(sink_001_perf.5, 150)
  assert_eq(sink_001_perf.6, "healthy")
  
  // 验证第二个输出性能
  let sink_002_perf = sink_performance[1]
  assert_eq(sink_002_perf.0, "sink_002")
  assert_eq(sink_002_perf.1, 99.6)   // 1245/1250 * 100
  assert_eq(sink_002_perf.2, 0.4)    // 5/1250 * 100
  assert_eq(sink_002_perf.3, 15)
  assert_eq(sink_002_perf.4, 125)
  assert_eq(sink_002_perf.5, 25)
  assert_eq(sink_002_perf.6, "healthy")
  
  // 验证第三个输出性能
  let sink_003_perf = sink_performance[2]
  assert_eq(sink_003_perf.0, "sink_003")
  assert_eq(sink_003_perf.1, 100.0)  // 5/5 * 100
  assert_eq(sink_003_perf.2, 0.0)    // 0/5 * 100
  assert_eq(sink_003_perf.3, 5)
  assert_eq(sink_003_perf.4, 0.5)
  assert_eq(sink_003_perf.5, 0)
  assert_eq(sink_003_perf.6, "healthy")
  
  // 按输出类型分组
  let mut sinks_by_type = []
  
  for sink in data_sinks {
    let sink_type = sink["sink_type"]
    
    // 查找类型分组
    let mut found_type = false
    let mut i = 0
    while i < sinks_by_type.length() {
      if sinks_by_type[i].0 == sink_type {
        sinks_by_type[i] = (sinks_by_type[i].0, sinks_by_type[i].1 + 1)
        found_type = true
        break
      }
      i = i + 1
    }
    
    if not found_type {
      sinks_by_type.push((sink_type, 1))
    }
  }
  
  // 验证输出类型分组
  assert_eq(sinks_by_type.length(), 3)
  
  // 验证输出类型计数
  let elasticsearch_count = 0
  let influxdb_count = 0
  let kafka_count = 0
  
  for type_count in sinks_by_type {
    match type_count.0 {
      "elasticsearch" => assert_eq(type_count.1, 1)
      "influxdb" => assert_eq(type_count.1, 1)
      "kafka" => assert_eq(type_count.1, 1)
      _ => ()
    }
  }
  
  // 计算总体输出统计
  let mut total_sent = 0
  let mut total_acknowledged = 0
  let mut total_failed = 0
  let mut total_queue_depth = 0
  
  for metrics in sink_metrics {
    let current_metrics = metrics["metrics"]
    total_sent = total_sent + current_metrics["records_sent"]
    total_acknowledged = total_acknowledged + current_metrics["records_acknowledged"]
    total_failed = total_failed + current_metrics["records_failed"]
    total_queue_depth = total_queue_depth + current_metrics["queue_depth"]
  }
  
  // 验证总体统计
  assert_eq(total_sent, 11105)     // 9850 + 1250 + 5
  assert_eq(total_acknowledged, 11050) // 9800 + 1245 + 5
  assert_eq(total_failed, 55)      // 50 + 5 + 0
  assert_eq(total_queue_depth, 175) // 150 + 25 + 0
  
  let overall_success_rate = total_acknowledged.to_double() / total_sent.to_double() * 100.0
  assert_eq(overall_success_rate, 99.5) // 11050/11105 * 100
  
  // 生成输出报告
  let sink_report = {
    "total_sinks": data_sinks.length(),
    "active_sinks": 3,
    "sink_types": sinks_by_type.length(),
    "overall_success_rate": overall_success_rate,
    "total_records_sent": total_sent,
    "total_records_failed": total_failed,
    "total_queue_depth": total_queue_depth,
    "overall_sink_health": "healthy"
  }
  
  // 验证输出报告
  assert_eq(sink_report["total_sinks"], 3)
  assert_eq(sink_report["active_sinks"], 3)
  assert_eq(sink_report["sink_types"], 3)
  assert_eq(sink_report["overall_success_rate"], 99.5)
  assert_eq(sink_report["total_records_sent"], 11105)
  assert_eq(sink_report["total_records_failed"], 55)
  assert_eq(sink_report["total_queue_depth"], 175)
  assert_eq(sink_report["overall_sink_health"], "healthy")
}