// Azimuth Telemetry Configuration Dynamic Update Tests
// 遥测配置动态更新测试用例，专注于运行时配置变更和热更新机制

test "采样率动态调整测试" {
  // 模拟初始采样配置
  let initial_sampling_config = {
    "sampling_strategy": "probabilistic",
    "sampling_probability": 0.1, // 10%采样率
    "default_decision": "drop",
    "update_timestamp": 1640995200000L
  }
  
  // 模拟采样决策函数
  let make_sampling_decision = |config: Map[String, Any], trace_id: String| {
    match config["sampling_strategy"] {
      "probabilistic" => {
        let probability = config["sampling_probability"].to_double()
        let hash = trace_id.hash() % 10000
        let normalized_hash = hash.to_double() / 10000.0
        if normalized_hash < probability {
          "record_and_sample"
        } else {
          "drop"
        }
      },
      "always_on" => "record_and_sample",
      "always_off" => "drop",
      _ => config["default_decision"]
    }
  }
  
  // 测试初始采样率
  let trace_ids = []
  for i = 0; i < 10000; i = i + 1 {
    trace_ids.push("trace_" + i.to_string())
  }
  
  let initial_sampled_count = 0
  for trace_id in trace_ids {
    let decision = make_sampling_decision(initial_sampling_config, trace_id)
    if decision == "record_and_sample" {
      initial_sampled_count = initial_sampled_count + 1
    }
  }
  
  // 验证初始采样率接近10%
  let initial_sampling_rate = initial_sampled_count.to_double() / trace_ids.length().to_double()
  assert_true(initial_sampling_rate >= 0.09 && initial_sampling_rate <= 0.11)
  
  // 动态更新采样配置 - 提高采样率到50%
  let updated_sampling_config = {
    "sampling_strategy": "probabilistic",
    "sampling_probability": 0.5, // 50%采样率
    "default_decision": "drop",
    "update_timestamp": 1640995300000L
  }
  
  // 测试更新后的采样率
  let updated_sampled_count = 0
  for trace_id in trace_ids {
    let decision = make_sampling_decision(updated_sampling_config, trace_id)
    if decision == "record_and_sample" {
      updated_sampled_count = updated_sampled_count + 1
    }
  }
  
  // 验证更新后的采样率接近50%
  let updated_sampling_rate = updated_sampled_count.to_double() / trace_ids.length().to_double()
  assert_true(updated_sampling_rate >= 0.48 && updated_sampling_rate <= 0.52)
  
  // 验证采样率确实提高了
  assert_true(updated_sampling_rate > initial_sampling_rate)
  
  // 再次动态更新 - 降低采样率到1%
  let low_sampling_config = {
    "sampling_strategy": "probabilistic",
    "sampling_probability": 0.01, // 1%采样率
    "default_decision": "drop",
    "update_timestamp": 1640995400000L
  }
  
  // 测试低采样率
  let low_sampled_count = 0
  for trace_id in trace_ids {
    let decision = make_sampling_decision(low_sampling_config, trace_id)
    if decision == "record_and_sample" {
      low_sampled_count = low_sampled_count + 1
    }
  }
  
  // 验证低采样率接近1%
  let low_sampling_rate = low_sampled_count.to_double() / trace_ids.length().to_double()
  assert_true(low_sampling_rate >= 0.005 && low_sampling_rate <= 0.015)
  
  // 验证采样率确实降低了
  assert_true(low_sampling_rate < updated_sampling_rate)
  
  // 动态切换采样策略
  let always_on_config = {
    "sampling_strategy": "always_on",
    "sampling_probability": 0.0, // 不使用概率采样
    "default_decision": "record_and_sample",
    "update_timestamp": 1640995500000L
  }
  
  // 测试始终采样策略
  let always_on_sampled_count = 0
  for trace_id in trace_ids {
    let decision = make_sampling_decision(always_on_config, trace_id)
    if decision == "record_and_sample" {
      always_on_sampled_count = always_on_sampled_count + 1
    }
  }
  
  // 验证始终采样策略下所有请求都被采样
  assert_eq(always_on_sampled_count, trace_ids.length())
  
  assert_true(true)
}

test "遥测指标动态配置测试" {
  // 模拟初始指标配置
  let initial_metrics_config = {
    "enabled_metrics": [
      {"name": "http.requests", "type": "counter", "enabled": true},
      {"name": "http.duration", "type": "histogram", "enabled": true},
      {"name": "cpu.usage", "type": "gauge", "enabled": true},
      {"name": "memory.usage", "type": "gauge", "enabled": true},
      {"name": "db.connections", "type": "updowncounter", "enabled": false},
      {"name": "cache.hits", "type": "counter", "enabled": false}
    ],
    "collection_interval_ms": 10000, // 10秒
    "export_interval_ms": 60000, // 1分钟
    "update_timestamp": 1640995200000L
  }
  
  // 模拟指标收集器
  let collect_metrics = |config: Map[String, Any]| {
    let enabled_metrics = config["enabled_metrics"].filter(|metric| metric["enabled"])
    let collection_interval = config["collection_interval_ms"]
    
    let collected_metrics = []
    for metric in enabled_metrics {
      let metric_data = {
        "name": metric["name"],
        "type": metric["type"],
        "value": match metric["type"] {
          "counter" => 100.0,
          "histogram" => [10.0, 20.0, 30.0, 40.0, 50.0],
          "gauge" => 75.5,
          "updowncounter" => 25.0,
          _ => 0.0
        },
        "timestamp": 1640995200000L,
        "collection_interval": collection_interval
      }
      collected_metrics.push(metric_data)
    }
    collected_metrics
  }
  
  // 测试初始指标收集
  let initial_collected_metrics = collect_metrics(initial_metrics_config)
  
  // 验证初始收集的指标
  assert_eq(initial_collected_metrics.length(), 4) // 只有4个启用的指标
  assert_true(initial_collected_metrics.map(|m| m["name"]).contains("http.requests"))
  assert_true(initial_collected_metrics.map(|m| m["name"]).contains("http.duration"))
  assert_true(initial_collected_metrics.map(|m| m["name"]).contains("cpu.usage"))
  assert_true(initial_collected_metrics.map(|m| m["name"]).contains("memory.usage"))
  assert_false(initial_collected_metrics.map(|m| m["name"]).contains("db.connections"))
  assert_false(initial_collected_metrics.map(|m| m["name"]).contains("cache.hits"))
  
  // 动态更新指标配置 - 启用更多指标
  let updated_metrics_config = {
    "enabled_metrics": [
      {"name": "http.requests", "type": "counter", "enabled": true},
      {"name": "http.duration", "type": "histogram", "enabled": true},
      {"name": "cpu.usage", "type": "gauge", "enabled": true},
      {"name": "memory.usage", "type": "gauge", "enabled": true},
      {"name": "db.connections", "type": "updowncounter", "enabled": true}, // 启用
      {"name": "cache.hits", "type": "counter", "enabled": true} // 启用
    ],
    "collection_interval_ms": 10000,
    "export_interval_ms": 60000,
    "update_timestamp": 1640995300000L
  }
  
  // 测试更新后的指标收集
  let updated_collected_metrics = collect_metrics(updated_metrics_config)
  
  // 验证更新后收集的指标
  assert_eq(updated_collected_metrics.length(), 6) // 现在有6个启用的指标
  assert_true(updated_collected_metrics.map(|m| m["name"]).contains("http.requests"))
  assert_true(updated_collected_metrics.map(|m| m["name"]).contains("http.duration"))
  assert_true(updated_collected_metrics.map(|m| m["name"]).contains("cpu.usage"))
  assert_true(updated_collected_metrics.map(|m| m["name"]).contains("memory.usage"))
  assert_true(updated_collected_metrics.map(|m| m["name"]).contains("db.connections"))
  assert_true(updated_collected_metrics.map(|m| m["name"]).contains("cache.hits"))
  
  // 动态更新收集间隔
  let interval_updated_config = {
    "enabled_metrics": updated_metrics_config["enabled_metrics"],
    "collection_interval_ms": 5000, // 减少到5秒
    "export_interval_ms": 30000, // 减少到30秒
    "update_timestamp": 1640995400000L
  }
  
  // 测试更新收集间隔后的指标收集
  let interval_updated_metrics = collect_metrics(interval_updated_config)
  
  // 验证收集间隔已更新
  assert_eq(interval_updated_metrics.length(), 6) // 指标数量不变
  for metric in interval_updated_metrics {
    assert_eq(metric["collection_interval"], 5000)
  }
  
  // 动态禁用某些指标
  let reduced_metrics_config = {
    "enabled_metrics": [
      {"name": "http.requests", "type": "counter", "enabled": true},
      {"name": "http.duration", "type": "histogram", "enabled": true},
      {"name": "cpu.usage", "type": "gauge", "enabled": false}, // 禁用
      {"name": "memory.usage", "type": "gauge", "enabled": false}, // 禁用
      {"name": "db.connections", "type": "updowncounter", "enabled": true},
      {"name": "cache.hits", "type": "counter", "enabled": true}
    ],
    "collection_interval_ms": 5000,
    "export_interval_ms": 30000,
    "update_timestamp": 1640995500000L
  }
  
  // 测试减少指标后的收集
  let reduced_collected_metrics = collect_metrics(reduced_metrics_config)
  
  // 验证减少后的指标
  assert_eq(reduced_collected_metrics.length(), 4) // 只有4个启用的指标
  assert_true(reduced_collected_metrics.map(|m| m["name"]).contains("http.requests"))
  assert_true(reduced_collected_metrics.map(|m| m["name"]).contains("http.duration"))
  assert_false(reduced_collected_metrics.map(|m| m["name"]).contains("cpu.usage"))
  assert_false(reduced_collected_metrics.map(|m| m["name"]).contains("memory.usage"))
  assert_true(reduced_collected_metrics.map(|m| m["name"]).contains("db.connections"))
  assert_true(reduced_collected_metrics.map(|m| m["name"]).contains("cache.hits"))
  
  assert_true(true)
}

test "日志级别动态调整测试" {
  // 模拟初始日志配置
  let initial_logging_config = {
    "global_level": "INFO",
    "logger_levels": {
      "auth.service": "DEBUG",
      "payment.service": "WARN",
      "database": "ERROR"
    },
    "update_timestamp": 1640995200000L
  }
  
  // 模拟日志级别优先级
  let level_priority = {
    "TRACE": 0,
    "DEBUG": 1,
    "INFO": 2,
    "WARN": 3,
    "ERROR": 4,
    "FATAL": 5
  }
  
  // 模拟日志记录函数
  let should_log = |config: Map[String, Any], logger_name: String, level: String| {
    let logger_level = config["logger_levels"][logger_name]
    let effective_level = if logger_level is Some { logger_level } else { config["global_level"] }
    
    level_priority[level] >= level_priority[effective_level]
  }
  
  // 测试初始日志级别配置
  let test_log_entries = [
    ("auth.service", "TRACE"),
    ("auth.service", "DEBUG"),
    ("auth.service", "INFO"),
    ("auth.service", "WARN"),
    ("auth.service", "ERROR"),
    ("payment.service", "DEBUG"),
    ("payment.service", "INFO"),
    ("payment.service", "WARN"),
    ("payment.service", "ERROR"),
    ("database", "INFO"),
    ("database", "WARN"),
    ("database", "ERROR"),
    ("unknown.service", "DEBUG"),
    ("unknown.service", "INFO"),
    ("unknown.service", "WARN")
  ]
  
  // 验证初始日志级别
  assert_true(should_log(initial_logging_config, "auth.service", "DEBUG"))
  assert_true(should_log(initial_logging_config, "auth.service", "INFO"))
  assert_true(should_log(initial_logging_config, "auth.service", "WARN"))
  assert_true(should_log(initial_logging_config, "auth.service", "ERROR"))
  assert_false(should_log(initial_logging_config, "auth.service", "TRACE"))
  
  assert_false(should_log(initial_logging_config, "payment.service", "DEBUG"))
  assert_false(should_log(initial_logging_config, "payment.service", "INFO"))
  assert_true(should_log(initial_logging_config, "payment.service", "WARN"))
  assert_true(should_log(initial_logging_config, "payment.service", "ERROR"))
  
  assert_false(should_log(initial_logging_config, "database", "INFO"))
  assert_false(should_log(initial_logging_config, "database", "WARN"))
  assert_true(should_log(initial_logging_config, "database", "ERROR"))
  
  assert_false(should_log(initial_logging_config, "unknown.service", "DEBUG"))
  assert_true(should_log(initial_logging_config, "unknown.service", "INFO"))
  assert_true(should_log(initial_logging_config, "unknown.service", "WARN"))
  
  // 动态更新全局日志级别
  let updated_global_logging_config = {
    "global_level": "DEBUG", // 降低到DEBUG
    "logger_levels": {
      "auth.service": "DEBUG",
      "payment.service": "WARN",
      "database": "ERROR"
    },
    "update_timestamp": 1640995300000L
  }
  
  // 验证更新后的全局日志级别
  assert_false(should_log(updated_global_logging_config, "unknown.service", "TRACE"))
  assert_true(should_log(updated_global_logging_config, "unknown.service", "DEBUG"))
  assert_true(should_log(updated_global_logging_config, "unknown.service", "INFO"))
  assert_true(should_log(updated_global_logging_config, "unknown.service", "WARN"))
  
  // 特定logger的级别应该不受影响
  assert_false(should_log(updated_global_logging_config, "auth.service", "TRACE"))
  assert_true(should_log(updated_global_logging_config, "auth.service", "DEBUG"))
  assert_false(should_log(updated_global_logging_config, "payment.service", "DEBUG"))
  assert_false(should_log(updated_global_logging_config, "database", "INFO"))
  
  // 动态更新特定logger的日志级别
  let updated_logger_logging_config = {
    "global_level": "DEBUG",
    "logger_levels": {
      "auth.service": "TRACE", // 降低到TRACE
      "payment.service": "INFO", // 降低到INFO
      "database": "WARN" // 降低到WARN
    },
    "update_timestamp": 1640995400000L
  }
  
  // 验证更新后的特定logger日志级别
  assert_true(should_log(updated_logger_logging_config, "auth.service", "TRACE"))
  assert_true(should_log(updated_logger_logging_config, "auth.service", "DEBUG"))
  
  assert_true(should_log(updated_logger_logging_config, "payment.service", "INFO"))
  assert_true(should_log(updated_logger_logging_config, "payment.service", "WARN"))
  
  assert_false(should_log(updated_logger_logging_config, "database", "INFO"))
  assert_true(should_log(updated_logger_logging_config, "database", "WARN"))
  assert_true(should_log(updated_logger_logging_config, "database", "ERROR"))
  
  // 动态添加新的logger配置
  let added_logger_logging_config = {
    "global_level": "INFO", // 提高全局级别
    "logger_levels": {
      "auth.service": "TRACE",
      "payment.service": "INFO",
      "database": "WARN",
      "cache.service": "DEBUG", // 新增
      "notification.service": "ERROR" // 新增
    },
    "update_timestamp": 1640995500000L
  }
  
  // 验证新增logger的日志级别
  assert_true(should_log(added_logger_logging_config, "cache.service", "DEBUG"))
  assert_true(should_log(added_logger_logging_config, "cache.service", "INFO"))
  assert_false(should_log(added_logger_logging_config, "cache.service", "TRACE"))
  
  assert_false(should_log(added_logger_logging_config, "notification.service", "WARN"))
  assert_true(should_log(added_logger_logging_config, "notification.service", "ERROR"))
  assert_true(should_log(added_logger_logging_config, "notification.service", "FATAL"))
  
  // 验证未配置的logger使用全局级别
  assert_false(should_log(added_logger_logging_config, "new.service", "DEBUG"))
  assert_true(should_log(added_logger_logging_config, "new.service", "INFO"))
  assert_true(should_log(added_logger_logging_config, "new.service", "WARN"))
  
  assert_true(true)
}

test "资源属性动态更新测试" {
  // 模拟初始资源属性配置
  let initial_resource_config = {
    "resource_attributes": {
      "service.name": "payment-service",
      "service.version": "1.0.0",
      "service.namespace": "production",
      "deployment.environment": "prod",
      "host.name": "payment-server-01",
      "host.ip": "192.168.1.100",
      "process.id": "12345",
      "process.executable.name": "payment-service"
    },
    "update_timestamp": 1640995200000L
  }
  
  // 模拟资源合并函数
  let merge_resource_attributes = |base_config: Map[String, Any], updates: Map[String, Any]| {
    let base_attributes = base_config["resource_attributes"]
    let merged_attributes = {}
    
    // 复制基础属性
    for (key, value) in base_attributes {
      merged_attributes[key] = value
    }
    
    // 应用更新
    for (key, value) in updates {
      merged_attributes[key] = value
    }
    
    merged_attributes
  }
  
  // 验证初始资源属性
  let initial_attributes = initial_resource_config["resource_attributes"]
  assert_eq(initial_attributes["service.name"], "payment-service")
  assert_eq(initial_attributes["service.version"], "1.0.0")
  assert_eq(initial_attributes["deployment.environment"], "prod")
  assert_eq(initial_attributes["host.name"], "payment-server-01")
  
  // 动态更新部分资源属性
  let version_update = {
    "service.version": "1.1.0",
    "deployment.environment": "staging",
    "host.ip": "192.168.1.101"
  }
  
  let updated_attributes = merge_resource_attributes(initial_resource_config, version_update)
  
  // 验证更新后的资源属性
  assert_eq(updated_attributes["service.name"], "payment-service") // 未更新
  assert_eq(updated_attributes["service.version"], "1.1.0") // 已更新
  assert_eq(updated_attributes["deployment.environment"], "staging") // 已更新
  assert_eq(updated_attributes["host.name"], "payment-server-01") // 未更新
  assert_eq(updated_attributes["host.ip"], "192.168.1.101") // 已更新
  
  // 添加新的资源属性
  let new_attributes = {
    "service.instance.id": "instance-abc123",
    "cloud.provider": "aws",
    "cloud.region": "us-west-2",
    "cloud.availability_zone": "us-west-2a"
  }
  
  let merged_attributes = merge_resource_attributes(
    {"resource_attributes": updated_attributes, "update_timestamp": 1640995300000L},
    new_attributes
  )
  
  // 验证添加新属性后的资源属性
  assert_eq(merged_attributes["service.instance.id"], "instance-abc123")
  assert_eq(merged_attributes["cloud.provider"], "aws")
  assert_eq(merged_attributes["cloud.region"], "us-west-2")
  assert_eq(merged_attributes["cloud.availability_zone"], "us-west-2a")
  
  // 验证原有属性仍然存在
  assert_eq(merged_attributes["service.name"], "payment-service")
  assert_eq(merged_attributes["service.version"], "1.1.0")
  
  // 批量更新资源属性
  let batch_update = {
    "service.name": "payment-service-v2",
    "service.version": "2.0.0",
    "deployment.environment": "prod",
    "host.name": "payment-server-02",
    "host.ip": "10.0.0.50",
    "process.id": "54321",
    "k8s.pod.name": "payment-service-7d4f8c9b-xyz",
    "k8s.namespace": "payment",
    "k8s.node.name": "worker-node-03"
  }
  
  let batch_updated_attributes = merge_resource_attributes(
    {"resource_attributes": merged_attributes, "update_timestamp": 1640995400000L},
    batch_update
  )
  
  // 验证批量更新后的资源属性
  assert_eq(batch_updated_attributes["service.name"], "payment-service-v2")
  assert_eq(batch_updated_attributes["service.version"], "2.0.0")
  assert_eq(batch_updated_attributes["deployment.environment"], "prod")
  assert_eq(batch_updated_attributes["host.name"], "payment-server-02")
  assert_eq(batch_updated_attributes["k8s.pod.name"], "payment-service-7d4f8c9b-xyz")
  
  // 验证未覆盖的云属性仍然存在
  assert_eq(batch_updated_attributes["cloud.provider"], "aws")
  assert_eq(batch_updated_attributes["cloud.region"], "us-west-2")
  assert_eq(batch_updated_attributes["service.instance.id"], "instance-abc123")
  
  // 验证资源属性数量
  assert_true(batch_updated_attributes.length() >= 12) // 至少应该有12个属性
  
  assert_true(true)
}

test "导出器配置动态切换测试" {
  // 模拟初始导出器配置
  let initial_exporter_config = {
    "exporters": [
      {
        "name": "otlp",
        "type": "otlp",
        "enabled": true,
        "endpoint": "http://otel-collector:4317",
        "timeout_ms": 5000,
        "retry_count": 3,
        "batch_size": 512
      },
      {
        "name": "prometheus",
        "type": "prometheus",
        "enabled": true,
        "port": 9090,
        "path": "/metrics"
      },
      {
        "name": "jaeger",
        "type": "jaeger",
        "enabled": false,
        "endpoint": "http://jaeger:14268/api/traces"
      },
      {
        "name": "zipkin",
        "type": "zipkin",
        "enabled": false,
        "endpoint": "http://zipkin:9411/api/v2/spans"
      }
    ],
    "update_timestamp": 1640995200000L
  }
  
  // 模拟导出器管理函数
  let get_enabled_exporters = |config: Map[String, Any]| {
    config["exporters"].filter(|exporter| exporter["enabled"])
  }
  
  let update_exporter = |config: Map[String, Any], exporter_name: String, updates: Map[String, Any]| {
    let updated_exporters = []
    
    for exporter in config["exporters"] {
      if exporter["name"] == exporter_name {
        let updated_exporter = {}
        
        // 复制原有属性
        for (key, value) in exporter {
          updated_exporter[key] = value
        }
        
        // 应用更新
        for (key, value) in updates {
          updated_exporter[key] = value
        }
        
        updated_exporters.push(updated_exporter)
      } else {
        updated_exporters.push(exporter)
      }
    }
    
    updated_exporters
  }
  
  // 验证初始导出器配置
  let initial_enabled_exporters = get_enabled_exporters(initial_exporter_config)
  assert_eq(initial_enabled_exporters.length(), 2) // 只有2个启用的导出器
  assert_true(initial_enabled_exporters.map(|e| e["name"]).contains("otlp"))
  assert_true(initial_enabled_exporters.map(|e| e["name"]).contains("prometheus"))
  assert_false(initial_enabled_exporters.map(|e| e["name"]).contains("jaeger"))
  assert_false(initial_enabled_exporters.map(|e| e["name"]).contains("zipkin"))
  
  // 动态启用jaeger导出器
  let enable_jaeger_updates = {"enabled": true}
  let updated_exporters = update_exporter(initial_exporter_config, "jaeger", enable_jaeger_updates)
  
  let updated_config = {
    "exporters": updated_exporters,
    "update_timestamp": 1640995300000L
  }
  
  // 验证jaeger导出器已启用
  let enabled_after_jaeger = get_enabled_exporters(updated_config)
  assert_eq(enabled_after_jaeger.length(), 3) // 现在有3个启用的导出器
  assert_true(enabled_after_jaeger.map(|e| e["name"]).contains("jaeger"))
  
  // 验证jaeger导出器的配置
  let jaeger_exporter = enabled_after_jaeger.find(|e| e["name"] == "jaeger")
  assert_true(jaeger_exporter is Some)
  assert_eq(jaeger_exporter["endpoint"], "http://jaeger:14268/api/traces")
  
  // 动态更新otlp导出器配置
  let otlp_updates = {
    "endpoint": "http://new-otel-collector:4317",
    "timeout_ms": 10000,
    "batch_size": 1024
  }
  
  let updated_with_otlp = update_exporter(updated_config, "otlp", otlp_updates)
  let otlp_updated_config = {
    "exporters": updated_with_otlp,
    "update_timestamp": 1640995400000L
  }
  
  // 验证otlp导出器配置已更新
  let otlp_exporter = otlp_updated_config["exporters"].find(|e| e["name"] == "otlp")
  assert_eq(otlp_exporter["endpoint"], "http://new-otel-collector:4317")
  assert_eq(otlp_exporter["timeout_ms"], 10000)
  assert_eq(otlp_exporter["batch_size"], 1024)
  
  // 动态禁用prometheus导出器
  let disable_prometheus_updates = {"enabled": false}
  let updated_with_prometheus_disabled = update_exporter(otlp_updated_config, "prometheus", disable_prometheus_updates)
  let prometheus_disabled_config = {
    "exporters": updated_with_prometheus_disabled,
    "update_timestamp": 1640995500000L
  }
  
  // 验证prometheus导出器已禁用
  let enabled_after_prometheus_disabled = get_enabled_exporters(prometheus_disabled_config)
  assert_eq(enabled_after_prometheus_disabled.length(), 2) // 回到2个启用的导出器
  assert_true(enabled_after_prometheus_disabled.map(|e| e["name"]).contains("otlp"))
  assert_true(enabled_after_prometheus_disabled.map(|e| e["name"]).contains("jaeger"))
  assert_false(enabled_after_prometheus_disabled.map(|e| e["name"]).contains("prometheus"))
  
  // 验证prometheus导出器仍然存在但已禁用
  let prometheus_exporter = prometheus_disabled_config["exporters"].find(|e| e["name"] == "prometheus")
  assert_true(prometheus_exporter is Some)
  assert_eq(prometheus_exporter["enabled"], false)
  
  // 动态启用zipkin导出器并更新配置
  let zipkin_updates = {
    "enabled": true,
    "endpoint": "http://new-zipkin:9411/api/v2/spans",
    "timeout_ms": 3000
  }
  
  let updated_with_zipkin = update_exporter(prometheus_disabled_config, "zipkin", zipkin_updates)
  let zipkin_enabled_config = {
    "exporters": updated_with_zipkin,
    "update_timestamp": 1640995600000L
  }
  
  // 验证zipkin导出器已启用并配置已更新
  let enabled_after_zipkin = get_enabled_exporters(zipkin_enabled_config)
  assert_eq(enabled_after_zipkin.length(), 3) // 又回到3个启用的导出器
  assert_true(enabled_after_zipkin.map(|e| e["name"]).contains("otlp"))
  assert_true(enabled_after_zipkin.map(|e| e["name"]).contains("jaeger"))
  assert_true(enabled_after_zipkin.map(|e| e["name"]).contains("zipkin"))
  
  let zipkin_exporter = enabled_after_zipkin.find(|e| e["name"] == "zipkin")
  assert_eq(zipkin_exporter["endpoint"], "http://new-zipkin:9411/api/v2/spans")
  assert_eq(zipkin_exporter["timeout_ms"], 3000)
  
  assert_true(true)
}