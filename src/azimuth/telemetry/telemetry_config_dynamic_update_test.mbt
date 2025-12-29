// 遥测配置动态更新测试用例

test "telemetry_service_config_dynamic_update" {
  // 测试服务配置动态更新
  
  let mut config = [
    ("service.name", "payment-service"),
    ("service.version", "1.0.0"),
    ("environment", "staging"),
    ("sampling.rate", "0.1"),
    ("export.interval", "60")
  ]
  
  // 验证初始配置
  assert_eq(config.length(), 5)
  assert_eq(get_config_value(config, "service.name"), "payment-service")
  assert_eq(get_config_value(config, "environment"), "staging")
  
  // 动态更新配置
  config = update_config(config, "environment", "production")
  config = update_config(config, "sampling.rate", "0.5")
  config = update_config(config, "export.interval", "30")
  
  // 验证更新后的配置
  assert_eq(get_config_value(config, "environment"), "production")
  assert_eq(get_config_value(config, "sampling.rate"), "0.5")
  assert_eq(get_config_value(config, "export.interval"), "30")
  assert_eq(get_config_value(config, "service.name"), "payment-service") // 未改变
  assert_eq(config.length(), 5) // 配置项数量不变
}

test "telemetry_sampling_config_hot_reload" {
  // 测试采样配置热重载
  
  let mut sampling_config = [
    ("default.sampling.probability", "0.1"),
    ("trace.id.ratio", "0.01"),
    ("span.count.limit", "1000"),
    ("sampling.decision.cache.size", "500")
  ]
  
  // 验证初始采样配置
  assert_eq(get_config_value(sampling_config, "default.sampling.probability"), "0.1")
  assert_eq(get_config_value(sampling_config, "span.count.limit"), "1000")
  
  // 模拟配置文件热重载
  let new_sampling_config = [
    ("default.sampling.probability", "0.2"),
    ("trace.id.ratio", "0.02"),
    ("span.count.limit", "2000"),
    ("sampling.decision.cache.size", "1000"),
    ("new.sampling.feature", "enabled")
  ]
  
  // 应用新配置
  sampling_config = new_sampling_config
  
  // 验证热重载结果
  assert_eq(get_config_value(sampling_config, "default.sampling.probability"), "0.2")
  assert_eq(get_config_value(sampling_config, "trace.id.ratio"), "0.02")
  assert_eq(get_config_value(sampling_config, "span.count.limit"), "2000")
  assert_eq(get_config_value(sampling_config, "sampling.decision.cache.size"), "1000")
  assert_eq(get_config_value(sampling_config, "new.sampling.feature"), "enabled")
  assert_eq(sampling_config.length(), 5)
}

test "telemetry_exporter_config_dynamic_update" {
  // 测试导出器配置动态更新
  
  let mut exporter_config = [
    ("exporter.type", "otlp"),
    ("exporter.endpoint", "http://localhost:4317"),
    ("exporter.timeout", "30"),
    ("exporter.batch.size", "512"),
    ("exporter.compression", "gzip")
  ]
  
  // 验证初始导出器配置
  assert_eq(get_config_value(exporter_config, "exporter.type"), "otlp")
  assert_eq(get_config_value(exporter_config, "exporter.endpoint"), "http://localhost:4317")
  
  // 动态更新导出器配置
  exporter_config = update_config(exporter_config, "exporter.endpoint", "http://collector:4317")
  exporter_config = update_config(exporter_config, "exporter.timeout", "60")
  exporter_config = update_config(exporter_config, "exporter.batch.size", "1024")
  
  // 添加新的导出器配置
  exporter_config.push(("exporter.retry.max_attempts", "3"))
  exporter_config.push(("exporter.retry.initial_backoff", "5"))
  
  // 验证更新结果
  assert_eq(get_config_value(exporter_config, "exporter.endpoint"), "http://collector:4317")
  assert_eq(get_config_value(exporter_config, "exporter.timeout"), "60")
  assert_eq(get_config_value(exporter_config, "exporter.batch.size"), "1024")
  assert_eq(get_config_value(exporter_config, "exporter.retry.max_attempts"), "3")
  assert_eq(get_config_value(exporter_config, "exporter.compression"), "gzip") // 未改变
  assert_eq(exporter_config.length(), 7)
}

test "telemetry_resource_config_dynamic_update" {
  // 测试资源配置动态更新
  
  let mut resource_config = [
    ("resource.attributes", "service.name=api-gateway,service.version=1.2.3"),
    ("resource.detectors", "env,host,os"),
    ("resource.schema.url", "https://opentelemetry.io/schemas/v1.20.0"),
    ("resource.sync.interval", "300")
  ]
  
  // 验证初始资源配置
  assert_eq(get_config_value(resource_config, "resource.attributes"), 
            "service.name=api-gateway,service.version=1.2.3")
  
  // 动态更新资源配置
  resource_config = update_config(resource_config, "resource.attributes", 
                                 "service.name=api-gateway,service.version=1.3.0,deployment.env=prod")
  resource_config = update_config(resource_config, "resource.detectors", "env,host,os,process")
  resource_config = update_config(resource_config, "resource.sync.interval", "600")
  
  // 验证更新结果
  assert_eq(get_config_value(resource_config, "resource.attributes"), 
            "service.name=api-gateway,service.version=1.3.0,deployment.env=prod")
  assert_eq(get_config_value(resource_config, "resource.detectors"), "env,host,os,process")
  assert_eq(get_config_value(resource_config, "resource.sync.interval"), "600")
  assert_eq(get_config_value(resource_config, "resource.schema.url"), 
            "https://opentelemetry.io/schemas/v1.20.0") // 未改变
}

test "telemetry_metrics_config_dynamic_update" {
  // 测试指标配置动态更新
  
  let mut metrics_config = [
    ("metrics.enabled", "true"),
    ("metrics.export.interval", "30"),
    ("metrics.aggregation.temporality", "cumulative"),
    ("cardinality.limit", "10000"),
    ("exemplars.enabled", "false")
  ]
  
  // 验证初始指标配置
  assert_eq(get_config_value(metrics_config, "metrics.enabled"), "true")
  assert_eq(get_config_value(metrics_config, "cardinality.limit"), "10000")
  
  // 动态更新指标配置
  metrics_config = update_config(metrics_config, "metrics.export.interval", "15")
  metrics_config = update_config(metrics_config, "cardinality.limit", "20000")
  metrics_config = update_config(metrics_config, "exemplars.enabled", "true")
  
  // 添加新的指标配置
  metrics_config.push(("metrics.histogram.boundaries", "0.1,0.5,1.0,5.0,10.0"))
  metrics_config.push(("metrics.view.config", "enabled"))
  
  // 验证更新结果
  assert_eq(get_config_value(metrics_config, "metrics.export.interval"), "15")
  assert_eq(get_config_value(metrics_config, "cardinality.limit"), "20000")
  assert_eq(get_config_value(metrics_config, "exemplars.enabled"), "true")
  assert_eq(get_config_value(metrics_config, "metrics.histogram.boundaries"), "0.1,0.5,1.0,5.0,10.0")
  assert_eq(get_config_value(metrics_config, "metrics.view.config"), "enabled")
  assert_eq(metrics_config.length(), 7)
}

test "telemetry_config_validation_on_update" {
  // 测试配置更新时的验证
  
  let mut config = [
    ("sampling.rate", "0.5"),
    ("export.timeout", "30"),
    ("batch.size", "512"),
    ("retry.count", "3")
  ]
  
  // 验证有效配置更新
  config = update_config_with_validation(config, "sampling.rate", "0.8", "float_range", "0.0,1.0")
  assert_eq(get_config_value(config, "sampling.rate"), "0.8")
  
  config = update_config_with_validation(config, "export.timeout", "60", "int_range", "1,300")
  assert_eq(get_config_value(config, "export.timeout"), "60")
  
  // 验证无效配置更新（应该被拒绝或修正）
  config = update_config_with_validation(config, "sampling.rate", "1.5", "float_range", "0.0,1.0")
  assert_eq(get_config_value(config, "sampling.rate"), "1.0") // 被修正为最大值
  
  config = update_config_with_validation(config, "batch.size", "2048", "int_range", "64,1024")
  assert_eq(get_config_value(config, "batch.size"), "1024") // 被修正为最大值
  
  // 验证配置完整性
  assert_eq(config.length(), 4)
  assert_eq(get_config_value(config, "retry.count"), "3") // 未改变
}

test "telemetry_config_rollback_on_failure" {
  // 测试配置更新失败时的回滚机制
  
  let original_config = [
    ("service.name", "auth-service"),
    ("service.version", "1.0.0"),
    ("environment", "production"),
    ("export.endpoint", "http://otel-collector:4317")
  ]
  
  let mut current_config = original_config.copy()
  
  // 模拟成功的配置更新
  let backup_config = current_config.copy()
  current_config = update_config(current_config, "service.version", "1.1.0")
  
  // 验证更新成功
  assert_eq(get_config_value(current_config, "service.version"), "1.1.0")
  
  // 模拟配置更新失败，需要回滚
  current_config = backup_config.copy()
  
  // 验证回滚成功
  assert_eq(get_config_value(current_config, "service.version"), "1.0.0")
  assert_eq(current_config.length(), original_config.length())
  
  // 验证所有配置项都恢复到原始状态
  for (key, value) in original_config {
    assert_eq(get_config_value(current_config, key), value)
  }
}

// 辅助函数
fn get_config_value(config : Array[(String, String)], key : String) -> String {
  for (k, v) in config {
    if k == key {
      return v
    }
  }
  return ""
}

fn update_config(config : Array[(String, String)], key : String, value : String) -> Array[(String, String)] {
  let mut new_config = Array[(String, String)]::new()
  let mut updated = false
  
  for (k, v) in config {
    if k == key {
      new_config.push((k, value))
      updated = true
    } else {
      new_config.push((k, v))
    }
  }
  
  if not updated {
    new_config.push((key, value))
  }
  
  return new_config
}

fn update_config_with_validation(config : Array[(String, String)], key : String, value : String, 
                                validation_type : String, validation_range : String) -> Array[(String, String)] {
  let validated_value = match validation_type {
    "float_range" => {
      let parts = validation_range",".split_to_string()
      let min_val = parts[0].to_double()
      let max_val = parts[1].to_double()
      let val = value.to_double()
      
      if val < min_val {
        parts[0]
      } else if val > max_val {
        parts[1]
      } else {
        value
      }
    }
    "int_range" => {
      let parts = validation_range",".split_to_string()
      let min_val = parts[0].to_int()
      let max_val = parts[1].to_int()
      let val = value.to_int()
      
      if val < min_val {
        parts[0]
      } else if val > max_val {
        parts[1]
      } else {
        value
      }
    }
    _ => value
  }
  
  return update_config(config, key, validated_value)
}