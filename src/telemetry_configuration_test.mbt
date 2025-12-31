// 遥测配置管理测试用例

test "telemetry_service_configuration" {
  // 测试遥测服务配置
  
  let service_config = [
    ("service.name", "payment-api"),
    ("service.version", "1.2.3"),
    ("service.instance.id", "instance-001"),
    ("deployment.environment", "production")
  ]
  
  // 验证配置项数量
  assert_eq(service_config.length(), 4)
  
  // 验证服务配置
  assert_eq(service_config[0].0, "service.name")
  assert_eq(service_config[0].1, "payment-api")
  assert_eq(service_config[1].0, "service.version")
  assert_eq(service_config[1].1, "1.2.3")
  
  // 验证实例配置
  assert_eq(service_config[2].0, "service.instance.id")
  assert_eq(service_config[2].1, "instance-001")
  assert_eq(service_config[2].1.has_prefix("instance-"), true)
  
  // 验证环境配置
  assert_eq(service_config[3].0, "deployment.environment")
  assert_eq(service_config[3].1, "production")
  
  // 验证配置键格式
  let mut i = 0
  while i < service_config.length() {
    let config_key = service_config[i].0
    assert_eq(config_key.contains("."), true)
    i = i + 1
  }
  
  // 验证配置值非空
  i = 0
  while i < service_config.length() {
    let config_value = service_config[i].1
    assert_eq(config_value.length() > 0, true)
    i = i + 1
  }
}

test "telemetry_exporter_configuration" {
  // 测试遥测导出器配置
  
  let exporter_configs = [
    ("otlp.endpoint", "http://localhost:4317"),
    ("otlp.protocol", "grpc"),
    ("otlp.timeout", "30s"),
    ("otlp.headers", "authorization=Bearer token123")
  ]
  
  // 验证导出器配置数量
  assert_eq(exporter_configs.length(), 4)
  
  // 验证OTLP配置
  assert_eq(exporter_configs[0].0, "otlp.endpoint")
  assert_eq(exporter_configs[0].1, "http://localhost:4317")
  assert_eq(exporter_configs[0].1.has_prefix("http://"), true)
  
  // 验证协议配置
  assert_eq(exporter_configs[1].0, "otlp.protocol")
  assert_eq(exporter_configs[1].1, "grpc")
  assert_eq(exporter_configs[1].1 == "grpc" || exporter_configs[1].1 == "http", true)
  
  // 验证超时配置
  assert_eq(exporter_configs[2].0, "otlp.timeout")
  assert_eq(exporter_configs[2].1, "30s")
  assert_eq(exporter_configs[2].1.has_suffix("s"), true)
  
  // 验证头部配置
  assert_eq(exporter_configs[3].0, "otlp.headers")
  assert_eq(exporter_configs[3].1.contains("authorization"), true)
  assert_eq(exporter_configs[3].1.contains("Bearer"), true)
  
  // 验证端口号
  let endpoint = exporter_configs[0].1
  let port = endpoint.split(":").last()
  assert_eq(port, "4317")
  assert_eq(port.to_int() > 0, true)
}

test "telemetry_sampling_configuration" {
  // 测试遥测采样配置
  
  let sampling_config = [
    ("sampler.type", "probability"),
    ("sampler.param", "0.1"),
    ("sampler.parent_based", "true"),
    ("sampler.default", "always_on")
  ]
  
  // 验证采样配置数量
  assert_eq(sampling_config.length(), 4)
  
  // 验证采样器类型
  assert_eq(sampling_config[0].0, "sampler.type")
  assert_eq(sampling_config[0].1, "probability")
  assert_eq(sampling_config[0].1 == "probability" || 
           sampling_config[0].1 == "constant" || 
           sampling_config[0].1 == "rate_limiting", true)
  
  // 验证采样参数
  assert_eq(sampling_config[1].0, "sampler.param")
  assert_eq(sampling_config[1].1, "0.1")
  let sampling_rate = sampling_config[1].1.to_float()
  assert_eq(sampling_rate >= 0.0 && sampling_rate <= 1.0, true)
  
  // 验证父级基于采样
  assert_eq(sampling_config[2].0, "sampler.parent_based")
  assert_eq(sampling_config[2].1, "true")
  assert_eq(sampling_config[2].1 == "true" || sampling_config[2].1 == "false", true)
  
  // 验证默认采样器
  assert_eq(sampling_config[3].0, "sampler.default")
  assert_eq(sampling_config[3].1, "always_on")
  assert_eq(sampling_config[3].1 == "always_on" || 
           sampling_config[3].1 == "always_off" || 
           sampling_config[3].1 == "trace_id_ratio", true)
  
  // 验证采样率计算
  let total_requests = 1000
  let sampled_requests = (total_requests.to_float() * sampling_rate).to_int()
  assert_eq(sampled_requests, 100)  // 1000 * 0.1 = 100
}

test "telemetry_resource_configuration" {
  // 测试遥测资源配置
  
  let resource_config = [
    ("telemetry.sdk.name", "opentelemetry"),
    ("telemetry.sdk.language", "moonbit"),
    ("telemetry.sdk.version", "1.0.0"),
    ("host.name", "web-server-01"),
    ("os.type", "linux"),
    ("process.pid", "1234")
  ]
  
  // 验证资源配置数量
  assert_eq(resource_config.length(), 6)
  
  // 验证SDK配置
  assert_eq(resource_config[0].0, "telemetry.sdk.name")
  assert_eq(resource_config[0].1, "opentelemetry")
  assert_eq(resource_config[1].0, "telemetry.sdk.language")
  assert_eq(resource_config[1].1, "moonbit")
  assert_eq(resource_config[2].0, "telemetry.sdk.version")
  assert_eq(resource_config[2].1, "1.0.0")
  
  // 验证主机配置
  assert_eq(resource_config[3].0, "host.name")
  assert_eq(resource_config[3].1, "web-server-01")
  assert_eq(resource_config[4].0, "os.type")
  assert_eq(resource_config[4].1, "linux")
  
  // 验证进程配置
  assert_eq(resource_config[5].0, "process.pid")
  assert_eq(resource_config[5].1, "1234")
  assert_eq(resource_config[5].1.to_int() > 0, true)
  
  // 验证版本格式
  let sdk_version = resource_config[2].1
  assert_eq(sdk_version.contains("."), true)
  let version_parts = sdk_version.split(".")
  assert_eq(version_parts.length(), 3)
}

test "telemetry_metric_configuration" {
  // 测试遥测指标配置
  
  let metric_config = [
    ("metric.export.interval", "60s"),
    ("metric.export.timeout", "30s"),
    ("metric.reader.type", "periodic"),
    ("metric.aggregation.temporality", "cumulative")
  ]
  
  // 验证指标配置数量
  assert_eq(metric_config.length(), 4)
  
  // 验证导出间隔
  assert_eq(metric_config[0].0, "metric.export.interval")
  assert_eq(metric_config[0].1, "60s")
  assert_eq(metric_config[0].1.has_suffix("s"), true)
  let export_interval = metric_config[0].1.replace("s", "").to_int()
  assert_eq(export_interval, 60)
  
  // 验证导出超时
  assert_eq(metric_config[1].0, "metric.export.timeout")
  assert_eq(metric_config[1].1, "30s")
  let export_timeout = metric_config[1].1.replace("s", "").to_int()
  assert_eq(export_timeout, 30)
  
  // 验证超时小于间隔
  assert_eq(export_timeout < export_interval, true)
  
  // 验证读取器类型
  assert_eq(metric_config[2].0, "metric.reader.type")
  assert_eq(metric_config[2].1, "periodic")
  assert_eq(metric_config[2].1 == "periodic" || 
           metric_config[2].1 == "manual", true)
  
  // 验证聚合临时性
  assert_eq(metric_config[3].0, "metric.aggregation.temporality")
  assert_eq(metric_config[3].1, "cumulative")
  assert_eq(metric_config[3].1 == "cumulative" || 
           metric_config[3].1 == "delta", true)
  
  // 计算每分钟导出次数
  let exports_per_minute = 60 / export_interval
  assert_eq(exports_per_minute, 1)  // 60/60 = 1
}