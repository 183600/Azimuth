// 遥测配置验证测试用例

test "telemetry_service_config_validation" {
  // 测试遥测服务配置验证
  
  let service_config = {
    "service_name": "payment-service",
    "service_version": "1.2.3",
    "service_namespace": "ecommerce",
    "service_instance_id": "instance-12345",
    "deployment_environment": "production"
  }
  
  // 验证必需字段存在
  assert_eq(service_config.contains("service_name"), true)
  assert_eq(service_config.contains("service_version"), true)
  assert_eq(service_config.contains("service_namespace"), true)
  assert_eq(service_config.contains("service_instance_id"), true)
  assert_eq(service_config.contains("deployment_environment"), true)
  
  // 验证服务名称格式
  let service_name = service_config.get("service_name", "")
  assert_eq(service_name.length() > 0, true)
  assert_eq(service_name.length() <= 100, true)
  assert_eq(service_name.has_prefix("payment"), true)
  assert_eq(service_name.has_suffix("service"), true)
  assert_eq(service_name.contains(" "), false)  // 不应包含空格
  
  // 验证版本号格式（语义化版本）
  let service_version = service_config.get("service_version", "")
  assert_eq(service_version.length() > 0, true)
  assert_eq(service_version.contains("."), true)
  assert_eq(service_version.has_prefix("1"), true)
  assert_eq(service_version.has_suffix("3"), true)
  
  // 验证命名空间
  let service_namespace = service_config.get("service_namespace", "")
  assert_eq(service_namespace.length() > 0, true)
  assert_eq(service_namespace.length() <= 50, true)
  assert_eq(service_namespace == "ecommerce", true)
  assert_eq(service_namespace.contains(" "), false)
  
  // 验证实例ID格式
  let service_instance_id = service_config.get("service_instance_id", "")
  assert_eq(service_instance_id.length() > 0, true)
  assert_eq(service_instance_id.has_prefix("instance-"), true)
  assert_eq(service_instance_id.contains(" "), false)
  
  // 验证部署环境
  let deployment_environment = service_config.get("deployment_environment", "")
  assert_eq(deployment_environment.length() > 0, true)
  assert_eq(deployment_environment == "production" || deployment_environment == "staging" || deployment_environment == "development", true)
}

test "telemetry_exporter_config_validation" {
  // 测试遥测导出器配置验证
  
  let exporter_config = {
    "exporter_type": "otlp",
    "endpoint": "https://otel-collector.example.com:4317",
    "headers": {
      "authorization": "Bearer token123",
      "x-custom-header": "custom-value"
    },
    "timeout_seconds": 30,
    "retry_count": 3,
    "batch_size": 512,
    "compression": "gzip",
    "tls_enabled": true,
    "cert_file": "/path/to/cert.pem"
  }
  
  // 验证导出器类型
  let exporter_type = exporter_config.get("exporter_type", "")
  assert_eq(exporter_type.length() > 0, true)
  assert_eq(exporter_type == "otlp" || exporter_type == "jaeger" || exporter_type == "zipkin" || exporter_type == "prometheus", true)
  
  // 验证端点URL
  let endpoint = exporter_config.get("endpoint", "")
  assert_eq(endpoint.length() > 0, true)
  assert_eq(endpoint.has_prefix("http"), true)
  assert_eq(endpoint.contains("://"), true)
  assert_eq(endpoint.contains("example.com"), true)
  assert_eq(endpoint.contains(":4317"), true)
  
  // 验证超时设置
  let timeout_seconds = exporter_config.get("timeout_seconds", 0)
  assert_eq(timeout_seconds > 0, true)
  assert_eq(timeout_seconds <= 300, true)  // 最大5分钟
  
  // 验证重试次数
  let retry_count = exporter_config.get("retry_count", 0)
  assert_eq(retry_count >= 0, true)
  assert_eq(retry_count <= 10, true)  // 最大重试10次
  
  // 验证批处理大小
  let batch_size = exporter_config.get("batch_size", 0)
  assert_eq(batch_size > 0, true)
  assert_eq(batch_size <= 10000, true)  // 最大批处理10000
  
  // 验证压缩类型
  let compression = exporter_config.get("compression", "")
  assert_eq(compression == "none" || compression == "gzip" || compression == "deflate", true)
  
  // 验证TLS配置
  let tls_enabled = exporter_config.get("tls_enabled", false)
  assert_eq(tls_enabled == true || tls_enabled == false, true)
  
  // 如果启用TLS，验证证书文件
  if tls_enabled {
    let cert_file = exporter_config.get("cert_file", "")
    assert_eq(cert_file.length() > 0, true)
    assert_eq(cert_file.has_suffix(".pem") || cert_file.has_suffix(".crt") || cert_file.has_suffix(".key"), true)
  }
}

test "telemetry_sampling_config_validation" {
  // 测试遥测采样配置验证
  
  let sampling_config = {
    "sampler_type": "trace_id_ratio",
    "sampling_ratio": 0.1,
    "parent_based": true,
    "default_sampler": "always_on",
    "samplers": {
      "debug": {
        "type": "always_on",
        "attributes": {
          "debug.enabled": "true"
        }
      },
      "production": {
        "type": "trace_id_ratio",
        "ratio": 0.01
      },
      "development": {
        "type": "always_on"
      }
    }
  }
  
  // 验证采样器类型
  let sampler_type = sampling_config.get("sampler_type", "")
  assert_eq(sampler_type.length() > 0, true)
  assert_eq(sampler_type == "always_on" || sampler_type == "always_off" || sampler_type == "trace_id_ratio" || sampler_type == "parent_based", true)
  
  // 验证采样比例
  let sampling_ratio = sampling_config.get("sampling_ratio", 0.0)
  assert_eq(sampling_ratio >= 0.0, true)
  assert_eq(sampling_ratio <= 1.0, true)
  
  // 验证父级采样
  let parent_based = sampling_config.get("parent_based", false)
  assert_eq(parent_based == true || parent_based == false, true)
  
  // 验证默认采样器
  let default_sampler = sampling_config.get("default_sampler", "")
  assert_eq(default_sampler.length() > 0, true)
  assert_eq(default_sampler == "always_on" || default_sampler == "always_off" || default_sampler == "trace_id_ratio", true)
  
  // 验证采样器配置
  let samplers = sampling_config.get("samplers", {})
  assert_eq(samplers.contains("debug"), true)
  assert_eq(samplers.contains("production"), true)
  assert_eq(samplers.contains("development"), true)
  
  // 验证生产环境采样器
  let production_sampler = samplers.get("production", {})
  assert_eq(production_sampler.get("type", ""), "trace_id_ratio")
  assert_eq(production_sampler.get("ratio", 0.0), 0.01)
}

test "telemetry_resource_config_validation" {
  // 测试遥测资源配置验证
  
  let resource_config = {
    "resource_attributes": {
      "service.name": "user-service",
      "service.version": "2.1.0",
      "service.namespace": "auth",
      "service.instance.id": "pod-abc123",
      "deployment.environment": "staging",
      "host.name": "host-01",
      "host.arch": "amd64",
      "os.type": "linux",
      "os.version": "ubuntu-20.04",
      "cloud.provider": "aws",
      "cloud.region": "us-west-2",
      "cloud.availability_zone": "us-west-2a"
    },
    "schema_url": "https://opentelemetry.io/schemas/v1.20.0",
    "detected_resources": true
  }
  
  // 验证资源属性
  let resource_attributes = resource_config.get("resource_attributes", {})
  assert_eq(resource_attributes.contains("service.name"), true)
  assert_eq(resource_attributes.contains("service.version"), true)
  assert_eq(resource_attributes.contains("service.namespace"), true)
  assert_eq(resource_attributes.contains("service.instance.id"), true)
  assert_eq(resource_attributes.contains("deployment.environment"), true)
  
  // 验证服务名称
  let service_name = resource_attributes.get("service.name", "")
  assert_eq(service_name.length() > 0, true)
  assert_eq(service_name == "user-service", true)
  
  // 验证服务版本
  let service_version = resource_attributes.get("service.version", "")
  assert_eq(service_version.length() > 0, true)
  assert_eq(service_version.contains("."), true)
  
  // 验证实例ID
  let instance_id = resource_attributes.get("service.instance.id", "")
  assert_eq(instance_id.length() > 0, true)
  assert_eq(instance_id.has_prefix("pod-"), true)
  
  // 验证部署环境
  let environment = resource_attributes.get("deployment.environment", "")
  assert_eq(environment == "production" || environment == "staging" || environment == "development", true)
  
  // 验证主机信息
  assert_eq(resource_attributes.contains("host.name"), true)
  assert_eq(resource_attributes.contains("host.arch"), true)
  assert_eq(resource_attributes.contains("os.type"), true)
  
  // 验证云信息
  assert_eq(resource_attributes.contains("cloud.provider"), true)
  assert_eq(resource_attributes.contains("cloud.region"), true)
  
  // 验证schema URL
  let schema_url = resource_config.get("schema_url", "")
  assert_eq(schema_url.length() > 0, true)
  assert_eq(schema_url.has_prefix("https://"), true)
  assert_eq(schema_url.contains("opentelemetry.io"), true)
  
  // 验证自动检测资源
  let detected_resources = resource_config.get("detected_resources", false)
  assert_eq(detected_resources == true || detected_resources == false, true)
}

test "telemetry_metrics_config_validation" {
  // 测试遥测指标配置验证
  
  let metrics_config = {
    "enabled_instruments": [
      "counter",
      "histogram", 
      "updown_counter",
      "observable_counter",
      "observable_gauge"
    ],
    "metric_readers": [
      {
        "type": "periodic",
        "interval_ms": 10000,
        "timeout_ms": 5000
      },
      {
        "type": "manual"
      }
    ],
    "views": [
      {
        "name": "http_requests_total",
        "instrument_type": "counter",
        "description": "Total number of HTTP requests",
        "unit": "requests",
        "aggregation": "sum"
      },
      {
        "name": "http_request_duration",
        "instrument_type": "histogram",
        "description": "HTTP request duration",
        "unit": "ms",
        "aggregation": "histogram",
        "boundaries": [1.0, 5.0, 10.0, 50.0, 100.0, 500.0, 1000.0]
      }
    ],
    "exemplars_enabled": true,
    "exemplar_filter": "trace_based"
  }
  
  // 验证启用的仪器类型
  let enabled_instruments = metrics_config.get("enabled_instruments", [])
  assert_eq(enabled_instruments.length() > 0, true)
  assert_eq(enabled_instruments.contains("counter"), true)
  assert_eq(enabled_instruments.contains("histogram"), true)
  
  // 验证指标读取器
  let metric_readers = metrics_config.get("metric_readers", [])
  assert_eq(metric_readers.length() > 0, true)
  
  // 验证周期性读取器
  let periodic_reader = metric_readers[0]
  assert_eq(periodic_reader.get("type", ""), "periodic")
  assert_eq(periodic_reader.get("interval_ms", 0) > 0, true)
  assert_eq(periodic_reader.get("timeout_ms", 0) > 0, true)
  assert_eq(periodic_reader.get("interval_ms", 0) > periodic_reader.get("timeout_ms", 0), true)
  
  // 验证视图配置
  let views = metrics_config.get("views", [])
  assert_eq(views.length() > 0, true)
  
  // 验证计数器视图
  let counter_view = views[0]
  assert_eq(counter_view.get("name", ""), "http_requests_total")
  assert_eq(counter_view.get("instrument_type", ""), "counter")
  assert_eq(counter_view.get("description", "").length() > 0, true)
  assert_eq(counter_view.get("unit", ""), "requests")
  assert_eq(counter_view.get("aggregation", ""), "sum")
  
  // 验证直方图视图
  let histogram_view = views[1]
  assert_eq(histogram_view.get("name", ""), "http_request_duration")
  assert_eq(histogram_view.get("instrument_type", ""), "histogram")
  assert_eq(histogram_view.get("boundaries", []).length() > 0, true)
  
  // 验证示例配置
  let exemplars_enabled = metrics_config.get("exemplars_enabled", false)
  assert_eq(exemplars_enabled == true || exemplars_enabled == false, true)
  
  if exemplars_enabled {
    let exemplar_filter = metrics_config.get("exemplar_filter", "")
    assert_eq(exemplar_filter.length() > 0, true)
    assert_eq(exemplar_filter == "trace_based" || exemplar_filter == "always_on" || exemplar_filter == "always_off", true)
  }
}

test "telemetry_tracing_config_validation" {
  // 测试遥测追踪配置验证
  
  let tracing_config = {
    "span_limits": {
      "max_attributes_per_span": 128,
      "max_events_per_span": 128,
      "max_links_per_span": 128,
      "max_attribute_length": 1024
    },
    "batch_span_processor": {
      "max_queue_size": 2048,
      "max_export_batch_size": 512,
      "export_timeout_ms": 30000,
      "max_export_batch_size_mb": 10
    },
    "id_generator": {
      "type": "random",
      "trace_id_bytes": 16,
      "span_id_bytes": 8
    },
    "propagators": [
      "tracecontext",
      "baggage",
      "jaeger",
      "b3"
    ]
  }
  
  // 验证span限制
  let span_limits = tracing_config.get("span_limits", {})
  assert_eq(span_limits.contains("max_attributes_per_span"), true)
  assert_eq(span_limits.contains("max_events_per_span"), true)
  assert_eq(span_limits.contains("max_links_per_span"), true)
  assert_eq(span_limits.contains("max_attribute_length"), true)
  
  // 验证属性限制
  let max_attributes = span_limits.get("max_attributes_per_span", 0)
  let max_events = span_limits.get("max_events_per_span", 0)
  let max_links = span_limits.get("max_links_per_span", 0)
  let max_attr_length = span_limits.get("max_attribute_length", 0)
  
  assert_eq(max_attributes > 0, true)
  assert_eq(max_attributes <= 1000, true)
  assert_eq(max_events > 0, true)
  assert_eq(max_events <= 1000, true)
  assert_eq(max_links > 0, true)
  assert_eq(max_links <= 1000, true)
  assert_eq(max_attr_length > 0, true)
  assert_eq(max_attr_length <= 10000, true)
  
  // 验证批处理span处理器配置
  let batch_processor = tracing_config.get("batch_span_processor", {})
  assert_eq(batch_processor.contains("max_queue_size"), true)
  assert_eq(batch_processor.contains("max_export_batch_size"), true)
  assert_eq(batch_processor.contains("export_timeout_ms"), true)
  
  let max_queue_size = batch_processor.get("max_queue_size", 0)
  let max_export_batch_size = batch_processor.get("max_export_batch_size", 0)
  let export_timeout = batch_processor.get("export_timeout_ms", 0)
  
  assert_eq(max_queue_size > 0, true)
  assert_eq(max_queue_size <= 10000, true)
  assert_eq(max_export_batch_size > 0, true)
  assert_eq(max_export_batch_size <= max_queue_size, true)
  assert_eq(export_timeout > 0, true)
  assert_eq(export_timeout <= 300000, true)  // 最大5分钟
  
  // 验证ID生成器配置
  let id_generator = tracing_config.get("id_generator", {})
  assert_eq(id_generator.get("type", ""), "random")
  assert_eq(id_generator.get("trace_id_bytes", 0), 16)
  assert_eq(id_generator.get("span_id_bytes", 0), 8)
  
  // 验证传播器配置
  let propagators = tracing_config.get("propagators", [])
  assert_eq(propagators.length() > 0, true)
  assert_eq(propagators.contains("tracecontext"), true)
  assert_eq(propagators.contains("baggage"), true)
}

test "telemetry_logging_config_validation" {
  // 测试遥测日志配置验证
  
  let logging_config = {
    "log_level": "info",
    "log_format": "json",
    "log_output": [
      {
        "type": "file",
        "path": "/var/log/telemetry.log",
        "max_size_mb": 100,
        "max_files": 10,
        "compress": true
      },
      {
        "type": "stdout"
      }
    ],
    "structured_logging": true,
    "include_stacktrace": true,
    "error_sampling": {
      "enabled": true,
      "sampling_ratio": 1.0
    }
  }
  
  // 验证日志级别
  let log_level = logging_config.get("log_level", "")
  assert_eq(log_level.length() > 0, true)
  assert_eq(log_level == "debug" || log_level == "info" || log_level == "warn" || log_level == "error" || log_level == "fatal", true)
  
  // 验证日志格式
  let log_format = logging_config.get("log_format", "")
  assert_eq(log_format == "json" || log_format == "text" || log_format == "console", true)
  
  // 验证日志输出配置
  let log_output = logging_config.get("log_output", [])
  assert_eq(log_output.length() > 0, true)
  
  // 验证文件输出配置
  let file_output = log_output[0]
  assert_eq(file_output.get("type", ""), "file")
  assert_eq(file_output.get("path", "").length() > 0, true)
  assert_eq(file_output.get("max_size_mb", 0) > 0, true)
  assert_eq(file_output.get("max_files", 0) > 0, true)
  
  // 验证结构化日志
  let structured_logging = logging_config.get("structured_logging", false)
  assert_eq(structured_logging == true || structured_logging == false, true)
  
  // 验证堆栈跟踪包含
  let include_stacktrace = logging_config.get("include_stacktrace", false)
  assert_eq(include_stacktrace == true || include_stacktrace == false, true)
  
  // 验证错误采样配置
  let error_sampling = logging_config.get("error_sampling", {})
  let sampling_enabled = error_sampling.get("enabled", false)
  let sampling_ratio = error_sampling.get("sampling_ratio", 0.0)
  
  if sampling_enabled {
    assert_eq(sampling_ratio >= 0.0, true)
    assert_eq(sampling_ratio <= 1.0, true)
  }
}