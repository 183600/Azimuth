// Azimuth Platform Specific Telemetry Adaptation Tests
// 平台特定遥测适配测试用例
// 测试不同平台和环境下的遥测适配能力

import "azimuth/azimuth"

// Test 1: 多平台兼容性测试
pub test "多平台兼容性测试" {
  // 模拟不同平台环境
  let platforms = [
    {
      "name": "linux",
      "os": "Linux",
      "arch": "x86_64",
      "version": "Ubuntu 20.04",
      "expected_metrics": ["cpu.usage", "memory.usage", "disk.io", "network.io"]
    },
    {
      "name": "windows",
      "os": "Windows",
      "arch": "amd64",
      "version": "Windows Server 2019",
      "expected_metrics": ["cpu.usage", "memory.usage", "disk.io", "network.io", "process.count"]
    },
    {
      "name": "macos",
      "os": "macOS",
      "arch": "arm64",
      "version": "macOS 12.0",
      "expected_metrics": ["cpu.usage", "memory.usage", "disk.io", "network.io"]
    },
    {
      "name": "kubernetes",
      "os": "Linux",
      "arch": "x86_64",
      "version": "Kubernetes 1.22",
      "expected_metrics": ["cpu.usage", "memory.usage", "disk.io", "network.io", "pod.count", "container.count"]
    }
  ]
  
  // 为每个平台创建遥测组件
  let platform_results = []
  
  for platform in platforms {
    // 创建平台特定的资源
    let platform_resource = azimuth::Resource::new()
    let platform_attrs = [
      ("os.name", azimuth::StringValue(platform["os"])),
      ("os.arch", azimuth::StringValue(platform["arch"])),
      ("os.version", azimuth::StringValue(platform["version"])),
      ("platform.name", azimuth::StringValue(platform["name"]))
    ]
    
    let resource_with_attrs = azimuth::Resource::with_attributes(platform_resource, platform_attrs)
    
    // 创建平台特定的Tracer
    let tracer_provider = azimuth::TracerProvider::with_resource(resource_with_attrs)
    let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, platform["name"] + "-tracer")
    
    // 创建平台特定的Meter
    let meter_provider = azimuth::MeterProvider::with_resource(resource_with_attrs)
    let meter = azimuth::MeterProvider::get_meter(meter_provider, platform["name"] + "-meter")
    
    // 创建平台特定的Logger
    let logger_provider = azimuth::LoggerProvider::with_resource(resource_with_attrs)
    let logger = azimuth::LoggerProvider::get_logger(logger_provider, platform["name"] + "-logger")
    
    // 测试平台特定的度量收集
    let collected_metrics = []
    
    for metric_name in platform["expected_metrics"] {
      let metric_value = generate_platform_metric_value(platform["name"], metric_name)
      
      let metric = {
        "name": metric_name,
        "value": metric_value,
        "platform": platform["name"],
        "timestamp": azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
      }
      
      collected_metrics.push(metric)
      
      // 创建相应的度量仪器
      match metric_name {
        "cpu.usage" => {
          let gauge = azimuth::Meter::create_gauge(meter, metric_name, Some("CPU usage percentage"), Some("%"))
          azimuth::Gauge::record(gauge, metric_value)
        }
        "memory.usage" => {
          let gauge = azimuth::Meter::create_gauge(meter, metric_name, Some("Memory usage percentage"), Some("%"))
          azimuth::Gauge::record(gauge, metric_value)
        }
        "disk.io" => {
          let counter = azimuth::Meter::create_counter(meter, metric_name, Some("Disk I/O operations"), Some("ops"))
          azimuth::Counter::add(counter, metric_value)
        }
        "network.io" => {
          let counter = azimuth::Meter::create_counter(meter, metric_name, Some("Network I/O bytes"), Some("bytes"))
          azimuth::Counter::add(counter, metric_value)
        }
        "process.count" => {
          let gauge = azimuth::Meter::create_gauge(meter, metric_name, Some("Process count"), Some("processes"))
          azimuth::Gauge::record(gauge, metric_value)
        }
        "pod.count" => {
          let gauge = azimuth::Meter::create_gauge(meter, metric_name, Some("Pod count"), Some("pods"))
          azimuth::Gauge::record(gauge, metric_value)
        }
        "container.count" => {
          let gauge = azimuth::Meter::create_gauge(meter, metric_name, Some("Container count"), Some("containers"))
          azimuth::Gauge::record(gauge, metric_value)
        }
        _ => {}
      }
    }
    
    // 创建平台特定的Span
    let span = azimuth::Tracer::start_span(tracer, "platform.operation")
    azimuth::Span::add_event(span, "platform.metrics.collected", Some([
      ("platform.name", azimuth::StringValue(platform["name"])),
      ("metrics.count", azimuth::IntValue(collected_metrics.length()))
    ]))
    azimuth::Span::end(span)
    
    // 创建平台特定的日志
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Platform metrics collected for " + platform["name"]),
      Some([
        ("platform.name", azimuth::StringValue(platform["name"])),
        ("metrics.count", azimuth::IntValue(collected_metrics.length())),
        ("os.version", azimuth::StringValue(platform["version"]))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
    
    // 记录平台测试结果
    platform_results.push({
      "platform": platform["name"],
      "metrics.collected": collected_metrics.length(),
      "expected.metrics": platform["expected_metrics"].length(),
      "resource.attributes": platform_attrs.length()
    })
  }
  
  // 验证平台测试结果
  for result in platform_results {
    assert_eq(result["metrics.collected"], result["expected.metrics"])
    assert_true(result["resource.attributes"] >= 4)
  }
  
  // 创建平台适配度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "platform-adaptation")
  let platform_count_gauge = azimuth::Meter::create_gauge(meter, "platforms.supported")
  let metrics_per_platform_histogram = azimuth::Meter::create_histogram(meter, "metrics.per.platform", Some("Metrics collected per platform"), None)
  
  azimuth::Gauge::record(platform_count_gauge, platform_results.length().to_double())
  
  for result in platform_results {
    azimuth::Histogram::record(metrics_per_platform_histogram, result["metrics.collected"].to_double(), Some([
      ("platform.name", azimuth::StringValue(result["platform"]))
    ]))
  }
}

// Test 2: 云环境适配测试
pub test "云环境适配测试" {
  // 模拟不同云环境
  let cloud_environments = [
    {
      "name": "aws",
      "provider": "Amazon Web Services",
      "region": "us-west-2",
      "services": ["EC2", "RDS", "S3", "Lambda"],
      "expected_attributes": ["cloud.provider", "cloud.region", "cloud.availability_zone", "instance.id", "instance.type"]
    },
    {
      "name": "azure",
      "provider": "Microsoft Azure",
      "region": "eastus",
      "services": ["VM", "SQL Database", "Blob Storage", "Functions"],
      "expected_attributes": ["cloud.provider", "cloud.region", "cloud.resource_group", "instance.id", "instance.type"]
    },
    {
      "name": "gcp",
      "provider": "Google Cloud Platform",
      "region": "us-central1",
      "services": ["Compute Engine", "Cloud SQL", "Cloud Storage", "Cloud Functions"],
      "expected_attributes": ["cloud.provider", "cloud.region", "cloud.zone", "instance.id", "instance.type"]
    },
    {
      "name": "alibaba",
      "provider": "Alibaba Cloud",
      "region": "cn-hangzhou",
      "services": ["ECS", "RDS", "OSS", "Function Compute"],
      "expected_attributes": ["cloud.provider", "cloud.region", "cloud.zone", "instance.id", "instance.type"]
    }
  ]
  
  // 为每个云环境创建遥测组件
  let cloud_results = []
  
  for cloud in cloud_environments {
    // 创建云特定的资源
    let cloud_resource = azimuth::Resource::new()
    let cloud_attrs = [
      ("cloud.provider", azimuth::StringValue(cloud["provider"])),
      ("cloud.region", azimuth::StringValue(cloud["region"])),
      ("cloud.platform", azimuth::StringValue(cloud["name"])),
      ("service.instance.id", azimuth::StringValue("instance-" + cloud["name"] + "-" + azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_string())),
      ("service.name", azimuth::StringValue("telemetry-service")),
      ("service.namespace", azimuth::StringValue("production"))
    ]
    
    // 添加云特定属性
    if cloud["name"] == "aws" {
      cloud_attrs.push(("cloud.availability_zone", azimuth::StringValue("us-west-2a")))
      cloud_attrs.push(("instance.type", azimuth::StringValue("t3.medium")))
    } else if cloud["name"] == "azure" {
      cloud_attrs.push(("cloud.resource_group", azimuth::StringValue("telemetry-rg")))
      cloud_attrs.push(("instance.type", azimuth::StringValue("Standard_B2s")))
    } else if cloud["name"] == "gcp" {
      cloud_attrs.push(("cloud.zone", azimuth::StringValue("us-central1-a")))
      cloud_attrs.push(("instance.type", azimuth::StringValue("e2-medium")))
    } else if cloud["name"] == "alibaba" {
      cloud_attrs.push(("cloud.zone", azimuth::StringValue("cn-hangzhou-a")))
      cloud_attrs.push(("instance.type", azimuth::StringValue("ecs.c6.large")))
    }
    
    let resource_with_attrs = azimuth::Resource::with_attributes(cloud_resource, cloud_attrs)
    
    // 创建云特定的Tracer
    let tracer_provider = azimuth::TracerProvider::with_resource(resource_with_attrs)
    let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, cloud["name"] + "-tracer")
    
    // 创建云特定的Meter
    let meter_provider = azimuth::MeterProvider::with_resource(resource_with_attrs)
    let meter = azimuth::MeterProvider::get_meter(meter_provider, cloud["name"] + "-meter")
    
    // 创建云特定的Logger
    let logger_provider = azimuth::LoggerProvider::with_resource(resource_with_attrs)
    let logger = azimuth::LoggerProvider::get_logger(logger_provider, cloud["name"] + "-logger")
    
    // 测试云服务特定的度量
    for service in cloud["services"] {
      let service_metric_name = cloud["name"] + "." + service.to_lower() + ".requests"
      let counter = azimuth::Meter::create_counter(meter, service_metric_name, Some(service + " requests"), Some("requests"))
      azimuth::Counter::add(counter, 10.0 + azimuth::Random::next_double(azimuth::Random::system()) * 20.0, Some([
        ("service.name", azimuth::StringValue(service)),
        ("cloud.region", azimuth::StringValue(cloud["region"]))
      ]))
      
      // 创建服务特定的Span
      let span = azimuth::Tracer::start_span(tracer, service + ".operation")
      azimuth::Span::add_event(span, "service.operation.started", Some([
        ("service.name", azimuth::StringValue(service)),
        ("cloud.provider", azimuth::StringValue(cloud["provider"])),
        ("cloud.region", azimuth::StringValue(cloud["region"]))
      ]))
      
      // 模拟服务操作时间
      azimuth::Clock::sleep(5)
      
      azimuth::Span::add_event(span, "service.operation.completed", Some([
        ("service.name", azimuth::StringValue(service)),
        ("operation.duration", azimuth::IntValue(5))
      ]))
      
      azimuth::Span::end(span)
      
      // 创建服务特定的日志
      let log_record = azimuth::LogRecord::new_with_context(
        azimuth::Info,
        Some(service + " operation completed in " + cloud["name"]),
        Some([
          ("service.name", azimuth::StringValue(service)),
          ("cloud.provider", azimuth::StringValue(cloud["provider"])),
          ("cloud.region", azimuth::StringValue(cloud["region"]))
        ]),
        Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
        Some(azimuth::Context::root())
      )
      
      azimuth::Logger::emit(logger, log_record)
    }
    
    // 记录云环境测试结果
    cloud_results.push({
      "cloud.provider": cloud["name"],
      "services.tested": cloud["services"].length(),
      "attributes.collected": cloud_attrs.length(),
      "expected.attributes": cloud["expected_attributes"].length()
    })
  }
  
  // 验证云环境测试结果
  for result in cloud_results {
    assert_eq(result["services.tested"], cloud_environments.find(e => e["name"] == result["cloud.provider"])["services"].length())
    assert_true(result["attributes.collected"] >= result["expected.attributes"])
  }
  
  // 创建云环境适配度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "cloud-adaptation")
  let cloud_count_gauge = azimuth::Meter::create_gauge(meter, "cloud.providers.supported")
  let services_per_cloud_histogram = azimuth::Meter::create_histogram(meter, "services.per.cloud", Some("Services tested per cloud provider"), None)
  
  azimuth::Gauge::record(cloud_count_gauge, cloud_results.length().to_double())
  
  for result in cloud_results {
    azimuth::Histogram::record(services_per_cloud_histogram, result["services.tested"].to_double(), Some([
      ("cloud.provider", azimuth::StringValue(result["cloud.provider"]))
    ]))
  }
}

// Test 3: 容器化环境适配测试
pub test "容器化环境适配测试" {
  // 模拟不同容器化环境
  let container_environments = [
    {
      "name": "docker",
      "runtime": "Docker",
      "version": "20.10.12",
      "orchestration": "standalone",
      "expected_attributes": ["container.id", "container.name", "container.image", "container.runtime"]
    },
    {
      "name": "kubernetes",
      "runtime": "containerd",
      "version": "1.4.3",
      "orchestration": "Kubernetes",
      "expected_attributes": ["container.id", "container.name", "container.image", "k8s.pod.name", "k8s.namespace", "k8s.deployment.name"]
    },
    {
      "name": "openshift",
      "runtime": "CRI-O",
      "version": "1.20.0",
      "orchestration": "OpenShift",
      "expected_attributes": ["container.id", "container.name", "container.image", "k8s.pod.name", "k8s.namespace", "openshift.project"]
    },
    {
      "name": "ecs",
      "runtime": "Docker",
      "version": "20.10.5",
      "orchestration": "Amazon ECS",
      "expected_attributes": ["container.id", "container.name", "container.image", "ecs.task.arn", "ecs.cluster"]
    }
  ]
  
  // 为每个容器化环境创建遥测组件
  let container_results = []
  
  for container in container_environments {
    // 创建容器特定的资源
    let container_resource = azimuth::Resource::new()
    let container_attrs = [
      ("container.runtime", azimuth::StringValue(container["runtime"])),
      ("container.runtime.version", azimuth::StringValue(container["version"])),
      ("container.name", azimuth::StringValue("telemetry-container-" + container["name"])),
      ("container.image", azimuth::StringValue("azimuth/telemetry:latest")),
      ("container.id", azimuth::StringValue("container-" + container["name"] + "-" + azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_string()))
    ]
    
    // 添加编排系统特定属性
    if container["orchestration"] == "Kubernetes" {
      container_attrs.push(("k8s.pod.name", azimuth::StringValue("telemetry-pod-" + container["name"])))
      container_attrs.push(("k8s.namespace", azimuth::StringValue("telemetry")))
      container_attrs.push(("k8s.deployment.name", azimuth::StringValue("telemetry-deployment")))
    } else if container["orchestration"] == "OpenShift" {
      container_attrs.push(("k8s.pod.name", azimuth::StringValue("telemetry-pod-" + container["name"])))
      container_attrs.push(("k8s.namespace", azimuth::StringValue("telemetry-project")))
      container_attrs.push(("openshift.project", azimuth::StringValue("telemetry-project")))
    } else if container["orchestration"] == "Amazon ECS" {
      container_attrs.push(("ecs.task.arn", azimuth::StringValue("arn:aws:ecs:us-west-2:123456789012:task/telemetry-cluster/" + azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_string())))
      container_attrs.push(("ecs.cluster", azimuth::StringValue("telemetry-cluster")))
    }
    
    let resource_with_attrs = azimuth::Resource::with_attributes(container_resource, container_attrs)
    
    // 创建容器特定的Tracer
    let tracer_provider = azimuth::TracerProvider::with_resource(resource_with_attrs)
    let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, container["name"] + "-tracer")
    
    // 创建容器特定的Meter
    let meter_provider = azimuth::MeterProvider::with_resource(resource_with_attrs)
    let meter = azimuth::MeterProvider::get_meter(meter_provider, container["name"] + "-meter")
    
    // 创建容器特定的Logger
    let logger_provider = azimuth::LoggerProvider::with_resource(resource_with_attrs)
    let logger = azimuth::LoggerProvider::get_logger(logger_provider, container["name"] + "-logger")
    
    // 测试容器特定的度量
    let container_cpu_gauge = azimuth::Meter::create_gauge(meter, "container.cpu.usage", Some("Container CPU usage"), Some("percent"))
    let container_memory_gauge = azimuth::Meter::create_gauge(meter, "container.memory.usage", Some("Container memory usage"), Some("MiB"))
    let container_network_counter = azimuth::Meter::create_counter(meter, "container.network.io", Some("Container network I/O"), Some("bytes"))
    
    // 模拟容器资源使用
    let cpu_usage = 20.0 + azimuth::Random::next_double(azimuth::Random::system()) * 60.0  // 20-80%
    let memory_usage = 128.0 + azimuth::Random::next_double(azimuth::Random::system()) * 512.0  // 128-640MiB
    let network_io = 1024.0 * 1024.0 * (10.0 + azimuth::Random::next_double(azimuth::Random::system()) * 100.0)  // 10-110MB
    
    azimuth::Gauge::record(container_cpu_gauge, cpu_usage, Some([
      ("container.runtime", azimuth::StringValue(container["runtime"]))
    ]))
    
    azimuth::Gauge::record(container_memory_gauge, memory_usage, Some([
      ("container.runtime", azimuth::StringValue(container["runtime"]))
    ]))
    
    azimuth::Counter::add(container_network_counter, network_io, Some([
      ("container.runtime", azimuth::StringValue(container["runtime"])),
      ("network.direction", azimuth::StringValue("tx"))
    ]))
    
    // 创建容器特定的Span
    let span = azimuth::Tracer::start_span(tracer, "container.operation")
    azimuth::Span::add_event(span, "container.operation.started", Some([
      ("container.runtime", azimuth::StringValue(container["runtime"])),
      ("container.name", azimuth::StringValue(container_attrs[2]["value"])),
      ("orchestration", azimuth::StringValue(container["orchestration"]))
    ]))
    
    // 模拟容器操作时间
    azimuth::Clock::sleep(10)
    
    azimuth::Span::add_event(span, "container.operation.completed", Some([
      ("container.operation.duration", azimuth::IntValue(10)),
      ("container.cpu.usage", azimuth::FloatValue(cpu_usage)),
      ("container.memory.usage", azimuth::FloatValue(memory_usage))
    ]))
    
    azimuth::Span::end(span)
    
    // 创建容器特定的日志
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Container operation completed in " + container["runtime"]),
      Some([
        ("container.runtime", azimuth::StringValue(container["runtime"])),
        ("container.name", azimuth::StringValue(container_attrs[2]["value"])),
        ("orchestration", azimuth::StringValue(container["orchestration"])),
        ("cpu.usage", azimuth::FloatValue(cpu_usage)),
        ("memory.usage", azimuth::FloatValue(memory_usage))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(span))),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
    
    // 记录容器环境测试结果
    container_results.push({
      "container.runtime": container["name"],
      "orchestration": container["orchestration"],
      "attributes.collected": container_attrs.length(),
      "expected.attributes": container["expected_attributes"].length(),
      "metrics.collected": 3
    })
  }
  
  // 验证容器环境测试结果
  for result in container_results {
    assert_true(result["attributes.collected"] >= result["expected.attributes"])
    assert_eq(result["metrics.collected"], 3)
  }
  
  // 创建容器环境适配度量
  let meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "container-adaptation")
  let runtime_count_gauge = azimuth::Meter::create_gauge(meter, "container.runtimes.supported")
  let attributes_per_runtime_histogram = azimuth::Meter::create_histogram(meter, "attributes.per.runtime", Some("Attributes collected per container runtime"), None)
  
  azimuth::Gauge::record(runtime_count_gauge, container_results.length().to_double())
  
  for result in container_results {
    azimuth::Histogram::record(attributes_per_runtime_histogram, result["attributes.collected"].to_double(), Some([
      ("container.runtime", azimuth::StringValue(result["container.runtime"]))
    ]))
  }
}

// 辅助函数：根据平台和度量名称生成度量值
fn generate_platform_metric_value(platform : String, metric_name : String) -> Double {
  match (platform, metric_name) {
    ("linux", "cpu.usage") => 30.0 + azimuth::Random::next_double(azimuth::Random::system()) * 40.0
    ("linux", "memory.usage") => 50.0 + azimuth::Random::next_double(azimuth::Random::system()) * 30.0
    ("linux", "disk.io") => 1000.0 + azimuth::Random::next_double(azimuth::Random::system()) * 5000.0
    ("linux", "network.io") => 10000.0 + azimuth::Random::next_double(azimuth::Random::system()) * 50000.0
    
    ("windows", "cpu.usage") => 25.0 + azimuth::Random::next_double(azimuth::Random::system()) * 45.0
    ("windows", "memory.usage") => 40.0 + azimuth::Random::next_double(azimuth::Random::system()) * 35.0
    ("windows", "disk.io") => 800.0 + azimuth::Random::next_double(azimuth::Random::system()) * 4000.0
    ("windows", "network.io") => 8000.0 + azimuth::Random::next_double(azimuth::Random::system()) * 40000.0
    ("windows", "process.count") => 100.0 + azimuth::Random::next_double(azimuth::Random::system()) * 200.0
    
    ("macos", "cpu.usage") => 20.0 + azimuth::Random::next_double(azimuth::Random::system()) * 50.0
    ("macos", "memory.usage") => 45.0 + azimuth::Random::next_double(azimuth::Random::system()) * 35.0
    ("macos", "disk.io") => 500.0 + azimuth::Random::next_double(azimuth::Random::system()) * 3000.0
    ("macos", "network.io") => 5000.0 + azimuth::Random::next_double(azimuth::Random::system()) * 30000.0
    
    ("kubernetes", "cpu.usage") => 35.0 + azimuth::Random::next_double(azimuth::Random::system()) * 35.0
    ("kubernetes", "memory.usage") => 55.0 + azimuth::Random::next_double(azimuth::Random::system()) * 25.0
    ("kubernetes", "disk.io") => 1200.0 + azimuth::Random::next_double(azimuth::Random::system()) * 4500.0
    ("kubernetes", "network.io") => 12000.0 + azimuth::Random::next_double(azimuth::Random::system()) * 45000.0
    ("kubernetes", "pod.count") => 10.0 + azimuth::Random::next_double(azimuth::Random::system()) * 50.0
    ("kubernetes", "container.count") => 20.0 + azimuth::Random::next_double(azimuth::Random::system()) * 100.0
    
    _ => 50.0 + azimuth::Random::next_double(azimuth::Random::system()) * 25.0
  }
}