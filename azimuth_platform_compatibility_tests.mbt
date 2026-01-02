// Azimuth Platform Compatibility Tests
// 平台兼容性测试用例

test "cross-platform telemetry collection" {
  // 测试跨平台遥测数据收集
  let platform_configs = [
    @azimuth.PlatformConfig {
      platform : @azimuth.Platform::Linux,
      architecture : @azimuth.Architecture::X86_64,
      os_version : "Ubuntu 20.04",
      kernel_version : "5.4.0-91-generic",
      telemetry_agent_version : "1.5.2",
      enabled_features : [
        "system.metrics",
        "process.metrics",
        "network.metrics",
        "file.system.metrics"
      ],
      platform_specific_settings : [
        ("proc.path", @azimuth.StringValue("/proc")),
        ("sys.path", @azimuth.StringValue("/sys"))
      ]
    },
    @azimuth.PlatformConfig {
      platform : @azimuth.Platform::Windows,
      architecture : @azimuth.Architecture::X86_64,
      os_version : "Windows Server 2019",
      kernel_version : "10.0.17763.2114",
      telemetry_agent_version : "1.5.2",
      enabled_features : [
        "system.metrics",
        "process.metrics",
        "network.metrics",
        "windows.performance.counters"
      ],
      platform_specific_settings : [
        ("wmi.namespace", @azimuth.StringValue("root\\cimv2")),
        ("perf.counter.category", @azimuth.StringValue("Processor"))
      ]
    },
    @azimuth.PlatformConfig {
      platform : @azimuth.Platform::MacOS,
      architecture : @azimuth.Architecture::ARM64,
      os_version : "macOS 12.0.1",
      kernel_version : "21.1.0",
      telemetry_agent_version : "1.5.2",
      enabled_features : [
        "system.metrics",
        "process.metrics",
        "network.metrics"
      ],
      platform_specific_settings : [
        ("sysctl.path", @azimuth.StringValue("/usr/sbin/sysctl")),
        ("powermetrics.path", @azimuth.StringValue("/usr/bin/powermetrics"))
      ]
    }
  ]
  
  // 验证平台配置
  assert_eq(platform_configs.length(), 3)
  
  // 验证Linux配置
  let linux_config = platform_configs[0]
  match linux_config.platform {
    @azimuth.Platform::Linux => assert_true(true)
    _ => assert_true(false)
  }
  match linux_config.architecture {
    @azimuth.Architecture::X86_64 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(linux_config.os_version, "Ubuntu 20.04")
  assert_eq(linux_config.enabled_features.length(), 4)
  assert_eq(linux_config.platform_specific_settings.length(), 2)
  
  // 验证Windows配置
  let windows_config = platform_configs[1]
  match windows_config.platform {
    @azimuth.Platform::Windows => assert_true(true)
    _ => assert_true(false)
  }
  match windows_config.architecture {
    @azimuth.Architecture::X86_64 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(windows_config.os_version, "Windows Server 2019")
  assert_eq(windows_config.enabled_features.length(), 4)
  assert_eq(windows_config.platform_specific_settings.length(), 2)
  
  // 验证macOS配置
  let macos_config = platform_configs[2]
  match macos_config.platform {
    @azimuth.Platform::MacOS => assert_true(true)
    _ => assert_true(false)
  }
  match macos_config.architecture {
    @azimuth.Architecture::ARM64 => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(macos_config.os_version, "macOS 12.0.1")
  assert_eq(macos_config.enabled_features.length(), 3)
  assert_eq(macos_config.platform_specific_settings.length(), 2)
}

test "container environment compatibility" {
  // 测试容器环境兼容性
  let container_configs = [
    @azimuth.ContainerConfig {
      container_runtime : @azimuth.ContainerRuntime::Docker,
      runtime_version : "20.10.7",
      orchestrator : @azimuth.Orchestrator::Kubernetes,
      orchestrator_version : "1.22.1",
      cgroup_version : "v2",
      namespace : "production",
      pod_name : "payment-service-7d4f8c9b-xyz",
      pod_uid : "12345678-1234-1234-1234-123456789012",
      container_name : "payment-service",
      container_id : "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890",
      image : "payment-service:2.1.0",
      image_id : "sha256:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
      enabled_features : [
        "container.metrics",
        "cgroup.metrics",
        "network.metrics",
        "volume.metrics"
      ]
    },
    @azimuth.ContainerConfig {
      container_runtime : @azimuth.ContainerRuntime::Containerd,
      runtime_version : "1.4.9",
      orchestrator : @azimuth.Orchestrator::Kubernetes,
      orchestrator_version : "1.22.1",
      cgroup_version : "v2",
      namespace : "staging",
      pod_name : "user-service-8a5b7d8c-abc",
      pod_uid : "87654321-4321-4321-4321-210987654321",
      container_name : "user-service",
      container_id : "fedcba0987654321fedcba0987654321fedcba0987654321fedcba0987654321",
      image : "user-service:1.5.3",
      image_id : "sha256:fedcba0987654321fedcba0987654321fedcba0987654321fedcba0987654321",
      enabled_features : [
        "container.metrics",
        "cgroup.metrics",
        "network.metrics"
      ]
    }
  ]
  
  // 验证容器配置
  assert_eq(container_configs.length(), 2)
  
  // 验证Docker配置
  let docker_config = container_configs[0]
  match docker_config.container_runtime {
    @azimuth.ContainerRuntime::Docker => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(docker_config.runtime_version, "20.10.7")
  match docker_config.orchestrator {
    @azimuth.Orchestrator::Kubernetes => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(docker_config.orchestrator_version, "1.22.1")
  assert_eq(docker_config.cgroup_version, "v2")
  assert_eq(docker_config.namespace, "production")
  assert_eq(docker_config.pod_name, "payment-service-7d4f8c9b-xyz")
  assert_eq(docker_config.enabled_features.length(), 4)
  
  // 验证Containerd配置
  let containerd_config = container_configs[1]
  match containerd_config.container_runtime {
    @azimuth.ContainerRuntime::Containerd => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(containerd_config.runtime_version, "1.4.9")
  match containerd_config.orchestrator {
    @azimuth.Orchestrator::Kubernetes => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(containerd_config.namespace, "staging")
  assert_eq(containerd_config.pod_name, "user-service-8a5b7d8c-abc")
  assert_eq(containerd_config.enabled_features.length(), 3)
}

test "cloud platform integration" {
  // 测试云平台集成
  let cloud_configs = [
    @azimuth.CloudConfig {
      cloud_provider : @azimuth.CloudProvider::AWS,
      region : "us-west-2",
      availability_zone : "us-west-2a",
      instance_type : "m5.large",
      instance_id : "i-1234567890abcdef0",
      account_id : "123456789012",
      vpc_id : "vpc-12345678",
      subnet_id : "subnet-12345678",
      security_groups : ["sg-12345678", "sg-87654321"],
      iam_role : "telemetry-role",
      metadata_service_available : true,
      enabled_features : [
        "cloud.metrics",
        "cloud.watch.integration",
        "xray.tracing",
        "resource.tagging"
      ],
      cloud_specific_settings : [
        ("aws.region", @azimuth.StringValue("us-west-2")),
        ("aws.imdsv2", @azimuth.BoolValue(true))
      ]
    },
    @azimuth.CloudConfig {
      cloud_provider : @azimuth.CloudProvider::GCP,
      region : "us-central1",
      zone : "us-central1-a",
      instance_type : "n1-standard-2",
      instance_id : "1234567890123456789",
      project_id : "my-project-123456",
      vpc_name : "default",
      subnet_name : "default",
      service_account : "telemetry-sa@my-project-123456.iam.gserviceaccount.com",
      metadata_service_available : true,
      enabled_features : [
        "cloud.metrics",
        "stackdriver.monitoring",
        "cloud.trace",
        "resource.labels"
      ],
      cloud_specific_settings : [
        ("gcp.project", @azimuth.StringValue("my-project-123456")),
        ("gcp.zone", @azimuth.StringValue("us-central1-a"))
      ]
    }
  ]
  
  // 验证云配置
  assert_eq(cloud_configs.length(), 2)
  
  // 验证AWS配置
  let aws_config = cloud_configs[0]
  match aws_config.cloud_provider {
    @azimuth.CloudProvider::AWS => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(aws_config.region, "us-west-2")
  assert_eq(aws_config.availability_zone, "us-west-2a")
  assert_eq(aws_config.instance_type, "m5.large")
  assert_eq(aws_config.instance_id, "i-1234567890abcdef0")
  assert_eq(aws_config.account_id, "123456789012")
  assert_eq(aws_config.security_groups.length(), 2)
  assert_eq(aws_config.iam_role, "telemetry-role")
  assert_true(aws_config.metadata_service_available)
  assert_eq(aws_config.enabled_features.length(), 4)
  
  // 验证GCP配置
  let gcp_config = cloud_configs[1]
  match gcp_config.cloud_provider {
    @azimuth.CloudProvider::GCP => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(gcp_config.region, "us-central1")
  assert_eq(gcp_config.zone, "us-central1-a")
  assert_eq(gcp_config.instance_type, "n1-standard-2")
  assert_eq(gcp_config.instance_id, "1234567890123456789")
  assert_eq(gcp_config.project_id, "my-project-123456")
  assert_eq(gcp_config.service_account, "telemetry-sa@my-project-123456.iam.gserviceaccount.com")
  assert_true(gcp_config.metadata_service_available)
  assert_eq(gcp_config.enabled_features.length(), 4)
}

test "runtime environment adaptation" {
  // 测试运行时环境适配
  let runtime_environments = [
    @azimuth.RuntimeEnvironment {
      runtime_type : @azimuth.RuntimeType::NodeJS,
      runtime_version : "16.13.1",
      framework : @azimuth.Framework::Express,
      framework_version : "4.17.1",
      package_manager : @azimuth.PackageManager::NPM,
      package_manager_version : "8.1.2",
      telemetry_integration : @azimuth.TelemetryIntegration::OpenTelemetry,
      instrumentation_libraries : [
        @azimuth.InstrumentationLibrary {
          name : "@opentelemetry/instrumentation-http",
          version : "0.24.0",
          enabled : true
        },
        @azimuth.InstrumentationLibrary {
          name : "@opentelemetry/instrumentation-express",
          version : "0.24.0",
          enabled : true
        },
        @azimuth.InstrumentationLibrary {
          name : "@opentelemetry/instrumentation-mongodb",
          version : "0.24.0",
          enabled : true
        }
      ]
    },
    @azimuth.RuntimeEnvironment {
      runtime_type : @azimuth.RuntimeType::Java,
      runtime_version : "11.0.13",
      framework : @azimuth.Framework::SpringBoot,
      framework_version : "2.6.1",
      package_manager : @azimuth.PackageManager::Maven,
      package_manager_version : "3.8.4",
      telemetry_integration : @azimuth.TelemetryIntegration::OpenTelemetry,
      instrumentation_libraries : [
        @azimuth.InstrumentationLibrary {
          name : "opentelemetry-api",
          version : "1.10.1",
          enabled : true
        },
        @azimuth.InstrumentationLibrary {
          name : "opentelemetry-sdk",
          version : "1.10.1",
          enabled : true
        },
        @azimuth.InstrumentationLibrary {
          name : "opentelemetry-instrumentation-spring",
          version : "1.10.1-alpha",
          enabled : true
        }
      ]
    }
  ]
  
  // 验证运行时环境
  assert_eq(runtime_environments.length(), 2)
  
  // 验证Node.js环境
  let nodejs_env = runtime_environments[0]
  match nodejs_env.runtime_type {
    @azimuth.RuntimeType::NodeJS => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(nodejs_env.runtime_version, "16.13.1")
  match nodejs_env.framework {
    @azimuth.Framework::Express => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(nodejs_env.framework_version, "4.17.1")
  match nodejs_env.package_manager {
    @azimuth.PackageManager::NPM => assert_true(true)
    _ => assert_true(false)
  }
  match nodejs_env.telemetry_integration {
    @azimuth.TelemetryIntegration::OpenTelemetry => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(nodejs_env.instrumentation_libraries.length(), 3)
  
  // 验证Java环境
  let java_env = runtime_environments[1]
  match java_env.runtime_type {
    @azimuth.RuntimeType::Java => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(java_env.runtime_version, "11.0.13")
  match java_env.framework {
    @azimuth.Framework::SpringBoot => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(java_env.framework_version, "2.6.1")
  match java_env.package_manager {
    @azimuth.PackageManager::Maven => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(java_env.instrumentation_libraries.length(), 3)
}

test "resource constrained environment optimization" {
  // 测试资源受限环境优化
  let resource_constraints = [
    @azimuth.ResourceConstraint {
      environment_type : @azimuth.EnvironmentType::IoT,
      total_memory_mb : 512,
      available_memory_mb : 256,
      cpu_cores : 2,
      cpu_usage_threshold : 0.8,
      storage_mb : 4096,
      network_bandwidth_kbps : 1000,
      battery_powered : true,
      optimization_strategies : [
        @azimuth.OptimizationStrategy {
          strategy_type : @azimuth.StrategyType::Batching,
          enabled : true,
          parameters : [
            ("batch.size", @azimuth.IntValue(50)),
            ("batch.timeout.ms", @azimuth.IntValue(5000))
          ]
        },
        @azimuth.OptimizationStrategy {
          strategy_type : @azimuth.StrategyType::Sampling,
          enabled : true,
          parameters : [
            ("sampling.probability", @azimuth.FloatValue(0.1)),
            ("sampling.type", @azimuth.StringValue("probabilistic"))
          ]
        },
        @azimuth.OptimizationStrategy {
          strategy_type : @azimuth.StrategyType::Compression,
          enabled : true,
          parameters : [
            ("compression.algorithm", @azimuth.StringValue("gzip")),
            ("compression.level", @azimuth.IntValue(6))
          ]
        }
      ]
    },
    @azimuth.ResourceConstraint {
      environment_type : @azimuth.EnvironmentType::Edge,
      total_memory_mb : 2048,
      available_memory_mb : 1024,
      cpu_cores : 4,
      cpu_usage_threshold : 0.7,
      storage_mb : 16384,
      network_bandwidth_kbps : 10000,
      battery_powered : false,
      optimization_strategies : [
        @azimuth.OptimizationStrategy {
          strategy_type : @azimuth.StrategyType::Batching,
          enabled : true,
          parameters : [
            ("batch.size", @azimuth.IntValue(100)),
            ("batch.timeout.ms", @azimuth.IntValue(2000))
          ]
        },
        @azimuth.OptimizationStrategy {
          strategy_type : @azimuth.StrategyType::Sampling,
          enabled : true,
          parameters : [
            ("sampling.probability", @azimuth.FloatValue(0.5)),
            ("sampling.type", @azimuth.StringValue("probabilistic"))
          ]
        }
      ]
    }
  ]
  
  // 验证资源约束
  assert_eq(resource_constraints.length(), 2)
  
  // 验证IoT环境约束
  let iot_constraint = resource_constraints[0]
  match iot_constraint.environment_type {
    @azimuth.EnvironmentType::IoT => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(iot_constraint.total_memory_mb, 512)
  assert_eq(iot_constraint.available_memory_mb, 256)
  assert_eq(iot_constraint.cpu_cores, 2)
  assert_eq(iot_constraint.cpu_usage_threshold, 0.8)
  assert_eq(iot_constraint.storage_mb, 4096)
  assert_eq(iot_constraint.network_bandwidth_kbps, 1000)
  assert_true(iot_constraint.battery_powered)
  assert_eq(iot_constraint.optimization_strategies.length(), 3)
  
  // 验证Edge环境约束
  let edge_constraint = resource_constraints[1]
  match edge_constraint.environment_type {
    @azimuth.EnvironmentType::Edge => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(edge_constraint.total_memory_mb, 2048)
  assert_eq(edge_constraint.available_memory_mb, 1024)
  assert_eq(edge_constraint.cpu_cores, 4)
  assert_eq(edge_constraint.cpu_usage_threshold, 0.7)
  assert_eq(edge_constraint.storage_mb, 16384)
  assert_eq(edge_constraint.network_bandwidth_kbps, 10000)
  assert_false(edge_constraint.battery_powered)
  assert_eq(edge_constraint.optimization_strategies.length(), 2)
}