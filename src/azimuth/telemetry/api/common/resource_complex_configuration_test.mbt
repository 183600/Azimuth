// Resource复杂配置测试
test "resource_complex_configuration" {
  // 测试Resource的复杂配置场景
  
  // 基本Resource创建
  let basic_resource = Resource::default("basic-service")
  assert_eq(basic_resource.service_name, "basic-service")
  assert_eq(basic_resource.service_version, None)
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 测试复杂的服务名称
  let complex_names = [
    "my-complex-service.v2",
    "special-service!@#$%^&*()",
    "Unicode服务测试🚀",
    "",
    "service with spaces",
    "service.with.many.dots",
    "service_with_underscores",
    "service-with-hyphens",
    "123numeric-service",
    "UPPERCASE-SERVICE",
    "mixed-Case-Service",
    "a".repeat(1000)  // 非常长的服务名
  ]
  
  let mut i = 0
  while i < complex_names.length() {
    let resource = Resource::default(complex_names[i])
    assert_eq(resource.service_name, complex_names[i])
    assert_eq(resource.telemetry_sdk_name, "azimuth")
    i = i + 1
  }
}

test "resource_attributes_configuration" {
  // 测试Resource属性的复杂配置
  
  let resource = Resource::default("attribute-test-service")
  
  // 创建复杂的属性集合
  let complex_attributes : Attributes = [
    // 服务基本信息
    ("service.name", AttributeValue::string("attribute-test-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("service.namespace", AttributeValue::string("production")),
    ("service.instance.id", AttributeValue::string("instance-123456789")),
    
    // 部署信息
    ("deployment.environment", AttributeValue::string("prod")),
    ("deployment.region", AttributeValue::string("us-west-2")),
    ("deployment.zone", AttributeValue::string("us-west-2a")),
    ("deployment.cluster", AttributeValue::string("main-cluster")),
    
    // 主机信息
    ("host.name", AttributeValue::string("prod-web-server-01")),
    ("host.id", AttributeValue::string("host-abcdef123456")),
    ("host.type", AttributeValue::string("c5.large")),
    ("host.image.name", AttributeValue::string("ami-1234567890abcdef0")),
    ("host.image.id", AttributeValue::string("ami-1234567890abcdef0")),
    ("host.image.version", AttributeValue::string("v1.2.3")),
    
    // 进程信息
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable.name", AttributeValue::string("my-service")),
    ("process.executable.path", AttributeValue::string("/usr/local/bin/my-service")),
    ("process.command", AttributeValue::string("my-service --config /etc/my-service/config.yaml")),
    ("process.command_line", AttributeValue::string("my-service --config /etc/my-service/config.yaml --log-level info")),
    ("process.command_args", AttributeValue::array_string(["--config", "/etc/my-service/config.yaml", "--log-level", "info"])),
    
    // 容器信息
    ("container.name", AttributeValue::string("my-service-container")),
    ("container.id", AttributeValue::string("container-abcdef123456")),
    ("container.image.name", AttributeValue::string("mycompany/my-service:2.1.0")),
    ("container.image.tag", AttributeValue::string("2.1.0")),
    
    // Kubernetes信息
    ("k8s.pod.name", AttributeValue::string("my-service-7d4f8c9b-12345")),
    ("k8s.pod.uid", AttributeValue::string("12345678-1234-1234-1234-123456789012")),
    ("k8s.namespace.name", AttributeValue::string("production")),
    ("k8s.deployment.name", AttributeValue::string("my-service")),
    ("k8s.replicaset.name", AttributeValue::string("my-service-7d4f8c9b")),
    ("k8s.node.name", AttributeValue::string("ip-10-0-1-123.us-west-2.compute.internal")),
    ("k8s.service.name", AttributeValue::string("my-service")),
    
    // 云提供商信息
    ("cloud.provider", AttributeValue::string("aws")),
    ("cloud.account.id", AttributeValue::string("123456789012")),
    ("cloud.region", AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", AttributeValue::string("us-west-2a")),
    ("cloud.platform", AttributeValue::string("aws_ec2")),
    
    // 性能指标
    ("process.cpu.percent", AttributeValue::float(75.5)),
    ("process.memory.rss", AttributeValue::int(1073741824L)),
    ("process.memory.vms", AttributeValue::int(2147483648L)),
    ("process.cpu.time", AttributeValue::float(12345.67)),
    ("process.threads", AttributeValue::int(10L)),
    ("process.open_fds", AttributeValue::int(50L)),
    
    // 网络信息
    ("network.interface.name", AttributeValue::string("eth0")),
    ("network.interface.address", AttributeValue::string("10.0.1.123")),
    ("network.interface.prefix_length", AttributeValue::int(24L)),
    ("network.mac_address", AttributeValue::string("02:42:ac:11:00:02")),
    
    // 业务标签
    ("business.unit", AttributeValue::string("payments")),
    ("business.criticality", AttributeValue::string("critical")),
    ("business.owner", AttributeValue::string("payments-team")),
    ("business.sla", AttributeValue::string("99.9")),
    
    // 数组属性
    ("service.tags", AttributeValue::array_string(["api", "payment", "critical", "production"])),
    ("service.ports", AttributeValue::array_int([8080L, 8443L, 9090L, 9091L])),
    ("service.thresholds", AttributeValue::array_float([60.0, 80.0, 95.0, 99.0])),
    ("service.features", AttributeValue::array_bool([true, false, true, false, true])),
    ("process.environment_vars", AttributeValue::array_string(["NODE_ENV=production", "LOG_LEVEL=info", "PORT=8080"])),
    ("k8s.labels", AttributeValue::array_string(["app=my-service", "version=2.1.0", "environment=production"])),
    
    // 特殊值
    ("process.healthy", AttributeValue::bool(true)),
    ("service.maintenance", AttributeValue::bool(false)),
    ("feature.flags", AttributeValue::array_bool([true, true, false, true, false, true])),
    
    // 边界值
    ("process.max_fds", AttributeValue::int(65536L)),
    ("process.uptime", AttributeValue::float(86400.0)),
    ("service.start_time", AttributeValue::int(1234567890L))
  ]
  
  // 验证所有属性都存在
  assert_eq(complex_attributes.length(), 75)
  
  // 验证特定类型的属性
  let mut found_service_name = false
  let mut found_process_pid = false
  let mut found_process_cpu_percent = false
  let mut found_process_healthy = false
  let mut found_service_tags = false
  let mut found_service_ports = false
  let mut found_service_thresholds = false
  let mut found_service_features = false
  
  let mut i = 0
  while i < complex_attributes.length() {
    let (key, value) = complex_attributes[i]
    
    match key {
      "service.name" => {
        match value {
          StringValue(name) => {
            assert_eq(name, "attribute-test-service")
            found_service_name = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.pid" => {
        match value {
          IntValue(pid) => {
            assert_eq(pid, 12345L)
            found_process_pid = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.cpu.percent" => {
        match value {
          FloatValue(cpu) => {
            assert_eq(cpu, 75.5)
            found_process_cpu_percent = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "process.healthy" => {
        match value {
          BoolValue(healthy) => {
            assert_eq(healthy, true)
            found_process_healthy = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "service.tags" => {
        match value {
          ArrayStringValue(tags) => {
            assert_eq(tags.length(), 4)
            assert_eq(tags[0], "api")
            assert_eq(tags[3], "production")
            found_service_tags = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "service.ports" => {
        match value {
          ArrayIntValue(ports) => {
            assert_eq(ports.length(), 4)
            assert_eq(ports[0], 8080L)
            assert_eq(ports[3], 9091L)
            found_service_ports = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "service.thresholds" => {
        match value {
          ArrayFloatValue(thresholds) => {
            assert_eq(thresholds.length(), 4)
            assert_eq(thresholds[0], 60.0)
            assert_eq(thresholds[3], 99.0)
            found_service_thresholds = true
          }
          _ => @test.fail("Test failed")
        }
      }
      "service.features" => {
        match value {
          ArrayBoolValue(features) => {
            assert_eq(features.length(), 5)
            assert_eq(features[0], true)
            assert_eq(features[4], true)
            found_service_features = true
          }
          _ => @test.fail("Test failed")
        }
      }
      _ => () // 忽略其他属性
    }
    
    i = i + 1
  }
  
  // 验证所有重要属性都找到了
  assert_eq(found_service_name, true)
  assert_eq(found_process_pid, true)
  assert_eq(found_process_cpu_percent, true)
  assert_eq(found_process_healthy, true)
  assert_eq(found_service_tags, true)
  assert_eq(found_service_ports, true)
  assert_eq(found_service_thresholds, true)
  assert_eq(found_service_features = true, true)
}

test "resource_edge_cases_configuration" {
  // 测试Resource的边界情况配置
  
  // 测试极值
  let max_int_attr = AttributeValue::int(9223372036854775807L)
  let min_int_attr = AttributeValue::int(-9223372036854775808L)
  let zero_int_attr = AttributeValue::int(0L)
  
  let max_float_attr = AttributeValue::float(1.7976931348623157e+308)
  let min_float_attr = AttributeValue::float(-1.7976931348623157e+308)
  let inf_float_attr = AttributeValue::float(1.0/0.0)
  let nan_float_attr = AttributeValue::float(0.0/0.0)
  
  let empty_string_attr = AttributeValue::string("")
  let very_long_string_attr = AttributeValue::string("a".repeat(10000))
  let unicode_string_attr = AttributeValue::string("测试中文🚀Unicode")
  
  let empty_array_attr = AttributeValue::array_string([])
  let large_array_attr = AttributeValue::array_string([for i = 0; i < 1000; i = i + 1].map(fn(_) { "item" }))
  
  // 创建包含边界值的Resource
  let edge_resource = Resource::default("edge-case-service")
  let edge_attributes : Attributes = [
    ("max.int.value", max_int_attr),
    ("min.int.value", min_int_attr),
    ("zero.int.value", zero_int_attr),
    ("max.float.value", max_float_attr),
    ("min.float.value", min_float_attr),
    ("inf.float.value", inf_float_attr),
    ("nan.float.value", nan_float_attr),
    ("empty.string", empty_string_attr),
    ("very.long.string", very_long_string_attr),
    ("unicode.string", unicode_string_attr),
    ("empty.array", empty_array_attr),
    ("large.array", large_array_attr)
  ]
  
  // 验证边界值属性
  assert_eq(edge_attributes.length(), 12)
  
  // 验证极值
  match edge_attributes[0].1 {  // max.int.value
    IntValue(val) => assert_eq(val, 9223372036854775807L)
    _ => @test.fail("Test failed")
  }
  
  match edge_attributes[1].1 {  // min.int.value
    IntValue(val) => assert_eq(val, -9223372036854775808L)
    _ => @test.fail("Test failed")
  }
  
  match edge_attributes[7].1 {  // empty.string
    StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Test failed")
  }
  
  match edge_attributes[8].1 {  // very.long.string
    StringValue(s) => assert_eq(s.length(), 10000)
    _ => @test.fail("Test failed")
  }
  
  match edge_attributes[11].1 {  // large.array
    ArrayStringValue(arr) => assert_eq(arr.length(), 1000)
    _ => @test.fail("Test failed")
  }
}

test "resource_multi_environment_configuration" {
  // 测试多环境Resource配置
  
  // 开发环境
  let dev_resource = Resource::default("my-service-dev")
  let dev_attributes : Attributes = [
    ("service.name", AttributeValue::string("my-service")),
    ("service.version", AttributeValue::string("1.0.0-dev")),
    ("deployment.environment", AttributeValue::string("development")),
    ("deployment.region", AttributeValue::string("local")),
    ("host.name", AttributeValue::string("localhost")),
    ("process.pid", AttributeValue::int(12345L)),
    ("service.debug", AttributeValue::bool(true)),
    ("service.ports", AttributeValue::array_int([3000L, 3001L])),
    ("service.tags", AttributeValue::array_string(["dev", "debug", "local"]))
  ]
  
  // 测试环境
  let test_resource = Resource::default("my-service-test")
  let test_attributes : Attributes = [
    ("service.name", AttributeValue::string("my-service")),
    ("service.version", AttributeValue::string("1.0.0-test")),
    ("deployment.environment", AttributeValue::string("testing")),
    ("deployment.region", AttributeValue::string("test-region")),
    ("host.name", AttributeValue::string("test-server-01")),
    ("process.pid", AttributeValue::int(54321L)),
    ("service.debug", AttributeValue::bool(true)),
    ("service.ports", AttributeValue::array_int([8000L, 8001L])),
    ("service.tags", AttributeValue::array_string(["test", "automated", "ci"]))
  ]
  
  // 预发布环境
  let staging_resource = Resource::default("my-service-staging")
  let staging_attributes : Attributes = [
    ("service.name", AttributeValue::string("my-service")),
    ("service.version", AttributeValue::string("1.0.0-rc")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("deployment.region", AttributeValue::string("staging-region")),
    ("host.name", AttributeValue::string("staging-server-01")),
    ("process.pid", AttributeValue::int(98765L)),
    ("service.debug", AttributeValue::bool(false)),
    ("service.ports", AttributeValue::array_int([8080L, 8443L])),
    ("service.tags", AttributeValue::array_string(["staging", "pre-production", "integration"]))
  ]
  
  // 生产环境
  let prod_resource = Resource::default("my-service-prod")
  let prod_attributes : Attributes = [
    ("service.name", AttributeValue::string("my-service")),
    ("service.version", AttributeValue::string("1.0.0")),
    ("deployment.environment", AttributeValue::string("production")),
    ("deployment.region", AttributeValue::string("prod-region")),
    ("host.name", AttributeValue::string("prod-server-01")),
    ("process.pid", AttributeValue::int(11111L)),
    ("service.debug", AttributeValue::bool(false)),
    ("service.ports", AttributeValue::array_int([8080L, 8443L, 9090L])),
    ("service.tags", AttributeValue::array_string(["prod", "critical", "high-availability"]))
  ]
  
  // 验证不同环境的配置
  assert_eq(dev_resource.service_name, "my-service-dev")
  assert_eq(test_resource.service_name, "my-service-test")
  assert_eq(staging_resource.service_name, "my-service-staging")
  assert_eq(prod_resource.service_name, "my-service-prod")
  
  assert_eq(dev_attributes.length(), 8)
  assert_eq(test_attributes.length(), 8)
  assert_eq(staging_attributes.length(), 8)
  assert_eq(prod_attributes.length(), 8)
  
  // 验证环境特定的属性值
  match dev_attributes[2].1 {  // deployment.environment
    StringValue(env) => assert_eq(env, "development")
    _ => @test.fail("Test failed")
  }
  
  match test_attributes[2].1 {  // deployment.environment
    StringValue(env) => assert_eq(env, "testing")
    _ => @test.fail("Test failed")
  }
  
  match staging_attributes[2].1 {  // deployment.environment
    StringValue(env) => assert_eq(env, "staging")
    _ => @test.fail("Test failed")
  }
  
  match prod_attributes[2].1 {  // deployment.environment
    StringValue(env) => assert_eq(env, "production")
    _ => @test.fail("Test failed")
  }
}

test "resource_microservices_configuration" {
  // 测试微服务架构的Resource配置
  
  // API网关
  let gateway_resource = Resource::default("api-gateway")
  let gateway_attributes : Attributes = [
    ("service.name", AttributeValue::string("api-gateway")),
    ("service.type", AttributeValue::string("gateway")),
    ("service.component", AttributeValue::string("nginx")),
    ("deployment.environment", AttributeValue::string("production")),
    ("k8s.namespace.name", AttributeValue::string("gateway")),
    ("k8s.service.name", AttributeValue::string("api-gateway")),
    ("network.interface.address", AttributeValue::string("10.0.0.10")),
    ("service.ports", AttributeValue::array_int([80L, 443L])),
    ("service.tags", AttributeValue::array_string(["gateway", "edge", "public"]))
  ]
  
  // 用户服务
  let user_service_resource = Resource::default("user-service")
  let user_service_attributes : Attributes = [
    ("service.name", AttributeValue::string("user-service")),
    ("service.type", AttributeValue::string("microservice")),
    ("service.component", AttributeValue::string("nodejs")),
    ("deployment.environment", AttributeValue::string("production")),
    ("k8s.namespace.name", AttributeValue::string("backend")),
    ("k8s.service.name", AttributeValue::string("user-service")),
    ("database.system", AttributeValue::string("postgresql")),
    ("database.name", AttributeValue::string("users_db")),
    ("service.ports", AttributeValue::array_int([3000L])),
    ("service.tags", AttributeValue::array_string(["user", "auth", "backend"]))
  ]
  
  // 订单服务
  let order_service_resource = Resource::default("order-service")
  let order_service_attributes : Attributes = [
    ("service.name", AttributeValue::string("order-service")),
    ("service.type", AttributeValue::string("microservice")),
    ("service.component", AttributeValue::string("java")),
    ("deployment.environment", AttributeValue::string("production")),
    ("k8s.namespace.name", AttributeValue::string("backend")),
    ("k8s.service.name", AttributeValue::string("order-service")),
    ("database.system", AttributeValue::string("mongodb")),
    ("database.name", AttributeValue::string("orders_db")),
    ("messaging.system", AttributeValue::string("kafka")),
    ("messaging.topic", AttributeValue::string("orders")),
    ("service.ports", AttributeValue::array_int([8080L])),
    ("service.tags", AttributeValue::array_string(["order", "business", "backend"]))
  ]
  
  // 支付服务
  let payment_service_resource = Resource::default("payment-service")
  let payment_service_attributes : Attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.type", AttributeValue::string("microservice")),
    ("service.component", AttributeValue::string("python")),
    ("deployment.environment", AttributeValue::string("production")),
    ("k8s.namespace.name", AttributeValue::string("payment")),
    ("k8s.service.name", AttributeValue::string("payment-service")),
    ("database.system", AttributeValue::string("postgresql")),
    ("database.name", AttributeValue::string("payments_db")),
    ("payment.gateway", AttributeValue::string("stripe")),
    ("payment.version", AttributeValue::string("v3")),
    ("service.ports", AttributeValue::array_int([5000L])),
    ("service.tags", AttributeValue::array_string(["payment", "critical", "financial"]))
  ]
  
  // 通知服务
  let notification_service_resource = Resource::default("notification-service")
  let notification_service_attributes : Attributes = [
    ("service.name", AttributeValue::string("notification-service")),
    ("service.type", AttributeValue::string("microservice")),
    ("service.component", AttributeValue::string("go")),
    ("deployment.environment", AttributeValue::string("production")),
    ("k8s.namespace.name", AttributeValue::string("notification")),
    ("k8s.service.name", AttributeValue::string("notification-service")),
    ("messaging.system", AttributeValue::string("rabbitmq")),
    ("messaging.exchange", AttributeValue::string("notifications")),
    ("email.provider", AttributeValue::string("sendgrid")),
    ("sms.provider", AttributeValue::string("twilio")),
    ("service.ports", AttributeValue::array_int([6000L])),
    ("service.tags", AttributeValue::array_string(["notification", "communication", "async"]))
  ]
  
  // 验证微服务配置
  assert_eq(gateway_resource.service_name, "api-gateway")
  assert_eq(user_service_resource.service_name, "user-service")
  assert_eq(order_service_resource.service_name, "order-service")
  assert_eq(payment_service_resource.service_name, "payment-service")
  assert_eq(notification_service_resource.service_name, "notification-service")
  
  assert_eq(gateway_attributes.length(), 9)
  assert_eq(user_service_attributes.length(), 9)
  assert_eq(order_service_attributes.length(), 11)
  assert_eq(payment_service_attributes.length(), 11)
  assert_eq(notification_service_attributes.length(), 11)
  
  // 验证服务类型
  match gateway_attributes[1].1 {  // service.type
    StringValue(service_type) => assert_eq(service_type, "gateway")
    _ => @test.fail("Test failed")
  }
  
  match user_service_attributes[1].1 {  // service.type
    StringValue(service_type) => assert_eq(service_type, "microservice")
    _ => @test.fail("Test failed")
  }
}

test "resource_performance_configuration" {
  // 测试性能相关的Resource配置
  
  // 高性能计算服务
  let hpc_resource = Resource::default("hpc-compute-service")
  let hpc_attributes : Attributes = [
    ("service.name", AttributeValue::string("hpc-compute-service")),
    ("service.type", AttributeValue::string("compute")),
    ("performance.tier", AttributeValue::string("high")),
    ("cpu.count", AttributeValue::int(64L)),
    ("cpu.architecture", AttributeValue::string("x86_64")),
    ("memory.total", AttributeValue::int(268435456000L)),  // 256GB
    ("gpu.count", AttributeValue::int(8L)),
    ("gpu.type", AttributeValue::string("NVIDIA V100")),
    ("gpu.memory.total", AttributeValue::int(32768000000L)),  // 32GB each
    ("storage.type", AttributeValue::string("nvme")),
    ("storage.iops", AttributeValue::int(100000L)),
    ("network.bandwidth", AttributeValue::int(100000L)),  // 100Gbps
    ("service.ports", AttributeValue::array_int([8080L, 9090L])),
    ("service.tags", AttributeValue::array_string(["hpc", "compute", "gpu", "high-performance"]))
  ]
  
  // 低延迟交易服务
  let latency_critical_resource = Resource::default("trading-service")
  let latency_critical_attributes : Attributes = [
    ("service.name", AttributeValue::string("trading-service")),
    ("service.type", AttributeValue::string("trading")),
    ("performance.tier", AttributeValue::string("latency-critical")),
    ("cpu.count", AttributeValue::int(32L)),
    ("cpu.architecture", AttributeValue::string("x86_64")),
    ("memory.total", AttributeValue::int(134217728000L)),  // 128GB
    ("memory.type", AttributeValue::string("DDR4-3200")),
    ("network.latency.target", AttributeValue::float(0.001)),  // 1ms
    ("network.bandwidth", AttributeValue::int(10000L)),  // 10Gbps
    ("storage.type", AttributeValue::string("nvme")),
    ("storage.latency.target", AttributeValue::float(0.0001)),  // 0.1ms
    ("service.ports", AttributeValue::array_int([8080L, 8443L])),
    ("service.tags", AttributeValue::array_string(["trading", "low-latency", "financial", "critical"]))
  ]
  
  // 批处理服务
  let batch_resource = Resource::default("batch-processing-service")
  let batch_attributes : Attributes = [
    ("service.name", AttributeValue::string("batch-processing-service")),
    ("service.type", AttributeValue::string("batch")),
    ("performance.tier", AttributeValue::string("throughput")),
    ("cpu.count", AttributeValue::int(16L)),
    ("memory.total", AttributeValue::int(67108864000L)),  // 64GB
    ("storage.type", AttributeValue::string("hdd")),
    ("storage.capacity", AttributeValue::int(10000000000000L)),  // 10TB
    ("batch.max.concurrent.jobs", AttributeValue::int(100L)),
    ("batch.max.job.size", AttributeValue::int(1000000L)),
    ("service.ports", AttributeValue::array_int([8080L])),
    ("service.tags", AttributeValue::array_string(["batch", "processing", "throughput", "background"]))
  ]
  
  // 验证性能配置
  assert_eq(hpc_resource.service_name, "hpc-compute-service")
  assert_eq(latency_critical_resource.service_name, "trading-service")
  assert_eq(batch_resource.service_name, "batch-processing-service")
  
  assert_eq(hpc_attributes.length(), 14)
  assert_eq(latency_critical_attributes.length(), 13)
  assert_eq(batch_attributes.length(), 10)
  
  // 验证性能特定属性
  match hpc_attributes[6].1 {  // gpu.count
    IntValue(gpu_count) => assert_eq(gpu_count, 8L)
    _ => @test.fail("Test failed")
  }
  
  match latency_critical_attributes[6].1 {  // network.latency.target
    FloatValue(latency) => assert_eq(latency, 0.001)
    _ => @test.fail("Test failed")
  }
  
  match batch_attributes[6].1 {  // batch.max.concurrent.jobs
    IntValue(max_jobs) => assert_eq(max_jobs, 100L)
    _ => @test.fail("Test failed")
  }
}