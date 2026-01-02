// 配置管理测试用例
// 测试Azimuth遥测系统的配置管理功能

test "基本配置加载和验证" {
  // 测试基本配置的加载和验证
  let config_manager = ConfigurationManager::new()
  
  // 测试默认配置
  let default_config = ConfigurationManager::get_default_config(config_manager)
  assert_true(ConfigurationManager::is_valid(default_config))
  
  // 验证默认配置值
  assert_eq(ConfigurationManager::get_service_name(default_config), "azimuth-service")
  assert_eq(ConfigurationManager::get_service_version(default_config), "1.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(default_config), 1.0)
  assert_eq(ConfigurationManager::get_export_timeout(default_config), 30000)
  
  // 测试配置覆盖
  let custom_config = ConfigurationManager::with_overrides(default_config, [
    ("service.name", "custom-service"),
    ("service.version", "2.0.0"),
    ("sampling.rate", "0.5"),
    ("export.timeout", "60000")
  ])
  
  // 验证配置覆盖
  assert_eq(ConfigurationManager::get_service_name(custom_config), "custom-service")
  assert_eq(ConfigurationManager::get_service_version(custom_config), "2.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(custom_config), 0.5)
  assert_eq(ConfigurationManager::get_export_timeout(custom_config), 60000)
  
  // 测试配置应用
  let tracer_provider = TracerProvider::with_config(custom_config)
  let tracer = TracerProvider::get_tracer(tracer_provider, "config.test")
  
  let span = Tracer::start_span(tracer, "config.test.operation")
  Span::set_attribute(span, "config.test", "basic.config")
  Span::end(span)
  
  assert_true(true)
}

test "环境变量配置" {
  // 测试从环境变量加载配置
  let config_manager = ConfigurationManager::new()
  
  // 模拟环境变量
  let env_config = ConfigurationManager::from_environment([
    ("AZIMUTH_SERVICE_NAME", "env-service"),
    ("AZIMUTH_SERVICE_VERSION", "3.0.0"),
    ("AZIMUTH_SAMPLING_RATE", "0.1"),
    ("AZIMUTH_EXPORT_ENDPOINT", "http://localhost:4317"),
    ("AZIMUTH_EXPORT_TIMEOUT", "45000"),
    ("AZIMUTH_BATCH_SIZE", "512"),
    ("AZIMUTH_MAX_QUEUE_SIZE", "2048")
  ])
  
  // 验证环境变量配置
  assert_eq(ConfigurationManager::get_service_name(env_config), "env-service")
  assert_eq(ConfigurationManager::get_service_version(env_config), "3.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(env_config), 0.1)
  assert_eq(ConfigurationManager::get_export_endpoint(env_config), Some("http://localhost:4317"))
  assert_eq(ConfigurationManager::get_export_timeout(env_config), 45000)
  assert_eq(ConfigurationManager::get_batch_size(env_config), 512)
  assert_eq(ConfigurationManager::get_max_queue_size(env_config), 2048)
  
  // 测试配置合并
  let merged_config = ConfigurationManager::merge([
    default_config,
    env_config
  ])
  
  // 验证配置合并优先级（环境变量优先）
  assert_eq(ConfigurationManager::get_service_name(merged_config), "env-service")
  assert_eq(ConfigurationManager::get_service_version(merged_config), "3.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(merged_config), 0.1)
  
  assert_true(true)
}

test "文件配置加载" {
  // 测试从文件加载配置
  let config_manager = ConfigurationManager::new()
  
  // 模拟JSON配置文件内容
  let json_config_content = "{\n    \"service\": {\n      \"name\": \"file-service\",\n      \"version\": \"4.0.0\"\n    },\n    \"sampling\": {\n      \"rate\": 0.8,\n      \"parent_based\": true\n    },\n    \"exporter\": {\n      \"endpoint\": \"http://otel-collector:4317\",\n      \"timeout\": 60000,\n      \"protocol\": \"grpc\"\n    },\n    \"batch\": {\n      \"size\": 1024,\n      \"timeout\": 5000\n    },\n    \"resource\": {\n      \"attributes\": {\n        \"deployment.environment\": \"production\",\n        \"host.name\": \"prod-server-01\",\n        \"region\": \"us-west-2\"\n      }\n    }\n  }"
  
  // 从JSON内容加载配置
  let file_config = ConfigurationManager::from_json(json_config_content)
  assert_true(ConfigurationManager::is_valid(file_config))
  
  // 验证文件配置
  assert_eq(ConfigurationManager::get_service_name(file_config), "file-service")
  assert_eq(ConfigurationManager::get_service_version(file_config), "4.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(file_config), 0.8)
  assert_eq(ConfigurationManager::is_parent_based_sampling(file_config), true)
  assert_eq(ConfigurationManager::get_export_endpoint(file_config), Some("http://otel-collector:4317"))
  assert_eq(ConfigurationManager::get_export_protocol(file_config), Some("grpc"))
  assert_eq(ConfigurationManager::get_batch_size(file_config), 1024)
  assert_eq(ConfigurationManager::get_batch_timeout(file_config), 5000)
  
  // 测试YAML配置文件内容
  let yaml_config_content = "\nservice:\n  name: yaml-service\n  version: 5.0.0\nsampling:\n  rate: 0.3\n  parent_based: false\nexporter:\n  endpoint: http://otel-collector:4318\n  timeout: 90000\n  protocol: http\nbatch:\n  size: 256\n  timeout: 10000\nresource:\n  attributes:\n    deployment.environment: staging\n    host.name: staging-server-01\n    region: us-east-1\n"
  
  // 从YAML内容加载配置
  let yaml_config = ConfigurationManager::from_yaml(yaml_config_content)
  assert_true(ConfigurationManager::is_valid(yaml_config))
  
  // 验证YAML配置
  assert_eq(ConfigurationManager::get_service_name(yaml_config), "yaml-service")
  assert_eq(ConfigurationManager::get_service_version(yaml_config), "5.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(yaml_config), 0.3)
  assert_eq(ConfigurationManager::is_parent_based_sampling(yaml_config), false)
  assert_eq(ConfigurationManager::get_export_endpoint(yaml_config), Some("http://otel-collector:4318"))
  assert_eq(ConfigurationManager::get_export_protocol(yaml_config), Some("http"))
  assert_eq(ConfigurationManager::get_batch_size(yaml_config), 256)
  assert_eq(ConfigurationManager::get_batch_timeout(yaml_config), 10000)
  
  assert_true(true)
}

test "动态配置更新" {
  // 测试动态配置更新
  let config_manager = ConfigurationManager::new()
  let initial_config = ConfigurationManager::get_default_config(config_manager)
  
  // 创建可更新的配置
  let dynamic_config = ConfigurationManager::make_dynamic(initial_config)
  
  // 应用初始配置
  let tracer_provider = TracerProvider::with_config(dynamic_config)
  let tracer = TracerProvider::get_tracer(tracer_provider, "dynamic.config.test")
  
  // 创建span使用初始配置
  let span1 = Tracer::start_span(tracer, "initial.config.span")
  Span::set_attribute(span1, "sampling.rate", Float::to_string(ConfigurationManager::get_sampling_rate(dynamic_config)))
  Span::end(span1)
  
  // 动态更新配置
  ConfigurationManager::update_dynamic(dynamic_config, [
    ("sampling.rate", "0.2"),
    ("batch.size", "2048"),
    ("export.timeout", "120000")
  ])
  
  // 创建span使用更新后的配置
  let span2 = Tracer::start_span(tracer, "updated.config.span")
  Span::set_attribute(span2, "sampling.rate", Float::to_string(ConfigurationManager::get_sampling_rate(dynamic_config)))
  Span::end(span2)
  
  // 验证配置已更新
  assert_eq(ConfigurationManager::get_sampling_rate(dynamic_config), 0.2)
  assert_eq(ConfigurationManager::get_batch_size(dynamic_config), 2048)
  assert_eq(ConfigurationManager::get_export_timeout(dynamic_config), 120000)
  
  // 测试配置变更通知
  let notification_received = ref false
  ConfigurationManager::on_config_change(dynamic_config, fn(old_config, new_config) {
    notification_received := true
    assert_eq(ConfigurationManager::get_sampling_rate(old_config), 1.0)
    assert_eq(ConfigurationManager::get_sampling_rate(new_config), 0.2)
  })
  
  // 再次更新配置
  ConfigurationManager::update_dynamic(dynamic_config, [
    ("sampling.rate", "0.7")
  ])
  
  // 验证通知被触发
  assert_true(notification_received)
  
  assert_true(true)
}

test "配置验证和错误处理" {
  // 测试配置验证和错误处理
  let config_manager = ConfigurationManager::new()
  
  // 测试无效配置
  let invalid_config = ConfigurationManager::with_overrides(
    ConfigurationManager::get_default_config(config_manager),
    [
      ("sampling.rate", "-0.1"),  // 负数采样率
      ("batch.size", "0"),       // 零批次大小
      ("export.timeout", "-1000") // 负超时
    ]
  )
  
  // 验证配置失败
  assert_false(ConfigurationManager::is_valid(invalid_config))
  
  // 测试配置验证错误
  let validation_errors = ConfigurationManager::validate(invalid_config)
  assert_true(Array::length(validation_errors) > 0)
  
  // 检查特定错误
  let has_sampling_error = Array::any(validation_errors, fn(error) {
    String::contains(error, "sampling.rate")
  })
  let has_batch_size_error = Array::any(validation_errors, fn(error) {
    String::contains(error, "batch.size")
  })
  let has_timeout_error = Array::any(validation_errors, fn(error) {
    String::contains(error, "export.timeout")
  })
  
  assert_true(has_sampling_error)
  assert_true(has_batch_size_error)
  assert_true(has_timeout_error)
  
  // 测试配置修复
  let fixed_config = ConfigurationManager::auto_fix(invalid_config)
  assert_true(ConfigurationManager::is_valid(fixed_config))
  
  // 验证修复后的值
  assert_eq(ConfigurationManager::get_sampling_rate(fixed_config), 0.0)  // 修复为0
  assert_eq(ConfigurationManager::get_batch_size(fixed_config), 1)      // 修复为1
  assert_eq(ConfigurationManager::get_export_timeout(fixed_config), 1000) // 修复为1000
  
  // 测试配置回退
  let fallback_config = ConfigurationManager::with_fallback(
    invalid_config,
    ConfigurationManager::get_default_config(config_manager)
  )
  
  assert_true(ConfigurationManager::is_valid(fallback_config))
  assert_eq(ConfigurationManager::get_service_name(fallback_config), "azimuth-service")
  
  assert_true(true)
}

test "配置继承和层次结构" {
  // 测试配置继承和层次结构
  let config_manager = ConfigurationManager::new()
  
  // 创建基础配置
  let base_config = ConfigurationManager::with_overrides(
    ConfigurationManager::get_default_config(config_manager),
    [
      ("service.name", "base-service"),
      ("service.version", "1.0.0"),
      ("sampling.rate", "0.5"),
      ("export.endpoint", "http://base-collector:4317")
    ]
  )
  
  // 创建环境配置（继承基础配置）
  let prod_config = ConfigurationManager::inherit(base_config, [
    ("service.name", "prod-service"),
    ("deployment.environment", "production"),
    ("export.endpoint", "http://prod-collector:4317")
  ])
  
  // 验证环境配置
  assert_eq(ConfigurationManager::get_service_name(prod_config), "prod-service")  // 覆盖
  assert_eq(ConfigurationManager::get_service_version(prod_config), "1.0.0")     // 继承
  assert_eq(ConfigurationManager::get_sampling_rate(prod_config), 0.5)           // 继承
  assert_eq(ConfigurationManager::get_export_endpoint(prod_config), Some("http://prod-collector:4317")) // 覆盖
  assert_eq(ConfigurationManager::get_deployment_environment(prod_config), Some("production")) // 新增
  
  // 创建服务特定配置
  let auth_service_config = ConfigurationManager::inherit(prod_config, [
    ("service.name", "auth-service"),
    ("service.type", "authentication"),
    ("sampling.rate", "1.0")  // 认证服务100%采样
  ])
  
  // 验证服务配置
  assert_eq(ConfigurationManager::get_service_name(auth_service_config), "auth-service")
  assert_eq(ConfigurationManager::get_service_version(auth_service_config), "1.0.0")
  assert_eq(ConfigurationManager::get_sampling_rate(auth_service_config), 1.0)
  assert_eq(ConfigurationManager::get_deployment_environment(auth_service_config), Some("production"))
  assert_eq(ConfigurationManager::get_service_type(auth_service_config), Some("authentication"))
  
  // 测试配置层次查询
  let config_hierarchy = ConfigurationManager::get_hierarchy(auth_service_config)
  assert_eq(Array::length(config_hierarchy), 3)  // base -> prod -> auth
  
  let base_layer = Array::get(config_hierarchy, 0)
  let prod_layer = Array::get(config_hierarchy, 1)
  let auth_layer = Array::get(config_hierarchy, 2)
  
  assert_eq(ConfigurationManager::get_service_name(base_layer), "base-service")
  assert_eq(ConfigurationManager::get_service_name(prod_layer), "prod-service")
  assert_eq(ConfigurationManager::get_service_name(auth_layer), "auth-service")
  
  assert_true(true)
}

test "配置模板和预设" {
  // 测试配置模板和预设
  let config_manager = ConfigurationManager::new()
  
  // 测试开发环境预设
  let dev_config = ConfigurationManager::use_preset("development")
  assert_eq(ConfigurationManager::get_deployment_environment(dev_config), Some("development"))
  assert_eq(ConfigurationManager::get_sampling_rate(dev_config), 1.0)  // 开发环境100%采样
  assert_eq(ConfigurationManager::get_debug_enabled(dev_config), true)
  
  // 测试生产环境预设
  let prod_config = ConfigurationManager::use_preset("production")
  assert_eq(ConfigurationManager::get_deployment_environment(prod_config), Some("production"))
  assert_eq(ConfigurationManager::get_sampling_rate(prod_config), 0.1)  // 生产环境10%采样
  assert_eq(ConfigurationManager::get_debug_enabled(prod_config), false)
  
  // 测试测试环境预设
  let test_config = ConfigurationManager::use_preset("testing")
  assert_eq(ConfigurationManager::get_deployment_environment(test_config), Some("testing"))
  assert_eq(ConfigurationManager::get_sampling_rate(test_config), 1.0)  // 测试环境100%采样
  assert_eq(ConfigurationManager::get_debug_enabled(test_config), true)
  
  // 创建自定义模板
  let custom_template = ConfigurationManager::create_template("custom", [
    ("service.name", "{{SERVICE_NAME}}"),
    ("service.version", "{{SERVICE_VERSION}}"),
    ("deployment.environment", "{{ENVIRONMENT}}"),
    ("sampling.rate", "{{SAMPLING_RATE}}"),
    ("export.endpoint", "{{EXPORT_ENDPOINT}}")
  ])
  
  // 应用模板并替换变量
  let template_config = ConfigurationManager::apply_template(custom_template, [
    ("SERVICE_NAME", "template-service"),
    ("SERVICE_VERSION", "2.0.0"),
    ("ENVIRONMENT", "staging"),
    ("SAMPLING_RATE", "0.3"),
    ("EXPORT_ENDPOINT", "http://staging-collector:4317")
  ])
  
  // 验证模板应用
  assert_eq(ConfigurationManager::get_service_name(template_config), "template-service")
  assert_eq(ConfigurationManager::get_service_version(template_config), "2.0.0")
  assert_eq(ConfigurationManager::get_deployment_environment(template_config), Some("staging"))
  assert_eq(ConfigurationManager::get_sampling_rate(template_config), 0.3)
  assert_eq(ConfigurationManager::get_export_endpoint(template_config), Some("http://staging-collector:4317"))
  
  assert_true(true)
}