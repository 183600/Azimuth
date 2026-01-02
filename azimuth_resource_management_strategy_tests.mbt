// 资源管理和合并策略测试用例
// 测试Azimuth遥测系统的资源创建、管理和合并功能

test "资源创建和基本属性管理" {
  // 创建基础资源
  let resource1 = Resource::builder()
    .with_service_name("azimuth-service")
    .with_service_version("1.0.0")
    .with_service_instance_id("instance-001")
    .with_attribute("environment", "production")
    .with_attribute("region", "us-west")
    .with_attribute("zone", "us-west-a")
    .build()
  
  // 验证资源属性
  assert_eq(Resource::get_attribute(resource1, "service.name"), Some("azimuth-service"))
  assert_eq(Resource::get_attribute(resource1, "service.version"), Some("1.0.0"))
  assert_eq(Resource::get_attribute(resource1, "service.instance.id"), Some("instance-001"))
  assert_eq(Resource::get_attribute(resource1, "environment"), Some("production"))
  assert_eq(Resource::get_attribute(resource1, "region"), Some("us-west"))
  assert_eq(Resource::get_attribute(resource1, "zone"), Some("us-west-a"))
  
  // 验证属性数量
  let attributes = Resource::get_all_attributes(resource1)
  assert_eq(attributes.length(), 6)
  
  // 测试属性更新
  let resource1_updated = Resource::with_attribute(resource1, "environment", "staging")
  assert_eq(Resource::get_attribute(resource1_updated, "environment"), Some("staging"))
  
  // 验证原始资源不变
  assert_eq(Resource::get_attribute(resource1, "environment"), Some("production"))
  
  assert_true(true)
}

test "资源合并策略 - 默认合并" {
  // 创建第一个资源
  let resource_a = Resource::builder()
    .with_service_name("service-a")
    .with_service_version("2.1.0")
    .with_attribute("environment", "development")
    .with_attribute("team", "backend")
    .with_attribute("cost_center", "engineering")
    .build()
  
  // 创建第二个资源
  let resource_b = Resource::builder()
    .with_service_name("service-b")
    .with_service_version("3.0.0")
    .with_attribute("environment", "production")
    .with_attribute("region", "eu-central")
    .with_attribute("data_center", "frankfurt")
    .build()
  
  // 使用默认合并策略
  let merger = ResourceMerger::new(ResourceMergeStrategy::Default)
  let merged_resource = ResourceMerger::merge(merger, resource_a, resource_b)
  
  // 验证合并结果 - 第二个资源应该覆盖第一个资源的相同属性
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some("service-b"))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some("3.0.0"))
  assert_eq(Resource::get_attribute(merged_resource, "environment"), Some("production"))
  
  // 验证合并的独有属性
  assert_eq(Resource::get_attribute(merged_resource, "team"), Some("backend"))
  assert_eq(Resource::get_attribute(merged_resource, "cost_center"), Some("engineering"))
  assert_eq(Resource::get_attribute(merged_resource, "region"), Some("eu-central"))
  assert_eq(Resource::get_attribute(merged_resource, "data_center"), Some("frankfurt"))
  
  // 验证总属性数量
  let merged_attributes = Resource::get_all_attributes(merged_resource)
  assert_eq(merged_attributes.length(), 7)
  
  assert_true(true)
}

test "资源合并策略 - 保留现有" {
  // 创建基础资源
  let base_resource = Resource::builder()
    .with_service_name("base-service")
    .with_service_version("1.0.0")
    .with_attribute("environment", "production")
    .with_attribute("region", "us-east")
    .with_attribute("critical", "true")
    .build()
  
  // 创建覆盖资源
  let override_resource = Resource::builder()
    .with_service_name("override-service")
    .with_service_version("2.0.0")
    .with_attribute("environment", "staging")
    .with_attribute("team", "platform")
    .with_attribute("temporary", "true")
    .build()
  
  // 使用保留现有策略
  let merger = ResourceMerger::new(ResourceMergeStrategy::KeepExisting)
  let merged_resource = ResourceMerger::merge(merger, base_resource, override_resource)
  
  // 验证合并结果 - 应该保留第一个资源的属性
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some("base-service"))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some("1.0.0"))
  assert_eq(Resource::get_attribute(merged_resource, "environment"), Some("production"))
  assert_eq(Resource::get_attribute(merged_resource, "region"), Some("us-east"))
  assert_eq(Resource::get_attribute(merged_resource, "critical"), Some("true"))
  
  // 验证添加的新属性
  assert_eq(Resource::get_attribute(merged_resource, "team"), Some("platform"))
  assert_eq(Resource::get_attribute(merged_resource, "temporary"), Some("true"))
  
  // 验证总属性数量
  let merged_attributes = Resource::get_all_attributes(merged_resource)
  assert_eq(merged_attributes.length(), 7)
  
  assert_true(true)
}

test "多级资源合并" {
  // 创建多个资源
  let resource1 = Resource::builder()
    .with_service_name("microservice-a")
    .with_service_version("1.2.3")
    .with_attribute("environment", "production")
    .with_attribute("region", "us-west")
    .build()
  
  let resource2 = Resource::builder()
    .with_service_instance_id("pod-12345")
    .with_attribute("kubernetes.namespace", "default")
    .with_attribute("kubernetes.pod.name", "microservice-a-7d4b8c9f")
    .build()
  
  let resource3 = Resource::builder()
    .with_attribute("host.name", "worker-node-1")
    .with_attribute("host.ip", "10.0.1.100")
    .with_attribute("availability_zone", "us-west-1a")
    .build()
  
  let resource4 = Resource::builder()
    .with_attribute("business_unit", "platform")
    .with_attribute("cost_center", "engineering-infrastructure")
    .with_attribute("sla_tier", "gold")
    .build()
  
  // 逐级合并所有资源
  let merger = ResourceMerger::new(ResourceMergeStrategy::Default)
  let merged1 = ResourceMerger::merge(merger, resource1, resource2)
  let merged2 = ResourceMerger::merge(merger, merged1, resource3)
  let final_merged = ResourceMerger::merge(merger, merged2, resource4)
  
  // 验证所有属性都存在
  assert_eq(Resource::get_attribute(final_merged, "service.name"), Some("microservice-a"))
  assert_eq(Resource::get_attribute(final_merged, "service.version"), Some("1.2.3"))
  assert_eq(Resource::get_attribute(final_merged, "environment"), Some("production"))
  assert_eq(Resource::get_attribute(final_merged, "region"), Some("us-west"))
  assert_eq(Resource::get_attribute(final_merged, "service.instance.id"), Some("pod-12345"))
  assert_eq(Resource::get_attribute(final_merged, "kubernetes.namespace"), Some("default"))
  assert_eq(Resource::get_attribute(final_merged, "kubernetes.pod.name"), Some("microservice-a-7d4b8c9f"))
  assert_eq(Resource::get_attribute(final_merged, "host.name"), Some("worker-node-1"))
  assert_eq(Resource::get_attribute(final_merged, "host.ip"), Some("10.0.1.100"))
  assert_eq(Resource::get_attribute(final_merged, "availability_zone"), Some("us-west-1a"))
  assert_eq(Resource::get_attribute(final_merged, "business_unit"), Some("platform"))
  assert_eq(Resource::get_attribute(final_merged, "cost_center"), Some("engineering-infrastructure"))
  assert_eq(Resource::get_attribute(final_merged, "sla_tier"), Some("gold"))
  
  // 验证总属性数量
  let final_attributes = Resource::get_all_attributes(final_merged)
  assert_eq(final_attributes.length(), 13)
  
  assert_true(true)
}

test "资源与遥测组件集成" {
  // 创建资源
  let telemetry_resource = Resource::builder()
    .with_service_name("azimuth-telemetry-service")
    .with_service_version("2.5.0")
    .with_service_instance_id("telemetry-instance-001")
    .with_attribute("environment", "production")
    .with_attribute("region", "us-east-1")
    .with_attribute("team", "observability")
    .build()
  
  // 创建带有资源的TracerProvider
  let tracer_provider = TracerProvider::builder()
    .with_resource(telemetry_resource)
    .build()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "resource.integration.test")
  
  // 创建span并验证资源属性
  let span = Tracer::start_span(tracer, "resource.aware.operation")
  Span::set_attribute(span, "operation.type", "database_query")
  Span::set_attribute(span, "db.system", "postgresql")
  
  // 获取span关联的资源
  let span_resource = Span::get_resource(span)
  assert_eq(Resource::get_attribute(span_resource, "service.name"), Some("azimuth-telemetry-service"))
  assert_eq(Resource::get_attribute(span_resource, "service.version"), Some("2.5.0"))
  assert_eq(Resource::get_attribute(span_resource, "environment"), Some("production"))
  
  // 创建带有资源的MeterProvider
  let meter_provider = MeterProvider::builder()
    .with_resource(telemetry_resource)
    .build()
  
  let meter = MeterProvider::get_meter(meter_provider, "resource.integration.meter")
  let counter = Meter::create_counter(meter, "resource.aware.requests")
  
  // 添加度量数据
  Counter::add_with_attributes(counter, 1.0, [
    ("endpoint", "/api/telemetry/data"),
    ("method", "GET")
  ])
  
  // 验证度量与资源关联
  let metrics = Meter::collect_metrics(meter)
  assert_true(metrics.length() > 0)
  
  // 获取度量关联的资源
  let metric_resource = Meter::get_resource(meter)
  assert_eq(Resource::get_attribute(metric_resource, "service.name"), Some("azimuth-telemetry-service"))
  assert_eq(Resource::get_attribute(metric_resource, "team"), Some("observability"))
  
  Span::end(span)
  
  assert_true(true)
}