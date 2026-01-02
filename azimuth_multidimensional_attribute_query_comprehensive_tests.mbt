// Azimuth Comprehensive Multidimensional Attribute Query Test Suite
// 综合多维度属性查询测试套件

// Test 1: 基础多维度属性查询
test "basic multidimensional attribute query" {
  let query_engine = @azimuth.AttributeQueryEngine::new()
  
  // 创建测试数据集
  let mut telemetry_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  // 生成具有多维属性的遥测数据
  let services = ["api-service", "auth-service", "db-service", "cache-service"]
  let regions = ["us-west-1", "us-east-1", "eu-west-1", "ap-southeast-1"]
  let environments = ["production", "staging", "development"]
  let versions = ["1.0.0", "1.1.0", "1.2.0", "2.0.0"]
  
  for i in 0..=1000 {
    let timestamp = base_timestamp + i * 60000000000 // 1分钟间隔
    
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    let environment = environments[i % environments.length()]
    let version = versions[i % versions.length()]
    
    let attributes = [
      ("service.name", @azimuth.StringValue(service)),
      ("service.version", @azimuth.StringValue(version)),
      ("deployment.region", @azimuth.StringValue(region)),
      ("deployment.environment", @azimuth.StringValue(environment)),
      ("host.name", @azimuth.StringValue("server-" + (i % 20).to_string())),
      ("instance.id", @azimuth.StringValue("instance-" + i.to_string())),
      ("request.id", @azimuth.StringValue("req-" + i.to_string())),
      ("user.id", @azimuth.StringValue("user-" + (i % 100).to_string())),
      ("operation.type", @azimuth.StringValue(["read", "write", "delete"][i % 3])),
      ("status.code", @azimuth.IntValue([200, 201, 400, 404, 500][i % 5]))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(timestamp, "response.time", 50.0 + @azimuth.Random::next_float() * 200.0, attributes)
    telemetry_data = telemetry_data.push(@azimuth.TelemetryData::Metric(metric_point))
  }
  
  // 构建查询索引
  @azimuth.AttributeQueryEngine::build_index(query_engine, telemetry_data)
  
  // 测试单维度查询
  let service_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_filter(service_query, "service.name", @azimuth.QueryOperator::Equals, @azimuth.StringValue("api-service"))
  
  let service_results = @azimuth.AttributeQueryEngine::execute(query_engine, service_query)
  assert_true(service_results.length() > 0)
  
  // 验证所有结果都匹配查询条件
  for result in service_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        match service_attr {
          Some(attr) => assert_eq(@azimuth.Attribute::value(attr), @azimuth.StringValue("api-service"))
          None => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试多维度AND查询
  let multi_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_filter(multi_query, "service.name", @azimuth.QueryOperator::Equals, @azimuth.StringValue("api-service"))
  @azimuth.AttributeQuery::add_filter(multi_query, "deployment.region", @azimuth.QueryOperator::Equals, @azimuth.StringValue("us-west-1"))
  @azimuth.AttributeQuery::add_filter(multi_query, "deployment.environment", @azimuth.QueryOperator::Equals, @azimuth.StringValue("production"))
  
  let multi_results = @azimuth.AttributeQueryEngine::execute(query_engine, multi_query)
  assert_true(multi_results.length() > 0)
  assert_true(multi_results.length() <= service_results.length()) // 多条件查询结果应该更少
  
  // 验证所有结果都匹配所有查询条件
  for result in multi_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        let region_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "deployment.region" })
        let env_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "deployment.environment" })
        
        match (service_attr, region_attr, env_attr) {
          (Some(service), Some(region), Some(env)) => {
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("api-service"))
            assert_eq(@azimuth.Attribute::value(region), @azimuth.StringValue("us-west-1"))
            assert_eq(@azimuth.Attribute::value(env), @azimuth.StringValue("production"))
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试范围查询
  let range_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_filter(range_query, "status.code", @azimuth.QueryOperator::GreaterThanOrEqual, @azimuth.IntValue(400))
  @azimuth.AttributeQuery::add_filter(range_query, "status.code", @azimuth.QueryOperator::LessThanOrEqual, @azimuth.IntValue(500))
  
  let range_results = @azimuth.AttributeQueryEngine::execute(query_engine, range_query)
  assert_true(range_results.length() > 0)
  
  // 验证所有结果都在指定范围内
  for result in range_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        let status_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "status.code" })
        match status_attr {
          Some(attr) => {
            match @azimuth.Attribute::value(attr) {
              @azimuth.IntValue(code) => assert_true(code >= 400 && code <= 500)
              _ => assert_true(false)
            }
          }
          None => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(true)
}

// Test 2: 复杂嵌套属性查询
test "complex nested attribute query" {
  let query_engine = @azimuth.AttributeQueryEngine::new()
  
  // 创建具有嵌套属性的测试数据
  let mut nested_telemetry_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  for i in 0..=500 {
    let timestamp = base_timestamp + i * 30000000000 // 30秒间隔
    
    // 创建嵌套属性结构
    let nested_attributes = [
      ("service.name", @azimuth.StringValue("payment-service")),
      ("service.metadata.version", @azimuth.StringValue("2.1.3")),
      ("service.metadata.commit", @azimuth.StringValue("abc123def456")),
      ("service.config.timeout", @azimuth.IntValue(5000)),
      ("service.config.retries", @azimuth.IntValue(3)),
      ("infrastructure.provider", @azimuth.StringValue("aws")),
      ("infrastructure.region", @azimuth.StringValue("us-west-2")),
      ("infrastructure.instance.type", @azimuth.StringValue("m5.large")),
      ("infrastructure.network.vpc", @azimuth.StringValue("vpc-12345")),
      ("infrastructure.network.subnet", @azimuth.StringValue("subnet-67890")),
      ("business.domain", @azimuth.StringValue("payments")),
      ("business.product", @azimuth.StringValue("transactions")),
      ("business.tier", @azimuth.StringValue(["basic", "premium", "enterprise"][i % 3])),
      ("request.method", @azimuth.StringValue(["GET", "POST", "PUT", "DELETE"][i % 4])),
      ("request.path", @azimuth.StringValue("/api/v1/payments/" + i.to_string())),
      ("response.status", @azimuth.IntValue([200, 201, 400, 401, 404, 500][i % 6])),
      ("response.duration_ms", @azimuth.IntValue(50 + i % 500))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(timestamp, "payment.processing.time", 100.0 + @azimuth.Random::next_float() * 400.0, nested_attributes)
    nested_telemetry_data = nested_telemetry_data.push(@azimuth.TelemetryData::Metric(metric_point))
  }
  
  // 构建嵌套查询索引
  @azimuth.AttributeQueryEngine::build_nested_index(query_engine, nested_telemetry_data)
  
  // 测试嵌套属性查询
  let nested_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_nested_filter(nested_query, "service.metadata", "version", @azimuth.QueryOperator::Equals, @azimuth.StringValue("2.1.3"))
  @azimuth.AttributeQuery::add_nested_filter(nested_query, "infrastructure.network", "vpc", @azimuth.QueryOperator::Equals, @azimuth.StringValue("vpc-12345"))
  
  let nested_results = @azimuth.AttributeQueryEngine::execute(query_engine, nested_query)
  assert_true(nested_results.length() > 0)
  
  // 验证嵌套查询结果
  for result in nested_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        
        let version_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.metadata.version" })
        let vpc_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "infrastructure.network.vpc" })
        
        match (version_attr, vpc_attr) {
          (Some(version), Some(vpc)) => {
            assert_eq(@azimuth.Attribute::value(version), @azimuth.StringValue("2.1.3"))
            assert_eq(@azimuth.Attribute::value(vpc), @azimuth.StringValue("vpc-12345"))
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试混合嵌套和平面属性查询
  let mixed_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_filter(mixed_query, "service.name", @azimuth.QueryOperator::Equals, @azimuth.StringValue("payment-service"))
  @azimuth.AttributeQuery::add_nested_filter(mixed_query, "business", "tier", @azimuth.QueryOperator::Equals, @azimuth.StringValue("premium"))
  @azimuth.AttributeQuery::add_nested_filter(mixed_query, "infrastructure", "provider", @azimuth.QueryOperator::Equals, @azimuth.StringValue("aws"))
  
  let mixed_results = @azimuth.AttributeQueryEngine::execute(query_engine, mixed_query)
  assert_true(mixed_results.length() > 0)
  
  // 验证混合查询结果
  for result in mixed_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        let tier_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "business.tier" })
        let provider_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "infrastructure.provider" })
        
        match (service_attr, tier_attr, provider_attr) {
          (Some(service), Some(tier), Some(provider)) => {
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("payment-service"))
            assert_eq(@azimuth.Attribute::value(tier), @azimuth.StringValue("premium"))
            assert_eq(@azimuth.Attribute::value(provider), @azimuth.StringValue("aws"))
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(true)
}

// Test 3: 高性能聚合查询
test "high-performance aggregation query" {
  let query_engine = @azimuth.AttributeQueryEngine::new()
  
  // 配置查询引擎以支持高性能聚合
  let perf_config = @azimuth.QueryPerformanceConfig::new()
  @azimuth.QueryPerformanceConfig::enable_parallel_execution(perf_config, true)
  @azimuth.QueryPerformanceConfig::set_parallel_workers(perf_config, 4)
  @azimuth.QueryPerformanceConfig::enable_caching(perf_config, true)
  @azimuth.QueryPerformanceConfig::set_cache_size(perf_config, 1000)
  
  @azimuth.AttributeQueryEngine::configure_performance(query_engine, perf_config)
  
  // 创建大量测试数据
  let mut large_dataset = []
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  let services = ["service-a", "service-b", "service-c", "service-d", "service-e"]
  let regions = ["region-1", "region-2", "region-3", "region-4", "region-5"]
  let operations = ["op-read", "op-write", "op-update", "op-delete", "op-create"]
  
  for i in 0..=10000 {
    let timestamp = base_timestamp + i * 1000000000 // 1秒间隔
    
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    let operation = operations[i % operations.length()]
    
    let attributes = [
      ("service.name", @azimuth.StringValue(service)),
      ("service.instance", @azimuth.StringValue("instance-" + (i % 50).to_string())),
      ("deployment.region", @azimuth.StringValue(region)),
      ("operation.name", @azimuth.StringValue(operation)),
      ("operation.duration_ms", @azimuth.IntValue(10 + i % 1000)),
      ("operation.success", @azimuth.BoolValue(i % 10 != 0)), // 90%成功率
      ("user.id", @azimuth.StringValue("user-" + (i % 1000).to_string())),
      ("request.size", @azimuth.IntValue(100 + i % 10000)),
      ("response.size", @azimuth.IntValue(200 + i % 20000))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(timestamp, "latency", 10.0 + (i % 1000).to_float(), attributes)
    large_dataset = large_dataset.push(@azimuth.TelemetryData::Metric(metric_point))
  }
  
  // 构建高性能查询索引
  let index_start = @azimuth.Time::now_unix_nanos()
  @azimuth.AttributeQueryEngine::build_optimized_index(query_engine, large_dataset)
  let index_end = @azimuth.Time::now_unix_nanos()
  
  let index_time = index_end - index_start
  let index_throughput = large_dataset.length() / (index_time / 1000000000.0)
  
  // 验证索引构建性能
  assert_true(index_throughput > 5000) // 至少5000条/秒索引构建速度
  
  // 测试聚合查询性能
  let aggregation_query = @azimuth.AggregationQuery::new()
  @azimuth.AggregationQuery::add_group_by(aggregation_query, "service.name")
  @azimuth.AggregationQuery::add_group_by(aggregation_query, "deployment.region")
  @azimuth.AggregationQuery::add_aggregation(aggregation_query, "operation.duration_ms", @azimuth.AggregationFunction::Avg)
  @azimuth.AggregationQuery::add_aggregation(aggregation_query, "operation.duration_ms", @azimuth.AggregationFunction::Max)
  @azimuth.AggregationQuery::add_aggregation(aggregation_query, "operation.duration_ms", @azimuth.AggregationFunction::Min)
  @azimuth.AggregationQuery::add_aggregation(aggregation_query, "operation.success", @azimuth.AggregationFunction::Count)
  
  // 执行聚合查询
  let agg_start = @azimuth.Time::now_unix_nanos()
  let aggregation_results = @azimuth.AttributeQueryEngine::execute_aggregation(query_engine, aggregation_query)
  let agg_end = @azimuth.Time::now_unix_nanos()
  
  let agg_time = agg_end - agg_start
  let agg_throughput = large_dataset.length() / (agg_time / 1000000000.0)
  
  // 验证聚合查询性能
  assert_true(agg_throughput > 2000) // 至少2000条/秒聚合查询速度
  assert_true(aggregation_results.length() > 0)
  
  // 验证聚合结果结构
  for result in aggregation_results {
    let group_keys = @azimuth.AggregationResult::group_keys(result)
    let aggregations = @azimuth.AggregationResult::aggregations(result)
    
    // 验证分组键
    assert_eq(group_keys.length(), 2) // service.name 和 deployment.region
    
    // 验证聚合值
    assert_eq(aggregations.length(), 4) // avg, max, min, count
    
    // 验证聚合值类型
    for agg in aggregations {
      match @azimuth.AggregationValue::function(agg) {
        @azimuth.AggregationFunction::Avg => assert_type(@azimuth.AggregationValue::value(agg), @azimuth.FloatValue)
        @azimuth.AggregationFunction::Max => assert_type(@azimuth.AggregationValue::value(agg), @azimuth.IntValue)
        @azimuth.AggregationFunction::Min => assert_type(@azimuth.AggregationValue::value(agg), @azimuth.IntValue)
        @azimuth.AggregationFunction::Count => assert_type(@azimuth.AggregationValue::value(agg), @azimuth.IntValue)
        _ => assert_true(false)
      }
    }
  }
  
  // 测试缓存效果
  let cached_agg_start = @azimuth.Time::now_unix_nanos()
  let cached_aggregation_results = @azimuth.AttributeQueryEngine::execute_aggregation(query_engine, aggregation_query)
  let cached_agg_end = @azimuth.Time::now_unix_nanos()
  
  let cached_agg_time = cached_agg_end - cached_agg_start
  let cached_agg_throughput = large_dataset.length() / (cached_agg_time / 1000000000.0)
  
  // 验证缓存提高了性能
  assert_true(cached_agg_throughput > agg_throughput)
  assert_eq(aggregation_results.length(), cached_aggregation_results.length())
  
  assert_true(true)
}

// Test 4: 动态属性查询和实时索引更新
test "dynamic attribute query and real-time index updates" {
  let query_engine = @azimuth.AttributeQueryEngine::new()
  
  // 配置动态查询引擎
  let dynamic_config = @azimuth.DynamicQueryConfig::new()
  @azimuth.DynamicQueryConfig::enable_real_time_updates(dynamic_config, true)
  @azimuth.DynamicQueryConfig::set_update_batch_size(dynamic_config, 100)
  @azimuth.DynamicQueryConfig::set_update_interval(dynamic_config, 1000000000) // 1秒
  @azimuth.DynamicQueryConfig::enable_incremental_indexing(dynamic_config, true)
  
  @azimuth.AttributeQueryEngine::configure_dynamic(query_engine, dynamic_config)
  
  // 初始化索引
  let initial_data = []
  @azimuth.AttributeQueryEngine::build_index(query_engine, initial_data)
  
  // 创建动态查询
  let dynamic_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_filter(dynamic_query, "service.name", @azimuth.QueryOperator::Equals, @azimuth.StringValue("dynamic-service"))
  @azimuth.AttributeQuery::add_filter(dynamic_query, "status", @azimuth.QueryOperator::Equals, @azimuth.StringValue("active"))
  
  // 初始查询应该返回空结果
  let initial_results = @azimuth.AttributeQueryEngine::execute(query_engine, dynamic_query)
  assert_eq(initial_results.length(), 0)
  
  // 动态添加数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  for i in 0..=500 {
    let timestamp = base_timestamp + i * 2000000000 // 2秒间隔
    
    let attributes = [
      ("service.name", @azimuth.StringValue("dynamic-service")),
      ("service.version", @azimuth.StringValue("1.0." + (i % 10).to_string())),
      ("status", @azimuth.StringValue(["active", "inactive", "maintenance"][i % 3])),
      ("instance.id", @azimuth.StringValue("instance-" + i.to_string())),
      ("region", @azimuth.StringValue(["us-east-1", "us-west-1", "eu-west-1"][i % 3])),
      ("load", @azimuth.FloatValue(0.1 + (i % 100).to_float() / 100.0))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(timestamp, "dynamic.metric", i.to_float(), attributes)
    let telemetry_data = @azimuth.TelemetryData::Metric(metric_point)
    
    // 动态添加到索引
    @azimuth.AttributeQueryEngine::add_data(query_engine, telemetry_data)
  }
  
  // 触发索引更新
  @azimuth.AttributeQueryEngine::update_index(query_engine)
  
  // 再次查询应该返回结果
  let updated_results = @azimuth.AttributeQueryEngine::execute(query_engine, dynamic_query)
  assert_true(updated_results.length() > 0)
  
  // 验证查询结果
  for result in updated_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        let status_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "status" })
        
        match (service_attr, status_attr) {
          (Some(service), Some(status)) => {
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("dynamic-service"))
            assert_eq(@azimuth.Attribute::value(status), @azimuth.StringValue("active"))
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试动态查询条件变更
  let updated_dynamic_query = @azimuth.AttributeQuery::new()
  @azimuth.AttributeQuery::add_filter(updated_dynamic_query, "service.name", @azimuth.QueryOperator::Equals, @azimuth.StringValue("dynamic-service"))
  @azimuth.AttributeQuery::add_filter(updated_dynamic_query, "load", @azimuth.QueryOperator::GreaterThan, @azimuth.FloatValue(0.5))
  
  let load_results = @azimuth.AttributeQueryEngine::execute(query_engine, updated_dynamic_query)
  assert_true(load_results.length() > 0)
  
  // 验证负载查询结果
  for result in load_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        let load_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "load" })
        
        match (service_attr, load_attr) {
          (Some(service), Some(load)) => {
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("dynamic-service"))
            match @azimuth.Attribute::value(load) {
              @azimuth.FloatValue(load_val) => assert_true(load_val > 0.5)
              _ => assert_true(false)
            }
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试数据删除和索引更新
  for i in 0..=100 {
    if i % 2 == 0 { // 删除偶数索引的数据
      let timestamp = base_timestamp + i * 2000000000
      let attributes = [
        ("service.name", @azimuth.StringValue("dynamic-service")),
        ("instance.id", @azimuth.StringValue("instance-" + i.to_string()))
      ]
      
      let metric_point = @azimuth.MetricPoint::new(timestamp, "dynamic.metric", i.to_float(), attributes)
      let telemetry_data = @azimuth.TelemetryData::Metric(metric_point)
      
      @azimuth.AttributeQueryEngine::remove_data(query_engine, telemetry_data)
    }
  }
  
  // 更新索引
  @azimuth.AttributeQueryEngine::update_index(query_engine)
  
  // 验证数据删除效果
  let after_removal_results = @azimuth.AttributeQueryEngine::execute(query_engine, dynamic_query)
  assert_true(after_removal_results.length() < updated_results.length()) // 结果应该减少
  
  assert_true(true)
}

// Test 5: 跨数据源联合查询
test "cross-datasource federated query" {
  let federated_engine = @azimuth.FederatedQueryEngine::new()
  
  // 创建多个数据源
  let datasource1 = @azimuth.TelemetryDataSource::new("metrics-db")
  let datasource2 = @azimuth.TelemetryDataSource::new("logs-store")
  let datasource3 = @azimuth.TelemetryDataSource::new("traces-archive")
  
  // 为每个数据源创建测试数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  // 数据源1：度量数据
  let mut metrics_data = []
  for i in 0..=300 {
    let timestamp = base_timestamp + i * 60000000000 // 1分钟间隔
    let attributes = [
      ("datasource", @azimuth.StringValue("metrics-db")),
      ("service.name", @azimuth.StringValue(["user-service", "order-service", "payment-service"][i % 3])),
      ("metric.type", @azimuth.StringValue(["counter", "gauge", "histogram"][i % 3])),
      ("metric.name", @azimuth.StringValue("request.count")),
      ("metric.value", @azimuth.FloatValue(10.0 + i.to_float() * 1.5))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(timestamp, "request.count", 10.0 + i.to_float() * 1.5, attributes)
    metrics_data = metrics_data.push(@azimuth.TelemetryData::Metric(metric_point))
  }
  
  // 数据源2：日志数据
  let mut logs_data = []
  for i in 0..=300 {
    let timestamp = base_timestamp + i * 60000000000 + 30000000000 // 30秒偏移
    let attributes = [
      ("datasource", @azimuth.StringValue("logs-store")),
      ("service.name", @azimuth.StringValue(["user-service", "order-service", "payment-service"][i % 3])),
      ("log.level", @azimuth.StringValue(["INFO", "WARN", "ERROR"][i % 3])),
      ("log.message", @azimuth.StringValue("Processing request " + i.to_string())),
      ("request.id", @azimuth.StringValue("req-" + i.to_string()))
    ]
    
    let log_record = @azimuth.LogRecord::new(timestamp, @azimuth.Severity::Info, "Processing request " + i.to_string(), attributes)
    logs_data = logs_data.push(@azimuth.TelemetryData::Log(log_record))
  }
  
  // 数据源3：追踪数据
  let mut traces_data = []
  for i in 0..=300 {
    let timestamp = base_timestamp + i * 60000000000 + 15000000000 // 15秒偏移
    let attributes = [
      ("datasource", @azimuth.StringValue("traces-archive")),
      ("service.name", @azimuth.StringValue(["user-service", "order-service", "payment-service"][i % 3])),
      ("span.name", @azimuth.StringValue("http.request")),
      ("span.duration_ms", @azimuth.IntValue(50 + i % 200)),
      ("trace.id", @azimuth.StringValue("trace-" + (i / 10).to_string())),
      ("span.id", @azimuth.StringValue("span-" + i.to_string()))
    ]
    
    let span_data = @azimuth.SpanData::new(
      @azimuth.SpanContext::new(@azimuth.TraceId::new(), @azimuth.SpanId::new(), []),
      "http.request",
      timestamp,
      timestamp + (50 + i % 200) * 1000000,
      @azimuth.SpanKind::Server,
      attributes
    )
    traces_data = traces_data.push(@azimuth.TelemetryData::Span(span_data))
  }
  
  // 将数据添加到数据源
  @azimuth.TelemetryDataSource::add_data(datasource1, metrics_data)
  @azimuth.TelemetryDataSource::add_data(datasource2, logs_data)
  @azimuth.TelemetryDataSource::add_data(datasource3, traces_data)
  
  // 注册数据源到联邦查询引擎
  @azimuth.FederatedQueryEngine::register_datasource(federated_engine, datasource1)
  @azimuth.FederatedQueryEngine::register_datasource(federated_engine, datasource2)
  @azimuth.FederatedQueryEngine::register_datasource(federated_engine, datasource3)
  
  // 创建跨数据源查询
  let federated_query = @azimuth.FederatedQuery::new()
  @azimuth.FederatedQuery::add_datasource_filter(federated_query, ["metrics-db", "logs-store", "traces-archive"])
  @azimuth.FederatedQuery::add_common_filter(federated_query, "service.name", @azimuth.QueryOperator::Equals, @azimuth.StringValue("user-service"))
  
  // 执行联邦查询
  let federated_results = @azimuth.FederatedQueryEngine::execute(federated_engine, federated_query)
  assert_true(federated_results.length() > 0)
  
  // 验证联邦查询结果包含来自所有数据源的数据
  let mut metrics_count = 0
  let mut logs_count = 0
  let mut traces_count = 0
  
  for result in federated_results {
    match result {
      @azimuth.TelemetryData::Metric(metric) => {
        metrics_count = metrics_count + 1
        let attributes = @azimuth.MetricPoint::attributes(metric)
        let datasource_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "datasource" })
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        
        match (datasource_attr, service_attr) {
          (Some(datasource), Some(service)) => {
            assert_eq(@azimuth.Attribute::value(datasource), @azimuth.StringValue("metrics-db"))
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("user-service"))
          }
          _ => assert_true(false)
        }
      }
      @azimuth.TelemetryData::Log(log) => {
        logs_count = logs_count + 1
        let attributes = @azimuth.LogRecord::attributes(log)
        let datasource_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "datasource" })
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        
        match (datasource_attr, service_attr) {
          (Some(datasource), Some(service)) => {
            assert_eq(@azimuth.Attribute::value(datasource), @azimuth.StringValue("logs-store"))
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("user-service"))
          }
          _ => assert_true(false)
        }
      }
      @azimuth.TelemetryData::Span(span) => {
        traces_count = traces_count + 1
        let attributes = @azimuth.SpanData::attributes(span)
        let datasource_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "datasource" })
        let service_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "service.name" })
        
        match (datasource_attr, service_attr) {
          (Some(datasource), Some(service)) => {
            assert_eq(@azimuth.Attribute::value(datasource), @azimuth.StringValue("traces-archive"))
            assert_eq(@azimuth.Attribute::value(service), @azimuth.StringValue("user-service"))
          }
          _ => assert_true(false)
        }
      }
    }
  }
  
  // 验证每种类型的数据都有结果
  assert_true(metrics_count > 0)
  assert_true(logs_count > 0)
  assert_true(traces_count > 0)
  
  // 测试跨数据源聚合查询
  let federated_aggregation_query = @azimuth.FederatedAggregationQuery::new()
  @azimuth.FederatedAggregationQuery::add_datasource_filter(federated_aggregation_query, ["metrics-db", "traces-archive"])
  @azimuth.FederatedAggregationQuery::add_group_by(federated_aggregation_query, "service.name")
  @azimuth.FederatedAggregationQuery::add_cross_datasource_aggregation(
    federated_aggregation_query, 
    "metric.value", 
    @azimuth.AggregationFunction::Avg,
    "span.duration_ms",
    @azimuth.AggregationFunction::Avg
  )
  
  // 执行联邦聚合查询
  let federated_agg_results = @azimuth.FederatedQueryEngine::execute_aggregation(federated_engine, federated_aggregation_query)
  assert_true(federated_agg_results.length() > 0)
  
  // 验证联邦聚合结果
  for result in federated_agg_results {
    let group_keys = @azimuth.FederatedAggregationResult::group_keys(result)
    let cross_datasource_aggs = @azimuth.FederatedAggregationResult::cross_datasource_aggregations(result)
    
    // 验证分组键
    assert_true(group_keys.length() > 0)
    
    // 验证跨数据源聚合
    assert_true(cross_datasource_aggs.length() > 0)
    
    for agg in cross_datasource_aggs {
      let datasource1_field = @azimuth.CrossDatasourceAggregation::datasource1_field(agg)
      let datasource2_field = @azimuth.CrossDatasourceAggregation::datasource2_field(agg)
      
      assert_true(datasource1_field == "metric.value")
      assert_true(datasource2_field == "span.duration_ms")
    }
  }
  
  assert_true(true)
}