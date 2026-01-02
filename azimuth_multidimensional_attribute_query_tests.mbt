// Azimuth Multidimensional Attribute Query Tests
// 多维属性查询测试用例 - 专注于复杂的多维数据查询和分析功能

// Test 1: 多维属性索引和查询优化
test "multidimensional attribute indexing and query optimization" {
  // 创建多维属性查询管理器
  let query_manager = MultidimensionalQueryManager::new()
  
  // 配置查询参数
  QueryManager::configure(query_manager, [
    ("indexing.strategy", StringValue("composite")),
    ("query.cache.size", IntValue(1000)),
    ("parallel.query.enabled", BoolValue(true)),
    ("result.limit.default", IntValue(1000))
  ])
  
  // 创建多维属性数据集
  let telemetry_dataset = MultidimensionalDataset::new("system_telemetry")
  
  // 生成多维测试数据
  let base_time = Time::now() - (7 * 24 * 60 * 60 * 1000) // 7天前
  
  for i in 0..<10000 {
    let timestamp = base_time + (i * 60 * 1000) // 每分钟一个数据点
    
    // 多维属性
    let attributes = [
      // 时间维度
      ("hour", IntValue((timestamp / (60 * 60 * 1000)) % 24)),
      ("day_of_week", IntValue(((timestamp / (24 * 60 * 60 * 1000)) + 4) % 7)), // 调整为周一开始
      ("month", IntValue(((timestamp / (30 * 24 * 60 * 60 * 1000)) % 12) + 1)),
      
      // 系统维度
      ("service", StringValue(["api", "database", "cache", "queue"][i % 4])),
      ("environment", StringValue(["prod", "staging", "dev"][i % 3])),
      ("region", StringValue(["us-east", "us-west", "eu-west", "ap-southeast"][i % 4])),
      ("instance", StringValue("instance_" + (i % 20).to_string())),
      
      // 性能维度
      ("cpu_usage", FloatValue(20.0 + (Math::random() * 60.0))),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0))),
      ("disk_io", IntValue((Math::random() * 1000).to_int())),
      ("network_io", IntValue((Math::random() * 2000).to_int())),
      
      // 业务维度
      ("user_id", StringValue("user_" + (i % 500).to_string())),
      ("session_id", StringValue("session_" + (i % 200).to_string())),
      ("request_type", StringValue(["read", "write", "update", "delete"][i % 4])),
      ("response_code", IntValue([200, 201, 400, 404, 500][i % 5]))
    ]
    
    let data_point = MultidimensionalDataPoint::new_with_timestamp(
      "telemetry_" + i.to_string(),
      attributes,
      timestamp
    )
    
    telemetry_dataset.add(data_point)
  }
  
  // 验证数据集大小
  assert_eq(telemetry_dataset.size(), 10000)
  
  // 创建多维索引
  let indexing_start = Time::now()
  
  // 单维度索引
  let service_index = QueryManager::create_index(query_manager, telemetry_dataset, ["service"])
  let environment_index = QueryManager::create_index(query_manager, telemetry_dataset, ["environment"])
  let region_index = QueryManager::create_index(query_manager, telemetry_dataset, ["region"])
  
  // 复合索引
  let service_env_index = QueryManager::create_index(query_manager, telemetry_dataset, ["service", "environment"])
  let region_service_index = QueryManager::create_index(query_manager, telemetry_dataset, ["region", "service"])
  let time_service_index = QueryManager::create_index(query_manager, telemetry_dataset, ["hour", "service"])
  
  // 多维复合索引
  let full_composite_index = QueryManager::create_index(
    query_manager,
    telemetry_dataset,
    ["service", "environment", "region", "hour"]
  )
  
  let indexing_end = Time::now()
  let indexing_duration = indexing_end - indexing_start
  
  assert_true(indexing_duration < 30000) // 索引创建应在30秒内完成
  
  // 验证索引创建
  assert_true(QueryManager::index_exists(query_manager, service_index))
  assert_true(QueryManager::index_exists(query_manager, environment_index))
  assert_true(QueryManager::index_exists(query_manager, service_env_index))
  assert_true(QueryManager::index_exists(query_manager, full_composite_index))
  
  // 测试单维度查询
  let single_dim_queries = [
    ("service", "api"),
    ("environment", "prod"),
    ("region", "us-east"),
    ("hour", 14),
    ("response_code", 200)
  ]
  
  for (dimension, value) in single_dim_queries {
    let query_start = Time::now()
    
    let query = MultidimensionalQuery::equals(dimension, value)
    let result = QueryManager::execute(query_manager, telemetry_dataset, query)
    
    let query_end = Time::now()
    let query_duration = query_end - query_start
    
    assert_true(query_duration < 1000) // 单维度查询应在1秒内完成
    assert_true(result.size() > 0)
    
    // 验证查询结果
    for data_point in result {
      match QueryManager::get_attribute(data_point, dimension) {
        Some(attr_value) => assert_eq(attr_value, value),
        None => assert_true(false)
      }
    }
  }
  
  // 测试多维度AND查询
  let multi_dim_and_query = MultidimensionalQuery::and([
    MultidimensionalQuery::equals("service", "database"),
    MultidimensionalQuery::equals("environment", "prod"),
    MultidimensionalQuery::greater_than("cpu_usage", 50.0),
    MultidimensionalQuery::less_than("memory_usage", 80.0)
  ])
  
  let multi_dim_start = Time::now()
  let and_result = QueryManager::execute(query_manager, telemetry_dataset, multi_dim_and_query)
  let multi_dim_end = Time::now()
  let multi_dim_duration = multi_dim_end - multi_dim_start
  
  assert_true(multi_dim_duration < 2000) // 多维度查询应在2秒内完成
  assert_true(and_result.size() > 0)
  
  // 验证AND查询结果
  for data_point in and_result {
    match QueryManager::get_attribute(data_point, "service") {
      Some(StringValue(service)) => assert_eq(service, "database"),
      _ => assert_true(false)
    }
    
    match QueryManager::get_attribute(data_point, "environment") {
      Some(StringValue(env)) => assert_eq(env, "prod"),
      _ => assert_true(false)
    }
    
    match QueryManager::get_attribute(data_point, "cpu_usage") {
      Some(FloatValue(cpu)) => assert_true(cpu > 50.0),
      _ => assert_true(false)
    }
    
    match QueryManager::get_attribute(data_point, "memory_usage") {
      Some(FloatValue(memory)) => assert_true(memory < 80.0),
      _ => assert_true(false)
    }
  }
  
  // 测试多维度OR查询
  let multi_dim_or_query = MultidimensionalQuery::or([
    MultidimensionalQuery::equals("service", "cache"),
    MultidimensionalQuery::greater_than("response_code", 400),
    MultidimensionalQuery::less_than("cpu_usage", 30.0)
  ])
  
  let or_result = QueryManager::execute(query_manager, telemetry_dataset, multi_dim_or_query)
  assert_true(or_result.size() > and_result.size()) // OR查询结果应大于AND查询
  
  // 测试范围查询
  let range_query = MultidimensionalQuery::and([
    MultidimensionalQuery::between("hour", 9, 17), // 工作时间
    MultidimensionalQuery::between("cpu_usage", 40.0, 70.0), // CPU使用率范围
    MultidimensionalQuery::between("disk_io", 200, 800) // 磁盘IO范围
  ])
  
  let range_result = QueryManager::execute(query_manager, telemetry_dataset, range_query)
  assert_true(range_result.size() > 0)
  
  // 验证范围查询结果
  for data_point in range_result {
    match QueryManager::get_attribute(data_point, "hour") {
      Some(IntValue(hour)) => assert_true(hour >= 9 && hour <= 17),
      _ => assert_true(false)
    }
    
    match QueryManager::get_attribute(data_point, "cpu_usage") {
      Some(FloatValue(cpu)) => assert_true(cpu >= 40.0 && cpu <= 70.0),
      _ => assert_true(false)
    }
    
    match QueryManager::get_attribute(data_point, "disk_io") {
      Some(IntValue(disk_io)) => assert_true(disk_io >= 200 && disk_io <= 800),
      _ => assert_true(false)
    }
  }
  
  // 测试聚合查询
  let aggregation_query = MultidimensionalQuery::aggregate(
    MultidimensionalQuery::equals("environment", "prod"),
    ["service", "region"],
    [
      Aggregation::avg("cpu_usage"),
      Aggregation::avg("memory_usage"),
      Aggregation::sum("disk_io"),
      Aggregation::count()
    ]
  )
  
  let aggregation_result = QueryManager::execute(query_manager, telemetry_dataset, aggregation_query)
  assert_true(aggregation_result.size() > 0)
  
  // 验证聚合结果
  for group in aggregation_result {
    assert_true(AggregationGroup::has_grouping_keys(group))
    assert_true(AggregationGroup::has_aggregations(group))
    
    let group_keys = AggregationGroup::grouping_keys(group)
    assert_true(group_keys.contains("service"))
    assert_true(group_keys.contains("region"))
    
    let aggregations = AggregationGroup::aggregations(group)
    assert_true(aggregations.contains("avg_cpu_usage"))
    assert_true(aggregations.contains("avg_memory_usage"))
    assert_true(aggregations.contains("sum_disk_io"))
    assert_true(aggregations.contains("count"))
  }
}

// Test 2: 复杂多维分析查询
test "complex multidimensional analysis queries" {
  // 创建复杂查询分析器
  let complex_query_analyzer = ComplexQueryAnalyzer::new()
  
  // 配置复杂查询参数
  ComplexQueryAnalyzer::configure(complex_query_analyzer, [
    ("query.complexity.limit", IntValue(10)),
    ("subquery.cache.enabled", BoolValue(true)),
    ("join.optimization", BoolValue(true)),
    ("analytics.functions", StringValue("percentile,correlation,regression"))
  ])
  
  // 创建多维数据集
  let analytics_dataset = MultidimensionalDataset::new("analytics_telemetry")
  
  // 生成更复杂的多维数据
  let base_time = Time::now() - (30 * 24 * 60 * 60 * 1000) // 30天前
  
  for i in 0..<5000 {
    let timestamp = base_time + (i * 10 * 60 * 1000) // 每10分钟一个数据点
    
    // 复杂的多维属性
    let attributes = [
      // 时间层次维度
      ("year", IntValue(2023)),
      ("month", IntValue(((timestamp / (30 * 24 * 60 * 60 * 1000)) % 12) + 1)),
      ("day", IntValue(((timestamp / (24 * 60 * 60 * 1000)) % 30) + 1)),
      ("hour", IntValue((timestamp / (60 * 60 * 1000)) % 24)),
      ("minute", IntValue((timestamp / (60 * 1000)) % 60)),
      
      // 地理层次维度
      ("continent", StringValue(["North America", "Europe", "Asia", "South America"][i % 4])),
      ("country", StringValue(["USA", "Canada", "UK", "Germany", "France", "China", "Japan", "Brazil"][i % 8])),
      ("city", StringValue(["New York", "Los Angeles", "London", "Berlin", "Paris", "Tokyo", "São Paulo"][i % 7])),
      
      // 服务层次维度
      ("service_category", StringValue(["compute", "storage", "network", "database"][i % 4])),
      ("service_name", StringValue(["web_api", "auth_service", "data_service", "cache_service"][i % 4])),
      ("service_version", StringValue("1." + ((i % 9) + 1).to_string() + ".0")),
      
      // 性能指标维度
      ("response_time", FloatValue(10.0 + (Math::random() * 500.0))),
      ("throughput", FloatValue(100.0 + (Math::random() * 1000.0))),
      ("error_rate", FloatValue(Math::random() * 10.0)),
      ("availability", FloatValue(90.0 + (Math::random() * 10.0))),
      
      // 业务指标维度
      ("user_satisfaction", FloatValue(1.0 + (Math::random() * 4.0))), // 1-5分
      ("revenue_impact", FloatValue(Math::random() * 10000.0)),
      ("transaction_count", IntValue((Math::random() * 1000).to_int())),
      
      // 系统状态维度
      ("cpu_cores", IntValue([2, 4, 8, 16][i % 4])),
      ("memory_gb", IntValue([4, 8, 16, 32][i % 4])),
      ("disk_type", StringValue(["SSD", "HDD", "NVMe"][i % 3])),
      ("network_speed", IntValue([100, 1000, 10000][i % 3])) // Mbps
    ]
    
    let data_point = MultidimensionalDataPoint::new_with_timestamp(
      "analytics_" + i.to_string(),
      attributes,
      timestamp
    )
    
    analytics_dataset.add(data_point)
  }
  
  // 测试层次化查询
  let hierarchical_query = ComplexQueryAnalyzer::create_hierarchical_query(
    complex_query_analyzer,
    [
      ("continent", "North America"),
      ("country", "USA"),
      ("city", "New York")
    ],
    "service_category",
    "compute"
  )
  
  let hierarchical_result = ComplexQueryAnalyzer::execute(
    complex_query_analyzer,
    analytics_dataset,
    hierarchical_query
  )
  
  assert_true(hierarchical_result.size() > 0)
  
  // 验证层次化查询结果
  for data_point in hierarchical_result {
    match ComplexQueryAnalyzer::get_attribute(data_point, "continent") {
      Some(StringValue(continent)) => assert_eq(continent, "North America"),
      _ => assert_true(false)
    }
    
    match ComplexQueryAnalyzer::get_attribute(data_point, "country") {
      Some(StringValue(country)) => assert_eq(country, "USA"),
      _ => assert_true(false)
    }
    
    match ComplexQueryAnalyzer::get_attribute(data_point, "city") {
      Some(StringValue(city)) => assert_eq(city, "New York"),
      _ => assert_true(false)
    }
    
    match ComplexQueryAnalyzer::get_attribute(data_point, "service_category") {
      Some(StringValue(category)) => assert_eq(category, "compute"),
      _ => assert_true(false)
    }
  }
  
  // 测试时间序列多维查询
  let time_series_query = ComplexQueryAnalyzer::create_time_series_query(
    complex_query_analyzer,
    base_time,
    base_time + (7 * 24 * 60 * 60 * 1000), // 7天窗口
    ["hour", "day"],
    ["service_name", "service_category"],
    [
      Aggregation::avg("response_time"),
      Aggregation::percentile("response_time", 95),
      Aggregation::sum("transaction_count"),
      Aggregation::avg("user_satisfaction")
    ]
  )
  
  let time_series_result = ComplexQueryAnalyzer::execute(
    complex_query_analyzer,
    analytics_dataset,
    time_series_query
  )
  
  assert_true(time_series_result.size() > 0)
  
  // 验证时间序列查询结果
  for group in time_series_result {
    assert_true(TimeSeriesGroup::has_time_dimension(group))
    assert_true(TimeSeriesGroup::has_grouping_keys(group))
    assert_true(TimeSeriesGroup::has_time_series_data(group))
    
    let time_dimension = TimeSeriesGroup::time_dimension(group)
    assert_true(time_dimension == "hour" || time_dimension == "day")
    
    let time_series_data = TimeSeriesGroup::time_series_data(group)
    assert_true(time_series_data.length() > 0)
    
    // 验证时间序列数据点
    for ts_point in time_series_data {
      assert_true(TimeSeriesPoint::has_timestamp(ts_point))
      assert_true(TimeSeriesPoint::has_values(ts_point))
    }
  }
  
  // 测试钻取查询
  let drill_down_query = ComplexQueryAnalyzer::create_drill_down_query(
    complex_query_analyzer,
    MultidimensionalQuery::equals("service_category", "database"),
    ["service_name", "service_version"],
    [
      DrillDownLevel::new("continent", ["country", "city"]),
      DrillDownLevel::new("country", ["city"]),
      DrillDownLevel::new("service_name", ["service_version"])
    ]
  )
  
  let drill_down_result = ComplexQueryAnalyzer::execute(
    complex_query_analyzer,
    analytics_dataset,
    drill_down_query
  )
  
  assert_true(drill_down_result.size() > 0)
  
  // 验证钻取查询结果
  for drill_down_group in drill_down_result {
    assert_true(DrillDownGroup::has_drill_down_path(drill_down_group))
    assert_true(DrillDownGroup::has_level_data(drill_down_group))
    
    let drill_down_path = DrillDownGroup::drill_down_path(drill_down_group)
    assert_true(drill_down_path.length() > 0)
    
    let level_data = DrillDownGroup::level_data(drill_down_group)
    assert_true(level_data.length() > 0)
  }
  
  // 测试切片和切块查询
  let slice_query = ComplexQueryAnalyzer::create_slice_query(
    complex_query_analyzer,
    ["service_category", "continent"],
    [("service_category", "storage"), ("continent", "Europe")]
  )
  
  let slice_result = ComplexQueryAnalyzer::execute(
    complex_query_analyzer,
    analytics_dataset,
    slice_query
  )
  
  assert_true(slice_result.size() > 0)
  
  // 验证切片查询结果
  for data_point in slice_result {
    match ComplexQueryAnalyzer::get_attribute(data_point, "service_category") {
      Some(StringValue(category)) => assert_eq(category, "storage"),
      _ => assert_true(false)
    }
    
    match ComplexQueryAnalyzer::get_attribute(data_point, "continent") {
      Some(StringValue(continent)) => assert_eq(continent, "Europe"),
      _ => assert_true(false)
    }
  }
  
  // 测试旋转查询
  let pivot_query = ComplexQueryAnalyzer::create_pivot_query(
    complex_query_analyzer,
    "service_category",
    "continent",
    "response_time",
    Aggregation::avg("response_time")
  )
  
  let pivot_result = ComplexQueryAnalyzer::execute(
    complex_query_analyzer,
    analytics_dataset,
    pivot_query
  )
  
  assert_true(pivot_result.size() > 0)
  
  // 验证旋转查询结果
  let pivot_table = PivotTable::from_result(pivot_result)
  assert_true(PivotTable::has_rows(pivot_table))
  assert_true(PivotTable::has_columns(pivot_table))
  assert_true(PivotTable::has_values(pivot_table))
  
  let rows = PivotTable::rows(pivot_table)
  let columns = PivotTable::columns(pivot_table)
  let values = PivotTable::values(pivot_table)
  
  assert_true(rows.length() > 0)
  assert_true(columns.length() > 0)
  assert_true(values.length() == rows.length() * columns.length())
}

// Test 3: 多维查询性能优化
test "multidimensional query performance optimization" {
  // 创建查询性能优化器
  let query_optimizer = MultidimensionalQueryOptimizer::new()
  
  // 配置优化参数
  QueryOptimizer::configure(query_optimizer, [
    ("index.hinting.enabled", BoolValue(true)),
    ("query.rewriting.enabled", BoolValue(true)),
    ("parallel.execution", BoolValue(true)),
    ("result.caching", BoolValue(true)),
    ("cost.based.optimization", BoolValue(true))
  ])
  
  // 创建大型多维数据集
  let large_dataset = MultidimensionalDataset::new("large_telemetry")
  
  // 生成大量多维数据
  let base_time = Time::now() - (60 * 24 * 60 * 60 * 1000) // 60天前
  
  for i in 0..<50000 {
    let timestamp = base_time + (i * 2 * 60 * 1000) // 每2分钟一个数据点
    
    let attributes = [
      ("service", StringValue(["api", "database", "cache", "queue", "search"][i % 5])),
      ("environment", StringValue(["prod", "staging", "dev", "test"][i % 4])),
      ("region", StringValue(["us-east", "us-west", "eu-west", "eu-central", "ap-southeast", "ap-northeast"][i % 6])),
      ("version", StringValue("1." + ((i % 20) + 1).to_string() + "." + ((i % 10) + 1).to_string())),
      ("instance", StringValue("instance_" + (i % 100).to_string())),
      ("cpu_usage", FloatValue(10.0 + (Math::random() * 80.0))),
      ("memory_usage", FloatValue(20.0 + (Math::random() * 70.0))),
      ("disk_io", IntValue((Math::random() * 2000).to_int())),
      ("network_io", IntValue((Math::random() * 5000).to_int())),
      ("response_time", FloatValue(5.0 + (Math::random() * 1000.0))),
      ("error_count", IntValue((Math::random() * 10).to_int())),
      ("request_count", IntValue((Math::random() * 5000).to_int()))
    ]
    
    let data_point = MultidimensionalDataPoint::new_with_timestamp(
      "large_" + i.to_string(),
      attributes,
      timestamp
    )
    
    large_dataset.add(data_point)
  }
  
  // 验证数据集大小
  assert_eq(large_dataset.size(), 50000)
  
  // 创建多个索引以测试优化效果
  let service_index = QueryOptimizer::create_index(query_optimizer, large_dataset, ["service"])
  let region_index = QueryOptimizer::create_index(query_optimizer, large_dataset, ["region"])
  let env_version_index = QueryOptimizer::create_index(query_optimizer, large_dataset, ["environment", "version"])
  let service_region_index = QueryOptimizer::create_index(query_optimizer, large_dataset, ["service", "region"])
  let cpu_mem_index = QueryOptimizer::create_index(query_optimizer, large_dataset, ["cpu_usage", "memory_usage"])
  
  // 测试简单查询优化
  let simple_query = MultidimensionalQuery::equals("service", "database")
  
  // 未优化的查询执行
  let unoptimized_start = Time::now()
  let unoptimized_result = QueryManager::execute_direct(large_dataset, simple_query)
  let unoptimized_end = Time::now()
  let unoptimized_duration = unoptimized_end - unoptimized_start
  
  // 优化的查询执行
  let optimized_start = Time::now()
  let optimized_result = QueryOptimizer::execute(query_optimizer, large_dataset, simple_query)
  let optimized_end = Time::now()
  let optimized_duration = optimized_end - optimized_start
  
  // 验证优化效果
  assert_true(optimized_duration <= unoptimized_duration) // 优化后应该更快或相等
  assert_eq(unoptimized_result.size(), optimized_result.size()) // 结果应该相同
  
  // 测试复杂查询优化
  let complex_query = MultidimensionalQuery::and([
    MultidimensionalQuery::in_list("service", ["database", "cache"]),
    MultidimensionalQuery::in_list("environment", ["prod", "staging"]),
    MultidimensionalQuery::greater_than("cpu_usage", 50.0),
    MultidimensionalQuery::less_than("memory_usage", 80.0),
    MultidimensionalQuery::between("response_time", 10.0, 500.0)
  ])
  
  // 未优化的复杂查询
  let complex_unoptimized_start = Time::now()
  let complex_unoptimized_result = QueryManager::execute_direct(large_dataset, complex_query)
  let complex_unoptimized_end = Time::now()
  let complex_unoptimized_duration = complex_unoptimized_end - complex_unoptimized_start
  
  // 优化的复杂查询
  let complex_optimized_start = Time::now()
  let complex_optimized_result = QueryOptimizer::execute(query_optimizer, large_dataset, complex_query)
  let complex_optimized_end = Time::now()
  let complex_optimized_duration = complex_optimized_end - complex_optimized_start
  
  // 验证复杂查询优化效果
  assert_true(complex_optimized_duration <= complex_unoptimized_duration)
  assert_eq(complex_unoptimized_result.size(), complex_optimized_result.size())
  
  // 测试查询计划优化
  let query_plan = QueryOptimizer::explain_query(query_optimizer, large_dataset, complex_query)
  
  // 验证查询计划
  assert_true(QueryPlan::has_steps(query_plan))
  assert_true(QueryPlan::has_estimated_cost(query_plan))
  assert_true(QueryPlan::has_index_usage(query_plan))
  
  let steps = QueryPlan::steps(query_plan)
  assert_true(steps.length() > 0)
  
  let estimated_cost = QueryPlan::estimated_cost(query_plan)
  assert_true(estimated_cost > 0.0)
  
  let index_usage = QueryPlan::index_usage(query_plan)
  assert_true(index_usage.length() > 0)
  
  // 测试并行查询优化
  let parallel_query = MultidimensionalQuery::or([
    MultidimensionalQuery::and([
      MultidimensionalQuery::equals("service", "api"),
      MultidimensionalQuery::greater_than("cpu_usage", 70.0)
    ]),
    MultidimensionalQuery::and([
      MultidimensionalQuery::equals("service", "database"),
      MultidimensionalQuery::greater_than("memory_usage", 70.0)
    ]),
    MultidimensionalQuery::and([
      MultidimensionalQuery::equals("service", "cache"),
      MultidimensionalQuery::greater_than("response_time", 500.0)
    ])
  ])
  
  // 串行执行
  let serial_start = Time::now()
  let serial_result = QueryManager::execute_direct(large_dataset, parallel_query)
  let serial_end = Time::now()
  let serial_duration = serial_end - serial_start
  
  // 并行执行
  let parallel_start = Time::now()
  let parallel_result = QueryOptimizer::execute_parallel(query_optimizer, large_dataset, parallel_query)
  let parallel_end = Time::now()
  let parallel_duration = parallel_end - parallel_start
  
  // 验证并行执行效果
  assert_true(parallel_duration <= serial_duration)
  assert_eq(serial_result.size(), parallel_result.size())
  
  // 测试缓存优化
  let cached_query = MultidimensionalQuery::equals("environment", "prod")
  
  // 第一次执行（无缓存）
  let first_start = Time::now()
  let first_result = QueryOptimizer::execute(query_optimizer, large_dataset, cached_query)
  let first_end = Time::now()
  let first_duration = first_end - first_start
  
  // 第二次执行（有缓存）
  let second_start = Time::now()
  let second_result = QueryOptimizer::execute(query_optimizer, large_dataset, cached_query)
  let second_end = Time::now()
  let second_duration = second_end - second_start
  
  // 验证缓存效果
  assert_true(second_duration < first_duration) // 缓存应该更快
  assert_eq(first_result.size(), second_result.size())
  
  // 测试查询重写优化
  let rewrite_query = MultidimensionalQuery::and([
    MultidimensionalQuery::or([
      MultidimensionalQuery::equals("service", "api"),
      MultidimensionalQuery::equals("service", "database")
    ]),
    MultidimensionalQuery::greater_than("cpu_usage", 50.0)
  ])
  
  // 原始查询
  let original_start = Time::now()
  let original_result = QueryManager::execute_direct(large_dataset, rewrite_query)
  let original_end = Time::now()
  let original_duration = original_end - original_start
  
  // 重写后的查询
  let rewritten_start = Time::now()
  let rewritten_result = QueryOptimizer::execute_with_rewriting(query_optimizer, large_dataset, rewrite_query)
  let rewritten_end = Time::now()
  let rewritten_duration = rewritten_end - rewritten_start
  
  // 验证查询重写效果
  assert_true(rewritten_duration <= original_duration)
  assert_eq(original_result.size(), rewritten_result.size())
  
  // 生成查询优化报告
  let optimization_report = QueryOptimizer::generate_optimization_report(query_optimizer)
  
  // 验证优化报告
  assert_true(OptimizationReport::has_summary(optimization_report))
  assert_true(OptimizationReport::has_performance_improvements(optimization_report))
  assert_true(OptimizationReport::has_recommendations(optimization_report))
  
  let performance_improvements = OptimizationReport::performance_improvements(optimization_report)
  assert_true(PerformanceImprovements::has_average_speedup(performance_improvements))
  
  let average_speedup = PerformanceImprovements::average_speedup(performance_improvements)
  assert_true(average_speedup >= 1.0) // 平均加速比应该至少为1
}

// Test 4: 多维查询缓存和内存管理
test "multidimensional query caching and memory management" {
  // 创建查询缓存管理器
  let cache_manager = QueryCacheManager::new()
  
  // 配置缓存参数
  CacheManager::configure(cache_manager, [
    ("cache.max.size", IntValue(1000)), // 最大缓存1000个查询
    ("cache.memory.limit.mb", IntValue(256)), // 最大256MB内存
    ("cache.ttl.minutes", IntValue(30)), // 缓存30分钟
    ("eviction.policy", StringValue("lru")) // LRU淘汰策略
  ])
  
  // 创建测试数据集
  let cache_test_dataset = MultidimensionalDataset::new("cache_test")
  
  // 生成测试数据
  let base_time = Time::now() - (7 * 24 * 60 * 60 * 1000) // 7天前
  
  for i in 0..<10000 {
    let timestamp = base_time + (i * 60 * 1000) // 每分钟一个数据点
    
    let attributes = [
      ("service", StringValue(["api", "database", "cache", "queue"][i % 4])),
      ("environment", StringValue(["prod", "staging", "dev"][i % 3])),
      ("region", StringValue(["us-east", "us-west", "eu-west"][i % 3])),
      ("cpu_usage", FloatValue(20.0 + (Math::random() * 60.0))),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0))),
      ("response_time", FloatValue(10.0 + (Math::random() * 500.0)))
    ]
    
    let data_point = MultidimensionalDataPoint::new_with_timestamp(
      "cache_" + i.to_string(),
      attributes,
      timestamp
    )
    
    cache_test_dataset.add(data_point)
  }
  
  // 测试查询缓存
  let test_queries = [
    MultidimensionalQuery::equals("service", "api"),
    MultidimensionalQuery::equals("environment", "prod"),
    MultidimensionalQuery::equals("region", "us-east"),
    MultidimensionalQuery::greater_than("cpu_usage", 50.0),
    MultidimensionalQuery::less_than("memory_usage", 70.0),
    MultidimensionalQuery::between("response_time", 100.0, 300.0),
    MultidimensionalQuery::and([
      MultidimensionalQuery::equals("service", "database"),
      MultidimensionalQuery::equals("environment", "staging")
    ]),
    MultidimensionalQuery::or([
      MultidimensionalQuery::equals("service", "cache"),
      MultidimensionalQuery::equals("service", "queue")
    ])
  ]
  
  // 第一次执行查询（无缓存）
  let first_execution_results = []
  let first_execution_times = []
  
  for query in test_queries {
    let execution_start = Time::now()
    let result = CacheManager::execute_with_cache(cache_manager, cache_test_dataset, query)
    let execution_end = Time::now()
    let execution_time = execution_end - execution_start
    
    first_execution_results = first_execution_results.push(result)
    first_execution_times = first_execution_times.push(execution_time)
  }
  
  // 验证缓存状态
  let cache_stats = CacheManager::get_cache_statistics(cache_manager)
  assert_true(CacheStats::has_cache_size(cache_stats))
  assert_true(CacheStats::has_hit_rate(cache_stats))
  assert_true(CacheStats::has_memory_usage(cache_stats))
  
  // 第一次执行后，缓存应该为空或命中率很低
  let initial_hit_rate = CacheStats::hit_rate(cache_stats)
  assert_true(initial_hit_rate <= 0.1) // 命中率应该很低
  
  // 第二次执行查询（有缓存）
  let second_execution_results = []
  let second_execution_times = []
  
  for query in test_queries {
    let execution_start = Time::now()
    let result = CacheManager::execute_with_cache(cache_manager, cache_test_dataset, query)
    let execution_end = Time::now()
    let execution_time = execution_end - execution_start
    
    second_execution_results = second_execution_results.push(result)
    second_execution_times = second_execution_times.push(execution_time)
  }
  
  // 验证缓存效果
  let updated_cache_stats = CacheManager::get_cache_statistics(cache_manager)
  let updated_hit_rate = CacheStats::hit_rate(updated_cache_stats)
  
  // 第二次执行后，命中率应该很高
  assert_true(updated_hit_rate > 0.8) // 命中率应该很高
  
  // 验证缓存结果正确性
  for i in 0..<test_queries.length() {
    let first_result = first_execution_results[i]
    let second_result = second_execution_results[i]
    
    assert_eq(first_result.size(), second_result.size())
    
    // 验证结果内容相同
    for j in 0..<first_result.size() {
      assert_eq(first_result[j].id, second_result[j].id)
    }
  }
  
  // 验证缓存性能提升
  for i in 0..<test_queries.length() {
    let first_time = first_execution_times[i]
    let second_time = second_execution_times[i]
    
    // 缓存命中应该更快
    assert_true(second_time <= first_time)
  }
  
  // 测试缓存淘汰
  let additional_queries = []
  
  // 生成更多查询以填满缓存
  for i in 0..<1200 {
    let random_service = ["api", "database", "cache", "queue"][i % 4]
    let random_env = ["prod", "staging", "dev"][i % 3]
    
    let query = MultidimensionalQuery::and([
      MultidimensionalQuery::equals("service", random_service),
      MultidimensionalQuery::equals("environment", random_env),
      MultidimensionalQuery::greater_than("cpu_usage", 20.0 + (i % 50))
    ])
    
    additional_queries = additional_queries.push(query)
  }
  
  // 执行额外查询以触发淘汰
  for query in additional_queries {
    CacheManager::execute_with_cache(cache_manager, cache_test_dataset, query)
  }
  
  // 验证缓存淘汰
  let eviction_cache_stats = CacheManager::get_cache_statistics(cache_manager)
  let eviction_count = CacheStats::eviction_count(eviction_cache_stats)
  
  assert_true(eviction_count > 0) // 应该有淘汰发生
  
  // 验证缓存大小限制
  let cache_size = CacheStats::cache_size(eviction_cache_stats)
  assert_true(cache_size <= 1000) // 缓存大小不应超过限制
  
  // 测试缓存内存管理
  let memory_usage = CacheStats::memory_usage(eviction_cache_stats)
  assert_true(memory_usage <= 256 * 1024 * 1024) // 内存使用不应超过限制
  
  // 测试缓存TTL
  let ttl_test_query = MultidimensionalQuery::equals("service", "api")
  
  // 执行查询
  CacheManager::execute_with_cache(cache_manager, cache_test_dataset, ttl_test_query)
  
  // 手动推进时间以测试TTL
  CacheManager::advance_time(cache_manager, 35 * 60 * 1000) // 35分钟
  
  // 再次执行查询，应该缓存过期
  let ttl_execution_start = Time::now()
  let ttl_result = CacheManager::execute_with_cache(cache_manager, cache_test_dataset, ttl_test_query)
  let ttl_execution_end = Time::now()
  let ttl_execution_time = ttl_execution_end - ttl_execution_start
  
  // 验证缓存过期
  let ttl_cache_stats = CacheManager::get_cache_statistics(cache_manager)
  let expired_count = CacheStats::expired_count(ttl_cache_stats)
  
  assert_true(expired_count > 0) // 应该有过期缓存
  
  // 测试缓存清理
  let clear_result = CacheManager::clear_cache(cache_manager)
  assert_true(clear_result)
  
  // 验证缓存清理
  let cleared_cache_stats = CacheManager::get_cache_statistics(cache_manager)
  let cleared_cache_size = CacheStats::cache_size(cleared_cache_stats)
  
  assert_eq(cleared_cache_size, 0) // 缓存应该被清空
  
  // 生成缓存性能报告
  let cache_performance_report = CacheManager::generate_performance_report(cache_manager)
  
  // 验证缓存性能报告
  assert_true(CachePerformanceReport::has_summary(cache_performance_report))
  assert_true(CachePerformanceReport::has_hit_rate_history(cache_performance_report))
  assert_true(CachePerformanceReport::has_memory_usage_history(cache_performance_report))
  assert_true(CachePerformanceReport::has_recommendations(cache_performance_report))
  
  let hit_rate_history = CachePerformanceReport::hit_rate_history(cache_performance_report)
  assert_true(HitRateHistory::has_data_points(hit_rate_history))
  
  let recommendations = CachePerformanceReport::recommendations(cache_performance_report)
  assert_true(CacheRecommendations::has_cache_size_recommendations(recommendations))
  assert_true(CacheRecommendations::has_ttl_recommendations(recommendations))
}

// Test 5: 多维查询安全和权限控制
test "multidimensional query security and access control" {
  // 创建查询安全管理器
  let security_manager = QuerySecurityManager::new()
  
  // 配置安全参数
  SecurityManager::configure(security_manager, [
    ("row.level.security", BoolValue(true)),
    ("column.level.security", BoolValue(true)),
    ("query.audit.enabled", BoolValue(true)),
    ("injection.protection", BoolValue(true))
  ])
  
  // 创建安全多维数据集
  let secure_dataset = MultidimensionalDataset::new("secure_telemetry")
  
  // 生成包含敏感信息的多维数据
  let base_time = Time::now() - (30 * 24 * 60 * 60 * 1000) // 30天前
  
  for i in 0..<5000 {
    let timestamp = base_time + (i * 10 * 60 * 1000) // 每10分钟一个数据点
    
    let attributes = [
      // 公开属性
      ("service", StringValue(["api", "database", "cache", "queue"][i % 4])),
      ("environment", StringValue(["prod", "staging", "dev"][i % 3])),
      ("region", StringValue(["us-east", "us-west", "eu-west"][i % 3])),
      ("cpu_usage", FloatValue(20.0 + (Math::random() * 60.0))),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0))),
      
      // 敏感属性
      ("user_id", StringValue("user_" + (i % 100).to_string())),
      ("session_id", StringValue("session_" + (i % 200).to_string())),
      ("ip_address", StringValue("192.168.1." + (i % 254).to_string())),
      ("personal_data", StringValue("sensitive_info_" + i.to_string())),
      ("api_key", StringValue("key_" + (i % 50).to_string())),
      
      // 部门级属性
      ("department", StringValue(["engineering", "marketing", "sales", "finance"][i % 4])),
      ("team", StringValue(["backend", "frontend", "devops", "qa"][i % 4])),
      ("project", StringValue(["project_a", "project_b", "project_c"][i % 3])),
      
      // 管理级属性
      ("cost_center", StringValue("cc_" + (i % 10).to_string())),
      ("budget_code", StringValue("budget_" + (i % 20).to_string())),
      ("revenue_impact", FloatValue(Math::random() * 10000.0))
    ]
    
    let data_point = MultidimensionalDataPoint::new_with_timestamp(
      "secure_" + i.to_string(),
      attributes,
      timestamp
    )
    
    secure_dataset.add(data_point)
  }
  
  // 创建用户和角色
  let users = [
    User::new("public_user", "Public User", ["public_viewer"]),
    User::new("department_user", "Department User", ["department_viewer", "department_editor"]),
    User::new("admin_user", "Admin User", ["admin", "auditor"])
  ]
  
  // 定义访问权限
  let access_policies = [
    AccessPolicy::new(
      "public_access",
      ["public_viewer"],
      ["service", "environment", "region", "cpu_usage", "memory_usage"],
      [] // 无行级限制
    ),
    AccessPolicy::new(
      "department_access",
      ["department_viewer", "department_editor"],
      ["service", "environment", "region", "cpu_usage", "memory_usage", "department", "team", "project"],
      [("department", "engineering")] // 只能访问工程部门数据
    ),
    AccessPolicy::new(
      "admin_access",
      ["admin", "auditor"],
      [], // 无列级限制
      [] // 无行级限制
    )
  ]
  
  for policy in access_policies {
    SecurityManager::add_access_policy(security_manager, policy)
  }
  
  // 测试公开用户查询
  let public_user = users[0]
  let public_query = MultidimensionalQuery::equals("service", "api")
  
  let public_result = SecurityManager::execute_secure_query(
    security_manager,
    secure_dataset,
    public_query,
    public_user
  )
  
  // 验证公开用户只能访问公开属性
  assert_true(public_result.size() > 0)
  
  for data_point in public_result {
    // 应该包含公开属性
    assert_true(SecureDataPoint::has_attribute(data_point, "service"))
    assert_true(SecureDataPoint::has_attribute(data_point, "cpu_usage"))
    
    // 不应该包含敏感属性
    assert_false(SecureDataPoint::has_attribute(data_point, "user_id"))
    assert_false(SecureDataPoint::has_attribute(data_point, "personal_data"))
    assert_false(SecureDataPoint::has_attribute(data_point, "api_key"))
  }
  
  // 测试部门用户查询
  let department_user = users[1]
  let department_query = MultidimensionalQuery::equals("service", "database")
  
  let department_result = SecurityManager::execute_secure_query(
    security_manager,
    secure_dataset,
    department_query,
    department_user
  )
  
  // 验证部门用户只能访问部门数据
  assert_true(department_result.size() > 0)
  
  for data_point in department_result {
    // 应该包含部门级属性
    assert_true(SecureDataPoint::has_attribute(data_point, "department"))
    assert_true(SecureDataPoint::has_attribute(data_point, "team"))
    
    // 验证行级安全
    match SecureDataPoint::get_attribute(data_point, "department") {
      Some(StringValue(dept)) => assert_eq(dept, "engineering"),
      _ => assert_true(false)
    }
    
    // 不应该包含管理级属性
    assert_false(SecureDataPoint::has_attribute(data_point, "cost_center"))
    assert_false(SecureDataPoint::has_attribute(data_point, "budget_code"))
  }
  
  // 测试管理员用户查询
  let admin_user = users[2]
  let admin_query = MultidimensionalQuery::equals("service", "cache")
  
  let admin_result = SecurityManager::execute_secure_query(
    security_manager,
    secure_dataset,
    admin_query,
    admin_user
  )
  
  // 验证管理员可以访问所有属性
  assert_true(admin_result.size() > 0)
  
  for data_point in admin_result {
    // 应该包含所有属性
    assert_true(SecureDataPoint::has_attribute(data_point, "service"))
    assert_true(SecureDataPoint::has_attribute(data_point, "user_id"))
    assert_true(SecureDataPoint::has_attribute(data_point, "personal_data"))
    assert_true(SecureDataPoint::has_attribute(data_point, "cost_center"))
    assert_true(SecureDataPoint::has_attribute(data_point, "revenue_impact"))
  }
  
  // 测试SQL注入防护
  let injection_queries = [
    "service'; DROP TABLE telemetry; --",
    "service' OR '1'='1",
    "service' UNION SELECT * FROM sensitive_data --"
  ]
  
  for injection_query in injection_queries {
    let injection_result = SecurityManager::execute_secure_query(
      security_manager,
      secure_dataset,
      MultidimensionalQuery::raw(injection_query),
      public_user
    )
    
    // 注入查询应该被阻止或返回空结果
    assert_true(injection_result.size() == 0)
  }
  
  // 测试查询审计
  let audit_queries = [
    (public_user, MultidimensionalQuery::equals("service", "api")),
    (department_user, MultidimensionalQuery::equals("service", "database")),
    (admin_user, MultidimensionalQuery::equals("service", "cache"))
  ]
  
  for (user, query) in audit_queries {
    SecurityManager::execute_secure_query(
      security_manager,
      secure_dataset,
      query,
      user
    )
  }
  
  // 验证审计日志
  let audit_logs = SecurityManager::get_audit_logs(security_manager)
  assert_true(audit_logs.length() >= audit_queries.length())
  
  // 验证审计日志内容
  for log in audit_logs {
    assert_true(AuditLog::has_user_id(log))
    assert_true(AuditLog::has_query(log))
    assert_true(AuditLog::has_timestamp(log))
    assert_true(AuditLog::has_result_count(log))
  }
  
  // 测试动态权限变更
  let dynamic_policy = AccessPolicy::new(
    "dynamic_access",
    ["department_viewer"],
    ["service", "cpu_usage"], // 限制列
    [("department", "marketing")] // 限制行
  )
  
  SecurityManager::add_access_policy(security_manager, dynamic_policy)
  
  // 重新执行部门用户查询
  let updated_department_result = SecurityManager::execute_secure_query(
    security_manager,
    secure_dataset,
    department_query,
    department_user
  )
  
  // 验证权限变更生效
  for data_point in updated_department_result {
    // 应该只包含新策略允许的属性
    assert_true(SecureDataPoint::has_attribute(data_point, "service"))
    assert_true(SecureDataPoint::has_attribute(data_point, "cpu_usage"))
    
    // 不应该包含之前允许但现在被限制的属性
    assert_false(SecureDataPoint::has_attribute(data_point, "memory_usage"))
    assert_false(SecureDataPoint::has_attribute(data_point, "team"))
    
    // 验证行级限制变更
    match SecureDataPoint::get_attribute(data_point, "department") {
      Some(StringValue(dept)) => assert_eq(dept, "marketing"),
      _ => assert_true(false)
    }
  }
  
  // 生成安全报告
  let security_report = SecurityManager::generate_security_report(security_manager)
  
  // 验证安全报告
  assert_true(SecurityReport::has_access_summary(security_report))
  assert_true(SecurityReport::has_violation_summary(security_report))
  assert_true(SecurityReport::has_recommendations(security_report))
  
  let access_summary = SecurityReport::access_summary(security_report)
  assert_true(AccessSummary::has_user_access_stats(access_summary))
  assert_true(AccessSummary::has_policy_effectiveness(access_summary))
  
  let violation_summary = SecurityReport::violation_summary(security_report)
  assert_true(ViolationSummary::has_blocked_queries(violation_summary))
  assert_true(ViolationSummary::has_injection_attempts(violation_summary))
}