// Azimuth Multidimensional Attribute Query Tests
// 多维属性查询测试用例 - 专注于复杂查询、多维分析和属性索引

// Test 1: 多维属性索引构建
test "multidimensional attribute index construction" {
  // 创建多维索引管理器
  let index_manager = MultidimensionalIndexManager::new()
  
  // 配置索引策略
  let index_config = IndexConfiguration::new()
  IndexConfig::set_max_memory_mb(index_config, 512)
  IndexConfig::set_index_update_threshold(index_config, 1000)
  IndexConfig::set_compaction_threshold(index_config, 0.3)
  
  // 创建多维属性索引
  let telemetry_index = MultidimensionalIndex::new("telemetry_data", index_config)
  
  // 添加维度定义
  Index::add_dimension(telemetry_index, Dimension::new("service", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("instance", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("environment", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("cpu_usage", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("memory_usage", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("timestamp", DimensionType::Temporal))
  Index::add_dimension(telemetry_index, Dimension::new("region", DimensionType::Geospatial))
  
  // 生成测试数据
  let test_data = generate_multidimensional_test_data(10000)
  
  // 构建索引
  let index_builder = IndexBuilder::new(telemetry_index)
  IndexBuilder::set_batch_size(index_builder, 1000)
  
  let build_start = Time::now()
  for i in 0..<test_data.length() {
    let data_point = test_data[i]
    IndexBuilder::add_data_point(index_builder, data_point)
    
    // 每1000个数据点提交一次
    if i % 1000 == 0 {
      IndexBuilder::commit_batch(index_builder)
    }
  }
  IndexBuilder::finalize(index_builder)
  let build_time = Time::now() - build_start
  
  // 验证索引构建
  let index_stats = Index::get_statistics(telemetry_index)
  assert_true(index_stats.total_points == test_data.length())
  assert_true(index_stats.indexed_points == test_data.length())
  assert_true(index_stats.build_time_ms > 0)
  assert_true(index_stats.index_size_bytes > 0)
  
  // 验证维度统计
  for dimension in Index::get_dimensions(telemetry_index) {
    let dim_stats = Index::get_dimension_statistics(telemetry_index, dimension.name)
    assert_true(dim_stats.unique_values > 0)
    assert_true(dim_stats.null_count >= 0)
    
    match dimension.type {
      DimensionType::Numeric => {
        assert_true(dim_stats.min_value < dim_stats.max_value)
        assert_true(dim_stats.avg_value >= dim_stats.min_value && dim_stats.avg_value <= dim_stats.max_value)
      }
      DimensionType::Categorical => {
        assert_true(dim_stats.cardinality > 0)
      }
      DimensionType::Temporal => {
        assert_true(dim_stats.time_range_ms > 0)
      }
      DimensionType::Geospatial => {
        assert_true(dim_stats.bounding_box.is_some())
      }
    }
  }
  
  // 测试索引持久化
  let persistence_result = IndexManager::persist_index(index_manager, telemetry_index)
  match persistence_result {
    Ok(index_id) => {
      assert_true(index_id.length() > 0)
      
      // 测试索引加载
      let loaded_index = IndexManager::load_index(index_manager, index_id)
      match loaded_index {
        Some(index) => {
          assert_eq(index.name, "telemetry_data")
          assert_eq(Index::get_statistics(index).total_points, test_data.length())
        }
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 2: 复杂多维查询执行
test "complex multidimensional query execution" {
  // 创建查询引擎
  let query_engine = MultidimensionalQueryEngine::new()
  
  // 加载测试索引
  let index_manager = MultidimensionalIndexManager::new()
  let telemetry_index = create_test_telemetry_index()
  
  QueryEngine::add_index(query_engine, telemetry_index)
  
  // 测试单维度查询
  let single_dim_query = MultidimensionalQuery::new()
  Query::add_filter(single_dim_query, Filter::equals("service", "web"))
  
  let single_dim_result = QueryEngine::execute(query_engine, single_dim_query)
  assert_true(single_dim_result.points.length() > 0)
  
  for point in single_dim_result.points {
    match TelemetryDataPoint::get_attribute(point, "service") {
      Some(service) => assert_eq(service, "web")
      None => assert_true(false)
    }
  }
  
  // 测试范围查询
  let range_query = MultidimensionalQuery::new()
  Query::add_filter(range_query, Filter::range("cpu_usage", 50.0, 80.0))
  
  let range_result = QueryEngine::execute(query_engine, range_query)
  assert_true(range_result.points.length() > 0)
  
  for point in range_result.points {
    match TelemetryDataPoint::get_attribute(point, "cpu_usage") {
      Some(cpu) => {
        assert_true(cpu >= 50.0 && cpu <= 80.0)
      }
      None => assert_true(false)
    }
  }
  
  // 测试多维度AND查询
  let and_query = MultidimensionalQuery::new()
  Query::add_filter(and_query, Filter::equals("environment", "prod"))
  Query::add_filter(and_query, Filter::range("memory_usage", 60.0, 90.0))
  Query::add_filter(and_query, Filter::in_set("region", ["us-east", "us-west"]))
  
  let and_result = QueryEngine::execute(query_engine, and_query)
  assert_true(and_result.points.length() > 0)
  
  for point in and_result.points {
    match TelemetryDataPoint::get_attribute(point, "environment") {
      Some(env) => assert_eq(env, "prod")
      None => assert_true(false)
    }
    
    match TelemetryDataPoint::get_attribute(point, "memory_usage") {
      Some(mem) => assert_true(mem >= 60.0 && mem <= 90.0)
      None => assert_true(false)
    }
    
    match TelemetryDataPoint::get_attribute(point, "region") {
      Some(region) => assert_true(region == "us-east" || region == "us-west")
      None => assert_true(false)
    }
  }
  
  // 测试多维度OR查询
  let or_query = MultidimensionalQuery::new()
  Query::add_filter(or_query, Filter::equals("service", "database"))
  Query::add_filter(or_query, Filter::greater_than("cpu_usage", 90.0), FilterOperator::Or)
  
  let or_result = QueryEngine::execute(query_engine, or_query)
  assert_true(or_result.points.length() > 0)
  
  for point in or_result.points {
    let service_match = match TelemetryDataPoint::get_attribute(point, "service") {
      Some(service) => service == "database"
      None => false
    }
    
    let cpu_match = match TelemetryDataPoint::get_attribute(point, "cpu_usage") {
      Some(cpu) => cpu > 90.0
      None => false
    }
    
    assert_true(service_match || cpu_match)
  }
  
  // 测试时间范围查询
  let time_range_query = MultidimensionalQuery::new()
  let now = Time::now()
  Query::add_filter(time_range_query, Filter::time_range("timestamp", now - 3600000, now)) // 最近1小时
  
  let time_range_result = QueryEngine::execute(query_engine, time_range_query)
  assert_true(time_range_result.points.length() > 0)
  
  for point in time_range_result.points {
    assert_true(point.timestamp >= now - 3600000 && point.timestamp <= now)
  }
  
  // 测试地理空间查询
  let geo_query = MultidimensionalQuery::new()
  let bbox = BoundingBox::new(
    min_lat = 40.0,
    min_lon = -75.0,
    max_lat = 42.0,
    max_lon = -73.0
  )
  Query::add_filter(geo_query, Filter::within_bbox("region", bbox))
  
  let geo_result = QueryEngine::execute(query_engine, geo_query)
  assert_true(geo_result.points.length() >= 0) // 可能为空
  
  // 测试复杂嵌套查询
  let complex_query = MultidimensionalQuery::new()
  let sub_query1 = Filter::and([
    Filter::equals("environment", "prod"),
    Filter::range("cpu_usage", 70.0, 100.0)
  ])
  let sub_query2 = Filter::and([
    Filter::equals("service", "database"),
    Filter::greater_than("memory_usage", 80.0)
  ])
  Query::add_filter(complex_query, Filter::or([sub_query1, sub_query2]))
  Query::add_time_range(complex_query, now - 7200000, now) // 最近2小时
  Query::set_limit(complex_query, 100)
  Query::set_order_by(complex_query, [("timestamp", OrderDirection::Desc)])
  
  let complex_result = QueryEngine::execute(query_engine, complex_query)
  assert_true(complex_result.points.length() <= 100)
  
  // 验证排序
  for i in 1..<complex_result.points.length() {
    assert_true(complex_result.points[i-1].timestamp >= complex_result.points[i].timestamp)
  }
}

// Test 3: 聚合查询和分组分析
test "aggregation queries and group analysis" {
  // 创建聚合查询引擎
  let aggregation_engine = AggregationQueryEngine::new()
  
  // 加载测试索引
  let telemetry_index = create_test_telemetry_index()
  AggregationEngine::add_index(aggregation_engine, telemetry_index)
  
  // 测试基本聚合
  let basic_agg_query = AggregationQuery::new()
  AggregationQuery::add_metric(basic_agg_query, AggregationMetric::avg("cpu_usage"))
  AggregationQuery::add_metric(basic_agg_query, AggregationMetric::max("memory_usage"))
  AggregationQuery::add_metric(basic_agg_query, AggregationMetric::count("service"))
  
  let basic_agg_result = AggregationEngine::execute(aggregation_engine, basic_agg_query)
  assert_true(basic_agg_result.aggregations.length() == 3)
  
  for agg in basic_agg_result.aggregations {
    match agg.metric_type {
      AggregationType::Avg => assert_true(agg.value >= 0.0)
      AggregationType::Max => assert_true(agg.value >= 0.0)
      AggregationType::Count => assert_true(agg.value > 0.0)
      _ => assert_true(false)
    }
  }
  
  // 测试分组聚合
  let group_agg_query = AggregationQuery::new()
  AggregationQuery::add_group_by(group_agg_query, "service")
  AggregationQuery::add_group_by(group_agg_query, "environment")
  AggregationQuery::add_metric(group_agg_query, AggregationMetric::avg("cpu_usage"))
  AggregationQuery::add_metric(group_agg_query, AggregationMetric::sum("request_count"))
  AggregationQuery::add_metric(group_agg_query, AggregationMetric::count("*"))
  
  let group_agg_result = AggregationEngine::execute(aggregation_engine, group_agg_query)
  assert_true(group_agg_result.groups.length() > 0)
  
  for group in group_agg_result.groups {
    assert_true(group.dimensions.length() == 2)
    assert_true(group.aggregations.length() == 3)
    
    // 验证分组键
    assert_true(group.dimensions.contains("service"))
    assert_true(group.dimensions.contains("environment"))
    
    // 验证聚合值
    for agg in group.aggregations {
      assert_true(agg.value >= 0.0)
    }
  }
  
  // 测试时间窗口聚合
  let time_window_agg_query = AggregationQuery::new()
  let now = Time::now()
  AggregationQuery::add_time_window(time_window_agg_query, TimeWindow::Hourly, now - 86400000, now)
  AggregationQuery::add_metric(time_window_agg_query, AggregationMetric::avg("cpu_usage"))
  AggregationQuery::add_metric(time_window_agg_query, AggregationMetric::p95("response_time"))
  
  let time_window_result = AggregationEngine::execute(aggregation_engine, time_window_agg_query)
  assert_true(time_window_result.time_windows.length() > 0)
  
  for window in time_window_result.time_windows {
    assert_true(window.start_time < window.end_time)
    assert_true(window.aggregations.length() == 2)
  }
  
  // 测试百分位数聚合
  let percentile_agg_query = AggregationQuery::new()
  AggregationQuery::add_group_by(percentile_agg_query, "service")
  AggregationQuery::add_metric(percentile_agg_query, AggregationMetric::p50("response_time"))
  AggregationQuery::add_metric(percentile_agg_query, AggregationMetric::p90("response_time"))
  AggregationQuery::add_metric(percentile_agg_query, AggregationMetric::p95("response_time"))
  AggregationQuery::add_metric(percentile_agg_query, AggregationMetric::p99("response_time"))
  
  let percentile_result = AggregationEngine::execute(aggregation_engine, percentile_agg_query)
  assert_true(percentile_result.groups.length() > 0)
  
  for group in percentile_result.groups {
    let p50 = AggregationGroup::get_metric(group, "p50_response_time")
    let p90 = AggregationGroup::get_metric(group, "p90_response_time")
    let p95 = AggregationGroup::get_metric(group, "p95_response_time")
    let p99 = AggregationGroup::get_metric(group, "p99_response_time")
    
    match (p50, p90, p95, p99) {
      (Some(v50), Some(v90), Some(v95), Some(v99)) => {
        assert_true(v50.value <= v90.value)
        assert_true(v90.value <= v95.value)
        assert_true(v95.value <= v99.value)
      }
      _ => assert_true(false)
    }
  }
  
  // 测试复合聚合
  let composite_agg_query = AggregationQuery::new()
  AggregationQuery::add_group_by(composite_agg_query, "service")
  AggregationQuery::add_time_window(composite_agg_query, TimeWindow::Daily, now - 604800000, now)
  AggregationQuery::add_metric(composite_agg_query, AggregationMetric::avg("cpu_usage"))
  AggregationQuery::add_metric(composite_agg_query, AggregationMetric::rate("error_count"))
  AggregationQuery::add_metric(composite_agg_query, AggregationMetric::ratio("error_count", "request_count"))
  
  let composite_result = AggregationEngine::execute(aggregation_engine, composite_agg_query)
  assert_true(composite_result.groups.length() > 0)
  
  for group in composite_result.groups {
    assert_true(group.dimensions.length() == 2) // service + time window
    assert_true(group.aggregations.length() == 3)
    
    let ratio = AggregationGroup::get_metric(group, "ratio_error_count_request_count")
    match ratio {
      Some(r) => {
        assert_true(r.value >= 0.0 && r.value <= 1.0)
      }
      None => assert_true(false)
    }
  }
}

// Test 4: 查询优化和性能调优
test "query optimization and performance tuning" {
  // 创建查询优化器
  let query_optimizer = QueryOptimizer::new()
  
  // 配置优化策略
  Optimizer::add_strategy(query_optimizer, OptimizationStrategy::IndexSelection)
  Optimizer::add_strategy(query_optimizer, OptimizationStrategy::FilterReordering)
  Optimizer::add_strategy(query_optimizer, OptimizationStrategy::PredicatePushdown)
  Optimizer::add_strategy(query_optimizer, OptimizationStrategy::QueryCaching)
  
  // 加载测试索引
  let telemetry_index = create_test_telemetry_index()
  Optimizer::add_index(query_optimizer, telemetry_index)
  
  // 创建复杂查询
  let complex_query = MultidimensionalQuery::new()
  Query::add_filter(complex_query, Filter::equals("environment", "prod"))
  Query::add_filter(complex_query, Filter::in_set("service", ["web", "api", "database"]))
  Query::add_filter(complex_query, Filter::range("cpu_usage", 50.0, 90.0))
  Query::add_filter(complex_query, Filter::greater_than("memory_usage", 60.0))
  Query::add_filter(complex_query, Filter::time_range("timestamp", Time::now() - 3600000, Time::now()))
  
  // 优化查询
  let optimized_query = Optimizer::optimize(query_optimizer, complex_query)
  assert_true(optimized_query !== None)
  
  let opt_query = optimized_query.unwrap()
  
  // 验证优化效果
  let original_plan = Query::get_execution_plan(complex_query)
  let optimized_plan = Query::get_execution_plan(opt_query)
  
  assert_true(optimized_plan.estimated_cost <= original_plan.estimated_cost)
  assert_true(optimized_plan.index_usage >= original_plan.index_usage)
  
  // 测试查询执行性能
  let query_engine = MultidimensionalQueryEngine::new()
  QueryEngine::add_index(query_engine, telemetry_index)
  
  // 执行原始查询
  let original_start = Time::now()
  let original_result = QueryEngine::execute(query_engine, complex_query)
  let original_time = Time::now() - original_start
  
  // 执行优化查询
  let optimized_start = Time::now()
  let optimized_result = QueryEngine::execute(query_engine, opt_query)
  let optimized_time = Time::now() - optimized_start
  
  // 验证结果一致性
  assert_eq(original_result.points.length(), optimized_result.points.length())
  
  // 验证性能提升
  assert_true(optimized_time <= original_time * 1.1) // 允许10%误差
  
  // 测试查询缓存
  let cache_result = QueryEngine::execute(query_engine, opt_query) // 第二次执行
  let cache_time = Time::now() - optimized_start
  
  assert_true(cache_time < optimized_time) // 缓存应该更快
  
  // 测试并发查询
  let concurrent_queries = []
  for i in 0..<10 {
    let concurrent_query = MultidimensionalQuery::new()
    Query::add_filter(concurrent_query, Filter::equals("service", ["web", "api", "database"][i % 3]))
    Query::add_filter(concurrent_query, Filter::range("cpu_usage", 30.0 + i.to_float() * 5.0, 80.0))
    
    concurrent_queries = concurrent_queries.push(concurrent_query)
  }
  
  let concurrent_start = Time::now()
  let concurrent_results = QueryEngine::execute_concurrent(query_engine, concurrent_queries)
  let concurrent_time = Time::now() - concurrent_start
  
  assert_eq(concurrent_results.length(), 10)
  for result in concurrent_results {
    assert_true(result.points.length() > 0)
  }
  
  // 测试查询计划分析
  let plan_analyzer = QueryPlanAnalyzer::new()
  let plan_analysis = Analyzer::analyze_plan(plan_analyzer, opt_query)
  
  assert_true(plan_analysis.indexes_used.length() > 0)
  assert_true(plan_analysis.filters_applied > 0)
  assert_true(plan_analysis.estimated_rows > 0)
  
  // 测试性能基准
  let benchmark_queries = generate_benchmark_queries(20)
  let benchmark_results = []
  
  for query in benchmark_queries {
    let start = Time::now()
    let result = QueryEngine::execute(query_engine, query)
    let duration = Time::now() - start
    
    benchmark_results = benchmark_results.push(BenchmarkResult::new(
      query = query,
      result_count = result.points.length(),
      execution_time_ms = duration
    ))
  }
  
  // 验证基准结果
  let avg_execution_time = ArrayUtils::sum(benchmark_results.map(fn(r) { r.execution_time_ms })).to_float() / benchmark_results.length().to_float()
  let max_execution_time = ArrayUtils::max(benchmark_results.map(fn(r) { r.execution_time_ms }))
  
  assert_true(avg_execution_time > 0)
  assert_true(max_execution_time > 0)
  assert_true(max_execution_time <= avg_execution_time * 5) // 最大执行时间不应该超过平均时间的5倍
}

// Test 5: 高级查询特性
test "advanced query features" {
  // 创建高级查询引擎
  let advanced_engine = AdvancedQueryEngine::new()
  
  // 加载测试索引
  let telemetry_index = create_test_telemetry_index()
  AdvancedEngine::add_index(advanced_engine, telemetry_index)
  
  // 测试模糊查询
  let fuzzy_query = MultidimensionalQuery::new()
  Query::add_filter(fuzzy_query, Filter::fuzzy("service", "web", 0.8)) // 80%相似度
  
  let fuzzy_result = AdvancedEngine::execute(advanced_engine, fuzzy_query)
  assert_true(fuzzy_result.points.length() > 0)
  
  // 测试正则表达式查询
  let regex_query = MultidimensionalQuery::new()
  Query::add_filter(regex_query, Filter::regex("instance", r"server-\d+"))
  
  let regex_result = AdvancedEngine::execute(advanced_engine, regex_query)
  assert_true(regex_result.points.length() > 0)
  
  for point in regex_result.points {
    match TelemetryDataPoint::get_attribute(point, "instance") {
      Some(instance) => assert_true(Regex::matches(r"server-\d+", instance))
      None => assert_true(false)
    }
  }
  
  // 测试地理距离查询
  let geo_distance_query = MultidimensionalQuery::new()
  let center_point = GeoPoint::new(40.7128, -74.0060) // 纽约
  Query::add_filter(geo_distance_query, Filter::within_distance("region", center_point, 100.0)) // 100公里内
  
  let geo_distance_result = AdvancedEngine::execute(advanced_engine, geo_distance_query)
  assert_true(geo_distance_result.points.length() >= 0)
  
  // 测试相似性查询
  let similarity_query = MultidimensionalQuery::new()
  let reference_point = generate_reference_telemetry_point()
  Query::add_filter(similarity_query, Filter::similarity(reference_point, ["cpu_usage", "memory_usage"], 0.9)) // 90%相似度
  
  let similarity_result = AdvancedEngine::execute(advanced_engine, similarity_query)
  assert_true(similarity_result.points.length() > 0)
  
  // 测试异常检测查询
  let anomaly_query = MultidimensionalQuery::new()
  Query::add_filter(anomaly_query, Filter::anomaly_detection(["cpu_usage", "memory_usage"], 0.95)) // 95%置信度
  
  let anomaly_result = AdvancedEngine::execute(advanced_engine, anomaly_query)
  assert_true(anomaly_result.points.length() >= 0)
  
  // 测试趋势分析查询
  let trend_query = MultidimensionalQuery::new()
  Query::add_filter(trend_query, Filter::time_range("timestamp", Time::now() - 86400000, Time::now()))
  Query::add_trend_analysis(trend_query, "cpu_usage", TrendDirection::Increasing, 0.8) // 80%置信度
  
  let trend_result = AdvancedEngine::execute(advanced_engine, trend_query)
  assert_true(trend_result.points.length() >= 0)
  
  // 测试预测查询
  let prediction_query = MultidimensionalQuery::new()
  Query::add_filter(prediction_query, Filter::time_range("timestamp", Time::now() - 3600000, Time::now()))
  Query::add_prediction(prediction_query, "cpu_usage", 1800000) // 预测未来30分钟
  
  let prediction_result = AdvancedEngine::execute(advanced_engine, prediction_query)
  assert_true(prediction_result.predictions.length() > 0)
  
  for prediction in prediction_result.predictions {
    assert_true(prediction.timestamp > Time::now())
    assert_true(prediction.confidence >= 0.0 && prediction.confidence <= 1.0)
  }
  
  // 测试关联规则查询
  let association_query = MultidimensionalQuery::new()
  Query::add_association_analysis(association_query, ["service", "environment"], "cpu_usage", 0.7, 0.5) // 70%支持度，50%置信度
  
  let association_result = AdvancedEngine::execute(advanced_engine, association_query)
  assert_true(association_result.associations.length() >= 0)
  
  // 测试自定义函数查询
  let custom_function_query = MultidimensionalQuery::new()
  Query::add_custom_function(custom_function_query, "efficiency_score", fn(point) {
    let cpu = match TelemetryDataPoint::get_attribute(point, "cpu_usage") {
      Some(v) => v
      None => 0.0
    }
    let memory = match TelemetryDataPoint::get_attribute(point, "memory_usage") {
      Some(v) => v
      None => 0.0
    }
    100.0 - (cpu + memory) / 2.0
  })
  Query::add_filter(custom_function_query, Filter::greater_than("efficiency_score", 50.0))
  
  let custom_function_result = AdvancedEngine::execute(advanced_engine, custom_function_query)
  assert_true(custom_function_result.points.length() > 0)
  
  for point in custom_function_result.points {
    let efficiency = match TelemetryDataPoint::get_attribute(point, "efficiency_score") {
      Some(v) => v
      None => 0.0
    }
    assert_true(efficiency > 50.0)
  }
}

// 辅助函数：生成多维测试数据
fn generate_multidimensional_test_data(count : Int) -> Array[TelemetryDataPoint] {
  let data = []
  let base_time = Time::now() - 86400000 // 24小时前开始
  
  let services = ["web", "api", "database", "cache", "queue"]
  let instances = ["server-01", "server-02", "server-03", "server-04", "server-05"]
  let environments = ["prod", "staging", "dev"]
  let regions = ["us-east", "us-west", "eu-central", "ap-southeast"]
  
  for i in 0..<count {
    let timestamp = base_time + i * 8640 // 每8.64秒一个数据点
    
    let data_point = TelemetryDataPoint::new(timestamp)
    
    // 分类属性
    TelemetryDataPoint::add_attribute(data_point, "service", services[i % services.length()])
    TelemetryDataPoint::add_attribute(data_point, "instance", instances[i % instances.length()])
    TelemetryDataPoint::add_attribute(data_point, "environment", environments[i % environments.length()])
    TelemetryDataPoint::add_attribute(data_point, "region", regions[i % regions.length()])
    
    // 数值属性
    TelemetryDataPoint::add_attribute(data_point, "cpu_usage", 20.0 + Math::random() * 80.0)
    TelemetryDataPoint::add_attribute(data_point, "memory_usage", 30.0 + Math::random() * 70.0)
    TelemetryDataPoint::add_attribute(data_point, "request_count", 100 + Math::random() * 900.0)
    TelemetryDataPoint::add_attribute(data_point, "error_count", Math::random() * 20.0)
    TelemetryDataPoint::add_attribute(data_point, "response_time", 10.0 + Math::random() * 200.0)
    
    // 地理空间属性
    let lat = 40.0 + Math::random() * 10.0
    let lon = -75.0 + Math::random() * 10.0
    TelemetryDataPoint::add_attribute(data_point, "latitude", lat)
    TelemetryDataPoint::add_attribute(data_point, "longitude", lon)
    
    data = data.push(data_point)
  }
  
  data
}

// 辅助函数：创建测试遥测索引
fn create_test_telemetry_index() -> MultidimensionalIndex {
  let index_config = IndexConfiguration::new()
  IndexConfig::set_max_memory_mb(index_config, 512)
  
  let telemetry_index = MultidimensionalIndex::new("telemetry_data", index_config)
  
  // 添加维度定义
  Index::add_dimension(telemetry_index, Dimension::new("service", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("instance", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("environment", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("region", DimensionType::Categorical))
  Index::add_dimension(telemetry_index, Dimension::new("cpu_usage", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("memory_usage", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("request_count", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("error_count", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("response_time", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("timestamp", DimensionType::Temporal))
  Index::add_dimension(telemetry_index, Dimension::new("latitude", DimensionType::Numeric))
  Index::add_dimension(telemetry_index, Dimension::new("longitude", DimensionType::Numeric))
  
  // 构建索引
  let test_data = generate_multidimensional_test_data(10000)
  let index_builder = IndexBuilder::new(telemetry_index)
  
  for data_point in test_data {
    IndexBuilder::add_data_point(index_builder, data_point)
  }
  IndexBuilder::finalize(index_builder)
  
  telemetry_index
}

// 辅助函数：生成基准查询
fn generate_benchmark_queries(count : Int) -> Array[MultidimensionalQuery] {
  let queries = []
  
  for i in 0..<count {
    let query = MultidimensionalQuery::new()
    
    // 随机添加过滤器
    match i % 5 {
      0 => Query::add_filter(query, Filter::equals("service", ["web", "api", "database"][i % 3]))
      1 => Query::add_filter(query, Filter::range("cpu_usage", 30.0 + i.to_float() * 5.0, 80.0))
      2 => Query::add_filter(query, Filter::in_set("environment", ["prod", "staging"]))
      3 => Query::add_filter(query, Filter::time_range("timestamp", Time::now() - 3600000, Time::now()))
      4 => {
        Query::add_filter(query, Filter::equals("service", "web"))
        Query::add_filter(query, Filter::range("memory_usage", 50.0, 90.0))
      }
      _ => ()
    }
    
    queries = queries.push(query)
  }
  
  queries
}

// 辅助函数：生成参考遥测点
fn generate_reference_telemetry_point() -> TelemetryDataPoint {
  let point = TelemetryDataPoint::new(Time::now())
  
  TelemetryDataPoint::add_attribute(point, "service", "web")
  TelemetryDataPoint::add_attribute(point, "instance", "server-01")
  TelemetryDataPoint::add_attribute(point, "environment", "prod")
  TelemetryDataPoint::add_attribute(point, "cpu_usage", 60.0)
  TelemetryDataPoint::add_attribute(point, "memory_usage", 70.0)
  TelemetryDataPoint::add_attribute(point, "request_count", 500.0)
  TelemetryDataPoint::add_attribute(point, "error_count", 5.0)
  TelemetryDataPoint::add_attribute(point, "response_time", 100.0)
  
  point
}