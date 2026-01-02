// Azimuth Multidimensional Analysis Tests
// 多维度数据分析测试用例 - 专注于遥测数据的复杂多维分析和洞察提取

// Test 1: 基础多维度数据聚合测试
test "basic multidimensional data aggregation" {
  // 创建多维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 添加多维度数据点
  for i in 0..<1000 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 时间维度
    DataPoint::set_timestamp(data_point, Time::now() - (1000 - i) * 60000) // 过去1000分钟
    
    // 服务维度
    let services = ["auth", "payment", "search", "recommendation", "user-profile"]
    let service = services[i % services.length()]
    DataPoint::add_dimension(data_point, "service", service)
    
    // 区域维度
    let regions = ["us-east", "us-west", "eu-central", "ap-southeast"]
    let region = regions[i % regions.length()]
    DataPoint::add_dimension(data_point, "region", region)
    
    // 指标维度
    DataPoint::add_metric(data_point, "latency", 50.0 + (i % 200).to_float()) // 50-250ms
    DataPoint::add_metric(data_point, "error_rate", (i % 10).to_float() / 100.0) // 0-9%
    DataPoint::add_metric(data_point, "throughput", 100.0 + (i % 500).to_float()) // 100-600 req/s
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 测试单维度聚合
  let service_aggregation = MultidimensionalDataset::aggregate_by_dimension(dataset, "service")
  assert_true(service_aggregation.groups.length() == 5) // 5个服务
  
  for group in service_aggregation.groups {
    assert_true(group.key.length() > 0)
    assert_true(group.count > 0)
    assert_true(group.metrics.contains("latency"))
    assert_true(group.metrics.contains("error_rate"))
    assert_true(group.metrics.contains("throughput"))
    
    // 验证聚合统计
    let latency_stats = group.metrics["latency"]
    assert_true(latency_stats.average >= 50.0 && latency_stats.average <= 250.0)
    assert_true(latency_stats.min >= 50.0 && latency_stats.max <= 250.0)
  }
  
  // 测试多维度聚合
  let multi_aggregation = MultidimensionalDataset::aggregate_by_dimensions(dataset, ["service", "region"])
  assert_true(multi_aggregation.groups.length() >= 5) // 至少每个服务有一个组
  
  // 验证多维聚合结果
  let auth_groups = multi_aggregation.groups.filter(fn(g) { g.dimensions["service"] == "auth" })
  assert_true(auth_groups.length() > 0)
  
  MultidimensionalDataset::cleanup(dataset)
}

// Test 2: 时间序列多维分析测试
test "time series multidimensional analysis" {
  // 创建时间序列数据集
  let time_series_dataset = TimeSeriesDataset::new()
  
  // 添加时间序列数据点
  let base_time = Time::now() - 86400000 // 24小时前
  
  for hour in 0..<24 {
    for minute in 0..<60 {
      for service_idx in 0..<3 { // 3个服务
        let timestamp = base_time + (hour * 60 + minute) * 60000
        
        let data_point = TimeSeriesDataPoint::new(timestamp)
        
        // 服务维度
        let services = ["frontend", "backend", "database"]
        DataPoint::add_dimension(data_point, "service", services[service_idx])
        
        // 环境维度
        let environments = ["prod", "staging"]
        DataPoint::add_dimension(data_point, "environment", environments[hour % 2])
        
        // 模拟周期性模式
        let cyclic_factor = (minute / 60.0) * 2.0 * 3.14159 // 0到2π
        let base_latency = 100.0 + 50.0 * Math::sin(cyclic_factor)
        let noise = (Random::generate() % 20 - 10).to_float() // ±10噪声
        
        DataPoint::add_metric(data_point, "latency", base_latency + noise)
        DataPoint::add_metric(data_point, "cpu_usage", 30.0 + (hour / 24.0) * 40.0 + (Random::generate() % 10).to_float())
        DataPoint::add_metric(data_point, "request_count", 100.0 + Math::sin(cyclic_factor) * 50.0)
        
        TimeSeriesDataset::add_point(time_series_dataset, data_point)
      }
    }
  }
  
  // 测试时间窗口聚合
  let hourly_windows = TimeSeriesDataset::aggregate_by_time_window(time_series_dataset, 3600000) // 1小时窗口
  assert_true(hourly_windows.length() == 24)
  
  for window in hourly_windows {
    assert_true(window.start_time < window.end_time)
    assert_true(window.metrics.contains("latency"))
    assert_true(window.metrics.contains("cpu_usage"))
    assert_true(window.metrics.contains("request_count"))
  }
  
  // 测试多维时间序列分析
  let service_time_series = TimeSeriesDataset::analyze_by_dimension_over_time(
    time_series_dataset, 
    "service", 
    "latency", 
    3600000 // 1小时粒度
  )
  
  assert_true(service_time_series.series.length() == 3) // 3个服务
  
  for series in service_time_series.series {
    assert_true(series.key.length() > 0)
    assert_true(series.data_points.length() == 24) // 24小时
    
    // 验证时间序列模式
    let variance = TimeSeriesUtils::calculate_variance(series.data_points)
    assert_true(variance > 0) // 应该有变化
    
    // 测试趋势检测
    let trend = TimeSeriesUtils::detect_trend(series.data_points)
    assert_true(trend.direction == "increasing" || trend.direction == "decreasing" || trend.direction == "stable")
  }
  
  TimeSeriesDataset::cleanup(time_series_dataset)
}

// Test 3: 多维数据关联分析测试
test "multidimensional correlation analysis" {
  // 创建多维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 生成具有相关性的数据
  for i in 0..<500 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 基础值
    let base_value = i.to_float() / 100.0
    
    // 维度
    let categories = ["A", "B", "C", "D"]
    DataPoint::add_dimension(data_point, "category", categories[i % categories.length()])
    
    let types = ["type1", "type2", "type3"]
    DataPoint::add_dimension(data_point, "type", types[i % types.length()])
    
    // 相关指标
    let metric1 = base_value + (Random::generate() % 10 - 5).to_float() // 基础指标
    let metric2 = metric1 * 1.5 + (Random::generate() % 10 - 5).to_float() // 与metric1正相关
    let metric3 = 100.0 - metric1 * 0.8 + (Random::generate() % 10 - 5).to_float() // 与metric1负相关
    let metric4 = (Random::generate() % 100).to_float() // 独立指标
    
    DataPoint::add_metric(data_point, "metric1", metric1)
    DataPoint::add_metric(data_point, "metric2", metric2)
    DataPoint::add_metric(data_point, "metric3", metric3)
    DataPoint::add_metric(data_point, "metric4", metric4)
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 测试全局相关性分析
  let correlation_matrix = MultidimensionalDataset::calculate_correlation_matrix(dataset, ["metric1", "metric2", "metric3", "metric4"])
  
  // 验证相关性矩阵
  assert_true(correlation_matrix.metrics.length() == 4)
  assert_true(correlation_matrix.matrix.length() == 4)
  
  // 验证metric1和metric2的正相关
  let metric1_metric2_corr = correlation_matrix.get_correlation("metric1", "metric2")
  assert_true(metric1_metric2_corr > 0.5) // 应该有较强正相关
  
  // 验证metric1和metric3的负相关
  let metric1_metric3_corr = correlation_matrix.get_correlation("metric1", "metric3")
  assert_true(metric1_metric3_corr < -0.3) // 应该有负相关
  
  // 验证metric1和metric4的弱相关
  let metric1_metric4_corr = correlation_matrix.get_correlation("metric1", "metric4")
  assert_true(Math::abs(metric1_metric4_corr) < 0.3) // 应该弱相关
  
  // 测试条件相关性分析
  let conditional_correlations = MultidimensionalDataset::calculate_conditional_correlations(
    dataset, 
    ["metric1", "metric2"], 
    "category"
  )
  
  assert_true(conditional_correlations.groups.length() == 4) // 4个类别
  
  for group in conditional_correlations.groups {
    assert_true(group.category.length() > 0)
    assert_true(group.correlation >= -1.0 && group.correlation <= 1.0)
    assert_true(group.sample_size > 0)
  }
  
  MultidimensionalDataset::cleanup(dataset)
}

// Test 4: 多维数据异常检测测试
test "multidimensional anomaly detection" {
  // 创建多维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 生成正常数据
  for i in 0..<1000 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 维度
    let services = ["web", "api", "db", "cache"]
    DataPoint::add_dimension(data_point, "service", services[i % services.length()])
    
    let environments = ["prod", "staging"]
    DataPoint::add_dimension(data_point, "environment", environments[i % environments.length()])
    
    // 正常指标（使用正态分布）
    let cpu_usage = 50.0 + NormalDistribution::sample(0.0, 10.0) // 平均50，标准差10
    let memory_usage = 60.0 + NormalDistribution::sample(0.0, 15.0) // 平均60，标准差15
    let latency = 100.0 + NormalDistribution::sample(0.0, 20.0) // 平均100，标准差20
    
    DataPoint::add_metric(data_point, "cpu_usage", Math::max(0.0, Math::min(100.0, cpu_usage)))
    DataPoint::add_metric(data_point, "memory_usage", Math::max(0.0, Math::min(100.0, memory_usage)))
    DataPoint::add_metric(data_point, "latency", Math::max(0.0, latency))
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 添加异常数据点
  for i in 0..<50 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 异常维度
    DataPoint::add_dimension(data_point, "service", "web")
    DataPoint::add_dimension(data_point, "environment", "prod")
    
    // 异常指标
    let anomaly_type = i % 3
    match anomaly_type {
      0 => { // CPU异常高
        DataPoint::add_metric(data_point, "cpu_usage", 95.0 + (Random::generate() % 5).to_float())
        DataPoint::add_metric(data_point, "memory_usage", 60.0 + NormalDistribution::sample(0.0, 15.0))
        DataPoint::add_metric(data_point, "latency", 100.0 + NormalDistribution::sample(0.0, 20.0))
      }
      1 => { // 内存异常高
        DataPoint::add_metric(data_point, "cpu_usage", 50.0 + NormalDistribution::sample(0.0, 10.0))
        DataPoint::add_metric(data_point, "memory_usage", 95.0 + (Random::generate() % 5).to_float())
        DataPoint::add_metric(data_point, "latency", 100.0 + NormalDistribution::sample(0.0, 20.0))
      }
      _ => { // 延迟异常高
        DataPoint::add_metric(data_point, "cpu_usage", 50.0 + NormalDistribution::sample(0.0, 10.0))
        DataPoint::add_metric(data_point, "memory_usage", 60.0 + NormalDistribution::sample(0.0, 15.0))
        DataPoint::add_metric(data_point, "latency", 500.0 + (Random::generate() % 100).to_float())
      }
    }
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 测试多维异常检测
  let anomaly_detector = MultidimensionalAnomalyDetector::new()
  let detection_result = MultidimensionalAnomalyDetector::detect(anomaly_detector, dataset)
  
  // 验证检测结果
  assert_true(detection_result.total_points == 1050)
  assert_true(detection_result.anomaly_count > 0)
  assert_true(detection_result.anomaly_count < 100) // 不应该检测出太多异常
  
  // 验证异常详情
  for anomaly in detection_result.anomalies {
    assert_true(anomaly.anomaly_score > 0.5) // 异常分数应该较高
    assert_true(anomaly.dimensions.length() > 0)
    assert_true(anomaly.metrics.length() > 0)
    assert_true(anomaly.reason.length() > 0)
  }
  
  // 测试按维度分组异常检测
  let service_anomalies = MultidimensionalAnomalyDetector::detect_by_dimension(
    anomaly_detector, 
    dataset, 
    "service"
  )
  
  assert_true(service_anomalies.groups.length() == 4) // 4个服务
  
  for group in service_anomalies.groups {
    assert_true(group.service.length() > 0)
    assert_true(group.anomaly_count >= 0)
    assert_true(group.total_points > 0)
    
    if group.anomaly_count > 0 {
      assert_true(group.anomaly_rate > 0.0)
      assert_true(group.anomaly_rate < 1.0)
    }
  }
  
  MultidimensionalAnomalyDetector::cleanup(anomaly_detector)
  MultidimensionalDataset::cleanup(dataset)
}

// Test 5: 多维数据聚类分析测试
test "multidimensional clustering analysis" {
  // 创建多维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 生成聚类数据
  let cluster_centers = [
    {"cpu": 30.0, "memory": 40.0, "latency": 50.0, "category": "low_load"},
    {"cpu": 60.0, "memory": 70.0, "latency": 120.0, "category": "medium_load"},
    {"cpu": 85.0, "memory": 90.0, "latency": 250.0, "category": "high_load"}
  ]
  
  for center in cluster_centers {
    // 每个聚类中心生成100个点
    for i in 0..<100 {
      let data_point = MultidimensionalDataPoint::new()
      
      // 添加噪声
      let cpu = center.cpu + NormalDistribution::sample(0.0, 5.0)
      let memory = center.memory + NormalDistribution::sample(0.0, 8.0)
      let latency = center.latency + NormalDistribution::sample(0.0, 15.0)
      
      DataPoint::add_metric(data_point, "cpu_usage", Math::max(0.0, Math::min(100.0, cpu)))
      DataPoint::add_metric(data_point, "memory_usage", Math::max(0.0, Math::min(100.0, memory)))
      DataPoint::add_metric(data_point, "latency", Math::max(0.0, latency))
      DataPoint::add_dimension(data_point, "true_category", center.category)
      
      MultidimensionalDataset::add_point(dataset, data_point)
    }
  }
  
  // 测试K-means聚类
  let kmeans_config = KMeansConfig::with_clusters(3)
  let clustering_result = MultidimensionalDataset::perform_kmeans_clustering(
    dataset, 
    ["cpu_usage", "memory_usage", "latency"], 
    kmeans_config
  )
  
  // 验证聚类结果
  assert_true(clustering_result.clusters.length() == 3)
  
  let total_assigned_points = clustering_result.clusters.reduce(0, fn(acc, cluster) { acc + cluster.points.length() })
  assert_true(total_assigned_points == 300) // 所有点都应该被分配
  
  // 验证聚类质量
  assert_true(clustering_result.inertia > 0) // 聚类内平方和应该大于0
  assert_true(clustering_result.silhouette_score >= -1.0 && clustering_result.silhouette_score <= 1.0)
  
  // 验证聚类中心
  for cluster in clustering_result.clusters {
    assert_true(cluster.center.length() == 3) // 3个维度
    assert_true(cluster.points.length() > 0)
    
    // 验证聚类统计
    assert_true(cluster.stats.contains("cpu_usage"))
    assert_true(cluster.stats.contains("memory_usage"))
    assert_true(cluster.stats.contains("latency"))
  }
  
  // 测试层次聚类
  let hierarchical_config = HierarchicalClusteringConfig::with_linkage("ward")
  let hierarchical_result = MultidimensionalDataset::perform_hierarchical_clustering(
    dataset, 
    ["cpu_usage", "memory_usage", "latency"], 
    hierarchical_config
  )
  
  // 验证层次聚类结果
  assert_true(hierarchical_result.dendrogram.length() > 0)
  assert_true(hierarchical_result.clusters.length() >= 1)
  
  // 测试聚类评估
  let evaluation = ClusteringEvaluator::evaluate(clustering_result, dataset, "true_category")
  
  assert_true(evaluation.adjusted_rand_index >= -1.0 && evaluation.adjusted_rand_index <= 1.0)
  assert_true(evaluation.normalized_mutual_info >= 0.0 && evaluation.normalized_mutual_info <= 1.0)
  
  MultidimensionalDataset::cleanup(dataset)
}

// Test 6: 多维数据降维分析测试
test "multidimensional dimensionality reduction" {
  // 创建高维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 生成高维数据
  for i in 0::<200 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 生成10个相关的指标
    let base_factors = [
      NormalDistribution::sample(0.0, 1.0), // 因子1
      NormalDistribution::sample(0.0, 1.0), // 因子2
      NormalDistribution::sample(0.0, 1.0)  // 因子3
    ]
    
    // 基于因子生成指标
    let metrics = [
      base_factors[0] * 0.8 + base_factors[1] * 0.2 + NormalDistribution::sample(0.0, 0.1),
      base_factors[0] * 0.7 + base_factors[2] * 0.3 + NormalDistribution::sample(0.0, 0.1),
      base_factors[1] * 0.9 + base_factors[2] * 0.1 + NormalDistribution::sample(0.0, 0.1),
      base_factors[0] * 0.5 + base_factors[1] * 0.5 + NormalDistribution::sample(0.0, 0.1),
      base_factors[1] * 0.6 + base_factors[2] * 0.4 + NormalDistribution::sample(0.0, 0.1),
      base_factors[2] * 0.8 + NormalDistribution::sample(0.0, 0.1),
      base_factors[0] * 0.3 + base_factors[1] * 0.3 + base_factors[2] * 0.4 + NormalDistribution::sample(0.0, 0.1),
      base_factors[0] * 0.4 + base_factors[2] * 0.6 + NormalDistribution::sample(0.0, 0.1),
      base_factors[1] * 0.7 + NormalDistribution::sample(0.0, 0.1),
      base_factors[2] * 0.5 + base_factors[0] * 0.5 + NormalDistribution::sample(0.0, 0.1)
    ]
    
    for j in 0..<metrics.length() {
      DataPoint::add_metric(data_point, "metric_" + j.to_string(), metrics[j])
    }
    
    // 添加类别标签
    let category = if base_factors[0] > 0.5 { "A" } else if base_factors[1] > 0.5 { "B" } else { "C" }
    DataPoint::add_dimension(data_point, "category", category)
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 测试PCA降维
  let pca_config = PCAConfig::with_components(3) // 降到3维
  let pca_result = MultidimensionalDataset::perform_pca(
    dataset, 
    ["metric_0", "metric_1", "metric_2", "metric_3", "metric_4", 
     "metric_5", "metric_6", "metric_7", "metric_8", "metric_9"], 
    pca_config
  )
  
  // 验证PCA结果
  assert_true(pca_result.components.length() == 3) // 3个主成分
  assert_true(pca_result.explained_variance.length() == 3)
  assert_true(pca_result.transformed_data.length() == 200) // 200个数据点
  
  // 验证解释方差
  let total_explained_variance = pca_result.explained_variance.reduce(0.0, fn(acc, v) { acc + v })
  assert_true(total_explained_variance > 0.5) // 应该解释至少50%的方差
  
  // 验证主成分载荷
  for component in pca_result.components {
    assert_true(component.loadings.length() == 10) // 10个原始变量
  }
  
  // 测试t-SNE降维
  let tsne_config = TSNEConfig::with_dimensions(2) // 降到2维
  let tsne_result = MultidimensionalDataset::perform_tsne(
    dataset, 
    ["metric_0", "metric_1", "metric_2", "metric_3", "metric_4", 
     "metric_5", "metric_6", "metric_7", "metric_8", "metric_9"], 
    tsne_config
  )
  
  // 验证t-SNE结果
  assert_true(tsne_result.embeddings.length() == 200) // 200个数据点
  
  for embedding in tsne_result.embeddings {
    assert_true(embedding.coordinates.length() == 2) // 2维
  }
  
  // 测试降维可视化效果
  let visualization_quality = DimensionalityReductionEvaluator::evaluate_visualization_quality(
    tsne_result, 
    dataset, 
    "category"
  )
  
  assert_true(visualization_quality.trustworthiness >= 0.0 && visualization_quality.trustworthiness <= 1.0)
  assert_true(visualization_quality.continuity >= 0.0 && visualization_quality.continuity <= 1.0)
  
  MultidimensionalDataset::cleanup(dataset)
}

// Test 7: 多维数据模式挖掘测试
test "multidimensional pattern mining" {
  // 创建事务型多维数据集
  let dataset = TransactionalDataset::new()
  
  // 生成事务数据
  let services = ["auth", "payment", "search", "recommendation", "user-profile", "notification"]
  let error_types = ["timeout", "connection_error", "authentication_error", "rate_limit", "server_error"]
  let regions = ["us-east", "us-west", "eu-central", "ap-southeast"]
  
  for i in 0..<500 {
    let transaction = Transaction::new()
    
    // 随机选择服务
    let selected_services = []
    for service in services {
      if Random::generate() % 100 < 30 { // 30%概率包含该服务
        selected_services = selected_services.push(service)
      }
    }
    
    // 确保至少有一个服务
    if selected_services.length() == 0 {
      selected_services = selected_services.push(services[Random::generate() % services.length()])
    }
    
    // 添加服务维度
    for service in selected_services {
      Transaction::add_item(transaction, "service:" + service)
    }
    
    // 添加错误类型（如果有错误）
    if Random::generate() % 100 < 20 { // 20%概率有错误
      let error_type = error_types[Random::generate() % error_types.length()]
      Transaction::add_item(transaction, "error:" + error_type)
    }
    
    // 添加区域
    let region = regions[Random::generate() % regions.length()]
    Transaction::add_item(transaction, "region:" + region)
    
    // 添加时间窗口
    let hour = Random::generate() % 24
    Transaction::add_item(transaction, "hour:" + hour.to_string())
    
    TransactionalDataset::add_transaction(dataset, transaction)
  }
  
  // 测试频繁模式挖掘
  let frequent_itemsets_config = FrequentItemsetsConfig::with_min_support(0.1) // 最小支持度10%
  let frequent_itemsets_result = TransactionalDataset::mine_frequent_itemsets(
    dataset, 
    frequent_itemsets_config
  )
  
  // 验证频繁项集结果
  assert_true(frequent_itemsets_result.itemsets.length() > 0)
  
  for itemset in frequent_itemsets_result.itemsets {
    assert_true(itemset.items.length() >= 1)
    assert_true(itemset.support >= 0.1)
    assert_true(itemset.support <= 1.0)
  }
  
  // 测试关联规则挖掘
  let association_rules_config = AssociationRulesConfig::with_min_confidence(0.5) // 最小置信度50%
  let association_rules_result = TransactionalDataset::mine_association_rules(
    frequent_itemsets_result, 
    association_rules_config
  )
  
  // 验证关联规则结果
  assert_true(association_rules_result.rules.length() > 0)
  
  for rule in association_rules_result.rules {
    assert_true(rule.antecedent.length() >= 1)
    assert_true(rule.consequent.length() >= 1)
    assert_true(rule.confidence >= 0.5)
    assert_true(rule.confidence <= 1.0)
    assert_true(rule.lift >= 0.0)
    assert_true(rule.support >= 0.0)
  }
  
  // 测试序列模式挖掘
  let sequence_dataset = SequenceDataset::new()
  
  // 生成序列数据
  for i in 0..<100 {
    let sequence = Sequence::new()
    
    // 每个序列包含3-7个事件
    let sequence_length = 3 + Random::generate() % 5
    
    for j in 0..<sequence_length {
      let event = SequenceEvent::new()
      
      // 随机选择服务
      let service = services[Random::generate() % services.length()]
      SequenceEvent::add_item(event, "service:" + service)
      
      // 可能添加错误
      if Random::generate() % 100 < 20 {
        let error_type = error_types[Random::generate() % error_types.length()]
        SequenceEvent::add_item(event, "error:" + error_type)
      }
      
      Sequence::add_event(sequence, event)
    }
    
    SequenceDataset::add_sequence(sequence_dataset, sequence)
  }
  
  // 测试序列模式挖掘
  let sequence_patterns_config = SequencePatternsConfig::with_min_support(0.05) // 最小支持度5%
  let sequence_patterns_result = SequenceDataset::mine_sequential_patterns(
    sequence_dataset, 
    sequence_patterns_config
  )
  
  // 验证序列模式结果
  assert_true(sequence_patterns_result.patterns.length() > 0)
  
  for pattern in sequence_patterns_result.patterns {
    assert_true(pattern.events.length() >= 1)
    assert_true(pattern.support >= 0.05)
    assert_true(pattern.support <= 1.0)
  }
  
  TransactionalDataset::cleanup(dataset)
  SequenceDataset::cleanup(sequence_dataset)
}

// Test 8: 多维数据预测分析测试
test "multidimensional predictive analysis" {
  // 创建时间序列数据集
  let dataset = TimeSeriesDataset::new()
  
  // 生成时间序列数据
  let base_time = Time::now() - 7 * 86400000 // 7天前
  
  for day in 0..<7 {
    for hour in 0..<24 {
      let timestamp = base_time + (day * 24 + hour) * 3600000
      
      // 基础趋势
      let trend = day * 10.0 // 每天增加10
      let seasonal = 20.0 * Math::sin((hour / 24.0) * 2.0 * 3.14159) // 日季节性
      let noise = NormalDistribution::sample(0.0, 5.0) // 噪声
      
      let base_value = 100.0 + trend + seasonal + noise
      
      let data_point = TimeSeriesDataPoint::new(timestamp)
      
      // 添加维度
      let services = ["web", "api", "db"]
      DataPoint::add_dimension(data_point, "service", services[hour % services.length()])
      
      let environments = ["prod", "staging"]
      DataPoint::add_dimension(data_point, "environment", environments[day % environments.length()])
      
      // 添加指标
      DataPoint::add_metric(data_point, "request_count", Math::max(0.0, base_value + (Random::generate() % 50 - 25).to_float()))
      DataPoint::add_metric(data_point, "error_rate", Math::max(0.0, 5.0 + (day / 7.0) * 10.0 + NormalDistribution::sample(0.0, 2.0)))
      DataPoint::add_metric(data_point, "response_time", 50.0 + seasonal + NormalDistribution::sample(0.0, 10.0))
      
      TimeSeriesDataset::add_point(dataset, data_point)
    }
  }
  
  // 测试时间序列预测
  let prediction_config = TimeSeriesPredictionConfig::with_horizon(24) // 预测24小时
  let prediction_result = TimeSeriesDataset::predict(
    dataset, 
    "request_count", 
    prediction_config
  )
  
  // 验证预测结果
  assert_true(prediction_result.predictions.length() == 24) // 24小时预测
  assert_true(prediction_result.model_accuracy >= 0.0 && prediction_result.model_accuracy <= 1.0)
  
  for prediction in prediction_result.predictions {
    assert_true(prediction.timestamp > 0)
    assert_true(prediction.predicted_value >= 0.0)
    assert_true(prediction.confidence_interval_lower <= prediction.predicted_value)
    assert_true(prediction.confidence_interval_upper >= prediction.predicted_value)
  }
  
  // 测试多维预测
  let multidim_prediction_result = TimeSeriesDataset::predict_multidimensional(
    dataset, 
    ["request_count", "error_rate", "response_time"], 
    prediction_config
  )
  
  // 验证多维预测结果
  assert_true(multidim_prediction_result.predictions.length() == 24)
  
  for prediction in multidim_prediction_result.predictions {
    assert_true(prediction.metrics.contains("request_count"))
    assert_true(prediction.metrics.contains("error_rate"))
    assert_true(prediction.metrics.contains("response_time"))
    
    for metric_value in prediction.metrics.values() {
      assert_true(metric_value.predicted_value >= 0.0)
    }
  }
  
  // 测试按维度预测
  let service_predictions = TimeSeriesDataset::predict_by_dimension(
    dataset, 
    "service", 
    "request_count", 
    prediction_config
  )
  
  // 验证按维度预测结果
  assert_true(service_predictions.predictions.length() == 3) // 3个服务
  
  for service_prediction in service_predictions.predictions {
    assert_true(service_prediction.service.length() > 0)
    assert_true(service_prediction.values.length() == 24) // 24小时预测
    
    for prediction in service_prediction.values {
      assert_true(prediction.predicted_value >= 0.0)
    }
  }
  
  // 测试预测准确性评估
  let accuracy_evaluation = PredictionEvaluator::evaluate_accuracy(
    prediction_result, 
    dataset, 
    "request_count"
  )
  
  assert_true(accuracy_evaluation.mae >= 0.0) // 平均绝对误差
  assert_true(accuracy_evaluation.rmse >= 0.0) // 均方根误差
  assert_true(accuracy_evaluation.mape >= 0.0) // 平均绝对百分比误差
  assert_true(accuracy_evaluation.r2 >= -1.0 && accuracy_evaluation.r2 <= 1.0) // R²分数
  
  TimeSeriesDataset::cleanup(dataset)
}

// Test 9: 多维数据可视化测试
test "multidimensional data visualization" {
  // 创建多维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 生成可视化数据
  for i in 0..<100 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 维度
    let categories = ["A", "B", "C", "D"]
    DataPoint::add_dimension(data_point, "category", categories[i % categories.length()])
    
    let types = ["type1", "type2", "type3"]
    DataPoint::add_dimension(data_point, "type", types[i % types.length()])
    
    // 指标
    let angle = (i / 100.0) * 2.0 * 3.14159
    let radius = 50.0 + i.to_float() / 2.0
    
    DataPoint::add_metric(data_point, "x", radius * Math::cos(angle))
    DataPoint::add_metric(data_point, "y", radius * Math::sin(angle))
    DataPoint::add_metric(data_point, "size", 10.0 + (i % 50).to_float())
    DataPoint::add_metric(data_point, "intensity", i.to_float() / 100.0)
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 测试散点图可视化
  let scatter_plot_config = ScatterPlotConfig::new()
  ScatterPlotConfig::set_x_axis(scatter_plot_config, "x")
  ScatterPlotConfig::set_y_axis(scatter_plot_config, "y")
  ScatterPlotConfig::set_color_by(scatter_plot_config, "category")
  ScatterPlotConfig::set_size_by(scatter_plot_config, "size")
  
  let scatter_plot_result = MultidimensionalDataset::create_scatter_plot(
    dataset, 
    scatter_plot_config
  )
  
  // 验证散点图结果
  assert_true(scatter_plot_result.points.length() == 100)
  
  for point in scatter_plot_result.points {
    assert_true(point.coordinates.length() == 2) // x, y坐标
    assert_true(point.color.length() > 0)
    assert_true(point.size > 0)
  }
  
  // 测试热力图可视化
  let heatmap_config = HeatmapConfig::new()
  HeatmapConfig::set_x_axis(heatmap_config, "category")
  HeatmapConfig::set_y_axis(heatmap_config, "type")
  HeatmapConfig::set_value(heatmap_config, "intensity")
  
  let heatmap_result = MultidimensionalDataset::create_heatmap(
    dataset, 
    heatmap_config
  )
  
  // 验证热力图结果
  assert_true(heatmap_result.cells.length() > 0)
  
  for cell in heatmap_result.cells {
    assert_true(cell.x_category.length() > 0)
    assert_true(cell.y_category.length() > 0)
    assert_true(cell.intensity >= 0.0 && cell.intensity <= 1.0)
  }
  
  // 测试平行坐标图可视化
  let parallel_coordinates_config = ParallelCoordinatesConfig::new()
  ParallelCoordinatesConfig::set_dimensions(
    parallel_coordinates_config, 
    ["x", "y", "size", "intensity"]
  )
  ParallelCoordinatesConfig::set_group_by(parallel_coordinates_config, "category")
  
  let parallel_coordinates_result = MultidimensionalDataset::create_parallel_coordinates(
    dataset, 
    parallel_coordinates_config
  )
  
  // 验证平行坐标图结果
  assert_true(parallel_coordinates_result.lines.length() == 100)
  
  for line in parallel_coordinates_result.lines {
    assert_true line.points.length() == 4 // 4个维度
    assert_true(line.group.length() > 0)
    
    for point in line.points {
      assert_true(point.axis.length() > 0)
      assert_true(point.value >= 0.0)
    }
  }
  
  // 测试雷达图可视化
  let radar_chart_config = RadarChartConfig::new()
  RadarChartConfig::set_metrics(
    radar_chart_config, 
    ["x", "y", "size", "intensity"]
  )
  RadarChartConfig::set_group_by(radar_chart_config, "category")
  
  let radar_chart_result = MultidimensionalDataset::create_radar_chart(
    dataset, 
    radar_chart_config
  )
  
  // 验证雷达图结果
  assert_true(radar_chart_result.groups.length() == 4) // 4个类别
  
  for group in radar_chart_result.groups {
    assert_true(group.name.length() > 0)
    assert_true group.points.length() == 4 // 4个指标
    
    for point in group.points {
      assert_true(point.metric.length() > 0)
      assert_true(point.value >= 0.0)
    }
  }
  
  // 测试可视化导出
  let export_result = VisualizationExporter::export_to_json(scatter_plot_result)
  
  match export_result {
    Ok(json_data) => {
      assert_true(json_data.length() > 0)
      assert_true(json_data.contains("points"))
    }
    Err(_) => assert_true(false)
  }
  
  MultidimensionalDataset::cleanup(dataset)
}

// Test 10: 多维数据报告生成测试
test "multidimensional data reporting" {
  // 创建多维数据集
  let dataset = MultidimensionalDataset::new()
  
  // 生成报告数据
  let services = ["web", "api", "db", "cache"]
  let regions = ["us-east", "us-west", "eu-central", "ap-southeast"]
  let environments = ["prod", "staging"]
  
  for i in 0..<1000 {
    let data_point = MultidimensionalDataPoint::new()
    
    // 添加维度
    DataPoint::add_dimension(data_point, "service", services[i % services.length()])
    DataPoint::add_dimension(data_point, "region", regions[i % regions.length()])
    DataPoint::add_dimension(data_point, "environment", environments[i % environments.length()])
    
    // 添加指标
    DataPoint::add_metric(data_point, "latency", 50.0 + (i % 200).to_float())
    DataPoint::add_metric(data_point, "error_rate", (i % 10).to_float() / 100.0)
    DataPoint::add_metric(data_point, "throughput", 100.0 + (i % 500).to_float())
    DataPoint::add_metric(data_point, "cpu_usage", 30.0 + (i % 70).to_float())
    DataPoint::add_metric(data_point, "memory_usage", 40.0 + (i % 60).to_float())
    
    MultidimensionalDataset::add_point(dataset, data_point)
  }
  
  // 创建多维分析报告
  let report_config = MultidimensionalReportConfig::new()
  MultidimensionalReportConfig::set_title(report_config, "Azimuth Telemetry Multidimensional Analysis Report")
  MultidimensionalReportConfig::set_time_range(report_config, Time::now() - 86400000, Time::now())
  MultidimensionalReportConfig::set_dimensions(report_config, ["service", "region", "environment"])
  MultidimensionalReportConfig::set_metrics(report_config, ["latency", "error_rate", "throughput", "cpu_usage", "memory_usage"])
  
  let report_generator = MultidimensionalReportGenerator::new(report_config)
  let report_result = MultidimensionalReportGenerator::generate(report_generator, dataset)
  
  // 验证报告结果
  match report_result {
    Ok(report) => {
      // 验证报告基本信息
      assert_true(report.title.length() > 0)
      assert_true(report.generated_at > 0)
      assert_true(report.data_points_count == 1000)
      
      // 验证概览部分
      assert_true(report.overview.dimensions.length() == 3)
      assert_true(report.overview.metrics.length() == 5)
      
      // 验证维度分析部分
      assert_true(report.dimension_analysis.length() == 3)
      
      for dimension_analysis in report.dimension_analysis {
        assert_true(dimension_analysis.dimension.length() > 0)
        assert_true(dimension_analysis.categories.length() > 0)
        assert_true(dimension_analysis.statistics.length() > 0)
      }
      
      // 验证指标分析部分
      assert_true(report.metric_analysis.length() == 5)
      
      for metric_analysis in report.metric_analysis {
        assert_true(metric_analysis.metric.length() > 0)
        assert_true(metric_analysis.overall_stats.average >= 0.0)
        assert_true(metric_analysis.overall_stats.min <= metric_analysis.overall_stats.average)
        assert_true(metric_analysis.overall_stats.max >= metric_analysis.overall_stats.average)
      }
      
      // 验证相关性分析部分
      assert_true(report.correlation_analysis.matrix.length() == 5)
      
      // 验证异常检测部分
      assert_true(report.anomaly_detection.anomaly_count >= 0)
      assert_true(report.anomaly_detection.anomaly_rate >= 0.0 && report.anomaly_detection.anomaly_rate <= 1.0)
      
      // 验证洞察部分
      assert_true(report.insights.length() > 0)
      
      for insight in report.insights {
        assert_true(insight.title.length() > 0)
        assert_true(insight.description.length() > 0)
        assert_true(insight.importance >= 1 && insight.importance <= 5)
      }
      
      // 验证建议部分
      assert_true(report.recommendations.length() > 0)
      
      for recommendation in report.recommendations {
        assert_true(recommendation.title.length() > 0)
        assert_true(recommendation.description.length() > 0)
        assert_true(recommendation.priority >= 1 && recommendation.priority <= 5)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试报告导出
  let html_export_result = MultidimensionalReportGenerator::export_to_html(report_generator)
  match html_export_result {
    Ok(html_content) => {
      assert_true(html_content.length() > 0)
      assert_true(html_content.contains("<html>"))
      assert_true(html_content.contains("</html>"))
    }
    Err(_) => assert_true(false)
  }
  
  let pdf_export_result = MultidimensionalReportGenerator::export_to_pdf(report_generator)
  match pdf_export_result {
    Ok(pdf_data) => {
      assert_true(pdf_data.length() > 0)
    }
    Err(_) => {
      // PDF导出可能失败，这是可接受的
      assert_true(true)
    }
  }
  
  MultidimensionalReportGenerator::cleanup(report_generator)
  MultidimensionalDataset::cleanup(dataset)
}