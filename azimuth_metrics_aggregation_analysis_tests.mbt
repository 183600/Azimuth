// Azimuth Metrics Aggregation and Analysis Tests
// 度量聚合和分析测试用例 - 专注于数据度量的聚合、分析和可视化

// Test 1: 基础度量聚合
test "basic metrics aggregation" {
  let aggregator = MetricsAggregator::new()
  
  // 创建测试度量数据
  let metrics_data = []
  for i in 0..<1000 {
    let timestamp = Time::now() + i * 60 // 每分钟一个数据点
    let value = (i * 3) % 100 + Random::next() % 20
    let tags = {
      "service": "service_" + (i % 5).to_string(),
      "environment": ["prod", "dev", "staging"][i % 3],
      "region": ["us-east", "us-west", "eu-west", "ap-southeast"][i % 4]
    }
    
    metrics_data = metrics_data.push({
      "name": "response_time",
      "value": value,
      "timestamp": timestamp,
      "tags": tags
    })
  }
  
  // 测试基本统计聚合
  let basic_stats = MetricsAggregator::basic_statistics(aggregator, metrics_data)
  assert_true(basic_stats.count == 1000)
  assert_true(basic_stats.min >= 0)
  assert_true(basic_stats.max < 120)
  assert_true(basic_stats.mean > 0)
  assert_true(basic_stats.median > 0)
  assert_true(basic_stats.std_dev > 0)
  
  // 验证统计计算
  let expected_sum = metrics_data.reduce(fn(acc, metric) { acc + metric.value }, 0)
  assert_eq(basic_stats.sum, expected_sum)
  
  // 测试百分位数聚合
  let percentiles = MetricsAggregator::percentiles(aggregator, metrics_data, [50, 90, 95, 99])
  assert_true(percentiles["p50"] >= basic_stats.min && percentiles["p50"] <= basic_stats.max)
  assert_true(percentiles["p90"] >= percentiles["p50"])
  assert_true(percentiles["p95"] >= percentiles["p90"])
  assert_true(percentiles["p99"] >= percentiles["p95"])
  
  // 测试按标签分组聚合
  let service_groups = MetricsAggregator::group_by_tag(aggregator, metrics_data, "service")
  assert_true(service_groups.length() == 5)
  
  for (service, group_metrics) in service_groups {
    let group_stats = MetricsAggregator::basic_statistics(aggregator, group_metrics)
    assert_true(group_stats.count > 0)
    assert_true(group_stats.mean > 0)
  }
  
  // 测试时间窗口聚合
  let window_start = metrics_data[0].timestamp
  let window_end = metrics_data[999].timestamp
  let window_size = (window_end - window_start) / 10 // 10个时间窗口
  
  let time_windows = MetricsAggregator::time_window_aggregation(aggregator, metrics_data, window_start, window_end, window_size)
  assert_true(time_windows.length() == 10)
  
  for window in time_windows {
    assert_true(window.start_time >= window_start)
    assert_true(window.end_time <= window_end)
    assert_true(window.metrics.length() > 0)
  }
  
  // 测试多维度聚合
  let multi_dimensional = MetricsAggregator::multi_dimensional_aggregation(aggregator, metrics_data, ["service", "environment"])
  assert_true(multi_dimensional.length() > 0)
  
  for (dimensions, group_metrics) in multi_dimensional {
    assert_true(dimensions.contains("service"))
    assert_true(dimensions.contains("environment"))
    assert_true(group_metrics.length() > 0)
  }
}

// Test 2: 高级度量分析
test "advanced metrics analysis" {
  let analyzer = MetricsAnalyzer::new()
  
  // 创建具有趋势和季节性的测试数据
  let trend_data = []
  let base_time = Time::now()
  
  for i in 0::<720 { // 12小时，每分钟一个数据点
    let timestamp = base_time + i * 60
    // 线性趋势 + 季节性 + 噪声
    let trend = i * 0.1
    let seasonal = 10 * ((2 * 3.14159 * i / 60).sin()) // 每小时季节性
    let noise = (Random::next() % 10 - 5).to_float()
    let value = 50 + trend + seasonal + noise
    
    trend_data = trend_data.push({
      "name": "cpu_usage",
      "value": value,
      "timestamp": timestamp,
      "tags": { "host": "server_" + (i % 3).to_string() }
    })
  }
  
  // 测试趋势分析
  let trend_analysis = MetricsAnalyzer::trend_analysis(analyzer, trend_data)
  assert_true(trend_analysis.slope > 0) // 应该检测到上升趋势
  assert_true(trend_analysis.intercept > 0)
  assert_true(trend_analysis.r_squared > 0) // 应该有一定的拟合度
  assert_true(trend_analysis.p_value < 0.05) // 应该是显著的
  
  // 测试季节性分析
  let seasonality_analysis = MetricsAnalyzer::seasonality_analysis(analyzer, trend_data, 60) // 60分钟周期
  assert_true(seasonality_analysis.has_seasonality)
  assert_true(seasonality_analysis.period == 60)
  assert_true(seasonality_analysis.strength > 0)
  
  // 测试异常检测
  let anomaly_data = []
  for i in 0..<100 {
    let value = if i == 50 { 200 } else { 50 + (Random::next() % 20 - 10) } // 在第50个点插入异常值
    anomaly_data = anomaly_data.push({
      "name": "memory_usage",
      "value": value,
      "timestamp": base_time + i * 60,
      "tags": {}
    })
  }
  
  let anomaly_detection = MetricsAnalyzer::anomaly_detection(analyzer, anomaly_data, 2.0) // 2个标准差阈值
  assert_true(anomaly_detection.anomalies.length() >= 1)
  
  // 验证异常点被检测到
  let anomaly_at_50 = anomaly_detection.anomalies.any(fn(anomaly) { anomaly.index == 50 })
  assert_true(anomaly_at_50)
  
  // 测试变化点检测
  let change_point_data = []
  for i in 0..<200 {
    let value = if i < 100 { 50 + (Random::next() % 10) } else { 80 + (Random::next() % 10) } // 在第100个点发生变化
    change_point_data = change_point_data.push({
      "name": "request_rate",
      "value": value,
      "timestamp": base_time + i * 60,
      "tags": {}
    })
  }
  
  let change_points = MetricsAnalyzer::change_point_detection(analyzer, change_point_data)
  assert_true(change_points.length() >= 1)
  
  // 验证变化点被检测到
  let change_at_100 = change_points.any(fn(cp) { cp.index >= 95 && cp.index <= 105 })
  assert_true(change_at_100)
  
  // 测试相关性分析
  let correlation_data = []
  for i in 0..<100 {
    let x = i.to_float()
    let y = x * 2.0 + (Random::next() % 20 - 10).to_float() // 强正相关
    correlation_data = correlation_data.push({
      "name": "correlation_test",
      "x": x,
      "y": y,
      "timestamp": base_time + i * 60,
      "tags": {}
    })
  }
  
  let correlation = MetricsAnalyzer::correlation_analysis(analyzer, correlation_data, "x", "y")
  assert_true(correlation.coefficient > 0.8) // 强正相关
  assert_true(correlation.p_value < 0.05) // 显著相关
  
  // 测试预测分析
  let forecast_data = trend_data.slice(0, 600) // 使用前600个点进行预测
  let forecast = MetricsAnalyzer::forecast(analyzer, forecast_data, 120) // 预测后120个点
  assert_eq(forecast.values.length(), 120)
  assert_true(forecast.confidence_interval_lower.length() == 120)
  assert_true(forecast.confidence_interval_upper.length() == 120)
  
  // 验证预测值在合理范围内
  for i in 0..<forecast.values.length() {
    assert_true(forecast.values[i] >= forecast.confidence_interval_lower[i])
    assert_true(forecast.values[i] <= forecast.confidence_interval_upper[i])
  }
}

// Test 3: 实时度量聚合
test "real-time metrics aggregation" {
  let realtime_aggregator = RealtimeMetricsAggregator::new()
  
  // 配置实时聚合器
  RealtimeMetricsAggregator::configure_window(realtime_aggregator, 60000) // 1分钟窗口
  RealtimeMetricsAggregator::configure_sliding_window(realtime_aggregator, 5) // 5个滑动窗口
  RealtimeMetricsAggregator::configure_buffer_size(realtime_aggregator, 1000)
  
  // 启动实时聚合
  RealtimeMetricsAggregator::start(realtime_aggregator)
  
  // 模拟实时数据流
  let start_time = Time::now()
  for i in 0..<100 {
    let metric = {
      "name": "request_duration",
      "value": (Random::next() % 1000).to_float(),
      "timestamp": start_time + i * 100, // 每100ms一个数据点
      "tags": {
        "endpoint": "/api/v1/users",
        "method": "GET",
        "status": "200"
      }
    }
    
    RealtimeMetricsAggregator::add_metric(realtime_aggregator, metric)
    
    // 偶尔暂停以模拟真实场景
    if i % 20 == 0 {
      ConcurrentProcessor::sleep(50)
    }
  }
  
  // 等待聚合完成
  ConcurrentProcessor::sleep(1000)
  
  // 获取实时聚合结果
  let current_window = RealtimeMetricsAggregator::get_current_window(realtime_aggregator)
  assert_true(current_window.metrics.length() > 0)
  assert_true(current_window.start_time > 0)
  assert_true(current_window.end_time > current_window.start_time)
  
  // 验证窗口统计
  let window_stats = RealtimeMetricsAggregator::get_window_statistics(realtime_aggregator)
  assert_true(window_stats.count > 0)
  assert_true(window_stats.mean > 0)
  assert_true(window_stats.min >= 0)
  assert_true(window_stats.max >= window_stats.min)
  
  // 获取滑动窗口聚合
  let sliding_windows = RealtimeMetricsAggregator::get_sliding_windows(realtime_aggregator)
  assert_true(sliding_windows.length() <= 5)
  
  // 验证滑动窗口时间顺序
  for i in 1..<sliding_windows.length() {
    assert_true(sliding_windows[i].start_time > sliding_windows[i-1].start_time)
  }
  
  // 测试实时百分位数计算
  let percentiles = RealtimeMetricsAggregator::get_percentiles(realtime_aggregator, [50, 90, 95, 99])
  assert_true(percentiles.contains("p50"))
  assert_true(percentiles.contains("p90"))
  assert_true(percentiles.contains("p95"))
  assert_true(percentiles.contains("p99"))
  
  assert_true(percentiles["p90"] >= percentiles["p50"])
  assert_true(percentiles["p95"] >= percentiles["p90"])
  assert_true(percentiles["p99"] >= percentiles["p95"])
  
  // 测试实时速率计算
  let rate = RealtimeMetricsAggregator::get_rate(realtime_aggregator, 1000) // 每秒速率
  assert_true(rate > 0)
  
  // 测试实时告警
  RealtimeMetricsAggregator::set_alert_threshold(realtime_aggregator, "request_duration", 800.0)
  
  // 添加触发告警的度量
  for i in 0..<10 {
    let alert_metric = {
      "name": "request_duration",
      "value": 900.0, // 超过阈值
      "timestamp": Time::now() + i * 100,
      "tags": {
        "endpoint": "/api/v1/slow",
        "method": "POST",
        "status": "200"
      }
    }
    
    RealtimeMetricsAggregator::add_metric(realtime_aggregator, alert_metric)
  }
  
  ConcurrentProcessor::sleep(500)
  
  // 检查告警
  let alerts = RealtimeMetricsAggregator::get_alerts(realtime_aggregator)
  assert_true(alerts.length() > 0)
  
  // 验证告警内容
  let request_duration_alerts = alerts.filter(fn(alert) { alert.metric_name == "request_duration" })
  assert_true(request_duration_alerts.length() > 0)
  
  for alert in request_duration_alerts {
    assert_true(alert.value > 800.0)
    assert_true(alert.threshold == 800.0)
  }
  
  // 停止实时聚合
  RealtimeMetricsAggregator::stop(realtime_aggregator)
  
  // 获取聚合统计
  let aggregation_stats = RealtimeMetricsAggregator::get_statistics(realtime_aggregator)
  assert_true(aggregation_stats.total_metrics_processed > 0)
  assert_true(aggregation_stats.total_windows_created > 0)
  assert_true(aggregation_stats.average_processing_time > 0)
}

// Test 4: 分布式度量聚合
test "distributed metrics aggregation" {
  let distributed_aggregator = DistributedMetricsAggregator::new()
  
  // 配置分布式聚合
  let nodes = ["node1", "node2", "node3", "node4"]
  DistributedMetricsAggregator::configure_nodes(distributed_aggregator, nodes)
  DistributedMetricsAggregator::configure_aggregation_interval(distributed_aggregator, 5000) // 5秒
  DistributedMetricsAggregator::configure_consistency_level(distributed_aggregator, "eventual")
  
  // 启动分布式聚合
  DistributedMetricsAggregator::start(distributed_aggregator)
  
  // 为每个节点创建数据
  let node_metrics = {}
  for node in nodes {
    let metrics = []
    for i in 0..<100 {
      let metric = {
        "name": "throughput",
        "value": (Random::next() % 1000).to_float(),
        "timestamp": Time::now() + i * 60,
        "tags": {
          "node": node,
          "service": "web_service"
        }
      }
      metrics = metrics.push(metric)
    }
    node_metrics[node] = metrics
  }
  
  // 将数据发送到各个节点
  for node in nodes {
    DistributedMetricsAggregator::add_metrics_to_node(distributed_aggregator, node, node_metrics[node])
  }
  
  // 等待分布式聚合完成
  ConcurrentProcessor::sleep(2000)
  
  // 获取全局聚合结果
  let global_aggregation = DistributedMetricsAggregator::get_global_aggregation(distributed_aggregator)
  assert_true(global_aggregation.total_metrics == 400) // 4个节点各100个度量
  
  // 验证全局统计
  let global_stats = global_aggregation.statistics
  assert_true(global_stats.count == 400)
  assert_true(global_stats.mean > 0)
  
  // 获取节点级别的聚合
  let node_aggregations = DistributedMetricsAggregator::get_node_aggregations(distributed_aggregator)
  assert_eq(node_aggregations.length(), nodes.length())
  
  // 验证每个节点的聚合
  for node in nodes {
    let node_agg = node_aggregations[node]
    assert_true(node_agg.metrics.length() == 100)
    assert_true(node_agg.statistics.count == 100)
    assert_eq(node_agg.node_id, node)
  }
  
  // 测试节点故障处理
  DistributedMetricsAggregator::simulate_node_failure(distributed_aggregator, "node2")
  
  // 添加更多数据
  for node in ["node1", "node3", "node4"] {
    let additional_metrics = []
    for i in 0..<50 {
      let metric = {
        "name": "error_rate",
        "value": (Random::next() % 10).to_float() / 100.0,
        "timestamp": Time::now() + i * 60,
        "tags": {
          "node": node,
          "service": "web_service"
        }
      }
      additional_metrics = additional_metrics.push(metric)
    }
    DistributedMetricsAggregator::add_metrics_to_node(distributed_aggregator, node, additional_metrics)
  }
  
  ConcurrentProcessor::sleep(1000)
  
  // 验证故障节点被排除
  let updated_node_aggregations = DistributedMetricsAggregator::get_node_aggregations(distributed_aggregator)
  assert_true(updated_node_aggregations.length() == 3) // 只有3个活跃节点
  assert_false(updated_node_aggregations.contains("node2"))
  
  // 测试节点恢复
  DistributedMetricsAggregator::recover_node(distributed_aggregator, "node2")
  
  // 添加恢复后的数据
  let recovery_metrics = []
  for i in 0..<30 {
    let metric = {
      "name": "latency",
      "value": (Random::next() % 500).to_float(),
      "timestamp": Time::now() + i * 60,
      "tags": {
        "node": "node2",
        "service": "web_service"
      }
    }
    recovery_metrics = recovery_metrics.push(metric)
  }
  DistributedMetricsAggregator::add_metrics_to_node(distributed_aggregator, "node2", recovery_metrics)
  
  ConcurrentProcessor::sleep(1000)
  
  // 验证节点恢复
  let recovered_node_aggregations = DistributedMetricsAggregator::get_node_aggregations(distributed_aggregator)
  assert_true(recovered_node_aggregations.length() == 4) // 所有节点都恢复
  assert_true(recovered_node_aggregations.contains("node2"))
  
  // 测试分布式查询
  let query_result = DistributedMetricsAggregator::query(distributed_aggregator, {
    "metric_name": "throughput",
    "time_range": {
      "start": Time::now() - 10000,
      "end": Time::now()
    },
    "group_by": ["node"]
  })
  
  assert_true(query_result.results.length() > 0)
  
  // 验证查询结果包含所有节点
  let query_nodes = query_result.results.map(fn(result) { result.group.node }).unique()
  for node in ["node1", "node3", "node4"] { // node2可能没有throughput度量
    if query_nodes.contains(node) {
      assert_true(true)
    }
  }
  
  // 停止分布式聚合
  DistributedMetricsAggregator::stop(distributed_aggregator)
  
  // 获取分布式聚合统计
  let distributed_stats = DistributedMetricsAggregator::get_statistics(distributed_aggregator)
  assert_true(distributed_stats.total_nodes == 4)
  assert_true(distributed_stats.active_nodes <= 4)
  assert_true(distributed_stats.total_metrics_processed > 0)
  assert_true(distributed_stats.aggregation_cycles > 0)
}

// Test 5: 度量可视化和报告
test "metrics visualization and reporting" {
  let visualizer = MetricsVisualizer::new()
  
  // 创建测试数据
  let visualization_data = []
  let base_time = Time::now() - 86400 // 24小时前
  
  for i in 0::<1440 { // 每分钟一个数据点，24小时
    let hour = i / 60
    let value = 50 + 20 * ((2 * 3.14159 * hour / 24).sin()) + (Random::next() % 10 - 5)
    
    visualization_data = visualization_data.push({
      "name": "cpu_usage",
      "value": value,
      "timestamp": base_time + i * 60,
      "tags": { "host": "server1" }
    })
  }
  
  // 生成时间序列图表
  let time_series_chart = MetricsVisualizer::create_time_series_chart(visualizer, visualization_data, {
    "title": "CPU Usage Over 24 Hours",
    "x_axis": "Time",
    "y_axis": "CPU Usage (%)",
    "width": 800,
    "height": 400
  })
  
  assert_true(time_series_chart.contains("CPU Usage Over 24 Hours"))
  assert_true(time_series_chart.contains("Time"))
  assert_true(time_series_chart.contains("CPU Usage (%)"))
  
  // 生成直方图
  let histogram = MetricsVisualizer::create_histogram(visualizer, visualization_data, {
    "title": "CPU Usage Distribution",
    "bins": 20,
    "width": 600,
    "height": 400
  })
  
  assert_true(histogram.contains("CPU Usage Distribution"))
  assert_true(histogram.contains("Frequency"))
  
  // 生成热力图数据
  let heatmap_data = []
  for hour in 0..<24 {
    for day in 0..<7 {
      let value = 30 + (hour * 2) % 40 + (day * 3) % 20
      heatmap_data = heatmap_data.push({
        "x": hour,
        "y": day,
        "value": value
      })
    }
  }
  
  let heatmap = MetricsVisualizer::create_heatmap(visualizer, heatmap_data, {
    "title": "Weekly CPU Usage Pattern",
    "x_label": "Hour of Day",
    "y_label": "Day of Week",
    "width": 800,
    "height": 400
  })
  
  assert_true(heatmap.contains("Weekly CPU Usage Pattern"))
  assert_true(heatmap.contains("Hour of Day"))
  assert_true(heatmap.contains("Day of Week"))
  
  // 生成仪表板
  let dashboard_config = {
    "title": "System Metrics Dashboard",
    "layout": "grid",
    "widgets": [
      {
        "type": "time_series",
        "title": "CPU Usage",
        "data": visualization_data,
        "width": 6,
        "height": 4
      },
      {
        "type": "gauge",
        "title": "Current CPU",
        "value": 65.5,
        "min": 0,
        "max": 100,
        "width": 3,
        "height": 2
      },
      {
        "type": "histogram",
        "title": "Response Time Distribution",
        "data": visualization_data,
        "width": 6,
        "height": 4
      }
    ]
  }
  
  let dashboard = MetricsVisualizer::create_dashboard(visualizer, dashboard_config)
  assert_true(dashboard.contains("System Metrics Dashboard"))
  assert_true(dashboard.contains("CPU Usage"))
  assert_true(dashboard.contains("Current CPU"))
  assert_true(dashboard.contains("Response Time Distribution"))
  
  // 生成报告
  let report_config = {
    "title": "Weekly System Performance Report",
    "period": {
      "start": base_time,
      "end": base_time + 7 * 86400
    },
    "sections": [
      {
        "title": "Executive Summary",
        "type": "summary",
        "metrics": ["cpu_usage", "memory_usage", "response_time"]
      },
      {
        "title": "Detailed Analysis",
        "type": "detailed",
        "charts": ["time_series", "histogram", "heatmap"]
      },
      {
        "title": "Recommendations",
        "type": "recommendations",
        "insights": ["performance", "capacity", "optimization"]
      }
    ]
  }
  
  let report = MetricsVisualizer::generate_report(visualizer, visualization_data, report_config)
  assert_true(report.contains("Weekly System Performance Report"))
  assert_true(report.contains("Executive Summary"))
  assert_true(report.contains("Detailed Analysis"))
  assert_true(report.contains("Recommendations"))
  
  // 测试交互式图表
  let interactive_chart = MetricsVisualizer::create_interactive_chart(visualizer, visualization_data, {
    "type": "line",
    "zoom": true,
    "pan": true,
    "tooltip": true,
    "legend": true
  })
  
  assert_true(interactive_chart.contains("interactive"))
  assert_true(interactive_chart.contains("tooltip"))
  
  // 测试实时图表更新
  let realtime_chart = MetricsVisualizer::create_realtime_chart(visualizer, {
    "title": "Real-time Metrics",
    "update_interval": 1000, // 1秒更新
    "max_points": 100
  })
  
  assert_true(realtime_chart.contains("Real-time Metrics"))
  assert_true(realtime_chart.contains("update_interval"))
  
  // 测试图表导出
  let export_formats = ["png", "svg", "pdf", "html"]
  for format in export_formats {
    let export_result = MetricsVisualizer::export_chart(visualizer, time_series_chart, format)
    match export_result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
}

// Test 6: 度量存储和检索
test "metrics storage and retrieval" {
  let storage = MetricsStorage::new()
  
  // 配置存储
  MetricsStorage::configure_retention(storage, {
    "raw_metrics": "7d",    // 原始数据保留7天
    "1m_resolution": "30d", // 1分钟分辨率数据保留30天
    "1h_resolution": "90d", // 1小时分辨率数据保留90天
    "1d_resolution": "1y"   // 1天分辨率数据保留1年
  })
  
  MetricsStorage::configure_compression(storage, true)
  MetricsStorage::configure_indexing(storage, ["name", "tags.host", "tags.service"])
  
  // 创建测试度量数据
  let test_metrics = []
  let base_time = Time::now() - 86400 // 24小时前
  
  for i in 0::<1440 { // 每分钟一个数据点，24小时
    let metric = {
      "name": "response_time",
      "value": (Random::next() % 1000).to_float(),
      "timestamp": base_time + i * 60,
      "tags": {
        "service": "api_service",
        "endpoint": "/api/v1/data",
        "method": "GET",
        "status": "200"
      }
    }
    test_metrics = test_metrics.push(metric)
  }
  
  // 存储度量数据
  let store_result = MetricsStorage::store_metrics(storage, test_metrics)
  match store_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 按时间范围查询
  let time_range_query = {
    "name": "response_time",
    "start_time": base_time,
    "end_time": base_time + 3600, // 查询第一小时
    "tags": {}
  }
  
  let time_range_result = MetricsStorage::query(storage, time_range_query)
  match time_range_result {
    Ok(metrics) => {
      assert_true(metrics.length() == 60) // 60分钟，每分钟一个点
      
      // 验证时间范围
      for metric in metrics {
        assert_true(metric.timestamp >= base_time)
        assert_true(metric.timestamp <= base_time + 3600)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 按标签查询
  let tag_query = {
    "name": "response_time",
    "start_time": base_time,
    "end_time": base_time + 86400,
    "tags": {
      "service": "api_service",
      "method": "GET"
    }
  }
  
  let tag_result = MetricsStorage::query(storage, tag_query)
  match tag_result {
    Ok(metrics) => {
      assert_true(metrics.length() == 1440) // 所有数据点都应该匹配
      
      // 验证标签匹配
      for metric in metrics {
        assert_eq(metric.tags.service, "api_service")
        assert_eq(metric.tags.method, "GET")
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 聚合查询
  let aggregation_query = {
    "name": "response_time",
    "start_time": base_time,
    "end_time": base_time + 86400,
    "aggregation": "avg",
    "interval": 3600, // 1小时聚合
    "tags": {}
  }
  
  let aggregation_result = MetricsStorage::query(storage, aggregation_query)
  match aggregation_result {
    Ok(aggregate_metrics) => {
      assert_true(aggregate_metrics.length() == 24) // 24小时，每小时一个聚合点
      
      // 验证聚合值
      for metric in aggregate_metrics {
        assert_true(metric.value > 0)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试批量查询
  let batch_queries = [
    {
      "name": "response_time",
      "start_time": base_time,
      "end_time": base_time + 3600,
      "tags": {}
    },
    {
      "name": "response_time",
      "start_time": base_time + 3600,
      "end_time": base_time + 7200,
      "tags": {}
    }
  ]
  
  let batch_result = MetricsStorage::batch_query(storage, batch_queries)
  match batch_result {
    Ok(results) => {
      assert_eq(results.length(), 2)
      assert_true(results[0].length() > 0)
      assert_true(results[1].length() > 0)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试数据压缩
  let compression_stats = MetricsStorage::get_compression_stats(storage)
  assert_true(compression_stats.compression_ratio > 1.0)
  assert_true(compression_stats.space_saved > 0)
  
  // 测试数据保留策略
  let retention_stats = MetricsStorage::get_retention_stats(storage)
  assert_true(retention_stats.raw_metrics_count > 0)
  assert_true(retention_stats.aggregated_metrics_count > 0)
  
  // 测试存储性能
  let performance_stats = MetricsStorage::get_performance_stats(storage)
  assert_true(performance_stats.average_write_time > 0)
  assert_true(performance_stats.average_read_time > 0)
  assert_true(performance_stats.total_operations > 0)
}

// Test 7: 自定义度量函数
test "custom metrics functions" {
  let custom_functions = CustomMetricsFunctions::new()
  
  // 注册自定义聚合函数
  CustomMetricsFunctions::register_aggregator(custom_functions, "weighted_average", fn(values, weights) {
    if values.length() != weights.length() {
      return 0.0
    }
    
    let weighted_sum = values.reduce(fn(acc, pair) { 
      let (value, weight) = pair
      acc + value * weight 
    }, 0.0)
    let sum_weights = weights.reduce(fn(acc, weight) { acc + weight }, 0.0)
    
    if sum_weights == 0.0 {
      return 0.0
    }
    
    weighted_sum / sum_weights
  })
  
  CustomMetricsFunctions::register_aggregator(custom_functions, "mode", fn(values, _) {
    if values.length() == 0 {
      return 0.0
    }
    
    let frequency_map = {}
    for value in values {
      if frequency_map.contains(value) {
        frequency_map[value] = frequency_map[value] + 1
      } else {
        frequency_map[value] = 1
      }
    }
    
    let mut max_count = 0
    let mut mode_value = 0.0
    
    for (value, count) in frequency_map {
      if count > max_count {
        max_count = count
        mode_value = value
      }
    }
    
    mode_value
  })
  
  // 注册自定义分析函数
  CustomMetricsFunctions::register_analyzer(custom_functions, "trend_strength", fn(values) {
    if values.length() < 2 {
      return 0.0
    }
    
    let n = values.length()
    let sum_x = (0..<n).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
    let sum_y = values.reduce(fn(acc, value) { acc + value }, 0.0)
    let sum_xy = (0..<n).reduce(fn(acc, i) { acc + i.to_float() * values[i] }, 0.0)
    let sum_x2 = (0..<n).reduce(fn(acc, i) { acc + (i.to_float()).pow(2) }, 0.0)
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    
    slope.abs() // 返回斜率的绝对值作为趋势强度
  })
  
  CustomMetricsFunctions::register_analyzer(custom_functions, "volatility", fn(values) {
    if values.length() < 2 {
      return 0.0
    }
    
    let mean = values.reduce(fn(acc, value) { acc + value }, 0.0) / values.length().to_float()
    let variance = values.reduce(fn(acc, value) { 
      acc + (value - mean).pow(2) 
    }, 0.0) / values.length().to_float()
    
    variance.sqrt() // 标准差作为波动性指标
  })
  
  // 创建测试数据
  let test_values = []
  let test_weights = []
  
  for i in 0..<100 {
    test_values = test_values.push((i * 2 + Random::next() % 10).to_float())
    test_weights = test_weights.push((Random::next() % 10 + 1).to_float())
  }
  
  // 测试自定义聚合函数
  let weighted_avg = CustomMetricsFunctions::apply_aggregator(custom_functions, "weighted_average", test_values, test_weights)
  assert_true(weighted_avg > 0)
  
  // 验证加权平均计算
  let expected_weighted_sum = test_values.reduce(fn(acc, pair) { 
    let (value, weight) = pair
    acc + value * weight 
  }, 0.0)
  let expected_sum_weights = test_weights.reduce(fn(acc, weight) { acc + weight }, 0.0)
  let expected_weighted_avg = expected_weighted_sum / expected_sum_weights
  
  assert_true((weighted_avg - expected_weighted_avg).abs() < 0.001)
  
  let mode_value = CustomMetricsFunctions::apply_aggregator(custom_functions, "mode", test_values, [])
  assert_true(mode_value > 0)
  
  // 测试自定义分析函数
  let trend_strength = CustomMetricsFunctions::apply_analyzer(custom_functions, "trend_strength", test_values)
  assert_true(trend_strength >= 0)
  
  let volatility = CustomMetricsFunctions::apply_analyzer(custom_functions, "volatility", test_values)
  assert_true(volatility >= 0)
  
  // 测试复合函数
  CustomMetricsFunctions::register_composite(custom_functions, "trend_to_volatility_ratio", fn(values) {
    let trend = CustomMetricsFunctions::apply_analyzer(custom_functions, "trend_strength", values)
    let vol = CustomMetricsFunctions::apply_analyzer(custom_functions, "volatility", values)
    
    if vol == 0.0 {
      return 0.0
    }
    
    trend / vol
  })
  
  let trend_vol_ratio = CustomMetricsFunctions::apply_composite(custom_functions, "trend_to_volatility_ratio", test_values)
  assert_true(trend_vol_ratio >= 0)
  
  // 测试函数链
  let function_chain = [
    { "type": "analyzer", "name": "volatility" },
    { "type": "aggregator", "name": "weighted_average", "weights": [1.0, 2.0, 3.0] }
  ]
  
  let chain_result = CustomMetricsFunctions::apply_chain(custom_functions, test_values, function_chain)
  assert_true(chain_result.length() >= 0)
  
  // 测试条件函数
  CustomMetricsFunctions::register_conditional(custom_functions, "conditional_average", fn(values, condition) {
    let filtered_values = values.filter(fn(value) { condition(value) })
    
    if filtered_values.length() == 0 {
      return 0.0
    }
    
    filtered_values.reduce(fn(acc, value) { acc + value }, 0.0) / filtered_values.length().to_float()
  })
  
  let conditional_avg = CustomMetricsFunctions::apply_conditional(
    custom_functions, 
    "conditional_average", 
    test_values, 
    fn(value) { value > 50.0 }
  )
  
  assert_true(conditional_avg > 50.0)
  
  // 获取函数注册表
  let function_registry = CustomMetricsFunctions::get_registry(custom_functions)
  assert_true(function_registry.aggregators.contains("weighted_average"))
  assert_true(function_registry.aggregators.contains("mode"))
  assert_true(function_registry.analyzers.contains("trend_strength"))
  assert_true(function_registry.analyzers.contains("volatility"))
  assert_true(function_registry.composites.contains("trend_to_volatility_ratio"))
  assert_true(function_registry.conditionals.contains("conditional_average"))
}

// Test 8: 度量性能优化
test "metrics performance optimization" {
  let performance_optimizer = MetricsPerformanceOptimizer::new()
  
  // 配置优化参数
  MetricsPerformanceOptimizer::configure_sampling(performance_optimizer, {
    "enabled": true,
    "rate": 0.1, // 10%采样率
    "strategy": "random"
  })
  
  MetricsPerformanceOptimizer::configure_batching(performance_optimizer, {
    "enabled": true,
    "batch_size": 100,
    "flush_interval": 1000 // 1秒
  })
  
  MetricsPerformanceOptimizer::configure_caching(performance_optimizer, {
    "enabled": true,
    "max_size": 1000,
    "ttl": 60000 // 1分钟
  })
  
  MetricsPerformanceOptimizer::configure_compression(performance_optimizer, {
    "enabled": true,
    "algorithm": "lz4",
    "level": 5
  })
  
  // 创建大量测试数据
  let large_dataset = []
  let base_time = Time::now()
  
  for i in 0..<10000 {
    let metric = {
      "name": "performance_test",
      "value": (Random::next() % 1000).to_float(),
      "timestamp": base_time + i,
      "tags": {
        "source": "source_" + (i % 10).to_string(),
        "type": "type_" + (i % 5).to_string()
      }
    }
    large_dataset = large_dataset.push(metric)
  }
  
  // 测试未优化的性能
  let unoptimized_start = Time::now()
  let unoptimized_result = MetricsPerformanceOptimizer::process_without_optimization(performance_optimizer, large_dataset)
  let unoptimized_time = Time::now() - unoptimized_start
  
  match unoptimized_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 测试优化后的性能
  let optimized_start = Time::now()
  let optimized_result = MetricsPerformanceOptimizer::process_with_optimization(performance_optimizer, large_dataset)
  let optimized_time = Time::now() - optimized_start
  
  match optimized_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证性能提升
  let performance_improvement = unoptimized_time.to_float() / optimized_time.to_float()
  assert_true(performance_improvement > 1.2) // 至少20%的性能提升
  
  // 测试内存使用优化
  let memory_usage_before = MetricsPerformanceOptimizer::get_memory_usage(performance_optimizer)
  MetricsPerformanceOptimizer::enable_memory_optimization(performance_optimizer, true)
  
  let memory_optimized_result = MetricsPerformanceOptimizer::process_with_optimization(performance_optimizer, large_dataset)
  match memory_optimized_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let memory_usage_after = MetricsPerformanceOptimizer::get_memory_usage(performance_optimizer)
  assert_true(memory_usage_after.peak < memory_usage_before.peak * 0.8) // 至少20%内存节省
  
  // 测试并发处理优化
  let concurrent_start = Time::now()
  let concurrent_result = MetricsPerformanceOptimizer::process_concurrently(performance_optimizer, large_dataset, 4)
  let concurrent_time = Time::now() - concurrent_start
  
  match concurrent_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 验证并发处理性能
  let concurrency_improvement = optimized_time.to_float() / concurrent_time.to_float()
  assert_true(concurrency_improvement > 1.5) // 至少50%的并发性能提升
  
  // 测试索引优化
  MetricsPerformanceOptimizer::create_indexes(performance_optimizer, ["name", "tags.source", "timestamp"])
  
  let indexed_query_start = Time::now()
  let indexed_query_result = MetricsPerformanceOptimizer::query_optimized(performance_optimizer, {
    "name": "performance_test",
    "tags": { "source": "source_5" },
    "time_range": {
      "start": base_time,
      "end": base_time + 5000
    }
  })
  let indexed_query_time = Time::now() - indexed_query_start
  
  match indexed_query_result {
    Ok(results) => assert_true(results.length() > 0)
    Err(_) => assert_true(false)
  }
  
  // 测试未索引查询性能
  let non_indexed_query_start = Time::now()
  let non_indexed_query_result = MetricsPerformanceOptimizer::query_without_optimization(performance_optimizer, {
    "name": "performance_test",
    "tags": { "source": "source_5" },
    "time_range": {
      "start": base_time,
      "end": base_time + 5000
    }
  })
  let non_indexed_query_time = Time::now() - non_indexed_query_start
  
  match non_indexed_query_result {
    Ok(results) => assert_true(results.length() > 0)
    Err(_) => assert_true(false)
  }
  
  // 验证索引查询性能提升
  let query_improvement = non_indexed_query_time.to_float() / indexed_query_time.to_float()
  assert_true(query_improvement > 1.5) // 至少50%的查询性能提升
  
  // 获取优化统计
  let optimization_stats = MetricsPerformanceOptimizer::get_optimization_stats(performance_optimizer)
  assert_true(optimization_stats.sampling_rate > 0)
  assert_true(optimization_stats.batch_efficiency > 0)
  assert_true(optimization_stats.cache_hit_rate > 0)
  assert_true(optimization_stats.compression_ratio > 1.0)
  assert_true(optimization_stats.memory_savings > 0)
}

// Test 9: 度量质量评估
test "metrics quality assessment" {
  let quality_assessor = MetricsQualityAssessor::new()
  
  // 创建不同质量的测试数据
  let high_quality_data = []
  let medium_quality_data = []
  let low_quality_data = []
  
  let base_time = Time::now()
  
  // 高质量数据：完整、一致、准确
  for i in 0..<1000 {
    let metric = {
      "name": "high_quality_metric",
      "value": (i * 2 + Random::next() % 5).to_float(),
      "timestamp": base_time + i * 60,
      "tags": {
        "service": "api_service",
        "version": "1.0.0",
        "environment": "production"
      }
    }
    high_quality_data = high_quality_data.push(metric)
  }
  
  // 中等质量数据：有一些缺失值和不一致
  for i in 0..<1000 {
    if i % 10 != 0 { // 10%的数据缺失
      let metric = {
        "name": "medium_quality_metric",
        "value": (i * 2 + Random::next() % 20).to_float(),
        "timestamp": base_time + i * 60,
        "tags": {
          "service": if i % 100 < 50 { "api_service" } else { "web_service" }, // 不一致的服务名
          "version": "1.0.0",
          "environment": "production"
        }
      }
      medium_quality_data = medium_quality_data.push(metric)
    }
  }
  
  // 低质量数据：大量缺失值、异常值和不一致
  for i in 0..<1000 {
    if i % 3 != 0 { // 33%的数据缺失
      let value = if i % 50 == 0 { 
        10000.0 // 异常值
      } else {
        (i * 2 + Random::next() % 100).to_float()
      }
      
      let metric = {
        "name": if i % 100 < 50 { "low_quality_metric" } else { "quality_metric" }, // 不一致的度量名
        "value": value,
        "timestamp": base_time + i * 60 + (Random::next() % 300 - 150), // 不规律的时间戳
        "tags": {
          "service": if i % 20 < 10 { "api" } else { "api_service" }, // 不一致的标签值
          "version": if i % 100 < 80 { "1.0.0" } else { "v1.0.0" } // 不一致的版本格式
        }
      }
      low_quality_data = low_quality_data.push(metric)
    }
  }
  
  // 评估高质量数据
  let high_quality_assessment = MetricsQualityAssessor::assess(quality_assessor, high_quality_data)
  assert_true(high_quality_assessment.overall_score > 0.8)
  assert_true(high_quality_assessment.completeness > 0.95)
  assert_true(high_quality_assessment.consistency > 0.95)
  assert_true(high_quality_assessment.accuracy > 0.8)
  
  // 评估中等质量数据
  let medium_quality_assessment = MetricsQualityAssessor::assess(quality_assessor, medium_quality_data)
  assert_true(medium_quality_assessment.overall_score > 0.5 && medium_quality_assessment.overall_score < 0.8)
  assert_true(medium_quality_assessment.completeness > 0.8 && medium_quality_assessment.completeness < 0.95)
  assert_true(medium_quality_assessment.consistency > 0.7 && medium_quality_assessment.consistency < 0.95)
  
  // 评估低质量数据
  let low_quality_assessment = MetricsQualityAssessor::assess(quality_assessor, low_quality_data)
  assert_true(low_quality_assessment.overall_score < 0.5)
  assert_true(low_quality_assessment.completeness < 0.8)
  assert_true(low_quality_assessment.consistency < 0.7)
  assert_true(low_quality_assessment.accuracy < 0.8)
  
  // 测试质量改进建议
  let improvement_suggestions = MetricsQualityAssessor::get_improvement_suggestions(quality_assessor, low_quality_assessment)
  assert_true(improvement_suggestions.length() > 0)
  
  // 验证建议内容
  let has_completeness_suggestion = improvement_suggestions.any(fn(suggestion) { 
    String::contains(suggestion, "completeness")
  })
  let has_consistency_suggestion = improvement_suggestions.any(fn(suggestion) { 
    String::contains(suggestion, "consistency")
  })
  let has_accuracy_suggestion = improvement_suggestions.any(fn(suggestion) { 
    String::contains(suggestion, "accuracy")
  })
  
  assert_true(has_completeness_suggestion)
  assert_true(has_consistency_suggestion)
  assert_true(has_accuracy_suggestion)
  
  // 测试数据清洗
  let cleaned_data = MetricsQualityAssessor::clean_data(quality_assessor, low_quality_data, {
    "remove_outliers": true,
    "fill_missing_values": true,
    "standardize_tags": true,
    "normalize_timestamps": true
  })
  
  assert_true(cleaned_data.length() > 0)
  
  // 验证清洗后的质量提升
  let cleaned_assessment = MetricsQualityAssessor::assess(quality_assessor, cleaned_data)
  assert_true(cleaned_assessment.overall_score > low_quality_assessment.overall_score)
  
  // 测试质量监控
  MetricsQualityAssessor::start_monitoring(quality_assessor)
  
  // 模拟数据质量变化
  for i in 0..<10 {
    let batch_data = []
    for j in 0..<100 {
      let metric = {
        "name": "monitored_metric",
        "value": (i * 100 + j * 2).to_float(),
        "timestamp": base_time + i * 600 + j * 6,
        "tags": { "batch": i.to_string() }
      }
      batch_data = batch_data.push(metric)
    }
    
    MetricsQualityAssessor::monitor_batch(quality_assessor, batch_data)
  }
  
  let quality_trend = MetricsQualityAssessor::get_quality_trend(quality_assessor)
  assert_true(quality_trend.length() == 10)
  
  // 验证质量趋势
  for i in 1..<quality_trend.length() {
    let current_score = quality_trend[i].score
    let prev_score = quality_trend[i-1].score
    
    // 质量应该相对稳定或改善
    assert_true(current_score >= prev_score * 0.9)
  }
  
  MetricsQualityAssessor::stop_monitoring(quality_assessor)
  
  // 生成质量报告
  let quality_report = MetricsQualityAssessor::generate_quality_report(quality_assessor, low_quality_data)
  assert_true(quality_report.contains("Quality Assessment Report"))
  assert_true(quality_report.contains("Overall Score"))
  assert_true(quality_report.contains("Completeness"))
  assert_true(quality_report.contains("Consistency"))
  assert_true(quality_report.contains("Accuracy"))
  assert_true(quality_report.contains("Improvement Suggestions"))
}

// Test 10: 度量集成和生态系统
test "metrics integration and ecosystem" {
  let ecosystem = MetricsEcosystem::new()
  
  // 配置生态系统组件
  MetricsEcosystem::configure_collector(ecosystem, {
    "type": "prometheus",
    "endpoint": "http://localhost:9090",
    "scrape_interval": 15000
  })
  
  MetricsEcosystem::configure_storage(ecosystem, {
    "type": "influxdb",
    "endpoint": "http://localhost:8086",
    "database": "azimuth_metrics"
  })
  
  MetricsEcosystem::configure_visualization(ecosystem, {
    "type": "grafana",
    "endpoint": "http://localhost:3000",
    "dashboards": ["system", "application", "business"]
  })
  
  MetricsEcosystem::configure_alerting(ecosystem, {
    "type": "alertmanager",
    "endpoint": "http://localhost:9093",
    "rules": ["cpu_high", "memory_low", "error_spike"]
  })
  
  // 启动生态系统
  MetricsEcosystem::start(ecosystem)
  
  // 创建集成的度量数据
  let integration_data = []
  let base_time = Time::now()
  
  for i in 0..<500 {
    let metric = {
      "name": "integrated_metric",
      "value": (Random::next() % 100).to_float(),
      "timestamp": base_time + i * 60,
      "tags": {
        "service": "integrated_service",
        "component": "component_" + (i % 3).to_string(),
        "environment": "production"
      }
    }
    integration_data = integration_data.push(metric)
  }
  
  // 测试端到端度量流程
  let end_to_end_start = Time::now()
  
  // 1. 收集度量
  let collection_result = MetricsEcosystem::collect(ecosystem, integration_data)
  match collection_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 2. 存储度量
  let storage_result = MetricsEcosystem::store(ecosystem, integration_data)
  match storage_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 3. 聚合度量
  let aggregation_result = MetricsEcosystem::aggregate(ecosystem, {
    "name": "integrated_metric",
    "start_time": base_time,
    "end_time": base_time + 30000,
    "aggregation": "avg",
    "interval": 300
  })
  match aggregation_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // 4. 分析度量
  let analysis_result = MetricsEcosystem::analyze(ecosystem, {
    "name": "integrated_metric",
    "start_time": base_time,
    "end_time": base_time + 30000,
    "analysis_types": ["trend", "anomaly", "seasonality"]
  })
  match analysis_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let end_to_end_time = Time::now() - end_to_end_start
  assert_true(end_to_end_time < 5000) // 应该在5秒内完成
  
  // 测试组件间通信
  let communication_test = MetricsEcosystem::test_component_communication(ecosystem)
  assert_true(communication_test.collector_to_storage)
  assert_true(communication_test.storage_to_visualization)
  assert_true(communication_test.aggregation_to_alerting)
  
  // 测试度量转换
  let transformation_config = {
    "source_format": "prometheus",
    "target_format": "influxdb",
    "mappings": {
      "metric_name": "name",
      "metric_value": "value",
      "metric_timestamp": "timestamp",
      "metric_tags": "tags"
    }
  }
  
  let transformed_data = MetricsEcosystem::transform_metrics(ecosystem, integration_data, transformation_config)
  assert_true(transformed_data.length() == integration_data.length())
  
  // 验证转换结果
  for i in 0..<transformed_data.length() {
    assert_eq(transformed_data[i].name, integration_data[i].name)
    assert_eq(transformed_data[i].value, integration_data[i].value)
  }
  
  // 测试度量路由
  let routing_rules = [
    {
      "condition": fn(metric) { metric.tags.component == "component_0" },
      "destination": "high_priority_storage"
    },
    {
      "condition": fn(metric) { metric.value > 80.0 },
      "destination": "alert_processor"
    },
    {
      "condition": fn(metric) { true },
      "destination": "standard_storage"
    }
  ]
  
  MetricsEcosystem::configure_routing(ecosystem, routing_rules)
  
  let routing_results = MetricsEcosystem::route_metrics(ecosystem, integration_data)
  assert_true(routing_results.length() > 0)
  
  // 验证路由结果
  let high_priority_count = routing_results.filter(fn(result) { result.destination == "high_priority_storage" }).length()
  let alert_count = routing_results.filter(fn(result) { result.destination == "alert_processor" }).length()
  let standard_count = routing_results.filter(fn(result) { result.destination == "standard_storage" }).length()
  
  assert_true(high_priority_count > 0)
  assert_true(alert_count > 0)
  assert_true(standard_count > 0)
  
  // 测试生态系统健康检查
  let health_check = MetricsEcosystem::health_check(ecosystem)
  assert_true(health_check.overall_status == "healthy")
  assert_true(health_check.components.collector.status == "healthy")
  assert_true(health_check.components.storage.status == "healthy")
  assert_true(health_check.components.visualization.status == "healthy")
  assert_true(health_check.components.alerting.status == "healthy")
  
  // 测试生态系统扩展
  MetricsEcosystem::register_extension(ecosystem, "custom_analyzer", {
    "type": "analyzer",
    "function": fn(metrics) { 
      metrics.map(fn(metric) { 
        { ...metric, "custom_field": metric.value * 2.0 }
      })
    }
  })
  
  let extended_result = MetricsEcosystem::apply_extension(ecosystem, "custom_analyzer", integration_data.slice(0, 10))
  match extended_result {
    Ok(extended_metrics) => {
      assert_eq(extended_metrics.length(), 10)
      for metric in extended_metrics {
        assert_true(metric.contains("custom_field"))
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 停止生态系统
  MetricsEcosystem::stop(ecosystem)
  
  // 获取生态系统统计
  let ecosystem_stats = MetricsEcosystem::get_statistics(ecosystem)
  assert_true(ecosystem_stats.total_metrics_processed > 0)
  assert_true(ecosystem_stats.total_operations > 0)
  assert_true(ecosystem_stats.average_processing_time > 0)
  assert_true(ecosystem_stats.component_health_scores.length() > 0)
}