// Azimuth Multidimensional Analysis Tests
// 测试多维度数据分析、聚合和查询功能

test "多维数据结构操作" {
  // 测试多维数据结构操作
  let data_cube = DataCube::new({
    "dimensions": ["time", "service", "region", "operation"],
    "measures": ["duration", "error_rate", "throughput", "memory_usage"]
  })
  
  // 添加多维数据点
  let base_timestamp = 1735689600000000000L
  let services = ["auth-service", "payment-service", "user-service", "order-service"]
  let regions = ["us-west-2", "us-east-1", "eu-west-1", "ap-southeast-1"]
  let operations = ["login", "payment", "search", "checkout", "register"]
  
  for i in 0..=99 {
    let data_point = {
      "coordinates": {
        "time": base_timestamp + (i * 3600000000L),  // 每小时一个点
        "service": services[i % services.length()],
        "region": regions[i % regions.length()],
        "operation": operations[i % operations.length()]
      },
      "measures": {
        "duration": 50.0 + (i % 100).to_float() * 2.5,
        "error_rate": if i % 20 == 0 { 0.05 } else { 0.01 },
        "throughput": 100.0 + (i % 50).to_float() * 10.0,
        "memory_usage": 200.0 + (i % 30).to_float() * 15.0
      }
    }
    DataCube::add_point(data_cube, data_point)
  }
  
  // 验证数据立方体
  assert_eq(DataCube::size(data_cube), 100)
  assert_eq(DataCube::dimension_count(data_cube), 4)
  assert_eq(DataCube::measure_count(data_cube), 4)
  
  // 测试切片操作
  let time_slice = DataCube::slice(data_cube, "time", base_timestamp + 3600000000L * 5)
  assert_eq(time_slice.size(), 1)
  assert_eq(time_slice.dimensions.length(), 3)  // 减少了一个维度
  
  let service_slice = DataCube::slice(data_cube, "service", "auth-service")
  assert_eq(service_slice.size(), 25)  // 100个点，4个服务，每个服务约25个点
  
  // 测试切块操作
  let region_operation_chunk = DataCube::chunk(
    data_cube,
    {
      "region": ["us-west-2", "us-east-1"],
      "operation": ["login", "payment"]
    }
  )
  assert_true(region_operation_chunk.size() > 0)
  assert_true(region_operation_chunk.size() < 100)
  
  // 测试旋转操作
  let rotated_cube = DataCube::rotate(data_cube, "time", "service")
  assert_eq(rotated_cube.dimensions.length(), 4)
  assert_true(rotated_cube.contains_dimension("time"))
  assert_true(rotated_cube.contains_dimension("service"))
}

test "多维聚合分析" {
  // 测试多维聚合分析
  let aggregator = MultiDimensionalAggregator::new()
  
  // 创建测试数据
  let multi_dim_data = []
  let categories = ["A", "B", "C", "D"]
  let types = ["X", "Y", "Z"]
  
  for i in 0..=199 {
    let record = {
      "category": categories[i % categories.length()],
      "type": types[i % types.length()],
      "group": (i % 5).to_string(),
      "value1": (i * 1.5) + 10.0,
      "value2": (i * 0.8) + 5.0,
      "value3": (i * 2.2) + 15.0,
      "timestamp": 1735689600000000000L + (i * 60000000L)  // 每分钟一个点
    }
    multi_dim_data = multi_dim_data.push(record)
  }
  
  // 测试单维聚合
  let category_agg = MultiDimensionalAggregator::aggregate_by(
    aggregator,
    multi_dim_data,
    ["category"],
    {
      "value1": "average",
      "value2": "sum",
      "value3": "max"
    }
  )
  
  assert_eq(category_agg.length(), 4)  // 4个类别
  assert_true(category_agg.all(fn(row) { row.contains("category") }))
  assert_true(category_agg.all(fn(row) { row.contains("value1_avg") }))
  assert_true(category_agg.all(fn(row) { row.contains("value2_sum") }))
  assert_true(category_agg.all(fn(row) { row.contains("value3_max") }))
  
  // 测试多维聚合
  let multi_agg = MultiDimensionalAggregator::aggregate_by(
    aggregator,
    multi_dim_data,
    ["category", "type"],
    {
      "value1": "average",
      "value2": "sum",
      "value3": "count"
    }
  )
  
  assert_eq(multi_agg.length(), 12)  // 4个类别 × 3个类型
  
  // 验证聚合结果
  let category_a_total = multi_agg
    .filter(fn(row) { row["category"] == "A" })
    .map(fn(row) { row["value3_count"] })
    .reduce(fn(acc, count) { acc + count }, 0)
  
  assert_eq(category_a_total, 50)  // 200个记录，4个类别，每个类别50个
  
  // 测试时间窗口聚合
  let time_windows = MultiDimensionalAggregator::aggregate_by_time_window(
    aggregator,
    multi_dim_data,
    "timestamp",
    3600000000L,  // 1小时窗口
    ["category"],
    {
      "value1": "average",
      "value2": "sum"
    }
  )
  
  assert_true(time_windows.length() > 0)
  assert_true(time_windows.all(fn(row) { row.contains("time_window") }))
  assert_true(time_windows.all(fn(row) { row.contains("category") }))
}

test "OLAP操作和查询" {
  // 测试OLAP操作和查询
  let olap_engine = OLAPEngine::new({
    "max_memory": 100 * 1024 * 1024,  // 100MB
    "cache_size": 50 * 1024 * 1024,   // 50MB缓存
    "parallel_workers": 4
  })
  
  // 创建OLAP数据立方体
  let sales_cube = OLAPEngine::create_cube(olap_engine, "sales", {
    "dimensions": [
      {"name": "time", "type": "temporal", "granularity": "day"},
      {"name": "product", "type": "categorical"},
      {"name": "region", "type": "categorical"},
      {"name": "channel", "type": "categorical"}
    ],
    "measures": [
      {"name": "revenue", "type": "numeric", "aggregation": "sum"},
      {"name": "quantity", "type": "numeric", "aggregation": "sum"},
      {"name": "profit", "type": "numeric", "aggregation": "sum"},
      {"name": "margin", "type": "numeric", "aggregation": "average"}
    ]
  })
  
  // 填充销售数据
  let products = ["laptop", "phone", "tablet", "watch", "headphones"]
  let regions = ["north", "south", "east", "west", "central"]
  let channels = ["online", "retail", "b2b", "marketplace"]
  
  for i in 0..=999 {
    let sale_record = {
      "time": 1735689600000000000L + (i * 86400000000L),  // 每天
      "product": products[i % products.length()],
      "region": regions[i % regions.length()],
      "channel": channels[i % channels.length()],
      "revenue": 100.0 + (i % 500).to_float() * 10.0,
      "quantity": 1 + (i % 10),
      "profit": 20.0 + (i % 200).to_float() * 5.0,
      "margin": 0.15 + (i % 20).to_float() * 0.01
    }
    OLAPEngine::add_data(olap_engine, "sales", sale_record)
  }
  
  // 测试钻取操作
  let yearly_sales = OLAPEngine::drill_down(
    olap_engine,
    "sales",
    {"time": "year"},
    {"revenue": "sum", "quantity": "sum"}
  )
  assert_eq(yearly_sales.length(), 1)  // 所有数据都在同一年
  
  let monthly_sales = OLAPEngine::drill_down(
    olap_engine,
    "sales",
    {"time": "month"},
    {"revenue": "sum", "profit": "sum"}
  )
  assert_true(monthly_sales.length() > 1)
  
  let daily_sales = OLAPEngine::drill_down(
    olap_engine,
    "sales",
    {"time": "day", "product": "product"},
    {"revenue": "sum", "quantity": "sum", "margin": "average"}
  )
  assert_true(daily_sales.length() > monthly_sales.length())
  
  // 测试上卷操作
  let regional_monthly = OLAPEngine::roll_up(
    olap_engine,
    "sales",
    {"region": "region", "time": "month"},
    {"revenue": "sum", "quantity": "sum", "margin": "average"}
  )
  assert_eq(regional_monthly.length(), regions.length())
  
  // 测试切片和切块
  let product_slice = OLAPEngine::slice(
    olap_engine,
    "sales",
    {"product": "laptop"}
  )
  assert_true(product_slice.size() > 0)
  
  let multi_slice = OLAPEngine::slice(
    olap_engine,
    "sales",
    {"product": "laptop", "channel": "online"}
  )
  assert_true(multi_slice.size() < product_slice.size())
  
  let region_channel_chunk = OLAPEngine::chunk(
    olap_engine,
    "sales",
    {"region": ["north", "south"], "channel": ["online", "retail"]}
  )
  assert_true(region_channel_chunk.size() > 0)
  assert_true(region_channel_chunk.size() < 1000)
  
  // 测试旋转
  let rotated_cube = OLAPEngine::pivot(
    olap_engine,
    "sales",
    "product",
    "region",
    "revenue",
    "sum"
  )
  assert_true(rotated_cube.rows.length() > 0)
  assert_true(rotated_cube.columns.length() > 0)
  
  // 测试复杂OLAP查询
  let complex_query_result = OLAPEngine::execute_query(
    olap_engine,
    """
    SELECT 
      region, 
      channel, 
      SUM(revenue) as total_revenue,
      AVG(margin) as avg_margin,
      COUNT(*) as transaction_count
    FROM sales
    WHERE time >= '2025-01-01' AND time <= '2025-01-31'
      AND product IN ('laptop', 'phone')
    GROUP BY region, channel
    HAVING total_revenue > 10000
    ORDER BY total_revenue DESC
    """
  )
  
  assert_true(complex_query_result.length() > 0)
  assert_true(complex_query_result.all(fn(row) {
    row.contains("region") && row.contains("channel") && 
    row.contains("total_revenue") && row.contains("avg_margin") &&
    row.contains("transaction_count")
  }))
}

test "多维数据可视化" {
  // 测试多维数据可视化
  let visualizer = MultiDimensionalVisualizer::new()
  
  // 创建测试数据集
  let visualization_data = []
  let dimensions = ["dim1", "dim2", "dim3"]
  let metrics = ["metric1", "metric2", "metric3"]
  
  for i in 0..=149 {
    let data_point = {
      "dim1": "A" + ((i % 5) + 1).to_string(),
      "dim2": "B" + ((i % 3) + 1).to_string(),
      "dim3": "C" + ((i % 4) + 1).to_string(),
      "metric1": (i * 1.2) + 20.0,
      "metric2": (i * 0.8) + 15.0,
      "metric3": (i * 1.5) + 25.0,
      "timestamp": 1735689600000000000L + (i * 3600000000L)
    }
    visualization_data = visualization_data.push(data_point)
  }
  
  // 测试热力图生成
  let heatmap_data = MultiDimensionalVisualizer::generate_heatmap(
    visualizer,
    visualization_data,
    "dim1",
    "dim2",
    "metric1",
    "average"
  )
  
  assert_true(heatmap_data.data_matrix.length() > 0)
  assert_true(heatmap_data.x_axis_labels.length() > 0)
  assert_true(heatmap_data.y_axis_labels.length() > 0)
  assert_true(heatmap_data.color_scale.min < heatmap_data.color_scale.max)
  
  // 测试平行坐标图
  let parallel_coords = MultiDimensionalVisualizer::generate_parallel_coordinates(
    visualizer,
    visualization_data,
    ["dim1", "dim2", "dim3"],
    ["metric1", "metric2", "metric3"]
  )
  
  assert_eq(parallel_coords.axes.length(), 6)  // 3个维度 + 3个指标
  assert_true(parallel_coords.data_lines.length() > 0)
  assert_true(parallel_coords.data_lines.all(fn(line) { line.points.length() == 6 }))
  
  // 测试散点图矩阵
  let scatter_matrix = MultiDimensionalVisualizer::generate_scatter_matrix(
    visualizer,
    visualization_data,
    ["metric1", "metric2", "metric3"],
    "dim1"
  )
  
  assert_eq(scatter_matrix.plots.length(), 9)  // 3x3矩阵
  assert_true(scatter_matrix.plots.all(fn(plot) {
    plot.x_axis != "" && plot.y_axis != "" && plot.points.length() > 0
  }))
  
  // 测试3D散点图
  let scatter_3d = MultiDimensionalVisualizer::generate_3d_scatter(
    visualizer,
    visualization_data,
    "metric1",
    "metric2",
    "metric3",
    "dim2"
  )
  
  assert_true(scatter_3d.points.length() > 0)
  assert_true(scatter_3d.points.all(fn(point) {
    point.x != null && point.y != null && point.z != null && point.category != ""
  }))
  assert_true(scatter_3d.color_legend.length() > 0)
  
  // 测试雷达图
  let radar_chart = MultiDimensionalVisualizer::generate_radar_chart(
    visualizer,
    visualization_data,
    ["metric1", "metric2", "metric3"],
    "dim1"
  )
  
  assert_true(radar_chart.axes.length() == 3)
  assert_true(radar_chart.series.length() > 0)
  assert_true(radar_chart.series.all(fn(series) {
    series.values.length() == 3 && series.name != ""
  }))
  
  // 测试树状图
  let treemap = MultiDimensionalVisualizer::generate_treemap(
    visualizer,
    visualization_data,
    ["dim1", "dim2"],
    "metric1",
    "sum"
  )
  
  assert_true(treemap.rectangles.length() > 0)
  assert_true(treemap.rectangles.all(fn(rect) {
    rect.x >= 0 && rect.y >= 0 && rect.width > 0 && rect.height > 0 &&
    rect.label != "" && rect.value >= 0
  }))
  
  // 测试维度相关性分析
  let correlation_analysis = MultiDimensionalVisualizer::analyze_correlations(
    visualizer,
    visualization_data,
    ["metric1", "metric2", "metric3"]
  )
  
  assert_eq(correlation_analysis.matrix.length(), 3)
  assert_eq(correlation_analysis.matrix[0].length(), 3)
  assert_true(correlation_analysis.matrix[0][0] == 1.0)  // 自相关
  assert_true(correlation_analysis.strong_correlations.length() >= 0)
}

test "实时多维分析" {
  // 测试实时多维分析
  let realtime_analyzer = RealTimeMultiDimensionalAnalyzer::new({
    "window_size": 60000000000L,  // 60秒窗口
    "slide_interval": 10000000000L,  // 10秒滑动
    "max_dimensions": 5,
    "aggregation_interval": 5000000000L  // 5秒聚合间隔
  })
  
  // 启动实时分析
  RealTimeMultiDimensionalAnalyzer::start(realtime_analyzer)
  
  // 模拟实时数据流
  let data_stream = RealTimeStream::new()
  let stream_dimensions = ["service", "operation", "status"]
  let stream_metrics = ["latency", "error_count", "throughput"]
  
  // 启动数据生产者
  let producer = StreamProducer::start(data_stream, fn() {
    let i = random_int(0, 1000)
    let record = {
      "timestamp": get_current_timestamp(),
      "service": ["auth", "payment", "user", "order"][i % 4],
      "operation": ["login", "checkout", "search", "register"][i % 4],
      "status": if i % 10 == 0 { "error" } else { "success" },
      "latency": 50.0 + (i % 200).to_float(),
      "error_count": if i % 10 == 0 { 1 } else { 0 },
      "throughput": 100.0 + (i % 50).to_float()
    }
    record
  })
  
  // 注册实时分析查询
  let query_id = RealTimeMultiDimensionalAnalyzer::register_query(
    realtime_analyzer,
    {
      "name": "service_performance",
      "dimensions": ["service", "status"],
      "metrics": ["latency": "average", "error_count": "sum", "throughput": "average"],
      "filters": {},
      "window": "60s",
      "group_by": ["service", "status"]
    }
  )
  
  // 等待数据积累
  sleep(15000)  // 15秒
  
  // 获取实时分析结果
  let query_results = RealTimeMultiDimensionalAnalyzer::get_results(
    realtime_analyzer,
    query_id
  )
  
  assert_true(query_results.length() > 0)
  assert_true(query_results.all(fn(result) {
    result.contains("service") && result.contains("status") &&
    result.contains("latency_avg") && result.contains("error_count_sum") &&
    result.contains("throughput_avg")
  }))
  
  // 测试动态维度添加
  RealTimeMultiDimensionalAnalyzer::add_dimension(
    realtime_analyzer,
    "region",
    "categorical"
  )
  
  // 测试实时异常检测
  let anomaly_detector = RealTimeAnomalyDetector::new({
    "algorithm": "isolation_forest",
    "sensitivity": 0.1,
    "window_size": 30
  })
  
  RealTimeMultiDimensionalAnalyzer::enable_anomaly_detection(
    realtime_analyzer,
    anomaly_detector,
    ["latency", "error_count", "throughput"]
  )
  
  // 等待异常检测
  sleep(10000)  // 10秒
  
  let anomalies = RealTimeMultiDimensionalAnalyzer::get_anomalies(realtime_analyzer)
  assert_true(anomalies.length() >= 0)
  
  // 测试实时趋势分析
  let trend_analysis = RealTimeMultiDimensionalAnalyzer::analyze_trends(
    realtime_analyzer,
    "latency",
    "service",
    300  // 5分钟趋势
  )
  
  assert_true(trend_analysis.length() > 0)
  assert_true(trend_analysis.all(fn(trend) {
    trend.contains("service") && trend.contains("trend_direction") &&
    trend.contains("trend_strength") && trend.contains("confidence")
  }))
  
  // 清理资源
  StreamProducer::stop(producer)
  RealTimeMultiDimensionalAnalyzer::stop(realtime_analyzer)
}