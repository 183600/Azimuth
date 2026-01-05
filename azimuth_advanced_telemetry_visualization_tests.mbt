// Azimuth 高级遥测数据可视化测试用例
// 专注于遥测数据的可视化表示、图表生成和交互式仪表板功能

// 测试1: 时间序列数据可视化
test "时间序列数据可视化生成" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    metadata: Array<(String, String)>
  }
  
  // 定义图表类型
  enum ChartType {
    Line
    Area
    Bar
    Scatter
    Heatmap
  }
  
  // 定义可视化配置
  type VisualizationConfig = {
    chart_type: ChartType,
    title: String,
    x_axis_label: String,
    y_axis_label: String,
    width: Int,
    height: Int,
    color_scheme: Array<String>,
    show_grid: Bool,
    show_legend: Bool
  }
  
  // 创建时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 25.5, metadata: [("service", "api"), ("region", "us-west")] },
    { timestamp: 1640995260, value: 32.1, metadata: [("service", "api"), ("region", "us-west")] },
    { timestamp: 1640995320, value: 28.7, metadata: [("service", "api"), ("region", "us-west")] },
    { timestamp: 1640995380, value: 45.2, metadata: [("service", "api"), ("region", "us-west")] },
    { timestamp: 1640995440, value: 38.9, metadata: [("service", "api"), ("region", "us-west")] },
    { timestamp: 1640995500, value: 42.3, metadata: [("service", "api"), ("region", "us-west")] }
  ]
  
  // 创建可视化配置
  let line_chart_config = {
    chart_type: ChartType::Line,
    title: "API响应时间趋势",
    x_axis_label: "时间",
    y_axis_label: "响应时间 (ms)",
    width: 800,
    height: 400,
    color_scheme: ["#3498db", "#e74c3c", "#2ecc71"],
    show_grid: true,
    show_legend: true
  }
  
  // 模拟图表数据生成
  let generate_chart_data = fn(data: Array[TimeSeriesPoint], config: VisualizationConfig) {
    let normalized_data = data.map(fn(point) {
      let normalized_time = (point.timestamp - data[0].timestamp) / 60  // 转换为分钟
      {
        x: normalized_time,
        y: point.value,
        label: point.timestamp.to_string()
      }
    })
    
    // 计算数据范围
    let x_values = normalized_data.map(fn(p) { p.x })
    let y_values = normalized_data.map(fn(p) { p.y })
    
    let x_min = x_values.reduce(fn(acc, v) { if acc < v { acc } else { v } }, x_values[0])
    let x_max = x_values.reduce(fn(acc, v) { if acc > v { acc } else { v } }, x_values[0])
    let y_min = y_values.reduce(fn(acc, v) { if acc < v { acc } else { v } }, y_values[0])
    let y_max = y_values.reduce(fn(acc, v) { if acc > v { acc } else { v } }, y_values[0])
    
    {
      chart_type: config.chart_type,
      title: config.title,
      data_points: normalized_data,
      x_range: (x_min, x_max),
      y_range: (y_min, y_max),
      dimensions: (config.width, config.height),
      color_scheme: config.color_scheme
    }
  }
  
  // 生成图表数据
  let chart_data = generate_chart_data(time_series_data, line_chart_config)
  
  // 验证图表数据
  assert_eq(chart_data.title, "API响应时间趋势")
  assert_eq(chart_data.data_points.length(), 6)
  assert_eq(chart_data.dimensions, (800, 400))
  
  let (x_min, x_max) = chart_data.x_range
  let (y_min, y_max) = chart_data.y_range
  assert_eq(x_min, 0)
  assert_eq(x_max, 50)
  assert_true(y_min <= 25.5)
  assert_true(y_max >= 45.2)
  
  // 测试不同图表类型
  let area_chart_config = { line_chart_config | chart_type: ChartType::Area }
  let area_chart_data = generate_chart_data(time_series_data, area_chart_config)
  match area_chart_data.chart_type {
    ChartType::Area => assert_true(true)
    _ => assert_true(false)
  }
  
  let bar_chart_config = { line_chart_config | chart_type: ChartType::Bar }
  let bar_chart_data = generate_chart_data(time_series_data, bar_chart_config)
  match bar_chart_data.chart_type {
    ChartType::Bar => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试2: 多维数据热力图可视化
test "多维数据热力图可视化" {
  // 定义热力图数据点
  type HeatmapPoint = {
    x: Int,
    y: Int,
    intensity: Float,
    label: String
  }
  
  // 定义热力图配置
  type HeatmapConfig = {
    title: String,
    x_label: String,
    y_label: String,
    color_gradient: Array<(Float, String)>,  // (强度, 颜色) 对
    cell_size: Int,
    show_values: Bool
  }
  
  // 创建热力图数据
  let heatmap_data = [
    { x: 0, y: 0, intensity: 0.2, label: "低" },
    { x: 1, y: 0, intensity: 0.5, label: "中" },
    { x: 2, y: 0, intensity: 0.8, label: "高" },
    { x: 0, y: 1, intensity: 0.3, label: "低" },
    { x: 1, y: 1, intensity: 0.9, label: "极高" },
    { x: 2, y: 1, intensity: 0.4, label: "中低" },
    { x: 0, y: 2, intensity: 0.7, label: "中高" },
    { x: 1, y: 2, intensity: 0.6, label: "中" },
    { x: 2, y: 2, intensity: 0.1, label: "极低" }
  ]
  
  // 创建热力图配置
  let heatmap_config = {
    title: "服务负载分布热力图",
    x_label: "服务实例",
    y_label: "时间区间",
    color_gradient: [
      (0.0, "#2c3e50"),    // 深蓝
      (0.3, "#3498db"),    // 蓝
      (0.5, "#f39c12"),    // 橙
      (0.7, "#e67e22"),    // 深橙
      (1.0, "#e74c3c")     // 红
    ],
    cell_size: 50,
    show_values: true
  }
  
  // 颜色插值函数
  let interpolate_color = fn(intensity: Float, gradient: Array<(Float, String)>) {
    // 找到强度在梯度中的位置
    let mut lower_bound = (0.0, "#000000")
    let mut upper_bound = (1.0, "#ffffff")
    
    for i in 0..(gradient.length() - 1) {
      let (lower_intensity, lower_color) = gradient[i]
      let (upper_intensity, upper_color) = gradient[i + 1]
      
      if intensity >= lower_intensity && intensity <= upper_intensity {
        lower_bound = (lower_intensity, lower_color)
        upper_bound = (upper_intensity, upper_color)
      }
    }
    
    // 简化的颜色插值（实际实现需要更复杂的颜色空间转换）
    let (lower_intensity, _) = lower_bound
    let (_, upper_color) = upper_bound
    
    if intensity <= 0.5 {
      "#3498db"  // 蓝色系
    } else {
      "#e74c3c"  // 红色系
    }
  }
  
  // 生成热力图可视化数据
  let generate_heatmap_visualization = fn(data: Array[HeatmapPoint], config: HeatmapConfig) {
    let max_x = data.reduce(fn(acc, p) { if acc > p.x { acc } else { p.x } }, 0)
    let max_y = data.reduce(fn(acc, p) { if acc > p.y { acc } else { p.y } }, 0)
    
    let visual_data = data.map(fn(point) {
      let color = interpolate_color(point.intensity, config.color_gradient)
      {
        x: point.x,
        y: point.y,
        color: color,
        intensity: point.intensity,
        label: point.label,
        position: (point.x * config.cell_size, point.y * config.cell_size),
        size: config.cell_size
      }
    })
    
    {
      title: config.title,
      dimensions: ((max_x + 1) * config.cell_size, (max_y + 1) * config.cell_size),
      cells: visual_data,
      color_gradient: config.color_gradient,
      axes_labels: (config.x_label, config.y_label)
    }
  }
  
  // 生成热力图
  let heatmap = generate_heatmap_visualization(heatmap_data, heatmap_config)
  
  // 验证热力图
  assert_eq(heatmap.title, "服务负载分布热力图")
  assert_eq(heatmap.cells.length(), 9)
  assert_eq(heatmap.dimensions, (150, 150))  // 3x3 网格，每个单元格50px
  
  // 验证特定数据点
  let high_intensity_cell = heatmap.cells.find(fn(cell) { cell.intensity == 0.9 })
  match high_intensity_cell {
    Some(cell) => {
      assert_eq(cell.x, 1)
      assert_eq(cell.y, 1)
      assert_eq(cell.label, "极高")
      assert_eq(cell.position, (50, 50))
      assert_eq(cell.size, 50)
    }
    None => assert_true(false)
  }
  
  // 验证低强度数据点
  let low_intensity_cells = heatmap.cells.filter(fn(cell) { cell.intensity < 0.2 })
  assert_eq(low_intensity_cells.length(), 1)
  assert_eq(low_intensity_cells[0].label, "极低")
}

// 测试3: 实时仪表板数据流处理
test "实时仪表板数据流处理" {
  // 定义仪表板组件类型
  enum DashboardComponent {
    MetricCard(String, String, String)  // 标题, 值, 单位
    LineChart(String, Array[(Int, Float)])  // 标题, 数据点
    ProgressBar(String, Float, String)  // 标题, 进度, 状态
    StatusIndicator(String, Bool)  // 标题, 状态
  }
  
  // 定义仪表板布局
  type DashboardLayout = {
    title: String,
    components: Array[DashboardComponent],
    refresh_interval_ms: Int,
    last_updated: Int
  }
  
  // 模拟实时数据流
  let simulate_real_time_data = fn(initial_metrics: Array[(String, Float)], update_count: Int) {
    let mut data_stream = []
    let mut current_metrics = initial_metrics
    
    for i in 0..update_count {
      // 模拟数据更新
      let updated_metrics = current_metrics.map(fn(metric) {
        let (name, value) = metric
        // 添加随机变化
        let change = (i % 3 - 1) as Float * 2.5  // -2.5, 0, 或 2.5
        (name, value + change)
      })
      
      current_metrics = updated_metrics
      data_stream = data_stream.push({
        timestamp: 1640995200 + i * 60,  // 每分钟更新
        metrics: updated_metrics,
        update_id: i
      })
    }
    
    data_stream
  }
  
  // 创建仪表板组件生成器
  let create_dashboard_components = fn(metrics: Array[(String, Float)]) {
    let components = []
    
    // 为每个指标创建卡片
    for i in 0..metrics.length() {
      let (name, value) = metrics[i]
      let component = DashboardComponent::MetricCard(
        name,
        value.round().to_string(),
        "ms"
      )
      components = components.push(component)
    }
    
    // 创建进度条组件
    let avg_response_time = metrics.reduce(fn(acc, m) { acc + m.1 }, 0.0) / (metrics.length() as Float)
    let progress = if avg_response_time < 50.0 { 1.0 } else if avg_response_time < 100.0 { 0.7 } else { 0.3 }
    let progress_component = DashboardComponent::ProgressBar(
      "系统健康度",
      progress,
      if progress > 0.8 { "优秀" } else if progress > 0.5 { "良好" } else { "需要关注" }
    )
    
    // 创建状态指示器
    let status_component = DashboardComponent::StatusIndicator(
      "服务状态",
      avg_response_time < 100.0
    )
    
    components.push(progress_component).push(status_component)
  }
  
  // 生成实时数据流
  let initial_metrics = [
    ("API响应时间", 45.2),
    ("数据库查询", 23.8),
    ("缓存命中", 12.1)
  ]
  
  let data_stream = simulate_real_time_data(initial_metrics, 5)
  assert_eq(data_stream.length(), 5)
  
  // 处理数据流并更新仪表板
  let process_data_stream = fn(stream: Array[Array[(String, Float)]>) {
    let mut dashboards = []
    
    for i in 0..stream.length() {
      let metrics = stream[i]
      let components = create_dashboard_components(metrics)
      
      let dashboard = {
        title: "实时监控仪表板",
        components: components,
        refresh_interval_ms: 60000,  // 1分钟
        last_updated: 1640995200 + i * 60
      }
      
      dashboards = dashboards.push(dashboard)
    }
    
    dashboards
  }
  
  // 提取指标流
  let metrics_stream = data_stream.map(fn(update) { update.metrics })
  let dashboards = process_data_stream(metrics_stream)
  
  // 验证仪表板生成
  assert_eq(dashboards.length(), 5)
  
  // 验证第一个仪表板
  let first_dashboard = dashboards[0]
  assert_eq(first_dashboard.title, "实时监控仪表板")
  assert_eq(first_dashboard.components.length(), 5)  // 3个指标卡片 + 1个进度条 + 1个状态指示器
  assert_eq(first_dashboard.refresh_interval_ms, 60000)
  assert_eq(first_dashboard.last_updated, 1640995200)
  
  // 验证最后一次更新
  let last_dashboard = dashboards[4]
  assert_eq(last_dashboard.last_updated, 1640995200 + 4 * 60)
  
  // 验证指标卡片组件
  let metric_cards = last_dashboard.components.filter(fn(comp) {
    match comp {
      DashboardComponent::MetricCard(_, _, _) => true
      _ => false
    }
  })
  assert_eq(metric_cards.length(), 3)
  
  // 验证进度条组件
  let progress_bars = last_dashboard.components.filter(fn(comp) {
    match comp {
      DashboardComponent::ProgressBar(_, _, _) => true
      _ => false
    }
  })
  assert_eq(progress_bars.length(), 1)
  
  match progress_bars[0] {
    DashboardComponent::ProgressBar(title, progress, status) => {
      assert_eq(title, "系统健康度")
      assert_true(progress >= 0.0 && progress <= 1.0)
      assert_true(status == "优秀" || status == "良好" || status == "需要关注")
    }
    _ => assert_true(false)
  }
}

// 测试4: 交互式数据探索功能
test "交互式数据探索功能" {
  // 定义数据过滤器
  type DataFilter = {
    field: String,
    operator: String,  // ">", "<", "=", "contains", "between"
    value: String
  }
  
  // 定义数据排序
  type DataSort = {
    field: String,
    direction: String  // "asc", "desc"
  }
  
  // 定义数据查询
  type DataQuery = {
    filters: Array[DataFilter],
    sort: Option[DataSort],
    limit: Option[Int>,
    offset: Option[Int]
  }
  
  // 定义遥测记录
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    service_name: String,
    operation: String,
    duration: Float,
    status: String,
    attributes: Array[(String, String)]
  }
  
  // 创建测试数据集
  let telemetry_dataset = [
    {
      id: "trace-001",
      timestamp: 1640995200,
      service_name: "payment-service",
      operation: "process_payment",
      duration: 125.5,
      status: "success",
      attributes: [("user_id", "user-123"), ("amount", "99.99")]
    },
    {
      id: "trace-002",
      timestamp: 1640995260,
      service_name: "user-service",
      operation: "get_user",
      duration: 45.2,
      status: "success",
      attributes: [("user_id", "user-456")]
    },
    {
      id: "trace-003",
      timestamp: 1640995320,
      service_name: "payment-service",
      operation: "validate_payment",
      duration: 230.1,
      status: "error",
      attributes: [("error_code", "INVALID_CARD")]
    },
    {
      id: "trace-004",
      timestamp: 1640995380,
      service_name: "inventory-service",
      operation: "check_stock",
      duration: 78.9,
      status: "success",
      attributes: [("product_id", "prod-789"), ("quantity", "5")]
    },
    {
      id: "trace-005",
      timestamp: 1640995440,
      service_name: "payment-service",
      operation: "process_payment",
      duration: 156.3,
      status: "success",
      attributes: [("user_id", "user-123"), ("amount", "49.99")]
    }
  ]
  
  // 创建数据过滤器应用函数
  let apply_filter = fn(record: TelemetryRecord, filter: DataFilter) {
    match filter.field {
      "service_name" => {
        match filter.operator {
          "=" => record.service_name == filter.value
          "contains" => record.service_name.contains(filter.value)
          _ => false
        }
      }
      "duration" => {
        match filter.operator {
          ">" => record.duration > filter.value.to_float()
          "<" => record.duration < filter.value.to_float()
          "between" => {
            let parts = filter.value.split(",")
            if parts.length() == 2 {
              let min = parts[0].to_float()
              let max = parts[1].to_float()
              record.duration >= min && record.duration <= max
            } else {
              false
            }
          }
          _ => false
        }
      }
      "status" => {
        match filter.operator {
          "=" => record.status == filter.value
          _ => false
        }
      }
      _ => false
    }
  }
  
  // 创建数据排序函数
  let sort_records = fn(records: Array[TelemetryRecord], sort: DataSort) {
    let comparator = fn(a: TelemetryRecord, b: TelemetryRecord) {
      let a_value = match sort.field {
        "duration" => a.duration.to_string()
        "timestamp" => a.timestamp.to_string()
        "service_name" => a.service_name
        _ => ""
      }
      
      let b_value = match sort.field {
        "duration" => b.duration.to_string()
        "timestamp" => b.timestamp.to_string()
        "service_name" => b.service_name
        _ => ""
      }
      
      if sort.direction == "asc" {
        if a_value < b_value { -1 } else if a_value > b_value { 1 } else { 0 }
      } else {
        if a_value > b_value { -1 } else if a_value < b_value { 1 } else { 0 }
      }
    }
    
    records.sort(comparator)
  }
  
  // 创建查询执行函数
  let execute_query = fn(dataset: Array[TelemetryRecord], query: DataQuery) {
    // 应用过滤器
    let mut filtered_data = dataset
    for filter in query.filters {
      filtered_data = filtered_data.filter(fn(record) { apply_filter(record, filter) })
    }
    
    // 应用排序
    let sorted_data = match query.sort {
      Some(sort_config) => sort_records(filtered_data, sort_config)
      None => filtered_data
    }
    
    // 应用分页
    let offset = match query.offset {
      Some(o) => o
      None => 0
    }
    
    let limit = match query.limit {
      Some(l) => l
      None => sorted_data.length()
    }
    
    let end_index = if offset + limit > sorted_data.length() {
      sorted_data.length()
    } else {
      offset + limit
    }
    
    sorted_data.slice(offset, end_index)
  }
  
  // 测试无过滤器的查询
  let basic_query = {
    filters: [],
    sort: None,
    limit: None,
    offset: None
  }
  
  let all_records = execute_query(telemetry_dataset, basic_query)
  assert_eq(all_records.length(), 5)
  
  // 测试单个过滤器查询
  let payment_service_filter = {
    field: "service_name",
    operator: "=",
    value: "payment-service"
  }
  
  let payment_query = {
    filters: [payment_service_filter],
    sort: None,
    limit: None,
    offset: None
  }
  
  let payment_records = execute_query(telemetry_dataset, payment_query)
  assert_eq(payment_records.length(), 3)
  for record in payment_records {
    assert_eq(record.service_name, "payment-service")
  }
  
  // 测试多个过滤器查询
  let duration_filter = {
    field: "duration",
    operator: ">",
    value: "100.0"
  }
  
  let multi_filter_query = {
    filters: [payment_service_filter, duration_filter],
    sort: None,
    limit: None,
    offset: None
  }
  
  let filtered_records = execute_query(telemetry_dataset, multi_filter_query)
  assert_eq(filtered_records.length(), 2)
  for record in filtered_records {
    assert_eq(record.service_name, "payment-service")
    assert_true(record.duration > 100.0)
  }
  
  // 测试排序查询
  let sort_config = {
    field: "duration",
    direction: "desc"
  }
  
  let sorted_query = {
    filters: [],
    sort: Some(sort_config),
    limit: None,
    offset: None
  }
  
  let sorted_records = execute_query(telemetry_dataset, sorted_query)
  assert_eq(sorted_records.length(), 5)
  assert_eq(sorted_records[0].id, "trace-003")  // 最长的持续时间
  assert_eq(sorted_records[4].id, "trace-002")  // 最短的持续时间
  
  // 测试分页查询
  let paginated_query = {
    filters: [],
    sort: Some(sort_config),
    limit: Some(2),
    offset: Some(1)
  }
  
  let paginated_records = execute_query(telemetry_dataset, paginated_query)
  assert_eq(paginated_records.length(), 2)
  assert_eq(paginated_records[0].id, "trace-005")  // 第二长的持续时间
  assert_eq(paginated_records[1].id, "trace-001")  // 第三长的持续时间
  
  // 测试复杂查询
  let between_filter = {
    field: "duration",
    operator: "between",
    value: "50.0,150.0"
  }
  
  let complex_query = {
    filters: [between_filter],
    sort: Some({
      field: "timestamp",
      direction: "asc"
    }),
    limit: Some(3),
    offset: Some(0)
  }
  
  let complex_results = execute_query(telemetry_dataset, complex_query)
  assert_eq(complex_results.length(), 3)
  for record in complex_results {
    assert_true(record.duration >= 50.0 && record.duration <= 150.0)
  }
  
  // 验证时间戳排序
  assert_true(complex_results[0].timestamp <= complex_results[1].timestamp)
  assert_true(complex_results[1].timestamp <= complex_results[2].timestamp)
}

// 测试5: 自定义可视化组件
test "自定义可视化组件" {
  // 定义组件基类
  type VisualComponent = {
    id: String,
    type: String,
    position: (Int, Int),
    size: (Int, Int),
    data: Array[(String, String)],
    style: Array[(String, String)]
  }
  
  // 定义仪表盘组件类型
  enum ComponentType {
    Gauge          // 仪表盘
    TreeMap        // 树状图
    Sankey         // 桑基图
    Funnel         // 漏斗图
    Radar          // 雷达图
  }
  
  // 创建仪表盘组件
  let create_gauge_component = fn(id: String, value: Float, min: Float, max: Float) {
    let percentage = (value - min) / (max - min) * 100.0
    let color = if percentage >= 80.0 {
      "#e74c3c"  // 红色
    } else if percentage >= 60.0 {
      "#f39c12"  // 橙色
    } else {
      "#2ecc71"  // 绿色
    }
    
    {
      id: id,
      type: "gauge",
      position: (0, 0),
      size: (200, 200),
      data: [
        ("value", value.to_string()),
        ("min", min.to_string()),
        ("max", max.to_string()),
        ("percentage", percentage.round().to_string() + "%")
      ],
      style: [
        ("color", color),
        ("background", "#ecf0f1"),
        ("border-radius", "10px")
      ]
    }
  }
  
  // 创建树状图组件
  let create_treemap_component = fn(id: String, data: Array[(String, Float, String)>) {
    let total = data.reduce(fn(acc, item) { acc + item.1 }, 0.0)
    
    let processed_data = data.map(fn(item) {
      let (name, value, color) = item
      let percentage = (value / total * 100.0).round()
      (name, value, color, percentage.to_string() + "%")
    })
    
    {
      id: id,
      type: "treemap",
      position: (0, 0),
      size: (400, 300),
      data: processed_data.map(fn(item) {
        let (name, value, color, percentage) = item
        (name, value.to_string() + " (" + percentage + ")", color)
      }),
      style: [
        ("border", "1px solid #bdc3c7"),
        ("padding", "5px")
      ]
    }
  }
  
  // 创建雷达图组件
  let create_radar_component = fn(id: String, metrics: Array[(String, Float)>) {
    let max_value = metrics.reduce(fn(acc, m) { if acc > m.1 { acc } else { m.1 } }, 0.0)
    
    let normalized_metrics = metrics.map(fn(metric) {
      let (name, value) = metric
      let normalized = if max_value > 0.0 { value / max_value } else { 0.0 }
      (name, normalized)
    })
    
    {
      id: id,
      type: "radar",
      position: (0, 0),
      size: (300, 300),
      data: normalized_metrics.map(fn(m) { (m.0, m.1.to_string()) }),
      style: [
        ("fill-color", "rgba(52, 152, 219, 0.3)"),
        ("stroke-color", "#3498db"),
        ("stroke-width", "2")
      ]
    }
  }
  
  // 创建仪表盘
  let gauge_component = create_gauge_component("cpu-gauge", 75.5, 0.0, 100.0)
  assert_eq(gauge_component.type, "gauge")
  assert_eq(gauge_component.size, (200, 200))
  assert_eq(gauge_component.data.length(), 4)
  assert_eq(gauge_component.data[3], ("percentage", "76%"))
  assert_eq(gauge_component.style[0], ("color", "#f39c12"))
  
  // 创建树状图
  let service_data = [
    ("api-service", 45.2, "#3498db"),
    ("payment-service", 32.1, "#e74c3c"),
    ("user-service", 22.7, "#2ecc71")
  ]
  
  let treemap_component = create_treemap_component("service-treemap", service_data)
  assert_eq(treemap_component.type, "treemap")
  assert_eq(treemap_component.size, (400, 300))
  assert_eq(treemap_component.data.length(), 3)
  
  // 验证百分比计算
  let api_data = treemap_component.data.find(fn(d) { d.0.contains("api-service") })
  match api_data {
    Some((name, label)) => {
      assert_true(label.contains("45"))
      assert_true(label.contains("%"))
    }
    None => assert_true(false)
  }
  
  // 创建雷达图
  let performance_metrics = [
    ("响应时间", 0.75),
    ("吞吐量", 0.85),
    ("错误率", 0.15),
    ("可用性", 0.95),
    ("资源利用率", 0.60)
  ]
  
  let radar_component = create_radar_component("performance-radar", performance_metrics)
  assert_eq(radar_component.type, "radar")
  assert_eq(radar_component.size, (300, 300))
  assert_eq(radar_component.data.length(), 5)
  
  // 验证数据归一化
  let availability_data = radar_component.data.find(fn(d) { d.0 == "可用性" })
  match availability_data {
    Some((name, value)) => {
      assert_eq(name, "可用性")
      assert_eq(value, "1.0")  // 0.95 / 0.95 = 1.0 (最大值)
    }
    None => assert_true(false)
  }
  
  // 创建组件布局管理器
  let layout_components = fn(components: Array[VisualComponent], layout: String) {
    let mut positioned_components = []
    
    match layout {
      "grid" => {
        let cols = 2
        for i in 0..components.length() {
          let row = i / cols
          let col = i % cols
          let component = components[i]
          let positioned = { component | 
            position: (col * 250, row * 220)
          }
          positioned_components = positioned_components.push(positioned)
        }
      }
      "horizontal" => {
        let mut x_offset = 0
        for component in components {
          let positioned = { component | 
            position: (x_offset, 0)
          }
          positioned_components = positioned_components.push(positioned)
          let (width, _) = component.size
          x_offset = x_offset + width + 20
        }
      }
      "vertical" => {
        let mut y_offset = 0
        for component in components {
          let positioned = { component | 
            position: (0, y_offset)
          }
          positioned_components = positioned_components.push(positioned)
          let (_, height) = component.size
          y_offset = y_offset + height + 20
        }
      }
      _ => {
        positioned_components = components
      }
    }
    
    positioned_components
  }
  
  // 测试组件布局
  let components = [gauge_component, treemap_component, radar_component]
  let grid_layout = layout_components(components, "grid")
  
  assert_eq(grid_layout.length(), 3)
  assert_eq(grid_layout[0].position, (0, 0))
  assert_eq(grid_layout[1].position, (250, 0))
  assert_eq(grid_layout[2].position, (0, 220))
  
  let horizontal_layout = layout_components(components, "horizontal")
  assert_eq(horizontal_layout[0].position, (0, 0))
  assert_eq(horizontal_layout[1].position, (220, 0))  // 200 + 20
  assert_eq(horizontal_layout[2].position, (620, 0))  // 220 + 400 + 20
  
  let vertical_layout = layout_components(components, "vertical")
  assert_eq(vertical_layout[0].position, (0, 0))
  assert_eq(vertical_layout[1].position, (0, 220))  // 200 + 20
  assert_eq(vertical_layout[2].position, (0, 540))  // 220 + 300 + 20
}