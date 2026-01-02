// Azimuth Custom Dashboard and Visualization Tests
// 自定义仪表板和可视化测试用例 - 专注于数据可视化和仪表板定制功能

// Test 1: 仪表板创建和配置
test "dashboard creation and configuration" {
  // 创建仪表板管理器
  let dashboard_manager = DashboardManager::new()
  
  // 配置仪表板参数
  DashboardManager::configure(dashboard_manager, [
    ("max.widgets", IntValue(50)),
    ("refresh.interval", IntValue(30000)), // 30秒
    ("theme.default", StringValue("light")),
    ("export.formats", StringValue("png,svg,pdf,json"))
  ])
  
  // 创建仪表板
  let dashboard_config = DashboardConfig::new(
    "system_performance_dashboard",
    "System Performance Overview",
    "Real-time system performance metrics and analytics",
    ["admin", "analyst"],
    "light"
  )
  
  let dashboard_creation_result = DashboardManager::create_dashboard(dashboard_manager, dashboard_config)
  assert_true(dashboard_creation_result)
  
  // 验证仪表板创建
  let dashboard = DashboardManager::get_dashboard(dashboard_manager, "system_performance_dashboard")
  assert_true(dashboard !== None)
  
  match dashboard {
    Some(dash) => {
      assert_eq(Dashboard::id(dash), "system_performance_dashboard")
      assert_eq(Dashboard::title(dash), "System Performance Overview")
      assert_eq(Dashboard::theme(dash), "light")
      assert_true(Dashboard::is_active(dash))
    }
    None => assert_true(false)
  }
  
  // 创建仪表板布局
  let layout_config = LayoutConfig::new(
    "grid_layout",
    12, // 12列网格
    8,  // 8行网格
    [
      GridLayout::new(0, 0, 6, 4), // CPU使用率图表
      GridLayout::new(6, 0, 6, 4), // 内存使用率图表
      GridLayout::new(0, 4, 4, 4), // 网络流量图表
      GridLayout::new(4, 4, 4, 4), // 磁盘I/O图表
      GridLayout::new(8, 4, 4, 4), // 系统负载图表
      GridLayout::new(0, 8, 12, 4) // 时间序列图表
    ]
  )
  
  let layout_result = DashboardManager::set_layout(dashboard_manager, "system_performance_dashboard", layout_config)
  assert_true(layout_result)
  
  // 验证布局设置
  let dashboard_layout = DashboardManager::get_layout(dashboard_manager, "system_performance_dashboard")
  assert_true(dashboard_layout !== None)
  
  match dashboard_layout {
    Some(layout) => {
      assert_eq(LayoutConfig::type(layout), "grid_layout")
      assert_eq(LayoutConfig::columns(layout), 12)
      assert_eq(LayoutConfig::rows(layout), 8)
      assert_eq(LayoutConfig::grid_count(layout), 6)
    }
    None => assert_true(false)
  }
}

// Test 2: 数据可视化组件创建
test "data visualization component creation" {
  // 创建可视化组件管理器
  let widget_manager = WidgetManager::new()
  
  // 配置可视化组件参数
  WidgetManager::configure(widget_manager, [
    ("chart.animation.enabled", BoolValue(true)),
    ("chart.animation.duration", IntValue(1000)),
    ("color.scheme", StringValue("azimuth_default")),
    ("font.family", StringValue("Inter,sans-serif"))
  ])
  
  // 创建折线图组件
  let line_chart_config = ChartConfig::new(
    "cpu_usage_line_chart",
    "CPU Usage Over Time",
    LineChart,
    [
      ChartAxis::new("x", "Time", DateTime),
      ChartAxis::new("y", "CPU Usage (%)", Numeric)
    ],
    [
      ChartSeries::new("CPU Usage", "cpu.usage", "#3b82f6", Line, true)
    ],
    ChartOptions::new(
      true, // show legend
      true, // show grid
      true, // show tooltips
      1000  // data points limit
    )
  )
  
  let line_chart_result = WidgetManager::create_chart(widget_manager, line_chart_config)
  assert_true(line_chart_result)
  
  // 创建饼图组件
  let pie_chart_config = ChartConfig::new(
    "resource_distribution_pie",
    "Resource Distribution",
    PieChart,
    [],
    [
      ChartSeries::new("CPU", "cpu.percentage", "#ef4444", Pie, false),
      ChartSeries::new("Memory", "memory.percentage", "#10b981", Pie, false),
      ChartSeries::new("Disk", "disk.percentage", "#f59e0b", Pie, false),
      ChartSeries::new("Network", "network.percentage", "#8b5cf6", Pie, false)
    ],
    ChartOptions::new(
      true,  // show legend
      false, // show grid
      true,  // show tooltips
      10     // data points limit
    )
  )
  
  let pie_chart_result = WidgetManager::create_chart(widget_manager, pie_chart_config)
  assert_true(pie_chart_result)
  
  // 创建仪表盘组件
  let gauge_config = GaugeConfig::new(
    "system_health_gauge",
    "System Health",
    0.0,
    100.0,
    [
      GaugeRange::new(0.0, 30.0, "#ef4444", "Critical"),
      GaugeRange::new(30.0, 60.0, "#f59e0b", "Warning"),
      GaugeRange::new(60.0, 100.0, "#10b981", "Healthy")
    ],
    GaugeOptions::new(
      true, // show labels
      true, // show needle
      true  // animated
    )
  )
  
  let gauge_result = WidgetManager::create_gauge(widget_manager, gauge_config)
  assert_true(gauge_result)
  
  // 创建热力图组件
  let heatmap_config = HeatmapConfig::new(
    "activity_heatmap",
    "System Activity Heatmap",
    ["Hour", "Day"],
    [
      HeatmapColor::new(0.0, "#f3f4f6"),
      HeatmapColor::new(25.0, "#fbbf24"),
      HeatmapColor::new(50.0, "#fb923c"),
      HeatmapColor::new(75.0, "#f87171"),
      HeatmapColor::new(100.0, "#dc2626")
    ],
    HeatmapOptions::new(
      true, // show labels
      true, // show legend
      true  // interactive
    )
  )
  
  let heatmap_result = WidgetManager::create_heatmap(widget_manager, heatmap_config)
  assert_true(heatmap_result)
  
  // 验证组件创建
  let widgets = WidgetManager::get_all_widgets(widget_manager)
  assert_eq(widgets.length(), 4)
  
  let widget_types = widgets.map(fn(widget) { Widget::type(widget) })
  assert_true(widget_types.contains(LineChart))
  assert_true(widget_types.contains(PieChart))
  assert_true(widget_types.contains(Gauge))
  assert_true(widget_types.contains(Heatmap))
}

// Test 3: 实时数据绑定和更新
test "real-time data binding and updates" {
  // 创建数据绑定管理器
  let data_binding_manager = DataBindingManager::new()
  
  // 配置数据绑定参数
  DataBindingManager::configure(data_binding_manager, [
    ("update.interval", IntValue(5000)), // 5秒
    ("buffer.size", IntValue(1000)),
    ("data.transformations", StringValue("aggregation,filtering,normalization")),
    ("error.retry.count", IntValue(3))
  ])
  
  // 创建数据源
  let telemetry_data_source = DataSource::new(
    "telemetry_metrics",
    "Real-time telemetry data",
    "azimuth.telemetry.stream",
    DataSourceType::Stream
  )
  
  let data_source_result = DataBindingManager::add_data_source(data_binding_manager, telemetry_data_source)
  assert_true(data_source_result)
  
  // 创建数据绑定
  let bindings = [
    DataBinding::new(
      "cpu_usage_line_chart",
      "telemetry_metrics",
      [
        DataMapping::new("cpu.usage", "y"),
        DataMapping::new("timestamp", "x")
      ],
      DataTransformation::new("aggregation", [
        ("function", "avg"),
        ("window", "1m")
      ])
    ),
    DataBinding::new(
      "resource_distribution_pie",
      "telemetry_metrics",
      [
        DataMapping::new("cpu.percentage", "value"),
        DataMapping::new("memory.percentage", "value"),
        DataMapping::new("disk.percentage", "value"),
        DataMapping::new("network.percentage", "value")
      ],
      DataTransformation::new("latest", [])
    ),
    DataBinding::new(
      "system_health_gauge",
      "telemetry_metrics",
      [
        DataMapping::new("health.score", "value")
      ],
      DataTransformation::new("weighted_average", [
        ("cpu.weight", "0.3"),
        ("memory.weight", "0.3"),
        ("disk.weight", "0.2"),
        ("network.weight", "0.2")
      ])
    )
  ]
  
  for binding in bindings {
    let binding_result = DataBindingManager::add_binding(data_binding_manager, binding)
    assert_true(binding_result)
  }
  
  // 生成模拟实时数据
  let real_time_data = []
  let base_time = Time::now()
  
  for i in 0..<100 {
    let timestamp = base_time + (i * 1000) // 每秒一个数据点
    
    let data_point = TelemetryData::new_with_timestamp([
      ("cpu.usage", FloatValue(50.0 + (Math::sin(i.to_float() * 0.1) * 30.0))),
      ("memory.usage", FloatValue(60.0 + (Math::cos(i.to_float() * 0.08) * 20.0))),
      ("disk.usage", FloatValue(40.0 + (Math::sin(i.to_float() * 0.05) * 15.0))),
      ("network.usage", FloatValue(30.0 + (Math::cos(i.to_float() * 0.12) * 25.0))),
      ("cpu.percentage", FloatValue(25.0)),
      ("memory.percentage", FloatValue(25.0)),
      ("disk.percentage", FloatValue(25.0)),
      ("network.percentage", FloatValue(25.0)),
      ("health.score", FloatValue(75.0 + (Math::sin(i.to_float() * 0.06) * 20.0)))
    ], timestamp)
    
    real_time_data = real_time_data.push(data_point)
  }
  
  // 测试数据绑定更新
  let update_start = Time::now()
  let update_result = DataBindingManager::update_bindings(data_binding_manager, real_time_data)
  let update_end = Time::now()
  let update_duration = update_end - update_start
  
  assert_true(update_result)
  assert_true(update_duration < 5000) // 更新应在5秒内完成
  
  // 验证绑定状态
  let binding_status = DataBindingManager::get_binding_status(data_binding_manager)
  assert_true(BindingStatus::is_active(binding_status))
  assert_true(BindingStatus::last_update_time(binding_status) > 0)
  assert_true(BindingStatus::data_points_processed(binding_status) > 0)
  
  // 测试数据转换
  let transformed_data = DataBindingManager::get_transformed_data(data_binding_manager, "cpu_usage_line_chart")
  assert_true(transformed_data.length() > 0)
  
  // 验证聚合数据
  for data in transformed_data {
    assert_true(DataPoint::has_attribute(data, "cpu.usage"))
    assert_true(DataPoint::has_attribute(data, "timestamp"))
  }
}

// Test 4: 交互式功能和用户操作
test "interactive features and user operations" {
  // 创建交互管理器
  let interaction_manager = InteractionManager::new()
  
  // 配置交互参数
  InteractionManager::configure(interaction_manager, [
    ("click.actions", StringValue("drill_down,filter,highlight")),
    ("hover.enabled", BoolValue(true)),
    ("selection.mode", StringValue("single")),
    ("zoom.enabled", BoolValue(true)),
    ("pan.enabled", BoolValue(true))
  ])
  
  // 创建交互式图表
  let interactive_chart = InteractiveChart::new(
    "interactive_performance_chart",
    "Interactive Performance Analysis",
    LineChart,
    [
      ChartSeries::new("Response Time", "response.time", "#3b82f6", Line, true),
      ChartSeries::new("Throughput", "throughput", "#10b981", Line, true),
      ChartSeries::new("Error Rate", "error.rate", "#ef4444", Line, true)
    ]
  )
  
  // 添加点击交互
  let click_interaction = ClickInteraction::new(
    "drill_down",
    "Click to drill down into detailed metrics",
    fn(data_point) {
      // 返回详细数据视图
      DetailView::new("Detailed Metrics", data_point)
    }
  )
  
  let hover_interaction = HoverInteraction::new(
    "tooltip",
    "Show detailed information on hover",
    fn(data_point) {
      // 返回工具提示内容
      Tooltip::new_with_data(data_point)
    }
  )
  
  let selection_interaction = SelectionInteraction::new(
    "time_range_selection",
    "Select time range for detailed analysis",
    Multiple,
    fn(selected_points) {
      // 返回时间范围分析
      TimeRangeAnalysis::new(selected_points)
    }
  )
  
  assert_true(InteractionManager::add_click_interaction(interaction_manager, interactive_chart, click_interaction))
  assert_true(InteractionManager::add_hover_interaction(interaction_manager, interactive_chart, hover_interaction))
  assert_true(InteractionManager::add_selection_interaction(interaction_manager, interactive_chart, selection_interaction))
  
  // 模拟用户交互
  let test_data_points = [
    DataPoint::new_with_attributes("performance", [
      ("response.time", FloatValue(120.5)),
      ("throughput", FloatValue(1500.0)),
      ("error.rate", FloatValue(0.05)),
      ("timestamp", IntValue(Time::now()))
    ]),
    DataPoint::new_with_attributes("performance", [
      ("response.time", FloatValue(95.2)),
      ("throughput", FloatValue(1800.0)),
      ("error.rate", FloatValue(0.02)),
      ("timestamp", IntValue(Time::now() + 60000))
    ])
  ]
  
  // 测试点击交互
  let click_result = InteractionManager::handle_click(interaction_manager, interactive_chart, test_data_points[0])
  match click_result {
    Some(DetailView(detail)) => {
      assert_true(DetailView::has_data(detail))
      assert_true(DetailView::data_contains(detail, "response.time"))
    }
    _ => assert_true(false)
  }
  
  // 测试悬停交互
  let hover_result = InteractionManager::handle_hover(interaction_manager, interactive_chart, test_data_points[1])
  match hover_result {
    Some(Tooltip(tooltip)) => {
      assert_true(Tooltip::has_content(tooltip))
      assert_true(Tooltip::content_contains(tooltip, "95.2"))
    }
    _ => assert_true(false)
  }
  
  // 测试选择交互
  let selection_result = InteractionManager::handle_selection(interaction_manager, interactive_chart, test_data_points)
  match selection_result {
    Some(TimeRangeAnalysis(analysis)) => {
      assert_true(TimeRangeAnalysis::has_start_time(analysis))
      assert_true(TimeRangeAnalysis::has_end_time(analysis))
      assert_true(TimeRangeAnalysis::data_point_count(analysis) == test_data_points.length())
    }
    _ => assert_true(false)
  }
  
  // 测试缩放和平移
  let zoom_result = InteractionManager::handle_zoom(interaction_manager, interactive_chart, ZoomIn, 1.5)
  assert_true(zoom_result)
  
  let pan_result = InteractionManager::handle_pan(interaction_manager, interactive_chart, PanRight, 100)
  assert_true(pan_result)
  
  // 验证图表状态更新
  let chart_state = InteractionManager::get_chart_state(interaction_manager, interactive_chart)
  assert_true(ChartState::zoom_level(chart_state) > 1.0)
  assert_true(ChartState::pan_offset(chart_state) > 0)
}

// Test 5: 仪表板导出和分享
test "dashboard export and sharing" {
  // 创建导出管理器
  let export_manager = ExportManager::new()
  
  // 配置导出参数
  ExportManager::configure(export_manager, [
    ("export.formats", StringValue("png,svg,pdf,json,html")),
    ("export.quality", StringValue("high")),
    ("include.data", BoolValue(false)),
    ("watermark.enabled", BoolValue(true))
  ])
  
  // 创建分享管理器
  let sharing_manager = SharingManager::new()
  
  // 配置分享参数
  SharingManager::configure(sharing_manager, [
    ("public.access", BoolValue(false)),
    ("share.expiration.days", IntValue(30)),
    ("password.protected", BoolValue(true)),
    ("allow.download", BoolValue(true))
  ])
  
  // 准备仪表板数据
  let dashboard_data = DashboardData::new(
    "system_performance_dashboard",
    "System Performance Overview",
    [
      WidgetData::new("cpu_chart", "CPU Usage", [
        DataPoint::new_with_attributes("cpu", [
          ("timestamp", IntValue(Time::now() - 3600000)),
          ("value", FloatValue(75.5))
        ]),
        DataPoint::new_with_attributes("cpu", [
          ("timestamp", IntValue(Time::now() - 1800000)),
          ("value", FloatValue(82.3))
        ]),
        DataPoint::new_with_attributes("cpu", [
          ("timestamp", IntValue(Time::now())),
          ("value", FloatValue(68.9))
        ])
      ]),
      WidgetData::new("memory_chart", "Memory Usage", [
        DataPoint::new_with_attributes("memory", [
          ("timestamp", IntValue(Time::now() - 3600000)),
          ("value", FloatValue(65.2))
        ]),
        DataPoint::new_with_attributes("memory", [
          ("timestamp", IntValue(Time::now() - 1800000)),
          ("value", FloatValue(71.8))
        ]),
        DataPoint::new_with_attributes("memory", [
          ("timestamp", IntValue(Time::now())),
          ("value", FloatValue(59.4))
        ])
      ])
    ]
  )
  
  // 测试PNG导出
  let png_export_start = Time::now()
  let png_export_result = ExportManager::export_to_png(export_manager, dashboard_data, 1920, 1080)
  let png_export_end = Time::now()
  let png_export_duration = png_export_end - png_export_start
  
  match png_export_result {
    Ok(png_data) => {
      assert_true(png_data.length() > 0)
      assert_true(png_export_duration < 10000) // PNG导出应在10秒内完成
    }
    Err(_) => assert_true(false)
  }
  
  // 测试SVG导出
  let svg_export_result = ExportManager::export_to_svg(export_manager, dashboard_data)
  match svg_export_result {
    Ok(svg_data) => {
      assert_true(svg_data.length() > 0)
      assert_true(svg_data.contains("<svg"))
      assert_true(svg_data.contains("</svg>"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试PDF导出
  let pdf_export_result = ExportManager::export_to_pdf(export_manager, dashboard_data, "A4", "landscape")
  match pdf_export_result {
    Ok(pdf_data) => {
      assert_true(pdf_data.length() > 0)
      assert_true(pdf_data.contains("%PDF"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试JSON导出
  let json_export_result = ExportManager::export_to_json(export_manager, dashboard_data)
  match json_export_result {
    Ok(json_data) => {
      assert_true(json_data.length() > 0)
      assert_true(json_data.contains("system_performance_dashboard"))
      assert_true(json_data.contains("cpu_chart"))
      assert_true(json_data.contains("memory_chart"))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试分享链接生成
  let sharing_config = SharingConfig::new(
    "system_performance_dashboard",
    ["admin", "analyst"],
    Time::now() + (30 * 24 * 60 * 60 * 1000), // 30天后过期
    "secure_share_password_123",
    true // 允许下载
  )
  
  let share_link_result = SharingManager::create_share_link(sharing_manager, sharing_config)
  match share_link_result {
    Ok(share_link) => {
      assert_true(share_link.length() > 0)
      assert_true(share_link.contains("share/"))
      
      // 验证分享链接访问
      let access_result = SharingManager::validate_share_access(sharing_manager, share_link, "secure_share_password_123")
      assert_true(access_result)
      
      // 测试错误密码
      let invalid_access = SharingManager::validate_share_access(sharing_manager, share_link, "wrong_password")
      assert_false(invalid_access)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试嵌入代码生成
  let embed_config = EmbedConfig::new(
    "system_performance_dashboard",
    800,
    600,
    "light",
    true // 显示标题
  )
  
  let embed_code_result = SharingManager::generate_embed_code(sharing_manager, embed_config)
  match embed_code_result {
    Ok(embed_code) => {
      assert_true(embed_code.length() > 0)
      assert_true(embed_code.contains("<iframe"))
      assert_true(embed_code.contains("width=\"800\""))
      assert_true(embed_code.contains("height=\"600\""))
    }
    Err(_) => assert_true(false)
  }
  
  // 测试分享权限管理
  let permission_update = SharingManager::update_share_permissions(
    sharing_manager,
    "system_performance_dashboard",
    ["admin", "analyst", "viewer"] // 添加viewer权限
  )
  assert_true(permission_update)
  
  let current_permissions = SharingManager::get_share_permissions(sharing_manager, "system_performance_dashboard")
  assert_true(current_permissions.contains("viewer"))
}

// Test 6: 响应式设计和多设备支持
test "responsive design and multi-device support" {
  // 创建响应式设计管理器
  let responsive_manager = ResponsiveManager::new()
  
  // 配置响应式参数
  ResponsiveManager::configure(responsive_manager, [
    ("breakpoints.mobile", IntValue(768)),
    ("breakpoints.tablet", IntValue(1024)),
    ("breakpoints.desktop", IntValue(1440)),
    ("auto.layout.adjustment", BoolValue(true)),
    ("touch.optimization", BoolValue(true))
  ])
  
  // 创建响应式仪表板布局
  let responsive_layout = ResponsiveLayout::new("adaptive_dashboard")
  
  // 定义不同屏幕尺寸的布局配置
  let mobile_layout = GridLayoutConfig::new(
    "mobile",
    4, // 4列网格
    12, // 12行网格
    [
      GridLayout::new(0, 0, 4, 3), // 系统健康仪表盘
      GridLayout::new(0, 3, 4, 3), // CPU使用率
      GridLayout::new(0, 6, 4, 3), // 内存使用率
      GridLayout::new(0, 9, 4, 3)  // 网络状态
    ]
  )
  
  let tablet_layout = GridLayoutConfig::new(
    "tablet",
    8, // 8列网格
    8,  // 8行网格
    [
      GridLayout::new(0, 0, 4, 4),  // 系统健康仪表盘
      GridLayout::new(4, 0, 4, 4),  // CPU使用率
      GridLayout::new(0, 4, 4, 4),  // 内存使用率
      GridLayout::new(4, 4, 4, 4),  // 网络状态
      GridLayout::new(0, 8, 8, 4)   // 时间序列图表
    ]
  )
  
  let desktop_layout = GridLayoutConfig::new(
    "desktop",
    12, // 12列网格
    6,  // 6行网格
    [
      GridLayout::new(0, 0, 3, 3),   // 系统健康仪表盘
      GridLayout::new(3, 0, 3, 3),   // CPU使用率
      GridLayout::new(6, 0, 3, 3),   // 内存使用率
      GridLayout::new(9, 0, 3, 3),   // 网络状态
      GridLayout::new(0, 3, 6, 3),   // 时间序列图表
      GridLayout::new(6, 3, 6, 3)    // 详细指标表格
    ]
  )
  
  // 添加响应式布局
  assert_true(ResponsiveManager::add_layout(responsive_manager, responsive_layout, mobile_layout))
  assert_true(ResponsiveManager::add_layout(responsive_manager, responsive_layout, tablet_layout))
  assert_true(ResponsiveManager::add_layout(responsive_manager, responsive_layout, desktop_layout))
  
  // 测试屏幕尺寸检测
  let mobile_screen = ScreenInfo::new(375, 667, 2.0) // iPhone 8
  let tablet_screen = ScreenInfo::new(768, 1024, 2.0) // iPad
  let desktop_screen = ScreenInfo::new(1920, 1080, 1.0) // Desktop
  
  // 验证布局选择
  let mobile_layout_result = ResponsiveManager::get_optimal_layout(responsive_manager, mobile_screen)
  match mobile_layout_result {
    Some(layout) => assert_eq(LayoutConfig::name(layout), "mobile")
    None => assert_true(false)
  }
  
  let tablet_layout_result = ResponsiveManager::get_optimal_layout(responsive_manager, tablet_screen)
  match tablet_layout_result {
    Some(layout) => assert_eq(LayoutConfig::name(layout), "tablet")
    None => assert_true(false)
  }
  
  let desktop_layout_result = ResponsiveManager::get_optimal_layout(responsive_manager, desktop_screen)
  match desktop_layout_result {
    Some(layout) => assert_eq(LayoutConfig::name(layout), "desktop")
    None => assert_true(false)
  }
  
  // 测试响应式图表调整
  let responsive_chart = ResponsiveChart::new("responsive_performance_chart")
  
  // 配置不同屏幕尺寸的图表选项
  let mobile_chart_options = ChartOptions::new(
    false, // 隐藏图例
    false, // 隐藏网格
    true,  // 显示工具提示
    50    // 限制数据点
  )
  
  let desktop_chart_options = ChartOptions::new(
    true,  // 显示图例
    true,  // 显示网格
    true,  // 显示工具提示
    1000   // 更多数据点
  )
  
  assert_true(ResponsiveManager::set_chart_options(responsive_manager, responsive_chart, "mobile", mobile_chart_options))
  assert_true(ResponsiveManager::set_chart_options(responsive_manager, responsive_chart, "desktop", desktop_chart_options))
  
  // 验证图表选项应用
  let mobile_options = ResponsiveManager::get_chart_options(responsive_manager, responsive_chart, mobile_screen)
  match mobile_options {
    Some(options) => {
      assert_false(ChartOptions::show_legend(options))
      assert_false(ChartOptions::show_grid(options))
      assert_eq(ChartOptions::data_points_limit(options), 50)
    }
    None => assert_true(false)
  }
  
  let desktop_options = ResponsiveManager::get_chart_options(responsive_manager, responsive_chart, desktop_screen)
  match desktop_options {
    Some(options) => {
      assert_true(ChartOptions::show_legend(options))
      assert_true(ChartOptions::show_grid(options))
      assert_eq(ChartOptions::data_points_limit(options), 1000)
    }
    None => assert_true(false)
  }
  
  // 测试触摸优化
  let touch_optimization = TouchOptimization::new()
  TouchOptimization::configure(touch_optimization, [
    ("touch.target.size", IntValue(44)), // 44px最小触摸目标
    ("gesture.enabled", BoolValue(true)),
    ("scroll.momentum", BoolValue(true))
  ])
  
  let touch_optimized_chart = ResponsiveManager::apply_touch_optimization(responsive_manager, responsive_chart, touch_optimization)
  assert_true(touch_optimized_chart !== responsive_chart) // 应该返回优化后的图表
  
  // 验证触摸优化配置
  let touch_config = ResponsiveManager::get_touch_config(responsive_manager, touch_optimized_chart)
  assert_true(TouchConfig::touch_target_size(touch_config) >= 44)
  assert_true(TouchConfig::gesture_enabled(touch_config))
  
  // 测试性能优化
  let performance_start = Time::now()
  let responsive_render_result = ResponsiveManager::render_responsive_dashboard(
    responsive_manager,
    "adaptive_dashboard",
    mobile_screen
  )
  let performance_end = Time::now()
  let render_duration = performance_end - performance_start
  
  assert_true(responsive_render_result)
  assert_true(render_duration < 3000) // 响应式渲染应在3秒内完成
  
  // 验证渲染统计
  let render_stats = ResponsiveManager::get_render_stats(responsive_manager)
  assert_true(RenderStats::layout_switch_time(render_stats) < 1000) // 布局切换应在1秒内完成
  assert_true(RenderStats::chart_resize_time(render_stats) < 500)   // 图表调整应在0.5秒内完成
}