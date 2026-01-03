// Data Visualization Tests for Azimuth Telemetry System
// This file contains test cases for data visualization functionality

// Test 1: Chart Generation
test "chart generation" {
  let chart_data = [
    ("Jan", 65.0),
    ("Feb", 75.0),
    ("Mar", 82.0),
    ("Apr", 73.0),
    ("May", 88.0),
    ("Jun", 92.0)
  ]
  
  // Test line chart generation
  let line_chart = ChartGenerator::create_line_chart(
    chart_data,
    "Monthly Metrics",
    "Month",
    "Value"
  )
  
  assert_eq(line_chart.title, "Monthly Metrics")
  assert_eq(line_chart.x_axis_label, "Month")
  assert_eq(line_chart.y_axis_label, "Value")
  assert_eq(line_chart.data_points.length(), 6)
  
  // Test bar chart generation
  let bar_chart = ChartGenerator::create_bar_chart(
    chart_data,
    "Monthly Metrics",
    "Month",
    "Value"
  )
  
  assert_eq(bar_chart.title, "Monthly Metrics")
  assert_eq(bar_chart.data_points.length(), 6)
  
  // Test pie chart generation
  let pie_data = [
    ("Category A", 30.0),
    ("Category B", 25.0),
    ("Category C", 20.0),
    ("Category D", 15.0),
    ("Category E", 10.0)
  ]
  
  let pie_chart = ChartGenerator::create_pie_chart(pie_data, "Distribution")
  assert_eq(pie_chart.title, "Distribution")
  assert_eq(pie_chart.segments.length(), 5)
  
  // Test area chart generation
  let area_chart = ChartGenerator::create_area_chart(
    chart_data,
    "Monthly Metrics",
    "Month",
    "Value"
  )
  
  assert_eq(area_chart.title, "Monthly Metrics")
  assert_eq(area_chart.data_points.length(), 6)
}

// Test 2: Real-time Dashboard Updates
test "real-time dashboard updates" {
  let dashboard = Dashboard::new("Telemetry Dashboard")
  
  // Test widget creation
  let metric_widget = DashboardWidget::metric_chart("cpu_usage", "CPU Usage", "percentage")
  Dashboard::add_widget(dashboard, metric_widget)
  
  let gauge_widget = DashboardWidget::gauge("memory_usage", "Memory Usage", 0.0, 100.0)
  Dashboard::add_widget(dashboard, gauge_widget)
  
  let table_widget = DashboardWidget::table("top_processes", "Top Processes", ["Name", "CPU", "Memory"])
  Dashboard::add_widget(dashboard, table_widget)
  
  // Test widget data updates
  Dashboard::update_widget_data(dashboard, "cpu_usage", [45.0, 50.0, 55.0, 48.0, 52.0])
  Dashboard::update_widget_data(dashboard, "memory_usage", 67.5)
  
  let table_data = [
    ["chrome.exe", "15.2", "256MB"],
    ["firefox.exe", "12.8", "198MB"],
    ["code.exe", "8.5", "512MB"]
  ]
  Dashboard::update_widget_data(dashboard, "top_processes", table_data)
  
  // Test dashboard snapshot
  let snapshot = Dashboard::get_snapshot(dashboard)
  assert_eq(snapshot.widgets.length(), 3)
  
  // Test widget removal
  Dashboard::remove_widget(dashboard, "top_processes")
  let updated_snapshot = Dashboard::get_snapshot(dashboard)
  assert_eq(updated_snapshot.widgets.length(), 2)
  
  // Test dashboard layout
  Dashboard::set_layout(dashboard, GridLayout::new(2, 2))
  let layout = Dashboard::get_layout(dashboard)
  assert_eq(layout.rows, 2)
  assert_eq(layout.columns, 2)
}

// Test 3: Data Aggregation for Visualization
test "data aggregation for visualization" {
  let raw_data = [
    (1640995200L, 10.5), // 2022-01-01 00:00:00
    (1640995260L, 12.3),
    (1640995320L, 11.8),
    (1640995380L, 13.2),
    (1640995440L, 14.7),
    (1640995500L, 13.9),
    (1640995560L, 15.1),
    (1640995620L, 14.3),
    (1640995680L, 16.2),
    (1640995740L, 15.8)
  ]
  
  // Test time-based aggregation (5-minute intervals)
  let aggregated_5min = DataAggregator::aggregate_by_time_interval(raw_data, 300) // 5 minutes
  assert_eq(aggregated_5min.length(), 2)
  
  // Test statistical aggregation
  let stats = DataAggregator::calculate_statistics(raw_data)
  assert_eq(stats.count, 10)
  assert_eq(stats.min, 10.5)
  assert_eq(stats.max, 16.2)
  assert_true(stats.average > 13.0 && stats.average < 14.0)
  
  // Test percentile calculation
  let p50 = DataAggregator::calculate_percentile(raw_data, 50.0)
  let p90 = DataAggregator::calculate_percentile(raw_data, 90.0)
  let p95 = DataAggregator::calculate_percentile(raw_data, 95.0)
  
  assert_true(p50 > 13.0 && p50 < 14.0)
  assert_true(p90 > 15.0 && p90 < 16.0)
  assert_true(p95 > 15.0 && p95 < 16.5)
  
  // Test moving average
  let moving_avg = DataAggregator::calculate_moving_average(raw_data, 3)
  assert_eq(moving_avg.length(), 8) // 10 - 3 + 1
  
  // Test data sampling
  let sampled_data = DataAggregator::sample_data(raw_data, 3)
  assert_eq(sampled_data.length(), 4) // ceil(10 / 3)
}

// Test 4: Color Palette and Theming
test "color palette and theming" {
  // Test default color palette
  let default_palette = ColorPalette::default()
  assert_eq(default_palette.colors.length(), 10)
  
  // Test custom color palette
  let custom_colors = ["#FF5733", "#33FF57", "#3357FF", "#F3FF33", "#FF33F3"]
  let custom_palette = ColorPalette::new(custom_colors)
  assert_eq(custom_palette.colors.length(), 5)
  
  // Test color generation
  let generated_colors = ColorPalette::generate_colors(8, ColorScheme::Qualitative)
  assert_eq(generated_colors.length(), 8)
  
  // Test theme creation
  let light_theme = Theme::new("Light", "#FFFFFF", "#000000", custom_palette)
  let dark_theme = Theme::new("Dark", "#000000", "#FFFFFF", custom_palette)
  
  assert_eq(light_theme.name, "Light")
  assert_eq(light_theme.background_color, "#FFFFFF")
  assert_eq(light_theme.text_color, "#000000")
  
  assert_eq(dark_theme.name, "Dark")
  assert_eq(dark_theme.background_color, "#000000")
  assert_eq(dark_theme.text_color, "#FFFFFF")
  
  // Test theme application
  let chart = Chart::new("Test Chart")
  let themed_chart = Theme::apply_to_chart(light_theme, chart)
  assert_eq(themed_chart.background_color, "#FFFFFF")
  assert_eq(themed_chart.text_color, "#000000")
  
  // Test color accessibility
  let contrast_ratio = ColorUtils::calculate_contrast_ratio("#FFFFFF", "#000000")
  assert_true(contrast_ratio > 7.0) // High contrast
  
  let is_accessible = ColorUtils::is_accessible_combination("#FFFFFF", "#000000")
  assert_true(is_accessible)
}

// Test 5: Interactive Visualizations
test "interactive visualizations" {
  let interactive_chart = InteractiveChart::new("Interactive Metrics")
  
  // Test tooltip configuration
  InteractiveChart::enable_tooltips(interactive_chart, TooltipConfig::new(
    "default",
    true,
    ["value", "timestamp"]
  ))
  
  // Test zoom functionality
  InteractiveChart::enable_zoom(interactive_chart, ZoomConfig::new(
    true,
    true,
    0.1,
    10.0
  ))
  
  // Test pan functionality
  InteractiveChart::enable_pan(interactive_chart, PanConfig::new(
    true,
    true,
    "drag"
  ))
  
  // Test crosshair
  InteractiveChart::enable_crosshair(interactive_chart, CrosshairConfig::new(
    true,
    "#FF0000",
    1.0,
    "dashed"
  ))
  
  // Test data point selection
  InteractiveChart::enable_selection(interactive_chart, SelectionConfig::new(
    true,
    "single",
    "#00FF00"
  ))
  
  // Test event handling
  let click_handler = InteractiveChart::add_event_handler(interactive_chart, "click", lambda { event =>
    assert_eq(event.type, "click")
    assert_true(event.data_point_index >= 0)
  })
  
  // Test annotation support
  InteractiveChart::add_annotation(interactive_chart, Annotation::new(
    "Important Event",
    1640995500L,
    15.0,
    "#FF0000"
  ))
  
  let annotations = InteractiveChart::get_annotations(interactive_chart)
  assert_eq(annotations.length(), 1)
  
  // Test export functionality
  let export_config = ExportConfig::new(
    "png",
    1920,
    1080,
    2.0
  )
  
  let export_result = InteractiveChart::export(interactive_chart, export_config)
  assert_true(export_result.is_success)
}

// Test 6: Map-based Visualizations
test "map-based visualizations" {
  let map_viz = MapVisualization::new("Geographic Distribution")
  
  // Test map layer addition
  let base_layer = MapLayer::new("Base", LayerType::BaseMap)
  MapVisualization::add_layer(map_viz, base_layer)
  
  // Test marker layer
  let marker_data = [
    MapMarker::new("Location 1", 40.7128, -74.0060, "New York", 100),
    MapMarker::new("Location 2", 34.0522, -118.2437, "Los Angeles", 85),
    MapMarker::new("Location 3", 41.8781, -87.6298, "Chicago", 75)
  ]
  
  let marker_layer = MapLayer::new("Markers", LayerType::Markers)
  MapLayer::add_markers(marker_layer, marker_data)
  MapVisualization::add_layer(map_viz, marker_layer)
  
  // Test heatmap layer
  let heatmap_data = [
    HeatmapPoint::new(40.7128, -74.0060, 0.8),
    HeatmapPoint::new(34.0522, -118.2437, 0.6),
    HeatmapPoint::new(41.8781, -87.6298, 0.7)
  ]
  
  let heatmap_layer = MapLayer::new("Heatmap", LayerType::Heatmap)
  MapLayer::add_heatmap_points(heatmap_layer, heatmap_data)
  MapVisualization::add_layer(map_viz, heatmap_layer)
  
  // Test clustering
  MapVisualization::enable_clustering(map_viz, ClusteringConfig::new(
    true,
    50,
    "max"
  ))
  
  // Test map controls
  MapVisualization::add_control(map_viz, MapControl::new(
    ControlType::Zoom,
    "top-right"
  ))
  
  MapVisualization::add_control(map_viz, MapControl::new(
    ControlType::LayerSwitcher,
    "top-left"
  ))
  
  // Test map bounds
  let bounds = MapVisualization::calculate_bounds(map_viz)
  assert_true(bounds.north > bounds.south)
  assert_true(bounds.east > bounds.west)
  
  // Test geospatial query
  let nearby_markers = MapVisualization::query_within_radius(
    map_viz,
    40.7128,
    -74.0060,
    100.0 // 100km
  )
  assert_true(nearby_markers.length() >= 1)
}

// Test 7: Advanced Chart Types
test "advanced chart types" {
  // Test scatter plot
  let scatter_data = [
    ScatterPoint::new(1.0, 2.0, 10),
    ScatterPoint::new(2.0, 4.0, 15),
    ScatterPoint::new(3.0, 3.0, 8),
    ScatterPoint::new(4.0, 5.0, 20),
    ScatterPoint::new(5.0, 4.5, 12)
  ]
  
  let scatter_plot = AdvancedChart::create_scatter_plot(
    scatter_data,
    "X vs Y",
    "X Value",
    "Y Value"
  )
  
  assert_eq(scatter_plot.points.length(), 5)
  assert_eq(scatter_plot.title, "X vs Y")
  
  // Test bubble chart
  let bubble_data = [
    BubblePoint::new(1.0, 2.0, 10, "Group A"),
    BubblePoint::new(2.0, 4.0, 15, "Group B"),
    BubblePoint::new(3.0, 3.0, 8, "Group A"),
    BubblePoint::new(4.0, 5.0, 20, "Group C"),
    BubblePoint::new(5.0, 4.5, 12, "Group B")
  ]
  
  let bubble_chart = AdvancedChart::create_bubble_chart(
    bubble_data,
    "Bubble Chart",
    "X Value",
    "Y Value",
    "Size"
  )
  
  assert_eq(bubble_chart.bubbles.length(), 5)
  assert_eq(bubble_chart.title, "Bubble Chart")
  
  // Test radar chart
  let radar_data = [
    ("Speed", 80),
    ("Reliability", 90),
    ("Comfort", 60),
    ("Safety", 95),
    ("Efficiency", 75)
  ]
  
  let radar_chart = AdvancedChart::create_radar_chart(
    radar_data,
    "Performance Metrics"
  )
  
  assert_eq(radar_chart.axes.length(), 5)
  assert_eq(radar_chart.title, "Performance Metrics")
  
  // Test sunburst chart
  let sunburst_data = SunburstNode::new(
    "Root",
    100,
    [
      SunburstNode::new(
        "Branch A",
        60,
        [
          SunburstNode::new("Leaf A1", 30, []),
          SunburstNode::new("Leaf A2", 30, [])
        ]
      ),
      SunburstNode::new(
        "Branch B",
        40,
        [
          SunburstNode::new("Leaf B1", 20, []),
          SunburstNode::new("Leaf B2", 20, [])
        ]
      )
    ]
  )
  
  let sunburst_chart = AdvancedChart::create_sunburst_chart(sunburst_data)
  assert_eq(sunburst_chart.root.label, "Root")
  assert_eq(sunburst_chart.root.children.length(), 2)
  
  // Test treemap
  let treemap_data = [
    TreemapNode::new("Category A", 30, []),
    TreemapNode::new("Category B", 25, []),
    TreemapNode::new(
      "Category C",
      45,
      [
        TreemapNode::new("Subcategory C1", 20, []),
        TreemapNode::new("Subcategory C2", 25, [])
      ]
    )
  ]
  
  let treemap = AdvancedChart::create_treemap(treemap_data)
  assert_eq(treemap.nodes.length(), 3)
  assert_eq(treemap.nodes[2].children.length(), 2)
}

// Test 8: Data Export and Sharing
test "data export and sharing" {
  let dashboard = Dashboard::new("Export Test Dashboard")
  
  // Test widget creation with data
  let chart_widget = DashboardWidget::metric_chart("test_chart", "Test Chart", "value")
  Dashboard::add_widget(dashboard, chart_widget)
  Dashboard::update_widget_data(dashboard, "test_chart", [
    (1, 10.0),
    (2, 15.0),
    (3, 12.0),
    (4, 18.0),
    (5, 20.0)
  ])
  
  // Test PNG export
  let png_export = Dashboard::export_as_png(dashboard, 1920, 1080)
  assert_true(png_export.is_success)
  assert_true(png_export.data.length() > 0)
  
  // Test JPEG export
  let jpeg_export = Dashboard::export_as_jpeg(dashboard, 1920, 1080, 0.9)
  assert_true(jpeg_export.is_success)
  assert_true(jpeg_export.data.length() > 0)
  
  // Test SVG export
  let svg_export = Dashboard::export_as_svg(dashboard)
  assert_true(svg_export.is_success)
  assert_true(svg_export.data.contains("<svg"))
  
  // Test PDF export
  let pdf_export = Dashboard::export_as_pdf(dashboard, "A4", "landscape")
  assert_true(pdf_export.is_success)
  assert_true(pdf_export.data.length() > 0)
  
  // Test data export (CSV)
  let csv_export = Dashboard::export_data_as_csv(dashboard)
  assert_true(csv_export.is_success)
  assert_true(csv_export.data.contains("test_chart"))
  
  // Test data export (JSON)
  let json_export = Dashboard::export_data_as_json(dashboard)
  assert_true(json_export.is_success)
  assert_true(json_export.data.contains("\"widgets\""))
  
  // Test sharing link generation
  let share_config = ShareConfig::new(
    true, // enabled
    24,   // expires in 24 hours
    true, // allow download
    false // password protected
  )
  
  let share_link = Dashboard::generate_share_link(dashboard, share_config)
  assert_true(share_link.is_success)
  assert_true(share_link.url.contains("dashboard"))
  assert_true(share_link.token.length() > 0)
}

// Test 9: Responsive Design
test "responsive design" {
  let dashboard = Dashboard::new("Responsive Dashboard")
  
  // Test responsive breakpoints
  let breakpoints = ResponsiveConfig::new(
    768,   // tablet breakpoint
    1024,  // desktop breakpoint
    1440   // large desktop breakpoint
  )
  
  Dashboard::set_responsive_config(dashboard, breakpoints)
  
  // Test layout adaptation
  let mobile_layout = Dashboard::get_layout_for_screen_size(dashboard, 375) // iPhone
  assert_eq(mobile_layout.columns, 1)
  assert_eq(mobile_layout.rows, 3)
  
  let tablet_layout = Dashboard::get_layout_for_screen_size(dashboard, 768) // iPad
  assert_eq(tablet_layout.columns, 2)
  assert_eq(tablet_layout.rows, 2)
  
  let desktop_layout = Dashboard::get_layout_for_screen_size(dashboard, 1920) // Desktop
  assert_eq(desktop_layout.columns, 3)
  assert_eq(desktop_layout.rows, 1)
  
  // Test widget adaptation
  let chart_widget = DashboardWidget::metric_chart("responsive_chart", "Responsive Chart", "value")
  Dashboard::add_widget(dashboard, chart_widget)
  
  let mobile_config = WidgetConfig::get_for_screen_size("mobile")
  assert_eq(mobile_config.height, 200)
  assert_eq(mobile_config.show_legend, false)
  
  let desktop_config = WidgetConfig::get_for_screen_size("desktop")
  assert_eq(desktop_config.height, 400)
  assert_eq(desktop_config.show_legend, true)
  
  // Test touch interaction
  Dashboard::enable_touch_interaction(dashboard, true)
  let touch_config = Dashboard::get_touch_config(dashboard)
  assert_true(touch_config.enabled)
  assert_eq(touch_config.tap_threshold, 10)
  assert_eq(touch_config.swipe_threshold, 50)
  
  // Test print optimization
  Dashboard::optimize_for_print(dashboard)
  let print_config = Dashboard::get_print_config(dashboard)
  assert_true(print_config.optimized)
  assert_eq(print_config.paper_size, "A4")
  assert_eq(print_config.orientation, "portrait")
}

// Test 10: Performance Optimization for Visualizations
test "performance optimization for visualizations" {
  let large_dataset = Array::new(10000, 0)
  
  // Generate large dataset
  for i = 0; i < large_dataset.length(); i = i + 1 {
    large_dataset[i] = (i, Random::float() * 100.0)
  }
  
  // Test data sampling for performance
  let sampled_data = PerformanceOptimizer::sample_data(large_dataset, 1000)
  assert_eq(sampled_data.length(), 1000)
  
  // Test lazy loading
  let lazy_chart = LazyChart::new("Lazy Chart")
  LazyChart::set_data_source(lazy_chart, large_dataset)
  
  assert_false(LazyChart::is_data_loaded(lazy_chart))
  
  let visible_data = LazyChart::get_visible_data(lazy_chart, 0, 1000)
  assert_eq(visible_data.length(), 1000)
  assert_true(LazyChart::is_data_loaded(lazy_chart))
  
  // Test virtual scrolling
  let virtual_chart = VirtualChart::new("Virtual Chart")
  VirtualChart::set_data_source(virtual_chart, large_dataset)
  VirtualChart::set_viewport_size(virtual_chart, 100)
  
  let viewport_data = VirtualChart::get_viewport_data(virtual_chart, 5000)
  assert_eq(viewport_data.length(), 100)
  
  // Test canvas rendering optimization
  let canvas_chart = CanvasChart::new("Canvas Chart", 1920, 1080)
  CanvasChart::enable_redraw_optimization(canvas_chart, true)
  
  let render_start_time = Time::now()
  CanvasChart::render(canvas_chart, large_dataset)
  let render_end_time = Time::now()
  
  let render_time = render_end_time - render_start_time
  assert_true(render_time < 1000) // Should render in less than 1 second
  
  // Test WebGL acceleration
  let webgl_chart = WebGLChart::new("WebGL Chart")
  WebGLChart::enable_acceleration(webgl_chart, true)
  
  let webgl_start_time = Time::now()
  WebGLChart::render(webgl_chart, large_dataset)
  let webgl_end_time = Time::now()
  
  let webgl_render_time = webgl_end_time - webgl_start_time
  assert_true(webgl_render_time < render_time) // WebGL should be faster
  
  // Test memory management
  let memory_before = MemoryMonitor::get_usage()
  
  // Create and destroy multiple charts
  for i = 0; i < 10; i = i + 1 {
    let temp_chart = Chart::new("Temp Chart " + i.to_string())
    Chart::set_data(temp_chart, large_dataset)
    Chart::render(temp_chart)
    Chart::destroy(temp_chart) // Explicit cleanup
  }
  
  let memory_after = MemoryMonitor::get_usage()
  let memory_increase = memory_after - memory_before
  
  // Memory increase should be minimal (less than 50MB)
  assert_true(memory_increase < 50 * 1024 * 1024)
}