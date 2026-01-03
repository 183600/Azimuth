// Azimuth Telemetry System - Data Visualization Dashboard Tests
// This file contains test cases for data visualization dashboard functionality

// Test 1: Dashboard Creation and Configuration
test "dashboard creation and configuration" {
  // Create dashboard manager
  let dashboard_manager = DashboardManager::new()
  
  // Create dashboard
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, "System Overview")
  
  // Configure dashboard properties
  Dashboard::set_title(dashboard, "System Overview Dashboard")
  Dashboard::set_description(dashboard, "Real-time system monitoring and metrics")
  Dashboard::set_refresh_interval(dashboard, 30)  // 30 seconds
  Dashboard::set_theme(dashboard, DashboardTheme::Dark)
  Dashboard::set_layout(dashboard, DashboardLayout::Grid)
  
  // Verify dashboard properties
  assert_eq(Dashboard::title(dashboard), "System Overview Dashboard")
  assert_eq(Dashboard::description(dashboard), "Real-time system monitoring and metrics")
  assert_eq(Dashboard::refresh_interval(dashboard), 30)
  assert_eq(Dashboard::theme(dashboard), DashboardTheme::Dark)
  assert_eq(Dashboard::layout(dashboard), DashboardLayout::Grid)
  
  // Add widgets to dashboard
  let cpu_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  let memory_widget = Widget::create_metric_widget("Memory Usage", MetricType::Gauge)
  let request_widget = Widget::create_chart_widget("Request Rate", ChartType::Line)
  let error_widget = Widget::create_chart_widget("Error Rate", ChartType::Bar)
  
  Dashboard::add_widget(dashboard, cpu_widget, Position::new(0, 0, 6, 4))
  Dashboard::add_widget(dashboard, memory_widget, Position::new(6, 0, 6, 4))
  Dashboard::add_widget(dashboard, request_widget, Position::new(0, 4, 6, 4))
  Dashboard::add_widget(dashboard, error_widget, Position::new(6, 4, 6, 4))
  
  // Verify widgets are added
  assert_eq(Dashboard::widget_count(dashboard), 4)
  assert_true(Dashboard::has_widget(dashboard, cpu_widget))
  assert_true(Dashboard::has_widget(dashboard, memory_widget))
  assert_true(Dashboard::has_widget(dashboard, request_widget))
  assert_true(Dashboard::has_widget(dashboard, error_widget))
  
  // Test widget positioning
  let cpu_position = Dashboard::get_widget_position(dashboard, cpu_widget)
  assert_eq(cpu_position.x, 0)
  assert_eq(cpu_position.y, 0)
  assert_eq(cpu_position.width, 6)
  assert_eq(cpu_position.height, 4)
  
  // Test dashboard serialization
  let dashboard_json = Dashboard::to_json(dashboard)
  assert_true(dashboard_json.contains("System Overview Dashboard"))
  assert_true(dashboard_json.contains("CPU Usage"))
  assert_true(dashboard_json.contains("Memory Usage"))
  
  // Test dashboard deserialization
  let restored_dashboard = Dashboard::from_json(dashboard_json)
  assert_eq(Dashboard::title(restored_dashboard), "System Overview Dashboard")
  assert_eq(Dashboard::widget_count(restored_dashboard), 4)
}

// Test 2: Widget Types and Data Binding
test "widget types and data binding" {
  // Create various widget types
  let gauge_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  let line_chart_widget = Widget::create_chart_widget("Response Time", ChartType::Line)
  let bar_chart_widget = Widget::create_chart_widget("Request Count", ChartType::Bar)
  let pie_chart_widget = Widget::create_chart_widget("Status Distribution", ChartType::Pie)
  let table_widget = Widget::create_table_widget("Recent Events")
  let text_widget = Widget::create_text_widget("System Status")
  let heatmap_widget = Widget::create_heatmap_widget("Activity Heatmap")
  
  // Configure gauge widget
  Widget::set_data_source(gauge_widget, "metrics.cpu.usage")
  Widget::set_range(gauge_widget, 0.0, 100.0)
  Widget::set_unit(gauge_widget, "%")
  Widget::set_thresholds(gauge_widget, [
    Threshold::new(70.0, Color::Yellow),
    Threshold::new(90.0, Color::Red)
  ])
  
  // Configure line chart widget
  Widget::set_data_source(line_chart_widget, "timeseries.response_time")
  Widget::set_time_range(line_chart_widget, TimeRange::LastHour)
  Widget::set_aggregation(line_chart_widget, AggregationType::Average)
  Widget::add_series(line_chart_widget, Series::new("p50", Color::Blue))
  Widget::add_series(line_chart_widget, Series::new("p95", Color::Red))
  Widget::add_series(line_chart_widget, Series::new("p99", Color::Orange))
  
  // Configure bar chart widget
  Widget::set_data_source(bar_chart_widget, "metrics.request_count")
  Widget::set_grouping(bar_chart_widget, GroupingType::ByService)
  Widget::set_sort_order(bar_chart_widget, SortOrder::Descending)
  
  // Configure pie chart widget
  Widget::set_data_source(pie_chart_widget, "metrics.status_distribution")
  Widget::set_colors(pie_chart_widget, [
    Color::Green,  // Success
    Color::Yellow, // Warning
    Color::Red     // Error
  ])
  
  // Configure table widget
  Widget::set_data_source(table_widget, "events.recent")
  Widget::add_column(table_widget, TableColumn::new("Timestamp", "timestamp", "datetime"))
  Widget::add_column(table_widget, TableColumn::new("Level", "level", "string"))
  Widget::add_column(table_widget, TableColumn::new("Message", "message", "string"))
  Widget::set_page_size(table_widget, 10)
  
  // Configure text widget
  Widget::set_data_source(text_widget, "status.system")
  Widget::set_template(text_widget, "System Status: {{status}} - Last updated: {{timestamp}}")
  
  // Configure heatmap widget
  Widget::set_data_source(heatmap_widget, "heatmap.activity")
  Widget::set_x_axis(heatmap_widget, "hour_of_day")
  Widget::set_y_axis(heatmap_widget, "day_of_week")
  Widget::set_color_scale(heatmap_widget, ColorScale::Viridis)
  
  // Test widget properties
  assert_eq(Widget::type(gauge_widget), WidgetType::Metric)
  assert_eq(Widget::type(line_chart_widget), WidgetType::Chart)
  assert_eq(Widget::type(bar_chart_widget), WidgetType::Chart)
  assert_eq(Widget::type(pie_chart_widget), WidgetType::Chart)
  assert_eq(Widget::type(table_widget), WidgetType::Table)
  assert_eq(Widget::type(text_widget), WidgetType::Text)
  assert_eq(Widget::type(heatmap_widget), WidgetType::Heatmap)
  
  // Test data binding
  assert_eq(Widget::data_source(gauge_widget), "metrics.cpu.usage")
  assert_eq(Widget::data_source(line_chart_widget), "timeseries.response_time")
  assert_eq(Widget::data_source(bar_chart_widget), "metrics.request_count")
  
  // Test widget-specific properties
  match Widget::get_metric_config(gauge_widget) {
    Some(config) => {
      assert_eq(config.metric_type, MetricType::Gauge)
      assert_eq(config.min_value, 0.0)
      assert_eq(config.max_value, 100.0)
      assert_eq(config.unit, "%")
      assert_eq(config.thresholds.length(), 2)
    }
    None => assert_true(false)
  }
  
  match Widget::get_chart_config(line_chart_widget) {
    Some(config) => {
      assert_eq(config.chart_type, ChartType::Line)
      assert_eq(config.time_range, TimeRange::LastHour)
      assert_eq(config.aggregation, AggregationType::Average)
      assert_eq(config.series.length(), 3)
    }
    None => assert_true(false)
  }
}

// Test 3: Real-time Data Updates
test "real-time data updates" {
  // Create dashboard with real-time updates
  let dashboard = Dashboard::new("Real-time Monitoring")
  Dashboard::set_refresh_interval(dashboard, 5)  // 5 seconds
  
  // Create real-time widgets
  let cpu_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  let memory_widget = Widget::create_metric_widget("Memory Usage", MetricType::Gauge)
  let request_widget = Widget::create_chart_widget("Request Rate", ChartType::Line)
  
  Widget::set_real_time(cpu_widget, true)
  Widget::set_real_time(memory_widget, true)
  Widget::set_real_time(request_widget, true)
  
  Dashboard::add_widget(dashboard, cpu_widget, Position::new(0, 0, 4, 4))
  Dashboard::add_widget(dashboard, memory_widget, Position::new(4, 0, 4, 4))
  Dashboard::add_widget(dashboard, request_widget, Position::new(0, 4, 8, 4))
  
  // Create data provider
  let data_provider = DataProvider::new()
  
  // Mock real-time data
  DataProvider::mock_time_series(data_provider, "metrics.cpu.usage", || {
    // Simulate CPU usage fluctuating between 20% and 80%
    let base_value = 50.0
    let variation = 30.0 * @sin(@time() / 10000.0)  // 10 second period
    base_value + variation
  })
  
  DataProvider::mock_time_series(data_provider, "metrics.memory.usage", || {
    // Simulate memory usage gradually increasing
    let base_value = 40.0
    let growth = (@time() / 60000.0) % 60.0  // 1 minute cycle
    base_value + growth
  })
  
  DataProvider::mock_time_series(data_provider, "metrics.request_rate", || {
    // Simulate request rate with spikes
    let base_rate = 100.0
    let spike = if @time() % 30000 < 5000 { 200.0 } else { 0.0 }  // 5 second spike every 30 seconds
    base_rate + spike
  })
  
  // Start real-time updates
  Dashboard::start_real_time_updates(dashboard, data_provider)
  
  // Wait for initial data load
  @sleep(1000)
  
  // Verify widgets have data
  let cpu_data = Widget::get_current_data(cpu_widget)
  let memory_data = Widget::get_current_data(memory_widget)
  let request_data = Widget::get_current_data(request_widget)
  
  match cpu_data {
    Some(data) => {
      assert_true(data.value >= 20.0)
      assert_true(data.value <= 80.0)
      assert_true(data.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  match memory_data {
    Some(data) => {
      assert_true(data.value >= 40.0)
      assert_true(data.value <= 100.0)
      assert_true(data.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  match request_data {
    Some(data) => {
      assert_true(data.value >= 100.0)
      assert_true(data.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // Test data history for charts
  let request_history = Widget::get_data_history(request_widget, 10)  // Last 10 data points
  assert_eq(request_history.length(), 10)
  
  // Verify timestamps are increasing
  for i in 1..=9 {
    assert_true(request_history[i].timestamp > request_history[i-1].timestamp)
  }
  
  // Test update callbacks
  let mut update_count = 0
  Widget::on_update(cpu_widget, || {
    update_count = update_count + 1
  })
  
  // Wait for more updates
  @sleep(6000)  // Wait for at least one refresh cycle
  
  // Verify update callbacks were called
  assert_true(update_count > 0)
  
  // Stop real-time updates
  Dashboard::stop_real_time_updates(dashboard)
}

// Test 4: Dashboard Interactions and Filtering
test "dashboard interactions and filtering" {
  // Create interactive dashboard
  let dashboard = Dashboard::new("Interactive Dashboard")
  
  // Create filterable widgets
  let service_chart = Widget::create_chart_widget("Service Performance", ChartType::Line)
  let error_chart = Widget::create_chart_widget("Error Rate", ChartType::Bar)
  let log_table = Widget::create_table_widget("Service Logs")
  
  // Configure filters
  let service_filter = Filter::new("service", FilterType::MultiSelect)
  Filter::add_option(service_filter, "user-service")
  Filter::add_option(service_filter, "order-service")
  Filter::add_option(service_filter, "payment-service")
  Filter::set_default(service_filter, ["user-service", "order-service"])
  
  let time_filter = Filter::new("time_range", FilterType::TimeRange)
  Filter::add_option(time_filter, "Last 15 minutes")
  Filter::add_option(time_filter, "Last hour")
  Filter::add_option(time_filter, "Last 24 hours")
  Filter::set_default(time_filter, "Last hour")
  
  let level_filter = Filter::new("log_level", FilterType::MultiSelect)
  Filter::add_option(level_filter, "ERROR")
  Filter::add_option(level_filter, "WARN")
  Filter::add_option(level_filter, "INFO")
  Filter::set_default(level_filter, ["ERROR", "WARN"])
  
  // Add filters to dashboard
  Dashboard::add_filter(dashboard, service_filter)
  Dashboard::add_filter(dashboard, time_filter)
  Dashboard::add_filter(dashboard, level_filter)
  
  // Add widgets to dashboard
  Dashboard::add_widget(dashboard, service_chart, Position::new(0, 0, 8, 4))
  Dashboard::add_widget(dashboard, error_chart, Position::new(0, 4, 4, 4))
  Dashboard::add_widget(dashboard, log_table, Position::new(4, 4, 4, 4))
  
  // Configure widget data sources with filter bindings
  Widget::set_data_source(service_chart, "metrics.service_performance")
  Widget::add_filter_binding(service_chart, "service")
  Widget::add_filter_binding(service_chart, "time_range")
  
  Widget::set_data_source(error_chart, "metrics.error_rate")
  Widget::add_filter_binding(error_chart, "service")
  Widget::add_filter_binding(error_chart, "time_range")
  
  Widget::set_data_source(log_table, "logs.service")
  Widget::add_filter_binding(log_table, "service")
  Widget::add_filter_binding(log_table, "log_level")
  Widget::add_filter_binding(log_table, "time_range")
  
  // Test initial filter values
  let current_filters = Dashboard::get_current_filters(dashboard)
  assert_eq(current_filters.get("service"), Some(["user-service", "order-service"]))
  assert_eq(current_filters.get("time_range"), Some(["Last hour"]))
  assert_eq(current_filters.get("log_level"), Some(["ERROR", "WARN"]))
  
  // Test filter application
  Dashboard::set_filter_value(dashboard, "service", ["payment-service"])
  
  let updated_filters = Dashboard::get_current_filters(dashboard)
  assert_eq(updated_filters.get("service"), Some(["payment-service"]))
  
  // Test widget data updates based on filters
  let service_data = Widget::get_filtered_data(service_chart)
  match service_data {
    Some(data) => {
      // Data should be filtered by payment-service only
      assert_true(data.filters.contains("service=payment-service"))
    }
    None => assert_true(false)
  }
  
  // Test multiple filter combinations
  Dashboard::set_filter_value(dashboard, "service", ["user-service", "payment-service"])
  Dashboard::set_filter_value(dashboard, "time_range", ["Last 15 minutes"])
  Dashboard::set_filter_value(dashboard, "log_level", ["ERROR"])
  
  let multi_filtered_data = Widget::get_filtered_data(log_table)
  match multi_filtered_data {
    Some(data) => {
      assert_true(data.filters.contains("service=user-service"))
      assert_true(data.filters.contains("service=payment-service"))
      assert_true(data.filters.contains("time_range=Last 15 minutes"))
      assert_true(data.filters.contains("log_level=ERROR"))
    }
    None => assert_true(false)
  }
  
  // Test filter presets
  let production_preset = FilterPreset::new("Production View")
  FilterPreset::set_filter_value(production_preset, "service", ["user-service", "order-service", "payment-service"])
  FilterPreset::set_filter_value(production_preset, "time_range", ["Last 24 hours"])
  FilterPreset::set_filter_value(production_preset, "log_level", ["ERROR", "WARN"])
  
  Dashboard::add_filter_preset(dashboard, production_preset)
  
  // Apply preset
  Dashboard::apply_filter_preset(dashboard, "Production View")
  
  let preset_filters = Dashboard::get_current_filters(dashboard)
  assert_eq(preset_filters.get("service"), Some(["user-service", "order-service", "payment-service"]))
  assert_eq(preset_filters.get("time_range"), Some(["Last 24 hours"]))
  assert_eq(preset_filters.get("log_level"), Some(["ERROR", "WARN"]))
  
  // Test filter callbacks
  let mut filter_change_count = 0
  Dashboard::on_filter_change(dashboard, |filter_name, old_value, new_value| {
    filter_change_count = filter_change_count + 1
    assert_eq(filter_name, "service")
  })
  
  Dashboard::set_filter_value(dashboard, "service", ["order-service"])
  
  assert_eq(filter_change_count, 1)
}

// Test 5: Dashboard Templates and Customization
test "dashboard templates and customization" {
  // Create dashboard template manager
  let template_manager = DashboardTemplateManager::new()
  
  // Create system monitoring template
  let system_template = DashboardTemplate::new("System Monitoring")
  
  // Add template widgets
  let cpu_widget = WidgetTemplate::new("CPU Usage", WidgetType::Metric)
  WidgetTemplate::set_property(cpu_widget, "metric_type", MetricType::Gauge)
  WidgetTemplate::set_property(cpu_widget, "data_source", "metrics.cpu.usage")
  WidgetTemplate::set_property(cpu_widget, "range", [0.0, 100.0])
  WidgetTemplate::set_property(cpu_widget, "unit", "%")
  
  let memory_widget = WidgetTemplate::new("Memory Usage", WidgetType::Metric)
  WidgetTemplate::set_property(memory_widget, "metric_type", MetricType::Gauge)
  WidgetTemplate::set_property(memory_widget, "data_source", "metrics.memory.usage")
  WidgetTemplate::set_property(memory_widget, "range", [0.0, 100.0])
  WidgetTemplate::set_property(memory_widget, "unit", "%")
  
  let network_widget = WidgetTemplate::new("Network I/O", WidgetType::Chart)
  WidgetTemplate::set_property(network_widget, "chart_type", ChartType::Line)
  WidgetTemplate::set_property(network_widget, "data_source", "metrics.network.io")
  WidgetTemplate::set_property(network_widget, "time_range", TimeRange::LastHour)
  
  let disk_widget = WidgetTemplate::new("Disk Usage", WidgetType::Chart)
  WidgetTemplate::set_property(disk_widget, "chart_type", ChartType::Bar)
  WidgetTemplate::set_property(disk_widget, "data_source", "metrics.disk.usage")
  
  // Add widgets to template
  DashboardTemplate::add_widget(system_template, cpu_widget, Position::new(0, 0, 6, 4))
  DashboardTemplate::add_widget(system_template, memory_widget, Position::new(6, 0, 6, 4))
  DashboardTemplate::add_widget(system_template, network_widget, Position::new(0, 4, 6, 4))
  DashboardTemplate::add_widget(system_template, disk_widget, Position::new(6, 4, 6, 4))
  
  // Set template properties
  DashboardTemplate::set_refresh_interval(system_template, 30)
  DashboardTemplate::set_theme(system_template, DashboardTheme::Dark)
  
  // Register template
  DashboardTemplateManager::register_template(template_manager, system_template)
  
  // Create dashboard from template
  let dashboard = DashboardTemplateManager::create_from_template(template_manager, "System Monitoring")
  
  // Verify dashboard properties
  assert_eq(Dashboard::refresh_interval(dashboard), 30)
  assert_eq(Dashboard::theme(dashboard), DashboardTheme::Dark)
  assert_eq(Dashboard::widget_count(dashboard), 4)
  
  // Verify widgets were created correctly
  let widgets = Dashboard::get_widgets(dashboard)
  let cpu_widget = widgets.find(@(w) Widget::title(w) == "CPU Usage").unwrap()
  let memory_widget = widgets.find(@(w) Widget::title(w) == "Memory Usage").unwrap()
  
  assert_eq(Widget::data_source(cpu_widget), "metrics.cpu.usage")
  assert_eq(Widget::data_source(memory_widget), "metrics.memory.usage")
  
  // Test template customization
  let custom_dashboard = DashboardTemplateManager::create_from_template(template_manager, "System Monitoring")
  
  // Customize CPU widget thresholds
  let custom_cpu_widget = Dashboard::get_widget_by_title(custom_dashboard, "CPU Usage").unwrap()
  Widget::set_thresholds(custom_cpu_widget, [
    Threshold::new(60.0, Color::Yellow),
    Threshold::new(85.0, Color::Red)
  ])
  
  // Add custom widget
  let custom_widget = Widget::create_metric_widget("Custom Metric", MetricType::Gauge)
  Dashboard::add_widget(custom_dashboard, custom_widget, Position::new(0, 8, 12, 4))
  
  // Verify customization
  let custom_thresholds = Widget::get_thresholds(custom_cpu_widget)
  assert_eq(custom_thresholds.length(), 2)
  assert_eq(custom_thresholds[0].value, 60.0)
  assert_eq(custom_thresholds[1].value, 85.0)
  
  assert_eq(Dashboard::widget_count(custom_dashboard), 5)
  
  // Test template inheritance
  let production_template = DashboardTemplate::new("Production Monitoring")
  DashboardTemplate::set_parent(production_template, "System Monitoring")
  
  // Override refresh interval
  DashboardTemplate::set_refresh_interval(production_template, 10)
  
  // Add additional widget
  let error_widget = WidgetTemplate::new("Error Rate", WidgetType::Chart)
  WidgetTemplate::set_property(error_widget, "chart_type", ChartType::Line)
  WidgetTemplate::set_property(error_widget, "data_source", "metrics.error.rate")
  
  DashboardTemplate::add_widget(production_template, error_widget, Position::new(0, 8, 12, 4))
  
  DashboardTemplateManager::register_template(template_manager, production_template)
  
  // Create dashboard from inherited template
  let production_dashboard = DashboardTemplateManager::create_from_template(template_manager, "Production Monitoring")
  
  // Verify inherited and overridden properties
  assert_eq(Dashboard::refresh_interval(production_dashboard), 10)  // Overridden
  assert_eq(Dashboard::theme(production_dashboard), DashboardTheme::Dark)  // Inherited
  assert_eq(Dashboard::widget_count(production_dashboard), 5)  // 4 from parent + 1 new
  
  // Test template listing and search
  let templates = DashboardTemplateManager::list_templates(template_manager)
  assert_eq(templates.length(), 2)
  assert_true(templates.contains("System Monitoring"))
  assert_true(templates.contains("Production Monitoring"))
  
  let monitoring_templates = DashboardTemplateManager::search_templates(template_manager, "monitoring")
  assert_eq(monitoring_templates.length(), 2)
}

// Test 6: Dashboard Export and Sharing
test "dashboard export and sharing" {
  // Create dashboard with various widgets
  let dashboard = Dashboard::new("Shared Dashboard")
  Dashboard::set_description(dashboard, "Dashboard for sharing with team")
  
  // Add widgets
  let cpu_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  let memory_widget = Widget::create_metric_widget("Memory Usage", MetricType::Gauge)
  let request_chart = Widget::create_chart_widget("Request Rate", ChartType::Line)
  let error_table = Widget::create_table_widget("Recent Errors")
  
  Dashboard::add_widget(dashboard, cpu_widget, Position::new(0, 0, 6, 4))
  Dashboard::add_widget(dashboard, memory_widget, Position::new(6, 0, 6, 4))
  Dashboard::add_widget(dashboard, request_chart, Position::new(0, 4, 6, 4))
  Dashboard::add_widget(dashboard, error_table, Position::new(6, 4, 6, 4))
  
  // Test dashboard export to different formats
  let json_export = Dashboard::export_to_json(dashboard)
  assert_true(json_export.contains("Shared Dashboard"))
  assert_true(json_export.contains("CPU Usage"))
  assert_true(json_export.contains("Memory Usage"))
  
  let yaml_export = Dashboard::export_to_yaml(dashboard)
  assert_true(yaml_export.contains("Shared Dashboard"))
  assert_true(yaml_export.contains("CPU Usage"))
  
  // Test dashboard import
  let imported_dashboard = Dashboard::import_from_json(json_export)
  assert_eq(Dashboard::title(imported_dashboard), "Shared Dashboard")
  assert_eq(Dashboard::widget_count(imported_dashboard), 4)
  
  // Test dashboard sharing configuration
  let share_config = ShareConfiguration::new()
  ShareConfiguration::set_public(share_config, false)
  ShareConfiguration::set_allowed_users(share_config, ["user1@example.com", "user2@example.com"])
  ShareConfiguration::set_permissions(share_config, SharePermission::View)
  ShareConfiguration::set_expiry(share_config, 7 * 24 * 60 * 60)  // 7 days
  
  // Create share link
  let share_link = Dashboard::create_share_link(dashboard, share_config)
  assert_true(share_link.contains("dashboards/share/"))
  
  // Test share link validation
  let share_token = share_link.split("/").last()
  let validation_result = Dashboard::validate_share_link(share_token)
  
  match validation_result {
    ShareValidationResult::Valid(dashboard_id) => {
      assert_true(dashboard_id.length() > 0)
    }
    ShareValidationResult::Expired => assert_true(false)
    ShareValidationResult::Invalid => assert_true(false)
  }
  
  // Test dashboard snapshot
  let snapshot = Dashboard::create_snapshot(dashboard)
  assert_eq(Snapshot::dashboard_title(snapshot), "Shared Dashboard")
  assert_eq(Snapshot::widget_count(snapshot), 4)
  assert_true(Snapshot::timestamp(snapshot) > 0)
  
  // Test snapshot data
  let snapshot_data = Snapshot::get_widget_data(snapshot, "CPU Usage")
  match snapshot_data {
    Some(data) => {
      assert_true(data.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // Test dashboard comparison
  let modified_dashboard = Dashboard::import_from_json(json_export)
  let new_widget = Widget::create_metric_widget("New Widget", MetricType::Gauge)
  Dashboard::add_widget(modified_dashboard, new_widget, Position::new(0, 8, 12, 4))
  
  let comparison = Dashboard::compare(dashboard, modified_dashboard)
  assert_eq(Comparison::added_widgets(comparison).length(), 1)
  assert_eq(Comparison::removed_widgets(comparison).length(), 0)
  assert_eq(Comparison::modified_widgets(comparison).length(), 0)
  
  // Test dashboard versioning
  let versioned_dashboard = Dashboard::create_versioned(dashboard)
  Dashboard::save_version(versioned_dashboard, "Initial version")
  
  // Modify dashboard
  Dashboard::set_title(versioned_dashboard, "Updated Dashboard")
  Dashboard::save_version(versioned_dashboard, "Updated title")
  
  let versions = Dashboard::list_versions(versioned_dashboard)
  assert_eq(versions.length(), 2)
  assert_eq(versions[0].title, "Updated Dashboard")
  assert_eq(versions[1].title, "Shared Dashboard")
  
  // Test dashboard rollback
  Dashboard::restore_version(versioned_dashboard, versions[1].id)
  assert_eq(Dashboard::title(versioned_dashboard), "Shared Dashboard")
}

// Test 7: Dashboard Performance and Optimization
test "dashboard performance and optimization" {
  // Create performance-optimized dashboard
  let dashboard = OptimizedDashboard::new("Performance Dashboard")
  
  // Configure optimization settings
  OptimizedDashboard::enable_data_caching(dashboard, 1000)  // Cache 1000 data points
  OptimizedDashboard::enable_lazy_loading(dashboard)
  OptimizedDashboard::enable_data_compression(dashboard)
  OptimizedDashboard::set_max_update_frequency(dashboard, 1000)  // Max 1 update per second
  
  // Create many widgets to test performance
  let widgets = []
  for i in 0..=49 {  // 50 widgets
    let widget = Widget::create_chart_widget("Chart " + i.to_string(), ChartType::Line)
    Widget::set_data_source(widget, "metrics.data_" + i.to_string())
    widgets.push(widget)
  }
  
  // Add widgets to dashboard in a grid layout
  for i in 0..=49 {
    let x = (i % 10) * 2
    let y = (i / 10) * 4
    Dashboard::add_widget(dashboard, widgets[i], Position::new(x, y, 2, 4))
  }
  
  // Test dashboard loading performance
  let start_time = @time()
  OptimizedDashboard::load(dashboard)
  let load_time = @time() - start_time
  
  // Loading should be fast even with many widgets
  assert_true(load_time < 1000)  // Less than 1 second
  
  // Test data loading performance
  let data_provider = DataProvider::new()
  
  // Mock data for all widgets
  for i in 0..=49 {
    DataProvider::mock_time_series(data_provider, "metrics.data_" + i.to_string(), || {
      @random() * 100.0
    })
  }
  
  // Test batch data loading
  let start_time = @time()
  OptimizedDashboard::load_data_batch(dashboard, data_provider)
  let batch_load_time = @time() - start_time
  
  // Batch loading should be efficient
  assert_true(batch_load_time < 2000)  // Less than 2 seconds
  
  // Test data caching
  let start_time = @time()
  OptimizedDashboard::refresh_widgets(dashboard, widgets.slice(0, 10))  // Refresh first 10 widgets
  let first_refresh_time = @time() - start_time
  
  // Second refresh should be faster due to caching
  let start_time = @time()
  OptimizedDashboard::refresh_widgets(dashboard, widgets.slice(0, 10))  // Refresh same widgets
  let second_refresh_time = @time() - start_time
  
  assert_true(second_refresh_time < first_refresh_time)
  
  // Test lazy loading
  let visible_widgets = widgets.slice(0, 10)  // Only first 10 are visible
  let start_time = @time()
  OptimizedDashboard::load_visible_widgets(dashboard, visible_widgets)
  let lazy_load_time = @time() - start_time
  
  // Lazy loading should only load visible widgets
  assert_true(lazy_load_time < batch_load_time)
  
  // Test data compression
  let compression_stats = OptimizedDashboard::get_compression_stats(dashboard)
  assert_true(CompressionStats::original_size(compression_stats) > CompressionStats::compressed_size(compression_stats))
  assert_true(CompressionStats::compression_ratio(compression_stats) > 0.0)
  
  // Test performance metrics
  let performance_metrics = OptimizedDashboard::get_performance_metrics(dashboard)
  
  assert_true(PerformanceMetrics::average_load_time(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::average_refresh_time(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::cache_hit_rate(performance_metrics) > 0.0)
  assert_true(PerformanceMetrics::memory_usage(performance_metrics) > 0)
  assert_true(PerformanceMetrics::data_throughput(performance_metrics) > 0.0)
  
  // Test widget virtualization for large datasets
  let large_chart = Widget::create_chart_widget("Large Dataset", ChartType::Line)
  Widget::set_data_source(large_chart, "metrics.large_dataset")
  Widget::enable_virtualization(large_chart, 1000)  // Virtualize beyond 1000 points
  
  Dashboard::add_widget(dashboard, large_chart, Position::new(0, 20, 12, 8))
  
  // Mock large dataset
  let large_data = []
  for i in 0..=9999 {  // 10,000 data points
    large_data.push(DataPoint::new(i * 1000, @random() * 100.0))
  }
  
  DataProvider::mock_large_dataset(data_provider, "metrics.large_dataset", large_data)
  
  // Test virtualized loading
  let start_time = @time()
  OptimizedDashboard::load_virtualized_data(dashboard, large_chart)
  let virtualized_load_time = @time() - start_time
  
  // Virtualized loading should be fast even with large datasets
  assert_true(virtualized_load_time < 500)  // Less than 500ms
  
  // Test optimization recommendations
  let recommendations = OptimizedDashboard::get_optimization_recommendations(dashboard)
  assert_true(recommendations.length() > 0)
  
  // Should include recommendations for widget count, refresh rates, etc.
  let has_widget_count_recommendation = recommendations.any(@(r) 
    Recommendation::type(r) == RecommendationType::WidgetCount
  )
  assert_true(has_widget_count_recommendation)
}

// Test 8: Dashboard Accessibility and Localization
test "dashboard accessibility and localization" {
  // Create accessible dashboard
  let dashboard = AccessibleDashboard::new("Accessible Dashboard")
  
  // Configure accessibility settings
  AccessibleDashboard::enable_high_contrast(dashboard)
  AccessibleDashboard::enable_screen_reader_support(dashboard)
  AccessibleDashboard::enable_keyboard_navigation(dashboard)
  AccessibleDashboard::set_font_size(dashboard, FontSize::Large)
  
  // Create widgets with accessibility features
  let cpu_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  Widget::set_accessibility_label(cpu_widget, "Current CPU usage percentage")
  Widget::set_accessibility_description(cpu_widget, "Shows the current CPU utilization as a percentage of total capacity")
  
  let memory_widget = Widget::create_metric_widget("Memory Usage", MetricType::Gauge)
  Widget::set_accessibility_label(memory_widget, "Current memory usage percentage")
  Widget::set_accessibility_description(memory_widget, "Shows the current memory utilization as a percentage of total capacity")
  
  let chart_widget = Widget::create_chart_widget("Response Time", ChartType::Line)
  Widget::set_accessibility_label(chart_widget, "Response time trend over time")
  Widget::set_accessibility_description(chart_widget, "Line chart showing response time trends with p50, p95, and p99 percentiles")
  
  // Add widgets to dashboard
  Dashboard::add_widget(dashboard, cpu_widget, Position::new(0, 0, 4, 4))
  Dashboard::add_widget(dashboard, memory_widget, Position::new(4, 0, 4, 4))
  Dashboard::add_widget(dashboard, chart_widget, Position::new(0, 4, 8, 4))
  
  // Test accessibility features
  assert_true(AccessibleDashboard::is_high_contrast_enabled(dashboard))
  assert_true(AccessibleDashboard::is_screen_reader_support_enabled(dashboard))
  assert_true(AccessibleDashboard::is_keyboard_navigation_enabled(dashboard))
  assert_eq(AccessibleDashboard::get_font_size(dashboard), FontSize::Large)
  
  // Test widget accessibility labels
  assert_eq(Widget::get_accessibility_label(cpu_widget), "Current CPU usage percentage")
  assert_eq(Widget::get_accessibility_description(cpu_widget), "Shows the current CPU utilization as a percentage of total capacity")
  
  // Test keyboard navigation order
  AccessibleDashboard::set_navigation_order(dashboard, [cpu_widget, memory_widget, chart_widget])
  let navigation_order = AccessibleDashboard::get_navigation_order(dashboard)
  assert_eq(navigation_order.length(), 3)
  assert_eq(navigation_order[0], cpu_widget)
  assert_eq(navigation_order[1], memory_widget)
  assert_eq(navigation_order[2], chart_widget)
  
  // Test localization
  let localization_manager = LocalizationManager::new()
  
  // Add translations
  LocalizationManager::add_translation(localization_manager, "en", "CPU Usage", "CPU Usage")
  LocalizationManager::add_translation(localization_manager, "en", "Memory Usage", "Memory Usage")
  LocalizationManager::add_translation(localization_manager, "en", "Response Time", "Response Time")
  
  LocalizationManager::add_translation(localization_manager, "es", "CPU Usage", "Uso de CPU")
  LocalizationManager::add_translation(localization_manager, "es", "Memory Usage", "Uso de Memoria")
  LocalizationManager::add_translation(localization_manager, "es", "Response Time", "Tiempo de Respuesta")
  
  LocalizationManager::add_translation(localization_manager, "fr", "CPU Usage", "Utilisation CPU")
  LocalizationManager::add_translation(localization_manager, "fr", "Memory Usage", "Utilisation Mémoire")
  LocalizationManager::add_translation(localization_manager, "fr", "Response Time", "Temps de Réponse")
  
  // Set dashboard locale
  AccessibleDashboard::set_locale(dashboard, "es")
  AccessibleDashboard::set_localization_manager(dashboard, localization_manager)
  
  // Test localized widget titles
  assert_eq(Widget::get_localized_title(cpu_widget), "Uso de CPU")
  assert_eq(Widget::get_localized_title(memory_widget), "Uso de Memoria")
  assert_eq(Widget::get_localized_title(chart_widget), "Tiempo de Respuesta")
  
  // Test locale change
  AccessibleDashboard::set_locale(dashboard, "fr")
  
  assert_eq(Widget::get_localized_title(cpu_widget), "Utilisation CPU")
  assert_eq(Widget::get_localized_title(memory_widget), "Utilisation Mémoire")
  assert_eq(Widget::get_localized_title(chart_widget), "Temps de Réponse")
  
  // Test right-to-left language support
  LocalizationManager::add_translation(localization_manager, "ar", "CPU Usage", "استخدام المعالج")
  LocalizationManager::set_text_direction(localization_manager, "ar", TextDirection::RTL)
  
  AccessibleDashboard::set_locale(dashboard, "ar")
  
  assert_eq(AccessibleDashboard::get_text_direction(dashboard), TextDirection::RTL)
  assert_eq(Widget::get_localized_title(cpu_widget), "استخدام المعالج")
  
  // Test number and date formatting
  AccessibleDashboard::set_locale(dashboard, "en")
  let formatted_number = AccessibleDashboard::format_number(dashboard, 1234.567)
  assert_eq(formatted_number, "1,234.567")
  
  let formatted_date = AccessibleDashboard::format_date(dashboard, 1640995200000L)
  assert_eq(formatted_date, "1/1/2022")
  
  AccessibleDashboard::set_locale(dashboard, "es")
  let spanish_number = AccessibleDashboard::format_number(dashboard, 1234.567)
  assert_eq(spanish_number, "1.234,567")
  
  // Test accessibility validation
  let validation_result = AccessibleDashboard::validate_accessibility(dashboard)
  
  assert_true(validation_result.overall_score > 0.8)  // Should have good accessibility score
  assert_true(validation_result.issues.length() == 0)  // No accessibility issues
  
  // Test missing accessibility label
  let new_widget = Widget::create_metric_widget("New Widget", MetricType::Gauge)
  Dashboard::add_widget(dashboard, new_widget, Position::new(0, 8, 4, 4))
  
  let validation_result_with_issue = AccessibleDashboard::validate_accessibility(dashboard)
  assert_true(validation_result_with_issue.issues.length() > 0)
  
  let missing_label_issue = validation_result_with_issue.issues.find(@(i) 
    AccessibilityIssue::type(i) == AccessibilityIssueType::MissingLabel
  )
  assert_true(missing_label_issue.is_some())
}

// Test 9: Dashboard Alerts and Notifications
test "dashboard alerts and notifications" {
  // Create dashboard with alerting
  let dashboard = AlertingDashboard::new("Alerting Dashboard")
  
  // Create widgets for monitoring
  let cpu_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  let memory_widget = Widget::create_metric_widget("Memory Usage", MetricType::Gauge)
  let error_rate_widget = Widget::create_chart_widget("Error Rate", ChartType::Line)
  let response_time_widget = Widget::create_chart_widget("Response Time", ChartType::Line)
  
  Dashboard::add_widget(dashboard, cpu_widget, Position::new(0, 0, 6, 4))
  Dashboard::add_widget(dashboard, memory_widget, Position::new(6, 0, 6, 4))
  Dashboard::add_widget(dashboard, error_rate_widget, Position::new(0, 4, 6, 4))
  Dashboard::add_widget(dashboard, response_time_widget, Position::new(6, 4, 6, 4))
  
  // Configure alert rules
  let cpu_alert = AlertRule::new("High CPU Usage", "cpu_widget")
  AlertRule::set_condition(cpu_alert, AlertCondition::GreaterThan(80.0))
  AlertRule::set_severity(cpu_alert, AlertSeverity::Warning)
  AlertRule::set_duration(cpu_alert, 300)  // 5 minutes
  AlertRule::set_message(cpu_alert, "CPU usage has been above 80% for more than 5 minutes")
  
  let memory_alert = AlertRule::new("High Memory Usage", "memory_widget")
  AlertRule::set_condition(memory_alert, AlertCondition::GreaterThan(90.0))
  AlertRule::set_severity(memory_alert, AlertSeverity::Critical)
  AlertRule::set_duration(memory_alert, 180)  // 3 minutes
  AlertRule::set_message(memory_alert, "Memory usage has been above 90% for more than 3 minutes")
  
  let error_rate_alert = AlertRule::new("High Error Rate", "error_rate_widget")
  AlertRule::set_condition(error_rate_alert, AlertCondition::GreaterThan(5.0))
  AlertRule::set_severity(error_rate_alert, AlertSeverity::Warning)
  AlertRule::set_duration(error_rate_alert, 600)  // 10 minutes
  
  let response_time_alert = AlertRule::new("High Response Time", "response_time_widget")
  AlertRule::set_condition(response_time_alert, AlertCondition::GreaterThan(1000.0))
  AlertRule::set_severity(response_time_alert, AlertSeverity::Critical)
  AlertRule::set_duration(response_time_alert, 120)  // 2 minutes
  
  // Add alert rules to dashboard
  AlertingDashboard::add_alert_rule(dashboard, cpu_alert)
  AlertingDashboard::add_alert_rule(dashboard, memory_alert)
  AlertingDashboard::add_alert_rule(dashboard, error_rate_alert)
  AlertingDashboard::add_alert_rule(dashboard, response_time_alert)
  
  // Configure notification channels
  let email_channel = NotificationChannel::email("admin@example.com")
  let slack_channel = NotificationChannel::slack("https://hooks.slack.com/webhook")
  let webhook_channel = NotificationChannel::webhook("https://api.example.com/alerts")
  
  AlertingDashboard::add_notification_channel(dashboard, email_channel)
  AlertingDashboard::add_notification_channel(dashboard, slack_channel)
  AlertingDashboard::add_notification_channel(dashboard, webhook_channel)
  
  // Configure alert routing
  AlertingDashboard::route_alert(dashboard, "High CPU Usage", [email_channel, slack_channel])
  AlertingDashboard::route_alert(dashboard, "High Memory Usage", [email_channel, slack_channel, webhook_channel])
  AlertingDashboard::route_alert(dashboard, "High Error Rate", [slack_channel])
  AlertingDashboard::route_alert(dashboard, "High Response Time", [email_channel, webhook_channel])
  
  // Create data provider with alert-triggering values
  let data_provider = DataProvider::new()
  
  // Mock data that triggers alerts
  DataProvider::mock_time_series(data_provider, "metrics.cpu.usage", || {
    85.0  // Above 80% threshold
  })
  
  DataProvider::mock_time_series(data_provider, "metrics.memory.usage", || {
    95.0  // Above 90% threshold
  })
  
  DataProvider::mock_time_series(data_provider, "metrics.error_rate", || {
    6.0  // Above 5% threshold
  })
  
  DataProvider::mock_time_series(data_provider, "metrics.response_time", || {
    1200.0  // Above 1000ms threshold
  })
  
  // Start alert monitoring
  AlertingDashboard::start_monitoring(dashboard, data_provider)
  
  // Wait for alert evaluation
  @sleep(1000)
  
  // Check active alerts
  let active_alerts = AlertingDashboard::get_active_alerts(dashboard)
  assert_eq(active_alerts.length(), 4)
  
  // Verify alert details
  let cpu_active_alert = active_alerts.find(@(a) Alert::rule_name(a) == "High CPU Usage")
  match cpu_active_alert {
    Some(alert) => {
      assert_eq(Alert::severity(alert), AlertSeverity::Warning)
      assert_eq(Alert::status(alert), AlertStatus::Firing)
      assert_eq(Alert::value(alert), 85.0)
    }
    None => assert_true(false)
  }
  
  let memory_active_alert = active_alerts.find(@(a) Alert::rule_name(a) == "High Memory Usage")
  match memory_active_alert {
    Some(alert) => {
      assert_eq(Alert::severity(alert), AlertSeverity::Critical)
      assert_eq(Alert::status(alert), AlertStatus::Firing)
      assert_eq(Alert::value(alert), 95.0)
    }
    None => assert_true(false)
  }
  
  // Test alert history
  let alert_history = AlertingDashboard::get_alert_history(dashboard)
  assert_true(alert_history.length() >= 4)
  
  // Test alert acknowledgment
  let cpu_alert_to_ack = active_alerts.find(@(a) Alert::rule_name(a) == "High CPU Usage").unwrap()
  AlertingDashboard::acknowledge_alert(dashboard, Alert::id(cpu_alert_to_ack), "Acknowledged by admin")
  
  let updated_alerts = AlertingDashboard::get_active_alerts(dashboard)
  let acked_alert = updated_alerts.find(@(a) Alert::id(a) == Alert::id(cpu_alert_to_ack))
  match acked_alert {
    Some(alert) => {
      assert_eq(Alert::status(alert), AlertStatus::Acknowledged)
      match Alert::acknowledgment(alert) {
        Some(ack) => {
          assert_eq(ack.message, "Acknowledged by admin")
          assert_true(ack.timestamp > 0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test alert resolution
  // Update data to resolve alerts
  DataProvider::mock_time_series(data_provider, "metrics.cpu.usage", || {
    45.0  // Below 80% threshold
  })
  
  // Wait for re-evaluation
  @sleep(1000)
  
  let resolved_alerts = AlertingDashboard::get_resolved_alerts(dashboard)
  let cpu_resolved = resolved_alerts.find(@(a) Alert::rule_name(a) == "High CPU Usage")
  match cpu_resolved {
    Some(alert) => {
      assert_eq(Alert::status(alert), AlertStatus::Resolved)
      assert_eq(Alert::value(alert), 45.0)
    }
    None => assert_true(false)
  }
  
  // Test alert suppression
  AlertingDashboard::suppress_alert(dashboard, "High Memory Usage", 3600)  // Suppress for 1 hour
  
  let suppressed_rules = AlertingDashboard::get_suppressed_rules(dashboard)
  assert_eq(suppressed_rules.length(), 1)
  assert_eq(suppressed_rules[0], "High Memory Usage")
  
  // Test alert statistics
  let alert_stats = AlertingDashboard::get_alert_statistics(dashboard)
  assert_eq(AlertStatistics::total_alerts(alert_stats), 4)
  assert_eq(AlertStatistics::active_alerts(alert_stats), 3)  // CPU resolved, memory suppressed
  assert_eq(AlertStatistics::critical_alerts(alert_stats), 2)
  assert_eq(AlertStatistics::warning_alerts(alert_stats), 2)
}

// Test 10: Dashboard Integration with External Systems
test "dashboard integration with external systems" {
  // Create dashboard with external integrations
  let dashboard = IntegratedDashboard::new("Integrated Dashboard")
  
  // Configure Prometheus integration
  let prometheus_config = PrometheusConfig::new("http://prometheus.example.com:9090")
  PrometheusConfig::set_query_timeout(prometheus_config, 30)
  PrometheusConfig::set_auth_token(prometheus_config, "prometheus-auth-token")
  
  IntegratedDashboard::add_prometheus_integration(dashboard, prometheus_config)
  
  // Configure Grafana integration
  let grafana_config = GrafanaConfig::new("http://grafana.example.com:3000")
  GrafanaConfig::set_api_key(grafana_config, "grafana-api-key")
  GrafanaConfig::set_default_datasource(grafana_config, "prometheus")
  
  IntegratedDashboard::add_grafana_integration(dashboard, grafana_config)
  
  // Configure Elasticsearch integration
  let elasticsearch_config = ElasticsearchConfig::new("http://elasticsearch.example.com:9200")
  ElasticsearchConfig::set_index_pattern(elasticsearch_config, "logs-*")
  ElasticsearchConfig::set_username(elasticsearch_config, "elastic")
  ElasticsearchConfig::set_password(elasticsearch_config, "elastic-password")
  
  IntegratedDashboard::add_elasticsearch_integration(dashboard, elasticsearch_config)
  
  // Create widgets with external data sources
  let cpu_widget = Widget::create_metric_widget("CPU Usage", MetricType::Gauge)
  Widget::set_prometheus_query(cpu_widget, "avg(100 - (avg by(instance) (irate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100))")
  
  let memory_widget = Widget::create_metric_widget("Memory Usage", MetricType::Gauge)
  Widget::set_prometheus_query(memory_widget, "(1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100")
  
  let request_rate_widget = Widget::create_chart_widget("Request Rate", ChartType::Line)
  Widget::set_prometheus_query(request_rate_widget, "sum(rate(http_requests_total[5m])) by(service)")
  
  let error_logs_widget = Widget::create_table_widget("Error Logs")
  Widget::set_elasticsearch_query(error_logs_widget, {
    "query": {
      "bool": {
        "must": [
          {"match": {"level": "ERROR"}}
        ],
        "filter": [
          {"range": {"@timestamp": {"gte": "now-1h"}}}
        ]
      }
    },
    "sort": [
      {"@timestamp": {"order": "desc"}}
    ]
  })
  
  Dashboard::add_widget(dashboard, cpu_widget, Position::new(0, 0, 6, 4))
  Dashboard::add_widget(dashboard, memory_widget, Position::new(6, 0, 6, 4))
  Dashboard::add_widget(dashboard, request_rate_widget, Position::new(0, 4, 12, 4))
  Dashboard::add_widget(dashboard, error_logs_widget, Position::new(0, 8, 12, 4))
  
  // Test Prometheus integration
  let prometheus_client = IntegratedDashboard::get_prometheus_client(dashboard)
  
  // Mock Prometheus response
  IntegratedDashboard::mock_prometheus_response(dashboard, "avg(100 - (avg by(instance) (irate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100))", {
    "status": "success",
    "data": {
      "resultType": "vector",
      "result": [
        {
          "metric": {"instance": "server1"},
          "value": [1640995200, "75.5"]
        },
        {
          "metric": {"instance": "server2"},
          "value": [1640995200, "82.3"]
        }
      ]
    }
  })
  
  let prometheus_data = PrometheusClient::query(prometheus_client, "avg(100 - (avg by(instance) (irate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100))")
  
  match prometheus_data {
    Ok(data) => {
      assert_eq(data.result_type, "vector")
      assert_eq(data.result.length(), 2)
      assert_eq(data.result[0].value[1], "75.5")
      assert_eq(data.result[1].value[1], "82.3")
    }
    Err(_) => assert_true(false)
  }
  
  // Test Elasticsearch integration
  let elasticsearch_client = IntegratedDashboard::get_elasticsearch_client(dashboard)
  
  // Mock Elasticsearch response
  IntegratedDashboard::mock_elasticsearch_response(dashboard, {
    "took": 5,
    "timed_out": false,
    "_shards": {"total": 1, "successful": 1, "skipped": 0, "failed": 0},
    "hits": {
      "total": {"value": 10000, "relation": "eq"},
      "max_score": 1.0,
      "hits": [
        {
          "_index": "logs-2022.01.01",
          "_id": "1",
          "_score": 1.0,
          "_source": {
            "@timestamp": "2022-01-01T12:00:00.000Z",
            "level": "ERROR",
            "message": "Database connection failed",
            "service": "user-service"
          }
        },
        {
          "_index": "logs-2022.01.01",
          "_id": "2",
          "_score": 1.0,
          "_source": {
            "@timestamp": "2022-01-01T12:01:00.000Z",
            "level": "ERROR",
            "message": "API rate limit exceeded",
            "service": "order-service"
          }
        }
      ]
    }
  })
  
  let elasticsearch_data = ElasticsearchClient::search(elasticsearch_client, {
    "query": {
      "bool": {
        "must": [
          {"match": {"level": "ERROR"}}
        ],
        "filter": [
          {"range": {"@timestamp": {"gte": "now-1h"}}}
        ]
      }
    },
    "sort": [
      {"@timestamp": {"order": "desc"}}
    ]
  })
  
  match elasticsearch_data {
    Ok(data) => {
      assert_eq(data.hits.total.value, 10000)
      assert_eq(data.hits.hits.length(), 2)
      assert_eq(data.hits.hits[0]._source.level, "ERROR")
      assert_eq(data.hits.hits[0]._source.message, "Database connection failed")
    }
    Err(_) => assert_true(false)
  }
  
  // Test Grafana integration
  let grafana_client = IntegratedDashboard::get_grafana_client(dashboard)
  
  // Mock Grafana response
  IntegratedDashboard::mock_grafana_response(dashboard, {
    "dashboard": {
      "id": 123,
      "title": "System Overview",
      "panels": [
        {
          "id": 1,
          "title": "CPU Usage",
          "type": "stat"
        },
        {
          "id": 2,
          "title": "Memory Usage",
          "type": "stat"
        }
      ]
    }
  })
  
  let grafana_dashboard = GrafanaClient::get_dashboard(grafana_client, 123)
  
  match grafana_dashboard {
    Ok(dashboard) => {
      assert_eq(dashboard.dashboard.id, 123)
      assert_eq(dashboard.dashboard.title, "System Overview")
      assert_eq(dashboard.dashboard.panels.length(), 2)
    }
    Err(_) => assert_true(false)
  }
  
  // Test widget data loading from external sources
  IntegratedDashboard::load_widget_data(dashboard, cpu_widget)
  
  let cpu_data = Widget::get_current_data(cpu_widget)
  match cpu_data {
    Some(data) => {
      assert_true(data.value >= 0.0)
      assert_true(data.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // Test data source health checks
  let health_status = IntegratedDashboard::check_data_source_health(dashboard)
  
  assert_true(health_status.contains("prometheus"))
  assert_true(health_status.contains("elasticsearch"))
  assert_true(health_status.contains("grafana"))
  
  // Test integration metrics
  let integration_metrics = IntegratedDashboard::get_integration_metrics(dashboard)
  
  assert_true(IntegrationMetrics::prometheus_query_count(integration_metrics) > 0)
  assert_true(IntegrationMetrics::elasticsearch_query_count(integration_metrics) > 0)
  assert_true(IntegrationMetrics::grafana_api_calls(integration_metrics) > 0)
  assert_true(IntegrationMetrics::average_response_time(integration_metrics) > 0.0)
  
  // Test external dashboard import
  let grafana_dashboard_json = {
    "dashboard": {
      "title": "Imported Dashboard",
      "panels": [
        {
          "title": "Imported Panel",
          "type": "graph",
          "targets": [
            {
              "expr": "up"
            }
          ]
        }
      ]
    }
  }
  
  let imported_dashboard = IntegratedDashboard::import_from_grafana(dashboard, grafana_dashboard_json)
  
  assert_eq(Dashboard::title(imported_dashboard), "Imported Dashboard")
  assert_eq(Dashboard::widget_count(imported_dashboard), 1)
  
  let imported_panel = Dashboard::get_widgets(imported_dashboard)[0]
  assert_eq(Widget::title(imported_panel), "Imported Panel")
}