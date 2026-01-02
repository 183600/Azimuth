// Azimuth Comprehensive Gauge and Dashboard Detailed Functionality Test Suite
// 综合度量仪表盘详细功能测试套件

// Test 1: 高级度量仪表盘组件功能测试
test "advanced gauge dashboard component functionality" {
  let gauge_dashboard = @azimuth.GaugeDashboard::new()
  
  // 配置仪表盘
  let dashboard_config = @azimuth.DashboardConfig::new()
  @azimuth.DashboardConfig::set_refresh_interval(dashboard_config, 5000000000) // 5秒刷新间隔
  @azimuth.DashboardConfig::enable_auto_scaling(dashboard_config, true)
  @azimuth.DashboardConfig::set_max_data_points(dashboard_config, 1000)
  @azimuth.DashboardConfig::enable_data_aggregation(dashboard_config, true)
  @azimuth.DashboardConfig::set_aggregation_window(dashboard_config, 60000000000) // 1分钟聚合窗口
  
  @azimuth.GaugeDashboard::configure(gauge_dashboard, dashboard_config)
  
  // 创建多种类型的仪表盘组件
  let gauge_types = [
    @azimuth.GaugeType::Linear,
    @azimuth.GaugeType::Circular,
    @azimuth.GaugeType::SemiCircular,
    @azimuth.GaugeType::Thermometer,
    @azimuth.GaugeType::LED
  ]
  
  let mut gauge_components = []
  
  for i in 0..gauge_types.length() - 1 {
    let gauge_type = gauge_types[i]
    let gauge_component = @azimuth.GaugeComponent::new()
    
    // 配置仪表盘组件
    @azimuth.GaugeComponent::set_type(gauge_component, gauge_type)
    @azimuth.GaugeComponent::set_title(gauge_component, "Gauge " + i.to_string())
    @azimuth.GaugeComponent::set_unit(gauge_component, match i {
      0 => "ms"
      1 => "%"
      2 => "°C"
      3 => "°C"
      _ => "count"
    })
    @azimuth.GaugeComponent::set_min_value(gauge_component, match i {
      0 => 0.0
      1 => 0.0
      2 => -20.0
      3 => 0.0
      _ => 0.0
    })
    @azimuth.GaugeComponent::set_max_value(gauge_component, match i {
      0 => 1000.0
      1 => 100.0
      2 => 50.0
      3 => 100.0
      _ => 100.0
    })
    @azimuth.GaugeComponent::set_thresholds(gauge_component, [
      (0.0, @azimuth.Color::Green),
      (0.6, @azimuth.Color::Yellow),
      (0.8, @azimuth.Color::Red)
    ])
    
    // 注册仪表盘组件
    @azimuth.GaugeDashboard::register_component(gauge_dashboard, gauge_component)
    gauge_components = gauge_components.push(gauge_component)
  }
  
  // 生成测试数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  let data_points_count = 500
  
  for i in 0..data_points_count {
    let timestamp = base_timestamp + i * 1000000000 // 1秒间隔
    
    // 为每个仪表盘组件生成数据
    for j in 0..gauge_components.length() - 1 {
      let gauge_component = gauge_components[j]
      let gauge_type = @azimuth.GaugeComponent::get_type(gauge_component)
      
      // 生成不同类型的数据
      let value = match gauge_type {
        @azimuth.GaugeType::Linear => 100.0 + 200.0 * @azimuth.Math::sin(i.to_float() / 50.0) + @azimuth.Random::next_float() * 50.0
        @azimuth.GaugeType::Circular => 50.0 + 30.0 * @azimuth.Math::cos(i.to_float() / 30.0) + @azimuth.Random::next_float() * 20.0
        @azimuth.GaugeType::SemiCircular => 20.0 + 15.0 * @azimuth.Math::sin(i.to_float() / 40.0) + @azimuth.Random::next_float() * 10.0
        @azimuth.GaugeType::Thermometer => 25.0 + 10.0 * @azimuth.Math::sin(i.to_float() / 60.0) + @azimuth.Random::next_float() * 5.0
        @azimuth.GaugeType::LED => @azimuth.Math::round(10.0 * @azimuth.Math::sin(i.to_float() / 20.0) + 10.0)
      }
      
      // 创建数据点
      let data_point = @azimuth.GaugeDataPoint::new(timestamp, value, [
        ("component.id", @azimuth.StringValue("gauge-" + j.to_string())),
        ("data.source", @azimuth.StringValue("test-generator")),
        ("quality.indicator", @azimuth.FloatValue(0.8 + @azimuth.Random::next_float() * 0.2))
      ])
      
      // 添加数据到仪表盘组件
      @azimuth.GaugeComponent::add_data_point(gauge_component, data_point)
    }
  }
  
  // 刷新仪表盘
  @azimuth.GaugeDashboard::refresh(gauge_dashboard)
  
  // 验证仪表盘状态
  let dashboard_state = @azimuth.GaugeDashboard::get_state(gauge_dashboard)
  
  // 验证每个仪表盘组件的状态
  for i in 0..gauge_components.length() - 1 {
    let gauge_component = gauge_components[i]
    let component_state = @azimuth.GaugeComponent::get_state(gauge_component)
    
    // 验证数据点数量
    let data_points_count = @azimuth.GaugeComponentState::data_points_count(component_state)
    assert_true(data_points_count > 0)
    
    // 验证当前值
    let current_value = @azimuth.GaugeComponentState::current_value(component_state)
    assert_true(current_value >= @azimuth.GaugeComponent::get_min_value(gauge_component))
    assert_true(current_value <= @azimuth.GaugeComponent::get_max_value(gauge_component))
    
    // 验证统计值
    let avg_value = @azimuth.GaugeComponentState::average_value(component_state)
    let min_value = @azimuth.GaugeComponentState::min_value(component_state)
    let max_value = @azimuth.GaugeComponentState::max_value(component_state)
    
    assert_true(avg_value >= min_value && avg_value <= max_value)
    assert_true(min_value <= max_value)
    
    // 验证阈值状态
    let threshold_status = @azimuth.GaugeComponentState::threshold_status(component_state)
    match threshold_status {
      @azimuth.ThresholdStatus::Normal => assert_true(true)
      @azimuth.ThresholdStatus::Warning => assert_true(true)
      @azimuth.ThresholdStatus::Critical => assert_true(true)
    }
    
    // 验证颜色状态
    let current_color = @azimuth.GaugeComponentState::current_color(component_state)
    assert_true(current_color != @azimuth.Color::Transparent)
  }
  
  // 测试仪表盘布局
  let layout_config = @azimuth.DashboardLayoutConfig::new()
  @azimuth.DashboardLayoutConfig::set_columns(layout_config, 3)
  @azimuth.DashboardLayoutConfig::set_rows(layout_config, 2)
  @azimuth.DashboardLayoutConfig::set_spacing(layout_config, 10)
  @azimuth.DashboardLayoutConfig::enable_responsive(layout_config, true)
  
  @azimuth.GaugeDashboard::apply_layout(gauge_dashboard, layout_config)
  
  // 验证布局效果
  let layout_state = @azimuth.GaugeDashboard::get_layout_state(gauge_dashboard)
  let component_positions = @azimuth.LayoutState::component_positions(layout_state)
  
  assert_eq(component_positions.length(), gauge_components.length())
  
  // 验证每个组件的位置
  for i in 0..component_positions.length() - 1 {
    let position = component_positions[i]
    let x = @azimuth.ComponentPosition::x(position)
    let y = @azimuth.ComponentPosition::y(position)
    let width = @azimuth.ComponentPosition::width(position)
    let height = @azimuth.ComponentPosition::height(position)
    
    assert_true(x >= 0 && y >= 0)
    assert_true(width > 0 && height > 0)
  }
  
  assert_true(true)
}

// Test 2: 实时数据流和动态更新测试
test "real-time data streaming and dynamic updates" {
  let realtime_dashboard = @azimuth.RealtimeGaugeDashboard::new()
  
  // 配置实时仪表盘
  let realtime_config = @azimuth.RealtimeConfig::new()
  @azimuth.RealtimeConfig::set_update_frequency(realtime_config, 1000000000) // 1秒更新频率
  @azimuth.RealtimeConfig::enable_smoothing(realtime_config, true)
  @azimuth.RealtimeConfig::set_smoothing_window(realtime_config, 5) // 5点平滑窗口
  @azimuth.RealtimeConfig::enable_anomaly_detection(realtime_config, true)
  @azimuth.RealtimeConfig::set_anomaly_threshold(realtime_config, 2.0) // 2个标准差
  
  @azimuth.RealtimeGaugeDashboard::configure(realtime_dashboard, realtime_config)
  
  // 创建实时数据源
  let data_source = @azimuth.RealtimeDataSource::new()
  @azimuth.RealtimeDataSource::set_data_rate(data_source, 10) // 10Hz数据速率
  @azimuth.RealtimeDataSource::enable_noise_simulation(data_source, true)
  @azimuth.RealtimeDataSource::set_noise_level(data_source, 0.1)
  
  // 注册数据源
  @azimuth.RealtimeGaugeDashboard::register_data_source(realtime_dashboard, data_source)
  
  // 创建实时仪表盘组件
  let realtime_gauge = @azimuth.RealtimeGaugeComponent::new()
  @azimuth.RealtimeGaugeComponent::set_title(realtime_gauge, "Real-time System Load")
  @azimuth.RealtimeGaugeComponent::set_unit(realtime_gauge, "%")
  @azimuth.RealtimeGaugeComponent::set_min_value(realtime_gauge, 0.0)
  @azimuth.RealtimeGaugeComponent::set_max_value(realtime_gauge, 100.0)
  @azimuth.RealtimeGaugeComponent::enable_trend_indicator(realtime_gauge, true)
  @azimuth.RealtimeGaugeComponent::enable_prediction(realtime_gauge, true)
  @azimuth.RealtimeGaugeComponent::set_prediction_window(realtime_gauge, 10) // 10秒预测窗口
  
  // 注册实时仪表盘组件
  @azimuth.RealtimeGaugeDashboard::register_component(realtime_dashboard, realtime_gauge)
  
  // 启动实时仪表盘
  @azimuth.RealtimeGaugeDashboard::start(realtime_dashboard)
  
  // 生成实时数据流
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  let realtime_duration = 20000000000 // 20秒实时测试
  let data_interval = 100000000 // 100ms数据间隔
  
  let mut expected_values = []
  let mut actual_values = []
  
  let mut current_time = base_timestamp
  while current_time - base_timestamp < realtime_duration {
    // 生成模拟系统负载数据
    let base_load = 50.0
    let daily_pattern = 20.0 * @azimuth.Math::sin((current_time - base_timestamp).to_float() / 10000000000.0) // 10秒周期
    let random_noise = (@azimuth.Random::next_float() - 0.5) * 10.0
    let spike_probability = @azimuth.Random::next_float()
    let spike = if spike_probability < 0.05 { 30.0 } else { 0.0 } // 5%概率出现峰值
    
    let system_load = base_load + daily_pattern + random_noise + spike
    let clamped_load = @azimuth.Math::max(0.0, @azimuth.Math::min(100.0, system_load))
    
    // 记录期望值
    expected_values = expected_values.push(clamped_load)
    
    // 创建实时数据点
    let data_point = @azimuth.RealtimeDataPoint::new(current_time, clamped_load, [
      ("metric.name", @azimuth.StringValue("system.load")),
      ("host.name", @azimuth.StringValue("server-001")),
      ("data.quality", @azimuth.FloatValue(0.9 + @azimuth.Random::next_float() * 0.1))
    ])
    
    // 发送实时数据
    @azimuth.RealtimeDataSource::send_data(data_source, data_point)
    
    // 短暂休眠
    @azimuth.Time::sleep(data_interval)
    current_time = current_time + data_interval
    
    // 获取当前仪表盘值
    if (current_time - base_timestamp) % 1000000000 < data_interval { // 每秒记录一次
      let gauge_value = @azimuth.RealtimeGaugeComponent::get_current_value(realtime_gauge)
      actual_values = actual_values.push(gauge_value)
    }
  }
  
  // 停止实时仪表盘
  @azimuth.RealtimeGaugeDashboard::stop(realtime_dashboard)
  
  // 获取实时处理结果
  let realtime_stats = @azimuth.RealtimeGaugeDashboard::get_stats(realtime_dashboard)
  
  // 验证实时数据处理
  let total_data_points = @azimuth.RealtimeStats::total_data_points(realtime_stats)
  let processed_data_points = @azimuth.RealtimeStats::processed_data_points(realtime_stats)
  let dropped_data_points = @azimuth.RealtimeStats::dropped_data_points(realtime_stats)
  let average_processing_latency = @azimuth.RealtimeStats::average_processing_latency(realtime_stats)
  
  assert_true(total_data_points > 0)
  assert_true(processed_data_points > 0)
  assert_true(dropped_data_points < total_data_points * 0.01) // 丢包率应该小于1%
  assert_true(average_processing_latency < 100000000) // 平均处理延迟应该小于100ms
  
  // 验证平滑效果
  let smoothing_stats = @azimuth.RealtimeGaugeComponent::get_smoothing_stats(realtime_gauge)
  let smoothing_effectiveness = @azimuth.SmoothingStats::effectiveness(smoothing_stats)
  let noise_reduction_ratio = @azimuth.SmoothingStats::noise_reduction_ratio(smoothing_stats)
  
  assert_true(smoothing_effectiveness > 0.5) // 平滑效果应该大于50%
  assert_true(noise_reduction_ratio > 0.3) // 噪声减少比例应该大于30%
  
  // 验证异常检测
  let anomaly_stats = @azimuth.RealtimeGaugeComponent::get_anomaly_stats(realtime_gauge)
  let detected_anomalies = @azimuth.AnomalyStats::detected_anomalies(anomaly_stats)
  let false_positive_rate = @azimuth.AnomalyStats::false_positive_rate(anomaly_stats)
  
  assert_true(detected_anomalies > 0) // 应该检测到异常
  assert_true(false_positive_rate < 0.1) // 误报率应该小于10%
  
  // 验证趋势分析
  let trend_stats = @azimuth.RealtimeGaugeComponent::get_trend_stats(realtime_gauge)
  let trend_direction = @azimuth.TrendStats::direction(trend_stats)
  let trend_strength = @azimuth.TrendStats::strength(trend_stats)
  
  match trend_direction {
    @azimuth.TrendDirection::Up => assert_true(true)
    @azimuth.TrendDirection::Down => assert_true(true)
    @azimuth.TrendDirection::Stable => assert_true(true)
  }
  
  assert_true(trend_strength >= 0.0 && trend_strength <= 1.0)
  
  // 验证预测功能
  let prediction_stats = @azimuth.RealtimeGaugeComponent::get_prediction_stats(realtime_gauge)
  let prediction_accuracy = @azimuth.PredictionStats::accuracy(prediction_stats)
  let prediction_horizon = @azimuth.PredictionStats::horizon(prediction_stats)
  
  assert_true(prediction_accuracy > 0.7) // 预测准确率应该大于70%
  assert_eq(prediction_horizon, 10) // 预测窗口应该是10秒
  
  assert_true(true)
}

// Test 3: 交互式仪表盘功能测试
test "interactive dashboard functionality" {
  let interactive_dashboard = @azimuth.InteractiveGaugeDashboard::new()
  
  // 配置交互式仪表盘
  let interactive_config = @azimuth.InteractiveConfig::new()
  @azimuth.InteractiveConfig::enable_zoom(interactive_config, true)
  @azimuth.InteractiveConfig::enable_pan(interactive_config, true)
  @azimuth.InteractiveConfig::enable_selection(interactive_config, true)
  @azimuth.InteractiveConfig::enable_tooltip(interactive_config, true)
  @azimuth.InteractiveConfig::enable_context_menu(interactive_config, true)
  
  @azimuth.InteractiveGaugeDashboard::configure(interactive_dashboard, interactive_config)
  
  // 创建交互式仪表盘组件
  let interactive_gauge = @azimuth.InteractiveGaugeComponent::new()
  @azimuth.InteractiveGaugeComponent::set_title(interactive_gauge, "Interactive Performance Monitor")
  @azimuth.InteractiveGaugeComponent::set_unit(interactive_gauge, "req/s")
  @azimuth.InteractiveGaugeComponent::set_min_value(interactive_gauge, 0.0)
  @azimuth.InteractiveGaugeComponent::set_max_value(interactive_gauge, 1000.0)
  @azimuth.InteractiveGaugeComponent::enable_history_view(interactive_gauge, true)
  @azimuth.InteractiveGaugeComponent::set_history_window(interactive_gauge, 3600000000000) // 1小时历史窗口
  
  // 注册交互式仪表盘组件
  @azimuth.InteractiveGaugeDashboard::register_component(interactive_dashboard, interactive_gauge)
  
  // 生成测试数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  let data_points_count = 1000
  
  for i in 0..data_points_count {
    let timestamp = base_timestamp + i * 3600000000 // 1小时间隔
    
    // 生成模拟请求数据
    let base_requests = 500.0
    let daily_pattern = 200.0 * @azimuth.Math::sin((i % 24).to_float() / 24.0 * 2.0 * @azimuth.Math::PI) // 24小时周期
    let random_variation = (@azimuth.Random::next_float() - 0.5) * 100.0
    let weekend_factor = if i % 7 >= 5 { 0.7 } else { 1.0 } // 周末流量减少
    
    let request_rate = (base_requests + daily_pattern + random_variation) * weekend_factor
    let clamped_rate = @azimuth.Math::max(0.0, @azimuth.Math::min(1000.0, request_rate))
    
    // 创建数据点
    let data_point = @azimuth.GaugeDataPoint::new(timestamp, clamped_rate, [
      ("metric.name", @azimuth.StringValue("http.requests.rate")),
      ("service.name", @azimuth.StringValue("api-service")),
      ("environment", @azimuth.StringValue("production")),
      ("region", @azimuth.StringValue(["us-east-1", "us-west-1", "eu-west-1"][i % 3]))
    ])
    
    // 添加数据到仪表盘组件
    @azimuth.InteractiveGaugeComponent::add_data_point(interactive_gauge, data_point)
  }
  
  // 测试缩放功能
  let zoom_factor = 2.0
  let zoom_center_time = base_timestamp + (data_points_count / 2) * 3600000000
  
  @azimuth.InteractiveGaugeComponent::zoom(interactive_gauge, zoom_factor, zoom_center_time)
  
  // 验证缩放效果
  let zoom_state = @azimuth.InteractiveGaugeComponent::get_zoom_state(interactive_gauge)
  let current_zoom_factor = @azimuth.ZoomState::zoom_factor(zoom_state)
  let visible_time_range = @azimuth.ZoomState::visible_time_range(zoom_state)
  
  assert_eq(current_zoom_factor, zoom_factor)
  assert_true(@azimuth.TimeRange::duration(visible_time_range) < data_points_count * 3600000000)
  
  // 测试平移功能
  let pan_offset = 3600000000 // 1小时偏移
  @azimuth.InteractiveGaugeComponent::pan(interactive_gauge, pan_offset)
  
  // 验证平移效果
  let pan_state = @azimuth.InteractiveGaugeComponent::get_pan_state(interactive_gauge)
  let current_pan_offset = @azimuth.PanState::offset(pan_state)
  
  assert_eq(current_pan_offset, pan_offset)
  
  // 测试选择功能
  let selection_start = base_timestamp + 100 * 3600000000
  let selection_end = base_timestamp + 200 * 3600000000
  
  @azimuth.InteractiveGaugeComponent::select_range(interactive_gauge, selection_start, selection_end)
  
  // 验证选择效果
  let selection_state = @azimuth.InteractiveGaugeComponent::get_selection_state(interactive_gauge)
  let selected_range = @azimuth.SelectionState::range(selection_state)
  let selected_data_points = @azimuth.SelectionState::data_points(selection_state)
  
  assert_eq(@azimuth.TimeRange::start(selected_range), selection_start)
  assert_eq(@azimuth.TimeRange::end(selected_range), selection_end)
  assert_true(selected_data_points.length() > 0)
  
  // 验证选择数据的统计
  let selection_stats = @azimuth.InteractiveGaugeComponent::get_selection_stats(interactive_gauge)
  let avg_selected_value = @azimuth.SelectionStats::average_value(selection_stats)
  let min_selected_value = @azimuth.SelectionStats::min_value(selection_stats)
  let max_selected_value = @azimuth.SelectionStats::max_value(selection_stats)
  let selected_count = @azimuth.SelectionStats::count(selection_stats)
  
  assert_true(avg_selected_value >= min_selected_value && avg_selected_value <= max_selected_value)
  assert_true(min_selected_value <= max_selected_value)
  assert_true(selected_count > 0)
  
  // 测试工具提示功能
  let tooltip_time = base_timestamp + 150 * 3600000000
  let tooltip_data = @azimuth.InteractiveGaugeComponent::get_tooltip_data(interactive_gauge, tooltip_time)
  
  // 验证工具提示数据
  assert_true(@azimuth.TooltipData::is_valid(tooltip_data))
  assert_eq(@azimuth.TooltipData::timestamp(tooltip_data), tooltip_time)
  assert_true(@azimuth.TooltipData::value(tooltip_data) >= 0.0)
  assert_true(@azimuth.TooltipData::attributes(tooltip_data).length() > 0)
  
  // 测试上下文菜单功能
  let context_menu_items = [
    @azimuth.ContextMenuItem::new("export.data", "Export Data"),
    @azimuth.ContextMenuItem::new("set.alert", "Set Alert"),
    @azimuth.ContextMenuItem::new("view.details", "View Details"),
    @azimuth.ContextMenuItem::new("reset.zoom", "Reset Zoom")
  ]
  
  for item in context_menu_items {
    @azimuth.InteractiveGaugeComponent::add_context_menu_item(interactive_gauge, item)
  }
  
  // 验证上下文菜单
  let available_menu_items = @azimuth.InteractiveGaugeComponent::get_context_menu_items(interactive_gauge)
  assert_eq(available_menu_items.length(), context_menu_items.length())
  
  // 测试上下文菜单操作
  let export_action = @azimuth.InteractiveGaugeComponent::execute_context_menu_action(interactive_gauge, "export.data")
  assert_true(@azimuth.ContextMenuAction::is_successful(export_action))
  
  let alert_action = @azimuth.InteractiveGaugeComponent::execute_context_menu_action(interactive_gauge, "set.alert")
  assert_true(@azimuth.ContextMenuAction::is_successful(alert_action))
  
  // 测试历史视图
  @azimuth.InteractiveGaugeComponent::show_history_view(interactive_gauge)
  
  // 验证历史视图状态
  let history_view_state = @azimuth.InteractiveGaugeComponent::get_history_view_state(interactive_gauge)
  let is_history_visible = @azimuth.HistoryViewState::is_visible(history_view_state)
  let history_time_range = @azimuth.HistoryViewState::time_range(history_view_state)
  
  assert_true(is_history_visible)
  assert_true(@azimuth.TimeRange::duration(history_time_range) >= 3600000000000) // 至少1小时历史
  
  // 测试数据导出
  let export_format = @azimuth.ExportFormat::CSV
  let export_options = @azimuth.ExportOptions::new()
  @azimuth.ExportOptions::set_include_metadata(export_options, true)
  @azimuth.ExportOptions::set_date_format(export_options, "YYYY-MM-DD HH:mm:ss")
  
  let exported_data = @azimuth.InteractiveGaugeComponent::export_data(interactive_gauge, export_format, export_options)
  
  // 验证导出数据
  assert_true(@azimuth.ExportedData::is_valid(exported_data))
  assert_true(@azimuth.ExportedData::size(exported_data) > 0)
  assert_eq(@azimuth.ExportedData::format(exported_data), export_format)
  
  assert_true(true)
}

// Test 4: 自定义仪表盘主题和样式测试
test "custom dashboard themes and styling" {
  let themed_dashboard = @azimuth.ThemedGaugeDashboard::new()
  
  // 创建自定义主题
  let custom_theme = @azimuth.DashboardTheme::new()
  @azimuth.DashboardTheme::set_name(custom_theme, "Dark Professional")
  @azimuth.DashboardTheme::set_background_color(custom_theme, @azimuth.Color::RGB(30, 30, 30))
  @azimuth.DashboardTheme::set_foreground_color(custom_theme, @azimuth.Color::RGB(220, 220, 220))
  @azimuth.DashboardTheme::set_primary_color(custom_theme, @azimuth.Color::RGB(64, 158, 255))
  @azimuth.DashboardTheme::set_secondary_color(custom_theme, @azimuth.Color::RGB(255, 159, 64))
  @azimuth.DashboardTheme::set_success_color(custom_theme, @azimuth.Color::RGB(40, 167, 69))
  @azimuth.DashboardTheme::set_warning_color(custom_theme, @azimuth.Color::RGB(255, 193, 7))
  @azimuth.DashboardTheme::set_error_color(custom_theme, @azimuth.Color::RGB(220, 53, 69))
  
  // 设置字体样式
  let font_config = @azimuth.FontConfig::new()
  @azimuth.FontConfig::set_family(font_config, "Inter, sans-serif")
  @azimuth.FontConfig::set_size(font_config, 14)
  @azimuth.FontConfig::set_weight(font_config, @azimuth.FontWeight::Normal)
  
  @azimuth.DashboardTheme::set_font_config(custom_theme, font_config)
  
  // 设置边框和阴影
  let border_config = @azimuth.BorderConfig::new()
  @azimuth.BorderConfig::set_width(border_config, 1)
  @azimuth.BorderConfig::set_color(border_config, @azimuth.Color::RGB(60, 60, 60))
  @azimuth.BorderConfig::set_radius(border_config, 8)
  
  let shadow_config = @azimuth.ShadowConfig::new()
  @azimuth.ShadowConfig::set_offset_x(shadow_config, 0)
  @azimuth.ShadowConfig::set_offset_y(shadow_config, 2)
  @azimuth.ShadowConfig::set_blur(shadow_config, 4)
  @azimuth.ShadowConfig::set_color(shadow_config, @azimuth.Color::RGBA(0, 0, 0, 0.2))
  
  @azimuth.DashboardTheme::set_border_config(custom_theme, border_config)
  @azimuth.DashboardTheme::set_shadow_config(custom_theme, shadow_config)
  
  // 应用自定义主题
  @azimuth.ThemedGaugeDashboard::apply_theme(themed_dashboard, custom_theme)
  
  // 创建主题化仪表盘组件
  let themed_gauge = @azimuth.ThemedGaugeComponent::new()
  @azimuth.ThemedGaugeComponent::set_title(themed_gauge, "Themed Performance Gauge")
  @azimuth.ThemedGaugeComponent::set_unit(themed_gauge, "ms")
  @azimuth.ThemedGaugeComponent::set_min_value(themed_gauge, 0.0)
  @azimuth.ThemedGaugeComponent::set_max_value(themed_gauge, 500.0)
  
  // 设置自定义颜色方案
  let custom_colors = [
    (0.0, @azimuth.Color::RGB(40, 167, 69)),   // 绿色 - 正常
    (0.6, @azimuth.Color::RGB(255, 193, 7)),   // 黄色 - 警告
    (0.8, @azimuth.Color::RGB(255, 159, 64)),  // 橙色 - 注意
    (0.9, @azimuth.Color::RGB(220, 53, 69))    // 红色 - 危险
  ]
  
  @azimuth.ThemedGaugeComponent::set_color_scheme(themed_gauge, custom_colors)
  
  // 设置自定义样式
  let gauge_style = @azimuth.GaugeStyle::new()
  @azimuth.GaugeStyle::set_stroke_width(gauge_style, 8)
  @azimuth.GaugeStyle::set_inner_radius(gauge_style, 0.7)
  @azimuth.GaugeStyle::set_start_angle(gauge_style, -135)
  @azimuth.GaugeStyle::set_end_angle(gauge_style, 135)
  @azimuth.GaugeStyle::enable_gradient(gauge_style, true)
  @azimuth.GaugeStyle::enable_animation(gauge_style, true)
  @azimuth.GaugeStyle::set_animation_duration(gauge_style, 1000) // 1秒动画
  
  @azimuth.ThemedGaugeComponent::set_style(themed_gauge, gauge_style)
  
  // 注册主题化仪表盘组件
  @azimuth.ThemedGaugeDashboard::register_component(themed_dashboard, themed_gauge)
  
  // 生成测试数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  for i in 0..100 {
    let timestamp = base_timestamp + i * 60000000000 // 1分钟间隔
    
    // 生成模拟延迟数据
    let base_latency = 100.0
    let load_factor = 1.0 + 0.5 * @azimuth.Math::sin(i.to_float() / 20.0)
    let random_variation = (@azimuth.Random::next_float() - 0.5) * 50.0
    
    let latency = base_latency * load_factor + random_variation
    let clamped_latency = @azimuth.Math::max(0.0, @azimuth.Math::min(500.0, latency))
    
    // 创建数据点
    let data_point = @azimuth.GaugeDataPoint::new(timestamp, clamped_latency, [
      ("metric.name", @azimuth.StringValue("response.latency")),
      ("service.name", @azimuth.StringValue("web-service")),
      ("endpoint", @azimuth.StringValue(["/api/users", "/api/orders", "/api/products"][i % 3]))
    ])
    
    // 添加数据到仪表盘组件
    @azimuth.ThemedGaugeComponent::add_data_point(themed_gauge, data_point)
  }
  
  // 刷新仪表盘
  @azimuth.ThemedGaugeDashboard::refresh(themed_dashboard)
  
  // 验证主题应用效果
  let applied_theme = @azimuth.ThemedGaugeDashboard::get_current_theme(themed_dashboard)
  assert_eq(@azimuth.DashboardTheme::name(applied_theme), "Dark Professional")
  assert_eq(@azimuth.DashboardTheme::background_color(applied_theme), @azimuth.Color::RGB(30, 30, 30))
  assert_eq(@azimuth.DashboardTheme::foreground_color(applied_theme), @azimuth.Color::RGB(220, 220, 220))
  
  // 验证组件样式应用
  let component_theme = @azimuth.ThemedGaugeComponent::get_applied_theme(themed_gauge)
  let component_style = @azimuth.ThemedGaugeComponent::get_applied_style(themed_gauge)
  
  assert_true(component_theme != None)
  assert_true(component_style != None)
  
  match component_style {
    Some(style) => {
      assert_eq(@azimuth.GaugeStyle::stroke_width(style), 8)
      assert_eq(@azimuth.GaugeStyle::inner_radius(style), 0.7)
      assert_eq(@azimuth.GaugeStyle::start_angle(style), -135)
      assert_eq(@azimuth.GaugeStyle::end_angle(style), 135)
      assert_true(@azimuth.GaugeStyle::gradient_enabled(style))
      assert_true(@azimuth.GaugeStyle::animation_enabled(style))
    }
    None => assert_true(false)
  }
  
  // 测试主题切换
  let light_theme = @azimuth.DashboardTheme::new()
  @azimuth.DashboardTheme::set_name(light_theme, "Light Professional")
  @azimuth.DashboardTheme::set_background_color(light_theme, @azimuth.Color::RGB(255, 255, 255))
  @azimuth.DashboardTheme::set_foreground_color(light_theme, @azimuth.Color::RGB(30, 30, 30))
  @azimuth.DashboardTheme::set_primary_color(light_theme, @azimuth.Color::RGB(0, 123, 255))
  
  @azimuth.ThemedGaugeDashboard::apply_theme(themed_dashboard, light_theme)
  
  // 验证主题切换效果
  let switched_theme = @azimuth.ThemedGaugeDashboard::get_current_theme(themed_dashboard)
  assert_eq(@azimuth.DashboardTheme::name(switched_theme), "Light Professional")
  assert_eq(@azimuth.DashboardTheme::background_color(switched_theme), @azimuth.Color::RGB(255, 255, 255))
  assert_eq(@azimuth.DashboardTheme::foreground_color(switched_theme), @azimuth.Color::RGB(30, 30, 30))
  
  // 测试响应式主题
  let responsive_config = @azimuth.ResponsiveThemeConfig::new()
  @azimuth.ResponsiveThemeConfig::enable_dark_mode(responsive_config, true)
  @azimuth.ResponsiveThemeConfig::set_dark_mode_trigger(responsive_config, @azimuth.DarkModeTrigger::System)
  @azimuth.ResponsiveThemeConfig::enable_adaptive_colors(responsive_config, true)
  
  @azimuth.ThemedGaugeDashboard::configure_responsive_theme(themed_dashboard, responsive_config)
  
  // 模拟系统主题变化
  @azimuth.ThemedGaugeDashboard::simulate_system_theme_change(themed_dashboard, @azimuth.SystemTheme::Dark)
  
  // 验证响应式主题切换
  let responsive_theme = @azimuth.ThemedGaugeDashboard::get_current_theme(themed_dashboard)
  assert_eq(@azimuth.DashboardTheme::name(responsive_theme), "Dark Professional") // 应该切换回深色主题
  
  // 测试主题导出和导入
  let exported_theme = @azimuth.ThemedGaugeDashboard::export_theme(themed_dashboard)
  assert_true(@azimuth.ExportedTheme::is_valid(exported_theme))
  
  // 创建新仪表盘并导入主题
  let new_dashboard = @azimuth.ThemedGaugeDashboard::new()
  @azimuth.ThemedGaugeDashboard::import_theme(new_dashboard, exported_theme)
  
  // 验证主题导入效果
  let imported_theme = @azimuth.ThemedGaugeDashboard::get_current_theme(new_dashboard)
  assert_eq(@azimuth.DashboardTheme::name(imported_theme), "Dark Professional")
  
  assert_true(true)
}

// Test 5: 仪表盘性能优化和资源管理测试
test "dashboard performance optimization and resource management" {
  let performance_dashboard = @azimuth.PerformanceOptimizedDashboard::new()
  
  // 配置性能优化
  let performance_config = @azimuth.PerformanceConfig::new()
  @azimuth.PerformanceConfig::enable_lazy_loading(performance_config, true)
  @azimuth.PerformanceConfig::set_lazy_load_threshold(performance_config, 100) // 100个数据点阈值
  @azimuth.PerformanceConfig::enable_virtualization(performance_config, true)
  @azimuth.PerformanceConfig::set_virtualization_window(performance_config, 50) // 50个数据点窗口
  @azimuth.PerformanceConfig::enable_data_caching(performance_config, true)
  @azimuth.PerformanceConfig::set_cache_size(performance_config, 10000) // 10000个缓存点
  @azimuth.PerformanceConfig::enable_memory_optimization(performance_config, true)
  @azimuth.PerformanceConfig::set_memory_limit(performance_config, 52428800) // 50MB内存限制
  
  @azimuth.PerformanceOptimizedDashboard::configure(performance_dashboard, performance_config)
  
  // 创建性能监控器
  let performance_monitor = @azimuth.DashboardPerformanceMonitor::new()
  @azimuth.DashboardPerformanceMonitor::enable_memory_tracking(performance_monitor, true)
  @azimuth.DashboardPerformanceMonitor::enable_cpu_tracking(performance_monitor, true)
  @azimuth.DashboardPerformanceMonitor::enable_rendering_tracking(performance_monitor, true)
  @azimuth.DashboardPerformanceMonitor::enable_data_processing_tracking(performance_monitor, true)
  
  // 注册性能监控器
  @azimuth.PerformanceOptimizedDashboard::register_performance_monitor(performance_dashboard, performance_monitor)
  
  // 创建多个性能优化仪表盘组件
  let component_count = 10
  let mut performance_components = []
  
  for i in 0..component_count {
    let performance_gauge = @azimuth.PerformanceOptimizedGauge::new()
    @azimuth.PerformanceOptimizedGauge::set_title(performance_gauge, "Performance Gauge " + i.to_string())
    @azimuth.PerformanceOptimizedGauge::set_unit(performance_gauge, "units")
    @azimuth.PerformanceOptimizedGauge::set_min_value(performance_gauge, 0.0)
    @azimuth.PerformanceOptimizedGauge::set_max_value(performance_gauge, 100.0)
    
    // 配置组件级别的性能优化
    @azimuth.PerformanceOptimizedGauge::enable_data_throttling(performance_gauge, true)
    @azimuth.PerformanceOptimizedGauge::set_throttle_rate(performance_gauge, 10) // 10Hz最大更新率
    @azimuth.PerformanceOptimizedGauge::enable_rendering_optimization(performance_gauge, true)
    @azimuth.PerformanceOptimizedGauge::set_rendering_fps(performance_gauge, 30) // 30FPS最大渲染率
    
    // 注册组件
    @azimuth.PerformanceOptimizedDashboard::register_component(performance_dashboard, performance_gauge)
    performance_components = performance_components.push(performance_gauge)
  }
  
  // 生成大量测试数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  let large_dataset_size = 50000
  
  let data_generation_start = @azimuth.Time::now_unix_nanos()
  
  for i in 0..large_dataset_size {
    let timestamp = base_timestamp + i * 1000000000 // 1秒间隔
    
    // 为每个组件生成数据
    for j in 0..performance_components.length() - 1 {
      let performance_gauge = performance_components[j]
      
      // 生成模拟数据
      let value = 50.0 + 30.0 * @azimuth.Math::sin(i.to_float() / 100.0) + 10.0 * @azimuth.Math::cos(i.to_float() / 50.0) + @azimuth.Random::next_float() * 10.0
      let clamped_value = @azimuth.Math::max(0.0, @azimuth.Math::min(100.0, value))
      
      // 创建数据点
      let data_point = @azimuth.GaugeDataPoint::new(timestamp, clamped_value, [
        ("component.id", @azimuth.StringValue("perf-gauge-" + j.to_string())),
        ("data.batch", @azimuth.IntValue(i / 1000)),
        ("quality.score", @azimuth.FloatValue(0.8 + @azimuth.Random::next_float() * 0.2))
      ])
      
      // 添加数据到组件
      @azimuth.PerformanceOptimizedGauge::add_data_point(performance_gauge, data_point)
    }
    
    // 每5000个数据点检查一次性能
    if i % 5000 == 0 && i > 0 {
      let current_memory = @azimuth.DashboardPerformanceMonitor::get_memory_usage(performance_monitor)
      let current_cpu = @azimuth.DashboardPerformanceMonitor::get_cpu_usage(performance_monitor)
      
      // 验证资源使用在限制范围内
      assert_true(current_memory < 52428800) // 内存使用应该小于50MB
      assert_true(current_cpu < 80.0) // CPU使用应该小于80%
    }
  }
  
  let data_generation_end = @azimuth.Time::now_unix_nanos()
  let data_generation_time = data_generation_end - data_generation_start
  
  // 刷新仪表盘
  let refresh_start = @azimuth.Time::now_unix_nanos()
  @azimuth.PerformanceOptimizedDashboard::refresh(performance_dashboard)
  let refresh_end = @azimuth.Time::now_unix_nanos()
  let refresh_time = refresh_end - refresh_start
  
  // 获取性能报告
  let performance_report = @azimuth.DashboardPerformanceMonitor::get_report(performance_monitor)
  
  // 验证数据处理性能
  let data_processing_stats = @azimuth.PerformanceReport::data_processing_stats(performance_report)
  let total_data_points = @azimuth.DataProcessingStats::total_points(data_processing_stats)
  let processed_data_points = @azimuth.DataProcessingStats::processed_points(data_processing_stats)
  let processing_throughput = @azimuth.DataProcessingStats::throughput(data_processing_stats)
  let average_processing_time = @azimuth.DataProcessingStats::average_time(data_processing_stats)
  
  assert_eq(total_data_points, large_dataset_size * component_count)
  assert_true(processed_data_points > 0)
  assert_true(processing_throughput > 1000) // 处理吞吐量应该大于1000点/秒
  assert_true(average_processing_time < 1000000) // 平均处理时间应该小于1ms
  
  // 验证内存管理效果
  let memory_stats = @azimuth.PerformanceReport::memory_stats(performance_report)
  let peak_memory_usage = @azimuth.MemoryStats::peak_usage(memory_stats)
  let average_memory_usage = @azimuth.MemoryStats::average_usage(memory_stats)
  let memory_efficiency = @azimuth.MemoryStats::efficiency(memory_stats)
  let gc_frequency = @azimuth.MemoryStats::gc_frequency(memory_stats)
  
  assert_true(peak_memory_usage < 52428800) // 峰值内存使用应该小于50MB
  assert_true(average_memory_usage < 41943040) // 平均内存使用应该小于40MB
  assert_true(memory_efficiency > 0.8) // 内存效率应该大于80%
  assert_true(gc_frequency < 10) // 垃圾回收频率应该小于10次/分钟
  
  // 验证渲染性能
  let rendering_stats = @azimuth.PerformanceReport::rendering_stats(performance_report)
  let average_frame_time = @azimuth.RenderingStats::average_frame_time(rendering_stats)
  let frame_rate = @azimuth.RenderingStats::frame_rate(rendering_stats)
  let dropped_frames = @azimuth.RenderingStats::dropped_frames(rendering_stats)
  
  assert_true(average_frame_time < 33333333) // 平均帧时间应该小于33.33ms (30FPS)
  assert_true(frame_rate > 25) // 帧率应该大于25FPS
  assert_true(dropped_frames < total_data_points * 0.01) // 丢帧率应该小于1%
  
  // 验证缓存效果
  let cache_stats = @azimuth.PerformanceReport::cache_stats(performance_report)
  let cache_hit_rate = @azimuth.CacheStats::hit_rate(cache_stats)
  let cache_efficiency = @azimuth.CacheStats::efficiency(cache_stats)
  let cache_memory_usage = @azimuth.CacheStats::memory_usage(cache_stats)
  
  assert_true(cache_hit_rate > 0.8) // 缓存命中率应该大于80%
  assert_true(cache_efficiency > 0.7) // 缓存效率应该大于70%
  assert_true(cache_memory_usage < 10485760) // 缓存内存使用应该小于10MB
  
  // 测试虚拟化效果
  for component in performance_components {
    let virtualization_stats = @azimuth.PerformanceOptimizedGauge::get_virtualization_stats(component)
    let virtualized_data_points = @azimuth.VirtualizationStats::virtualized_points(virtualization_stats)
    let actual_rendered_points = @azimuth.VirtualizationStats::rendered_points(virtualization_stats)
    let virtualization_efficiency = @azimuth.VirtualizationStats::efficiency(virtualization_stats)
    
    assert_true(virtualized_data_points > actual_rendered_points) // 虚拟化数据点应该大于实际渲染点
    assert_true(virtualization_efficiency > 0.9) // 虚拟化效率应该大于90%
  }
  
  // 测试懒加载效果
  let lazy_loading_stats = @azimuth.PerformanceOptimizedDashboard::get_lazy_loading_stats(performance_dashboard)
  let loaded_components = @azimuth.LazyLoadingStats::loaded_components(lazy_loading_stats)
  let deferred_components = @azimuth.LazyLoadingStats::deferred_components(lazy_loading_stats)
  let loading_time_reduction = @azimuth.LazyLoadingStats::loading_time_reduction(lazy_loading_stats)
  
  assert_true(loaded_components <= component_count) // 加载的组件数应该不超过总组件数
  assert_true(deferred_components >= 0) // 延迟加载的组件数应该大于等于0
  assert_true(loading_time_reduction > 0.3) // 加载时间减少应该大于30%
  
  // 验证总体性能
  let overall_performance = @azimuth.PerformanceReport::overall_score(performance_report)
  assert_true(overall_performance > 0.8) // 总体性能评分应该大于80%
  
  assert_true(true)
}