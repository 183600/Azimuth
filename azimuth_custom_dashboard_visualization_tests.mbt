// Azimuth 自定义仪表板可视化测试用例
// 专注于验证遥测系统的仪表板创建、配置和可视化功能

// 测试1: 基本仪表板创建和配置
test "基本仪表板创建和配置" {
  let dashboard_manager = DashboardManager::new()
  
  // 创建仪表板配置
  let dashboard_config = {
    "id": "system-overview",
    "name": "System Overview Dashboard",
    "description": "Overall system performance and health metrics",
    "layout": {
      "type": "grid",
      "columns": 3,
      "rows": 2,
      "gap": 16
    },
    "refresh_interval": 30000, // 30秒
    "time_range": {
      "default": "1h",
      "options": ["15m", "1h", "6h", "24h", "7d"]
    },
    "theme": "light"
  }
  
  // 创建仪表板
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, dashboard_config)
  
  // 验证仪表板创建
  assert_eq(Dashboard::id(dashboard), "system-overview")
  assert_eq(Dashboard::name(dashboard), "System Overview Dashboard")
  assert_eq(Dashboard::refresh_interval(dashboard), 30000)
  assert_eq(Dashboard::layout_type(dashboard), "grid")
  assert_eq(Dashboard::layout_columns(dashboard), 3)
  assert_eq(Dashboard::layout_rows(dashboard), 2)
  
  // 添加仪表板变量
  Dashboard::add_variable(dashboard, "service", {
    "type": "select",
    "label": "Service",
    "options": ["api-gateway", "user-service", "payment-service", "order-service"],
    "default": "api-gateway"
  })
  
  Dashboard::add_variable(dashboard, "region", {
    "type": "select",
    "label": "Region",
    "options": ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"],
    "default": "us-east-1"
  })
  
  // 验证变量添加
  let variables = Dashboard::get_variables(dashboard)
  assert_eq(variables.length(), 2)
  assert_true(variables.contains_key("service"))
  assert_true(variables.contains_key("region"))
  
  // 更新仪表板配置
  Dashboard::update_config(dashboard, {
    "refresh_interval": 60000, // 更新为1分钟
    "theme": "dark"
  })
  
  // 验证配置更新
  assert_eq(Dashboard::refresh_interval(dashboard), 60000)
  assert_eq(Dashboard::theme(dashboard), "dark")
}

// 测试2: 图表组件创建和配置
test "图表组件创建和配置" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "charts-test",
    "name": "Charts Test Dashboard",
    "layout": { "type": "grid", "columns": 2, "rows": 2 }
  })
  
  // 创建时间序列图表
  let time_series_chart = ChartBuilder::time_series("request-rate", {
    "title": "Request Rate",
    "description": "HTTP requests per second",
    "position": { "x": 0, "y": 0, "w": 1, "h": 1 },
    "query": {
      "metric": "http.requests.total",
      "aggregation": "rate",
      "group_by": ["service"],
      "time_range": "$time_range"
    },
    "y_axis": {
      "label": "Requests/sec",
      "min": 0,
      "unit": "reqps"
    },
    "x_axis": {
      "label": "Time",
      "format": "time"
    },
    "legend": {
      "position": "bottom",
      "show": true
    }
  })
  
  // 创建饼图
  let pie_chart = ChartBuilder::pie("error-distribution", {
    "title": "Error Distribution",
    "description": "Distribution of error types",
    "position": { "x": 1, "y": 0, "w": 1, "h": 1 },
    "query": {
      "metric": "http.errors.total",
      "aggregation": "count",
      "group_by": ["error.type"]
    },
    "colors": ["#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7"]
  })
  
  // 创建仪表盘
  let gauge_chart = ChartBuilder::gauge("cpu-usage", {
    "title": "CPU Usage",
    "description": "Current CPU utilization",
    "position": { "x": 0, "y": 1, "w": 1, "h": 1 },
    "query": {
      "metric": "system.cpu.usage",
      "aggregation": "avg"
    },
    "min": 0,
    "max": 100,
    "thresholds": [
      { "value": 50, "color": "#45B7D1" },
      { "value": 80, "color": "#FFA500" },
      { "value": 90, "color": "#FF6B6B" }
    ],
    "unit": "%"
  })
  
  // 创建热力图
  let heatmap_chart = ChartBuilder::heatmap("response-time-heatmap", {
    "title": "Response Time Heatmap",
    "description": "Response times by endpoint and hour",
    "position": { "x": 1, "y": 1, "w": 1, "h": 1 },
    "query": {
      "metric": "http.response.duration",
      "aggregation": "avg",
      "group_by": ["endpoint", "hour"]
    },
    "color_scale": {
      "min": "#45B7D1",
      "max": "#FF6B6B"
    }
  })
  
  // 添加图表到仪表板
  Dashboard::add_chart(dashboard, time_series_chart)
  Dashboard::add_chart(dashboard, pie_chart)
  Dashboard::add_chart(dashboard, gauge_chart)
  Dashboard::add_chart(dashboard, heatmap_chart)
  
  // 验证图表添加
  let charts = Dashboard::get_charts(dashboard)
  assert_eq(charts.length(), 4)
  
  // 验证图表配置
  let request_rate_chart = Dashboard::get_chart(dashboard, "request-rate")
  assert_eq(request_rate_chart.type, "time_series")
  assert_eq(request_rate_chart.title, "Request Rate")
  assert_eq(request_rate_chart.position.x, 0)
  assert_eq(request_rate_chart.position.y, 0)
  
  let cpu_usage_chart = Dashboard::get_chart(dashboard, "cpu-usage")
  assert_eq(cpu_usage_chart.type, "gauge")
  assert_eq(cpu_usage_chart.min, 0)
  assert_eq(cpu_usage_chart.max, 100)
  assert_eq(cpu_usage_chart.thresholds.length(), 3)
}

// 测试3: 图表数据查询和更新
test "图表数据查询和更新" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "data-test",
    "name": "Data Test Dashboard"
  })
  
  // 创建测试图表
  let chart = ChartBuilder::time_series("test-chart", {
    "title": "Test Chart",
    "query": {
      "metric": "test.metric",
      "aggregation": "avg",
      "time_range": "1h"
    }
  })
  
  Dashboard::add_chart(dashboard, chart)
  
  // 模拟数据源
  let data_source = MockDataSource::new()
  DataSource::add_metric(data_source, "test.metric", [
    { "timestamp": 1640995200000, "value": 100.0, "tags": { "service": "api-gateway" } },
    { "timestamp": 1640995260000, "value": 120.0, "tags": { "service": "api-gateway" } },
    { "timestamp": 1640995320000, "value": 90.0, "tags": { "service": "api-gateway" } },
    { "timestamp": 1640995380000, "value": 110.0, "tags": { "service": "api-gateway" } }
  ])
  
  // 查询图表数据
  let chart_data = Dashboard::query_chart_data(dashboard, "test-chart", data_source)
  
  // 验证数据查询结果
  assert_true(chart_data.has_data)
  assert_eq(chart_data.series.length(), 1)
  assert_eq(chart_data.series[0].name, "api-gateway")
  assert_eq(chart_data.series[0].data_points.length(), 4)
  
  // 验证数据点
  let data_points = chart_data.series[0].data_points
  assert_eq(data_points[0].timestamp, 1640995200000)
  assert_eq(data_points[0].value, 100.0)
  assert_eq(data_points[1].value, 120.0)
  assert_eq(data_points[2].value, 90.0)
  assert_eq(data_points[3].value, 110.0)
  
  // 测试实时数据更新
  let updated_data_source = MockDataSource::new()
  DataSource::add_metric(updated_data_source, "test.metric", [
    { "timestamp": 1640995440000, "value": 115.0, "tags": { "service": "api-gateway" } },
    { "timestamp": 1640995500000, "value": 105.0, "tags": { "service": "api-gateway" } }
  ])
  
  // 更新图表数据
  let updated_chart_data = Dashboard::update_chart_data(dashboard, "test-chart", updated_data_source)
  
  // 验证数据更新
  assert_true(updated_chart_data.has_data)
  assert_eq(updated_chart_data.series[0].data_points.length(), 2)
  assert_eq(updated_chart_data.series[0].data_points[0].value, 115.0)
  assert_eq(updated_chart_data.series[0].data_points[1].value, 105.0)
  
  // 测试多变量查询
  Dashboard::add_variable(dashboard, "service", {
    "type": "select",
    "options": ["api-gateway", "user-service"],
    "default": "api-gateway"
  })
  
  let variable_chart = ChartBuilder::time_series("variable-chart", {
    "title": "Variable Chart",
    "query": {
      "metric": "test.metric",
      "aggregation": "avg",
      "filters": [{ "key": "service", "value": "$service" }],
      "time_range": "1h"
    }
  })
  
  Dashboard::add_chart(dashboard, variable_chart)
  
  // 使用变量查询数据
  let variable_data = Dashboard::query_chart_data_with_variables(
    dashboard, 
    "variable-chart", 
    data_source,
    { "service": "user-service" }
  )
  
  // 验证变量查询结果
  assert_true(variable_data.has_data)
}

// 测试4: 仪表板布局和响应式设计
test "仪表板布局和响应式设计" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "layout-test",
    "name": "Layout Test Dashboard",
    "layout": { "type": "grid", "columns": 4, "rows": 3 }
  })
  
  // 创建不同大小的图表
  let large_chart = ChartBuilder::time_series("large-chart", {
    "title": "Large Chart",
    "position": { "x": 0, "y": 0, "w": 2, "h": 2 }
  })
  
  let medium_chart = ChartBuilder::pie("medium-chart", {
    "title": "Medium Chart",
    "position": { "x": 2, "y": 0, "w": 2, "h": 1 }
  })
  
  let small_charts = []
  for i = 0; i < 4; i = i + 1 {
    let small_chart = ChartBuilder::gauge("small-chart-" + i.to_string(), {
      "title": "Small Chart " + i.to_string(),
      "position": { 
        "x": (i % 2) * 2, 
        "y": 1 + (i / 2), 
        "w": 2, 
        "h": 1 
      }
    })
    small_charts.push(small_chart)
  }
  
  // 添加图表到仪表板
  Dashboard::add_chart(dashboard, large_chart)
  Dashboard::add_chart(dashboard, medium_chart)
  for i = 0; i < small_charts.length(); i = i + 1 {
    Dashboard::add_chart(dashboard, small_charts[i])
  }
  
  // 验证布局
  let layout = Dashboard::get_layout(dashboard)
  assert_eq(layout.type, "grid")
  assert_eq(layout.columns, 4)
  assert_eq(layout.rows, 3)
  
  // 测试响应式布局
  let mobile_layout = Dashboard::get_responsive_layout(dashboard, "mobile")
  assert_eq(mobile_layout.columns, 1) // 移动设备单列布局
  
  let tablet_layout = Dashboard::get_responsive_layout(dashboard, "tablet")
  assert_eq(tablet_layout.columns, 2) // 平板双列布局
  
  let desktop_layout = Dashboard::get_responsive_layout(dashboard, "desktop")
  assert_eq(desktop_layout.columns, 4) // 桌面四列布局
  
  // 测试图表位置调整
  Dashboard::resize_chart(dashboard, "large-chart", { "w": 3, "h": 1 })
  Dashboard::move_chart(dashboard, "medium-chart", { "x": 0, "y": 1 })
  
  // 验证图表位置调整
  let resized_chart = Dashboard::get_chart(dashboard, "large-chart")
  assert_eq(resized_chart.position.w, 3)
  assert_eq(resized_chart.position.h, 1)
  
  let moved_chart = Dashboard::get_chart(dashboard, "medium-chart")
  assert_eq(moved_chart.position.x, 0)
  assert_eq(moved_chart.position.y, 1)
  
  // 测试布局冲突检测
  let conflict_chart = ChartBuilder::time_series("conflict-chart", {
    "title": "Conflict Chart",
    "position": { "x": 0, "y": 1, "w": 2, "h": 1 } // 与medium-chart冲突
  })
  
  let has_conflict = Dashboard::check_layout_conflict(dashboard, conflict_chart)
  assert_true(has_conflict)
}

// 测试5: 仪表板主题和样式定制
test "仪表板主题和样式定制" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "theme-test",
    "name": "Theme Test Dashboard",
    "theme": "light"
  })
  
  // 创建自定义主题
  let custom_theme = {
    "name": "custom-dark",
    "type": "dark",
    "colors": {
      "background": "#1a1a1a",
      "surface": "#2d2d2d",
      "primary": "#4fc3f7",
      "secondary": "#81c784",
      "error": "#e57373",
      "warning": "#ffb74d",
      "info": "#64b5f6",
      "text": {
        "primary": "#ffffff",
        "secondary": "#b0b0b0"
      },
      "grid": {
        "color": "#404040",
        "width": 1
      }
    },
    "typography": {
      "font_family": "Inter, sans-serif",
      "font_size": {
        "small": "12px",
        "medium": "14px",
        "large": "16px",
        "xlarge": "18px"
      }
    },
    "spacing": {
      "small": "8px",
      "medium": "16px",
      "large": "24px"
    },
    "border_radius": "4px",
    "shadow": "0 2px 8px rgba(0, 0, 0, 0.3)"
  }
  
  // 应用自定义主题
  Dashboard::apply_theme(dashboard, custom_theme)
  
  // 验证主题应用
  let current_theme = Dashboard::get_theme(dashboard)
  assert_eq(current_theme.name, "custom-dark")
  assert_eq(current_theme.type, "dark")
  assert_eq(current_theme.colors.background, "#1a1a1a")
  assert_eq(current_theme.colors.primary, "#4fc3f7")
  
  // 创建图表并应用主题样式
  let chart = ChartBuilder::time_series("themed-chart", {
    "title": "Themed Chart",
    "colors": ["$primary", "$secondary", "$warning"],
    "background_color": "$surface",
    "text_color": "$text.primary",
    "grid_color": "$grid.color"
  })
  
  Dashboard::add_chart(dashboard, chart)
  
  // 验证图表主题样式
  let themed_chart = Dashboard::get_chart(dashboard, "themed-chart")
  assert_eq(themed_chart.colors[0], "#4fc3f7") // $primary
  assert_eq(themed_chart.colors[1], "#81c784") // $secondary
  assert_eq(themed_chart.colors[2], "#ffb74d") // $warning
  assert_eq(themed_chart.background_color, "#2d2d2d") // $surface
  assert_eq(themed_chart.text_color, "#ffffff") // $text.primary
  
  // 测试主题切换
  Dashboard::switch_theme(dashboard, "light")
  let light_theme = Dashboard::get_theme(dashboard)
  assert_eq(light_theme.type, "light")
  
  // 测试主题变量
  Dashboard::add_theme_variable(dashboard, "brand_color", "#4fc3f7")
  Dashboard::add_theme_variable(dashboard, "success_color", "#81c784")
  
  let theme_variables = Dashboard::get_theme_variables(dashboard)
  assert_eq(theme_variables["brand_color"], "#4fc3f7")
  assert_eq(theme_variables["success_color"], "#81c784")
}

// 测试6: 仪表板导出和分享
test "仪表板导出和分享" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "share-test",
    "name": "Share Test Dashboard",
    "description": "Dashboard for testing export and sharing"
  })
  
  // 添加一些图表
  let chart1 = ChartBuilder::time_series("chart1", { "title": "Chart 1" })
  let chart2 = ChartBuilder::pie("chart2", { "title": "Chart 2" })
  
  Dashboard::add_chart(dashboard, chart1)
  Dashboard::add_chart(dashboard, chart2)
  
  // 添加变量
  Dashboard::add_variable(dashboard, "service", {
    "type": "select",
    "options": ["service1", "service2"],
    "default": "service1"
  })
  
  // 测试JSON导出
  let json_export = Dashboard::export_json(dashboard)
  
  // 验证JSON导出
  assert_true(json_export.contains("\"id\":\"share-test\""))
  assert_true(json_export.contains("\"name\":\"Share Test Dashboard\""))
  assert_true(json_export.contains("\"chart1\""))
  assert_true(json_export.contains("\"chart2\""))
  assert_true(json_export.contains("\"service\""))
  
  // 测试配置导出（不包含数据）
  let config_export = Dashboard::export_config(dashboard)
  let config = Json::parse(config_export)
  
  // 验证配置导出
  assert_eq(config["id"], "share-test")
  assert_eq(config["name"], "Share Test Dashboard")
  assert_eq(config["charts"].length(), 2)
  assert_eq(config["variables"].length(), 1)
  
  // 测试仪表板分享
  let share_settings = {
    "public": false,
    "allow_export": true,
    "allow_copy": true,
    "expire_time": null,
    "password": null
  }
  
  let share_link = Dashboard::create_share_link(dashboard, share_settings)
  
  // 验证分享链接
  assert_true(share_link.contains("/dashboard/share/"))
  assert_true(share_link.length() > 50) // 应该包含足够长的token
  
  // 测试分享链接访问
  let shared_dashboard = DashboardManager::load_from_share_link(dashboard_manager, share_link)
  
  // 验证分享的仪表板
  assert_eq(shared_dashboard.id, "share-test")
  assert_eq(shared_dashboard.name, "Share Test Dashboard")
  assert_eq(shared_dashboard.charts.length(), 2)
  
  // 测试仪表板模板创建
  let template = DashboardManager::create_template(dashboard, {
    "name": "System Monitoring Template",
    "description": "Template for system monitoring dashboards",
    "category": "monitoring",
    "tags": ["system", "monitoring", "performance"]
  })
  
  // 验证模板创建
  assert_eq(template.name, "System Monitoring Template")
  assert_eq(template.category, "monitoring")
  assert_true(template.tags.contains("system"))
  assert_true(template.tags.contains("monitoring"))
  
  // 测试从模板创建仪表板
  let new_dashboard = DashboardManager::create_from_template(dashboard_manager, template.id, {
    "name": "My System Dashboard",
    "variables": {
      "service": {
        "default": "my-service",
        "options": ["my-service", "another-service"]
      }
    }
  })
  
  // 验证从模板创建的仪表板
  assert_eq(new_dashboard.name, "My System Dashboard")
  assert_eq(new_dashboard.charts.length(), 2) // 继承模板的图表
  assert_true(new_dashboard.variables.contains_key("service"))
  assert_eq(new_dashboard.variables["service"]["default"], "my-service")
}

// 测试7: 仪表板权限和访问控制
test "仪表板权限和访问控制" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "auth-test",
    "name": "Auth Test Dashboard",
    "owner": "admin"
  })
  
  // 创建用户和角色
  let admin_user = User::new("admin", ["admin"])
  let viewer_user = User::new("viewer", ["viewer"])
  let editor_user = User::new("editor", ["editor"])
  let guest_user = User::new("guest", ["guest"])
  
  // 设置仪表板权限
  Dashboard::set_permission(dashboard, {
    "owner": "admin",
    "viewers": ["viewer"],
    "editors": ["editor"],
    "public": false
  })
  
  // 测试管理员权限
  let admin_permissions = Dashboard::check_permissions(dashboard, admin_user)
  assert_true(admin_permissions.can_view)
  assert_true(admin_permissions.can_edit)
  assert_true(admin_permissions.can_share)
  assert_true(admin_permissions.can_delete)
  
  // 测试查看者权限
  let viewer_permissions = Dashboard::check_permissions(dashboard, viewer_user)
  assert_true(viewer_permissions.can_view)
  assert_false(viewer_permissions.can_edit)
  assert_false(viewer_permissions.can_share)
  assert_false(viewer_permissions.can_delete)
  
  // 测试编辑者权限
  let editor_permissions = Dashboard::check_permissions(dashboard, editor_user)
  assert_true(editor_permissions.can_view)
  assert_true(editor_permissions.can_edit)
  assert_false(editor_permissions.can_share)
  assert_false(editor_permissions.can_delete)
  
  // 测试访客权限
  let guest_permissions = Dashboard::check_permissions(dashboard, guest_user)
  assert_false(guest_permissions.can_view)
  assert_false(guest_permissions.can_edit)
  assert_false(guest_permissions.can_share)
  assert_false(guest_permissions.can_delete)
  
  // 测试权限修改
  Dashboard::add_viewer(dashboard, "guest")
  Dashboard::add_editor(dashboard, "viewer")
  Dashboard::remove_editor(dashboard, "editor")
  
  // 验证权限修改
  let updated_guest_permissions = Dashboard::check_permissions(dashboard, guest_user)
  assert_true(updated_guest_permissions.can_view)
  
  let updated_viewer_permissions = Dashboard::check_permissions(dashboard, viewer_user)
  assert_true(updated_viewer_permissions.can_edit)
  
  let updated_editor_permissions = Dashboard::check_permissions(dashboard, editor_user)
  assert_false(updated_editor_permissions.can_edit)
  
  // 测试角色权限
  let role_permissions = {
    "admin": {
      "can_view": true,
      "can_edit": true,
      "can_share": true,
      "can_delete": true
    },
    "editor": {
      "can_view": true,
      "can_edit": true,
      "can_share": false,
      "can_delete": false
    },
    "viewer": {
      "can_view": true,
      "can_edit": false,
      "can_share": false,
      "can_delete": false
    },
    "guest": {
      "can_view": false,
      "can_edit": false,
      "can_share": false,
      "can_delete": false
    }
  }
  
  DashboardManager::set_role_permissions(dashboard_manager, role_permissions)
  
  // 验证角色权限应用
  let role_based_admin_permissions = DashboardManager::check_role_permissions(dashboard_manager, admin_user)
  assert_eq(role_based_admin_permissions.can_edit, role_permissions["admin"]["can_edit"])
  
  let role_based_viewer_permissions = DashboardManager::check_role_permissions(dashboard_manager, viewer_user)
  assert_eq(role_based_viewer_permissions.can_edit, role_permissions["viewer"]["can_edit"])
}

// 测试8: 仪表板性能优化
test "仪表板性能优化" {
  let dashboard_manager = DashboardManager::new()
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, {
    "id": "performance-test",
    "name": "Performance Test Dashboard",
    "refresh_interval": 30000
  })
  
  // 创建多个图表
  let charts = []
  for i = 0; i < 20; i = i + 1 {
    let chart = ChartBuilder::time_series("chart-" + i.to_string(), {
      "title": "Chart " + i.to_string(),
      "query": {
        "metric": "metric." + i.to_string(),
        "aggregation": "avg",
        "time_range": "1h"
      }
    })
    charts.push(chart)
    Dashboard::add_chart(dashboard, chart)
  }
  
  // 启用数据缓存
  Dashboard::enable_caching(dashboard, {
    "enabled": true,
    "ttl": 60000, // 1分钟
    "max_size": 100
  })
  
  // 验证缓存配置
  let cache_config = Dashboard::get_cache_config(dashboard)
  assert_true(cache_config.enabled)
  assert_eq(cache_config.ttl, 60000)
  assert_eq(cache_config.max_size, 100)
  
  // 模拟数据源
  let data_source = MockDataSource::new()
  for i = 0; i < 20; i = i + 1 {
    let metric_data = []
    for j = 0; j < 60; j = j + 1 { // 60个数据点
      metric_data.push({
        "timestamp": 1640995200000 + (j * 60000),
        "value": 100.0 + (i.to_float() * 10.0) + (Random::float() * 20.0 - 10.0)
      })
    }
    DataSource::add_metric(data_source, "metric." + i.to_string(), metric_data)
  }
  
  // 测试首次数据加载（无缓存）
  let start_time = Time::now()
  Dashboard::load_all_chart_data(dashboard, data_source)
  let first_load_time = Time::now() - start_time
  
  // 测试缓存数据加载
  start_time = Time::now()
  Dashboard::load_all_chart_data(dashboard, data_source)
  let cached_load_time = Time::now() - start_time
  
  // 验证缓存效果
  assert_true(cached_load_time < first_load_time / 2) // 缓存加载应该至少快2倍
  
  // 测试懒加载
  Dashboard::enable_lazy_loading(dashboard, {
    "enabled": true,
    "threshold": 5, // 超过5个图表启用懒加载
    "load_priority": "visible" // 优先加载可见图表
  })
  
  // 验证懒加载配置
  let lazy_config = Dashboard::get_lazy_loading_config(dashboard)
  assert_true(lazy_config.enabled)
  assert_eq(lazy_config.threshold, 5)
  
  // 测试增量更新
  Dashboard::enable_incremental_updates(dashboard, {
    "enabled": true,
    "interval": 10000, // 10秒
    "strategy": "latest" // 只获取最新数据点
  })
  
  // 验证增量更新配置
  let incremental_config = Dashboard::get_incremental_update_config(dashboard)
  assert_true(incremental_config.enabled)
  assert_eq(incremental_config.interval, 10000)
  
  // 测试性能监控
  let performance_metrics = Dashboard::get_performance_metrics(dashboard)
  
  // 验证性能指标
  assert_true(performance_metrics.contains_key("load_time"))
  assert_true(performance_metrics.contains_key("cache_hit_rate"))
  assert_true(performance_metrics.contains_key("chart_count"))
  assert_true(performance_metrics.contains_key("data_points"))
  
  // 测试性能优化建议
  let optimization_suggestions = Dashboard::get_optimization_suggestions(dashboard)
  
  // 验证优化建议
  assert_true(optimization_suggestions.length() > 0)
  
  // 检查是否包含缓存相关建议
  let has_cache_suggestion = false
  for i = 0; i < optimization_suggestions.length(); i = i + 1 {
    if (optimization_suggestions[i].type == "cache") {
      has_cache_suggestion = true
      break
    }
  }
  assert_true(has_cache_suggestion)
}