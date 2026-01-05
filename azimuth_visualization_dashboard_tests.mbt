// Azimuth 遥测系统 - 可视化报表生成测试
// 专注于遥测数据的可视化、报表生成和数据分析功能

// 测试1: 实时仪表板生成
test "实时仪表板生成" {
  // 创建仪表板管理器
  let dashboard_manager = DashboardManager::new()
  
  // 配置仪表板模板
  dashboard_manager.configure_template("service_overview", {
    layout: "grid_2x2",
    refresh_interval: 30,  // 30秒刷新
    widgets: [
      { type: "metric_chart", title: "请求速率", metric: "request_rate", aggregation: "sum" },
      { type: "metric_chart", title: "错误率", metric: "error_rate", aggregation: "avg" },
      { type: "metric_chart", title: "响应时间", metric: "response_time", aggregation: "p95" },
      { type: "status_list", title: "服务状态", services: ["api-gateway", "order-service", "payment-service"] }
    ]
  })
  
  dashboard_manager.configure_template("infrastructure_health", {
    layout: "grid_3x2",
    refresh_interval: 60,  // 60秒刷新
    widgets: [
      { type: "resource_gauge", title: "CPU使用率", resource: "cpu", threshold: 80 },
      { type: "resource_gauge", title: "内存使用率", resource: "memory", threshold: 85 },
      { type: "resource_gauge", title: "磁盘使用率", resource: "disk", threshold: 90 },
      { type: "network_chart", title: "网络流量", metric: "network_bytes" },
      { type: "alert_panel", title: "活跃告警", severity: ["critical", "warning"] },
      { type: "log_stream", title: "最新日志", level: ["error", "warn"] }
    ]
  })
  
  // 生成模拟实时数据
  let real_time_data = generate_real_time_telemetry_data(
    services=["api-gateway", "order-service", "payment-service"],
    duration_minutes=10,
    interval_seconds=5
  )
  
  // 创建服务概览仪表板
  let service_dashboard = dashboard_manager.create_dashboard(
    template="service_overview",
    title="服务概览仪表板",
    time_range="last_1h"
  )
  
  // 填充数据
  dashboard_manager.populate_dashboard(service_dashboard.id, real_time_data)
  
  // 验证仪表板生成
  assert_true(service_dashboard.id.length() > 0)
  assert_eq(service_dashboard.title, "服务概览仪表板")
  assert_true(service_dashboard.widgets.length() == 4)
  
  // 验证各组件数据
  for widget in service_dashboard.widgets {
    assert_true(widget.data.length() > 0)
    assert_true(widget.last_updated > 0)
    
    match widget.type {
      "metric_chart" => {
        assert_true(widget.chart_config.type == "line" || widget.chart_config.type == "area")
        assert_true(widget.chart_config.x_axis == "time")
        assert_true(widget.chart_config.y_axis.length() > 0)
      }
      "status_list" => {
        assert_true(widget.status_items.length() > 0)
        for item in widget.status_items {
          assert_true(item.name.length() > 0)
          assert_true(item.status == "healthy" || item.status == "unhealthy" || item.status == "degraded")
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 创建基础设施健康仪表板
  let infra_dashboard = dashboard_manager.create_dashboard(
    template="infrastructure_health",
    title="基础设施健康仪表板",
    time_range="last_24h"
  )
  
  // 填充基础设施数据
  let infra_data = generate_infrastructure_telemetry_data(
    resources=["cpu", "memory", "disk", "network"],
    duration_minutes=60,
    interval_seconds=30
  )
  
  dashboard_manager.populate_dashboard(infra_dashboard.id, infra_data)
  
  // 验证基础设施仪表板
  assert_true(infra_dashboard.widgets.length() == 6)
  
  // 验证资源仪表组件
  let resource_widgets = infra_dashboard.widgets.filter({ widget => widget.type == "resource_gauge" })
  for widget in resource_widgets {
    assert_true(widget.gauge_value >= 0.0 && widget.gauge_value <= 100.0)
    assert_true(widget.threshold > 0.0 && widget.threshold <= 100.0)
    assert_true(widget.status == "normal" || widget.status == "warning" || widget.status == "critical")
  }
  
  // 测试仪表板实时更新
  let update_data = generate_real_time_telemetry_data(
    services=["api-gateway", "order-service", "payment-service"],
    duration_minutes=2,
    interval_seconds=10
  )
  
  let update_result = dashboard_manager.update_dashboard(service_dashboard.id, update_data)
  assert_true(update_result.success)
  assert_true(update_result.updated_widgets > 0)
  
  // 验证更新后的时间戳
  let updated_dashboard = dashboard_manager.get_dashboard(service_dashboard.id)
  let current_time = get_current_timestamp()
  for widget in updated_dashboard.widgets {
    assert_true(current_time - widget.last_updated <= 60)  // 应该在最近60秒内更新
  }
  
  // 测试仪表板导出
  let export_result = dashboard_manager.export_dashboard(service_dashboard.id, "json")
  assert_true(export_result.success)
  assert_true(export_result.content.length() > 0)
  
  let pdf_export = dashboard_manager.export_dashboard(service_dashboard.id, "pdf")
  assert_true(pdf_export.success)
  assert_true(pdf_export.file_size_bytes > 0)
  
  // 停止仪表板管理器
  dashboard_manager.stop()
}

// 测试2: 自定义报表生成
test "自定义报表生成" {
  // 创建报表管理器
  let report_manager = ReportManager::new()
  
  // 配置报表模板
  report_manager.configure_template("performance_summary", {
    name: "性能摘要报表",
    description: "系统性能综合分析报表",
    sections: [
      {
        title: "执行摘要",
        type: "summary",
        metrics: ["avg_response_time", "error_rate", "throughput", "availability"]
      },
      {
        title: "服务性能详情",
        type: "service_details",
        services: ["api-gateway", "order-service", "payment-service"],
        metrics: ["request_count", "avg_response_time", "p95_response_time", "error_rate"]
      },
      {
        title: "趋势分析",
        type: "trend_analysis",
        time_ranges: ["last_24h", "last_7d", "last_30d"],
        metrics: ["response_time", "error_rate", "throughput"]
      },
      {
        title: "异常检测",
        type: "anomaly_detection",
        algorithms: ["statistical", "ml_based"],
        sensitivity: "medium"
      }
    ],
    format: "html",
    include_charts: true,
    include_raw_data: false
  })
  
  report_manager.configure_template("sla_compliance", {
    name: "SLA合规性报表",
    description: "服务级别协议合规性分析",
    sections: [
      {
        title: "SLA概览",
        type: "sla_overview",
        services: ["critical_services"],
        sla_targets: ["availability", "response_time", "error_rate"]
      },
      {
        title: "合规性详情",
        type: "compliance_details",
        time_period: "monthly",
        grace_periods: true
      },
      {
        title: "违规分析",
        type: "violation_analysis",
        root_cause_analysis: true
      }
    ],
    format: "pdf",
    include_charts: true,
    include_raw_data: true
  })
  
  // 生成报表数据
  let report_data = generate_report_telemetry_data(
    start_date="2023-01-01",
    end_date="2023-01-31",
    services=["api-gateway", "order-service", "payment-service"],
    granularity="hourly"
  )
  
  // 创建性能摘要报表
  let performance_report = report_manager.create_report(
    template="performance_summary",
    title="2023年1月性能摘要",
    data=report_data,
    output_format="html"
  )
  
  // 等待报表生成完成
  let report_completed = report_manager.wait_for_completion(performance_report.id, timeout_seconds=120)
  assert_true(report_completed)
  
  // 验证报表生成结果
  let report_result = report_manager.get_report_result(performance_report.id)
  assert_true(report_result.success)
  assert_true(report_result.file_size_bytes > 0)
  assert_eq(report_result.format, "html")
  
  // 验证报表内容
  let report_content = report_manager.get_report_content(performance_report.id)
  assert_true(report_content.contains("<html>"))
  assert_true(report_content.contains("性能摘要报表"))
  assert_true(report_content.contains("执行摘要"))
  assert_true(report_content.contains("服务性能详情"))
  
  // 创建SLA合规性报表
  let sla_data = generate_sla_telemetry_data(
    start_date="2023-01-01",
    end_date="2023-01-31",
    services=["api-gateway", "order-service", "payment-service"],
    sla_targets={
      "api-gateway": { availability: 99.9, response_time: 200, error_rate: 0.01 },
      "order-service": { availability: 99.5, response_time: 500, error_rate: 0.05 },
      "payment-service": { availability: 99.99, response_time: 100, error_rate: 0.005 }
    }
  )
  
  let sla_report = report_manager.create_report(
    template="sla_compliance",
    title="2023年1月SLA合规性报告",
    data=sla_data,
    output_format="pdf"
  )
  
  // 等待SLA报表生成完成
  let sla_report_completed = report_manager.wait_for_completion(sla_report.id, timeout_seconds=180)
  assert_true(sla_report_completed)
  
  // 验证SLA报表
  let sla_report_result = report_manager.get_report_result(sla_report.id)
  assert_true(sla_report_result.success)
  assert_eq(sla_report_result.format, "pdf")
  
  // 测试自定义报表模板
  let custom_template = {
    name: "自定义错误分析报表",
    sections: [
      {
        title: "错误分布",
        type: "error_distribution",
        group_by: "service",
        time_range: "last_7d"
      },
      {
        title: "错误趋势",
        type: "error_trends",
        chart_type: "stacked_area"
      },
      {
        title: "TOP错误",
        type: "top_errors",
        limit: 10
      }
    ],
    format: "json",
    include_charts: false,
    include_raw_data: true
  }
  
  let custom_template_id = report_manager.create_custom_template(custom_template)
  assert_true(custom_template_id.length() > 0)
  
  // 生成自定义报表
  let error_data = generate_error_telemetry_data(
    start_date="2023-01-25",
    end_date="2023-01-31",
    services=["api-gateway", "order-service", "payment-service"]
  )
  
  let custom_report = report_manager.create_report(
    template=custom_template_id,
    title="错误分析自定义报表",
    data=error_data,
    output_format="json"
  )
  
  // 等待自定义报表生成
  let custom_report_completed = report_manager.wait_for_completion(custom_report.id, timeout_seconds=90)
  assert_true(custom_report_completed)
  
  // 验证自定义报表
  let custom_report_result = report_manager.get_report_result(custom_report.id)
  assert_true(custom_report_result.success)
  assert_eq(custom_report_result.format, "json")
  
  // 验证JSON报表内容
  let json_content = report_manager.get_report_content(custom_report.id)
  let parsed_json = parse_json(json_content)
  assert_true(parsed_json.contains("错误分布"))
  assert_true(parsed_json.contains("错误趋势"))
  
  // 测试计划报表生成
  let schedule_config = {
    template: "performance_summary",
    name: "每日性能报表",
    schedule: "0 8 * * *",  // 每天早上8点
    recipients: ["admin@example.com", "ops@example.com"],
    formats: ["html", "pdf"],
    retention_days: 90
  }
  
  let schedule_id = report_manager.schedule_report(schedule_config)
  assert_true(schedule_id.length() > 0)
  
  // 验证计划报表配置
  let schedule_info = report_manager.get_schedule_info(schedule_id)
  assert_eq(schedule_info.name, "每日性能报表")
  assert_eq(schedule_info.schedule, "0 8 * * *")
  assert_true(schedule_info.is_active)
  
  // 测试报表历史记录
  let report_history = report_manager.get_report_history(limit=10)
  assert_true(report_history.length() >= 3)  // 至少包含我们生成的3个报表
  
  // 验证历史记录完整性
  for record in report_history {
    assert_true(record.id.length() > 0)
    assert_true(record.title.length() > 0)
    assert_true(record.generation_time > 0)
    assert_true(record.file_size_bytes > 0)
  }
  
  // 停止报表管理器
  report_manager.stop()
}

// 测试3: 数据可视化图表生成
test "数据可视化图表生成" {
  // 创建可视化生成器
  let visualization_generator = VisualizationGenerator::new()
  
  // 配置图表类型
  visualization_generator.configure_chart_type("time_series", {
    default_options: {
      width: 800,
      height: 400,
      animation: true,
      responsive: true,
      interaction_mode: "index"
    },
    supported_formats: ["svg", "png", "jpg", "pdf"]
  })
  
  visualization_generator.configure_chart_type("heatmap", {
    default_options: {
      width: 600,
      height: 500,
      color_scheme: "viridis",
      show_values: false
    },
    supported_formats: ["svg", "png", "jpg"]
  })
  
  visualization_generator.configure_chart_type("sankey", {
    default_options: {
      width: 900,
      height: 600,
      node_alignment: "justify",
      link_color: "gradient"
    },
    supported_formats: ["svg", "png"]
  })
  
  // 生成时间序列数据
  let time_series_data = generate_time_series_data(
    metrics=["response_time", "error_rate", "throughput"],
    start_time=1640995200,
    end_time=1641081600,
    interval=3600  // 1小时间隔
  )
  
  // 创建时间序列图表
  let time_series_chart = visualization_generator.create_chart(
    type="time_series",
    title="系统性能时间序列",
    data=time_series_data,
    options={
      x_axis: { label: "时间", format: "datetime" },
      y_axis: { label: "数值", format: "auto" },
      series: [
        { name: "响应时间", color: "#3498db", axis: "left" },
        { name: "错误率", color: "#e74c3c", axis: "right" },
        { name: "吞吐量", color: "#2ecc71", axis: "right" }
      ]
    }
  )
  
  // 验证时间序列图表
  assert_true(time_series_chart.id.length() > 0)
  assert_eq(time_series_chart.title, "系统性能时间序列")
  assert_eq(time_series_chart.type, "time_series")
  assert_true(time_series_chart.data_points > 0)
  
  // 导出不同格式
  let svg_export = visualization_generator.export_chart(time_series_chart.id, "svg")
  assert_true(svg_export.success)
  assert_true(svg_export.content.contains("<svg"))
  
  let png_export = visualization_generator.export_chart(time_series_chart.id, "png")
  assert_true(png_export.success)
  assert_true(png_export.file_size_bytes > 0)
  
  // 生成热力图数据
  let heatmap_data = generate_heatmap_data(
    x_axis=["周一", "周二", "周三", "周四", "周五", "周六", "周日"],
    y_axis=["00:00", "06:00", "12:00", "18:00"],
    metric="request_count"
  )
  
  // 创建热力图
  let heatmap_chart = visualization_generator.create_chart(
    type="heatmap",
    title="一周请求量热力图",
    data=heatmap_data,
    options={
      color_scale: "sequential",
      show_legend: true,
      tooltip_format: "{x}, {y}: {value} 请求"
    }
  )
  
  // 验证热力图
  assert_eq(heatmap_chart.type, "heatmap")
  assert_true(heatmap_chart.width == 600)
  assert_true(heatmap_chart.height == 500)
  
  // 生成桑基图数据
  let sankey_data = generate_sankey_data(
    nodes=[
      { id: "用户", label: "用户访问" },
      { id: "api-gateway", label: "API网关" },
      { id: "order-service", label: "订单服务" },
      { id: "payment-service", label: "支付服务" },
      { id: "notification-service", label: "通知服务" },
      { id: "完成", label: "完成" }
    ],
    links=[
      { source: "用户", target: "api-gateway", value: 1000 },
      { source: "api-gateway", target: "order-service", value: 600 },
      { source: "api-gateway", target: "payment-service", value: 300 },
      { source: "order-service", target: "payment-service", value: 500 },
      { source: "order-service", target: "notification-service", value: 100 },
      { source: "payment-service", target: "完成", value: 800 },
      { source: "notification-service", target: "完成", value: 100 }
    ]
  )
  
  // 创建桑基图
  let sankey_chart = visualization_generator.create_chart(
    type="sankey",
    title="请求流向桑基图",
    data=sankey_data,
    options={
      link_color: "gradient",
      node_width: 30,
      node_padding: 10,
      auto_sort: true
    }
  )
  
  // 验证桑基图
  assert_eq(sankey_chart.type, "sankey")
  assert_true(sankey_chart.width == 900)
  assert_true(sankey_chart.height == 600)
  
  // 创建交互式图表
  let interactive_config = {
    zoom: true,
    pan: true,
    crosshair: true,
    data_labels: true,
    legend: {
      interactive: true,
      position: "top"
    },
    tooltip: {
      enabled: true,
      format: "custom",
      shared: true
    }
  }
  
  let interactive_chart = visualization_generator.create_chart(
    type="time_series",
    title="交互式性能图表",
    data=time_series_data,
    options=interactive_config
  )
  
  // 验证交互式功能
  assert_true(interactive_chart.interactive_features.zoom)
  assert_true(interactive_chart.interactive_features.pan)
  assert_true(interactive_chart.interactive_features.crosshair)
  
  // 测试图表组合
  let dashboard_config = {
    layout: "grid_2x2",
    title: "系统监控仪表板",
    charts: [
      { id: time_series_chart.id, position: { row: 1, col: 1, width: 2, height: 1 } },
      { id: heatmap_chart.id, position: { row: 2, col: 1, width: 1, height: 1 } },
      { id: sankey_chart.id, position: { row: 2, col: 2, width: 1, height: 1 } }
    ],
    theme: "light",
    responsive: true
  }
  
  let dashboard = visualization_generator.create_dashboard(dashboard_config)
  assert_true(dashboard.id.length() > 0)
  assert_eq(dashboard.title, "系统监控仪表板")
  assert_true(dashboard.charts.length() == 3)
  
  // 导出整个仪表板
  let dashboard_export = visualization_generator.export_dashboard(dashboard.id, "html")
  assert_true(dashboard_export.success)
  assert_true(dashboard_export.content.contains("<html"))
  assert_true(dashboard_export.content.contains("系统监控仪表板"))
  
  // 测试图表样式定制
  let custom_theme = {
    name: "dark_theme",
    colors: ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"],
    background: "#2b2b2b",
    text: "#ffffff",
    grid: "#444444"
  }
  
  let themed_chart = visualization_generator.create_chart(
    type="time_series",
    title="深色主题图表",
    data=time_series_data,
    options={ theme: custom_theme }
  )
  
  // 验证主题应用
  assert_eq(themed_chart.theme.name, "dark_theme")
  assert_eq(themed_chart.theme.background, "#2b2b2b")
  
  // 停止可视化生成器
  visualization_generator.stop()
}

// 测试4: 数据分析和洞察生成
test "数据分析和洞察生成" {
  // 创建数据分析器
  let data_analyzer = DataAnalyzer::new()
  
  // 配置分析算法
  data_analyzer.configure_algorithm("anomaly_detection", {
    type: "isolation_forest",
    parameters: {
      contamination: 0.1,
      n_estimators: 100,
      max_features: "auto"
    },
    sensitivity: "medium"
  })
  
  data_analyzer.configure_algorithm("trend_analysis", {
    type: "seasonal_decomposition",
    parameters: {
      model: "additive",
      period: 24,  // 24小时季节性
      extrapolation_periods: 7
    }
  })
  
  data_analyzer.configure_algorithm("correlation_analysis", {
    type: "pearson",
    parameters: {
      min_correlation: 0.5,
      significance_level: 0.05
    }
  })
  
  // 生成分析数据
  let analysis_data = generate_analysis_telemetry_data(
    start_date="2023-01-01",
    end_date="2023-01-31",
    metrics=["response_time", "error_rate", "throughput", "cpu_usage", "memory_usage"],
    granularity="hourly"
  )
  
  // 执行异常检测分析
  let anomaly_analysis = data_analyzer.run_analysis(
    algorithm="anomaly_detection",
    data=analysis_data,
    target_metrics=["response_time", "error_rate"]
  )
  
  // 验证异常检测结果
  assert_true(anomaly_analysis.success)
  assert_true(anomaly_analysis.anomalies.length() > 0)
  
  for anomaly in anomaly_analysis.anomalies {
    assert_true(anomaly.timestamp > 0)
    assert_true(anomaly.metric.length() > 0)
    assert_true(anomaly.anomaly_score >= 0.0 && anomaly.anomaly_score <= 1.0)
    assert_true(anomaly.severity == "low" || anomaly.severity == "medium" || anomaly.severity == "high")
  }
  
  // 执行趋势分析
  let trend_analysis = data_analyzer.run_analysis(
    algorithm="trend_analysis",
    data=analysis_data,
    target_metrics=["response_time", "throughput"]
  )
  
  // 验证趋势分析结果
  assert_true(trend_analysis.success)
  assert_true(trend_analysis.trends.length() > 0)
  
  for trend in trend_analysis.trends {
    assert_true(trend.metric.length() > 0)
    assert_true(trend.direction == "increasing" || trend.direction == "decreasing" || trend.direction == "stable")
    assert_true(trend.strength >= 0.0 && trend.strength <= 1.0)
    assert_true(trend.seasonal_pattern == "present" || trend.seasonal_pattern == "absent")
  }
  
  // 执行相关性分析
  let correlation_analysis = data_analyzer.run_analysis(
    algorithm="correlation_analysis",
    data=analysis_data,
    target_metrics=["response_time", "error_rate", "throughput", "cpu_usage", "memory_usage"]
  )
  
  // 验证相关性分析结果
  assert_true(correlation_analysis.success)
  assert_true(correlation_analysis.correlations.length() > 0)
  
  for correlation in correlation_analysis.correlations {
    assert_true(correlation.metric_1.length() > 0)
    assert_true(correlation.metric_2.length() > 0)
    assert_true(correlation.coefficient >= -1.0 && correlation.coefficient <= 1.0)
    assert_true(correlation.significance >= 0.0 && correlation.significance <= 1.0)
  }
  
  // 生成洞察报告
  let insight_config = {
    title: "系统性能洞察报告",
    time_range: "2023年1月",
    include_sections: [
      "executive_summary",
      "key_findings",
      "anomaly_highlights",
      "trend_insights",
      "correlation_discoveries",
      "recommendations"
    ],
    visualization_level: "comprehensive",
    language: "zh-CN"
  }
  
  let insight_report = data_analyzer.generate_insights(
    analyses=[anomaly_analysis, trend_analysis, correlation_analysis],
    config=insight_config
  )
  
  // 验证洞察报告
  assert_true(insight_report.id.length() > 0)
  assert_eq(insight_report.title, "系统性能洞察报告")
  assert_true(insight_report.sections.length() > 0)
  
  // 验证执行摘要
  let executive_summary = insight_report.sections.find({ section => section.type == "executive_summary" })
  assert_true(executive_summary.type == "executive_summary")
  assert_true(executive_summary.content.length() > 0)
  assert_true(executive_summary.key_metrics.length() > 0)
  
  // 验证关键发现
  let key_findings = insight_report.sections.find({ section => section.type == "key_findings" })
  assert_true(key_findings.type == "key_findings")
  assert_true(key_findings.findings.length() > 0)
  
  for finding in key_findings.findings {
    assert_true(finding.title.length() > 0)
    assert_true(finding.description.length() > 0)
    assert_true(finding.impact == "low" || finding.impact == "medium" || finding.impact == "high")
    assert_true(finding.confidence >= 0.0 && finding.confidence <= 1.0)
  }
  
  // 验证建议
  let recommendations = insight_report.sections.find({ section => section.type == "recommendations" })
  assert_true(recommendations.type == "recommendations")
  assert_true(recommendations.recommendations.length() > 0)
  
  for recommendation in recommendations.recommendations {
    assert_true(recommendation.title.length() > 0)
    assert_true(recommendation.description.length() > 0)
    assert_true(recommendation.priority == "low" || recommendation.priority == "medium" || recommendation.priority == "high")
    assert_true(recommendation.effort == "low" || recommendation.effort == "medium" || recommendation.effort == "high")
    assert_true(recommendation.expected_impact.length() > 0)
  }
  
  // 测试预测分析
  let forecast_config = {
    algorithm: "prophet",
    horizon_days: 7,
    confidence_interval: 0.95,
    seasonality: {
      daily: true,
      weekly: true,
      yearly: false
    }
  }
  
  let forecast_analysis = data_analyzer.run_forecast(
    data=analysis_data,
    target_metric="response_time",
    config=forecast_config
  )
  
  // 验证预测分析结果
  assert_true(forecast_analysis.success)
  assert_true(forecast_analysis.forecast_values.length() > 0)
  
  for forecast in forecast_analysis.forecast_values {
    assert_true(forecast.timestamp > 0)
    assert_true(forecast.predicted_value >= 0.0)
    assert_true(forecast.lower_bound <= forecast.predicted_value)
    assert_true(forecast.upper_bound >= forecast.predicted_value)
  }
  
  // 测试根因分析
  let incident_data = generate_incident_telemetry_data(
    incident_start="2023-01-15T10:00:00Z",
    incident_end="2023-01-15T11:30:00Z",
    affected_services=["order-service", "payment-service"]
  )
  
  let root_cause_analysis = data_analyzer.run_root_cause_analysis(
    incident_data=incident_data,
    baseline_data=analysis_data,
    analysis_depth="deep"
  )
  
  // 验证根因分析结果
  assert_true(root_cause_analysis.success)
  assert_true(root_cause_analysis.potential_causes.length() > 0)
  
  for cause in root_cause_analysis.potential_causes {
    assert_true(cause.description.length() > 0)
    assert_true(cause.likelihood >= 0.0 && cause.likelihood <= 1.0)
    assert_true(cause.evidence.length() > 0)
  }
  
  // 停止数据分析器
  data_analyzer.stop()
}

// 测试5: 多维度数据探索
test "多维度数据探索" {
  // 创建数据探索器
  let data_explorer = DataExplorer::new()
  
  // 配置数据源
  data_explorer.configure_data_source("telemetry", {
    type: "time_series_database",
    connection: "influxdb://...",
    retention_policy: "autogen",
    default_time_range: "last_7d"
  })
  
  data_explorer.configure_data_source("logs", {
    type: "elasticsearch",
    connection: "https://elasticsearch.example.com",
    index_pattern: "telemetry-logs-*",
    default_time_range: "last_24h"
  })
  
  // 配置维度
  data_explorer.configure_dimension("service", {
    type: "categorical",
    values: ["api-gateway", "order-service", "payment-service", "notification-service"],
    default_aggregation: "group_by"
  })
  
  data_explorer.configure_dimension("environment", {
    type: "categorical",
    values: ["production", "staging", "development"],
    default_aggregation: "filter"
  })
  
  data_explorer.configure_dimension("response_time", {
    type: "numerical",
    range: { min: 0, max: 10000 },
    buckets: 20,
    default_aggregation: "histogram"
  })
  
  data_explorer.configure_dimension("timestamp", {
    type: "temporal",
    granularity: ["minute", "hour", "day", "week"],
    default_granularity: "hour"
  })
  
  // 创建探索会话
  let exploration_session = data_explorer.create_session(
    name="性能问题探索",
    data_sources=["telemetry", "logs"],
    time_range={ start: "2023-01-15T00:00:00Z", end: "2023-01-15T23:59:59Z" }
  )
  
  // 执行多维度查询
  let query1 = data_explorer.build_query()
    .select(["service", "avg(response_time)", "count(*)"])
    .from("telemetry")
    .filter({ environment: "production" })
    .group_by(["service"])
    .time_range({ start: "2023-01-15T10:00:00Z", end: "2023-01-15T12:00:00Z" })
    .order_by("avg(response_time)", "desc")
    .limit(10)
  
  let result1 = data_explorer.execute_query(exploration_session.id, query1)
  assert_true(result1.success)
  assert_true(result1.rows.length() > 0)
  
  // 验证查询结果
  for row in result1.rows {
    assert_true(row.contains_key("service"))
    assert_true(row.contains_key("avg(response_time)"))
    assert_true(row.contains_key("count(*)"))
  }
  
  // 执行交叉分析查询
  let query2 = data_explorer.build_query()
    .select(["service", "environment", "percentile(response_time, 95)", "error_rate"])
    .from("telemetry")
    .filter({ response_time: { gt: 1000 } })
    .group_by(["service", "environment"])
    .time_range({ start: "2023-01-15T00:00:00Z", end: "2023-01-15T23:59:59Z" })
    .order_by("percentile(response_time, 95)", "desc")
  
  let result2 = data_explorer.execute_query(exploration_session.id, query2)
  assert_true(result2.success)
  
  // 执行关联查询
  let query3 = data_explorer.build_query()
    .select(["telemetry.service", "telemetry.trace_id", "logs.level", "logs.message"])
    .from(["telemetry", "logs"])
    .filter({ 
      "telemetry.response_time": { gt: 2000 },
      "logs.level": "error"
    })
    .join({ 
      type: "inner",
      left: "telemetry.trace_id",
      right: "logs.trace_id"
    })
    .time_range({ start: "2023-01-15T00:00:00Z", end: "2023-01-15T23:59:59Z" })
    .limit(50)
  
  let result3 = data_explorer.execute_query(exploration_session.id, query3)
  assert_true(result3.success)
  
  // 执行下钻分析
  let drill_down_config = {
    initial_dimension: "service",
    drill_path: ["environment", "operation", "error_type"],
    metric: "avg(response_time)",
    threshold: 1000
  }
  
  let drill_down_result = data_explorer.execute_drill_down(
    session_id=exploration_session.id,
    config=drill_down_config
  )
  
  // 验证下钻结果
  assert_true(drill_down_result.success)
  assert_true(drill_down_result.levels.length() > 0)
  
  for level in drill_down_result.levels {
    assert_true(level.dimension.length() > 0)
    assert_true(level.values.length() > 0)
    assert_true(level.metric_values.length() == level.values.length())
  }
  
  // 执行切片分析
  let slice_analysis = data_explorer.execute_slice_analysis(
    session_id=exploration_session.id,
    dimension="environment",
    metric="error_rate",
    comparison_type="percentage_change",
    baseline_period="previous_week"
  )
  
  // 验证切片分析结果
  assert_true(slice_analysis.success)
  assert_true(slice_analysis.slices.length() > 0)
  
  for slice in slice_analysis.slices {
    assert_true(slice.dimension_value.length() > 0)
    assert_true(slice.current_value >= 0.0)
    assert_true(slice.baseline_value >= 0.0)
    assert_true(slice.percentage_change != null)
  }
  
  // 执行模式发现
  let pattern_discovery = data_explorer.discover_patterns(
    session_id=exploration_session.id,
    algorithms=["frequent_itemset", "sequential_pattern", "anomaly_pattern"],
    min_support=0.1,
    min_confidence=0.7
  )
  
  // 验证模式发现结果
  assert_true(pattern_discovery.success)
  assert_true(pattern_discovery.patterns.length() > 0)
  
  for pattern in pattern_discovery.patterns {
    assert_true(pattern.type == "frequent_itemset" || 
                pattern.type == "sequential_pattern" || 
                pattern.type == "anomaly_pattern")
    assert_true(pattern.support >= 0.0 && pattern.support <= 1.0)
    assert_true(pattern.confidence >= 0.0 && pattern.confidence <= 1.0)
    assert_true(pattern.items.length() > 0)
  }
  
  // 生成探索报告
  let exploration_report = data_explorer.generate_exploration_report(
    session_id=exploration_session.id,
    include_queries=true,
    include_visualizations=true,
    format="html"
  )
  
  // 验证探索报告
  assert_true(exploration_report.success)
  assert_true(exploration_report.content.length() > 0)
  assert_true(exploration_report.content.contains("<html"))
  assert_true(exploration_report.content.contains("性能问题探索"))
  
  // 测试交互式探索
  let interactive_config = {
    auto_suggestions: true,
    query_builder: true,
    result_visualization: true,
    collaborative_mode: false
  }
  
  let interactive_session = data_explorer.create_interactive_session(
    name="交互式数据探索",
    config=interactive_config
  )
  
  // 模拟用户交互
  let user_query1 = "显示各服务的平均响应时间"
  let natural_language_result1 = data_explorer.execute_natural_language_query(
    session_id=interactive_session.id,
    query=user_query1
  )
  
  assert_true(natural_language_result1.success)
  assert_true(natural_language_result1.generated_query.length() > 0)
  assert_true(natural_language_result1.results.rows.length() > 0)
  
  let user_query2 = "找出响应时间超过2秒的订单服务请求"
  let natural_language_result2 = data_explorer.execute_natural_language_query(
    session_id=interactive_session.id,
    query=user_query2
  )
  
  assert_true(natural_language_result2.success)
  
  // 测试查询建议
  let suggestions = data_explorer.get_query_suggestions(
    session_id=interactive_session.id,
    partial_query="显示错误率最高的"
  )
  
  assert_true(suggestions.length() > 0)
  
  for suggestion in suggestions {
    assert_true(suggestion.query.length() > 0)
    assert_true(suggestion.description.length() > 0)
    assert_true(suggestion.confidence >= 0.0 && suggestion.confidence <= 1.0)
  }
  
  // 停止数据探索器
  data_explorer.stop()
}