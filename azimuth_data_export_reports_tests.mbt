// Azimuth 数据导出和报告生成测试用例
// 专注于数据导出、报告生成、可视化和分析功能的测试

// 测试1: 基本数据导出功能
test "基本数据导出功能测试" {
  // 创建数据导出管理器
  let export_manager = DataExportManager::new({
    max_export_size: 100 * 1024 * 1024,  // 100MB
    supported_formats: ["json", "csv", "xml", "xlsx"],
    compression_enabled: true,
    encryption_enabled: false,
    temp_directory: "/tmp/azimuth_exports"
  })
  
  // 准备测试数据
  let test_data = [
    {
      "id": 1,
      "name": "张三",
      "email": "zhangsan@example.com",
      "department": "技术部",
      "salary": 15000,
      "join_date": "2022-01-15",
      "active": true
    },
    {
      "id": 2,
      "name": "李四",
      "email": "lisi@example.com",
      "department": "销售部",
      "salary": 12000,
      "join_date": "2021-06-20",
      "active": true
    },
    {
      "id": 3,
      "name": "王五",
      "email": "wangwu@example.com",
      "department": "市场部",
      "salary": 13500,
      "join_date": "2022-03-10",
      "active": false
    }
  ]
  
  // 测试JSON格式导出
  let json_export_result = export_manager.export_data(test_data, {
    format: "json",
    filename: "employees",
    include_headers: true,
    encoding: "utf-8"
  })
  
  assert_true(json_export_result.success)
  assert_true(json_export_result.file_path.length() > 0)
  assert_true(json_export_result.file_size > 0)
  
  // 验证JSON导出内容
  let exported_json_content = File::read(json_export_result.file_path)
  let parsed_json = JSON::parse(exported_json_content)
  assert_true(parsed_json.is_array())
  assert_eq(parsed_json.length(), 3)
  
  // 测试CSV格式导出
  let csv_export_result = export_manager.export_data(test_data, {
    format: "csv",
    filename: "employees",
    include_headers: true,
    encoding: "utf-8",
    delimiter: ","
  })
  
  assert_true(csv_export_result.success)
  
  // 验证CSV导出内容
  let exported_csv_content = File::read(csv_export_result.file_path)
  assert_true(exported_csv_content.contains("id,name,email"))
  assert_true(exported_csv_content.contains("张三"))
  assert_true(exported_csv_content.contains("李四"))
  
  // 测试XML格式导出
  let xml_export_result = export_manager.export_data(test_data, {
    format: "xml",
    filename: "employees",
    root_element: "employees",
    record_element: "employee",
    encoding: "utf-8"
  })
  
  assert_true(xml_export_result.success)
  
  // 验证XML导出内容
  let exported_xml_content = File::read(xml_export_result.file_path)
  assert_true(exported_xml_content.contains("<employees>"))
  assert_true(exported_xml_content.contains("<employee>"))
  assert_true(exported_xml_content.contains("<name>张三</name>"))
  
  // 测试压缩导出
  let compressed_export_result = export_manager.export_data(test_data, {
    format: "json",
    filename: "employees_compressed",
    compression: "gzip",
    encoding: "utf-8"
  })
  
  assert_true(compressed_export_result.success)
  assert_true(compressed_export_result.compressed)
  
  // 验证压缩文件大小小于未压缩文件
  assert_true(compressed_export_result.file_size < json_export_result.file_size)
  
  // 测试加密导出
  let encrypted_export_result = export_manager.export_data(test_data, {
    format: "json",
    filename: "employees_encrypted",
    encryption: {
      enabled: true,
      algorithm: "AES-256-GCM",
      password: "secure_password"
    },
    encoding: "utf-8"
  })
  
  assert_true(encrypted_export_result.success)
  assert_true(encrypted_export_result.encrypted)
  
  // 测试分页导出
  let large_dataset = generate_large_dataset(1000)  // 生成1000条记录
  let paginated_export_result = export_manager.export_data_paginated(large_dataset, {
    format: "csv",
    filename: "large_dataset",
    page_size: 100,
    include_headers: true,
    encoding: "utf-8"
  })
  
  assert_true(paginated_export_result.success)
  assert_eq(paginated_export_result.total_pages, 10)
  assert_eq(paginated_export_result.files.length(), 10)
  
  // 测试导出进度跟踪
  let progress_tracker = export_manager.create_export_tracker()
  let progress_export_result = export_manager.export_data_with_progress(large_dataset, {
    format: "json",
    filename: "large_dataset_progress",
    encoding: "utf-8"
  }, progress_tracker)
  
  assert_true(progress_export_result.success)
  
  // 检查进度
  let progress = progress_tracker.get_progress()
  assert_eq(progress.percentage, 100.0)
  assert_true(progress.completed)
  
  // 测试导出任务管理
  let task_id = export_manager.create_export_task({
    name: "员工数据导出",
    data_source: "database",
    query: "SELECT * FROM employees",
    format: "xlsx",
    schedule: {
      enabled: false
    },
    notification: {
      enabled: true,
      recipients: ["admin@example.com"]
    }
  })
  
  assert_true(task_id != None)
  
  // 执行导出任务
  let task_execution = export_manager.execute_export_task(task_id.unwrap())
  assert_true(task_execution.success)
  
  // 检查任务状态
  let task_status = export_manager.get_task_status(task_id.unwrap())
  assert_true(task_status.completed)
  assert_true(task_status.result.file_path.length() > 0)
}

// 测试2: 高级数据导出功能
test "高级数据导出功能测试" {
  // 创建高级数据导出管理器
  let advanced_export_manager = AdvancedDataExportManager::new({
    transformation_enabled: true,
    validation_enabled: true,
    filtering_enabled: true,
    aggregation_enabled: true,
    custom_formatting_enabled: true
  })
  
  // 准备复杂数据集
  let complex_dataset = [
    {
      "user_id": 1001,
      "username": "user001",
      "profile": {
        "first_name": "张",
        "last_name": "三",
        "age": 30,
        "address": {
          "street": "北京市朝阳区xxx街道",
          "city": "北京",
          "country": "中国",
          "postal_code": "100000"
        }
      },
      "activities": [
        { "type": "login", "timestamp": "2022-01-01T09:00:00Z", "ip": "192.168.1.100" },
        { "type": "purchase", "timestamp": "2022-01-01T10:30:00Z", "amount": 299.99 }
      ],
      "preferences": {
        "language": "zh-CN",
        "timezone": "Asia/Shanghai",
        "notifications": true
      },
      "metadata": {
        "created_at": "2021-12-01T00:00:00Z",
        "updated_at": "2022-01-01T10:30:00Z",
        "version": 1
      }
    },
    {
      "user_id": 1002,
      "username": "user002",
      "profile": {
        "first_name": "李",
        "last_name": "四",
        "age": 25,
        "address": {
          "street": "上海市浦东新区xxx路",
          "city": "上海",
          "country": "中国",
          "postal_code": "200000"
        }
      },
      "activities": [
        { "type": "login", "timestamp": "2022-01-02T08:30:00Z", "ip": "192.168.1.101" },
        { "type": "browse", "timestamp": "2022-01-02T09:15:00Z", "pages": ["home", "products", "about"] }
      ],
      "preferences": {
        "language": "zh-CN",
        "timezone": "Asia/Shanghai",
        "notifications": false
      },
      "metadata": {
        "created_at": "2021-11-15T00:00:00Z",
        "updated_at": "2022-01-02T09:15:00Z",
        "version": 2
      }
    }
  ]
  
  // 测试数据转换和扁平化
  let transform_result = advanced_export_manager.transform_data(complex_dataset, {
    operations: [
      {
        type: "flatten",
        source: "profile",
        separator: "_"
      },
      {
        type: "extract",
        source: "activities",
        field: "type",
        target: "last_activity_type"
      },
      {
        type: "calculate",
        expression: "profile.age * 12",
        target: "age_in_months"
      },
      {
        type: "format",
        source: "profile.address",
        template: "{{street}}, {{city}} {{postal_code}}",
        target: "full_address"
      }
    ]
  })
  
  assert_true(transform_result.success)
  
  let transformed_data = transform_result.data
  assert_eq(transformed_data.length(), 2)
  
  // 验证转换结果
  let first_record = transformed_data[0]
  assert_eq(first_record.get("profile_first_name"), Some("张"))
  assert_eq(first_record.get("profile_last_name"), Some("三"))
  assert_eq(first_record.get("profile_age"), Some(30))
  assert_eq(first_record.get("last_activity_type"), Some("purchase"))
  assert_eq(first_record.get("age_in_months"), Some(360))
  
  // 测试数据过滤
  let filter_result = advanced_export_manager.filter_data(complex_dataset, {
    conditions: [
      { field: "profile.age", operator: ">", value: 25 },
      { field: "preferences.notifications", operator: "==", value: true }
    ]
  })
  
  assert_true(filter_result.success)
  assert_eq(filter_result.data.length(), 1)  // 只有第一个用户满足条件
  
  // 测试数据聚合
  let aggregation_result = advanced_export_manager.aggregate_data(complex_dataset, {
    group_by: ["preferences.language"],
    aggregations: [
      { field: "user_id", function: "count", alias: "user_count" },
      { field: "profile.age", function: "avg", alias: "avg_age" },
      { field: "profile.age", function: "min", alias: "min_age" },
      { field: "profile.age", function: "max", alias: "max_age" }
    ]
  })
  
  assert_true(aggregation_result.success)
  assert_eq(aggregation_result.data.length(), 1)  // 只有一种语言
  
  let aggregated_data = aggregation_result.data[0]
  assert_eq(aggregated_data.get("preferences.language"), Some("zh-CN"))
  assert_eq(aggregated_data.get("user_count"), Some(2))
  
  // 测试自定义格式化
  let custom_format_result = advanced_export_manager.export_with_custom_format(complex_dataset, {
    format: "custom",
    template: {
      header: "用户数据报告",
      record_template: "用户ID: {user_id}, 姓名: {profile.first_name}{profile.last_name}, 年龄: {profile.age}岁",
      footer: "报告生成时间: {timestamp}",
      field_separator: "\n"
    },
    filename: "custom_user_report"
  })
  
  assert_true(custom_format_result.success)
  
  // 验证自定义格式内容
  let custom_content = File::read(custom_format_result.file_path)
  assert_true(custom_content.contains("用户数据报告"))
  assert_true(custom_content.contains("用户ID: 1001, 姓名: 张三, 年龄: 30岁"))
  
  // 测试条件导出
  let conditional_export_result = advanced_export_manager.conditional_export(complex_dataset, {
    format: "json",
    filename: "conditional_export",
    condition: "profile.age >= 30",
    include_fields: ["user_id", "username", "profile.first_name", "profile.age"]
  })
  
  assert_true(conditional_export_result.success)
  
  // 测试数据验证
  let validation_rules = [
    {
      field: "user_id",
      type: "required",
      message: "用户ID不能为空"
    },
    {
      field: "profile.age",
      type: "range",
      min: 18,
      max: 100,
      message: "年龄必须在18-100之间"
    },
    {
      field: "preferences.language",
      type: "enum",
      values: ["zh-CN", "en-US", "ja-JP"],
      message: "语言必须是支持的值"
    }
  ]
  
  let validation_result = advanced_export_manager.validate_and_export(complex_dataset, {
    format: "csv",
    filename: "validated_export",
    validation_rules: validation_rules,
    on_validation_error: "exclude"  // 排除无效记录
  })
  
  assert_true(validation_result.success)
  assert_true(validation_result.validation_summary.invalid_records == 0)  // 所有记录都有效
  
  // 测试数据脱敏
  let masking_result = advanced_export_manager.mask_data(complex_dataset, {
    masking_rules: [
      {
        field: "username",
        type: "partial",
        visible_chars: 2,
        mask_char: "*"
      },
      {
        field: "profile.address.street",
        type: "hash"
      },
      {
        field: "activities",
        type: "remove"
      }
    ],
    format: "json",
    filename: "masked_data"
  })
  
  assert_true(masking_result.success)
  
  // 验证脱敏结果
  let masked_content = File::read(masking_result.file_path)
  let masked_data = JSON::parse(masked_content)
  
  assert_eq(masked_data[0].username, "us******")  // 部分脱敏
  assert_eq(masked_data[0].activities, [])  // 完全移除
  
  // 测试增量导出
  let incremental_export_result = advanced_export_manager.incremental_export({
    data_source: "database",
    table: "users",
    format: "csv",
    filename: "incremental_export",
    last_export_time: "2022-01-01T00:00:00Z",
    timestamp_field: "metadata.updated_at"
  })
  
  assert_true(incremental_export_result.success)
  assert_true(incremental_export_result.incremental)
}

// 测试3: 报告生成功能
test "报告生成功能测试" {
  // 创建报告生成器
  let report_generator = ReportGenerator::new({
    templates_directory: "/templates",
    output_directory: "/reports",
    default_format: "pdf",
    default_orientation: "portrait",
    default_page_size: "A4"
  })
  
  // 注册报告模板
  let template_registration = report_generator.register_template({
    name: "员工月度报告",
    description: "员工月度绩效报告模板",
    type: "performance_report",
    sections: [
      {
        name: "概览",
        type: "summary",
        template: "本月共有{total_employees}名员工，其中{active_employees}名在职"
      },
      {
        name: "部门分布",
        type: "chart",
        chart_type: "pie",
        data_source: "department_distribution",
        template: "各部门人员分布情况"
      },
      {
        name: "绩效详情",
        type: "table",
        columns: ["员工姓名", "部门", "绩效评分", "评级"],
        data_source: "performance_details"
      }
    ]
  })
  
  assert_true(template_registration.success)
  
  // 准备报告数据
  let report_data = {
    "title": "2022年1月员工绩效报告",
    "period": "2022年1月",
    "generated_at": "2022-02-01T09:00:00Z",
    "total_employees": 150,
    "active_employees": 145,
    "department_distribution": [
      { "department": "技术部", "count": 45 },
      { "department": "销售部", "count": 35 },
      { "department": "市场部", "count": 25 },
      { "department": "人事部", "count": 15 },
      { "department": "财务部", "count": 30 }
    ],
    "performance_details": [
      { "name": "张三", "department": "技术部", "score": 95, "rating": "优秀" },
      { "name": "李四", "department": "销售部", "score": 88, "rating": "良好" },
      { "name": "王五", "department": "市场部", "score": 92, "rating": "优秀" }
    ],
    "summary": {
      "average_score": 87.5,
      "excellent_count": 45,
      "good_count": 78,
      "average_count": 22
    }
  }
  
  // 生成PDF报告
  let pdf_report_result = report_generator.generate_report({
    template_name: "员工月度报告",
    data: report_data,
    format: "pdf",
    filename: "monthly_performance_report_202201",
    options: {
      include_toc: true,
      include_page_numbers: true,
      watermark: "内部文件"
    }
  })
  
  assert_true(pdf_report_result.success)
  assert_true(pdf_report_result.file_path.length() > 0)
  assert_true(pdf_report_result.file_size > 0)
  
  // 生成HTML报告
  let html_report_result = report_generator.generate_report({
    template_name: "员工月度报告",
    data: report_data,
    format: "html",
    filename: "monthly_performance_report_202201",
    options: {
      include_css: true,
      include_interactive_charts: true,
      responsive: true
    }
  })
  
  assert_true(html_report_result.success)
  
  // 验证HTML报告内容
  let html_content = File::read(html_report_result.file_path)
  assert_true(html_content.contains("<html>"))
  assert_true(html_content.contains("2022年1月员工绩效报告"))
  assert_true(html_content.contains("张三"))
  
  // 生成Excel报告
  let excel_report_result = report_generator.generate_report({
    template_name: "员工月度报告",
    data: report_data,
    format: "xlsx",
    filename: "monthly_performance_report_202201",
    options: {
      include_charts: true,
      auto_filter: true,
      freeze_header: true
    }
  })
  
  assert_true(excel_report_result.success)
  
  // 测试自定义报告模板
  let custom_template = {
    name: "销售业绩报告",
    description: "销售团队业绩分析报告",
    type: "sales_report",
    sections: [
      {
        name: "销售概览",
        type: "kpi",
        kpis: [
          { label: "总销售额", value: "{total_sales}", format: "currency" },
          { label: "销售目标完成率", value: "{target_completion_rate}", format: "percentage" },
          { label: "新客户数", value: "{new_customers}", format: "number" }
        ]
      },
      {
        name: "销售趋势",
        type: "line_chart",
        data_source: "sales_trend",
        x_axis: "month",
        y_axis: "amount"
      },
      {
        name: "销售排名",
        type: "table",
        columns: ["销售员", "销售额", "完成率", "排名"],
        data_source: "sales_ranking"
      }
    ]
  }
  
  let custom_template_registration = report_generator.register_template(custom_template)
  assert_true(custom_template_registration.success)
  
  // 使用自定义模板生成报告
  let sales_data = {
    "title": "2022年第一季度销售业绩报告",
    "period": "2022年Q1",
    "total_sales": 5250000,
    "target_completion_rate": 105.2,
    "new_customers": 128,
    "sales_trend": [
      { "month": "1月", "amount": 1500000 },
      { "month": "2月", "amount": 1750000 },
      { "month": "3月", "amount": 2000000 }
    ],
    "sales_ranking": [
      { "name": "赵六", "amount": 650000, "completion_rate": 130.0, "rank": 1 },
      { "name": "钱七", "amount": 520000, "completion_rate": 104.0, "rank": 2 },
      { "name": "孙八", "amount": 480000, "completion_rate": 96.0, "rank": 3 }
    ]
  }
  
  let custom_report_result = report_generator.generate_report({
    template_name: "销售业绩报告",
    data: sales_data,
    format: "pdf",
    filename: "q1_sales_performance_2022"
  })
  
  assert_true(custom_report_result.success)
  
  // 测试报告调度
  let schedule_result = report_generator.schedule_report({
    name: "月度员工报告",
    template_name: "员工月度报告",
    data_source: "database",
    schedule: "0 9 1 * *",  // 每月1号上午9点
    format: "pdf",
    recipients: ["manager@example.com", "hr@example.com"],
    enabled: true
  })
  
  assert_true(schedule_result.success)
  assert_true(schedule_result.schedule_id != None)
  
  // 测试报告版本控制
  let versioned_report_result = report_generator.generate_versioned_report({
    template_name: "员工月度报告",
    data: report_data,
    format: "pdf",
    filename: "monthly_performance_report_202201",
    version: "v2.0",
    changelog: "增加了新的KPI指标"
  })
  
  assert_true(versioned_report_result.success)
  assert_eq(versioned_report_result.version, "v2.0")
  
  // 获取报告版本历史
  let version_history = report_generator.get_report_history("monthly_performance_report_202201")
  assert_true(version_history.length() >= 2)  // 至少有v1.0和v2.0
  
  // 测试报告模板变量
  let template_variables = report_generator.get_template_variables("员工月度报告")
  assert_true(template_variables.contains("title"))
  assert_true(template_variables.contains("period"))
  assert_true(template_variables.contains("total_employees"))
  
  // 测试报告预览
  let preview_result = report_generator.preview_report({
    template_name: "员工月度报告",
    data: report_data,
    format: "html",
    max_records: 1  // 只预览第一条记录
  })
  
  assert_true(preview_result.success)
  
  // 测试报告统计
  let report_stats = report_generator.get_statistics({
    period: "30d",
    group_by: ["template", "format"]
  })
  
  assert_true(report_stats.success)
  assert_true(report_stats.total_reports > 0)
}

// 测试4: 数据可视化功能
test "数据可视化功能测试" {
  // 创建数据可视化管理器
  let visualization_manager = DataVisualizationManager::new({
    default_chart_library: "chart.js",
    default_theme: "light",
    output_formats: ["png", "svg", "html"],
    max_data_points: 10000
  })
  
  // 准备可视化数据
  let chart_data = {
    "labels": ["1月", "2月", "3月", "4月", "5月", "6月"],
    "datasets": [
      {
        "label": "销售额",
        "data": [1200000, 1350000, 1500000, 1420000, 1650000, 1800000],
        "backgroundColor": "rgba(54, 162, 235, 0.2)",
        "borderColor": "rgba(54, 162, 235, 1)"
      },
      {
        "label": "成本",
        "data": [800000, 850000, 920000, 880000, 950000, 1020000],
        "backgroundColor": "rgba(255, 99, 132, 0.2)",
        "borderColor": "rgba(255, 99, 132, 1)"
      }
    ]
  }
  
  // 生成折线图
  let line_chart_result = visualization_manager.create_chart({
    type: "line",
    data: chart_data,
    options: {
      title: "2022年上半年销售趋势",
      responsive: true,
      plugins: {
        legend: { position: "top" },
        tooltip: { mode: "index", intersect: false }
      },
      scales: {
        y: {
          beginAtZero: true,
          ticks: {
            callback: "function(value) { return '¥' + value.toLocaleString(); }"
          }
        }
      }
    },
    output_format: "png",
    filename: "sales_trend_line_chart"
  })
  
  assert_true(line_chart_result.success)
  assert_true(line_chart_result.file_path.length() > 0)
  assert_true(line_chart_result.file_size > 0)
  
  // 生成柱状图
  let bar_chart_result = visualization_manager.create_chart({
    type: "bar",
    data: {
      "labels": ["技术部", "销售部", "市场部", "人事部", "财务部"],
      "datasets": [
        {
          "label": "员工数量",
          "data": [45, 35, 25, 15, 30],
          "backgroundColor": [
            "rgba(255, 99, 132, 0.2)",
            "rgba(54, 162, 235, 0.2)",
            "rgba(255, 206, 86, 0.2)",
            "rgba(75, 192, 192, 0.2)",
            "rgba(153, 102, 255, 0.2)"
          ]
        }
      ]
    },
    options: {
      title: "各部门员工数量分布",
      responsive: true,
      plugins: {
        legend: { display: false }
      }
    },
    output_format: "svg",
    filename: "department_employee_bar_chart"
  })
  
  assert_true(bar_chart_result.success)
  
  // 生成饼图
  let pie_chart_result = visualization_manager.create_chart({
    type: "pie",
    data: {
      "labels": ["产品A", "产品B", "产品C", "产品D", "产品E"],
      "datasets": [
        {
          "data": [30, 25, 20, 15, 10],
          "backgroundColor": [
            "#FF6384",
            "#36A2EB",
            "#FFCE56",
            "#4BC0C0",
            "#9966FF"
          ]
        }
      ]
    },
    options: {
      title: "产品销售占比",
      responsive: true,
      plugins: {
        tooltip: {
          callbacks: {
            label: "function(context) { return context.label + ': ' + context.parsed + '%'; }"
          }
        }
      }
    },
    output_format: "html",
    filename: "product_sales_pie_chart"
  })
  
  assert_true(pie_chart_result.success)
  
  // 验证HTML图表内容
  let html_chart_content = File::read(pie_chart_result.file_path)
  assert_true(html_chart_content.contains("<canvas"))
  assert_true(html_chart_content.contains("产品销售占比"))
  
  // 生成散点图
  let scatter_data = {
    "datasets": [
      {
        "label": "数据集A",
        "data": [
          { "x": 10, "y": 20 },
          { "x": 15, "y": 25 },
          { "x": 20, "y": 30 },
          { "x": 25, "y": 35 },
          { "x": 30, "y": 40 }
        ],
        "backgroundColor": "rgba(255, 99, 132, 0.5)"
      },
      {
        "label": "数据集B",
        "data": [
          { "x": 12, "y": 18 },
          { "x": 18, "y": 28 },
          { "x": 22, "y": 32 },
          { "x": 28, "y": 38 },
          { "x": 32, "y": 42 }
        ],
        "backgroundColor": "rgba(54, 162, 235, 0.5)"
      }
    ]
  }
  
  let scatter_chart_result = visualization_manager.create_chart({
    type: "scatter",
    data: scatter_data,
    options: {
      title: "数据相关性分析",
      responsive: true,
      scales: {
        x: { title: { display: true, text: "X轴" } },
        y: { title: { display: true, text: "Y轴" } }
      }
    },
    output_format: "png",
    filename: "correlation_scatter_chart"
  })
  
  assert_true(scatter_chart_result.success)
  
  // 生成热力图
  let heatmap_data = {
    "labels": ["周一", "周二", "周三", "周四", "周五"],
    "datasets": [
      {
        "label": "网站访问量",
        "data": [
          { "x": "周一", "y": "9:00", "v": 120 },
          { "x": "周一", "y": "12:00", "v": 250 },
          { "x": "周一", "y": "15:00", "v": 180 },
          { "x": "周一", "y": "18:00", "v": 300 },
          { "x": "周二", "y": "9:00", "v": 150 },
          { "x": "周二", "y": "12:00", "v": 280 },
          { "x": "周二", "y": "15:00", "v": 200 },
          { "x": "周二", "y": "18:00", "v": 320 }
        ],
        "backgroundColor": function(ctx) {
          const value = ctx.parsed.v;
          const alpha = value / 320;
          return `rgba(255, 0, 0, ${alpha})`;
        }
      }
    ]
  }
  
  let heatmap_result = visualization_manager.create_chart({
    type: "bubble",
    data: heatmap_data,
    options: {
      title: "一周网站访问热力图",
      responsive: true,
      plugins: {
        tooltip: {
          callbacks: {
            label: "function(context) { return context.raw.x + ' ' + context.raw.y + ': ' + context.raw.v + ' 访问'; }"
          }
        }
      }
    },
    output_format: "png",
    filename: "website_traffic_heatmap"
  })
  
  assert_true(heatmap_result.success)
  
  // 创建仪表板
  let dashboard_result = visualization_manager.create_dashboard({
    title: "业务数据仪表板",
    layout: "grid",
    panels: [
      {
        id: "sales_trend",
        type: "chart",
        chart_type: "line",
        title: "销售趋势",
        data: chart_data,
        position: { x: 0, y: 0, width: 6, height: 4 }
      },
      {
        id: "department_dist",
        type: "chart",
        chart_type: "bar",
        title: "部门分布",
        data: bar_chart_result.data,
        position: { x: 6, y: 0, width: 6, height: 4 }
      },
      {
        id: "product_share",
        type: "chart",
        chart_type: "pie",
        title: "产品占比",
        data: pie_chart_result.data,
        position: { x: 0, y: 4, width: 6, height: 4 }
      },
      {
        id: "kpi_summary",
        type: "kpi",
        title: "关键指标",
        kpis: [
          { label: "总销售额", value: "¥8,920,000", trend: "up" },
          { label: "总成本", value: "¥5,420,000", trend: "down" },
          { label: "净利润", value: "¥3,500,000", trend: "up" }
        ],
        position: { x: 6, y: 4, width: 6, height: 4 }
      }
    ],
    output_format: "html",
    filename: "business_dashboard",
    options: {
      responsive: true,
      auto_refresh: 300,  // 5分钟自动刷新
      theme: "light"
    }
  })
  
  assert_true(dashboard_result.success)
  
  // 验证仪表板内容
  let dashboard_content = File::read(dashboard_result.file_path)
  assert_true(dashboard_content.contains("业务数据仪表板"))
  assert_true(dashboard_content.contains("销售趋势"))
  assert_true(dashboard_content.contains("关键指标"))
  
  // 测试实时数据可视化
  let realtime_chart_result = visualization_manager.create_realtime_chart({
    type: "line",
    title: "实时系统监控",
    data_source: "system_metrics",
    metrics: ["cpu_usage", "memory_usage", "disk_io"],
    update_interval: 5000,  // 5秒更新
    max_points: 100,
    output_format: "html",
    filename: "realtime_system_monitoring"
  })
  
  assert_true(realtime_chart_result.success)
  
  // 测试图表交互功能
  let interactive_chart_result = visualization_manager.create_interactive_chart({
    type: "line",
    data: chart_data,
    interactions: {
      zoom: true,
      pan: true,
      crosshair: true,
      data_point_selection: true,
      tooltip_customization: true
    },
    output_format: "html",
    filename: "interactive_sales_chart"
  })
  
  assert_true(interactive_chart_result.success)
  
  // 测试图表主题
  let dark_theme_chart_result = visualization_manager.create_chart({
    type: "bar",
    data: bar_chart_result.data,
    theme: "dark",
    output_format: "png",
    filename: "dark_theme_department_chart"
  })
  
  assert_true(dark_theme_chart_result.success)
  
  // 测试图表导出选项
  let export_options_result = visualization_manager.export_chart_with_options(line_chart_result.chart_id, {
    formats: ["png", "svg", "pdf"],
    width: 1200,
    height: 800,
    quality: 90,
    background: "white"
  })
  
  assert_true(export_options_result.success)
  assert_eq(export_options_result.exported_files.length(), 3)
}

// 测试5: 自动化报告和调度
test "自动化报告和调度测试" {
  // 创建自动化报告管理器
  let automation_manager = ReportAutomationManager::new({
    scheduler: "cron",
    default_timezone: "Asia/Shanghai",
    retry_attempts: 3,
    retry_delay: 300,  // 5分钟
    notification_channels: ["email", "webhook"]
  })
  
  // 创建自动化报告配置
  let automated_report_config = {
    name: "每日销售报告",
    description: "自动生成每日销售业绩报告",
    template_name: "销售业绩报告",
    data_source: {
      type: "database",
      connection: "mysql://localhost:3306/sales",
      query: "SELECT * FROM daily_sales_summary WHERE date = CURDATE()"
    },
    schedule: "0 8 * * *",  // 每天上午8点
    output: {
      formats: ["pdf", "xlsx"],
      filename_pattern: "daily_sales_{YYYY-MM-DD}",
      storage: {
        type: "s3",
        bucket: "reports-bucket",
        path: "daily-sales/"
      }
    },
    distribution: {
      email: {
        enabled: true,
        recipients: ["sales-team@example.com", "manager@example.com"],
        subject: "每日销售报告 - {date}",
        template: "daily_report_email"
      },
      webhook: {
        enabled: true,
        url: "https://api.example.com/webhooks/reports",
        headers: { "Authorization": "Bearer {token}" }
      }
    },
    conditions: {
      only_if_data_exists: true,
      min_data_points: 1
    },
    error_handling: {
      notify_on_failure: true,
      retry_automatically: true,
      fallback_report: true
    }
  }
  
  // 注册自动化报告
  let registration_result = automation_manager.register_automated_report(automated_report_config)
  assert_true(registration_result.success)
  assert_true(registration_result.report_id != None)
  
  let report_id = registration_result.report_id.unwrap()
  
  // 测试手动执行自动化报告
  let manual_execution = automation_manager.execute_report(report_id, {
    dry_run: false,
    override_date: "2022-01-15"
  })
  
  assert_true(manual_execution.success)
  assert_true(manual_execution.generated_files.length() > 0)
  
  // 测试报告调度状态
  let schedule_status = automation_manager.get_schedule_status(report_id)
  assert_true(schedule_status.enabled)
  assert_eq(schedule_status.next_run, "2022-01-16T08:00:00+08:00")  // 假设当前是2022-01-15
  
  // 测试报告执行历史
  let execution_history = automation_manager.get_execution_history(report_id, {
    limit: 10,
    include_failures: true
  })
  
  assert_true(execution_history.length() >= 1)
  
  let latest_execution = execution_history[0]
  assert_eq(latest_execution.report_id, report_id)
  assert_true(latest_execution.success)
  
  // 测试报告依赖关系
  let dependency_config = {
    name: "月度汇总报告",
    description: "基于每日报告生成月度汇总",
    template_name: "月度汇总报告",
    dependencies: [report_id],  // 依赖每日销售报告
    schedule: "0 9 1 * *",  // 每月1号上午9点
    data_source: {
      type: "aggregated",
      sources: [
        {
          report_id: report_id,
          date_range: "last_month",
          aggregation: "sum"
        }
      ]
    }
  }
  
  let dependency_registration = automation_manager.register_automated_report(dependency_config)
  assert_true(dependency_registration.success)
  
  let monthly_report_id = dependency_registration.report_id.unwrap()
  
  // 测试依赖执行
  let dependency_execution = automation_manager.execute_report(monthly_report_id, {
    dry_run: true  // 只验证依赖，不实际执行
  })
  
  assert_true(dependency_execution.success)
  assert_true(dependency_execution.dependencies_met)
  
  // 测试报告条件执行
  let conditional_config = {
    name: "异常报告",
    description: "仅在检测到异常时生成报告",
    template_name: "异常检测报告",
    schedule: "0 */6 * * *",  // 每6小时执行一次
    conditions: {
      custom_check: "check_anomalies()",  // 自定义检查函数
      data_threshold: 100,
      time_range: "6h"
    }
  }
  
  let conditional_registration = automation_manager.register_automated_report(conditional_config)
  assert_true(conditional_registration.success)
  
  let conditional_report_id = conditional_registration.report_id.unwrap()
  
  // 模拟条件检查（假设没有异常）
  let conditional_execution = automation_manager.execute_report(conditional_report_id)
  assert_true(conditional_execution.success)
  assert_true(conditional_execution.skipped)  // 应该被跳过
  
  // 测试报告通知
  let notification_config = {
    name: "报告通知配置",
    channels: [
      {
        type: "email",
        enabled: true,
        settings: {
          smtp_server: "smtp.example.com",
          from: "reports@example.com",
          templates: {
            success: "report_success_template",
            failure: "report_failure_template"
          }
        }
      },
      {
        type: "slack",
        enabled: true,
        settings: {
          webhook_url: "https://hooks.slack.com/services/...",
          channel: "#reports",
          username: "ReportBot"
        }
      },
      {
        type: "webhook",
        enabled: true,
        settings: {
          url: "https://api.example.com/webhooks/reports",
          headers: { "Authorization": "Bearer {token}" },
          retry_attempts: 3
        }
      }
    ]
  }
  
  let notification_setup = automation_manager.configure_notifications(notification_config)
  assert_true(notification_setup.success)
  
  // 测试报告模板变量
  let template_variables = automation_manager.get_template_variables(report_id)
  assert_true(template_variables.contains("date"))
  assert_true(template_variables.contains("report_id"))
  assert_true(template_variables.contains("execution_time"))
  
  // 测试报告版本控制
  let versioning_result = automation_manager.enable_versioning(report_id, {
    max_versions: 12,
    retention_days: 90,
    auto_cleanup: true
  })
  
  assert_true(versioning_result.success)
  
  // 测试报告性能监控
  let performance_metrics = automation_manager.get_performance_metrics(report_id, {
    period: "30d",
    include_execution_time: true,
    include_data_processing_time: true,
    include_rendering_time: true
  })
  
  assert_true(performance_metrics.success)
  assert_true(performance_metrics.avg_execution_time > 0)
  
  // 测试报告备份
  let backup_result = automation_manager.create_backup({
    report_ids: [report_id, monthly_report_id],
    backup_location: "s3://backup-bucket/reports/",
    include_dependencies: true,
    compression: true
  })
  
  assert_true(backup_result.success)
  assert_true(backup_result.backup_id != None)
  
  // 测试报告恢复
  let restore_result = automation_manager.restore_from_backup(backup_result.backup_id.unwrap(), {
    target_location: "/restored_reports/",
    overwrite_existing: false
  })
  
  assert_true(restore_result.success)
  
  // 测试报告API
  let api_server = ReportAPIServer::new({
    host: "localhost",
    port: 8080,
    authentication: "jwt",
    rate_limiting: {
      enabled: true,
      requests_per_minute: 100
    }
  })
  
  // 启动API服务器
  let server_start = api_server.start()
  assert_true(server_start.success)
  
  // 测试API端点
  let api_client = APIClient::new({
    base_url: "http://localhost:8080/api/v1",
    auth_token: "test_token"
  })
  
  // 获取报告列表
  let reports_list = api_client.get("reports")
  assert_true(reports_list.success)
  
  // 获取特定报告
  let report_detail = api_client.get("reports/" + report_id)
  assert_true(report_detail.success)
  
  // 执行报告
  let execute_api = api_client.post("reports/" + report_id + "/execute", {})
  assert_true(execute_api.success)
  
  // 停止API服务器
  let server_stop = api_server.stop()
  assert_true(server_stop.success)
}

// 测试6: 数据分析和洞察
test "数据分析和洞察测试" {
  // 创建数据分析管理器
  let analytics_manager = DataAnalyticsManager::new({
    analysis_engines: ["statistical", "ml", "time_series"],
    default_confidence_level: 0.95,
    max_data_points: 100000,
    caching_enabled: true
  })
  
  // 准备分析数据
  let sales_data = [
    { "month": "2022-01", "sales": 1200000, "costs": 800000, "region": "华北" },
    { "month": "2022-01", "sales": 800000, "costs": 600000, "region": "华南" },
    { "month": "2022-01", "sales": 1000000, "costs": 750000, "region": "华东" },
    { "month": "2022-02", "sales": 1350000, "costs": 850000, "region": "华北" },
    { "month": "2022-02", "sales": 900000, "costs": 650000, "region": "华南" },
    { "month": "2022-02", "sales": 1100000, "costs": 800000, "region": "华东" },
    { "month": "2022-03", "sales": 1500000, "costs": 900000, "region": "华北" },
    { "month": "2022-03", "sales": 950000, "costs": 700000, "region": "华南" },
    { "month": "2022-03", "sales": 1200000, "costs": 850000, "region": "华东" }
  ]
  
  // 测试描述性统计分析
  let descriptive_analysis = analytics_manager.descriptive_analysis(sales_data, {
    metrics: ["sales", "costs"],
    group_by: ["region"],
    calculations: ["mean", "median", "std", "min", "max"]
  })
  
  assert_true(descriptive_analysis.success)
  
  let analysis_results = descriptive_analysis.results
  assert_true(analysis_results.contains_key("sales"))
  assert_true(analysis_results.contains_key("costs"))
  
  let sales_stats = analysis_results.get("sales")
  assert_true(sales_stats.contains_key("mean"))
  assert_true(sales_stats.contains_key("std"))
  
  // 测试相关性分析
  let correlation_analysis = analytics_manager.correlation_analysis(sales_data, {
    variables: ["sales", "costs"],
    method: "pearson",
    significance_level: 0.05
  })
  
  assert_true(correlation_analysis.success)
  
  let correlation_results = correlation_analysis.results
  assert_true(correlation_results.correlation_coefficient > 0.8)  // 销售和成本应该高度相关
  assert_true(correlation_results.p_value < 0.05)  // 应该显著相关
  
  // 测试趋势分析
  let trend_analysis = analytics_manager.trend_analysis(sales_data, {
    time_column: "month",
    value_column: "sales",
    group_by: ["region"],
    method: "linear_regression",
    forecast_periods: 3
  })
  
  assert_true(trend_analysis.success)
  
  let trend_results = trend_analysis.results
  assert_true(trend_results.forecasts.length() > 0)
  assert_true(trend_results.trend_direction in ["increasing", "decreasing", "stable"])
  
  // 测试异常检测
  let anomaly_detection = analytics_manager.anomaly_detection(sales_data, {
    algorithm: "isolation_forest",
    contamination: 0.1,
    features: ["sales", "costs"],
    sensitivity: "medium"
  })
  
  assert_true(anomaly_detection.success)
  
  let anomaly_results = anomaly_detection.results
  assert_true(anomaly_results.anomalies.length() >= 0)
  
  // 测试聚类分析
  let clustering_analysis = analytics_manager.clustering_analysis(sales_data, {
    algorithm: "kmeans",
    features: ["sales", "costs"],
    n_clusters: 3,
    standardization: true
  })
  
  assert_true(clustering_analysis.success)
  
  let clustering_results = clustering_analysis.results
  assert_eq(clustering_results.cluster_assignments.length(), sales_data.length())
  assert_eq(clustering_results.cluster_centers.length(), 3)
  
  // 测试分类分析
  let classification_data = [
    { "experience": 5, "education": "Bachelor", "salary": 8000, "department": "Tech" },
    { "experience": 3, "education": "Master", "salary": 7500, "department": "Tech" },
    { "experience": 7, "education": "Bachelor", "salary": 9000, "department": "Tech" },
    { "experience": 4, "education": "PhD", "salary": 8500, "department": "Research" },
    { "experience": 6, "education": "Master", "salary": 8200, "department": "Research" }
  ]
  
  let classification_analysis = analytics_manager.classification_analysis(classification_data, {
    target_column: "department",
    features: ["experience", "education", "salary"],
    algorithm: "decision_tree",
    test_size: 0.3,
    cross_validation: 5
  })
  
  assert_true(classification_analysis.success)
  
  let classification_results = classification_analysis.results
  assert_true(classification_results.accuracy > 0.5)  // 至少比随机猜测好
  assert_true(classification_results.feature_importance.length() > 0)
  
  // 测试时间序列分析
  let time_series_data = [
    { "date": "2022-01-01", "value": 100 },
    { "date": "2022-01-02", "value": 105 },
    { "date": "2022-01-03", "value": 98 },
    { "date": "2022-01-04", "value": 110 },
    { "date": "2022-01-05", "value": 115 },
    { "date": "2022-01-06", "value": 108 },
    { "date": "2022-01-07", "value": 120 }
  ]
  
  let time_series_analysis = analytics_manager.time_series_analysis(time_series_data, {
    date_column: "date",
    value_column: "value",
    methods: ["seasonality", "trend", "forecast"],
    forecast_periods: 7,
    confidence_interval: 0.95
  })
  
  assert_true(time_series_analysis.success)
  
  let ts_results = time_series_analysis.results
  assert_true(ts_results.seasonality_detected != None)
  assert_true(ts_results.forecasts.length() == 7)
  
  // 测试AB测试分析
  let ab_test_data = [
    { "user_id": 1, "group": "A", "conversion": 1, "revenue": 100 },
    { "user_id": 2, "group": "A", "conversion": 0, "revenue": 0 },
    { "user_id": 3, "group": "B", "conversion": 1, "revenue": 120 },
    { "user_id": 4, "group": "B", "conversion": 1, "revenue": 110 },
    { "user_id": 5, "group": "A", "conversion": 0, "revenue": 0 },
    { "user_id": 6, "group": "B", "conversion": 1, "revenue": 130 }
  ]
  
  let ab_test_analysis = analytics_manager.ab_test_analysis(ab_test_data, {
    group_column: "group",
    metrics: ["conversion", "revenue"],
    significance_level: 0.05,
    test_type: "two_tailed"
  })
  
  assert_true(ab_test_analysis.success)
  
  let ab_results = ab_test_analysis.results
  assert_true(ab_results.conversion_test.statistic != None)
  assert_true(ab_results.revenue_test.statistic != None)
  
  // 测试数据质量评估
  let data_quality_assessment = analytics_manager.assess_data_quality(sales_data, {
    checks: [
      "completeness",
      "uniqueness",
      "consistency",
      "validity",
      "accuracy"
    ],
    thresholds: {
      completeness: 0.95,
      uniqueness: 0.90,
      consistency: 0.95,
      validity: 0.95,
      accuracy: 0.90
    }
  })
  
  assert_true(data_quality_assessment.success)
  
  let quality_results = data_quality_assessment.results
  assert_true(quality_results.overall_score >= 0.0 && quality_results.overall_score <= 100.0)
  assert_true(quality_results.issues.length() >= 0)
  
  // 测试洞察生成
  let insight_generation = analytics_manager.generate_insights(sales_data, {
    insight_types: ["trends", "outliers", "correlations", "patterns"],
    confidence_threshold: 0.8,
    max_insights: 10
  })
  
  assert_true(insight_generation.success)
  
  let insights = insight_generation.insights
  assert_true(insights.length() > 0)
  
  for insight in insights {
    assert_true(insight.type in ["trend", "outlier", "correlation", "pattern"])
    assert_true(insight.confidence >= 0.0 && insight.confidence <= 1.0)
    assert_true(insight.description.length() > 0)
  }
  
  // 测试预测模型
  let prediction_model = analytics_manager.build_prediction_model(sales_data, {
    target_column: "sales",
    features: ["costs", "region"],
    algorithm: "random_forest",
    test_size: 0.2,
    hyperparameter_tuning: true
  })
  
  assert_true(prediction_model.success)
  
  let model_results = prediction_model.results
  assert_true(model_results.model_performance.r_squared > 0.0)
  assert_true(model_results.feature_importance.length() > 0)
  
  // 测试模型评估
  let model_evaluation = analytics_manager.evaluate_model(prediction_model.model_id, {
    metrics: ["accuracy", "precision", "recall", "f1_score"],
    cross_validation: true,
    cv_folds: 5
  })
  
  assert_true(model_evaluation.success)
  
  let evaluation_results = model_evaluation.results
  assert_true(evaluation_results.metrics.contains_key("accuracy"))
  
  // 测试分析报告生成
  let analysis_report = analytics_manager.generate_analysis_report({
    title: "销售数据分析报告",
    analyses: [
      descriptive_analysis.analysis_id,
      correlation_analysis.analysis_id,
      trend_analysis.analysis_id
    ],
    include_visualizations: true,
    format: "pdf",
    filename: "sales_data_analysis_report"
  })
  
  assert_true(analysis_report.success)
  assert_true(analysis_report.file_path.length() > 0)
}

// 测试7: 数据转换和处理
test "数据转换和处理测试" {
  // 创建数据转换管理器
  let transform_manager = DataTransformManager::new({
    max_batch_size: 10000,
    parallel_processing: true,
    error_handling: "skip_invalid",
    validation_enabled: true
  })
  
  // 定义转换规则
  let transformation_rules = [
    {
      name: "数据清洗",
      type: "cleaning",
      operations: [
        { type: "remove_duplicates", keys: ["id"] },
        { type: "fill_missing", strategy: "forward_fill", columns: ["category"] },
        { type: "remove_outliers", method: "iqr", columns: ["amount"] }
      ]
    },
    {
      name: "数据标准化",
      type: "normalization",
      operations: [
        { type: "standardize", columns: ["amount"] },
        { type: "min_max_scale", columns: ["quantity"] }
      ]
    },
    {
      name: "数据聚合",
      type: "aggregation",
      operations: [
        { type: "group_by", columns: ["category", "region"] },
        { type: "aggregate", measures: [{ column: "amount", function: "sum" }] }
      ]
    }
  ]
  
  // 准备原始数据
  let raw_data = [
    { "id": 1, "name": "产品A", "category": "电子", "amount": 1000, "quantity": 10, "region": "华北" },
    { "id": 2, "name": "产品B", "category": "家居", "amount": 500, "quantity": 5, "region": "华东" },
    { "id": 3, "name": "产品A", "category": "电子", "amount": 1200, "quantity": 12, "region": "华北" },  // 重复ID
    { "id": 4, "name": "产品C", "category": "", "amount": 800, "quantity": 8, "region": "华南" },  // 空类别
    { "id": 5, "name": "产品D", "category": "服装", "amount": 50000, "quantity": 50, "region": "华西" }  // 异常值
  ]
  
  // 执行数据转换
  let transformation_result = transform_manager.transform_data(raw_data, transformation_rules)
  assert_true(transformation_result.success)
  
  let transformed_data = transformation_result.data
  assert_eq(transformed_data.length(), 4)  // 移除重复后应该有4条记录
  
  // 验证转换结果
  let first_record = transformed_data[0]
  assert_eq(first_record.get("id"), Some(1))
  assert_eq(first_record.get("category"), Some("电子"))
  assert_true(first_record.get("amount") != Some(1000))  // 应该被标准化
  
  // 测试自定义转换函数
  let custom_transform = {
    name: "自定义转换",
    type: "custom",
    operations: [
      {
        type: "function",
        function: fn(record) {
          // 添加计算字段
          let amount = record.get("amount").unwrap_or(0)
          let quantity = record.get("quantity").unwrap_or(1)
          let unit_price = amount.to_float() / quantity.to_float()
          record.set("unit_price", unit_price)
          record
        }
      }
    ]
  }
  
  let custom_transform_result = transform_manager.transform_data(transformed_data, [custom_transform])
  assert_true(custom_transform_result.success)
  
  let custom_transformed_data = custom_transform_result.data
  let first_custom_record = custom_transformed_data[0]
  assert_true(first_custom_record.contains_key("unit_price"))
  
  // 测试数据验证
  let validation_rules = [
    {
      field: "amount",
      type: "range",
      min: 0,
      max: 10000,
      message: "金额必须在0-10000之间"
    },
    {
      field: "quantity",
      type: "positive_integer",
      message: "数量必须是正整数"
    },
    {
      field: "category",
      type: "non_empty",
      message: "类别不能为空"
    }
  ]
  
  let validation_result = transform_manager.validate_data(custom_transformed_data, validation_rules)
  assert_true(validation_result.success)
  
  let validation_summary = validation_result.summary
  assert_true(validation_summary.valid_records > 0)
  assert_true(validation_summary.invalid_records >= 0)
  
  // 测试数据合并
  let dataset1 = [
    { "id": 1, "name": "产品A", "price": 100 },
    { "id": 2, "name": "产品B", "price": 200 }
  ]
  
  let dataset2 = [
    { "id": 1, "stock": 50, "category": "电子" },
    { "id": 2, "stock": 30, "category": "家居" },
    { "id": 3, "stock": 20, "category": "服装" }
  ]
  
  let merge_result = transform_manager.merge_datasets([
    { data: dataset1, key: "id" },
    { data: dataset2, key: "id" }
  ], {
    merge_strategy: "inner",
    conflict_resolution: "keep_first"
  })
  
  assert_true(merge_result.success)
  
  let merged_data = merge_result.data
  assert_eq(merged_data.length(), 2)  // 内连接应该有2条记录
  
  let first_merged = merged_data[0]
  assert_eq(first_merged.get("id"), Some(1))
  assert_eq(first_merged.get("name"), Some("产品A"))
  assert_eq(first_merged.get("price"), Some(100))
  assert_eq(first_merged.get("stock"), Some(50))
  assert_eq(first_merged.get("category"), Some("电子"))
  
  // 测试数据透视
  let pivot_result = transform_manager.create_pivot_table(custom_transformed_data, {
    index: ["region"],
    columns: ["category"],
    values: ["amount"],
    agg_functions: ["sum", "mean"]
  })
  
  assert_true(pivot_result.success)
  
  let pivot_table = pivot_result.pivot_table
  assert_true(pivot_table.rows.length() > 0)
  assert_true(pivot_table.columns.length() > 0)
  
  // 测试数据透视逆操作
  let unpivot_result = transform_manager.unpivot_table(pivot_table, {
    index_columns: ["region"],
    value_columns: ["amount_sum", "amount_mean"],
    variable_name: "metric",
    value_name: "value"
  })
  
  assert_true(unpivot_result.success)
  
  // 测试数据抽样
  let sampling_result = transform_manager.sample_data(custom_transformed_data, {
    method: "stratified",
    strata_column: "category",
    sample_size: 2,
    random_seed: 42
  })
  
  assert_true(sampling_result.success)
  assert_eq(sampling_result.sampled_data.length(), 2)
  
  // 测试数据分区
  let partition_result = transform_manager.partition_data(custom_transformed_data, {
    partition_by: "region",
    output_format: "separate_files",
    output_directory: "/tmp/partitioned_data"
  })
  
  assert_true(partition_result.success)
  assert_eq(partition_result.partitions.length(), 4)  // 4个不同的区域
  
  // 测试数据管道
  let pipeline_config = {
    name: "销售数据处理管道",
    steps: [
      {
        name: "数据加载",
        type: "load",
        source: "database",
        query: "SELECT * FROM sales_raw"
      },
      {
        name: "数据清洗",
        type: "transform",
        transformations: transformation_rules
      },
      {
        name: "数据验证",
        type: "validate",
        rules: validation_rules
      },
      {
        name: "数据保存",
        type: "save",
        destination: "database",
        table: "sales_processed"
      }
    ]
  }
  
  let pipeline_execution = transform_manager.execute_pipeline(pipeline_config, {
    dry_run: false,
    error_handling: "stop_on_error"
  })
  
  assert_true(pipeline_execution.success)
  
  let pipeline_results = pipeline_execution.results
  assert_true(pipeline_results.steps_executed > 0)
  assert_true(pipeline_results.records_processed > 0)
  
  // 测试数据血缘追踪
  let lineage_result = transform_manager.get_data_lineage("sales_processed.amount")
  assert_true(lineage_result.success)
  
  let lineage = lineage_result.lineage
  assert_true(lineage.source_tables.contains("sales_raw"))
  assert_true(lineage.transformations.length() > 0)
  
  // 测试转换性能监控
  let performance_metrics = transform_manager.get_performance_metrics({
    time_range: "1h",
    include_memory_usage: true,
    include_processing_time: true
  })
  
  assert_true(performance_metrics.success)
  assert_true(performance_metrics.avg_processing_time > 0)
  assert_true(performance_metrics.memory_usage > 0)
}

// 测试8: 多格式支持和兼容性
test "多格式支持和兼容性测试" {
  // 创建多格式支持管理器
  let format_manager = MultiFormatSupportManager::new({
    supported_formats: ["json", "xml", "csv", "xlsx", "parquet", "avro", "yaml"],
    default_encoding: "utf-8",
    compression_formats: ["gzip", "zip", "bz2"],
    conversion_cache_size: 1000
  })
  
  // 准备测试数据
  let test_data = [
    {
      "id": 1,
      "name": "测试产品1",
      "category": "电子产品",
      "price": 199.99,
      "available": true,
      "tags": ["热销", "新品"],
      "metadata": {
        "created_at": "2022-01-01T00:00:00Z",
        "updated_at": "2022-01-15T12:30:00Z"
      }
    },
    {
      "id": 2,
      "name": "测试产品2",
      "category": "家居用品",
      "price": 89.99,
      "available": false,
      "tags": ["促销"],
      "metadata": {
        "created_at": "2022-01-02T00:00:00Z",
        "updated_at": "2022-01-10T09:15:00Z"
      }
    }
  ]
  
  // 测试JSON到XML转换
  let json_to_xml_result = format_manager.convert_format(test_data, {
    from: "json",
    to: "xml",
    options: {
      root_element: "products",
      record_element: "product",
      attribute_fields: ["id"],
      pretty_print: true
    }
  })
  
  assert_true(json_to_xml_result.success)
  
  let xml_content = File::read(json_to_xml_result.file_path)
  assert_true(xml_content.contains("<products>"))
  assert_true(xml_content.contains("<product id=\"1\">"))
  assert_true(xml_content.contains("<name>测试产品1</name>"))
  
  // 测试JSON到CSV转换
  let json_to_csv_result = format_manager.convert_format(test_data, {
    from: "json",
    to: "csv",
    options: {
      flatten_nested: true,
      separator: ",",
      include_headers: true,
      array_separator: "|"
    }
  })
  
  assert_true(json_to_csv_result.success)
  
  let csv_content = File::read(json_to_csv_result.file_path)
  assert_true(csv_content.contains("id,name,category"))
  assert_true(csv_content.contains("测试产品1"))
  assert_true(csv_content.contains("热销|新品"))
  
  // 测试JSON到YAML转换
  let json_to_yaml_result = format_manager.convert_format(test_data, {
    from: "json",
    to: "yaml",
    options: {
      indent: 2,
      flow_style: "block"
    }
  })
  
  assert_true(json_to_yaml_result.success)
  
  let yaml_content = File::read(json_to_yaml_result.file_path)
  assert_true(yaml_content.contains("id: 1"))
  assert_true(yaml_content.contains("name: 测试产品1"))
  
  // 测试CSV到JSON转换
  let csv_to_json_result = format_manager.convert_format(json_to_csv_result.file_path, {
    from: "csv",
    to: "json",
    options: {
      header_row: true,
      data_types: {
        "id": "integer",
        "price": "float",
        "available": "boolean"
      }
    }
  })
  
  assert_true(csv_to_json_result.success)
  
  let converted_json_content = File::read(csv_to_json_result.file_path)
  let parsed_converted_json = JSON::parse(converted_json_content)
  assert_true(parsed_converted_json.is_array())
  assert_eq(parsed_converted_json.length(), 2)
  
  // 测试格式验证
  let json_validation = format_manager.validate_format(json_to_xml_result.file_path, "xml")
  assert_true(json_validation.valid)
  
  let invalid_json = "{\"name\": \"test\", invalid_json}"
  let invalid_validation = format_manager.validate_content(invalid_json, "json")
  assert_false(invalid_validation.valid)
  assert_true(invalid_validation.errors.length() > 0)
  
  // 测试格式检测
  let format_detection = format_manager.detect_format(json_to_xml_result.file_path)
  assert_eq(format_detection.detected_format, Some("xml"))
  
  // 测试批量格式转换
  let batch_conversion_result = format_manager.batch_convert_format([
    { file: json_to_xml_result.file_path, from: "json", to: "xml" },
    { file: json_to_csv_result.file_path, from: "json", to: "yaml" },
    { file: json_to_yaml_result.file_path, from: "json", to: "csv" }
  ])
  
  assert_true(batch_conversion_result.success)
  assert_eq(batch_conversion_result.converted_files.length(), 3)
  
  // 测试格式兼容性检查
  let compatibility_check = format_manager.check_compatibility({
    source_format: "json",
    target_format: "csv",
    data_structure: {
      nested_objects: true,
      arrays: true,
      mixed_types: true
    }
  })
  
  assert_true(compatibility_check.compatible)
  assert_true(compatibility_check.warnings.length() >= 0)
  
  // 测试格式特定选项
  let xml_options_result = format_manager.convert_with_format_options(test_data, {
    from: "json",
    to: "xml",
    format_options: {
      "xml": {
        "declaration": true,
        "encoding": "UTF-8",
        "standalone": true
      }
    }
  })
  
  assert_true(xml_options_result.success)
  
  let xml_with_options = File::read(xml_options_result.file_path)
  assert_true(xml_with_options.contains("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"))
  
  // 测试压缩格式支持
  let compressed_result = format_manager.compress_file(json_to_csv_result.file_path, {
    algorithm: "gzip",
    keep_original: false
  })
  
  assert_true(compressed_result.success)
  assert_true(compressed_result.compressed)
  
  // 测试解压缩
  let decompressed_result = format_manager.decompress_file(compressed_result.file_path)
  assert_true(decompressed_result.success)
  
  // 测试格式转换性能
  let large_dataset = generate_large_dataset(10000)  // 10000条记录
  let performance_test = format_manager.convert_with_performance_tracking(large_dataset, {
    from: "json",
    to: "csv"
  })
  
  assert_true(performance_test.success)
  assert_true(performance_test.conversion_time > 0)
  assert_true(performance_test.memory_usage > 0)
  
  // 测试格式模板
  let template_registration = format_manager.register_format_template({
    name: "产品数据模板",
    source_format: "json",
    target_format: "xml",
    template: {
      root_element: "products",
      record_element: "product",
      field_mappings: {
        "id": "@id",
        "name": "name/text()",
        "price": "price",
        "available": "available",
        "tags": "tags/tag"
      },
      transformations: {
        "price": "format_currency",
        "available": "boolean_to_yes_no"
      }
    }
  })
  
  assert_true(template_registration.success)
  
  // 使用模板转换
  let template_conversion_result = format_manager.convert_with_template(
    test_data,
    "产品数据模板"
  )
  
  assert_true(template_conversion_result.success)
  
  // 测试格式版本控制
  let versioning_result = format_manager.enable_versioning("json", {
    max_versions: 5,
    auto_upgrade: false,
    compatibility_mode: "backward"
  })
  
  assert_true(versioning_result.success)
  
  // 测试格式扩展
  let custom_format_definition = {
    name: "custom_format",
    description: "自定义数据格式",
    file_extension: ".cst",
    mime_type: "application/custom",
    parser: "custom_parser",
    serializer: "custom_serializer"
  }
  
  let custom_format_registration = format_manager.register_custom_format(custom_format_definition)
  assert_true(custom_format_registration.success)
}

// 测试9: 大数据处理和优化
test "大数据处理和优化测试" {
  // 创建大数据处理管理器
  let big_data_manager = BigDataProcessingManager::new({
    processing_engine: "spark",
    cluster_config: {
      master: "spark://localhost:7077",
      executor_memory: "2g",
      executor_cores: 2,
      total_executors: 4
    },
    optimization: {
      partitioning: true,
      caching: true,
      parallelism: 8,
      batch_size: 1000
    },
    monitoring: {
      enabled: true,
      metrics_collection: true,
      performance_profiling: true
    }
  })
  
  // 生成大数据集
  let large_dataset = generate_large_dataset(100000)  // 100,000条记录
  
  // 测试分布式数据处理
  let distributed_processing = big_data_manager.process_distributed(large_dataset, {
    operations: [
      {
        type: "filter",
        condition: "amount > 1000"
      },
      {
        type: "group_by",
        columns: ["category", "region"]
      },
      {
        type: "aggregate",
        measures: [
          { column: "amount", function: "sum" },
          { column: "amount", function: "avg" }
        ]
      }
    ],
    output_format: "parquet",
    output_path: "/tmp/processed_data"
  })
  
  assert_true(distributed_processing.success)
  assert_true(distributed_processing.processed_records > 0)
  assert_true(distributed_processing.processing_time > 0)
  
  // 测试分区优化
  let partitioning_result = big_data_manager.optimize_partitioning(large_dataset, {
    partition_columns: ["category", "region"],
    partition_strategy: "hash",
    num_partitions: 16
  })
  
  assert_true(partitioning_result.success)
  assert_eq(partitioning_result.num_partitions, 16)
  
  // 测试数据缓存
  let caching_result = big_data_manager.cache_data(large_dataset, {
    cache_key: "sales_data_202201",
    storage_level: "memory_and_disk",
    eviction_policy: "lru"
  })
  
  assert_true(caching_result.success)
  
  // 测试缓存命中
  let cache_hit_result = big_data_manager.get_cached_data("sales_data_202201")
  assert_true(cache_hit_result.success)
  assert_true(cache_hit_result.from_cache)
  
  // 测试并行处理
  let parallel_processing = big_data_manager.process_parallel(large_dataset, {
    tasks: [
      {
        name: "统计分析",
        operations: ["describe", "correlation"],
        columns: ["amount", "quantity"]
      },
      {
        name: "异常检测",
        algorithm: "isolation_forest",
        features: ["amount", "quantity"]
      },
      {
        name: "聚类分析",
        algorithm: "kmeans",
        features: ["amount", "quantity"],
        k: 5
      }
    ],
    max_concurrent_tasks: 3
  })
  
  assert_true(parallel_processing.success)
  assert_eq(parallel_processing.completed_tasks, 3)
  
  // 测试流式处理
  let stream_processing = big_data_manager.setup_stream_processing({
    input_source: "kafka",
    topic: "sales_events",
    processing_interval: 5000,  // 5秒
    window_size: "1分钟",
    operations: [
      {
        type: "windowed_aggregation",
        window: "tumbling",
        duration: "1minute",
        aggregations: ["sum", "count", "avg"]
      },
      {
        type: "anomaly_detection",
        algorithm: "streaming_anomaly_detection",
        threshold: 2.0
      }
    ]
  })
  
  assert_true(stream_processing.success)
  
  // 模拟流数据
  let stream_data = generate_stream_data(1000)
  let stream_processing_result = big_data_manager.process_stream_data(stream_data)
  assert_true(stream_processing_result.success)
  
  // 测试数据倾斜处理
  let skewed_data = generate_skewed_data(50000)  // 生成倾斜数据
  let skew_handling = big_data_manager.handle_data_skew(skewed_data, {
    skew_detection: true,
    skew_threshold: 3.0,
    salting: true,
    broadcast_join_threshold: 10000
  })
  
  assert_true(skew_handling.success)
  assert_true(skew_handling.skew_detected)
  
  // 测试增量处理
  let incremental_processing = big_data_manager.incremental_processing({
    base_data: large_dataset.slice(0, 50000),  // 前50%作为基础数据
    incremental_data: large_dataset.slice(50000, 50000),  // 后50%作为增量数据
    key_columns: ["id"],
    merge_strategy: "upsert"
  })
  
  assert_true(incremental_processing.success)
  assert_true(incremental_processing.processed_incremental_records > 0)
  
  // 测试性能优化
  let optimization_result = big_data_manager.optimize_performance({
    query: "SELECT category, SUM(amount) FROM sales GROUP BY category",
    optimizations: [
      "predicate_pushdown",
      "column_pruning",
      "broadcast_join",
      "adaptive_query_execution"
    ]
  })
  
  assert_true(optimization_result.success)
  assert_true(optimization_result.optimizations_applied.length() > 0)
  
  // 测试资源管理
  let resource_management = big_data_manager.manage_resources({
    executor_memory: "4g",
    executor_cores: 4,
    total_executors: 2,
    adaptive_scaling: true,
    scaling_thresholds: {
      cpu_usage: 0.8,
      memory_usage: 0.85
    }
  })
  
  assert_true(resource_management.success)
  
  // 测试成本优化
  let cost_optimization = big_data_manager.optimize_cost({
    cluster_type: "spot",
    auto_scaling: true,
    preemptible_instances: true,
    data_locality: true,
    compression: "snappy"
  })
  
  assert_true(cost_optimization.success)
  
  // 测试故障恢复
  let fault_tolerance = big_data_manager.setup_fault_tolerance({
    retry_attempts: 3,
    retry_delay: 5000,
    checkpoint_interval: 60000,  // 1分钟
    speculative_execution: true
  })
  
  assert_true(fault_tolerance.success)
  
  // 测试监控和指标
  let monitoring_metrics = big_data_manager.get_monitoring_metrics({
    time_range: "1h",
    include_system_metrics: true,
    include_application_metrics: true,
    include_job_metrics: true
  })
  
  assert_true(monitoring_metrics.success)
  assert_true(monitoring_metrics.metrics.length() > 0)
  
  // 测试作业调度
  let job_scheduling = big_data_manager.schedule_job({
    name: "每日销售数据处理",
    schedule: "0 2 * * *",  // 每天凌晨2点
    job_config: {
      input_path: "/data/sales/raw",
      output_path: "/data/sales/processed",
      transformations: ["clean", "aggregate", "validate"]
    },
    notification: {
      on_success: ["email"],
      on_failure: ["email", "slack"]
    }
  })
  
  assert_true(job_scheduling.success)
  
  // 测试数据血缘追踪
  let lineage_tracking = big_data_manager.track_data_lineage({
    source_systems: ["mysql", "kafka"],
    transformations: ["cleaning", "aggregation"],
    output_systems: ["hdfs", "hive"]
  })
  
  assert_true(lineage_tracking.success)
  assert_true(lineage_tracking.lineage_graph.nodes.length() > 0)
}

// 测试10: 数据安全和隐私保护
test "数据安全和隐私保护测试" {
  // 创建数据安全管理器
  let security_manager = DataSecurityManager::new({
    encryption_algorithm: "AES-256-GCM",
    key_management: "hsm",
    access_control: "rbac",
    audit_logging: true
  })
  
  // 准备敏感数据
  let sensitive_data = [
    {
      "id": 1,
      "name": "张三",
      "id_card": "110101199001011234",
      "phone": "13800138000",
      "email": "zhangsan@example.com",
      "address": "北京市朝阳区xxx街道xxx号",
      "bank_account": "6222021234567890123"
    },
    {
      "id": 2,
      "name": "李四",
      "id_card": "110101199002022345",
      "phone": "13900139000",
      "email": "lisi@example.com",
      "address": "上海市浦东新区xxx路xxx号",
      "bank_account": "6222021234567890456"
    }
  ]
  
  // 测试数据脱敏
  let masking_result = security_manager.mask_data(sensitive_data, {
    masking_rules: [
      {
        field: "id_card",
        type: "partial",
        visible_chars: 6,
        mask_char: "*",
        preserve_format: true
      },
      {
        field: "phone",
        type: "partial",
        visible_chars: 3,
        mask_char: "*",
        preserve_format: true
      },
      {
        field: "email",
        type: "domain_preserve",
        mask_char: "*"
      },
      {
        field: "address",
        type: "hash",
        algorithm: "sha256"
      },
      {
        field: "bank_account",
        type: "tokenization",
        token_format: "TOKEN_{random}"
      }
    ]
  })
  
  assert_true(masking_result.success)
  
  let masked_data = masking_result.masked_data
  let first_masked_record = masked_data[0]
  
  // 验证脱敏结果
  assert_eq(first_masked_record.get("name"), Some("张三"))  // 未脱敏字段
  assert_eq(first_masked_record.get("id_card"), Some("110101********1234"))  // 部分脱敏
  assert_eq(first_masked_record.get("phone"), Some("138********00"))  // 部分脱敏
  assert_true(first_masked_record.get("email").unwrap_or("").contains("@"))  // 保留域名
  assert_ne(first_masked_record.get("address"), Some("北京市朝阳区xxx街道xxx号"))  // 哈希化
  assert_true(first_masked_record.get("bank_account").unwrap_or("").starts_with("TOKEN_"))  // 令牌化
  
  // 测试数据加密
  let encryption_result = security_manager.encrypt_data(sensitive_data, {
    algorithm: "AES-256-GCM",
    key_id: "data_encryption_key_001",
    aad: "sales_data_202201",
    compression: true
  })
  
  assert_true(encryption_result.success)
  assert_true(encryption_result.encrypted_data.length() > 0)
  assert_true(encryption_result.metadata.key_id == "data_encryption_key_001")
  
  // 测试数据解密
  let decryption_result = security_manager.decrypt_data(
    encryption_result.encrypted_data,
    encryption_result.metadata
  )
  
  assert_true(decryption_result.success)
  assert_eq(decryption_result.decrypted_data.length(), sensitive_data.length())
  
  // 测试访问控制
  let access_control_setup = security_manager.setup_access_control({
    policies: [
      {
        name: "销售数据访问",
        resources: ["sales_data"],
        actions: ["read", "write"],
        subjects: ["sales_team", "managers"],
        conditions: {
          department: "sales",
          data_classification: "internal"
        }
      },
      {
        name: "敏感数据访问",
        resources: ["sensitive_data"],
        actions: ["read"],
        subjects: ["admin", "data_protection_officer"],
        conditions: {
          purpose: "business_need",
          approval_required: true
        }
      }
    ]
  })
  
  assert_true(access_control_setup.success)
  
  // 测试访问权限检查
  let access_check = security_manager.check_access({
    subject: "sales_team_member",
    resource: "sales_data",
    action: "read",
    context: {
      department: "sales",
      data_classification: "internal"
    }
  })
  
  assert_true(access_check.allowed)
  
  let unauthorized_access = security_manager.check_access({
    subject: "intern",
    resource: "sensitive_data",
    action: "read",
    context: {
      department: "engineering",
      data_classification: "confidential"
    }
  })
  
  assert_false(unauthorized_access.allowed)
  
  // 测试数据分类
  let classification_result = security_manager.classify_data(sensitive_data, {
    classification_scheme: "sensitivity",
    auto_classify: true,
    rules: [
      {
        field: "id_card",
        classification: "confidential",
        keywords: ["id", "card"]
      },
      {
        field: "bank_account",
        classification: "restricted",
        keywords: ["bank", "account"]
      }
    ]
  })
  
  assert_true(classification_result.success)
  
  let classified_data = classification_result.classified_data
  let first_classified_record = classified_data[0]
  assert_eq(first_classified_record.get("id_card_classification"), Some("confidential"))
  assert_eq(first_classified_record.get("bank_account_classification"), Some("restricted"))
  
  // 测试数据水印
  let watermarking_result = security_manager.add_watermark(sensitive_data, {
    type: "invisible",
    method: "lsb",
    watermark_text: "CONFIDENTIAL_USER_123",
    strength: "medium"
  })
  
  assert_true(watermarking_result.success)
  
  // 测试水印检测
  let watermark_detection = security_manager.detect_watermark(watermarking_result.watermarked_data)
  assert_true(watermark_detection.detected)
  assert_eq(watermark_detection.watermark_text, Some("CONFIDENTIAL_USER_123"))
  
  // 测试数据完整性验证
  let integrity_check = security_manager.verify_data_integrity(sensitive_data, {
    algorithm: "SHA-256",
    include_metadata: true
  })
  
  assert_true(integrity_check.valid)
  assert_true(integrity_check.checksum.length() > 0)
  
  // 修改数据以测试完整性
  let tampered_data = sensitive_data
  tampered_data[0] = { tampered_data[0] | name: "已修改" }
  
  let tampered_check = security_manager.verify_data_integrity(tampered_data, {
    algorithm: "SHA-256",
    include_metadata: true,
    original_checksum: integrity_check.checksum
  })
  
  assert_false(tampered_check.valid)
  
  // 测试数据审计日志
  let audit_log = security_manager.get_audit_log({
    time_range: "24h",
    events: ["data_access", "data_modification", "encryption", "decryption"],
    users: ["admin"],
    resources: ["sensitive_data"]
  })
  
  assert_true(audit_log.success)
  assert_true(audit_log.entries.length() > 0)
  
  // 测试数据备份和恢复
  let backup_result = security_manager.create_secure_backup(sensitive_data, {
    backup_type: "encrypted",
    storage_location: "secure_backup_storage",
    retention_period: 90,
    compression: true
  })
  
  assert_true(backup_result.success)
  assert_true(backup_result.backup_id != None)
  
  let restore_result = security_manager.restore_from_backup(backup_result.backup_id.unwrap(), {
    validate_integrity: true,
    decrypt: true
  })
  
  assert_true(restore_result.success)
  assert_eq(restore_result.restored_data.length(), sensitive_data.length())
  
  // 测试隐私政策合规
  let privacy_compliance = security_manager.check_privacy_compliance(sensitive_data, {
    regulations: ["GDPR", "CCPA", "PIPL"],
    consent_required: true,
    data_minimization: true,
    purpose_limitation: true
  })
  
  assert_true(privacy_compliance.compliant)
  assert_true(privacy_compliance.assessments.length() > 0)
  
  // 测试数据匿名化
  let anonymization_result = security_manager.anonymize_data(sensitive_data, {
    method: "k_anonymity",
    k_value: 5,
    quasi_identifiers: ["age", "gender", "city"],
    sensitive_attributes: ["name", "phone", "email"]
  })
  
  assert_true(anonymization_result.success)
  
  let anonymized_data = anonymization_result.anonymized_data
  assert_true(anonymized_data.length() > 0)
  
  // 测试数据生命周期管理
  let lifecycle_result = security_manager.manage_data_lifecycle({
    data: sensitive_data,
    retention_policy: {
      classification_based: true,
      retention_periods: {
        "public": 365,
        "internal": 1095,
        "confidential": 1825,
        "restricted": 2555
      }
    },
    auto_deletion: true,
    deletion_notification: true
  })
  
  assert_true(lifecycle_result.success)
}

// 辅助函数：生成大型数据集
fn generate_large_dataset(count: Int) -> Array[Map[String, Any]] {
  let mut data = []
  for i in 0..=count - 1 {
    data = data.push({
      "id": i + 1,
      "name": "产品" + (i + 1).to_string(),
      "category": ["电子", "家居", "服装", "食品"][i % 4],
      "amount": (i + 1) * 10.5,
      "quantity": i % 100 + 1,
      "region": ["华北", "华东", "华南", "华西"][i % 4],
      "date": "2022-01-" + ((i % 28) + 1).to_string()
    })
  }
  data
}

// 辅助函数：生成流数据
fn generate_stream_data(count: Int) -> Array[Map[String, Any]] {
  let mut data = []
  let base_time = Time::now()
  
  for i in 0..=count - 1 {
    data = data.push({
      "event_id": "evt_" + (i + 1).to_string(),
      "timestamp": base_time + i * 1000,
      "event_type": ["purchase", "browse", "search"][i % 3],
      "user_id": "user_" + (i % 100 + 1).to_string(),
      "amount": (i + 1) * 5.5
    })
  }
  data
}

// 辅助函数：生成倾斜数据
fn generate_skewed_data(count: Int) -> Array[Map[String, Any]] {
  let mut data = []
  
  // 80%的数据集中在少数几个类别
  for i in 0..=count - 1 {
    let category = if i < count * 0.8 {
      "hot_category"
    } else {
      "cold_category_" + (i % 10).to_string()
    }
    
    data = data.push({
      "id": i + 1,
      "category": category,
      "amount": (i + 1) * 10.0
    })
  }
  data
}