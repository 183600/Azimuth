// Azimuth 遥测数据导出和导入测试
// 测试遥测数据的导出和导入功能

// 测试1: JSON格式导出和导入
test "JSON格式导出和导入测试" {
  // 创建导出导入管理器
  let export_import_manager = ExportImportManager::new()
  
  // 创建测试数据
  let test_spans = []
  let test_metrics = []
  let test_logs = []
  
  // 生成测试Span数据
  for i in 0..50 {
    let span = {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      parent_span_id: if i > 0 { Some("span-" + (i - 1).to_string()) } else { None },
      operation_name: "operation." + (i % 10).to_string(),
      service_name: "service-" + (i % 5).to_string(),
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      status: if i % 10 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 4) * 100).to_string()),
        ("user.id", "user-" + (i % 20).to_string())
      ],
      events: [
        {
          name: "event.start",
          timestamp: 1640995200 + i * 1000,
          attributes: [("component", "client")]
        },
        {
          name: "event.end",
          timestamp: 1640995250 + i * 1000,
          attributes: [("component", "server")]
        }
      ]
    }
    
    test_spans = test_spans.push(span)
  }
  
  // 生成测试Metric数据
  for i in 0..30 {
    let metric = {
      name: "metric." + (i % 5).to_string(),
      description: "Test metric " + i.to_string(),
      unit: "ms",
      value: 100.0 + i.to_float() * 10.0,
      timestamp: 1640995200 + i * 2000,
      tags: [
        ("service.name", "service-" + (i % 5).to_string()),
        ("environment", "production")
      ]
    }
    
    test_metrics = test_metrics.push(metric)
  }
  
  // 生成测试Log数据
  for i in 0..20 {
    let log = {
      timestamp: 1640995200 + i * 3000,
      level: if i % 4 == 0 { "error" } else if i % 4 == 1 { "warn" } else if i % 4 == 2 { "info" } else { "debug" },
      message: "Test log message " + i.to_string(),
      service_name: "service-" + (i % 5).to_string(),
      trace_id: "trace-" + (i % 10).to_string(),
      span_id: "span-" + i.to_string(),
      fields: [
        ("logger", "test.logger"),
        ("thread", "thread-" + (i % 3).to_string())
      ]
    }
    
    test_logs = test_logs.push(log)
  }
  
  // 导出为JSON格式
  let json_export_config = {
    format: "json",
    compression: false,
    include_metadata: true,
    pretty_print: true,
    batch_size: 100
  }
  
  let json_export_result = ExportImportManager::export_data(export_import_manager, {
    spans: test_spans,
    metrics: test_metrics,
    logs: test_logs
  }, json_export_config)
  
  // 验证JSON导出结果
  assert_true(json_export_result.success)
  assert_true(json_export_result.file_path.length() > 0)
  assert_true(json_export_result.file_size > 0)
  assert_eq(json_export_result.exported_counts.spans, 50)
  assert_eq(json_export_result.exported_counts.metrics, 30)
  assert_eq(json_export_result.exported_counts.logs, 20)
  
  // 验证JSON文件内容
  let json_content = File::read_to_string(json_export_result.file_path)
  assert_true(json_content.length() > 0)
  assert_true(json_content.contains("\"spans\""))
  assert_true(json_content.contains("\"metrics\""))
  assert_true(json_content.contains("\"logs\""))
  assert_true(json_content.contains("\"metadata\""))
  
  // 从JSON导入
  let json_import_config = {
    format: "json",
    validate_on_import: true,
    skip_invalid_items: true,
    batch_size: 50
  }
  
  let json_import_result = ExportImportManager::import_data(export_import_manager, json_export_result.file_path, json_import_config)
  
  // 验证JSON导入结果
  assert_true(json_import_result.success)
  assert_eq(json_import_result.imported_counts.spans, 50)
  assert_eq(json_import_result.imported_counts.metrics, 30)
  assert_eq(json_import_result.imported_counts.logs, 20)
  assert_eq(json_import_result.skipped_count, 0)
  assert_eq(json_import_result.error_count, 0)
  
  // 验证导入的数据完整性
  let imported_spans = json_import_result.imported_data.spans
  let imported_metrics = json_import_result.imported_data.metrics
  let imported_logs = json_import_result.imported_data.logs
  
  assert_eq(imported_spans.length(), test_spans.length())
  assert_eq(imported_metrics.length(), test_metrics.length())
  assert_eq(imported_logs.length(), test_logs.length())
  
  // 验证Span数据
  for i in 0..imported_spans.length() {
    let imported = imported_spans[i]
    let original = test_spans[i]
    
    assert_eq(imported.trace_id, original.trace_id)
    assert_eq(imported.span_id, original.span_id)
    assert_eq(imported.operation_name, original.operation_name)
    assert_eq(imported.service_name, original.service_name)
    assert_eq(imported.start_time, original.start_time)
    assert_eq(imported.end_time, original.end_time)
    assert_eq(imported.status, original.status)
    
    // 验证属性
    for attr in original.attributes {
      let found = imported.attributes.any(fn(a) { 
        a[0] == attr[0] and a[1] == attr[1] 
      })
      assert_true(found)
    }
    
    // 验证事件
    assert_eq(imported.events.length(), original.events.length())
    for j in 0..imported.events.length() {
      assert_eq(imported.events[j].name, original.events[j].name)
      assert_eq(imported.events[j].timestamp, original.events[j].timestamp)
    }
  }
  
  // 验证Metric数据
  for i in 0..imported_metrics.length() {
    let imported = imported_metrics[i]
    let original = test_metrics[i]
    
    assert_eq(imported.name, original.name)
    assert_eq(imported.description, original.description)
    assert_eq(imported.unit, original.unit)
    assert_eq(imported.value, original.value)
    assert_eq(imported.timestamp, original.timestamp)
    
    // 验证标签
    for tag in original.tags {
      let found = imported.tags.any(fn(t) { 
        t[0] == tag[0] and t[1] == tag[1] 
      })
      assert_true(found)
    }
  }
  
  // 验证Log数据
  for i in 0..imported_logs.length() {
    let imported = imported_logs[i]
    let original = test_logs[i]
    
    assert_eq(imported.timestamp, original.timestamp)
    assert_eq(imported.level, original.level)
    assert_eq(imported.message, original.message)
    assert_eq(imported.service_name, original.service_name)
    assert_eq(imported.trace_id, original.trace_id)
    assert_eq(imported.span_id, original.span_id)
    
    // 验证字段
    for field in original.fields {
      let found = imported.fields.any(fn(f) { 
        f[0] == field[0] and f[1] == field[1] 
      })
      assert_true(found)
    }
  }
}

// 测试2: CSV格式导出和导入
test "CSV格式导出和导入测试" {
  // 创建导出导入管理器
  let export_import_manager = ExportImportManager::new()
  
  // 创建测试数据
  let test_metrics = []
  
  for i in 0..100 {
    let metric = {
      name: "response.time",
      value: 50.0 + (i % 50).to_float() * 2.0,
      timestamp: 1640995200 + i * 60000,  // 每分钟一个数据点
      tags: [
        ("service", "service-" + (i % 5).to_string()),
        ("endpoint", "/api/endpoint-" + (i % 10).to_string()),
        ("method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("status", (200 + (i % 3) * 100).to_string())
      ]
    }
    
    test_metrics = test_metrics.push(metric)
  }
  
  // 导出为CSV格式
  let csv_export_config = {
    format: "csv",
    include_headers: true,
    delimiter: ",",
    quote_character: "\"",
    escape_character: "\\",
    line_terminator: "\n",
    date_format: "ISO8601",
    flatten_tags: true
  }
  
  let csv_export_result = ExportImportManager::export_metrics(export_import_manager, test_metrics, csv_export_config)
  
  // 验证CSV导出结果
  assert_true(csv_export_result.success)
  assert_true(csv_export_result.file_path.length() > 0)
  assert_true(csv_export_result.file_size > 0)
  assert_eq(csv_export_result.exported_count, 100)
  
  // 验证CSV文件内容
  let csv_content = File::read_to_string(csv_export_result.file_path)
  let csv_lines = csv_content.split("\n")
  
  // 应该有101行（包括标题行）
  assert_eq(csv_lines.length(), 101)
  
  // 验证标题行
  let headers = csv_lines[0].split(",")
  assert_true(headers.contains("name"))
  assert_true(headers.contains("value"))
  assert_true(headers.contains("timestamp"))
  assert_true(headers.contains("tag.service"))
  assert_true(headers.contains("tag.endpoint"))
  assert_true(headers.contains("tag.method"))
  assert_true(headers.contains("tag.status"))
  
  // 验证数据行
  for i in 1..csv_lines.length() - 1 {
    let line = csv_lines[i]
    assert_true(line.length() > 0)
    
    let fields = line.split(",")
    assert_eq(fields.length(), headers.length())
    
    // 验证必填字段
    assert_true(fields[0].length() > 0)  // name
    assert_true(fields[1].length() > 0)  // value
    assert_true(fields[2].length() > 0)  // timestamp
  }
  
  // 从CSV导入
  let csv_import_config = {
    format: "csv",
    delimiter: ",",
    quote_character: "\"",
    escape_character: "\\",
    has_headers: true,
    date_format: "ISO8601",
    infer_types: true,
    skip_empty_lines: true,
    batch_size: 50
  }
  
  let csv_import_result = ExportImportManager::import_metrics(export_import_manager, csv_export_result.file_path, csv_import_config)
  
  // 验证CSV导入结果
  assert_true(csv_import_result.success)
  assert_eq(csv_import_result.imported_count, 100)
  assert_eq(csv_import_result.skipped_count, 0)
  assert_eq(csv_import_result.error_count, 0)
  
  // 验证导入的数据完整性
  let imported_metrics = csv_import_result.imported_data
  assert_eq(imported_metrics.length(), test_metrics.length())
  
  for i in 0..imported_metrics.length() {
    let imported = imported_metrics[i]
    let original = test_metrics[i]
    
    assert_eq(imported.name, original.name)
    assert_eq(imported.value, original.value)
    assert_eq(imported.timestamp, original.timestamp)
    
    // 验证标签
    for tag in original.tags {
      let found = imported.tags.any(fn(t) { 
        t[0] == tag[0] and t[1] == tag[1] 
      })
      assert_true(found)
    }
  }
  
  // 测试CSV导出性能
  let large_metrics = []
  
  for i in 0..10000 {
    let metric = {
      name: "large.metric." + (i % 100).to_string(),
      value: 100.0 + i.to_float(),
      timestamp: 1640995200 + i * 60,
      tags: [
        ("service", "service-" + (i % 20).to_string()),
        ("region", "region-" + (i % 5).to_string())
      ]
    }
    
    large_metrics = large_metrics.push(metric)
  }
  
  let csv_large_export_start = Time::now()
  let csv_large_export_result = ExportImportManager::export_metrics(export_import_manager, large_metrics, csv_export_config)
  let csv_large_export_time = Time::now() - csv_large_export_start
  
  // 验证大数据集导出性能
  assert_true(csv_large_export_result.success)
  assert_eq(csv_large_export_result.exported_count, 10000)
  assert_true(csv_large_export_time < 30000)  // 应该在30秒内完成
  
  let csv_large_import_start = Time::now()
  let csv_large_import_result = ExportImportManager::import_metrics(export_import_manager, csv_large_export_result.file_path, csv_import_config)
  let csv_large_import_time = Time::now() - csv_large_import_start
  
  // 验证大数据集导入性能
  assert_true(csv_large_import_result.success)
  assert_eq(csv_large_import_result.imported_count, 10000)
  assert_true(csv_large_import_time < 30000)  // 应该在30秒内完成
}

// 测试3: Parquet格式导出和导入
test "Parquet格式导出和导入测试" {
  // 创建导出导入管理器
  let export_import_manager = ExportImportManager::new()
  
  // 创建测试数据
  let test_spans = []
  
  for i in 0..1000 {
    let span = {
      trace_id: "trace-" + (i % 100).to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + (i % 20).to_string(),
      service_name: "service-" + (i % 10).to_string(),
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      duration: 50,
      status: if i % 20 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 5) * 100).to_string()),
        ("user.id", "user-" + (i % 50).to_string()),
        ("request.size", (1000 + i * 10).to_string()),
        ("response.size", (2000 + i * 20).to_string())
      ]
    }
    
    test_spans = test_spans.push(span)
  }
  
  // 导出为Parquet格式
  let parquet_export_config = {
    format: "parquet",
    compression: "snappy",
    row_group_size: 1000,
    page_size: 8192,
    dictionary_encoding: true,
    statistics: true,
    partition_by: ["service_name", "status"]
  }
  
  let parquet_export_result = ExportImportManager::export_spans(export_import_manager, test_spans, parquet_export_config)
  
  // 验证Parquet导出结果
  assert_true(parquet_export_result.success)
  assert_true(parquet_export_result.file_path.length() > 0)
  assert_true(parquet_export_result.file_size > 0)
  assert_eq(parquet_export_result.exported_count, 1000)
  
  // 验证分区文件
  assert_true(parquet_export_result.partition_files.length() > 0)
  
  // 验证压缩效果
  let json_size_estimate = test_spans.reduce(fn(acc, span) { 
    acc + span.to_string().length() 
  }, 0)
  
  let compression_ratio = parquet_export_result.file_size.to_float() / json_size_estimate.to_float()
  assert_true(compression_ratio < 0.5)  // Parquet压缩应该比JSON小至少50%
  
  // 从Parquet导入
  let parquet_import_config = {
    format: "parquet",
    parallelism: 4,
    batch_size: 1000,
    push_down_filters: true,
    column_pruning: true
  }
  
  let parquet_import_result = ExportImportManager::import_spans(export_import_manager, parquet_export_result.file_path, parquet_import_config)
  
  // 验证Parquet导入结果
  assert_true(parquet_import_result.success)
  assert_eq(parquet_import_result.imported_count, 1000)
  assert_eq(parquet_import_result.skipped_count, 0)
  assert_eq(parquet_import_result.error_count, 0)
  
  // 验证导入的数据完整性
  let imported_spans = parquet_import_result.imported_data
  assert_eq(imported_spans.length(), test_spans.length())
  
  for i in 0..imported_spans.length() {
    let imported = imported_spans[i]
    let original = test_spans[i]
    
    assert_eq(imported.trace_id, original.trace_id)
    assert_eq(imported.span_id, original.span_id)
    assert_eq(imported.operation_name, original.operation_name)
    assert_eq(imported.service_name, original.service_name)
    assert_eq(imported.start_time, original.start_time)
    assert_eq(imported.end_time, original.end_time)
    assert_eq(imported.duration, original.duration)
    assert_eq(imported.status, original.status)
    
    // 验证属性
    for attr in original.attributes {
      let found = imported.attributes.any(fn(a) { 
        a[0] == attr[0] and a[1] == attr[1] 
      })
      assert_true(found)
    }
  }
  
  // 测试Parquet列裁剪
  let column_pruning_config = { parquet_import_config | columns: ["trace_id", "span_id", "service_name", "duration"] }
  
  let column_pruning_result = ExportImportManager::import_spans(export_import_manager, parquet_export_result.file_path, column_pruning_config)
  
  // 验证列裁剪结果
  assert_true(column_pruning_result.success)
  assert_eq(column_pruning_result.imported_count, 1000)
  
  // 验证只导入了指定的列
  let pruned_spans = column_pruning_result.imported_data
  for span in pruned_spans {
    assert_true(span.trace_id.length() > 0)
    assert_true(span.span_id.length() > 0)
    assert_true(span.service_name.length() > 0)
    assert_true(span.duration > 0)
    
    // 其他列应该为空或默认值
    assert_eq(span.operation_name, "")
    assert_eq(span.start_time, 0)
    assert_eq(span.end_time, 0)
    assert_eq(span.status, "")
    assert_eq(span.attributes.length(), 0)
  }
  
  // 测试Parquet谓词下推
  let predicate_pushdown_config = { parquet_import_config | 
    filters: [
      { column: "service_name", operator: "equals", value: "service-1" },
      { column: "status", operator: "equals", value: "ok" }
    ]
  }
  
  let predicate_pushdown_result = ExportImportManager::import_spans(export_import_manager, parquet_export_result.file_path, predicate_pushdown_config)
  
  // 验证谓词下推结果
  assert_true(predicate_pushdown_result.success)
  assert_true(predicate_pushdown_result.imported_count < 1000)  // 应该只导入过滤后的数据
  
  let filtered_spans = predicate_pushdown_result.imported_data
  for span in filtered_spans {
    assert_eq(span.service_name, "service-1")
    assert_eq(span.status, "ok")
  }
  
  // 测试Parquet性能
  let large_spans = []
  
  for i in 0..100000 {
    let span = {
      trace_id: "trace-" + (i % 1000).to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + (i % 100).to_string(),
      service_name: "service-" + (i % 50).to_string(),
      start_time: 1640995200 + i * 10,
      end_time: 1640995250 + i * 10,
      duration: 50,
      status: if i % 50 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 5) * 100).to_string())
      ]
    }
    
    large_spans = large_spans.push(span)
  }
  
  let parquet_large_export_start = Time::now()
  let parquet_large_export_result = ExportImportManager::export_spans(export_import_manager, large_spans, parquet_export_config)
  let parquet_large_export_time = Time::now() - parquet_large_export_start
  
  // 验证大数据集Parquet导出性能
  assert_true(parquet_large_export_result.success)
  assert_eq(parquet_large_export_result.exported_count, 100000)
  assert_true(parquet_large_export_time < 60000)  // 应该在60秒内完成
  
  let parquet_large_import_start = Time::now()
  let parquet_large_import_result = ExportImportManager::import_spans(export_import_manager, parquet_large_export_result.file_path, parquet_import_config)
  let parquet_large_import_time = Time::now() - parquet_large_import_start
  
  // 验证大数据集Parquet导入性能
  assert_true(parquet_large_import_result.success)
  assert_eq(parquet_large_import_result.imported_count, 100000)
  assert_true(parquet_large_import_time < 60000)  // 应该在60秒内完成
}

// 测试4: 增量导出和导入
test "增量导出和导入测试" {
  // 创建导出导入管理器
  let export_import_manager = ExportImportManager::new()
  
  // 创建初始测试数据
  let initial_data = []
  
  for i in 0..100 {
    let data = {
      id: "data-" + i.to_string(),
      timestamp: 1640995200 + i * 60000,  // 每分钟一个数据点
      value: i * 10,
      category: "category-" + (i % 5).to_string(),
      updated_at: 1640995200 + i * 60000
    }
    
    initial_data = initial_data.push(data)
  }
  
  // 导出初始数据
  let initial_export_config = {
    format: "json",
    compression: true,
    include_metadata: true,
    incremental: false,
    checkpoint_enabled: true
  }
  
  let initial_export_result = ExportImportManager::export_data(export_import_manager, {
    spans: [],
    metrics: initial_data,
    logs: []
  }, initial_export_config)
  
  // 验证初始导出结果
  assert_true(initial_export_result.success)
  assert_eq(initial_export_result.exported_counts.metrics, 100)
  assert_true(initial_export_result.checkpoint != None)
  
  // 创建增量数据
  let incremental_data = []
  
  for i in 100..150 {
    let data = {
      id: "data-" + i.to_string(),
      timestamp: 1640995200 + i * 60000,
      value: i * 10,
      category: "category-" + (i % 5).to_string(),
      updated_at: 1640995200 + i * 60000
    }
    
    incremental_data = incremental_data.push(data)
  }
  
  // 更新一些现有数据
  let updated_data = []
  
  for i in 0..20 {
    let data = {
      id: "data-" + i.to_string(),
      timestamp: 1640995200 + i * 60000,
      value: i * 15,  // 更新值
      category: "category-" + ((i + 1) % 5).to_string(),  // 更新类别
      updated_at: 1640995200 + (100 + i) * 60000  // 更新时间戳
    }
    
    updated_data = updated_data.push(data)
  }
  
  // 导出增量数据
  let incremental_export_config = {
    format: "json",
    compression: true,
    include_metadata: true,
    incremental: true,
    checkpoint: initial_export_result.checkpoint,
    time_range: {
      start: 1640995200 + 100 * 60000,
      end: 1640995200 + 150 * 60000
    }
  }
  
  let incremental_export_result = ExportImportManager::export_data(export_import_manager, {
    spans: [],
    metrics: incremental_data + updated_data,
    logs: []
  }, incremental_export_config)
  
  // 验证增量导出结果
  assert_true(incremental_export_result.success)
  assert_eq(incremental_export_result.exported_counts.metrics, 70)  // 50个新数据 + 20个更新数据
  assert_true(incremental_export_result.checkpoint != None)
  
  // 导入初始数据
  let initial_import_config = {
    format: "json",
    validate_on_import: true,
    incremental: false,
    create_tables: true
  }
  
  let initial_import_result = ExportImportManager::import_data(export_import_manager, initial_export_result.file_path, initial_import_config)
  
  // 验证初始导入结果
  assert_true(initial_import_result.success)
  assert_eq(initial_import_result.imported_counts.metrics, 100)
  
  // 导入增量数据
  let incremental_import_config = {
    format: "json",
    validate_on_import: true,
    incremental: true,
    update_existing: true,
    conflict_resolution: "last_update_wins"
  }
  
  let incremental_import_result = ExportImportManager::import_data(export_import_manager, incremental_export_result.file_path, incremental_import_config)
  
  // 验证增量导入结果
  assert_true(incremental_import_result.success)
  assert_eq(incremental_import_result.imported_counts.metrics, 50)  // 50个新数据
  assert_eq(incremental_import_result.updated_counts.metrics, 20)   // 20个更新数据
  
  // 验证合并后的数据
  let merged_data = ExportImportManager::get_merged_data(export_import_manager, "metrics")
  assert_eq(merged_data.length(), 150)  // 100个初始数据 + 50个新数据
  
  // 验证更新后的数据
  let updated_item = merged_data.find(fn(d) { d.id == "data-0" })
  assert_true(updated_item != None)
  
  match updated_item {
    Some(item) => {
      assert_eq(item.value, 0 * 15)  // 更新后的值
      assert_eq(item.category, "category-1")  // 更新后的类别
    }
    None => assert_true(false)
  }
  
  // 测试增量导出性能
  let large_initial_data = []
  
  for i in 0..10000 {
    let data = {
      id: "large-data-" + i.to_string(),
      timestamp: 1640995200 + i * 60,
      value: i,
      category: "category-" + (i % 100).to_string(),
      updated_at: 1640995200 + i * 60
    }
    
    large_initial_data = large_initial_data.push(data)
  }
  
  let large_initial_export_start = Time::now()
  let large_initial_export_result = ExportImportManager::export_data(export_import_manager, {
    spans: [],
    metrics: large_initial_data,
    logs: []
  }, initial_export_config)
  let large_initial_export_time = Time::now() - large_initial_export_start
  
  // 验证大数据集初始导出性能
  assert_true(large_initial_export_result.success)
  assert_eq(large_initial_export_result.exported_counts.metrics, 10000)
  assert_true(large_initial_export_time < 30000)  // 应该在30秒内完成
  
  // 创建大数据集增量数据
  let large_incremental_data = []
  
  for i in 10000..11000 {
    let data = {
      id: "large-data-" + i.to_string(),
      timestamp: 1640995200 + i * 60,
      value: i,
      category: "category-" + (i % 100).to_string(),
      updated_at: 1640995200 + i * 60
    }
    
    large_incremental_data = large_incremental_data.push(data)
  }
  
  let large_incremental_export_start = Time::now()
  let large_incremental_export_result = ExportImportManager::export_data(export_import_manager, {
    spans: [],
    metrics: large_incremental_data,
    logs: []
  }, { incremental_export_config | checkpoint: large_initial_export_result.checkpoint })
  let large_incremental_export_time = Time::now() - large_incremental_export_start
  
  // 验证大数据集增量导出性能
  assert_true(large_incremental_export_result.success)
  assert_eq(large_incremental_export_result.exported_counts.metrics, 1000)
  assert_true(large_incremental_export_time < 10000)  // 应该在10秒内完成
  
  // 验证增量导出比全量导出更快
  let incremental_vs_full_ratio = large_incremental_export_time.to_float() / large_initial_export_time.to_float()
  assert_true(incremental_vs_full_ratio < 0.5)  // 增量导出应该比全量导出快至少50%
}

// 测试5: 跨格式转换导出导入
test "跨格式转换导出导入测试" {
  // 创建导出导入管理器
  let export_import_manager = ExportImportManager::new()
  
  // 创建测试数据
  let test_data = []
  
  for i in 0..500 {
    let data = {
      trace_id: "trace-" + (i % 50).to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + (i % 20).to_string(),
      service_name: "service-" + (i % 10).to_string(),
      start_time: 1640995200 + i * 1000,
      end_time: 1640995250 + i * 1000,
      duration: 50,
      status: if i % 25 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 4) * 100).to_string()),
        ("user.id", "user-" + (i % 100).to_string())
      ]
    }
    
    test_data = test_data.push(data)
  }
  
  // 导出为JSON格式
  let json_export_config = {
    format: "json",
    compression: false,
    include_metadata: true
  }
  
  let json_export_result = ExportImportManager::export_spans(export_import_manager, test_data, json_export_config)
  
  // 验证JSON导出结果
  assert_true(json_export_result.success)
  assert_eq(json_export_result.exported_count, 500)
  
  // 从JSON转换为Parquet
  let json_to_parquet_result = ExportImportManager::convert_format(export_import_manager, {
    input_file: json_export_result.file_path,
    input_format: "json",
    output_format: "parquet",
    output_compression: "snappy",
    preserve_schema: true
  })
  
  // 验证JSON到Parquet转换结果
  assert_true(json_to_parquet_result.success)
  assert_true(json_to_parquet_result.output_file_path.length() > 0)
  assert_true(json_to_parquet_result.output_file_size > 0)
  assert_eq(json_to_parquet_result.converted_count, 500)
  
  // 从Parquet转换为CSV
  let parquet_to_csv_result = ExportImportManager::convert_format(export_import_manager, {
    input_file: json_to_parquet_result.output_file_path,
    input_format: "parquet",
    output_format: "csv",
    include_headers: true,
    flatten_attributes: true
  })
  
  // 验证Parquet到CSV转换结果
  assert_true(parquet_to_csv_result.success)
  assert_true(parquet_to_csv_result.output_file_path.length() > 0)
  assert_true(parquet_to_csv_result.output_file_size > 0)
  assert_eq(parquet_to_csv_result.converted_count, 500)
  
  // 验证CSV文件内容
  let csv_content = File::read_to_string(parquet_to_csv_result.output_file_path)
  let csv_lines = csv_content.split("\n")
  
  // 应该有501行（包括标题行）
  assert_eq(csv_lines.length(), 501)
  
  // 验证标题行
  let headers = csv_lines[0].split(",")
  assert_true(headers.contains("trace_id"))
  assert_true(headers.contains("span_id"))
  assert_true(headers.contains("operation_name"))
  assert_true(headers.contains("service_name"))
  assert_true(headers.contains("start_time"))
  assert_true(headers.contains("end_time"))
  assert_true(headers.contains("duration"))
  assert_true(headers.contains("status"))
  
  // 从CSV导入验证数据完整性
  let csv_import_config = {
    format: "csv",
    has_headers: true,
    infer_types: true
  }
  
  let csv_import_result = ExportImportManager::import_spans(export_import_manager, parquet_to_csv_result.output_file_path, csv_import_config)
  
  // 验证CSV导入结果
  assert_true(csv_import_result.success)
  assert_eq(csv_import_result.imported_count, 500)
  
  // 验证转换后的数据完整性
  let converted_data = csv_import_result.imported_data
  assert_eq(converted_data.length(), test_data.length())
  
  for i in 0..converted_data.length() {
    let converted = converted_data[i]
    let original = test_data[i]
    
    assert_eq(converted.trace_id, original.trace_id)
    assert_eq(converted.span_id, original.span_id)
    assert_eq(converted.operation_name, original.operation_name)
    assert_eq(converted.service_name, original.service_name)
    assert_eq(converted.start_time, original.start_time)
    assert_eq(converted.end_time, original.end_time)
    assert_eq(converted.duration, original.duration)
    assert_eq(converted.status, original.status)
  }
  
  // 测试批量格式转换
  let batch_conversion_config = {
    input_files: [
      json_export_result.file_path,
      json_to_parquet_result.output_file_path
    ],
    input_format: "auto",  // 自动检测格式
    output_format: "avro",
    output_compression: "deflate",
    parallelism: 2
  }
  
  let batch_conversion_result = ExportImportManager::batch_convert_format(export_import_manager, batch_conversion_config)
  
  // 验证批量转换结果
  assert_true(batch_conversion_result.success)
  assert_eq(batch_conversion_result.converted_files.length(), 2)
  assert_eq(batch_conversion_result.total_converted_count, 1000)  // 500 + 500
  
  // 测试格式转换性能
  let large_test_data = []
  
  for i in 0..10000 {
    let data = {
      trace_id: "trace-" + (i % 1000).to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + (i % 200).to_string(),
      service_name: "service-" + (i % 100).to_string(),
      start_time: 1640995200 + i * 100,
      end_time: 1640995250 + i * 100,
      duration: 50,
      status: if i % 100 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", if i % 2 == 0 { "GET" } else { "POST" }),
        ("http.status_code", (200 + (i % 5) * 100).to_string())
      ]
    }
    
    large_test_data = large_test_data.push(data)
  }
  
  // 导出大数据集为JSON
  let large_json_export_start = Time::now()
  let large_json_export_result = ExportImportManager::export_spans(export_import_manager, large_test_data, json_export_config)
  let large_json_export_time = Time::now() - large_json_export_start
  
  // 验证大数据集JSON导出性能
  assert_true(large_json_export_result.success)
  assert_eq(large_json_export_result.exported_count, 10000)
  assert_true(large_json_export_time < 60000)  // 应该在60秒内完成
  
  // 转换为Parquet
  let large_conversion_start = Time::now()
  let large_conversion_result = ExportImportManager::convert_format(export_import_manager, {
    input_file: large_json_export_result.file_path,
    input_format: "json",
    output_format: "parquet",
    output_compression: "snappy"
  })
  let large_conversion_time = Time::now() - large_conversion_start
  
  // 验证大数据集转换性能
  assert_true(large_conversion_result.success)
  assert_eq(large_conversion_result.converted_count, 10000)
  assert_true(large_conversion_time < 60000)  // 应该在60秒内完成
  
  // 验证压缩效果
  let compression_ratio = large_conversion_result.output_file_size.to_float() / large_json_export_result.file_size.to_float()
  assert_true(compression_ratio < 0.5)  // Parquet压缩应该比JSON小至少50%
}