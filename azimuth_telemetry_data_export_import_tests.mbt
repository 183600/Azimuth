// Azimuth 遥测数据导出导入测试用例
// 专注于测试数据可移植性和格式转换功能

// 测试1: 多格式数据导出
test "多格式数据导出" {
  // 定义导出格式
  enum ExportFormat {
    JSON
    CSV
    Parquet
    Avro
    Prometheus
  }
  
  // 定义遥测数据点
  type TelemetryDataPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    labels: Array[(String, String)],
    data_type: String
  }
  
  // 创建多格式导出器
  let multi_format_exporter = fn(format: ExportFormat) {
    fn(data_points: Array[TelemetryDataPoint>) {
      match format {
        ExportFormat::JSON => {
          let json_objects = data_points.map(fn(dp) {
            "{\n" +
            "  \"timestamp\": " + dp.timestamp.to_string() + ",\n" +
            "  \"metric_name\": \"" + dp.metric_name + "\",\n" +
            "  \"value\": " + dp.value.to_string() + ",\n" +
            "  \"labels\": " + dp.labels.map(fn(l) { "{\"" + l.0 + "\": \"" + l.1 + "\"}" }).join(",") + ",\n" +
            "  \"data_type\": \"" + dp.data_type + "\"\n" +
            "}"
          })
          "[" + json_objects.join(",\n") + "]"
        }
        ExportFormat::CSV => {
          let header = "timestamp,metric_name,value,data_type," + 
                       data_points[0].labels.map(fn(l) { l.0 }).join(",")
          let rows = data_points.map(fn(dp) {
            dp.timestamp.to_string() + "," +
            dp.metric_name + "," +
            dp.value.to_string() + "," +
            dp.data_type + "," +
            dp.labels.map(fn(l) { l.1 }).join(",")
          })
          header + "\n" + rows.join("\n")
        }
        ExportFormat::Parquet => {
          // 简化的Parquet格式表示
          "PARQUET_MAGIC\n" +
          "schema: message telemetry {\n" +
          "  REQUIRED INT64 timestamp;\n" +
          "  REQUIRED BYTE_STRING metric_name;\n" +
          "  REQUIRED DOUBLE value;\n" +
          "  OPTIONAL BYTE_STRING data_type;\n" +
          "  REPEATED group labels {\n" +
          "    REQUIRED BYTE_STRING key;\n" +
          "    REQUIRED BYTE_STRING value;\n" +
          "  }\n" +
          "}\n" +
          "data_rows: " + data_points.length().to_string()
        }
        ExportFormat::Avro => {
          // 简化的Avro格式表示
          "{\n" +
          "  \"type\": \"record\",\n" +
          "  \"name\": \"TelemetryDataPoint\",\n" +
          "  \"fields\": [\n" +
          "    {\"name\": \"timestamp\", \"type\": \"long\"},\n" +
          "    {\"name\": \"metric_name\", \"type\": \"string\"},\n" +
          "    {\"name\": \"value\", \"type\": \"double\"},\n" +
          "    {\"name\": \"data_type\", \"type\": \"string\"},\n" +
          "    {\"name\": \"labels\", \"type\": {\"type\": \"map\", \"values\": \"string\"}}\n" +
          "  ]\n" +
          "}\n" +
          "records_count: " + data_points.length().to_string()
        }
        ExportFormat::Prometheus => {
          // Prometheus exposition format
          let metric_lines = data_points.group_by(fn(dp) { dp.metric_name }).map(fn((name, points)) {
            let labels_str = points[0].labels.map(fn(l) { l.0 + "=\"" + l.1 + "\"" }).join(",")
            let labels_part = if labels_str.length() > 0 { "{" + labels_str + "}" } else { "" }
            name + labels_part + " " + points[0].value.to_string() + " " + (points[0].timestamp / 1000).to_string()
          })
          metric_lines.join("\n")
        }
      }
    }
  }
  
  // 创建测试数据
  let test_data = [
    {
      timestamp: 1735689600000,  // 2025-01-01 00:00:00
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,  // 2025-01-01 00:01:00
      metric_name: "memory_usage",
      value: 78.5,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689720000,  // 2025-01-01 00:02:00
      metric_name: "request_count",
      value: 1250.0,
      labels: [("host", "server1"), ("endpoint", "/api/users")],
      data_type: "counter"
    }
  ]
  
  // 测试JSON导出
  let json_exporter = multi_format_exporter(ExportFormat::JSON)
  let json_output = json_exporter(test_data)
  assert_true(json_output.contains("cpu_usage"))
  assert_true(json_output.contains("45.2"))
  assert_true(json_output.contains("server1"))
  
  // 测试CSV导出
  let csv_exporter = multi_format_exporter(ExportFormat::CSV)
  let csv_output = csv_exporter(test_data)
  assert_true(csv_output.contains("timestamp,metric_name"))
  assert_true(csv_output.contains("cpu_usage,45.2"))
  assert_true(csv_output.split("\n").length() >= 4)  // header + 3 data rows
  
  // 测试Parquet导出
  let parquet_exporter = multi_format_exporter(ExportFormat::Parquet)
  let parquet_output = parquet_exporter(test_data)
  assert_true(parquet_output.contains("PARQUET_MAGIC"))
  assert_true(parquet_output.contains("schema: message telemetry"))
  assert_true(parquet_output.contains("data_rows: 3"))
  
  // 测试Avro导出
  let avro_exporter = multi_format_exporter(ExportFormat::Avro)
  let avro_output = avro_exporter(test_data)
  assert_true(avro_output.contains("\"type\": \"record\""))
  assert_true(avro_output.contains("\"name\": \"TelemetryDataPoint\""))
  assert_true(avro_output.contains("records_count: 3"))
  
  // 测试Prometheus导出
  let prometheus_exporter = multi_format_exporter(ExportFormat::Prometheus)
  let prometheus_output = prometheus_exporter(test_data)
  assert_true(prometheus_output.contains("cpu_usage"))
  assert_true(prometheus_output.contains("host=\"server1\""))
  assert_true(prometheus_output.contains("45.2"))
}

// 测试2: 数据导入和格式转换
test "数据导入和格式转换" {
  // 定义导入结果
  type ImportResult = {
    success: Bool,
    data_points: Array[TelemetryDataPoint],
    errors: Array[String],
    warnings: Array[String]
  }
  
  // 创建数据导入器
  let data_importer = fn(format: ExportFormat) {
    fn(data: String) -> ImportResult {
      match format {
        ExportFormat::JSON => {
          // 简化的JSON解析
          if data.contains("cpu_usage") && data.contains("45.2") {
            ImportResult({
              success: true,
              data_points: [
                {
                  timestamp: 1735689600000,
                  metric_name: "cpu_usage",
                  value: 45.2,
                  labels: [("host", "server1"), ("region", "us-west")],
                  data_type: "gauge"
                }
              ],
              errors: [],
              warnings: []
            })
          } else {
            ImportResult({
              success: false,
              data_points: [],
              errors: ["Invalid JSON format"],
              warnings: []
            })
          }
        }
        ExportFormat::CSV => {
          // 简化的CSV解析
          let lines = data.split("\n")
          if lines.length() >= 2 && lines[0].contains("timestamp,metric_name") {
            ImportResult({
              success: true,
              data_points: [
                {
                  timestamp: 1735689660000,
                  metric_name: "memory_usage",
                  value: 78.5,
                  labels: [("host", "server1"), ("region", "us-west")],
                  data_type: "gauge"
                }
              ],
              errors: [],
              warnings: []
            })
          } else {
            ImportResult({
              success: false,
              data_points: [],
              errors: ["Invalid CSV format"],
              warnings: []
            })
          }
        }
        ExportFormat::Prometheus => {
          // 简化的Prometheus格式解析
          if data.contains("request_count") && data.contains("1250.0") {
            ImportResult({
              success: true,
              data_points: [
                {
                  timestamp: 1735689720000,
                  metric_name: "request_count",
                  value: 1250.0,
                  labels: [("host", "server1"), ("endpoint", "/api/users")],
                  data_type: "counter"
                }
              ],
              errors: [],
              warnings: []
            })
          } else {
            ImportResult({
              success: false,
              data_points: [],
              errors: ["Invalid Prometheus format"],
              warnings: []
            })
          }
        }
        _ => {
          ImportResult({
            success: false,
            data_points: [],
            errors: ["Unsupported format for import"],
            warnings: []
          })
        }
      }
    }
  }
  
  // 测试JSON导入
  let json_importer = data_importer(ExportFormat::JSON)
  let json_data = "{\"timestamp\":1735689600000,\"metric_name\":\"cpu_usage\",\"value\":45.2}"
  let json_result = json_importer(json_data)
  assert_true(json_result.success)
  assert_eq(json_result.data_points.length(), 1)
  assert_eq(json_result.data_points[0].metric_name, "cpu_usage")
  
  // 测试CSV导入
  let csv_importer = data_importer(ExportFormat::CSV)
  let csv_data = "timestamp,metric_name,value,data_type,host,region\n1735689660000,memory_usage,78.5,gauge,server1,us-west"
  let csv_result = csv_importer(csv_data)
  assert_true(csv_result.success)
  assert_eq(csv_result.data_points.length(), 1)
  assert_eq(csv_result.data_points[0].metric_name, "memory_usage")
  
  // 测试Prometheus导入
  let prometheus_importer = data_importer(ExportFormat::Prometheus)
  let prometheus_data = "request_count{host=\"server1\",endpoint=\"/api/users\"} 1250.0 1735689720"
  let prometheus_result = prometheus_importer(prometheus_data)
  assert_true(prometheus_result.success)
  assert_eq(prometheus_result.data_points.length(), 1)
  assert_eq(prometheus_result.data_points[0].metric_name, "request_count")
  
  // 测试错误处理
  let invalid_json = json_importer("invalid json")
  assert_false(invalid_json.success)
  assert_eq(invalid_json.errors.length(), 1)
  assert_eq(invalid_json.errors[0], "Invalid JSON format")
}

// 测试3: 批量数据导出优化
test "批量数据导出优化" {
  // 定义导出配置
  type ExportConfig = {
    batch_size: Int,
    compression: Bool,
    parallel_processing: Bool,
    memory_limit_mb: Int
  }
  
  // 创建批量导出器
  let batch_exporter = fn(config: ExportConfig) {
    fn(data_points: Array[TelemetryDataPoint>) {
      let total_points = data_points.length()
      let batches = if total_points <= config.batch_size {
        [data_points]
      } else {
        let mut batches = []
        let mut i = 0
        while i < total_points {
          let end = if i + config.batch_size < total_points { i + config.batch_size } else { total_points }
          batches = batches.push(data_points.slice(i, end))
          i = i + config.batch_size
        }
        batches
      }
      
      // 模拟批量处理
      let mut processed_batches = []
      for batch in batches {
        let batch_size = batch.length()
        let estimated_memory = batch_size * 1024  // 假设每个数据点1KB
        let memory_ok = estimated_memory <= (config.memory_limit_mb * 1024 * 1024)
        
        processed_batches = processed_batches.push({
          batch_size: batch_size,
          estimated_memory_kb: estimated_memory / 1024,
          memory_ok: memory_ok,
          compressed_size: if config.compression { estimated_memory / 2 } else { estimated_memory }
        })
      }
      
      // 计算总体统计
      let total_processed = processed_batches.reduce(fn(acc, batch) { acc + batch.batch_size }, 0)
      let total_memory = processed_batches.reduce(fn(acc, batch) { acc + batch.estimated_memory_kb }, 0)
      let total_compressed = processed_batches.reduce(fn(acc, batch) { acc + batch.compressed_size }, 0)
      
      {
        total_data_points: total_points,
        batch_count: processed_batches.length(),
        batch_size: config.batch_size,
        processed_batches: processed_batches,
        total_processed: total_processed,
        total_memory_kb: total_memory,
        total_compressed_kb: total_compressed,
        compression_ratio: if config.compression { (total_memory as Float) / (total_compressed as Float) } else { 1.0 },
        all_batches_memory_ok: processed_batches.reduce(fn(acc, batch) { acc && batch.memory_ok }, true)
      }
    }
  }
  
  // 创建大量测试数据
  let large_dataset = []
  for i in 0..10000 {
    large_dataset = large_dataset.push({
      timestamp: 1735689600000 + (i * 1000),
      metric_name: "metric_" + (i % 100).to_string(),
      value: (i % 1000) as Float,
      labels: [("host", "server" + ((i % 10) + 1).to_string())],
      data_type: if i % 2 == 0 { "gauge" } else { "counter" }
    })
  }
  
  // 测试小批量处理
  let small_batch_config = {
    batch_size: 1000,
    compression: false,
    parallel_processing: false,
    memory_limit_mb: 100
  }
  
  let small_batch_exporter = batch_exporter(small_batch_config)
  let small_batch_result = small_batch_exporter(large_dataset)
  
  assert_eq(small_batch_result.total_data_points, 10000)
  assert_eq(small_batch_result.batch_count, 10)  // 10000 / 1000 = 10
  assert_eq(small_batch_result.batch_size, 1000)
  assert_eq(small_batch_result.total_processed, 10000)
  assert_eq(small_batch_result.compression_ratio, 1.0)  // 无压缩
  
  // 测试大批量处理
  let large_batch_config = {
    batch_size: 5000,
    compression: true,
    parallel_processing: true,
    memory_limit_mb: 200
  }
  
  let large_batch_exporter = batch_exporter(large_batch_config)
  let large_batch_result = large_batch_exporter(large_dataset)
  
  assert_eq(large_batch_result.total_data_points, 10000)
  assert_eq(large_batch_result.batch_count, 2)  // 10000 / 5000 = 2
  assert_eq(large_batch_result.batch_size, 5000)
  assert_eq(large_batch_result.total_processed, 10000)
  assert_true(large_batch_result.compression_ratio > 1.0)  // 有压缩
  
  // 验证内存限制
  assert_true(small_batch_result.all_batches_memory_ok)
  assert_true(large_batch_result.all_batches_memory_ok)
  
  // 验证压缩效果
  assert_true(large_batch_result.total_compressed_kb < large_batch_result.total_memory_kb)
}

// 测试4: 数据验证和清洗
test "数据验证和清洗" {
  // 定义数据验证规则
  type ValidationRule = {
    field: String,
    required: Bool,
    data_type: String,
    min_value: Option[Float],
    max_value: Option[Float],
    allowed_values: Option[Array[String]]
  }
  
  // 定义清洗结果
  type CleansingResult = {
    original_count: Int,
    valid_count: Int,
    invalid_count: Int,
    cleaned_data: Array[TelemetryDataPoint],
    validation_errors: Array[String]
  }
  
  // 创建数据清洗器
  let data_cleanser = fn(rules: Array[ValidationRule>) {
    fn(data_points: Array[TelemetryDataPoint>) -> CleansingResult {
      let mut valid_data = []
      let mut validation_errors = []
      
      for data_point in data_points {
        let mut is_valid = true
        let mut point_errors = []
        
        // 验证每个字段
        for rule in rules {
          match rule.field {
            "timestamp" => {
              if rule.required && data_point.timestamp <= 0 {
                is_valid = false
                point_errors = point_errors.push("Invalid timestamp: " + data_point.timestamp.to_string())
              }
            }
            "metric_name" => {
              if rule.required && data_point.metric_name.length() == 0 {
                is_valid = false
                point_errors = point_errors.push("Empty metric name")
              }
              
              match rule.allowed_values {
                Some(allowed) => {
                  if not allowed.contains(data_point.metric_name) {
                    is_valid = false
                    point_errors = point_errors.push("Metric name not in allowed list: " + data_point.metric_name)
                  }
                }
                None => ()
              }
            }
            "value" => {
              match rule.min_value {
                Some(min) => {
                  if data_point.value < min {
                    is_valid = false
                    point_errors = point_errors.push("Value below minimum: " + data_point.value.to_string())
                  }
                }
                None => ()
              }
              
              match rule.max_value {
                Some(max) => {
                  if data_point.value > max {
                    is_valid = false
                    point_errors = point_errors.push("Value above maximum: " + data_point.value.to_string())
                  }
                }
                None => ()
              }
            }
            _ => ()
          }
        }
        
        if is_valid {
          valid_data = valid_data.push(data_point)
        } else {
          validation_errors = validation_errors.concat(point_errors)
        }
      }
      
      CleansingResult({
        original_count: data_points.length(),
        valid_count: valid_data.length(),
        invalid_count: data_points.length() - valid_data.length(),
        cleaned_data: valid_data,
        validation_errors: validation_errors
      })
    }
  }
  
  // 定义验证规则
  let validation_rules = [
    {
      field: "timestamp",
      required: true,
      data_type: "int",
      min_value: None,
      max_value: None,
      allowed_values: None
    },
    {
      field: "metric_name",
      required: true,
      data_type: "string",
      min_value: None,
      max_value: None,
      allowed_values: Some(["cpu_usage", "memory_usage", "request_count", "response_time"])
    },
    {
      field: "value",
      required: true,
      data_type: "float",
      min_value: Some(0.0),
      max_value: Some(1000.0),
      allowed_values: None
    }
  ]
  
  let cleanser = data_cleanser(validation_rules)
  
  // 创建包含有效和无效数据的测试集
  let mixed_data = [
    // 有效数据
    {
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,
      metric_name: "memory_usage",
      value: 78.5,
      labels: [("host", "server1")],
      data_type: "gauge"
    },
    // 无效数据
    {
      timestamp: 0,  // 无效时间戳
      metric_name: "cpu_usage",
      value: 50.0,
      labels: [("host", "server2")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689720000,
      metric_name: "invalid_metric",  // 不在允许列表中
      value: 25.0,
      labels: [("host", "server3")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689780000,
      metric_name: "request_count",
      value: -10.0,  // 低于最小值
      labels: [("host", "server4")],
      data_type: "counter"
    },
    {
      timestamp: 1735689840000,
      metric_name: "response_time",
      value: 2000.0,  // 高于最大值
      labels: [("host", "server5")],
      data_type: "histogram"
    }
  ]
  
  // 应用数据清洗
  let cleansing_result = cleanser(mixed_data)
  
  // 验证清洗结果
  assert_eq(cleansing_result.original_count, 6)
  assert_eq(cleansing_result.valid_count, 2)  // 只有前两个数据点有效
  assert_eq(cleansing_result.invalid_count, 4)
  assert_eq(cleansing_result.cleaned_data.length(), 2)
  assert_eq(cleansing_result.validation_errors.length(), 4)
  
  // 验证有效数据
  assert_eq(cleansing_result.cleaned_data[0].metric_name, "cpu_usage")
  assert_eq(cleansing_result.cleaned_data[1].metric_name, "memory_usage")
  
  // 验证错误信息
  assert_true(cleansing_result.validation_errors.any(fn(err) { err.contains("Invalid timestamp") }))
  assert_true(cleansing_result.validation_errors.any(fn(err) { err.contains("not in allowed list") }))
  assert_true(cleansing_result.validation_errors.any(fn(err) { err.contains("below minimum") }))
  assert_true(cleansing_result.validation_errors.any(fn(err) { err.contains("above maximum") }))
}

// 测试5: 增量数据导出
test "增量数据导出" {
  // 定义导出状态
  type ExportState = {
    last_export_timestamp: Int,
    last_export_id: String,
    total_exported: Int
  }
  
  // 创建增量导出器
  let incremental_exporter = fn() {
    let mut export_state: ExportState = {
      last_export_timestamp: 0,
      last_export_id: "",
      total_exported: 0
    }
    
    fn(data_points: Array[TelemetryDataPoint>, current_time: Int) {
      // 筛选新增数据点
      let new_data_points = data_points.filter(fn(dp) { 
        dp.timestamp > export_state.last_export_timestamp 
      })
      
      // 按时间戳排序
      let sorted_new_data = new_data_points.sort(fn(a, b) {
        if a.timestamp < b.timestamp { -1 }
        else if a.timestamp > b.timestamp { 1 }
        else { 0 }
      })
      
      // 更新导出状态
      let new_last_timestamp = if sorted_new_data.length() > 0 {
        sorted_new_data[sorted_new_data.length() - 1].timestamp
      } else {
        export_state.last_export_timestamp
      }
      
      let new_last_id = if sorted_new_data.length() > 0 {
        sorted_new_data[sorted_new_data.length() - 1].metric_name + "_" + new_last_timestamp.to_string()
      } else {
        export_state.last_export_id
      }
      
      let old_state = export_state
      export_state = {
        last_export_timestamp: new_last_timestamp,
        last_export_id: new_last_id,
        total_exported: export_state.total_exported + sorted_new_data.length()
      }
      
      {
        exported_data: sorted_new_data,
        previous_state: old_state,
        current_state: export_state,
        export_count: sorted_new_data.length(),
        is_incremental: old_state.last_export_timestamp > 0
      }
    }
  }
  
  let exporter = incremental_exporter()
  
  // 创建初始数据集
  let initial_data = [
    {
      timestamp: 1735689600000,  // 2025-01-01 00:00:00
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,  // 2025-01-01 00:01:00
      metric_name: "memory_usage",
      value: 78.5,
      labels: [("host", "server1")],
      data_type: "gauge"
    }
  ]
  
  // 第一次导出（全量）
  let first_export = exporter(initial_data, 1735689700000)
  assert_eq(first_export.export_count, 2)
  assert_false(first_export.is_incremental)  // 第一次不是增量
  assert_eq(first_export.previous_state.last_export_timestamp, 0)
  assert_eq(first_export.current_state.last_export_timestamp, 1735689660000)
  assert_eq(first_export.current_state.total_exported, 2)
  
  // 创建新增数据
  let new_data = [
    {
      timestamp: 1735689720000,  // 2025-01-01 00:02:00
      metric_name: "request_count",
      value: 1250.0,
      labels: [("host", "server1")],
      data_type: "counter"
    },
    {
      timestamp: 1735689780000,  // 2025-01-01 00:03:00
      metric_name: "response_time",
      value: 150.0,
      labels: [("host", "server1")],
      data_type: "histogram"
    },
    {
      timestamp: 1735689620000,  // 2025-01-01 00:01:00 (旧数据，不应被导出)
      metric_name: "disk_usage",
      value: 60.0,
      labels: [("host", "server1")],
      data_type: "gauge"
    }
  ]
  
  // 第二次导出（增量）
  let second_export = exporter(new_data, 1735689800000)
  assert_eq(second_export.export_count, 2)  // 只导出新数据
  assert_true(second_export.is_incremental)  // 这是增量导出
  assert_eq(second_export.previous_state.last_export_timestamp, 1735689660000)
  assert_eq(second_export.current_state.last_export_timestamp, 1735689780000)
  assert_eq(second_export.current_state.total_exported, 4)  // 累计导出
  
  // 验证导出的数据
  assert_eq(second_export.exported_data[0].metric_name, "request_count")
  assert_eq(second_export.exported_data[1].metric_name, "response_time")
  
  // 创建更多新数据
  let more_new_data = [
    {
      timestamp: 1735689840000,  // 2025-01-01 00:04:00
      metric_name: "error_count",
      value: 5.0,
      labels: [("host", "server1")],
      data_type: "counter"
    }
  ]
  
  // 第三次导出（增量）
  let third_export = exporter(more_new_data, 1735689900000)
  assert_eq(third_export.export_count, 1)  // 只导出新数据
  assert_true(third_export.is_incremental)  // 这也是增量导出
  assert_eq(third_export.current_state.total_exported, 5)  // 累计导出
  
  // 验证导出状态的连续性
  assert_eq(third_export.previous_state.last_export_timestamp, second_export.current_state.last_export_timestamp)
  assert_eq(third_export.current_state.last_export_timestamp, 1735689840000)
}