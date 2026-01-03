// Azimuth 自定义遥测格式化器测试用例
// 专注于测试自定义输出格式和格式转换功能

// 测试1: 自定义JSON格式化器
test "自定义JSON格式化器" {
  // 定义遥测数据点
  type TelemetryDataPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    labels: Array<(String, String)>,
    data_type: String
  }
  
  // 定义JSON格式化配置
  type JSONFormatConfig = {
    pretty_print: Bool,
    include_timestamp: Bool,
    include_labels: Bool,
    custom_fields: Array<(String, String)>,
    field_order: Array<String>,
    date_format: String  // "iso", "unix", "custom"
  }
  
  // 创建自定义JSON格式化器
  let custom_json_formatter = fn(config: JSONFormatConfig) {
    fn(data_points: Array[TelemetryDataPoint>) -> String {
      let format_timestamp = fn(timestamp: Int) {
        match config.date_format {
          "iso" => "2025-01-01T00:00:00Z"  // 简化的ISO格式
          "unix" => timestamp.to_string()
          "custom" => "2025-01-01 00:00:00"  // 简化的自定义格式
          _ => timestamp.to_string()
        }
      }
      
      let format_data_point = fn(dp: TelemetryDataPoint) {
        let mut fields = Array::empty()
        
        // 按指定顺序添加字段
        for field_name in config.field_order {
          match field_name {
            "timestamp" => {
              if config.include_timestamp {
                fields = fields.push("\"timestamp\": \"" + format_timestamp(dp.timestamp) + "\"")
              }
            }
            "metric_name" => {
              fields = fields.push("\"metric_name\": \"" + dp.metric_name + "\"")
            }
            "value" => {
              fields = fields.push("\"value\": " + dp.value.to_string())
            }
            "data_type" => {
              fields = fields.push("\"data_type\": \"" + dp.data_type + "\"")
            }
            "labels" => {
              if config.include_labels {
                let labels_str = dp.labels.map(fn(l) {
                  "\"" + l.0 + "\": \"" + l.1 + "\""
                }).join(",")
                fields = fields.push("\"labels\": {" + labels_str + "}")
              }
            }
            _ => ()
          }
        }
        
        // 添加自定义字段
        for (field_name, field_value) in config.custom_fields {
          fields = fields.push("\"" + field_name + "\": \"" + field_value + "\"")
        }
        
        let json_str = "{" + fields.join(",") + "}"
        
        if config.pretty_print {
          // 简化的美化打印
          json_str
            .replace("{", "{\n  ")
            .replace("}", "\n}")
            .replace(",", ",\n  ")
        } else {
          json_str
        }
      }
      
      let formatted_points = data_points.map(format_data_point)
      
      if data_points.length() == 1 {
        formatted_points[0]
      } else {
        "[" + formatted_points.join(",") + "]"
      }
    }
  }
  
  // 创建测试数据
  let test_data = [
    {
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,
      metric_name: "memory_usage",
      value: 78.5,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge"
    }
  ]
  
  // 测试紧凑JSON格式
  let compact_config = JSONFormatConfig({
    pretty_print: false,
    include_timestamp: true,
    include_labels: true,
    custom_fields: [],
    field_order: ["timestamp", "metric_name", "value", "data_type", "labels"],
    date_format: "unix"
  })
  
  let compact_formatter = custom_json_formatter(compact_config)
  let compact_output = compact_formatter(test_data)
  
  assert_true(compact_output.contains("\"timestamp\": \"1735689600000\""))
  assert_true(compact_output.contains("\"metric_name\": \"cpu_usage\""))
  assert_true(compact_output.contains("\"value\": 45.2"))
  assert_true(compact_output.contains("\"host\": \"server1\""))
  assert_false(compact_output.contains("\n"))  // 不应该有换行符
  
  // 测试美化JSON格式
  let pretty_config = JSONFormatConfig({
    pretty_print: true,
    include_timestamp: true,
    include_labels: false,
    custom_fields: [("source", "telemetry-collector"), ("version", "1.2.3")],
    field_order: ["metric_name", "value", "data_type", "timestamp"],
    date_format: "iso"
  })
  
  let pretty_formatter = custom_json_formatter(pretty_config)
  let pretty_output = pretty_formatter([test_data[0]])  // 只格式化第一个数据点
  
  assert_true(pretty_output.contains("\"metric_name\": \"cpu_usage\""))
  assert_true(pretty_output.contains("\"value\": 45.2"))
  assert_true(pretty_output.contains("\"source\": \"telemetry-collector\""))
  assert_true(pretty_output.contains("\"version\": \"1.2.3\""))
  assert_false(pretty_output.contains("\"labels\""))  // 不包含标签
  assert_true(pretty_output.contains("\n"))  // 应该有换行符
  
  // 测试自定义字段顺序
  let custom_order_config = JSONFormatConfig({
    pretty_print: false,
    include_timestamp: true,
    include_labels: true,
    custom_fields: [],
    field_order: ["data_type", "metric_name", "value", "timestamp", "labels"],
    date_format: "custom"
  })
  
  let custom_order_formatter = custom_json_formatter(custom_order_config)
  let custom_order_output = custom_order_formatter([test_data[0]])
  
  // 验证字段顺序
  let data_type_pos = custom_order_output.index_of("\"data_type\":")
  let metric_name_pos = custom_order_output.index_of("\"metric_name\":")
  let value_pos = custom_order_output.index_of("\"value\":")
  let timestamp_pos = custom_order_output.index_of("\"timestamp\":")
  
  assert_true(data_type_pos < metric_name_pos)
  assert_true(metric_name_pos < value_pos)
  assert_true(value_pos < timestamp_pos)
}

// 测试2: 自定义CSV格式化器
test "自定义CSV格式化器" {
  // 定义CSV格式化配置
  type CSVFormatConfig = {
    delimiter: String,
    include_header: Bool,
    include_timestamp: Bool,
    include_labels: Bool,
    label_columns: Array<String>,
    date_format: String,
    float_precision: Int,
    quote_fields: Bool
  }
  
  // 创建自定义CSV格式化器
  let custom_csv_formatter = fn(config: CSVFormatConfig) {
    fn(data_points: Array[TelemetryDataPoint>) -> String {
      let format_timestamp = fn(timestamp: Int) {
        match config.date_format {
          "iso" => "2025-01-01T00:00:00Z"  // 简化的ISO格式
          "unix" => timestamp.to_string()
          "readable" => "2025-01-01 00:00:00"  // 简化的可读格式
          _ => timestamp.to_string()
        }
      }
      
      let format_value = fn(value: Float) {
        if config.float_precision >= 0 {
          let multiplier = 10.0_f64.pow(config.float_precision as Float)
          (value * multiplier).round() / multiplier
        } else {
          value
        }
      }
      
      let escape_field = fn(field: String) {
        if config.quote_fields || field.contains(config.delimiter) || field.contains("\"") || field.contains("\n") {
          "\"" + field.replace("\"", "\"\"") + "\""
        } else {
          field
        }
      }
      
      // 构建表头
      let mut header_fields = Array::empty()
      
      if config.include_timestamp {
        header_fields = header_fields.push("timestamp")
      }
      
      header_fields = header_fields.concat(["metric_name", "value", "data_type"])
      
      if config.include_labels {
        for label_name in config.label_columns {
          header_fields = header_fields.push("label_" + label_name)
        }
      }
      
      let header = header_fields.map(escape_field).join(config.delimiter)
      
      // 格式化数据行
      let data_rows = data_points.map(fn(dp) {
        let mut row_fields = Array::empty()
        
        if config.include_timestamp {
          row_fields = row_fields.push(format_timestamp(dp.timestamp))
        }
        
        row_fields = row_fields.concat([
          dp.metric_name,
          format_value(dp.value).to_string(),
          dp.data_type
        ])
        
        if config.include_labels {
          for label_name in config.label_columns {
            let label_value = match dp.labels.find(fn(l) { l.0 == label_name }) {
              Some((_, value)) => value
              None => ""
            }
            row_fields = row_fields.push(label_value)
          }
        }
        
        row_fields.map(escape_field).join(config.delimiter)
      })
      
      // 组合表头和数据行
      if config.include_header {
        header + "\n" + data_rows.join("\n")
      } else {
        data_rows.join("\n")
      }
    }
  }
  
  // 创建测试数据
  let test_data = [
    {
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 45.234567,
      labels: [("host", "server1"), ("region", "us-west"), ("env", "prod")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,
      metric_name: "memory_usage",
      value: 78.891234,
      labels: [("host", "server1"), ("region", "us-west"), ("env", "prod")],
      data_type: "gauge"
    }
  ]
  
  // 测试标准CSV格式
  let standard_config = CSVFormatConfig({
    delimiter: ",",
    include_header: true,
    include_timestamp: true,
    include_labels: true,
    label_columns: ["host", "region", "env"],
    date_format: "unix",
    float_precision: 2,
    quote_fields: false
  })
  
  let standard_formatter = custom_csv_formatter(standard_config)
  let standard_output = standard_formatter(test_data)
  
  assert_true(standard_output.contains("timestamp,metric_name,value,data_type,label_host,label_region,label_env"))
  assert_true(standard_output.contains("1735689600000,cpu_usage,45.23,gauge,server1,us-west,prod"))
  assert_true(standard_output.contains("1735689660000,memory_usage,78.89,gauge,server1,us-west,prod"))
  
  // 测试分号分隔符
  let semicolon_config = CSVFormatConfig({
    delimiter: ";",
    include_header: true,
    include_timestamp: false,
    include_labels: false,
    label_columns: [],
    date_format: "iso",
    float_precision: 3,
    quote_fields: true
  })
  
  let semicolon_formatter = custom_csv_formatter(semicolon_config)
  let semicolon_output = semicolon_formatter(test_data)
  
  assert_true(semicolon_output.contains("metric_name;value;data_type"))
  assert_true(semicolon_output.contains("\"cpu_usage\";\"45.235\";\"gauge\""))
  assert_true(semicolon_output.contains("\"memory_usage\";\"78.891\";\"gauge\""))
  assert_false(semicolon_output.contains("timestamp"))  // 不包含时间戳
  assert_false(semicolon_output.contains("server1"))   // 不包含标签
  
  // 测试无表头格式
  let no_header_config = CSVFormatConfig({
    delimiter: "\t",  // 制表符分隔
    include_header: false,
    include_timestamp: true,
    include_labels: true,
    label_columns: ["host", "env"],
    date_format: "readable",
    float_precision: 1,
    quote_fields: false
  })
  
  let no_header_formatter = custom_csv_formatter(no_header_config)
  let no_header_output = no_header_formatter(test_data)
  
  assert_false(no_header_output.contains("metric_name"))  // 无表头
  assert_true(no_header_output.contains("2025-01-01 00:00:00\tcpu_usage\t45.2\tgauge\tserver1\tprod"))
  assert_true(no_header_output.contains("server1"))        // 包含host标签
  assert_true(no_header_output.contains("prod"))          // 包含env标签
  assert_false(no_header_output.contains("us-west"))     // 不包含region标签
}

// 测试3: 自定义Prometheus格式化器
test "自定义Prometheus格式化器" {
  // 定义Prometheus格式化配置
  type PrometheusFormatConfig = {
    metric_prefix: String,
    include_help: Bool,
    include_type: Bool,
    label_handling: String,  // "include", "exclude", "custom"
    custom_labels: Array<(String, String)>,
    timestamp_format: String,  // "seconds", "milliseconds"
    value_format: String      // "float", "integer"
  }
  
  // 创建自定义Prometheus格式化器
  let custom_prometheus_formatter = fn(config: PrometheusFormatConfig) {
    fn(data_points: Array[TelemetryDataPoint>) -> String {
      let format_timestamp = fn(timestamp: Int) {
        match config.timestamp_format {
          "seconds" => (timestamp / 1000).to_string()
          "milliseconds" => timestamp.to_string()
          _ => (timestamp / 1000).to_string()
        }
      }
      
      let format_value = fn(value: Float) {
        match config.value_format {
          "integer" => (value as Int).to_string()
          "float" => value.to_string()
          _ => value.to_string()
        }
      }
      
      // 按指标名称分组
      let grouped_metrics = data_points.group_by(fn(dp) { dp.metric_name })
      
      let mut output_lines = Array::empty()
      
      // 生成每个指标的输出
      for (metric_name, points) in grouped_metrics {
        let full_metric_name = if config.metric_prefix.length() > 0 {
          config.metric_prefix + "_" + metric_name
        } else {
          metric_name
        }
        
        // 添加HELP注释
        if config.include_help {
          output_lines = output_lines.push("# HELP " + full_metric_name + " " + metric_name + " metric")
        }
        
        // 添加TYPE注释
        if config.include_type {
          let metric_type = match points[0].data_type {
            "counter" => "counter"
            "gauge" => "gauge"
            "histogram" => "histogram"
            "summary" => "summary"
            _ => "untyped"
          }
          output_lines = output_lines.push("# TYPE " + full_metric_name + " " + metric_type)
        }
        
        // 生成指标数据行
        for point in points {
          let mut labels = Array::empty()
          
          // 处理标签
          match config.label_handling {
            "include" => {
              labels = labels.concat(point.labels)
            }
            "exclude" => {
              // 不包含原始标签
            }
            "custom" => {
              // 只包含自定义标签
              labels = labels.concat(config.custom_labels)
            }
            _ => {
              labels = labels.concat(point.labels)
            }
          }
          
          // 添加自定义标签
          for (label_name, label_value) in config.custom_labels {
            if not labels.any(fn(l) { l.0 == label_name }) {
              labels = labels.push((label_name, label_value))
            }
          }
          
          // 格式化标签
          let labels_str = if labels.length() > 0 {
            "{" + labels.map(fn(l) { l.0 + "=\"" + l.1 + "\"" }).join(",") + "}"
          } else {
            ""
          }
          
          // 格式化指标行
          let metric_line = full_metric_name + labels_str + " " + 
                           format_value(point.value) + " " + 
                           format_timestamp(point.timestamp)
          
          output_lines = output_lines.push(metric_line)
        }
      }
      
      output_lines.join("\n")
    }
  }
  
  // 创建测试数据
  let test_data = [
    {
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,
      metric_name: "cpu_usage",
      value: 50.7,
      labels: [("host", "server2"), ("region", "us-east")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689720000,
      metric_name: "request_count",
      value: 1250.0,
      labels: [("endpoint", "/api/users"), ("method", "GET")],
      data_type: "counter"
    }
  ]
  
  // 测试标准Prometheus格式
  let standard_config = PrometheusFormatConfig({
    metric_prefix: "",
    include_help: true,
    include_type: true,
    label_handling: "include",
    custom_labels: [],
    timestamp_format: "milliseconds",
    value_format: "float"
  })
  
  let standard_formatter = custom_prometheus_formatter(standard_config)
  let standard_output = standard_formatter(test_data)
  
  assert_true(standard_output.contains("# HELP cpu_usage cpu_usage metric"))
  assert_true(standard_output.contains("# TYPE cpu_usage gauge"))
  assert_true(standard_output.contains("cpu_usage{host=\"server1\",region=\"us-west\"} 45.2 1735689600000"))
  assert_true(standard_output.contains("cpu_usage{host=\"server2\",region=\"us-east\"} 50.7 1735689660000"))
  assert_true(standard_output.contains("# HELP request_count request_count metric"))
  assert_true(standard_output.contains("# TYPE request_count counter"))
  
  // 测试带前缀和自定义标签的格式
  let custom_config = PrometheusFormatConfig({
    metric_prefix: "azimuth",
    include_help: true,
    include_type: true,
    label_handling: "custom",
    custom_labels: [("service", "telemetry"), ("env", "production")],
    timestamp_format: "seconds",
    value_format: "integer"
  })
  
  let custom_formatter = custom_prometheus_formatter(custom_config)
  let custom_output = custom_formatter(test_data)
  
  assert_true(custom_output.contains("# HELP azimuth_cpu_usage cpu_usage metric"))
  assert_true(custom_output.contains("# TYPE azimuth_cpu_usage gauge"))
  assert_true(custom_output.contains("azimuth_cpu_usage{service=\"telemetry\",env=\"production\"} 45 1735689600"))
  assert_true(custom_output.contains("azimuth_cpu_usage{service=\"telemetry\",env=\"production\"} 51 1735689660"))  // 50.7四舍五入为51
  assert_true(custom_output.contains("azimuth_request_count{service=\"telemetry\",env=\"production\"} 1250 1735689720"))
  assert_false(custom_output.contains("host=\"server1\""))  // 不包含原始标签
  
  // 测试简化格式（无注释）
  let simple_config = PrometheusFormatConfig({
    metric_prefix: "app",
    include_help: false,
    include_type: false,
    label_handling: "include",
    custom_labels: [],
    timestamp_format: "seconds",
    value_format: "float"
  })
  
  let simple_formatter = custom_prometheus_formatter(simple_config)
  let simple_output = simple_formatter(test_data)
  
  assert_false(simple_output.contains("# HELP"))  // 无HELP注释
  assert_false(simple_output.contains("# TYPE"))  // 无TYPE注释
  assert_true(simple_output.contains("app_cpu_usage{host=\"server1\",region=\"us-west\"} 45.2 1735689600"))
  assert_true(simple_output.contains("app_cpu_usage{host=\"server2\",region=\"us-east\"} 50.7 1735689660"))
}

// 测试4: 自定义XML格式化器
test "自定义XML格式化器" {
  // 定义XML格式化配置
  type XMLFormatConfig = {
    root_element: String,
    item_element: String,
    include_attributes: Bool,
    include_timestamp: Bool,
    include_labels: Bool,
    label_format: String,  // "attribute", "subelement", "mixed"
    date_format: String,
    pretty_print: Bool,
    indent_size: Int
  }
  
  // 创建自定义XML格式化器
  let custom_xml_formatter = fn(config: XMLFormatConfig) {
    fn(data_points: Array[TelemetryDataPoint>) -> String {
      let format_timestamp = fn(timestamp: Int) {
        match config.date_format {
          "iso" => "2025-01-01T00:00:00Z"  // 简化的ISO格式
          "unix" => timestamp.to_string()
          "readable" => "2025-01-01 00:00:00"  // 简化的可读格式
          _ => timestamp.to_string()
        }
      }
      
      let escape_xml = fn(text: String) {
        text
          .replace("&", "&amp;")
          .replace("<", "&lt;")
          .replace(">", "&gt;")
          .replace("\"", "&quot;")
          .replace("'", "&apos;")
      }
      
      let indent = if config.pretty_print {
        " ".repeat(config.indent_size)
      } else {
        ""
      }
      
      let newline = if config.pretty_print { "\n" } else { "" }
      
      // 格式化单个数据点
      let format_data_point = fn(dp: TelemetryDataPoint) {
        let mut attributes = Array::empty()
        let mut subelements = Array::empty()
        
        // 处理时间戳
        if config.include_timestamp {
          if config.include_attributes {
            attributes = attributes.push(("timestamp", format_timestamp(dp.timestamp)))
          } else {
            subelements = subelements.push(indent + indent + "<timestamp>" + escape_xml(format_timestamp(dp.timestamp)) + "</timestamp>")
          }
        }
        
        // 处理指标名称
        if config.include_attributes {
          attributes = attributes.push(("name", escape_xml(dp.metric_name)))
        } else {
          subelements = subelements.push(indent + indent + "<name>" + escape_xml(dp.metric_name) + "</name>")
        }
        
        // 处理值
        if config.include_attributes {
          attributes = attributes.push(("value", dp.value.to_string()))
        } else {
          subelements = subelements.push(indent + indent + "<value>" + dp.value.to_string() + "</value>")
        }
        
        // 处理数据类型
        if config.include_attributes {
          attributes = attributes.push(("type", escape_xml(dp.data_type)))
        } else {
          subelements = subelements.push(indent + indent + "<type>" + escape_xml(dp.data_type) + "</type>")
        }
        
        // 处理标签
        if config.include_labels {
          match config.label_format {
            "attribute" => {
              for label in dp.labels {
                attributes = attributes.push(("label_" + label.0, escape_xml(label.1)))
              }
            }
            "subelement" => {
              let label_elements = dp.labels.map(fn(l) {
                indent + indent + "<label name=\"" + escape_xml(l.0) + "\">" + escape_xml(l.1) + "</label>"
              })
              subelements = subelements.concat(label_elements)
            }
            "mixed" => {
              // 混合模式：部分作为属性，部分作为子元素
              for label in dp.labels {
                if label.0 == "host" || label.0 == "region" {
                  attributes = attributes.push(("label_" + label.0, escape_xml(label.1)))
                } else {
                  subelements = subelements.push(indent + indent + "<label name=\"" + escape_xml(l.0) + "\">" + escape_xml(l.1) + "</label>")
                }
              }
            }
            _ => ()
          }
        }
        
        // 构建属性字符串
        let attributes_str = if attributes.length() > 0 {
          " " + attributes.map(fn(a) { a.0 + "=\"" + a.1 + "\"" }).join(" ")
        } else {
          ""
        }
        
        // 构建子元素字符串
        let subelements_str = if subelements.length() > 0 {
          newline + subelements.join(newline) + newline + indent
        } else {
          ""
        }
        
        // 构建完整的元素
        indent + "<" + config.item_element + attributes_str + ">" + 
        subelements_str + 
        "</" + config.item_element + ">"
      }
      
      // 格式化所有数据点
      let formatted_points = data_points.map(format_data_point)
      
      // 构建完整的XML文档
      let xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + newline
      let opening_tag = "<" + config.root_element + ">" + newline
      let closing_tag = "</" + config.root_element + ">"
      
      xml_header + opening_tag + formatted_points.join(newline) + newline + closing_tag
    }
  }
  
  // 创建测试数据
  let test_data = [
    {
      timestamp: 1735689600000,
      metric_name: "cpu_usage",
      value: 45.2,
      labels: [("host", "server1"), ("region", "us-west"), ("env", "prod")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,
      metric_name: "memory_usage",
      value: 78.5,
      labels: [("host", "server2"), ("region", "us-east"), ("env", "prod")],
      data_type: "gauge"
    }
  ]
  
  // 测试属性格式XML
  let attribute_config = XMLFormatConfig({
    root_element: "telemetry",
    item_element: "metric",
    include_attributes: true,
    include_timestamp: true,
    include_labels: true,
    label_format: "attribute",
    date_format: "iso",
    pretty_print: true,
    indent_size: 2
  })
  
  let attribute_formatter = custom_xml_formatter(attribute_config)
  let attribute_output = attribute_formatter(test_data)
  
  assert_true(attribute_output.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
  assert_true(attribute_output.contains("<telemetry>"))
  assert_true(attribute_output.contains("<metric timestamp=\"2025-01-01T00:00:00Z\" name=\"cpu_usage\" value=\"45.2\" type=\"gauge\" label_host=\"server1\" label_region=\"us-west\" label_env=\"prod\" />"))
  assert_true(attribute_output.contains("<metric timestamp=\"2025-01-01T00:01:00Z\" name=\"memory_usage\" value=\"78.5\" type=\"gauge\" label_host=\"server2\" label_region=\"us-east\" label_env=\"prod\" />"))
  
  // 测试子元素格式XML
  let subelement_config = XMLFormatConfig({
    root_element: "metrics",
    item_element: "data",
    include_attributes: false,
    include_timestamp: true,
    include_labels: true,
    label_format: "subelement",
    date_format: "unix",
    pretty_print: true,
    indent_size: 4
  })
  
  let subelement_formatter = custom_xml_formatter(subelement_config)
  let subelement_output = subelement_formatter(test_data)
  
  assert_true(subelement_output.contains("<metrics>"))
  assert_true(subelement_output.contains("<data>"))
  assert_true(subelement_output.contains("<timestamp>1735689600000</timestamp>"))
  assert_true(subelement_output.contains("<name>cpu_usage</name>"))
  assert_true(subelement_output.contains("<value>45.2</value>"))
  assert_true(subelement_output.contains("<label name=\"host\">server1</label>"))
  assert_true(subelement_output.contains("<label name=\"region\">us-west</label>"))
  
  // 测试紧凑格式XML
  let compact_config = XMLFormatConfig({
    root_element: "data",
    item_element: "m",
    include_attributes: true,
    include_timestamp: false,
    include_labels: false,
    label_format: "attribute",
    date_format: "unix",
    pretty_print: false,
    indent_size: 0
  })
  
  let compact_formatter = custom_xml_formatter(compact_config)
  let compact_output = compact_formatter(test_data)
  
  assert_true(compact_output.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
  assert_true(compact_output.contains("<data>"))
  assert_true(compact_output.contains("<m name=\"cpu_usage\" value=\"45.2\" type=\"gauge\"/>"))
  assert_true(compact_output.contains("<m name=\"memory_usage\" value=\"78.5\" type=\"gauge\"/>"))
  assert_false(compact_output.contains("\n"))  // 无换行符
  assert_false(compact_output.contains("timestamp"))  // 不包含时间戳
  assert_false(compact_output.contains("host"))  // 不包含标签
}

// 测试5: 自定义二进制格式化器
test "自定义二进制格式化器" {
  // 定义二进制格式化配置
  type BinaryFormatConfig = {
    version: Int,
    include_header: Bool,
    compression: Bool,
    endianness: String,  // "little", "big"
    value_encoding: String,  // "float32", "float64", "fixed_point"
    timestamp_encoding: String,  // "unix32", "unix64", "custom"
    string_encoding: String  // "utf8", "ascii", "length_prefixed"
  }
  
  // 创建自定义二进制格式化器
  let custom_binary_formatter = fn(config: BinaryFormatConfig) {
    fn(data_points: Array[TelemetryDataPoint>) -> Array[Byte> {
      let mut output = Array::empty()
      
      // 添加文件头
      if config.include_header {
        // 魔数 (4字节)
        output = output.concat([0x41, 0x5A, 0x49, 0x4D])  // "AZIM"
        
        // 版本 (2字节)
        output = output.concat([
          (config.version / 256) as Byte,
          (config.version % 256) as Byte
        ])
        
        // 数据点数量 (4字节)
        let count = data_points.length()
        output = output.concat([
          (count / 16777216) as Byte,
          ((count / 65536) % 256) as Byte,
          ((count / 256) % 256) as Byte,
          (count % 256) as Byte
        ])
        
        // 标志位 (1字节)
        let flags = if config.compression { 0x01 } else { 0x00 }
        output = output.push(flags)
        
        // 保留字节 (3字节)
        output = output.concat([0x00, 0x00, 0x00])
      }
      
      // 格式化数据点
      for dp in data_points {
        // 时间戳 (8或4字节)
        match config.timestamp_encoding {
          "unix64" => {
            let timestamp = dp.timestamp
            if config.endianness == "little" {
              output = output.concat([
                (timestamp % 256) as Byte,
                ((timestamp / 256) % 256) as Byte,
                ((timestamp / 65536) % 256) as Byte,
                ((timestamp / 16777216) % 256) as Byte,
                ((timestamp / 4294967296) % 256) as Byte,
                ((timestamp / 1099511627776) % 256) as Byte,
                ((timestamp / 281474976710656) % 256) as Byte,
                ((timestamp / 72057594037927936) % 256) as Byte
              ])
            } else {
              output = output.concat([
                ((timestamp / 72057594037927936) % 256) as Byte,
                ((timestamp / 281474976710656) % 256) as Byte,
                ((timestamp / 1099511627776) % 256) as Byte,
                ((timestamp / 4294967296) % 256) as Byte,
                ((timestamp / 16777216) % 256) as Byte,
                ((timestamp / 65536) % 256) as Byte,
                ((timestamp / 256) % 256) as Byte,
                (timestamp % 256) as Byte
              ])
            }
          }
          "unix32" => {
            let timestamp = (dp.timestamp / 1000) as Int  // 转换为秒
            if config.endianness == "little" {
              output = output.concat([
                (timestamp % 256) as Byte,
                ((timestamp / 256) % 256) as Byte,
                ((timestamp / 65536) % 256) as Byte,
                ((timestamp / 16777216) % 256) as Byte
              ])
            } else {
              output = output.concat([
                ((timestamp / 16777216) % 256) as Byte,
                ((timestamp / 65536) % 256) as Byte,
                ((timestamp / 256) % 256) as Byte,
                (timestamp % 256) as Byte
              ])
            }
          }
          _ => {
            // 默认使用64位Unix时间戳
            let timestamp = dp.timestamp
            output = output.concat([
              (timestamp % 256) as Byte,
              ((timestamp / 256) % 256) as Byte,
              ((timestamp / 65536) % 256) as Byte,
              ((timestamp / 16777216) % 256) as Byte,
              ((timestamp / 4294967296) % 256) as Byte,
              ((timestamp / 1099511627776) % 256) as Byte,
              ((timestamp / 281474976710656) % 256) as Byte,
              ((timestamp / 72057594037927936) % 256) as Byte
            ])
          }
        }
        
        // 指标名称长度 (2字节)
        let name_length = dp.metric_name.length()
        output = output.concat([
          (name_length / 256) as Byte,
          (name_length % 256) as Byte
        ])
        
        // 指标名称 (变长)
        match config.string_encoding {
          "utf8" => {
            for char in dp.metric_name.chars() {
              output = output.push(char as Byte)
            }
          }
          "ascii" => {
            for char in dp.metric_name.chars() {
              output = output.push((char % 128) as Byte)  // 限制为ASCII
            }
          }
          _ => {
            // 默认UTF-8
            for char in dp.metric_name.chars() {
              output = output.push(char as Byte)
            }
          }
        }
        
        // 值 (4或8字节)
        match config.value_encoding {
          "float32" => {
            let value = dp.value as Float32
            let bits = value.to_bits() as Int
            if config.endianness == "little" {
              output = output.concat([
                (bits % 256) as Byte,
                ((bits / 256) % 256) as Byte,
                ((bits / 65536) % 256) as Byte,
                ((bits / 16777216) % 256) as Byte
              ])
            } else {
              output = output.concat([
                ((bits / 16777216) % 256) as Byte,
                ((bits / 65536) % 256) as Byte,
                ((bits / 256) % 256) as Byte,
                (bits % 256) as Byte
              ])
            }
          }
          "fixed_point" => {
            // 简化的定点数表示 (整数部分2字节，小数部分2字节)
            let int_part = dp.value as Int
            let frac_part = ((dp.value - (int_part as Float)) * 10000.0) as Int
            
            if config.endianness == "little" {
              output = output.concat([
                (int_part % 256) as Byte,
                ((int_part / 256) % 256) as Byte,
                (frac_part % 256) as Byte,
                ((frac_part / 256) % 256) as Byte
              ])
            } else {
              output = output.concat([
                ((int_part / 256) % 256) as Byte,
                (int_part % 256) as Byte,
                ((frac_part / 256) % 256) as Byte,
                (frac_part % 256) as Byte
              ])
            }
          }
          _ => {
            // 默认使用64位浮点数
            let bits = dp.value.to_bits() as Int
            if config.endianness == "little" {
              output = output.concat([
                (bits % 256) as Byte,
                ((bits / 256) % 256) as Byte,
                ((bits / 65536) % 256) as Byte,
                ((bits / 16777216) % 256) as Byte,
                ((bits / 4294967296) % 256) as Byte,
                ((bits / 1099511627776) % 256) as Byte,
                ((bits / 281474976710656) % 256) as Byte,
                ((bits / 72057594037927936) % 256) as Byte
              ])
            } else {
              output = output.concat([
                ((bits / 72057594037927936) % 256) as Byte,
                ((bits / 281474976710656) % 256) as Byte,
                ((bits / 1099511627776) % 256) as Byte,
                ((bits / 4294967296) % 256) as Byte,
                ((bits / 16777216) % 256) as Byte,
                ((bits / 65536) % 256) as Byte,
                ((bits / 256) % 256) as Byte,
                (bits % 256) as Byte
              ])
            }
          }
        }
        
        // 数据类型 (1字节)
        let type_byte = match dp.data_type {
          "counter" => 0x01
          "gauge" => 0x02
          "histogram" => 0x03
          "summary" => 0x04
          _ => 0x00
        }
        output = output.push(type_byte)
        
        // 标签数量 (2字节)
        let label_count = dp.labels.length()
        output = output.concat([
          (label_count / 256) as Byte,
          (label_count % 256) as Byte
        ])
        
        // 标签 (变长)
        for label in dp.labels {
          // 标签名称长度 (2字节)
          let label_name_length = label.0.length()
          output = output.concat([
            (label_name_length / 256) as Byte,
            (label_name_length % 256) as Byte
          ])
          
          // 标签名称
          for char in label.0.chars() {
            output = output.push(char as Byte)
          }
          
          // 标签值长度 (2字节)
          let label_value_length = label.1.length()
          output = output.concat([
            (label_value_length / 256) as Byte,
            (label_value_length % 256) as Byte
          ])
          
          // 标签值
          for char in label.1.chars() {
            output = output.push(char as Byte)
          }
        }
      }
      
      // 模拟压缩（简化：只是返回前一半的数据）
      if config.compression {
        output.slice(0, output.length() / 2)
      } else {
        output
      }
    }
  }
  
  // 创建测试数据
  let test_data = [
    {
      timestamp: 1735689600000,
      metric_name: "cpu",
      value: 45.5,
      labels: [("host", "server1"), ("region", "us-west")],
      data_type: "gauge"
    },
    {
      timestamp: 1735689660000,
      metric_name: "mem",
      value: 78.25,
      labels: [("host", "server2"), ("region", "us-east")],
      data_type: "counter"
    }
  ]
  
  // 测试标准二进制格式
  let standard_config = BinaryFormatConfig({
    version: 1,
    include_header: true,
    compression: false,
    endianness: "little",
    value_encoding: "float64",
    timestamp_encoding: "unix64",
    string_encoding: "utf8"
  })
  
  let standard_formatter = custom_binary_formatter(standard_config)
  let standard_output = standard_formatter(test_data)
  
  // 验证文件头
  assert_eq(standard_output[0], 0x41)  // 'A'
  assert_eq(standard_output[1], 0x5A)  // 'Z'
  assert_eq(standard_output[2], 0x49)  // 'I'
  assert_eq(standard_output[3], 0x4D)  // 'M'
  assert_eq(standard_output[4], 0x00)  // 版本1
  assert_eq(standard_output[5], 0x01)
  assert_eq(standard_output[6], 0x00)  // 数据点数量2
  assert_eq(standard_output[7], 0x00)
  assert_eq(standard_output[8], 0x00)
  assert_eq(standard_output[9], 0x02)
  
  // 测试压缩格式
  let compressed_config = BinaryFormatConfig({
    version: 1,
    include_header: true,
    compression: true,
    endianness: "little",
    value_encoding: "float32",
    timestamp_encoding: "unix32",
    string_encoding: "ascii"
  })
  
  let compressed_formatter = custom_binary_formatter(compressed_config)
  let compressed_output = compressed_formatter(test_data)
  
  // 验证压缩标志
  assert_eq(compressed_output[10], 0x01)  // 压缩标志位
  
  // 验证压缩后的数据大小
  assert_true(compressed_output.length() < standard_output.length())
  
  // 测试无头格式
  let no_header_config = BinaryFormatConfig({
    version: 1,
    include_header: false,
    compression: false,
    endianness: "big",
    value_encoding: "fixed_point",
    timestamp_encoding: "unix64",
    string_encoding: "utf8"
  })
  
  let no_header_formatter = custom_binary_formatter(no_header_config)
  let no_header_output = no_header_formatter(test_data)
  
  // 验证无文件头
  assert_true(no_header_output.length() < standard_output.length())
  assert_ne(no_header_output[0], 0x41)  // 不是魔数开头
}
