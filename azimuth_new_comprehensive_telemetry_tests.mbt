// Azimuth 新增综合遥测测试用例
// 专注于遥测系统的高级功能和特性

// 测试1: 遥测数据聚合分析
test "遥测数据多维度聚合分析" {
  // 模拟多维度遥测数据
  let telemetry_data = [
    { service: "api-gateway", region: "us-east-1", metric: "latency", value: 120.0, timestamp: 1640995200 },
    { service: "api-gateway", region: "us-east-1", metric: "throughput", value: 1500.0, timestamp: 1640995201 },
    { service: "api-gateway", region: "us-west-2", metric: "latency", value: 95.0, timestamp: 1640995202 },
    { service: "api-gateway", region: "us-west-2", metric: "throughput", value: 1800.0, timestamp: 1640995203 },
    { service: "auth-service", region: "us-east-1", metric: "latency", value: 85.0, timestamp: 1640995204 },
    { service: "auth-service", region: "us-east-1", metric: "throughput", value: 800.0, timestamp: 1640995205 },
    { service: "auth-service", region: "us-west-2", metric: "latency", value: 75.0, timestamp: 1640995206 },
    { service: "auth-service", region: "us-west-2", metric: "throughput", value: 900.0, timestamp: 1640995207 }
  ]
  
  // 按服务维度聚合
  let mut service_aggregates = []
  let services = ["api-gateway", "auth-service"]
  
  for service in services {
    let mut service_data = []
    for data in telemetry_data {
      if data.service == service {
        service_data = service_data.push(data)
      }
    }
    
    // 计算服务级别的延迟平均值
    let mut latency_sum = 0.0
    let mut latency_count = 0
    let mut throughput_sum = 0.0
    let mut throughput_count = 0
    
    for data in service_data {
      if data.metric == "latency" {
        latency_sum = latency_sum + data.value
        latency_count = latency_count + 1
      } else if data.metric == "throughput" {
        throughput_sum = throughput_sum + data.value
        throughput_count = throughput_count + 1
      }
    }
    
    let avg_latency = if latency_count > 0 { latency_sum / latency_count.to_float() } else { 0.0 }
    let avg_throughput = if throughput_count > 0 { throughput_sum / throughput_count.to_float() } else { 0.0 }
    
    service_aggregates = service_aggregates.push({
      service: service,
      avg_latency: avg_latency,
      avg_throughput: avg_throughput
    })
  }
  
  // 验证服务维度聚合结果
  assert_eq(service_aggregates.length(), 2)
  assert_eq(service_aggregates[0].service, "api-gateway")
  assert_eq(service_aggregates[0].avg_latency, 107.5) // (120 + 95) / 2
  assert_eq(service_aggregates[0].avg_throughput, 1650.0) // (1500 + 1800) / 2
  assert_eq(service_aggregates[1].service, "auth-service")
  assert_eq(service_aggregates[1].avg_latency, 80.0) // (85 + 75) / 2
  assert_eq(service_aggregates[1].avg_throughput, 850.0) // (800 + 900) / 2
  
  // 按区域维度聚合
  let mut region_aggregates = []
  let regions = ["us-east-1", "us-west-2"]
  
  for region in regions {
    let mut region_data = []
    for data in telemetry_data {
      if data.region == region {
        region_data = region_data.push(data)
      }
    }
    
    // 计算区域级别的延迟平均值
    let mut latency_sum = 0.0
    let mut latency_count = 0
    
    for data in region_data {
      if data.metric == "latency" {
        latency_sum = latency_sum + data.value
        latency_count = latency_count + 1
      }
    }
    
    let avg_latency = if latency_count > 0 { latency_sum / latency_count.to_float() } else { 0.0 }
    
    region_aggregates = region_aggregates.push({
      region: region,
      avg_latency: avg_latency
    })
  }
  
  // 验证区域维度聚合结果
  assert_eq(region_aggregates.length(), 2)
  assert_eq(region_aggregates[0].region, "us-east-1")
  assert_eq(region_aggregates[0].avg_latency, 102.5) // (120 + 85) / 2
  assert_eq(region_aggregates[1].region, "us-west-2")
  assert_eq(region_aggregates[1].avg_latency, 85.0) // (95 + 75) / 2
}

// 测试2: 遥测数据异常检测
test "遥测数据异常检测与告警" {
  // 模拟正常和异常的遥测数据
  let telemetry_metrics = [
    { metric: "cpu_usage", value: 45.0, timestamp: 1640995200, status: "normal" },
    { metric: "cpu_usage", value: 48.0, timestamp: 1640995260, status: "normal" },
    { metric: "cpu_usage", value: 52.0, timestamp: 1640995320, status: "normal" },
    { metric: "cpu_usage", value: 95.0, timestamp: 1640995380, status: "anomaly" },
    { metric: "cpu_usage", value: 98.0, timestamp: 1640995440, status: "anomaly" },
    { metric: "memory_usage", value: 1024.0, timestamp: 1640995200, status: "normal" },
    { metric: "memory_usage", value: 1100.0, timestamp: 1640995260, status: "normal" },
    { metric: "memory_usage", value: 2048.0, timestamp: 1640995320, status: "anomaly" },
    { metric: "disk_usage", value: 75.0, timestamp: 1640995200, status: "warning" },
    { metric: "disk_usage", value: 78.0, timestamp: 1640995260, status: "warning" },
    { metric: "disk_usage", value: 85.0, timestamp: 1640995320, status: "critical" }
  ]
  
  // 定义异常检测阈值
  let thresholds = {
    cpu_usage: { normal: 60.0, warning: 80.0, critical: 90.0 },
    memory_usage: { normal: 1500.0, warning: 1800.0, critical: 2000.0 },
    disk_usage: { normal: 70.0, warning: 80.0, critical: 85.0 }
  }
  
  // 异常检测逻辑
  let mut detected_anomalies = []
  let mut warnings = []
  let mut critical_alerts = []
  
  for metric in telemetry_metrics {
    let threshold = match metric.metric {
      "cpu_usage" => thresholds.cpu_usage
      "memory_usage" => thresholds.memory_usage
      "disk_usage" => thresholds.disk_usage
      _ => { normal: 100.0, warning: 100.0, critical: 100.0 }
    }
    
    if metric.value >= threshold.critical {
      critical_alerts = critical_alerts.push({
        metric: metric.metric,
        value: metric.value,
        timestamp: metric.timestamp,
        severity: "critical"
      })
    } else if metric.value >= threshold.warning {
      warnings = warnings.push({
        metric: metric.metric,
        value: metric.value,
        timestamp: metric.timestamp,
        severity: "warning"
      })
    } else if metric.value >= threshold.normal {
      // 正常范围内，但可能需要监控
      detected_anomalies = detected_anomalies.push({
        metric: metric.metric,
        value: metric.value,
        timestamp: metric.timestamp,
        severity: "monitor"
      })
    }
  }
  
  // 验证异常检测结果
  assert_eq(critical_alerts.length(), 3)
  assert_eq(warnings.length(), 3)
  assert_eq(detected_anomalies.length(), 2)
  
  // 验证关键告警
  assert_eq(critical_alerts[0].metric, "cpu_usage")
  assert_eq(critical_alerts[0].value, 95.0)
  assert_eq(critical_alerts[1].metric, "cpu_usage")
  assert_eq(critical_alerts[1].value, 98.0)
  assert_eq(critical_alerts[2].metric, "disk_usage")
  assert_eq(critical_alerts[2].value, 85.0)
  
  // 验证警告
  assert_eq(warnings[0].metric, "memory_usage")
  assert_eq(warnings[0].value, 2048.0)
  assert_eq(warnings[1].metric, "disk_usage")
  assert_eq(warnings[1].value, 75.0)
  assert_eq(warnings[2].metric, "disk_usage")
  assert_eq(warnings[2].value, 78.0)
  
  // 验证监控项
  assert_eq(detected_anomalies[0].metric, "cpu_usage")
  assert_eq(detected_anomalies[0].value, 45.0)
  assert_eq(detected_anomalies[1].metric, "memory_usage")
  assert_eq(detected_anomalies[1].value, 1024.0)
}

// 测试3: 遥测数据序列化与反序列化
test "遥测数据序列化与反序列化处理" {
  // 模拟原始遥测数据
  let raw_telemetry = [
    { trace_id: "trace-123", span_id: "span-456", operation: "db_query", start_time: 1640995200, end_time: 1640995250, status: "ok" },
    { trace_id: "trace-123", span_id: "span-789", operation: "cache_lookup", start_time: 1640995250, end_time: 1640995270, status: "ok" },
    { trace_id: "trace-456", span_id: "span-111", operation: "api_call", start_time: 1640995300, end_time: 1640995350, status: "error" }
  ]
  
  // 模拟序列化为JSON格式
  let serialize_to_json = fn(data) {
    let mut json_strings = []
    for span in data {
      let json_string = "{" +
        "\"trace_id\":\"" + span.trace_id + "\"," +
        "\"span_id\":\"" + span.span_id + "\"," +
        "\"operation\":\"" + span.operation + "\"," +
        "\"start_time\":" + span.start_time.to_string() + "," +
        "\"end_time\":" + span.end_time.to_string() + "," +
        "\"status\":\"" + span.status + "\"" +
      "}"
      json_strings = json_strings.push(json_string)
    }
    "[" + json_strings.join(",") + "]"
  }
  
  // 执行序列化
  let json_data = serialize_to_json(raw_telemetry)
  
  // 验证序列化结果包含必要字段
  assert_true(json_data.contains("trace_id"))
  assert_true(json_data.contains("span_id"))
  assert_true(json_data.contains("operation"))
  assert_true(json_data.contains("start_time"))
  assert_true(json_data.contains("end_time"))
  assert_true(json_data.contains("status"))
  assert_true(json_data.contains("trace-123"))
  assert_true(json_data.contains("span-456"))
  assert_true(json_data.contains("db_query"))
  
  // 模拟反序列化过程
  let parse_json_value = fn(json_str: String, key: String) {
    let key_pattern = "\"" + key + "\":\""
    let start_index = json_str.index_of(key_pattern)
    if start_index == -1 {
      return ""
    }
    
    let value_start = start_index + key_pattern.length()
    let value_end = json_str.index_of("\"", value_start)
    if value_end == -1 {
      return ""
    }
    
    json_str.substring(value_start, value_end - value_start)
  }
  
  let parse_json_number = fn(json_str: String, key: String) {
    let key_pattern = "\"" + key + "\":"
    let start_index = json_str.index_of(key_pattern)
    if start_index == -1 {
      return 0
    }
    
    let value_start = start_index + key_pattern.length()
    let mut value_end = value_start
    while value_end < json_str.length() and (
      json_str[value_end] >= '0' and json_str[value_end] <= '9'
    ) {
      value_end = value_end + 1
    }
    
    let number_str = json_str.substring(value_start, value_end - value_start)
    let mut result = 0
    let mut i = 0
    while i < number_str.length() {
      result = result * 10 + (number_str[i].to_int() - '0'.to_int())
      i = i + 1
    }
    result
  }
  
  // 提取第一个span的JSON字符串进行反序列化测试
  let first_span_start = json_data.index_of("{") + 1
  let first_span_end = json_data.index_of("}", first_span_start)
  let first_span_json = json_data.substring(first_span_start, first_span_end - first_span_start)
  
  // 执行反序列化并验证结果
  let trace_id = parse_json_value(first_span_json, "trace_id")
  let span_id = parse_json_value(first_span_json, "span_id")
  let operation = parse_json_value(first_span_json, "operation")
  let start_time = parse_json_number(first_span_json, "start_time")
  let end_time = parse_json_number(first_span_json, "end_time")
  let status = parse_json_value(first_span_json, "status")
  
  // 验证反序列化结果
  assert_eq(trace_id, "trace-123")
  assert_eq(span_id, "span-456")
  assert_eq(operation, "db_query")
  assert_eq(start_time, 1640995200)
  assert_eq(end_time, 1640995250)
  assert_eq(status, "ok")
  
  // 验证序列化与反序列化的一致性
  assert_eq(trace_id, raw_telemetry[0].trace_id)
  assert_eq(span_id, raw_telemetry[0].span_id)
  assert_eq(operation, raw_telemetry[0].operation)
  assert_eq(start_time, raw_telemetry[0].start_time)
  assert_eq(end_time, raw_telemetry[0].end_time)
  assert_eq(status, raw_telemetry[0].status)
}

// 测试4: 遥测数据质量验证
test "遥测数据质量验证与清洗" {
  // 模拟包含质量问题的遥测数据
  let raw_telemetry_data = [
    { metric: "cpu", value: 45.0, timestamp: 1640995200, source: "server1" }, // 正常数据
    { metric: "cpu", value: -5.0, timestamp: 1640995201, source: "server2" }, // 负值异常
    { metric: "memory", value: 1024.0, timestamp: 1640995202, source: "server1" }, // 正常数据
    { metric: "memory", value: 999999999.0, timestamp: 1640995203, source: "server3" }, // 异常高值
    { metric: "disk", value: 75.0, timestamp: 1640995204, source: "" }, // 缺少源信息
    { metric: "network", value: 0.0, timestamp: 0, source: "server1" }, // 缺少时间戳
    { metric: "", value: 50.0, timestamp: 1640995206, source: "server2" }, // 缺少指标名称
    { metric: "cpu", value: 55.0, timestamp: 1640995207, source: "server1" } // 正常数据
  ]
  
  // 数据质量验证规则
  let quality_rules = {
    metric_name_required: true,
    source_required: true,
    timestamp_required: true,
    value_non_negative: true,
    value_reasonable_range: true,
    cpu_range: { min: 0.0, max: 100.0 },
    memory_range: { min: 0.0, max: 1048576.0 }, // 1TB in MB
    disk_range: { min: 0.0, max: 100.0 },
    network_range: { min: 0.0, max: 10000.0 }
  }
  
  // 数据质量验证函数
  let validate_data_point = fn(data_point) {
    let mut issues = []
    let mut is_valid = true
    
    // 检查必需字段
    if data_point.metric == "" {
      issues = issues.push("Missing metric name")
      is_valid = false
    }
    
    if data_point.source == "" {
      issues = issues.push("Missing source")
      is_valid = false
    }
    
    if data_point.timestamp == 0 {
      issues = issues.push("Missing timestamp")
      is_valid = false
    }
    
    // 检查值的有效性
    if data_point.value < 0.0 {
      issues = issues.push("Negative value")
      is_valid = false
    }
    
    // 检查特定指标的合理范围
    let range = match data_point.metric {
      "cpu" => quality_rules.cpu_range
      "memory" => quality_rules.memory_range
      "disk" => quality_rules.disk_range
      "network" => quality_rules.network_range
      _ => { min: 0.0, max: 1000000.0 }
    }
    
    if data_point.value < range.min || data_point.value > range.max {
      issues = issues.push("Value out of reasonable range")
      is_valid = false
    }
    
    { is_valid, issues }
  }
  
  // 执行数据质量验证
  let mut valid_data = []
  let mut invalid_data = []
  let mut quality_issues = []
  
  for data_point in raw_telemetry_data {
    let validation_result = validate_data_point(data_point)
    
    if validation_result.is_valid {
      valid_data = valid_data.push(data_point)
    } else {
      invalid_data = invalid_data.push({
        data_point: data_point,
        issues: validation_result.issues
      })
      
      for issue in validation_result.issues {
        quality_issues = quality_issues.push(issue)
      }
    }
  }
  
  // 验证数据质量检查结果
  assert_eq(valid_data.length(), 3)
  assert_eq(invalid_data.length(), 5)
  assert_eq(quality_issues.length(), 6)
  
  // 验证有效数据
  assert_eq(valid_data[0].metric, "cpu")
  assert_eq(valid_data[0].value, 45.0)
  assert_eq(valid_data[1].metric, "memory")
  assert_eq(valid_data[1].value, 1024.0)
  assert_eq(valid_data[2].metric, "cpu")
  assert_eq(valid_data[2].value, 55.0)
  
  // 验证无效数据及其问题
  assert_eq(invalid_data[0].data_point.metric, "cpu")
  assert_eq(invalid_data[0].data_point.value, -5.0)
  assert_true(invalid_data[0].issues.contains("Negative value"))
  
  assert_eq(invalid_data[1].data_point.metric, "memory")
  assert_eq(invalid_data[1].data_point.value, 999999999.0)
  assert_true(invalid_data[1].issues.contains("Value out of reasonable range"))
  
  assert_eq(invalid_data[2].data_point.metric, "disk")
  assert_eq(invalid_data[2].data_point.source, "")
  assert_true(invalid_data[2].issues.contains("Missing source"))
  
  assert_eq(invalid_data[3].data_point.metric, "network")
  assert_eq(invalid_data[3].data_point.timestamp, 0)
  assert_true(invalid_data[3].issues.contains("Missing timestamp"))
  
  assert_eq(invalid_data[4].data_point.metric, "")
  assert_true(invalid_data[4].issues.contains("Missing metric name"))
  
  // 验证质量问题统计
  let mut negative_value_count = 0
  let mut missing_field_count = 0
  let mut out_of_range_count = 0
  
  for issue in quality_issues {
    if issue == "Negative value" {
      negative_value_count = negative_value_count + 1
    } else if issue == "Missing metric name" || issue == "Missing source" || issue == "Missing timestamp" {
      missing_field_count = missing_field_count + 1
    } else if issue == "Value out of reasonable range" {
      out_of_range_count = out_of_range_count + 1
    }
  }
  
  assert_eq(negative_value_count, 1)
  assert_eq(missing_field_count, 3)
  assert_eq(out_of_range_count, 2)
}

// 测试5: 遥测系统资源管理
test "遥测系统资源管理与优化" {
  // 模拟系统资源使用情况
  let system_resources = {
    total_memory: 8192, // MB
    available_memory: 4096, // MB
    total_disk: 102400, // MB
    available_disk: 51200, // MB
    cpu_cores: 4,
    network_bandwidth: 1000 // Mbps
  }
  
  // 模拟遥测系统组件的资源需求
  let telemetry_components = [
    { name: "data_collector", memory_requirement: 512, cpu_requirement: 1, priority: "high" },
    { name: "data_processor", memory_requirement: 1024, cpu_requirement: 2, priority: "high" },
    { name: "data_storage", memory_requirement: 2048, cpu_requirement: 1, priority: "medium" },
    { name: "data_analyzer", memory_requirement: 1536, cpu_requirement: 2, priority: "medium" },
    { name: "alert_manager", memory_requirement: 256, cpu_requirement: 1, priority: "low" },
    { name: "dashboard", memory_requirement: 512, cpu_requirement: 1, priority: "low" }
  ]
  
  // 资源分配算法
  let allocate_resources = fn(components, resources) {
    let mut allocated_components = []
    let mut remaining_memory = resources.available_memory
    let mut remaining_cpu = resources.cpu_cores
    
    // 按优先级排序组件
    let mut high_priority = []
    let mut medium_priority = []
    let mut low_priority = []
    
    for component in components {
      match component.priority {
        "high" => high_priority = high_priority.push(component)
        "medium" => medium_priority = medium_priority.push(component)
        "low" => low_priority = low_priority.push(component)
        _ => ()
      }
    }
    
    // 按优先级分配资源
    let priority_groups = [high_priority, medium_priority, low_priority]
    
    for group in priority_groups {
      for component in group {
        if component.memory_requirement <= remaining_memory && 
           component.cpu_requirement <= remaining_cpu {
          allocated_components = allocated_components.push({
            name: component.name,
            memory_allocated: component.memory_requirement,
            cpu_allocated: component.cpu_requirement,
            status: "allocated"
          })
          
          remaining_memory = remaining_memory - component.memory_requirement
          remaining_cpu = remaining_cpu - component.cpu_requirement
        } else {
          allocated_components = allocated_components.push({
            name: component.name,
            memory_allocated: 0,
            cpu_allocated: 0,
            status: "pending"
          })
        }
      }
    }
    
    {
      allocated_components,
      remaining_memory,
      remaining_cpu
    }
  }
  
  // 执行资源分配
  let allocation_result = allocate_resources(telemetry_components, system_resources)
  
  // 验证资源分配结果
  assert_eq(allocation_result.allocated_components.length(), 6)
  
  // 验证高优先级组件被分配
  let data_collector = allocation_result.allocated_components[0]
  let data_processor = allocation_result.allocated_components[1]
  assert_eq(data_collector.name, "data_collector")
  assert_eq(data_collector.status, "allocated")
  assert_eq(data_collector.memory_allocated, 512)
  assert_eq(data_collector.cpu_allocated, 1)
  
  assert_eq(data_processor.name, "data_processor")
  assert_eq(data_processor.status, "allocated")
  assert_eq(data_processor.memory_allocated, 1024)
  assert_eq(data_processor.cpu_allocated, 2)
  
  // 验证中优先级组件分配情况
  let data_storage = allocation_result.allocated_components[2]
  let data_analyzer = allocation_result.allocated_components[3]
  assert_eq(data_storage.name, "data_storage")
  assert_eq(data_storage.status, "allocated")
  assert_eq(data_storage.memory_allocated, 2048)
  assert_eq(data_storage.cpu_allocated, 1)
  
  assert_eq(data_analyzer.name, "data_analyzer")
  assert_eq(data_analyzer.status, "allocated")
  assert_eq(data_analyzer.memory_allocated, 1536)
  assert_eq(data_analyzer.cpu_allocated, 2)
  
  // 验证低优先级组件分配情况（可能因资源不足而pending）
  let alert_manager = allocation_result.allocated_components[4]
  let dashboard = allocation_result.allocated_components[5]
  
  // 计算剩余资源
  let total_allocated_memory = 512 + 1024 + 2048 + 1536 // 5120 MB
  let total_allocated_cpu = 1 + 2 + 1 + 2 // 6 CPU cores
  
  assert_eq(allocation_result.remaining_memory, system_resources.available_memory - total_allocated_memory)
  assert_eq(allocation_result.remaining_cpu, 0) // 所有CPU核心已分配
  
  // 由于CPU不足，低优先级组件应该处于pending状态
  assert_eq(alert_manager.status, "pending")
  assert_eq(dashboard.status, "pending")
  
  // 资源优化：动态调整组件资源分配
  let optimize_resources = fn(allocated_components, remaining_memory, remaining_cpu) {
    let mut optimized_components = []
    
    for component in allocated_components {
      if component.status == "pending" && remaining_memory > 256 {
        // 为pending组件分配最小资源
        optimized_components = optimized_components.push({
          name: component.name,
          memory_allocated: 256,
          cpu_allocated: 0,
          status: "minimal"
        })
        remaining_memory = remaining_memory - 256
      } else {
        optimized_components = optimized_components.push(component)
      }
    }
    
    optimized_components
  }
  
  // 执行资源优化
  let optimized_components = optimize_resources(
    allocation_result.allocated_components,
    allocation_result.remaining_memory,
    allocation_result.remaining_cpu
  )
  
  // 验证资源优化结果
  assert_eq(optimized_components.length(), 6)
  assert_eq(optimized_components[4].status, "minimal") // alert_manager
  assert_eq(optimized_components[4].memory_allocated, 256)
  assert_eq(optimized_components[5].status, "minimal") // dashboard
  assert_eq(optimized_components[5].memory_allocated, 256)
}

// 测试6: 遥测数据可视化预处理
test "遥测数据可视化预处理" {
  // 模拟原始遥测数据
  let raw_telemetry = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "api" },
    { timestamp: 1640995260, metric: "cpu", value: 48.0, service: "api" },
    { timestamp: 1640995320, metric: "cpu", value: 52.0, service: "api" },
    { timestamp: 1640995380, metric: "cpu", value: 95.0, service: "api" },
    { timestamp: 1640995440, metric: "cpu", value: 98.0, service: "api" },
    { timestamp: 1640995500, metric: "cpu", value: 85.0, service: "api" },
    { timestamp: 1640995200, metric: "memory", value: 1024.0, service: "db" },
    { timestamp: 1640995260, metric: "memory", value: 1100.0, service: "db" },
    { timestamp: 1640995320, metric: "memory", value: 1200.0, service: "db" },
    { timestamp: 1640995380, metric: "memory", value: 2048.0, service: "db" },
    { timestamp: 1640995440, metric: "memory", value: 1900.0, service: "db" },
    { timestamp: 1640995500, metric: "memory", value: 1800.0, service: "db" }
  ]
  
  // 数据采样：降采样以减少可视化数据点
  let downsample_data = fn(data, interval) {
    let mut downsampled = []
    let mut i = 0
    
    while i < data.length() {
      downsampled = downsampled.push(data[i])
      i = i + interval
    }
    
    downsampled
  }
  
  // 数据聚合：按时间窗口聚合
  let aggregate_by_time_window = fn(data, window_size) {
    let mut aggregated = []
    let mut i = 0
    
    while i < data.length() {
      let window_start = data[i].timestamp
      let window_end = window_start + window_size
      
      let mut window_data = []
      let mut j = i
      
      while j < data.length() && data[j].timestamp < window_end {
        window_data = window_data.push(data[j])
        j = j + 1
      }
      
      // 计算窗口内的平均值
      let mut sum = 0.0
      for point in window_data {
        sum = sum + point.value
      }
      let avg_value = sum / window_data.length().to_float()
      
      aggregated = aggregated.push({
        timestamp: window_start,
        metric: data[i].metric,
        value: avg_value,
        service: data[i].service,
        count: window_data.length()
      })
      
      i = j
    }
    
    aggregated
  }
  
  // 数据标准化：将不同指标缩放到相同范围
  let normalize_data = fn(data) {
    // 按指标分组
    let mut cpu_data = []
    let mut memory_data = []
    
    for point in data {
      match point.metric {
        "cpu" => cpu_data = cpu_data.push(point.value)
        "memory" => memory_data = memory_data.push(point.value)
        _ => ()
      }
    }
    
    // 计算每个指标的最小值和最大值
    let find_min_max = fn(values) {
      let mut min = values[0]
      let mut max = values[0]
      
      for value in values {
        if value < min {
          min = value
        }
        if value > max {
          max = value
        }
      }
      
      { min, max }
    }
    
    let cpu_min_max = find_min_max(cpu_data)
    let memory_min_max = find_min_max(memory_data)
    
    // 标准化数据到0-100范围
    let mut normalized = []
    
    for point in data {
      let normalized_value = match point.metric {
        "cpu" => {
          if cpu_min_max.max > cpu_min_max.min {
            ((point.value - cpu_min_max.min) / (cpu_min_max.max - cpu_min_max.min)) * 100.0
          } else {
            50.0
          }
        }
        "memory" => {
          if memory_min_max.max > memory_min_max.min {
            ((point.value - memory_min_max.min) / (memory_min_max.max - memory_min_max.min)) * 100.0
          } else {
            50.0
          }
        }
        _ => point.value
      }
      
      normalized = normalized.push({
        timestamp: point.timestamp,
        metric: point.metric,
        value: normalized_value,
        service: point.service
      })
    }
    
    normalized
  }
  
  // 执行数据预处理
  let downsampled = downsample_data(raw_telemetry, 2)
  let aggregated = aggregate_by_time_window(raw_telemetry, 600) // 10分钟窗口
  let normalized = normalize_data(raw_telemetry)
  
  // 验证降采样结果
  assert_eq(downsampled.length(), 6) // 原始12个点，每2个取1个
  assert_eq(downsampled[0].timestamp, 1640995200)
  assert_eq(downsampled[1].timestamp, 1640995320)
  assert_eq(downsampled[2].timestamp, 1640995440)
  
  // 验证聚合结果
  assert_eq(aggregated.length(), 2) // 12个数据点，按10分钟窗口分为2组
  assert_eq(aggregated[0].timestamp, 1640995200)
  assert_eq(aggregated[0].count, 6) // 第一组6个数据点
  assert_eq(aggregated[1].timestamp, 1640995800)
  assert_eq(aggregated[1].count, 6) // 第二组6个数据点
  
  // 验证CPU数据聚合
  let cpu_aggregated = aggregated.filter(fn(point) { point.metric == "cpu" })
  assert_eq(cpu_aggregated.length(), 1)
  assert_eq(cpu_aggregated[0].value, 63.833333333333336) // (45 + 48 + 52 + 95 + 98 + 85) / 6
  
  // 验证内存数据聚合
  let memory_aggregated = aggregated.filter(fn(point) { point.metric == "memory" })
  assert_eq(memory_aggregated.length(), 1)
  assert_eq(memory_aggregated[0].value, 1512.0) // (1024 + 1100 + 1200 + 2048 + 1900 + 1800) / 6
  
  // 验证标准化结果
  assert_eq(normalized.length(), 12)
  
  // 验证CPU数据标准化范围
  let cpu_normalized = normalized.filter(fn(point) { point.metric == "cpu" })
  for point in cpu_normalized {
    assert_true(point.value >= 0.0)
    assert_true(point.value <= 100.0)
  }
  
  // 验证内存数据标准化范围
  let memory_normalized = normalized.filter(fn(point) { point.metric == "memory" })
  for point in memory_normalized {
    assert_true(point.value >= 0.0)
    assert_true(point.value <= 100.0)
  }
  
  // 验证最小值被标准化为0
  let cpu_values = cpu_normalized.map(fn(point) { point.value })
  let memory_values = memory_normalized.map(fn(point) { point.value })
  
  let find_min = fn(values) {
    let mut min = values[0]
    for value in values {
      if value < min {
        min = value
      }
    }
    min
  }
  
  let find_max = fn(values) {
    let mut max = values[0]
    for value in values {
      if value > max {
        max = value
      }
    }
    max
  }
  
  assert_eq(find_min(cpu_values), 0.0) // 最小CPU值(45)应该被标准化为0
  assert_eq(find_max(cpu_values), 100.0) // 最大CPU值(98)应该被标准化为100
  assert_eq(find_min(memory_values), 0.0) // 最小内存值(1024)应该被标准化为0
  assert_eq(find_max(memory_values), 100.0) // 最大内存值(2048)应该被标准化为100
}

// 测试7: 遥测系统自适应配置
test "遥测系统自适应配置管理" {
  // 模拟系统负载情况
  let system_load = {
    cpu_utilization: 65.0,
    memory_utilization: 70.0,
    disk_io: 1200.0, // IOPS
    network_throughput: 800.0 // Mbps
  }
  
  // 模拟遥测系统配置
  let base_config = {
    sampling_rate: 1.0, // 100%采样率
    batch_size: 100, // 批处理大小
    aggregation_interval: 60, // 聚合间隔(秒)
    retention_period: 7, // 数据保留期(天)
    compression_enabled: true,
    alert_threshold: 80.0 // 告警阈值
  }
  
  // 自适应配置规则
  let adaptation_rules = {
    high_cpu_threshold: 80.0,
    high_memory_threshold: 85.0,
    low_resource_threshold: 50.0,
    critical_threshold: 90.0
  }
  
  // 自适应配置算法
  let adapt_configuration = fn(config, load, rules) {
    let mut adapted_config = config
    
    // 根据CPU使用率调整采样率
    if load.cpu_utilization > rules.high_cpu_threshold {
      // CPU使用率高，降低采样率
      adapted_config.sampling_rate = 0.5
      adapted_config.batch_size = 200 // 增大批处理大小
    } else if load.cpu_utilization > rules.low_resource_threshold {
      // CPU使用率中等，适度降低采样率
      adapted_config.sampling_rate = 0.75
      adapted_config.batch_size = 150
    }
    
    // 根据内存使用率调整聚合间隔
    if load.memory_utilization > rules.high_memory_threshold {
      // 内存使用率高，增加聚合间隔以减少内存占用
      adapted_config.aggregation_interval = 120
      adapted_config.retention_period = 3 // 缩短保留期
    } else if load.memory_utilization > rules.low_resource_threshold {
      // 内存使用率中等，适度增加聚合间隔
      adapted_config.aggregation_interval = 90
      adapted_config.retention_period = 5
    }
    
    // 根据磁盘IO调整压缩设置
    if load.disk_io > 1000.0 {
      // 磁盘IO高，启用压缩
      adapted_config.compression_enabled = true
    } else {
      // 磁盘IO低，可以禁用压缩以节省CPU
      adapted_config.compression_enabled = false
    }
    
    // 根据网络吞吐量调整批处理大小
    if load.network_throughput > 750.0 {
      // 网络吞吐量高，增大批处理大小
      adapted_config.batch_size = adapted_config.batch_size * 2
    }
    
    // 根据整体负载调整告警阈值
    let overall_load = (load.cpu_utilization + load.memory_utilization) / 2.0
    if overall_load > rules.critical_threshold {
      // 系统负载高，提高告警阈值避免告警风暴
      adapted_config.alert_threshold = 90.0
    } else if overall_load > rules.high_cpu_threshold {
      // 系统负载中等，适度提高告警阈值
      adapted_config.alert_threshold = 85.0
    }
    
    adapted_config
  }
  
  // 执行自适应配置
  let adapted_config = adapt_configuration(base_config, system_load, adaptation_rules)
  
  // 验证自适应配置结果
  // CPU使用率65% > 50%，但< 80%，所以采样率应该调整为0.75
  assert_eq(adapted_config.sampling_rate, 0.75)
  assert_eq(adapted_config.batch_size, 150)
  
  // 内存使用率70% > 50%，但< 85%，所以聚合间隔应该调整为90秒
  assert_eq(adapted_config.aggregation_interval, 90)
  assert_eq(adapted_config.retention_period, 5)
  
  // 磁盘IO 1200 > 1000，所以压缩应该启用
  assert_eq(adapted_config.compression_enabled, true)
  
  // 网络吞吐量800 > 750，所以批处理大小应该翻倍
  assert_eq(adapted_config.batch_size, 300) // 150 * 2
  
  // 整体负载(65+70)/2 = 67.5 < 80%，所以告警阈值应该保持不变
  assert_eq(adapted_config.alert_threshold, 80.0)
  
  // 测试极端负载情况下的自适应配置
  let extreme_load = {
    cpu_utilization: 95.0,
    memory_utilization: 92.0,
    disk_io: 2000.0,
    network_throughput: 950.0
  }
  
  let extreme_adapted_config = adapt_configuration(base_config, extreme_load, adaptation_rules)
  
  // 验证极端负载下的配置
  assert_eq(extreme_adapted_config.sampling_rate, 0.5) // CPU > 80%
  assert_eq(extreme_adapted_config.batch_size, 400) // 200 * 2 (网络)
  assert_eq(extreme_adapted_config.aggregation_interval, 120) // 内存 > 85%
  assert_eq(extreme_adapted_config.retention_period, 3) // 内存 > 85%
  assert_eq(extreme_adapted_config.compression_enabled, true) // 磁盘IO > 1000
  assert_eq(extreme_adapted_config.alert_threshold, 90.0) // 整体负载 > 90%
  
  // 测试低负载情况下的自适应配置
  let low_load = {
    cpu_utilization: 30.0,
    memory_utilization: 40.0,
    disk_io: 500.0,
    network_throughput: 400.0
  }
  
  let low_adapted_config = adapt_configuration(base_config, low_load, adaptation_rules)
  
  // 验证低负载下的配置
  assert_eq(low_adapted_config.sampling_rate, 1.0) // 保持原始配置
  assert_eq(low_adapted_config.batch_size, 100) // 保持原始配置
  assert_eq(low_adapted_config.aggregation_interval, 60) // 保持原始配置
  assert_eq(low_adapted_config.retention_period, 7) // 保持原始配置
  assert_eq(low_adapted_config.compression_enabled, false) // 磁盘IO < 1000
  assert_eq(low_adapted_config.alert_threshold, 80.0) // 保持原始配置
}

// 测试8: 遥测数据生命周期管理
test "遥测数据生命周期管理" {
  // 模拟当前时间戳
  let current_time = 1641600000 // 2022-01-08 00:00:00 UTC
  
  // 模拟不同年龄的遥测数据
  let telemetry_data = [
    { id: "data1", timestamp: 1640995200, size: 1024, category: "metrics", access_count: 10 }, // 7天前
    { id: "data2", timestamp: 1641081600, size: 2048, category: "traces", access_count: 5 },   // 6天前
    { id: "data3", timestamp: 1641168000, size: 1536, category: "logs", access_count: 20 },    // 5天前
    { id: "data4", timestamp: 1641254400, size: 512, category: "metrics", access_count: 15 },   // 4天前
    { id: "data5", timestamp: 1641340800, size: 3072, category: "traces", access_count: 8 },   // 3天前
    { id: "data6", timestamp: 1641427200, size: 1024, category: "logs", access_count: 25 },     // 2天前
    { id: "data7", timestamp: 1641513600, size: 2560, category: "metrics", access_count: 12 },  // 1天前
    { id: "data8", timestamp: 1641600000, size: 1792, category: "traces", access_count: 18 }    // 当前
  ]
  
  // 数据生命周期策略
  let lifecycle_policy = {
    hot_storage_period: 2, // 热存储期(天)
    warm_storage_period: 5, // 温存储期(天)
    cold_storage_period: 7, // 冷存储期(天)
    archive_period: 30,     // 归档期(天)
    delete_period: 90,      // 删除期(天)
    min_access_count: 10    // 最小访问次数阈值
  }
  
  // 计算数据年龄(天)
  let calculate_data_age = fn(data_timestamp, current_timestamp) {
    (current_timestamp - data_timestamp) / 86400 // 秒转换为天
  }
  
  // 确定数据存储层级
  let determine_storage_tier = fn(data_age, access_count, policy) {
    if data_age <= policy.hot_storage_period {
      "hot"
    } else if data_age <= policy.warm_storage_period {
      "warm"
    } else if data_age <= policy.cold_storage_period {
      if access_count >= policy.min_access_count {
        "cold" // 高访问频率，保留在冷存储
      } else {
        "archive" // 低访问频率，移至归档
      }
    } else if data_age <= policy.archive_period {
      "archive"
    } else if data_age <= policy.delete_period {
      "delete_pending"
    } else {
      "deleted"
    }
  }
  
  // 应用生命周期策略
  let mut categorized_data = []
  let mut total_size_by_tier = {
    hot: 0,
    warm: 0,
    cold: 0,
    archive: 0,
    delete_pending: 0,
    deleted: 0
  }
  
  for data in telemetry_data {
    let data_age = calculate_data_age(data.timestamp, current_time)
    let storage_tier = determine_storage_tier(data_age, data.access_count, lifecycle_policy)
    
    let categorized_item = {
      id: data.id,
      timestamp: data.timestamp,
      size: data.size,
      category: data.category,
      access_count: data.access_count,
      age_days: data_age,
      storage_tier: storage_tier
    }
    
    categorized_data = categorized_data.push(categorized_item)
    
    // 统计各层级的存储大小
    match storage_tier {
      "hot" => total_size_by_tier.hot = total_size_by_tier.hot + data.size
      "warm" => total_size_by_tier.warm = total_size_by_tier.warm + data.size
      "cold" => total_size_by_tier.cold = total_size_by_tier.cold + data.size
      "archive" => total_size_by_tier.archive = total_size_by_tier.archive + data.size
      "delete_pending" => total_size_by_tier.delete_pending = total_size_by_tier.delete_pending + data.size
      "deleted" => total_size_by_tier.deleted = total_size_by_tier.deleted + data.size
      _ => ()
    }
  }
  
  // 验证数据分类结果
  assert_eq(categorized_data.length(), 8)
  
  // 验证热存储数据(≤2天)
  let hot_data = categorized_data.filter(fn(item) { item.storage_tier == "hot" })
  assert_eq(hot_data.length(), 3) // data6, data7, data8
  assert_eq(hot_data[0].id, "data6") // 2天前
  assert_eq(hot_data[1].id, "data7") // 1天前
  assert_eq(hot_data[2].id, "data8") // 当前
  
  // 验证温存储数据(>2天且≤5天)
  let warm_data = categorized_data.filter(fn(item) { item.storage_tier == "warm" })
  assert_eq(warm_data.length(), 2) // data4, data5
  assert_eq(warm_data[0].id, "data4") // 4天前
  assert_eq(warm_data[1].id, "data5") // 3天前
  
  // 验证冷存储数据(>5天且≤7天，高访问频率)
  let cold_data = categorized_data.filter(fn(item) { item.storage_tier == "cold" })
  assert_eq(cold_data.length(), 2) // data1(访问10次), data3(访问20次)
  assert_eq(cold_data[0].id, "data1") // 7天前，访问10次
  assert_eq(cold_data[1].id, "data3") // 5天前，访问20次
  
  // 验证归档数据(>5天且≤7天，低访问频率)
  let archive_data = categorized_data.filter(fn(item) { item.storage_tier == "archive" })
  assert_eq(archive_data.length(), 1) // data2(访问5次)
  assert_eq(archive_data[0].id, "data2") // 6天前，访问5次
  
  // 验证各层级存储大小统计
  assert_eq(total_size_by_tier.hot, 5376) // 1024 + 2560 + 1792
  assert_eq(total_size_by_tier.warm, 3584) // 512 + 3072
  assert_eq(total_size_by_tier.cold, 2560) // 1024 + 1536
  assert_eq(total_size_by_tier.archive, 2048) // 2048
  
  // 验证总存储大小
  let total_size = total_size_by_tier.hot + total_size_by_tier.warm + 
                   total_size_by_tier.cold + total_size_by_tier.archive
  let expected_total = 5376 + 3584 + 2560 + 2048
  assert_eq(total_size, expected_total)
  
  // 模拟存储成本计算(不同层级有不同的成本)
  let storage_costs = {
    hot: 0.10,      // $0.10/GB/天
    warm: 0.05,     // $0.05/GB/天
    cold: 0.01,     // $0.01/GB/天
    archive: 0.002  // $0.002/GB/天
  }
  
  let calculate_storage_cost = fn(size_by_tier, costs) {
    let total_cost = 
      (size_by_tier.hot / 1024.0 / 1024.0) * costs.hot +
      (size_by_tier.warm / 1024.0 / 1024.0) * costs.warm +
      (size_by_tier.cold / 1024.0 / 1024.0) * costs.cold +
      (size_by_tier.archive / 1024.0 / 1024.0) * costs.archive
    total_cost
  }
  
  let daily_storage_cost = calculate_storage_cost(total_size_by_tier, storage_costs)
  
  // 验证存储成本计算
  assert_true(daily_storage_cost > 0.0)
  
  // 热存储成本应该是最高的部分
  let hot_cost = (total_size_by_tier.hot / 1024.0 / 1024.0) * storage_costs.hot
  assert_true(hot_cost > 0.0)
  
  // 归档存储成本应该是最低的部分
  let archive_cost = (total_size_by_tier.archive / 1024.0 / 1024.0) * storage_costs.archive
  assert_true(archive_cost > 0.0)
  assert_true(archive_cost < hot_cost)
}