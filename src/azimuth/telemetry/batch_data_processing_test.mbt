// 批量数据处理测试用例

test "batch_data_collection" {
  // 测试批量数据收集
  
  let data_generators = [
    ("metrics", 100),
    ("traces", 50),
    ("logs", 200)
  ]
  
  let base_timestamp = 1640995200000
  let mut collected_batches = {}
  
  let mut i = 0
  while i < data_generators.length() {
    let data_type = data_generators[i].0
    let batch_size = data_generators[i].1
    
    let mut batch = []
    let mut j = 0
    while j < batch_size {
      let data_item = {
        "id": data_type + "_" + j.to_string(),
        "timestamp": base_timestamp + j,
        "type": data_type,
        "value": (j * 10).to_string()
      }
      batch.push(data_item)
      j = j + 1
    }
    
    collected_batches[data_type] = batch
    i = i + 1
  }
  
  // 验证批量收集结果
  assert_eq(collected_batches["metrics"].length(), 100)
  assert_eq(collected_batches["traces"].length(), 50)
  assert_eq(collected_batches["logs"].length(), 200)
  
  // 验证数据完整性
  let metrics_batch = collected_batches["metrics"]
  assert_eq(metrics_batch[0]["id"], "metrics_0")
  assert_eq(metrics_batch[0]["timestamp"], base_timestamp)
  assert_eq(metrics_batch[99]["id"], "metrics_99")
  assert_eq(metrics_batch[99]["timestamp"], base_timestamp + 99)
  
  // 验证跨类型数据的一致性
  let traces_batch = collected_batches["traces"]
  assert_eq(traces_batch[0]["type"], "traces")
  assert_eq(traces_batch[49]["type"], "traces")
  
  let logs_batch = collected_batches["logs"]
  assert_eq(logs_batch[0]["value"], "0")
  assert_eq(logs_batch[199]["value"], "1990")
}

test "batch_size_optimization" {
  // 测试批量大小优化
  
  let batch_size_options = [10, 50, 100, 500, 1000]
  let mut optimization_results = []
  
  let mut i = 0
  while i < batch_size_options.length() {
    let batch_size = batch_size_options[i]
    let start_time = 1640995200000
    
    // 模拟批量处理
    let mut processing_time = 0
    let mut memory_usage = 0
    let mut throughput = 0.0
    
    // 创建数据批次
    let mut batch = []
    let mut j = 0
    while j < batch_size {
      let data_item = {
        "id": "item_" + j.to_string(),
        "payload": "data_" + j.to_string(),
        "metadata": {
          "created_at": start_time + j,
          "size": 1024  // 1KB per item
        }
      }
      batch.push(data_item)
      j = j + 1
    }
    
    // 模拟处理时间（随批次大小增长）
    processing_time = batch_size / 10  // 简化：每10个项目需要1ms
    
    // 模拟内存使用（每项目1KB）
    memory_usage = batch_size * 1024
    
    // 计算吞吐量
    throughput = batch_size.to_double() / processing_time.to_double()
    
    optimization_results.push({
      "batch_size": batch_size,
      "processing_time": processing_time,
      "memory_usage": memory_usage,
      "throughput": throughput
    })
    
    i = i + 1
  }
  
  // 验证优化结果
  assert_eq(optimization_results.length(), 5)
  
  // 验证处理时间随批次大小增长
  let mut time_growth = true
  let mut i = 1
  while i < optimization_results.length() {
    if optimization_results[i]["processing_time"] < optimization_results[i-1]["processing_time"] {
      time_growth = false
      break
    }
    i = i + 1
  }
  assert_eq(time_growth, true)
  
  // 验证内存使用随批次大小增长
  let mut memory_growth = true
  i = 1
  while i < optimization_results.length() {
    if optimization_results[i]["memory_usage"] < optimization_results[i-1]["memory_usage"] {
      memory_growth = false
      break
    }
    i = i + 1
  }
  assert_eq(memory_growth, true)
  
  // 验证吞吐量计算
  i = 0
  while i < optimization_results.length() {
    let batch_size = optimization_results[i]["batch_size"]
    let processing_time = optimization_results[i]["processing_time"]
    let expected_throughput = batch_size.to_double() / processing_time.to_double()
    let actual_throughput = optimization_results[i]["throughput"]
    
    assert_eq(actual_throughput, expected_throughput)
    assert_eq(actual_throughput > 0.0, true)
    i = i + 1
  }
  
  // 找到最优批次大小（基于吞吐量/内存比率）
  let mut optimal_batch = optimization_results[0]
  let mut best_efficiency = optimal_batch["throughput"] / optimal_batch["memory_usage"].to_double()
  
  i = 1
  while i < optimization_results.length() {
    let current_efficiency = optimization_results[i]["throughput"] / optimization_results[i]["memory_usage"].to_double()
    if current_efficiency > best_efficiency {
      best_efficiency = current_efficiency
      optimal_batch = optimization_results[i]
    }
    i = i + 1
  }
  
  // 验证最优批次选择
  assert_eq(optimal_batch["batch_size"] > 0, true)
  assert_eq(best_efficiency > 0.0, true)
}

test "batch_data_compression" {
  // 测试批量数据压缩
  
  let raw_data_sizes = [1024, 5120, 10240, 51200, 102400]  // 1KB, 5KB, 10KB, 50KB, 100KB
  let mut compression_results = []
  
  let mut i = 0
  while i < raw_data_sizes.length() {
    let raw_size = raw_data_sizes[i]
    
    // 创建原始数据
    let mut raw_data = ""
    let mut j = 0
    while j < raw_size {
      raw_data = raw_data + "x"  // 重复字符，易于压缩
      j = j + 1
    }
    
    // 模拟压缩（简化：压缩率随数据大小增加）
    let compression_ratio = 0.3 + (raw_size.to_double() / 100000.0) * 0.4  // 30% - 70%
    let compressed_size = (raw_size.to_double() * compression_ratio).to_int()
    
    // 模拟压缩时间
    let compression_time = raw_size / 10000  // 每10KB需要1ms
    
    compression_results.push({
      "raw_size": raw_size,
      "compressed_size": compressed_size,
      "compression_ratio": compression_ratio,
      "compression_time": compression_time,
      "space_saved": raw_size - compressed_size
    })
    
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(compression_results.length(), 5)
  
  // 验证压缩后数据更小
  let mut i = 0
  while i < compression_results.length() {
    let result = compression_results[i]
    assert_eq(result["compressed_size"] < result["raw_size"], true)
    assert_eq(result["space_saved"] > 0, true)
    i = i + 1
  }
  
  // 验证压缩率在合理范围内
  i = 0
  while i < compression_results.length() {
    let compression_ratio = compression_results[i]["compression_ratio"]
    assert_eq(compression_ratio >= 0.3 and compression_ratio <= 0.7, true)
    i = i + 1
  }
  
  // 验证压缩时间随数据大小增长
  let mut time_growth = true
  i = 1
  while i < compression_results.length() {
    if compression_results[i]["compression_time"] < compression_results[i-1]["compression_time"] {
      time_growth = false
      break
    }
    i = i + 1
  }
  assert_eq(time_growth, true)
  
  // 计算压缩效率
  let mut compression_efficiency = []
  i = 0
  while i < compression_results.length() {
    let result = compression_results[i]
    let efficiency = result["space_saved"].to_double() / result["compression_time"].to_double()
    
    compression_efficiency.push({
      "raw_size": result["raw_size"],
      "efficiency": efficiency
    })
    i = i + 1
  }
  
  // 验证效率计算
  assert_eq(compression_efficiency.length(), 5)
  i = 0
  while i < compression_efficiency.length() {
    let efficiency = compression_efficiency[i]["efficiency"]
    assert_eq(efficiency > 0.0, true)
    i = i + 1
  }
}

test "batch_data_aggregation" {
  // 测试批量数据聚合
  
  let raw_metrics = [
    ("cpu_usage", 45.2),
    ("cpu_usage", 52.8),
    ("cpu_usage", 38.9),
    ("cpu_usage", 61.3),
    ("cpu_usage", 48.7),
    ("memory_usage", 67.4),
    ("memory_usage", 71.2),
    ("memory_usage", 65.8),
    ("memory_usage", 73.1),
    ("memory_usage", 69.6)
  ]
  
  // 按指标名称分组
  let mut grouped_metrics = {}
  let mut i = 0
  while i < raw_metrics.length() {
    let metric_name = raw_metrics[i].0
    let metric_value = raw_metrics[i].1
    
    if not grouped_metrics.contains(metric_name) {
      grouped_metrics[metric_name] = []
    }
    
    let values = grouped_metrics[metric_name]
    values.push(metric_value)
    grouped_metrics[metric_name] = values
    i = i + 1
  }
  
  // 验证分组结果
  assert_eq(grouped_metrics["cpu_usage"].length(), 5)
  assert_eq(grouped_metrics["memory_usage"].length(), 5)
  
  // 计算聚合统计
  let mut aggregation_results = {}
  for metric_name in grouped_metrics.keys() {
    let values = grouped_metrics[metric_name]
    
    let mut sum = 0.0
    let mut min_value = 999999.0
    let mut max_value = 0.0
    let mut j = 0
    while j < values.length() {
      let value = values[j]
      sum = sum + value
      
      if value < min_value {
        min_value = value
      }
      if value > max_value {
        max_value = value
      }
      j = j + 1
    }
    
    let average = sum / values.length().to_double()
    
    aggregation_results[metric_name] = {
      "count": values.length(),
      "sum": sum,
      "average": average,
      "min": min_value,
      "max": max_value
    }
  }
  
  // 验证聚合结果
  let cpu_stats = aggregation_results["cpu_usage"]
  assert_eq(cpu_stats["count"], 5)
  assert_eq(cpu_stats["sum"], 45.2 + 52.8 + 38.9 + 61.3 + 48.7)
  assert_eq(cpu_stats["min"], 38.9)
  assert_eq(cpu_stats["max"], 61.3)
  assert_eq(cpu_stats["average"] > 40.0, true)
  assert_eq(cpu_stats["average"] < 55.0, true)
  
  let memory_stats = aggregation_results["memory_usage"]
  assert_eq(memory_stats["count"], 5)
  assert_eq(memory_stats["min"], 65.8)
  assert_eq(memory_stats["max"], 73.1)
  assert_eq(memory_stats["average"] > 65.0, true)
  assert_eq(memory_stats["average"] < 75.0, true)
  
  // 验证聚合统计的数学性质
  assert_eq(cpu_stats["average"] >= cpu_stats["min"] and cpu_stats["average"] <= cpu_stats["max"], true)
  assert_eq(memory_stats["average"] >= memory_stats["min"] and memory_stats["average"] <= memory_stats["max"], true)
  
  // 计算百分位数
  let cpu_values = grouped_metrics["cpu_usage"]
  let mut sorted_cpu = []
  let mut j = 0
  while j < cpu_values.length() {
    sorted_cpu.push(cpu_values[j])
    j = j + 1
  }
  
  // 简单排序
  let mut k = 0
  while k < sorted_cpu.length() - 1 {
    let mut l = 0
    while l < sorted_cpu.length() - 1 - k {
      if sorted_cpu[l] > sorted_cpu[l + 1] {
        let temp = sorted_cpu[l]
        sorted_cpu[l] = sorted_cpu[l + 1]
        sorted_cpu[l + 1] = temp
      }
      l = l + 1
    }
    k = k + 1
  }
  
  // 计算百分位数
  let p50_index = sorted_cpu.length() / 2
  let p95_index = (sorted_cpu.length() * 95) / 100
  let p99_index = (sorted_cpu.length() * 99) / 100
  
  let p50 = sorted_cpu[p50_index]
  let p95 = sorted_cpu[p95_index >= sorted_cpu.length() ? sorted_cpu.length() - 1 : p95_index]
  let p99 = sorted_cpu[p99_index >= sorted_cpu.length() ? sorted_cpu.length() - 1 : p99_index]
  
  // 验证百分位数
  assert_eq(p50 >= sorted_cpu[0] and p50 <= sorted_cpu[sorted_cpu.length() - 1], true)
  assert_eq(p95 >= p50 and p95 <= p99, true)
  assert_eq(p99 >= p95 and p99 <= sorted_cpu[sorted_cpu.length() - 1], true)
}

test "batch_data_filtering" {
  // 测试批量数据过滤
  
  let raw_data = [
    {
      "id": "item_001",
      "type": "metric",
      "value": 25.5,
      "timestamp": 1640995200000,
      "tags": ["service:user", "region:us-east"]
    },
    {
      "id": "item_002",
      "type": "trace",
      "duration_ms": 150,
      "timestamp": 1640995201000,
      "tags": ["service:order", "region:us-west"]
    },
    {
      "id": "item_003",
      "type": "log",
      "level": "ERROR",
      "timestamp": 1640995202000,
      "tags": ["service:payment", "region:us-east"]
    },
    {
      "id": "item_004",
      "type": "metric",
      "value": 78.9,
      "timestamp": 1640995203000,
      "tags": ["service:user", "region:eu-west"]
    },
    {
      "id": "item_005",
      "type": "trace",
      "duration_ms": 85,
      "timestamp": 1640995204000,
      "tags": ["service:auth", "region:us-east"]
    }
  ]
  
  // 按类型过滤
  let mut type_filters = {}
  let types = ["metric", "trace", "log"]
  let mut i = 0
  while i < types.length() {
    let filter_type = types[i]
    let mut filtered_items = []
    let mut j = 0
    while j < raw_data.length() {
      if raw_data[j]["type"] == filter_type {
        filtered_items.push(raw_data[j])
      }
      j = j + 1
    }
    type_filters[filter_type] = filtered_items
    i = i + 1
  }
  
  // 验证类型过滤结果
  assert_eq(type_filters["metric"].length(), 2)
  assert_eq(type_filters["trace"].length(), 2)
  assert_eq(type_filters["log"].length(), 1)
  assert_eq(type_filters["metric"][0]["id"], "item_001")
  assert_eq(type_filters["metric"][1]["id"], "item_004")
  
  // 按时间范围过滤
  let start_time = 1640995201000
  let end_time = 1640995203000
  let mut time_filtered = []
  i = 0
  while i < raw_data.length() {
    let timestamp = raw_data[i]["timestamp"]
    if timestamp >= start_time and timestamp <= end_time {
      time_filtered.push(raw_data[i])
    }
    i = i + 1
  }
  
  // 验证时间过滤结果
  assert_eq(time_filtered.length(), 3)
  assert_eq(time_filtered[0]["id"], "item_002")
  assert_eq(time_filtered[1]["id"], "item_003")
  assert_eq(time_filtered[2]["id"], "item_004")
  
  // 按标签过滤
  let mut tag_filtered = []
  i = 0
  while i < raw_data.length() {
    let tags = raw_data[i]["tags"]
    let mut has_region_us_east = false
    let mut j = 0
    while j < tags.length() {
      if tags[j] == "region:us-east" {
        has_region_us_east = true
        break
      }
      j = j + 1
    }
    
    if has_region_us_east {
      tag_filtered.push(raw_data[i])
    }
    i = i + 1
  }
  
  // 验证标签过滤结果
  assert_eq(tag_filtered.length(), 3)
  assert_eq(tag_filtered[0]["id"], "item_001")
  assert_eq(tag_filtered[1]["id"], "item_003")
  assert_eq(tag_filtered[2]["id"], "item_005")
  
  // 组合过滤：类型 + 时间 + 标签
  let mut combined_filtered = []
  i = 0
  while i < raw_data.length() {
    let item = raw_data[i]
    
    // 类型条件
    let type_match = item["type"] == "metric" or item["type"] == "trace"
    
    // 时间条件
    let time_match = item["timestamp"] >= 1640995200000 and item["timestamp"] <= 1640995204000
    
    // 标签条件
    let mut tag_match = false
    let tags = item["tags"]
    let mut j = 0
    while j < tags.length() {
      if tags[j].contains("us-east") or tags[j].contains("us-west") {
        tag_match = true
        break
      }
      j = j + 1
    }
    
    if type_match and time_match and tag_match {
      combined_filtered.push(item)
    }
    i = i + 1
  }
  
  // 验证组合过滤结果
  assert_eq(combined_filtered.length(), 4)  // item_001, item_002, item_004, item_005
}

test "batch_data_transformation" {
  // 测试批量数据转换
  
  let source_data = [
    {
      "metric_name": "cpu_usage_percent",
      "metric_value": "75.5",
      "timestamp_ms": "1640995200000",
      "host_name": "web-server-01"
    },
    {
      "metric_name": "memory_usage_mb",
      "metric_value": "8192",
      "timestamp_ms": "1640995201000",
      "host_name": "web-server-01"
    },
    {
      "metric_name": "disk_io_ops_per_sec",
      "metric_value": "1250",
      "timestamp_ms": "1640995202000",
      "host_name": "web-server-01"
    }
  ]
  
  // 数据转换：字符串到数值，时间戳标准化
  let mut transformed_data = []
  let mut i = 0
  while i < source_data.length() {
    let source = source_data[i]
    
    let transformed = {
      "name": source["metric_name"],
      "value": source["metric_value"].to_double(),
      "timestamp": source["timestamp_ms"].to_int() / 1000,  // 转换为秒
      "host": source["host_name"],
      "unit": ""
    }
    
    // 添加单位信息
    if transformed["name"].contains("percent") {
      transformed["unit"] = "percent"
    } else if transformed["name"].contains("mb") {
      transformed["unit"] = "megabytes"
    } else if transformed["name"].contains("ops_per_sec") {
      transformed["unit"] = "operations_per_second"
    }
    
    transformed_data.push(transformed)
    i = i + 1
  }
  
  // 验证转换结果
  assert_eq(transformed_data.length(), 3)
  assert_eq(transformed_data[0]["value"], 75.5)
  assert_eq(transformed_data[1]["value"], 8192.0)
  assert_eq(transformed_data[2]["timestamp"], 1640995202)  // 1640995202000 / 1000
  assert_eq(transformed_data[0]["unit"], "percent")
  assert_eq(transformed_data[1]["unit"], "megabytes")
  
  // 数据标准化：统一命名约定
  let mut standardized_data = []
  i = 0
  while i < transformed_data.length() {
    let item = transformed_data[i]
    let original_name = item["name"]
    let mut standardized_name = ""
    
    // 简化的标准化：移除单位后缀，用下划线分隔
    let mut j = 0
    while j < original_name.length() {
      let char = original_name.char_at(j)
      if char == '_' {
        standardized_name = standardized_name + "."
      } else {
        standardized_name = standardized_name + char
      }
      j = j + 1
    }
    
    // 移除单位后缀
    if standardized_name.contains("_percent") {
      standardized_name = standardized_name.replace("_percent", "")
    } else if standardized_name.contains("_mb") {
      standardized_name = standardized_name.replace("_mb", "")
    } else if standardized_name.contains("_ops_per_sec") {
      standardized_name = standardized_name.replace("_ops_per_sec", "")
    }
    
    let standardized = {
      "metric": standardized_name,
      "value": item["value"],
      "timestamp": item["timestamp"],
      "host": item["host"],
      "unit": item["unit"]
    }
    
    standardized_data.push(standardized)
    i = i + 1
  }
  
  // 验证标准化结果
  assert_eq(standardized_data.length(), 3)
  assert_eq(standardized_data[0]["metric"], "cpu.usage")
  assert_eq(standardized_data[1]["metric"], "memory.usage")
  assert_eq(standardized_data[2]["metric"], "disk.io")
  assert_eq(standardized_data[0]["unit"], "percent")
  assert_eq(standardized_data[1]["unit"], "megabytes")
  
  // 数据丰富：添加计算字段
  let mut enriched_data = []
  i = 0
  while i < standardized_data.length() {
    let item = standardized_data[i]
    let enriched = {
      "metric": item["metric"],
      "value": item["value"],
      "timestamp": item["timestamp"],
      "host": item["host"],
      "unit": item["unit"],
      "category": "",
      "is_anomaly": false
    }
    
    // 添加分类
    if enriched["metric"].contains("cpu") or enriched["metric"].contains("memory") {
      enriched["category"] = "system"
    } else if enriched["metric"].contains("disk") {
      enriched["category"] = "io"
    }
    
    // 异常检测（简化）
    if enriched["metric"] == "cpu.usage" and enriched["value"] > 80.0 {
      enriched["is_anomaly"] = true
    } else if enriched["metric"] == "memory.usage" and enriched["value"] > 10000.0 {
      enriched["is_anomaly"] = true
    }
    
    enriched_data.push(enriched)
    i = i + 1
  }
  
  // 验证丰富结果
  assert_eq(enriched_data.length(), 3)
  assert_eq(enriched_data[0]["category"], "system")
  assert_eq(enriched_data[1]["category"], "system")
  assert_eq(enriched_data[2]["category"], "io")
  assert_eq(enriched_data[0]["is_anomaly"], false)  // 75.5 < 80
  assert_eq(enriched_data[1]["is_anomaly"], false)  // 8192 < 10000
}