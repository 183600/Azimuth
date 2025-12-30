// 性能基准测试用例

test "telemetry_data_ingestion_performance" {
  // 测试遥测数据摄取性能
  
  let ingestion_scenarios = [
    {
      "scenario": "low_volume",
      "events_per_second": 100,
      "event_size_bytes": 512,
      "duration_seconds": 10
    },
    {
      "scenario": "medium_volume",
      "events_per_second": 1000,
      "event_size_bytes": 1024,
      "duration_seconds": 10
    },
    {
      "scenario": "high_volume",
      "events_per_second": 10000,
      "event_size_bytes": 2048,
      "duration_seconds": 10
    },
    {
      "scenario": "burst_volume",
      "events_per_second": 50000,
      "event_size_bytes": 1024,
      "duration_seconds": 5
    }
  ]
  
  // 测试数据摄取性能
  let mut ingestion_results = []
  let mut i = 0
  while i < ingestion_scenarios.length() {
    let scenario = ingestion_scenarios[i]
    let events_per_second = scenario["events_per_second"]
    let event_size = scenario["event_size_bytes"]
    let duration = scenario["duration_seconds"]
    
    // 计算总事件数和数据量
    let total_events = events_per_second * duration
    let total_data_mb = (total_events * event_size) / (1024 * 1024)
    
    // 模拟摄取性能指标
    let processing_latency_ms = 
      if events_per_second <= 100 {
        5
      } else if events_per_second <= 1000 {
        15
      } else if events_per_second <= 10000 {
        50
      } else {
        200
      }
    
    let throughput_mbps = (total_data_mb.to_double() / duration.to_double())
    let cpu_utilization = (events_per_second.to_double() / 100000.0) * 100  // 简化模型
    let memory_usage_mb = (total_events.to_double() / 1000.0) * 10  // 简化模型
    
    ingestion_results.push({
      "scenario": scenario["scenario"],
      "total_events": total_events,
      "total_data_mb": total_data_mb,
      "processing_latency_ms": processing_latency_ms,
      "throughput_mbps": throughput_mbps,
      "cpu_utilization_percent": cpu_utilization,
      "memory_usage_mb": memory_usage_mb,
      "events_processed_per_second": events_per_second
    })
    
    i = i + 1
  }
  
  // 验证摄取性能结果
  assert_eq(ingestion_results.length(), 4)
  
  // 验证低音量场景
  let low_volume = ingestion_results[0]
  assert_eq(low_volume["total_events"], 1000)        // 100 * 10
  assert_eq(low_volume["total_data_mb"], (1000 * 512) / (1024 * 1024))  // 约0.49MB
  assert_eq(low_volume["processing_latency_ms"], 5)
  assert_eq(low_volume["cpu_utilization_percent"], 0.1)  // 100/100000 * 100
  
  // 验证高音量场景
  let high_volume = ingestion_results[2]
  assert_eq(high_volume["total_events"], 100000)      // 10000 * 10
  assert_eq(high_volume["processing_latency_ms"], 50)
  assert_eq(high_volume["cpu_utilization_percent"], 10.0)  // 10000/100000 * 100
  
  // 验证性能随负载增长的趋势
  let mut latency_growth = true
  let mut i = 1
  while i < ingestion_results.length() {
    if ingestion_results[i]["processing_latency_ms"] < ingestion_results[i-1]["processing_latency_ms"] {
      latency_growth = false
      break
    }
    i = i + 1
  }
  assert_eq(latency_growth, true)
  
  // 验证吞吐量计算
  i = 0
  while i < ingestion_results.length() {
    let result = ingestion_results[i]
    let expected_throughput = result["total_data_mb"].to_double() / ingestion_scenarios[i]["duration_seconds"].to_double()
    assert_eq(result["throughput_mbps"], expected_throughput)
    assert_eq(result["throughput_mbps"] > 0.0, true)
    i = i + 1
  }
}

test "telemetry_data_processing_performance" {
  // 测试遥测数据处理性能
  
  let processing_workloads = [
    {
      "workload": "metrics_aggregation",
      "data_points": 10000,
      "processing_complexity": "O(n)",
      "expected_time_ms": 50
    },
    {
      "workload": "trace_analysis",
      "spans": 5000,
      "processing_complexity": "O(n log n)",
      "expected_time_ms": 200
    },
    {
      "workload": "log_parsing",
      "log_entries": 20000,
      "processing_complexity": "O(n)",
      "expected_time_ms": 150
    },
    {
      "workload": "anomaly_detection",
      "data_points": 8000,
      "processing_complexity": "O(n²)",
      "expected_time_ms": 500
    }
  ]
  
  // 测试处理性能
  let mut processing_results = []
  let mut i = 0
  while i < processing_workloads.length() {
    let workload = processing_workloads[i]
    let data_size = 
      if workload["workload"] == "metrics_aggregation" {
        workload["data_points"]
      } else if workload["workload"] == "trace_analysis" {
        workload["spans"]
      } else if workload["workload"] == "log_parsing" {
        workload["log_entries"]
      } else {
        workload["data_points"]
      }
    
    let complexity = workload["processing_complexity"]
    
    // 模拟处理时间（基于复杂度和数据大小）
    let base_time = 10  // 基础时间10ms
    let processing_time = 
      if complexity == "O(n)" {
        base_time + (data_size / 1000)
      } else if complexity == "O(n log n)" {
        base_time + ((data_size / 1000) * 2)
      } else if complexity == "O(n²)" {
        base_time + ((data_size / 1000) * (data_size / 1000))
      } else {
        base_time
      }
    
    // 计算吞吐量
    let throughput = data_size.to_double() / processing_time.to_double()
    
    // 计算资源使用
    let memory_usage_mb = (data_size.to_double() / 1000.0) * 5  // 每1000数据点使用5MB
    let cpu_cycles = data_size * 100  // 每数据点需要100个CPU周期
    
    processing_results.push({
      "workload": workload["workload"],
      "data_size": data_size,
      "complexity": complexity,
      "processing_time_ms": processing_time,
      "throughput_ops_per_ms": throughput,
      "memory_usage_mb": memory_usage_mb,
      "cpu_cycles": cpu_cycles,
      "meets_expectation": processing_time <= workload["expected_time_ms"]
    })
    
    i = i + 1
  }
  
  // 验证处理性能结果
  assert_eq(processing_results.length(), 4)
  
  // 验证不同工作负载的性能特征
  let metrics_agg = processing_results[0]
  let trace_analysis = processing_results[1]
  let log_parsing = processing_results[2]
  let anomaly_detection = processing_results[3]
  
  // 验证数据大小
  assert_eq(metrics_agg["data_size"], 10000)
  assert_eq(trace_analysis["data_size"], 5000)
  assert_eq(log_parsing["data_size"], 20000)
  assert_eq(anomaly_detection["data_size"], 8000)
  
  // 验证复杂度影响
  assert_eq(metrics_agg["complexity"], "O(n)")
  assert_eq(trace_analysis["complexity"], "O(n log n)")
  assert_eq(anomaly_detection["complexity"], "O(n²)")
  
  // 验证处理时间随复杂度增长
  assert_eq(metrics_agg["processing_time_ms"] < trace_analysis["processing_time_ms"], true)
  assert_eq(trace_analysis["processing_time_ms"] < anomaly_detection["processing_time_ms"], true)
  
  // 验证吞吐量计算
  assert_eq(metrics_agg["throughput_ops_per_ms"], 10000.0 / metrics_agg["processing_time_ms"])
  assert_eq(trace_analysis["throughput_ops_per_ms"], 5000.0 / trace_analysis["processing_time_ms"])
  
  // 验证资源使用合理性
  assert_eq(metrics_agg["memory_usage_mb"], 10000.0 / 1000.0 * 5)  // 50MB
  assert_eq(log_parsing["memory_usage_mb"], 20000.0 / 1000.0 * 5)  // 100MB
}

test "telemetry_storage_performance" {
  // 测试遥测存储性能
  
  let storage_operations = [
    {
      "operation": "write_metrics",
      "record_count": 1000,
      "record_size_bytes": 256,
      "storage_type": "time_series_db"
    },
    {
      "operation": "write_traces",
      "record_count": 500,
      "record_size_bytes": 1024,
      "storage_type": "document_db"
    },
    {
      "operation": "write_logs",
      "record_count": 2000,
      "record_size_bytes": 512,
      "storage_type": "log_store"
    },
    {
      "operation": "read_metrics",
      "record_count": 5000,
      "record_size_bytes": 128,
      "storage_type": "time_series_db"
    }
  ]
  
  // 测试存储性能
  let mut storage_results = []
  let mut i = 0
  while i < storage_operations.length() {
    let op = storage_operations[i]
    let operation = op["operation"]
    let record_count = op["record_count"]
    let record_size = op["record_size_bytes"]
    let storage_type = op["storage_type"]
    
    // 计算总数据量
    let total_data_kb = (record_count * record_size) / 1024
    
    // 模拟存储性能（基于操作类型和存储类型）
    let base_latency = 
      if storage_type == "time_series_db" {
        5
      } else if storage_type == "document_db" {
        10
      } else if storage_type == "log_store" {
        3
      } else {
        8
      }
    
    let operation_type = operation.split("_")[0]  // write or read
    let latency_multiplier = 
      if operation_type == "write" {
        1.0
      } else {
        0.5  // 读操作通常更快
      }
    
    let avg_latency_ms = (base_latency + (record_count / 1000)) * latency_multiplier
    let total_latency_ms = avg_latency_ms * record_count
    
    // 计算吞吐量
    let throughput_ops_per_sec = 1000.0 / avg_latency_ms
    let data_throughput_mbps = (total_data_kb.to_double() / 1024.0) / (total_latency_ms.to_double() / 1000.0)
    
    // 计算存储效率
    let compression_ratio = 
      if storage_type == "time_series_db" {
        0.3  // 70% compression
      } else if storage_type == "document_db" {
        0.5  // 50% compression
      } else {
        0.4  // 60% compression
      }
    
    let storage_efficiency = 1.0 - compression_ratio
    
    storage_results.push({
      "operation": operation,
      "storage_type": storage_type,
      "record_count": record_count,
      "total_data_kb": total_data_kb,
      "avg_latency_ms": avg_latency_ms,
      "total_latency_ms": total_latency_ms,
      "throughput_ops_per_sec": throughput_ops_per_sec,
      "data_throughput_mbps": data_throughput_mbps,
      "storage_efficiency": storage_efficiency
    })
    
    i = i + 1
  }
  
  // 验证存储性能结果
  assert_eq(storage_results.length(), 4)
  
  // 验证写操作性能
  let write_metrics = storage_results[0]
  let write_traces = storage_results[1]
  let write_logs = storage_results[2]
  
  assert_eq(write_metrics["operation"], "write_metrics")
  assert_eq(write_metrics["record_count"], 1000)
  assert_eq(write_metrics["total_data_kb"], (1000 * 256) / 1024)  // 250KB
  assert_eq(write_metrics["storage_type"], "time_series_db")
  
  // 验证读操作性能（应该比写快）
  let read_metrics = storage_results[3]
  assert_eq(read_metrics["avg_latency_ms"] < write_metrics["avg_latency_ms"], true)
  
  // 验证吞吐量计算
  assert_eq(write_metrics["throughput_ops_per_sec"], 1000.0 / write_metrics["avg_latency_ms"])
  assert_eq(write_traces["throughput_ops_per_sec"], 1000.0 / write_traces["avg_latency_ms"])
  
  // 验证存储效率
  assert_eq(write_metrics["storage_efficiency"], 0.7)  // 70% efficiency
  assert_eq(write_traces["storage_efficiency"], 0.5)   // 50% efficiency
  assert_eq(write_logs["storage_efficiency"], 0.6)     // 60% efficiency
  
  // 验证数据吞吐量
  assert_eq(write_metrics["data_throughput_mbps"] > 0.0, true)
  assert_eq(write_traces["data_throughput_mbps"] > 0.0, true)
}

test "telemetry_query_performance" {
  // 测试遥测查询性能
  
  let query_scenarios = [
    {
      "query_type": "time_range_filter",
      "data_set_size": 1000000,
      "time_range_hours": 24,
      "expected_complexity": "O(log n)"
    },
    {
      "query_type": "attribute_filter",
      "data_set_size": 500000,
      "filter_conditions": 3,
      "expected_complexity": "O(n)"
    },
    {
      "query_type": "aggregation_query",
      "data_set_size": 2000000,
      "aggregation_type": "avg",
      "expected_complexity": "O(n)"
    },
    {
      "query_type": "join_query",
      "data_set_size": 100000,
      "join_tables": 3,
      "expected_complexity": "O(n²)"
    }
  ]
  
  // 测试查询性能
  let mut query_results = []
  let mut i = 0
  while i < query_scenarios.length() {
    let scenario = query_scenarios[i]
    let query_type = scenario["query_type"]
    let data_size = scenario["data_set_size"]
    
    // 模拟查询执行时间
    let base_time = 20  // 基础查询时间20ms
    let complexity = scenario["expected_complexity"]
    
    let execution_time = 
      if complexity == "O(log n)" {
        base_time + (data_size.to_double().log() * 2)
      } else if complexity == "O(n)" {
        base_time + (data_size / 10000)
      } else if complexity == "O(n²)" {
        base_time + ((data_size / 1000) * (data_size / 1000) / 100)
      } else {
        base_time
      }
    
    // 计算查询效率
    let records_per_ms = data_size.to_double() / execution_time
    let efficiency_score = 1000000.0 / execution_time  // 简化的效率评分
    
    // 计算资源消耗
    let memory_usage_mb = (data_size.to_double() / 100000.0) * 10  // 每10万记录使用10MB
    let cpu_utilization = (execution_time / 1000.0) * 25  // 简化的CPU使用率
    
    query_results.push({
      "query_type": query_type,
      "data_set_size": data_size,
      "complexity": complexity,
      "execution_time_ms": execution_time,
      "records_per_ms": records_per_ms,
      "efficiency_score": efficiency_score,
      "memory_usage_mb": memory_usage_mb,
      "cpu_utilization_percent": cpu_utilization
    })
    
    i = i + 1
  }
  
  // 验证查询性能结果
  assert_eq(query_results.length(), 4)
  
  // 验证不同查询类型的性能特征
  let time_range_query = query_results[0]
  let attribute_filter = query_results[1]
  let aggregation_query = query_results[2]
  let join_query = query_results[3]
  
  // 验证数据集大小
  assert_eq(time_range_query["data_set_size"], 1000000)
  assert_eq(attribute_filter["data_set_size"], 500000)
  assert_eq(aggregation_query["data_set_size"], 2000000)
  assert_eq(join_query["data_set_size"], 100000)
  
  // 验证复杂度对性能的影响
  assert_eq(time_range_query["complexity"], "O(log n)")
  assert_eq(join_query["complexity"], "O(n²)")
  
  // O(log n) 应该比 O(n²) 快
  assert_eq(time_range_query["execution_time_ms"] < join_query["execution_time_ms"], true)
  
  // 验证查询效率计算
  assert_eq(time_range_query["records_per_ms"], 1000000.0 / time_range_query["execution_time_ms"])
  assert_eq(attribute_filter["records_per_ms"], 500000.0 / attribute_filter["execution_time_ms"])
  
  // 验证效率评分
  assert_eq(time_range_query["efficiency_score"], 1000000.0 / time_range_query["execution_time_ms"])
  assert_eq(join_query["efficiency_score"], 1000000.0 / join_query["execution_time_ms"])
  
  // O(log n) 查询应该有更高的效率评分
  assert_eq(time_range_query["efficiency_score"] > join_query["efficiency_score"], true)
  
  // 验证资源使用合理性
  assert_eq(time_range_query["memory_usage_mb"], 1000000.0 / 100000.0 * 10)  // 100MB
  assert_eq(join_query["memory_usage_mb"], 100000.0 / 100000.0 * 10)     // 10MB
}

test "telemetry_system_scalability" {
  // 测试遥测系统可扩展性
  
  let scalability_tests = [
    {
      "test_name": "horizontal_scaling",
      "initial_nodes": 2,
      "scaled_nodes": 8,
      "load_per_node": 1000,
      "expected_scaling_factor": 4.0
    },
    {
      "test_name": "vertical_scaling",
      "initial_cpu_cores": 4,
      "scaled_cpu_cores": 16,
      "memory_gb": 32,
      "expected_scaling_factor": 3.5
    },
    {
      "test_name": "data_volume_scaling",
      "initial_daily_volume_gb": 100,
      "scaled_daily_volume_gb": 1000,
      "retention_days": 30,
      "expected_scaling_factor": 8.0
    }
  ]
  
  // 测试可扩展性
  let mut scalability_results = []
  let mut i = 0
  while i < scalability_tests.length() {
    let test = scalability_tests[i]
    let test_name = test["test_name"]
    
    let initial_throughput = 
      if test_name == "horizontal_scaling" {
        test["initial_nodes"] * test["load_per_node"]
      } else if test_name == "vertical_scaling" {
        test["initial_cpu_cores"] * 250  // 每核心250 ops/s
      } else if test_name == "data_volume_scaling" {
        test["initial_daily_volume_gb"] * 1000  // 每GB 1000 ops/s
      } else {
        1000
      }
    
    let scaled_throughput = 
      if test_name == "horizontal_scaling" {
        test["scaled_nodes"] * test["load_per_node"]
      } else if test_name == "vertical_scaling" {
        test["scaled_cpu_cores"] * 250
      } else if test_name == "data_volume_scaling" {
        test["scaled_daily_volume_gb"] * 1000
      } else {
        4000
      }
    
    let actual_scaling_factor = scaled_throughput.to_double() / initial_throughput.to_double()
    let scaling_efficiency = actual_scaling_factor / test["expected_scaling_factor"]
    
    // 计算资源成本
    let resource_cost_increase = 
      if test_name == "horizontal_scaling" {
        test["scaled_nodes"].to_double() / test["initial_nodes"].to_double()
      } else if test_name == "vertical_scaling" {
        test["scaled_cpu_cores"].to_double() / test["initial_cpu_cores"].to_double()
      } else if test_name == "data_volume_scaling" {
        test["scaled_daily_volume_gb"].to_double() / test["initial_daily_volume_gb"].to_double()
      } else {
        1.0
      }
    
    let cost_efficiency = actual_scaling_factor / resource_cost_increase
    
    scalability_results.push({
      "test_name": test_name,
      "initial_throughput": initial_throughput,
      "scaled_throughput": scaled_throughput,
      "actual_scaling_factor": actual_scaling_factor,
      "expected_scaling_factor": test["expected_scaling_factor"],
      "scaling_efficiency": scaling_efficiency,
      "resource_cost_increase": resource_cost_increase,
      "cost_efficiency": cost_efficiency,
      "meets_expectation": scaling_efficiency >= 0.8  // 80%效率阈值
    })
    
    i = i + 1
  }
  
  // 验证可扩展性结果
  assert_eq(scalability_results.length(), 3)
  
  // 验证水平扩展
  let horizontal_scaling = scalability_results[0]
  assert_eq(horizontal_scaling["test_name"], "horizontal_scaling")
  assert_eq(horizontal_scaling["initial_throughput"], 2 * 1000)  // 2000
  assert_eq(horizontal_scaling["scaled_throughput"], 8 * 1000)  // 8000
  assert_eq(horizontal_scaling["actual_scaling_factor"], 4.0)
  assert_eq(horizontal_scaling["expected_scaling_factor"], 4.0)
  assert_eq(horizontal_scaling["scaling_efficiency"], 1.0)  // 完美线性扩展
  assert_eq(horizontal_scaling["meets_expectation"], true)
  
  // 验证垂直扩展
  let vertical_scaling = scalability_results[1]
  assert_eq(vertical_scaling["initial_throughput"], 4 * 250)  // 1000
  assert_eq(vertical_scaling["scaled_throughput"], 16 * 250)  // 4000
  assert_eq(vertical_scaling["actual_scaling_factor"], 4.0)
  assert_eq(vertical_scaling["expected_scaling_factor"], 3.5)
  assert_eq(vertical_scaling["scaling_efficiency"] > 1.0, true)  // 超出预期
  assert_eq(vertical_scaling["meets_expectation"], true)
  
  // 验证数据量扩展
  let data_volume_scaling = scalability_results[2]
  assert_eq(data_volume_scaling["initial_throughput"], 100 * 1000)  // 100000
  assert_eq(data_volume_scaling["scaled_throughput"], 1000 * 1000)  // 1000000
  assert_eq(data_volume_scaling["actual_scaling_factor"], 10.0)
  assert_eq(data_volume_scaling["expected_scaling_factor"], 8.0)
  assert_eq(data_volume_scaling["scaling_efficiency"], 1.25)  // 125%效率
  assert_eq(data_volume_scaling["meets_expectation"], true)
  
  // 验证成本效率
  assert_eq(horizontal_scaling["cost_efficiency"], 4.0 / 4.0)  // 1.0
  assert_eq(vertical_scaling["cost_efficiency"], 4.0 / 4.0)    // 1.0
  assert_eq(data_volume_scaling["cost_efficiency"], 10.0 / 10.0) // 1.0
}

test "telemetry_memory_performance" {
  // 测试遥测内存性能
  
  let memory_scenarios = [
    {
      "scenario": "metrics_buffer",
      "buffer_size": 10000,
      "metric_size_bytes": 128,
      "retention_period_ms": 60000
    },
    {
      "scenario": "trace_cache",
      "cache_size": 5000,
      "trace_size_bytes": 512,
      "eviction_policy": "LRU"
    },
    {
      "scenario": "log_ring_buffer",
      "buffer_size": 50000,
      "log_size_bytes": 256,
      "max_size_mb": 100
    },
    {
      "scenario": "aggregation_state",
      "state_entries": 1000,
      "state_size_bytes": 1024,
      "update_frequency_hz": 10
    }
  ]
  
  // 测试内存性能
  let mut memory_results = []
  let mut i = 0
  while i < memory_scenarios.length() {
    let scenario = memory_scenarios[i]
    let scenario_name = scenario["scenario"]
    
    // 计算内存使用
    let memory_usage_mb = 
      if scenario_name == "metrics_buffer" {
        (scenario["buffer_size"] * scenario["metric_size_bytes"]) / (1024 * 1024)
      } else if scenario_name == "trace_cache" {
        (scenario["cache_size"] * scenario["trace_size_bytes"]) / (1024 * 1024)
      } else if scenario_name == "log_ring_buffer" {
        (scenario["buffer_size"] * scenario["log_size_bytes"]) / (1024 * 1024)
      } else if scenario_name == "aggregation_state" {
        (scenario["state_entries"] * scenario["state_size_bytes"]) / (1024 * 1024)
      } else {
        0
      }
    
    // 模拟内存分配性能
    let allocation_time_ms = memory_usage_mb * 2  // 每MB分配需要2ms
    let deallocation_time_ms = memory_usage_mb * 1  // 每MB释放需要1ms
    
    // 计算内存效率
    let memory_efficiency = 
      if scenario_name == "metrics_buffer" {
        0.85  // 85% 利用率
      } else if scenario_name == "trace_cache" {
        0.75  // 75% 利用率（考虑缓存开销）
      } else if scenario_name == "log_ring_buffer" {
        0.90  // 90% 利用率
      } else if scenario_name == "aggregation_state" {
        0.70  // 70% 利用率（考虑状态管理开销）
      } else {
        0.80
      }
    
    let effective_memory_mb = memory_usage_mb * memory_efficiency
    
    // 计算内存压力
    let memory_pressure = 
      if scenario_name == "log_ring_buffer" and scenario.contains("max_size_mb") {
        effective_memory_mb / scenario["max_size_mb"].to_double()
      } else {
        effective_memory_mb / 1024.0  // 相对于1GB
      }
    
    // GC影响（简化）
    let gc_frequency_per_min = 
      if memory_usage_mb < 50 {
        2
      } else if memory_usage_mb < 200 {
        5
      } else {
        10
      }
    
    memory_results.push({
      "scenario": scenario_name,
      "memory_usage_mb": memory_usage_mb,
      "effective_memory_mb": effective_memory_mb,
      "memory_efficiency": memory_efficiency,
      "allocation_time_ms": allocation_time_ms,
      "deallocation_time_ms": deallocation_time_ms,
      "memory_pressure": memory_pressure,
      "gc_frequency_per_min": gc_frequency_per_min,
      "memory_health": memory_pressure < 0.8  // 内存压力小于80%为健康
    })
    
    i = i + 1
  }
  
  // 验证内存性能结果
  assert_eq(memory_results.length(), 4)
  
  // 验证内存使用计算
  let metrics_buffer = memory_results[0]
  let trace_cache = memory_results[1]
  let log_ring_buffer = memory_results[2]
  let aggregation_state = memory_results[3]
  
  assert_eq(metrics_buffer["memory_usage_mb"], (10000 * 128) / (1024 * 1024))  // 约1.22MB
  assert_eq(trace_cache["memory_usage_mb"], (5000 * 512) / (1024 * 1024))      // 约2.44MB
  assert_eq(log_ring_buffer["memory_usage_mb"], (50000 * 256) / (1024 * 1024))  // 约12.21MB
  assert_eq(aggregation_state["memory_usage_mb"], (1000 * 1024) / (1024 * 1024)) // 约1.0MB
  
  // 验证内存效率
  assert_eq(metrics_buffer["memory_efficiency"], 0.85)
  assert_eq(trace_cache["memory_efficiency"], 0.75)
  assert_eq(log_ring_buffer["memory_efficiency"], 0.90)
  assert_eq(aggregation_state["memory_efficiency"], 0.70)
  
  // 验证有效内存
  assert_eq(metrics_buffer["effective_memory_mb"], metrics_buffer["memory_usage_mb"] * 0.85)
  assert_eq(trace_cache["effective_memory_mb"], trace_cache["memory_usage_mb"] * 0.75)
  
  // 验证内存压力
  assert_eq(log_ring_buffer["memory_pressure"], log_ring_buffer["effective_memory_mb"] / 100.0)
  assert_eq(metrics_buffer["memory_pressure"], metrics_buffer["effective_memory_mb"] / 1024.0)
  
  // 验证GC频率
  assert_eq(metrics_buffer["gc_frequency_per_min"], 2)  // < 50MB
  assert_eq(trace_cache["gc_frequency_per_min"], 2)    // < 50MB
  assert_eq(log_ring_buffer["gc_frequency_per_min"], 5) // 12.21MB < 200MB
  
  // 验证内存健康状态
  assert_eq(metrics_buffer["memory_health"], true)
  assert_eq(trace_cache["memory_health"], true)
  assert_eq(log_ring_buffer["memory_health"], true)  // 压力应该小于0.8
  assert_eq(aggregation_state["memory_health"], true)
}