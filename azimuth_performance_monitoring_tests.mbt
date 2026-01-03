// Azimuth 性能监控测试用例
// 专注于系统性能指标的收集、分析和报告功能

// 测试1: CPU性能监控
test "CPU性能监控指标收集" {
  // 创建CPU性能数据
  let cpu_metrics = {
    "timestamp": 1640995200,
    "cpu": {
      "usage_percent": 75.5,
      "user_percent": 45.2,
      "system_percent": 20.3,
      "idle_percent": 24.5,
      "load_average": {
        "1min": 1.25,
        "5min": 1.18,
        "15min": 1.05
      },
      "cores": [
        {"core_id": 0, "usage_percent": 72.1, "user_percent": 42.5, "system_percent": 19.8},
        {"core_id": 1, "usage_percent": 78.9, "user_percent": 47.9, "system_percent": 20.8},
        {"core_id": 2, "usage_percent": 74.3, "user_percent": 44.1, "system_percent": 20.2},
        {"core_id": 3, "usage_percent": 76.7, "user_percent": 46.3, "system_percent": 20.4}
      ]
    }
  }
  
  // 验证CPU总体指标
  assert_eq(cpu_metrics["cpu"]["usage_percent"], 75.5)
  assert_eq(cpu_metrics["cpu"]["user_percent"], 45.2)
  assert_eq(cpu_metrics["cpu"]["system_percent"], 20.3)
  assert_eq(cpu_metrics["cpu"]["idle_percent"], 24.5)
  
  // 验证负载平均值
  assert_eq(cpu_metrics["cpu"]["load_average"]["1min"], 1.25)
  assert_eq(cpu_metrics["cpu"]["load_average"]["5min"], 1.18)
  assert_eq(cpu_metrics["cpu"]["load_average"]["15min"], 1.05)
  
  // 验证CPU核心数量
  assert_eq(cpu_metrics["cpu"]["cores"].length(), 4)
  
  // 验证第一个核心指标
  let core0 = cpu_metrics["cpu"]["cores"][0]
  assert_eq(core0["core_id"], 0)
  assert_eq(core0["usage_percent"], 72.1)
  assert_eq(core0["user_percent"], 42.5)
  assert_eq(core0["system_percent"], 19.8)
  
  // 计算平均核心使用率
  let avg_core_usage = cpu_metrics["cpu"]["cores"].reduce(fn(acc, core) {
    acc + core["usage_percent"]
  }, 0.0) / (cpu_metrics["cpu"]["cores"].length() as Float)
  
  assert_eq(avg_core_usage.round(), 76.0)
  
  // 验证CPU使用率告警阈值
  let warning_threshold = 80.0
  let critical_threshold = 90.0
  
  let cpu_alert_level = if cpu_metrics["cpu"]["usage_percent"] >= critical_threshold {
    "critical"
  } else if cpu_metrics["cpu"]["usage_percent"] >= warning_threshold {
    "warning"
  } else {
    "normal"
  }
  
  assert_eq(cpu_alert_level, "normal")
}

// 测试2: 内存性能监控
test "内存性能监控指标收集" {
  // 创建内存性能数据
  let memory_metrics = {
    "timestamp": 1640995200,
    "memory": {
      "total": 8589934592,      // 8GB
      "available": 2147483648,   // 2GB
      "used": 6442450944,        // 6GB
      "free": 2147483648,        // 2GB
      "cached": 1073741824,      // 1GB
      "buffers": 536870912,      // 512MB
      "swap": {
        "total": 2147483648,     // 2GB
        "used": 268435456,       // 256MB
        "free": 1879048192       // 1.75GB
      },
      "processes": [
        {"pid": 1234, "name": "api-service", "memory_mb": 512.5},
        {"pid": 5678, "name": "database", "memory_mb": 2048.0},
        {"pid": 9012, "name": "cache-service", "memory_mb": 1024.0}
      ]
    }
  }
  
  // 验证内存总体指标
  assert_eq(memory_metrics["memory"]["total"], 8589934592)
  assert_eq(memory_metrics["memory"]["available"], 2147483648)
  assert_eq(memory_metrics["memory"]["used"], 6442450948)
  assert_eq(memory_metrics["memory"]["free"], 2147483648)
  assert_eq(memory_metrics["memory"]["cached"], 1073741824)
  assert_eq(memory_metrics["memory"]["buffers"], 536870912)
  
  // 验证交换内存指标
  assert_eq(memory_metrics["memory"]["swap"]["total"], 2147483648)
  assert_eq(memory_metrics["memory"]["swap"]["used"], 268435456)
  assert_eq(memory_metrics["memory"]["swap"]["free"], 1879048192)
  
  // 计算内存使用率
  let memory_usage_percent = (memory_metrics["memory"]["used"] as Float) / 
                            (memory_metrics["memory"]["total"] as Float) * 100.0
  
  assert_eq(memory_usage_percent.round(), 75.0)
  
  // 计算可用内存百分比
  let available_percent = (memory_metrics["memory"]["available"] as Float) / 
                         (memory_metrics["memory"]["total"] as Float) * 100.0
  
  assert_eq(available_percent.round(), 25.0)
  
  // 验证进程内存使用
  assert_eq(memory_metrics["memory"]["processes"].length(), 3)
  
  // 计算进程总内存使用
  let total_process_memory = memory_metrics["memory"]["processes"].reduce(fn(acc, process) {
    acc + process["memory_mb"]
  }, 0.0)
  
  assert_eq(total_process_memory, 3584.5)
  
  // 找出内存使用最高的进程
  let top_memory_process = memory_metrics["memory"]["processes"].reduce(fn(acc, process) {
    if process["memory_mb"] > acc["memory_mb"] { process } else { acc }
  }, memory_metrics["memory"]["processes"][0])
  
  assert_eq(top_memory_process["name"], "database")
  assert_eq(top_memory_process["memory_mb"], 2048.0)
  
  // 内存告警检查
  let memory_warning_threshold = 80.0
  let memory_critical_threshold = 90.0
  
  let memory_alert_level = if memory_usage_percent >= memory_critical_threshold {
    "critical"
  } else if memory_usage_percent >= memory_warning_threshold {
    "warning"
  } else {
    "normal"
  }
  
  assert_eq(memory_alert_level, "normal")
}

// 测试3: 磁盘I/O性能监控
test "磁盘I/O性能监控指标收集" {
  // 创建磁盘I/O性能数据
  let disk_metrics = {
    "timestamp": 1640995200,
    "disk": {
      "devices": [
        {
          "device": "/dev/sda",
          "total": 107374182400,    // 100GB
          "used": 53687091200,      // 50GB
          "free": 53687091200,      // 50GB
          "usage_percent": 50.0,
          "read_bytes": 1073741824,  // 1GB
          "write_bytes": 2147483648, // 2GB
          "read_ops": 10000,
          "write_ops": 5000,
          "read_time_ms": 1500,
          "write_time_ms": 2500
        },
        {
          "device": "/dev/sdb",
          "total": 214748364800,    // 200GB
          "used": 64424509440,      // 60GB
          "free": 150323814560,     // 140GB
          "usage_percent": 30.0,
          "read_bytes": 536870912,   // 512MB
          "write_bytes": 1073741824, // 1GB
          "read_ops": 5000,
          "write_ops": 2500,
          "read_time_ms": 800,
          "write_time_ms": 1200
        }
      ]
    }
  }
  
  // 验证磁盘设备数量
  assert_eq(disk_metrics["disk"]["devices"].length(), 2)
  
  // 验证第一个磁盘设备
  let sda = disk_metrics["disk"]["devices"][0]
  assert_eq(sda["device"], "/dev/sda")
  assert_eq(sda["total"], 107374182400)
  assert_eq(sda["used"], 53687091200)
  assert_eq(sda["free"], 53687091200)
  assert_eq(sda["usage_percent"], 50.0)
  assert_eq(sda["read_bytes"], 1073741824)
  assert_eq(sda["write_bytes"], 2147483648)
  assert_eq(sda["read_ops"], 10000)
  assert_eq(sda["write_ops"], 5000)
  assert_eq(sda["read_time_ms"], 1500)
  assert_eq(sda["write_time_ms"], 2500)
  
  // 计算平均读取时间 (微秒/操作)
  let avg_read_time_sda = (sda["read_time_ms"] as Float) / (sda["read_ops"] as Float)
  assert_eq(avg_read_time_sda.round(), 0.0)
  
  // 计算平均写入时间 (微秒/操作)
  let avg_write_time_sda = (sda["write_time_ms"] as Float) / (sda["write_ops"] as Float)
  assert_eq(avg_write_time_sda.round(), 1.0)
  
  // 计算总磁盘使用率
  let total_disk_space = disk_metrics["disk"]["devices"].reduce(fn(acc, device) {
    acc + device["total"]
  }, 0)
  
  let total_disk_used = disk_metrics["disk"]["devices"].reduce(fn(acc, device) {
    acc + device["used"]
  }, 0)
  
  let overall_disk_usage = (total_disk_used as Float) / (total_disk_space as Float) * 100.0
  assert_eq(overall_disk_usage.round(), 37.0)
  
  // 找出使用率最高的磁盘
  let highest_usage_disk = disk_metrics["disk"]["devices"].reduce(fn(acc, device) {
    if device["usage_percent"] > acc["usage_percent"] { device } else { acc }
  }, disk_metrics["disk"]["devices"][0])
  
  assert_eq(highest_usage_disk["device"], "/dev/sda")
  assert_eq(highest_usage_disk["usage_percent"], 50.0)
  
  // 磁盘空间告警检查
  let disk_warning_threshold = 80.0
  let disk_critical_threshold = 90.0
  
  let disk_alerts = disk_metrics["disk"]["devices"].map(fn(device) {
    let alert_level = if device["usage_percent"] >= disk_critical_threshold {
      "critical"
    } else if device["usage_percent"] >= disk_warning_threshold {
      "warning"
    } else {
      "normal"
    }
    
    {
      "device": device["device"],
      "usage_percent": device["usage_percent"],
      "alert_level": alert_level
    }
  })
  
  // 验证所有磁盘告警级别
  assert_eq(disk_alerts[0]["alert_level"], "normal")
  assert_eq(disk_alerts[1]["alert_level"], "normal")
}

// 测试4: 网络性能监控
test "网络性能监控指标收集" {
  // 创建网络性能数据
  let network_metrics = {
    "timestamp": 1640995200,
    "network": {
      "interfaces": [
        {
          "interface": "eth0",
          "bytes_sent": 10737418240,   // 10GB
          "bytes_recv": 21474836480,   // 20GB
          "packets_sent": 1000000,
          "packets_recv": 2000000,
          "err_in": 10,
          "err_out": 5,
          "drop_in": 2,
          "drop_out": 1,
          "speed_mbps": 1000
        },
        {
          "interface": "lo",
          "bytes_sent": 1073741824,    // 1GB
          "bytes_recv": 1073741824,    // 1GB
          "packets_sent": 100000,
          "packets_recv": 100000,
          "err_in": 0,
          "err_out": 0,
          "drop_in": 0,
          "drop_out": 0,
          "speed_mbps": 0
        }
      ],
      "connections": {
        "active": 150,
        "listening": 10,
        "time_wait": 25,
        "established": 125
      }
    }
  }
  
  // 验证网络接口数量
  assert_eq(network_metrics["network"]["interfaces"].length(), 2)
  
  // 验证eth0接口指标
  let eth0 = network_metrics["network"]["interfaces"][0]
  assert_eq(eth0["interface"], "eth0")
  assert_eq(eth0["bytes_sent"], 10737418240)
  assert_eq(eth0["bytes_recv"], 21474836480)
  assert_eq(eth0["packets_sent"], 1000000)
  assert_eq(eth0["packets_recv"], 2000000)
  assert_eq(eth0["err_in"], 10)
  assert_eq(eth0["err_out"], 5)
  assert_eq(eth0["drop_in"], 2)
  assert_eq(eth0["drop_out"], 1)
  assert_eq(eth0["speed_mbps"], 1000)
  
  // 计算网络错误率
  let total_packets = eth0["packets_sent"] + eth0["packets_recv"]
  let total_errors = eth0["err_in"] + eth0["err_out"]
  let error_rate = (total_errors as Float) / (total_packets as Float) * 100.0
  
  assert_eq(error_rate.round(), 0.0)
  
  // 计算网络丢包率
  let total_drops = eth0["drop_in"] + eth0["drop_out"]
  let drop_rate = (total_drops as Float) / (total_packets as Float) * 100.0
  
  assert_eq(drop_rate.round(), 0.0)
  
  // 计算平均数据包大小
  let total_bytes = eth0["bytes_sent"] + eth0["bytes_recv"]
  let avg_packet_size = (total_bytes as Float) / (total_packets as Float)
  
  assert_eq(avg_packet_size.round(), 10737.0)
  
  // 验证网络连接统计
  let connections = network_metrics["network"]["connections"]
  assert_eq(connections["active"], 150)
  assert_eq(connections["listening"], 10)
  assert_eq(connections["time_wait"], 25)
  assert_eq(connections["established"], 125)
  
  // 验证连接状态总和
  let total_connections = connections["active"] + connections["listening"] + 
                         connections["time_wait"] + connections["established"]
  assert_eq(total_connections, 310)
  
  // 网络性能告警检查
  let error_rate_threshold = 0.1  // 0.1%
  let drop_rate_threshold = 0.05  // 0.05%
  
  let network_alert_level = if error_rate > error_rate_threshold || drop_rate > drop_rate_threshold {
    "warning"
  } else {
    "normal"
  }
  
  assert_eq(network_alert_level, "normal")
}

// 测试5: 应用程序性能监控
test "应用程序性能监控指标收集" {
  // 创建应用程序性能数据
  let app_metrics = {
    "timestamp": 1640995200,
    "application": {
      "name": "api-service",
      "version": "1.2.3",
      "uptime_seconds": 86400,
      "request_metrics": {
        "total": 10000,
        "success": 9500,
        "error": 500,
        "timeout": 0,
        "avg_response_time_ms": 120.5,
        "p50_response_time_ms": 100.0,
        "p95_response_time_ms": 250.0,
        "p99_response_time_ms": 500.0
      },
      "throughput": {
        "requests_per_second": 115.7,
        "bytes_per_second": 10485760  // 10MB/s
      },
      "resource_usage": {
        "cpu_percent": 45.2,
        "memory_mb": 512.5,
        "file_descriptors": 1024,
        "threads": 50,
        "goroutines": 100
      },
      "database_metrics": {
        "connection_pool": {
          "active": 8,
          "idle": 12,
          "max": 20
        },
        "query_stats": {
          "total": 5000,
          "avg_duration_ms": 25.5,
          "slow_queries": 50
        }
      },
      "cache_metrics": {
        "hits": 8000,
        "misses": 2000,
        "hit_rate_percent": 80.0
      }
    }
  }
  
  // 验证应用程序基本信息
  assert_eq(app_metrics["application"]["name"], "api-service")
  assert_eq(app_metrics["application"]["version"], "1.2.3")
  assert_eq(app_metrics["application"]["uptime_seconds"], 86400)
  
  // 验证请求指标
  let request_metrics = app_metrics["application"]["request_metrics"]
  assert_eq(request_metrics["total"], 10000)
  assert_eq(request_metrics["success"], 9500)
  assert_eq(request_metrics["error"], 500)
  assert_eq(request_metrics["timeout"], 0)
  assert_eq(request_metrics["avg_response_time_ms"], 120.5)
  assert_eq(request_metrics["p50_response_time_ms"], 100.0)
  assert_eq(request_metrics["p95_response_time_ms"], 250.0)
  assert_eq(request_metrics["p99_response_time_ms"], 500.0)
  
  // 计算错误率
  let error_rate = (request_metrics["error"] as Float) / 
                  (request_metrics["total"] as Float) * 100.0
  
  assert_eq(error_rate, 5.0)
  
  // 验证吞吐量指标
  let throughput = app_metrics["application"]["throughput"]
  assert_eq(throughput["requests_per_second"], 115.7)
  assert_eq(throughput["bytes_per_second"], 10485760)
  
  // 验证资源使用指标
  let resource_usage = app_metrics["application"]["resource_usage"]
  assert_eq(resource_usage["cpu_percent"], 45.2)
  assert_eq(resource_usage["memory_mb"], 512.5)
  assert_eq(resource_usage["file_descriptors"], 1024)
  assert_eq(resource_usage["threads"], 50)
  assert_eq(resource_usage["goroutines"], 100)
  
  // 验证数据库连接池
  let connection_pool = app_metrics["application"]["database_metrics"]["connection_pool"]
  assert_eq(connection_pool["active"], 8)
  assert_eq(connection_pool["idle"], 12)
  assert_eq(connection_pool["max"], 20)
  
  // 计算连接池使用率
  let pool_usage_rate = (connection_pool["active"] as Float) / 
                       (connection_pool["max"] as Float) * 100.0
  
  assert_eq(pool_usage_rate, 40.0)
  
  // 验证数据库查询统计
  let query_stats = app_metrics["application"]["database_metrics"]["query_stats"]
  assert_eq(query_stats["total"], 5000)
  assert_eq(query_stats["avg_duration_ms"], 25.5)
  assert_eq(query_stats["slow_queries"], 50)
  
  // 计算慢查询率
  let slow_query_rate = (query_stats["slow_queries"] as Float) / 
                       (query_stats["total"] as Float) * 100.0
  
  assert_eq(slow_query_rate, 1.0)
  
  // 验证缓存指标
  let cache_metrics = app_metrics["application"]["cache_metrics"]
  assert_eq(cache_metrics["hits"], 8000)
  assert_eq(cache_metrics["misses"], 2000)
  assert_eq(cache_metrics["hit_rate_percent"], 80.0)
  
  // 应用程序性能告警检查
  let error_rate_threshold = 5.0
  let avg_response_time_threshold = 500.0
  let p95_response_time_threshold = 1000.0
  let cpu_threshold = 80.0
  let memory_threshold = 1024.0
  let slow_query_threshold = 5.0
  let cache_hit_rate_threshold = 70.0
  
  let app_alerts = []
  
  // 检查错误率
  if error_rate > error_rate_threshold {
    app_alerts = app_alerts.push("High error rate: " + error_rate.to_string() + "%")
  }
  
  // 检查响应时间
  if request_metrics["avg_response_time_ms"] > avg_response_time_threshold {
    app_alerts = app_alerts.push("High average response time: " + 
                                request_metrics["avg_response_time_ms"].to_string() + "ms")
  }
  
  // 检查P95响应时间
  if request_metrics["p95_response_time_ms"] > p95_response_time_threshold {
    app_alerts = app_alerts.push("High P95 response time: " + 
                                request_metrics["p95_response_time_ms"].to_string() + "ms")
  }
  
  // 检查CPU使用率
  if resource_usage["cpu_percent"] > cpu_threshold {
    app_alerts = app_alerts.push("High CPU usage: " + 
                                resource_usage["cpu_percent"].to_string() + "%")
  }
  
  // 检查内存使用
  if resource_usage["memory_mb"] > memory_threshold {
    app_alerts = app_alerts.push("High memory usage: " + 
                                resource_usage["memory_mb"].to_string() + "MB")
  }
  
  // 检查慢查询率
  if slow_query_rate > slow_query_threshold {
    app_alerts = app_alerts.push("High slow query rate: " + 
                                slow_query_rate.to_string() + "%")
  }
  
  // 检查缓存命中率
  if cache_metrics["hit_rate_percent"] < cache_hit_rate_threshold {
    app_alerts = app_alerts.push("Low cache hit rate: " + 
                                cache_metrics["hit_rate_percent"].to_string() + "%")
  }
  
  // 验证没有告警
  assert_eq(app_alerts.length(), 0)
}

// 测试6: 性能基准测试
test "性能基准测试和比较" {
  // 创建基准测试数据
  let benchmark_results = {
    "test_name": "api_endpoint_performance",
    "timestamp": 1640995200,
    "test_duration_seconds": 60,
    "concurrent_users": 100,
    "results": [
      {
        "endpoint": "/api/users",
        "avg_response_time_ms": 120.5,
        "min_response_time_ms": 50.0,
        "max_response_time_ms": 500.0,
        "p50_response_time_ms": 100.0,
        "p95_response_time_ms": 250.0,
        "p99_response_time_ms": 450.0,
        "requests_per_second": 833.3,
        "total_requests": 50000,
        "error_rate_percent": 0.1,
        "throughput_mbps": 10.5
      },
      {
        "endpoint": "/api/orders",
        "avg_response_time_ms": 250.8,
        "min_response_time_ms": 100.0,
        "max_response_time_ms": 1000.0,
        "p50_response_time_ms": 200.0,
        "p95_response_time_ms": 500.0,
        "p99_response_time_ms": 800.0,
        "requests_per_second": 400.0,
        "total_requests": 24000,
        "error_rate_percent": 0.2,
        "throughput_mbps": 15.2
      },
      {
        "endpoint": "/api/products",
        "avg_response_time_ms": 180.3,
        "min_response_time_ms": 80.0,
        "max_response_time_ms": 750.0,
        "p50_response_time_ms": 150.0,
        "p95_response_time_ms": 400.0,
        "p99_response_time_ms": 650.0,
        "requests_per_second": 555.6,
        "total_requests": 33336,
        "error_rate_percent": 0.15,
        "throughput_mbps": 12.8
      }
    ]
  }
  
  // 验证基准测试基本信息
  assert_eq(benchmark_results["test_name"], "api_endpoint_performance")
  assert_eq(benchmark_results["timestamp"], 1640995200)
  assert_eq(benchmark_results["test_duration_seconds"], 60)
  assert_eq(benchmark_results["concurrent_users"], 100)
  
  // 验证端点数量
  assert_eq(benchmark_results["results"].length(), 3)
  
  // 验证第一个端点结果
  let users_endpoint = benchmark_results["results"][0]
  assert_eq(users_endpoint["endpoint"], "/api/users")
  assert_eq(users_endpoint["avg_response_time_ms"], 120.5)
  assert_eq(users_endpoint["min_response_time_ms"], 50.0)
  assert_eq(users_endpoint["max_response_time_ms"], 500.0)
  assert_eq(users_endpoint["p50_response_time_ms"], 100.0)
  assert_eq(users_endpoint["p95_response_time_ms"], 250.0)
  assert_eq(users_endpoint["p99_response_time_ms"], 450.0)
  assert_eq(users_endpoint["requests_per_second"], 833.3)
  assert_eq(users_endpoint["total_requests"], 50000)
  assert_eq(users_endpoint["error_rate_percent"], 0.1)
  assert_eq(users_endpoint["throughput_mbps"], 10.5)
  
  // 找出响应时间最快的端点
  let fastest_endpoint = benchmark_results["results"].reduce(fn(acc, result) {
    if result["avg_response_time_ms"] < acc["avg_response_time_ms"] { result } else { acc }
  }, benchmark_results["results"][0])
  
  assert_eq(fastest_endpoint["endpoint"], "/api/users")
  assert_eq(fastest_endpoint["avg_response_time_ms"], 120.5)
  
  // 找出吞吐量最高的端点
  let highest_throughput = benchmark_results["results"].reduce(fn(acc, result) {
    if result["requests_per_second"] > acc["requests_per_second"] { result } else { acc }
  }, benchmark_results["results"][0])
  
  assert_eq(highest_throughput["endpoint"], "/api/users")
  assert_eq(highest_throughput["requests_per_second"], 833.3)
  
  // 计算总体平均响应时间
  let total_requests = benchmark_results["results"].reduce(fn(acc, result) {
    acc + result["total_requests"]
  }, 0)
  
  let weighted_avg_response_time = benchmark_results["results"].reduce(fn(acc, result) {
    acc + (result["avg_response_time_ms"] * (result["total_requests"] as Float))
  }, 0.0) / (total_requests as Float)
  
  assert_eq(weighted_avg_response_time.round(), 177.0)
  
  // 计算总体错误率
  let total_errors = benchmark_results["results"].reduce(fn(acc, result) {
    acc + (result["total_requests"] as Float * result["error_rate_percent"] / 100.0)
  }, 0.0)
  
  let overall_error_rate = (total_errors / (total_requests as Float)) * 100.0
  assert_eq(overall_error_rate.round(), 0.15)
  
  // 性能基准比较
  let performance_baseline = {
    "/api/users": {"avg_response_time_ms": 100.0, "requests_per_second": 1000.0},
    "/api/orders": {"avg_response_time_ms": 200.0, "requests_per_second": 500.0},
    "/api/products": {"avg_response_time_ms": 150.0, "requests_per_second": 600.0}
  }
  
  // 比较实际结果与基准
  let performance_comparisons = benchmark_results["results"].map(fn(result) {
    let endpoint = result["endpoint"]
    let baseline = performance_baseline[endpoint]
    
    let response_time_diff = result["avg_response_time_ms"] - baseline["avg_response_time_ms"]
    let response_time_change = (response_time_diff / baseline["avg_response_time_ms"]) * 100.0
    
    let throughput_diff = result["requests_per_second"] - baseline["requests_per_second"]
    let throughput_change = (throughput_diff / baseline["requests_per_second"]) * 100.0
    
    {
      "endpoint": endpoint,
      "response_time_change_percent": response_time_change,
      "throughput_change_percent": throughput_change
    }
  })
  
  // 验证性能比较结果
  assert_eq(performance_comparisons.length(), 3)
  
  // 验证用户端点性能变化
  let users_comparison = performance_comparisons[0]
  assert_eq(users_comparison["endpoint"], "/api/users")
  assert_eq(users_comparison["response_time_change_percent"].round(), 21.0) // 20.5% increase
  assert_eq(users_comparison["throughput_change_percent"].round(), -17.0) // -16.67% decrease
}