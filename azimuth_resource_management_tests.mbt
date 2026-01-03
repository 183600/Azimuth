// Azimuth 资源管理测试用例
// 专注于系统资源的分配、监控、优化和释放功能

// 测试1: 内存资源管理
test "内存资源分配和管理" {
  // 模拟内存资源池
  let memory_pool = {
    "total_memory_mb": 8192,  // 8GB
    "allocated_memory_mb": 4096,  // 4GB已分配
    "available_memory_mb": 4096,  // 4GB可用
    "reserved_memory_mb": 1024,  // 1GB保留
    "allocations": [
      {
        "id": "alloc-001",
        "process_id": "api-service",
        "size_mb": 1024,
        "type": "heap",
        "allocated_at": 1640995200,
        "priority": "high"
      },
      {
        "id": "alloc-002",
        "process_id": "database",
        "size_mb": 2048,
        "type": "buffer",
        "allocated_at": 1640995210,
        "priority": "critical"
      },
      {
        "id": "alloc-003",
        "process_id": "cache-service",
        "size_mb": 1024,
        "type": "cache",
        "allocated_at": 1640995220,
        "priority": "medium"
      }
    ],
    "fragmentation_percent": 15.2
  }
  
  // 验证内存池状态
  assert_eq(memory_pool["total_memory_mb"], 8192)
  assert_eq(memory_pool["allocated_memory_mb"], 4096)
  assert_eq(memory_pool["available_memory_mb"], 4096)
  assert_eq(memory_pool["reserved_memory_mb"], 1024)
  assert_eq(memory_pool["allocations"].length(), 3)
  assert_eq(memory_pool["fragmentation_percent"], 15.2)
  
  // 计算内存使用率
  let memory_usage_percent = (memory_pool["allocated_memory_mb"] as Float) / 
                            (memory_pool["total_memory_mb"] as Float) * 100.0
  
  assert_eq(memory_usage_percent, 50.0)
  
  // 计算可用内存百分比 (排除保留内存)
  let usable_memory_mb = memory_pool["total_memory_mb"] - memory_pool["reserved_memory_mb"]
  let usable_available_mb = memory_pool["available_memory_mb"] - memory_pool["reserved_memory_mb"]
  let usable_usage_percent = ((usable_memory_mb - usable_available_mb) as Float) / 
                            (usable_memory_mb as Float) * 100.0
  
  assert_eq(usable_usage_percent.round(), 57.0)
  
  // 按优先级分类内存分配
  let allocations_by_priority = {
    "critical": memory_pool["allocations"].filter(fn(alloc) { alloc["priority"] == "critical" }),
    "high": memory_pool["allocations"].filter(fn(alloc) { alloc["priority"] == "high" }),
    "medium": memory_pool["allocations"].filter(fn(alloc) { alloc["priority"] == "medium" }),
    "low": memory_pool["allocations"].filter(fn(alloc) { alloc["priority"] == "low" })
  }
  
  assert_eq(allocations_by_priority["critical"].length(), 1)
  assert_eq(allocations_by_priority["high"].length(), 1)
  assert_eq(allocations_by_priority["medium"].length(), 1)
  assert_eq(allocations_by_priority["low"].length(), 0)
  
  // 实现内存分配策略
  let allocate_memory = fn(pool, process_id, size_mb, priority, allocation_type) {
    let available_for_allocation = pool["available_memory_mb"] - pool["reserved_memory_mb"]
    
    if size_mb > available_for_allocation {
      // 内存不足，尝试释放低优先级内存
      let low_priority_allocations = pool["allocations"].filter(fn(alloc) { 
        alloc["priority"] == "low" || alloc["priority"] == "medium" 
      }).sort_by(fn(a, b) { 
        if a["priority"] < b["priority"] { -1 } 
        else if a["priority"] > b["priority"] { 1 } 
        else { 0 } 
      })
      
      let mut freed_memory = 0
      let mut updated_allocations = pool["allocations"]
      
      for alloc in low_priority_allocations {
        if freed_memory >= size_mb { break }
        
        updated_allocations = updated_allocations.filter(fn(a) { a["id"] != alloc["id"] })
        freed_memory = freed_memory + alloc["size_mb"]
      }
      
      if freed_memory >= size_mb {
        // 成功释放足够内存，进行分配
        let new_allocation = {
          "id": "alloc-" + (pool["allocations"].length() + 1).to_string(),
          "process_id": process_id,
          "size_mb": size_mb,
          "type": allocation_type,
          "allocated_at": 1640995300,
          "priority": priority
        }
        
        updated_allocations = updated_allocations.push(new_allocation)
        
        {
          ...pool,
          "allocated_memory_mb": pool["allocated_memory_mb"] - freed_memory + size_mb,
          "available_memory_mb": pool["available_memory_mb"] + freed_memory - size_mb,
          "allocations": updated_allocations
        }
      } else {
        // 无法释放足够内存，分配失败
        pool
      }
    } else {
      // 内存充足，直接分配
      let new_allocation = {
        "id": "alloc-" + (pool["allocations"].length() + 1).to_string(),
        "process_id": process_id,
        "size_mb": size_mb,
        "type": allocation_type,
        "allocated_at": 1640995300,
        "priority": priority
      }
      
      let updated_allocations = pool["allocations"].push(new_allocation)
      
      {
        ...pool,
        "allocated_memory_mb": pool["allocated_memory_mb"] + size_mb,
        "available_memory_mb": pool["available_memory_mb"] - size_mb,
        "allocations": updated_allocations
      }
    }
  }
  
  // 测试内存分配
  let updated_pool = allocate_memory(memory_pool, "log-processor", 512, "low", "buffer")
  
  // 验证分配结果
  assert_eq(updated_pool["allocations"].length(), 4)
  assert_eq(updated_pool["allocated_memory_mb"], 4608) // 4096 + 512
  assert_eq(updated_pool["available_memory_mb"], 3584) // 4096 - 512
  
  // 验证新分配
  let new_allocation = updated_pool["allocations"].filter(fn(alloc) { 
    alloc["process_id"] == "log-processor" 
  })[0]
  
  assert_eq(new_allocation["size_mb"], 512)
  assert_eq(new_allocation["priority"], "low")
  assert_eq(new_allocation["type"], "buffer")
}

// 测试2: CPU资源管理
test "CPU资源分配和调度" {
  // 模拟CPU资源管理
  let cpu_resources = {
    "total_cores": 8,
    "available_cores": 2,
    "core_utilization": [
      {"core_id": 0, "utilization_percent": 75.5, "processes": ["api-service", "system"]},
      {"core_id": 1, "utilization_percent": 82.3, "processes": ["database", "system"]},
      {"core_id": 2, "utilization_percent": 68.9, "processes": ["cache-service"]},
      {"core_id": 3, "utilization_percent": 91.2, "processes": ["data-processor", "system"]},
      {"core_id": 4, "utilization_percent": 45.8, "processes": []},
      {"core_id": 5, "utilization_percent": 52.1, "processes": ["monitoring"]},
      {"core_id": 6, "utilization_percent": 78.6, "processes": ["web-server", "system"]},
      {"core_id": 7, "utilization_percent": 39.4, "processes": []}
    ],
    "process_queue": [
      {
        "pid": "batch-job-001",
        "priority": "low",
        "cpu_required": 2,
        "estimated_duration_sec": 300,
        "queued_at": 1640995200
      },
      {
        "pid": "urgent-task-002",
        "priority": "high",
        "cpu_required": 1,
        "estimated_duration_sec": 60,
        "queued_at": 1640995210
      }
    ],
    "scheduling_policy": "priority_based"
  }
  
  // 验证CPU资源状态
  assert_eq(cpu_resources["total_cores"], 8)
  assert_eq(cpu_resources["available_cores"], 2)
  assert_eq(cpu_resources["core_utilization"].length(), 8)
  assert_eq(cpu_resources["process_queue"].length(), 2)
  assert_eq(cpu_resources["scheduling_policy"], "priority_based")
  
  // 计算平均CPU利用率
  let avg_utilization = cpu_resources["core_utilization"].reduce(fn(acc, core) {
    acc + core["utilization_percent"]
  }, 0.0) / (cpu_resources["core_utilization"].length() as Float)
  
  assert_eq(avg_utilization.round(), 67.0)
  
  // 找出空闲CPU核心
  let idle_cores = cpu_resources["core_utilization"].filter(fn(core) { 
    core["utilization_percent"] < 50.0 
  })
  
  assert_eq(idle_cores.length(), 3)
  assert_eq(idle_cores[0]["core_id"], 4)
  assert_eq(idle_cores[1]["core_id"], 5)
  assert_eq(idle_cores[2]["core_id"], 7)
  
  // 找出高负载CPU核心
  let high_load_cores = cpu_resources["core_utilization"].filter(fn(core) { 
    core["utilization_percent"] > 80.0 
  })
  
  assert_eq(high_load_cores.length(), 2)
  assert_eq(high_load_cores[0]["core_id"], 1)
  assert_eq(high_load_cores[1]["core_id"], 3)
  
  // 实现CPU调度策略
  let schedule_processes = fn(resources) {
    let updated_queue = resources["process_queue"].sort_by(fn(a, b) {
      if a["priority"] < b["priority"] { -1 }
      else if a["priority"] > b["priority"] { 1 }
      else { 0 }
    })
    
    let mut updated_cores = resources["core_utilization"]
    let mut remaining_queue = []
    let mut scheduled_processes = []
    
    for process in updated_queue {
      let required_cores = process["cpu_required"]
      
      // 找出可用的CPU核心
      let available_cores = updated_cores.filter(fn(core) {
        core["utilization_percent"] < 70.0 && core["processes"].length() == 0
      })
      
      if available_cores.length() >= required_cores {
        // 调度进程到可用核心
        let assigned_cores = available_cores.slice(0, required_cores)
        
        // 更新核心状态
        for i in 0..=(updated_cores.length() - 1) {
          let core = updated_cores[i]
          if assigned_cores.any(fn(assigned) { assigned["core_id"] == core["core_id"] }) {
            updated_cores = updated_cores.update(i, {
              ...core,
              "processes": core["processes"].push(process["pid"]),
              "utilization_percent": core["utilization_percent"] + 20.0
            })
          }
        }
        
        scheduled_processes = scheduled_processes.push(process["pid"])
      } else {
        // 没有足够的核心，保留在队列中
        remaining_queue = remaining_queue.push(process)
      }
    }
    
    {
      ...resources,
      "core_utilization": updated_cores,
      "process_queue": remaining_queue,
      "scheduled_processes": scheduled_processes
    }
  }
  
  // 应用调度策略
  let scheduled_resources = schedule_processes(cpu_resources)
  
  // 验证调度结果
  assert_eq(scheduled_resources["scheduled_processes"].length(), 1)
  assert_eq(scheduled_resources["scheduled_processes"][0], "urgent-task-002")
  assert_eq(scheduled_resources["process_queue"].length(), 1)
  assert_eq(scheduled_resources["process_queue"][0]["pid"], "batch-job-001")
  
  // 验证核心状态更新
  let updated_core = scheduled_resources["core_utilization"].filter(fn(core) {
    core["processes"].contains("urgent-task-002")
  })[0]
  
  assert_true(updated_core["utilization_percent"] > 39.4) // 原始值 + 20.0
  assert_true(updated_core["processes"].contains("urgent-task-002"))
}

// 测试3: 磁盘空间管理
test "磁盘空间分配和清理" {
  // 模拟磁盘空间管理
  let disk_management = {
    "total_space_gb": 500,
    "used_space_gb": 425,
    "free_space_gb": 75,
    "reserved_space_gb": 25,
    "directories": [
      {
        "path": "/var/log",
        "size_gb": 50.5,
        "file_count": 125000,
        "retention_policy": "30d",
        "compressible": true,
        "last_cleanup": 1640995000
      },
      {
        "path": "/tmp",
        "size_gb": 15.2,
        "file_count": 45000,
        "retention_policy": "7d",
        "compressible": false,
        "last_cleanup": 1640995200
      },
      {
        "path": "/app/cache",
        "size_gb": 120.8,
        "file_count": 250000,
        "retention_policy": "7d",
        "compressible": true,
        "last_cleanup": 1640995100
      },
      {
        "path": "/data",
        "size_gb": 200.5,
        "file_count": 50000,
        "retention_policy": "never",
        "compressible": false,
        "last_cleanup": 0
      }
    ],
    "cleanup_policies": {
      "min_free_space_percent": 15,
      "aggressive_cleanup_threshold": 10,
      "compression_ratio_target": 0.6
    }
  }
  
  // 验证磁盘管理状态
  assert_eq(disk_management["total_space_gb"], 500)
  assert_eq(disk_management["used_space_gb"], 425)
  assert_eq(disk_management["free_space_gb"], 75)
  assert_eq(disk_management["reserved_space_gb"], 25)
  assert_eq(disk_management["directories"].length(), 4)
  
  // 计算磁盘使用率
  let disk_usage_percent = (disk_management["used_space_gb"] as Float) / 
                          (disk_management["total_space_gb"] as Float) * 100.0
  
  assert_eq(disk_usage_percent, 85.0)
  
  // 计算可用空间百分比
  let free_space_percent = (disk_management["free_space_gb"] as Float) / 
                          (disk_management["total_space_gb"] as Float) * 100.0
  
  assert_eq(free_space_percent, 15.0)
  
  // 找出可清理的目录
  let cleanable_dirs = disk_management["directories"].filter(fn(dir) { 
    dir["retention_policy"] != "never" 
  })
  
  assert_eq(cleanable_dirs.length(), 3)
  
  // 找出可压缩的目录
  let compressible_dirs = disk_management["directories"].filter(fn(dir) { 
    dir["compressible"] 
  })
  
  assert_eq(compressible_dirs.length(), 2)
  
  // 实现磁盘清理策略
  let cleanup_disk_space = fn(management) {
    let free_space_percent = (management["free_space_gb"] as Float) / 
                            (management["total_space_gb"] as Float) * 100.0
    
    let min_free_percent = management["cleanup_policies"]["min_free_space_percent"]
    let aggressive_threshold = management["cleanup_policies"]["aggressive_cleanup_threshold"]
    
    let mut freed_space_gb = 0.0
    let mut updated_dirs = management["directories"]
    
    if free_space_percent < min_free_percent {
      // 需要清理空间
      let cleanable_dirs = management["directories"].filter(fn(dir) { 
        dir["retention_policy"] != "never" 
      })
      
      // 按清理优先级排序 (临时文件优先，然后是日志，最后是缓存)
      let prioritized_dirs = cleanable_dirs.sort_by(fn(a, b) {
        let priority_a = match a["path"] {
          "/tmp" => 1,
          "/var/log" => 2,
          _ => 3
        }
        
        let priority_b = match b["path"] {
          "/tmp" => 1,
          "/var/log" => 2,
          _ => 3
        }
        
        if priority_a < priority_b { -1 }
        else if priority_a > priority_b { 1 }
        else { 0 }
      })
      
      // 清理目录
      for dir in prioritized_dirs {
        if free_space_percent + (freed_space_gb / management["total_space_gb"] * 100.0) >= min_free_percent {
          break
        }
        
        let cleanup_ratio = if free_space_percent < aggressive_threshold {
          0.8 // 激进清理：删除80%
        } else {
          0.5 // 常规清理：删除50%
        }
        
        let potential_freed = dir["size_gb"] * cleanup_ratio
        
        // 更新目录大小
        for i in 0..=(updated_dirs.length() - 1) {
          if updated_dirs[i]["path"] == dir["path"] {
            updated_dirs = updated_dirs.update(i, {
              ...updated_dirs[i],
              "size_gb": dir["size_gb"] * (1.0 - cleanup_ratio),
              "last_cleanup": 1640995400
            })
            break
          }
        }
        
        freed_space_gb = freed_space_gb + potential_freed
      }
      
      // 如果清理后空间仍然不足，尝试压缩
      let final_free_percent = free_space_percent + (freed_space_gb / management["total_space_gb"] * 100.0)
      
      if final_free_percent < min_free_percent {
        let compressible_dirs = updated_dirs.filter(fn(dir) { dir["compressible"] })
        
        for dir in compressible_dirs {
          if final_free_percent >= min_free_percent { break }
          
          let compression_ratio = management["cleanup_policies"]["compression_ratio_target"]
          let potential_freed = dir["size_gb"] * (1.0 - compression_ratio)
          
          // 更新目录大小 (压缩后)
          for i in 0..=(updated_dirs.length() - 1) {
            if updated_dirs[i]["path"] == dir["path"] {
              updated_dirs = updated_dirs.update(i, {
                ...updated_dirs[i],
                "size_gb": dir["size_gb"] * compression_ratio,
                "last_cleanup": 1640995400
              })
              break
            }
          }
          
          freed_space_gb = freed_space_gb + potential_freed
          final_free_percent = final_free_percent + (potential_freed / management["total_space_gb"] * 100.0)
        }
      }
    }
    
    {
      ...management,
      "used_space_gb": management["used_space_gb"] - freed_space_gb,
      "free_space_gb": management["free_space_gb"] + freed_space_gb,
      "directories": updated_dirs,
      "last_cleanup": 1640995400
    }
  }
  
  // 应用清理策略
  let cleaned_disk = cleanup_disk_space(disk_management)
  
  // 验证清理结果
  assert_true(cleaned_disk["free_space_gb"] > disk_management["free_space_gb"])
  assert_true(cleaned_disk["used_space_gb"] < disk_management["used_space_gb"])
  assert_eq(cleaned_disk["last_cleanup"], 1640995400)
  
  // 验证目录清理
  let cleaned_tmp = cleaned_disk["directories"].filter(fn(dir) { 
    dir["path"] == "/tmp" 
  })[0]
  
  assert_true(cleaned_tmp["size_gb"] < disk_management["directories"][1]["size_gb"])
  assert_eq(cleaned_tmp["last_cleanup"], 1640995400)
  
  // 验证最终可用空间百分比
  let final_free_percent = (cleaned_disk["free_space_gb"] as Float) / 
                          (cleaned_disk["total_space_gb"] as Float) * 100.0
  
  assert_true(final_free_percent >= disk_management["cleanup_policies"]["min_free_space_percent"])
}

// 测试4: 网络带宽管理
test "网络带宽分配和控制" {
  // 模拟网络带宽管理
  let network_bandwidth = {
    "total_bandwidth_mbps": 1000,
    "allocated_bandwidth_mbps": 750,
    "available_bandwidth_mbps": 250,
    "reserved_bandwidth_mbps": 100,
    "services": [
      {
        "name": "api-service",
        "allocated_bandwidth_mbps": 300,
        "current_usage_mbps": 225,
        "priority": "high",
        "min_required_mbps": 100,
        "max_allowed_mbps": 500
      },
      {
        "name": "database-replication",
        "allocated_bandwidth_mbps": 200,
        "current_usage_mbps": 180,
        "priority": "critical",
        "min_required_mbps": 150,
        "max_allowed_mbps": 400
      },
      {
        "name": "backup-service",
        "allocated_bandwidth_mbps": 150,
        "current_usage_mbps": 120,
        "priority": "low",
        "min_required_mbps": 50,
        "max_allowed_mbps": 300
      },
      {
        "name": "monitoring",
        "allocated_bandwidth_mbps": 100,
        "current_usage_mbps": 45,
        "priority": "medium",
        "min_required_mbps": 25,
        "max_allowed_mbps": 150
      }
    ],
    "qos_policies": {
      "enable_traffic_shaping": true,
      "burst_allowance_percent": 20,
      "priority_preemption": true
    }
  }
  
  // 验证网络带宽状态
  assert_eq(network_bandwidth["total_bandwidth_mbps"], 1000)
  assert_eq(network_bandwidth["allocated_bandwidth_mbps"], 750)
  assert_eq(network_bandwidth["available_bandwidth_mbps"], 250)
  assert_eq(network_bandwidth["reserved_bandwidth_mbps"], 100)
  assert_eq(network_bandwidth["services"].length(), 4)
  
  // 计算带宽使用率
  let bandwidth_usage_percent = (network_bandwidth["allocated_bandwidth_mbps"] as Float) / 
                               (network_bandwidth["total_bandwidth_mbps"] as Float) * 100.0
  
  assert_eq(bandwidth_usage_percent, 75.0)
  
  // 计算实际使用率
  let actual_usage_mbps = network_bandwidth["services"].reduce(fn(acc, service) {
    acc + service["current_usage_mbps"]
  }, 0)
  
  let actual_usage_percent = (actual_usage_mbps as Float) / 
                            (network_bandwidth["total_bandwidth_mbps"] as Float) * 100.0
  
  assert_eq(actual_usage_percent.round(), 57.0)
  
  // 按优先级分类服务
  let services_by_priority = {
    "critical": network_bandwidth["services"].filter(fn(service) { service["priority"] == "critical" }),
    "high": network_bandwidth["services"].filter(fn(service) { service["priority"] == "high" }),
    "medium": network_bandwidth["services"].filter(fn(service) { service["priority"] == "medium" }),
    "low": network_bandwidth["services"].filter(fn(service) { service["priority"] == "low" })
  }
  
  assert_eq(services_by_priority["critical"].length(), 1)
  assert_eq(services_by_priority["high"].length(), 1)
  assert_eq(services_by_priority["medium"].length(), 1)
  assert_eq(services_by_priority["low"].length(), 1)
  
  // 实现带宽动态调整策略
  let adjust_bandwidth_allocation = fn(bandwidth) {
    let available_for_reallocation = bandwidth["available_bandwidth_mbps"]
    let mut updated_services = bandwidth["services"]
    
    // 检查是否需要调整带宽分配
    let services_needing_adjustment = updated_services.filter(fn(service) {
      let usage_ratio = (service["current_usage_mbps"] as Float) / 
                       (service["allocated_bandwidth_mbps"] as Float)
      
      // 使用率超过90%或低于50%的服务需要调整
      usage_ratio > 0.9 || usage_ratio < 0.5
    })
    
    if services_needing_adjustment.length() > 0 && available_for_reallocation > 0 {
      // 按优先级排序需要调整的服务
      let prioritized_adjustments = services_needing_adjustment.sort_by(fn(a, b) {
        let priority_a = match a["priority"] {
          "critical" => 1,
          "high" => 2,
          "medium" => 3,
          "low" => 4,
          _ => 5
        }
        
        let priority_b = match b["priority"] {
          "critical" => 1,
          "high" => 2,
          "medium" => 3,
          "low" => 4,
          _ => 5
        }
        
        if priority_a < priority_b { -1 }
        else if priority_a > priority_b { 1 }
        else { 0 }
      })
      
      // 调整带宽分配
      let mut remaining_bandwidth = available_for_reallocation
      
      for service in prioritized_adjustments {
        if remaining_bandwidth <= 0 { break }
        
        let usage_ratio = (service["current_usage_mbps"] as Float) / 
                         (service["allocated_bandwidth_mbps"] as Float)
        
        let adjustment = if usage_ratio > 0.9 {
          // 增加带宽分配
          let potential_increase = service["max_allowed_mbps"] - service["allocated_bandwidth_mbps"]
          let recommended_increase = (service["allocated_bandwidth_mbps"] as Float) * 0.3
          let actual_increase = if recommended_increase > remaining_bandwidth {
            remaining_bandwidth
          } else if recommended_increase > potential_increase {
            potential_increase
          } else {
            recommended_increase as Int
          }
          
          actual_increase
        } else {
          // 减少带宽分配
          let potential_decrease = service["allocated_bandwidth_mbps"] - service["min_required_mbps"]
          let recommended_decrease = (service["allocated_bandwidth_mbps"] as Float) * 0.2
          let actual_decrease = if recommended_decrease > potential_decrease {
            potential_decrease
          } else {
            recommended_decrease as Int
          }
          
          -actual_decrease // 负数表示减少
        }
        
        if adjustment != 0 {
          // 更新服务带宽分配
          for i in 0..=(updated_services.length() - 1) {
            if updated_services[i]["name"] == service["name"] {
              updated_services = updated_services.update(i, {
                ...updated_services[i],
                "allocated_bandwidth_mbps": updated_services[i]["allocated_bandwidth_mbps"] + adjustment
              })
              break
            }
          }
          
          remaining_bandwidth = remaining_bandwidth - adjustment
        }
      }
    }
    
    // 重新计算总分配带宽
    let new_allocated_bandwidth = updated_services.reduce(fn(acc, service) {
      acc + service["allocated_bandwidth_mbps"]
    }, 0)
    
    {
      ...bandwidth,
      "services": updated_services,
      "allocated_bandwidth_mbps": new_allocated_bandwidth,
      "available_bandwidth_mbps": bandwidth["total_bandwidth_mbps"] - new_allocated_bandwidth,
      "last_adjustment": 1640995400
    }
  }
  
  // 应用带宽调整策略
  let adjusted_bandwidth = adjust_bandwidth_allocation(network_bandwidth)
  
  // 验证调整结果
  assert_eq(adjusted_bandwidth["last_adjustment"], 1640995400)
  
  // 验证总分配带宽不超过总带宽
  assert_true(adjusted_bandwidth["allocated_bandwidth_mbps"] <= adjusted_bandwidth["total_bandwidth_mbps"])
  
  // 验证高优先级服务获得足够带宽
  let critical_service = adjusted_bandwidth["services"].filter(fn(service) { 
    service["priority"] == "critical" 
  })[0]
  
  assert_true(critical_service["allocated_bandwidth_mbps"] >= critical_service["min_required_mbps"])
  assert_true(critical_service["allocated_bandwidth_mbps"] <= critical_service["max_allowed_mbps"])
}

// 测试5: 连接池管理
test "连接池资源管理" {
  // 模拟数据库连接池
  let connection_pool = {
    "pool_name": "database-connection-pool",
    "max_connections": 20,
    "min_connections": 5,
    "active_connections": 12,
    "idle_connections": 5,
    "creating_connections": 1,
    "connections": [
      {
        "id": "conn-001",
        "status": "active",
        "created_at": 1640995000,
        "last_used_at": 1640995200,
        "usage_count": 125,
        "process_id": "api-service-001"
      },
      {
        "id": "conn-002",
        "status": "active",
        "created_at": 1640995010,
        "last_used_at": 1640995190,
        "usage_count": 98,
        "process_id": "api-service-002"
      },
      {
        "id": "conn-003",
        "status": "idle",
        "created_at": 1640995020,
        "last_used_at": 1640995150,
        "usage_count": 76,
        "process_id": None
      },
      {
        "id": "conn-004",
        "status": "idle",
        "created_at": 1640995030,
        "last_used_at": 1640995180,
        "usage_count": 54,
        "process_id": None
      },
      {
        "id": "conn-005",
        "status": "creating",
        "created_at": 1640995210,
        "last_used_at": 0,
        "usage_count": 0,
        "process_id": None
      }
    ],
    "config": {
      "connection_timeout_ms": 30000,
      "idle_timeout_ms": 300000,
      "max_lifetime_ms": 3600000,
      "health_check_interval_ms": 60000
    }
  }
  
  // 验证连接池状态
  assert_eq(connection_pool["pool_name"], "database-connection-pool")
  assert_eq(connection_pool["max_connections"], 20)
  assert_eq(connection_pool["min_connections"], 5)
  assert_eq(connection_pool["active_connections"], 12)
  assert_eq(connection_pool["idle_connections"], 5)
  assert_eq(connection_pool["creating_connections"], 1)
  assert_eq(connection_pool["connections"].length(), 5)
  
  // 计算连接池使用率
  let total_connections = connection_pool["active_connections"] + 
                         connection_pool["idle_connections"] + 
                         connection_pool["creating_connections"]
  
  let utilization_rate = (connection_pool["active_connections"] as Float) / 
                        (connection_pool["max_connections"] as Float) * 100.0
  
  assert_eq(utilization_rate, 60.0)
  
  // 按状态分类连接
  let connections_by_status = {
    "active": connection_pool["connections"].filter(fn(conn) { conn["status"] == "active" }),
    "idle": connection_pool["connections"].filter(fn(conn) { conn["status"] == "idle" }),
    "creating": connection_pool["connections"].filter(fn(conn) { conn["status"] == "creating" })
  }
  
  assert_eq(connections_by_status["active"].length(), 2)
  assert_eq(connections_by_status["idle"].length(), 2)
  assert_eq(connections_by_status["creating"].length(), 1)
  
  // 实现连接池管理策略
  let manage_connection_pool = fn(pool, current_time) {
    let idle_timeout = pool["config"]["idle_timeout_ms"]
    let max_lifetime = pool["config"]["max_lifetime_ms"]
    
    // 1. 清理超时的空闲连接
    let mut updated_connections = pool["connections"].filter(fn(conn) {
      if conn["status"] == "idle" {
        let idle_time = current_time - conn["last_used_at"]
        idle_time < idle_timeout
      } else {
        true
      }
    })
    
    // 2. 清理超过最大生命周期的连接
    updated_connections = updated_connections.filter(fn(conn) {
      let lifetime = current_time - conn["created_at"]
      lifetime < max_lifetime
    })
    
    // 3. 计算当前连接数
    let current_active_count = updated_connections.filter(fn(conn) { 
      conn["status"] == "active" 
    }).length()
    
    let current_idle_count = updated_connections.filter(fn(conn) { 
      conn["status"] == "idle" 
    }).length()
    
    let current_creating_count = updated_connections.filter(fn(conn) { 
      conn["status"] == "creating" 
    }).length()
    
    let total_current = current_active_count + current_idle_count + current_creating_count
    
    // 4. 确保最小连接数
    let mut final_connections = updated_connections
    
    if total_current < pool["min_connections"] {
      let connections_to_add = pool["min_connections"] - total_current
      
      for i in 0..=(connections_to_add - 1) {
        let new_connection = {
          "id": "conn-" + (pool["connections"].length() + i + 1).to_string(),
          "status": "idle",
          "created_at": current_time,
          "last_used_at": 0,
          "usage_count": 0,
          "process_id": None
        }
        
        final_connections = final_connections.push(new_connection)
      }
    }
    
    // 5. 更新连接池状态
    let new_active_count = final_connections.filter(fn(conn) { 
      conn["status"] == "active" 
    }).length()
    
    let new_idle_count = final_connections.filter(fn(conn) { 
      conn["status"] == "idle" 
    }).length()
    
    let new_creating_count = final_connections.filter(fn(conn) { 
      conn["status"] == "creating" 
    }).length()
    
    {
      ...pool,
      "active_connections": new_active_count,
      "idle_connections": new_idle_count,
      "creating_connections": new_creating_count,
      "connections": final_connections,
      "last_maintenance": current_time
    }
  }
  
  // 应用连接池管理策略
  let managed_pool = manage_connection_pool(connection_pool, 1640995400)
  
  // 验证管理结果
  assert_eq(managed_pool["last_maintenance"], 1640995400)
  
  // 验证最小连接数
  let total_after_management = managed_pool["active_connections"] + 
                              managed_pool["idle_connections"] + 
                              managed_pool["creating_connections"]
  
  assert_true(total_after_management >= connection_pool["min_connections"])
  
  // 验证连接总数不超过最大连接数
  assert_true(total_after_management <= connection_pool["max_connections"])
}