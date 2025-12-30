// 遥测数据缓存测试用例

test "telemetry_memory_cache_performance" {
  // 测试遥测内存缓存性能
  
  let cache_configurations = [
    {
      "cache_name": "hot_metrics_cache",
      "cache_type": "lru",
      "max_size_mb": 100,
      "ttl_seconds": 300,
      "expected_hit_rate": 0.85
    },
    {
      "cache_name": "trace_data_cache",
      "cache_type": "lfu",
      "max_size_mb": 500,
      "ttl_seconds": 1800,
      "expected_hit_rate": 0.75
    },
    {
      "cache_name": "log_entries_cache",
      "cache_type": "fifo",
      "max_size_mb": 200,
      "ttl_seconds": 600,
      "expected_hit_rate": 0.65
    },
    {
      "cache_name": "aggregated_metrics_cache",
      "cache_type": "lru",
      "max_size_mb": 50,
      "ttl_seconds": 3600,
      "expected_hit_rate": 0.90
    }
  ]
  
  // 验证缓存配置
  assert_eq(cache_configurations.length(), 4)
  
  // 模拟内存缓存操作
  let mut cache_performance_results = []
  
  for config in cache_configurations {
    let cache_name = config["cache_name"]
    let cache_type = config["cache_type"]
    let max_size_mb = config["max_size_mb"]
    let ttl_seconds = config["ttl_seconds"]
    let expected_hit_rate = config["expected_hit_rate"]
    
    // 模拟缓存操作
    let operation_count = 10000
    let mut cache_hits = 0
    let mut cache_misses = 0
    let mut evictions = 0
    
    // 模拟缓存项大小
    let avg_item_size_bytes = match cache_name {
      "hot_metrics_cache" => 1024,      // 1KB
      "trace_data_cache" => 4096,       // 4KB
      "log_entries_cache" => 512,       // 512B
      "aggregated_metrics_cache" => 256, // 256B
      _ => 1024
    }
    
    let max_items = (max_size_mb * 1024 * 1024) / avg_item_size_bytes
    let mut current_size = 0
    
    // 模拟缓存操作序列
    let mut i = 0
    while i < operation_count {
      let key = "key_" + (i % (max_items * 2)).to_string()  // 2倍的项目空间，确保有重复访问
      let access_time = 1641018000000L + (i * 1000)
      
      // 模拟缓存查找
      let cache_hit = if i > max_items / 2 {
        // 在缓存填充后，有更高的命中率
        (i * 7) % 100 < (expected_hit_rate * 100.0)
      } else {
        // 缓存填充阶段，命中率较低
        (i * 7) % 100 < 20.0
      }
      
      if cache_hit {
        cache_hits = cache_hits + 1
      } else {
        cache_misses = cache_misses + 1
        
        // 模拟缓存插入
        if current_size < max_items {
          current_size = current_size + 1
        } else {
          // 模拟缓存淘汰
          evictions = evictions + 1
          
          // 不同淘汰策略的行为
          match cache_type {
            "lru" => {
              // LRU: 淘汰最近最少使用的项
              current_size = max_items - 1
            }
            "lfu" => {
              // LFU: 淘汰最少使用的项
              current_size = max_items - 1
            }
            "fifo" => {
              // FIFO: 淘汰最早插入的项
              current_size = max_items - 1
            }
            _ => {
              current_size = max_items - 1
            }
          }
        }
      }
      
      i = i + 1
    }
    
    // 计算性能指标
    let actual_hit_rate = cache_hits.to_double() / operation_count.to_double()
    let miss_rate = cache_misses.to_double() / operation_count.to_double()
    let eviction_rate = evictions.to_double() / operation_count.to_double()
    
    // 模拟内存使用情况
    let memory_usage_mb = (current_size * avg_item_size_bytes).to_double() / (1024.0 * 1024.0)
    let memory_efficiency = memory_usage_mb / max_size_mb.to_double()
    
    // 模拟延迟
    let hit_latency_us = 10.0  // 缓存命中延迟
    let miss_latency_us = 1000.0  // 缓存未命中延迟（需要从后端获取）
    let avg_latency_us = hit_latency_us * actual_hit_rate + miss_latency_us * miss_rate
    
    cache_performance_results.push((
      cache_name,
      cache_type,
      max_items,
      cache_hits,
      cache_misses,
      evictions,
      actual_hit_rate,
      miss_rate,
      eviction_rate,
      memory_usage_mb,
      memory_efficiency,
      avg_latency_us
    ))
  }
  
  // 验证缓存性能结果
  assert_eq(cache_performance_results.length(), 4)
  
  // 验证热指标缓存
  let hot_metrics_result = cache_performance_results[0]
  assert_eq(hot_metrics_result.0, "hot_metrics_cache")
  assert_eq(hot_metrics_result.1, "lru")
  assert_eq(hot_metrics_result.6 > 0.8, true)  // 实际命中率应该接近预期
  assert_eq(hot_metrics_result.10 <= 1.0, true)  // 内存效率应该不超过100%
  
  // 验证聚合指标缓存（应该有最高命中率）
  let aggregated_metrics_result = cache_performance_results[3]
  assert_eq(aggregated_metrics_result.0, "aggregated_metrics_cache")
  assert_eq(aggregated_metrics_result.6 > hot_metrics_result.6, true)  // 命中率应该更高
  
  // 验证所有缓存的性能指标合理
  for result in cache_performance_results {
    assert_eq(result.2 > 0, true)      // 最大项目数应该大于0
    assert_eq(result.3 >= 0, true)      // 缓存命中数应该非负
    assert_eq(result.4 >= 0, true)      // 缓存未命中数应该非负
    assert_eq(result.5 >= 0, true)      // 淘汰数应该非负
    assert_eq(result.6 >= 0.0, true)    // 命中率应该非负
    assert_eq(result.6 <= 1.0, true)    // 命中率应该不超过100%
    assert_eq(result.11 > 0.0, true)    // 平均延迟应该大于0
  }
}

test "telemetry_distributed_cache_consistency" {
  // 测试遥测分布式缓存一致性
  
  let distributed_cache_nodes = [
    {
      "node_id": "node_001",
      "region": "us-east-1",
      "data_center": "dc1",
      "cache_size_mb": 1024,
      "replication_factor": 2
    },
    {
      "node_id": "node_002",
      "region": "us-east-1",
      "data_center": "dc2",
      "cache_size_mb": 1024,
      "replication_factor": 2
    },
    {
      "node_id": "node_003",
      "region": "us-west-1",
      "data_center": "dc3",
      "cache_size_mb": 1024,
      "replication_factor": 2
    },
    {
      "node_id": "node_004",
      "region": "eu-west-1",
      "data_center": "dc4",
      "cache_size_mb": 1024,
      "replication_factor": 2
    }
  ]
  
  // 验证分布式缓存节点
  assert_eq(distributed_cache_nodes.length(), 4)
  
  // 模拟分布式缓存操作
  let mut consistency_results = []
  
  for node in distributed_cache_nodes {
    let node_id = node["node_id"]
    let region = node["region"]
    let data_center = node["data_center"]
    let replication_factor = node["replication_factor"]
    
    // 模拟缓存写入操作
    let write_operations = 1000
    let mut successful_writes = 0
    let mut replication_successes = 0
    let mut write_conflicts = 0
    
    let mut i = 0
    while i < write_operations {
      let key = "metric_" + i.to_string()
      let value = "value_" + i.to_string()
      let timestamp = 1641018000000L + (i * 1000)
      
      // 模拟写入成功（95%成功率）
      let write_success = (i * 13) % 100 < 95
      
      if write_success {
        successful_writes = successful_writes + 1
        
        // 模拟复制到其他节点
        let mut replication_success_count = 0
        let mut j = 0
        while j < replication_factor {
          // 模拟网络延迟和复制成功（90%成功率）
          let replication_success = (i * 7 + j * 11) % 100 < 90
          if replication_success {
            replication_success_count = replication_success_count + 1
          }
          j = j + 1
        }
        
        if replication_success_count == replication_factor {
          replication_successes = replication_successes + 1
        }
      } else {
        // 模拟写入冲突
        if (i * 17) % 100 < 10 {
          write_conflicts = write_conflicts + 1
        }
      }
      
      i = i + 1
    }
    
    // 模拟缓存读取操作
    let read_operations = 2000
    let mut cache_hits = 0
    let mut cache_misses = 0
    let mut stale_reads = 0
    let mut inconsistency_detected = 0
    
    let mut i = 0
    while i < read_operations {
      let key = "metric_" + (i % write_operations).to_string()
      let read_timestamp = 1641018000000L + (write_operations * 1000) + (i * 500)
      
      // 模拟本地缓存命中（70%命中率）
      let local_hit = (i * 19) % 100 < 70
      
      if local_hit {
        cache_hits = cache_hits + 1
        
        // 检查数据是否过期（5%概率读取到过期数据）
        let is_stale = (i * 23) % 100 < 5
        if is_stale {
          stale_reads = stale_reads + 1
        }
      } else {
        cache_misses = cache_misses + 1
        
        // 模拟从其他节点读取
        let remote_hit = (i * 29) % 100 < 80  // 80%远程命中率
        
        if not remote_hit {
          // 模拟不一致检测
          let inconsistency = (i * 31) % 100 < 2  // 2%不一致概率
          if inconsistency {
            inconsistency_detected = inconsistency_detected + 1
          }
        }
      }
      
      i = i + 1
    }
    
    // 计算一致性指标
    let write_success_rate = successful_writes.to_double() / write_operations.to_double()
    let replication_success_rate = replication_successes.to_double() / successful_writes.to_double()
    let read_hit_rate = cache_hits.to_double() / read_operations.to_double()
    let stale_read_rate = stale_reads.to_double() / cache_hits.to_double()
    let inconsistency_rate = inconsistency_detected.to_double() / read_operations.to_double()
    
    // 模拟网络分区影响
    let network_partition_probability = 0.05  // 5%概率发生网络分区
    let network_partition_affected = (node_id.hash() % 100) < (network_partition_probability * 100.0)
    
    consistency_results.push((
      node_id,
      region,
      data_center,
      write_success_rate,
      replication_success_rate,
      read_hit_rate,
      stale_read_rate,
      inconsistency_rate,
      network_partition_affected
    ))
  }
  
  // 验证一致性结果
  assert_eq(consistency_results.length(), 4)
  
  // 验证所有节点的一致性指标
  for result in consistency_results {
    assert_eq(result.3 > 0.8, true)   // 写入成功率应该大于80%
    assert_eq(result.4 > 0.7, true)   // 复制成功率应该大于70%
    assert_eq(result.5 > 0.5, true)   // 读取命中率应该大于50%
    assert_eq(result.6 < 0.1, true)   // 过期读取率应该小于10%
    assert_eq(result.7 < 0.05, true)  // 不一致率应该小于5%
  }
  
  // 验证不同区域的性能差异
  let us_east_nodes = []
  let us_west_nodes = []
  let eu_west_nodes = []
  
  for result in consistency_results {
    if result.1 == "us-east-1" {
      us_east_nodes.push(result)
    } else if result.1 == "us-west-1" {
      us_west_nodes.push(result)
    } else if result.1 == "eu-west-1" {
      eu_west_nodes.push(result)
    }
  }
  
  assert_eq(us_east_nodes.length(), 2)  // 2个us-east-1节点
  assert_eq(us_west_nodes.length(), 1)  // 1个us-west-1节点
  assert_eq(eu_west_nodes.length(), 1)  // 1个eu-west-1节点
}

test "telemetry_cache_eviction_strategies" {
  // 测试遥测缓存淘汰策略
  
  let eviction_strategies = [
    {
      "strategy_name": "lru",
      "description": "Least Recently Used",
      "memory_efficiency": 0.85,
      "cpu_overhead": "low",
      "hit_rate_optimal": "temporal_locality"
    },
    {
      "strategy_name": "lfu",
      "description": "Least Frequently Used",
      "memory_efficiency": 0.90,
      "cpu_overhead": "medium",
      "hit_rate_optimal": "frequency_locality"
    },
    {
      "strategy_name": "fifo",
      "description": "First In First Out",
      "memory_efficiency": 0.75,
      "cpu_overhead": "very_low",
      "hit_rate_optimal": "simple_workloads"
    },
    {
      "strategy_name": "random",
      "description": "Random Replacement",
      "memory_efficiency": 0.65,
      "cpu_overhead": "very_low",
      "hit_rate_optimal": "uniform_access"
    },
    {
      "strategy_name": "arc",
      "description": "Adaptive Replacement Cache",
      "memory_efficiency": 0.95,
      "cpu_overhead": "high",
      "hit_rate_optimal": "mixed_workloads"
    }
  ]
  
  // 验证淘汰策略
  assert_eq(eviction_strategies.length(), 5)
  
  // 模拟不同访问模式下的淘汰策略性能
  let access_patterns = [
    {
      "pattern_name": "temporal_locality",
      "description": "时间局部性访问模式",
      "hot_keys_percentage": 20,
      "access_distribution": "skewed"
    },
    {
      "pattern_name": "frequency_locality",
      "description": "频率局部性访问模式",
      "hot_keys_percentage": 30,
      "access_distribution": "zipf"
    },
    {
      "pattern_name": "uniform_access",
      "description": "均匀访问模式",
      "hot_keys_percentage": 5,
      "access_distribution": "uniform"
    },
    {
      "pattern_name": "mixed_workloads",
      "description": "混合工作负载",
      "hot_keys_percentage": 15,
      "access_distribution": "mixed"
    }
  ]
  
  // 验证访问模式
  assert_eq(access_patterns.length(), 4)
  
  // 模拟淘汰策略性能测试
  let mut eviction_performance_results = []
  
  for strategy in eviction_strategies {
    let strategy_name = strategy["strategy_name"]
    let memory_efficiency = strategy["memory_efficiency"]
    let cpu_overhead = strategy["cpu_overhead"]
    
    for pattern in access_patterns {
      let pattern_name = pattern["pattern_name"]
      let hot_keys_percentage = pattern["hot_keys_percentage"]
      let access_distribution = pattern["access_distribution"]
      
      // 模拟缓存操作
      let cache_size = 1000
      let total_accesses = 10000
      let mut cache_hits = 0
      let mut cache_misses = 0
      let mut evictions = 0
      
      // 模拟缓存内容
      let mut cache_contents = []
      let mut access_frequency = {}  // key -> frequency
      let mut last_access_time = {}  // key -> timestamp
      let mut insertion_time = {}   // key -> timestamp
      
      let mut i = 0
      while i < total_accesses {
        let timestamp = i.to_long()
        
        // 生成访问键
        let key = match access_distribution {
          "skewed" => {
            // 80%的访问集中在20%的键上
            if (i * 7) % 100 < 80 {
              "hot_" + (i % (cache_size * hot_keys_percentage / 100)).to_string()
            } else {
              "cold_" + (i % (cache_size * 2)).to_string()
            }
          }
          "zipf" => {
            // Zipf分布
            let rank = (i * 13) % cache_size + 1
            "zipf_" + rank.to_string()
          }
          "uniform" => {
            // 均匀分布
            "uniform_" + (i % (cache_size * 2)).to_string()
          }
          "mixed" => {
            // 混合分布
            if i % 3 == 0 {
              "temporal_" + (i % cache_size).to_string()
            } else if i % 3 == 1 {
              "frequency_" + (i % (cache_size * hot_keys_percentage / 100)).to_string()
            } else {
              "random_" + (i % (cache_size * 2)).to_string()
            }
          }
          _ => "key_" + i.to_string()
        }
        
        // 检查缓存命中
        let cache_hit = cache_contents.contains(key)
        
        if cache_hit {
          cache_hits = cache_hits + 1
          
          // 更新访问信息
          access_frequency[key] = access_frequency.get(key, 0) + 1
          last_access_time[key] = timestamp
        } else {
          cache_misses = cache_misses + 1
          
          // 检查是否需要淘汰
          if cache_contents.length() >= cache_size {
            let evict_key = match strategy_name {
              "lru" => {
                // 找到最近最少使用的键
                let mut lru_key = ""
                let mut oldest_time = timestamp
                
                for cached_key in cache_contents {
                  let last_access = last_access_time.get(cached_key, 0L)
                  if last_access < oldest_time {
                    oldest_time = last_access
                    lru_key = cached_key
                  }
                }
                lru_key
              }
              "lfu" => {
                // 找到最少使用的键
                let mut lfu_key = ""
                let mut min_freq = 2147483647
                
                for cached_key in cache_contents {
                  let freq = access_frequency.get(cached_key, 0)
                  if freq < min_freq {
                    min_freq = freq
                    lfu_key = cached_key
                  }
                }
                lfu_key
              }
              "fifo" => {
                // 找到最早插入的键
                let mut fifo_key = ""
                let mut earliest_time = timestamp
                
                for cached_key in cache_contents {
                  let insert_time = insertion_time.get(cached_key, 0L)
                  if insert_time < earliest_time {
                    earliest_time = insert_time
                    fifo_key = cached_key
                  }
                }
                fifo_key
              }
              "random" => {
                // 随机选择一个键
                let random_index = (i * 17) % cache_contents.length()
                cache_contents[random_index]
              }
              "arc" => {
                // 简化的ARC实现：结合LRU和LFU
                if i % 2 == 0 {
                  // 使用LRU
                  let mut lru_key = ""
                  let mut oldest_time = timestamp
                  
                  for cached_key in cache_contents {
                    let last_access = last_access_time.get(cached_key, 0L)
                    if last_access < oldest_time {
                      oldest_time = last_access
                      lru_key = cached_key
                    }
                  }
                  lru_key
                } else {
                  // 使用LFU
                  let mut lfu_key = ""
                  let mut min_freq = 2147483647
                  
                  for cached_key in cache_contents {
                    let freq = access_frequency.get(cached_key, 0)
                    if freq < min_freq {
                      min_freq = freq
                      lfu_key = cached_key
                    }
                  }
                  lfu_key
                }
              }
              _ => {
                // 默认使用LRU
                cache_contents[0]
              }
            }
            
            // 执行淘汰
            if evict_key != "" {
              cache_contents = cache_contents.filter(fn(k) { k != evict_key })
              access_frequency.remove(evict_key)
              last_access_time.remove(evict_key)
              insertion_time.remove(evict_key)
              evictions = evictions + 1
            }
          }
          
          // 插入新键
          cache_contents.push(key)
          access_frequency[key] = 1
          last_access_time[key] = timestamp
          insertion_time[key] = timestamp
        }
        
        i = i + 1
      }
      
      // 计算性能指标
      let hit_rate = cache_hits.to_double() / total_accesses.to_double()
      let miss_rate = cache_misses.to_double() / total_accesses.to_double()
      let eviction_rate = evictions.to_double() / total_accesses.to_double()
      
      // 模拟CPU开销
      let cpu_overhead_factor = match cpu_overhead {
        "very_low" => 1.0,
        "low" => 1.2,
        "medium" => 1.5,
        "high" => 2.0,
        _ => 1.0
      }
      
      let adjusted_hit_rate = hit_rate / cpu_overhead_factor
      
      eviction_performance_results.push((
        strategy_name,
        pattern_name,
        hit_rate,
        miss_rate,
        eviction_rate,
        adjusted_hit_rate,
        cache_hits,
        cache_misses,
        evictions
      ))
    }
  }
  
  // 验证淘汰策略性能结果
  assert_eq(eviction_performance_results.length(), 20)  // 5个策略 × 4个模式
  
  // 验证LRU在时间局部性模式下的表现
  let lru_temporal = eviction_performance_results[0]
  assert_eq(lru_temporal.0, "lru")
  assert_eq(lru_temporal.1, "temporal_locality")
  assert_eq(lru_temporal.2 > 0.7, true)  // 命中率应该较高
  
  // 验证LFU在频率局部性模式下的表现
  let lfu_frequency = eviction_performance_results[5]
  assert_eq(lfu_frequency.0, "lfu")
  assert_eq(lfu_frequency.1, "frequency_locality")
  assert_eq(lfu_frequency.2 > 0.7, true)  // 命中率应该较高
  
  // 验证ARC在混合工作负载下的表现
  let arc_mixed = eviction_performance_results[19]
  assert_eq(arc_mixed.0, "arc")
  assert_eq(arc_mixed.1, "mixed_workloads")
  assert_eq(arc_mixed.2 > 0.6, true)  // 命中率应该较好
  
  // 验证所有策略都有合理的性能
  for result in eviction_performance_results {
    assert_eq(result.2 >= 0.0, true)  // 命中率应该非负
    assert_eq(result.2 <= 1.0, true)  // 命中率应该不超过100%
    assert_eq(result.3 >= 0.0, true)  // 未命中率应该非负
    assert_eq(result.4 >= 0.0, true)  // 淘汰率应该非负
    assert_eq(result.6 >= 0, true)    // 命中数应该非负
    assert_eq(result.7 >= 0, true)    // 未命中数应该非负
    assert_eq(result.8 >= 0, true)    // 淘汰数应该非负
  }
}

test "telemetry_cache_warming_and_preloading" {
  // 测试遥测缓存预热和预加载
  
  let warming_strategies = [
    {
      "strategy_name": "startup_warming",
      "trigger": "application_start",
      "data_source": "historical_hot_data",
      "warming_duration_seconds": 300,
      "target_hit_rate": 0.70
    },
    {
      "strategy_name": "scheduled_warming",
      "trigger": "cron_schedule",
      "data_source": "predictive_analysis",
      "warming_duration_seconds": 180,
      "target_hit_rate": 0.80
    },
    {
      "strategy_name": "demand_based_warming",
      "trigger": "traffic_spike",
      "data_source": "realtime_trends",
      "warming_duration_seconds": 60,
      "target_hit_rate": 0.60
    },
    {
      "strategy_name": "background_warming",
      "trigger": "low_load_periods",
      "data_source": "access_patterns",
      "warming_duration_seconds": 600,
      "target_hit_rate": 0.90
    }
  ]
  
  // 验证预热策略
  assert_eq(warming_strategies.length(), 4)
  
  // 模拟缓存预热过程
  let mut warming_results = []
  
  for strategy in warming_strategies {
    let strategy_name = strategy["strategy_name"]
    let trigger = strategy["trigger"]
    let data_source = strategy["data_source"]
    let warming_duration_seconds = strategy["warming_duration_seconds"]
    let target_hit_rate = strategy["target_hit_rate"]
    
    // 模拟预热数据源
    let data_source_size = match data_source {
      "historical_hot_data" => 10000,
      "predictive_analysis" => 5000,
      "realtime_trends" => 2000,
      "access_patterns" => 15000,
      _ => 5000
    }
    
    // 模拟预热过程
    let warming_start_time = 1641018000000L
    let warming_end_time = warming_start_time + (warming_duration_seconds * 1000)
    
    let mut items_loaded = 0
    let mut loading_errors = 0
    let mut loading_time_ms = 0
    
    // 模拟数据加载
    let mut i = 0
    while i < data_source_size {
      let item_load_time = match data_source {
        "historical_hot_data" => 50,  // 50ms per item
        "predictive_analysis" => 80,  // 80ms per item
        "realtime_trends" => 30,      // 30ms per item
        "access_patterns" => 40,      // 40ms per item
        _ => 50
      }
      
      // 模拟加载成功/失败
      let load_success = (i * 13) % 100 < 95  // 95%成功率
      
      if load_success {
        items_loaded = items_loaded + 1
        loading_time_ms = loading_time_ms + item_load_time
      } else {
        loading_errors = loading_errors + 1
        loading_time_ms = loading_time_ms + item_load_time / 2  // 错误处理时间较短
      }
      
      // 检查是否超时
      let current_time = warming_start_time + loading_time_ms
      if current_time >= warming_end_time {
        break
      }
      
      i = i + 1
    }
    
    // 计算预热效率
    let loading_efficiency = items_loaded.to_double() / data_source_size.to_double()
    let error_rate = loading_errors.to_double() / data_source_size.to_double()
    let actual_warming_duration = loading_time_ms.to_double() / 1000.0
    
    // 模拟预热后的缓存性能
    let post_warming_accesses = 1000
    let mut cache_hits = 0
    
    let mut i = 0
    while i < post_warming_accesses {
      // 预热后的命中率应该更高
      let hit_probability = target_hit_rate + (loading_efficiency - 0.8) * 0.2
      let cache_hit = (i * 19) % 100 < (hit_probability * 100.0)
      
      if cache_hit {
        cache_hits = cache_hits + 1
      }
      
      i = i + 1
    }
    
    let actual_hit_rate = cache_hits.to_double() / post_warming_accesses.to_double()
    
    // 计算预热收益
    let baseline_hit_rate = 0.3  // 假设基准命中率为30%
    let hit_rate_improvement = actual_hit_rate - baseline_hit_rate
    let warming_benefit = hit_rate_improvement * items_loaded.to_double() / 1000.0
    
    warming_results.push((
      strategy_name,
      trigger,
      data_source,
      data_source_size,
      items_loaded,
      loading_errors,
      loading_efficiency,
      error_rate,
      actual_warming_duration,
      actual_hit_rate,
      hit_rate_improvement,
      warming_benefit
    ))
  }
  
  // 验证预热结果
  assert_eq(warming_results.length(), 4)
  
  // 验证启动预热
  let startup_warming = warming_results[0]
  assert_eq(startup_warming.0, "startup_warming")
  assert_eq(startup_warming.1, "application_start")
  assert_eq(startup_warming.2, "historical_hot_data")
  assert_eq(startup_warming.5 > 0, true)  // 应该有一些加载错误
  assert_eq(startup_warming.9 > 0.6, true)  // 实际命中率应该大于60%
  
  // 验证后台预热
  let background_warming = warming_results[3]
  assert_eq(background_warming.0, "background_warming")
  assert_eq(background_warming.1, "low_load_periods")
  assert_eq(background_warming.2, "access_patterns")
  assert_eq(background_warming.4 > startup_warming.4, true)  // 应该加载更多项目
  assert_eq(background_warming.9 > startup_warming.9, true)  // 命中率应该更高
  
  // 验证所有预热策略都有收益
  for result in warming_results {
    assert_eq(result.4 >= 0, true)      // 加载项目数应该非负
    assert_eq(result.5 >= 0, true)      // 加载错误数应该非负
    assert_eq(result.6 >= 0.0, true)    // 加载效率应该非负
    assert_eq(result.6 <= 1.0, true)    // 加载效率应该不超过100%
    assert_eq(result.7 >= 0.0, true)    // 错误率应该非负
    assert_eq(result.9 >= 0.0, true)    // 实际命中率应该非负
    assert_eq(result.10 >= 0.0, true)   // 命中率改善应该非负
    assert_eq(result.11 >= 0.0, true)   // 预热收益应该非负
  }
  
  // 验证不同策略的特点
  let demand_based = warming_results[2]
  assert_eq(demand_based.0, "demand_based_warming")
  assert_eq(demand_based.8 < 120.0, true)  // 实际预热时间应该较短（快速响应）
  
  let scheduled = warming_results[1]
  assert_eq(scheduled.0, "scheduled_warming")
  assert_eq(scheduled.9 > 0.7, true)  // 预测性预热应该有较高命中率
}