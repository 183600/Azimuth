// Azimuth Premium Concurrent Safety Tests
// 高质量并发安全性测试用例 - 专注于多线程环境下的数据一致性和竞态条件

test "并发写入遥测数据的安全性" {
  // 创建共享的遥测数据存储
  let telemetry_store = ConcurrentTelemetryStore::new()
  let num_threads = 10
  let operations_per_thread = 100
  
  // 创建多个线程同时写入数据
  let threads = []
  for thread_id in 0..num_threads {
    let thread = Thread::spawn(fn() {
      for i in 0..operations_per_thread {
        let metric_name = "concurrent_metric_" + thread_id.to_string()
        let value = (thread_id * operations_per_thread + i).to_float()
        let timestamp = 1234567890L + (thread_id * operations_per_thread + i).to_long()
        
        let data_point = TelemetryDataPoint::new(metric_name, value, timestamp)
        ConcurrentTelemetryStore::add(telemetry_store, data_point)
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证数据完整性
  let all_data = ConcurrentTelemetryStore::get_all(telemetry_store)
  assert_eq(all_data.length(), num_threads * operations_per_thread)
  
  // 验证没有数据丢失或重复
  let mut value_set = Set::new()
  for data_point in all_data {
    assert_false(value_set.contains(data_point.value)) // 确保没有重复
    value_set.add(data_point.value)
  }
  assert_eq(value_set.size(), num_threads * operations_per_thread)
}

test "并发读取和写入的一致性" {
  // 创建共享的遥测数据存储并预填充数据
  let telemetry_store = ConcurrentTelemetryStore::new()
  
  // 预填充一些数据
  for i in 0..50 {
    let data_point = TelemetryDataPoint::new("initial_metric", i.to_float(), 1234567890L + i.to_long())
    ConcurrentTelemetryStore::add(telemetry_store, data_point)
  }
  
  let num_reader_threads = 5
  let num_writer_threads = 3
  let operations_per_thread = 20
  
  let threads = []
  
  // 创建读取线程
  for reader_id in 0..num_reader_threads {
    let thread = Thread::spawn(fn() {
      for i in 0..operations_per_thread {
        // 读取所有数据并验证一致性
        let snapshot = ConcurrentTelemetryStore::get_snapshot(telemetry_store)
        
        // 验证快照中的数据是自洽的
        let mut expected_size = 50 // 初始数据量
        for j in 0..num_writer_threads {
          expected_size = expected_size + (i * num_writer_threads + j) // 估算当前应该有的数据量
        }
        
        // 数据量应该至少等于初始数据量
        assert_true(snapshot.length() >= 50)
        
        // 验证数据的时间戳是递增的（如果按时间戳排序）
        let sorted_snapshot = snapshot.sort_by(fn(a, b) { a.timestamp <=> b.timestamp })
        for k in 1..sorted_snapshot.length() {
          assert_true(sorted_snapshot[k].timestamp >= sorted_snapshot[k-1].timestamp)
        }
        
        Thread::sleep(1) // 短暂休眠以增加并发冲突的可能性
      }
    })
    threads.push(thread)
  }
  
  // 创建写入线程
  for writer_id in 0..num_writer_threads {
    let thread = Thread::spawn(fn() {
      for i in 0..operations_per_thread {
        let metric_name = "writer_metric_" + writer_id.to_string()
        let value = (writer_id * operations_per_thread + i).to_float() + 1000.0
        let timestamp = 1234568000L + (writer_id * operations_per_thread + i).to_long()
        
        let data_point = TelemetryDataPoint::new(metric_name, value, timestamp)
        ConcurrentTelemetryStore::add(telemetry_store, data_point)
        
        Thread::sleep(2) // 短暂休眠以增加并发冲突的可能性
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证最终数据完整性
  let final_data = ConcurrentTelemetryStore::get_all(telemetry_store)
  assert_eq(final_data.length(), 50 + (num_writer_threads * operations_per_thread))
}

test "并发聚合操作的线程安全性" {
  // 创建共享的遥测数据存储
  let telemetry_store = ConcurrentTelemetryStore::new()
  
  // 预填充大量数据
  for i in 0..1000 {
    let metric_name = "agg_metric_" + (i % 10).to_string() // 10种不同的指标
    let value = (i * 0.5) + (Random::next_float() * 10.0) // 添加一些随机性
    let timestamp = 1234567890L + i.to_long()
    
    let data_point = TelemetryDataPoint::new(metric_name, value, timestamp)
    ConcurrentTelemetryStore::add(telemetry_store, data_point)
  }
  
  let num_aggregator_threads = 8
  let threads = []
  let results = ConcurrentResults::new()
  
  // 创建多个聚合线程
  for thread_id in 0..num_aggregator_threads {
    let thread = Thread::spawn(fn() {
      for metric_idx in 0..10 {
        let metric_name = "agg_metric_" + metric_idx.to_string()
        
        // 执行各种聚合操作
        let count = ConcurrentTelemetryStore::count_by_metric(telemetry_store, metric_name)
        let sum = ConcurrentTelemetryStore::sum_by_metric(telemetry_store, metric_name)
        let average = ConcurrentTelemetryStore::average_by_metric(telemetry_store, metric_name)
        let min = ConcurrentTelemetryStore::min_by_metric(telemetry_store, metric_name)
        let max = ConcurrentTelemetryStore::max_by_metric(telemetry_store, metric_name)
        
        // 验证聚合结果的一致性
        assert_true(count > 0)
        assert_true(average >= min && average <= max)
        assert_true(sum / count.to_float() >= min && sum / count.to_float() <= max)
        
        // 存储结果用于后续验证
        ConcurrentResults::add(results, thread_id, metric_name, count, sum, average, min, max)
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证所有线程得到相同的聚合结果
  let all_results = ConcurrentResults::get_all(results)
  for metric_idx in 0..10 {
    let metric_name = "agg_metric_" + metric_idx.to_string()
    let metric_results = all_results.filter(fn(r) { r.metric_name == metric_name })
    
    // 所有线程对同一指标的聚合结果应该相同
    let first_result = metric_results[0]
    for result in metric_results {
      assert_eq(result.count, first_result.count)
      assert_eq(result.sum, first_result.sum)
      assert_eq(result.average, first_result.average)
      assert_eq(result.min, first_result.min)
      assert_eq(result.max, first_result.max)
    }
  }
}

test "并发资源池管理的安全性" {
  // 创建共享的资源池
  let resource_pool = ConcurrentResourcePool::new(5) // 池大小为5
  let num_threads = 10
  let operations_per_thread = 20
  
  let threads = []
  let usage_stats = ConcurrentUsageStats::new()
  
  // 创建多个线程竞争资源
  for thread_id in 0..num_threads {
    let thread = Thread::spawn(fn() {
      for i in 0..operations_per_thread {
        // 尝试获取资源
        let resource = ConcurrentResourcePool::acquire(resource_pool, 100) // 100ms超时
        match resource {
          Some(res) => {
            // 成功获取资源，记录使用情况
            ConcurrentUsageStats::record_acquisition(usage_stats, thread_id)
            
            // 模拟资源使用
            let work_time = Random::next_int(10) + 5 // 5-15ms的工作时间
            Thread::sleep(work_time)
            
            // 释放资源
            ConcurrentResourcePool::release(resource_pool, res)
            ConcurrentUsageStats::record_release(usage_stats, thread_id)
          }
          None => {
            // 获取资源失败，记录超时
            ConcurrentUsageStats::record_timeout(usage_stats, thread_id)
          }
        }
        
        Thread::sleep(Random::next_int(5)) // 随机休眠
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证资源池状态
  assert_eq(ConcurrentResourcePool::available_count(resource_pool), 5) // 所有资源应该被释放
  
  // 验证使用统计
  let stats = ConcurrentUsageStats::get_summary(usage_stats)
  assert_eq(stats.total_acquisitions, stats.total_releases) // 获取和释放次数应该相等
  assert_true(stats.total_acquisitions > 0) // 应该有成功的获取操作
  assert_true(stats.total_timeouts >= 0) // 可能有超时操作
  
  // 验证没有资源泄漏
  assert_eq(stats.total_acquisitions - stats.total_releases, 0)
}

test "并发缓存操作的一致性" {
  // 创建共享缓存
  let cache = ConcurrentCache::new(100) // 最大100个条目
  let num_threads = 8
  let operations_per_thread = 50
  
  let threads = []
  
  // 创建多个线程进行缓存操作
  for thread_id in 0..num_threads {
    let thread = Thread::spawn(fn() {
      for i in 0..operations_per_thread {
        let key = "cache_key_" + (thread_id * operations_per_thread + i).to_string()
        let value = "cache_value_" + thread_id.to_string() + "_" + i.to_string()
        
        // 写入缓存
        ConcurrentCache::put(cache, key, value)
        
        // 立即读取验证
        let retrieved = ConcurrentCache::get(cache, key)
        match retrieved {
          Some(v) => assert_eq(v, value)
          None => assert_true(false) // 刚写入的值应该能立即读取
        }
        
        // 随机读取其他键
        if i > 0 && Random::next_float() > 0.7 {
          let random_key = "cache_key_" + Random::next_int(thread_id * operations_per_thread + i).to_string()
          let random_value = ConcurrentCache::get(cache, random_key)
          // 不做断言，因为是随机读取，可能存在也可能不存在
        }
        
        // 偶尔删除一些条目
        if i > 10 && Random::next_float() > 0.8 {
          let key_to_delete = "cache_key_" + (thread_id * operations_per_thread + i - 10).to_string()
          ConcurrentCache::remove(cache, key_to_delete)
        }
      }
    })
    threads.push(thread)
  }
  
  // 等待所有线程完成
  for thread in threads {
    Thread::join(thread)
  }
  
  // 验证缓存一致性
  let cache_stats = ConcurrentCache::get_stats(cache)
  assert_true(cache_stats.size <= 100) // 缓存大小不应超过限制
  assert_true(cache_stats.hits > 0) // 应该有缓存命中
  assert_true(cache_stats.misses >= 0) // 可能有缓存未命中
  
  // 验证缓存中的数据一致性
  let all_keys = ConcurrentCache::get_all_keys(cache)
  for key in all_keys {
    let value = ConcurrentCache::get(cache, key)
    match value {
      Some(v) => {
        // 验证值格式正确
        assert_true(v.starts_with("cache_value_"))
      }
      None => assert_true(false) // 键存在但值不存在是不一致的
    }
  }
}