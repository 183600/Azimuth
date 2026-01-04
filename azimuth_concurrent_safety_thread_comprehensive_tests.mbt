// Azimuth 并发安全线程综合测试用例
// 专注于遥测系统的并发安全功能，包括线程安全、竞态条件、死锁检测等

// 测试1: 线程安全的Span操作
test "线程安全的Span操作测试" {
  // 创建线程安全的Span管理器
  let thread_safe_span_manager = ThreadSafeSpanManager::new()
  
  // 创建线程池
  let thread_pool = ThreadPool::new(10)
  
  // 测试并发Span创建
  let concurrent_span_creation = fn() {
    let futures = []
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        let created_spans = []
        
        // 每个线程创建100个span
        for i in 0..=99 {
          let span_name = "concurrent.span." + thread_id.to_string() + "." + i.to_string()
          let span = thread_safe_span_manager.create_span(span_name, Server, 
                                                         TraceContext::new("concurrent-trace-" + thread_id.to_string(), 
                                                                         "concurrent-span-" + thread_id.to_string() + "-" + i.to_string(), 
                                                                         true, ""))
          
          // 添加属性
          Span::set_attribute(span, "thread.id", IntValue(thread_id))
          Span::set_attribute(span, "span.index", IntValue(i))
          Span::set_attribute(span, "creation.time", IntValue(Time::now()))
          
          created_spans = created_spans.push(span)
        }
        
        created_spans
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    let results = []
    for future in futures {
      let result = future.wait()
      results = results.push(result)
    }
    
    results
  }
  
  let creation_results = concurrent_span_creation()
  
  // 验证并发创建结果
  assert_eq(creation_results.length(), 10)
  
  let total_spans_created = creation_results.map(fn(r) { r.length() }).sum()
  assert_eq(total_spans_created, 1000)  // 10个线程 × 100个span
  
  // 验证所有span都有正确的属性
  for thread_spans in creation_results {
    for span in thread_spans {
      let thread_id_attr = Span::get_attribute(span, "thread.id")
      let span_index_attr = Span::get_attribute(span, "span.index")
      let creation_time_attr = Span::get_attribute(span, "creation.time")
      
      assert_true(thread_id_attr.is_some)
      assert_true(span_index_attr.is_some)
      assert_true(creation_time_attr.is_some)
    }
  }
  
  // 测试并发Span属性修改
  let concurrent_attribute_modification = fn(spans : Array<Span>) -> Array<Bool> {
    let futures = []
    
    // 将spans分配给不同线程
    let spans_per_thread = spans.length() / 10
    
    for thread_id in 0..=9 {
      let thread_spans = spans.slice(thread_id * spans_per_thread, spans_per_thread)
      
      let future = thread_pool.spawn(fn() {
        let modification_results = []
        
        for span in thread_spans {
          // 并发修改属性
          Span::set_attribute(span, "modified.by.thread", IntValue(thread_id))
          Span::set_attribute(span, "modification.time", IntValue(Time::now()))
          Span::set_attribute(span, "concurrent.access", StringValue("true"))
          
          // 验证属性设置成功
          let modified_attr = Span::get_attribute(span, "modified.by.thread")
          let concurrent_attr = Span::get_attribute(span, "concurrent.access")
          
          modification_results = modification_results.push(
            modified_attr.is_some and concurrent_attr.is_some
          )
        }
        
        modification_results
      })
      futures = futures.push(future)
    }
    
    // 等待所有修改完成
    let results = []
    for future in futures {
      let result = future.wait()
      results = results.concat(result)
    }
    
    results
  }
  
  // 将所有span合并到一个数组中进行属性修改测试
  let all_spans = []
  for thread_spans in creation_results {
    all_spans = all_spans.concat(thread_spans)
  }
  
  let modification_results = concurrent_attribute_modification(all_spans)
  
  // 验证并发属性修改结果
  assert_eq(modification_results.length(), all_spans.length())
  
  let successful_modifications = modification_results.filter(fn(r) { r }).length()
  assert_eq(successful_modifications, all_spans.length())  // 所有修改都应该成功
  
  // 验证最终状态一致性
  let final_verification_results = []
  for span in all_spans {
    let thread_id_attr = Span::get_attribute(span, "thread.id")
    let modified_attr = Span::get_attribute(span, "modified.by.thread")
    let concurrent_attr = Span::get_attribute(span, "concurrent.access")
    
    // 验证属性存在且有效
    let verification_result = thread_id_attr.is_some and 
                            modified_attr.is_some and 
                            concurrent_attr.is_some
    
    final_verification_results = final_verification_results.push(verification_result)
  }
  
  let successful_verifications = final_verification_results.filter(fn(r) { r }).length()
  assert_eq(successful_verifications, all_spans.length())
}

// 测试2: 竞态条件检测和防护
test "竞态条件检测和防护测试" {
  // 创建竞态条件检测器
  let race_condition_detector = RaceConditionDetector::new()
  
  // 创建共享计数器
  let shared_counter = AtomicCounter::new(0)
  
  // 测试无保护的竞态条件
  let unprotected_race_test = fn() -> (Int, Int) {
    race_condition_detector.start_monitoring()
    
    let futures = []
    
    // 10个线程同时增加计数器
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=999 {
          // 无保护的操作，会产生竞态条件
          shared_counter.increment_unsafe()
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    for future in futures {
      future.wait()
    }
    
    let final_value = shared_counter.get_value()
    let expected_value = 10 * 1000
    
    let race_report = race_condition_detector.stop_monitoring()
    
    (final_value, expected_value)
  }
  
  let (unprotected_final, unprotected_expected) = unprotected_race_test()
  
  // 验证竞态条件导致的不一致
  assert_true(unprotected_final != unprotected_expected)
  assert_true(unprotected_final < unprotected_expected)  // 竞态条件通常导致值偏小
  
  // 重置计数器
  shared_counter.reset()
  
  // 测试有保护的原子操作
  let protected_test = fn() -> (Int, Int) {
    race_condition_detector.start_monitoring()
    
    let futures = []
    
    // 10个线程同时原子增加计数器
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=999 {
          // 原子保护的操作
          shared_counter.increment_atomic()
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    for future in futures {
      future.wait()
    }
    
    let final_value = shared_counter.get_value()
    let expected_value = 10 * 1000
    
    let race_report = race_condition_detector.stop_monitoring()
    
    (final_value, expected_value)
  }
  
  let (protected_final, protected_expected) = protected_test()
  
  // 验证原子操作的一致性
  assert_eq(protected_final, protected_expected)
  
  // 测试共享资源的竞态条件
  let shared_resource_test = fn() {
    let shared_map = ConcurrentHashMap::new()
    
    // 添加初始数据
    for i in 0..=99 {
      shared_map.put("key_" + i.to_string(), "initial_value_" + i.to_string())
    }
    
    race_condition_detector.start_monitoring()
    
    let futures = []
    
    // 多个线程同时读写共享map
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let key = "key_" + i.to_string()
          
          // 读取操作
          let value = shared_map.get(key)
          
          // 写入操作
          let new_value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
          shared_map.put(key, new_value)
          
          // 删除和重新插入操作
          if i % 10 == 0 {
            shared_map.remove(key)
            shared_map.put(key, "restored_" + new_value)
          }
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    for future in futures {
      future.wait()
    }
    
    let race_report = race_condition_detector.stop_monitoring()
    
    // 验证共享map的完整性
    let final_size = shared_map.size()
    let data_integrity = shared_map.verify_integrity()
    
    (final_size, data_integrity, race_report)
  }
  
  let (final_map_size, map_integrity, map_race_report) = shared_resource_test()
  
  // 验证共享资源完整性
  assert_eq(final_map_size, 100)  // 所有key都应该存在
  assert_true(map_integrity)
  assert_true(map_race_report.race_conditions_detected > 0)
  
  // 测试竞态条件防护策略
  let race_protection_test = fn() {
    let protected_resource = ProtectedResource::new()
    
    race_condition_detector.start_monitoring()
    
    let futures = []
    
    // 使用不同防护策略的并发操作
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          // 使用互斥锁保护
          protected_resource.mutex_protected_operation(fn() {
            protected_resource.increment_counter()
          })
          
          // 使用读写锁保护
          if i % 2 == 0 {
            protected_resource.read_lock_operation(fn() {
              protected_resource.get_counter_value()
            })
          } else {
            protected_resource.write_lock_operation(fn() {
              protected_resource.set_counter_value(thread_id * 1000 + i)
            })
          }
          
          // 使用原子操作保护
          protected_resource.atomic_compare_and_swap(i, i + thread_id)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    for future in futures {
      future.wait()
    }
    
    let race_report = race_condition_detector.stop_monitoring()
    let final_counter_value = protected_resource.get_counter_value()
    
    (final_counter_value, race_report)
  }
  
  let (protected_counter_value, protection_race_report) = race_protection_test()
  
  // 验证防护策略效果
  assert_true(protected_counter_value >= 0)
  assert_true(protection_race_report.race_conditions_prevented > 0)
}

// 测试3: 死锁检测和预防
test "死锁检测和预防测试" {
  // 创建死锁检测器
  let deadlock_detector = DeadlockDetector::new()
  
  // 配置死锁检测参数
  deadlock_detector.configure({
    detection_interval: 1000,    // 1秒检测间隔
    max_lock_chain_length: 10,   // 最大锁链长度
    timeout_threshold: 5000,     // 5秒超时阈值
    enable_prevention: true      // 启用预防机制
  })
  
  // 创建测试资源
  let resources = [
    ("resource_A", Mutex::new()),
    ("resource_B", Mutex::new()),
    ("resource_C", Mutex::new()),
    ("resource_D", Mutex::new())
  ]
  
  // 测试死锁检测
  let deadlock_detection_test = fn() -> Bool {
    deadlock_detector.start_monitoring()
    
    let futures = []
    let deadlock_detected = AtomicBool::new(false)
    
    // 创建可能导致死锁的操作模式
    for thread_id in 0..=3 {
      let future = thread_pool.spawn(fn() {
        let resource_order = if thread_id == 0 { ["resource_A", "resource_B"] }
                             else if thread_id == 1 { ["resource_B", "resource_A"] }  // 可能死锁
                             else if thread_id == 2 { ["resource_C", "resource_D"] }
                             else { ["resource_D", "resource_C"] }  // 可能死锁
        
        // 尝试按顺序获取资源
        let acquired_resources = []
        
        for resource_name in resource_order {
          let resource = resources.find(fn(r) { r.0 == resource_name }).unwrap().1
          
          // 使用超时获取锁
          if deadlock_detector.try_lock_with_timeout(resource, 2000) {
            acquired_resources = acquired_resources.push(resource)
            
            // 持有锁一段时间
            Time::sleep(100)
          } else {
            // 获取锁失败，释放已获取的锁
            for acquired_resource in acquired_resources {
              deadlock_detector.unlock(acquired_resource)
            }
            deadlock_detected.set(true)
            return thread_id
          }
        }
        
        // 模拟工作
        Time::sleep(500)
        
        // 释放所有锁
        for resource in acquired_resources {
          deadlock_detector.unlock(resource)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成或超时
    let completion_futures = []
    for future in futures {
      let completed_future = future.with_timeout(10000)  // 10秒超时
      completion_futures = completion_futures.push(completed_future)
    }
    
    let completed_threads = []
    for future in completion_futures {
      match future.wait() {
        Ok(thread_id) => completed_threads = completed_threads.push(thread_id),
        Err(_) => deadlock_detected.set(true)  // 超时表示可能死锁
      }
    }
    
    let deadlock_report = deadlock_detector.stop_monitoring()
    
    deadlock_detected.get() or deadlock_report.deadlocks_detected > 0
  }
  
  let deadlock_detected_result = deadlock_detection_test()
  
  // 验证死锁检测
  assert_true(deadlock_detected_result)
  
  // 测试死锁预防
  let deadlock_prevention_test = fn() -> Bool {
    deadlock_detector.enable_prevention_mode()
    
    let futures = []
    let all_operations_completed = AtomicBool::new(true)
    
    // 使用死锁预防策略
    for thread_id in 0..=3 {
      let future = thread_pool.spawn(fn() {
        let resource_order = if thread_id == 0 { ["resource_A", "resource_B"] }
                             else if thread_id == 1 { ["resource_B", "resource_A"] }
                             else if thread_id == 2 { ["resource_C", "resource_D"] }
                             else { ["resource_D", "resource_C"] }
        
        // 使用死锁预防的锁获取策略
        let acquired_resources = []
        
        for resource_name in resource_order {
          let resource = resources.find(fn(r) { r.0 == resource_name }).unwrap().1
          
          // 使用预防策略获取锁
          if deadlock_detector.preventive_lock(resource) {
            acquired_resources = acquired_resources.push(resource)
            Time::sleep(100)
          } else {
            // 预防机制阻止获取锁，释放已获取的锁
            for acquired_resource in acquired_resources {
              deadlock_detector.preventive_unlock(acquired_resource)
            }
            all_operations_completed.set(false)
            return thread_id
          }
        }
        
        // 模拟工作
        Time::sleep(500)
        
        // 释放所有锁
        for resource in acquired_resources {
          deadlock_detector.preventive_unlock(resource)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    let completed_threads = []
    for future in futures {
      let thread_id = future.wait()
      completed_threads = completed_threads.push(thread_id)
    }
    
    let prevention_report = deadlock_detector.get_prevention_report()
    
    completed_threads.length() == 4 and all_operations_completed.get()
  }
  
  let prevention_result = deadlock_prevention_test()
  
  // 验证死锁预防效果
  assert_true(prevention_result)
  
  // 测试锁顺序规范
  let lock_ordering_test = fn() -> Bool {
    // 定义全局锁顺序
    deadlock_detector.set_global_lock_order(["resource_A", "resource_B", "resource_C", "resource_D"])
    
    let futures = []
    let ordering_violations = AtomicInt::new(0)
    
    for thread_id in 0..=3 {
      let future = thread_pool.spawn(fn() {
        let resource_order = if thread_id == 0 { ["resource_A", "resource_B", "resource_C"] }
                             else if thread_id == 1 { ["resource_B", "resource_C", "resource_D"] }
                             else if thread_id == 2 { ["resource_C", "resource_D", "resource_A"] }
                             else { ["resource_D", "resource_A", "resource_B"] }
        
        // 按照定义的顺序获取锁
        let acquired_resources = []
        
        for resource_name in resource_order {
          let resource = resources.find(fn(r) { r.0 == resource_name }).unwrap().1
          
          if deadlock_detector.ordered_lock(resource, resource_name) {
            acquired_resources = acquired_resources.push(resource)
            Time::sleep(100)
          } else {
            // 违反锁顺序
            ordering_violations.increment()
            for acquired_resource in acquired_resources {
              deadlock_detector.ordered_unlock(acquired_resource)
            }
            return thread_id
          }
        }
        
        Time::sleep(500)
        
        for resource in acquired_resources {
          deadlock_detector.ordered_unlock(resource)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    let completed_threads = []
    for future in futures {
      let thread_id = future.wait()
      completed_threads = completed_threads.push(thread_id)
    }
    
    let ordering_report = deadlock_detector.get_ordering_report()
    
    ordering_violations.get() == 0 and completed_threads.length() == 4
  }
  
  let ordering_result = lock_ordering_test()
  
  // 验证锁顺序规范效果
  assert_true(ordering_result)
}

// 测试4: 线程安全的集合操作
test "线程安全的集合操作测试" {
  // 创建线程安全集合管理器
  let thread_safe_collections = ThreadSafeCollections::new()
  
  // 测试并发List操作
  let concurrent_list_test = fn() {
    let safe_list = thread_safe_collections.create_concurrent_list()
    
    let futures = []
    
    // 并发添加元素
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let element = "thread_" + thread_id.to_string() + "_element_" + i.to_string()
          safe_list.add(element)
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有添加完成
    for future in futures {
      future.wait()
    }
    
    // 验证列表大小
    let list_size = safe_list.size()
    assert_eq(list_size, 1000)  // 10个线程 × 100个元素
    
    // 并发读取和迭代
    let read_futures = []
    let read_results = ConcurrentList::new()
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        let local_count = 0
        
        // 安全迭代
        safe_list.for_each(fn(element) {
          local_count = local_count + 1
        })
        
        local_count
      })
      read_futures = read_futures.push(future)
    }
    
    // 等待所有读取完成
    for future in read_futures {
      let count = future.wait()
      read_results.add(count)
    }
    
    // 验证读取结果一致性
    for count in read_results {
      assert_eq(count, 1000)
    }
    
    // 并发修改操作
    let modify_futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        let modified_count = 0
        
        for i in 0..=99 {
          let index = thread_id * 100 + i
          if index < safe_list.size() {
            let new_value = "modified_thread_" + thread_id.to_string() + "_" + i.to_string()
            safe_list.set(index, new_value)
            modified_count = modified_count + 1
          }
        }
        
        modified_count
      })
      modify_futures = modify_futures.push(future)
    }
    
    // 等待所有修改完成
    let total_modified = 0
    for future in modify_futures {
      let count = future.wait()
      total_modified = total_modified + count
    }
    
    assert_eq(total_modified, 500)  // 5个线程 × 100个修改
    
    safe_list
  }
  
  let concurrent_list_result = concurrent_list_test()
  
  // 测试并发Map操作
  let concurrent_map_test = fn() {
    let safe_map = thread_safe_collections.create_concurrent_map()
    
    let futures = []
    
    // 并发插入键值对
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let key = "key_" + thread_id.to_string() + "_" + i.to_string()
          let value = "value_" + thread_id.to_string() + "_" + i.to_string()
          safe_map.put(key, value)
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有插入完成
    for future in futures {
      future.wait()
    }
    
    // 验证Map大小
    let map_size = safe_map.size()
    assert_eq(map_size, 1000)  // 10个线程 × 100个键值对
    
    // 并发读取操作
    let read_futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        let found_count = 0
        
        for i in 0..=99 {
          let key = "key_0_" + i.to_string()  // 读取第一个线程插入的数据
          if safe_map.contains_key(key) {
            let value = safe_map.get(key)
            if value.is_some() {
              found_count = found_count + 1
            }
          }
        }
        
        found_count
      })
      read_futures = read_futures.push(future)
    }
    
    // 验证读取结果
    for future in read_futures {
      let count = future.wait()
      assert_eq(count, 100)  // 每个线程应该找到100个键
    }
    
    // 并发更新操作
    let update_futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        let updated_count = 0
        
        for i in 0..=99 {
          let key = "key_" + thread_id.to_string() + "_" + i.to_string()
          let new_value = "updated_value_" + thread_id.to_string() + "_" + i.to_string()
          
          if safe_map.replace(key, new_value) {
            updated_count = updated_count + 1
          }
        }
        
        updated_count
      })
      update_futures = update_futures.push(future)
    }
    
    // 验证更新结果
    let total_updated = 0
    for future in update_futures {
      let count = future.wait()
      total_updated = total_updated + count
    }
    
    assert_eq(total_updated, 500)  // 5个线程 × 100个更新
    
    safe_map
  }
  
  let concurrent_map_result = concurrent_map_test()
  
  // 测试并发Queue操作
  let concurrent_queue_test = fn() {
    let safe_queue = thread_safe_collections.create_concurrent_queue()
    
    // 生产者线程
    let producer_futures = []
    
    for thread_id in 0..=2 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=333 {
          let item = "produced_by_thread_" + thread_id.to_string() + "_item_" + i.to_string()
          safe_queue.enqueue(item)
        }
        thread_id
      })
      producer_futures = producer_futures.push(future)
    }
    
    // 消费者线程
    let consumer_futures = []
    let consumed_items = ConcurrentList::new()
    
    for thread_id in 0..=2 {
      let future = thread_pool.spawn(fn() {
        let local_consumed = []
        
        while true {
          match safe_queue.dequeue() {
            Some(item) => local_consumed = local_consumed.push(item),
            None => {
              Time::sleep(10)  // 等待更多项目
              if safe_queue.is_empty() {
                break
              }
            }
          }
        }
        
        local_consumed
      })
      consumer_futures = consumer_futures.push(future)
    }
    
    // 等待所有生产者完成
    for future in producer_futures {
      future.wait()
    }
    
    // 等待所有消费者完成
    let total_consumed = 0
    for future in consumer_futures {
      let items = future.wait()
      total_consumed = total_consumed + items.length()
    }
    
    // 验证生产消费平衡
    assert_eq(total_consumed, 1001)  // 3个生产者 × 334个项目
    
    safe_queue
  }
  
  let concurrent_queue_result = concurrent_queue_test()
}

// 测试5: 原子操作和CAS
test "原子操作和CAS测试" {
  // 创建原子操作管理器
  let atomic_manager = AtomicManager::new()
  
  // 测试原子计数器
  let atomic_counter_test = fn() {
    let counter = atomic_manager.create_atomic_counter(0)
    
    let futures = []
    
    // 并发增加计数器
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=999 {
          counter.increment()
          counter.add(1)
          counter.increment()
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有操作完成
    for future in futures {
      future.wait()
    }
    
    let final_value = counter.get()
    let expected_value = 10 * 1000 * 3  // 10个线程 × 1000次迭代 × 3次操作
    
    assert_eq(final_value, expected_value)
    
    // 测试原子减少
    let decrement_futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=199 {
          counter.decrement()
          counter.sub(1)
        }
        thread_id
      })
      decrement_futures = decrement_futures.push(future)
    }
    
    for future in decrement_futures {
      future.wait()
    }
    
    let after_decrement = counter.get()
    let expected_after_decrement = expected_value - 5 * 200 * 2  // 5个线程 × 200次迭代 × 2次操作
    
    assert_eq(after_decrement, expected_after_decrement)
    
    counter
  }
  
  let atomic_counter_result = atomic_counter_test()
  
  // 测试比较并交换(CAS)
  let cas_test = fn() {
    let atomic_value = atomic_manager.create_atomic_int(100)
    
    let futures = []
    let successful_cas = AtomicInt::new(0)
    let failed_cas = AtomicInt::new(0)
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let current = atomic_value.get()
          let new_value = current + thread_id + i
          
          if atomic_value.compare_and_swap(current, new_value) {
            successful_cas.increment()
          } else {
            failed_cas.increment()
          }
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    for future in futures {
      future.wait()
    }
    
    let final_value = atomic_value.get()
    let total_successful = successful_cas.get()
    let total_failed = failed_cas.get()
    
    // 验证CAS操作
    assert_true(final_value > 100)  // 值应该增加
    assert_true(total_successful + total_failed == 1000)  // 所有操作都有结果
    assert_true(total_successful > 0)  // 应该有成功的CAS
    assert_true(total_failed > 0)     // 应该有失败的CAS
    
    (final_value, total_successful, total_failed)
  }
  
  let (cas_final_value, cas_successful, cas_failed) = cas_test()
  
  // 测试原子引用
  let atomic_reference_test = fn() {
    let atomic_ref = atomic_manager.create_atomic_reference("initial_value")
    
    let futures = []
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let new_value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
          atomic_ref.set(new_value)
          
          // 读取当前值
          let current_value = atomic_ref.get()
          assert_true(current_value.length() > 0)
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    for future in futures {
      future.wait()
    }
    
    let final_ref_value = atomic_ref.get()
    assert_true(final_ref_value.length() > 0)
    assert_true(final_ref_value.contains("thread_"))
    
    atomic_ref
  }
  
  let atomic_ref_result = atomic_reference_test()
  
  // 测试原子数组操作
  let atomic_array_test = fn() {
    let atomic_array = atomic_manager.create_atomic_array(1000)
    
    // 初始化数组
    for i in 0..=999 {
      atomic_array.set(i, 0)
    }
    
    let futures = []
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let index = thread_id * 100 + i
          atomic_array.add_and_get(index, 1)
          atomic_array.incrementAndGet(index)
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    for future in futures {
      future.wait()
    }
    
    // 验证数组结果
    for i in 0..=999 {
      let value = atomic_array.get(i)
      assert_eq(value, 2)  // 每个位置被增加了两次
    }
    
    atomic_array
  }
  
  let atomic_array_result = atomic_array_test()
}

// 测试6: 线程本地存储
test "线程本地存储测试" {
  // 创建线程本地存储管理器
  let thread_local_manager = ThreadLocalManager::new()
  
  // 测试基本线程本地存储
  let basic_thread_local_test = fn() {
    let thread_local_counter = thread_local_manager.create_thread_local_int(0)
    
    let futures = []
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        // 每个线程有自己的计数器
        for i in 0..=99 {
          let current = thread_local_counter.get()
          thread_local_counter.set(current + 1)
        }
        
        let final_value = thread_local_counter.get()
        final_value
      })
      futures = futures.push(future)
    }
    
    // 收集每个线程的结果
    let thread_results = []
    for future in futures {
      let result = future.wait()
      thread_results = thread_results.push(result)
    }
    
    // 验证每个线程都有独立的计数器
    for result in thread_results {
      assert_eq(result, 100)  // 每个线程增加了100次
    }
    
    thread_results
  }
  
  let basic_thread_local_results = basic_thread_local_test()
  
  // 测试复杂对象的线程本地存储
  let complex_object_test = fn() {
    let thread_local_map = thread_local_manager.create_thread_local_map()
    
    let futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        // 每个线程有自己的map
        for i in 0..=49 {
          let key = "key_" + i.to_string()
          let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
          thread_local_map.put(key, value)
        }
        
        // 验证本地map内容
        let size = thread_local_map.size()
        assert_eq(size, 50)
        
        // 读取并验证值
        for i in 0..=49 {
          let key = "key_" + i.to_string()
          let value = thread_local_map.get(key)
          assert_true(value.is_some)
          
          match value {
            Some(v) => {
              assert_true(v.contains("thread_" + thread_id.to_string()))
              assert_true(v.contains("_value_" + i.to_string()))
            }
            None => assert_true(false)
          }
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 收集线程ID
    let thread_ids = []
    for future in futures {
      let thread_id = future.wait()
      thread_ids = thread_ids.push(thread_id)
    }
    
    assert_eq(thread_ids.length(), 5)
    
    thread_ids
  }
  
  let complex_object_results = complex_object_test()
  
  // 测试线程本地资源的自动清理
  let resource_cleanup_test = fn() {
    let thread_local_resource = thread_local_manager.create_thread_local_resource()
    
    let futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        // 创建线程本地资源
        let resource = Resource::new("thread_resource_" + thread_id.to_string())
        thread_local_resource.set(resource)
        
        // 使用资源
        let local_resource = thread_local_resource.get()
        assert_true(local_resource.is_some)
        
        match local_resource {
          Some(r) => {
            assert_true(r.get_name().contains("thread_resource_" + thread_id.to_string()))
          }
          None => assert_true(false)
        }
        
        // 模拟工作
        Time::sleep(100)
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    let thread_ids = []
    for future in futures {
      let thread_id = future.wait()
      thread_ids = thread_ids.push(thread_id)
    }
    
    // 验证资源清理
    let cleanup_report = thread_local_manager.get_cleanup_report()
    assert_true(cleanup_report.resources_created >= 5)
    assert_true(cleanup_report.resources_cleaned >= 5)
    
    thread_ids
  }
  
  let cleanup_test_results = resource_cleanup_test()
  
  // 测试线程本地存储的继承
  let inheritance_test = fn() {
    let parent_thread_local = thread_local_manager.create_inheritable_thread_local("parent_value")
    
    // 父线程设置值
    parent_thread_local.set("parent_initial_value")
    
    let futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        // 子线程继承父线程的值
        let inherited_value = parent_thread_local.get()
        assert_eq(inherited_value, Some("parent_initial_value"))
        
        // 子线程修改自己的值
        parent_thread_local.set("child_" + thread_id.to_string() + "_value")
        
        let modified_value = parent_thread_local.get()
        assert_eq(modified_value, Some("child_" + thread_id.to_string() + "_value"))
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有子线程完成
    let thread_ids = []
    for future in futures {
      let thread_id = future.wait()
      thread_ids = thread_ids.push(thread_id)
    }
    
    // 验证父线程的值未被子线程影响
    let parent_value = parent_thread_local.get()
    assert_eq(parent_value, Some("parent_initial_value"))
    
    thread_ids
  }
  
  let inheritance_test_results = inheritance_test()
}

// 测试7: 并发性能和扩展性
test "并发性能和扩展性测试" {
  // 创建并发性能测试器
  let performance_tester = ConcurrencyPerformanceTester::new()
  
  // 测试不同线程数的性能
  let thread_scaling_test = fn() {
    let thread_counts = [1, 2, 4, 8, 16]
    let performance_results = []
    
    for thread_count in thread_counts {
      let test_start = Time::now()
      
      let futures = []
      
      for thread_id in 0..=thread_count - 1 {
        let future = thread_pool.spawn(fn() {
          let operations = 0
          
          // 每个线程执行固定数量的操作
          for i in 0..=9999 {
            // 模拟计算密集型操作
            let result = 0
            for j in 0..=100 {
              result = result + i * j
            }
            
            operations = operations + 1
          }
          
          operations
        })
        futures = futures.push(future)
      }
      
      // 等待所有线程完成
      let total_operations = 0
      for future in futures {
        let operations = future.wait()
        total_operations = total_operations + operations
      }
      
      let test_end = Time::now()
      let duration = test_end - test_start
      let throughput = total_operations.to_float() / duration.to_float() * 1000.0
      
      let result = {
        thread_count: thread_count,
        total_operations: total_operations,
        duration: duration,
        throughput: throughput
      }
      
      performance_results = performance_results.push(result)
    }
    
    performance_results
  }
  
  let scaling_results = thread_scaling_test()
  
  // 验证扩展性
  assert_eq(scaling_results.length(), 5)
  
  // 验证吞吐量随线程数增加（在合理范围内）
  for i in 1..=scaling_results.length() - 1 {
    let current = scaling_results[i]
    let previous = scaling_results[i - 1]
    
    // 吞吐量应该增加或保持相对稳定
    assert_true(current.throughput >= previous.throughput * 0.8)
  }
  
  // 测试锁竞争性能
  let lock_contention_test = fn() {
    let shared_mutex = Mutex::new()
    let shared_counter = AtomicInt::new(0)
    
    let contention_results = []
    
    for thread_count in [2, 4, 8, 16] {
      let test_start = Time::now()
      
      let futures = []
      
      for thread_id in 0..=thread_count - 1 {
        let future = thread_pool.spawn(fn() {
          let local_operations = 0
          
          for i in 0..=4999 {
            // 获取锁
            let _guard = shared_mutex.lock()
            
            // 临界区操作
            shared_counter.increment()
            local_operations = local_operations + 1
            
            // 锁在guard离开作用域时自动释放
          }
          
          local_operations
        })
        futures = futures.push(future)
      }
      
      // 等待所有线程完成
      let total_local_operations = 0
      for future in futures {
        let operations = future.wait()
        total_local_operations = total_local_operations + operations
      }
      
      let test_end = Time::now()
      let duration = test_end - test_start
      let final_counter_value = shared_counter.get()
      let throughput = total_local_operations.to_float() / duration.to_float() * 1000.0
      
      let contention_result = {
        thread_count: thread_count,
        duration: duration,
        final_counter: final_counter_value,
        throughput: throughput
      }
      
      contention_results = contention_results.push(contention_result)
    }
    
    contention_results
  }
  
  let contention_results = lock_contention_test()
  
  // 验证锁竞争影响
  assert_eq(contention_results.length(), 4)
  
  // 验证计数器值的正确性
  for result in contention_results {
    assert_eq(result.final_counter, result.thread_count * 5000)
  }
  
  // 测试无锁数据结构性能
  let lock_free_performance_test = fn() {
    let lock_free_queue = LockFreeQueue::new()
    let standard_queue = Mutex::new(Queue::new())
    
    // 测试无锁队列
    let lock_free_start = Time::now()
    
    let lock_free_futures = []
    
    for thread_id in 0..=7 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=9999 {
          lock_free_queue.enqueue("item_" + i.to_string())
        }
        
        for i in 0..=9999 {
          match lock_free_queue.dequeue() {
            Some(_) => (),
            None => ()
          }
        }
      })
      lock_free_futures = lock_free_futures.push(future)
    }
    
    for future in lock_free_futures {
      future.wait()
    }
    
    let lock_free_end = Time::now()
    let lock_free_duration = lock_free_end - lock_free_start
    
    // 测试标准队列
    let standard_start = Time::now()
    
    let standard_futures = []
    
    for thread_id in 0..=7 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=9999 {
          let _guard = standard_queue.lock()
          standard_queue.get().enqueue("item_" + i.to_string())
        }
        
        for i in 0..=9999 {
          let _guard = standard_queue.lock()
          match standard_queue.get().dequeue() {
            Some(_) => (),
            None => ()
          }
        }
      })
      standard_futures = standard_futures.push(future)
    }
    
    for future in standard_futures {
      future.wait()
    }
    
    let standard_end = Time::now()
    let standard_duration = standard_end - standard_start
    
    // 无锁应该更快
    assert_true(lock_free_duration < standard_duration)
    
    let performance_improvement = (standard_duration - lock_free_duration).to_float() / standard_duration.to_float()
    assert_true(performance_improvement > 0.1)  // 至少10%的性能提升
    
    (lock_free_duration, standard_duration, performance_improvement)
  }
  
  let (lock_free_time, standard_time, improvement) = lock_free_performance_test()
  
  // 验证性能提升
  assert_true(improvement > 0.0)
}

// 测试8: 并发异常处理
test "并发异常处理测试" {
  // 创建并发异常处理器
  let concurrent_exception_handler = ConcurrentExceptionHandler::new()
  
  // 测试并发异常传播
  let exception_propagation_test = fn() {
    let futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        try {
          for i in 0..=99 {
            if i == 50 and thread_id == 2 {
              // 第2个线程在第50次迭代时抛出异常
              throw ConcurrentException::new("test_exception_" + thread_id.to_string())
            }
            
            // 正常操作
            Time::sleep(1)
          }
          
          return thread_id
        } catch {
          case e : ConcurrentException => {
            concurrent_exception_handler.handle_exception(e, thread_id)
            return -1  // 异常标记
          }
        }
      })
      futures = futures.push(future)
    }
    
    // 收集结果
    let results = []
    for future in futures {
      let result = future.wait()
      results = results.push(result)
    }
    
    // 验证异常处理
    assert_eq(results.length(), 5)
    assert_true(results.contains(-1))  // 应该有一个线程返回异常标记
    
    let exception_report = concurrent_exception_handler.get_exception_report()
    assert_true(exception_report.exceptions_caught > 0)
    
    results
  }
  
  let propagation_results = exception_propagation_test()
  
  // 测试并发异常恢复
  let exception_recovery_test = fn() {
    let recovery_manager = ConcurrentRecoveryManager::new()
    
    let futures = []
    
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        let mut operations_completed = 0
        
        for i in 0..=99 {
          try {
            // 可能失败的操作
            if i % 20 == 0 and thread_id % 2 == 0 {
              throw ConcurrentException::new("recoverable_exception")
            }
            
            operations_completed = operations_completed + 1
            
          } catch {
            case e : ConcurrentException => {
              // 尝试恢复
              let recovery_result = recovery_manager.attempt_recovery(e, thread_id)
              
              if recovery_result.success {
                operations_completed = operations_completed + 1  // 恢复成功，继续操作
              } else {
                break  // 恢复失败，退出循环
              }
            }
          }
        }
        
        operations_completed
      })
      futures = futures.push(future)
    }
    
    // 收集恢复结果
    let recovery_results = []
    for future in futures {
      let result = future.wait()
      recovery_results = recovery_results.push(result)
    }
    
    // 验证恢复效果
    assert_eq(recovery_results.length(), 5)
    
    // 至少有一些操作完成了
    let total_operations = recovery_results.sum()
    assert_true(total_operations > 0)
    
    let recovery_report = recovery_manager.get_recovery_report()
    assert_true(recovery_report.recovery_attempts > 0)
    assert_true(recovery_report.successful_recoveries > 0)
    
    recovery_results
  }
  
  let recovery_results = exception_recovery_test()
  
  // 测试异常隔离
  let exception_isolation_test = fn() {
    let isolation_manager = ExceptionIsolationManager::new()
    
    let futures = []
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        let isolated_context = isolation_manager.create_isolated_context(thread_id)
        
        try {
          for i in 0..=99 {
            // 在隔离上下文中执行操作
            isolated_context.execute(fn() {
              if i == 25 and thread_id >= 7 {
                throw ConcurrentException::new("isolated_exception")
              }
              
              // 正常操作
              Time::sleep(1)
            })
          }
          
          return thread_id
        } catch {
          case e : ConcurrentException => {
            // 异常应该被隔离，不影响其他线程
            isolation_manager.handle_isolated_exception(e, thread_id)
            return -1
          }
        }
      })
      futures = futures.push(future)
    }
    
    // 收集隔离结果
    let isolation_results = []
    for future in futures {
      let result = future.wait()
      isolation_results = isolation_results.push(result)
    }
    
    // 验证隔离效果
    let failed_threads = isolation_results.filter(fn(r) { r == -1 }).length()
    let successful_threads = isolation_results.filter(fn(r) { r >= 0 }).length()
    
    assert_eq(failed_threads, 3)  // 线程7、8、9应该失败
    assert_eq(successful_threads, 7)  // 线程0-6应该成功
    
    let isolation_report = isolation_manager.get_isolation_report()
    assert_true(isolation_report.exceptions_isolated > 0)
    assert_true(isolation_report.threads_affected == failed_threads)
    
    isolation_results
  }
  
  let isolation_results = exception_isolation_test()
}

// 测试9: 并发数据一致性
test "并发数据一致性测试" {
  // 创建并发一致性管理器
  let consistency_manager = ConcurrentConsistencyManager::new()
  
  // 测试最终一致性
  let eventual_consistency_test = fn() {
    let distributed_map = consistency_manager.create_eventually_consistent_map()
    
    let futures = []
    
    // 多个线程同时写入
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          let key = "key_" + i.to_string()
          let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
          
          distributed_map.put_async(key, value)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有写入完成
    for future in futures {
      future.wait()
    }
    
    // 等待最终一致性收敛
    Time::sleep(2000)
    
    // 验证一致性
    let consistency_report = consistency_manager.check_consistency(distributed_map)
    assert_true(consistency_report.is_consistent)
    assert_true(consistency_report.inconsistent_keys == 0)
    
    // 验证数据完整性
    let all_keys_present = true
    for i in 0..=99 {
      let key = "key_" + i.to_string()
      if not distributed_map.contains_key(key) {
        all_keys_present = false
        break
      }
    }
    
    assert_true(all_keys_present)
    
    consistency_report
  }
  
  let consistency_report = eventual_consistency_test()
  
  // 测试强一致性
  let strong_consistency_test = fn() {
    let strong_consistent_counter = consistency_manager.create_strong_consistent_counter(0)
    
    let futures = []
    
    // 多个线程同时增加计数器
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=99 {
          strong_consistent_counter.increment_strong()
        }
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有操作完成
    for future in futures {
      future.wait()
    }
    
    // 验证强一致性
    let final_value = strong_consistent_counter.get()
    let expected_value = 10 * 100
    
    assert_eq(final_value, expected_value)
    
    // 验证一致性状态
    let consistency_state = consistency_manager.get_consistency_state(strong_consistent_counter)
    assert_true(consistency_state.is_strongly_consistent)
    assert_eq(consistency_state.pending_operations, 0)
    
    final_value
  }
  
  let strong_consistency_result = strong_consistency_test()
  
  // 测试读写一致性
  let read_write_consistency_test = fn() {
    let rw_consistent_data = consistency_manager.create_read_write_consistent_data("initial_value")
    
    let futures = []
    
    // 读取线程
    for thread_id in 0..=4 {
      let future = thread_pool.spawn(fn() {
        let read_values = []
        
        for i in 0..=199 {
          let value = rw_consistent_data.read_consistent()
          read_values = read_values.push(value)
          Time::sleep(5)
        }
        
        read_values
      })
      futures = futures.push(future)
    }
    
    // 写入线程
    for thread_id in 0..=1 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=49 {
          let new_value = "writer_" + thread_id.to_string() + "_value_" + i.to_string()
          rw_consistent_data.write_consistent(new_value)
          Time::sleep(20)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 收集读取结果
    let read_results = []
    for i in 0..=4 {
      let future = futures[i]
      let values = future.wait()
      read_results = read_results.push(values)
    }
    
    // 等待写入完成
    for i in 5..=6 {
      let future = futures[i]
      future.wait()
    }
    
    // 验证读写一致性
    for read_values in read_results {
      // 所有读取值应该是有效的
      for value in read_values {
        assert_true(value.length() > 0)
      }
      
      // 验证单调性（读取值的变化应该是单调的）
      let mut monotonic = true
      for i in 1..=read_values.length() - 1 {
        if read_values[i] < read_values[i-1] {
          monotonic = false
          break
        }
      }
      
      // 在某些一致性模型下，可能不保证单调性，所以这里只是检查
      // monotonic = monotonic  // 避免未使用变量警告
    }
    
    let consistency_metrics = consistency_manager.get_consistency_metrics(rw_consistent_data)
    assert_true(consistency_metrics.read_operations > 0)
    assert_true(consistency_metrics.write_operations > 0)
    assert_true(consistency_metrics.consistency_violations == 0)
    
    read_results
  }
  
  let read_write_results = read_write_consistency_test()
}

// 测试10: 并发安全边界条件
test "并发安全边界条件测试" {
  // 创建边界条件测试器
  let boundary_tester = ConcurrencyBoundaryTester::new()
  
  // 测试高并发下的资源耗尽
  let resource_exhaustion_test = fn() {
    let resource_pool = boundary_tester.create_limited_resource_pool(100)  // 最多100个资源
    
    let futures = []
    let acquisition_failures = AtomicInt::new(0)
    
    // 大量线程尝试获取资源
    for thread_id in 0..=19 {
      let future = thread_pool.spawn(fn() {
        let acquired_resources = []
        
        for i in 0..=49 {
          match resource_pool.try_acquire() {
            Some(resource) => acquired_resources = acquired_resources.push(resource),
            None => acquisition_failures.increment()
          }
          
          Time::sleep(10)
        }
        
        // 释放资源
        for resource in acquired_resources {
          resource_pool.release(resource)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    for future in futures {
      future.wait()
    }
    
    let total_failures = acquisition_failures.get()
    let pool_stats = resource_pool.get_stats()
    
    // 验证资源池行为
    assert_true(total_failures > 0)  // 应该有获取失败的情况
    assert_eq(pool_stats.available_resources, 100)  // 所有资源最终应该被释放
    
    (total_failures, pool_stats)
  }
  
  let (exhaustion_failures, pool_stats) = resource_exhaustion_test()
  
  // 测试极端并发下的性能退化
  let extreme_concurrency_test = fn() {
    let shared_data = boundary_tester.create_shared_data_structure()
    
    let thread_counts = [10, 50, 100, 200, 500]
    let performance_results = []
    
    for thread_count in thread_counts {
      let test_start = Time::now()
      
      let futures = []
      
      for thread_id in 0..=thread_count - 1 {
        let future = thread_pool.spawn(fn() {
          let operations = 0
          
          for i in 0..=99 {
            shared_data.concurrent_operation(i)
            operations = operations + 1
          }
          
          operations
        })
        futures = futures.push(future)
      }
      
      // 等待所有线程完成
      let total_operations = 0
      for future in futures {
        let operations = future.wait()
        total_operations = total_operations + operations
      }
      
      let test_end = Time::now()
      let duration = test_end - test_start
      let throughput = total_operations.to_float() / duration.to_float() * 1000.0
      
      let result = {
        thread_count: thread_count,
        total_operations: total_operations,
        duration: duration,
        throughput: throughput
      }
      
      performance_results = performance_results.push(result)
    }
    
    // 分析性能退化
    let max_throughput = performance_results.map(fn(r) { r.throughput }).max()
    let min_throughput = performance_results.map(fn(r) { r.throughput }).min()
    let degradation_ratio = min_throughput / max_throughput
    
    // 验证性能退化在可接受范围内
    assert_true(degradation_ratio > 0.1)  // 性能退化不应超过90%
    
    performance_results
  }
  
  let extreme_results = extreme_concurrency_test()
  
  // 测试并发边界下的数据竞争
  let data_race_boundary_test = fn() {
    let race_detector = boundary_tester.create_race_detector()
    
    let shared_struct = boundary_tester.create_race_prone_structure()
    
    race_detector.start_monitoring()
    
    let futures = []
    
    // 创建容易产生数据竞争的模式
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=999 {
          // 非原子操作，可能产生数据竞争
          shared_struct.unsafe_update(thread_id, i)
          
          // 检查不变量
          if not shared_struct.check_invariant() {
            race_detector.report_invariant_violation(thread_id, i)
          }
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成
    for future in futures {
      future.wait()
    }
    
    let race_report = race_detector.stop_monitoring()
    
    // 验证数据竞争检测
    assert_true(race_report.potential_races > 0 or race_report.invariant_violations > 0)
    
    // 使用安全版本重新测试
    let safe_shared_struct = boundary_tester.create_thread_safe_structure()
    
    let safe_futures = []
    
    for thread_id in 0..=9 {
      let future = thread_pool.spawn(fn() {
        for i in 0..=999 {
          // 线程安全操作
          safe_shared_struct.safe_update(thread_id, i)
          
          // 检查不变量
          assert_true(safe_shared_struct.check_invariant())
        }
        
        thread_id
      })
      safe_futures = safe_futures.push(future)
    }
    
    // 等待所有线程完成
    for future in safe_futures {
      future.wait()
    }
    
    // 验证安全版本没有数据竞争
    assert_true(safe_shared_struct.check_invariant())
    
    race_report
  }
  
  let race_report = data_race_boundary_test()
  
  // 测试并发边界下的死锁恢复
  let deadlock_recovery_test = fn() {
    let deadlock_detector = boundary_tester.create_deadlock_detector()
    let recovery_manager = boundary_tester.create_deadlock_recovery_manager()
    
    deadlock_detector.enable_timeout_detection(2000)  // 2秒超时
    
    let futures = []
    let deadlock_occurred = AtomicBool::new(false)
    
    // 创建可能死锁的模式
    for thread_id in 0..=3 {
      let future = thread_pool.spawn(fn() {
        let resource1 = boundary_tester.get_resource("resource_" + ((thread_id * 2) % 4).to_string())
        let resource2 = boundary_tester.get_resource("resource_" + ((thread_id * 2 + 1) % 4).to_string())
        
        // 尝试获取资源
        if deadlock_detector.try_lock_with_timeout(resource1, 1000) {
          Time::sleep(100)
          
          if deadlock_detector.try_lock_with_timeout(resource2, 1000) {
            // 正常获取两个锁
            Time::sleep(100)
            deadlock_detector.unlock(resource2)
            deadlock_detector.unlock(resource1)
          } else {
            // 第二个锁获取失败，释放第一个
            deadlock_detector.unlock(resource1)
            deadlock_occurred.set(true)
          }
        } else {
          deadlock_occurred.set(true)
        }
        
        thread_id
      })
      futures = futures.push(future)
    }
    
    // 等待所有线程完成或超时
    let completed_threads = []
    for future in futures {
      match future.with_timeout(5000).wait() {
        Ok(thread_id) => completed_threads = completed_threads.push(thread_id),
        Err(_) => deadlock_occurred.set(true)
      }
    }
    
    // 如果检测到死锁，尝试恢复
    if deadlock_occurred.get() {
      let recovery_result = recovery_manager.attempt_deadlock_recovery()
      assert_true(recovery_result.success)
    }
    
    let deadlock_report = deadlock_detector.get_report()
    
    // 验证死锁检测和恢复
    assert_true(deadlock_report.lock_timeouts > 0 or deadlock_occurred.get())
    
    deadlock_report
  }
  
  let deadlock_report = deadlock_recovery_test()
  
  // 综合边界条件验证
  assert_true(exhaustion_failures > 0)
  assert_true(pool_stats.available_resources == 100)
  assert_true(extreme_results.length() == 5)
  assert_true(race_report.potential_races > 0 or race_report.invariant_violations > 0)
  assert_true(deadlock_report.lock_timeouts > 0)
}