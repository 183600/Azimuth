// Azimuth 并发安全测试用例
// 专注于遥测系统在并发环境下的安全性和一致性

// 测试1: 并发指标更新安全性
test "并发指标更新安全性测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent.safety.test")
  
  // 创建共享指标
  let shared_counter = Meter::create_counter(meter, "concurrent.counter")
  let shared_gauge = Meter::create_gauge(meter, "concurrent.gauge")
  let shared_histogram = Meter::create_histogram(meter, "concurrent.histogram")
  
  // 初始化指标
  Counter::add(shared_counter, 0.0)
  Gauge::set(shared_gauge, 0.0)
  
  // 创建并发任务
  let concurrent_tasks = []
  let num_tasks = 10
  let increments_per_task = 100
  
  // 启动多个并发任务更新计数器
  for task_id in 0..num_tasks {
    let task = ConcurrentTask::spawn(fn() {
      for i in 0..increments_per_task {
        // 每个任务增加计数器
        Counter::add(shared_counter, 1.0)
        
        // 随机更新仪表值
        let gauge_value = (task_id * increments_per_task + i).to_float()
        Gauge::set(shared_gauge, gauge_value)
        
        // 记录直方图数据
        Histogram::record(shared_histogram, 0.001 * i.to_float())
        
        // 模拟一些处理时间
        Thread::sleep(Duration::from_millis(1))
      }
    })
    concurrent_tasks = concurrent_tasks.push(task)
  }
  
  // 等待所有任务完成
  for task in concurrent_tasks {
    ConcurrentTask::join(task)
  }
  
  // 验证计数器的最终值
  let expected_counter_value = (num_tasks * increments_per_task).to_float()
  let actual_counter_value = Counter::value(shared_counter)
  assert_eq(actual_counter_value, expected_counter_value)
  
  // 验证仪表值（应该是某个任务设置的最后一个值）
  let gauge_value = Gauge::value(shared_gauge)
  assert_true(gauge_value >= 0.0)
  
  // 验证直方图数据
  let histogram_stats = Histogram::get_stats(shared_histogram)
  assert_true(histogram_stats.count > 0)
  assert_true(histogram_stats.sum > 0.0)
}
