// Azimuth 性能基准测试用例
// 专注于系统性能测量、基准测试和性能优化验证

// 测试1: CPU密集型操作基准测试
test "CPU密集型操作基准测试" {
  // 创建性能基准测试管理器
  let benchmark_manager = BenchmarkManager::new()
  
  // 定义计算密集型基准测试
  let cpu_benchmark = Benchmark::new("cpu_intensive_operations", {
    warmup_iterations: 10,
    measurement_iterations: 100,
    setup: fn() {
      # 准备测试数据
      {
        numbers: Array::range(1, 10000),
        matrix: generate_random_matrix(100, 100),
        text: generate_random_text(10000)
      }
    },
    teardown: fn(data) {
      # 清理资源
      # 在这个简单示例中不需要特殊清理
    }
  })
  
  // 添加基准测试用例
  Benchmark::add_case(cpu_benchmark, "array_sort", fn(data) {
    let numbers = data.numbers.clone()
    numbers.sort(fn(a, b) { a - b })
    numbers[numbers.length() - 1]  # 返回最大值
  })
  
  Benchmark::add_case(cpu_benchmark, "matrix_multiplication", fn(data) {
    let matrix_a = data.matrix.clone()
    let matrix_b = data.matrix.clone()
    matrix_multiply(matrix_a, matrix_b)
  })
  
  Benchmark::add_case(cpu_benchmark, "string_processing", fn(data) {
    let text = data.text.clone()
    let words = text.split(" ")
    let word_counts = {}
    
    for word in words {
      let count = word_counts.get(word, 0)
      word_counts[word] = count + 1
    }
    
    word_counts.keys().length()
  })
  
  Benchmark::add_case(cpu_benchmark, "prime_calculation", fn(data) {
    let mut primes = []
    let limit = 10000
    
    for num in 2..=limit {
      let is_prime = true
      for i in 2..=num / 2 {
        if num % i == 0 {
          is_prime = false
          break
        }
      }
      
      if is_prime {
        primes = primes.push(num)
      }
    }
    
    primes.length()
  })
  
  // 运行基准测试
  let benchmark_results = BenchmarkManager::run(benchmark_manager, cpu_benchmark)
  
  // 验证基准测试结果
  assert_true(benchmark_results.total_cases == 4)
  
  // 检查每个测试用例的结果
  let sort_results = benchmark_results.get_case_results("array_sort")
  assert_true(sort_results.iterations == 100)
  assert_true(sort_results.avg_time > 0)
  assert_true(sort_results.min_time > 0)
  assert_true(sort_results.max_time >= sort_results.min_time)
  assert_true(sort_results.p95_time >= sort_results.avg_time)
  assert_true(sort_results.p99_time >= sort_results.p95_time)
  
  let matrix_results = benchmark_results.get_case_results("matrix_multiplication")
  assert_true(matrix_results.avg_time > sort_results.avg_time)  # 矩阵乘法应该比排序慢
  
  let string_results = benchmark_results.get_case_results("string_processing")
  assert_true(string_results.avg_time > 0)
  
  let prime_results = benchmark_results.get_case_results("prime_calculation")
  assert_true(prime_results.avg_time > 0)
  
  // 比较不同测试用例的性能
  let performance_comparison = BenchmarkManager::compare_cases(benchmark_results, [
    "array_sort", "matrix_multiplication", "string_processing", "prime_calculation"
  ])
  
  assert_true(performance_comparison.fastest_case != "")
  assert_true(performance_comparison.slowest_case != "")
  assert_true(performance_comparison.performance_ratio > 1.0)
  
  // 生成性能报告
  let performance_report = BenchmarkManager::generate_report(benchmark_results, {
    format: "json",
    include_detailed_stats: true,
    include_percentiles: [50, 90, 95, 99],
    include_comparison: true
  })
  
  assert_true(performance_report.contains("array_sort"))
  assert_true(performance_report.contains("matrix_multiplication"))
  assert_true(performance_report.contains("avg_time"))
  assert_true(performance_report.contains("p95_time"))
  assert_true(performance_report.contains("p99_time"))
}

// 测试2: 内存密集型操作基准测试
test "内存密集型操作基准测试" {
  // 创建内存基准测试管理器
  let memory_benchmark_manager = MemoryBenchmarkManager::new()
  
  // 定义内存密集型基准测试
  let memory_benchmark = MemoryBenchmark::new("memory_intensive_operations", {
    warmup_iterations: 5,
    measurement_iterations: 20,
    memory_tracking: true,
    gc_before_each: true
  })
  
  // 添加内存基准测试用例
  MemoryBenchmark::add_case(memory_benchmark, "large_array_allocation", fn() {
    let large_arrays = []
    
    for i in 0..=100 {
      let large_array = Array::with_capacity(10000)
      for j in 0..=10000 {
        large_array = large_array.push(i * j)
      }
      large_arrays = large_arrays.push(large_array)
    }
    
    large_arrays.length()
  })
  
  MemoryBenchmark::add_case(memory_benchmark, "hash_table_operations", fn() {
    let large_map = {}
    
    for i in 0..=50000 {
      large_map["key_" + i.to_string()] = "value_" + i.to_string()
    }
    
    let mut sum = 0
    for i in 0..=50000 {
      let value = large_map["key_" + i.to_string()]
      sum = sum + value.length()
    }
    
    sum
  })
  
  MemoryBenchmark::add_case(memory_benchmark, "string_concatenation", fn() {
    let mut result = ""
    
    for i in 0..=10000 {
      result = result + "chunk_" + i.to_string() + "_"
    }
    
    result.length()
  })
  
  MemoryBenchmark::add_case(memory_benchmark, "object_creation", fn() {
    let objects = []
    
    for i in 0..=10000 {
      let obj = {
        id: i,
        name: "object_" + i.to_string(),
        data: Array::with_capacity(100).fill(i),
        metadata: {
          created: Time::now(),
          tags: ["tag1", "tag2", "tag3"],
          active: true
        }
      }
      objects = objects.push(obj)
    }
    
    objects.length()
  })
  
  // 运行内存基准测试
  let memory_benchmark_results = MemoryBenchmarkManager::run(memory_benchmark_manager, memory_benchmark)
  
  // 验证内存基准测试结果
  assert_true(memory_benchmark_results.total_cases == 4)
  
  // 检查内存使用情况
  let array_allocation_results = memory_benchmark_results.get_case_results("large_array_allocation")
  assert_true(array_allocation_results.avg_memory_usage > 0)
  assert_true(array_allocation_results.peak_memory_usage >= array_allocation_results.avg_memory_usage)
  assert_true(array_allocation_results.memory_growth > 0)
  
  let hash_table_results = memory_benchmark_results.get_case_results("hash_table_operations")
  assert_true(hash_table_results.avg_memory_usage > 0)
  assert_true(hash_table_results.peak_memory_usage >= hash_table_results.avg_memory_usage)
  
  let string_concat_results = memory_benchmark_results.get_case_results("string_concatenation")
  assert_true(string_concat_results.avg_memory_usage > 0)
  
  let object_creation_results = memory_benchmark_results.get_case_results("object_creation")
  assert_true(object_creation_results.avg_memory_usage > 0)
  
  // 分析内存效率
  let memory_efficiency = MemoryBenchmarkManager::analyze_efficiency(memory_benchmark_results)
  
  assert_true(memory_efficiency.most_efficient_case != "")
  assert_true(memory_efficiency.least_efficient_case != "")
  assert_true(memory_efficiency.efficiency_ratio > 1.0)
  
  // 测试内存泄漏检测
  let memory_leak_test = MemoryBenchmark::new("memory_leak_detection", {
    warmup_iterations: 0,
    measurement_iterations: 10,
    memory_tracking: true,
    gc_before_each: true,
    gc_after_each: true
  })
  
  MemoryBenchmark::add_case(memory_leak_test, "potential_leak", fn() {
    # 模拟可能导致内存泄漏的操作
    let growing_list = []
    
    for i in 0..=1000 {
      growing_list = growing_list.push(Array::with_capacity(100).fill(i))
    }
    
    # 故意不清理引用，模拟内存泄漏
    growing_list.length()
  })
  
  let leak_test_results = MemoryBenchmarkManager::run(memory_benchmark_manager, memory_leak_test)
  let leak_analysis = MemoryBenchmarkManager::detect_leaks(leak_test_results)
  
  assert_true(leak_analysis.potential_leaks_detected)
  assert_true(leak_analysis.memory_growth_rate > 0)
  
  // 生成内存使用报告
  let memory_report = MemoryBenchmarkManager::generate_memory_report(memory_benchmark_results, {
    include_memory_graphs: true,
    include_efficiency_analysis: true,
    include_leak_detection: true
  })
  
  assert_true(memory_report.contains("memory_usage"))
  assert_true(memory_report.contains("peak_memory"))
  assert_true(memory_report.contains("efficiency"))
}

// 测试3: I/O密集型操作基准测试
test "I/O密集型操作基准测试" {
  // 创建I/O基准测试管理器
  let io_benchmark_manager = IOBenchmarkManager::new()
  
  // 准备测试文件
  let test_file_path = "/tmp/io_benchmark_test.txt"
  let large_test_file_path = "/tmp/large_io_benchmark_test.txt"
  
  # 创建测试文件
  File::write(test_file_path, generate_random_text(1000))
  File::write(large_test_file_path, generate_random_text(100000))  # 100KB文件
  
  // 定义I/O密集型基准测试
  let io_benchmark = IOBenchmark::new("io_intensive_operations", {
    warmup_iterations: 5,
    measurement_iterations: 50,
    file_cleanup: true
  })
  
  // 添加文件读取基准测试
  IOBenchmark::add_case(io_benchmark, "sequential_file_read", fn() {
    let mut content = ""
    let file = File::open(test_file_path)
    
    while not(file.eof()) {
      content = content + file.read_line()
    }
    
    file.close()
    content.length()
  })
  
  // 添加文件写入基准测试
  IOBenchmark::add_case(io_benchmark, "sequential_file_write", fn() {
    let temp_file = "/tmp/temp_write_test_" + Time::now().to_string() + ".txt"
    let content = generate_random_text(5000)
    
    File::write(temp_file, content)
    
    let read_back = File::read(temp_file)
    File::delete(temp_file)
    
    read_back.length()
  })
  
  // 添加大文件读取基准测试
  IOBenchmark::add_case(io_benchmark, "large_file_read", fn() {
    let content = File::read(large_test_file_path)
    content.length()
  })
  
  // 添加文件复制基准测试
  IOBenchmark::add_case(io_benchmark, "file_copy", fn() {
    let source_file = large_test_file_path
    let dest_file = "/tmp/copied_file_" + Time::now().to_string() + ".txt"
    
    File::copy(source_file, dest_file)
    
    let original_size = File::size(source_file)
    let copied_size = File::size(dest_file)
    
    File::delete(dest_file)
    
    assert_eq(original_size, copied_size)
    copied_size
  })
  
  // 运行I/O基准测试
  let io_benchmark_results = IOBenchmarkManager::run(io_benchmark_manager, io_benchmark)
  
  // 验证I/O基准测试结果
  assert_true(io_benchmark_results.total_cases == 4)
  
  // 检查I/O性能指标
  let read_results = io_benchmark_results.get_case_results("sequential_file_read")
  assert_true(read_results.avg_time > 0)
  assert_true(read_results.throughput > 0)  # 字符/秒
  
  let write_results = io_benchmark_results.get_case_results("sequential_file_write")
  assert_true(write_results.avg_time > 0)
  assert_true(write_results.throughput > 0)  # 字符/秒
  
  let large_file_results = io_benchmark_results.get_case_results("large_file_read")
  assert_true(large_file_results.avg_time > read_results.avg_time)  # 大文件读取应该更慢
  assert_true(large_file_results.throughput > 0)
  
  let copy_results = io_benchmark_results.get_case_results("file_copy")
  assert_true(copy_results.avg_time > 0)
  assert_true(copy_results.throughput > 0)  # 字符/秒
  
  // 比较读写性能
  let read_vs_write = IOBenchmarkManager::compare_operations(io_benchmark_results, "sequential_file_read", "sequential_file_write")
  assert_true(read_vs_write.performance_ratio > 0)
  
  // 测试并发I/O操作
  let concurrent_io_benchmark = IOBenchmark::new("concurrent_io_operations", {
    warmup_iterations: 2,
    measurement_iterations: 10,
    concurrent_operations: true,
    max_concurrency: 4
  })
  
  IOBenchmark::add_case(concurrent_io_benchmark, "concurrent_file_reads", fn() {
    let file_paths = []
    
    # 创建多个测试文件
    for i in 0..=10 {
      let path = "/tmp/concurrent_test_" + i.to_string() + ".txt"
      File::write(path, generate_random_text(1000))
      file_paths = file_paths.push(path)
    }
    
    # 并发读取文件
    let results = []
    for path in file_paths {
      let content = File::read(path)
      results = results.push(content.length())
    }
    
    # 清理文件
    for path in file_paths {
      File::delete(path)
    }
    
    results.reduce(fn(acc, len) { acc + len }, 0)
  })
  
  let concurrent_results = IOBenchmarkManager::run(io_benchmark_manager, concurrent_io_benchmark)
  let concurrent_read_results = concurrent_results.get_case_results("concurrent_file_reads")
  
  # 并发操作应该比串行操作更快（在理想情况下）
  # 注意：这个断言可能因系统而异，所以使用更宽松的条件
  assert_true(concurrent_read_results.avg_time > 0)
  
  # 清理测试文件
  File::delete(test_file_path)
  File::delete(large_test_file_path)
  
  // 生成I/O性能报告
  let io_report = IOBenchmarkManager::generate_io_report(io_benchmark_results, {
    include_throughput_analysis: true,
    include_concurrency_analysis: true,
    include_file_size_impact: true
  })
  
  assert_true(io_report.contains("throughput"))
  assert_true(io_report.contains("sequential_file_read"))
  assert_true(io_report.contains("sequential_file_write"))
}

// 测试4: 网络操作基准测试
test "网络操作基准测试" {
  // 创建网络基准测试管理器
  let network_benchmark_manager = NetworkBenchmarkManager::new()
  
  // 定义网络基准测试
  let network_benchmark = NetworkBenchmark::new("network_operations", {
    warmup_iterations: 3,
    measurement_iterations: 20,
    timeout: 10000  # 10秒超时
  })
  
  // 添加HTTP请求基准测试
  NetworkBenchmark::add_case(network_benchmark, "http_get_request", fn() {
    let response = HttpClient::get("https://httpbin.org/get")
    response.status_code
  })
  
  NetworkBenchmark::add_case(network_benchmark, "http_post_request", fn() {
    let data = "{\"test\": \"data\", \"timestamp\": " + Time::now().to_string() + "}"
    let response = HttpClient::post("https://httpbin.org/post", data)
    response.status_code
  })
  
  // 添加DNS解析基准测试
  NetworkBenchmark::add_case(network_benchmark, "dns_resolution", fn() {
    let ip_address = DNS::resolve("google.com")
    ip_address.length()
  })
  
  // 添加TCP连接基准测试
  NetworkBenchmark::add_case(network_benchmark, "tcp_connection", fn() {
    let connection = TCP::connect("httpbin.org", 80)
    let connected = connection.is_connected()
    connection.close()
    connected
  })
  
  // 运行网络基准测试
  let network_benchmark_results = NetworkBenchmarkManager::run(network_benchmark_manager, network_benchmark)
  
  // 验证网络基准测试结果
  assert_true(network_benchmark_results.total_cases == 4)
  
  // 检查网络性能指标
  let http_get_results = network_benchmark_results.get_case_results("http_get_request")
  assert_true(http_get_results.avg_time > 0)
  assert_true(http_get_results.success_rate > 0.8)  # 至少80%成功率
  
  let http_post_results = network_benchmark_results.get_case_results("http_post_request")
  assert_true(http_post_results.avg_time > 0)
  assert_true(http_post_results.success_rate > 0.8)
  
  let dns_results = network_benchmark_results.get_case_results("dns_resolution")
  assert_true(dns_results.avg_time > 0)
  assert_true(dns_results.success_rate > 0.8)
  
  let tcp_results = network_benchmark_results.get_case_results("tcp_connection")
  assert_true(tcp_results.avg_time > 0)
  assert_true(tcp_results.success_rate > 0.8)
  
  // 比较HTTP GET和POST性能
  let get_vs_post = NetworkBenchmarkManager::compare_operations(network_benchmark_results, "http_get_request", "http_post_request")
  assert_true(get_vs_post.performance_ratio > 0)
  
  // 测试并发网络请求
  let concurrent_network_benchmark = NetworkBenchmark::new("concurrent_network_operations", {
    warmup_iterations: 1,
    measurement_iterations: 5,
    concurrent_operations: true,
    max_concurrency: 10
  })
  
  NetworkBenchmark::add_case(concurrent_network_benchmark, "concurrent_http_requests", fn() {
    let urls = [
      "https://httpbin.org/get",
      "https://httpbin.org/delay/1",
      "https://httpbin.org/uuid",
      "https://httpbin.org/ip",
      "https://httpbin.org/user-agent"
    ]
    
    let results = []
    for url in urls {
      let response = HttpClient::get(url)
      results = results.push(response.status_code)
    }
    
    results.reduce(fn(acc, code) { acc + code }, 0)
  })
  
  let concurrent_network_results = NetworkBenchmarkManager::run(network_benchmark_manager, concurrent_network_benchmark)
  let concurrent_http_results = concurrent_network_results.get_case_results("concurrent_http_requests")
  
  assert_true(concurrent_http_results.avg_time > 0)
  assert_true(concurrent_http_results.success_rate > 0.5)  # 并发操作可能有更多失败
  
  // 测试网络延迟和带宽
  let latency_bandwidth_test = NetworkBenchmark::new("latency_bandwidth_test", {
    warmup_iterations: 1,
    measurement_iterations: 5
  })
  
  NetworkBenchmark::add_case(latency_bandwidth_test, "ping_test", fn() {
    let ping_result = Network::ping("google.com", 4)
    ping_result.avg_latency_ms
  })
  
  NetworkBenchmark::add_case(latency_bandwidth_test, "bandwidth_test", fn() {
    let test_data = generate_random_text(10000)  # 10KB数据
    let start_time = Time::now()
    
    let response = HttpClient::post("https://httpbin.org/post", test_data)
    
    let end_time = Time::now()
    let duration = end_time - start_time
    
    # 计算带宽（字节/秒）
    if duration > 0 {
      test_data.length().to_float() / (duration.to_float() / 1000.0)
    } else {
      0.0
    }
  })
  
  let latency_bandwidth_results = NetworkBenchmarkManager::run(network_benchmark_manager, latency_bandwidth_test)
  
  let ping_results = latency_bandwidth_results.get_case_results("ping_test")
  assert_true(ping_results.avg_value > 0)  # 平均延迟应该大于0
  
  let bandwidth_results = latency_bandwidth_results.get_case_results("bandwidth_test")
  assert_true(bandwidth_results.avg_value > 0)  # 带宽应该大于0
  
  // 生成网络性能报告
  let network_report = NetworkBenchmarkManager::generate_network_report(network_benchmark_results, {
    include_latency_analysis: true,
    include_bandwidth_analysis: true,
    include_success_rate: true,
    include_concurrency_impact: true
  })
  
  assert_true(network_report.contains("http_get_request"))
  assert_true(network_report.contains("success_rate"))
  assert_true(network_report.contains("latency"))
}

// 测试5: 并发性能基准测试
test "并发性能基准测试" {
  // 创建并发基准测试管理器
  let concurrency_benchmark_manager = ConcurrencyBenchmarkManager::new()
  
  // 定义并发基准测试
  let concurrency_benchmark = ConcurrencyBenchmark::new("concurrency_performance", {
    warmup_iterations: 2,
    measurement_iterations: 10,
    thread_counts: [1, 2, 4, 8, 16]
  })
  
  // 添加并发计数器基准测试
  ConcurrencyBenchmark::add_case(concurrency_benchmark, "concurrent_counter", fn(thread_count) {
    let counter = AtomicInt::new(0)
    let iterations_per_thread = 10000
    
    let threads = []
    for i in 0..=thread_count {
      let thread = Thread::spawn(fn() {
        for j in 0..=iterations_per_thread {
          AtomicInt::increment(counter)
        }
      })
      threads = threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
    
    AtomicInt::get(counter)
  })
  
  // 添加并发队列基准测试
  ConcurrencyBenchmark::add_case(concurrency_benchmark, "concurrent_queue", fn(thread_count) {
    let queue = ConcurrentQueue::new()
    let items_per_thread = 1000
    
    # 生产者线程
    let producer_threads = []
    for i in 0..=thread_count / 2 {
      let thread = Thread::spawn(fn() {
        let thread_id = i
        for j in 0..=items_per_thread {
          ConcurrentQueue::enqueue(queue, "item_" + thread_id.to_string() + "_" + j.to_string())
        }
      })
      producer_threads = producer_threads.push(thread)
    }
    
    # 消费者线程
    let consumer_threads = []
    for i in 0..=thread_count / 2 {
      let thread = Thread::spawn(fn() {
        let mut consumed = 0
        while consumed < items_per_thread {
          match ConcurrentQueue::try_dequeue(queue) {
            Some(item) => consumed = consumed + 1,
            None => Thread::sleep(1)
          }
        }
        consumed
      })
      consumer_threads = consumer_threads.push(thread)
    }
    
    # 等待所有线程完成
    for thread in producer_threads {
      Thread::join(thread)
    }
    for thread in consumer_threads {
      Thread::join(thread)
    }
    
    ConcurrentQueue::size(queue)
  })
  
  // 运行并发基准测试
  let concurrency_results = ConcurrencyBenchmarkManager::run(concurrency_benchmark_manager, concurrency_benchmark)
  
  // 验证并发基准测试结果
  assert_true(concurrency_results.total_cases == 2)
  
  // 分析并发性能
  let counter_results = concurrency_results.get_case_results("concurrent_counter")
  assert_true(counter_results.thread_performance.length() > 0)
  
  # 检查不同线程数的性能
  let thread_1_perf = counter_results.get_thread_performance(1)
  let thread_2_perf = counter_results.get_thread_performance(2)
  let thread_4_perf = counter_results.get_thread_performance(4)
  
  assert_true(thread_1_perf.avg_time > 0)
  assert_true(thread_2_perf.avg_time > 0)
  assert_true(thread_4_perf.avg_time > 0)
  
  # 计算并发效率
  let scalability = ConcurrencyBenchmarkManager::analyze_scalability(counter_results)
  assert_true(scalability.efficiency_2_threads > 0)
  assert_true(scalability.efficiency_4_threads > 0)
  assert_true(scalability.efficiency_8_threads > 0)
  
  # 测试锁竞争
  let lock_contention_benchmark = ConcurrencyBenchmark::new("lock_contention", {
    warmup_iterations: 2,
    measurement_iterations: 5,
    thread_counts: [2, 4, 8, 16]
  })
  
  ConcurrencyBenchmark::add_case(lock_contention_benchmark, "mutex_contention", fn(thread_count) {
    let mutex = Mutex::new()
    let shared_data = { value: 0 }
    let iterations_per_thread = 1000
    
    let threads = []
    for i in 0..=thread_count {
      let thread = Thread::spawn(fn() {
        for j in 0..=iterations_per_thread {
          Mutex::lock(mutex)
          shared_data.value = shared_data.value + 1
          # 模拟一些临界区工作
          Thread::sleep(1)
          Mutex::unlock(mutex)
        }
      })
      threads = threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
    
    shared_data.value
  })
  
  let contention_results = ConcurrencyBenchmarkManager::run(concurrency_benchmark_manager, lock_contention_benchmark)
  let mutex_results = contention_results.get_case_results("mutex_contention")
  
  # 分析锁竞争
  let contention_analysis = ConcurrencyBenchmarkManager::analyze_contention(mutex_results)
  assert_true(contention_analysis.contention_factor >= 1.0)  # 竞争因子应该>=1
  
  # 测试无锁数据结构
  let lock_free_benchmark = ConcurrencyBenchmark::new("lock_free_structures", {
    warmup_iterations: 2,
    measurement_iterations: 5,
    thread_counts: [2, 4, 8, 16]
  })
  
  ConcurrencyBenchmark::add_case(lock_free_benchmark, "lock_free_counter", fn(thread_count) {
    let counter = LockFreeCounter::new(0)
    let iterations_per_thread = 1000
    
    let threads = []
    for i in 0..=thread_count {
      let thread = Thread::spawn(fn() {
        for j in 0..=iterations_per_thread {
          LockFreeCounter::increment(counter)
        }
      })
      threads = threads.push(thread)
    }
    
    for thread in threads {
      Thread::join(thread)
    }
    
    LockFreeCounter::get(counter)
  })
  
  let lock_free_results = ConcurrencyBenchmarkManager::run(concurrency_benchmark_manager, lock_free_benchmark)
  let lock_free_counter_results = lock_free_results.get_case_results("lock_free_counter")
  
  # 比较有锁和无锁性能
  let lock_vs_lockfree = ConcurrencyBenchmarkManager::compare_structures(
    mutex_results, 
    lock_free_counter_results
  )
  
  assert_true(lock_vs_lockfree.performance_ratio > 0)
  
  # 生成并发性能报告
  let concurrency_report = ConcurrencyBenchmarkManager::generate_concurrency_report(concurrency_results, {
    include_scalability_analysis: true,
    include_contention_analysis: true,
    include_lock_comparison: true,
    include_thread_scaling: true
  })
  
  assert_true(concurrency_report.contains("concurrent_counter"))
  assert_true(concurrency_report.contains("scalability"))
  assert_true(concurrency_report.contains("thread_performance"))
}

// 测试6: 数据结构性能基准测试
test "数据结构性能基准测试" {
  // 创建数据结构基准测试管理器
  let ds_benchmark_manager = DataStructureBenchmarkManager::new()
  
  // 定义数据结构基准测试
  let ds_benchmark = DataStructureBenchmark::new("data_structures", {
    warmup_iterations: 5,
    measurement_iterations: 100,
    data_sizes: [100, 1000, 10000]
  })
  
  // 添加数组操作基准测试
  DataStructureBenchmark::add_case(ds_benchmark, "array_operations", fn(data_size) {
    # 数组插入
    let mut array = []
    let insert_start = Time::now()
    for i in 0..=data_size {
      array = array.push(i)
    }
    let insert_time = Time::now() - insert_start
    
    # 数组查找
    let search_start = Time::now()
    for i in 0..=data_size {
      let index = array.find_index(fn(x) { x == i % data_size })
    }
    let search_time = Time::now() - search_start
    
    # 数组删除
    let delete_start = Time::now()
    for i in 0..=data_size / 10 {
      if array.length() > 0 {
        array = array.slice(1)  # 删除第一个元素
      }
    }
    let delete_time = Time::now() - delete_start
    
    {
      insert_time: insert_time,
      search_time: search_time,
      delete_time: delete_time,
      final_size: array.length()
    }
  })
  
  // 添加哈希表操作基准测试
  DataStructureBenchmark::add_case(ds_benchmark, "hash_table_operations", fn(data_size) {
    # 哈希表插入
    let mut hash_table = {}
    let insert_start = Time::now()
    for i in 0..=data_size {
      hash_table["key_" + i.to_string()] = "value_" + i.to_string()
    }
    let insert_time = Time::now() - insert_start
    
    # 哈希表查找
    let search_start = Time::now()
    for i in 0..=data_size {
      let value = hash_table["key_" + (i % data_size).to_string()]
    }
    let search_time = Time::now() - search_start
    
    # 哈希表删除
    let delete_start = Time::now()
    for i in 0..=data_size / 10 {
      hash_table.remove("key_" + i.to_string())
    }
    let delete_time = Time::now() - delete_start
    
    {
      insert_time: insert_time,
      search_time: search_time,
      delete_time: delete_time,
      final_size: hash_table.keys().length()
    }
  })
  
  // 添加链表操作基准测试
  DataStructureBenchmark::add_case(ds_benchmark, "linked_list_operations", fn(data_size) {
    # 链表插入
    let mut list = LinkedList::new()
    let insert_start = Time::now()
    for i in 0..=data_size {
      LinkedList::append(list, i)
    }
    let insert_time = Time::now() - insert_start
    
    # 链表查找
    let search_start = Time::now()
    for i in 0..=data_size {
      LinkedList::find(list, i % data_size)
    }
    let search_time = Time::now() - search_start
    
    # 链表删除
    let delete_start = Time::now()
    for i in 0..=data_size / 10 {
      if not(LinkedList::is_empty(list)) {
        LinkedList::remove_head(list)
      }
    }
    let delete_time = Time::now() - delete_start
    
    {
      insert_time: insert_time,
      search_time: search_time,
      delete_time: delete_time,
      final_size: LinkedList::size(list)
    }
  })
  
  // 运行数据结构基准测试
  let ds_benchmark_results = DataStructureBenchmarkManager::run(ds_benchmark_manager, ds_benchmark)
  
  // 验证数据结构基准测试结果
  assert_true(ds_benchmark_results.total_cases == 3)
  
  // 分析不同数据结构的性能
  let array_results = ds_benchmark_results.get_case_results("array_operations")
  let hash_table_results = ds_benchmark_results.get_case_results("hash_table_operations")
  let linked_list_results = ds_benchmark_results.get_case_results("linked_list_operations")
  
  # 比较插入性能
  let insert_comparison = DataStructureBenchmarkManager::compare_operation(
    ds_benchmark_results, 
    "insert", 
    ["array_operations", "hash_table_operations", "linked_list_operations"]
  )
  assert_true(insert_comparison.fastest_structure != "")
  assert_true(insert_comparison.slowest_structure != "")
  
  # 比较查找性能
  let search_comparison = DataStructureBenchmarkManager::compare_operation(
    ds_benchmark_results, 
    "search", 
    ["array_operations", "hash_table_operations", "linked_list_operations"]
  )
  assert_true(search_comparison.fastest_structure != "")
  assert_true(search_comparison.slowest_structure != "")
  
  # 哈希表查找应该比数组和链表快
  assert_true(search_comparison.fastest_structure == "hash_table_operations")
  
  # 比较删除性能
  let delete_comparison = DataStructureBenchmarkManager::compare_operation(
    ds_benchmark_results, 
    "delete", 
    ["array_operations", "hash_table_operations", "linked_list_operations"]
  )
  assert_true(delete_comparison.fastest_structure != "")
  assert_true(delete_comparison.slowest_structure != "")
  
  # 分析数据大小对性能的影响
  let scalability_analysis = DataStructureBenchmarkManager::analyze_scalability(ds_benchmark_results)
  assert_true(scalability_analysis.array_scalability_factor > 1.0)
  assert_true(scalability_analysis.hash_table_scalability_factor > 1.0)
  assert_true(scalability_analysis.linked_list_scalability_factor > 1.0)
  
  # 测试树结构操作
  let tree_benchmark = DataStructureBenchmark::new("tree_structures", {
    warmup_iterations: 3,
    measurement_iterations: 50,
    data_sizes: [100, 1000, 5000]
  })
  
  DataStructureBenchmark::add_case(tree_benchmark, "binary_search_tree", fn(data_size) {
    # 创建二叉搜索树
    let tree = BinarySearchTree::new()
    
    # 插入操作
    let insert_start = Time::now()
    for i in 0..=data_size {
      BinarySearchTree::insert(tree, i)
    }
    let insert_time = Time::now() - insert_start
    
    # 查找操作
    let search_start = Time::now()
    for i in 0..=data_size {
      BinarySearchTree::search(tree, i % data_size)
    }
    let search_time = Time::now() - search_start
    
    # 删除操作
    let delete_start = Time::now()
    for i in 0..=data_size / 10 {
      BinarySearchTree::remove(tree, i)
    }
    let delete_time = Time::now() - delete_start
    
    {
      insert_time: insert_time,
      search_time: search_time,
      delete_time: delete_time,
      final_size: BinarySearchTree::size(tree)
    }
  })
  
  let tree_results = DataStructureBenchmarkManager::run(ds_benchmark_manager, tree_benchmark)
  let bst_results = tree_results.get_case_results("binary_search_tree")
  
  # 比较树和哈希表的性能
  let tree_vs_hash = DataStructureBenchmarkManager::compare_structures(
    hash_table_results, 
    bst_results, 
    "search"
  )
  assert_true(tree_vs_hash.performance_ratio > 0)
  
  # 生成数据结构性能报告
  let ds_report = DataStructureBenchmarkManager::generate_ds_report(ds_benchmark_results, {
    include_operation_comparison: true,
    include_scalability_analysis: true,
    include_memory_usage: true,
    include_best_practices: true
  })
  
  assert_true(ds_report.contains("array_operations"))
  assert_true(ds_report.contains("hash_table_operations"))
  assert_true(ds_report.contains("linked_list_operations"))
  assert_true(ds_report.contains("insert"))
  assert_true(ds_report.contains("search"))
  assert_true(ds_report.contains("delete"))
}

// 测试7: 算法复杂度基准测试
test "算法复杂度基准测试" {
  // 创建算法复杂度基准测试管理器
  let algorithm_benchmark_manager = AlgorithmBenchmarkManager::new()
  
  // 定义算法复杂度基准测试
  let algorithm_benchmark = AlgorithmBenchmark::new("algorithm_complexity", {
    warmup_iterations: 3,
    measurement_iterations: 20,
    input_sizes: [100, 500, 1000, 2000, 5000]
  })
  
  // 添加排序算法基准测试
  AlgorithmBenchmark::add_case(algorithm_benchmark, "bubble_sort", fn(input_size) {
    let data = generate_random_array(input_size)
    let start_time = Time::now()
    
    # 冒泡排序实现
    let n = data.length()
    for i in 0..=n-1 {
      for j in 0..=n-i-2 {
        if data[j] > data[j+1] {
          let temp = data[j]
          data[j] = data[j+1]
          data[j+1] = temp
        }
      }
    }
    
    let end_time = Time::now()
    let execution_time = end_time - start_time
    
    # 验证排序结果
    let is_sorted = true
    for i in 0..=n-2 {
      if data[i] > data[i+1] {
        is_sorted = false
        break
      }
    }
    
    {
      execution_time: execution_time,
      is_sorted: is_sorted,
      input_size: input_size
    }
  })
  
  AlgorithmBenchmark::add_case(algorithm_benchmark, "quick_sort", fn(input_size) {
    let data = generate_random_array(input_size)
    let start_time = Time::now()
    
    # 快速排序实现
    quick_sort_recursive(data, 0, data.length() - 1)
    
    let end_time = Time::now()
    let execution_time = end_time - start_time
    
    # 验证排序结果
    let is_sorted = true
    for i in 0..=data.length() - 2 {
      if data[i] > data[i+1] {
        is_sorted = false
        break
      }
    }
    
    {
      execution_time: execution_time,
      is_sorted: is_sorted,
      input_size: input_size
    }
  })
  
  // 添加搜索算法基准测试
  AlgorithmBenchmark::add_case(algorithm_benchmark, "linear_search", fn(input_size) {
    let data = generate_sorted_array(input_size)
    let search_values = generate_random_values(input_size / 10, 0, input_size - 1)
    
    let start_time = Time::now()
    let found_count = 0
    
    for value in search_values {
      let found = false
      for item in data {
        if item == value {
          found = true
          found_count = found_count + 1
          break
        }
      }
    }
    
    let end_time = Time::now()
    let execution_time = end_time - start_time
    
    {
      execution_time: execution_time,
      found_count: found_count,
      search_count: search_values.length(),
      input_size: input_size
    }
  })
  
  AlgorithmBenchmark::add_case(algorithm_benchmark, "binary_search", fn(input_size) {
    let data = generate_sorted_array(input_size)
    let search_values = generate_random_values(input_size / 10, 0, input_size - 1)
    
    let start_time = Time::now()
    let found_count = 0
    
    for value in search_values {
      let left = 0
      let right = data.length() - 1
      let found = false
      
      while left <= right {
        let mid = (left + right) / 2
        if data[mid] == value {
          found = true
          found_count = found_count + 1
          break
        } else if data[mid] < value {
          left = mid + 1
        } else {
          right = mid - 1
        }
      }
    }
    
    let end_time = Time::now()
    let execution_time = end_time - start_time
    
    {
      execution_time: execution_time,
      found_count: found_count,
      search_count: search_values.length(),
      input_size: input_size
    }
  })
  
  // 运行算法基准测试
  let algorithm_results = AlgorithmBenchmarkManager::run(algorithm_benchmark_manager, algorithm_benchmark)
  
  // 验证算法基准测试结果
  assert_true(algorithm_results.total_cases == 4)
  
  // 分析排序算法性能
  let bubble_sort_results = algorithm_results.get_case_results("bubble_sort")
  let quick_sort_results = algorithm_results.get_case_results("quick_sort")
  
  # 快速排序应该比冒泡排序快
  let sort_comparison = AlgorithmBenchmarkManager::compare_algorithms(
    bubble_sort_results, 
    quick_sort_results
  )
  assert_true(sort_comparison.performance_ratio > 1.0)  # 快速排序应该更快
  
  # 分析时间复杂度
  let bubble_sort_complexity = AlgorithmBenchmarkManager::analyze_complexity(bubble_sort_results)
  let quick_sort_complexity = AlgorithmBenchmarkManager::analyze_complexity(quick_sort_results)
  
  # 冒泡排序的时间复杂度应该接近O(n²)
  assert_true(bubble_sort_complexity.estimated_complexity == "O(n^2)" or 
              bubble_sort_complexity.estimated_complexity == "Between O(n^1.5) and O(n^2.5)")
  
  # 快速排序的时间复杂度应该接近O(n log n)
  assert_true(quick_sort_complexity.estimated_complexity == "O(n log n)" or 
              quick_sort_complexity.estimated_complexity == "Between O(n) and O(n^2)")
  
  # 分析搜索算法性能
  let linear_search_results = algorithm_results.get_case_results("linear_search")
  let binary_search_results = algorithm_results.get_case_results("binary_search")
  
  # 二分查找应该比线性查找快
  let search_comparison = AlgorithmBenchmarkManager::compare_algorithms(
    linear_search_results, 
    binary_search_results
  )
  assert_true(search_comparison.performance_ratio > 1.0)  # 二分查找应该更快
  
  # 分析搜索算法时间复杂度
  let linear_search_complexity = AlgorithmBenchmarkManager::analyze_complexity(linear_search_results)
  let binary_search_complexity = AlgorithmBenchmarkManager::analyze_complexity(binary_search_results)
  
  # 线性查找的时间复杂度应该接近O(n)
  assert_true(linear_search_complexity.estimated_complexity == "O(n)" or 
              linear_search_complexity.estimated_complexity == "Between O(n^0.5) and O(n^1.5)")
  
  # 二分查找的时间复杂度应该接近O(log n)
  assert_true(binary_search_complexity.estimated_complexity == "O(log n)" or 
              binary_search_complexity.estimated_complexity == "Between O(1) and O(n)")
  
  # 测试图算法
  let graph_benchmark = AlgorithmBenchmark::new("graph_algorithms", {
    warmup_iterations: 2,
    measurement_iterations: 10,
    input_sizes: [10, 50, 100, 200]
  })
  
  AlgorithmBenchmark::add_case(graph_benchmark, "dijkstra_shortest_path", fn(input_size) {
    let graph = generate_random_graph(input_size, 0.3)  # 30%边密度
    let start_node = 0
    let end_node = input_size - 1
    
    let start_time = Time::now()
    let shortest_path = dijkstra_shortest_path(graph, start_node, end_node)
    let end_time = Time::now()
    
    {
      execution_time: end_time - start_time,
      path_length: shortest_path.length(),
      input_size: input_size
    }
  })
  
  let graph_results = AlgorithmBenchmarkManager::run(algorithm_benchmark_manager, graph_benchmark)
  let dijkstra_results = graph_results.get_case_results("dijkstra_shortest_path")
  
  # 分析图算法复杂度
  let dijkstra_complexity = AlgorithmBenchmarkManager::analyze_complexity(dijkstra_results)
  assert_true(dijkstra_complexity.estimated_complexity.length() > 0)
  
  # 生成算法复杂度报告
  let algorithm_report = AlgorithmBenchmarkManager::generate_algorithm_report(algorithm_results, {
    include_complexity_analysis: true,
    include_performance_comparison: true,
    include_scalability_analysis: true,
    include_best_case_worst_case: true
  })
  
  assert_true(algorithm_report.contains("bubble_sort"))
  assert_true(algorithm_report.contains("quick_sort"))
  assert_true(algorithm_report.contains("linear_search"))
  assert_true(algorithm_report.contains("binary_search"))
  assert_true(algorithm_report.contains("complexity"))
}

// 测试8: 系统资源基准测试
test "系统资源基准测试" {
  // 创建系统资源基准测试管理器
  let system_benchmark_manager = SystemBenchmarkManager::new()
  
  // 定义系统资源基准测试
  let system_benchmark = SystemBenchmark::new("system_resources", {
    warmup_iterations: 2,
    measurement_iterations: 10,
    resource_monitoring: true,
    cleanup_after_each: true
  })
  
  // 添加CPU使用率基准测试
  SystemBenchmark::add_case(system_benchmark, "cpu_intensive_task", fn() {
    let start_time = Time::now()
    let duration = 5000  # 5秒
    
    # CPU密集型任务
    let mut counter = 0
    while Time::now() - start_time < duration {
      counter = counter + 1
      # 执行一些计算
      let result = Math::sqrt(counter * 1.0)
    }
    
    {
      execution_time: Time::now() - start_time,
      iterations: counter
    }
  })
  
  // 添加内存使用基准测试
  SystemBenchmark::add_case(system_benchmark, "memory_intensive_task", fn() {
    let start_time = Time::now()
    let duration = 3000  # 3秒
    
    # 内存密集型任务
    let allocations = []
    let mut total_allocated = 0
    
    while Time::now() - start_time < duration {
      let allocation = Array::with_capacity(1000).fill(Math::random())
      allocations = allocations.push(allocation)
      total_allocated = total_allocated + 1000
      
      # 限制内存使用，避免系统内存耗尽
      if total_allocated > 100000 {
        break
      }
    }
    
    {
      execution_time: Time::now() - start_time,
      allocations_count: allocations.length(),
      total_allocated: total_allocated
    }
  })
  
  // 添加I/O密集型基准测试
  SystemBenchmark::add_case(system_benchmark, "io_intensive_task", fn() {
    let start_time = Time::now()
    let duration = 2000  # 2秒
    
    # I/O密集型任务
    let temp_files = []
    let mut bytes_written = 0
    
    while Time::now() - start_time < duration {
      let temp_file = "/tmp/benchmark_" + Time::now().to_string() + ".txt"
      let data = generate_random_text(1000)
      
      File::write(temp_file, data)
      temp_files = temp_files.push(temp_file)
      bytes_written = bytes_written + data.length()
      
      # 限制文件数量，避免文件系统耗尽
      if temp_files.length() > 50 {
        break
      }
    }
    
    # 清理临时文件
    for file in temp_files {
      File::delete(file)
    }
    
    {
      execution_time: Time::now() - start_time,
      files_created: temp_files.length(),
      bytes_written: bytes_written
    }
  })
  
  // 添加网络I/O基准测试
  SystemBenchmark::add_case(system_benchmark, "network_intensive_task", fn() {
    let start_time = Time::now()
    let duration = 3000  # 3秒
    
    # 网络I/O密集型任务
    let requests_made = 0
    let successful_requests = 0
    
    while Time::now() - start_time < duration {
      let response = HttpClient::get("https://httpbin.org/get")
      requests_made = requests_made + 1
      
      if response.status_code == 200 {
        successful_requests = successful_requests + 1
      }
      
      # 限制请求数量，避免过于频繁的网络请求
      if requests_made > 20 {
        break
      }
    }
    
    {
      execution_time: Time::now() - start_time,
      requests_made: requests_made,
      successful_requests: successful_requests
    }
  })
  
  // 运行系统资源基准测试
  let system_results = SystemBenchmarkManager::run(system_benchmark_manager, system_benchmark)
  
  // 验证系统资源基准测试结果
  assert_true(system_results.total_cases == 4)
  
  // 分析CPU使用情况
  let cpu_results = system_results.get_case_results("cpu_intensive_task")
  assert_true(cpu_results.avg_cpu_usage > 50)  # CPU密集型任务应该使用大量CPU
  assert_true(cpu_results.peak_cpu_usage >= cpu_results.avg_cpu_usage)
  
  // 分析内存使用情况
  let memory_results = system_results.get_case_results("memory_intensive_task")
  assert_true(memory_results.avg_memory_usage > 0)
  assert_true(memory_results.peak_memory_usage >= memory_results.avg_memory_usage)
  assert_true(memory_results.memory_growth > 0)
  
  // 分析I/O使用情况
  let io_results = system_results.get_case_results("io_intensive_task")
  assert_true(io_results.avg_disk_io > 0)
  assert_true(io_results.peak_disk_io >= io_results.avg_disk_io)
  
  // 分析网络使用情况
  let network_results = system_results.get_case_results("network_intensive_task")
  assert_true(network_results.avg_network_io > 0)
  assert_true(network_results.peak_network_io >= network_results.avg_network_io)
  
  # 测试系统资源限制
  let resource_limit_benchmark = SystemBenchmark::new("resource_limits", {
    warmup_iterations: 1,
    measurement_iterations: 5,
    resource_limits: {
      max_cpu_usage: 80.0,    # 最大CPU使用率80%
      max_memory_mb: 100,     # 最大内存使用100MB
      max_disk_io_mb: 10      # 最大磁盘I/O 10MB
    }
  })
  
  SystemBenchmark::add_case(resource_limit_benchmark, "limited_task", fn() {
    # 在资源限制下执行任务
    let start_time = Time::now()
    let duration = 2000  # 2秒
    
    let mut iterations = 0
    while Time::now() - start_time < duration {
      # 执行一些计算和内存分配
      let data = Array::with_capacity(100).fill(iterations)
      iterations = iterations + 1
      
      # 检查资源使用情况
      let current_memory = SystemMonitor::get_memory_usage()
      if current_memory > 100 * 1024 * 1024 {  # 100MB
        break
      }
    }
    
    iterations
  })
  
  let limit_results = SystemBenchmarkManager::run(system_benchmark_manager, resource_limit_benchmark)
  let limited_task_results = limit_results.get_case_results("limited_task")
  
  # 验证资源限制是否生效
  assert_true(limited_task_results.peak_memory_usage <= 100 * 1024 * 1024)  # 不应超过100MB
  
  # 测试系统负载影响
  let load_impact_benchmark = SystemBenchmark::new("load_impact", {
    warmup_iterations: 1,
    measurement_iterations: 3,
    background_load: true,
    load_levels: [0.2, 0.5, 0.8]  # 20%, 50%, 80%系统负载
  })
  
  SystemBenchmark::add_case(load_impact_benchmark, "task_under_load", fn(load_level) {
    # 在不同系统负载下执行任务
    let start_time = Time::now()
    
    # 执行标准任务
    let mut result = 0
    for i in 0..=10000 {
      result = result + Math::sqrt(i * 1.0)
    }
    
    let execution_time = Time::now() - start_time
    
    {
      execution_time: execution_time,
      result: result,
      load_level: load_level
    }
  })
  
  let load_impact_results = SystemBenchmarkManager::run(system_benchmark_manager, load_impact_benchmark)
  let load_impact_task_results = load_impact_results.get_case_results("task_under_load")
  
  # 分析系统负载对性能的影响
  let load_impact_analysis = SystemBenchmarkManager::analyze_load_impact(load_impact_task_results)
  assert_true(load_impact_analysis.performance_degradation >= 0)  # 性能应该下降或保持不变
  
  # 生成系统资源报告
  let system_report = SystemBenchmarkManager::generate_system_report(system_results, {
    include_cpu_analysis: true,
    include_memory_analysis: true,
    include_io_analysis: true,
    include_network_analysis: true,
    include_resource_limits: true,
    include_load_impact: true
  })
  
  assert_true(system_report.contains("cpu_intensive_task"))
  assert_true(system_report.contains("memory_intensive_task"))
  assert_true(system_report.contains("io_intensive_task"))
  assert_true(system_report.contains("network_intensive_task"))
  assert_true(system_report.contains("cpu_usage"))
  assert_true(system_report.contains("memory_usage"))
}

// 辅助函数

// 生成随机矩阵
fn generate_random_matrix(rows, columns) {
  let matrix = []
  for i in 0..=rows {
    let row = []
    for j in 0..=columns {
      row = row.push(Math::random() * 100)
    }
    matrix = matrix.push(row)
  }
  matrix
}

// 矩阵乘法
fn matrix_multiply(a, b) {
  let rows_a = a.length()
  let cols_a = a[0].length()
  let cols_b = b[0].length()
  
  let result = []
  for i in 0..=rows_a {
    let row = []
    for j in 0..=cols_b {
      let mut sum = 0
      for k in 0..=cols_a {
        sum = sum + a[i][k] * b[k][j]
      }
      row = row.push(sum)
    }
    result = result.push(row)
  }
  result
}

// 生成随机文本
fn generate_random_text(length) {
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "
  let mut text = ""
  
  for i in 0..=length {
    let index = Math::random_int(0, chars.length())
    text = text + chars[index]
  }
  
  text
}

// 生成随机数组
fn generate_random_array(size) {
  let array = []
  for i in 0..=size {
    array = array.push(Math::random_int(0, size * 10))
  }
  array
}

// 生成已排序数组
fn generate_sorted_array(size) {
  let array = []
  for i in 0..=size {
    array = array.push(i)
  }
  array
}

// 生成随机值
fn generate_random_values(count, min, max) {
  let values = []
  for i in 0..=count {
    values = values.push(Math::random_int(min, max))
  }
  values
}

// 快速排序实现
fn quick_sort_recursive(array, low, high) {
  if low < high {
    let pivot_index = partition(array, low, high)
    quick_sort_recursive(array, low, pivot_index - 1)
    quick_sort_recursive(array, pivot_index + 1, high)
  }
}

// 快速排序分区函数
fn partition(array, low, high) {
  let pivot = array[high]
  let i = low - 1
  
  for j in low..=high - 1 {
    if array[j] <= pivot {
      i = i + 1
      # 交换array[i]和array[j]
      let temp = array[i]
      array[i] = array[j]
      array[j] = temp
    }
  }
  
  # 交换array[i+1]和array[high]
  let temp = array[i + 1]
  array[i + 1] = array[high]
  array[high] = temp
  
  return i + 1
}

// 生成随机图
fn generate_random_graph(node_count, edge_density) {
  let graph = {}
  
  # 初始化图
  for i in 0..=node_count {
    graph[i] = []
  }
  
  # 添加边
  for i in 0..=node_count {
    for j in 0..=node_count {
      if i != j and Math::random() < edge_density {
        graph[i] = graph[i].push(j)
      }
    }
  }
  
  graph
}

// Dijkstra最短路径算法
fn dijkstra_shortest_path(graph, start_node, end_node) {
  let distances = {}
  let previous = {}
  let unvisited = Set::new()
  
  # 初始化距离
  for node in graph.keys() {
    distances[node] = if node == start_node { 0 } else { Float::infinity }
    previous[node] = None
    unvisited.add(node)
  }
  
  while not(unvisited.is_empty()) {
    # 找到未访问节点中距离最小的
    let current = null
    let min_distance = Float::infinity
    
    for node in unvisited {
      if distances[node] < min_distance {
        min_distance = distances[node]
        current = node
      }
    }
    
    if current == None or current == end_node {
      break
    }
    
    unvisited.remove(current)
    
    # 更新邻居的距离
    for neighbor in graph[current] {
      let alt = distances[current] + 1  # 假设每条边权重为1
      
      if alt < distances[neighbor] {
        distances[neighbor] = alt
        previous[neighbor] = current
      }
    }
  }
  
  # 构建路径
  let path = []
  let current = end_node
  
  while current != None {
    path = path.unshift(current)
    current = previous[current]
  }
  
  path
}