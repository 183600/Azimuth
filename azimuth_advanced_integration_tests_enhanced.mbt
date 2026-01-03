// Azimuth 高级集成测试增强版
// 专注于复杂场景和边界条件的高质量测试用例

// 测试1: 高级数据分析和异常检测
test "高级数据分析和异常检测测试" {
  // 创建数据分析器
  let analyzer = TelemetryAnalyzer::new()
  
  // 设置基准数据
  let baseline_metrics = [
    MetricData::new("cpu.usage", 45.2, TimeStamp::now()),
    MetricData::new("memory.usage", 60.8, TimeStamp::now()),
    MetricData::new("network.throughput", 1024.5, TimeStamp::now()),
    MetricData::new("disk.io", 256.3, TimeStamp::now())
  ]
  
  // 注入异常数据点
  let anomalous_metrics = [
    MetricData::new("cpu.usage", 95.7, TimeStamp::now()), // 异常CPU使用率
    MetricData::new("memory.usage", 15.2, TimeStamp::now()), // 异常内存使用率
    MetricData::new("network.throughput", 0.0, TimeStamp::now()), // 网络中断
    MetricData::new("disk.io", 2048.9, TimeStamp::now()) // 异常磁盘IO
  ]
  
  // 分析基准数据
  let baseline_profile = Analyzer::learn_baseline(analyzer, baseline_metrics)
  
  // 检测异常
  let anomalies = Analyzer::detect_anomalies(analyzer, anomalous_metrics, baseline_profile)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 4)
  assert_true(Analyzer::is_anomaly_present(anomalies, "cpu.usage"))
  assert_true(Analyzer::is_anomaly_present(anomalies, "memory.usage"))
  assert_true(Analyzer::is_anomaly_present(anomalies, "network.throughput"))
  assert_true(Analyzer::is_anomaly_present(anomalies, "disk.io"))
  
  // 验证异常严重程度
  let cpu_anomaly = Analyzer::get_anomaly(anomalies, "cpu.usage")
  assert_eq(Anomaly::severity(cpu_anomaly), Critical)
  
  // 创建自动恢复策略
  let recovery_strategy = RecoveryStrategy::adaptive()
  let recovery_actions = Analyzer::generate_recovery_actions(analyzer, anomalies, recovery_strategy)
  
  // 验证恢复策略
  assert_true(recovery_actions.length() > 0)
  assert_true(RecoveryActions::contains(recovery_actions, "scale_horizontal"))
}

// 测试2: 自适应性能调优
test "自适应性能调优测试" {
  // 创建性能调优器
  let tuner = PerformanceTuner::new()
  
  // 设置初始配置
  let initial_config = PerformanceConfig::new()
  Config::set_sampling_rate(initial_config, 0.1)
  Config::set_batch_size(initial_config, 512)
  Config::set_buffer_size(initial_config, 1024)
  Config::set_concurrency_limit(initial_config, 16)
  
  // 模拟不同负载场景
  let low_load_scenarios = [
    LoadScenario::new("light", 100, 0.95),
    LoadScenario::new("normal", 500, 0.90),
    LoadScenario::new("moderate", 1000, 0.85)
  ]
  
  let high_load_scenarios = [
    LoadScenario::new("heavy", 5000, 0.75),
    LoadScenario::new("peak", 10000, 0.65),
    LoadScenario::new("extreme", 20000, 0.50)
  ]
  
  // 在低负载场景下优化配置
  let low_load_optimized = Tuner::optimize_for_load(tuner, initial_config, low_load_scenarios)
  
  // 验证低负载优化
  assert_eq(Config::get_sampling_rate(low_load_optimized), 0.05) // 降低采样率
  assert_eq(Config::get_batch_size(low_load_optimized), 256) // 减小批处理大小
  assert_eq(Config::get_concurrency_limit(low_load_optimized), 8) // 降低并发限制
  
  // 在高负载场景下优化配置
  let high_load_optimized = Tuner::optimize_for_load(tuner, initial_config, high_load_scenarios)
  
  // 验证高负载优化
  assert_eq(Config::get_sampling_rate(high_load_optimized), 0.2) // 提高采样率
  assert_eq(Config::get_batch_size(high_load_optimized), 1024) // 增大批处理大小
  assert_eq(Config::get_concurrency_limit(high_load_optimized), 32) // 提高并发限制
  
  // 验证性能改进
  let low_load_performance = Tuner::measure_performance(tuner, low_load_optimized, low_load_scenarios)
  let high_load_performance = Tuner::measure_performance(tuner, high_load_optimized, high_load_scenarios)
  
  assert_true(Performance::average_latency(low_load_performance) < Performance::average_latency(initial_config))
  assert_true(Performance::throughput(high_load_performance) > Performance::throughput(initial_config))
}

// 测试3: 分布式系统一致性验证
test "分布式系统一致性验证测试" {
  // 创建分布式节点集群
  let cluster = DistributedCluster::new(5) // 5个节点
  
  // 初始化每个节点的遥测配置
  for node in Cluster::nodes(cluster) {
    let tracer = TracerProvider::get_tracer(TracerProvider::default(), "distributed.consistency")
    let meter = MeterProvider::get_meter(MeterProvider::default(), "distributed.metrics")
    Node::set_tracer(node, tracer)
    Node::set_meter(node, meter)
  }
  
  // 创建分布式事务
  let transaction = DistributedTransaction::new("consistency.test")
  Transaction::add_participant(transaction, Node::id(Cluster::get_node(cluster, 0)))
  Transaction::add_participant(transaction, Node::id(Cluster::get_node(cluster, 1)))
  Transaction::add_participant(transaction, Node::id(Cluster::get_node(cluster, 2)))
  
  // 执行分布式操作
  let operation_results = []
  for participant in Transaction::participants(transaction) {
    let node = Cluster::get_node_by_id(cluster, participant)
    let span = Node::start_span(node, "distributed.operation")
    Span::set_attribute(span, "transaction.id", StringValue(Transaction::id(transaction)))
    Span::set_attribute(span, "participant.id", StringValue(participant))
    
    // 模拟操作执行
    let result = Node::execute_operation(node, "data.update", Some(span))
    operation_results = operation_results.push(result)
    
    Span::set_status(span, Ok)
    Span::end(span)
  }
  
  // 验证操作结果一致性
  let all_success = operation_results.all(fn(r) { Result::is_ok(r) })
  assert_true(all_success)
  
  // 验证追踪上下文传播
  let trace_contexts = []
  for node in Cluster::nodes(cluster) {
    let spans = Node::get_completed_spans(node)
    for span in spans {
      if Span::get_attribute(span, "transaction.id") == Some(StringValue(Transaction::id(transaction))) {
        trace_contexts = trace_contexts.push(Span::context(span))
      }
    }
  }
  
  // 验证所有相关span属于同一追踪
  let trace_ids = trace_contexts.map(fn(ctx) { Context::trace_id(ctx) })
  let unique_trace_ids = trace_ids.unique()
  assert_eq(unique_trace_ids.length(), 1) // 所有span应该有相同的trace_id
  
  // 验证span父子关系
  let root_spans = trace_contexts.filter(fn(ctx) { Context::parent_span_id(ctx) == None })
  assert_eq(root_spans.length(), 1) // 应该只有一个根span
  
  let child_spans = trace_contexts.filter(fn(ctx) { Context::parent_span_id(ctx) != None })
  assert_eq(child_spans.length(), 3) // 应该有3个子span
}

// 测试4: 高并发场景下的资源管理
test "高并发场景下的资源管理测试" {
  // 创建资源管理器
  let resource_manager = ResourceManager::new()
  
  // 设置资源限制
  let resource_limits = ResourceLimits::new()
  Limits::set_max_memory(resource_limits, 1024 * 1024 * 1024) // 1GB
  Limits::set_max_cpu_cores(resource_limits, 8)
  Limits::set_max_network_connections(resource_limits, 1000)
  Limits::set_max_file_descriptors(resource_limits, 10000)
  
  Manager::set_limits(resource_manager, resource_limits)
  
  // 创建高并发工作负载
  let concurrent_operations = 1000
  let operation_results = ConcurrentArray::new()
  
  // 使用并发执行器
  let executor = ConcurrentExecutor::new(16) // 16个工作线程
  
  // 提交并发任务
  for i in 0..concurrent_operations {
    let task = ConcurrentTask::new(fn() {
      // 每个任务分配资源
      let resource_request = ResourceRequest::new()
      Request::set_memory(resource_request, 1024 * 1024) // 1MB
      Request::set_cpu_time(resource_request, 100) // 100ms
      
      let allocation = Manager::allocate(resource_manager, resource_request)
      
      match allocation {
        Some(alloc) => {
          // 执行模拟工作
          let span = Tracer::start_span_with_current_context("concurrent.operation")
          Span::set_attribute(span, "task.id", IntValue(i))
          Span::set_attribute(span, "allocated.memory", IntValue(1024 * 1024))
          
          // 模拟工作负载
          Workload::simulate_cpu_work(100) // 100ms CPU工作
          Workload::simulate_memory_work(1024 * 1024) // 1MB内存工作
          
          Span::set_status(span, Ok)
          Span::end(span)
          
          // 释放资源
          Manager::deallocate(resource_manager, alloc)
          Ok("task.completed")
        }
        None => Err("resource.allocation.failed")
      }
    })
    
    Executor::submit(executor, task, operation_results)
  }
  
  // 等待所有任务完成
  Executor::wait_for_completion(executor, 30000) // 30秒超时
  
  // 验证结果
  let completed_tasks = Array::filter(operation_results.to_array(), fn(r) { Result::is_ok(r) })
  let failed_tasks = Array::filter(operation_results.to_array(), fn(r) { Result::is_err(r) })
  
  assert_true(completed_tasks.length() > failed_tasks.length())
  assert_true(failed_tasks.length() < concurrent_operations * 0.1) // 失败率应低于10%
  
  // 验证资源使用情况
  let resource_usage = Manager::get_current_usage(resource_manager)
  assert_true(Usage::memory(resource_usage) <= Limits::max_memory(resource_limits))
  assert_true(Usage::cpu_cores(resource_usage) <= Limits::max_cpu_cores(resource_limits))
  
  // 验证资源泄漏检测
  let leaked_resources = Manager::detect_leaks(resource_manager)
  assert_eq(leaked_resources.length(), 0) // 不应该有资源泄漏
}

// 测试5: 智能缓存策略测试
test "智能缓存策略测试" {
  // 创建智能缓存管理器
  let cache_manager = SmartCacheManager::new()
  
  // 配置缓存策略
  let cache_config = CacheConfig::new()
  Config::set_max_size(cache_config, 10000)
  Config::set_ttl(cache_config, 300) // 5分钟TTL
  Config::set_eviction_policy(cache_config, LRU) // 最近最少使用淘汰策略
  Config::set_adaptive_threshold(cache_config, 0.8) // 80%命中率阈值
  
  Manager::configure(cache_manager, cache_config)
  
  // 创建测试数据集
  let hot_data = DataGenerator::generate_hot_dataset(1000) // 热点数据
  let cold_data = DataGenerator::generate_cold_dataset(5000) // 冷数据
  let normal_data = DataGenerator::generate_normal_dataset(2000) // 正常数据
  
  // 模拟访问模式
  let access_patterns = [
    AccessPattern::new("hot", hot_data, 0.7), // 70%访问热点数据
    AccessPattern::new("normal", normal_data, 0.25), // 25%访问正常数据
    AccessPattern::new("cold", cold_data, 0.05) // 5%访问冷数据
  ]
  
  // 执行缓存测试
  let cache_metrics = CacheMetrics::new()
  
  // 第一阶段：预热缓存
  for pattern in access_patterns {
    for item in Pattern::data(pattern) {
      let key = Data::key(item)
      let value = Data::value(item)
      
      // 尝试从缓存获取
      let cached_value = Manager::get(cache_manager, key)
      
      match cached_value {
        Some(v) => {
          Metrics::record_hit(cache_metrics)
          assert_eq(v, value)
        }
        None => {
          Metrics::record_miss(cache_metrics)
          // 从数据源获取并缓存
          Manager::put(cache_manager, key, value)
        }
      }
    }
  }
  
  // 第二阶段：测试缓存性能
  let start_time = TimeStamp::now()
  
  for i in 0..10000 {
    // 根据访问模式随机选择数据
    let random_pattern = Pattern::select_by_probability(access_patterns)
    let random_item = Array::random_element(Pattern::data(random_pattern))
    let key = Data::key(random_item)
    
    // 测量缓存访问时间
    let access_start = TimeStamp::now()
    let cached_value = Manager::get(cache_manager, key)
    let access_end = TimeStamp::now()
    
    Metrics::record_access_time(cache_metrics, TimeStamp::duration(access_start, access_end))
    
    match cached_value {
      Some(_) => Metrics::record_hit(cache_metrics)
      None => Metrics::record_miss(cache_metrics)
    }
  }
  
  let end_time = TimeStamp::now()
  let total_duration = TimeStamp::duration(start_time, end_time)
  
  // 验证缓存性能
  let hit_rate = Metrics::hit_rate(cache_metrics)
  let average_access_time = Metrics::average_access_time(cache_metrics)
  
  assert_true(hit_rate > 0.6) // 命中率应超过60%
  assert_true(average_access_time < 0.001) // 平均访问时间应小于1ms
  
  // 验证自适应策略
  let adaptive_adjustments = Manager::get_adaptive_adjustments(cache_manager)
  assert_true(adaptive_adjustments.length() > 0)
  
  // 验证缓存大小控制
  let current_size = Manager::current_size(cache_manager)
  assert_true(current_size <= Config::max_size(cache_config))
}

// 测试6: 实时数据流处理
test "实时数据流处理测试" {
  // 创建数据流处理器
  let stream_processor = StreamProcessor::new()
  
  // 配置流处理管道
  let pipeline = Pipeline::new()
  
  // 添加数据源
  let telemetry_source = TelemetrySource::new("realtime.metrics")
  Source::set_buffer_size(telemetry_source, 10000)
  Source::set_batch_size(telemetry_source, 100)
  
  // 添加处理阶段
  let filter_stage = FilterStage::new(fn(metric) {
    // 过滤掉无效指标
    Metric::value(metric) >= 0.0 && Metric::value(metric) <= 100.0
  })
  
  let transform_stage = TransformStage::new(fn(metric) {
    // 转换指标格式
    let transformed_name = "transformed." + Metric::name(metric)
    let transformed_value = Metric::value(metric) * 1.5 // 应用转换函数
    Metric::new(transformed_name, transformed_value, Metric::timestamp(metric))
  })
  
  let aggregation_stage = AggregationStage::new(TimeWindow::seconds(10), fn(window) {
    // 10秒时间窗口聚合
    let metrics = Window::metrics(window)
    let avg_value = Array::average(metrics.map(fn(m) { Metric::value(m) }))
    let max_value = Array::max(metrics.map(fn(m) { Metric::value(m) }))
    let min_value = Array::min(metrics.map(fn(m) { Metric::value(m) }))
    
    [
      AggregatedMetric::new("average", avg_value, Window::end_time(window)),
      AggregatedMetric::new("maximum", max_value, Window::end_time(window)),
      AggregatedMetric::new("minimum", min_value, Window::end_time(window))
    ]
  })
  
  // 添加输出目标
  let alert_sink = AlertSink::new()
  let metrics_sink = MetricsSink::new()
  
  // 构建处理管道
  Pipeline::add_source(pipeline, telemetry_source)
  Pipeline::add_stage(pipeline, filter_stage)
  Pipeline::add_stage(pipeline, transform_stage)
  Pipeline::add_stage(pipeline, aggregation_stage)
  Pipeline::add_sink(pipeline, alert_sink)
  Pipeline::add_sink(pipeline, metrics_sink)
  
  // 启动流处理
  Processor::start(stream_processor, pipeline)
  
  // 生成测试数据流
  let data_generator = DataStreamGenerator::new()
  Generator::set_rate(data_generator, 1000) // 每秒1000个指标
  
  // 生成异常数据以测试告警
  let normal_metrics = Generator::generate_normal_metrics(data_generator, 5000)
  let anomaly_metrics = Generator::generate_anomaly_metrics(data_generator, 100)
  
  // 注入数据流
  Source::inject(telemetry_source, normal_metrics)
  Source::inject(telemetry_source, anomaly_metrics)
  
  // 等待处理完成
  Timer::sleep(15000) // 等待15秒确保处理完成
  
  // 验证处理结果
  let processed_metrics = Sink::get_processed_metrics(metrics_sink)
  let triggered_alerts = Sink::get_triggered_alerts(alert_sink)
  
  assert_true(processed_metrics.length() > 0)
  assert_true(triggered_alerts.length() > 0)
  
  // 验证聚合结果
  let aggregated_metrics = processed_metrics.filter(fn(m) { Metric::name(m).contains("average") || 
                                                               Metric::name(m).contains("maximum") || 
                                                               Metric::name(m).contains("minimum") })
  assert_true(aggregated_metrics.length() > 0)
  
  // 验证告警触发
  let anomaly_alerts = triggered_alerts.filter(fn(a) { Alert::severity(a) == Warning || 
                                                      Alert::severity(a) == Critical })
  assert_true(anomaly_alerts.length() > 0)
  
  // 验证处理延迟
  let processing_latency = Processor::get_average_latency(stream_processor)
  assert_true(processing_latency < 1.0) // 处理延迟应小于1秒
  
  // 停止流处理
  Processor::stop(stream_processor)
}

// 测试7: 安全性和权限控制
test "安全性和权限控制测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  
  // 创建用户角色和权限
  let admin_role = Role::new("admin")
  Role::add_permission(admin_role, Permission::read_all_metrics)
  Role::add_permission(admin_role, Permission::write_all_metrics)
  Role::add_permission(admin_role, Permission::manage_configuration)
  Role::add_permission(admin_role, Permission::manage_users)
  
  let operator_role = Role::new("operator")
  Role::add_permission(operator_role, Permission::read_all_metrics)
  Role::add_permission(operator_role, Permission::write_service_metrics)
  
  let viewer_role = Role::new("viewer")
  Role::add_permission(viewer_role, Permission::read_service_metrics)
  
  // 创建用户
  let admin_user = User::new("admin", "admin@example.com")
  User::assign_role(admin_user, admin_role)
  
  let operator_user = User::new("operator", "operator@example.com")
  User::assign_role(operator_user, operator_role)
  
  let viewer_user = User::new("viewer", "viewer@example.com")
  User::assign_role(viewer_user, viewer_role)
  
  // 注册用户到安全管理器
  Manager::register_user(security_manager, admin_user)
  Manager::register_user(security_manager, operator_user)
  Manager::register_user(security_manager, viewer_user)
  
  // 创建资源
  let sensitive_resource = Resource::new("sensitive.metrics")
  Resource::set_access_level(sensitive_resource, Restricted)
  
  let service_resource = Resource::new("service.metrics")
  Resource::set_access_level(service_resource, Normal)
  
  let public_resource = Resource::new("public.metrics")
  Resource::set_access_level(public_resource, Public)
  
  // 测试权限控制
  let test_cases = [
    (admin_user, sensitive_resource, Permission::read_all_metrics, true),
    (admin_user, sensitive_resource, Permission::write_all_metrics, true),
    (operator_user, sensitive_resource, Permission::read_all_metrics, false),
    (operator_user, service_resource, Permission::read_all_metrics, true),
    (operator_user, service_resource, Permission::write_service_metrics, true),
    (viewer_user, service_resource, Permission::read_service_metrics, true),
    (viewer_user, service_resource, Permission::write_service_metrics, false),
    (viewer_user, public_resource, Permission::read_service_metrics, true)
  ]
  
  for (user, resource, permission, expected_result) in test_cases {
    let access_result = Manager::check_permission(security_manager, user, resource, permission)
    assert_eq(access_result, expected_result, 
              "User: " + User::username(user) + 
              ", Resource: " + Resource::name(resource) + 
              ", Permission: " + Permission::name(permission))
  }
  
  // 测试审计日志
  let audit_logger = AuditLogger::new()
  Manager::set_audit_logger(security_manager, audit_logger)
  
  // 执行一些操作
  Manager::log_access_attempt(security_manager, admin_user, sensitive_resource, Permission::read_all_metrics, true)
  Manager::log_access_attempt(security_manager, operator_user, sensitive_resource, Permission::read_all_metrics, false)
  Manager::log_access_attempt(security_manager, viewer_user, service_resource, Permission::read_service_metrics, true)
  
  // 验证审计日志
  let audit_logs = Logger::get_logs(audit_logger)
  assert_eq(audit_logs.length(), 3)
  
  let admin_log = Array::find(audit_logs, fn(log) { Log::username(log) == "admin" })
  let operator_log = Array::find(audit_logs, fn(log) { Log::username(log) == "operator" })
  let viewer_log = Array::find(audit_logs, fn(log) { Log::username(log) == "viewer" })
  
  assert_true(admin_log != None)
  assert_true(operator_log != None)
  assert_true(viewer_log != None)
  
  assert_eq(Log::success(admin_log.unwrap()), true)
  assert_eq(Log::success(operator_log.unwrap()), false)
  assert_eq(Log::success(viewer_log.unwrap()), true)
  
  // 测试加密功能
  let encryption_service = EncryptionService::new()
  Service::set_algorithm(encryption_service, AES256)
  
  let sensitive_data = "sensitive.telemetry.data"
  let encrypted_data = Service::encrypt(encryption_service, sensitive_data)
  let decrypted_data = Service::decrypt(encryption_service, encrypted_data)
  
  assert_eq(decrypted_data, sensitive_data)
  assert_ne(encrypted_data, sensitive_data) // 确保数据已加密
}

// 测试8: 跨平台兼容性测试
test "跨平台兼容性测试" {
  // 创建平台检测器
  let platform_detector = PlatformDetector::new()
  
  // 检测当前平台
  let current_platform = Detector::detect_platform(platform_detector)
  let platform_info = Detector::get_platform_info(platform_detector)
  
  // 验证平台检测
  assert_true(Platform::is_supported(current_platform))
  assert_true(Platform::architecture(platform_info) != Unknown)
  assert_true(Platform::os_type(platform_info) != Unknown)
  
  // 创建跨平台兼容性测试套件
  let compatibility_suite = CompatibilitySuite::new()
  
  // 测试文件系统操作
  let file_system_tests = FileSystemTests::new()
  Tests::add_test(file_system_tests, "create_temp_file", fn() {
    let temp_file = FileSystem::create_temp_file("azimuth_test")
    assert_true(FileSystem::exists(temp_file))
    FileSystem::write_string(temp_file, "test content")
    let content = FileSystem::read_string(temp_file)
    assert_eq(content, "test content")
    FileSystem::delete(temp_file)
    assert_false(FileSystem::exists(temp_file))
    Ok
  })
  
  // 测试网络操作
  let network_tests = NetworkTests::new()
  Tests::add_test(network_tests, "socket_creation", fn() {
    let socket = Network::create_socket(TCP)
    assert_true(Network::is_valid(socket))
    Network::close(socket)
    Ok
  })
  
  // 测试时间操作
  let time_tests = TimeTests::new()
  Tests::add_test(time_tests, "precision_timing", fn() {
    let start_time = Time::high_resolution_now()
    Timer::sleep(100) // 睡眠100ms
    let end_time = Time::high_resolution_now()
    let elapsed = Time::duration(start_time, end_time)
    assert_true(elapsed >= 90 && elapsed <= 110) // 允许10ms误差
    Ok
  })
  
  // 测试内存操作
  let memory_tests = MemoryTests::new()
  Tests::add_test(memory_tests, "memory_alignment", fn() {
    let aligned_buffer = Memory::allocate_aligned(1024, 64) // 1KB，64字节对齐
    assert_true(Memory::is_aligned(aligned_buffer, 64))
    Memory::free(aligned_buffer)
    Ok
  })
  
  // 添加测试套件到兼容性测试
  Suite::add_test_suite(compatibility_suite, file_system_tests)
  Suite::add_test_suite(compatibility_suite, network_tests)
  Suite::add_test_suite(compatibility_suite, time_tests)
  Suite::add_test_suite(compatibility_suite, memory_tests)
  
  // 执行兼容性测试
  let test_results = Suite::run_all(compatibility_suite)
  
  // 验证测试结果
  assert_true(Results::passed(test_results) > 0)
  assert_eq(Results::failed(test_results), 0)
  
  // 测试平台特定优化
  let platform_optimizer = PlatformOptimizer::new()
  let optimizations = Optimizer::detect_available_optimizations(platform_optimizer, current_platform)
  
  match Platform::os_type(platform_info) {
    Linux => {
      assert_true(Optimizations::contains(optimizations, "epoll"))
      assert_true(Optimizations::contains(optimizations, "inotify"))
    }
    Windows => {
      assert_true(Optimizations::contains(optimizations, "iocp"))
      assert_true(Optimizations::contains(optimizations, "windows_events"))
    }
    MacOS => {
      assert_true(Optimizations::contains(optimizations, "kqueue"))
      assert_true(Optimizations::contains(optimizations, "fsevents"))
    }
    _ => {} // 其他平台
  }
  
  // 测试跨平台序列化
  let cross_platform_data = CrossPlatformData::new()
  Data::add_field(cross_platform_data, "timestamp", TimeValue(Time::now()))
  Data::add_field(cross_platform_data, "platform", StringValue(Platform::name(current_platform)))
  Data::add_field(cross_platform_data, "architecture", StringValue(Platform::architecture_name(platform_info)))
  
  // 序列化数据
  let serialized_data = Data::serialize(cross_platform_data, JSON)
  
  // 反序列化数据
  let deserialized_data = Data::deserialize(serialized_data, JSON)
  
  // 验证序列化结果
  assert_eq(Data::get_field(deserialized_data, "platform"), Data::get_field(cross_platform_data, "platform"))
  assert_eq(Data::get_field(deserialized_data, "architecture"), Data::get_field(cross_platform_data, "architecture"))
  
  // 测试端序转换
  let test_value = 0x12345678
  let little_endian = ByteOrder::to_little_endian(test_value)
  let big_endian = ByteOrder::to_big_endian(test_value)
  
  if Platform::is_little_endian(current_platform) {
    assert_eq(little_endian, test_value)
    assert_ne(big_endian, test_value)
  } else {
    assert_eq(big_endian, test_value)
    assert_ne(little_endian, test_value)
  }
  
  // 验证端序转换的可逆性
  assert_eq(ByteOrder::from_little_endian(little_endian), test_value)
  assert_eq(ByteOrder::from_big_endian(big_endian), test_value)
}