// Azimuth Distributed Transaction Tracing Tests
// 分布式事务跟踪测试用例 - 专注于分布式系统中的事务跟踪和监控

// Test 1: 分布式事务创建和传播
test "distributed transaction creation and propagation" {
  // 创建分布式事务管理器
  let tx_manager = DistributedTransactionManager::new()
  
  // 配置事务管理参数
  TransactionManager::configure(tx_manager, [
    ("timeout.default", IntValue(30000)), // 30秒默认超时
    ("retry.max.attempts", IntValue(3)),
    ("propagation.format", StringValue("w3c_trace_context")),
    ("sampling.rate", FloatValue(1.0)), // 100%采样率用于测试
    ("max.spans.per.transaction", IntValue(1000))
  ])
  
  // 创建根事务
  let root_tx = TransactionManager::begin_transaction(tx_manager, "user_order_processing", [
    ("user.id", StringValue("user_12345")),
    ("order.id", StringValue("order_67890")),
    ("service.version", StringValue("1.2.3"))
  ])
  
  // 验证根事务创建
  assert_true(root_tx !== None)
  
  match root_tx {
    Some(transaction) => {
      assert_eq(Transaction::id(transaction), "user_order_processing")
      assert_true(Transaction::is_root(transaction))
      assert_true(Transaction::is_active(transaction))
      assert_eq(Transaction::span_count(transaction), 0)
    }
    None => assert_true(false)
  }
  
  // 创建子事务
  let child_tx1 = TransactionManager::begin_child_transaction(
    tx_manager,
    root_tx,
    "inventory_check",
    [("product.id", StringValue("prod_001"))]
  )
  
  let child_tx2 = TransactionManager::begin_child_transaction(
    tx_manager,
    root_tx,
    "payment_processing",
    [("payment.method", StringValue("credit_card"))]
  )
  
  // 验证子事务创建
  assert_true(child_tx1 !== None)
  assert_true(child_tx2 !== None)
  
  match (child_tx1, child_tx2) {
    (Some(tx1), Some(tx2)) => {
      assert_eq(Transaction::id(tx1), "inventory_check")
      assert_eq(Transaction::id(tx2), "payment_processing")
      assert_false(Transaction::is_root(tx1))
      assert_false(Transaction::is_root(tx2))
      assert_eq(Transaction::parent_id(tx1), Some("user_order_processing"))
      assert_eq(Transaction::parent_id(tx2), Some("user_order_processing"))
      
      // 验证跟踪上下文传播
      let ctx1 = Transaction::trace_context(tx1)
      let ctx2 = Transaction::trace_context(tx2)
      
      assert_eq(TraceContext::trace_id(ctx1), TraceContext::trace_id(Transaction::trace_context(transaction)))
      assert_eq(TraceContext::trace_id(ctx2), TraceContext::trace_id(Transaction::trace_context(transaction)))
      assert_false(TraceContext::span_id(ctx1) == TraceContext::span_id(ctx2))
    }
    _ => assert_true(false)
  }
  
  // 创建嵌套子事务
  let nested_tx = TransactionManager::begin_child_transaction(
    tx_manager,
    child_tx1,
    "stock_reservation",
    [("warehouse.id", StringValue("wh_001"))]
  )
  
  // 验证嵌套事务
  match nested_tx {
    Some(tx) => {
      assert_eq(Transaction::id(tx), "stock_reservation")
      assert_eq(Transaction::parent_id(tx), Some("inventory_check"))
      assert_eq(Transaction::root_id(tx), Some("user_order_processing"))
    }
    None => assert_true(false)
  }
  
  // 测试跨服务传播
  let propagation_headers = TransactionManager::extract_propagation_headers(tx_manager, root_tx)
  assert_true(propagation_headers.length() > 0)
  
  // 验证必要的传播头部
  let trace_header = propagation_headers.find(fn(header) {
    Header::name(header) == "traceparent"
  })
  assert_true(trace_header !== None)
  
  let state_header = propagation_headers.find(fn(header) {
    Header::name(header) == "tracestate"
  })
  assert_true(state_header !== None)
  
  // 模拟远程服务接收事务上下文
  let remote_tx = TransactionManager::extract_transaction_from_headers(
    tx_manager,
    propagation_headers,
    "remote_service_processing"
  )
  
  // 验证远程事务
  match remote_tx {
    Some(tx) => {
      assert_eq(Transaction::id(tx), "remote_service_processing")
      assert_eq(Transaction::trace_id(tx), Transaction::trace_id(transaction))
      assert_true(Transaction::is_remote(tx))
    }
    None => assert_true(false)
  }
}

// Test 2: 跨服务调用链跟踪
test "cross-service call chain tracking" {
  // 创建调用链跟踪器
  let call_chain_tracker = CallChainTracker::new()
  
  // 配置跟踪参数
  CallChainTracker::configure(call_chain_tracker, [
    ("call.recording", BoolValue(true)),
    ("latency.threshold", IntValue(1000)), // 1秒
    ("error.recording", BoolValue(true)),
    ("stack.trace.capture", BoolValue(true))
  ])
  
  // 创建服务调用链
  let api_gateway = ServiceNode::new("api_gateway", "1.0.0")
  let user_service = ServiceNode::new("user_service", "2.1.0")
  let order_service = ServiceNode::new("order_service", "1.5.2")
  let payment_service = ServiceNode::new("payment_service", "3.0.1")
  let inventory_service = ServiceNode::new("inventory_service", "2.3.0")
  let notification_service = ServiceNode::new("notification_service", "1.2.0")
  
  // 模拟API网关接收请求
  let gateway_span = CallChainTracker::start_span(
    call_chain_tracker,
    "api_request",
    api_gateway,
    [("endpoint", StringValue("/api/orders")), ("method", StringValue("POST"))]
  )
  
  // 模拟API网关调用用户服务
  let user_call_start = Time::now()
  let user_span = CallChainTracker::start_child_span(
    call_chain_tracker,
    gateway_span,
    "user_authentication",
    user_service,
    [("user.id", StringValue("user_12345"))]
  )
  
  // 模拟用户服务处理时间
  Thread::sleep(100) // 100ms
  let user_call_end = Time::now()
  
  CallChainTracker::finish_span(call_chain_tracker, user_span, Ok, [
    ("duration", IntValue(user_call_end - user_call_start))
  ])
  
  // 模拟API网关调用订单服务
  let order_call_start = Time::now()
  let order_span = CallChainTracker::start_child_span(
    call_chain_tracker,
    gateway_span,
    "order_creation",
    order_service,
    [("order.total", FloatValue(199.99))]
  )
  
  // 模拟订单服务调用支付服务
  let payment_call_start = Time::now()
  let payment_span = CallChainTracker::start_child_span(
    call_chain_tracker,
    order_span,
    "payment_processing",
    payment_service,
    [("payment.amount", FloatValue(199.99)), ("payment.method", StringValue("credit_card"))]
  )
  
  // 模拟支付处理
  Thread::sleep(150) // 150ms
  let payment_call_end = Time::now()
  
  CallChainTracker::finish_span(call_chain_tracker, payment_span, Ok, [
    ("duration", IntValue(payment_call_end - payment_call_start)),
    ("transaction.id", StringValue("txn_abc123"))
  ])
  
  // 模拟订单服务调用库存服务
  let inventory_call_start = Time::now()
  let inventory_span = CallChainTracker::start_child_span(
    call_chain_tracker,
    order_span,
    "inventory_check",
    inventory_service,
    [("product.id", StringValue("prod_001")), ("quantity", IntValue(2))]
  )
  
  // 模拟库存检查（故意延迟以测试超时）
  Thread::sleep(1200) // 1.2秒，超过阈值
  let inventory_call_end = Time::now()
  
  CallChainTracker::finish_span(call_chain_tracker, inventory_span, Ok, [
    ("duration", IntValue(inventory_call_end - inventory_call_start)),
    ("stock.available", BoolValue(true))
  ])
  
  // 模拟订单服务调用通知服务（故意失败）
  let notification_call_start = Time::now()
  let notification_span = CallChainTracker::start_child_span(
    call_chain_tracker,
    order_span,
    "send_notification",
    notification_service,
    [("notification.type", StringValue("email")), ("recipient", StringValue("user@example.com"))]
  )
  
  // 模拟通知服务错误
  Thread::sleep(50) // 50ms
  let notification_call_end = Time::now()
  
  CallChainTracker::finish_span(call_chain_tracker, notification_span, Error, [
    ("duration", IntValue(notification_call_end - notification_call_start)),
    ("error.code", StringValue("SMTP_CONNECTION_FAILED")),
    ("error.message", StringValue("Unable to connect to SMTP server"))
  ])
  
  let order_call_end = Time::now()
  CallChainTracker::finish_span(call_chain_tracker, order_span, Ok, [
    ("duration", IntValue(order_call_end - order_call_start)),
    ("order.id", StringValue("order_67890"))
  ])
  
  // 完成网关跨度
  let gateway_end = Time::now()
  CallChainTracker::finish_span(call_chain_tracker, gateway_span, Ok, [
    ("duration", IntValue(gateway_end - user_call_start)),
    ("response.code", IntValue(200))
  ])
  
  // 分析调用链
  let call_chain = CallChainTracker::get_call_chain(call_chain_tracker, gateway_span)
  assert_true(call_chain !== None)
  
  match call_chain {
    Some(chain) => {
      assert_eq(CallChain::root_span_id(chain), Span::id(gateway_span))
      assert_eq(CallChain::span_count(chain), 6) // 网关 + 用户 + 订单 + 支付 + 库存 + 通知
      
      // 验证调用顺序
      let span_order = CallChain::get_span_execution_order(chain)
      assert_eq(span_order[0], Span::id(gateway_span))
      assert_eq(span_order[1], Span::id(user_span))
      assert_eq(span_order[2], Span::id(order_span))
      
      // 验证父子关系
      assert_eq(CallChain::get_parent_span(chain, Span::id(user_span)), Some(Span::id(gateway_span)))
      assert_eq(CallChain::get_parent_span(chain, Span::id(payment_span)), Some(Span::id(order_span)))
      
      // 验证服务调用统计
      let service_stats = CallChain::get_service_statistics(chain)
      assert_true(ServiceStats::has_call_count(service_stats, "api_gateway"))
      assert_true(ServiceStats::has_call_count(service_stats, "user_service"))
      assert_true(ServiceStats::has_call_count(service_stats, "order_service"))
      assert_true(ServiceStats::has_call_count(service_stats, "payment_service"))
      assert_true(ServiceStats::has_call_count(service_stats, "inventory_service"))
      assert_true(ServiceStats::has_call_count(service_stats, "notification_service"))
      
      // 验证性能指标
      let performance_metrics = CallChain::get_performance_metrics(chain)
      assert_true(PerformanceMetrics::has_total_duration(performance_metrics))
      assert_true(PerformanceMetrics::has_slowest_span(performance_metrics))
      
      let slowest_span = PerformanceMetrics::slowest_span(performance_metrics)
      assert_eq(slowest_span.service, "inventory_service") // 库存服务最慢
      
      // 验证错误统计
      let error_stats = CallChain::get_error_statistics(chain)
      assert_eq(ErrorStats::error_count(error_stats), 1) // 通知服务出错
      assert_true(ErrorStats::has_error(error_stats, "SMTP_CONNECTION_FAILED"))
    }
    None => assert_true(false)
  }
}

// Test 3: 分布式事务一致性验证
test "distributed transaction consistency validation" {
  // 创建一致性验证器
  let consistency_validator = ConsistencyValidator::new()
  
  // 配置验证参数
  ConsistencyValidator::configure(consistency_validator, [
    ("validation.rules", StringValue("atomicity,consistency,isolation,durability")),
    ("timeout.threshold", IntValue(60000)), // 1分钟
    ("retry.policy", StringValue("exponential_backoff")),
    ("compensation.enabled", BoolValue(true))
  ])
  
  // 创建分布式事务场景：订单处理
  let order_tx = DistributedTransaction::new("order_processing_001")
  
  // 添加事务参与者
  let participants = [
    TransactionParticipant::new("inventory_service", "reserve_inventory"),
    TransactionParticipant::new("payment_service", "process_payment"),
    TransactionParticipant::new("shipping_service", "schedule_shipping"),
    TransactionParticipant::new("notification_service", "send_confirmation")
  ]
  
  for participant in participants {
    DistributedTransaction::add_participant(order_tx, participant)
  }
  
  // 执行两阶段提交
  let phase1_start = Time::now()
  
  // 阶段1：准备阶段
  let phase1_results = []
  for participant in participants {
    let prepare_result = ConsistencyValidator::prepare_participant(
      consistency_validator,
      order_tx,
      participant
    )
    phase1_results = phase1_results.push(prepare_result)
  }
  
  let phase1_end = Time::now()
  
  // 验证所有参与者都准备成功
  let phase1_success = phase1_results.all(fn(result) { 
    match result {
      Ok(_) => true
      Err(_) => false
    }
  })
  
  // 阶段2：提交阶段
  let phase2_start = Time::now()
  let phase2_results = []
  
  if (phase1_success) {
    // 所有参与者准备成功，执行提交
    for participant in participants {
      let commit_result = ConsistencyValidator::commit_participant(
        consistency_validator,
        order_tx,
        participant
      )
      phase2_results = phase2_results.push(commit_result)
    }
  } else {
    // 有参与者准备失败，执行回滚
    for participant in participants {
      let rollback_result = ConsistencyValidator::rollback_participant(
        consistency_validator,
        order_tx,
        participant
      )
      phase2_results = phase2_results.push(rollback_result)
    }
  }
  
  let phase2_end = Time::now()
  
  // 验证事务结果
  let transaction_result = ConsistencyValidator::validate_transaction_consistency(
    consistency_validator,
    order_tx,
    phase1_results,
    phase2_results
  )
  
  // 验证ACID属性
  let acid_validation = ConsistencyValidator::validate_acid_properties(
    consistency_validator,
    order_tx,
    transaction_result
  )
  
  // 验证原子性
  assert_true(ACIDValidation::atomicity_holds(acid_validation))
  
  // 验证一致性
  assert_true(ACIDValidation::consistency_holds(acid_validation))
  
  // 验证隔离性
  assert_true(ACIDValidation::isolation_holds(acid_validation))
  
  // 验证持久性
  assert_true(ACIDValidation::durability_holds(acid_validation))
  
  // 测试Saga模式（补偿事务）
  let saga_tx = DistributedTransaction::new("saga_order_processing_002")
  
  // 添加Saga步骤
  let saga_steps = [
    SagaStep::new("reserve_inventory", "inventory_service", "compensate_inventory_reservation"),
    SagaStep::new("process_payment", "payment_service", "refund_payment"),
    SagaStep::new("schedule_shipping", "shipping_service", "cancel_shipping"),
    SagaStep::new("send_notification", "notification_service", "send_cancellation_notification")
  ]
  
  // 执行Saga步骤
  let saga_results = []
  for step in saga_steps {
    let step_result = ConsistencyValidator::execute_saga_step(
      consistency_validator,
      saga_tx,
      step
    )
    saga_results = saga_results.push(step_result)
    
    // 模拟第三步失败
    if (SagaStep::name(step) == "schedule_shipping") {
      break
    }
  }
  
  // 执行补偿事务
  let compensation_results = []
  for i in saga_results.length() - 1 downto 0 {
    match saga_results[i] {
      Ok(_) => {
        let step = saga_steps[i]
        let compensation_result = ConsistencyValidator::execute_compensation(
          consistency_validator,
          saga_tx,
          step
        )
        compensation_results = compensation_results.push(compensation_result)
      }
      Err(_) => break
    }
  }
  
  // 验证Saga一致性
  let saga_consistency = ConsistencyValidator::validate_saga_consistency(
    consistency_validator,
    saga_tx,
    saga_results,
    compensation_results
  )
  
  assert_true(SagaConsistency::compensation_successful(saga_consistency))
  assert_true(SagaConsistency::system_state_consistent(saga_consistency))
}

// Test 4: 分布式事务性能监控
test "distributed transaction performance monitoring" {
  // 创建性能监控器
  let performance_monitor = DistributedTxPerformanceMonitor::new()
  
  // 配置监控参数
  PerformanceMonitor::configure(performance_monitor, [
    ("metrics.collection.interval", IntValue(1000)), // 1秒
    ("performance.thresholds", StringValue("latency:1000,error_rate:0.05,throughput:100")),
    ("alerting.enabled", BoolValue(true)),
    ("historical.retention.days", IntValue(30))
  ])
  
  // 创建性能测试场景
  let test_scenarios = [
    PerformanceTestScenario::new("low_load", 10, 100, 50),   // 10个并发，100个事务，50ms延迟
    PerformanceTestScenario::new("medium_load", 50, 500, 100), // 50个并发，500个事务，100ms延迟
    PerformanceTestScenario::new("high_load", 100, 1000, 200)  // 100个并发，1000个事务，200ms延迟
  ]
  
  for scenario in test_scenarios {
    let scenario_name = PerformanceTestScenario::name(scenario)
    let concurrent_tx = PerformanceTestScenario::concurrent_transactions(scenario)
    let total_tx = PerformanceTestScenario::total_transactions(scenario)
    let avg_delay = PerformanceTestScenario::average_delay(scenario)
    
    // 执行性能测试
    let test_start = Time::now()
    let test_results = []
    
    for i in 0..<total_tx {
      let tx = DistributedTransaction::new("perf_test_" + scenario_name + "_" + i.to_string())
      
      // 添加参与者
      let participants = [
        TransactionParticipant::new("service_a", "operation_a"),
        TransactionParticipant::new("service_b", "operation_b"),
        TransactionParticipant::new("service_c", "operation_c")
      ]
      
      for participant in participants {
        DistributedTransaction::add_participant(tx, participant)
      }
      
      // 执行事务
      let tx_start = Time::now()
      
      // 模拟服务调用延迟
      Thread::sleep(avg_delay)
      
      // 模拟事务处理
      let tx_result = PerformanceMonitor::execute_transaction(performance_monitor, tx)
      
      let tx_end = Time::now()
      let tx_duration = tx_end - tx_start
      
      test_results = test_results.push((tx_result, tx_duration))
      
      // 记录性能指标
      PerformanceMonitor::record_transaction_metrics(
        performance_monitor,
        tx,
        tx_result,
        tx_duration
      )
    }
    
    let test_end = Time::now()
    let test_duration = test_end - test_start
    
    // 分析性能结果
    let performance_analysis = PerformanceMonitor::analyze_performance(
      performance_monitor,
      scenario_name,
      test_results
    )
    
    // 验证性能指标
    assert_true(PerformanceAnalysis::has_throughput(performance_analysis))
    assert_true(PerformanceAnalysis::has_average_latency(performance_analysis))
    assert_true(PerformanceAnalysis::has_error_rate(performance_analysis))
    assert_true(PerformanceAnalysis::has_percentiles(performance_analysis))
    
    let throughput = PerformanceAnalysis::throughput(performance_analysis)
    let avg_latency = PerformanceAnalysis::average_latency(performance_analysis)
    let error_rate = PerformanceAnalysis::error_rate(performance_analysis)
    let p95_latency = PerformanceAnalysis::percentile(performance_analysis, 95)
    let p99_latency = PerformanceAnalysis::percentile(performance_analysis, 99)
    
    // 验证性能合理性
    assert_true(throughput > 0)
    assert_true(avg_latency > avg_delay) // 平均延迟应大于模拟延迟
    assert_true(p95_latency >= avg_latency)
    assert_true(p99_latency >= p95_latency)
    
    // 验证性能阈值
    let threshold_violations = PerformanceMonitor::check_threshold_violations(
      performance_monitor,
      performance_analysis
    )
    
    // 根据负载级别验证不同的性能期望
    match scenario_name {
      "low_load" => {
        assert_true(throughput > 50) // 低负载应有更高吞吐量
        assert_true(avg_latency < 200) // 低负载应有更低延迟
      }
      "medium_load" => {
        assert_true(throughput > 20)
        assert_true(avg_latency < 500)
      }
      "high_load" => {
        assert_true(throughput > 10)
        assert_true(avg_latency < 1000)
      }
      _ => ()
    }
  }
  
  // 生成性能报告
  let performance_report = PerformanceMonitor::generate_performance_report(performance_monitor)
  
  // 验证性能报告
  assert_true(PerformanceReport::has_summary(performance_report))
  assert_true(PerformanceReport::has_trends(performance_report))
  assert_true(PerformanceReport::has_recommendations(performance_report))
  
  let summary = PerformanceReport::summary(performance_report)
  assert_true(Summary::has_total_transactions(summary))
  assert_true(Summary::has_average_throughput(summary))
  assert_true(Summary::has_average_latency(summary))
  
  // 验证性能趋势
  let trends = PerformanceReport::trends(performance_report)
  assert_true(Trends::has_throughput_trend(trends))
  assert_true(Trends::has_latency_trend(trends))
  assert_true(Trends::has_error_rate_trend(trends))
}

// Test 5: 分布式事务故障恢复
test "distributed transaction failure recovery" {
  // 创建故障恢复管理器
  let recovery_manager = DistributedTxRecoveryManager::new()
  
  // 配置恢复参数
  RecoveryManager::configure(recovery_manager, [
    ("retry.max.attempts", IntValue(5)),
    ("retry.backoff.strategy", StringValue("exponential")),
    ("recovery.timeout", IntValue(300000)), // 5分钟
    ("checkpoint.interval", IntValue(10000) // 10秒
  ])
  
  // 创建故障场景
  let failure_scenarios = [
    FailureScenario::new("network_partition", "Network partition between services", 0.3),
    FailureScenario::new("service_crash", "Service crash during transaction", 0.2),
    FailureScenario::new("timeout", "Transaction timeout", 0.4),
    FailureScenario::new("resource_exhaustion", "Resource exhaustion", 0.1)
  ]
  
  for scenario in failure_scenarios {
    let scenario_name = FailureScenario::name(scenario)
    let failure_description = FailureScenario::description(scenario)
    let failure_probability = FailureScenario::failure_probability(scenario)
    
    // 创建测试事务
    let test_tx = DistributedTransaction::new("recovery_test_" + scenario_name)
    
    // 添加参与者
    let participants = [
      TransactionParticipant::new("primary_service", "primary_operation"),
      TransactionParticipant::new("secondary_service", "secondary_operation"),
      TransactionParticipant::new("backup_service", "backup_operation")
    ]
    
    for participant in participants {
      DistributedTransaction::add_participant(test_tx, participant)
    }
    
    // 设置检查点
    let checkpoint_result = RecoveryManager::create_checkpoint(recovery_manager, test_tx)
    assert_true(checkpoint_result)
    
    // 执行事务（模拟故障）
    let execution_result = RecoveryManager::execute_with_failure_simulation(
      recovery_manager,
      test_tx,
      scenario
    )
    
    match execution_result {
      Ok(_) => {
        // 事务成功，验证一致性
        let consistency_check = RecoveryManager::verify_transaction_consistency(
          recovery_manager,
          test_tx
        )
        assert_true(consistency_check)
      }
      Err(failure_info) => {
        // 事务失败，执行恢复
        let recovery_start = Time::now()
        let recovery_result = RecoveryManager::recover_transaction(
          recovery_manager,
          test_tx,
          failure_info
        )
        let recovery_end = Time::now()
        let recovery_duration = recovery_end - recovery_start
        
        // 验证恢复结果
        match recovery_result {
          Ok(recovered_tx) => {
            assert_true(recovery_duration < 60000) // 恢复应在1分钟内完成
            
            // 验证恢复后的事务状态
            assert_true(DistributedTransaction::is_consistent(recovered_tx))
            assert_true(DistributedTransaction::has_all_participants_resolved(recovered_tx))
            
            // 验证恢复日志
            let recovery_log = RecoveryManager::get_recovery_log(recovery_manager, test_tx)
            assert_true(RecoveryLog::has_entries(recovery_log))
            assert_true(RecoveryLog::has_failure_details(recovery_log))
            assert_true(RecoveryLog::has_recovery_actions(recovery_log))
          }
          Err(recovery_failure) => {
            // 恢复失败，验证已采取的补救措施
            assert_true(RecoveryFailure::has_compensation_applied(recovery_failure))
            assert_true(RecoveryFailure::has_manual_intervention_required(recovery_failure))
          }
        }
      }
    }
    
    // 测试部分故障恢复
    let partial_failure_tx = DistributedTransaction::new("partial_failure_" + scenario_name)
    
    // 添加参与者
    let partial_participants = [
      TransactionParticipant::new("service_a", "operation_a"),
      TransactionParticipant::new("service_b", "operation_b"),
      TransactionParticipant::new("service_c", "operation_c"),
      TransactionParticipant::new("service_d", "operation_d")
    ]
    
    for participant in partial_participants {
      DistributedTransaction::add_participant(partial_failure_tx, participant)
    }
    
    // 模拟部分参与者失败
    let partial_failure_result = RecoveryManager::execute_with_partial_failure(
      recovery_manager,
      partial_failure_tx,
      [1, 3] // service_b和service_d失败
    )
    
    // 执行部分恢复
    let partial_recovery = RecoveryManager::recover_partial_failure(
      recovery_manager,
      partial_failure_tx,
      partial_failure_result
    )
    
    // 验证部分恢复结果
    match partial_recovery {
      Ok(recovered_tx) => {
        // 验证成功的参与者已提交
        let successful_participants = [0, 2] // service_a和service_c
        for index in successful_participants {
          let participant = partial_participants[index]
          assert_true(DistributedTransaction::is_participant_committed(recovered_tx, participant))
        }
        
        // 验证失败的参与者已回滚或补偿
        let failed_participants = [1, 3] // service_b和service_d
        for index in failed_participants {
          let participant = partial_participants[index]
          assert_true(DistributedTransaction::is_participant_compensated(recovered_tx, participant))
        }
      }
      Err(_) => assert_true(false)
    }
  }
  
  // 测试恢复策略优化
  let recovery_strategies = [
    RecoveryStrategy::new("immediate_retry", "立即重试", ["timeout", "network_transient"]),
    RecoveryStrategy::new("exponential_backoff", "指数退避重试", ["service_overload"]),
    RecoveryStrategy::new("circuit_breaker", "熔断器模式", ["service_crash", "resource_exhaustion"]),
    RecoveryStrategy::new("manual_intervention", "人工干预", ["data_corruption", "security_breach"])
  ]
  
  for strategy in recovery_strategies {
    let strategy_name = RecoveryStrategy::name(strategy)
    let applicable_failures = RecoveryStrategy::applicable_failures(strategy)
    
    // 测试策略适用性
    for failure_type in applicable_failures {
      let strategy_applicable = RecoveryManager::is_strategy_applicable(
        recovery_manager,
        strategy,
        failure_type
      )
      assert_true(strategy_applicable)
    }
  }
  
  // 生成恢复报告
  let recovery_report = RecoveryManager::generate_recovery_report(recovery_manager)
  
  // 验证恢复报告
  assert_true(RecoveryReport::has_summary(recovery_report))
  assert_true(RecoveryReport::has_failure_analysis(recovery_report))
  assert_true(RecoveryReport::has_recovery_statistics(recovery_report))
  assert_true(RecoveryReport::has_recommendations(recovery_report))
}

// Test 6: 分布式事务监控和告警
test "distributed transaction monitoring and alerting" {
  // 创建监控告警管理器
  let monitoring_manager = DistributedTxMonitoringManager::new()
  
  // 配置监控参数
  MonitoringManager::configure(monitoring_manager, [
    ("monitoring.interval", IntValue(5000)), // 5秒
    ("alert.thresholds", StringValue("error_rate:0.1,latency_p99:2000,active_tx:1000")),
    ("notification.channels", StringValue("email,slack,pagerduty")),
    ("dashboard.enabled", BoolValue(true))
  ])
  
  // 创建监控指标收集器
  let metrics_collector = MetricsCollector::new()
  
  // 定义监控指标
  let metrics = [
    Metric::new("transaction_throughput", "Transactions per second", Counter),
    Metric::new("transaction_latency", "Transaction latency in milliseconds", Histogram),
    Metric::new("error_rate", "Transaction error rate", Gauge),
    Metric::new("active_transactions", "Number of active transactions", Gauge),
    Metric::new("participant_health", "Health status of transaction participants", Gauge)
  ]
  
  for metric in metrics {
    MetricsCollector::register_metric(metrics_collector, metric)
  }
  
  // 创建告警规则
  let alert_rules = [
    AlertRule::new(
      "high_error_rate",
      "Transaction error rate is too high",
      [("error_rate", ">", 0.1)],
      Critical,
      ["email", "pagerduty"]
    ),
    AlertRule::new(
      "high_latency",
      "Transaction latency is too high",
      ["latency_p99", ">", 2000],
      Warning,
      ["slack"]
    ),
    AlertRule::new(
      "too_many_active_tx",
      "Too many active transactions",
      ["active_transactions", ">", 1000],
      Warning,
      ["slack"]
    ),
    AlertRule::new(
      "participant_unhealthy",
      "Transaction participant is unhealthy",
      ["participant_health", "<", 0.8],
      Critical,
      ["email", "slack", "pagerduty"]
    )
  ]
  
  for rule in alert_rules {
    MonitoringManager::add_alert_rule(monitoring_manager, rule)
  }
  
  // 模拟分布式事务流量
  let simulation_duration = 30000 // 30秒
  let simulation_start = Time::now()
  let simulation_end = simulation_start + simulation_duration
  
  let mut total_transactions = 0
  let mut successful_transactions = 0
  let mut failed_transactions = 0
  let mut total_latency = 0
  
  while (Time::now() < simulation_end) {
    // 模拟事务创建
    let tx_count = (Math::random() * 10).to_int() + 1 // 1-10个事务
    
    for i in 0..<tx_count {
      let tx = DistributedTransaction::new("monitoring_test_" + total_transactions.to_string())
      
      // 添加参与者
      let participant_count = (Math::random() * 3).to_int() + 2 // 2-4个参与者
      for j in 0..<participant_count {
        let participant = TransactionParticipant::new(
          "service_" + j.to_string(),
          "operation_" + j.to_string()
        )
        DistributedTransaction::add_participant(tx, participant)
      }
      
      // 模拟事务执行
      let tx_start = Time::now()
      
      // 模拟不同的事务执行时间
      let base_latency = 100 + (Math::random() * 400).to_int() // 100-500ms
      
      // 10%的事务有高延迟
      let actual_latency = if (Math::random() < 0.1) {
        base_latency + 2000 // 额外2秒
      } else {
        base_latency
      }
      
      Thread::sleep(actual_latency)
      
      // 5%的事务失败
      let tx_success = Math::random() > 0.05
      
      let tx_end = Time::now()
      let tx_duration = tx_end - tx_start
      
      // 记录指标
      MetricsCollector::increment_counter(metrics_collector, "transaction_throughput")
      MetricsCollector::record_histogram(metrics_collector, "transaction_latency", tx_duration)
      
      if (tx_success) {
        successful_transactions = successful_transactions + 1
      } else {
        failed_transactions = failed_transactions + 1
        MetricsCollector::increment_counter(metrics_collector, "error_rate")
      }
      
      total_transactions = total_transactions + 1
      total_latency = total_latency + tx_duration
      
      // 更新活跃事务数
      let active_tx = total_transactions - successful_transactions - failed_transactions
      MetricsCollector::set_gauge(metrics_collector, "active_transactions", active_tx.to_float())
      
      // 模拟参与者健康状态
      for j in 0..<participant_count {
        let health = 0.7 + (Math::random() * 0.3) // 0.7-1.0健康分数
        MetricsCollector::set_gauge(
          metrics_collector,
          "participant_health",
          health
        )
      }
    }
    
    // 检查告警规则
    let current_metrics = MetricsCollector::get_all_metrics(metrics_collector)
    let alert_evaluations = MonitoringManager::evaluate_alert_rules(
      monitoring_manager,
      current_metrics
    )
    
    // 处理触发的告警
    for evaluation in alert_evaluations {
      if (AlertEvaluation::is_triggered(evaluation)) {
        let alert = AlertEvaluation::to_alert(evaluation)
        let notification_result = MonitoringManager::send_alert_notification(
          monitoring_manager,
          alert
        )
        assert_true(notification_result)
      }
    }
    
    Thread::sleep(1000) // 1秒间隔
  }
  
  // 生成监控报告
  let monitoring_report = MonitoringManager::generate_monitoring_report(
    monitoring_manager,
    simulation_start,
    simulation_end
  )
  
  // 验证监控报告
  assert_true(MonitoringReport::has_summary(monitoring_report))
  assert_true(MonitoringReport::has_performance_metrics(monitoring_report))
  assert_true(MonitoringReport::has_alert_summary(monitoring_report))
  assert_true(MonitoringReport::has_health_status(monitoring_report))
  
  let summary = MonitoringReport::summary(monitoring_report)
  assert_true(Summary::has_total_transactions(summary))
  assert_true(Summary::has_success_rate(summary))
  assert_true(Summary::has_average_latency(summary))
  
  // 验证性能指标
  let performance_metrics = MonitoringReport::performance_metrics(monitoring_report)
  assert_true(PerformanceMetrics::has_throughput(performance_metrics))
  assert_true(PerformanceMetrics::has_latency_distribution(performance_metrics))
  assert_true(PerformanceMetrics::has_error_rate(performance_metrics))
  
  // 验证告警摘要
  let alert_summary = MonitoringReport::alert_summary(monitoring_report)
  assert_true(AlertSummary::has_total_alerts(alert_summary))
  assert_true(AlertSummary::has_critical_alerts(alert_summary))
  assert_true(AlertSummary::has_warning_alerts(alert_summary))
  
  // 验证健康状态
  let health_status = MonitoringReport::health_status(monitoring_report)
  assert_true(HealthStatus::has_overall_health(health_status))
  assert_true(HealthStatus::has_participant_health(health_status))
  
  let overall_health = HealthStatus::overall_health(health_status)
  assert_true(overall_health >= 0.0 && overall_health <= 1.0)
  
  // 测试实时仪表板数据
  let dashboard_data = MonitoringManager::get_dashboard_data(monitoring_manager)
  assert_true(DashboardData::has_real_time_metrics(dashboard_data))
  assert_true(DashboardData::has_active_transactions(dashboard_data))
  assert_true(DashboardData::has_recent_alerts(dashboard_data))
  
  // 验证实时指标
  let real_time_metrics = DashboardData::real_time_metrics(dashboard_data)
  assert_true(RealTimeMetrics::has_current_throughput(real_time_metrics))
  assert_true(RealTimeMetrics::has_current_latency(real_time_metrics))
  assert_true(RealTimeMetrics::has_current_error_rate(real_time_metrics))
}