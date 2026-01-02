// Azimuth 遥测事件驱动架构测试用例
// 专注于遥测系统的事件驱动架构和消息传递机制

// 测试1: 基础事件发布和订阅
test "基础事件发布和订阅测试" {
  let event_bus = EventBus::new("遥测事件总线")
  
  // 创建事件发布者
  let telemetry_publisher = TelemetryEventPublisher::new("遥测发布者")
  
  // 创建事件订阅者
  let metric_subscriber = MetricEventSubscriber::new("指标订阅者")
  let trace_subscriber = TraceEventSubscriber::new("追踪订阅者")
  let log_subscriber = LogEventSubscriber::new("日志订阅者")
  
  // 注册订阅者
  EventBus::subscribe(event_bus, "metric.event", metric_subscriber)
  EventBus::subscribe(event_bus, "trace.event", trace_subscriber)
  EventBus::subscribe(event_bus, "log.event", log_subscriber)
  
  // 注册发布者
  EventBus::register_publisher(event_bus, telemetry_publisher)
  
  // 创建测试事件
  let metric_event = MetricEvent::new("cpu.usage", 75.5, "server-01")
  let trace_event = TraceEvent::new("trace-123", "api.request", "success", 250)
  let log_event = LogEvent::new("error", "Database connection failed", "db.service")
  
  // 发布事件
  EventBus::publish(event_bus, telemetry_publisher, "metric.event", metric_event)
  EventBus::publish(event_bus, telemetry_publisher, "trace.event", trace_event)
  EventBus::publish(event_bus, telemetry_publisher, "log.event", log_event)
  
  // 等待事件处理
  Time::sleep(1000)
  
  // 验证事件处理结果
  let metric_events = MetricEventSubscriber::get_received_events(metric_subscriber)
  let trace_events = TraceEventSubscriber::get_received_events(trace_subscriber)
  let log_events = LogEventSubscriber::get_received_events(log_subscriber)
  
  assert_eq(metric_events.length(), 1)
  assert_eq(trace_events.length(), 1)
  assert_eq(log_events.length(), 1)
  
  assert_eq(metric_events[0].metric_name, "cpu.usage")
  assert_eq(metric_events[0].value, 75.5)
  
  assert_eq(trace_events[0].trace_id, "trace-123")
  assert_eq(trace_events[0].operation_name, "api.request")
  
  assert_eq(log_events[0].level, "error")
  assert_eq(log_events[0].message, "Database connection failed")
}

// 测试2: 事件路由和分发策略
test "事件路由和分发策略测试" {
  let event_bus = EventBus::new("路由事件总线")
  let router = EventRouter::new()
  
  // 配置路由规则
  let content_based_router = ContentBasedRouter::new()
  let header_based_router = HeaderBasedRouter::new()
  let pattern_based_router = PatternBasedRouter::new()
  
  // 内容路由规则
  ContentBasedRouter::add_rule(content_based_router, 
    fn(event) { event.get_attribute("severity") == Some("critical") },
    ["critical.handler", "alert.handler"]
  )
  
  ContentBasedRouter::add_rule(content_based_router,
    fn(event) { event.get_attribute("service") == Some("payment.service") },
    ["payment.handler"]
  )
  
  // 头部路由规则
  HeaderBasedRouter::add_rule(header_based_router, "event-type", "metric", ["metric.handler"])
  HeaderBasedRouter::add_rule(header_based_router, "event-type", "trace", ["trace.handler"])
  HeaderBasedRouter::add_rule(header_based_router, "event-type", "log", ["log.handler"])
  
  // 模式路由规则
  PatternBasedRouter::add_rule(pattern_based_router, "telemetry.*", ["telemetry.handler"])
  PatternBasedRouter::add_rule(pattern_based_router, "alert.*", ["alert.handler"])
  
  // 设置路由策略
  EventRouter::add_strategy(router, content_based_router)
  EventRouter::add_strategy(router, header_based_router)
  EventRouter::add_strategy(router, pattern_based_router)
  
  // 创建处理器
  let critical_handler = EventHandler::new("critical.handler")
  let alert_handler = EventHandler::new("alert.handler")
  let payment_handler = EventHandler::new("payment.handler")
  let metric_handler = EventHandler::new("metric.handler")
  let trace_handler = EventHandler::new("trace.handler")
  let log_handler = EventHandler::new("log.handler")
  let telemetry_handler = EventHandler::new("telemetry.handler")
  
  // 注册处理器
  for handler in [critical_handler, alert_handler, payment_handler, metric_handler, trace_handler, log_handler, telemetry_handler] {
    EventBus::register_handler(event_bus, handler)
  }
  
  // 设置路由器
  EventBus::set_router(event_bus, router)
  
  // 创建测试事件
  let critical_metric = create_telemetry_event("telemetry.metric", [
    ("severity", "critical"),
    ("event-type", "metric"),
    ("metric.name", "cpu.usage"),
    ("metric.value", "95.0")
  ])
  
  let payment_trace = create_telemetry_event("telemetry.trace", [
    ("service", "payment.service"),
    ("event-type", "trace"),
    ("trace.id", "trace-456")
  ])
  
  let normal_log = create_telemetry_event("telemetry.log", [
    ("severity", "info"),
    ("event-type", "log"),
    ("log.message", "User login successful")
  ])
  
  // 发布事件
  let publisher = TelemetryEventPublisher::new("测试发布者")
  EventBus::register_publisher(event_bus, publisher)
  
  EventBus::publish(event_bus, publisher, "telemetry.metric", critical_metric)
  EventBus::publish(event_bus, publisher, "telemetry.trace", payment_trace)
  EventBus::publish(event_bus, publisher, "telemetry.log", normal_log)
  
  // 等待事件处理
  Time::sleep(1000)
  
  // 验证路由结果
  let critical_events = EventHandler::get_received_events(critical_handler)
  let alert_events = EventHandler::get_received_events(alert_handler)
  let payment_events = EventHandler::get_received_events(payment_handler)
  let metric_events = EventHandler::get_received_events(metric_handler)
  let trace_events = EventHandler::get_received_events(trace_handler)
  let log_events = EventHandler::get_received_events(log_handler)
  let telemetry_events = EventHandler::get_received_events(telemetry_handler)
  
  // 关键指标应该路由到critical.handler和alert.handler
  assert_eq(critical_events.length(), 1)
  assert_eq(alert_events.length(), 1)
  
  // 支付追踪应该路由到payment.handler和trace.handler
  assert_eq(payment_events.length(), 1)
  assert_eq(trace_events.length(), 1)
  
  // 普通日志应该路由到log.handler
  assert_eq(log_events.length(), 1)
  
  // 所有事件都应该路由到telemetry.handler
  assert_eq(telemetry_events.length(), 3)
  
  // 指标事件应该路由到metric.handler
  assert_eq(metric_events.length(), 1)
}

// 测试3: 事件持久化和重放
test "事件持久化和重放测试" {
  let event_bus = EventBus::new("持久化事件总线")
  let persistence_manager = EventPersistenceManager::new()
  
  // 配置持久化策略
  let file_persistence = FileEventPersistence::new("/tmp/telemetry_events")
  let database_persistence = DatabaseEventPersistence::new("postgresql://localhost/telemetry_events")
  
  EventPersistenceManager::add_strategy(persistence_manager, file_persistence)
  EventPersistenceManager::add_strategy(persistence_manager, database_persistence)
  
  // 配置快照策略
  let snapshot_strategy = SnapshotStrategy::new()
  SnapshotStrategy::set_interval(snapshot_strategy, 10000)  // 每10秒快照
  SnapshotStrategy::set_event_threshold(snapshot_strategy, 100)  // 每100个事件快照
  
  EventPersistenceManager::set_snapshot_strategy(persistence_manager, snapshot_strategy)
  
  // 启用持久化
  EventBus::enable_persistence(event_bus, persistence_manager)
  
  // 创建事件发布者
  let publisher = TelemetryEventPublisher::new("持久化测试发布者")
  EventBus::register_publisher(event_bus, publisher)
  
  // 发布一系列事件
  for i in 0..=49 {
    let event = create_telemetry_event("telemetry.metric", [
      ("metric.name", "test.metric." + i.to_string()),
      ("metric.value", i.to_string()),
      ("sequence", i.to_string())
    ])
    EventBus::publish(event_bus, publisher, "telemetry.metric", event)
  }
  
  // 创建快照
  let snapshot_id = EventPersistenceManager::create_snapshot(persistence_manager, event_bus)
  
  // 继续发布事件
  for i in 50..=99 {
    let event = create_telemetry_event("telemetry.metric", [
      ("metric.name", "test.metric." + i.to_string()),
      ("metric.value", i.to_string()),
      ("sequence", i.to_string())
    ])
    EventBus::publish(event_bus, publisher, "telemetry.metric", event)
  }
  
  // 验证持久化统计
  let persistence_stats = EventPersistenceManager::get_stats(persistence_manager)
  assert_eq(persistence_stats.events_persisted, 100)
  assert_true(persistence_stats.snapshots_created >= 1)
  
  // 创建新的事件总线并重放事件
  let new_event_bus = EventBus::new("重放事件总线")
  let replay_subscriber = ReplayTestSubscriber::new()
  
  EventBus::subscribe(new_event_bus, "telemetry.metric", replay_subscriber)
  
  // 重放事件
  EventPersistenceManager::replay_from_snapshot(persistence_manager, new_event_bus, snapshot_id)
  
  // 等待重放完成
  Time::sleep(2000)
  
  // 验证重放结果
  let replayed_events = ReplayTestSubscriber::get_received_events(replay_subscriber)
  assert_eq(replayed_events.length(), 50)  // 只重放快照时的50个事件
  
  // 验证事件顺序
  for i in 0..=49 {
    let event = replayed_events[i]
    assert_eq(event.get_attribute("sequence"), Some(i.to_string()))
  }
}

// 测试4: 事件过滤和转换
test "事件过滤和转换测试" {
  let event_bus = EventBus::new("过滤转换事件总线")
  let event_processor = EventProcessor::new()
  
  // 创建事件过滤器
  let severity_filter = EventFilter::new("severity", ["critical", "error"])
  let service_filter = EventFilter::new("service", ["payment.service", "auth.service"])
  let time_range_filter = TimeRangeEventFilter::new(
    "2023-01-01T00:00:00Z",
    "2023-01-01T23:59:59Z"
  )
  
  // 创建事件转换器
  let enrichment_transformer = EventEnrichmentTransformer::new()
  let format_transformer = EventFormatTransformer::new()
  let aggregation_transformer = EventAggregationTransformer::new()
  
  // 配置转换器
  EventEnrichmentTransformer::add_rule(enrichment_transformer, 
    fn(event) { event.get_attribute("service") == Some("api.service") },
    fn(event) { event.add_attribute("service.type", "frontend") }
  )
  
  EventFormatTransformer::add_rule(format_transformer,
    fn(event) { event.get_type() == "metric" },
    fn(event) { event.set_format("prometheus") }
  )
  
  EventAggregationTransformer::add_aggregation(aggregation_transformer,
    "metric.count",
    fn(events) { events.length().to_float() }
  )
  
  // 添加过滤器和转换器
  EventProcessor::add_filter(event_processor, severity_filter)
  EventProcessor::add_filter(event_processor, service_filter)
  EventProcessor::add_filter(event_processor, time_range_filter)
  
  EventProcessor::add_transformer(event_processor, enrichment_transformer)
  EventProcessor::add_transformer(event_processor, format_transformer)
  EventProcessor::add_transformer(event_processor, aggregation_transformer)
  
  // 设置事件处理器
  EventBus::set_processor(event_bus, event_processor)
  
  // 创建测试事件
  let events = [
    create_telemetry_event_with_time("telemetry.metric", "2023-01-01T10:00:00Z", [
      ("severity", "info"),
      ("service", "api.service"),
      ("metric.name", "cpu.usage")
    ]),
    create_telemetry_event_with_time("telemetry.metric", "2023-01-01T11:00:00Z", [
      ("severity", "critical"),
      ("service", "payment.service"),
      ("metric.name", "transaction.count")
    ]),
    create_telemetry_event_with_time("telemetry.log", "2023-01-01T12:00:00Z", [
      ("severity", "error"),
      ("service", "auth.service"),
      ("log.message", "Authentication failed")
    ]),
    create_telemetry_event_with_time("telemetry.metric", "2022-12-31T23:00:00Z", [  // 超出时间范围
      ("severity", "critical"),
      ("service", "payment.service"),
      ("metric.name", "error.rate")
    ])
  ]
  
  // 发布事件
  let publisher = TelemetryEventPublisher::new("过滤测试发布者")
  EventBus::register_publisher(event_bus, publisher)
  
  let test_subscriber = TestEventSubscriber::new()
  EventBus::subscribe(event_bus, "telemetry.*", test_subscriber)
  
  for event in events {
    EventBus::publish(event_bus, publisher, event.get_type(), event)
  }
  
  // 等待处理完成
  Time::sleep(1000)
  
  // 验证过滤和转换结果
  let processed_events = TestEventSubscriber::get_received_events(test_subscriber)
  
  // 只有3个事件应该通过过滤（排除时间范围外的事件）
  assert_eq(processed_events.length(), 3)
  
  // 验证转换结果
  for event in processed_events {
    // 验证服务类型丰富
    if event.get_attribute("service") == Some("api.service") {
      assert_eq(event.get_attribute("service.type"), Some("frontend"))
    }
    
    // 验证格式转换
    if event.get_type() == "metric" {
      assert_eq(event.get_format(), Some("prometheus"))
    }
  }
  
  // 验证聚合转换
  let aggregated_events = processed_events.filter(|e| e.get_type() == "metric.count")
  assert_true(aggregated_events.length() > 0)
}

// 测试5: 事件可靠性和容错
test "事件可靠性和容错测试" {
  let event_bus = EventBus::new("可靠事件总线")
  let reliability_manager = EventReliabilityManager::new()
  
  // 配置可靠性策略
  let retry_strategy = ExponentialBackoffRetryStrategy::new(3, 100, 2000)
  let dead_letter_strategy = DeadLetterStrategy::new("/tmp/dead_letter_events")
  let circuit_breaker_strategy = CircuitBreakerStrategy::new(5, 30000)  // 5次失败后断路，30秒后重试
  
  EventReliabilityManager::add_strategy(reliability_manager, retry_strategy)
  EventReliabilityManager::add_strategy(reliability_manager, dead_letter_strategy)
  EventReliabilityManager::add_strategy(reliability_manager, circuit_breaker_strategy)
  
  // 启用可靠性
  EventBus::enable_reliability(event_bus, reliability_manager)
  
  // 创建会失败的处理器
  let failing_handler = FailingEventHandler::new("失败处理器", 0.6)  // 60%失败率
  let recovery_handler = RecoveryEventHandler::new("恢复处理器")
  
  // 配置故障转移
  let failover_strategy = FailoverStrategy::new()
  FailoverStrategy::add_handler(failover_strategy, failing_handler)
  FailoverStrategy::add_handler(failover_strategy, recovery_handler)
  
  // 注册处理器
  EventBus::register_handler(event_bus, failing_handler)
  EventBus::register_handler(event_bus, recovery_handler)
  EventBus::set_failover_strategy(event_bus, failover_strategy)
  
  // 发布事件
  let publisher = TelemetryEventPublisher::new("可靠性测试发布者")
  EventBus::register_publisher(event_bus, publisher)
  
  for i in 0..=19 {
    let event = create_telemetry_event("telemetry.test", [
      ("event.id", "event-" + i.to_string()),
      ("payload", "test data " + i.to_string())
    ])
    EventBus::publish(event_bus, publisher, "telemetry.test", event)
  }
  
  // 等待处理完成
  Time::sleep(5000)
  
  // 验证可靠性结果
  let reliability_stats = EventReliabilityManager::get_stats(reliability_manager)
  let failing_stats = FailingEventHandler::get_stats(failing_handler)
  let recovery_stats = RecoveryEventHandler::get_stats(recovery_handler)
  
  // 验证重试机制
  assert_true(reliability_stats.total_retries > 0)
  assert_true(reliability_stats.successful_retries > 0)
  
  // 验证死信队列
  assert_true(reliability_stats.dead_letter_events >= 0)
  
  // 验证故障转移
  assert_true(failing_stats.attempted_events > 0)
  assert_true(recovery_stats.processed_events > 0)
  
  // 验证总处理量
  let total_processed = failing_stats.successful_events + recovery_stats.processed_events + reliability_stats.dead_letter_events
  assert_eq(total_processed, 20)
}

// 测试6: 事件监控和指标
test "事件监控和指标测试" {
  let event_bus = EventBus::new("监控事件总线")
  let event_monitor = EventMonitor::new()
  
  // 配置监控指标
  EventMonitor::enable_throughput_monitoring(event_monitor)
  EventMonitor::enable_latency_monitoring(event_monitor)
  EventMonitor::enable_error_rate_monitoring(event_monitor)
  EventMonitor::enable_queue_size_monitoring(event_monitor)
  
  // 启动监控
  EventMonitor::start_monitoring(event_monitor, event_bus)
  
  // 创建测试发布者和订阅者
  let publisher = TelemetryEventPublisher::new("监控测试发布者")
  let subscriber = TestEventSubscriber::new()
  
  EventBus::register_publisher(event_bus, publisher)
  EventBus::subscribe(event_bus, "telemetry.*", subscriber)
  
  // 发布不同类型和负载的事件
  for i in 0..=99 {
    let event_type = ["telemetry.metric", "telemetry.trace", "telemetry.log"][i % 3]
    let event = create_telemetry_event(event_type, [
      ("event.id", "event-" + i.to_string()),
      ("payload", "test data " + i.to_string())
    ])
    EventBus::publish(event_bus, publisher, event_type, event)
  }
  
  // 等待处理完成
  Time::sleep(3000)
  
  // 获取监控指标
  let monitoring_metrics = EventMonitor::get_metrics(event_monitor)
  
  // 验证吞吐量指标
  assert_true(monitoring_metrics.events_per_second > 0)
  assert_true(monitoring_metrics.peak_throughput > 0)
  assert_true(monitoring_metrics.total_events_published == 100)
  assert_true(monitoring_metrics.total_events_processed == 100)
  
  // 验证延迟指标
  assert_true(monitoring_metrics.avg_latency_ms > 0)
  assert_true(monitoring_metrics.p95_latency_ms > 0)
  assert_true(monitoring_metrics.p99_latency_ms > 0)
  
  // 验证错误率指标
  assert_true(monitoring_metrics.error_rate >= 0.0 && monitoring_metrics.error_rate <= 1.0)
  
  // 验证队列大小指标
  assert_true(monitoring_metrics.max_queue_size >= 0)
  assert_true(monitoring_metrics.avg_queue_size >= 0)
  
  // 验证事件类型分布
  assert_eq(monitoring_metrics.event_type_distribution.get("telemetry.metric"), Some(34))
  assert_eq(monitoring_metrics.event_type_distribution.get("telemetry.trace"), Some(33))
  assert_eq(monitoring_metrics.event_type_distribution.get("telemetry.log"), Some(33))
  
  // 验证监控历史
  let monitoring_history = EventMonitor::get_history(event_monitor, 3600)  // 最近1小时
  assert_true(monitoring_history.length() > 0)
}

// 测试7: 分布式事件总线
test "分布式事件总线测试" {
  // 创建分布式事件总线节点
  let node1 = DistributedEventBusNode::new("node1", "localhost:8001")
  let node2 = DistributedEventBusNode::new("node2", "localhost:8002")
  let node3 = DistributedEventBusNode::new("node3", "localhost:8003")
  
  // 创建分布式事件总线集群
  let cluster = DistributedEventBusCluster::new()
  DistributedEventBusCluster::add_node(cluster, node1)
  DistributedEventBusCluster::add_node(cluster, node2)
  DistributedEventBusCluster::add_node(cluster, node3)
  
  // 配置分布式策略
  let consistent_hashing = ConsistentHashingStrategy::new()
  let replication_strategy = EventReplicationStrategy::new(2)  // 2副本
  let partition_strategy = PartitionStrategy::new("event.type")
  
  DistributedEventBusCluster::set_distribution_strategy(cluster, consistent_hashing)
  DistributedEventBusCluster::set_replication_strategy(cluster, replication_strategy)
  DistributedEventBusCluster::set_partition_strategy(cluster, partition_strategy)
  
  // 创建分布式发布者
  let distributed_publisher = DistributedEventPublisher::new("分布式发布者")
  
  // 创建分布式订阅者
  let subscriber1 = TestEventSubscriber::new()
  let subscriber2 = TestEventSubscriber::new()
  let subscriber3 = TestEventSubscriber::new()
  
  // 在每个节点上注册订阅者
  DistributedEventBusCluster::subscribe(cluster, "node1", "telemetry.*", subscriber1)
  DistributedEventBusCluster::subscribe(cluster, "node2", "telemetry.*", subscriber2)
  DistributedEventBusCluster::subscribe(cluster, "node3", "telemetry.*", subscriber3)
  
  // 发布分布式事件
  for i in 0..=29 {
    let event_type = ["telemetry.metric", "telemetry.trace", "telemetry.log"][i % 3]
    let event = create_telemetry_event(event_type, [
      ("event.id", "distributed-event-" + i.to_string()),
      ("node.id", "publisher-node")
    ])
    DistributedEventPublisher::publish(distributed_publisher, cluster, event_type, event)
  }
  
  // 等待分布式处理完成
  Time::sleep(3000)
  
  // 验证分布式处理结果
  let events1 = TestEventSubscriber::get_received_events(subscriber1)
  let events2 = TestEventSubscriber::get_received_events(subscriber2)
  let events3 = TestEventSubscriber::get_received_events(subscriber3)
  
  // 验证事件分布
  let total_received = events1.length() + events2.length() + events3.length()
  assert_true(total_received >= 30)  // 至少所有事件都被接收一次
  
  // 验证复制
  let distribution_stats = DistributedEventBusCluster::get_distribution_stats(cluster)
  assert_true(distribution_stats.replication_factor >= 2)
  assert_true(distribution_stats.event_distribution.all(|count| count > 0))
  
  // 验证节点故障处理
  DistributedEventBusCluster::simulate_node_failure(cluster, "node2")
  
  // 继续发布事件
  for i in 30..=39 {
    let event = create_telemetry_event("telemetry.metric", [
      ("event.id", "failure-test-" + i.to_string()),
      ("node.id", "publisher-node")
    ])
    DistributedEventPublisher::publish(distributed_publisher, cluster, "telemetry.metric", event)
  }
  
  Time::sleep(2000)
  
  // 验证故障转移
  let failover_stats = DistributedEventBusCluster::get_failover_stats(cluster)
  assert_true(failover_stats.failed_nodes > 0)
  assert_true(failover_stats.successful_failovers > 0)
}

// 测试8: 事件安全和加密
test "事件安全和加密测试" {
  let event_bus = EventBus::new("安全事件总线")
  let security_manager = EventSecurityManager::new()
  
  // 配置安全策略
  let encryption_strategy = AESEventEncryptionStrategy::new("secure-key-123")
  let authentication_strategy = JWTAuthenticationStrategy::new("secret-key")
  let authorization_strategy = RBACAuthorizationStrategy::new()
  
  // 配置基于角色的访问控制
  RBACAuthorizationStrategy::add_role(authorization_strategy, "admin", ["publish", "subscribe", "manage"])
  RBACAuthorizationStrategy::add_role(authorization_strategy, "publisher", ["publish"])
  RBACAuthorizationStrategy::add_role(authorization_strategy, "subscriber", ["subscribe"])
  
  // 添加安全策略
  EventSecurityManager::add_encryption_strategy(security_manager, encryption_strategy)
  EventSecurityManager::add_authentication_strategy(security_manager, authentication_strategy)
  EventSecurityManager::add_authorization_strategy(security_manager, authorization_strategy)
  
  // 启用安全
  EventBus::enable_security(event_bus, security_manager)
  
  // 创建用户和令牌
  let admin_user = User::new("admin", "admin")
  let publisher_user = User::new("publisher", "publisher")
  let subscriber_user = User::new("subscriber", "subscriber")
  
  let admin_token = JWTAuthenticationStrategy::generate_token(authentication_strategy, admin_user)
  let publisher_token = JWTAuthenticationStrategy::generate_token(authentication_strategy, publisher_user)
  let subscriber_token = JWTAuthenticationStrategy::generate_token(authentication_strategy, subscriber_user)
  
  // 创建带认证的发布者和订阅者
  let admin_publisher = AuthenticatedEventPublisher::new("管理员发布者", admin_token)
  let publisher_publisher = AuthenticatedEventPublisher::new("普通发布者", publisher_token)
  let subscriber_subscriber = AuthenticatedEventSubscriber::new("订阅者", subscriber_token)
  
  // 注册带认证的发布者和订阅者
  EventBus::register_authenticated_publisher(event_bus, admin_publisher)
  EventBus::register_authenticated_publisher(event_bus, publisher_publisher)
  EventBus::subscribe_authenticated(event_bus, "telemetry.*", subscriber_subscriber)
  
  // 创建包含敏感数据的事件
  let sensitive_event = create_telemetry_event("telemetry.sensitive", [
    ("user.id", "user-12345"),
    ("credit.card", "4111-1111-1111-1111"),
    ("ssn", "123-45-6789"),
    ("public.data", "this is public")
  ])
  
  // 发布事件
  EventBus::publish_authenticated(event_bus, admin_publisher, "telemetry.sensitive", sensitive_event)
  
  // 等待处理
  Time::sleep(1000)
  
  // 验证安全和加密结果
  let received_events = AuthenticatedEventSubscriber::get_received_events(subscriber_subscriber)
  assert_eq(received_events.length(), 1)
  
  let received_event = received_events[0]
  
  // 验证敏感数据已加密
  let user_id = received_event.get_attribute("user.id")
  let credit_card = received_event.get_attribute("credit.card")
  let ssn = received_event.get_attribute("ssn")
  
  assert_ne(user_id, Some("user-12345"))
  assert_ne(credit_card, Some("4111-1111-1111-1111"))
  assert_ne(ssn, Some("123-45-6789"))
  
  // 验证公开数据保持原样
  assert_eq(received_event.get_attribute("public.data"), Some("this is public"))
  
  // 验证安全统计
  let security_stats = EventSecurityManager::get_stats(security_manager)
  assert_eq(security_stats.encrypted_events, 1)
  assert_eq(security_stats.authenticated_operations, 2)  // 1 publish + 1 subscribe
  assert_true(security_stats.authorization_checks > 0)
}

// 测试9: 事件流处理
test "事件流处理测试" {
  let event_bus = EventBus::new("流处理事件总线")
  let stream_processor = EventStreamProcessor::new()
  
  // 配置流处理拓扑
  let topology = StreamProcessingTopology::new()
  
  // 创建流处理节点
  let source_node = StreamSourceNode::new("event.source")
  let filter_node = StreamFilterNode::new("event.filter", fn(event) {
    event.get_attribute("severity") == Some("critical")
  })
  let transform_node = StreamTransformNode::new("event.transform", fn(event) {
    event.add_attribute("processed", "true")
    event.add_attribute("process.time", Time::now().to_string())
    event
  })
  let aggregate_node = StreamAggregateNode::new("event.aggregate", "1m")  // 1分钟窗口
  let sink_node = StreamSinkNode::new("event.sink")
  
  // 构建拓扑
  StreamProcessingTopology::add_node(topology, source_node)
  StreamProcessingTopology::add_node(topology, filter_node)
  StreamProcessingTopology::add_node(topology, transform_node)
  StreamProcessingTopology::add_node(topology, aggregate_node)
  StreamProcessingTopology::add_node(topology, sink_node)
  
  StreamProcessingTopology::connect(topology, source_node, filter_node)
  StreamProcessingTopology::connect(topology, filter_node, transform_node)
  StreamProcessingTopology::connect(topology, transform_node, aggregate_node)
  StreamProcessingTopology::connect(topology, aggregate_node, sink_node)
  
  // 配置流处理器
  EventStreamProcessor::set_topology(stream_processor, topology)
  EventBus::set_stream_processor(event_bus, stream_processor)
  
  // 创建测试事件流
  let publisher = TelemetryEventPublisher::new("流处理测试发布者")
  EventBus::register_publisher(event_bus, publisher)
  
  // 发布事件流
  for i in 0..=59 {
    let severity = if i % 10 == 0 { "critical" } else { "info" }
    let event = create_telemetry_event("telemetry.stream", [
      ("event.id", "stream-event-" + i.to_string()),
      ("severity", severity),
      ("metric.value", i.to_string())
    ])
    EventBus::publish(event_bus, publisher, "telemetry.stream", event)
    Time::sleep(100)  // 100ms间隔
  }
  
  // 等待流处理完成
  Time::sleep(2000)
  
  // 验证流处理结果
  let sink_events = StreamSinkNode::get_events(sink_node)
  let stream_stats = EventStreamProcessor::get_stats(stream_processor)
  
  // 验证过滤结果（只有critical事件通过）
  assert_eq(sink_events.length(), 6)  // 60个事件中，每10个有1个critical
  
  // 验证转换结果
  for event in sink_events {
    assert_eq(event.get_attribute("processed"), Some("true"))
    assert_true(event.get_attribute("process.time").is_some())
  }
  
  // 验证聚合结果
  let aggregated_events = sink_events.filter(|e| e.get_type() == "aggregated")
  assert_true(aggregated_events.length() > 0)
  
  // 验证流处理统计
  assert_eq(stream_stats.total_events_processed, 60)
  assert_eq(stream_stats.filtered_events, 6)
  assert_true(stream_stats.processing_latency_ms > 0)
  assert_true(stream_stats.throughput_events_per_second > 0)
}

// 测试10: 事件模式匹配和复杂事件处理
test "事件模式匹配和复杂事件处理测试" {
  let event_bus = EventBus::new("复杂事件处理总线")
  let complex_processor = ComplexEventProcessor::new()
  
  // 定义事件模式
  let error_pattern = EventPattern::new("error.pattern", [
    PatternCondition::attribute_equals("severity", "error"),
    PatternCondition::attribute_exists("error.code"),
    PatternCondition::time_window(60000)  // 1分钟内
  ])
  
  let performance_pattern = EventPattern::new("performance.pattern", [
    PatternCondition::attribute_greater_than("response.time", 1000),
    PatternCondition::attribute_equals("service", "api.service"),
    PatternCondition::frequency_condition(5, 300000)  // 5分钟内出现5次
  ])
  
  let fraud_pattern = EventPattern::new("fraud.pattern", [
    PatternCondition::attribute_equals("event.type", "transaction"),
    PatternCondition::sequence_condition([
      ("login", "success"),
      ("transaction", "large_amount"),
      ("logout", "quick")
    ]),
    PatternCondition::time_window(300000)  // 5分钟内
  ])
  
  // 添加模式
  ComplexEventProcessor::add_pattern(complex_processor, error_pattern)
  ComplexEventProcessor::add_pattern(complex_processor, performance_pattern)
  ComplexEventProcessor::add_pattern(complex_processor, fraud_pattern)
  
  // 创建复杂事件处理器
  let error_handler = ComplexEventHandler::new("错误处理器")
  let performance_handler = ComplexEventHandler::new("性能处理器")
  let fraud_handler = ComplexEventHandler::new("欺诈处理器")
  
  ComplexEventProcessor::add_handler(complex_processor, "error.pattern", error_handler)
  ComplexEventProcessor::add_handler(complex_processor, "performance.pattern", performance_handler)
  ComplexEventProcessor::add_handler(complex_processor, "fraud.pattern", fraud_handler)
  
  // 设置复杂事件处理器
  EventBus::set_complex_processor(event_bus, complex_processor)
  
  // 创建测试事件序列
  let publisher = TelemetryEventPublisher::new("复杂事件测试发布者")
  EventBus::register_publisher(event_bus, publisher)
  
  // 发布错误事件序列
  let error_events = [
    create_telemetry_event_with_time("telemetry.error", "2023-01-01T10:00:00Z", [
      ("severity", "error"),
      ("error.code", "500"),
      ("service", "api.service")
    ]),
    create_telemetry_event_with_time("telemetry.error", "2023-01-01T10:00:30Z", [
      ("severity", "error"),
      ("error.code", "503"),
      ("service", "api.service")
    ])
  ]
  
  // 发布性能事件序列
  for i in 0..=4 {
    let event = create_telemetry_event_with_time("telemetry.metric", 
      "2023-01-01T10:0" + i.to_string() + ":00Z", [
      ("response.time", (1500 + i * 100).to_string()),
      ("service", "api.service")
    ])
    EventBus::publish(event_bus, publisher, "telemetry.metric", event)
  }
  
  // 发布欺诈事件序列
  let fraud_events = [
    create_telemetry_event_with_time("telemetry.transaction", "2023-01-01T11:00:00Z", [
      ("event.type", "transaction"),
      ("action", "login"),
      ("result", "success")
    ]),
    create_telemetry_event_with_time("telemetry.transaction", "2023-01-01T11:01:00Z", [
      ("event.type", "transaction"),
      ("action", "transaction"),
      ("amount", "10000")
    ]),
    create_telemetry_event_with_time("telemetry.transaction", "2023-01-01T11:01:30Z", [
      ("event.type", "transaction"),
      ("action", "logout"),
      ("duration", "30")
    ])
  ]
  
  // 发布所有事件
  for event in error_events + fraud_events {
    EventBus::publish(event_bus, publisher, event.get_type(), event)
  }
  
  // 等待复杂事件处理
  Time::sleep(3000)
  
  // 验证复杂事件处理结果
  let error_matches = ComplexEventHandler::get_matched_events(error_handler)
  let performance_matches = ComplexEventHandler::get_matched_events(performance_handler)
  let fraud_matches = ComplexEventHandler::get_matched_events(fraud_handler)
  
  // 验证错误模式匹配
  assert_true(error_matches.length() >= 1)
  
  // 验证性能模式匹配
  assert_true(performance_matches.length() >= 1)
  
  // 验证欺诈模式匹配
  assert_true(fraud_matches.length() >= 1)
  
  // 验证复杂事件处理统计
  let complex_stats = ComplexEventProcessor::get_stats(complex_processor)
  assert_true(complex_stats.total_events_processed > 0)
  assert_true(complex_stats.pattern_matches > 0)
  assert_true(complex_stats.false_positives >= 0)
}

// 辅助函数：创建遥测事件
fn create_telemetry_event(event_type : String, attributes : List<(String, String)>) -> TelemetryEvent {
  let event = TelemetryEvent::new(event_type)
  
  for (key, value) in attributes {
    event.add_attribute(key, value)
  }
  
  event
}

// 辅助函数：创建带时间的遥测事件
fn create_telemetry_event_with_time(event_type : String, timestamp : String, attributes : List<(String, String)>) -> TelemetryEvent {
  let event = create_telemetry_event(event_type, attributes)
  event.set_timestamp(timestamp)
  event
}