// Distributed System Tests for Azimuth Telemetry System
// This file contains test cases for distributed system functionality

// Test 1: Distributed Tracing
test "distributed tracing" {
  let tracer = DistributedTracer::new("azimuth_service")
  
  // Test span creation
  let root_span = Tracer::start_span(tracer, "user_request")
  Span::set_attribute(root_span, "user.id", "12345")
  Span::set_attribute(root_span, "operation.type", "query")
  
  // Test child span creation
  let db_span = Tracer::start_span_with_parent(tracer, "database_query", root_span)
  Span::set_attribute(db_span, "db.statement", "SELECT * FROM users")
  Span::set_attribute(db_span, "db.type", "postgresql")
  
  Span::add_event(db_span, "query.start", [("timestamp", Time::now().to_string())])
  
  // Simulate database operation
  Time::sleep(10) // 10ms
  
  Span::add_event(db_span, "query.complete", [("duration", "10ms")])
  Span::set_status(db_span, SpanStatus::Ok)
  Span::end(db_span)
  
  // Test another child span
  let cache_span = Tracer::start_span_with_parent(tracer, "cache_lookup", root_span)
  Span::set_attribute(cache_span, "cache.key", "user:12345")
  Span::set_attribute(cache_span, "cache.hit", true)
  Span::end(cache_span)
  
  // End root span
  Span::end(root_span)
  
  // Test span context propagation
  let span_context = Span::get_context(root_span)
  assert_true(SpanContext::is_valid(span_context))
  
  let trace_id = SpanContext::get_trace_id(span_context)
  let span_id = SpanContext::get_span_id(span_context)
  
  assert_true(trace_id.length() > 0)
  assert_true(span_id.length() > 0)
  
  // Test span serialization
  let serialized_context = SpanContext::serialize(span_context)
  assert_true(serialized_context.length() > 0)
  
  let deserialized_context = SpanContext::deserialize(serialized_context)
  assert_eq(SpanContext::get_trace_id(deserialized_context), trace_id)
  assert_eq(SpanContext::get_span_id(deserialized_context), span_id)
  
  // Test span collection
  let finished_spans = Tracer::get_finished_spans(tracer)
  assert_eq(finished_spans.length(), 3)
  
  // Test span tree reconstruction
  let span_tree = Tracer::build_span_tree(tracer, trace_id)
  assert_eq(span_tree.root_span.name, "user_request")
  assert_eq(span_tree.child_spans.length(), 2)
}

// Test 2: Service Mesh Integration
test "service mesh integration" {
  let service_mesh = ServiceMesh::new("azimuth_mesh")
  
  // Test service registration
  let user_service = Service::new("user_service", "1.0.0", "http://user-service:8080")
  let order_service = Service::new("order_service", "1.2.0", "http://order-service:8080")
  let payment_service = Service::new("payment_service", "2.0.0", "http://payment-service:8080")
  
  ServiceMesh::register_service(service_mesh, user_service)
  ServiceMesh::register_service(service_mesh, order_service)
  ServiceMesh::register_service(service_mesh, payment_service)
  
  // Test service discovery
  let discovered_services = ServiceMesh::discover_services(service_mesh)
  assert_eq(discovered_services.length(), 3)
  
  let user_service_instance = ServiceMesh::get_service(service_mesh, "user_service")
  assert_true(user_service_instance.is_some())
  
  // Test service health checking
  ServiceMesh::enable_health_checks(service_mesh, true)
  ServiceMesh::set_health_check_interval(service_mesh, 30) // 30 seconds
  
  // Simulate health check results
  ServiceMesh::update_service_health(service_mesh, "user_service", ServiceHealth::Healthy)
  ServiceMesh::update_service_health(service_mesh, "order_service", ServiceHealth::Healthy)
  ServiceMesh::update_service_health(service_mesh, "payment_service", ServiceHealth::Unhealthy)
  
  let healthy_services = ServiceMesh::get_healthy_services(service_mesh)
  assert_eq(healthy_services.length(), 2)
  
  // Test traffic routing
  let routing_rule = RoutingRule::new(
    "user_requests",
    "user_service",
    "path_prefix",
    "/api/users",
    ["user_service"]
  )
  
  ServiceMesh::add_routing_rule(service_mesh, routing_rule)
  
  let target_service = ServiceMesh::route_request(service_mesh, "/api/users/123")
  assert_eq(target_service, Some("user_service"))
  
  // Test load balancing
  ServiceMesh::set_load_balancing_strategy(service_mesh, LoadBalancingStrategy::RoundRobin)
  
  // Add multiple instances of the same service
  let user_service_instance2 = Service::new("user_service", "1.0.0", "http://user-service-2:8080")
  ServiceMesh::register_service_instance(service_mesh, user_service_instance2)
  
  let instance1 = ServiceMesh::select_instance(service_mesh, "user_service")
  let instance2 = ServiceMesh::select_instance(service_mesh, "user_service")
  
  assert_ne(instance1.get_endpoint(), instance2.get_endpoint())
}

// Test 3: Distributed Configuration Management
test "distributed configuration management" {
  let config_manager = DistributedConfigManager::new("azimuth_config")
  
  // Test configuration namespace creation
  let user_config_ns = ConfigManager::create_namespace(config_manager, "user_service")
  let order_config_ns = ConfigManager::create_namespace(config_manager, "order_service")
  
  // Test configuration key-value operations
  ConfigManager::set(config_manager, "user_service", "database.host", "db.user-service.local")
  ConfigManager::set(config_manager, "user_service", "database.port", "5432")
  ConfigManager::set(config_manager, "user_service", "cache.ttl", "300")
  
  ConfigManager::set(config_manager, "order_service", "payment.timeout", "30")
  ConfigManager::set(config_manager, "order_service", "payment.retry_count", "3")
  
  // Test configuration retrieval
  let db_host = ConfigManager::get(config_manager, "user_service", "database.host")
  let db_port = ConfigManager::get(config_manager, "user_service", "database.port")
  let cache_ttl = ConfigManager::get(config_manager, "user_service", "cache.ttl")
  
  assert_eq(db_host, Some("db.user-service.local"))
  assert_eq(db_port, Some("5432"))
  assert_eq(cache_ttl, Some("300"))
  
  // Test configuration watching
  let config_changes = Ref::new([])
  
  ConfigManager::watch(config_manager, "user_service", lambda { key, value =>
    config_changes.set(config_changes.get() + [(key, value)])
  })
  
  ConfigManager::set(config_manager, "user_service", "database.pool_size", "10")
  
  // Simulate async notification (simplified for test)
  Time::sleep(10)
  
  let changes = config_changes.get()
  assert_eq(changes.length(), 1)
  assert_eq(changes[0], ("database.pool_size", "10"))
  
  // Test configuration versioning
  ConfigManager::create_version(config_manager, "user_service", "v1.0")
  
  ConfigManager::set(config_manager, "user_service", "database.host", "db.user-service-v2.local")
  ConfigManager::create_version(config_manager, "user_service", "v1.1")
  
  let versions = ConfigManager::list_versions(config_manager, "user_service")
  assert_eq(versions.length(), 2)
  
  // Test configuration rollback
  ConfigManager::rollback(config_manager, "user_service", "v1.0")
  
  let rollback_db_host = ConfigManager::get(config_manager, "user_service", "database.host")
  assert_eq(rollback_db_host, Some("db.user-service.local"))
}

// Test 4: Distributed Lock Management
test "distributed lock management" {
  let lock_manager = DistributedLockManager::new("azimuth_locks")
  
  // Test lock acquisition
  let lock1 = LockManager::acquire(lock_manager, "resource_1", 30) // 30 seconds TTL
  assert_true(lock1.is_acquired)
  assert_eq(lock1.resource, "resource_1")
  
  // Test lock conflict
  let lock2 = LockManager::try_acquire(lock_manager, "resource_1", 30)
  assert_false(lock2.is_acquired)
  
  // Test lock release
  let release_result = LockManager::release(lock_manager, lock1)
  assert_true(release_result)
  
  // Test re-acquisition after release
  let lock3 = LockManager::try_acquire(lock_manager, "resource_1", 30)
  assert_true(lock3.is_acquired)
  
  // Test lock renewal
  let renewed_lock = LockManager::renew(lock_manager, lock3, 60) // Extend to 60 seconds
  assert_true(renewed_lock.is_acquired)
  assert_true(renewed_lock.expires_at > lock3.expires_at)
  
  // Test lock timeout
  let short_lock = LockManager::acquire(lock_manager, "resource_2", 1) // 1 second TTL
  
  // Wait for expiration
  Time::sleep(1100) // 1.1 seconds
  
  let expired_lock = LockManager::get_lock_status(lock_manager, "resource_2")
  assert_false(expired_lock.is_acquired)
  
  // Test lock acquisition after expiration
  let new_lock = LockManager::try_acquire(lock_manager, "resource_2", 30)
  assert_true(new_lock.is_acquired)
  
  // Test lock ownership verification
  let is_owner = LockManager::is_owner(lock_manager, new_lock, "resource_2")
  assert_true(is_owner)
  
  // Test lock metadata
  LockManager::set_metadata(lock_manager, new_lock, "owner", "service_1")
  LockManager::set_metadata(lock_manager, new_lock, "purpose", "batch_processing")
  
  let metadata = LockManager::get_metadata(lock_manager, new_lock)
  assert_eq(metadata.get("owner"), Some("service_1"))
  assert_eq(metadata.get("purpose"), Some("batch_processing"))
}

// Test 5: Distributed Event System
test "distributed event system" {
  let event_bus = DistributedEventBus::new("azimuth_events")
  
  // Test event subscription
  let user_events = Ref::new([])
  let order_events = Ref::new([])
  
  event_bus.subscribe("user.created", lambda { event =>
    user_events.set(user_events.get() + [event])
  })
  
  event_bus.subscribe("order.placed", lambda { event =>
    order_events.set(order_events.get() + [event])
  })
  
  // Test event publishing
  let user_event = Event::new("user.created", [
    ("user_id", "12345"),
    ("email", "user@example.com"),
    ("timestamp", Time::now().to_string())
  ])
  
  let order_event = Event::new("order.placed", [
    ("order_id", "order-123"),
    ("user_id", "12345"),
    ("amount", "99.99"),
    ("timestamp", Time::now().to_string())
  ])
  
  event_bus.publish(user_event)
  event_bus.publish(order_event)
  
  // Wait for event processing (simplified for test)
  Time::sleep(10)
  
  // Test event reception
  let received_user_events = user_events.get()
  let received_order_events = order_events.get()
  
  assert_eq(received_user_events.length(), 1)
  assert_eq(received_order_events.length(), 1)
  
  assert_eq(received_user_events[0].type, "user.created")
  assert_eq(received_order_events[0].type, "order.placed")
  
  // Test event filtering
  let filtered_events = event_bus.get_events_by_type("user.created")
  assert_eq(filtered_events.length(), 1)
  
  // Test event persistence
  event_bus.enable_persistence(true)
  event_bus.persist_event(user_event)
  event_bus.persist_event(order_event)
  
  let persisted_events = event_bus.get_persisted_events()
  assert_eq(persisted_events.length(), 2)
  
  // Test event replay
  let replay_events = Ref::new([])
  
  event_bus.replay_events(lambda { event =>
    replay_events.set(replay_events.get() + [event])
  })
  
  let replayed_events = replay_events.get()
  assert_eq(replayed_events.length(), 2)
  
  // Test event aggregation
  let event_aggregator = EventAggregator::new()
  
  event_aggregator.add_rule("user_count", "user.created", lambda { events => events.length().to_string() })
  event_aggregator.add_rule("order_total", "order.placed", lambda { events =>
    let total = events.map(lambda { event => event.data.get("amount").unwrap_or("0").parse_float().unwrap_or(0.0) }).sum()
    total.to_string()
  })
  
  event_aggregator.process_events([user_event, order_event])
  
  let user_count = event_aggregator.get_aggregated_value("user_count")
  let order_total = event_aggregator.get_aggregated_value("order_total")
  
  assert_eq(user_count, Some("1"))
  assert_eq(order_total, Some("99.99"))
}

// Test 6: Distributed Cache
test "distributed cache" {
  let cache = DistributedCache::new("azimuth_cache")
  
  // Test cache operations
  cache.set("user:12345", "{'name': 'John Doe', 'email': 'john@example.com'}", 3600) // 1 hour TTL
  cache.set("product:67890", "{'name': 'Product A', 'price': 29.99}", 1800) // 30 minutes TTL
  
  // Test cache retrieval
  let user_data = cache.get("user:12345")
  let product_data = cache.get("product:67890")
  let non_existent = cache.get("non_existent_key")
  
  assert_true(user_data.is_some())
  assert_true(product_data.is_some())
  assert_true(non_existent.is_none())
  
  // Test cache update
  cache.set("user:12345", "{'name': 'John Doe', 'email': 'john.doe@example.com'}", 3600)
  
  let updated_user_data = cache.get("user:12345")
  assert_eq(updated_user_data, Some("{'name': 'John Doe', 'email': 'john.doe@example.com'}"))
  
  // Test cache deletion
  cache.delete("product:67890")
  
  let deleted_product_data = cache.get("product:67890")
  assert_true(deleted_product_data.is_none())
  
  // Test cache expiration
  cache.set("temp_key", "temp_value", 1) // 1 second TTL
  
  Time::sleep(1100) // 1.1 seconds
  
  let expired_data = cache.get("temp_key")
  assert_true(expired_data.is_none())
  
  // Test cache statistics
  let stats = cache.get_statistics()
  assert_eq(stats.hits, 3) // user:12345 (twice) + product:67890
  assert_eq(stats.misses, 2) // non_existent_key + deleted_product_data + expired_data
  assert_eq(stats.sets, 4) // user:12345 (twice) + product:67890 + temp_key
  assert_eq(stats.deletes, 1) // product:67890
  
  // Test cache clustering
  let cache_node2 = DistributedCache::new("azimuth_cache_node2")
  let cache_node3 = DistributedCache::new("azimuth_cache_node3")
  
  cache.add_node_to_cluster(cache_node2)
  cache.add_node_to_cluster(cache_node3)
  
  // Test cache replication
  cache.set("replicated_key", "replicated_value", 3600)
  
  // Simulate replication (simplified for test)
  Time::sleep(10)
  
  let node2_value = cache_node2.get("replicated_key")
  let node3_value = cache_node3.get("replicated_key")
  
  assert_eq(node2_value, Some("replicated_value"))
  assert_eq(node3_value, Some("replicated_value"))
  
  // Test cache invalidation across cluster
  cache.invalidate("replicated_key")
  
  Time::sleep(10)
  
  let node2_invalidated = cache_node2.get("replicated_key")
  let node3_invalidated = cache_node3.get("replicated_key")
  
  assert_true(node2_invalidated.is_none())
  assert_true(node3_invalidated.is_none())
}

// Test 7: Distributed Task Queue
test "distributed task queue" {
  let task_queue = DistributedTaskQueue::new("azimuth_tasks")
  
  // Test task creation
  let email_task = Task::new(
    "send_email",
    ["user@example.com", "Welcome to our service!"],
    TaskPriority::Normal,
    1 // retry count
  )
  
  let report_task = Task::new(
    "generate_report",
    ["monthly", "2023-01"],
    TaskPriority::High,
    3 // retry count
  )
  
  // Test task enqueue
  let email_task_id = task_queue.enqueue(email_task)
  let report_task_id = task_queue.enqueue(report_task)
  
  assert_true(email_task_id.length() > 0)
  assert_true(report_task_id.length() > 0)
  
  // Test task dequeue
  let dequeued_task = task_queue.dequeue()
  assert_true(dequeued_task.is_some())
  
  match dequeued_task {
    Some(task) => {
      assert_eq(task.type, "generate_report") // High priority should be dequeued first
    }
    None => assert_true(false)
  }
  
  // Test task status tracking
  let task_status = task_queue.get_task_status(report_task_id)
  assert_eq(task_status, TaskStatus::Processing)
  
  // Test task completion
  task_queue.complete_task(report_task_id, TaskResult::Success("Report generated successfully"))
  
  let completed_status = task_queue.get_task_status(report_task_id)
  assert_eq(completed_status, TaskStatus::Completed)
  
  // Test task failure and retry
  let dequeued_email_task = task_queue.dequeue()
  assert_true(dequeued_email_task.is_some())
  
  match dequeued_email_task {
    Some(task) => {
      assert_eq(task.type, "send_email")
      assert_eq(task.task_id, email_task_id)
    }
    None => assert_true(false)
  }
  
  // Simulate task failure
  task_queue.fail_task(email_task_id, TaskResult::Error("SMTP server unavailable"))
  
  let failed_status = task_queue.get_task_status(email_task_id)
  assert_eq(failed_status, TaskStatus::Failed)
  
  // Test task retry
  let retry_result = task_queue.retry_task(email_task_id)
  assert_true(retry_result)
  
  let retry_status = task_queue.get_task_status(email_task_id)
  assert_eq(retry_status, TaskStatus::Queued)
  
  // Test task queue statistics
  let queue_stats = task_queue.get_statistics()
  assert_eq(queue_stats.total_tasks, 2)
  assert_eq(queue_stats.completed_tasks, 1)
  assert_eq(queue_stats.failed_tasks, 1)
  assert_eq(queue_stats.queued_tasks, 1)
  
  // Test task scheduling
  let scheduled_task = Task::new(
    "cleanup",
    ["temp_files"],
    TaskPriority::Low,
    0
  )
  
  let scheduled_time = Time::now() + 3600 // 1 hour from now
  let scheduled_task_id = task_queue.schedule(scheduled_task, scheduled_time)
  
  let scheduled_status = task_queue.get_task_status(scheduled_task_id)
  assert_eq(scheduled_status, TaskStatus::Scheduled)
  
  // Test task worker
  let worker = TaskWorker::new("worker_1")
  worker.register_handler("send_email", lambda { task =>
    // Simulate email sending
    Time::sleep(100) // 100ms
    TaskResult::Success("Email sent successfully")
  })
  
  worker.register_handler("generate_report", lambda { task =>
    // Simulate report generation
    Time::sleep(500) // 500ms
    TaskResult::Success("Report generated successfully")
  })
  
  // Start worker
  worker.start()
  
  // Wait for task processing
  Time::sleep(1000)
  
  // Stop worker
  worker.stop()
  
  // Check if tasks were processed
  let final_email_status = task_queue.get_task_status(email_task_id)
  assert_eq(final_email_status, TaskStatus::Completed)
}

// Test 8: Distributed Consensus
test "distributed consensus" {
  // Create a cluster of nodes
  let node1 = ConsensusNode::new("node1", ["node2", "node3"])
  let node2 = ConsensusNode::new("node2", ["node1", "node3"])
  let node3 = ConsensusNode::new("node3", ["node1", "node2"])
  
  // Form a cluster
  let cluster = ConsensusCluster::new([node1, node2, node3])
  
  // Test leader election
  let leader = Cluster::elect_leader(cluster)
  assert_true(leader.is_some())
  
  match leader {
    Some(node) => {
      assert_true(node.id == "node1" || node.id == "node2" || node.id == "node3")
      assert_true(Cluster::is_leader(cluster, node.id))
    }
    None => assert_true(false)
  }
  
  // Test log replication
  let log_entry = LogEntry::new(
    1, // term
    "SET key value",
    Time::now()
  )
  
  let replication_result = Cluster::replicate_log(cluster, log_entry)
  assert_true(replication_result.is_success)
  
  // Test commit
  let commit_result = Cluster::commit_log(cluster, log_entry.index)
  assert_true(commit_result.is_success)
  
  // Test state machine application
  let state_machine = StateMachine::new()
  StateMachine::apply(state_machine, "SET key value")
  
  let value = StateMachine::get(state_machine, "key")
  assert_eq(value, Some("value"))
  
  // Test consensus failure handling
  // Simulate network partition
  Cluster::simulate_partition(cluster, ["node3"])
  
  let partitioned_log = LogEntry::new(
    2, // term
    "SET key2 value2",
    Time::now()
  )
  
  let partitioned_result = Cluster::replicate_log(cluster, partitioned_log)
  assert_false(partitioned_result.is_success) // Should fail due to partition
  
  // Heal partition
  Cluster::heal_partition(cluster)
  
  // Test recovery after partition
  let recovery_result = Cluster::replicate_log(cluster, partitioned_log)
  assert_true(recovery_result.is_success)
  
  // Test snapshot
  let snapshot = StateMachine::create_snapshot(state_machine)
  assert_true(snapshot.data.length() > 0)
  
  // Test restore from snapshot
  let new_state_machine = StateMachine::new()
  StateMachine::restore_from_snapshot(new_state_machine, snapshot)
  
  let restored_value = StateMachine::get(new_state_machine, "key")
  assert_eq(restored_value, Some("value"))
}

// Test 9: Distributed Data Store
test "distributed data store" {
  let data_store = DistributedDataStore::new("azimuth_data")
  
  // Test data store configuration
  let config = DataStoreConfig::new()
    .replication_factor(3)
    .consistency_level(ConsistencyLevel::Quorum)
    .shard_count(4)
  
  data_store.configure(config)
  
  // Test key-value operations
  data_store.put("user:12345", "{'name': 'John', 'age': 30}")
  data_store.put("user:67890", "{'name': 'Jane', 'age': 25}")
  data_store.put("product:11111", "{'name': 'Product A', 'price': 29.99}")
  
  // Test data retrieval
  let user_data = data_store.get("user:12345")
  let product_data = data_store.get("product:11111")
  let non_existent = data_store.get("non_existent")
  
  assert_eq(user_data, Some("{'name': 'John', 'age': 30}"))
  assert_eq(product_data, Some("{'name': 'Product A', 'price': 29.99}"))
  assert_eq(non_existent, None)
  
  // Test data update
  data_store.put("user:12345", "{'name': 'John', 'age': 31}")
  
  let updated_user_data = data_store.get("user:12345")
  assert_eq(updated_user_data, Some("{'name': 'John', 'age': 31}"))
  
  // Test data deletion
  data_store.delete("user:67890")
  
  let deleted_user_data = data_store.get("user:67890")
  assert_eq(deleted_user_data, None)
  
  // Test range queries
  let range_results = data_store.scan("user:", "user:~") // Scan all keys starting with "user:"
  assert_eq(range_results.length(), 1) // Only user:12345 remains
  assert_eq(range_results[0].key, "user:12345")
  
  // Test transactions
  let transaction = data_store.begin_transaction()
  
  transaction.put("order:12345", "{'user_id': '12345', 'total': 99.99}")
  transaction.put("order:67890", "{'user_id': '12345', 'total': 49.99}")
  
  let transaction_result = data_store.commit_transaction(transaction)
  assert_true(transaction_result.is_success)
  
  let order_data = data_store.get("order:12345")
  assert_eq(order_data, Some("{'user_id': '12345', 'total': 99.99}"))
  
  // Test transaction rollback
  let rollback_transaction = data_store.begin_transaction()
  
  rollback_transaction.put("temp:12345", "temporary data")
  rollback_transaction.put("temp:67890", "more temporary data")
  
  data_store.rollback_transaction(rollback_transaction)
  
  let temp_data = data_store.get("temp:12345")
  assert_eq(temp_data, None)
  
  // Test data replication
  let replication_status = data_store.get_replication_status("user:12345")
  assert_eq(replication_status.successful_replicas, 3)
  assert_eq(replication_status.required_replicas, 3)
  
  // Test data consistency
  let consistency_check = data_store.check_consistency("user:12345")
  assert_true(consistency_check.is_consistent)
  
  // Test shard information
  let shard_info = data_store.get_shard_info("user:12345")
  assert_true(shard_info.shard_id >= 0 && shard_info.shard_id < 4)
  assert_eq(shard_info.replica_nodes.length(), 3)
}

// Test 10: Distributed Monitoring
test "distributed monitoring" {
  let monitoring_system = DistributedMonitoring::new("azimuth_monitoring")
  
  // Test service registration
  monitoring_system.register_service("user_service", "http://user-service:8080/metrics")
  monitoring_system.register_service("order_service", "http://order-service:8080/metrics")
  monitoring_system.register_service("payment_service", "http://payment-service:8080/metrics")
  
  // Test metric collection
  let user_metrics = monitoring_system.collect_metrics("user_service")
  let order_metrics = monitoring_system.collect_metrics("order_service")
  
  assert_true(user_metrics.length() > 0)
  assert_true(order_metrics.length() > 0)
  
  // Test metric aggregation across services
  let aggregated_metrics = monitoring_system.aggregate_metrics([
    ("cpu_usage", MetricAggregation::Average),
    ("memory_usage", MetricAggregation::Average),
    ("request_count", MetricAggregation::Sum),
    ("error_rate", MetricAggregation::Average)
  ])
  
  assert_eq(aggregated_metrics.length(), 4)
  
  // Test distributed alerting
  let alert_rule = AlertRule::new(
    "high_error_rate",
    "error_rate",
    AlertCondition::GreaterThan,
    5.0,
    AlertSeverity::Critical
  )
  
  monitoring_system.add_alert_rule(alert_rule)
  
  // Simulate high error rate
  monitoring_system.submit_metric("payment_service", "error_rate", 10.0)
  
  let alerts = monitoring_system.evaluate_alerts()
  assert_eq(alerts.length(), 1)
  assert_eq(alerts[0].rule_name, "high_error_rate")
  assert_eq(alerts[0].service_name, "payment_service")
  
  // Test distributed tracing
  let trace_data = monitoring_system.collect_traces()
  assert_true(trace_data.length() > 0)
  
  // Test trace analysis
  let trace_analysis = monitoring_system.analyze_traces(trace_data)
  assert_true(trace_analysis.average_duration > 0)
  assert_true(trace_analysis.error_rate >= 0.0)
  
  // Test service dependency mapping
  let dependencies = monitoring_system.map_service_dependencies()
  assert_true(dependencies.length() > 0)
  
  // Test performance baselining
  monitoring_system.establish_baseline("user_service", 7 * 24 * 3600) // 7 days
  
  let baseline = monitoring_system.get_baseline("user_service")
  assert_true(baseline.is_some())
  
  // Test anomaly detection
  let anomalies = monitoring_system.detect_anomalies()
  assert_true(anomalies.length() >= 0)
  
  // Test health checks
  let health_status = monitoring_system.check_service_health()
  assert_eq(health_status.length(), 3)
  
  // Test dashboard generation
  let dashboard_data = monitoring_system.generate_dashboard_data()
  assert_true(dashboard_data.services.length() > 0)
  assert_true(dashboard_data.metrics.length() > 0)
  assert_true(dashboard_data.alerts.length() >= 0)
  
  // Test monitoring data export
  let export_data = monitoring_system.export_monitoring_data(ExportFormat::JSON)
  assert_true(export_data.contains("\"services\""))
  assert_true(export_data.contains("\"metrics\""))
  
  // Test distributed monitoring cluster
  let monitoring_node2 = DistributedMonitoring::new("azimuth_monitoring_node2")
  let monitoring_node3 = DistributedMonitoring::new("azimuth_monitoring_node3")
  
  monitoring_system.add_monitoring_node(monitoring_node2)
  monitoring_system.add_monitoring_node(monitoring_node3)
  
  // Test monitoring data replication
  monitoring_system.replicate_data()
  
  // Test monitoring failover
  let failover_result = monitoring_system.test_failover()
  assert_true(failover_result.is_success)
}