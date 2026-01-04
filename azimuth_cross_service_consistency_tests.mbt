// Cross-Service Consistency Tests for Azimuth Telemetry System
// This file contains test cases for cross-service consistency and coordination

// Test 1: Distributed Transaction Consistency
test "distributed transaction consistency" {
  let transaction_coordinator = DistributedTransactionCoordinator::new()
  
  // Initialize transaction coordinator
  transaction_coordinator.initialize(CoordinatorConfig::new()
    .with_timeout(30000) // 30 seconds
    .with_retry_policy(RetryPolicy::exponential(3, 1000, 2.0))
    .with_isolation_level("serializable")
    .with_consistency_model("strong"))
  
  // Start transaction coordinator
  transaction_coordinator.start()
  
  // Define participating services
  let services = [
    Service::new("telemetry_service", "http://telemetry:8080"),
    Service::new("storage_service", "http://storage:8081"),
    Service::new("analytics_service", "http://analytics:8082"),
    Service::new("notification_service", "http://notification:8083")
  ]
  
  // Register services with coordinator
  for service in services {
    let registration_result = transaction_coordinator.register_service(service)
    assert_true(registration_result.success)
    assert_true(registration_result.service_id.length() > 0)
  }
  
  // Test two-phase commit transaction
  let transaction_operations = [
    TransactionOperation::new("telemetry_service", "create_telemetry_record", {"id": "tel_001", "data": "sample_data"}),
    TransactionOperation::new("storage_service", "store_telemetry_data", {"id": "tel_001", "path": "/data/tel_001"}),
    TransactionOperation::new("analytics_service", "process_telemetry", {"id": "tel_001", "type": "real_time"}),
    TransactionOperation::new("notification_service", "send_alert", {"id": "tel_001", "severity": "info"})
  ]
  
  // Begin transaction
  let transaction_result = transaction_coordinator.begin_transaction(transaction_operations)
  assert_true(transaction_result.success)
  assert_true(transaction_result.transaction_id.length() > 0)
  assert_eq(transaction_result.status, "prepared")
  
  // Commit transaction
  let commit_result = transaction_coordinator.commit_transaction(transaction_result.transaction_id)
  assert_true(commit_result.success)
  assert_eq(commit_result.status, "committed")
  assert_true(commit_result.commit_time_ms > 0)
  
  // Verify transaction consistency
  let verification_result = transaction_coordinator.verify_transaction_consistency(transaction_result.transaction_id)
  assert_true(verification_result.consistent)
  assert_eq(verification_result.verified_operations.length(), 4)
  
  for operation in verification_result.verified_operations {
    assert_true(operation.service_name.length() > 0)
    assert_true(operation.operation_name.length() > 0)
    assert_true(operation.success)
    assert_true(operation.response_time_ms > 0)
  }
  
  // Test transaction rollback
  let rollback_operations = [
    TransactionOperation::new("telemetry_service", "create_telemetry_record", {"id": "tel_002", "data": "sample_data"}),
    TransactionOperation::new("storage_service", "store_telemetry_data", {"id": "tel_002", "path": "/data/tel_002"}),
    TransactionOperation::new("analytics_service", "process_telemetry", {"id": "tel_002", "type": "real_time"}),
    TransactionOperation::new("notification_service", "send_alert", {"id": "tel_002", "severity": "info"})
  ]
  
  // Begin transaction
  let rollback_transaction_result = transaction_coordinator.begin_transaction(rollback_operations)
  assert_true(rollback_transaction_result.success)
  
  // Simulate failure in one service
  transaction_coordinator.simulate_service_failure("analytics_service")
  
  // Attempt to commit (should fail and rollback)
  let rollback_commit_result = transaction_coordinator.commit_transaction(rollback_transaction_result.transaction_id)
  assert_false(rollback_commit_result.success)
  assert_eq(rollback_commit_result.status, "rolled_back")
  assert_true(rollback_commit_result.rollback_time_ms > 0)
  
  // Verify rollback consistency
  let rollback_verification_result = transaction_coordinator.verify_transaction_consistency(rollback_transaction_result.transaction_id)
  assert_true(rollback_verification_result.consistent)
  assert_eq(rollback_verification_result.verified_operations.length(), 4)
  
  for operation in rollback_verification_result.verified_operations {
    if operation.service_name == "analytics_service" {
      assert_false(operation.success) // Should have failed
    } else {
      assert_true(operation.rolled_back) // Should have been rolled back
    }
  }
  
  // Test transaction isolation levels
  let isolation_levels = ["read_uncommitted", "read_committed", "repeatable_read", "serializable"]
  
  for isolation_level in isolation_levels {
    transaction_coordinator.set_isolation_level(isolation_level)
    
    let isolation_operations = [
      TransactionOperation::new("telemetry_service", "read_telemetry", {"id": "tel_003"}),
      TransactionOperation::new("storage_service", "read_storage", {"id": "tel_003"}),
      TransactionOperation::new("analytics_service", "read_analytics", {"id": "tel_003"})
    ]
    
    let isolation_transaction_result = transaction_coordinator.begin_transaction(isolation_operations)
    assert_true(isolation_transaction_result.success)
    
    let isolation_commit_result = transaction_coordinator.commit_transaction(isolation_transaction_result.transaction_id)
    assert_true(isolation_commit_result.success)
    
    // Verify isolation level behavior
    let isolation_verification = transaction_coordinator.verify_isolation_level(isolation_transaction_result.transaction_id)
    assert_true(isolation_verification.isolation_level_compliant)
  }
  
  // Stop transaction coordinator
  transaction_coordinator.stop()
}

// Test 2: Eventual Consistency Across Services
test "eventual consistency across services" {
  let consistency_manager = EventualConsistencyManager::new()
  
  // Initialize consistency manager
  consistency_manager.initialize(ConsistencyConfig::new()
    .with_convergence_timeout(60000) // 60 seconds
    .with_reconciliation_interval(10000) // 10 seconds
    .with_conflict_resolution_strategy("last_writer_wins")
    .with_propagation_delay(1000)) // 1 second
  
  // Start consistency manager
  consistency_manager.start()
  
  // Define service replicas
  let replicas = [
    Replica::new("telemetry_replica_1", "http://telemetry-1:8080", "primary"),
    Replica::new("telemetry_replica_2", "http://telemetry-2:8080", "secondary"),
    Replica::new("telemetry_replica_3", "http://telemetry-3:8080", "secondary"),
    Replica::new("telemetry_replica_4", "http://telemetry-4:8080", "secondary")
  ]
  
  // Register replicas
  for replica in replicas {
    let registration_result = consistency_manager.register_replica(replica)
    assert_true(registration_result.success)
    assert_true(registration_result.replica_id.length() > 0)
  }
  
  // Test write propagation
  let write_data = {
    "id": "tel_004",
    "timestamp": get_current_time_ms(),
    "data": "sample_telemetry_data",
    "version": 1
  }
  
  let write_result = consistency_manager.write_to_primary("telemetry_replica_1", write_data)
  assert_true(write_result.success)
  assert_true(write_result.write_time_ms > 0)
  
  // Wait for propagation
  let propagation_wait_time = 5000
  thread::sleep(propagation_wait_time)
  
  // Verify propagation to all replicas
  let propagation_results = []
  for replica in replicas {
    if replica.replica_id != "telemetry_replica_1" { // Skip primary
      let read_result = consistency_manager.read_from_replica(replica.replica_id, "tel_004")
      propagation_results.push(read_result)
    }
  }
  
  // Verify eventual consistency
  assert_eq(propagation_results.length(), 3)
  
  let consistent_replicas = propagation_results.filter(|r| r.success && r.data.get("id") == Some("tel_004")).length()
  assert_true(consistent_replicas >= 2) // At least 2 out of 3 replicas should be consistent
  
  // Test concurrent writes and conflict resolution
  let concurrent_writes = [
    {"replica": "telemetry_replica_2", "data": {"id": "tel_005", "value": "value_1", "version": 1}},
    {"replica": "telemetry_replica_3", "data": {"id": "tel_005", "value": "value_2", "version": 1}},
    {"replica": "telemetry_replica_4", "data": {"id": "tel_005", "value": "value_3", "version": 1}}
  ]
  
  let concurrent_write_results = []
  for write in concurrent_writes {
    let result = consistency_manager.write_to_replica(write.replica, write.data)
    concurrent_write_results.push(result)
  }
  
  // Wait for conflict resolution
  thread::sleep(10000)
  
  // Verify conflict resolution
  let conflict_resolution_result = consistency_manager.resolve_conflicts("tel_005")
  assert_true(conflict_resolution_result.conflicts_detected)
  assert_true(conflict_resolution_result.conflicts_resolved)
  assert_true(conflict_resolution_result.resolved_value.length() > 0)
  
  // Verify final consistency
  let final_consistency_results = []
  for replica in replicas {
    let read_result = consistency_manager.read_from_replica(replica.replica_id, "tel_005")
    final_consistency_results.push(read_result)
  }
  
  let final_consistent_replicas = final_consistency_results.filter(|r| r.success).length()
  assert_true(final_consistent_replicas >= 3) // At least 3 out of 4 replicas should be consistent
  
  // Test consistency monitoring
  let consistency_monitor_result = consistency_manager.monitor_consistency()
  assert_true(consistency_monitor_result.monitoring_active)
  assert_true(consistency_monitor_result.total_replicas == 4)
  assert_true(consistency_monitor_result.consistent_replicas >= 3)
  assert_true(consistency_monitor_result.inconsistent_replicas >= 0)
  assert_true(consistency_monitor_result.consistency_percentage >= 75.0)
  
  // Test anti-entropy mechanism
  let anti_entropy_result = consistency_manager.run_anti_entropy()
  assert_true(anti_entropy_result.success)
  assert_true(anti_entropy_result.inconsistencies_detected >= 0)
  assert_true(anti_entropy_result.inconsistencies_repaired >= 0)
  assert_true(anti_entropy_result.repair_time_ms > 0)
  
  // Test different consistency models
  let consistency_models = ["strong", "bounded", "causal", "eventual"]
  
  for model in consistency_models {
    consistency_manager.set_consistency_model(model)
    
    let model_test_data = {
      "id": "tel_" + model,
      "timestamp": get_current_time_ms(),
      "data": "test_data_for_" + model,
      "version": 1
    }
    
    let model_write_result = consistency_manager.write_with_consistency(model_test_data, model)
    assert_true(model_write_result.success)
    
    // Verify consistency model behavior
    thread::sleep(2000)
    
    let model_consistency_result = consistency_manager.verify_consistency_model("tel_" + model, model)
    assert_true(model_consistency_result.model_compliant)
  }
  
  // Stop consistency manager
  consistency_manager.stop()
}

// Test 3: Cross-Service Data Synchronization
test "cross-service data synchronization" {
  let sync_manager = CrossServiceSyncManager::new()
  
  // Initialize sync manager
  sync_manager.initialize(SyncConfig::new()
    .with_sync_interval(5000) // 5 seconds
    .with_batch_size(100)
    .with_sync_strategy("bidirectional")
    .with_conflict_resolution("merge")
    .with_compression_enabled(true))
  
  // Start sync manager
  sync_manager.start()
  
  // Define data sources
  let data_sources = [
    DataSource::new("telemetry_db", "postgresql", "postgresql://telemetry:password@telemetry-db:5432/telemetry"),
    DataSource::new("analytics_db", "mongodb", "mongodb://analytics:password@analytics-db:27017/analytics"),
    DataSource::new("cache_store", "redis", "redis://cache:6379"),
    DataSource::new("file_store", "filesystem", "/data/telemetry")
  ]
  
  // Register data sources
  for source in data_sources {
    let registration_result = sync_manager.register_data_source(source)
    assert_true(registration_result.success)
    assert_true(registration_result.source_id.length() > 0)
  }
  
  // Test one-way synchronization
  let one_way_sync_result = sync_manager.setup_one_way_sync("telemetry_db", "analytics_db")
  assert_true(one_way_sync_result.success)
  assert_true(one_way_sync_result.sync_id.length() > 0)
  assert_eq(one_way_sync_result.direction, "one_way")
  
  // Generate test data in source
  let test_data = generate_telemetry_data(50)
  for data in test_data {
    sync_manager.insert_data("telemetry_db", data)
  }
  
  // Trigger synchronization
  let sync_trigger_result = sync_manager.trigger_sync(one_way_sync_result.sync_id)
  assert_true(sync_trigger_result.success)
  assert_true(sync_trigger_result.sync_started)
  
  // Wait for synchronization to complete
  thread::sleep(10000)
  
  // Verify synchronization
  let sync_verification_result = sync_manager.verify_sync(one_way_sync_result.sync_id)
  assert_true(sync_verification_result.success)
  assert_true(sync_verification_result.records_synchronized > 0)
  assert_true(sync_verification_result.sync_time_ms > 0)
  
  // Test two-way synchronization
  let two_way_sync_result = sync_manager.setup_two_way_sync("telemetry_db", "analytics_db")
  assert_true(two_way_sync_result.success)
  assert_true(two_way_sync_result.sync_id.length() > 0)
  assert_eq(two_way_sync_result.direction, "two_way")
  
  // Generate conflicting data in both sources
  let telemetry_conflicts = generate_conflicting_telemetry_data(20)
  let analytics_conflicts = generate_conflicting_analytics_data(20)
  
  for data in telemetry_conflicts {
    sync_manager.insert_data("telemetry_db", data)
  }
  
  for data in analytics_conflicts {
    sync_manager.insert_data("analytics_db", data)
  }
  
  // Trigger two-way synchronization
  let two_way_trigger_result = sync_manager.trigger_sync(two_way_sync_result.sync_id)
  assert_true(two_way_trigger_result.success)
  
  // Wait for synchronization to complete
  thread::sleep(15000)
  
  // Verify two-way synchronization
  let two_way_verification_result = sync_manager.verify_sync(two_way_sync_result.sync_id)
  assert_true(two_way_verification_result.success)
  assert_true(two_way_verification_result.records_synchronized > 0)
  assert_true(two_way_verification_result.conflicts_detected > 0)
  assert_true(two_way_verification_result.conflicts_resolved > 0)
  
  // Test multi-source synchronization
  let multi_source_sync_result = sync_manager.setup_multi_source_sync(
    ["telemetry_db", "analytics_db", "cache_store"],
    "file_store"
  )
  assert_true(multi_source_sync_result.success)
  assert_true(multi_source_sync_result.sync_id.length() > 0)
  
  // Trigger multi-source synchronization
  let multi_source_trigger_result = sync_manager.trigger_sync(multi_source_sync_result.sync_id)
  assert_true(multi_source_trigger_result.success)
  
  // Wait for synchronization to complete
  thread::sleep(20000)
  
  // Verify multi-source synchronization
  let multi_source_verification_result = sync_manager.verify_sync(multi_source_sync_result.sync_id)
  assert_true(multi_source_verification_result.success)
  assert_true(multi_source_verification_result.records_synchronized > 0)
  assert_true(multi_source_verification_result.sources_synchronized == 3)
  
  // Test synchronization filtering
  let filter_config = SyncFilter::new()
    .with_table_filter("telemetry_data")
    .with_column_filter(["id", "timestamp", "value"])
    .with_time_range(get_current_time_ms() - 3600000, get_current_time_ms()) // Last hour
    .with_data_condition("value > 100")
  
  let filtered_sync_result = sync_manager.setup_filtered_sync("telemetry_db", "analytics_db", filter_config)
  assert_true(filtered_sync_result.success)
  assert_true(filtered_sync_result.filter_applied)
  
  // Trigger filtered synchronization
  let filtered_trigger_result = sync_manager.trigger_sync(filtered_sync_result.sync_id)
  assert_true(filtered_trigger_result.success)
  
  // Wait for synchronization to complete
  thread::sleep(10000)
  
  // Verify filtered synchronization
  let filtered_verification_result = sync_manager.verify_sync(filtered_sync_result.sync_id)
  assert_true(filtered_verification_result.success)
  assert_true(filtered_verification_result.records_filtered > 0)
  assert_true(filtered_verification_result.records_synchronized > 0)
  assert_true(filtered_verification_result.records_synchronized <= filtered_verification_result.records_filtered)
  
  // Test synchronization monitoring
  let sync_monitor_result = sync_manager.monitor_synchronization()
  assert_true(sync_monitor_result.monitoring_active)
  assert_true(sync_monitor_result.active_syncs > 0)
  assert_true(sync_monitor_result.total_syncs > 0)
  assert_true(sync_monitor_result.successful_syncs > 0)
  assert_true(sync_monitor_result.failed_syncs >= 0)
  assert_true(sync_monitor_result.average_sync_time_ms > 0)
  
  // Stop sync manager
  sync_manager.stop()
}

// Test 4: Distributed Cache Consistency
test "distributed cache consistency" {
  let cache_manager = DistributedCacheManager::new()
  
  // Initialize cache manager
  cache_manager.initialize(CacheConfig::new()
    .with_cache_nodes([
      "cache-1:6379",
      "cache-2:6379",
      "cache-3:6379",
      "cache-4:6379"
    ])
    .with_consistency_protocol("raft")
    .with_replication_factor(3)
    .with_eviction_policy("lru")
    .with_ttl(3600)) // 1 hour
  
  // Start cache manager
  cache_manager.start()
  
  // Test cache write operations
  let cache_data = [
    {"key": "tel_006", "value": {"data": "sample_1", "timestamp": get_current_time_ms()}},
    {"key": "tel_007", "value": {"data": "sample_2", "timestamp": get_current_time_ms()}},
    {"key": "tel_008", "value": {"data": "sample_3", "timestamp": get_current_time_ms()}}
  ]
  
  let write_results = []
  for data in cache_data {
    let result = cache_manager.write(data.key, data.value)
    write_results.push(result)
  }
  
  // Verify write operations
  assert_eq(write_results.length(), 3)
  
  for result in write_results {
    assert_true(result.success)
    assert_true(result.write_time_ms > 0)
    assert_true(result.replicated_nodes >= 2) // Should be replicated to at least 2 other nodes
  }
  
  // Test cache read operations
  let read_results = []
  for data in cache_data {
    let result = cache_manager.read(data.key)
    read_results.push(result)
  }
  
  // Verify read operations
  assert_eq(read_results.length(), 3)
  
  for result in read_results {
    assert_true(result.success)
    assert_true(result.value.length() > 0)
    assert_true(result.read_time_ms > 0)
    assert_true(result.cache_hit)
  }
  
  // Test cache consistency across nodes
  let consistency_results = []
  for data in cache_data {
    let result = cache_manager.verify_consistency(data.key)
    consistency_results.push(result)
  }
  
  // Verify consistency across nodes
  assert_eq(consistency_results.length(), 3)
  
  for result in consistency_results {
    assert_true(result.consistent)
    assert_eq(result.total_nodes, 4)
    assert_true(result.consistent_nodes >= 3) // At least 3 out of 4 nodes should be consistent
  }
  
  // Test cache invalidation
  let invalidation_result = cache_manager.invalidate("tel_006")
  assert_true(invalidation_result.success)
  assert_true(invalidation_result.invalidated_nodes >= 3)
  
  // Verify invalidation
  let invalidated_read_result = cache_manager.read("tel_006")
  assert_false(invalidated_read_result.success)
  assert_false(invalidated_read_result.cache_hit)
  
  // Test cache update with version control
  let update_data = {"key": "tel_007", "value": {"data": "updated_sample_2", "timestamp": get_current_time_ms()}, "version": 2}
  let update_result = cache_manager.update_with_version(update_data.key, update_data.value, update_data.version)
  assert_true(update_result.success)
  assert_true(update_result.version == 2)
  
  // Verify update
  let updated_read_result = cache_manager.read("tel_007")
  assert_true(updated_read_result.success)
  assert_true(updated_read_result.value.contains("updated_sample_2"))
  
  // Test concurrent access and race conditions
  let concurrent_access_data = {"key": "tel_009", "value": {"counter": 0}}
  let initial_write_result = cache_manager.write(concurrent_access_data.key, concurrent_access_data.value)
  assert_true(initial_write_result.success)
  
  // Simulate concurrent increments
  let concurrent_results = []
  for i in 1..=10 {
    let increment_result = cache_manager.increment_counter("tel_009", "counter")
    concurrent_results.push(increment_result)
  }
  
  // Verify concurrent operations
  let final_read_result = cache_manager.read("tel_009")
  assert_true(final_read_result.success)
  
  // Should have incremented 10 times
  let final_value = parse_int(final_read_result.value.get("counter"))
  assert_eq(final_value, 10)
  
  // Test cache node failure and recovery
  let node_failure_result = cache_manager.simulate_node_failure("cache-3:6379")
  assert_true(node_failure_result.success)
  assert_true(node_failure_result.node_failed)
  
  // Verify cluster still operational
  let cluster_status_result = cache_manager.get_cluster_status()
  assert_true(cluster_status_result.healthy_nodes == 3)
  assert_true(cluster_status_result.failed_nodes == 1)
  assert_true(cluster_status_result.cluster_operational)
  
  // Test cache recovery
  let node_recovery_result = cache_manager.recover_node("cache-3:6379")
  assert_true(node_recovery_result.success)
  assert_true(node_recovery_result.node_recovered)
  
  // Verify recovery
  let recovery_status_result = cache_manager.get_cluster_status()
  assert_true(recovery_status_result.healthy_nodes == 4)
  assert_true(recovery_status_result.failed_nodes == 0)
  assert_true(recovery_status_result.cluster_operational)
  
  // Test cache metrics
  let cache_metrics = cache_manager.get_cache_metrics()
  assert_true(cache_metrics.total_operations > 0)
  assert_true(cache_metrics.hit_ratio > 0.0)
  assert_true(cache_metrics.miss_ratio >= 0.0)
  assert_true(cache_metrics.average_read_time_ms > 0.0)
  assert_true(cache_metrics.average_write_time_ms > 0.0)
  assert_true(cache_metrics.total_memory_usage_mb > 0.0)
  assert_true(cache_metrics.eviction_count >= 0)
  
  // Stop cache manager
  cache_manager.stop()
}

// Test 5: Cross-Service Message Ordering
test "cross-service message ordering" {
  let message_ordering_manager = MessageOrderingManager::new()
  
  // Initialize message ordering manager
  message_ordering_manager.initialize(OrderingConfig::new()
    .with_ordering_strategy("total_order")
    .with_sequencer_service("sequencer:8084")
    .with_delivery_guarantee("exactly_once")
    .with_timeout(30000) // 30 seconds
    .with_retry_policy(RetryPolicy::exponential(3, 1000, 2.0)))
  
  // Start message ordering manager
  message_ordering_manager.start()
  
  // Define message producers and consumers
  let producers = [
    MessageProducer::new("producer_1", "http://producer-1:8085"),
    MessageProducer::new("producer_2", "http://producer-2:8085"),
    MessageProducer::new("producer_3", "http://producer-3:8085")
  ]
  
  let consumers = [
    MessageConsumer::new("consumer_1", "http://consumer-1:8086"),
    MessageConsumer::new("consumer_2", "http://consumer-2:8086"),
    MessageConsumer::new("consumer_3", "http://consumer-3:8086")
  ]
  
  // Register producers and consumers
  for producer in producers {
    let registration_result = message_ordering_manager.register_producer(producer)
    assert_true(registration_result.success)
  }
  
  for consumer in consumers {
    let registration_result = message_ordering_manager.register_consumer(consumer)
    assert_true(registration_result.success)
  }
  
  // Test total ordering
  let total_order_messages = []
  for i in 1..=20 {
    let message = Message::new()
      .with_id("msg_" + i.to_string())
      .with_payload({"sequence": i, "data": "sample_data_" + i.to_string()})
      .with_producer_id(producers[i % 3].producer_id)
      .with_timestamp(get_current_time_ms())
    
    total_order_messages.push(message)
  }
  
  // Send messages in random order
  let shuffled_messages = shuffle(total_order_messages)
  
  let send_results = []
  for message in shuffled_messages {
    let result = message_ordering_manager.send_message(message)
    send_results.push(result)
  }
  
  // Verify all messages were sent
  assert_eq(send_results.length(), 20)
  
  for result in send_results {
    assert_true(result.success)
    assert_true(result.sequence_number > 0)
    assert_true(result.assigned_timestamp > 0)
  }
  
  // Receive messages and verify ordering
  let received_messages = []
  let consumer_results = []
  
  for consumer in consumers {
    let result = message_ordering_manager.receive_messages(consumer.consumer_id, 10)
    consumer_results.push(result)
    
    for message in result.messages {
      received_messages.push(message)
    }
  }
  
  // Verify total ordering
  assert_eq(received_messages.length(), 20)
  
  for i in 1..=received_messages.length() - 1 {
    let current_message = received_messages[i]
    let previous_message = received_messages[i - 1]
    
    assert_true(current_message.sequence_number > previous_message.sequence_number)
    assert_true(current_message.assigned_timestamp >= previous_message.assigned_timestamp)
  }
  
  // Test FIFO ordering per producer
  let fifo_order_messages = []
  for producer in producers {
    for i in 1..=10 {
      let message = Message::new()
        .with_id(producer.producer_id + "_msg_" + i.to_string())
        .with_payload({"producer_sequence": i, "data": "fifo_data_" + i.to_string()})
        .with_producer_id(producer.producer_id)
        .with_fifo_group(producer.producer_id) // Group by producer
        .with_timestamp(get_current_time_ms())
      
      fifo_order_messages.push(message)
    }
  }
  
  // Send FIFO messages
  let fifo_send_results = []
  for message in fifo_order_messages {
    let result = message_ordering_manager.send_message(message)
    fifo_send_results.push(result)
  }
  
  // Receive FIFO messages and verify ordering per producer
  let fifo_received_messages = []
  
  for consumer in consumers {
    let result = message_ordering_manager.receive_messages(consumer.consumer_id, 20)
    
    for message in result.messages {
      fifo_received_messages.push(message)
    }
  }
  
  // Group messages by producer
  let messages_by_producer = group_by(fifo_received_messages, |m| m.producer_id)
  
  // Verify FIFO ordering per producer
  for (producer_id, messages) in messages_by_producer {
    for i in 1..=messages.length() - 1 {
      let current_message = messages[i]
      let previous_message = messages[i - 1]
      
      assert_true(current_message.sequence_number > previous_message.sequence_number)
      assert_eq(current_message.fifo_group, producer_id)
      assert_eq(previous_message.fifo_group, producer_id)
    }
  }
  
  // Test causal ordering
  let causal_messages = [
    Message::new().with_id("causal_1").with_payload({"operation": "create", "entity": "tel_010"}).with_causal_context([]),
    Message::new().with_id("causal_2").with_payload({"operation": "update", "entity": "tel_010"}).with_causal_context(["causal_1"]),
    Message::new().with_id("causal_3").with_payload({"operation": "delete", "entity": "tel_010"}).with_causal_context(["causal_2"])
  ]
  
  // Send causal messages
  let causal_send_results = []
  for message in causal_messages {
    let result = message_ordering_manager.send_message(message)
    causal_send_results.push(result)
  }
  
  // Receive causal messages and verify ordering
  let causal_received_messages = []
  
  for consumer in consumers {
    let result = message_ordering_manager.receive_messages(consumer.consumer_id, 10)
    
    for message in result.messages {
      if message.id.starts_with("causal_") {
        causal_received_messages.push(message)
      }
    }
  }
  
  // Verify causal ordering
  assert_eq(causal_received_messages.length(), 3)
  
  for i in 1..=causal_received_messages.length() - 1 {
    let current_message = causal_received_messages[i]
    let previous_message = causal_received_messages[i - 1]
    
    assert_true(current_message.causal_context.contains(previous_message.id))
  }
  
  // Test message ordering under failure conditions
  let failure_messages = []
  for i in 1..=10 {
    let message = Message::new()
      .with_id("failure_msg_" + i.to_string())
      .with_payload({"sequence": i, "data": "failure_data_" + i.to_string()})
      .with_producer_id(producers[0].producer_id)
      .with_timestamp(get_current_time_ms())
    
    failure_messages.push(message)
  }
  
  // Simulate sequencer failure
  message_ordering_manager.simulate_sequencer_failure()
  
  // Send messages during failure
  let failure_send_results = []
  for message in failure_messages {
    let result = message_ordering_manager.send_message(message)
    failure_send_results.push(result)
  }
  
  // Some messages should fail during sequencer failure
  let failed_sends = failure_send_results.filter(|r| !r.success).length()
  assert_true(failed_sends > 0)
  
  // Recover sequencer
  message_ordering_manager.recover_sequencer()
  
  // Retry failed messages
  let retry_results = []
  for (i, result) in failure_send_results.enumerate() {
    if !result.success {
      let retry_result = message_ordering_manager.retry_message(failure_messages[i])
      retry_results.push(retry_result)
    }
  }
  
  // Verify retry results
  let successful_retries = retry_results.filter(|r| r.success).length()
  assert_true(successful_retries > 0)
  
  // Test message ordering metrics
  let ordering_metrics = message_ordering_manager.get_ordering_metrics()
  assert_true(ordering_metrics.total_messages > 0)
  assert_true(ordering_metrics.successful_deliveries > 0)
  assert_true(ordering_metrics.failed_deliveries >= 0)
  assert_true(ordering_metrics.average_delivery_time_ms > 0.0)
  assert_true(ordering_metrics.out_of_order_deliveries == 0)
  assert_true(ordering_metrics.duplicate_deliveries == 0)
  
  // Stop message ordering manager
  message_ordering_manager.stop()
}

// Test 6: Cross-Service Lock Management
test "cross-service lock management" {
  let lock_manager = DistributedLockManager::new()
  
  // Initialize lock manager
  lock_manager.initialize(LockConfig::new()
    .with_lock_backend("redis")
    .with_lock_timeout(30000) // 30 seconds
    .with_heartbeat_interval(5000) // 5 seconds
    .with_retry_policy(RetryPolicy::exponential(3, 1000, 2.0))
    .with_auto_renewal_enabled(true))
  
  // Start lock manager
  lock_manager.start()
  
  // Test basic lock operations
  let lock_resources = ["resource_1", "resource_2", "resource_3"]
  
  let lock_results = []
  for resource in lock_resources {
    let result = lock_manager.acquire_lock(resource, "service_1", 10000) // 10 seconds
    lock_results.push(result)
  }
  
  // Verify lock acquisition
  assert_eq(lock_results.length(), 3)
  
  for result in lock_results {
    assert_true(result.success)
    assert_true(result.lock_id.length() > 0)
    assert_true(result.acquired_at > 0)
    assert_true(result.expires_at > result.acquired_at)
  }
  
  // Test lock conflict
  let conflict_result = lock_manager.acquire_lock("resource_1", "service_2", 10000)
  assert_false(conflict_result.success)
  assert_eq(conflict_result.reason, "resource_already_locked")
  assert_true(conflict_result.lock_holder == "service_1")
  
  // Test lock release
  let release_result = lock_manager.release_lock("resource_1", lock_results[0].lock_id)
  assert_true(release_result.success)
  assert_true(release_result.released_at > 0)
  
  // Test lock acquisition after release
  let reacquire_result = lock_manager.acquire_lock("resource_1", "service_2", 10000)
  assert_true(reacquire_result.success)
  assert_true(reacquire_result.lock_id != lock_results[0].lock_id)
  
  // Test lock renewal
  let renewal_result = lock_manager.renew_lock("resource_2", lock_results[1].lock_id, 20000) // Extend to 20 seconds
  assert_true(renewal_result.success)
  assert_true(renewal_result.new_expires_at > lock_results[1].expires_at)
  
  // Test lock timeout
  let timeout_result = lock_manager.acquire_lock("resource_4", "service_3", 1000) // 1 second timeout
  assert_true(timeout_result.success)
  
  // Wait for lock to expire
  thread::sleep(2000)
  
  // Try to acquire expired lock
  let expired_acquire_result = lock_manager.acquire_lock("resource_4", "service_4", 10000)
  assert_true(expired_acquire_result.success)
  
  // Test lock hierarchy
  let hierarchy_locks = [
    {"resource": "parent_resource", "service": "service_5", "timeout": 10000},
    {"resource": "parent_resource.child_1", "service": "service_5", "timeout": 10000},
    {"resource": "parent_resource.child_2", "service": "service_5", "timeout": 10000}
  ]
  
  let hierarchy_results = []
  for lock in hierarchy_locks {
    let result = lock_manager.acquire_lock(lock.resource, lock.service, lock.timeout)
    hierarchy_results.push(result)
  }
  
  // Verify hierarchical lock acquisition
  assert_eq(hierarchy_results.length(), 3)
  
  for result in hierarchy_results {
    assert_true(result.success)
  }
  
  // Test lock conflict with hierarchy
  let hierarchy_conflict_result = lock_manager.acquire_lock("parent_resource.child_1", "service_6", 10000)
  assert_false(hierarchy_conflict_result.success)
  assert_eq(hierarchy_conflict_result.reason, "resource_already_locked")
  
  // Test distributed lock across multiple services
  let services = ["service_7", "service_8", "service_9"]
  let distributed_lock_results = []
  
  for service in services {
    let result = lock_manager.acquire_lock("distributed_resource", service, 15000)
    distributed_lock_results.push(result)
  }
  
  // Only first service should acquire the lock
  let successful_distributed_locks = distributed_lock_results.filter(|r| r.success).length()
  assert_eq(successful_distributed_locks, 1)
  
  // Test lock monitoring
  let lock_monitor_result = lock_manager.monitor_locks()
  assert_true(lock_monitor_result.active_locks > 0)
  assert_true(lock_monitor_result.expired_locks >= 0)
  assert_true(lock_monitor_result.lock_contentions >= 0)
  
  // Test lock statistics
  let lock_stats = lock_manager.get_lock_statistics()
  assert_true(lock_stats.total_lock_requests > 0)
  assert_true(lock_stats.successful_lock_acquisitions > 0)
  assert_true(lock_stats.failed_lock_acquisitions >= 0)
  assert_true(lock_stats.average_acquisition_time_ms > 0.0)
  assert_true(lock_stats.lock_contentions >= 0)
  assert_true(lock_stats.lock_timeouts >= 0)
  
  // Test lock cleanup
  let cleanup_result = lock_manager.cleanup_expired_locks()
  assert_true(cleanup_result.success)
  assert_true(cleanup_result.cleaned_up_locks >= 0)
  
  // Test lock transfer
  let transfer_result = lock_manager.transfer_lock("resource_2", lock_results[1].lock_id, "service_10")
  assert_true(transfer_result.success)
  assert_true(transfer_result.transferred_at > 0)
  assert_eq(transfer_result.new_holder, "service_10")
  
  // Verify transfer
  let verify_transfer_result = lock_manager.get_lock_info("resource_2")
  assert_true(verify_transfer_result.success)
  assert_eq(verify_transfer_result.lock_holder, "service_10")
  
  // Stop lock manager
  lock_manager.stop()
}

// Test 7: Cross-Service State Replication
test "cross-service state replication" {
  let state_replication_manager = StateReplicationManager::new()
  
  // Initialize state replication manager
  state_replication_manager.initialize(ReplicationConfig::new()
    .with_replication_factor(3)
    .with_consistency_level("quorum")
    .with_replication_strategy("leader_followers")
    .with_election_timeout(5000) // 5 seconds
    .with_heartbeat_interval(1000)) // 1 second
  
  // Start state replication manager
  state_replication_manager.start()
  
  // Define state nodes
  let state_nodes = [
    StateNode::new("node_1", "http://node-1:8087", "leader"),
    StateNode::new("node_2", "http://node-2:8087", "follower"),
    StateNode::new("node_3", "http://node-3:8087", "follower"),
    StateNode::new("node_4", "http://node-4:8087", "follower")
  ]
  
  // Register state nodes
  for node in state_nodes {
    let registration_result = state_replication_manager.register_node(node)
    assert_true(registration_result.success)
    assert_true(registration_result.node_id.length() > 0)
  }
  
  // Test leader election
  let election_result = state_replication_manager.elect_leader()
  assert_true(election_result.success)
  assert_true(election_result.leader_id.length() > 0)
  assert_true(election_result.election_time_ms > 0)
  
  // Test state write operations
  let state_operations = [
    StateOperation::new("set", "config.telemetry.interval", 1000),
    StateOperation::new("set", "config.telemetry.batch_size", 100),
    StateOperation::new("set", "config.sampling.rate", 0.1),
    StateOperation::new("set", "config.storage.retention_days", 30)
  ]
  
  let write_results = []
  for operation in state_operations {
    let result = state_replication_manager.apply_state_operation(operation)
    write_results.push(result)
  }
  
  // Verify write operations
  assert_eq(write_results.length(), 4)
  
  for result in write_results {
    assert_true(result.success)
    assert_true(result.operation_id.length() > 0)
    assert_true(result.applied_at > 0)
    assert_true(result.replicated_nodes >= 2) // Should be replicated to at least 2 other nodes
  }
  
  // Test state read operations
  let read_operations = [
    StateOperation::new("get", "config.telemetry.interval", ""),
    StateOperation::new("get", "config.telemetry.batch_size", ""),
    StateOperation::new("get", "config.sampling.rate", ""),
    StateOperation::new("get", "config.storage.retention_days", "")
  ]
  
  let read_results = []
  for operation in read_operations {
    let result = state_replication_manager.apply_state_operation(operation)
    read_results.push(result)
  }
  
  // Verify read operations
  assert_eq(read_results.length(), 4)
  
  for result in read_results {
    assert_true(result.success)
    assert_true(result.value.length() > 0)
    assert_true(result.read_from_nodes >= 1)
  }
  
  // Test state consistency across nodes
  let consistency_result = state_replication_manager.verify_state_consistency()
  assert_true(consistency_result.consistent)
  assert_true(consistency_result.total_nodes == 4)
  assert_true(consistency_result.consistent_nodes >= 3) // At least 3 out of 4 nodes should be consistent
  
  // Test leader failure and re-election
  let leader_failure_result = state_replication_manager.simulate_leader_failure()
  assert_true(leader_failure_result.success)
  assert_true(leader_failure_result.leader_failed)
  
  // Wait for re-election
  thread::sleep(10000)
  
  // Verify new leader election
  let re_election_result = state_replication_manager.verify_leader_election()
  assert_true(re_election_result.new_leader_elected)
  assert_true(re_election_result.new_leader_id.length() > 0)
  assert_ne(re_election_result.new_leader_id, election_result.leader_id)
  
  // Test state replication after leader change
  let post_failure_operation = StateOperation::new("set", "config.new.setting", "test_value")
  let post_failure_result = state_replication_manager.apply_state_operation(post_failure_operation)
  assert_true(post_failure_result.success)
  
  // Verify replication after leader change
  let post_failure_consistency = state_replication_manager.verify_state_consistency()
  assert_true(post_failure_consistency.consistent)
  
  // Test node failure and recovery
  let node_failure_result = state_replication_manager.simulate_node_failure("node_3")
  assert_true(node_failure_result.success)
  assert_true(node_failure_result.node_failed)
  
  // Verify cluster still operational
  let cluster_status = state_replication_manager.get_cluster_status()
  assert_true(cluster_status.healthy_nodes == 3)
  assert_true(cluster_status.failed_nodes == 1)
  assert_true(cluster_status.cluster_operational)
  
  // Test node recovery
  let node_recovery_result = state_replication_manager.recover_node("node_3")
  assert_true(node_recovery_result.success)
  assert_true(node_recovery_result.node_recovered)
  
  // Verify recovery
  let recovery_status = state_replication_manager.get_cluster_status()
  assert_true(recovery_status.healthy_nodes == 4)
  assert_true(recovery_status.failed_nodes == 0)
  
  // Test state snapshots
  let snapshot_result = state_replication_manager.create_state_snapshot()
  assert_true(snapshot_result.success)
  assert_true(snapshot_result.snapshot_id.length() > 0)
  assert_true(snapshot_result.snapshot_size > 0)
  assert_true(snapshot_result.created_at > 0)
  
  // Test state restoration from snapshot
  let restoration_result = state_replication_manager.restore_from_snapshot(snapshot_result.snapshot_id)
  assert_true(restoration_result.success)
  assert_true(restoration_result.restored)
  assert_true(restoration_result.restoration_time_ms > 0)
  
  // Test state replication metrics
  let replication_metrics = state_replication_manager.get_replication_metrics()
  assert_true(replication_metrics.total_operations > 0)
  assert_true(replication_metrics.successful_replications > 0)
  assert_true(replication_metrics.failed_replications >= 0)
  assert_true(replication_metrics.average_replication_time_ms > 0.0)
  assert_true(replication_metrics.leader_elections > 0)
  assert_true(replication_metrics.node_failures > 0)
  assert_true(replication_metrics.node_recoveries > 0)
  
  // Stop state replication manager
  state_replication_manager.stop()
}

// Test 8: Cross-Service Coordination Patterns
test "cross-service coordination patterns" {
  let coordination_manager = CrossServiceCoordinationManager::new()
  
  // Initialize coordination manager
  coordination_manager.initialize(CoordinationConfig::new()
    .with_coordination_backend("zookeeper")
    .with_session_timeout(30000) // 30 seconds
    .with_connection_timeout(5000) // 5 seconds
    .with_retry_policy(RetryPolicy::exponential(3, 1000, 2.0)))
  
  // Start coordination manager
  coordination_manager.start()
  
  // Test leader election pattern
  let election_participants = [
    ElectionParticipant::new("service_1", "http://service-1:8088"),
    ElectionParticipant::new("service_2", "http://service-2:8088"),
    ElectionParticipant::new("service_3", "http://service-3:8088")
  ]
  
  let election_results = []
  for participant in election_participants {
    let result = coordination_manager.participate_in_election("leader_election", participant)
    election_results.push(result)
  }
  
  // Verify election results
  assert_eq(election_results.length(), 3)
  
  let elected_leaders = election_results.filter(|r| r.elected).length()
  assert_eq(elected_leaders, 1) // Only one leader should be elected
  
  let leader_result = election_results.find(|r| r.elected).unwrap()
  assert_true(leader_result.leader_id.length() > 0)
  assert_true(leader_result.election_time_ms > 0)
  
  // Test barrier synchronization pattern
  let barrier_participants = [
    BarrierParticipant::new("service_1", "http://service-1:8088"),
    BarrierParticipant::new("service_2", "http://service-2:8088"),
    BarrierParticipant::new("service_3", "http://service-3:8088")
  ]
  
  let barrier_result = coordination_manager.create_barrier("sync_barrier", 3) // Wait for 3 participants
  assert_true(barrier_result.success)
  assert_true(barrier_result.barrier_id.length() > 0)
  
  // Participants enter barrier
  let barrier_entry_results = []
  for participant in barrier_participants {
    let result = coordination_manager.enter_barrier(barrier_result.barrier_id, participant.participant_id)
    barrier_entry_results.push(result)
  }
  
  // Verify barrier synchronization
  assert_eq(barrier_entry_results.length(), 3)
  
  for result in barrier_entry_results {
    assert_true(result.success)
    assert_true(result.barrier_released)
  }
  
  // Test distributed semaphore pattern
  let semaphore_result = coordination_manager.create_semaphore("resource_semaphore", 2) // 2 permits
  assert_true(semaphore_result.success)
  assert_true(semaphore_result.semaphore_id.length() > 0)
  
  // Acquire semaphore permits
  let semaphore_acquire_results = []
  for i in 1..=3 {
    let result = coordination_manager.acquire_semaphore("resource_semaphore", "service_" + i.to_string())
    semaphore_acquire_results.push(result)
  }
  
  // Verify semaphore acquisition
  assert_eq(semaphore_acquire_results.length(), 3)
  
  let successful_acquisitions = semaphore_acquire_results.filter(|r| r.success).length()
  assert_eq(successful_acquisitions, 2) // Only 2 permits available
  
  let failed_acquisition = semaphore_acquire_results.find(|r| !r.success).unwrap()
  assert_eq(failed_acquisition.reason, "no_permits_available")
  
  // Release semaphore permit
  let release_result = coordination_manager.release_semaphore("resource_semaphore", semaphore_acquire_results[0].permit_id)
  assert_true(release_result.success)
  
  // Try to acquire again after release
  let retry_acquire_result = coordination_manager.acquire_semaphore("resource_semaphore", "service_4")
  assert_true(retry_acquire_result.success)
  
  // Test distributed queue pattern
  let queue_result = coordination_manager.create_distributed_queue("task_queue")
  assert_true(queue_result.success)
  assert_true(queue_result.queue_id.length() > 0)
  
  // Enqueue tasks
  let tasks = [
    {"id": "task_1", "type": "processing", "data": "sample_data_1"},
    {"id": "task_2", "type": "processing", "data": "sample_data_2"},
    {"id": "task_3", "type": "processing", "data": "sample_data_3"}
  ]
  
  let enqueue_results = []
  for task in tasks {
    let result = coordination_manager.enqueue_task("task_queue", task)
    enqueue_results.push(result)
  }
  
  // Verify task enqueue
  assert_eq(enqueue_results.length(), 3)
  
  for result in enqueue_results {
    assert_true(result.success)
    assert_true(result.task_position > 0)
  }
  
  // Dequeue tasks
  let dequeue_results = []
  for i in 1..=3 {
    let result = coordination_manager.dequeue_task("task_queue", "worker_" + i.to_string())
    dequeue_results.push(result)
  }
  
  // Verify task dequeue
  assert_eq(dequeue_results.length(), 3)
  
  for result in dequeue_results {
    assert_true(result.success)
    assert_true(result.task.id.length() > 0)
    assert_true(result.assigned_to.length() > 0)
  }
  
  // Test distributed map pattern
  let map_result = coordination_manager.create_distributed_map("config_map")
  assert_true(map_result.success)
  assert_true(map_result.map_id.length() > 0)
  
  // Put key-value pairs
  let map_entries = [
    {"key": "telemetry.interval", "value": 1000},
    {"key": "telemetry.batch_size", "value": 100},
    {"key": "sampling.rate", "value": 0.1}
  ]
  
  let put_results = []
  for entry in map_entries {
    let result = coordination_manager.put_map_entry("config_map", entry.key, entry.value)
    put_results.push(result)
  }
  
  // Verify map put operations
  assert_eq(put_results.length(), 3)
  
  for result in put_results {
    assert_true(result.success)
    assert_true(result.version > 0)
  }
  
  // Get key-value pairs
  let get_results = []
  for entry in map_entries {
    let result = coordination_manager.get_map_entry("config_map", entry.key)
    get_results.push(result)
  }
  
  // Verify map get operations
  assert_eq(get_results.length(), 3)
  
  for (i, result) in get_results.enumerate() {
    assert_true(result.success)
    assert_eq(result.value, map_entries[i].value)
    assert_true(result.version > 0)
  }
  
  // Test notification pattern
  let notification_result = coordination_manager.create_notification_channel("events")
  assert_true(notification_result.success)
  assert_true(notification_result.channel_id.length() > 0)
  
  // Subscribe to notifications
  let subscribers = ["service_1", "service_2", "service_3"]
  let subscribe_results = []
  
  for subscriber in subscribers {
    let result = coordination_manager.subscribe_to_channel("events", subscriber)
    subscribe_results.push(result)
  }
  
  // Verify subscription
  assert_eq(subscribe_results.length(), 3)
  
  for result in subscribe_results {
    assert_true(result.success)
    assert_true(result.subscription_id.length() > 0)
  }
  
  // Publish notifications
  let notifications = [
    {"type": "config_update", "data": {"key": "telemetry.interval", "value": 2000}},
    {"type": "config_update", "data": {"key": "sampling.rate", "value": 0.2}}
  ]
  
  let publish_results = []
  for notification in notifications {
    let result = coordination_manager.publish_notification("events", notification)
    publish_results.push(result)
  }
  
  // Verify notification publishing
  assert_eq(publish_results.length(), 2)
  
  for result in publish_results {
    assert_true(result.success)
    assert_true(result.delivered_to >= 2) // At least 2 subscribers should receive
  }
  
  // Test coordination metrics
  let coordination_metrics = coordination_manager.get_coordination_metrics()
  assert_true(coordination_metrics.total_operations > 0)
  assert_true(coordination_metrics.successful_operations > 0)
  assert_true(coordination_metrics.failed_operations >= 0)
  assert_true(coordination_metrics.average_operation_time_ms > 0.0)
  assert_true(coordination_metrics.active_elections >= 0)
  assert_true(coordination_metrics.active_barriers >= 0)
  assert_true(coordination_metrics.active_semaphores >= 0)
  
  // Stop coordination manager
  coordination_manager.stop()
}

// Test 9: Cross-Service Consistency Validation
test "cross-service consistency validation" {
  let consistency_validator = CrossServiceConsistencyValidator::new()
  
  // Initialize consistency validator
  consistency_validator.initialize(ValidationConfig::new()
    .with_validation_interval(30000) // 30 seconds
    .with_consistency_checks(["data", "schema", "semantic"])
    .with_thresholds(ValidationThresholds::new()
      .with_data_consistency_threshold(0.95)
      .with_schema_consistency_threshold(1.0)
      .with_semantic_consistency_threshold(0.9))
    .with_auto_repair_enabled(true))
  
  // Start consistency validator
  consistency_validator.start()
  
  // Define services to validate
  let services = [
    Service::new("telemetry_service", "http://telemetry:8080"),
    Service::new("storage_service", "http://storage:8081"),
    Service::new("analytics_service", "http://analytics:8082")
  ]
  
  // Register services for validation
  for service in services {
    let registration_result = consistency_validator.register_service(service)
    assert_true(registration_result.success)
  }
  
  // Test data consistency validation
  let data_consistency_result = consistency_validator.validate_data_consistency()
  assert_true(data_consistency_result.validation_completed)
  assert_true(data_consistency_result.overall_consistency_score >= 0.95)
  
  // Verify detailed consistency results
  for service_result in data_consistency_result.service_results {
    assert_true(service_result.service_name.length() > 0)
    assert_true(service_result.consistency_score >= 0.0)
    assert_true(service_result.inconsistencies_found >= 0)
    assert_true(service_result.validation_time_ms > 0)
  }
  
  // Test schema consistency validation
  let schema_consistency_result = consistency_validator.validate_schema_consistency()
  assert_true(schema_consistency_result.validation_completed)
  assert_true(schema_consistency_result.overall_consistency_score == 1.0) // Schema should be fully consistent
  
  // Verify schema consistency details
  for schema_result in schema_consistency_result.schema_results {
    assert_true(schema_result.schema_name.length() > 0)
    assert_true(schema_result.consistent)
    assert_true(schema_result.version_consistent)
    assert_true(schema_result.structure_consistent)
  }
  
  // Test semantic consistency validation
  let semantic_consistency_result = consistency_validator.validate_semantic_consistency()
  assert_true(semantic_consistency_result.validation_completed)
  assert_true(semantic_consistency_result.overall_consistency_score >= 0.9)
  
  // Verify semantic consistency details
  for semantic_result in semantic_consistency_result.semantic_results {
    assert_true(semantic_result.entity_type.length() > 0)
    assert_true(semantic_result.consistency_score >= 0.0)
    assert_true(semantic_result.semantic_violations >= 0)
  }
  
  // Test cross-service transaction consistency
  let transaction_consistency_result = consistency_validator.validate_transaction_consistency()
  assert_true(transaction_consistency_result.validation_completed)
  assert_true(transaction_consistency_result.consistent_transactions >= 0)
  assert_true(transaction_consistency_result.inconsistent_transactions >= 0)
  
  // Test temporal consistency validation
  let temporal_consistency_result = consistency_validator.validate_temporal_consistency()
  assert_true(temporal_consistency_result.validation_completed)
  assert_true(temporal_consistency_result.time_drift_ms < 1000) // Should be less than 1 second
  
  // Test consistency repair
  let repair_result = consistency_validator.repair_inconsistencies()
  assert_true(repair_result.success)
  assert_true(repair_result.inconsistencies_repaired >= 0)
  assert_true(repair_result.repair_time_ms > 0)
  
  // Verify repair effectiveness
  let post_repair_validation = consistency_validator.validate_data_consistency()
  assert_true(post_repair_validation.overall_consistency_score >= data_consistency_result.overall_consistency_score)
  
  // Test consistency monitoring
  let monitoring_result = consistency_validator.monitor_consistency_trends(24) // Last 24 hours
  assert_true(monitoring_result.monitoring_period_hours == 24)
  assert_true(monitoring_result.data_points > 0)
  assert_true(monitoring_result.average_consistency_score >= 0.0)
  assert_true(monitoring_result.consistency_trend == "improving" || monitoring_result.consistency_trend == "stable" || monitoring_result.consistency_trend == "degrading")
  
  // Test consistency alerts
  let alert_config = ConsistencyAlertConfig::new()
    .with_data_consistency_threshold(0.9)
    .with_schema_consistency_threshold(0.95)
    .with_semantic_consistency_threshold(0.85)
    .with_notification_channels(["email", "slack"])
  
  let alert_result = consistency_validator.configure_consistency_alerts(alert_config)
  assert_true(alert_result.success)
  assert_true(alert_result.alerts_configured)
  
  // Simulate consistency degradation
  consistency_validator.simulate_consistency_degradation(0.8) // Drop to 80%
  
  // Check for alerts
  let alert_check_result = consistency_validator.check_consistency_alerts()
  assert_true(alert_check_result.alerts_triggered)
  assert_true(alert_check_result.alert_count > 0)
  
  // Verify alert details
  for alert in alert_check_result.alerts {
    assert_true(alert.alert_id.length() > 0)
    assert_true(alert.alert_type.length() > 0)
    assert_true(alert.severity == "warning" || alert.severity == "critical")
    assert_true(alert.current_value < alert.threshold_value)
    assert_true(alert.message.length() > 0)
    assert_true(alert.timestamp > 0)
  }
  
  // Test consistency reporting
  let report_result = consistency_validator.generate_consistency_report()
  assert_true(report_result.success)
  assert_true(report_result.report_content.length() > 0)
  assert_true(report_result.report_format == "json" || report_result.report_format == "pdf")
  assert_true(report_result.generated_at > 0)
  
  // Test consistency validation metrics
  let validation_metrics = consistency_validator.get_validation_metrics()
  assert_true(validation_metrics.total_validations > 0)
  assert_true(validation_metrics.successful_validations > 0)
  assert_true(validation_metrics.failed_validations >= 0)
  assert_true(validation_metrics.average_validation_time_ms > 0.0)
  assert_true(validation_metrics.inconsistencies_detected >= 0)
  assert_true(validation_metrics.inconsistencies_repaired >= 0)
  assert_true(validation_metrics.alerts_triggered >= 0)
  
  // Stop consistency validator
  consistency_validator.stop()
}