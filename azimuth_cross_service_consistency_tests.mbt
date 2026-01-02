// Azimuth Cross-Service Consistency Tests
// This file contains comprehensive test cases for cross-service consistency

// Test 1: Distributed Transaction Consistency
test "distributed transaction consistency across services" {
  // Create distributed transaction coordinator
  let tx_coordinator = azimuth::DistributedTransactionCoordinator::new()
  
  // Register services for distributed transactions
  let services = ["user-service", "order-service", "inventory-service", "payment-service"]
  
  for service in services {
    azimuth::DistributedTransactionCoordinator::register_service(tx_coordinator, service)
  }
  
  // Configure transaction settings
  azimuth::DistributedTransactionCoordinator::configure(tx_coordinator, {
    "isolation_level": "serializable",
    "timeout_ms": 30000,
    "retry_attempts": 3,
    "compensation_enabled": true
  })
  
  // Test successful distributed transaction
  let tx_id = azimuth::DistributedTransactionCoordinator::begin_transaction(tx_coordinator)
  
  // Step 1: Validate user in user-service
  let user_validation = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    tx_id, 
    "user-service", 
    "validate_user", 
    {"user_id": "user-123"}
  )
  
  assert_true(user_validation.success)
  
  // Step 2: Create order in order-service
  let order_creation = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    tx_id, 
    "order-service", 
    "create_order", 
    {
      "user_id": "user-123",
      "items": [{"product_id": "prod-1", "quantity": 2}],
      "total": 199.99
    }
  )
  
  assert_true(order_creation.success)
  let order_id = order_creation.result["order_id"]
  
  // Step 3: Reserve inventory in inventory-service
  let inventory_reservation = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    tx_id, 
    "inventory-service", 
    "reserve_inventory", 
    {
      "product_id": "prod-1",
      "quantity": 2
    }
  )
  
  assert_true(inventory_reservation.success)
  
  // Step 4: Process payment in payment-service
  let payment_processing = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    tx_id, 
    "payment-service", 
    "process_payment", 
    {
      "order_id": order_id,
      "amount": 199.99,
      "payment_method": "credit_card"
    }
  )
  
  assert_true(payment_processing.success)
  
  // Commit transaction
  let commit_result = azimuth::DistributedTransactionCoordinator::commit_transaction(tx_coordinator, tx_id)
  assert_true(commit_result.success)
  
  // Verify all services have committed state
  let user_state = azimuth::DistributedTransactionCoordinator::get_service_state(
    tx_coordinator, 
    "user-service", 
    tx_id
  )
  assert_eq(user_state.status, "committed")
  
  let order_state = azimuth::DistributedTransactionCoordinator::get_service_state(
    tx_coordinator, 
    "order-service", 
    tx_id
  )
  assert_eq(order_state.status, "committed")
  
  let inventory_state = azimuth::DistributedTransactionCoordinator::get_service_state(
    tx_coordinator, 
    "inventory-service", 
    tx_id
  )
  assert_eq(inventory_state.status, "committed")
  
  let payment_state = azimuth::DistributedTransactionCoordinator::get_service_state(
    tx_coordinator, 
    "payment-service", 
    tx_id
  )
  assert_eq(payment_state.status, "committed")
  
  // Test transaction rollback on failure
  let rollback_tx_id = azimuth::DistributedTransactionCoordinator::begin_transaction(tx_coordinator)
  
  // Execute successful steps
  let rollback_user_validation = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    rollback_tx_id, 
    "user-service", 
    "validate_user", 
    {"user_id": "user-456"}
  )
  
  assert_true(rollback_user_validation.success)
  
  let rollback_order_creation = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    rollback_tx_id, 
    "order-service", 
    "create_order", 
    {
      "user_id": "user-456",
      "items": [{"product_id": "prod-2", "quantity": 1}],
      "total": 299.99
    }
  )
  
  assert_true(rollback_order_creation.success)
  
  // Simulate payment failure
  let payment_failure = azimuth::DistributedTransactionCoordinator::execute_step(
    tx_coordinator, 
    rollback_tx_id, 
    "payment-service", 
    "process_payment", 
    {
      "order_id": "order-failed",
      "amount": 299.99,
      "payment_method": "invalid_card"
    }
  )
  
  assert_false(payment_failure.success)
  
  // Rollback should be automatic
  let rollback_result = azimuth::DistributedTransactionCoordinator::rollback_transaction(tx_coordinator, rollback_tx_id)
  assert_true(rollback_result.success)
  
  // Verify all services have rolled back
  let rollback_user_state = azimuth::DistributedTransactionCoordinator::get_service_state(
    tx_coordinator, 
    "user-service", 
    rollback_tx_id
  )
  assert_eq(rollback_user_state.status, "rolled_back")
  
  let rollback_order_state = azimuth::DistributedTransactionCoordinator::get_service_state(
    tx_coordinator, 
    "order-service", 
    rollback_tx_id
  )
  assert_eq(rollback_order_state.status, "rolled_back")
  
  // Test transaction consistency verification
  let consistency_check = azimuth::DistributedTransactionCoordinator::verify_consistency(tx_coordinator)
  assert_true(consistency_check.consistent)
  assert_eq(consistency_check.inconsistent_transactions.length(), 0)
}

// Test 2: Eventual Consistency Across Services
test "eventual consistency across microservices" {
  // Create eventual consistency manager
  let consistency_manager = azimuth::EventualConsistencyManager::new()
  
  // Configure consistency settings
  azimuth::EventualConsistencyManager::configure(consistency_manager, {
    "consistency_window_ms": 5000,
    "reconciliation_interval_ms": 10000,
    "conflict_resolution": "last_write_wins",
    "propagation_strategy": "async"
  })
  
  // Register services
  let services = [
    ("profile-service", "user-profiles"),
    ("order-service", "orders"),
    ("notification-service", "notifications"),
    ("analytics-service", "analytics")
  ]
  
  for (service, data_store) in services {
    azimuth::EventualConsistencyManager::register_service(consistency_manager, service, data_store)
  }
  
  // Test user profile update propagation
  let user_id = "user-consistency-test"
  let base_timestamp = azimuth::Time::now()
  
  // Update user profile in profile-service
  let profile_update = {
    "user_id": user_id,
    "name": "John Doe",
    "email": "john.doe@example.com",
    "preferences": {
      "notifications": "email",
      "theme": "dark"
    },
    "updated_at": base_timestamp
  }
  
  let profile_result = azimuth::EventualConsistencyManager::update_data(
    consistency_manager, 
    "profile-service", 
    "user-profiles", 
    user_id, 
    profile_update
  )
  
  assert_true(profile_result.success)
  
  // Verify update was propagated to other services
  let propagation_results = []
  
  // Check order-service
  let order_profile = azimuth::EventualConsistencyManager::get_data(
    consistency_manager, 
    "order-service", 
    "orders", 
    user_id
  )
  
  // Check notification-service
  let notification_profile = azimuth::EventualConsistencyManager::get_data(
    consistency_manager, 
    "notification-service", 
    "notifications", 
    user_id
  )
  
  // Check analytics-service
  let analytics_profile = azimuth::EventualConsistencyManager::get_data(
    consistency_manager, 
    "analytics-service", 
    "analytics", 
    user_id
  )
  
  // Wait for propagation
  azimuth::Thread::sleep(2000) // 2 seconds
  
  // Verify eventual consistency
  let consistency_check = azimuth::EventualConsistencyManager::verify_cross_service_consistency(
    consistency_manager, 
    user_id
  )
  
  assert_true(consistency_check.eventually_consistent)
  assert_true(consistency_check.consistent_services.contains("profile-service"))
  assert_true(consistency_check.consistent_services.contains("order-service"))
  assert_true(consistency_check.consistent_services.contains("notification-service"))
  assert_true(consistency_check.consistent_services.contains("analytics-service"))
  
  // Test conflict detection and resolution
  let conflict_timestamp = base_timestamp + 1000
  
  // Simulate concurrent update to user profile
  let conflicting_update = {
    "user_id": user_id,
    "name": "Jonathan Doe", // Different name
    "email": "john.doe@example.com",
    "preferences": {
      "notifications": "sms", // Different preference
      "theme": "light"
    },
    "updated_at": conflict_timestamp
  }
  
  // Update in analytics-service directly (simulating concurrent update)
  let conflict_result = azimuth::EventualConsistencyManager::update_data(
    consistency_manager, 
    "analytics-service", 
    "analytics", 
    user_id, 
    conflicting_update
  )
  
  assert_true(conflict_result.success)
  
  // Detect conflicts
  let conflict_detection = azimuth::EventualConsistencyManager::detect_conflicts(
    consistency_manager, 
    user_id
  )
  
  assert_true(conflict_detection.conflicts_detected)
  assert_true(conflict_detection.conflicts.length() > 0)
  
  // Resolve conflicts using last-write-wins
  let resolution_result = azimuth::EventualConsistencyManager::resolve_conflicts(
    consistency_manager, 
    user_id, 
    "last_write_wins"
  )
  
  assert_true(resolution_result.success)
  assert_true(resolution_result.resolved_conflicts > 0)
  
  // Verify resolution
  let post_resolution_check = azimuth::EventualConsistencyManager::verify_cross_service_consistency(
    consistency_manager, 
    user_id
  )
  
  assert_true(post_resolution_check.eventually_consistent)
  assert_eq(post_resolution_check.conflicts.length(), 0)
  
  // Test consistency reconciliation
  let reconciliation_result = azimuth::EventualConsistencyManager::reconcile_data(
    consistency_manager, 
    {
      "reconciliation_scope": "all",
      "conflict_resolution": "manual_review",
      "create_reconciliation_report": true
    }
  )
  
  assert_true(reconciliation_result.success)
  assert_true(reconciliation_result.reconciled_items > 0)
  assert_true(reconciliation_result.report_generated)
  
  // Get reconciliation report
  let report = azimuth::EventualConsistencyManager::get_reconciliation_report(
    consistency_manager, 
    reconciliation_result.report_id
  )
  
  assert_true(report.total_items_checked > 0)
  assert_true(report.inconsistent_items > 0)
  assert_true(report.resolved_items > 0)
}

// Test 3: Data Synchronization Consistency
test "data synchronization consistency across distributed systems" {
  // Create synchronization manager
  let sync_manager = azimuth::DataSynchronizationManager::new()
  
  // Configure synchronization settings
  azimuth::DataSynchronizationManager::configure(sync_manager, {
    "sync_mode": "bidirectional",
    "conflict_resolution": "timestamp_priority",
    "sync_interval_ms": 5000,
    "batch_size": 100,
    "compression": true
  })
  
  // Set up distributed data stores
  let data_stores = [
    ("primary-store", "us-east-1"),
    ("secondary-store", "us-west-2"),
    ("backup-store", "eu-west-1")
  ]
  
  for (store, region) in data_stores {
    azimuth::DataSynchronizationManager::register_data_store(sync_manager, store, region)
  }
  
  // Generate test data set
  let test_data = []
  let base_timestamp = azimuth::Time::now()
  
  for i in 0..1000 {
    let data_item = {
      "id": "sync-test-" + i.to_string(),
      "timestamp": base_timestamp + i * 1000,
      "value": i.to_double(),
      "metadata": {
        "source": "test-generator",
        "version": "1.0",
        "region": "us-east-1"
      }
    }
    test_data.push(data_item)
  }
  
  // Write data to primary store
  let primary_write_result = azimuth::DataSynchronizationManager::write_batch(
    sync_manager, 
    "primary-store", 
    test_data
  )
  
  assert_true(primary_write_result.success)
  assert_eq(primary_write_result.written_count, 1000)
  
  // Initiate synchronization
  let sync_result = azimuth::DataSynchronizationManager::synchronize_all(
    sync_manager, 
    {
      "direction": "push",
      "target_stores": ["secondary-store", "backup-store"],
      "verify_after_sync": true
    }
  )
  
  assert_true(sync_result.success)
  assert_true(sync_result.synced_stores.contains("secondary-store"))
  assert_true(sync_result.synced_stores.contains("backup-store"))
  
  // Verify synchronization consistency
  let consistency_verification = azimuth::DataSynchronizationManager::verify_consistency(
    sync_manager, 
    ["primary-store", "secondary-store", "backup-store"]
  )
  
  assert_true(consistency_verification.consistent)
  assert_eq(consistency_verification.inconsistent_stores.length(), 0)
  
  // Test concurrent updates and conflict resolution
  let concurrent_updates = []
  
  // Update primary store
  let primary_updates = []
  for i in 0..100 {
    let update = {
      "id": "sync-test-" + i.to_string(),
      "timestamp": base_timestamp + i * 1000 + 50000, // Later timestamp
      "value": (i + 1000).to_double(), // Updated value
      "metadata": {
        "source": "primary-updater",
        "version": "1.1",
        "region": "us-east-1"
      }
    }
    primary_updates.push(update)
  }
  
  let primary_update_result = azimuth::DataSynchronizationManager::write_batch(
    sync_manager, 
    "primary-store", 
    primary_updates
  )
  
  assert_true(primary_update_result.success)
  
  // Simulate concurrent updates in secondary store
  let secondary_updates = []
  for i in 0..50 {
    let update = {
      "id": "sync-test-" + i.to_string(),
      "timestamp": base_timestamp + i * 1000 + 25000, // Earlier timestamp
      "value": (i + 2000).to_double(), // Different value
      "metadata": {
        "source": "secondary-updater",
        "version": "1.1",
        "region": "us-west-2"
      }
    }
    secondary_updates.push(update)
  }
  
  let secondary_update_result = azimuth::DataSynchronizationManager::write_batch(
    sync_manager, 
    "secondary-store", 
    secondary_updates
  )
  
  assert_true(secondary_update_result.success)
  
  // Synchronize with conflict resolution
  let conflict_sync_result = azimuth::DataSynchronizationManager::synchronize_with_resolution(
    sync_manager, 
    {
      "conflict_resolution": "timestamp_priority",
      "create_conflict_report": true
    }
  )
  
  assert_true(conflict_sync_result.success)
  assert_true(conflict_sync_result.conflicts_detected)
  assert_true(conflict_sync_result.conflicts_resolved > 0)
  
  // Verify conflicts were resolved correctly (timestamp priority)
  let post_conflict_verification = azimuth::DataSynchronizationManager::verify_consistency(
    sync_manager, 
    ["primary-store", "secondary-store", "backup-store"]
  )
  
  assert_true(post_conflict_verification.consistent)
  
  // Check specific items for correct resolution
  for i in 0..50 {
    let item_id = "sync-test-" + i.to_string()
    let primary_item = azimuth::DataSynchronizationManager::get_item(
      sync_manager, 
      "primary-store", 
      item_id
    )
    let secondary_item = azimuth::DataSynchronizationManager::get_item(
      sync_manager, 
      "secondary-store", 
      item_id
    )
    
    assert_true(primary_item.is_some)
    assert_true(secondary_item.is_some)
    
    // Should have primary's value (later timestamp)
    assert_eq(primary_item.unwrap()["value"], (i + 1000).to_double())
    assert_eq(secondary_item.unwrap()["value"], (i + 1000).to_double())
  }
  
  // Test incremental synchronization
  let incremental_data = []
  for i in 1000..1100 {
    let data_item = {
      "id": "sync-test-" + i.to_string(),
      "timestamp": base_timestamp + i * 1000,
      "value": i.to_double(),
      "metadata": {
        "source": "incremental-generator",
        "version": "1.0",
        "region": "us-east-1"
      }
    }
    incremental_data.push(data_item)
  }
  
  // Write incremental data to primary
  let incremental_write_result = azimuth::DataSynchronizationManager::write_batch(
    sync_manager, 
    "primary-store", 
    incremental_data
  )
  
  assert_true(incremental_write_result.success)
  
  // Perform incremental sync
  let incremental_sync_result = azimuth::DataSynchronizationManager::synchronize_incremental(
    sync_manager, 
    {
      "since_timestamp": base_timestamp + 1000 * 999,
      "verify_after_sync": true
    }
  )
  
  assert_true(incremental_sync_result.success)
  assert_eq(incremental_sync_result.synced_items, 100)
  
  // Verify incremental sync consistency
  let incremental_verification = azimuth::DataSynchronizationManager::verify_consistency(
    sync_manager, 
    ["primary-store", "secondary-store", "backup-store"]
  )
  
  assert_true(incremental_verification.consistent)
  
  // Test synchronization failure recovery
  azimuth::DataSynchronizationManager::simulate_store_failure(sync_manager, "backup-store")
  
  let failure_sync_result = azimuth::DataSynchronizationManager::synchronize_all(
    sync_manager, 
    {
      "direction": "push",
      "target_stores": ["secondary-store", "backup-store"],
      "retry_on_failure": true,
      "max_retries": 3
    }
  )
  
  assert_true(failure_sync_result.success)
  assert_true(failure_sync_result.failed_stores.contains("backup-store"))
  assert_true(failure_sync_result.successful_stores.contains("secondary-store"))
  
  // Recover store
  azimuth::DataSynchronizationManager::recover_store(sync_manager, "backup-store")
  
  // Retry failed synchronization
  let recovery_sync_result = azimuth::DataSynchronizationManager::synchronize_all(
    sync_manager, 
    {
      "direction": "push",
      "target_stores": ["backup-store"],
      "verify_after_sync": true
    }
  )
  
  assert_true(recovery_sync_result.success)
  assert_true(recovery_sync_result.synced_stores.contains("backup-store"))
  
  // Final consistency verification
  let final_verification = azimuth::DataSynchronizationManager::verify_consistency(
    sync_manager, 
    ["primary-store", "secondary-store", "backup-store"]
  )
  
  assert_true(final_verification.consistent)
}

// Test 4: Cross-Service State Consistency
test "cross-service state consistency validation" {
  // Create state consistency validator
  let state_validator = azimuth::StateConsistencyValidator::new()
  
  // Register services and their states
  let services = [
    {
      "name": "user-service",
      "state_type": "user_profile",
      "dependencies": ["order-service", "notification-service"]
    },
    {
      "name": "order-service", 
      "state_type": "order_state",
      "dependencies": ["user-service", "inventory-service", "payment-service"]
    },
    {
      "name": "inventory-service",
      "state_type": "inventory_state", 
      "dependencies": ["order-service"]
    },
    {
      "name": "payment-service",
      "state_type": "payment_state",
      "dependencies": ["order-service", "user-service"]
    },
    {
      "name": "notification-service",
      "state_type": "notification_state",
      "dependencies": ["user-service", "order-service"]
    }
  ]
  
  for service in services {
    azimuth::StateConsistencyValidator::register_service(state_validator, service)
  }
  
  // Define consistency rules
  let consistency_rules = [
    {
      "name": "user_order_consistency",
      "description": "User must exist for all orders",
      "services": ["user-service", "order-service"],
      "validation": "order.user_id EXISTS IN user-service.users"
    },
    {
      "name": "order_inventory_consistency", 
      "description": "Inventory must be sufficient for orders",
      "services": ["order-service", "inventory-service"],
      "validation": "SUM(order.items.quantity) <= inventory-service.stock"
    },
    {
      "name": "order_payment_consistency",
      "description": "Payment must match order total",
      "services": ["order-service", "payment-service"],
      "validation": "payment.amount = order.total"
    }
  ]
  
  for rule in consistency_rules {
    azimuth::StateConsistencyValidator::add_consistency_rule(state_validator, rule)
  }
  
  // Create consistent test scenario
  let user_id = "user-state-test"
  let order_id = "order-state-test"
  
  // Set up user state
  let user_state = {
    "user_id": user_id,
    "name": "Test User",
    "email": "test@example.com",
    "status": "active",
    "created_at": azimuth::Time::now()
  }
  
  azimuth::StateConsistencyValidator::set_service_state(
    state_validator, 
    "user-service", 
    user_id, 
    user_state
  )
  
  // Set up order state
  let order_state = {
    "order_id": order_id,
    "user_id": user_id,
    "status": "completed",
    "items": [
      {"product_id": "prod-1", "quantity": 2, "price": 50.0},
      {"product_id": "prod-2", "quantity": 1, "price": 25.0}
    ],
    "total": 125.0,
    "created_at": azimuth::Time::now()
  }
  
  azimuth::StateConsistencyValidator::set_service_state(
    state_validator, 
    "order-service", 
    order_id, 
    order_state
  )
  
  // Set up inventory state
  let inventory_state = {
    "product_id": "prod-1",
    "stock": 100,
    "reserved": 2,
    "available": 98
  }
  
  azimuth::StateConsistencyValidator::set_service_state(
    state_validator, 
    "inventory-service", 
    "prod-1", 
    inventory_state
  )
  
  // Set up payment state
  let payment_state = {
    "payment_id": "payment-state-test",
    "order_id": order_id,
    "user_id": user_id,
    "amount": 125.0,
    "status": "completed",
    "method": "credit_card"
  }
  
  azimuth::StateConsistencyValidator::set_service_state(
    state_validator, 
    "payment-service", 
    "payment_state["payment_id"], 
    payment_state
  )
  
  // Validate consistency
  let consistency_result = azimuth::StateConsistencyValidator::validate_consistency(state_validator)
  
  assert_true(consistency_result.overall_consistent)
  assert_eq(consistency_result.passed_rules.length(), 3)
  assert_eq(consistency_result.failed_rules.length(), 0)
  
  // Test inconsistent scenario
  // Create order with non-existent user
  let invalid_order_id = "invalid-order-test"
  let invalid_order_state = {
    "order_id": invalid_order_id,
    "user_id": "non-existent-user",
    "status": "pending",
    "items": [{"product_id": "prod-1", "quantity": 1, "price": 50.0}],
    "total": 50.0,
    "created_at": azimuth::Time::now()
  }
  
  azimuth::StateConsistencyValidator::set_service_state(
    state_validator, 
    "order-service", 
    invalid_order_id, 
    invalid_order_state
  )
  
  // Validate with inconsistency
  let inconsistent_result = azimuth::StateConsistencyValidator::validate_consistency(state_validator)
  
  assert_false(inconsistent_result.overall_consistent)
  assert_true(inconsistent_result.failed_rules.length() > 0)
  assert_true(inconsistent_result.failed_rules.contains("user_order_consistency"))
  
  // Test state repair
  let repair_result = azimuth::StateConsistencyValidator::repair_inconsistencies(
    state_validator, 
    {
      "auto_fix": true,
      "create_missing_references": true,
      "remove_orphaned_records": true
    }
  )
  
  assert_true(repair_result.success)
  assert_true(repair_result.repaired_inconsistencies > 0)
  
  // Verify repair
  let post_repair_result = azimuth::StateConsistencyValidator::validate_consistency(state_validator)
  assert_true(post_repair_result.overall_consistent)
  
  // Test state transitions
  let transition_rules = [
    {
      "service": "order-service",
      "from_state": "pending",
      "to_state": "processing",
      "conditions": ["user.active", "inventory.sufficient"],
      "actions": ["inventory.reserve", "notification.send"]
    },
    {
      "service": "order-service",
      "from_state": "processing", 
      "to_state": "completed",
      "conditions": ["payment.completed"],
      "actions": ["inventory.confirm", "notification.send"]
    }
  ]
  
  for rule in transition_rules {
    azimuth::StateConsistencyValidator::add_transition_rule(state_validator, rule)
  }
  
  // Create pending order
  let pending_order_id = "pending-order-test"
  let pending_order_state = {
    "order_id": pending_order_id,
    "user_id": user_id,
    "status": "pending",
    "items": [{"product_id": "prod-1", "quantity": 1, "price": 50.0}],
    "total": 50.0,
    "created_at": azimuth::Time::now()
  }
  
  azimuth::StateConsistencyValidator::set_service_state(
    state_validator, 
    "order-service", 
    pending_order_id, 
    pending_order_state
  )
  
  // Execute state transition
  let transition_result = azimuth::StateConsistencyValidator::execute_transition(
    state_validator, 
    "order-service", 
    pending_order_id, 
    "processing"
  )
  
  assert_true(transition_result.success)
  assert_true(transition_result.conditions_met)
  assert_true(transition_result.actions_executed.contains("inventory.reserve"))
  
  // Verify transition consistency
  let transition_validation = azimuth::StateConsistencyValidator::validate_transition_consistency(
    state_validator
  )
  
  assert_true(transition_validation.consistent)
  
  // Test distributed state snapshot
  let snapshot_result = azimuth::StateConsistencyValidator::create_distributed_snapshot(
    state_validator, 
    {
      "include_all_services": true,
      "compress_snapshot": true,
      "verify_snapshot": true
    }
  )
  
  assert_true(snapshot_result.success)
  assert_true(snapshot_result.snapshot_id.length() > 0)
  
  // Verify snapshot consistency
  let snapshot_consistency = azimuth::StateConsistencyValidator::verify_snapshot_consistency(
    state_validator, 
    snapshot_result.snapshot_id
  )
  
  assert_true(snapshot_consistency.consistent)
  
  // Test state restoration from snapshot
  let restoration_result = azimuth::StateConsistencyValidator::restore_from_snapshot(
    state_validator, 
    snapshot_result.snapshot_id
  )
  
  assert_true(restoration_result.success)
  assert_true(restoration_result.restored_services.length() > 0)
}

// Test 5: Cross-Service Causal Consistency
test "cross-service causal consistency in distributed systems" {
  // Create causal consistency manager
  let causal_manager = azimuth::CausalConsistencyManager::new()
  
  // Configure causal consistency settings
  azimuth::CausalConsistencyManager::configure(causal_manager, {
    "causality_tracking": true,
    "vector_clocks": true,
    "causal_ordering": true,
    "conflict_resolution": "causal_context"
  })
  
  // Register services with causal consistency
  let services = ["event-service", "command-service", "query-service", "projection-service"]
  
  for service in services {
    azimuth::CausalConsistencyManager::register_service(causal_manager, service)
  }
  
  // Initialize vector clocks
  let vector_clocks = {}
  for service in services {
    vector_clocks[service] = 0
  }
  
  // Test causal ordering of events
  let events = []
  
  // Event 1: User created
  let event1 = {
    "event_id": "event-1",
    "event_type": "user_created",
    "data": {"user_id": "user-1", "name": "Alice"},
    "causal_context": {
      "vector_clock": vector_clocks,
      "parent_event_id": null
    },
    "timestamp": azimuth::Time::now()
  }
  
  vector_clocks["event-service"] = vector_clocks["event-service"] + 1
  events.push(event1)
  
  // Event 2: User profile updated (causally after Event 1)
  let event2 = {
    "event_id": "event-2",
    "event_type": "user_profile_updated",
    "data": {"user_id": "user-1", "email": "alice@example.com"},
    "causal_context": {
      "vector_clock": vector_clocks,
      "parent_event_id": "event-1"
    },
    "timestamp": azimuth::Time::now()
  }
  
  vector_clocks["event-service"] = vector_clocks["event-service"] + 1
  events.push(event2)
  
  // Event 3: Order created (causally after Event 1, concurrent with Event 2)
  let event3 = {
    "event_id": "event-3",
    "event_type": "order_created",
    "data": {"user_id": "user-1", "order_id": "order-1"},
    "causal_context": {
      "vector_clock": vector_clocks,
      "parent_event_id": "event-1"
    },
    "timestamp": azimuth::Time::now()
  }
  
  vector_clocks["event-service"] = vector_clocks["event-service"] + 1
  events.push(event3)
  
  // Process events through services
  for event in events {
    let process_result = azimuth::CausalConsistencyManager::process_event(
      causal_manager, 
      event
    )
    
    assert_true(process_result.success)
    assert_true(process_result.causal_order_preserved)
  }
  
  // Verify causal ordering
  let causal_order = azimuth::CausalConsistencyManager::get_causal_order(causal_manager)
  
  assert_eq(causal_order[0], "event-1") // First event
  assert_true(causal_order[1] == "event-2" || causal_order[1] == "event-3") // Events 2 and 3 can be in either order
  assert_true(causal_order[2] == "event-2" || causal_order[2] == "event-3") // But both must come after event-1
  
  // Test concurrent event processing
  let concurrent_events = []
  
  // Concurrent Event A: Update inventory
  let event_a = {
    "event_id": "event-a",
    "event_type": "inventory_updated",
    "data": {"product_id": "prod-1", "quantity": 100},
    "causal_context": {
      "vector_clock": {"event-service": 3, "command-service": 0, "query-service": 0, "projection-service": 0},
      "parent_event_id": null
    },
    "timestamp": azimuth::Time::now()
  }
  
  // Concurrent Event B: Update price
  let event_b = {
    "event_id": "event-b", 
    "event_type": "price_updated",
    "data": {"product_id": "prod-1", "price": 99.99},
    "causal_context": {
      "vector_clock": {"event-service": 0, "command-service": 1, "query-service": 0, "projection-service": 0},
      "parent_event_id": null
    },
    "timestamp": azimuth::Time::now()
  }
  
  concurrent_events.push(event_a)
  concurrent_events.push(event_b)
  
  // Process concurrent events
  for event in concurrent_events {
    let result = azimuth::CausalConsistencyManager::process_event(causal_manager, event)
    assert_true(result.success)
  }
  
  // Verify concurrent events are properly tracked
  let concurrent_tracking = azimuth::CausalConsistencyManager::get_concurrent_events(causal_manager)
  assert_true(concurrent_tracking.contains("event-a"))
  assert_true(concurrent_tracking.contains("event-b"))
  
  // Test causality violation detection
  let violating_event = {
    "event_id": "event-violation",
    "event_type": "order_completed",
    "data": {"order_id": "order-1", "user_id": "user-1"},
    "causal_context": {
      "vector_clock": {"event-service": 1, "command-service": 0, "query-service": 0, "projection-service": 0},
      "parent_event_id": "event-3" // Claims to be after event-3 but has earlier vector clock
    },
    "timestamp": azimuth::Time::now()
  }
  
  let violation_result = azimuth::CausalConsistencyManager::process_event(causal_manager, violating_event)
  
  assert_false(violation_result.success)
  assert_true(violation_result.causality_violation)
  assert_true(violation_result.violation_type == "causal_context_mismatch")
  
  // Test causal consistency verification
  let consistency_verification = azimuth::CausalConsistencyManager::verify_causal_consistency(
    causal_manager
  )
  
  assert_true(consistency_verification.causal_consistency_maintained)
  assert_true(consistency_verification.violations_detected == 1) // The violating event
  
  // Test event replay with causal consistency
  let replay_events = []
  
  // Create events with proper causal ordering for replay
  let base_clock = {"event-service": 0, "command-service": 0, "query-service": 0, "projection-service": 0}
  
  let replay_event1 = {
    "event_id": "replay-1",
    "event_type": "user_registered",
    "data": {"user_id": "user-replay", "name": "Bob"},
    "causal_context": {
      "vector_clock": base_clock,
      "parent_event_id": null
    },
    "timestamp": azimuth::Time::now()
  }
  
  let clock_after_e1 = {"event-service": 1, "command-service": 0, "query-service": 0, "projection-service": 0}
  
  let replay_event2 = {
    "event_id": "replay-2",
    "event_type": "user_verified",
    "data": {"user_id": "user-replay", "verified": true},
    "causal_context": {
      "vector_clock": clock_after_e1,
      "parent_event_id": "replay-1"
    },
    "timestamp": azimuth::Time::now()
  }
  
  let clock_after_e2 = {"event-service": 2, "command-service": 0, "query-service": 0, "projection-service": 0}
  
  let replay_event3 = {
    "event_id": "replay-3",
    "event_type": "user_preferences_set",
    "data": {"user_id": "user-replay", "theme": "dark"},
    "causal_context": {
      "vector_clock": clock_after_e2,
      "parent_event_id": "replay-2"
    },
    "timestamp": azimuth::Time::now()
  }
  
  replay_events.push(replay_event1)
  replay_events.push(replay_event2)
  replay_events.push(replay_event3)
  
  // Shuffle events to test replay ordering
  let shuffled_events = [replay_event2, replay_event1, replay_event3]
  
  let replay_result = azimuth::CausalConsistencyManager::replay_events(
    causal_manager, 
    shuffled_events,
    {
      "enforce_causal_order": true,
      "detect_cycles": true
    }
  )
  
  assert_true(replay_result.success)
  assert_eq(replay_result.processed_events, 3)
  assert_true(replay_result.events_reordered)
  
  // Verify final state after replay
  let final_state = azimuth::CausalConsistencyManager::get_projection_state(
    causal_manager, 
    "user-replay"
  )
  
  assert_true(final_state.contains("user_id"))
  assert_true(final_state["verified"] == true)
  assert_true(final_state["theme"] == "dark")
  
  // Test distributed vector clock synchronization
  let sync_result = azimuth::CausalConsistencyManager::synchronize_vector_clocks(
    causal_manager, 
    {
      "sync_all_services": true,
      "merge_strategy": "element_wise_maximum"
    }
  )
  
  assert_true(sync_result.success)
  assert_true(sync_result.services_synchronized.contains("event-service"))
  assert_true(sync_result.services_synchronized.contains("command-service"))
  assert_true(sync_result.services_synchronized.contains("query-service"))
  assert_true(sync_result.services_synchronized.contains("projection-service"))
  
  // Verify synchronized clocks
  let synchronized_clocks = azimuth::CausalConsistencyManager::get_vector_clocks(causal_manager)
  assert_true(synchronized_clocks.length() == 4)
  
  for (service, clock) in synchronized_clocks {
    assert_true(clock >= 0)
  }
}

// Test 6: Cross-Service Cache Consistency
test "cross-service cache consistency and invalidation" {
  // Create cache consistency manager
  let cache_manager = azimuth::CacheConsistencyManager::new()
  
  // Configure cache settings
  azimuth::CacheConsistencyManager::configure(cache_manager, {
    "invalidation_strategy": "propagation",
    "consistency_window_ms": 5000,
    "cache_sync_interval_ms": 10000,
    "versioning": true
  })
  
  // Register service caches
  let services = [
    {
      "name": "user-service",
      "cache_type": "user_cache",
      "ttl_seconds": 300
    },
    {
      "name": "product-service", 
      "cache_type": "product_cache",
      "ttl_seconds": 600
    },
    {
      "name": "order-service",
      "cache_type": "order_cache",
      "ttl_seconds": 180
    }
  ]
  
  for service in services {
    azimuth::CacheConsistencyManager::register_cache(cache_manager, service)
  }
  
  // Populate caches with test data
  let user_data = {
    "user_id": "user-cache-test",
    "name": "Cache Test User",
    "email": "cache@example.com",
    "preferences": {"theme": "light"},
    "version": 1
  }
  
  azimuth::CacheConsistencyManager::put(cache_manager, "user-service", "user-cache-test", user_data)
  
  let product_data = {
    "product_id": "product-cache-test",
    "name": "Cache Test Product",
    "price": 99.99,
    "stock": 100,
    "version": 1
  }
  
  azimuth::CacheConsistencyManager::put(cache_manager, "product-service", "product-cache-test", product_data)
  
  // Test cache hits
  let user_cache_hit = azimuth::CacheConsistencyManager::get(cache_manager, "user-service", "user-cache-test")
  assert_true(user_cache_hit.is_some)
  assert_eq(user_cache_hit.unwrap()["name"], "Cache Test User")
  
  let product_cache_hit = azimuth::CacheConsistencyManager::get(cache_manager, "product-service", "product-cache-test")
  assert_true(product_cache_hit.is_some)
  assert_eq(product_cache_hit.unwrap()["name"], "Cache Test Product")
  
  // Test cache invalidation propagation
  let updated_user_data = {
    "user_id": "user-cache-test",
    "name": "Updated Cache User",
    "email": "cache@example.com",
    "preferences": {"theme": "dark"},
    "version": 2
  }
  
  // Update in user-service
  azimuth::CacheConsistencyManager::put(cache_manager, "user-service", "user-cache-test", updated_user_data)
  
  // Invalidate and propagate
  let invalidation_result = azimuth::CacheConsistencyManager::invalidate_and_propagate(
    cache_manager, 
    "user-service", 
    "user-cache-test",
    {
      "propagate_to_all": true,
      "version_check": true
    }
  )
  
  assert_true(invalidation_result.success)
  assert_true(invalidation_result.invalidated_services.contains("user-service"))
  
  // Verify cache was updated
  let updated_cache_hit = azimuth::CacheConsistencyManager::get(cache_manager, "user-service", "user-cache-test")
  assert_true(updated_cache_hit.is_some)
  assert_eq(updated_cache_hit.unwrap()["name"], "Updated Cache User")
  assert_eq(updated_cache_hit.unwrap()["version"], 2)
  
  // Test cross-service cache dependencies
  // Set up dependency: order-service caches user data
  let order_user_data = {
    "user_id": "user-cache-test",
    "name": "Cache Test User",
    "email": "cache@example.com",
    "version": 1
  }
  
  azimuth::CacheConsistencyManager::put(cache_manager, "order-service", "user-cache-test", order_user_data)
  
  // Add cache dependency
  azimuth::CacheConsistencyManager::add_dependency(
    cache_manager, 
    "order-service", 
    "user-cache-test", 
    "user-service"
  )
  
  // Update user data again
  let final_user_data = {
    "user_id": "user-cache-test",
    "name": "Final Cache User",
    "email": "final@example.com",
    "preferences": {"theme": "blue"},
    "version": 3
  }
  
  azimuth::CacheConsistencyManager::put(cache_manager, "user-service", "user-cache-test", final_user_data)
  
  // Invalidate with dependency propagation
  let dependency_invalidation = azimuth::CacheConsistencyManager::invalidate_with_dependencies(
    cache_manager, 
    "user-service", 
    "user-cache-test"
  )
  
  assert_true(dependency_invalidation.success)
  assert_true(dependency_invalidation.invalidated_services.contains("user-service"))
  assert_true(dependency_invalidation.invalidated_services.contains("order-service"))
  
  // Verify dependent cache was invalidated
  let order_cache_after_invalidation = azimuth::CacheConsistencyManager::get(cache_manager, "order-service", "user-cache-test")
  assert_true(order_cache_after_invalidation.is_none) // Should be invalidated
  
  // Test cache consistency verification
  let consistency_verification = azimuth::CacheConsistencyManager::verify_consistency(
    cache_manager, 
    {
      "check_all_services": true,
      "check_versions": true,
      "check_dependencies": true
    }
  )
  
  assert_true(consistency_verification.overall_consistent)
  assert_eq(consistency_verification.inconsistent_entries.length(), 0)
  
  // Test cache synchronization
  // Add same data to multiple services with different versions
  let sync_user_data_v1 = {
    "user_id": "sync-user-test",
    "name": "Sync User",
    "version": 1
  }
  
  let sync_user_data_v2 = {
    "user_id": "sync-user-test", 
    "name": "Sync User Updated",
    "version": 2
  }
  
  azimuth::CacheConsistencyManager::put(cache_manager, "user-service", "sync-user-test", sync_user_data_v1)
  azimuth::CacheConsistencyManager::put(cache_manager, "order-service", "sync-user-test", sync_user_data_v1)
  azimuth::CacheConsistencyManager::put(cache_manager, "product-service", "sync-user-test", sync_user_data_v2)
  
  // Synchronize to latest version
  let sync_result = azimuth::CacheConsistencyManager::synchronize_caches(
    cache_manager, 
    "sync-user-test",
    {
      "strategy": "latest_version",
      "propagate_updates": true
    }
  )
  
  assert_true(sync_result.success)
  assert_true(sync_result.updated_services.contains("user-service"))
  assert_true(sync_result.updated_services.contains("order-service"))
  
  // Verify synchronization
  let user_sync_result = azimuth::CacheConsistencyManager::get(cache_manager, "user-service", "sync-user-test")
  let order_sync_result = azimuth::CacheConsistencyManager::get(cache_manager, "order-service", "sync-user-test")
  let product_sync_result = azimuth::CacheConsistencyManager::get(cache_manager, "product-service", "sync-user-test")
  
  assert_true(user_sync_result.is_some)
  assert_true(order_sync_result.is_some)
  assert_true(product_sync_result.is_some)
  
  // All should have version 2
  assert_eq(user_sync_result.unwrap()["version"], 2)
  assert_eq(order_sync_result.unwrap()["version"], 2)
  assert_eq(product_sync_result.unwrap()["version"], 2)
  
  // Test cache warmup with consistency
  let warmup_data = []
  for i in 0..100 {
    let data = {
      "key": "warmup-" + i.to_string(),
      "value": "value-" + i.to_string(),
      "version": 1
    }
    warmup_data.push(data)
  }
  
  let warmup_result = azimuth::CacheConsistencyManager::warmup_caches(
    cache_manager, 
    warmup_data,
    {
      "target_services": ["user-service", "product-service"],
      "verify_after_warmup": true
    }
  )
  
  assert_true(warmup_result.success)
  assert_eq(warmup_result.warmed_items, 100)
  assert_eq(warmup_result.target_services.length(), 2)
  
  // Verify warmup consistency
  for i in 0..100 {
    let key = "warmup-" + i.to_string()
    let user_warmup_hit = azimuth::CacheConsistencyManager::get(cache_manager, "user-service", key)
    let product_warmup_hit = azimuth::CacheConsistencyManager::get(cache_manager, "product-service", key)
    
    assert_true(user_warmup_hit.is_some)
    assert_true(product_warmup_hit.is_some)
    assert_eq(user_warmup_hit.unwrap()["value"], "value-" + i.to_string())
    assert_eq(product_warmup_hit.unwrap()["value"], "value-" + i.to_string())
  }
}

// Test 7: Cross-Service Distributed Lock Consistency
test "cross-service distributed lock consistency" {
  // Create distributed lock manager
  let lock_manager = azimuth::DistributedLockManager::new()
  
  // Configure lock settings
  azimuth::DistributedLockManager::configure(lock_manager, {
    "lock_timeout_ms": 30000,
    "heartbeat_interval_ms": 5000,
    "auto_renewal": true,
    "consistency_check_interval_ms": 10000
  })
  
  // Register services for distributed locking
  let services = ["order-service", "inventory-service", "payment-service"]
  
  for service in services {
    azimuth::DistributedLockManager::register_service(lock_manager, service)
  }
  
  // Test basic distributed lock
  let lock_key = "order-processing-123"
  
  let lock1_result = azimuth::DistributedLockManager::acquire_lock(
    lock_manager, 
    "order-service", 
    lock_key,
    {
      "timeout_ms": 10000,
      "auto_renew": true
    }
  )
  
  assert_true(lock1_result.success)
  assert_true(lock1_result.lock_id.length() > 0)
  
  // Try to acquire same lock from different service
  let lock2_result = azimuth::DistributedLockManager::acquire_lock(
    lock_manager, 
    "inventory-service", 
    lock_key,
    {
      "timeout_ms": 5000,
      "auto_renew": false
    }
  )
  
  assert_false(lock2_result.success) // Should fail, lock already held
  assert_true(lock2_result.reason == "lock_already_held")
  
  // Verify lock state
  let lock_state = azimuth::DistributedLockManager::get_lock_state(lock_manager, lock_key)
  assert_true(lock_state.is_some)
  assert_eq(lock_state.unwrap().holder_service, "order-service")
  assert_eq(lock_state.unwrap().lock_id, lock1_result.lock_id)
  
  // Test lock renewal
  let renewal_result = azimuth::DistributedLockManager::renew_lock(
    lock_manager, 
    lock1_result.lock_id
  )
  
  assert_true(renewal_result.success)
  assert_true(renewal_result.new_expiry > azimuth::Time::now())
  
  // Test lock transfer
  let transfer_result = azimuth::DistributedLockManager::transfer_lock(
    lock_manager, 
    lock1_result.lock_id,
    "inventory-service"
  )
  
  assert_true(transfer_result.success)
  
  // Verify lock was transferred
  let transferred_state = azimuth::DistributedLockManager::get_lock_state(lock_manager, lock_key)
  assert_eq(transferred_state.unwrap().holder_service, "inventory-service")
  
  // Original service should no longer hold the lock
  let original_check = azimuth::DistributedLockManager::check_lock_holder(
    lock_manager, 
    lock1_result.lock_id, 
    "order-service"
  )
  
  assert_false(original_check.holds_lock)
  
  // New service should hold the lock
  let new_check = azimuth::DistributedLockManager::check_lock_holder(
    lock_manager, 
    transfer_result.new_lock_id, 
    "inventory-service"
  )
  
  assert_true(new_check.holds_lock)
  
  // Test lock release
  let release_result = azimuth::DistributedLockManager::release_lock(
    lock_manager, 
    transfer_result.new_lock_id
  )
  
  assert_true(release_result.success)
  
  // Verify lock is released
  let released_state = azimuth::DistributedLockManager::get_lock_state(lock_manager, lock_key)
  assert_true(released_state.is_none)
  
  // Now other service should be able to acquire the lock
  let lock3_result = azimuth::DistributedLockManager::acquire_lock(
    lock_manager, 
    "payment-service", 
    lock_key,
    {
      "timeout_ms": 5000,
      "auto_renew": false
    }
  )
  
  assert_true(lock3_result.success)
  
  // Test lock consistency verification
  let consistency_check = azimuth::DistributedLockManager::verify_consistency(lock_manager)
  assert_true(consistency_check.consistent)
  assert_eq(consistency_check.inconsistent_locks.length(), 0)
  
  // Test concurrent lock acquisition
  let concurrent_locks = []
  let concurrent_results = []
  
  // Try to acquire multiple locks for the same order
  let order_id = "concurrent-order-456"
  let lock_keys = [
    "order-" + order_id,
    "inventory-" + order_id,
    "payment-" + order_id
  ]
  
  for lock_key in lock_keys {
    let result = azimuth::DistributedLockManager::acquire_lock(
      lock_manager, 
      "order-service", 
      lock_key,
      {
        "timeout_ms": 1000,
        "auto_renew": false
      }
    )
    concurrent_results.push(result)
    
    if result.success {
      concurrent_locks.push(result.lock_id)
    }
  }
  
  // Should acquire all locks
  assert_eq(concurrent_results.filter(|r| r.success).length(), 3)
  assert_eq(concurrent_locks.length(), 3)
  
  // Release all locks
  for lock_id in concurrent_locks {
    let release_result = azimuth::DistributedLockManager::release_lock(lock_manager, lock_id)
    assert_true(release_result.success)
  }
  
  // Test lock timeout and recovery
  let timeout_lock_key = "timeout-test-789"
  
  let timeout_lock_result = azimuth::DistributedLockManager::acquire_lock(
    lock_manager, 
    "order-service", 
    timeout_lock_key,
    {
      "timeout_ms": 1000, // Short timeout for testing
      "auto_renew": false
    }
  )
  
  assert_true(timeout_lock_result.success)
  
  // Wait for timeout
  azimuth::Thread::sleep(1500) // 1.5 seconds
  
  // Lock should be expired
  let expired_check = azimuth::DistributedLockManager::check_lock_holder(
    lock_manager, 
    timeout_lock_result.lock_id, 
    "order-service"
  )
  
  assert_false(expired_check.holds_lock)
  assert_true(expired_check.expired)
  
  // Another service should be able to acquire the expired lock
  let recovery_lock_result = azimuth::DistributedLockManager::acquire_lock(
    lock_manager, 
    "inventory-service", 
    timeout_lock_key,
    {
      "timeout_ms": 5000,
      "auto_renew": false
    }
  )
  
  assert_true(recovery_lock_result.success)
  
  // Test distributed lock state synchronization
  let sync_result = azimuth::DistributedLockManager::synchronize_lock_state(
    lock_manager, 
    {
      "force_sync": true,
      "cleanup_expired": true
    }
  )
  
  assert_true(sync_result.success)
  assert_true(sync_result.expired_locks_cleaned >= 0)
  
  // Final consistency check
  let final_consistency = azimuth::DistributedLockManager::verify_consistency(lock_manager)
  assert_true(final_consistency.consistent)
  
  // Release remaining lock
  azimuth::DistributedLockManager::release_lock(lock_manager, recovery_lock_result.lock_id)
}

// Test 8: Cross-Service Data Versioning Consistency
test "cross-service data versioning consistency" {
  // Create versioning manager
  let versioning_manager = azimuth::DataVersioningManager::new()
  
  // Configure versioning settings
  azimuth::DataVersioningManager::configure(versioning_manager, {
    "version_strategy": "semantic",
    "conflict_resolution": "highest_version",
    "version_propagation": true,
    "compatibility_check": true
  })
  
  // Register services for versioning
  let services = ["user-service", "profile-service", "preferences-service"]
  
  for service in services {
    azimuth::DataVersioningManager::register_service(versioning_manager, service)
  }
  
  // Test versioned data creation
  let user_id = "versioning-user-123"
  let base_version = "1.0.0"
  
  let user_data_v1 = {
    "user_id": user_id,
    "name": "Versioning User",
    "email": "version@example.com",
    "version": base_version,
    "created_at": azimuth::Time::now(),
    "schema_version": "1.0"
  }
  
  let version_result = azimuth::DataVersioningManager::create_versioned_data(
    versioning_manager, 
    "user-service", 
    user_id, 
    user_data_v1
  )
  
  assert_true(version_result.success)
  assert_eq(version_result.version, base_version)
  
  // Test version propagation
  let propagation_result = azimuth::DataVersioningManager::propagate_version(
    versioning_manager, 
    "user-service", 
    user_id,
    {
      "target_services": ["profile-service", "preferences-service"],
      "async": false
    }
  )
  
  assert_true(propagation_result.success)
  assert_true(propagation_result.propagated_to.contains("profile-service"))
  assert_true(propagation_result.propagated_to.contains("preferences-service"))
  
  // Verify propagated versions
  let profile_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "profile-service", 
    user_id
  )
  
  let preferences_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "preferences-service", 
    user_id
  )
  
  assert_eq(profile_version, base_version)
  assert_eq(preferences_version, base_version)
  
  // Test version update with compatibility check
  let updated_data = {
    "user_id": user_id,
    "name": "Updated Versioning User",
    "email": "updated@example.com",
    "phone": "+1234567890", // New field
    "version": "1.1.0",
    "updated_at": azimuth::Time::now(),
    "schema_version": "1.1"
  }
  
  let update_result = azimuth::DataVersioningManager::update_versioned_data(
    versioning_manager, 
    "user-service", 
    user_id, 
    updated_data,
    {
      "compatibility_check": true,
      "auto_propagate": true
    }
  )
  
  assert_true(update_result.success)
  assert_eq(update_result.new_version, "1.1.0")
  assert_true(update_result.compatibility_check_passed)
  
  // Verify version was updated across services
  let updated_profile_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "profile-service", 
    user_id
  )
  
  let updated_preferences_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "preferences-service", 
    user_id
  )
  
  assert_eq(updated_profile_version, "1.1.0")
  assert_eq(updated_preferences_version, "1.1.0")
  
  // Test version conflict detection
  // Simulate concurrent update with different version
  let conflicting_data = {
    "user_id": user_id,
    "name": "Conflicting User",
    "email": "conflict@example.com",
    "version": "1.2.0", // Higher version
    "updated_at": azimuth::Time::now(),
    "schema_version": "1.2"
  }
  
  // Update in profile-service directly (simulating concurrent update)
  let conflict_update = azimuth::DataVersioningManager::update_versioned_data(
    versioning_manager, 
    "profile-service", 
    user_id, 
    conflicting_data,
    {
      "compatibility_check": false,
      "auto_propagate": false
    }
  )
  
  assert_true(conflict_update.success)
  
  // Detect version conflicts
  let conflict_detection = azimuth::DataVersioningManager::detect_version_conflicts(
    versioning_manager, 
    user_id
  )
  
  assert_true(conflict_detection.conflicts_detected)
  assert_true(conflict_detection.conflicts.length() > 0)
  
  // Verify specific conflict
  let profile_conflict = conflict_detection.conflicts.find(|c| c.service == "profile-service")
  assert_true(profile_conflict.is_some)
  assert_eq(profile_conflict.unwrap().local_version, "1.2.0")
  assert_eq(profile_conflict.unwrap().remote_version, "1.1.0")
  
  // Resolve conflicts using highest version strategy
  let resolution_result = azimuth::DataVersioningManager::resolve_version_conflicts(
    versioning_manager, 
    user_id,
    {
      "strategy": "highest_version",
      "propagate_resolution": true
    }
  )
  
  assert_true(resolution_result.success)
  assert_true(resolution_result.resolved_conflicts > 0)
  
  // Verify resolution
  let post_resolution_conflicts = azimuth::DataVersioningManager::detect_version_conflicts(
    versioning_manager, 
    user_id
  )
  
  assert_false(post_resolution_conflicts.conflicts_detected)
  
  // Verify all services have the same version
  let final_user_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "user-service", 
    user_id
  )
  
  let final_profile_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "profile-service", 
    user_id
  )
  
  let final_preferences_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "preferences-service", 
    user_id
  )
  
  assert_eq(final_user_version, "1.2.0")
  assert_eq(final_profile_version, "1.2.0")
  assert_eq(final_preferences_version, "1.2.0")
  
  // Test version rollback
  let rollback_result = azimuth::DataVersioningManager::rollback_to_version(
    versioning_manager, 
    "user-service", 
    user_id, 
    "1.1.0",
    {
      "propagate_rollback": true,
      "create_rollback_checkpoint": true
    }
  )
  
  assert_true(rollback_result.success)
  assert_eq(rollback_result.rollback_version, "1.1.0")
  
  // Verify rollback across services
  let rolled_back_user_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "user-service", 
    user_id
  )
  
  let rolled_back_profile_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "profile-service", 
    user_id
  )
  
  let rolled_back_preferences_version = azimuth::DataVersioningManager::get_version(
    versioning_manager, 
    "preferences-service", 
    user_id
  )
  
  assert_eq(rolled_back_user_version, "1.1.0")
  assert_eq(rolled_back_profile_version, "1.1.0")
  assert_eq(rolled_back_preferences_version, "1.1.0")
  
  // Test version history tracking
  let version_history = azimuth::DataVersioningManager::get_version_history(
    versioning_manager, 
    "user-service", 
    user_id
  )
  
  assert_true(version_history.length() >= 3) // 1.0.0, 1.1.0, 1.2.0
  
  // Verify history contains expected versions
  let history_versions = version_history.map(|v| v.version)
  assert_true(history_versions.contains("1.0.0"))
  assert_true(history_versions.contains("1.1.0"))
  assert_true(history_versions.contains("1.2.0"))
  
  // Test cross-service version consistency verification
  let consistency_verification = azimuth::DataVersioningManager::verify_version_consistency(
    versioning_manager
  )
  
  assert_true(consistency_verification.overall_consistent)
  assert_eq(consistency_verification.inconsistent_entities.length(), 0)
  
  // Test version migration
  let migration_result = azimuth::DataVersioningManager::migrate_to_version(
    versioning_manager, 
    "2.0.0",
    {
      "migration_strategy": "backward_compatible",
      "create_migration_checkpoint": true,
      "verify_after_migration": true
    }
  )
  
  assert_true(migration_result.success)
  assert_eq(migration_result.target_version, "2.0.0")
  assert_true(migration_result.migration_checkpoint_created)
  
  // Verify migration
  let post_migration_verification = azimuth::DataVersioningManager::verify_version_consistency(
    versioning_manager
  )
  
  assert_true(post_migration_verification.overall_consistent)
}

// Test 9: Cross-Service Event Sourcing Consistency
test "cross-service event sourcing consistency" {
  // Create event sourcing manager
  let event_sourcing = azimuth::EventSourcingManager::new()
  
  // Configure event sourcing
  azimuth::EventSourcingManager::configure(event_sourcing, {
    "event_store": "distributed",
    "snapshot_interval": 100,
    "event_versioning": true,
    "causality_tracking": true
  })
  
  // Register aggregates
  let aggregates = [
    {
      "name": "User",
      "services": ["user-service", "profile-service"],
      "events": ["UserCreated", "UserUpdated", "UserDeleted"]
    },
    {
      "name": "Order",
      "services": ["order-service", "inventory-service", "payment-service"],
      "events": ["OrderCreated", "OrderUpdated", "OrderCancelled", "OrderCompleted"]
    }
  ]
  
  for aggregate in aggregates {
    azimuth::EventSourcingManager::register_aggregate(event_sourcing, aggregate)
  }
  
  // Test event creation and storage
  let user_id = "event-user-123"
  let user_events = []
  
  // Event 1: UserCreated
  let user_created_event = {
    "event_id": "user-created-1",
    "aggregate_id": user_id,
    "aggregate_type": "User",
    "event_type": "UserCreated",
    "event_data": {
      "user_id": user_id,
      "name": "Event User",
      "email": "event@example.com",
      "created_at": azimuth::Time::now()
    },
    "event_version": "1.0",
    "causation_id": null
  }
  
  let create_result = azimuth::EventSourcingManager::store_event(
    event_sourcing, 
    user_created_event
  )
  
  assert_true(create_result.success)
  user_events.push(user_created_event)
  
  // Event 2: UserUpdated
  let user_updated_event = {
    "event_id": "user-updated-1",
    "aggregate_id": user_id,
    "aggregate_type": "User",
    "event_type": "UserUpdated",
    "event_data": {
      "user_id": user_id,
      "name": "Updated Event User",
      "email": "updated@example.com",
      "updated_at": azimuth::Time::now()
    },
    "event_version": "1.0",
    "causation_id": user_created_event["event_id"]
  }
  
  let update_result = azimuth::EventSourcingManager::store_event(
    event_sourcing, 
    user_updated_event
  )
  
  assert_true(update_result.success)
  user_events.push(user_updated_event)
  
  // Test event replay and state reconstruction
  let replay_result = azimuth::EventSourcingManager::replay_events(
    event_sourcing, 
    user_id, 
    "User",
    {
      "from_version": 1,
      "to_version": null, // Replay all
      "rebuild_state": true
    }
  )
  
  assert_true(replay_result.success)
  assert_eq(replay_result.replayed_events, 2)
  assert_true(replay_result.final_state.is_some)
  
  let final_state = replay_result.final_state.unwrap()
  assert_eq(final_state["user_id"], user_id)
  assert_eq(final_state["name"], "Updated Event User")
  assert_eq(final_state["email"], "updated@example.com")
  
  // Test cross-service event propagation
  let propagation_result = azimuth::EventSourcingManager::propagate_events(
    event_sourcing, 
    user_events,
    {
      "target_services": ["user-service", "profile-service"],
      "async": false,
      "verify_delivery": true
    }
  )
  
  assert_true(propagation_result.success)
  assert_true(propagation_result.delivered_to.contains("user-service"))
  assert_true(propagation_result.delivered_to.contains("profile-service"))
  
  // Verify event consistency across services
  let consistency_check = azimuth::EventSourcingManager::verify_event_consistency(
    event_sourcing, 
    user_id, 
    "User"
  )
  
  assert_true(consistency_check.consistent)
  assert_eq(consistency_check.inconsistent_services.length(), 0)
  
  // Test concurrent event handling
  let order_id = "event-order-456"
  let concurrent_events = []
  
  // Create order events that can be processed concurrently
  let order_created_event = {
    "event_id": "order-created-1",
    "aggregate_id": order_id,
    "aggregate_type": "Order",
    "event_type": "OrderCreated",
    "event_data": {
      "order_id": order_id,
      "user_id": user_id,
      "items": [{"product_id": "prod-1", "quantity": 2}],
      "total": 199.99
    },
    "event_version": "1.0",
    "causation_id": null
  }
  
  let order_updated_event = {
    "event_id": "order-updated-1",
    "aggregate_id": order_id,
    "aggregate_type": "Order",
    "event_type": "OrderUpdated",
    "event_data": {
      "order_id": order_id,
      "items": [{"product_id": "prod-1", "quantity": 3}], // Updated quantity
      "total": 299.99
    },
    "event_version": "1.0",
    "causation_id": order_created_event["event_id"]
  }
  
  // Store events concurrently
  let concurrent_store_result = azimuth::EventSourcingManager::store_events_concurrent(
    event_sourcing, 
    [order_created_event, order_updated_event]
  )
  
  assert_true(concurrent_store_result.success)
  assert_eq(concurrent_store_result.stored_events, 2)
  
  // Verify event ordering is preserved
  let order_events = azimuth::EventSourcingManager::get_events(
    event_sourcing, 
    order_id, 
    "Order"
  )
  
  assert_eq(order_events.length(), 2)
  assert_eq(order_events[0]["event_type"], "OrderCreated")
  assert_eq(order_events[1]["event_type"], "OrderUpdated")
  assert_eq(order_events[1]["causation_id"], order_events[0]["event_id"])
  
  // Test snapshot creation
  let snapshot_result = azimuth::EventSourcingManager::create_snapshot(
    event_sourcing, 
    user_id, 
    "User",
    {
      "include_state": true,
      "include_metadata": true,
      "compress_snapshot": true
    }
  )
  
  assert_true(snapshot_result.success)
  assert_true(snapshot_result.snapshot_id.length() > 0)
  
  // Test replay from snapshot
  let snapshot_replay_result = azimuth::EventSourcingManager::replay_from_snapshot(
    event_sourcing, 
    snapshot_result.snapshot_id
  )
  
  assert_true(snapshot_replay_result.success)
  assert_true(snapshot_replay_result.state_reconstructed)
  
  // Test event versioning compatibility
  let incompatible_event = {
    "event_id": "user-incompatible-1",
    "aggregate_id": user_id,
    "aggregate_type": "User",
    "event_type": "UserUpdated",
    "event_data": {
      "user_id": user_id,
      "name": "Incompatible User",
      "new_field": "incompatible_value" // New field not in version 1.0
    },
    "event_version": "2.0", // Different version
    "causation_id": null
  }
  
  let incompatible_result = azimuth::EventSourcingManager::store_event(
    event_sourcing, 
    incompatible_event
  )
  
  assert_true(incompatible_result.success)
  assert_true(incompatible_result.version_compatibility_warning)
  
  // Test event sourcing consistency verification
  let final_consistency_check = azimuth::EventSourcingManager::verify_consistency(
    event_sourcing
  )
  
  assert_true(final_consistency_check.overall_consistent)
  assert_eq(final_consistency_check.inconsistent_aggregates.length(), 0)
}

// Test 10: Cross-Service Saga Pattern Consistency
test "cross-service saga pattern consistency and compensation" {
  // Create saga manager
  let saga_manager = azimuth::SagaManager::new()
  
  // Configure saga settings
  azimuth::SagaManager::configure(saga_manager, {
    "timeout_ms": 60000,
    "retry_attempts": 3,
    "compensation_strategy": "reverse_order",
    "persistent_state": true
  })
  
  // Define order processing saga
  let order_saga = {
    "saga_id": "order-processing-saga",
    "saga_type": "order_processing",
    "steps": [
      {
        "step_id": "validate_user",
        "service": "user-service",
        "action": "validate_user",
        "compensation": "noop",
        "timeout_ms": 5000
      },
      {
        "step_id": "create_order",
        "service": "order-service", 
        "action": "create_order",
        "compensation": "cancel_order",
        "timeout_ms": 10000
      },
      {
        "step_id": "reserve_inventory",
        "service": "inventory-service",
        "action": "reserve_inventory",
        "compensation": "release_inventory",
        "timeout_ms": 5000
      },
      {
        "step_id": "process_payment",
        "service": "payment-service",
        "action": "process_payment",
        "compensation": "refund_payment",
        "timeout_ms": 15000
      },
      {
        "step_id": "send_confirmation",
        "service": "notification-service",
        "action": "send_confirmation",
        "compensation": "send_cancellation",
        "timeout_ms": 5000
      }
    ]
  }
  
  azimuth::SagaManager::register_saga(saga_manager, order_saga)
  
  // Test successful saga execution
  let saga_data = {
    "user_id": "saga-user-123",
    "order_id": "saga-order-456",
    "items": [{"product_id": "prod-1", "quantity": 2}],
    "total": 199.99,
    "payment_method": "credit_card"
  }
  
  let saga_execution = azimuth::SagaManager::execute_saga(
    saga_manager, 
    "order-processing-saga", 
    saga_data
  )
  
  assert_true(saga_execution.success)
  assert_eq(saga_execution.completed_steps, 5)
  assert_eq(saga_execution.status, "completed")
  
  // Verify all steps were executed successfully
  for step in order_saga.steps {
    let step_result = azimuth::SagaManager::get_step_result(
      saga_manager, 
      saga_execution.execution_id, 
      step.step_id
    )
    
    assert_true(step_result.is_some)
    assert_true(step_result.unwrap().success)
  }
  
  // Test saga with failure and compensation
  let failing_saga_data = {
    "user_id": "saga-user-789",
    "order_id": "failing-order-123",
    "items": [{"product_id": "prod-2", "quantity": 1}],
    "total": 299.99,
    "payment_method": "invalid_card" // Will cause payment to fail
  }
  
  let failing_saga_execution = azimuth::SagaManager::execute_saga(
    saga_manager, 
    "order-processing-saga", 
    failing_saga_data
  )
  
  assert_false(failing_saga_execution.success)
  assert_eq(failing_saga_execution.status, "compensated")
  assert_true(failing_saga_execution.compensation_triggered)
  
  // Verify compensation steps were executed
  let compensation_steps = ["cancel_order", "release_inventory", "refund_payment", "send_cancellation"]
  
  for compensation_step in compensation_steps {
    let step_result = azimuth::SagaManager::get_step_result(
      saga_manager, 
      failing_saga_execution.execution_id, 
      compensation_step
    )
    
    assert_true(step_result.is_some)
    assert_true(step_result.unwrap().success)
    assert_true(step_result.unwrap().is_compensation)
  }
  
  // Test saga consistency verification
  let consistency_verification = azimuth::SagaManager::verify_saga_consistency(
    saga_manager
  )
  
  assert_true(consistency_verification.overall_consistent)
  assert_eq(consistency_verification.inconsistent_sagas.length(), 0)
  
  // Test saga state persistence and recovery
  let persistent_saga_data = {
    "user_id": "persistent-user-456",
    "order_id": "persistent-order-789",
    "items": [{"product_id": "prod-3", "quantity": 3}],
    "total": 399.99,
    "payment_method": "credit_card"
  }
  
  let persistent_execution = azimuth::SagaManager::execute_saga(
    saga_manager, 
    "order-processing-saga", 
    persistent_saga_data
  )
  
  assert_true(persistent_execution.success)
  
  // Simulate saga restart and recovery
  let recovery_execution = azimuth::SagaManager::recover_saga(
    saga_manager, 
    persistent_execution.execution_id
  )
  
  assert_true(recovery_execution.success)
  assert_eq(recovery_execution.status, "completed")
  
  // Test concurrent saga execution
  let concurrent_saga_data = [
    {
      "user_id": "concurrent-user-1",
      "order_id": "concurrent-order-1",
      "items": [{"product_id": "prod-1", "quantity": 1}],
      "total": 99.99
    },
    {
      "user_id": "concurrent-user-2", 
      "order_id": "concurrent-order-2",
      "items": [{"product_id": "prod-2", "quantity": 2}],
      "total": 199.99
    },
    {
      "user_id": "concurrent-user-3",
      "order_id": "concurrent-order-3", 
      "items": [{"product_id": "prod-3", "quantity": 1}],
      "total": 149.99
    }
  ]
  
  let concurrent_results = []
  
  for saga_data in concurrent_saga_data {
    let result = azimuth::SagaManager::execute_saga(
      saga_manager, 
      "order-processing-saga", 
      saga_data
    )
    concurrent_results.push(result)
  }
  
  // Verify all concurrent sagas completed successfully
  assert_eq(concurrent_results.filter(|r| r.success).length(), 3)
  
  // Test saga timeout handling
  let timeout_saga_data = {
    "user_id": "timeout-user-789",
    "order_id": "timeout-order-456",
    "items": [{"product_id": "prod-4", "quantity": 1}],
    "total": 89.99,
    "payment_method": "slow_payment" // Will cause timeout
  }
  
  let timeout_execution = azimuth::SagaManager::execute_saga(
    saga_manager, 
    "order-processing-saga", 
    timeout_saga_data,
    {
      "timeout_ms": 5000, // Short timeout for testing
      "auto_compensate": true
    }
  )
  
  assert_false(timeout_execution.success)
  assert_eq(timeout_execution.status, "timed_out")
  assert_true(timeout_execution.compensation_triggered)
  
  // Test saga monitoring and metrics
  let saga_metrics = azimuth::SagaManager::get_saga_metrics(saga_manager)
  
  assert_true(saga_metrics.total_executions > 0)
  assert_true(saga_metrics.successful_executions > 0)
  assert_true(saga_metrics.failed_executions > 0)
  assert_true(saga_metrics.average_execution_time_ms > 0)
  
  // Test saga audit trail
  let audit_trail = azimuth::SagaManager::get_audit_trail(saga_manager)
  
  assert_true(audit_trail.length() > 0)
  
  // Verify audit trail contains all saga executions
  let audit_execution_ids = audit_trail.map(|entry| entry.execution_id).unique()
  assert_true(audit_execution_ids.contains(saga_execution.execution_id))
  assert_true(audit_execution_ids.contains(failing_saga_execution.execution_id))
  assert_true(audit_execution_ids.contains(persistent_execution.execution_id))
  
  // Final consistency check
  let final_consistency_verification = azimuth::SagaManager::verify_saga_consistency(
    saga_manager
  )
  
  assert_true(final_consistency_verification.overall_consistent)
}