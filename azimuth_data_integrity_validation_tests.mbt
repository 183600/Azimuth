// Azimuth 数据完整性验证测试用例
// 测试系统在各种场景下的数据完整性保证和验证机制

test "数据传输完整性校验" {
  // 创建数据完整性验证器
  let integrity_validator = azimuth::DataIntegrityValidator::new()
  
  // 创建测试数据
  let original_data = "这是一段重要的遥测数据，需要确保在传输过程中不被篡改。包含用户ID、时间戳、度量值等关键信息。"
  let data_bytes = original_data.to_bytes()
  
  // 计算校验和
  let checksum = integrity_validator.calculate_checksum(data_bytes, azimuth::ChecksumAlgorithm::SHA256)
  assert_true(checksum.length() > 0)
  
  // 模拟数据传输
  let transmitted_data = integrity_validator.transmit_with_validation(
    data_bytes,
    checksum,
    azimuth::TransmissionProtocol::TCP
  )
  
  // 验证接收到的数据完整性
  let received_data = transmitted_data.data
  let received_checksum = transmitted_data.checksum
  
  // 验证校验和匹配
  let calculated_checksum = integrity_validator.calculate_checksum(received_data, azimuth::ChecksumAlgorithm::SHA256)
  assert_eq(calculated_checksum, received_checksum)
  
  // 验证数据内容一致
  let received_string = String::from_bytes(received_data)
  assert_eq(received_string, original_data)
  
  // 测试数据篡改检测
  let tampered_data = received_data.slice(0, received_data.length() - 1) + [0] // 修改最后一个字节
  let tampered_checksum = integrity_validator.calculate_checksum(tampered_data, azimuth::ChecksumAlgorithm::SHA256)
  
  assert_ne(tampered_checksum, original_checksum)
  
  // 验证篡改检测
  let is_tampered = integrity_validator.verify_integrity(tampered_data, checksum)
  assert_false(is_tampered)
}

test "序列化和反序列化完整性" {
  // 创建序列化完整性验证器
  let serialization_validator = azimuth::SerializationIntegrityValidator::new()
  
  // 创建复杂遥测数据结构
  let telemetry_data = azimuth::TelemetryData::new()
  telemetry_data.set_trace_id("trace-12345")
  telemetry_data.set_span_id("span-67890")
  telemetry_data.set_timestamp(1609459200000L)
  telemetry_data.set_service_name("user-service")
  telemetry_data.set_operation_name("get_user_profile")
  
  // 添加属性
  telemetry_data.add_attribute("user_id", "user-001")
  telemetry_data.add_attribute("request_id", "req-12345")
  telemetry_data.add_attribute("response_time_ms", "150")
  telemetry_data.add_attribute("status_code", "200")
  
  // 添加事件
  telemetry_data.add_event("operation_started", 1609459200000L, [("component", "service-handler")])
  telemetry_data.add_event("database_query", 1609459200050L, [("query", "SELECT * FROM users WHERE id = ?")])
  telemetry_data.add_event("operation_completed", 1609459200150L, [("duration_ms", "150")])
  
  // 序列化数据
  let serialized_data = serialization_validator.serialize_with_validation(
    telemetry_data,
    azimuth::SerializationFormat::JSON,
    azimuth::CompressionAlgorithm::GZIP
  )
  
  // 验证序列化结果
  assert_true(serialized_data.data.length() > 0)
  assert_true(serialized_data.checksum.length() > 0)
  assert_true(serialized_data.metadata.contains_key("format"))
  assert_true(serialized_data.metadata.contains_key("compression"))
  assert_true(serialized_data.metadata.contains_key("original_size"))
  
  // 反序列化数据
  let deserialization_result = serialization_validator.deserialize_with_validation(
    serialized_data.data,
    serialized_data.checksum,
    azimuth::SerializationFormat::JSON,
    azimuth::CompressionAlgorithm::GZIP
  )
  
  // 验证反序列化成功
  assert_true(deserialization_result.is_success)
  
  let deserialized_data = deserialization_result.data
  assert_eq(deserialized_data.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized_data.span_id, telemetry_data.span_id)
  assert_eq(deserialized_data.timestamp, telemetry_data.timestamp)
  assert_eq(deserialized_data.service_name, telemetry_data.service_name)
  assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
  
  // 验证属性完整性
  assert_eq(deserialized_data.attributes.length(), telemetry_data.attributes.length())
  for attr in telemetry_data.attributes {
    assert_eq(deserialized_data.get_attribute(attr.key), Some(attr.value))
  }
  
  // 验证事件完整性
  assert_eq(deserialized_data.events.length(), telemetry_data.events.length())
  for i in 0..telemetry_data.events.length() - 1 {
    assert_eq(deserialized_data.events[i].name, telemetry_data.events[i].name)
    assert_eq(deserialized_data.events[i].timestamp, telemetry_data.events[i].timestamp)
    assert_eq(deserialized_data.events[i].attributes.length(), telemetry_data.events[i].attributes.length())
  }
  
  // 测试序列化数据篡改检测
  let tampered_serialized = serialized_data.data.slice(0, serialized_data.data.length() - 1) + [0]
  let tampered_deserialization = serialization_validator.deserialize_with_validation(
    tampered_serialized,
    serialized_data.checksum,
    azimuth::SerializationFormat::JSON,
    azimuth::CompressionAlgorithm::GZIP
  )
  
  // 验证篡改检测
  assert_false(tampered_deserialization.is_success)
  assert_true(tampered_deserialization.error.contains("checksum") || tampered_deserialization.error.contains("integrity"))
}

test "数据库事务完整性" {
  // 创建数据库完整性验证器
  let db_validator = azimuth::DatabaseIntegrityValidator::new()
  
  // 初始化测试数据库
  let test_db = db_validator.create_test_database("integrity_test_db")
  
  // 创建表结构
  let create_table_sql = "
    CREATE TABLE orders (
      id VARCHAR(36) PRIMARY KEY,
      user_id VARCHAR(36) NOT NULL,
      product_id VARCHAR(36) NOT NULL,
      quantity INTEGER NOT NULL,
      unit_price DECIMAL(10,2) NOT NULL,
      total_amount DECIMAL(10,2) NOT NULL,
      status VARCHAR(20) NOT NULL,
      created_at TIMESTAMP NOT NULL,
      updated_at TIMESTAMP NOT NULL,
      version INTEGER NOT NULL DEFAULT 1,
      CONSTRAINT chk_quantity_positive CHECK (quantity > 0),
      CONSTRAINT chk_unit_price_positive CHECK (unit_price > 0),
      CONSTRAINT chk_total_amount_positive CHECK (total_amount > 0)
    );
    
    CREATE TABLE order_items (
      id VARCHAR(36) PRIMARY KEY,
      order_id VARCHAR(36) NOT NULL,
      product_id VARCHAR(36) NOT NULL,
      quantity INTEGER NOT NULL,
      unit_price DECIMAL(10,2) NOT NULL,
      total_price DECIMAL(10,2) NOT NULL,
      created_at TIMESTAMP NOT NULL,
      FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE
    );
    
    CREATE TABLE inventory (
      id VARCHAR(36) PRIMARY KEY,
      product_id VARCHAR(36) NOT NULL UNIQUE,
      available_quantity INTEGER NOT NULL,
      reserved_quantity INTEGER NOT NULL DEFAULT 0,
      version INTEGER NOT NULL DEFAULT 1,
      updated_at TIMESTAMP NOT NULL,
      CONSTRAINT chk_available_non_negative CHECK (available_quantity >= 0),
      CONSTRAINT chk_reserved_non_negative CHECK (reserved_quantity >= 0)
    );
  "
  
  db_validator.execute_sql(test_db, create_table_sql)
  
  // 插入初始数据
  let insert_inventory_sql = "
    INSERT INTO inventory (id, product_id, available_quantity, updated_at) VALUES
    ('inv-001', 'prod-001', 100, CURRENT_TIMESTAMP),
    ('inv-002', 'prod-002', 50, CURRENT_TIMESTAMP),
    ('inv-003', 'prod-003', 200, CURRENT_TIMESTAMP);
  "
  
  db_validator.execute_sql(test_db, insert_inventory_sql)
  
  // 测试事务完整性
  let transaction = db_validator.begin_transaction(test_db)
  
  try {
    // 步骤1：创建订单
    let order_id = "order-" + azimuth::UUID::generate()
    let insert_order_sql = "
      INSERT INTO orders (id, user_id, product_id, quantity, unit_price, total_amount, status, created_at, updated_at, version)
      VALUES (?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, 1)
    "
    
    db_validator.execute_transaction_sql(transaction, insert_order_sql, [
      order_id,
      "user-001",
      "prod-001",
      2,
      25.50,
      51.00,
      "pending"
    ])
    
    // 步骤2：创建订单项
    let item_id = "item-" + azimuth::UUID::generate()
    let insert_item_sql = "
      INSERT INTO order_items (id, order_id, product_id, quantity, unit_price, total_price, created_at)
      VALUES (?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
    "
    
    db_validator.execute_transaction_sql(transaction, insert_item_sql, [
      item_id,
      order_id,
      "prod-001",
      2,
      25.50,
      51.00
    ])
    
    // 步骤3：更新库存（使用乐观锁）
    let update_inventory_sql = "
      UPDATE inventory 
      SET available_quantity = available_quantity - ?, 
          reserved_quantity = reserved_quantity + ?,
          version = version + 1,
          updated_at = CURRENT_TIMESTAMP
      WHERE product_id = ? AND version = ?
    "
    
    let update_result = db_validator.execute_transaction_sql(transaction, update_inventory_sql, [
      2,  // 减少可用数量
      2,  // 增加预留数量
      "prod-001",
      1   // 当前版本号
    ])
    
    // 验证库存更新成功
    assert_eq(update_result.affected_rows, 1)
    
    // 提交事务
    db_validator.commit_transaction(transaction)
    
  } catch {
    error => {
      // 回滚事务
      db_validator.rollback_transaction(transaction)
      assert_true(false, "事务执行失败: " + error.message)
    }
  }
  
  // 验证事务提交后的数据一致性
  let order_query = "SELECT * FROM orders WHERE id = ?"
  let order_result = db_validator.query_sql(test_db, order_query, [order_id])
  assert_eq(order_result.rows.length(), 1)
  
  let order_row = order_result.rows[0]
  assert_eq(order_row.get("status"), "pending")
  assert_eq(order_row.get("quantity"), 2)
  assert_eq(order_row.get("total_amount"), 51.00)
  
  let item_query = "SELECT * FROM order_items WHERE order_id = ?"
  let item_result = db_validator.query_sql(test_db, item_query, [order_id])
  assert_eq(item_result.rows.length(), 1)
  
  let item_row = item_result.rows[0]
  assert_eq(item_row.get("quantity"), 2)
  assert_eq(item_row.get("total_price"), 51.00)
  
  let inventory_query = "SELECT * FROM inventory WHERE product_id = ?"
  let inventory_result = db_validator.query_sql(test_db, inventory_query, ["prod-001"])
  assert_eq(inventory_result.rows.length(), 1)
  
  let inventory_row = inventory_result.rows[0]
  assert_eq(inventory_row.get("available_quantity"), 98)  // 100 - 2
  assert_eq(inventory_row.get("reserved_quantity"), 2)   // 0 + 2
  assert_eq(inventory_row.get("version"), 2)             // 版本号递增
  
  // 测试并发事务冲突检测
  let concurrent_transactions = []
  
  for i in 0..=4 {
    let concurrent_tx = db_validator.begin_transaction(test_db)
    concurrent_transactions.push(concurrent_tx)
    
    // 尝试更新同一产品的库存
    let concurrent_update_sql = "
      UPDATE inventory 
      SET available_quantity = available_quantity - 1,
          version = version + 1,
          updated_at = CURRENT_TIMESTAMP
      WHERE product_id = 'prod-002' AND version = 1
    "
    
    let result = db_validator.execute_transaction_sql(concurrent_tx, concurrent_update_sql, [])
    
    if result.affected_rows > 0 {
      // 更新成功，提交事务
      db_validator.commit_transaction(concurrent_tx)
    } else {
      // 更新失败（版本冲突），回滚事务
      db_validator.rollback_transaction(concurrent_tx)
    }
  }
  
  // 验证最终库存状态（只有一个事务应该成功）
  let final_inventory_query = "SELECT * FROM inventory WHERE product_id = 'prod-002'"
  let final_inventory_result = db_validator.query_sql(test_db, final_inventory_query, [])
  assert_eq(final_inventory_result.rows.length(), 1)
  
  let final_inventory_row = final_inventory_result.rows[0]
  // 由于版本冲突，只有一个事务成功，所以库存应该是49而不是45
  assert_eq(final_inventory_row.get("available_quantity"), 49)
  assert_eq(final_inventory_row.get("version"), 2)
}

test "分布式数据一致性" {
  // 创建分布式一致性验证器
  let consistency_validator = azimuth::DistributedConsistencyValidator::new()
  
  // 初始化分布式节点
  let nodes = [
    consistency_validator.create_node("node-1", "us-east-1"),
    consistency_validator.create_node("node-2", "us-west-2"),
    consistency_validator.create_node("node-3", "eu-west-1")
  ]
  
  // 配置复制策略
  let replication_config = azimuth::ReplicationConfig {
    replication_factor: 3,
    consistency_level: azimuth::ConsistencyLevel::Quorum,
    write_timeout_ms: 5000,
    read_timeout_ms: 3000
  }
  
  // 测试分布式写入
  let write_key = "user-12345"
  let write_value = azimuth::UserProfile::new()
  write_value.set_user_id("user-12345")
  write_value.set_name("张三")
  write_value.set_email("zhangsan@example.com")
  write_value.set_created_at(1609459200000L)
  
  // 执行分布式写入
  let write_result = consistency_validator.distributed_write(
    nodes,
    write_key,
    write_value,
    replication_config
  )
  
  // 验证写入成功
  assert_true(write_result.success)
  assert_eq(write_result.successful_replicas, 3) // 所有节点都成功写入
  assert_eq(write_result.failed_replicas, 0)
  
  // 测试分布式读取
  let read_result = consistency_validator.distributed_read(
    nodes,
    write_key,
    replication_config
  )
  
  // 验证读取成功和数据一致性
  assert_true(read_result.success)
  assert_eq(read_result.consistent_values, 3) // 所有节点返回一致值
  assert_eq(read_result.value.get_user_id(), write_value.get_user_id())
  assert_eq(read_result.value.get_name(), write_value.get_name())
  assert_eq(read_result.value.get_email(), write_value.get_email())
  
  // 测试网络分区场景
  // 模拟node-1网络分区
  consistency_validator.simulate_network_partition("node-1")
  
  // 在分区期间执行写入
  let partition_write_key = "user-67890"
  let partition_write_value = azimuth::UserProfile::new()
  partition_write_value.set_user_id("user-67890")
  partition_write_value.set_name("李四")
  partition_write_value.set_email("lisi@example.com")
  
  let partition_write_result = consistency_validator.distributed_write(
    nodes,
    partition_write_key,
    partition_write_value,
    replication_config
  )
  
  // 由于网络分区，写入应该失败（无法达到法定数量）
  assert_false(partition_write_result.success)
  assert_eq(partition_write_result.successful_replicas, 2) // 只有node-2和node-3成功
  assert_eq(partition_write_result.failed_replicas, 1)   // node-1失败
  
  // 测试分区恢复后的数据同步
  consistency_validator.heal_network_partition("node-1")
  
  // 等待数据同步
  consistency_validator.wait_for_data_sync(nodes, 5000) // 最多等待5秒
  
  // 验证数据同步完成
  let sync_read_result = consistency_validator.distributed_read(
    nodes,
    partition_write_key,
    replication_config
  )
  
  // 恢复后应该能读取到一致的数据
  assert_true(sync_read_result.success)
  assert_eq(sync_read_result.consistent_values, 3) // 所有节点现在都有一致的数据
  
  // 测试并发写入冲突解决
  let concurrent_write_key = "counter-001"
  
  // 从多个节点并发写入
  let concurrent_results = []
  
  for i in 0..=2 {
    let node = nodes[i]
    let counter_value = azimuth::Counter::new()
    counter_value.set_key(concurrent_write_key)
    counter_value.set_value((i + 1) * 10)
    counter_value.set_timestamp(azimuth::TimeUtil::current_time_millis())
    
    let result = consistency_validator.distributed_write_to_node(
      node,
      concurrent_write_key,
      counter_value
    )
    concurrent_results.push(result)
  }
  
  // 验证所有写入都成功
  for result in concurrent_results {
    assert_true(result.success)
  }
  
  // 使用冲突解决策略读取最终值
  let conflict_resolution_result = consistency_resolver.resolve_conflicts(
    nodes,
    concurrent_write_key,
    azimuth::ConflictResolutionStrategy::LastWriteWins
  )
  
  // 验证冲突解决成功
  assert_true(conflict_resolution_result.success)
  
  // 验证所有节点最终一致
  let final_consistency_check = consistency_validator.check_eventual_consistency(
    nodes,
    concurrent_write_key
  )
  
  assert_true(final_consistency_check.is_consistent)
  assert_eq(final_consistency_check.consistent_nodes, 3)
}

test "数据备份和恢复完整性" {
  // 创建备份恢复验证器
  let backup_validator = azimuth::BackupRecoveryValidator::new()
  
  // 创建测试数据集
  let test_dataset = backup_validator.create_test_dataset("telemetry_data")
  
  // 生成大量测试数据
  for i in 0..=9999 {
    let telemetry_record = azimuth::TelemetryRecord::new()
    telemetry_record.set_id("record-" + i.to_string())
    telemetry_record.set_timestamp(1609459200000L + (i * 1000L))
    telemetry_record.set_service_name("service-" + (i % 10).to_string())
    telemetry_record.set_operation_name("operation-" + (i % 20).to_string())
    telemetry_record.set_duration_ms(50 + (i % 500))
    telemetry_record.set_status_code(if i % 20 == 0 { 500 } else { 200 })
    
    // 添加随机属性
    telemetry_record.add_attribute("user_id", "user-" + (i % 100).to_string())
    telemetry_record.add_attribute("region", "region-" + (i % 5).to_string())
    telemetry_record.add_attribute("version", "1." + (i % 10).to_string() + ".0")
    
    test_dataset.add_record(telemetry_record)
  }
  
  // 验证原始数据集
  assert_eq(test_dataset.record_count(), 10000)
  
  // 计算原始数据集的校验和
  let original_checksum = backup_validator.calculate_dataset_checksum(test_dataset)
  
  // 创建备份
  let backup_config = azimuth::BackupConfig {
    backup_type: azimuth::BackupType::Full,
    compression_enabled: true,
    encryption_enabled: true,
    encryption_key: "backup-key-12345",
    storage_location: "/backups/telemetry",
    retention_days: 30
  }
  
  let backup_result = backup_validator.create_backup(test_dataset, backup_config)
  
  // 验证备份创建成功
  assert_true(backup_result.success)
  assert_true(backup_result.backup_id.length() > 0)
  assert_true(backup_result.file_path.length() > 0)
  assert_true(backup_result.file_size > 0)
  assert_true(backup_result.checksum.length() > 0)
  
  // 验证备份文件完整性
  let backup_integrity = backup_validator.verify_backup_integrity(
    backup_result.file_path,
    backup_result.checksum
  )
  assert_true(backup_integrity)
  
  // 模拟数据丢失（删除原始数据集）
  backup_validator.simulate_data_loss(test_dataset)
  assert_eq(test_dataset.record_count(), 0)
  
  // 从备份恢复数据
  let recovery_result = backup_validator.restore_from_backup(
    backup_result.backup_id,
    backup_result.file_path,
    backup_result.checksum,
    backup_config.encryption_key
  )
  
  // 验证恢复成功
  assert_true(recovery_result.success)
  assert_eq(recovery_result.restored_record_count, 10000)
  
  // 验证恢复后的数据完整性
  let restored_dataset = recovery_result.dataset
  assert_eq(restored_dataset.record_count(), 10000)
  
  // 计算恢复数据集的校验和
  let restored_checksum = backup_validator.calculate_dataset_checksum(restored_dataset)
  assert_eq(restored_checksum, original_checksum)
  
  // 验证恢复数据的随机采样
  let sample_records = [1000, 2500, 5000, 7500, 9000] // 随机选择几个索引
  for index in sample_records {
    let original_record = test_dataset.get_record_by_index(index)
    let restored_record = restored_dataset.get_record_by_index(index)
    
    assert_eq(original_record.get_id(), restored_record.get_id())
    assert_eq(original_record.get_timestamp(), restored_record.get_timestamp())
    assert_eq(original_record.get_service_name(), restored_record.get_service_name())
    assert_eq(original_record.get_operation_name(), restored_record.get_operation_name())
    assert_eq(original_record.get_duration_ms(), restored_record.get_duration_ms())
    assert_eq(original_record.get_status_code(), restored_record.get_status_code())
  }
  
  // 测试增量备份和恢复
  // 添加新数据到恢复的数据集
  for i in 10000..=10499 {
    let new_record = azimuth::TelemetryRecord::new()
    new_record.set_id("record-" + i.to_string())
    new_record.set_timestamp(1609459200000L + (i * 1000L))
    new_record.set_service_name("new-service-" + (i % 3).to_string())
    new_record.set_operation_name("new-operation-" + (i % 5).to_string())
    new_record.set_duration_ms(25 + (i % 200))
    new_record.set_status_code(200)
    
    restored_dataset.add_record(new_record)
  }
  
  // 创建增量备份
  let incremental_backup_config = azimuth::BackupConfig {
    backup_type: azimuth::BackupType::Incremental,
    compression_enabled: true,
    encryption_enabled: true,
    encryption_key: "backup-key-12345",
    storage_location: "/backups/telemetry",
    retention_days: 30,
    base_backup_id: backup_result.backup_id
  }
  
  let incremental_backup_result = backup_validator.create_backup(
    restored_dataset,
    incremental_backup_config
  )
  
  // 验证增量备份成功
  assert_true(incremental_backup_result.success)
  assert_eq(incremental_backup_result.incremental_record_count, 500) // 新增500条记录
  
  // 再次模拟数据丢失
  backup_validator.simulate_data_loss(restored_dataset)
  
  // 从完整备份和增量备份恢复
  let full_recovery_result = backup_validator.restore_from_multiple_backups([
    backup_result,
    incremental_backup_result
  ])
  
  // 验证完全恢复成功
  assert_true(full_recovery_result.success)
  assert_eq(full_recovery_result.restored_record_count(), 10500) // 10000 + 500
}

test "数据版本控制和迁移完整性" {
  // 创建版本控制验证器
  let version_validator = azimuth::VersionControlValidator::new()
  
  // 定义数据模型版本
  let v1_schema = azimuth::DataSchema::new("user_profile", "1.0")
  v1_schema.add_field("user_id", azimuth::FieldType::String, true)
  v1_schema.add_field("name", azimuth::FieldType::String, true)
  v1_schema.add_field("email", azimuth::FieldType::String, true)
  v1_schema.add_field("created_at", azimuth::FieldType::Timestamp, true)
  
  let v2_schema = azimuth::DataSchema::new("user_profile", "2.0")
  v2_schema.add_field("user_id", azimuth::FieldType::String, true)
  v2_schema.add_field("name", azimuth::FieldType::String, true)
  v2_schema.add_field("email", azimuth::FieldType::String, true)
  v2_schema.add_field("created_at", azimuth::FieldType::Timestamp, true)
  v2_schema.add_field("updated_at", azimuth::FieldType::Timestamp, false) // 新增字段
  v2_schema.add_field("phone", azimuth::FieldType::String, false)        // 新增字段
  v2_schema.remove_field("name")                                          // 删除字段
  v2_schema.add_field("first_name", azimuth::FieldType::String, true)    // 拆分字段
  v2_schema.add_field("last_name", azimuth::FieldType::String, true)     // 拆分字段
  
  // 注册模式版本
  version_validator.register_schema(v1_schema)
  version_validator.register_schema(v2_schema)
  
  // 创建V1版本的数据
  let v1_data = azimuth::UserData::new("1.0")
  v1_data.set_field("user_id", "user-001")
  v1_data.set_field("name", "张三")
  v1_data.set_field("email", "zhangsan@example.com")
  v1_data.set_field("created_at", 1609459200000L)
  
  // 验证V1数据符合V1模式
  let v1_validation = version_validator.validate_data(v1_data, "1.0")
  assert_true(v1_validation.is_valid)
  assert_eq(v1_validation.errors.length(), 0)
  
  // 验证V1数据不符合V2模式
  let v1_to_v2_validation = version_validator.validate_data(v1_data, "2.0")
  assert_false(v1_to_v2_validation.is_valid)
  assert_true(v1_to_v2_validation.errors.length() > 0)
  
  // 定义迁移策略
  let migration_strategy = azimuth::MigrationStrategy::new("1.0", "2.0")
  migration_strategy.add_transformation("name", ["first_name", "last_name"], azimuth::TransformationType::Split)
  migration_strategy.add_default_value("updated_at", 1609459200000L)
  migration_strategy.add_default_value("phone", "")
  
  // 执行数据迁移
  let migration_result = version_validator.migrate_data(v1_data, migration_strategy)
  
  // 验证迁移成功
  assert_true(migration_result.success)
  
  let v2_data = migration_result.migrated_data
  assert_eq(v2_data.get_version(), "2.0")
  assert_eq(v2_data.get_field("user_id"), "user-001")
  assert_eq(v2_data.get_field("first_name"), "张")
  assert_eq(v2_data.get_field("last_name"), "三")
  assert_eq(v2_data.get_field("email"), "zhangsan@example.com")
  assert_eq(v2_data.get_field("created_at"), 1609459200000L)
  assert_eq(v2_data.get_field("updated_at"), 1609459200000L)
  assert_eq(v2_data.get_field("phone"), "")
  
  // 验证迁移后的数据符合V2模式
  let v2_validation = version_validator.validate_data(v2_data, "2.0")
  assert_true(v2_validation.is_valid)
  assert_eq(v2_validation.errors.length(), 0)
  
  // 测试批量迁移
  let v1_dataset = azimuth::DataSet::new("user_profiles", "1.0")
  
  // 创建多个V1版本的数据记录
  let names = ["李四", "王五", "赵六", "钱七", "孙八"]
  for i in 0..=4 {
    let record = azimuth::UserData::new("1.0")
    record.set_field("user_id", "user-00" + (i + 2).to_string())
    record.set_field("name", names[i])
    record.set_field("email", names[i] + "@example.com")
    record.set_field("created_at", 1609459200000L + (i * 1000L))
    
    v1_dataset.add_record(record)
  }
  
  // 执行批量迁移
  let batch_migration_result = version_validator.migrate_dataset(v1_dataset, migration_strategy)
  
  // 验证批量迁移成功
  assert_true(batch_migration_result.success)
  assert_eq(batch_migration_result.migrated_count, 5)
  assert_eq(batch_migration_result.failed_count, 0)
  
  let v2_dataset = batch_migration_result.migrated_dataset
  assert_eq(v2_dataset.get_version(), "2.0")
  assert_eq(v2_dataset.record_count(), 5)
  
  // 验证批量迁移后的数据完整性
  for i in 0..=4 {
    let record = v2_dataset.get_record(i)
    assert_eq(record.get_field("user_id"), "user-00" + (i + 2).to_string())
    assert_eq(record.get_field("email"), names[i] + "@example.com")
    
    // 验证姓名拆分正确
    let full_name = names[i]
    let first_name = record.get_field("first_name")
    let last_name = record.get_field("last_name")
    
    assert_eq(first_name + last_name, full_name)
    
    // 验证新字段有默认值
    assert_eq(record.get_field("updated_at"), 1609459200000L)
    assert_eq(record.get_field("phone"), "")
  }
  
  // 测试回滚迁移
  let rollback_strategy = azimuth::MigrationStrategy::new("2.0", "1.0")
  rollback_strategy.add_transformation(["first_name", "last_name"], "name", azimuth::TransformationType::Merge)
  rollback_strategy.remove_field("updated_at")
  rollback_strategy.remove_field("phone")
  
  let rollback_result = version_validator.migrate_data(v2_data, rollback_strategy)
  
  // 验证回滚成功
  assert_true(rollback_result.success)
  
  let rolled_back_data = rollback_result.migrated_data
  assert_eq(rolled_back_data.get_version(), "1.0")
  assert_eq(rolled_back_data.get_field("user_id"), "user-001")
  assert_eq(rolled_back_data.get_field("name"), "张三") // 姓名重新合并
  assert_eq(rolled_back_data.get_field("email"), "zhangsan@example.com")
  assert_eq(rolled_back_data.get_field("created_at"), 1609459200000L)
  
  // 验证回滚后的数据符合V1模式
  let rollback_validation = version_validator.validate_data(rolled_back_data, "1.0")
  assert_true(rollback_validation.is_valid)
  assert_eq(rollback_validation.errors.length(), 0)
}