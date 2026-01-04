// Azimuth Database Operations Test Suite
// 数据库操作测试套件 - 专注于数据库CRUD、事务处理和查询优化

// 测试1: 基本CRUD操作
test "基本CRUD操作测试" {
  // 创建内存数据库连接
  let db_connection = DatabaseConnection::new()
  DatabaseConnection::set_database_type(db_connection, DatabaseType::SQLite)
  DatabaseConnection::set_connection_string(db_connection, ":memory:")
  
  // 连接数据库
  let connect_result = DatabaseConnection::connect(db_connection)
  assert_true(connect_result.success)
  
  // 创建遥测数据表
  let create_table_sql = "
    CREATE TABLE telemetry_spans (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      trace_id TEXT NOT NULL,
      span_id TEXT NOT NULL,
      parent_span_id TEXT,
      operation_name TEXT NOT NULL,
      service_name TEXT NOT NULL,
      start_time INTEGER NOT NULL,
      end_time INTEGER NOT NULL,
      status TEXT NOT NULL,
      duration_ms INTEGER,
      attributes TEXT,
      created_at INTEGER DEFAULT (strftime('%s', 'now'))
    )
  "
  
  let create_result = DatabaseConnection::execute(db_connection, create_table_sql)
  assert_true(create_result.success)
  assert_eq(create_result.affected_rows, 0)
  
  // 插入测试数据
  let insert_sql = "
    INSERT INTO telemetry_spans 
    (trace_id, span_id, parent_span_id, operation_name, service_name, start_time, end_time, status, duration_ms, attributes)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "
  
  let telemetry_spans = [
    ("trace-001", "span-001", None, "http.request", "azimuth-telemetry", 1640995200000, 1640995200500, "ok", 500, "{\"http.method\": \"GET\", \"http.url\": \"/api/telemetry\"}"),
    ("trace-001", "span-002", Some("span-001"), "db.query", "azimuth-telemetry", 1640995200100, 1640995200300, "ok", 200, "{\"db.statement\": \"SELECT * FROM metrics\"}"),
    ("trace-002", "span-003", None, "cache.get", "azimuth-telemetry", 1640995201000, 1640995201050, "miss", 50, "{\"cache.key\": \"user:123\"}"),
    ("trace-002", "span-004", Some("span-003"), "db.query", "azimuth-telemetry", 1640995201100, 1640995201400, "error", 300, "{\"db.statement\": \"SELECT * FROM users\", \"error\": \"timeout\"}"),
    ("trace-003", "span-005", None, "api.call", "azimuth-telemetry", 1640995202000, 1640995202300, "ok", 300, "{\"api.endpoint\": \"/external/service\", \"http.status_code\": 200}")
  ]
  
  let mut inserted_ids = []
  for (trace_id, span_id, parent_span_id, operation_name, service_name, start_time, end_time, status, duration_ms, attributes) in telemetry_spans {
    let params = [
      DatabaseValue::Text(trace_id),
      DatabaseValue::Text(span_id),
      match parent_span_id {
        Some(id) => DatabaseValue::Text(id),
        None => DatabaseValue::Null
      },
      DatabaseValue::Text(operation_name),
      DatabaseValue::Text(service_name),
      DatabaseValue::Integer(start_time),
      DatabaseValue::Integer(end_time),
      DatabaseValue::Text(status),
      DatabaseValue::Integer(duration_ms),
      DatabaseValue::Text(attributes)
    ]
    
    let insert_result = DatabaseConnection::execute_with_params(db_connection, insert_sql, params)
    assert_true(insert_result.success)
    assert_eq(insert_result.affected_rows, 1)
    
    inserted_ids.push(insert_result.last_insert_id)
  }
  
  assert_eq(inserted_ids.length(), 5)
  
  // 查询所有数据
  let select_all_sql = "SELECT * FROM telemetry_spans ORDER BY id"
  let select_result = DatabaseConnection::query(db_connection, select_all_sql)
  
  assert_true(select_result.success)
  assert_eq(select_result.rows.length(), 5)
  
  // 验证查询结果
  let first_row = select_result.rows[0]
  assert_eq(first_row.get_integer("id"), inserted_ids[0])
  assert_eq(first_row.get_text("trace_id"), "trace-001")
  assert_eq(first_row.get_text("span_id"), "span-001")
  assert_eq(first_row.get_text("operation_name"), "http.request")
  assert_eq(first_row.get_integer("duration_ms"), 500)
  
  // 按条件查询
  let select_by_trace_sql = "SELECT * FROM telemetry_spans WHERE trace_id = ?"
  let trace_params = [DatabaseValue::Text("trace-001")]
  let trace_result = DatabaseConnection::query_with_params(db_connection, select_by_trace_sql, trace_params)
  
  assert_true(trace_result.success)
  assert_eq(trace_result.rows.length(), 2) // trace-001有2个span
  
  // 更新数据
  let update_sql = "UPDATE telemetry_spans SET status = ? WHERE id = ?"
  let update_params = [DatabaseValue::Text("updated"), DatabaseValue::Integer(inserted_ids[0])]
  let update_result = DatabaseConnection::execute_with_params(db_connection, update_sql, update_params)
  
  assert_true(update_result.success)
  assert_eq(update_result.affected_rows, 1)
  
  // 验证更新结果
  let verify_sql = "SELECT status FROM telemetry_spans WHERE id = ?"
  let verify_params = [DatabaseValue::Integer(inserted_ids[0])]
  let verify_result = DatabaseConnection::query_with_params(db_connection, verify_sql, verify_params)
  
  assert_true(verify_result.success)
  assert_eq(verify_result.rows[0].get_text("status"), "updated")
  
  // 删除数据
  let delete_sql = "DELETE FROM telemetry_spans WHERE id = ?"
  let delete_params = [DatabaseValue::Integer(inserted_ids[4])]
  let delete_result = DatabaseConnection::execute_with_params(db_connection, delete_sql, delete_params)
  
  assert_true(delete_result.success)
  assert_eq(delete_result.affected_rows, 1)
  
  // 验证删除结果
  let count_sql = "SELECT COUNT(*) as count FROM telemetry_spans"
  let count_result = DatabaseConnection::query(db_connection, count_sql)
  
  assert_true(count_result.success)
  assert_eq(count_result.rows[0].get_integer("count"), 4)
  
  // 关闭数据库连接
  let close_result = DatabaseConnection::close(db_connection)
  assert_true(close_result.success)
}

// 测试2: 事务处理
test "事务处理测试" {
  // 创建数据库连接
  let db_connection = DatabaseConnection::new()
  DatabaseConnection::set_database_type(db_connection, DatabaseType::PostgreSQL)
  DatabaseConnection::set_connection_string(db_connection, "postgresql://user:password@localhost:5432/azimuth_test")
  
  let connect_result = DatabaseConnection::connect(db_connection)
  assert_true(connect_result.success)
  
  // 创建测试表
  let create_table_sql = "
    CREATE TABLE IF NOT EXISTS telemetry_metrics (
      id SERIAL PRIMARY KEY,
      metric_name TEXT NOT NULL,
      metric_value DOUBLE PRECISION NOT NULL,
      metric_unit TEXT,
      timestamp INTEGER NOT NULL,
      tags TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  "
  
  let create_result = DatabaseConnection::execute(db_connection, create_table_sql)
  assert_true(create_result.success)
  
  // 开始事务
  let transaction = DatabaseConnection::begin_transaction(db_connection)
  assert_true(transaction.is_active)
  
  // 在事务中插入数据
  let insert_sql = "
    INSERT INTO telemetry_metrics (metric_name, metric_value, metric_unit, timestamp, tags)
    VALUES (?, ?, ?, ?, ?)
  "
  
  let metrics_data = [
    ("cpu.usage", 75.5, "percent", 1640995200000, "{\"host\": \"server-01\", \"region\": \"us-west\"}"),
    ("memory.usage", 60.2, "percent", 1640995200000, "{\"host\": \"server-01\", \"region\": \"us-west\"}"),
    ("disk.usage", 45.8, "percent", 1640995200000, "{\"host\": \"server-01\", \"region\": \"us-west\"}"),
    ("network.throughput", 1024.5, "mbps", 1640995200000, "{\"host\": \"server-01\", \"region\": \"us-west\"}")
  ]
  
  let mut inserted_count = 0
  for (metric_name, metric_value, metric_unit, timestamp, tags) in metrics_data {
    let params = [
      DatabaseValue::Text(metric_name),
      DatabaseValue::Float(metric_value),
      DatabaseValue::Text(metric_unit),
      DatabaseValue::Integer(timestamp),
      DatabaseValue::Text(tags)
    ]
    
    let insert_result = DatabaseConnection::execute_with_params_in_transaction(transaction, insert_sql, params)
    assert_true(insert_result.success)
    assert_eq(insert_result.affected_rows, 1)
    
    inserted_count = inserted_count + 1
  }
  
  assert_eq(inserted_count, 4)
  
  // 在事务中查询数据
  let count_sql = "SELECT COUNT(*) as count FROM telemetry_metrics"
  let count_result = DatabaseConnection::query_in_transaction(transaction, count_sql)
  
  assert_true(count_result.success)
  assert_eq(count_result.rows[0].get_integer("count"), 4)
  
  // 提交事务
  let commit_result = DatabaseConnection::commit_transaction(transaction)
  assert_true(commit_result.success)
  assert_false(transaction.is_active)
  
  // 验证事务提交后的数据
  let post_commit_count_result = DatabaseConnection::query(db_connection, count_sql)
  assert_true(post_commit_count_result.success)
  assert_eq(post_commit_count_result.rows[0].get_integer("count"), 4)
  
  // 测试事务回滚
  let rollback_transaction = DatabaseConnection::begin_transaction(db_connection)
  
  // 插入一些数据
  let rollback_params = [
    DatabaseValue::Text("test.metric"),
    DatabaseValue::Float(99.9),
    DatabaseValue::Text("percent"),
    DatabaseValue::Integer(1640995300000),
    DatabaseValue::Text("{\"test\": true}")
  ]
  
  let rollback_insert_result = DatabaseConnection::execute_with_params_in_transaction(rollback_transaction, insert_sql, rollback_params)
  assert_true(rollback_insert_result.success)
  
  // 在事务中检查数据
  let rollback_count_result = DatabaseConnection::query_in_transaction(rollback_transaction, count_sql)
  assert_true(rollback_count_result.success)
  assert_eq(rollback_count_result.rows[0].get_integer("count"), 5)
  
  // 回滚事务
  let rollback_result = DatabaseConnection::rollback_transaction(rollback_transaction)
  assert_true(rollback_result.success)
  assert_false(rollback_transaction.is_active)
  
  // 验证回滚后的数据
  let post_rollback_count_result = DatabaseConnection::query(db_connection, count_sql)
  assert_true(post_rollback_count_result.success)
  assert_eq(post_rollback_count_result.rows[0].get_integer("count"), 4) // 回滚后应该还是4条
  
  // 测试嵌套事务（保存点）
  let savepoint_transaction = DatabaseConnection::begin_transaction(db_connection)
  
  // 插入第一条数据
  let savepoint1_params = [
    DatabaseValue::Text("savepoint.metric1"),
    DatabaseValue::Float(10.0),
    DatabaseValue::Text("count"),
    DatabaseValue::Integer(1640995400000),
    DatabaseValue::Text("{\"savepoint\": \"1\"}")
  ]
  
  let savepoint1_result = DatabaseConnection::execute_with_params_in_transaction(savepoint_transaction, insert_sql, savepoint1_params)
  assert_true(savepoint1_result.success)
  
  // 创建保存点
  let savepoint = DatabaseConnection::create_savepoint(savepoint_transaction, "sp1")
  assert_true(savepoint.success)
  
  // 插入第二条数据
  let savepoint2_params = [
    DatabaseValue::Text("savepoint.metric2"),
    DatabaseValue::Float(20.0),
    DatabaseValue::Text("count"),
    DatabaseValue::Integer(1640995400000),
    DatabaseValue::Text("{\"savepoint\": \"2\"}")
  ]
  
  let savepoint2_result = DatabaseConnection::execute_with_params_in_transaction(savepoint_transaction, insert_sql, savepoint2_params)
  assert_true(savepoint2_result.success)
  
  // 检查数据数量
  let savepoint_count_result = DatabaseConnection::query_in_transaction(savepoint_transaction, count_sql)
  assert_true(savepoint_count_result.success)
  assert_eq(savepoint_count_result.rows[0].get_integer("count"), 6)
  
  // 回滚到保存点
  let rollback_to_savepoint_result = DatabaseConnection::rollback_to_savepoint(savepoint_transaction, "sp1")
  assert_true(rollback_to_savepoint_result.success)
  
  // 检查回滚后的数据数量
  let post_savepoint_count_result = DatabaseConnection::query_in_transaction(savepoint_transaction, count_sql)
  assert_true(post_savepoint_count_result.success)
  assert_eq(post_savepoint_count_result.rows[0].get_integer("count"), 5) // 回滚到保存点后应该是5条
  
  // 提交事务
  DatabaseConnection::commit_transaction(savepoint_transaction)
  
  // 验证最终数据数量
  let final_count_result = DatabaseConnection::query(db_connection, count_sql)
  assert_true(final_count_result.success)
  assert_eq(final_count_result.rows[0].get_integer("count"), 5)
  
  // 清理测试数据
  let cleanup_sql = "DELETE FROM telemetry_metrics"
  let cleanup_result = DatabaseConnection::execute(db_connection, cleanup_sql)
  assert_true(cleanup_result.success)
  
  // 关闭数据库连接
  DatabaseConnection::close(db_connection)
}

// 测试3: 批量操作
test "批量操作测试" {
  // 创建数据库连接
  let db_connection = DatabaseConnection::new()
  DatabaseConnection::set_database_type(db_connection, DatabaseType::MySQL)
  DatabaseConnection::set_connection_string(db_connection, "mysql://user:password@localhost:3306/azimuth_test")
  
  let connect_result = DatabaseConnection::connect(db_connection)
  assert_true(connect_result.success)
  
  // 创建测试表
  let create_table_sql = "
    CREATE TABLE IF NOT EXISTS telemetry_events (
      id BIGINT AUTO_INCREMENT PRIMARY KEY,
      event_type VARCHAR(50) NOT NULL,
      event_name VARCHAR(100) NOT NULL,
      event_data JSON,
      timestamp BIGINT NOT NULL,
      source_service VARCHAR(100),
      INDEX idx_event_type (event_type),
      INDEX idx_timestamp (timestamp),
      INDEX idx_source_service (source_service)
    )
  "
  
  let create_result = DatabaseConnection::execute(db_connection, create_table_sql)
  assert_true(create_result.success)
  
  // 准备批量插入数据
  let batch_size = 1000
  let batch_events = []
  
  for i in 1..=batch_size {
    let event_type = ["span", "metric", "log", "error", "trace"][i % 5]
    let event_name = event_type + "-" + i.to_string()
    let event_data = {
      "event_id": i,
      "trace_id": "trace-" + ((i / 10) + 1).to_string(),
      "severity": ["info", "warn", "error"][i % 3],
      "attributes": {
        "host": "server-" + ((i % 5) + 1).to_string(),
        "region": ["us-east", "us-west", "eu-west", "ap-southeast"][i % 4]
      }
    }
    let timestamp = 1640995200000 + i * 1000
    let source_service = "azimuth-service-" + ((i % 3) + 1).to_string()
    
    batch_events.push((event_type, event_name, Json::serialize(event_data), timestamp, source_service))
  }
  
  // 测试批量插入
  let batch_insert_start = Clock::now_unix_nanos(Clock::system())
  
  let insert_sql = "
    INSERT INTO telemetry_events (event_type, event_name, event_data, timestamp, source_service)
    VALUES (?, ?, ?, ?, ?)
  "
  
  let batch_params = []
  for (event_type, event_name, event_data, timestamp, source_service) in batch_events {
    let params = [
      DatabaseValue::Text(event_type),
      DatabaseValue::Text(event_name),
      DatabaseValue::Text(event_data),
      DatabaseValue::Integer(timestamp),
      DatabaseValue::Text(source_service)
    ]
    batch_params.push(params)
  }
  
  let batch_insert_result = DatabaseConnection::execute_batch(db_connection, insert_sql, batch_params)
  let batch_insert_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(batch_insert_result.success)
  assert_eq(batch_insert_result.affected_rows, batch_size)
  
  // 验证批量插入性能
  let batch_insert_duration = batch_insert_end - batch_insert_start
  assert_true(batch_insert_duration < 5000000000) // 小于5秒
  
  // 测试批量查询
  let batch_query_start = Clock::now_unix_nanos(Clock::system())
  
  let select_by_types_sql = "
    SELECT event_type, COUNT(*) as count 
    FROM telemetry_events 
    WHERE event_type IN (?, ?, ?)
    GROUP BY event_type
    ORDER BY count DESC
  "
  
  let type_params = [
    DatabaseValue::Text("span"),
    DatabaseValue::Text("metric"),
    DatabaseValue::Text("log")
  ]
  
  let batch_query_result = DatabaseConnection::query_with_params(db_connection, select_by_types_sql, type_params)
  let batch_query_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(batch_query_result.success)
  assert_eq(batch_query_result.rows.length(), 3)
  
  // 验证查询结果
  for row in batch_query_result.rows {
    let event_type = row.get_text("event_type")
    let count = row.get_integer("count")
    
    // 每种类型应该有大约200个事件（1000/5≈200）
    assert_true(count >= 190 && count <= 210)
  }
  
  // 验证批量查询性能
  let batch_query_duration = batch_query_end - batch_query_start
  assert_true(batch_query_duration < 1000000000) // 小于1秒
  
  // 测试批量更新
  let batch_update_start = Clock::now_unix_nanos(Clock::system())
  
  let update_sql = "
    UPDATE telemetry_events 
    SET source_service = ? 
    WHERE id % ? = 0
  "
  
  let update_params = [
    [DatabaseValue::Text("updated-service-1"), DatabaseValue::Integer(2)],
    [DatabaseValue::Text("updated-service-2"), DatabaseValue::Integer(3)],
    [DatabaseValue::Text("updated-service-3"), DatabaseValue::Integer(5)]
  ]
  
  let batch_update_result = DatabaseConnection::execute_batch(db_connection, update_sql, update_params)
  let batch_update_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(batch_update_result.success)
  
  // 验证批量更新结果
  let verify_update_sql = "
    SELECT source_service, COUNT(*) as count 
    FROM telemetry_events 
    WHERE source_service LIKE 'updated-service-%'
    GROUP BY source_service
  "
  
  let verify_update_result = DatabaseConnection::query(db_connection, verify_update_sql)
  assert_true(verify_update_result.success)
  assert_eq(verify_update_result.rows.length(), 3)
  
  // 验证批量更新性能
  let batch_update_duration = batch_update_end - batch_update_start
  assert_true(batch_update_duration < 3000000000) // 小于3秒
  
  // 测试批量删除
  let batch_delete_start = Clock::now_unix_nanos(Clock::system())
  
  let delete_sql = "DELETE FROM telemetry_events WHERE id % ? = 0"
  let delete_params = [
    [DatabaseValue::Integer(7)],
    [DatabaseValue::Integer(11)],
    [DatabaseValue::Integer(13)]
  ]
  
  let batch_delete_result = DatabaseConnection::execute_batch(db_connection, delete_sql, delete_params)
  let batch_delete_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(batch_delete_result.success)
  
  // 验证删除结果
  let remaining_count_sql = "SELECT COUNT(*) as count FROM telemetry_events"
  let remaining_count_result = DatabaseConnection::query(db_connection, remaining_count_sql)
  
  assert_true(remaining_count_result.success)
  let remaining_count = remaining_count_result.rows[0].get_integer("count")
  assert_true(remaining_count < batch_size) // 应该有记录被删除
  
  // 验证批量删除性能
  let batch_delete_duration = batch_delete_end - batch_delete_start
  assert_true(batch_delete_duration < 3000000000) // 小于3秒
  
  // 清理测试数据
  let cleanup_sql = "DELETE FROM telemetry_events"
  let cleanup_result = DatabaseConnection::execute(db_connection, cleanup_sql)
  assert_true(cleanup_result.success)
  
  // 关闭数据库连接
  DatabaseConnection::close(db_connection)
}

// 测试4: 复杂查询和连接
test "复杂查询和连接测试" {
  // 创建数据库连接
  let db_connection = DatabaseConnection::new()
  DatabaseConnection::set_database_type(db_connection, DatabaseType::PostgreSQL)
  DatabaseConnection::set_connection_string(db_connection, "postgresql://user:password@localhost:5432/azimuth_test")
  
  let connect_result = DatabaseConnection::connect(db_connection)
  assert_true(connect_result.success)
  
  // 创建测试表
  let create_spans_sql = "
    CREATE TABLE IF NOT EXISTS spans (
      id SERIAL PRIMARY KEY,
      trace_id TEXT NOT NULL,
      span_id TEXT NOT NULL,
      parent_span_id TEXT,
      operation_name TEXT NOT NULL,
      service_name TEXT NOT NULL,
      start_time INTEGER NOT NULL,
      end_time INTEGER NOT NULL,
      status TEXT NOT NULL,
      duration_ms INTEGER,
      INDEX idx_trace_id (trace_id),
      INDEX idx_service_name (service_name),
      INDEX idx_start_time (start_time)
    )
  "
  
  let create_services_sql = "
    CREATE TABLE IF NOT EXISTS services (
      id SERIAL PRIMARY KEY,
      service_name TEXT UNIQUE NOT NULL,
      version TEXT,
      environment TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  "
  
  let create_attributes_sql = "
    CREATE TABLE IF NOT EXISTS span_attributes (
      id SERIAL PRIMARY KEY,
      span_id TEXT NOT NULL,
      attribute_key TEXT NOT NULL,
      attribute_value TEXT,
      attribute_type TEXT NOT NULL,
      INDEX idx_span_id (span_id),
      INDEX idx_attribute_key (attribute_key)
    )
  "
  
  // 创建表
  DatabaseConnection::execute(db_connection, create_spans_sql)
  DatabaseConnection::execute(db_connection, create_services_sql)
  DatabaseConnection::execute(db_connection, create_attributes_sql)
  
  // 插入测试数据
  let services = [
    ("azimuth-telemetry", "1.0.0", "production"),
    ("azimuth-collector", "1.1.0", "production"),
    ("azimuth-analytics", "2.0.0", "staging"),
    ("user-service", "3.2.1", "production"),
    ("order-service", "2.5.0", "production")
  ]
  
  let insert_service_sql = "INSERT INTO services (service_name, version, environment) VALUES (?, ?, ?)"
  for (service_name, version, environment) in services {
    let params = [
      DatabaseValue::Text(service_name),
      DatabaseValue::Text(version),
      DatabaseValue::Text(environment)
    ]
    DatabaseConnection::execute_with_params(db_connection, insert_service_sql, params)
  }
  
  let spans = [
    ("trace-001", "span-001", None, "http.request", "azimuth-telemetry", 1640995200000, 1640995200500, "ok", 500),
    ("trace-001", "span-002", Some("span-001"), "db.query", "azimuth-telemetry", 1640995200100, 1640995200300, "ok", 200),
    ("trace-002", "span-003", None, "user.auth", "user-service", 1640995201000, 1640995201200, "ok", 200),
    ("trace-002", "span-004", Some("span-003"), "db.query", "user-service", 1640995201100, 1640995201350, "error", 250),
    ("trace-003", "span-005", None, "order.create", "order-service", 1640995202000, 1640995202800, "ok", 800),
    ("trace-003", "span-006", Some("span-005"), "payment.process", "order-service", 1640995202100, 1640995202600, "ok", 500),
    ("trace-004", "span-007", None, "analytics.query", "azimuth-analytics", 1640995203000, 1640995203400, "ok", 400),
    ("trace-005", "span-008", None, "data.collect", "azimuth-collector", 1640995204000, 1640995204200, "ok", 200)
  ]
  
  let insert_span_sql = "
    INSERT INTO spans (trace_id, span_id, parent_span_id, operation_name, service_name, start_time, end_time, status, duration_ms)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
  "
  
  for (trace_id, span_id, parent_span_id, operation_name, service_name, start_time, end_time, status, duration_ms) in spans {
    let params = [
      DatabaseValue::Text(trace_id),
      DatabaseValue::Text(span_id),
      match parent_span_id {
        Some(id) => DatabaseValue::Text(id),
        None => DatabaseValue::Null
      },
      DatabaseValue::Text(operation_name),
      DatabaseValue::Text(service_name),
      DatabaseValue::Integer(start_time),
      DatabaseValue::Integer(end_time),
      DatabaseValue::Text(status),
      DatabaseValue::Integer(duration_ms)
    ]
    DatabaseConnection::execute_with_params(db_connection, insert_span_sql, params)
  }
  
  let attributes = [
    ("span-001", "http.method", "GET", "string"),
    ("span-001", "http.url", "/api/telemetry", "string"),
    ("span-001", "http.status_code", "200", "integer"),
    ("span-002", "db.statement", "SELECT * FROM metrics", "string"),
    ("span-002", "db.rows", "42", "integer"),
    ("span-003", "user.id", "12345", "string"),
    ("span-003", "auth.method", "jwt", "string"),
    ("span-004", "error.message", "Connection timeout", "string"),
    ("span-005", "order.id", "order-12345", "string"),
    ("span-005", "order.amount", "99.99", "float")
  ]
  
  let insert_attribute_sql = "
    INSERT INTO span_attributes (span_id, attribute_key, attribute_value, attribute_type)
    VALUES (?, ?, ?, ?)
  "
  
  for (span_id, attribute_key, attribute_value, attribute_type) in attributes {
    let params = [
      DatabaseValue::Text(span_id),
      DatabaseValue::Text(attribute_key),
      DatabaseValue::Text(attribute_value),
      DatabaseValue::Text(attribute_type)
    ]
    DatabaseConnection::execute_with_params(db_connection, insert_attribute_sql, params)
  }
  
  // 测试内连接查询
  let inner_join_sql = "
    SELECT s.id, s.trace_id, s.operation_name, s.service_name, sv.version, sv.environment
    FROM spans s
    INNER JOIN services sv ON s.service_name = sv.service_name
    WHERE s.status = 'ok'
    ORDER BY s.start_time DESC
    LIMIT 5
  "
  
  let inner_join_result = DatabaseConnection::query(db_connection, inner_join_sql)
  assert_true(inner_join_result.success)
  assert_eq(inner_join_result.rows.length(), 5)
  
  // 验证连接结果
  for row in inner_join_result.rows {
    assert_true(row.get_text("version").length() > 0)
    assert_true(row.get_text("environment").length() > 0)
  }
  
  // 测试左连接查询
  let left_join_sql = "
    SELECT s.id, s.span_id, s.operation_name, COUNT(sa.id) as attribute_count
    FROM spans s
    LEFT JOIN span_attributes sa ON s.span_id = sa.span_id
    GROUP BY s.id, s.span_id, s.operation_name
    ORDER BY attribute_count DESC
  "
  
  let left_join_result = DatabaseConnection::query(db_connection, left_join_sql)
  assert_true(left_join_result.success)
  assert_eq(left_join_result.rows.length(), 8)
  
  // 验证左连接结果
  let first_row = left_join_result.rows[0]
  assert_true(first_row.get_integer("attribute_count") >= 2)
  
  // 测试子查询
  let subquery_sql = "
    SELECT service_name, AVG(duration_ms) as avg_duration, COUNT(*) as span_count
    FROM spans
    WHERE service_name IN (
      SELECT service_name FROM services WHERE environment = 'production'
    )
    GROUP BY service_name
    HAVING COUNT(*) > 1
    ORDER BY avg_duration DESC
  "
  
  let subquery_result = DatabaseConnection::query(db_connection, subquery_sql)
  assert_true(subquery_result.success)
  assert_true(subquery_result.rows.length() >= 2)
  
  // 验证子查询结果
  for row in subquery_result.rows {
    assert_true(row.get_float("avg_duration") > 0)
    assert_true(row.get_integer("span_count") > 1)
  }
  
  // 测试复杂聚合查询
  let aggregate_sql = "
    SELECT 
      DATE_TRUNC('hour', TO_TIMESTAMP(start_time / 1000)) as hour,
      service_name,
      COUNT(*) as total_spans,
      COUNT(CASE WHEN status = 'ok' THEN 1 END) as successful_spans,
      COUNT(CASE WHEN status = 'error' THEN 1 END) as error_spans,
      AVG(duration_ms) as avg_duration,
      MAX(duration_ms) as max_duration,
      MIN(duration_ms) as min_duration
    FROM spans
    WHERE start_time >= ? AND start_time <= ?
    GROUP BY hour, service_name
    ORDER BY hour DESC, total_spans DESC
  "
  
  let aggregate_params = [
    DatabaseValue::Integer(1640995200000),
    DatabaseValue::Integer(1640999999000)
  ]
  
  let aggregate_result = DatabaseConnection::query_with_params(db_connection, aggregate_sql, aggregate_params)
  assert_true(aggregate_result.success)
  assert_true(aggregate_result.rows.length() > 0)
  
  // 验证聚合结果
  for row in aggregate_result.rows {
    assert_true(row.get_integer("total_spans") > 0)
    assert_true(row.get_integer("successful_spans") >= 0)
    assert_true(row.get_integer("error_spans") >= 0)
    assert_true(row.get_float("avg_duration") > 0)
    assert_true(row.get_integer("max_duration") >= row.get_integer("min_duration"))
  }
  
  // 测试窗口函数
  let window_sql = "
    SELECT 
      trace_id,
      span_id,
      operation_name,
      duration_ms,
      ROW_NUMBER() OVER (PARTITION BY trace_id ORDER BY duration_ms DESC) as rank_in_trace,
      LAG(duration_ms) OVER (PARTITION BY trace_id ORDER BY start_time) as prev_duration,
      SUM(duration_ms) OVER (PARTITION BY trace_id ORDER BY start_time ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as cumulative_duration
    FROM spans
    WHERE trace_id IN ('trace-001', 'trace-002', 'trace-003')
    ORDER BY trace_id, start_time
  "
  
  let window_result = DatabaseConnection::query(db_connection, window_sql)
  assert_true(window_result.success)
  assert_true(window_result.rows.length() > 0)
  
  // 验证窗口函数结果
  let trace_groups = window_result.rows.group_by(fn(row) { row.get_text("trace_id") })
  for (trace_id, rows) in trace_groups {
    let mut prev_duration = None
    let mut cumulative_sum = 0
    
    for (i, row) in rows.enumerate() {
      let rank = row.get_integer("rank_in_trace")
      assert_eq(rank, i + 1)
      
      let current_duration = row.get_integer("duration_ms")
      cumulative_sum = cumulative_sum + current_duration
      assert_eq(row.get_integer("cumulative_duration"), cumulative_sum)
      
      if i > 0 {
        assert_eq(row.get_integer("prev_duration"), prev_duration.unwrap())
      }
      
      prev_duration = Some(current_duration)
    }
  }
  
  // 清理测试数据
  DatabaseConnection::execute(db_connection, "DELETE FROM span_attributes")
  DatabaseConnection::execute(db_connection, "DELETE FROM spans")
  DatabaseConnection::execute(db_connection, "DELETE FROM services")
  
  // 关闭数据库连接
  DatabaseConnection::close(db_connection)
}

// 测试5: 数据库连接池
test "数据库连接池测试" {
  // 创建连接池配置
  let pool_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_database_type(pool_config, DatabaseType::PostgreSQL)
  ConnectionPoolConfig::set_connection_string(pool_config, "postgresql://user:password@localhost:5432/azimuth_test")
  ConnectionPoolConfig::set_min_connections(pool_config, 2)
  ConnectionPoolConfig::set_max_connections(pool_config, 10)
  ConnectionPoolConfig::set_connection_timeout(pool_config, 5000) // 5秒
  ConnectionPoolConfig::set_idle_timeout(pool_config, 30000) // 30秒
  ConnectionPoolConfig::set_max_lifetime(pool_config, 3600000) // 1小时
  
  // 创建连接池
  let connection_pool = ConnectionPool::new(pool_config)
  
  // 初始化连接池
  let init_result = ConnectionPool::initialize(connection_pool)
  assert_true(init_result.success)
  
  // 获取连接
  let conn1 = ConnectionPool::get_connection(connection_pool)
  assert_true(conn1.is_some())
  
  let conn2 = ConnectionPool::get_connection(connection_pool)
  assert_true(conn2.is_some())
  
  // 验证连接池状态
  let pool_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(pool_stats.active_connections >= 2)
  assert_true(pool_stats.idle_connections >= 0)
  assert_true(pool_stats.total_connections >= 2)
  
  // 使用连接执行查询
  let connection = conn1.unwrap()
  let test_sql = "SELECT 1 as test_value"
  let query_result = DatabaseConnection::query(connection, test_sql)
  assert_true(query_result.success)
  assert_eq(query_result.rows[0].get_integer("test_value"), 1)
  
  // 归还连接
  let return_result = ConnectionPool::return_connection(connection_pool, connection)
  assert_true(return_result.success)
  
  // 验证归还后的连接池状态
  let returned_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(returned_stats.idle_connections > 0)
  
  // 测试并发连接获取
  let concurrent_connections = []
  let mut concurrent_results = []
  
  // 启动多个并发任务获取连接
  for i in 1..=8 {
    let task = Task::spawn(fn() {
      let conn = ConnectionPool::get_connection(connection_pool)
      if conn.is_some() {
        let connection = conn.unwrap()
        let sql = "SELECT " + i.to_string() + " as task_id"
        let result = DatabaseConnection::query(connection, sql)
        ConnectionPool::return_connection(connection_pool, connection)
        result.success
      } else {
        false
      }
    })
    concurrent_results.push(task)
  }
  
  // 等待所有任务完成
  let mut success_count = 0
  for task in concurrent_results {
    if Task::await(task) {
      success_count = success_count + 1
    }
  }
  
  // 验证并发连接结果
  assert_true(success_count >= 6) // 至少6个任务成功获取连接
  
  // 测试连接池扩展
  let expansion_connections = []
  for i in 1..=5 {
    let conn = ConnectionPool::get_connection(connection_pool)
    if conn.is_some() {
      expansion_connections.push(conn.unwrap())
    }
  }
  
  // 验证连接池扩展
  let expanded_stats = ConnectionPool::get_stats(connection_pool)
  assert_true(expanded_stats.active_connections >= expansion_connections.length())
  
  // 归还扩展连接
  for conn in expansion_connections {
    ConnectionPool::return_connection(connection_pool, conn)
  }
  
  // 测试连接超时
  let timeout_config = ConnectionPoolConfig::new()
  ConnectionPoolConfig::set_database_type(timeout_config, DatabaseType::PostgreSQL)
  ConnectionPoolConfig::set_connection_string(timeout_config, "postgresql://user:password@nonexistent-host:5432/test")
  ConnectionPoolConfig::set_connection_timeout(timeout_config, 1000) // 1秒超时
  
  let timeout_pool = ConnectionPool::new(timeout_config)
  let timeout_init = ConnectionPool::initialize(timeout_pool)
  
  // 由于连接字符串无效，初始化应该失败或超时
  match timeout_init.success {
    true => {
      // 如果初始化成功，尝试获取连接应该超时
      let timeout_conn = ConnectionPool::get_connection(timeout_pool)
      assert_true(timeout_conn.is_none())
    }
    false => assert_true(true) // 初始化失败也是预期的
  }
  
  // 测试连接池清理
  let cleanup_result = ConnectionPool::cleanup_idle_connections(connection_pool)
  assert_true(cleanup_result.success)
  
  // 关闭连接池
  let close_result = ConnectionPool::close(connection_pool)
  assert_true(close_result.success)
  
  // 验证连接池已关闭
  let final_stats = ConnectionPool::get_stats(connection_pool)
  assert_eq(final_stats.active_connections, 0)
  assert_eq(final_stats.idle_connections, 0)
}

// 测试6: 数据库性能优化
test "数据库性能优化测试" {
  // 创建数据库连接
  let db_connection = DatabaseConnection::new()
  DatabaseConnection::set_database_type(db_connection, DatabaseType::PostgreSQL)
  DatabaseConnection::set_connection_string(db_connection, "postgresql://user:password@localhost:5432/azimuth_test")
  
  let connect_result = DatabaseConnection::connect(db_connection)
  assert_true(connect_result.success)
  
  // 创建性能测试表
  let create_performance_table_sql = "
    CREATE TABLE IF NOT EXISTS performance_metrics (
      id BIGSERIAL PRIMARY KEY,
      metric_name VARCHAR(100) NOT NULL,
      metric_value DOUBLE PRECISION NOT NULL,
      metric_timestamp BIGINT NOT NULL,
      host_id VARCHAR(50),
      service_name VARCHAR(100),
      tags JSONB,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  "
  
  let create_result = DatabaseConnection::execute(db_connection, create_performance_table_sql)
  assert_true(create_result.success)
  
  // 创建索引
  let create_indexes_sql = [
    "CREATE INDEX IF NOT EXISTS idx_metrics_name ON performance_metrics(metric_name)",
    "CREATE INDEX IF NOT EXISTS idx_metrics_timestamp ON performance_metrics(metric_timestamp)",
    "CREATE INDEX IF NOT EXISTS idx_metrics_host ON performance_metrics(host_id)",
    "CREATE INDEX IF NOT EXISTS idx_metrics_service ON performance_metrics(service_name)",
    "CREATE INDEX IF NOT EXISTS idx_metrics_tags ON performance_metrics USING GIN(tags)"
  ]
  
  for index_sql in create_indexes_sql {
    DatabaseConnection::execute(db_connection, index_sql)
  }
  
  // 准备大量测试数据
  let data_size = 10000
  let insert_start = Clock::now_unix_nanos(Clock::system())
  
  // 使用批量插入提高性能
  let batch_size = 1000
  let insert_sql = "
    INSERT INTO performance_metrics (metric_name, metric_value, metric_timestamp, host_id, service_name, tags)
    VALUES (?, ?, ?, ?, ?, ?)
  "
  
  for batch in 0..=(data_size / batch_size) {
    let batch_params = []
    let start_index = batch * batch_size
    let end_index = if start_index + batch_size > data_size {
      data_size
    } else {
      start_index + batch_size
    }
    
    for i in start_index..end_index - 1 {
      let metric_name = ["cpu.usage", "memory.usage", "disk.usage", "network.throughput", "response.time"][i % 5]
      let metric_value = 10.0 + (i % 90) + (Random::next_double() * 0.9) // 10-99.9
      let metric_timestamp = 1640995200000 + i * 1000
      let host_id = "host-" + ((i % 10) + 1).to_string()
      let service_name = "service-" + ((i % 5) + 1).to_string()
      let tags = {
        "region": ["us-east", "us-west", "eu-west", "ap-southeast"][i % 4],
        "environment": ["production", "staging"][i % 2],
        "version": "1.0." + ((i % 10) + 1).to_string()
      }
      
      let params = [
        DatabaseValue::Text(metric_name),
        DatabaseValue::Float(metric_value),
        DatabaseValue::Integer(metric_timestamp),
        DatabaseValue::Text(host_id),
        DatabaseValue::Text(service_name),
        DatabaseValue::Text(Json::serialize(tags))
      ]
      batch_params.push(params)
    }
    
    DatabaseConnection::execute_batch(db_connection, insert_sql, batch_params)
  }
  
  let insert_end = Clock::now_unix_nanos(Clock::system())
  let insert_duration = insert_end - insert_start
  
  // 验证批量插入性能
  assert_true(insert_duration < 10000000000) // 小于10秒
  
  // 测试索引性能
  let indexed_query_start = Clock::now_unix_nanos(Clock::system())
  
  let indexed_query_sql = "
    SELECT metric_name, AVG(metric_value) as avg_value, COUNT(*) as count
    FROM performance_metrics
    WHERE metric_timestamp >= ? AND metric_timestamp <= ?
    AND service_name = ?
    GROUP BY metric_name
    ORDER BY avg_value DESC
  "
  
  let indexed_params = [
    DatabaseValue::Integer(1640995200000),
    DatabaseValue::Integer(1640999999000),
    DatabaseValue::Text("service-1")
  ]
  
  let indexed_query_result = DatabaseConnection::query_with_params(db_connection, indexed_query_sql, indexed_params)
  let indexed_query_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(indexed_query_result.success)
  assert_eq(indexed_query_result.rows.length(), 5)
  
  // 验证索引查询性能
  let indexed_query_duration = indexed_query_end - indexed_query_start
  assert_true(indexed_query_duration < 1000000000) // 小于1秒
  
  // 测试复杂查询优化
  let complex_query_start = Clock::now_unix_nanos(Clock::system())
  
  let complex_query_sql = "
    WITH host_metrics AS (
      SELECT 
        host_id,
        AVG(metric_value) as avg_cpu,
        MAX(CASE WHEN metric_name = 'cpu.usage' THEN metric_value END) as max_cpu,
        COUNT(CASE WHEN metric_name = 'cpu.usage' THEN 1 END) as cpu_samples
      FROM performance_metrics
      WHERE metric_name = 'cpu.usage'
      AND metric_timestamp >= ? AND metric_timestamp <= ?
      GROUP BY host_id
    ),
    service_metrics AS (
      SELECT 
        service_name,
        AVG(metric_value) as avg_response_time,
        COUNT(*) as total_samples
      FROM performance_metrics
      WHERE metric_name = 'response.time'
      AND metric_timestamp >= ? AND metric_timestamp <= ?
      GROUP BY service_name
    )
    SELECT 
      h.host_id,
      h.avg_cpu,
      h.max_cpu,
      s.service_name,
      s.avg_response_time,
      s.total_samples
    FROM host_metrics h
    CROSS JOIN service_metrics s
    WHERE h.avg_cpu > 50.0
    ORDER BY h.avg_cpu DESC, s.avg_response_time ASC
    LIMIT 20
  "
  
  let complex_params = [
    DatabaseValue::Integer(1640995200000),
    DatabaseValue::Integer(1640999999000),
    DatabaseValue::Integer(1640995200000),
    DatabaseValue::Integer(1640999999000)
  ]
  
  let complex_query_result = DatabaseConnection::query_with_params(db_connection, complex_query_sql, complex_params)
  let complex_query_end = Clock::now_unix_nanos(Clock::system())
  
  assert_true(complex_query_result.success)
  assert_true(complex_query_result.rows.length() <= 20)
  
  // 验证复杂查询性能
  let complex_query_duration = complex_query_end - complex_query_start
  assert_true(complex_query_duration < 2000000000) // 小于2秒
  
  // 测试查询计划分析
  let explain_sql = "EXPLAIN ANALYZE " + indexed_query_sql
  let explain_result = DatabaseConnection::query_with_params(db_connection, explain_sql, indexed_params)
  
  assert_true(explain_result.success)
  assert_true(explain_result.rows.length() > 0)
  
  // 验证查询计划使用了索引
  let explain_text = explain_result.rows.reduce(fn(acc, row) { 
    acc + row.get_text("QUERY PLAN") + "\n" 
  }, "")
  assert_true(explain_text.contains("Index") || explain_text.contains("Bitmap"))
  
  // 测试预编译语句
  let prepared_stmt = DatabaseConnection::prepare_statement(db_connection, "
    SELECT metric_name, AVG(metric_value) as avg_value
    FROM performance_metrics
    WHERE metric_name = $1 AND metric_timestamp >= $2 AND metric_timestamp <= $3
    GROUP BY metric_name
  ")
  
  assert_true(prepared_stmt.success)
  
  let prepared_start = Clock::now_unix_nanos(Clock::system())
  
  // 使用预编译语句执行多次查询
  let metric_names = ["cpu.usage", "memory.usage", "disk.usage"]
  for metric_name in metric_names {
    let params = [
      DatabaseValue::Text(metric_name),
      DatabaseValue::Integer(1640995200000),
      DatabaseValue::Integer(1640999999000)
    ]
    
    let prepared_result = DatabaseConnection::execute_prepared_statement(prepared_stmt.statement, params)
    assert_true(prepared_result.success)
  }
  
  let prepared_end = Clock::now_unix_nanos(Clock::system())
  let prepared_duration = prepared_end - prepared_start
  
  // 验证预编译语句性能
  assert_true(prepared_duration < 500000000) // 小于500毫秒
  
  // 清理预编译语句
  DatabaseConnection::close_statement(prepared_stmt.statement)
  
  // 清理测试数据
  let cleanup_result = DatabaseConnection::execute(db_connection, "DELETE FROM performance_metrics")
  assert_true(cleanup_result.success)
  
  // 关闭数据库连接
  DatabaseConnection::close(db_connection)
}