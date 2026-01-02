// Azimuth Telemetry System - Premium Quality Test Suite
// 高质量测试用例集合，涵盖多个关键领域

// 测试1: 加密和安全相关功能
test "加密和安全功能测试" {
  // 简单的哈希函数模拟
  let simple_hash = fn(input: String) -> String {
    let chars = input.to_char_array()
    let hash_value = chars.reduce(|acc, char| {
      let char_code = char.to_int()
      (acc * 31 + char_code) % 1000000
    }, 0)
    hash_value.to_string()
  }
  
  // 测试哈希一致性
  let test_data = "azimuth-telemetry-data"
  let hash1 = simple_hash(test_data)
  let hash2 = simple_hash(test_data)
  
  assert_eq(hash1, hash2)
  assert_eq(hash1.length(), 6) // 确保哈希值有固定长度
  
  // 测试不同输入产生不同哈希
  let different_data = "azimuth-telemetry-different"
  let hash3 = simple_hash(different_data)
  assert_not_eq(hash1, hash3)
  
  // 简单的对称加密模拟
  let simple_encrypt = fn(plaintext: String, key: String) -> String {
    let plain_chars = plaintext.to_char_array()
    let key_chars = key.to_char_array()
    
    let encrypted_chars = plain_chars.map_with_index(|i, char| {
      let key_char = key_chars[i % key_chars.length()]
      let plain_code = char.to_int()
      let key_code = key_char.to_int()
      let encrypted_code = (plain_code + key_code) % 256
      encrypted_code.to_char()
    })
    
    encrypted_chars.from_char_array()
  }
  
  let simple_decrypt = fn(ciphertext: String, key: String) -> String {
    let cipher_chars = ciphertext.to_char_array()
    let key_chars = key.to_char_array()
    
    let decrypted_chars = cipher_chars.map_with_index(|i, char| {
      let key_char = key_chars[i % key_chars.length()]
      let cipher_code = char.to_int()
      let key_code = key_char.to_int()
      let decrypted_code = (cipher_code - key_code + 256) % 256
      decrypted_code.to_char()
    })
    
    decrypted_chars.from_char_array()
  }
  
  let secret_key = "azimuth-key-2023"
  let original_message = "secure telemetry data"
  let encrypted = simple_encrypt(original_message, secret_key)
  let decrypted = simple_decrypt(encrypted, secret_key)
  
  assert_eq(original_message, decrypted)
  assert_not_eq(original_message, encrypted)
  
  // 访问控制验证
  let user_permissions = [
    ("admin", ["read", "write", "delete", "manage"]),
    ("operator", ["read", "write"]),
    ("viewer", ["read"])
  ]
  
  let check_permission = fn(user_role: String, required_permission: String) -> Bool {
    match user_permissions.find(|(role, _)| role == user_role) {
      Some((_, permissions)) => permissions.contains(required_permission)
      None => false
    }
  }
  
  assert_true(check_permission("admin", "delete"))
  assert_true(check_permission("operator", "write"))
  assert_false(check_permission("viewer", "write"))
  assert_false(check_permission("guest", "read"))
}

// 测试2: 数据序列化和反序列化
test "数据序列化和反序列化测试" {
  // 简单的JSON序列化模拟
  type JsonValue {
    StringValue(String)
    NumberValue(Int)
    BoolValue(Bool)
    ArrayValue(Array<JsonValue>)
    ObjectValue(Array<(String, JsonValue)>)
    NullValue
  }
  
  let json_to_string = fn(value: JsonValue) -> String {
    match value {
      StringValue(s) => "\"" + s + "\""
      NumberValue(n) => n.to_string()
      BoolValue(b) => if b { "true" } else { "false" }
      NullValue => "null"
      ArrayValue(arr) => {
        let elements = arr.map(json_to_string)
        "[" + elements.reduce(|acc, elem| acc + "," + elem, "") + "]"
      }
      ObjectValue(pairs) => {
        let properties = pairs.map(|(key, val)| {
          "\"" + key + "\":" + json_to_string(val)
        })
        "{" + properties.reduce(|acc, prop| acc + "," + prop, "") + "}"
      }
    }
  }
  
  // 创建复杂的嵌套数据结构
  let telemetry_data = ObjectValue([
    ("timestamp", NumberValue(1640995200)),
    ("service", StringValue("azimuth-collector")),
    ("metrics", ArrayValue([
      ObjectValue([
        ("name", StringValue("cpu_usage")),
        ("value", NumberValue(75)),
        ("unit", StringValue("percent"))
      ]),
      ObjectValue([
        ("name", StringValue("memory_usage")),
        ("value", NumberValue(2048)),
        ("unit", StringValue("mb"))
      ])
    ])),
    ("metadata", ObjectValue([
      ("region", StringValue("us-west-2")),
      ("environment", StringValue("production")),
      ("version", StringValue("1.2.3"))
    ])),
    ("healthy", BoolValue(true))
  ])
  
  let serialized = json_to_string(telemetry_data)
  
  // 验证序列化结果包含关键信息
  assert_true(serialized.contains("azimuth-collector"))
  assert_true(serialized.contains("cpu_usage"))
  assert_true(serialized.contains("memory_usage"))
  assert_true(serialized.contains("us-west-2"))
  assert_true(serialized.contains("production"))
  assert_true(serialized.contains("1.2.3"))
  assert_true(serialized.contains("true"))
  
  // 二进制序列化模拟
  let binary_serialize = fn(data: Array<Int>) -> Array<Int> {
    // 简化版本：添加长度前缀和数据
    let length = data.length()
    let result = [length]
    result + data
  }
  
  let binary_deserialize = fn(binary_data: Array<Int>) -> Array<Int> {
    if binary_data.length() == 0 { [] }
    else {
      let length = binary_data[0]
      binary_data.slice(1, length + 1)
    }
  }
  
  let original_data = [10, 20, 30, 40, 50]
  let binary_data = binary_serialize(original_data)
  let deserialized_data = binary_deserialize(binary_data)
  
  assert_eq(original_data, deserialized_data)
  assert_eq(binary_data.length(), original_data.length() + 1)
  assert_eq(binary_data[0], original_data.length())
}

// 测试3: 网络通信模拟
test "网络通信模拟测试" {
  // 网络包结构模拟
  type NetworkPacket {
    NetworkPacket(String, Array<Int>, Int) // 源地址, 数据, 校验和
  }
  
  let calculate_checksum = fn(data: Array<Int>) -> Int {
    data.reduce(|acc, value| (acc + value) % 65536, 0)
  }
  
  let create_packet = fn(source: String, data: Array<Int>) -> NetworkPacket {
    let checksum = calculate_checksum(data)
    NetworkPacket(source, data, checksum)
  }
  
  let verify_packet = fn(packet: NetworkPacket) -> Bool {
    match packet {
      NetworkPacket(_, data, checksum) => {
        calculate_checksum(data) == checksum
      }
    }
  }
  
  // 创建和验证网络包
  let test_data = [72, 101, 108, 108, 111] // "Hello"的ASCII码
  let packet = create_packet("192.168.1.100", test_data)
  
  assert_true(verify_packet(packet))
  
  // 模拟包损坏
  let corrupted_packet = match packet {
    NetworkPacket(source, data, checksum) => {
      let corrupted_data = [72, 101, 108, 108, 79] // 改变最后一个字节
      NetworkPacket(source, corrupted_data, checksum)
    }
  }
  
  assert_false(verify_packet(corrupted_packet))
  
  // 网络连接池模拟
  type ConnectionPool {
    ConnectionPool(Int, Array<String>, Int) // 最大连接数, 活跃连接, 当前索引
  }
  
  let get_connection = fn(pool: ConnectionPool) -> (ConnectionPool, String) {
    match pool {
      ConnectionPool(max_conn, active_conns, index) => {
        if active_conns.length() < max_conn {
          let new_conn = "conn-" + (active_conns.length() + 1).to_string()
          let new_pool = ConnectionPool(max_conn, active_conns + [new_conn], 0)
          (new_pool, new_conn)
        } else {
          // 复用现有连接
          let conn_index = index % active_conns.length()
          let conn = active_conns[conn_index]
          let new_pool = ConnectionPool(max_conn, active_conns, conn_index + 1)
          (new_pool, conn)
        }
      }
    }
  }
  
  let initial_pool = ConnectionPool(3, [], 0)
  let (pool1, conn1) = get_connection(initial_pool)
  let (pool2, conn2) = get_connection(pool1)
  let (pool3, conn3) = get_connection(pool2)
  let (pool4, conn4) = get_connection(pool3) // 应该复用连接
  
  assert_eq(conn1, "conn-1")
  assert_eq(conn2, "conn-2")
  assert_eq(conn3, "conn-3")
  assert_true(conn4 == "conn-1" || conn4 == "conn-2" || conn4 == "conn-3")
}

// 测试4: 配置管理
test "配置管理测试" {
  // 配置项类型
  type ConfigValue {
    StringConfig(String)
    IntConfig(Int)
    BoolConfig(Bool)
    FloatConfig(Float)
    ArrayConfig(Array<String>)
  }
  
  type ConfigEntry {
    ConfigEntry(String, ConfigValue, String) // 键, 值, 描述
  }
  
  // 默认配置
  let default_configs = [
    ConfigEntry("server.port", IntConfig(8080), "Server listening port"),
    ConfigEntry("server.host", StringConfig("localhost"), "Server host address"),
    ConfigEntry("database.timeout", IntConfig(30), "Database connection timeout in seconds"),
    ConfigEntry("logging.level", StringConfig("info"), "Logging level (debug, info, warn, error)"),
    ConfigEntry("metrics.enabled", BoolConfig(true), "Enable metrics collection"),
    ConfigEntry("cache.ttl", IntConfig(300), "Cache time-to-live in seconds"),
    ConfigEntry("retry.max_attempts", IntConfig(3), "Maximum retry attempts"),
    ConfigEntry("compression.enabled", BoolConfig(true), "Enable data compression")
  ]
  
  // 配置验证
  let validate_config = fn(configs: Array<ConfigEntry>) -> Array<String> {
    let validation_errors = []
    
    // 检查端口范围
    let port_config = configs.find(|entry| {
      match entry {
        ConfigEntry(key, _, _) => key == "server.port"
      }
    })
    
    match port_config {
      Some(ConfigEntry(_, IntConfig(port), _)) => {
        if port < 1 || port > 65535 {
          validation_errors + ["Invalid port number: " + port.to_string()]
        } else { validation_errors }
      }
      _ => validation_errors + ["Port configuration missing or invalid"]
    }
  }
  
  let validation_result = validate_config(default_configs)
  assert_eq(validation_result.length(), 0) // 默认配置应该有效
  
  // 测试无效配置
  let invalid_configs = default_configs.map(|entry| {
    match entry {
      ConfigEntry("server.port", _, desc) => ConfigEntry("server.port", IntConfig(70000), desc)
      _ => entry
    }
  })
  
  let invalid_validation = validate_config(invalid_configs)
  assert_eq(invalid_validation.length(), 1)
  assert_true(invalid_validation[0].contains("Invalid port number"))
  
  // 配置合并
  let merge_configs = fn(base: Array<ConfigEntry>, override: Array<ConfigEntry>) -> Array<ConfigEntry> {
    override.reduce(|acc, override_entry| {
      match override_entry {
        ConfigEntry(key, value, desc) => {
          let filtered = acc.filter(|entry| {
            match entry {
              ConfigEntry(k, _, _) => k != key
            }
          })
          filtered + [override_entry]
        }
      }
    }, base)
  }
  
  let override_configs = [
    ConfigEntry("server.port", IntConfig(9090), "Overridden server port"),
    ConfigEntry("logging.level", StringConfig("debug"), "Debug logging level")
  ]
  
  let merged_configs = merge_configs(default_configs, override_configs)
  
  // 验证合并结果
  let merged_port = merged_configs.find(|entry| {
    match entry {
      ConfigEntry(key, _, _) => key == "server.port"
    }
  })
  
  match merged_port {
    Some(ConfigEntry(_, IntConfig(port), _)) => assert_eq(port, 9090)
    _ => assert_true(false)
  }
  
  let merged_log_level = merged_configs.find(|entry| {
    match entry {
      ConfigEntry(key, _, _) => key == "logging.level"
    }
  })
  
  match merged_log_level {
    Some(ConfigEntry(_, StringConfig(level), _)) => assert_eq(level, "debug")
    _ => assert_true(false)
  }
}

// 测试5: 性能优化
test "性能优化测试" {
  // 内存池模拟
  type MemoryPool {
    MemoryPool(Array<Int>, Int) // 可用块列表, 块大小
  }
  
  let allocate_from_pool = fn(pool: MemoryPool, size: Int) -> (MemoryPool, Option<Int>) {
    match pool {
      MemoryPool(available_blocks, block_size) => {
        if size <= block_size && available_blocks.length() > 0 {
          let block = available_blocks[0]
          let remaining_blocks = available_blocks.slice(1, available_blocks.length())
          (MemoryPool(remaining_blocks, block_size), Some(block))
        } else {
          (pool, None)
        }
      }
    }
  }
  
  let return_to_pool = fn(pool: MemoryPool, block: Int) -> MemoryPool {
    match pool {
      MemoryPool(available_blocks, block_size) => {
        MemoryPool(available_blocks + [block], block_size)
      }
    }
  }
  
  let initial_pool = MemoryPool([1001, 1002, 1003, 1004, 1005], 1024)
  
  // 分配和释放内存块
  let (pool1, block1) = allocate_from_pool(initial_pool, 512)
  let (pool2, block2) = allocate_from_pool(pool1, 1024)
  let (pool3, block3) = allocate_from_pool(pool2, 2048) // 应该失败
  
  match block1 { Some(b) => assert_eq(b, 1001); None => assert_true(false) }
  match block2 { Some(b) => assert_eq(b, 1002); None => assert_true(false) }
  match block3 { Some(_) => assert_true(false); None => assert_true(true) } // 应该失败
  
  // 返回内存块到池中
  let pool_with_returned = return_to_pool(pool3, 1001)
  let (pool4, block4) = allocate_from_pool(pool_with_returned, 1024)
  
  match block4 { Some(b) => assert_eq(b, 1001); None => assert_true(false) }
  
  // 批处理优化
  let batch_process = fn(items: Array<String>, batch_size: Int) -> Array<Array<String>> {
    let create_batches = fn(remaining: Array<String>, current_batch: Array<String>, result: Array<Array<String>>) -> Array<Array<String>> {
      if remaining.length() == 0 {
        if current_batch.length() > 0 {
          result + [current_batch]
        } else {
          result
        }
      } else if current_batch.length() >= batch_size {
        create_batches(remaining.slice(1, remaining.length()), [remaining[0]], result + [current_batch])
      } else {
        create_batches(remaining.slice(1, remaining.length()), current_batch + [remaining[0]], result)
      }
    }
    
    create_batches(items, [], [])
  }
  
  let test_items = ["item1", "item2", "item3", "item4", "item5", "item6", "item7"]
  let batches = batch_process(test_items, 3)
  
  assert_eq(batches.length(), 3)
  assert_eq(batches[0], ["item1", "item2", "item3"])
  assert_eq(batches[1], ["item4", "item5", "item6"])
  assert_eq(batches[2], ["item7"])
  
  // 缓存友好的数据访问模式
  let cache_friendly_access = fn(matrix: Array<Array<Int>>) -> Int {
    let rows = matrix.length()
    let cols = if rows > 0 { matrix[0].length() } else { 0 }
    
    // 行优先访问（缓存友好）
    let sum = 0
    for i in 0..<rows {
      for j in 0..<cols {
        sum = sum + matrix[i][j]
      }
    }
    sum
  }
  
  let test_matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  let matrix_sum = cache_friendly_access(test_matrix)
  assert_eq(matrix_sum, 45)
}

// 测试6: 数据库操作模拟
test "数据库操作模拟测试" {
  // 简化的数据库表结构
  type DatabaseRow {
    DatabaseRow(Array<String>, Array<String>) // 列名, 值
  }
  
  type DatabaseTable {
    DatabaseTable(String, Array<DatabaseRow>) // 表名, 行数据
  }
  
  // 创建表
  let create_table = fn(name: String) -> DatabaseTable {
    DatabaseTable(name, [])
  }
  
  // 插入行
  let insert_row = fn(table: DatabaseTable, columns: Array<String>, values: Array<String>) -> DatabaseTable {
    match table {
      DatabaseTable(name, rows) => {
        let new_row = DatabaseRow(columns, values)
        DatabaseTable(name, rows + [new_row])
      }
    }
  }
  
  // 查询行
  let query_rows = fn(table: DatabaseTable, condition: fn(DatabaseRow) -> Bool) -> Array<DatabaseRow> {
    match table {
      DatabaseTable(_, rows) => {
        rows.filter(condition)
      }
    }
  }
  
  // 创建用户表并插入数据
  let users_table = create_table("users")
  let users_table1 = insert_row(users_table, ["id", "name", "email", "active"], ["1", "Alice", "alice@example.com", "true"])
  let users_table2 = insert_row(users_table1, ["id", "name", "email", "active"], ["2", "Bob", "bob@example.com", "true"])
  let users_table3 = insert_row(users_table2, ["id", "name", "email", "active"], ["3", "Charlie", "charlie@example.com", "false"])
  
  // 查询活跃用户
  let active_users = query_rows(users_table3, fn(row) {
    match row {
      DatabaseRow(columns, values) => {
        let active_index = columns.find_index("active").unwrap()
        values[active_index] == "true"
      }
    }
  })
  
  assert_eq(active_users.length(), 2)
  
  // 查询特定用户
  let alice_user = query_rows(users_table3, fn(row) {
    match row {
      DatabaseRow(columns, values) => {
        let name_index = columns.find_index("name").unwrap()
        values[name_index] == "Alice"
      }
    }
  })
  
  assert_eq(alice_user.length(), 1)
  
  match alice_user[0] {
    DatabaseRow(columns, values) => {
      let email_index = columns.find_index("email").unwrap()
      assert_eq(values[email_index], "alice@example.com")
    }
  }
  
  // 事务模拟
  type Transaction {
    Transaction(Array<DatabaseTable>, Array<fn(DatabaseTable) -> DatabaseTable>) // 原始表, 操作列表
  }
  
  let begin_transaction = fn(tables: Array<DatabaseTable>) -> Transaction {
    Transaction(tables, [])
  }
  
  let add_operation = fn(transaction: Transaction, operation: fn(DatabaseTable) -> DatabaseTable) -> Transaction {
    match transaction {
      Transaction(tables, operations) => {
        Transaction(tables, operations + [operation])
      }
    }
  }
  
  let commit_transaction = fn(transaction: Transaction) -> Array<DatabaseTable> {
    match transaction {
      Transaction(tables, operations) => {
        operations.reduce(|current_tables, op| {
          current_tables.map(op)
        }, tables)
      }
    }
  }
  
  // 创建事务并执行多个操作
  let initial_tables = [users_table3]
  let transaction = begin_transaction(initial_tables)
  let transaction1 = add_operation(transaction, fn(table) {
    match table {
      DatabaseTable(name, _) if name == "users" => {
        insert_row(table, ["id", "name", "email", "active"], ["4", "David", "david@example.com", "true"])
      }
      _ => table
    }
  })
  
  let final_tables = commit_transaction(transaction1)
  
  match final_tables[0] {
    DatabaseTable(_, rows) => {
      assert_eq(rows.length(), 4) // 应该有4个用户
    }
  }
}

// 测试7: 缓存策略
test "缓存策略测试" {
  // 缓存项
  type CacheItem {
    CacheItem(String, String, Int) // 键, 值, 过期时间戳
  }
  
  // LRU缓存实现
  type LRUCache {
    LRUCache(Int, Array<CacheItem>, Int) // 容量, 缓存项, 当前时间
  }
  
  let lru_get = fn(cache: LRUCache, key: String) -> (LRUCache, Option<String>) {
    match cache {
      LRUCache(capacity, items, current_time) => {
        let item_index = items.find_index(fn(item) {
          match item {
            CacheItem(k, _, expiry) => k == key && expiry > current_time
          }
        })
        
        match item_index {
          Some(index) => {
            let item = items[index]
            match item {
              CacheItem(_, value, _) => {
                // 移动到最前面（最近使用）
                let new_items = [item] + items.filter_with_index(|_, i| i != index)
                (LRUCache(capacity, new_items, current_time), Some(value))
              }
            }
          }
          None => (cache, None)
        }
      }
    }
  }
  
  let lru_put = fn(cache: LRUCache, key: String, value: String, ttl_seconds: Int) -> LRUCache {
    match cache {
      LRUCache(capacity, items, current_time) => {
        let expiry_time = current_time + ttl_seconds
        let new_item = CacheItem(key, value, expiry_time)
        
        // 检查是否已存在
        let existing_index = items.find_index(fn(item) {
          match item {
            CacheItem(k, _, _) => k == key
          }
        })
        
        match existing_index {
          Some(index) => {
            // 更新现有项
            let new_items = items.map_with_index(|i, item| {
              if i == index { new_item } else { item }
            })
            LRUCache(capacity, new_items, current_time)
          }
          None => {
            // 添加新项
            if items.length() < capacity {
              LRUCache(capacity, [new_item] + items, current_time)
            } else {
              // 移除最旧的项
              let new_items = [new_item] + items.slice(0, capacity - 1)
              LRUCache(capacity, new_items, current_time)
            }
          }
        }
      }
    }
  }
  
  let lru_cleanup = fn(cache: LRUCache) -> LRUCache {
    match cache {
      LRUCache(capacity, items, current_time) => {
        let valid_items = items.filter(fn(item) {
          match item {
            CacheItem(_, _, expiry) => expiry > current_time
          }
        })
        LRUCache(capacity, valid_items, current_time)
      }
    }
  }
  
  // 测试LRU缓存
  let initial_cache = LRUCache(3, [], 1000)
  let cache1 = lru_put(initial_cache, "key1", "value1", 100) // 过期时间: 1100
  let cache2 = lru_put(cache1, "key2", "value2", 200) // 过期时间: 1200
  let cache3 = lru_put(cache2, "key3", "value3", 300) // 过期时间: 1300
  
  // 获取缓存项
  let (cache4, value1) = lru_get(cache3, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // 添加第四个项，应该移除最旧的
  let cache5 = lru_put(cache4, "key4", "value4", 400) // 过期时间: 1400
  let (cache6, value2) = lru_get(cache5, "key2") // key2应该被移除
  
  match value2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试过期清理
  let expired_cache = LRUCache(3, [
    CacheItem("expired1", "value1", 1050),
    CacheItem("valid1", "value2", 1150),
    CacheItem("expired2", "value3", 900)
  ], 1100)
  
  let cleaned_cache = lru_cleanup(expired_cache)
  
  match cleaned_cache {
    LRUCache(_, items, _) => {
      assert_eq(items.length(), 1)
      match items[0] {
        CacheItem(key, _, _) => assert_eq(key, "valid1")
      }
    }
  }
  
  // 缓存命中率统计
  type CacheStats {
    CacheStats(Int, Int) // 命中次数, 未命中次数
  }
  
  let update_stats = fn(stats: CacheStats, hit: Bool) -> CacheStats {
    match stats {
      CacheStats(hits, misses) => {
        if hit {
          CacheStats(hits + 1, misses)
        } else {
          CacheStats(hits, misses + 1)
        }
      }
    }
  }
  
  let get_hit_rate = fn(stats: CacheStats) -> Float {
    match stats {
      CacheStats(hits, misses) => {
        let total = hits + misses
        if total == 0 { 0.0 }
        else { hits.to_float() / total.to_float() }
      }
    }
  }
  
  let initial_stats = CacheStats(0, 0)
  let stats1 = update_stats(initial_stats, true)
  let stats2 = update_stats(stats1, false)
  let stats3 = update_stats(stats2, true)
  let stats4 = update_stats(stats3, true)
  
  let hit_rate = get_hit_rate(stats4)
  assert_eq(hit_rate, 0.75) // 3 hits out of 4 requests
}

// 测试8: 负载均衡
test "负载均衡测试" {
  // 服务器节点
  type ServerNode {
    ServerNode(String, String, Int, Bool) // ID, 地址, 权重, 健康状态
  }
  
  // 负载均衡器
  type LoadBalancer {
    LoadBalancer(Array<ServerNode>, String, Int) // 节点列表, 算法类型, 当前索引
  }
  
  // 轮询算法
  let round_robin_select = fn(lb: LoadBalancer) -> (LoadBalancer, Option<ServerNode>) {
    match lb {
      LoadBalancer(nodes, algorithm, current_index) if algorithm == "round_robin" => {
        let healthy_nodes = nodes.filter(fn(node) {
          match node {
            ServerNode(_, _, _, healthy) => healthy
          }
        })
        
        if healthy_nodes.length() == 0 {
          (lb, None)
        } else {
          let selected_index = current_index % healthy_nodes.length()
          let selected_node = healthy_nodes[selected_index]
          let new_lb = LoadBalancer(nodes, algorithm, current_index + 1)
          (new_lb, Some(selected_node))
        }
      }
      _ => (lb, None)
    }
  }
  
  // 加权轮询算法
  let weighted_round_robin_select = fn(lb: LoadBalancer, current_weights: Array<Int>) -> (LoadBalancer, Array<Int>, Option<ServerNode>) {
    match lb {
      LoadBalancer(nodes, algorithm, _) if algorithm == "weighted_round_robin" => {
        let healthy_nodes = nodes.filter_with_index(|i, node| {
          match node {
            ServerNode(_, _, _, healthy) => healthy
          }
        })
        
        if healthy_nodes.length() == 0 {
          (lb, current_weights, None)
        } else {
          // 找到当前权重最高的节点
          let max_weight_index = current_weights.reduce(|(max_idx, max_val), (idx, val)| {
            if val > max_val { (idx, val) } else { (max_idx, max_val) }
          }, (0, current_weights[0])).0
          
          let selected_node = healthy_nodes[max_weight_index]
          
          // 更新权重
          let new_weights = current_weights.map_with_index(|i, weight| {
            if i == max_weight_index {
              match selected_node {
                ServerNode(_, _, node_weight, _) => {
                  if weight > 0 { weight - 1 } else { node_weight - 1 }
                }
              }
            } else {
              weight
            }
          })
          
          // 如果所有权重都为0，重置权重
          let all_zero = new_weights.all(|w| w == 0)
          let reset_weights = if all_zero {
            healthy_nodes.map(|node| {
              match node {
                ServerNode(_, _, weight, _) => weight
              }
            })
          } else {
            new_weights
          }
          
          let new_lb = LoadBalancer(nodes, algorithm, 0)
          (new_lb, reset_weights, Some(selected_node))
        }
      }
      _ => (lb, current_weights, None)
    }
  }
  
  // 创建服务器节点
  let server_nodes = [
    ServerNode("server1", "192.168.1.10:8080", 1, true),
    ServerNode("server2", "192.168.1.11:8080", 2, true),
    ServerNode("server3", "192.168.1.12:8080", 3, false) // 不健康
  ]
  
  // 测试轮询算法
  let round_robin_lb = LoadBalancer(server_nodes, "round_robin", 0)
  
  let (lb1, node1) = round_robin_select(round_robin_lb)
  let (lb2, node2) = round_robin_select(lb1)
  let (lb3, node3) = round_robin_select(lb2)
  let (lb4, node4) = round_robin_select(lb3)
  
  match node1 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server1")
    None => assert_true(false)
  }
  
  match node2 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server2")
    None => assert_true(false)
  }
  
  match node3 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server1")
    None => assert_true(false)
  }
  
  match node4 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server2")
    None => assert_true(false)
  }
  
  // 测试加权轮询算法
  let weighted_lb = LoadBalancer(server_nodes, "weighted_round_robin", 0)
  let initial_weights = [1, 2] // 只包含健康节点的权重
  
  let (wlb1, weights1, wnode1) = weighted_round_robin_select(weighted_lb, initial_weights)
  let (wlb2, weights2, wnode2) = weighted_round_robin_select(wlb1, weights1)
  let (wlb3, weights3, wnode3) = weighted_round_robin_select(wlb2, weights2)
  
  // server2权重更高，应该被选中更多次
  match wnode1 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server2")
    None => assert_true(false)
  }
  
  match wnode2 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server2")
    None => assert_true(false)
  }
  
  match wnode3 {
    Some(ServerNode(id, _, _, _)) => assert_eq(id, "server1")
    None => assert_true(false)
  }
  
  // 健康检查
  let health_check = fn(nodes: Array<ServerNode>) -> Array<ServerNode> {
    nodes.map(|node| {
      match node {
        ServerNode(id, address, weight, _) => {
          // 模拟健康检查：server1和server2健康，server3不健康
          let is_healthy = id != "server3"
          ServerNode(id, address, weight, is_healthy)
        }
      }
    })
  }
  
  let updated_nodes = health_check(server_nodes)
  let healthy_count = updated_nodes.filter(|node| {
    match node {
      ServerNode(_, _, _, healthy) => healthy
    }
  }).length()
  
  assert_eq(healthy_count, 2)
}

// 测试9: 容错处理
test "容错处理测试" {
  // 断路器状态
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // 断路器
  type CircuitBreaker {
    CircuitBreaker(CircuitState, Int, Int, Int, Int) // 状态, 失败次数, 失败阈值, 成功次数, 成功阈值
  }
  
  let circuit_breaker_call = fn(cb: CircuitBreaker, operation: fn() -> (Bool, String)) -> (CircuitBreaker, (Bool, String)) {
    match cb {
      CircuitBreaker(state, failures, failure_threshold, successes, success_threshold) => {
        match state {
          Closed => {
            let (success, result) = operation()
            if success {
              let new_cb = CircuitBreaker(Closed, 0, failure_threshold, 0, success_threshold)
              (new_cb, (true, result))
            } else {
              let new_failures = failures + 1
              if new_failures >= failure_threshold {
                let new_cb = CircuitBreaker(Open, new_failures, failure_threshold, 0, success_threshold)
                (new_cb, (false, "Circuit opened due to failures"))
              } else {
                let new_cb = CircuitBreaker(Closed, new_failures, failure_threshold, 0, success_threshold)
                (new_cb, (false, result))
              }
            }
          }
          Open => (cb, (false, "Circuit is open"))
          HalfOpen => {
            let (success, result) = operation()
            if success {
              let new_successes = successes + 1
              if new_successes >= success_threshold {
                let new_cb = CircuitBreaker(Closed, 0, failure_threshold, 0, success_threshold)
                (new_cb, (true, result))
              } else {
                let new_cb = CircuitBreaker(HalfOpen, 0, failure_threshold, new_successes, success_threshold)
                (new_cb, (true, result))
              }
            } else {
              let new_cb = CircuitBreaker(Open, 1, failure_threshold, 0, success_threshold)
              (new_cb, (false, "Circuit opened again"))
            }
          }
        }
      }
    }
  }
  
  let attempt_reset = fn(cb: CircuitBreaker) -> CircuitBreaker {
    match cb {
      CircuitBreaker(Open, _, failure_threshold, _, success_threshold) => {
        CircuitBreaker(HalfOpen, 0, failure_threshold, 0, success_threshold)
      }
      _ => cb
    }
  }
  
  // 测试断路器
  let initial_cb = CircuitBreaker(Closed, 0, 3, 0, 2)
  
  // 模拟失败操作
  let failing_operation = fn() -> (Bool, String) {
    (false, "Operation failed")
  }
  
  let successful_operation = fn() -> (Bool, String) {
    (true, "Operation succeeded")
  }
  
  let (cb1, result1) = circuit_breaker_call(initial_cb, failing_operation)
  let (cb2, result2) = circuit_breaker_call(cb1, failing_operation)
  let (cb3, result3) = circuit_breaker_call(cb2, failing_operation) // 应该打开断路器
  
  assert_false(result3.0) // 应该失败
  assert_true(result3.1.contains("Circuit opened"))
  
  // 尝试在断路器打开时调用
  let (cb4, result4) = circuit_breaker_call(cb3, successful_operation)
  assert_false(result4.0) // 应该失败
  assert_true(result4.1.contains("Circuit is open"))
  
  // 尝试重置断路器
  let cb5 = attempt_reset(cb4)
  let (cb6, result5) = circuit_breaker_call(cb5, successful_operation)
  let (cb7, result6) = circuit_breaker_call(cb6, successful_operation) // 应该关闭断路器
  
  assert_true(result5.0) // 应该成功
  assert_true(result6.0) // 应该成功
  
  // 超时处理
  type TimeoutConfig {
    TimeoutConfig(Int, Int) // 超时时间(毫秒), 重试次数
  }
  
  let execute_with_timeout = fn(config: TimeoutConfig, operation: fn() -> String, current_attempt: Int) -> (Bool, String) {
    match config {
      TimeoutConfig(timeout_ms, max_retries) => {
        // 模拟操作执行时间
        let operation_time = 150 // 模拟操作需要150ms
        
        if operation_time > timeout_ms {
          if current_attempt < max_retries {
            execute_with_timeout(config, operation, current_attempt + 1)
          } else {
            (false, "Operation timed out after " + max_retries.to_string() + " retries")
          }
        } else {
          (true, operation())
        }
      }
    }
  }
  
  let timeout_config = TimeoutConfig(100, 3)
  let (success, message) = execute_with_timeout(timeout_config, fn() { "Operation completed" }, 0)
  
  assert_false(success)
  assert_true(message.contains("timed out"))
  
  // 重试策略
  type RetryConfig {
    RetryConfig(Int, fn(Int) -> Int) // 最大重试次数, 延迟计算函数
  }
  
  let exponential_backoff = fn(attempt: Int) -> Int {
    100 * (2 ^ attempt) // 指数退避
  }
  
  let execute_with_retry = fn(config: RetryConfig, operation: fn() -> (Bool, String), current_attempt: Int) -> (Bool, String) {
    match config {
      RetryConfig(max_retries, delay_fn) => {
        let (success, result) = operation()
        if success || current_attempt >= max_retries {
          (success, result)
        } else {
          let delay = delay_fn(current_attempt)
          // 在实际实现中，这里会等待delay毫秒
          execute_with_retry(config, operation, current_attempt + 1)
        }
      }
    }
  }
  
  let retry_config = RetryConfig(3, exponential_backoff)
  let attempt_count = { mut count = 0; fn() { count = count + 1; (count <= 2, "Attempt " + count.to_string()) } }
  let (retry_success, retry_result) = execute_with_retry(retry_config, attempt_count(), 0)
  
  assert_true(retry_success)
  assert_eq(retry_result, "Attempt 3")
}

// 测试10: 监控和告警
test "监控和告警测试" {
  // 指标类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  // 指标
  type Metric {
    Metric(String, MetricType, Float, Array<String>) // 名称, 类型, 值, 标签
  }
  
  // 告警规则
  type AlertRule {
    AlertRule(String, fn(Metric) -> Bool, String, Int) // 名称, 条件函数, 消息模板, 严重级别
  }
  
  // 告警
  type Alert {
    Alert(String, String, Int, Int) // 规则名称, 消息, 严重级别, 时间戳
  }
  
  // 监控系统
  type MonitoringSystem {
    MonitoringSystem(Array<Metric>, Array<AlertRule>, Array<Alert>) // 指标, 告警规则, 活跃告警
  }
  
  let add_metric = fn(system: MonitoringSystem, metric: Metric) -> MonitoringSystem {
    match system {
      MonitoringSystem(metrics, rules, alerts) => {
        MonitoringSystem(metrics + [metric], rules, alerts)
      }
    }
  }
  
  let evaluate_alerts = fn(system: MonitoringSystem, current_time: Int) -> MonitoringSystem {
    match system {
      MonitoringSystem(metrics, rules, alerts) => {
        let new_alerts = rules.reduce(|current_alerts, rule| {
          match rule {
            AlertRule(name, condition, message_template, severity) => {
              let triggered_metrics = metrics.filter(condition)
              
              if triggered_metrics.length() > 0 {
                // 检查是否已有相同的告警
                let existing_alert = alerts.find(|alert| {
                  match alert {
                    Alert(rule_name, _, _, _) => rule_name == name
                  }
                })
                
                match existing_alert {
                  Some(_) => current_alerts // 已有告警，不再重复
                  None => {
                    let message = message_template + " (value: " + triggered_metrics[0].value.to_string() + ")"
                    current_alerts + [Alert(name, message, severity, current_time)]
                  }
                }
              } else {
                current_alerts
              }
            }
          }
        }, [])
        
        MonitoringSystem(metrics, rules, new_alerts)
      }
    }
  }
  
  let clear_resolved_alerts = fn(system: MonitoringSystem) -> MonitoringSystem {
    match system {
      MonitoringSystem(metrics, rules, alerts) => {
        let resolved_alerts = alerts.filter(|alert| {
          match alert {
            Alert(rule_name, _, _, _) => {
              let rule = rules.find(|r| {
                match r {
                  AlertRule(name, _, _, _) => name == rule_name
                }
              })
              
              match rule {
                Some(AlertRule(_, condition, _, _)) => {
                  // 如果没有指标满足条件，则告警已解决
                  !metrics.any(condition)
                }
                None => false
              }
            }
          }
        })
        
        MonitoringSystem(metrics, rules, resolved_alerts)
      }
    }
  }
  
  // 创建告警规则
  let alert_rules = [
    AlertRule("high_cpu_usage", fn(metric) {
      match metric {
        Metric(name, _, value, labels) => {
          name == "cpu_usage" && value > 80.0
        }
      }
    }, "CPU usage is too high", 2),
    
    AlertRule("high_memory_usage", fn(metric) {
      match metric {
        Metric(name, _, value, labels) => {
          name == "memory_usage" && value > 90.0
        }
      }
    }, "Memory usage is critical", 3),
    
    AlertRule("disk_space_low", fn(metric) {
      match metric {
        Metric(name, _, value, labels) => {
          name == "disk_usage" && value > 95.0
        }
      }
    }, "Disk space is running low", 1)
  ]
  
  // 创建监控系统
  let initial_system = MonitoringSystem([], alert_rules, [])
  
  // 添加指标
  let system1 = add_metric(initial_system, Metric("cpu_usage", Gauge, 85.0, ["service:api"]))
  let system2 = add_metric(system1, Metric("memory_usage", Gauge, 75.0, ["service:api"]))
  let system3 = add_metric(system2, Metric("disk_usage", Gauge, 96.0, ["device:/dev/sda1"]))
  
  // 评估告警
  let system4 = evaluate_alerts(system3, 1640995200)
  
  match system4 {
    MonitoringSystem(_, _, alerts) => {
      assert_eq(alerts.length(), 2) // CPU和磁盘告警应该被触发
      
      let cpu_alert = alerts.find(|alert| {
        match alert {
          Alert(name, _, _, _) => name == "high_cpu_usage"
        }
      })
      
      match cpu_alert {
        Some(Alert(_, message, severity, _)) => {
          assert_true(message.contains("CPU usage is too high"))
          assert_eq(severity, 2)
        }
        None => assert_true(false)
      }
      
      let disk_alert = alerts.find(|alert| {
        match alert {
          Alert(name, _, _, _) => name == "disk_space_low"
        }
      })
      
      match disk_alert {
        Some(Alert(_, message, severity, _)) => {
          assert_true(message.contains("Disk space is running low"))
          assert_eq(severity, 1)
        }
        None => assert_true(false)
      }
    }
  }
  
  // 更新指标并检查告警解决
  let system5 = add_metric(system4, Metric("cpu_usage", Gauge, 60.0, ["service:api"]))
  let system6 = add_metric(system5, Metric("disk_usage", Gauge, 80.0, ["device:/dev/sda1"]))
  let system7 = evaluate_alerts(system6, 1640995300)
  let system8 = clear_resolved_alerts(system7)
  
  match system8 {
    MonitoringSystem(_, _, alerts) => {
      assert_eq(alerts.length(), 0) // 所有告警应该已解决
    }
  }
  
  // 指标聚合
  let aggregate_metrics = fn(metrics: Array<Metric>, operation: fn(Array<Float>) -> Float) -> Array<Metric> {
    let grouped = metrics.reduce(|groups, metric| {
      match metric {
        Metric(name, _, value, labels) => {
          let key = name + ":" + labels.join(",")
          match groups.get(key) {
            Some(values) => {
              groups.set(key, values + [value])
            }
            None => {
              groups.set(key, [value])
            }
          }
          groups
        }
      }
    }, {})
    
    grouped.map(|(key, values)| {
      let parts = key.split(":")
      let name = parts[0]
      let labels_str = if parts.length() > 1 { parts[1] } else { "" }
      let labels = if labels_str == "" { [] } else { labels_str.split(",") }
      let aggregated_value = operation(values)
      Metric(name, Gauge, aggregated_value, labels)
    }).to_array()
  }
  
  let test_metrics = [
    Metric("response_time", Histogram, 100.0, ["endpoint:/api/users"]),
    Metric("response_time", Histogram, 150.0, ["endpoint:/api/users"]),
    Metric("response_time", Histogram, 200.0, ["endpoint:/api/users"]),
    Metric("response_time", Histogram, 120.0, ["endpoint:/api/orders"]),
    Metric("response_time", Histogram, 180.0, ["endpoint:/api/orders"])
  ]
  
  let avg_metrics = aggregate_metrics(test_metrics, fn(values) {
    values.reduce(|acc, val| acc + val, 0.0) / values.length().to_float()
  })
  
  assert_eq(avg_metrics.length(), 2)
  
  let users_avg = avg_metrics.find(|metric| {
    match metric {
      Metric(name, _, _, labels) => {
        name == "response_time" && labels.contains("endpoint:/api/users")
      }
    }
  })
  
  match users_avg {
    Some(Metric(_, _, value, _)) => assert_eq(value, 150.0) // (100+150+200)/3
    None => assert_true(false)
  }
}