// Azimuth Data Consistency Validation Test Suite
// 数据一致性验证测试用例

// 测试1: 数据完整性校验
test "数据完整性校验测试" {
  // 校验和计算器
  struct ChecksumCalculator {
    algorithm : String
  }
  
  fn ChecksumCalculator::new(algorithm : String) -> ChecksumCalculator {
    ChecksumCalculator::{ algorithm }
  }
  
  fn ChecksumCalculator::calculate(calculator : ChecksumCalculator, data : Array[Byte]) -> Int {
    match calculator.algorithm {
      "simple" => {
        // 简单校验和：所有字节的总和
        let mut sum = 0
        for byte in data {
          sum = sum + byte.to_int()
        }
        sum % 256  // 限制在0-255范围内
      }
      "weighted" => {
        // 加权校验和：位置加权
        let mut sum = 0
        for i in 0..data.length() {
          sum = sum + (data[i].to_int() * (i + 1))
        }
        sum % 256
      }
      "xor" => {
        // XOR校验和
        let mut result = 0
        for byte in data {
          result = result ^ byte.to_int()
        }
        result
      }
      _ => 0
    }
  }
  
  // 数据块
  struct DataBlock {
    id : String
    data : Array[Byte]
    checksum : Int
    timestamp : Int64
  }
  
  fn DataBlock::new(id : String, data : Array[Byte], calculator : ChecksumCalculator) -> DataBlock {
    let checksum = ChecksumCalculator::calculate(calculator, data)
    DataBlock::{ id, data, checksum, timestamp: 1640995200000000000L }
  }
  
  fn DataBlock::verify(block : DataBlock, calculator : ChecksumCalculator) -> Bool {
    let calculated_checksum = ChecksumCalculator::calculate(calculator, block.data)
    calculated_checksum == block.checksum
  }
  
  // 测试校验和计算
  let simple_calc = ChecksumCalculator::new("simple")
  let weighted_calc = ChecksumCalculator::new("weighted")
  let xor_calc = ChecksumCalculator::new("xor")
  
  let test_data = [1, 2, 3, 4, 5]
  
  // 简单校验和：1+2+3+4+5 = 15
  assert_eq(ChecksumCalculator::calculate(simple_calc, test_data), 15)
  
  // 加权校验和：1*1 + 2*2 + 3*3 + 4*4 + 5*5 = 55
  assert_eq(ChecksumCalculator::calculate(weighted_calc, test_data), 55)
  
  // XOR校验和：1^2^3^4^5
  let xor_result = ChecksumCalculator::calculate(xor_calc, test_data)
  assert_true(xor_result >= 0 && xor_result <= 255)
  
  // 测试数据块创建和验证
  let block1 = DataBlock::new("block-1", test_data, simple_calc)
  assert_eq(block1.id, "block-1")
  assert_eq(block1.data, test_data)
  assert_eq(block1.checksum, 15)
  assert_true(DataBlock::verify(block1, simple_calc))
  
  // 测试数据损坏检测
  let corrupted_data = [1, 2, 3, 4, 6]  // 最后一个字节被修改
  let corrupted_block = DataBlock::{ 
    id: "block-1", 
    data: corrupted_data, 
    checksum: 15,  // 原始校验和
    timestamp: 1640995200000000000L 
  }
  
  assert_false(DataBlock::verify(corrupted_block, simple_calc))
  
  // 测试不同算法的验证
  let block2 = DataBlock::new("block-2", test_data, weighted_calc)
  assert_true(DataBlock::verify(block2, weighted_calc))
  assert_false(DataBlock::verify(block2, simple_calc))  // 使用错误的算法应该失败
  
  let block3 = DataBlock::new("block-3", test_data, xor_calc)
  assert_true(DataBlock::verify(block3, xor_calc))
  assert_false(DataBlock::verify(block3, weighted_calc))  // 使用错误的算法应该失败
}

// 测试2: 数据版本控制
test "数据版本控制测试" {
  // 版本信息
  struct Version {
    major : Int
    minor : Int
    patch : Int
  }
  
  fn Version::new(major : Int, minor : Int, patch : Int) -> Version {
    Version::{ major, minor, patch }
  }
  
  fn Version::to_string(version : Version) -> String {
    version.major.to_string() + "." + version.minor.to_string() + "." + version.patch.to_string()
  }
  
  fn Version::compare(v1 : Version, v2 : Version) -> Int {
    if v1.major != v2.major {
      if v1.major > v2.major { 1 } else { -1 }
    } else if v1.minor != v2.minor {
      if v1.minor > v2.minor { 1 } else { -1 }
    } else if v1.patch != v2.patch {
      if v1.patch > v2.patch { 1 } else { -1 }
    } else {
      0
    }
  }
  
  // 版本化数据
  struct VersionedData[T] {
    data : T
    version : Version
    timestamp : Int64
    metadata : Array[(String, String)]
  }
  
  fn VersionedData::new[T](data : T, version : Version) -> VersionedData[T] {
    VersionedData::{ data, version, timestamp: 1640995200000000000L, metadata: [] }
  }
  
  fn VersionedData::with_metadata[T](data : T, version : Version, metadata : Array[(String, String)]) -> VersionedData[T] {
    VersionedData::{ data, version, timestamp: 1640995200000000000L, metadata }
  }
  
  // 版本兼容性检查器
  struct CompatibilityChecker {
    current_version : Version
    min_compatible_version : Version
  }
  
  fn CompatibilityChecker::new(current : Version, min_compatible : Version) -> CompatibilityChecker {
    CompatibilityChecker::{ current_version: current, min_compatible_version: min_compatible }
  }
  
  fn CompatibilityChecker::is_compatible(checker : CompatibilityChecker, version : Version) -> Bool {
    Version::compare(version, checker.min_compatible_version) >= 0 && 
    Version::compare(version, checker.current_version) <= 0
  }
  
  // 测试版本操作
  let v1 = Version::new(1, 0, 0)
  let v2 = Version::new(1, 2, 3)
  let v3 = Version::new(2, 0, 0)
  
  assert_eq(Version::to_string(v1), "1.0.0")
  assert_eq(Version::to_string(v2), "1.2.3")
  assert_eq(Version::to_string(v3), "2.0.0")
  
  assert_eq(Version::compare(v1, v2), -1)  // v1 < v2
  assert_eq(Version::compare(v2, v1), 1)   // v2 > v1
  assert_eq(Version::compare(v1, v1), 0)   // v1 == v1
  assert_eq(Version::compare(v3, v2), 1)   // v3 > v2
  
  // 测试版本化数据
  let data_v1 = VersionedData::new("old-data", v1)
  assert_eq(data_v1.data, "old-data")
  assert_eq(data_v1.version, v1)
  
  let data_v2 = VersionedData::with_metadata("new-data", v2, [("feature", "advanced")])
  assert_eq(data_v2.data, "new-data")
  assert_eq(data_v2.version, v2)
  assert_eq(data_v2.metadata.length(), 1)
  
  // 测试兼容性检查
  let checker = CompatibilityChecker::new(v3, v1)  // 当前版本2.0.0，最小兼容1.0.0
  
  assert_true(CompatibilityChecker::is_compatible(checker, v1))  // 1.0.0 >= 1.0.0
  assert_true(CompatibilityChecker::is_compatible(checker, v2))  // 1.2.3 >= 1.0.0
  assert_true(CompatibilityChecker::is_compatible(checker, v3))  // 2.0.0 <= 2.0.0
  
  let v0 = Version::new(0, 9, 0)
  assert_false(CompatibilityChecker::is_compatible(checker, v0))  // 0.9.0 < 1.0.0
  
  let v4 = Version::new(2, 1, 0)
  assert_false(CompatibilityChecker::is_compatible(checker, v4))  // 2.1.0 > 2.0.0
}

// 测试3: 事务一致性
test "事务一致性测试" {
  // 事务状态
  enum TransactionState {
    Active
    Committed
    Aborted
  }
  
  // 操作类型
  enum OperationType {
    Create
    Update
    Delete
  }
  
  // 操作记录
  struct Operation {
    op_type : OperationType
    target : String
    old_value : Option[String]
    new_value : Option[String]
  }
  
  // 事务
  struct Transaction {
    id : String
    state : TransactionState
    operations : Array[Operation]
    start_time : Int64
  }
  
  fn Transaction::new(id : String) -> Transaction {
    Transaction::{
      id,
      state: TransactionState::Active,
      operations: [],
      start_time: 1640995200000000000L
    }
  }
  
  fn Transaction::add_operation(transaction : Transaction, op_type : OperationType, target : String, old_value : Option[String], new_value : Option[String]) -> Transaction {
    if transaction.state != TransactionState::Active {
      return transaction  // 只能为活动事务添加操作
    }
    
    let operation = Operation::{ op_type, target, old_value, new_value }
    Transaction::{
      id: transaction.id,
      state: transaction.state,
      operations: transaction.operations.push(operation),
      start_time: transaction.start_time
    }
  }
  
  fn Transaction::commit(transaction : Transaction) -> Transaction {
    if transaction.state != TransactionState::Active {
      return transaction
    }
    
    Transaction::{ 
      id: transaction.id, 
      state: TransactionState::Committed, 
      operations: transaction.operations, 
      start_time: transaction.start_time 
    }
  }
  
  fn Transaction::abort(transaction : Transaction) -> Transaction {
    if transaction.state != TransactionState::Active {
      return transaction
    }
    
    Transaction::{ 
      id: transaction.id, 
      state: TransactionState::Aborted, 
      operations: transaction.operations, 
      start_time: transaction.start_time 
    }
  }
  
  // 事务管理器
  struct TransactionManager {
    active_transactions : Array[Transaction]
    committed_transactions : Array[Transaction]
    data : Array[(String, String)]  // 简化的数据存储
  }
  
  fn TransactionManager::new() -> TransactionManager {
    TransactionManager::{
      active_transactions: [],
      committed_transactions: [],
      data: []
    }
  }
  
  fn TransactionManager::begin_transaction(manager : TransactionManager, id : String) -> TransactionManager {
    let transaction = Transaction::new(id)
    TransactionManager::{
      active_transactions: manager.active_transactions.push(transaction),
      committed_transactions: manager.committed_transactions,
      data: manager.data
    }
  }
  
  fn TransactionManager::commit_transaction(manager : TransactionManager, id : String) -> TransactionManager {
    // 找到指定的事务
    match manager.active_transactions.find_index(fn(t) { t.id == id }) {
      None => manager
      Some(index) => {
        let transaction = manager.active_transactions[index]
        let committed_transaction = Transaction::commit(transaction)
        
        // 应用事务操作到数据存储
        let mut updated_data = manager.data
        for operation in committed_transaction.operations {
          match operation.op_type {
            OperationType::Create => {
              match operation.new_value {
                Some(value) => updated_data = updated_data.push((operation.target, value))
                None => ()
              }
            }
            OperationType::Update => {
              match operation.new_value {
                Some(value) => {
                  let mut found = false
                  let mut temp_data = []
                  for (key, val) in updated_data {
                    if key == operation.target {
                      temp_data = temp_data.push((key, value))
                      found = true
                    } else {
                      temp_data = temp_data.push((key, val))
                    }
                  }
                  if found {
                    updated_data = temp_data
                  }
                }
                None => ()
              }
            }
            OperationType::Delete => {
              updated_data = updated_data.filter(fn(pair) { pair.0 != operation.target })
            }
          }
        }
        
        // 更新事务列表
        let mut updated_active = manager.active_transactions
        updated_active.remove(index)
        
        TransactionManager::{
          active_transactions: updated_active,
          committed_transactions: manager.committed_transactions.push(committed_transaction),
          data: updated_data
        }
      }
    }
  }
  
  fn TransactionManager::abort_transaction(manager : TransactionManager, id : String) -> TransactionManager {
    match manager.active_transactions.find_index(fn(t) { t.id == id }) {
      None => manager
      Some(index) => {
        let transaction = manager.active_transactions[index]
        let aborted_transaction = Transaction::abort(transaction)
        
        // 更新事务列表（不应用操作）
        let mut updated_active = manager.active_transactions
        updated_active.remove(index)
        
        TransactionManager::{
          active_transactions: updated_active,
          committed_transactions: manager.committed_transactions.push(aborted_transaction),
          data: manager.data
        }
      }
    }
  }
  
  fn TransactionManager::get_data(manager : TransactionManager, key : String) -> Option[String] {
    for (k, v) in manager.data {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // 测试事务管理
  let manager = TransactionManager::new()
  assert_eq(manager.active_transactions.length(), 0)
  assert_eq(manager.committed_transactions.length(), 0)
  assert_eq(manager.data.length(), 0)
  
  // 开始事务
  let manager1 = TransactionManager::begin_transaction(manager, "tx-1")
  assert_eq(manager1.active_transactions.length(), 1)
  
  // 获取事务并添加操作
  let transaction = manager1.active_transactions[0]
  let tx_with_ops = Transaction::add_operation(transaction, OperationType::Create, "key-1", None, Some("value-1"))
  
  // 更新管理器中的事务
  let mut updated_active = manager1.active_transactions
  updated_active[0] = tx_with_ops
  let manager2 = TransactionManager::{ 
    active_transactions: updated_active, 
    committed_transactions: manager1.committed_transactions, 
    data: manager1.data 
  }
  
  // 提交事务
  let manager3 = TransactionManager::commit_transaction(manager2, "tx-1")
  assert_eq(manager3.active_transactions.length(), 0)
  assert_eq(manager3.committed_transactions.length(), 1)
  assert_eq(manager3.data.length(), 1)
  assert_eq(TransactionManager::get_data(manager3, "key-1"), Some("value-1"))
  
  // 开始另一个事务
  let manager4 = TransactionManager::begin_transaction(manager3, "tx-2");
  let manager5 = TransactionManager::begin_transaction(manager4, "tx-3")
  
  assert_eq(manager5.active_transactions.length(), 2)
  
  // 为tx-2添加操作
  let tx2 = manager5.active_transactions[0]
  let tx2_with_ops = Transaction::add_operation(tx2, OperationType::Update, "key-1", Some("value-1"), Some("value-2"))
  
  // 为tx-3添加操作
  let tx3 = manager5.active_transactions[1]
  let tx3_with_ops = Transaction::add_operation(tx3, OperationType::Create, "key-2", None, Some("value-3"))
  
  // 更新管理器
  let mut updated_active2 = manager5.active_transactions
  updated_active2[0] = tx2_with_ops
  updated_active2[1] = tx3_with_ops
  let manager6 = TransactionManager::{ 
    active_transactions: updated_active2, 
    committed_transactions: manager5.committed_transactions, 
    data: manager5.data 
  }
  
  // 提交tx-2
  let manager7 = TransactionManager::commit_transaction(manager6, "tx-2")
  assert_eq(manager7.active_transactions.length(), 1)
  assert_eq(manager7.data.length(), 1)
  assert_eq(TransactionManager::get_data(manager7, "key-1"), Some("value-2"))  // 值已更新
  
  // 中止tx-3
  let manager8 = TransactionManager::abort_transaction(manager7, "tx-3")
  assert_eq(manager8.active_transactions.length(), 0)
  assert_eq(manager8.committed_transactions.length(), 3)
  assert_eq(manager8.data.length(), 1)  // tx-3的操作没有应用
  assert_eq(TransactionManager::get_data(manager8, "key-2"), None)  // key-2不存在
}

// 测试4: 数据同步验证
test "数据同步验证测试" {
  // 同步状态
  enum SyncStatus {
    InSync
    OutOfSync
    Syncing
    Error
  }
  
  // 数据副本
  struct DataReplica {
    id : String
    data : Array[(String, String)]
    version : Int
    last_sync : Int64
    status : SyncStatus
  }
  
  fn DataReplica::new(id : String) -> DataReplica {
    DataReplica::{
      id,
      data: [],
      version: 0,
      last_sync: 0L,
      status: SyncStatus::InSync
    }
  }
  
  fn DataReplica::update_data(replica : DataReplica, key : String, value : String) -> DataReplica {
    let mut updated_data = []
    let mut found = false
    
    for (k, v) in replica.data {
      if k == key {
        updated_data = updated_data.push((key, value))
        found = true
      } else {
        updated_data = updated_data.push((k, v))
      }
    }
    
    if !found {
      updated_data = updated_data.push((key, value))
    }
    
    DataReplica::{
      id: replica.id,
      data: updated_data,
      version: replica.version + 1,
      last_sync: replica.last_sync,
      status: SyncStatus::OutOfSync
    }
  }
  
  fn DataReplica::get_value(replica : DataReplica, key : String) -> Option[String] {
    for (k, v) in replica.data {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // 同步管理器
  struct SyncManager {
    primary : DataReplica
    replicas : Array[DataReplica]
    sync_history : Array[(String, Int64, Bool)]  // (replica_id, timestamp, success)
  }
  
  fn SyncManager::new(primary_id : String) -> SyncManager {
    SyncManager::{
      primary: DataReplica::new(primary_id),
      replicas: [],
      sync_history: []
    }
  }
  
  fn SyncManager::add_replica(manager : SyncManager, replica_id : String) -> SyncManager {
    let replica = DataReplica::new(replica_id)
    SyncManager::{
      primary: manager.primary,
      replicas: manager.replicas.push(replica),
      sync_history: manager.sync_history
    }
  }
  
  fn SyncManager::update_primary(manager : SyncManager, key : String, value : String) -> SyncManager {
    let updated_primary = DataReplica::update_data(manager.primary, key, value)
    
    // 将所有副本标记为不同步
    let mut updated_replicas = []
    for replica in manager.replicas {
      updated_replicas = updated_replicas.push(DataReplica::{
        id: replica.id,
        data: replica.data,
        version: replica.version,
        last_sync: replica.last_sync,
        status: SyncStatus::OutOfSync
      })
    }
    
    SyncManager::{
      primary: updated_primary,
      replicas: updated_replicas,
      sync_history: manager.sync_history
    }
  }
  
  fn SyncManager::sync_replica(manager : SyncManager, replica_id : String, timestamp : Int64) -> SyncManager {
    // 查找副本
    match manager.replicas.find_index(fn(r) { r.id == replica_id }) {
      None => manager
      Some(index) => {
        let replica = manager.replicas[index]
        
        // 执行同步（简单复制主副本的数据）
        let synced_replica = DataReplica::{
          id: replica.id,
          data: manager.primary.data,
          version: manager.primary.version,
          last_sync: timestamp,
          status: SyncStatus::InSync
        }
        
        // 更新副本列表
        let mut updated_replicas = manager.replicas
        updated_replicas[index] = synced_replica
        
        // 记录同步历史
        let updated_history = manager.sync_history.push((replica_id, timestamp, true))
        
        SyncManager::{
          primary: manager.primary,
          replicas: updated_replicas,
          sync_history: updated_history
        }
      }
    }
  }
  
  fn SyncManager::verify_consistency(manager : SyncManager) -> Array[String] {
    let mut inconsistent_replicas = []
    
    for replica in manager.replicas {
      // 检查版本
      if replica.version != manager.primary.version {
        inconsistent_replicas = inconsistent_replicas.push(replica.id)
        continue
      }
      
      // 检查数据内容
      if replica.data.length() != manager.primary.data.length() {
        inconsistent_replicas = inconsistent_replicas.push(replica.id)
        continue
      }
      
      // 检查每个键值对
      let mut data_match = true
      for (key, value) in manager.primary.data {
        match DataReplica::get_value(replica, key) {
          Some(replica_value) => {
            if replica_value != value {
              data_match = false
              break
            }
          }
          None => {
            data_match = false
            break
          }
        }
      }
      
      if !data_match {
        inconsistent_replicas = inconsistent_replicas.push(replica.id)
      }
    }
    
    inconsistent_replicas
  }
  
  // 测试同步管理
  let manager = SyncManager::new("primary")
  let manager1 = SyncManager::add_replica(manager, "replica-1")
  let manager2 = SyncManager::add_replica(manager1, "replica-2")
  
  assert_eq(manager2.replicas.length(), 2)
  assert_eq(manager2.primary.version, 0)
  assert_eq(manager2.replicas[0].version, 0)
  assert_eq(manager2.replicas[1].version, 0)
  
  // 验证初始一致性
  let inconsistent1 = SyncManager::verify_consistency(manager2)
  assert_eq(inconsistent1.length(), 0)
  
  // 更新主副本
  let manager3 = SyncManager::update_primary(manager2, "key-1", "value-1")
  assert_eq(manager3.primary.version, 1)
  assert_eq(manager3.primary.data.length(), 1)
  
  // 副本应该被标记为不同步
  assert_eq(manager3.replicas[0].status, SyncStatus::OutOfSync)
  assert_eq(manager3.replicas[1].status, SyncStatus::OutOfSync)
  
  // 验证不一致性
  let inconsistent2 = SyncManager::verify_consistency(manager3)
  assert_eq(inconsistent2.length(), 2)  // 两个副本都不同步
  
  // 同步第一个副本
  let manager4 = SyncManager::sync_replica(manager3, "replica-1", 1640995200000000000L)
  assert_eq(manager4.replicas[0].version, 1)
  assert_eq(manager4.replicas[0].status, SyncStatus::InSync)
  assert_eq(manager4.replicas[0].data.length(), 1)
  assert_eq(DataReplica::get_value(manager4.replicas[0], "key-1"), Some("value-1"))
  
  // 验证部分一致性
  let inconsistent3 = SyncManager::verify_consistency(manager4)
  assert_eq(inconsistent3.length(), 1)  // 只有replica-2不同步
  
  // 同步第二个副本
  let manager5 = SyncManager::sync_replica(manager4, "replica-2", 1640995200000001000L)
  assert_eq(manager5.replicas[1].version, 1)
  assert_eq(manager5.replicas[1].status, SyncStatus::InSync)
  
  // 验证完全一致性
  let inconsistent4 = SyncManager::verify_consistency(manager5)
  assert_eq(inconsistent4.length(), 0)
  
  // 再次更新主副本
  let manager6 = SyncManager::update_primary(manager5, "key-2", "value-2")
  assert_eq(manager6.primary.version, 2)
  assert_eq(manager6.primary.data.length(), 2)
  
  // 验证再次不一致
  let inconsistent5 = SyncManager::verify_consistency(manager6)
  assert_eq(inconsistent5.length(), 2)
}

// 测试5: 数据约束验证
test "数据约束验证测试" {
  // 约束类型
  enum ConstraintType {
    NotNull
    Unique
    MinLength(Int)
    MaxLength(Int)
    Range(Int, Int)
    Pattern(String)  // 简化的正则表达式
    Custom((String) -> Bool)
  }
  
  // 约束定义
  struct Constraint {
    name : String
    constraint_type : ConstraintType
    error_message : String
  }
  
  fn Constraint::new(name : String, constraint_type : ConstraintType, error_message : String) -> Constraint {
    Constraint::{ name, constraint_type, error_message }
  }
  
  // 验证结果
  struct ValidationResult {
    is_valid : Bool
    errors : Array[String]
  }
  
  fn ValidationResult::new() -> ValidationResult {
    ValidationResult::{ is_valid: true, errors: [] }
  }
  
  fn ValidationResult::add_error(result : ValidationResult, error : String) -> ValidationResult {
    ValidationResult::{
      is_valid: false,
      errors: result.errors.push(error)
    }
  }
  
  // 约束验证器
  struct ConstraintValidator {
    constraints : Array[Constraint]
  }
  
  fn ConstraintValidator::new() -> ConstraintValidator {
    ConstraintValidator::{ constraints: [] }
  }
  
  fn ConstraintValidator::add_constraint(validator : ConstraintValidator, constraint : Constraint) -> ConstraintValidator {
    ConstraintValidator::{
      constraints: validator.constraints.push(constraint)
    }
  }
  
  fn ConstraintValidator::validate_string(validator : ConstraintValidator, value : String) -> ValidationResult {
    let mut result = ValidationResult::new()
    
    for constraint in validator.constraints {
      let is_valid = match constraint.constraint_type {
        ConstraintType::NotNull => value.length() > 0
        ConstraintType::MinLength(min) => value.length() >= min
        ConstraintType::MaxLength(max) => value.length() <= max
        ConstraintType::Pattern(pattern) => {
          // 简化的模式匹配
          if pattern == "email" {
            value.contains("@") && value.contains(".")
          } else if pattern == "phone" {
            value.length() >= 10 && value.chars().all(fn(c) { c.is_digit() || c == '-' })
          } else {
            true  // 未知模式，总是通过
          }
        }
        ConstraintType::Custom(validator_fn) => validator_fn(value)
        _ => true  // 不适用于字符串的约束
      }
      
      if !is_valid {
        result = ValidationResult::add_error(result, constraint.error_message)
      }
    }
    
    result
  }
  
  fn ConstraintValidator::validate_int(validator : ConstraintValidator, value : Int) -> ValidationResult {
    let mut result = ValidationResult::new()
    
    for constraint in validator.constraints {
      let is_valid = match constraint.constraint_type {
        ConstraintType::Range(min, max) => value >= min && value <= max
        ConstraintType::Custom(validator_fn) => validator_fn(value.to_string())
        _ => true  // 不适用于整数的约束
      }
      
      if !is_valid {
        result = ValidationResult::add_error(result, constraint.error_message)
      }
    }
    
    result
  }
  
  // 测试约束验证
  let validator = ConstraintValidator::new()
  
  // 添加字符串约束
  let not_null_constraint = Constraint::new("not_null", ConstraintType::NotNull, "值不能为空")
  let min_length_constraint = Constraint::new("min_length", ConstraintType::MinLength(5), "长度不能少于5个字符")
  let max_length_constraint = Constraint::new("max_length", ConstraintType::MaxLength(20), "长度不能超过20个字符")
  let email_constraint = Constraint::new("email", ConstraintType::Pattern("email"), "必须是有效的邮箱地址")
  
  let string_validator = ConstraintValidator::add_constraint(
    ConstraintValidator::add_constraint(
      ConstraintValidator::add_constraint(
        ConstraintValidator::add_constraint(validator, not_null_constraint),
        min_length_constraint
      ),
      max_length_constraint
    ),
    email_constraint
  )
  
  // 测试有效字符串
  let valid_email = "user@example.com"
  let result1 = ConstraintValidator::validate_string(string_validator, valid_email)
  assert_true(result1.is_valid)
  assert_eq(result1.errors.length(), 0)
  
  // 测试无效字符串（太短）
  let short_email = "a@b.c"
  let result2 = ConstraintValidator::validate_string(string_validator, short_email)
  assert_false(result2.is_valid)
  assert_true(result2.errors.length() > 0)
  assert_true(result2.errors.any(fn(err) { err.contains("长度不能少于5个字符") }))
  
  // 测试无效字符串（不是邮箱）
  let not_email = "not-an-email"
  let result3 = ConstraintValidator::validate_string(string_validator, not_email)
  assert_false(result3.is_valid)
  assert_true(result3.errors.any(fn(err) { err.contains("必须是有效的邮箱地址") }))
  
  // 测试无效字符串（空字符串）
  let empty_string = ""
  let result4 = ConstraintValidator::validate_string(string_validator, empty_string)
  assert_false(result4.is_valid)
  assert_true(result4.errors.any(fn(err) { err.contains("值不能为空") }))
  
  // 添加整数约束
  let range_constraint = Constraint::new("range", ConstraintType::Range(1, 100), "值必须在1到100之间")
  
  let int_validator = ConstraintValidator::add_constraint(ConstraintValidator::new(), range_constraint)
  
  // 测试有效整数
  let valid_int = 50
  let result5 = ConstraintValidator::validate_int(int_validator, valid_int)
  assert_true(result5.is_valid)
  assert_eq(result5.errors.length(), 0)
  
  // 测试无效整数（超出范围）
  let invalid_int = 150
  let result6 = ConstraintValidator::validate_int(int_validator, invalid_int)
  assert_false(result6.is_valid)
  assert_true(result6.errors.any(fn(err) { err.contains("值必须在1到100之间") }))
  
  // 测试自定义约束
  let custom_constraint = Constraint::new("custom", ConstraintType::Custom(fn(s) { s.starts_with("prefix") }), "必须以'prefix'开头")
  
  let custom_validator = ConstraintValidator::add_constraint(ConstraintValidator::new(), custom_constraint)
  
  // 测试自定义约束（有效）
  let valid_custom = "prefix-value"
  let result7 = ConstraintValidator::validate_string(custom_validator, valid_custom)
  assert_true(result7.is_valid)
  
  // 测试自定义约束（无效）
  let invalid_custom = "value-without-prefix"
  let result8 = ConstraintValidator::validate_string(custom_validator, invalid_custom)
  assert_false(result8.is_valid)
  assert_true(result8.errors.any(fn(err) { err.contains("必须以'prefix'开头") }))
}