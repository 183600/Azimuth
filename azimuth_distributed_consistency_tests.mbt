// Azimuth 分布式系统一致性测试
// 专注于分布式环境下系统一致性和协调机制的验证

// 测试1: 分布式锁机制
test "分布式锁机制测试" {
  // 分布式锁系统
  type DistributedLock = {
    resource_id: String,
    owner_id: String,
    acquired_at: Int,
    expires_at: Int
  }
  
  type LockManager = {
    locks: Map[String, DistributedLock],
    current_time: Int
  }
  
  fn create_lock_manager() -> LockManager {
    { 
      locks: Map::new(), 
      current_time: 1609459200 
    }
  }
  
  fn acquire_lock(manager: LockManager, resource_id: String, owner_id: String, ttl: Int) -> (LockManager, Bool) {
    match manager.locks.get(resource_id) {
      Some(lock) => {
        // 检查锁是否已过期
        if manager.current_time >= lock.expires_at {
          // 锁已过期，可以获取
          let new_lock = {
            resource_id: resource_id,
            owner_id: owner_id,
            acquired_at: manager.current_time,
            expires_at: manager.current_time + ttl
          }
          
          let updated_locks = Map::new()
          for (id, l) in manager.locks {
            updated_locks.insert(id, l)
          }
          updated_locks.insert(resource_id, new_lock)
          
          let updated_manager = {
            locks: updated_locks,
            current_time: manager.current_time
          }
          
          (updated_manager, true)
        } else {
          // 锁仍然有效，无法获取
          (manager, false)
        }
      }
      None => {
        // 资源未被锁定，可以获取
        let new_lock = {
          resource_id: resource_id,
          owner_id: owner_id,
          acquired_at: manager.current_time,
          expires_at: manager.current_time + ttl
        }
        
        let updated_locks = Map::new()
        for (id, l) in manager.locks {
          updated_locks.insert(id, l)
        }
        updated_locks.insert(resource_id, new_lock)
        
        let updated_manager = {
          locks: updated_locks,
          current_time: manager.current_time
        }
        
        (updated_manager, true)
      }
    }
  }
  
  fn release_lock(manager: LockManager, resource_id: String, owner_id: String) -> (LockManager, Bool) {
    match manager.locks.get(resource_id) {
      Some(lock) => {
        if lock.owner_id == owner_id {
          // 锁属于该所有者，可以释放
          let updated_locks = Map::new()
          for (id, l) in manager.locks {
            if id != resource_id {
              updated_locks.insert(id, l)
            }
          }
          
          let updated_manager = {
            locks: updated_locks,
            current_time: manager.current_time
          }
          
          (updated_manager, true)
        } else {
          // 锁不属于该所有者，无法释放
          (manager, false)
        }
      }
      None => {
        // 锁不存在
        (manager, false)
      }
    }
  }
  
  // 创建锁管理器
  let lock_manager = create_lock_manager()
  
  // 获取锁
  let (manager_with_lock, acquired) = acquire_lock(lock_manager, "resource1", "client1", 300)
  assert_true(acquired)
  
  // 尝试再次获取同一资源的锁
  let (_, acquired_again) = acquire_lock(manager_with_lock, "resource1", "client2", 300)
  assert_false(acquired_again)
  
  // 释放锁
  let (manager_without_lock, released) = release_lock(manager_with_lock, "resource1", "client1")
  assert_true(released)
  
  // 尝试获取已释放的锁
  let (_, acquired_after_release) = acquire_lock(manager_without_lock, "resource1", "client2", 300)
  assert_true(acquired_after_release)
  
  // 测试锁过期
  let (manager_with_expiring_lock, _) = acquire_lock(lock_manager, "resource2", "client1", 60)
  let manager_with_advanced_time = { ...manager_with_expiring_lock, current_time: 1609459200 + 120 }
  let (_, acquired_after_expiry) = acquire_lock(manager_with_advanced_time, "resource2", "client2", 300)
  assert_true(acquired_after_expiry)
  
  // 测试错误的所有者尝试释放锁
  let (manager_with_lock2, _) = acquire_lock(lock_manager, "resource3", "client1", 300)
  let (_, released_by_wrong_owner) = release_lock(manager_with_lock2, "resource3", "client2")
  assert_false(released_by_wrong_owner)
}

// 测试2: 分布式共识算法
test "分布式共识算法测试" {
  // 简化的分布式共识系统
  type Node = {
    id: String,
    state: String,
    term: Int,
    voted_for: Option[String]
  }
  
  type ConsensusMessage = {
    sender: String,
    term: Int,
    message_type: String,
    data: String
  }
  
  type ConsensusSystem = {
    nodes: Map[String, Node],
    messages: Array[ConsensusMessage],
    current_term: Int
  }
  
  fn create_consensus_system(node_count: Int) -> ConsensusSystem {
    let nodes = Map::new()
    for i in 0..<node_count {
      let node_id = "node" + i.to_string()
      let node = {
        id: node_id,
        state: "follower",
        term: 0,
        voted_for: None
      }
      nodes.insert(node_id, node)
    }
    
    {
      nodes: nodes,
      messages: [],
      current_term: 0
    }
  }
  
  fn start_election(system: ConsensusSystem, candidate_id: String) -> ConsensusSystem {
    // 增加任期
    let new_term = system.current_term + 1
    
    // 更新候选节点状态
    let updated_nodes = Map::new()
    for (id, node) in system.nodes {
      if id == candidate_id {
        let updated_node = {
          id: node.id,
          state: "candidate",
          term: new_term,
          voted_for: Some(candidate_id)
        }
        updated_nodes.insert(id, updated_node)
      } else {
        updated_nodes.insert(id, node)
      }
    }
    
    // 创建投票请求消息
    let vote_request = {
      sender: candidate_id,
      term: new_term,
      message_type: "vote_request",
      data: ""
    }
    
    let updated_messages = [] : Array[ConsensusMessage]
    for msg in system.messages {
      updated_messages.push(msg)
    }
    updated_messages.push(vote_request)
    
    {
      nodes: updated_nodes,
      messages: updated_messages,
      current_term: new_term
    }
  }
  
  fn process_vote_request(system: ConsensusSystem, voter_id: String, request: ConsensusMessage) -> ConsensusSystem {
    let voter_node = system.nodes.get(voter_id).unwrap()
    
    // 检查是否可以投票
    let can_vote = voter_node.term < request.term || 
                   (voter_node.term == request.term && 
                    match voter_node.voted_for {
                      Some(voted_for) => voted_for == request.sender
                      None => true
                    })
    
    if can_vote {
      // 更新投票者节点状态
      let updated_nodes = Map::new()
      for (id, node) in system.nodes {
        if id == voter_id {
          let updated_node = {
            id: node.id,
            state: node.state,
            term: request.term,
            voted_for: Some(request.sender)
          }
          updated_nodes.insert(id, updated_node)
        } else {
          updated_nodes.insert(id, node)
        }
      }
      
      // 创建投票响应消息
      let vote_response = {
        sender: voter_id,
        term: request.term,
        message_type: "vote_response",
        data: request.sender
      }
      
      let updated_messages = [] : Array[ConsensusMessage]
      for msg in system.messages {
        updated_messages.push(msg)
      }
      updated_messages.push(vote_response)
      
      {
        nodes: updated_nodes,
        messages: updated_messages,
        current_term: system.current_term
      }
    } else {
      system
    }
  }
  
  fn count_votes(system: ConsensusSystem, candidate_id: String, term: Int) -> Int {
    let vote_count = 0
    for (id, node) in system.nodes {
      if node.term == term {
        match node.voted_for {
          Some(voted_for) => {
            if voted_for == candidate_id {
              vote_count = vote_count + 1
            }
          }
          None => ()
        }
      }
    }
    vote_count
  }
  
  // 创建3节点共识系统
  let consensus_system = create_consensus_system(3)
  
  // 节点0开始选举
  let system_with_election = start_election(consensus_system, "node0")
  
  // 节点1和节点2处理投票请求
  let system_with_vote1 = process_vote_request(system_with_election, "node1", system_with_election.messages[0])
  let system_with_votes = process_vote_request(system_with_vote1, "node2", system_with_election.messages[0])
  
  // 计算投票数
  let votes_for_node0 = count_votes(system_with_votes, "node0", 1)
  assert_eq(votes_for_node0, 3)  // 所有节点都投给了node0
  
  // 验证节点状态
  let node0 = system_with_votes.nodes.get("node0").unwrap()
  let node1 = system_with_votes.nodes.get("node1").unwrap()
  let node2 = system_with_votes.nodes.get("node2").unwrap()
  
  assert_eq(node0.state, "candidate")
  assert_eq(node0.term, 1)
  assert_eq(node0.voted_for.unwrap_or(""), "node0")
  
  assert_eq(node1.state, "follower")
  assert_eq(node1.term, 1)
  assert_eq(node1.voted_for.unwrap_or(""), "node0")
  
  assert_eq(node2.state, "follower")
  assert_eq(node2.term, 1)
  assert_eq(node2.voted_for.unwrap_or(""), "node0")
}

// 测试3: 分布式事务一致性
test "分布式事务一致性测试" {
  // 分布式事务系统
  type TransactionState = {
    id: String,
    status: String,  // "prepared", "committed", "aborted"
    participants: Array[String],
    operations: Array[String]
  }
  
  type TransactionCoordinator = {
    transactions: Map[String, TransactionState],
    participant_logs: Map[String, Array[String>>
  }
  
  fn create_coordinator() -> TransactionCoordinator {
    { 
      transactions: Map::new(), 
      participant_logs: Map::new() 
    }
  }
  
  fn begin_transaction(coordinator: TransactionCoordinator, transaction_id: String, participants: Array[String]) -> TransactionCoordinator {
    let transaction = {
      id: transaction_id,
      status: "prepared",
      participants: participants,
      operations: []
    }
    
    let updated_transactions = Map::new()
    for (id, tx) in coordinator.transactions {
      updated_transactions.insert(id, tx)
    }
    updated_transactions.insert(transaction_id, transaction)
    
    // 初始化参与者日志
    let updated_logs = Map::new()
    for (participant, log) in coordinator.participant_logs {
      updated_logs.insert(participant, log)
    }
    for participant in participants {
      if !updated_logs.contains(participant) {
        updated_logs.insert(participant, [])
      }
      let log = updated_logs.get(participant).unwrap_or([])
      let updated_log = [] : Array[String>
      for entry in log {
        updated_log.push(entry)
      }
      updated_log.push("PREPARE " + transaction_id)
      updated_logs.insert(participant, updated_log)
    }
    
    {
      transactions: updated_transactions,
      participant_logs: updated_logs
    }
  }
  
  fn commit_transaction(coordinator: TransactionCoordinator, transaction_id: String) -> TransactionCoordinator {
    match coordinator.transactions.get(transaction_id) {
      Some(transaction) => {
        // 更新事务状态
        let updated_transaction = {
          id: transaction.id,
          status: "committed",
          participants: transaction.participants,
          operations: transaction.operations
        }
        
        let updated_transactions = Map::new()
        for (id, tx) in coordinator.transactions {
          if id == transaction_id {
            updated_transactions.insert(id, updated_transaction)
          } else {
            updated_transactions.insert(id, tx)
          }
        }
        
        // 更新参与者日志
        let updated_logs = Map::new()
        for (participant, log) in coordinator.participant_logs {
          updated_logs.insert(participant, log)
        }
        for participant in transaction.participants {
          let log = updated_logs.get(participant).unwrap_or([])
          let updated_log = [] : Array[String>
          for entry in log {
            updated_log.push(entry)
          }
          updated_log.push("COMMIT " + transaction_id)
          updated_logs.insert(participant, updated_log)
        }
        
        {
          transactions: updated_transactions,
          participant_logs: updated_logs
        }
      }
      None => coordinator
    }
  }
  
  fn abort_transaction(coordinator: TransactionCoordinator, transaction_id: String) -> TransactionCoordinator {
    match coordinator.transactions.get(transaction_id) {
      Some(transaction) => {
        // 更新事务状态
        let updated_transaction = {
          id: transaction.id,
          status: "aborted",
          participants: transaction.participants,
          operations: transaction.operations
        }
        
        let updated_transactions = Map::new()
        for (id, tx) in coordinator.transactions {
          if id == transaction_id {
            updated_transactions.insert(id, updated_transaction)
          } else {
            updated_transactions.insert(id, tx)
          }
        }
        
        // 更新参与者日志
        let updated_logs = Map::new()
        for (participant, log) in coordinator.participant_logs {
          updated_logs.insert(participant, log)
        }
        for participant in transaction.participants {
          let log = updated_logs.get(participant).unwrap_or([])
          let updated_log = [] : Array[String>
          for entry in log {
            updated_log.push(entry)
          }
          updated_log.push("ABORT " + transaction_id)
          updated_logs.insert(participant, updated_log)
        }
        
        {
          transactions: updated_transactions,
          participant_logs: updated_logs
        }
      }
      None => coordinator
    }
  }
  
  // 创建事务协调器
  let coordinator = create_coordinator()
  
  // 开始事务
  let participants = ["database1", "database2", "cache"]
  let coordinator_with_tx = begin_transaction(coordinator, "tx123", participants)
  
  // 验证事务状态
  let transaction = coordinator_with_tx.transactions.get("tx123").unwrap()
  assert_eq(transaction.status, "prepared")
  assert_eq(transaction.participants.length(), 3)
  
  // 验证参与者日志
  let db1_log = coordinator_with_tx.participant_logs.get("database1").unwrap_or([])
  let db2_log = coordinator_with_tx.participant_logs.get("database2").unwrap_or([])
  let cache_log = coordinator_with_tx.participant_logs.get("cache").unwrap_or([])
  
  assert_eq(db1_log.length(), 1)
  assert_eq(db1_log[0], "PREPARE tx123")
  
  assert_eq(db2_log.length(), 1)
  assert_eq(db2_log[0], "PREPARE tx123")
  
  assert_eq(cache_log.length(), 1)
  assert_eq(cache_log[0], "PREPARE tx123")
  
  // 提交事务
  let coordinator_with_commit = commit_transaction(coordinator_with_tx, "tx123")
  
  // 验证事务提交状态
  let committed_transaction = coordinator_with_commit.transactions.get("tx123").unwrap()
  assert_eq(committed_transaction.status, "committed")
  
  // 验证参与者日志
  let db1_commit_log = coordinator_with_commit.participant_logs.get("database1").unwrap_or([])
  let db2_commit_log = coordinator_with_commit.participant_logs.get("database2").unwrap_or([])
  let cache_commit_log = coordinator_with_commit.participant_logs.get("cache").unwrap_or([])
  
  assert_eq(db1_commit_log.length(), 2)
  assert_eq(db1_commit_log[1], "COMMIT tx123")
  
  assert_eq(db2_commit_log.length(), 2)
  assert_eq(db2_commit_log[1], "COMMIT tx123")
  
  assert_eq(cache_commit_log.length(), 2)
  assert_eq(cache_commit_log[1], "COMMIT tx123")
  
  // 测试事务中止
  let coordinator_with_abort_tx = begin_transaction(coordinator, "tx456", participants)
  let coordinator_with_abort = abort_transaction(coordinator_with_abort_tx, "tx456")
  
  // 验证事务中止状态
  let aborted_transaction = coordinator_with_abort.transactions.get("tx456").unwrap()
  assert_eq(aborted_transaction.status, "aborted")
  
  // 验证参与者日志
  let db1_abort_log = coordinator_with_abort.participant_logs.get("database1").unwrap_or([])
  let db2_abort_log = coordinator_with_abort.participant_logs.get("database2").unwrap_or([])
  let cache_abort_log = coordinator_with_abort.participant_logs.get("cache").unwrap_or([])
  
  assert_eq(db1_abort_log.length(), 2)
  assert_eq(db1_abort_log[1], "ABORT tx456")
  
  assert_eq(db2_abort_log.length(), 2)
  assert_eq(db2_abort_log[1], "ABORT tx456")
  
  assert_eq(cache_abort_log.length(), 2)
  assert_eq(cache_abort_log[1], "ABORT tx456")
}

// 测试4: 分布式数据复制一致性
test "分布式数据复制一致性测试" {
  // 分布式数据复制系统
  type Replica = {
    id: String,
    data: Map[String, String],
    version: Int,
    last_updated: Int
  }
  
  type ReplicationSystem = {
    replicas: Map[String, Replica],
    write_ahead_log: Array[String]
  }
  
  fn create_replication_system(replica_count: Int) -> ReplicationSystem {
    let replicas = Map::new()
    for i in 0..<replica_count {
      let replica_id = "replica" + i.to_string()
      let replica = {
        id: replica_id,
        data: Map::new(),
        version: 0,
        last_updated: 1609459200
      }
      replicas.insert(replica_id, replica)
    }
    
    {
      replicas: replicas,
      write_ahead_log: []
    }
  }
  
  fn write_data(system: ReplicationSystem, key: String, value: String) -> ReplicationSystem {
    // 记录写入日志
    let log_entry = "WRITE " + key + " " + value + " " + (1609459200).to_string()
    let updated_log = [] : Array[String]
    for entry in system.write_ahead_log {
      updated_log.push(entry)
    }
    updated_log.push(log_entry)
    
    // 更新所有副本
    let updated_replicas = Map::new()
    for (id, replica) in system.replicas {
      let updated_data = Map::new()
      for (k, v) in replica.data {
        updated_data.insert(k, v)
      }
      updated_data.insert(key, value)
      
      let updated_replica = {
        id: replica.id,
        data: updated_data,
        version: replica.version + 1,
        last_updated: 1609459200
      }
      updated_replicas.insert(id, updated_replica)
    }
    
    {
      replicas: updated_replicas,
      write_ahead_log: updated_log
    }
  }
  
  fn check_consistency(system: ReplicationSystem) -> Bool {
    if system.replicas.size() == 0 {
      return true
    }
    
    // 获取第一个副本作为参考
    let first_replica = system.replicas.values().next()
    match first_replica {
      Some(reference) => {
        // 检查所有副本是否与参考副本一致
        for (id, replica) in system.replicas {
          if replica.data.size() != reference.data.size() {
            return false
          }
          
          for (key, value) in reference.data {
            match replica.data.get(key) {
              Some(v) => {
                if v != value {
                  return false
                }
              }
              None => return false
            }
          }
          
          if replica.version != reference.version {
            return false
          }
        }
        
        true
      }
      None => true
    }
  }
  
  fn simulate_network_partition(system: ReplicationSystem, affected_replicas: Array[String]) -> ReplicationSystem {
    // 模拟网络分区，部分副本无法同步
    let updated_replicas = Map::new()
    for (id, replica) in system.replicas {
      if !affected_replicas.contains(id) {
        // 未受影响的副本可以更新
        updated_replicas.insert(id, replica)
      } else {
        // 受影响的副本保持原状
        updated_replicas.insert(id, replica)
      }
    }
    
    {
      replicas: updated_replicas,
      write_ahead_log: system.write_ahead_log
    }
  }
  
  fn repair_network_partition(system: ReplicationSystem, affected_replicas: Array[String]) -> ReplicationSystem {
    // 修复网络分区，同步所有副本
    if system.replicas.size() == 0 {
      return system
    }
    
    // 找到最新的副本作为同步源
    let mut latest_replica = system.replicas.values().next()
    let mut latest_version = -1
    
    for (id, replica) in system.replicas {
      if replica.version > latest_version {
        latest_version = replica.version
        latest_replica = Some(replica)
      }
    }
    
    match latest_replica {
      Some(reference) => {
        // 同步所有副本到最新状态
        let updated_replicas = Map::new()
        for (id, replica) in system.replicas {
          if affected_replicas.contains(id) {
            // 同步受影响的副本
            let updated_replica = {
              id: replica.id,
              data: reference.data,
              version: reference.version,
              last_updated: 1609459200
            }
            updated_replicas.insert(id, updated_replica)
          } else {
            updated_replicas.insert(id, replica)
          }
        }
        
        {
          replicas: updated_replicas,
          write_ahead_log: system.write_ahead_log
        }
      }
      None => system
    }
  }
  
  // 创建复制系统
  let replication_system = create_replication_system(3)
  
  // 写入数据
  let system_with_data = write_data(replication_system, "key1", "value1")
  let system_with_more_data = write_data(system_with_data, "key2", "value2")
  
  // 验证数据一致性
  assert_true(check_consistency(system_with_more_data))
  
  // 验证所有副本都有相同的数据
  let replica1 = system_with_more_data.replicas.get("replica0").unwrap()
  let replica2 = system_with_more_data.replicas.get("replica1").unwrap()
  let replica3 = system_with_more_data.replicas.get("replica2").unwrap()
  
  assert_eq(replica1.data.get("key1").unwrap_or(""), "value1")
  assert_eq(replica1.data.get("key2").unwrap_or(""), "value2")
  assert_eq(replica1.version, 2)
  
  assert_eq(replica2.data.get("key1").unwrap_or(""), "value1")
  assert_eq(replica2.data.get("key2").unwrap_or(""), "value2")
  assert_eq(replica2.version, 2)
  
  assert_eq(replica3.data.get("key1").unwrap_or(""), "value1")
  assert_eq(replica3.data.get("key2").unwrap_or(""), "value2")
  assert_eq(replica3.version, 2)
  
  // 模拟网络分区
  let affected_replicas = ["replica2"]
  let system_with_partition = simulate_network_partition(system_with_more_data, affected_replicas)
  
  // 在分区期间写入数据
  let system_with_partition_data = write_data(system_with_partition, "key3", "value3")
  
  // 验证系统一致性（应该不一致）
  assert_false(check_consistency(system_with_partition_data))
  
  // 修复网络分区
  let system_with_repair = repair_network_partition(system_with_partition_data, affected_replicas)
  
  // 验证系统一致性（应该恢复一致）
  assert_true(check_consistency(system_with_repair))
}

// 测试5: 分布式事件顺序保证
test "分布式事件顺序保证测试" {
  // 分布式事件系统
  type Event = {
    id: String,
    source: String,
    timestamp: Int,
    logical_clock: Int,
    data: String
  }
  
  type NodeState = {
    id: String,
    logical_clock: Int,
    events: Array[Event]
  }
  
  type EventSystem = {
    nodes: Map[String, NodeState],
    global_events: Array[Event]
  }
  
  fn create_event_system(node_count: Int) -> EventSystem {
    let nodes = Map::new()
    for i in 0..<node_count {
      let node_id = "node" + i.to_string()
      let node = {
        id: node_id,
        logical_clock: 0,
        events: []
      }
      nodes.insert(node_id, node)
    }
    
    {
      nodes: nodes,
      global_events: []
    }
  }
  
  fn create_event(system: EventSystem, source: String, data: String) -> EventSystem {
    match system.nodes.get(source) {
      Some(node) => {
        // 增加逻辑时钟
        let new_logical_clock = node.logical_clock + 1
        
        // 创建事件
        let event = {
          id: "event_" + source + "_" + new_logical_clock.to_string(),
          source: source,
          timestamp: 1609459200,  // 固定时间戳用于测试
          logical_clock: new_logical_clock,
          data: data
        }
        
        // 更新节点状态
        let updated_node = {
          id: node.id,
          logical_clock: new_logical_clock,
          events: node.events + [event]
        }
        
        let updated_nodes = Map::new()
        for (id, n) in system.nodes {
          if id == source {
            updated_nodes.insert(id, updated_node)
          } else {
            updated_nodes.insert(id, n)
          }
        }
        
        // 更新全局事件列表
        let updated_global_events = [] : Array[Event]
        for e in system.global_events {
          updated_global_events.push(e)
        }
        updated_global_events.push(event)
        
        {
          nodes: updated_nodes,
          global_events: updated_global_events
        }
      }
      None => system
    }
  }
  
  fn send_event(system: EventSystem, source: String, target: String, event: Event) -> EventSystem {
    match system.nodes.get(target) {
      let target_node => {
        // 更新目标节点的逻辑时钟
        let new_logical_clock = max(target_node.logical_clock, event.logical_clock) + 1
        
        // 更新目标节点状态
        let updated_node = {
          id: target_node.id,
          logical_clock: new_logical_clock,
          events: target_node.events + [event]
        }
        
        let updated_nodes = Map::new()
        for (id, n) in system.nodes {
          if id == target {
            updated_nodes.insert(id, updated_node)
          } else {
            updated_nodes.insert(id, n)
          }
        }
        
        {
          nodes: updated_nodes,
          global_events: system.global_events
        }
      }
    }
  }
  
  fn sort_events_by_logical_clock(events: Array[Event]) -> Array[Event] {
    // 简化的事件排序（实际实现中应使用更高效的排序算法）
    let sorted = [] : Array[Event]
    let unsorted = events
    
    while unsorted.length() > 0 {
      let mut min_event = unsorted[0]
      let mut min_index = 0
      
      for i in 1..<unsorted.length() {
        if unsorted[i].logical_clock < min_event.logical_clock {
          min_event = unsorted[i]
          min_index = i
        }
      }
      
      sorted.push(min_event)
      unsorted.remove(min_index)
    }
    
    sorted
  }
  
  // 创建事件系统
  let event_system = create_event_system(3)
  
  // 节点0创建事件
  let system_with_event1 = create_event(event_system, "node0", "Event from node0")
  
  // 节点1创建事件
  let system_with_event2 = create_event(system_with_event1, "node1", "Event from node1")
  
  // 节点0创建另一个事件
  let system_with_event3 = create_event(system_with_event2, "node0", "Another event from node0")
  
  // 验证逻辑时钟
  let node0 = system_with_event3.nodes.get("node0").unwrap()
  let node1 = system_with_event3.nodes.get("node1").unwrap()
  let node2 = system_with_event3.nodes.get("node2").unwrap()
  
  assert_eq(node0.logical_clock, 2)  // node0创建了两个事件
  assert_eq(node1.logical_clock, 1)  // node1创建了一个事件
  assert_eq(node2.logical_clock, 0)  // node2没有创建事件
  
  // 验证事件逻辑时钟
  let node0_events = node0.events
  let node1_events = node1.events
  
  assert_eq(node0_events.length(), 2)
  assert_eq(node0_events[0].logical_clock, 1)
  assert_eq(node0_events[1].logical_clock, 2)
  
  assert_eq(node1_events.length(), 1)
  assert_eq(node1_events[0].logical_clock, 1)
  
  // 发送事件到其他节点
  let system_with_sent_event = send_event(system_with_event3, "node0", "node2", node0_events[0])
  
  // 验证接收节点的逻辑时钟更新
  let updated_node2 = system_with_sent_event.nodes.get("node2").unwrap()
  assert_eq(updated_node2.logical_clock, 1)  // 更新为max(0, 1) + 1
  
  // 验证全局事件排序
  let sorted_events = sort_events_by_logical_clock(system_with_sent_event.global_events)
  assert_eq(sorted_events[0].logical_clock, 1)
  assert_eq(sorted_events[1].logical_clock, 1)
  assert_eq(sorted_events[2].logical_clock, 2)
}